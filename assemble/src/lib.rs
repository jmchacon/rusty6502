//! `assemble` provides methods for processing a list of input
//! lines into a binary 64k image file suitable for a 6502
//! system.

use color_eyre::eyre::{eyre, Result};
use regex::Regex;
use rusty6502::prelude::*;
use std::collections::HashMap;
use std::fmt::{self, Write};
use std::io::BufReader;
use std::num::Wrapping;
use std::str::FromStr;
use std::sync::OnceLock;
use std::{fs::File, io::Lines};

// Basic grammer:
//
// <u8> - 0-255 expressed as decimal (default), hex ($ or 0x), binary (%), or a single char as "X".
//        NOTE: Characters can be expressed as \X for newline (\n), CR (\r) and Tab (\t). Anything else
//              just set the direct 8 bit value...
// <u16> - 0-65534 expressed as decimal (default), hex ($ or 0x), or binary (%).
// <Val16> - <LABEL>|u16 - A label ref or a u16 value.
// <Val8> - <LABEL>|u8 - A label ref (which must be u8) or a u8 value.
// <Val> - <LABEL>|u8|u16 - A label ref, a u8, or a u16 value.
// STRING - An ASCII string surrounded by quotes - "STRING".
// <LABEL> - A string of the form /^[a-zA-Z][a-zA-Z0-9+-_]+$/ or '*' which always
//           refers to the current PC location.
//           TODO(jchacon) - Remove + and implement real addition, subtraction, or/and/etc support.
//
// All entries below implicitly take comments after though as defined comments can also be standalone.
//
// ORG|.ORG <u16> - Reset the PC location to the u16 value
// LABEL <EQU|=> <u8|u16> - A label followed by equality setting the label to the indicated value.
// [<LABEL>]: - A single line label defining a location with no other data.
// [<LABEL>] WORD|.WORD <Val16> [<Val16> ...] - One or more u16 values with an optional
//                                        label defining the location.
// [<LABEL>] BYTE|.BYTE <Val8>|STRING [<Val8>|STRING ...] - One or more u8/string values
//                                                    with an optional label defining the location.
//                                                    Strings are automatically expanded but not NUL
//                                                    terminated (use ASCIIZ for that).
// [<LABEL>] ASCIIZ|.ASCIIZ STRING - An ASCII string that automatically appends a NUL on expansion.
// [<LABEL>] OPCODE [<Val>|] - A valid 6502 opcode with an optional label defining
//                             the location. Val validity for size depends on opcode.
//                             i.e. JMP must be a u16 while branch/immediate modes are u8.
// ; <ascii>... - A comment.

#[cfg(test)]
mod tests;

// State defines the state machine for parsing a given line.
//
// Begin starts all lines and the progressions that are valid:
//
// Begin -> Label -> Equ -> Remainder -> Begin|Comment
//      |        |-> Op -> Remainder -> Begin|Comment
//      |-> Org -> Remainder -> Begin|Comment
//      |-> Word -> Remainder -> Begin|Comment
//      |-> Op -> Remainder -> Begin|Comment
//      |-> Comment -> Begin
#[derive(Clone, Debug, PartialEq)]
enum State {
    Begin,
    Label(String, bool),
    Equ(String),
    Org,
    Word(Vec<MemDef>),
    Byte(Vec<MemDef>),
    AsciiZ(Vec<MemDef>),
    Op(Opcode),
    Comment(String),
    Remainder(bool),
}

#[derive(Clone, Debug, PartialEq)]
struct MemDef {
    val: OpVal8,
    pc: u16,
}

// Token defines an entry on a given line post tokenizing.
// Some values (such as the Operation in Op) may only be
// partially filled in at this point until further parse
// rounds occur.
#[derive(Debug)]
enum Token {
    Label(String),
    Org(u16),
    Word(Vec<MemDef>),
    Byte(Vec<MemDef>),
    AsciiZ(Vec<MemDef>),
    Op(Operation),
    Comment(String),
    Equ(String),
}

// Operation defines an assembly instruction with
// all of it's parsed values (such as address mode, optional value, etc).
// NOTE: width and pc are not valid until we are done with label parsing
//       as only at that point is the final addressing mode known for certain.
#[derive(Debug, Default)]
struct Operation {
    op: Opcode,
    mode: AddressMode,
    op_val: Option<Vec<OpVal>>,
    x_index: bool,
    y_index: bool,
    width: u16,
    pc: u16,
}

// OpVal defines the operation value for an opcode.
// This is either an 8/16 bit value or a label which
// eventually will reference an 8/16 bit value.
#[derive(Clone, Debug, PartialEq)]
enum OpVal {
    Label(String),
    Val(TokenVal),
}

impl fmt::Display for OpVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpVal::Label(s) => write!(f, "{s}"),
            OpVal::Val(tok) => match tok {
                TokenVal::Val8(v) => write!(f, "{v:#04X}"),
                TokenVal::Val16(v) => write!(f, "{v:#06X}"),
            },
        }
    }
}

// OpVal8 is similar to an OpVal but is constrained to an 8 bit value only.
// Mostly used with WORD/BYTE to emit a series of values or label values.
#[derive(Clone, Debug, PartialEq)]
enum OpVal8 {
    Label(String),
    Val(u8),
}

impl fmt::Display for OpVal8 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpVal8::Label(s) => write!(f, "{s}"),
            OpVal8::Val(tok) => write!(f, "{tok:#04X}"),
        }
    }
}

// TokenVal defines an operation value which is either 8 or 16 bit.
#[derive(Debug, Copy, Clone, PartialEq)]
enum TokenVal {
    Val8(u8),
    Val16(u16),
}

// LabelDef defines a value and a location for a label
// declaration (i.e. EQU or a location label). Also
// anywhere that references this label also has the locations
// recorded.
#[derive(Debug)]
struct LabelDef {
    val: Option<TokenVal>,
    line: usize,
    refs: Vec<usize>,
}

/// Assembly defines the output from an assemble pass.
/// Both a binary image and a listing file are returned.
pub struct Assembly {
    /// The binary image. Always 64k to represent a complete
    /// memory image (often used to simulate a cartridge).
    pub bin: [u8; 1 << 16],
    /// A listing file of the translated input.
    pub listing: String,
}

struct ASTOutput {
    ast: Vec<Vec<Token>>,
    labels: HashMap<String, LabelDef>,
}

// pass1 does the initial AST build for the input given.
// It will compute an AST from the file reader along with a label map
// that has references to both fully defined labels (EQU) and references
// to location labels (either defs or refs).
#[allow(clippy::too_many_lines)]
fn pass1(cpu: &dyn CPU, lines: Lines<BufReader<File>>) -> Result<ASTOutput> {
    let mut ret = ASTOutput {
        ast: Vec::<Vec<Token>>::new(),
        labels: HashMap::new(),
    };

    for (line_num, line) in lines.map_while(Result::ok).enumerate() {
        let fields: Vec<&str> = line.split_whitespace().collect();

        let mut l = Vec::<Token>::new();

        let mut state = State::Begin;

        for (index, token) in fields.iter().enumerate() {
            let upper = token.to_uppercase();

            let cur_state = state.clone();
            state = match state {
                State::Begin => match upper.as_str() {
                    // We can match an ORG, WORD or comment here directly.
                    "ORG" | ".ORG" => State::Org,
                    "WORD" | ".WORD" => State::Word(vec![]),
                    "BYTE" | ".BYTE" => State::Byte(vec![]),
                    "ASCIIZ" | ".ASCIIZ" => State::AsciiZ(vec![]),
                    // Anything else is either a comment, opcode or a label to be fleshed
                    // out in a later state.
                    _ => {
                        if upper.starts_with(';') {
                            State::Comment(token[1..].into())
                        } else if let Ok(op) = Opcode::from_str(token) {
                            State::Op(op)
                        } else {
                            let (token, single) = if token.ends_with(':') {
                                (&token[0..token.len() - 1], true)
                            } else {
                                (*token, false)
                            };
                            match parse_label(token) {
                                Ok(label) => State::Label(label, single),
                                Err(err) => {
                                    return Err(eyre!(
                                        "Error parsing line {}: invalid label {err} - {line}",
                                        line_num + 1
                                    ));
                                }
                            }
                        }
                    }
                },
                State::Label(label, single) => {
                    if single {
                        // The only way we get here is if there are more tokens
                        // which pretty much can only be a comment. So add the label
                        // token and move to that state.
                        if !token.starts_with(';') {
                            return Err(eyre!("Error parsing line {}: only comment after parsed tokens allowed - remainder {token} - {line}", line_num + 1));
                        }

                        add_label(true, &mut ret, &label, None, &line, line_num)?;
                        l.push(Token::Label(label));
                        State::Comment(token[1..].into())
                    } else {
                        match upper.as_str() {
                            // Labels can be EQU style in which case we need one more state to get
                            // the value.
                            "EQU" | "=" => {
                                if fields.len() < 3 {
                                    return Err(eyre!(
                                        "Error parsing line {}: invalid EQU - {line}",
                                        line_num + 1
                                    ));
                                }
                                // Don't have to validate label since State::Begin did it above before
                                // progressing here.
                                State::Equ(label)
                            }
                            "WORD" | ".WORD" => {
                                add_label(true, &mut ret, &label, None, &line, line_num)?;
                                l.push(Token::Label(label));
                                State::Word(vec![])
                            }
                            "BYTE" | ".BYTE" => {
                                add_label(true, &mut ret, &label, None, &line, line_num)?;
                                l.push(Token::Label(label));
                                State::Byte(vec![])
                            }
                            "ASCIIZ" | ".ASCIIZ" => {
                                add_label(true, &mut ret, &label, None, &line, line_num)?;
                                l.push(Token::Label(label));
                                State::AsciiZ(vec![])
                            }

                            // Otherwise this should be an opcode and we can push a label
                            // into tokens. Phase 2 will figure out its value.
                            _ => match Opcode::from_str(upper.as_str()) {
                                Ok(op) => {
                                    add_label(true, &mut ret, &label, None, &line, line_num)?;
                                    l.push(Token::Label(label));
                                    State::Op(op)
                                }
                                Err(_) => {
                                    return Err(eyre!(
                                        "Error parsing line {}: invalid opcode - {line}",
                                        line_num + 1
                                    ));
                                }
                            },
                        }
                    }
                }
                // An ORG statement must be followed by a u16 value.
                State::Org => {
                    let Some(TokenVal::Val16(pc)) = parse_val(token, true) else {
                        return Err(eyre!(
                                "Error parsing line {}: invalid ORG value not 16 bit - {token} - {line}",
                                line_num + 1,
                            ));
                    };
                    l.push(Token::Org(pc));
                    State::Remainder(false)
                }
                State::Word(mut md) | State::Byte(mut md) | State::AsciiZ(mut md) => {
                    let (is_word, is_byte, is_asciiz) = match cur_state {
                        State::Word(_) => (true, false, false),
                        State::Byte(_) => (false, true, false),
                        State::AsciiZ(_) => (false, false, true),
                        _ => panic!("impossible"),
                    };
                    // This is a tiny bit more complicated because we handle both sets of logic (word vs byte) but 90%+ of that
                    // is the same so this avoids duplicating a bunch of code.
                    // We may have tokens left which is a comment so check for that and move to that phase.
                    if token.starts_with(';') {
                        if md.is_empty() {
                            // Account for '<WORD|BYTE|ASCIIZ> ; some comments'
                            return Err(eyre!(
                                "Error parsing line {}: WORD, BYTE or ASCIIZ without value - {line}",
                                line_num + 1
                            ));
                        }
                        let tok = if is_word {
                            Token::Word(md)
                        } else if is_byte {
                            Token::Byte(md)
                        } else {
                            Token::AsciiZ(md)
                        };
                        l.push(tok);
                        // SAFETY: We know it has a prefix..
                        let c = unsafe { token.strip_prefix(';').unwrap_unchecked() };
                        State::Comment(c.into())
                    } else {
                        // Just something not implied so it doesn't care about the rest.
                        // TODO(jchacon): Do this better. This is so leaky...
                        let mut op = Operation {
                            mode: AddressMode::Immediate,
                            ..Default::default()
                        };

                        if is_asciiz {
                            // This gets a little harder. We'll start with a token that has
                            // to start with a quote. But...it could have whitespace inside
                            // of it so we really have to parse the rest of the line here.
                            if !token.starts_with('"') {
                                return Err(eyre!(
                                    "Error parsing line {}: ASCIIZ must be of the form \"XXX\" bad token '{token}' - {line}",
                                    line_num + 1
                                ));
                            }

                            let lstr = line.as_str();
                            // SAFETY: We know this contains ASCIIZ or else we
                            //         can't get to this state. And it must have
                            //         a whitespace char after that due to the initial match from Begin.
                            let mut idx =
                                unsafe { lstr.to_uppercase().find("ASCIIZ").unwrap_unchecked() }
                                    + "ASCIIZ".len();
                            let start = idx;
                            let bytes = lstr.as_bytes();
                            for i in bytes.iter().skip(start) {
                                if !i.is_ascii_whitespace() {
                                    break;
                                }
                                idx += 1;
                            }
                            if bytes[idx] != b'"' {
                                return Err(eyre!(
                                    "Error parsing line {}: ASCIIZ1 must be of the form \"XXX\" bad token '{:?}' - {line}", bytes[idx],
                                    line_num + 1
                                ));
                            }
                            // Now we're at the beginning of the 1st string.
                            let mut in_str = true;
                            idx += 1;
                            let mut comment = String::new();

                            for i in bytes.iter().skip(idx) {
                                idx += 1;
                                if in_str {
                                    if *i == b'"' {
                                        in_str = false;
                                        // Add the automatic trailing NUL
                                        md.push(MemDef {
                                            pc: 0x0000,
                                            val: OpVal8::Val(0x00),
                                        });
                                    } else {
                                        md.push(MemDef {
                                            pc: 0x0000,
                                            val: OpVal8::Val(*i),
                                        });
                                    }
                                } else {
                                    if *i == b'"' {
                                        in_str = true;
                                    }
                                    if *i == b';' {
                                        // Comment so we're done.

                                        // SAFETY: We never changed this so it's a valid utf8 string still from
                                        // here forward.
                                        comment = unsafe {
                                            String::from_utf8_unchecked(bytes[idx..].to_vec())
                                        };
                                    }
                                }
                            }
                            l.push(Token::AsciiZ(md));
                            if comment.is_empty() {
                                State::Remainder(true)
                            } else {
                                State::Comment(comment)
                            }
                        } else {
                            match token_to_val_or_label(
                                token, &mut op, &mut ret, is_word, line_num, &line,
                            )? {
                                OpVal::Label(l) => {
                                    md.push(MemDef {
                                        pc: 0x0000,
                                        val: OpVal8::Label(l),
                                    });
                                    if is_word {
                                        State::Word(md)
                                    } else {
                                        State::Byte(md)
                                    }
                                }
                                OpVal::Val(v) => match v {
                                    TokenVal::Val8(v) => {
                                        if is_word {
                                            return Err(eyre!("Error parsing line {}: invalid WORD value not 16 bit - {token} - {line}", line_num + 1));
                                        }
                                        md.push(MemDef {
                                            pc: 0x0000,
                                            val: OpVal8::Val(v),
                                        });
                                        State::Byte(md)
                                    }
                                    TokenVal::Val16(v) => {
                                        if !is_word {
                                            return Err(eyre!("Error parsing line {}: invalid BYTE value not 8 bit - {token} - {line}", line_num + 1));
                                        }
                                        // Even though we must parse as a u16 we store this in little endian
                                        // form so we can emit straight into memory. Plus we don't need
                                        // a separate form for Byte.
                                        let high = ((v & 0xFF00) >> 8) as u8;
                                        let low = (v & 0x00FF) as u8;
                                        md.push(MemDef {
                                            pc: 0x0000,
                                            val: OpVal8::Val(low),
                                        });
                                        md.push(MemDef {
                                            pc: 0x0000,
                                            val: OpVal8::Val(high),
                                        });
                                        State::Word(md)
                                    }
                                },
                            }
                        }
                    }
                }
                // Remainder is an intermediate state after we've processed something.
                // At this point we can only define a comment and move to that stage.
                // If instead this ran out of tokens in fields the loop would end and the
                // next line starts at Begin automatically.
                State::Remainder(line_done) => {
                    if line_done {
                        State::Remainder(line_done)
                    } else {
                        match token.as_bytes() {
                            [b';', ..] => State::Comment(token[1..].into()),
                            _ => {
                                return Err(eyre!(
                            "Error parsing line {}: only comment after parsed tokens allowed - remainder {token} - {line}",
                            line_num + 1,
                        ));
                            }
                        }
                    }
                }
                // For comments take everything left, construct a comment and then abort the loop now.
                State::Comment(c) => {
                    let mut comment: String = ";".into();
                    comment += c.as_str();
                    comment += " ";
                    comment.push_str(fields.as_slice()[index..].join(" ").as_str());
                    l.push(Token::Comment(comment));
                    state = State::Begin;
                    break;
                }
                // EQU is label EQU <val> so process the value and push a new TokenDef into tokens.
                State::Equ(label) => {
                    let val = match parse_val(token, false) {
                        Some(TokenVal::Val8(v)) => TokenVal::Val8(v),
                        Some(TokenVal::Val16(v)) => TokenVal::Val16(v),
                        _ => {
                            return Err(eyre!(
                                "Error parsing line {}: not valid u8 or u16 for EQU - {line}",
                                line_num + 1,
                            ));
                        }
                    };
                    add_label(true, &mut ret, &label, Some(val), &line, line_num)?;
                    l.push(Token::Equ(label));
                    State::Remainder(false)
                }
                State::Op(op) => {
                    // Start all address modes with implied. If we can figure
                    // it out at a given point change to the correct one. This
                    // way later steps can determine if we haven't determined
                    // it completely yet.
                    let mut operation = Operation {
                        op,
                        mode: AddressMode::Implied,
                        ..Default::default()
                    };
                    // If this has no operand we may only have a comment.
                    if token.as_bytes().first() == Some(&b';') {
                        operation.mode = AddressMode::Implied;
                        l.push(Token::Op(operation));
                        State::Comment(token[1..].into())
                    } else if cpu
                        .resolve_opcode(&operation.op, &AddressMode::ZeroPageRelative)
                        .is_ok()
                    {
                        // Ops of this nature are also the only mode and since they have a different op_val
                        // we'll parse it directly.
                        operation.mode = AddressMode::ZeroPageRelative;
                        let parts: Vec<&str> = token.split(',').collect();
                        if parts.len() != 3 {
                            return Err(eyre!(
                                "Invalid zero page relative (too many parts) on line {} - {token}",
                                line_num + 1
                            ));
                        }
                        let pre = parts[0].parse::<u8>().unwrap_or(10);
                        if pre > 7 {
                            return Err(eyre!(
                                "Invalid zero page relative (index too large) on line {} - {token}",
                                line_num + 1
                            ));
                        }
                        let zpaddr = token_to_val_or_label(
                            parts[1],
                            &mut operation,
                            &mut ret,
                            false,
                            line_num,
                            &line,
                        )?;
                        let dest = token_to_val_or_label(
                            parts[2],
                            &mut operation,
                            &mut ret,
                            false,
                            line_num,
                            &line,
                        )?;
                        operation.op_val =
                            Some(vec![OpVal::Val(TokenVal::Val8(pre)), zpaddr, dest]);
                        l.push(Token::Op(operation));
                        State::Remainder(false)
                    } else {
                        if cpu
                            .resolve_opcode(&operation.op, &AddressMode::Relative)
                            .is_ok()
                        {
                            // Ops which are relative only have this mode so we can just assign
                            // and parse the val which must be 8 bit.
                            // Can't validate value now as while it's 8 bit this can be 16 bit
                            // and mean a PC difference.
                            operation.mode = AddressMode::Relative;
                        }
                        // NOTE: We don't validate here if op_val is the right size
                        //       that gets handled on a later pass.
                        let val = match token.as_bytes() {
                            // Immediate - #val
                            [b'#', val @ ..] => {
                                operation.mode = AddressMode::Immediate;
                                val
                            }
                            // Indirect X - (val,x)
                            [b'(', val @ .., b',', b'x' | b'X', b')'] => {
                                operation.mode = AddressMode::IndirectX;
                                val
                            }
                            // Indirect Y - (val),y
                            [b'(', val @ .., b')', b',', b'y' | b'Y'] => {
                                operation.mode = AddressMode::IndirectY;
                                val
                            }
                            // Indirect - (val)
                            [b'(', val @ .., b')'] => {
                                operation.mode = AddressMode::AbsoluteIndirect;
                                val
                            }
                            // AbsoluteX or ZeroPageX - val,x
                            [val @ .., b',', b'x' | b'X'] => {
                                operation.x_index = true;
                                val
                            }
                            // AbsoluteY or ZeroPageY - val,y
                            [val @ .., b',', b'y' | b'Y'] => {
                                operation.y_index = true;
                                val
                            }
                            // Either Absolute, ZeroPage, Relative or a Label
                            _ => token.as_bytes(),
                        };
                        // SAFETY: We know the remainder is valid utf8 since we only removed ASCII
                        //         and started with a valid utf8 str.
                        let op_val = unsafe { std::str::from_utf8(val).unwrap_unchecked() };

                        operation.op_val = Some(vec![token_to_val_or_label(
                            op_val,
                            &mut operation,
                            &mut ret,
                            false,
                            line_num,
                            &line,
                        )?]);
                        l.push(Token::Op(operation));
                        State::Remainder(false)
                    }
                }
            };
        }

        // At this point we've run out of tokens and are in some final state.
        // Some are valid and some are not:
        //
        // Handle the case of a line of ";". i.e. nothing trailing the ; so we don't loop around
        // to process the Comment state. Properly processing would have left us in Begin instead.
        // It may also be a single token opcode such as SEI which has no operand value/label.
        match state {
            State::Comment(c) => {
                let mut comment: String = ";".into();
                comment += c.as_str();
                l.push(Token::Comment(comment));
            }
            State::Op(op) => l.push(Token::Op(Operation {
                op,
                mode: AddressMode::Implied,
                ..Default::default()
            })),
            State::Label(label, true) => {
                // Handle
                //
                // LABEL:
                //
                // Correctly by directly adding the label now. The loop above
                // can only handle
                //
                // LABEL: ; Some comments
                add_label(true, &mut ret, &label, None, &line, line_num)?;
                l.push(Token::Label(label));
            }
            // These are valid states to exit so we can ignore them.
            State::Begin | State::Remainder(_) => {}
            // Word gets here if we processed and then ended without a comment
            State::Word(md) => {
                l.push(Token::Word(md));
            }
            State::Byte(md) => {
                l.push(Token::Byte(md));
            }
            State::AsciiZ(md) => {
                l.push(Token::AsciiZ(md));
            }
            // Label and Org can happen if they define and then end the line
            // Label in particular would have to do this without the : form
            // as that's the only valid single label form.
            State::Label(_, false) | State::Org => {
                return Err(eyre!(
                    "Error parsing line {}: missing data for {state:?} - {line}",
                    line_num + 1,
                ));
            }
            // Anything else is an internal error which shouldn't be possible.
            State::Equ(_) => {
                panic!(
                    "Internal error parsing line {}: invalid state {state:?} - {line}",
                    line_num + 1
                )
            }
        };
        ret.ast.push(l);
    }
    Ok(ret)
}

// compute_refs takes the previous AST output and cross references all labels,
// builds PC values for each operation and updates the AST with these results.
// The only thing it leaves is relative address computation since that can't be
// known until all labels are fully cross referenced. That will be handled during
// byte code generation.
#[allow(clippy::too_many_lines)]
fn compute_refs(cpu: &dyn CPU, ast_output: &mut ASTOutput) -> Result<()> {
    let mut pc: u16 = 0;
    for (line_num, line) in ast_output.ast.iter_mut().enumerate() {
        for t in line.iter_mut() {
            match t {
                Token::Label(s) => {
                    // A left side label is the PC value.
                    get_label_mut(&mut ast_output.labels, s).val = Some(TokenVal::Val16(pc));
                }
                Token::Org(npc) => {
                    pc = *npc;
                }
                Token::Word(md) | Token::Byte(md) | Token::AsciiZ(md) => {
                    for m in md.iter_mut() {
                        m.pc = pc;
                        pc += 1;
                    }
                }
                Token::Equ(_) | Token::Comment(_) => {}
                Token::Op(o) => {
                    let op = &o.op;
                    let width: u16;
                    if o.mode != AddressMode::Implied && cpu.resolve_opcode(op, &o.mode).is_err() {
                        return Err(eyre!(
                            "Error parsing line {}: opcode {op} doesn't support mode {}",
                            line_num + 1,
                            o.mode
                        ));
                    }
                    match o.mode {
                        // Implied gets handled here as it could resolve into another mode
                        // still due to not knowing all labels earlier.
                        AddressMode::Implied | AddressMode::NOPCmos => {
                            // Check the operand and if there is one it means
                            // this isn't Implied and we need to use this to compute either ZeroPage or Absolute
                            match &o.op_val {
                                Some(v) => {
                                    match &v[0] {
                                        OpVal::Label(l) => {
                                            let ld = get_label(&ast_output.labels, l);
                                            if let Some(TokenVal::Val8(_)) = ld.val {
                                                o.mode = find_mode(TokenVal::Val8(0), o);
                                                width = 2;
                                            } else {
                                                // Anything else is either 16 bit or a label we don't know which
                                                // also must be 16 bit at this point.
                                                o.mode = find_mode(TokenVal::Val16(0), o);
                                                width = 3;
                                            }
                                        }
                                        // Anything else was a direct value and already matched in pass1 or is actually implied.
                                        // If it directly matched that'll resolve below.
                                        OpVal::Val(_) => {
                                            width = 1;
                                        }
                                    }
                                }
                                // Anything else was a direct value and already matched in pass1 or is actually implied.
                                // If it directly matched that'll resolve below.
                                _ => {
                                    width = 1;
                                }
                            }
                            // Check mode here since it was either implied, ZP, or Absolute and didn't get
                            // checked yet for this opcode.
                            if cpu.resolve_opcode(op, &o.mode).is_err() {
                                return Err(eyre!(
                                    "Error parsing line {}: opcode {op} doesn't support mode - {}",
                                    line_num + 1,
                                    o.mode
                                ));
                            }
                        }
                        AddressMode::Immediate
                        | AddressMode::ZeroPage
                        | AddressMode::ZeroPageX
                        | AddressMode::ZeroPageY
                        | AddressMode::Indirect
                        | AddressMode::IndirectX
                        | AddressMode::IndirectY => {
                            let mut ok = false;
                            width = 2;
                            if let Some(v) = &o.op_val {
                                match &v[0] {
                                    OpVal::Label(l) => {
                                        if let Some(TokenVal::Val8(_)) =
                                            get_label(&ast_output.labels, l).val
                                        {
                                            ok = true;
                                        }
                                    }
                                    OpVal::Val(t) => {
                                        if let TokenVal::Val8(_) = t {
                                            ok = true;
                                        };
                                    }
                                };
                            }
                            if !ok {
                                return Err(eyre!(
                                    "Error parsing line {}: {} must have an 8 bit arg",
                                    line_num + 1,
                                    o.mode
                                ));
                            }
                        }
                        AddressMode::Absolute
                        | AddressMode::AbsoluteNOP
                        | AddressMode::AbsoluteX
                        | AddressMode::AbsoluteY
                        | AddressMode::AbsoluteIndirect
                        | AddressMode::AbsoluteIndirectX => {
                            let mut ok = false;
                            if let Some(v) = &o.op_val {
                                match &v[0] {
                                    OpVal::Label(l) => {
                                        if let Some(TokenVal::Val16(_)) =
                                            get_label(&ast_output.labels, l).val
                                        {
                                            ok = true;
                                        } else if get_label(&ast_output.labels, l).val.is_none() {
                                            // location ref which must be 16 bit.
                                            ok = true;
                                        }
                                    }
                                    OpVal::Val(t) => {
                                        if let TokenVal::Val16(_) = t {
                                            ok = true;
                                        };
                                    }
                                };
                            };
                            if !ok {
                                return Err(eyre!(
                                    "Error parsing line {}: {} must have a 16 bit arg",
                                    line_num + 1,
                                    o.mode
                                ));
                            }
                            width = 3;
                        }
                        AddressMode::Relative => {
                            // We just add width here. In byte emission below it'll compute and validate
                            // the actual offset. This is due to forward label refs. i.e. BEQ DONE before DONE
                            // has been processed so we don't know the PC value yet but will below.
                            width = 2;
                        }
                        AddressMode::ZeroPageRelative => {
                            // We just add width here. In byte emission below it'll compute and validate
                            // the actual offset. This is due to forward label refs. i.e. BBR 0,0x12,DONE before DONE
                            // has been processed so we don't know the PC value yet but will below.
                            width = 3;
                        }
                    };

                    o.pc = pc;
                    o.width = width;
                    pc += width;
                    continue;
                }
            };
        }
    }
    Ok(())
}

// generate_output does the final byte code and listing file generation from
// the previously generated AST. This must be mutable as final relative addresses
// are computed before byte codes are generated for a given operation.
#[allow(clippy::too_many_lines)]
fn generate_output(cpu: &dyn CPU, ast_output: &mut ASTOutput) -> Result<Assembly> {
    // Always emit 64k so just allocate a block.
    let mut res = Assembly {
        bin: [0; 1 << 16],
        listing: String::new(),
    };

    for (line_num, line) in ast_output.ast.iter_mut().enumerate() {
        let mut output = String::new();
        let mut label_out = String::new();
        for (index, t) in line.iter_mut().enumerate() {
            match t {
                Token::Label(td) => {
                    write!(label_out, "{td:<8}").unwrap();
                }
                Token::Org(pc) => {
                    write!(output, "ORG           {pc:04X}").unwrap();
                }
                Token::AsciiZ(md) => {
                    let mut post_bytes = format!("{label_out:<8}ASCIIZ ");
                    let mut byte_dump = format!("{:04X}", md[0].pc);
                    // Starts with a "
                    let mut val = String::from("\"");
                    for m in md {
                        let OpVal8::Val(v) = m.val else {
                            panic!("Impossible ASCIIZ state!");
                        };
                        res.bin[usize::from(m.pc)] = v;

                        // End of string so process it.
                        if v == 0x00 {
                            write!(byte_dump, " 00").unwrap();
                            write!(post_bytes, " {val}\"").unwrap();
                            val.clear();
                            val.push('"');
                            continue;
                        }
                        val.push(char::from(v));

                        write!(byte_dump, " {v:02X}").unwrap();
                    }
                    write!(output, "{byte_dump} {post_bytes}").unwrap();
                    label_out.clear();
                }
                Token::Word(md) => {
                    let mut post_bytes = format!("{label_out:<8}WORD   ");
                    let mut byte_dump = format!("{:04X}", md[0].pc);
                    let mut it = md.iter();
                    while let Some(m) = it.next() {
                        let (low, high, pc, lbl) = match &m.val {
                            OpVal8::Label(l) => {
                                // SAFETY: WORD is defined always per above parsing.
                                match unsafe {
                                    get_label(&ast_output.labels, l).val.unwrap_unchecked()
                                } {
                                    TokenVal::Val8(v) => (v, 0x00, m.pc, l.clone()),
                                    TokenVal::Val16(v) => {
                                        let low = (v & 0x00FF) as u8;
                                        let high = ((v & 0xFF00) >> 8) as u8;
                                        (low, high, m.pc, l.clone())
                                    }
                                }
                            }
                            OpVal8::Val(v) => {
                                let low = *v;
                                let low_pc = m.pc;
                                let Some(m) = it.next() else {
                                    panic!(
                                        "Impossible state. One byte followed by nothing for WORD"
                                    );
                                };
                                let high = match m.val {
                                    OpVal8::Label(_) => panic!(
                                        "Impossible state. One byte followed by label for WORD"
                                    ),
                                    OpVal8::Val(v) => v,
                                };
                                (low, high, low_pc, String::new())
                            }
                        };
                        if lbl.is_empty() {
                            let val = u16::from(high) << 8 | u16::from(low);
                            write!(post_bytes, " {val:04X}").unwrap();
                        } else {
                            write!(post_bytes, " {lbl}").unwrap();
                        }
                        write!(byte_dump, " {low:02X} {high:02X}").unwrap();
                        res.bin[usize::from(pc)] = low;
                        res.bin[usize::from(pc + 1)] = high;
                    }
                    write!(output, "{byte_dump} {post_bytes}").unwrap();
                    label_out.clear();
                }
                Token::Byte(md) => {
                    let mut post_bytes = format!("{label_out:<8}BYTE   ");
                    let mut byte_dump = format!("{:04X}", md[0].pc);
                    for m in md {
                        let (val, lbl) = match &m.val {
                            OpVal8::Label(l) => {
                                // SAFETY: BYTTE is defined always per above parsing.
                                match unsafe {
                                    get_label(&ast_output.labels, l).val.unwrap_unchecked()
                                } {
                                    TokenVal::Val8(v) => (vec![v], l.clone()),
                                    TokenVal::Val16(v) => {
                                        let low = (v & 0x00FF) as u8;
                                        let high = ((v & 0xFF00) >> 8) as u8;
                                        (vec![low, high], l.clone())
                                    }
                                }
                            }
                            OpVal8::Val(v) => (vec![*v], String::new()),
                        };
                        if lbl.is_empty() {
                            write!(post_bytes, " {:02X}", val[0]).unwrap();
                            write!(byte_dump, " {:02X}", val[0]).unwrap();
                            res.bin[usize::from(m.pc)] = val[0];
                        } else {
                            write!(post_bytes, " {lbl}").unwrap();
                            for (p, v) in val.iter().enumerate() {
                                write!(byte_dump, " {v:02X}").unwrap();
                                res.bin[usize::from(m.pc) + p] = *v;
                            }
                        }
                    }
                    write!(output, "{byte_dump} {post_bytes}").unwrap();
                    label_out.clear();
                }
                Token::Equ(td) => {
                    // SAFETY: EQU is defined always per above parsing.
                    match unsafe { get_label(&ast_output.labels, td).val.unwrap_unchecked() } {
                        TokenVal::Val8(v) => {
                            write!(output, "{td:<13} EQU      {v:02X}").unwrap();
                        }
                        TokenVal::Val16(v) => {
                            write!(output, "{td:<13} EQU      {v:04X}").unwrap();
                        }
                    };
                }
                Token::Comment(c) => {
                    // If we get here with a non-empty label_out it means this
                    // was a single entry so print it.
                    if !label_out.is_empty() {
                        let lbl = String::from(label_out.as_str().trim_end());
                        // SAFETY: We just checked labels above has everything referenced and filled in.
                        let ld =
                            unsafe { get_label(&ast_output.labels, &lbl).val.unwrap_unchecked() };

                        let TokenVal::Val16(v) = ld else {
                            panic!("Single line label must be a PC value!");
                        };
                        write!(output, "{v:04X}          {label_out}").unwrap();
                        label_out.clear();
                    }
                    if index != 0 {
                        write!(output, " ").unwrap();
                    }
                    write!(output, "{c}").unwrap();
                }
                Token::Op(o) => {
                    let modes = cpu.resolve_opcode(&o.op, &o.mode).unwrap_or_else(|err| {
                        panic!("Internal error on line {}: {err}", line_num + 1)
                    });

                    assert!(!(o.op_val.is_none() && o.mode != AddressMode::Implied),"Internal error on line {}: no op val but not implied instruction for opcode {}", line_num+1, o.op);
                    assert!(!(o.mode == AddressMode::Implied && o.width != 1), "Internal error on line {}: implied mode for opcode {} but width not 1: {o:#?}", line_num+1, o.op);

                    // Things are mutable here as we may have to fixup op_val for branches before
                    // emitting bytes below.
                    if o.mode == AddressMode::Relative || o.mode == AddressMode::ZeroPageRelative {
                        let mut ok = false;
                        if let Some(v) = &o.op_val {
                            let offset = if o.mode == AddressMode::Relative {
                                &v[0]
                            } else {
                                &v[2]
                            };
                            match offset {
                                OpVal::Label(l) => {
                                    let mut r: Option<u16> = None;
                                    // Handle * usages in branches.
                                    if l == "*" {
                                        r = Some(o.pc);
                                    }
                                    match get_label(&ast_output.labels, l).val {
                                        Some(TokenVal::Val8(_)) => {
                                            ok = true;
                                        }
                                        Some(TokenVal::Val16(p)) => {
                                            r = Some(p);
                                        }
                                        // Technically not possible as we already validated all labels resolve.
                                        _ => {}
                                    }
                                    if let Some(r) = r {
                                        #[allow(clippy::cast_possible_wrap)]
                                        // The actual diff is from the pc following the instruction.
                                        let diff = (Wrapping(r) - Wrapping(o.pc + 2)).0 as i16;
                                        if (i16::from(i8::MIN)..=i16::from(i8::MAX)).contains(&diff)
                                        {
                                            ok = true;
                                            // Remove the label ref here and insert the relative offset
                                            // as the value now. We can blind cast this as we know
                                            // it's in signed i8 range so bit casting to u8 is fine.
                                            #[allow(clippy::cast_sign_loss)]
                                            let val = (diff & 0x00FF) as u8;
                                            let new = OpVal::Val(TokenVal::Val8(val));
                                            if o.mode == AddressMode::Relative {
                                                o.op_val = Some(vec![new]);
                                            } else {
                                                o.op_val =
                                                    Some(vec![v[0].clone(), v[1].clone(), new]);
                                            }
                                        }
                                    }
                                }
                                OpVal::Val(t) => {
                                    if let TokenVal::Val8(_) = t {
                                        ok = true;
                                    };
                                }
                            };
                        };
                        if !ok {
                            return Err(eyre!(
                            "Error parsing line {}: either not 8 bit or out of range for relative instruction",
                            line_num + 1
                        ));
                        }
                    }

                    // Grab the first entry in the bytes vec for the opcode value.
                    // TODO(jchacon): If > 1 pick one randomly.
                    if o.mode == AddressMode::ZeroPageRelative {
                        // BBR/BBS are special. This is actually the vector offsets based on the first opval value.
                        // SAFETY: We know this is fine since we range checked it above.
                        let v = unsafe { o.op_val.as_ref().unwrap_unchecked() };
                        let OpVal::Val(TokenVal::Val8(offset)) = v[0] else {
                            panic!("First value of ZeroPageRelative must be a u8");
                        };
                        res.bin[usize::from(o.pc)] = modes[usize::from(offset)];
                    } else {
                        res.bin[usize::from(o.pc)] = modes[0];
                    }
                    if label_out.is_empty() {
                        label_out = "        ".into();
                    }
                    if let Some(v) = &o.op_val {
                        let test = if o.mode == AddressMode::ZeroPageRelative {
                            &v[2]
                        } else {
                            &v[0]
                        };
                        let val = match test {
                            OpVal::Label(s) => {
                                // SAFETY: We just checked labels above has everything referenced and filled in.
                                // The one exception is * which we just handled.
                                unsafe { get_label(&ast_output.labels, s).val.unwrap_unchecked() }
                            }
                            OpVal::Val(t) => *t,
                        };
                        match val {
                            TokenVal::Val8(b) => {
                                // ZP Relative is odd...It's width 3 but is all 8 bit values.
                                if o.mode == AddressMode::ZeroPageRelative {
                                    assert!(o.width == 3,"Internal error on line {}: got wrong data for ZP rel on op {} and mode {}", line_num+1, o.op, o.mode);

                                    let zpval = match &v[1] {
                                        OpVal::Label(s) => {
                                            // SAFETY: We just checked labels above has everything referenced and filled in.
                                            // The one exception is * which we just handled.
                                            unsafe {
                                                get_label(&ast_output.labels, s)
                                                    .val
                                                    .unwrap_unchecked()
                                            }
                                        }
                                        OpVal::Val(t) => *t,
                                    };
                                    let TokenVal::Val8(zp) = zpval else {
                                        return Err(eyre!("{}: Invalid value for ZP address for ZP Relative on op {} and mode {}", line_num+1, o.op, o.mode));
                                    };

                                    res.bin[usize::from(o.pc + 1)] = zp;
                                    res.bin[usize::from(o.pc + 2)] = b;
                                    write!(
                                        output,
                                        "{:04X} {:02X} {zp:02X} {b:02X} {label_out} ",
                                        o.pc,
                                        res.bin[usize::from(o.pc)]
                                    )
                                    .unwrap();
                                } else {
                                    assert!(o.width == 2,"Internal error on line {}: got 8 bit value and expect 16 bit for op {} and mode {}", line_num+1, o.op, o.mode);

                                    res.bin[usize::from(o.pc + 1)] = b;
                                    write!(
                                        output,
                                        "{:04X} {:02X} {b:02X}    {label_out} ",
                                        o.pc, modes[0]
                                    )
                                    .unwrap();
                                }
                            }
                            TokenVal::Val16(b) => {
                                assert!(o.width == 3,"Internal error on line {}: got 16 bit value and expect 8 bit for op {} and mode {}", line_num+1, o.op, o.mode);

                                // Store 16 bit values in little endian.
                                let low = (b & 0x00FF) as u8;
                                let high = ((b & 0xFF00) >> 8) as u8;
                                res.bin[usize::from(o.pc + 1)] = low;
                                res.bin[usize::from(o.pc + 2)] = high;
                                write!(
                                    output,
                                    "{:04X} {:02X} {low:02X} {high:02X} {label_out} ",
                                    o.pc, modes[0]
                                )
                                .unwrap();
                            }
                        };
                        let op = &o.op;
                        let val: String;
                        // SAFETY: we already know this has a value
                        unsafe {
                            if o.mode == AddressMode::ZeroPageRelative {
                                let v = o.op_val.as_ref().unwrap_unchecked();
                                let OpVal::Val(TokenVal::Val8(index)) = v[0] else {
                                    panic!("Internal error on line {}: Invalid ZP Relative token for {o:?}", line_num+1);
                                };
                                val = format!("{},{},{}", index, v[1], v[2]);
                            } else {
                                val = format!("{}", o.op_val.as_ref().unwrap_unchecked()[0]);
                            }
                        }

                        // TODO(jchacon): Can we use disassemble here instead
                        //                of duplicating this logic?
                        write!(output, "{op:?}").unwrap();
                        match o.mode {
                            AddressMode::Immediate => {
                                write!(output, " #{val}").unwrap();
                            }
                            AddressMode::Indirect | AddressMode::AbsoluteIndirect => {
                                write!(output, " ({val})").unwrap();
                            }
                            AddressMode::IndirectX | AddressMode::AbsoluteIndirectX => {
                                write!(output, " ({val},X)").unwrap();
                            }
                            AddressMode::IndirectY => {
                                write!(output, " ({val}),Y").unwrap();
                            }
                            AddressMode::AbsoluteX | AddressMode::ZeroPageX => {
                                write!(output, " {val},X").unwrap();
                            }
                            AddressMode::AbsoluteY | AddressMode::ZeroPageY => {
                                write!(output, " {val},Y").unwrap();
                            }
                            _ => {
                                if !val.is_empty() {
                                    write!(output, " {val}").unwrap();
                                }
                            }
                        };
                        label_out.clear();
                    } else {
                        write!(
                            output,
                            "{:04X} {:02X}       {label_out} {:?}",
                            o.pc, modes[0], o.op
                        )
                        .unwrap();
                        label_out.clear();
                    }
                }
            }
        }

        if !label_out.is_empty() {
            let lbl = String::from(label_out.as_str().trim_end());
            // SAFETY: We just checked labels above has everything referenced and filled in.
            let ld = unsafe { get_label(&ast_output.labels, &lbl).val.unwrap_unchecked() };

            let TokenVal::Val16(v) = ld else {
                panic!("Single line label must be a PC value!");
            };
            write!(output, "{v:04X}          {label_out}").unwrap();
            label_out.clear();
        }
        // If this was a blank line that also auto-parses for output purposes.
        writeln!(res.listing, "{output}").unwrap();
    }
    Ok(res)
}

/// `parse` will take the given filename, read it into RAM
/// and assemble it into 6502 assembly written back as the 64k
/// array returned along with a listing file.
///
/// # Errors
/// Any parsing error of the input stream can be returned in Result.
pub fn parse(cpu: &dyn CPU, lines: Lines<BufReader<File>>, debug: bool) -> Result<Assembly> {
    // Assemblers generally are 2+ passes. One pass to tokenize as much as possible
    // while filling in a label mapping. The 2nd one does actual assembly over
    // the tokens with labels getting filled in from the map or computed as needed.

    // Pass one, read over the file line by line generating a set of tokens per line
    // for our AST. The labels map that comes back is complete so referencing keys
    // we lookup in later passes can use get_label to resolve them.
    let mut ast_output = pass1(cpu, lines)?;

    // Do another run so we can fill in label references.
    // Also compute addressing mode, validate and set pc.
    compute_refs(cpu, &mut ast_output)?;

    // Verify all the labels defined got values (EQU and location definitions/references line up)
    let mut errors = String::new();
    for (l, ld) in &ast_output.labels {
        //        println!("{l} -> {ld:?}");
        let locs = ld
            .refs
            .iter()
            .map(|x| format!("{x}"))
            .collect::<Vec<_>>()
            .join(",");
        // As long as it's not "*" (which is resolved in generate_output) makes
        // sure everything else resolves.
        if l != "*" && (ld.line == 0 || ld.val.is_none()) {
            writeln!(
                errors,
                "Parsing error: Label {l} was never defined. Located on lines {locs}"
            )
            .unwrap();
        }
    }
    if !errors.is_empty() {
        return Err(eyre!(errors));
    }

    // If debug print this out
    if debug {
        println!("AST:\n");
        for a in &ast_output.ast {
            println!("{a:?}");
        }
        println!();
        println!("Labels:\n");
        for (k, v) in &ast_output.labels {
            println!("{k:?} -> {v:?}");
        }
    }

    // Finally generate the output
    generate_output(cpu, &mut ast_output)
}

// Given a labels map (label->LabelDef) return the labeldef directly w/o checking.
// This should be only called after AST building as it assumes all labels are valid.
fn get_label<'a>(hm: &'a HashMap<String, LabelDef>, label: &String) -> &'a LabelDef {
    // SAFETY: Only called after AST built so all labels are in map.
    unsafe { hm.get(label).unwrap_unchecked() }
}

fn get_label_mut<'a>(hm: &'a mut HashMap<String, LabelDef>, label: &String) -> &'a mut LabelDef {
    // SAFETY: Only called after AST built so all labels are in map.
    unsafe { hm.get_mut(label).unwrap_unchecked() }
}

// parse_val returns a parsed TokenVal8/16 or nothing. Set is_u16 to force returning a Val16 always
// if a value would parse.
// Also handles the case of "X" (i.e. explicit string surrounding a char).
// This will handle \n, \r and \t but any other escapes should just use direct values.
fn parse_val(val: &str, is_u16: bool) -> Option<TokenVal> {
    let (trimmed, base, string_val) = match val.as_bytes() {
        // Remove any possibly leading 0x or $. If we did this is also base 16
        // Can index back into val without worry about utf8 since we only matched
        // ascii chars here.
        [b'0', b'x', ..] => (&val[2..], 16, false),
        [b'$', ..] => (&val[1..], 16, false),
        [b'%', ..] => (&val[1..], 2, false),
        [b'"', b'\\', b'n', b'"'] => ("\n", 10, true),
        [b'"', b'\\', b'r', b'"'] => ("\r", 10, true),
        [b'"', b'\\', b't', b'"'] => ("\t", 10, true),
        [b'"', .., b'"'] => (&val[1..2], 10, true),
        _ => (val, 10, false),
    };

    // If nothing was left we're done with no value.
    if trimmed.is_empty() {
        return None;
    }

    if string_val {
        let v = trimmed.as_bytes()[0];
        if is_u16 {
            #[allow(clippy::cast_lossless)]
            return Some(TokenVal::Val16(v as u16));
        }
        return Some(TokenVal::Val8(v));
    }

    if let Ok(v) = usize::from_str_radix(trimmed, base) {
        // All of the casts are ok as we range check before assigning.
        match v {
            65536.. => None,
            0..=255 => {
                if is_u16 {
                    #[allow(clippy::cast_possible_truncation)]
                    Some(TokenVal::Val16(v as u16))
                } else {
                    #[allow(clippy::cast_possible_truncation)]
                    Some(TokenVal::Val8(v as u8))
                }
            }
            #[allow(clippy::cast_possible_truncation)]
            _ => Some(TokenVal::Val16(v as u16)),
        }
    } else {
        None
    }
}

// parse_label will validate a label is of the right form (letter followed by 0..N letter/number/some punc).
// NOTE: * is a special case and always allowed.
fn parse_label(label: &str) -> Result<String> {
    // * is special case where it's always ok
    if label == "*" {
        return Ok(label.into());
    }

    if re().is_match(label) {
        Ok(label.into())
    } else {
        Err(eyre!(
            "Error parsing label - {label}. Must be of the form {LABEL}"
        ))
    }
}

const LABEL: &str = "^[a-zA-Z][a-zA-Z0-9+-_]+$";

fn re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| match Regex::new(LABEL) {
        Ok(re) => re,
        Err(err) => {
            panic!("Error parsing regex {LABEL} - {err}");
        }
    })
}

fn find_mode(v: TokenVal, operation: &Operation) -> AddressMode {
    match v {
        TokenVal::Val8(_) => match (operation.x_index, operation.y_index) {
            (true, false) => AddressMode::ZeroPageX,
            (false, true) => AddressMode::ZeroPageY,
            (false, false) => AddressMode::ZeroPage,
            (true, true) => {
                panic!("can't have x and y index set - {operation:?}");
            }
        },
        TokenVal::Val16(_) => match (operation.x_index, operation.y_index) {
            (true, false) => AddressMode::AbsoluteX,
            (false, true) => AddressMode::AbsoluteY,
            (false, false) => AddressMode::Absolute,
            (true, true) => {
                panic!("can't have x and y index set - {operation:?}");
            }
        },
    }
}

fn token_to_val_or_label(
    val: &str,
    operation: &mut Operation,
    ret: &mut ASTOutput,
    is_16: bool,
    line_num: usize,
    line: &str,
) -> Result<OpVal> {
    let x = match parse_val(val, is_16) {
        Some(v) => {
            if operation.mode == AddressMode::Implied {
                operation.mode = find_mode(v, operation);
            }
            OpVal::Val(v)
        }
        None => match parse_label(val) {
            Ok(label) => {
                add_label(false, ret, &label, None, line, line_num)?;
                OpVal::Label(label)
            }
            Err(err) => {
                return Err(eyre!(
                    "Error parsing line {}: invalid label or value {err} - {line}",
                    line_num + 1,
                ));
            }
        },
    };
    Ok(x)
}

// add_label does all the heavy lifting for either updating a label ref or
// defining a brand new one. As we can get labels in a few forms:
//
// LABEL OP .. - definition
// LABEL EQU X - definition
// OP LABEL - reference
//
// Handling all of these needs a few options. Opcode parsing (references) should
// set label_def to false. Always pass a value in if known.
fn add_label(
    label_def: bool,
    ret: &mut ASTOutput,
    label: &String,
    val: Option<TokenVal>,
    line: &str,
    line_num: usize,
) -> Result<()> {
    if let Some(ld) = ret.labels.get_mut(label) {
        // Could be a reference created this so there's no line number. If there is one this is a duplicate.
        if label_def {
            if ld.line != 0 {
                return Err(eyre!("Error parsing line {}: can't redefine location label {label}. Already defined at line {} - {line}", line_num+1, ld.line));
            }
            ld.val = val;
            ld.line = line_num + 1;
        } else {
            ld.refs.push(line_num + 1);
        }
    } else {
        // Defining a new label means no references yet. Anything else is ref
        // placeholder so include this line as a starting ref.
        let refs = if label_def {
            Vec::new()
        } else {
            vec![line_num + 1]
        };
        // Same for line number. If this is a definition this is the line we
        // record. References leave this blank for later updates (above).
        let l = if label_def {
            line_num + 1
        } else {
            //  If this is the first mention of this label we'll have to create a placeholder
            // definition.
            0
        };

        // Don't have to validate label since already did it above before
        // progressing here.
        ret.labels
            .insert(label.clone(), LabelDef { val, line: l, refs });
    }
    Ok(())
}
