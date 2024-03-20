//! `assemble` provides methods for processing a list of input
//! lines into a binary 64k image file suitable for a 6502
//! system.

use color_eyre::eyre::{eyre, Result};
use regex::Regex;
use rusty6502::prelude::*;
use std::collections::HashMap;
use std::fmt::Write;
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
// LABEL[:] <EQU|.EQU|=> <u8|u16> - A label followed by equality setting the label to the indicated value.
// [<LABEL>]: - A single line label defining a location with no other data.
// [<LABEL>][:] WORD|.WORD <Val16> [<Val16> ...] - One or more u16 values with an optional
//                                        label defining the location.
// [<LABEL>][:] BYTE|.BYTE <Val8>|STRING [<Val8>|STRING ...] - One or more u8/string values
//                                                    with an optional label defining the location.
//                                                    Strings are automatically expanded but not NUL
//                                                    terminated (use ASCIIZ for that).
// [<LABEL>][:] ASCIIZ|.ASCIIZ STRING - An ASCII string that automatically appends a NUL on expansion.
// [<LABEL>][:] OPCODE [<Val>|] - A valid 6502 opcode with an optional label defining
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
    Label(String),
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
    Line(String),
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

// OpVal8 is similar to an OpVal but is constrained to an 8 bit value only.
// Mostly used with WORD/BYTE to emit a series of values or label values.
#[derive(Clone, Debug, PartialEq)]
enum OpVal8 {
    Label(String),
    Val(u8),
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
#[derive(Debug)]
pub struct Assembly {
    /// The binary image. Always 64k to represent a complete
    /// memory image (often used to simulate a cartridge).
    pub bin: [u8; 1 << 16],
    /// A listing file of the translated input.
    pub listing: String,
}

struct ASTOutput {
    ast: Vec<Vec<Token>>, // A set of lines each of which is N tokens.
    labels: HashMap<String, LabelDef>,
}

// pass1 does the initial AST build for the input given.
// It will compute an AST from the file reader along with a label map
// that has references to both fully defined labels (EQU) and references
// to location labels (either defs or refs).
fn pass1(cpu: &dyn CPU, lines: Lines<BufReader<File>>) -> Result<ASTOutput> {
    let mut ret = ASTOutput {
        ast: Vec::<Vec<Token>>::new(),
        labels: HashMap::new(),
    };

    for (line_num, line) in lines.map_while(Result::ok).enumerate() {
        // We only support ASCII encoded files (parsing is far simpler).
        // TODO(jchacon): Add support for comments?
        if !line.is_ascii() {
            return Err(eyre!(
                "Input line has non ASCII characters which is not currently supported - {line}"
            ));
        }

        let fields: Vec<&str> = line.split_whitespace().collect();

        // Always record a copy of the line unless it was a blank line
        // This way generate output can emit this so we can see the original
        // constants used ("a" for instance) which is otherwise hard to retain
        // post processing.
        let mut tokens = if fields.is_empty() {
            Vec::<Token>::new()
        } else {
            Vec::<Token>::from([Token::Line(line.clone())])
        };

        // Every line starts here. If there are no tokens we'll just fall
        // through into the final state handling after the loop below.
        let mut state = State::Begin;

        for (index, token) in fields.iter().copied().enumerate() {
            let upper = token.to_uppercase();

            state = match state {
                State::Begin => beginning_states(token, &upper, &line, line_num)?,
                State::Label(label) => {
                    label_state(label, &mut ret, token, &upper, &mut tokens, &line, line_num)?
                }

                // An ORG statement must be followed by a u16 value.
                State::Org => org_state(token, &mut tokens, &line, line_num)?,

                // Word and Byte are effectively the same logic except which size
                // is required (bytes can't be 16 bit).
                State::Word(md) => {
                    word_byte_state(token, &mut ret, md, &mut tokens, true, &line, line_num)?
                }
                State::Byte(md) => {
                    word_byte_state(token, &mut ret, md, &mut tokens, false, &line, line_num)?
                }

                // Asciiz parsing.
                State::AsciiZ(md) => asciiz_state(token, md, &mut tokens, &line, line_num)?,

                // Remainder is an intermediate state after we've processed something.
                // At this point we can only define a comment and move to that stage.
                // If instead this ran out of tokens in fields the loop would end and the
                // next line starts at Begin automatically.
                // Needs the line_done field as ASCIIZ and comment support parses the whole line
                // and this gives us a way out of the enclosing token parsing loop
                // (it'll just end up here for all the tokens and fall out).
                State::Remainder(line_done) => remainder_state(line_done, token, &line, line_num)?,

                // For comments take everything left, construct a comment and then jump to Remainder
                // to ignore the remaining tokens.
                State::Comment(c) => {
                    // A parsed comment is anything after the ; plus
                    // the remaining fields space separated.
                    tokens.push(Token::Comment(format!(
                        ";{c} {}",
                        fields.as_slice()[index..].join(" ")
                    )));
                    State::Remainder(true)
                }

                // EQU is label EQU <val> so process the value and push a new TokenDef into tokens.
                State::Equ(label) => {
                    equ_state(token, &mut ret, &mut tokens, label, &line, line_num)?
                }
                State::Op(op) => op_state(token, &mut ret, &mut tokens, cpu, op, &line, line_num)?,
            };
        }

        // At this point we've run out of tokens and are in some final state.
        // Some are valid and some are not:
        //
        // Handle the case of a line of ";". i.e. nothing trailing the ; so we don't loop around
        // to process the Comment state. Properly processing would have left us in Begin instead.
        // It may also be a single token opcode such as SEI which has no operand value/label.
        match state {
            // These are valid states to exit so we can ignore them.
            State::Begin | State::Remainder(_) => {}

            // Comment with nothing left.
            State::Comment(c) => {
                tokens.push(Token::Comment(format!(";{c}")));
            }

            // Single opcode (SEI)
            State::Op(op) => tokens.push(Token::Op(Operation {
                op,
                mode: AddressMode::Implied,
                ..Default::default()
            })),

            // Lots of label cases
            State::Label(label) => {
                // Handle
                //
                // LABEL:
                //
                // Correctly by directly adding the label now. The loop above
                // can only handle
                //
                // LABEL: ; Some comments
                add_label(true, &mut ret, &label, None, &line, line_num)?;
                tokens.push(Token::Label(label));
            }

            // Word/Byte gets here if we processed and then ended without a comment
            State::Word(md) => {
                tokens.push(Token::Word(md));
            }
            State::Byte(md) => {
                tokens.push(Token::Byte(md));
            }

            // Org can happen if they start and then end the line.
            State::Org => {
                return Err(eyre!(
                    "Error parsing line {}: missing data for {state:?} - {line}",
                    line_num + 1
                ));
            }

            // Anything else is an internal error which shouldn't be possible.
            // i.e. EQU and ASCIIZ handle all their parsing and can't fall through here.
            State::Equ(_) | State::AsciiZ(_) => {
                panic!(
                    "Internal error parsing line {}: invalid state {state:?} - {line}",
                    line_num + 1
                )
            }
        };
        ret.ast.push(tokens);
    }
    Ok(ret)
}

fn beginning_states(token: &str, upper: &str, line: &str, line_num: usize) -> Result<State> {
    match upper {
        // We can match an ORG, WORD, BYTE or ASCIIZ here directly.
        "ORG" | ".ORG" => Ok(State::Org),
        "WORD" | ".WORD" => Ok(State::Word(vec![])),
        "BYTE" | ".BYTE" => Ok(State::Byte(vec![])),
        "ASCIIZ" | ".ASCIIZ" => Ok(State::AsciiZ(vec![])),
        // Anything else is either a comment, opcode or a label to be fleshed
        // out in a later state.
        _ => {
            if upper.starts_with(';') {
                Ok(State::Comment(token[1..].into()))
            } else if let Ok(op) = Opcode::from_str(token) {
                Ok(State::Op(op))
            } else {
                // Must be a label.

                // Labels are allowed to end with one optional colon
                // (helps visually sometimes) but it's not part of
                // the label so remove it if here.
                let token = if let Some(tok) = token.strip_suffix(':') {
                    tok
                } else {
                    token
                };
                match parse_label(token) {
                    Ok(label) => Ok(State::Label(label)),
                    Err(err) => {
                        return Err(eyre!(
                            "Error parsing line {}: invalid label {err} - {line}",
                            line_num + 1
                        ));
                    }
                }
            }
        }
    }
}

fn label_state(
    label: String,
    ret: &mut ASTOutput,
    token: &str,
    upper: &str,
    tokens: &mut Vec<Token>,
    line: &str,
    line_num: usize,
) -> Result<State> {
    // We may get here a few ways
    //
    // Single line label w. comments:
    //   LABEL[:] ; comments
    // Label with an opcode:
    //   LABEL[:] OP OPVAL [; [comments]]
    // Label with a definition
    //   LABEL[:] <WORD|BYTE|EQU|ASCIIZ> <val>

    if let Some(tok) = token.strip_prefix(';') {
        add_label(true, ret, &label, None, line, line_num)?;
        tokens.push(Token::Label(label));
        Ok(State::Comment(tok.into()))
    } else {
        match upper {
            // Labels can be EQU style in which case we need one more state to get
            // the value.
            "EQU" | ".EQU" | "=" => {
                let fc = line.split_whitespace().count();
                if fc < 3 {
                    return Err(eyre!(
                        "Error parsing line {}: invalid EQU - {line}",
                        line_num + 1
                    ));
                }
                // Don't have to validate label since State::Begin did it above before
                // progressing here.
                Ok(State::Equ(label))
            }
            "WORD" | ".WORD" => {
                add_label(true, ret, &label, None, line, line_num)?;
                tokens.push(Token::Label(label));
                Ok(State::Word(vec![]))
            }
            "BYTE" | ".BYTE" => {
                add_label(true, ret, &label, None, line, line_num)?;
                tokens.push(Token::Label(label));
                Ok(State::Byte(vec![]))
            }
            "ASCIIZ" | ".ASCIIZ" => {
                add_label(true, ret, &label, None, line, line_num)?;
                tokens.push(Token::Label(label));
                Ok(State::AsciiZ(vec![]))
            }

            // Otherwise this should be an opcode and we can push a label
            // into tokens. Phase 2 will figure out its value.
            _ => match Opcode::from_str(upper) {
                Ok(op) => {
                    add_label(true, ret, &label, None, line, line_num)?;
                    tokens.push(Token::Label(label));
                    Ok(State::Op(op))
                }
                Err(_) => Err(eyre!(
                    "Error parsing line {}: invalid opcode '{token}' - {line}",
                    line_num + 1
                )),
            },
        }
    }
}

fn word_byte_state(
    token: &str,
    ret: &mut ASTOutput,
    mut md: Vec<MemDef>,
    tokens: &mut Vec<Token>,
    is_word: bool,
    line: &str,
    line_num: usize,
) -> Result<State> {
    // We may have tokens left which is a comment so check for that and move to that phase.
    if let Some(token) = token.strip_prefix(';') {
        if md.is_empty() {
            // Account for '<WORD|BYTE> ; some comments'
            return Err(eyre!(
                "Error parsing line {}: WORD or BYTE without value - {line}",
                line_num + 1
            ));
        }

        let tok = if is_word {
            Token::Word(md)
        } else {
            Token::Byte(md)
        };
        tokens.push(tok);
        return Ok(State::Comment(token.into()));
    }

    // Below for all MemDef the PC value is computed and changed during
    // `compute_refs`.
    match token_to_val_or_label(token, ret, is_word, line, line_num)? {
        OpVal::Label(l) => {
            md.push(MemDef {
                pc: 0x0000,
                val: OpVal8::Label(l),
            });
            if is_word {
                Ok(State::Word(md))
            } else {
                Ok(State::Byte(md))
            }
        }
        OpVal::Val(v) => {
            match v {
                TokenVal::Val8(v) => {
                    md.push(MemDef {
                        pc: 0x0000,
                        val: OpVal8::Val(v),
                    });
                    Ok(State::Byte(md))
                }
                TokenVal::Val16(v) => {
                    if !is_word {
                        return Err(eyre!("Error parsing line {}: invalid BYTE value not 8 bit - {token} - {line}", line_num + 1));
                    }
                    // Even though we must parse as a u16 we store this in little endian
                    // form so we can emit straight into memory. Plus we don't need
                    // a separate form for Byte vs Word then.
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
                    Ok(State::Word(md))
                }
            }
        }
    }
}

fn asciiz_state(
    token: &str,
    mut md: Vec<MemDef>,
    tokens: &mut Vec<Token>,
    line: &str,
    line_num: usize,
) -> Result<State> {
    // If this is a comment and we've never defined anything something is wrong.
    // The rest of comment parsing happens below since this handles the entire
    // line parsing here vs returned state like WORD/BYTE.
    if token.starts_with(';') && md.is_empty() {
        // Account for '<ASCIIZ> ; some comments'
        return Err(eyre!(
            "Error parsing line {}: ASCIIZ without value - {line}",
            line_num + 1
        ));
    }

    // This gets a little harder. We'll start with a token that has
    // to start with a quote. But...it could have whitespace inside
    // of it so we really have to parse the rest of the line here.
    if !token.starts_with('"') {
        return Err(eyre!("Error parsing line {}: ASCIIZ must be of the form \"XXX\" bad token '{token}' - {line}", line_num + 1));
    };

    // Un-escape the special 3 chars through the whole input string.
    // This will never support anything else as raw bytes can just be emitted
    // with BYTE instead at that point.
    let st = line
        .replace("\\n", "\n")
        .replace("\\r", "\r")
        .replace("\\t", "\t");

    // We know this contains ASCIIZ or else we
    // can't get to this state. And it must have
    // a whitespace char after that due to the initial match from Begin.
    let mut idx = st
        .to_uppercase()
        .find("ASCIIZ")
        .unwrap_or_else(|| panic!("ASCIIZ state without token in line - {line}?"))
        + "ASCIIZ".len();

    let bytes = st.as_bytes();
    for i in bytes.iter().skip(idx) {
        if !i.is_ascii_whitespace() {
            break;
        }
        idx += 1;
    }
    // We know the first thing after ASCIIZ was a proper string
    // beginning (see the beginning of this block) so just move
    // idx over to account (no need to check).
    idx += 1;

    let mut in_str = true;

    // When we're not in a string and find a comment block, stuff it all here.
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
                // Push the current character (which we know is ASCII)
                md.push(MemDef {
                    pc: 0x0000,
                    val: OpVal8::Val(*i),
                });
            }
            continue;
        }

        // Not in a string
        if *i == b'"' {
            in_str = true;
        }
        if *i == b';' {
            // Comment so we're done.

            // We never changed this so it's a valid utf8 string still from
            // here forward since this was always an ASCII string.
            comment = String::from_utf8(bytes[idx - 1..].to_vec())?;
        }
    }
    // Always push an ASCIIZ token along with possibly a comment and we always
    // end up in the remainder state.
    tokens.push(Token::AsciiZ(md));
    if !comment.is_empty() {
        tokens.push(Token::Comment(comment));
    }
    Ok(State::Remainder(true))
}

fn remainder_state(line_done: bool, token: &str, line: &str, line_num: usize) -> Result<State> {
    // Just keep looping here until the line runs out of tokens.
    if line_done {
        return Ok(State::Remainder(line_done));
    }

    match token.as_bytes() {
            [b';', ..] => Ok(State::Comment(token[1..].into())),
            _ => Err(eyre!("Error parsing line {}: only comment after parsed tokens allowed - remainder {token} - {line}", line_num + 1)),
        }
}

fn org_state(token: &str, tokens: &mut Vec<Token>, line: &str, line_num: usize) -> Result<State> {
    let Some(TokenVal::Val16(pc)) = parse_val(token, true) else {
        return Err(eyre!(
            "Error parsing line {}: invalid ORG value not 16 bit - {token} - {line}",
            line_num + 1,
        ));
    };
    tokens.push(Token::Org(pc));
    Ok(State::Remainder(false))
}

fn equ_state(
    token: &str,
    ret: &mut ASTOutput,
    tokens: &mut Vec<Token>,
    label: String,
    line: &str,
    line_num: usize,
) -> Result<State> {
    // Find a value and then create the token.
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
    add_label(true, ret, &label, Some(val), line, line_num)?;
    tokens.push(Token::Equ(label));
    Ok(State::Remainder(false))
}

fn op_state(
    token: &str,
    ret: &mut ASTOutput,
    tokens: &mut Vec<Token>,
    cpu: &dyn CPU,
    op: Opcode,
    line: &str,
    line_num: usize,
) -> Result<State> {
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
        tokens.push(Token::Op(operation));
        return Ok(State::Comment(token[1..].into()));
    }

    if cpu
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
            return Err(eyre!("Invalid zero page relative (index {pre} too large - greater than 7) on line {} - {token}", line_num + 1));
        }
        let zpaddr = token_to_val_or_label(parts[1], ret, false, line, line_num)?;
        let dest = token_to_val_or_label(parts[2], ret, false, line, line_num)?;
        operation.op_val = Some(vec![OpVal::Val(TokenVal::Val8(pre)), zpaddr, dest]);
        tokens.push(Token::Op(operation));
        return Ok(State::Remainder(false));
    }

    if cpu
        .resolve_opcode(&operation.op, &AddressMode::Relative)
        .is_ok()
    {
        // Ops which are relative only have this mode so we can just assign
        // and parse the val which must be 8 bit.
        // Can't validate value now as while it's 8 bit this can be 16 bit (via
        // a label or *) and mean a PC difference.
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
    // We know the remainder is valid utf8 since we only removed ASCII
    // and started with a valid utf8 ASCII str.
    let op_val = std::str::from_utf8(val)?;

    let ov = token_to_val_or_label(op_val, ret, false, line, line_num)?;
    match ov {
        OpVal::Label(_) => {}
        OpVal::Val(v) => {
            // Fixup the address mode now that we have an arg.
            if operation.mode == AddressMode::Implied {
                operation.mode = find_mode(v, &operation);
            }
        }
    };
    operation.op_val = Some(vec![ov]);
    tokens.push(Token::Op(operation));
    Ok(State::Remainder(false))
}

// compute_refs takes the previous AST output and cross references all labels,
// builds PC values for each operation and updates the AST with these results.
// The only thing it leaves is relative address computation since that can't be
// known until all labels are fully cross referenced. That will be handled during
// byte code generation.
fn compute_refs(cpu: &dyn CPU, ast_output: &mut ASTOutput) -> Result<()> {
    // Start the initial PC at 0x0000 until something resets it via ORG.
    let mut pc: u16 = 0x0000;

    for (line_num, line) in ast_output.ast.iter_mut().enumerate() {
        // For each line process the token list.
        for t in line.iter_mut() {
            match t {
                // Empty states we don't process.
                Token::Equ(_) | Token::Comment(_) | Token::Line(_) => {}

                // Stand alone labels are location labels so just assign those to
                // the current PC.
                Token::Label(s) => {
                    // A left side label is the PC value.
                    get_label_mut(&mut ast_output.labels, s).val = Some(TokenVal::Val16(pc));
                }

                // ORG resets the PC.
                Token::Org(npc) => {
                    pc = *npc;
                }

                // Word, Byte and AsciiZ all just emit a set of bytes that move the PC along.
                Token::Word(md) | Token::Byte(md) | Token::AsciiZ(md) => {
                    for m in md.iter_mut() {
                        m.pc = pc;
                        // If this is a label ref this is always a 2 byte jump.
                        if let OpVal8::Label(_) = m.val {
                            pc += 2;
                        } else {
                            pc += 1;
                        }
                    }
                }

                // The meat is the opcode which can contain label refs.
                Token::Op(o) => {
                    compute_opcode_refs(o, &mut ast_output.labels, &mut pc, cpu, line_num)?;
                }
            };
        }
    }
    Ok(())
}

fn compute_opcode_refs(
    o: &mut Operation,
    labels: &mut HashMap<String, LabelDef>,
    pc: &mut u16,
    cpu: &dyn CPU,
    line_num: usize,
) -> Result<()> {
    let mut width: u16 = 2;
    let mut ok = false;
    match o.mode {
        // Implied gets handled here as it could resolve into another mode
        // still due to not knowing all labels earlier.
        AddressMode::Implied | AddressMode::NOPCmos => {
            // Check the operand and if there is one it means
            // this isn't Implied and we need to use this to compute either ZeroPage or Absolute
            if let Some(v) = &o.op_val {
                if let OpVal::Label(l) = &v[0] {
                    let ld = get_label(labels, l);
                    if let Some(TokenVal::Val8(_)) = ld.val {
                        o.mode = find_mode(TokenVal::Val8(0), o);
                    } else {
                        // Anything else is either 16 bit or a label we don't know which
                        // also must be 16 bit at this point.
                        o.mode = find_mode(TokenVal::Val16(0), o);
                        width = 3;
                    }
                }
            } else {
                // Anything else was a direct value and already matched in pass1 or is actually implied.
                // If it directly matched that'll resolve below.
                width = 1;
            }
        }
        AddressMode::Immediate
        | AddressMode::ZeroPage
        | AddressMode::ZeroPageX
        | AddressMode::ZeroPageY
        | AddressMode::Indirect
        | AddressMode::IndirectX
        | AddressMode::IndirectY => {
            if let Some(v) = &o.op_val {
                match &v[0] {
                    OpVal::Label(l) => {
                        if let Some(TokenVal::Val8(_)) = get_label(labels, l).val {
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
            width = 3;
            if let Some(v) = &o.op_val {
                match &v[0] {
                    OpVal::Label(l) => {
                        if let Some(TokenVal::Val16(_)) = get_label(labels, l).val {
                            ok = true;
                        } else if get_label(labels, l).val.is_none() {
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
            }
            if !ok {
                return Err(eyre!(
                    "Error parsing line {}: {} must have a 16 bit arg",
                    line_num + 1,
                    o.mode
                ));
            }
        }
        // Width is already correct. In byte emission below it'll compute and validate
        // the actual offset. This is due to forward label refs. i.e. BEQ DONE before DONE
        // has been processed so we don't know the PC value yet but will below.
        AddressMode::Relative => {}
        AddressMode::ZeroPageRelative => {
            // We just add width here. In byte emission below it'll compute and validate
            // the actual offset. This is due to forward label refs. i.e. BBR 0,0x12,DONE before DONE
            // has been processed so we don't know the PC value yet but will below.
            width = 3;
        }
    }
    // Check final mode here.
    if cpu.resolve_opcode(&o.op, &o.mode).is_err() {
        return Err(eyre!(
            "Error parsing line {}: opcode {} doesn't support mode - {}",
            line_num + 1,
            o.op,
            o.mode
        ));
    }

    o.pc = *pc;
    o.width = width;
    *pc += width;
    Ok(())
}

// OutputArgs contains the comment args all the helper routines need.
struct OutputArgs<'a> {
    res: &'a mut Assembly,
    output: &'a mut String,
    label_out: &'a mut String,
    labels: &'a HashMap<String, LabelDef>,
}

// generate_output does the final byte code and listing file generation from
// the previously generated AST. This must be mutable as final relative addresses
// are computed before byte codes are generated for a given operation.
fn generate_output(cpu: &dyn CPU, ast_output: &mut ASTOutput) -> Result<Assembly> {
    // Always emit 64k so just allocate a block. Further uses can extract the
    // exact section they want.
    let mut res = Assembly {
        bin: [0; 1 << 16],
        listing: String::new(),
    };

    for (line_num, line) in ast_output.ast.iter_mut().enumerate() {
        // output is where the whole line gets assembled.
        // label_out is used for holding location labels to later be inserted
        // into output in the correct place.
        let (mut output, mut label_out) = (String::new(), String::new());
        for (index, t) in line.iter_mut().enumerate() {
            match t {
                Token::Line(line) => {
                    // Print the original input line as a comment in the output
                    writeln!(output, "; {line}").unwrap();
                }
                Token::Label(td) => {
                    // Location labels are always left justified
                    write!(label_out, "{td:<8}").unwrap();
                }
                Token::Org(pc) => {
                    // ORG statements line up with EQU
                    write!(output, "{:<13} {:<8} {pc:<04X}", " ", "ORG").unwrap();
                }
                Token::AsciiZ(md) => asciiz_output(
                    &mut OutputArgs {
                        res: &mut res,
                        output: &mut output,
                        label_out: &mut label_out,
                        labels: &ast_output.labels,
                    },
                    md,
                )?,

                // WORD and BYTE are effectively the same logic with very few differences
                Token::Word(md) => word_byte_output(
                    &mut OutputArgs {
                        res: &mut res,
                        output: &mut output,
                        label_out: &mut label_out,
                        labels: &ast_output.labels,
                    },
                    md,
                    true,
                ),
                Token::Byte(md) => word_byte_output(
                    &mut OutputArgs {
                        res: &mut res,
                        output: &mut output,
                        label_out: &mut label_out,
                        labels: &ast_output.labels,
                    },
                    md,
                    false,
                ),
                Token::Equ(td) => equ_output(
                    &mut OutputArgs {
                        res: &mut res,
                        output: &mut output,
                        label_out: &mut label_out,
                        labels: &ast_output.labels,
                    },
                    td,
                    &ast_output.labels,
                ),
                Token::Comment(c) => {
                    comment_output(
                        &mut OutputArgs {
                            res: &mut res,
                            output: &mut output,
                            label_out: &mut label_out,
                            labels: &ast_output.labels,
                        },
                        c,
                        index,
                    );
                }

                // The meat of it all: print the opcode and its args.
                Token::Op(o) => op_output(
                    &mut OutputArgs {
                        res: &mut res,
                        output: &mut output,
                        label_out: &mut label_out,
                        labels: &ast_output.labels,
                    },
                    o,
                    cpu,
                    line_num,
                )?,
            }
        }

        // If we get here with label_out still containing something it means
        // this was a line just with a label:
        //
        // LABEL[:]
        //
        // So print it along with it's PC value.
        if !label_out.is_empty() {
            let lbl = String::from(label_out.as_str().trim_end());
            let ld = get_label_val(&ast_output.labels, &lbl);

            let TokenVal::Val16(v) = ld else {
                panic!("Single line label {lbl} must be a PC value!");
            };
            write!(output, "{v:04X}          {label_out}").unwrap();
        }

        // If this was a blank line that also auto-parses for output purposes.
        writeln!(res.listing, "{output}").unwrap();
    }
    Ok(res)
}

fn word_byte_output(oa: &mut OutputArgs, md: &[MemDef], is_word: bool) {
    // Other than the type these are very similar as BYTE may have a label ref
    // which could be a u16 and is then treated as 2 bytes. So similar to WORD
    // enough most of the logic is the same.
    let t = if is_word { "WORD" } else { "BYTE" };
    let (mut post_bytes, mut byte_dump) = compute_md_leader(oa, md, t);

    // March through the vec manually as WORD does things in 2 byte chunks which
    // requires an extra iteration per loop.
    let mut it = md.iter();
    while let Some(m) = it.next() {
        let (val, lbl) = match &m.val {
            OpVal8::Label(l) => match get_label_val(oa.labels, l) {
                TokenVal::Val8(v) => (vec![v], l.clone()),
                TokenVal::Val16(v) => {
                    let low = (v & 0x00FF) as u8;
                    let high = ((v & 0xFF00) >> 8) as u8;
                    (vec![low, high], l.clone())
                }
            },
            OpVal8::Val(v) => {
                if is_word {
                    // Words are 2 bytes so extract and then get the next one.
                    let low = *v;
                    // Impossible state to have a word def with no values and already checked.
                    let m = it
                        .next()
                        .unwrap_or_else(|| panic!("Iterator expired early for WORD?"));
                    let OpVal8::Val(high) = m.val else {
                        panic!("Impossible state. One byte followed by label for WORD");
                    };

                    (vec![low, high], String::new())
                } else {
                    // Whereas bytes are always one value.
                    (vec![*v], String::new())
                }
            }
        };
        if lbl.is_empty() {
            if is_word {
                let val = u16::from(val[1]) << 8 | u16::from(val[0]);
                write!(post_bytes, " {val:04X}").unwrap();
            } else {
                write!(post_bytes, " {:02X}", val[0]).unwrap();
            }
        } else {
            write!(post_bytes, " {lbl}").unwrap();
        }
        for (p, v) in val.iter().enumerate() {
            write!(byte_dump, " {v:02X}").unwrap();
            oa.res.bin[usize::from(m.pc) + p] = *v;
        }
    }
    write!(oa.output, "{byte_dump} {post_bytes}").unwrap();
    oa.label_out.clear();
}

fn asciiz_output(oa: &mut OutputArgs, md: &[MemDef]) -> Result<()> {
    let (mut post_bytes, mut byte_dump) = compute_md_leader(oa, md, "ASCIIZ");
    // Starts with a "
    let mut val = String::from("\"");
    for m in md {
        let OpVal8::Val(v) = m.val else {
            panic!("Impossible ASCIIZ state!");
        };
        oa.res.bin[usize::from(m.pc)] = v;

        // End of string so process it.
        if v == 0x00 {
            write!(byte_dump, " 00").unwrap();
            write!(post_bytes, " {val}\"").unwrap();
            val.clear();
            val.push('"');
            continue;
        }
        if v == b'\n' {
            val.write_str("\\n")?;
        } else if v == b'\r' {
            val.write_str("\\r")?;
        } else if v == b'\t' {
            val.write_str("\\t")?;
        } else {
            val.push(char::from(v));
        }

        write!(byte_dump, " {v:02X}").unwrap();
    }
    write!(oa.output, "{byte_dump} {post_bytes}").unwrap();
    oa.label_out.clear();
    Ok(())
}

fn compute_md_leader(oa: &OutputArgs, md: &[MemDef], pre: &str) -> (String, String) {
    // Line things up so depending on how many bytes this emits for short runs
    // make it line up with opcode columns. Past that, nothing we can really do.
    let (leader, lbl) = match md.len() {
        2 => (4, 8),
        3 => (1, 8),
        _ => (0, 6),
    };
    let post_bytes = format!("{:<leader$}{:<lbl$}{pre} ", "", oa.label_out);

    let byte_dump = format!("{:04X}", md[0].pc);
    (post_bytes, byte_dump)
}

fn equ_output(oa: &mut OutputArgs, td: &String, labels: &HashMap<String, LabelDef>) {
    match get_label_val(labels, td) {
        TokenVal::Val8(v) => {
            write!(oa.output, "{td:<13} EQU      {v:02X}").unwrap();
        }
        TokenVal::Val16(v) => {
            write!(oa.output, "{td:<13} EQU      {v:04X}").unwrap();
        }
    };
}

fn comment_output(oa: &mut OutputArgs, c: &String, index: usize) {
    // If we get here with a non-empty label_out it means this
    // was a single entry so print it.
    if !oa.label_out.is_empty() {
        let lbl = String::from(oa.label_out.as_str().trim_end());
        let ld = get_label_val(oa.labels, &lbl);

        let TokenVal::Val16(v) = ld else {
            panic!("Single line label {lbl} must be a PC value!");
        };
        write!(oa.output, "{v:04X}          {}", oa.label_out).unwrap();
        oa.label_out.clear();
    }
    if index != 0 {
        write!(oa.output, " ").unwrap();
    }
    write!(oa.output, "{c}").unwrap();
}

fn op_output(oa: &mut OutputArgs, o: &mut Operation, cpu: &dyn CPU, line_num: usize) -> Result<()> {
    let modes = cpu.resolve_opcode(&o.op, &o.mode)?;

    assert!(
        !(o.op_val.is_none() && o.mode != AddressMode::Implied),
        "Internal error on line {}: no op val but not implied instruction for opcode {}",
        line_num + 1,
        o.op
    );
    assert!(
        !(o.mode == AddressMode::Implied && o.width != 1),
        "Internal error on line {}: implied mode for opcode {} but width not 1: {o:#?}",
        line_num + 1,
        o.op
    );

    // Things are mutable here as we may have to fixup op_val for branches before
    // emitting bytes below.
    if o.mode == AddressMode::Relative || o.mode == AddressMode::ZeroPageRelative {
        fixup_relative_addr(o, oa.labels, line_num)?;
    }

    // Grab the first entry in the bytes vec for the opcode value.
    // TODO(jchacon): If > 1 pick one randomly.
    if o.mode == AddressMode::ZeroPageRelative {
        // BBR/BBS are special. This is actually the vector offsets based on the first opval value.
        // We know this is fine since we range checked it above.
        let v = o
            .op_val
            .as_ref()
            .unwrap_or_else(|| panic!("op_val is None for zprel?"));
        println!("Got v: {v:?}");
        let OpVal::Val(TokenVal::Val8(offset)) = v[0] else {
            panic!("First value of ZeroPageRelative must be a u8");
        };
        oa.res.bin[usize::from(o.pc)] = modes[usize::from(offset)];
    } else {
        oa.res.bin[usize::from(o.pc)] = modes[0];
    }
    if oa.label_out.is_empty() {
        write!(oa.label_out, "{:<8}", "").unwrap();
    }
    if let Some(v) = &o.op_val {
        let test = if o.mode == AddressMode::ZeroPageRelative {
            &v[2]
        } else {
            &v[0]
        };
        let val = match test {
            OpVal::Label(s) => get_label_val(oa.labels, s),
            OpVal::Val(t) => *t,
        };
        match val {
            TokenVal::Val8(b) => {
                // ZP Relative is odd...It's width 3 but is all 8 bit values.
                if o.mode == AddressMode::ZeroPageRelative {
                    emit_zp_relative(oa, b, o, v, line_num)?;
                } else {
                    assert!(o.width == 2, "Internal error on line {}: got 8 bit value and expect 16 bit for op {} and mode {}", line_num+1, o.op, o.mode);

                    oa.res.bin[usize::from(o.pc + 1)] = b;
                    write!(
                        oa.output,
                        "{:04X} {:02X} {b:02X}    {} ",
                        o.pc, modes[0], oa.label_out
                    )
                    .unwrap();
                }
            }
            TokenVal::Val16(b) => {
                assert!(o.width == 3, "Internal error on line {}: got 16 bit value and expect 8 bit for op {} and mode {}", line_num+1, o.op, o.mode);

                // Store 16 bit values in little endian.
                let low = (b & 0x00FF) as u8;
                let high = ((b & 0xFF00) >> 8) as u8;
                oa.res.bin[usize::from(o.pc + 1)] = low;
                oa.res.bin[usize::from(o.pc + 2)] = high;
                write!(
                    oa.output,
                    "{:04X} {:02X} {low:02X} {high:02X} {} ",
                    o.pc, modes[0], oa.label_out
                )
                .unwrap();
            }
        };

        let mut val = String::new();
        cpu.disassemble(&mut val, o.pc, &oa.res.bin, true);
        write!(oa.output, "{val}").unwrap();
        oa.label_out.clear();
    } else {
        write!(
            oa.output,
            "{:04X} {:02X}       {} {:?}",
            o.pc, modes[0], oa.label_out, o.op
        )
        .unwrap();
        oa.label_out.clear();
    }
    Ok(())
}

fn fixup_relative_addr(
    o: &mut Operation,
    labels: &HashMap<String, LabelDef>,
    line_num: usize,
) -> Result<()> {
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
                match get_label(labels, l).val {
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
                    if (i16::from(i8::MIN)..=i16::from(i8::MAX)).contains(&diff) {
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
                            o.op_val = Some(vec![v[0].clone(), v[1].clone(), new]);
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
    }
    if !ok {
        return Err(eyre!(
            "Error parsing line {}: either not 8 bit or out of range for relative instruction",
            line_num + 1
        ));
    }
    Ok(())
}

fn emit_zp_relative(
    oa: &mut OutputArgs,
    b: u8,
    o: &Operation,
    v: &[OpVal],
    line_num: usize,
) -> Result<()> {
    assert!(
        o.width == 3,
        "Internal error on line {}: got wrong data for ZP rel on op {} and mode {}",
        line_num + 1,
        o.op,
        o.mode
    );

    let zpval = match &v[1] {
        OpVal::Label(s) => get_label_val(oa.labels, s),
        OpVal::Val(t) => *t,
    };
    let TokenVal::Val8(zp) = zpval else {
        return Err(eyre!(
            "{}: Invalid value for ZP address for ZP Relative on op {} and mode {}",
            line_num + 1,
            o.op,
            o.mode
        ));
    };

    oa.res.bin[usize::from(o.pc + 1)] = zp;
    oa.res.bin[usize::from(o.pc + 2)] = b;
    write!(
        oa.output,
        "{:04X} {:02X} {zp:02X} {b:02X} {} ",
        o.pc,
        oa.res.bin[usize::from(o.pc)],
        oa.label_out
    )
    .unwrap();
    Ok(())
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

// Given a labels map (label->LabelDef) return a labeldef ref directly w/o checking.
// This should be only called after AST building as it assumes all labels are valid.
fn get_label<'a>(hm: &'a HashMap<String, LabelDef>, label: &String) -> &'a LabelDef {
    hm.get(label)
        .unwrap_or_else(|| panic!("Missing label {label}"))
}

// Given a labels map (label->LabelDef) return the label's val directly w/o checking.
// This should be only called after AST building as it assumes all labels are valid.
fn get_label_val(hm: &HashMap<String, LabelDef>, label: &String) -> TokenVal {
    get_label(hm, label)
        .val
        .unwrap_or_else(|| panic!("Missing val for label {label}"))
}

// Given a labels map (label->LabelDef) return a mutable labeldef ref directly w/o checking.
// This should be only called after AST building as it assumes all labels are valid.
fn get_label_mut<'a>(hm: &'a mut HashMap<String, LabelDef>, label: &String) -> &'a mut LabelDef {
    hm.get_mut(label)
        .unwrap_or_else(|| panic!("Missing label {label}"))
}

// parse_val returns a parsed TokenVal8/16 or nothing. Set is_u16 to force returning a Val16 always
// if a value would parse.
// Also handles the case of "X" (i.e. explicit string surrounding a char).
// This will handle \n, \r and \t but any other escapes should just use direct values.
fn parse_val(val: &str, mut is_u16: bool) -> Option<TokenVal> {
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
        [b'"', _, b'"'] => (&val[1..2], 10, true),
        _ => (val, 10, false),
    };

    // If nothing was left we're done with no value.
    if trimmed.is_empty() {
        return None;
    }

    // For non decimal we can know that someone setting
    // 0x0004 means they want a 16 bit value. Same with large binary strings.
    if base == 16 && trimmed.len() > 2 {
        is_u16 = true;
    }
    if base == 2 && trimmed.len() > 8 {
        is_u16 = true;
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

const LABEL: &str = "^[a-zA-Z][a-zA-Z0-9-+_]+$";

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
    ret: &mut ASTOutput,
    is_16: bool,
    line: &str,
    line_num: usize,
) -> Result<OpVal> {
    let x = match parse_val(val, is_16) {
        Some(v) => OpVal::Val(v),
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
