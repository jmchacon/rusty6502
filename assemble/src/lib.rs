//! `assemble` provides methods for processing a list of input
//! lines into a binary 64k image file suitable for a 6502
//! system.

use color_eyre::eyre::{eyre, Result};
use lazy_static::lazy_static;
use regex::Regex;
use rusty6502::prelude::*;
use std::collections::HashMap;
use std::fmt::{self, Write};
use std::io::BufReader;
use std::num::Wrapping;
use std::str::FromStr;
use std::{fs::File, io::Lines};

// State defines the state machine for parsing a given line.
//
// Begin starts all lines and the progressions that are valid:
//
// Begin -> Label -> Equ -> Remainder -> Begin|Comment
//      |        |-> Op -> Remainder -> Begin|Comment
//      |-> Org -> Remainder -> Begin|Comment
//      |-> Op -> Remainder -> Begin|Comment
//      |-> Comment -> Begin
#[derive(PartialEq, Debug)]
enum State {
    Begin,
    Label(String),
    Equ(String),
    Org,
    Op(Opcode),
    Comment(String),
    Remainder,
}

// Token defines an entry on a given line post tokenizing.
// Some values (such as the Operation in Op) may only be
// partially filled in at this point until further parse
// rounds occur.
enum Token {
    Label(String),
    Org(u16),
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
    op_val: Option<OpVal>,
    x_index: bool,
    y_index: bool,
    width: u16,
    pc: u16,
}

// OpVal defines the operation value for an opcode.
// This is either an 8/16 bit value or a label which
// eventually will reference an 8/16 bit value.
#[derive(Debug, Clone, PartialEq)]
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
fn pass1(ty: Type, lines: Lines<BufReader<File>>) -> Result<ASTOutput> {
    let mut ret = ASTOutput {
        ast: Vec::<Vec<Token>>::new(),
        labels: HashMap::new(),
    };

    for (line_num, line) in lines.flatten().enumerate() {
        let fields: Vec<&str> = line.split_whitespace().collect();

        let mut l = Vec::<Token>::new();

        let mut state = State::Begin;

        for (index, token) in fields.iter().enumerate() {
            let upper = token.to_uppercase();

            state = match state {
                State::Begin => match upper.as_str().as_bytes() {
                    // We can match an ORG or comment here directly.
                    [b'O', b'R', b'G', ..] => State::Org,
                    [b';', ..] => State::Comment(String::from(&token[1..])),
                    // Anything else is either an opcode or a label to be fleshed
                    // out in a later state.
                    _ => {
                        if let Ok(op) = Opcode::from_str(token) {
                            State::Op(op)
                        } else {
                            match parse_label(*token) {
                                Ok(label) => State::Label(label),
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
                State::Label(label) => {
                    match upper.as_str() {
                        // Labels can be EQU style in which case we need one more state to get
                        // the value.
                        "EQU" => {
                            if fields.len() < 3 {
                                return Err(eyre!(
                                    "Error parsing line {}: invalid EQU - {line}",
                                    line_num + 1
                                ));
                            }
                            match parse_label(label.as_str()) {
                                Ok(_) => State::Equ(label),
                                Err(err) => {
                                    return Err(eyre!(
                                        "Error parsing line {}: invalid label {err} - {line}",
                                        line_num + 1
                                    ));
                                }
                            }
                        }
                        // Otherwise this should be an opcode and we can push a label
                        // into tokens. Phase 2 will figure out its value.
                        _ => match Opcode::from_str(upper.as_str()) {
                            Ok(op) => {
                                if let Some(ld) = ret.labels.get_mut(&label) {
                                    // Could be a reference created this so there's no line number. If there is one this is a duplicate.
                                    if ld.line != 0 {
                                        return Err(eyre!("Error parsing line {}: can't redefine location label {label}. Already defined at line {} - {line}", line_num+1, ld.line));
                                    }
                                    ld.line = line_num + 1;
                                } else {
                                    // Don't have to validate label since State::Begin did it above before
                                    // progressing here.
                                    ret.labels.insert(
                                        label.clone(),
                                        LabelDef {
                                            val: None,
                                            line: line_num + 1,
                                            refs: Vec::new(),
                                        },
                                    );
                                }
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
                // An ORG statement must be followed by a u16 value.
                State::Org => {
                    let pc = match parse_val(token, true) {
                        Some(TokenVal::Val16(v)) => v,
                        _ => {
                            return Err(eyre!(
                                "Error parsing line {}: invalid ORG value not 16 bit - {token} - {line}",
                                line_num + 1,
                            ));
                        }
                    };
                    l.push(Token::Org(pc));
                    State::Remainder
                }
                // Remainder is an intermediate state after we've processed something.
                // At this point we can only define a comment and move to that stage.
                // If instead this ran out of tokens in fields the loop would end and the
                // next line starts at Begin automatically.
                State::Remainder => match token.as_bytes() {
                    [b';', ..] => State::Comment(String::from(&token[1..])),
                    _ => {
                        return Err(eyre!(
                            "Error parsing line {}: only comment after parsed tokens allowed - {line}",
                            line_num + 1,
                        ));
                    }
                },
                // For comments take everything left, construct a comment and then abort the loop now.
                State::Comment(c) => {
                    let mut comment = String::from(";");
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
                    if let Some(ld) = ret.labels.get_mut(&label) {
                        // Could be a reference created this so there's no line number. If there is one this is a duplicate.
                        if ld.line != 0 {
                            return Err(eyre!("Error parsing line {}: can't redefine EQU label {label}. Already defined at line {} - {line}", line_num+1, ld.line));
                        }
                        ld.val = Some(val);
                        ld.line = line_num + 1;
                    } else {
                        // Don't have to validate label since State::Begin did it above before
                        // progressing here.
                        ret.labels.insert(
                            label.clone(),
                            LabelDef {
                                val: Some(val),
                                line: line_num + 1,
                                refs: Vec::new(),
                            },
                        );
                    }
                    l.push(Token::Equ(label));
                    State::Remainder
                }
                State::Op(op) => {
                    let mut operation = Operation {
                        op,
                        mode: AddressMode::Implied,
                        ..Default::default()
                    };
                    // If this has no operand we may only have a comment.
                    if token.as_bytes().first() == Some(&b';') {
                        operation.mode = AddressMode::Implied;
                        l.push(Token::Op(operation));
                        State::Comment(String::from(&token[1..]))
                    } else {
                        if resolve_opcode(ty, &operation.op, &AddressMode::Relative).is_ok() {
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
                            // Zero page X - (val,x)
                            [b'(', val @ .., b',', b'x' | b'X', b')'] => {
                                operation.mode = AddressMode::IndirectX;
                                val
                            }
                            // Zero page Y - (val),y
                            [b'(', val @ .., b')', b',', b'y' | b'Y'] => {
                                operation.mode = AddressMode::IndirectY;
                                val
                            }
                            // Indirect - (val)
                            [b'(', val @ .., b')'] => {
                                operation.mode = AddressMode::Indirect;
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
                        // Safety: We know the remainder is valid utf8 since we only removed ASCII
                        //         and started with a valid utf8 str.
                        let op_val = unsafe { std::str::from_utf8(val).unwrap_unchecked() };

                        operation.op_val = match parse_val(op_val, false) {
                            Some(v) => {
                                if operation.mode == AddressMode::Implied {
                                    match v {
                                        TokenVal::Val8(_) => {
                                            match (operation.x_index, operation.y_index) {
                                                (true, false) => {
                                                    operation.mode = AddressMode::ZeroPageX;
                                                }
                                                (false, true) => {
                                                    operation.mode = AddressMode::ZeroPageY;
                                                }
                                                (false, false) => {
                                                    operation.mode = AddressMode::ZeroPage;
                                                }
                                                (true, true) => {
                                                    return Err(eyre!("Internal error line {}: can't have x and y index set", line_num+1));
                                                }
                                            }
                                        }
                                        TokenVal::Val16(_) => {
                                            match (operation.x_index, operation.y_index) {
                                                (true, false) => {
                                                    operation.mode = AddressMode::AbsoluteX;
                                                }
                                                (false, true) => {
                                                    operation.mode = AddressMode::AbsoluteY;
                                                }
                                                (false, false) => {
                                                    operation.mode = AddressMode::Absolute;
                                                }
                                                (true, true) => {
                                                    return Err(eyre!("Internal error line {}: can't have x and y index set", line_num+1));
                                                }
                                            }
                                        }
                                    };
                                }
                                Some(OpVal::Val(v))
                            }
                            None => match parse_label(op_val) {
                                Ok(label) => {
                                    if let Some(ld) = ret.labels.get_mut(&label) {
                                        ld.refs.push(line_num + 1);
                                    } else {
                                        // If this is the first mention of this label we'll have to create a placeholder
                                        // definition.
                                        ret.labels.insert(
                                            label.clone(),
                                            LabelDef {
                                                val: None,
                                                line: 0,
                                                refs: vec![line_num + 1],
                                            },
                                        );
                                    }
                                    Some(OpVal::Label(label))
                                }
                                Err(err) => {
                                    return Err(eyre!(
                                        "Error parsing line {}: invalid label {err} - {line}",
                                        line_num + 1,
                                    ));
                                }
                            },
                        };
                        l.push(Token::Op(operation));
                        State::Remainder
                    }
                }
            };
        }
        // Handle the case of a line of ";". i.e. nothing trailing the ; so we don't loop around
        // to process the Comment state. Properly processing would have left us in Begin instead.
        // It may also be a single token opcode such as SEI which has no operand value/label.
        match state {
            State::Comment(c) => {
                let mut comment = String::from(";");
                comment += c.as_str();
                l.push(Token::Comment(comment));
            }
            State::Op(op) => l.push(Token::Op(Operation {
                op,
                mode: AddressMode::Implied,
                ..Default::default()
            })),
            State::Begin | State::Remainder => {}
            State::Label(_) | State::Equ(_) | State::Org => {
                return Err(eyre!(
                    "Error parsing line {}: invalid state {state:?} - {line}",
                    line_num + 1,
                ));
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
fn compute_refs(ty: Type, ast_output: &mut ASTOutput) -> Result<()> {
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
                Token::Equ(_) | Token::Comment(_) => {}
                Token::Op(o) => {
                    let op = &o.op;
                    let width: u16;
                    if o.mode != AddressMode::Implied {
                        // Implied gets handled in the match as it could resolve
                        // into another mode.
                        if resolve_opcode(ty, op, &o.mode).is_err() {
                            return Err(eyre!(
                                "Error parsing line {}: opcode {op} doesn't support mode {}",
                                line_num + 1,
                                o.mode
                            ));
                        }
                    }
                    match o.mode {
                        AddressMode::Implied => {
                            // Check the operand and if there is one it means
                            // this isn't Implied and we need to use this to compute either ZeroPage or Absolute
                            match &o.op_val {
                                Some(OpVal::Label(l)) => {
                                    let ld = get_label(&ast_output.labels, l);
                                    if let Some(TokenVal::Val8(_)) = ld.val {
                                        match (o.x_index, o.y_index) {
                                            (true, false) => {
                                                o.mode = AddressMode::ZeroPageX;
                                            }
                                            (false, true) => {
                                                o.mode = AddressMode::ZeroPageY;
                                            }
                                            (false, false) => {
                                                o.mode = AddressMode::ZeroPage;
                                            }
                                            (true, true) => {
                                                return Err(eyre!("Error parsing line {}: can't have both x and y index", line_num+1));
                                            }
                                        }
                                        width = 2;
                                    } else {
                                        // Anything else is either 16 bit or a label we don't know which
                                        // also must be 16 bit at this point.
                                        match (o.x_index, o.y_index) {
                                            (true, false) => {
                                                o.mode = AddressMode::AbsoluteX;
                                            }
                                            (false, true) => {
                                                o.mode = AddressMode::AbsoluteY;
                                            }
                                            (false, false) => {
                                                o.mode = AddressMode::Absolute;
                                            }
                                            (true, true) => {
                                                return Err(eyre!("Error parsing line {}: can't have both x and y index", line_num+1));
                                            }
                                        };
                                        width = 3;
                                    }
                                }
                                Some(OpVal::Val(TokenVal::Val8(_))) => {
                                    o.mode = AddressMode::ZeroPage;
                                    width = 2;
                                }
                                Some(OpVal::Val(TokenVal::Val16(_))) => {
                                    o.mode = AddressMode::Absolute;
                                    width = 3;
                                }
                                None => {
                                    width = 1;
                                }
                            };
                            // Check mode here since it was either implied, ZP, or Absolute and didn't get
                            // checked yet for this opcode.
                            if resolve_opcode(ty, op, &o.mode).is_err() {
                                return Err(eyre!(
                                    "Error parsing line {}: opcode {op} doesn't support mode {}",
                                    line_num + 1,
                                    o.mode
                                ));
                            }
                        }
                        AddressMode::Immediate
                        | AddressMode::ZeroPage
                        | AddressMode::ZeroPageX
                        | AddressMode::ZeroPageY
                        | AddressMode::IndirectX
                        | AddressMode::IndirectY => {
                            let mut ok = false;
                            width = 2;
                            if let Some(v) = &o.op_val {
                                match v {
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
                        | AddressMode::AbsoluteX
                        | AddressMode::AbsoluteY
                        | AddressMode::Indirect => {
                            let mut ok = false;
                            if let Some(v) = &o.op_val {
                                match v {
                                    OpVal::Label(l) => {
                                        if let Some(TokenVal::Val16(_)) =
                                            get_label(&ast_output.labels, l).val
                                        {
                                            ok = true;
                                        }
                                        // location ref which must be 16 bit.
                                        if get_label(&ast_output.labels, l).val.is_none() {
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
                    };

                    if !(1..=3).contains(&width) {
                        return Err(eyre!(
                            "Internal error. Width {width} invalid for {o:#?} on line {}",
                            line_num + 1
                        ));
                    }
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
fn generate_output(ty: Type, ast_output: &mut ASTOutput) -> Result<Assembly> {
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
                Token::Equ(td) => {
                    let val: TokenVal;
                    // Safety: EQU is defined always per above parsing.
                    unsafe { val = get_label(&ast_output.labels, td).val.unwrap_unchecked() }
                    match val {
                        TokenVal::Val8(v) => {
                            write!(output, "{td:<13} EQU      {v:02X}").unwrap();
                        }
                        TokenVal::Val16(v) => {
                            write!(output, "{td:<13} EQU      {v:04X}").unwrap();
                        }
                    };
                }
                Token::Comment(c) => {
                    if index != 0 {
                        write!(output, " ").unwrap();
                    }
                    write!(output, "{c}").unwrap();
                }
                Token::Op(o) => {
                    let modes = match resolve_opcode(ty, &o.op, &o.mode) {
                        Ok(modes) => modes,
                        Err(err) => {
                            return Err(eyre!("Internal error on line {}: {err}", line_num + 1));
                        }
                    };
                    if o.op_val.is_none() && o.mode != AddressMode::Implied {
                        return Err(eyre!("Internal error on line {}: no op val but not implied instruction for opcode {}", line_num+1, o.op));
                    }
                    if o.mode == AddressMode::Implied && o.width != 1 {
                        return Err(eyre!("Internal error on line {}: implied mode for opcode {} but width not 1: {o:#?}", line_num+1, o.op));
                    }

                    // Things are mutable here as we may have to fixup op_val for branches before
                    // emitting bytes below.
                    if o.mode == AddressMode::Relative {
                        let mut ok = false;
                        if let Some(v) = &o.op_val {
                            match v {
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
                                            o.op_val = Some(OpVal::Val(TokenVal::Val8(val)));
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
                    // TODO: If > 1 pick one randomly.
                    res.bin[usize::from(o.pc)] = modes[0];
                    if label_out.is_empty() {
                        label_out = String::from("        ");
                    }
                    if let Some(val) = &o.op_val {
                        let val = match val {
                            OpVal::Label(s) => {
                                if s == "*" {
                                    TokenVal::Val16(o.pc)
                                } else {
                                    // Safety: We just checked labels above has everything referenced and filled in.
                                    // The one exception is * which we just handled.
                                    unsafe {
                                        get_label(&ast_output.labels, s).val.unwrap_unchecked()
                                    }
                                }
                            }
                            OpVal::Val(t) => *t,
                        };
                        match val {
                            TokenVal::Val8(b) => {
                                if o.width != 2 {
                                    return Err(eyre!("Internal error on line {}: got 8 bit value and expect 16 bit for op {} and mode {}", line_num+1, o.op, o.mode));
                                }
                                res.bin[usize::from(o.pc + 1)] = b;
                                write!(
                                    output,
                                    "{:04X} {:02X} {b:02X}    {label_out} ",
                                    o.pc, modes[0]
                                )
                                .unwrap();
                            }
                            TokenVal::Val16(b) => {
                                if o.width != 3 {
                                    return Err(eyre!("Internal error on line {}: got 16 bit value and expect 8 bit for op {} and mode {}", line_num+1, o.op, o.mode));
                                }
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
                        let val = if let Some(val) = &o.op_val {
                            format!("{val}")
                        } else {
                            String::from("")
                        };

                        write!(output, "{op:?}").unwrap();
                        match o.mode {
                            AddressMode::Immediate => {
                                write!(output, " #{val}").unwrap();
                            }
                            AddressMode::Indirect => {
                                write!(output, " ({val})").unwrap();
                            }
                            AddressMode::ZeroPageX => {
                                write!(output, " ({val}),X").unwrap();
                            }
                            AddressMode::ZeroPageY => {
                                write!(output, " ({val},Y)").unwrap();
                            }
                            _ => {
                                if !val.is_empty() {
                                    write!(output, " {val}").unwrap();
                                }
                            }
                        };
                    } else {
                        write!(
                            output,
                            "{:04X} {:02X}       {label_out} {:?}",
                            o.pc, modes[0], o.op
                        )
                        .unwrap();
                    }
                }
            }
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
pub fn parse(ty: Type, lines: Lines<BufReader<File>>) -> Result<Assembly> {
    // Assemblers generally are 2+ passes. One pass to tokenize as much as possible
    // while filling in a label mapping. The 2nd one does actual assembly over
    // the tokens with labels getting filled in from the map or computed as needed.

    // Pass one, read over the file line by line generating a set of tokens per line
    // for our AST. The labels map that comes back is complete so referencing keys
    // we lookup in later passes can use get_label to resolve them.
    let mut ast_output = pass1(ty, lines)?;
    //    let (mut ast, mut labels) = pass1(lines)?;

    // Do another run so we can fill in label references. Also compute addressing mode, validate and set pc.
    compute_refs(ty, &mut ast_output)?;

    // Verify all the labels defined got values (EQU and location definitions/references line up)
    let mut errors = String::new();
    for (l, ld) in &ast_output.labels {
        let locs = ld
            .refs
            .iter()
            .map(|x| format!("{x}"))
            .collect::<Vec<_>>()
            .join(",");
        if ld.line == 0 || ld.val.is_none() {
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

    // Finally generate the output
    generate_output(ty, &mut ast_output)
}

// Given a labels map (label->LabelDef) return the labeldef directly w/o checking.
// This should be only called after AST building as it assumes all labels are valid.
fn get_label<'a>(hm: &'a HashMap<String, LabelDef>, label: &String) -> &'a LabelDef {
    // Safety: Only called after AST built so all labels are in map.
    unsafe { hm.get(label).unwrap_unchecked() }
}

fn get_label_mut<'a>(hm: &'a mut HashMap<String, LabelDef>, label: &String) -> &'a mut LabelDef {
    // Safety: Only called after AST built so all labels are in map.
    unsafe { hm.get_mut(label).unwrap_unchecked() }
}

// parse_val returns a parsed TokenVal8/16 or nothing. Set is_u16 to force returning a Val16 always
// if a value would parse.
fn parse_val(val: &str, is_u16: bool) -> Option<TokenVal> {
    let (trimmed, base) = match val.as_bytes() {
        // Remove any possibly leading 0x or $. If we did this is also base 16
        // Can index back into val without worry about utf8 since we only matched
        // ascii chars here.
        [b'0', b'x', ..] => (&val[2..], 16),
        [b'$', ..] => (&val[1..], 16),
        _ => (val, 10),
    };

    // If nothing was left we're done with no value.
    if trimmed.is_empty() {
        return None;
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
        return Ok(String::from(label));
    }

    if RE.is_match(label) {
        Ok(String::from(label))
    } else {
        Err(eyre!(
            "Error parsing label - {label}. Must be of the form {LABEL}"
        ))
    }
}

const LABEL: &str = "^[a-zA-Z][a-zA-Z0-9+-]+$";

lazy_static! {
    static ref RE: Regex = {
        match Regex::new(LABEL) {
            Ok(re) => re,
            Err(err) => {
                panic!("Error parsing regex {LABEL} - {err}");
            }
        }
    };
}
