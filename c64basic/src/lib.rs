use rusty6502::prelude::*;
use std::fmt::Write;
use std::str;

mod tests;

fn read_addr(r: &impl Memory, pc: u16) -> u16 {
    let low = r.read(pc);
    let high = r.read(pc + 1);
    ((high as u16) << 8) + low as u16
}

/// list will take the given PC value and disassembles the Basic line at that location
/// returning a string for the line and the PC of the next line. This does no sanity
/// checking so a basic program which points to itself for listing will infinite loop
/// if the PC values passed in aren't compared for loops.
/// On a normal program end (next addr == 0x0000) it will return an empty string and PC of 0x0000.
/// If there is a token parsing problem an error is returned instead with as much of the
/// line as would tokenize. Normally a c64 won't continue so the newPC value here will be 0.
/// NOTE: This returns the ASCII characters as parsed, displaying in PETSCII is up to the caller
///      to determine.
pub fn list(pc: u16, r: &impl Memory) -> Result<(String, u16), (String, String)> {
    let new_pc = read_addr(r, pc);
    let mut working_pc = pc + 2;

    // Return an empty string and PC = 0x0000 for end of program.
    if new_pc == 0x0000 {
        return Ok((String::from(""), 0x0000));
    }

    // Next 2 are line number also stored in little endian so we can just use readAddr again.
    let line_num = read_addr(r, working_pc);
    working_pc += 2;

    let mut output_string = String::new();

    // Emit the line number
    write!(output_string, "{line_num} ").unwrap();

    // a 1 byte array to stick tok into when we need to stringify it below.
    let mut b: [u8; 1] = [0; 1];

    // Read until we reach a NUL indicating EOL.
    loop {
        let tok = r.read(working_pc);
        working_pc += 1;
        if tok == 0x00 {
            break;
        }
        if tok > 0xCB {
            return Err((String::from("?SYNTAX ERROR"), output_string));
        }
        let emit = match tok {
            0x80 => "END",
            0x81 => "FOR",
            0x82 => "NEXT",
            0x83 => "DATA",
            0x84 => "INPUT#",
            0x85 => "INPUT",
            0x86 => "DIM",
            0x87 => "READ",
            0x88 => "LET",
            0x89 => "GOTO",
            0x8A => "RUN",
            0x8B => "IF",
            0x8C => "RESTORE",
            0x8D => "GOSUB",
            0x8E => "RETURN",
            0x8F => "REM",
            0x90 => "STOP",
            0x91 => "ON",
            0x92 => "WAIT",
            0x93 => "LOAD",
            0x94 => "SAVE",
            0x95 => "VERIFY",
            0x96 => "DEF",
            0x97 => "POKE",
            0x98 => "PRINT#",
            0x99 => "PRINT",
            0x9A => "CONT",
            0x9B => "LIST",
            0x9C => "CLR",
            0x9D => "CMD",
            0x9E => "SYS",
            0x9F => "OPEN",
            0xA0 => "CLOSE",
            0xA1 => "GET",
            0xA2 => "NEW",
            0xA3 => "TAB(",
            0xA4 => "TO",
            0xA5 => "FN",
            0xA6 => "SPC(",
            0xA7 => "THEN",
            0xA8 => "NOT",
            0xA9 => "STEP",
            0xAA => "+",
            0xAB => "-",
            0xAC => "*",
            0xAD => "/",
            0xAE => "^",
            0xAF => "AND",
            0xB0 => "OR",
            0xB1 => ">",
            0xB2 => "=",
            0xB3 => "<",
            0xB4 => "SGN",
            0xB5 => "INT",
            0xB6 => "ABS",
            0xB7 => "USR",
            0xB8 => "FRE",
            0xB9 => "POS",
            0xBA => "SQR",
            0xBB => "RND",
            0xBC => "LOG",
            0xBD => "EXP",
            0xBE => "COS",
            0xBF => "SIN",
            0xC0 => "TAN",
            0xC1 => "ATN",
            0xC2 => "PEEK",
            0xC3 => "LEN",
            0xC4 => "STR$",
            0xC5 => "VAL",
            0xC6 => "ASC",
            0xC7 => "CHR$",
            0xC8 => "LEFT$",
            0xC9 => "RIGHT$",
            0xCA => "MID$",
            0xCB => "GO",
            _ => {
                b[0] = tok;
                str::from_utf8(&b).expect("invalid utf. Must be below 0x80 which is impossible?")
            }
        };
        write!(output_string, "{emit}").unwrap();
    }
    Ok((output_string, working_pc))
}
