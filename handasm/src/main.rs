//! `handasm` will take a file which looks like disassembler output
//! and use that to construct a binary image.
//!
//! Example:
//!
//! D000 4C 20 21 JMP 2120
//!
//! would produce a 64k binary image with the bytes at 0xD000 set to 4C,20 and 21
//! respectively with the remainder all zeros.

use std::{
    fs::{write, File},
    io::{self, BufRead},
    num::Wrapping,
    path::Path,
};

use clap::Parser;
use color_eyre::eyre::{eyre, Result};

#[cfg(test)]
mod tests;

/// handasm will take a file which looks like disassembler output
/// and use that to construct a 64k binary image file.
#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    filename: String,
    output: String,
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let args: Args = Args::parse();

    let filename = args.filename;
    let output = Path::new(&args.output);

    // Just read everything into RAM to process.
    let lines = read_lines(filename)?;

    let block = process_lines(lines.map_while(Result::ok))?;

    write(output, block)?;
    Ok(())
}

/// `process_lines` parses disassembler-style listing lines into a 64k binary
/// image. Addresses are real 6502 16 bit addresses, so an opcode with
/// operand bytes starting near the top of memory (e.g. `FFFE`/`FFFF`) wraps
/// around to the start of the image rather than panicking, matching how the
/// 6502 address space itself wraps.
///
/// # Errors
/// Returns an error if a line has a valid address field but an unparseable
/// opcode field.
fn process_lines(lines: impl Iterator<Item = String>) -> Result<[u8; 1 << 16]> {
    // Always emit 64k so just allocate a block.
    let mut block: [u8; 1 << 16] = [0; 1 << 16];

    for (line_num, line) in lines.enumerate() {
        let fields: Vec<&str> = line.split_whitespace().collect();

        // If there aren't 2 fields don't even try.
        if fields.len() < 2 {
            continue;
        }
        // If the 2nd field is equ it's a label def
        if fields[1].to_lowercase() == "equ" {
            continue;
        }

        // If the first field isn't 4 chars it's not an addr.
        if fields[0].len() != 4 {
            continue;
        }
        // 2nd field has to be 2 chars.
        if fields[1].len() != 2 {
            continue;
        }

        // There's always an address (16 bit) and at least one opcode
        let mut op1 = None;
        let mut op2 = None;

        // If the first field matches as an addr this must be something we can use.
        let Ok(addr) = u16::from_str_radix(fields[0], 16) else {
            continue;
        };

        // If we have an addr opcode is required or this is a bad line and we should stop.
        let Ok(op) = u8::from_str_radix(fields[1], 16) else {
            return Err(eyre!("Error parsing line {}: {}", line_num + 1, line));
        };

        // The next 2 are optional
        if fields.len() > 2 && fields[2].len() == 2 {
            op1 = u8::from_str_radix(fields[2], 16).ok();
        }
        if fields.len() > 3 && fields[3].len() == 2 {
            op2 = u8::from_str_radix(fields[3], 16).ok();
        }

        // We know op is valid and maybe the other 2 bytes so
        // write what we know and deconstruct to see about the others.
        // Address arithmetic wraps at 0xFFFF (real 6502 address space) rather
        // than panicking, since an addr near the top of memory (e.g. FFFE)
        // with a 2-3 byte opcode is otherwise a valid, if unusual, listing.
        let addr = Wrapping(addr);
        block[usize::from(addr.0)] = op;
        if let Some(op1) = op1 {
            block[usize::from((addr + Wrapping(1)).0)] = op1;
        }
        if let Some(op2) = op2 {
            block[usize::from((addr + Wrapping(2)).0)] = op2;
        }
    }

    Ok(block)
}

// `read_lines` returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Args::command().debug_assert();
}
