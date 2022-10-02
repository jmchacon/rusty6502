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
    fmt,
    fs::{write, File},
    io::{self, BufRead},
    path::Path,
};

use clap::Parser;
use color_eyre::eyre::{eyre, Result};

/// handasm will take a file which looks like disassembler output
/// and use that to construct a 64k binary image file.
#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    filename: String,
    output: String,
}

#[derive(Debug, Clone)]
/// `ParseError` indicates a syntax error parsing the input.
pub struct ParseError {
    line: String,
    line_num: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Can't parse line {} - {}", self.line_num, self.line)
    }
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let args: Args = Args::parse();

    let filename = args.filename;
    let output = Path::new(&args.output);

    // Just read everything into RAM to process.
    let lines = read_lines(filename)?;

    // Always emit 64k so just allocate a block.
    let mut block: [u8; 1 << 16] = [0; 1 << 16];

    // Consumes the iterator, return Strings
    for (line_num, line) in lines.flatten().enumerate() {
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

        // There's always an address (16 bit but we parse as usize so it can index into block) and at least one opcode
        let mut op1 = None;
        let mut op2 = None;

        // If the first field matches as an addr this must be something we can use.
        let addr = match usize::from_str_radix(fields[0], 16) {
            Ok(addr) => addr,
            Err(_) => continue,
        };

        // If we have an addr opcode is required or this is a bad line and we should stop.
        let op = match u8::from_str_radix(fields[1], 16) {
            Ok(op) => op,
            Err(_) => {
                return Err(eyre!("Error parsing line {}: {}", line_num + 1, line));
            }
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
        block[addr] = op;
        if let Some(op1) = op1 {
            block[addr + 1] = op1;
        }
        if let Some(op2) = op2 {
            block[addr + 2] = op2;
        }
    }

    write(output, block)?;
    Ok(())
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
    Args::command().debug_assert()
}
