//! assembler will take the given input file and generate a .bin file
//! after assembling the instructions presented in it.

use assemble::parse;
use clap::Parser;
use color_eyre::eyre::Result;
use rusty6502::prelude::*;
use std::{
    fs::{write, File},
    io::{self, BufRead},
    path::Path,
};

/// assembler will take the given input file and generate a .bin file
/// after assembling the instructions presented in it.
#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    cpu_type: Type,
    filename: String,
    output: String,
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let args: Args = Args::parse();

    let lines = read_lines(args.filename)?;

    match parse(args.cpu_type, lines) {
        Err(e) => Err(e),
        Ok(res) => {
            write(Path::new(args.output.as_str()), res.bin)?;
            print!("{}", res.listing);
            Ok(())
        }
    }
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
