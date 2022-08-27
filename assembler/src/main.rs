//! Assembler will take the given input file and generate a .bin file
//! after assembling the instructions presented in it.

use assemble::parse;
use clap::Parser;
use color_eyre::eyre::Result;
use std::{
    fs::{write, File},
    io::{self, BufRead},
    path::Path,
};

/// Assembler will take the given input file and generate a .bin file
/// after assembling the instructions presented in it.
#[derive(Parser)]
#[clap(author, version)]
struct Args {
    #[clap()]
    filename: String,
    output: String,
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let args: Args = Args::parse();

    let lines = read_lines(args.filename)?;

    match parse(lines) {
        Err(e) => Err(e),
        Ok(block) => {
            write(Path::new(args.output.as_str()), block)?;
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
