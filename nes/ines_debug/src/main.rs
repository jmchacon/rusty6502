//! `ines_debug` takes the path to the given ines file, parses it and then
//! prints the debug output of it's contents.
use std::fs::read;

use clap::Parser;

use color_eyre::eyre::Result;

/// `ines_debug` will load the given NES file and print the data from it.
#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    #[arg(help = "Filenames containing .nes data", long)]
    filename: String,
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let args: Args = Args::parse();

    let bytes: Vec<u8> = read(&args.filename)?;

    let ines = ines::parse(&bytes)?;

    println!("NES data for {}\n{ines}", args.filename);
    Ok(())
}
