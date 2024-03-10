//! assembler will take the given input file and generate a .bin file
//! after assembling the instructions presented in it.

use assemble::parse;
use clap::Parser;
use clap_num::maybe_hex;
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
    cpu_type: CPUType,
    filename: String,
    output: String,

    #[arg(
        long,
        default_value_t = 0x0000, value_parser=maybe_hex::<usize>,
        help = "The memory location to start emitting"
    )]
    start_loc: usize,

    #[arg(
        long,
        default_value_t = 0x10000, value_parser=maybe_hex::<usize>,
        help = "Number of bytes to emit"
    )]
    bytes: usize,

    #[arg(
        long,
        default_value_t = false,
        help = "Set to true to emit AST debugging"
    )]
    debug: bool,
}

#[allow(clippy::similar_names)]
fn main() -> Result<()> {
    color_eyre::install()?;
    let args: Args = Args::parse();

    let lines = read_lines(args.filename)?;

    let nmos = CPU6502::new(ChipDef::default());
    let ricoh = CPURicoh::new(ChipDef::default());
    let c6510 = CPU6510::new(ChipDef::default(), None);
    let cmos = CPU65C02::new(ChipDef::default());
    let rockwell = CPU65C02Rockwell::new(ChipDef::default());
    let c65sc02 = CPU65SC02::new(ChipDef::default());

    let cpu: &dyn CPU = match args.cpu_type {
        CPUType::NMOS => &nmos,
        CPUType::RICOH => &ricoh,
        CPUType::NMOS6510 => &c6510,
        CPUType::CMOS => &cmos,
        CPUType::CMOSRockwell => &rockwell,
        CPUType::CMOS65SC02 => &c65sc02,
    };

    let end = args.start_loc + args.bytes;
    assert!(
        end <= 0x10000,
        "Starting at {} + {} bytes is > 64k",
        args.start_loc,
        args.bytes
    );
    match parse(cpu, lines, args.debug) {
        Err(e) => Err(e),
        Ok(res) => {
            write(
                Path::new(args.output.as_str()),
                &res.bin[args.start_loc..end],
            )?;
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
    Args::command().debug_assert();
}
