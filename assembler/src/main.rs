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
use strum_macros::{Display, EnumString};

/// assembler will take the given input file and generate a .bin file
/// after assembling the instructions presented in it.
#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    cpu_type: Type,
    filename: String,
    output: String,
}

// Type defines the various implementations of the 6502 available.
#[derive(Copy, Clone, Debug, Display, PartialEq, Eq, EnumString)]
#[allow(clippy::upper_case_acronyms)]
enum Type {
    /// Basic NMOS 6502 including all undocumented opcodes.
    NMOS,

    /// Ricoh version used in the NES which is identical to NMOS except BCD mode is unimplemented.
    #[strum(to_string = "NMOS_RICOH")]
    RICOH,

    /// NMOS 6501 variant (used in c64) which includes I/O ports mapped at addresses 0x00 and 0x01.
    #[strum(to_string = "NMOS_6510")]
    NMOS6510,

    /// 65C02 CMOS version where undocumented opcodes are all explicit NOP's and defined.
    /// This is an implementation of the later WDC spec so will include support
    /// for WAI, STP, SMB/RMB and BBR/BBS instructions.
    CMOS,
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
    let cpu: &dyn CPU = match args.cpu_type {
        Type::NMOS => &nmos,
        Type::RICOH => &ricoh,
        Type::NMOS6510 => &c6510,
        Type::CMOS => &cmos,
    };

    match parse(cpu, lines, false) {
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
    Args::command().debug_assert();
}
