//! disassembler will take a memory image file (.bin generally) or a .prg file (c64 basic program)
//! and disassemble it.

use c64basic::{list, BASIC_LOAD_ADDR};
use clap::Parser;
use clap_num::maybe_hex;
use color_eyre::eyre::Result;
use rusty6502::prelude::*;
use std::{ffi::OsStr, fs::read, num::Wrapping, path::Path};

/// disassembler will take a memory image file (.bin generally) or a .prg file (c64 basic program)
/// and disassemble it.
///
/// If it's a c64 basic program the basic code will be interpreted and anything
/// remaining after will be disassembled as assembly.
#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    cpu_type: CPUType,

    filename: String,

    #[arg(
        long,
        default_value_t = 0,value_parser=maybe_hex::<u16>,
        help = "Offset into RAM to start loading data. All other RAM will be zero'd out. Ignored for PRG files."
    )]
    offset: u16,

    #[arg(long, default_value_t = 0, value_parser=maybe_hex::<u16>, help = "The PC value to start disassembly.")]
    start_pc: u16,
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let args: Args = Args::parse();

    let mut ram = FlatRAM::new();

    // Check if this is a c64 binary.
    let mut c64 = false;
    let filename = args.filename;
    let ext = if let Some(ext) = Path::new(filename.as_str())
        .extension()
        .and_then(OsStr::to_str)
    {
        ext
    } else {
        eprintln!("{filename} has no extension can't verify c64 program");
        ""
    };

    if ext == "prg" {
        c64 = true;
        println!("C64 program file");
    }

    let mut bytes = read(filename)?;

    let mut start = Wrapping::<u16>(args.start_pc);
    let mut addr = Wrapping::<u16>(args.offset);
    let mut pc = 0;

    if c64 {
        // The load addr is actually the first 2 bytes and then data goes there.
        // This overrides --offset.
        addr = Wrapping::<u16>((u16::from(bytes[1]) << 8) + u16::from(bytes[0]));

        // It's also the start PC
        start = addr;

        // Trim these bytes off. Yes this isn't efficient but it's 2 bytes also.
        bytes.remove(0);
        bytes.remove(0);
    }

    let max = (1 << 16) - (addr.0 as usize);
    if bytes.len() > max {
        println!(
            "Length {} at offset {addr} too long, truncating to 64k",
            bytes.len()
        );
        bytes.truncate(max);
    }
    for b in &bytes {
        ram.write(addr.0, *b);
        // Don't add in this case as we'll wrap and panic.
        // Could make addr a Wrapping but not needed otherwise.
        if addr.0 != u16::MAX {
            addr += 1;
        }
    }
    pc = (Wrapping(pc) + start).0;

    println!("{:#06X} bytes at pc: {pc:#06X}\n", bytes.len());

    if c64 && start == Wrapping::<u16>(BASIC_LOAD_ADDR) {
        // Start with basic first
        loop {
            let res = match list(pc, &ram) {
                Ok((out, pc)) => (out, pc),
                Err(err) => {
                    eprintln!("{err}");
                    std::process::exit(1);
                }
            };
            if res.1 == 0x0000 {
                // Account for 3 NULs indicating end of program
                pc += 2;
                println!("PC: {pc:04X}");
                break;
            }
            println!("{pc:04X} {}\n", res.0);
            pc = res.1;
        }
    }
    let mut newpc: u16;
    println!("start: {start:04X} len {:04X}", bytes.len());
    // Set the most we'll do. If start was moved this will limit further.
    #[allow(clippy::cast_possible_truncation)]
    let limit = Wrapping((usize::from(start.0) + bytes.len() - 1) as u16).0;
    println!("limit {limit:04X}");

    let c6502_cpu = CPU6502::new(ChipDef::default());
    let ricoh_cpu = CPURicoh::new(ChipDef::default());
    let c6510_cpu = CPU6510::new(ChipDef::default(), None);
    let cmos_cpu = CPU65C02::new(ChipDef::default());
    let rockwell_cpu = CPU65C02Rockwell::new(ChipDef::default());
    let c65sc02_cpu = CPU65SC02::new(ChipDef::default());

    let cpu: &dyn CPU = match args.cpu_type {
        CPUType::NMOS => &c6502_cpu,
        CPUType::RICOH => &ricoh_cpu,
        CPUType::NMOS6510 => &c6510_cpu,
        CPUType::CMOS => &cmos_cpu,
        CPUType::CMOSRockwell => &rockwell_cpu,
        CPUType::CMOS65SC02 => &c65sc02_cpu,
    };
    let mut dis = String::with_capacity(32);
    loop {
        newpc = cpu.disassemble(&mut dis, pc, &ram, false);
        println!("{dis}");
        // Check if we went off the end, or the newpc wrapped
        // as step() can overflow.
        if newpc > limit || newpc < pc {
            break;
        }
        pc = newpc;
    }
    Ok(())
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Args::command().debug_assert();
}
