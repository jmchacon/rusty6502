//! disassembler will take a memory image file (.bin generally) or a .prg file (c64 basic program)
//! and disassemble it.

use c64basic::{list, BASIC_LOAD_ADDR};
use clap::Parser;
use clap_num::maybe_hex;
use color_eyre::eyre::{eyre, Result};
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

    let bytes = read(filename)?;

    let loaded = load_bytes(&mut ram, bytes, c64, args.offset, args.start_pc)?;
    let mut pc = loaded.pc;

    println!("{:#06X} bytes at pc: {pc:#06X}\n", loaded.data_len);

    let start = Wrapping(loaded.start);
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
    println!("start: {start:04X} len {:04X}", loaded.data_len);

    // If there was no data loaded there's nothing to disassemble.
    let Some(limit) = loaded.limit else {
        println!("No data to disassemble");
        return Ok(());
    };
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
        let newpc = cpu.disassemble(&mut dis, pc, &ram, false);
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

/// Result of loading raw file bytes into RAM ready for disassembly.
struct Loaded {
    /// The PC to start disassembly from.
    pc: u16,
    /// The (possibly PRG-header-derived) start address, used to detect the
    /// C64 BASIC load address.
    start: u16,
    /// How many bytes of program data were actually loaded (after any PRG
    /// header was stripped and/or truncation to fit in 64k).
    data_len: usize,
    /// The last PC we should disassemble up to and including, or `None` if
    /// no data was loaded at all (so there's nothing to disassemble).
    limit: Option<u16>,
}

/// `load_bytes` writes `bytes` into `ram` starting at `offset` (or, for a c64
/// PRG file, at the address encoded in its first 2 bytes) and computes the
/// PC range to disassemble.
///
/// # Errors
/// Returns an error if `c64` is set but `bytes` is too short to contain the
/// 2 byte PRG load address header.
fn load_bytes(
    ram: &mut FlatRAM,
    mut bytes: Vec<u8>,
    c64: bool,
    offset: u16,
    start_pc: u16,
) -> Result<Loaded> {
    let mut start = Wrapping::<u16>(start_pc);
    let mut addr = Wrapping::<u16>(offset);

    if c64 {
        if bytes.len() < 2 {
            return Err(eyre!(
                "PRG file too short ({} bytes) - must have at least a 2 byte load address header",
                bytes.len()
            ));
        }

        // The load addr is actually the first 2 bytes and then data goes there.
        // This overrides --offset.
        addr = Wrapping::<u16>((u16::from(bytes[1]) << 8) + u16::from(bytes[0]));

        // It's also the start PC
        start = addr;

        // Trim these bytes off. Yes this isn't efficient but it's 2 bytes also.
        bytes.remove(0);
        bytes.remove(0);
    }

    let max = (1 << 16) - usize::from(addr.0);
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
    let pc = (Wrapping(0u16) + start).0;

    let limit = if bytes.is_empty() {
        None
    } else {
        #[allow(clippy::cast_possible_truncation)]
        Some(Wrapping((usize::from(start.0) + bytes.len() - 1) as u16).0)
    };

    Ok(Loaded {
        pc,
        start: start.0,
        data_len: bytes.len(),
        limit,
    })
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Args::command().debug_assert();
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::load_bytes;
    use rusty6502::prelude::*;

    #[test]
    fn load_bytes_rejects_a_prg_shorter_than_the_header() {
        let mut ram = FlatRAM::new();
        assert!(load_bytes(&mut ram, vec![], true, 0, 0).is_err());
        assert!(load_bytes(&mut ram, vec![0x01], true, 0, 0).is_err());
    }

    #[test]
    fn load_bytes_handles_an_empty_bin_file_without_panicking() {
        let mut ram = FlatRAM::new();
        let loaded = load_bytes(&mut ram, vec![], false, 0x1000, 0x1000).unwrap();
        assert_eq!(loaded.data_len, 0);
        assert!(loaded.limit.is_none());
    }

    #[test]
    fn load_bytes_handles_a_prg_with_only_the_header_and_no_data() {
        let mut ram = FlatRAM::new();
        let loaded = load_bytes(&mut ram, vec![0x00, 0x08], true, 0, 0).unwrap();
        assert_eq!(loaded.data_len, 0);
        assert!(loaded.limit.is_none());
        assert_eq!(loaded.start, 0x0800);
    }

    #[test]
    fn load_bytes_loads_a_normal_bin_file() {
        let mut ram = FlatRAM::new();
        let loaded = load_bytes(&mut ram, vec![0xA9, 0x42, 0x60], false, 0x1000, 0x1000).unwrap();
        assert_eq!(loaded.pc, 0x1000);
        assert_eq!(loaded.data_len, 3);
        assert_eq!(loaded.limit, Some(0x1002));
        assert_eq!(ram.read(0x1000), 0xA9);
        assert_eq!(ram.read(0x1001), 0x42);
        assert_eq!(ram.read(0x1002), 0x60);
    }

    #[test]
    fn load_bytes_parses_the_prg_load_address_header() {
        let mut ram = FlatRAM::new();
        let loaded = load_bytes(&mut ram, vec![0x00, 0xC0, 0xEA, 0xEA], true, 0, 0).unwrap();
        assert_eq!(loaded.start, 0xC000);
        assert_eq!(loaded.pc, 0xC000);
        assert_eq!(loaded.data_len, 2);
        assert_eq!(ram.read(0xC000), 0xEA);
        assert_eq!(ram.read(0xC001), 0xEA);
    }
}
