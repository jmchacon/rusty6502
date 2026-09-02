//! convertprg takes a C64 style PRG file
//! and converts it into a 64k bin image for
//! running as a test cart.

use clap::Parser;
use clap_num::maybe_hex;
use color_eyre::eyre::{eyre, Result};
use std::{ffi::OsStr, fs::read, fs::write, path::Path};

/// convertprg takes a C64 style PRG file
/// and converts it into a 64k bin image for
/// running as a test cart.
///
/// This assumes exection will start at 0xD000
/// which will then JSR to the start PC given.
///
/// BRK/IRQ/NMI vectors will all point at 0xC000
/// which simply infinite loops.
///
/// Certain parts of RAM in zero page will be initialized
/// with c64 values (such as the vectors used for finding
/// start of basic, etc).
///
/// The output file is named after the input with .bin
/// appended onto the end replacing .prg.
#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    filename: String,

    #[arg(
        long,
        default_value_t = 0, value_parser=maybe_hex::<u16>,
        help = "The PC value to start running via a JSR from 0xD000"
    )]
    start_pc: u16,
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let args: Args = Args::parse();

    let start_pc = args.start_pc;

    let filename = args.filename;
    let p = Path::new(filename.as_str());
    let Some(ext) = p.extension().and_then(OsStr::to_str) else {
        eprintln!("{filename} has no extension");
        std::process::exit(1);
    };

    if ext != "prg" {
        eprintln!("filename {filename} not a prg file?");
        std::process::exit(1);
    }
    let output = p.with_extension("bin");

    let bytes = read(filename)?;

    let block = convert_prg(&bytes, start_pc)?;
    write(output.as_path(), block.as_ref())?;
    Ok(())
}

/// `convert_prg` converts raw PRG file bytes (a 2 byte load address header
/// followed by the program data) into a 64k memory image ready to run as a
/// test cart, with the C64 startup scaffolding and known zero page/RAM
/// values pre-populated.
///
/// # Errors
/// Returns an error if `bytes` is too short to contain the 2 byte load
/// address header.
fn convert_prg(bytes: &[u8], start_pc: u16) -> Result<Box<[u8; 1 << 16]>> {
    if bytes.len() < 2 {
        return Err(eyre!(
            "PRG data too short ({} bytes) - must have at least a 2 byte load address header",
            bytes.len()
        ));
    }

    // The load addr is the first 2 bytes followed by the data.
    let addr = (usize::from(bytes[1]) << 8) + usize::from(bytes[0]);
    println!("Addr is {addr:#06X} start_pc is {start_pc:#06X}");

    let mut data = &bytes[2..];

    let max = (1 << 16) - addr;
    if data.len() > max {
        println!(
            "Length {} at offset {addr} too long, truncating to 64k",
            data.len()
        );
        data = &data[..max];
    }

    // We know this will just be a 64k block so do that.
    let mut block: Box<[u8; 1 << 16]> = Box::new([0; 1 << 16]);

    // Copy everything over starting at the load address parsed above.
    for (a, b) in (addr..).zip(data.iter()) {
        block[a] = *b;
    }

    // Now setup our startup function
    block[0xD000] = 0xD8; // CLD
    block[0xD001] = 0x20; // JSR <addr>
    block[0xD002] = (start_pc & 0xFF) as u8;
    block[0xD003] = ((start_pc >> 8) & 0xFF) as u8;
    block[0xD004] = 0x4C; // JMP D004
    block[0xD005] = 0x04;
    block[0xD006] = 0xD0;

    // Set infinite loop for all 3 vectors.
    block[0xC000] = 0x4C; // JMP 0xC000
    block[0xC001] = 0x00;
    block[0xC002] = 0xC0;

    // I do not recall why this is here.
    block[0xFFD2] = 0x60; // RTS

    // All 3 vectors point at 0xC000 which loops.
    block[0xFFFA] = 0x00;
    block[0xFFFB] = 0xC0;
    block[0xFFFC] = 0x00;
    block[0xFFFD] = 0xC0;
    block[0xFFFE] = 0x00;
    block[0xFFFF] = 0xC0;

    write_c64_values(block.as_mut_slice());
    Ok(block)
}

fn write_c64_values(block: &mut [u8]) {
    // Based from data in http://sta.c64.org/cbm64mem.html.

    // Setup zero page.
    block[0x0000] = 0x2F;
    block[0x0000] = 0x37;
    block[0x0003] = 0xAA;
    block[0x0004] = 0xB1;
    block[0x0005] = 0x91;
    block[0x0006] = 0xB3;
    block[0x0016] = 0x19;
    block[0x002B] = 0x01; // Pointer to start of BASIC area
    block[0x002C] = 0x08;
    block[0x0038] = 0xA0; // Pointer to end of BASIC area
    block[0x0053] = 0x03;
    block[0x0054] = 0x4C;
    block[0x0091] = 0xFF;
    block[0x009A] = 0x03;
    block[0x00B2] = 0x3C;
    block[0x00B3] = 0x03;
    block[0x00C8] = 0x27;
    block[0x00D5] = 0x27;

    // Some other random locations in RAM that have presets
    // which may be used in test programs assuming c64.
    block[0x0282] = 0x08;
    block[0x0284] = 0xA0;
    block[0x0288] = 0x04;
    block[0x0300] = 0x8B;
    block[0x0301] = 0xE3;
    block[0x0302] = 0x83;
    block[0x0303] = 0xA4;
    block[0x0304] = 0x7C;
    block[0x0305] = 0xA5;
    block[0x0306] = 0x1A;
    block[0x0307] = 0xA7;
    block[0x0308] = 0xE4;
    block[0x0309] = 0xA7;
    block[0x030A] = 0x86;
    block[0x030B] = 0xAE;
    block[0x0310] = 0x4C;
    block[0x0314] = 0x31;
    block[0x0315] = 0xEA;
    block[0x0316] = 0x66;
    block[0x0317] = 0xFE;
    block[0x0318] = 0x47;
    block[0x0319] = 0xFE;
    block[0x031A] = 0x4A;
    block[0x031B] = 0xF3;
    block[0x031C] = 0x91;
    block[0x031D] = 0xF2;
    block[0x031E] = 0x0E;
    block[0x031F] = 0xF2;
    block[0x0320] = 0x50;
    block[0x0321] = 0xF2;
    block[0x0322] = 0x33;
    block[0x0323] = 0xF3;
    block[0x0324] = 0x57;
    block[0x0325] = 0xF1;
    block[0x0326] = 0xCA;
    block[0x0327] = 0xF1;
    block[0x0328] = 0xED;
    block[0x0329] = 0xF6;
    block[0x032A] = 0x3E;
    block[0x032B] = 0xF1;
    block[0x032C] = 0x2F;
    block[0x032D] = 0xF3;
    block[0x032E] = 0x66;
    block[0x032F] = 0xFE;
    block[0x0330] = 0xA5;
    block[0x0331] = 0xF4;
    block[0x0332] = 0xED;
    block[0x0333] = 0xF5;
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Args::command().debug_assert();
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::convert_prg;

    #[test]
    fn convert_prg_writes_data_at_the_parsed_load_address() {
        // Load address 0x1234 (little endian header) followed by 4 data bytes.
        let bytes = [0x34, 0x12, 0xAA, 0xBB, 0xCC, 0xDD];
        let block = convert_prg(&bytes, 0x0000).unwrap();
        assert_eq!(block[0x1234], 0xAA);
        assert_eq!(block[0x1235], 0xBB);
        assert_eq!(block[0x1236], 0xCC);
        assert_eq!(block[0x1237], 0xDD);
    }

    #[test]
    fn convert_prg_rejects_input_shorter_than_the_header() {
        assert!(convert_prg(&[], 0x0000).is_err());
        assert!(convert_prg(&[0x01], 0x0000).is_err());
    }

    #[test]
    fn convert_prg_handles_a_header_only_prg_with_no_data() {
        // Just the 2 byte load address header, no program bytes - must not panic.
        let block = convert_prg(&[0x00, 0x08], 0x0000).unwrap();
        assert_eq!(block[0x0800], 0x00);
    }

    #[test]
    fn convert_prg_truncates_data_that_would_overflow_64k() {
        // Load address 0xFFF0, with more data than the remaining 16 bytes of
        // address space can hold - without truncation this would try to index
        // past the end of the 64k block and panic.
        let mut bytes = vec![0xF0, 0xFF]; // addr = 0xFFF0
        bytes.extend(std::iter::repeat_n(0xAA, 64));
        let block = convert_prg(&bytes, 0x0000).unwrap();
        assert_eq!(block[0xFFF0], 0xAA);
    }
}
