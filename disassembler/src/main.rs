use c64basic::{list, BASIC_LOAD_ADDR};
use disassemble::*;
use rusty6502::prelude::*;
use std::fs::read;

gflags::define! {
    /// The PC value to start disassembly.
    --start_pc: u16 = 0
}
gflags::define! {
    /// Offset into RAM to start loading data. All other RAM will be zero'd out. Ignored for PRG files.
    --offset: u16 = 0
}

// Disassembler will take a memory image file (.bin generally) or a .prg file (c64 basic program)
// and disassemble it. If it's a c64 basic program the basic code will be interpreted and anything
// remaining after will be disassembled as assembly.
fn main() {
    let args = gflags::parse();
    let mut ram = FlatRAM::new();

    if args.len() != 1 {
        eprintln!("Must supply a single filename to load");
        std::process::exit(1);
    }

    let filename = args[0];

    // Check if this is a c64 binary.
    let mut c64 = false;
    let f = filename.to_string();
    let v = f.split('.').collect::<Vec<&str>>();
    let mut last = v[v.len() - 1].to_string();
    last.make_ascii_lowercase();
    if last == "prg" {
        c64 = true;
        println!("C64 program file");
    }

    let mut bytes = read(filename).unwrap_or_else(|_| panic!("can't read file: {filename}"));

    let mut start = START_PC.flag;
    let mut addr = OFFSET.flag;
    let mut pc = 0u16;

    if c64 {
        // The load addr is actually the first 2 bytes and then data goes there.
        // This overrides --offset.
        addr = ((bytes[1] as u16) << 8) + bytes[0] as u16;

        // It's also the start PC
        start = addr;

        // Trim these bytes off. Yes this isn't efficient but it's 2 bytes also.
        bytes.remove(0);
        bytes.remove(0);
    }

    let max = (u16::MAX as usize) + 1 - (addr as usize);
    if bytes.len() > max {
        println!(
            "Length {} at offset {addr} too long, truncating to 64k",
            bytes.len()
        );
        bytes.truncate(max as usize);
    }
    for b in bytes.iter() {
        ram.write(addr, *b);
        // Don't add in this case as we'll wrap and panic.
        // Could make addr a Wrapping but not needed otherwise.
        if addr != u16::MAX {
            addr += 1;
        }
    }
    pc += start;

    println!("0x{:04X} bytes at pc: {pc:04X}\n", bytes.len());

    if c64 && start == BASIC_LOAD_ADDR {
        // Start with basic first
        loop {
            let res = list(pc, &ram).unwrap_or_else(|_| panic!("error from list"));
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
    let mut dis;
    let mut newpc: u16;
    loop {
        (dis, newpc) = step(pc, &ram);
        println!("{dis}");
        // Check if we went off the end, or the newpc wrapped
        // as step() internally uses Wrapping and will overflow.
        if newpc > (start + (bytes.len() - 1) as u16) || newpc < pc {
            break;
        }
        pc = newpc;
    }
}
