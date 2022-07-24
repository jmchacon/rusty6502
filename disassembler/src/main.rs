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

    let start = START_PC.flag;
    let mut addr = OFFSET.flag;
    let filename = args[0];
    let mut pc = 0u16;
    println!("start: {start} offset: {addr} args: {filename}",);

    let mut bytes = read(filename).unwrap_or_else(|_| panic!("can't read file: {filename}"));
    let max = u16::MAX - addr;

    if bytes.len() > max.into() {
        bytes.truncate(max as usize);
    }
    for b in bytes.iter() {
        ram.write(addr, *b);
        addr += 1;
    }

    pc += start;
    let mut dis;
    loop {
        (dis, pc) = step(pc, &ram);
        println!("{dis}");
        if pc >= (OFFSET.flag + bytes.len() as u16) {
            break;
        }
    }
}
