#![feature(destructuring_assignment)]

use disassemble::*;
use rusty6502::prelude::*;
use std::fs::read;

gflags::define! {
    --start_pc: u16 = 0
}
gflags::define! {
    --offset: u16 = 0
}

fn main() {
    let args = gflags::parse();
    let mut ram = FlatRAM::new();

    if args.len() != 1 {
        eprintln!("Must supply a single filename to load");
        std::process::exit(1);
    }

    println!(
        "start: {} offset: {} args: {} addr: {:04X} val: {:02X}",
        START_PC.flag,
        OFFSET.flag,
        args[0],
        0 as u16,
        ram.read(0)
    );

    let mut bytes = read(args[0]).expect(format!("can't read file: {}", args[0]).as_str());
    let mut addr = OFFSET.flag;
    let max = u16::MAX - addr;

    if bytes.len() > max.into() {
        bytes.truncate(max as usize);
    }
    for b in bytes.iter() {
        ram.write(addr, *b);
        addr += 1;
    }

    let mut pc = 0u16 + START_PC.flag;
    let start_pc = pc;
    let mut dis;
    loop {
        (dis, pc) = step(pc, &ram);
        println!("{}", dis);
        if pc >= start_pc + bytes.len() as u16 {
            break;
        }
    }
}
