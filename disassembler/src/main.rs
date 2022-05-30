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

    let start = START_PC.flag;
    let mut addr = OFFSET.flag;
    let filename = args[0];
    let mut pc = 0u16;
    println!("start: {start} offset: {addr} args: {filename}",);

    let mut bytes = read(args[0]).expect(format!("can't read file: {filename}").as_str());
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
