//! monitor implements a basic monitor program for running a 65xx device.
use clap::Parser;
use color_eyre::eyre::{eyre, Result};
use rusty6502::prelude::*;
use std::cell::RefCell;
use std::fs::{read, write};
use std::io::Write;
use std::path::Path;
use std::rc::Rc;
use std::sync::mpsc::{channel, Receiver, Sender, TryRecvError};
use std::{io, thread};
use strum_macros::{Display, EnumString};

mod commands;

use commands::{Command, CommandResponse, Location, LocationRange, Stop, StopReason, Val};

/// monitor will start a 65xx of the given chip and begin a blank memory session.
///
/// It can take an optional memory image file (.bin generally) or a
/// .prg file (c64 basic program which loads at 0x0801)
/// and load it before turning over control.
///
/// If it's a c64 basic program the basic code will be interpreted and anything
/// remaining after will be disassembled as assembly.
#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    cpu_type: Type,

    #[arg(help = "Filename containing binary image or PRG file")]
    filename: Option<String>,

    #[arg(
        help = "Offset into RAM to start loading data. All other RAM will be zero'd out. Ignored for PRG files."
    )]
    offset: Option<u16>,
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

fn main() -> Result<()> {
    color_eyre::install()?;
    let args: Args = Args::parse();

    // The command channel.
    let (cmdtx, cmdrx) = channel();
    // The response channel.
    let (resptx, resprx) = channel();

    thread::spawn(move || cpu_loop(args.cpu_type, &cmdrx, &resptx));

    if let Some(file) = args.filename {
        if let Some(offset) = args.offset {
            cmdtx.send(Command::Load(file, Some(Location { addr: offset })))?;
        } else {
            cmdtx.send(Command::Load(file, None))?;
        }
        // Grab the response check for receive error then decode it to get a load error.
        // TODO: Handle this pretty, not via panic.
        resprx.recv()??;
    }
    main_loop(&cmdtx, &resprx)?;
    Ok(())
}

#[allow(clippy::too_many_lines)]
fn main_loop(tx: &Sender<Command>, rx: &Receiver<Result<CommandResponse>>) -> Result<()> {
    let (stdintx, stdinrx) = channel();
    thread::spawn(move || loop {
        let mut buffer = String::new();
        #[allow(clippy::unwrap_used)]
        io::stdin().read_line(&mut buffer).unwrap();
        #[allow(clippy::unwrap_used)]
        stdintx.send(buffer).unwrap();
    });
    let mut running = false;
    print!("> ");
    #[allow(clippy::unwrap_used)]
    io::stdout().flush().unwrap();

    loop {
        match stdinrx.try_recv() {
            Ok(line) => {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() > 4 {
                    println!("ERROR: Invalid command - {line}");
                    continue;
                }
                if !parts.is_empty() {
                    let cmd = parts[0].to_uppercase();

                    match cmd.as_str() {
                        "H" | "HELP" => {
                            println!("Usage:\n");
                            println!("RUN - run continually until either a breakpoint/watchpoint is hit or a STOP is sent");
                            println!("STOP - cause the CPU to stop at the current instrunction from continual running");
                            println!("BP <addr> - Break when PC is equal to the addr");
                            println!("BPL - List all breakpoints");
                            println!("DB <num> - Delete the given breakpoint");
                            println!("S - Step instruction (2-8 clock cycles)");
                            println!("T - Tick instruction (one clock cycle)");
                            println!(
                                "R <addr> - Read the given memory location and return its value"
                            );
                            println!(
                            "RR <addr> <len> - Read starting at the given location for len times."
                        );
                            println!("                - NOTE: When printing this will show a memory dump but the only");
                            println!(
                                "                -       defined values are from the range given"
                            );
                            println!("W <addr> <len> - Write the value to the addr given");
                            println!("WR <addr> <len> <val> - Write the value to the range of addresses given");
                            println!("CPU - Dump the current CPU state");
                            println!("RAM - Dump the current RAM contents");
                            println!("D <addr> - Disassemble at the given address");
                            println!("DR <addr> <len> - Disassemble starting at the given address for until the address given");
                            println!("                  by addr+len is hit");
                            println!("WP <addr> - Set a watchpoint on the given memory location");
                            println!("WPL - List all watchpoints");
                            println!("DW <num> - Delete the given watchpoint");
                            println!("L <path> [<start>] - Load a binary image and optionally reset PC to the start addr");
                            println!("BIN <path> - Dump a memory image of RAM to the given path");
                            println!("PC <addr> - Set the PC to the addr");
                            println!("RESET - Run a reset sequence on the CPU");
                            println!("QUIT - Exit the monitor");
                            println!();
                        }
                        "QUIT" => std::process::exit(0),
                        "RUN" => {
                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::Run).unwrap();
                            running = true;
                        }
                        "STOP" => {
                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::Stop).unwrap();
                            #[allow(clippy::unwrap_used)]
                            let r = rx.recv().unwrap();
                            match r {
                                Ok(CommandResponse::Stop(st)) => print_state(&st),
                                Err(e) => println!("Stop error - {e}"),
                                _ => panic!("Invalid return from Stop - {r:?}"),
                            }
                            running = false;
                        }
                        "B" => {
                            if parts.len() != 2 {
                                println!("Error - Break must include an addr - B <addr>");
                                continue;
                            }
                            match parse_u16(parts[1]) {
                                Ok(addr) => {
                                    #[allow(clippy::unwrap_used)]
                                    tx.send(Command::Break(Location { addr })).unwrap();
                                    #[allow(clippy::unwrap_used)]
                                    let r = rx.recv().unwrap();
                                    match r {
                                        Ok(CommandResponse::Break) => {}
                                        Err(e) => println!("Break error - {e}"),
                                        _ => panic!("Invalid return from Break - {r:?}"),
                                    }
                                }
                                Err(e) => {
                                    println!("Error - parse error on addr: {e}");
                                    continue;
                                }
                            }
                        }
                        "BPL" => {
                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::BreakList).unwrap();
                            #[allow(clippy::unwrap_used)]
                            let r = rx.recv().unwrap();
                            match r {
                                Ok(CommandResponse::BreakList(bl)) => {
                                    println!("Breakpoints:");
                                    for (pos, l) in bl.iter().enumerate() {
                                        println!("{pos} - ${:04X}", l.addr);
                                    }
                                }
                                Err(e) => println!("BreakList error - {e}"),
                                _ => panic!("Invalid return from BreakList - {r:?}"),
                            }
                        }
                        "DB" => {
                            if parts.len() != 2 {
                                println!(
                                    "Error - Delete Breakpoint must include an num - DB <num>"
                                );
                                continue;
                            }
                            match parts[1].parse::<usize>() {
                                Ok(num) => {
                                    #[allow(clippy::unwrap_used)]
                                    tx.send(Command::DeleteBreakpoint(num)).unwrap();
                                    #[allow(clippy::unwrap_used)]
                                    let r = rx.recv().unwrap();
                                    match r {
                                        Ok(CommandResponse::DeleteBreakpoint) => {}
                                        Err(e) => println!("Delete Breakpoint error - {e}"),
                                        _ => {
                                            panic!("Invalid return from Delete Breakpoint - {r:?}")
                                        }
                                    }
                                }
                                Err(e) => {
                                    println!("Error - parse error on num: {e}");
                                    continue;
                                }
                            }
                        }
                        "S" => {
                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::Step).unwrap();
                            #[allow(clippy::unwrap_used)]
                            let r = rx.recv().unwrap();
                            match r {
                                Ok(CommandResponse::Step(st)) => print_state(&st),
                                Err(e) => println!("Step error - {e}"),
                                _ => panic!("Invalid return from Step - {r:?}"),
                            }
                        }
                        "T" => {
                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::Tick).unwrap();
                            #[allow(clippy::unwrap_used)]
                            let r = rx.recv().unwrap();
                            match r {
                                Ok(CommandResponse::Tick(st)) => print_state(&st),
                                Err(e) => println!("Tick error - {e}"),
                                _ => panic!("Invalid return from Tick - {r:?}"),
                            }
                        }
                        "R" => {
                            if parts.len() != 2 {
                                println!("Error - Read must include an addr - R <addr>");
                                continue;
                            }
                            match parse_u16(parts[1]) {
                                Ok(addr) => {
                                    #[allow(clippy::unwrap_used)]
                                    tx.send(Command::Read(Location { addr })).unwrap();
                                    #[allow(clippy::unwrap_used)]
                                    let r = rx.recv().unwrap();
                                    match r {
                                        Ok(CommandResponse::Read(r)) => {
                                            println!("{addr:04X}  {:02X}", r.val);
                                        }
                                        Err(e) => println!("Read error - {e}"),
                                        _ => panic!("Invalid return from Read - {r:?}"),
                                    }
                                }
                                Err(e) => {
                                    println!("Error - parse error on addr: {e}");
                                    continue;
                                }
                            }
                        }
                        "RR" => {
                            if parts.len() != 3 {
                                println!(
                                "Error - Read Range must include an addr and len - RR <addr> <len>"
                            );
                                continue;
                            }
                            let addr = match parse_u16(parts[1]) {
                                Ok(addr) => addr,
                                Err(e) => {
                                    println!("Error - parse error on addr: {e}");
                                    continue;
                                }
                            };
                            let len = match parse_u16(parts[2]) {
                                Ok(len) => len,
                                Err(e) => {
                                    println!("Error - parse error on len: {e}");
                                    continue;
                                }
                            };
                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::ReadRange(LocationRange {
                                addr,
                                len: Some(len),
                            }))
                            .unwrap();
                            #[allow(clippy::unwrap_used)]
                            let r = rx.recv().unwrap();
                            match r {
                                Ok(CommandResponse::ReadRange(l)) => {
                                    // Memory has a good Display impl we can use.
                                    let mut r = [0; MAX_SIZE];
                                    for (pos, v) in l.iter().enumerate() {
                                        r[addr as usize + pos] = v.val;
                                    }
                                    print!("{}", &r as &dyn Memory);
                                }
                                Err(e) => println!("Read Range error - {e}"),
                                _ => panic!("Invalid return from Read Range - {r:?}"),
                            }
                        }
                        "W" => {
                            if parts.len() != 3 {
                                println!(
                                    "Error - Write must include an addr and value - W <addr> <val>"
                                );
                                continue;
                            }
                            let addr = match parse_u16(parts[1]) {
                                Ok(addr) => addr,
                                Err(e) => {
                                    println!("Error - parse error on addr: {e}");
                                    continue;
                                }
                            };
                            let val = match parse_u8(parts[2]) {
                                Ok(val) => val,
                                Err(e) => {
                                    println!("Error - parse error on val: {e}");
                                    continue;
                                }
                            };

                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::Write(Location { addr }, Val { val }))
                                .unwrap();
                            #[allow(clippy::unwrap_used)]
                            let r = rx.recv().unwrap();
                            match r {
                                Ok(CommandResponse::Write) => {}
                                Err(e) => println!("Write error - {e}"),
                                _ => panic!("Invalid return from Write - {r:?}"),
                            }
                        }
                        "WR" => {
                            if parts.len() != 4 {
                                println!("Error - Write Range must include an addr len and val - WR <addr> <len> <val>");
                                continue;
                            }
                            let addr = match parse_u16(parts[1]) {
                                Ok(addr) => addr,
                                Err(e) => {
                                    println!("Error - parse error on addr: {e}");
                                    continue;
                                }
                            };
                            let len = match parse_u16(parts[2]) {
                                Ok(len) => len,
                                Err(e) => {
                                    println!("Error - parse error on len: {e}");
                                    continue;
                                }
                            };
                            let val = match parse_u8(parts[3]) {
                                Ok(val) => val,
                                Err(e) => {
                                    println!("Error - parse error on val: {e}");
                                    continue;
                                }
                            };
                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::WriteRange(
                                LocationRange {
                                    addr,
                                    len: Some(len),
                                },
                                Val { val },
                            ))
                            .unwrap();
                            #[allow(clippy::unwrap_used)]
                            let r = rx.recv().unwrap();
                            match r {
                                Ok(CommandResponse::WriteRange) => {}
                                Err(e) => println!("Write Range error - {e}"),
                                _ => panic!("Invalid return from Write Range - {r:?}"),
                            }
                        }
                        "CPU" => {
                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::Cpu).unwrap();
                            #[allow(clippy::unwrap_used)]
                            let r = rx.recv().unwrap();
                            match r {
                                Ok(CommandResponse::Cpu(st)) => print_state(&Stop {
                                    state: st,
                                    reason: StopReason::None,
                                }),
                                Err(e) => println!("Cpu error - {e}"),
                                _ => panic!("Invalid return from Cpu - {r:?}"),
                            }
                        }
                        "RAM" => {
                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::Ram).unwrap();
                            #[allow(clippy::unwrap_used)]
                            let r = rx.recv().unwrap();
                            match r {
                                Ok(CommandResponse::Ram(r)) => print!("{}", &r as &dyn Memory),
                                Err(e) => println!("Ram error - {e}"),
                                _ => panic!("Invalid return from Ram - {r:?}"),
                            }
                        }
                        "D" => {
                            if parts.len() != 2 {
                                println!("Error - Disassemble must include an addr - D <addr>");
                                continue;
                            }
                            match parse_u16(parts[1]) {
                                Ok(addr) => {
                                    #[allow(clippy::unwrap_used)]
                                    tx.send(Command::Disassemble(Location { addr })).unwrap();
                                    #[allow(clippy::unwrap_used)]
                                    let r = rx.recv().unwrap();
                                    match r {
                                        Ok(CommandResponse::Disassemble(d)) => println!("{d}"),
                                        Err(e) => println!("Disassemble error - {e}"),
                                        _ => panic!("Invalid return from Disassemble - {r:?}"),
                                    }
                                }
                                Err(e) => {
                                    println!("Error - parse error on addr: {e}");
                                    continue;
                                }
                            }
                        }
                        "DR" => {
                            if parts.len() != 3 {
                                println!(
                                "Error - Disassemble Range must include an addr and len - DR <addr> <len>"
                            );
                                continue;
                            }
                            let addr = match parse_u16(parts[1]) {
                                Ok(addr) => addr,
                                Err(e) => {
                                    println!("Error - parse error on addr: {e}");
                                    continue;
                                }
                            };
                            let len = match parse_u16(parts[2]) {
                                Ok(len) => len,
                                Err(e) => {
                                    println!("Error - parse error on len: {e}");
                                    continue;
                                }
                            };
                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::DisassembleRange(LocationRange {
                                addr,
                                len: Some(len),
                            }))
                            .unwrap();
                            #[allow(clippy::unwrap_used)]
                            let r = rx.recv().unwrap();
                            match r {
                                Ok(CommandResponse::DisassembleRange(l)) => {
                                    for d in l {
                                        println!("{d}");
                                    }
                                }
                                Err(e) => println!("Dissasemble Range error - {e}"),
                                _ => panic!("Invalid return from Disassemble Range - {r:?}"),
                            }
                        }
                        "WP" => {
                            if parts.len() != 2 {
                                println!("Error - Watchpoint must include an addr - WP <addr>");
                                continue;
                            }
                            match parse_u16(parts[1]) {
                                Ok(addr) => {
                                    #[allow(clippy::unwrap_used)]
                                    tx.send(Command::Watch(Location { addr })).unwrap();
                                    #[allow(clippy::unwrap_used)]
                                    let r = rx.recv().unwrap();
                                    match r {
                                        Ok(CommandResponse::Watch) => {}
                                        Err(e) => println!("Watch error - {e}"),
                                        _ => panic!("Invalid return from Watch - {r:?}"),
                                    }
                                }
                                Err(e) => {
                                    println!("Error - parse error on addr: {e}");
                                    continue;
                                }
                            }
                        }
                        "WPL" => {
                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::WatchList).unwrap();
                            #[allow(clippy::unwrap_used)]
                            let r = rx.recv().unwrap();
                            match r {
                                Ok(CommandResponse::WatchList(wl)) => {
                                    println!("Watchpoints:");
                                    for (pos, l) in wl.iter().enumerate() {
                                        println!("{pos} - ${:04X}", l.addr);
                                    }
                                }
                                Err(e) => println!("WatchList error - {e}"),
                                _ => panic!("Invalid return from WatchList - {r:?}"),
                            }
                        }
                        "DW" => {
                            if parts.len() != 2 {
                                println!(
                                    "Error - Delete Watchpoint must include an num - DW <num>"
                                );
                                continue;
                            }
                            match parts[1].parse::<usize>() {
                                Ok(num) => {
                                    #[allow(clippy::unwrap_used)]
                                    tx.send(Command::DeleteWatchpoint(num)).unwrap();
                                    #[allow(clippy::unwrap_used)]
                                    let r = rx.recv().unwrap();
                                    match r {
                                        Ok(CommandResponse::DeleteWatchpoint) => {}
                                        Err(e) => println!("Delete Watchpoint error - {e}"),
                                        _ => {
                                            panic!("Invalid return from Delete Watchpoint - {r:?}")
                                        }
                                    }
                                }
                                Err(e) => {
                                    println!("Error - parse error on num: {e}");
                                    continue;
                                }
                            }
                        }
                        "L" => {
                            if parts.len() != 2 && parts.len() != 3 {
                                println!("Error - Load must include a filename and optionally load location - L <path to file> [location]");
                                continue;
                            }
                            let file = String::from(parts[1]);
                            let mut loc = None;
                            if parts.len() == 3 {
                                let addr = match parse_u16(parts[2]) {
                                    Ok(addr) => addr,
                                    Err(e) => {
                                        println!("Error - parse error on len: {e}");
                                        continue;
                                    }
                                };
                                loc = Some(Location { addr });
                            }
                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::Load(file, loc)).unwrap();
                            #[allow(clippy::unwrap_used)]
                            let r = rx.recv().unwrap();
                            match r {
                                Ok(CommandResponse::Load) => {}
                                Err(e) => println!("Load error - {e}"),
                                _ => panic!("Invalid return from Load - {r:?}"),
                            }
                        }
                        "BIN" => {
                            if parts.len() != 2 {
                                println!(
                                    "Error - Dump must include a filename - BIN <path to file>"
                                );
                                continue;
                            }
                            let file = String::from(parts[1]);
                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::Dump(file)).unwrap();
                            #[allow(clippy::unwrap_used)]
                            let r = rx.recv().unwrap();
                            match r {
                                Ok(CommandResponse::Dump) => {}
                                Err(e) => println!("Dump error - {e}"),
                                _ => panic!("Invalid return from Dump - {r:?}"),
                            }
                        }
                        "PC" => {
                            if parts.len() != 2 {
                                println!("Error - PC must include an addr - PC <addr>");
                                continue;
                            }
                            let addr = match parse_u16(parts[1]) {
                                Ok(addr) => addr,
                                Err(e) => {
                                    println!("Error - parse error on len: {e}");
                                    continue;
                                }
                            };
                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::PC(Location { addr })).unwrap();
                            #[allow(clippy::unwrap_used)]
                            let r = rx.recv().unwrap();
                            match r {
                                Ok(CommandResponse::PC(st)) => print_state(&Stop {
                                    state: st,
                                    reason: StopReason::None,
                                }),
                                Err(e) => println!("PC error - {e}"),
                                _ => panic!("Invalid return from PC - {r:?}"),
                            }
                        }
                        "RESET" => {
                            #[allow(clippy::unwrap_used)]
                            tx.send(Command::Reset).unwrap();
                            #[allow(clippy::unwrap_used)]
                            let r = rx.recv().unwrap();
                            match r {
                                Ok(CommandResponse::Reset(st)) => print_state(&Stop {
                                    state: st,
                                    reason: StopReason::None,
                                }),
                                Err(e) => println!("Reset error - {e}"),
                                _ => panic!("Invalid return from Reset - {r:?}"),
                            }
                            running = false;
                        }
                        _ => {
                            println!("ERROR: Invalid command - {}", parts[0]);
                            continue;
                        }
                    }
                }
                print!("> ");
                #[allow(clippy::unwrap_used)]
                io::stdout().flush().unwrap();
            }
            Err(TryRecvError::Empty) => {}
            Err(TryRecvError::Disconnected) => panic!("stdin died?"),
        }
        // Once it's running we have to juggle stdin vs possibly brk/watch happening.
        if running {
            match rx.try_recv() {
                Ok(s) => match s {
                    Ok(ret) => match ret {
                        CommandResponse::Stop(st) => print_state(&st),
                        _ => panic!("invalid response from run: {ret:?}"),
                    },
                    Err(e) => println!("Error from Run - {e}"),
                },
                Err(TryRecvError::Empty) => {}
                Err(TryRecvError::Disconnected) => panic!("Sender channel died"),
            }
        }
    }
}

fn parse_u16(val: &str) -> Result<u16> {
    let (trimmed, base) = match val.as_bytes() {
        // Remove any possibly leading 0x or $. If we did this is also base 16
        // Can index back into val without worry about utf8 since we only matched
        // ascii chars here.
        [b'0', b'x', ..] => (&val[2..], 16),
        [b'$', ..] => (&val[1..], 16),
        _ => (val, 10),
    };

    // If nothing was left we're done with no value.
    if trimmed.is_empty() {
        return Err(eyre!("no input"));
    }

    match u16::from_str_radix(trimmed, base) {
        Ok(v) => Ok(v),
        Err(e) => Err(eyre!("parse error: {e}")),
    }
}

fn parse_u8(val: &str) -> Result<u8> {
    let (trimmed, base) = match val.as_bytes() {
        // Remove any possibly leading 0x or $. If we did this is also base 16
        // Can index back into val without worry about utf8 since we only matched
        // ascii chars here.
        [b'0', b'x', ..] => (&val[2..], 16),
        [b'$', ..] => (&val[1..], 16),
        _ => (val, 10),
    };

    // If nothing was left we're done with no value.
    if trimmed.is_empty() {
        return Err(eyre!("no input"));
    }

    match u8::from_str_radix(trimmed, base) {
        Ok(v) => Ok(v),
        Err(e) => Err(eyre!("parse error: {e}")),
    }
}

fn print_state(st: &Stop) {
    // Different from just using Display for CPUState since we don't want the
    // memory dump and also clocks aren't as important.
    if let StopReason::Break(addr) = st.reason {
        println!("Breakpoint at {addr:04X}");
    }
    if let StopReason::Watch(addr) = st.reason {
        println!(
            "Watchpoint triggered for addr {addr:04X} at {:04X}",
            st.state.pc
        );
    }
    println!(
            "{:>24}: A: {:02X} X: {:02X} Y: {:02X} S: {:02X} P: {} op_val: {:02X} op_addr: {:04X} op_tick: {} cycles: {}",
            st.state.dis, st.state.a, st.state.x, st.state.y, st.state.s, st.state.p, st.state.op_val, st.state.op_addr, st.state.op_tick, st.state.clocks);
}

struct Debug {
    state: Rc<RefCell<CPUState>>,
    full: bool,
}

impl Debug {
    fn debug(&self) -> (Rc<RefCell<CPUState>>, bool) {
        (Rc::clone(&self.state), self.full)
    }
}

#[allow(clippy::similar_names, clippy::too_many_lines)]
fn cpu_loop(ty: Type, rx: &Receiver<Command>, tx: &Sender<Result<CommandResponse>>) {
    let mut nmos = CPU6502::new(ChipDef::default());
    let mut ricoh = CPURicoh::new(ChipDef::default());
    let mut c6510 = CPU6510::new(ChipDef::default(), None);
    let mut cmos = CPU65C02::new(ChipDef::default());
    let cpu: &mut dyn CPU = match ty {
        Type::NMOS => &mut nmos,
        Type::RICOH => &mut ricoh,
        Type::NMOS6510 => &mut c6510,
        Type::CMOS => &mut cmos,
    };

    let mut is_running = false;
    let mut is_init = false;
    let d = Debug {
        state: Rc::new(RefCell::new(CPUState::default())),
        full: false,
    };
    let debug = { || d.debug() };
    cpu.set_debug(Some(&debug));

    let step = |cpu: &mut dyn CPU| -> Result<()> {
        loop {
            cpu.tick()?;
            cpu.tick_done()?;
            cpu.debug();

            if d.state.borrow().op_tick == Tick::Reset {
                break;
            }
        }
        Ok(())
    };
    let valid_range = |range: &LocationRange| -> Result<u16> {
        let mut len = 0;
        if let Some(l) = range.len {
            len = l;
        }
        if (usize::from(range.addr) + usize::from(len)) > MAX_SIZE {
            #[allow(clippy::unwrap_used)]
            tx.send(Err(eyre!(
                "invalid size {} + {} exceeds {MAX_SIZE}",
                range.addr,
                len
            )))
            .unwrap();
            return Err(eyre!("range error"));
        }
        Ok(len)
    };

    let mut breakpoints: Vec<Location> = Vec::new();
    let mut watchpoints: Vec<Location> = Vec::new();

    loop {
        if is_running {
            let mut ram = [0; MAX_SIZE];
            if !watchpoints.is_empty() {
                // If we have watchpoints get a memory snapshot.
                cpu.ram().borrow().ram(&mut ram);
            }
            let r = step(cpu);
            if let Err(e) = r {
                #[allow(clippy::unwrap_used)]
                tx.send(Err(eyre!("step error: {e}"))).unwrap();
                return;
            }

            cpu.debug();
            (d.state.borrow_mut().dis, _) = cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
            let mut reason = StopReason::Stop;
            for b in &breakpoints {
                if b.addr == cpu.pc() {
                    reason = StopReason::Break(b.addr);
                }
            }
            if !watchpoints.is_empty() {
                let cr = cpu.ram();
                let cr = cr.borrow();
                for w in &watchpoints {
                    if cr.read(w.addr) != ram[usize::from(w.addr)] {
                        reason = StopReason::Watch(w.addr);
                        break;
                    }
                }
            }
            if reason != StopReason::Stop {
                let st = Stop {
                    state: d.state.borrow().clone(),
                    reason,
                };
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::Stop(st))).unwrap();
                is_running = false;
            }
        }

        // Peek at the channel. If it's an error but empty and we're
        // just running then we loop around and step again. A real error
        // aborts for now since we can't recover from a closed channel.
        // This way in running we match per instruction but also check
        // after each one for a Stop.
        let c = rx.try_recv();
        if let Err(e) = c {
            assert!(!(e == TryRecvError::Disconnected), "rx dropped!");
            if e == TryRecvError::Empty && is_running {
                continue;
            }
        }

        // At this point we're not actively running or we've pulled a command
        // off the channel. So either process that or wait and pull one.
        // Can block now as we're not otherwise doing anything.
        let c = if let Ok(c) = c {
            // If we pulled one off and we're running make sure it's valid.
            // If not write an error and loop back to running.
            if is_running {
                match c {
                    Command::Stop | Command::Reset => {}
                    _ => {
                        #[allow(clippy::unwrap_used)]
                        tx.send(Err(eyre!("only Stop and Reset allowed in Run state")))
                            .unwrap();
                        continue;
                    }
                };
            }
            c
        } else {
            let x = rx.recv();
            assert!(x.is_ok(), "rx dropped!");
            // SAFETY: We know this is Ok now
            unsafe { x.unwrap_unchecked() }
        };

        match c {
            Command::Run => {
                if !is_init {
                    is_init = true;
                    #[allow(clippy::unwrap_used)]
                    cpu.power_on().unwrap();
                }
                is_running = true;
            }
            Command::Stop => {
                is_running = false;
                cpu.debug();
                (d.state.borrow_mut().dis, _) =
                    cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
                let st = Stop {
                    state: d.state.borrow().clone(),
                    reason: StopReason::Stop,
                };
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::Stop(st))).unwrap();
            }
            Command::Break(addr) => {
                breakpoints.push(addr);
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::Break)).unwrap();
            }
            Command::BreakList => {
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::BreakList(breakpoints.clone())))
                    .unwrap();
            }
            Command::DeleteBreakpoint(num) => {
                if num >= breakpoints.len() {
                    #[allow(clippy::unwrap_used)]
                    tx.send(Err(eyre!(
                        "breakpoint index {num} out of range 0-{}",
                        breakpoints.len() - 1
                    )))
                    .unwrap();
                    continue;
                }
                breakpoints.swap_remove(num);
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::DeleteBreakpoint)).unwrap();
            }
            Command::Step => {
                if !is_init {
                    is_init = true;
                    #[allow(clippy::unwrap_used)]
                    cpu.power_on().unwrap();
                }
                let mut ram = [0; MAX_SIZE];
                if !watchpoints.is_empty() {
                    // If we have watchpoints get a memory snapshot.
                    cpu.ram().borrow().ram(&mut ram);
                }
                let r = step(cpu);
                if let Err(e) = r {
                    #[allow(clippy::unwrap_used)]
                    tx.send(Err(eyre!("step error: {e}"))).unwrap();
                    continue;
                }
                (d.state.borrow_mut().dis, _) =
                    cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
                let mut reason = StopReason::Step;
                for b in &breakpoints {
                    if b.addr == cpu.pc() {
                        reason = StopReason::Break(b.addr);
                    }
                }
                if !watchpoints.is_empty() {
                    let cr = cpu.ram();
                    let cr = cr.borrow();
                    for w in &watchpoints {
                        if cr.read(w.addr) != ram[usize::from(w.addr)] {
                            reason = StopReason::Watch(w.addr);
                            break;
                        }
                    }
                }
                let st = Stop {
                    state: d.state.borrow().clone(),
                    reason,
                };
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::Step(st))).unwrap();
            }
            Command::Tick => {
                if !is_init {
                    is_init = true;
                    #[allow(clippy::unwrap_used)]
                    cpu.power_on().unwrap();
                }
                let mut ram = [0; MAX_SIZE];
                if !watchpoints.is_empty() {
                    // If we have watchpoints get a memory snapshot.
                    cpu.ram().borrow().ram(&mut ram);
                }
                let r = cpu.tick();
                if let Err(e) = r {
                    #[allow(clippy::unwrap_used)]
                    tx.send(Err(eyre!("tick error: {e}"))).unwrap();
                    return;
                }
                let r = cpu.tick_done();
                if let Err(e) = r {
                    #[allow(clippy::unwrap_used)]
                    tx.send(Err(eyre!("tick_done error: {e}"))).unwrap();
                    return;
                }
                cpu.debug();
                (d.state.borrow_mut().dis, _) =
                    cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
                let mut reason = StopReason::Tick;
                for b in &breakpoints {
                    if b.addr == cpu.pc() {
                        reason = StopReason::Break(b.addr);
                    }
                }
                if !watchpoints.is_empty() {
                    let cr = cpu.ram();
                    let cr = cr.borrow();
                    for w in &watchpoints {
                        if cr.read(w.addr) != ram[usize::from(w.addr)] {
                            reason = StopReason::Watch(w.addr);
                            break;
                        }
                    }
                }
                let st = Stop {
                    state: d.state.borrow().clone(),
                    reason,
                };
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::Tick(st))).unwrap();
            }

            Command::Read(addr) => {
                let val = cpu.ram().borrow().read(addr.addr);
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::Read(Val { val }))).unwrap();
            }
            Command::ReadRange(range) => {
                if let Ok(len) = valid_range(&range) {
                    let mut r = Vec::new();
                    for i in 0..len {
                        let val = cpu.ram().borrow().read(range.addr + i);
                        r.push(Val { val });
                    }
                    #[allow(clippy::unwrap_used)]
                    tx.send(Ok(CommandResponse::ReadRange(r))).unwrap();
                }
            }
            Command::Write(addr, val) => {
                cpu.ram().borrow_mut().write(addr.addr, val.val);
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::Write)).unwrap();
            }
            Command::WriteRange(range, val) => {
                if let Ok(len) = valid_range(&range) {
                    for i in 0..len {
                        cpu.ram().borrow_mut().write(range.addr + i, val.val);
                    }
                    #[allow(clippy::unwrap_used)]
                    tx.send(Ok(CommandResponse::WriteRange)).unwrap();
                }
            }
            Command::Cpu => {
                cpu.debug();
                (d.state.borrow_mut().dis, _) =
                    cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::Cpu(d.state.borrow().clone())))
                    .unwrap();
            }
            Command::Ram => {
                let mut r = [0u8; MAX_SIZE];
                cpu.ram().borrow().ram(&mut r);
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::Ram(r))).unwrap();
            }
            Command::Disassemble(addr) => {
                let (s, _) = cpu.disassemble(addr.addr, cpu.ram().borrow().as_ref());
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::Disassemble(s))).unwrap();
            }
            Command::DisassembleRange(range) => {
                if let Ok(len) = valid_range(&range) {
                    let mut r = Vec::new();
                    let mut pc = range.addr;
                    while pc < range.addr + len {
                        let (s, newpc) = cpu.disassemble(pc, cpu.ram().borrow().as_ref());
                        r.push(s);
                        pc = newpc;
                    }
                    #[allow(clippy::unwrap_used)]
                    tx.send(Ok(CommandResponse::DisassembleRange(r))).unwrap();
                }
            }
            Command::Watch(addr) => {
                watchpoints.push(addr);
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::Watch)).unwrap();
            }
            Command::WatchList => {
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::WatchList(watchpoints.clone())))
                    .unwrap();
            }
            Command::DeleteWatchpoint(num) => {
                if num >= watchpoints.len() {
                    #[allow(clippy::unwrap_used)]
                    tx.send(Err(eyre!(
                        "watchpoint index {num} out of range 0-{}",
                        watchpoints.len() - 1
                    )))
                    .unwrap();
                    continue;
                }
                watchpoints.swap_remove(num);
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::DeleteWatchpoint)).unwrap();
            }
            Command::Load(file, start) => {
                let start = if let Some(s) = start {
                    s
                } else {
                    Location { addr: 0x0000 }
                };
                let path = Path::new(&file);
                match read(path) {
                    Ok(b) => {
                        if b.len() > MAX_SIZE || (usize::from(start.addr) + b.len()) > MAX_SIZE {
                            #[allow(clippy::unwrap_used)]
                            tx.send(Err(eyre!(
                                "file too large {} at offset {}",
                                b.len(),
                                start.addr
                            )))
                            .unwrap();
                            continue;
                        }
                        for (addr, b) in b.iter().enumerate() {
                            #[allow(clippy::cast_possible_truncation)]
                            cpu.ram().borrow_mut().write(start.addr + addr as u16, *b);
                        }
                    }
                    Err(e) => {
                        #[allow(clippy::unwrap_used)]
                        tx.send(Err(eyre!("can't read file: {e}"))).unwrap();
                        continue;
                    }
                }
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::Load)).unwrap();
                continue;
            }
            Command::Dump(file) => {
                let mut r = [0; MAX_SIZE];
                cpu.ram().borrow().ram(&mut r);
                match write(Path::new(&file), r) {
                    Ok(_) => {
                        #[allow(clippy::unwrap_used)]
                        tx.send(Ok(CommandResponse::Dump)).unwrap();
                    }
                    Err(e) => {
                        #[allow(clippy::unwrap_used)]
                        tx.send(Err(eyre!("can't write file: {e}"))).unwrap();
                    }
                }
            }
            Command::PC(addr) => {
                cpu.pc_mut(addr.addr);
                #[allow(clippy::unwrap_used)]
                cpu.debug();
                (d.state.borrow_mut().dis, _) =
                    cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
                #[allow(clippy::unwrap_used)]
                tx.send(Ok(CommandResponse::PC(d.state.borrow().clone())))
                    .unwrap();
            }
            Command::Reset => loop {
                match cpu.reset() {
                    Ok(OpState::Done) => {
                        cpu.debug();
                        (d.state.borrow_mut().dis, _) =
                            cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
                        #[allow(clippy::unwrap_used)]
                        tx.send(Ok(CommandResponse::Reset(d.state.borrow().clone())))
                            .unwrap();
                        break;
                    }
                    Ok(OpState::Processing) => continue,
                    Err(e) => {
                        #[allow(clippy::unwrap_used)]
                        tx.send(Err(eyre!("reset error: {e}"))).unwrap();
                        break;
                    }
                }
            },
        }
    }
}
