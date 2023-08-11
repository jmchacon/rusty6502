//! monitor implements a library for running a 65xx chip in a monitor and
//! then interfacing to the outside world via input/output channels.
use color_eyre::eyre::{eyre, Result};
use rusty6502::prelude::*;
use std::cell::RefCell;
use std::fmt::Write as fmtWrite;
use std::fs::{read, write};
use std::path::Path;
use std::rc::Rc;
use std::sync::mpsc::{Receiver, Sender, TryRecvError};
use strum_macros::{Display, EnumString};

mod commands;

use commands::{Command, CommandResponse, Location, LocationRange, Stop, StopReason, Val, PC};

/// Type defines the various implementations of the 6502 available.
#[derive(Copy, Clone, Debug, Display, PartialEq, Eq, EnumString)]
#[allow(clippy::upper_case_acronyms)]
pub enum Type {
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

/// This function is the main input loop which expects a channel to receive
/// commands over and a channel connecting to/from the CPU to send commands
/// and receive responses.
///
/// NOTE: preload is set to true if the caller already executed a load
///       instruction before starting this loop. This way the proper prompt is
///       emitted.
///
/// # Errors
/// Any premature close of the channels will result in an error.
/// Additionally any invalid state transitions will result in an error.
#[allow(clippy::too_many_lines)]
pub fn input_loop(
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
    inputtx: &Receiver<String>,
    outputtx: &Sender<(String, bool)>,
    preload: bool,
) -> Result<()> {
    // Only print if we didn't preload something since that will be
    // already in stdin and processed below and print the prompt.
    if !preload {
        outputtx.send((String::new(), true))?;
    }

    let mut running = 0;
    loop {
        match inputtx.try_recv() {
            Ok(line) => {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() > 4 {
                    outputtx.send((format!("ERROR: Invalid command - {line}\n"), true))?;
                    continue;
                }
                if !parts.is_empty() {
                    let cmd = parts[0].to_uppercase();

                    match cmd.as_str() {
                        "H" | "HELP" => {
                            outputtx.send((
                                String::from(
                                    r#"Usage:
HELP | H - This usage information
RUN | C - run continually until either a breakpoint/watchpoint is hit or a STOP is sent
STOP - cause the CPU to stop at the current instrunction from continual running
BP <addr> - Break when PC is equal to the addr
BPL - List all breakpoints
DB <num> - Delete the given breakpoint
S - Step instruction (2-8 clock cycles)
T - Tick instruction (one clock cycle)
R <addr> - Read the given memory location and return its value
RR <addr> <len> - Read starting at the given location for len times.
                  NOTE: When printing this will show a memory dump but the only
                        defined values are from the range given
W <addr> <len> - Write the value to the addr given
WR <addr> <len> <val> - Write the value to the range of addresses given
CPU - Dump the current CPU state
RAM - Dump the current RAM contents
D <addr> - Disassemble at the given address
DR <addr> <len> - Disassemble starting at the given address for until the address given
                  by addr+len is hit
WP <addr> - Set a watchpoint on the given memory location
WPL - List all watchpoints
DW <num> - Delete the given watchpoint
L <path> [<start> [<pc>]] - Load a binary image and optionally start loading at the given
                            address and optionally reset PC to the start addr
                            NOTE: Load implies a RESET sequence will be run as well
BIN <path> - Dump a memory image of RAM to the given path
PC <addr> - Set the PC to the addr
RESET - Run a reset sequence on the CPU
QUIT | Q - Exit the monitor

"#,
                                ),
                                false,
                            ))?;
                        }
                        "QUIT" | "Q" => return Ok(()),
                        "RUN" | "C" => {
                            cpucommandtx.send(Command::Run)?;
                            running = 1;
                        }
                        "STOP" => {
                            cpucommandtx.send(Command::Stop)?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Stop(st)) => {
                                    print_state(&st, cpucommandtx, cpucommandresprx, outputtx)?;
                                }
                                Err(e) => outputtx.send((format!("Stop error - {e}\n"), false))?,
                                _ => return Err(eyre!("Invalid return from Stop - {r:?}")),
                            }
                            running = 0;
                        }
                        "B" => {
                            if parts.len() != 2 {
                                outputtx.send((
                                    String::from("Error - Break must include an addr - B <addr>\n"),
                                    true,
                                ))?;
                                continue;
                            }
                            match parse_u16(parts[1]) {
                                Ok(addr) => {
                                    cpucommandtx.send(Command::Break(Location { addr }))?;
                                    let r = cpucommandresprx.recv()?;
                                    match r {
                                        Ok(CommandResponse::Break) => {}
                                        Err(e) => {
                                            outputtx
                                                .send((format!("Break error - {e}\n"), false))?;
                                        }
                                        _ => {
                                            return Err(eyre!("Invalid return from Break - {r:?}"))
                                        }
                                    }
                                }
                                Err(e) => {
                                    outputtx.send((
                                        format!("Error - parse error on addr: {e}\n"),
                                        false,
                                    ))?;
                                }
                            }
                        }
                        "BPL" => {
                            cpucommandtx.send(Command::BreakList)?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::BreakList(bl)) => {
                                    let mut bps = String::new();
                                    writeln!(bps, "Breakpoints:")?;
                                    for (pos, l) in bl.iter().enumerate() {
                                        writeln!(bps, "{pos} - ${:04X}", l.addr)?;
                                    }
                                    outputtx.send((bps, false))?;
                                }
                                Err(e) => {
                                    outputtx.send((format!("BreakList error - {e}\n"), false))?;
                                }
                                _ => return Err(eyre!("Invalid return from BreakList - {r:?}")),
                            }
                        }
                        "DB" => {
                            if parts.len() != 2 {
                                outputtx.send((
                                    String::from(
                                        "Error - Delete Breakpoint must include an num - DB <num>\n",
                                    ),
                                    true,
                                ))?;
                                continue;
                            }
                            match parts[1].parse::<usize>() {
                                Ok(num) => {
                                    cpucommandtx.send(Command::DeleteBreakpoint(num))?;
                                    let r = cpucommandresprx.recv()?;
                                    match r {
                                        Ok(CommandResponse::DeleteBreakpoint) => {}
                                        Err(e) => outputtx.send((
                                            format!("Delete Breakpoint error - {e}\n"),
                                            false,
                                        ))?,
                                        _ => {
                                            return Err(eyre!(
                                                "Invalid return from Delete Breakpoint - {r:?}"
                                            ))
                                        }
                                    }
                                }
                                Err(e) => {
                                    outputtx.send((
                                        format!("Error - parse error on num: {e}\n"),
                                        false,
                                    ))?;
                                }
                            }
                        }
                        "S" => {
                            cpucommandtx.send(Command::Step)?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Step(st)) => {
                                    print_state(&st, cpucommandtx, cpucommandresprx, outputtx)?;
                                }
                                Err(e) => outputtx.send((format!("Step error - {e}\n"), false))?,
                                _ => return Err(eyre!("Invalid return from Step - {r:?}")),
                            }
                        }
                        "T" => {
                            cpucommandtx.send(Command::Tick)?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Tick(st)) => {
                                    print_state(&st, cpucommandtx, cpucommandresprx, outputtx)?;
                                }
                                Err(e) => outputtx.send((format!("Tick error - {e}\n"), false))?,
                                _ => return Err(eyre!("Invalid return from Tick - {r:?}")),
                            }
                        }
                        "R" => {
                            if parts.len() != 2 {
                                outputtx.send((
                                    String::from("Error - Read must include an addr - R <addr>\n"),
                                    true,
                                ))?;
                                continue;
                            }
                            match parse_u16(parts[1]) {
                                Ok(addr) => {
                                    cpucommandtx.send(Command::Read(Location { addr }))?;
                                    let r = cpucommandresprx.recv()?;
                                    match r {
                                        Ok(CommandResponse::Read(r)) => {
                                            outputtx.send((
                                                format!("{addr:04X}  {:02X}\n", r.val),
                                                false,
                                            ))?;
                                        }
                                        Err(e) => {
                                            outputtx
                                                .send((format!("Read error - {e}\n"), false))?;
                                        }
                                        _ => return Err(eyre!("Invalid return from Read - {r:?}")),
                                    }
                                }
                                Err(e) => {
                                    outputtx.send((
                                        format!("Error - parse error on addr: {e}"),
                                        false,
                                    ))?;
                                }
                            }
                        }
                        "RR" => {
                            if parts.len() != 3 {
                                outputtx.send((String::from("Error - Read Range must include an addr and len - RR <addr> <len>\n"), true))?;
                                continue;
                            }
                            let addr = match parse_u16(parts[1]) {
                                Ok(addr) => addr,
                                Err(e) => {
                                    outputtx.send((
                                        format!("Error - parse error on addr: {e}\n"),
                                        true,
                                    ))?;
                                    continue;
                                }
                            };
                            let len = match parse_u16(parts[2]) {
                                Ok(len) => len,
                                Err(e) => {
                                    outputtx.send((
                                        format!("Error - parse error on len: {e}\n"),
                                        true,
                                    ))?;
                                    continue;
                                }
                            };
                            cpucommandtx.send(Command::ReadRange(LocationRange {
                                addr,
                                len: Some(len),
                            }))?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::ReadRange(l)) => {
                                    // Memory has a good Display impl we can use.
                                    let mut r = [0; MAX_SIZE];
                                    for (pos, v) in l.iter().enumerate() {
                                        r[addr as usize + pos] = v.val;
                                    }
                                    outputtx.send((format!("{}", &r as &dyn Memory), false))?;
                                }
                                Err(e) => {
                                    outputtx.send((format!("Read Range error - {e}\n"), false))?;
                                }
                                _ => return Err(eyre!("Invalid return from Read Range - {r:?}")),
                            }
                        }
                        "W" => {
                            if parts.len() != 3 {
                                outputtx.send((String::from(
                                    "Error - Write must include an addr and value - W <addr> <val>\n"
                                ), true))?;
                                continue;
                            }
                            let addr = match parse_u16(parts[1]) {
                                Ok(addr) => addr,
                                Err(e) => {
                                    outputtx.send((
                                        format!("Error - parse error on addr: {e}\n"),
                                        true,
                                    ))?;
                                    continue;
                                }
                            };
                            let val = match parse_u8(parts[2]) {
                                Ok(val) => val,
                                Err(e) => {
                                    outputtx.send((
                                        format!("Error - parse error on val: {e}\n"),
                                        true,
                                    ))?;
                                    continue;
                                }
                            };

                            cpucommandtx.send(Command::Write(Location { addr }, Val { val }))?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Write) => {}
                                Err(e) => outputtx.send((format!("Write error - {e}\n"), false))?,
                                _ => return Err(eyre!("Invalid return from Write - {r:?}")),
                            }
                        }
                        "WR" => {
                            if parts.len() != 4 {
                                outputtx.send((String::from("Error - Write Range must include an addr len and val - WR <addr> <len> <val>\n"), true))?;
                                continue;
                            }
                            let addr = match parse_u16(parts[1]) {
                                Ok(addr) => addr,
                                Err(e) => {
                                    outputtx.send((
                                        format!("Error - parse error on addr: {e}\n"),
                                        true,
                                    ))?;
                                    continue;
                                }
                            };
                            let len = match parse_u16(parts[2]) {
                                Ok(len) => len,
                                Err(e) => {
                                    outputtx.send((
                                        format!("Error - parse error on len: {e}\n"),
                                        true,
                                    ))?;
                                    continue;
                                }
                            };
                            let val = match parse_u8(parts[3]) {
                                Ok(val) => val,
                                Err(e) => {
                                    outputtx.send((
                                        format!("Error - parse error on val: {e}\n"),
                                        true,
                                    ))?;
                                    continue;
                                }
                            };
                            cpucommandtx.send(Command::WriteRange(
                                LocationRange {
                                    addr,
                                    len: Some(len),
                                },
                                Val { val },
                            ))?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::WriteRange) => {}
                                Err(e) => {
                                    outputtx.send((format!("Write Range error - {e}\n"), false))?;
                                }
                                _ => return Err(eyre!("Invalid return from Write Range - {r:?}")),
                            }
                        }
                        "CPU" => {
                            cpucommandtx.send(Command::Cpu)?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Cpu(st)) => print_state(
                                    &Stop {
                                        state: st,
                                        reason: StopReason::None,
                                    },
                                    cpucommandtx,
                                    cpucommandresprx,
                                    outputtx,
                                )?,
                                Err(e) => outputtx.send((format!("Cpu error - {e}\n"), false))?,
                                _ => return Err(eyre!("Invalid return from Cpu - {r:?}")),
                            }
                        }
                        "RAM" => {
                            cpucommandtx.send(Command::Ram)?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Ram(r)) => {
                                    outputtx.send((format!("{}", &r as &dyn Memory), false))?;
                                }
                                Err(e) => outputtx.send((format!("Ram error - {e}\n"), false))?,
                                _ => return Err(eyre!("Invalid return from Ram - {r:?}")),
                            }
                        }
                        "D" => {
                            if parts.len() != 2 {
                                outputtx.send((
                                    String::from(
                                        "Error - Disassemble must include an addr - D <addr>\n",
                                    ),
                                    true,
                                ))?;
                                continue;
                            }
                            match parse_u16(parts[1]) {
                                Ok(addr) => {
                                    cpucommandtx.send(Command::Disassemble(Location { addr }))?;
                                    let r = cpucommandresprx.recv()?;
                                    match r {
                                        Ok(CommandResponse::Disassemble(d)) => {
                                            outputtx.send((format!("{d}\n"), false))?;
                                        }
                                        Err(e) => outputtx
                                            .send((format!("Disassemble error - {e}\n"), false))?,
                                        _ => {
                                            return Err(eyre!(
                                                "Invalid return from Disassemble - {r:?}"
                                            ))
                                        }
                                    }
                                }
                                Err(e) => {
                                    outputtx.send((
                                        format!("Error - parse error on addr: {e}\n"),
                                        false,
                                    ))?;
                                }
                            }
                        }
                        "DR" => {
                            if parts.len() != 3 {
                                outputtx.send((String::from("Error - Disassemble Range must include an addr and len - DR <addr> <len>\n"), true))?;
                                continue;
                            }
                            let addr = match parse_u16(parts[1]) {
                                Ok(addr) => addr,
                                Err(e) => {
                                    outputtx.send((
                                        format!("Error - parse error on addr: {e}\n"),
                                        true,
                                    ))?;
                                    continue;
                                }
                            };
                            let len = match parse_u16(parts[2]) {
                                Ok(len) => len,
                                Err(e) => {
                                    outputtx.send((
                                        format!("Error - parse error on len: {e}\n"),
                                        true,
                                    ))?;
                                    continue;
                                }
                            };
                            cpucommandtx.send(Command::DisassembleRange(LocationRange {
                                addr,
                                len: Some(len),
                            }))?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::DisassembleRange(l)) => {
                                    for d in l {
                                        outputtx.send((format!("{d}\n"), false))?;
                                    }
                                }
                                Err(e) => outputtx
                                    .send((format!("Dissasemble Range error - {e}\n"), false))?,
                                _ => {
                                    return Err(eyre!(
                                        "Invalid return from Disassemble Range - {r:?}"
                                    ))
                                }
                            }
                        }
                        "WP" => {
                            if parts.len() != 2 {
                                outputtx.send((
                                    String::from(
                                        "Error - Watchpoint must include an addr - WP <addr>\n",
                                    ),
                                    true,
                                ))?;
                                continue;
                            }
                            match parse_u16(parts[1]) {
                                Ok(addr) => {
                                    cpucommandtx.send(Command::Watch(Location { addr }))?;
                                    let r = cpucommandresprx.recv()?;
                                    match r {
                                        Ok(CommandResponse::Watch) => {}
                                        Err(e) => outputtx
                                            .send((format!("Watch error - {e}\n"), false))?,
                                        _ => {
                                            return Err(eyre!("Invalid return from Watch - {r:?}"))
                                        }
                                    }
                                }
                                Err(e) => {
                                    outputtx.send((
                                        format!("Error - parse error on addr: {e}\n"),
                                        false,
                                    ))?;
                                }
                            }
                        }
                        "WPL" => {
                            cpucommandtx.send(Command::WatchList)?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::WatchList(wl)) => {
                                    let mut wps = String::new();
                                    writeln!(wps, "Watchpoints:")?;
                                    for (pos, l) in wl.iter().enumerate() {
                                        writeln!(wps, "{pos} - ${:04X}", l.addr)?;
                                    }
                                    outputtx.send((wps, false))?;
                                }
                                Err(e) => {
                                    outputtx.send((format!("WatchList error - {e}\n"), false))?;
                                }
                                _ => return Err(eyre!("Invalid return from WatchList - {r:?}")),
                            }
                        }
                        "DW" => {
                            if parts.len() != 2 {
                                outputtx.send((String::from("Error - Delete Watchpoint must include an num - DW <num>\n"), true))?;
                                continue;
                            }
                            match parts[1].parse::<usize>() {
                                Ok(num) => {
                                    cpucommandtx.send(Command::DeleteWatchpoint(num))?;
                                    let r = cpucommandresprx.recv()?;
                                    match r {
                                        Ok(CommandResponse::DeleteWatchpoint) => {}
                                        Err(e) => outputtx.send((
                                            format!("Delete Watchpoint error - {e}\n"),
                                            false,
                                        ))?,
                                        _ => {
                                            return Err(eyre!(
                                                "Invalid return from Delete Watchpoint - {r:?}"
                                            ))
                                        }
                                    }
                                }
                                Err(e) => {
                                    println!("Error - parse error on num: {e}");
                                }
                            }
                        }
                        "L" => {
                            if parts.len() < 2 || parts.len() > 4 {
                                outputtx.send((String::from("Error - Load must include a filename and optionally load location with optional start - L <path to file> [location [start]]\n"), true))?;
                                continue;
                            }
                            let file = String::from(parts[1]);
                            let loc = if parts.len() == 3 {
                                let addr = match parse_u16(parts[2]) {
                                    Ok(addr) => addr,
                                    Err(e) => {
                                        outputtx.send((
                                            format!("Error - parse error on location: {e}\n"),
                                            true,
                                        ))?;
                                        continue;
                                    }
                                };
                                Some(Location { addr })
                            } else {
                                None
                            };
                            let start = if parts.len() == 4 {
                                let addr = match parse_u16(parts[3]) {
                                    Ok(addr) => addr,
                                    Err(e) => {
                                        outputtx.send((
                                            format!("Error - parse error on start: {e}\n"),
                                            true,
                                        ))?;
                                        continue;
                                    }
                                };
                                Some(PC { addr })
                            } else {
                                None
                            };
                            cpucommandtx.send(Command::Load(file, loc, start))?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Load(st)) => print_state(
                                    &Stop {
                                        state: st,
                                        reason: StopReason::None,
                                    },
                                    cpucommandtx,
                                    cpucommandresprx,
                                    outputtx,
                                )?,
                                Err(e) => outputtx.send((format!("Load error - {e}\n"), false))?,
                                _ => return Err(eyre!("Invalid return from Load - {r:?}")),
                            }
                        }
                        "BIN" => {
                            if parts.len() != 2 {
                                outputtx.send((String::from("Error - Dump must include a filename - BIN <path to file>\n"), true))?;
                                continue;
                            }
                            let file = String::from(parts[1]);
                            cpucommandtx.send(Command::Dump(file))?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Dump) => {}
                                Err(e) => outputtx.send((format!("Dump error - {e}\n"), false))?,
                                _ => return Err(eyre!("Invalid return from Dump - {r:?}")),
                            }
                        }
                        "PC" => {
                            if parts.len() != 2 {
                                outputtx.send((
                                    String::from("Error - PC must include an addr - PC <addr>\n"),
                                    true,
                                ))?;
                                continue;
                            }
                            let addr = match parse_u16(parts[1]) {
                                Ok(addr) => addr,
                                Err(e) => {
                                    outputtx.send((
                                        format!("Error - parse error on len: {e}\n"),
                                        true,
                                    ))?;
                                    continue;
                                }
                            };
                            cpucommandtx.send(Command::PC(Location { addr }))?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::PC(st)) => print_state(
                                    &Stop {
                                        state: st,
                                        reason: StopReason::None,
                                    },
                                    cpucommandtx,
                                    cpucommandresprx,
                                    outputtx,
                                )?,
                                Err(e) => outputtx.send((format!("PC error - {e}\n"), false))?,
                                _ => return Err(eyre!("Invalid return from PC - {r:?}")),
                            }
                        }
                        "RESET" => {
                            cpucommandtx.send(Command::Reset)?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Reset(st)) => print_state(
                                    &Stop {
                                        state: st,
                                        reason: StopReason::None,
                                    },
                                    cpucommandtx,
                                    cpucommandresprx,
                                    outputtx,
                                )?,
                                Err(e) => outputtx.send((format!("Reset error - {e}\n"), false))?,
                                _ => return Err(eyre!("Invalid return from Reset - {r:?}")),
                            }
                            running = 0;
                        }
                        _ => {
                            outputtx.send((
                                format!("ERROR: Invalid command - {}\n", parts[0]),
                                false,
                            ))?;
                        }
                    }
                }
                outputtx.send((String::new(), true))?;
            }
            Err(TryRecvError::Empty) => {}
            Err(TryRecvError::Disconnected) => return Err(eyre!("stdin died?")),
        }

        // Once it's running we have to juggle stdin vs possibly brk/watch happening.
        if running > 0 {
            match cpucommandresprx.try_recv() {
                Ok(s) => match s {
                    Ok(ret) => match ret {
                        CommandResponse::Stop(st) => {
                            if st.reason == StopReason::Run {
                                // First Run response emit a blank line to line up
                                // vs the prompt.
                                if running < 2 {
                                    running += 1;
                                    outputtx.send((String::from("\n"), false))?;
                                }
                                print_state(&st, cpucommandtx, cpucommandresprx, outputtx)?;
                            } else {
                                running = 0;
                                outputtx.send((String::new(), false))?;
                                print_state(&st, cpucommandtx, cpucommandresprx, outputtx)?;
                                outputtx.send((String::new(), true))?;
                            }
                        }
                        _ => return Err(eyre!("invalid response from run: {ret:?}")),
                    },
                    Err(e) => outputtx.send((format!("Error from Run - {e}"), true))?,
                },
                Err(TryRecvError::Empty) => {}
                Err(TryRecvError::Disconnected) => return Err(eyre!("Sender channel died")),
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

fn print_state(
    st: &Stop,
    tx: &Sender<Command>,
    rx: &Receiver<Result<CommandResponse>>,
    outputtx: &Sender<(String, bool)>,
) -> Result<()> {
    // Different from just using Display for CPUState since we don't want the
    // memory dump and slightly differeing order.
    if let StopReason::Break(addr) = &st.reason {
        outputtx.send((format!("\nBreakpoint at {:04X}\n", addr.addr), false))?;
    }
    if let StopReason::Watch(pc, addr) = &st.reason {
        outputtx.send((
            format!(
                "\nWatchpoint triggered for addr {:04X} at {:04X} (next PC at {:04X})\n",
                addr.addr, pc.addr, st.state.pc,
            ),
            false,
        ))?;
        tx.send(Command::Disassemble(Location { addr: pc.addr }))?;
        let r = rx.recv()?;
        match r {
            Ok(CommandResponse::Disassemble(d)) => outputtx.send((format!("{d}\n"), false))?,
            Err(e) => outputtx.send((format!("Disassemble error - {e}\n"), false))?,
            _ => return Err(eyre!("Invalid return from Disassemble - {r:?}")),
        }
    }
    outputtx.send((format!(
            "{:<33}A: {:02X} X: {:02X} Y: {:02X} S: {:02X} P: {} op_val: {:02X} op_addr: {:04X} op_tick: {} cycles: {}\n",
            st.state.dis, st.state.a, st.state.x, st.state.y, st.state.s, st.state.p, st.state.op_val, st.state.op_addr, st.state.op_tick, st.state.clocks), false))?;
    Ok(())
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

/// `cpu_loop` is the runner which accepts commands over the channel, instruments
/// the CPU and then returns the response over the other channel.
///
/// NOTE: For most commands this is a command/response relationship except for
///       RUN. Once that starts this will emit a `StopResponse` as either
///       RUN/BREAK/WATCH with appropriate state for every instruction processed.
///       This will continue until a BREAK or WATCH is hit or a new STOP command
///       comes down the command channel.
///
/// # Errors
/// Any premature close of the channels will result in an error.
#[allow(clippy::similar_names, clippy::too_many_lines)]
pub fn cpu_loop(
    ty: Type,
    cpucommandrx: &Receiver<Command>,
    cpucommandresptx: &Sender<Result<CommandResponse>>,
) -> Result<()> {
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
            cpucommandresptx.send(Err(eyre!(
                "invalid size {} + {} exceeds {MAX_SIZE}",
                range.addr,
                len
            )))?;
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
            let oldpc = cpu.pc();
            let r = step(cpu);
            if let Err(e) = r {
                let e = eyre!("step error: {e}");
                cpucommandresptx.send(Err(e))?;
                return Err(eyre!("step error"));
            }

            cpu.debug();
            (d.state.borrow_mut().dis, _) = cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
            let mut reason = StopReason::Run;
            for b in &breakpoints {
                if b.addr == cpu.pc() {
                    reason = StopReason::Break(Location { addr: b.addr });
                }
            }
            if !watchpoints.is_empty() {
                let cr = cpu.ram();
                let cr = cr.borrow();
                for w in &watchpoints {
                    if cr.read(w.addr) != ram[usize::from(w.addr)] {
                        reason = StopReason::Watch(PC { addr: oldpc }, Location { addr: w.addr });
                        break;
                    }
                }
            }
            if reason != StopReason::Run {
                is_running = false;
            }
            let st = Stop {
                state: d.state.borrow().clone(),
                reason,
            };
            cpucommandresptx.send(Ok(CommandResponse::Stop(st)))?;
        }

        // Peek at the channel. If it's an error but empty and we're
        // just running then we loop around and step again. A real error
        // aborts for now since we can't recover from a closed channel.
        // This way in running we match per instruction but also check
        // after each one for a Stop.
        let c = cpucommandrx.try_recv();
        if let Err(e) = c {
            if e == TryRecvError::Disconnected {
                return Err(eyre!("disconnected from rx"));
            }
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
                        cpucommandresptx
                            .send(Err(eyre!("only Stop and Reset allowed in Run state")))?;
                        continue;
                    }
                };
            }
            c
        } else {
            cpucommandrx.recv()?
        };

        match c {
            Command::Run => {
                if !is_init {
                    is_init = true;
                    cpu.power_on()?;
                }
                is_running = true;
                cpu.debug();
                (d.state.borrow_mut().dis, _) =
                    cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
                let st = Stop {
                    state: d.state.borrow().clone(),
                    reason: StopReason::Run,
                };
                cpucommandresptx.send(Ok(CommandResponse::Stop(st)))?;
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
                cpucommandresptx.send(Ok(CommandResponse::Stop(st)))?;
            }
            Command::Break(addr) => {
                breakpoints.push(addr);
                cpucommandresptx.send(Ok(CommandResponse::Break))?;
            }
            Command::BreakList => {
                cpucommandresptx.send(Ok(CommandResponse::BreakList(breakpoints.clone())))?;
            }
            Command::DeleteBreakpoint(num) => {
                if num >= breakpoints.len() {
                    if breakpoints.is_empty() {
                        cpucommandresptx.send(Err(eyre!("no breakpoints to delete")))?;
                        continue;
                    }
                    cpucommandresptx.send(Err(eyre!(
                        "breakpoint index {num} out of range 0-{}",
                        breakpoints.len() - 1
                    )))?;
                    continue;
                }
                breakpoints.swap_remove(num);
                cpucommandresptx.send(Ok(CommandResponse::DeleteBreakpoint))?;
            }
            Command::Step => {
                if !is_init {
                    is_init = true;
                    cpu.power_on()?;
                }
                let mut ram = [0; MAX_SIZE];
                if !watchpoints.is_empty() {
                    // If we have watchpoints get a memory snapshot.
                    cpu.ram().borrow().ram(&mut ram);
                }
                let oldpc = cpu.pc();
                let r = step(cpu);
                if let Err(e) = r {
                    cpucommandresptx.send(Err(eyre!("step error: {e}")))?;
                    continue;
                }
                (d.state.borrow_mut().dis, _) =
                    cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
                let mut reason = StopReason::Step;
                for b in &breakpoints {
                    if b.addr == cpu.pc() {
                        reason = StopReason::Break(Location { addr: b.addr });
                    }
                }
                if !watchpoints.is_empty() {
                    let cr = cpu.ram();
                    let cr = cr.borrow();
                    for w in &watchpoints {
                        if cr.read(w.addr) != ram[usize::from(w.addr)] {
                            reason =
                                StopReason::Watch(PC { addr: oldpc }, Location { addr: w.addr });
                            break;
                        }
                    }
                }
                let st = Stop {
                    state: d.state.borrow().clone(),
                    reason,
                };
                cpucommandresptx.send(Ok(CommandResponse::Step(st)))?;
            }
            Command::Tick => {
                if !is_init {
                    is_init = true;
                    cpu.power_on()?;
                }
                let mut ram = [0; MAX_SIZE];
                if !watchpoints.is_empty() {
                    // If we have watchpoints get a memory snapshot.
                    cpu.ram().borrow().ram(&mut ram);
                }
                let oldpc = cpu.pc();
                let r = cpu.tick();
                if let Err(e) = r {
                    let e = eyre!("tick error: {e}");
                    cpucommandresptx.send(Err(e))?;
                    return Err(eyre!("tick error"));
                }
                let r = cpu.tick_done();
                if let Err(e) = r {
                    let e = eyre!("tick done error: {e}");
                    cpucommandresptx.send(Err(e))?;
                    return Err(eyre!("tick done error"));
                }
                cpu.debug();
                (d.state.borrow_mut().dis, _) =
                    cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
                let mut reason = StopReason::Tick;
                for b in &breakpoints {
                    if b.addr == cpu.pc() {
                        reason = StopReason::Break(Location { addr: b.addr });
                    }
                }
                if !watchpoints.is_empty() {
                    let cr = cpu.ram();
                    let cr = cr.borrow();
                    for w in &watchpoints {
                        if cr.read(w.addr) != ram[usize::from(w.addr)] {
                            reason =
                                StopReason::Watch(PC { addr: oldpc }, Location { addr: w.addr });
                            break;
                        }
                    }
                }
                let st = Stop {
                    state: d.state.borrow().clone(),
                    reason,
                };
                cpucommandresptx.send(Ok(CommandResponse::Tick(st)))?;
            }

            Command::Read(addr) => {
                let val = cpu.ram().borrow().read(addr.addr);
                cpucommandresptx.send(Ok(CommandResponse::Read(Val { val })))?;
            }
            Command::ReadRange(range) => {
                if let Ok(len) = valid_range(&range) {
                    let mut r = Vec::new();
                    for i in 0..len {
                        let val = cpu.ram().borrow().read(range.addr + i);
                        r.push(Val { val });
                    }
                    cpucommandresptx.send(Ok(CommandResponse::ReadRange(r)))?;
                }
            }
            Command::Write(addr, val) => {
                cpu.ram().borrow_mut().write(addr.addr, val.val);
                cpucommandresptx.send(Ok(CommandResponse::Write))?;
            }
            Command::WriteRange(range, val) => {
                if let Ok(len) = valid_range(&range) {
                    for i in 0..len {
                        cpu.ram().borrow_mut().write(range.addr + i, val.val);
                    }
                    cpucommandresptx.send(Ok(CommandResponse::WriteRange))?;
                }
            }
            Command::Cpu => {
                cpu.debug();
                (d.state.borrow_mut().dis, _) =
                    cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
                cpucommandresptx.send(Ok(CommandResponse::Cpu(d.state.borrow().clone())))?;
            }
            Command::Ram => {
                let mut r = [0u8; MAX_SIZE];
                cpu.ram().borrow().ram(&mut r);
                cpucommandresptx.send(Ok(CommandResponse::Ram(r)))?;
            }
            Command::Disassemble(addr) => {
                let (s, _) = cpu.disassemble(addr.addr, cpu.ram().borrow().as_ref());
                cpucommandresptx.send(Ok(CommandResponse::Disassemble(s)))?;
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
                    cpucommandresptx.send(Ok(CommandResponse::DisassembleRange(r)))?;
                }
            }
            Command::Watch(addr) => {
                watchpoints.push(addr);
                cpucommandresptx.send(Ok(CommandResponse::Watch))?;
            }
            Command::WatchList => {
                cpucommandresptx.send(Ok(CommandResponse::WatchList(watchpoints.clone())))?;
            }
            Command::DeleteWatchpoint(num) => {
                if num >= watchpoints.len() {
                    cpucommandresptx.send(Err(eyre!(
                        "watchpoint index {num} out of range 0-{}",
                        watchpoints.len() - 1
                    )))?;
                    continue;
                }
                watchpoints.swap_remove(num);
                cpucommandresptx.send(Ok(CommandResponse::DeleteWatchpoint))?;
            }
            Command::Load(file, loc, start) => {
                let loc = if let Some(loc) = loc {
                    loc
                } else {
                    Location { addr: 0x000 }
                };

                let path = Path::new(&file);
                match read(path) {
                    Ok(b) => {
                        if b.len() > MAX_SIZE || (usize::from(loc.addr) + b.len()) > MAX_SIZE {
                            cpucommandresptx.send(Err(eyre!(
                                "file too large {} at offset {}",
                                b.len(),
                                loc.addr
                            )))?;
                            continue;
                        }
                        for (addr, b) in b.iter().enumerate() {
                            let a = u16::try_from(addr)?;
                            cpu.ram().borrow_mut().write(loc.addr + a, *b);
                        }
                        if !is_init {
                            is_init = true;
                            cpu.power_on()?;
                        }
                        loop {
                            match cpu.reset() {
                                Ok(OpState::Done) => {
                                    if let Some(start) = start {
                                        cpu.pc_mut(start.addr);
                                    }
                                    cpu.debug();
                                    (d.state.borrow_mut().dis, _) =
                                        cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
                                    cpucommandresptx.send(Ok(CommandResponse::Load(
                                        d.state.borrow().clone(),
                                    )))?;
                                    break;
                                }
                                Ok(OpState::Processing) => continue,
                                Err(e) => {
                                    cpucommandresptx.send(Err(eyre!("reset error: {e}")))?;
                                    break;
                                }
                            }
                        }
                    }
                    Err(e) => {
                        cpucommandresptx.send(Err(eyre!("can't read file: {e}")))?;
                        continue;
                    }
                }
            }
            Command::Dump(file) => {
                let mut r = [0; MAX_SIZE];
                cpu.ram().borrow().ram(&mut r);
                match write(Path::new(&file), r) {
                    Ok(_) => {
                        cpucommandresptx.send(Ok(CommandResponse::Dump))?;
                    }
                    Err(e) => {
                        cpucommandresptx.send(Err(eyre!("can't write file: {e}")))?;
                    }
                }
            }
            Command::PC(addr) => {
                cpu.pc_mut(addr.addr);
                cpu.debug();
                (d.state.borrow_mut().dis, _) =
                    cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
                cpucommandresptx.send(Ok(CommandResponse::PC(d.state.borrow().clone())))?;
            }
            Command::Reset => {
                if !is_init {
                    is_init = true;
                    cpu.power_on()?;
                    // Power on does a reset so we don't have to do it again below.
                    cpu.debug();
                    (d.state.borrow_mut().dis, _) =
                        cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
                    cpucommandresptx.send(Ok(CommandResponse::Reset(d.state.borrow().clone())))?;
                    continue;
                }
                loop {
                    match cpu.reset() {
                        Ok(OpState::Done) => {
                            cpu.debug();
                            (d.state.borrow_mut().dis, _) =
                                cpu.disassemble(cpu.pc(), cpu.ram().borrow().as_ref());
                            cpucommandresptx
                                .send(Ok(CommandResponse::Reset(d.state.borrow().clone())))?;
                            break;
                        }
                        Ok(OpState::Processing) => continue,
                        Err(e) => {
                            cpucommandresptx.send(Err(eyre!("reset error: {e}")))?;
                            break;
                        }
                    }
                }
            }
        }
    }
}
