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

mod commands;
use commands::{
    Command, CommandResponse, Location, LocationRange, StepN, StepNReason, Stop, StopReason, Val,
    PC,
};

#[cfg(test)]
mod tests;

/// Items the output channel can receive and should render as it sees appropriate
#[derive(Debug)]
pub enum Output {
    /// Print an optional leading string, then a newline and a new prompt.
    Prompt(Option<String>),
    /// An error from a command which prints along with a newline and a new prompt.
    Error(String),
    /// The CPU State including why we received it (NOTE: the ram section isn't filled in).
    /// An optional leading string (which will have newline appended) may also be supplied.
    /// No prompt should be printed here as this is often used for repeating output
    /// such as during RUN.
    CPU(Box<Stop>, Option<String>),
    /// When running N steps returns a vector of states to emit
    StepN(Vec<CPUState>),
    /// A RAM dump. No prompt should be printed after this as it's often used
    /// in non interactive areas.
    RAM(Box<[u8; MAX_SIZE]>),
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
///
/// # Panics
/// Additionally any invalid state transitions will result in an panic.
#[allow(clippy::too_many_lines)]
pub fn input_loop(
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
    inputtx: &Receiver<String>,
    outputtx: &Sender<Output>,
    preload: bool,
) -> Result<()> {
    // Only print if we didn't preload something since that will be
    // already in stdin and processed below and print the prompt.
    if !preload {
        outputtx.send(Output::Prompt(None))?;
    }

    let mut running = 0;
    loop {
        let mut pre = None;
        match inputtx.try_recv() {
            Ok(line) => {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() > 4 {
                    outputtx.send(Output::Error(format!("ERROR: Invalid command - {line}")))?;
                    continue;
                }
                if !parts.is_empty() {
                    let cmd = parts[0].to_uppercase();

                    // If we're running prevent commands as the other thread is filling the response
                    // channel which means everything below which assumes synchronous command+response
                    // will likely fail and panic. Instead just reject outright.
                    if running > 0 {
                        match cmd.as_str() {
                            "H" | "HELP" | "QUIT" | "Q" | "RUN" | "C" | "STOP" => {}
                            _ => {
                                outputtx.send(Output::Error(format!(
                                    "ERROR: Invalid command during RUN - {line}"
                                )))?;
                                continue;
                            }
                        }
                    }

                    match cmd.as_str() {
                        "H" | "HELP" => {
                            pre = Some(
                                r#"Usage:
HELP | H - This usage information
RUN | C [true] - run continually until either a breakpoint/watchpoint is hit or a STOP is sent.
                 If the bool is set to true then a RAM snapshot will be taken on each instruction.
STOP - cause the CPU to stop at the current instrunction from continual running
BP <addr> - Break when PC is equal to the addr
BPL - List all breakpoints
DB <num> - Delete the given breakpoint
S [true] - Step instruction (2-8 clock cycles)
           If the bool is set to true then a RAM snapshot (expensive) will be taken on each instruction.
STEPN <count> <report> <true|X> - Step count instructions and return <report> length vector
                                  of the last ones executed. If the last param is true
                                  a RAM snapshot (expensive) will be taken on each instruction.
T [true] - Tick instruction (one clock cycle)
           If the bool is set to true then a RAM snapshot (expensive) will be taken on each tick.
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
QUIT | Q - Exit the monitor"#
                                    .into(),
                            );
                        }
                        "QUIT" | "Q" => {
                            return Ok(());
                        }
                        "RUN" | "C" => {
                            if parts.len() > 2 {
                                outputtx.send(Output::Error(String::from(
                                    "Error - Run can only optionally be follow by true: RUN [true]",
                                )))?;
                                continue;
                            }
                            let ram = parts.len() == 2 && parts[1] == "TRUE";
                            cpucommandtx.send(Command::Run(ram))?;
                            running = 1;
                        }
                        "STOP" => {
                            cpucommandtx.send(Command::Stop)?;
                        }
                        "B" => {
                            if parts.len() != 2 {
                                outputtx.send(Output::Error(
                                    "Error - Break must include an addr - B <addr>".into(),
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
                                            outputtx.send(Output::Error(format!(
                                                "Break error - {e}"
                                            )))?;
                                            continue;
                                        }
                                        _ => panic!("Invalid return from Break - {r:?}"),
                                    }
                                }
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on addr: {e}"
                                    )))?;
                                    continue;
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
                                    trim_newline(&mut bps);
                                    pre = Some(bps);
                                }
                                Err(e) => {
                                    outputtx
                                        .send(Output::Error(format!("BreakList error - {e}")))?;
                                    continue;
                                }
                                _ => panic!("Invalid return from BreakList - {r:?}"),
                            }
                        }
                        "DB" => {
                            if parts.len() != 2 {
                                outputtx.send(Output::Error(
                                    "Error - Delete Breakpoint must include an num - DB <num>"
                                        .into(),
                                ))?;
                                continue;
                            }
                            match parts[1].parse::<usize>() {
                                Ok(num) => {
                                    cpucommandtx.send(Command::DeleteBreakpoint(num))?;
                                    let r = cpucommandresprx.recv()?;
                                    match r {
                                        Ok(CommandResponse::DeleteBreakpoint) => {}
                                        Err(e) => {
                                            outputtx.send(Output::Error(format!(
                                                "Delete Breakpoint error - {e}"
                                            )))?;
                                            continue;
                                        }
                                        _ => {
                                            panic!("Invalid return from Delete Breakpoint - {r:?}")
                                        }
                                    }
                                }
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on num: {e}"
                                    )))?;
                                    continue;
                                }
                            }
                        }
                        "S" => {
                            if parts.len() > 2 {
                                outputtx.send(Output::Error(
                                    "Error - Step can only optionally be follow by true: S [true]"
                                        .into(),
                                ))?;
                                continue;
                            }
                            let ram = parts.len() == 2 && parts[1].to_uppercase() == "TRUE";
                            cpucommandtx.send(Command::Step(ram))?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Step(st)) => {
                                    let mut pre = None;
                                    if let StopReason::Break(addr) = &st.reason {
                                        pre = Some(format!("\nBreakpoint at {:04X}", addr.addr));
                                    }
                                    if let StopReason::Watch(pc, addr, wp) = &st.reason {
                                        pre = Some(format!("\nWatchpoint triggered for addr {:04X} at {:04X} (next PC at {:04X})\n{wp}", addr.addr, pc.addr, st.state.pc));
                                    }
                                    outputtx.send(Output::CPU(st, pre))?;
                                }
                                Err(e) => {
                                    outputtx.send(Output::Error(format!("Step error - {e}")))?;
                                    continue;
                                }
                                _ => panic!("Invalid return from Step - {r:?}"),
                            }
                        }
                        "STEPN" => {
                            if parts.len() != 4 {
                                outputtx.send(Output::Error("Error - STEPN must be followed by a count, repeat and RAM snapshot indicator".into()))?;
                                continue;
                            }
                            let reps = match parse_u16(parts[1]) {
                                Ok(reps) => usize::from(reps),
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on count: {e}"
                                    )))?;
                                    continue;
                                }
                            };
                            let capture = match parse_u16(parts[2]) {
                                Ok(capture) => usize::from(capture),
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on repeat: {e}"
                                    )))?;
                                    continue;
                                }
                            };
                            let ram = parts[3].to_uppercase() == "TRUE";
                            cpucommandtx.send(Command::StepN(StepN {
                                reps,
                                capture: vec![CPUState::default(); capture],
                                ram,
                            }))?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::StepN(st)) => match st {
                                    StepNReason::Stop(stop) => {
                                        let mut pre = None;
                                        if let StopReason::Break(addr) = &stop.reason {
                                            pre =
                                                Some(format!("\nBreakpoint at {:04X}", addr.addr));
                                        }
                                        if let StopReason::Watch(pc, addr, wp) = &stop.reason {
                                            pre = Some(format!("\nWatchpoint triggered for addr {:04X} at {:04X} (next PC at {:04X})\n{wp}", addr.addr, pc.addr, stop.state.pc));
                                        }
                                        outputtx.send(Output::CPU(stop, pre))?;
                                    }
                                    StepNReason::StepN(stepn) => {
                                        outputtx.send(Output::StepN(stepn))?;
                                    }
                                },
                                Err(e) => {
                                    outputtx.send(Output::Error(format!("StepN error - {e}")))?;
                                    continue;
                                }
                                _ => panic!("Invalid return from Stepn - {r:?}"),
                            }
                        }
                        "T" => {
                            if parts.len() > 2 {
                                outputtx.send(Output::Error(String::from(
                                    "Error - Tick can only optionally be follow by true: T [true]",
                                )))?;
                                continue;
                            }
                            let ram = parts.len() == 2 && parts[1].to_uppercase() == "TRUE";
                            cpucommandtx.send(Command::Tick(ram))?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Tick(st)) => {
                                    let mut pre = None;
                                    if let StopReason::Break(addr) = &st.reason {
                                        pre = Some(format!("\nBreakpoint at {:04X}", addr.addr));
                                    }
                                    if let StopReason::Watch(pc, addr, wp) = &st.reason {
                                        pre = Some(format!("\nWatchpoint triggered for addr {:04X} at {:04X} (next PC at {:04X})\n{wp}", addr.addr, pc.addr, st.state.pc));
                                    }
                                    outputtx.send(Output::CPU(st, pre))?;
                                }
                                Err(e) => {
                                    outputtx.send(Output::Error(format!("Tick error - {e}")))?;
                                    continue;
                                }
                                _ => panic!("Invalid return from Tick - {r:?}"),
                            }
                        }
                        "R" => {
                            if parts.len() != 2 {
                                outputtx.send(Output::Error(
                                    "Error - Read must include an addr - R <addr>".into(),
                                ))?;
                                continue;
                            }
                            match parse_u16(parts[1]) {
                                Ok(addr) => {
                                    cpucommandtx.send(Command::Read(Location { addr }))?;
                                    let r = cpucommandresprx.recv()?;
                                    match r {
                                        Ok(CommandResponse::Read(r)) => {
                                            pre = Some(format!("{addr:04X}  {:02X}", r.val));
                                        }
                                        Err(e) => {
                                            outputtx.send(Output::Error(format!(
                                                "Read error - {e}\n"
                                            )))?;
                                            continue;
                                        }
                                        _ => panic!("Invalid return from Read - {r:?}"),
                                    }
                                }
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on addr: {e}"
                                    )))?;
                                    continue;
                                }
                            }
                        }
                        "RR" => {
                            if parts.len() != 3 {
                                outputtx.send(Output::Error("Error - Read Range must include an addr and len - RR <addr> <len>".into()))?;
                                continue;
                            }
                            let addr = match parse_u16(parts[1]) {
                                Ok(addr) => addr,
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on addr: {e}"
                                    )))?;
                                    continue;
                                }
                            };
                            let len = match parse_u16(parts[2]) {
                                Ok(len) => len,
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on len: {e}"
                                    )))?;
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
                                    // Just fill in the bytes we received and
                                    // everything will collapse around the nuls.
                                    let mut r = Box::new([0; MAX_SIZE]);
                                    for (pos, v) in l.iter().enumerate() {
                                        r[addr as usize + pos] = v.val;
                                    }
                                    outputtx.send(Output::RAM(r))?;
                                }
                                Err(e) => {
                                    outputtx
                                        .send(Output::Error(format!("Read Range error - {e}")))?;
                                    continue;
                                }
                                _ => panic!("Invalid return from Read Range - {r:?}"),
                            }
                        }
                        "W" => {
                            if parts.len() != 3 {
                                outputtx.send(Output::Error(
                                    "Error - Write must include an addr and value - W <addr> <val>"
                                        .into(),
                                ))?;
                                continue;
                            }
                            let addr = match parse_u16(parts[1]) {
                                Ok(addr) => addr,
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on addr: {e}"
                                    )))?;
                                    continue;
                                }
                            };
                            let val = match parse_u8(parts[2]) {
                                Ok(val) => val,
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on val: {e}"
                                    )))?;
                                    continue;
                                }
                            };

                            cpucommandtx.send(Command::Write(Location { addr }, Val { val }))?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Write) => {}
                                Err(e) => {
                                    outputtx.send(Output::Error(format!("Write error - {e}")))?;
                                    continue;
                                }
                                _ => panic!("Invalid return from Write - {r:?}"),
                            }
                        }
                        "WR" => {
                            if parts.len() != 4 {
                                outputtx.send(Output::Error("Error - Write Range must include an addr len and val - WR <addr> <len> <val>".into()))?;
                                continue;
                            }
                            let addr = match parse_u16(parts[1]) {
                                Ok(addr) => addr,
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on addr: {e}"
                                    )))?;
                                    continue;
                                }
                            };
                            let len = match parse_u16(parts[2]) {
                                Ok(len) => len,
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on len: {e}"
                                    )))?;
                                    continue;
                                }
                            };
                            let val = match parse_u8(parts[3]) {
                                Ok(val) => val,
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on val: {e}"
                                    )))?;
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
                                    outputtx
                                        .send(Output::Error(format!("Write Range error - {e}")))?;
                                    continue;
                                }
                                _ => panic!("Invalid return from Write Range - {r:?}"),
                            }
                        }
                        "CPU" => {
                            cpucommandtx.send(Command::Cpu)?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Cpu(st)) => {
                                    outputtx.send(Output::CPU(
                                        Box::new(Stop {
                                            state: st,
                                            reason: StopReason::None,
                                        }),
                                        None,
                                    ))?;
                                }
                                Err(e) => {
                                    outputtx.send(Output::Error(format!("Cpu error - {e}")))?;
                                    continue;
                                }
                                _ => panic!("Invalid return from Cpu - {r:?}"),
                            }
                        }
                        "RAM" => {
                            cpucommandtx.send(Command::Ram)?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Ram(r)) => {
                                    outputtx.send(Output::RAM(r))?;
                                }
                                Err(e) => {
                                    outputtx.send(Output::Error(format!("Ram error - {e}")))?;
                                    continue;
                                }
                                _ => panic!("Invalid return from Ram - {r:?}"),
                            }
                        }
                        "D" => {
                            if parts.len() != 2 {
                                outputtx.send(Output::Error(
                                    "Error - Disassemble must include an addr - D <addr>".into(),
                                ))?;
                                continue;
                            }
                            match parse_u16(parts[1]) {
                                Ok(addr) => {
                                    cpucommandtx.send(Command::Disassemble(Location { addr }))?;
                                    let r = cpucommandresprx.recv()?;
                                    match r {
                                        Ok(CommandResponse::Disassemble(d)) => {
                                            pre = Some(d);
                                        }
                                        Err(e) => {
                                            outputtx.send(Output::Error(format!(
                                                "Disassemble error - {e}"
                                            )))?;
                                            continue;
                                        }
                                        _ => panic!("Invalid return from Disassemble - {r:?}"),
                                    }
                                }
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on addr: {e}"
                                    )))?;
                                    continue;
                                }
                            }
                        }
                        "DR" => {
                            if parts.len() != 3 {
                                outputtx.send(Output::Error("Error - Disassemble Range must include an addr and len - DR <addr> <len>".into()))?;
                                continue;
                            }
                            let addr = match parse_u16(parts[1]) {
                                Ok(addr) => addr,
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on addr: {e}"
                                    )))?;
                                    continue;
                                }
                            };
                            let len = match parse_u16(parts[2]) {
                                Ok(len) => len,
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on len: {e}\n"
                                    )))?;
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
                                    let mut dr = String::new();
                                    for d in l {
                                        writeln!(dr, "{d}")?;
                                    }
                                    trim_newline(&mut dr);
                                    pre = Some(dr);
                                }
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Dissasemble Range error - {e}"
                                    )))?;
                                    continue;
                                }
                                _ => panic!("Invalid return from Disassemble Range - {r:?}"),
                            }
                        }
                        "WP" => {
                            if parts.len() != 2 {
                                outputtx.send(Output::Error(
                                    "Error - Watchpoint must include an addr - WP <addr>".into(),
                                ))?;
                                continue;
                            }
                            match parse_u16(parts[1]) {
                                Ok(addr) => {
                                    cpucommandtx.send(Command::Watch(Location { addr }))?;
                                    let r = cpucommandresprx.recv()?;
                                    match r {
                                        Ok(CommandResponse::Watch) => {}
                                        Err(e) => {
                                            outputtx.send(Output::Error(format!(
                                                "Watch error - {e}"
                                            )))?;
                                            continue;
                                        }
                                        _ => panic!("Invalid return from Watch - {r:?}"),
                                    }
                                }
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on addr: {e}"
                                    )))?;
                                    continue;
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
                                    trim_newline(&mut wps);
                                    pre = Some(wps);
                                }
                                Err(e) => {
                                    outputtx
                                        .send(Output::Error(format!("WatchList error - {e}\n")))?;
                                    continue;
                                }
                                _ => panic!("Invalid return from WatchList - {r:?}"),
                            }
                        }
                        "DW" => {
                            if parts.len() != 2 {
                                outputtx.send(Output::Error(
                                    "Error - Delete Watchpoint must include an num - DW <num>"
                                        .into(),
                                ))?;
                                continue;
                            }
                            match parts[1].parse::<usize>() {
                                Ok(num) => {
                                    cpucommandtx.send(Command::DeleteWatchpoint(num))?;
                                    let r = cpucommandresprx.recv()?;
                                    match r {
                                        Ok(CommandResponse::DeleteWatchpoint) => {}
                                        Err(e) => {
                                            outputtx.send(Output::Error(format!(
                                                "Delete Watchpoint error - {e}"
                                            )))?;
                                            continue;
                                        }
                                        _ => {
                                            panic!("Invalid return from Delete Watchpoint - {r:?}")
                                        }
                                    }
                                }
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on num: {e}"
                                    )))?;
                                    continue;
                                }
                            }
                        }
                        "L" => {
                            if parts.len() < 2 || parts.len() > 4 {
                                outputtx.send(Output::Error("Error - Load must include a filename and optionally load location with optional start - L <path to file> [location [start]]".into()))?;
                                continue;
                            }
                            let file: String = parts[1].into();
                            let loc = if parts.len() == 3 {
                                let addr = match parse_u16(parts[2]) {
                                    Ok(addr) => addr,
                                    Err(e) => {
                                        outputtx.send(Output::Error(format!(
                                            "Error - parse error on location: {e}"
                                        )))?;
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
                                        outputtx.send(Output::Error(format!(
                                            "Error - parse error on start: {e}"
                                        )))?;
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
                                Ok(CommandResponse::Load(st)) => {
                                    outputtx.send(Output::CPU(
                                        Box::new(Stop {
                                            state: st,
                                            reason: StopReason::None,
                                        }),
                                        None,
                                    ))?;
                                }
                                Err(e) => {
                                    outputtx.send(Output::Error(format!("Load error - {e}")))?;
                                    continue;
                                }
                                _ => panic!("Invalid return from Load - {r:?}"),
                            }
                        }
                        "BIN" => {
                            if parts.len() != 2 {
                                outputtx.send(Output::Error(
                                    "Error - Dump must include a filename - BIN <path to file>"
                                        .into(),
                                ))?;
                                continue;
                            }
                            let file: String = parts[1].into();
                            cpucommandtx.send(Command::Dump(file))?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Dump) => {}
                                Err(e) => {
                                    outputtx.send(Output::Error(format!("Dump error - {e}")))?;
                                    continue;
                                }
                                _ => panic!("Invalid return from Dump - {r:?}"),
                            }
                        }
                        "PC" => {
                            if parts.len() != 2 {
                                outputtx.send(Output::Error(
                                    "Error - PC must include an addr - PC <addr>".into(),
                                ))?;
                                continue;
                            }
                            let addr = match parse_u16(parts[1]) {
                                Ok(addr) => addr,
                                Err(e) => {
                                    outputtx.send(Output::Error(format!(
                                        "Error - parse error on len: {e}"
                                    )))?;
                                    continue;
                                }
                            };
                            cpucommandtx.send(Command::PC(Location { addr }))?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::PC(st)) => {
                                    outputtx.send(Output::CPU(
                                        Box::new(Stop {
                                            state: st,
                                            reason: StopReason::None,
                                        }),
                                        None,
                                    ))?;
                                }
                                Err(e) => {
                                    outputtx.send(Output::Error(format!("PC error - {e}")))?;
                                    continue;
                                }
                                _ => panic!("Invalid return from PC - {r:?}"),
                            }
                        }
                        "RESET" => {
                            cpucommandtx.send(Command::Reset)?;
                            let r = cpucommandresprx.recv()?;
                            match r {
                                Ok(CommandResponse::Reset(st)) => {
                                    outputtx.send(Output::CPU(
                                        Box::new(Stop {
                                            state: st,
                                            reason: StopReason::None,
                                        }),
                                        None,
                                    ))?;
                                }
                                Err(e) => {
                                    outputtx.send(Output::Error(format!("Reset error - {e}")))?;
                                    continue;
                                }
                                _ => panic!("Invalid return from Reset - {r:?}"),
                            }
                            running = 0;
                        }
                        _ => {
                            outputtx.send(Output::Error(format!(
                                "ERROR: Invalid command - {}",
                                parts[0]
                            )))?;
                            continue;
                        }
                    }
                }
                outputtx.send(Output::Prompt(pre))?;
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
                                let prompt = if running < 2 {
                                    running += 1;
                                    Some(String::new())
                                } else {
                                    None
                                };
                                outputtx.send(Output::CPU(st, prompt))?;
                            } else {
                                running = 0;
                                let mut pre = String::new();
                                if let StopReason::Break(addr) = &st.reason {
                                    writeln!(pre, "\nBreakpoint at {:04X}", addr.addr)?;
                                }
                                if let StopReason::Watch(pc, addr, wp) = &st.reason {
                                    writeln!(pre, "\nWatchpoint triggered for addr {:04X} at {:04X} (next PC at {:04X})\n{wp}", addr.addr, pc.addr, st.state.pc)?;
                                }
                                outputtx.send(Output::CPU(st, Some(pre)))?;
                                outputtx.send(Output::Prompt(None))?;
                            }
                        }
                        _ => panic!("invalid response from run: {ret:?}"),
                    },
                    Err(e) => outputtx.send(Output::Error(format!("Error from Run - {e}")))?,
                },
                Err(TryRecvError::Empty) => {}
                Err(TryRecvError::Disconnected) => return Err(eyre!("Sender channel died")),
            }
        }
    }
}

fn trim_newline(bps: &mut String) {
    // Trim off the last newline
    if bps.ends_with('\n') {
        bps.pop();
        if bps.ends_with('\r') {
            bps.pop();
        }
    }
}

fn val_and_base(val: &str) -> (&str, u32) {
    match val.as_bytes() {
        // Remove any possibly leading 0x or $. If we did this is also base 16
        // Can index back into val without worry about utf8 since we only matched
        // ascii chars here.
        [b'0', b'x', ..] => (&val[2..], 16),
        [b'$', ..] => (&val[1..], 16),
        [b'%', ..] => (&val[1..], 2),
        _ => (val, 10),
    }
}

fn parse_u16(val: &str) -> Result<u16> {
    let (trimmed, base) = val_and_base(val);

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
    let (trimmed, base) = val_and_base(val);

    // If nothing was left we're done with no value.
    if trimmed.is_empty() {
        return Err(eyre!("no input"));
    }

    match u8::from_str_radix(trimmed, base) {
        Ok(v) => Ok(v),
        Err(e) => Err(eyre!("parse error: {e}")),
    }
}

struct Debug {
    state: Rc<RefCell<CPUState>>,
    full: Rc<RefCell<bool>>,
}

impl Debug {
    fn debug(&self) -> (Rc<RefCell<CPUState>>, bool) {
        (Rc::clone(&self.state), *self.full.borrow())
    }
}

fn step(cpu: &mut dyn CPU) -> Result<()> {
    loop {
        cpu.tick()?;
        cpu.tick_done()?;

        if cpu.op_tick() == Tick::Reset {
            break;
        }
    }
    Ok(())
}

fn valid_range(
    range: &LocationRange,
    cpucommandresptx: &Sender<Result<CommandResponse>>,
) -> Result<u16> {
    let mut len = 0;
    if let Some(l) = range.len {
        len = l;
    }
    if (usize::from(range.addr) + usize::from(len - 1)) > MAX_SIZE {
        cpucommandresptx.send(Err(eyre!(
            "invalid size {} + {} exceeds {MAX_SIZE}",
            range.addr,
            len - 1
        )))?;
        return Err(eyre!("range error"));
    }
    Ok(len)
}

fn reset(cpu: &mut dyn CPU, cpucommandresptx: &Sender<Result<CommandResponse>>) -> Result<()> {
    loop {
        match cpu.reset() {
            Ok(OpState::Done) => break,
            Ok(OpState::Processing) => continue,
            Err(e) => {
                cpucommandresptx.send(Err(eyre!("reset error: {e}")))?;
                break;
            }
        }
    }
    Ok(())
}

fn advance(
    cpu: &mut dyn CPU,
    cpucommandresptx: &Sender<Result<CommandResponse>>,
    ram: &mut [u8; MAX_SIZE],
    tick_pc: &mut u16,
    breakpoints: &Vec<Location>,
    watchpoints: &Vec<Location>,
    do_tick: bool,
) -> Result<StopReason> {
    if !watchpoints.is_empty() {
        // If we have watchpoints get a memory snapshot.
        cpu.ram().borrow().ram(ram);
    }

    if cpu.op_tick() == Tick::Reset {
        *tick_pc = cpu.pc();
    }

    // Check PC before we execute as we break before stepping.
    let mut reason = StopReason::Run;
    for b in breakpoints {
        if b.addr == *tick_pc {
            reason = StopReason::Break(Location { addr: b.addr });
            return Ok(reason);
        }
    }

    if do_tick {
        let r = cpu.tick();
        if let Err(e) = r {
            let e = eyre!("tick error: {e}");
            cpucommandresptx.send(Err(e))?;
            return Ok(StopReason::None);
        }
        let r = cpu.tick_done();
        if let Err(e) = r {
            let e = eyre!("tick done error: {e}");
            cpucommandresptx.send(Err(e))?;
            return Ok(StopReason::None);
        }
    } else {
        let r = step(cpu);
        if let Err(e) = r {
            let e = eyre!("step error: {e}");
            cpucommandresptx.send(Err(e))?;
            return Ok(StopReason::None);
        }
    }

    if !watchpoints.is_empty() {
        let cr = cpu.ram();
        let cr = cr.borrow();
        for w in watchpoints {
            if cr.read(w.addr) != ram[usize::from(w.addr)] {
                let mut pre = String::with_capacity(32);
                let _ = cpu.disassemble(&mut pre, *tick_pc, cpu.ram().borrow().as_ref());
                reason = StopReason::Watch(PC { addr: *tick_pc }, Location { addr: w.addr }, pre);
                break;
            }
        }
    }

    Ok(reason)
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
    ty: CPUType,
    cpucommandrx: &Receiver<Command>,
    cpucommandresptx: &Sender<Result<CommandResponse>>,
) -> Result<()> {
    let mut nmos = CPU6502::new(ChipDef::default());
    let mut ricoh = CPURicoh::new(ChipDef::default());
    let mut c6510 = CPU6510::new(ChipDef::default(), None);
    let mut cmos = CPU65C02::new(ChipDef::default());
    let cpu: &mut dyn CPU = match ty {
        CPUType::NMOS => &mut nmos,
        CPUType::RICOH => &mut ricoh,
        CPUType::NMOS6510 => &mut c6510,
        CPUType::CMOS => &mut cmos,
    };

    let mut is_running = false;
    let mut is_init = false;
    let d = Debug {
        state: Rc::new(RefCell::new(CPUState::default())),
        full: Rc::new(RefCell::new(false)),
    };
    let debug = { || d.debug() };

    let mut breakpoints: Vec<Location> = Vec::new();
    let mut watchpoints: Vec<Location> = Vec::new();

    let mut running_ram_snapshot = false;
    let mut tick_pc = 0_u16;

    let mut ram = [0_u8; MAX_SIZE];

    loop {
        if is_running {
            let reason;
            {
                reason = advance(
                    cpu,
                    cpucommandresptx,
                    &mut ram,
                    &mut tick_pc,
                    &breakpoints,
                    &watchpoints,
                    false,
                )?;
            }
            if reason != StopReason::Run {
                is_running = false;
            }
            cpu.set_debug(Some(&debug));
            *d.full.borrow_mut() = running_ram_snapshot;
            cpu.debug();
            cpu.set_debug(None);
            *d.full.borrow_mut() = false;
            let _ = cpu.disassemble(
                &mut d.state.borrow_mut().dis,
                cpu.pc(),
                cpu.ram().borrow().as_ref(),
            );
            if let StopReason::Break(_) = reason {
                // Progress one tick into the next instruction. This moves PC
                // forward so hitting "C" after a breakpoint will "just work".
                // Otherwise we'd have to delete the breakpoint to get past it.
                cpu.tick()?;
                cpu.tick_done()?;
                tick_pc = cpu.pc();
            }
            let st = Box::new(Stop {
                state: Box::new(d.state.borrow().clone()),
                reason: reason.clone(),
            });
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
            Command::Run(ram) => {
                if !is_init {
                    is_init = true;
                    cpu.power_on()?;
                }
                running_ram_snapshot = ram;
                is_running = true;
                cpu.set_debug(Some(&debug));
                *d.full.borrow_mut() = running_ram_snapshot;
                cpu.debug();
                cpu.set_debug(None);
                *d.full.borrow_mut() = false;
                let _ = cpu.disassemble(
                    &mut d.state.borrow_mut().dis,
                    cpu.pc(),
                    cpu.ram().borrow().as_ref(),
                );
                let st = Box::new(Stop {
                    state: Box::new(d.state.borrow().clone()),
                    reason: StopReason::Run,
                });
                cpucommandresptx.send(Ok(CommandResponse::Stop(st)))?;
            }
            Command::Stop => {
                is_running = false;
                cpu.set_debug(Some(&debug));
                cpu.debug();
                cpu.set_debug(None);
                let _ = cpu.disassemble(
                    &mut d.state.borrow_mut().dis,
                    cpu.pc(),
                    cpu.ram().borrow().as_ref(),
                );
                let st = Box::new(Stop {
                    state: Box::new(d.state.borrow().clone()),
                    reason: StopReason::Stop,
                });
                println!("Sending stop: {st:?}");
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
            Command::Step(capture_ram) => {
                if !is_init {
                    is_init = true;
                    cpu.power_on()?;
                }
                let mut reason = advance(
                    cpu,
                    cpucommandresptx,
                    &mut ram,
                    &mut tick_pc,
                    &breakpoints,
                    &watchpoints,
                    false,
                )?;
                if !capture_ram {
                    // Reset RAM to clear as we might have copied before this time and
                    // future ones should start clean.
                    d.state.borrow_mut().ram = [0; MAX_SIZE];
                }
                // The closure assumes run. Since we're doing single step only change
                // that when returned or things go off contract.
                if reason == StopReason::Run {
                    reason = StopReason::Step;
                }
                cpu.set_debug(Some(&debug));
                *d.full.borrow_mut() = capture_ram;
                cpu.debug();
                cpu.set_debug(None);
                *d.full.borrow_mut() = false;
                let _ = cpu.disassemble(
                    &mut d.state.borrow_mut().dis,
                    cpu.pc(),
                    cpu.ram().borrow().as_ref(),
                );
                if let StopReason::Break(_) = reason {
                    // Progress one tick into the next instruction. This moves PC
                    // forward so hitting "C" after a breakpoint will "just work".
                    // Otherwise we'd have to delete the breakpoint to get past it.
                    cpu.tick()?;
                    cpu.tick_done()?;
                    tick_pc = cpu.pc();
                }
                let st = Box::new(Stop {
                    state: Box::new(d.state.borrow().clone()),
                    reason,
                });
                cpucommandresptx.send(Ok(CommandResponse::Step(st)))?;
            }
            Command::StepN(stepn) => {
                if !is_init {
                    is_init = true;
                    cpu.power_on()?;
                }
                let mut early = false;
                let mut out = stepn.capture;

                // Reset RAM to clear as we might have copied before this time and
                // future ones should start clean.
                d.state.borrow_mut().ram = [0; MAX_SIZE];
                for i in 0..stepn.reps {
                    let reason = advance(
                        cpu,
                        cpucommandresptx,
                        &mut ram,
                        &mut tick_pc,
                        &breakpoints,
                        &watchpoints,
                        false,
                    )?;
                    // Might have triggered a break/watch so return early then.
                    if reason != StopReason::Run {
                        cpu.set_debug(Some(&debug));
                        *d.full.borrow_mut() = stepn.ram;
                        cpu.debug();
                        let _ = cpu.disassemble(
                            &mut d.state.borrow_mut().dis,
                            cpu.pc(),
                            cpu.ram().borrow().as_ref(),
                        );
                        *d.full.borrow_mut() = false;
                        cpu.set_debug(None);
                        if let StopReason::Break(_) = reason {
                            // Progress one tick into the next instruction. This moves PC
                            // forward so hitting "C" after a breakpoint will "just work".
                            // Otherwise we'd have to delete the breakpoint to get past it.
                            cpu.tick()?;
                            cpu.tick_done()?;
                            tick_pc = cpu.pc();
                        }
                        let st = Box::new(Stop {
                            state: Box::new(d.state.borrow().clone()),
                            reason,
                        });
                        cpucommandresptx.send(Ok(CommandResponse::StepN(StepNReason::Stop(st))))?;
                        early = true;
                        break;
                    }
                    // Don't record till the end.
                    if i >= stepn.reps - out.len() {
                        cpu.set_debug(Some(&debug));
                        *d.full.borrow_mut() = stepn.ram;
                        cpu.debug();
                        cpu.set_debug(None);
                        *d.full.borrow_mut() = false;
                        let _ = cpu.disassemble(
                            &mut d.state.borrow_mut().dis,
                            cpu.pc(),
                            cpu.ram().borrow().as_ref(),
                        );
                        let idx = i + out.len() - stepn.reps;
                        if stepn.ram {
                            out[idx] = d.state.borrow().clone();
                        } else {
                            d.state.borrow().shallow_copy(&mut out[idx]);
                        }
                        //out.push(d.state.borrow().clone());
                    }
                }
                if early {
                    continue;
                }
                cpucommandresptx.send(Ok(CommandResponse::StepN(StepNReason::StepN(out))))?;
            }
            Command::Tick(capture_ram) => {
                if !is_init {
                    is_init = true;
                    cpu.power_on()?;
                }
                let mut reason = advance(
                    cpu,
                    cpucommandresptx,
                    &mut ram,
                    &mut tick_pc,
                    &breakpoints,
                    &watchpoints,
                    true,
                )?;
                if !capture_ram {
                    // Reset RAM to clear as we might have copied before this time and
                    // future ones should start clean.
                    d.state.borrow_mut().ram = [0; MAX_SIZE];
                }

                // The closure assumes run. Since we're doing single step only change
                // that when returned or things go off contract.
                if reason == StopReason::Run {
                    reason = StopReason::Tick;
                }
                cpu.set_debug(Some(&debug));
                *d.full.borrow_mut() = capture_ram;
                cpu.debug();
                cpu.set_debug(None);
                *d.full.borrow_mut() = false;
                if let StopReason::Break(_) = reason {
                    // Progress one tick into the next instruction. This moves PC
                    // forward so hitting "C" after a breakpoint will "just work".
                    // Otherwise we'd have to delete the breakpoint to get past it.
                    cpu.tick()?;
                    cpu.tick_done()?;
                    tick_pc = cpu.pc();
                }
                let _ = cpu.disassemble(
                    &mut d.state.borrow_mut().dis,
                    cpu.pc(),
                    cpu.ram().borrow().as_ref(),
                );
                let st = Box::new(Stop {
                    state: Box::new(d.state.borrow().clone()),
                    reason,
                });
                cpucommandresptx.send(Ok(CommandResponse::Tick(st)))?;
            }

            Command::Read(addr) => {
                let val = cpu.ram().borrow().read(addr.addr);
                cpucommandresptx.send(Ok(CommandResponse::Read(Val { val })))?;
            }
            Command::ReadRange(range) => {
                if let Ok(len) = valid_range(&range, cpucommandresptx) {
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
                if let Ok(len) = valid_range(&range, cpucommandresptx) {
                    for i in 0..len {
                        cpu.ram().borrow_mut().write(range.addr + i, val.val);
                    }
                    cpucommandresptx.send(Ok(CommandResponse::WriteRange))?;
                }
            }
            Command::Cpu => {
                cpu.set_debug(Some(&debug));
                cpu.debug();
                cpu.set_debug(None);
                let _ = cpu.disassemble(
                    &mut d.state.borrow_mut().dis,
                    cpu.pc(),
                    cpu.ram().borrow().as_ref(),
                );
                cpucommandresptx
                    .send(Ok(CommandResponse::Cpu(Box::new(d.state.borrow().clone()))))?;
            }
            Command::Ram => {
                let mut r = Box::new([0u8; MAX_SIZE]);
                cpu.ram().borrow().ram(&mut r);
                cpucommandresptx.send(Ok(CommandResponse::Ram(r)))?;
            }
            Command::Disassemble(addr) => {
                let mut s = String::with_capacity(32);
                let _ = cpu.disassemble(&mut s, addr.addr, cpu.ram().borrow().as_ref());
                cpucommandresptx.send(Ok(CommandResponse::Disassemble(s)))?;
            }
            Command::DisassembleRange(range) => {
                if let Ok(len) = valid_range(&range, cpucommandresptx) {
                    let mut r = Vec::new();
                    let mut pc = range.addr;
                    let mut s = String::with_capacity(32);
                    while pc < range.addr + len {
                        let newpc = cpu.disassemble(&mut s, pc, cpu.ram().borrow().as_ref());
                        r.push(s.clone());
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
                    if watchpoints.is_empty() {
                        cpucommandresptx.send(Err(eyre!("no watchpoints to delete")))?;
                    } else {
                        cpucommandresptx.send(Err(eyre!(
                            "watchpoint index {num} out of range 0-{}",
                            watchpoints.len() - 1
                        )))?;
                    }
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
                        reset(cpu, cpucommandresptx)?;
                        if let Some(start) = start {
                            cpu.pc_mut(start.addr);
                        } else {
                            // We don't always reset so force start PC
                            // to reset vector always.
                            let addr = u16::from(cpu.ram().borrow().read(RESET_VECTOR + 1)) << 8
                                | u16::from(cpu.ram().borrow().read(RESET_VECTOR));
                            cpu.pc_mut(addr);
                        }
                        cpu.set_debug(Some(&debug));
                        cpu.debug();
                        cpu.set_debug(None);
                        let _ = cpu.disassemble(
                            &mut d.state.borrow_mut().dis,
                            cpu.pc(),
                            cpu.ram().borrow().as_ref(),
                        );
                        cpucommandresptx.send(Ok(CommandResponse::Load(Box::new(
                            d.state.borrow().clone(),
                        ))))?;
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
                    Ok(()) => {
                        cpucommandresptx.send(Ok(CommandResponse::Dump))?;
                    }
                    Err(e) => {
                        cpucommandresptx.send(Err(eyre!("can't write file: {e}")))?;
                    }
                }
            }
            Command::PC(addr) => {
                cpu.pc_mut(addr.addr);
                cpu.set_debug(Some(&debug));
                cpu.debug();
                cpu.set_debug(None);
                let _ = cpu.disassemble(
                    &mut d.state.borrow_mut().dis,
                    cpu.pc(),
                    cpu.ram().borrow().as_ref(),
                );
                cpucommandresptx
                    .send(Ok(CommandResponse::PC(Box::new(d.state.borrow().clone()))))?;
            }
            Command::Reset => {
                if !is_init {
                    is_init = true;
                    cpu.power_on()?;
                    // Power on does a reset so we don't have to do it again below.
                    cpu.set_debug(Some(&debug));
                    cpu.debug();
                    cpu.set_debug(None);
                    let _ = cpu.disassemble(
                        &mut d.state.borrow_mut().dis,
                        cpu.pc(),
                        cpu.ram().borrow().as_ref(),
                    );
                    cpucommandresptx.send(Ok(CommandResponse::Reset(Box::new(
                        d.state.borrow().clone(),
                    ))))?;
                    continue;
                }
                reset(cpu, cpucommandresptx)?;
                cpu.set_debug(Some(&debug));
                cpu.debug();
                cpu.set_debug(None);
                let _ = cpu.disassemble(
                    &mut d.state.borrow_mut().dis,
                    cpu.pc(),
                    cpu.ram().borrow().as_ref(),
                );
                cpucommandresptx.send(Ok(CommandResponse::Reset(Box::new(
                    d.state.borrow().clone(),
                ))))?;
            }
        }
    }
}
