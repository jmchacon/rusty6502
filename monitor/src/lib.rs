//! monitor implements a library for running a 65xx chip in a monitor and
//! then interfacing to the outside world via input/output channels.
use color_eyre::eyre::{eyre, Result};
use rusty6502::prelude::*;
use std::borrow::BorrowMut;
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

    let mut res = MatchCmdResult::default();
    loop {
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
                    if res.running > 0 {
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
                    res = match_cmd(&cmd, &parts, outputtx, cpucommandtx, cpucommandresprx)?;

                    // The user said to QUIT.
                    if res.exit {
                        return Ok(());
                    }
                    // If this was an error we just loop back around now.
                    if res.cont {
                        continue;
                    }
                }
                outputtx.send(Output::Prompt(res.pre.clone()))?;
            }
            Err(TryRecvError::Empty) => {}
            Err(TryRecvError::Disconnected) => return Err(eyre!("stdin died?")),
        }

        // Once it's running we have to juggle stdin vs possibly brk/watch happening.
        if res.running > 0 {
            res.running = process_running(res.running, outputtx, cpucommandresprx)?;
        }
    }
}

#[derive(Default, Debug)]
struct MatchCmdResult {
    running: i32,
    cont: bool,
    pre: Option<String>,
    exit: bool,
}

fn match_cmd(
    cmd: &str,
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<MatchCmdResult> {
    let mut res = MatchCmdResult::default();
    match cmd {
        "H" | "HELP" => {
            res.pre = help_cmd().into();
        }
        "QUIT" | "Q" => {
            res.exit = true;
        }
        "RUN" | "C" => {
            (res.running, res.cont) = run_cmd(parts, outputtx, cpucommandtx)?;
        }
        "STOP" => {
            cpucommandtx.send(Command::Stop)?;
        }
        "B" => {
            res.cont = b_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "BPL" => {
            (res.pre, res.cont) = bpl_cmd(outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "DB" => {
            res.cont = db_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "S" => {
            (res.pre, res.cont) = s_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "STEPN" => {
            res.cont = stepn_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "T" => {
            (res.pre, res.cont) = t_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }

        "R" => {
            (res.pre, res.cont) = r_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "RR" => {
            res.cont = rr_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "W" => {
            res.cont = w_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "WR" => {
            res.cont = wr_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "CPU" => {
            res.cont = cpu_cmd(outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "RAM" => {
            res.cont = ram_cmd(outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "D" => {
            (res.pre, res.cont) = d_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "DR" => {
            (res.pre, res.cont) = dr_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "WP" => {
            res.cont = wp_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "WPL" => {
            (res.pre, res.cont) = wpl_cmd(outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "DW" => {
            res.cont = dw_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "L" => {
            res.cont = l_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "BIN" => {
            res.cont = bin_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "PC" => {
            res.cont = pc_cmd(parts, outputtx, cpucommandtx, cpucommandresprx)?;
        }
        "RESET" => {
            (res.running, res.cont) = reset_cmd(outputtx, cpucommandtx, cpucommandresprx)?;
        }
        _ => {
            outputtx.send(Output::Error(format!("ERROR: Invalid command - {cmd}")))?;
            res.cont = true;
        }
    }
    Ok(res)
}

fn help_cmd() -> String {
    r"Usage:
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
QUIT | Q - Exit the monitor"
                                    .into()
}

fn run_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
) -> Result<(i32, bool)> {
    if parts.len() > 2 {
        outputtx.send(Output::Error(String::from(
            "Error - Run can only optionally be follow by true: RUN [true]",
        )))?;
        Ok((0, true))
    } else {
        let ram = parts.len() == 2 && parts[1] == "TRUE";
        cpucommandtx.send(Command::Run(ram))?;
        Ok((1, false))
    }
}

fn b_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<bool> {
    if parts.len() != 2 {
        outputtx.send(Output::Error(
            "Error - Break must include an addr - B <addr>".into(),
        ))?;
        return Ok(true);
    }
    match parse_u16(parts[1]) {
        Ok(addr) => {
            cpucommandtx.send(Command::Break(Location { addr }))?;
            let r = cpucommandresprx.recv()?;
            println!("Got resp: {r:?}");
            match r {
                Ok(CommandResponse::Break) => {}
                Err(e) => {
                    outputtx.send(Output::Error(format!("Break error - {e}")))?;
                    return Ok(true);
                }
                _ => return Err(eyre!("Invalid return from Break - {r:?}")),
            }
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on addr: {e}")))?;
            return Ok(true);
        }
    }
    Ok(false)
}

fn bpl_cmd(
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<(Option<String>, bool)> {
    cpucommandtx.send(Command::Breaklist)?;
    let r = cpucommandresprx.recv()?;
    match r {
        Ok(CommandResponse::BreakList(bl)) => {
            let mut bps = String::new();
            writeln!(bps, "Breakpoints:")?;
            for (pos, l) in bl.iter().enumerate() {
                writeln!(bps, "{pos} - ${:04X}", l.addr)?;
            }
            trim_newline(&mut bps);
            Ok((Some(bps), false))
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("BreakList error - {e}")))?;
            Ok((None, true))
        }
        _ => Err(eyre!("Invalid return from BreakList - {r:?}")),
    }
}

fn db_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<bool> {
    if parts.len() != 2 {
        outputtx.send(Output::Error(
            "Error - Delete Breakpoint must include an num - DB <num>".into(),
        ))?;
        return Ok(true);
    }
    match parts[1].parse::<usize>() {
        Ok(num) => {
            cpucommandtx.send(Command::DeleteBreakpoint(num))?;
            let r = cpucommandresprx.recv()?;
            match r {
                Ok(CommandResponse::DeleteBreakpoint) => Ok(false),
                Err(e) => {
                    outputtx.send(Output::Error(format!("Delete Breakpoint error - {e}")))?;
                    Ok(true)
                }
                _ => Err(eyre!("Invalid return from Delete Breakpoint - {r:?}")),
            }
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on num: {e}")))?;
            Ok(true)
        }
    }
}

fn s_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<(Option<String>, bool)> {
    if parts.len() > 2 {
        outputtx.send(Output::Error(
            "Error - Step can only optionally be follow by true: S [true]".into(),
        ))?;
        return Ok((None, true));
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
                pre = Some(format!(
                    "\nWatchpoint triggered for addr {:04X} at {:04X} (next PC at {:04X})\n{wp}",
                    addr.addr, pc.addr, st.state.pc
                ));
            }
            outputtx.send(Output::CPU(st, pre.clone()))?;
            Ok((pre, false))
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("Step error - {e}")))?;
            Ok((None, true))
        }
        _ => Err(eyre!("Invalid return from Step - {r:?}")),
    }
}

fn stepn_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<bool> {
    if parts.len() != 4 {
        outputtx.send(Output::Error(
            "Error - STEPN must be followed by a count, repeat and RAM snapshot indicator".into(),
        ))?;
        return Ok(true);
    }
    let reps = match parse_u16(parts[1]) {
        Ok(reps) => usize::from(reps),
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on count: {e}")))?;
            return Ok(true);
        }
    };
    let capture = match parse_u16(parts[2]) {
        Ok(capture) => usize::from(capture),
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on repeat: {e}")))?;
            return Ok(true);
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
                    pre = Some(format!("\nBreakpoint at {:04X}", addr.addr));
                }
                if let StopReason::Watch(pc, addr, wp) = &stop.reason {
                    pre = Some(format!("\nWatchpoint triggered for addr {:04X} at {:04X} (next PC at {:04X})\n{wp}", addr.addr, pc.addr, stop.state.pc));
                }
                outputtx.send(Output::CPU(stop, pre))?;
                Ok(false)
            }
            StepNReason::StepN(stepn) => {
                outputtx.send(Output::StepN(stepn))?;
                Ok(false)
            }
        },
        Err(e) => {
            outputtx.send(Output::Error(format!("StepN error - {e}")))?;
            Ok(true)
        }
        _ => Err(eyre!("Invalid return from StepN - {r:?}")),
    }
}

fn t_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<(Option<String>, bool)> {
    if parts.len() > 2 {
        outputtx.send(Output::Error(String::from(
            "Error - Tick can only optionally be follow by true: T [true]",
        )))?;
        return Ok((None, true));
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
                pre = Some(format!(
                    "\nWatchpoint triggered for addr {:04X} at {:04X} (next PC at {:04X})\n{wp}",
                    addr.addr, pc.addr, st.state.pc
                ));
            }
            outputtx.send(Output::CPU(st, pre.clone()))?;
            Ok((pre, false))
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("Tick error - {e}")))?;
            Ok((None, true))
        }
        _ => Err(eyre!("Invalid return from Tick - {r:?}")),
    }
}

fn r_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<(Option<String>, bool)> {
    if parts.len() != 2 {
        outputtx.send(Output::Error(
            "Error - Read must include an addr - R <addr>".into(),
        ))?;
        return Ok((None, true));
    }
    match parse_u16(parts[1]) {
        Ok(addr) => {
            cpucommandtx.send(Command::Read(Location { addr }))?;
            let r = cpucommandresprx.recv()?;
            match r {
                Ok(CommandResponse::Read(r)) => {
                    Ok((Some(format!("{addr:04X}  {:02X}", r.val)), false))
                }
                Err(e) => {
                    outputtx.send(Output::Error(format!("Read error - {e}\n")))?;
                    Ok((None, true))
                }
                _ => Err(eyre!("Invalid return from Read - {r:?}")),
            }
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on addr: {e}")))?;
            Ok((None, true))
        }
    }
}

fn rr_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<bool> {
    if parts.len() != 3 {
        outputtx.send(Output::Error(
            "Error - Read Range must include an addr and len - RR <addr> <len>".into(),
        ))?;
        return Ok(true);
    }
    let addr = match parse_u16(parts[1]) {
        Ok(addr) => addr,
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on addr: {e}")))?;
            return Ok(true);
        }
    };
    let len = match parse_u16(parts[2]) {
        Ok(len) => len,
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on len: {e}")))?;
            return Ok(true);
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
            Ok(false)
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("Read Range error - {e}")))?;
            Ok(true)
        }
        _ => Err(eyre!("Invalid return from Read Range - {r:?}")),
    }
}

fn w_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<bool> {
    if parts.len() != 3 {
        outputtx.send(Output::Error(
            "Error - Write must include an addr and value - W <addr> <val>".into(),
        ))?;
        return Ok(true);
    }
    let addr = match parse_u16(parts[1]) {
        Ok(addr) => addr,
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on addr: {e}")))?;
            return Ok(true);
        }
    };
    let val = match parse_u8(parts[2]) {
        Ok(val) => val,
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on val: {e}")))?;
            return Ok(true);
        }
    };

    cpucommandtx.send(Command::Write(Location { addr }, Val { val }))?;
    let r = cpucommandresprx.recv()?;
    match r {
        Ok(CommandResponse::Write) => Ok(false),
        Err(e) => {
            outputtx.send(Output::Error(format!("Write error - {e}")))?;
            Ok(true)
        }
        _ => Err(eyre!("Invalid return from Write - {r:?}")),
    }
}

fn wr_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<bool> {
    if parts.len() != 4 {
        outputtx.send(Output::Error(
            "Error - Write Range must include an addr len and val - WR <addr> <len> <val>".into(),
        ))?;
        return Ok(true);
    }
    let addr = match parse_u16(parts[1]) {
        Ok(addr) => addr,
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on addr: {e}")))?;
            return Ok(true);
        }
    };
    let len = match parse_u16(parts[2]) {
        Ok(len) => len,
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on len: {e}")))?;
            return Ok(true);
        }
    };
    let val = match parse_u8(parts[3]) {
        Ok(val) => val,
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on val: {e}")))?;
            return Ok(true);
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
        Ok(CommandResponse::WriteRange) => Ok(false),
        Err(e) => {
            outputtx.send(Output::Error(format!("Write Range error - {e}")))?;
            Ok(true)
        }
        _ => Err(eyre!("Invalid return from Write Range - {r:?}")),
    }
}

fn cpu_cmd(
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<bool> {
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
            Ok(false)
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("Cpu error - {e}")))?;
            Ok(true)
        }
        _ => Err(eyre!("Invalid return from Cpu - {r:?}")),
    }
}

fn ram_cmd(
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<bool> {
    cpucommandtx.send(Command::Ram)?;
    let r = cpucommandresprx.recv()?;
    match r {
        Ok(CommandResponse::Ram(r)) => {
            outputtx.send(Output::RAM(r))?;
            Ok(false)
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("Ram error - {e}")))?;
            Ok(true)
        }
        _ => Err(eyre!("Invalid return from Ram - {r:?}")),
    }
}

fn d_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<(Option<String>, bool)> {
    if parts.len() != 2 {
        outputtx.send(Output::Error(
            "Error - Disassemble must include an addr - D <addr>".into(),
        ))?;
        return Ok((None, true));
    }
    match parse_u16(parts[1]) {
        Ok(addr) => {
            cpucommandtx.send(Command::Disassemble(Location { addr }))?;
            let r = cpucommandresprx.recv()?;
            match r {
                Ok(CommandResponse::Disassemble(d)) => Ok((Some(d), false)),
                Err(e) => {
                    outputtx.send(Output::Error(format!("Disassemble error - {e}")))?;
                    Ok((None, true))
                }
                _ => Err(eyre!("Invalid return from Disassemble - {r:?}")),
            }
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on addr: {e}")))?;
            Ok((None, true))
        }
    }
}

fn dr_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<(Option<String>, bool)> {
    if parts.len() != 3 {
        outputtx.send(Output::Error(
            "Error - Disassemble Range must include an addr and len - DR <addr> <len>".into(),
        ))?;
        return Ok((None, true));
    }
    let addr = match parse_u16(parts[1]) {
        Ok(addr) => addr,
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on addr: {e}")))?;
            return Ok((None, true));
        }
    };
    let len = match parse_u16(parts[2]) {
        Ok(len) => len,
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on len: {e}\n")))?;
            return Ok((None, true));
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
            Ok((Some(dr), false))
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("Disassemble Range error - {e}")))?;
            Ok((None, true))
        }
        _ => Err(eyre!("Invalid return from Disassemble Range - {r:?}")),
    }
}

fn wp_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<bool> {
    if parts.len() != 2 {
        outputtx.send(Output::Error(
            "Error - Watchpoint must include an addr - WP <addr>".into(),
        ))?;
        return Ok(true);
    }
    match parse_u16(parts[1]) {
        Ok(addr) => {
            cpucommandtx.send(Command::Watch(Location { addr }))?;
            let r = cpucommandresprx.recv()?;
            match r {
                Ok(CommandResponse::Watch) => Ok(false),
                Err(e) => {
                    outputtx.send(Output::Error(format!("Watchpoint error - {e}")))?;
                    Ok(true)
                }
                _ => Err(eyre!("Invalid return from Watchpoint - {r:?}")),
            }
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on addr: {e}")))?;
            Ok(true)
        }
    }
}

fn wpl_cmd(
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<(Option<String>, bool)> {
    cpucommandtx.send(Command::Watchlist)?;
    let r = cpucommandresprx.recv()?;
    match r {
        Ok(CommandResponse::WatchList(wl)) => {
            let mut wps = String::new();
            writeln!(wps, "Watchpoints:")?;
            for (pos, l) in wl.iter().enumerate() {
                writeln!(wps, "{pos} - ${:04X}", l.addr)?;
            }
            trim_newline(&mut wps);
            Ok((Some(wps), false))
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("WatchList error - {e}\n")))?;
            Ok((None, true))
        }
        _ => Err(eyre!("Invalid return from WatchList - {r:?}")),
    }
}

fn dw_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<bool> {
    if parts.len() != 2 {
        outputtx.send(Output::Error(
            "Error - Delete Watchpoint must include an num - DW <num>".into(),
        ))?;
        return Ok(true);
    }
    match parts[1].parse::<usize>() {
        Ok(num) => {
            cpucommandtx.send(Command::DeleteWatchpoint(num))?;
            let r = cpucommandresprx.recv()?;
            match r {
                Ok(CommandResponse::DeleteWatchpoint) => Ok(false),
                Err(e) => {
                    outputtx.send(Output::Error(format!("Delete Watchpoint error - {e}")))?;
                    Ok(true)
                }
                _ => Err(eyre!("Invalid return from Delete Watchpoint - {r:?}")),
            }
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on num: {e}")))?;
            Ok(true)
        }
    }
}

fn l_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<bool> {
    if parts.len() < 2 || parts.len() > 4 {
        outputtx.send(Output::Error("Error - Load must include a filename and optionally load location with optional start - L <path to file> [location [start]]".into()))?;
        return Ok(true);
    }
    let file: String = parts[1].into();
    let loc = if parts.len() == 3 {
        let addr = match parse_u16(parts[2]) {
            Ok(addr) => addr,
            Err(e) => {
                outputtx.send(Output::Error(format!(
                    "Error - parse error on location: {e}"
                )))?;
                return Ok(true);
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
                outputtx.send(Output::Error(format!("Error - parse error on start: {e}")))?;
                return Ok(true);
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
            Ok(false)
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("Load error - {e}")))?;
            Ok(true)
        }
        _ => Err(eyre!("Invalid return from Load - {r:?}")),
    }
}

fn bin_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<bool> {
    if parts.len() != 2 {
        outputtx.send(Output::Error(
            "Error - Dump must include a filename - BIN <path to file>".into(),
        ))?;
        return Ok(true);
    }
    let file: String = parts[1].into();
    cpucommandtx.send(Command::Dump(file))?;
    let r = cpucommandresprx.recv()?;
    match r {
        Ok(CommandResponse::Dump) => Ok(false),
        Err(e) => {
            outputtx.send(Output::Error(format!("Dump error - {e}")))?;
            Ok(true)
        }
        _ => Err(eyre!("Invalid return from Dump - {r:?}")),
    }
}

fn pc_cmd(
    parts: &[&str],
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<bool> {
    if parts.len() != 2 {
        outputtx.send(Output::Error(
            "Error - PC must include an addr - PC <addr>".into(),
        ))?;
        return Ok(true);
    }
    let addr = match parse_u16(parts[1]) {
        Ok(addr) => addr,
        Err(e) => {
            outputtx.send(Output::Error(format!("Error - parse error on len: {e}")))?;
            return Ok(true);
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
            Ok(false)
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("PC error - {e}")))?;
            Ok(true)
        }
        _ => Err(eyre!("Invalid return from PC - {r:?}")),
    }
}

fn reset_cmd(
    outputtx: &Sender<Output>,
    cpucommandtx: &Sender<Command>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<(i32, bool)> {
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
            Ok((0, false))
        }
        Err(e) => {
            outputtx.send(Output::Error(format!("Reset error - {e}")))?;
            Ok((0, true))
        }
        _ => Err(eyre!("Invalid return from Reset - {r:?}")),
    }
}

fn process_running(
    mut running: i32,
    outputtx: &Sender<Output>,
    cpucommandresprx: &Receiver<Result<CommandResponse>>,
) -> Result<i32> {
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
                        Ok(running)
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
                        Ok(running)
                    }
                }
                _ => Err(eyre!("Invalid return from Run: {ret:?}")),
            },
            Err(e) => {
                outputtx.send(Output::Error(format!("Run error - {e}")))?;
                Ok(running)
            }
        },
        Err(TryRecvError::Empty) => Ok(running),
        Err(TryRecvError::Disconnected) => Err(eyre!("Sender channel died")),
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

#[derive(Debug, Clone)]
struct Debug {
    state: Rc<RefCell<CPUState>>,
    full: Rc<RefCell<bool>>,
}

impl CPUDebug for Debug {
    fn get_debug(&self) -> (Rc<RefCell<CPUState>>, bool) {
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
            Ok(OpState::Processing) => {}
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
                let _ = cpu.disassemble(&mut pre, *tick_pc, &*cpu.ram().borrow(), false);
                reason = StopReason::Watch(PC { addr: *tick_pc }, Location { addr: w.addr }, pre);
                break;
            }
        }
    }

    Ok(reason)
}

// Struct {
//  is_init
//  running_ram_snapshot
//  is_running
//  debug
//  ram
//  tick_pc
//  breakpoints
//  watchpoints
//  cpucommandresptx
struct CPULoopState<'a> {
    is_init: bool,
    running_ram_snapshot: bool,
    is_running: bool,
    debug: Box<Debug>,
    ram: [u8; MAX_SIZE],
    tick_pc: u16,
    breakpoints: Vec<Location>,
    watchpoints: Vec<Location>,
    cpucommandresptx: &'a Sender<Result<CommandResponse>>,
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
pub fn cpu_loop(
    mut cpu: Box<dyn CPU>,
    cpucommandrx: &Receiver<Command>,
    cpucommandresptx: &Sender<Result<CommandResponse>>,
) -> Result<()> {
    // Hold a borrowed ref here as doing this in the advance calls has lifetime issues.
    let cpu: &mut dyn CPU = cpu.borrow_mut();

    let debug = Box::new(Debug {
        state: Rc::new(RefCell::new(CPUState::default())),
        full: Rc::new(RefCell::new(false)),
    });
    let mut state = CPULoopState {
        is_init: false,
        is_running: false,
        running_ram_snapshot: false,
        breakpoints: vec![],
        watchpoints: vec![],
        tick_pc: 0,
        debug: debug.clone(),
        ram: [0; MAX_SIZE],
        cpucommandresptx,
    };

    loop {
        if state.is_running {
            let reason = advance(
                cpu,
                cpucommandresptx,
                &mut state.ram,
                &mut state.tick_pc,
                &state.breakpoints,
                &state.watchpoints,
                false,
            )?;

            if reason != StopReason::Run {
                state.is_running = false;
            }
            cpu.set_debug(Some(debug.clone() as Box<dyn CPUDebug>));
            *std::cell::RefCell::<_>::borrow_mut(&debug.full) = state.running_ram_snapshot;
            cpu.debug();
            cpu.set_debug(None);
            *std::cell::RefCell::<_>::borrow_mut(&debug.full) = false;
            let _ = cpu.disassemble(
                &mut std::cell::RefCell::<_>::borrow_mut(&debug.state).dis,
                cpu.pc(),
                &*cpu.ram().borrow(),
                false,
            );
            if let StopReason::Break(_) = reason {
                // Progress one tick into the next instruction. This moves PC
                // forward so hitting "C" after a breakpoint will "just work".
                // Otherwise we'd have to delete the breakpoint to get past it.
                cpu.tick()?;
                cpu.tick_done()?;
                state.tick_pc = cpu.pc();
            }
            let st = Box::new(Stop {
                state: Box::new(debug.state.borrow().clone()),
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
                // Do this so we have an obvious escape from the loop
                // which helps lifetime analysis.
                break;
            }
            if e == TryRecvError::Empty && state.is_running {
                continue;
            }
        }

        // At this point we're not actively running or we've pulled a command
        // off the channel. So either process that or wait and pull one.
        // Can block now as we're not otherwise doing anything.
        let c = if let Ok(c) = c {
            // If we pulled one off and we're running make sure it's valid.
            // If not write an error and loop back to running.
            if state.is_running {
                match c {
                    Command::Stop | Command::Reset => {}
                    _ => {
                        cpucommandresptx
                            .send(Err(eyre!("only Stop and Reset allowed in Run state")))?;
                        continue;
                    }
                }
            }
            c
        } else {
            cpucommandrx.recv()?
        };

        cpu_match_cmd(c, cpu, &mut state)?;
    }
    Err(eyre!("disconnected from rx"))
}

fn cpu_match_cmd(c: Command, cpu: &mut dyn CPU, state: &mut CPULoopState) -> Result<()> {
    match c {
        Command::Run(ram) => cpu_run(cpu, state, ram)?,
        Command::Stop => cpu_stop(cpu, state)?,
        Command::Break(addr) => cpu_break(state, addr)?,
        Command::Breaklist => cpu_breaklist(state)?,
        Command::DeleteBreakpoint(num) => cpu_delete_breakpoint(state, num)?,
        Command::Step(capture_ram) => cpu_step(cpu, state, capture_ram)?,
        Command::StepN(stepn) => cpu_step_n(cpu, state, stepn)?,
        Command::Tick(capture_ram) => cpu_tick(cpu, state, capture_ram)?,
        Command::Read(addr) => cpu_read(cpu, state, &addr)?,
        Command::ReadRange(range) => cpu_read_range(cpu, state, &range)?,
        Command::Write(addr, val) => cpu_write(cpu, state, &addr, &val)?,
        Command::WriteRange(range, val) => cpu_write_range(cpu, state, &range, &val)?,
        Command::Cpu => cpu_cpu(cpu, state)?,
        Command::Ram => cpu_ram(cpu, state)?,
        Command::Disassemble(addr) => cpu_disassemble(cpu, state, &addr)?,
        Command::DisassembleRange(range) => cpu_disassemble_range(cpu, state, &range)?,
        Command::Watch(addr) => cpu_watch(state, addr)?,
        Command::Watchlist => cpu_watchlist(state)?,
        Command::DeleteWatchpoint(num) => cpu_delete_watchpoint(state, num)?,
        Command::Load(file, loc, start) => cpu_load(cpu, state, &file, loc, start)?,
        Command::Dump(file) => cpu_dump(cpu, state, &file)?,
        Command::PC(addr) => cpu_pc(cpu, state, &addr)?,
        Command::Reset => cpu_reset(cpu, state)?,
    }
    Ok(())
}

fn cpu_run(cpu: &mut dyn CPU, state: &mut CPULoopState, ram: bool) -> Result<()> {
    if !state.is_init {
        state.is_init = true;
        cpu.power_on()?;
    }
    state.running_ram_snapshot = ram;
    state.is_running = true;
    cpu.set_debug(Some(state.debug.clone()));
    *std::cell::RefCell::<_>::borrow_mut(&state.debug.full) = state.running_ram_snapshot;
    cpu.debug();
    cpu.set_debug(None);
    *std::cell::RefCell::<_>::borrow_mut(&state.debug.full) = false;
    let _ = cpu.disassemble(
        &mut std::cell::RefCell::<_>::borrow_mut(&state.debug.state).dis,
        cpu.pc(),
        &*cpu.ram().borrow(),
        false,
    );
    let st = Box::new(Stop {
        state: Box::new(state.debug.state.borrow().clone()),
        reason: StopReason::Run,
    });
    state.cpucommandresptx.send(Ok(CommandResponse::Stop(st)))?;
    Ok(())
}

fn cpu_stop(cpu: &mut dyn CPU, state: &mut CPULoopState) -> Result<()> {
    state.is_running = false;
    cpu.set_debug(Some(state.debug.clone()));
    cpu.debug();
    cpu.set_debug(None);
    let _ = cpu.disassemble(
        &mut std::cell::RefCell::<_>::borrow_mut(&state.debug.state).dis,
        cpu.pc(),
        &*cpu.ram().borrow(),
        false,
    );
    let st = Box::new(Stop {
        state: Box::new(state.debug.state.borrow().clone()),
        reason: StopReason::Stop,
    });
    state.cpucommandresptx.send(Ok(CommandResponse::Stop(st)))?;
    Ok(())
}

fn cpu_break(state: &mut CPULoopState, addr: Location) -> Result<()> {
    state.breakpoints.push(addr);
    state.cpucommandresptx.send(Ok(CommandResponse::Break))?;
    Ok(())
}

fn cpu_breaklist(state: &mut CPULoopState) -> Result<()> {
    state
        .cpucommandresptx
        .send(Ok(CommandResponse::BreakList(state.breakpoints.clone())))?;
    Ok(())
}

fn cpu_delete_breakpoint(state: &mut CPULoopState, num: usize) -> Result<()> {
    if num >= state.breakpoints.len() {
        if state.breakpoints.is_empty() {
            state
                .cpucommandresptx
                .send(Err(eyre!("no breakpoints to delete")))?;
            return Ok(());
        }
        state.cpucommandresptx.send(Err(eyre!(
            "breakpoint index {num} out of range 0-{}",
            state.breakpoints.len() - 1
        )))?;
        return Ok(());
    }
    state.breakpoints.swap_remove(num);
    state
        .cpucommandresptx
        .send(Ok(CommandResponse::DeleteBreakpoint))?;
    Ok(())
}

fn cpu_step(cpu: &mut dyn CPU, state: &mut CPULoopState, capture_ram: bool) -> Result<()> {
    if !state.is_init {
        state.is_init = true;
        cpu.power_on()?;
    }
    let mut reason = advance(
        cpu,
        state.cpucommandresptx,
        &mut state.ram,
        &mut state.tick_pc,
        &state.breakpoints,
        &state.watchpoints,
        false,
    )?;
    if !capture_ram {
        // Reset RAM to clear as we might have copied before this time and
        // future ones should start clean.
        std::cell::RefCell::<_>::borrow_mut(&state.debug.state).ram = [0; MAX_SIZE];
    }
    // The closure assumes run. Since we're doing single step only change
    // that when returned or things go off contract.
    if reason == StopReason::Run {
        reason = StopReason::Step;
    }
    cpu.set_debug(Some(state.debug.clone()));
    *std::cell::RefCell::<_>::borrow_mut(&state.debug.full) = capture_ram;
    cpu.debug();
    cpu.set_debug(None);
    *std::cell::RefCell::<_>::borrow_mut(&state.debug.full) = false;
    let _ = cpu.disassemble(
        &mut std::cell::RefCell::<_>::borrow_mut(&state.debug.state).dis,
        cpu.pc(),
        &*cpu.ram().borrow(),
        false,
    );
    if let StopReason::Break(_) = reason {
        // Progress one tick into the next instruction. This moves PC
        // forward so hitting "C" after a breakpoint will "just work".
        // Otherwise we'd have to delete the breakpoint to get past it.
        cpu.tick()?;
        cpu.tick_done()?;
        state.tick_pc = cpu.pc();
    }
    let st = Box::new(Stop {
        state: Box::new(state.debug.state.borrow().clone()),
        reason,
    });
    state.cpucommandresptx.send(Ok(CommandResponse::Step(st)))?;
    Ok(())
}

fn cpu_step_n(cpu: &mut dyn CPU, state: &mut CPULoopState, stepn: StepN) -> Result<()> {
    if !state.is_init {
        state.is_init = true;
        cpu.power_on()?;
    }
    let mut early = false;
    let mut out = stepn.capture;

    // Reset RAM to clear as we might have copied before this time and
    // future ones should start clean.
    std::cell::RefCell::<_>::borrow_mut(&state.debug.state).ram = [0; MAX_SIZE];
    for i in 0..stepn.reps {
        let reason = advance(
            cpu,
            state.cpucommandresptx,
            &mut state.ram,
            &mut state.tick_pc,
            &state.breakpoints,
            &state.watchpoints,
            false,
        )?;
        // Might have triggered a break/watch so return early then.
        if reason != StopReason::Run {
            cpu.set_debug(Some(state.debug.clone()));
            *std::cell::RefCell::<_>::borrow_mut(&state.debug.full) = stepn.ram;
            cpu.debug();
            let _ = cpu.disassemble(
                &mut std::cell::RefCell::<_>::borrow_mut(&state.debug.state).dis,
                cpu.pc(),
                &*cpu.ram().borrow(),
                false,
            );
            *std::cell::RefCell::<_>::borrow_mut(&state.debug.full) = false;
            cpu.set_debug(None);
            if let StopReason::Break(_) = reason {
                // Progress one tick into the next instruction. This moves PC
                // forward so hitting "C" after a breakpoint will "just work".
                // Otherwise we'd have to delete the breakpoint to get past it.
                cpu.tick()?;
                cpu.tick_done()?;
                state.tick_pc = cpu.pc();
            }
            let st = Box::new(Stop {
                state: Box::new(state.debug.state.borrow().clone()),
                reason,
            });
            state
                .cpucommandresptx
                .send(Ok(CommandResponse::StepN(StepNReason::Stop(st))))?;
            early = true;
            break;
        }
        // Don't record till the end.
        if i >= stepn.reps - out.len() {
            cpu.set_debug(Some(state.debug.clone()));
            *std::cell::RefCell::<_>::borrow_mut(&state.debug.full) = stepn.ram;
            cpu.debug();
            cpu.set_debug(None);
            *std::cell::RefCell::<_>::borrow_mut(&state.debug.full) = false;
            let _ = cpu.disassemble(
                &mut std::cell::RefCell::<_>::borrow_mut(&state.debug.state).dis,
                cpu.pc(),
                &*cpu.ram().borrow(),
                false,
            );
            let idx = i + out.len() - stepn.reps;
            if stepn.ram {
                out[idx] = state.debug.state.borrow().clone();
            } else {
                state.debug.state.borrow().shallow_copy(&mut out[idx]);
            }
        }
    }
    if early {
        return Ok(());
    }
    state
        .cpucommandresptx
        .send(Ok(CommandResponse::StepN(StepNReason::StepN(out))))?;
    Ok(())
}

fn cpu_tick(cpu: &mut dyn CPU, state: &mut CPULoopState, capture_ram: bool) -> Result<()> {
    if !state.is_init {
        state.is_init = true;
        cpu.power_on()?;
    }
    let mut reason = advance(
        cpu,
        state.cpucommandresptx,
        &mut state.ram,
        &mut state.tick_pc,
        &state.breakpoints,
        &state.watchpoints,
        true,
    )?;
    if !capture_ram {
        // Reset RAM to clear as we might have copied before this time and
        // future ones should start clean.
        std::cell::RefCell::<_>::borrow_mut(&state.debug.state).ram = [0; MAX_SIZE];
    }

    // The closure assumes run. Since we're doing single step only change
    // that when returned or things go off contract.
    if reason == StopReason::Run {
        reason = StopReason::Tick;
    }
    cpu.set_debug(Some(state.debug.clone()));
    *std::cell::RefCell::<_>::borrow_mut(&state.debug.full) = capture_ram;
    cpu.debug();
    cpu.set_debug(None);
    *std::cell::RefCell::<_>::borrow_mut(&state.debug.full) = false;
    if let StopReason::Break(_) = reason {
        // Progress one tick into the next instruction. This moves PC
        // forward so hitting "C" after a breakpoint will "just work".
        // Otherwise we'd have to delete the breakpoint to get past it.
        cpu.tick()?;
        cpu.tick_done()?;
        state.tick_pc = cpu.pc();
    }
    let _ = cpu.disassemble(
        &mut std::cell::RefCell::<_>::borrow_mut(&state.debug.state).dis,
        cpu.pc(),
        &*cpu.ram().borrow(),
        false,
    );
    let st = Box::new(Stop {
        state: Box::new(state.debug.state.borrow().clone()),
        reason,
    });
    state.cpucommandresptx.send(Ok(CommandResponse::Tick(st)))?;
    Ok(())
}

fn cpu_read(cpu: &mut dyn CPU, state: &mut CPULoopState, addr: &Location) -> Result<()> {
    let val = cpu.ram().borrow().read(addr.addr);
    state
        .cpucommandresptx
        .send(Ok(CommandResponse::Read(Val { val })))?;
    Ok(())
}

fn cpu_read_range(
    cpu: &mut dyn CPU,
    state: &mut CPULoopState,
    range: &LocationRange,
) -> Result<()> {
    if let Ok(len) = valid_range(range, state.cpucommandresptx) {
        let mut r = Vec::new();
        for i in 0..len {
            let val = cpu.ram().borrow().read(range.addr + i);
            r.push(Val { val });
        }
        state
            .cpucommandresptx
            .send(Ok(CommandResponse::ReadRange(r)))?;
    }
    Ok(())
}

fn cpu_write(
    cpu: &mut dyn CPU,
    state: &mut CPULoopState,
    addr: &Location,
    val: &Val,
) -> Result<()> {
    std::cell::RefCell::<_>::borrow_mut(&cpu.ram()).write(addr.addr, val.val);
    state.cpucommandresptx.send(Ok(CommandResponse::Write))?;
    Ok(())
}

fn cpu_write_range(
    cpu: &mut dyn CPU,
    state: &mut CPULoopState,
    range: &LocationRange,
    val: &Val,
) -> Result<()> {
    if let Ok(len) = valid_range(range, state.cpucommandresptx) {
        for i in 0..len {
            std::cell::RefCell::<_>::borrow_mut(&cpu.ram()).write(range.addr + i, val.val);
        }
        state
            .cpucommandresptx
            .send(Ok(CommandResponse::WriteRange))?;
    }
    Ok(())
}

fn cpu_cpu(cpu: &mut dyn CPU, state: &mut CPULoopState) -> Result<()> {
    cpu.set_debug(Some(state.debug.clone()));
    cpu.debug();
    cpu.set_debug(None);
    let _ = cpu.disassemble(
        &mut std::cell::RefCell::<_>::borrow_mut(&state.debug.state).dis,
        cpu.pc(),
        &*cpu.ram().borrow(),
        false,
    );
    state
        .cpucommandresptx
        .send(Ok(CommandResponse::Cpu(Box::new(
            state.debug.state.borrow().clone(),
        ))))?;

    Ok(())
}

fn cpu_ram(cpu: &mut dyn CPU, state: &mut CPULoopState) -> Result<()> {
    let mut r = Box::new([0u8; MAX_SIZE]);
    cpu.ram().borrow().ram(&mut r);
    state.cpucommandresptx.send(Ok(CommandResponse::Ram(r)))?;
    Ok(())
}

fn cpu_disassemble(cpu: &mut dyn CPU, state: &mut CPULoopState, addr: &Location) -> Result<()> {
    let mut s = String::with_capacity(32);
    let _ = cpu.disassemble(&mut s, addr.addr, &*cpu.ram().borrow(), false);
    state
        .cpucommandresptx
        .send(Ok(CommandResponse::Disassemble(s)))?;
    Ok(())
}

fn cpu_disassemble_range(
    cpu: &mut dyn CPU,
    state: &mut CPULoopState,
    range: &LocationRange,
) -> Result<()> {
    if let Ok(len) = valid_range(range, state.cpucommandresptx) {
        let mut r = Vec::new();
        let mut pc = range.addr;
        let mut s = String::with_capacity(32);
        while pc < range.addr + len {
            let newpc = cpu.disassemble(&mut s, pc, &*cpu.ram().borrow(), false);
            r.push(s.clone());
            pc = newpc;
        }
        state
            .cpucommandresptx
            .send(Ok(CommandResponse::DisassembleRange(r)))?;
    }
    Ok(())
}

fn cpu_watch(state: &mut CPULoopState, addr: Location) -> Result<()> {
    state.watchpoints.push(addr);
    state.cpucommandresptx.send(Ok(CommandResponse::Watch))?;
    Ok(())
}

fn cpu_watchlist(state: &mut CPULoopState) -> Result<()> {
    state
        .cpucommandresptx
        .send(Ok(CommandResponse::WatchList(state.watchpoints.clone())))?;
    Ok(())
}

fn cpu_delete_watchpoint(state: &mut CPULoopState, num: usize) -> Result<()> {
    if num >= state.watchpoints.len() {
        if state.watchpoints.is_empty() {
            state
                .cpucommandresptx
                .send(Err(eyre!("no watchpoints to delete")))?;
        } else {
            state.cpucommandresptx.send(Err(eyre!(
                "watchpoint index {num} out of range 0-{}",
                state.watchpoints.len() - 1
            )))?;
        }
        return Ok(());
    }
    state.watchpoints.swap_remove(num);
    state
        .cpucommandresptx
        .send(Ok(CommandResponse::DeleteWatchpoint))?;
    Ok(())
}

fn cpu_load(
    cpu: &mut dyn CPU,
    state: &mut CPULoopState,
    file: &str,
    loc: Option<Location>,
    start: Option<PC>,
) -> Result<()> {
    let loc = if let Some(loc) = loc {
        loc
    } else {
        Location { addr: 0x000 }
    };

    let path = Path::new(&file);
    match read(path) {
        Ok(b) => {
            if b.len() > MAX_SIZE || (usize::from(loc.addr) + b.len()) > MAX_SIZE {
                state.cpucommandresptx.send(Err(eyre!(
                    "file too large {} at offset {}",
                    b.len(),
                    loc.addr
                )))?;
                return Ok(());
            }
            for (addr, b) in b.iter().enumerate() {
                let a = u16::try_from(addr)?;
                std::cell::RefCell::<_>::borrow_mut(&cpu.ram()).write(loc.addr + a, *b);
            }
            if !state.is_init {
                state.is_init = true;
                cpu.power_on()?;
            }
            reset(cpu, state.cpucommandresptx)?;
            if let Some(start) = start {
                cpu.pc_mut(start.addr);
            } else {
                // We don't always reset so force start PC
                // to reset vector always.
                let addr = u16::from(cpu.ram().borrow().read(RESET_VECTOR + 1)) << 8
                    | u16::from(cpu.ram().borrow().read(RESET_VECTOR));
                cpu.pc_mut(addr);
            }
            cpu.set_debug(Some(state.debug.clone()));
            cpu.debug();
            cpu.set_debug(None);
            let _ = cpu.disassemble(
                &mut std::cell::RefCell::<_>::borrow_mut(&state.debug.state).dis,
                cpu.pc(),
                &*cpu.ram().borrow(),
                false,
            );
            state
                .cpucommandresptx
                .send(Ok(CommandResponse::Load(Box::new(
                    state.debug.state.borrow().clone(),
                ))))?;
        }
        Err(e) => {
            state
                .cpucommandresptx
                .send(Err(eyre!("can't read file: {e}")))?;
            return Ok(());
        }
    }
    Ok(())
}

fn cpu_dump(cpu: &mut dyn CPU, state: &mut CPULoopState, file: &str) -> Result<()> {
    let mut r = [0; MAX_SIZE];
    cpu.ram().borrow().ram(&mut r);
    match write(Path::new(&file), r) {
        Ok(()) => {
            state.cpucommandresptx.send(Ok(CommandResponse::Dump))?;
        }
        Err(e) => {
            state
                .cpucommandresptx
                .send(Err(eyre!("can't write file: {e}")))?;
        }
    }
    Ok(())
}

fn cpu_pc(cpu: &mut dyn CPU, state: &mut CPULoopState, addr: &Location) -> Result<()> {
    cpu.pc_mut(addr.addr);
    cpu.set_debug(Some(state.debug.clone()));
    cpu.debug();
    cpu.set_debug(None);
    let _ = cpu.disassemble(
        &mut std::cell::RefCell::<_>::borrow_mut(&state.debug.state).dis,
        cpu.pc(),
        &*cpu.ram().borrow(),
        false,
    );
    state
        .cpucommandresptx
        .send(Ok(CommandResponse::PC(Box::new(
            state.debug.state.borrow().clone(),
        ))))?;
    Ok(())
}

fn cpu_reset(cpu: &mut dyn CPU, state: &mut CPULoopState) -> Result<()> {
    if !state.is_init {
        state.is_init = true;
        cpu.power_on()?;
        // Power on does a reset so we don't have to do it again below.
        cpu.set_debug(Some(state.debug.clone()));
        cpu.debug();
        cpu.set_debug(None);
        let _ = cpu.disassemble(
            &mut std::cell::RefCell::<_>::borrow_mut(&state.debug.state).dis,
            cpu.pc(),
            &*cpu.ram().borrow(),
            false,
        );
        state
            .cpucommandresptx
            .send(Ok(CommandResponse::Reset(Box::new(
                state.debug.state.borrow().clone(),
            ))))?;
        return Ok(());
    }
    reset(cpu, state.cpucommandresptx)?;
    cpu.set_debug(Some(state.debug.clone()));
    cpu.debug();
    cpu.set_debug(None);
    let _ = cpu.disassemble(
        &mut std::cell::RefCell::<_>::borrow_mut(&state.debug.state).dis,
        cpu.pc(),
        &*cpu.ram().borrow(),
        false,
    );
    state
        .cpucommandresptx
        .send(Ok(CommandResponse::Reset(Box::new(
            state.debug.state.borrow().clone(),
        ))))?;
    Ok(())
}
