use rusty6502::prelude::*;
use std::fmt;
use strum_macros::Display;

#[derive(Debug)]
pub enum Command {
    // Bool indicates whether to snapshot RAM on each instruction (expensive).
    Run(bool),
    Stop,
    Break(Location),
    BreakList,
    DeleteBreakpoint(usize),
    // Bool indicates whether to snapshot RAM on each instruction (expensive).
    Step(bool),
    // Step N instructions and return the last X
    StepN(StepN),
    // Bool indicates whether to snapshot RAM on each instruction (expensive).
    Tick(bool),
    Read(Location),
    ReadRange(LocationRange),
    Write(Location, Val),
    WriteRange(LocationRange, Val),
    Cpu,
    Ram,
    Disassemble(Location),
    DisassembleRange(LocationRange),
    Watch(Location),
    WatchList,
    DeleteWatchpoint(usize),
    Load(String, Option<Location>, Option<PC>),
    Dump(String),
    PC(Location),
    Reset,
}

#[derive(Debug)]
pub struct StepN {
    pub reps: usize,
    pub capture: Vec<CPUState>,
    pub ram: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Location {
    pub addr: u16,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PC {
    pub addr: u16,
}

#[derive(Debug)]
pub struct LocationRange {
    pub addr: u16,
    pub len: Option<u16>,
}

#[derive(Debug)]
pub struct Val {
    pub val: u8,
}

#[derive(Clone, Debug, Display, PartialEq)]
pub enum StopReason {
    Run,
    Step,
    Tick,
    Break(Location),
    Watch(PC, Location, String),
    Stop,
    None,
}

#[derive(Debug)]
pub struct Stop {
    pub state: Box<CPUState>,
    pub reason: StopReason,
}

impl fmt::Display for Stop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Reason: {}", self.reason)?;
        writeln!(f, "State: {}", self.state)
    }
}

pub enum StepNReason {
    Stop(Box<Stop>),
    StepN(Vec<CPUState>),
}

#[derive(Display)]
pub enum CommandResponse {
    // NOTE: There is no Run response as it uses Stop to indicate updates.
    //       It will either hit a break, watch, get a new Stop command or an error.
    Stop(Box<Stop>),
    Break,
    BreakList(Vec<Location>),
    DeleteBreakpoint,
    Step(Box<Stop>),
    StepN(StepNReason),
    Tick(Box<Stop>),
    Read(Val),
    ReadRange(Vec<Val>),
    Write,
    WriteRange,
    Cpu(Box<CPUState>),
    Ram(Box<[u8; MAX_SIZE]>),
    Disassemble(String),
    DisassembleRange(Vec<String>),
    Watch,
    WatchList(Vec<Location>),
    DeleteWatchpoint,
    Load(Box<CPUState>),
    Dump,
    PC(Box<CPUState>),
    Reset(Box<CPUState>),
}

impl fmt::Debug for CommandResponse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CommandResponse::Ram(r) => {
                // Handle ram directly since we want to use the formatter in Memory to print this.
                let r = &((r.as_ref()) as &dyn Memory) as &dyn std::fmt::Debug;
                write!(f, "{r:?}")
            }
            _ => write!(f, "{self}"),
        }
    }
}
