use rusty6502::prelude::*;

#[derive(Debug)]
pub enum Command {
    Run,
    Stop,
    Break(Location),
    BreakList,
    DeleteBreakpoint(usize),
    Step,
    Tick,
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

#[derive(Debug, PartialEq)]
pub enum StopReason {
    //Run,
    Step,
    Tick,
    Break(Location),
    Watch(PC, Location),
    Stop,
    None,
}

#[derive(Debug)]
pub struct Stop {
    pub state: CPUState,
    pub reason: StopReason,
}

#[derive(Debug)]
pub enum CommandResponse {
    // NOTE: There is no Run response as this will never return
    //       directly from that state. It will either hit a break, watch,
    //       get a new Stop command or an error.
    Stop(Stop),
    Break,
    BreakList(Vec<Location>),
    DeleteBreakpoint,
    Step(Stop),
    Tick(Stop),
    Read(Val),
    ReadRange(Vec<Val>),
    Write,
    WriteRange,
    Cpu(CPUState),
    Ram([u8; MAX_SIZE]),
    Disassemble(String),
    DisassembleRange(Vec<String>),
    Watch,
    WatchList(Vec<Location>),
    DeleteWatchpoint,
    Load(CPUState),
    Dump,
    PC(CPUState),
    Reset(CPUState),
}
