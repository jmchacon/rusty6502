use rusty6502::prelude::*;

#[derive(Debug)]
pub(crate) enum Command {
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
    Load(String, Option<Location>),
    Dump(String),
    PC(Location),
    Reset,
}

#[derive(Clone, Debug)]
pub(crate) struct Location {
    pub(crate) addr: u16,
}

#[derive(Debug)]
pub(crate) struct LocationRange {
    pub(crate) addr: u16,
    pub(crate) len: Option<u16>,
}

#[derive(Debug)]
pub(crate) struct Val {
    pub(crate) val: u8,
}

#[derive(Debug, PartialEq)]
pub(crate) enum StopReason {
    Step,
    Tick,
    Break(u16),
    Watch(u16),
    Stop,
    None,
}

#[derive(Debug)]
pub(crate) struct Stop {
    pub(crate) state: CPUState,
    pub(crate) reason: StopReason,
}

#[derive(Debug)]
pub(crate) enum CommandResponse {
    // NOTE: There is no Run response as this will never return
    //       directly from that state. It will either hit a break, watch,
    //       get a new Stop command or an error.
    //Error(Result<()>),
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
    Load,
    Dump,
    PC(CPUState),
    Reset(CPUState),
}
