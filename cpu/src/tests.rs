use crate::{
    AddressMode, CPU65C02Rockwell, CPUCMOSInternal, CPUDebug, CPUError, CPUInternal,
    CPUNMOSInternal, CPURicoh, CPUState, ChipDef, Flags, FlatRAM, InstructionMode, InterruptState,
    InterruptStyle, LastBusAction, OpState, Opcode, Register, ResetTick, State, Tick, Vectors, CPU,
    CPU6502, CPU6510, CPU65C02, CPU65SC02, P_B, P_CARRY, P_DECIMAL, P_INTERRUPT, P_NEGATIVE,
    P_OVERFLOW, P_S1, P_ZERO, STACK_START,
};
use chip::{CPUType, Chip};
use color_eyre::eyre::{eyre, Result};
use irq::Sender;
use memory::{Memory, MAX_SIZE};
use serde::Deserialize;
use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::{self, Display, Write};
use std::fs::{read, File};
use std::io::{BufRead, BufReader, Read};
use std::num::Wrapping;
use std::ops::Deref;
use std::panic;
use std::path::Path;
use std::rc::Rc;

// Debug provides a way to capture debug output from a CPU
// without having to grab everything. Normally a few recent
// instructions is all one needs. The functional test below for
// instance runs millions of instructions which can lead to very
// large buffering during a test run.
//
// As grabbing a memory footprint can be expensive optionally
// define how often to do this.
#[derive(Clone)]
struct Debug<T> {
    state: Vec<Rc<RefCell<T>>>,
    cur: RefCell<usize>,
    wrapped: RefCell<bool>,
    full_dump_every_n: Option<usize>,
    count: RefCell<usize>,
}

impl<T: Display + std::default::Default> Debug<T> {
    fn new(cap: usize, full_dump_every_n: Option<usize>) -> Self {
        let mut d = Self {
            state: Vec::with_capacity(cap),
            cur: RefCell::new(0),
            wrapped: RefCell::new(false),
            full_dump_every_n,
            count: RefCell::new(0),
        };
        for _ in 0..cap {
            d.state.push(Rc::new(RefCell::new(T::default())));
        }
        d
    }
    fn roll_buffer(&self) {
        // Now roll the circular buffer portion of this.
        let mut new = *self.cur.borrow() + 1;
        if new >= self.state.len() {
            *self.wrapped.borrow_mut() = true;
            new = 0;
        }
        *self.cur.borrow_mut() = new;
    }
    fn reset(&self) {
        *self.cur.borrow_mut() = 0;
        *self.wrapped.borrow_mut() = false;
        *self.count.borrow_mut() = 0;
    }
}

impl Debug<CPUState> {
    fn dump(&self, s: &str, cpu: &dyn CPU) -> String {
        let mut out = String::new();
        writeln!(out, "\nFAIL: {s}\n\nExecution buffer:\n").unwrap();
        if *self.wrapped.borrow() {
            for i in *self.cur.borrow()..self.state.len() {
                let mut s = String::with_capacity(32);
                {
                    // Late bind disassembly since this is expensive to
                    // generate the String. So only do it on actual dumps.
                    let pc = self.state[i].borrow().pc;
                    let r = &self.state[i].borrow().ram;
                    _ = cpu.disassemble(&mut s, pc, r, false);
                }
                self.state[i].borrow_mut().dis = s;
                writeln!(out, "{}", self.state[i].borrow()).unwrap();
            }
        }
        for i in 0..*self.cur.borrow() {
            let mut s = String::with_capacity(32);
            {
                // Late bind disassembly since this is expensive to
                // generate the String. So only do it on actual dumps.
                let pc = self.state[i].borrow().pc;
                let r = &self.state[i].borrow().ram;
                _ = cpu.disassemble(&mut s, pc, r, false);
            }
            self.state[i].borrow_mut().dis = s;
            writeln!(out, "{}", self.state[i].borrow()).unwrap();
        }
        out
    }
}

impl CPUDebug for Debug<CPUState> {
    fn get_debug(&self) -> (Rc<RefCell<CPUState>>, bool) {
        let ret = Rc::clone(&self.state[*self.cur.borrow()]);

        // Now roll the circular buffer portion of this.
        self.roll_buffer();
        let mut mem = false;
        if let Some(n) = self.full_dump_every_n {
            *self.count.borrow_mut() += 1;
            if (*self.count.borrow() % n) == 0 {
                mem = true;
            }
        }
        (ret, mem)
    }
}

// tester is a wrapper around assert which is passed a `Debug` so any failure
// will also print out the contents of the circular buffer prefixed by the error expression.
macro_rules! tester {
    ($test:expr, $dumper:ident, $cpu:expr, $error:expr) => {
        assert!($test, "{}", $dumper.dump(&format!($error), $cpu))
    };
}

const RESET: u16 = 0x1FFE;
const IRQ_ADDR: u16 = 0xD001;

macro_rules! setup_cpu {
    ($cpu:ident, $name:ident) => {
        #[allow(non_snake_case)]
        fn $name<'a>(
            hlt: u16,
            fill: u8,
            irq: Option<&'a dyn Sender>,
            nmi: Option<&'a dyn Sender>,
            rdy: Option<&'a dyn Sender>,
            debug: Option<Box<dyn CPUDebug>>,
        ) -> $cpu<'a> {
            // This should halt the cpu if a test goes off the rails since
            // endless execution will eventually end up at the NMI vector (it's the first
            // one) which contains HLT instructions.
            let r = FlatRAM::default()
                .vectors(Vectors {
                    nmi: hlt,
                    reset: RESET,
                    irq: IRQ_ADDR,
                })
                .fill_value(fill);
            let mut r = Box::new(r);
            r.power_on();

            let def = ChipDef {
                ram: r,
                irq,
                nmi,
                rdy,
            };

            let mut cpu = $cpu::new(def);
            cpu.set_debug(debug);
            cpu
        }
    };
}

setup_cpu!(CPU6502, setup_cpu_nmos);
setup_cpu!(CPURicoh, setup_cpu_ricoh);
setup_cpu!(CPU65C02, setup_cpu_cmos);
setup_cpu!(CPU65C02Rockwell, setup_cpu_cmos_rockwell);
setup_cpu!(CPU65SC02, setup_cpu_cmos_C65SC02);

fn setup_cpu_nmos_6510<'a>(
    hlt: u16,
    fill: u8,
    irq: Option<&'a dyn Sender>,
    nmi: Option<&'a dyn Sender>,
    rdy: Option<&'a dyn Sender>,
    debug: Option<Box<dyn CPUDebug>>,
) -> CPU6510<'a> {
    // This should halt the cpu if a test goes off the rails since
    // endless execution will eventually end up at the NMI vector (it's the first
    // one) which contains HLT instructions.
    let r = FlatRAM::new()
        .vectors(Vectors {
            nmi: hlt,
            reset: RESET,
            irq: IRQ_ADDR,
        })
        .fill_value(fill);
    let mut r = Box::new(r);
    r.power_on();

    let def = ChipDef {
        ram: r,
        irq,
        nmi,
        rdy,
    };

    let mut cpu = CPU6510::new(def, None);
    cpu.set_debug(debug);
    cpu
}

macro_rules! step {
    ($cpu:ident, $name:ident) => {
        fn $name(cpu: &mut $cpu) -> Result<usize> {
            let mut cycles = 0;
            loop {
                cpu.tick()?;
                cpu.tick_done()?;
                cycles += 1;
                if cpu.op_tick == Tick::Reset {
                    break;
                }
            }
            Ok(cycles)
        }
    };
}

step!(CPU6502, step_cpu_nmos);
step!(CPURicoh, step_cpu_ricoh);
step!(CPU65C02, step_cpu_cmos);

#[test]
fn tick_next() {
    let mut ticker = Tick::default();

    assert!(ticker == Tick::Reset, "not in valid reset state");
    // Validate when we get to the end it stays on it
    loop {
        ticker = ticker.next();
        if ticker == Tick::Tick7 {
            break;
        }
    }
    ticker = ticker.next();
    assert!(ticker == Tick::Tick8, "didn't advance to tick8");
    ticker = ticker.next();
    assert!(ticker == Tick::Tick8, "didn't continue to stay in tick8");
}

#[test]
fn shallow_cpustate() {
    // Validate shallow copy skips ram.
    let mut cpustate = CPUState::default();
    let mut shallow = CPUState::default();
    cpustate.a = 0x01;
    cpustate.s = 0xCC;
    cpustate.ram[0x1234] = 0x56;
    cpustate.shallow_copy(&mut shallow);
    assert!(
        shallow.a == 0x01,
        "A differs: orig {} new {}",
        cpustate.a,
        shallow.a
    );
    assert!(
        shallow.s == 0xCC,
        "S differs: orig {} new {}",
        cpustate.s,
        shallow.s
    );
    assert!(
        shallow.ram[0x1234] != cpustate.ram[0x1234],
        "RAM wrong should be different - orig: {:02X} new {:02X}",
        cpustate.ram[0x1234],
        shallow.ram[0x1234],
    );
}

#[test]
#[allow(clippy::too_many_lines)]
fn invalid_states() -> Result<()> {
    // These tests are a little ridiculous but eliminating that one place
    // of no coverage makes it easier to spots real issues.
    let a = Register::A;
    #[allow(clippy::clone_on_copy)]
    let x = a.clone();
    assert!(x.to_string() == "A", "Clone didn't work for register?");

    let cpu = CPUType::NMOS;
    #[allow(clippy::clone_on_copy)]
    let cpucopy = cpu.clone();
    assert!(
        cpucopy.to_string() == "NMOS",
        "Clone didn't work for CPUType?"
    );

    let tick = ResetTick::Tick1;
    #[allow(clippy::clone_on_copy)]
    let tickcopy = tick.clone();
    assert!(
        tick == tickcopy,
        "ticks differ - {tick:?} vs copy {tickcopy:?}"
    );
    println!("{tick:?} {tickcopy:?}");
    let cpu = CPUState::default();
    let _ = cpu.clone();

    let d = Box::new(Debug::<CPUState>::new(128, Some(1)));

    let nmi_addr = 0x1212;
    let mut cmos_cpu = setup_cpu_cmos(
        nmi_addr,
        0xEA,
        None,
        None,
        None, // RDY doesn't matter here
        Some(d.clone()),
    );
    cmos_cpu.power_on()?;
    let mut c65cs02 = setup_cpu_cmos_C65SC02(
        nmi_addr,
        0xEA,
        None,
        None,
        None, // RDY doesn't matter here
        Some(d.clone()),
    );
    c65cs02.power_on()?;
    let mut rockwell = setup_cpu_cmos_rockwell(
        nmi_addr,
        0xEA,
        None,
        None,
        None, // RDY doesn't matter here
        Some(d.clone()),
    );
    rockwell.power_on()?;
    let mut nmos = setup_cpu_nmos(
        nmi_addr,
        0xEA,
        None,
        None,
        None, // RDY doesn't matter here
        Some(d),
    );
    nmos.power_on()?;

    // Test the CPU's don't resolve each others opcodes
    let e = cmos_cpu.resolve_opcode(&Opcode::AHX, &AddressMode::IndirectY);
    assert!(e.is_err(), "CMOS resolved AHX?");
    #[allow(clippy::unwrap_used)]
    let errs = e.as_ref().err().unwrap().to_string();
    assert!(
        errs.contains("invalid opcode AHX for"),
        "CMOS - Wrong error for invalid op? - {e:?}"
    );

    let e = c65cs02.resolve_opcode(&Opcode::AHX, &AddressMode::IndirectY);
    assert!(e.is_err(), "CMOS 65CS02 resolved AHX?");
    #[allow(clippy::unwrap_used)]
    let errs = e.as_ref().err().unwrap().to_string();
    assert!(
        errs.contains("invalid opcode AHX for"),
        "CMOS 65CS02 - Wrong error for invalid op? - {e:?}"
    );

    let e = rockwell.resolve_opcode(&Opcode::AHX, &AddressMode::IndirectY);
    assert!(e.is_err(), "Rockwell resolved AHX?");
    #[allow(clippy::unwrap_used)]
    let errs = e.as_ref().err().unwrap().to_string();
    assert!(
        errs.contains("invalid opcode AHX for"),
        "Rockwell - Wrong error for invalid op? - {e:?}"
    );

    let e = nmos.resolve_opcode(&Opcode::BBR, &AddressMode::ZeroPageRelative);
    assert!(e.is_err(), "NMOS resolved BBR?");
    #[allow(clippy::unwrap_used)]
    let errs = e.as_ref().err().unwrap().to_string();
    assert!(
        errs.contains("invalid opcode BBR for"),
        "NMOS - Wrong error for invalid op? - {e:?}"
    );

    // Make sure the bad states in reset error.
    cmos_cpu.reset()?;

    cmos_cpu.reset_tick = ResetTick::Tick3;
    cmos_cpu.op_tick = Tick::Tick8;
    let ret = cmos_cpu.reset();
    let Err(errs) = ret else {
        panic!("CMOS: didn't get op_tick reset error?");
    };
    assert!(
        errs.to_string().contains("invalid op_tick in reset"),
        "CMOS: wrong error for invalid op_tick: {errs}"
    );

    nmos.reset()?;
    nmos.reset_tick = ResetTick::Tick3;
    nmos.op_tick = Tick::Tick8;
    let ret = nmos.reset();
    let Err(errs) = ret else {
        panic!("NMOS: didn't get op_tick reset error?");
    };
    assert!(
        errs.to_string().contains("invalid op_tick in reset"),
        "NMOS: wrong error for invalid op_tick: {errs}"
    );

    // Make sure invalid combos aren't somehow present in our processing.
    cmos_cpu.op.op = Opcode::NOP;
    cmos_cpu.op.mode = AddressMode::Relative;
    let ret = cmos_cpu.process_opcode();
    assert!(ret.is_err(), "No error for a relative NOP in CMOS? {ret:?}");

    nmos.op.op = Opcode::NOP;
    nmos.op.mode = AddressMode::Relative;
    let ret = nmos.process_opcode();
    assert!(ret.is_err(), "No error for a relative NOP in NMOS? {ret:?}");

    // Now go through all the various modes/instructions getting an error
    cmos_cpu.op_tick = Tick::Tick1;
    let ret = cmos_cpu.addr_absolute(&InstructionMode::Load);
    assert!(
        ret.is_err(),
        "didn't get an error for addr mode func addr_absolute"
    );
    let ret = nmos.addr_absolute(&InstructionMode::Load);
    assert!(
        ret.is_err(),
        "didn't get an error for NMOS addr mode func addr_absolute"
    );
    let ret = cmos_cpu.addr_absolute_x(&InstructionMode::Load);
    assert!(
        ret.is_err(),
        "didn't get an error for addr mode func addr_absolute_x"
    );
    let ret = nmos.addr_absolute_x(&InstructionMode::Load);
    assert!(
        ret.is_err(),
        "didn't get an error for NMOS addr mode func addr_absolute_x"
    );
    let ret = cmos_cpu.addr_immediate(&InstructionMode::Load);
    assert!(
        ret.is_err(),
        "didn't get an error for addr mode func addr_immediate"
    );
    let ret = nmos.addr_immediate(&InstructionMode::Load);
    assert!(
        ret.is_err(),
        "didn't get an error for NMOS addr mode func addr_immediate"
    );
    let ret = cmos_cpu.addr_indirect_x(&InstructionMode::Load);
    assert!(
        ret.is_err(),
        "didn't get an error for addr mode func addr_indirect_x"
    );
    let ret = nmos.addr_indirect_x(&InstructionMode::Load);
    assert!(
        ret.is_err(),
        "didn't get an error for NMOS addr mode func addr_indirect_x"
    );
    let ret = cmos_cpu.addr_indirect(&InstructionMode::Load);
    assert!(
        ret.is_err(),
        "didn't get an error for addr mode func addr_indirect"
    );
    let ret = cmos_cpu.addr_indirect_y(&InstructionMode::Load);
    assert!(
        ret.is_err(),
        "didn't get an error for addr mode func addr_indirect_y"
    );
    let ret = nmos.addr_indirect_y(&InstructionMode::Load);
    assert!(
        ret.is_err(),
        "didn't get an error for NMOS addr mode func addr_indirect_y"
    );
    let ret = cmos_cpu.addr_zp(&InstructionMode::Load);
    assert!(
        ret.is_err(),
        "didn't get an error for addr mode func addr_zp"
    );
    let ret = nmos.addr_zp(&InstructionMode::Load);
    assert!(
        ret.is_err(),
        "didn't get an error for NMOS addr mode func addr_zp"
    );
    let ret = cmos_cpu.addr_zp_x(&InstructionMode::Load);
    assert!(
        ret.is_err(),
        "didn't get an error for addr mode func addr_zp_x"
    );
    let ret = nmos.addr_zp_x(&InstructionMode::Load);
    assert!(
        ret.is_err(),
        "didn't get an error for NMOS addr mode func addr_zp_x"
    );
    let ret = nmos.hlt();
    assert!(ret.is_err(), "didn't get an error for NMOS HLT");

    let ret = cmos_cpu.perform_branch();
    assert!(ret.is_err(), "didn't get an error for perform_branch");
    let ret = cmos_cpu.branch_nop();
    assert!(ret.is_err(), "didn't get an error for branch_nop");
    let ret = cmos_cpu.run_interrupt(0x1234, false);
    assert!(ret.is_err(), "didn't get an error for run_interrupt");
    let ret = nmos.run_interrupt(0x1234, false);
    assert!(ret.is_err(), "didn't get an error for NMOS run_interrupt");

    let ret = cmos_cpu.bbr(1);
    assert!(ret.is_err(), "didn't get an error for bbr1");

    let ret = cmos_cpu.jmp();
    assert!(ret.is_err(), "didn't get an error for jmp");
    let ret = cmos_cpu.jmp_indirect();
    assert!(ret.is_err(), "didn't get an error for jmp_indirect");
    let ret = nmos.jmp_indirect();
    assert!(ret.is_err(), "didn't get an error for NMOS jmp_indirect");
    let ret = cmos_cpu.jmp_indirect_x();
    assert!(ret.is_err(), "didn't get an error for jmp_indirect_x");
    let ret = cmos_cpu.jsr();
    assert!(ret.is_err(), "didn't get an error for jsr");
    let ret = cmos_cpu.rts();
    assert!(ret.is_err(), "didn't get an error for rts");
    let ret = cmos_cpu.rti();
    assert!(ret.is_err(), "didn't get an error for rti");

    let ret = cmos_cpu.nop8();
    assert!(ret.is_err(), "didn't get an error for nop8");

    let ret = cmos_cpu.pha();
    assert!(ret.is_err(), "didn't get an error for pha");
    let ret = cmos_cpu.phx();
    assert!(ret.is_err(), "didn't get an error for phx");
    let ret = cmos_cpu.phy();
    assert!(ret.is_err(), "didn't get an error for phy");
    let ret = cmos_cpu.php();
    assert!(ret.is_err(), "didn't get an error for php");
    let ret = cmos_cpu.pla();
    assert!(ret.is_err(), "didn't get an error for pla");
    let ret = cmos_cpu.plx();
    assert!(ret.is_err(), "didn't get an error for plx");
    let ret = cmos_cpu.ply();
    assert!(ret.is_err(), "didn't get an error for ply");
    let ret = cmos_cpu.plp();
    assert!(ret.is_err(), "didn't get an error for plp");

    let ret = cmos_cpu.wai();
    assert!(ret.is_err(), "didn't get an error for wai");

    Ok(())
}

#[test]
fn wai_test() -> Result<()> {
    let d = Box::new(Debug::<CPUState>::new(128, Some(1)));

    let irq = Irq {
        raised: RefCell::new(false),
    };
    let nmi = Irq {
        raised: RefCell::new(false),
    };
    let nmi_addr = 0x1212;
    let mut cpu = setup_cpu_cmos(
        nmi_addr,
        0xEA,
        Some(&irq),
        Some(&nmi),
        None, // RDY doesn't matter here
        Some(d),
    );
    cpu.power_on()?;

    cpu.ram.borrow_mut().write(0x1FFE, 0xA9); // LDA #A1
    cpu.ram.borrow_mut().write(0x1FFF, 0xA1);
    cpu.ram.borrow_mut().write(0x2000, 0xCB); // WAI
    cpu.ram.borrow_mut().write(0x2001, 0x1A); // INC

    // Run to WAI
    step_cpu_cmos(&mut cpu)?;
    step_cpu_cmos(&mut cpu)?;
    let p = cpu.pc.0;
    assert!(p == 0x2001, "PC after WAI not 0x2001 - is {p:#06X}");
    let state = cpu.state;
    assert!(
        state == State::WaitingForInterrupt,
        "State not WAI is {state}"
    );

    // Show no time advances
    let c = cpu.clocks;
    cpu.tick()?;
    let n = cpu.clocks;
    assert!(c == n, "Clocks advanced from {c} to {n} during WAI");

    // Set I
    cpu.p |= P_INTERRUPT;

    // Trigger IRQ
    *irq.raised.borrow_mut() = true;

    // Check PC == INC
    step_cpu_cmos(&mut cpu)?;
    *irq.raised.borrow_mut() = false;
    let p = cpu.pc.0;
    assert!(
        p == 0x2002,
        "PC after WAI unpauses not 0x2002 after running - is {p:#06X}"
    );

    // Reset PC to WAI and run to it.
    cpu.pc.0 = 0x2000;
    step_cpu_cmos(&mut cpu)?;
    let p = cpu.pc.0;
    assert!(p == 0x2001, "PC after WAI not 0x2001 - is {p:#06X}");
    let state = cpu.state;
    assert!(
        state == State::WaitingForInterrupt,
        "State not WAI is {state}"
    );

    // Show no time advances
    let c = cpu.clocks;
    println!("cpu - \n{cpu:?}");
    cpu.tick()?;
    let n = cpu.clocks;
    assert!(
        c == n,
        "Clocks advanced from {c} to {n} during WAI - \n{cpu:?}"
    );

    // Clear I
    cpu.p &= !P_INTERRUPT;

    // Trigger IRQ
    *irq.raised.borrow_mut() = true;

    // Check PC == IRQ vector
    step_cpu_cmos(&mut cpu)?;
    let p = cpu.pc.0;
    assert!(
        p == IRQ_ADDR,
        "PC after WAI not {IRQ_ADDR:#06X} - is {p:#06X}"
    );
    *irq.raised.borrow_mut() = false;

    // Reset PC to WAI and run to it.
    cpu.pc.0 = 0x2000;
    step_cpu_cmos(&mut cpu)?;
    let p = cpu.pc.0;
    assert!(p == 0x2001, "PC after WAI not 0x2001 - is {p:#06X}");
    let state = cpu.state;
    assert!(
        state == State::WaitingForInterrupt,
        "State not WAI is {state}"
    );

    // Show no time advances
    let c = cpu.clocks;
    println!("cpu - \n{cpu:?}");
    cpu.tick()?;
    let n = cpu.clocks;
    assert!(
        c == n,
        "Clocks advanced from {c} to {n} during WAI - \n{cpu:?}"
    );

    // Set I
    cpu.p |= P_INTERRUPT;

    // Trigger NMI
    *nmi.raised.borrow_mut() = true;

    // Check PC == NMI vector
    step_cpu_cmos(&mut cpu)?;
    let p = cpu.pc.0;
    assert!(
        p == nmi_addr,
        "PC after WAI not {nmi_addr:#06X} - is {p:#06X}"
    );
    Ok(())
}

#[test]
fn disassemble_test() -> Result<()> {
    let cpus = vec![
        (
            "CMOS",
            Box::new(setup_cpu_cmos(0x1212, 0xEA, None, None, None, None)) as Box<dyn CPU>,
            // The main tests use CMOS since it's the superset of all modes
            vec![
                (vec![0xA9, 0x69], "LDA #$69"),                    // Immediate
                (vec![0xA5, 0x69], "LDA $69"),                     // ZP
                (vec![0xB5, 0x69], "LDA $69,X"),                   // ZPX
                (vec![0xB6, 0x69], "LDX $69,Y"),                   // ZPY
                (vec![0xB2, 0x69], "LDA ($69)"),                   // Indirect
                (vec![0xA1, 0x69], "LDA ($69,X)"),                 // Indirect X
                (vec![0xB1, 0x69], "LDA ($69),Y"),                 // Indirect Y
                (vec![0xAD, 0x69, 0x55], "LDA $5569"),             // Absolute
                (vec![0xBD, 0x69, 0x55], "LDA $5569,X"),           // Absolute X
                (vec![0xB9, 0x69, 0x55], "LDA $5569,Y"),           // Absolute Y
                (vec![0x6C, 0x69, 0x55], "JMP ($5569)"),           // Absolute Indirect
                (vec![0x7C, 0x69, 0x55], "JMP ($5569,X)"),         // Absolute Indirect
                (vec![0x1A], "INC"),                               // Implied
                (vec![0x03], "NOP"),                               // NOPCmos
                (vec![0x5C, 0x34, 0x12], "NOP $1234"),             // Absolute NOP
                (vec![0x80, 0x69], "BRA $69 ($006A)"),             // Relative,
                (vec![0x1F, 0x55, 0x69], "BBR 1,$55,$69 ($006A)"), // Zero Page Relative
                (vec![0xCB], "WAI"), // Implied but needed for WDC specific testing.
                (vec![0xDB], "STP"), // Implied but needed for WDC specific testing.
            ],
        ),
        (
            "Rockwell",
            Box::new(setup_cpu_cmos_rockwell(
                0x1212, 0xEA, None, None, None, None,
            )) as Box<dyn CPU>,
            vec![
                (vec![0xCB], "NOP"),                               // Implied
                (vec![0xDB, 0xFA], "NOP $FA,X"),                   // ZPX
                (vec![0x1F, 0x55, 0x69], "BBR 1,$55,$69 ($006A)"), // Zero Page Relative
            ],
        ),
        (
            "C65SC02",
            Box::new(setup_cpu_cmos_C65SC02(0x1212, 0xEA, None, None, None, None)) as Box<dyn CPU>,
            vec![
                (vec![0xCB], "NOP"),                   // Implied
                (vec![0xDB, 0xFA], "NOP $FA,X"),       // ZPX
                (vec![0x1F, 0x55, 0x69], "NOP $6955"), // Absolute NOP
            ],
        ),
    ];

    for (name, cpu, tests) in cpus {
        let mut s = String::with_capacity(32);

        // Set addr at end of RAM to test wrapping.
        let addr = Wrapping(0xFFFF);
        for t in &tests {
            #[allow(clippy::unwrap_used)]
            let num = u16::try_from(t.0.len()).unwrap();
            let mut out = format!("{addr:04X} ");
            for l in 0..num {
                // Assemble test bytes.
                let b = t.0[usize::from(l)];
                cpu.ram().borrow_mut().write((addr + Wrapping(l)).0, b);
                write!(out, "{b:02X} ")?;
            }
            match num {
                1 => write!(out, "        ")?,
                2 => write!(out, "     ")?,
                3 => write!(out, "  ")?,
                _ => panic!("odd vec?"),
            }
            write!(out, "{}", t.1)?;

            cpu.disassemble(&mut s, addr.0, &*cpu.ram().borrow(), false);
            assert!(
                s == out,
                "{name}: Expected output\n{out}\n\nDoesn't match\n{s}"
            );
            cpu.disassemble(&mut s, addr.0, &*cpu.ram().borrow(), true);
            assert!(
                s == t.1,
                "{name}: Expected output (op only)\n{out}\n\nDoesn't match\n{s}"
            );
        }
    }

    Ok(())
}

#[test]
fn flags_test() {
    let mut f = Flags::default();
    assert!(f == P_S1, "Flags default not S1");

    let b = P_B.0;
    let flags_b = P_B;

    assert!(f | flags_b == (P_S1 | P_B), "Flags not BitOr Self");
    #[allow(clippy::op_ref)]
    let x = f | &b;
    assert!(x == (P_S1 | P_B), "Flags not BitOr &u8");
    assert!(f | b == (P_S1 | P_B), "Flags not BitOr u8");
    assert!(b | f == (P_S1 | P_B), "u8 not BitOr Flags");

    f |= flags_b;
    assert!(f == (P_S1 | P_B), "Flags BitOrAssign Self");
    f = Flags::default();

    f |= &b;
    assert!(f == (P_S1 | P_B), "Flags BitOrAssign &u8");
    f = Flags::default();

    f |= b;
    assert!(f == (P_S1 | P_B), "Flags BitOrAssign u8");
    f = Flags::default();

    let carry = P_CARRY.0;
    let flags_carry = P_CARRY;

    f |= b | carry;
    assert!(f & flags_carry == P_CARRY, "Flags not BitAnd Self");
    #[allow(clippy::op_ref)]
    let x = f & &carry;
    assert!(x == P_CARRY, "Flags not BitAnd &u8");
    assert!(f & carry == P_CARRY, "Flags not BitAnd u8");
    assert!(carry & f == P_CARRY, "u8 not BitAnd Flags");

    f &= flags_carry;
    assert!(f == P_CARRY, "Flags BitAndAssign Self");
    f = Flags::default();

    f |= b | carry;
    f &= &carry;
    assert!(f == P_CARRY, "Flags BitAndAssign &u8");
    f = Flags::default();

    f |= b | carry;
    f &= carry;
    assert!(f == P_CARRY, "Flags BitAndAssign u8");
    f = Flags::default();

    f |= b | carry;
    assert!(!f == (P_NEGATIVE | P_OVERFLOW | P_DECIMAL | P_INTERRUPT | P_ZERO));

    f = Flags::default();
    println!("Flags: {f} - debug - {f:?}");
    f = !f;
    println!("Flags: {f} - debug - {f:?}");
}

#[test]
#[allow(clippy::too_many_lines)]
fn c6510_io_tests() {
    // Mostly a copy from setup() but we want to define I/O here which we generally
    // never need.
    let r = FlatRAM::new()
        .vectors(Vectors {
            nmi: 0x1212,
            reset: 0x1212,
            irq: 0x1212,
        })
        .fill_value(0x12)
        .debug();
    let mut r = Box::new(r);
    r.power_on();

    r.write(0x00, 0x12);
    r.write(0x01, 0x34);
    println!("mem: {r:?}");

    let def = ChipDef {
        ram: r,
        irq: None,
        nmi: None,
        rdy: None,
    };

    #[allow(clippy::clone_on_copy)]
    let mut cpu = CPU6510::new(
        def,
        Some([
            io::Style::In(&io::Pullup {}).clone(),
            io::Style::In(&io::Pullup {}),
            io::Style::In(&io::Pullup {}),
            io::Style::In(&io::Pullup {}),
            io::Style::In(&io::Pullup {}),
            io::Style::In(&io::Pulldown {}),
        ]),
    );
    assert!(cpu.power_on().is_ok(), "CPU can't power on");

    cpu.ram.borrow_mut().write(0x1234, 0x56);
    let ret = cpu.ram.borrow().read(0x1234);
    assert!(
        ret == 0x56,
        "C6510 RAM not working. Expected 0x56 at 0x1234 and got {ret}"
    );

    // Verify we start as all pullups on 0-4, pulldown on 5 and as inputs.
    for i in 0..6 {
        match cpu.io_pin(i) {
            Ok(io) => match io {
                io::Style::In(p) => {
                    if i < 5 {
                        assert!(p.input(), "i/o input {i} not high");
                    } else {
                        assert!(!p.input(), "i/o input 5 not low");
                    }
                }
                io::Style::Out(_) => panic!("pin set to output?"),
            },
            Err(e) => panic!("io_pin returned error: {e:?}"),
        }
    }

    // Validate out of range i/o pin
    assert!(cpu.io_pin(6).is_err(), "I/O pin for 6 didn't error");

    // Now set the data direction register to all output and validate it changed
    // type and that they are all low (since they default to 0 in the reg)
    cpu.ram.borrow_mut().write(0x00, 0xFF);
    for i in 0..6 {
        match cpu.io_pin(i) {
            Ok(io) => match io {
                io::Style::Out(p) => assert!(!p.output(), "i/o output not low?"),
                io::Style::In(_) => panic!("pin set to output?"),
            },
            Err(e) => panic!("io_pin returned error: {e:?}"),
        }
    }

    // Now set the output bits all high and verify type and state confirm
    cpu.ram.borrow_mut().write(0x01, 0xFF);
    for i in 0..6 {
        match cpu.io_pin(i) {
            Ok(io) => match io {
                io::Style::Out(p) => assert!(p.output(), "i/o output not high?"),
                io::Style::In(_) => panic!("pin set to output?"),
            },
            Err(e) => panic!("io_pin returned error: {e:?}"),
        }
    }

    // Make all pins high except the last one
    cpu.ram.borrow_mut().write(0x01, 0xFF - 0x20);
    // Set direction back to input for first 3 bits
    cpu.ram.borrow_mut().write(0x00, 0xF8);
    // Write it a 2nd time for coverage to make sure we skip inputs.
    cpu.ram.borrow_mut().write(0x01, 0xDF);
    // Should be high on the first 3 pins as input.
    // Then high on next 2 pins as outputs and finally last pin as low as output.
    for i in 0..6 {
        match cpu.io_pin(i) {
            Ok(io) => match io {
                io::Style::In(p) => {
                    if i < 3 {
                        assert!(p.input(), "i/o input dual/mode not high?");
                    } else {
                        panic!("Pin {i} not an input");
                    }
                }
                io::Style::Out(p) => match i {
                    0..=2 => panic!("Pin {i} not an output"),
                    3 | 4 => assert!(p.output(), "i/o output dual/mode 3/4 not high?"),
                    5 => assert!(!p.output(), "i/o output dual/mode 5 not low?"),
                    _ => {}
                },
            },
            Err(e) => panic!("io_pin returned error: {e:?}"),
        }
    }

    let mut ram = [0; MAX_SIZE];
    cpu.ram.borrow().ram(&mut ram);
    let io0 = ram[0x0000];
    assert!(io0 == 0xF8, "I/O register 0 not 0xF8 - is {io0:2X}");
    let io1 = ram[0x0001];
    assert!(io1 == 0xDF, "I/O register 0 not 0xDF - is {io1:2X}");
    let loc = ram[0x1234];
    assert!(loc == 0x56, "0x1234 not 0x56 - is {loc:2X}");
}

macro_rules! rdy_test {
    ($suite:ident, $($name:ident: $type:expr, $cmos:expr,)*) => {
        mod $suite {
            use super::*;

            $(
                #[test]
                fn $name() -> Result<()> {
                    let r = Irq {
                        raised: RefCell::new(false),
                    };
                    let fill = 0xEE; // INC $EEEE

                    let d = Box::new(Debug::<CPUState>::new(128, None));
                    let mut cpu = $type(0x1212, fill, None, None, Some(&r), Some(d));

                    cpu.power_on()?;
                    loop {
                        match cpu.reset() {
                            Ok(OpState::Done) => break,
                            Ok(OpState::Processing) => continue,
                            Err(e) => panic!("error from reset: {e:?}"),
                        }
                    }

                    // Pull RDY high and tick should return true but not advance
                    // after this tick is done.

                    // Get a copy of tick and advance it as that's what should
                    // happen when RDY runs once before engaging.
                    let tick = cpu.op_tick;
                    let tick = tick.next();

                    // Same with clocks which should advance once.
                    let clocks = cpu.clocks + 1;
                    *r.raised.borrow_mut() = true;
                    let ret = cpu.tick();
                    assert!(ret.is_ok(), "Tick didn't return true - RDY high - {ret:?}");
                    let ret = cpu.tick_done();
                    assert!(
                        ret.is_ok(),
                        "Tick done didn't return true - RDY high - {ret:?}"
                    );
                    assert!(tick == cpu.op_tick, "op_tick didn't advance after 1 clock with RDY high?");
                    assert!(clocks == cpu.clocks, "clocks didn't advance after 1 clock  with RDY high?");
                    assert!(cpu.state == State::RDY, "State isn't in RDY with RDY about to clock high? - {}", cpu.state);

                    // Let's run some iterations to prove it holds
                    for _ in 0..10 {
                        let ret = cpu.tick();
                        assert!(ret.is_ok(), "Tick didn't return true - RDY high - {ret:?}");
                        let ret = cpu.tick_done();
                        assert!(
                            ret.is_ok(),
                            "Tick done didn't return true - RDY high - {ret:?}"
                        );
                        assert!(tick == cpu.op_tick, "op_tick advanced with RDY high?");
                        assert!(clocks == cpu.clocks, "clocks advanced with RDY high?");
                        assert!(cpu.state == State::RDY, "State isn't in RDY with RDY high?");
                    }
                    *r.raised.borrow_mut() = false;

                    // Advance to tick 5 and assert again.

                    // Reruns tick1
                    cpu.tick()?;
                    cpu.tick_done()?;

                    // Tick2
                    cpu.tick()?;
                    cpu.tick_done()?;

                    // Tick3
                    cpu.tick()?;
                    cpu.tick_done()?;

                    // Tick4
                    cpu.tick()?;
                    cpu.tick_done()?;

                    assert!(cpu.op_tick == Tick::Tick4, "Tick is not 4? {}", cpu.op_tick);

                    if $cmos {
                        // For CMOS advance to tick5 so we can move to tick6 which is where the write happens.
                        cpu.tick()?;
                        cpu.tick_done()?;
                        assert!(cpu.op_tick == Tick::Tick5, "Tick is not 6? {}", cpu.op_tick);
                        assert!(cpu.state == State::Running, "State is not running - {}", cpu.state);
                        *r.raised.borrow_mut() = true;
                    } else {
                        // NMOS: Now re-enable and run tick5 which is a write so RDY should get skipped.
                        *r.raised.borrow_mut() = true;
                        cpu.tick()?;
                        cpu.tick_done()?;

                        assert!(cpu.op_tick == Tick::Tick5, "Tick is not 5? {}", cpu.op_tick);
                        assert!(cpu.state == State::Running, "State is not running - {}", cpu.state);
                    }

                    // Now advance one more. For both NMOS/CMOS this is a write so RDY should also
                    // be skipped.
                    cpu.tick()?;
                    cpu.tick_done()?;
                    assert!(cpu.op_tick == Tick::Reset, "Tick is not done? {}", cpu.op_tick);
                    assert!(cpu.state == State::Running, "State is not running - {}", cpu.state);

                    // Now do one more which should be a new instruction start so RDY enables.
                    cpu.tick()?;
                    cpu.tick_done()?;
                    assert!(cpu.op_tick == Tick::Tick1, "Tick is not 1? {}", cpu.op_tick);
                    assert!(cpu.state == State::RDY, "State is not RDY after RMW instr? - {}", cpu.state);

                    Ok(())
                }
            )*
        }
    }
}

rdy_test!(
    rdy_tests,
    nmos: setup_cpu_nmos, false,
    ricoh: setup_cpu_ricoh, false,
    nmos6510: setup_cpu_nmos_6510, false,
    cmos: setup_cpu_cmos, true,
);

macro_rules! init_test {
    ($suite:ident, $($name:ident: $type:expr, $fill:expr, $rand:expr,)*) => {
        mod $suite {
            use super::*;

            $(
                #[test]
                fn $name() -> Result<()> {
                    let mut iter: usize = 0;
                    let mut track = HashSet::new();
                    // If decimal state is random run 100 iterations
                    // and validate both states showed up. Yes..this could
                    // fail but 0.5^100 is a fairly small chance there...
                    if $rand {
                        iter = 100;
                    }
                    for _ in 0..=iter {
                        let r = Irq {
                            raised: RefCell::new(false),
                        };
                        let fill = $fill;

                        let d = Box::new(Debug::<CPUState>::new(128, None));
                        let mut cpu = $type(0x1212, fill, None, None, Some(&r), Some(d));

                        // This should fail
                        assert!(cpu.reset().is_err(), "reset worked before power_on");

                        // Now it should work.
                        cpu.power_on()?;
                        if cpu.p&P_DECIMAL == P_DECIMAL {
                            track.insert(true);
                        } else {
                            track.insert(false);
                        }

                        // Now run through a reset ourselves and see that
                        // tick fails partially through it.
                        cpu.reset()?;
                        let ret = cpu.tick();
                        assert!(ret.is_err(), "Tick didn't return error during reset");
                        loop {
                            match cpu.reset() {
                                Ok(OpState::Done) => break,
                                Ok(OpState::Processing) => continue,
                                Err(e) => panic!("error from reset: {e:?}"),
                            }
                        }

                        // This should fail now.
                        assert!(cpu.power_on().is_err(), "power_on passes twice");

                        // First tick should pass
                        assert!(cpu.tick().is_ok(), "Tick1 didn't pass");
                        assert!(cpu.tick_done().is_ok(), "Tick done didn't pass");

                        // STP doesn't need 3 cycles like HLT does.
                        if fill != 0xDB {
                            // Second tick should pass
                            assert!(cpu.tick().is_ok(), "Tick2 didn't pass");
                            assert!(cpu.tick_done().is_ok(), "Tick done didn't pass");
                        }

                        // Now we should get an error but it's from HLT itself.
                        let res = cpu.tick();
                        assert!(res.is_err(), "Tick3 didn't produce a halted error? {cpu}");
                        assert!(cpu.state == State::Halted, "CPU isn't halted?");

                        // This next error should be a halt error from tick itself since state
                        // is halted now.
                        let res = cpu.tick();
                        assert!(res.is_err(), "Tick didn't produce a halted error? {cpu}");

                        // SAFETY: We know it's an error so unwrap_err is fine.
                        let err = res.unwrap_err();
                        println!("Got err: {err:?}");
                        match err.root_cause().downcast_ref::<CPUError>() {
                            Some(CPUError::Halted{op: _}) => {},
                            _ => {
                                panic!("Error isn't a CPUError::Halted. Is '{err}'");
                            }
                        }
                    }
                    if $rand {
                        assert!(track.len() == 2, "didn't get both decimal states");
                    }
                    Ok(())
                }
            )*
        }
    }
}

init_test!(
    init_tests,
    nmos: setup_cpu_nmos, 0x12,
    true,
    ricoh: setup_cpu_ricoh, 0x12,
    false,
    nmos6510: setup_cpu_nmos_6510, 0x12,
    true,
    cmos: setup_cpu_cmos, 0xDB, // STP which acts like undocumented HLT did on a 6502.
    false,
);

macro_rules! tick_test {
    ($suite:ident, $($name:ident: $type:ident,)*) => {
        mod $suite {
            use super::*;

            $(
                #[test]
                fn $name() -> Result<()> {
                    let mut cpu = $type(0x1212, 0xAA, None, None, None, None);

                    // This should fail as we haven't powered on/reset.
                    {
                        let ret = cpu.tick();
                        assert!(ret.is_err(), "tick worked before reset");
                    }
                    cpu.power_on()?;

                    // Now start a reset sequence and then attempt to tick. This should fail also.
                    cpu.reset()?;
                    {
                        let ret = cpu.tick();
                        assert!(ret.is_err(), "tick worked while inside reset");
                    }

                    // Finish reset
                    loop {
                        match cpu.reset() {
                            Ok(OpState::Done) => break,
                            Ok(OpState::Processing) => continue,
                            Err(e) => return Err(e),
                        }
                    }

                    // Should work now to advance a few ticks.
                    for _ in 0..4 {
                        cpu.tick()?;
                        cpu.tick_done()?;
                    }

                    // Now validate you can't call tick_done() twice in a row or tick twice in a row.
                    {
                        let ret = cpu.tick_done();
                        assert!(ret.is_err(), "tick_done called twice");
                    }
                    {
                        cpu.tick()?;
                        let ret = cpu.tick();
                        assert!(ret.is_err(), "tick called twice");
                    }
                    Ok(())
                }
            )*
        }
    }
}

tick_test!(
    tick_tests,
    nmos: setup_cpu_nmos,
    ricoh: setup_cpu_ricoh,
    nmos6510: setup_cpu_nmos_6510,
    cmos: setup_cpu_cmos,
);

#[derive(Debug)]
struct NopHltTest {
    fill: u8,
    halt: u8,
    cycles: usize,
    bump: u8,
}

macro_rules! nop_hlt_test {
        ($suite:ident, $($name:ident: $test:expr)*) => {
            mod $suite {
                use super::*;

                $(
                    #[test]
                    fn $name() -> Result<()> {
                        let test = $test;
                        let addr = u16::from(test.halt) << 8 | u16::from(test.halt);
                        let d = Box::new(Debug::<CPUState>::new(128, Some(1)));
                        let mut cpu = setup_cpu_nmos(addr, $test.fill, None, None, None, Some(d.clone()));
                        // Make a copy so we can compare if RAM changed.
                        let mut canonical = setup_cpu_nmos(addr, test.fill, None, None, None, None);
                        cpu.power_on()?;
                        canonical.a = cpu.a;
                        canonical.x = cpu.x;
                        canonical.y = cpu.y;
                        canonical.s = cpu.s;
                        canonical.p = cpu.p;

                        // Set things up so we execute 1000 NOP's before halting.
                        let end = (Wrapping(RESET) + Wrapping(u16::from(test.bump)*1000)).0;
                        cpu.ram.borrow_mut().write(end, test.halt);
                        cpu.ram.borrow_mut().write(end + 1, test.halt);
                        canonical.ram.borrow_mut().write(end, test.halt);
                        canonical.ram.borrow_mut().write(end + 1, test.halt);

                        let got = cpu.pc.0;
                        let want = RESET;
                        tester!(got == want, d, &cpu, "PC {got:04X} isn't {want:04X}");

                        let mut tot: usize = 0;
                        let mut page_cross = 0;
                        let mut ret: Result<usize>;

                        // 9 clocks for a reset sequence.
                        let got = cpu.clocks;
                        let want = 9;
                        tester!(got == want, d, &cpu, "cpu clocks wrong. expected {want} and got {got}");
                        loop {
                            let pc = cpu.pc;
                            ret = step_cpu_nmos(&mut cpu);
                            let Ok(cycles) = ret else { break; };
                            tot += cycles;

                            if cycles != test.cycles {
                                if cycles == test.cycles + 1 {
                                    page_cross += 1;
                                } else {
                                    let got = cycles;
                                    let want = test.cycles;
                                    tester!(true, d, &cpu, "cycles incorrect - got {got} and want {want} or {want}+1 for each instruction.");
                                }
                            }

                            // NOPs generally bump the PC by one but can differ with other addressing modes.
                            let got = cpu.pc.0;
                            let want = (pc+Wrapping(u16::from(test.bump))).0;
                            tester!(got == want, d, &cpu, "PC didn't increment by bump in {test:?}. Got PC {got:04X} and started at PC {pc:04X}");

                            // Registers shouldn't be changing. Move canonical PC to match then compare.
                            canonical.pc = cpu.pc;
                            tester!(cpu == canonical, d, &cpu, "Registers differ. CPU - \n{cpu}\n and saved - \n{canonical}");

                            // We've wrapped around so abort
                            if tot > (0xFFFF * 2) {
                                break;
                            }
                        }

                        // RAM shouldn't be changing but only test once.
                        for addr in 0x0000u16..=0xFFFF {
                            let got = cpu.ram.borrow().read(addr);
                            let want = canonical.ram.borrow().read(addr);
                            tester!(got == want, d, &cpu, "RAM contents differ at {addr:04X} - got {got:02X} and want {want:02X}");
                        }


                        tester!(ret.is_err(), d, &cpu, "Loop didn't exit with error");

                        // Should end up executing X cycles times 1000 + any page crossings + 3 for halt.
                        // NOTE: since HLT returns an error from tick() step() can't report the 3 cycles it takes so we'll
                        //       check that with clocks from the cpu.
                        let want_clocks: usize = 9 + page_cross + (1000 * $test.cycles) + 3;
                        // The cycles we recorded is 12 less than that (9 for reset plus we don't record HLT clocks in step)
                        let got = tot;
                        let want = want_clocks - 12;
                        let pc = cpu.pc.0;
                        tester!(got == want, d, &cpu, "Invalid cycle count. Stopped PC: {pc:04X}. Got {got} cycles and want {want} cycles");
                        let got = cpu.clocks;
                        let want = want_clocks;
                        tester!(got == want, d, &cpu, "Invalid clock count. Got {got} clocks and want {want}");
                        // SAFETY: We know it's an error so unwrap_err is fine.
                        #[allow(clippy::unwrap_used)]
                        let err = ret.unwrap_err();
                        match err.root_cause().downcast_ref::<CPUError>() {
                            Some(CPUError::Halted{op: _}) => {},
                            _ => {
                                  tester!(true, d, &cpu, "Error isn't a CPUError::Halted. Is '{err}'");
                            }
                        }
                        println!("CPU - {cpu}");
                        println!("CPU debug - {cpu:?}");
                        d.dump("nop dump", &cpu);
                        Ok(())
                    }
                )*
            }
        }
    }

nop_hlt_test!(
    nop_hlt_tests,
    classic_nop_0x02_hlt: NopHltTest{fill: 0xEA, halt: 0x02, cycles: 2, bump: 1}
    classic_nop_0x12_hlt: NopHltTest{fill: 0xEA, halt: 0x12, cycles: 2, bump: 1}
    classic_nop_0x22_hlt: NopHltTest{fill: 0xEA, halt: 0x22, cycles: 2, bump: 1}
    classic_nop_0x32_hlt: NopHltTest{fill: 0xEA, halt: 0x32, cycles: 2, bump: 1}
    classic_nop_0x42_hlt: NopHltTest{fill: 0xEA, halt: 0x42, cycles: 2, bump: 1}
    classic_nop_0x52_hlt: NopHltTest{fill: 0xEA, halt: 0x52, cycles: 2, bump: 1}
    classic_nop_0x62_hlt: NopHltTest{fill: 0xEA, halt: 0x62, cycles: 2, bump: 1}
    classic_nop_0x72_hlt: NopHltTest{fill: 0xEA, halt: 0x72, cycles: 2, bump: 1}
    classic_nop_0x92_hlt: NopHltTest{fill: 0xEA, halt: 0x92, cycles: 2, bump: 1}
    classic_nop_0xb2_hlt: NopHltTest{fill: 0xEA, halt: 0xB2, cycles: 2, bump: 1}
    classic_nop_0xd2_hlt: NopHltTest{fill: 0xEA, halt: 0xD2, cycles: 2, bump: 1}
    classic_nop_0xf2_hlt: NopHltTest{fill: 0xEA, halt: 0xF2, cycles: 2, bump: 1}
    nop_0x04_hlt_0x12: NopHltTest{fill: 0x04, halt: 0x12, cycles: 3, bump: 2}
    nop_0x0c_hlt_0x12: NopHltTest{fill: 0x0C, halt: 0x12, cycles: 4, bump: 3}
    nop_0x14_hlt_0x12: NopHltTest{fill: 0x14, halt: 0x12, cycles: 4, bump: 2}
    nop_0x1a_hlt_0x12: NopHltTest{fill: 0x1A, halt: 0x12, cycles: 2, bump: 1}
    nop_0x1c_hlt_0x12: NopHltTest{fill: 0x1C, halt: 0x12, cycles: 4, bump: 3}
    nop_0x34_hlt_0x12: NopHltTest{fill: 0x34, halt: 0x12, cycles: 3, bump: 2}
    nop_0x3a_hlt_0x12: NopHltTest{fill: 0x3A, halt: 0x12, cycles: 2, bump: 1}
    nop_0x3c_hlt_0x12: NopHltTest{fill: 0x3C, halt: 0x12, cycles: 4, bump: 3}
    nop_0x44_hlt_0x12: NopHltTest{fill: 0x44, halt: 0x12, cycles: 3, bump: 2}
    nop_0x54_hlt_0x12: NopHltTest{fill: 0x54, halt: 0x12, cycles: 4, bump: 2}
    nop_0x5a_hlt_0x12: NopHltTest{fill: 0x5A, halt: 0x12, cycles: 2, bump: 1}
    nop_0x5c_hlt_0x12: NopHltTest{fill: 0x5C, halt: 0x12, cycles: 4, bump: 3}
    nop_0x64_hlt_0x12: NopHltTest{fill: 0x64, halt: 0x12, cycles: 3, bump: 2}
    nop_0x74_hlt_0x12: NopHltTest{fill: 0x74, halt: 0x12, cycles: 4, bump: 2}
    nop_0x7a_hlt_0x12: NopHltTest{fill: 0x7A, halt: 0x12, cycles: 2, bump: 1}
    nop_0x7c_hlt_0x12: NopHltTest{fill: 0x7C, halt: 0x12, cycles: 4, bump: 3}
    nop_0x80_hlt_0x12: NopHltTest{fill: 0x80, halt: 0x12, cycles: 2, bump: 2}
    nop_0x82_hlt_0x12: NopHltTest{fill: 0x82, halt: 0x12, cycles: 2, bump: 2}
    nop_0x89_hlt_0x12: NopHltTest{fill: 0x89, halt: 0x12, cycles: 2, bump: 2}
    nop_0xc2_hlt_0x12: NopHltTest{fill: 0xC2, halt: 0x12, cycles: 2, bump: 2}
    nop_0xd4_hlt_0x12: NopHltTest{fill: 0xD4, halt: 0x12, cycles: 4, bump: 2}
    nop_0xda_hlt_0x12: NopHltTest{fill: 0xDA, halt: 0x12, cycles: 2, bump: 1}
    nop_0xdc_hlt_0x12: NopHltTest{fill: 0xDC, halt: 0x12, cycles: 4, bump: 3}
    nop_0xe2_hlt_0x12: NopHltTest{fill: 0xE2, halt: 0x12, cycles: 2, bump: 2}
    nop_0xf4_hlt_0x12: NopHltTest{fill: 0xF4, halt: 0x12, cycles: 4, bump: 2}
    nop_0xfa_hlt_0x12: NopHltTest{fill: 0xFA, halt: 0x12, cycles: 2, bump: 1}
    nop_0xfc_hlt_0x12: NopHltTest{fill: 0xFC, halt: 0x12, cycles: 4, bump: 3}
);

macro_rules! load_test {
        ($suite:ident, $($name:ident: $x:expr, $expected:expr)*) => {
            mod $suite {
                use super::*;

                $(
                    #[test]
                    fn $name() -> Result<()> {
                        let d = Box::new(Debug::<CPUState>::new(128, Some(1)));
                        let mut cpu = setup_cpu_nmos(0x1212, 0xEA, None, None, None, Some(d.clone()));
                        cpu.power_on()?;

                        cpu.ram.borrow_mut().write(0x1FFE, 0xA1); // LDA ($EA,x)
                        cpu.ram.borrow_mut().write(0x1FFF, 0xEA);
                        cpu.ram.borrow_mut().write(0x2000, 0xA1); // LDA ($FF,x)
                        cpu.ram.borrow_mut().write(0x2001, 0xFF);
                        cpu.ram.borrow_mut().write(0x2002, 0x12); // HLT

                        // (0x00EA) points to 0x650F
                        cpu.ram.borrow_mut().write(0x00EA, 0x0F);
                        cpu.ram.borrow_mut().write(0x00EB, 0x65);

                        // (0x00FA) points to 0x551F
                        cpu.ram.borrow_mut().write(0x00FA, 0x1F);
                        cpu.ram.borrow_mut().write(0x00FB, 0x55);

                        // (0x00FF) points to 0xA1FA (since 0x0000 is 0xA1)
                        cpu.ram.borrow_mut().write(0x00FF, 0xFA);
                        cpu.ram.borrow_mut().write(0x0000, 0xA1);

                        // (0x001F) points to 0xA20A
                        cpu.ram.borrow_mut().write(0x000F, 0x0A);
                        cpu.ram.borrow_mut().write(0x0010, 0xA2);

                        // For LDA ($FA,x) X = 0x00
                        cpu.ram.borrow_mut().write(0x650F, 0xAB);
                        // For LDA ($FA,x) X = 0x10
                        cpu.ram.borrow_mut().write(0x551F, 0xCD);

                        // For LDA ($FF,x) X = 0x00
                        cpu.ram.borrow_mut().write(0xA1FA, 0xEF);
                        // For LDA ($FF,x) X = 0x10
                        cpu.ram.borrow_mut().write(0xA20A, 0x00);

                        // Do reset
                        loop {
                            match cpu.reset() {
                                Ok(OpState::Done) => break,
                                Ok(OpState::Processing) => continue,
                                Err(e) => return Err(e),
                            }
                        }

                        for (iteration, e) in $expected.iter().enumerate() {
                            cpu.a = Wrapping(*e) - Wrapping(1);
                            cpu.x = Wrapping($x);
                            // These all take 6 cycles so validate
                            let cycles = step_cpu_nmos(&mut cpu)?;
                            tester!(cycles == 6, d, &cpu, "Invalid cycle count: {cycles} expected 6");
                            let got = cpu.a.0;
                            let want = *e;
                            tester!(got == want, d, &cpu, "A register doesn't have correct value for iteration {iteration}. Got {got:02X} and want {want:02X})");
                            let got = cpu.p & P_ZERO == Flags(0x00);
                            let want = *e != 0x00;
                            let got_p = cpu.p;
                            let got_a = cpu.a.0;
                            tester!(
                                got == want, d, &cpu,
                                "Z flag is incorrect. Got {got_p} and A is {got_a:02X}");
                            let got = cpu.p & P_NEGATIVE == Flags(0x00);
                            let want = *e < 0x80;
                            tester!(
                                got == want, d, &cpu,
                                "N flag is incorrect. Got {got_p} and A is {got_a:02X}");
                        }
                        Ok(())
                    }
                )*
            }
        }
    }

load_test!(
    load_tests,
    x_is_zero: 0x00, [0xAB, 0xEF]
    x_is_10: 0x10, [0xCD, 0x00]
);

macro_rules! store_test {
    ($suite:ident, $($name:ident: $a:expr, $x:expr, $expected:expr,)*) => {
        mod $suite {
            use super::*;

            $(
                #[test]
                fn $name() -> Result<()> {
                    let d = Box::new(Debug::<CPUState>::new(128, Some(1)));
                    let mut cpu = setup_cpu_nmos(0x1212, 0xEA, None, None, None, Some(d.clone()));
                    cpu.power_on()?;

                    cpu.ram.borrow_mut().write(0x1FFE, 0x81); // STA ($EA,x)
                    cpu.ram.borrow_mut().write(0x1FFF, 0xEA);
                    cpu.ram.borrow_mut().write(0x2000, 0x81); // STA ($FF,x)
                    cpu.ram.borrow_mut().write(0x2001, 0xFF);
                    cpu.ram.borrow_mut().write(0x2002, 0x12); // HLT

                    // (0x00EA) points to 0x650F
                    cpu.ram.borrow_mut().write(0x00EA, 0x0F);
                    cpu.ram.borrow_mut().write(0x00EB, 0x65);

                    // (0x00FA) points to 0x551F
                    cpu.ram.borrow_mut().write(0x00FA, 0x1F);
                    cpu.ram.borrow_mut().write(0x00FB, 0x55);

                    // (0x00FF) points to 0xA1FA (since 0x0000 is 0xA1)
                    cpu.ram.borrow_mut().write(0x00FF, 0xFA);
                    cpu.ram.borrow_mut().write(0x0000, 0xA1);

                    // (0x001F) points to 0xA20A
                    cpu.ram.borrow_mut().write(0x000F, 0x0A);
                    cpu.ram.borrow_mut().write(0x0010, 0xA2);

                    // For STA ($FA,x) X = 0x00
                    cpu.ram.borrow_mut().write(0x650F, 0x00);
                    // For STA ($FA,x) X = 0x10
                    cpu.ram.borrow_mut().write(0x551F, 0x00);

                    // For STA ($FF,x) X = 0x00
                    cpu.ram.borrow_mut().write(0xA1FA, 0x00);
                    // For STA ($FF,x) X = 0x10
                    cpu.ram.borrow_mut().write(0xA20A, 0x00);

                    // Do reset
                    loop {
                        match cpu.reset() {
                            Ok(OpState::Done) => break,
                            Ok(OpState::Processing) => continue,
                            Err(e) => return Err(e),
                        }
                    }

                    for (iteration, e) in $expected.iter().enumerate() {
                        // These don't change status flags but we do verify that too.
                        let p = cpu.p;
                        cpu.a = Wrapping($a);
                        cpu.x = Wrapping($x);
                        // These all take 6 cycles so validate
                        let cycles = step_cpu_nmos(&mut cpu)?;
                        tester!(cycles == 6, d, &cpu, "Invalid cycle count: {cycles} expected 6");
                        let got = cpu.a.0;
                        let want = cpu.ram.borrow().read(*e);
                        tester!(got == want, d, &cpu, "A register doesn't have correct value for iteration {iteration}. Got {got:02X} from {e:04X} and want {want:02X}");
                        let got_p = cpu.p;
                        tester!(got_p == p, d, &cpu, "Status changed. Orig {p} and got {got_p}");
                    }
                    Ok(())
                }
            )*
        }
    }
}

store_test!(
    store_tests,
    x_is_zero: 0xAA, 0x00, [0x650F, 0xA1FA],
    x_is_10: 0x55, 0x10, [0x551F, 0xA20A],
);

// The test ROM doesn't test that BBR at a page boudary actually happens so
// we'll set one up and have it jump across the boundary and validate we ran
// as many clocks as expected.
#[test]
fn cmos_bbr_extra_clocks() -> Result<()> {
    let d = Box::new(Debug::<CPUState>::new(128, Some(1)));
    let mut cpu = setup_cpu_cmos(0xFFFF, 0x00, None, None, None, Some(d));
    cpu.power_on()?;

    let start = 0x1FFA;
    cpu.ram.borrow_mut().write(start, 0x0F); // BBR 0,$00,$40
    cpu.ram.borrow_mut().write(start + 1, 0x00);
    cpu.ram.borrow_mut().write(start + 2, 0x40);

    cpu.pc = Wrapping(start);
    let cycles = step_cpu_cmos(&mut cpu)?;
    let want = 7;
    assert!(
        cycles == want,
        "Didn't get expected cycles {want} got {cycles}"
    );
    let want = start + 3 + 0x40;
    assert!(
        cpu.pc.0 == want,
        "Didn't get expected PC. Want {want:04X} and got {:04X}",
        cpu.pc.0
    );
    Ok(())
}

struct Irq {
    raised: RefCell<bool>,
}

impl Sender for Irq {
    fn raised(&self) -> bool {
        *self.raised.borrow()
    }
}

macro_rules! irq_and_nmi_test {
    ($suite:ident, $($name:ident: $setup:ident, $cmos:expr,)*) => {
        mod $suite {
            use super::*;

            $(
                #[test]
                #[allow(clippy::too_many_lines)]
                fn $name() -> Result<()> {
                    // This test is little more serial and long than other tests as the corner cases with
                    // interrupt handling only occur when triggered on specific ticks and while in certain states.
                    // So this has to be done clock by clock and conditions checked at each.

                    let nmi: u16 = 0x0202; // If executed should halt the processor but we'll put code at this PC.
                    let irq = IRQ_ADDR;
                    let i = Irq {
                        raised: RefCell::new(false),
                    };
                    let n = Irq {
                        raised: RefCell::new(false),
                    };

                    let d = Box::new(Debug::<CPUState>::new(128, Some(1)));
                    let mut cpu = $setup(
                        nmi,
                        0xEA,
                        Some(&i),
                        Some(&n),
                        None,
                        Some(d.clone()),
                    );
                    cpu.power_on()?;

                    cpu.ram.borrow_mut().write(IRQ_ADDR, 0x69); // ADC #AB
                    cpu.ram.borrow_mut().write(IRQ_ADDR + 1, 0xAB);
                    cpu.ram.borrow_mut().write(IRQ_ADDR + 2, 0x40); // RTI
                    cpu.ram.borrow_mut().write(nmi, 0x40); // RTI
                    cpu.ram.borrow_mut().write(RESET, 0xEA); // NOP
                    cpu.ram.borrow_mut().write(RESET + 1, 0x00); // BRK #00
                    cpu.ram.borrow_mut().write(RESET + 2, 0x00);
                    cpu.ram.borrow_mut().write(RESET + 3, 0xD0); // BNE +2
                    cpu.ram.borrow_mut().write(RESET + 4, 0x00);
                    cpu.ram.borrow_mut().write(RESET + 5, 0xD0); // BNE +2
                    cpu.ram.borrow_mut().write(RESET + 6, 0x00);

                    // Set D on up front and I on
                    cpu.p |= P_DECIMAL;
                    cpu.p &= !P_INTERRUPT;

                    // Save a copy of P so we can compare
                    let saved_p = cpu.p;

                    // Now turn on interrupt for initial tests below.
                    cpu.p |= P_INTERRUPT;

                    // Set A to 0
                    cpu.a = Wrapping(0x00);

                    // Now wrap this into a RefCell so we can create verify below and use it mutablely there
                    // but still be able to peek inside to check other invariants later.
                    let wrapped_cpu = RefCell::new(cpu);

                    let verify = |irq: bool, nmi: bool, state: &str, done: bool| -> Result<()> {
                        *i.raised.borrow_mut() = irq;
                        *n.raised.borrow_mut() = nmi;

                        // We don't use Step because we want to inspect/change things on a per tick basis.
                        {
                            let c = wrapped_cpu.borrow();
                            println!("pre: {state} tick: {} irq: {irq} nmi: {nmi} done: {done} irq_raised: {} skip: {} interrupt_state: {}", c.op_tick, c.irq_raised, c.skip_interrupt, c.interrupt_state);
                        }
                        wrapped_cpu.borrow_mut().tick()?;
                        wrapped_cpu.borrow_mut().tick_done()?;
                        let c = wrapped_cpu.borrow();
                        println!("post: {state} tick: {} irq: {irq} nmi: {nmi} done: {done} irq_raised: {} skip: {} interrupt_state: {}", c.op_tick, c.irq_raised, c.skip_interrupt, c.interrupt_state);
                        println!("{c}");
                        Ok(())
                    };

                    verify(false, false, "pre NOP", false)?;

                    // IRQ but should finish instruction and skip IRQ and set PC to RESET+1
                    let state = "pre 2nd NOP";
                    verify(true, false, state, true)?;
                    let got = wrapped_cpu.borrow().pc.0;
                    let want = RESET + 1;
                    tester!(
                        got == want,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: got wrong PC {got:04X} want {want:04X}"
                    );

                    // Verify we move on
                    let state = "pre 3rd NOP";
                    verify(true, false, state, false)?;
                    let got = wrapped_cpu.borrow().pc.0;
                    let want = RESET + 2;
                    tester!(
                        got == want,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: got wrong PC {got:04X} want {want:04X}"
                    );

                    // But should be None as I is set.
                    let st = wrapped_cpu.borrow().irq_raised;
                    tester!(
                        st == InterruptStyle::None,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: IRQ wasn't set to be handled post instruction - {st}"
                    );
                    // Verify P still has S1, I and D set
                    let got = wrapped_cpu.borrow().p;
                    let want = P_S1 | P_DECIMAL|P_INTERRUPT;
                    tester!(got == want, d, wrapped_cpu.borrow().deref(), "{state}: got wrong flags {got} want {want}");

                    // Turn off P_INTERRUPT so we can test it happening below
                    wrapped_cpu.borrow_mut().p &= !P_INTERRUPT;

                    // Start running BRK and interrupt part way through (with IRQ) which should complete BRK
                    // but skip it upon return. This means running 5 ticks normally.
                    // This only happens on NMOS. For CMOS the BRK runs, then the IRQ and then we continue.
                    // NOTE: We already ran tick1 of this above.
                    let state = "BRK w IRQ";
                    for _ in 0..3 {
                        verify(false, false, state, false)?;
                    }
                    // Now set IRQ
                    verify(true, false, state, false)?;
                    // Now should jump
                    verify(false, false, state, false)?;
                    verify(false, false, state, true)?;
                    let want = irq;
                    // For CMOS advance time until we're prepping for the IRQ
                    // i.e. run out the BRK
                    if $cmos {
                        loop {
                            if wrapped_cpu.borrow().pc.0 == want {
                                break;
                            }
                            println!("cmos advance for IRQ");
                            verify(false, false, state, false)?;
                        }
                    }
                    let got = wrapped_cpu.borrow().pc.0;
                    tester!(got == want, d, wrapped_cpu.borrow().deref(), "{state}: Got wrong PC {got:#06X} want {want:#06X}");
                    // Pull P off the stack and verify the B bit didn't get set because by then we were in IRQ.
                    let addr = (wrapped_cpu.borrow().s + Wrapping(1)).0;
                    let got = Flags(wrapped_cpu.borrow().ram.borrow().read(STACK_START + u16::from(addr)));
                    let mut want = saved_p;
                    if $cmos {
                        // In the CMOS case the BRK always runs so this will have B set.
                        want |= P_B;
                    }
                    {
                        let c = wrapped_cpu.borrow();
                        tester!(got == want, d, c.deref(), "{state}: Flags aren't correct. Got {got} and want {want} - cpu: {c}");
                    }
                    {
                        let mut want = &InterruptStyle::None;
                        if $cmos {
                            // In CMOS this is still raised since we need to run IRQ
                            want = &InterruptStyle::IRQ;
                        }
                        let i = &wrapped_cpu.borrow().irq_raised;
                        tester!(i == want, d, wrapped_cpu.borrow().deref(), "{state}: IRQ wasn't cleared after BRK - {i:?} vs {want:?}");
                    }
                    tester!(
                        wrapped_cpu.borrow().interrupt_state == InterruptState::None,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: running interrupt still?"
                    );

                    // Reset PC and turn I off and make sure D is on.
                    loop {
                        match wrapped_cpu.borrow_mut().reset() {
                          Ok(OpState::Done) => break,
                          Ok(OpState::Processing) => continue,
                          Err(e) => panic!("error from reset: {e:?}"),
                        }
                    }
                    wrapped_cpu.borrow_mut().p |= P_DECIMAL;
                    wrapped_cpu.borrow_mut().p &= !P_INTERRUPT;

                    let got = wrapped_cpu.borrow().pc.0;
                    tester!(got == RESET, d, wrapped_cpu.borrow().deref(), "{state}: PC's don't match want {RESET:04X} and got {got:04X}");

                    // Now start fresh with no IRQ.
                    verify(false, false, "First NOP", false)?;

                    // IRQ but should finish instruction and set PC to RESET+1
                    let state = "2nd NOP";
                    verify(true, false, state, true)?;
                    let got = wrapped_cpu.borrow().pc.0;
                    let want = RESET + 1;
                    tester!(
                        got == want,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: got wrong PC {got:04X} want {want:04X}"
                    );
                    // Verify it'll try and run an IRQ next.
                    tester!(
                        wrapped_cpu.borrow().irq_raised == InterruptStyle::IRQ,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: IRQ wasn't set to be handled post instruction"
                    );
                    // Verify P still has S1 and D set
                    let got = wrapped_cpu.borrow().p;
                    let want = P_S1 | P_DECIMAL;
                    tester!(got == want, d, wrapped_cpu.borrow().deref(), "{state}: got wrong flags {got} want {want}");

                    // Don't assert IRQ anymore as should be cached state. Also this should take 7 cycles.
                    let state = "IRQ setup";
                    for _ in 0..6 {
                        verify(false, false, state, false)?;
                    }
                    verify(false, false, state, true)?;
                    let got = wrapped_cpu.borrow().pc.0;
                    let want = IRQ_ADDR;
                    tester!(
                        got == want,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: got wrong PC {got:04X} want {want:04X}"
                    );
                    // Verify the only things set in flags right now are S1 and I and maybe D.
                    // D shouldn't be cleared for NMOS but is for CMOS;
                    println!("CPU: {}", wrapped_cpu.borrow());
                    let got = wrapped_cpu.borrow().p;
                    let want;
                    if $cmos {
                        want = P_S1 | P_INTERRUPT;
                    } else {
                        want = P_S1 | P_INTERRUPT | P_DECIMAL;
                    }
                    tester!(got == want, d, wrapped_cpu.borrow().deref(), "{state}: got wrong flags {got} want {want}");
                    tester!(
                        wrapped_cpu.borrow().irq_raised == InterruptStyle::None,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: IRQ wasn't cleared after run"
                    );
                    tester!(
                        wrapped_cpu.borrow().interrupt_state == InterruptState::None,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: running interrupt still?"
                    );

                    // Pull P off the stack and verify the B bit didn't get set.
                    {
                        let c = wrapped_cpu.borrow();
                        let addr = (c.s + Wrapping(1)).0;
                        let got = Flags(c.ram.borrow().read(u16::from(addr) + STACK_START));
                        tester!(
                            got == saved_p,
                            d, wrapped_cpu.borrow().deref(),
                            "{state}: flags aren't correct. Didn't match original. got {got} want {saved_p}"
                        );
                    }

                    // Now set IRQ. Should still let this instruction finish since the first instruction
                    // of a handler always completes before we trigger another handler.
                    let state = "ADC #AB";
                    verify(true, false, state, false)?;
                    // Now set NMI also and it should win.
                    verify(true, true, state, true)?;
                    let got = wrapped_cpu.borrow().a.0;
                    let want;
                    if $cmos {
                      // Not BCD
                      want = 0xAB;
                    } else {
                      // NMOS BCD is still on.
                      want = 0x11;
                     }
                    tester!(
                        got == want,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: A doesn't match. got {got:02X} and want {want:02X}"
                    );

                    // NMI setup takes 7 cycles too.
                    let state = "NMI setup";
                    for _ in 0..6 {
                        verify(false, false, state, false)?;
                    }
                    verify(false, false, state, true)?;
                    let got = wrapped_cpu.borrow().pc.0;
                    let want = nmi;
                    tester!(
                        got == want,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: got wrong PC {got:04X} want {want:04X}"
                    );
                    tester!(
                        wrapped_cpu.borrow().irq_raised == InterruptStyle::None,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: IRQ wasn't cleared after run"
                    );
                    tester!(
                        wrapped_cpu.borrow().interrupt_state == InterruptState::None,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: running interrupt still?"
                    );

                    // Should be an RTI that takes 6 cycles
                    let state = "First RTI";
                    for _ in 0..5 {
                        verify(false, false, state, false)?;
                    }
                    verify(false, false, state, true)?;
                    let got = wrapped_cpu.borrow().pc.0;
                    let want = IRQ_ADDR + 2;
                    tester!(
                        got == want,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: got wrong PC {got:04X} want {want:04X}"
                    );

                    // Another RTI
                    let state = "Second RTI";
                    for _ in 0..5 {
                        verify(false, false, state, false)?;
                    }
                    verify(false, false, state, true)?;
                    let got = wrapped_cpu.borrow().pc.0;
                    let want = RESET + 1;
                    tester!(
                        got == want,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: got wrong PC {got:04X} want {want:04X}"
                    );
                    let got = wrapped_cpu.borrow().p;
                    tester!(
                        got == saved_p,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: flags didn't reset got {got} and want {saved_p}"
                    );

                    // Start running BRK and interrupt part way through (with NMI) which should complete BRK
                    // but skip it upon return. This means running 5 ticks normally.
                    // This only happens on NMOS. For CMOS the BRK runs, then the NMI and then we continue.
                    let state = "BRK";
                    for _ in 0..5 {
                        verify(false, false, state, false)?;
                    }
                    // Now set NMI
                    verify(false, true, state, false)?;
                    // Now should jump
                    verify(false, false, state, true)?;
                    let want = nmi;
                    // For CMOS advance time until we're prepping for the NMI.
                    // i.e. run out the BRK
                    if $cmos {
                        loop {
                            if wrapped_cpu.borrow().pc.0 == want {
                                break;
                            }
                            println!("cmos advance for NMI");
                            verify(false, false, state, false)?;
                        }
                    }
                    let got = wrapped_cpu.borrow().pc.0;
                    tester!(got == want, d, wrapped_cpu.borrow().deref(), "{state}: Got wrong PC {got:#06X} want {want:#06X}");
                    // Pull P off the stack and verify the B bit did get set even though we're in an NMI handler.
                    let addr = (wrapped_cpu.borrow().s + Wrapping(1)).0;
                    let got = Flags(wrapped_cpu.borrow().ram.borrow().read(STACK_START + u16::from(addr)));
                    let mut want = saved_p | P_B;
                    // Don't test flags for CMOS as this state only happens on NMOS
                    // i.e. we're running a real NMI for CMOS so actually one more stack level deep
                    // and B won't be set.
                    if $cmos {
                        // We'll actually see I on here since BRK/IRQ set this and we NMI'd in the middle
                        // of their handler. Also D will be off as all interrupts disable this on CMOS.
                        want = saved_p | P_INTERRUPT;
                        want &= !P_DECIMAL;
                    }
                    {
                        let c = wrapped_cpu.borrow();
                        tester!(got == want, d, wrapped_cpu.borrow().deref(), "{state}: Flags aren't correct. Don't include P_B even for NMI. Got {got} and want {want} - cpu: {c}");
                    }
                    {
                        let i = &wrapped_cpu.borrow().irq_raised;
                        tester!(i == &InterruptStyle::None, d, wrapped_cpu.borrow().deref(), "{state}: IRQ wasn't cleared after BRK - {i:?}");
                    }
                    tester!(
                        wrapped_cpu.borrow().interrupt_state == InterruptState::None,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: running interrupt still?"
                    );

                    // Yet another RTI
                    let state = "3rd RTI";
                    for _ in 0..5 {
                        verify(false, false, state, false)?;
                    }
                    verify(false, false, state, true)?;
                    let want = RESET + 3;
                    // For CMOS we're technically in 4th RTI so run this off to
                    // get back to the 3rd one.
                    if $cmos {
                        loop {
                            if wrapped_cpu.borrow().pc.0 == want {
                                break;
                            }
                            println!("cmos advance for RTI");
                            verify(false, false, state, false)?;
                        }
                    }
                    let got = wrapped_cpu.borrow().pc.0;
                    tester!(
                        got == want,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: got wrong PC {got:#06X} want {want:#06X}"
                    );

                    // Now we're going to run BNE+2 (so the next instruction) and set NMI in the middle of it.
                    // It shoudn't start that processing until after this and the next instruction.
                    // These take 3 cycles since they aren't page boundary crossing.
                    let state = "1st BNE";
                    verify(false, false, state, false)?;
                    verify(false, true, state, false)?;
                    verify(false, false, state, true)?;
                    // PC should have advanced to the next instruction.
                    let got = wrapped_cpu.borrow().pc.0;
                    let want = RESET + 5;
                    tester!(
                        got == want,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: got wrong PC {got:04X} want {want:04X}"
                    );
                    // And it should advance again into the next instruction.
                    let state = "2nd BNE";
                    verify(false, false, state, false)?;
                    let got = wrapped_cpu.borrow().pc.0;
                    let want = RESET + 6;
                    tester!(
                        got == want,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: got wrong PC {got:04X} want {want:04X}"
                    );
                    // And then finish with NMI set again but it won't skip this time.
                    verify(false, true, state, false)?;
                    verify(false, false, state, true)?;
                    // Now it should start an NMI
                    let state = "2nd NMI setup";
                    for _ in 0..6 {
                        verify(false, false, state, false)?;
                    }
                    verify(false, false, state, true)?;
                    let got = wrapped_cpu.borrow().pc.0;
                    let want = nmi;
                    tester!(
                        got == want,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: got wrong PC {got:04X} want {want:04X}"
                    );

                    // Should be another RTI
                    let state = "4th RTI";
                    for _ in 0..5 {
                        verify(false, false, state, false)?;
                    }
                    verify(false, false, state, true)?;
                    let got = wrapped_cpu.borrow().pc.0;
                    let want = RESET + 7;
                    tester!(
                        got == want,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: got wrong PC {got:04X} want {want:04X}"
                    );

                    // Finally fire off an NMI at the start of this NOP which should immediately run the interrupt.
                    let state = "3rd NMI setup";
                    verify(false, true, state, false)?;
                    for _ in 0..5 {
                        verify(false, false, state, false)?;
                    }
                    verify(false, false, state, true)?;
                    let got = wrapped_cpu.borrow().pc.0;
                    let want = nmi;
                    tester!(
                        got == want,
                        d, wrapped_cpu.borrow().deref(),
                        "{state}: got wrong PC {got:04X} want {want:04X}"
                    );

                    Ok(())
                }
            )*
        }
    }
}

irq_and_nmi_test!(
    irq_and_nmi_tests,
    nmos: setup_cpu_nmos, false,
    cmos: setup_cpu_cmos, true,
);

struct RomTest<'a> {
    filename: &'a str,
    nes: bool,
    start_pc: u16,
    load_traces: Option<fn() -> Result<Vec<Verify>>>,
    end_check: fn(u16, u16, &dyn Memory) -> bool,
    success_check: fn(u16, u16, &dyn Memory) -> Result<()>,
    expected_cycles: Option<usize>,
    expected_instructions: Option<usize>,
}

#[derive(Debug, Default)]
struct Verify {
    pc: u16,
    a: u8,
    x: u8,
    y: u8,
    p: Flags,
    s: u8,
    cyc: usize,
}

impl fmt::Display for Verify {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Verify {{ pc: {:04X}, a: {:02X}, x: {:02X}, y: {:02X}, s: {:02X}, p: {}, cyc: {} }}",
            self.pc, self.a, self.x, self.y, self.s, self.p, self.cyc
        )
    }
}

macro_rules! rom_test {
        ($suite:ident, $($name:ident: $rom_test:expr, $rom:ident, $cpu:ident, $setup:expr, $step:expr,)*) => {
            mod $suite {
                use super::*;

                $(
                    #[test]
                    fn $name() -> Result<()> {
                        let r = $rom_test;
                        let res = panic::catch_unwind(|| {$rom(&r, false)});
                        if res.is_err() {
                            return $rom(&r, true)
                        }
                        Ok(())
                    }

                    #[allow(clippy::too_many_lines)]
                    fn $rom(r: &RomTest, dbg: bool) -> Result<()> {
                        let mut cpu: $cpu<'_>;
                        // Initialize as always but then we'll overwrite it with a ROM image.
                        // For this we'll use BRK and a vector which if executed should halt the processor.
                        let d = Box::new(Debug::<CPUState>::new(128, Some(1)));

                        // If we want debugging setup the debug hook. Otherwise go for faster execution.
                        // Callers should run without debug and then if fails rerun with debug so
                        // normal testing is fast.
                        if dbg {
                            cpu = $setup(0x0202, 0x00, None, None, None, Some(d.clone()));
                        } else {
                            cpu = $setup(0x0202, 0x00, None, None, None, None);
                        }
                        cpu.power_on()?;

                        // Get the input ROM and poke it into place.
                        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
                            .join("../testdata")
                            .join(r.filename);
                        println!("path: {}", path.display());
                        let bytes = read(path)?;

                        if r.nes {
                            // The NES test assumes registers are zeroed and SP is FD.
                            // Easier to do that here than modifying it and it's trace log.
                            cpu.a = Wrapping(0x00);
                            cpu.x = Wrapping(0x00);
                            cpu.y = Wrapping(0x00);
                            cpu.s = Wrapping(0xFD);

                            assert!(
                                bytes.len() >= 16_384
                                && bytes[0] == b'N'
                                && bytes[1] == b'E'
                                && bytes[2] == b'S'
                                && bytes[3] == 0x1A,
                                "Bad NES ROM format"
                            );
                            let prg_count = bytes[4];
                            let chr_count = bytes[5];
                            println!("PRG: {prg_count}\nCHR: {chr_count}");

                            // Map the first PRG into place
                            for i in 0..16 * 1024 {
                                cpu.ram
                                    .borrow_mut()
                                    .write(u16::try_from(i)? + 0xC000, bytes[16 + i]);
                            }
                            // Nothing else needs to happen unless we get more extensive NES ROM's
                        } else {
                            for (addr, b) in bytes.iter().enumerate() {
                                cpu.ram.borrow_mut().write(u16::try_from(addr)?, *b);
                            }
                        }

                        // Load up traces if we need them.
                        let mut traces: Vec<Verify> = Vec::new();

                        if let Some(load_traces) = r.load_traces {
                            traces = load_traces()?;
                        }

                        // Do reset
                        loop {
                            match cpu.reset() {
                                Ok(OpState::Done) => break,
                                Ok(OpState::Processing) => continue,
                                Err(e) => return Err(e),
                            }
                        }

                        cpu.pc = Wrapping(r.start_pc);

                        let mut total_cycles: usize = 0;
                        let mut total_instructions: usize = 0;

                        loop {
                            let old_pc = cpu.pc.0;

                            if !traces.is_empty() {
                                tester!(
                                    total_instructions < traces.len(),
                                    d, &cpu,
                                    "Ran out of trace log at PC: {old_pc:04X}"
                                );

                                let entry = &traces[total_instructions];
                                // The NES executes 3 clocks per cpu clock and rolls every 341 to account for scan lines.
                                // Adjust to match cycle counts if this an nes test cart.

                                let test_cycle = if r.nes {
                                    (total_cycles * 3) % 341
                                } else {
                                    total_cycles
                                };

                                if cpu.pc.0 != entry.pc
                                    || cpu.p != entry.p
                                    || cpu.a.0 != entry.a
                                    || cpu.x.0 != entry.x
                                    || cpu.y.0 != entry.y
                                    || cpu.s.0 != entry.s
                                    || test_cycle != entry.cyc
                                {
                                    tester!(
                                        false,
                                        d, &cpu,
                                        "Trace log violation.\nGot CPU: {cpu} cyc: {test_cycle}\nWant entry: {entry}"
                                    );
                                }
                            }

                            let cycles = $step(&mut cpu)?;
                            total_cycles += cycles;
                            total_instructions += 1;

                            if (r.end_check)(old_pc, cpu.pc.0, &*cpu.ram.borrow()) {
                                let res = (r.success_check)(old_pc, cpu.pc.0, &*cpu.ram.borrow());
                                if let Err(err) = res {
                                    tester!(false, d, &cpu, "{err}");
                                }
                                break;
                            }
                        }

                        let got = total_cycles;
                        if let Some(want) = r.expected_cycles {
                            tester!(
                                got == want,
                                d, &cpu,
                                "cycles don't match: got {got} and want {want}"
                            );
                        }

                        let got = total_instructions;
                        if let Some(want) = r.expected_instructions {
                            tester!(
                                got == want,
                                d, &cpu,
                                "instructions don't match: got {got} and want {want}"
                            );
                        }
                        Ok(())
                    }
                )*
            }
        }
}

rom_test!(
    rom_tests,
    functional_test: RomTest{
        filename: "6502_functional_test.bin",
        nes: false,
        start_pc: 0x0400,
        load_traces: None,
        end_check: |old, cur, _ram| {
            old == cur
        },
        success_check: |_old, cur, _ram| {
            if cur == 0x3469 {
                return Ok(());
            }
            Err(eyre!("CPU looping at PC: {cur:#06X}"))
        },
        expected_cycles: Some(96_241_367),
        expected_instructions: Some(30_646_177),
    }, rom_functional_test, CPU6502, setup_cpu_nmos, step_cpu_nmos,
    functional_cmos_test: RomTest{
        filename: "6502_functional_test.bin",
        nes: false,
        start_pc: 0x0400,
        load_traces: None,
        end_check: |old, cur, _ram| {
            old == cur
        },
        success_check: |_old, cur, _ram| {
            if cur == 0x3469 {
                return Ok(());
            }
            Err(eyre!("CPU looping at PC: {cur:#06X}"))
        },
        expected_cycles: Some(96_561_324),
        expected_instructions: Some(30_646_177),
    }, rom_functional_cmos_test, CPU65C02, setup_cpu_cmos, step_cpu_cmos,
    functional_extended_cmos_test: RomTest{
        filename: "65C02_extended_opcodes_test.bin",
        nes: false,
        start_pc: 0x0400,
        load_traces: None,
        end_check: |old, cur, _ram| {
            old == cur
        },
        success_check: |_old, cur, _ram| {
            if cur == 0x24F1 {
                return Ok(());
            }
            Err(eyre!("CPU looping at PC: {cur:#06X}"))
        },
        expected_cycles: Some(66_907_084),
        expected_instructions: Some(21_986_986),
    }, rom_functional_extended_cmos_test, CPU65C02, setup_cpu_cmos, step_cpu_cmos,
    // The next tests (up to and including vsbx.bin) all come from http://nesdev.com/6502_cpu.txt
    // NOTE: They are hard to debug even with the ring buffer since the test is self modifying code...
    // So you'll have to use the register values and dumps to infer state along the way.
    dadc_test: RomTest{
        filename: "dadc.bin",
        nes: false,
        start_pc: 0xD000,
        load_traces: None,
        end_check: |old, cur, _ram| {
            old == cur
        },
        success_check: |_old, cur, _ram| {
            if cur == 0xD004 {
                return Ok(());
            }
            Err(eyre!("CPU looping at PC: {cur:#06X}"))
        },
        expected_cycles: Some(21_230_741),
        expected_instructions: Some(8_109_022),
    }, rom_dadc_test, CPU6502, setup_cpu_nmos, step_cpu_nmos,
    dincsbc_test: RomTest{
        filename: "dincsbc.bin",
        nes: false,
        start_pc: 0xD000,
        load_traces: None,
        end_check: |old, cur, _ram| {
            old == cur
        },
        success_check: |_old, cur, _ram| {
            if cur == 0xD004 {
                return Ok(());
            }
            Err(eyre!("CPU looping at PC: {cur:#06X}"))
        },
        expected_cycles: Some(18_939_481),
        expected_instructions: Some(6_781_980),
    }, rom_dincsbc_test, CPU6502, setup_cpu_nmos, step_cpu_nmos,
    dincsbc_deccmp_test: RomTest{
        filename: "dincsbc-deccmp.bin",
        nes: false,
        start_pc: 0xD000,
        load_traces: None,
        end_check: |old, cur, _ram| {
            old == cur
        },
        success_check: |_old, cur, _ram| {
            if cur == 0xD004 {
                return Ok(());
            }
            Err(eyre!("CPU looping at PC: {cur:#06X}"))
        },
        expected_cycles: Some(18_095_480),
        expected_instructions: Some(5_507_189),
    }, rom_dincsbc_deccmp_test, CPU6502, setup_cpu_nmos, step_cpu_nmos,
    droradc_test: RomTest{
        filename: "droradc.bin",
        nes: false,
        start_pc: 0xD000,
        load_traces: None,
        end_check: |old, cur, _ram| {
            old == cur
        },
        success_check: |_old, cur, _ram| {
            if cur == 0xD004 {
                return Ok(());
            }
            Err(eyre!("CPU looping at PC: {cur:#06X}"))
        },
        expected_cycles: Some(22_148_245),
        expected_instructions: Some(8_240_094),
    }, rom_droradc_test, CPU6502, setup_cpu_nmos, step_cpu_nmos,
    dsbc_test: RomTest{
        filename: "dsbc.bin",
        nes: false,
        start_pc: 0xD000,
        load_traces: None,
        end_check: |old, cur, _ram| {
            old == cur
        },
        success_check: |_old, cur, _ram| {
            if cur == 0xD004 {
                return Ok(());
            }
            Err(eyre!("CPU looping at PC: {cur:#06X}"))
        },
        expected_cycles: Some(18_021_977),
        expected_instructions: Some(6_650_908),
    }, rom_dsbc_test, CPU6502, setup_cpu_nmos, step_cpu_nmos,
    dsbc_cmp_flags_test: RomTest{
        filename: "dsbc-cmp-flags.bin",
        nes: false,
        start_pc: 0xD000,
        load_traces: None,
        end_check: |old, cur, _ram| {
            old == cur
        },
        success_check: |_old, cur, _ram| {
            if cur == 0xD004 {
                return Ok(());
            }
            Err(eyre!("CPU looping at PC: {cur:#06X}"))
        },
        expected_cycles: Some(14_425_356),
        expected_instructions: Some(4_982_869),
    }, rom_dsbc_cmp_flags_test, CPU6502, setup_cpu_nmos, step_cpu_nmos,
    sbx_test: RomTest{
        filename: "sbx.bin",
        nes: false,
        start_pc: 0xD000,
        load_traces: None,
        end_check: |old, cur, _ram| {
            if old == cur {
                println!();
                return true
            }
            // On this test it JSR's to FFD2 which is the C64
            // ROM print routine. It prints a dot for each iteration.
            // Do the same for easier debugging if it fails.
            if cur == 0xFFD2 {
                print!(".");
            }
            false
        },
        success_check: |_old, cur, _ram| {
            if cur == 0xD004 {
                return Ok(());
            }
            Err(eyre!("CPU looping at PC: {cur:#06X}"))
        },
        expected_cycles: Some(6_044_288_253),
        expected_instructions: Some(2_081_694_800),
    }, rom_sbx_test, CPU6502, setup_cpu_nmos, step_cpu_nmos,
    vsbx_test: RomTest{
        filename: "vsbx.bin",
        nes: false,
        start_pc: 0xD000,
        load_traces: None,
        end_check: |old, cur, _ram| {
            if old == cur {
                println!();
                return true
            }
            // On this test it JSR's to FFD2 which is the C64
            // ROM print routine. It prints a dot for each iteration.
            // Do the same for easier debugging if it fails.
            if cur == 0xFFD2 {
                print!(".");
            }
            false
        },
        success_check: |_old, cur, _ram| {
            if cur == 0xD004 {
                return Ok(());
            }
            Err(eyre!("CPU looping at PC: {cur:#06X}"))
        },
        expected_cycles: Some(7_525_173_529),
        expected_instructions: Some(2_552_776_790),
    }, rom_vsbx_test, CPU6502, setup_cpu_nmos, step_cpu_nmos,
    bcd_test: RomTest{
        filename: "bcd_test.bin",
        nes: false,
        start_pc: 0xC000,
        load_traces: None,
        end_check: |old, cur, _ram| {
            old == cur || old == 0xC04B
        },
        success_check: |_old, _cur, ram| {
            let val = ram.read(0x0000);
            if val != 0x00 {
                return Err(eyre!("Invalid value at 0x0000: Got {val:#04X} and want 0x00"))
            }
            Ok(())
        },
        expected_cycles: Some(53_953_828),
        expected_instructions: Some(17_609_916),
    }, rom_bcd_test, CPU6502, setup_cpu_nmos, step_cpu_nmos,
    bcd_cmos_test: RomTest{
        filename: "bcd_test_cmos.bin",
        nes: false,
        start_pc: 0xC000,
        load_traces: None,
        end_check: |old, cur, _ram| {
            old == cur || old == 0xC04B
        },
        success_check: |_old, _cur, ram| {
            let val = ram.read(0x0000);
            if val != 0x00 {
                return Err(eyre!("Invalid value at 0x0000: Got {val:#04X} and want 0x00"))
            }
            Ok(())
        },
        expected_cycles: Some(56_640_804),
        expected_instructions: Some(18_396_348),
    }, rom_bcd_cmos_test, CPU65C02, setup_cpu_cmos, step_cpu_cmos,
    undocumented_opcodes_test: RomTest{
        filename: "undocumented.bin",
        nes: false,
        start_pc: 0xC000,
        load_traces: None,
        end_check: |old, cur, _ram| {
            old == cur
        },
        success_check: |_old, cur, _ram| {
            if cur == 0xC11D {
                return Ok(());
            }
            Err(eyre!("CPU looping at PC: {cur:#06X}"))
        },
        // No expected cycles/instructions (just not worth it here).
        expected_cycles: None,
        expected_instructions: None,
    }, rom_undocumented_opcodes_test, CPU6502, setup_cpu_nmos, step_cpu_nmos,
    nes_functional_test: RomTest{
      filename: "nestest.nes",
      nes: true,
      start_pc: 0xC000,
      load_traces: Some(|| -> Result<Vec<Verify>> {
        let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../testdata/nestest.log");
        println!("trace path: {}", path.display());
        let file = File::open(path)?;
        let lines = BufReader::new(file).lines();

        let mut ret = Vec::new();

        for line in lines.map_while(Result::ok) {
            // Each line is 81 characters and each field is a specific offset.
            ret.push(Verify {
                pc: u16::from_str_radix(&line[0..4], 16)?,
                a: u8::from_str_radix(&line[50..52], 16)?,
                x: u8::from_str_radix(&line[55..57], 16)?,
                y: u8::from_str_radix(&line[60..62], 16)?,
                p: Flags(u8::from_str_radix(&line[65..67], 16)?),
                s: u8::from_str_radix(&line[71..73], 16)?,
                cyc: line[78..81].trim().parse::<usize>()?,
            });
        }

        Ok(ret)
      }),
      end_check: |old, _cur, ram| {
            old == 0xC66E || ram.read(0x0002) != 0x00 || ram.read(0x0003) != 0x00
      },
      success_check: |old, _cur, ram| {
          if old == 0xC66E {
              return Ok(());
          }
          Err(eyre!("Error codes - 0x02: {:#06X} 0x03: {:#06X}", ram.read(0x0002), ram.read(0x0003)))
      },
      expected_cycles: Some(26553),
      expected_instructions: Some(8991),
    }, rom_nes_functional_test, CPURicoh, setup_cpu_ricoh, step_cpu_ricoh,
);

#[derive(Deserialize, PartialEq)]
struct ProcRAM(u16, u8);

impl fmt::Display for ProcRAM {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:04X}, {:02X})", &self.0, &self.1)
    }
}

impl fmt::Debug for ProcRAM {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Deserialize, PartialEq)]
struct ProcState {
    pc: u16,
    s: u8,
    a: u8,
    x: u8,
    y: u8,
    p: u8,
    ram: Vec<ProcRAM>,
}

impl fmt::Display for ProcState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "ProcState {{")?;
        writeln!(f, "       pc: {:04X}", self.pc)?;
        writeln!(f, "        a: {:02X}", self.a)?;
        writeln!(f, "        x: {:02X}", self.x)?;
        writeln!(f, "        y: {:02X}", self.y)?;
        writeln!(f, "        s: {:02X}", self.s)?;
        writeln!(f, "        p: {}", Flags(self.p))?;
        if self.ram.is_empty() {
            writeln!(f, "      ram: []")?;
        } else {
            writeln!(f, "      ram: [")?;
            for r in &self.ram {
                writeln!(f, "             {r}")?;
            }
            writeln!(f, "           ]")?;
        }
        write!(f, "}}")
    }
}

impl fmt::Debug for ProcState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{self}")
    }
}

#[derive(Deserialize, PartialEq)]
struct Cycles(u16, u8, LastBusAction);

impl fmt::Display for Cycles {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:04X}, {:02X}, {})", &self.0, &self.1, &self.2)
    }
}

impl fmt::Debug for Cycles {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Deserialize)]
struct ProcTest {
    name: String,
    initial: ProcState,
    r#final: ProcState,
    cycles: Vec<Cycles>,
}

impl fmt::Display for ProcTest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "ProcTest {{")?;
        writeln!(f, "     name: {}", self.name)?;
        writeln!(f, "  initial: {}", self.initial)?;
        writeln!(f, "    final: {}", self.r#final)?;
        if self.cycles.is_empty() {
            writeln!(f, "   cycles: []")?;
        } else {
            writeln!(f, "   cycles: [")?;
            for c in &self.cycles {
                writeln!(f, "             {c}")?;
            }
            writeln!(f, "           ]")?;
        }
        write!(f, "}}")
    }
}

impl fmt::Debug for ProcTest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{self}")
    }
}

macro_rules! coverage_opcodes_test {
        ($suite:ident, $($name:ident: $cov:ident, $cpu:ident, $path:expr, $setup:expr,)*) => {
            mod $suite {
                use super::*;

                $(
                    #[test]
                    fn $name() -> Result<()> {
                        let res = panic::catch_unwind(|| {$cov(false)});
                        if res.is_err() {
                            return $cov(true)
                        }
                        Ok(())
                    }

                    #[allow(clippy::too_many_lines)]
                    fn $cov(dbg: bool) -> Result<()> {
                        // If this isn't set we just skip this test.
                        let Ok(loc) = std::env::var("TOM_HARTE_PROCESSOR_TESTS") else {
                            println!("Skipping tests because TOM_HARTE_PROCESSOR_TESTS isn't set in the environment");
                            println!("Checkout https://github.com/TomHarte/ProcessorTests.git and set env var to point at root of that tree to run relative to this package");
                            println!("Window example: Set-Item -Path \"Env:TOM_HARTE_PROCESSOR_TESTS\" -Value \"../../../TomHarte\"");
                            println!("Unix example: export TOM_HARTE_PROCESSOR_TESTS=../../../TomHarte");
                            panic!("Skipping tests?");
                        };

                        // One CPU for the whole run. Otherwise this is *slow* creating a new
                        // one 256*10000 times...
                        let mut cpu: $cpu<'_>;
                        // Initialize as always but then we'll overwrite it with a ROM image.
                        // For this we'll use BRK and a vector which if executed should halt the processor.
                        let d = Box::new(Debug::<CPUState>::new(128, Some(1)));

                        // If we want debugging setup the debug hook. Otherwise go for faster execution.
                        // Callers should run without debug and then if fails rerun with debug so
                        // normal testing is fast.
                        if dbg {
                            cpu = $setup(0x0000, 0xAA, None, None, None, Some(d.clone()));
                        } else {
                            cpu = $setup(0x0000, 0xAA, None, None, None, None);
                        }
                        cpu.power_on()?;

                        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
                            .join(loc)
                            .join($path);
                        println!("tests base path: {}", path.display());

                        for i in 0..=255 {
                            println!("Opening: {:?}", path.join(format!("{i:02x}.json")));
                            let mut file = File::open(path.join(format!("{i:02x}.json")))?;
                            let mut s = String::new();
                            file.read_to_string(&mut s)?;
                            if s.is_empty() {
                                println!("Skipping due to no tests!");
                                continue;
                            }
                            let mut v: Vec<ProcTest> = serde_json::from_str(&s)?;

                            for test in &mut v {
                                let is_cmos = $path.contains("65c02");
                                let mut check_cycles = true;
                                if is_cmos {
                                    // The TomHarte tests disagree with my impl (based on docs online I could find).
                                    // In some places it felt fine (ADC/SBC D immediate mode 3rd cycle) but others
                                    // don't make as much sense so documenting below.

                                    // BBR/BBS here gets the cycles wrong but otherwise seems to function.
                                    // Issue opened for this one: https://github.com/TomHarte/ProcessorTests/issues/72
                                    if i & 0x0F == 0x0F {
                                        check_cycles = false;
                                    }

                                    // NOP8 but tests think it's 0xDC equiv on WDC and only 4 cycles?
                                    // based on https://laughtonelectronics.com/Arcana/KimKlone/Kimklone_opcode_mapping.html
                                    // which checked a real rockwell I think it's correct and not the tests.
                                    if i == 0x5C {
                                        check_cycles = false;
                                    }

                                    // 0x7C - JMP(A,X)
                                    // Have agreement on cycle count but the extra read
                                    // on cycle 4 appears to be rereading the LSB not
                                    // the MSB of the addr which is weird (the pc would have had to step backwards?)
                                    if i == 0x7C {
                                        check_cycles = false;
                                    }

                                    // This is in theory a NOP A but the cycles don't match
                                    // since this just rereads the last PC instead of the ABS addr
                                    // which it shouldn't have any choice honestly so I think the
                                    // test is wrong.
                                    if i == 0xDC || i == 0xFC {
                                        check_cycles = false;
                                    }
                                }

                                // Setup initial RAM
                                for r in &test.initial.ram {
                                    cpu.ram.borrow_mut().write(r.0, r.1);
                                }
                                let mut out = String::with_capacity(32);
                                cpu.pc = Wrapping(test.initial.pc);
                                cpu.a = Wrapping(test.initial.a);
                                cpu.x = Wrapping(test.initial.x);
                                cpu.y = Wrapping(test.initial.y);
                                cpu.s = Wrapping(test.initial.s);
                                cpu.p = Flags(test.initial.p);
                                cpu.disassemble(&mut out, cpu.pc.0, &*cpu.ram.borrow(), false);

                                let mut bus = vec![];
                                loop {
                                    // Some of these are HLT so account for that.
                                    let ret = cpu.tick();
                                    if let Err(err) = ret {
                                        match err.root_cause().downcast_ref::<CPUError>() {
                                            Some(CPUError::Halted { op: _ }) => {
                                                // If we halted that's fine. The checks below will
                                                // verify state. But set CPU back to running so the
                                                // next test won't error out.
                                                cpu.state = State::Running;
                                                cpu.op_tick = Tick::Reset;
                                            }
                                            _ => {
                                                panic!("Error isn't a CPUError::Halted. Is '{err}'");
                                            }
                                        }
                                    } else {
                                        cpu.tick_done()?;
                                    }
                                    let cpur = cpu.cpu_ram();
                                    let r = cpur.borrow();
                                    let last = r.last_action.borrow();
                                    bus.push(Cycles(last.1, last.2, last.0.clone()));
                                    if cpu.op_tick == Tick::Reset {
                                      break;
                                    }
                                }
                                let mut final_state = ProcState {
                                    pc: cpu.pc.0,
                                    a: cpu.a.0,
                                    x: cpu.x.0,
                                    y: cpu.y.0,
                                    s: cpu.s.0,
                                    p: cpu.p.0,
                                    ram: vec![],
                                };
                                for r in &test.r#final.ram {
                                    let pr = ProcRAM(r.0, cpu.ram().borrow().read(r.0));
                                    final_state.ram.push(pr);
                                }
                                let name = &test.name;
                                let op_tick = cpu.op_tick;
                                tester!(cpu.op_tick == Tick::Reset || cpu.state == State::Halted, d, &cpu,
                                    "{name}: CPU in weird state. Not done with instruction or halted? - {cpu:?} op_tick: {op_tick}\n{test}\n{out}\n{bus:?}"
                                );
                                if check_cycles {
                                    let want = &test.cycles;
                                    tester!(
                                        bus == *want, d, &cpu,
                                        "{name}: Expected cycles\n{want:?}\nand got\n{bus:?}\n{test}\n{final_state}\n{out}"
                                    );
                                }
                                let want = &test.r#final;
                                tester!(
                                    final_state == *want, d, &cpu,
                                    "{name}: Final states don't match. Expected {want} and got {final_state}\n{test}\ncycles: {bus:?}\n{out}"
                                );
                                // Reset all the RAM we touched back to a known value.
                                for r in final_state.ram {
                                    cpu.ram().borrow_mut().write(r.0, 0xAA);
                                }
                                d.reset();
                            }
                        }
                        Ok(())
                    }
                )*
            }
        }
}

coverage_opcodes_test! {
    coverage_opcodes_tests,
    c6502: cov_c6502, CPU6502, "6502/v1", setup_cpu_nmos,
    c6510: cov_c6510, CPU6510, "6502/v1", setup_cpu_nmos_6510,
    ricoh: cov_ricoh, CPURicoh, "nes6502/v1", setup_cpu_ricoh,
    cmos_wdc: cov_cmos_wdc, CPU65C02, "wdc65c02/v1", setup_cpu_cmos,
    cmos_rockwell: cov_cmos_rockwell, CPU65C02Rockwell, "rockwell65c02/v1", setup_cpu_cmos_rockwell,
    cmos_c65sc02: cov_cmos_c65sc02, CPU65SC02, "synertek65c02/v1", setup_cpu_cmos_C65SC02,
}
