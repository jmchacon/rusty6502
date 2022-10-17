//! cpu defines a 6502 CPU which is clock accurate to the supporting environment.
use std::num::Wrapping;

use memory::{Memory, MAX_SIZE};
use thiserror::Error;

use color_eyre::eyre::{eyre, Result};
use rand::Rng;
use strum_macros::{Display, EnumIter, EnumString};

mod lookup;
pub use crate::lookup::*;

mod tests;

/// `AddressMode` defines the 6502 addressing modes.
#[derive(Clone, Copy, Display, Debug, Default, PartialEq, Eq, Hash, EnumString)]
pub enum AddressMode {
    /// `Immediate` mode uses the constant following the opcode to perform the operation.
    /// Example: LDA #04 loads 0x04 into A.
    #[default]
    Immediate,

    /// `ZeroPage` references the first 256 bytes.
    /// Example: LDA 0F would load the value from 0x0F into A.
    ZeroPage,

    /// `ZeroPageX` references the first 256 bytes (zero page) with addition from the X register.
    /// Overflow simply wraps.
    /// Example: LDA 0F,X with X=4 would load the value from 0x13 into A.
    ZeroPageX,

    /// `ZeroPageY` references the first 256 bytes (zero page) with addition from the Y register.
    /// Overflow simply wraps.
    /// Example: LDA 0F,Y with Y=4 would load the value from 0x13 into A.
    ZeroPageY,

    /// `IndirectX` uses the given address from the first 256 bytes (zero page) with addition from the X register.
    /// It then uses this and the following location as a pointer to use as the final address.
    /// Example LDA (04,X) with X = 8 and location 0C,0D have 02 and 01. This would load the value at 0x0102 into A
    /// after adding 8 to 2 and referencing 0x0C.
    IndirectX,

    /// `IndirectY` uses the given address from the first 256 bytes (zero page).
    /// It then uses this and the following address as a pointer. Then Y is added to that to get the final pointer
    /// address to use.
    /// Example LDA (04),Y with Y = 4 and location 04,05 have 02 and 01. This would load the value at 0x0106 into A
    /// after adding 4 to 0x0102.
    IndirectY,

    /// `Absolute` references a direct 16 bit constant as an address.
    /// Example: LDA D000 loads A from 0xD000
    Absolute,

    /// `AbsoluteX` references a direct 16 bit constant as an address and adds X to get the final address.
    /// Example: LDA D000,X with X = 4 loads A from 0xD004
    AbsoluteX,

    /// `AbsoluteY` references a direct 16 bit constant as an address and adds Y to get the final address.
    /// Example: LDA D000,X with Y = 5 loads A from 0xD005
    AbsoluteY,

    /// `Indirect` loads a pointer from the given address and de-references it to get the final address.
    /// Example: JMP (D000) with 0xD000,0xD001 equal to 0x01,0xC0 will jump to 0xC001
    Indirect,

    /// `Implied` takes no arguments and instead operates directly based on the opcode only.
    /// Example: INX increments the X register.
    Implied,

    /// `Relative` takes the argument and adds it as a signed value to the PC to determine the next PC location.
    /// This is used for branching.
    /// Example: D004 D0 FE is a BNE which computes to D004 if true and would infinite loop.
    Relative,
}

// Opcode matric taken from:
// http://wiki.nesdev.com/w/index.php/CPU_unofficial_opcodes#Games_using_unofficial_opcodes
//
// NOTE: The above lists 0xAB as LAX #i but we call it OAL since it has odd behavior and needs
//       it's own code compared to other LAX. See 6502-NMOS.extra.opcodes below.
//
// Description of undocumented opcodes:
//
// http://www.ffd2.com/fridge/docs/6502-NMOS.extra.opcodes
// http://nesdev.com/6502_cpu.txt
// http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_(XAA,_ANE)
//
// Opcode descriptions/timing/etc:
// http://obelisk.me.uk/6502/reference.html

/// `Opcode` defines all the unique 6502 opcodes including undocumented ones.
/// A given implementation may include only some of these (such as the CMOS version).
#[derive(Clone, Copy, Debug, Display, Default, PartialEq, Eq, Hash, EnumIter, EnumString)]
#[strum(ascii_case_insensitive)]
pub enum Opcode {
    /// Add with Carry A with the value at the operand address.
    ADC,

    /// Undocumented opcode AHX. This stores a value as (A & X & (ADDR_HI + 1)).
    AHX,

    /// Undocumented opcode ALR. This does AND #i and then LSR setting all associated flags
    ALR,

    /// Undocumented opcode ANC. This does AND #i and then sets carry based on bit 7 (sign extend).
    ANC,

    /// Bitwise and operation.
    AND,

    /// Undocumented opcode ARR. This does AND #i and then ROR except some flags are set differently.
    ARR,

    /// Arithmetic shift left with possibly carry bit set as a result.
    ASL,

    /// Undocumented opcode AXS. This does (A AND X) - operand (no borrow) setting all associated flags post SBC.
    AXS,

    /// Branch if carry is clear.
    BCC,

    /// Branch if carry is set.
    BCS,

    /// Branch if equal (Z is set).
    BEQ,

    /// Bit test by AND'ing against A and setting N/V based on the value.
    BIT,

    /// Branch on minus (N is set)
    BMI,

    /// Branch if not equal (Z is clear)
    BNE,

    /// Branch on plus (N is clear)
    BPL,

    /// Break execution. Same as an IRQ but software defined. B bit is set in P on stack to indicate source.
    #[default]
    BRK,

    /// Branch if overflow (V) is clear.
    BVC,

    /// Branch if overflow (V) is set.
    BVS,

    /// Clear the C flag.
    CLC,

    /// Clear the D flag.
    CLD,

    /// Clear the I flag.
    CLI,

    /// Clear the V flag.
    CLV,

    /// Compare values with A setting Z based on whether they are equal or not.
    CMP,

    /// Compare values with X setting Z based on whether they are equal or not.
    CPX,

    /// Compare values with Y setting Z based on whether they are equal or not.
    CPY,

    /// Undocumented opcode DCP. This decrements the value at the operand address and then does a CMP with A setting associated flags.
    DCP,

    /// Decrements the value at the operand address.
    DEC,

    /// Decrements the X register.
    DEX,

    /// Decrements the Y register.
    DEY,

    /// Exclusive OR (XOR) A with the value at the operand address.
    EOR,

    /// Undocumented opcode HLT. This will halt the CPU (effectively internally locked up).
    HLT,

    /// Increments the value at the operation address.
    INC,

    /// Increments the X register.
    INX,

    /// Increments the Y register.
    INY,

    /// Undocumented opcode ISC. This increments the value at the operand address and then does an SBC with setting associated flags
    ISC,

    /// Jumps to the value given by the operand address (16 bits).
    JMP,

    /// Jumps to a subroutine given by the operand address (16 bits). Before jumping pushes PC onto the stack so RTS can return to the PC after the JSR instruction.
    JSR,

    /// Undocumented opcode LAS. This take the value at the operand address and ANDs it with S and then stores that in A,X,S setting flags accordingly.
    LAS,

    /// Undocumented opcode LAX.  This loads A and X with the same value from the operand address.
    LAX,

    /// Loads the A register from the value at the operand address.
    LDA,

    /// Loads the X register from the value at the operand address.
    LDX,

    /// Loads the Y register from the value at the operand address.
    LDY,

    /// Logical shift right of the value at the operand address or the A register. Bit 0 is shifted into the C flag.
    LSR,

    /// No operation. Simply burns clock cycles depending on addressing mode.
    NOP,

    /// Undocumented opcode OAL. This one acts a bit randomly. It somtimes does XAA and sometimes does A=X=A&val.
    OAL,

    /// ORs the value in A with the value at the operand address.
    ORA,

    /// Pushes A onto the stack.
    PHA,

    /// Pushes P onto the stack.
    PHP,

    /// Pulls A from the stack.
    PLA,

    /// Pulls P from the stack.
    PLP,

    /// Undocumented opcode RLA. This does a ROL on the value at the operand address and then AND's it against A. Sets flags and carry.
    RLA,

    /// Rotates left the value at the operand address or the A register. Bit 7 is shifted into the C flag and the C flag is shifted into bit 0.
    ROL,

    /// Rotates right the value at the operand address or the A register. Bit 0 is shifted into the C register the C flag is shifted into bit 7.
    ROR,

    /// Undocumented opcode RRA. This does a ROR on the value at the operand address and then ADC's it against A. Sets flags and carry.
    RRA,

    /// Return from interuppt. Should be called when a handler for IRQ/NMI or BRK has been called through their respective vector.
    /// Pops P and PC off the stack and returns execution to the point pre interrupt. The main difference between this and RTS is P
    /// is also popped (and bits set) where-as RTS only deals with PC.
    RTI,

    /// Return from subroutine. Pops the PC from the stack and sets execution to continue at that value.
    RTS,

    /// Undocumented instruction SAX. Store A and X to the location specified in the operand address.
    SAX,

    /// Subtract with carry the value in A with the value at the operand address.
    SBC,

    /// Set the C flag.
    SEC,

    /// Set the D flag.
    SED,

    /// Set the I flag.
    SEI,

    /// Undocumented instruction SHX. Similar to AHX but the value stored is (X & (ADDR_HI + 1))
    SHX,

    /// Undocumented instruction SHX. Similar to AHX but the value stored is (Y & (ADDR_HI + 1))
    SHY,

    /// Undocumented instruction SLO. This does an ASL on the value at the operand address and then OR's it against A. Sets flags and carry
    SLO,

    /// Undocumented instruction SRE. This does a LSR on the value at the operand address and then EOR's it against A. Sets flags and carry.
    SRE,

    /// Stores the A register at the operand address.
    STA,

    /// Stores the X register at the operand address.
    STX,

    /// Stores the Y register at the operand address.
    STY,

    /// Undocumented instruction TAS. This does the same operations as AHX but then also sets S = A&X.
    TAS,

    /// Loads the X register with the value of the A register.
    TAX,

    /// Loads the Y register with the value of the A register.
    TAY,

    /// Loads the X register with the value of the S register.
    TSX,

    /// Loads the A register with the value of the X register.
    TXA,

    /// Loads the S register with the value of the X register. No flags are set from S loads.
    TXS,

    /// Loads the A register with the value of the Y register.
    TYA,

    /// Undocumented instruction XAA. We'll go with http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_(XAA,_ANE)
    /// for implementation and pick 0xEE as the constant. According to VICE this may break so might need to change it to 0xFF
    /// https://sourceforge.net/tracker/?func=detail&aid=2110948&group_id=223021&atid=1057617
    XAA,
}

/// Type defines the various implementations of the 6502 available.
#[derive(Copy, Clone, Debug, Display, PartialEq, Eq, EnumString)]
pub enum Type {
    /// Basic NMOS 6502 including all undocumented opcodes.
    NMOS,

    /// Ricoh version used in the NES which is identical to NMOS except BCD mode is unimplemented.
    #[strum(to_string = "NMOS_RICOH")]
    Ricoh,

    /// NMOS 6501 variant (used in c64) which includes I/O ports mapped at addresses 0x00 and 0x01.
    #[strum(to_string = "NMOS_6510")]
    NMOS6510,

    /// 65C02 CMOS version where undocumented opcodes are all explicit NOP's and defined.
    CMOS,
}

#[derive(Debug, Default, Copy, Clone)]
/// Operation defines an opcode plus its addressing mode together. This will be combined in hashes
/// with the actual u8 value as the key.
pub struct Operation {
    /// op is the Opcode such as ADC, LDA, etc.
    pub op: Opcode,
    /// AddressMode is a valid addressing mode for this opcode such as Absolute.
    pub mode: AddressMode,
}

/// `NMI_VECTOR` is the location in memory the 6502 uses for NMI interrupts.
/// It is a pointer to the location to start execution.
pub const NMI_VECTOR: u16 = 0xFFFA;

/// `RESET_VECTOR` is the location in memory the 6502 uses on startup to begin execution.
/// It is a pointer to the location to start execution.
pub const RESET_VECTOR: u16 = 0xFFFC;

/// `IRQ_VECTOR` is the location in memory the 6502 uses when executing an IRQ (BRK).
/// It is a pointer to the location to start execution.
pub const IRQ_VECTOR: u16 = 0xFFFE;

#[derive(Debug, Display, Copy, Clone, PartialEq, EnumString)]
enum CPUState {
    // The default when initialized.
    // Will throw errors in any functions if this state because `power_on`
    // hasn't been called.
    Off,

    // The state `power_on` leaves the chip which generally moves
    // immediate into Reset.
    On,

    // If we're in a reset sequence and not done yet.
    Reset,

    // Set in `reset` or `tick_done`
    Running,

    // The mode we are in after a `tick` has run
    Tick,

    // If we ran a halted instruction we end up here.
    Halted,
}

#[derive(Default)]
enum IRQ {
    #[default]
    None,
    Irq,
    Nmi,
}
// Used to indicate whether an opcode/addressing mode is done or not.
#[derive(Copy, Clone, PartialEq)]
enum OpState {
    Done,
    Processing,
}

// The various types of instruction modes internally used.
enum InstructionMode {
    Load,
    Rmw,
    Store,
}

// Clock ticks
#[derive(Debug, Default, Display, Copy, Clone, PartialEq, EnumString)]
enum Tick {
    // The reset state. Used to start a new instruction assuming all
    // processing immediately calls `next` before evaluatin.
    #[default]
    Reset,

    // This is the normal start case for a new instruction.
    Tick1,
    Tick2,
    Tick3,
    Tick4,
    Tick5,
    Tick6,
    Tick7,

    // Technically documented 6502 instructions take no more than 7 cycles.
    // But a RMW indirect X/Y will take 8.
    Tick8,
}

impl Tick {
    fn next(self) -> Self {
        match self {
            Tick::Reset => Tick::Tick1,
            Tick::Tick1 => Tick::Tick2,
            Tick::Tick2 => Tick::Tick3,
            Tick::Tick3 => Tick::Tick4,
            Tick::Tick4 => Tick::Tick5,
            Tick::Tick5 => Tick::Tick6,
            Tick::Tick6 => Tick::Tick7,
            Tick::Tick7 | Tick::Tick8 => Tick::Tick8,
        }
    }
}

/// The definition for a given 6502 implementation.
pub struct Cpu<'a> {
    // The specific variant implemented.
    cpu_type: Type,

    /// Accumulator register
    pub a: Wrapping<u8>,

    /// X register
    pub x: Wrapping<u8>,

    /// Y register
    pub y: Wrapping<u8>,

    /// Stack pointer
    pub s: Wrapping<u8>,

    /// Status register
    pub p: u8,

    /// Program counter
    pub pc: Wrapping<u16>,

    // If true `debug` will return data.
    debug: bool,

    // Initialized or not.
    state: CPUState,

    // State of IRQ line
    irq: IRQ,

    // Whether we're currently running an interrupt
    running_interrupt: bool,

    // Tracking for reset when we need to clear the extra clocks
    // up front before simulating BRK. If `tick` is called and this
    // isn't in Tick::Reset an error will result.
    reset_tick: Tick,

    // Total number of clock cycles since start.
    clocks: usize,

    // Memory implementation
    ram: &'a mut dyn Memory,

    // The current working opcode
    op: Operation,
    op_raw: u8,

    // The 1st byte argument after the opcode (all instruction have this).
    // Often used as a temp value while building the whole instruction.
    op_val: u8,

    // Tick number for internal operation of opcode.
    op_tick: Tick,

    // Address computed during opcode to be used for read/write (indirect, etc modes).
    op_addr: u16,

    // Stays OpState::Processing until the current opcode has completed any addressing mode ticks.
    // NOTE: This is instruction dependent as to whether it gets updated. i.e. NOP may just run through
    //       addressing mode cycles and complete without bothering to set this since nothing else in that
    //       instruction will care. Constrast to a RMW instruction which has to run a cycle one past when
    //       this is marked Done.
    addr_done: OpState,

    // Whether we've previously skipped processing an interrupt due to 2 firing back to back.
    prev_skip_interrupt: bool,

    // The opcode value used to halt the CPU
    halt_opcode: u8,

    // The PC value of the halt instruction
    halt_pc: u16,
}

const P_NEGATIVE: u8 = 0x80;
const P_OVERFLOW: u8 = 0x40;
const P_S1: u8 = 0x20; // Always on
const P_B: u8 = 0x10; // Only set when pushing onto the stack during BRK.
const P_DECIMAL: u8 = 0x08;
const P_INTERRUPT: u8 = 0x04;
const P_ZERO: u8 = 0x02;
const P_CARRY: u8 = 0x01;

/// Define the characteristics of the 6502 wanted.
pub struct ChipDef<'a> {
    /// The CPU type.
    pub cpu_type: Type,

    /// Memory implementation.
    pub ram: &'a mut dyn Memory,

    /// If true debugging is enabled.
    /// TODO: Define the interface for this.
    pub debug: bool,
}

/// `CPUError` defines specific conditions where `tick` may return
/// an error. Use this to determine specific internal issues.
#[derive(Error, Debug)]
pub enum CPUError {
    /// Halted indicates the CPU has halted and the specific opcode
    /// which triggered it.
    #[error("Halted condition opcode: {op:02X}")]
    Halted {
        /// op is the opcode which triggered the current Halted state.
        op: u8,
    },
}

impl<'a> Cpu<'a> {
    /// Build a new Cpu.
    /// NOTE: This is not usable at this point. Call `power_on` to begin
    /// operation. Anything else will return errors.
    #[must_use]
    pub fn new(def: &'a mut ChipDef) -> Self {
        Cpu {
            cpu_type: def.cpu_type,
            a: Wrapping(0x00),
            x: Wrapping(0x00),
            y: Wrapping(0x00),
            s: Wrapping(0x00),
            p: 0x00,
            pc: Wrapping(0x0000),
            debug: def.debug,
            state: CPUState::Off,
            irq: IRQ::default(),
            running_interrupt: false,
            clocks: 0,
            ram: def.ram,
            op: Operation::default(),
            op_raw: 0x00,
            op_val: 0x00,
            op_tick: Tick::Reset,
            reset_tick: Tick::Reset,
            op_addr: 0x0000,
            addr_done: OpState::Done,
            prev_skip_interrupt: false,
            halt_opcode: 0x00,
            halt_pc: 0x0000,
        }
    }

    /// `power_on` will reset the CPU to power on state which isn't well defined.
    /// Registers are random and stack is at random (though visual 6502 claims it's 0xFD
    /// due to a push P/PC in reset which gets run at power on).
    /// P is cleared with interrupts disabled and decimal mode random (for NMOS versions).
    /// The starting PC value is loaded from the reset vector.
    ///
    /// # Errors
    /// If reset has any issues an error will be returned.
    ///
    /// TODO(jchacon): See if any of this gets more defined on CMOS versions.
    pub fn power_on(&mut self) -> Result<()> {
        if self.state != CPUState::Off {
            return Err(eyre!("cannot power on except from an off state"));
        }

        let mut rng = rand::thread_rng();

        // This is always set and clears the rest.
        self.p = P_S1;

        // Randomize decimal for CPU's which implement and aren't CMOS.
        self.p |= match self.cpu_type {
            Type::NMOS | Type::NMOS6510 => {
                if rng.gen::<f64>() > 0.5 {
                    P_DECIMAL
                } else {
                    0x00
                }
            }
            _ => 0x00,
        };

        // Randomize register contents
        self.a = rng.gen();
        self.x = rng.gen();
        self.y = rng.gen();
        self.s = rng.gen();

        self.state = CPUState::On;

        // Use reset to get everything else done.
        loop {
            match self.reset() {
                Ok(true) => break,
                Ok(false) => continue,
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }

    /// reset is similar to `power_on` except the main registers are not touched. The stack reset to 0x00
    /// and then moved 3 bytes as if PC/P have been pushed though R/W is only set to read so nothing
    /// changes. Flags are not disturbed except for interrupts being disabled
    /// and the PC is loaded from the reset vector.
    /// There are 2 cycles of "setup" before the same sequence as BRK happens (internally it forces BRK
    /// into the IR). It holds the R/W line high so no writes happen but otherwise the sequence is the same.
    /// Visual 6502 simulation shows this all in detail.
    /// This takes 7 cycles once triggered (same as interrupts).
    /// Will return true when reset is complete and errors if any occur.
    ///
    /// # Errors
    /// Internal problems (getting into the wrong state) can result in errors.
    ///
    /// TODO: Can we reuse the BRK code for this?
    pub fn reset(&mut self) -> Result<bool> {
        if self.state == CPUState::Off {
            return Err(eyre!("power_on not called before calling reset!"));
        }

        // If we haven't started a reset sequence start it now.
        if self.state != CPUState::Reset {
            self.state = CPUState::Reset;
            self.op_tick = Tick::Reset;
            self.reset_tick = Tick::Tick1;
        }
        self.op_tick = self.op_tick.next();
        self.clocks += 1;

        match self.reset_tick {
            Tick::Tick1 | Tick::Tick2 => {
                // Burn off 2 clocks internally to reset before we start processing.
                // TODO: This does trigger bus reads so figure those out and do them.
                self.reset_tick = self.reset_tick.next();

                // Leave op_tick in reset mode so once we're done here it'll match below
                // as Tick1.
                self.op_tick = Tick::Reset;

                Ok(false)
            }
            Tick::Tick3 => {
                match self.op_tick {
                    Tick::Tick1 => {
                        // Standard first tick reads current PC value which normally
                        // is the opcode but we discard.
                        self.ram.read(self.pc.0);
                        self.pc += 1;

                        // Reset our other internal state

                        // If we were halted before, clear that out.
                        self.halt_opcode = 0x00;
                        self.halt_pc = 0x0000;

                        // The stack ends up at 0xFD which implies it gets set to 0x00 now
                        // as we pull 3 bytes off the stack in the end.
                        // TODO: Double check this in visual 6502.
                        self.s = Wrapping(0x00);
                        Ok(false)
                    }
                    Tick::Tick2 => {
                        // Read another throw away value which is normally the opval but
                        // discarded as well.
                        self.ram.read(self.pc.0);
                        self.pc += 1;
                        Ok(false)
                    }
                    Tick::Tick3 | Tick::Tick4 => {
                        // Tick3: Simulate pushing P onto stack but reset holds R/W high so we don't write.
                        // Tick4: Simulate writing low byte of PC onto stack. Same rules as Tick3.
                        // These reads go nowhere (technically they end up in internal regs but since that's
                        // not visible externally, who cares?).
                        let addr: u16 = 0x0100 + u16::from(self.s.0);
                        self.ram.read(addr);
                        self.s -= 1;
                        Ok(false)
                    }
                    Tick::Tick5 => {
                        // Final write to stack (PC high) but actual read due to being in reset.
                        let addr: u16 = 0x0100 + u16::from(self.s.0);
                        self.ram.read(addr);
                        self.s -= 1;

                        // Disable interrupts
                        self.p |= P_INTERRUPT;

                        // On NMOS D is random after reset.
                        let mut rng = rand::thread_rng();
                        self.p &= !P_DECIMAL;

                        self.p |= match self.cpu_type {
                            Type::NMOS | Type::NMOS6510 => {
                                if rng.gen::<f64>() > 0.5 {
                                    P_DECIMAL
                                } else {
                                    0x00
                                }
                            }
                            _ => 0x00,
                        };
                        Ok(false)
                    }
                    Tick::Tick6 => {
                        // Load PCL from reset vector
                        self.op_val = self.ram.read(RESET_VECTOR);
                        Ok(false)
                    }
                    Tick::Tick7 => {
                        // Load PCH from reset vector and go back into normal operations.
                        self.pc = Wrapping(u16::from(self.ram.read(RESET_VECTOR + 1)) << 8)
                            + Wrapping(u16::from(self.op_val));
                        self.reset_tick = Tick::Reset;
                        self.op_tick = Tick::Reset;
                        self.state = CPUState::Running;
                        Ok(true)
                    }
                    // Technically both this and the reset_tick one below are impossible
                    // sans bugs here as tick() won't run while we're in reset and it's the only other
                    // way to modify the sequence.
                    _ => Err(eyre!("invalid op_tick in reset: {}", self.op_tick)),
                }
            }
            _ => Err(eyre!("invalid reset_tick: {}", self.reset_tick)),
        }
    }

    /// `tick` is used to move the clock forward one tick and either start processing
    /// a new opcode or continue on one already started.
    ///
    /// # Errors
    /// Bad internal state or causing a halt of the CPU will result in errors.
    ///
    /// Bad internal state includes not powering on, not completing a reset sequence
    /// and already being halted.
    pub fn tick(&mut self) -> Result<()> {
        // Handles improper state. Only tick_done() or reset() can move into this state.
        if self.state != CPUState::Running {
            return Err(eyre!("CPU not in Running state - {}", self.state));
        }

        // Move to the state tick_done() has to move us out of.
        self.state = CPUState::Tick;

        // Always bump the clock count and op_tick.
        self.clocks += 1;
        self.op_tick = self.op_tick.next();

        match self.op_tick {
            Tick::Tick1 => {
                // If we're in Tick1 this means start a new instruction based on the PC value so grab
                // the opcode now and look it up.
                self.op_raw = self.ram.read(self.pc.0);
                self.op = opcode_op(self.cpu_type, self.op_raw);

                // PC advances always when we start a new opcode
                // TODO(jchacon): Handle interrupts
                self.pc += 1;

                // Move out of the done state.
                self.addr_done = OpState::Processing;
                return Ok(());
            }
            Tick::Tick2 => {
                // All instructions fetch the value after the opcode (though some like BRK/PHP/etc ignore it).
                // We keep it since some instructions such as absolute addr then require getting one
                // more byte. So cache at this stage since we no idea if it's needed.
                // NOTE: the PC doesn't increment here as that's dependent on addressing mode which will handle it.
                self.op_val = self.ram.read(self.pc.0);
            }
            // The remainder don't do anything general per cycle so they can be ignored. Opcode processing determines
            // when they are done and all of them do sanity checking for Tick range.
            _ => {}
        }

        // Process the opcode
        let ret = self.process_opcode();

        // We'll treat an error as halted since internal state is unknown at that point.
        // For halted instructions though we'll return a custom error type so it can
        // be detected vs internal errors.
        if self.state == CPUState::Halted || ret.is_err() {
            self.state = CPUState::Halted;
            self.halt_opcode = self.op_raw;
            ret?;
            return Err(eyre!(CPUError::Halted {
                op: self.halt_opcode,
            }));
        }
        if let Ok(state) = ret {
            if state == OpState::Done {
                // Reset so the next tick starts a new instruction
                // It'll handle doing start of instruction reset on state.
                self.op_tick = Tick::Reset;

                // If we're already running an IRQ clear state so we don't loop
                // trying to start it again.
                if self.running_interrupt {
                    self.irq = IRQ::None;
                }
                self.running_interrupt = false;
            }
        }
        Ok(())
    }

    /// `tick_done` moves the CPU back to a state where the next tick can run.
    /// For the 6502 there are no internal latches so generally there is no shadow
    /// state to account but all Chip implementations need this function.
    ///
    /// # Errors
    /// Bad internal state will result in errors.
    pub fn tick_done(&mut self) -> Result<()> {
        if self.state != CPUState::Tick {
            return Err(eyre!("tick_done called outside Tick - {}", self.state));
        }
        // Move to the next state.
        self.state = CPUState::Running;
        Ok(())
    }

    fn process_opcode(&mut self) -> Result<OpState> {
        match (self.op.op, self.op.mode) {
            // 0x00 - BRK #i
            (Opcode::BRK, AddressMode::Immediate) => self.brk(),
            // 0x01 - ORA (d,x)
            (Opcode::ORA, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::ora)
            }
            // 0x02 0x12 - HLT
            (Opcode::HLT, AddressMode::Implied) => {
                self.state = CPUState::Halted;
                Ok(OpState::Done)
            }
            // 0x03 - SLO (d,x)
            (Opcode::SLO, AddressMode::IndirectX) => {
                self.rmw_instruction(Self::addr_indirect_x, Self::slo)
            }
            // 0x04 - NOP d
            (Opcode::NOP, AddressMode::ZeroPage) => self.addr_zp(&InstructionMode::Load),
            // 0x05 - ORA d
            (Opcode::ORA, AddressMode::ZeroPage) => self.load_instruction(Self::addr_zp, Self::ora),
            // 0x06 - ASL d
            (Opcode::ASL, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::asl),
            // 0x07 - SLO d
            (Opcode::SLO, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::slo),
            // 0x08 - PHP
            (Opcode::PHP, AddressMode::Implied) => self.php(),
            // 0x09 - ORA #i
            (Opcode::ORA, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::ora)
            }
            // 0x0A - ASL
            (Opcode::ASL, AddressMode::Implied) => self.asl_acc(),
            // 0x0B - ANC #i
            (Opcode::ANC, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::anc)
            }
            // 0x0C - NOP a
            (Opcode::NOP, AddressMode::Absolute) => self.addr_absolute(&InstructionMode::Load),
            // 0x0D - ORA a
            (Opcode::ORA, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::ora)
            }
            // 0x0E - ASL a
            (Opcode::ASL, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::asl)
            }
            // 0x0F - SLO a
            (Opcode::SLO, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::slo)
            }
            // 0x10 - BPL *+r
            (Opcode::BPL, AddressMode::Relative) => self.bpl(),
            // 0x11 - ORA (d),y
            (Opcode::ORA, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::ora)
            }
            // 0x12 - see 0x02
            // 0x13 - SLO (d),y
            (Opcode::SLO, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::slo)
            }
            // 0x14 - NOP d,x
            (Opcode::NOP, AddressMode::ZeroPageX) => self.addr_zp_x(&InstructionMode::Load),
            // 0x15 - ORA d,x
            (Opcode::ORA, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::ora)
            }
            // 0x16 - ASL d,x
            (Opcode::ASL, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::asl)
            }
            // 0x17 - SLO d,x
            (Opcode::SLO, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::slo)
            }
            // 0x18 - CLC
            (Opcode::CLC, AddressMode::Implied) => self.clc(),
            // 0x19 - ORA a,y
            (Opcode::ORA, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::ora)
            }
            // 0x1A - NOP
            (Opcode::NOP, AddressMode::Implied) => Ok(OpState::Done),
            // 0x1B - SLO a,y
            (Opcode::SLO, AddressMode::AbsoluteY) => {
                self.rmw_instruction(Self::addr_absolute_y, Self::slo)
            }
            // 0x1C - NOP a,x
            (Opcode::NOP, AddressMode::AbsoluteX) => self.addr_absolute_x(&InstructionMode::Load),
            // 0x1D - ORA a,x
            (Opcode::ORA, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::ora)
            }
            // 0x1E - ASL a,x
            (Opcode::ASL, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::asl)
            }
            // 0x1F - SLO a,x
            (Opcode::SLO, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::slo)
            }
            // 0x20 - JSR a
            (Opcode::JSR, AddressMode::Absolute) => self.jsr(),
            // 0xAA - TAX
            (Opcode::TAX, AddressMode::Implied) => {
                Self::load_register(&mut self.p, &mut self.x, self.a.0)
            }
            _ => todo!("implement for {:?} {:?}", self.op.op, self.op.mode),
        }
    }

    // load_register takes the val and inserts it into the given register.
    // It then does Z and N checks against the new value and sets flags.
    // Always returns OpState::Done as this happens on a single tick.
    // NOTE: This isn't a Self method as that would require passing both
    //       a mutable self and a reference to the register which double borrows.
    //       Instead this breaks out the pieces needed which means this and the status
    //       checks below are standalone methods.
    #[allow(clippy::unnecessary_wraps)]
    fn load_register(flags: &mut u8, reg: &mut Wrapping<u8>, val: u8) -> Result<OpState> {
        *reg = Wrapping(val);
        Self::zero_check(flags, val);
        Self::negative_check(flags, val);
        Ok(OpState::Done)
    }

    // zero_check sets the Z flag based on the value.
    fn zero_check(flags: &mut u8, val: u8) {
        *flags &= !P_ZERO;
        if val == 0 {
            *flags |= P_ZERO;
        }
    }

    // negative_check sets the N flag based on the value.
    fn negative_check(flags: &mut u8, val: u8) {
        *flags &= !P_NEGATIVE;
        if (val & P_NEGATIVE) == 0x80 {
            *flags |= P_NEGATIVE;
        }
    }

    // carry_check sets the C flag based on the value.
    fn carry_check(flags: &mut u8, val: u16) {
        *flags &= !P_CARRY;
        if val >= 0x100 {
            *flags |= P_CARRY;
        }
    }

    // overflow_check sets the V flag is the result of the ALU operation
    // caused a two's complement sign change.
    // Taken from http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    fn overflow_check(flags: &mut u8, reg: u8, arg: u8, result: u8) {
        *flags &= !P_OVERFLOW;
        // If the original sign differs from the end sign bit.
        if (reg ^ result) & (arg ^ result) & 0x80 != 0x00 {
            *flags |= P_OVERFLOW;
        }
    }

    // load_instruction abstracts all load instruction opcodes. The address mode function is
    // used to get the proper values loaded into op_addr and op_val.
    // Then on the same tick this is done op_func is called to load the appropriate register.
    // Returns OpState::Done when complete and/or any error.
    fn load_instruction(
        &mut self,
        address_mode: fn(&mut Self, &InstructionMode) -> Result<OpState>,
        op_func: fn(&mut Self) -> Result<OpState>,
    ) -> Result<OpState> {
        self.addr_done = address_mode(self, &InstructionMode::Load)?;
        match self.addr_done {
            OpState::Processing => Ok(OpState::Processing),
            OpState::Done => op_func(self),
        }
    }

    // rmw_instruction abstracts all RMW instruction opcodes. The address mode function is
    // used to get the proper values loaded into op_addr and op_val.
    // This assumes the address mode function also handle the extra write RMW instructions perform.
    // Then on the next tick the op_func is called to perform the final write operation.
    // Returns OpState::Done when complete and/or any error.
    fn rmw_instruction(
        &mut self,
        address_mode: fn(&mut Self, &InstructionMode) -> Result<OpState>,
        op_func: fn(&mut Self) -> Result<OpState>,
    ) -> Result<OpState> {
        match self.addr_done {
            OpState::Processing => {
                self.addr_done = address_mode(self, &InstructionMode::Rmw)?;
                Ok(OpState::Processing)
            }
            OpState::Done => op_func(self),
        }
    }

    // store_instruction abstracts all store instruction opcodes.  The address mode function is
    // used to get the proper values loaded into op_addr and op_val.
    // Then on the next tick the op_func is called to perform the final write operation.
    // Returns OpState::Done when complete and/or any error.
    fn store_instruction(
        &mut self,
        address_mode: fn(&mut Self, &InstructionMode) -> Result<OpState>,
        op_func: fn(&mut Self) -> Result<OpState>,
    ) -> Result<OpState> {
        match self.addr_done {
            OpState::Processing => {
                self.addr_done = address_mode(self, &InstructionMode::Store)?;
                Ok(OpState::Processing)
            }
            OpState::Done => op_func(self),
        }
    }

    // addr_absolute implements Absolute mode - a
    // returning the value in op_val and the address read in op_addr (so RW operations can do things without having to
    // reread memory incorrectly to compute a storage address).
    // If mode is RMW then another tick will occur that writes the read value back to the same address due to how
    // the 6502 operates.
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_absolute(&mut self, mode: &InstructionMode) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 | Tick::Tick6 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("addr_absolute invalid op_tick: {:?}", self.op_tick))
            }
            Tick::Tick2 => {
                // op_val has the first start of the address so start computing it.
                self.op_addr = u16::from(self.op_val) & 0x00FF;
                self.pc += 1;
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                self.op_val = self.ram.read(self.pc.0);
                self.pc += 1;
                self.op_addr |= u16::from(self.op_val) << 8;
                match mode {
                    // For a store we're done since the opcode can decide what to store.
                    InstructionMode::Store => Ok(OpState::Done),
                    _ => Ok(OpState::Processing),
                }
            }
            Tick::Tick4 => {
                // For load and RMW we read the value
                self.op_val = self.ram.read(self.op_addr);
                match mode {
                    &InstructionMode::Load => Ok(OpState::Done),
                    _ => Ok(OpState::Processing),
                }
            }
            Tick::Tick5 => {
                self.ram.write(self.op_addr, self.op_val);
                Ok(OpState::Done)
            }
        }
    }

    // addr_absolute_x implements Absolute X mode - a,x
    // returning the value in op_val and the address read in op_addr (so RW operations can do things without having to
    // reread memory incorrectly to compute a storage address).
    // If mode is RMW then another tick will occur that writes the read value back to the same address due to how
    // the 6502 operates.
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_absolute_x(&mut self, mode: &InstructionMode) -> Result<OpState> {
        self.addr_absolute_xy(mode, self.x.0)
    }

    // addr_absolute_y implements Absolute Y mode - a,y
    // returning the value in op_val and the address read in op_addr (so RW operations can do things without having to
    // reread memory incorrectly to compute a storage address).
    // If mode is RMW then another tick will occur that writes the read value back to the same address due to how
    // the 6502 operates.
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_absolute_y(&mut self, mode: &InstructionMode) -> Result<OpState> {
        self.addr_absolute_xy(mode, self.y.0)
    }

    // TODO(jchacon): Double check all address computations in addr_X functions. Likely all need Wrapping around them.

    // addr_absolute_xy implements the details for addr_absolute_x and addr_absolute_y since they only differ based on the register used.
    // See those functions for arg/return specifics.
    fn addr_absolute_xy(&mut self, mode: &InstructionMode, reg: u8) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("addr_absolute invalid op_tick: {:?}", self.op_tick))
            }
            Tick::Tick2 => {
                // op_val has the first start of the address so start computing it.
                self.op_addr = u16::from(self.op_val) & 0x00FF;
                self.pc += 1;
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                self.op_val = self.ram.read(self.pc.0);
                self.pc += 1;
                self.op_addr |= u16::from(self.op_val) << 8;
                // Add reg but do it in a way which won't page wrap (if needed).
                let a = (self.op_addr & 0xFF00) + u16::from((self.op_addr & 0x00FF) as u8 + reg);
                self.op_val = 0;
                if a != self.op_addr + u16::from(reg) {
                    // Signal for next phase fixup is needed
                    self.op_val = 1;
                }
                self.op_addr = a;
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                let t = self.op_val;
                self.op_val = self.ram.read(self.op_addr);
                // Check old opVal to see if it's non-zero. If so it means the reg addition
                // crosses a page boundary and we'll have to fixup.
                // For a load operation that means another tick to read the correct
                // address.
                // For RMW it doesn't matter (we always do the extra tick).
                // For Store we're done. Just fixup p.opAddr so the return value is correct.
                let mut done = Ok(OpState::Done);
                if t != 0 {
                    self.op_addr += 0x0100;
                    if let InstructionMode::Load = mode {
                        done = Ok(OpState::Processing);
                    }
                }
                // For RMW it doesn't matter, we tick again.
                if let InstructionMode::Rmw = mode {
                    done = Ok(OpState::Processing);
                }
                done
            }
            Tick::Tick5 => {
                // Optional (on load) in case adding reg went past a page boundary.
                self.op_val = self.ram.read(self.op_addr);
                if let InstructionMode::Rmw = mode {
                    Ok(OpState::Processing)
                } else {
                    Ok(OpState::Done)
                }
            }
            Tick::Tick6 => {
                self.ram.write(self.op_addr, self.op_val);
                Ok(OpState::Done)
            }
        }
    }

    // addr_immediate implements Immediate mode - #i
    // returning the value in op_val.
    // NOTE: This has no mode other than load so the argument is ignored.
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_immediate(&mut self, _mode: &InstructionMode) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset
            | Tick::Tick1
            | Tick::Tick3
            | Tick::Tick4
            | Tick::Tick5
            | Tick::Tick6
            | Tick::Tick7
            | Tick::Tick8 => Err(eyre!("addr_immediate invalid op_tick: {:?}", self.op_tick)),
            Tick::Tick2 => {
                // Already read the value but need to bump the PC
                // since this mode consumes op_val.
                self.pc += 1;
                Ok(OpState::Done)
            }
        }
    }

    // addr_indirect_x implements Zero page indirect plus X mode - (d,x)
    // returning the value in op_val and the address read in op_addr (so RW operations can do things without having to
    // reread memory incorrectly to compute a storage address).
    // If mode is RMW then another tick will occur that writes the read value back to the same address due to how
    // the 6502 operates.
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_indirect_x(&mut self, mode: &InstructionMode) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 | Tick::Tick8 => {
                Err(eyre!("addr_indirect_x invalid op_tick: {:?}", self.op_tick))
            }
            Tick::Tick2 => {
                // We've already read the value but need to bump the PC
                // and assign it into op_addr so the throw away read in
                // tick3 reads the right place.
                self.op_addr = u16::from(self.op_val) & 0x00FF;
                self.pc += 1;
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // A throwaway read from the ZP addr. We'll add the X register as well for the real read next.
                self.ram.read(self.op_addr);
                // Does this as a Wrapping so it wraps as needed since it stays in ZP.
                self.op_addr = u16::from((Wrapping(self.op_val) + self.x).0);
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // Read effective addr low byte
                self.op_val = self.ram.read(self.op_addr);
                // Now increment (with ZP rollover) for next read.
                // There is no truncation since we know this is always
                // 0-255.
                #[allow(clippy::cast_possible_truncation)]
                let a = Wrapping(self.op_addr as u8);
                self.op_addr = u16::from((a + Wrapping(1)).0);
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                // Read high byte, shift over and add op_val which has the low byte.
                self.op_addr =
                    (u16::from(self.ram.read(self.op_addr)) << 8) + u16::from(self.op_val);
                match mode {
                    // For a store we're done as op_addr now contains the destination address.
                    InstructionMode::Store => Ok(OpState::Done),
                    _ => Ok(OpState::Processing),
                }
            }
            Tick::Tick6 => {
                self.op_val = self.ram.read(self.op_addr);
                match mode {
                    // For a load we're done as we've loaded the value.
                    InstructionMode::Load => Ok(OpState::Done),
                    _ => Ok(OpState::Processing),
                }
            }
            Tick::Tick7 => {
                self.ram.write(self.op_addr, self.op_val);
                Ok(OpState::Done)
            }
        }
    }

    // addr_indirect_y implements Zero page indirect plus Y mode - (d),y
    // returning the value in op_val and the address read in op_addr (so RW operations can do things without having to
    // reread memory incorrectly to compute a storage address).
    // If mode is RMW then another tick will occur that writes the read value back to the same address due to how
    // the 6502 operates.
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_indirect_y(&mut self, mode: &InstructionMode) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 | Tick::Tick8 => {
                Err(eyre!("addr_indirect_y invalid op_tick: {:?}", self.op_tick))
            }
            Tick::Tick2 => {
                // We've already read the value but need to bump the PC
                // and assign it into op_addr so the throw away read in
                // tick3 reads the right place.
                self.op_addr = u16::from(self.op_val) & 0x00FF;
                self.pc += 1;
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // Read from the ZP addr to start building our pointer.
                self.op_val = self.ram.read(self.op_addr);
                // Setup op_addr for next read and handle ZP wrapping.
                #[allow(clippy::cast_possible_truncation)]
                let a = u16::from((self.op_addr as u8) + 1);
                self.op_addr = a;
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // Compute effective address and then add Y to it (possibly wrongly).
                self.op_addr =
                    (u16::from(self.ram.read(self.op_addr)) << 8) + u16::from(self.op_val);
                // Add Y but do it in a way which won't page wrap (if needed).
                #[allow(clippy::cast_possible_truncation)]
                let a = (self.op_addr & 0xFF00) + u16::from((self.op_addr as u8) + self.y.0);
                self.op_val = 0;
                if a != (self.op_addr + u16::from(self.y.0)) {
                    // Signal for next phase we got it wrong.
                    self.op_val = 1;
                }
                self.op_addr = a;
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                // Save op_val so we know if this needed fixing.
                let t = self.op_val;
                // Even with an incorrect op_addr we still read from it.
                self.op_val = self.ram.read(self.op_addr);

                // Check old opVal to see if it's non-zero. If so it means the Y addition
                // crosses a page boundary and we'll have to fixup.
                // For a load operation that means another tick to read the correct
                // address.
                // For RMW it doesn't matter (we always do the extra tick).
                // For Store we're done. Just fixup p.opAddr so the return value is correct.
                let mut done = Ok(OpState::Done);
                if t != 0 {
                    self.op_addr += 0x0100;
                    if let InstructionMode::Load = mode {
                        done = Ok(OpState::Processing);
                    }
                }
                // For RMW it doesn't matter, we tick again.
                if let InstructionMode::Rmw = mode {
                    done = Ok(OpState::Processing);
                }
                done
            }
            Tick::Tick6 => {
                // Optional (on load) in case adding Y went past a page boundary.
                self.op_val = self.ram.read(self.op_addr);
                match mode {
                    InstructionMode::Rmw => Ok(OpState::Processing),
                    _ => Ok(OpState::Done),
                }
            }
            Tick::Tick7 => {
                self.ram.write(self.op_addr, self.op_val);
                Ok(OpState::Done)
            }
        }
    }

    // addr_zp implements Zero page mode - d
    // returning the value in op_val and the address read in op_addr (so RW operations can do things without having to
    // reread memory incorrectly to compute a storage address).
    // If mode is RMW then another tick will occur that writes the read value back to the same address due to how
    // the 6502 operates.
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_zp(&mut self, mode: &InstructionMode) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 | Tick::Tick5 | Tick::Tick6 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("addr_zp invalid op_tick: {:?}", self.op_tick))
            }
            Tick::Tick2 => {
                // Already read the value but need to bump the PC
                self.op_addr = u16::from(self.op_val);
                self.pc += 1;
                match mode {
                    // For a store we're done since we have the address needed.
                    &InstructionMode::Store => Ok(OpState::Done),
                    _ => Ok(OpState::Processing),
                }
            }
            Tick::Tick3 => {
                self.op_val = self.ram.read(self.op_addr);
                match mode {
                    // For a load we're now done since the value is loaded.
                    &InstructionMode::Load => Ok(OpState::Done),
                    _ => Ok(OpState::Processing),
                }
            }
            Tick::Tick4 => {
                self.ram.write(self.op_addr, self.op_val);
                Ok(OpState::Done)
            }
        }
    }

    // addr_zp_x implements Zero page X mode - d,x
    // returning the value in op_val and the address read in op_addr (so RW operations can do things without having to
    // reread memory incorrectly to compute a storage address).
    // If mode is RMW then another tick will occur that writes the read value back to the same address due to how
    // the 6502 operates.
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_zp_x(&mut self, mode: &InstructionMode) -> Result<OpState> {
        self.addr_zp_xy(mode, self.x.0)
    }

    // addr_zp_y implements Zero page Y mode - d,y
    // returning the value in op_val and the address read in op_addr (so RW operations can do things without having to
    // reread memory incorrectly to compute a storage address).
    // If mode is RMW then another tick will occur that writes the read value back to the same address due to how
    // the 6502 operates.
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_zp_y(&mut self, mode: &InstructionMode) -> Result<OpState> {
        self.addr_zp_xy(mode, self.y.0)
    }

    // addr_zp_xy implements the details for addr_zp_x and addr_zp_y since they only differ based on the register used.
    // See those functions for arg/return specifics.
    fn addr_zp_xy(&mut self, mode: &InstructionMode, reg: u8) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 | Tick::Tick6 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("addr_zp_x invalid op_tick: {:?}", self.op_tick))
            }
            Tick::Tick2 => {
                // Already read the value but need to bump the PC.
                self.op_addr = u16::from(self.op_val);
                self.pc += 1;
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // Read from the ZP addr and then add the register for the real read later.
                _ = self.ram.read(self.op_addr);
                // Do this as a u8 so it wraps as needed.
                self.op_addr = u16::from(self.op_val + reg);
                // For a store we're done since we have the address needed.
                if let InstructionMode::Store = mode {
                    Ok(OpState::Done)
                } else {
                    Ok(OpState::Processing)
                }
            }
            Tick::Tick4 => {
                // Now read from the final address.
                self.op_val = self.ram.read(self.op_addr);
                if let InstructionMode::Load = mode {
                    Ok(OpState::Done)
                } else {
                    Ok(OpState::Processing)
                }
            }
            Tick::Tick5 => {
                self.ram.write(self.op_addr, self.op_val);
                Ok(OpState::Done)
            }
        }
    }

    fn perform_branch(&mut self) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 | Tick::Tick5 | Tick::Tick6 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("perform_branch invalid op_tick: {:?}", self.op_tick))
            }
            Tick::Tick2 => {
                self.pc += 1;
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // We only skip if the last instruction didn't. This way a branch always doesn't prevent interrupt processing
                // since on real silicon this is what happens (just a delay in the pipelining).
                if !self.prev_skip_interrupt {
                    self.prev_skip_interrupt = true;
                }

                // Per http://www.6502.org/tutorials/6502opcodes.html
                // the wrong page is defined as the a different page than
                // the next byte after the jump. i.e. current PC at the moment.

                // Now compute the new PC but possibly wrong page.
                // Stash the old one in p.opAddr so we can use in tick 4 if needed.
                self.op_addr = self.pc.0;
                self.pc = Wrapping(
                    (self.pc.0 & 0xFF00) + u16::from(((self.pc.0 & 0x00FF) as u8) + self.op_val),
                );
                // It always triggers a bus read of the newly computed PC.
                _ = self.ram.read(self.pc.0);
                // Now check the pc against one which didn't truncate for page by sign extending
                // op_val and adding it to op_addr.
                // NOTE: We don't lose the sign here since Wrapping will do the right thing.
                #[allow(
                    clippy::cast_sign_loss,
                    clippy::cast_lossless,
                    clippy::cast_possible_wrap
                )]
                if self.pc == Wrapping(self.op_addr) + Wrapping(self.op_val as i8 as i16 as u16) {
                    Ok(OpState::Done)
                } else {
                    Ok(OpState::Processing)
                }
            }
            Tick::Tick4 => {
                // Set the correct PC value if we got here.
                // NOTE: We don't lose the sign here since Wrapping will do the right thing.
                #[allow(
                    clippy::cast_sign_loss,
                    clippy::cast_lossless,
                    clippy::cast_possible_wrap
                )]
                let val = Wrapping(self.op_addr) + Wrapping(self.op_val as i8 as i16 as u16);
                self.pc = val;
                // Always read the next opcode now.
                _ = self.ram.read(self.pc.0);
                Ok(OpState::Done)
            }
        }
    }

    // branch_nopreads the next byte as the branch offset and increments the PC.
    // Used for the 2rd tick when branches aren't taken.
    fn branch_nop(&mut self) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset
            | Tick::Tick1
            | Tick::Tick3
            | Tick::Tick4
            | Tick::Tick5
            | Tick::Tick6
            | Tick::Tick7
            | Tick::Tick8 => Err(eyre!("branch_nop invalid op_tick: {:?}", self.op_tick)),
            Tick::Tick2 => {
                self.pc += 1;
                Ok(OpState::Done)
            }
        }
    }

    // pop_stack pops the top value from the stack and adjusts
    // the stack pointer.
    fn pop_stack(&mut self) -> u8 {
        self.s += 1;
        self.ram.read(u16::from(self.s.0) + 0x0100)
    }

    // push_stack takes the given 8 bit value and pushes it into the stack and adjusts
    // the stack pointer.
    fn push_stack(&mut self, val: u8) {
        self.ram.write(u16::from(self.s.0) + 0x0100, val);
        self.s -= 1;
    }

    // run_interrupt does all the heavy lifting for any interrupt processing.
    // i.e. pushing values onto the stack and loading PC with the right address.
    // Pass in the vector to be used for loading the PC (which means for BRK
    // it can change if an NMI happens before we get to the load ticks).
    // Returns OpState::Done when complete (and PC is correct). Can return an error on an
    // invalid tick count.
    fn run_interrupt(&mut self, vec: u16, irq: bool) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 | Tick::Tick8 => {
                Err(eyre!("run_interrupt: invalid op_tick: {:?}", self.op_tick))
            }
            Tick::Tick2 => {
                // Increment the PC on a non IRQ (i.e. BRK) since that changes where returns happen.
                if !irq {
                    self.pc += 1;
                }
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // There is no truncation as we mask and shift into 8 bits.
                #[allow(clippy::cast_possible_truncation)]
                self.push_stack(((self.pc.0 & 0xFF00) >> 8) as u8);
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // There is no truncation as we mask into 8 bits.
                #[allow(clippy::cast_possible_truncation)]
                self.push_stack((self.pc.0 & 0x00FF) as u8);
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                let mut push = self.p;
                self.p |= P_INTERRUPT;
                // S1 is always set
                push |= P_S1;
                // B always set unless this triggered due to IRQ
                push |= P_B;
                if irq {
                    push &= !P_B;
                }
                // CMOS turns off D always
                if Type::CMOS == self.cpu_type {
                    push &= !P_DECIMAL;
                }
                self.push_stack(push);
                Ok(OpState::Processing)
            }
            Tick::Tick6 => {
                self.op_val = self.ram.read(vec);
                Ok(OpState::Processing)
            }
            Tick::Tick7 => {
                // Compute the new PC from the 2nd vector component and the previous val read.
                self.pc =
                    Wrapping((u16::from(self.ram.read(vec + 1)) << 8) + u16::from(self.op_val));

                // If we didn't previously skip an interrupt from processing make sure we execute the first instruction of
                // a handler before firing again.
                if irq && !self.prev_skip_interrupt {
                    self.prev_skip_interrupt = true;
                }
                Ok(OpState::Done)
            }
        }
    }

    // anc implements the undocumented opcode for ANC. This does AND #i (op_val) and then
    // sets carry based on bit 7 (sign extend).
    // Always returns Done since this takes one tick and never returns an error.
    fn anc(&mut self) -> Result<OpState> {
        let val = self.a.0 & self.op_val;
        Self::load_register(&mut self.p, &mut self.a, val)?;
        Self::carry_check(&mut self.p, u16::from(self.a.0) << 1);
        Ok(OpState::Done)
    }

    // asl implements the ASL instruction on the given memory location in op_addr.
    // It then sets all associated flags and adjust cycles as needed.
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn asl(&mut self) -> Result<OpState> {
        let new = self.op_val << 1;
        self.ram.write(self.op_addr, new);
        Self::carry_check(&mut self.p, u16::from(self.op_val) << 1);
        Self::zero_check(&mut self.p, new);
        Self::negative_check(&mut self.p, new);
        Ok(OpState::Done)
    }

    // asl_acc implements the ASL instruction directly on the accumulator.
    // It then sets all associated flags and adjust cycles as needed.
    // Always returns true since accumulator mode is done on tick 2 and never returns an error.
    fn asl_acc(&mut self) -> Result<OpState> {
        Self::carry_check(&mut self.p, u16::from(self.a.0) << 1);
        let val = self.a.0 << 1;
        Self::load_register(&mut self.p, &mut self.a, val)
    }

    // bpl implements the BPL instructions and branches if N is clear.
    // Returns Done when the branch has set the correct PC and/or an error.
    fn bpl(&mut self) -> Result<OpState> {
        if self.p & P_NEGATIVE == 0x00 {
            self.perform_branch()
        } else {
            self.branch_nop()
        }
    }

    // brk implements the BRK instruction. This does setup and then calls the
    // interrupt processing handler refenced at IRQ_VECTOR (normally).
    fn brk(&mut self) -> Result<OpState> {
        // This is the same as an interrupt handler so the vector we call
        // can change on a per tick basis. i.e. we might push P with P_B set
        // but go to the NMI vector depending on timing.

        // New PC comes from IRQ_VECTOR unless we've raised an NMI.
        let ret = match self.irq {
            IRQ::Irq => self.run_interrupt(IRQ_VECTOR, true),
            IRQ::Nmi => self.run_interrupt(NMI_VECTOR, true),
            IRQ::None => self.run_interrupt(IRQ_VECTOR, false),
        };
        // If we're done on this tick eat any pending interrupt since BRK is special.
        if let Ok(r) = ret {
            if r == OpState::Done {
                self.irq = IRQ::None;
            }
        }
        ret
    }

    // clc implements the CLC instruction clearing the C status bit.
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn clc(&mut self) -> Result<OpState> {
        self.p &= !P_CARRY;
        Ok(OpState::Done)
    }

    // ora implements the ORA instruction which ORs op_val with A.
    // Always returns true since this takes one tick and never returns an error.
    fn ora(&mut self) -> Result<OpState> {
        let val = self.a.0 | self.op_val;
        Self::load_register(&mut self.p, &mut self.a, val)
    }

    // jsr implements the JSR instruction for jumping to a subroutine.
    // Returns Done when done and/or errors.
    fn jsr(&mut self) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("jsr: invalid op_tick: {:?}", self.op_tick))
            }
            Tick::Tick2 => {
                // Nothing happens here except to make the PC correct.
                // NOTE: This means the PC pushed below is actually off by one.
                //       RTS handles this by adding one to the popped PC.
                self.pc += 1;
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // Not 100% sure what happens on this cycle.
                // Per http://nesdev.com/6502_cpu.txt we read the current stack
                // value because there needs to be a tick to make S correct.
                self.s -= 1;
                _ = self.pop_stack();
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                self.push_stack(((self.pc.0 & 0xFF00) >> 8) as u8);
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                self.push_stack((self.pc.0 & 0x00FF) as u8);
                Ok(OpState::Processing)
            }
            Tick::Tick6 => {
                self.pc =
                    Wrapping((u16::from(self.ram.read(self.pc.0)) << 8) + u16::from(self.op_val));
                Ok(OpState::Done)
            }
        }
    }

    // php implements the PHP instructions for pushing P onto the stacks.
    // Returns Done when done and/or errors.
    fn php(&mut self) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset
            | Tick::Tick1
            | Tick::Tick4
            | Tick::Tick5
            | Tick::Tick6
            | Tick::Tick7
            | Tick::Tick8 => Err(eyre!("php: invalid op_tick: {:?}", self.op_tick)),
            Tick::Tick2 => Ok(OpState::Processing),
            Tick::Tick3 => {
                let mut push = self.p;

                // This is always set
                push |= P_S1;

                // PHP always sets B where-as IRQ/NMI don't.
                push |= P_B;
                self.push_stack(push);
                Ok(OpState::Done)
            }
        }
    }

    // slo implements the undocumented opcode for SLO. This does an ASL on the
    // contents of op_addr and then OR's it against A. Sets flags and carry.
    // Always returns true since this takes one tick and never returns an error.
    fn slo(&mut self) -> Result<OpState> {
        self.ram.write(self.op_addr, self.op_val << 1);
        Self::carry_check(&mut self.p, u16::from(self.op_val) << 1);
        let val = (self.op_val << 1) | self.a.0;
        Self::load_register(&mut self.p, &mut self.a, val)
    }
}

/// `FlatRAM` gives a flat 64k RAM block to use.
/// It will be initialized to all zeros and `power_on`
/// can use a different value if `fill_value` is set.
/// Additionally the irq/reset and nmi vectors can be set as well.
/// Generally used only for testing.
#[derive(Debug, Clone, Copy)]
#[must_use]
pub struct FlatRAM {
    fill_value: u8,
    vectors: Vectors,
    memory: [u8; MAX_SIZE],
    debug: bool,
}

#[derive(Debug, Default, Clone, Copy)]
/// `Vectors` defines the 3 6502 vectors for interrupts and reset behavior.
pub struct Vectors {
    /// `nmi` is the NMI vector.
    pub nmi: u16,

    /// `reset` is the reset vector.
    pub reset: u16,

    /// `irq` is the IRQ vector.
    pub irq: u16,
}

impl Memory for FlatRAM {
    fn read(&self, addr: u16) -> u8 {
        if self.debug {
            println!("read: {addr:04X}: {:02X}", self.memory[usize::from(addr)]);
        }
        self.memory[usize::from(addr)]
    }

    fn write(&mut self, addr: u16, val: u8) {
        if self.debug {
            println!("write: {addr:04X}: {val:02X}");
        }
        self.memory[usize::from(addr)] = val;
    }

    /// `power_on` will perform power on behavior. For `FlatRAM` this entails
    /// setting all memory locations to the fill value and then setting the 3 vectors
    /// to their assigned values.
    fn power_on(&mut self) {
        for i in 0..self.memory.len() {
            self.memory[i] = self.fill_value;
        }
        self.memory[NMI_VECTOR as usize] = (self.vectors.nmi & 0xFF) as u8;
        self.memory[(NMI_VECTOR + 1) as usize] = ((self.vectors.nmi & 0xff00) >> 8) as u8;
        self.memory[RESET_VECTOR as usize] = (self.vectors.reset & 0xFF) as u8;
        self.memory[(RESET_VECTOR + 1) as usize] = ((self.vectors.reset & 0xff00) >> 8) as u8;
        self.memory[IRQ_VECTOR as usize] = (self.vectors.irq & 0xFF) as u8;
        self.memory[(IRQ_VECTOR + 1) as usize] = ((self.vectors.irq & 0xff00) >> 8) as u8;
    }
}

impl Default for FlatRAM {
    fn default() -> Self {
        Self::new()
    }
}

impl FlatRAM {
    /// new will return a `FlatRAM` with 0x00 set for everything (vectors, fill value, etc).
    /// Use other builders to set additional items.
    pub fn new() -> Self {
        Self {
            fill_value: 0,
            vectors: Vectors {
                ..Default::default()
            },
            memory: [0; MAX_SIZE],
            debug: false,
        }
    }

    /// `debug` is a builder which enables debug mode for `FlatRAM`
    pub const fn debug(mut self) -> Self {
        self.debug = true;
        self
    }

    /// `fill_value` is a builder which sets the fill value to use when performing `power_on`.
    pub const fn fill_value(mut self, value: u8) -> Self {
        self.fill_value = value;
        self
    }

    /// `fill_value` is a builder which sets the vectors to use when performing `power_on`.
    pub const fn vectors(mut self, vectors: Vectors) -> Self {
        self.vectors = vectors;
        self
    }
}
