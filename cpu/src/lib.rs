//! cpu defines a 6502 CPU which is clock accurate to the supporting environment.
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Write;
use std::num::Wrapping;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, Not};
use std::rc::Rc;

use chip::Chip;

mod cmos_opcodes;
mod nmos_opcodes;

use cmos_opcodes::{cmos_opcodes, cmos_opcodes_values};
use memory::{Memory, MAX_SIZE};
use nmos_opcodes::{nmos_opcodes, nmos_opcodes_values};
use thiserror::Error;

use color_eyre::eyre::{eyre, ErrReport, Result};
use rand::Rng;
use strum_macros::{Display, EnumIter, EnumString};

use cpu_proc_macros::cpu_base_struct;

#[cfg(test)]
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

    /// `Indirect` uses the given address from the first 256 bytes (zero page).
    /// It then uses this and the following location as a pointer to use as the final address.
    /// Example LDA (04) and location 04,05 have 02 and 01. This would load the value from 0x0102 into A.
    /// NOTE: CMOS only
    Indirect,

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

    /// `AbsoluteIndirect` loads a pointer from the given address and de-references it to get the final address.
    /// Example: JMP (D000) with 0xD000,0xD001 equal to 0x01,0xC0 will jump to 0xC001
    AbsoluteIndirect,

    /// `AbsoluteIndirectX` loads a pointer from the given address after adding X and de-references it to get the final address.
    /// This is useful for assembling jump tables.
    /// Example: JMP (D000,X) with 0xD002,0xD003 equal to 0x01,0xC0 and X=2 will jump to 0xC001.
    /// NOTE: CMOS only
    AbsoluteIndirectX,

    /// `AbsoluteNOP` is a special mode for CMOS NOP 0x5C which takes 8 cycles
    /// and then reads from somewhere in memory.
    /// Node: CMOS only.
    AbsoluteNOP,

    /// `Implied` takes no arguments and instead operates directly based on the opcode only.
    /// Example: INX increments the X register.
    Implied,

    /// `Relative` takes the argument and adds it as a signed value to the PC to determine the next PC location.
    /// This is used for branching.
    /// Example: D004 D0 FE is a BNE which computes to D004 if true and would infinite loop.
    Relative,

    /// `ZeroPageRelative` is a combination addressing mode used in 2 CMOS instructions (BBR,BBS)
    /// It takes both a ZP address and a relative offset destination to test and then branch.
    /// Example BBR 0,$12,FE would test bit 0 on location 12 and if clear branches to FE which is an infinite loop.
    ZeroPageRelative,

    /// `NOPCmos` is a CMOS only mode indicating a 1 byte NOP that also executes
    /// in one cycle.
    /// Note: CMOS only.
    NOPCmos,
}

// Opcode matric taken from:
// http://wiki.nesdev.com/w/index.php/CPU_unofficial_opcodes#Games_using_unofficial_opcodes
//
// NOTE: The above lists 0xAB as LAX #i but we call it OAL since it has odd behavior and needs
//       its own code compared to other LAX. See 6502-NMOS.extra.opcodes below.
//
// Description of undocumented opcodes:
//
// http://www.ffd2.com/fridge/docs/6502-NMOS.extra.opcodes
// http://nesdev.com/6502_cpu.txt
// http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_(XAA,_ANE)
//
// Opcode descriptions/timing/etc:
// http://obelisk.me.uk/6502/reference.html

/// `Opcode` defines all the unique 65XX and 65C02 opcodes including undocumented ones.
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

    /// Branch on bit reset
    /// Note: CMOS only with Rockwell/WDC extensions.
    BBR,

    /// Branch on bit set
    /// Note: CMOS only with Rockwell/WDC extensions.
    BBS,

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

    /// Branch always
    /// Note: CMOS only
    BRA,

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

    /// Pushes X onto the stack.
    /// Note: CMOS only
    PHX,

    /// Pushes Y onto the stack.
    /// Note: CMOS only
    PHY,

    /// Pulls A from the stack.
    PLA,

    /// Pulls X from the stack.
    /// Note: CMOS only
    PLX,

    /// Pulls Y from the stack.
    /// Note: CMOS only
    PLY,

    /// Pulls P from the stack.
    PLP,

    /// Undocumented opcode RLA. This does a ROL on the value at the operand address and then AND's it against A. Sets flags and carry.
    RLA,

    /// Reset memory bit.
    /// Note: CMOS only with Rockwell/WDC extensions.
    RMB,

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

    /// Set memory bit.
    /// Note: CMOS only with Rockwell/WDC extensions.
    SMB,

    /// Undocumented instruction SRE. This does a LSR on the value at the operand address and then EOR's it against A. Sets flags and carry.
    SRE,

    /// Stores the A register at the operand address.
    STA,

    /// Stops the CPU until a reset occurs
    /// Note: CMOS only on WDC implementations.
    STP,

    /// Stores the X register at the operand address.
    STX,

    /// Stores the Y register at the operand address.
    STY,

    /// Stores a 0 at the given location.
    /// Note: CMOS only.
    STZ,

    /// Undocumented instruction TAS. This does the same operations as AHX but then also sets S = A&X.
    TAS,

    /// Loads the X register with the value of the A register.
    TAX,

    /// Loads the Y register with the value of the A register.
    TAY,

    /// Test and reset bits.
    /// Note: CMOS only
    TRB,

    /// Test and set bits.
    /// Note: CMOS only
    TSB,

    /// Loads the X register with the value of the S register.
    TSX,

    /// Loads the A register with the value of the X register.
    TXA,

    /// Loads the S register with the value of the X register. No flags are set from S loads.
    TXS,

    /// Loads the A register with the value of the Y register.
    TYA,

    /// Wait for interrupt. Pauses the CPU until either an interrupt or reset occurs.
    /// Note: CMOS only on WDC implementations.
    WAI,

    /// Undocumented instruction XAA. We'll go with http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_(XAA,_ANE)
    /// for implementation and pick 0xEE as the constant. According to VICE this may break so might need to change it to 0xFF
    /// https://sourceforge.net/tracker/?func=detail&aid=2110948&group_id=223021&atid=1057617
    XAA,
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

/// `STACK_START` is the location in memory where the stack page starts.
pub const STACK_START: u16 = 0x0100;

/// `NMI_VECTOR` is the location in memory the 6502 uses for NMI interrupts.
/// It is a pointer to the location to start execution.
pub const NMI_VECTOR: u16 = 0xFFFA;

/// `RESET_VECTOR` is the location in memory the 6502 uses on startup to begin execution.
/// It is a pointer to the location to start execution.
pub const RESET_VECTOR: u16 = 0xFFFC;

/// `IRQ_VECTOR` is the location in memory the 6502 uses when executing an IRQ (BRK).
/// It is a pointer to the location to start execution.
pub const IRQ_VECTOR: u16 = 0xFFFE;

#[derive(Debug, Default, Display, Copy, Clone, PartialEq, EnumString)]
/// `State` defines the current CPU state. i.e. on/off/resetting, etc
pub enum State {
    /// The default when initialized.
    /// Will throw errors in any functions if this state because `power_on`
    /// hasn't been called.
    #[default]
    Off,

    /// The state `power_on` leaves the chip which generally moves
    /// immediate into Reset.
    On,

    /// If we're in a reset sequence and not done yet.
    Reset,

    /// Set in `reset` or `tick_done`
    Running,

    /// The mode we are in after a `tick` has run
    Tick,

    /// If we ran an HLT/STP instruction we end up here.
    Halted,

    /// WAI will cause this state where processing is paused
    /// until an interrupt happens.
    WaitingForInterrupt,
}

#[derive(Copy, Clone, Default, Debug, Display, PartialEq)]
#[allow(clippy::upper_case_acronyms)]
enum InterruptStyle {
    #[default]
    None,
    IRQ,
    NMI,
}

#[derive(Default, Debug, Display, PartialEq)]
enum InterruptState {
    #[default]
    None,
    Running,
}

#[derive(Copy, Clone, Default, Debug, Display, PartialEq)]
enum SkipInterrupt {
    #[default]
    None,
    Skip,
    PrevSkip,
}

/// `OpState` is used to indicate whether an opcode/addressing mode is done or not.
#[derive(Debug, Copy, Clone, Display, PartialEq)]
pub enum OpState {
    /// Everything is done.
    Done,
    /// There is more work to be done.
    Processing,
}

// The various types of instruction modes internally used.
#[derive(PartialEq)]
enum InstructionMode {
    Load,
    Rmw,
    Store,
}

/// Clock tick state during a given instruction/reset sequence.
#[derive(Debug, Default, Display, Copy, Clone, PartialEq, EnumString)]
pub enum Tick {
    /// The reset state. Used to start a new instruction assuming all
    /// processing immediately calls `next` before evaluatin.
    #[default]
    Reset,

    /// This is the normal start case for a new instruction.
    Tick1,
    /// All instructions take this long so this always reads the 2nd byte
    /// which is a throw away for single byte instructions.
    Tick2,
    /// Optional 3rd tick.
    Tick3,
    /// Optional 4th tick.
    Tick4,
    /// Optional 5th tick.
    Tick5,
    /// Optional 6th tick.
    Tick6,
    /// Optional 7th tick.
    Tick7,

    /// Technically documented 6502 instructions take no more than 7 cycles.
    /// But a RMW indirect X/Y will take 8.
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

/// Flags defines a type to represent the processor flags.
/// It will print out with all of the flag values but otherwise
/// can be treated as a u8 when needed.
/// NOTE: S1 is always on so a default will create as such.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Flags(u8);

impl Default for Flags {
    fn default() -> Self {
        // S1 is always set.
        P_S1
    }
}

impl BitOr for Flags {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl BitOr<&u8> for Flags {
    type Output = Self;

    fn bitor(self, rhs: &u8) -> Self::Output {
        Self(self.0 | rhs)
    }
}

impl BitOr<u8> for Flags {
    type Output = Self;

    fn bitor(self, rhs: u8) -> Self::Output {
        Self(self.0 | rhs)
    }
}

impl BitOr<Flags> for u8 {
    type Output = Flags;

    fn bitor(self, rhs: Flags) -> Self::Output {
        Flags(self | rhs.0)
    }
}

impl BitOrAssign for Flags {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl BitOrAssign<&u8> for Flags {
    fn bitor_assign(&mut self, rhs: &u8) {
        self.0 |= rhs;
    }
}

impl BitOrAssign<u8> for Flags {
    fn bitor_assign(&mut self, rhs: u8) {
        self.0 |= rhs;
    }
}

impl BitAnd for Flags {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl BitAnd<&u8> for Flags {
    type Output = Self;

    fn bitand(self, rhs: &u8) -> Self::Output {
        Self(self.0 & rhs)
    }
}

impl BitAnd<u8> for Flags {
    type Output = Self;

    fn bitand(self, rhs: u8) -> Self::Output {
        Self(self.0 & rhs)
    }
}

impl BitAnd<Flags> for u8 {
    type Output = Flags;

    fn bitand(self, rhs: Flags) -> Self::Output {
        // P_S1 always stays on
        Flags(self & rhs.0)
    }
}

impl BitAndAssign for Flags {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl BitAndAssign<&u8> for Flags {
    fn bitand_assign(&mut self, rhs: &u8) {
        self.0 &= rhs;
    }
}

impl BitAndAssign<u8> for Flags {
    fn bitand_assign(&mut self, rhs: u8) {
        self.0 &= rhs;
    }
}

impl Not for Flags {
    type Output = Self;
    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

// The bitmasks for all of the Flags bits.
const P_NEGATIVE: Flags = Flags(0x80);
const P_OVERFLOW: Flags = Flags(0x40);
const P_S1: Flags = Flags(0x20); // Always on
const P_B: Flags = Flags(0x10); // Only set when pushing onto the stack during BRK.
const P_DECIMAL: Flags = Flags(0x08);
const P_INTERRUPT: Flags = Flags(0x04);
const P_ZERO: Flags = Flags(0x02);
const P_CARRY: Flags = Flags(0x01);

const P_NONE: Flags = Flags(0x00);

impl fmt::Display for Flags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        if self.0 & P_NEGATIVE == P_NEGATIVE {
            out += "N";
        } else {
            out += "n";
        }
        if self.0 & P_OVERFLOW == P_OVERFLOW {
            out += "V";
        } else {
            out += "v";
        }
        if self.0 & P_S1 == P_S1 {
            out += "S";
        } else {
            out += "s";
        }
        if self.0 & P_B == P_B {
            out += "B";
        } else {
            out += "b";
        }
        if self.0 & P_DECIMAL == P_DECIMAL {
            out += "D";
        } else {
            out += "d";
        }
        if self.0 & P_INTERRUPT == P_INTERRUPT {
            out += "I";
        } else {
            out += "i";
        }
        if self.0 & P_ZERO == P_ZERO {
            out += "Z";
        } else {
            out += "z";
        }
        if self.0 & P_CARRY == P_CARRY {
            out += "C";
        } else {
            out += "c";
        }
        write!(f, "{out}")
    }
}

#[derive(Clone, Copy, Display, EnumString)]
enum Register {
    A,
    P,
    X,
    Y,
}

/// `CPUState` is the public information about the CPU and RAM
/// at a point in time. This is generally used through the debug Option
/// in the Cpu below.
#[derive(Clone, Debug)]
pub struct CPUState {
    /// CPU state
    pub state: State,

    /// Accumulator register
    pub a: u8,

    /// X register
    pub x: u8,

    /// Y register
    pub y: u8,

    /// Stack pointer
    pub s: u8,

    /// Status register
    pub p: Flags,

    /// Program counter
    pub pc: u16,

    /// RAM contents
    pub ram: [u8; MAX_SIZE],

    /// How many clocks have run since power on
    pub clocks: usize,

    /// The current op_val
    pub op_val: u8,

    /// The current op_addr
    pub op_addr: u16,

    /// The dissasembly of the current instruction at PC
    pub dis: String,

    /// The current op_tick
    pub op_tick: Tick,
}

impl Default for CPUState {
    fn default() -> Self {
        Self {
            state: State::Off,
            a: 0x00,
            x: 0x00,
            y: 0x00,
            s: 0x00,
            p: P_NONE,
            pc: 0x0000,
            clocks: 0,
            op_val: 0x00,
            op_addr: 0x0000,
            ram: [0; MAX_SIZE],
            dis: String::new(),
            op_tick: Tick::default(),
        }
    }
}

impl fmt::Display for CPUState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{:>6} {:<24}: A: {:02X} X: {:02X} Y: {:02X} S: {:02X} P: {} op_val: {:02X} op_addr: {:04X} op_tick: {}",
            self.clocks, self.dis, self.a, self.x, self.y, self.s, self.p, self.op_val, self.op_addr, self.op_tick,
        )?;
        writeln!(f, "Memory:")?;
        writeln!(f)?;
        writeln!(f, "{}", &self.ram as &dyn Memory)
    }
}

/// The interface any 6502 implementation must conform to.
pub trait CPU<'a>: Chip {
    /// Given an `Opcode` and `AddressMode` return the valid u8 values that
    /// can represent it.
    ///
    /// # Errors
    /// If the `AddressMode` is not valid for this opcode an error will result.
    fn resolve_opcode(&self, op: &Opcode, mode: &AddressMode) -> Result<&'static Vec<u8>> {
        // Default impl is NMOS
        let hm: &HashMap<AddressMode, Vec<u8>>;
        // SAFETY: When we built NMOS_OPCODES we validated all Opcodes were present
        unsafe {
            hm = nmos_opcodes().get(op).unwrap_unchecked();
        }
        let Some(v) = hm.get(mode) else {
            return Err(eyre!("address mode {mode} isn't valid for opcode {op}"));
        };
        Ok(v)
    }

    /// Given an opcode u8 value this will return the Operation struct
    /// defining it. i.e. `Opcode` and `AddressMode`.
    #[must_use]
    fn opcode_op(&self, op: u8) -> Operation {
        // Default impl is NMOS

        // SAFETY: We know a u8 is in range due to how we built this
        //         so a direct index is fine vs having the range check
        //         an index lookup.
        unsafe { *nmos_opcodes_values().get_unchecked(usize::from(op)) }
    }

    /// Use this to enable or disable state based debugging dynamically.
    fn set_debug(&mut self, d: Option<&'a dyn Fn() -> (Rc<RefCell<CPUState>>, bool)>);

    /// If the debug hook is set with `set_debug` then when this is called it
    /// will call that hook to get and then fill in a `CPUState`
    fn debug(&self);

    /// `power_on` will reset the CPU to power on state. This state is dependent
    /// on implementation as the various flavors (NMOS, CMOS, etc) have
    /// differences. When done it will then run the reset sequence for the chip
    /// simulating a normal RST line held for Xms at startup to trigger this
    /// sequence.
    ///
    /// # Errors
    /// If reset has any issues an error will be returned.
    fn power_on(&mut self) -> Result<()>;

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
    fn reset(&mut self) -> Result<OpState>;

    /// ram returns a reference to the Memory implementation.
    fn ram(&self) -> Rc<RefCell<Box<dyn Memory>>>;

    /// pc returns the current PC value.
    fn pc(&self) -> u16;
    /// `pc_mut` sets PC to the given address.
    fn pc_mut(&mut self, new: u16);

    /// disassemble will take the given pc and Memory implementation and disassemble the segment
    /// at that location. It will return a string of the dissembly as well as the next pc
    /// to continue disassembling.
    /// As a real 6502 will wrap around if it's asked to step off the end
    /// this will do the same. i.e. disassembling 0xFFFF with a multi-byte opcode will result
    /// in reading 0x0000 and 0x0001 and returning a pc from that area as well.
    fn disassemble(&self, pc: u16, r: &dyn Memory) -> (String, u16) {
        let pc1 = r.read((Wrapping(pc) + Wrapping(1)).0);
        let pc2 = r.read((Wrapping(pc) + Wrapping(2)).0);

        // Sign extend a 16 bit value so it can be added to PC for branch offsets
        #[allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
        let pc116 = Wrapping(i16::from(pc1 as i8) as u16);
        #[allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
        let pc216 = Wrapping(i16::from(pc2 as i8) as u16);

        let op = r.read(pc);

        let (opcode, mode) = {
            let operation = self.opcode_op(op);
            (operation.op.to_string(), operation.mode)
        };

        let mut out = format!("{pc:04X} {op:02X} ");
        let mut count = Wrapping(pc) + Wrapping(2);

        match mode {
            AddressMode::Immediate => {
                write!(out, "{pc1:02X}      {opcode} #${pc1:02X}").unwrap();
            }
            AddressMode::ZeroPage => {
                write!(out, "{pc1:02X}      {opcode} ${pc1:02X}").unwrap();
            }
            AddressMode::ZeroPageX => {
                write!(out, "{pc1:02X}      {opcode} ${pc1:02X},X").unwrap();
            }
            AddressMode::ZeroPageY => {
                write!(out, "{pc1:02X}      {opcode} ${pc1:02X},Y").unwrap();
            }
            AddressMode::Indirect => {
                write!(out, "{pc1:02X}      {opcode} (${pc1:02X})").unwrap();
            }
            AddressMode::IndirectX => {
                write!(out, "{pc1:02X}      {opcode} (${pc1:02X},X)",).unwrap();
            }
            AddressMode::IndirectY => {
                write!(out, "{pc1:02X}      {opcode} (${pc1:02X}),Y").unwrap();
            }
            AddressMode::Absolute | AddressMode::AbsoluteNOP => {
                write!(out, "{pc1:02X} {pc2:02X}   {opcode} ${pc2:02X}{pc1:02X}",).unwrap();
                count += 1;
            }
            AddressMode::AbsoluteX => {
                write!(out, "{pc1:02X} {pc2:02X}   {opcode} ${pc2:02X}{pc1:02X},X",).unwrap();
                count += 1;
            }
            AddressMode::AbsoluteY => {
                write!(out, "{pc1:02X} {pc2:02X}   {opcode} ${pc2:02X}{pc1:02X},Y",).unwrap();
                count += 1;
            }
            AddressMode::AbsoluteIndirect => {
                write!(out, "{pc1:02X} {pc2:02X}   {opcode} (${pc2:02X}{pc1:02X})",).unwrap();
                count += 1;
            }
            AddressMode::AbsoluteIndirectX => {
                write!(
                    out,
                    "{pc1:02X} {pc2:02X}   {opcode} (${pc2:02X}{pc1:02X},X)",
                )
                .unwrap();
                count += 1;
            }
            AddressMode::Implied | AddressMode::NOPCmos => {
                write!(out, "        {opcode}").unwrap();
                count -= 1;
            }
            AddressMode::Relative => {
                write!(
                    out,
                    "{pc1:02X}      {opcode} ${pc1:02X} (${:04X})",
                    Wrapping(pc) + pc116 + Wrapping(2u16)
                )
                .unwrap();
            }
            AddressMode::ZeroPageRelative => {
                write!(
                    out,
                    "{pc1:02X} {pc2:02X}   {opcode} {},${pc1:02X},${pc2:02X} (${:04X})",
                    (op & 0xF0) >> 4,
                    Wrapping(pc) + pc216 + Wrapping(2u16)
                )
                .unwrap();
            }
        }

        (out, count.0)
    }
}

// NOTE: All of the 65xx implementations below generate their base struct from
// the attribute attached to them. Use cargo-expand or read the proc macro for
// the description of all the members.

/// The NMOS 6502 implementation for the 6502 architecture
#[cpu_base_struct]
pub struct CPU6502<'a> {}

/// The NMOS 6510 implementation for the 6502 architecture.
/// Includes the 6 I/O pins and support for their memory mapped behavior
/// at locations 0x0000 and 0x0001
#[cpu_base_struct]
pub struct CPU6510<'a> {
    io: Rc<RefCell<[io::Style; 6]>>,
}

/// The Richo implementation for the 6502 architecture.
/// The same as an NMOS 6502 except it doesn't have BCD support for ADC/SBC."
#[cpu_base_struct]
pub struct CPURicoh<'a> {}

/// The CMOS implementation for the 65C02 architecture.
/// Fixes many bugs from the 6502 (no more undocumented opcodes) and adds
/// additional instructions.
/// This implementation is the Rockwell + WDC additions including WAI and STP."
#[cpu_base_struct]
pub struct CPU65C02<'a> {}

macro_rules! common_cpu_funcs {
  ($cpu:ident, $t:expr) => {
      impl fmt::Debug for $cpu<'_> {
          fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
              f.debug_struct(format!("CPU {}", $t).as_str())
                  .field("a", &self.a.0)
                  .field("x", &self.x.0)
                  .field("y", &self.y.0)
                  .field("s", &self.s.0)
                  .field("p", &self.p)
                  .field("pc", &self.pc.0)
                  .field("state", &self.state)
                  .field("irq", &self.irq_raised)
                  .finish()
          }
      }

      impl fmt::Display for $cpu<'_> {
          fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
              let (dis, _) = self.disassemble(self.pc.0, self.ram.borrow().as_ref());

              write!(
                  f,
                  "{:>6} {dis:>24}: A: {:02X} X: {:02X} Y: {:02X} S: {:02X} P: {} op_val: {:02X} op_addr: {:04X}",
                  self.clocks, self.a, self.x, self.y, self.s, self.p, self.op_val, self.op_addr
              )
          }
      }

      impl PartialEq for $cpu<'_> {
          fn eq(&self, other: &Self) -> bool {
              self.a == other.a
                  && self.x == other.x
                  && self.y == other.y
                  && self.s == other.s
                  && self.p == other.p
                  && self.pc == other.pc
          }
      }
  };
}

common_cpu_funcs!(CPU6502, "NMOS");
common_cpu_funcs!(CPU6510, "6510");
common_cpu_funcs!(CPURicoh, "Ricoh");
common_cpu_funcs!(CPU65C02, "CMOS");

// Common implementations which are NMOS only specific (undocumented opcodes
// and opcode processing). CMOS can implement `process_opcode` directly and
// doesn't need the rest.
trait CPUNmosInternal<'a>: CPUInternal<'a> + CPU<'a> {
    #[allow(clippy::too_many_lines)]
    fn process_opcode(&mut self) -> Result<OpState> {
        match (self.op().op, self.op().mode) {
            // 0x00 - BRK #i
            (Opcode::BRK, AddressMode::Immediate) => self.brk(),
            // 0x01 - ORA (d,x)
            (Opcode::ORA, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::ora)
            }
            // 0x02 0x12 0x22 0x32 0x42 0x52 0x62 0x72 0x92 0xB2 0xD2 0xF2 - HLT
            (Opcode::HLT, AddressMode::Implied) => {
                self.state_mut(State::Halted);
                Ok(OpState::Done)
            }
            // 0x03 - SLO (d,x)
            (Opcode::SLO, AddressMode::IndirectX) => {
                self.rmw_instruction(Self::addr_indirect_x, Self::slo)
            }
            // 0x04 0x44 0x64 - NOP d
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
            // 0x0B 0x2B - ANC #i
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
            // 0x12 - HLT see 0x02
            // 0x13 - SLO (d),y
            (Opcode::SLO, AddressMode::IndirectY) => {
                self.rmw_instruction(Self::addr_indirect_y, Self::slo)
            }
            // 0x14 0x34 0x54 0x74 0xD4 0xF4 - NOP d,x
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
            // 0x1A 0x3A 0x5A 0x7A 0xDA 0xEA 0xFA - NOP
            (Opcode::NOP, AddressMode::Implied) => Ok(OpState::Done),
            // 0x1B - SLO a,y
            (Opcode::SLO, AddressMode::AbsoluteY) => {
                self.rmw_instruction(Self::addr_absolute_y, Self::slo)
            }
            // 0x1C 0x3C 0x5C 0x7C 0xDC 0xFC - NOP a,x
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
            // 0x21 - AND (d,x)
            (Opcode::AND, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::and)
            }
            // 0x22 - HLT see 0x02
            // 0x23 - RLA (d,x)
            (Opcode::RLA, AddressMode::IndirectX) => {
                self.rmw_instruction(Self::addr_indirect_x, Self::rla)
            }
            // 0x24 - BIT d
            (Opcode::BIT, AddressMode::ZeroPage) => self.load_instruction(Self::addr_zp, Self::bit),
            // 0x25 - AND d
            (Opcode::AND, AddressMode::ZeroPage) => self.load_instruction(Self::addr_zp, Self::and),
            // 0x26 - ROL d
            (Opcode::ROL, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::rol),
            // 0x27 - RLA d
            (Opcode::RLA, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::rla),
            // 0x28 - PLP
            (Opcode::PLP, AddressMode::Implied) => self.plp(),
            // 0x29 - AND #i
            (Opcode::AND, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::and)
            }
            // 0x2A - ROL
            (Opcode::ROL, AddressMode::Implied) => self.rol_acc(),
            // 0x2B - ANC #i see 0x0B
            // 0x2C - BIT a
            (Opcode::BIT, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::bit)
            }
            // 0x2D - AND a
            (Opcode::AND, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::and)
            }
            // 0x2E - ROL a
            (Opcode::ROL, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::rol)
            }
            // 0x2F - RLA a
            (Opcode::RLA, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::rla)
            }
            // 0x30 - BMI *+r
            (Opcode::BMI, AddressMode::Relative) => self.bmi(),
            // 0x31 - AND (d),y
            (Opcode::AND, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::and)
            }
            // 0x32 - HLT see 0x02
            // 0x33 - RLA (d),y
            (Opcode::RLA, AddressMode::IndirectY) => {
                self.rmw_instruction(Self::addr_indirect_y, Self::rla)
            }
            // 0x34 - NOP d,x see 0x14
            // 0x35 - AND d,x
            (Opcode::AND, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::and)
            }
            // 0x36 - ROL d,x
            (Opcode::ROL, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::rol)
            }
            // 0x37 - RLA d,x
            (Opcode::RLA, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::rla)
            }
            // 0x38 - SEC
            (Opcode::SEC, AddressMode::Implied) => self.sec(),
            // 0x39 - AND a,y
            (Opcode::AND, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::and)
            }
            // 0x3A - NOP see 0x1A
            // 0x3B - RLA a,y
            (Opcode::RLA, AddressMode::AbsoluteY) => {
                self.rmw_instruction(Self::addr_absolute_y, Self::rla)
            }
            // 0x3C - NOP see 0x1C
            // 0x3D - AND a,x
            (Opcode::AND, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::and)
            }
            // 0x3E - ROL a,x
            (Opcode::ROL, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::rol)
            }
            // 0x3F - RLA a,x
            (Opcode::RLA, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::rla)
            }
            // 0x40 - RTI
            (Opcode::RTI, AddressMode::Implied) => self.rti(),
            // 0x41 - EOR (d,x)
            (Opcode::EOR, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::eor)
            }
            // 0x42 - HLT see 0x02
            // 0x43 - SRE (d,x)
            (Opcode::SRE, AddressMode::IndirectX) => {
                self.rmw_instruction(Self::addr_indirect_x, Self::sre)
            }
            // 0x44 - NOP see 0x04
            // 0x45 - EOR d
            (Opcode::EOR, AddressMode::ZeroPage) => self.load_instruction(Self::addr_zp, Self::eor),
            // 0x46 - LSR d
            (Opcode::LSR, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::lsr),
            // 0x47 - SRE d
            (Opcode::SRE, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::sre),
            // 0x48 - PHA
            (Opcode::PHA, AddressMode::Implied) => self.pha(),
            // 0x49 - EOR #i
            (Opcode::EOR, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::eor)
            }
            // 0x4A - LSR
            (Opcode::LSR, AddressMode::Implied) => self.lsr_acc(),
            // 0x4B - ALR #i
            (Opcode::ALR, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::alr)
            }
            // 0x4C - JMP a
            (Opcode::JMP, AddressMode::Absolute) => self.jmp(),
            // 0x4D - EOR a
            (Opcode::EOR, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::eor)
            }
            // 0x4E - LSR a
            (Opcode::LSR, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::lsr)
            }
            // 0x4F - SRE a
            (Opcode::SRE, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::sre)
            }
            // 0x50 - BVC *+r
            (Opcode::BVC, AddressMode::Relative) => self.bvc(),
            // 0x51 - EOR (d),y
            (Opcode::EOR, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::eor)
            }
            // 0x52 - HLT 0x02
            // 0x53 - SRE (d),y
            (Opcode::SRE, AddressMode::IndirectY) => {
                self.rmw_instruction(Self::addr_indirect_y, Self::sre)
            }
            // 0x54 - NOP 0x14
            // 0x55 - EOR d,x
            (Opcode::EOR, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::eor)
            }
            // 0x56 - LSR d,x
            (Opcode::LSR, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::lsr)
            }
            // 0x57 - SRE d,x
            (Opcode::SRE, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::sre)
            }
            // 0x58 - CLI
            (Opcode::CLI, AddressMode::Implied) => self.cli(),
            // 0x59 - EOR a,y
            (Opcode::EOR, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::eor)
            }
            // 0x5A - NOP see 0x1A
            // 0x5B - SRE a,y
            (Opcode::SRE, AddressMode::AbsoluteY) => {
                self.rmw_instruction(Self::addr_absolute_y, Self::sre)
            }
            // 0x5C - NOP see 0x1C
            // 0x5D - EOR a,x
            (Opcode::EOR, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::eor)
            }
            // 0x5E - LSR a,x
            (Opcode::LSR, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::lsr)
            }
            // 0x5F - SRE a,x
            (Opcode::SRE, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::sre)
            }
            // 0x60 - RTS
            (Opcode::RTS, AddressMode::Implied) => self.rts(),
            // 0x61 - ADC (d,x)
            (Opcode::ADC, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::adc)
            }
            // 0x62 - HLT see 0x02
            // 0x63 - RRA (d,x)
            (Opcode::RRA, AddressMode::IndirectX) => {
                self.rmw_instruction(Self::addr_indirect_x, Self::rra)
            }
            // 0x64 - NOP - see 0x04
            // 0x65 - ADC d
            (Opcode::ADC, AddressMode::ZeroPage) => self.load_instruction(Self::addr_zp, Self::adc),
            // 0x66 - ROR d
            (Opcode::ROR, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::ror),
            // 0x67 - RRA d
            (Opcode::RRA, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::rra),
            // 0x68 - PLA
            (Opcode::PLA, AddressMode::Implied) => self.pla(),
            // 0x69 - ADC #i
            (Opcode::ADC, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::adc)
            }
            // 0x6A - ROR
            (Opcode::ROR, AddressMode::Implied) => self.ror_acc(),
            // 0x6B - ARR #i
            (Opcode::ARR, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::arr)
            }
            // 0x6C - JMP (a)
            (Opcode::JMP, AddressMode::AbsoluteIndirect) => self.jmp_indirect(),
            // 0x6D - ADC a
            (Opcode::ADC, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::adc)
            }
            // 0x6E - ROR a
            (Opcode::ROR, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::ror)
            }
            // 0x6F - RRA a
            (Opcode::RRA, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::rra)
            }
            // 0x70 - BVS *+r
            (Opcode::BVS, AddressMode::Relative) => self.bvs(),
            // 0x71 - ADC (d),y
            (Opcode::ADC, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::adc)
            }
            // 0x72 - HLT - see 0x02
            // 0x73 - RRA (d),y
            (Opcode::RRA, AddressMode::IndirectY) => {
                self.rmw_instruction(Self::addr_indirect_y, Self::rra)
            }
            // 0x74 - NOP - see 0x14
            // 0x75 - ADC d,x
            (Opcode::ADC, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::adc)
            }
            // 0x76 - ROR d,x
            (Opcode::ROR, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::ror)
            }
            // 0x77 - RRA d,x
            (Opcode::RRA, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::rra)
            }
            // 0x78 - SEI
            (Opcode::SEI, AddressMode::Implied) => self.sei(),
            // 0x79 - ADC a,y
            (Opcode::ADC, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::adc)
            }
            // 0x7A - NOP - see 0x1A
            // 0x7B - RRA a,y
            (Opcode::RRA, AddressMode::AbsoluteY) => {
                self.rmw_instruction(Self::addr_absolute_y, Self::rra)
            }
            // 0x7C - NOP - see 0x1C
            // 0x7D - ADC a,x
            (Opcode::ADC, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::adc)
            }
            // 0x7E - ROR a,x
            (Opcode::ROR, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::ror)
            }
            // 0x7F - RRA a,x
            (Opcode::RRA, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::rra)
            }
            // 0x80 0x82 0x89 0xC2 0xE2 - NOP #i
            (Opcode::NOP, AddressMode::Immediate) => self.addr_immediate(&InstructionMode::Load),
            // 0x81 - STA (d,x)
            (Opcode::STA, AddressMode::IndirectX) => {
                self.store_instruction(Self::addr_indirect_x, self.a().0)
            }
            // 0x82 - NOP see 0x80
            // 0x83 - SAX (d,x)
            (Opcode::SAX, AddressMode::IndirectX) => {
                self.store_instruction(Self::addr_indirect_x, self.a().0 & self.x().0)
            }
            // 0x84 - STY d
            (Opcode::STY, AddressMode::ZeroPage) => {
                self.store_instruction(Self::addr_zp, self.y().0)
            }
            // 0x85 - STA d
            (Opcode::STA, AddressMode::ZeroPage) => {
                self.store_instruction(Self::addr_zp, self.a().0)
            }
            // 0x86 - STX d
            (Opcode::STX, AddressMode::ZeroPage) => {
                self.store_instruction(Self::addr_zp, self.x().0)
            }
            // 0x87 - SAX d
            (Opcode::SAX, AddressMode::ZeroPage) => {
                self.store_instruction(Self::addr_zp, self.a().0 & self.x().0)
            }
            // 0x88 - DEY
            (Opcode::DEY, AddressMode::Implied) => {
                self.load_register(Register::Y, (self.y() - Wrapping(1)).0)
            }
            // 0x89 - NOP see 0x80
            // 0x8A - TXA
            (Opcode::TXA, AddressMode::Implied) => self.load_register(Register::A, self.x().0),
            // 0x8B - XAA #i
            (Opcode::XAA, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::xaa)
            }
            // 0x8C - STY a
            (Opcode::STY, AddressMode::Absolute) => {
                self.store_instruction(Self::addr_absolute, self.y().0)
            }
            // 0x8D - STA a
            (Opcode::STA, AddressMode::Absolute) => {
                self.store_instruction(Self::addr_absolute, self.a().0)
            }
            // 0x8E - STX a
            (Opcode::STX, AddressMode::Absolute) => {
                self.store_instruction(Self::addr_absolute, self.x().0)
            }
            // 0x8F - SAX a
            (Opcode::SAX, AddressMode::Absolute) => {
                self.store_instruction(Self::addr_absolute, self.a().0 & self.x().0)
            }
            // 0x90 - BCC *+r
            (Opcode::BCC, AddressMode::Relative) => self.bcc(),
            // 0x91 - STA (d),y
            (Opcode::STA, AddressMode::IndirectY) => {
                self.store_instruction(Self::addr_indirect_y, self.a().0)
            }
            // 0x92 - HLT see 0x02
            // 0x93 - AHX (d),y
            (Opcode::AHX, AddressMode::IndirectY) => self.ahx(Self::addr_indirect_y),
            // 0x94 - STY d,x
            (Opcode::STY, AddressMode::ZeroPageX) => {
                self.store_instruction(Self::addr_zp_x, self.y().0)
            }
            // 0x95 - STA d,x
            (Opcode::STA, AddressMode::ZeroPageX) => {
                self.store_instruction(Self::addr_zp_x, self.a().0)
            }
            // 0x96 - STX d,y
            (Opcode::STX, AddressMode::ZeroPageY) => {
                self.store_instruction(Self::addr_zp_y, self.x().0)
            }
            // 0x97 - SAX d,y
            (Opcode::SAX, AddressMode::ZeroPageY) => {
                self.store_instruction(Self::addr_zp_y, self.a().0 & self.x().0)
            }
            // 0x98 - TYA
            (Opcode::TYA, AddressMode::Implied) => self.load_register(Register::A, self.y().0),
            // 0x99 - STA a,y
            (Opcode::STA, AddressMode::AbsoluteY) => {
                self.store_instruction(Self::addr_absolute_y, self.a().0)
            }
            // 0x9A - TXS
            (Opcode::TXS, AddressMode::Implied) => {
                self.s_mut(self.x());
                Ok(OpState::Done)
            }
            // 0x9B - TAS a,y
            (Opcode::TAS, AddressMode::AbsoluteY) => self.tas(),
            // 0x9C - SHY a,x
            (Opcode::SHY, AddressMode::AbsoluteX) => self.shy(Self::addr_absolute_x),
            // 0x9D - STA a,x
            (Opcode::STA, AddressMode::AbsoluteX) => {
                self.store_instruction(Self::addr_absolute_x, self.a().0)
            }
            // 0x9E - SHX a,y
            (Opcode::SHX, AddressMode::AbsoluteY) => self.shx(Self::addr_absolute_y),
            // 0x9F - AHX a,y
            (Opcode::AHX, AddressMode::AbsoluteY) => self.ahx(Self::addr_absolute_y),
            // 0xA0 - LDY #i
            (Opcode::LDY, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::load_register_y)
            }
            // 0xA1 - LDA (d,x)
            (Opcode::LDA, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::load_register_a)
            }
            // 0xA2 - LDX #i
            (Opcode::LDX, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::load_register_x)
            }
            // 0xA3 - LAX (d,x)
            (Opcode::LAX, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::lax)
            }
            // 0xA4 - LDY d
            (Opcode::LDY, AddressMode::ZeroPage) => {
                self.load_instruction(Self::addr_zp, Self::load_register_y)
            }
            // 0xA5 - LDA d
            (Opcode::LDA, AddressMode::ZeroPage) => {
                self.load_instruction(Self::addr_zp, Self::load_register_a)
            }
            // 0xA6 - LDX d
            (Opcode::LDX, AddressMode::ZeroPage) => {
                self.load_instruction(Self::addr_zp, Self::load_register_x)
            }
            // 0xA7 - LAX d
            (Opcode::LAX, AddressMode::ZeroPage) => self.load_instruction(Self::addr_zp, Self::lax),
            // 0xA8 - TAY
            (Opcode::TAY, AddressMode::Implied) => self.load_register(Register::Y, self.a().0),
            // 0xA9 - LDA #i
            (Opcode::LDA, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::load_register_a)
            }
            // 0xAA - TAX
            (Opcode::TAX, AddressMode::Implied) => self.load_register(Register::X, self.a().0),
            // 0xAB - OAL #i
            (Opcode::OAL, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::oal)
            }
            // 0xAC - LDY a
            (Opcode::LDY, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::load_register_y)
            }
            // 0xAD - LDA a
            (Opcode::LDA, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::load_register_a)
            }
            // 0xAE - LDX a
            (Opcode::LDX, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::load_register_x)
            }
            // 0xAF - LAX a
            (Opcode::LAX, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::lax)
            }
            // 0xB0 - BCS *+r
            (Opcode::BCS, AddressMode::Relative) => self.bcs(),
            // 0xB1 - LDA (d),y
            (Opcode::LDA, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::load_register_a)
            }
            // 0xB2 - HLT see 0x02
            // 0xB3 - LAX (d),y
            (Opcode::LAX, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::lax)
            }
            // 0xB4 - LDY d,x
            (Opcode::LDY, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::load_register_y)
            }
            // 0xB5 - LDA d,x
            (Opcode::LDA, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::load_register_a)
            }
            // 0xB6 - LDX d,y
            (Opcode::LDX, AddressMode::ZeroPageY) => {
                self.load_instruction(Self::addr_zp_y, Self::load_register_x)
            }
            // 0xB7 - LAX d,y
            (Opcode::LAX, AddressMode::ZeroPageY) => {
                self.load_instruction(Self::addr_zp_y, Self::lax)
            }
            // 0xB8 - CLV
            (Opcode::CLV, AddressMode::Implied) => self.clv(),
            // 0xB9 - LDA a,y
            (Opcode::LDA, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::load_register_a)
            }
            // 0xBA - TSX
            (Opcode::TSX, AddressMode::Implied) => self.load_register(Register::X, self.s().0),
            // 0xBB - LAS a,y
            (Opcode::LAS, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::las)
            }
            // 0xBC - LDY a,x
            (Opcode::LDY, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::load_register_y)
            }
            // 0xBD - LDA a,x
            (Opcode::LDA, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::load_register_a)
            }
            // 0xBE - LDX a,y
            (Opcode::LDX, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::load_register_x)
            }
            // 0xBF - LAX a,y
            (Opcode::LAX, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::lax)
            }
            // 0xC0 - CPY #i
            (Opcode::CPY, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::compare_y)
            }
            // 0xC1 - CMP (d,x)
            (Opcode::CMP, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::compare_a)
            }
            // 0xC2 - NOP see 0x80
            // 0xC3 - DCP (d,X)
            (Opcode::DCP, AddressMode::IndirectX) => {
                self.rmw_instruction(Self::addr_indirect_x, Self::dcp)
            }
            // 0xC4 - CPY d
            (Opcode::CPY, AddressMode::ZeroPage) => {
                self.load_instruction(Self::addr_zp, Self::compare_y)
            }
            // 0xC5 - CMP d
            (Opcode::CMP, AddressMode::ZeroPage) => {
                self.load_instruction(Self::addr_zp, Self::compare_a)
            }
            // 0xC6 - DEC d
            (Opcode::DEC, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::dec),
            // 0xC7 - DCP d
            (Opcode::DCP, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::dcp),
            // 0xC8 - INY
            (Opcode::INY, AddressMode::Implied) => {
                self.load_register(Register::Y, (self.y() + Wrapping(1)).0)
            }
            // 0xC9 - CMP #i
            (Opcode::CMP, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::compare_a)
            }
            // 0xCA - DEX
            (Opcode::DEX, AddressMode::Implied) => {
                self.load_register(Register::X, (self.x() - Wrapping(1)).0)
            }
            // 0xCB - AXS #i
            (Opcode::AXS, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::axs)
            }
            // 0xCC - CPY a
            (Opcode::CPY, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::compare_y)
            }
            // 0xCD - CMP a
            (Opcode::CMP, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::compare_a)
            }
            // 0xCE - DEC a
            (Opcode::DEC, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::dec)
            }
            // 0xCF - DCP a
            (Opcode::DCP, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::dcp)
            }
            // 0xD0 - BNE *+r
            (Opcode::BNE, AddressMode::Relative) => self.bne(),
            // 0xD1 - CMP (d),y
            (Opcode::CMP, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::compare_a)
            }
            // 0xD2 - HLT see 0x02
            // 0xD3 - DCP (d),y
            (Opcode::DCP, AddressMode::IndirectY) => {
                self.rmw_instruction(Self::addr_indirect_y, Self::dcp)
            }
            // 0xD4 - NOP see 0x14
            // 0xD5 - CMP d,x
            (Opcode::CMP, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::compare_a)
            }
            // 0xD6 - DEC d,x
            (Opcode::DEC, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::dec)
            }
            // 0xD7 - DCP d,x
            (Opcode::DCP, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::dcp)
            }
            // 0xD8 - CLD
            (Opcode::CLD, AddressMode::Implied) => self.cld(),
            // 0xD9 - CMP a,y
            (Opcode::CMP, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::compare_a)
            }
            // 0xDA - NOP see 0x1A
            // 0xDB - DCP a,y
            (Opcode::DCP, AddressMode::AbsoluteY) => {
                self.rmw_instruction(Self::addr_absolute_y, Self::dcp)
            }
            // 0xDC - NOP see 0x1C
            // 0xDD - CMP a,x
            (Opcode::CMP, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::compare_a)
            }
            // 0xDE - DEC a,x
            (Opcode::DEC, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::dec)
            }
            // 0xDF - DCP a,x
            (Opcode::DCP, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::dcp)
            }
            // 0xE0 - CPX #i
            (Opcode::CPX, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::compare_x)
            }
            // 0xE1 - SBC (d,x)
            (Opcode::SBC, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::sbc)
            }
            // 0xE2 - NOP see 0x80
            // 0xE3 - ISC (d,x)
            (Opcode::ISC, AddressMode::IndirectX) => {
                self.rmw_instruction(Self::addr_indirect_x, Self::isc)
            }
            // 0xE4 - CPX d
            (Opcode::CPX, AddressMode::ZeroPage) => {
                self.load_instruction(Self::addr_zp, Self::compare_x)
            }
            // 0xE5 - SBC d
            (Opcode::SBC, AddressMode::ZeroPage) => self.load_instruction(Self::addr_zp, Self::sbc),
            // 0xE6 - INC d
            (Opcode::INC, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::inc),
            // 0xE7 - ISC d
            (Opcode::ISC, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::isc),
            // 0xE8 - INX
            (Opcode::INX, AddressMode::Implied) => {
                self.load_register(Register::X, (self.x() + Wrapping(1)).0)
            }
            // 0xE9 0xEB - SBC #i
            (Opcode::SBC, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::sbc)
            }
            // 0xEA - NOP see 0x1A
            // 0xEB - SBC see 0xE9
            // 0xEC - CPX a
            (Opcode::CPX, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::compare_x)
            }
            // 0xED - SBC a
            (Opcode::SBC, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::sbc)
            }
            // 0xEE - INC a
            (Opcode::INC, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::inc)
            }
            // 0xEF - ISC a
            (Opcode::ISC, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::isc)
            }
            // 0xF0 - BEQ *+r
            (Opcode::BEQ, AddressMode::Relative) => self.beq(),
            // 0xF1 - SBC (d),y
            (Opcode::SBC, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::sbc)
            }
            // 0xF2 - HLT see 0x02
            // 0xF3 - ISC (d),y
            (Opcode::ISC, AddressMode::IndirectY) => {
                self.rmw_instruction(Self::addr_indirect_y, Self::isc)
            }
            // 0xF4 - NOP see 0x14
            // 0xF5 - SBC d,x
            (Opcode::SBC, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::sbc)
            }
            // 0xF6 - INC d,x
            (Opcode::INC, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::inc)
            }
            // 0xF7 - ISC d,x
            (Opcode::ISC, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::isc)
            }
            // 0xF8 - SED
            (Opcode::SED, AddressMode::Implied) => self.sed(),
            // 0xF9 - SBC a,y
            (Opcode::SBC, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::sbc)
            }
            // 0xFA - NOP see 0x1A
            // 0xFB - ISC a,y
            (Opcode::ISC, AddressMode::AbsoluteY) => {
                self.rmw_instruction(Self::addr_absolute_y, Self::isc)
            }
            // 0xFC - NOP see 0x1C
            // 0xFD - SBC a,x
            (Opcode::SBC, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::sbc)
            }
            // 0xFE - INC a,x
            (Opcode::INC, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::inc)
            }
            // 0xFF - ISC a,x
            (Opcode::ISC, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::isc)
            }
            _ => Err(eyre!(
                "no implementation for {:?} {:?}",
                self.op().op,
                self.op().mode
            )),
        }
    }

    // ahx implements the undocumented AHX instruction based on the addressing mode passed in.
    // The value stored is (A & X & (ADDR_HI + 1))
    // Returns Done when complete and/or errors
    fn ahx(
        &mut self,
        address_mode: fn(&mut Self, &InstructionMode) -> Result<OpState>,
    ) -> Result<OpState> {
        // This is a store but we can't use store_instruction since it depends on knowing op_addr
        // for the final computed value so we have to do the addressing mode ourselves.
        match self.addr_done() {
            OpState::Processing => {
                let ret = address_mode(self, &InstructionMode::Store)?;
                self.addr_done_mut(ret);
                Ok(OpState::Processing)
            }
            OpState::Done => {
                let val =
                    (self.a() & self.x() & (Wrapping((self.op_addr() >> 8) as u8) + Wrapping(1))).0;
                self.ram().borrow_mut().write(self.op_addr(), val);
                Ok(OpState::Done)
            }
        }
    }

    // alr implements the undocumented opcode for ALR.
    // This does AND #i (op_val) and then LSR on A setting all associated flags.
    // Always returns Done since this takes one tick and never returns an error.
    fn alr(&mut self) -> Result<OpState> {
        self.load_register(Register::A, self.a().0 & self.op_val())?;
        self.lsr_acc()
    }

    // anc implements the undocumented opcode for ANC. This does AND #i (op_val) and then
    // sets carry based on bit 7 (sign extend).
    // Always returns Done since this takes one tick and never returns an error.
    fn anc(&mut self) -> Result<OpState> {
        self.load_register(Register::A, self.a().0 & self.op_val())?;
        self.carry_check(u16::from(self.a().0) << 1);
        Ok(OpState::Done)
    }

    // arr implements implements the undocumented opcode for ARR.
    // This does AND #i (p.opVal) and then ROR except some flags are set differently.
    // Implemented as described in http://nesdev.com/6502_cpu.txt
    // Always returns Done since this takes one tick and never returns an error.
    fn arr(&mut self) -> Result<OpState> {
        let val = self.a().0 & self.op_val();
        self.load_register(Register::A, val)?;
        self.ror_acc()?;

        // Flags are different based on BCD or not (since the ALU acts different).
        if self.p() & P_DECIMAL != P_NONE {
            // If bit 6 changed state between AND output and rotate output then set V.
            if (val ^ self.a().0) & 0x40 == 0x00 {
                self.p_mut(self.p() & !P_OVERFLOW);
            } else {
                self.p_mut(self.p() | P_OVERFLOW);
            }

            // Now do possible odd BCD fixups and set C
            let ah = val >> 4;
            let al = val & 0x0F;
            if (al + (al & 0x01)) > 5 {
                self.a_mut(Wrapping((self.a().0 & 0xF0) | ((self.a().0 + 6) & 0x0F)));
            }
            if (ah + (ah & 0x01)) > 5 {
                self.p_mut(self.p() | P_CARRY);
                self.a_mut(self.a() + Wrapping(0x60));
            } else {
                self.p_mut(self.p() & !P_CARRY);
            }
            return Ok(OpState::Done);
        }

        // C is bit 6
        self.carry_check((u16::from(self.a().0) << 2) & 0x0100);
        // V is bit 5 ^ bit 6
        if ((self.a().0 & 0x40) >> 6) ^ ((self.a().0 & 0x20) >> 5) == 0x00 {
            self.p_mut(self.p() & !P_OVERFLOW);
        } else {
            self.p_mut(self.p() | P_OVERFLOW);
        }
        Ok(OpState::Done)
    }

    // axs implements the undocumented opcode for AXS.
    // (A AND X) - p.opVal (no borrow) setting all associated flags post SBC.
    // Always returns Done since this takes one tick and never returns an error.
    fn axs(&mut self) -> Result<OpState> {
        // Save A off to restore later
        let a = self.a().0;
        self.load_register(Register::A, self.a().0 & self.x().0)?;
        // Carry is always set
        self.p_mut(self.p() | P_CARRY);

        // Save D & V state since it's always ignored for this but needs to keep values.
        let d = self.p() & P_DECIMAL;
        let v = self.p() & P_OVERFLOW;
        // Clear D so SBC never uses BCD mode (we'll reset it later from saved state).
        self.p_mut(self.p() & !P_DECIMAL);
        self.sbc()?;

        // Clear V now in case SBC set it so we can properly restore it below.
        self.p_mut(self.p() & !P_OVERFLOW);

        // Save A in a temp so we can load registers in the right order to set flags (based on X, not old A)
        let x = self.a().0;
        self.load_register(Register::A, a)?;
        self.load_register(Register::X, x)?;

        // Restore D & V from our initial state.
        self.p_mut(self.p() | d | v);
        Ok(OpState::Done)
    }

    // dcp implements the undocumented opcode for DCP.
    // This decrements the value at op_addr and then does a CMP with A setting associated flags.
    // Always returns Done since this takes one tick and never returns an error.
    fn dcp(&mut self) -> Result<OpState> {
        self.op_val_mut((Wrapping(self.op_val()) - Wrapping(1)).0);
        self.ram().borrow_mut().write(self.op_addr(), self.op_val());
        self.compare_a()
    }

    // isc implements the undocumented opcode for ISC.
    // This increments the value at op_addr and then does an SBC and sets associated flags.
    // Always returns Done since this takes one tick and never returns an error.
    fn isc(&mut self) -> Result<OpState> {
        self.op_val_mut((Wrapping(self.op_val()) + Wrapping(1)).0);
        self.ram().borrow_mut().write(self.op_addr(), self.op_val());
        self.sbc()
    }

    // las implements the undocumented opcode for LAS.
    // This take op_val and ANDs it with S and then stores that in A,X,S setting flags accordingly.
    // Always returns Done since this takes one tick and never returns an error.
    fn las(&mut self) -> Result<OpState> {
        self.s_mut(Wrapping(self.s().0 & self.op_val()));
        self.load_register(Register::X, self.s().0)?;
        self.load_register(Register::A, self.s().0)
    }

    // lax implements the undocumented opcode for LAX.
    // This loads A and X with the same value and sets all associated flags.
    // Always returns Done since this takes one tick and never returns an error.
    fn lax(&mut self) -> Result<OpState> {
        self.load_register(Register::A, self.op_val())?;
        self.load_register(Register::X, self.op_val())
    }

    // oal implements the undocumented opcode for OAL.
    // This one acts a bit randomly. It somtimes does XAA and sometimes
    // does A=X=A&val.
    // Always returns Done since this takes one tick and never returns an error.
    fn oal(&mut self) -> Result<OpState> {
        let mut rng = rand::thread_rng();
        if rng.gen::<f64>() > 0.5 {
            return self.xaa();
        }
        let val = self.a().0 & self.op_val();
        self.load_register(Register::A, val)?;
        self.load_register(Register::X, val)
    }

    // rla implements the undocumented opcode for RLA. This does a ROL on
    // the contents of op_addr and then AND's it against A. Sets flags and carry.
    // Always returns Done since this takes one tick and never returns an error.
    fn rla(&mut self) -> Result<OpState> {
        let val = (self.op_val() << 1) | (self.p() & P_CARRY).0;
        self.ram().borrow_mut().write(self.op_addr(), val);
        self.carry_check(u16::from(self.op_val()) << 1);
        self.load_register(Register::A, self.a().0 & val)
    }

    // rra implements the undocumented opcode for RRA. This does a ROR on op_addr
    // and then ADC's it against A. Sets flags and carry.
    // Always returns Done since this is one tick and never returns an error.
    fn rra(&mut self) -> Result<OpState> {
        let n = ((self.p() & P_CARRY).0 << 7) | (self.op_val() >> 1);
        self.ram().borrow_mut().write(self.op_addr(), n);
        // Old bit 0 becomes carry
        self.carry_check((u16::from(self.op_val()) << 8) & 0x0100);
        self.op_val_mut(n);
        self.adc()
    }

    // shx_shy_common implements the common logic for SHX and SHY.
    fn shx_shy_common(
        &mut self,
        address_mode: fn(&mut Self, &InstructionMode) -> Result<OpState>,
        v: Wrapping<u8>,
    ) -> Result<OpState> {
        // This is a store but we can't use store_instruction since it depends on knowing op_addr
        // for the final computed value so we have to do the addressing mode ourselves.
        match self.addr_done() {
            OpState::Processing => {
                let ret = address_mode(self, &InstructionMode::Store)?;
                self.addr_done_mut(ret);
                Ok(OpState::Processing)
            }
            OpState::Done => {
                let val = (v & (Wrapping((self.op_addr() >> 8) as u8) + Wrapping(1))).0;
                self.ram().borrow_mut().write(self.op_addr(), val);
                Ok(OpState::Done)
            }
        }
    }

    // shx implements the undocumented SHX instruction based on the addressing mode passed in.
    // The value stored is (X & (ADDR_HI + 1))
    // Returns Done and/or errors when complete.
    fn shx(
        &mut self,
        address_mode: fn(&mut Self, &InstructionMode) -> Result<OpState>,
    ) -> Result<OpState> {
        self.shx_shy_common(address_mode, self.x())
    }

    // shy implements the undocumented SHY instruction based on the addressing mode passed in.
    // The value stored is (Y & (ADDR_HI + 1))
    // Returns Done and/or errors when complete.
    fn shy(
        &mut self,
        address_mode: fn(&mut Self, &InstructionMode) -> Result<OpState>,
    ) -> Result<OpState> {
        self.shx_shy_common(address_mode, self.y())
    }

    // slo implements the undocumented opcode for SLO. This does an ASL on the
    // contents of op_addr and then OR's it against A. Sets flags and carry.
    // Always returns Done since this takes one tick and never returns an error.
    fn slo(&mut self) -> Result<OpState> {
        self.ram()
            .borrow_mut()
            .write(self.op_addr(), self.op_val() << 1);
        self.carry_check(u16::from(self.op_val()) << 1);
        self.load_register(Register::A, (self.op_val() << 1) | self.a().0)
    }

    // sre implements the undocumented opcode for SRE. This does a LSR on
    // op_addr and then XOR's it against A. Sets flags and carry.
    // Always returns Done since this takes one tick and never returns an error.
    fn sre(&mut self) -> Result<OpState> {
        self.ram()
            .borrow_mut()
            .write(self.op_addr(), self.op_val() >> 1);
        // Old bit 0 becomes carry
        self.carry_check(u16::from(self.op_val()) << 8);
        self.load_register(Register::A, (self.op_val() >> 1) ^ self.a().0)
    }

    // tas implements the undocumented opcode for TAS which only has one addressing mode.
    // This does the same operations as AHX above but then also sets S = A&X
    // Returns Done and/or errors when complete.
    fn tas(&mut self) -> Result<OpState> {
        self.s_mut(self.a() & self.x());
        self.ahx(Self::addr_absolute_y)
    }

    // xaa implements the undocumented opcode for XAA.
    // We'll go with http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_(XAA,_ANE)
    // for implementation and pick 0xEE as the constant. According to VICE this may break so
    // we might need to change it to 0xFF.
    // https://sourceforge.net/tracker/?func=detail&aid=2110948&group_id=223021&atid=1057617
    // Always returns Done since this takes one tick and never returns an error.
    fn xaa(&mut self) -> Result<OpState> {
        self.load_register(
            Register::A,
            (self.a().0 | 0xEE) & self.x().0 & self.op_val(),
        )
    }
}

impl<'a> CPUNmosInternal<'a> for CPU6502<'a> {}
impl<'a> CPUNmosInternal<'a> for CPU6510<'a> {}
impl<'a> CPUNmosInternal<'a> for CPURicoh<'a> {}

// The common implementation definitions for all 6502 chips but done as a private
// crate only trait so the default implementations can get at all the internal
// cpu entries without having to expose those publicly.
trait CPUInternal<'a>: Chip + CPU<'a> {
    // The following methods are all ones which cannot be defaulted
    // and must be implmented by the relevant struct in order to provide
    // access and mutability for the default trait methods below.

    // debug_hook returns the optional debug function callback.
    fn debug_hook(&self) -> Option<&'a dyn Fn() -> (Rc<RefCell<CPUState>>, bool)>;

    // state returns the current CPU state.
    fn state(&self) -> State;
    // state_mut sets the current CPU state.
    fn state_mut(&mut self, new: State);

    // a returns the contents of the A register.
    fn a(&self) -> Wrapping<u8>;
    // a_mut sets the contents of the A register.
    fn a_mut(&mut self, new: Wrapping<u8>);

    // x returns the contents of the X register.
    fn x(&self) -> Wrapping<u8>;
    // x_mut sets the contents of the X register.
    fn x_mut(&mut self, new: Wrapping<u8>);

    // y returns the contents of the Y register.
    fn y(&self) -> Wrapping<u8>;
    // y_mut sets the contents of the Y register.
    fn y_mut(&mut self, new: Wrapping<u8>);

    // s returns the contents of the S register.
    fn s(&self) -> Wrapping<u8>;
    // s_mut sets the contents of the S register.
    fn s_mut(&mut self, new: Wrapping<u8>);

    // p returns the current Flags from the P register.
    fn p(&self) -> Flags;
    // p_mut sets the P register to the given Flags.
    fn p_mut(&mut self, new: Flags);

    // clocks returns the number of clock cycles since startup.
    fn clocks(&self) -> usize;

    // op_val returns the internal op_val register.
    fn op_val(&self) -> u8;
    // op_val_mut sets the internal op_val register.
    fn op_val_mut(&mut self, new: u8);

    // op_addr returns the internal op_addr register.

    fn op_addr(&self) -> u16;
    // op_addr_mut sets the internal op_addr register.
    fn op_addr_mut(&mut self, new: u16);

    // op_raw returns the current opcode byte.
    fn op_raw(&self) -> u8;

    // op returns the current decoded Operation.
    fn op(&self) -> Operation;

    // addr_done returns the internal addr_done state.
    fn addr_done(&self) -> OpState;
    // addr_done_mut sets the internal addr_done state.
    fn addr_done_mut(&mut self, new: OpState);

    // op_tick returns the current Tick value for the operation in progress.
    fn op_tick(&self) -> Tick;

    // skip_interrupt returns the current SkipInterrupt state.
    fn skip_interrupt(&self) -> SkipInterrupt;
    // skip_interrupt_mut sets the SkipInterrupt state.
    fn skip_interrupt_mut(&mut self, new: SkipInterrupt);

    // irq_raised returns the current InterruptStyle state.
    fn irq_raised(&self) -> InterruptStyle;
    // irq_raised_mut sets the InterruptStyle state.
    fn irq_raised_mut(&mut self, new: InterruptStyle);

    // run_interrupt does all the heavy lifting for any interrupt processing.
    // i.e. pushing values onto the stack and loading PC with the right address.
    // Pass in the vector to be used for loading the PC (which means for BRK
    // it can change if an NMI happens before we get to the load ticks).
    // Returns OpState::Done when complete (and PC is correct). Can return an error on an
    // invalid tick count.
    fn run_interrupt(&mut self, vec: u16, irq: bool) -> Result<OpState> {
        match self.op_tick() {
            Tick::Reset | Tick::Tick1 | Tick::Tick8 => Err(eyre!(
                "run_interrupt: invalid op_tick: {:?}",
                self.op_tick()
            )),
            Tick::Tick2 => {
                // Increment the PC on a non IRQ (i.e. BRK) since that changes where returns happen.
                if !irq {
                    self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);
                }
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // There is no truncation as we mask and shift into 8 bits.
                #[allow(clippy::cast_possible_truncation)]
                self.push_stack(((self.pc() & 0xFF00) >> 8) as u8);
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // There is no truncation as we mask into 8 bits.
                #[allow(clippy::cast_possible_truncation)]
                self.push_stack((self.pc() & 0x00FF) as u8);
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                let mut push = self.p();
                // S1 is always set
                push |= P_S1;
                // B always set unless this triggered due to IRQ
                push |= P_B;
                if irq {
                    push &= !P_B;
                }
                self.push_stack(push.0);
                // Now set P after we've pushed.

                // For BRK/IRQ we set I. NMI does not.
                if self.irq_raised() != InterruptStyle::NMI {
                    self.p_mut(self.p() | P_INTERRUPT);
                }
                Ok(OpState::Processing)
            }
            Tick::Tick6 => {
                self.op_val_mut(self.ram().borrow().read(vec));
                Ok(OpState::Processing)
            }
            Tick::Tick7 => {
                // Compute the new PC from the 2nd vector component and the previous val read.
                self.pc_mut(
                    (u16::from(self.ram().borrow().read(vec + 1)) << 8) | u16::from(self.op_val()),
                );

                // If we didn't previously skip an interrupt from processing make sure we execute the first instruction of
                // a handler before firing again.
                if irq && self.skip_interrupt() != SkipInterrupt::PrevSkip {
                    self.skip_interrupt_mut(SkipInterrupt::Skip);
                }
                Ok(OpState::Done)
            }
        }
    }

    // load_register takes the val and inserts it into the given register.
    // It then does Z and N checks against the new value if and sets flags (only
    // for A/X/Y).
    // Always returns OpState::Done as this happens on a single tick.
    #[allow(clippy::unnecessary_wraps)]
    fn load_register(&mut self, reg: Register, val: u8) -> Result<OpState> {
        match reg {
            Register::A => self.a_mut(Wrapping(val)),
            Register::X => self.x_mut(Wrapping(val)),
            Register::Y => self.y_mut(Wrapping(val)),
            Register::P => {
                self.p_mut(Flags(val));
                return Ok(OpState::Done);
            }
        };
        self.zero_check(val);
        self.negative_check(val);
        Ok(OpState::Done)
    }

    // load_register_a is the curried version of load_register that uses op_val and A implicitly.
    // This way it can be used as the op_func argument during load/rmw instructions.
    // Always returns Done since this is a single tick operation.
    fn load_register_a(&mut self) -> Result<OpState> {
        self.load_register(Register::A, self.op_val())
    }

    // load_register_x is the curried version of load_register that uses op_val and X implicitly.
    // This way it can be used as the op_func argument during load/rmw instructions.
    // Always returns Done since this is a single tick operation.
    fn load_register_x(&mut self) -> Result<OpState> {
        self.load_register(Register::X, self.op_val())
    }

    // load_register_y is the curried version of load_register that uses op_val and Y implicitly.
    // This way it can be used as the op_func argument during load/rmw instructions.
    // Always returns Done since this is a single tick operation.
    fn load_register_y(&mut self) -> Result<OpState> {
        self.load_register(Register::Y, self.op_val())
    }

    // store_with_flags writes the value to the address given but also updates
    // N/Z flags at the same time.
    #[allow(clippy::unnecessary_wraps)]
    fn store_with_flags(&mut self, addr: u16, val: u8) -> Result<OpState> {
        self.zero_check(val);
        self.negative_check(val);
        self.ram().borrow_mut().write(addr, val);
        Ok(OpState::Done)
    }

    // compare implements the logic for all CMP/CPX/CPY instructions and
    // sets flags accordingly from the results.
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn compare(&mut self, reg: u8, val: u8) -> Result<OpState> {
        let check = (Wrapping(reg) - Wrapping(val)).0;
        self.zero_check(check);
        self.negative_check(check);
        // A-M done as 2's complement addition by ones complement and add 1
        // This way we get valid sign extension and a carry bit test.
        self.carry_check(u16::from(reg) + u16::from(!val) + 1);
        Ok(OpState::Done)
    }

    // compare_a is a curried version of compare that references A and uses op_val for the value.
    // This way it can be used as the op_func in load_instruction.
    // Always returns Done since this takes one tick and never returns an error.
    fn compare_a(&mut self) -> Result<OpState> {
        self.compare(self.a().0, self.op_val())
    }

    // compare_x is a curried version of compare that references X and uses op_val for the value.
    // This way it can be used as the op_func in load_instruction.
    // Always returns Done since this takes one tick and never returns an error.
    fn compare_x(&mut self) -> Result<OpState> {
        self.compare(self.x().0, self.op_val())
    }

    // compare_y is a curried version of compare that references Y and uses op_val for the value.
    // This way it can be used as the op_func in load_instruction.
    // Always returns Done since this takes one tick and never returns an error.
    fn compare_y(&mut self) -> Result<OpState> {
        self.compare(self.y().0, self.op_val())
    }

    // zero_check sets the Z flag based on the value.
    fn zero_check(&mut self, val: u8) {
        let mut new = self.p();
        new &= !P_ZERO;
        if val == 0 {
            new |= P_ZERO;
        }
        self.p_mut(new);
    }

    // negative_check sets the N flag based on the value.
    fn negative_check(&mut self, val: u8) {
        let mut new = self.p();
        new &= !P_NEGATIVE;
        if (val & P_NEGATIVE) == P_NEGATIVE {
            new |= P_NEGATIVE;
        }
        self.p_mut(new);
    }

    // carry_check sets the C flag based on the value.
    fn carry_check(&mut self, val: u16) {
        let mut new = self.p();
        new &= !P_CARRY;
        if val >= 0x0100 {
            new |= P_CARRY;
        }
        self.p_mut(new);
    }

    // overflow_check sets the V flag is the result of the ALU operation
    // caused a two's complement sign change.
    // Taken from http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    fn overflow_check(&mut self, reg: u8, arg: u8, result: u8) {
        let mut new = self.p();
        new &= !P_OVERFLOW;
        // If the original sign differs from the end sign bit.
        if (reg ^ result) & (arg ^ result) & 0x80 != 0x00 {
            new |= P_OVERFLOW;
        }
        self.p_mut(new);
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
        let ret = address_mode(self, &InstructionMode::Load)?;
        self.addr_done_mut(ret);
        match self.addr_done() {
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
        match self.addr_done() {
            OpState::Processing => {
                let ret = address_mode(self, &InstructionMode::Rmw)?;
                self.addr_done_mut(ret);
                Ok(OpState::Processing)
            }
            OpState::Done => op_func(self),
        }
    }

    // store_instruction abstracts all store instruction opcodes.  The address mode function is
    // used to get the proper values loaded into op_addr and op_val.
    // Then on the next tick the val passed is stored to op_addr.
    // Returns OpState::Done when complete and/or any error.
    fn store_instruction(
        &mut self,
        address_mode: fn(&mut Self, &InstructionMode) -> Result<OpState>,
        val: u8,
    ) -> Result<OpState> {
        match self.addr_done() {
            OpState::Processing => {
                let ret = address_mode(self, &InstructionMode::Store)?;
                self.addr_done_mut(ret);
                Ok(OpState::Processing)
            }
            OpState::Done => {
                self.ram().borrow_mut().write(self.op_addr(), val);
                Ok(OpState::Done)
            }
        }
    }

    // addr_absolute implements Absolute mode - a
    // returning the value in op_val and the address read in op_addr (so RW operations can do things without having to
    // reread memory incorrectly to compute a storage address).
    // If mode is RMW then another tick will occur that writes the read value back to the same address due to how
    // the 6502 operates.
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_absolute(&mut self, mode: &InstructionMode) -> Result<OpState> {
        match self.op_tick() {
            Tick::Reset | Tick::Tick1 | Tick::Tick6 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("addr_absolute invalid op_tick: {:?}", self.op_tick()))
            }
            Tick::Tick2 => {
                // op_val has the first start of the address so start computing it.
                self.op_addr_mut(u16::from(self.op_val()) & 0x00FF);
                self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                self.op_val_mut(self.ram().borrow().read(self.pc()));
                self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);
                self.op_addr_mut(self.op_addr() | u16::from(self.op_val()) << 8);
                match mode {
                    // For a store we're done since the opcode can decide what to store.
                    InstructionMode::Store => Ok(OpState::Done),
                    _ => Ok(OpState::Processing),
                }
            }
            Tick::Tick4 => {
                // For load and RMW we read the value
                self.op_val_mut(self.ram().borrow().read(self.op_addr()));
                match mode {
                    // For a load we're now done.
                    &InstructionMode::Load => Ok(OpState::Done),
                    _ => Ok(OpState::Processing),
                }
            }
            Tick::Tick5 => {
                // On NMOS there's a spurious write here where we replay back.
                self.ram().borrow_mut().write(self.op_addr(), self.op_val());
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
        self.addr_absolute_xy(mode, self.x().0)
    }

    // addr_absolute_y implements Absolute Y mode - a,y
    // returning the value in op_val and the address read in op_addr (so RW operations can do things without having to
    // reread memory incorrectly to compute a storage address).
    // If mode is RMW then another tick will occur that writes the read value back to the same address due to how
    // the 6502 operates.
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_absolute_y(&mut self, mode: &InstructionMode) -> Result<OpState> {
        self.addr_absolute_xy(mode, self.y().0)
    }

    // addr_absolute_xy implements the details for addr_absolute_x and addr_absolute_y since
    // they only differ based on the register used.
    // See those functions for arg/return specifics.
    fn addr_absolute_xy(&mut self, mode: &InstructionMode, reg: u8) -> Result<OpState> {
        match self.op_tick() {
            Tick::Reset | Tick::Tick1 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("addr_absolute invalid op_tick: {:?}", self.op_tick()))
            }
            Tick::Tick2 => {
                // op_val has the first start of the address so start computing it.
                self.op_addr_mut(u16::from(self.op_val()) & 0x00FF);
                self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                self.op_val_mut(self.ram().borrow().read(self.pc()));
                self.op_addr_mut(self.op_addr() | u16::from(self.op_val()) << 8);
                // Add reg but do it in a way which won't page wrap (if needed).
                let a = (self.op_addr() & 0xFF00)
                    | u16::from((Wrapping((self.op_addr() & 0x00FF) as u8) + Wrapping(reg)).0);
                self.op_val_mut(0);
                if a != (Wrapping(self.op_addr()) + Wrapping(u16::from(reg))).0 {
                    // Signal for next phase fixup is needed
                    self.op_val_mut(1);
                }
                self.op_addr_mut(a);
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // Check old opVal to see if it's non-zero. If so it means the reg addition
                // crosses a page boundary and we'll have to fixup otherwise this is fine.
                // For a load operation that means another tick to read the correct address.
                // For RMW it doesn't matter (we always do the extra tick).
                // For Store we're done. Just fixup p.opAddr so the return value is correct.

                // If we didn't make the wrong address and load/storing we're good.
                // For RMW we still read here but do continue to the next tick.
                if self.op_val() == 0 {
                    // Advance the PC since we didn't earlier as down below CMOS
                    // can reread this value.
                    self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);
                    self.op_val_mut(self.ram().borrow().read(self.op_addr()));
                    if mode == &InstructionMode::Rmw {
                        return Ok(OpState::Processing);
                    }
                    return Ok(OpState::Done);
                }

                // Every tick requires a bus cycle so while we're fixing up
                // we have to read something. In this case we read the wrong op_addr.
                // Leave op_val as it was so the next tick can tell we had to
                // do a fixup (for RMW since it always gets there).
                _ = self.ram().borrow().read(self.op_addr());
                self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);

                // We computed the wrong addr before so fix it now by page wrapping.
                self.op_addr_mut((Wrapping(self.op_addr()) + Wrapping(0x0100)).0);

                // Stores are done and ready to write on their next cycle since
                // the addr is correct now.
                if mode == &InstructionMode::Store {
                    return Ok(OpState::Done);
                }

                // Everything else runs another tick at this point
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                // So get the correct value in now for both loads (extra cycle for fixup)
                // and RMW (which always gets here minimum).
                self.op_val_mut(self.ram().borrow().read(self.op_addr()));

                // For a load we're done.
                // Otherwise RMW overflow always advances.
                if mode == &InstructionMode::Load {
                    return Ok(OpState::Done);
                }
                Ok(OpState::Processing)
            }
            Tick::Tick6 => {
                // On NMOS there's a spurious write here where we replay back
                // what we just read.
                self.ram().borrow_mut().write(self.op_addr(), self.op_val());
                Ok(OpState::Done)
            }
        }
    }

    // addr_immediate implements Immediate mode - #i
    // returning the value in op_val.
    // NOTE: This has no mode other than load so the argument is ignored.
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_immediate(&mut self, _mode: &InstructionMode) -> Result<OpState> {
        match self.op_tick() {
            Tick::Reset
            | Tick::Tick1
            | Tick::Tick3
            | Tick::Tick4
            | Tick::Tick5
            | Tick::Tick6
            | Tick::Tick7
            | Tick::Tick8 => Err(eyre!(
                "addr_immediate invalid op_tick: {:?}",
                self.op_tick()
            )),
            Tick::Tick2 => {
                // Already read the value but need to bump the PC
                // since this mode consumes op_val.
                // Set op_addr in case ADC/SBC need an extra bus cycle and a place
                // to read.
                self.op_addr_mut(self.pc());
                self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);
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
        match self.op_tick() {
            Tick::Reset | Tick::Tick1 | Tick::Tick8 => Err(eyre!(
                "addr_indirect_x invalid op_tick: {:?}",
                self.op_tick()
            )),
            Tick::Tick2 => {
                // We've already read the value but need to bump the PC
                // and assign it into op_addr so the throw away read in
                // tick3 reads the right place.
                self.op_addr_mut(u16::from(self.op_val()) & 0x00FF);
                self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // A throwaway read from the ZP addr. We'll add the X register as well for the real read next.
                _ = self.ram().borrow().read(self.op_addr());
                // Does this as a Wrapping so it wraps as needed since it stays in ZP.
                self.op_addr_mut(u16::from((Wrapping(self.op_val()) + self.x()).0));
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // Read effective addr low byte
                self.op_val_mut(self.ram().borrow().read(self.op_addr()));
                // Now increment (with ZP rollover) for next read.
                // There is no truncation since we know this is always
                // a u8 so within ZP.
                #[allow(clippy::cast_possible_truncation)]
                let a = Wrapping(self.op_addr() as u8);
                self.op_addr_mut(u16::from((a + Wrapping(1)).0));
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                // Read high byte, shift over and add op_val which has the low byte.
                self.op_addr_mut(
                    (u16::from(self.ram().borrow().read(self.op_addr())) << 8)
                        | u16::from(self.op_val()),
                );
                match mode {
                    // For a store we're done as op_addr now contains the destination address.
                    InstructionMode::Store => Ok(OpState::Done),
                    _ => Ok(OpState::Processing),
                }
            }
            Tick::Tick6 => {
                self.op_val_mut(self.ram().borrow().read(self.op_addr()));
                match mode {
                    // For a load we're done as we've loaded the value.
                    InstructionMode::Load => Ok(OpState::Done),
                    _ => Ok(OpState::Processing),
                }
            }
            Tick::Tick7 => {
                // On NMOS there's a spurious write here where we replay back
                // what we just read.
                self.ram().borrow_mut().write(self.op_addr(), self.op_val());
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
        match self.op_tick() {
            Tick::Reset | Tick::Tick1 | Tick::Tick8 => Err(eyre!(
                "addr_indirect_y invalid op_tick: {:?}",
                self.op_tick()
            )),
            Tick::Tick2 => {
                // We've already read the value but need to bump the PC
                // and assign it into op_addr so the throw away read in
                // tick3 reads the right place.
                self.op_addr_mut(u16::from(self.op_val()) & 0x00FF);
                self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // Read from the ZP addr to start building our pointer.
                self.op_val_mut(self.ram().borrow().read(self.op_addr()));
                // Setup op_addr for next read and handle ZP wrapping.
                #[allow(clippy::cast_possible_truncation)]
                let a = u16::from((Wrapping(self.op_addr() as u8) + Wrapping(1)).0);
                self.op_addr_mut(a);
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // Compute effective address and then add Y to it (possibly wrongly).
                self.op_addr_mut(
                    (u16::from(self.ram().borrow().read(self.op_addr())) << 8)
                        | u16::from(self.op_val()),
                );
                // Add Y but do it in a way which won't page wrap (if needed).
                #[allow(clippy::cast_possible_truncation)]
                let a = (self.op_addr() & 0xFF00)
                    | u16::from((Wrapping(self.op_addr() as u8) + self.y()).0);
                self.op_val_mut(0);
                if a != (Wrapping(self.op_addr()) + Wrapping(u16::from(self.y().0))).0 {
                    // Signal for next phase we got it wrong.
                    self.op_val_mut(1);
                }
                self.op_addr_mut(a);
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                // Save op_val so we know if this needed fixing.
                let t = self.op_val();
                // Even with an incorrect op_addr we still read from it.
                self.op_val_mut(self.ram().borrow().read(self.op_addr()));

                // Check old opVal to see if it's non-zero. If so it means the Y addition
                // crosses a page boundary and we'll have to fixup.
                // For a load operation that means another tick to read the correct
                // address.
                // For RMW it doesn't matter (we always do the extra tick).
                // For Store we're done. Just fixup p.opAddr so the return value is correct.
                let mut done = Ok(OpState::Done);
                if t != 0 {
                    self.op_addr_mut((Wrapping(self.op_addr()) + Wrapping(0x0100)).0);
                    if mode == &InstructionMode::Load {
                        done = Ok(OpState::Processing);
                    }
                }
                // For RMW it doesn't matter, we tick again.
                if mode == &InstructionMode::Rmw {
                    done = Ok(OpState::Processing);
                }
                done
            }
            Tick::Tick6 => {
                // Optional (on load) in case adding Y went past a page boundary.
                self.op_val_mut(self.ram().borrow().read(self.op_addr()));
                match mode {
                    InstructionMode::Rmw => Ok(OpState::Processing),
                    _ => Ok(OpState::Done),
                }
            }
            Tick::Tick7 => {
                // On NMOS there's a spurious write here where we replay back
                // what we just read.
                self.ram().borrow_mut().write(self.op_addr(), self.op_val());
                Ok(OpState::Done)
            }
        }
    }

    // addr_zp implements Zero page mode - d
    // returning the value in op_val and the address read in op_addr (so RW operations can do things without having to
    // reread memory incorrectly to compute a storage address).
    // If mode is RMW then another tick will occur that writes the read value back to the same address due to how
    // the 6502 operates. In the CMOS case this is a 2nd read.
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_zp(&mut self, mode: &InstructionMode) -> Result<OpState> {
        match self.op_tick() {
            Tick::Reset | Tick::Tick1 | Tick::Tick5 | Tick::Tick6 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("addr_zp invalid op_tick: {:?}", self.op_tick()))
            }
            Tick::Tick2 => {
                // Already read the value but need to bump the PC
                self.op_addr_mut(u16::from(self.op_val()));
                self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);
                match mode {
                    // For a store we're done since we have the address needed.
                    &InstructionMode::Store => Ok(OpState::Done),
                    _ => Ok(OpState::Processing),
                }
            }
            Tick::Tick3 => {
                self.op_val_mut(self.ram().borrow().read(self.op_addr()));
                match mode {
                    // For a load we're now done since the value is loaded.
                    &InstructionMode::Load => Ok(OpState::Done),
                    _ => Ok(OpState::Processing),
                }
            }
            Tick::Tick4 => {
                // On NMOS there's a spurious write here where we replay back
                // what we just read.
                self.ram().borrow_mut().write(self.op_addr(), self.op_val());
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
        self.addr_zp_xy(mode, self.x().0)
    }

    // addr_zp_y implements Zero page Y mode - d,y
    // returning the value in op_val and the address read in op_addr (so RW operations can do things without having to
    // reread memory incorrectly to compute a storage address).
    // If mode is RMW then another tick will occur that writes the read value back to the same address due to how
    // the 6502 operates.
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_zp_y(&mut self, mode: &InstructionMode) -> Result<OpState> {
        self.addr_zp_xy(mode, self.y().0)
    }

    // addr_zp_xy implements the details for addr_zp_x and addr_zp_y since they only differ based on the register used.
    // See those functions for arg/return specifics.
    fn addr_zp_xy(&mut self, mode: &InstructionMode, reg: u8) -> Result<OpState> {
        match self.op_tick() {
            Tick::Reset | Tick::Tick1 | Tick::Tick6 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("addr_zp_x invalid op_tick: {:?}", self.op_tick()))
            }
            Tick::Tick2 => {
                // Already read the value but need to bump the PC.
                self.op_addr_mut(u16::from(self.op_val()));
                self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // Read from the ZP addr and then add the register for the real read later.
                _ = self.ram().borrow().read(self.op_addr());
                // Do this as a u8 so it wraps as needed.
                self.op_addr_mut(u16::from((Wrapping(self.op_val()) + Wrapping(reg)).0));
                // For a store we're done since we have the address needed.
                if mode == &InstructionMode::Store {
                    Ok(OpState::Done)
                } else {
                    Ok(OpState::Processing)
                }
            }
            Tick::Tick4 => {
                // Now read from the final address.
                self.op_val_mut(self.ram().borrow().read(self.op_addr()));
                // If we're load we're now done.
                if mode == &InstructionMode::Load {
                    Ok(OpState::Done)
                } else {
                    Ok(OpState::Processing)
                }
            }
            Tick::Tick5 => {
                // On NMOS there's a spurious write here where we replay back
                // what we just read.
                self.ram().borrow_mut().write(self.op_addr(), self.op_val());
                Ok(OpState::Done)
            }
        }
    }

    // perform_branch does all of the main logic for processing a branch which is taken.
    fn perform_branch(&mut self) -> Result<OpState> {
        match self.op_tick() {
            Tick::Reset | Tick::Tick1 | Tick::Tick5 | Tick::Tick6 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!(
                    "perform_branch invalid op_tick: {:?}",
                    self.op_tick()
                ))
            }
            Tick::Tick2 => {
                self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // We only skip if the last instruction didn't. This way a branch always doesn't prevent interrupt processing
                // since on real silicon this is what happens (just a delay in the pipelining).
                if self.skip_interrupt() != SkipInterrupt::PrevSkip {
                    self.skip_interrupt_mut(SkipInterrupt::Skip);
                }
                self.branch_taken()
            }
            Tick::Tick4 => self.branch_taken2(),
        }
    }

    // branch_taken computes the first attempt to take a branch. This may be wrong
    // due to page crossing so it will return Processing or Done to indicate
    // whether to call branch_taken2 or not (call if Processing returns)
    #[allow(clippy::unnecessary_wraps)]
    fn branch_taken(&mut self) -> Result<OpState> {
        // Per http://www.6502.org/tutorials/6502opcodes.html
        // the wrong page is defined as the a different page than
        // the next byte after the jump. i.e. current PC at the moment.

        // Now compute the new PC but possibly wrong page.
        // Stash the old one in p.opAddr so we can use in tick 4 if needed.
        self.op_addr_mut(self.pc());
        self.pc_mut(
            Wrapping(
                (self.pc() & 0xFF00)
                    | u16::from((Wrapping((self.pc() & 0x00FF) as u8) + Wrapping(self.op_val())).0),
            )
            .0,
        ); // It always triggers a bus read of the newly computed PC.
        _ = self.ram().borrow().read(self.pc());
        // Now check the pc against one which didn't truncate for page by sign extending
        // op_val and adding it to op_addr.
        // NOTE: We don't lose the sign here since Wrapping will do the right thing.
        #[allow(
            clippy::cast_sign_loss,
            clippy::cast_lossless,
            clippy::cast_possible_wrap
        )]
        if self.pc() == (Wrapping(self.op_addr()) + Wrapping(self.op_val() as i8 as i16 as u16)).0 {
            return Ok(OpState::Done);
        }
        Ok(OpState::Processing)
    }

    // branch_taken2 is used when fixing up a page boundary issue from a previous
    // branch_taken call.
    #[allow(clippy::unnecessary_wraps)]
    fn branch_taken2(&mut self) -> Result<OpState> {
        // Set the correct PC value if we got here.
        // NOTE: We don't lose the sign here since Wrapping will do the right thing.
        #[allow(
            clippy::cast_sign_loss,
            clippy::cast_lossless,
            clippy::cast_possible_wrap
        )]
        let val = Wrapping(self.op_addr()) + Wrapping(self.op_val() as i8 as i16 as u16);
        self.pc_mut(val.0);
        // Always read the next opcode now.
        _ = self.ram().borrow().read(self.pc());
        Ok(OpState::Done)
    }

    // branch_nop reads the next byte as the branch offset and increments the PC.
    // Used for the 2nd tick when branches aren't taken.
    fn branch_nop(&mut self) -> Result<OpState> {
        match self.op_tick() {
            Tick::Reset
            | Tick::Tick1
            | Tick::Tick3
            | Tick::Tick4
            | Tick::Tick5
            | Tick::Tick6
            | Tick::Tick7
            | Tick::Tick8 => Err(eyre!("branch_nop invalid op_tick: {:?}", self.op_tick())),
            Tick::Tick2 => {
                self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);
                Ok(OpState::Done)
            }
        }
    }

    // pop_stack pops the top value from the stack and adjusts
    // the stack pointer.
    fn pop_stack(&mut self) -> u8 {
        self.s_mut(self.s() + Wrapping(1));
        self.ram()
            .borrow()
            .read(u16::from(self.s().0) + STACK_START)
    }

    // push_stack takes the given 8 bit value and pushes it into the stack and adjusts
    // the stack pointer.
    fn push_stack(&mut self, val: u8) {
        self.ram()
            .borrow_mut()
            .write(u16::from(self.s().0) + STACK_START, val);
        self.s_mut(self.s() - Wrapping(1));
    }

    // adc implements the ADC/SBC opcodes which does add/subtract with carry on A.
    // This sets all associated flags in P. For SBC simply ones-complement op_val
    // before calling.
    // NOTE: For SBC this only works in non BCD mode. sbc() handles BCD directly but
    // otherwise calls adc for binary mode.
    // Always returns Done since this takes one tick and never returns an error.
    fn adc(&mut self) -> Result<OpState> {
        // Pull the carry bit out which thankfully is the low bit so can be
        // used directly.
        let carry = (self.p() & P_CARRY).0;

        // Do BCD mode.
        if (self.p() & P_DECIMAL) != P_NONE {
            // BCD details - http://6502.org/tutorials/decimal_mode.html
            // Also http://nesdev.com/6502_cpu.txt but it has errors
            let mut al =
                Wrapping(self.a().0 & 0x0F) + Wrapping(self.op_val() & 0x0F) + Wrapping(carry);
            // Low nibble fixup
            if al >= Wrapping(0x0A) {
                al = ((al + Wrapping(0x06)) & Wrapping(0x0F)) + Wrapping(0x10);
            }
            let mut sum = Wrapping(u16::from(self.a().0 & 0xF0))
                + Wrapping(u16::from(self.op_val() & 0xF0))
                + Wrapping(u16::from(al.0));
            // High nibble fixup
            if sum >= Wrapping(0xA0) {
                sum += 0x60;
            }
            let res = (sum.0 & 0xFF) as u8;
            let seq = Wrapping(self.a().0 & 0xF0) + Wrapping(self.op_val() & 0xF0) + al;
            let bin = self.a() + Wrapping(self.op_val()) + Wrapping(carry);
            self.overflow_check(self.a().0, self.op_val(), seq.0);
            self.carry_check(sum.0);
            self.negative_check(seq.0);
            self.zero_check(bin.0);
            self.a_mut(Wrapping(res));
            return Ok(OpState::Done);
        }

        // Otherwise do normal binary math.
        let sum = (self.a() + Wrapping(self.op_val()) + Wrapping(carry)).0;
        self.overflow_check(self.a().0, self.op_val(), sum);

        // Yes, could do bit checks here like the hardware but
        // just treating as uint16 math is simpler to code.
        self.carry_check(u16::from(self.a().0) + u16::from(self.op_val()) + u16::from(carry));
        // Now set the accumulator so the other flag checks are against the result.
        self.load_register(Register::A, sum)
    }

    // and implements the AND instruction on the given memory location in op_addr.
    // It then sets all associated flags and adjust cycles as needed.
    // Always returns Done since this takes one tick and never returns an error.
    fn and(&mut self) -> Result<OpState> {
        self.load_register(Register::A, self.a().0 & self.op_val())
    }

    // asl implements the ASL instruction on the given memory location in op_addr.
    // It then sets all associated flags and adjust cycles as needed.
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn asl(&mut self) -> Result<OpState> {
        let new = self.op_val() << 1;
        self.ram().borrow_mut().write(self.op_addr(), new);
        self.carry_check(u16::from(self.op_val()) << 1);
        self.zero_check(new);
        self.negative_check(new);
        Ok(OpState::Done)
    }

    // asl_acc implements the ASL instruction directly on the accumulator.
    // It then sets all associated flags and adjust cycles as needed.
    // Always returns Done since accumulator mode is done on tick 2 and never returns an error.
    fn asl_acc(&mut self) -> Result<OpState> {
        self.carry_check(u16::from(self.a().0) << 1);
        self.load_register(Register::A, self.a().0 << 1)
    }

    // bcc implements the BCC instruction and branches if C is clear.
    // Returns Done when the branch has set the correct PC and/or an error.
    fn bcc(&mut self) -> Result<OpState> {
        if self.p() & P_CARRY == P_NONE {
            self.perform_branch()
        } else {
            self.branch_nop()
        }
    }

    // bcs implements the BCS instruction and branches if C is set.
    // Returns Done when the branch has set the correct PC and/or an error.
    fn bcs(&mut self) -> Result<OpState> {
        if self.p() & P_CARRY == P_NONE {
            self.branch_nop()
        } else {
            self.perform_branch()
        }
    }

    // beq implements the BEQ instruction and branches is Z is set.
    // Returns Done when the branch has set the correct PC and/or an error.
    fn beq(&mut self) -> Result<OpState> {
        if self.p() & P_ZERO == P_NONE {
            self.branch_nop()
        } else {
            self.perform_branch()
        }
    }

    // bit implements the BIT instruction for AND'ing against A
    // and setting N/V/Z based on the value.
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn bit(&mut self) -> Result<OpState> {
        self.zero_check(self.a().0 & self.op_val());
        self.negative_check(self.op_val());
        // Copy V from bit 6
        self.p_mut(self.p() & !P_OVERFLOW);
        if self.op_val() & P_OVERFLOW != P_NONE {
            self.p_mut(self.p() | P_OVERFLOW);
        }

        Ok(OpState::Done)
    }

    // bmi implements the BMI instruction and branches if N is set.
    // Returns Done when the branch has set the correct PC and/or an error.
    fn bmi(&mut self) -> Result<OpState> {
        if self.p() & P_NEGATIVE == P_NONE {
            self.branch_nop()
        } else {
            self.perform_branch()
        }
    }

    // bne implements the BNE instruction and branches is Z is clear.
    // Returns Done when the branch has set the correct PC and/or an error.
    fn bne(&mut self) -> Result<OpState> {
        if self.p() & P_ZERO == P_NONE {
            self.perform_branch()
        } else {
            self.branch_nop()
        }
    }

    // bpl implements the BPL instruction and branches if N is clear.
    // Returns Done when the branch has set the correct PC and/or an error.
    fn bpl(&mut self) -> Result<OpState> {
        if self.p() & P_NEGATIVE == P_NONE {
            self.perform_branch()
        } else {
            self.branch_nop()
        }
    }

    // bvc implements the BVC instruction and branches if V is clear.
    // Returns Done when the branch has set the correct PC and/or an error.
    fn bvc(&mut self) -> Result<OpState> {
        if self.p() & P_OVERFLOW == P_NONE {
            self.perform_branch()
        } else {
            self.branch_nop()
        }
    }

    // bvs implements the BVS instruction and branches if V is set.
    // Returns Done when the branch has set the correct PC and/or an error.
    fn bvs(&mut self) -> Result<OpState> {
        if self.p() & P_OVERFLOW == P_NONE {
            self.branch_nop()
        } else {
            self.perform_branch()
        }
    }

    // brk implements the BRK instruction. This does setup and then calls the
    // interrupt processing handler refenced at IRQ_VECTOR (normally).
    // Returns Done when done and/or errors.
    fn brk(&mut self) -> Result<OpState> {
        // This is the same as an interrupt handler so the vector we call
        // can change on a per tick basis on NMOS. i.e. we might push P with P_B set
        // but go to the NMI vector depending on timing.

        // New PC comes from IRQ_VECTOR unless we've raised an NMI.
        let ret = match self.irq_raised() {
            InterruptStyle::IRQ => self.run_interrupt(IRQ_VECTOR, true)?,
            InterruptStyle::NMI => self.run_interrupt(NMI_VECTOR, true)?,
            InterruptStyle::None => self.run_interrupt(IRQ_VECTOR, false)?,
        };
        // If we're done on this tick eat any pending interrupt since BRK is special.
        if ret == OpState::Done {
            self.irq_raised_mut(InterruptStyle::None);
        }
        Ok(ret)
    }

    // clc implements the CLC instruction clearing the C status bit.
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn clc(&mut self) -> Result<OpState> {
        self.p_mut(self.p() & !P_CARRY);
        Ok(OpState::Done)
    }

    // cld implements the CLD instruction clearing the D status bit.
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn cld(&mut self) -> Result<OpState> {
        self.p_mut(self.p() & !P_DECIMAL);
        Ok(OpState::Done)
    }

    // cli implements the CLI instruction for clearing the I status bit.
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn cli(&mut self) -> Result<OpState> {
        self.p_mut(self.p() & !P_INTERRUPT);
        Ok(OpState::Done)
    }

    // clv implements the CLV instruction clearing the V status bit.
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn clv(&mut self) -> Result<OpState> {
        self.p_mut(self.p() & !P_OVERFLOW);
        Ok(OpState::Done)
    }

    // dec implements the DEC instruction by decrementing the value (op_val) at op_addr.
    // Always returns Done since this takes one tick and never returns an error.
    fn dec(&mut self) -> Result<OpState> {
        self.store_with_flags(self.op_addr(), (Wrapping(self.op_val()) - Wrapping(1)).0)
    }

    // eor implements the EOR instruction which XORs op_val with A.
    // Always returns true since this takes one tick and never returns an error.
    fn eor(&mut self) -> Result<OpState> {
        self.load_register(Register::A, self.a().0 ^ self.op_val())
    }

    // inc implements the INC instruction by incrementing the value (op_val) at op_addr.
    // Always returns Done since this takes one tick and never returns an error.
    fn inc(&mut self) -> Result<OpState> {
        self.store_with_flags(self.op_addr(), (Wrapping(self.op_val()) + Wrapping(1)).0)
    }

    // jmp implements the JMP instruction for jumping to a new address.
    // Doesn't use addressing mode functions since it's technically not
    // a load/rmw/store instruction so doesn't fit exactly.
    // Returns Done when done and/or errors.
    fn jmp(&mut self) -> Result<OpState> {
        match self.op_tick() {
            Tick::Reset
            | Tick::Tick1
            | Tick::Tick4
            | Tick::Tick5
            | Tick::Tick6
            | Tick::Tick7
            | Tick::Tick8 => Err(eyre!("jmp: invalid op_tick: {:?}", self.op_tick())),
            Tick::Tick2 => {
                // We've already read op_val which is the new PCL so increment PC only.
                self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // Read PCH and assemble the PC
                self.op_addr_mut(
                    (u16::from(self.ram().borrow().read(self.pc())) << 8)
                        | u16::from(self.op_val()),
                );
                self.pc_mut(self.op_addr());
                Ok(OpState::Done)
            }
        }
    }

    // jmp_indirect implements the indirect JMP instruction for jumping through a pointer to a new address.
    // Returns Done when the PC is correct. Returns an error on an invalid tick.
    fn jmp_indirect(&mut self) -> Result<OpState> {
        match self.op_tick() {
            Tick::Reset | Tick::Tick1 | Tick::Tick6 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("jmp indirect: invalid op_tick: {:?}", self.op_tick()))
            }
            Tick::Tick2 | Tick::Tick3 => {
                // Same as loading an absolute address
                self.addr_absolute(&InstructionMode::Load)
            }
            Tick::Tick4 => {
                // Read the low byte of the pointer and stash it in op_val if NMOS
                self.op_val_mut(self.ram().borrow().read(self.op_addr()));
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                // Read the high byte. On NMOS this tick reads the wrong address if there was a page wrap.
                let addr = (self.op_addr() & 0xFF00)
                    | u16::from((Wrapping((self.op_addr() & 0x00FF) as u8) + Wrapping(1)).0);
                let val = self.ram().borrow().read(addr);
                self.op_addr_mut((u16::from(val) << 8) | u16::from(self.op_val()));
                self.pc_mut(self.op_addr());
                Ok(OpState::Done)
            }
        }
    }

    // jsr implements the JSR instruction for jumping to a subroutine.
    // Returns Done when done and/or errors.
    fn jsr(&mut self) -> Result<OpState> {
        match self.op_tick() {
            Tick::Reset | Tick::Tick1 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("jsr: invalid op_tick: {:?}", self.op_tick()))
            }
            Tick::Tick2 => {
                // Nothing happens here except to make the PC correct.
                // NOTE: This means the PC pushed below is actually off by one.
                //       RTS handles this by adding one to the popped PC.
                self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // Not 100% sure what happens on this cycle.
                // Per http://nesdev.com/6502_cpu.txt we read the current stack
                // value because there needs to be a tick to make S correct.
                self.s_mut(self.s() - Wrapping(1));
                _ = self.pop_stack();
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                self.push_stack(((self.pc() & 0xFF00) >> 8) as u8);
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                self.push_stack((self.pc() & 0x00FF) as u8);
                Ok(OpState::Processing)
            }
            Tick::Tick6 => {
                self.pc_mut(
                    (u16::from(self.ram().borrow().read(self.pc())) << 8)
                        | u16::from(self.op_val()),
                );
                Ok(OpState::Done)
            }
        }
    }

    // lsr implements the LSR instruction as a logical shift right on op_addr.
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn lsr(&mut self) -> Result<OpState> {
        let val = self.op_val() >> 1;
        self.ram().borrow_mut().write(self.op_addr(), val);
        // Get bit 0 from the original but as a 16 bit value so
        // we can shift it into the carry position.
        self.carry_check(u16::from(self.op_val() & 0x01) << 8);
        self.zero_check(val);
        self.negative_check(val);
        Ok(OpState::Done)
    }

    // lsr_acc implements the LSR instruction as a logical shift right on A.
    // Always returns Done since this takes one tick and never returns an error.
    fn lsr_acc(&mut self) -> Result<OpState> {
        // Get bit 0 from A but in a 16 bit value and then shift it up into
        // the carry position
        self.carry_check(u16::from(self.a().0 & 0x01) << 8);
        self.load_register(Register::A, self.a().0 >> 1)
    }

    // ora implements the ORA instruction which ORs op_val with A.
    // Always returns Done since this takes one tick and never returns an error.
    fn ora(&mut self) -> Result<OpState> {
        self.load_register(Register::A, self.a().0 | self.op_val())
    }

    // push_register is the common logic for pushing A,X,Y onto the stack.
    fn push_register(&mut self, val: u8) -> Result<OpState> {
        match self.op_tick() {
            Tick::Reset
            | Tick::Tick1
            | Tick::Tick4
            | Tick::Tick5
            | Tick::Tick6
            | Tick::Tick7
            | Tick::Tick8 => Err(eyre!("pha: invalid op_tick: {:?}", self.op_tick())),
            Tick::Tick2 => Ok(OpState::Processing),
            Tick::Tick3 => {
                self.push_stack(val);
                Ok(OpState::Done)
            }
        }
    }

    // pha implements the PHA instruction for pushing A onto the stack.
    // Returns Done when done and/or errors.
    fn pha(&mut self) -> Result<OpState> {
        self.push_register(self.a().0)
    }

    // php implements the PHP instruction for pushing P onto the stack.
    // Returns Done when done and/or errors.
    fn php(&mut self) -> Result<OpState> {
        let mut push = self.p();

        // This is always set
        push |= P_S1;

        // PHP always sets B where-as IRQ/NMI don't.
        push |= P_B;
        self.push_register(push.0)
    }

    // pull_register is the common logic for pulling A,X,Y off the stack.
    fn pull_register(&mut self, reg: Register) -> Result<OpState> {
        match self.op_tick() {
            Tick::Reset | Tick::Tick1 | Tick::Tick5 | Tick::Tick6 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("pla: invalid op_tick: {:?}", self.op_tick()))
            }
            Tick::Tick2 => Ok(OpState::Processing),
            Tick::Tick3 => {
                // A read of the current stack happens while the CPU is incrementing S.
                // Since our popStack does both of these together on this cycle it's just
                // a throw away read.
                self.s_mut(self.s() - Wrapping(1));
                _ = self.pop_stack();
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // The real read
                let val = self.pop_stack();
                self.load_register(reg, val)
            }
        }
    }

    // pla implements the PLA instruction for pulling A from the stack.
    // Returns Done when done and/or errors.
    fn pla(&mut self) -> Result<OpState> {
        self.pull_register(Register::A)
    }

    // plp implements the PLP instructions for pulling P from the stack.
    // Returns Done when done and/or errors.
    fn plp(&mut self) -> Result<OpState> {
        let ret = self.pull_register(Register::P)?;
        if ret == OpState::Done {
            // The actual flags register always has S1 set to one.
            self.p_mut(self.p() | P_S1);
            // And the B bit is never set in the register.
            self.p_mut(self.p() & !P_B);
        }
        Ok(ret)
    }

    // rol implements the ROL instruction which does a rotate left on op_addr.
    // It then sets all associated flags and adjust cycles as needed.
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn rol(&mut self) -> Result<OpState> {
        let carry = (self.p() & P_CARRY).0;
        let new = (self.op_val() << 1) | carry;
        self.ram().borrow_mut().write(self.op_addr(), new);
        self.carry_check(u16::from(self.op_val()) << 1);
        self.zero_check(new);
        self.negative_check(new);
        Ok(OpState::Done)
    }

    // rol_acc implements the ROL instruction directly on the accumulator.
    // It then sets all associated flags and adjust cycles as needed.
    // Always returns Done since accumulator mode is done on tick 2 and never returns an error.
    fn rol_acc(&mut self) -> Result<OpState> {
        let carry = (self.p() & P_CARRY).0;
        self.carry_check(u16::from(self.a().0) << 1);
        self.load_register(Register::A, (self.a().0 << 1) | carry)
    }

    // ror implements the ROR instruction for doing a rotate right on the contents
    // of op_addr. Sets flags.
    // Always returns Done since this is one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn ror(&mut self) -> Result<OpState> {
        let carry = (self.p() & P_CARRY).0 << 7;
        let new = (self.op_val() >> 1) | carry;
        self.ram().borrow_mut().write(self.op_addr(), new);
        // Just see if carry is set or not.
        self.carry_check((u16::from(self.op_val()) << 8) & 0x0100);
        self.zero_check(new);
        self.negative_check(new);
        Ok(OpState::Done)
    }

    // ror_acc implements the ROR instruction directly on the accumulator.
    // It then sets all associated flags and adjust cycles as needed.
    // Always returns Done since accumulator mode is done on tick 2 and never returns an error.
    fn ror_acc(&mut self) -> Result<OpState> {
        let carry = (self.p() & P_CARRY).0 << 7;
        // Just see if carry is set or not.
        self.carry_check((u16::from(self.a().0) << 8) & 0x0100);
        self.load_register(Register::A, (self.a().0 >> 1) | carry)
    }

    // rti implements the RTI instruction for returning out of an interrupt handler.
    // Returns Done when done and/or errors.
    fn rti(&mut self) -> Result<OpState> {
        match self.op_tick() {
            Tick::Reset | Tick::Tick1 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("rti: invalid op_tick: {:?}", self.op_tick()))
            }
            Tick::Tick2 => Ok(OpState::Processing),
            Tick::Tick3 => {
                // A read of the current stack happens while the CPU is incrementing S.
                // Since our popStack does both of these together on this cycle it's just
                // a throw away read.
                self.s_mut(self.s() - Wrapping(1));
                _ = self.pop_stack();
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // The real read for P
                let p = self.pop_stack();
                self.p_mut(Flags(p));
                // The actual flags registers always has S1 set to one
                self.p_mut(self.p() | P_S1);
                // And B is never set.
                self.p_mut(self.p() & !P_B);
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                // PCL
                let o = self.pop_stack();
                self.op_val_mut(o);
                Ok(OpState::Processing)
            }
            Tick::Tick6 => {
                // Pop PCH and set PC
                let o = self.pop_stack();
                self.pc_mut((u16::from(o) << 8) | u16::from(self.op_val()));
                Ok(OpState::Done)
            }
        }
    }

    // RTS implements the RTS instruction and pops the PC off the stack
    // for returning from an subroutine.
    // Returns Done when done and/or errors.
    fn rts(&mut self) -> Result<OpState> {
        match self.op_tick() {
            Tick::Reset | Tick::Tick1 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("rts: invalid op_tick: {:?}", self.op_tick()))
            }
            Tick::Tick2 => Ok(OpState::Processing),
            Tick::Tick3 => {
                // A read of the current stack happens while the CPU is incrementing S.
                // Since our popStack does both of these together on this cycle it's just
                // a throw away read.
                self.s_mut(self.s() - Wrapping(1));
                _ = self.pop_stack();
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // PCL
                let o = self.pop_stack();
                self.op_val_mut(o);
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                // PCH
                let o = self.pop_stack();
                self.pc_mut((u16::from(o) << 8) | u16::from(self.op_val()));
                Ok(OpState::Processing)
            }
            Tick::Tick6 => {
                // Read the current PC value then increment PC so we're pointing at the right addr.
                _ = self.ram().borrow().read(self.pc());
                self.pc_mut((Wrapping(self.pc()) + Wrapping(1)).0);
                Ok(OpState::Done)
            }
        }
    }

    // sbc implements the SBC instruction for both binary and BCD modes (if implemented)
    // and sets all associated flags.
    // Always returns Done since this takes one tick and never returns an error.
    fn sbc(&mut self) -> Result<OpState> {
        // Do BCD mode.
        if (self.p() & P_DECIMAL) != P_NONE {
            // Pull the carry bit out which thankfully is the low bit so can be
            // used directly.
            let carry = (self.p() & P_CARRY).0;

            // BCD details - http://6502.org/tutorials/decimal_mode.html
            // Also http://nesdev.com/6502_cpu.txt but it has errors
            // Note: Wraps are ok here and intended as we're doing bit extensions on purpose.
            #[allow(clippy::cast_possible_wrap)]
            let mut al = Wrapping((self.a().0 & 0x0F) as i8)
                - Wrapping((self.op_val() & 0x0F) as i8)
                + Wrapping(carry as i8)
                - Wrapping(1);

            // Low nibble fixup.
            if al < Wrapping(0) {
                al = ((al - Wrapping(0x06)) & Wrapping(0x0F)) - Wrapping(0x10);
            }

            let mut sum;
            sum = Wrapping(i16::from(self.a().0 & 0xF0))
                - Wrapping(i16::from(self.op_val() & 0xF0))
                + Wrapping(i16::from(al.0));
            // High nibble fixup (for all)
            if sum < Wrapping(0x0000) {
                sum -= 0x60;
            }

            // NOTE: We don't lose the sign here BCD doesn't care.
            #[allow(clippy::cast_sign_loss)]
            let res = (sum.0 & 0xFF) as u8;

            // Do normal binary math to set C,N,Z
            let b = self.a() + Wrapping(!self.op_val()) + Wrapping(carry);
            self.overflow_check(self.a().0, !self.op_val(), b.0);

            self.negative_check(b.0);
            self.zero_check(b.0);

            // Yes, could do bit checks here like the hardware but
            // just treating as uint16 math is simpler to code.
            self.carry_check(u16::from(self.a().0) + u16::from(!self.op_val()) + u16::from(carry));
            self.a_mut(Wrapping(res));
            return Ok(OpState::Done);
        }

        // Otherwise binary mode is just ones complement p.opVal and ADC.
        self.op_val_mut(!self.op_val());
        self.adc()
    }

    // sec implements the SEC instruction for setting the carry bit.
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn sec(&mut self) -> Result<OpState> {
        self.p_mut(self.p() | P_CARRY);
        Ok(OpState::Done)
    }

    // sed implements the SED instruction for setting the D status bit.
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn sed(&mut self) -> Result<OpState> {
        self.p_mut(self.p() | P_DECIMAL);
        Ok(OpState::Done)
    }

    // sei implements the SEI instruction for setting the I status bit.
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn sei(&mut self) -> Result<OpState> {
        self.p_mut(self.p() | P_INTERRUPT);
        Ok(OpState::Done)
    }
}

macro_rules! cpu_internal {
    () => {
        // debug_hook returns the optional debug function callback.
        fn debug_hook(&self) -> Option<&'a dyn Fn() -> (Rc<RefCell<CPUState>>, bool)> {
            self.debug
        }

        // state returns the current CPU state.
        fn state(&self) -> State {
            self.state
        }
        // state_mut sets the current CPU state.
        fn state_mut(&mut self, new: State) {
            self.state = new;
        }

        // a returns the contents of the A register.
        fn a(&self) -> Wrapping<u8> {
            self.a
        }
        // a_mut sets the contents of the A register.
        fn a_mut(&mut self, new: Wrapping<u8>) {
            self.a = new;
        }

        // x returns the contents of the X register.
        fn x(&self) -> Wrapping<u8> {
            self.x
        }
        // x_mut sets the contents of the X register.
        fn x_mut(&mut self, new: Wrapping<u8>) {
            self.x = new;
        }

        // y returns the contents of the Y register.
        fn y(&self) -> Wrapping<u8> {
            self.y
        }
        // y_mut sets the contents of the Y register.
        fn y_mut(&mut self, new: Wrapping<u8>) {
            self.y = new;
        }

        // s returns the contents of the S register.
        fn s(&self) -> Wrapping<u8> {
            self.s
        }
        // s_mut sets the contents of the S register.
        fn s_mut(&mut self, new: Wrapping<u8>) {
            self.s = new;
        }

        // p returns the current Flags from the P register.
        fn p(&self) -> Flags {
            self.p
        }
        // p_mut sets the P register to the given Flags.
        fn p_mut(&mut self, new: Flags) {
            self.p = new;
        }

        // clocks returns the number of clock cycles since startup.
        fn clocks(&self) -> usize {
            self.clocks
        }

        // op_val returns the internal op_val register.
        fn op_val(&self) -> u8 {
            self.op_val
        }
        // op_val_mut sets the internal op_val register.
        fn op_val_mut(&mut self, new: u8) {
            self.op_val = new;
        }

        // op_addr returns the internal op_addr register.
        fn op_addr(&self) -> u16 {
            self.op_addr
        }
        // op_addr_mut sets the internal op_addr register.
        fn op_addr_mut(&mut self, new: u16) {
            self.op_addr = new;
        }

        // op_raw returns the current opcode byte.
        fn op_raw(&self) -> u8 {
            self.op_raw
        }

        // op returns the current decoded Operation.
        fn op(&self) -> Operation {
            self.op
        }

        // addr_done returns the internal addr_done state.
        fn addr_done(&self) -> OpState {
            self.addr_done
        }
        // addr_done_mut sets the internal addr_done state.
        fn addr_done_mut(&mut self, new: OpState) {
            self.addr_done = new;
        }

        // op_tick returns the current Tick value for the operation in progress.
        fn op_tick(&self) -> Tick {
            self.op_tick
        }

        // skip_interrupt returns the current SkipInterrupt state.
        fn skip_interrupt(&self) -> SkipInterrupt {
            self.skip_interrupt
        }
        // skip_interrupt_mut sets the SkipInterrupt state.
        fn skip_interrupt_mut(&mut self, new: SkipInterrupt) {
            self.skip_interrupt = new;
        }

        // irq_raised returns the current InterruptStyle state.
        fn irq_raised(&self) -> InterruptStyle {
            self.irq_raised
        }
        // irq_raised_mut sets the InterruptStyle state.
        fn irq_raised_mut(&mut self, new: InterruptStyle) {
            self.irq_raised = new;
        }
    };
}

impl<'a> CPUInternal<'a> for CPU6502<'a> {
    cpu_internal!();
}
impl<'a> CPUInternal<'a> for CPU6510<'a> {
    cpu_internal!();
}
impl<'a> CPUInternal<'a> for CPURicoh<'a> {
    cpu_internal!();

    // adc implements the ADC/SBC opcodes which does add/subtract with carry on A.
    // This sets all associated flags in P. For SBC simply ones-complement op_val
    // before calling. For Richo this means no BCD.
    // Always returns Done since this takes one tick and never returns an error.
    fn adc(&mut self) -> Result<OpState> {
        // Pull the carry bit out which thankfully is the low bit so can be
        // used directly.
        let carry = (self.p & P_CARRY).0;

        // Do normal binary math.
        let sum = (self.a + Wrapping(self.op_val) + Wrapping(carry)).0;
        self.overflow_check(self.a.0, self.op_val, sum);

        // Yes, could do bit checks here like the hardware but
        // just treating as uint16 math is simpler to code.
        self.carry_check(u16::from(self.a.0) + u16::from(self.op_val) + u16::from(carry));
        // Now set the accumulator so the other flag checks are against the result.
        self.load_register(Register::A, sum)
    }

    // sbc implements the SBC instruction for binary math. This is just an
    // inversion and calls ADC. As this is Ricoh there is no BCD support.
    // Sets all associated flags.
    // Always returns Done since this takes one tick and never returns an error.
    fn sbc(&mut self) -> Result<OpState> {
        // Binary mode is just ones complement p.opVal and ADC.
        self.op_val = !self.op_val;
        self.adc()
    }
}

impl<'a> CPUInternal<'a> for CPU65C02<'a> {
    cpu_internal!();

    // load_instruction abstracts all load instruction opcodes. The address mode function is
    // used to get the proper values loaded into op_addr and op_val.
    // Then on the same tick this is done op_func is called to load the appropriate register.
    // Returns OpState::Done when complete and/or any error.
    fn load_instruction(
        &mut self,
        address_mode: fn(&mut Self, &InstructionMode) -> Result<OpState>,
        op_func: fn(&mut Self) -> Result<OpState>,
    ) -> Result<OpState> {
        // Account for ADC/SBC in D mode on CMOS needing to do one more cycle.
        let mut skip_done = false;
        if self.addr_done != OpState::Done {
            self.addr_done = address_mode(self, &InstructionMode::Load)?;
            if self.addr_done == OpState::Done {
                skip_done = true;
            }
        }
        match self.addr_done {
            OpState::Processing => Ok(OpState::Processing),
            OpState::Done => {
                if skip_done
                    && self.p & P_DECIMAL != P_NONE
                    && (self.op.op == Opcode::ADC || self.op.op == Opcode::SBC)
                {
                    // Do another read cycle to account for SBC/ADC needing this
                    self.op_val = self.ram.borrow().read(self.op_addr);
                    Ok(OpState::Processing)
                } else {
                    op_func(self)
                }
            }
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
                self.op_val = self.ram.borrow().read(self.pc.0);
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
                self.op_val = self.ram.borrow().read(self.op_addr);
                match mode {
                    // For a load we're now done.
                    &InstructionMode::Load => Ok(OpState::Done),
                    _ => Ok(OpState::Processing),
                }
            }
            Tick::Tick5 => {
                // On NMOS there's a spurious write here where we replay back
                // what we just read. On CMOS instead it's a spurious read.
                self.ram.borrow().read(self.op_addr);
                Ok(OpState::Done)
            }
        }
    }

    // addr_absolute_xy implements the details for addr_absolute_x and addr_absolute_y since
    // they only differ based on the register used.
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
                self.op_val = self.ram.borrow().read(self.pc.0);
                self.op_addr |= u16::from(self.op_val) << 8;
                // Add reg but do it in a way which won't page wrap (if needed).
                let a = (self.op_addr & 0xFF00)
                    | u16::from((Wrapping((self.op_addr & 0x00FF) as u8) + Wrapping(reg)).0);
                self.op_val = 0;
                if a != (Wrapping(self.op_addr) + Wrapping(u16::from(reg))).0 {
                    // Signal for next phase fixup is needed
                    self.op_val = 1;
                }
                self.op_addr = a;
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // Check old opVal to see if it's non-zero. If so it means the reg addition
                // crosses a page boundary and we'll have to fixup otherwise this is fine.
                // For a load operation that means another tick to read the correct address.
                // For RMW it doesn't matter (we always do the extra tick).
                // For Store we're done. Just fixup p.opAddr so the return value is correct.

                // If we didn't make the wrong address and load/storing we're good.
                // For RMW we still read here but do continue to the next tick.
                if self.op_val == 0 {
                    // Advance the PC since we didn't earlier as down below CMOS
                    // can reread this value.
                    self.pc += 1;
                    self.op_val = self.ram.borrow().read(self.op_addr);
                    if mode == &InstructionMode::Rmw {
                        return Ok(OpState::Processing);
                    }
                    return Ok(OpState::Done);
                }

                // Every tick requires a bus cycle so while we're fixing up
                // we have to read something. CMOS rereads the last PC+2 value.
                // Leave op_val as it was so the next tick can tell we had to
                // do a fixup (for RMW since it always gets there).
                _ = self.ram.borrow().read(self.pc.0);
                self.pc += 1;

                // We computed the wrong addr before so fix it now by page wrapping.
                self.op_addr = (Wrapping(self.op_addr) + Wrapping(0x0100)).0;

                // Stores are done and ready to write on their next cycle since
                // the addr is correct now.
                if mode == &InstructionMode::Store {
                    return Ok(OpState::Done);
                }

                // Everything else runs another tick at this point
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                let t = self.op_val;
                // So get the correct value in now for both loads (extra cycle for fixup)
                // and RMW (which always gets here minimum).
                self.op_val = self.ram.borrow().read(self.op_addr);

                // For a load or a RMW that didn't overflow we're done. Unless...it's
                // a INC/DEC in which case it always goes the extra cycle per
                // http://www.6502.org/tutorials/65c02opcodes.html
                // Otherwise RMW overflow always advances as does non CMOS RMW always.
                if mode == &InstructionMode::Load
                    || (t == 0 && self.op.op != Opcode::INC && self.op.op != Opcode::DEC)
                {
                    return Ok(OpState::Done);
                }
                Ok(OpState::Processing)
            }
            Tick::Tick6 => {
                // On NMOS there's a spurious write here where we replay back
                // what we just read. On CMOS instead it's a spurious read.
                // The WDC datasheet says this is AA+X+1 which
                // I doubt actually. Everything else just seems to think it
                // rereads the same addr twice.
                _ = self.ram.borrow().read(self.op_addr);
                Ok(OpState::Done)
            }
        }
    }

    // addr_indirect_x implements Zero page indirect plus X mode - (d,x)
    // returning the value in op_val and the address read in op_addr (so RW operations can do things without having to
    // reread memory incorrectly to compute a storage address).
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_indirect_x(&mut self, mode: &InstructionMode) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 | Tick::Tick7 | Tick::Tick8 => {
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
                self.ram.borrow().read(self.op_addr);
                // Does this as a Wrapping so it wraps as needed since it stays in ZP.
                self.op_addr = u16::from((Wrapping(self.op_val) + self.x).0);
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // Read effective addr low byte
                self.op_val = self.ram.borrow().read(self.op_addr);
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
                    (u16::from(self.ram.borrow().read(self.op_addr)) << 8) | u16::from(self.op_val);
                match mode {
                    // For a store we're done as op_addr now contains the destination address.
                    InstructionMode::Store => Ok(OpState::Done),
                    _ => Ok(OpState::Processing),
                }
            }
            Tick::Tick6 => {
                self.op_val = self.ram.borrow().read(self.op_addr);
                // We're done as we've loaded the value and there are no
                // RMW instructions for CMOS.
                Ok(OpState::Done)
            }
        }
    }

    // addr_indirect_y implements Zero page indirect plus Y mode - (d),y
    // returning the value in op_val and the address read in op_addr (so RW operations can do things without having to
    // reread memory incorrectly to compute a storage address).
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_indirect_y(&mut self, mode: &InstructionMode) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 | Tick::Tick7 | Tick::Tick8 => {
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
                self.op_val = self.ram.borrow().read(self.op_addr);
                // Setup op_addr for next read and handle ZP wrapping.
                #[allow(clippy::cast_possible_truncation)]
                let a = u16::from((Wrapping(self.op_addr as u8) + Wrapping(1)).0);
                self.op_addr = a;
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // Compute effective address and then add Y to it (possibly wrongly).
                self.op_addr =
                    (u16::from(self.ram.borrow().read(self.op_addr)) << 8) | u16::from(self.op_val);
                // Add Y but do it in a way which won't page wrap (if needed).
                #[allow(clippy::cast_possible_truncation)]
                let a =
                    (self.op_addr & 0xFF00) | u16::from((Wrapping(self.op_addr as u8) + self.y).0);
                self.op_val = 0;
                if a != (Wrapping(self.op_addr) + Wrapping(u16::from(self.y.0))).0 {
                    // Signal for next phase we got it wrong.
                    self.op_val = 1;
                }
                self.op_addr = a;
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                // Save op_val so we know if this needed fixing.
                let t = self.op_val;
                if t == 0 {
                    // Read the right thing.
                    self.op_val = self.ram.borrow().read(self.op_addr);
                } else {
                    // CMOS doesn't spurious read the wrong op_addr. It just rereads the last PC.
                    _ = self.ram.borrow().read((self.pc - Wrapping(1)).0);
                }

                // Check old opVal to see if it's non-zero. If so it means the Y addition
                // crosses a page boundary and we'll have to fixup.
                // For a load operation that means another tick to read the correct
                // address.
                // For Store we're done. Just fixup p.opAddr so the return value is correct.
                let mut done = Ok(OpState::Done);
                if t != 0 {
                    self.op_addr = (Wrapping(self.op_addr) + Wrapping(0x0100)).0;
                    if mode == &InstructionMode::Load {
                        done = Ok(OpState::Processing);
                    }
                }
                done
            }
            Tick::Tick6 => {
                // Optional (on load) in case adding Y went past a page boundary.
                self.op_val = self.ram.borrow().read(self.op_addr);
                Ok(OpState::Done)
            }
        }
    }

    // addr_zp implements Zero page mode - d
    // returning the value in op_val and the address read in op_addr (so RW operations can do things without having to
    // reread memory incorrectly to compute a storage address).
    // If mode is RMW then another tick will occur that writes the read value back to the same address due to how
    // the 6502 operates. In the CMOS case this is a 2nd read.
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
                self.op_val = self.ram.borrow().read(self.op_addr);
                match mode {
                    // For a load we're now done since the value is loaded.
                    &InstructionMode::Load => Ok(OpState::Done),
                    _ => Ok(OpState::Processing),
                }
            }
            Tick::Tick4 => {
                // On NMOS there's a spurious write here where we replay back
                // what we just read. On CMOS instead it's a spurious read.
                self.ram.borrow().read(self.op_addr);
                Ok(OpState::Done)
            }
        }
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
                _ = self.ram.borrow().read(self.op_addr);
                // Do this as a u8 so it wraps as needed.
                self.op_addr = u16::from((Wrapping(self.op_val) + Wrapping(reg)).0);
                // For a store we're done since we have the address needed.
                if mode == &InstructionMode::Store {
                    Ok(OpState::Done)
                } else {
                    Ok(OpState::Processing)
                }
            }
            Tick::Tick4 => {
                // Now read from the final address.
                self.op_val = self.ram.borrow().read(self.op_addr);
                // If we're load we're now done.
                if mode == &InstructionMode::Load {
                    Ok(OpState::Done)
                } else {
                    Ok(OpState::Processing)
                }
            }
            Tick::Tick5 => {
                // On NMOS there's a spurious write here where we replay back
                // what we just read. On CMOS instead it's a spurious read.
                self.ram.borrow().read(self.op_addr);
                Ok(OpState::Done)
            }
        }
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
                // S1 is always set
                push |= P_S1;
                // B always set unless this triggered due to IRQ
                push |= P_B;
                if irq {
                    push &= !P_B;
                }
                self.push_stack(push.0);
                // Now set P after we've pushed.

                // CMOS turns off D always
                self.p &= !P_DECIMAL;

                // For BRK/IRQ we set I. NMI does not.
                if self.irq_raised != InterruptStyle::NMI {
                    self.p |= P_INTERRUPT;
                }
                Ok(OpState::Processing)
            }
            Tick::Tick6 => {
                self.op_val = self.ram.borrow().read(vec);
                Ok(OpState::Processing)
            }
            Tick::Tick7 => {
                // Compute the new PC from the 2nd vector component and the previous val read.
                self.pc = Wrapping(
                    (u16::from(self.ram.borrow().read(vec + 1)) << 8) | u16::from(self.op_val),
                );

                // If we didn't previously skip an interrupt from processing make sure we execute the first instruction of
                // a handler before firing again.
                if irq && self.skip_interrupt != SkipInterrupt::PrevSkip {
                    self.skip_interrupt = SkipInterrupt::Skip;
                }
                Ok(OpState::Done)
            }
        }
    }

    // adc implements the ADC/SBC opcodes which does add/subtract with carry on A.
    // This sets all associated flags in P. For SBC simply ones-complement op_val
    // before calling.
    // NOTE: SBC this only works in non BCD mode. sbc() handles this directly.
    // Always returns Done since this takes one tick and never returns an error.
    fn adc(&mut self) -> Result<OpState> {
        // Pull the carry bit out which thankfully is the low bit so can be
        // used directly.
        let carry = (self.p & P_CARRY).0;

        // Do BCD.
        if (self.p & P_DECIMAL) != P_NONE {
            // BCD details - http://6502.org/tutorials/decimal_mode.html
            // Also http://nesdev.com/6502_cpu.txt but it has errors
            let mut al = Wrapping(self.a.0 & 0x0F) + Wrapping(self.op_val & 0x0F) + Wrapping(carry);
            // Low nibble fixup
            if al >= Wrapping(0x0A) {
                al = ((al + Wrapping(0x06)) & Wrapping(0x0F)) + Wrapping(0x10);
            }
            let mut sum = Wrapping(u16::from(self.a.0 & 0xF0))
                + Wrapping(u16::from(self.op_val & 0xF0))
                + Wrapping(u16::from(al.0));
            // High nibble fixup
            if sum >= Wrapping(0xA0) {
                sum += 0x60;
            }
            let res = (sum.0 & 0xFF) as u8;
            let seq = Wrapping(self.a.0 & 0xF0) + Wrapping(self.op_val & 0xF0) + al;
            self.overflow_check(self.a.0, self.op_val, seq.0);
            self.carry_check(sum.0);
            // Do the correct checks for CMOS.
            self.negative_check(res);
            self.zero_check(res);
            self.a = Wrapping(res);
            return Ok(OpState::Done);
        }

        // Otherwise do normal binary math.
        let sum = (self.a + Wrapping(self.op_val) + Wrapping(carry)).0;
        self.overflow_check(self.a.0, self.op_val, sum);

        // Yes, could do bit checks here like the hardware but
        // just treating as uint16 math is simpler to code.
        self.carry_check(u16::from(self.a.0) + u16::from(self.op_val) + u16::from(carry));
        // Now set the accumulator so the other flag checks are against the result.
        self.load_register(Register::A, sum)
    }

    // bit implements the BIT instruction for AND'ing against A
    // and setting N/V/Z based on the value.
    // In immediate mode (CMOS only) only Z is set, not N/V.
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn bit(&mut self) -> Result<OpState> {
        self.zero_check(self.a.0 & self.op_val);
        if self.op.mode != AddressMode::Immediate {
            self.negative_check(self.op_val);
            // Copy V from bit 6
            self.p &= !P_OVERFLOW;
            if self.op_val & P_OVERFLOW != P_NONE {
                self.p |= P_OVERFLOW;
            }
        }
        Ok(OpState::Done)
    }

    // brk implements the BRK instruction. This does setup and then calls the
    // interrupt processing handler refenced at IRQ_VECTOR (normally).
    // Returns Done when done and/or errors.
    fn brk(&mut self) -> Result<OpState> {
        // This is the same as an interrupt handler so the vector we call
        // can change on a per tick basis on NMOS. i.e. we might push P with P_B set
        // but go to the NMI vector depending on timing.
        // CMOS doesn't do this. It will always run BRK to completion first.
        self.run_interrupt(IRQ_VECTOR, false)
    }

    // jmp_indirect implements the indirect JMP instruction for jumping through a pointer to a new address.
    // Returns Done when the PC is correct. Returns an error on an invalid tick.
    fn jmp_indirect(&mut self) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("jmp indirect: invalid op_tick: {:?}", self.op_tick))
            }
            Tick::Tick2 | Tick::Tick3 => {
                // Same as loading an absolute address
                self.addr_absolute(&InstructionMode::Load)
            }
            Tick::Tick4 => {
                // Read the low byte of the pointer and stash it in op_val if NMOS
                // For CMOS we simply reread PC+2
                self.op_val = self.ram.borrow().read((self.pc - Wrapping(1)).0);
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                // CMOS always reads the right low byte as compared to NMOS.
                self.op_val = self.ram.borrow().read(self.op_addr);
                self.op_addr = (Wrapping(self.op_addr) + Wrapping(1)).0;
                Ok(OpState::Processing)
            }
            Tick::Tick6 => {
                // CMOS always takes this tick to correct the final addr.
                let val = self.ram.borrow().read(self.op_addr);
                self.op_addr = (u16::from(val) << 8) | u16::from(self.op_val);
                self.pc = Wrapping(self.op_addr);
                Ok(OpState::Done)
            }
        }
    }

    // sbc implements the SBC instruction for both binary and BCD modes.
    // and sets all associated flags.
    // Always returns Done since this takes one tick and never returns an error.
    fn sbc(&mut self) -> Result<OpState> {
        // Do BCD
        if (self.p & P_DECIMAL) != P_NONE {
            // Pull the carry bit out which thankfully is the low bit so can be
            // used directly.
            let carry = (self.p & P_CARRY).0;

            // BCD details - http://6502.org/tutorials/decimal_mode.html
            // Also http://nesdev.com/6502_cpu.txt but it has errors
            // Note: Wraps are ok here and intended as we're doing bit extensions on purpose.
            #[allow(clippy::cast_possible_wrap)]
            let al = Wrapping((self.a.0 & 0x0F) as i8) - Wrapping((self.op_val & 0x0F) as i8)
                + Wrapping(carry as i8)
                - Wrapping(1);

            let mut sum = Wrapping(i16::from(self.a.0)) - Wrapping(i16::from(self.op_val))
                + Wrapping(i16::from(carry))
                - Wrapping(1);
            // High nibble fixup
            if sum < Wrapping(0x0000) {
                sum -= 0x60;
            }
            // Fixup low nibble
            if al < Wrapping(0x0000) {
                sum -= 0x06;
            }

            // NOTE: We don't lose the sign here BCD doesn't care.
            #[allow(clippy::cast_sign_loss)]
            let res = (sum.0 & 0xFF) as u8;

            // Do normal binary math to set C,N,Z
            let b = self.a + Wrapping(!self.op_val) + Wrapping(carry);
            self.overflow_check(self.a.0, !self.op_val, b.0);

            // CMOS gets these right.
            self.negative_check(res);
            self.zero_check(res);

            // Yes, could do bit checks here like the hardware but
            // just treating as uint16 math is simpler to code.
            self.carry_check(u16::from(self.a.0) + u16::from(!self.op_val) + u16::from(carry));
            self.a = Wrapping(res);
            return Ok(OpState::Done);
        }

        // Otherwise binary mode is just ones complement p.opVal and ADC.
        self.op_val = !self.op_val;
        self.adc()
    }
}

macro_rules! cpu_nmos_power_reset {
    () => {
        /// `power_on` will reset the CPU to power on state which isn't well defined.
        /// Registers are random and stack is at random (though visual 6502 claims it's 0xFD
        /// due to a push P/PC in reset which gets run at power on).
        /// P is cleared with interrupts disabled and decimal mode random (for NMOS versions).
        /// The starting PC value is loaded from the reset vector.
        ///
        /// # Errors
        /// If reset has any issues an error will be returned.
        fn power_on(&mut self) -> Result<()> {
            if self.state != State::Off {
                return Err(eyre!("cannot power on except from an off state"));
            }

            let mut rng = rand::thread_rng();

            // This is always set and clears the rest.
            self.p = P_S1;

            // Randomize decimal mode
            if rng.gen::<f64>() > 0.5 {
                self.p |= P_DECIMAL;
            }

            // Randomize register contents
            self.a = rng.gen();
            self.x = rng.gen();
            self.y = rng.gen();
            self.s = rng.gen();

            self.state = State::On;

            // Use reset to get everything else done.
            loop {
                match self.reset() {
                    Ok(OpState::Done) => break,
                    Ok(OpState::Processing) => continue,
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
        fn reset(&mut self) -> Result<OpState> {
            if self.state == State::Off {
                return Err(eyre!("power_on not called before calling reset!"));
            }

            // If we haven't started a reset sequence start it now.
            if self.state != State::Reset {
                self.state = State::Reset;
                self.op_tick = Tick::Reset;
                self.reset_tick = Tick::Tick1;
            }
            self.op_tick = self.op_tick.next();
            self.debug();
            self.clocks += 1;

            match self.reset_tick {
                Tick::Tick1 | Tick::Tick2 => {
                    // Burn off 2 clocks internally to reset before we start processing.
                    // Technically this runs the next 2 sequences of the current opcode.
                    // We don't bother emulating that and instead just reread the
                    // current PC
                    _ = self.ram.borrow().read(self.pc.0);

                    self.reset_tick = self.reset_tick.next();

                    // Leave op_tick in reset mode so once we're done here it'll match below
                    // as Tick1.
                    self.op_tick = Tick::Reset;

                    Ok(OpState::Processing)
                }
                Tick::Tick3 => {
                    match self.op_tick {
                        Tick::Tick1 => {
                            // Standard first tick reads current PC value which normally
                            // is the opcode but we discard.
                            self.ram.borrow().read(self.pc.0);
                            self.pc += 1;

                            // Reset our other internal state

                            // If we were halted before, clear that out.
                            self.halt_opcode = 0x00;
                            self.halt_pc = 0x0000;

                            // The stack ends up at 0xFD which implies it gets set to 0x00 now
                            // as we pull 3 bytes off the stack in the end.
                            self.s = Wrapping(0x00);
                            Ok(OpState::Processing)
                        }
                        Tick::Tick2 => {
                            // Read another throw away value which is normally the opval but
                            // discarded as well.
                            self.ram.borrow().read(self.pc.0);
                            self.pc += 1;
                            Ok(OpState::Processing)
                        }
                        Tick::Tick3 | Tick::Tick4 => {
                            // Tick3: Simulate pushing P onto stack but reset holds R/W high so we don't write.
                            // Tick4: Simulate writing low byte of PC onto stack. Same rules as Tick3.
                            // These reads go nowhere (technically they end up in internal regs but since that's
                            // not visible externally, who cares?).
                            let addr: u16 = STACK_START + u16::from(self.s.0);
                            self.ram.borrow().read(addr);
                            self.s -= 1;
                            Ok(OpState::Processing)
                        }
                        Tick::Tick5 => {
                            // Final write to stack (PC high) but actual read due to being in reset.
                            let addr: u16 = STACK_START + u16::from(self.s.0);
                            self.ram.borrow().read(addr);
                            self.s -= 1;

                            // Disable interrupts
                            self.p |= P_INTERRUPT;

                            // On NMOS D is random after reset.
                            let mut rng = rand::thread_rng();
                            self.p &= !P_DECIMAL;

                            if rng.gen::<f64>() > 0.5 {
                                self.p |= P_DECIMAL;
                            }
                            Ok(OpState::Processing)
                        }
                        Tick::Tick6 => {
                            // Load PCL from reset vector
                            self.op_val = self.ram.borrow().read(RESET_VECTOR);
                            Ok(OpState::Processing)
                        }
                        Tick::Tick7 => {
                            // Load PCH from reset vector and go back into normal operations.
                            self.pc = Wrapping(
                                u16::from(self.ram.borrow().read(RESET_VECTOR + 1)) << 8
                                    | u16::from(self.op_val),
                            );
                            self.reset_tick = Tick::Reset;
                            self.op_tick = Tick::Reset;
                            self.state = State::Running;
                            Ok(OpState::Done)
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
    };
}

macro_rules! cpu_impl {
    () => {
        /// Use this to enable or disable state based debugging dynamically.
        fn set_debug(&mut self, d: Option<&'a dyn Fn() -> (Rc<RefCell<CPUState>>, bool)>) {
            self.debug = d;
        }

        /// debug will emit a filled in value when called. If called from `tick` this
        /// will only happen at the start of an opcode.
        /// If RDY is asserted it will simply repeat the previous state if at Tick1.
        fn debug(&self) {
            if let Some(d) = self.debug {
                let (ref_state, full) = d();
                let mut state = ref_state.borrow_mut();
                state.state = self.state;
                state.a = self.a.0;
                state.x = self.x.0;
                state.y = self.y.0;
                state.s = self.s.0;
                state.p = self.p;
                state.pc = self.pc.0;
                state.clocks = self.clocks;
                state.op_val = self.op_val;
                state.op_addr = self.op_addr;
                state.op_tick = self.op_tick;

                let ram = self.ram.borrow();
                if full {
                    // Do a full copy. This is expensive for every instruction.
                    ram.ram(&mut state.ram);
                } else {
                    // We don't need to memcpy 64k. We just need
                    // the 3 possible instructions bytes at PC.
                    state.ram[usize::from(self.pc.0)] = ram.read(self.pc.0);
                    // These could wrap around so make sure we don't go out of range.
                    let pc1 = (self.pc + Wrapping(1)).0;
                    state.ram[usize::from(pc1)] = ram.read(pc1);
                    let pc2 = (self.pc + Wrapping(2)).0;
                    state.ram[usize::from(pc2)] = ram.read(pc2);
                }
            }
        }

        /// ram returns a reference to the Memory implementation.
        fn ram(&self) -> Rc<RefCell<Box<dyn Memory>>> {
            self.ram.clone()
        }

        // pc returns the current PC value.
        fn pc(&self) -> u16 {
            self.pc.0
        }
        // pc_muts sets PC to the given address.
        fn pc_mut(&mut self, new: u16) {
            self.pc = Wrapping(new);
        }
    };
}

impl<'a> CPU<'a> for CPU6502<'a> {
    cpu_impl!();
    cpu_nmos_power_reset!();
}

impl<'a> CPU<'a> for CPU6510<'a> {
    cpu_impl!();
    cpu_nmos_power_reset!();
}

// The Richo and CMOS while technically very different actually share the same
// power_on/reset functions since the main difference from NMOS is a defined
// D state (CMOS sets it to off and Richo disables it so the same effect).
macro_rules! cpu_cmos_ricoh_power_reset {
    () => {
        /// `power_on` will reset the CPU to power on state which isn't well defined.
        /// Registers are random and stack is at random (though visual 6502 claims it's 0xFD
        /// due to a push P/PC in reset which gets run at power on).
        /// P is cleared with interrupts disabled and decimal mode random (for NMOS versions).
        /// The starting PC value is loaded from the reset vector.
        ///
        /// # Errors
        /// If reset has any issues an error will be returned.
        fn power_on(&mut self) -> Result<()> {
            if self.state != State::Off {
                return Err(eyre!("cannot power on except from an off state"));
            }

            let mut rng = rand::thread_rng();

            // This is always set and clears the rest.
            // This includes D which is always off.
            self.p = P_S1;

            // Randomize register contents
            self.a = rng.gen();
            self.x = rng.gen();
            self.y = rng.gen();
            self.s = rng.gen();

            self.state = State::On;

            // Use reset to get everything else done.
            loop {
                match self.reset() {
                    Ok(OpState::Done) => break,
                    Ok(OpState::Processing) => continue,
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
        fn reset(&mut self) -> Result<OpState> {
            if self.state == State::Off {
                return Err(eyre!("power_on not called before calling reset!"));
            }

            // If we haven't started a reset sequence start it now.
            if self.state != State::Reset {
                self.state = State::Reset;
                self.op_tick = Tick::Reset;
                self.reset_tick = Tick::Tick1;
            }
            self.op_tick = self.op_tick.next();
            self.debug();
            self.clocks += 1;

            match self.reset_tick {
                Tick::Tick1 | Tick::Tick2 => {
                    // Burn off 2 clocks internally to reset before we start processing.
                    // Technically this runs the next 2 sequences of the current opcode.
                    // We don't bother emulating that and instead just reread the
                    // current PC
                    _ = self.ram.borrow().read(self.pc.0);

                    self.reset_tick = self.reset_tick.next();

                    // Leave op_tick in reset mode so once we're done here it'll match below
                    // as Tick1.
                    self.op_tick = Tick::Reset;

                    Ok(OpState::Processing)
                }
                Tick::Tick3 => {
                    match self.op_tick {
                        Tick::Tick1 => {
                            // Standard first tick reads current PC value which normally
                            // is the opcode but we discard.
                            self.ram.borrow().read(self.pc.0);
                            self.pc += 1;

                            // Reset our other internal state

                            // If we were halted before, clear that out.
                            self.halt_opcode = 0x00;
                            self.halt_pc = 0x0000;

                            // The stack ends up at 0xFD which implies it gets set to 0x00 now
                            // as we pull 3 bytes off the stack in the end.
                            self.s = Wrapping(0x00);
                            Ok(OpState::Processing)
                        }
                        Tick::Tick2 => {
                            // Read another throw away value which is normally the opval but
                            // discarded as well.
                            self.ram.borrow().read(self.pc.0);
                            self.pc += 1;
                            Ok(OpState::Processing)
                        }
                        Tick::Tick3 | Tick::Tick4 => {
                            // Tick3: Simulate pushing P onto stack but reset holds R/W high so we don't write.
                            // Tick4: Simulate writing low byte of PC onto stack. Same rules as Tick3.
                            // These reads go nowhere (technically they end up in internal regs but since that's
                            // not visible externally, who cares?).
                            let addr: u16 = STACK_START + u16::from(self.s.0);
                            self.ram.borrow().read(addr);
                            self.s -= 1;
                            Ok(OpState::Processing)
                        }
                        Tick::Tick5 => {
                            // Final write to stack (PC high) but actual read due to being in reset.
                            let addr: u16 = STACK_START + u16::from(self.s.0);
                            self.ram.borrow().read(addr);
                            self.s -= 1;

                            // Disable interrupts and make sure decimal is off
                            self.p |= P_INTERRUPT;
                            self.p &= !P_DECIMAL;
                            Ok(OpState::Processing)
                        }
                        Tick::Tick6 => {
                            // Load PCL from reset vector
                            self.op_val = self.ram.borrow().read(RESET_VECTOR);
                            Ok(OpState::Processing)
                        }
                        Tick::Tick7 => {
                            // Load PCH from reset vector and go back into normal operations.
                            self.pc = Wrapping(
                                u16::from(self.ram.borrow().read(RESET_VECTOR + 1)) << 8
                                    | u16::from(self.op_val),
                            );
                            self.reset_tick = Tick::Reset;
                            self.op_tick = Tick::Reset;
                            self.state = State::Running;
                            Ok(OpState::Done)
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
    };
}

// Ricoh has variations in power/reset so need different macros since CMOS shares this impl.
// However resolve_opcode and opcode_op are consistent across all NMOS so the
// trait default is fine there.
impl<'a> CPU<'a> for CPURicoh<'a> {
    cpu_impl!();
    cpu_cmos_ricoh_power_reset!();
}

// CMOS has variations in ops and power/reset so need to direct implement ops
// and use the macros for everything else.
impl<'a> CPU<'a> for CPU65C02<'a> {
    /// Given an `Opcode` and `AddressMode` return the valid u8 values that
    /// can represent it.
    ///
    /// # Errors
    /// If the `AddressMode` is not valid for this opcode an error will result.
    fn resolve_opcode(&self, op: &Opcode, mode: &AddressMode) -> Result<&'static Vec<u8>> {
        let hm: &HashMap<AddressMode, Vec<u8>>;
        // SAFETY: When we built NMOS_OPCODES we validated all Opcodes were present
        unsafe {
            hm = cmos_opcodes().get(op).unwrap_unchecked();
        }
        let Some(v) = hm.get(mode) else {
            return Err(eyre!("address mode {mode} isn't valid for opcode {op}"));
        };
        Ok(v)
    }

    /// Given an opcode u8 value this will return the Operation struct
    /// defining it. i.e. `Opcode` and `AddressMode`.
    #[must_use]
    fn opcode_op(&self, op: u8) -> Operation {
        // SAFETY: We know a u8 is in range due to how we built this
        //         so a direct index is fine vs having the range check
        //         an index lookup.
        unsafe { *cmos_opcodes_values().get_unchecked(usize::from(op)) }
    }

    cpu_impl!();
    cpu_cmos_ricoh_power_reset!();
}

macro_rules! chip_impl_nmos {
    ($cpu:ident) => {
        impl<'a> Chip for $cpu<'a> {
            /// `tick` is used to move the clock forward one tick and either start processing
            /// a new opcode or continue on one already started.
            ///
            /// # Errors
            /// Bad internal state or causing a halt of the CPU will result in errors.
            ///
            /// Bad internal state includes not powering on, not completing a reset sequence
            /// and already being halted.
            #[allow(clippy::too_many_lines)]
            fn tick(&mut self) -> Result<()> {
                // Fast path if halted. PC doesn't advance nor do we take clocks.
                if self.state == State::Halted {
                    return Err(ErrReport::new(CPUError::Halted {
                        op: self.halt_opcode,
                    }));
                }

                // Handles improper states. Only tick_done() or reset() can move into this state.
                if self.state != State::Running {
                    return Err(eyre!("CPU not in Running state - {}", self.state));
                }

                // If RDY is held high we do nothing and just return (time doesn't advance in the CPU).
                // Ok, this technically only works like this in combination with SYNC being held high as well.
                // Otherwise it acts like a single step and continues after the next clock.
                // But, the only use known right now was atari 2600 which tied SYNC high and RDY low at the same
                // time so "good enough".
                if let Some(rdy) = self.rdy {
                    if rdy.raised() {
                        return Ok(());
                    }
                }

                // If we get a new interrupt while running one then NMI always wins until it's done.
                let mut irq_raised = false;
                let mut nmi_raised = false;
                if let Some(irq) = self.irq {
                    irq_raised = irq.raised();
                }
                if let Some(nmi) = self.nmi {
                    nmi_raised = nmi.raised();
                }

                // NMI always wins so even if we're already processing an interrupt it can upgrade
                // to NMI from IRQ mid processing.
                if irq_raised || nmi_raised {
                    match self.irq_raised {
                        InterruptStyle::None => {
                            self.irq_raised = InterruptStyle::IRQ;
                            if nmi_raised {
                                self.irq_raised = InterruptStyle::NMI;
                            }
                        }
                        InterruptStyle::IRQ => {
                            if nmi_raised {
                                self.irq_raised = InterruptStyle::NMI;
                            }
                        }
                        InterruptStyle::NMI => {}
                    }
                }

                // Move to the state tick_done() has to move us out of.
                self.state = State::Tick;

                // Always bump the clock count and op_tick.
                self.clocks += 1;
                self.op_tick = self.op_tick.next();

                // Emit debugging if required
                if self.debug.is_some() && self.op_tick == Tick::Tick1 {
                    self.debug();
                }

                match self.op_tick {
                    Tick::Tick1 => {
                        // If we're in Tick1 this means start a new instruction based on the PC value so grab
                        // the opcode now and look it up.
                        self.op_raw = self.ram.borrow().read(self.pc.0);
                        self.op = self.opcode_op(self.op_raw);

                        // If one is raised and we're not skipping and interrupts
                        // are enabled or it's an NMI then we move into interrupt mode.
                        // If interrupts are disabled it doesn't change state.
                        if self.irq_raised != InterruptStyle::None
                            && self.skip_interrupt != SkipInterrupt::Skip
                        {
                            if self.p & P_INTERRUPT == P_INTERRUPT
                                && self.irq_raised == InterruptStyle::IRQ
                            {
                                self.irq_raised = InterruptStyle::None;
                                self.interrupt_state = InterruptState::None;
                            } else {
                                self.interrupt_state = InterruptState::Running;
                            }
                        }

                        // PC advances always when we start a new opcode except for IRQ/NMI
                        // (unless we're skipping to run one more instruction).
                        if self.interrupt_state != InterruptState::Running {
                            self.pc += 1;
                        }

                        // Move out of the done state.
                        self.addr_done = OpState::Processing;
                        return Ok(());
                    }
                    Tick::Tick2 => {
                        // All instructions fetch the value after the opcode (though some like BRK/PHP/etc ignore it).
                        // We keep it since some instructions such as absolute addr then require getting one
                        // more byte. So cache at this stage since we no idea if it's needed.
                        // NOTE: the PC doesn't increment here as that's dependent on addressing mode which will handle it.
                        self.op_val = self.ram.borrow().read(self.pc.0);

                        // We've started a new instruction so no longer skipping interrupt processing.
                        if self.skip_interrupt == SkipInterrupt::Skip {
                            self.skip_interrupt = SkipInterrupt::PrevSkip;
                        } else {
                            self.skip_interrupt = SkipInterrupt::None;
                        }
                    }
                    // The remainder don't do anything general per cycle so they can be ignored. Opcode processing determines
                    // when they are done and all of them do sanity checking for Tick range.
                    _ => {}
                }

                // Process the opcode or interrupt
                let ret: Result<OpState>;
                if self.interrupt_state == InterruptState::Running {
                    let mut addr = IRQ_VECTOR;
                    if self.irq_raised == InterruptStyle::NMI {
                        addr = NMI_VECTOR;
                    }
                    ret = self.run_interrupt(addr, true);
                } else {
                    ret = self.process_opcode();
                }

                // We'll treat an error as halted since internal state is unknown at that point.
                // For halted instructions though we'll return a custom error type so it can
                // be detected vs internal errors.
                if self.state == State::Halted || ret.is_err() {
                    self.state = State::Halted;
                    self.halt_opcode = self.op_raw;
                    ret?;
                    return Err(ErrReport::new(CPUError::Halted {
                        op: self.halt_opcode,
                    }));
                }

                let state;
                // SAFETY: Err was checked above so just pull this out.
                unsafe {
                    state = ret.unwrap_unchecked();
                }

                if state == OpState::Done {
                    // Reset so the next tick starts a new instruction
                    // It'll handle doing start of instruction reset on state.
                    self.op_tick = Tick::Reset;

                    // If we're already running an IRQ clear state so we don't loop
                    // trying to start it again.
                    if self.interrupt_state == InterruptState::Running {
                        self.irq_raised = InterruptStyle::None;
                    }
                    self.interrupt_state = InterruptState::None;
                }
                Ok(())
            }

            /// `tick_done` moves the CPU back to a state where the next tick can run.
            /// For the 6502 there are no internal latches so generally there is no shadow
            /// state to account but all Chip implementations need this function.
            /// NOTE: Calling `tick` with rdy() high and `tick_done` with it low will
            ///       result in an error as `tick`+`tick_done` is assumed to be otherwise atomic.
            ///
            /// # Errors
            /// Bad internal state will result in errors.
            fn tick_done(&mut self) -> Result<()> {
                if let Some(rdy) = self.rdy {
                    if rdy.raised() {
                        return Ok(());
                    }
                }

                // If we're in WAI just return, no state changes.
                if self.state == State::WaitingForInterrupt {
                    return Ok(());
                }

                if self.state != State::Tick {
                    return Err(eyre!("tick_done called outside Tick - {}", self.state));
                }
                // Move to the next state.
                self.state = State::Running;
                Ok(())
            }
        }
    };
}

chip_impl_nmos!(CPU6502);
chip_impl_nmos!(CPU6510);
chip_impl_nmos!(CPURicoh);

// CMOS is slightly different so needs a direct impl and can't use the macro.
impl<'a> Chip for CPU65C02<'a> {
    /// `tick` is used to move the clock forward one tick and either start processing
    /// a new opcode or continue on one already started.
    ///
    /// # Errors
    /// Bad internal state or causing a halt of the CPU will result in errors.
    ///
    /// Bad internal state includes not powering on, not completing a reset sequence
    /// and already being halted.
    #[allow(clippy::too_many_lines)]
    fn tick(&mut self) -> Result<()> {
        // Fast path if halted. PC doesn't advance nor do we take clocks.
        if self.state == State::Halted {
            return Err(ErrReport::new(CPUError::Halted {
                op: self.halt_opcode,
            }));
        }

        // Handles improper states. Only tick_done() or reset() or wai() can move into these states.
        if self.state != State::Running && self.state != State::WaitingForInterrupt {
            return Err(eyre!(
                "CPU not in Running or WaitingForInterrupt state - {}",
                self.state
            ));
        }

        // If RDY is held high we do nothing and just return (time doesn't advance in the CPU).
        // Ok, this technically only works like this in combination with SYNC being held high as well.
        // Otherwise it acts like a single step and continues after the next clock.
        // But, the only use known right now was atari 2600 which tied SYNC high and RDY low at the same
        // time so "good enough".
        if let Some(rdy) = self.rdy {
            if rdy.raised() {
                return Ok(());
            }
        }

        // If we get a new interrupt while running one then NMI always wins until it's done.
        let mut irq_raised = false;
        let mut nmi_raised = false;
        if let Some(irq) = self.irq {
            irq_raised = irq.raised();
        }
        if let Some(nmi) = self.nmi {
            nmi_raised = nmi.raised();
        }

        // NMI always wins so even if we're already processing an interrupt it can upgrade
        // to NMI from IRQ mid processing.
        if irq_raised || nmi_raised {
            match self.irq_raised {
                InterruptStyle::None => {
                    self.irq_raised = InterruptStyle::IRQ;
                    if nmi_raised {
                        self.irq_raised = InterruptStyle::NMI;
                    }
                }
                InterruptStyle::IRQ => {
                    if nmi_raised {
                        self.irq_raised = InterruptStyle::NMI;
                    }
                }
                InterruptStyle::NMI => {}
            }
        }

        // If we're waiting for an interrupt and none have been raised we don't
        // advance time and just stay waiting for instruction start.
        if self.state == State::WaitingForInterrupt && self.irq_raised == InterruptStyle::None {
            return Ok(());
        }

        // Move to the state tick_done() has to move us out of.
        self.state = State::Tick;

        // Always bump the clock count and op_tick.
        self.clocks += 1;
        self.op_tick = self.op_tick.next();

        // Emit debugging if required
        if self.debug.is_some() && self.op_tick == Tick::Tick1 {
            self.debug();
        }

        match self.op_tick {
            Tick::Tick1 => {
                // If we're in Tick1 this means start a new instruction based on the PC value so grab
                // the opcode now and look it up.
                self.op_raw = self.ram.borrow().read(self.pc.0);
                self.op = self.opcode_op(self.op_raw);

                // Special case the 1 cycle NOP for CMOS. This just burns a cycle and ignores interrupts.
                if self.op.op == Opcode::NOP && self.op.mode == AddressMode::NOPCmos {
                    self.pc += 1;
                    self.op_tick = Tick::Reset;
                    return Ok(());
                }

                // If one is raised and we're not skipping and interrupts
                // are enabled or it's an NMI then we move into interrupt mode.
                // If interrupts are disabled it doesn't change state.
                if self.irq_raised != InterruptStyle::None
                    && self.skip_interrupt != SkipInterrupt::Skip
                {
                    if self.p & P_INTERRUPT == P_INTERRUPT && self.irq_raised == InterruptStyle::IRQ
                    {
                        self.irq_raised = InterruptStyle::None;
                        self.interrupt_state = InterruptState::None;
                    } else {
                        self.interrupt_state = InterruptState::Running;
                    }
                }

                // PC advances always when we start a new opcode except for IRQ/NMI
                // (unless we're skipping to run one more instruction).
                if self.interrupt_state != InterruptState::Running {
                    self.pc += 1;
                }

                // Move out of the done state.
                self.addr_done = OpState::Processing;
                return Ok(());
            }
            Tick::Tick2 => {
                // All instructions fetch the value after the opcode (though some like BRK/PHP/etc ignore it).
                // We keep it since some instructions such as absolute addr then require getting one
                // more byte. So cache at this stage since we no idea if it's needed.
                // NOTE: the PC doesn't increment here as that's dependent on addressing mode which will handle it.
                self.op_val = self.ram.borrow().read(self.pc.0);

                // We've started a new instruction so no longer skipping interrupt processing.
                if self.skip_interrupt == SkipInterrupt::Skip {
                    self.skip_interrupt = SkipInterrupt::PrevSkip;
                } else {
                    self.skip_interrupt = SkipInterrupt::None;
                }
            }
            // The remainder don't do anything general per cycle so they can be ignored. Opcode processing determines
            // when they are done and all of them do sanity checking for Tick range.
            _ => {}
        }

        // Process the opcode or interrupt
        let ret: Result<OpState>;
        if self.interrupt_state == InterruptState::Running {
            let mut addr = IRQ_VECTOR;
            if self.irq_raised == InterruptStyle::NMI {
                addr = NMI_VECTOR;
            }
            ret = self.run_interrupt(addr, true);
        } else {
            ret = self.process_opcode();
        }

        // We'll treat an error as halted since internal state is unknown at that point.
        // For halted instructions though we'll return a custom error type so it can
        // be detected vs internal errors.
        if self.state == State::Halted || ret.is_err() {
            self.state = State::Halted;
            self.halt_opcode = self.op_raw;
            ret?;
            return Err(ErrReport::new(CPUError::Halted {
                op: self.halt_opcode,
            }));
        }

        let state;
        // SAFETY: Err was checked above so just pull this out.
        unsafe {
            state = ret.unwrap_unchecked();
        }

        if state == OpState::Done {
            // Reset so the next tick starts a new instruction
            // It'll handle doing start of instruction reset on state.
            self.op_tick = Tick::Reset;

            // If we're already running an IRQ clear state so we don't loop
            // trying to start it again.
            if self.interrupt_state == InterruptState::Running {
                self.irq_raised = InterruptStyle::None;
            }
            self.interrupt_state = InterruptState::None;
        }
        Ok(())
    }

    /// `tick_done` moves the CPU back to a state where the next tick can run.
    /// For the 6502 there are no internal latches so generally there is no shadow
    /// state to account but all Chip implementations need this function.
    /// NOTE: Calling `tick` with rdy() high and `tick_done` with it low will
    ///       result in an error as `tick`+`tick_done` is assumed to be otherwise atomic.
    ///
    /// # Errors
    /// Bad internal state will result in errors.
    fn tick_done(&mut self) -> Result<()> {
        if let Some(rdy) = self.rdy {
            if rdy.raised() {
                return Ok(());
            }
        }

        // If we're in WAI just return, no state changes.
        if self.state == State::WaitingForInterrupt {
            return Ok(());
        }

        if self.state != State::Tick {
            return Err(eyre!("tick_done called outside Tick - {}", self.state));
        }
        // Move to the next state.
        self.state = State::Running;
        Ok(())
    }
}

macro_rules! cpu_new {
    () => {
        /// Build a new CPU.
        /// NOTE: This is not usable at this point. Call `power_on` to begin
        /// operation. Anything else will return errors.
        #[must_use]
        pub fn new(def: ChipDef<'a>) -> Self {
            Self {
                a: Wrapping(0x00),
                x: Wrapping(0x00),
                y: Wrapping(0x00),
                s: Wrapping(0x00),
                p: P_NONE,
                pc: Wrapping(0x0000),
                debug: None,
                state: State::Off,
                irq_raised: InterruptStyle::default(),
                irq: def.irq,
                nmi: def.nmi,
                rdy: def.rdy,
                interrupt_state: InterruptState::default(),
                skip_interrupt: SkipInterrupt::default(),
                clocks: 0,
                ram: Rc::new(RefCell::new(def.ram)),
                op: Operation::default(),
                op_raw: 0x00,
                op_val: 0x00,
                op_tick: Tick::Reset,
                reset_tick: Tick::Reset,
                op_addr: 0x0000,
                addr_done: OpState::Done,
                halt_opcode: 0x00,
                halt_pc: 0x0000,
            }
        }
    };
}

impl<'a> CPU6502<'a> {
    cpu_new!();
}

impl<'a> CPURicoh<'a> {
    cpu_new!();
}

impl<'a> CPU6510<'a> {
    /// Build a new Cpu. Optionally pass input states for the I/O pins. If
    /// nothing is passed these will all be tied to pulldowns.
    /// If the io pins are not input style this will panic as it's intended
    /// to emulate initial wiring for a 6510 which powers up in input mode.
    /// NOTE: This is not usable at this point. Call `power_on` to begin
    /// operation. Anything else will return errors.
    #[must_use]
    pub fn new(def: ChipDef<'a>, io: Option<[io::Style; 6]>) -> Self {
        let input = match io {
            Some(io) => io,
            None => [io::Style::In(&io::Pulldown {}); 6],
        };
        let mut r = Box::new(C6510RAM::new(def.ram, input));
        r.power_on();
        let io = r.io_state.clone();
        let ram: std::rc::Rc<std::cell::RefCell<std::boxed::Box<(dyn memory::Memory + 'static)>>> =
            Rc::new(RefCell::new(r));
        Self {
            a: Wrapping(0x00),
            x: Wrapping(0x00),
            y: Wrapping(0x00),
            s: Wrapping(0x00),
            p: P_NONE,
            pc: Wrapping(0x0000),
            debug: None,
            state: State::Off,
            irq_raised: InterruptStyle::default(),
            irq: def.irq,
            nmi: def.nmi,
            rdy: def.rdy,
            interrupt_state: InterruptState::default(),
            skip_interrupt: SkipInterrupt::default(),
            clocks: 0,
            ram,
            op: Operation::default(),
            op_raw: 0x00,
            op_val: 0x00,
            op_tick: Tick::Reset,
            reset_tick: Tick::Reset,
            op_addr: 0x0000,
            addr_done: OpState::Done,
            halt_opcode: 0x00,
            halt_pc: 0x0000,
            io,
        }
    }

    /// Return the state of I/O pin 0-5 for a 6510.
    ///
    /// # Errors
    /// Any usage outside 0-5 for pin will result in an error.
    pub fn io_pin(&self, pin: usize) -> Result<io::Style> {
        if pin > 5 {
            return Err(eyre!("I/O pin {pin} out of range"));
        }
        Ok(self.io.borrow()[pin])
    }
}

impl<'a> CPU65C02<'a> {
    cpu_new!();

    // CMOS has a different process_opcode so we override the trait default impl.
    #[allow(clippy::too_many_lines)]
    fn process_opcode(&mut self) -> Result<OpState> {
        match (self.op.op, self.op.mode) {
            // 0x00 - BRK #i
            (Opcode::BRK, AddressMode::Immediate) => self.brk(),
            // 0x01 - ORA (d,x)
            (Opcode::ORA, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::ora)
            }
            // 0x02 0x22 0x42 0x62 0x82 0xC2 0xE2 - NOP #i
            (Opcode::NOP, AddressMode::Immediate) => self.addr_immediate(&InstructionMode::Load),
            // 0x03 0x13 0x23 0x33 0x43 0x53 0x63 0x73 0x83 0x93 0xA3 0xB3 0xC3 0xD3 0xE3 0xF3
            // 0x0B 0x1B 0x2B 0x3B 0x4B 0x5B 0x6B 0x7B 0x8B 0x9B 0xAB 0xBB 0xEA 0xEB 0xFB
            // NOP
            // Note: Technically none of these which trigger NOPCmos will get here as single
            //       cycles ones are handled directly in tick(). But here for completeness.
            (Opcode::NOP, AddressMode::NOPCmos | AddressMode::Implied) => Ok(OpState::Done),
            // 0x04 - TSB d
            (Opcode::TSB, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::tsb),
            // 0x05 - ORA d
            (Opcode::ORA, AddressMode::ZeroPage) => self.load_instruction(Self::addr_zp, Self::ora),
            // 0x06 - ASL d
            (Opcode::ASL, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::asl),
            // 0x07 0x17 0x27 0x37 0x47 0x57 0x67 0x77 - RMB x,d
            (Opcode::RMB, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::rmb),
            // 0x08 - PHP
            (Opcode::PHP, AddressMode::Implied) => self.php(),
            // 0x09 - ORA #i
            (Opcode::ORA, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::ora)
            }
            // 0x0A - ASL
            (Opcode::ASL, AddressMode::Implied) => self.asl_acc(),
            // 0x0B - NOP see 0x03
            // 0x0C - TSB a
            (Opcode::TSB, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::tsb)
            }
            // 0x0D - ORA a
            (Opcode::ORA, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::ora)
            }
            // 0x0E - ASL a
            (Opcode::ASL, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::asl)
            }
            // 0x0F 0x1F 0x2F 0x3F 0x4F 0x5F 0x6F 0x7F - BBR x,dr
            (Opcode::BBR, AddressMode::ZeroPageRelative) => {
                // Figure out the x based on opcode. MSB is the index
                let x = (self.op_raw & 0xF0) >> 4;
                self.bbr(x)
            }
            // 0x10 - BPL *+r
            (Opcode::BPL, AddressMode::Relative) => self.bpl(),
            // 0x11 - ORA (d),y
            (Opcode::ORA, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::ora)
            }
            // 0x12 - ORA (d)
            (Opcode::ORA, AddressMode::Indirect) => {
                self.load_instruction(Self::addr_indirect, Self::ora)
            }
            // 0x13 - NOP see 0x03
            // 0x14 - TRB d
            (Opcode::TRB, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::trb),
            // 0x15 - ORA d,x
            (Opcode::ORA, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::ora)
            }
            // 0x16 - ASL d,x
            (Opcode::ASL, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::asl)
            }
            // 0x17 - RMB 1,x see 0x07
            // 0x18 - CLC
            (Opcode::CLC, AddressMode::Implied) => self.clc(),
            // 0x19 - ORA a,y
            (Opcode::ORA, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::ora)
            }
            // 0x1A - INC
            (Opcode::INC, AddressMode::Implied) => {
                self.load_register(Register::A, (self.a + Wrapping(1)).0)
            }
            // 0x1B - NOP see 0x03
            // 0x1C - TRB a
            (Opcode::TRB, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::trb)
            }
            // 0x1D - ORA a,x
            (Opcode::ORA, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::ora)
            }
            // 0x1E - ASL a,x
            (Opcode::ASL, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::asl)
            }
            // 0x1F - BBR 1,dr see 0x0F
            // 0x20 - JSR a
            (Opcode::JSR, AddressMode::Absolute) => self.jsr(),
            // 0x21 - AND (d,x)
            (Opcode::AND, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::and)
            }
            // 0x22 - NOP #i see 0x02
            // 0x23 - NOP see 0x03
            // 0x24 - BIT d
            (Opcode::BIT, AddressMode::ZeroPage) => self.load_instruction(Self::addr_zp, Self::bit),
            // 0x25 - AND d
            (Opcode::AND, AddressMode::ZeroPage) => self.load_instruction(Self::addr_zp, Self::and),
            // 0x26 - ROL d
            (Opcode::ROL, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::rol),
            // 0x27 - RMB 2,x see 0x07
            // 0x28 - PLP
            (Opcode::PLP, AddressMode::Implied) => self.plp(),
            // 0x29 - AND #i
            (Opcode::AND, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::and)
            }
            // 0x2A - ROL
            (Opcode::ROL, AddressMode::Implied) => self.rol_acc(),
            // 0x2B - NOP see 0x03
            // 0x2C - BIT a
            (Opcode::BIT, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::bit)
            }
            // 0x2D - AND a
            (Opcode::AND, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::and)
            }
            // 0x2E - ROL a
            (Opcode::ROL, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::rol)
            }
            // 0x2F - BBR 2,dr see 0x0F
            // 0x30 - BMI *+r
            (Opcode::BMI, AddressMode::Relative) => self.bmi(),
            // 0x31 - AND (d),y
            (Opcode::AND, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::and)
            }
            // 0x32 - AND (d)
            (Opcode::AND, AddressMode::Indirect) => {
                self.load_instruction(Self::addr_indirect, Self::and)
            }
            // 0x33 - NOP see 0x03
            // 0x34 - BIT d,x
            (Opcode::BIT, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::bit)
            }
            // 0x35 - AND d,x
            (Opcode::AND, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::and)
            }
            // 0x36 - ROL d,x
            (Opcode::ROL, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::rol)
            }
            // 0x37 - RMB 3,x see 0x07
            // 0x38 - SEC
            (Opcode::SEC, AddressMode::Implied) => self.sec(),
            // 0x39 - AND a,y
            (Opcode::AND, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::and)
            }
            // 0x3A - DEC
            (Opcode::DEC, AddressMode::Implied) => {
                self.load_register(Register::A, (self.a - Wrapping(1)).0)
            }
            // 0x3B - NOP see 0x03
            // 0x3C - BIT a,x
            (Opcode::BIT, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::bit)
            }
            // 0x3D - AND a,x
            (Opcode::AND, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::and)
            }
            // 0x3E - ROL a,x
            (Opcode::ROL, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::rol)
            }
            // 0x3F - BBR 3,dr see 0x0F
            // 0x40 - RTI
            (Opcode::RTI, AddressMode::Implied) => self.rti(),
            // 0x41 - EOR (d,x)
            (Opcode::EOR, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::eor)
            }
            // 0x42 - NOP #i see 0x02
            // 0x43 - NOP see 0x03
            // 0x44 - NOP d
            (Opcode::NOP, AddressMode::ZeroPage) => self.addr_zp(&InstructionMode::Load),
            // 0x45 - EOR d
            (Opcode::EOR, AddressMode::ZeroPage) => self.load_instruction(Self::addr_zp, Self::eor),
            // 0x46 - LSR d
            (Opcode::LSR, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::lsr),
            // 0x47 - RMB 4,x see 0x07
            // 0x48 - PHA
            (Opcode::PHA, AddressMode::Implied) => self.pha(),
            // 0x49 - EOR #i
            (Opcode::EOR, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::eor)
            }
            // 0x4A - LSR
            (Opcode::LSR, AddressMode::Implied) => self.lsr_acc(),
            // 0x4B - NOP see 0x03
            // 0x4C - JMP a
            (Opcode::JMP, AddressMode::Absolute) => self.jmp(),
            // 0x4D - EOR a
            (Opcode::EOR, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::eor)
            }
            // 0x4E - LSR a
            (Opcode::LSR, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::lsr)
            }
            // 0x4F - BBR 4,dr see 0x0F
            // 0x50 - BVC *+r
            (Opcode::BVC, AddressMode::Relative) => self.bvc(),
            // 0x51 - EOR (d),y
            (Opcode::EOR, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::eor)
            }
            // 0x52 - EOR (d)
            (Opcode::EOR, AddressMode::Indirect) => {
                self.load_instruction(Self::addr_indirect, Self::eor)
            }
            // 0x53 - NOP see 0x03
            // 0x54 0xD4 0xF4 - NOP d,x
            (Opcode::NOP, AddressMode::ZeroPageX) => self.addr_zp_x(&InstructionMode::Load),
            // 0x55 - EOR d,x
            (Opcode::EOR, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::eor)
            }
            // 0x56 - LSR d,x
            (Opcode::LSR, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::lsr)
            }
            // 0x57 - RMB 5,x see 0x07
            // 0x58 - CLI
            (Opcode::CLI, AddressMode::Implied) => self.cli(),
            // 0x59 - EOR a,y
            (Opcode::EOR, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::eor)
            }
            // 0x5A - PHY
            (Opcode::PHY, AddressMode::Implied) => self.phy(),
            // 0x5B - NOP see 0x03
            // 0x5C - NOP 8
            (Opcode::NOP, AddressMode::AbsoluteNOP) => self.nop8(),
            // 0x5D - EOR a,x
            (Opcode::EOR, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::eor)
            }
            // 0x5E - LSR a,x
            (Opcode::LSR, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::lsr)
            }
            // 0x5F - BBR 5,dr see 0x0F
            // 0x60 - RTS
            (Opcode::RTS, AddressMode::Implied) => self.rts(),
            // 0x61 - ADC (d,x)
            (Opcode::ADC, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::adc)
            }
            // 0x62 - NOP #i see 0x02
            // 0x63 - NOP see 0x03
            // 0x64 - STZ d
            (Opcode::STZ, AddressMode::ZeroPage) => self.store_instruction(Self::addr_zp, 0),
            // 0x65 - ADC d
            (Opcode::ADC, AddressMode::ZeroPage) => self.load_instruction(Self::addr_zp, Self::adc),
            // 0x66 - ROR d
            (Opcode::ROR, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::ror),
            // 0x67 - RMB 6,x see 0x07
            // 0x68 - PLA
            (Opcode::PLA, AddressMode::Implied) => self.pla(),
            // 0x69 - ADC #i
            (Opcode::ADC, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::adc)
            }
            // 0x6A - ROR
            (Opcode::ROR, AddressMode::Implied) => self.ror_acc(),
            // 0x6B - NOP see 0x03
            // 0x6C - JMP (a)
            (Opcode::JMP, AddressMode::AbsoluteIndirect) => self.jmp_indirect(),
            // 0x6D - ADC a
            (Opcode::ADC, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::adc)
            }
            // 0x6E - ROR a
            (Opcode::ROR, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::ror)
            }
            // 0x6F - BBR 6,dr see 0x0F
            // 0x70 - BVS *+r
            (Opcode::BVS, AddressMode::Relative) => self.bvs(),
            // 0x71 - ADC (d),y
            (Opcode::ADC, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::adc)
            }
            // 0x72 - ADC (zp)
            (Opcode::ADC, AddressMode::Indirect) => {
                self.load_instruction(Self::addr_indirect, Self::adc)
            }
            // 0x73 - NOP see 0x03
            // 0x74 - STZ d,x
            (Opcode::STZ, AddressMode::ZeroPageX) => self.store_instruction(Self::addr_zp_x, 0),
            // 0x75 - ADC d,x
            (Opcode::ADC, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::adc)
            }
            // 0x76 - ROR d,x
            (Opcode::ROR, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::ror)
            }
            // 0x77 - RMB 7,x see 0x07
            // 0x78 - SEI
            (Opcode::SEI, AddressMode::Implied) => self.sei(),
            // 0x79 - ADC a,y
            (Opcode::ADC, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::adc)
            }
            // 0x7A - PLY
            (Opcode::PLY, AddressMode::Implied) => self.ply(),
            // 0x7B - NOP see 0x03
            // 0x7C - JMP (a,x)
            (Opcode::JMP, AddressMode::AbsoluteIndirectX) => self.jmp_indirect_x(),
            // 0x7D - ADC a,x
            (Opcode::ADC, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::adc)
            }
            // 0x7E - ROR a,x
            (Opcode::ROR, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::ror)
            }
            // 0x7F - BBR 7,dr see 0x0F
            // 0x80 - BRA r
            (Opcode::BRA, AddressMode::Relative) => self.perform_branch(),
            // 0x81 - STA (d,x)
            (Opcode::STA, AddressMode::IndirectX) => {
                self.store_instruction(Self::addr_indirect_x, self.a.0)
            }
            // 0x82 - NOP #i see 0x02
            // 0x83 - NOP see 0x03
            // 0x84 - STY d
            (Opcode::STY, AddressMode::ZeroPage) => self.store_instruction(Self::addr_zp, self.y.0),
            // 0x85 - STA d
            (Opcode::STA, AddressMode::ZeroPage) => self.store_instruction(Self::addr_zp, self.a.0),
            // 0x86 - STX d
            (Opcode::STX, AddressMode::ZeroPage) => self.store_instruction(Self::addr_zp, self.x.0),
            // 0x87 - 0x87 0x97 0xA7 0xB7 0xC7 0xD7 0xE7 0xF7 - SMB x,d
            (Opcode::SMB, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::smb),
            // 0x88 - DEY
            (Opcode::DEY, AddressMode::Implied) => {
                self.load_register(Register::Y, (self.y - Wrapping(1)).0)
            }
            // 0x89 - BIT #i
            (Opcode::BIT, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::bit)
            }
            // 0x8A - TXA
            (Opcode::TXA, AddressMode::Implied) => self.load_register(Register::A, self.x.0),
            // 0x8B - NOP see 0x03
            // 0x8C - STY a
            (Opcode::STY, AddressMode::Absolute) => {
                self.store_instruction(Self::addr_absolute, self.y.0)
            }
            // 0x8D - STA a
            (Opcode::STA, AddressMode::Absolute) => {
                self.store_instruction(Self::addr_absolute, self.a.0)
            }
            // 0x8E - STX a
            (Opcode::STX, AddressMode::Absolute) => {
                self.store_instruction(Self::addr_absolute, self.x.0)
            }
            // 0x8F 0x9F 0xAF 0xBF 0xCF 0xDF 0xEF 0xFF - BBR x,dr
            (Opcode::BBS, AddressMode::ZeroPageRelative) => {
                // Figure out the x based on opcode. MSB is the index - 8.
                let x = ((self.op_raw & 0xF0) >> 4) - 8;
                self.bbs(x)
            }
            // 0x90 - BCC *+r
            (Opcode::BCC, AddressMode::Relative) => self.bcc(),
            // 0x91 - STA (d),y
            (Opcode::STA, AddressMode::IndirectY) => {
                self.store_instruction(Self::addr_indirect_y, self.a.0)
            }
            // 0x92 - STA (zp)
            (Opcode::STA, AddressMode::Indirect) => {
                self.store_instruction(Self::addr_indirect, self.a.0)
            }
            // 0x93 - NOP see 0x03
            // 0x94 - STY d,x
            (Opcode::STY, AddressMode::ZeroPageX) => {
                self.store_instruction(Self::addr_zp_x, self.y.0)
            }
            // 0x95 - STA d,x
            (Opcode::STA, AddressMode::ZeroPageX) => {
                self.store_instruction(Self::addr_zp_x, self.a.0)
            }
            // 0x96 - STX d,y
            (Opcode::STX, AddressMode::ZeroPageY) => {
                self.store_instruction(Self::addr_zp_y, self.x.0)
            }
            // 0x97 - SMB 1,d see 0x87
            // 0x98 - TYA
            (Opcode::TYA, AddressMode::Implied) => self.load_register(Register::A, self.y.0),
            // 0x99 - STA a,y
            (Opcode::STA, AddressMode::AbsoluteY) => {
                self.store_instruction(Self::addr_absolute_y, self.a.0)
            }
            // 0x9A - TXS
            (Opcode::TXS, AddressMode::Implied) => {
                self.s = self.x;
                Ok(OpState::Done)
            }
            // 0x9B - NOP see 0x03
            // 0x9C - STZ a
            (Opcode::STZ, AddressMode::Absolute) => self.store_instruction(Self::addr_absolute, 0),
            // 0x9D - STA a,x
            (Opcode::STA, AddressMode::AbsoluteX) => {
                self.store_instruction(Self::addr_absolute_x, self.a.0)
            }
            // 0x9E - STZ a,x
            (Opcode::STZ, AddressMode::AbsoluteX) => {
                self.store_instruction(Self::addr_absolute_x, 0)
            }
            // 0x9F - BBS 1,dr - see 0x8F
            // 0xA0 - LDY #i
            (Opcode::LDY, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::load_register_y)
            }
            // 0xA1 - LDA (d,x)
            (Opcode::LDA, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::load_register_a)
            }
            // 0xA2 - LDX #i
            (Opcode::LDX, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::load_register_x)
            }
            // 0xA3 - NOP see 0x03
            // 0xA4 - LDY d
            (Opcode::LDY, AddressMode::ZeroPage) => {
                self.load_instruction(Self::addr_zp, Self::load_register_y)
            }
            // 0xA5 - LDA d
            (Opcode::LDA, AddressMode::ZeroPage) => {
                self.load_instruction(Self::addr_zp, Self::load_register_a)
            }
            // 0xA6 - LDX d
            (Opcode::LDX, AddressMode::ZeroPage) => {
                self.load_instruction(Self::addr_zp, Self::load_register_x)
            }
            // 0xA7 - SMB 2,d see 0x87
            // 0xA8 - TAY
            (Opcode::TAY, AddressMode::Implied) => self.load_register(Register::Y, self.a.0),
            // 0xA9 - LDA #i
            (Opcode::LDA, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::load_register_a)
            }
            // 0xAA - TAX
            (Opcode::TAX, AddressMode::Implied) => self.load_register(Register::X, self.a.0),
            // 0xAB - NOP see 0x03
            // 0xAC - LDY a
            (Opcode::LDY, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::load_register_y)
            }
            // 0xAD - LDA a
            (Opcode::LDA, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::load_register_a)
            }
            // 0xAE - LDX a
            (Opcode::LDX, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::load_register_x)
            }
            // 0xAF - BBS 2,dr - see 0x8F
            // 0xB0 - BCS *+r
            (Opcode::BCS, AddressMode::Relative) => self.bcs(),
            // 0xB1 - LDA (d),y
            (Opcode::LDA, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::load_register_a)
            }
            // 0xB2 - LDA (zp)
            (Opcode::LDA, AddressMode::Indirect) => {
                self.load_instruction(Self::addr_indirect, Self::load_register_a)
            }
            // 0xB3 - NOP see 0x03
            // 0xB4 - LDY d,x
            (Opcode::LDY, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::load_register_y)
            }
            // 0xB5 - LDA d,x
            (Opcode::LDA, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::load_register_a)
            }
            // 0xB6 - LDX d,y
            (Opcode::LDX, AddressMode::ZeroPageY) => {
                self.load_instruction(Self::addr_zp_y, Self::load_register_x)
            }
            // 0xB7 - SMB 3,d see 0x87
            // 0xB8 - CLV
            (Opcode::CLV, AddressMode::Implied) => self.clv(),
            // 0xB9 - LDA a,y
            (Opcode::LDA, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::load_register_a)
            }
            // 0xBA - TSX
            (Opcode::TSX, AddressMode::Implied) => self.load_register(Register::X, self.s.0),
            // 0xBB - NOP see 0x03
            // 0xBC - LDY a,x
            (Opcode::LDY, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::load_register_y)
            }
            // 0xBD - LDA a,x
            (Opcode::LDA, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::load_register_a)
            }
            // 0xBE - LDX a,y
            (Opcode::LDX, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::load_register_x)
            }
            // 0xBF - BBS 3,dr - see 0x8F
            // 0xC0 - CPY #i
            (Opcode::CPY, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::compare_y)
            }
            // 0xC1 - CMP (d,x)
            (Opcode::CMP, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::compare_a)
            }
            // 0xC2 - NOP #i see 0x02
            // 0xC3 - NOP see 0x03
            // 0xC4 - CPY d
            (Opcode::CPY, AddressMode::ZeroPage) => {
                self.load_instruction(Self::addr_zp, Self::compare_y)
            }
            // 0xC5 - CMP d
            (Opcode::CMP, AddressMode::ZeroPage) => {
                self.load_instruction(Self::addr_zp, Self::compare_a)
            }
            // 0xC6 - DEC d
            (Opcode::DEC, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::dec),
            // 0xC7 - SMB 4,d see 0x87
            // 0xC8 - INY
            (Opcode::INY, AddressMode::Implied) => {
                self.load_register(Register::Y, (self.y + Wrapping(1)).0)
            }
            // 0xC9 - CMP #i
            (Opcode::CMP, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::compare_a)
            }
            // 0xCA - DEX
            (Opcode::DEX, AddressMode::Implied) => {
                self.load_register(Register::X, (self.x - Wrapping(1)).0)
            }
            // 0xCB - WAI
            (Opcode::WAI, AddressMode::Implied) => self.wai(),
            // 0xCC - CPY a
            (Opcode::CPY, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::compare_y)
            }
            // 0xCD - CMP a
            (Opcode::CMP, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::compare_a)
            }
            // 0xCE - DEC a
            (Opcode::DEC, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::dec)
            }
            // 0xCF - BBS 4,dr - see 0x8F
            // 0xD0 - BNE *+r
            (Opcode::BNE, AddressMode::Relative) => self.bne(),
            // 0xD1 - CMP (d),y
            (Opcode::CMP, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::compare_a)
            }
            // 0xD2 - CMP (d)
            (Opcode::CMP, AddressMode::Indirect) => {
                self.load_instruction(Self::addr_indirect, Self::compare_a)
            }
            // 0xD3 - NOP see 0x03
            // 0xD4 - NOP d,x see 0x54
            // 0xD5 - CMP d,x
            (Opcode::CMP, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::compare_a)
            }
            // 0xD6 - DEC d,x
            (Opcode::DEC, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::dec)
            }
            // 0xD7 - SMB 5,d see 0x87
            // 0xD8 - CLD
            (Opcode::CLD, AddressMode::Implied) => self.cld(),
            // 0xD9 - CMP a,y
            (Opcode::CMP, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::compare_a)
            }
            // 0xDA - PHX
            (Opcode::PHX, AddressMode::Implied) => self.phx(),
            // 0xDB - STP
            (Opcode::STP, AddressMode::Implied) => {
                self.state = State::Halted;
                Ok(OpState::Done)
            }
            // 0xDC 0xFC - NOP a,x
            (Opcode::NOP, AddressMode::AbsoluteX) => self.addr_absolute_x(&InstructionMode::Load),
            // 0xDD - CMP a,x
            (Opcode::CMP, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::compare_a)
            }
            // 0xDE - DEC a,x
            (Opcode::DEC, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::dec)
            }
            // 0xDF - BBS 5,dr - see 0x8F
            // 0xE0 - CPX #i
            (Opcode::CPX, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::compare_x)
            }
            // 0xE1 - SBC (d,x)
            (Opcode::SBC, AddressMode::IndirectX) => {
                self.load_instruction(Self::addr_indirect_x, Self::sbc)
            }
            // 0xE2 - NOP #i see 0x02
            // 0xE3 - NOP see 0x03
            // 0xE4 - CPX d
            (Opcode::CPX, AddressMode::ZeroPage) => {
                self.load_instruction(Self::addr_zp, Self::compare_x)
            }
            // 0xE5 - SBC d
            (Opcode::SBC, AddressMode::ZeroPage) => self.load_instruction(Self::addr_zp, Self::sbc),
            // 0xE6 - INC d
            (Opcode::INC, AddressMode::ZeroPage) => self.rmw_instruction(Self::addr_zp, Self::inc),
            // 0xE7 - SMB 6,d see 0x87
            // 0xE8 - INX
            (Opcode::INX, AddressMode::Implied) => {
                self.load_register(Register::X, (self.x + Wrapping(1)).0)
            }
            // 0xE9 0xEB - SBC #i
            (Opcode::SBC, AddressMode::Immediate) => {
                self.load_instruction(Self::addr_immediate, Self::sbc)
            }
            // 0xEA - NOP see 0x03
            // 0xEB - NOP see 0x03
            // 0xEC - CPX a
            (Opcode::CPX, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::compare_x)
            }
            // 0xED - SBC a
            (Opcode::SBC, AddressMode::Absolute) => {
                self.load_instruction(Self::addr_absolute, Self::sbc)
            }
            // 0xEE - INC a
            (Opcode::INC, AddressMode::Absolute) => {
                self.rmw_instruction(Self::addr_absolute, Self::inc)
            }
            // 0xEF - BBS 6,dr - see 0x8F
            // 0xF0 - BEQ *+r
            (Opcode::BEQ, AddressMode::Relative) => self.beq(),
            // 0xF1 - SBC (d),y
            (Opcode::SBC, AddressMode::IndirectY) => {
                self.load_instruction(Self::addr_indirect_y, Self::sbc)
            }
            // 0xF2 - SBC (d)
            (Opcode::SBC, AddressMode::Indirect) => {
                self.load_instruction(Self::addr_indirect, Self::sbc)
            }
            // 0xF3 - NOP see 0x03
            // 0xF4 - NOP d,x see 0x54
            // 0xF5 - SBC d,x
            (Opcode::SBC, AddressMode::ZeroPageX) => {
                self.load_instruction(Self::addr_zp_x, Self::sbc)
            }
            // 0xF6 - INC d,x
            (Opcode::INC, AddressMode::ZeroPageX) => {
                self.rmw_instruction(Self::addr_zp_x, Self::inc)
            }
            // 0xF7 - SMB 7,d see 0x87
            // 0xF8 - SED
            (Opcode::SED, AddressMode::Implied) => self.sed(),
            // 0xF9 - SBC a,y
            (Opcode::SBC, AddressMode::AbsoluteY) => {
                self.load_instruction(Self::addr_absolute_y, Self::sbc)
            }
            // 0xFA - PLX
            (Opcode::PLX, AddressMode::Implied) => self.plx(),
            // 0xFB - NOP see 0x03
            // 0xFC - NOP a,x see 0xDC
            // 0xFD - SBC a,x
            (Opcode::SBC, AddressMode::AbsoluteX) => {
                self.load_instruction(Self::addr_absolute_x, Self::sbc)
            }
            // 0xFE - INC a,x
            (Opcode::INC, AddressMode::AbsoluteX) => {
                self.rmw_instruction(Self::addr_absolute_x, Self::inc)
            }
            // 0xFF - BBS 7,dr - see 0x8F
            _ => Err(eyre!(
                "no implementation for {:?} {:?}",
                self.op.op,
                self.op.mode
            )),
        }
    }

    // addr_indirect implements Zero page indirect mode - (d)
    // returning the value in op_val and the address read in op_addr.
    // Returns OpState::Done if this tick ends address processing and/or any errors.
    fn addr_indirect(&mut self, mode: &InstructionMode) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 | Tick::Tick6 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("addr_indirect_x invalid op_tick: {:?}", self.op_tick))
            }
            Tick::Tick2 => {
                // We've already read the value but need to bump the PC
                // and assign it into op_addr.
                self.op_addr = u16::from(self.op_val) & 0x00FF;
                self.pc += 1;
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // Read effective addr low byte
                self.op_val = self.ram.borrow().read(self.op_addr);
                // Now increment (with ZP rollover) for next read.
                // There is no truncation since we know this is always
                // 0-255.
                #[allow(clippy::cast_possible_truncation)]
                let a = Wrapping(self.op_addr as u8);
                self.op_addr = u16::from((a + Wrapping(1)).0);
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // Read high byte, shift over and add op_val which has the low byte.
                self.op_addr =
                    (u16::from(self.ram.borrow().read(self.op_addr)) << 8) | u16::from(self.op_val);
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                // Addr is in op_addr. If this is a load go ahead and read it.
                // Otherwise a store will just use it.
                if *mode == InstructionMode::Load {
                    self.op_val = self.ram.borrow().read(self.op_addr);
                }
                Ok(OpState::Done)
            }
        }
    }

    // bbr_bbs_common implement the common tick portions which both instructions
    // share. They only differ on how the bit is tested but have unique bus cycle
    // operations compared to normal branches.
    fn bbr_bbs_common(&mut self) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 | Tick::Tick8 => {
                Err(eyre!("bbr_bbs_common: invalid op_tick: {:?}", self.op_tick))
            }
            Tick::Tick2 => {
                // Just bump the PC and continue after computing op_addr for
                // the ZP read. op_val was already read for us before entering.
                self.op_addr = u16::from(self.op_val);
                self.pc += 1;
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // Read the ZP value into op_addr which we can use to mask
                // below to test.
                self.op_addr = u16::from(self.ram.borrow().read(self.op_addr));
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // Read dest offset
                self.op_val = self.ram.borrow().read(self.pc.0);
                self.pc += 1;
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                // Compute the initial branch PC and read from it regardless if
                // we branch or not.
                let a = Wrapping(
                    (self.pc.0 & 0xFF00)
                        | u16::from(
                            (Wrapping((self.pc.0 & 0x00FF) as u8) + Wrapping(self.op_val)).0,
                        ),
                )
                .0;
                _ = self.ram.borrow().read(a);
                // Just return here and let bbr/bbs pick up this.
                Ok(OpState::Processing)
            }
            Tick::Tick6 => self.branch_taken(),
            Tick::Tick7 => self.branch_taken2(),
        }
    }

    // bbr implements support for all BBR x,d instructions which will branch
    // if bit x is clear in the given ZP location.
    fn bbr(&mut self, pos: u8) -> Result<OpState> {
        let ret = self.bbr_bbs_common()?;
        if self.op_tick == Tick::Tick5 {
            // For tick 5 common implemented the parts needed for bus cycling
            // and then returns processing. Pick the actual return based on
            // whether we really branch or not.
            let mask = 1 << pos;
            if self.op_addr & mask == 0x00 {
                // We keep going to do the actual branch
                Ok(OpState::Processing)
            } else {
                Ok(OpState::Done)
            }
        } else {
            Ok(ret)
        }
    }

    // bbs implements support for all BBR x,d instructions which will branch
    // if bit x is set in the given ZP location.
    fn bbs(&mut self, pos: u8) -> Result<OpState> {
        let ret = self.bbr_bbs_common()?;
        if self.op_tick == Tick::Tick5 {
            // For tick 5 common implemented the parts needed for bus cycling
            // and then returns processing. Pick the actual return based on
            // whether we really branch or not.
            let mask = 1 << pos;
            if self.op_addr & mask == 0x00 {
                // In this one if the bit is clear we're done.
                Ok(OpState::Done)
            } else {
                // We keep going to do the actual branch
                Ok(OpState::Processing)
            }
        } else {
            Ok(ret)
        }
    }

    // jmp_indirect_x implements the indirect JMP instruction for jumping through a pointer to a new address
    // while adding X to the pointer before reading. This allows for easier jump
    // table construction. This is mostly the same as jmp_indirect except
    // bus reads are simpler and different due to CMOS only and no legacy bugs.
    // Returns Done when the PC is correct. Returns an error on an invalid tick.
    fn jmp_indirect_x(&mut self) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 | Tick::Tick7 | Tick::Tick8 => {
                Err(eyre!("jmp indirect_x: invalid op_tick: {:?}", self.op_tick))
            }
            Tick::Tick2 => {
                // op_val has the first start of the address so start computing it.
                self.op_addr = u16::from(self.op_val) & 0x00FF;
                self.pc += 1;
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                self.op_val = self.ram.borrow().read(self.pc.0);
                self.op_addr |= u16::from(self.op_val) << 8;
                // NOTE: Don't increment PC as we read this again in tick 4
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // Do nothing read of 2nd op byte
                _ = self.ram.borrow().read(self.pc.0);
                // Add X to op_addr
                self.op_addr = (Wrapping(self.op_addr) + Wrapping(u16::from(self.x.0))).0;
                Ok(OpState::Processing)
            }
            Tick::Tick5 => {
                // Read the high byte.
                self.op_val = self.ram.borrow().read(self.op_addr);
                self.op_addr = (Wrapping(self.op_addr) + Wrapping(1)).0;
                Ok(OpState::Processing)
            }
            Tick::Tick6 => {
                // Read the low byte and compute new PC
                let val = self.ram.borrow().read(self.op_addr);
                self.pc = Wrapping(u16::from(val) << 8) | Wrapping(u16::from(self.op_val));
                Ok(OpState::Done)
            }
        }
    }

    // nop8 implements the strange NOP for CMOS.
    // This will read an absolute argument (bb aa) and then
    // read FFBB followed by 4 FFFF reads
    // Returns Done when done and/or errors.
    fn nop8(&mut self) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset | Tick::Tick1 => Err(eyre!("pha: invalid op_tick: {:?}", self.op_tick)),
            Tick::Tick2 => {
                // op_val has the first start of the address so start computing it.
                self.op_addr = u16::from(self.op_val) & 0x00FF;
                self.pc += 1;
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                self.op_val = self.ram.borrow().read(self.pc.0);
                self.pc += 1;
                self.op_addr |= u16::from(self.op_val) << 8;
                Ok(OpState::Processing)
            }
            Tick::Tick4 => {
                // Turn this into FFbb and read from it.
                self.op_addr |= 0xFF00;
                _ = self.ram.borrow().read(self.op_addr);
                Ok(OpState::Processing)
            }
            Tick::Tick5 | Tick::Tick6 | Tick::Tick7 => {
                self.op_addr = 0xFFFF;
                _ = self.ram.borrow().read(self.op_addr);
                Ok(OpState::Processing)
            }
            Tick::Tick8 => {
                _ = self.ram.borrow().read(self.op_addr);
                Ok(OpState::Done)
            }
        }
    }

    // phx implements the PHX instruction for pushing X onto the stack.
    // Returns Done when done and/or errors.
    fn phx(&mut self) -> Result<OpState> {
        self.push_register(self.x().0)
    }

    // phy implements the PHY instruction for pushing Y onto the stack.
    // Returns Done when done and/or errors.
    fn phy(&mut self) -> Result<OpState> {
        self.push_register(self.y().0)
    }

    // plx implements the PLX instruction for pulling X from the stack.
    // Returns Done when done and/or errors.
    fn plx(&mut self) -> Result<OpState> {
        self.pull_register(Register::X)
    }

    // ply implements the PLY instruction for pulling Y from the stack.
    // Returns Done when done and/or errors.
    fn ply(&mut self) -> Result<OpState> {
        self.pull_register(Register::Y)
    }

    // rmb implements all RMB instructions for clearing bit X at the given ZP location
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn rmb(&mut self) -> Result<OpState> {
        // The MSB nibble is the bit to use.
        let loc = (self.op_raw() & 0xF0) >> 4;
        let mask = 1 << loc;
        let mask = !mask;
        self.ram()
            .borrow_mut()
            .write(self.op_addr(), self.op_val() & mask);
        Ok(OpState::Done)
    }

    // smb implements all SMB instructions for setting bit X at the given ZP location
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn smb(&mut self) -> Result<OpState> {
        // The MSB nibble is the bit to use.
        let loc = ((self.op_raw & 0xF0) >> 4) - 0x08;
        let mask = 1 << loc;
        self.ram
            .borrow_mut()
            .write(self.op_addr, self.op_val | mask);
        Ok(OpState::Done)
    }

    // trb implements for testing and setting bits.
    // This modifies a memory location based on the value in A.
    // Z is set based on loc&A
    // The memory location is set based on loc&(A^0xFF)
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn trb(&mut self) -> Result<OpState> {
        self.zero_check(self.a.0 & self.op_val);
        self.ram
            .borrow_mut()
            .write(self.op_addr, (self.a.0 ^ 0xFF) & self.op_val);
        Ok(OpState::Done)
    }

    // tsb implements for testing and setting bits.
    // This modifies a memory location based on the value in A.
    // Z is set based on loc&A
    // The memory location is set based on loc|A
    // Always returns Done since this takes one tick and never returns an error.
    #[allow(clippy::unnecessary_wraps)]
    fn tsb(&mut self) -> Result<OpState> {
        self.zero_check(self.a.0 & self.op_val);
        self.ram
            .borrow_mut()
            .write(self.op_addr, self.a.0 | self.op_val);
        Ok(OpState::Done)
    }

    // wai implements the WAI instruction which pauses the CPU until an interrupt occurs.
    fn wai(&mut self) -> Result<OpState> {
        match self.op_tick {
            Tick::Reset
            | Tick::Tick1
            | Tick::Tick4
            | Tick::Tick5
            | Tick::Tick6
            | Tick::Tick7
            | Tick::Tick8 => Err(eyre!("wait: invalid op_tick: {:?}", self.op_tick)),
            Tick::Tick2 => {
                // All that happens here is a bus read of the next instruction
                // which already happened in tick(). But we'll do it again in tick3.
                Ok(OpState::Processing)
            }
            Tick::Tick3 => {
                // Read the op_val byte again and leave PC alone for when we wake up.
                self.op_val = self.ram.borrow().read(self.pc.0);
                self.state = State::WaitingForInterrupt;
                Ok(OpState::Done)
            }
        }
    }
}

/// Define the characteristics of the 6502 wanted.
pub struct ChipDef<'a> {
    /// Memory implementation.
    pub ram: Box<dyn Memory>,

    /// irq is an optional IRQ source to trigger the IRQ line.
    pub irq: Option<&'a dyn irq::Sender>,

    /// nmi is an optional IRQ source to trigger the NMI line.
    pub nmi: Option<&'a dyn irq::Sender>,

    /// rdy is an optional IRQ source to trigger the RDY line (which halts the CPU).
    /// This is not technically an IRQ but acts the same.
    pub rdy: Option<&'a dyn irq::Sender>,
}

impl<'a> Default for ChipDef<'a> {
    fn default() -> Self {
        Self {
            ram: Box::new([0u8; MAX_SIZE]),
            irq: None,
            nmi: None,
            rdy: None,
        }
    }
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
// A C6510RAM handles the I/O port mapped in as address 0x0000 and 0x0001
// while maintaining the underlying Memory implementation as a pass through
// for all other addresses.
struct C6510RAM {
    // The current state of address 0x0000
    output_0x00: u8,

    // The current state of address 0x0001
    output_0x01: u8,

    // Use interior mutability so the caller can get a reference
    // to this to return state but we can also update from the port addresses
    // get updated.
    io_state: Rc<RefCell<[io::Style; 6]>>,

    // The input state as passed in on construction (generally pullup/down).
    input_io: [io::Style; 6],

    // The underlying RAM implementation.
    ram: Box<dyn Memory>,

    // A memory block we can return. Need interior mutability to avoid
    // allocating a new one each time but `ram` is not a mut function so
    // to build this requires mutating there by combining our 2 bytes plus
    // the underlying RAM together.
    memory: RefCell<[u8; MAX_SIZE]>,
}

impl C6510RAM {
    // Create a new RAM impl for a 6510 with the given hard coded input lines.
    fn new(ram: Box<dyn Memory>, input_dest: [io::Style; 6]) -> Self {
        Self {
            // Defaults to ports set to input.
            output_0x00: 0x00,
            output_0x01: 0x00,
            io_state: Rc::new(RefCell::new(input_dest)),
            input_io: input_dest,
            ram,
            memory: RefCell::new([0; MAX_SIZE]), // Only here to provide a copy for ram()
        }
    }
}

impl Memory for C6510RAM {
    // `read` will pass through to the underlying Memory except for the first
    // 2 addresses which are maintained internally. This means without another
    // reference to that Memory implementation those 2 locations are forever hidden.
    fn read(&self, addr: u16) -> u8 {
        match addr {
            // This means by default both of these locations are not accessible
            // in the underlying RAM.
            0x0000 => self.output_0x00,
            0x0001 => self.output_0x01,
            _ => self.ram.read(addr),
        }
    }

    // `write` either writes to the underlying Memory or if addresses 0x0000 or 0x0001
    // are chosen will change the state of the I/O lines in response.
    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            // This means by default both of these locations are not accessible
            // in the underlying RAM.
            0x0000 => {
                self.output_0x00 = val;
                // Since direction possibly changed run through all the relevant
                // bits and determine direction. Either point at the previously
                // wired up input sink or depending on output bit state pull
                // high or low directly.
                for i in 0..6 {
                    // If it's not set this is an input.
                    if val & (1 << i) == 0x00 {
                        self.io_state.borrow_mut()[i] = self.input_io[i];
                    } else {
                        // If the output bit is set reflect in io_state
                        if self.output_0x01 & (1 << i) == 0x00 {
                            self.io_state.borrow_mut()[i] = io::Style::Out(&io::OutputLow {});
                        } else {
                            self.io_state.borrow_mut()[i] = io::Style::Out(&io::OutputHigh {});
                        }
                    }
                }
            }
            0x0001 => {
                self.output_0x01 = val;
                // Output state may have changed and if the direction is output
                // as well need to reflect that.
                for i in 0..6 {
                    // If it's an input we don't care and skip.
                    if self.output_0x00 & (1 << i) == 0x00 {
                        continue;
                    }
                    // If the output is low set the correspondning pin to low.
                    if val & (1 << i) == 0x00 {
                        self.io_state.borrow_mut()[i] = io::Style::Out(&io::OutputLow {});
                    } else {
                        self.io_state.borrow_mut()[i] = io::Style::Out(&io::OutputHigh {});
                    }
                }
            }
            _ => {
                self.ram.write(addr, val);
            }
        }
    }

    /// `power_on` will perform power on behavior. For `c6510RAM` this will
    /// reset the i/o port to input only and then reset the underlying ram.
    fn power_on(&mut self) {
        self.output_0x00 = 0x00;
        self.output_0x01 = 0x00;
        self.ram.power_on();
    }

    /// `ram` gives a copy of `FlatRAM` back out as an array.
    fn ram(&self, dest: &mut [u8; MAX_SIZE]) {
        self.ram.ram(&mut self.memory.borrow_mut());
        self.memory.borrow_mut()[0x00] = self.output_0x00;
        self.memory.borrow_mut()[0x01] = self.output_0x01;
        *dest = *self.memory.borrow();
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
    /// `read` returns the value at the given address.
    fn read(&self, addr: u16) -> u8 {
        if self.debug {
            println!("read: {addr:04X}: {:02X}", self.memory[usize::from(addr)]);
        }
        self.memory[usize::from(addr)]
    }

    /// `write` sets the value at the given address.
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

    /// `ram` gives a copy of `FlatRAM` back out as an array.
    fn ram(&self, dest: &mut [u8; MAX_SIZE]) {
        *dest = self.memory;
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
