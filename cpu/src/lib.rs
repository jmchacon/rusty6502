//! cpu defines a 6502 CPU which is clock accurate to the supporting environment.
use std::fmt;
use strum_macros::{EnumIter, EnumString};

mod lookup;
pub use crate::lookup::*;

/// `AddressMode` defines the 6502 addressing modes.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, EnumString)]
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

/// `Opcode` defines all the unique 6502 opcodes.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, EnumIter, EnumString)]
pub enum Opcode {
    /// Add with Carry A with the value at the operand address.
    #[strum(ascii_case_insensitive)]
    ADC,

    /// Undocumented opcode AHX. This stores a value as (A & X & (ADDR_HI + 1)).
    #[strum(ascii_case_insensitive)]
    AHX,

    /// Undocumented opcode ALR. This does AND #i and then LSR setting all associated flags
    #[strum(ascii_case_insensitive)]
    ALR,

    /// Undocumented opcode ANC. This does AND #i and then sets carry based on bit 7 (sign extend).
    #[strum(ascii_case_insensitive)]
    ANC,

    /// Bitwise and operation.
    #[strum(ascii_case_insensitive)]
    AND,

    /// Undocumented opcode ARR. This does AND #i and then ROR except some flags are set differently.
    #[strum(ascii_case_insensitive)]
    ARR,

    /// Arithmetic shift left with possibly carry bit set as a result.
    #[strum(ascii_case_insensitive)]
    ASL,

    /// Undocumented opcode AXS. This does (A AND X) - operand (no borrow) setting all associated flags post SBC.
    #[strum(ascii_case_insensitive)]
    AXS,

    /// Branch if carry is clear.
    #[strum(ascii_case_insensitive)]
    BCC,

    /// Branch if carry is set.
    #[strum(ascii_case_insensitive)]
    BCS,

    /// Branch if equal (Z is set).
    #[strum(ascii_case_insensitive)]
    BEQ,

    /// Bit test by AND'ing against A and setting N/V based on the value.
    #[strum(ascii_case_insensitive)]
    BIT,

    /// Branch on minus (N is set)
    #[strum(ascii_case_insensitive)]
    BMI,

    /// Branch if not equal (Z is clear)
    #[strum(ascii_case_insensitive)]
    BNE,

    /// Branch on plus (N is clear)
    #[strum(ascii_case_insensitive)]
    BPL,

    /// Break execution. Same as an IRQ but software defined. B bit is set in P on stack to indicate source.
    #[strum(ascii_case_insensitive)]
    #[default]
    BRK,

    /// Branch if overflow (V) is clear.
    #[strum(ascii_case_insensitive)]
    BVC,

    /// Branch if overflow (V) is set.
    #[strum(ascii_case_insensitive)]
    BVS,

    /// Clear the C flag.
    #[strum(ascii_case_insensitive)]
    CLC,

    /// Clear the D flag.
    #[strum(ascii_case_insensitive)]
    CLD,

    /// Clear the I flag.
    #[strum(ascii_case_insensitive)]
    CLI,

    /// Clear the V flag.
    #[strum(ascii_case_insensitive)]
    CLV,

    /// Compare values with A setting Z based on whether they are equal or not.
    #[strum(ascii_case_insensitive)]
    CMP,

    /// Compare values with X setting Z based on whether they are equal or not.
    #[strum(ascii_case_insensitive)]
    CPX,

    /// Compare values with Y setting Z based on whether they are equal or not.
    #[strum(ascii_case_insensitive)]
    CPY,

    /// Undocumented opcode DCP. This decrements the value at the operand address and then does a CMP with A setting associated flags.
    #[strum(ascii_case_insensitive)]
    DCP,

    /// Decrements the value at the operand address.
    #[strum(ascii_case_insensitive)]
    DEC,

    /// Decrements the X register.
    #[strum(ascii_case_insensitive)]
    DEX,

    /// Decrements the Y register.
    #[strum(ascii_case_insensitive)]
    DEY,

    /// Exclusive OR (XOR) A with the value at the operand address.
    #[strum(ascii_case_insensitive)]
    EOR,

    /// Undocumented opcode HLT. This will halt the CPU (effectively internally locked up).
    #[strum(ascii_case_insensitive)]
    HLT,

    /// Increments the value at the operation address.
    #[strum(ascii_case_insensitive)]
    INC,

    /// Increments the X register.
    #[strum(ascii_case_insensitive)]
    INX,

    /// Increments the Y register.
    #[strum(ascii_case_insensitive)]
    INY,

    /// Undocumented opcode ISC. This increments the value at the operand address and then does an SBC with setting associated flags
    #[strum(ascii_case_insensitive)]
    ISC,

    /// Jumps to the value given by the operand address (16 bits).
    #[strum(ascii_case_insensitive)]
    JMP,

    /// Jumps to a subroutine given by the operand address (16 bits). Before jumping pushes PC onto the stack so RTS can return to the PC after the JSR instruction.
    #[strum(ascii_case_insensitive)]
    JSR,

    /// Undocumented opcode LAS. This take the value at the operand address and ANDs it with S and then stores that in A,X,S setting flags accordingly.
    #[strum(ascii_case_insensitive)]
    LAS,

    /// Undocumented opcode LAX.  This loads A and X with the same value from the operand address.
    #[strum(ascii_case_insensitive)]
    LAX,

    /// Loads the A register from the value at the operand address.
    #[strum(ascii_case_insensitive)]
    LDA,

    /// Loads the X register from the value at the operand address.
    #[strum(ascii_case_insensitive)]
    LDX,

    /// Loads the Y register from the value at the operand address.
    #[strum(ascii_case_insensitive)]
    LDY,

    /// Logical shift right of the value at the operand address or the A register. Bit 0 is shifted into the C flag.
    #[strum(ascii_case_insensitive)]
    LSR,

    /// No operation. Simply burns clock cycles depending on addressing mode.
    #[strum(ascii_case_insensitive)]
    NOP,

    /// Undocumented opcode OAL. This one acts a bit randomly. It somtimes does XAA and sometimes does A=X=A&val.
    #[strum(ascii_case_insensitive)]
    OAL,

    /// ORs the value in A with the value at the operand address.
    #[strum(ascii_case_insensitive)]
    ORA,

    /// Pushes A onto the stack.
    #[strum(ascii_case_insensitive)]
    PHA,

    /// Pushes P onto the stack.
    #[strum(ascii_case_insensitive)]
    PHP,

    /// Pulls A from the stack.
    #[strum(ascii_case_insensitive)]
    PLA,

    /// Pulls P from the stack.
    #[strum(ascii_case_insensitive)]
    PLP,

    /// Undocumented opcode RLA. This does a ROL on the value at the operand address and then AND's it against A. Sets flags and carry.
    #[strum(ascii_case_insensitive)]
    RLA,

    /// Rotates left the value at the operand address or the A register. Bit 7 is shifted into the C flag and the C flag is shifted into bit 0.
    #[strum(ascii_case_insensitive)]
    ROL,

    /// Rotates right the value at the operand address or the A register. Bit 0 is shifted into the C register the C flag is shifted into bit 7.
    #[strum(ascii_case_insensitive)]
    ROR,

    /// Undocumented opcode RRA. This does a ROR on the value at the operand address and then ADC's it against A. Sets flags and carry.
    #[strum(ascii_case_insensitive)]
    RRA,

    /// Return from interuppt. Should be called when a handler for IRQ/NMI or BRK has been called through their respective vector.
    /// Pops P and PC off the stack and returns execution to the point pre interrupt. The main difference between this and RTS is P
    /// is also popped (and bits set) where-as RTS only deals with PC.
    #[strum(ascii_case_insensitive)]
    RTI,

    /// Return from subroutine. Pops the PC from the stack and sets execution to continue at that value.
    #[strum(ascii_case_insensitive)]
    RTS,

    /// Undocumented instruction SAX. Store A and X to the location specified in the operand address.
    #[strum(ascii_case_insensitive)]
    SAX,

    /// Subtract with carry the value in A with the value at the operand address.
    #[strum(ascii_case_insensitive)]
    SBC,

    /// Set the C flag.
    #[strum(ascii_case_insensitive)]
    SEC,

    /// Set the D flag.
    #[strum(ascii_case_insensitive)]
    SED,

    /// Set the I flag.
    #[strum(ascii_case_insensitive)]
    SEI,

    /// Undocumented instruction SHX. Similar to AHX but the value stored is (X & (ADDR_HI + 1))
    #[strum(ascii_case_insensitive)]
    SHX,

    /// Undocumented instruction SHX. Similar to AHX but the value stored is (Y & (ADDR_HI + 1))
    #[strum(ascii_case_insensitive)]
    SHY,

    /// Undocumented instruction SLO. This does an ASL on the value at the operand address and then OR's it against A. Sets flags and carry
    #[strum(ascii_case_insensitive)]
    SLO,

    /// Undocumented instruction SRE. This does a LSR on the value at the operand address and then EOR's it against A. Sets flags and carry.
    #[strum(ascii_case_insensitive)]
    SRE,

    /// Stores the A register at the operand address.
    #[strum(ascii_case_insensitive)]
    STA,

    /// Stores the X register at the operand address.
    #[strum(ascii_case_insensitive)]
    STX,

    /// Stores the Y register at the operand address.
    #[strum(ascii_case_insensitive)]
    STY,

    /// Undocumented instruction TAS. This does the same operations as AHX but then also sets S = A&X.
    #[strum(ascii_case_insensitive)]
    TAS,

    /// Loads the X register with the value of the A register.
    #[strum(ascii_case_insensitive)]
    TAX,

    /// Loads the Y register with the value of the A register.
    #[strum(ascii_case_insensitive)]
    TAY,

    /// Loads the X register with the value of the S register.
    #[strum(ascii_case_insensitive)]
    TSX,

    /// Loads the A register with the value of the X register.
    #[strum(ascii_case_insensitive)]
    TXA,

    /// Loads the S register with the value of the X register. No flags are set from S loads.
    #[strum(ascii_case_insensitive)]
    TXS,

    /// Loads the A register with the value of the Y register.
    #[strum(ascii_case_insensitive)]
    TYA,

    /// Undocumented instruction XAA. We'll go with http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_(XAA,_ANE)
    /// for implementation and pick 0xEE as the constant. According to VICE this may break so might need to change it to 0xFF
    /// https://sourceforge.net/tracker/?func=detail&aid=2110948&group_id=223021&atid=1057617
    #[strum(ascii_case_insensitive)]
    XAA,
}

#[derive(Debug, Copy, Clone)]
/// Operation defines an opcode plus its addressing mode together. This will be combined in hashes
/// with the actual u8 value as the key.
pub struct Operation {
    /// op is the Opcode such as ADC, LDA, etc.
    pub op: Opcode,
    /// AddressMode is a valid addressing mode for this opcode such as Absolute.
    pub mode: AddressMode,
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Display for AddressMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
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
