//! cpu defines a 6502 CPU which is clock accurate to the supporting environment.

use lazy_static::lazy_static;
use std::{
    collections::{HashMap, HashSet},
    fmt,
};
use strum::IntoEnumIterator;
use strum_macros::{EnumIter, EnumString};

/// `AddressMode` defines the 6502 addressing modes.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, EnumString)]
pub enum AddressMode {
    /// `Immediate` mode uses the constant following the opcode to perform the operation.
    /// Example: LDA #04 loads 0x04 into A.
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, EnumIter, EnumString)]
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

lazy_static! {
    /// OPCODES is a hashmap of the Opcode -> Hashmap of valid addressing modes and their u8 opcode values.
    /// This is a vector since NOP, HLT and a few others duplicate address mode and can do the same thing from N values.
    /// An assembler should simply use the first value of each Vec unless they want to randomly chose.
    pub static ref OPCODES: HashMap<Opcode, HashMap<AddressMode, Vec<u8>>> = {
        let mut m = HashMap::new();

        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x8B];
        hm.insert(AddressMode::Immediate, v);
        let v: Vec<u8> = vec![0x6D];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x7D];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0x79];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0x69];
        hm.insert(AddressMode::Immediate, v);
        let v: Vec<u8> = vec![0x61];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0x71];
        hm.insert(AddressMode::IndirectY, v);
        let v: Vec<u8> = vec![0x65];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x75];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::ADC, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x9F];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0x93];
        hm.insert(AddressMode::IndirectY, v);
        m.insert(Opcode::AHX, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x4B];
        hm.insert(AddressMode::Immediate, v);
        m.insert(Opcode::ALR, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x0B, 0x2B];
        hm.insert(AddressMode::Immediate, v);
        m.insert(Opcode::ANC, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x2D];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x3D];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0x39];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0x29];
        hm.insert(AddressMode::Immediate, v);
        let v: Vec<u8> = vec![0x21];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0x31];
        hm.insert(AddressMode::IndirectY, v);
        let v: Vec<u8> = vec![0x25];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x35];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::AND, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x6B];
        hm.insert(AddressMode::Immediate, v);
        m.insert(Opcode::ARR, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x0E];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x1E];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0x0A];
        hm.insert(AddressMode::Implied, v);
        let v: Vec<u8> = vec![0x06];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x16];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::ASL, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xCB];
        hm.insert(AddressMode::Immediate, v);
        m.insert(Opcode::AXS, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x90];
        hm.insert(AddressMode::Relative, v);
        m.insert(Opcode::BCC, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xB0];
        hm.insert(AddressMode::Relative, v);
        m.insert(Opcode::BCS, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xF0];
        hm.insert(AddressMode::Relative, v);
        m.insert(Opcode::BEQ, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x2C];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x24];
        hm.insert(AddressMode::ZeroPage, v);
        m.insert(Opcode::BIT, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x30];
        hm.insert(AddressMode::Relative, v);
        m.insert(Opcode::BMI, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xD0];
        hm.insert(AddressMode::Relative, v);
        m.insert(Opcode::BNE, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x10];
        hm.insert(AddressMode::Relative, v);
        m.insert(Opcode::BPL, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x00];
        hm.insert(AddressMode::Immediate, v);
        m.insert(Opcode::BRK, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x50];
        hm.insert(AddressMode::Relative, v);
        m.insert(Opcode::BVC, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x70];
        hm.insert(AddressMode::Relative, v);
        m.insert(Opcode::BVS, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x18];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::CLC, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xD8];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::CLD, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x58];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::CLI, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xB8];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::CLV, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xCD];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0xDD];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0xD9];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0xC9];
        hm.insert(AddressMode::Immediate, v);
        let v: Vec<u8> = vec![0xC1];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0xD1];
        hm.insert(AddressMode::IndirectY, v);
        let v: Vec<u8> = vec![0xC5];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0xD5];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::CMP, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xEC];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0xE0];
        hm.insert(AddressMode::Immediate, v);
        let v: Vec<u8> = vec![0xE4];
        hm.insert(AddressMode::ZeroPage, v);
        m.insert(Opcode::CPX, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xCC];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0xC0];
        hm.insert(AddressMode::Immediate, v);
        let v: Vec<u8> = vec![0xC4];
        hm.insert(AddressMode::ZeroPage, v);
        m.insert(Opcode::CPY, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xCF];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0xDF];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0xDB];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0xC3];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0xD3];
        hm.insert(AddressMode::IndirectY, v);
        let v: Vec<u8> = vec![0xC7];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0xD7];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::DCP, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xCE];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0xDE];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0xC6];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0xD6];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::DEC, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xCA];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::DEX, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x88];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::DEY, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x4D];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x5D];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0x59];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0x49];
        hm.insert(AddressMode::Immediate, v);
        let v: Vec<u8> = vec![0x41];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0x51];
        hm.insert(AddressMode::IndirectY, v);
        let v: Vec<u8> = vec![0x45];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x55];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::EOR, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92, 0xB2, 0xD2, 0xF2];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::HLT, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xEE];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0xFE];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0xE6];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0xF6];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::INC, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xE8];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::INX, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xC8];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::INY, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xEF];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0xFF];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0xFB];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0xE3];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0xF3];
        hm.insert(AddressMode::IndirectY, v);
        let v: Vec<u8> = vec![0xE7];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0xF7];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::ISC, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x4C];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x6C];
        hm.insert(AddressMode::Indirect, v);
        m.insert(Opcode::JMP, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x20];
        hm.insert(AddressMode::Absolute, v);
        m.insert(Opcode::JSR, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xBB];
        hm.insert(AddressMode::AbsoluteY, v);
        m.insert(Opcode::LAS, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xAF];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0xBF];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0xA3];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0xB3];
        hm.insert(AddressMode::IndirectY, v);
        let v: Vec<u8> = vec![0xA7];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0xB7];
        hm.insert(AddressMode::ZeroPageY, v);
        m.insert(Opcode::LAX, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xAD];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0xBD];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0xB9];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0xA9];
        hm.insert(AddressMode::Immediate, v);
        let v: Vec<u8> = vec![0xA1];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0xB1];
        hm.insert(AddressMode::IndirectY, v);
        let v: Vec<u8> = vec![0xA5];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0xB5];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::LDA, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xAE];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0xBE];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0xA2];
        hm.insert(AddressMode::Immediate, v);
        let v: Vec<u8> = vec![0xA6];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0xB6];
        hm.insert(AddressMode::ZeroPageY, v);
        m.insert(Opcode::LDX, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xAC];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0xBC];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0xA0];
        hm.insert(AddressMode::Immediate, v);
        let v: Vec<u8> = vec![0xA4];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0xB4];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::LDY, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x4E];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x5E];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0x4A];
        hm.insert(AddressMode::Implied, v);
        let v: Vec<u8> = vec![0x46];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x56];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::LSR, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x0C];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x1C, 0x3C, 0x5C, 0x7C, 0xDC, 0xFC];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0x80, 0x82, 0x89, 0xC2, 0xE2];
        hm.insert(AddressMode::Immediate, v);
        let v: Vec<u8> = vec![0x1A, 0x3A, 0x5A, 0x7A, 0xDA, 0xEA, 0xFA];
        hm.insert(AddressMode::Implied, v);
        let v: Vec<u8> = vec![0x04, 0x44, 0x64];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x14, 0x34, 0x54, 0x74, 0xD4, 0xF4];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::NOP, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xAB];
        hm.insert(AddressMode::Immediate, v);
        m.insert(Opcode::OAL, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x0D];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x1D];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0x19];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0x09];
        hm.insert(AddressMode::Immediate, v);
        let v: Vec<u8> = vec![0x01];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0x11];
        hm.insert(AddressMode::IndirectY, v);
        let v: Vec<u8> = vec![0x05];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x15];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::ORA, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x48];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::PHA, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x08];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::PHP, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x68];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::PLA, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x28];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::PLP, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x2F];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x3F];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0x3B];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0x23];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0x33];
        hm.insert(AddressMode::IndirectY, v);
        let v: Vec<u8> = vec![0x27];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x37];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::RLA, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x2E];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x3E];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0x2A];
        hm.insert(AddressMode::Implied, v);
        let v: Vec<u8> = vec![0x26];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x36];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::ROL, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x6E];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x7E];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0x6A];
        hm.insert(AddressMode::Implied, v);
        let v: Vec<u8> = vec![0x66];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x76];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::ROR, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x6F];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x7F];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0x7B];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0x63];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0x73];
        hm.insert(AddressMode::IndirectY, v);
        let v: Vec<u8> = vec![0x67];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x77];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::RRA, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x40];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::RTI, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x60];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::RTS, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x8F];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x83];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0x87];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x97];
        hm.insert(AddressMode::ZeroPageY, v);
        m.insert(Opcode::SAX, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xED];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0xFD];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0xF9];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0xE9, 0xEB];
        hm.insert(AddressMode::Immediate, v);
        let v: Vec<u8> = vec![0xE1];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0xF1];
        hm.insert(AddressMode::IndirectY, v);
        let v: Vec<u8> = vec![0xE5];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0xF5];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::SBC, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x38];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::SEC, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xF8];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::SED, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x78];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::SEI, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x9E];
        hm.insert(AddressMode::AbsoluteY, v);
        m.insert(Opcode::SHX, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x9C];
        hm.insert(AddressMode::AbsoluteX, v);
        m.insert(Opcode::SHY, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x0F];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x1F];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0x1B];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0x03];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0x13];
        hm.insert(AddressMode::IndirectY, v);
        let v: Vec<u8> = vec![0x07];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x17];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::SLO, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x4F];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x5F];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0x5B];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0x43];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0x53];
        hm.insert(AddressMode::IndirectY, v);
        let v: Vec<u8> = vec![0x47];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x57];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::SRE, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x8D];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x9D];
        hm.insert(AddressMode::AbsoluteX, v);
        let v: Vec<u8> = vec![0x99];
        hm.insert(AddressMode::AbsoluteY, v);
        let v: Vec<u8> = vec![0x81];
        hm.insert(AddressMode::IndirectX, v);
        let v: Vec<u8> = vec![0x91];
        hm.insert(AddressMode::IndirectY, v);
        let v: Vec<u8> = vec![0x85];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x95];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::STA, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x8E];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x86];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x96];
        hm.insert(AddressMode::ZeroPageY, v);
        m.insert(Opcode::STX, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x8C];
        hm.insert(AddressMode::Absolute, v);
        let v: Vec<u8> = vec![0x84];
        hm.insert(AddressMode::ZeroPage, v);
        let v: Vec<u8> = vec![0x94];
        hm.insert(AddressMode::ZeroPageX, v);
        m.insert(Opcode::STY, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x9B];
        hm.insert(AddressMode::AbsoluteY, v);
        m.insert(Opcode::TAS, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xAA];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::TAX, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xA8];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::TAY, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0xBA];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::TSX, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x8A];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::TXA, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x9A];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::TXS, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x98];
        hm.insert(AddressMode::Implied, v);
        m.insert(Opcode::TYA, hm);
        let mut hm = HashMap::new();
        let v: Vec<u8> = vec![0x8B];
        hm.insert(AddressMode::Immediate, v);
        m.insert(Opcode::XAA, hm);

        for op in Opcode::iter() {
            match m.get(&op) {
                Some(_) => {},
                None => panic!("Not all opcodes covered!. Missing {op}"),
            };
        }

        m
    };


    /// OPCODES_VALUES is the inverse of OPCODES where the keys are the u8 byte codes and values Operation defining
    /// the Opcode and AddressMode. Used in processing the CPU tick() or in disassembly for mapping a byte code back
    /// to an Opcode.
    pub static ref OPCODES_VALUES: Vec<Operation> = {
        // We know this much be a vector of all u8 values since the 6502
        // has behavior at each so we'll have some combo of opcode/addressmode.
        //
        // Preallocate a vector of that size and fill with placeholders.
        // Then track in the hashset which indexes we've seen (panic on dups)
        // and insert directly to each index.
        let mut m = Vec::new();
        m.resize(1 << 8, Operation{
            op: Opcode::BRK,
            mode: AddressMode::Implied,
        });
        let sl = m.as_mut_slice();
        let mut hs = HashSet::new();

        for (op, hm) in OPCODES.iter() {
            for (am, opbytes) in hm {
                for opbyte in opbytes {
                    assert!(!hs.contains(opbyte),"OPCODES contains multiple entries for {opbyte:#04X} found in opcode {op} but we already have {:?}", sl[usize::from(*opbyte)]);
                    hs.insert(*opbyte);
                    sl[usize::from(*opbyte)] = Operation{
                                op: *op,
                                mode: *am,
                    };
                }
            }
        }

        assert!(hs.len() == (1 << 8), "Didn't fill out {} opcodes. Only defined {} - {:?}", 1<<8, hs.len(), m);
        m
    };
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
