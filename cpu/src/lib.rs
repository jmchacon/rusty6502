//! cpu defines a 6502 CPU which is clock accurate to the supporting environment.

/// `AddressMode` defines the 6502 addressing modes.
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

/// `NMI_VECTOR` is the location in memory the 6502 uses for NMI interrupts.
/// It is a pointer to the location to start execution.
pub const NMI_VECTOR: u16 = 0xFFFA;

/// `RESET_VECTOR` is the location in memory the 6502 uses on startup to begin execution.
/// It is a pointer to the location to start execution.
pub const RESET_VECTOR: u16 = 0xFFFC;

/// `IRQ_VECTOR` is the location in memory the 6502 uses when executing an IRQ (BRK).
/// It is a pointer to the location to start execution.
pub const IRQ_VECTOR: u16 = 0xFFFE;
