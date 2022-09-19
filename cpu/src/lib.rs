//! cpu defines a 6502 CPU which is clock accurate to the supporting environment.
use memory::{Memory, MAX_SIZE};

use color_eyre::eyre::{eyre, Result};
use rand::Rng;
use strum_macros::{Display, EnumIter, EnumString};

mod lookup;
pub use crate::lookup::*;

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
#[derive(Copy, Clone, Debug, Display, EnumString)]
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

#[derive(Debug, Copy, Clone)]
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

// Used to indicate whether an opcode/addressing mode is done or not.
enum OpState {
    Done,
    Processing,
}

// Clock ticks
#[derive(Debug, Display, Copy, Clone, PartialEq, EnumString)]
enum Tick {
    // The reset state. Used to start a new instruction assuming all
    // processing immediately calls `next` before evaluatin.
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
    pub a: u8,

    /// X register
    pub x: u8,

    /// Y register
    pub y: u8,

    /// Stack pointer
    pub s: u8,

    /// Status register
    pub p: u8,

    /// Program counter
    pub pc: u16,

    // If true `debug` will return data.
    debug: bool,

    // Initialized or not.
    state: CPUState,

    // Tracking for reset when we need to clear the extra clocks
    // up front before simulating BRK. If `tick` is called and this
    // isn't in Tick::Reset an error will result.
    reset_tick: Tick,

    // Total number of clock cycles since start.
    clocks: usize,

    // Memory implementation
    ram: &'a dyn Memory,

    // The current working opcode
    op: u8,

    // The 1st byte argument after the opcode (all instruction have this).
    // Often used as a temp value while building the whole instruction.
    op_val: u8,

    // Tick number for internal operation of opcode.
    op_tick: Tick,

    // Address computed during opcode to be used for read/write (indirect, etc modes).
    op_addr: u16,

    // Stays OpState::Processing until the current opcode has completed all ticks.
    op_done: OpState,

    // Stays OpState::Processing until the current opcode has completed any addressing mode ticks.
    addr_done: OpState,

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
    pub ram: &'a dyn Memory,

    /// If true debugging is enabled.
    /// TODO: Define the interface for this.
    pub debug: bool,
}

impl<'a> Cpu<'a> {
    /// Build a new Cpu.
    /// NOTE: This is not usable at this point. Call `power_on` to begin
    /// operation. Anything else will return errors.
    #[must_use]
    pub fn new(def: &'a ChipDef) -> Self {
        Cpu {
            cpu_type: def.cpu_type,
            a: 0x00,
            x: 0x00,
            y: 0x00,
            s: 0x00,
            p: 0x00,
            pc: 0x00,
            debug: def.debug,
            state: CPUState::Off,
            clocks: 0,
            ram: def.ram,
            op: 0x00,
            op_val: 0x00,
            op_tick: Tick::Reset,
            reset_tick: Tick::Reset,
            op_addr: 0x0000,
            op_done: OpState::Done,
            addr_done: OpState::Done,
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

        // Use reset to get everything else done.
        loop {
            match self.reset() {
                Ok(true) => break,
                Ok(false) => {}
                Err(e) => return Err(eyre!("{e}")),
            }
        }
        self.state = CPUState::On;
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
                Ok(false)
            }
            Tick::Tick3 => {
                match self.op_tick {
                    Tick::Tick1 => {
                        // Standard first tick reads current PC value which normally
                        // is the opcode but we discard.
                        self.ram.read(self.pc);
                        self.pc += 1;

                        // Reset our other internal state

                        // If we were halted before, clear that out.
                        self.state = CPUState::Running;
                        self.halt_opcode = 0x00;
                        self.halt_pc = 0x0000;

                        // The stack ends up at 0xFD which implies it gets set to 0x00 now
                        // as we pull 3 bytes off the stack in the end.
                        // TODO: Double check this in visual 6502.
                        self.s = 0x00;
                        Ok(false)
                    }
                    Tick::Tick2 => {
                        // Read another throw away value which is normally the opval but
                        // discarded as well.
                        self.ram.read(self.pc);
                        self.pc += 1;
                        Ok(false)
                    }
                    Tick::Tick3 | Tick::Tick4 => {
                        // Tick3: Simulate pushing P onto stack but reset holds R/W high so we don't write.
                        // Tick4: Simulate writing low byte of PC onto stack. Same rules as Tick3.
                        // These reads go nowhere (technically they end up in internal regs but since that's
                        // not visible externally, who cares?).
                        let addr: u16 = 0x0100 + u16::from(self.s);
                        self.ram.read(addr);
                        self.s -= 1;
                        Ok(false)
                    }
                    Tick::Tick5 => {
                        // Final write to stack (PC high) but actual read due to being in reset.
                        let addr: u16 = 0x0100 + u16::from(self.s);
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
                        self.pc = (u16::from(self.ram.read(RESET_VECTOR + 1)) << 8)
                            + u16::from(self.op_val);
                        self.reset_tick = Tick::Reset;
                        self.op_tick = Tick::Reset;
                        self.state = CPUState::Running;
                        Ok(true)
                    }
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
    pub fn tick(&mut self) -> Result<bool> {
        // Handles improper state. Only tick_done() or reset() can move into this state.
        if self.state != CPUState::Running {
            return Err(eyre!(
                "Cannot tick except in Running state - {}",
                self.state
            ));
        }
        // Move to the state tick_done() has to move us out of.
        self.state = CPUState::Tick;

        if self.reset_tick != Tick::Reset {
            return Err(eyre!(
                "Cannot tick if reset_tick is currently in progress - {}",
                self.reset_tick
            ));
        }

        // Always bump the clock count and op_tick.
        self.clocks += 1;
        self.op_tick = self.op_tick.next();

        match self.op_tick {
            Tick::Tick1 => {
                // If we're in 1 this means start a new instruction based on the PC value so grab
                // the opcode now.
                self.op = self.ram.read(self.pc);

                // Move out of the done state.
                self.op_done = OpState::Processing;
                self.addr_done = OpState::Processing;
                return Ok(false);
            }
            Tick::Tick2 => {
                // All instructions fetch the value after the opcode (though some like BRK/PHP/etc ignore it).
                // We keep it since some instructions such as absolute addr then require getting one
                // more byte. So cache at this stage since we no idea if it's needed.
                // NOTE: the PC doesn't increment here as that's dependent on addressing mode which will handle it.
                self.op_val = self.ram.read(self.pc);
            }
            // The remainder don't do anything and no need to test Reset state as next() above has to have moved
            // out of it.
            _ => {}
        }
        return Ok(true);
    }

    /// `tick_done` moves the CPU back to a state where the next tick can run.
    /// For the 6502 there are no internal latches so generally there is no shadow
    /// state to account but all Chip implementations need this function.
    pub fn tick_done(&mut self) -> Result<()> {
        self.state = CPUState::Running;
        Ok(())
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
