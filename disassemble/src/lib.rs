use rusty6502::prelude::*;
use std::fmt::Write as _;
use std::num::Wrapping; // import without risk of name clashing

/// step will take the given pc and Memory implementation and disassemble the segment
/// at that location. It will return a string of the dissembly as well as the next pc
/// to continue disassembling.
/// As a real 6502 will wrap around if it's asked to step off the end
/// this will do the same. i.e. disassembling 0xFFFF with a multi-byte opcode will result
/// in reading 0x0000 and 0x0001 and returning a pc from that area as well.
#[allow(clippy::too_many_lines)]
pub fn step(pc: Wrapping<u16>, r: &impl Memory) -> (String, Wrapping<u16>) {
    let pc1 = r.read((pc + Wrapping(1)).0);
    let pc2 = r.read((pc + Wrapping(2)).0);

    // Sign extend a 16 bit value so it can be added to PC for branch offsets
    #[allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
    let pc116 = Wrapping(i16::from(pc1 as i8) as u16);
    let op = r.read(pc.0);

    let (opcode, mode) = match op {
        0x00 => ("BRK", AddressMode::Immediate), // Ok, not really but the byte after BRK is read and skipped.
        0x01 => ("ORA", AddressMode::IndirectX),
        0x02 | 0x12 | 0x22 => ("HLT", AddressMode::Implied),
        0x03 => ("SLO", AddressMode::IndirectX),
        0x04 | 0x44 | 0x64 => ("NOP", AddressMode::ZeroPage),
        0x05 => ("ORA", AddressMode::ZeroPage),
        0x06 => ("ASL", AddressMode::ZeroPage),
        0x07 => ("SLO", AddressMode::ZeroPage),
        0x08 => ("PHP", AddressMode::Implied),
        0x09 => ("ORA", AddressMode::Immediate),
        0x0A => ("ASL", AddressMode::Implied),
        0x0B | 0x2B => ("ANC", AddressMode::Immediate),
        0x0C => ("NOP", AddressMode::Absolute),
        0x0D => ("ORA", AddressMode::Absolute),
        0x0E => ("ASL", AddressMode::Absolute),
        0x0F => ("SLO", AddressMode::Absolute),
        0x10 => ("BPL", AddressMode::Relative),
        0x11 => ("ORA", AddressMode::IndirectY),
        0x13 => ("SLO", AddressMode::IndirectY),
        0x14 | 0x34 | 0x54 | 0x74 | 0xD4 | 0xF4 => ("NOP", AddressMode::ZeroPageX),
        0x15 => ("ORA", AddressMode::ZeroPageX),
        0x16 => ("ASL", AddressMode::ZeroPageX),
        0x17 => ("SLO", AddressMode::ZeroPageX),
        0x18 => ("CLC", AddressMode::Implied),
        0x19 => ("ORA", AddressMode::AbsoluteY),
        0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xEA | 0xFA => ("NOP", AddressMode::Implied),
        0x1B => ("SLO", AddressMode::AbsoluteY),
        0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => ("NOP", AddressMode::AbsoluteX),
        0x1D => ("ORA", AddressMode::AbsoluteX),
        0x1E => ("ASL", AddressMode::AbsoluteX),
        0x1F => ("SLO", AddressMode::AbsoluteX),
        0x20 => ("JSR", AddressMode::Absolute),
        0x21 => ("AND", AddressMode::IndirectX),
        0x23 => ("RLA", AddressMode::IndirectX),
        0x24 => ("BIT", AddressMode::ZeroPage),
        0x25 => ("AND", AddressMode::ZeroPage),
        0x26 => ("ROL", AddressMode::ZeroPage),
        0x27 => ("RLA", AddressMode::ZeroPage),
        0x28 => ("PLP", AddressMode::Implied),
        0x29 => ("AND", AddressMode::Immediate),
        0x2A => ("ROL", AddressMode::Implied),
        0x2C => ("BIT", AddressMode::Absolute),
        0x2D => ("AND", AddressMode::Absolute),
        0x2E => ("ROL", AddressMode::Absolute),
        0x2F => ("RLA", AddressMode::Absolute),
        0x30 => ("BMI", AddressMode::Relative),
        0x31 => ("AND", AddressMode::IndirectY),
        0x32 | 0x42 | 0x52 | 0x62 | 0x72 | 0x92 | 0xB2 | 0xD2 | 0xF2 => {
            ("HLT", AddressMode::Implied)
        }
        0x33 => ("RLA", AddressMode::IndirectY),
        0x35 => ("AND", AddressMode::ZeroPageX),
        0x36 => ("ROL", AddressMode::ZeroPageX),
        0x37 => ("RLA", AddressMode::ZeroPageX),
        0x38 => ("SEC", AddressMode::Implied),
        0x39 => ("AND", AddressMode::AbsoluteY),
        0x3B => ("RLA", AddressMode::AbsoluteY),
        0x3D => ("AND", AddressMode::AbsoluteX),
        0x3E => ("ROL", AddressMode::AbsoluteX),
        0x3F => ("RLA", AddressMode::AbsoluteX),
        0x40 => ("RTI", AddressMode::Implied),
        0x41 => ("EOR", AddressMode::IndirectX),
        0x43 => ("SRE", AddressMode::IndirectX),
        0x45 => ("EOR", AddressMode::ZeroPage),
        0x46 => ("LSR", AddressMode::ZeroPage),
        0x47 => ("SRE", AddressMode::ZeroPage),
        0x48 => ("PHA", AddressMode::Implied),
        0x49 => ("EOR", AddressMode::Immediate),
        0x4A => ("LSR", AddressMode::Implied),
        0x4B => ("ALR", AddressMode::Immediate),
        0x4C => ("JMP", AddressMode::Absolute),
        0x4D => ("EOR", AddressMode::Absolute),
        0x4E => ("LSR", AddressMode::Absolute),
        0x4F => ("SRE", AddressMode::Absolute),
        0x50 => ("BVC", AddressMode::Relative),
        0x51 => ("EOR", AddressMode::IndirectY),
        0x53 => ("SRE", AddressMode::IndirectY),
        0x55 => ("EOR", AddressMode::ZeroPageX),
        0x56 => ("LSR", AddressMode::ZeroPageX),
        0x57 => ("SRE", AddressMode::ZeroPageX),
        0x58 => ("CLI", AddressMode::Implied),
        0x59 => ("EOR", AddressMode::AbsoluteY),
        0x5B => ("SRE", AddressMode::AbsoluteY),
        0x5D => ("EOR", AddressMode::AbsoluteX),
        0x5E => ("LSR", AddressMode::AbsoluteX),
        0x5F => ("SRE", AddressMode::AbsoluteX),
        0x60 => ("RTS", AddressMode::Implied),
        0x61 => ("ADC", AddressMode::IndirectX),
        0x63 => ("RRA", AddressMode::IndirectX),
        0x65 => ("ADC", AddressMode::ZeroPage),
        0x66 => ("ROR", AddressMode::ZeroPage),
        0x67 => ("RRA", AddressMode::ZeroPage),
        0x68 => ("PLA", AddressMode::Implied),
        0x69 => ("ADC", AddressMode::Immediate),
        0x6A => ("ROR", AddressMode::Implied),
        0x6B => ("ARR", AddressMode::Immediate),
        0x6C => ("JMP", AddressMode::Indirect),
        0x6D => ("ADC", AddressMode::Absolute),
        0x6E => ("ROR", AddressMode::Absolute),
        0x6F => ("RRA", AddressMode::Absolute),
        0x70 => ("BVS", AddressMode::Relative),
        0x71 => ("ADC", AddressMode::IndirectY),
        0x73 => ("RRA", AddressMode::IndirectY),
        0x75 => ("ADC", AddressMode::ZeroPageX),
        0x76 => ("ROR", AddressMode::ZeroPageX),
        0x77 => ("RRA", AddressMode::ZeroPageX),
        0x78 => ("SEI", AddressMode::Implied),
        0x79 => ("ADC", AddressMode::AbsoluteY),
        0x7B => ("RRA", AddressMode::AbsoluteY),
        0x7D => ("ADC", AddressMode::AbsoluteX),
        0x7E => ("ROR", AddressMode::AbsoluteX),
        0x7F => ("RRA", AddressMode::AbsoluteX),
        0x80 | 0x82 | 0x89 | 0xC2 | 0xE2 => ("NOP", AddressMode::Immediate),
        0x81 => ("STA", AddressMode::IndirectX),
        0x83 => ("SAX", AddressMode::IndirectX),
        0x84 => ("STY", AddressMode::ZeroPage),
        0x85 => ("STA", AddressMode::ZeroPage),
        0x86 => ("STX", AddressMode::ZeroPage),
        0x87 => ("SAX", AddressMode::ZeroPage),
        0x88 => ("DEY", AddressMode::Implied),
        0x8A => ("TXA", AddressMode::Implied),
        0x8B => ("XAA", AddressMode::Immediate),
        0x8C => ("STY", AddressMode::Absolute),
        0x8D => ("STA", AddressMode::Absolute),
        0x8E => ("STX", AddressMode::Absolute),
        0x8F => ("SAX", AddressMode::Absolute),
        0x90 => ("BCC", AddressMode::Relative),
        0x91 => ("STA", AddressMode::IndirectY),
        0x93 => ("AHX", AddressMode::IndirectY),
        0x94 => ("STY", AddressMode::ZeroPageX),
        0x95 => ("STA", AddressMode::ZeroPageX),
        0x96 => ("STX", AddressMode::ZeroPageY),
        0x97 => ("SAX", AddressMode::ZeroPageY),
        0x98 => ("TYA", AddressMode::Implied),
        0x99 => ("STA", AddressMode::AbsoluteY),
        0x9A => ("TXS", AddressMode::Implied),
        0x9B => ("XXX", AddressMode::AbsoluteY),
        0x9C => ("SHY", AddressMode::AbsoluteX),
        0x9D => ("STA", AddressMode::AbsoluteX),
        0x9E => ("SHX", AddressMode::AbsoluteY),
        0x9F => ("AHX", AddressMode::AbsoluteY),
        0xA0 => ("LDY", AddressMode::Immediate),
        0xA1 => ("LDA", AddressMode::IndirectX),
        0xA2 => ("LDX", AddressMode::Immediate),
        0xA3 => ("LAX", AddressMode::IndirectX),
        0xA4 => ("LDY", AddressMode::ZeroPage),
        0xA5 => ("LDA", AddressMode::ZeroPage),
        0xA6 => ("LDX", AddressMode::ZeroPage),
        0xA7 => ("LAX", AddressMode::ZeroPage),
        0xA8 => ("TAY", AddressMode::Implied),
        0xA9 => ("LDA", AddressMode::Immediate),
        0xAA => ("TAX", AddressMode::Implied),
        0xAB => ("OAL", AddressMode::Immediate),
        0xAC => ("LDY", AddressMode::Absolute),
        0xAD => ("LDA", AddressMode::Absolute),
        0xAE => ("LDX", AddressMode::Absolute),
        0xAF => ("LAX", AddressMode::Absolute),
        0xB0 => ("BCS", AddressMode::Relative),
        0xB1 => ("LDA", AddressMode::IndirectY),
        0xB3 => ("LAX", AddressMode::IndirectY),
        0xB4 => ("LDY", AddressMode::ZeroPageX),
        0xB5 => ("LDA", AddressMode::ZeroPageX),
        0xB6 => ("LDX", AddressMode::ZeroPageY),
        0xB7 => ("LAX", AddressMode::ZeroPageY),
        0xB8 => ("CLV", AddressMode::Implied),
        0xB9 => ("LDA", AddressMode::AbsoluteY),
        0xBA => ("TSX", AddressMode::Implied),
        0xBB => ("LAS", AddressMode::AbsoluteY),
        0xBC => ("LDY", AddressMode::AbsoluteX),
        0xBD => ("LDA", AddressMode::AbsoluteX),
        0xBE => ("LDX", AddressMode::AbsoluteY),
        0xBF => ("LAX", AddressMode::AbsoluteY),
        0xC0 => ("CPY", AddressMode::Immediate),
        0xC1 => ("CMP", AddressMode::IndirectX),
        0xC3 => ("DCP", AddressMode::IndirectX),
        0xC4 => ("CPY", AddressMode::ZeroPage),
        0xC5 => ("CMP", AddressMode::ZeroPage),
        0xC6 => ("DEC", AddressMode::ZeroPage),
        0xC7 => ("DCP", AddressMode::ZeroPage),
        0xC8 => ("INY", AddressMode::Implied),
        0xC9 => ("CMP", AddressMode::Immediate),
        0xCA => ("DEX", AddressMode::Implied),
        0xCB => ("AXS", AddressMode::Immediate),
        0xCC => ("CPY", AddressMode::Absolute),
        0xCD => ("CMP", AddressMode::Absolute),
        0xCE => ("DEC", AddressMode::Absolute),
        0xCF => ("DCP", AddressMode::Absolute),
        0xD0 => ("BNE", AddressMode::Relative),
        0xD1 => ("CMP", AddressMode::IndirectY),
        0xD3 => ("DCP", AddressMode::IndirectY),
        0xD5 => ("CMP", AddressMode::ZeroPageX),
        0xD6 => ("DEC", AddressMode::ZeroPageX),
        0xD7 => ("DCP", AddressMode::ZeroPageX),
        0xD8 => ("CLD", AddressMode::Implied),
        0xD9 => ("CMP", AddressMode::AbsoluteY),
        0xDB => ("DCP", AddressMode::AbsoluteY),
        0xDD => ("CMP", AddressMode::AbsoluteX),
        0xDE => ("DEC", AddressMode::AbsoluteX),
        0xDF => ("DCP", AddressMode::AbsoluteX),
        0xE0 => ("CPX", AddressMode::Immediate),
        0xE1 => ("SBC", AddressMode::IndirectX),
        0xE3 => ("ISC", AddressMode::IndirectX),
        0xE4 => ("CPX", AddressMode::ZeroPage),
        0xE5 => ("SBC", AddressMode::ZeroPage),
        0xE6 => ("INC", AddressMode::ZeroPage),
        0xE7 => ("ISC", AddressMode::ZeroPage),
        0xE8 => ("INX", AddressMode::Implied),
        0xE9 | 0xEB => ("SBC", AddressMode::Immediate),
        0xEC => ("CPX", AddressMode::Absolute),
        0xED => ("SBC", AddressMode::Absolute),
        0xEE => ("INC", AddressMode::Absolute),
        0xEF => ("ISC", AddressMode::Absolute),
        0xF0 => ("BEQ", AddressMode::Relative),
        0xF1 => ("SBC", AddressMode::IndirectY),
        0xF3 => ("ISC", AddressMode::IndirectY),
        0xF5 => ("SBC", AddressMode::ZeroPageX),
        0xF6 => ("INC", AddressMode::ZeroPageX),
        0xF7 => ("ISC", AddressMode::ZeroPageX),
        0xF8 => ("SED", AddressMode::Implied),
        0xF9 => ("SBC", AddressMode::AbsoluteY),
        0xFB => ("ISC", AddressMode::AbsoluteY),
        0xFD => ("SBC", AddressMode::AbsoluteX),
        0xFE => ("INC", AddressMode::AbsoluteX),
        0xFF => ("ISC", AddressMode::AbsoluteX),
    };

    let mut out = format!("{pc:04X} {op:02X} ");
    let mut count = pc + Wrapping(2);

    match mode {
        AddressMode::Immediate => {
            write!(out, "{pc1:02X}      {opcode} #{pc1:02X}").unwrap();
        }
        AddressMode::ZeroPage => {
            write!(out, "{pc1:02X}      {opcode} {pc1:02X}").unwrap();
        }
        AddressMode::ZeroPageX => {
            write!(out, "{pc1:02X}      {opcode} {pc1:02X},X").unwrap();
        }
        AddressMode::ZeroPageY => {
            write!(out, "{pc1:02X}      {opcode} {pc1:02X},Y").unwrap();
        }
        AddressMode::IndirectX => {
            write!(out, "{pc1:02X}      {opcode} ({pc1:02X},X)",).unwrap();
        }
        AddressMode::IndirectY => {
            write!(out, "{pc1:02X}      {opcode} ({pc1:02X},Y)").unwrap();
        }
        AddressMode::Absolute => {
            write!(out, "{pc1:02X} {pc2:02X}   {opcode} {pc2:02X}{pc1:02X}",).unwrap();
            count += 1;
        }
        AddressMode::AbsoluteX => {
            write!(out, "{pc1:02X} {pc2:02X}   {opcode} {pc2:02X}{pc1:02X},X",).unwrap();
            count += 1;
        }
        AddressMode::AbsoluteY => {
            write!(out, "{pc1:02X} {pc2:02X}   {opcode} {pc2:02X}{pc1:02X},Y",).unwrap();
            count += 1;
        }
        AddressMode::Indirect => {
            write!(out, "{pc1:02X} {pc2:02X}   {opcode} ({pc2:02X}{pc1:02X})",).unwrap();
            count += 1;
        }
        AddressMode::Implied => {
            write!(out, "        {opcode}").unwrap();
            count -= 1;
        }
        AddressMode::Relative => {
            write!(
                out,
                "{pc1:02X}      {opcode} {pc1:02X} ({:04X})",
                pc + pc116 + Wrapping(2u16)
            )
            .unwrap();
        }
    }

    (out, count)
}
