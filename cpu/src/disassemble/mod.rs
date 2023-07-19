//! disassemble provides functions for disassembling a given
//! location.

use super::{opcode_op, AddressMode, Memory, Type};
use std::fmt::Write as _;
use std::num::Wrapping; // import without risk of name clashing

/// step will take the given pc and Memory implementation and disassemble the segment
/// at that location. It will return a string of the dissembly as well as the next pc
/// to continue disassembling.
/// As a real 6502 will wrap around if it's asked to step off the end
/// this will do the same. i.e. disassembling 0xFFFF with a multi-byte opcode will result
/// in reading 0x0000 and 0x0001 and returning a pc from that area as well.
pub fn step(ty: Type, pc: Wrapping<u16>, r: &dyn Memory) -> (String, Wrapping<u16>) {
    let pc1 = r.read((pc + Wrapping(1)).0);
    let pc2 = r.read((pc + Wrapping(2)).0);

    // Sign extend a 16 bit value so it can be added to PC for branch offsets
    #[allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
    let pc116 = Wrapping(i16::from(pc1 as i8) as u16);
    #[allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
    let pc216 = Wrapping(i16::from(pc2 as i8) as u16);

    let op = r.read(pc.0);

    let (opcode, mode) = {
        let operation = opcode_op(ty, op);
        (operation.op.to_string(), operation.mode)
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
        AddressMode::Indirect => {
            write!(out, "{pc1:02X}      {opcode} ({pc1:02X})").unwrap();
        }
        AddressMode::IndirectX => {
            write!(out, "{pc1:02X}      {opcode} ({pc1:02X},X)",).unwrap();
        }
        AddressMode::IndirectY => {
            write!(out, "{pc1:02X}      {opcode} ({pc1:02X}),Y").unwrap();
        }
        AddressMode::Absolute | AddressMode::AbsoluteNOP => {
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
        AddressMode::AbsoluteIndirect => {
            write!(out, "{pc1:02X} {pc2:02X}   {opcode} ({pc2:02X}{pc1:02X})",).unwrap();
            count += 1;
        }
        AddressMode::AbsoluteIndirectX => {
            write!(out, "{pc1:02X} {pc2:02X}   {opcode} ({pc2:02X}{pc1:02X},X)",).unwrap();
            count += 1;
        }
        AddressMode::Implied | AddressMode::NOPCmos => {
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
        AddressMode::ZeroPageRelative => {
            write!(
                out,
                "{pc1:02X} {pc2:02X}   {opcode} {},{pc1:02X},{pc2:02X} ({:04X})",
                (op & 0xF0) >> 4,
                pc + pc216 + Wrapping(2u16)
            )
            .unwrap();
        }
    }

    (out, count)
}
