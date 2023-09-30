use crate::{AddressMode, Opcode, Operation};
use ahash::{AHashMap, AHashSet};
use std::sync::OnceLock;
#[cfg(not(coverage))]
use strum::IntoEnumIterator;

// TODO(jchacon): Replace all OnceLock with LazyLock (and get rid of the fn)
//                once LazyLock stablizes.

// CMOS_OPCODES is a hashmap of the Opcode -> Hashmap of valid addressing modes and their u8 opcode values.
// This is a vector since NOP, HLT and a few others duplicate address mode and can do the same thing from N values.
// An assembler should simply use the first value of each Vec (unless they want to randomly chose) except for BBR/BBS/RMB/SMB
// where the index in the vec indicates the bit it is impacting.
#[allow(clippy::too_many_lines)]
pub(crate) fn cmos_opcodes() -> &'static AHashMap<Opcode, AHashMap<AddressMode, Vec<u8>>> {
    static CMOS_OPCODES: OnceLock<AHashMap<Opcode, AHashMap<AddressMode, Vec<u8>>>> =
        OnceLock::new();
    CMOS_OPCODES.get_or_init(|| {
        let m = AHashMap::from([
            (
                Opcode::ADC,
                AHashMap::from([
                    (AddressMode::IndirectY, vec![0x71]),
                    (AddressMode::IndirectX, vec![0x61]),
                    (AddressMode::ZeroPageX, vec![0x75]),
                    (AddressMode::AbsoluteX, vec![0x7D]),
                    (AddressMode::AbsoluteY, vec![0x79]),
                    (AddressMode::ZeroPage, vec![0x65]),
                    (AddressMode::Immediate, vec![0x69]),
                    (AddressMode::Absolute, vec![0x6D]),
                    (AddressMode::Indirect, vec![0x72]),
                ]),
            ),
            (
                Opcode::AND,
                AHashMap::from([
                    (AddressMode::Absolute, vec![0x2D]),
                    (AddressMode::ZeroPageX, vec![0x35]),
                    (AddressMode::Immediate, vec![0x29]),
                    (AddressMode::IndirectY, vec![0x31]),
                    (AddressMode::IndirectX, vec![0x21]),
                    (AddressMode::AbsoluteX, vec![0x3D]),
                    (AddressMode::ZeroPage, vec![0x25]),
                    (AddressMode::AbsoluteY, vec![0x39]),
                    (AddressMode::Indirect, vec![0x32]),
                ]),
            ),
            (
                Opcode::ASL,
                AHashMap::from([
                    (AddressMode::ZeroPage, vec![0x06]),
                    (AddressMode::Implied, vec![0x0A]),
                    (AddressMode::ZeroPageX, vec![0x16]),
                    (AddressMode::Absolute, vec![0x0E]),
                    (AddressMode::AbsoluteX, vec![0x1E]),
                ]),
            ),
            (
                Opcode::BBR,
                AHashMap::from([(
                    AddressMode::ZeroPageRelative,
                    vec![0x0F, 0x1F, 0x2F, 0x3F, 0x4F, 0x5F, 0x6F, 0x7F],
                )]),
            ),
            (
                Opcode::BBS,
                AHashMap::from([(
                    AddressMode::ZeroPageRelative,
                    vec![0x8F, 0x9F, 0xAF, 0xBF, 0xCF, 0xDF, 0xEF, 0xFF],
                )]),
            ),
            (
                Opcode::BCC,
                AHashMap::from([(AddressMode::Relative, vec![0x90])]),
            ),
            (
                Opcode::BCS,
                AHashMap::from([(AddressMode::Relative, vec![0xB0])]),
            ),
            (
                Opcode::BEQ,
                AHashMap::from([(AddressMode::Relative, vec![0xF0])]),
            ),
            (
                Opcode::BIT,
                AHashMap::from([
                    (AddressMode::ZeroPage, vec![0x24]),
                    (AddressMode::Absolute, vec![0x2C]),
                    (AddressMode::Immediate, vec![0x89]),
                    (AddressMode::ZeroPageX, vec![0x34]),
                    (AddressMode::AbsoluteX, vec![0x3C]),
                ]),
            ),
            (
                Opcode::BMI,
                AHashMap::from([(AddressMode::Relative, vec![0x30])]),
            ),
            (
                Opcode::BNE,
                AHashMap::from([(AddressMode::Relative, vec![0xD0])]),
            ),
            (
                Opcode::BPL,
                AHashMap::from([(AddressMode::Relative, vec![0x10])]),
            ),
            (
                Opcode::BRA,
                AHashMap::from([(AddressMode::Relative, vec![0x80])]),
            ),
            (
                Opcode::BRK,
                AHashMap::from([(AddressMode::Immediate, vec![0x00])]),
            ),
            (
                Opcode::BVC,
                AHashMap::from([(AddressMode::Relative, vec![0x50])]),
            ),
            (
                Opcode::BVS,
                AHashMap::from([(AddressMode::Relative, vec![0x70])]),
            ),
            (
                Opcode::CLC,
                AHashMap::from([(AddressMode::Implied, vec![0x18])]),
            ),
            (
                Opcode::CLD,
                AHashMap::from([(AddressMode::Implied, vec![0xD8])]),
            ),
            (
                Opcode::CLI,
                AHashMap::from([(AddressMode::Implied, vec![0x58])]),
            ),
            (
                Opcode::CLV,
                AHashMap::from([(AddressMode::Implied, vec![0xB8])]),
            ),
            (
                Opcode::CMP,
                AHashMap::from([
                    (AddressMode::IndirectY, vec![0xD1]),
                    (AddressMode::ZeroPage, vec![0xC5]),
                    (AddressMode::Absolute, vec![0xCD]),
                    (AddressMode::ZeroPageX, vec![0xD5]),
                    (AddressMode::AbsoluteX, vec![0xDD]),
                    (AddressMode::Immediate, vec![0xC9]),
                    (AddressMode::IndirectX, vec![0xC1]),
                    (AddressMode::AbsoluteY, vec![0xD9]),
                    (AddressMode::Indirect, vec![0xD2]),
                ]),
            ),
            (
                Opcode::CPX,
                AHashMap::from([
                    (AddressMode::Immediate, vec![0xE0]),
                    (AddressMode::ZeroPage, vec![0xE4]),
                    (AddressMode::Absolute, vec![0xEC]),
                ]),
            ),
            (
                Opcode::CPY,
                AHashMap::from([
                    (AddressMode::Absolute, vec![0xCC]),
                    (AddressMode::Immediate, vec![0xC0]),
                    (AddressMode::ZeroPage, vec![0xC4]),
                ]),
            ),
            (
                Opcode::DEC,
                AHashMap::from([
                    (AddressMode::ZeroPageX, vec![0xD6]),
                    (AddressMode::Absolute, vec![0xCE]),
                    (AddressMode::ZeroPage, vec![0xC6]),
                    (AddressMode::AbsoluteX, vec![0xDE]),
                    (AddressMode::Implied, vec![0x3A]),
                ]),
            ),
            (
                Opcode::DEX,
                AHashMap::from([(AddressMode::Implied, vec![0xCA])]),
            ),
            (
                Opcode::DEY,
                AHashMap::from([(AddressMode::Implied, vec![0x88])]),
            ),
            (
                Opcode::EOR,
                AHashMap::from([
                    (AddressMode::IndirectX, vec![0x41]),
                    (AddressMode::ZeroPage, vec![0x45]),
                    (AddressMode::Absolute, vec![0x4D]),
                    (AddressMode::AbsoluteX, vec![0x5D]),
                    (AddressMode::AbsoluteY, vec![0x59]),
                    (AddressMode::IndirectY, vec![0x51]),
                    (AddressMode::Immediate, vec![0x49]),
                    (AddressMode::ZeroPageX, vec![0x55]),
                    (AddressMode::Indirect, vec![0x52]),
                ]),
            ),
            (
                Opcode::INC,
                AHashMap::from([
                    (AddressMode::Absolute, vec![0xEE]),
                    (AddressMode::ZeroPageX, vec![0xF6]),
                    (AddressMode::AbsoluteX, vec![0xFE]),
                    (AddressMode::ZeroPage, vec![0xE6]),
                    (AddressMode::Implied, vec![0x1A]),
                ]),
            ),
            (
                Opcode::INX,
                AHashMap::from([(AddressMode::Implied, vec![0xE8])]),
            ),
            (
                Opcode::INY,
                AHashMap::from([(AddressMode::Implied, vec![0xC8])]),
            ),
            (
                Opcode::JMP,
                AHashMap::from([
                    (AddressMode::Absolute, vec![0x4C]),
                    (AddressMode::AbsoluteIndirect, vec![0x6C]),
                    (AddressMode::AbsoluteIndirectX, vec![0x7C]),
                ]),
            ),
            (
                Opcode::JSR,
                AHashMap::from([(AddressMode::Absolute, vec![0x20])]),
            ),
            (
                Opcode::LDA,
                AHashMap::from([
                    (AddressMode::ZeroPage, vec![0xA5]),
                    (AddressMode::ZeroPageX, vec![0xB5]),
                    (AddressMode::IndirectY, vec![0xB1]),
                    (AddressMode::AbsoluteX, vec![0xBD]),
                    (AddressMode::AbsoluteY, vec![0xB9]),
                    (AddressMode::Absolute, vec![0xAD]),
                    (AddressMode::IndirectX, vec![0xA1]),
                    (AddressMode::Immediate, vec![0xA9]),
                    (AddressMode::Indirect, vec![0xB2]),
                ]),
            ),
            (
                Opcode::LDX,
                AHashMap::from([
                    (AddressMode::ZeroPage, vec![0xA6]),
                    (AddressMode::AbsoluteY, vec![0xBE]),
                    (AddressMode::ZeroPageY, vec![0xB6]),
                    (AddressMode::Immediate, vec![0xA2]),
                    (AddressMode::Absolute, vec![0xAE]),
                ]),
            ),
            (
                Opcode::LDY,
                AHashMap::from([
                    (AddressMode::Immediate, vec![0xA0]),
                    (AddressMode::AbsoluteX, vec![0xBC]),
                    (AddressMode::ZeroPage, vec![0xA4]),
                    (AddressMode::ZeroPageX, vec![0xB4]),
                    (AddressMode::Absolute, vec![0xAC]),
                ]),
            ),
            (
                Opcode::LSR,
                AHashMap::from([
                    (AddressMode::ZeroPageX, vec![0x56]),
                    (AddressMode::ZeroPage, vec![0x46]),
                    (AddressMode::Absolute, vec![0x4E]),
                    (AddressMode::AbsoluteX, vec![0x5E]),
                    (AddressMode::Implied, vec![0x4A]),
                ]),
            ),
            (
                Opcode::NOP,
                AHashMap::from([
                    (AddressMode::ZeroPageX, vec![0x54, 0xD4, 0xF4]),
                    (AddressMode::ZeroPage, vec![0x44]),
                    (AddressMode::AbsoluteX, vec![0xDC, 0xFC]),
                    (AddressMode::AbsoluteNOP, vec![0x5C]),
                    (AddressMode::Implied, vec![0xEA]),
                    (
                        AddressMode::Immediate,
                        vec![0x02, 0x22, 0x42, 0x62, 0x82, 0xC2, 0xE2],
                    ),
                    (
                        AddressMode::NOPCmos,
                        vec![
                            0x03, 0x13, 0x23, 0x33, 0x43, 0x53, 0x63, 0x73, 0x83, 0x93, 0xA3, 0xB3,
                            0xC3, 0xD3, 0xE3, 0xF3, 0x0B, 0x1B, 0x2B, 0x3B, 0x4B, 0x5B, 0x6B, 0x7B,
                            0x8B, 0x9B, 0xAB, 0xBB, 0xEB, 0xFB,
                        ],
                    ),
                ]),
            ),
            (
                Opcode::ORA,
                AHashMap::from([
                    (AddressMode::AbsoluteX, vec![0x1D]),
                    (AddressMode::IndirectY, vec![0x11]),
                    (AddressMode::ZeroPageX, vec![0x15]),
                    (AddressMode::ZeroPage, vec![0x05]),
                    (AddressMode::Immediate, vec![0x09]),
                    (AddressMode::IndirectX, vec![0x01]),
                    (AddressMode::Absolute, vec![0x0D]),
                    (AddressMode::AbsoluteY, vec![0x19]),
                    (AddressMode::Indirect, vec![0x12]),
                ]),
            ),
            (
                Opcode::PHA,
                AHashMap::from([(AddressMode::Implied, vec![0x48])]),
            ),
            (
                Opcode::PHP,
                AHashMap::from([(AddressMode::Implied, vec![0x08])]),
            ),
            (
                Opcode::PHX,
                AHashMap::from([(AddressMode::Implied, vec![0xDA])]),
            ),
            (
                Opcode::PHY,
                AHashMap::from([(AddressMode::Implied, vec![0x5A])]),
            ),
            (
                Opcode::PLA,
                AHashMap::from([(AddressMode::Implied, vec![0x68])]),
            ),
            (
                Opcode::PLP,
                AHashMap::from([(AddressMode::Implied, vec![0x28])]),
            ),
            (
                Opcode::PLX,
                AHashMap::from([(AddressMode::Implied, vec![0xFA])]),
            ),
            (
                Opcode::PLY,
                AHashMap::from([(AddressMode::Implied, vec![0x7A])]),
            ),
            (
                Opcode::RMB,
                AHashMap::from([(
                    AddressMode::ZeroPage,
                    vec![0x07, 0x17, 0x27, 0x37, 0x47, 0x57, 0x67, 0x77],
                )]),
            ),
            (
                Opcode::ROL,
                AHashMap::from([
                    (AddressMode::Absolute, vec![0x2E]),
                    (AddressMode::ZeroPage, vec![0x26]),
                    (AddressMode::ZeroPageX, vec![0x36]),
                    (AddressMode::AbsoluteX, vec![0x3E]),
                    (AddressMode::Implied, vec![0x2A]),
                ]),
            ),
            (
                Opcode::ROR,
                AHashMap::from([
                    (AddressMode::ZeroPageX, vec![0x76]),
                    (AddressMode::AbsoluteX, vec![0x7E]),
                    (AddressMode::ZeroPage, vec![0x66]),
                    (AddressMode::Implied, vec![0x6A]),
                    (AddressMode::Absolute, vec![0x6E]),
                ]),
            ),
            (
                Opcode::RTI,
                AHashMap::from([(AddressMode::Implied, vec![0x40])]),
            ),
            (
                Opcode::RTS,
                AHashMap::from([(AddressMode::Implied, vec![0x60])]),
            ),
            (
                Opcode::SBC,
                AHashMap::from([
                    (AddressMode::ZeroPageX, vec![0xF5]),
                    (AddressMode::AbsoluteY, vec![0xF9]),
                    (AddressMode::ZeroPage, vec![0xE5]),
                    (AddressMode::AbsoluteX, vec![0xFD]),
                    (AddressMode::IndirectX, vec![0xE1]),
                    (AddressMode::Immediate, vec![0xE9]),
                    (AddressMode::IndirectY, vec![0xF1]),
                    (AddressMode::Absolute, vec![0xED]),
                    (AddressMode::Indirect, vec![0xF2]),
                ]),
            ),
            (
                Opcode::SEC,
                AHashMap::from([(AddressMode::Implied, vec![0x38])]),
            ),
            (
                Opcode::SED,
                AHashMap::from([(AddressMode::Implied, vec![0xF8])]),
            ),
            (
                Opcode::SEI,
                AHashMap::from([(AddressMode::Implied, vec![0x78])]),
            ),
            (
                Opcode::SMB,
                AHashMap::from([(
                    AddressMode::ZeroPage,
                    vec![0x87, 0x97, 0xA7, 0xB7, 0xC7, 0xD7, 0xE7, 0xF7],
                )]),
            ),
            (
                Opcode::STA,
                AHashMap::from([
                    (AddressMode::IndirectY, vec![0x91]),
                    (AddressMode::ZeroPageX, vec![0x95]),
                    (AddressMode::IndirectX, vec![0x81]),
                    (AddressMode::ZeroPage, vec![0x85]),
                    (AddressMode::AbsoluteX, vec![0x9D]),
                    (AddressMode::Absolute, vec![0x8D]),
                    (AddressMode::AbsoluteY, vec![0x99]),
                    (AddressMode::Indirect, vec![0x92]),
                ]),
            ),
            (
                Opcode::STP,
                AHashMap::from([(AddressMode::Implied, vec![0xDB])]),
            ),
            (
                Opcode::STX,
                AHashMap::from([
                    (AddressMode::ZeroPageY, vec![0x96]),
                    (AddressMode::ZeroPage, vec![0x86]),
                    (AddressMode::Absolute, vec![0x8E]),
                ]),
            ),
            (
                Opcode::STY,
                AHashMap::from([
                    (AddressMode::ZeroPageX, vec![0x94]),
                    (AddressMode::Absolute, vec![0x8C]),
                    (AddressMode::ZeroPage, vec![0x84]),
                ]),
            ),
            (
                Opcode::STZ,
                AHashMap::from([
                    (AddressMode::ZeroPageX, vec![0x74]),
                    (AddressMode::Absolute, vec![0x9C]),
                    (AddressMode::AbsoluteX, vec![0x9E]),
                    (AddressMode::ZeroPage, vec![0x64]),
                ]),
            ),
            (
                Opcode::TAX,
                AHashMap::from([(AddressMode::Implied, vec![0xAA])]),
            ),
            (
                Opcode::TAY,
                AHashMap::from([(AddressMode::Implied, vec![0xA8])]),
            ),
            (
                Opcode::TRB,
                AHashMap::from([
                    (AddressMode::ZeroPage, vec![0x14]),
                    (AddressMode::Absolute, vec![0x1C]),
                ]),
            ),
            (
                Opcode::TSB,
                AHashMap::from([
                    (AddressMode::ZeroPage, vec![0x04]),
                    (AddressMode::Absolute, vec![0x0C]),
                ]),
            ),
            (
                Opcode::TSX,
                AHashMap::from([(AddressMode::Implied, vec![0xBA])]),
            ),
            (
                Opcode::TXA,
                AHashMap::from([(AddressMode::Implied, vec![0x8A])]),
            ),
            (
                Opcode::TXS,
                AHashMap::from([(AddressMode::Implied, vec![0x9A])]),
            ),
            (
                Opcode::TYA,
                AHashMap::from([(AddressMode::Implied, vec![0x98])]),
            ),
            (
                Opcode::WAI,
                AHashMap::from([(AddressMode::Implied, vec![0xCB])]),
            ),
        ]);

        // opcodes that are NMOS only so we can't validate they are in the map.
        #[cfg(not(coverage))]
        let nmos = AHashSet::from([
            Opcode::AHX,
            Opcode::ALR,
            Opcode::ANC,
            Opcode::ARR,
            Opcode::AXS,
            Opcode::DCP,
            Opcode::HLT,
            Opcode::ISC,
            Opcode::LAS,
            Opcode::LAX,
            Opcode::OAL,
            Opcode::RLA,
            Opcode::RRA,
            Opcode::SAX,
            Opcode::SHX,
            Opcode::SHY,
            Opcode::SLO,
            Opcode::SRE,
            Opcode::TAS,
            Opcode::XAA,
        ]);

        // Make sure every other opcode in the enum has a map entry.
        #[cfg(not(coverage))]
        assert!(
            !Opcode::iter().any(|op| !nmos.contains(&op) && m.get(&op).is_none()),
            "Not all opcodes covered! - {m:?}"
        );
        m
    })
}

// CMOS_OPCODES_VALUES is the inverse of CMOS_OPCODES where the keys are the u8 byte codes and values Operation defining
// the Opcode and AddressMode. Used in processing the CPU tick() or in disassembly for mapping a byte code back
// to an Opcode.
pub(crate) fn cmos_opcodes_values() -> &'static Vec<Operation> {
    static CMOS_OPCODES_VALUES: OnceLock<Vec<Operation>> = OnceLock::new();
    CMOS_OPCODES_VALUES.get_or_init(|| {
        // We know this much be a vector of all u8 values since the 6502
        // has behavior at each so we'll have some combo of opcode/addressmode.
        //
        // Preallocate a vector of that size and fill with placeholders.
        // Then track in the hashset which indexes we've seen (panic on dups)
        // and insert directly to each index.
        let mut m = Vec::new();
        m.resize(
            1 << 8,
            Operation {
                op: Opcode::BRK,
                mode: AddressMode::Implied,
            },
        );
        let sl = m.as_mut_slice();
        let mut hs = AHashSet::new();

        for (op, hm) in cmos_opcodes() {
            for (am, opbytes) in hm {
                for opbyte in opbytes {
                    assert!(!hs.contains(opbyte),"CMOS_OPCODES contains multiple entries for {opbyte:#04X} found in opcode {op} but we already have {:?}", sl[usize::from(*opbyte)]);
                    hs.insert(*opbyte);
                    sl[usize::from(*opbyte)] = Operation { op: *op, mode: *am };
                }
            }
        }

        #[cfg(not(coverage))]
        assert!(
            hs.len() == (1 << 8),
            "Didn't fill out {} opcodes. Only defined {} - {m:?}",
            1 << 8,
            hs.len()
        );
        m
    })
}
