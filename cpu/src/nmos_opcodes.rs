use crate::{AddressMode, Opcode, Operation};
use ahash::{AHashMap, AHashSet};
use std::sync::LazyLock;
#[cfg(not(coverage))]
use strum::IntoEnumIterator;

// NMOS_OPCODES is a hashmap of the Opcode -> Hashmap of valid addressing modes and their u8 opcode values.
// This is a vector since NOP, HLT and a few others duplicate address mode and can do the same thing from N values.
// An assembler should simply use the first value of each Vec unless they want to randomly chose.
#[allow(clippy::too_many_lines)]
pub(crate) static NMOS_OPCODES: LazyLock<AHashMap<Opcode, AHashMap<AddressMode, Vec<u8>>>> =
    LazyLock::new(|| {
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
                ]),
            ),
            (
                Opcode::AHX,
                AHashMap::from([
                    (AddressMode::IndirectY, vec![0x93]),
                    (AddressMode::AbsoluteY, vec![0x9F]),
                ]),
            ),
            (
                Opcode::ALR,
                AHashMap::from([(AddressMode::Immediate, vec![0x4B])]),
            ),
            (
                Opcode::ANC,
                AHashMap::from([(AddressMode::Immediate, vec![0x0B, 0x2B])]),
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
                ]),
            ),
            (
                Opcode::ARR,
                AHashMap::from([(AddressMode::Immediate, vec![0x6B])]),
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
                Opcode::AXS,
                AHashMap::from([(AddressMode::Immediate, vec![0xCB])]),
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
                ]),
            ),
            (
                Opcode::DEX,
                AHashMap::from([(AddressMode::Implied, vec![0xCA])]),
            ),
            (
                Opcode::DCP,
                AHashMap::from([
                    (AddressMode::Absolute, vec![0xCF]),
                    (AddressMode::ZeroPage, vec![0xC7]),
                    (AddressMode::IndirectY, vec![0xD3]),
                    (AddressMode::IndirectX, vec![0xC3]),
                    (AddressMode::AbsoluteX, vec![0xDF]),
                    (AddressMode::ZeroPageX, vec![0xD7]),
                    (AddressMode::AbsoluteY, vec![0xDB]),
                ]),
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
                ]),
            ),
            (
                Opcode::HLT,
                AHashMap::from([(
                    AddressMode::Implied,
                    vec![
                        0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92, 0xB2, 0xD2, 0xF2,
                    ],
                )]),
            ),
            (
                Opcode::INC,
                AHashMap::from([
                    (AddressMode::Absolute, vec![0xEE]),
                    (AddressMode::ZeroPageX, vec![0xF6]),
                    (AddressMode::AbsoluteX, vec![0xFE]),
                    (AddressMode::ZeroPage, vec![0xE6]),
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
                Opcode::ISC,
                AHashMap::from([
                    (AddressMode::Absolute, vec![0xEF]),
                    (AddressMode::IndirectX, vec![0xE3]),
                    (AddressMode::AbsoluteY, vec![0xFB]),
                    (AddressMode::ZeroPage, vec![0xE7]),
                    (AddressMode::AbsoluteX, vec![0xFF]),
                    (AddressMode::ZeroPageX, vec![0xF7]),
                    (AddressMode::IndirectY, vec![0xF3]),
                ]),
            ),
            (
                Opcode::JMP,
                AHashMap::from([
                    (AddressMode::Absolute, vec![0x4C]),
                    (AddressMode::AbsoluteIndirect, vec![0x6C]),
                ]),
            ),
            (
                Opcode::JSR,
                AHashMap::from([(AddressMode::Absolute, vec![0x20])]),
            ),
            (
                Opcode::LAS,
                AHashMap::from([(AddressMode::AbsoluteY, vec![0xBB])]),
            ),
            (
                Opcode::LAX,
                AHashMap::from([
                    (AddressMode::AbsoluteY, vec![0xBF]),
                    (AddressMode::Absolute, vec![0xAF]),
                    (AddressMode::ZeroPage, vec![0xA7]),
                    (AddressMode::ZeroPageY, vec![0xB7]),
                    (AddressMode::IndirectY, vec![0xB3]),
                    (AddressMode::IndirectX, vec![0xA3]),
                ]),
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
                    (
                        AddressMode::ZeroPageX,
                        vec![0x14, 0x34, 0x54, 0x74, 0xD4, 0xF4],
                    ),
                    (
                        AddressMode::Implied,
                        vec![0x1A, 0x3A, 0x5A, 0x7A, 0xDA, 0xEA, 0xFA],
                    ),
                    (AddressMode::ZeroPage, vec![0x04, 0x44, 0x64]),
                    (AddressMode::Immediate, vec![0x80, 0x82, 0x89, 0xC2, 0xE2]),
                    (AddressMode::Absolute, vec![0x0C]),
                    (
                        AddressMode::AbsoluteX,
                        vec![0x1C, 0x3C, 0x5C, 0x7C, 0xDC, 0xFC],
                    ),
                ]),
            ),
            (
                Opcode::OAL,
                AHashMap::from([(AddressMode::Immediate, vec![0xAB])]),
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
                Opcode::PLA,
                AHashMap::from([(AddressMode::Implied, vec![0x68])]),
            ),
            (
                Opcode::PLP,
                AHashMap::from([(AddressMode::Implied, vec![0x28])]),
            ),
            (
                Opcode::RLA,
                AHashMap::from([
                    (AddressMode::AbsoluteY, vec![0x3B]),
                    (AddressMode::ZeroPageX, vec![0x37]),
                    (AddressMode::IndirectX, vec![0x23]),
                    (AddressMode::Absolute, vec![0x2F]),
                    (AddressMode::IndirectY, vec![0x33]),
                    (AddressMode::ZeroPage, vec![0x27]),
                    (AddressMode::AbsoluteX, vec![0x3F]),
                ]),
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
                Opcode::RRA,
                AHashMap::from([
                    (AddressMode::IndirectY, vec![0x73]),
                    (AddressMode::AbsoluteY, vec![0x7B]),
                    (AddressMode::AbsoluteX, vec![0x7F]),
                    (AddressMode::ZeroPageX, vec![0x77]),
                    (AddressMode::ZeroPage, vec![0x67]),
                    (AddressMode::Absolute, vec![0x6F]),
                    (AddressMode::IndirectX, vec![0x63]),
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
                Opcode::SAX,
                AHashMap::from([
                    (AddressMode::ZeroPageY, vec![0x97]),
                    (AddressMode::Absolute, vec![0x8F]),
                    (AddressMode::ZeroPage, vec![0x87]),
                    (AddressMode::IndirectX, vec![0x83]),
                ]),
            ),
            (
                Opcode::SBC,
                AHashMap::from([
                    (AddressMode::ZeroPageX, vec![0xF5]),
                    (AddressMode::AbsoluteY, vec![0xF9]),
                    (AddressMode::ZeroPage, vec![0xE5]),
                    (AddressMode::AbsoluteX, vec![0xFD]),
                    (AddressMode::IndirectX, vec![0xE1]),
                    (AddressMode::Immediate, vec![0xE9, 0xEB]),
                    (AddressMode::IndirectY, vec![0xF1]),
                    (AddressMode::Absolute, vec![0xED]),
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
                Opcode::SHX,
                AHashMap::from([(AddressMode::AbsoluteY, vec![0x9E])]),
            ),
            (
                Opcode::SHY,
                AHashMap::from([(AddressMode::AbsoluteX, vec![0x9C])]),
            ),
            (
                Opcode::SLO,
                AHashMap::from([
                    (AddressMode::AbsoluteY, vec![0x1B]),
                    (AddressMode::IndirectX, vec![0x03]),
                    (AddressMode::IndirectY, vec![0x13]),
                    (AddressMode::ZeroPage, vec![0x07]),
                    (AddressMode::AbsoluteX, vec![0x1F]),
                    (AddressMode::Absolute, vec![0x0F]),
                    (AddressMode::ZeroPageX, vec![0x17]),
                ]),
            ),
            (
                Opcode::SRE,
                AHashMap::from([
                    (AddressMode::AbsoluteY, vec![0x5B]),
                    (AddressMode::ZeroPage, vec![0x47]),
                    (AddressMode::Absolute, vec![0x4F]),
                    (AddressMode::AbsoluteX, vec![0x5F]),
                    (AddressMode::IndirectX, vec![0x43]),
                    (AddressMode::IndirectY, vec![0x53]),
                    (AddressMode::ZeroPageX, vec![0x57]),
                ]),
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
                ]),
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
                Opcode::TAS,
                AHashMap::from([(AddressMode::AbsoluteY, vec![0x9B])]),
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
                Opcode::XAA,
                AHashMap::from([(AddressMode::Immediate, vec![0x8B])]),
            ),
        ]);

        // opcodes that are CMOS only so we can't validate they are in the map.
        #[cfg(not(coverage))]
        let cmos = AHashSet::from([
            Opcode::BBR,
            Opcode::BBS,
            Opcode::BRA,
            Opcode::PHX,
            Opcode::PHY,
            Opcode::PLX,
            Opcode::PLY,
            Opcode::RMB,
            Opcode::SMB,
            Opcode::STP,
            Opcode::STZ,
            Opcode::TRB,
            Opcode::TSB,
            Opcode::WAI,
        ]);

        // Make sure every other opcode in the enum has a map entry.
        #[cfg(not(coverage))]
        assert!(
            !Opcode::iter().any(|op| !cmos.contains(&op) && m.get(&op).is_none()),
            "Not all opcodes covered! - {m:?}"
        );
        m
    });

// NMOS_OPCODES_VALUES is the inverse of NMOS_OPCODES where the keys are the u8 byte codes and values Operation defining
// the Opcode and AddressMode. Used in processing the CPU tick() or in disassembly for mapping a byte code back
// to an Opcode.
pub(crate) static NMOS_OPCODES_VALUES: LazyLock<Vec<Operation>> = LazyLock::new(|| {
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

    for (op, hm) in &*NMOS_OPCODES {
        for (am, opbytes) in hm {
            for opbyte in opbytes {
                assert!(!hs.contains(opbyte),"NMOS_OPCODES contains multiple entries for {opbyte:#04X} found in opcode {op} but we already have {:?}", sl[usize::from(*opbyte)]);
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
});
