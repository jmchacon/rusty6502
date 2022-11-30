use crate::{AddressMode, Opcode, Operation, Type};
use color_eyre::eyre::{eyre, Result};
use lazy_static::lazy_static;
use std::collections::{HashMap, HashSet};
use strum::IntoEnumIterator;

lazy_static! {
    // OPCODES is a hashmap of the Opcode -> Hashmap of valid addressing modes and their u8 opcode values.
    // This is a vector since NOP, HLT and a few others duplicate address mode and can do the same thing from N values.
    // An assembler should simply use the first value of each Vec unless they want to randomly chose.
    static ref NMOS_OPCODES: HashMap<Opcode, HashMap<AddressMode, Vec<u8>>> = {
        let m = HashMap::from([
            (
                Opcode::ADC,
                HashMap::from([
                    (AddressMode::AbsoluteX, vec![125]),
                    (AddressMode::ZeroPage, vec![101]),
                    (AddressMode::Absolute, vec![109]),
                    (AddressMode::ZeroPageX, vec![117]),
                    (AddressMode::IndirectY, vec![113]),
                    (AddressMode::AbsoluteY, vec![121]),
                    (AddressMode::Immediate, vec![105]),
                    (AddressMode::IndirectX, vec![97]),
                ]),
            ),
            (
                Opcode::AHX,
                HashMap::from([
                    (AddressMode::IndirectY, vec![147]),
                    (AddressMode::AbsoluteY, vec![159]),
                ]),
            ),
            (
                Opcode::ALR,
                HashMap::from([(AddressMode::Immediate, vec![75])]),
            ),
            (
                Opcode::ANC,
                HashMap::from([(AddressMode::Immediate, vec![11, 43])]),
            ),
            (
                Opcode::AND,
                HashMap::from([
                    (AddressMode::ZeroPageX, vec![53]),
                    (AddressMode::Absolute, vec![45]),
                    (AddressMode::IndirectX, vec![33]),
                    (AddressMode::AbsoluteX, vec![61]),
                    (AddressMode::AbsoluteY, vec![57]),
                    (AddressMode::ZeroPage, vec![37]),
                    (AddressMode::Immediate, vec![41]),
                    (AddressMode::IndirectY, vec![49]),
                ]),
            ),
            (
                Opcode::ARR,
                HashMap::from([(AddressMode::Immediate, vec![107])]),
            ),
            (
                Opcode::ASL,
                HashMap::from([
                    (AddressMode::ZeroPageX, vec![22]),
                    (AddressMode::ZeroPage, vec![6]),
                    (AddressMode::Absolute, vec![14]),
                    (AddressMode::AbsoluteX, vec![30]),
                    (AddressMode::Implied, vec![10]),
                ]),
            ),
            (
                Opcode::AXS,
                HashMap::from([(AddressMode::Immediate, vec![203])]),
            ),
            (
                Opcode::BCC,
                HashMap::from([(AddressMode::Relative, vec![144])]),
            ),
            (
                Opcode::BCS,
                HashMap::from([(AddressMode::Relative, vec![176])]),
            ),
            (
                Opcode::BEQ,
                HashMap::from([(AddressMode::Relative, vec![240])]),
            ),
            (
                Opcode::BIT,
                HashMap::from([
                    (AddressMode::Absolute, vec![44]),
                    (AddressMode::ZeroPage, vec![36]),
                ]),
            ),
            (
                Opcode::BMI,
                HashMap::from([(AddressMode::Relative, vec![48])]),
            ),
            (
                Opcode::BNE,
                HashMap::from([(AddressMode::Relative, vec![208])]),
            ),
            (
                Opcode::BPL,
                HashMap::from([(AddressMode::Relative, vec![16])]),
            ),
            (
                Opcode::BRK,
                HashMap::from([(AddressMode::Immediate, vec![0])]),
            ),
            (
                Opcode::BVC,
                HashMap::from([(AddressMode::Relative, vec![80])]),
            ),
            (
                Opcode::BVS,
                HashMap::from([(AddressMode::Relative, vec![112])]),
            ),
            (
                Opcode::CLC,
                HashMap::from([(AddressMode::Implied, vec![24])]),
            ),
            (
                Opcode::CLD,
                HashMap::from([(AddressMode::Implied, vec![216])]),
            ),
            (
                Opcode::CLI,
                HashMap::from([(AddressMode::Implied, vec![88])]),
            ),
            (
                Opcode::CLV,
                HashMap::from([(AddressMode::Implied, vec![184])]),
            ),
            (
                Opcode::CMP,
                HashMap::from([
                    (AddressMode::IndirectX, vec![193]),
                    (AddressMode::AbsoluteY, vec![217]),
                    (AddressMode::IndirectY, vec![209]),
                    (AddressMode::ZeroPage, vec![197]),
                    (AddressMode::AbsoluteX, vec![221]),
                    (AddressMode::Immediate, vec![201]),
                    (AddressMode::ZeroPageX, vec![213]),
                    (AddressMode::Absolute, vec![205]),
                ]),
            ),
            (
                Opcode::CPX,
                HashMap::from([
                    (AddressMode::Immediate, vec![224]),
                    (AddressMode::ZeroPage, vec![228]),
                    (AddressMode::Absolute, vec![236]),
                ]),
            ),
            (
                Opcode::CPY,
                HashMap::from([
                    (AddressMode::Absolute, vec![204]),
                    (AddressMode::Immediate, vec![192]),
                    (AddressMode::ZeroPage, vec![196]),
                ]),
            ),
            (
                Opcode::DCP,
                HashMap::from([
                    (AddressMode::IndirectY, vec![211]),
                    (AddressMode::ZeroPageX, vec![215]),
                    (AddressMode::AbsoluteX, vec![223]),
                    (AddressMode::AbsoluteY, vec![219]),
                    (AddressMode::Absolute, vec![207]),
                    (AddressMode::IndirectX, vec![195]),
                    (AddressMode::ZeroPage, vec![199]),
                ]),
            ),
            (
                Opcode::DEC,
                HashMap::from([
                    (AddressMode::AbsoluteX, vec![222]),
                    (AddressMode::ZeroPageX, vec![214]),
                    (AddressMode::Absolute, vec![206]),
                    (AddressMode::ZeroPage, vec![198]),
                ]),
            ),
            (
                Opcode::DEX,
                HashMap::from([(AddressMode::Implied, vec![202])]),
            ),
            (
                Opcode::DEY,
                HashMap::from([(AddressMode::Implied, vec![136])]),
            ),
            (
                Opcode::EOR,
                HashMap::from([
                    (AddressMode::Absolute, vec![77]),
                    (AddressMode::AbsoluteX, vec![93]),
                    (AddressMode::Immediate, vec![73]),
                    (AddressMode::AbsoluteY, vec![89]),
                    (AddressMode::ZeroPage, vec![69]),
                    (AddressMode::ZeroPageX, vec![85]),
                    (AddressMode::IndirectX, vec![65]),
                    (AddressMode::IndirectY, vec![81]),
                ]),
            ),
            (
                Opcode::HLT,
                HashMap::from([(
                    AddressMode::Implied,
                    vec![2, 18, 34, 50, 66, 82, 98, 114, 146, 178, 210, 242],
                )]),
            ),
            (
                Opcode::INC,
                HashMap::from([
                    (AddressMode::Absolute, vec![238]),
                    (AddressMode::AbsoluteX, vec![254]),
                    (AddressMode::ZeroPage, vec![230]),
                    (AddressMode::ZeroPageX, vec![246]),
                ]),
            ),
            (
                Opcode::INX,
                HashMap::from([(AddressMode::Implied, vec![232])]),
            ),
            (
                Opcode::INY,
                HashMap::from([(AddressMode::Implied, vec![200])]),
            ),
            (
                Opcode::ISC,
                HashMap::from([
                    (AddressMode::IndirectX, vec![227]),
                    (AddressMode::Absolute, vec![239]),
                    (AddressMode::AbsoluteY, vec![251]),
                    (AddressMode::IndirectY, vec![243]),
                    (AddressMode::ZeroPageX, vec![247]),
                    (AddressMode::ZeroPage, vec![231]),
                    (AddressMode::AbsoluteX, vec![255]),
                ]),
            ),
            (
                Opcode::JMP,
                HashMap::from([
                    (AddressMode::Indirect, vec![108]),
                    (AddressMode::Absolute, vec![76]),
                ]),
            ),
            (
                Opcode::JSR,
                HashMap::from([(AddressMode::Absolute, vec![32])]),
            ),
            (
                Opcode::LAS,
                HashMap::from([(AddressMode::AbsoluteY, vec![187])]),
            ),
            (
                Opcode::LAX,
                HashMap::from([
                    (AddressMode::AbsoluteY, vec![191]),
                    (AddressMode::ZeroPageY, vec![183]),
                    (AddressMode::Absolute, vec![175]),
                    (AddressMode::ZeroPage, vec![167]),
                    (AddressMode::IndirectY, vec![179]),
                    (AddressMode::IndirectX, vec![163]),
                ]),
            ),
            (
                Opcode::LDA,
                HashMap::from([
                    (AddressMode::AbsoluteY, vec![185]),
                    (AddressMode::Absolute, vec![173]),
                    (AddressMode::Immediate, vec![169]),
                    (AddressMode::IndirectY, vec![177]),
                    (AddressMode::IndirectX, vec![161]),
                    (AddressMode::ZeroPage, vec![165]),
                    (AddressMode::ZeroPageX, vec![181]),
                    (AddressMode::AbsoluteX, vec![189]),
                ]),
            ),
            (
                Opcode::LDX,
                HashMap::from([
                    (AddressMode::Immediate, vec![162]),
                    (AddressMode::Absolute, vec![174]),
                    (AddressMode::ZeroPage, vec![166]),
                    (AddressMode::ZeroPageY, vec![182]),
                    (AddressMode::AbsoluteY, vec![190]),
                ]),
            ),
            (
                Opcode::LDY,
                HashMap::from([
                    (AddressMode::ZeroPage, vec![164]),
                    (AddressMode::Absolute, vec![172]),
                    (AddressMode::AbsoluteX, vec![188]),
                    (AddressMode::Immediate, vec![160]),
                    (AddressMode::ZeroPageX, vec![180]),
                ]),
            ),
            (
                Opcode::LSR,
                HashMap::from([
                    (AddressMode::ZeroPageX, vec![86]),
                    (AddressMode::Implied, vec![74]),
                    (AddressMode::AbsoluteX, vec![94]),
                    (AddressMode::ZeroPage, vec![70]),
                    (AddressMode::Absolute, vec![78]),
                ]),
            ),
            (
                Opcode::NOP,
                HashMap::from([
                    (AddressMode::Immediate, vec![128, 130, 137, 194, 226]),
                    (AddressMode::Implied, vec![26, 58, 90, 122, 218, 234, 250]),
                    (AddressMode::AbsoluteX, vec![28, 60, 92, 124, 220, 252]),
                    (AddressMode::Absolute, vec![12]),
                    (AddressMode::ZeroPage, vec![4, 68, 100]),
                    (AddressMode::ZeroPageX, vec![20, 52, 84, 116, 212, 244]),
                ]),
            ),
            (
                Opcode::OAL,
                HashMap::from([(AddressMode::Immediate, vec![171])]),
            ),
            (
                Opcode::ORA,
                HashMap::from([
                    (AddressMode::AbsoluteY, vec![25]),
                    (AddressMode::ZeroPage, vec![5]),
                    (AddressMode::IndirectX, vec![1]),
                    (AddressMode::AbsoluteX, vec![29]),
                    (AddressMode::IndirectY, vec![17]),
                    (AddressMode::Immediate, vec![9]),
                    (AddressMode::ZeroPageX, vec![21]),
                    (AddressMode::Absolute, vec![13]),
                ]),
            ),
            (
                Opcode::PHA,
                HashMap::from([(AddressMode::Implied, vec![72])]),
            ),
            (
                Opcode::PHP,
                HashMap::from([(AddressMode::Implied, vec![8])]),
            ),
            (
                Opcode::PLA,
                HashMap::from([(AddressMode::Implied, vec![104])]),
            ),
            (
                Opcode::PLP,
                HashMap::from([(AddressMode::Implied, vec![40])]),
            ),
            (
                Opcode::RLA,
                HashMap::from([
                    (AddressMode::AbsoluteY, vec![59]),
                    (AddressMode::ZeroPage, vec![39]),
                    (AddressMode::ZeroPageX, vec![55]),
                    (AddressMode::IndirectX, vec![35]),
                    (AddressMode::Absolute, vec![47]),
                    (AddressMode::AbsoluteX, vec![63]),
                    (AddressMode::IndirectY, vec![51]),
                ]),
            ),
            (
                Opcode::ROL,
                HashMap::from([
                    (AddressMode::Absolute, vec![46]),
                    (AddressMode::AbsoluteX, vec![62]),
                    (AddressMode::ZeroPageX, vec![54]),
                    (AddressMode::Implied, vec![42]),
                    (AddressMode::ZeroPage, vec![38]),
                ]),
            ),
            (
                Opcode::ROR,
                HashMap::from([
                    (AddressMode::ZeroPage, vec![102]),
                    (AddressMode::AbsoluteX, vec![126]),
                    (AddressMode::ZeroPageX, vec![118]),
                    (AddressMode::Implied, vec![106]),
                    (AddressMode::Absolute, vec![110]),
                ]),
            ),
            (
                Opcode::RRA,
                HashMap::from([
                    (AddressMode::AbsoluteY, vec![123]),
                    (AddressMode::ZeroPageX, vec![119]),
                    (AddressMode::ZeroPage, vec![103]),
                    (AddressMode::AbsoluteX, vec![127]),
                    (AddressMode::Absolute, vec![111]),
                    (AddressMode::IndirectX, vec![99]),
                    (AddressMode::IndirectY, vec![115]),
                ]),
            ),
            (
                Opcode::RTI,
                HashMap::from([(AddressMode::Implied, vec![64])]),
            ),
            (
                Opcode::RTS,
                HashMap::from([(AddressMode::Implied, vec![96])]),
            ),
            (
                Opcode::SAX,
                HashMap::from([
                    (AddressMode::ZeroPageY, vec![151]),
                    (AddressMode::ZeroPage, vec![135]),
                    (AddressMode::IndirectX, vec![131]),
                    (AddressMode::Absolute, vec![143]),
                ]),
            ),
            (
                Opcode::SBC,
                HashMap::from([
                    (AddressMode::AbsoluteY, vec![249]),
                    (AddressMode::ZeroPage, vec![229]),
                    (AddressMode::IndirectY, vec![241]),
                    (AddressMode::ZeroPageX, vec![245]),
                    (AddressMode::Immediate, vec![233, 235]),
                    (AddressMode::AbsoluteX, vec![253]),
                    (AddressMode::Absolute, vec![237]),
                    (AddressMode::IndirectX, vec![225]),
                ]),
            ),
            (
                Opcode::SEC,
                HashMap::from([(AddressMode::Implied, vec![56])]),
            ),
            (
                Opcode::SED,
                HashMap::from([(AddressMode::Implied, vec![248])]),
            ),
            (
                Opcode::SEI,
                HashMap::from([(AddressMode::Implied, vec![120])]),
            ),
            (
                Opcode::SHX,
                HashMap::from([(AddressMode::AbsoluteY, vec![158])]),
            ),
            (
                Opcode::SHY,
                HashMap::from([(AddressMode::AbsoluteX, vec![156])]),
            ),
            (
                Opcode::SLO,
                HashMap::from([
                    (AddressMode::Absolute, vec![15]),
                    (AddressMode::ZeroPageX, vec![23]),
                    (AddressMode::AbsoluteY, vec![27]),
                    (AddressMode::IndirectY, vec![19]),
                    (AddressMode::IndirectX, vec![3]),
                    (AddressMode::AbsoluteX, vec![31]),
                    (AddressMode::ZeroPage, vec![7]),
                ]),
            ),
            (
                Opcode::SRE,
                HashMap::from([
                    (AddressMode::AbsoluteY, vec![91]),
                    (AddressMode::ZeroPageX, vec![87]),
                    (AddressMode::IndirectX, vec![67]),
                    (AddressMode::IndirectY, vec![83]),
                    (AddressMode::ZeroPage, vec![71]),
                    (AddressMode::Absolute, vec![79]),
                    (AddressMode::AbsoluteX, vec![95]),
                ]),
            ),
            (
                Opcode::STA,
                HashMap::from([
                    (AddressMode::ZeroPageX, vec![149]),
                    (AddressMode::AbsoluteX, vec![157]),
                    (AddressMode::IndirectY, vec![145]),
                    (AddressMode::IndirectX, vec![129]),
                    (AddressMode::Absolute, vec![141]),
                    (AddressMode::AbsoluteY, vec![153]),
                    (AddressMode::ZeroPage, vec![133]),
                ]),
            ),
            (
                Opcode::STX,
                HashMap::from([
                    (AddressMode::Absolute, vec![142]),
                    (AddressMode::ZeroPage, vec![134]),
                    (AddressMode::ZeroPageY, vec![150]),
                ]),
            ),
            (
                Opcode::STY,
                HashMap::from([
                    (AddressMode::Absolute, vec![140]),
                    (AddressMode::ZeroPage, vec![132]),
                    (AddressMode::ZeroPageX, vec![148]),
                ]),
            ),
            (
                Opcode::TAS,
                HashMap::from([(AddressMode::AbsoluteY, vec![155])]),
            ),
            (
                Opcode::TAX,
                HashMap::from([(AddressMode::Implied, vec![170])]),
            ),
            (
                Opcode::TAY,
                HashMap::from([(AddressMode::Implied, vec![168])]),
            ),
            (
                Opcode::TSX,
                HashMap::from([(AddressMode::Implied, vec![186])]),
            ),
            (
                Opcode::TXA,
                HashMap::from([(AddressMode::Implied, vec![138])]),
            ),
            (
                Opcode::TXS,
                HashMap::from([(AddressMode::Implied, vec![154])]),
            ),
            (
                Opcode::TYA,
                HashMap::from([(AddressMode::Implied, vec![152])]),
            ),
            (
                Opcode::XAA,
                HashMap::from([(AddressMode::Immediate, vec![139])]),
            ),
        ]);

        for op in Opcode::iter() {
            match m.get(&op) {
                Some(_) => {}
                None => panic!("Not all opcodes covered!. Missing {op}"),
            };
        }
        m
    };

    // OPCODES_VALUES is the inverse of OPCODES where the keys are the u8 byte codes and values Operation defining
    // the Opcode and AddressMode. Used in processing the CPU tick() or in disassembly for mapping a byte code back
    // to an Opcode.
    static ref NMOS_OPCODES_VALUES: Vec<Operation> = {
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

        for (op, hm) in NMOS_OPCODES.iter() {
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

        assert!(hs.len() == (1 << 8), "Didn't fill out {} opcodes. Only defined {} - {m:?}", 1<<8, hs.len());
        m
    };
}

/// Given an `Opcode` and `AddressMode` return the valid u8 values that
/// can represent it.
///
/// # Errors
/// If the `AddressMode` is not valid for this opcode an error will result.
pub fn resolve_opcode(t: Type, op: &Opcode, mode: &AddressMode) -> Result<&'static Vec<u8>> {
    let hm: &HashMap<AddressMode, Vec<u8>>;
    match t {
        Type::NMOS | Type::NMOS6510 | Type::Ricoh => {
            // SAFETY: When we built OPCODES we validated all Opcode were present
            unsafe {
                hm = NMOS_OPCODES.get(op).unwrap_unchecked();
            }
        }
        Type::CMOS => todo!("implement CMOS"),
    };
    let Some(v) = hm.get(mode) else {
        return Err(eyre!("address mode {mode} isn't valid for opcode {op}"));
    };
    Ok(v)
}

/// Given an opcode u8 value this will return the Operation struct
/// defining it. i.e. `Opcode` and `AddressMode`.
#[must_use]
pub fn opcode_op(t: Type, op: u8) -> Operation {
    match t {
        Type::NMOS | Type::NMOS6510 | Type::Ricoh =>
        // SAFETY: We know a u8 is in range due to how we build this
        //         so a direct index is fine.
        {
            NMOS_OPCODES_VALUES[usize::from(op)]
        }
        Type::CMOS => todo!("implement CMOS"),
    }
}
