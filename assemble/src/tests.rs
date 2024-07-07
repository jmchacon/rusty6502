use rusty6502::prelude::*;
use std::{collections::HashMap, error::Error, fs::read, path::Path};

struct AssembleTest<'a> {
    asm: &'a str,
    bin: &'a str,
    cpus: Vec<CPUType>,
}

use crate::{
    find_mode, generate_output, parse_file, ASTOutput, LabelDef, MemDef, OpVal, OpVal8, Operation,
    Token, TokenVal,
};

macro_rules! assemble_test {
    ($suite:ident, $($name:ident: $assemble_test:expr)*) => {
        mod $suite {
            use super::*;

            $(
                #[test]
                fn $name() -> Result<(), Box<dyn Error>> {
                    let a = $assemble_test;

                    let bytes = read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../testdata/").join(a.bin))?;

                    let mut image: [u8; MAX_SIZE] = [0; MAX_SIZE];
                    for (pos, b) in bytes.iter().enumerate() {
                        image[pos] = *b;
                    }

                    for t in &a.cpus {
                      // Get the input asm and read it in.
                      let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../testdata/").join(a.asm);

                      let nmos = CPU6502::new(ChipDef::default());
                      let ricoh = CPURicoh::new(ChipDef::default());
                      let c6510 = CPU6510::new(ChipDef::default(), None);
                      let cmos = CPU65C02::new(ChipDef::default());
                      let rockwell = CPU65C02Rockwell::new(ChipDef::default());
                      let c65sc02 = CPU65SC02::new(ChipDef::default());

                      let cpu: &dyn CPU = match t {
                          CPUType::NMOS => &nmos,
                          CPUType::RICOH => &ricoh,
                          CPUType::NMOS6510 => &c6510,
                          CPUType::CMOS => &cmos,
                          CPUType::CMOSRockwell => &rockwell,
                          CPUType::CMOS65SC02 => &c65sc02,
                      };

                      let asm = parse_file(cpu, &path, true)?;

                      let diff = image
                          .iter()
                          .enumerate()
                          .filter_map(|(pos, b)| {
                              if asm.bin[pos] == *b {
                                  None
                             } else {
                                  Some(format!("{t:?} - diff - {pos:04X} - {b:02X} is actually {:02X}", asm.bin[pos]))
                              }
                          })
                          .collect::<Vec<_>>();
                      assert!(diff.is_empty(),
                          "{t:?} - Images don't match - Diff at these locations - \n{diff:?}");
                    }
                    Ok(())
                }
            )*
        }
    }
}

assemble_test!(
    assemble_tests,
    bcd_test: AssembleTest{
        asm: "bcd_test.asm",
        bin: "bcd_test.bin",
        cpus: vec![CPUType::NMOS, CPUType::NMOS6510, CPUType::CMOS, CPUType::RICOH],
    }
    undocumented_test: AssembleTest{
        asm: "undocumented.asm",
        bin: "undocumented.bin",
        cpus: vec![CPUType::NMOS, CPUType::NMOS6510, CPUType::RICOH],
    }
    testasm_test: AssembleTest{
        asm: "testasm.asm",
        bin: "testasm.bin",
        cpus: vec![CPUType::NMOS, CPUType::NMOS6510, CPUType::RICOH],
    }
    romasm_test: AssembleTest{
        asm: "rom.asm",
        bin: "rom.bin",
        cpus: vec![CPUType::NMOS, CPUType::NMOS6510, CPUType::RICOH],
    }
    testasm_cmos_test: AssembleTest{
        asm: "testasm-cmos.asm",
        bin: "testasm-cmos.bin",
        cpus: vec![CPUType::CMOS],
    }
);

struct BadAssembleTest<'a> {
    asm: &'a str,
    error: &'a str,
}

macro_rules! bad_assemble_test {
    ($suite:ident, $($name:ident: $bad_assemble_test:expr, $cpu:expr)*) => {
        mod $suite {
            use super::*;

            $(
                #[test]
                fn $name() -> Result<(), Box<dyn Error>> {
                    let a = $bad_assemble_test;

                    // Get the input asm and read it in.
                    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../testdata/badasm/").join(a.asm);
                    println!("trace path: {}", path.display());

                    let nmos = CPU6502::new(ChipDef::default());
                    let ricoh = CPURicoh::new(ChipDef::default());
                    let c6510 = CPU6510::new(ChipDef::default(), None);
                    let cmos = CPU65C02::new(ChipDef::default());
                    let rockwell = CPU65C02Rockwell::new(ChipDef::default());
                    let c65sc02 = CPU65SC02::new(ChipDef::default());

                    let cpu: &dyn CPU = match $cpu {
                        CPUType::NMOS => &nmos,
                        CPUType::RICOH => &ricoh,
                        CPUType::NMOS6510 => &c6510,
                        CPUType::CMOS => &cmos,
                        CPUType::CMOSRockwell => &rockwell,
                        CPUType::CMOS65SC02 => &c65sc02,
                    };

                    let asm = parse_file(cpu, &path, true);
                    assert!(asm.is_err(), "Didn't get error for {}", a.asm);
                    let e = asm.err().unwrap();
                    assert!(e.to_string().contains(a.error), "Missing error string '{}' for {e:?}", a.error);
                    println!("{e:?}");
                    Ok(())
                }
              )*
        }
    }
}

bad_assemble_test!(
  bad_assemble_tests,
  invalid_label: BadAssembleTest{
      asm: "invalid_label.asm",
      error: "invalid label",
  },
  CPUType::NMOS
  invalid_equ: BadAssembleTest{
      asm: "invalid_equ.asm",
      error: "invalid EQU",
  },
  CPUType::NMOS
  dup_location: BadAssembleTest{
      asm: "dup_location.asm",
      error: "redefine location label",
  },
  CPUType::NMOS
  invalid_opcode: BadAssembleTest{
      asm: "invalid_opcode.asm",
      error: "invalid opcode",
  },
  CPUType::NMOS
  invalid_opcode_ricoh: BadAssembleTest{
      asm: "invalid_opcode.asm",
      error: "invalid opcode",
  },
  CPUType::RICOH
  invalid_opcode_6510: BadAssembleTest{
      asm: "invalid_opcode.asm",
      error: "invalid opcode",
  },
  CPUType::NMOS6510
  invalid_org: BadAssembleTest{
      asm: "invalid_org.asm",
      error: "invalid ORG",
  },
  CPUType::NMOS
  invalid_tokens: BadAssembleTest{
      asm: "invalid_tokens.asm",
      error: "only comment after",
  },
  CPUType::NMOS
  invalid_equ_value: BadAssembleTest{
      asm: "invalid_equ_value.asm",
      error: "invalid label or value Error parsing label - $C0001. Must be of the form ^[a-zA-Z][a-zA-Z0-9_]+$",
  },
  CPUType::NMOS
  double_equ_ref: BadAssembleTest{
      asm: "double_equ_ref.asm",
      error: "can't redefine location label",
  },
  CPUType::NMOS
  bad_opcode_label: BadAssembleTest{
      asm: "bad_opcode_label.asm",
      error: "invalid label or value",
  },
  CPUType::NMOS
  bad_org: BadAssembleTest{
      asm: "bad_org.asm",
      error: "missing data",
  },
  CPUType::NMOS
  bad_label2: BadAssembleTest{
      asm: "bad_label2.asm",
      error: "Label START was never defined",
  },
  CPUType::NMOS
  bad_label3: BadAssembleTest{
      asm: "bad_label3.asm",
      error: "invalid opcode 'some'",
  },
  CPUType::NMOS
  bad_opcode: BadAssembleTest{
      asm: "bad_opcode.asm",
      error: "opcode LDA doesn't support mode",
  },
  CPUType::NMOS
  bad_opcode2: BadAssembleTest{
      asm: "bad_opcode2.asm",
      error: "opcode LDA doesn't support mode -",
  },
  CPUType::NMOS
  bad_opcode3: BadAssembleTest{
      asm: "bad_opcode3.asm",
      error: "Immediate must have an 8 bit arg",
  },
  CPUType::NMOS
  bad_opcode4: BadAssembleTest{
      asm: "bad_opcode4.asm",
      error: "Indirect must have a 16 bit arg",
  },
  CPUType::NMOS
  bad_token: BadAssembleTest{
      asm: "bad_token.asm",
      error: "Error parsing label",
  },
  CPUType::NMOS
  bad_branch: BadAssembleTest{
      asm: "bad_branch.asm",
      error: "either not 8 bit or out of range",
  },
  CPUType::NMOS
  bad_branch_ricoh: BadAssembleTest{
      asm: "bad_branch.asm",
      error: "either not 8 bit or out of range",
  },
  CPUType::RICOH
  bad_branch_6510: BadAssembleTest{
      asm: "bad_branch.asm",
      error: "either not 8 bit or out of range",
  },
  CPUType::NMOS6510
  bad_branch_cmos: BadAssembleTest{
      asm: "bad_branch.asm",
      error: "either not 8 bit or out of range",
  },
  CPUType::CMOS
  bad_branch_rockwell: BadAssembleTest{
      asm: "bad_branch.asm",
      error: "either not 8 bit or out of range",
  },
  CPUType::CMOSRockwell
  bad_branch_65sc02: BadAssembleTest{
      asm: "bad_branch.asm",
      error: "either not 8 bit or out of range",
  },
  CPUType::CMOS65SC02
  bad_bbr: BadAssembleTest{
    asm: "cmos-bbr.asm",
    error: "too many parts"
  },
  CPUType::CMOS
  bad_bbr2: BadAssembleTest{
    asm: "cmos-bbr2.asm",
    error: "Invalid zero page relative (index 10 too large - greater than 7)",
  },
  CPUType::CMOS
  bad_bbr3: BadAssembleTest{
    asm: "cmos-bbr3.asm",
    error: "either not 8 bit or out of range for relative instruction",
  },
  CPUType::CMOS
  bad_bbr4: BadAssembleTest{
    asm: "cmos-bbr4.asm",
    error: "Invalid value for ZP address for ZP Relative"
  },
  CPUType::CMOS
  bad_word: BadAssembleTest{
    asm: "bad_word.asm",
    error: "invalid label or value",
  },
  CPUType::NMOS
  bad_word2: BadAssembleTest{
    asm: "bad_word2.asm",
    error: " WORD or BYTE without value",
  },
  CPUType::NMOS
  bad_word3: BadAssembleTest{
    asm: "bad_byte.asm",
    error: "invalid BYTE value not 8 bit",
  },
  CPUType::NMOS
  bad_asciiz: BadAssembleTest{
    asm: "bad_asciiz.asm",
    error: "ASCIIZ must be of the form",
  },
  CPUType::NMOS
  bad_asciiz2: BadAssembleTest{
    asm: "bad_asciiz2.asm",
    error: "ASCIIZ without value",
  },
  CPUType::NMOS
  not_ascii: BadAssembleTest{
    asm: "not_ascii.asm",
    error: "Input line has non ASCII characters",
  },
  CPUType::NMOS
  bad_label_crossref: BadAssembleTest{
    asm: "bad_label_crossref.asm",
    error: "The following labels are referenced but never defined:",
  },
  CPUType::NMOS
  bad_include: BadAssembleTest{
    asm: "bad_include.asm",
    error: "Must supply a filename",
  },
  CPUType::NMOS
  bad_include2: BadAssembleTest{
    asm: "bad_include2.asm",
    error: "INCLUDE directive must have filename surrounded by quotes",
  },
  CPUType::NMOS
);

#[test]
#[should_panic(expected = "Single line label lbl must be a PC value!")]
fn bad_generate_label_loc_input() {
    let cpu = CPU6502::new(ChipDef::default());
    // A single line label but has a u8 value is invalid and should panic
    let mut ast_output = ASTOutput {
        ast: vec![vec![Token::Label("lbl".into())]],
        labels: HashMap::from([(
            "lbl".into(),
            LabelDef {
                val: Some(TokenVal::Val8(1)),
                file_info: crate::FileInfo {
                    filename: String::new(),
                    line_num: 1,
                },
                refs: vec![],
            },
        )]),
    };
    let e = generate_output(&cpu, &mut ast_output);
    assert!(e.is_ok(), "Random error: {e:?}");
}

#[test]
#[should_panic(expected = "Single line label lbl must be a PC value!")]
fn bad_generate_comment() {
    let cpu = CPU6502::new(ChipDef::default());
    // A single line label but has a u8 value is invalid and should panic
    let mut ast_output = ASTOutput {
        ast: vec![vec![
            Token::Label("lbl".into()),
            Token::Comment("comment".into()),
        ]],
        labels: HashMap::from([(
            "lbl".into(),
            LabelDef {
                val: Some(TokenVal::Val8(1)),
                file_info: crate::FileInfo {
                    filename: String::new(),
                    line_num: 1,
                },
                refs: vec![],
            },
        )]),
    };
    let e = generate_output(&cpu, &mut ast_output);
    assert!(e.is_ok(), "Random error: {e:?}");
}

#[test]
#[should_panic(expected = "Impossible state. One byte followed by label for WORD")]
fn bad_generate_input_bad_word() {
    let cpu = CPU6502::new(ChipDef::default());
    // Words should have 2 bytes defined, not labels.
    let mut ast_output = ASTOutput {
        ast: vec![vec![Token::Word(vec![
            MemDef {
                pc: 0,
                val: OpVal8::Val(1),
            },
            MemDef {
                pc: 0,
                val: OpVal8::Label("lbl".into()),
            },
        ])]],
        labels: HashMap::new(),
    };
    let e = generate_output(&cpu, &mut ast_output);
    assert!(e.is_ok(), "Random error: {e:?}");
}

#[test]
#[should_panic(expected = "Impossible ASCIIZ state!")]
fn bad_generate_input_bad_asciiz() {
    let cpu = CPU6502::new(ChipDef::default());
    // AsciiZ can't have label refs. Just bytes
    let mut ast_output = ASTOutput {
        ast: vec![vec![Token::AsciiZ(vec![MemDef {
            pc: 0,
            val: OpVal8::Label("lbl".into()),
        }])]],
        labels: HashMap::new(),
    };
    let e = generate_output(&cpu, &mut ast_output);
    assert!(e.is_ok(), "Random error: {e:?}");
}

#[test]
#[should_panic(expected = "no op val but not implied instruction for opcode")]
fn bad_generate_immediate_no_op_val_opcode() {
    let cpu = CPU6502::new(ChipDef::default());
    // A non implied mode means it must have an op_val.
    let mut ast_output = ASTOutput {
        ast: vec![vec![Token::Op(Operation {
            op: Opcode::LDA,
            mode: AddressMode::Immediate,
            op_val: None,
            pc: 0x0000,
            width: 1,
            x_index: false,
            y_index: false,
        })]],
        labels: HashMap::new(),
    };
    let e = generate_output(&cpu, &mut ast_output);
    assert!(e.is_ok(), "Random error: {e:?}");
}

#[test]
#[should_panic(expected = "implied mode for opcode SEI but width not 1")]
fn bad_generate_implied_opcode() {
    let cpu = CPU6502::new(ChipDef::default());
    // An implied instruction must be width 1.
    let mut ast_output = ASTOutput {
        ast: vec![vec![Token::Op(Operation {
            op: Opcode::SEI,
            mode: AddressMode::Implied,
            op_val: None,
            pc: 0x0000,
            width: 2,
            x_index: false,
            y_index: false,
        })]],
        labels: HashMap::new(),
    };
    let e = generate_output(&cpu, &mut ast_output);
    assert!(e.is_ok(), "Random error: {e:?}");
}

#[test]
#[should_panic(expected = "First value of ZeroPageRelative must be a u8")]
fn bad_generate_bad_zprel_vals() {
    let cpu = CPU65C02::new(ChipDef::default());
    // First op_val for ZPRel must be a u8.
    let mut ast_output = ASTOutput {
        ast: vec![vec![Token::Op(Operation {
            op: Opcode::BBR,
            mode: AddressMode::ZeroPageRelative,
            op_val: Some(vec![
                OpVal::Val(TokenVal::Val16(0x1234)),
                OpVal::Val(TokenVal::Val8(0x02)),
                OpVal::Val(TokenVal::Val8(0x03)),
            ]),
            pc: 0x0000,
            width: 3,
            x_index: false,
            y_index: false,
        })]],
        labels: HashMap::new(),
    };
    let e = generate_output(&cpu, &mut ast_output);
    assert!(e.is_ok(), "Random error: {e:?}");
}

#[test]
#[should_panic(expected = "got wrong data for ZP rel on op BBR and mode ZeroPageRelative")]
fn bad_generate_bad_zprel_width() {
    let cpu = CPU65C02::new(ChipDef::default());
    // First op_val for ZPRel must be a u8.
    let mut ast_output = ASTOutput {
        ast: vec![vec![Token::Op(Operation {
            op: Opcode::BBR,
            mode: AddressMode::ZeroPageRelative,
            op_val: Some(vec![
                OpVal::Val(TokenVal::Val8(0x1)),
                OpVal::Val(TokenVal::Val8(0x02)),
                OpVal::Val(TokenVal::Val8(0x03)),
            ]),
            pc: 0x0000,
            width: 2,
            x_index: false,
            y_index: false,
        })]],
        labels: HashMap::new(),
    };
    let e = generate_output(&cpu, &mut ast_output);
    assert!(e.is_ok(), "Random error: {e:?}");
}

#[test]
#[should_panic(expected = "can't have x and y index set")]
fn bad_find_mode_u8() {
    let tv = TokenVal::Val8(1);
    let op = Operation {
        op: Opcode::ADC,
        mode: AddressMode::Absolute,
        op_val: None,
        x_index: true,
        y_index: true,
        width: 3,
        pc: 0x0000,
    };
    find_mode(tv, &op);
}

#[test]
#[should_panic(expected = "can't have x and y index set")]
fn bad_find_mode_u16() {
    let tv = TokenVal::Val16(0x1234);
    let op = Operation {
        op: Opcode::ADC,
        mode: AddressMode::Absolute,
        op_val: None,
        x_index: true,
        y_index: true,
        width: 3,
        pc: 0x0000,
    };
    find_mode(tv, &op);
}
