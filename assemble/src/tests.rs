use rusty6502::prelude::*;
use std::{
    error::Error,
    fs::{read, File},
    io::{self, BufRead},
    path::Path,
};

struct AssembleTest<'a> {
    asm: &'a str,
    bin: &'a str,
    cpus: Vec<CPUType>,
}

use crate::parse;

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

                      let file = File::open(path)?;
                      let lines = io::BufReader::new(file).lines();

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

                      let asm = parse(cpu, lines, true)?;

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
                    let file = File::open(path)?;
                    let lines = io::BufReader::new(file).lines();

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

                    let asm = parse(cpu, lines, true);
                    assert!(asm.is_err(), "Didn't get error for {}", a.asm);
                    let e = asm.err().unwrap();
                    assert!(e.to_string().contains(a.error), "Missing error string {} for {e:?}", a.error);
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
      error: "not valid u8 or u16 for EQU",
  },
  CPUType::NMOS
  double_equ_ref: BadAssembleTest{
      asm: "double_equ_ref.asm",
      error: "can't redefine EQU label",
  },
  CPUType::NMOS
  bad_opcode_label: BadAssembleTest{
      asm: "bad_opcode_label.asm",
      error: "invalid opcode label",
  },
  CPUType::NMOS
  bad_org: BadAssembleTest{
      asm: "bad_org.asm",
      error: "missing data",
  },
  CPUType::NMOS
  bad_label: BadAssembleTest{
      asm: "bad_label.asm",
      error: "missing data",
  },
  CPUType::NMOS
  bad_label2: BadAssembleTest{
      asm: "bad_label2.asm",
      error: "Label START was never defined",
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
    error: "index too large"
  },
  CPUType::CMOS
  bad_word: BadAssembleTest{
    asm: "bad_word.asm",
    error: "invalid WORD value not 16 bit",
  },
  CPUType::NMOS
);
