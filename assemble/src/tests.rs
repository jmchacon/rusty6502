use rusty6502::prelude::*;
use std::{
    error::Error,
    fs::{read, File},
    io::{self, BufRead},
    path::Path,
};
use strum_macros::{Display, EnumString};

struct AssembleTest<'a> {
    asm: &'a str,
    bin: &'a str,
    cpus: Vec<Type>,
}

use crate::parse;

// Type defines the various implementations of the 6502 available.
#[derive(Copy, Clone, Debug, Display, PartialEq, Eq, EnumString)]
#[allow(clippy::upper_case_acronyms)]
enum Type {
    /// Basic NMOS 6502 including all undocumented opcodes.
    NMOS,

    /// Ricoh version used in the NES which is identical to NMOS except BCD mode is unimplemented.
    #[strum(to_string = "NMOS_RICOH")]
    RICOH,

    /// NMOS 6501 variant (used in c64) which includes I/O ports mapped at addresses 0x00 and 0x01.
    #[strum(to_string = "NMOS_6510")]
    NMOS6510,

    /// 65C02 CMOS version where undocumented opcodes are all explicit NOP's and defined.
    /// This is an implementation of the later WDC spec so will include support
    /// for WAI, STP, SMB/RMB and BBR/BBS instructions.
    CMOS,
}

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
                      let cpu: &dyn CPU = match t {
                          Type::NMOS => &nmos,
                          Type::RICOH => &ricoh,
                          Type::NMOS6510 => &c6510,
                          Type::CMOS => &cmos,
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
        cpus: vec![Type::NMOS, Type::NMOS6510, Type::CMOS, Type::RICOH],
    }
    undocumented_test: AssembleTest{
        asm: "undocumented.asm",
        bin: "undocumented.bin",
        cpus: vec![Type::NMOS, Type::NMOS6510, Type::RICOH],
    }
    testasm_test: AssembleTest{
        asm: "testasm.asm",
        bin: "testasm.bin",
        cpus: vec![Type::NMOS, Type::NMOS6510, Type::RICOH],
    }
);

struct BadAssembleTest<'a> {
    asm: &'a str,
    error: &'a str,
}

macro_rules! bad_assemble_test {
    ($suite:ident, $($name:ident: $bad_assemble_test:expr)*) => {
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
                    let asm = parse(&nmos, lines, true);
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
  }
  invalid_equ: BadAssembleTest{
      asm: "invalid_equ.asm",
      error: "invalid EQU",
  }
  dup_location: BadAssembleTest{
      asm: "dup_location.asm",
      error: "redefine location label",
  }
  invalid_opcode: BadAssembleTest{
      asm: "invalid_opcode.asm",
      error: "invalid opcode",
  }
  invalid_org: BadAssembleTest{
      asm: "invalid_org.asm",
      error: "invalid ORG",
  }
  invalid_tokens: BadAssembleTest{
      asm: "invalid_tokens.asm",
      error: "only comment after",
  }
  invalid_equ_value: BadAssembleTest{
      asm: "invalid_equ_value.asm",
      error: "not valid u8 or u16 for EQU",
  }
  double_equ_ref: BadAssembleTest{
      asm: "double_equ_ref.asm",
      error: "can't redefine EQU label",
  }
  bad_opcode_label: BadAssembleTest{
      asm: "bad_opcode_label.asm",
      error: "invalid opcode label",
  }
  bad_org: BadAssembleTest{
      asm: "bad_org.asm",
      error: "missing data",
  }
  bad_label: BadAssembleTest{
      asm: "bad_label.asm",
      error: "missing data",
  }
  bad_label2: BadAssembleTest{
      asm: "bad_label2.asm",
      error: "Label START was never defined",
  }
  bad_opcode: BadAssembleTest{
      asm: "bad_opcode.asm",
      error: "opcode LDA doesn't support mode",
  }
  bad_opcode2: BadAssembleTest{
      asm: "bad_opcode2.asm",
      error: "opcode LDA doesn't support mode -",
  }
  bad_opcode3: BadAssembleTest{
      asm: "bad_opcode3.asm",
      error: "Immediate must have an 8 bit arg",
  }
  bad_opcode4: BadAssembleTest{
      asm: "bad_opcode4.asm",
      error: "Indirect must have a 16 bit arg",
  }
  bad_token: BadAssembleTest{
      asm: "bad_token.asm",
      error: "Error parsing label",
  }
  bad_branch: BadAssembleTest{
      asm: "bad_branch.asm",
      error: "either not 8 bit or out of range",
  }
);
