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

                    // Get the input asm and read it in.
                    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../testdata/").join(a.asm);
                    println!("trace path: {}", path.display());
                    let file = File::open(path)?;
                    let lines = io::BufReader::new(file).lines();

                    let bytes = read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../testdata/").join(a.bin))?;

                    let mut image: [u8; MAX_SIZE] = [0; MAX_SIZE];
                    for (pos, b) in bytes.iter().enumerate() {
                        image[pos] = *b;
                    }
                    let asm = parse(Type::NMOS, lines)?;

                    let diff = image
                        .iter()
                        .enumerate()
                        .filter_map(|(pos, b)| {
                            if asm.bin[pos] == *b {
                                None
                            } else {
                                Some(format!("diff - {pos:04X} - {b:02X} is actually {:02X}", asm.bin[pos]))
                            }
                        })
                        .collect::<Vec<_>>();
                    assert!(diff.is_empty(),
                        "Images don't match - Diff at these locations - \n{diff:?}");
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
    }
    undocumented_test: AssembleTest{
        asm: "undocumented.asm",
        bin: "undocumented.bin",
    }
);
