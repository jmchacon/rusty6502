#[cfg(test)]
mod tests {
    use super::super::{list, Keyword, BASIC_LOAD_ADDR};
    use rusty6502::prelude::*;
    use std::error::Error;
    use std::fmt::Write as _;
    use std::fs::read;
    use std::num::Wrapping;
    use std::path::Path;
    use strum::IntoEnumIterator;

    macro_rules! list_test {
        ($suite:ident, $($name:ident: $file:literal,)*) => {
            mod $suite {
                use std::error::Error;
                use std::num::Wrapping;

                use super::*;
                $(
                    #[test]
                    fn $name() -> Result<(), Box<dyn Error>> {
                        // This should halt the cpu if a test goes off the rails.
                        let mut r = FlatRAM::new().vectors(Vectors {
                            nmi: 0x0202,
                            reset: 0x1FFe,
                            irq: 0xD001,
                        });
                        r.power_on();


                        let bytes = read(Path::new("../testdata/").join($file)).unwrap_or_else(|_| panic!("can't read file: {}", $file));

                        assert!(bytes[0] == (BASIC_LOAD_ADDR&0xFF) as u8 && bytes[1] == ((BASIC_LOAD_ADDR >> 8)&0xFF) as u8, "{} doesn't appear to be a valid Basic PRG file. Start address not 0x0801 but: {:#04X}{:02X}", $file, bytes[1], bytes[0]);

                        // Copy the program into place.
                        for i in 2..bytes.len() {
                            r.write(BASIC_LOAD_ADDR+(i-2) as u16, bytes[i]);
                        }

                        let mut got = String::new();
                        let mut pc = Wrapping::<u16>(BASIC_LOAD_ADDR);
                        loop {
                            let res = list(pc, &r).unwrap_or_else(|_| panic!("error from list"));
                            println!("{}", res.0);

                            // A 0 PC means we're done.
                            if res.1.0 == 0x0000 {
                                break;
                            }
                            _ = write!(got, "{}", res.0);
                            assert!(pc != res.1, "Looping");
                            pc = res.1;
                        }
                        assert_eq!(got, "1993 SYSPEEK(43)+256*PEEK(44)+26");
                        Ok(())
                    }
                )*
            }
        }
    }

    // These tests are images we use in cpu testing but they are c64 PRG files so
    // also work for basic sanity checking here.
    list_test!(
        list_tests,
        dadc: "dadc.prg",
        dincsbc:"dincsbc.prg",
        dincsbc_deccmp: "dincsbc-deccmp.prg",
        droradc: "droradc.prg",
        dsbc: "dsbc.prg",
        dsbc_cmp_flags: "dsbc-cmp-flags.prg",
        sbx: "sbx.prg",
        vsbx: "vsbx.prg",
    );

    #[test]
    fn bad_token() -> Result<(), Box<dyn Error>> {
        // This should halt the cpu if a test goes off the rails.
        let mut r = FlatRAM::new().vectors(Vectors {
            nmi: 0x0202,
            reset: 0x1FFe,
            irq: 0xD001,
        });
        r.power_on();

        // Create a single line which is
        // 10 ILLEGAL_OPCODE

        // NOTE: All address/line numbers are in little endian.

        // Next PC
        r.write(BASIC_LOAD_ADDR, 0x07);
        r.write(BASIC_LOAD_ADDR + 1, 0x08);

        // Line number
        r.write(BASIC_LOAD_ADDR + 2, 0x0A);
        r.write(BASIC_LOAD_ADDR + 3, 0x00);

        // Operation
        r.write(BASIC_LOAD_ADDR + 4, 0xCC);

        // NUL for end of line
        r.write(BASIC_LOAD_ADDR + 5, 0x00);

        // Next PC which is 0x0000 indicating done.
        r.write(BASIC_LOAD_ADDR + 6, 0x00);
        r.write(BASIC_LOAD_ADDR + 7, 0x00);

        let res = list(Wrapping(BASIC_LOAD_ADDR), &r);
        assert!(res.is_err());
        if let Err(e) = res {
            println!("{e}");
        }
        Ok(())
    }
}
