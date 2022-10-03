#[cfg(test)]
mod tests {
    use crate::{ChipDef, Cpu, FlatRAM, Tick, Type, Vectors, P_DECIMAL};
    use color_eyre::eyre::Result;
    use memory::Memory;
    use std::collections::HashSet;

    #[test]
    fn tick_next() -> Result<()> {
        let mut ticker = Tick::default();

        assert!(ticker == Tick::Reset, "not in valid reset state");
        // Validate when we get to the end it stays on it
        loop {
            ticker = ticker.next();
            if ticker == Tick::Tick7 {
                break;
            }
        }
        ticker = ticker.next();
        assert!(ticker == Tick::Tick8, "didn't advance to tick8");
        ticker = ticker.next();
        assert!(ticker == Tick::Tick8, "didn't continue to stay in tick8");
        Ok(())
    }

    macro_rules! init_test {
        ($suite:ident, $($name:ident: $type:expr, $rand:expr,)*) => {
            mod $suite {
                use super::*;

                $(
                    #[test]
                    fn $name() -> Result<()> {
                        let mut iter: usize = 0;
                        let mut track = HashSet::new();
                        // If decimal state is random run 100 iterations
                        // and validate both states showed up. Yes..this could
                        // fail but 0.5^100 is a fairly small chance there...
                        if $rand {
                            iter = 100;
                        }
                        for _ in 0..=iter {
                            // This should halt the cpu if a test goes off the rails.
                            let mut r = FlatRAM::new().vectors(Vectors {
                                nmi: 0x0202,
                                reset: 0x1FFe,
                                irq: 0xD001,
                            });
                            r.power_on();

                            let def = ChipDef {
                                cpu_type: $type,
                                ram: &r,
                                debug: false,
                            };

                            let mut cpu = Cpu::new(&def);

                            // This should fail
                            {
                                let ret = cpu.reset();
                                assert!(ret.is_err(), "reset worked before power_on");
                            }

                            // Now it should work.
                            cpu.power_on()?;
                            if cpu.p&P_DECIMAL == P_DECIMAL {
                                track.insert(true);
                            } else {
                                track.insert(false);
                            }

                            // This should fail now.
                            {
                                let ret = cpu.power_on();
                                assert!(ret.is_err(), "power_on passes twice");
                            }
                        }
                        if $rand {
                            assert!(track.len() == 2, "didn't get both decimal states");
                        }
                        Ok(())
                    }
                )*
            }
        }
    }

    init_test!(
        init_tests,
        nmos: Type::NMOS,
        true,
        ricoh: Type::Ricoh,
        false,
        nmos6510: Type::NMOS6510,
        true,
        cmos: Type::CMOS,
        false,
    );

    macro_rules! tick_test {
        ($suite:ident, $($name:ident: $type:expr,)*) => {
            mod $suite {
                use super::*;

                $(
                    #[test]
                    fn $name() -> Result<()> {
                        // This should halt the cpu if a test goes off the rails.
                        let mut r = FlatRAM::default().vectors(Vectors {
                            nmi: 0x0202,
                            reset: 0x1FFe,
                            irq: 0xD001,
                        }).fill_value(0xAA).debug();
                        r.power_on();

                        let def = ChipDef {
                            cpu_type: $type,
                            ram: &r,
                            debug: false,
                        };

                        let mut cpu = Cpu::new(&def);

                        // This should fail as we haven't powered on/reset.
                        {
                            let ret = cpu.tick();
                            assert!(ret.is_err(), "tick worked before reset");
                        }
                        cpu.power_on()?;

                        // Now start a reset sequence and then attempt to tick. This should fail also.
                        cpu.reset()?;
                        {
                            let ret = cpu.tick();
                            assert!(ret.is_err(), "tick worked while inside reset");
                        }

                        // Finish reset
                        loop {
                            match cpu.reset() {
                                Ok(true) => break,
                                Ok(false) => continue,
                                Err(e) => return Err(e),
                            }
                        }

                        // Should work now to advance a few ticks.
                        for _ in 0..4 {
                            cpu.tick()?;
                            cpu.tick_done()?;
                        }

                        // Now validate you can't call tick_done() twice in a row or tick twice in a row.
                        {
                            let ret = cpu.tick_done();
                            assert!(ret.is_err(), "tick_done called twice");
                        }
                        {
                            cpu.tick()?;
                            let ret = cpu.tick();
                            assert!(ret.is_err(), "tick called twice");
                        }
                        Ok(())
                    }
                )*
            }
        }
    }

    tick_test!(
        tick_tests,
        nmos: Type::NMOS,
        ricoh: Type::Ricoh,
        nmos6510: Type::NMOS6510,
        //   cmos: Type::CMOS,
    );
}
