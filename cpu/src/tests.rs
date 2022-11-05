#[cfg(test)]
mod tests {
    use crate::{
        CPUError, ChipDef, Cpu, FlatRAM, OpState, Tick, Type, Vectors, P_DECIMAL, P_NEGATIVE,
        P_ZERO,
    };
    use chip::Chip;
    use color_eyre::eyre::Result;
    use memory::Memory;
    use std::collections::HashSet;
    use std::num::Wrapping;

    fn debug(s: String) {
        print!("{s}");
    }

    const RESET: u16 = 0x1FFE;
    const IRQ: u16 = 0xD001;

    fn setup(t: Type, hlt: u8, fill: u8, debug: Option<fn(String)>) -> Box<Cpu<'static>> {
        // NOTE: For tests only we simply put all this on the heap and then
        //       leak things with leak so we can handle all the setup pieces.

        // This should halt the cpu if a test goes off the rails since
        // endless execution will eventually end up at the NMI vector (it's the first
        // one) which contains HLT instructions.
        let r = FlatRAM::new()
            .vectors(Vectors {
                nmi: u16::from(hlt) << 8 | u16::from(hlt),
                reset: RESET,
                irq: IRQ,
            })
            .fill_value(fill);
        let mut r = Box::new(r);
        r.power_on();

        let def = Box::new(ChipDef {
            cpu_type: t,
            ram: Box::leak(r),
            debug: debug,
            irq: None,
            nmi: None,
            rdy: None,
        });

        Box::new(Cpu::new(Box::leak(def)))
    }

    fn step(cpu: &mut Cpu) -> Result<usize> {
        let mut cycles = 0;
        loop {
            cpu.tick()?;
            cpu.tick_done()?;
            cycles += 1;
            if cpu.op_tick == Tick::Reset {
                break;
            }
        }
        Ok(cycles)
    }

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
                           let mut cpu = setup($type, 0x12, 0xEA, None);

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
                        let mut cpu = setup($type, 0x12, 0xAA, None);

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
                                Ok(OpState::Done) => break,
                                Ok(OpState::Processing) => continue,
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

    struct NopHltTest {
        fill: u8,
        halt: u8,
        cycles: usize,
        bump: u8,
    }

    macro_rules! nop_hlt_test {
        ($suite:ident, $($name:ident: $test:expr)*) => {
            mod $suite {
                use super::*;

                $(
                    #[test]
                    fn $name() -> Result<()> {
                        let mut cpu = setup(Type::NMOS, $test.halt, $test.fill, Some(debug));
                        // Make a copy so we can compare if RAM changed.
                        let mut canonical = setup(Type::NMOS, $test.halt, $test.fill, None);
                        cpu.power_on()?;
                        canonical.a = cpu.a;
                        canonical.x = cpu.x;
                        canonical.y = cpu.y;
                        canonical.s = cpu.s;
                        canonical.p = cpu.p;

                        // Set things up so we execute 1000 NOP's before halting.
                        let end = (Wrapping(RESET) + Wrapping(u16::from($test.bump)*1000)).0;
                        cpu.ram.write(end, $test.halt);
                        cpu.ram.write(end + 1, $test.halt);
                        canonical.ram.write(end, $test.halt);
                        canonical.ram.write(end + 1, $test.halt);

                        assert!(cpu.pc == Wrapping(RESET), "PC {:04X} isn't {:04X}", cpu.pc.0, RESET);

                        let mut got: usize = 0;
                        let mut page_cross = 0;
                        let mut ret: Result<usize>;

                        // 9 clocks for a reset sequence.
                        assert!(cpu.clocks == 9, "cpu clocks wrong. expected 9 and got {}", cpu.clocks);
                        loop {
                            let pc = cpu.pc;
                            ret = step(&mut cpu);
                            let cycles = match(ret) {
                                Ok(c) => c,
                                _ => {
                                    break;
                                }
                            };
                            got += cycles;

                            if cycles != $test.cycles {
                                if cycles == $test.cycles + 1 {
                                    page_cross += 1;
                                } else {
                                    assert!(true, "cycles incorrect - got {cycles} and want {} for each instruction. On PC {pc}", $test.cycles);
                                }
                            }

                            // NOPs generally bump the PC by one but can differ with other addressing modes.
                            assert!(cpu.pc == pc+Wrapping(u16::from($test.bump)), "PC didn't increment by $bump. Got PC {:04X} and started at {pc:04X}", cpu.pc);

                            // Registers shouldn't be changing.
                            assert!(cpu.a == canonical.a && cpu.x == canonical.x && cpu.y == canonical.y && cpu.s == canonical.s && cpu.p == canonical.p, "Registers differ. CPU - {cpu:?} and saved: {canonical:?}",);

                            // We've wrapped around so abort
                            if got > (0xFFFF * 2) {
                                break;
                            }
                        }

                        // RAM shouldn't be changing but only test once.
                        for addr in 0x0000u16..=0xFFFF {
                            let got = cpu.ram.read(addr);
                            let want = canonical.ram.read(addr);
                            assert!(got == want, "RAM contents differ at {addr:04X} - got {got:02X} and want {want:02X}");
                        }


                        assert!(ret.is_err(), "Loop didn't exit with error");

                        // Should end up executing X cycles times 1000 + any page crossings + 2 for halt.
                        // NOTE: since HLT returns an error from tick() step() can't report the 2 cycles it takes so we'll
                        //       check that with clocks from the cpu.
                        let want_clocks: usize = 9 + page_cross + (1000 * $test.cycles) + 2;
                        // The cycles we recorded is 11 less than that (9 for reset plus we don't record HLT clocks in step)
                        let want = want_clocks - 11;
                        assert!(got == want, "Invalid cycle count. Stopped PC: {:04X}. Got {got} cycles and want {want} cycles", cpu.pc);
                        assert!(cpu.clocks == want_clocks, "Invalid clock count. Got {} clocks and want {want_clocks}", cpu.clocks);
                        // Safety: We know it's an error so unwrap_err is fine.
                        let err = ret.unwrap_err();
                        match err.root_cause().downcast_ref::<CPUError>() {
                            Some(CPUError::Halted{op: _}) => {},
                            _ => {
                                assert!(true, "Error isn't a CPUError::Halted. Is {err}");
                            }
                        }
                        Ok(())
                    }
                )*
            }
        }
    }

    nop_hlt_test!(
        nop_hlt_tests,
        classic_nop_0x02_hlt: NopHltTest{fill: 0xEA, halt: 0x02, cycles: 2, bump: 1}
        classic_nop_0x12_hlt: NopHltTest{fill: 0xEA, halt: 0x12, cycles: 2, bump: 1}
        classic_nop_0x22_hlt: NopHltTest{fill: 0xEA, halt: 0x22, cycles: 2, bump: 1}
        classic_nop_0x32_hlt: NopHltTest{fill: 0xEA, halt: 0x32, cycles: 2, bump: 1}
        classic_nop_0x42_hlt: NopHltTest{fill: 0xEA, halt: 0x42, cycles: 2, bump: 1}
        classic_nop_0x52_hlt: NopHltTest{fill: 0xEA, halt: 0x52, cycles: 2, bump: 1}
        classic_nop_0x62_hlt: NopHltTest{fill: 0xEA, halt: 0x62, cycles: 2, bump: 1}
        classic_nop_0x72_hlt: NopHltTest{fill: 0xEA, halt: 0x72, cycles: 2, bump: 1}
        classic_nop_0x92_hlt: NopHltTest{fill: 0xEA, halt: 0x92, cycles: 2, bump: 1}
        classic_nop_0xb2_hlt: NopHltTest{fill: 0xEA, halt: 0xB2, cycles: 2, bump: 1}
        classic_nop_0xd2_hlt: NopHltTest{fill: 0xEA, halt: 0xD2, cycles: 2, bump: 1}
        classic_nop_0xf2_hlt: NopHltTest{fill: 0xEA, halt: 0xF2, cycles: 2, bump: 1}
        nop_0x04_hlt_0x12: NopHltTest{fill: 0x04, halt: 0x12, cycles: 3, bump: 2}
        nop_0x0c_hlt_0x12: NopHltTest{fill: 0x0C, halt: 0x12, cycles: 4, bump: 3}
        nop_0x14_hlt_0x12: NopHltTest{fill: 0x14, halt: 0x12, cycles: 4, bump: 2}
        nop_0x1a_hlt_0x12: NopHltTest{fill: 0x1A, halt: 0x12, cycles: 2, bump: 1}
        nop_0x1c_hlt_0x12: NopHltTest{fill: 0x1C, halt: 0x12, cycles: 4, bump: 3}
        nop_0x34_hlt_0x12: NopHltTest{fill: 0x34, halt: 0x12, cycles: 3, bump: 2}
        nop_0x3a_hlt_0x12: NopHltTest{fill: 0x3A, halt: 0x12, cycles: 2, bump: 1}
        nop_0x3c_hlt_0x12: NopHltTest{fill: 0x3C, halt: 0x12, cycles: 4, bump: 3}
        nop_0x44_hlt_0x12: NopHltTest{fill: 0x44, halt: 0x12, cycles: 3, bump: 2}
        nop_0x54_hlt_0x12: NopHltTest{fill: 0x54, halt: 0x12, cycles: 4, bump: 2}
        nop_0x5a_hlt_0x12: NopHltTest{fill: 0x5A, halt: 0x12, cycles: 2, bump: 1}
        nop_0x5c_hlt_0x12: NopHltTest{fill: 0x5C, halt: 0x12, cycles: 4, bump: 3}
        nop_0x64_hlt_0x12: NopHltTest{fill: 0x64, halt: 0x12, cycles: 3, bump: 2}
        nop_0x74_hlt_0x12: NopHltTest{fill: 0x74, halt: 0x12, cycles: 4, bump: 2}
        nop_0x7a_hlt_0x12: NopHltTest{fill: 0x7A, halt: 0x12, cycles: 2, bump: 1}
        nop_0x7c_hlt_0x12: NopHltTest{fill: 0x7C, halt: 0x12, cycles: 4, bump: 3}
        nop_0x80_hlt_0x12: NopHltTest{fill: 0x80, halt: 0x12, cycles: 2, bump: 2}
        nop_0x82_hlt_0x12: NopHltTest{fill: 0x82, halt: 0x12, cycles: 2, bump: 2}
        nop_0x89_hlt_0x12: NopHltTest{fill: 0x89, halt: 0x12, cycles: 2, bump: 2}
        nop_0xc2_hlt_0x12: NopHltTest{fill: 0xC2, halt: 0x12, cycles: 2, bump: 2}
        nop_0xd4_hlt_0x12: NopHltTest{fill: 0xD4, halt: 0x12, cycles: 4, bump: 2}
        nop_0xda_hlt_0x12: NopHltTest{fill: 0xDA, halt: 0x12, cycles: 2, bump: 1}
        nop_0xdc_hlt_0x12: NopHltTest{fill: 0xDC, halt: 0x12, cycles: 4, bump: 3}
        nop_0xe2_hlt_0x12: NopHltTest{fill: 0xE2, halt: 0x12, cycles: 2, bump: 2}
        nop_0xf4_hlt_0x12: NopHltTest{fill: 0xF4, halt: 0x12, cycles: 4, bump: 2}
        nop_0xfa_hlt_0x12: NopHltTest{fill: 0xFA, halt: 0x12, cycles: 2, bump: 1}
        nop_0xfc_hlt_0x12: NopHltTest{fill: 0xFC, halt: 0x12, cycles: 4, bump: 3}
    );

    macro_rules! load_test {
        ($suite:ident, $($name:ident: $x:expr, $expected:expr)*) => {
            mod $suite {
                use super::*;

                $(
                    #[test]
                    fn $name() -> Result<()> {
                        let mut cpu = setup(Type::NMOS, 0x12, 0xEA, Some(debug));
                        cpu.power_on()?;

                        cpu.ram.write(0x1FFE, 0xA1); // LDA ($EA,x)
                        cpu.ram.write(0x1FFF, 0xEA);
                        cpu.ram.write(0x2000, 0xA1); // LDA ($FF,x)
                        cpu.ram.write(0x2001, 0xFF);
                        cpu.ram.write(0x2002, 0x12); // HLT

                        // (0x00EA) points to 0x650F
                        cpu.ram.write(0x00EA, 0x0F);
                        cpu.ram.write(0x00EB, 0x65);

                        // (0x00FA) points to 0x551F
                        cpu.ram.write(0x00FA, 0x1F);
                        cpu.ram.write(0x00FB, 0x55);

                        // (0x00FF) points to 0xA1FA (since 0x0000 is 0xA1)
                        cpu.ram.write(0x00FF, 0xFA);
                        cpu.ram.write(0x0000, 0xA1);

                        // (0x001F) points to 0xA20A
                        cpu.ram.write(0x000F, 0x0A);
                        cpu.ram.write(0x0010, 0xA2);

                        // For LDA ($FA,x) X = 0x00
                        cpu.ram.write(0x650F, 0xAB);
                        // For LDA ($FA,x) X = 0x10
                        cpu.ram.write(0x551F, 0xCD);

                        // For LDA ($FF,x) X = 0x00
                        cpu.ram.write(0xA1FA, 0xEF);
                        // For LDA ($FF,x) X = 0x10
                        cpu.ram.write(0xA20A, 0x00);

                        // Do reset
                        loop {
                            match cpu.reset() {
                                Ok(OpState::Done) => break,
                                Ok(OpState::Processing) => continue,
                                Err(e) => return Err(e),
                            }
                        }

                        for (iteration, e) in $expected.iter().enumerate() {
                            cpu.a = Wrapping(*e) - Wrapping(1);
                            cpu.x = Wrapping($x);
                            // These all take 6 cycles so validate
                            let cycles = step(&mut cpu)?;
                            assert!(cycles == 6, "Invalid cycle count: {cycles} expected 6");
                            assert!(cpu.a.0 == *e, "A register doesn't have correct value for iteration {iteration}. Got {:02X} and want {e:02X}", cpu.a.0);
                            let got = cpu.p & P_ZERO == 0x00;
                            let want = *e != 0x00;
                            assert!(
                                got == want,
                                "Z flag is incorrect. Got {:02X} and A is {:02X}",
                                cpu.p,
                                cpu.a.0
                            );
                            let got = cpu.p & P_NEGATIVE == 0x00;
                            let want = *e < 0x80;
                            assert!(
                                got == want,
                                "N flag is incorrect. Got {:02X} and A is {:02X}",
                                cpu.p,
                                cpu.a.0
                            )
                        }
                        Ok(())
                    }
                )*
            }
        }
    }

    load_test!(
        load_tests,
        x_is_zero: 0x00, vec![0xAB, 0xEF]
        x_is_10: 0x10, vec![0xCD, 0x00]
    );

    macro_rules! store_test {
        ($suite:ident, $($name:ident: $a:expr, $x:expr, $expected:expr)*) => {
            mod $suite {
                use super::*;

                $(
                    #[test]
                    fn $name() -> Result<()> {
                        let mut cpu = setup(Type::NMOS, 0x12, 0xEA, Some(debug));
                        cpu.power_on()?;

                        cpu.ram.write(0x1FFE, 0x81); // STA ($EA,x)
                        cpu.ram.write(0x1FFF, 0xEA);
                        cpu.ram.write(0x2000, 0x81); // STA ($FF,x)
                        cpu.ram.write(0x2001, 0xFF);
                        cpu.ram.write(0x2002, 0x12); // HLT

                        // (0x00EA) points to 0x650F
                        cpu.ram.write(0x00EA, 0x0F);
                        cpu.ram.write(0x00EB, 0x65);

                        // (0x00FA) points to 0x551F
                        cpu.ram.write(0x00FA, 0x1F);
                        cpu.ram.write(0x00FB, 0x55);

                        // (0x00FF) points to 0xA1FA (since 0x0000 is 0xA1)
                        cpu.ram.write(0x00FF, 0xFA);
                        cpu.ram.write(0x0000, 0xA1);

                        // (0x001F) points to 0xA20A
                        cpu.ram.write(0x000F, 0x0A);
                        cpu.ram.write(0x0010, 0xA2);

                        // For STA ($FA,x) X = 0x00
                        cpu.ram.write(0x650F, 0x00);
                        // For STA ($FA,x) X = 0x10
                        cpu.ram.write(0x551F, 0x00);

                        // For STA ($FF,x) X = 0x00
                        cpu.ram.write(0xA1FA, 0x00);
                        // For STA ($FF,x) X = 0x10
                        cpu.ram.write(0xA20A, 0x00);

                        // Do reset
                        loop {
                            match cpu.reset() {
                                Ok(OpState::Done) => break,
                                Ok(OpState::Processing) => continue,
                                Err(e) => return Err(e),
                            }
                        }

                        for (iteration, e) in $expected.iter().enumerate() {
                            // These don't change status flags but we do verify that too.
                            let p = cpu.p;
                            cpu.a = Wrapping($a);
                            cpu.x = Wrapping($x);
                            // These all take 6 cycles so validate
                            let cycles = step(&mut cpu)?;
                            assert!(cycles == 6, "Invalid cycle count: {cycles} expected 6");
                            let want = cpu.ram.read(*e);
                            assert!(want == cpu.a.0, "A register doesn't have correct value for iteration {iteration}. Got {:02X} from {e:04X} and want {want:02X}", cpu.a.0);
                            assert!(p == cpu.p, "Status changed. Orig {p:02X} and got {:02X}", cpu.p);
                        }
                        Ok(())
                    }
                )*
            }
        }
    }

    store_test!(
        store_tests,
        x_is_zero: 0xAA, 0x00, vec![0x650F, 0xA1FA]
        x_is_10: 0x55, 0x10, vec![0x551F, 0xA20A]
    );
}
