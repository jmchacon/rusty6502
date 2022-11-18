#[cfg(test)]
mod tests {
    use crate::{
        CPUError, ChipDef, Cpu, Flags, FlatRAM, InterruptState, InterruptStyle, OpState, Tick,
        Type, Vectors, P_B, P_DECIMAL, P_INTERRUPT, P_NEGATIVE, P_S1, P_ZERO, STACK_START,
    };
    use chip::Chip;
    use color_eyre::eyre::{eyre, Result};
    use irq::Sender;
    use memory::Memory;
    use ringbuffer::{AllocRingBuffer, RingBufferExt, RingBufferWrite};
    use std::cell::RefCell;
    use std::collections::HashSet;
    use std::fmt::Write;
    use std::fs::read;
    use std::num::Wrapping;
    use std::path::Path;

    // Debug provides a way to capture debug output from a Cpu
    // without having to grab everything. Normally a few recent
    // instructions is all one needs. The functional test below for
    // instance runs millions of instructions which can lead to very
    // large buffering during a test run.
    struct Debug {
        buf: RefCell<AllocRingBuffer<String>>,
    }

    impl Debug {
        fn new(cap: usize) -> Self {
            Debug {
                buf: RefCell::new(AllocRingBuffer::with_capacity(cap)),
            }
        }

        fn debug(&self, s: String) {
            self.buf.borrow_mut().push(s)
        }

        fn dump(&self, s: String) -> String {
            let mut out = String::new();
            writeln!(out, "\nFAIL: {s}\n\nExecution buffer:\n").unwrap();
            for i in self.buf.borrow().iter() {
                write!(out, "{i}").unwrap();
            }
            out
        }
    }

    // tester is a wrapper around assert which is passed a `Debug` so any failure
    // will also print out the contents of the circular buffer prefixed by the error expression.
    macro_rules! tester {
        ($test:expr, $dumper:ident, $error:expr) => {
            assert!($test, "{}", $dumper.dump(format!($error)))
        };
    }

    const RESET: u16 = 0x1FFE;
    const IRQ_ADDR: u16 = 0xD001;

    fn setup(
        t: Type,
        hlt: u16,
        fill: u8,
        irq: Option<&'static dyn Sender>,
        nmi: Option<&'static dyn Sender>,
        debug: Option<&'static dyn Fn(String)>,
    ) -> Box<Cpu<'static>> {
        // NOTE: For tests only we simply put all this on the heap and then
        //       leak things with leak so we can handle all the setup pieces.

        // This should halt the cpu if a test goes off the rails since
        // endless execution will eventually end up at the NMI vector (it's the first
        // one) which contains HLT instructions.
        let r = FlatRAM::new()
            .vectors(Vectors {
                nmi: hlt,
                reset: RESET,
                irq: IRQ_ADDR,
            })
            .fill_value(fill);
        let mut r = Box::new(r);
        r.power_on();

        let def = Box::new(ChipDef {
            cpu_type: t,
            ram: Box::leak(r),
            debug: debug,
            irq: irq,
            nmi: nmi,
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
                           let mut cpu = setup($type, 0x1212, 0xEA, None, None, None);

                            // This should fail
                            {
                                let ret = cpu.reset();
                                assert!(ret.is_err(), "reset worked before power_on");
                            }

                            // Now it should work.
                            cpu.power_on()?;
                            if cpu.p&P_DECIMAL == Flags(P_DECIMAL) {
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
                        let mut cpu = setup($type, 0x1212, 0xAA, None, None, None);

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

    #[derive(Debug)]
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
                        let test = $test;
                        let addr = u16::from(test.halt) << 8 | u16::from(test.halt);
                        let d = Box::leak(Box::new(Debug::new(128)));
                        let debug = Box::leak(Box::new(|s| d.debug(s)));
                        let mut cpu = setup(Type::NMOS, addr, $test.fill, None, None, Some(debug));
                        // Make a copy so we can compare if RAM changed.
                        let mut canonical = setup(Type::NMOS, addr, test.fill, None, None, None);
                        cpu.power_on()?;
                        canonical.a = cpu.a;
                        canonical.x = cpu.x;
                        canonical.y = cpu.y;
                        canonical.s = cpu.s;
                        canonical.p = cpu.p;

                        // Set things up so we execute 1000 NOP's before halting.
                        let end = (Wrapping(RESET) + Wrapping(u16::from(test.bump)*1000)).0;
                        cpu.ram.write(end, test.halt);
                        cpu.ram.write(end + 1, test.halt);
                        canonical.ram.write(end, test.halt);
                        canonical.ram.write(end + 1, test.halt);

                        let got = cpu.pc.0;
                        let want = RESET;
                        tester!(got == want, d, "PC {got:04X} isn't {want:04X}");

                        let mut tot: usize = 0;
                        let mut page_cross = 0;
                        let mut ret: Result<usize>;

                        // 9 clocks for a reset sequence.
                        let got = cpu.clocks;
                        let want = 9;
                        tester!(got == want, d, "cpu clocks wrong. expected {want} and got {got}");
                        loop {
                            let pc = cpu.pc;
                            ret = step(&mut cpu);
                            let cycles = match(ret) {
                                Ok(c) => c,
                                _ => {
                                    break;
                                }
                            };
                            tot += cycles;

                            if cycles != test.cycles {
                                if cycles == test.cycles + 1 {
                                    page_cross += 1;
                                } else {
                                    let got = cycles;
                                    let want = test.cycles;
                                    tester!(true, d, "cycles incorrect - got {got} and want {want} or {want}+1 for each instruction.");
                                }
                            }

                            // NOPs generally bump the PC by one but can differ with other addressing modes.
                            let got = cpu.pc.0;
                            let want = (pc+Wrapping(u16::from(test.bump))).0;
                            tester!(got == want, d, "PC didn't increment by bump in {test:?}. Got PC {got:04X} and started at PC {pc:04X}");

                            // Registers shouldn't be changing. Move canonical PC to match then compare.
                            canonical.pc = cpu.pc;
                            tester!(cpu == canonical, d, "Registers differ. CPU - \n{cpu}\n and saved - \n{canonical}");

                            // We've wrapped around so abort
                            if tot > (0xFFFF * 2) {
                                break;
                            }
                        }

                        // RAM shouldn't be changing but only test once.
                        for addr in 0x0000u16..=0xFFFF {
                            let got = cpu.ram.read(addr);
                            let want = canonical.ram.read(addr);
                            tester!(got == want, d, "RAM contents differ at {addr:04X} - got {got:02X} and want {want:02X}");
                        }


                        tester!(ret.is_err(), d, "Loop didn't exit with error");

                        // Should end up executing X cycles times 1000 + any page crossings + 2 for halt.
                        // NOTE: since HLT returns an error from tick() step() can't report the 2 cycles it takes so we'll
                        //       check that with clocks from the cpu.
                        let want_clocks: usize = 9 + page_cross + (1000 * $test.cycles) + 2;
                        // The cycles we recorded is 11 less than that (9 for reset plus we don't record HLT clocks in step)
                        let got = tot;
                        let want = want_clocks - 11;
                        let pc = cpu.pc.0;
                        tester!(got == want, d, "Invalid cycle count. Stopped PC: {pc:04X}. Got {got} cycles and want {want} cycles");
                        let got = cpu.clocks;
                        let want = want_clocks;
                        tester!(got == want, d, "Invalid clock count. Got {got} clocks and want {want}");
                        // Safety: We know it's an error so unwrap_err is fine.
                        let err = ret.unwrap_err();
                        match err.root_cause().downcast_ref::<CPUError>() {
                            Some(CPUError::Halted{op: _}) => {},
                            _ => {
                                  tester!(true, d, "Error isn't a CPUError::Halted. Is '{err}'");
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
                        let d = Box::leak(Box::new(Debug::new(128)));
                        let debug = Box::leak(Box::new(|s| d.debug(s)));
                        let mut cpu = setup(Type::NMOS, 0x1212, 0xEA, None, None, Some(debug));
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
                            tester!(cycles == 6, d, "Invalid cycle count: {cycles} expected 6");
                            let got = cpu.a.0;
                            let want = *e;
                            tester!(got == want, d, "A register doesn't have correct value for iteration {iteration}. Got {got:02X} and want {want:02X})");
                            let got = cpu.p & P_ZERO == Flags(0x00);
                            let want = *e != 0x00;
                            let got_p = cpu.p;
                            let got_a = cpu.a.0;
                            tester!(
                                got == want, d,
                                "Z flag is incorrect. Got {got_p} and A is {got_a:02X}");
                            let got = cpu.p & P_NEGATIVE == Flags(0x00);
                            let want = *e < 0x80;
                            tester!(
                                got == want, d,
                                "N flag is incorrect. Got {got_p} and A is {got_a:02X}");
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
                        let d = Box::leak(Box::new(Debug::new(128)));
                        let debug = Box::leak(Box::new(|s| d.debug(s)));
                        let mut cpu = setup(Type::NMOS, 0x1212, 0xEA, None, None, Some(debug));
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
                            tester!(cycles == 6, d, "Invalid cycle count: {cycles} expected 6");
                            let got = cpu.a.0;
                            let want = cpu.ram.read(*e);
                            tester!(got == want, d, "A register doesn't have correct value for iteration {iteration}. Got {got:02X} from {e:04X} and want {want:02X}");
                            let got_p = cpu.p;
                            tester!(got_p == p, d, "Status changed. Orig {p} and got {got_p}");
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

    struct Irq {
        raised: RefCell<bool>,
    }

    impl Sender for Irq {
        fn raised(&self) -> bool {
            *self.raised.borrow()
        }
    }

    #[test]
    fn irq_and_nmi() -> Result<()> {
        // This test is little more serial and long than other tests as the corner cases with
        // interrupt handling only occur when triggered on specific ticks and while in certain states.
        // So this has to be done clock by clock and conditions checked at each.

        let nmi: u16 = 0x0202; // If executed should halt the processor but we'll put code at this PC.
        let i = Box::leak(Box::new(Irq {
            raised: RefCell::new(false),
        }));
        let n = Box::leak(Box::new(Irq {
            raised: RefCell::new(false),
        }));

        // TODO(jchacon): Make this a macro so we can test for CMOS too and the D bit flips.
        let d = Box::leak(Box::new(Debug::new(128)));
        let debug = Box::leak(Box::new(|s| d.debug(s)));
        let mut cpu = setup(Type::NMOS, nmi, 0xEA, Some(i), Some(n), Some(debug));
        cpu.power_on()?;

        cpu.ram.write(IRQ_ADDR, 0x69); // ADC #AB
        cpu.ram.write(IRQ_ADDR + 1, 0xAB);
        cpu.ram.write(IRQ_ADDR + 2, 0x40); // RTI
        cpu.ram.write(nmi, 0x40); // RTI
        cpu.ram.write(RESET, 0xEA); // NOP
        cpu.ram.write(RESET + 1, 0x00); // BRK #00
        cpu.ram.write(RESET + 2, 0x00);
        cpu.ram.write(RESET + 3, 0xD0); // BNE +2
        cpu.ram.write(RESET + 4, 0x00);
        cpu.ram.write(RESET + 5, 0xD0); // BNE +2
        cpu.ram.write(RESET + 6, 0x00);

        // Set D on up front and I off
        cpu.p |= P_DECIMAL;
        cpu.p &= !P_INTERRUPT;

        // Set A to 0
        cpu.a = Wrapping(0x00);

        // Now wrap this into a RefCell so we can create verify below and use it mutablely there
        // but still be able to peek inside to check other invariants later.
        let wrapped_cpu = RefCell::new(cpu);

        // Save a copy of P so we can compare
        let saved_p = wrapped_cpu.borrow().p;

        let verify = |irq: bool, nmi: bool, state: &str, done: bool| -> Result<()> {
            *i.raised.borrow_mut() = irq;
            *n.raised.borrow_mut() = nmi;

            // We don't use Step because we want to inspect/change things on a per tick basis.
            let c = wrapped_cpu.borrow();
            println!("pre: {state} tick: {} irq: {irq} nmi: {nmi} done: {done} irq_raised: {} skip: {} interrupt_state: {}", c.op_tick, c.irq_raised, c.skip_interrupt, c.interrupt_state);
            drop(c);
            wrapped_cpu.borrow_mut().tick()?;
            wrapped_cpu.borrow_mut().tick_done()?;
            let c = wrapped_cpu.borrow();
            println!("post: {state} tick: {} irq: {irq} nmi: {nmi} done: {done} irq_raised: {} skip: {} interrupt_state: {}", c.op_tick, c.irq_raised, c.skip_interrupt, c.interrupt_state);
            Ok(())
        };

        verify(false, false, "First NOP", false)?;

        // IRQ but should finish instruction and set PC to RESET+1
        let state = "2nd NOP";
        verify(true, false, state, true)?;
        let got = wrapped_cpu.borrow().pc.0;
        let want = RESET + 1;
        tester!(
            got == want,
            d,
            "{state}: got wrong PC {got:04X} want {want:04X}"
        );
        // Verify P still has S1 and D set
        let got = wrapped_cpu.borrow().p;
        let want = Flags(P_S1 | P_DECIMAL);
        tester!(got == want, d, "{state}: got wrong flags {got} want {want}");

        // Don't assert IRQ anymore as should be cached state. Also this should take 7 cycles.
        let state = "IRQ setup";
        for _ in 0..6 {
            verify(false, false, state, false)?;
        }
        verify(false, false, state, true)?;
        let got = wrapped_cpu.borrow().pc.0;
        let want = IRQ_ADDR;
        tester!(
            got == want,
            d,
            "{state}: got wrong PC {got:04X} want {want:04X}"
        );
        // Verify the only things set in flags right now are S1 and I and D. D shouldn't be cleared for NMOS.
        let got = wrapped_cpu.borrow().p;
        let want = Flags(P_S1 | P_INTERRUPT | P_DECIMAL);
        tester!(got == want, d, "{state}: got wrong flags {got} want {want}");
        tester!(
            wrapped_cpu.borrow().irq_raised == InterruptStyle::None,
            d,
            "{state}: IRQ wasn't cleared after run"
        );
        tester!(
            wrapped_cpu.borrow().interrupt_state == InterruptState::None,
            d,
            "{state}: running interrupt still?"
        );

        // Pull P off the stack and verify the B bit didn't get set.
        let c = wrapped_cpu.borrow();
        let addr = (c.s + Wrapping(1)).0;
        let got = Flags(c.ram.read(u16::from(addr) + STACK_START));
        tester!(
            got == saved_p,
            d,
            "{state}: flags aren't correct. Didn't match original. got {got} want {saved_p}"
        );
        drop(c);

        // Now set IRQ. Should still let this instruction finish since the first instruction
        // of a handler always completes before we trigger another handler.
        let state = "ADC #AB";
        verify(true, false, state, false)?;
        // Now set NMI also and it should win.
        verify(true, true, state, true)?;
        let got = wrapped_cpu.borrow().a.0;
        let want = 0x11; // TODO(jchacon): 0xAB for non BCD
        tester!(
            got == want,
            d,
            "{state}: A doesn't match. got {got:02X} and want {want:02X}"
        );

        // NMI setup takes 7 cycles too.
        let state = "NMI setup";
        for _ in 0..6 {
            verify(false, false, state, false)?;
        }
        verify(false, false, state, true)?;
        let got = wrapped_cpu.borrow().pc.0;
        let want = nmi;
        tester!(
            got == want,
            d,
            "{state}: got wrong PC {got:04X} want {want:04X}"
        );
        tester!(
            wrapped_cpu.borrow().irq_raised == InterruptStyle::None,
            d,
            "{state}: IRQ wasn't cleared after run"
        );
        tester!(
            wrapped_cpu.borrow().interrupt_state == InterruptState::None,
            d,
            "{state}: running interrupt still?"
        );

        // Should be an RTI that takes 6 cycles
        let state = "First RTI";
        for _ in 0..5 {
            verify(false, false, state, false)?;
        }
        verify(false, false, state, true)?;
        let got = wrapped_cpu.borrow().pc.0;
        let want = IRQ_ADDR + 2;
        tester!(
            got == want,
            d,
            "{state}: got wrong PC {got:04X} want {want:04X}"
        );

        // Another RTI
        let state = "Second RTI";
        for _ in 0..5 {
            verify(false, false, state, false)?;
        }
        verify(false, false, state, true)?;
        let got = wrapped_cpu.borrow().pc.0;
        let want = RESET + 1;
        tester!(
            got == want,
            d,
            "{state}: got wrong PC {got:04X} want {want:04X}"
        );
        let got = wrapped_cpu.borrow().p;
        tester!(
            got == saved_p,
            d,
            "{state}: flags didn't reset got {got} and want {saved_p}"
        );

        // Start running BRK and interrupt part wayn through (with NMI) which should complete BRK
        // but skip it upon return. This means running 5 ticks normally.
        let state = "BRK";
        for _ in 0..5 {
            verify(false, false, state, false)?;
        }
        // Now set NMI
        verify(false, true, state, false)?;
        // Now should jump
        verify(false, false, state, true)?;
        let got = wrapped_cpu.borrow().pc.0;
        let want = nmi;
        tester!(got == want, d, "{state}: Got wrong PC {got} want {want}");
        // Pull P off the stack and verify the B bit did get set even though we're in an NMI handler.
        let addr = (wrapped_cpu.borrow().s + Wrapping(1)).0;
        let got = Flags(wrapped_cpu.borrow().ram.read(STACK_START + u16::from(addr)));
        let want = saved_p | P_B;
        let c = wrapped_cpu.borrow();
        tester!(got == want, d, "{state}: Flags aren't correct. Don't include P_B even for NMI. Got {got} and want {want} - cpu: {c}");
        drop(c);
        tester!(
            wrapped_cpu.borrow().irq_raised == InterruptStyle::None,
            d,
            "{state}: IRQ wasn't cleared after run"
        );
        tester!(
            wrapped_cpu.borrow().interrupt_state == InterruptState::None,
            d,
            "{state}: running interrupt still?"
        );

        // Yet another RTI
        let state = "3rd RTI";
        for _ in 0..5 {
            verify(false, false, state, false)?;
        }
        verify(false, false, state, true)?;
        let got = wrapped_cpu.borrow().pc.0;
        let want = RESET + 3;
        tester!(
            got == want,
            d,
            "{state}: got wrong PC {got:04X} want {want:04X}"
        );

        // Now we're going to run BNE+2 (so the next instruction) and set NMI in the middle of it.
        // It shoudn't start that processing until after this and the next instruction.
        // These take 3 cycles since they aren't page boundary crossing.
        let state = "1st BNE";
        verify(false, false, state, false)?;
        verify(false, true, state, false)?;
        verify(false, false, state, true)?;
        // PC should have advanced to the next instruction.
        let got = wrapped_cpu.borrow().pc.0;
        let want = RESET + 5;
        tester!(
            got == want,
            d,
            "{state}: got wrong PC {got:04X} want {want:04X}"
        );
        // And it should advance again into the next instruction.
        let state = "2nd BNE";
        verify(false, false, state, false)?;
        let got = wrapped_cpu.borrow().pc.0;
        let want = RESET + 6;
        tester!(
            got == want,
            d,
            "{state}: got wrong PC {got:04X} want {want:04X}"
        );
        // And then finish with NMI set again but it won't skip this time.
        verify(false, true, state, false)?;
        verify(false, false, state, true)?;
        // Now it should start an NMI
        let state = "2nd NMI setup";
        for _ in 0..6 {
            verify(false, false, state, false)?;
        }
        verify(false, false, state, true)?;
        let got = wrapped_cpu.borrow().pc.0;
        let want = nmi;
        tester!(
            got == want,
            d,
            "{state}: got wrong PC {got:04X} want {want:04X}"
        );

        // Should be another RTI
        let state = "4th RTI";
        for _ in 0..5 {
            verify(false, false, state, false)?;
        }
        verify(false, false, state, true)?;
        let got = wrapped_cpu.borrow().pc.0;
        let want = RESET + 7;
        tester!(
            got == want,
            d,
            "{state}: got wrong PC {got:04X} want {want:04X}"
        );

        // Finally fire off an NMI at the start of this NOP which should immediately run the interrupt.
        let state = "3rd NMI setup";
        verify(false, true, state, false)?;
        for _ in 0..5 {
            verify(false, false, state, false)?;
        }
        verify(false, false, state, true)?;
        let got = wrapped_cpu.borrow().pc.0;
        let want = nmi;
        tester!(
            got == want,
            d,
            "{state}: got wrong PC {got:04X} want {want:04X}"
        );

        Ok(())
    }

    struct RomTest<'a> {
        filename: &'a str,
        cpu: Type,
        start_pc: u16,
        end_check: fn(u16, &Cpu) -> bool,
        success_check: fn(u16, &Cpu) -> Result<()>,
        expected_cycles: Option<usize>,
        expected_instructions: Option<usize>,
    }

    macro_rules! rom_test {
        ($suite:ident, $($name:ident: $rom_test:expr)*) => {
            mod $suite {
                use super::*;

                $(
                    #[test]
                    fn $name() -> Result<()> {
                        let r = $rom_test;
                        // Initialize as always but then we'll overwrite it with a ROM image.
			            // For this we'll use BRK and a vector which if executed should halt the processor.
                        let d = Box::leak(Box::new(Debug::new(8192)));
                        let debug = Box::leak(Box::new(|s| d.debug(s)));
                        let mut cpu = setup(r.cpu, 0x0202, 0x00, None, None, Some(debug));
                        cpu.power_on()?;

                        // Get the input ROM and poke it into place.
                        let bytes = read(Path::new("../testdata/").join(r.filename))?;

                        for (addr, b) in bytes.iter().enumerate() {
                            cpu.ram.write(addr as u16, *b);
                        }

                        // Do reset
                        loop {
                            match cpu.reset() {
                                Ok(OpState::Done) => break,
                                Ok(OpState::Processing) => continue,
                                Err(e) => return Err(e),
                            }
                        }

                        cpu.pc = Wrapping($rom_test.start_pc);

                        let mut total_cycles: usize = 0;
                        let mut total_instructions: usize = 0;

                        loop {
                            let old_pc = cpu.pc.0;
                            let cycles = step(&mut cpu)?;
                            total_cycles += cycles;
                            total_instructions += 1;

                            if (r.end_check)(old_pc, &cpu) {
                                let res = (r.success_check)(old_pc, &cpu);
                                if let Err(err) = res {
                                    tester!(true, d, "{err}");
                                }
                                break;
                            }
                        }

                        let got = total_cycles;
                        if let Some(want) = r.expected_cycles {
                            tester!(got == want, d, "cycles don't match: got {got} and want {want}");
                        }

                        let got = total_instructions;
                        if let Some(want) = r.expected_instructions {
                            tester!(got == want, d, "instructions don't match: got {got} and want {want}");
                        }
                        Ok(())
                    }
                )*
            }
        }
    }

    rom_test!(
        rom_tests,
        functional_test: RomTest{
            filename: "6502_functional_test.bin",
            cpu: Type::NMOS,
            start_pc: 0x0400,
            end_check: |old, cpu| {
                old == cpu.pc.0
            },
            success_check: |_old, cpu| {
                if cpu.pc.0 == 0x3469 {
                    return Ok(());
                }
                Err(eyre!("CPU looping at PC: 0x{:04X}", cpu.pc.0))
            },
            expected_cycles: Some(96241367),
            expected_instructions: Some(30646177),
        }
        undocumented_opcodes_test: RomTest{
            filename: "undocumented.bin",
            cpu: Type::NMOS,
            start_pc: 0xC000,
            end_check: |old, cpu| {
                old == cpu.pc.0
            },
            success_check: |_old, cpu| {
                if cpu.pc.0 == 0xC123 {
                    return Ok(());
                }
                Err(eyre!("CPU looping at PC: 0x{:04X}", cpu.pc.0))
            },
            // No expected cycles/instructions because OAL can generate different paths.
            expected_cycles: None,
            expected_instructions: None,
        }
    );
}
