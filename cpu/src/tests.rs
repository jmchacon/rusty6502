#[cfg(test)]
mod tests {
    use crate::{ChipDef, Cpu, FlatRAM, Type, Vectors};
    use memory::Memory;
    use std::error::Error;

    #[test]
    fn init_cpu() -> Result<(), Box<dyn Error>> {
        // This should halt the cpu if a test goes off the rails.
        let mut r = FlatRAM::new().vectors(Vectors {
            nmi: 0x0202,
            reset: 0x1FFe,
            irq: 0xD001,
        });
        r.power_on();

        let def = ChipDef {
            cpu_type: Type::NMOS,
            ram: &r,
            debug: false,
        };
        let mut cpu = Cpu::new(&def);

        cpu.power_on()?;
        Ok(())
    }
}
