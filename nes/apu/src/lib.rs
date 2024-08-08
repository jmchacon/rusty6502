//! `apu` implements support for the APU chip in the original NES

use chip::Chip;
use color_eyre::Result;
use memory::Memory;

/// `APU` is the implementation for the APU chip in the original NES
pub struct APU {
    ram: [u8; 0x18],
}

impl Chip for APU {
    fn tick(&mut self) -> Result<()> {
        todo!()
    }

    fn tick_done(&mut self) -> Result<()> {
        todo!()
    }
}

impl Memory for APU {
    fn read(&self, addr: u16) -> u8 {
        match addr & 0x18 {
            0x00 | 0x01 => {}
            _ => panic!(),
        }
        0x00
    }

    fn write(&mut self, addr: u16, val: u8) {
        match addr & 0x18 {
            0x00 | 0x01 => {}
            _ => panic!(),
        }
    }

    fn power_on(&mut self) {
        todo!()
    }

    fn ram(&self, dest: &mut [u8; memory::MAX_SIZE]) {
        todo!()
    }
}

impl APU {
    /// Create a new APU
    #[must_use]
    pub fn new() -> Self {
        Self { ram: [0; 0x18] }
    }
}

impl Default for APU {
    fn default() -> Self {
        Self::new()
    }
}
