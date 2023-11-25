//! `ppu` implements support for the PPU chip in the original NES

use chip::Chip;
use color_eyre::Result;
use memory::Memory;

/// `PPU` is the implementation for the PPU chip in the original NES
pub struct PPU {
    mapper: Box<dyn PPUMapper>,
    pallete_ram: [u8; 0x20],
    oam_ram: [u8; 0x100],
    oam_ram_addr: usize,
}

impl Chip for PPU {
    fn tick(&mut self) -> Result<()> {
        todo!()
    }

    fn tick_done(&mut self) -> Result<()> {
        todo!()
    }
}

/// NES cartridges have a variety of mappers depending on how much ROM/RAM
/// is on the cart and the method used to trigger bank switching.
/// As the RAM range from 0x2000-0x2FFF is generally 2K+mirrored but can
/// change per mapper the mapper is responsible for handling this range entirely.
pub trait PPUMapper {
    /// Read a given memory location. This may be mirrored.
    fn read(&self, addr: u16) -> u8;

    /// Write a given memory location.
    fn write(&mut self, addr: u16, val: u8);
}

impl Memory for PPU {
    fn read(&self, addr: u16) -> u8 {
        match addr & 0x08 {
            0x00 | 0x01 => {}
            _ => panic!(),
        }
        0x00
    }

    fn write(&mut self, addr: u16, val: u8) {
        match addr & 0x08 {
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

impl PPU {
    /// Create a new PPU
    #[must_use]
    pub fn new(mapper: Box<dyn PPUMapper>) -> Self {
        Self {
            mapper,
            pallete_ram: [0; 0x20],
            oam_ram: [0; 0x100],
            oam_ram_addr: 0x000,
        }
    }

    /// A `Memory` implementation which handles the public register locations
    /// for the PPU.
    pub fn ram(&mut self) -> &dyn Memory {
        self
    }

    /// For use in OAM DMA transfers. Every write will write to the current OAM
    /// addr and then increment (and rollover) the addr.
    pub fn oam_write(&mut self, val: u8) {
        self.oam_ram[self.oam_ram_addr] = val;
        self.oam_ram_addr += 1;
        self.oam_ram_addr &= 0xFF;
    }

    fn ppu_read(&self, addr: u16) -> u8 {
        if addr < 0x3F00 {
            return self.mapper.read(addr);
        }
        self.pallete_ram[usize::from(addr & 0x20)]
    }

    fn ppu_write(&mut self, addr: u16, val: u8) {
        if addr < 0x3F00 {
            return self.mapper.write(addr, val);
        }
        self.pallete_ram[usize::from(addr & 0x20)] = val;
    }
}
