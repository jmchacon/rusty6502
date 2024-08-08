//! `nes_memory` defines an implementation of `Memory` which can be used by a 6502
//! CPU impl (specifically the Ricoh version) for the NES system.
//!
//! This should be the RAM implementation passed into the CPU when constructing
//! a system as it will handle all the direct memory/mirroring/APU/PPU and
//! mapper functionality.

use std::cell::RefCell;

use apu::APU;
use ines::NES;
use memory::Memory;
use ppu::PPU;

/// NES cartridges have a variety of mappers depending on how much ROM/RAM
/// is on the cart and the method used to trigger bank switching.
/// In general mappers can passively observe the bus for every address except
/// 0x4015 which generally means every `Memory` based access on a NES does
/// a mirrored one to this trait as well to process any possible side effects.
pub trait Mapper {
    /// Read a given memory location. This may be mirrored or even open bus.
    /// In the latter case this will return the last value seen on the bus.
    fn read(&self, addr: u16) -> u8;

    /// Write a given memory location. If this is open bus it goes nowhere.
    fn write(&mut self, addr: u16, val: u8);
}

/// The implementation for all memory on an NES.
pub struct NESMemory {
    nes: NES,
    cart_ram: bool,
    apu: APU,
    ppu: PPU,
    mapper: Box<dyn Mapper>,
    ram: [u8; 0x0800],
    last_read: RefCell<u8>,
    oam_dma: bool,
}

impl NESMemory {
    /// Construct a new `NESMemory`
    #[must_use]
    pub fn new(nes: NES, cart_ram: bool, apu: APU, ppu: PPU, mapper: Box<dyn Mapper>) -> Self {
        Self {
            nes,
            cart_ram,
            apu,
            ppu,
            mapper,
            ram: [0; 0x0800],
            last_read: RefCell::new(0x00),
            oam_dma: false,
        }
    }

    /// This should be called after every CPU tick(). If it returns true the CPU
    /// has attempted a write to 0x4014 which triggers OAM DMA to attempt to
    /// begin. The top level chip controller can then initate OAM DMA.
    /// This may still be delayed by 0-3 clock cycles depending when the
    /// CPU is done writing and moves to a read state.
    pub fn oam_dma(&self) -> bool {
        self.oam_dma
    }
}

// The CPU memory map:
//
// Address range	Size	Device
// $0000–$07FF	  $0800	2 KB internal RAM
// $0800–$0FFF	  $0800	Mirrors of $0000–$07FF
// $1000–$17FF	  $0800
// $1800–$1FFF	  $0800
// $2000–$2007	  $0008	NES PPU registers
// $2008–$3FFF	  $1FF8	Mirrors of $2000–$2007 (repeats every 8 bytes)
// $4000–$4017	  $0018	NES APU and I/O registers
// $4018–$401F	  $0008	APU and I/O functionality that is normally disabled. See CPU Test Mode.
// $4020–$FFFF	  $BFE0	Cartridge space: PRG ROM, PRG RAM, and mapper registers
//
// NOTE: 0x4014 is special as this is the OAM DMA trigger.
// TODO(jchacon): Figure out how to handle this. Trap here and have PPU just provide
//                an interface to writing the OAM bits?

impl Memory for NESMemory {
    fn read(&self, addr: u16) -> u8 {
        let mut ret = 0x00;
        if addr < 0x2000 {
            ret = self.ram[usize::from(addr & 0x07FF)];
        }
        if (0x2000..0x3FFF).contains(&addr) {
            ret = self.ppu.read(addr & 0x7);
        }
        if (0x4000..0x4018).contains(&addr) {
            ret = self.apu.read(addr & 0x1F);
        }
        if (0x4018..0x4020).contains(&addr) {
            // TODO - hardcode for now
            ret = 0x00;
        }
        // The mapper sees everything except 0x4015
        if addr == 0x4015 {
            return ret;
        }
        if addr >= 0x4020 {
            ret = self.mapper.read(addr);
        } else {
            self.mapper.read(addr);
        }
        ret
    }

    fn write(&mut self, addr: u16, val: u8) {
        self.mapper.write(addr, val);
    }

    fn power_on(&mut self) {
        // Nothing to do.
    }

    fn ram(&self, dest: &mut [u8; memory::MAX_SIZE]) {
        todo!()
    }
}
