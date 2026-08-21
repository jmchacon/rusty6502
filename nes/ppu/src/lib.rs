//! `ppu` implements support for the PPU chip in the original NES

use std::{cell::RefCell, rc::Rc};

use chip::Chip;
use color_eyre::Result;
use irq::Sender;
use memory::Memory;
use rand::Rng;
use rusty6502::prelude::*;

/// `PPU` is the implementation for the PPU chip in the original NES
pub struct PPU<'a> {
    mapper: Box<dyn PPUMapper>,    // The PPU address bus/memory map
    cpu: Rc<RefCell<dyn CPU<'a>>>, // Direct access to the CPU for RAM and read state (for RDY needed for OAM DMA).
    ppuctrl: u8,
    ppumask: u8,
    ppustatus: u8,
    oamaddr: u8,
    oamdata: u8,
    ppuscroll: u8,
    ppuaddr: u8,
    ppudata: u8,
    oamdma: u8,
    int_v: u8, // During rendering, used for the scroll position. Outside of rendering, used as the current VRAM address.
    int_t: u8, // During rendering, specifies the starting coarse-x scroll for the next scanline and the starting y scroll for the screen. Outside of rendering, holds the scroll or VRAM address before transferring it to v.
    int_x: u8, // The fine-x position of the current scroll, used during rendering alongside v.
    int_w: RefCell<u8>, // Toggles on each write to either PPUSCROLL or PPUADDR, indicating whether this is the first or second write. Clears on reads of PPUSTATUS. Sometimes called the 'write latch' or 'write toggle'.
    pallete_ram: [u8; 0x20],
    oam_ram: [u8; 0x100],
    oam_ram_addr: usize,
    ticks: usize,
    reset: bool,
    odd_frame: bool,
    x_cord: u8,
    y_cord: u8,
    rdy: Rc<PpuRdy>,
    rdy_val: bool,
    rdy_ticks: usize,
    image: Box<[u8]>,
}

const PPUCTRL: u16 = 0x0000;
const PPUMASK: u16 = 0x0001;
const PPUSTATUS: u16 = 0x0002;
const OAMADDR: u16 = 0x0003;
const OAMDATA: u16 = 0x0004;
const PPUSCROLL: u16 = 0x0005;
const PPUADDR: u16 = 0x0006;
const PPUDATA: u16 = 0x0007;

const PPU_VBL_MASK: u8 = 0x80;
const PPU_SPRITE0_HIT_MASK: u8 = 0x40;
const PPU_SPRITE0_OVERFLOW_MASK: u8 = 0x20;

const PPU_NTSC_X_SIZE: usize = 341;
const PPU_NTSC_Y_SIZE: usize = 262;
const PPU_NTSC_IMAGE_RGB: usize = PPU_NTSC_X_SIZE * PPU_NTSC_Y_SIZE * 3;

impl Chip for PPU<'_> {
    fn tick(&mut self) -> Result<()> {
        self.ticks += 1;
        todo!()
    }

    fn tick_done(&mut self) -> Result<()> {
        // Set here so it always takes effect on the next CPU tick, not this one.
        self.rdy.set_rdy(self.rdy_val);
        Ok(())
    }
}

/// NES cartridges have a variety of mappers depending on how much ROM/RAM
/// is on the cart and the method used to trigger bank switching.
/// As the RAM range from 0x2000-0x2FFF is generally 2K+mirrored but can
/// change per mapper the mapper is responsible for handling this range entirely.
/// NOTE: This is only 14 bits so the upper 2 bits are ignored.
///
/// Map:
///
/// Address range Size  Description             Mapped by
/// $0000-$0FFF   $1000 Pattern table 0         Cartridge
/// $1000-$1FFF   $1000 Pattern table 1         Cartridge
/// $2000-$23BF   $03C0 Nametable 0             Cartridge
/// $23C0-$23FF   $0040 Attribute table 0       Cartridge
/// $2400-$27BF   $03C0 Nametable 1             Cartridge
/// $27C0-$27FF   $0040 Attribute table 1       Cartridge
/// $2800-$2BBF   $03C0 Nametable 2             Cartridge
/// $2BC0-$2BFF   $0040 Attribute table 2       Cartridge
/// $2C00-$2FBF   $03C0 Nametable 3             Cartridge
/// $2FC0-$2FFF   $0040 Attribute table 3       Cartridge
/// $3000-$3EFF   $0F00 Unused                  Cartridge
/// $3F00-$3F1F   $0020 Palette RAM indexes     Internal to PPU
/// $3F20-$3FFF   $00E0 Mirrors of $3F00-$3F1F  Internal to PPU
pub trait PPUMapper {
    /// Read a given memory location. This may be mirrored.
    fn read(&self, addr: u16) -> u8;

    /// Write a given memory location.
    fn write(&mut self, addr: u16, val: u8);

    /// Provide a snapshot of the memory the PPU sees
    fn ram(&self, dest: &mut [u8; memory::MAX_SIZE]);
}

/// The CPU side interface to the PPU. Technically only 3 bits are exposed here
/// so many CPU side addresses mirror.
impl Memory for PPU<'_> {
    fn read(&self, addr: u16) -> u8 {
        match addr & 0x0007 {
            PPUCTRL | PPUMASK | OAMADDR | PPUSCROLL | PPUDATA => 0x00,
            PPUSTATUS => {
                *self.int_w.borrow_mut() = 0x00;
                self.ppustatus
            }
            PPUADDR => self.ppuaddr,
            OAMDATA => self.oamdata,
            _ => unreachable!(),
        }
    }
    fn write(&mut self, addr: u16, val: u8) {
        if self.reset {
            return;
        }
        match addr & 0x0007 {
            PPUCTRL => self.ppuctrl = val,
            PPUMASK => self.ppumask = val,
            OAMADDR => self.oamaddr = val,
            PPUSCROLL => self.ppuscroll = val,
            PPUDATA => self.ppudata = val,
            PPUSTATUS | PPUADDR | OAMDATA => {}
            _ => unreachable!(),
        }
    }

    fn power_on(&mut self) {
        todo!()
    }

    fn ram(&self, dest: &mut [u8; memory::MAX_SIZE]) {
        self.mapper.ram(dest);
    }
}

impl<'a> PPU<'a> {
    /// Create a new PPU
    #[must_use]
    pub fn new(mapper: Box<dyn PPUMapper>, cpu: Rc<RefCell<dyn CPU<'a>>>, rdy: Rc<PpuRdy>) -> Self {
        Self {
            mapper,
            cpu,
            ppuctrl: 0x00,
            ppumask: 0x00,
            ppustatus: 0x00,
            oamaddr: 0x00,
            oamdata: 0x00,
            ppuscroll: 0x00,
            ppuaddr: 0x00,
            ppudata: 0x00,
            oamdma: 0x00,
            int_v: 0x00,
            int_t: 0x00,
            int_x: 0x00,
            int_w: RefCell::new(0x00),
            pallete_ram: [0; 0x20],
            oam_ram: [0; 0x100],
            oam_ram_addr: 0x000,
            ticks: 0,
            reset: true,
            odd_frame: false,
            x_cord: 0x00,
            y_cord: 0x00,
            rdy,
            rdy_val: false,
            rdy_ticks: 0,
            image: vec![0; PPU_NTSC_IMAGE_RGB].into_boxed_slice(),
        }
    }

    /// Do a power-on sequence for the PPU. As this is a superset of `reset`
    /// that is not needed to be called as well (it is a no-op if it is).
    pub fn power_on(&mut self) {
        self.ppuctrl = 0x00;
        self.ppumask = 0x00;
        self.ppustatus = 0x00;

        let mut rng = rand::rng();

        // Randomize VBL on power up (on reset it's simply left alone)
        if rng.random::<f64>() > 0.5 {
            self.ppustatus |= PPU_VBL_MASK;
        }

        self.oamaddr = 0x00;
        self.ppuscroll = 0x00;
        self.ppuaddr = 0x00;
        self.ppudata = 0x00;

        self.reset = true;
        self.odd_frame = false;
        self.x_cord = 0x00;
        self.y_cord = 0x00;
    }

    /// Do a reset sequence for the PPU.
    pub fn reset(&mut self) {
        self.ppuctrl = 0x00;
        self.ppumask = 0x00;

        // VBL isn't changed on reset
        self.ppustatus &= PPU_VBL_MASK;

        // oamaddr is left alone.
        self.ppuscroll = 0x00;
        // ppuaddr is left alone.
        self.ppudata = 0x00;

        self.reset = true;
        self.odd_frame = false;
        self.x_cord = 0x00;
        self.y_cord = 0x00;
    }

    /// For use in OAM DMA transfers. This triggers the start of OAM DMA.
    /// The value written here is the page in the CPU RAM impl to read over and
    /// copy to `oam_ram`.
    pub fn oam_write(&mut self, val: u8) {
        self.oamdma = val;
        self.rdy_val = true;
        self.rdy_ticks = 0;
    }
}

/// `PpuRdy` encapsultes the RDY state between a PPU and a 6502 CPU
#[derive(Default)]
pub struct PpuRdy {
    rdy: RefCell<bool>,
}

impl PpuRdy {
    /// Call `set_rdy` to change the RDY state the CPU sees.
    pub fn set_rdy(&self, rdy: bool) {
        *self.rdy.borrow_mut() = rdy;
    }
}

impl Sender for PpuRdy {
    fn raised(&self) -> bool {
        *self.rdy.borrow()
    }
}
