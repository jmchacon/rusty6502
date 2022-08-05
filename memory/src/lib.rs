//! `memory` defines the traits and implementations the memory model
//! a 6502 CPU uses.

use cpu::{IRQ_VECTOR, NMI_VECTOR, RESET_VECTOR};

/// Representation of 6502 memory. Doesn't include bank support (yet).
pub trait Memory {
    /// `read` will return a value from the given address.
    fn read(&self, addr: u16) -> u8;

    /// `write` will write the value to the given address.
    fn write(&mut self, addr: u16, val: u8);

    /// `power_on` will perform power on behavior such as setting specific
    /// memory locations (or randomizing).
    fn power_on(&mut self);
}

const MAX_SIZE: usize = 1 << 16;

/// `FlatRAM` gives a flat 64k RAM block to use.
/// It will be initialized to all zeros and `power_on`
/// can use a different value if `fill_value` is set.
/// Additionally the irq/reset and nmi vectors can be set as well.
/// Generally used only for testing.
#[derive(Debug, Clone, Copy)]
#[must_use]
pub struct FlatRAM {
    fill_value: u8,
    vectors: Vectors,
    memory: [u8; MAX_SIZE],
}

#[derive(Debug, Default, Clone, Copy)]
/// `Vectors` defines the 3 6502 vectors for interrupts and reset behavior.
pub struct Vectors {
    /// `nmi` is the NMI vector.
    pub nmi: u16,

    /// `reset` is the reset vector.
    pub reset: u16,

    /// `irq` is the IRQ vector.
    pub irq: u16,
}

impl Memory for FlatRAM {
    fn read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn write(&mut self, addr: u16, val: u8) {
        self.memory[addr as usize] = val;
    }

    /// `power_on` will perform power on behavior. For `FlatRAM` this entails
    /// setting all memory locations to the fill value and then setting the 3 vectors
    /// to their assigned values.
    fn power_on(&mut self) {
        for i in 0..self.memory.len() {
            self.memory[i] = self.fill_value;
        }
        self.memory[NMI_VECTOR as usize] = (self.vectors.nmi & 0xFF) as u8;
        self.memory[(NMI_VECTOR + 1) as usize] = ((self.vectors.nmi & 0xff00) >> 8) as u8;
        self.memory[RESET_VECTOR as usize] = (self.vectors.reset & 0xFF) as u8;
        self.memory[(RESET_VECTOR + 1) as usize] = ((self.vectors.reset & 0xff00) >> 8) as u8;
        self.memory[IRQ_VECTOR as usize] = (self.vectors.irq & 0xFF) as u8;
        self.memory[(IRQ_VECTOR + 1) as usize] = ((self.vectors.irq & 0xff00) >> 8) as u8;
    }
}

impl Default for FlatRAM {
    fn default() -> Self {
        Self::new()
    }
}

impl FlatRAM {
    /// new will return a `FlatRAM` with 0x00 set for everything (vectors, fill value, etc).
    /// Use other builders to set additional items.
    pub fn new() -> Self {
        Self {
            fill_value: 0,
            vectors: Vectors {
                ..Default::default()
            },
            memory: [0; MAX_SIZE],
        }
    }

    /// `fill_value` is a builder which sets the fill value to use when performing `power_on`.
    pub const fn fill_value(mut self, value: u8) -> Self {
        self.fill_value = value;
        self
    }

    /// `fill_value` is a builder which sets the vectors to use when performing `power_on`.
    pub const fn vectors(mut self, vectors: Vectors) -> Self {
        self.vectors = vectors;
        self
    }
}
