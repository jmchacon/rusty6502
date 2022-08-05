use cpu::{IRQ_VECTOR, NMI_VECTOR, RESET_VECTOR};

/// Representation of 6502 memory. Doesn't include bank support (yet).
pub trait Memory {
    fn read(&self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, val: u8);
    fn power_on(&mut self);
}

pub const MAX_SIZE: usize = 1 << 16;

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
pub struct Vectors {
    pub nmi: u16,
    pub reset: u16,
    pub irq: u16,
}

impl Memory for FlatRAM {
    fn read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }
    fn write(&mut self, addr: u16, val: u8) {
        self.memory[addr as usize] = val;
    }
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

    pub const fn fill_value(mut self, value: u8) -> Self {
        self.fill_value = value;
        self
    }

    pub const fn vectors(mut self, vectors: Vectors) -> Self {
        self.vectors = vectors;
        self
    }
}
