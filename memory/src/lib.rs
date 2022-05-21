/// Representation of 6502 memory. Doesn't include bank support (yet).
pub trait Memory {
    fn read(&self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, val: u8);
    fn power_on(&mut self);
}

/// FlatRAM gives a flat 64k RAM block to use.
/// It will be initialized to all zeros for power_on.
/// Generally used only for testing.
#[derive(Debug, Clone, Copy)]
pub struct FlatRAM {
    memory: [u8; 65536],
}

impl Memory for FlatRAM {
    fn read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }
    fn write(&mut self, addr: u16, val: u8) {
        self.memory[addr as usize] = val
    }
    fn power_on(&mut self) {
        // Nothing to do
    }
}

impl Default for FlatRAM {
    fn default() -> FlatRAM {
        FlatRAM::new()
    }
}

impl FlatRAM {
    pub fn new() -> Self {
        FlatRAM { memory: [0; 65536] }
    }
}
