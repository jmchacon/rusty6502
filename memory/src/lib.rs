//! `memory` defines the traits and implementations the memory model
//! a 6502 CPU uses.

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

/// The maxmimum memory size one can address.
pub const MAX_SIZE: usize = 1 << 16;
