//! `memory` defines the traits and implementations the memory model
//! a 6502 CPU uses.

#[cfg(test)]
mod tests;

/// Representation of 6502 memory. Doesn't include bank support (yet).
pub trait Memory {
    /// `read` will return a value from the given address.
    fn read(&self, addr: u16) -> u8;

    /// `write` will write the value to the given address.
    fn write(&mut self, addr: u16, val: u8);

    /// `power_on` will perform power on behavior such as setting specific
    /// memory locations (or randomizing).
    fn power_on(&mut self);

    /// Get a copy of the whole 64k RAM block as the CPU would see it.
    /// i.e. if there is shadowing this will show that with copies in the
    /// relevant places.
    fn ram(&self, dest: &mut [u8; MAX_SIZE]);
}

/// The maxmimum memory size one can address.
pub const MAX_SIZE: usize = 1 << 16;

/// `Memory` implementation for a basic flat 64k which
/// returns a ref to the underlying array when calling `ram`.
impl Memory for [u8; MAX_SIZE] {
    fn read(&self, addr: u16) -> u8 {
        self[usize::from(addr)]
    }

    fn write(&mut self, addr: u16, val: u8) {
        self[usize::from(addr)] = val;
    }

    fn power_on(&mut self) {}

    fn ram(&self, dest: &mut [u8; MAX_SIZE]) {
        *dest = *self;
    }
}
