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

impl std::fmt::Debug for dyn Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl std::fmt::Display for dyn Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Get a copy of the RAM image after accounting for banks/mirroring/etc.
        let mut r = [0; MAX_SIZE];
        self.ram(&mut r);

        let mut repeat = false;
        for row in 0..MAX_SIZE / 16 {
            // After the first row compare this row with the previous one.
            // If they are identical print a single * once and skip processing
            // until they aren't identical anymore.
            let cur = row * 16;
            if row != 0 {
                let old = (row - 1) * 16;
                if r[cur..cur + 16] == r[old..old + 16] {
                    if !repeat {
                        writeln!(f, "*")?;
                        repeat = true;
                    }
                    continue;
                }
                repeat = false;
            }
            // Match hexdump output and use lowercase hex
            write!(f, "{cur:08x}  ")?;
            for col in 0..8 {
                write!(f, "{:02x} ", r[cur + col])?;
            }
            write!(f, " ")?;
            for col in 8..16 {
                write!(f, "{:02x} ", r[cur + col])?;
            }
            write!(f, " |")?;
            for col in 0..16 {
                let c = char::from(r[cur + col]);
                if c == ' ' {
                    write!(f, " ")?;
                } else if c.is_ascii_graphic() {
                    write!(f, "{c}")?;
                } else {
                    write!(f, ".")?;
                }
            }
            writeln!(f, "|")?;
        }
        Ok(())
    }
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
