//! `chip` defines the basic interfaces for chips in the 6502 family.
//! Each chip must implement this in order to be used together in a system
//! emulation.

use color_eyre::eyre::Result;

/// Chip defines the trait for an 8 bit chip in the 6502 chip era.
pub trait Chip {
    /// `tick` is called to run a clock tick.
    ///
    /// # Errors
    /// If calling `tick` results in a lockup/illegal condition an error
    /// will be returned.
    fn tick(&mut self) -> Result<()>;

    /// `done` is called on each `Chip` when all `tick` calls have
    /// completed. As in real hardware there are no guarentees on ordering
    /// of `tick` calls so emulation of combinational logic including latches
    /// should only get updated in `done` to prevent chips on the same
    /// clock cycle from possibly seeing differing views of data.
    /// i.e. the Atari 2600 is full of latches which this can come into play.
    ///
    /// # Errors
    /// If calling `done` results in a lockup/illegal condition an error
    /// will be returned.
    fn tick_done(&mut self) -> Result<()>;
}
