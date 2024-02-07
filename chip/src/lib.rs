//! `chip` defines the basic interfaces for chips in the 6502 family.
//! Each chip must implement this in order to be used together in a system
//! emulation.

use color_eyre::eyre::Result;
use strum_macros::{Display, EnumString};

/// Chip defines the trait for an 8 bit chip in the 6502 chip era.
pub trait Chip {
    /// `tick` is called to run a clock tick.
    ///
    /// # Errors
    /// If calling `tick` results in a lockup/illegal condition an error
    /// will be returned.
    fn tick(&mut self) -> Result<()>;

    /// `tick_done` is called on each `Chip` when all `tick` calls have
    /// completed. As in real hardware there are no guarentees on ordering
    /// of `tick` calls so emulation of combinational logic including latches
    /// should only get updated here to prevent chips on the same
    /// clock cycle from possibly seeing differing views of data.
    /// i.e. the Atari 2600 is full of latches which this can come into play.
    ///
    /// For instance if some write would trigger pulling RDY then it should be
    /// noted in `tick()` but not actually set until `tick_done` is called. This
    /// allows for accurate cycle emulation.
    ///
    /// # Errors
    /// If calling results in a lockup/illegal condition an error
    /// will be returned.
    fn tick_done(&mut self) -> Result<()>;
}

/// `CPUType` defines the various implementations of the 6502 available.
#[derive(Copy, Clone, Debug, Display, EnumString)]
#[allow(clippy::upper_case_acronyms)]
pub enum CPUType {
    /// Basic NMOS 6502 including all undocumented opcodes.
    NMOS,

    /// Ricoh version used in the NES which is identical to NMOS except BCD mode is unimplemented.
    #[strum(to_string = "NMOS_RICOH")]
    RICOH,

    /// NMOS 6501 variant (used in c64) which includes I/O ports mapped at addresses 0x00 and 0x01.
    #[strum(to_string = "NMOS_6510")]
    NMOS6510,

    /// 65C02 CMOS version where undocumented opcodes are all explicit NOP's and defined.
    /// This is an implementation of the later WDC spec so will include support
    /// for WAI, STP, SMB/RMB and BBR/BBS instructions.
    CMOS,

    /// 65C02 CMOS Rockwell version which is identical to WDC version above except for missing
    /// WAI and STP instructions.
    #[strum(to_string = "CMOS_ROCKWELL")]
    CMOSRockwell,

    /// 65C02 CMOS Synertek version which is identical to WDC version above except for missing
    /// all WDC extensions - WAI, STP, SMB/RMB and BBR/BBS instructions.
    #[strum(to_string = "CMOS_65SC02")]
    CMOS65SC02,
}
