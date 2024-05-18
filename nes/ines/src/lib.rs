//! `ines` provides utilities for decoding an INES file into the relative
//! components (PRG, CHR, etc) as well as decoding all of the flags.
//!
//! It supports Archaic, INES and NES 2.0 formats as described in
//! <https://www.nesdev.org/wiki/INES>

use color_eyre::eyre::{eyre, Result};
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    sync::OnceLock,
};
use strum::IntoEnumIterator;
use strum_macros::{Display, EnumCount, EnumIter};

#[cfg(test)]
mod tests;

/// The full description and data from a .ines file
#[derive(PartialEq)]
pub struct NES {
    /// PRG-ROM segments
    pub prg: Vec<[u8; PRG_BLOCK_SIZE_U]>,

    /// CHR-ROM segments
    pub chr: Vec<[u8; CHR_BLOCK_SIZE_U]>,

    /// Nametable mirroring
    pub nametable_mirror: NametableMirroring,

    /// Battery backup precense
    pub battery: bool,

    /// Optional trainer data
    pub trainer: Option<[u8; 512]>,

    /// 4 screen mode
    pub four_screen_mode: bool,

    /// Console type
    pub console_type: ConsoleType,

    /// Mapper number
    pub mapper: u16,

    /// Optional sub mapper (used with NES 2.0 only)
    pub sub_mapper: Option<u8>,

    /// PRG RAM size
    pub prg_ram: u64,

    /// PRG NVRAM size
    pub prg_nvram: u64,

    /// CHR RAM size
    pub chr_ram: u64,

    /// CHR NVRAM size
    pub chr_nvram: u64,

    /// CPU Timing
    pub cpu_timing: CPUTiming,

    /// VS system PPU
    pub vs_ppu: Option<VsPPU>,

    /// VS system hardware
    pub vs_hw: Option<VsHardware>,

    /// Any trailing ROM data
    pub misc_rom: Option<Vec<u8>>,

    /// The number of misc ROM's contained in the `misc_rom` field
    /// The specific cart/mapper code will need to know what to do with this.
    pub num_misc_rom: u8,

    /// Optional required expansion devices
    pub expansion_device: Option<ExpansionDevice>,

    /// The style of input data used to parse this cart.
    pub cart_style: CartStyle,
}

impl Default for NES {
    fn default() -> Self {
        // Done by hand since the most basic is 1 PRG, 1 CHR (0 filled is still legal)
        // and an NTSC NES with no mapper. Technically we can have 0 CHR and
        // it does CHR RAM instead. That's handled in parse().
        Self {
            prg: vec![[0; PRG_BLOCK_SIZE_U]],
            chr: vec![[0; CHR_BLOCK_SIZE_U]],
            nametable_mirror: NametableMirroring::default(),
            battery: false,
            trainer: None,
            four_screen_mode: false,
            console_type: ConsoleType::default(),
            mapper: 0,
            sub_mapper: None,
            prg_ram: 0,
            prg_nvram: 0,
            chr_ram: 0,
            chr_nvram: 0,
            cpu_timing: CPUTiming::default(),
            vs_ppu: None,
            vs_hw: None,
            misc_rom: None,
            num_misc_rom: 0,
            expansion_device: None,
            cart_style: CartStyle::default(),
        }
    }
}

impl Debug for NES {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NES")
            // Don't want these arrays printing so just print their length instead.
            .field("prg segments", &self.prg.len())
            .field("chr segments", &self.chr.len())
            .field("nametable_mirror", &self.nametable_mirror)
            .field("battery", &self.battery)
            .field("trainer", &self.trainer.is_some_and(|x| !x.is_empty()))
            .field("four_screen_mode", &self.four_screen_mode)
            .field("console_type", &self.console_type)
            .field("mapper", &self.mapper)
            .field("sub_mapper", &self.sub_mapper)
            .field("prg_ram", &self.prg_ram)
            .field("prg_nvram", &self.prg_nvram)
            .field("chr_ram", &self.chr_ram)
            .field("chr_nvram", &self.chr_nvram)
            .field("cpu_timing", &self.cpu_timing)
            .field("vs_ppu", &self.vs_ppu)
            .field("vs_hw", &self.vs_hw)
            .field(
                "misc_rom size",
                &self.misc_rom.as_ref().map_or(0, std::vec::Vec::len),
            )
            .field("num_misc_rom", &self.num_misc_rom)
            .field("expansion_device", &self.expansion_device)
            .field("cart_style", &self.cart_style)
            .finish()
    }
}

impl Display for NES {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "NES: {}", self.cart_style)?;
        writeln!(f, "prg segments: {}", self.prg.len())?;
        writeln!(f, "chr segments: {}", self.chr.len())?;
        writeln!(f, "nametable_mirror: {}", &self.nametable_mirror)?;
        writeln!(f, "battery: {}", self.battery)?;
        writeln!(
            f,
            "trainer: {}",
            self.trainer.is_some_and(|x| !x.is_empty())
        )?;
        writeln!(f, "four_screen_mode: {}", self.four_screen_mode)?;
        writeln!(f, "console_type: {}", self.console_type)?;
        writeln!(f, "mapper: {}", self.mapper)?;
        writeln!(f, "sub_mapper: {:?}", self.sub_mapper)?;
        writeln!(f, "prg_ram: {}", self.prg_ram)?;
        writeln!(f, "prg_nvram: {}", self.prg_nvram)?;
        writeln!(f, "chr_ram: {}", self.chr_ram)?;
        writeln!(f, "chr_nvram: {}", self.chr_nvram)?;
        writeln!(f, "cpu_timing: {}", self.cpu_timing)?;
        writeln!(f, "vs_ppu: {:?}", self.vs_ppu)?;
        writeln!(f, "vs_hw: {:?}", self.vs_hw)?;
        writeln!(
            f,
            "misc_rom len: {}",
            self.misc_rom.as_ref().map_or(0, std::vec::Vec::len)
        )?;
        writeln!(f, "num_misc_rom: {}", self.num_misc_rom)?;
        writeln!(f, "expansion_device: {:?}", self.expansion_device)
    }
}

/// Nametable mirroring
#[derive(Default, Debug, Display, PartialEq)]
pub enum NametableMirroring {
    /// Horizonal
    #[default]
    Horizontal,

    /// Vertical
    Vertical,
}

/// Console type
#[derive(Copy, Clone, Default, Debug, Display, EnumCount, EnumIter, PartialEq)]
pub enum ConsoleType {
    /// NES
    #[default]
    NintendoEntertainmentSystem,

    /// Vs System
    NintendoVsSystem,

    /// Playchoice 10
    NintendoPlaychoice10,

    /// Famicom with BCD enabled CPU
    FamicomWithBCD,

    /// Famicom with EPSM chip
    FamicomNESWithEPSM,

    /// VR Technology VT01
    VRTechnologyVT01,

    /// VR Technology VT02
    VRTechnologyVT02,

    /// VR Technology VT03
    VRTechnologyVT03,

    /// VR Technology VT09
    VRTechnologyVT09,

    /// VR Technology VT32
    VRTechnologyVT32,

    /// VR Technology VT369
    VRTechnologyVT369,

    /// UMC UM6578
    UMCUM6578,

    /// Famicom Network System
    FamicomNetworkSystem,
}

// TODO(jchacon): Replace all OnceLock with LazyLock (and get rid of the fn)
//                once LazyLock stablizes.
fn console_type() -> &'static HashMap<u8, ConsoleType> {
    static CONSOLE_TYPE: OnceLock<HashMap<u8, ConsoleType>> = OnceLock::new();
    CONSOLE_TYPE.get_or_init(|| {
        let mut m = HashMap::new();
        for (idx, e) in ConsoleType::iter().enumerate() {
            #[allow(clippy::cast_possible_truncation)]
            m.insert(idx as u8, e);
        }
        m
    })
}

/// CPU Timing for cart
#[derive(Copy, Clone, Default, Debug, Display, EnumCount, EnumIter, PartialEq)]
pub enum CPUTiming {
    /// NTSC
    #[default]
    NTSC,

    /// PAL
    PAL,

    /// Multiregion support
    MultiRegion,

    /// Dendy
    Dendy,
}

// TODO(jchacon): Replace all OnceLock with LazyLock (and get rid of the fn)
//                once LazyLock stablizes.
fn cpu_timing() -> &'static HashMap<u8, CPUTiming> {
    static CONSOLE_TYPE: OnceLock<HashMap<u8, CPUTiming>> = OnceLock::new();
    CONSOLE_TYPE.get_or_init(|| {
        let mut m = HashMap::new();
        for (idx, e) in CPUTiming::iter().enumerate() {
            #[allow(clippy::cast_possible_truncation)]
            m.insert(idx as u8, e);
        }
        m
    })
}

/// Vs system PPU
#[derive(Copy, Clone, Debug, Display, EnumCount, EnumIter, PartialEq)]
pub enum VsPPU {
    /// RP2C03B
    RP2C03B,

    /// RP2C03G
    RP2C03G,

    /// RP2C04-0001
    RP2C04_0001,

    /// RP2C04-0002
    RP2C04_0002,

    /// RP2C04-0003
    RP2C04_0003,

    /// RP2C04-0004
    RP2C04_0004,

    /// RP2C03B
    RC2C03B,

    /// RP2C03C
    RC2C03C,

    /// RC2C05-01
    RC2C05_01,

    /// RC2C05-02
    RC2C05_02,

    /// RC2C05-03
    RC2C05_03,

    /// RC2C05-04
    RC2C05_04,

    /// RC2C05-05
    RC2C05_05,
}

// TODO(jchacon): Replace all OnceLock with LazyLock (and get rid of the fn)
//                once LazyLock stablizes.
fn vs_ppu() -> &'static HashMap<u8, VsPPU> {
    static CONSOLE_TYPE: OnceLock<HashMap<u8, VsPPU>> = OnceLock::new();
    CONSOLE_TYPE.get_or_init(|| {
        let mut m = HashMap::new();
        for (idx, e) in VsPPU::iter().enumerate() {
            #[allow(clippy::cast_possible_truncation)]
            m.insert(idx as u8, e);
        }
        m
    })
}

/// Vs Hardware
#[derive(Copy, Clone, Debug, Display, EnumCount, EnumIter, PartialEq)]
pub enum VsHardware {
    /// Unisystem
    Unisystem,

    /// Unisystem RBI Baseball
    UnisystemRBIBaseball,

    /// Unisystem TKO Boxing
    UnisystemTKOBoxing,

    /// Unisystem Super Xevious
    UnisystemSuperXevious,

    /// Unisystem Ice Climber Japan
    UnisystemIceClimberJapan,

    /// Dual System
    DualSystem,

    /// Dual System Raid on Bungeling Bay
    DualSystemRaidOnBungelingBay,
}

// TODO(jchacon): Replace all OnceLock with LazyLock (and get rid of the fn)
//                once LazyLock stablizes.
fn vs_hw() -> &'static HashMap<u8, VsHardware> {
    static CONSOLE_TYPE: OnceLock<HashMap<u8, VsHardware>> = OnceLock::new();
    CONSOLE_TYPE.get_or_init(|| {
        let mut m = HashMap::new();
        for (idx, e) in VsHardware::iter().enumerate() {
            #[allow(clippy::cast_possible_truncation)]
            m.insert(idx as u8, e);
        }
        m
    })
}

/// Game expected devices for cart
#[derive(Copy, Clone, Debug, Display, EnumCount, EnumIter, PartialEq)]
pub enum ExpansionDevice {
    /// Technically not valid - but don't error if 0
    Unspecified,

    /// Standard NES controller
    StandardNES,

    /// NES Four Score
    NESFourScore,

    /// Famicom Four Players Adapter
    FamicomFourPlayersAdapter,

    /// Vs System (1P at $4016)
    VsSystem4016,

    /// Vs System (1P at $4017)
    VsSystem4017,

    /// Reserved entry (invalid)
    Reserved,

    /// Vs System Zapper
    VsZapper,

    /// Zapper
    Zapper,

    /// Two Zappers
    TwoZappers,

    /// Bandai Hyper Shot Lightgun
    BandaiHyperShotLightgun,

    /// Power Pad Side A
    PowerPadSideA,

    /// Power Pad Side B
    PowerPadSideB,

    /// Family Trainer Side A
    FamilyTrainerSideA,

    /// Family Trainer Side B
    FamilyTrainerSideB,

    /// Arkanoid Vaus Controller NES
    ArkanoidVausControllerNES,

    /// Arkanoid Vaus Controller Famicom
    ArkanoidVausControllerFamicom,

    /// Two Vaus Controllers w. Famicom Data Recorder
    TwoVausControllersFamicomDataRecorder,

    /// Konami Hyper Shot Controller
    KonamiHyperShotController,

    /// Coconuts Pachinko Controller
    CoconutsPachinkoController,

    /// Exciting Boxing Punching Bag
    ExcitingBoxingPunchingBag,

    /// Jissen Mahjong Controller
    JissenMahjongController,

    /// Party Tap
    PartyTap,

    /// Oeka Kids Tablet
    OekaKidsTablet,

    /// Sunsort Barecode Battler
    SunsoftBarcodeBattler,

    /// Miracle Piano Keyboard
    MiraclePianoKeyboard,

    /// Pokkun Moguraa
    PokkunMoguraa,

    /// Top Rider
    TopRider,

    /// Double Fisted
    DoubleFisted,

    /// Famicom 3D System
    Famicom3DSystem,

    /// Doremikko Keyboard
    DoremikkoKeyboard,

    /// R.O.B Gyro Set
    ROBGyroSet,

    /// Famicom Data Recorder
    FamicomDataRecorder,

    /// ASCII Turbo File
    ASCIITurboFile,

    /// IGS Storage Battle Box
    IGSStorageBattleBox,

    /// Family BASIC Keyboard
    FamilyBASICKeyboard,

    /// Dongda PEC586 Keyboard
    DongdaPEC586Keyboard,

    /// `BitCorp` Bit79 Keyboard
    BitCorpBit79Keyboard,

    /// Subor Keyboard
    SuborKeyboard,

    /// Subor Keyboard plus Mouse
    SuborKeyboardPlusMouse,

    /// Subor Keyboard plus Mouse (24 bit at $4016)
    SuborKeyboardPlusMouse24bit4016,

    /// SNES Mouse
    SNESMouse,

    /// Multicart
    Multicart,

    /// Two SNES Controllers
    TwoSNESControllers,

    /// Racer Mate Bicycle
    RacerMateBicycle,

    /// `UForce`
    UForce,

    /// R.O.B. Stack Up
    ROBStackUp,

    /// City Patrolman Lightgun
    CityPatrolmanLightgun,

    /// Sharp C1 Cassette Interface
    SharpC1CassetteInterface,

    /// Standard Controllers Swapped
    StandardControllersSwapped,

    /// Excalibor Sudoko Pad
    ExcaliborSudokuPad,

    /// ABL Pinball
    ABLPinball,

    /// Golden Nugget Casino
    GoldenNuggetCasino,

    /// Golden Key Keyboard
    GoldenKeyKeyboard,

    /// Subor Keyboard plus Mouse (24 bit at $4017)
    SuborKeyboardPlusMouse24bit4017,

    /// Port Test Controller
    PortTestController,

    /// Bandai Multi Game Player Gamepad
    BandaiMultiGamePlayerGamepad,

    /// Venom TV Dance Mat
    VenomTVDanceMat,

    /// LG TV Remote Control
    LGTVRemoteControl,
}

// TODO(jchacon): Replace all OnceLock with LazyLock (and get rid of the fn)
//                once LazyLock stablizes.
fn expansion_device() -> &'static HashMap<u8, ExpansionDevice> {
    static CONSOLE_TYPE: OnceLock<HashMap<u8, ExpansionDevice>> = OnceLock::new();
    CONSOLE_TYPE.get_or_init(|| {
        let mut m = HashMap::new();
        for (idx, e) in ExpansionDevice::iter().enumerate() {
            #[allow(clippy::cast_possible_truncation)]
            m.insert(idx as u8, e);
        }
        m
    })
}

/// The NES file format used for this cart
#[derive(Debug, Default, Display, PartialEq)]
pub enum CartStyle {
    #[default]
    /// Before there was a standard.
    ArchaicNES,
    /// Later version which added PRG RAM support
    INES1,
    /// Latest version used since 2010's
    NES20,
}

const MIRROR_MASK: u8 = 0x01;
const BATTERY_MASK: u8 = 0x02;
const TRAINER_MASK: u8 = 0x04;
const FOUR_SCREEN_MASK: u8 = 0x08;
const MAPPER_D0_D3_MASK: u8 = 0xF0;
const MAPPER_D4_D7_MASK: u8 = 0xF0;
const MAPPER_D8_D11_MASK: u8 = 0x0F;
const MAPPER_SUBMAPPER_MASK: u8 = 0xF0;
const PRG_ROM_MSB_MASK: u8 = 0x0F;
const CHR_ROM_MSB_MASK: u8 = 0xF0;
const ROM_MSB_SHIFT: u16 = 8;

const ROM_EXPONENT_MODE: u64 = 0x0F;
const ROM_EXPONENT_MASK: u64 = 0xFC;
const ROM_MULTIPLIER_MASK: u64 = 0x03;
const ROM_EXPONENT_SHIFT: u64 = 2;

const CHR_ROM_MSB_SHIFT: u8 = 4;

const CART_TYPE_MASK: u8 = 0x0C;

const CONSOLE_TYPE_MASK: u8 = 0x03;

const MAPPER_D0_D3_SHIFT: usize = 4;
const MAPPER_D8_D11_SHIFT: usize = 8;
const MAPPER_SUBMAPPER_SHIFT: usize = 4;

const TRAINER_SIZE: u64 = 512;
const TRAINER_SIZE_U: usize = 512;
const HEADER_SIZE: u64 = 16;
const HEADER_SIZE_U: usize = 16;

/// The size of a PRG block in u64 format.
pub const PRG_BLOCK_SIZE: u64 = 16_384;

/// The size of a PRG block in usize format.
pub const PRG_BLOCK_SIZE_U: usize = 16_384;

/// The size of a CHR block in u64 format.
pub const CHR_BLOCK_SIZE: u64 = 8_192;

/// The size of a CHR block in usize format.
pub const CHR_BLOCK_SIZE_U: usize = 8_192;

/// The size of a PRG RAM block in u64 format.
pub const PRG_RAM_BLOCK_SIZE: u64 = 8_192;

const MIN_SIZE: u64 = HEADER_SIZE + PRG_BLOCK_SIZE;
const MIN_SIZE_U: usize = HEADER_SIZE_U + PRG_BLOCK_SIZE_U;

const SIG_BYTE_0: usize = 0;
const SIG_BYTE_1: usize = 1;
const SIG_BYTE_2: usize = 2;
const SIG_BYTE_3: usize = 3;
const PRG_BYTE: usize = 4;
const CHR_BYTE: usize = 5;
const FLAGS_6_BYTE: usize = 6;
const FLAGS_7_BYTE: usize = 7;
const MAPPER_BYTE: usize = 8;
const PRG_CHR_MSB_BYTE: usize = 9;
const PRG_RAM_BYTE: usize = 10;
const PRG_RAM_SHIFT_MASK: u8 = 0x0F;
const PRG_NVRAM_SHIFT_MASK: u8 = 0xF0;
const PRG_NVRAM_SHIFT: u8 = 4;
const CHR_RAM_SHIFT_MASK: u8 = 0x0F;
const CHR_NVRAM_SHIFT_MASK: u8 = 0xF0;
const CHR_NVRAM_SHIFT: u8 = 4;

const CHR_RAM_BYTE: usize = 11;
const TIMING_BYTE: usize = 12;
const SYSTEMS_BYTE: usize = 13;
const MISC_ROMS_BYTE: usize = 14;
const EXPANSION_DEVICE_BYTE: usize = 15;

const EXTENDED_CONSOLE: u8 = 0x03;

const SYSTEMS_MASK: u8 = 0x0F;
const VS_PPU_TYPE_MASK: u8 = 0x0F;
const VS_HARDWARE_TYPE_MASK: u8 = 0xF0;
const VS_HARDWARE_TYPE_SHIFT: u8 = 4;

const INES1_TV_MASK: u8 = 0x01;

const NES20_CART_SIG: u8 = 0x08;
const INES_CART_SIG: u8 = 0x00;

/// Parse a given set of .ines data into an NES struct. This can handle legacy
/// and NES 2.0 data.
///
/// # Errors
/// PRG or CHR ROM data not aligned, missing or extra data can result in an error.
#[allow(clippy::too_many_lines)]
pub fn parse(data: &[u8]) -> Result<Box<NES>> {
    let mut nes = Box::<NES>::default();

    // The bare minimum size is the header, and 1 PRG (CHR might be RAM)
    if data.len() < MIN_SIZE_U {
        return Err(eyre!(
            "Minimum size {MIN_SIZE} not met. Data is only {} bytes",
            data.len()
        ));
    }

    // Make sure this even registers as the header
    if data[SIG_BYTE_0] != b'N'
        || data[SIG_BYTE_1] != b'E'
        || data[SIG_BYTE_2] != b'S'
        || data[SIG_BYTE_3] != 0x1A
    {
        return Err(eyre!("Doesn't have NES<EOF> header"));
    }

    let mut prg_blocks = u64::from(data[PRG_BYTE]);
    let mut chr_blocks = u64::from(data[CHR_BYTE]);

    if data[FLAGS_6_BYTE] & MIRROR_MASK != 0x00 {
        nes.nametable_mirror = NametableMirroring::Vertical;
    }
    nes.battery = data[FLAGS_6_BYTE] & BATTERY_MASK != 0x00;

    if data[FLAGS_6_BYTE] & TRAINER_MASK != 0x00 {
        if data.len() < (MIN_SIZE_U + TRAINER_SIZE_U) {
            return Err(eyre!(
                "Can't have trainer as size {} < min size {}",
                data.len(),
                MIN_SIZE + TRAINER_SIZE
            ));
        }
        let mut trainer = [0u8; TRAINER_SIZE_U];
        // SAFETY: We know this fits from the size checks above.
        // Copy the trainer data over.
        unsafe {
            std::ptr::copy_nonoverlapping(
                &data[HEADER_SIZE_U],
                trainer.as_mut_ptr(),
                trainer.len(),
            );
        }
        nes.trainer = Some(trainer);
    }

    nes.four_screen_mode = data[FLAGS_6_BYTE] & FOUR_SCREEN_MASK != 0x00;
    nes.mapper = u16::from(data[FLAGS_6_BYTE] & MAPPER_D0_D3_MASK) >> MAPPER_D0_D3_SHIFT;

    // Before moving on figure out the ROM type as that matters whether we even
    // care about the next byte.
    nes.cart_style = match data[FLAGS_7_BYTE] & CART_TYPE_MASK {
        NES20_CART_SIG => {
            let prg_msb = u64::from(data[PRG_CHR_MSB_BYTE] & PRG_ROM_MSB_MASK);
            let chr_msb = u64::from(data[PRG_CHR_MSB_BYTE] & CHR_ROM_MSB_MASK) >> CHR_ROM_MSB_SHIFT;
            let prg_rom = (prg_msb << ROM_MSB_SHIFT) | prg_blocks;
            let chr_rom = (chr_msb << ROM_MSB_SHIFT) | chr_blocks;

            // NOTE: Spec says exponent mode can go to 64 and theoretically
            //       the max value is 2^64*7 which is nuts.
            //       No one needs multi-exabyte ROMs....

            // Blocks or exponent mode.
            let prg_size = if prg_msb == ROM_EXPONENT_MODE {
                let exp = (prg_rom & ROM_EXPONENT_MASK) >> ROM_EXPONENT_SHIFT;
                if exp > 30 {
                    return Err(eyre!("PRG size too large (over 1G) - 2^{exp}"));
                }
                // Multiplier is bottom 2 bits * 2 + 1 (so 1 3 5 or 7)
                let mul = (prg_rom & ROM_MULTIPLIER_MASK) * 2 + 1;
                2u64.pow(exp.try_into()?) * mul
            } else {
                PRG_BLOCK_SIZE * prg_rom
            };
            let chr_size = if chr_msb == ROM_EXPONENT_MODE {
                let exp = (chr_rom & ROM_EXPONENT_MASK) >> ROM_EXPONENT_SHIFT;
                if exp > 29 {
                    return Err(eyre!("CHR size too large (over 512M) - 2^{exp}"));
                }
                // Multiplier is bottom 2 bits * 2 + 1 (so 1 3 5 or 7)
                let mul = (chr_rom & ROM_MULTIPLIER_MASK) * 2 + 1;
                2u64.pow(exp.try_into()?) * mul
            } else {
                CHR_BLOCK_SIZE * chr_rom
            };
            if (data.len() as u64) < (HEADER_SIZE + prg_size + chr_size)
                || prg_size < PRG_BLOCK_SIZE
            {
                CartStyle::ArchaicNES
            } else {
                // 1G / 16K (max) will fit into a u16 so casts are fine.
                // 2^30 / 2^14 = 2^16
                prg_blocks = prg_size / PRG_BLOCK_SIZE;
                // Pad for any remainder
                if prg_size % PRG_BLOCK_SIZE != 0 {
                    prg_blocks += 1;
                }

                // 512M / 8K (max) will fit into a u16 so casts are fine.
                // 2^29 / 2^13 = 2^16
                chr_blocks = chr_size / CHR_BLOCK_SIZE;
                // Pad for any remainder
                if chr_size % CHR_BLOCK_SIZE != 0 {
                    chr_blocks += 1;
                }

                CartStyle::NES20
            }
        }
        INES_CART_SIG => {
            if data[TIMING_BYTE..=EXPANSION_DEVICE_BYTE] == [0, 0, 0, 0] {
                CartStyle::INES1
            } else {
                CartStyle::ArchaicNES
            }
        }
        _ => CartStyle::ArchaicNES,
    };

    // At this point we know the sizes and if it's more than 1 extend the
    // vectors to the new sizes. This way later for copies we can just direct
    // copy into each one.
    let mut min_req = HEADER_SIZE + prg_blocks * PRG_BLOCK_SIZE + chr_blocks * CHR_BLOCK_SIZE;
    if nes.trainer.is_some() {
        min_req += TRAINER_SIZE;
    }
    if data.len() < min_req.try_into()? {
        return Err(eyre!(
            "PRG + CHR (and optional trainer) and header data size {} doesn't match data len {}",
            min_req,
            data.len()
        ));
    }

    // After all parsing there must be at least 1 PRG block.
    if prg_blocks == 0 {
        return Err(eyre!("No PRG blocks?"));
    }
    for _ in 0..prg_blocks - 1 {
        nes.prg.push([0; PRG_BLOCK_SIZE_U]);
    }
    // Copy over the data
    let start = if nes.trainer.is_some() {
        HEADER_SIZE_U + TRAINER_SIZE_U
    } else {
        HEADER_SIZE_U
    };
    for i in 0..nes.prg.len() {
        // SAFETY: We know this fits from the size checks above.
        // Copy the ROM data over.
        unsafe {
            std::ptr::copy_nonoverlapping(
                &data[start + i * PRG_BLOCK_SIZE_U],
                nes.prg[i].as_mut_ptr(),
                PRG_BLOCK_SIZE_U,
            );
        }
    }

    if chr_blocks > 0 {
        for _ in 0..chr_blocks - 1 {
            nes.chr.push([0; CHR_BLOCK_SIZE_U]);
        }
        // Copy over the data
        let start = if nes.trainer.is_some() {
            HEADER_SIZE_U + TRAINER_SIZE_U + nes.prg.len() * PRG_BLOCK_SIZE_U
        } else {
            HEADER_SIZE_U + nes.prg.len() * PRG_BLOCK_SIZE_U
        };
        for i in 0..nes.chr.len() {
            // SAFETY: We know this fits from the size checks above.
            // Copy the ROM data over.
            unsafe {
                std::ptr::copy_nonoverlapping(
                    &data[start + i * CHR_BLOCK_SIZE_U],
                    nes.chr[i].as_mut_ptr(),
                    CHR_BLOCK_SIZE_U,
                );
            }
        }
    }

    // If there are no CHR blocks we didn't copy and should eliminate the
    // assumed one.
    if chr_blocks == 0 {
        // In the special case for old ines files chr_blocks == 0 means this is
        // RAM actually so truncate the ROM vector.
        nes.chr = Vec::new();
        if nes.cart_style != CartStyle::NES20 {
            nes.chr_ram = CHR_BLOCK_SIZE;
        }
    }

    if nes.cart_style != CartStyle::ArchaicNES {
        nes.mapper |= u16::from(data[FLAGS_7_BYTE] & MAPPER_D4_D7_MASK);
    }

    match nes.cart_style {
        CartStyle::INES1 => {
            // Very little otherwise to set. Could have a Vs system
            // and may specify PRG RAM and TV type but unlikely to be set/used.

            // We can lookup by index and INES1 carts only have 3 possible types.
            let lookup = data[FLAGS_7_BYTE] & CONSOLE_TYPE_MASK;
            if lookup < 0x03 {
                // This is fine to direct map since we know the map has this many entries.
                nes.console_type = *console_type().get(&lookup).ok_or(eyre!("impossible"))?;
            } else {
                return Err(eyre!("Invalid console type 0x03 for INES1 format"));
            }

            let blocks = u64::from(data[MAPPER_BYTE]);
            nes.prg_ram = if blocks == 0 {
                PRG_RAM_BLOCK_SIZE
            } else {
                blocks * PRG_RAM_BLOCK_SIZE
            };
            if data[PRG_CHR_MSB_BYTE] & INES1_TV_MASK != 0x00 {
                nes.cpu_timing = CPUTiming::PAL;
            }
        }
        CartStyle::NES20 => {
            let lookup = data[FLAGS_7_BYTE] & CONSOLE_TYPE_MASK;
            if lookup == EXTENDED_CONSOLE {
                // Entry 0x03 means used the extended list.
                let lookup = data[SYSTEMS_BYTE] & SYSTEMS_MASK;
                nes.console_type = if let Some(v) = console_type().get(&lookup) {
                    *v
                } else {
                    return Err(eyre!("Illegal console system type: {}", data[SYSTEMS_BYTE]));
                };
            } else {
                // Under 3 are the same base consoles

                // This is fine to direct map since we know the map has this many entries.
                nes.console_type = *console_type().get(&lookup).ok_or(eyre!("impossible"))?;
            }

            // Now if this is a Vs one we need to fill that in.
            if nes.console_type == ConsoleType::NintendoVsSystem {
                let lookup = data[SYSTEMS_BYTE] & VS_PPU_TYPE_MASK;
                nes.vs_ppu = if let Some(v) = vs_ppu().get(&lookup) {
                    Some(*v)
                } else {
                    return Err(eyre!("Invalid Vs PPU type: {}", data[SYSTEMS_BYTE]));
                };
                let lookup = (data[SYSTEMS_BYTE] & VS_HARDWARE_TYPE_MASK) >> VS_HARDWARE_TYPE_SHIFT;
                nes.vs_hw = if let Some(v) = vs_hw().get(&lookup) {
                    Some(*v)
                } else {
                    return Err(eyre!("Invalid Vs Hardware type: {}", data[SYSTEMS_BYTE]));
                };
            }

            nes.mapper |= u16::from(data[MAPPER_BYTE] & MAPPER_D8_D11_MASK) << MAPPER_D8_D11_SHIFT;
            nes.sub_mapper =
                Some((data[MAPPER_BYTE] & MAPPER_SUBMAPPER_MASK) >> MAPPER_SUBMAPPER_SHIFT);

            // PRG/CHR ROM already covered above if this matched NES 2.0
            let shift = data[PRG_RAM_BYTE] & PRG_RAM_SHIFT_MASK;
            if shift > 0 {
                nes.prg_ram = 64 << shift;
            }
            let shift = (data[PRG_RAM_BYTE] & PRG_NVRAM_SHIFT_MASK) >> PRG_NVRAM_SHIFT;
            if shift > 0 {
                nes.prg_nvram = 64 << shift;
            }
            let shift = data[CHR_RAM_BYTE] & CHR_RAM_SHIFT_MASK;
            if shift > 0 {
                nes.chr_ram = 64 << shift;
            }
            let shift = (data[CHR_RAM_BYTE] & CHR_NVRAM_SHIFT_MASK) >> CHR_NVRAM_SHIFT;
            if shift > 0 {
                nes.chr_nvram = 64 << shift;
            }

            let lookup = data[TIMING_BYTE];
            nes.cpu_timing = if let Some(v) = cpu_timing().get(&lookup) {
                *v
            } else {
                return Err(eyre!("Invalid CPU timing byte: {}", data[TIMING_BYTE]));
            };

            if data[MISC_ROMS_BYTE] != 0x00 {
                if data[MISC_ROMS_BYTE] > 3 {
                    return Err(eyre!(
                        "Invalid number of trailing ROM segments: {}",
                        data[MISC_ROMS_BYTE]
                    ));
                }
                nes.num_misc_rom = data[MISC_ROMS_BYTE];

                let mut end = nes.chr.len() * CHR_BLOCK_SIZE_U
                    + nes.prg.len() * PRG_BLOCK_SIZE_U
                    + HEADER_SIZE_U;
                if nes.trainer.is_some() {
                    end += TRAINER_SIZE_U;
                }
                if end >= data.len() {
                    return Err(eyre!("Misc ROMs claims segments but no size remains?"));
                }
                let size = data.len() - end;
                let mut rom = vec![0; size];
                // SAFETY: We know this fits from the size checks above.
                // Copy the ROM data over.
                unsafe {
                    std::ptr::copy_nonoverlapping(&data[end], rom.as_mut_ptr(), size);
                }
                nes.misc_rom = Some(rom);
            }

            let lookup = data[EXPANSION_DEVICE_BYTE];
            let Some(v) = expansion_device().get(&lookup) else {
                return Err(eyre!(
                    "Invalid expansion device value {}",
                    data[EXPANSION_DEVICE_BYTE]
                ));
            };
            // There's one entry that's defined (so the iterator maps correctly) but
            // isn't used. If that's there the same error as above happens.
            nes.expansion_device = if *v == ExpansionDevice::Reserved {
                return Err(eyre!(
                    "Invalid expansion device value {}",
                    data[EXPANSION_DEVICE_BYTE]
                ));
            } else if *v != ExpansionDevice::Unspecified {
                Some(*v)
            } else {
                None
            };
        }
        CartStyle::ArchaicNES => {}
    };

    Ok(nes)
}
