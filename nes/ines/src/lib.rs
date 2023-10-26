//! `ines` provides utilities for decoding an INES file into the relative
//! components (PRG, CHR, etc) as well as decoding all of the flags.
//!
//! It supports Archaic, INES and NES 2.0 formats as described in
//! <https://www.nesdev.org/wiki/INES>

use color_eyre::eyre::{eyre, Result};
use std::fmt::{Debug, Display};
use strum_macros::Display;

/// The full description and data from a .ines file
pub struct NES {
    /// PRG-ROM segments
    pub prg: Vec<[u8; 16_384]>,

    /// CHR-ROM segments
    pub chr: Vec<[u8; 8_192]>,

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
    pub prg_ram: usize,

    /// PRG NVRAM size
    pub prg_nvram: usize,

    /// CHR RAM size
    pub chr_ram: usize,

    /// CHR NVRAM size
    pub chr_nvram: usize,

    /// CPU Timing
    pub cpu_timing: CPUTiming,

    /// VS system PPU
    pub vs_ppu: Option<VsPPU>,

    /// VS system hardware
    pub vs_hw: Option<VsHardware>,

    /// Any trailing ROM data
    pub misc_rom: Option<Vec<u8>>,

    /// The number of misc ROM's contained in the misc_rom field
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
        // and an NTSC NES with no mapper.
        Self {
            prg: vec![[0; 16_384]],
            chr: vec![[0; 8_192]],
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
            .field("trainer", &self.trainer)
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
            .field("misc_rom", &self.misc_rom)
            .field("num_misc_rom", &self.num_misc_rom)
            .field("expansion_device", &self.expansion_device)
            .field("cart_style", &self.cart_style)
            .finish()
    }
}

impl Display for NES {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // The debug version is fine.
        write!(f, "{self:?}")
    }
}

/// Nametable mirroring
#[derive(Default, Debug, Display)]
pub enum NametableMirroring {
    /// Horizonal
    #[default]
    Horizontal,

    /// Vertical
    Vertical,
}

/// Console type
#[derive(Default, Debug, Display)]
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

/// CPU Timing for cart
#[derive(Default, Debug, Display)]
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

/// Vs system PPU
#[derive(Debug, Display)]
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

/// Vs Hardware
#[derive(Debug, Display)]
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

/// Game expected devices for cart
#[derive(Debug, Display)]
pub enum ExpansionDevice {
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

    /// BitCorp Bit79 Keyboard
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

    /// UForce
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

const ROM_EXPONENT_MODE: u16 = 0x0F;
const ROM_EXPONENT_MASK: u16 = 0xFC;
const ROM_MULTIPLIER_MASK: u16 = 0x03;
const ROM_EXPONENT_SHIFT: u16 = 2;

const CHR_ROM_MSB_SHIFT: u8 = 4;

const CART_TYPE_MASK: u8 = 0x0C;

const CONSOLE_TYPE_MASK: u8 = 0x03;

const MAPPER_D0_D3_SHIFT: usize = 4;
const MAPPER_D8_D11_SHIFT: usize = 8;
const MAPPER_SUBMAPPER_SHIFT: usize = 4;

const TRAINER_SIZE: usize = 512;
const HEADER_SIZE: usize = 16;
const PRG_BLOCK_SIZE: usize = 16_384;
const CHR_BLOCK_SIZE: usize = 8_192;
const PRG_RAM_BLOCK_SIZE: usize = 8_192;
const MIN_SIZE: usize = HEADER_SIZE + PRG_BLOCK_SIZE + CHR_BLOCK_SIZE;

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

const SYSTEMS_MASK: u8 = 0x0F;
const VS_PPU_TYPE_MASK: u8 = 0x0F;
const VS_HARDWARE_TYPE_MASK: u8 = 0xF0;
const VS_HARDWARE_TYPE_SHIFT: u8 = 4;

const INES1_TV_MASK: u8 = 0x01;

/// Parse a given set of .ines data into an NES struct. This can handle legacy
/// and NES 2.0 data.
///
/// # Errors
/// PRG or CHR ROM data not aligned, missing or extra data can result in an error.
#[allow(clippy::too_many_lines)]
pub fn parse(data: &[u8]) -> Result<Box<NES>> {
    let mut nes = Box::<NES>::default();

    // The bare minimum size is the header, 1 PRG and 1 CHR.
    if data.len() < MIN_SIZE {
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

    let mut prg_blocks = usize::from(data[PRG_BYTE]);
    let mut chr_blocks = usize::from(data[CHR_BYTE]);

    if data[FLAGS_6_BYTE] & MIRROR_MASK != 0x00 {
        nes.nametable_mirror = NametableMirroring::Vertical;
    }
    nes.battery = data[FLAGS_6_BYTE] & BATTERY_MASK != 0x00;

    if data[FLAGS_6_BYTE] & TRAINER_MASK != 0x00 {
        if data.len() < MIN_SIZE + TRAINER_SIZE {
            return Err(eyre!(
                "Can't have trainer as size {} < min size {}",
                data.len(),
                MIN_SIZE + TRAINER_SIZE
            ));
        }
        let mut trainer = [0u8; TRAINER_SIZE];
        // SAFETY: We know this fits from the size checks above.
        // Copy the trainer data over.
        unsafe { std::ptr::copy_nonoverlapping(&data[16], trainer.as_mut_ptr(), trainer.len()) }
        nes.trainer = Some(trainer);
    }

    nes.four_screen_mode = data[FLAGS_6_BYTE] & FOUR_SCREEN_MASK != 0x00;
    nes.mapper = u16::from(data[FLAGS_6_BYTE] & MAPPER_D0_D3_MASK >> MAPPER_D0_D3_SHIFT);

    // Before moving on figure out the ROM type as that matters whether we even
    // care about the next byte.
    nes.cart_style = match data[FLAGS_7_BYTE] & CART_TYPE_MASK {
        0x08 => {
            let prg_msb = u16::from(data[PRG_CHR_MSB_BYTE] & PRG_ROM_MSB_MASK);
            let chr_msb = u16::from(data[PRG_CHR_MSB_BYTE] & CHR_ROM_MSB_MASK) >> CHR_ROM_MSB_SHIFT;
            #[allow(clippy::cast_possible_truncation)]
            let prg_rom = (prg_msb << ROM_MSB_SHIFT) | prg_blocks as u16;
            #[allow(clippy::cast_possible_truncation)]
            let chr_rom = (chr_msb << ROM_MSB_SHIFT) | chr_blocks as u16;

            // Blocks or exponent mode.
            let prg_size = if prg_msb == ROM_EXPONENT_MODE {
                let exp = u32::from(prg_rom & ROM_EXPONENT_MASK >> ROM_EXPONENT_SHIFT);
                // Multiplier is bottom 2 bits * 2 + 1 (so 1 3 5 or 7)
                let mul = u128::from((prg_rom & ROM_MULTIPLIER_MASK) * 2 + 1);
                2u128.pow(exp) * mul
            } else {
                #[allow(clippy::cast_possible_truncation)]
                u128::from(PRG_BLOCK_SIZE as u16 * prg_rom)
            };
            let chr_size = if chr_msb == ROM_EXPONENT_MODE {
                let exp = u32::from(chr_rom & ROM_EXPONENT_MASK >> ROM_EXPONENT_SHIFT);
                // Multiplier is bottom 2 bits * 2 + 1 (so 1 3 5 or 7)
                let mul = u128::from((chr_rom & ROM_MULTIPLIER_MASK) * 2 + 1);
                2u128.pow(exp) * mul
            } else {
                #[allow(clippy::cast_possible_truncation)]
                u128::from(CHR_BLOCK_SIZE as u16 * chr_rom)
            };
            // Over 512M, 1G just give up, we're not playing here.
            if chr_size > 1 << 28 || prg_size < 1 << 29 {
                return Err(eyre!(
                    "CHR or PRG size too large (over 512M, 1G): prg_size {prg_size} chr_size {chr_size}"
                ));
            }
            if (data.len() as u128) < HEADER_SIZE as u128 + prg_size + chr_size {
                CartStyle::ArchaicNES
            } else {
                // 1G / 16K (max) will fit into a u16 so casts are fine.
                // 2^30 / 2^14 = 2^16
                #[allow(clippy::cast_possible_truncation)]
                let blocks = (prg_size / PRG_BLOCK_SIZE as u128) as usize;
                prg_blocks = blocks;

                // 512M / 8K (max) will fit into a u16 so casts are fine.
                // 2^29 / 2^13 = 2^16
                #[allow(clippy::cast_possible_truncation)]
                let blocks = (chr_size / CHR_BLOCK_SIZE as u128) as usize;
                chr_blocks = blocks;
                CartStyle::NES20
            }
        }
        0x00 => {
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
    if data.len() < min_req {
        return Err(eyre!(
            "PRG + CHR (and optional trainer) and header data size {} doesn't match data len {}",
            min_req,
            data.len()
        ));
    }
    if prg_blocks > 1 {
        nes.chr.reserve_exact(prg_blocks - 1);
    }
    if chr_blocks > 1 {
        nes.chr.reserve_exact(chr_blocks - 1);
    }

    if nes.cart_style != CartStyle::ArchaicNES {
        nes.mapper |= u16::from(data[FLAGS_7_BYTE] & MAPPER_D4_D7_MASK);
    }

    match nes.cart_style {
        CartStyle::INES1 => {
            // Very little otherwise to set. Could have a Vs system
            // and may specify PRG RAM and TV type but unlikely to be set/used.
            if data[FLAGS_7_BYTE] & CONSOLE_TYPE_MASK == 0x01 {
                nes.console_type = ConsoleType::NintendoVsSystem;
            }
            let blocks = usize::from(data[MAPPER_BYTE]);
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
            nes.console_type = match data[FLAGS_7_BYTE] & CONSOLE_TYPE_MASK {
                0x00 => ConsoleType::NintendoEntertainmentSystem,
                0x01 => {
                    nes.vs_ppu = Some(match data[SYSTEMS_BYTE] & VS_PPU_TYPE_MASK {
                        0x00 => VsPPU::RP2C03B,
                        0x01 => VsPPU::RP2C03G,
                        0x02 => VsPPU::RP2C04_0001,
                        0x03 => VsPPU::RP2C04_0002,
                        0x04 => VsPPU::RP2C04_0003,
                        0x05 => VsPPU::RP2C04_0004,
                        0x06 => VsPPU::RC2C03B,
                        0x07 => VsPPU::RC2C03C,
                        0x08 => VsPPU::RC2C05_01,
                        0x09 => VsPPU::RC2C05_02,
                        0x0A => VsPPU::RC2C05_03,
                        0x0B => VsPPU::RC2C05_04,
                        0x0C => VsPPU::RC2C05_05,
                        _ => return Err(eyre!("Invalid Vs PPU type: {}", data[SYSTEMS_BYTE])),
                    });
                    nes.vs_hw = Some(
                        match data[SYSTEMS_BYTE] & VS_HARDWARE_TYPE_MASK >> VS_HARDWARE_TYPE_SHIFT {
                            0x00 => VsHardware::Unisystem,
                            0x01 => VsHardware::UnisystemRBIBaseball,
                            0x02 => VsHardware::UnisystemTKOBoxing,
                            0x03 => VsHardware::UnisystemSuperXevious,
                            0x04 => VsHardware::UnisystemIceClimberJapan,
                            0x05 => VsHardware::DualSystem,
                            0x06 => VsHardware::DualSystemRaidOnBungelingBay,
                            _ => {
                                return Err(eyre!(
                                    "Invalid Vs Hardware type: {}",
                                    data[SYSTEMS_BYTE]
                                ))
                            }
                        },
                    );
                    ConsoleType::NintendoVsSystem
                }
                0x02 => ConsoleType::NintendoPlaychoice10,
                0x03 => match data[SYSTEMS_BYTE] & SYSTEMS_MASK {
                    0x00 => ConsoleType::NintendoEntertainmentSystem,
                    0x01 => ConsoleType::NintendoVsSystem,
                    0x02 => ConsoleType::NintendoPlaychoice10,
                    0x03 => ConsoleType::FamicomWithBCD,
                    0x04 => ConsoleType::FamicomNESWithEPSM,
                    0x05 => ConsoleType::VRTechnologyVT01,
                    0x06 => ConsoleType::VRTechnologyVT02,
                    0x07 => ConsoleType::VRTechnologyVT03,
                    0x08 => ConsoleType::VRTechnologyVT09,
                    0x09 => ConsoleType::VRTechnologyVT32,
                    0x0A => ConsoleType::VRTechnologyVT369,
                    0x0B => ConsoleType::UMCUM6578,
                    0x0C => ConsoleType::FamicomNetworkSystem,
                    _ => return Err(eyre!("Illegal console system type: {}", data[SYSTEMS_BYTE])),
                },
                _ => todo!("Impossible due to mask)"),
            };
            nes.mapper |= u16::from(data[MAPPER_BYTE] & MAPPER_D8_D11_MASK) << MAPPER_D8_D11_SHIFT;
            nes.sub_mapper =
                Some((data[MAPPER_BYTE] & MAPPER_SUBMAPPER_MASK) >> MAPPER_SUBMAPPER_SHIFT);

            // PRG/CHR ROM already covered above if this matched NES 2.0
            let shift = data[PRG_RAM_BYTE] & PRG_RAM_SHIFT_MASK;
            if shift > 0 {
                nes.prg_ram = 64 << shift;
            }
            let shift = data[PRG_RAM_BYTE] & PRG_NVRAM_SHIFT_MASK >> PRG_NVRAM_SHIFT;
            if shift > 0 {
                nes.prg_nvram = 64 << shift;
            }
            let shift = data[CHR_RAM_BYTE] & CHR_RAM_SHIFT_MASK;
            if shift > 0 {
                nes.chr_ram = 64 << shift;
            }
            let shift = data[CHR_RAM_BYTE] & CHR_NVRAM_SHIFT_MASK >> CHR_NVRAM_SHIFT;
            if shift > 0 {
                nes.chr_nvram = 64 << shift;
            }

            nes.cpu_timing = match data[TIMING_BYTE] {
                0x00 => CPUTiming::NTSC,
                0x01 => CPUTiming::PAL,
                0x02 => CPUTiming::MultiRegion,
                0x03 => CPUTiming::Dendy,
                _ => return Err(eyre!("Invalid CPU timing byte: {}", data[TIMING_BYTE])),
            };

            if data[MISC_ROMS_BYTE] != 0x00 {
                nes.num_misc_rom = data[MISC_ROMS_BYTE];
                if nes.num_misc_rom > 3 {
                    return Err(eyre!(
                        "Invalid number of trailing ROM segments: {}",
                        data[MISC_ROMS_BYTE]
                    ));
                }
                let mut end =
                    nes.chr.len() * CHR_BLOCK_SIZE + nes.prg.len() * PRG_BLOCK_SIZE + HEADER_SIZE;
                if nes.trainer.is_some() {
                    end += TRAINER_SIZE;
                }
                if end > data.len() {
                    return Err(eyre!("Misc ROMs claims segments but no size remains?"));
                }
                let size = data.len() - end;
                let mut rom = Vec::with_capacity(size);
                // SAFETY: We know this fits from the size checks above.
                // Copy the ROM data over.
                unsafe {
                    std::ptr::copy_nonoverlapping(&data[end], rom.as_mut_ptr(), size);
                }
                nes.misc_rom = Some(rom);
            }
            if data[EXPANSION_DEVICE_BYTE] != 0x00 {
                nes.expansion_device = Some(match data[EXPANSION_DEVICE_BYTE] {
                    0x01 => ExpansionDevice::StandardNES,
                    0x02 => ExpansionDevice::NESFourScore,
                    0x03 => ExpansionDevice::FamicomFourPlayersAdapter,
                    0x04 => ExpansionDevice::VsSystem4016,
                    0x05 => ExpansionDevice::VsSystem4017,
                    0x07 => ExpansionDevice::VsZapper,
                    0x08 => ExpansionDevice::Zapper,
                    0x09 => ExpansionDevice::TwoZappers,
                    0x0A => ExpansionDevice::BandaiHyperShotLightgun,
                    0x0B => ExpansionDevice::PowerPadSideA,
                    0x0C => ExpansionDevice::PowerPadSideB,
                    0x0D => ExpansionDevice::FamilyTrainerSideA,
                    0x0E => ExpansionDevice::FamilyTrainerSideB,
                    0x0F => ExpansionDevice::ArkanoidVausControllerNES,
                    0x10 => ExpansionDevice::ArkanoidVausControllerFamicom,
                    0x11 => ExpansionDevice::TwoVausControllersFamicomDataRecorder,
                    0x12 => ExpansionDevice::KonamiHyperShotController,
                    0x13 => ExpansionDevice::CoconutsPachinkoController,
                    0x14 => ExpansionDevice::ExcitingBoxingPunchingBag,
                    0x15 => ExpansionDevice::JissenMahjongController,
                    0x16 => ExpansionDevice::PartyTap,
                    0x17 => ExpansionDevice::OekaKidsTablet,
                    0x18 => ExpansionDevice::SunsoftBarcodeBattler,
                    0x19 => ExpansionDevice::MiraclePianoKeyboard,
                    0x1A => ExpansionDevice::PokkunMoguraa,
                    0x1B => ExpansionDevice::TopRider,
                    0x1C => ExpansionDevice::DoubleFisted,
                    0x1D => ExpansionDevice::Famicom3DSystem,
                    0x1E => ExpansionDevice::DoremikkoKeyboard,
                    0x1F => ExpansionDevice::ROBGyroSet,
                    0x20 => ExpansionDevice::FamicomDataRecorder,
                    0x21 => ExpansionDevice::ASCIITurboFile,
                    0x22 => ExpansionDevice::IGSStorageBattleBox,
                    0x23 => ExpansionDevice::FamilyBASICKeyboard,
                    0x24 => ExpansionDevice::DongdaPEC586Keyboard,
                    0x25 => ExpansionDevice::BitCorpBit79Keyboard,
                    0x26 => ExpansionDevice::SuborKeyboard,
                    0x27 => ExpansionDevice::SuborKeyboardPlusMouse,
                    0x28 => ExpansionDevice::SuborKeyboardPlusMouse24bit4016,
                    0x29 => ExpansionDevice::SNESMouse,
                    0x2A => ExpansionDevice::Multicart,
                    0x2B => ExpansionDevice::TwoSNESControllers,
                    0x2C => ExpansionDevice::RacerMateBicycle,
                    0x2D => ExpansionDevice::UForce,
                    0x2E => ExpansionDevice::ROBStackUp,
                    0x2F => ExpansionDevice::CityPatrolmanLightgun,
                    0x30 => ExpansionDevice::SharpC1CassetteInterface,
                    0x31 => ExpansionDevice::StandardControllersSwapped,
                    0x32 => ExpansionDevice::ExcaliborSudokuPad,
                    0x33 => ExpansionDevice::ABLPinball,
                    0x34 => ExpansionDevice::GoldenNuggetCasino,
                    0x35 => ExpansionDevice::GoldenKeyKeyboard,
                    0x36 => ExpansionDevice::SuborKeyboardPlusMouse24bit4017,
                    0x37 => ExpansionDevice::PortTestController,
                    0x38 => ExpansionDevice::BandaiMultiGamePlayerGamepad,
                    0x39 => ExpansionDevice::VenomTVDanceMat,
                    0x3A => ExpansionDevice::LGTVRemoteControl,
                    _ => {
                        return Err(eyre!(
                            "Invalid expansion device value {}",
                            data[EXPANSION_DEVICE_BYTE]
                        ))
                    }
                });
            }
        }
        CartStyle::ArchaicNES => {}
    };

    Ok(nes)
}
