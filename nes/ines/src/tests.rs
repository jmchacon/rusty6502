use crate::{
    CPUTiming, CartStyle, ConsoleType, ExpansionDevice, NametableMirroring, VsHardware, VsPPU,
    CHR_BLOCK_SIZE_U, CHR_BYTE, CHR_RAM_BYTE, EXPANSION_DEVICE_BYTE, EXTENDED_CONSOLE,
    FLAGS_6_BYTE, FLAGS_7_BYTE, HEADER_SIZE_U, INES1_TV_MASK, INES_CART_SIG, MAPPER_BYTE,
    MIN_SIZE_U, MIRROR_MASK, MISC_ROMS_BYTE, NES, NES20_CART_SIG, PRG_BLOCK_SIZE_U, PRG_BYTE,
    PRG_CHR_MSB_BYTE, PRG_RAM_BLOCK_SIZE, PRG_RAM_BYTE, ROM_EXPONENT_MODE, SIG_BYTE_0, SIG_BYTE_1,
    SIG_BYTE_2, SIG_BYTE_3, SYSTEMS_BYTE, TIMING_BYTE, TRAINER_MASK, TRAINER_SIZE_U,
};
use std::{fs::read, path::Path};

use color_eyre::eyre::Result;
use strum::{EnumCount, IntoEnumIterator};

#[test]
fn nestest_rom() -> Result<()> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../testdata/nestest.nes");
    println!("Path: {path:?}");
    let bytes: Vec<u8> = read(path)?;

    let ines = crate::parse(&bytes)?;

    let mut want = Box::new(NES {
        cart_style: CartStyle::INES1,
        prg_ram: 8192,
        ..Default::default()
    });

    // SAFETY: The test ROM has known sizes and a default NES has 1 PRG and 1 CHR
    //         so everything matches size wise.
    unsafe {
        std::ptr::copy_nonoverlapping(
            &raw const bytes[HEADER_SIZE_U],
            want.prg[0].as_mut_ptr(),
            PRG_BLOCK_SIZE_U,
        );
        std::ptr::copy_nonoverlapping(
            &raw const bytes[HEADER_SIZE_U + PRG_BLOCK_SIZE_U],
            want.chr[0].as_mut_ptr(),
            CHR_BLOCK_SIZE_U,
        );
    }

    validate_nes("nestest_rom", &ines, &want);

    Ok(())
}

fn validate_nes(test: &str, ines: &NES, want: &NES) {
    assert!(
        ines.prg == want.prg,
        "{test}: PRG sections different\nGot:\n{:?}\nWant:\n{:?}",
        ines.prg,
        want.prg
    );
    assert!(
        ines.chr == want.chr,
        "{test}: CHR sections different\nGot:\n{:?}\nWant:\n{:?}",
        ines.chr,
        want.chr
    );
    assert!(
        ines.trainer == want.trainer,
        "{test}: Trainer sections different:\nGot:\n{:?}\nWant:\n{:?}",
        ines.trainer,
        want.trainer
    );
    assert!(
        ines.misc_rom == want.misc_rom,
        "{test}: Misc ROM sections different:\nGot:\n{:?}\nWant:\n{:?}",
        ines.misc_rom,
        want.misc_rom
    );
    assert!(
        ines == want,
        "{test}: NES differs\nGot:\n{ines}\nWant:\n{want}"
    );
}

#[test]
fn bad_roms() -> Result<()> {
    let mut data = Vec::new();

    // Should fail for being too small
    let res = crate::parse(&data);
    assert!(res.is_err(), "Parsed an empty set of data? - {res:?}");

    // Now push a header in but a short PRG and no CHR
    data.push(b'N');
    data.push(b'E');
    data.push(b'S');
    data.push(0x1A);
    data.push(0x01); // PRG
    data.push(0x00); // CHR

    data.resize(MIN_SIZE_U - 1, 0x00); // Fill remainder with 0's.

    // Should still fail
    let res = crate::parse(&data);
    assert!(res.is_err(), "Parsed a short set of data? - {res:?}");

    // Make it large enough.
    data.resize(MIN_SIZE_U, 0x00);

    // Validate this parses.
    let ines = crate::parse(&data)?;

    // An all zeros header implies more RAM sections and we have no CHR ROM.
    let mut want = Box::new(NES {
        chr: Vec::new(),
        cart_style: CartStyle::INES1,
        prg_ram: 8192,
        chr_ram: 8192,
        ..Default::default()
    });

    validate_nes("bad roms - valid rom", &ines, &want);

    // Now mess up the header
    data[0] = b'M';

    // Should still fail
    let res = crate::parse(&data);
    assert!(res.is_err(), "Parsed an invalid header? - {res:?}");

    // Put back the header and make this vertical mirroring now.
    data[0] = b'N';
    data[FLAGS_6_BYTE] = MIRROR_MASK;

    let ines = crate::parse(&data)?;
    want.nametable_mirror = NametableMirroring::Vertical;

    validate_nes("bad roms - vertical", &ines, &want);

    // Add trainer data which should fail due to lack of space.
    data[FLAGS_6_BYTE] = MIRROR_MASK | TRAINER_MASK;

    let res = crate::parse(&data);
    assert!(
        res.is_err(),
        "Parsed a trainer but not enough data? - {res:?}"
    );

    data.resize(MIN_SIZE_U + TRAINER_SIZE_U, 0x00);

    let ines = crate::parse(&data)?;
    want.trainer = Some([0; TRAINER_SIZE_U]);
    validate_nes("bad roms - trainer data", &ines, &want);

    println!("NES: {ines}");
    println!("NES debug: {ines:?}");

    // Now add a CHR block and parsing should fail.
    data[CHR_BYTE] = 0x01;
    let res = crate::parse(&data);
    assert!(
        res.is_err(),
        "Parsed without enough room for CHR? - {res:?}"
    );

    Ok(())
}

#[test]
#[allow(clippy::too_many_lines)]
fn nes20_parse() -> Result<()> {
    let mut data = vec![0; MIN_SIZE_U];
    data[SIG_BYTE_0] = b'N';
    data[SIG_BYTE_1] = b'E';
    data[SIG_BYTE_2] = b'S';
    data[SIG_BYTE_3] = 0x1A;

    // No PRG/CHR defined and also no prg_size defined.
    data[FLAGS_7_BYTE] = NES20_CART_SIG;

    // No PRG/CHR defined and also no prg/chr_size defined.
    // This should error.
    let res = crate::parse(&data);
    assert!(res.is_err(), "Parsed nes 2.0 but no PRG/CHR? - {res:?}");

    // Now create a single 16k PRG in 2.0 style
    data[PRG_CHR_MSB_BYTE] = ROM_EXPONENT_MODE.try_into()?;
    data[PRG_BYTE] = 0x0E << 2; // EXP = 2^14 == 16384

    // This should parse.
    let ines = crate::parse(&data)?;

    // NES2 is easier since 0's are default everywhere.
    let mut want = Box::new(NES {
        chr: Vec::new(),
        cart_style: CartStyle::NES20,
        sub_mapper: Some(0),
        ..Default::default()
    });

    validate_nes("nes 2.0 - basic", &ines, &want);

    // Reset PRG to 1 block classic style and validate it matches cart wise for archaic
    data[PRG_BYTE] = 0x01;
    data[FLAGS_7_BYTE] = b'D';
    data[MAPPER_BYTE] = b'i';
    data[PRG_CHR_MSB_BYTE] = b's';
    data[PRG_RAM_BYTE] = b'k';
    data[CHR_RAM_BYTE] = b'D';
    data[TIMING_BYTE] = b'u';
    data[SYSTEMS_BYTE] = b'd';
    data[MISC_ROMS_BYTE] = b'e';
    data[EXPANSION_DEVICE_BYTE] = b'!';

    // This should parse as Archaic and basics
    want.chr_ram = CHR_BLOCK_SIZE_U.try_into()?;
    want.cart_style = CartStyle::ArchaicNES;
    want.sub_mapper = None;

    let ines = crate::parse(&data)?;
    validate_nes("nes 2.0 - archaic", &ines, &want);

    // Now do the same thing but with an INES_CART_SIG instead of data.
    data[FLAGS_7_BYTE] = INES_CART_SIG;
    let ines = crate::parse(&data)?;
    validate_nes("nes 2.0 - ines1", &ines, &want);

    // Now add some CHR ROM in 2.0 exp format. 1 block is enough.
    // Reset everything else back to PRG exp mode and 2.0.
    data[PRG_BYTE] = 0x0E << 2; // EXP = 2^14 == 16384
    data[FLAGS_7_BYTE] = NES20_CART_SIG;
    data[MAPPER_BYTE] = 0x00;
    data[PRG_CHR_MSB_BYTE] = 0xFF;
    data[PRG_RAM_BYTE] = 0x00;
    data[CHR_RAM_BYTE] = 0x00;
    data[TIMING_BYTE] = 0x00;
    data[SYSTEMS_BYTE] = 0x00;
    data[MISC_ROMS_BYTE] = 0x00;
    data[EXPANSION_DEVICE_BYTE] = 0x00;

    want.chr_ram = 0x00;
    want.cart_style = CartStyle::NES20;
    want.sub_mapper = Some(0);

    data.resize(MIN_SIZE_U + CHR_BLOCK_SIZE_U, 0x00);
    data[CHR_BYTE] = 0x0D << 2; // EXP = 2^13 == 8192

    // This should parse.
    let ines = crate::parse(&data)?;
    want.chr = vec![[0; CHR_BLOCK_SIZE_U]];
    validate_nes("nes 2.0 - chr exp mode", &ines, &want);

    // Make the PRG ROM in exp format too large (in regular format that's not possible)
    data[PRG_BYTE] = 0x1F << 2; // EXP = 2 ^ 31 == 2G (so too large)

    let res = crate::parse(&data);
    assert!(res.is_err(), "Parsed nes 2.0 with too large PRG? - {res:?}");

    // Reset PRG and try too large CHR
    data[PRG_BYTE] = 0x0E << 2; // EXP = 2^14 == 16384
    data[CHR_BYTE] = 0x1E << 2; // EXP = 2^30 == 1G (so too large)

    let res = crate::parse(&data);
    assert!(res.is_err(), "Parsed nes 2.0 with too large CHR? - {res:?}");

    // Set this for 24k of PRG and 24k of CHR (so we round up blocks)
    data[PRG_BYTE] = (0x0D << 2) | 0x01; // EXP = 2^13 == 8192, M = 1 * 2 + 1 = 3 so 2^13*7 == 24k
    data[CHR_BYTE] = (0x0B << 2) | 0x03; // EXP = 2^11 == 2048, M = 3 * 2 + 1 = 7 so 2^11*7 == 14k

    // This assumes 1 more PRG and 1 more CHR block so extend data.
    data.resize(MIN_SIZE_U + PRG_BLOCK_SIZE_U + CHR_BLOCK_SIZE_U * 2, 0x00);

    want.prg = vec![[0; PRG_BLOCK_SIZE_U], [0; PRG_BLOCK_SIZE_U]];
    want.chr = vec![[0; CHR_BLOCK_SIZE_U], [0; CHR_BLOCK_SIZE_U]];

    let ines = crate::parse(&data)?;
    validate_nes("nes 2.0 - N blocks", &ines, &want);

    // Now add some trainer data.
    data.resize(
        MIN_SIZE_U + TRAINER_SIZE_U + PRG_BLOCK_SIZE_U + CHR_BLOCK_SIZE_U * 2,
        0x00,
    );
    data[FLAGS_6_BYTE] = TRAINER_MASK;
    want.trainer = Some([0; TRAINER_SIZE_U]);

    let ines = crate::parse(&data)?;
    validate_nes("nes 2.0 - trainer", &ines, &want);

    // Move to an extended console type
    data[FLAGS_7_BYTE] |= EXTENDED_CONSOLE;
    data[SYSTEMS_BYTE] = 0x08; // VR Technology VT09

    want.console_type = ConsoleType::VRTechnologyVT09;

    let ines = crate::parse(&data)?;
    validate_nes("nes 2.0 - extended console", &ines, &want);

    // Set an illegal one
    #[allow(clippy::cast_possible_truncation)]
    let bad = (ConsoleType::COUNT + 1) as u8;
    data[SYSTEMS_BYTE] = bad;

    let res = crate::parse(&data);
    assert!(
        res.is_err(),
        "Parsed nes 2.0 with bad console type? - {res:?}"
    );

    // Now make it a Vs type and fill that in
    #[allow(clippy::unwrap_used)]
    let (val, _) = ConsoleType::iter()
        .enumerate()
        .find(|(_, c)| *c == ConsoleType::NintendoVsSystem)
        .unwrap();
    #[allow(clippy::cast_possible_truncation)]
    let vs = val as u8;
    data[FLAGS_7_BYTE] = NES20_CART_SIG | vs;
    data[SYSTEMS_BYTE] = (0x05 << 4) | 0x06; // Vs. Dual System (normal) and RC2C03B PPU

    want.console_type = ConsoleType::NintendoVsSystem;
    want.vs_ppu = Some(VsPPU::RC2C03B);
    want.vs_hw = Some(VsHardware::DualSystem);

    let ines = crate::parse(&data)?;
    validate_nes("nes 2.0 - vs console", &ines, &want);

    // Invalid VsPPU
    #[allow(clippy::cast_possible_truncation)]
    let bad = (VsPPU::COUNT + 1) as u8;
    data[SYSTEMS_BYTE] = (0x05 << 4) | bad; // Vs. Dual System (normal) and bad

    let res = crate::parse(&data);
    assert!(
        res.is_err(),
        "Parsed nes 2.0 with bad VsPPU console type? - {res:?}"
    );

    // Good VsPPU but bad VsHardware
    #[allow(clippy::cast_possible_truncation)]
    let bad = (VsHardware::COUNT + 1) as u8;
    data[SYSTEMS_BYTE] = (bad << 4) | 0x06; // bad and RC2C03B PPU

    let res = crate::parse(&data);
    assert!(
        res.is_err(),
        "Parsed nes 2.0 with bad VsHardware console type? - {res:?}"
    );

    // Back to a good setup and add PRG/CHR RAM and NVRAM
    data[SYSTEMS_BYTE] = (0x05 << 4) | 0x06; // Vs. Dual System (normal) and RC2C03B PPU

    data[PRG_RAM_BYTE] = (0xB << 4) | 0xC; // 64 << 11 nvram and 64 << 12 RAM
    data[CHR_RAM_BYTE] = (0xC << 4) | 0xB; // 64 << 12 nvram and 64 << 11 RAM

    want.prg_ram = 64 << 0x0C;
    want.prg_nvram = 64 << 0x0B;
    want.chr_ram = 64 << 0x0B;
    want.chr_nvram = 64 << 0x0C;

    let ines = crate::parse(&data)?;
    validate_nes("nes 2.0 - vs console", &ines, &want);

    // Bad CPU Timing
    #[allow(clippy::cast_possible_truncation)]
    let bad = (CPUTiming::COUNT + 1) as u8;
    data[TIMING_BYTE] = bad;

    let res = crate::parse(&data);
    assert!(
        res.is_err(),
        "Parsed nes 2.0 with bad cpu timing? - {res:?}"
    );

    // Set a valid CPU timing
    data[TIMING_BYTE] = 0x02;
    want.cpu_timing = CPUTiming::MultiRegion;

    let ines = crate::parse(&data)?;
    validate_nes("nes 2.0 - cpu timing", &ines, &want);

    // Misc ROMs

    // Add a misc rom but don't give it any space.
    data[MISC_ROMS_BYTE] = 0x01;

    let res = crate::parse(&data);
    assert!(
        res.is_err(),
        "Parsed nes 2.0 with misc ROM but no space? - {res:?}"
    );

    // Invalid number of roms
    data[MISC_ROMS_BYTE] = 0x04;

    let res = crate::parse(&data);
    assert!(
        res.is_err(),
        "Parsed nes 2.0 with misc ROM but no space? - {res:?}"
    );

    // Add a ROM of 128 bytes
    data.resize(data.len() + 128, 0x00);
    data[MISC_ROMS_BYTE] = 3;
    want.misc_rom = Some(vec![0; 128]);
    want.num_misc_rom = 3;

    let ines = crate::parse(&data)?;
    validate_nes("nes 2.0 - misc roms", &ines, &want);

    // Expansion device.

    // Bad one
    #[allow(clippy::cast_possible_truncation)]
    let bad = (ExpansionDevice::COUNT + 1) as u8;
    data[EXPANSION_DEVICE_BYTE] = bad;

    let res = crate::parse(&data);
    assert!(
        res.is_err(),
        "Parsed nes 2.0 with bad expansion device? - {res:?}"
    );

    // The reserved one is also bad.
    #[allow(clippy::unwrap_used)]
    let (val, _) = ExpansionDevice::iter()
        .enumerate()
        .find(|(_, e)| *e == ExpansionDevice::Reserved)
        .unwrap();
    #[allow(clippy::cast_possible_truncation)]
    let res = val as u8;

    data[EXPANSION_DEVICE_BYTE] = res;

    let res = crate::parse(&data);
    assert!(
        res.is_err(),
        "Parsed nes 2.0 with reserved expansion device? - {res:?}"
    );

    // A valid one
    data[EXPANSION_DEVICE_BYTE] = 0x25; // Bit Corp. Bit-79 Keyboard
    want.expansion_device = Some(ExpansionDevice::BitCorpBit79Keyboard);

    let ines = crate::parse(&data)?;
    validate_nes("nes 2.0 - expansion device", &ines, &want);

    Ok(())
}

#[test]
fn nes_11_parse() -> Result<()> {
    let mut data = vec![0; MIN_SIZE_U];
    data[SIG_BYTE_0] = b'N';
    data[SIG_BYTE_1] = b'E';
    data[SIG_BYTE_2] = b'S';
    data[SIG_BYTE_3] = 0x1A;

    // 1 PRG and 0 CHR are fine
    data[PRG_BYTE] = 0x01;
    data[CHR_BYTE] = 0x00;
    data[FLAGS_6_BYTE] = 0x00;
    data[FLAGS_7_BYTE] = 0x01; // VsSystem
    data[MAPPER_BYTE] = 0x02; // PRG RAM
    data[PRG_CHR_MSB_BYTE] = INES1_TV_MASK;

    let mut want = Box::new(NES {
        chr: Vec::new(),
        cart_style: CartStyle::INES1,
        prg_ram: 2 * PRG_RAM_BLOCK_SIZE,
        chr_ram: CHR_BLOCK_SIZE_U.try_into()?,
        console_type: ConsoleType::NintendoVsSystem,
        cpu_timing: CPUTiming::PAL,
        ..Default::default()
    });

    let ines = crate::parse(&data)?;
    validate_nes("nes11 - vs", &ines, &want);

    // Now move to playchoice
    data[FLAGS_7_BYTE] = 0x02; // Playchoice;
    want.console_type = ConsoleType::NintendoPlaychoice10;

    let ines = crate::parse(&data)?;
    validate_nes("nes11 - playchoice", &ines, &want);

    // Finally an invalid one
    data[FLAGS_7_BYTE] = 0x03;

    let res = crate::parse(&data);
    assert!(res.is_err(), "Parsed an invalid console type 3? {res:?}");

    Ok(())
}
