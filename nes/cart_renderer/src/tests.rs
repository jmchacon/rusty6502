use crate::{build_output_bytes, EditableCart};
use color_eyre::eyre::Result;
use ines::{CHR_BLOCK_SIZE_U, HEADER_SIZE_U, NES20_CART_SIG, PRG_BLOCK_SIZE_U};

/// Builds a minimal, valid NES 2.0 file (1 PRG bank, 1 CHR bank, and a
/// distinguishing non-default submapper value that only NES 2.0 headers can
/// carry) with recognizable, non-zero PRG/CHR content so a round trip can
/// tell "unchanged" from "corrupted" instead of everything coincidentally
/// looking like zeros.
fn nes20_fixture() -> Vec<u8> {
    let mut data = vec![0u8; HEADER_SIZE_U + PRG_BLOCK_SIZE_U + CHR_BLOCK_SIZE_U];
    data[0..4].copy_from_slice(b"NES\x1A");
    data[4] = 1; // 1 PRG bank
    data[5] = 1; // 1 CHR bank
    data[6] = 0; // flags 6: no mirroring/battery/trainer/four-screen
    data[7] = NES20_CART_SIG; // identifies this as an NES 2.0 header
    data[8] = 0x50; // submapper = 5 (upper nibble) -- NES 2.0 only field
                    // bytes 9-15 (PRG/CHR MSB, RAM sizes, timing, systems, misc ROMs,
                    // expansion device) all stay 0/default.

    for (i, b) in data[HEADER_SIZE_U..HEADER_SIZE_U + PRG_BLOCK_SIZE_U]
        .iter_mut()
        .enumerate()
    {
        #[allow(clippy::cast_possible_truncation)]
        {
            *b = i as u8;
        }
    }
    let chr_start = HEADER_SIZE_U + PRG_BLOCK_SIZE_U;
    for (i, b) in data[chr_start..chr_start + CHR_BLOCK_SIZE_U]
        .iter_mut()
        .enumerate()
    {
        #[allow(clippy::cast_possible_truncation)]
        {
            *b = (i * 7 + 3) as u8;
        }
    }
    data
}

/// Parses `data` the same way [`crate::load_cart_for_editing`] does, without
/// needing a real file on disk.
fn to_editable_cart(data: &[u8]) -> Result<EditableCart> {
    let nes = ines::parse(data)?;
    let mut tiles = Vec::new();
    for t in &nes.chr {
        tiles.push(nes_chr::map_chr_rom(t)?);
    }
    let chr_offset = nes.chr_offset();
    Ok(EditableCart {
        tiles,
        raw: data.to_vec(),
        chr_offset,
    })
}

#[test]
fn saving_an_nes20_file_with_no_edits_reproduces_it_byte_for_byte() -> Result<()> {
    let data = nes20_fixture();
    let cart = to_editable_cart(&data)?;

    let out = build_output_bytes(&cart.raw, cart.chr_offset, &cart.tiles)?;

    assert_eq!(
        out, data,
        "saving with no edits should reproduce the original NES 2.0 file exactly"
    );
    Ok(())
}

#[test]
fn saving_a_blank_start_synthesizes_an_nes20_file() -> Result<()> {
    let cart = EditableCart::blank();

    let out = build_output_bytes(&cart.raw, cart.chr_offset, &cart.tiles)?;

    assert_eq!(&out[0..4], b"NES\x1A", "missing iNES/NES 2.0 magic");
    assert_eq!(
        out[7] & NES20_CART_SIG,
        NES20_CART_SIG,
        "a from-scratch save should be marked NES 2.0, not plain iNES"
    );
    // Default mapper (0, NROM): both the low nibble (flags 6) and high
    // nibble (flags 7) of the mapper number are 0.
    assert_eq!(out[6] & 0xF0, 0, "expected mapper 0 (low nibble)");
    assert_eq!(out[7] & 0xF0, 0, "expected mapper 0 (high nibble)");
    Ok(())
}

#[test]
fn saving_an_edited_nes20_file_only_changes_the_chr_region() -> Result<()> {
    let data = nes20_fixture();
    let mut cart = to_editable_cart(&data)?;

    // Edit a single pixel of the first tile.
    let original_pixel = cart.tiles[0][0].data[0];
    cart.tiles[0][0].data[0] = (original_pixel + 1) % 4;

    let out = build_output_bytes(&cart.raw, cart.chr_offset, &cart.tiles)?;

    // The header (including the NES 2.0-only submapper byte) and PRG data
    // are byte-for-byte identical to the original file.
    assert_eq!(
        out[..cart.chr_offset],
        data[..cart.chr_offset],
        "header/PRG region should be untouched by a CHR-only edit"
    );
    // Specifically, the submapper nibble (an NES 2.0-only field) survived.
    assert_eq!(out[8] & 0xF0, 0x50, "NES 2.0 submapper field was lost");

    // The CHR region changed size not at all, but its content did.
    assert_eq!(out.len(), data.len());
    assert_ne!(
        out[cart.chr_offset..],
        data[cart.chr_offset..],
        "the CHR region should reflect the edit"
    );
    Ok(())
}
