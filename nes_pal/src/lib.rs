//! `nes_pal` implements a library for loading NES .pal palette files and
//! returning their parsed structs.

use color_eyre::eyre::{eyre, Result};

/// `Color` describes a parsed PAL entry
pub struct Color {
    /// Red
    pub r: u8,
    /// Green
    pub g: u8,
    /// Blue
    pub b: u8,
}

const ENTRIES: usize = 64;
const BYTE_PER_ENTRY: usize = 3;
const TOTAL_SIZE: usize = ENTRIES * BYTE_PER_ENTRY;

/// Given the input slice of raw PAL data return a Vec of parsed entries.
///
/// # Errors
/// The input must be exactly sized to 192 bytes (64 entries) to match NES palettes or
/// an error will result.
pub fn parse_pal(input: &[u8]) -> Result<Vec<Color>> {
    if input.len() != TOTAL_SIZE {
        return Err(eyre!(
            "Must have {} bytes for {} entries in the PAL data",
            TOTAL_SIZE,
            ENTRIES
        ));
    }
    let mut out = Vec::new();

    for i in 0..ENTRIES {
        let offset = i * BYTE_PER_ENTRY;
        out.push(Color {
            r: input[offset],
            g: input[offset + 1],
            b: input[offset + 2],
        });
    }
    Ok(out)
}
