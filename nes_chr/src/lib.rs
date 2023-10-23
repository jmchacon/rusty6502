//! `nes_chr` provides functions and structures for manipulating CHR ROM
//! segments for the NES.

use color_eyre::eyre::{eyre, Result};
use std::fmt::Write;

#[cfg(test)]
mod tests;

/// `Tile` represents a parsed entry from the CHR ROM.
#[derive(Debug)]
pub struct Tile {
    /// This is the 8x8 tile with each value being 0-3 to indicate
    /// the color entry to use (where 0 is background).
    pub data: [u8; 8 * 8],
}

impl Default for Tile {
    fn default() -> Self {
        // Have to implement this ourselves as arrays don't go to 64 for defaults.
        Self { data: [0; 8 * 8] }
    }
}

/// Given a chunk of data (which must be 8KB aligned)
/// parse it and return a list of bitmaps in 8x8 format
///
/// # Errors
/// If the supplied data is not 8KB aligned this will return an error.
pub fn map_chr_rom(data: &[u8]) -> Result<Vec<Tile>> {
    if data.len() % 8_192 != 0 {
        return Err(eyre!("Length of data must be units of 8KB"));
    }

    let mut ret = Vec::new();

    // Each tile consumes 16 bytes to represent it.
    // It's 2 bit planes so you need the the first bit from X and the
    // 2nd from X+8 OR'd with it. So process in chunks of 16 for each tile.
    for i in 0..data.len() / 16 {
        let mut tile = Tile::default();

        for idx in 0..8 {
            let first = data[i * 16 + idx];
            let second = data[i * 16 + idx + 8];

            for bit in 0..8 {
                // The first bit is always extracted and then shifted to the right end.
                let one = (first & (1 << bit)) >> bit;
                // The second bit does the same (but stops in the 2nd bit) unless
                // we're at the first bit in which case put it directly into bit 2.
                let two = if bit > 0 {
                    (second & (1 << bit)) >> (bit - 1)
                } else {
                    (second & 0x01) << 1
                };
                // The tile data goes in direct bit order so bit 7 is the first
                // index and bit 0 is the 8th index.
                tile.data[idx * 8 + 7 - bit] = one | two;
            }
        }
        ret.push(tile);
    }
    Ok(ret)
}

/// Given a tile of data in palette lookup form (0-3 values) return
/// a set of strings describing it where . == background (0) and 1-3
/// for palette lookups.
pub fn tile_print(data: &[u8; 64]) -> Vec<String> {
    let mut ret = Vec::new();
    for y in 0..8 {
        let mut line = String::new();
        for x in 0..8 {
            let e = data[y * 8 + x];
            if e == 0x00 {
                write!(line, ".").unwrap();
            } else {
                write!(line, "{e}").unwrap();
            }
        }
        ret.push(line);
    }
    ret
}
