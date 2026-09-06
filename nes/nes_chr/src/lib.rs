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
    if !data.len().is_multiple_of(8_192) {
        return Err(eyre!("Length of data must be units of 8KB"));
    }

    let mut ret = Vec::new();

    // Each tile in data consumes 16 bytes to represent it.
    // It's 2 bit planes so you need the the first bit from X and the
    // 2nd from X+8 OR'd with it. So process in chunks of 16 for each tile.
    // The Tile is just 8x8 of 2 bits per byte representing the attribute entry
    // to use for this pixel.
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

/// Encodes a tile's palette lookup data (0-3 per pixel) back into the 2 bit
/// plane, 16 byte CHR ROM format -- the exact inverse of the decoding done
/// by [`map_chr_rom`] for a single tile.
#[must_use]
pub fn tile_to_chr_bytes(tile: &Tile) -> [u8; 16] {
    let mut out = [0u8; 16];
    for idx in 0..8 {
        let mut first = 0u8;
        let mut second = 0u8;
        for bit in 0..8 {
            let e = tile.data[idx * 8 + 7 - bit];
            first |= (e & 0x01) << bit;
            second |= ((e & 0x02) >> 1) << bit;
        }
        out[idx] = first;
        out[idx + 8] = second;
    }
    out
}

/// Encodes a full set of tiles (as produced by [`map_chr_rom`]) back into a
/// raw CHR ROM byte block.
///
/// # Errors
/// Returns an error if `tiles` isn't a multiple of the number of tiles
/// [`map_chr_rom`] would produce for a whole number of 8KB blocks.
pub fn tiles_to_chr_rom(tiles: &[Tile]) -> Result<Vec<u8>> {
    if !(tiles.len() * 16).is_multiple_of(8_192) {
        return Err(eyre!(
            "Number of tiles ({}) doesn't divide evenly into 8KB blocks",
            tiles.len()
        ));
    }
    let mut out = Vec::with_capacity(tiles.len() * 16);
    for tile in tiles {
        out.extend_from_slice(&tile_to_chr_bytes(tile));
    }
    Ok(out)
}

/// Given a tile of data in palette lookup form (0-3 values) return
/// a set of strings describing it where . == background (0) and 1-3
/// for palette lookups.
#[must_use]
pub fn tile_print(data: &[u8; 64]) -> Vec<String> {
    let mut ret = Vec::new();
    for y in 0..8 {
        let mut line = String::new();
        for x in 0..8 {
            let e = data[y * 8 + x];
            if e == 0x00 {
                #[allow(clippy::unwrap_used)]
                write!(line, ".").unwrap();
            } else {
                #[allow(clippy::unwrap_used)]
                write!(line, "{e}").unwrap();
            }
        }
        ret.push(line);
    }
    ret
}
