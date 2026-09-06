use crate::{map_chr_rom, tile_print, tile_to_chr_bytes, tiles_to_chr_rom};
use color_eyre::eyre::Result;

#[test]
fn invalid_chr() {
    let data: [u8; 6] = [0, 1, 2, 3, 4, 5];

    let res = map_chr_rom(&data);
    assert!(res.is_err(), "result isn't error? {res:?}");
}

#[test]
fn parse_tile() -> Result<()> {
    let mut data = [0_u8; 8_192];

    // File in just enough for 1 tile to parse.
    let tile: [u8; 16] = [
        0x41, 0xC2, 0x44, 0x48, 0x10, 0x20, 0x40, 0x80, 0x01, 0x02, 0x04, 0x08, 0x16, 0x21, 0x42,
        0x87,
    ];

    // SAFETY: We know this fits from the sizes above.
    unsafe { std::ptr::copy_nonoverlapping(tile.as_ptr(), data.as_mut_ptr(), tile.len()) }

    let tiles = map_chr_rom(&data)?;
    println!("{:?}", tiles[0]); // So Tile debug gets invoked.

    let want: [u8; 64] = [
        0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x03,
        0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x03, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x00, 0x02, 0x02, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00,
        0x00, 0x00, 0x02, 0x00, 0x03, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x03, 0x00, 0x00, 0x00,
        0x00, 0x02, 0x02, 0x02,
    ];

    let w = tile_print(&want);
    let g = tile_print(&tiles[0].data);
    if tiles[0].data != want {
        println!("Tiles differ");
        println!("Want:       Got:");
        for i in 0..8 {
            println!("{}    {}", w[i], g[i]);
        }
        panic!();
    }
    Ok(())
}

#[test]
fn round_trip_tile_encoding() -> Result<()> {
    // A full 8KB block (512 tiles) of varied bytes so the round trip
    // exercises every bit pattern, not just one hand picked tile.
    let mut data = [0_u8; 8_192];
    for (i, b) in data.iter_mut().enumerate() {
        // We know this cast is safe since it's masked to a byte.
        #[allow(clippy::cast_possible_truncation)]
        {
            *b = (i * 37 + 11) as u8;
        }
    }

    let tiles = map_chr_rom(&data)?;
    let re_encoded = tiles_to_chr_rom(&tiles)?;
    assert_eq!(
        re_encoded, data,
        "re-encoding decoded tiles didn't reproduce the original CHR bytes"
    );

    // A single tile's worth also round trips through `tile_to_chr_bytes`
    // directly.
    let single = tile_to_chr_bytes(&tiles[0]);
    assert_eq!(single, data[0..16]);
    Ok(())
}

#[test]
fn tiles_to_chr_rom_rejects_non_block_sized_input() {
    let tiles: Vec<_> = (0..3).map(|_| crate::Tile::default()).collect();
    let res = tiles_to_chr_rom(&tiles);
    assert!(
        res.is_err(),
        "expected error for non-8KB-aligned tile count"
    );
}
