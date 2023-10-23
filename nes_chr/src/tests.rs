use crate::map_chr_rom;
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

    let want: [u8; 64] = [
        0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x03,
        0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x03, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x00, 0x02, 0x02, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00,
        0x00, 0x00, 0x02, 0x00, 0x03, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x03, 0x00, 0x00, 0x00,
        0x00, 0x02, 0x02, 0x02,
    ];

    let tile_print = |data: &[u8; 64]| {
        for y in 0..8 {
            for x in 0..8 {
                let e = data[y * 8 + x];
                if e == 0x00 {
                    print! {"."};
                } else {
                    print!("{e}");
                }
            }
            println!();
        }
    };
    if tiles[0].data != want {
        println!("Tiles differ");
        println!("Want:");
        tile_print(&want);
        println!("\nGot:");
        tile_print(&tiles[0].data);
        panic!();
    }
    Ok(())
}
