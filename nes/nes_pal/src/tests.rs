use color_eyre::Result;
use std::{fs::File, io::Read, path::Path};

use crate::{parse_pal, Color, ENTRIES};

#[test]
fn invalid_pal() {
    let data: [u8; 6] = [0, 1, 2, 3, 4, 5];

    let res = parse_pal(&data);
    assert!(res.is_err(), "result isn't error? {res:?}");
}

#[test]
fn load_ntsc() -> Result<()> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../testdata/NTSC.pal");
    let mut file = File::open(path)?;

    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    let colors = parse_pal(&data)?;

    assert!(
        colors.len() == ENTRIES,
        "Didn't find enough entries in colors. Got {} and expected {}",
        colors.len(),
        ENTRIES
    );

    let test = Color {
        r: 0x24,
        g: 0x04,
        b: 0xC8,
    };
    assert!(
        test == colors[2],
        "Colors don't match. Expected {test:?} and got {:?}",
        colors[2]
    );
    Ok(())
}
