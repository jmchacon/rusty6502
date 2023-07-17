use std::{error::Error, fmt::Write, fs::read, path::Path};

use crate::{Memory, MAX_SIZE};

#[test]
fn array_memory() {
    let mut r: [u8; MAX_SIZE] = [0; MAX_SIZE];

    r.power_on();
    assert!(r.read(0x1234) == 0x00, "Bad value");
    r.write(0x1234, 0xAE);
    assert!(r.read(0x1234) == 0xAE, "Bad value");

    let mut t = [0; MAX_SIZE];
    r.ram(&mut t);
    assert!(t[0x1234] == 0xAE, "Bad ram() value");
}

#[test]
fn dump_memory() -> Result<(), Box<dyn Error>> {
    // Get the input ROM and poke it into place.
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../testdata/bcd_test.bin");
    let bytes = read(path)?;

    let mut r: [u8; MAX_SIZE] = [0; MAX_SIZE];

    for (addr, b) in bytes.iter().enumerate() {
        r.write(u16::try_from(addr)?, *b);
    }

    let golden = r"00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
*
0000c000  a0 01 84 00 a9 00 85 01  85 02 a5 02 29 0f 85 03  |............)...|
0000c010  a5 02 29 f0 85 04 09 0f  85 05 a5 01 29 0f 85 06  |..).........)...|
0000c020  a5 01 29 f0 85 07 20 4d  c0 20 0b c1 20 e6 c0 d0  |..)... M. .. ...|
0000c030  1a 20 91 c0 20 14 c1 20  e6 c0 d0 0f e6 01 d0 da  |. .. .. ........|
0000c040  e6 02 d0 c6 88 10 c3 a9  00 85 00 f0 fe f8 c0 01  |................|
0000c050  a5 01 65 02 85 08 08 68  85 09 d8 c0 01 a5 01 65  |..e....h.......e|
0000c060  02 85 0a 08 68 85 0b c0  01 a5 06 65 03 c9 0a a2  |....h......e....|
0000c070  00 90 06 e8 69 05 29 0f  38 05 07 75 04 08 b0 04  |....i.).8..u....|
0000c080  c9 a0 90 03 69 5f 38 85  0c 08 68 85 0d 68 85 0e  |....i_8...h..h..|
0000c090  60 f8 c0 01 a5 01 e5 02  85 08 08 68 85 09 d8 c0  |`..........h....|
0000c0a0  01 a5 01 e5 02 85 0a 08  68 85 0b 60 c0 01 a5 06  |........h..`....|
0000c0b0  e5 03 a2 00 b0 06 e8 e9  05 29 0f 18 05 07 f5 04  |.........)......|
0000c0c0  b0 02 e9 5f 85 0c 60 c0  01 a5 06 e5 03 a2 00 b0  |..._..`.........|
0000c0d0  04 e8 29 0f 18 05 07 f5  04 b0 02 e9 5f e0 00 f0  |..)........._...|
0000c0e0  02 e9 06 85 0c 60 a5 08  c5 0c d0 1e a5 09 45 0f  |.....`........E.|
0000c0f0  29 80 d0 16 a5 09 45 0e  29 40 d0 0e a5 09 45 10  |).....E.)@....E.|
0000c100  29 02 d0 06 a5 09 45 0d  29 01 60 a5 0e 85 0f a5  |).....E.).`.....|
0000c110  0b 85 10 60 20 ac c0 a5  0b 85 0f 85 0e 85 10 85  |...` ...........|
0000c120  0d 60 a5 0c 08 68 85 0f  85 10 60 20 c7 c0 a5 0c  |.`...h....` ....|
0000c130  08 68 85 0f 85 10 a5 0b  85 0e 85 0d 60 a5 0c 08  |.h..........`...|
0000c140  68 85 0f 85 10 60 20 ac  c0 a5 0c 08 68 85 0f 85  |h....` .....h...|
0000c150  10 a5 0b 85 0e 85 0d 60  00 00 00 00 00 00 00 00  |.......`........|
0000c160  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
*
";
    let mut out = String::new();
    write!(out, "{}", &r as &dyn Memory)?;
    assert!(golden == out, "Strings differ. Got:\n{out}Want:\n{golden}");
    Ok(())
}
