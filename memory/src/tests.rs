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
