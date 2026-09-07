#![allow(clippy::unwrap_used)]

use crate::process_lines;

fn lines(s: &str) -> impl Iterator<Item = String> + '_ {
    s.lines().map(str::to_string)
}

#[test]
fn process_lines_writes_a_normal_3_byte_opcode() {
    let block = process_lines(lines("D000 4C 20 21 JMP 2120")).unwrap();
    assert_eq!(block[0xD000], 0x4C);
    assert_eq!(block[0xD001], 0x20);
    assert_eq!(block[0xD002], 0x21);
}

#[test]
fn process_lines_wraps_instead_of_panicking_near_the_top_of_memory() {
    // A 3 byte opcode starting at FFFE must wrap its last operand byte
    // back around to 0x0000 rather than indexing out of bounds.
    let block = process_lines(lines("FFFE 4C 20 21 JMP 2120")).unwrap();
    assert_eq!(block[0xFFFE], 0x4C);
    assert_eq!(block[0xFFFF], 0x20);
    assert_eq!(block[0x0000], 0x21);
}

#[test]
fn process_lines_wraps_a_2_byte_opcode_at_the_very_last_address() {
    let block = process_lines(lines("FFFF A9 42       LDA #42")).unwrap();
    assert_eq!(block[0xFFFF], 0xA9);
    assert_eq!(block[0x0000], 0x42);
}
