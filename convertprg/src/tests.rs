#![allow(clippy::unwrap_used)]

use crate::convert_prg;

#[test]
fn convert_prg_writes_data_at_the_parsed_load_address() {
    // Load address 0x1234 (little endian header) followed by 4 data bytes.
    let bytes = [0x34, 0x12, 0xAA, 0xBB, 0xCC, 0xDD];
    let block = convert_prg(&bytes, 0x0000).unwrap();
    assert_eq!(block[0x1234], 0xAA);
    assert_eq!(block[0x1235], 0xBB);
    assert_eq!(block[0x1236], 0xCC);
    assert_eq!(block[0x1237], 0xDD);
}

#[test]
fn convert_prg_rejects_input_shorter_than_the_header() {
    assert!(convert_prg(&[], 0x0000).is_err());
    assert!(convert_prg(&[0x01], 0x0000).is_err());
}

#[test]
fn convert_prg_handles_a_header_only_prg_with_no_data() {
    // Just the 2 byte load address header, no program bytes - must not panic.
    let block = convert_prg(&[0x00, 0x08], 0x0000).unwrap();
    assert_eq!(block[0x0800], 0x00);
}

#[test]
fn convert_prg_truncates_data_that_would_overflow_64k() {
    // Load address 0xFFF0, with more data than the remaining 16 bytes of
    // address space can hold - without truncation this would try to index
    // past the end of the 64k block and panic.
    let mut bytes = vec![0xF0, 0xFF]; // addr = 0xFFF0
    bytes.extend(std::iter::repeat_n(0xAA, 64));
    let block = convert_prg(&bytes, 0x0000).unwrap();
    assert_eq!(block[0xFFF0], 0xAA);
}
