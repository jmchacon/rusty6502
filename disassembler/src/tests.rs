#![allow(clippy::unwrap_used)]

use crate::load_bytes;
use rusty6502::prelude::*;

#[test]
fn load_bytes_rejects_a_prg_shorter_than_the_header() {
    let mut ram = FlatRAM::new();
    assert!(load_bytes(&mut ram, vec![], true, 0, 0).is_err());
    assert!(load_bytes(&mut ram, vec![0x01], true, 0, 0).is_err());
}

#[test]
fn load_bytes_handles_an_empty_bin_file_without_panicking() {
    let mut ram = FlatRAM::new();
    let loaded = load_bytes(&mut ram, vec![], false, 0x1000, 0x1000).unwrap();
    assert_eq!(loaded.data_len, 0);
    assert!(loaded.limit.is_none());
}

#[test]
fn load_bytes_handles_a_prg_with_only_the_header_and_no_data() {
    let mut ram = FlatRAM::new();
    let loaded = load_bytes(&mut ram, vec![0x00, 0x08], true, 0, 0).unwrap();
    assert_eq!(loaded.data_len, 0);
    assert!(loaded.limit.is_none());
    assert_eq!(loaded.start, 0x0800);
}

#[test]
fn load_bytes_loads_a_normal_bin_file() {
    let mut ram = FlatRAM::new();
    let loaded = load_bytes(&mut ram, vec![0xA9, 0x42, 0x60], false, 0x1000, 0x1000).unwrap();
    assert_eq!(loaded.pc, 0x1000);
    assert_eq!(loaded.data_len, 3);
    assert_eq!(loaded.limit, Some(0x1002));
    assert_eq!(ram.read(0x1000), 0xA9);
    assert_eq!(ram.read(0x1001), 0x42);
    assert_eq!(ram.read(0x1002), 0x60);
}

#[test]
fn load_bytes_parses_the_prg_load_address_header() {
    let mut ram = FlatRAM::new();
    let loaded = load_bytes(&mut ram, vec![0x00, 0xC0, 0xEA, 0xEA], true, 0, 0).unwrap();
    assert_eq!(loaded.start, 0xC000);
    assert_eq!(loaded.pc, 0xC000);
    assert_eq!(loaded.data_len, 2);
    assert_eq!(ram.read(0xC000), 0xEA);
    assert_eq!(ram.read(0xC001), 0xEA);
}
