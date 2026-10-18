//! The immediate layout: that the derived quantities are the constants the emitters rely on, and that the packing round-trips.

use super::*;

/// The layout's quantities, stated as literals rather than recomputed, so a change to the derivation is a failure here rather than a silently different program.
#[test]
fn the_derived_quantities_are_the_transcribed_constants() {
    let bytes = ImmediateLayout::new();
    assert_eq!(
        (
            bytes.envelope,
            bytes.len_shift,
            bytes.payload_mask(),
            bytes.slots(),
            ImmediateLayout::STRIDE,
            ImmediateLayout::ELEM_MASK
        ),
        (3, 29, 0x00FF_FFFF, 3, 8, 0xFF)
    );
}

/// A packed immediate reads back as the length and bytes it was built from, which is the agreement the emitted `box` helper depends on.
#[test]
fn a_packed_immediate_reads_back_as_its_length_and_bytes() {
    for (length, bytes) in [
        (0usize, vec![]),
        (1, vec![0xAB]),
        (3, vec![0x01, 0x02, 0x03]),
    ] {
        let layout = ImmediateLayout::new();
        let packed = layout.pack(length, &bytes);
        assert_eq!((packed >> layout.len_shift) as usize, length, "length");
        let payload = packed & layout.payload_mask();
        for (index, &byte) in bytes.iter().enumerate() {
            assert_eq!((payload >> (8 * index)) & 0xFF, byte as i32, "byte {index}");
        }
        assert!(packed >= 0, "the immediate stays inside the i31");
    }
}

/// Three bytes ride an immediate where four do not.
#[test]
fn the_envelope_decides_which_lengths_ride_an_immediate() {
    let bytes = ImmediateLayout::new();
    assert!(bytes.holds(3));
    assert!(!bytes.holds(4));
}
