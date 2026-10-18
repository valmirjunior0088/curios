//! The immediate layout: that the derived quantities are the constants the emitters rely on, and that the packing round-trips.

use super::*;

/// The layout's quantities, stated as literals rather than recomputed, so a change to the derivation is a failure here rather than a silently different program.
#[test]
fn the_derived_quantities_are_the_transcribed_constants() {
    let bytes = ImmediateLayout::of(Grain::X);
    assert_eq!(
        (
            bytes.envelope,
            bytes.len_shift,
            bytes.payload_mask(),
            bytes.slots(),
            bytes.unit(),
            bytes.stride(),
            bytes.elem_mask()
        ),
        (3, 29, 0x00FF_FFFF, 3, 1, 8, 0xFF)
    );
}

/// A packed immediate reads back as the length and bytes it was built from, which is the agreement the emitted `box` helper depends on.
#[test]
fn a_packed_immediate_reads_back_as_its_length_and_bytes() {
    for (grain, length, bytes) in [
        (Grain::X, 0usize, vec![]),
        (Grain::X, 1, vec![0xAB]),
        (Grain::X, 3, vec![0x01, 0x02, 0x03]),
    ] {
        let layout = ImmediateLayout::of(grain);
        let packed = layout.pack(length, &bytes);
        assert_eq!(
            (packed >> layout.len_shift) as usize,
            length,
            "{grain:?} length"
        );
        let payload = packed & layout.payload_mask();
        for (index, &byte) in bytes.iter().enumerate() {
            assert_eq!(
                (payload >> (8 * index)) & 0xFF,
                byte as i32,
                "{grain:?} byte {index}"
            );
        }
        assert!(packed >= 0, "{grain:?} stays inside the i31");
    }
}

/// Three bytes ride an immediate where four do not.
#[test]
fn the_envelope_decides_which_lengths_ride_an_immediate() {
    let bytes = ImmediateLayout::of(Grain::X);
    assert!(bytes.holds(3));
    assert!(!bytes.holds(4));
}
