//! The immediate layout: that the derived quantities are the constants the emitters used to spell by hand, and that the packing round-trips.

use super::*;

/// The values every site transcribed before this module owned them. Stated as literals rather than recomputed, so a change to the derivation is a failure here rather than a silently different program.
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

    let bits = ImmediateLayout::of(Grain::B);
    assert_eq!(
        (
            bits.envelope,
            bits.len_shift,
            bits.payload_mask(),
            bits.slots(),
            bits.unit(),
            bits.stride(),
            bits.elem_mask()
        ),
        (26, 26, 0x03FF_FFFF, 4, 8, 1, 1)
    );
}

/// A packed immediate reads back as the length and bytes it was built from, which is the agreement the emitted `box` helper depends on.
#[test]
fn a_packed_immediate_reads_back_as_its_length_and_bytes() {
    for (grain, length, bytes) in [
        (Grain::X, 0usize, vec![]),
        (Grain::X, 1, vec![0xAB]),
        (Grain::X, 3, vec![0x01, 0x02, 0x03]),
        (Grain::B, 1, vec![0x01]),
        (Grain::B, 26, vec![0xFF, 0xFF, 0xFF, 0x03]),
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

/// The envelope is a length question, not a byte-count one: three bytes ride an immediate where four do not, and twenty-six bits where twenty-seven do not.
#[test]
fn the_envelope_decides_which_lengths_ride_an_immediate() {
    let bytes = ImmediateLayout::of(Grain::X);
    assert!(bytes.holds(3));
    assert!(!bytes.holds(4));

    let bits = ImmediateLayout::of(Grain::B);
    assert!(bits.holds(26));
    assert!(!bits.holds(27));
}
