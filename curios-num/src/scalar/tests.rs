use super::{flt_max, flt_min};

/// Wasm's `f32.min`/`f32.max` on an equal pair answer by sign: `-0.0` is the minimum and `+0.0` the maximum of the two zeros whichever order they arrive in. Compared as bit patterns, because IEEE equality is what makes the zeros indistinguishable in the first place.
#[test]
fn an_equal_pair_answers_by_sign() {
    let bits = |value: Option<f32>| value.map(f32::to_bits);
    for (left, right) in [(-0.0f32, 0.0f32), (0.0, -0.0)] {
        assert_eq!(bits(flt_min(left, right)), Some((-0.0f32).to_bits()));
        assert_eq!(bits(flt_max(left, right)), Some(0.0f32.to_bits()));
    }
    assert_eq!(bits(flt_min(-0.0, -0.0)), Some((-0.0f32).to_bits()));
    assert_eq!(bits(flt_max(0.0, 0.0)), Some(0.0f32.to_bits()));
    assert_eq!(flt_min(1.5, 2.5), Some(1.5));
    assert_eq!(flt_max(1.5, 2.5), Some(2.5));
}

/// A NaN operand declines rather than answering: Rust's `min` drops the NaN, Wasm's propagates it, so no fold may decide.
#[test]
fn a_nan_operand_declines() {
    assert_eq!(flt_min(f32::NAN, 1.0), None);
    assert_eq!(flt_min(1.0, f32::NAN), None);
    assert_eq!(flt_max(f32::NAN, 1.0), None);
    assert_eq!(flt_max(1.0, f32::NAN), None);
}
