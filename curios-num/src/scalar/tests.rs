use super::{ScalarTrap, flt_max, flt_min, int_shl, nat_shl};

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

/// A left shift refuses every product past its carrier, at every count.
///
/// Widening the intermediate to `u64` is not the condition on its own: a large enough count pushes the value past *its* top too, and `2^30 << 40` — `2^70`, whose low sixty-four bits are zero — came back as a representable `Ok(0)`. `curios-core`'s unbounded `Natural` is the oracle, so the only permitted answers are the exact product or a refusal, and a folded `0` where the backend traps is a third one.
#[test]
fn a_left_shift_refuses_every_product_past_the_carrier() {
    for (value, shift) in [
        (1_u32 << 30, 40_u32),
        (1 << 30, 34),
        (1 << 30, 33),
        (2, 63),
        (1 << 20, 44),
    ] {
        assert_eq!(
            nat_shl(value, shift),
            Err(ScalarTrap::Overflow),
            "nat_shl({value}, {shift})"
        );
    }
    for (value, shift) in [(1_i32 << 29, 35_i32), (-(1 << 29), 35), (1 << 29, 40)] {
        assert_eq!(
            int_shl(value, shift),
            Some(Err(ScalarTrap::Overflow)),
            "int_shl({value}, {shift})"
        );
    }

    // The clamp decides only what had already left the carrier: zero is zero at every count, an in-range product still folds, and a negative count is still the theory's silence.
    assert_eq!(nat_shl(0, 40), Ok(0));
    assert_eq!(nat_shl(3, 29), Ok(3 << 29));
    assert_eq!(int_shl(0, 40), Some(Ok(0)));
    assert_eq!(int_shl(-1, 31), Some(Ok(i32::MIN)));
    assert_eq!(int_shl(1, -1), None);
}
