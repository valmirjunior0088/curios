//! `flt_min`/`flt_max` were here, pinning that an equal pair answers by sign and that a NaN operand declines the fold. Neither is this module's to state any more: the model answers both — 754-2019's `minimum`/`maximum`, NaN propagated — and `Floating`'s own edge grid checks them against the host over every pair of the IEEE corners. What is left of `Flt` here is the *carrier* narrowing, which has no oracle but the carrier's width.

use super::{ScalarTrap, int_shl, nat_shl};

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
