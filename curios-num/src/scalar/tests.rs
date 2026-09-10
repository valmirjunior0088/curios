//! `flt_min`/`flt_max` were here, pinning that an equal pair answers by sign and that a NaN operand declines the fold. Neither is this module's to state any more: the model answers both — 754-2019's `minimum`/`maximum`, NaN propagated — and `Floating`'s own edge grid checks them against the host over every pair of the IEEE corners. What is left is the *allowance*, which has no oracle but the caller's own resources.

use {
    super::{int_mul, int_shl, nat_mul, nat_shl},
    crate::{Integer, Natural},
};

fn nat(value: u64) -> Natural {
    Natural::from(value)
}

fn int(value: i64) -> Integer {
    Integer::from(value)
}

/// `2^power`, for stating an expected result whose magnitude no literal spells.
fn pow2(power: u64) -> Natural {
    Natural::from(1u64)
        .checked_shl(Natural::from(power))
        .expect("a power the host holds")
}

/// A growing operation answers the exact value inside its allowance and declines outside it — never a truncation, and never an allocation the caller did not sanction.
///
/// The allowance is stated in *result* bits rather than operand bits, because that is the quantity the caller is protecting: `2^30 << 40` is `2^70`, which two thirty-bit operands do not predict.
#[test]
fn a_growing_operation_declines_past_its_allowance() {
    assert_eq!(nat_shl(&nat(1 << 30), &nat(40), 128), Some(pow2(70)));
    assert_eq!(nat_shl(&nat(1 << 30), &nat(40), 64), None);
    assert_eq!(
        int_shl(&int(-(1 << 29)), &nat(35), 128),
        Some(-Integer::from(pow2(64)))
    );
    assert_eq!(int_shl(&int(-(1 << 29)), &nat(35), 32), None);

    assert_eq!(nat_mul(&nat(1 << 40), &nat(1 << 40), 128), Some(pow2(80)));
    assert_eq!(nat_mul(&nat(1 << 40), &nat(1 << 40), 64), None);
    assert_eq!(
        int_mul(&int(1 << 40), &int(-(1 << 40)), 128),
        Some(-Integer::from(pow2(80)))
    );
    assert_eq!(int_mul(&int(1 << 40), &int(-(1 << 40)), 64), None);
}

/// Zero is answered before the count is consulted, so an allowance never leaves a fold undone for a value that could not grow.
#[test]
fn a_shift_of_zero_folds_under_any_allowance() {
    assert_eq!(nat_shl(&Natural::zero(), &nat(1 << 40), 0), Some(nat(0)));
    assert_eq!(int_shl(&int(0), &nat(1 << 40), 0), Some(int(0)));
}

/// A count no machine word holds declines rather than being reduced modulo anything: the theory's answer is a numeral with that many bits, and refusing to build it is the only answer that is not a different number.
#[test]
fn a_count_past_a_machine_word_declines() {
    let huge = pow2(70);

    assert_eq!(nat_shl(&nat(1), &huge, u64::MAX), None);
    assert_eq!(int_shl(&int(1), &huge, u64::MAX), None);
}
