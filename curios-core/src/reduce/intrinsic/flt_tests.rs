//! The `Flt` folds that read more than two literals: a difference spelled as the sum it is, a rounding of a rounding, and the byte round trip, each held to `curios_num::Floating` over bit patterns.

use {
    super::test_support::*,
    crate::{Intrinsic, Term},
    curios_num::{Floating, Rounding},
};

const DIRECTIONS: [Rounding; 5] = [
    Rounding::TiesToEven,
    Rounding::TiesToAway,
    Rounding::TowardZero,
    Rounding::TowardPositive,
    Rounding::TowardNegative,
];

/// Both zeros, both infinities, a subnormal, three ordinary values with fractions, the greatest finite value, and a quiet and a signaling NaN with a payload of each sign.
const PATTERNS: [u64; 12] = [
    0x0000_0000_0000_0000,
    0x8000_0000_0000_0000,
    0x7FF0_0000_0000_0000,
    0xFFF0_0000_0000_0000,
    0x0000_0000_0000_0001,
    0x3FE0_0000_0000_0000,
    0xBFF8_0000_0000_0000,
    0x4004_0000_0000_0000,
    0x7FEF_FFFF_FFFF_FFFF,
    0x7FF8_0000_0000_0001,
    0xFFF8_0000_0000_0123,
    0xFFF4_0000_0000_0000,
];

fn flt(bits: u64) -> Term {
    Term::intrinsic(Intrinsic::Flt(Floating::from_bits(bits)))
}

// A stuck difference is the sum with its subtrahend negated, in the direction it was written in, and a closed one folds to what that sum folds to at every pair of patterns, a NaN's sign and payload included: the model's rule, which is what lets the fold spell one as the other.
#[test]
fn a_difference_is_the_sum_with_its_subtrahend_negated() {
    let (a, b) = (sym(0, "a"), sym(1, "b"));
    for rounding in DIRECTIONS {
        assert_eq!(
            fold(Term::intrinsic(Intrinsic::FltSub(
                rounding,
                a.clone(),
                b.clone()
            ))),
            Term::intrinsic(Intrinsic::FltAdd(
                rounding,
                a.clone(),
                Term::intrinsic(Intrinsic::FltNeg(b.clone()))
            )),
        );
        for left in PATTERNS {
            for right in PATTERNS {
                let difference = fold(Term::intrinsic(Intrinsic::FltSub(
                    rounding,
                    flt(left),
                    flt(right),
                )));
                let sum = fold(Term::intrinsic(Intrinsic::FltAdd(
                    rounding,
                    flt(left),
                    Term::intrinsic(Intrinsic::FltNeg(flt(right))),
                )));
                assert_eq!(
                    difference, sum,
                    "{left:#018x} - {right:#018x} under {rounding:?}"
                );
            }
        }
    }
}

// A rounding to an integral value is fixed by every direction: at every pattern, rounding in one direction and then in another is the first rounding, a signaling NaN quieted once. So a rounding of a stuck rounding is the inner one, and a rounding of anything else stays.
#[test]
fn a_rounding_to_an_integral_value_is_fixed_by_every_direction() {
    let a = sym(0, "a");
    let rounded = |rounding: Rounding, operand: Term| {
        Term::intrinsic(Intrinsic::FltRoundIntegral(rounding, operand))
    };
    for inner in DIRECTIONS {
        for outer in DIRECTIONS {
            assert_eq!(
                fold(rounded(outer, rounded(inner, a.clone()))),
                rounded(inner, a.clone()),
            );
            for bits in PATTERNS {
                assert_eq!(
                    fold(rounded(outer, rounded(inner, flt(bits)))),
                    fold(rounded(inner, flt(bits))),
                    "{bits:#018x} under {inner:?} then {outer:?}"
                );
            }
        }
    }
    assert_eq!(
        fold(rounded(Rounding::TowardZero, a.clone())),
        rounded(Rounding::TowardZero, a),
    );
}

// Encoding what a decoding read is the run it was handed, as decoding what an encoding wrote is the float: every pattern is a value and no NaN is canonicalized, so the two conversions undo each other from both sides, on a symbol as on every pattern.
#[test]
fn the_byte_round_trip_reduces_from_both_sides() {
    let (run, float) = (sym(0, "b"), sym(1, "f"));
    let decoded = |bin: Term| {
        Term::intrinsic(Intrinsic::FltOfLeBytes {
            bin,
            eight_bytes: qed(),
        })
    };
    let encoded = |float: Term| Term::intrinsic(Intrinsic::FltToLeBytes(float));

    assert_eq!(fold(encoded(decoded(run.clone()))), run);
    assert_eq!(fold(decoded(encoded(float.clone()))), float);
    for bits in PATTERNS {
        let bytes = run_bytes(&bits.to_le_bytes());
        assert_eq!(fold(encoded(decoded(bytes.clone()))), bytes);
        assert_eq!(fold(decoded(encoded(flt(bits)))), flt(bits));
    }
}
