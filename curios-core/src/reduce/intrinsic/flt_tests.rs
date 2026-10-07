//! The `Flt` folds that read more than two literals: a difference spelled as the sum it is, a rounding of a rounding, the byte round trip, and a composition of sign operations written in its one form, each held to `curios_num::Floating` over bit patterns.

use {
    super::test_support::*,
    crate::{Free, Intrinsic, Term},
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

fn neg(operand: Term) -> Term {
    Term::intrinsic(Intrinsic::FltNeg(operand))
}

fn abs(operand: Term) -> Term {
    Term::intrinsic(Intrinsic::FltAbs(operand))
}

fn copysign(magnitude: Term, sign: Term) -> Term {
    Term::intrinsic(Intrinsic::FltCopysign(magnitude, sign))
}

// A composition of the three sign operations is written one way: a value under its own sign is the value, under its own sign flipped its negation, under a clear or a set bit its absolute value or that negated, and under another value's sign a `copysign` whose magnitude carries no sign operation and whose sign carries at most a negation. A literal is its absolute value under the bit it has.
//
// Mutation-checked: a literal read under the opposite bit writes `copysign(a, 1.0)` as `neg(abs(a))`.
#[test]
fn a_composition_of_sign_operations_is_written_one_way() {
    let (a, b, c) = (sym(0, "a"), sym(1, "b"), sym(2, "c"));
    let (two, minus_two) = (flt(0x4000_0000_0000_0000), flt(0xC000_0000_0000_0000));
    let minus_zero = flt(0x8000_0000_0000_0000);

    for (composed, form) in [
        (neg(neg(a.clone())), a.clone()),
        (abs(abs(a.clone())), abs(a.clone())),
        (abs(neg(a.clone())), abs(a.clone())),
        (neg(abs(neg(a.clone()))), neg(abs(a.clone()))),
        (
            copysign(copysign(a.clone(), b.clone()), c.clone()),
            copysign(a.clone(), c.clone()),
        ),
        (
            neg(copysign(a.clone(), b.clone())),
            copysign(a.clone(), neg(b.clone())),
        ),
        (
            neg(copysign(a.clone(), neg(b.clone()))),
            copysign(a.clone(), b.clone()),
        ),
        (abs(copysign(a.clone(), b.clone())), abs(a.clone())),
        (copysign(a.clone(), a.clone()), a.clone()),
        (copysign(neg(a.clone()), a.clone()), a.clone()),
        (copysign(a.clone(), neg(a.clone())), neg(a.clone())),
        (
            copysign(abs(a.clone()), b.clone()),
            copysign(a.clone(), b.clone()),
        ),
        (
            copysign(neg(a.clone()), b.clone()),
            copysign(a.clone(), b.clone()),
        ),
        (copysign(a.clone(), abs(b.clone())), abs(a.clone())),
        (
            copysign(a.clone(), neg(abs(b.clone()))),
            neg(abs(a.clone())),
        ),
        (
            copysign(a.clone(), copysign(b.clone(), c.clone())),
            copysign(a.clone(), c.clone()),
        ),
        (copysign(a.clone(), two.clone()), abs(a.clone())),
        (copysign(a.clone(), minus_zero), neg(abs(a.clone()))),
        (
            copysign(minus_two.clone(), b.clone()),
            copysign(two.clone(), b.clone()),
        ),
        (
            neg(copysign(minus_two, b.clone())),
            copysign(two, neg(b.clone())),
        ),
    ] {
        assert_eq!(fold(composed.clone()), form, "{composed:?}");
    }
}

// The form a composition folds to is the composition at every pattern: with a literal for one operand and a symbol for the other, the form taken at a pattern for the symbol folds to what the composition does there, so a literal is read under the bit it has and a NaN keeps its payload under either sign.
#[test]
fn a_sign_form_over_a_literal_is_the_composition_at_every_pattern() {
    let binder = Free::local(0, Some("a"));
    let a = Term::free_var(&binder);

    for bits in PATTERNS {
        for composed in [
            copysign(a.clone(), flt(bits)),
            copysign(flt(bits), a.clone()),
            neg(copysign(flt(bits), a.clone())),
            copysign(flt(bits), neg(a.clone())),
            abs(copysign(a.clone(), flt(bits))),
            copysign(neg(a.clone()), copysign(flt(bits), a.clone())),
            copysign(a.clone(), neg(copysign(a.clone(), flt(bits)))),
        ] {
            let form = fold(composed.clone());
            for value in PATTERNS {
                assert_eq!(
                    fold(at(form.clone(), &binder, flt(value))),
                    fold(at(composed.clone(), &binder, flt(value))),
                    "{composed:?} at {value:#018x}"
                );
            }
        }
    }
}
