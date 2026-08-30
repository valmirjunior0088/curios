//! `Nat` bounds and Euclidean split, and the cancellation a symbolic operand admits.

use {
    super::{Comparison, compare_nat, nat_bound, nat_euclid_split},
    crate::{Free, Intrinsic, Peel, Subterm, Term, peel_nat},
    curios_num::Natural,
};

use super::test_support::*;

// Soundness gate: `nat_bound` must never under-report, because the division split and the comparison body both turn a bound into a definitional equation — an under-report there is a false equation, not merely a wrong value. Every closed instantiation of each bounded shape must land at or below the bound the oracle states for the shape itself.
#[test]
fn bound_upper_bounds_every_closed_instantiation() {
    let byte_shape = to_nat_of(symbol(0, "b"));
    let byte_bound = nat_bound(&byte_shape).expect("a Byte carries a bound");
    for byte in [0u8, 1, 17, 128, 254, 255] {
        let value = fold(to_nat_of(Term::intrinsic(Intrinsic::Byte(byte))));
        let value = value
            .as_nat()
            .expect("closed")
            .to_natural()
            .expect("literal");
        assert!(
            value <= byte_bound,
            "Byte/to_nat({byte}) exceeded its bound"
        );
    }

    for divisor in [1u32, 2, 7, 256, 1000] {
        let shape = Term::intrinsic(Intrinsic::NatRem {
            dividend: symbol(0, "x"),
            divisor: lit(divisor),
            non_zero: qed(),
        });
        let bound = nat_bound(&shape).expect("a remainder carries a bound");
        for dividend in [0u32, 1, 5, 255, 999, 100_000] {
            let value = fold(Term::intrinsic(Intrinsic::NatRem {
                dividend: lit(dividend),
                divisor: lit(divisor),
                non_zero: qed(),
            }));
            let value = value
                .as_nat()
                .expect("closed")
                .to_natural()
                .expect("literal");
            assert!(value <= bound, "{dividend} % {divisor} exceeded its bound");
        }
    }
}

// Soundness gate: whatever the split returns must satisfy the Euclidean specification — `n·quotient + remainder` equals the dividend at every instantiation, and the remainder is provably below `n`. Those two together *are* the definition of division, so a split passing both cannot be a false equation whatever its symbolic parts take.
#[test]
fn euclid_split_is_a_euclidean_division() {
    let count = Free::local(0, Some("x"));
    let byte = Free::local(1, Some("b"));
    let x = Term::free_var(&count);
    let digit = to_nat_of(Term::free_var(&byte));

    let cases = [
        (fold(plus(scaled(256, x.clone()), digit.clone())), 256u32),
        (fold(plus(scaled(256, x.clone()), lit(700))), 256),
        (fold(scaled(12, x.clone())), 4),
        (fold(plus(scaled(1024, x.clone()), digit.clone())), 256),
    ];

    for (dividend, divisor) in cases {
        let n = Natural::from(divisor);
        let (quotient, remainder) = nat_euclid_split(&dividend, &n).expect("these dividends split");

        assert!(
            nat_bound(&remainder).expect("a split remainder is bounded") < n,
            "the split remainder was not below {divisor}",
        );

        let rebuilt = plus(scaled(divisor, quotient), remainder);
        for sample in [0u32, 1, 7, 255, 1000] {
            let close = |term: Term| {
                let term = at(term, &count, lit(sample));
                fold(at(term, &byte, Term::intrinsic(Intrinsic::Byte(201))))
            };

            assert_eq!(
                close(rebuilt.clone()),
                close(dividend.clone()),
                "n·quotient + remainder disagreed with the dividend at {divisor}, x = {sample}",
            );
        }
    }
}

// What matching summands *up to universe instances* actually decides, stated over a term rather than in a comment. Two occurrences of one polymorphic name carry independently minted instances, so `Nat::cancel_common` and `compare_nat` both key through `project_erased_universes` to stop a bound mentioning one from stalling forever — and the equation that buys is this one: two summands the ordinary structural comparison would refuse, because it reaches the differing levels, are cancelled against each other and the sums decide equal.
//
// **The premise is that no `Nat` value can depend on a level, and the projection is unsound the moment that stops holding.** The same projection, read as a quotient by definitional equality, is what let the certifier's refinement key certify a coercion between distinct types, because a *type* can depend on a level: `Type u` embeds one in a term, so `wrap(Type u)` is genuinely two values at two instances (`curios-cert`'s `recheck::tests::a_case_equation_does_not_refine_an_occurrence_at_another_universe_instance`). What stops the same argument here is narrower than the comment beside the code claims: not that instances are erased before anything runs, but that Core offers no elimination from a type or a level into a `Nat`, so the two summands below denote one number however they are spelled. Adding one — any intrinsic reading a level or a sort as a count — makes this fixture a witness rather than a record.
//
// The control is the second pair, which shares that shape in every respect except the one that matters: distinct *arguments* under one instance must not cancel, or the key would be collapsing terms wholesale rather than levels.
#[test]
fn summands_cancel_across_a_universe_instance_and_not_across_an_argument() {
    let instanced = |level: u32| {
        Term::instance(
            crate::InstanceHead::Var(crate::Var::free(Free::local(0, Some("g")))),
            vec![crate::Level::constant(level)],
        )
    };

    let peel = peel_nat(
        &as_nat(&fold(plus(instanced(0), lit(1)))),
        &as_nat(&fold(plus(instanced(1), lit(1)))),
    );

    assert!(
        matches!(peel, Peel::Equal),
        "`g<0> + 1` and `g<1> + 1` differ only in a level, which is not part of a number",
    );

    let applied = |argument: Term| Term::apply(instanced(0), vec![argument]);

    let peel = peel_nat(
        &as_nat(&fold(plus(applied(symbol(1, "x")), lit(1)))),
        &as_nat(&fold(plus(applied(symbol(2, "y")), lit(1)))),
    );

    assert!(
        matches!(peel, Peel::Continue(..)),
        "`g<0>(x) + 1` and `g<0>(y) + 1` are undecided, not one number",
    );
}

// The rule the base-256 encodings need: a digit whose carrier bounds it below the divisor cannot carry, so the scaled symbol divides out exactly and the digit is the whole remainder.
#[test]
fn a_bounded_digit_divides_out_of_a_scaled_symbol() {
    let x = symbol(0, "x");
    let digit = to_nat_of(symbol(1, "b"));
    let dividend = fold(plus(scaled(256, x.clone()), digit.clone()));

    assert_eq!(
        fold(Term::intrinsic(Intrinsic::NatDiv {
            dividend: dividend.clone(),
            divisor: lit(256),
            non_zero: qed(),
        })),
        x,
    );
    assert_eq!(
        fold(Term::intrinsic(Intrinsic::NatRem {
            dividend,
            divisor: lit(256),
            non_zero: qed(),
        })),
        digit,
    );
}

// The refusals that keep the rule sound: a coefficient the divisor does not divide could carry, and an unbounded summand could be anything at all. Both must stay neutral rather than fold.
#[test]
fn an_uncertain_summand_leaves_the_division_neutral() {
    let x = symbol(0, "x");
    let unbounded = plus(scaled(256, x.clone()), symbol(1, "y"));
    let indivisible = plus(scaled(100, x.clone()), to_nat_of(symbol(1, "b")));

    for dividend in [fold(unbounded), fold(indivisible)] {
        let divided = fold(Term::intrinsic(Intrinsic::NatDiv {
            dividend: dividend.clone(),
            divisor: lit(256),
            non_zero: qed(),
        }));
        assert!(
            matches!(&*divided, Subterm::Intrinsic(Intrinsic::NatDiv { .. })),
            "a division that is not forced folded anyway: {divided:?}",
        );
    }
}

// A bounded operand decides a comparison the floors cannot: `x % n` is a stuck remainder the structural body has nothing to say about, yet it is below `n` for every `x`.
#[test]
fn a_bounded_operand_decides_a_comparison_against_a_literal() {
    let mut reducer = Folding;
    let remainder = Term::intrinsic(Intrinsic::NatRem {
        dividend: symbol(0, "x"),
        divisor: lit(256),
        non_zero: qed(),
    });

    assert_eq!(
        compare_nat(&mut reducer, remainder.clone(), lit(256))
            .expect("reduces")
            .0,
        Comparison::Lt,
    );
    assert_eq!(
        compare_nat(&mut reducer, remainder, lit(200))
            .expect("reduces")
            .0,
        Comparison::Stuck,
    );
}
