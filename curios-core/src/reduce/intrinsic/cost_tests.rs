//! What a fold is charged for what it builds, and the shifts refused before they are built at all.

use {
    super::Reducer,
    crate::{Category, Cost, Intrinsic, Nat, ReduceError, Term},
    curios_num::{Integer, Natural},
    curios_utilities::{Grain, PackedBin},
};

use super::test_support::*;

/// A shift's result is `bits(value) + amount` wide and the amount is a *value*, so no operand size bounds it. The charge is computed from the amount and refused before `num-bigint` is asked for anything — which is the difference between a diagnostic and an allocation the process may not survive.
///
/// The two arms differ only in the shift amount, and the affordable one establishes that the refusal is about size rather than about the operation.
#[test]
fn an_oversized_shift_is_refused_before_it_is_built() {
    let shift = |amount: usize| {
        Term::intrinsic(Intrinsic::NatShl(
            lit(1),
            Term::intrinsic(Intrinsic::Nat(Nat::new(amount))),
        ))
    };

    let mut reducer = Budgeted { remaining: 1_000 };
    assert_eq!(
        reducer.reduce(shift(40)),
        Ok(Term::intrinsic(Intrinsic::Nat(Nat::new(1usize << 40_u32))))
    );

    let mut reducer = Budgeted { remaining: 1_000 };
    assert_eq!(
        reducer.reduce(shift(1 << 30)),
        Err(ReduceError::Exhausted {
            category: Category::Limbs,
            remaining: 1_000,
            // The value's own width plus the amount: `1` is one bit wide, so the charge is a bit past the shift itself.
            attempted: Cost::big_number(1 + (1 << 30)).get(),
        }),
        "the refusal names the row it was refused on, and the size it was refused at"
    );
}

/// The refusal is target-independent, which a shift priced through `usize` would not be: `usize` is 32 bits on wasm32 and 64 natively, so an amount between the two would be folded on one target and left neutral on the other. Any such amount prices at 2^32 bits or more — sixty-seven million units before the value's own width — which no shippable budget affords, so both targets refuse.
///
/// The budget below is a thousand times the shipped default and still refuses, which is the margin that makes "no shippable budget" a claim rather than a hope. **It is also a live regression guard**: with the charge removed, this test does not fail, it *aborts* — `memory allocation of 2305843009213693960 bytes failed`, which is what the fold does when nothing stops it.
#[test]
fn a_shift_amount_past_a_host_index_is_refused_rather_than_folded() {
    let huge = Term::intrinsic(Intrinsic::NatShl(
        lit(1),
        Term::intrinsic(Intrinsic::Nat(Nat::new(Natural::from(u64::MAX)))),
    ));

    let mut reducer = Budgeted {
        remaining: 1_000_000_000,
    };
    assert_eq!(
        reducer.reduce(huge),
        Err(ReduceError::Exhausted {
            category: Category::Limbs,
            remaining: 1_000_000_000,
            attempted: Cost::big_number(u64::MAX).get(),
        })
    );
}

/// Bytes are not the only protected payload. Each subject below builds a result whose logical size its operands decide, and each is charged **at least** that size — so a carrier priced as a constant would show up here as a charge that does not cover what it built.
///
/// At least, rather than exactly: a fold pays for traversing its operands as well as for its result, and the price list is an upper bound rather than an equality. The lower bound is the half that matters, because undercharging is the direction that loses the property.
///
/// The `Bin` subject appends rather than concatenates, deliberately: `FUSION_CAP` stops a concatenation fusing past 64 generators, so past that it builds *nothing* and correctly charges nothing. An append has no cap and rebuilds its whole value, which is the shape whose price has to scale.
#[test]
fn every_payload_carrier_is_charged_for_at_least_what_it_builds() {
    const BITS: usize = 4_096;
    const DIGITS: usize = 1_000;
    const ELEMENTS: usize = 32;

    let appended_bits = Term::intrinsic(Intrinsic::BinAppend {
        grain: Grain::B,
        bin: Term::intrinsic(Intrinsic::Bin(
            Grain::B,
            PackedBin::from_bits((0..BITS).map(|index| index % 2 == 0)),
        )),
        element: Term::intrinsic(Intrinsic::Bool(true)),
    });

    let wide =
        Integer::from(Natural::parse_bytes(&vec![b'9'; DIGITS], 10).expect("a decimal numeral"));
    let product = Term::intrinsic(Intrinsic::IntMul(
        Term::intrinsic(Intrinsic::Int(wide.clone())),
        Term::intrinsic(Intrinsic::Int(wide)),
    ));

    let joined = Term::intrinsic(Intrinsic::ListConcat {
        element: nat_type(),
        operands: vec![run_of(ELEMENTS), run_of(ELEMENTS)],
    });

    // A decimal digit is a little over three and a third bits; rounding down keeps this a lower bound.
    let digit_bits = (DIGITS as u64 * 33) / 10;

    for (carrier, charge, built) in [
        ("bits", charged(appended_bits), BITS as u64 / 64),
        ("integer", charged(product), digit_bits * 2 / 64),
        ("list", charged(joined), ELEMENTS as u64 * 2),
    ] {
        assert!(
            charge >= built,
            "{carrier}: charged {charge} for a result of {built} units"
        );
    }
}

/// A window over a value builds no payload — it takes a reference count on the buffer somebody else owns — so slicing a large run costs about what slicing a small one costs. This is the sharing half of the audit's central distinction, and the half a price list gets wrong by charging for every value it touches rather than every value it builds.
#[test]
fn a_window_charges_for_no_payload_it_did_not_build() {
    let slice = |n: usize| {
        Term::intrinsic(Intrinsic::BinSlice {
            grain: Grain::X,
            bin: Term::intrinsic(Intrinsic::Bin(
                Grain::X,
                PackedBin::from_bytes(vec![7u8; n]),
            )),
            start: Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            length: Term::intrinsic(Intrinsic::Nat(Nat::new(4usize))),
            within: qed(),
        })
    };

    assert_eq!(charged(slice(8)), charged(slice(8_000)));
}
