//! What a fold is charged for what it builds, and the shifts refused before they are built at all.

use {
    super::{
        Footprint, Reducer, image_bound, operand_bound, product_bound, sum_bound, test_support::*,
    },
    crate::{Category, Cost, Intrinsic, Nat, ReduceError, Term, int_normalize},
    curios_num::{Binary, Grain, Integer, Natural},
};

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

/// A literal count under a *symbolic* value builds the coefficient `2ᵏ` and nothing else, so that is what it is charged for, before it is built: an affordable count lands on the product, and one past the budget is refused on the same row and at the same size as the closed fold's.
#[test]
fn a_shift_coefficient_is_refused_before_it_is_built() {
    let shift = |amount: usize| {
        Term::intrinsic(Intrinsic::NatShl(
            sym(0, "x"),
            Term::intrinsic(Intrinsic::Nat(Nat::new(amount))),
        ))
    };

    let mut reducer = Budgeted { remaining: 1_000 };
    assert_eq!(
        reducer.reduce(shift(40)),
        Ok(Term::intrinsic(Intrinsic::nat_mul(
            Term::intrinsic(Intrinsic::Nat(Nat::new(1usize << 40_u32))),
            sym(0, "x"),
        )))
    );

    let mut reducer = Budgeted { remaining: 1_000 };
    assert_eq!(
        reducer.reduce(shift(1 << 30)),
        Err(ReduceError::Exhausted {
            category: Category::Limbs,
            remaining: 1_000,
            attempted: Cost::big_number(1 + (1 << 30)).get(),
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
            Binary::from_bits((0..BITS).map(|index| index % 2 == 0)),
        )),
        element: Term::intrinsic(Intrinsic::Bool(true)),
    });

    let digits = Natural::parse_bytes(&vec![b'9'; DIGITS], 10).expect("a decimal numeral");
    let natural = || Term::intrinsic(Intrinsic::Nat(Nat::new(digits.clone())));
    let sum = add(natural(), natural());

    let wide = Integer::from(digits.clone());
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
        ("natural", charged(sum), digit_bits / 64),
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
            bin: Term::intrinsic(Intrinsic::Bin(Grain::X, Binary::from_bytes(vec![7u8; n]))),
            start: Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            length: Term::intrinsic(Intrinsic::Nat(Nat::new(4usize))),
            within: qed(),
        })
    };

    assert_eq!(charged(slice(8)), charged(slice(8_000)));
}

/// A sum is written back whole where it is added to, so an addition is charged for the sum it builds and not for the summand it adds: over what its two operands cost to reduce, the fold pays [`sum_bound`] of every summand they hold, on each carrier, and a budget one unit short of that refuses it before anything is built.
///
/// Mutation-checked: with the charge taken out of either fold, its row costs nothing over its operands.
#[test]
fn a_sum_is_charged_for_every_summand_it_writes_back() {
    const SUMMANDS: u32 = 64;

    type Join = fn(Term, Term) -> Term;
    let chain = |join: Join, from: u32| {
        (from..from + SUMMANDS)
            .map(|index| sym(index, "x"))
            .reduce(join)
            .expect("a sum of at least one summand")
    };
    let int_add: Join = |left, right| Term::intrinsic(Intrinsic::IntAdd(left, right));

    for (carrier, join) in [("nat", add as Join), ("int", int_add)] {
        let (left, right) = (chain(join, 0), chain(join, SUMMANDS));
        let operands = charged(left.clone()) + charged(right.clone());
        let bound = sum_bound(2 * SUMMANDS as usize).get();

        assert_eq!(
            charged(join(left.clone(), right.clone())) - operands,
            bound,
            "{carrier}: a sum of two sums is charged for the summands of both"
        );

        let mut reducer = Budgeted {
            remaining: operands + bound - 1,
        };
        assert!(
            matches!(
                reducer.reduce(join(left, right)),
                Err(ReduceError::Exhausted {
                    category: Category::Reconstruction,
                    ..
                })
            ),
            "{carrier}: a budget short of the sum refuses it"
        );
    }
}

/// A subtraction and a comparison write back two residuals where a summand cancels, and are charged for those: two sums that share one summand cost each fold [`sum_bound`] of what is left of both over what the same two sums cost it sharing nothing, which are handed back as they stand.
///
/// Mutation-checked: with the charge taken out of a cancellation, its row costs what the pair sharing nothing does.
#[test]
fn a_cancellation_is_charged_for_the_residuals_it_writes_back() {
    const SUMMANDS: u32 = 32;

    type Join = fn(Term, Term) -> Term;
    let sum_over = |join: Join, from: u32, first: Term| {
        (from..from + SUMMANDS)
            .map(|index| sym(index, "x"))
            .fold(first, join)
    };
    let int_add: Join = |left, right| Term::intrinsic(Intrinsic::IntAdd(left, right));
    let nat_sub: Join = |left, right| Term::intrinsic(Intrinsic::NatSub(left, right));
    let nat_lt: Join = |left, right| Term::intrinsic(Intrinsic::NatLt(left, right));
    let int_lt: Join = |left, right| Term::intrinsic(Intrinsic::IntLt(left, right));
    let int_sub: Join = |left, right| Term::intrinsic(Intrinsic::IntSub(left, right));
    let (shared, other) = (sym(1_000, "s"), sym(1_001, "t"));
    // The `Int` comparison goes on to split what is left by sign and read each side's preimage: a summand fewer a side where the shared one cancelled.
    let preimages = |each: u32| {
        let side = Footprint {
            summands: each as usize,
            factors: each as usize,
            bits: u64::from(each),
        };

        sum_bound(2 * each as usize).get() + 2 * image_bound(&side).get()
    };
    let narrower = preimages(SUMMANDS + 1) - preimages(SUMMANDS);

    for (fold, join, over, saved) in [
        ("Nat/sub", add as Join, nat_sub, 0),
        ("Nat/lt", add as Join, nat_lt, 0),
        ("Int/lt", int_add, int_lt, narrower),
    ] {
        let left = sum_over(join, 0, shared.clone());
        let cancelling = over(left.clone(), sum_over(join, SUMMANDS, shared.clone()));
        let standing = over(left, sum_over(join, SUMMANDS, other.clone()));

        assert_eq!(
            charged(cancelling) + saved - charged(standing),
            sum_bound(2 * SUMMANDS as usize).get(),
            "{fold}: two residuals are charged for the summands of both"
        );
    }

    // A difference of two `Int` sums is one sum over the negated subtrahend: charged for both, whatever cancels.
    let left = sum_over(int_add, 0, shared);
    let right = sum_over(int_add, SUMMANDS, other);
    let operands = charged(left.clone()) + charged(right.clone());
    let each = SUMMANDS as usize + 1;

    assert_eq!(
        charged(int_sub(left, right)) - operands,
        sum_bound(each).get() + sum_bound(2 * each).get(),
        "Int/sub: the negated subtrahend, then the sum of both"
    );
}

/// Every other fold that writes a sum back is charged for it as addition is: a widening for the image of each summand, a narrowing for each preimage, and the division's split for the summands it parts into a quotient and a residual — each at least [`sum_bound`] of its operand's summands over what the operand costs to reduce.
///
/// Mutation-checked: with the charge taken out of a fold, its row costs less than the sum it writes.
#[test]
fn a_fold_that_writes_a_sum_back_is_charged_for_it() {
    const SUMMANDS: u32 = 32;

    let doubled = || {
        (0..SUMMANDS)
            .map(|index| scaled(2, sym(index, "x")))
            .reduce(add)
            .expect("a sum of at least one summand")
    };
    let widened = || Term::intrinsic(Intrinsic::NatToInt(doubled()));
    let narrowed = Term::intrinsic(Intrinsic::IntToNat {
        int: widened(),
        non_neg: qed(),
    });
    let halved = Term::intrinsic(Intrinsic::NatDiv {
        dividend: doubled(),
        divisor: lit(2),
        non_zero: qed(),
    });

    for (fold, over, operand) in [
        ("Nat/to_int", widened(), doubled()),
        ("Int/to_nat", narrowed, widened()),
        ("Nat/div", halved, doubled()),
    ] {
        assert!(
            charged(over) - charged(operand) >= sum_bound(SUMMANDS as usize).get(),
            "{fold}: a sum written back is charged for its summands"
        );
    }
}

/// The literal a sum computes is charged by its operands' widths wherever the fold adds two, over a symbolic part as between two closed literals, on each carrier: two wide floors, each over a symbol, cost their sum at least the number it builds, and a wide floor added to a bare symbol, which stands as it was, costs nothing for its width.
///
/// Mutation-checked: with the literals charged only between closed operands, a carrier's first sum costs less than its floor.
#[test]
fn a_floor_added_over_a_symbol_is_charged_by_its_width() {
    const DIGITS: usize = 1_000;

    type Join = fn(Term, Term) -> Term;
    let int_add: Join = |left, right| Term::intrinsic(Intrinsic::IntAdd(left, right));
    let digits = Natural::parse_bytes(&vec![b'9'; DIGITS], 10).expect("a decimal numeral");
    let nat_wide = Term::intrinsic(Intrinsic::Nat(Nat::new(digits.clone())));
    let int_wide = Term::intrinsic(Intrinsic::Int(Integer::from(digits)));
    // A decimal digit is a little over three and a third bits; rounding down keeps this a lower bound.
    let limbs = (DIGITS as u64 * 33) / 10 / 64;

    for (carrier, join, wide) in [("nat", add as Join, nat_wide), ("int", int_add, int_wide)] {
        let over = |index: u32| join(sym(index, "x"), wide.clone());

        let both = charged(join(over(0), over(1))) - charged(over(0)) - charged(over(1));
        assert!(
            both >= limbs,
            "{carrier}, two floors: charged {both} for a number of {limbs} units"
        );

        let one = charged(join(over(0), sym(1, "x"))) - charged(over(0));
        assert!(
            one < limbs,
            "{carrier}, one floor: charged {one} for a number that stands as it was"
        );
    }
}

/// A sum no product changed is handed back as the fold left it, the node it was, on each carrier: normalizing writes back, and is charged for, only a sum one of whose summands it distributed.
///
/// Mutation-checked: with the sum written back whatever its summands, a carrier's answer is another node.
#[test]
fn a_sum_no_product_changed_is_handed_back_unbuilt() {
    type Join = fn(Term, Term) -> Term;
    let int_add: Join = |left, right| Term::intrinsic(Intrinsic::IntAdd(left, right));
    let sum = |join: Join| {
        fold(
            (0..8)
                .map(|index| sym(index, "x"))
                .reduce(join)
                .expect("a sum of at least one summand"),
        )
    };

    let nat = sum(add);
    let normalized = Nat::normalize(&mut Inert, nat.clone()).expect("normalizes");
    assert_eq!(normalized.identity(), nat.identity(), "nat");

    let int = sum(int_add);
    let normalized = int_normalize(&mut Inert, int.clone()).expect("normalizes");
    assert_eq!(normalized.identity(), int.identity(), "int");
}

/// A product over a symbol is written back distributed, so the fold is charged for what distribution writes and not for one node: over what its two operands cost to reduce, a sum of symbols times a symbol pays [`product_bound`] of the two, on each carrier, and a budget one unit short of that refuses it before anything is built.
///
/// Mutation-checked: with the charge taken out of either fold, its row costs nothing over its operands and its literal row.
#[test]
fn a_product_is_charged_for_every_monomial_it_writes_back() {
    const SUMMANDS: u32 = 64;

    type Join = fn(Term, Term) -> Term;
    let int_add: Join = |left, right| Term::intrinsic(Intrinsic::IntAdd(left, right));
    let nat_mul: Join = |left, right| Term::intrinsic(Intrinsic::nat_mul(left, right));
    let int_mul: Join = |left, right| Term::intrinsic(Intrinsic::IntMul(left, right));
    // Every summand is one symbol under the coefficient one, a bit wide.
    let sum = Footprint {
        summands: SUMMANDS as usize,
        factors: SUMMANDS as usize,
        bits: u64::from(SUMMANDS),
    };
    let symbol = Footprint {
        summands: 1,
        factors: 1,
        bits: 1,
    };
    let bound = product_bound(&sum, &symbol).get();

    // `Nat`'s fold pays its literal row whatever its operands are; `Int`'s only between two literals.
    for (carrier, join, over, literals) in [
        ("nat", add as Join, nat_mul, operand_bound(0, 0).get()),
        ("int", int_add, int_mul, 0),
    ] {
        let left = (0..SUMMANDS)
            .map(|index| sym(index, "x"))
            .reduce(join)
            .expect("a sum of at least one summand");
        let right = sym(SUMMANDS, "y");
        let operands = charged(left.clone());

        assert_eq!(
            charged(over(left.clone(), right.clone())) - operands,
            literals + bound,
            "{carrier}: a sum times a symbol is charged for every monomial and its factors"
        );

        let mut reducer = Budgeted {
            remaining: operands + literals + bound - 1,
        };
        assert!(
            matches!(
                reducer.reduce(over(left, right)),
                Err(ReduceError::Exhausted { .. })
            ),
            "{carrier}: a budget short of the product refuses it"
        );
    }
}

/// A monomial is as long as the two it is the product of, so a product is charged by its operands' factors and a power built a factor at a time costs each step what it has grown to: a symbol times a monomial of so many factors pays a node for each of them, on each carrier.
///
/// Mutation-checked: with a product charged by its monomials alone, each row costs one node whatever its degree.
#[test]
fn a_product_is_charged_for_every_factor_it_writes_back() {
    const FACTORS: u32 = 64;

    type Join = fn(Term, Term) -> Term;
    let nat_mul: Join = |left, right| Term::intrinsic(Intrinsic::nat_mul(left, right));
    let int_mul: Join = |left, right| Term::intrinsic(Intrinsic::IntMul(left, right));

    for (carrier, over) in [("nat", nat_mul), ("int", int_mul)] {
        let monomial = (0..FACTORS)
            .map(|index| sym(index, "x"))
            .reduce(over)
            .expect("a product of at least one factor");
        let operands = charged(monomial.clone());

        assert!(
            charged(over(monomial, sym(FACTORS, "y"))) - operands
                >= Cost::term(2).get() * u64::from(FACTORS),
            "{carrier}: a monomial written back is charged for its factors"
        );
    }
}

/// A coefficient is as wide as the two it is the product of and is written once for every summand it is multiplied over, so a wide literal times a sum of symbols costs at least that many numbers of its width, on each carrier.
///
/// Mutation-checked: with a product charged for no coefficient, each row costs its literal row's one number at most.
#[test]
fn a_coefficient_multiplied_over_a_sum_is_charged_by_its_width() {
    const DIGITS: usize = 1_000;
    const SUMMANDS: u32 = 16;

    type Join = fn(Term, Term) -> Term;
    let int_add: Join = |left, right| Term::intrinsic(Intrinsic::IntAdd(left, right));
    let nat_mul: Join = |left, right| Term::intrinsic(Intrinsic::nat_mul(left, right));
    let int_mul: Join = |left, right| Term::intrinsic(Intrinsic::IntMul(left, right));
    let digits = Natural::parse_bytes(&vec![b'9'; DIGITS], 10).expect("a decimal numeral");
    let nat_wide = Term::intrinsic(Intrinsic::Nat(Nat::new(digits.clone())));
    let int_wide = Term::intrinsic(Intrinsic::Int(Integer::from(digits)));
    // A decimal digit is a little over three and a third bits; rounding down keeps this a lower bound.
    let limbs = (DIGITS as u64 * 33) / 10 / 64;

    for (carrier, join, over, wide) in [
        ("nat", add as Join, nat_mul, nat_wide),
        ("int", int_add, int_mul, int_wide),
    ] {
        let sum = (0..SUMMANDS)
            .map(|index| sym(index, "x"))
            .reduce(join)
            .expect("a sum of at least one summand");
        let product = charged(over(wide, sum.clone())) - charged(sum);

        assert!(
            product >= limbs * u64::from(SUMMANDS),
            "{carrier}: charged {product} for {SUMMANDS} numbers of {limbs} units"
        );
    }
}

/// A product of two symbolic sums stands as written and costs nothing over its operands, on each carrier; it is charged where its value is asked for by name and distribution writes it, every summand of one against every summand of the other: normalizing it pays [`product_bound`] of the two over what reducing it costs.
///
/// Mutation-checked: with the charge taken out of either normalization, its row pays nothing for the product.
#[test]
fn a_product_of_two_sums_is_charged_where_it_is_distributed() {
    const SUMMANDS: u32 = 16;
    const AMPLE: u64 = 1_000_000_000;

    type Join = fn(Term, Term) -> Term;
    type Normalize = fn(&mut Budgeted, Term) -> Result<Term, ReduceError>;
    let int_add: Join = |left, right| Term::intrinsic(Intrinsic::IntAdd(left, right));
    let nat_mul: Join = |left, right| Term::intrinsic(Intrinsic::nat_mul(left, right));
    let int_mul: Join = |left, right| Term::intrinsic(Intrinsic::IntMul(left, right));
    let each = Footprint {
        summands: SUMMANDS as usize,
        factors: SUMMANDS as usize,
        bits: u64::from(SUMMANDS),
    };
    let bound = product_bound(&each, &each).get();

    for (carrier, join, over, normalize) in [
        ("nat", add as Join, nat_mul, Nat::normalize as Normalize),
        ("int", int_add, int_mul, int_normalize as Normalize),
    ] {
        let sum = |from: u32| {
            (from..from + SUMMANDS)
                .map(|index| sym(index, "x"))
                .reduce(join)
                .expect("a sum of at least one summand")
        };
        let normalized = |term: Term| {
            let mut reducer = Budgeted { remaining: AMPLE };
            normalize(&mut reducer, term).expect("the subject normalizes");

            AMPLE - reducer.remaining
        };
        let stuck = over(sum(0), sum(SUMMANDS));
        let standing = charged(stuck.clone());

        assert_eq!(
            standing,
            charged(sum(0)) + charged(sum(SUMMANDS)),
            "{carrier}: a product of two sums costs nothing where it stands"
        );
        assert_eq!(
            normalized(stuck) - standing,
            bound,
            "{carrier}: normalizing is charged for the product it distributes"
        );
    }
}

/// A normalization forces its term once, at its root, on each carrier: a fold forces its operands before it writes, so the nodes under a weak-head term are weak-head already, and a monomial normalized costs what reducing it costs and nothing for its factors forced again.
///
/// Mutation-checked: with each node forced on the way down, a carrier's monomial costs a reduction a level.
#[test]
fn a_normalization_forces_its_term_once() {
    const FACTORS: u32 = 16;
    const AMPLE: u64 = 1_000_000_000;

    type Join = fn(Term, Term) -> Term;
    type Normalize = fn(&mut Budgeted, Term) -> Result<Term, ReduceError>;
    let nat_mul: Join = |left, right| Term::intrinsic(Intrinsic::nat_mul(left, right));
    let int_mul: Join = |left, right| Term::intrinsic(Intrinsic::IntMul(left, right));

    for (carrier, over, normalize) in [
        ("nat", nat_mul, Nat::normalize as Normalize),
        ("int", int_mul, int_normalize as Normalize),
    ] {
        let monomial = (0..FACTORS)
            .map(|index| sym(index, "x"))
            .reduce(over)
            .expect("a product of at least one factor");
        let mut reducer = Budgeted { remaining: AMPLE };
        normalize(&mut reducer, monomial.clone()).expect("the subject normalizes");

        assert_eq!(AMPLE - reducer.remaining, charged(monomial), "{carrier}");
    }
}

/// A monomial the fold wrote is handed back as it stands, the node it was, on each carrier: normalizing distributes, and is charged for, only a product of two sums and what holds one.
///
/// Mutation-checked: with every product distributed again, a carrier's answer is another node.
#[test]
fn a_monomial_the_fold_wrote_is_handed_back_unbuilt() {
    type Join = fn(Term, Term) -> Term;
    type Normalize = fn(&mut Inert, Term) -> Result<Term, ReduceError>;
    let nat_mul: Join = |left, right| Term::intrinsic(Intrinsic::nat_mul(left, right));
    let int_mul: Join = |left, right| Term::intrinsic(Intrinsic::IntMul(left, right));
    let int = |value: i32| Term::intrinsic(Intrinsic::Int(Integer::from(value)));

    for (carrier, over, two, normalize) in [
        ("nat", nat_mul, lit(2), Nat::normalize as Normalize),
        ("int", int_mul, int(2), int_normalize as Normalize),
    ] {
        let monomial = fold((0..8).map(|index| sym(index, "x")).fold(two, over));

        let normalized = normalize(&mut Inert, monomial.clone()).expect("normalizes");

        assert_eq!(normalized.identity(), monomial.identity(), "{carrier}");
    }
}

/// A monomial's image under the embedding of `Nat` in `Int` is one spine, written once, so each fold that writes an image or reads one back is charged by the factors it holds and no more: a monomial of so many factors widened, narrowed and taken the magnitude of pays each a node a factor at least, and no fold pays for the square of them. The comparison that reads two images back is held to its figure by the cancellation above.
///
/// Mutation-checked: with the charge taken out of a fold, its row costs less than its factors.
#[test]
fn an_image_is_charged_for_every_factor_it_writes() {
    const FACTORS: u32 = 64;

    let monomial = || {
        (0..FACTORS)
            .map(|index| sym(index, "x"))
            .reduce(|left, right| Term::intrinsic(Intrinsic::nat_mul(left, right)))
            .expect("a product of at least one factor")
    };
    let widened = || Term::intrinsic(Intrinsic::NatToInt(monomial()));
    let narrowed = Term::intrinsic(Intrinsic::IntToNat {
        int: widened(),
        non_neg: qed(),
    });
    let magnitude = Term::intrinsic(Intrinsic::IntAbs(widened()));
    let factors = Cost::term(2).get() * u64::from(FACTORS);
    let square = factors * u64::from(FACTORS) / 2;

    for (fold, over, operand) in [
        ("Nat/to_int", widened(), monomial()),
        ("Int/to_nat", narrowed, widened()),
        ("Int/abs", magnitude, widened()),
    ] {
        let image = charged(over) - charged(operand);

        assert!(
            (factors..square).contains(&image),
            "{fold}: charged {image} for {FACTORS} factors"
        );
    }
}
