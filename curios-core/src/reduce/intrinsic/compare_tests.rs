//! Symbolic `Nat` comparison: shared addends, commuted sums, and the bound an indexed loop walks under.

use {
    super::{
        Comparison, align_comparisons, compare_int, compare_nat, reduce_intrinsic, test_support::*,
    },
    crate::{Aligned, Intrinsic, Nat, ReduceError, Subterm, Term},
    curios_num::Integer,
};

// What the cancellation buys the comparison family: a shared addend decides nothing, so removing it lets a stuck comparison stall on the operands that actually differ. `Le(x + a, x + b)` becoming `Le(a, b)` is what makes a decided proposition usable under a binder rather than only at literals.
#[test]
fn nat_sees_through_a_shared_addend() {
    let (x, a, b) = (sym(0, "x"), sym(1, "a"), sym(2, "b"));

    let (outcome, left, right) = compare_nat(
        &mut Inert,
        add(x.clone(), a.clone()),
        add(x.clone(), b.clone()),
    )
    .expect("reduces");

    assert_eq!(
        outcome,
        Comparison::Stuck,
        "two distinct symbols decide nothing"
    );
    assert_eq!(
        occurrences(&left, &x),
        0,
        "the shared `x` is gone from the left"
    );
    assert_eq!(occurrences(&right, &x), 0, "and from the right");
    assert_eq!(occurrences(&left, &a), 1);
    assert_eq!(occurrences(&right, &b), 1);
}

// Commutativity of `+` becomes definitional for the whole comparison family, which is the larger half of what cancellation buys: nothing else in the reducer normalises the order of a sum's summands.
#[test]
fn nat_decides_a_commuted_sum_equal() {
    let (a, b) = (sym(0, "a"), sym(1, "b"));

    let (outcome, _, _) = compare_nat(
        &mut Inert,
        add(a.clone(), b.clone()),
        add(b.clone(), a.clone()),
    )
    .expect("reduces");

    assert_eq!(
        outcome,
        Comparison::Eq,
        "`a + b` and `b + a` are the same number"
    );
}

// The bound every indexed loop in the standard library needs: walking `i` up to `n` under an invariant `i + k = n` asks for `i < i + kp + 1` at each step. The comparison decides it outright, by cancellation.
#[test]
fn nat_decides_the_bound_an_indexed_loop_walks_under() {
    let (i, kp) = (sym(0, "i"), sym(1, "kp"));
    let ceiling = Nat::rebuild(1u32.into(), add(i.clone(), kp.clone()));

    let (outcome, _, _) = compare_nat(&mut Inert, i.clone(), ceiling).expect("reduces");

    assert_eq!(
        outcome,
        Comparison::Lt,
        "`i < i + kp + 1` holds for every `kp`"
    );
}

// A bound met exactly is the non-strict verdict: `x % 7` is at most `6`, which `<=` reads as `true` where `<` reads nothing, and the same bound one higher is the strict one. The mirrored pair puts the literal on the left.
#[test]
fn nat_decides_a_bound_met_exactly_as_non_strict() {
    let remainder = Term::intrinsic(Intrinsic::NatRem {
        dividend: sym(0, "x"),
        divisor: lit(7),
        non_zero: qed(),
    });

    let (met, _, _) = compare_nat(&mut Inert, remainder.clone(), lit(6)).expect("reduces");
    assert_eq!(met, Comparison::Le, "`x % 7 <= 6` for every `x`");

    let (under, _, _) = compare_nat(&mut Inert, remainder.clone(), lit(7)).expect("reduces");
    assert_eq!(under, Comparison::Lt, "`x % 7 < 7` for every `x`");

    let (mirrored, _, _) = compare_nat(&mut Inert, lit(6), remainder).expect("reduces");
    assert_eq!(mirrored, Comparison::Ge, "`6 >= x % 7` for every `x`");
}

// Soundness gate: the conversions preserve the number. `Nat/to_int` folds every literal — ℕ embeds in ℤ, both unbounded here — and `Int/to_nat` folds a non-negative to the same value and reports a negative like a zero divisor, never wrapping bits.
#[test]
fn conversion_folds_preserve_the_number() {
    for n in [
        0u64,
        1,
        0x3FFF_FFFF,
        0x4000_0000,
        0x7FFF_FFFF,
        0x8000_0000,
        0xFFFF_FFFF,
        0x1_0000_0000,
    ] {
        let nat = Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
        let reduced = reduce_intrinsic(&mut Inert, &Intrinsic::NatToInt(nat)).expect("reduces");
        assert_eq!(
            reduced,
            Subterm::Intrinsic(Intrinsic::Int(Integer::from(n))),
            "Nat/to_int changed the number on {n}",
        );
    }
    for i in [0i64, 1, 0x3FFF_FFFF, 0x7FFF_FFFF, 0x1_0000_0000] {
        let int = Term::intrinsic(Intrinsic::Int(Integer::from(i)));
        let reduced = reduce_intrinsic(
            &mut Inert,
            &Intrinsic::IntToNat {
                int,
                non_neg: qed(),
            },
        )
        .expect("reduces");
        assert_eq!(
            reduced,
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(i as u64))),
            "Int/to_nat changed the number on {i}",
        );
    }
    for i in [-1i64, -0x4000_0000, i32::MIN as i64, i64::MIN] {
        let int = Term::intrinsic(Intrinsic::Int(Integer::from(i)));
        let reduced = reduce_intrinsic(
            &mut Inert,
            &Intrinsic::IntToNat {
                int,
                non_neg: qed(),
            },
        );
        assert!(
            matches!(reduced, Err(ReduceError::IntToNatNegative { .. })),
            "Int/to_nat failed to report the negative {i}",
        );
    }
}

// Soundness gate: the structural body agrees with the host ordering on every pair of literals — the decidable closed case where the two routes into a `Comparison` (the shared-inner shortcut vs. the host `cmp`) must coincide.
#[test]
fn nat_agrees_with_literal_ordering() {
    let mut reducer = Inert;
    let samples = [0u32, 1, 2, 5, 42, 128, 255, 256, 1000];
    for &m in &samples {
        for &n in &samples {
            assert_eq!(
                compare_nat(&mut reducer, lit(m), lit(n))
                    .expect("reduces")
                    .0,
                Comparison::of(m.cmp(&n)),
                "compare_nat disagreed with the literal ordering on ({m}, {n})",
            );
        }
    }
}

// A value compared to an operand it never exceeds: the minuend bounds a difference whatever is subtracted, and a divisor bounds a remainder strictly, `non_zero` having placed it at one or more. One short of the divisor the verdict stops at the non-strict one: `x % (y + 1)` can be `y`.
#[test]
fn nat_decides_a_value_against_an_operand_it_never_exceeds() {
    let (x, y) = (sym(0, "x"), sym(1, "y"));
    let divisor = Nat::rebuild(1u32.into(), y.clone());

    let difference = Term::intrinsic(Intrinsic::nat_sub(x.clone(), y.clone()));
    let (at_most, _, _) = compare_nat(&mut Inert, difference, x.clone()).expect("reduces");
    assert_eq!(at_most, Comparison::Le, "`x - y <= x` for every `y`");

    let remainder = Term::intrinsic(Intrinsic::NatRem {
        dividend: x,
        divisor: divisor.clone(),
        non_zero: qed(),
    });
    let (below, _, _) = compare_nat(&mut Inert, remainder.clone(), divisor).expect("reduces");
    assert_eq!(below, Comparison::Lt, "`x % (y + 1) < y + 1` for every `x`");

    let (at_most, _, _) = compare_nat(&mut Inert, remainder, y).expect("reduces");
    assert_eq!(
        at_most,
        Comparison::Le,
        "`x % (y + 1) <= y`, and no more than that"
    );
}

// A side is compared through each operand it never exceeds, and so is the other, so two sides nested this deep lead to one pair of their subterms along every interleaving of the two descents: more paths than a run walks. The comparison keeps each pair's verdict, and what it costs is the product of the two sides' sizes.
#[test]
fn two_chains_of_operands_a_value_never_exceeds_compare_each_pair_once() {
    let links: [fn(Term, Term) -> Term; 4] = [
        |so_far, operand| Term::intrinsic(Intrinsic::NatAnd(so_far, operand)),
        |so_far, operand| Term::intrinsic(Intrinsic::NatShr(so_far, operand)),
        |so_far, operand| {
            Term::intrinsic(Intrinsic::NatDiv {
                dividend: so_far,
                divisor: Nat::rebuild(1u32.into(), operand),
                non_zero: qed(),
            })
        },
        |so_far, operand| {
            Term::intrinsic(Intrinsic::NatRem {
                dividend: so_far,
                divisor: Nat::rebuild(1u32.into(), operand),
                non_zero: qed(),
            })
        },
    ];

    for link in links {
        let chain = |first: u32| {
            (1..=24).fold(sym(first, "x"), |so_far, at| {
                link(so_far, sym(first + at, "y"))
            })
        };
        let (verdict, _, _) = compare_nat(&mut Inert, chain(0), chain(100)).expect("reduces");
        assert_eq!(
            verdict,
            Comparison::Stuck,
            "two chains that share no symbol decide nothing"
        );
    }
}

// A side with a literal bound decides against the floor under a symbolic side, as it does against a bare literal: the other side is at least its floor whatever stands above it. A byte's value has a bound and no operand it never exceeds, so nothing else decides these. The control is a floor one short of the bound.
#[test]
fn nat_decides_a_bounded_side_against_the_floor_under_a_symbolic_one() {
    let (y, digit) = (
        sym(1, "y"),
        Term::intrinsic(Intrinsic::ByteToNat(sym(2, "b"))),
    );
    let above = |floor: u32| Nat::rebuild(floor.into(), y.clone());

    let (met, _, _) = compare_nat(&mut Inert, digit.clone(), above(255)).expect("reduces");
    assert_eq!(met, Comparison::Le, "`Byte/to_nat(b) <= y + 255`");
    let (under, _, _) = compare_nat(&mut Inert, digit.clone(), above(256)).expect("reduces");
    assert_eq!(under, Comparison::Lt, "`Byte/to_nat(b) < y + 256`");
    let (mirrored, _, _) = compare_nat(&mut Inert, above(255), digit.clone()).expect("reduces");
    assert_eq!(mirrored, Comparison::Ge, "`y + 255 >= Byte/to_nat(b)`");

    let (open, _, _) = compare_nat(&mut Inert, digit, above(254)).expect("reduces");
    assert_eq!(
        open,
        Comparison::Stuck,
        "`b = 255` passes `y + 254` at zero"
    );
}

// A remainder is strictly below its divisor, so it is at most whatever the divisor is at most one past: the two spellings of that fact across the `<`/`<=` seam decide alike, at a literal divisor and at a symbolic one. The control is two short of the divisor.
#[test]
fn nat_decides_a_remainder_at_most_one_short_of_its_divisor() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(3, "z"));
    let remainder = |divisor: Term| {
        Term::intrinsic(Intrinsic::NatRem {
            dividend: x.clone(),
            divisor,
            non_zero: qed(),
        })
    };
    let above = |floor: u32| Nat::rebuild(floor.into(), y.clone());

    let (strict, _, _) = compare_nat(&mut Inert, remainder(lit(8)), above(8)).expect("reduces");
    assert_eq!(strict, Comparison::Lt, "`x % 8 < y + 8`");
    let (at_most, _, _) = compare_nat(&mut Inert, remainder(lit(8)), above(7)).expect("reduces");
    assert_eq!(at_most, Comparison::Le, "`x % 8 <= y + 7`");
    let (mirrored, _, _) = compare_nat(&mut Inert, above(7), remainder(lit(8))).expect("reduces");
    assert_eq!(mirrored, Comparison::Ge, "`y + 7 >= x % 8`");
    let (open, _, _) = compare_nat(&mut Inert, remainder(lit(8)), above(6)).expect("reduces");
    assert_eq!(
        open,
        Comparison::Stuck,
        "`x % 8` can be `7`, past `y + 6` at zero"
    );

    // No literal bounds a remainder by a symbolic divisor: only the divisor itself does.
    let divisor = Nat::rebuild(1u32.into(), z.clone());
    let sum = add(y.clone(), z.clone());
    let (symbolic, _, _) =
        compare_nat(&mut Inert, remainder(divisor.clone()), sum.clone()).expect("reduces");
    assert_eq!(symbolic, Comparison::Le, "`x % (z + 1) <= y + z`");
    let (mirrored, _, _) = compare_nat(&mut Inert, sum, remainder(divisor)).expect("reduces");
    assert_eq!(mirrored, Comparison::Ge, "`y + z >= x % (z + 1)`");
}

// The order the comparison decides is transitive over the shapes it reads: wherever `a <= b` and `b <= c` are each decided, so is `a <= c`. An operation that keeps the least of its operands gives one answer for both associations only on that, since the inner node may drop the operand that ordered the other two.
#[test]
fn the_order_nat_decides_is_transitive() {
    let (x, y) = (sym(0, "x"), sym(1, "y"));
    let above = |floor: u32, inner: &Term| Nat::rebuild(floor.into(), inner.clone());
    let remainder = |divisor: Term| {
        Term::intrinsic(Intrinsic::NatRem {
            dividend: x.clone(),
            divisor,
            non_zero: qed(),
        })
    };
    // A power is at least one, so what holds one is at least one past its floor.
    let power = Term::intrinsic(Intrinsic::NatShl(lit(1), y.clone()));
    let shapes = [
        ("0", lit(0)),
        ("7", lit(7)),
        ("8", lit(8)),
        ("255", lit(255)),
        ("x", x.clone()),
        ("y", y.clone()),
        ("x + 1", above(1, &x)),
        ("y + 7", above(7, &y)),
        ("y + 8", above(8, &y)),
        ("y + 255", above(255, &y)),
        ("x + y", add(x.clone(), y.clone())),
        ("x + y + 1", above(1, &add(x.clone(), y.clone()))),
        (
            "x - y",
            Term::intrinsic(Intrinsic::nat_sub(x.clone(), y.clone())),
        ),
        ("x % 8", remainder(lit(8))),
        ("x % 8 + 1", above(1, &remainder(lit(8)))),
        ("x % (y + 1)", remainder(above(1, &y))),
        (
            "Nat/and(x, 7)",
            Term::intrinsic(Intrinsic::NatAnd(x.clone(), lit(7))),
        ),
        (
            "Nat/and(x, y)",
            Term::intrinsic(Intrinsic::NatAnd(x.clone(), y.clone())),
        ),
        (
            "Nat/shr(x, y)",
            Term::intrinsic(Intrinsic::NatShr(x.clone(), y.clone())),
        ),
        (
            "x / 3",
            Term::intrinsic(Intrinsic::NatDiv {
                dividend: x.clone(),
                divisor: lit(3),
                non_zero: qed(),
            }),
        ),
        (
            "Byte/to_nat(b)",
            Term::intrinsic(Intrinsic::ByteToNat(sym(2, "b"))),
        ),
        (
            "Nat/min(x, y)",
            Term::intrinsic(Intrinsic::NatMin(x.clone(), y.clone())),
        ),
        (
            "Nat/max(x, y)",
            Term::intrinsic(Intrinsic::NatMax(x.clone(), y.clone())),
        ),
        (
            "Nat/min(x, y) + 7",
            above(7, &Term::intrinsic(Intrinsic::NatMin(x.clone(), y.clone()))),
        ),
        (
            "Nat/max(x % 8, y)",
            Term::intrinsic(Intrinsic::NatMax(remainder(lit(8)), y.clone())),
        ),
        (
            "Nat/min(x, 7)",
            Term::intrinsic(Intrinsic::NatMin(x.clone(), lit(7))),
        ),
        ("1", lit(1)),
        ("Nat/shl(1, y)", power.clone()),
        ("Nat/shl(1, y) + 7", above(7, &power)),
        ("x + Nat/shl(1, y)", add(x.clone(), power.clone())),
        (
            "x * Nat/shl(1, y)",
            Term::intrinsic(Intrinsic::nat_mul(x.clone(), power.clone())),
        ),
    ];

    let at_most = shapes
        .iter()
        .map(|(_, left)| {
            shapes
                .iter()
                .map(|(_, right)| {
                    let (verdict, _, _) =
                        compare_nat(&mut Inert, left.clone(), right.clone()).expect("reduces");
                    matches!(verdict, Comparison::Lt | Comparison::Le | Comparison::Eq)
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    // A grid that decided nothing would hold vacuously.
    assert!(at_most.iter().flatten().filter(|decided| **decided).count() > shapes.len());

    let mut apart = Vec::new();
    for a in 0..shapes.len() {
        for b in 0..shapes.len() {
            for c in 0..shapes.len() {
                if at_most[a][b] && at_most[b][c] && !at_most[a][c] {
                    apart.push(format!(
                        "`{}` <= `{}` <= `{}`",
                        shapes[a].0, shapes[b].0, shapes[c].0
                    ));
                }
            }
        }
    }
    assert!(
        apart.is_empty(),
        "decided at each step and not across:\n{}",
        apart.join("\n")
    );
}

// The signed comparison's order is transitive too, over a constant apart, a widened natural apart, and a widened natural's own floor and bound.
#[test]
fn the_order_int_decides_is_transitive() {
    let (i, n, m) = (sym(0, "i"), sym(1, "n"), sym(2, "m"));
    let integer = |value: i32| Term::intrinsic(Intrinsic::Int(Integer::from(value)));
    let plus = |left: &Term, right: Term| Term::intrinsic(Intrinsic::IntAdd(left.clone(), right));
    let widened = |natural: Term| Term::intrinsic(Intrinsic::NatToInt(natural));
    let low = Term::intrinsic(Intrinsic::NatRem {
        dividend: n.clone(),
        divisor: lit(8),
        non_zero: qed(),
    });
    // `Nat`'s power widened, which a comparison pulls back to `Nat` and reads at least one.
    let power = Term::intrinsic(Intrinsic::NatShl(lit(1), n.clone()));
    let shapes = [
        ("-1", integer(-1)),
        ("+0", integer(0)),
        ("+7", integer(7)),
        ("+8", integer(8)),
        ("i", i.clone()),
        ("i - 1", plus(&i, integer(-1))),
        ("i + 1", plus(&i, integer(1))),
        ("Nat/to_int(n)", widened(n.clone())),
        ("Nat/to_int(n) + 1", plus(&widened(n.clone()), integer(1))),
        (
            "Nat/to_int(n) + Nat/to_int(m)",
            plus(&widened(n.clone()), widened(m)),
        ),
        ("i + Nat/to_int(n)", plus(&i, widened(n.clone()))),
        (
            "i + Nat/to_int(n) + 1",
            plus(&plus(&i, widened(n.clone())), integer(1)),
        ),
        ("Nat/to_int(n % 8)", widened(low.clone())),
        ("Nat/to_int(n % 8) + 1", plus(&widened(low), integer(1))),
        (
            "Int/min(i, Nat/to_int(n))",
            Term::intrinsic(Intrinsic::IntMin(i.clone(), widened(n.clone()))),
        ),
        (
            "Int/max(i, Nat/to_int(n))",
            Term::intrinsic(Intrinsic::IntMax(i.clone(), widened(n.clone()))),
        ),
        (
            "Int/max(i, +7)",
            Term::intrinsic(Intrinsic::IntMax(i.clone(), integer(7))),
        ),
        ("+1", integer(1)),
        ("Nat/to_int(Nat/shl(1, n))", widened(power.clone())),
        (
            "Nat/to_int(Nat/shl(1, n)) + 7",
            plus(&widened(power), integer(7)),
        ),
    ];

    let at_most = shapes
        .iter()
        .map(|(_, left)| {
            shapes
                .iter()
                .map(|(_, right)| {
                    let (verdict, _, _) =
                        compare_int(&mut Inert, left.clone(), right.clone()).expect("reduces");
                    matches!(verdict, Comparison::Lt | Comparison::Le | Comparison::Eq)
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    assert!(at_most.iter().flatten().filter(|decided| **decided).count() > shapes.len());

    let mut apart = Vec::new();
    for a in 0..shapes.len() {
        for b in 0..shapes.len() {
            for c in 0..shapes.len() {
                if at_most[a][b] && at_most[b][c] && !at_most[a][c] {
                    apart.push(format!(
                        "`{}` <= `{}` <= `{}`",
                        shapes[a].0, shapes[b].0, shapes[c].0
                    ));
                }
            }
        }
    }
    assert!(
        apart.is_empty(),
        "decided at each step and not across:\n{}",
        apart.join("\n")
    );
}

// Divisibility decides what no ordering does: every symbolic summand is a multiple of the gcd of the coefficients, so two floors apart modulo it meet at no value, and the verdict is unequal with the order left open. The two controls are what a test that dropped the modulus, or the gcd, would decide wrongly: `2 · x` meets `2 · y + 2` at `x = y + 1`, and coefficients whose gcd is `1` say nothing — `2 · x + 1` meets `3 · y` at `x = y = 1`.
#[test]
fn nat_decides_floors_apart_modulo_the_coefficients_gcd_unequal() {
    let (x, y) = (sym(0, "x"), sym(1, "y"));
    let odd = Nat::rebuild(1u32.into(), scaled(2, x.clone()));

    let (parity, _, _) =
        compare_nat(&mut Inert, odd.clone(), scaled(2, y.clone())).expect("reduces");
    assert_eq!(parity, Comparison::Ne, "`2 · x + 1` is never `2 · y`");

    let (wider, _, _) = compare_nat(
        &mut Inert,
        Nat::rebuild(6u32.into(), scaled(4, x.clone())),
        Nat::rebuild(1u32.into(), scaled(2, y.clone())),
    )
    .expect("reduces");
    assert_eq!(wider, Comparison::Ne, "`4 · x + 6` is never `2 · y + 1`");

    let (congruent, _, _) = compare_nat(
        &mut Inert,
        scaled(2, x),
        Nat::rebuild(2u32.into(), scaled(2, y.clone())),
    )
    .expect("reduces");
    assert_eq!(
        congruent,
        Comparison::Stuck,
        "floors that agree modulo the gcd decide nothing"
    );

    let (coprime, _, _) = compare_nat(&mut Inert, odd, scaled(3, y)).expect("reduces");
    assert_eq!(
        coprime,
        Comparison::Stuck,
        "a gcd of one divides every floor"
    );
}

// The signed twin: the argument needs every monomial to be an integer and nothing more, so it holds with a negative coefficient and a negative constant, and the two constants are apart exactly when their difference is.
#[test]
fn int_decides_floors_apart_modulo_the_coefficients_gcd_unequal() {
    let (i, j) = (sym(0, "i"), sym(1, "j"));
    let integer = |value: i32| Term::intrinsic(Intrinsic::Int(Integer::from(value)));
    let times = |coefficient: i32, factor: &Term| {
        Term::intrinsic(Intrinsic::IntMul(integer(coefficient), factor.clone()))
    };
    let plus =
        |left: Term, constant: i32| Term::intrinsic(Intrinsic::IntAdd(left, integer(constant)));

    let (parity, _, _) =
        compare_int(&mut Inert, plus(times(2, &i), 1), times(2, &j)).expect("reduces");
    assert_eq!(parity, Comparison::Ne, "`2 · i + 1` is never `2 · j`");

    let (negative, _, _) =
        compare_int(&mut Inert, plus(times(-4, &i), -3), times(6, &j)).expect("reduces");
    assert_eq!(negative, Comparison::Ne, "`-4 · i - 3` is never `6 · j`");

    let (congruent, _, _) =
        compare_int(&mut Inert, plus(times(2, &i), -2), times(2, &j)).expect("reduces");
    assert_eq!(
        congruent,
        Comparison::Stuck,
        "constants that agree modulo the gcd decide nothing"
    );
}

// The `<`/`<=` seam, as conversion aligns it: `x + 1 <= y` and `x < y` have one linear view once `<` is read as the `<=` of its successor, so they are one proposition — on either side of the pair.
#[test]
fn a_floored_le_meets_the_lt_it_spells() {
    let (x, y) = (sym(0, "x"), sym(1, "y"));
    let floored = Intrinsic::nat_lte(Nat::rebuild(1u32.into(), x.clone()), y.clone());
    let strict = Intrinsic::nat_lt(x, y);

    let aligned = align_comparisons(&mut Inert, &floored, &strict).expect("reduces");
    assert!(matches!(aligned, Some(Aligned::Same)));

    let mirrored = align_comparisons(&mut Inert, &strict, &floored).expect("reduces");
    assert!(matches!(mirrored, Some(Aligned::Same)));
}

// Two comparisons whose views differ are both respelled in the one spelling their views give — the positive part of the difference on the left of a `<=` — so the congruence after compares aligned operands: `n < y + 1` meets `x <= y` as `n <= y`, where `n` against `x` is what is left.
#[test]
fn two_comparisons_whose_views_differ_are_respelled_alike() {
    let (n, x, y) = (sym(0, "n"), sym(1, "x"), sym(2, "y"));
    let strict = Intrinsic::nat_lt(n.clone(), Nat::rebuild(1u32.into(), y.clone()));
    let loose = Intrinsic::nat_lte(x.clone(), y.clone());

    match align_comparisons(&mut Inert, &strict, &loose).expect("reduces") {
        Some(Aligned::Respelled(pair)) => {
            assert_eq!(pair.0, Intrinsic::NatLe(n, y.clone()));
            assert_eq!(pair.1, Intrinsic::NatLe(x, y));
        }
        _ => panic!("two views that differ are respelled"),
    }
}

// A power of two is at least one, so it is positive at every count, and so is what a sum or a product puts over one: `0 < shl(1, x)`, `shl(1, x) != 0`, `1 <= x + shl(1, y)` and `3 < shl(1, x) + 3`. A product by a factor nothing is known of is at least zero and no more, so `0 < x * shl(1, y)` stays undecided.
//
// Mutation-checked: a comparison that reads a side's floor for what it is at least leaves `0 < shl(1, x)` at `Le`.
#[test]
fn nat_decides_a_power_positive() {
    let (x, y) = (sym(0, "x"), sym(1, "y"));
    let power = |count: &Term| Term::intrinsic(Intrinsic::NatShl(lit(1), count.clone()));
    let verdict = |left: Term, right: Term| {
        let (verdict, _, _) = compare_nat(&mut Inert, left, right).expect("reduces");
        verdict
    };

    assert_eq!(verdict(lit(0), power(&x)), Comparison::Lt);
    assert_eq!(verdict(power(&x), lit(0)), Comparison::Gt);
    assert_eq!(verdict(lit(1), power(&x)), Comparison::Le);
    assert_eq!(
        verdict(lit(1), add(x.clone(), power(&y))),
        Comparison::Le,
        "a sum over a power is at least one"
    );
    assert_eq!(
        verdict(lit(3), Nat::rebuild(3u32.into(), power(&x))),
        Comparison::Lt,
        "the floor stands under what the power is at least"
    );
    assert_eq!(
        verdict(
            lit(0),
            Term::intrinsic(Intrinsic::nat_mul(power(&x), power(&y)))
        ),
        Comparison::Lt,
        "a product of two powers is at least one"
    );
    assert_eq!(
        verdict(
            lit(0),
            Term::intrinsic(Intrinsic::nat_mul(x.clone(), power(&y)))
        ),
        Comparison::Le,
        "a factor nothing is known of may be zero"
    );
}

// A `min` is at most each of its leaves and at least what is at most all of them, and a `max` is the mirror, read over the plain comparison's verdicts of the leaves: `min(x, y) <= x`, `x <= max(x, y)`, `z <= min(z + x, z + y)`, `max(x, y) <= x + y`, and a `min` against the `max` of the same two. Two trees of one leaf set are equal, which is what folds an equality of them. An operand nothing orders against the other side decides nothing.
//
// Mutation-checked: a `min` read as at most a term where every leaf is, and not where one is, leaves `min(x, y) <= x` undecided.
#[test]
fn nat_reads_a_min_and_a_max_through_the_lattice() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));
    let min =
        |left: &Term, right: &Term| Term::intrinsic(Intrinsic::NatMin(left.clone(), right.clone()));
    let max =
        |left: &Term, right: &Term| Term::intrinsic(Intrinsic::NatMax(left.clone(), right.clone()));
    let verdict = |left: Term, right: Term| {
        let (verdict, _, _) = compare_nat(&mut Inert, left, right).expect("reduces");
        verdict
    };

    assert_eq!(verdict(min(&x, &y), x.clone()), Comparison::Le);
    assert_eq!(verdict(x.clone(), min(&x, &y)), Comparison::Ge);
    assert_eq!(verdict(x.clone(), max(&x, &y)), Comparison::Le);
    assert_eq!(verdict(max(&x, &y), y.clone()), Comparison::Ge);
    assert_eq!(
        verdict(
            z.clone(),
            min(&add(z.clone(), x.clone()), &add(z.clone(), y.clone()))
        ),
        Comparison::Le,
        "at most both leaves is at most their `min`"
    );
    assert_eq!(
        verdict(max(&x, &y), add(x.clone(), y.clone())),
        Comparison::Le,
        "both leaves at most a term is their `max` at most it"
    );
    assert_eq!(verdict(min(&x, &y), max(&x, &y)), Comparison::Le);
    assert_eq!(verdict(min(&x, &y), min(&y, &x)), Comparison::Eq);
    assert_eq!(
        verdict(min(&x, &y), Nat::rebuild(1u32.into(), x.clone())),
        Comparison::Lt,
        "a leaf strictly below is the `min` strictly below"
    );
    assert_eq!(verdict(min(&x, &y), z.clone()), Comparison::Stuck);
    assert_eq!(verdict(max(&x, &y), x), Comparison::Ge);
}

// The same reading on `Int`, where nothing else is known of an operand: a `min` is at most each leaf and a `max` at least each, at either sign.
#[test]
fn int_reads_a_min_and_a_max_through_the_lattice() {
    let (i, j) = (sym(0, "i"), sym(1, "j"));
    let min = Term::intrinsic(Intrinsic::IntMin(i.clone(), j.clone()));
    let max = Term::intrinsic(Intrinsic::IntMax(i.clone(), j.clone()));
    let verdict = |left: Term, right: Term| {
        let (verdict, _, _) = compare_int(&mut Inert, left, right).expect("reduces");
        verdict
    };

    assert_eq!(verdict(min.clone(), i.clone()), Comparison::Le);
    assert_eq!(verdict(j.clone(), max.clone()), Comparison::Le);
    assert_eq!(verdict(min.clone(), max.clone()), Comparison::Le);
    assert_eq!(verdict(max, min.clone()), Comparison::Ge);
    assert_eq!(
        verdict(
            min,
            Term::intrinsic(Intrinsic::IntAdd(
                i,
                Term::intrinsic(Intrinsic::Int(Integer::from(1i32)))
            ))
        ),
        Comparison::Lt
    );
}
