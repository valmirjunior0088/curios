//! The intrinsic fold laws over symbolic operands, and the free-monoid peel behind them.

use super::test_support::qed;

use curios_core::*;
use {
    crate::{Context, reduce},
    curios_num::Natural,
    curios_utilities::{Grain, PackedBin},
};

fn context() -> Context {
    Context::with_default_budget(crate::SYNTAX)
}

fn lit(n: u32) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n as usize)))
}

fn succ(inner: Term) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::Succ(Natural::from(1u32), inner)))
}

fn reduced(context: &mut Context, term: Term) -> Subterm {
    Term::unwrap_or_clone(reduce(context, term).expect("reduces"))
}

// Symbolic successor bounds the family must decide — exactly the cases the old bespoke `lt` rule handled, now shared by the whole family (a regression guard).
#[test]
fn comparisons_decide_symbolic_successor_bounds() {
    let mut context = context();
    let symbolic = context.fresh(Some("x"));
    let x = || Term::free_var(&symbolic);

    // `succ x ≥ 1`: lt is false, ge is true; and `0 < succ x` is true.
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::nat_lt(succ(x()), lit(1)))
        ),
        Subterm::Intrinsic(Intrinsic::Bool(false)),
    );
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::nat_lte(lit(1), succ(x())))
        ),
        Subterm::Intrinsic(Intrinsic::Bool(true)),
    );
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::nat_lt(lit(0), succ(x())))
        ),
        Subterm::Intrinsic(Intrinsic::Bool(true)),
    );

    // Shared inner: `lt(x, succ x) = true`, `le(succ x, x) = false`.
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::nat_lt(x(), succ(x())))
        ),
        Subterm::Intrinsic(Intrinsic::Bool(true)),
    );
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::nat_lte(succ(x()), x()))
        ),
        Subterm::Intrinsic(Intrinsic::Bool(false)),
    );

    // The Str decoder blocker: `eql(succ(succ x), 1) = false` (shapes differ once the shared floor is peeled).
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::nat_eql(succ(succ(x())), lit(1)))
        ),
        Subterm::Intrinsic(Intrinsic::Bool(false)),
    );

    // A non-strict bound decides `le` but leaves `lt` genuinely undecidable (neutral), since `2 ≤ succ(succ x)` says nothing about strictness.
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::nat_lte(lit(2), succ(succ(x()))))
        ),
        Subterm::Intrinsic(Intrinsic::Bool(true)),
    );
    assert!(matches!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::nat_lt(lit(2), succ(succ(x()))))
        ),
        Subterm::Intrinsic(Intrinsic::NatLt(..)),
    ));
}

// Soundness gate for the distributing `Nat/mul`: on closed inputs it must still agree with the host product — the literal fold the floor distribution subsumes (`il = ir = 0`, so only the floors `sl · sr` remain).
#[test]
fn mul_agrees_with_literal_product() {
    let mut context = context();
    let samples = [0u32, 1, 2, 7, 13, 100];
    for &a in &samples {
        for &b in &samples {
            assert_eq!(
                reduced(
                    &mut context,
                    Term::intrinsic(Intrinsic::nat_mul(lit(a), lit(b)))
                ),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new((a * b) as usize))),
                "mul disagreed with the literal product on ({a}, {b})",
            );
        }
    }
}

// `Nat/mul` distributes a literal factor over a symbolic successor floor, the multiplicative twin of `NatAdd`'s floor law: `(x + 1) · c` and `x · c + c` reduce to the same normal form (either side may be the literal). Two symbolic operands have no literal factor, so the product stays neutral.
#[test]
fn mul_distributes_literal_over_symbolic_floor() {
    let mut context = context();
    let symbolic = context.fresh(Some("x"));
    let x = || Term::free_var(&symbolic);

    // `(x + 1) · 2 = x · 2 + 2`.
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::nat_mul(succ(x()), lit(2)))
        ),
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::nat_add(
                Term::intrinsic(Intrinsic::nat_mul(x(), lit(2))),
                lit(2)
            )),
        ),
    );

    // Commutative: `2 · (x + 1) = 2 · x + 2`.
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::nat_mul(lit(2), succ(x())))
        ),
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::nat_add(
                Term::intrinsic(Intrinsic::nat_mul(lit(2), x())),
                lit(2)
            )),
        ),
    );

    // No literal factor ⇒ neutral.
    assert!(matches!(
        reduced(&mut context, Term::intrinsic(Intrinsic::nat_mul(x(), x()))),
        Subterm::Intrinsic(Intrinsic::NatMul(..)),
    ));
}

// `cons(7, xs) = [7] ++ xs` over a symbolic tail `xs` — the symbolic cons `List/get` and `List/slice` previously could not peel (they folded only literal arrays), now decoded one element at a time like their `Bin` twins.
fn list_cons_seven(xs: &Term) -> Term {
    Term::intrinsic(Intrinsic::list_concat(
        Term::intrinsic(Intrinsic::NatType),
        [
            Term::intrinsic(Intrinsic::List {
                element: Term::intrinsic(Intrinsic::NatType),
                items: vec![lit(7)],
            }),
            xs.clone(),
        ],
    ))
}

#[test]
fn list_get_peels_symbolic_cons() {
    let mut context = context();
    let xs_binder = context.fresh(Some("xs"));
    let cons = list_cons_seven(&Term::free_var(&xs_binder));

    // `get(cons(7, xs), 0) = 7`.
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::list_get(
                Term::intrinsic(Intrinsic::NatType),
                cons.clone(),
                lit(0),
                qed(),
            ))
        ),
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(7usize))),
    );

    // `get(cons(7, xs), 1)` peels to `get(xs, 0)` — neutral over a symbolic tail.
    assert!(matches!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::list_get(
                Term::intrinsic(Intrinsic::NatType),
                cons,
                lit(1),
                qed(),
            ))
        ),
        Subterm::Intrinsic(Intrinsic::ListGet { .. }),
    ));
}

#[test]
fn list_slice_peels_symbolic_cons() {
    let mut context = context();
    let xs_binder = context.fresh(Some("xs"));
    let cons = list_cons_seven(&Term::free_var(&xs_binder));

    // `slice(cons(7, xs), 0, 1) = [7] ++ slice(xs, 0, 0) = [7]` — one element from the front.
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::list_slice(
                Term::intrinsic(Intrinsic::NatType),
                cons.clone(),
                lit(0),
                lit(1),
                qed(),
            ))
        ),
        Subterm::Intrinsic(Intrinsic::List {
            element: Term::intrinsic(Intrinsic::NatType),
            items: vec![lit(7)]
        }),
    );

    // `slice(cons(7, xs), 1, 0) = []` — the empty-window identity, which a count decides on the length alone.
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::list_slice(
                Term::intrinsic(Intrinsic::NatType),
                cons,
                lit(1),
                lit(0),
                qed(),
            ))
        ),
        Subterm::Intrinsic(Intrinsic::List {
            element: Term::intrinsic(Intrinsic::NatType),
            items: Vec::new()
        }),
    );
}

// `List/len` distributes over the monoid like `Bin/len`: a symbolic cons or append reduces its length to a `succ` spine instead of stalling.
#[test]
fn list_len_distributes_over_cons_and_append() {
    let mut context = context();
    let xs_binder = context.fresh(Some("xs"));
    let xs = Term::free_var(&xs_binder);
    // `1 + len(xs)`, the shape both symbolic cases reduce to.
    let succ_len = |context: &mut Context| {
        reduced(
            context,
            Term::intrinsic(Intrinsic::nat_add(
                lit(1),
                Term::intrinsic(Intrinsic::list_len(
                    Term::intrinsic(Intrinsic::NatType),
                    xs.clone(),
                )),
            )),
        )
    };

    // Literal: `len([1, 2, 3]) = 3`.
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::list_len(
                Term::intrinsic(Intrinsic::NatType),
                Term::intrinsic(Intrinsic::List {
                    element: Term::intrinsic(Intrinsic::NatType),
                    items: vec![lit(1), lit(2), lit(3)]
                })
            )),
        ),
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(3usize))),
    );

    // `len(cons(7, xs)) = 1 + len(xs)`.
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::list_len(
                Term::intrinsic(Intrinsic::NatType),
                list_cons_seven(&xs)
            ))
        ),
        succ_len(&mut context),
    );

    // `len(append(xs, 9)) = 1 + len(xs)`.
    let appended = Term::intrinsic(Intrinsic::list_append(
        Term::intrinsic(Intrinsic::NatType),
        xs.clone(),
        lit(9),
    ));
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::list_len(
                Term::intrinsic(Intrinsic::NatType),
                appended
            ))
        ),
        succ_len(&mut context),
    );
}

// The full slice is the identity even over a symbolic array: `slice(xs, 0, len xs) = xs` (the `List` twin of `BinSlice`'s full-window identity).
#[test]
fn list_slice_full_window_is_identity() {
    let mut context = context();
    let xs_binder = context.fresh(Some("xs"));
    let xs = Term::free_var(&xs_binder);
    let len = Term::intrinsic(Intrinsic::list_len(
        Term::intrinsic(Intrinsic::NatType),
        xs.clone(),
    ));
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::list_slice(
                Term::intrinsic(Intrinsic::NatType),
                xs.clone(),
                lit(0),
                len,
                qed(),
            )),
        ),
        reduced(&mut context, xs.clone()),
    );
}

// `Bin/eql` decides definitional equality through the spine peel: reflexivity and a peeled-equal pair fold to `true`, a definite byte/length clash to `false`, and a genuinely undecided pair stays neutral.
#[test]
fn bin_eql_decides_structurally() {
    let mut context = context();
    let x_binder = context.fresh(Some("x"));
    let y_binder = context.fresh(Some("y"));
    let bin =
        |bytes: Vec<u8>| Term::intrinsic(Intrinsic::Bin(Grain::X, PackedBin::from_bytes(bytes)));
    let x = Term::free_var(&x_binder);

    // Reflexivity over a symbolic value: `eql(x, x) = true`.
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::bin_eql(Grain::X, x.clone(), x.clone()))
        ),
        Subterm::Intrinsic(Intrinsic::Bool(true)),
    );

    // Literal decisions: equal folds true, unequal folds false.
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::bin_eql(
                Grain::X,
                bin(vec![1, 2]),
                bin(vec![1, 2])
            ))
        ),
        Subterm::Intrinsic(Intrinsic::Bool(true)),
    );
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::bin_eql(
                Grain::X,
                bin(vec![1, 2]),
                bin(vec![1, 3])
            ))
        ),
        Subterm::Intrinsic(Intrinsic::Bool(false)),
    );

    // A first-byte clash decides `false` even past a shared symbolic tail: `eql([1] ++ x, [2] ++ x) = false`.
    let lhs = Term::intrinsic(Intrinsic::bin_concat(Grain::X, [bin(vec![1]), x.clone()]));
    let rhs = Term::intrinsic(Intrinsic::bin_concat(Grain::X, [bin(vec![2]), x.clone()]));
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::bin_eql(Grain::X, lhs, rhs))
        ),
        Subterm::Intrinsic(Intrinsic::Bool(false)),
    );

    // Distinct variables are undecidable: `eql(x, y)` stays neutral.
    let y = Term::free_var(&y_binder);
    assert!(matches!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::bin_eql(Grain::X, x, y))
        ),
        Subterm::Intrinsic(Intrinsic::BinEql(Grain::X, ..)),
    ));
}

#[test]
fn bits_reduce_through_symbolic_free_monoid_spines() {
    let mut context = context();
    let tail_binder = context.fresh(Some("tail"));
    let bits = |values: &[bool]| {
        Term::intrinsic(Intrinsic::Bin(
            Grain::B,
            PackedBin::from_bits(values.iter().copied()),
        ))
    };
    let tail = Term::free_var(&tail_binder);
    let cons = Term::intrinsic(Intrinsic::bin_concat(
        Grain::B,
        [bits(&[true]), tail.clone()],
    ));

    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::bin_get(Grain::B, cons.clone(), lit(0), qed()))
        ),
        Subterm::Intrinsic(Intrinsic::Bool(true)),
    );
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::bin_slice(
                Grain::B,
                cons.clone(),
                lit(0),
                lit(1),
                qed(),
            ))
        ),
        Term::unwrap_or_clone(bits(&[true])),
    );
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::bin_len(Grain::B, cons.clone()))
        ),
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::nat_add(
                lit(1),
                Term::intrinsic(Intrinsic::bin_len(Grain::B, tail.clone())),
            ))
        ),
    );

    let false_cons = Term::intrinsic(Intrinsic::bin_concat(
        Grain::B,
        [bits(&[false]), tail.clone()],
    ));
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::bin_eql(Grain::B, cons, false_cons))
        ),
        Subterm::Intrinsic(Intrinsic::Bool(false)),
    );
    assert_eq!(
        reduced(
            &mut context,
            Term::intrinsic(Intrinsic::bin_concat(
                Grain::B,
                [bits(&[]), tail.clone(), bits(&[])],
            ))
        ),
        Term::unwrap_or_clone(tail),
    );
}
