//! Every peel verdict and open fold law, checked at every closed instantiation.

use {
    crate::{Free, Intrinsic, Nat, Peel, Subterm, Term, peel_bin, peel_list, peel_nat_terms},
    curios_num::Integer,
    curios_utilities::Grain,
};

use super::test_support::*;

// Soundness gate for the peel's own verdicts over values, which nothing stated before this: `Nat::cancel_common` decides all three, and the perimeter grades the law behind them argued in code comments only.
//
// Each verdict is believed by a different consumer, so each has its own obligation. A `Peel::Equal` reaches conversion as a definitional equation, and congruence carries a false one to `False`. A `Peel::Clash` reaches inversion as *impossible*, which excuses an omitted arm — the vacuous-elimination route. `Peel::Continue` is the one with no property stated anywhere, and it needs the strongest: the caller compares the residuals and reports *their* verdict as the original pair's, so the residuals must be equi-satisfiable with the pair they replaced, not merely implied by it. A residual pair that disagreed where the originals agreed would turn a later clash into a clash on the originals.
//
// So each verdict is checked against ground truth at every closed instantiation of its symbols, which is the only thing that can distinguish a valid equation from a plausible one. The grid reaches what cancelling *summands* newly decides rather than the successor spine alone: a commuted sum, a summand carried at multiplicity two, a floor surviving over shared summands, and two spellings of one number that share no summand syntactically.
//
// It reaches the **floorless** pairs too, which is the coverage widening the peel's gate to a sum spine added. Those are the pairs no `Intrinsic::Nat` carrier can express — `(x + y) + z` reduces to a bare `NatAdd`, not to a successor floor — and the reassociation among them is the equation the window-fusion bound rests on. `Peel::Stuck` is now *reachable* and carries its own case: a floorless pair sharing no summand comes back from `cancel_common` untouched, and returning it as `Continue` would re-enter the same congruence on the same terms forever.
#[test]
fn every_nat_peel_verdict_holds_at_every_closed_instantiation() {
    let (first, second, third) = (
        Free::local(0, Some("x")),
        Free::local(1, Some("y")),
        Free::local(2, Some("z")),
    );
    let (x, y, z) = (
        Term::free_var(&first),
        Term::free_var(&second),
        Term::free_var(&third),
    );

    let value_at = |term: &Term, a: u32, b: u32, c: u32| {
        let closed = at(term.clone(), &first, lit(a));
        let closed = at(closed, &second, lit(b));
        let closed = fold(at(closed, &third, lit(c)));
        let (floor, inner) = Nat::decompose(&closed);
        assert!(Nat::is_zero(&inner), "a closed Nat folds to a literal");
        floor
    };

    let cases = [
        (
            "x + y + 1 ~ y + x + 1",
            fold(plus(plus(x.clone(), y.clone()), lit(1))),
            fold(plus(plus(y.clone(), x.clone()), lit(1))),
        ),
        (
            "x + 2 ~ x + 1",
            fold(plus(x.clone(), lit(2))),
            fold(plus(x.clone(), lit(1))),
        ),
        (
            "x + 1 ~ y + 1",
            fold(plus(x.clone(), lit(1))),
            fold(plus(y.clone(), lit(1))),
        ),
        (
            "x + x + 1 ~ x + 1",
            fold(plus(plus(x.clone(), x.clone()), lit(1))),
            fold(plus(x.clone(), lit(1))),
        ),
        (
            "x + y + 3 ~ y + 1",
            fold(plus(plus(x.clone(), y.clone()), lit(3))),
            fold(plus(y.clone(), lit(1))),
        ),
        (
            "x + x + y + 1 ~ x + y + 1",
            fold(plus(plus(plus(x.clone(), x.clone()), y.clone()), lit(1))),
            fold(plus(plus(x.clone(), y.clone()), lit(1))),
        ),
        ("0 ~ x + 1", lit(0), fold(plus(x.clone(), lit(1)))),
        (
            "2·x + 1 ~ x + x + 1",
            fold(plus(scaled(2, x.clone()), lit(1))),
            fold(plus(plus(x.clone(), x.clone()), lit(1))),
        ),
        // The floorless pairs. The first is the equation window fusion's bound rests on, and the one the carrier gate used to hide: both sides reduce to a bare `NatAdd`, so no `Intrinsic::Nat` ever carried them to the cancellation.
        (
            "x + y + z ~ x + (y + z)",
            fold(plus(plus(x.clone(), y.clone()), z.clone())),
            fold(plus(x.clone(), plus(y.clone(), z.clone()))),
        ),
        (
            "x + y ~ y + x",
            fold(plus(x.clone(), y.clone())),
            fold(plus(y.clone(), x.clone())),
        ),
        // A floored side against a floorless one, which the carrier gate could not admit either: the shared summands cancel and the floor is what is left to decide on.
        (
            "x + y + 1 ~ x + y",
            fold(plus(plus(x.clone(), y.clone()), lit(1))),
            fold(plus(x.clone(), y.clone())),
        ),
        (
            "x + y ~ x + z",
            fold(plus(x.clone(), y.clone())),
            fold(plus(x.clone(), z.clone())),
        ),
        // Nothing shared and no floor to strip: the pair comes back untouched, and declining is the only answer that terminates.
        ("x + y ~ z", fold(plus(x.clone(), y.clone())), z.clone()),
    ];

    let (mut equal, mut clash, mut carried, mut stuck) = (0, 0, 0, 0);

    for (label, left, right) in cases {
        let peel = peel_nat_terms(&left, &right).expect("a `Nat`-shaped pair");

        match &peel {
            Peel::Equal => equal += 1,
            Peel::Clash => clash += 1,
            Peel::Continue(..) => carried += 1,
            Peel::Stuck => stuck += 1,
        }

        for a in [0u32, 1, 2, 5] {
            for b in [0u32, 1, 2, 5] {
                for c in [0u32, 1, 2, 5] {
                    let agree = value_at(&left, a, b, c) == value_at(&right, a, b, c);

                    match &peel {
                        Peel::Equal => assert!(
                            agree,
                            "`{label}` was decided equal but differs at x = {a}, y = {b}, z = {c}"
                        ),
                        Peel::Clash => assert!(
                            !agree,
                            "`{label}` was decided impossible but holds at x = {a}, y = {b}, z = {c}"
                        ),
                        Peel::Continue(residual_left, residual_right) => assert_eq!(
                            value_at(residual_left, a, b, c) == value_at(residual_right, a, b, c),
                            agree,
                            "`{label}`'s residuals disagree with the pair they replaced at x = {a}, y = {b}, z = {c}",
                        ),
                        // A declined pair claims nothing about its values, so there is nothing to check against ground truth — the obligation it carries is termination, and the count below is what holds the case in the grid.
                        Peel::Stuck => {}
                    }
                }
            }
        }
    }

    // Every verdict above holds vacuously of a grid that reaches only one of them, and `Continue` is the one a shape falls to when nothing fires — so a grid that decided nothing would pass while checking nothing. This is the count that says otherwise, and it is an assertion rather than a comment because the perimeter's own record is that inert rules are what hide defects.
    // `2·x + 1 ~ x + x + 1` moved from `Continue` to `Equal` when the sum normal form began merging like terms: both sides now *reduce* to `2·x + 1`, so the peel has nothing left to carry.
    assert_eq!(
        (equal, clash, carried, stuck),
        (4, 4, 4, 1),
        "the grid stopped reaching every peel verdict",
    );
}

// Soundness gate for the `Bin` peel's verdicts over values — the `Bin` half of what `every_nat_peel_verdict_holds_at_every_closed_instantiation` states for `Nat`, written because the perimeter graded these laws argued in code comments only. The obligations are the same three. A `Peel::Equal` reaches conversion as a definitional equation, and congruence carries a false one to `False`. A `Peel::Clash` reaches inversion as *impossible*, which excuses an omitted arm — the vacuous-elimination route. A `Peel::Continue`'s residuals must be equi-satisfiable with the pair they replaced, since the caller compares the residuals and reports their verdict as the originals'. `Peel::Stuck` promises nothing and is only tallied.
//
// The shapes reach the laws the code comments assert and nothing else stated: symbolic chunks cancelling by syntactic equality with a byte clash surviving past them, window fusion across a shared seam (`slice(w, s, l₁) ++ slice(w, s + l₁, l₂) = slice(w, s, l₁ + l₂)`), the empty-window drop (`slice(w, i, 0)` vanishing), append-as-concatenation (`append(b, c) = b ++ append(x[], c)`), and a near-miss control beside each: windows meeting at no seam must not fuse, and a one-byte symbolic cons against the identity stays undecided. Ground truth is the folded value at every closed instantiation of the symbols — instantiations respect `/sys/slice`'s `s + l <= len(b)` precondition, since a program outside them cannot be written, and that typing fact is exactly what makes the window laws unconditional.
//
// Mutation-checked: fusing two windows of one base without the seam check (`*seam == lo` dropped from `push`) turns the no-seam control into a false `Equal` and this grid fails it at the first anchor whose seam bytes differ. The tally is the anti-inertness assertion the perimeter asks of a sole-reach fixture: `Stuck` is where a pair falls when nothing fires, so a grid that decided nothing would otherwise pass while checking nothing.
#[test]
fn every_bin_peel_verdict_holds_at_every_closed_instantiation() {
    let bin_left = Free::local(0, Some("x"));
    let bin_right = Free::local(1, Some("y"));
    let byte_free = Free::local(2, Some("c"));
    let anchor_free = Free::local(3, Some("w"));
    let x = Term::free_var(&bin_left);
    let y = Term::free_var(&bin_right);
    let w = Term::free_var(&anchor_free);
    let c = Term::free_var(&byte_free);

    let cat = |parts: Vec<Term>| {
        Term::intrinsic(Intrinsic::BinConcat {
            grain: Grain::X,
            operands: parts,
        })
    };
    let window = |lo: u32, hi: u32| {
        Term::intrinsic(Intrinsic::bin_slice(
            Grain::X,
            w.clone(),
            lit(lo),
            lit(hi),
            qed(),
        ))
    };
    let chunk = Term::intrinsic(Intrinsic::bin_append(Grain::X, run_bytes(&[]), c.clone()));

    let cases = [
        (
            "x ++ x[05] ~ x ++ x[05]",
            cat(vec![x.clone(), run_bytes(&[5])]),
            cat(vec![x.clone(), run_bytes(&[5])]),
        ),
        (
            "x[05] ++ x ~ x[09] ++ x",
            cat(vec![run_bytes(&[5]), x.clone()]),
            cat(vec![run_bytes(&[9]), x.clone()]),
        ),
        (
            "x ++ x[05] ~ x ++ x[09]",
            cat(vec![x.clone(), run_bytes(&[5])]),
            cat(vec![x.clone(), run_bytes(&[9])]),
        ),
        (
            "x[05] ++ x ~ x[05] ++ y",
            cat(vec![run_bytes(&[5]), x.clone()]),
            cat(vec![run_bytes(&[5]), y.clone()]),
        ),
        (
            "x[0509] ~ x[05] ++ x",
            run_bytes(&[5, 9]),
            cat(vec![run_bytes(&[5]), x.clone()]),
        ),
        (
            "x[05] ++ x ~ x ++ x[05]",
            cat(vec![run_bytes(&[5]), x.clone()]),
            cat(vec![x.clone(), run_bytes(&[5])]),
        ),
        (
            "append(x, c) ~ x ++ append(x[], c)",
            Term::intrinsic(Intrinsic::bin_append(Grain::X, x.clone(), c.clone())),
            cat(vec![x.clone(), chunk.clone()]),
        ),
        (
            "slice(w, 0, 2) ++ slice(w, 2, 2) ~ slice(w, 0, 4)",
            cat(vec![window(0, 2), window(2, 2)]),
            window(0, 4),
        ),
        // The near miss: the second window starts at 3 where the first ends at 0 + 2, so nothing fuses and the two sides genuinely differ.
        (
            "slice(w, 0, 2) ++ slice(w, 3, 1) ~ slice(w, 0, 3)",
            cat(vec![window(0, 2), window(3, 1)]),
            window(0, 3),
        ),
        (
            "slice(w, 1, 0) ++ x ~ x ++ slice(w, 2, 0)",
            cat(vec![window(1, 0), x.clone()]),
            cat(vec![x.clone(), window(2, 0)]),
        ),
        (
            "append(x[], c) ++ x ~ append(x[], c) ++ y",
            cat(vec![chunk.clone(), x.clone()]),
            cat(vec![chunk.clone(), y.clone()]),
        ),
        ("append(x[], c) ~ x[]", chunk.clone(), run_bytes(&[])),
    ];

    let as_intrinsic = |term: &Term| match &**term {
        Subterm::Intrinsic(intrinsic) => intrinsic.clone(),
        other => unreachable!("every side of the grid is an intrinsic, got {other:?}"),
    };

    let runs: [&[u8]; 5] = [&[], &[5], &[9], &[9, 8], &[1, 1]];
    let anchors: [&[u8]; 2] = [&[9, 8, 7, 6], &[9, 8, 7, 7, 3]];

    let (mut equal, mut clash, mut carried, mut stuck) = (0, 0, 0, 0);

    for (label, left, right) in &cases {
        let peel =
            peel_bin(&as_intrinsic(left), &as_intrinsic(right)).expect("two Bin values peel");

        match &peel {
            Peel::Equal => equal += 1,
            Peel::Clash => clash += 1,
            Peel::Continue(..) => carried += 1,
            Peel::Stuck => stuck += 1,
        }

        for left_run in runs {
            for right_run in runs {
                for byte_value in [0u8, 7, 255] {
                    for anchor in anchors {
                        let close = |term: &Term| {
                            let term = at(term.clone(), &bin_left, run_bytes(left_run));
                            let term = at(term, &bin_right, run_bytes(right_run));
                            let term = at(
                                term,
                                &byte_free,
                                Term::intrinsic(Intrinsic::Byte(byte_value)),
                            );
                            bin_value(at(term, &anchor_free, run_bytes(anchor)))
                        };

                        let agree = close(left) == close(right);

                        match &peel {
                            Peel::Equal => assert!(
                                agree,
                                "`{label}` was decided equal but differs at x = {left_run:?}, y = {right_run:?}, c = {byte_value}, w = {anchor:?}",
                            ),
                            Peel::Clash => assert!(
                                !agree,
                                "`{label}` was decided impossible but holds at x = {left_run:?}, y = {right_run:?}, c = {byte_value}, w = {anchor:?}",
                            ),
                            Peel::Continue(residual_left, residual_right) => assert_eq!(
                                close(residual_left) == close(residual_right),
                                agree,
                                "`{label}`'s residuals disagree with the pair they replaced at x = {left_run:?}, y = {right_run:?}, c = {byte_value}, w = {anchor:?}",
                            ),
                            Peel::Stuck => {}
                        }
                    }
                }
            }
        }
    }

    assert_eq!(
        (equal, clash, carried, stuck),
        (4, 2, 3, 3),
        "the grid stopped reaching every peel verdict",
    );
}

// The `List` half of the grid above, separate because the two carriers differ exactly where a copied rule would be wrong: `List` literals hold *terms*, so two leading runs whose heads differ syntactically are NOT a clash — the elements may still be convertible — while a leftover run against the exhausted identity is still a definite length clash whatever its elements are. The first shape pins that difference over values: `[a + b]` and `[b + a]` denote one list at every instantiation, so the `Bin` byte-disagreement rule applied here would be a false impossibility, which is the vacuous-elimination route to `False`. Mutation-checked: clashing two differing literal heads the way `peel_bin` does fails that shape at its first instantiation. The other shapes and the tally mirror the `Bin` grid's obligations: append-as-concatenation with a symbolic element, window fusion over the element carrier, a genuine length clash, and residual equi-satisfiability.
#[test]
fn every_list_peel_verdict_holds_at_every_closed_instantiation() {
    let list_left = Free::local(0, Some("xs"));
    let list_right = Free::local(1, Some("ys"));
    let nat_a = Free::local(2, Some("a"));
    let nat_b = Free::local(3, Some("b"));
    let anchor_free = Free::local(4, Some("ws"));
    let xs = Term::free_var(&list_left);
    let ys = Term::free_var(&list_right);
    let a = Term::free_var(&nat_a);
    let b = Term::free_var(&nat_b);
    let ws = Term::free_var(&anchor_free);

    let elem = symbol(1000, "T");
    let cat = |parts: Vec<Term>| {
        Term::intrinsic(Intrinsic::ListConcat {
            element: elem.clone(),
            operands: parts,
        })
    };
    let one = |element: Term| {
        Term::intrinsic(Intrinsic::List {
            element: elem.clone(),
            items: vec![element],
        })
    };
    let window = |lo: u32, hi: u32| {
        Term::intrinsic(Intrinsic::list_slice(
            elem.clone(),
            ws.clone(),
            lit(lo),
            lit(hi),
            qed(),
        ))
    };

    let cases = [
        (
            "[a + b] ++ xs ~ [b + a] ++ xs",
            cat(vec![one(plus(a.clone(), b.clone())), xs.clone()]),
            cat(vec![one(plus(b.clone(), a.clone())), xs.clone()]),
        ),
        (
            "xs ++ [7] ~ xs ++ []",
            cat(vec![xs.clone(), nat_list(&[7])]),
            cat(vec![xs.clone(), nat_list(&[])]),
        ),
        (
            "[7] ++ xs ~ [7] ++ ys",
            cat(vec![nat_list(&[7]), xs.clone()]),
            cat(vec![nat_list(&[7]), ys.clone()]),
        ),
        (
            "append(xs, a) ~ xs ++ [a]",
            Term::intrinsic(Intrinsic::list_append(elem.clone(), xs.clone(), a.clone())),
            cat(vec![xs.clone(), one(a.clone())]),
        ),
        (
            "slice(ws, 0, 2) ++ slice(ws, 2, 2) ~ slice(ws, 0, 4)",
            cat(vec![window(0, 2), window(2, 2)]),
            window(0, 4),
        ),
        (
            "[7, 8] ~ [7] ++ xs",
            nat_list(&[7, 8]),
            cat(vec![nat_list(&[7]), xs.clone()]),
        ),
    ];

    let as_intrinsic = |term: &Term| match &**term {
        Subterm::Intrinsic(intrinsic) => intrinsic.clone(),
        other => unreachable!("every side of the grid is an intrinsic, got {other:?}"),
    };

    let runs: [&[u32]; 4] = [&[], &[8], &[7, 8], &[1, 2]];
    let anchors: [&[u32]; 2] = [&[9, 8, 7, 6], &[6, 6, 5, 4, 3]];

    let (mut equal, mut clash, mut carried, mut stuck) = (0, 0, 0, 0);

    for (label, left, right) in &cases {
        let peel =
            peel_list(&as_intrinsic(left), &as_intrinsic(right)).expect("two List values peel");

        match &peel {
            Peel::Equal => equal += 1,
            Peel::Clash => clash += 1,
            Peel::Continue(..) => carried += 1,
            Peel::Stuck => stuck += 1,
        }

        for left_run in runs {
            for right_run in runs {
                for first in [0u32, 1, 2] {
                    for second in [0u32, 1, 2] {
                        for anchor in anchors {
                            let close = |term: &Term| {
                                let term = at(term.clone(), &list_left, nat_list(left_run));
                                let term = at(term, &list_right, nat_list(right_run));
                                let term = at(term, &nat_a, lit(first));
                                let term = at(term, &nat_b, lit(second));
                                list_value(at(term, &anchor_free, nat_list(anchor)))
                            };

                            let agree = close(left) == close(right);

                            match &peel {
                                Peel::Equal => assert!(
                                    agree,
                                    "`{label}` was decided equal but differs at xs = {left_run:?}, ys = {right_run:?}, a = {first}, b = {second}",
                                ),
                                Peel::Clash => assert!(
                                    !agree,
                                    "`{label}` was decided impossible but holds at xs = {left_run:?}, ys = {right_run:?}, a = {first}, b = {second}",
                                ),
                                Peel::Continue(residual_left, residual_right) => assert_eq!(
                                    close(residual_left) == close(residual_right),
                                    agree,
                                    "`{label}`'s residuals disagree with the pair they replaced at xs = {left_run:?}, ys = {right_run:?}, a = {first}, b = {second}",
                                ),
                                Peel::Stuck => {}
                            }
                        }
                    }
                }
            }
        }
    }

    assert_eq!(
        (equal, clash, carried, stuck),
        (2, 1, 2, 1),
        "the grid stopped reaching every peel verdict",
    );
}

// Soundness gate for the open-term reduction laws the code comments beside the folds assert and nothing else stated: the subtraction borrow within the floor, the literal-factor distribution of `·` on either side, the full-window collapse `slice(b, 0, len(b)) = b`, the empty window `slice(b, i, 0) = x[]` over a symbolic base and start, the cons peels of `get` and `slice` over a symbolic tail and symbolic bounds, and the `len`/`map` homomorphisms over an append and a concatenation. Each case states the law's own reduct and holds the pair to two obligations. The open fold must land on exactly that reduct — so the law demonstrably fired, and where its comment claims, which is what keeps a case from passing vacuously when a rule stops firing. And the original and the reduct must agree as values at every closed instantiation, which is what a definitional equation promises and the only thing a false one fails. Instantiations respect the operations' `/sys` preconditions (`i < len` for `get`, `s + l <= len` for `slice`), since a program outside them cannot be written.
//
// `map`'s ground truth is structural rather than numeric: the mapped function stays a free symbol, so both sides fold to element runs of identical stuck applications, and their agreement says no element was dropped, duplicated or reordered — which is the whole of what the distribution law claims. Mutation-checked: misstating the append measure (`nat_add(2, base)` for `nat_add(1, base)` in the `BinLen` slot) fails the length case on both obligations at once.
#[test]
fn every_open_fold_law_preserves_the_value_at_every_closed_instantiation() {
    let nat_x = Free::local(0, Some("x"));
    let nat_y = Free::local(1, Some("y"));
    let bin_base = Free::local(2, Some("b"));
    let bin_tail = Free::local(3, Some("t"));
    let byte_free = Free::local(4, Some("c"));
    let nat_end = Free::local(5, Some("e"));
    let nat_start = Free::local(6, Some("s"));
    let list_base = Free::local(7, Some("xs"));
    let nat_elem = Free::local(8, Some("a"));
    let fun = Free::local(9, Some("f"));
    let bool_p = Free::local(10, Some("p"));
    let int_i = Free::local(11, Some("i"));
    let nat_z = Free::local(12, Some("z"));
    let x = Term::free_var(&nat_x);
    let z = Term::free_var(&nat_z);
    let p = Term::free_var(&bool_p);
    let i = Term::free_var(&int_i);
    let boolean = |value: bool| Term::intrinsic(Intrinsic::Bool(value));
    let integer = |value: i32| Term::intrinsic(Intrinsic::Int(Integer::from(value)));
    let bools = || {
        vec![
            vec![(&bool_p, boolean(false))],
            vec![(&bool_p, boolean(true))],
        ]
    };
    let ints = || {
        vec![
            vec![(&int_i, integer(-3))],
            vec![(&int_i, integer(0))],
            vec![(&int_i, integer(5))],
        ]
    };
    let nats = || {
        vec![
            vec![(&nat_x, lit(0))],
            vec![(&nat_x, lit(1))],
            vec![(&nat_x, lit(6))],
        ]
    };
    let y = Term::free_var(&nat_y);
    let b = Term::free_var(&bin_base);
    let t = Term::free_var(&bin_tail);
    let c = Term::free_var(&byte_free);
    let e = Term::free_var(&nat_end);
    let s = Term::free_var(&nat_start);
    let xs = Term::free_var(&list_base);
    let a = Term::free_var(&nat_elem);
    let f = Term::free_var(&fun);

    let sub = |left: Term, right: Term| Term::intrinsic(Intrinsic::nat_sub(left, right));
    let mul = |left: Term, right: Term| Term::intrinsic(Intrinsic::nat_mul(left, right));
    let cat = |parts: Vec<Term>| {
        Term::intrinsic(Intrinsic::BinConcat {
            grain: Grain::X,
            operands: parts,
        })
    };
    let bin_slice = |base: Term, start: Term, count: Term| {
        Term::intrinsic(Intrinsic::bin_slice(Grain::X, base, start, count, qed()))
    };
    let bin_get =
        |base: Term, index: Term| Term::intrinsic(Intrinsic::bin_get(Grain::X, base, index, qed()));
    let bin_len = |base: Term| Term::intrinsic(Intrinsic::bin_len(Grain::X, base));
    let chunk = Term::intrinsic(Intrinsic::bin_append(Grain::X, run_bytes(&[]), c.clone()));
    let elem = symbol(1000, "T");
    let list_append = |base: Term, element: Term| {
        Term::intrinsic(Intrinsic::list_append(elem.clone(), base, element))
    };
    let list_len = |base: Term| Term::intrinsic(Intrinsic::list_len(elem.clone(), base));
    let list_map = |base: Term| {
        Term::intrinsic(Intrinsic::list_map(
            elem.clone(),
            elem.clone(),
            base,
            f.clone(),
        ))
    };
    let byte = |value: u8| Term::intrinsic(Intrinsic::Byte(value));

    let cases = vec![
        (
            "(x + 5) - 3 = x + 2",
            sub(plus(x.clone(), lit(5)), lit(3)),
            plus(x.clone(), lit(2)),
            vec![
                vec![(&nat_x, lit(0))],
                vec![(&nat_x, lit(1))],
                vec![(&nat_x, lit(9))],
            ],
        ),
        (
            "(x + 1) - (x + 2) = 0",
            sub(plus(x.clone(), lit(1)), plus(x.clone(), lit(2))),
            lit(0),
            vec![vec![(&nat_x, lit(0))], vec![(&nat_x, lit(4))]],
        ),
        (
            "(x + y + 3) - (y + 1) = x + 2",
            sub(
                plus(plus(x.clone(), y.clone()), lit(3)),
                plus(y.clone(), lit(1)),
            ),
            plus(x.clone(), lit(2)),
            vec![
                vec![(&nat_x, lit(0)), (&nat_y, lit(0))],
                vec![(&nat_x, lit(2)), (&nat_y, lit(5))],
                vec![(&nat_x, lit(7)), (&nat_y, lit(1))],
            ],
        ),
        (
            "(x + 1) * 2 = x * 2 + 2",
            mul(plus(x.clone(), lit(1)), lit(2)),
            plus(mul(x.clone(), lit(2)), lit(2)),
            vec![vec![(&nat_x, lit(0))], vec![(&nat_x, lit(3))]],
        ),
        (
            "3 * (x + 2) = 3 * x + 6",
            mul(lit(3), plus(x.clone(), lit(2))),
            plus(mul(lit(3), x.clone()), lit(6)),
            vec![vec![(&nat_x, lit(0))], vec![(&nat_x, lit(3))]],
        ),
        // The unit and annihilation laws, on either side, and the nested literal factor — the three shapes the distribution above left neutral, each of which a first `*` theorem meets.
        (
            "x * 1 = x",
            mul(x.clone(), lit(1)),
            x.clone(),
            vec![vec![(&nat_x, lit(0))], vec![(&nat_x, lit(7))]],
        ),
        (
            "1 * (x + 2) = x + 2",
            mul(lit(1), plus(x.clone(), lit(2))),
            plus(x.clone(), lit(2)),
            vec![vec![(&nat_x, lit(0))], vec![(&nat_x, lit(7))]],
        ),
        (
            "(x + 2) * 0 = 0",
            mul(plus(x.clone(), lit(2)), lit(0)),
            lit(0),
            vec![vec![(&nat_x, lit(0))], vec![(&nat_x, lit(7))]],
        ),
        (
            "0 * x = 0",
            mul(lit(0), x.clone()),
            lit(0),
            vec![vec![(&nat_x, lit(0))], vec![(&nat_x, lit(7))]],
        ),
        (
            "2 * (3 * x) = 6 * x",
            mul(lit(2), mul(lit(3), x.clone())),
            mul(lit(6), x.clone()),
            vec![vec![(&nat_x, lit(0))], vec![(&nat_x, lit(5))]],
        ),
        (
            "(x * 2) * 3 = 6 * x",
            mul(mul(x.clone(), lit(2)), lit(3)),
            mul(lit(6), x.clone()),
            vec![vec![(&nat_x, lit(0))], vec![(&nat_x, lit(5))]],
        ),
        (
            "slice(b, 0, len(b)) = b",
            bin_slice(b.clone(), lit(0), bin_len(b.clone())),
            b.clone(),
            vec![
                vec![(&bin_base, run_bytes(&[]))],
                vec![(&bin_base, run_bytes(&[9, 8, 7]))],
            ],
        ),
        // Decided on the *length* alone now, where a `(start, end)` window had to compare two subjects — which is what a count buys.
        (
            "slice(b, e, 0) = x[]",
            bin_slice(b.clone(), e.clone(), lit(0)),
            run_bytes(&[]),
            vec![
                vec![(&bin_base, run_bytes(&[])), (&nat_end, lit(0))],
                vec![(&bin_base, run_bytes(&[9, 8, 7])), (&nat_end, lit(2))],
                vec![(&bin_base, run_bytes(&[9, 8, 7])), (&nat_end, lit(3))],
            ],
        ),
        (
            "slice(cons(c, b), 0, e + 1) = cons(c, x[]) ++ slice(b, 0, e)",
            bin_slice(
                cat(vec![chunk.clone(), b.clone()]),
                lit(0),
                plus(e.clone(), lit(1)),
            ),
            cat(vec![chunk.clone(), bin_slice(b.clone(), lit(0), e.clone())]),
            vec![
                vec![
                    (&bin_base, run_bytes(&[])),
                    (&byte_free, byte(7)),
                    (&nat_end, lit(0)),
                ],
                vec![
                    (&bin_base, run_bytes(&[8, 9])),
                    (&byte_free, byte(255)),
                    (&nat_end, lit(1)),
                ],
                vec![
                    (&bin_base, run_bytes(&[8, 9])),
                    (&byte_free, byte(0)),
                    (&nat_end, lit(2)),
                ],
            ],
        ),
        (
            // Advancing the start leaves the count alone, which is the reparameterisation paying for itself: peeling the base is invariant for the window's own length, so nothing about it has to be recomputed to move it.
            "slice(cons(c, b), s + 1, e) = slice(b, s, e)",
            bin_slice(
                cat(vec![chunk.clone(), b.clone()]),
                plus(s.clone(), lit(1)),
                e.clone(),
            ),
            bin_slice(b.clone(), s.clone(), e.clone()),
            vec![
                vec![
                    (&bin_base, run_bytes(&[8, 9])),
                    (&byte_free, byte(7)),
                    (&nat_start, lit(0)),
                    (&nat_end, lit(0)),
                ],
                vec![
                    (&bin_base, run_bytes(&[8, 9])),
                    (&byte_free, byte(7)),
                    (&nat_start, lit(0)),
                    (&nat_end, lit(2)),
                ],
                vec![
                    (&bin_base, run_bytes(&[8, 9])),
                    (&byte_free, byte(7)),
                    (&nat_start, lit(1)),
                    (&nat_end, lit(1)),
                ],
                vec![
                    (&bin_base, run_bytes(&[8, 9])),
                    (&byte_free, byte(7)),
                    (&nat_start, lit(2)),
                    (&nat_end, lit(0)),
                ],
            ],
        ),
        (
            "get(cons(c, x[]), 0) = c",
            bin_get(chunk.clone(), lit(0)),
            c.clone(),
            vec![
                vec![(&byte_free, byte(0))],
                vec![(&byte_free, byte(7))],
                vec![(&byte_free, byte(255))],
            ],
        ),
        (
            "get(cons(c, b), e + 1) = get(b, e)",
            bin_get(cat(vec![chunk.clone(), b.clone()]), plus(e.clone(), lit(1))),
            bin_get(b.clone(), e.clone()),
            vec![
                vec![
                    (&bin_base, run_bytes(&[8, 9])),
                    (&byte_free, byte(7)),
                    (&nat_end, lit(0)),
                ],
                vec![
                    (&bin_base, run_bytes(&[8, 9])),
                    (&byte_free, byte(7)),
                    (&nat_end, lit(1)),
                ],
            ],
        ),
        (
            "len(append(b, c)) = len(b) + 1",
            bin_len(Term::intrinsic(Intrinsic::bin_append(
                Grain::X,
                b.clone(),
                c.clone(),
            ))),
            plus(bin_len(b.clone()), lit(1)),
            vec![
                vec![(&bin_base, run_bytes(&[])), (&byte_free, byte(0))],
                vec![(&bin_base, run_bytes(&[9, 8])), (&byte_free, byte(255))],
            ],
        ),
        (
            "len(b ++ x[0509] ++ t) = len(b) + len(t) + 2",
            bin_len(cat(vec![b.clone(), run_bytes(&[5, 9]), t.clone()])),
            plus(plus(bin_len(b.clone()), bin_len(t.clone())), lit(2)),
            vec![
                vec![(&bin_base, run_bytes(&[])), (&bin_tail, run_bytes(&[]))],
                vec![
                    (&bin_base, run_bytes(&[7])),
                    (&bin_tail, run_bytes(&[1, 2])),
                ],
            ],
        ),
        (
            "map(append(xs, a), f) = append(map(xs, f), f(a))",
            list_map(list_append(xs.clone(), a.clone())),
            list_append(list_map(xs.clone()), Term::apply(f.clone(), [a.clone()])),
            vec![
                vec![(&list_base, nat_list(&[])), (&nat_elem, lit(5))],
                vec![(&list_base, nat_list(&[1, 2])), (&nat_elem, lit(5))],
            ],
        ),
        (
            "len(append(xs, a)) = len(xs) + 1",
            list_len(list_append(xs.clone(), a.clone())),
            plus(list_len(xs.clone()), lit(1)),
            vec![
                vec![(&list_base, nat_list(&[])), (&nat_elem, lit(5))],
                vec![(&list_base, nat_list(&[1, 2])), (&nat_elem, lit(5))],
            ],
        ),
        // The one-literal and identical-operand laws of `Bool`, the bitwise lattice on ℕ, and the ring `Int` — each the unit, absorber, idempotence or self-cancellation its operation has, taken after the two-literal fold declines.
        (
            "p && true = p",
            Term::intrinsic(Intrinsic::BoolAnd(p.clone(), boolean(true))),
            p.clone(),
            bools(),
        ),
        (
            "false && p = false",
            Term::intrinsic(Intrinsic::BoolAnd(boolean(false), p.clone())),
            boolean(false),
            bools(),
        ),
        (
            "p || p = p",
            Term::intrinsic(Intrinsic::BoolOr(p.clone(), p.clone())),
            p.clone(),
            bools(),
        ),
        (
            "p == p = true",
            Term::intrinsic(Intrinsic::BoolEql(p.clone(), p.clone())),
            boolean(true),
            bools(),
        ),
        (
            "p == false = xor(p, true)",
            Term::intrinsic(Intrinsic::BoolEql(p.clone(), boolean(false))),
            Term::intrinsic(Intrinsic::BoolXor(p.clone(), boolean(true))),
            bools(),
        ),
        (
            "xor(xor(p, true), true) = p",
            Term::intrinsic(Intrinsic::BoolXor(
                Term::intrinsic(Intrinsic::BoolXor(p.clone(), boolean(true))),
                boolean(true),
            )),
            p.clone(),
            bools(),
        ),
        (
            "xor(p, p) = false",
            Term::intrinsic(Intrinsic::BoolXor(p.clone(), p.clone())),
            boolean(false),
            bools(),
        ),
        (
            "and(x, 0) = 0",
            Term::intrinsic(Intrinsic::NatAnd(x.clone(), lit(0))),
            lit(0),
            nats(),
        ),
        (
            "or(0, x) = x",
            Term::intrinsic(Intrinsic::NatOr(lit(0), x.clone())),
            x.clone(),
            nats(),
        ),
        (
            "xor(x, x) = 0",
            Term::intrinsic(Intrinsic::NatXor(x.clone(), x.clone())),
            lit(0),
            nats(),
        ),
        (
            "and(x, x) = x",
            Term::intrinsic(Intrinsic::NatAnd(x.clone(), x.clone())),
            x.clone(),
            nats(),
        ),
        (
            "shl(x, 0) = x",
            Term::intrinsic(Intrinsic::NatShl(x.clone(), lit(0))),
            x.clone(),
            nats(),
        ),
        (
            "shr(0, x) = 0",
            Term::intrinsic(Intrinsic::NatShr(lit(0), x.clone())),
            lit(0),
            nats(),
        ),
        (
            "i + 0 = i",
            Term::intrinsic(Intrinsic::IntAdd(i.clone(), integer(0))),
            i.clone(),
            ints(),
        ),
        (
            "0 + i = i",
            Term::intrinsic(Intrinsic::IntAdd(integer(0), i.clone())),
            i.clone(),
            ints(),
        ),
        (
            "i - 0 = i",
            Term::intrinsic(Intrinsic::IntSub(i.clone(), integer(0))),
            i.clone(),
            ints(),
        ),
        (
            "i - i = 0",
            Term::intrinsic(Intrinsic::IntSub(i.clone(), i.clone())),
            integer(0),
            ints(),
        ),
        (
            "i * 1 = i",
            Term::intrinsic(Intrinsic::IntMul(i.clone(), integer(1))),
            i.clone(),
            ints(),
        ),
        (
            "0 * i = 0",
            Term::intrinsic(Intrinsic::IntMul(integer(0), i.clone())),
            integer(0),
            ints(),
        ),
        (
            "i == i = true",
            Term::intrinsic(Intrinsic::IntEql(i.clone(), i.clone())),
            boolean(true),
            ints(),
        ),
        (
            "i != i = false",
            Term::intrinsic(Intrinsic::IntNeq(i.clone(), i.clone())),
            boolean(false),
            ints(),
        ),
        // Subtraction's zero minuend and reassociation, and the division family's unconditional laws — the ones a symbolic part cannot falsify.
        ("0 - x = 0", sub(lit(0), x.clone()), lit(0), nats()),
        (
            "(x - y) - 3 = x - (y + 3)",
            sub(sub(x.clone(), y.clone()), lit(3)),
            sub(x.clone(), plus(y.clone(), lit(3))),
            vec![
                vec![(&nat_x, lit(0)), (&nat_y, lit(0))],
                vec![(&nat_x, lit(9)), (&nat_y, lit(2))],
                vec![(&nat_x, lit(4)), (&nat_y, lit(2))],
                vec![(&nat_x, lit(2)), (&nat_y, lit(9))],
            ],
        ),
        (
            "x / 1 = x",
            Term::intrinsic(Intrinsic::NatDiv {
                dividend: x.clone(),
                divisor: lit(1),
                non_zero: qed(),
            }),
            x.clone(),
            nats(),
        ),
        (
            "x % 1 = 0",
            Term::intrinsic(Intrinsic::NatRem {
                dividend: x.clone(),
                divisor: lit(1),
                non_zero: qed(),
            }),
            lit(0),
            nats(),
        ),
        (
            "0 / (x + 1) = 0",
            Term::intrinsic(Intrinsic::NatDiv {
                dividend: lit(0),
                divisor: plus(x.clone(), lit(1)),
                non_zero: qed(),
            }),
            lit(0),
            nats(),
        ),
        (
            "(x + 1) / (x + 1) = 1",
            Term::intrinsic(Intrinsic::NatDiv {
                dividend: plus(x.clone(), lit(1)),
                divisor: plus(x.clone(), lit(1)),
                non_zero: qed(),
            }),
            lit(1),
            nats(),
        ),
        (
            "(x + 1) % (x + 1) = 0",
            Term::intrinsic(Intrinsic::NatRem {
                dividend: plus(x.clone(), lit(1)),
                divisor: plus(x.clone(), lit(1)),
                non_zero: qed(),
            }),
            lit(0),
            nats(),
        ),
        // The free monoid's seam windows over symbolic operands, and the measure through a map.
        (
            "slice(b ++ t, 0, len(b)) = b",
            bin_slice(cat(vec![b.clone(), t.clone()]), lit(0), bin_len(b.clone())),
            b.clone(),
            vec![
                vec![(&bin_base, run_bytes(&[])), (&bin_tail, run_bytes(&[7]))],
                vec![
                    (&bin_base, run_bytes(&[1, 2])),
                    (&bin_tail, run_bytes(&[3, 4, 5])),
                ],
            ],
        ),
        (
            "slice(b ++ t, len(b), len(t)) = t",
            bin_slice(
                cat(vec![b.clone(), t.clone()]),
                bin_len(b.clone()),
                bin_len(t.clone()),
            ),
            t.clone(),
            vec![
                vec![(&bin_base, run_bytes(&[9])), (&bin_tail, run_bytes(&[]))],
                vec![
                    (&bin_base, run_bytes(&[1, 2])),
                    (&bin_tail, run_bytes(&[3, 4, 5])),
                ],
            ],
        ),
        (
            "len(map(xs, f)) = len(xs)",
            list_len(list_map(xs.clone())),
            list_len(xs.clone()),
            vec![
                vec![(&list_base, nat_list(&[]))],
                vec![(&list_base, nat_list(&[1, 2, 3]))],
            ],
        ),
        // The sum normal form as a linear combination: like terms merge by coefficient, and a literal distributes over a symbolic sum.
        (
            "x + x = 2 * x",
            plus(x.clone(), x.clone()),
            mul(lit(2), x.clone()),
            nats(),
        ),
        (
            "2 * x + 3 * x = 5 * x",
            plus(mul(lit(2), x.clone()), mul(lit(3), x.clone())),
            mul(lit(5), x.clone()),
            nats(),
        ),
        (
            "(x + y) * 2 = 2 * x + 2 * y",
            mul(plus(x.clone(), y.clone()), lit(2)),
            plus(mul(lit(2), x.clone()), mul(lit(2), y.clone())),
            vec![
                vec![(&nat_x, lit(0)), (&nat_y, lit(0))],
                vec![(&nat_x, lit(2)), (&nat_y, lit(5))],
                vec![(&nat_x, lit(7)), (&nat_y, lit(1))],
            ],
        ),
        (
            "(x + y + 1) + (x + 2) = 2 * x + y + 3",
            plus(
                plus(plus(x.clone(), y.clone()), lit(1)),
                plus(x.clone(), lit(2)),
            ),
            plus(plus(mul(lit(2), x.clone()), y.clone()), lit(3)),
            vec![
                vec![(&nat_x, lit(0)), (&nat_y, lit(0))],
                vec![(&nat_x, lit(2)), (&nat_y, lit(5))],
                vec![(&nat_x, lit(7)), (&nat_y, lit(1))],
            ],
        ),
        // Monomials: a product of symbols has one factor order, a product spine flattens, and a symbolic factor distributes over a symbolic sum.
        (
            "x * y = y * x",
            mul(x.clone(), y.clone()),
            mul(y.clone(), x.clone()),
            vec![
                vec![(&nat_x, lit(0)), (&nat_y, lit(3))],
                vec![(&nat_x, lit(2)), (&nat_y, lit(5))],
            ],
        ),
        (
            "(x * y) * z = x * (y * z)",
            mul(mul(x.clone(), y.clone()), z.clone()),
            mul(x.clone(), mul(y.clone(), z.clone())),
            vec![
                vec![(&nat_x, lit(1)), (&nat_y, lit(2)), (&nat_z, lit(3))],
                vec![(&nat_x, lit(4)), (&nat_y, lit(0)), (&nat_z, lit(3))],
            ],
        ),
        (
            "x * (y + z) = x * y + x * z",
            mul(x.clone(), plus(y.clone(), z.clone())),
            plus(mul(x.clone(), y.clone()), mul(x.clone(), z.clone())),
            vec![
                vec![(&nat_x, lit(0)), (&nat_y, lit(2)), (&nat_z, lit(3))],
                vec![(&nat_x, lit(4)), (&nat_y, lit(5)), (&nat_z, lit(6))],
            ],
        ),
        (
            "(x + 1) * (y + 2) = x * y + 2 * x + y + 2",
            mul(plus(x.clone(), lit(1)), plus(y.clone(), lit(2))),
            plus(
                plus(
                    plus(mul(x.clone(), y.clone()), mul(lit(2), x.clone())),
                    y.clone(),
                ),
                lit(2),
            ),
            vec![
                vec![(&nat_x, lit(0)), (&nat_y, lit(0))],
                vec![(&nat_x, lit(3)), (&nat_y, lit(7))],
            ],
        ),
    ];

    for (label, term, reduct, samples) in cases {
        assert_eq!(
            fold(term.clone()),
            fold(reduct.clone()),
            "`{label}`: the open fold did not land on the law's stated reduct",
        );

        for (index, sample) in samples.iter().enumerate() {
            let close = |term: &Term| {
                let mut closed = term.clone();
                for (binder, value) in sample {
                    closed = at(closed, binder, value.clone());
                }
                closed_value(closed)
            };

            assert_eq!(
                close(&term),
                close(&reduct),
                "`{label}` changed its value at closed instantiation {index}",
            );
        }
    }
}
