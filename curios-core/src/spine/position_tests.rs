//! Positions: a read through a window, a window of a window, and the declining side of each.

use super::{test_support::*, *};

fn list_window(base: Term, start: Term, count: Term) -> Term {
    Term::intrinsic(Intrinsic::list_slice(
        sym(1000, "T"),
        base,
        start,
        count,
        sym(9_999, "qed"),
    ))
}

fn list_get(list: Term, index: Term) -> Intrinsic {
    Intrinsic::ListGet {
        element: sym(1000, "T"),
        list,
        index,
        in_range: sym(9_998, "in_range"),
    }
}

fn bin_get(grain: Grain, bin: Term, index: Term) -> Intrinsic {
    Intrinsic::BinGet {
        grain,
        bin,
        index,
        in_range: sym(9_998, "in_range"),
    }
}

// A position inside a window is the position it names in the base: `get(slice(xs, s, l), i)` reads `xs` at `s + i`. Decided as a comparison, since a rewritten node would owe a bound no term in hand proves; the absolute positions are compared as numbers, so a commuted sum is the same position, and a window of a window nests the same way. Both carriers, since a copied arm is where the two would drift.
#[test]
fn peel_position_decides_a_position_through_a_window_equal() {
    let (xs, s, l, i, t) = (
        sym(0, "xs"),
        sym(1, "s"),
        sym(2, "l"),
        sym(3, "i"),
        sym(4, "t"),
    );
    let window = list_window(xs.clone(), s.clone(), l.clone());

    let inside = list_get(window.clone(), i.clone());
    let named = list_get(xs.clone(), add(i.clone(), s.clone()));
    assert!(
        matches!(peel_position(&inside, &named), Some(Peel::Equal)),
        "`get(slice(xs, s, l), i)` is `get(xs, i + s)`"
    );

    let nested = list_get(list_window(window, t.clone(), i.clone()), i.clone());
    let deep = list_get(xs.clone(), add(add(s.clone(), t.clone()), i.clone()));
    assert!(
        matches!(peel_position(&nested, &deep), Some(Peel::Equal)),
        "a window of a window adds both starts"
    );

    let bytes_window = Term::intrinsic(Intrinsic::bin_slice(
        Grain::X,
        xs.clone(),
        s.clone(),
        l,
        sym(9_999, "qed"),
    ));
    assert!(
        matches!(
            peel_position(
                &bin_get(Grain::X, bytes_window, i.clone()),
                &bin_get(Grain::X, xs, add(s, i)),
            ),
            Some(Peel::Equal)
        ),
        "the packed carrier reads a position the same way"
    );
}

// The declining side: a position one past the named one, and the same position of another base, may still hold one element, so neither clashes; and two grains are not one carrier's pair at all.
#[test]
fn peel_position_declines_an_unlike_position_or_base_without_clashing() {
    let (xs, ys, s, l) = (sym(0, "xs"), sym(1, "ys"), sym(2, "s"), sym(3, "l"));
    let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
    let inside = list_get(list_window(xs.clone(), s.clone(), l), zero);

    let past = Term::intrinsic(Intrinsic::Nat(nat_of(1, s.clone())));
    assert!(
        matches!(
            peel_position(&inside, &list_get(xs.clone(), past)),
            Some(Peel::Stuck)
        ),
        "position `s + 1` is not the window's first"
    );
    assert!(
        matches!(
            peel_position(&inside, &list_get(ys, s.clone())),
            Some(Peel::Stuck)
        ),
        "another base is not this one"
    );
    assert!(
        peel_position(
            &bin_get(Grain::X, xs.clone(), s.clone()),
            &bin_get(Grain::B, xs, s)
        )
        .is_none(),
        "two grains are two carriers"
    );
}

// A window of a window is the window it names in the root, which the prefix step decides by reading both spans from the root; the near miss differs by one in its start and declines.
#[test]
fn peel_list_decides_a_window_of_a_window_against_the_window_it_names() {
    let (xs, s, l, t, m) = (
        sym(0, "xs"),
        sym(1, "s"),
        sym(2, "l"),
        sym(3, "t"),
        sym(4, "m"),
    );
    let nested = list_window(list_window(xs.clone(), s.clone(), l), t.clone(), m.clone());
    let named = list_window(xs.clone(), add(s.clone(), t.clone()), m.clone());
    assert!(
        matches!(
            peel_list(as_intrinsic(&nested), as_intrinsic(&named)),
            Some(Peel::Equal)
        ),
        "`slice(slice(xs, s, l), t, m)` is `slice(xs, s + t, m)`"
    );

    let shifted = list_window(xs, Term::intrinsic(Intrinsic::Nat(nat_of(1, add(s, t)))), m);
    assert!(
        matches!(
            peel_list(as_intrinsic(&nested), as_intrinsic(&shifted)),
            Some(Peel::Stuck)
        ),
        "a start one past the named one is another window"
    );
}

fn list_cat(operands: Vec<Term>) -> Term {
    Term::intrinsic(Intrinsic::ListConcat {
        element: sym(1000, "T"),
        operands,
    })
}

fn list_len(list: Term) -> Term {
    Term::intrinsic(Intrinsic::ListLen {
        element: sym(1000, "T"),
        list,
    })
}

// A position inside an operand of a concatenation is that operand's position: `get(xs ++ ys, i)` reads `xs` at `i`, and `get(xs ++ ys ++ zs, len(xs) + i)` reads `ys` at `i`. The concatenation's own bound reaches past the operand, so this is a comparison and never a reduction — the read of the operand is typed by its own bound, which places the position inside it at every well-typed instantiation. A window of an operand is decided the same way, by the prefix step.
#[test]
fn peel_position_decides_a_position_inside_an_operand_of_a_concatenation_equal() {
    let (xs, ys, zs, i) = (sym(0, "xs"), sym(1, "ys"), sym(2, "zs"), sym(3, "i"));

    let left = list_get(list_cat(vec![xs.clone(), ys.clone()]), i.clone());
    assert!(
        matches!(
            peel_position(&left, &list_get(xs.clone(), i.clone())),
            Some(Peel::Equal)
        ),
        "`get(xs ++ ys, i)` is `get(xs, i)`"
    );

    let middle = list_get(
        list_cat(vec![xs.clone(), ys.clone(), zs]),
        add(i.clone(), list_len(xs.clone())),
    );
    assert!(
        matches!(
            peel_position(&list_get(ys, i.clone()), &middle),
            Some(Peel::Equal)
        ),
        "`get(ys, i)` is `get(xs ++ ys ++ zs, i + len(xs))`, whichever side the concatenation is on"
    );

    let packed = Term::intrinsic(Intrinsic::BinConcat {
        grain: Grain::X,
        operands: vec![xs.clone(), sym(4, "b")],
    });
    assert!(
        matches!(
            peel_position(
                &bin_get(Grain::X, packed, i.clone()),
                &bin_get(Grain::X, xs.clone(), i.clone())
            ),
            Some(Peel::Equal)
        ),
        "the packed carrier reads an operand the same way"
    );

    let (s, n) = (sym(5, "s"), sym(6, "n"));
    let through = list_window(
        list_cat(vec![xs.clone(), sym(1, "ys")]),
        s.clone(),
        n.clone(),
    );
    let within = list_window(xs, s, n);
    assert!(
        matches!(
            peel_list(as_intrinsic(&through), as_intrinsic(&within)),
            Some(Peel::Equal)
        ),
        "`slice(xs ++ ys, s, n)` is `slice(xs, s, n)`"
    );
}

// The declining side: an operand read at another offset than where it begins, a position one past, and a base the concatenation does not hold may each still hold one element, so none clashes.
#[test]
fn peel_position_declines_an_operand_read_at_another_offset() {
    let (xs, ys, zs, i) = (sym(0, "xs"), sym(1, "ys"), sym(2, "zs"), sym(3, "i"));
    let joined = list_cat(vec![xs.clone(), ys.clone()]);

    assert!(
        matches!(
            peel_position(
                &list_get(joined.clone(), i.clone()),
                &list_get(ys, i.clone())
            ),
            Some(Peel::Stuck)
        ),
        "`ys` begins at `len(xs)`, not at `0`"
    );

    let past = Term::intrinsic(Intrinsic::Nat(nat_of(1, i.clone())));
    assert!(
        matches!(
            peel_position(&list_get(joined.clone(), past), &list_get(xs, i.clone())),
            Some(Peel::Stuck)
        ),
        "position `i + 1` is not `i`"
    );
    assert!(
        matches!(
            peel_position(&list_get(joined, i.clone()), &list_get(zs, i)),
            Some(Peel::Stuck)
        ),
        "a base the concatenation does not hold is not one of its operands"
    );
}
