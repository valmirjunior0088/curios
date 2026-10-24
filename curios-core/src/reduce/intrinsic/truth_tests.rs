//! The truth table over two `Bool` terms' atoms: what it decides equal, what it leaves alone, and where it stops.

use {
    super::{decide_bool, test_support::*},
    crate::{Intrinsic, Nat, Term},
    curios_algebra::BOOL_ATOM_CAP,
};

fn and(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::BoolAnd(left, right))
}

fn or(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::BoolOr(left, right))
}

fn not(term: Term) -> Term {
    Term::intrinsic(Intrinsic::BoolXor(
        term,
        Term::intrinsic(Intrinsic::Bool(true)),
    ))
}

fn decided(this: &Term, that: &Term) -> bool {
    decide_bool(&mut Folding, this, that).expect("reduces")
}

// The laws no one node can see and no leaf set relates: De Morgan across the two connectives, absorption against a term that is no connective at all, and distribution of one over the other.
#[test]
fn a_law_across_connectives_is_decided_equal() {
    let (b, c, d) = (sym(0, "b"), sym(1, "c"), sym(2, "d"));

    assert!(
        decided(
            &not(and(b.clone(), c.clone())),
            &or(not(b.clone()), not(c.clone()))
        ),
        "De Morgan"
    );
    assert!(
        decided(&or(b.clone(), and(b.clone(), c.clone())), &b),
        "absorption, against the bare atom"
    );
    assert!(
        decided(&b, &and(b.clone(), or(b.clone(), c.clone()))),
        "and its dual, the bare atom first"
    );
    assert!(
        decided(
            &and(b.clone(), or(c.clone(), d.clone())),
            &or(and(b.clone(), c), and(b, d))
        ),
        "distribution"
    );
}

// A comparison and its dual are one atom at two polarities, so a tree that holds both is read as holding an atom beside its negation — here buried where no local law sees the pair. The control is the soundness half: one side holding the atom and the other its dual are *not* one value, and a table that read the two at one polarity would decide them equal.
#[test]
fn a_comparison_and_its_dual_are_one_atom_at_two_polarities() {
    let (x, y, c, d) = (sym(0, "x"), sym(1, "y"), sym(2, "c"), sym(3, "d"));
    let less = Term::intrinsic(Intrinsic::nat_lt(x.clone(), y.clone()));
    let not_less = Term::intrinsic(Intrinsic::NatLe(y, x));

    assert!(
        decided(
            &and(and(less.clone(), c.clone()), and(not_less.clone(), d)),
            &Term::intrinsic(Intrinsic::Bool(false))
        ),
        "`x < y` and `y <= x` cannot both hold"
    );
    assert!(
        !decided(&and(less, c.clone()), &and(not_less, c)),
        "`x < y && c` is not `y <= x && c`"
    );
}

// A comparison is the atom of what it states, however it is spelled: `y < x + 1` states the negation of `x < y`, `x + 1 <= y` states `x < y`, and `n == 0` the negation of `0 < n`. The control is the soundness half: `y < x + 2` states no negation of `x < y`, so the two are not a contradiction.
#[test]
fn a_comparison_is_the_atom_of_what_it_states() {
    let (x, y, c, n) = (sym(0, "x"), sym(1, "y"), sym(2, "c"), sym(3, "n"));
    let less = Term::intrinsic(Intrinsic::nat_lt(x.clone(), y.clone()));
    let successor = Term::intrinsic(Intrinsic::nat_lte(
        Nat::rebuild(1u32.into(), x.clone()),
        y.clone(),
    ));
    let negation = Term::intrinsic(Intrinsic::nat_lt(
        y.clone(),
        Nat::rebuild(1u32.into(), x.clone()),
    ));
    let further = Term::intrinsic(Intrinsic::nat_lt(y, Nat::rebuild(2u32.into(), x)));
    let (falsity, truth) = (
        Term::intrinsic(Intrinsic::Bool(false)),
        Term::intrinsic(Intrinsic::Bool(true)),
    );

    assert!(
        decided(&and(less.clone(), negation), &falsity),
        "`x < y` and `y < x + 1` cannot both hold"
    );
    assert!(
        decided(&or(less.clone(), and(successor, c)), &less),
        "absorption, its inner leaf spelled across the seam"
    );
    assert!(
        decided(
            &or(
                Term::intrinsic(Intrinsic::nat_eql(n.clone(), lit(0))),
                Term::intrinsic(Intrinsic::nat_lt(lit(0), n))
            ),
            &truth
        ),
        "a natural is zero or above it"
    );
    assert!(
        !decided(&and(less, further), &falsity),
        "`x < y` and `y < x + 2` hold together at `y = x + 1`"
    );
}

// Undecided is the only other answer: two terms that differ at an assignment are left alone, and so is a pair neither of whose sides is a connective, which costs two shape tests and nothing more.
#[test]
fn a_pair_that_differs_at_an_assignment_is_left_undecided() {
    let (b, c) = (sym(0, "b"), sym(1, "c"));

    assert!(!decided(&or(b.clone(), c.clone()), &b), "differs at `c`");
    assert!(
        !decided(&and(b.clone(), c.clone()), &or(b.clone(), c.clone())),
        "differs where exactly one holds"
    );
    assert!(!decided(&b, &c), "no connective on either side");
}

// The cap is on the atoms the two sides hold between them: at the cap a law is still decided, and one atom past it the same law is declined — completeness given up, never a verdict.
#[test]
fn a_pair_past_the_atom_cap_is_declined() {
    let absorbed = |atoms: usize| {
        let chain = (1..atoms as u32)
            .map(|index| sym(index, "a"))
            .fold(sym(0, "a"), and);
        (or(sym(0, "a"), chain), sym(0, "a"))
    };

    let (tree, atom) = absorbed(BOOL_ATOM_CAP);
    assert!(decided(&tree, &atom), "at the cap");

    let (tree, atom) = absorbed(BOOL_ATOM_CAP + 1);
    assert!(!decided(&tree, &atom), "one atom past it");
}
