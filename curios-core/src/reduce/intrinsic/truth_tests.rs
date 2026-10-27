//! The decision diagram over two `Bool` terms' atoms: what it decides equal, what it leaves alone, and what it is charged; and a connective's leaves as the converters flatten them.

use {
    super::{bool_value, decide_bool, normalize_bool, test_support::*},
    crate::{Exhaustion, Intrinsic, Nat, Subterm, Term, test_support::*},
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

/// `a0 || (a0 && a1 && … )` over so many atoms against the bare `a0`: absorption.
fn absorbed(atoms: usize) -> (Term, Term) {
    let chain = (1..atoms as u32)
        .map(|index| sym(index, "a"))
        .fold(sym(0, "a"), and);
    (or(sym(0, "a"), chain), sym(0, "a"))
}

// No pair is declined: absorption is decided at every size, far past any a table reads, and what it is charged grows as the square of its atoms — the diagrams of a chain of conjunctions — a doubling of them charging under five times as much.
#[test]
fn a_pair_is_decided_at_every_size_in_the_square_of_its_atoms() {
    let mut last: Option<u64> = None;
    for atoms in [32, 64, 128, 256] {
        let (tree, atom) = absorbed(atoms);
        let mut reducer = Budgeted {
            remaining: u64::MAX,
        };
        assert!(
            matches!(decide_bool(&mut reducer, &tree, &atom), Ok(true)),
            "absorption over {atoms} atoms"
        );
        let spent = u64::MAX - reducer.remaining;
        if let Some(last) = last {
            assert!(spent < 5 * last, "{atoms} atoms: {spent} after {last}");
        }
        last = Some(spent);
    }
}

// A pair its budget cannot finish exhausts the budget, which is no verdict, and a larger budget decides it: the comparison is priced in the budget and has no limit of its own.
#[test]
fn a_spent_budget_is_no_verdict_and_a_larger_one_decides() {
    let (tree, atom) = absorbed(64);
    let mut starved = Budgeted { remaining: 100 };
    assert!(
        decide_bool(&mut starved, &tree, &atom).is_err_and(|error| error.is_exhausted()),
        "a budget of a hundred units"
    );
    let mut ample = Budgeted {
        remaining: 10_000_000,
    };
    assert!(matches!(decide_bool(&mut ample, &tree, &atom), Ok(true)));
}

/// `w(0) = s0` and `w(k) = (w(k - 1) && sk) && (sk && w(k - 1))`: each level names the one below it twice, a graph of three nodes a level and a tree of a path for every way down.
fn web(depth: u32) -> Term {
    (1..=depth).fold(sym(0, "s"), |below, level| {
        let leaf = sym(level, "s");
        and(and(below.clone(), leaf.clone()), and(leaf, below))
    })
}

/// The leaves of a `&&` tree.
fn conjuncts(term: &Term) -> usize {
    match &**term {
        Subterm::Intrinsic(Intrinsic::BoolAnd(left, right)) => conjuncts(left) + conjuncts(right),
        _ => 1,
    }
}

// A connective is flattened once per subterm: a web twenty levels deep holds a leaf a level, where a walk of its paths would force and keep a million and spend the budget its graph needs many times over.
#[test]
fn a_connective_is_flattened_once_per_subterm() {
    let web = web(20);
    let Subterm::Intrinsic(connective) = &*web else {
        unreachable!("a conjunction")
    };
    let mut reducer = Budgeted { remaining: 100_000 };
    let flattened = normalize_bool(&mut reducer, connective)
        .expect("flattened inside the budget")
        .expect("a conjunction flattens");
    assert_eq!(conjuncts(&flattened), 21);
}

// A connective is valued as the literal it is at every assignment of its atoms, which no fold sees across connectives: `(b && c) && ¬b` is `false` and `b || ¬(b && c)` is `true`, and `b && c` is no one literal, which is undecided. A literal is itself and an atom none. A web is read once per subterm, so twenty levels of it are valued inside a budget its paths would spend many times over.
#[test]
fn a_connective_is_valued_as_the_literal_it_is_everywhere() {
    let (b, c) = (sym(0, "b"), sym(1, "c"));
    let valued = |term: &Term| bool_value(&mut Folding, term).expect("reduces");

    assert_eq!(
        valued(&and(and(b.clone(), c.clone()), not(b.clone()))),
        Some(false)
    );
    assert_eq!(
        valued(&or(b.clone(), not(and(b.clone(), c.clone())))),
        Some(true)
    );
    assert_eq!(valued(&and(b.clone(), c)), None);
    assert_eq!(valued(&Term::intrinsic(Intrinsic::Bool(true))), Some(true));
    assert_eq!(valued(&b), None);

    let mut reducer = Budgeted { remaining: 100_000 };
    assert_eq!(bool_value(&mut reducer, &web(20)), Ok(None));
}
