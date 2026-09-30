use super::*;

fn sym(index: u32, hint: &'static str) -> Term {
    Term::free_var(&crate::Free::local(index, Some(hint)))
}

fn add(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::nat_add(left, right))
}

fn occurrences(term: &Term, wanted: &Term) -> usize {
    Nat::summands(term)
        .iter()
        .filter(|summand| *summand == wanted)
        .count()
}

// Soundness gate on the cancellation: it is a *multiset* operation. A summand held twice on one side against once on the other must leave one behind, because reading `a + b ⋈ c` off `a + a + b ⋈ a + c` is a false definitional equation — the route this file's perimeter records as reaching `False` by congruence.
#[test]
fn cancellation_removes_one_occurrence_per_match() {
    let (a, b, c) = (sym(0, "a"), sym(1, "b"), sym(2, "c"));

    let (left, right) = Nat::cancel_common(
        &add(add(a.clone(), a.clone()), b.clone()),
        &add(a.clone(), c.clone()),
    );

    assert_eq!(occurrences(&left, &a), 1, "one `a` must survive the cancel");
    assert_eq!(occurrences(&left, &b), 1, "`b` is shared with nothing");
    assert_eq!(occurrences(&right, &a), 0, "the right's single `a` cancels");
    assert_eq!(occurrences(&right, &c), 1, "`c` is shared with nothing");
}

// Regression: a pass that cancels nothing must return its operands *identically*, not merely equivalently. Rebuilding a sum through `sum_over_floor` re-associates and reorders it, so a stuck comparison rebuilt from reordered operands is a new term the caller reduces again — which reorders again. That oscillation is not a slow reduction, it is an unbounded one, and it would overflow the stack building the fixed prelude.
#[test]
fn cancellation_is_stable_when_nothing_is_shared() {
    let (a, b, c, d) = (sym(0, "a"), sym(1, "b"), sym(2, "c"), sym(3, "d"));
    let left = add(a, add(b, c));

    let (settled_left, settled_right) = Nat::cancel_common(&left, &d);

    assert_eq!(
        settled_left, left,
        "an uncancelled sum keeps its own association"
    );
    assert_eq!(
        settled_right, d,
        "and so does the side it was compared against"
    );
}

// The read-then-rebuild identity the sum normal form promises: a sum already merged, read back through `Nat::sum` beside nothing, is the very term it was — like terms merged, first-appearance order kept, the floor outermost — which is what lets the fold hand a reduced sum back through itself without changing it.
#[test]
fn a_sum_in_normal_form_reads_back_as_itself() {
    let (a, b) = (sym(0, "a"), sym(1, "b"));
    let merged = Nat::sum(
        &add(Nat::scaled(2u32.into(), a), b),
        &Term::intrinsic(Intrinsic::Nat(Nat::new(3u32))),
    );

    assert_eq!(
        Nat::sum(&merged, &Term::intrinsic(Intrinsic::Nat(Nat::Zero))),
        merged
    );
}

// A residual is rebuilt from the terms it was read from, never from a copy: `curios-algebra` hands each summand's origin back untouched, so the `b` left over from `a + b` against `a + c` is the very allocation the caller wrote, and a shared term graph stays shared through the cancellation.
#[test]
fn a_residual_keeps_the_terms_it_was_read_from() {
    let (a, b, c) = (sym(0, "a"), sym(1, "b"), sym(2, "c"));
    let application = |head: &Term, argument: &Term| {
        Term::intrinsic(Intrinsic::nat_mul(head.clone(), argument.clone()))
    };
    let (kept, other) = (application(&b, &c), application(&c, &c));

    let (left, right) = Nat::cancel_common(
        &add(a.clone(), kept.clone()),
        &add(a.clone(), other.clone()),
    );

    assert_eq!(
        left.identity(),
        kept.identity(),
        "the left residual is `b · c` as written"
    );
    assert_eq!(
        right.identity(),
        other.identity(),
        "and the right one is `c · c` as written"
    );
}
