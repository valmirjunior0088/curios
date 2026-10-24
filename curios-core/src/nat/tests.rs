use super::*;

fn sym(index: u32, hint: &'static str) -> Term {
    Term::free_var(&crate::Free::local(u64::from(index), Some(hint)))
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

// Soundness gate on the cancellation: it is a *multiset* operation. A summand held twice on one side against once on the other must leave one behind, because reading `a + b ⋈ c` off `a + a + b ⋈ a + c` is a false definitional equation — the route this file's board records as reaching `False` by congruence.
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

// A sum one summand longer is written over the chain it extends, so a reduct holds the sum before it and a walk over the two meets one node; the term is the one the fold writes from nothing.
//
// Mutation-checked: with every sum written from nothing, the longer sum's chain is another node.
#[test]
fn a_sum_one_summand_longer_holds_the_sum_it_extends() {
    let (a, b, c) = (sym(0, "a"), sym(1, "b"), sym(2, "c"));
    let shorter = Nat::sum(
        &Nat::sum(&a, &Nat::scaled(2u32.into(), b.clone())),
        &Term::intrinsic(Intrinsic::Nat(Nat::new(3u32))),
    );

    let longer = Nat::sum(&shorter, &c);

    let (_, chain) = Nat::decompose(&shorter);
    let (_, extended) = Nat::decompose(&longer);
    let Subterm::Intrinsic(Intrinsic::NatAdd(held, _)) = &*extended else {
        panic!("a sum of three summands: {longer}");
    };
    assert_eq!(held.identity(), chain.identity(), "the sum it extends");
    assert_eq!(
        longer,
        Nat::sum_over_floor(vec![a, Nat::scaled(2u32.into(), b), c], 3u32.into()),
        "the term the fold writes from nothing"
    );
}

// A summand that stands at the coefficient it was read at is the node it was read from, where its product would otherwise be written again.
#[test]
fn a_summand_at_its_coefficient_is_the_node_it_was_read_from() {
    let (a, x) = (sym(0, "a"), sym(1, "x"));
    let scaled = Nat::scaled(3u32.into(), x);

    let sum = Nat::sum(&a, &scaled);

    let Subterm::Intrinsic(Intrinsic::NatAdd(_, written)) = &*sum else {
        panic!("a sum of two summands: {sum}");
    };
    assert_eq!(written.identity(), scaled.identity());
}

// A summand a merge moved is written anew, and the chain with it: `a + b` beside `a` is `2 · a + b`, which holds no chain of the left operand's.
#[test]
fn a_merged_summand_is_written_anew() {
    let (a, b) = (sym(0, "a"), sym(1, "b"));

    let merged = Nat::sum(&add(a.clone(), b.clone()), &a);

    assert_eq!(merged, add(Nat::scaled(2u32.into(), a), b));
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

// A product of two sums distributed under a sum is merged into it: `(a + w + 1) * (w + a + 1) + y` normalizes to one sum of six monomials over the product's floor, so the successor the product holds is the whole sum's, which a structural elimination reads.
#[test]
fn a_product_distributed_under_a_sum_is_merged_into_it() {
    let (a, w, y) = (sym(0, "a"), sym(1, "w"), sym(2, "y"));
    let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
    let sum = |first: &Term, second: &Term| add(add(first.clone(), second.clone()), one.clone());
    let product = Term::intrinsic(Intrinsic::nat_mul(sum(&a, &w), sum(&w, &a)));

    let normalized =
        Nat::normalize(&mut crate::Folding, add(product, y.clone())).expect("normalizes");
    let (floor, inner) = Nat::decompose(&normalized);
    assert_eq!(floor, Natural::from(1usize), "{normalized}");
    assert_eq!(Nat::summands(&inner).len(), 6, "{normalized}");
    assert_eq!(occurrences(&inner, &y), 1);
}

// The fold distributes a product unless both operands are symbolic sums: a literal, a monomial or one summand over a floor on either side is distributed in the fold, and a product of two symbolic sums is the one shape the fold leaves stuck and `Nat::normalize` distributes on demand — one line, stated once and read by both.
#[test]
fn a_product_is_stuck_only_between_two_symbolic_sums() {
    let (a, w) = (sym(0, "a"), sym(1, "w"));
    let two = Term::intrinsic(Intrinsic::Nat(Nat::new(2usize)));
    let product = |left: &Term, right: &Term| {
        Term::intrinsic(Intrinsic::nat_mul(left.clone(), right.clone()))
    };
    let sum = add(a.clone(), w.clone());
    let floored = Nat::rebuild(Natural::from(1usize), a.clone());

    assert!(Nat::distributes(&a, &w), "two atoms are a monomial");
    assert!(
        Nat::distributes(&two, &sum),
        "a literal distributes over a sum"
    );
    assert!(
        Nat::distributes(&floored, &sum),
        "one summand over a floor distributes over a sum"
    );
    assert!(
        !Nat::distributes(&sum, &add(w.clone(), a.clone())),
        "two symbolic sums stay stuck"
    );

    assert!(Nat::stuck_product(&product(&sum, &sum)));
    assert!(!Nat::stuck_product(&product(&a, &w)));
    assert!(
        !Nat::has_stuck_product(&product(&product(&a, &w), &two)),
        "a monomial holds no stuck product"
    );
    assert!(
        Nat::has_stuck_product(&product(&product(&sum, &sum), &a)),
        "a stuck factor is found under a product"
    );
    assert!(
        Nat::has_stuck_product(&Nat::rebuild(
            Natural::from(1usize),
            add(product(&sum, &sum), a)
        )),
        "and under a floored sum"
    );
}
