//! The size-change algebra: composition, join, the shape order, a literal's run, an arithmetic claim's place in it, and what a guard excludes.

use {
    super::test_support::*,
    crate::totality::*,
    curios_core::{Free, Intrinsic, Nat, Size, Term},
    std::slice,
};

#[test]
fn composition_annihilates_on_unknown_and_absorbs_on_less() {
    // A chain says nothing as soon as one link says nothing, and says "decreases" as soon as one link decreases.
    for size in [LESS, SAME, NONE] {
        assert_eq!(size.compose(NONE), NONE);
        assert_eq!(NONE.compose(size), NONE);
    }
    assert_eq!(LESS.compose(SAME), LESS);
    assert_eq!(SAME.compose(LESS), LESS);
    assert_eq!(LESS.compose(LESS), LESS);
    assert_eq!(SAME.compose(SAME), SAME);
}

#[test]
fn join_keeps_the_strongest_of_two_routes() {
    assert_eq!(NONE.join(SAME), SAME);
    assert_eq!(SAME.join(LESS), LESS);
    assert_eq!(NONE.join(LESS), LESS);
    assert_eq!(LESS.join(LESS), LESS);
}

#[test]
fn shapes_are_compared_by_the_proper_subterm_order() {
    let head = Free::local(1, None);
    let tail = Free::local(2, None);
    let whole = Shape::elem_run(Carriers::Bytes, vec![Shape::Atom(head)], Shape::Atom(tail));

    assert_eq!(whole.against(&whole), Size::Same);
    assert_eq!(Shape::Atom(tail).against(&whole), Size::Less);
    assert_eq!(Shape::Atom(Free::local(9, None)).against(&whole), NONE);
}

#[test]
fn an_unread_term_is_never_equal_to_another_unread_term() {
    // Two terms the walk could not read are not thereby the same term; treating them as equal would manufacture a `Same` the analysis has no evidence for, and a chain of those is what an idempotent matrix needs.
    assert_eq!(Shape::Opaque.against(&Shape::Opaque), NONE);
}

#[test]
fn a_rebuilt_constructor_is_smaller_than_the_binder_it_rebuilds() {
    // `raw_trimmed(cons(a2, b2), …)` where `y` refined to `cons(yh, yt)` and `yt` refined to `cons(a2, b2)`: the argument is reached structurally, without folding the constructor back to `yt`.
    let (outer, a2, b2) = (
        Free::local(1, None),
        Free::local(2, None),
        Free::local(3, None),
    );
    let inner = Shape::elem_run(Carriers::Bytes, vec![Shape::Atom(a2)], Shape::Atom(b2));
    let whole = Shape::elem_run(Carriers::Bytes, vec![Shape::Atom(outer)], inner.clone());

    assert_eq!(inner.against(&whole), Size::Less);
}

#[test]
fn a_literal_is_one_packed_run_however_large_its_value() {
    // The size order on literals is decided by count arithmetic on the packed spine — never by expanding the value into a unary chain, whose depth for a `u64`-sized switch literal or call argument is a stack overflow.
    let empty = || Shape::node(Tag::Empty(Carriers::Unary), []);
    let huge = Shape::unary_run(Natural::from(u64::MAX), empty());
    let smaller = Shape::unary_run(Natural::from(u64::MAX - 1), empty());

    assert_eq!(huge.against(&huge), Size::Same);
    assert_eq!(smaller.against(&huge), Size::Less);
    assert_eq!(huge.against(&smaller), NONE);
    assert_eq!(empty().against(&huge), Size::Less);
}

#[test]
fn runs_merge_so_a_refined_tail_stays_one_chain() {
    // `match n` refines `n` to a 1-run over `pred`; a nested arm refining `pred` to a 4-run must expand to the single 5-run the chain denotes, or a literal argument could never grade against it.
    let empty = || Shape::node(Tag::Empty(Carriers::Unary), []);
    let merged = Shape::unary_run(
        Natural::from(1usize),
        Shape::unary_run(Natural::from(4usize), empty()),
    );

    assert!(merged.same_as(&Shape::unary_run(Natural::from(5usize), empty())));
    assert_eq!(
        Shape::unary_run(Natural::from(3usize), empty()).against(&merged),
        Size::Less
    );
}

#[test]
fn an_arithmetic_decrease_is_below_its_value_and_what_that_value_is_below() {
    // `n / 10` is below `n`, so below a parameter an enclosing arm has refined to a constructor over `n` as well, and says nothing about anything else. Withheld there, it would grade below `at_most`'s weaker claim at the same parameter.
    let (n, other) = (Free::local(1, None), Free::local(2, None));
    let smaller = Shape::smaller(Shape::Atom(n));

    assert_eq!(smaller.against(&Shape::Atom(n)), Size::Less);
    assert_eq!(smaller.against(&Shape::Atom(other)), NONE);
    assert_eq!(smaller.against(&Shape::Opaque), NONE);

    let refined = Shape::unary_run(Natural::from(1usize), Shape::Atom(n));
    assert_eq!(smaller.against(&refined), Size::Less);
    assert_eq!(Shape::at_most(Shape::Atom(n)).against(&refined), Size::Less);
}

#[test]
fn an_arithmetic_decrease_on_a_successor_is_below_it() {
    // A successor arm refines `n` to `pred + 1`, which is not zero by its shape, so `n - 1` is below the value `n` stands for there, and the parameter `n` expands to that value too. The control is the zero arm's refinement, where `n - 1` is `0` again: no larger than the parameter, and no descent.
    let mut probe = Unreducing::default();
    let n = Free::local(1, Some("n"));
    let pred = Free::local(2, Some("pred"));
    let successor = Shape::unary_run(Natural::from(1usize), Shape::Atom(pred));
    let zero = Shape::node(Tag::Empty(Carriers::Unary), []);
    let argument = Term::intrinsic(Intrinsic::nat_sub(
        Term::free_var(&n),
        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
    ));

    for (refined, size) in [(successor, LESS), (zero, SAME)] {
        let mut context = SizeContext::default();
        context.open(Some((n, refined)), None, Vec::new());
        let Ok(call) = grade(
            &mut probe,
            &context,
            0,
            &[n],
            0,
            1,
            slice::from_ref(&argument),
        );
        assert_eq!(call.matrix.entry(0, 0), size);
    }
}

#[test]
fn an_arithmetic_result_is_at_most_its_operand() {
    // Neither operation grows, so `x - 1` with `x` not known nonzero is at most `x`: the parameter's size where the parameter is `x`, a fixed point at zero and no descent, and below a parameter `x` is below. A successor arm's case equation is where the second is met: the certifier reads `n - 2` under `n = x + 1`, which cancels to `x - 1`.
    let mut probe = Unreducing::default();
    let n = Free::local(1, Some("n"));
    let x = Free::local(2, Some("x"));
    let successor = Shape::unary_run(Natural::from(1usize), Shape::Atom(x));
    let argument = Term::intrinsic(Intrinsic::nat_sub(
        Term::free_var(&x),
        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
    ));

    let mut context = SizeContext::default();
    context.open(Some((n, successor)), None, Vec::new());
    let Ok(call) = grade(&mut probe, &context, 0, &[n, x], 0, 1, &[argument]);
    assert_eq!(call.matrix.entry(0, 0), LESS);
    assert_eq!(call.matrix.entry(1, 0), SAME);
}

#[test]
fn an_arithmetic_claim_is_inert_in_the_constructor_order() {
    // Each is a claim about another value, not a value of its own, so it can never be found equal to or inside a tree. Anything else would manufacture a decrease out of a term the walk never read.
    let n = Free::local(1, None);
    for claim in [
        Shape::smaller(Shape::Atom(n)),
        Shape::at_most(Shape::Atom(n)),
    ] {
        let tree = Shape::node(Tag::Tuple, [claim.clone(), Shape::Atom(n)]);
        let run = Shape::elem_run(Carriers::List, vec![claim.clone()], Shape::Atom(n));

        assert!(!claim.same_as(&claim));
        assert!(!claim.proper_subterm_of(&tree));
        assert!(!claim.proper_subterm_of(&run));
    }
}

#[test]
fn a_guard_excludes_zero_only_when_its_arm_does() {
    let guard = |relation, literal: usize| Guard {
        atom: Free::local(1, None),
        literal: Natural::from(literal),
        relation,
    };

    // `/std/Nat/to_str`: the false arm of `n < 10` gives `n >= 10`.
    assert!(guard(Relation::Lt, 10).establishes_nonzero(false));
    // ...but the true arm gives `n < 10`, which admits zero.
    assert!(!guard(Relation::Lt, 10).establishes_nonzero(true));

    // `/std/Bytes/of_nat`: the false arm of `n == 0` gives `n != 0`.
    assert!(guard(Relation::Eql, 0).establishes_nonzero(false));
    assert!(!guard(Relation::Eql, 0).establishes_nonzero(true));

    // `n >= 0` is no information at all, and `n < 0` is unsatisfiable rather than informative — neither may be read as excluding zero.
    assert!(!guard(Relation::Ge, 0).establishes_nonzero(true));
    assert!(!guard(Relation::Lt, 0).establishes_nonzero(false));

    // A strict lower bound excludes zero whatever the literal is.
    assert!(guard(Relation::Gt, 0).establishes_nonzero(true));
    assert!(guard(Relation::Le, 0).establishes_nonzero(false));
}

#[test]
fn a_guard_written_with_its_literal_first_reads_the_same() {
    // `10 > n` is `n < 10`, so the arm that excludes zero is the same one.
    assert_eq!(Relation::Gt.flipped(), Relation::Lt);
    assert_eq!(Relation::Le.flipped(), Relation::Ge);
    assert_eq!(Relation::Eql.flipped(), Relation::Eql);
    assert_eq!(Relation::Neq.flipped(), Relation::Neq);
}
