//! Structural conversion: reflexivity, beta and delta, intrinsic congruence and the carriers' readers, plicity and marks, and universe levels.

use {
    super::test_support::*,
    crate::{Error, convert},
    curios_core::{
        Free, FuncType, InstanceHead, Intrinsic, Level, MetavarId, Subterm, Term, UniverseContext,
        Universes, Var,
    },
    curios_utilities::Plicity,
};

#[test]
fn a_term_converts_with_itself() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    assert_eq!(
        convert(
            &mut kernel,
            &nat_type(),
            &Term::free_var(&x),
            &Term::free_var(&x)
        ),
        Ok(true),
    );
}

#[test]
fn distinct_literals_do_not_convert() {
    let mut kernel = kernel();

    assert_eq!(
        convert(&mut kernel, &nat_type(), &nat(1), &nat(2)),
        Ok(false)
    );
}

/// Like terms convert and a sum against a literal clashes — decided in the fold, which merges eagerly; this pins that deferring products leaves both untouched, in the trusted checker.
#[test]
fn like_terms_convert_and_a_stuck_sum_clashes_with_a_literal() {
    let mut kernel = kernel();
    let x = binder(0, "x");
    let sum = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), Term::free_var(&x)));
    let scaled = Term::intrinsic(Intrinsic::nat_mul(nat(2), Term::free_var(&x)));
    assert_eq!(
        convert(&mut kernel, &nat_type(), &sum, &scaled),
        Ok(true),
        "x + x converts with 2 · x"
    );
    let stuck = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat(1)));
    assert_eq!(
        convert(&mut kernel, &nat_type(), &stuck, &nat(0)),
        Ok(false),
        "x + 1 clashes with 0"
    );
}

/// Conversion is up to computation, so a redex converts with its value.
#[test]
fn beta_equal_terms_convert() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let redex = Term::apply(Term::func([(x, nat_type())], Term::free_var(&x)), [nat(4)]);

    assert_eq!(convert(&mut kernel, &nat_type(), &redex, &nat(4)), Ok(true));
}

#[test]
fn a_definition_converts_with_its_value() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    kernel
        .define(&f, &nat_type(), &nat(3), &UniverseContext::default())
        .expect("a name is declared once");

    assert_eq!(
        convert(&mut kernel, &nat_type(), &Term::free_var(&f), &nat(3)),
        Ok(true),
    );
}

/// An intrinsic is congruent when it is the same operation on convertible operands — decided generically, so no operation can be omitted from the rule.
#[test]
fn an_intrinsic_is_congruent_in_its_operands() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let m = binder(1, "m");
    kernel
        .define(
            &m,
            &nat_type(),
            &Term::free_var(&n),
            &UniverseContext::default(),
        )
        .expect("a name is declared once");

    let left = Term::intrinsic(Intrinsic::nat_mul(Term::free_var(&n), nat(3)));
    let right = Term::intrinsic(Intrinsic::nat_mul(Term::free_var(&m), nat(3)));

    assert_eq!(convert(&mut kernel, &nat_type(), &left, &right), Ok(true));
}

#[test]
fn different_operations_do_not_convert() {
    let mut kernel = kernel();
    let n = binder(0, "n");

    let add = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(3)));
    let mul = Term::intrinsic(Intrinsic::nat_mul(Term::free_var(&n), nat(3)));

    assert_eq!(convert(&mut kernel, &nat_type(), &add, &mul), Ok(false));
}

/// Two stuck operations of one kind are each the whole of their side, so the pair has no atoms to class and is compared operand by operand. Were the pair handed back as its own two atoms, classing them would ask the comparison already in progress: the guard in `curios-analysis`'s `atoms_of` is what hands back none.
#[test]
fn two_stuck_operations_on_different_operands_do_not_convert() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let m = binder(1, "m");
    let k = binder(2, "k");

    let left = Term::intrinsic(Intrinsic::NatSub(Term::free_var(&n), Term::free_var(&k)));
    let right = Term::intrinsic(Intrinsic::NatSub(Term::free_var(&m), Term::free_var(&k)));

    assert_eq!(convert(&mut kernel, &nat_type(), &left, &right), Ok(false));
}

/// A truncated difference does not commute where a sum does: a chain that paired `-`'s operands as it pairs a sum's would hold `a - b` one with `b - a`, which part at `a := 5`, `b := 3`. The control is the sum, whose operands the chain pairs. `tests::board`'s twin shares the name.
#[test]
fn a_difference_does_not_convert_with_its_operands_swapped() {
    let mut kernel = kernel();
    let (a, b) = (
        Term::free_var(&binder(0, "a")),
        Term::free_var(&binder(1, "b")),
    );

    let sum = |left: &Term, right: &Term| {
        Term::intrinsic(Intrinsic::nat_add(left.clone(), right.clone()))
    };
    let difference =
        |left: &Term, right: &Term| Term::intrinsic(Intrinsic::NatSub(left.clone(), right.clone()));

    assert_eq!(
        convert(&mut kernel, &nat_type(), &sum(&a, &b), &sum(&b, &a)),
        Ok(true)
    );
    assert_eq!(
        convert(
            &mut kernel,
            &nat_type(),
            &difference(&a, &b),
            &difference(&b, &a)
        ),
        Ok(false)
    );
}

/// The free-monoid peel is what decides `n + 2 ≡ m + 2` by comparing `n` with `m` rather than comparing two opaque symbolic sums.
#[test]
fn a_shared_successor_floor_is_peeled_before_comparing() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let m = binder(1, "m");

    let left = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(2)));
    let right = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&m), nat(2)));

    // Distinct symbolic bases: the peel exposes the real disagreement.
    assert_eq!(convert(&mut kernel, &nat_type(), &left, &right), Ok(false));

    // The same base: equal after the shared floor comes off.
    let same = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(2)));
    assert_eq!(convert(&mut kernel, &nat_type(), &left, &same), Ok(true));
}

/// The heads are one number spelled two ways, so the peel cannot strip them, and a peel handing the nesting back intact would leave shape congruence a two-operand concatenation against a three-operand one. Regrouping in the peel is what lets the pair reach the operand comparison that decides `n + m ≡ m + n`.
#[test]
fn a_nested_concatenation_converts_with_its_flat_spelling_past_unlike_heads() {
    let mut kernel = kernel();
    let g = binder(0, "g");
    let n = binder(1, "n");
    let m = binder(2, "m");
    let t = binder(3, "t");
    let h = binder(4, "h");

    let head = |left: &Free, right: &Free| {
        Term::apply(
            Term::free_var(&g),
            [Term::intrinsic(Intrinsic::nat_add(
                Term::free_var(left),
                Term::free_var(right),
            ))],
        )
    };
    let single = Term::intrinsic(Intrinsic::List {
        element: nat_type(),
        items: vec![Term::free_var(&h)],
    });
    let cat = |operands: Vec<Term>| Term::intrinsic(Intrinsic::list_concat(nat_type(), operands));

    let nested = cat(vec![
        cat(vec![head(&n, &m), Term::free_var(&t)]),
        single.clone(),
    ]);
    let flat = cat(vec![head(&m, &n), Term::free_var(&t), single]);

    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &nested, &flat),
        Ok(true)
    );
}

/// Plicity is part of a function type's identity: `(A) -> A` and `(@A) -> A` have different calling conventions, and conflating them would let a value be applied through the wrong one.
#[test]
fn plicity_distinguishes_two_function_types() {
    let mut kernel = kernel();
    let a = binder(0, "a");

    let explicit = Term::func_type([(a, nat_type())], nat_type());
    let implicit = Term::from(Subterm::FuncType(FuncType::new(
        match &*explicit {
            Subterm::FuncType(func) => func.telescope.clone(),
            _ => unreachable!("built as a function type"),
        },
        vec![Plicity::Implicit],
    )));

    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &explicit, &implicit),
        Ok(false),
    );
}

/// Plicity is part of a function's identity, a literal's as a type's: two literals under one mark whose bodies differ by a β-step are one function, and two apart in a mark are two. Asked where no type directs the comparison, as a stuck match's arms are compared, where η contracting the second literal across its mark would hold the first pair apart. The elaborator's twin keeps the name.
#[test]
fn plicity_distinguishes_two_function_literals() {
    let mut kernel = kernel();
    let (n, x) = (binder(0, "n"), binder(1, "x"));

    let identity = Term::func_marked([(Plicity::Implicit, n, nat_type())], Term::free_var(&n));
    let forwarded = Term::func_marked(
        [(Plicity::Implicit, n, nat_type())],
        Term::apply(
            Term::func([(x, nat_type())], Term::free_var(&x)),
            [Term::free_var(&n)],
        ),
    );
    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &identity, &forwarded),
        Ok(true),
    );

    let shown = Term::func([(n, nat_type())], Term::free_var(&n));
    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &identity, &shown),
        Ok(false),
    );
}

/// Two universes convert only at the same level. Cumulativity is a *subtyping* rule and belongs to checking, not here: conversion is symmetric and levels are not.
#[test]
fn universes_convert_only_at_the_same_level() {
    let mut kernel = kernel();
    let zero = Term::type_ground();
    let one = Term::type_at(Level::zero().succ().expect("level zero succeeds"));

    assert_eq!(convert(&mut kernel, &zero, &zero, &zero), Ok(true));
    assert_eq!(convert(&mut kernel, &zero, &zero, &one), Ok(false));
}

/// A metavariable is elaboration-only syntax, and conversion refuses it rather than comparing ids — the exclusion is the kernel's own, not an inherited guarantee of the zonk traversal. Reflexivity is the one admitted case (the syntactic fast path, sound because it decides nothing about the unknown); any comparison that would have to *look* at a metavariable refuses.
#[test]
fn a_metavariable_does_not_convert_with_anything_else() {
    let mut kernel = kernel();
    let left = Term::hole(MetavarId::from(0usize));
    let right = Term::hole(MetavarId::from(1usize));

    assert!(matches!(
        convert(&mut kernel, &Term::type_ground(), &left, &right),
        Err(Error::NotCore(_)),
    ));
    assert!(matches!(
        convert(&mut kernel, &Term::type_ground(), &left, &nat(0)),
        Err(Error::NotCore(_)),
    ));
}

/// A `Bool` connective against a term that is no intrinsic at all — absorption's shape — never reaches the intrinsic congruence, so the decision diagram over the two sides' atoms is asked at the head dispatch. The control differs at one assignment and stays apart.
#[test]
fn a_bool_tree_converts_with_the_bare_term_it_equals_at_every_assignment() {
    let mut kernel = kernel();
    let (b, c) = (
        Term::free_var(&binder(0, "b")),
        Term::free_var(&binder(1, "c")),
    );
    let bool_type = Term::intrinsic(Intrinsic::BoolType);
    let or = |left: Term, right: Term| Term::intrinsic(Intrinsic::BoolOr(left, right));
    let absorbed = or(
        b.clone(),
        Term::intrinsic(Intrinsic::BoolAnd(b.clone(), c.clone())),
    );

    assert_eq!(convert(&mut kernel, &bool_type, &absorbed, &b), Ok(true));
    assert_eq!(convert(&mut kernel, &bool_type, &b, &absorbed), Ok(true));
    assert_eq!(
        convert(&mut kernel, &bool_type, &or(b.clone(), c), &b),
        Ok(false)
    );
}

/// Two connectives are one only where they agree at every assignment of their atoms: `a && b` and `a || b` part at `a := true`, `b := false`. The control is a conjunction against its operands swapped, which agree everywhere. `tests::board`'s twin shares the name.
#[test]
fn a_conjunction_does_not_convert_with_the_disjunction_of_its_operands() {
    let mut kernel = kernel();
    let (a, b) = (
        Term::free_var(&binder(0, "a")),
        Term::free_var(&binder(1, "b")),
    );
    let bool_type = Term::intrinsic(Intrinsic::BoolType);
    let and = |left: &Term, right: &Term| {
        Term::intrinsic(Intrinsic::BoolAnd(left.clone(), right.clone()))
    };
    let or =
        |left: &Term, right: &Term| Term::intrinsic(Intrinsic::BoolOr(left.clone(), right.clone()));

    assert_eq!(
        convert(&mut kernel, &bool_type, &and(&a, &b), &and(&b, &a)),
        Ok(true)
    );
    assert_eq!(
        convert(&mut kernel, &bool_type, &and(&a, &b), &or(&a, &b)),
        Ok(false)
    );
}

/// Two applications of one `Nat`-valued operation that differ only in a universe instance are one number: `len(n<0>)` and `len(n<1>)` meet before their congruence, which would compare the two instances' levels and refuse. A number never depends on a level, which is the licence the cancellation already reads inside a sum. The control is two different operands at one instance, which stay apart. The elaborator's twin of this proposition shares the name.
#[test]
fn one_operation_at_two_universe_instances_is_one_number() {
    let mut kernel = kernel();
    let length = |operand: &Free, level: u32| {
        Term::intrinsic(Intrinsic::bytes_len(Term::instance(
            InstanceHead::Var(Var::free(*operand)),
            vec![Level::constant(level)],
        )))
    };
    let (n, m) = (binder(60, "n"), binder(61, "m"));

    assert_eq!(
        convert(&mut kernel, &nat_type(), &length(&n, 0), &length(&n, 1)),
        Ok(true)
    );
    assert_eq!(
        convert(&mut kernel, &nat_type(), &length(&n, 0), &length(&m, 0)),
        Ok(false)
    );
}

/// The levels a result carries are compared before its operands: two polls of one cell whose results are the cell's family at two ground levels are two terms, though every operand agrees. The control is the same poll at one level. The elaborator's twin of this proposition shares the name.
#[test]
fn two_polls_of_one_cell_at_two_ground_levels_do_not_convert() {
    let mut kernel = kernel();
    let cell = Term::free_var(&binder(62, "cell"));
    let poll = |level: u32| {
        Term::intrinsic(Intrinsic::CellPoll {
            element: nat_type(),
            cell: cell.clone(),
            universes: Universes::levels(vec![Level::constant(level)]),
        })
    };

    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &poll(0), &poll(1)),
        Ok(false)
    );
    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &poll(0), &poll(0)),
        Ok(true)
    );
}

/// Two spines of one head converge by their arguments alone. A written mark is held to its parameter's where the application is typed, so two well-typed spines of one head cannot differ in one, and conversion reads none. Put to both checkers under this name.
#[test]
fn applications_differing_in_a_mark_alone_convert() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    let x = binder(1, "x");
    kernel.assume(&f, &Term::func_type([(x, nat_type())], nat_type()));

    let plain = Term::apply(Term::free_var(&f), [nat(1)]);
    let marked = Term::apply_marked(Term::free_var(&f), [(Plicity::Implicit, nat(1))]);

    assert_eq!(convert(&mut kernel, &nat_type(), &plain, &marked), Ok(true));
}
