//! Structural conversion: reflexivity, beta and delta, eta at a function and a pair, intrinsic congruence, plicity and universe levels.

use super::test_support::*;
use {
    crate::{KernelError, convert},
    curios_core::{
        FuncType, Global, Intrinsic, Level, MetavarId, StructDecl, StructType, Subterm, Telescope,
        Term, UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
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

/// Like terms convert and a sum against a literal clashes — decided in the fold, which merges eagerly; this pins that the product-only deferral left both untouched, in the trusted checker.
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

    let redex = Term::apply(
        Term::func([(x.clone(), nat_type())], Term::free_var(&x)),
        [nat(4)],
    );

    assert_eq!(convert(&mut kernel, &nat_type(), &redex, &nat(4)), Ok(true));
}

#[test]
fn a_definition_converts_with_its_value() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    kernel.define(&f, &nat_type(), &nat(3), &UniverseContext::default());

    assert_eq!(
        convert(&mut kernel, &nat_type(), &Term::free_var(&f), &nat(3)),
        Ok(true),
    );
}

/// Eta at a function type: `f` and `(x) => f(x)` are the same function, and neither side has to be written in the other's shape for conversion to see it.
#[test]
fn eta_makes_a_function_converge_with_its_expansion() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    let x = binder(1, "x");
    let arrow = Term::func_type([(x.clone(), nat_type())], nat_type());

    let expanded = Term::func(
        [(x.clone(), nat_type())],
        Term::apply(Term::free_var(&f), [Term::free_var(&x)]),
    );

    assert_eq!(
        convert(&mut kernel, &arrow, &Term::free_var(&f), &expanded),
        Ok(true),
    );
}

/// Eta at a Σ type: `p` and `(p.0, p.1)` are the same pair.
#[test]
fn eta_makes_a_pair_converge_with_its_projections() {
    let mut kernel = kernel();
    let p = binder(0, "p");
    let pair_type = Term::tuple_type([(binder(8, "a"), nat_type()), (binder(9, "b"), nat_type())]);

    let expanded = Term::tuple([
        Term::proj(Term::free_var(&p), 0),
        Term::proj(Term::free_var(&p), 1),
    ]);

    assert_eq!(
        convert(&mut kernel, &pair_type, &Term::free_var(&p), &expanded),
        Ok(true),
    );
}

/// A struct literal with fewer fields than its declaration must not convert with a neutral inhabitant.
///
/// The eta walk is driven by the literal's fields, so a short literal used to run out before the declaration's telescope did and the vacuous remainder passed — equating a malformed literal with *any* neutral at the type, in the accepting direction. The walk now answers with whether it consumed the whole telescope.
#[test]
fn a_short_struct_literal_does_not_convert_with_a_neutral() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["S"]));
    kernel.declare_struct(
        &name,
        &StructDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::build(
                [(binder(8, "a"), nat_type()), (binder(9, "b"), nat_type())],
                (),
            )),
            result_sort: Term::type_ground(),
            module: Qualifier::from(["S"]),
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    let type_ = Term::from(Subterm::StructType(StructType {
        name: name.clone(),
        universes: Vec::new(),
        params: Vec::new(),
    }));
    let literal = Term::struct_(name, Vec::<Term>::new(), Vec::<Term>::new());
    let neutral = Term::free_var(&binder(0, "s"));

    assert_eq!(convert(&mut kernel, &type_, &literal, &neutral), Ok(false));
}

/// An intrinsic is congruent when it is the same operation on convertible operands — decided generically, so no operation can be omitted from the rule.
#[test]
fn an_intrinsic_is_congruent_in_its_operands() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let m = binder(1, "m");
    kernel.define(
        &m,
        &nat_type(),
        &Term::free_var(&n),
        &UniverseContext::default(),
    );

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

/// Plicity is part of a function type's identity: `(A) -> A` and `(@A) -> A` have different calling conventions, and conflating them would let a value be applied through the wrong one.
#[test]
fn plicity_distinguishes_two_function_types() {
    let mut kernel = kernel();
    let a = binder(0, "a");

    let explicit = Term::func_type([(a.clone(), nat_type())], nat_type());
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
        Err(KernelError::NotCore(_)),
    ));
    assert!(matches!(
        convert(&mut kernel, &Term::type_ground(), &left, &nat(0)),
        Err(KernelError::NotCore(_)),
    ));
}
