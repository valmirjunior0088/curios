//! Structural conversion: reflexivity, beta and delta, intrinsic congruence and the carriers' readers, plicity and marks, and universe levels.

use {
    crate::certifier::convert::test_support::*,
    crate::{test_support::*, *},
    curios_common::fill,
};

#[test]
fn a_term_converts_with_itself() {
    let mut certifier = certifier();
    let [x, again] = terms("(x, x)");

    assert_eq!(
        convert(&mut certifier, &term("#NatType"), &x, &again),
        Ok(true)
    );
}

/// Two indices no scope binds name no binders, so neither is the other: conversion answers them apart rather than aborting on either.
#[test]
fn two_bound_indices_are_apart_rather_than_aborted_on() {
    let mut certifier = certifier();

    assert_eq!(
        convert(
            &mut certifier,
            &term("Type"),
            &term("#{binding = Bound(0)} _"),
            &term("#{binding = Bound(1)} _")
        ),
        Ok(false),
    );
}

#[test]
fn distinct_literals_do_not_convert() {
    let mut certifier = certifier();

    assert_eq!(
        convert(&mut certifier, &term("#NatType"), &term("1"), &term("2")),
        Ok(false)
    );
}

/// Like terms convert and a sum against a literal clashes — decided in the fold, which merges eagerly; this pins that deferring products leaves both untouched, in the trusted checker.
#[test]
fn like_terms_convert_and_a_stuck_sum_clashes_with_a_literal() {
    let mut certifier = certifier();
    let nat = term("#NatType");
    let [sum, scaled, stuck] = terms("(#NatAdd(x, x), #NatMul(2, x), #NatAdd(x, 1))");

    assert_eq!(
        convert(&mut certifier, &nat, &sum, &scaled),
        Ok(true),
        "x + x converts with 2 · x"
    );
    assert_eq!(
        convert(&mut certifier, &nat, &stuck, &term("0")),
        Ok(false),
        "x + 1 clashes with 0"
    );
}

/// Conversion is up to computation, so a redex converts with its value.
#[test]
fn beta_equal_terms_convert() {
    let mut certifier = certifier();

    assert_eq!(
        convert(
            &mut certifier,
            &term("#NatType"),
            &term("((x: #NatType) => x)(4)"),
            &term("4")
        ),
        Ok(true)
    );
}

#[test]
fn a_definition_converts_with_its_value() {
    let mut certifier = certifier();
    declared(&mut certifier, "let /f: #NatType = 3;");

    assert_eq!(
        convert(&mut certifier, &term("#NatType"), &term("/f"), &term("3")),
        Ok(true),
    );
}

/// An intrinsic is congruent when it is the same operation on convertible operands — decided generically, so no operation can be omitted from the rule.
///
/// `m` is defined as a local no binder binds, which two texts name as one local by stating it.
#[test]
fn an_intrinsic_is_congruent_in_its_operands() {
    let mut certifier = certifier();
    declared(&mut certifier, "let /m: #NatType = #{binding = Free(7)} n;");
    let [left, right] = terms("(#NatMul(#{binding = Free(7)} n, 3), #NatMul(/m, 3))");

    assert_eq!(
        convert(&mut certifier, &term("#NatType"), &left, &right),
        Ok(true)
    );
}

#[test]
fn different_operations_do_not_convert() {
    let mut certifier = certifier();
    let [add, mul] = terms("(#NatAdd(n, 3), #NatMul(n, 3))");

    assert_eq!(
        convert(&mut certifier, &term("#NatType"), &add, &mul),
        Ok(false)
    );
}

/// Two stuck operations of one kind are each the whole of their side, so the pair has no atoms to class and is compared operand by operand. Were the pair handed back as its own two atoms, classing them would ask the comparison already in progress: the guard in `curios-analysis`'s `atoms_of` is what hands back none.
#[test]
fn two_stuck_operations_on_different_operands_do_not_convert() {
    let mut certifier = certifier();
    let [left, right] = terms("(#NatSub(n, k), #NatSub(m, k))");

    assert_eq!(
        convert(&mut certifier, &term("#NatType"), &left, &right),
        Ok(false)
    );
}

/// A truncated difference does not commute where a sum does: a chain that paired `-`'s operands as it pairs a sum's would hold `a - b` one with `b - a`, which part at `a := 5`, `b := 3`. The control is the sum, whose operands the chain pairs. `tests::rules`'s twin shares the name.
#[test]
fn a_difference_does_not_convert_with_its_operands_swapped() {
    let mut certifier = certifier();
    let nat = term("#NatType");
    let [sum, swapped_sum, difference, swapped_difference] =
        terms("(#NatAdd(a, b), #NatAdd(b, a), #NatSub(a, b), #NatSub(b, a))");

    assert_eq!(convert(&mut certifier, &nat, &sum, &swapped_sum), Ok(true));
    assert_eq!(
        convert(&mut certifier, &nat, &difference, &swapped_difference),
        Ok(false)
    );
}

/// The free-monoid peel is what decides `n + 2 ≡ m + 2` by comparing `n` with `m` rather than comparing two opaque symbolic sums.
#[test]
fn a_shared_successor_floor_is_peeled_before_comparing() {
    let mut certifier = certifier();
    let nat = term("#NatType");
    let [left, right, same] = terms("(#NatAdd(n, 2), #NatAdd(m, 2), #NatAdd(n, 2))");

    // Distinct symbolic bases: the peel exposes the real disagreement.
    assert_eq!(convert(&mut certifier, &nat, &left, &right), Ok(false));

    // The same base: equal after the shared floor comes off.
    assert_eq!(convert(&mut certifier, &nat, &left, &same), Ok(true));
}

/// The heads are one number spelled two ways, so the peel cannot strip them, and a peel handing the nesting back intact would leave shape congruence a two-operand concatenation against a three-operand one. Regrouping in the peel is what lets the pair reach the operand comparison that decides `n + m ≡ m + n`.
#[test]
fn a_nested_concatenation_converts_with_its_flat_spelling_past_unlike_heads() {
    let mut certifier = certifier();
    let [nested, flat] = terms(
        "(#ListConcat(#NatType, [#ListConcat(#NatType, [g(#NatAdd(n, m)), t]), #List(#NatType, [h])]), #ListConcat(#NatType, [g(#NatAdd(m, n)), t, #List(#NatType, [h])]))",
    );

    assert_eq!(
        convert(&mut certifier, &term("Type"), &nested, &flat),
        Ok(true)
    );
}

/// Plicity is part of a function type's identity: `(A) -> A` and `(@A) -> A` have different calling conventions, and conflating them would let a value be applied through the wrong one.
#[test]
fn plicity_distinguishes_two_function_types() {
    let mut certifier = certifier();

    assert_eq!(
        convert(
            &mut certifier,
            &term("Type"),
            &term("(a: #NatType) -> #NatType"),
            &term("(@a: #NatType) -> #NatType")
        ),
        Ok(false),
    );
}

/// Plicity is part of a function's identity, a literal's as a type's: two literals under one mark whose bodies differ by a β-step are one function, and two apart in a mark are two. Asked where no type directs the comparison, as a stuck match's arms are compared, where η contracting the second literal across its mark would hold the first pair apart. The elaborator's twin keeps the name.
#[test]
fn plicity_distinguishes_two_function_literals() {
    let mut certifier = certifier();
    let untyped = term("Type");
    let identity = term("(@n: #NatType) => n");

    assert_eq!(
        convert(
            &mut certifier,
            &untyped,
            &identity,
            &term("(@n: #NatType) => ((x: #NatType) => x)(n)")
        ),
        Ok(true),
    );
    assert_eq!(
        convert(
            &mut certifier,
            &untyped,
            &identity,
            &term("(n: #NatType) => n")
        ),
        Ok(false),
    );
}

/// Two universes convert only at the same level. Cumulativity is a *subtyping* rule and belongs to checking, not here: conversion is symmetric and levels are not.
#[test]
fn universes_convert_only_at_the_same_level() {
    let mut certifier = certifier();
    let zero = term("Type");
    let one = term("#{level = 1} Type");

    assert_eq!(convert(&mut certifier, &zero, &zero, &zero), Ok(true));
    assert_eq!(convert(&mut certifier, &zero, &zero, &one), Ok(false));
}

/// A metavariable is elaboration-only syntax, and conversion refuses it rather than comparing ids — the exclusion is the certifier's own, not an inherited guarantee of the zonk traversal. Reflexivity is the one admitted case (the syntactic fast path, sound because it decides nothing about the unknown); any comparison that would have to *look* at a metavariable refuses.
#[test]
fn a_metavariable_does_not_convert_with_anything_else() {
    let mut certifier = certifier();
    let untyped = term("Type");
    let left = term("#{id = 0, origin = Hole} ?");

    assert!(matches!(
        convert(
            &mut certifier,
            &untyped,
            &left,
            &term("#{id = 1, origin = Hole} ?")
        ),
        Err(Error::NotCore(_)),
    ));
    assert!(matches!(
        convert(&mut certifier, &untyped, &left, &term("0")),
        Err(Error::NotCore(_)),
    ));
}

/// A `Bool` connective against a term that is no intrinsic at all — absorption's shape — never reaches the intrinsic congruence, so the decision diagram over the two sides' atoms is asked at the head dispatch. The control differs at one assignment and stays apart.
#[test]
fn a_bool_tree_converts_with_the_bare_term_it_equals_at_every_assignment() {
    let mut certifier = certifier();
    let bool_type = term("#BoolType");
    let [absorbed, b, disjunction] = terms("(#BoolOr(b, #BoolAnd(b, c)), b, #BoolOr(b, c))");

    assert_eq!(convert(&mut certifier, &bool_type, &absorbed, &b), Ok(true));
    assert_eq!(convert(&mut certifier, &bool_type, &b, &absorbed), Ok(true));
    assert_eq!(
        convert(&mut certifier, &bool_type, &disjunction, &b),
        Ok(false)
    );
}

/// Two connectives are one only where they agree at every assignment of their atoms: `a && b` and `a || b` part at `a := true`, `b := false`. The control is a conjunction against its operands swapped, which agree everywhere. `tests::rules`'s twin shares the name.
#[test]
fn a_conjunction_does_not_convert_with_the_disjunction_of_its_operands() {
    let mut certifier = certifier();
    let bool_type = term("#BoolType");
    let [and, swapped, or] = terms("(#BoolAnd(a, b), #BoolAnd(b, a), #BoolOr(a, b))");

    assert_eq!(
        convert(&mut certifier, &bool_type, &and, &swapped),
        Ok(true)
    );
    assert_eq!(convert(&mut certifier, &bool_type, &and, &or), Ok(false));
}

/// Two applications of one `Nat`-valued operation that differ only in a universe instance are one number: `len(n<0>)` and `len(n<1>)` meet before their congruence, which would compare the two instances' levels and refuse. A number never depends on a level, which is the licence the cancellation already reads inside a sum. The control is two different operands at one instance, which stay apart. The elaborator's twin of this proposition shares the name.
#[test]
fn one_operation_at_two_universe_instances_is_one_number() {
    let mut certifier = certifier();
    let nat = term("#NatType");
    let length = |operand: &str, level: u32| {
        term(&fill!(
            "#BytesLen(#{universes = {levels = [${level}]}} /${operand})",
            level,
            operand
        ))
    };

    assert_eq!(
        convert(&mut certifier, &nat, &length("n", 0), &length("n", 1)),
        Ok(true)
    );
    assert_eq!(
        convert(&mut certifier, &nat, &length("n", 0), &length("m", 0)),
        Ok(false)
    );
}

/// The levels a result carries are compared before its operands: two polls of one cell whose results are the cell's family at two ground levels are two terms, though every operand agrees. The control is the same poll at one level. The elaborator's twin of this proposition shares the name.
#[test]
fn two_polls_of_one_cell_at_two_ground_levels_do_not_convert() {
    let mut certifier = certifier();
    let untyped = term("Type");
    let [at_zero, at_one, again_at_zero] = terms(
        "(#CellPoll(#NatType, cell, {levels = [0]}), #CellPoll(#NatType, cell, {levels = [1]}), #CellPoll(#NatType, cell, {levels = [0]}))",
    );

    assert_eq!(
        convert(&mut certifier, &untyped, &at_zero, &at_one),
        Ok(false)
    );
    assert_eq!(
        convert(&mut certifier, &untyped, &at_zero, &again_at_zero),
        Ok(true)
    );
}

/// Two spines of one head converge by their arguments alone. A written mark is held to its parameter's where the application is typed, so two well-typed spines of one head cannot differ in one, and conversion reads none. Put to both checkers under this name.
#[test]
fn applications_differing_in_a_mark_alone_convert() {
    let mut certifier = certifier();
    let [plain, marked] = under(
        &mut certifier,
        "f: (x: #NatType) -> #NatType",
        "f(1), f(@1)",
    );

    assert_eq!(
        convert(&mut certifier, &term("#NatType"), &plain, &marked),
        Ok(true)
    );
}
