//! Metavariable solving: flex-rigid, occurs and scope checks, pattern inversion and pruning, flex-flex, revalidation, and the goal history key.

use super::test_support::*;
use crate::*;
use curios_core::*;

// === Metavariables / unification ===========================================

#[test]
fn flex_rigid_commits_solution() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());

    // ?0 ≟ Nat  (at type Type)
    let nat = Term::intrinsic(Intrinsic::NatType);
    assert_eq!(conv(&mut context, &Term::hole(0), &nat), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&nat));
}

#[test]
fn solve_is_symmetric() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());

    let nat = Term::intrinsic(Intrinsic::NatType);
    // rigid on the left, flex on the right
    assert_eq!(conv(&mut context, &nat, &Term::hole(0)), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&nat));
}

#[test]
fn occurs_check_rejects_cyclic_solution() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());

    // ?0 ≟ (x : ?0) -> Nat  — the candidate mentions ?0 itself.
    let cyclic = Term::func_type(
        [(x.clone(), Term::hole(0))],
        Term::intrinsic(Intrinsic::NatType),
    );
    assert_eq!(conv(&mut context, &Term::hole(0), &cyclic), Ok(false));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn scope_check_rejects_out_of_context_variable() {
    let mut context = context();
    let x_binder = context.fresh(Some("x"));
    // Birth with empty Γ: no variable is in scope for ?0.
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());

    // ?0 ≟ x  — `x` is not available to ?0.
    let x = Term::free_var(&x_binder);
    assert_eq!(conv(&mut context, &Term::hole(0), &x), Ok(false));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn scope_check_allows_in_context_variable() {
    let mut context = context();
    let x_binder = context.fresh(Some("x"));
    // Γ = (x : Type); result is Type, and the candidate `x` is in scope.
    context.assume(&x_binder, &Term::type_ground());
    context.birth_metavar(
        MetavarId(0),
        vec![(x_binder.clone(), Term::type_ground())],
        Term::type_ground(),
    );

    let x = Term::free_var(&x_binder);
    let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![x.clone()]);
    assert_eq!(conv(&mut context, &occurrence, &x), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&x));
}

#[test]
fn revalidation_admits_checkable_but_not_inferable_candidate() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));
    // ?0 : (x : Nat, y : Nat) — a tuple type, born in empty Γ.
    let pair_type = Term::tuple_type([
        (x.clone(), Term::intrinsic(Intrinsic::NatType)),
        (y.clone(), Term::intrinsic(Intrinsic::NatType)),
    ]);
    context.birth_metavar(MetavarId(0), Vec::new(), pair_type);

    // ?0 ≟ (1, 2). A bare tuple has no synthesizable type (`elaborate_tuple` is Check-only), so synthesize-then-convert re-validation rejected it; checking it against the frozen tuple result type admits it.
    let pair = Term::tuple([nat(1), nat(2)]);
    assert_eq!(conv(&mut context, &Term::hole(0), &pair), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&pair));
}

#[test]
fn revalidation_rejects_ill_typed_candidate_through_checking() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));
    // ?0 : (x : Nat, y : Nat).
    let pair_type = Term::tuple_type([
        (x.clone(), Term::intrinsic(Intrinsic::NatType)),
        (y.clone(), Term::intrinsic(Intrinsic::NatType)),
    ]);
    context.birth_metavar(MetavarId(0), Vec::new(), pair_type);

    // ?0 ≟ (1, 2, 3): a three-field tuple does not check against a two-field tuple type, so checking still rejects the candidate and commits nothing.
    let wrong = Term::tuple([nat(1), nat(2), nat(3)]);
    assert_eq!(conv(&mut context, &Term::hole(0), &wrong), Ok(false));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn flex_flex_equal_id_short_circuits() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());

    // ?0 ≟ ?0 is trivially true and leaves the metavariable unsolved.
    assert_eq!(conv(&mut context, &Term::hole(0), &Term::hole(0)), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn flex_flex_distinct_is_residual() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    context.birth_metavar(MetavarId(1), Vec::new(), Term::type_ground());

    // ?0 ≟ ?1 postpones with no way to progress — a residual constraint.
    assert_eq!(
        conv(&mut context, &Term::hole(0), &Term::hole(1)),
        Ok(false)
    );
}

#[test]
fn conversion_cannot_solve_a_protected_recursive_slot() {
    let mut context = context();
    let (id, slot) = context.fresh_rec_slot(Term::type_ground());
    let nat_type = Term::intrinsic(Intrinsic::NatType);

    assert!(matches!(
        convert_outcome(&mut context, &Term::type_ground(), &slot, &nat_type),
        Ok(Outcome::Blocked(_))
    ));
    assert!(context.metavar_solution(id).is_none());

    context.fill_rec_slot(id, nat_type.clone());
    assert_eq!(reduce(&mut context, slot), Ok(nat_type));
}

#[test]
fn embedded_metavar_within_the_target_context_commits() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    context.birth_metavar(MetavarId(1), Vec::new(), Term::type_ground());

    // ?0 ≟ (x : ?1) -> Nat — ?1 is unsolved but its birth context is contained in ?0's, so nothing it can ever inject escapes ?0's scope: the forced solution commits with ?1 riding embedded, instead of stranding as a residual. This is what lets a settle-synthesized lambda type pin its expectation while a domain metavariable is still open.
    let candidate = Term::func_type(
        [(x.clone(), Term::hole(1))],
        Term::intrinsic(Intrinsic::NatType),
    );
    assert_eq!(conv(&mut context, &Term::hole(0), &candidate), Ok(true));
    assert!(context.metavar_solution(MetavarId(0)).is_some());
    assert_eq!(context.metavar_solution(MetavarId(1)), None);
}

#[test]
fn embedded_metavar_of_a_wider_context_postpones_to_residual() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    context.birth_metavar(
        MetavarId(1),
        vec![(y.clone(), Term::intrinsic(Intrinsic::NatType))],
        Term::type_ground(),
    );

    // ?0 ≟ (x : ?1) -> Nat — ?1's birth context holds a binder ?0's lacks, so its eventual solution could smuggle `y` past ?0's scope: postponed, the stand-in for pruning.
    let candidate = Term::func_type(
        [(x.clone(), Term::hole(1))],
        Term::intrinsic(Intrinsic::NatType),
    );
    assert_eq!(conv(&mut context, &Term::hole(0), &candidate), Ok(false));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn revalidation_rejects_ill_typed_solution() {
    let mut context = context();
    // ?0 : Nat under empty Γ. A candidate of type Type (e.g. `Bool`) does not type-check against Nat, so re-validation rejects it.
    context.birth_metavar(
        MetavarId(0),
        Vec::new(),
        Term::intrinsic(Intrinsic::NatType),
    );

    let bool_ = Term::intrinsic(Intrinsic::BoolType);
    assert_eq!(conv(&mut context, &Term::hole(0), &bool_), Ok(false));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn revalidation_suppresses_refinements_rejecting_a_refined_solution() {
    // The regression this guards against: Γ = (t : Type) with a counterfactual match-arm refinement `t := Nat` in force (as inside `bool_match b { true => ... }`, where the family `T(b) ⇝ Nat`). `?0 : t` is born under the *frozen* Γ = (t : Type) — its result type depends on the refined head, mirroring `m : T(b)`.
    let mut context = context();
    let t_binder = context.fresh(Some("t"));
    context.assume(&t_binder, &Term::type_ground());
    context.refine(&t_binder, &Term::intrinsic(Intrinsic::NatType));
    context.birth_metavar(
        MetavarId(0),
        vec![(t_binder.clone(), Term::type_ground())],
        Term::free_var(&t_binder),
    );

    // `?0 ≟ 5` at type `t`. Locally (refinement on) `t ⇝ Nat` and `5 : t` holds, but re-validation suppresses refinements, leaving `t` abstract, so `5 : t` fails and the solution is rejected — the program is unsound otherwise.
    let t = Term::free_var(&t_binder);
    let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![t.clone()]);
    let five = Term::intrinsic(Intrinsic::Nat(Nat::new(5usize)));
    assert_eq!(convert(&mut context, &t, &occurrence, &five), Ok(false));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn revalidation_accepts_a_refinement_independent_solution() {
    // The mirror case of `revalidation_suppresses_refinements_rejecting_a_refined_solution`. The same refinement `t := Nat` is in force, but `?0`'s result type is `Nat` directly — it does not depend on the refined head. Re-validation checks `5 : Nat` with refinements suppressed (none are needed) and commits.
    let mut context = context();
    let t = context.fresh(Some("t"));
    context.assume(&t, &Term::type_ground());
    context.refine(&t, &Term::intrinsic(Intrinsic::NatType));
    context.birth_metavar(
        MetavarId(0),
        vec![(t.clone(), Term::type_ground())],
        Term::intrinsic(Intrinsic::NatType),
    );

    let nat = Term::intrinsic(Intrinsic::NatType);
    let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&t)]);
    let five = Term::intrinsic(Intrinsic::Nat(Nat::new(5usize)));
    assert_eq!(convert(&mut context, &nat, &occurrence, &five), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&five));
}

#[test]
fn inverts_a_renaming() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let y = context.fresh(Some("y"));
    // ?0 born under Γ = [a : Nat]; this occurrence's spine maps `a` to the live name `y` (the enclosing binders were re-closed and reopened).
    context.birth_metavar(MetavarId(0), vec![(a.clone(), nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&y)]);

    // ?0[y] ≟ y — inverting the renaming stores the solution in birth-named form: `a`, not `y`.
    assert_eq!(
        conv(&mut context, &occurrence, &Term::free_var(&y)),
        Ok(true)
    );
    assert_eq!(
        context.metavar_solution(MetavarId(0)),
        Some(&Term::free_var(&a))
    );
}

#[test]
fn solve_through_an_identity_spine_matches_legacy() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    context.birth_metavar(MetavarId(0), vec![(a.clone(), nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&a)]);

    // The identity spine behaves exactly like the empty (legacy bare-hole) spine: the candidate is stored unchanged.
    assert_eq!(conv(&mut context, &occurrence, &nat(1)), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&nat(1)));
}

#[test]
fn postpones_a_duplicated_renaming() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let y = context.fresh(Some("y"));
    context.birth_metavar(
        MetavarId(0),
        vec![(a.clone(), nat_type()), (b.clone(), nat_type())],
        nat_type(),
    );
    // Both entries are the same live name: which birth binder `y` stands for is ambiguous, so a candidate mentioning it is undecided, not unequal.
    let occurrence = Term::metavar_birthed(
        0,
        MetavarOrigin::Hole,
        vec![Term::free_var(&y), Term::free_var(&y)],
    );

    let outcome = convert_outcome(
        &mut context,
        &Term::type_ground(),
        &occurrence,
        &Term::free_var(&y),
    );
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn prunes_dependence_on_a_non_pattern_entry() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let z = context.fresh(Some("z"));
    let y = context.fresh(Some("y"));
    context.birth_metavar(
        MetavarId(0),
        vec![(a.clone(), nat_type()), (b.clone(), nat_type())],
        nat_type(),
    );
    // First slot a pattern variable, second a compound term: the candidate may depend on the first but not (yet) on the second.
    let compound: Term = Subterm::Intrinsic(Intrinsic::nat_add(Term::free_var(&z), nat(1))).into();
    let occurrence = Term::metavar_birthed(
        0,
        MetavarOrigin::Hole,
        vec![Term::free_var(&y), compound.clone()],
    );

    // ?0[y, z+1] ≟ y — solvable through the pattern slot alone.
    assert_eq!(
        conv(&mut context, &occurrence, &Term::free_var(&y)),
        Ok(true)
    );
    assert_eq!(
        context.metavar_solution(MetavarId(0)),
        Some(&Term::free_var(&a))
    );
}

#[test]
fn postpones_a_candidate_reaching_through_a_non_pattern_entry() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let z = context.fresh(Some("z"));
    let y = context.fresh(Some("y"));
    context.birth_metavar(
        MetavarId(0),
        vec![(a.clone(), nat_type()), (b.clone(), nat_type())],
        nat_type(),
    );
    let compound: Term = Subterm::Intrinsic(Intrinsic::nat_add(Term::free_var(&z), nat(1))).into();
    let occurrence =
        Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&y), compound]);

    // ?0[y, z+1] ≟ z — `z` is reachable only through the non-pattern slot (and is not an occurrence of the whole entry): undecided.
    let outcome = convert_outcome(
        &mut context,
        &Term::type_ground(),
        &occurrence,
        &Term::free_var(&z),
    );
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn rejects_an_out_of_image_variable() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let y = context.fresh(Some("y"));
    let z = context.fresh(Some("z"));
    context.birth_metavar(MetavarId(0), vec![(a.clone(), nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&y)]);

    // ?0[y] ≟ z — `z` corresponds to no birth binder and never can: a hard mismatch, not a postponement.
    let outcome = convert_outcome(
        &mut context,
        &Term::type_ground(),
        &occurrence,
        &Term::free_var(&z),
    );
    assert!(matches!(outcome, Ok(Outcome::Mismatch)));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn classifies_a_solved_metavariable_spine_entry_by_its_value() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let y = context.fresh(Some("y"));
    let b = context.fresh(Some("b"));
    // ?0 is already solved to its own binder, so an occurrence ?0[y] stands for `y` — a perfectly good pattern variable hiding behind a node.
    context.birth_metavar(MetavarId(0), vec![(a.clone(), nat_type())], nat_type());
    context.solve_metavar(MetavarId(0), Term::free_var(&a));
    let entry = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&y)]);

    context.birth_metavar(MetavarId(1), vec![(b.clone(), nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(1, MetavarOrigin::Hole, vec![entry]);

    // ?1[?0[y]] ≟ y — the entry resolves to `y` and inverts to `b`.
    assert_eq!(
        conv(&mut context, &occurrence, &Term::free_var(&y)),
        Ok(true)
    );
    assert_eq!(
        context.metavar_solution(MetavarId(1)),
        Some(&Term::free_var(&b))
    );
}

#[test]
fn abstracts_a_non_pattern_occurrence() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let z = context.fresh(Some("z"));
    let y = context.fresh(Some("y"));
    context.birth_metavar(
        MetavarId(0),
        vec![(a.clone(), nat_type()), (b.clone(), nat_type())],
        nat_type(),
    );
    // A reduce-stable compound (a tuple is a normal form), matched by the raw spelling; the reduced-spelling case is the next test.
    let compound = Term::tuple([Term::free_var(&z)]);
    let occurrence = Term::metavar_birthed(
        0,
        MetavarOrigin::Hole,
        vec![Term::free_var(&y), compound.clone()],
    );

    // ?0[y, (z,)] ≟ (z,) — the candidate *is* an occurrence of the non-pattern entry, which abstracts to its birth binder `b`.
    assert_eq!(conv(&mut context, &occurrence, &compound), Ok(true));
    assert_eq!(
        context.metavar_solution(MetavarId(0)),
        Some(&Term::free_var(&b))
    );
}

// === Parked-constraint retries ==============================================

#[test]
fn parked_goals_retry_under_their_frozen_refinements() {
    let mut context = context();
    let b = context.fresh(Some("b"));

    // Park (inside an arm-like frame) a goal that converts only through the frame's counterfactual refinement: `b` reduces to `Nat` via `refine`, not via any definition.
    context.with_frame(|context| {
        context.assume(&b, &Term::type_ground());
        context.refine(&b, &nat_type());
        context.park(
            ParkedWork::Conversion(Problem {
                type_: Term::type_ground(),
                this: Term::free_var(&b),
                that: nat_type(),
            }),
            Term::free_var(&b),
        );
    });

    // The frame is gone; the drain retries under the frozen one, where the refinement still holds and the goal converts.
    assert!(context.drain_parked().is_ok());
}

#[test]
fn parked_goals_without_their_refinement_mismatch() {
    let mut context = context();
    let b = context.fresh(Some("b"));

    // Control: the same goal parked without the refinement cannot convert, and the drain reports it at its origin.
    context.with_frame(|context| {
        context.assume(&b, &Term::type_ground());
        context.park(
            ParkedWork::Conversion(Problem {
                type_: Term::type_ground(),
                this: Term::free_var(&b),
                that: nat_type(),
            }),
            Term::free_var(&b),
        );
    });

    assert!(context.drain_parked().is_err());
}

#[test]
fn abstracts_a_reduced_spelling_occurrence() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let z = context.fresh(Some("z"));
    let y = context.fresh(Some("y"));
    context.birth_metavar(
        MetavarId(0),
        vec![(a.clone(), nat_type()), (b.clone(), nat_type())],
        nat_type(),
    );
    // `z + 1` successor-peels under reduction, and the candidate side arrives reduced — each subject contributes both spellings, so the occurrence still abstracts, and the round-trip verification accepts the pair by definitional (not syntactic) equality.
    let compound: Term = Subterm::Intrinsic(Intrinsic::nat_add(Term::free_var(&z), nat(1))).into();
    let occurrence = Term::metavar_birthed(
        0,
        MetavarOrigin::Hole,
        vec![Term::free_var(&y), compound.clone()],
    );

    assert_eq!(conv(&mut context, &occurrence, &compound), Ok(true));
    assert_eq!(
        context.metavar_solution(MetavarId(0)),
        Some(&Term::free_var(&b))
    );
}

#[test]
fn flex_flex_same_id_converts_through_equal_spines() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    context.birth_metavar(MetavarId(0), vec![(a.clone(), nat_type())], nat_type());

    // Two occurrences of the same unsolved metavariable whose spines differ syntactically but agree definitionally (`1 + 1` reduces to `2`): the congruence probe discharges the goal without solving anything.
    let sum: Term = Subterm::Intrinsic(Intrinsic::nat_add(nat(1), nat(1))).into();
    let this = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![sum]);
    let that = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![nat(2)]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn flex_flex_same_id_with_disagreeing_spines_stays_blocked() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    context.birth_metavar(MetavarId(0), vec![(a.clone(), nat_type())], nat_type());

    // Disagreeing spines are not *unequal* — the solution may ignore the slot — so the pair parks rather than mismatching.
    let this = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![nat(1)]);
    let that = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![nat(2)]);

    let outcome = convert_outcome(&mut context, &Term::type_ground(), &this, &that);
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
}

#[test]
fn flex_flex_distinct_heads_with_a_common_solution_stays_blocked() {
    // The intersection wontfix's witness, pinned: two *distinct* unsolved metavariables over compatible telescopes, met through the same live name. Flex–flex assignment (`?0 := ?1` through the renaming) would discharge this; v1 does no intersection, so the pair parks and — with nothing else to pin either head — stays undecided. When intersection is built, this test should flip to `Converts` with `?0` solved to an occurrence of `?1` (and this comment retired).
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let x = context.fresh(Some("x"));
    context.birth_metavar(MetavarId(0), vec![(a.clone(), nat_type())], nat_type());
    context.birth_metavar(MetavarId(1), vec![(b.clone(), nat_type())], nat_type());

    let this = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&x)]);
    let that = Term::metavar_birthed(1, MetavarOrigin::Hole, vec![Term::free_var(&x)]);

    let outcome = convert_outcome(&mut context, &Term::type_ground(), &this, &that);
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
    assert_eq!(context.metavar_solution(MetavarId(1)), None);
}

#[test]
fn rollback_solutions_unwinds_to_the_mark() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    context.birth_metavar(MetavarId(0), vec![(a.clone(), nat_type())], nat_type());
    context.birth_metavar(MetavarId(1), vec![(a.clone(), nat_type())], nat_type());

    context.solve_metavar(MetavarId(0), nat(1));
    let mark = context.solution_mark();
    context.solve_metavar(MetavarId(1), nat(2));

    context.rollback_solutions(mark);

    // The solution past the mark is unwound; the one before it survives. This is the bracket `solve` wraps around re-validation, so a rejected candidate's nested solves leave no fingerprints.
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&nat(1)));
    assert_eq!(context.metavar_solution(MetavarId(1)), None);
}

#[test]
fn stuck_intrinsic_on_a_metavar_parks_instead_of_mismatching() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    context.birth_metavar(MetavarId(0), vec![(a.clone(), nat_type())], nat_type());
    let m = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&a)]);
    let stuck: Term = Subterm::Intrinsic(Intrinsic::NatSub(m.clone(), nat(1))).into();

    // `?0 - 1 ≈ 0` is undecided, not unequal: solving `?0` may fold the subtraction. (`NatAdd` escapes via successor peeling; the other operators rely on this parking.)
    let outcome = convert_outcome(&mut context, &Term::type_ground(), &stuck, &nat(0));
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);

    // Within one run, a sibling goal pins `?0 := 1`; the parked subtraction is retried, folds to `0`, and converts.
    let this = Term::tuple([stuck, m]);
    let that = Term::tuple([nat(0), nat(1)]);
    assert_eq!(conv(&mut context, &this, &that), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&nat(1)));
}

#[test]
fn rigid_head_mismatch_with_a_metavar_inside_still_fails_fast() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    context.birth_metavar(MetavarId(0), vec![(a.clone(), nat_type())], nat_type());
    let m = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&a)]);

    // An inductive type against `Nat` is provably unequal whatever `?0` becomes — the heads are rigid — so the mismatch stays hard (and is reported at the use site, not deferred to the drain).
    let induct_decl = Term::induct_type(nominal("Vec"), [m], Vec::<Term>::new());
    let outcome = convert_outcome(
        &mut context,
        &Term::type_ground(),
        &induct_decl,
        &nat_type(),
    );
    assert!(matches!(outcome, Ok(Outcome::Mismatch)));
}

#[test]
fn arm_refinement_does_not_taint_a_committed_solution() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    context.assume(&n, &nat_type());
    context.birth_metavar(MetavarId(0), vec![(n.clone(), nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&n)]);

    // Inside a frame that counterfactually refines `n := 0` (a match arm), the goal `?0[n] ≈ n` still discharges — but the *committed* solution is the refinement-free `n`, not the arm-local `0`: a metavariable must not be pinned to a value that holds only counterfactually inside the arm.
    let converts = context.with_frame(|context| {
        context.refine(&n, &nat(0));
        conv(context, &occurrence, &Term::free_var(&n))
    });
    assert_eq!(converts, Ok(true));
    assert_eq!(
        context.metavar_solution(MetavarId(0)),
        Some(&Term::free_var(&n))
    );
}

#[test]
fn eta_at_unit_trusts_the_goal_type_label() {
    let mut context = context();

    // Pinned wart, internal to the conversion API: when one side is the unit tuple literal `()`, `eta_expand_tuple` enqueues one goal per field — zero — and succeeds *without ever confirming the goal's type reduces to `{}`. So the kernel, asked directly, judges `() ≈ 1` at type `Nat`. Elaboration never produces a heterotyped goal (both sides of every `expect`/index comparison were checked at the same type), so this is not reachable from the surface language — but the conversion entry point is only sound under that caller invariant. If η-at-unit ever gates on the type actually being a 0-ary tuple type, flip this to `Ok(false)`.
    assert_eq!(
        convert(
            &mut context,
            &nat_type(),
            &Term::tuple(Vec::<Term>::new()),
            &nat(1)
        ),
        Ok(true)
    );
}

/// Two goals distinct under their binder types land on one history fingerprint; see `documentation/soundness/per-term-rules/conversion-recurrence.md`.
///
/// `history_key` renames the openings a conversion minted to placeholders by mint order and records no local context, so the body goals two telescope walks open — one under a `Nat` binder, one under a `Bool` binder, minted apart — rename onto the same entry. The drain consults `in_history` before the structural dispatch, so when both arise in one run the second is *assumed* rather than compared. The goals here are built through the same `compare_func_type` walk the drain dispatches to, and while the hole was open to attack, the collision was confirmed to fire inside a real drain too: instrumenting the drain's history hit showed `a_goal_assumed_by_key_collision_cannot_move_the_verdict`'s `Bool`-bound goal skipped on the `Nat`-bound goal's entry, in both of that fixture's halves.
///
/// What keeps the assumption from admitting anything is not the key but the openings' uniformity, which the fixture below holds in both directions.
#[test]
fn two_goals_distinct_under_their_binder_types_share_one_history_key() {
    let mut context = context();
    let f = context.fresh(Some("f"));
    let g = context.fresh(Some("g"));
    let x = context.fresh(Some("x"));

    // `(x : domain) -> head(x)` — the walk compares the domains as a sibling goal and opens the bodies at a shared fresh binder, so the domain is exactly the part of the goal's provenance the key forgets.
    let arrow = |domain: Term, head: &Free| {
        as_func_type(Term::func_type(
            [(x.clone(), domain)],
            Term::apply(Term::free_var(head), [Term::free_var(&x)]),
        ))
    };

    let mut cmp = Convert::new(
        Term::type_ground(),
        Term::type_ground(),
        Term::type_ground(),
    );
    cmp.pending.clear();

    let nat = Term::intrinsic(Intrinsic::NatType);
    let bool_ = Term::intrinsic(Intrinsic::BoolType);
    assert_eq!(
        cmp.compare_func_type(&mut context, arrow(nat.clone(), &f), arrow(nat, &g)),
        Ok(true)
    );
    assert_eq!(
        cmp.compare_func_type(&mut context, arrow(bool_.clone(), &f), arrow(bool_, &g)),
        Ok(true)
    );

    // Each walk enqueued its domain goal then its body goal: [Nat ≡ Nat, f v1 ≡ g v1, Bool ≡ Bool, f v2 ≡ g v2].
    assert_eq!(cmp.pending.len(), 4);
    let under_nat = cmp.pending[1].clone();
    let under_bool = cmp.pending[3].clone();
    assert_ne!(under_nat, under_bool);

    let first = cmp.history_key(&mut context, &under_nat);
    let second = cmp.history_key(&mut context, &under_bool);
    assert_eq!(first, second);

    // The drain's guard: the first visit inserts, and the collided second hits — the branch that assumes rather than compares.
    assert!(!cmp.in_history(&first));
    assert!(cmp.in_history(&second));
}

/// A goal assumed through a cross-branch key collision cannot move the verdict, and the reason is the openings' uniformity rather than the key: `Context::fresh` mints a bare label, no rule assumes a type for it, and `synth_neutral` and `apply_param_types` answer `None` for one, so every rule behind the history guard treats two openings alike. A collided goal is therefore its twin under an injective relabeling of openings, and whatever finite disagreement it hides is also the twin's, whose own children surface it — the refusing half below. That uniformity is an inventory of the same kind `ground_scope`'s is in `curios-cert`: a rule taught to read an opening's type would invalidate it in silence, and this fixture is what would notice.
///
/// Both halves run the collision the fixture above demonstrates at the key level, inside a real drain: the tuple type's two function-type fields put the `Nat`-bound body goal in history first, and the `Bool`-bound twin arrives at the same fingerprint and is assumed (confirmed by instrumentation while this probe was written — see the fixture above).
#[test]
fn a_goal_assumed_by_key_collision_cannot_move_the_verdict() {
    let mut context = context();
    let c = context.fresh(Some("c"));
    let p = context.fresh(Some("p"));
    let q = context.fresh(Some("q"));
    let x = context.fresh(Some("x"));

    // `{ p : (x : Nat) -> c(x, d), q : (x : Bool) -> c(x, d) }`, one side per `d`. The head `c` stays an untyped neutral so both body goals stick, and `d` is the only place the sides disagree.
    let side = |d: &Term| {
        Term::tuple_type([
            (
                p.clone(),
                Term::func_type(
                    [(x.clone(), Term::intrinsic(Intrinsic::NatType))],
                    Term::apply(Term::free_var(&c), [Term::free_var(&x), d.clone()]),
                ),
            ),
            (
                q.clone(),
                Term::func_type(
                    [(x.clone(), Term::intrinsic(Intrinsic::BoolType))],
                    Term::apply(Term::free_var(&c), [Term::free_var(&x), d.clone()]),
                ),
            ),
        ])
    };

    // Accepting: `one` and `uno` are distinct spellings of one definition, so the sides differ as terms (weak-head reduction leaves spine arguments unread), the `Bool` goal is assumed on the `Nat` goal's entry, and every goal actually compared agrees.
    let one = context.fresh(Some("one"));
    let uno = context.fresh(Some("uno"));
    context.define(&one, &nat(1), None);
    context.define(&uno, &nat(1), None);
    assert_eq!(
        conv(
            &mut context,
            &side(&Term::free_var(&one)),
            &side(&Term::free_var(&uno))
        ),
        Ok(true)
    );

    // Refusing: the assumed `Bool` goal hides a real disagreement (`1` against `2`), and the conversion still refuses — the same disagreement is the `Nat` twin's, and the twin's spine child entered the queue before the collision was consulted.
    assert_eq!(
        conv(&mut context, &side(&nat(1)), &side(&nat(2))),
        Ok(false)
    );
}
