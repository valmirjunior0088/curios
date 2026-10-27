//! Revalidating a solution: checked at the arm a metavariable was born in, and a metavariable a candidate embeds restricted to what it may reach.

use {
    super::test_support::*,
    crate::*,
    curios_core::{Intrinsic, MetavarId, MetavarOrigin, Nat, Subterm, Term},
};

#[test]
fn revalidation_admits_checkable_but_not_inferable_candidate() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));
    // ?0 : (x : Nat, y : Nat) — a tuple type, born in empty Γ.
    let pair_type = Term::tuple_type([
        (x, Term::intrinsic(Intrinsic::NatType)),
        (y, Term::intrinsic(Intrinsic::NatType)),
    ]);
    context.birth_metavar(MetavarId(0), Vec::new(), pair_type);

    // ?0 ≟ (1, 2). A bare tuple has no synthesizable type (`elaborate_tuple` is Check-only), so synthesize-then-convert re-validation would reject it; checking it against the frozen tuple result type admits it.
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
        (x, Term::intrinsic(Intrinsic::NatType)),
        (y, Term::intrinsic(Intrinsic::NatType)),
    ]);
    context.birth_metavar(MetavarId(0), Vec::new(), pair_type);

    // ?0 ≟ (1, 2, 3): a three-field tuple does not check against a two-field tuple type, so checking still rejects the candidate and commits nothing.
    let wrong = Term::tuple([nat(1), nat(2), nat(3)]);
    assert_eq!(conv(&mut context, &Term::hole(0), &wrong), Ok(false));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn embedded_metavar_within_the_target_context_commits() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    context.birth_metavar(MetavarId(1), Vec::new(), Term::type_ground());

    // ?0 ≟ (x : ?1) -> Nat — ?1 is unsolved but its birth context is contained in ?0's, so nothing it can ever inject escapes ?0's scope: the forced solution commits with ?1 riding embedded, instead of stranding as a residual. This is what lets a settle-synthesized lambda type pin its expectation while a domain metavariable is still open.
    let candidate = Term::func_type([(x, Term::hole(1))], Term::intrinsic(Intrinsic::NatType));
    assert_eq!(conv(&mut context, &Term::hole(0), &candidate), Ok(true));
    assert!(context.metavar_solution(MetavarId(0)).is_some());
    assert_eq!(context.metavar_solution(MetavarId(1)), None);
}

// A candidate may embed a metavariable born under more than the solved one's context where the candidate binds the rest itself: `?0 ≟ (x: Nat) -> ?1`, with `?1` born under `x` and standing under the function type's own binder. Whatever `?1` comes to arrives through that occurrence and names nothing the function type does not bind, so the candidate commits. It is what a settled lambda's codomain is in the type the lambda states.
#[test]
fn a_candidate_embeds_a_metavariable_born_under_a_binder_of_its_own() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    context.birth_metavar(
        MetavarId(1),
        vec![(x, Term::intrinsic(Intrinsic::NatType))],
        Term::type_ground(),
    );

    let codomain = Term::metavar_birthed(1, MetavarOrigin::Hole, vec![Term::free_var(&x)]);
    let candidate = Term::func_type([(x, Term::intrinsic(Intrinsic::NatType))], codomain);
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
        vec![(y, Term::intrinsic(Intrinsic::NatType))],
        Term::type_ground(),
    );

    // ?0 ≟ (x : ?1) -> Nat — ?1's birth context holds a binder ?0's lacks, so its eventual solution could smuggle `y` past ?0's scope. This bare occurrence says nothing of what `y` is at it, so ?1 cannot be re-expressed over ?0's binders, and the candidate waits.
    let candidate = Term::func_type([(x, Term::hole(1))], Term::intrinsic(Intrinsic::NatType));
    assert_eq!(conv(&mut context, &Term::hole(0), &candidate), Ok(false));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

/// A metavariable born in an arm, embedded in a candidate for one born outside it, is restricted to the outer one's refinements as the candidate commits rather than holding it back: `?0 ≟ (x : ?1) -> Nat` solves, and `?1` is solved to a fresh metavariable born under no refinement, whose own solution is then judged without the arm's guard. The control is the same candidate over a written goal, which reports by its identity and still waits. Mutation-checked: postponing on refinements, as containment alone does, leaves `?0` unsolved.
#[test]
fn a_metavariable_born_in_an_arm_is_restricted_to_the_candidate_that_embeds_it() {
    let embedding = |origin: MetavarOrigin| {
        let mut context = context();
        let b = context.fresh(Some("b"));
        let x = context.fresh(Some("x"));
        let bool_type = Term::intrinsic(Intrinsic::BoolType);
        context.assume(&b, &bool_type);
        context.birth_metavar(
            MetavarId(0),
            vec![(b, bool_type.clone())],
            Term::type_ground(),
        );
        context.with_frame(|context| {
            context.refine(&b, &Term::intrinsic(Intrinsic::Bool(true)));
            context.birth_metavar(
                MetavarId(1),
                vec![(b, bool_type.clone())],
                Term::type_ground(),
            );
        });

        let spine = vec![Term::free_var(&b)];
        let outer = Term::metavar_birthed(0, MetavarOrigin::Hole, spine.clone());
        let candidate = Term::func_type(
            [(x, Term::metavar_birthed(1, origin, spine))],
            Term::intrinsic(Intrinsic::NatType),
        );
        let converted = conv(&mut context, &outer, &candidate);
        (context, converted)
    };

    let (context, converted) = embedding(MetavarOrigin::Domain(Some("x".into())));
    assert_eq!(converted, Ok(true));
    assert!(context.metavar_solution(MetavarId(0)).is_some());
    let Some(Subterm::Metavar(narrowed)) = context
        .metavar_solution(MetavarId(1))
        .map(|solution| &**solution)
    else {
        panic!("the embedded metavariable is solved to its restriction");
    };
    assert!(
        context
            .metavar_entry(narrowed.id)
            .is_some_and(|entry| !entry.frames().refined() && entry.solution().is_none()),
        "the restriction holds the frames the two share, which register no refinement, and is left open"
    );

    let (context, converted) = embedding(MetavarOrigin::Goal);
    assert_eq!(converted, Ok(false));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

/// A silent hole — an elided annotation, motive or element type — is restricted like an omitted implicit, while a parked check's placeholder, minted the same way, waits: its check reads any solution as the check discharged and would never elaborate its term. The two share their origin and differ in kind. Mutation-checked: restricting a placeholder commits the control.
#[test]
fn a_silent_hole_is_restricted_where_a_parked_checks_placeholder_waits() {
    let embedding = |placeholder: bool| {
        let mut context = context();
        let x = context.fresh(Some("x"));
        let y = context.fresh(Some("y"));
        context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
        let hole = context.with_frame(|context| {
            context.assume(&y, &nat_type());
            match placeholder {
                true => context.fresh_placeholder(Term::type_ground(), None).1,
                false => context.fresh_hole_metavar(Term::type_ground(), None),
            }
        });

        let candidate = Term::func_type([(x, hole)], nat_type());
        let converted = conv(&mut context, &Term::hole(0), &candidate);
        (context, converted)
    };

    let (context, converted) = embedding(false);
    assert_eq!(converted, Ok(true));
    assert!(context.metavar_solution(MetavarId(0)).is_some());

    let (context, converted) = embedding(true);
    assert_eq!(converted, Ok(false));
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
fn revalidation_withholds_an_arm_a_metavariable_was_not_born_in() {
    // Γ = (t : Type), and `?0 : t` born *before* the arm's refinement `t := Nat` is registered — outside the arm, as a metavariable of an enclosing call is. Its result type depends on the refined head, mirroring `m : T(b)`.
    let mut context = context();
    let t_binder = context.fresh(Some("t"));
    context.assume(&t_binder, &Term::type_ground());
    context.birth_metavar(
        MetavarId(0),
        vec![(t_binder, Term::type_ground())],
        Term::free_var(&t_binder),
    );

    // `?0 ≟ 5` at type `t`, asked in the arm. There `t ⇝ Nat` and `5 : t` holds, but re-validation stands on the frames `?0` was born under, which hold nothing of the arm, leaving `t` abstract, so `5 : t` fails and the solution is rejected: `?0` also stands outside the arm, where `5 : t` is false.
    context.with_frame(|context| {
        context.refine(&t_binder, &Term::intrinsic(Intrinsic::NatType));

        let t = Term::free_var(&t_binder);
        let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![t.clone()]);
        let five = Term::intrinsic(Intrinsic::Nat(Nat::new(5usize)));
        assert_eq!(convert(context, &t, &occurrence, &five), Ok(false));
    });
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn revalidation_keeps_the_arm_a_metavariable_was_born_in() {
    // The mirror: `?0 : t` born *after* the refinement `t := Nat` — inside the arm, where all of its occurrences are. Re-validation stands under the arm's refinement, `5 : t` holds there, and the solution commits; withholding it would refuse a solution resting on the arm's own guard.
    let mut context = context();
    let t_binder = context.fresh(Some("t"));
    context.assume(&t_binder, &Term::type_ground());
    context.refine(&t_binder, &Term::intrinsic(Intrinsic::NatType));
    context.birth_metavar(
        MetavarId(0),
        vec![(t_binder, Term::type_ground())],
        Term::free_var(&t_binder),
    );

    let t = Term::free_var(&t_binder);
    let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![t.clone()]);
    let five = Term::intrinsic(Intrinsic::Nat(Nat::new(5usize)));
    assert_eq!(convert(&mut context, &t, &occurrence, &five), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&five));
}

#[test]
fn a_metavariable_born_in_an_arm_is_not_contained_in_one_born_outside_it() {
    // A `Bool` arm opens no binder, so the two metavariables share one telescope; what tells them apart is the refinement the inner one was born under. Contained by names alone, the inner one could carry a solution resting on the arm's guard into the outer one's.
    let mut context = context();
    let b = context.fresh(Some("b"));
    let bool_type = || Term::intrinsic(Intrinsic::BoolType);
    context.assume(&b, &bool_type());
    context.birth_metavar(MetavarId(0), vec![(b, bool_type())], nat_type());
    context.with_frame(|context| {
        context.refine(&b, &Term::intrinsic(Intrinsic::Bool(true)));
        context.birth_metavar(MetavarId(1), vec![(b, bool_type())], nat_type());
    });

    assert!(!context.metavar_context_contained(MetavarId(1), MetavarId(0)));
    assert!(context.metavar_context_contained(MetavarId(0), MetavarId(1)));
}

#[test]
fn revalidation_accepts_a_refinement_independent_solution() {
    // The mirror of `revalidation_withholds_an_arm_a_metavariable_was_not_born_in`: `?0` is again born before the arm's refinement `t := Nat`, but its result type is `Nat` directly — it does not depend on the refined head. Re-validation checks `5 : Nat` without the arm's refinement, which it does not need, and commits.
    let mut context = context();
    let t = context.fresh(Some("t"));
    context.assume(&t, &Term::type_ground());
    context.birth_metavar(
        MetavarId(0),
        vec![(t, Term::type_ground())],
        Term::intrinsic(Intrinsic::NatType),
    );

    let five = Term::intrinsic(Intrinsic::Nat(Nat::new(5usize)));
    context.with_frame(|context| {
        context.refine(&t, &Term::intrinsic(Intrinsic::NatType));

        let nat = Term::intrinsic(Intrinsic::NatType);
        let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&t)]);
        assert_eq!(convert(context, &nat, &occurrence, &five), Ok(true));
    });
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&five));
}

#[test]
fn arm_refinement_does_not_taint_a_committed_solution() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    context.assume(&n, &nat_type());
    context.birth_metavar(MetavarId(0), vec![(n, nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&n)]);

    // Inside a frame that counterfactually refines `n := 0` (a match arm), the goal `?0[n] ≈ n` still discharges — but the *committed* solution is `n`, not the arm-local `0`: `?0` was born outside the arm, and a metavariable must not be pinned to a value that holds only inside an arm it was not born in.
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
