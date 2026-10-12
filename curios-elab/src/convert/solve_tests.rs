//! Metavariable solving: flex-rigid, occurs and scope checks, pattern inversion and pruning, flex-flex, and revalidation.

use {
    super::test_support::*,
    crate::*,
    curios_core::{
        Atom, CalleeId, DefinitionKind, Free, InductDecl, InductParam, Intrinsic, Level, MetavarId,
        MetavarOrigin, Nat, Quality, Subterm, Telescope, Term, UniverseContext, WitnessOrigin,
    },
    curios_utilities::{Plicity, Qualifier},
};

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

/// Abstracting a subject out of a candidate costs the candidate's graph, not its tree: each level sums the one below with itself, sixty over, and the subject at the base is abstracted once. The result stays a graph — both operands of its root are one node.
#[test]
fn abstraction_walks_a_shared_candidate_once_per_node() {
    let mut context = context();
    let f = context.fresh(Some("f"));
    let birth = context.fresh(Some("b"));
    let subject = Term::apply(Term::free_var(&f), [nat(1)]);

    let mut candidate = subject.clone();
    for _ in 0..60 {
        candidate = Term::intrinsic(Intrinsic::nat_add(candidate.clone(), candidate));
    }
    let abstracted = super::abstract_occurrences(&candidate, &[(subject, birth)]);

    assert!(!abstracted.mentions_free(&f));
    assert!(abstracted.mentions_free(&birth));
    let Subterm::Intrinsic(Intrinsic::NatAdd(left, right)) = &*abstracted else {
        panic!("the abstraction changed the root: {abstracted}");
    };
    assert!(std::ptr::eq::<Subterm>(&**left, &**right));
}

#[test]
fn occurs_check_rejects_cyclic_solution() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());

    // ?0 ≟ (x : ?0) -> Nat  — the candidate mentions ?0 itself.
    let cyclic = Term::func_type([(x, Term::hole(0))], Term::intrinsic(Intrinsic::NatType));
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
        vec![(x_binder, Term::type_ground())],
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
        (x, Term::intrinsic(Intrinsic::NatType)),
        (y, Term::intrinsic(Intrinsic::NatType)),
    ]);
    context.birth_metavar(MetavarId(0), Vec::new(), pair_type);

    // ?0 ≟ (1, 2). A bare tuple has no synthesizable type (`elaborate_tuple` is Check-only), so synthesize-then-convert re-validation would reject it; checking it against the frozen tuple result type admits it.
    let pair = Term::tuple([nat(1), nat(2)]);
    assert_eq!(conv(&mut context, &Term::hole(0), &pair), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&pair));
}

// What is committed is the term the check rebuilt. A lambda annotated below the domain it is solved at checks, its annotation against that domain by subsumption, and is committed at the domain, which is the annotation the kernel requires of it; committed as it came, it is a solution the elaborator accepts and the kernel refuses. The same holds of a lambda beneath a tuple, which the check reaches through the tuple's field type.
#[test]
fn a_candidate_is_committed_as_its_check_rebuilt_it() {
    let mut context = context();
    let carrier = context.fresh(Some("T"));
    let field = context.fresh(Some("f"));
    let sort = |level: u32| Term::type_at(Level::constant(level));
    let identity = |level: u32| Term::func([(carrier, sort(level))], Term::free_var(&carrier));
    let family = Term::func_type([(carrier, sort(1))], sort(1));
    let annotation = |term: &Term| {
        let Subterm::Func(func) = &**term else {
            panic!("not a lambda: {term}");
        };
        func.telescope
            .cursor()
            .entry()
            .expect("a lambda binds a parameter")
            .1
    };

    // ?0 : (Type 1) -> Type 1, against the identity annotated at Type 0.
    context.birth_metavar(MetavarId(0), Vec::new(), family.clone());
    assert_eq!(conv(&mut context, &Term::hole(0), &identity(0)), Ok(true));
    let solution = context
        .metavar_solution(MetavarId(0))
        .cloned()
        .expect("the candidate checks");
    assert_eq!(annotation(&solution), sort(1));

    // ?1 : {f: (Type 1) -> Type 1}, against a tuple holding that identity.
    context.birth_metavar(
        MetavarId(1),
        Vec::new(),
        Term::tuple_type([(field, family)]),
    );
    assert_eq!(
        conv(&mut context, &Term::hole(1), &Term::tuple([identity(0)])),
        Ok(true)
    );
    let solution = context
        .metavar_solution(MetavarId(1))
        .cloned()
        .expect("the candidate checks");
    let Subterm::Tuple(tuple) = &*solution else {
        panic!("not a tuple: {solution}");
    };
    assert_eq!(annotation(&tuple.fields[0]), sort(1));
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

/// A candidate re-validation cannot judge yet is postponed, not refused. `?1 : ?0` against a lambda has no function type to be checked at while `?0` is unsolved — the check would park, and inside the oracle may not — and the candidate is committed once `?0` is solved. The same holds one level down: the lambda inside a pair at `{?0, Nat}`, where the frozen type is a record and only its field waits. The control is a candidate at a type that contradicts it, which nothing leaves undecided and which is refused outright.
///
/// Mutation-checked: with a declined park not recorded the two waiting goals are mismatches, and with every failed check read as undecided the control is blocked.
#[test]
fn a_candidate_waits_for_the_type_it_is_checked_at() {
    let mut context = context();
    let (x, a, b) = (
        context.fresh(Some("x")),
        context.fresh(Some("a")),
        context.fresh(Some("b")),
    );
    let lambda = Term::func([(x, nat_type())], Term::free_var(&x));
    let pair = Term::tuple([lambda.clone(), nat(1)]);
    let pair_type = Term::tuple_type([(a, Term::hole(0)), (b, nat_type())]);

    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    context.birth_metavar(MetavarId(1), Vec::new(), Term::hole(0));
    context.birth_metavar(MetavarId(2), Vec::new(), pair_type.clone());
    context.birth_metavar(MetavarId(3), Vec::new(), nat_type());

    let goals = |context: &mut Context| {
        [
            (Term::hole(0), Term::hole(1), lambda.clone()),
            (pair_type.clone(), Term::hole(2), pair.clone()),
            (nat_type(), Term::hole(3), lambda.clone()),
        ]
        .map(|(type_, slot, candidate)| {
            match convert_outcome(context, &type_, &slot, &candidate) {
                Ok(Outcome::Converts) => "converts",
                Ok(Outcome::Blocked(_)) => "blocked",
                Ok(Outcome::Mismatch(_)) => "mismatch",
                Err(_) => "exhausted",
            }
        })
    };

    assert_eq!(goals(&mut context), ["blocked", "blocked", "mismatch"]);
    assert_eq!(context.metavar_solution(MetavarId(1)), None);
    assert_eq!(context.metavar_solution(MetavarId(2)), None);

    context.solve_metavar(MetavarId(0), Term::func_type([(x, nat_type())], nat_type()));

    assert_eq!(goals(&mut context), ["converts", "converts", "mismatch"]);
    assert_eq!(context.metavar_solution(MetavarId(1)), Some(&lambda));
    assert_eq!(context.metavar_solution(MetavarId(2)), Some(&pair));
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
    let member = context.fresh(Some("member"));
    let (id, slot) = context.fresh_rec_slot(&member, Term::type_ground());
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

    let (context, converted) = embedding(MetavarOrigin::Domain("x".into()));
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
            .is_some_and(|entry| entry.refinements.is_empty() && entry.solution.is_none()),
        "the restriction is born under the refinements the two share — none — and left open"
    );

    let (context, converted) = embedding(MetavarOrigin::Goal);
    assert_eq!(converted, Ok(false));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

/// A metavariable born under binders the candidate's metavariable lacks is re-expressed over the candidate's own: `?0 ≟ (x : ?1(z, w)) -> Nat` solves, `?1` is solved to a stand-in over `?0`'s binder `z` applied to `y` — `z` spelled in `?1`'s birth names — and `w`, which no entry of `?0`'s spine reaches, is pruned, since no solution of `?1` using it could be one `?0` takes. Mutation-checked: refusing every binder `?0` lacks leaves `?0` unsolved.
#[test]
fn a_stand_in_keeps_the_binders_the_candidate_reaches_and_prunes_the_rest() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));
    let w = context.fresh(Some("w"));
    let z = context.fresh(Some("z"));
    context.birth_metavar(MetavarId(0), vec![(z, nat_type())], Term::type_ground());
    context.birth_metavar(
        MetavarId(1),
        vec![(y, nat_type()), (w, nat_type())],
        Term::type_ground(),
    );

    let outer = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&z)]);
    let embedded = Term::metavar_birthed(
        1,
        MetavarOrigin::Domain("x".into()),
        vec![Term::free_var(&z), Term::free_var(&w)],
    );
    let candidate = Term::func_type([(x, embedded)], nat_type());
    assert_eq!(conv(&mut context, &outer, &candidate), Ok(true));
    assert!(context.metavar_solution(MetavarId(0)).is_some());

    let Some(Subterm::Metavar(stand_in)) = context
        .metavar_solution(MetavarId(1))
        .map(|solution| &**solution)
    else {
        panic!("the embedded metavariable is solved to its stand-in");
    };
    assert_eq!(*stand_in.spine, vec![Term::free_var(&y)]);
    let entry = context
        .metavar_entry(stand_in.id)
        .expect("the stand-in has a birth record");
    assert_eq!(
        entry
            .telescope
            .iter()
            .map(|(name, _)| name)
            .collect::<Vec<_>>(),
        vec![&z],
        "the stand-in is born over the binder of `?0`'s the candidate reaches"
    );
    assert!(entry.solution.is_none());
}

/// An argument the candidate's metavariable reaches only inside a non-pattern entry of its spine is spelled through that entry, as the inversion abstracts it: a match's type `?0(r, (v,))`, met by an arm's type embedding `?1(r, v)`, solves `?1` to a stand-in over the match's binders applied to `(r, (v,))`, and `?0` to the arm's type over the stand-in at its own binders. Pruning `v` instead would lose every solution of `?1` through `(v,)`, which is how a match's inferred type depends on its scrutinee. The control passes `?1` the entry itself, which is no renaming its birth names could spell the spine through, and waits. Mutation-checked: carrying only the entries that are variables fails the first half.
#[test]
fn an_argument_reached_through_a_non_pattern_entry_is_spelled_through_it() {
    let embedding = |renaming: bool| {
        let mut context = context();
        let x = context.fresh(Some("x"));
        let r = context.fresh(Some("r"));
        let s = context.fresh(Some("s"));
        let v = context.fresh(Some("v"));
        let field = context.fresh(None);
        let pair = Term::tuple([Term::free_var(&v)]);
        context.birth_metavar(
            MetavarId(0),
            vec![
                (r, nat_type()),
                (s, Term::tuple_type([(field, nat_type())])),
            ],
            Term::type_ground(),
        );
        context.birth_metavar(
            MetavarId(1),
            vec![(r, nat_type()), (v, nat_type())],
            Term::type_ground(),
        );

        let outer = Term::metavar_birthed(
            0,
            MetavarOrigin::Hole,
            vec![Term::free_var(&r), pair.clone()],
        );
        let argument = match renaming {
            true => Term::free_var(&v),
            false => pair.clone(),
        };
        let embedded = Term::metavar_birthed(
            1,
            MetavarOrigin::Domain("x".into()),
            vec![Term::free_var(&r), argument],
        );
        let candidate = Term::func_type([(x, embedded)], nat_type());
        let converted = conv(&mut context, &outer, &candidate);
        (context, converted, [x, r, s], pair)
    };

    let (context, converted, [x, r, s], pair) = embedding(true);
    assert_eq!(converted, Ok(true));
    let Some(Subterm::Metavar(stand_in)) = context
        .metavar_solution(MetavarId(1))
        .map(|solution| &**solution)
    else {
        panic!("the embedded metavariable is solved to its stand-in");
    };
    assert_eq!(*stand_in.spine, vec![Term::free_var(&r), pair]);
    assert_eq!(
        context.metavar_entry(stand_in.id).map(|entry| entry
            .telescope
            .iter()
            .map(|(name, _)| *name)
            .collect::<Vec<_>>()),
        Some(vec![r, s]),
    );
    let arm = Term::func_type(
        [(
            x,
            Term::metavar_birthed(
                stand_in.id,
                MetavarOrigin::Domain("x".into()),
                vec![Term::free_var(&r), Term::free_var(&s)],
            ),
        )],
        nat_type(),
    );
    assert_eq!(
        context.metavar_solution(MetavarId(0)),
        Some(&arm),
        "the match's type is the arm's, over the stand-in at the match's own binders"
    );

    let (context, converted, _, _) = embedding(false);
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
    context.refine(&t_binder, &Term::intrinsic(Intrinsic::NatType));

    // `?0 ≟ 5` at type `t`. In the arm `t ⇝ Nat` and `5 : t` holds, but re-validation stands under the refinements `?0` was born under — none — leaving `t` abstract, so `5 : t` fails and the solution is rejected: `?0` also stands outside the arm, where `5 : t` is false.
    let t = Term::free_var(&t_binder);
    let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![t.clone()]);
    let five = Term::intrinsic(Intrinsic::Nat(Nat::new(5usize)));
    assert_eq!(convert(&mut context, &t, &occurrence, &five), Ok(false));
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
    context.refine(&t, &Term::intrinsic(Intrinsic::NatType));

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
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());
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
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&a)]);

    // The identity spine behaves exactly like the empty (bare-hole) spine: the candidate is stored unchanged.
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
        vec![(a, nat_type()), (b, nat_type())],
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
        vec![(a, nat_type()), (b, nat_type())],
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
        vec![(a, nat_type()), (b, nat_type())],
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
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&y)]);

    // ?0[y] ≟ z — `z` corresponds to no birth binder and never can: a hard mismatch, not a postponement.
    let outcome = convert_outcome(
        &mut context,
        &Term::type_ground(),
        &occurrence,
        &Term::free_var(&z),
    );
    assert!(matches!(outcome, Ok(Outcome::Mismatch(_))));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn classifies_a_solved_metavariable_spine_entry_by_its_value() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let y = context.fresh(Some("y"));
    let b = context.fresh(Some("b"));
    // ?0 is already solved to its own binder, so an occurrence ?0[y] stands for `y` — a perfectly good pattern variable hiding behind a node.
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());
    context.solve_metavar(MetavarId(0), Term::free_var(&a));
    let entry = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&y)]);

    context.birth_metavar(MetavarId(1), vec![(b, nat_type())], nat_type());
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
        vec![(a, nat_type()), (b, nat_type())],
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
                binders: Binders::default(),
                pairs: Pairs::default(),
                relation: Relation::Equal,
            }),
            Term::free_var(&b),
        );
    });

    // The frame is gone; the drain retries under the frozen one, where the refinement still holds and the goal converts.
    assert!(context.drain_parked().is_ok());
}

// A parked conversion watches the type it is compared at as it watches its two sides: postponed because that type is unsolved, it is woken where the type's metavariable is solved and does not wait for the drain's last sweep.
#[test]
fn a_parked_goal_is_woken_by_its_types_solution() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    context.park(
        ParkedWork::Conversion(Problem {
            type_: Term::hole(0),
            this: nat(1),
            that: nat(1),
            binders: Binders::default(),
            pairs: Pairs::default(),
            relation: Relation::Equal,
        }),
        nat(1),
    );
    assert!(context.wake_parked().is_empty());

    context.solve_metavar(MetavarId(0), nat_type());

    assert_eq!(context.wake_parked().len(), 1);
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
                binders: Binders::default(),
                pairs: Pairs::default(),
                relation: Relation::Equal,
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
        vec![(a, nat_type()), (b, nat_type())],
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
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());

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
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());

    // Disagreeing spines are not *unequal* — the solution may ignore the slot — so the pair parks rather than mismatching.
    let this = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![nat(1)]);
    let that = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![nat(2)]);

    let outcome = convert_outcome(&mut context, &Term::type_ground(), &this, &that);
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
}

#[test]
fn flex_flex_distinct_heads_with_a_common_solution_stays_blocked() {
    // Pinned: two *distinct* unsolved metavariables over compatible telescopes, met through the same live name. Flex–flex assignment (`?0 := ?1` through the renaming) would discharge this; flex–flex does no intersection, so the pair parks and — with nothing else to pin either head — stays undecided.
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let x = context.fresh(Some("x"));
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());
    context.birth_metavar(MetavarId(1), vec![(b, nat_type())], nat_type());

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
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());
    context.birth_metavar(MetavarId(1), vec![(a, nat_type())], nat_type());

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
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());
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
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());
    let m = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&a)]);

    // An inductive type against `Nat` is provably unequal whatever `?0` becomes — the heads are rigid — so the mismatch stays hard (and is reported at the use site, not deferred to the drain).
    let induct_decl = Term::induct_type(nominal("Vec"), [m], Vec::<Term>::new());
    let outcome = convert_outcome(
        &mut context,
        &Term::type_ground(),
        &induct_decl,
        &nat_type(),
    );
    assert!(matches!(outcome, Ok(Outcome::Mismatch(_))));
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

/// A goal conversion surrenders is retried under the binders it was posed under. Two eliminations of one scrutinee hand on, in their arm, a proof the arm binds: one through an elimination stuck on an unsolved metavariable, the other the arm's second proof. The pair cannot be decided while the metavariable is unsolved, so it is surrendered as blocked; once it is solved the first side reduces to the arm's first proof, and the two are one by the proposition the arm's binders were opened at. Retried under no binder they are two labels nothing types, and stay apart — which is what the verdict would be, following whether the goal parked, were the binders kept anywhere but with the goal.
///
/// Mutation-checked: with a goal parked under no binder the retry is a mismatch.
#[test]
fn a_goal_surrendered_under_a_binder_is_retried_under_it() {
    let mut context = context();
    let family = |context: &mut Context, path: &str, constructors, sort| {
        context
            .register_induct(
                &nominal(path),
                InductDecl {
                    universe_context: UniverseContext::empty(),
                    arity: Telescope::done(Telescope::done(())),
                    constructors,
                    result_sort: sort,
                    module: Qualifier::empty(),
                    rep_public: true,
                    polarities: Vec::new(),
                    variances: Vec::new(),
                },
            )
            .unwrap();

        Term::induct_type(nominal(path), Vec::<Term>::new(), Vec::<Term>::new())
    };
    // `induct P : Prop` and `induct Pair : Type | two(a : P, b : P)`.
    let proposition = family(&mut context, "P", Vec::new(), Term::prop());
    let (a, b) = (context.fresh(Some("a")), context.fresh(Some("b")));
    let pair = family(
        &mut context,
        "Pair",
        Vec::from([(
            Atom::from("two"),
            InductParam::new(
                Telescope::build(
                    [(a, proposition.clone()), (b, proposition.clone())],
                    Vec::new(),
                ),
                vec![Plicity::Explicit, Plicity::Explicit],
            ),
        )]),
        Term::type_ground(),
    );
    let scrutinee = context.fresh(Some("t"));
    context.assume(&scrutinee, &pair);
    context.birth_metavar(MetavarId(0), Vec::new(), nat_type());

    // `match t | two(a, b) => (<handed on>, 1) end`, at the ambient `{P, Nat}`.
    let (x, y) = (context.fresh(Some("x")), context.fresh(Some("y")));
    let result = Term::tuple_type([(x, proposition.clone()), (y, nat_type())]);
    let handing_on = |context: &mut Context, through_the_hole: bool| {
        let (a, b, m) = (
            context.fresh(Some("a")),
            context.fresh(Some("b")),
            context.fresh(Some("m")),
        );
        // `match ?0 | 0 => a | pred + 1; ih => a end`, stuck until the metavariable is solved.
        let (inner, pred, ih) = (
            context.fresh(Some("m")),
            context.fresh(Some("pred")),
            context.fresh(Some("ih")),
        );
        let handed = match through_the_hole {
            true => Term::nat_match(
                Term::hole(0),
                Some(&inner),
                proposition.clone(),
                Term::free_var(&a),
                &pred,
                &ih,
                Term::free_var(&a),
            ),
            false => Term::free_var(&b),
        };

        Term::induct_match(
            Term::free_var(&scrutinee),
            Some(&m),
            result.clone(),
            [("two", vec![a, b], Term::tuple([handed, nat(1)]))],
        )
    };
    let (this, that) = (
        handing_on(&mut context, true),
        handing_on(&mut context, false),
    );

    let Ok(Outcome::Blocked(goals)) =
        convert_outcome(&mut context, &Term::type_ground(), &this, &that)
    else {
        panic!("a pair stuck on an unsolved metavariable was decided");
    };
    let [goal] = &goals[..] else {
        panic!(
            "one goal is stuck on the metavariable, and {} were surrendered",
            goals.len()
        );
    };
    context.solve_metavar(MetavarId(0), nat(0));

    assert!(matches!(
        convert_under(
            &mut context,
            &goal.binders,
            &goal.pairs,
            &goal.type_,
            &goal.this,
            &goal.that
        ),
        Ok(Outcome::Converts)
    ));
    assert!(matches!(
        convert_outcome(&mut context, &goal.type_, &goal.this, &goal.that),
        Ok(Outcome::Mismatch(_))
    ));
}

// A goal parked at below resumes at below. `Type 0 ≤ ?0` waits on `?0`; solved to `Type 1`, the retry finds `Type 0` below it, where an equation would compare the two levels and refuse.
#[test]
fn a_goal_parked_at_below_resumes_at_below() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));
    context.park(
        ParkedWork::Conversion(Problem {
            type_: Term::type_ground(),
            this: Term::type_ground(),
            that: Term::hole(0),
            binders: Binders::default(),
            pairs: Pairs::default(),
            relation: Relation::Below,
        }),
        Term::hole(0),
    );
    context.solve_metavar(MetavarId(0), Term::type_at(Level::constant(1)));

    assert!(context.drain_parked().is_ok());
}

// Control: the same goal parked as an equation is refused once `?0` is `Type 1`.
#[test]
fn a_goal_parked_at_equal_resumes_at_equal() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));
    context.park(
        ParkedWork::Conversion(Problem {
            type_: Term::type_ground(),
            this: Term::type_ground(),
            that: Term::hole(0),
            binders: Binders::default(),
            pairs: Pairs::default(),
            relation: Relation::Equal,
        }),
        Term::hole(0),
    );
    context.solve_metavar(MetavarId(0), Term::type_at(Level::constant(1)));

    assert!(context.drain_parked().is_err());
}

// === A metavariable met at below ============================================

// `Type 0 ≤ ?0` solves `?0` to a `Type` at a level of its own over zero, so `Type 1 ≤ ?0` still holds and the levels close. Solved by the equation, `?0` is `Type 0` and the second bound is refused.
#[test]
fn a_metavariable_met_at_a_type_from_below_takes_a_level_of_its_own() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));

    let small = Term::type_ground();
    let large = Term::type_at(Level::constant(1));
    assert!(matches!(
        subsume_outcome(&mut context, &small, &Term::hole(0), None),
        Ok(Outcome::Converts)
    ));
    assert!(matches!(
        subsume_outcome(&mut context, &large, &Term::hole(0), None),
        Ok(Outcome::Converts)
    ));
    assert!(context.universes_mut().finalize([], [], []).is_ok());
}

// Control: the same two bounds with the first an equation.
#[test]
fn a_metavariable_met_at_a_type_as_an_equation_is_that_type() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));

    let small = Term::type_ground();
    let large = Term::type_at(Level::constant(1));
    assert_eq!(conv(&mut context, &small, &Term::hole(0)), Ok(true));
    let second = subsume_outcome(&mut context, &large, &Term::hole(0), None);
    let closed = context.universes_mut().finalize([], [], []);
    assert!(
        !matches!(second, Ok(Outcome::Converts)) || closed.is_err(),
        "{second:?} {closed:?}"
    );
}

// The metavariable below: `?0 ≤ Type 1` solves `?0` to a `Type` at a level of its own under one, so `?0 ≤ Type 0` still holds.
#[test]
fn a_metavariable_met_at_a_type_from_above_takes_a_level_of_its_own() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));

    let small = Term::type_ground();
    let large = Term::type_at(Level::constant(1));
    assert!(matches!(
        subsume_outcome(&mut context, &Term::hole(0), &large, None),
        Ok(Outcome::Converts)
    ));
    assert!(matches!(
        subsume_outcome(&mut context, &Term::hole(0), &small, None),
        Ok(Outcome::Converts)
    ));
    assert!(context.universes_mut().finalize([], [], []).is_ok());
}

// A function type's codomain is the position: `(x: Nat) -> Type 0 ≤ ?0` then `(x: Nat) -> Type 1 ≤ ?0`.
#[test]
fn a_function_type_met_from_below_takes_its_codomain_at_a_level_of_its_own() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));
    let family = |context: &mut Context, level: u32| {
        Term::func_type(
            [(context.fresh(Some("x")), nat_type())],
            Term::type_at(Level::constant(level)),
        )
    };

    let small = family(&mut context, 0);
    let large = family(&mut context, 1);
    assert!(matches!(
        subsume_outcome(&mut context, &small, &Term::hole(0), None),
        Ok(Outcome::Converts)
    ));
    assert!(matches!(
        subsume_outcome(&mut context, &large, &Term::hole(0), None),
        Ok(Outcome::Converts)
    ));
    assert!(context.universes_mut().finalize([], [], []).is_ok());
}

// A metavariable met at `Prop` from below is a `Prop`, at a level of its own over the one it met, so a larger proposition met after it is below it too and the levels close.
#[test]
fn a_metavariable_met_at_a_proposition_from_below_is_a_proposition_at_a_level_of_its_own() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));

    let small = Term::prop();
    let large = Term::prop_at(Level::constant(1));
    assert!(matches!(
        subsume_outcome(&mut context, &small, &Term::hole(0), None),
        Ok(Outcome::Converts)
    ));
    assert!(matches!(
        context
            .metavar_solution(MetavarId(0))
            .map(|solution| &**solution),
        Some(Subterm::Sort(Quality::Prop, _))
    ));
    assert!(matches!(
        subsume_outcome(&mut context, &large, &Term::hole(0), None),
        Ok(Outcome::Converts)
    ));
    assert!(context.universes_mut().finalize([], [], []).is_ok());
}

// No proposition stands where a type is wanted, so a `Type` met after the proposition is refused, and so is the proposition met after a `Type`: the metavariable is a sort of the kind it met first.
#[test]
fn a_type_and_a_proposition_met_from_below_are_refused_in_either_order() {
    let met = |first: Term, second: Term| {
        let mut context = context();
        context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));
        assert!(matches!(
            subsume_outcome(&mut context, &first, &Term::hole(0), None),
            Ok(Outcome::Converts)
        ));

        subsume_outcome(&mut context, &second, &Term::hole(0), None)
    };

    assert!(matches!(
        met(Term::prop(), Term::type_ground()),
        Ok(Outcome::Mismatch(_))
    ));
    assert!(matches!(
        met(Term::type_ground(), Term::prop()),
        Ok(Outcome::Mismatch(_))
    ));
}

// A `use` slot met against a dictionary's name is solved to the name: a witness names a structure of methods, and a slot committed reduced would spell the structure out wherever it stands. A hole of any other origin is solved to what the name reduces to, as it is for any value.
#[test]
fn a_use_slot_is_solved_to_the_dictionary_as_it_is_named() {
    let dictionary = Free::from(&nominal("dictionary"));
    let methods = || Term::tuple([nat(1), nat(2)]);
    let solved = |origin: MetavarOrigin| {
        let mut context = context();
        let type_ = Term::tuple_type([
            (context.fresh(Some("first")), nat_type()),
            (context.fresh(Some("second")), nat_type()),
        ]);
        context.define_assuming(
            &dictionary,
            &type_,
            &methods(),
            Some(&DefinitionKind::Authored),
        );
        context.birth_metavar(MetavarId(0), Vec::new(), type_.clone());
        let slot = Term::metavar_birthed(0, origin, Vec::new());
        assert_eq!(
            convert(&mut context, &type_, &slot, &Term::free_var(&dictionary)),
            Ok(true)
        );

        context.metavar_solution(MetavarId(0)).cloned()
    };
    let premise = MetavarOrigin::Witness(WitnessOrigin {
        func: CalleeId::Function(Free::from(&nominal("holds"))),
        binder: "key".into(),
    });

    assert_eq!(solved(premise), Some(Term::free_var(&dictionary)));
    assert_eq!(solved(MetavarOrigin::Hole), Some(methods()));
}

// A metavariable met against another's solution is solved to what that one spells: `?0` holds the name `flat`, and `?1`, met against `?0`, holds the name too, where the reduct of either is the definition's lambda.
#[test]
fn a_solution_met_through_a_solved_metavariable_keeps_the_name_it_spells() {
    let mut context = context();
    let flat = Free::from(&nominal("flat"));
    let argument = context.fresh(Some("n"));
    let type_ = Term::func_type([(argument, nat_type())], nat_type());
    context.define_assuming(
        &flat,
        &type_,
        &Term::func([(argument, nat_type())], nat(0)),
        Some(&DefinitionKind::Authored),
    );
    context.birth_metavar(MetavarId(0), Vec::new(), type_.clone());
    context.birth_metavar(MetavarId(1), Vec::new(), type_.clone());

    let name = Term::free_var(&flat);
    assert_eq!(
        convert(&mut context, &type_, &Term::hole(0), &name),
        Ok(true)
    );
    assert_eq!(
        convert(&mut context, &type_, &Term::hole(1), &Term::hole(0)),
        Ok(true)
    );
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&name));
    assert_eq!(context.metavar_solution(MetavarId(1)), Some(&name));
}

/// `flat(?0)` against `flat(1)`, where `flat` forgets its argument: the two are equal whatever `?0` is, and their spines agree at `?0 := 1`.
fn two_calls_of_a_definition_that_forgets_its_argument(context: &mut Context) -> Problem {
    let flat = Free::from(&nominal("flat"));
    let argument = context.fresh(Some("n"));
    context.define_assuming(
        &flat,
        &Term::func_type([(argument, nat_type())], nat_type()),
        &Term::func([(argument, nat_type())], nat(0)),
        Some(&DefinitionKind::Authored),
    );
    context.birth_metavar(MetavarId(0), Vec::new(), nat_type());

    Problem {
        type_: nat_type(),
        this: Term::apply(Term::free_var(&flat), [Term::hole(0)]),
        that: Term::apply(Term::free_var(&flat), [nat(1)]),
        binders: Binders::default(),
        pairs: Pairs::default(),
        relation: Relation::Equal,
    }
}

// Two calls of one definition agree where their arguments do, and agreement that takes a solution is one answer among those a definition that is not injective has: a term's to take where it completes, and none of conversion's, which decides this pair by reduction and leaves the metavariable open. Asked what the spines would solve, the pair offers `?0 := 1` and commits nothing; held, it commits it.
#[test]
fn two_spines_offer_what_they_would_solve_and_hold_it_only_when_asked() {
    let mut context = context();
    let problem = two_calls_of_a_definition_that_forgets_its_argument(&mut context);

    assert_eq!(
        convert(&mut context, &problem.type_, &problem.this, &problem.that),
        Ok(true)
    );
    assert_eq!(context.metavar_solution(MetavarId(0)), None);

    assert_eq!(
        hold_spines(&mut context, &problem, Spines::Offered),
        Ok(Some(vec![(MetavarId(0), nat(1))]))
    );
    assert_eq!(context.metavar_solution(MetavarId(0)), None);

    assert_eq!(
        hold_spines(&mut context, &problem, Spines::Held),
        Ok(Some(vec![(MetavarId(0), nat(1))]))
    );
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&nat(1)));
}
