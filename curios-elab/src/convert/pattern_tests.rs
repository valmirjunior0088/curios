//! Pattern inversion and pruning: a renaming inverted, a duplicated one postponed, and an entry outside the pattern pruned or spelled through.

use {
    super::test_support::*,
    crate::*,
    curios_core::{Intrinsic, MetavarId, MetavarOrigin, MetavarSpine, Subterm, Term},
};

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
        MetavarOrigin::Domain(Some("x".into())),
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
    assert_eq!(stand_in.spine, MetavarSpine::from(vec![Term::free_var(&y)]));
    let entry = context
        .metavar_entry(stand_in.id)
        .expect("the stand-in has a birth record");
    assert_eq!(
        entry
            .telescope()
            .iter()
            .map(|(name, _)| name)
            .collect::<Vec<_>>(),
        vec![&z],
        "the stand-in is born over the binder of `?0`'s the candidate reaches"
    );
    assert!(entry.solution().is_none());
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
            MetavarOrigin::Domain(Some("x".into())),
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
    assert_eq!(
        stand_in.spine,
        MetavarSpine::from(vec![Term::free_var(&r), pair])
    );
    assert_eq!(
        context.metavar_entry(stand_in.id).map(|entry| entry
            .telescope()
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
                MetavarOrigin::Domain(Some("x".into())),
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
