//! The refinements an arm installs: read at the universe instances they hold to one, and not where their frame is set aside.

use {
    super::test_support::{context, nat},
    crate::*,
    curios_core::{Intrinsic, Level, Term, UniverseMetaId, UniverseRole},
};

#[test]
fn proj_refinement_lookup() {
    let mut context = context();
    let r = context.fresh(Some("r"));

    context.refine_projection(Term::free_var(&r), 0, nat(1));

    let term: Term = Term::proj(Term::free_var(&r), 0);

    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(1)));
}

/// Two occurrences of one scrutinee mint an instance each, and the key meets them while both are open. The hit holds the two to one instance, since the arm's value is the scrutinee's as the arm recorded it: left apart, they close wherever their own bounds put them, and the kernel's key, which keeps its levels, answers no term at another.
#[test]
fn scrutinee_refinement_holds_fresh_universe_instances_to_one() {
    let mut context = context();
    let classify = context.fresh(Some("classify"));
    let (registered_at, probed_at) = fresh_instances(&mut context);
    let registered = Term::apply(
        Term::instance_of(&classify, vec![Level::meta(registered_at)]),
        [nat(0)],
    );
    let probe = Term::apply(
        Term::instance_of(&classify, vec![Level::meta(probed_at)]),
        [nat(0)],
    );
    let canonical = shallow_scrutinee(&context, &registered);
    context.refine_scrutinee_spellings(vec![(canonical, registered, false)], &nat(1));
    assert_ne!(
        instance(&context, registered_at),
        instance(&context, probed_at)
    );

    assert_eq!(reduce(&mut context, probe), Ok(nat(1)));
    assert_eq!(
        instance(&context, registered_at),
        instance(&context, probed_at)
    );
}

/// Two levels still open, as two occurrences of one polymorphic name mint them.
fn fresh_instances(context: &mut Context) -> (UniverseMetaId, UniverseMetaId) {
    let mut fresh = || {
        context
            .universes_mut()
            .fresh(UniverseRole::Generalizable, None)
    };

    (fresh(), fresh())
}

/// The level `meta` stands for now.
fn instance(context: &Context, meta: UniverseMetaId) -> Level {
    context
        .universes()
        .zonk(&Level::meta(meta))
        .expect("a level the test minted")
}

/// The pair the key must keep apart, and the one it does not: two occurrences of one definition at *ground* instances.
///
/// A fresh instance is undecided and is held to the recorded one — the test above is that rule, and it is what the prelude needs — but `Type u` embeds a level in a *term*, so a definition carrying its parameter into a constructor payload reduces to genuinely different values at two ground instances. Erasing the instance identifies them, and the arm's equation then refines a stuck term it was never shown to be about. The kernel refuses the coercion this forges (`curios-cert`'s `recheck::universes_tests::a_case_equation_does_not_refine_an_occurrence_at_another_universe_instance`, over the whole module); this is the elaborator's own store, asked directly.
///
/// The probe is checker-level rather than a source program on purpose: the surface has no spelling for an instance, so a program arranges two ground instances only through what bounds them, where this states them.
///
/// Mutation-checked: removing the guard fails these two and leaves both `holds_fresh_universe_instances_to_one` tests green, so the forgery pair and the collapse pair are not testing one thing twice — which is the whole of what the guard is claimed to do.
#[test]
fn scrutinee_refinement_does_not_fire_at_another_ground_universe_instance() {
    let mut context = context();
    let classify = context.fresh(Some("classify"));
    let at = |level: Level| Term::apply(Term::instance_of(&classify, vec![level]), [nat(0)]);
    let registered = at(Level::zero());
    let probe = at(Level::zero().succ().expect("level zero has a successor"));
    let canonical = shallow_scrutinee(&context, &registered);
    context.refine_scrutinee_spellings(vec![(canonical, registered, false)], &nat(1));

    assert_eq!(reduce(&mut context, probe.clone()), Ok(probe));
}

/// [`scrutinee_refinement_does_not_fire_at_another_ground_universe_instance`] over the projection store, which keys the same way and keeps no unerased spelling of its own.
#[test]
fn projection_refinement_does_not_fire_at_another_ground_universe_instance() {
    let mut context = context();
    let record_binder = context.fresh(Some("record"));
    let at = |level: Level| Term::apply(Term::instance_of(&record_binder, vec![level]), [nat(0)]);
    let registered = at(Level::zero());
    let probe = Term::proj(
        at(Level::zero().succ().expect("level zero has a successor")),
        0,
    );
    context.refine_projection(registered, 0, nat(1));

    assert_eq!(reduce(&mut context, probe.clone()), Ok(probe));
}

/// [`scrutinee_refinement_holds_fresh_universe_instances_to_one`] over the projection store.
#[test]
fn projection_refinement_holds_fresh_universe_instances_to_one() {
    let mut context = context();
    let record_binder = context.fresh(Some("record"));
    let (registered_at, probed_at) = fresh_instances(&mut context);
    let registered = Term::apply(
        Term::instance_of(&record_binder, vec![Level::meta(registered_at)]),
        [nat(0)],
    );
    let probe = Term::apply(
        Term::instance_of(&record_binder, vec![Level::meta(probed_at)]),
        [nat(0)],
    );
    context.refine_projection(registered, 0, nat(1));

    assert_eq!(reduce(&mut context, Term::proj(probe, 0)), Ok(nat(1)));
    assert_eq!(
        instance(&context, registered_at),
        instance(&context, probed_at)
    );
}

#[test]
fn a_refinement_is_not_read_where_its_frame_is_set_aside() {
    let mut context = context();
    let b_binder = context.fresh(Some("b"));
    let b = Term::free_var(&b_binder);
    let truth = Term::intrinsic(Intrinsic::Bool(true));
    let outside = context.hold_frames();

    context.with_frame(|context| {
        context.refine(&b_binder, &truth);

        // With the refinement in view, `b` reduces to its counterfactual value.
        assert_eq!(reduce(context, b.clone()), Ok(truth));

        // Judged where a metavariable born outside the arm was born (as in re-validation), `b` is abstract again.
        let reduced = context.at_birth(&outside, |context| reduce(context, b.clone()));
        assert_eq!(reduced, Ok(b));
    });
}

/// An equation in force follows the solution an arm is checked under: under the guard `f(n) < 5`, an arm that refines `n` to `0` holds `f(0) < 5`, the recorded spelling stepping aside for that instance, and gives both back when it is left. The kernel's `whnf::equation_scope_tests` holds this proposition under this name, by substituting the solution through the arm; here the variable stays spelled, so a term of the arm that still names it — the guard as written, or `f(n + 0) < 5`, which is no key — is read as the arm spells it.
///
/// Mutation-checked four ways. With `Context::refine` restating nothing the instance stays stuck inside the arm. With the recorded equation left answering beside its instance, the recorded key still answers there. With a key spelled as it is written, the guard and its instance are two keys in the arm. With a stuck form put to the reduced spellings as it is written, `f(n + 0) < 5` is put to no equation, the instance naming no `n`. `context::retry_tests` holds an instance that names no local withheld.
#[test]
fn a_case_equation_follows_the_solution_an_arm_is_checked_under() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let f = context.fresh(Some("f"));
    let x = context.fresh(Some("x"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    context.assume(&n, &nat_type);
    context.assume(&f, &Term::func_type([(x, nat_type.clone())], nat_type));
    let guard = |argument: Term| {
        Term::intrinsic(Intrinsic::nat_lt(
            Term::apply(Term::free_var(&f), [argument]),
            nat(5),
        ))
    };
    let written = guard(Term::free_var(&n));
    let at_zero = guard(nat(0));
    let respelled = guard(Term::intrinsic(Intrinsic::nat_add(
        Term::free_var(&n),
        nat(0),
    )));
    let truth = Term::intrinsic(Intrinsic::Bool(true));
    let decided = |context: &mut Context, term: &Term| {
        reduce(context, term.clone()).map(|reduct| reduct.as_bool())
    };

    context.with_frame(|context| {
        let recorded = shallow_scrutinee(context, &written);
        context
            .refine_scrutinee_spellings(vec![(recorded.clone(), written.clone(), false)], &truth);
        assert_eq!(
            decided(context, &at_zero),
            Ok(None),
            "the instance is no term the guard was written as, or the arm below proves nothing"
        );

        context.with_frame(|context| {
            context.refine(&n, &nat(0));

            assert_eq!(
                decided(context, &at_zero),
                Ok(Some(true)),
                "the equation answers at the arm's solution"
            );
            assert_eq!(
                context.scrutinee_reduct(&recorded, &written),
                None,
                "and the recorded spelling steps aside for it"
            );
            assert_eq!(
                shallow_scrutinee(context, &written),
                shallow_scrutinee(context, &at_zero),
                "a key is spelled as the arm spells it, so the guard as written meets the instance for one lookup"
            );
            assert_eq!(
                decided(context, &written),
                Ok(Some(true)),
                "a term that still names the variable is read as the arm spells it"
            );
            assert_eq!(
                decided(context, &respelled),
                Ok(Some(true)),
                "and so is one that is no key, where it reaches the reduced spellings"
            );
        });

        assert_eq!(
            decided(context, &at_zero),
            Ok(None),
            "the restatement does not outlive the arm"
        );
        assert_eq!(
            decided(context, &written),
            Ok(Some(true)),
            "and the recorded equation answers again"
        );
    });
}

/// A solution may name a variable an arm inside solves in turn: under the guard `f(n) < 5`, the arm that solves `n` as `k + 1` holds the equation at `f(k + 1) < 5`, and the arm inside it that solves `k` as `0` holds it at that instance again. The kernel has substituted both solutions by then; here both variables stay spelled, so a term is spelled as refined until no refined variable is left in it.
///
/// Mutation-checked: with a term spelled as refined for one round only, the guard as written still names `k` in the inner arm and meets no equation.
#[test]
fn a_case_equation_follows_a_solution_naming_a_variable_solved_in_turn() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let k = context.fresh(Some("k"));
    let f = context.fresh(Some("f"));
    let x = context.fresh(Some("x"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    context.assume(&n, &nat_type);
    context.assume(&k, &nat_type);
    context.assume(&f, &Term::func_type([(x, nat_type.clone())], nat_type));
    let guard = |argument: Term| {
        Term::intrinsic(Intrinsic::nat_lt(
            Term::apply(Term::free_var(&f), [argument]),
            nat(5),
        ))
    };
    let written = guard(Term::free_var(&n));
    let successor = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&k), nat(1)));
    let truth = Term::intrinsic(Intrinsic::Bool(true));
    let decided = |context: &mut Context, term: &Term| {
        reduce(context, term.clone()).map(|reduct| reduct.as_bool())
    };

    context.with_frame(|context| {
        let recorded = shallow_scrutinee(context, &written);
        context.refine_scrutinee_spellings(vec![(recorded, written.clone(), false)], &truth);

        context.with_frame(|context| {
            context.refine(&n, &successor);
            assert_eq!(decided(context, &guard(successor.clone())), Ok(Some(true)));

            context.with_frame(|context| {
                context.refine(&k, &nat(0));
                assert_eq!(
                    decided(context, &guard(nat(1))),
                    Ok(Some(true)),
                    "the equation followed both solutions"
                );
                assert_eq!(
                    decided(context, &written),
                    Ok(Some(true)),
                    "and the guard as written is read under both"
                );
            });

            assert_eq!(decided(context, &written), Ok(Some(true)));
        });
    });
}
