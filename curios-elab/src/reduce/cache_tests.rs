//! What invalidates a cached reduct, and what one served across universe spellings states.

use {
    super::test_support::{context, nat, nominal},
    crate::*,
    curios_core::{
        Free, Intrinsic, Level, Many, MetavarId, Quality, QualityMetaId, QualityParam, RecGroup,
        RecMemberScopes, Scope, Term, UniverseContext, UniverseMetaId, UniverseParam, Universes,
    },
};

#[test]
fn define_invalidates_cached_reduction() {
    let mut context = context();
    let x_binder = context.fresh(Some("x"));
    let x: Term = Term::free_var(&x_binder);

    // No definition yet: x reduces to itself and the result is cached.
    assert_eq!(reduce(&mut context, x.clone()), Ok(x.clone()));

    // Defining x must clear the cache so the next reduce unfolds.
    context.define(&x_binder, &nat(3), None);
    assert_eq!(reduce(&mut context, x), Ok(nat(3)));
}

#[test]
fn refine_projection_invalidates_cached_reduction() {
    let mut context = context();
    let r = context.fresh(Some("r"));
    let proj: Term = Term::proj(Term::free_var(&r), 0);

    // No projection refinement yet: proj reduces to itself and is cached.
    assert_eq!(reduce(&mut context, proj.clone()), Ok(proj.clone()));

    // Refining the projection must clear the cache.
    context.refine_projection(Term::free_var(&r), 0, nat(1));
    assert_eq!(reduce(&mut context, proj), Ok(nat(1)));
}

#[test]
fn redefine_invalidates_reduction_cached_under_the_old_value() {
    let mut context = context();
    let x_binder = context.fresh(Some("x"));
    let x: Term = Term::free_var(&x_binder);

    // First definition: x reduces to 4 and the reduct — which no longer mentions `x` — is cached.
    context.define(&x_binder, &nat(4), None);
    assert_eq!(reduce(&mut context, x.clone()), Ok(nat(4)));

    // Rebinding the same label must evict that entry even though a selective retain keyed on mentions of `x` cannot see it.
    context.define(&x_binder, &nat(5), None);
    assert_eq!(reduce(&mut context, x), Ok(nat(5)));
}

#[test]
fn leave_frame_with_definitions_invalidates_cached_reduction() {
    let mut context = context();
    let x_binder = context.fresh(Some("x"));
    let x: Term = Term::free_var(&x_binder);

    // Inside a frame, define x and reduce — the cache will hold x → "inner".
    context.with_frame(|context| {
        context.define(&x_binder, &nat(4), None);
        assert_eq!(reduce(context, x.clone()), Ok(nat(4)));
    });

    // After the frame pops, x has no definition again. A stale cache entry would still return "inner"; the cache clear on leave_frame prevents that.
    assert_eq!(reduce(&mut context, x.clone()), Ok(x));
}

/// Writing the universe scheme a name already has rewrites nothing, so the reduction cache keeps its reducts: the second reduction of a closed term across such a write costs no steps. A scheme that does change still clears, since a reduct through the old scheme is unsound.
#[test]
fn an_unchanged_universe_scheme_keeps_the_reduction_cache_warm() {
    let mut context = context();
    let poly = context.fresh(Some("poly"));
    let scheme = UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: Vec::new(),
    };
    context.assume(
        &poly,
        &Term::type_at(Level::param(UniverseParam(0)).succ().unwrap()),
    );
    context.define(&poly, &Term::type_at(Level::param(UniverseParam(0))), None);
    context.set_assumption_universe_context(&poly, scheme.clone());

    let term = Term::intrinsic(Intrinsic::nat_add(
        nat(3),
        Term::intrinsic(Intrinsic::nat_add(nat(2), nat(2))),
    ));
    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(7)));
    let spent = context.consumed().units();

    context.set_assumption_universe_context(&poly, scheme.clone());
    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(7)));
    assert_eq!(
        context.consumed().units(),
        spent,
        "an unchanged scheme leaves the reduct cached, so the second reduction is a hit"
    );

    context.set_assumption_universe_context(
        &poly,
        UniverseContext {
            parameter_count: 2,
            quality_count: 0,
            constraints: Vec::new(),
        },
    );
    assert_eq!(reduce(&mut context, term), Ok(nat(7)));
    assert!(
        context.consumed().units() > spent,
        "a changed scheme clears the cache, so the third reduction is paid again"
    );
}

/// A closed reduct is served across universe spellings: a definition unfolded at a universe metavariable and then at a constant is one computation, the second answered from the first through the cache's erased second key with the reduct's level rewritten to the asking spelling. Checking writes a term with metavariables and totality reads it with them solved, so without the second key every fold over a literal would run once per phase and per mention.
#[test]
fn a_closed_reduct_is_served_across_universe_spellings() {
    let mut context = context();
    // A global rather than a minted local: the second door serves closed terms alone, since a local-bearing reduct dies with its declaration.
    let poly = Free::Global(nominal("poly"));
    let parameter = Level::param(UniverseParam(0));
    context.assume(&poly, &Term::type_at(parameter.succ().unwrap()));
    context.define(&poly, &Term::type_at(parameter), None);
    context.set_assumption_universe_context(
        &poly,
        UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: Vec::new(),
        },
    );

    let meta = Level::meta(UniverseMetaId(7));
    assert_eq!(
        reduce(&mut context, Term::instance_of(&poly, vec![meta.clone()])),
        Ok(Term::type_at(meta.clone()))
    );
    let spent = context.consumed().units();
    assert_eq!(
        reduce(
            &mut context,
            Term::instance_of(&poly, vec![Level::constant(3)])
        ),
        Ok(Term::type_at(Level::constant(3))),
        "the reduct is rewritten to the asking spelling's level"
    );
    assert_eq!(
        context.consumed().units(),
        spent,
        "and served without a step, since the two spellings share one erased key"
    );
}

/// A stored spelling that holds one level metavariable at two positions answers for no spelling asking it for itself at one and for another level at the other: the map the two determine sends one metavariable to two levels, and the reduct rewritten through either would state the wrong level at one of them.
#[test]
fn a_level_metavariable_asked_for_itself_and_for_another_level_is_reduced_afresh() {
    let mut context = context();
    let poly = Free::Global(nominal("poly"));
    let x = context.fresh(Some("x"));
    let (first, second) = (
        Level::param(UniverseParam(0)),
        Level::param(UniverseParam(1)),
    );
    context.assume(&poly, &Term::type_at(first.succ().unwrap()));
    context.define(
        &poly,
        &Term::func_type([(x, Term::type_at(first))], Term::type_at(second)),
        None,
    );
    context.set_assumption_universe_context(
        &poly,
        UniverseContext {
            parameter_count: 2,
            quality_count: 0,
            constraints: Vec::new(),
        },
    );
    let arrow = |domain: &Level, codomain: &Level| {
        Term::func_type(
            [(x, Term::type_at(domain.clone()))],
            Term::type_at(codomain.clone()),
        )
    };

    let (kept, other) = (
        Level::meta(UniverseMetaId(7)),
        Level::meta(UniverseMetaId(8)),
    );
    assert_eq!(
        reduce(
            &mut context,
            Term::instance_of(&poly, vec![kept.clone(), kept.clone()])
        ),
        Ok(arrow(&kept, &kept))
    );
    assert_eq!(
        reduce(
            &mut context,
            Term::instance_of(&poly, vec![kept.clone(), other.clone()])
        ),
        Ok(arrow(&kept, &other)),
        "the first position keeps the metavariable the second is asked to leave"
    );
}

/// A closed reduct served across universe spellings states the asking spelling's sorts: an instance's sorts are dropped from the erased key with its levels, so a definition unfolded at one sort metavariable answers for it at another sort, and the reduct is rewritten to that sort as it is to the level. Served as stored, a family's occurrence at one sort would be read at the sort of the occurrence before it.
#[test]
fn a_closed_reduct_served_across_spellings_states_the_asking_sorts() {
    let mut context = context();
    let poly = Free::Global(nominal("poly"));
    let level = Level::param(UniverseParam(0));
    let left = Quality::Param(QualityParam(0));
    context.assume(&poly, &Term::type_at(level.succ().unwrap()));
    context.define(&poly, &Term::sort_at(left, level), None);
    context.set_assumption_universe_context(
        &poly,
        UniverseContext {
            parameter_count: 1,
            quality_count: 1,
            constraints: Vec::new(),
        },
    );
    let at = |level: Level, quality: Quality| {
        Term::instance_of(
            &poly,
            Universes {
                levels: vec![level],
                qualities: vec![quality],
            },
        )
    };

    let meta = Level::meta(UniverseMetaId(7));
    let open = Quality::Meta(QualityMetaId(2));
    assert_eq!(
        reduce(&mut context, at(meta.clone(), open)),
        Ok(Term::sort_at(open, meta))
    );
    let spent = context.consumed().units();

    let other = Quality::Meta(QualityMetaId(3));
    assert_eq!(
        reduce(&mut context, at(Level::constant(3), other)),
        Ok(Term::sort_at(other, Level::constant(3))),
        "the reduct is rewritten to the asking spelling's sort"
    );
    assert_eq!(
        context.consumed().units(),
        spent,
        "and served without a step, since the two spellings share one erased key"
    );
}

/// A sort an instance states outright is no metavariable to map: a reduct stored for an occurrence at `Prop` is declined for one at `Type`, which is reduced as itself.
#[test]
fn a_reduct_stored_at_one_stated_sort_is_not_served_at_another() {
    let mut context = context();
    let poly = Free::Global(nominal("poly"));
    let level = Level::param(UniverseParam(0));
    context.assume(&poly, &Term::type_at(level.succ().unwrap()));
    context.define(
        &poly,
        &Term::sort_at(Quality::Param(QualityParam(0)), level),
        None,
    );
    context.set_assumption_universe_context(
        &poly,
        UniverseContext {
            parameter_count: 1,
            quality_count: 1,
            constraints: Vec::new(),
        },
    );
    let at = |quality: Quality| {
        Term::instance_of(
            &poly,
            Universes {
                levels: vec![Level::constant(3)],
                qualities: vec![quality],
            },
        )
    };

    assert_eq!(
        reduce(&mut context, at(Quality::Prop)),
        Ok(Term::sort_at(Quality::Prop, Level::constant(3)))
    );
    assert_eq!(
        reduce(&mut context, at(Quality::Type)),
        Ok(Term::sort_at(Quality::Type, Level::constant(3)))
    );
}

/// `λ(a, b). a`, the closed redex the door's fixtures reduce: its reduct is its first argument as it stands.
fn first_of(context: &mut Context<'static>) -> Term {
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let at_five = Term::type_at(Level::constant(5));
    Term::func([(a, at_five.clone()), (b, at_five)], Term::free_var(&a))
}

/// A ground sort is a position like any other. Once `first(Type 1, Type 0)` is stored, `first(Type 0, Type 1)` is no instance of it, nor `first(Type 0, Type ?8)` of a stored `first(Type ?7, Type 0)`, and each is reduced as itself: a walk over universe data alone passed over the ground `Type 0` on both sides, and served the first's reduct or carried `?7` to `?8` across positions that do not correspond.
#[test]
fn a_ground_sort_at_another_position_is_reduced_afresh() {
    let applied = |first: &Term, left: Level, right: Level| {
        Term::apply(first.clone(), [Term::type_at(left), Term::type_at(right)])
    };

    let mut context = context();
    let first = first_of(&mut context);
    let one = Level::constant(1);
    assert_eq!(
        reduce(&mut context, applied(&first, one.clone(), Level::zero())),
        Ok(Term::type_at(one.clone()))
    );
    assert_eq!(
        reduce(&mut context, applied(&first, Level::zero(), one)),
        Ok(Term::type_ground())
    );

    let mut context = super::test_support::context();
    let first = first_of(&mut context);
    let stored = Level::meta(UniverseMetaId(7));
    assert_eq!(
        reduce(&mut context, applied(&first, stored.clone(), Level::zero())),
        Ok(Term::type_at(stored))
    );
    assert_eq!(
        reduce(
            &mut context,
            applied(&first, Level::zero(), Level::meta(UniverseMetaId(8)))
        ),
        Ok(Term::type_ground())
    );
}

/// A recursive group that binds its own universe parameter and the same group binding none, whose `u` is the enclosing declaration's, erase alike and list the same levels; a walk over levels alone served the first's reduct for the second. Read over the pair they are apart in their universe context, and the second is reduced as itself.
#[test]
fn a_group_binding_its_own_levels_answers_no_group_binding_none() {
    let mut context = context();
    let first = first_of(&mut context);
    let member = |term: Term| Scope::constant(Many(1), term);
    let group = |parameter_count| {
        Term::rec_proj(
            RecGroup::new(vec![RecMemberScopes {
                type_: member(Term::type_at(Level::param(UniverseParam(0)))),
                body: member(Term::type_at(Level::param(UniverseParam(0)))),
            }])
            .with_universe_context(UniverseContext {
                parameter_count,
                quality_count: 0,
                constraints: Vec::new(),
            }),
            0,
        )
    };
    // Under a pair, so that the reduct holds the group as it stands.
    let applied = |parameter_count| {
        Term::apply(
            first.clone(),
            [
                Term::tuple([group(parameter_count), nat(0)]),
                Term::type_ground(),
            ],
        )
    };

    assert_eq!(
        reduce(&mut context, applied(1)),
        Ok(Term::tuple([group(1), nat(0)]))
    );
    assert_eq!(
        reduce(&mut context, applied(0)),
        Ok(Term::tuple([group(0), nat(0)]))
    );
}

/// A spelling holding a term metavariable is no instance its key's levels can be carried to: the solution holds a universe metavariable the key holds too, which carrying would move in the key and not in the solution. Once `second(poly.{?7}, ?0)` is stored, `?0` solved to `Type ?7`, `second(poly.{?8}, ?0)` is the solution as it stands, and so is the spelling with the solution written in, which the stored key carried to `?8` is not.
#[test]
fn a_universe_metavariable_a_solution_holds_is_not_carried() {
    let mut context = context();
    let poly = Free::Global(nominal("poly"));
    let stored = Level::meta(UniverseMetaId(7));
    context.birth_metavar(
        MetavarId(0),
        Vec::new(),
        Term::type_at(stored.succ().unwrap()),
    );
    context.solve_metavar(MetavarId(0), Term::type_at(stored.clone()));
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let at_five = Term::type_at(Level::constant(5));
    let second = Term::func([(a, at_five.clone()), (b, at_five)], Term::free_var(&b));
    let at = |level: Level| {
        Term::apply(
            second.clone(),
            [Term::instance_of(&poly, vec![level]), Term::hole(0)],
        )
    };

    assert_eq!(
        reduce(&mut context, at(stored.clone())),
        Ok(Term::type_at(stored.clone()))
    );
    assert_eq!(
        reduce(&mut context, at(Level::meta(UniverseMetaId(8)))),
        Ok(Term::type_at(stored))
    );
}
