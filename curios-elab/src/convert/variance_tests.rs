//! A nominal type's levels paired by variance: what conversion requires of two instances, and what it records without requiring.

use {
    super::test_support::*,
    crate::*,
    curios_core::{
        InductDecl, Level, Telescope, Term, UniverseConstraintKind, UniverseConstraintOrigin,
        UniverseContext, UniverseMetaId, UniverseParam, UniverseRole, Variance,
    },
    curios_utilities::Qualifier,
};

/// `induct Wrap.{u}(A: Type u): Type u` with no constructor, registered carrying `variance` for its one level.
fn declare_wrap(context: &mut Context, variance: Variance) {
    let carrier = context.fresh(Some("A"));
    let sort = Term::type_at(Level::param(UniverseParam(0)));

    context
        .register_induct(
            &nominal("Wrap"),
            InductDecl {
                universe_context: UniverseContext {
                    parameter_count: 1,
                    constraints: Vec::new(),
                },
                arity: Telescope::build([(carrier, sort.clone())], Telescope::done(())),
                constructors: Vec::new(),
                result_sort: sort,
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: vec![variance],
                plicities: Vec::new(),
            },
        )
        .unwrap();
}

/// `Wrap.{level}(parameter)`, as the node reduction hands conversion.
fn wrap(level: Level, parameter: Term) -> Term {
    Term::induct_type_at(nominal("Wrap"), [level], [parameter], Vec::<Term>::new())
}

/// `Nat`, as written or as a redex that reduces to it. Two nodes over one spelling differ in levels alone, which the levels-only shortcut decides; over two spellings they are not one term up to levels, and the node's own arm compares them.
fn parameter(context: &mut Context, redex: bool) -> Term {
    match redex {
        true => {
            let carrier = context.fresh(Some("X"));
            Term::apply(
                Term::func([(carrier, Term::type_ground())], Term::free_var(&carrier)),
                [nat_type()],
            )
        }
        false => nat_type(),
    }
}

/// A level a declaration leaves open, which its closing makes a parameter unless something joins it to another.
fn open(context: &mut Context) -> UniverseMetaId {
    context
        .universes_mut()
        .fresh(UniverseRole::Generalizable, None)
}

// Two instances apart in a level the family is irrelevant in are one type whatever the two levels are, and the pair is recorded where an equation used to be required: two open levels are joined when the declaration closes, as the equation joined them, and a level bounded away from the other side is left where its bound puts it, which the equation refused. Carried invariant, the pair is the equation. Both sites that meet such a pair record it: the levels-only shortcut, over two spellings of one term, and the node's arm, over two that differ in a parameter's spelling.
#[test]
fn an_irrelevant_level_is_joined_where_it_can_be_and_never_required() {
    for redex in [false, true] {
        for (variance, required) in [(Variance::Irrelevant, false), (Variance::Invariant, true)] {
            let mut context = context();
            declare_wrap(&mut context, variance);
            let this = open(&mut context);
            let that = open(&mut context);
            let other = parameter(&mut context, redex);
            assert_eq!(
                conv(
                    &mut context,
                    &wrap(Level::meta(this), nat_type()),
                    &wrap(Level::meta(that), other)
                ),
                Ok(true)
            );
            let scheme = context
                .universes_mut()
                .finalize([this, that], [], [])
                .unwrap();
            assert_eq!(scheme.parameter_count, 1, "{variance:?}, redex: {redex}");

            let mut context = self::context();
            declare_wrap(&mut context, variance);
            let bounded = open(&mut context);
            let other = parameter(&mut context, redex);
            context
                .universes_mut()
                .add_leq(
                    Level::constant(1),
                    Level::meta(bounded),
                    UniverseConstraintOrigin::new(UniverseConstraintKind::Conversion),
                )
                .unwrap();
            assert_eq!(
                conv(
                    &mut context,
                    &wrap(Level::meta(bounded), nat_type()),
                    &wrap(Level::zero(), other)
                ),
                Ok(true)
            );
            assert_eq!(
                context.universes_mut().finalize([bounded], [], []).is_err(),
                required,
                "{variance:?}, redex: {redex}"
            );
        }
    }
}

// Two decided instances hold nothing to join: one type where the level is irrelevant, two where it is invariant.
#[test]
fn two_decided_instances_apart_in_an_irrelevant_level_convert() {
    for (variance, converts) in [(Variance::Irrelevant, true), (Variance::Invariant, false)] {
        let mut context = context();
        declare_wrap(&mut context, variance);

        assert_eq!(
            conv(
                &mut context,
                &wrap(Level::zero(), nat_type()),
                &wrap(Level::constant(1), nat_type())
            ) == Ok(true),
            converts,
            "{variance:?}"
        );
    }
}
