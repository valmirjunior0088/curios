//! A nominal type's levels and a definition's paired by variance: what conversion requires of two instances, and what it records without requiring.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Definition, DefinitionKind, Free, InductDecl, InductParam, Level, Telescope, Term,
        Totality, UniverseConstraintKind, UniverseConstraintOrigin, UniverseContext,
        UniverseMetaId, UniverseParam, UniverseRole, Variance,
    },
    curios_utilities::{Plicity, Qualifier},
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
                    quality_count: 0,
                    constraints: Vec::new(),
                },
                arity: Telescope::build([(carrier, sort.clone())], Telescope::done(())),
                constructors: Vec::new(),
                result_sort: sort,
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: vec![variance],
            },
        )
        .unwrap();
}

/// `induct Held.{u}: Type u | hold(Drop.{u}(Nat))` over `let Drop.{u}(X: Type u) -> Type = Nat`: the payload holds `u` as it is written and no level once `Drop` is unfolded, which the walk does to read it.
fn declare_held(context: &mut Context) {
    let level = || Level::param(UniverseParam(0));
    let scheme = UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: Vec::new(),
    };

    let drop = Free::from(&nominal("Drop"));
    let dropped = context.fresh(Some("X"));
    context.define_assuming(
        &drop,
        &Term::func_type([(dropped, Term::type_at(level()))], Term::type_ground()),
        &Term::func([(dropped, Term::type_at(level()))], nat_type()),
        Some(&DefinitionKind::Authored),
    );
    context.set_assumption_universe_context(&drop, scheme.clone());

    let held = context.fresh(Some("held"));
    let payload = Term::apply(Term::instance_of(&drop, vec![level()]), [nat_type()]);
    context
        .register_induct(
            &nominal("Held"),
            InductDecl {
                universe_context: scheme,
                arity: Telescope::done(Telescope::done(())),
                constructors: vec![(
                    Atom::from("hold"),
                    InductParam::new(
                        Telescope::build([(held, payload)], Vec::new()),
                        vec![Plicity::Explicit],
                    ),
                )],
                result_sort: Term::type_at(level()),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .unwrap();
}

/// `Wrap.{level}(parameter)`, as the node reduction hands conversion.
fn wrap(level: Level, parameter: Term) -> Term {
    Term::induct_type_at(nominal("Wrap"), [level], [parameter], Vec::<Term>::new())
}

/// `let pick.{u}(A: Type u) -> Nat = 3`, published carrying `variance` for its one level, which its value does not hold.
fn define_pick(context: &mut Context, variance: Variance) {
    let carrier = context.fresh(Some("A"));
    let sort = Term::type_at(Level::param(UniverseParam(0)));
    let definition = Definition {
        name: nominal("pick"),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: Vec::new(),
        },
        island: Qualifier::empty(),
        totality: Totality::Total,
        proof: false,
        variances: vec![variance],
        type_: Term::func_type([(carrier, sort.clone())], nat_type()),
        body: Term::func([(carrier, sort)], nat(3)),
    };

    let name = Free::from(&definition.name);
    context.define_assuming(
        &name,
        &definition.type_,
        &definition.body,
        Some(&definition.kind),
    );
    context.set_assumption_universe_context(&name, definition.universe_context.clone());
    context.record_definition_variances(&definition);
}

/// `pick.{level}`, passed bare.
fn pick(level: Level) -> Term {
    Term::instance_of(&Free::from(&nominal("pick")), vec![level])
}

/// A level bounded away from zero, which an equation with zero refuses where the declaration closes.
fn bounded(context: &mut Context) -> UniverseMetaId {
    let bounded = open(context);
    context
        .universes_mut()
        .add_leq(
            Level::constant(1),
            Level::meta(bounded),
            UniverseConstraintOrigin::new(UniverseConstraintKind::Conversion),
        )
        .unwrap();

    bounded
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

// A definition applied by the one spine its type asks for is its value at those arguments, so two instances apart in a level the value does not hold are one term: two open levels are joined when the declaration closes and a level bounded away from the other side is left where its bound puts it. Carried invariant, the pair is the equation it was.
#[test]
fn a_definition_applied_in_full_is_joined_where_it_can_be_and_never_required() {
    for (variance, required) in [(Variance::Irrelevant, false), (Variance::Invariant, true)] {
        let mut context = context();
        define_pick(&mut context, variance);
        let this = open(&mut context);
        let that = open(&mut context);
        assert_eq!(
            conv(
                &mut context,
                &Term::apply(pick(Level::meta(this)), [nat_type()]),
                &Term::apply(pick(Level::meta(that)), [nat_type()])
            ),
            Ok(true)
        );
        let scheme = context
            .universes_mut()
            .finalize([this, that], [], [])
            .unwrap();
        assert_eq!(scheme.parameter_count, 1, "{variance:?}");

        let mut context = self::context();
        define_pick(&mut context, variance);
        let bounded = bounded(&mut context);
        assert_eq!(
            conv(
                &mut context,
                &Term::apply(pick(Level::meta(bounded)), [nat_type()]),
                &Term::apply(pick(Level::zero()), [nat_type()])
            ),
            Ok(true)
        );
        assert_eq!(
            context.universes_mut().finalize([bounded], [], []).is_err(),
            required,
            "{variance:?}"
        );
    }
}

// Unfolded, a definition passed bare is a lambda, and the level its instance held is the level its binder is annotated at. Two lambdas apart there are read as the two instances are, with nothing required, at their function type and where a position hands conversion none: a nominal type's parameter is posed at `Type`.
#[test]
fn two_lambdas_apart_in_a_binders_annotation_are_left_to_their_function_type() {
    let carrier = Free::local(7_100, Some("A"));
    let lambda = |level: Level| Term::func([(carrier, Term::type_at(level))], nat(3));
    let at = Term::func_type([(carrier, Term::type_ground())], nat_type());

    for type_ in [at, Term::type_ground()] {
        let mut context = context();
        let bounded = bounded(&mut context);
        assert_eq!(
            convert(
                &mut context,
                &type_,
                &lambda(Level::meta(bounded)),
                &lambda(Level::zero())
            ),
            Ok(true)
        );
        assert!(context.universes_mut().finalize([bounded], [], []).is_ok());

        let mut context = self::context();
        let this = open(&mut context);
        let that = open(&mut context);
        assert_eq!(
            convert(
                &mut context,
                &type_,
                &lambda(Level::meta(this)),
                &lambda(Level::meta(that))
            ),
            Ok(true)
        );
        let scheme = context
            .universes_mut()
            .finalize([this, that], [], [])
            .unwrap();
        assert_eq!(scheme.parameter_count, 1);
    }
}

// Passed bare the definition is a function, and two instances of it meet at a function type, where both sides are applied before either is read. So the pair is left to that type with nothing required, and recorded where an equation used to be: two open levels are joined when the declaration closes, and a level bounded away from the other side stays where its bound puts it. Carried invariant, the pair is the equation it was.
#[test]
fn a_definition_passed_bare_is_left_to_its_function_type() {
    for (variance, required) in [(Variance::Irrelevant, false), (Variance::Invariant, true)] {
        let carrier = Free::local(7_000, Some("A"));
        let at = Term::func_type([(carrier, Term::type_ground())], nat_type());

        let mut context = context();
        define_pick(&mut context, variance);
        let this = open(&mut context);
        let that = open(&mut context);
        assert_eq!(
            convert(
                &mut context,
                &at,
                &pick(Level::meta(this)),
                &pick(Level::meta(that))
            ),
            Ok(true)
        );
        let scheme = context
            .universes_mut()
            .finalize([this, that], [], [])
            .unwrap();
        assert_eq!(scheme.parameter_count, 1, "{variance:?}");

        let mut context = self::context();
        define_pick(&mut context, variance);
        let bounded = bounded(&mut context);
        assert_eq!(
            convert(
                &mut context,
                &at,
                &pick(Level::meta(bounded)),
                &pick(Level::zero())
            ),
            Ok(true)
        );
        assert_eq!(
            context.universes_mut().finalize([bounded], [], []).is_err(),
            required,
            "{variance:?}"
        );
    }
}

// A group whose walk the budget cuts short is refused for the budget and files no vector: a reduction the budget refuses reads its term as holding every level it carries, so the vector left claims less than the family's own, and on record it would refuse two instances a later item compares, with no word of the budget.
#[test]
fn a_family_whose_walk_the_budget_cuts_short_files_no_vector() {
    for budget in [0, 100_000] {
        let mut context = Context::new(budget, SYNTAX);
        declare_held(&mut context);

        let recorded = record_variances(&mut context, &[nominal("Held")]);
        let filed = context
            .induct_decl(&nominal("Held"))
            .map(|declaration| declaration.variances.clone());
        match budget {
            0 => {
                assert!(recorded.is_err(), "with no budget the group is refused");
                assert_eq!(filed, Some(Vec::new()), "and its entry keeps no vector");
            }
            _ => {
                recorded.expect("with the budget to unfold `Drop` the group is read whole");
                assert_eq!(filed, Some(vec![Variance::Irrelevant]));
            }
        }
    }
}
