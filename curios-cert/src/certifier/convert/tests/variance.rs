//! A nominal node's levels compared by its family's variance: what the arms compare, what they still compare whatever the levels, and what reaches them.

use {
    crate::certifier::convert::test_support::*,
    crate::*,
    curios_analysis::held_levels,
    curios_common::Qualifier,
    curios_core::{
        Global, Intrinsic, Level, LevelHead, Reducer, Term, UniverseContext, UniverseParam,
        Variance,
    },
};

/// Whether `name` at two instances apart in its one level is one term at `type_`: as written, and reduced first, which leaves a projected former as two projections of one group.
fn two_instances_convert(certifier: &mut Certifier, name: Global, type_: &Term) -> [bool; 2] {
    let at = |level: u32| Term::instance(name, vec![Level::constant(level)]);
    let reduced = [0, 1].map(|level| certifier.reduce(at(level)).expect("an instance unfolds"));

    [
        convert(certifier, type_, &at(0), &at(1)),
        convert(certifier, type_, &reduced[0], &reduced[1]),
    ]
    .map(|verdict| verdict.expect("a verdict within the budget"))
}

// Two values compared with no type to direct them, as a stuck elimination's arm is: `wrap` at two instances is one value, `box` is two, and a payload is compared whatever the levels.
#[test]
fn a_value_compares_at_its_familys_variance() {
    let mut certifier = certifier();
    let wrap = declare_wrap(
        &mut certifier,
        vec![Variance::Irrelevant],
        Former::Projected,
    );
    let boxed = declare_box(&mut certifier, vec![Variance::Invariant]);
    let wrapped = |level: u32, held: usize| {
        Term::variant_at(
            wrap,
            [Level::constant(level)],
            [nat_type()],
            "wrap",
            [nat(held)],
        )
    };
    let boxing = |level: u32| {
        Term::variant_at(
            boxed,
            [Level::constant(level)],
            Vec::<Term>::new(),
            "box",
            [nat_type()],
        )
    };
    let untyped = Term::type_ground();

    assert_eq!(
        convert(&mut certifier, &untyped, &wrapped(0, 3), &wrapped(1, 3)),
        Ok(true)
    );
    assert_eq!(
        convert(&mut certifier, &untyped, &boxing(0), &boxing(1)),
        Ok(false)
    );
    assert_eq!(
        convert(&mut certifier, &untyped, &wrapped(0, 3), &wrapped(1, 4)),
        Ok(false)
    );
}

// Only the level goes uncompared. Either side leads, the arguments being opened at the left side's instance.
#[test]
fn an_irrelevant_levels_arguments_are_compared_at_either_instance() {
    let mut certifier = certifier();
    let wrap = declare_wrap(
        &mut certifier,
        vec![Variance::Irrelevant],
        Former::Projected,
    );
    let at = |level: u32, param: Term| {
        Term::induct_type_at(wrap, [Level::constant(level)], [param], Vec::<Term>::new())
    };
    let small = at(0, nat_type());
    let large = at(1, Term::intrinsic(Intrinsic::BoolType));
    let untyped = Term::type_ground();

    assert_eq!(
        convert(&mut certifier, &untyped, &small, &at(1, nat_type())),
        Ok(true)
    );
    assert_eq!(convert(&mut certifier, &untyped, &small, &large), Ok(false));
    assert_eq!(convert(&mut certifier, &untyped, &large, &small), Ok(false));
}

// `Alias.{u} = (A: Type u) => A` at two instances: the spines of one definition decide nothing once its levels differ, and both sides unfold to `Nat`. It is why a field that mentions a level only on such an instance is one type at every instance of it.
#[test]
fn a_definition_applied_at_two_instances_converts_by_unfolding() {
    let mut certifier = certifier();
    let alias = Global::Authored(Qualifier::from(["Alias"]));
    let carrier = binder(85, "A");
    let sort = Term::type_at(Level::param(UniverseParam(0)));
    certifier
        .define(
            &alias,
            &Term::func_type([(carrier, sort.clone())], sort.clone()),
            &Term::func([(carrier, sort)], Term::free_var(&carrier)),
            &UniverseContext {
                parameter_count: 1,
                quality_count: 0,
                constraints: Vec::new(),
            },
        )
        .expect("a name is declared once");
    let at = |level: u32| {
        Term::apply(
            Term::instance(alias, vec![Level::constant(level)]),
            [nat_type()],
        )
    };

    assert_eq!(
        convert(&mut certifier, &Term::type_ground(), &at(0), &at(1)),
        Ok(true)
    );
}

// A former not yet applied is a function, and eta applies both sides to one binder before either head is read, so the nodes they build are what is compared: by variance, as any two nodes are. Carried invariant, the same pair is two functions. Reduced first, a projected former is two projections of one group, whose levels would refuse the pair before its type was read.
#[test]
fn two_bare_formers_apart_in_an_irrelevant_level_convert_at_their_type() {
    let carrier = binder(83, "A");
    let family = Term::func_type([(carrier, Term::type_ground())], Term::type_ground());

    for former in [Former::Projected, Former::Plain] {
        for (variance, converts) in [(Variance::Irrelevant, true), (Variance::Invariant, false)] {
            let mut certifier = certifier();
            let wrap = declare_wrap(&mut certifier, vec![variance], former);

            assert_eq!(
                two_instances_convert(&mut certifier, wrap, &family),
                [converts; 2],
                "{former:?}, {variance:?}"
            );
        }
    }
}

// A family with no parameter is applied in full by its bare name, so two instances of the name apart in an irrelevant level are one type, in either spelling of its former and whether or not the pair was reduced first.
#[test]
fn a_family_with_no_parameter_converts_by_its_name_at_two_instances() {
    for former in [Former::Projected, Former::Plain] {
        for (variance, converts) in [(Variance::Irrelevant, true), (Variance::Invariant, false)] {
            let mut certifier = certifier();
            let leaf = declare_leaf(&mut certifier, vec![variance], former);

            assert_eq!(
                two_instances_convert(&mut certifier, leaf, &Term::type_ground()),
                [converts; 2],
                "{former:?}, {variance:?}"
            );
        }
    }
}

// An arm's equation answers at another instance of a level its scrutinee's family is irrelevant in, and at no other instance of one it is invariant in: what reduction asks conversion of a stuck form and an equation's scrutinee is compared by variance, as every pair is. `curios-elab`'s `reduce::question_tests` holds this proposition under this name.
#[test]
fn a_case_equation_answers_at_another_instance_of_an_irrelevant_level() {
    let mut certifier = certifier();
    let wrap = declare_wrap(
        &mut certifier,
        vec![Variance::Irrelevant],
        Former::Projected,
    );
    let boxed = declare_box(&mut certifier, vec![Variance::Invariant]);
    let over = binder(90, "over");
    certifier.assume(
        &over,
        &Term::func_type(
            [(binder(91, "T"), Term::type_at(Level::constant(2)))],
            nat_type(),
        ),
    );
    let call = |family: Term| Term::apply(Term::free_var(&over), [family]);
    let wrapped = |level: u32| {
        call(Term::induct_type_at(
            wrap,
            [Level::constant(level)],
            [nat_type()],
            Vec::<Term>::new(),
        ))
    };
    let boxing = |level: u32| {
        call(Term::induct_type_at(
            boxed,
            [Level::constant(level)],
            Vec::<Term>::new(),
            Vec::<Term>::new(),
        ))
    };

    certifier.scoped(|certifier| {
        certifier
            .refine(wrapped(0), nat(1))
            .expect("the equation records");
        assert_eq!(whnf(certifier, wrapped(1)), Ok(nat(1)));
    });
    certifier.scoped(|certifier| {
        certifier
            .refine(boxing(0), nat(1))
            .expect("the equation records");
        assert_eq!(whnf(certifier, boxing(1)), Ok(boxing(1)));
    });
}

// A lambda's body is read by the vector the certifier filed for its family: `(A: Type u) => Wrap.{v}(A)` holds `u`, its binder's, and not `v`, which `Wrap` is irrelevant in, where over `Box`, invariant in its level, both are held.
#[test]
fn a_lambdas_body_is_read_by_the_vector_filed_for_its_family() {
    let mut certifier = certifier();
    let wrap = declare_wrap(
        &mut certifier,
        vec![Variance::Irrelevant],
        Former::Projected,
    );
    let boxed = declare_box(&mut certifier, vec![Variance::Invariant]);
    let carrier = binder(90, "A");
    let level = |index: usize| Level::param(UniverseParam(index));
    let over = |body: Term| Term::func([(carrier, Term::type_at(level(0)))], body);
    let held = |certifier: &mut Certifier, term: Term| {
        let found = held_levels(certifier, &[term]);

        [0, 1].map(|index| found.holds(LevelHead::Param(UniverseParam(index))))
    };

    let wrapped = Term::induct_type_at(
        wrap,
        [level(1)],
        [Term::free_var(&carrier)],
        Vec::<Term>::new(),
    );
    assert_eq!(held(&mut certifier, over(wrapped)), [true, false]);

    let boxing = Term::induct_type_at(boxed, [level(1)], Vec::<Term>::new(), Vec::<Term>::new());
    assert_eq!(held(&mut certifier, over(boxing)), [true, true]);
}

// `pair.{u}: {Type u, Nat} = (Nat, 3)` and `held.{u}: {Type (u + 1)} = (Type u,)`, each projected at two instances: the certifier forces a projection to its field before it compares heads, so a field whose value drops the level is one term at both, and one whose value holds it is two. The elaborator's twin of this proposition shares the name.
#[test]
fn a_field_that_drops_a_level_is_one_term_at_two_instances() {
    let mut certifier = certifier();
    let level = || Level::param(UniverseParam(0));
    let scheme = UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: Vec::new(),
    };
    let pair = Global::Authored(Qualifier::from(["pair"]));
    let held = Global::Authored(Qualifier::from(["held"]));
    certifier
        .define(
            &pair,
            &Term::tuple_type([
                (binder(88, "first"), Term::type_at(level())),
                (binder(89, "second"), nat_type()),
            ]),
            &Term::tuple([nat_type(), nat(3)]),
            &scheme,
        )
        .expect("a name is declared once");
    certifier
        .define(
            &held,
            &Term::tuple_type([(
                binder(92, "first"),
                Term::type_at(level().succ().expect("a parameter has a successor")),
            )]),
            &Term::tuple([Term::type_at(level())]),
            &scheme,
        )
        .expect("a name is declared once");
    let first = |name: Global, level: u32| {
        Term::proj(Term::instance(name, vec![Level::constant(level)]), 0)
    };

    assert_eq!(
        convert(
            &mut certifier,
            &Term::type_ground(),
            &first(pair, 0),
            &first(pair, 1)
        ),
        Ok(true)
    );
    assert_eq!(
        convert(
            &mut certifier,
            &Term::type_ground(),
            &first(held, 0),
            &first(held, 1)
        ),
        Ok(false)
    );
}
