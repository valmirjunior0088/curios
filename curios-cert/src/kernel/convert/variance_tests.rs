//! A nominal node's levels compared by its family's variance: what the arms compare, what they still compare whatever the levels, and what reaches them.

use {
    super::test_support::*,
    crate::{Kernel, convert},
    curios_core::{
        Free, Intrinsic, Level, Reducer, Term, UniverseContext, UniverseParam, Variance,
    },
};

/// Whether `name` at two instances apart in its one level is one term at `type_`: as written, and reduced first, which leaves a projected former as two projections of one group.
fn two_instances_convert(kernel: &mut Kernel, name: &Free, type_: &Term) -> [bool; 2] {
    let at = |level: u32| Term::instance_of(name, vec![Level::constant(level)]);
    let reduced = [0, 1].map(|level| kernel.reduce(at(level)).expect("an instance unfolds"));

    [
        convert(kernel, type_, &at(0), &at(1)),
        convert(kernel, type_, &reduced[0], &reduced[1]),
    ]
    .map(|verdict| verdict.expect("a verdict within the budget"))
}

// Two values compared with no type to direct them, as a stuck elimination's arm is: `wrap` at two instances is one value, `box` is two, and a payload is compared whatever the levels.
#[test]
fn a_value_compares_at_its_familys_variance() {
    let mut kernel = kernel();
    let wrap = declare_wrap(&mut kernel, vec![Variance::Irrelevant], Former::Projected);
    let boxed = declare_box(&mut kernel, vec![Variance::Invariant]);
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
        convert(&mut kernel, &untyped, &wrapped(0, 3), &wrapped(1, 3)),
        Ok(true)
    );
    assert_eq!(
        convert(&mut kernel, &untyped, &boxing(0), &boxing(1)),
        Ok(false)
    );
    assert_eq!(
        convert(&mut kernel, &untyped, &wrapped(0, 3), &wrapped(1, 4)),
        Ok(false)
    );
}

// Only the level goes uncompared. Either side leads, the arguments being opened at the left side's instance.
#[test]
fn an_irrelevant_levels_arguments_are_compared_at_either_instance() {
    let mut kernel = kernel();
    let wrap = declare_wrap(&mut kernel, vec![Variance::Irrelevant], Former::Projected);
    let at = |level: u32, param: Term| {
        Term::induct_type_at(wrap, [Level::constant(level)], [param], Vec::<Term>::new())
    };
    let small = at(0, nat_type());
    let large = at(1, Term::intrinsic(Intrinsic::BoolType));
    let untyped = Term::type_ground();

    assert_eq!(
        convert(&mut kernel, &untyped, &small, &at(1, nat_type())),
        Ok(true)
    );
    assert_eq!(convert(&mut kernel, &untyped, &small, &large), Ok(false));
    assert_eq!(convert(&mut kernel, &untyped, &large, &small), Ok(false));
}

// `Alias.{u} = (A: Type u) => A` at two instances: the spines of one definition decide nothing once its levels differ, and both sides unfold to `Nat`. It is why a field that mentions a level only on such an instance is one type at every instance of it.
#[test]
fn a_definition_applied_at_two_instances_converts_by_unfolding() {
    let mut kernel = kernel();
    let alias = binder(84, "Alias");
    let carrier = binder(85, "A");
    let sort = Term::type_at(Level::param(UniverseParam(0)));
    kernel.define(
        &alias,
        &Term::func_type([(carrier, sort.clone())], sort.clone()),
        &Term::func([(carrier, sort)], Term::free_var(&carrier)),
        &UniverseContext {
            parameter_count: 1,
            constraints: Vec::new(),
        },
    );
    let at = |level: u32| {
        Term::apply(
            Term::instance_of(&alias, vec![Level::constant(level)]),
            [nat_type()],
        )
    };

    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &at(0), &at(1)),
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
            let mut kernel = kernel();
            let wrap = Free::from(&declare_wrap(&mut kernel, vec![variance], former));

            assert_eq!(
                two_instances_convert(&mut kernel, &wrap, &family),
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
            let mut kernel = kernel();
            let leaf = Free::from(&declare_leaf(&mut kernel, vec![variance], former));

            assert_eq!(
                two_instances_convert(&mut kernel, &leaf, &Term::type_ground()),
                [converts; 2],
                "{former:?}, {variance:?}"
            );
        }
    }
}
