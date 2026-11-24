use {
    crate::*,
    curios_analysis::test_support::*,
    curios_common::Qualifier,
    curios_core::{
        DefinitionKind, Free, Global, Intrinsic, Level, Nat, RecGroup, Subterm, Term,
        UniverseContext, UniverseParam, Universes, instantiate_universes,
    },
    std::rc::Rc,
};

/// `base` under sixty levels that each sum the one below with itself: a tree past anything a walk per path finishes, and a graph of sixty-one nodes.
fn doubled(base: Term) -> Term {
    let mut term = base;
    for _ in 0..60 {
        term = Term::intrinsic(Intrinsic::nat_add(term.clone(), term));
    }
    term
}

/// A report's term is refolded once per node of its graph, not once per path, and stays a graph.
#[test]
fn a_shared_term_is_refolded_once_per_node() {
    let refolded = super::refold_with(
        &Rc::new(Vec::new()),
        &doubled(Term::free_var(&Free::local(0, Some("a")))),
    );

    let Subterm::Intrinsic(Intrinsic::NatAdd(left, right)) = &*refolded else {
        panic!("the walk changed the root: {refolded:?}");
    };
    assert!(std::ptr::eq::<Subterm>(&**left, &**right));
}

/// A lambda applied where it stands is contracted for display, under a binder as at the root, and a lambda no argument reaches is left: `(c) => ((a, b) => a + b)(c, 1)` is shown as `(c) => c + 1`.
#[test]
fn an_applied_lambda_is_contracted_where_it_stands() {
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let a = Free::local(0, Some("a"));
    let b = Free::local(1, Some("b"));
    let c = Free::local(2, Some("c"));
    let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));

    let sum = Term::func(
        [(a, nat()), (b, nat())],
        Term::intrinsic(Intrinsic::nat_add(Term::free_var(&a), Term::free_var(&b))),
    );
    let applied = Term::func(
        [(c, nat())],
        Term::apply(sum, [Term::free_var(&c), one.clone()]),
    );
    let shown = Term::func(
        [(c, nat())],
        Term::intrinsic(Intrinsic::nat_add(Term::free_var(&c), one)),
    );

    assert_eq!(super::contract_redexes(&applied), shown);
}

/// `rec spin(n: Nat): Nat = spin(n)`: a group of one member and no level, which two names may each be defined as.
fn spin(context: &mut Context) -> RecGroup {
    let spin = context.fresh(Some("spin"));
    let n = context.fresh(Some("n"));
    let nat = || Term::intrinsic(Intrinsic::NatType);
    RecGroup::closing(vec![(
        spin,
        Term::func_type([(n, nat())], nat()),
        Term::func(
            [(n, nat())],
            Term::apply(Term::free_var(&spin), [Term::free_var(&n)]),
        ),
    )])
}

/// A group two declarations define is named by the one reduction opened it from, in either order the two are defined: what a committed term calls it follows what its own declaration unfolded, and no order the unit's declarations were elaborated in.
#[test]
fn a_group_two_names_define_is_named_by_the_one_reduction_opened() {
    let name = |label: &str| Global::Authored(Qualifier::from([label]));

    for order in [["first", "second"], ["second", "first"]] {
        let mut context = Context::new(100_000, SYNTAX);
        let group = spin(&mut context);
        for label in order {
            context.define_declaration(
                &name(label),
                &Term::rec_proj(group.clone(), 0),
                &DefinitionKind::Authored,
            );
        }

        assert!(
            context
                .value_at(&name("first"), &Universes::default())
                .is_some()
        );
        assert_eq!(
            super::named_members(&context, &Term::rec_proj(group, 0)),
            Term::instance(
                Global::Authored(Qualifier::from(["first"])),
                Universes::default()
            ),
            "defined in the order {order:?}",
        );
    }
}

/// `rec sized(A: Type u): Nat = sized(A)`, whose level only sizes the parameter, declared over that one level under the name it is called by: the name, and the group as the declaration states it.
fn sized(context: &mut Context) -> (Global, RecGroup) {
    let sized = context.fresh(Some("sized"));
    let a = context.fresh(Some("A"));
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let domain = || Term::type_at(Level::param(UniverseParam(0)));
    let group = RecGroup::closing(vec![(
        sized,
        Term::func_type([(a, domain())], nat()),
        Term::func(
            [(a, domain())],
            Term::apply(Term::free_var(&sized), [Term::free_var(&a)]),
        ),
    )]);

    let name = Global::Authored(Qualifier::from(["sized"]));
    context.declare(&name, &group.member_type(0));
    context.set_universe_context(
        &name,
        UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: Vec::new(),
        },
    );
    context.define_declaration(
        &name,
        &Term::rec_proj(group.clone(), 0),
        &DefinitionKind::Authored,
    );

    (name, group)
}

/// `group`'s first member as it stands at `universes`: what reduction leaves of a call that does not compute.
fn held_at(group: &RecGroup, universes: &Universes) -> Term {
    instantiate_universes(&Term::rec_proj(group.clone(), 0), universes)
        .expect("one level for the group's one parameter")
}

/// A group a term holds at an instance of its definition is spelled by its member's name at that instance: two calls of one definition apart in a level alone are two spellings, and neither is a copy of the group.
#[test]
fn a_member_of_a_group_held_at_an_instance_is_named_at_that_instance() {
    let mut context = Context::new(100_000, SYNTAX);
    let (name, group) = sized(&mut context);

    for level in [Level::constant(1), Level::constant(2)] {
        let universes = Universes::levels(vec![level]);
        // What reduction reads where it opens the name.
        assert!(context.value_at(&name, &universes).is_some());

        assert_eq!(
            super::named_members(&context, &held_at(&group, &universes)),
            Term::instance(name, universes)
        );
    }
}

/// An instance no reader opened is named as one a reader did: the reduction table serves a reduct at another spelling's levels, so the group a term holds may stand at levels its definition was never read at, and it is known by what it is.
#[test]
fn a_member_of_an_instance_no_reader_opened_is_named_at_its_levels() {
    let mut context = Context::new(100_000, SYNTAX);
    let (name, group) = sized(&mut context);
    assert!(
        context
            .value_at(&name, &Universes::levels(vec![Level::constant(1)]))
            .is_some()
    );

    let asked = Universes::levels(vec![Level::constant(2)]);
    assert_eq!(
        super::named_members(&context, &held_at(&group, &asked)),
        Term::instance(name, asked)
    );
}

/// A group reduction opened from no name stands as it was, at an instance as at none: the name a term is spelled by is one its declaration unfolded.
#[test]
fn an_instance_of_a_group_no_name_opened_stands_as_it_was() {
    let mut context = Context::new(100_000, SYNTAX);
    let (_, group) = sized(&mut context);

    let held = held_at(&group, &Universes::levels(vec![Level::constant(1)]));
    assert_eq!(super::named_members(&context, &held), held);
}
