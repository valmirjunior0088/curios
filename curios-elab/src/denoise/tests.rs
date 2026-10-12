use {
    crate::Context,
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Free, Global, Intrinsic, Level, Nat, Rec, RecGroup, Subterm, Term, UniverseContext,
        UniverseParam, UniverseRole, Universes,
    },
    curios_utilities::Qualifier,
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
        panic!("the walk changed the root: {refolded}");
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

/// `rec sized(A: Type u): Nat = sized(A)`, whose level only sizes the parameter, defined under the name it is called by: the name, and the group.
fn sized(context: &mut Context) -> (Free, RecGroup) {
    let sized = context.fresh(Some("sized"));
    let a = context.fresh(Some("A"));
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let domain = || Term::type_at(Level::param(UniverseParam(0)));
    let written = Term::rec(
        [(
            sized,
            Term::func_type([(a, domain())], nat()),
            Term::func(
                [(a, domain())],
                Term::apply(Term::free_var(&sized), [Term::free_var(&a)]),
            ),
        )],
        Term::free_var(&sized),
    );
    let Subterm::Rec(Rec { group, .. }) = &*written else {
        unreachable!()
    };
    let group = group.clone().with_universe_context(UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: Vec::new(),
    });
    let name = Free::Global(Global::Authored(Qualifier::from(["sized"])));
    context.define(&name, &Term::rec_proj(group.clone(), 0), None);

    (name, group)
}

/// A group reduction opened at an instance is spelled by its member's name at that instance: two calls of one definition apart in a level alone are two spellings, which a report that shows universes tells apart.
#[test]
fn a_member_of_an_opened_instance_is_spelled_at_its_levels() {
    let mut context = Context::new(100_000, SYNTAX);
    let (name, group) = sized(&mut context);

    for level in [Level::constant(1), Level::constant(2)] {
        let instance = context
            .opened_instance(&group, &Universes::levels(vec![level.clone()]))
            .expect("one level for the group's one parameter");

        assert_eq!(
            super::refold_recs(&context, &Term::rec_proj(instance, 0)),
            Term::instance_of(&name, vec![level])
        );
    }
}

/// A reduct the table serves at another spelling's levels holds the instance reduction would have opened there, and its member is named at those levels as that one's would be: `sized` at one level is reduced, the entry answers `sized` at another, and what it hands back names `sized` at the level asked. Served unrecorded, the instance would stand in whatever is committed from it as a copy of the group.
#[test]
fn a_member_of_an_instance_served_at_other_levels_is_named_at_them() {
    let mut context = Context::new(100_000, SYNTAX);
    let (name, group) = sized(&mut context);
    let mut level = || {
        Level::meta(
            context
                .universes_mut()
                .fresh(UniverseRole::Generalizable, None),
        )
    };
    let (stored, asked) = (level(), level());

    let instance = context
        .opened_instance(&group, &Universes::levels(vec![stored.clone()]))
        .expect("one level for the group's one parameter");
    context.reduce(
        Term::instance_of(&name, vec![stored]),
        &Term::rec_proj(instance, 0),
    );

    let served = context
        .cached_reduced(&Term::instance_of(&name, vec![asked.clone()]))
        .expect("the entry answers at another level");
    assert_eq!(
        super::named_members(&context, &served),
        Term::instance_of(&name, vec![asked])
    );
}
