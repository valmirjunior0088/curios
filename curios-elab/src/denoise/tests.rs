use {
    crate::*,
    curios_analysis::test_support::*,
    curios_common::Qualifier,
    curios_core::{
        DefinitionKind, Free, Global, Intrinsic, Nat, Rec, RecGroup, Subterm, Term, Universes,
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

/// `rec spin(n: Nat): Nat = spin(n)`: a group of one member and no level, which two names may each be defined as.
fn spin(context: &mut Context) -> RecGroup {
    let spin = context.fresh(Some("spin"));
    let n = context.fresh(Some("n"));
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let written = Term::rec(
        [(
            spin,
            Term::func_type([(n, nat())], nat()),
            Term::func(
                [(n, nat())],
                Term::apply(Term::free_var(&spin), [Term::free_var(&n)]),
            ),
        )],
        Term::free_var(&spin),
    );
    let Subterm::Rec(Rec { group, .. }) = &*written else {
        unreachable!()
    };

    group.clone()
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
