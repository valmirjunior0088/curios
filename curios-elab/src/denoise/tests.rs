use {
    curios_core::{Free, Intrinsic, Nat, Subterm, Term},
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
