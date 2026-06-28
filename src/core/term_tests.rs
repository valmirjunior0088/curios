use {super::*, std::collections::BTreeSet};

#[test]
fn close_open_substitutes_label_name() {
    let term = Scope::close(One, &["x"], Term::free_var("x")).open(&[&Term::free_var("y")]);

    let Subterm::Var(var) = &*term else {
        panic!("unexpected `{term:?}`")
    };

    assert_eq!(var, &Var::free("y"));
}

#[test]
fn close_open_preserves_nested_bind() {
    let term = Scope::close(
        One,
        &["x"],
        Term::func([("y", Term::type_())], Term::free_var("x")),
    )
    .open(&[&Term::free_var("z")]);

    let Subterm::Func(body) = &*term else {
        panic!("unexpected `{term:?}`")
    };

    let opened = body.telescope.open(&[&Term::free_var("w")]);
    let Subterm::Var(var) = &*opened else {
        panic!("unexpected term")
    };

    assert_eq!(var, &Var::free("z"));
}

#[test]
fn collect_ignores_index_names() {
    let term = Term::func(
        [("x", Term::type_())],
        Term::tuple([
            Term::free_var("x"),
            Term::rec(
                vec![("y", Term::type_(), Term::free_var("z"))],
                Term::tuple([Term::free_var("y"), Term::free_var("w")]),
            ),
        ]),
    );

    assert_eq!(
        term.free_vars(),
        BTreeSet::from([String::from("w"), String::from("z")])
    );
}

#[test]
fn metavar_is_a_closed_global_head() {
    let m = Term::metavar(7);
    assert_eq!(m.reach(), 0);
    assert!(m.closed());
    assert_eq!(format!("{m}"), "?7");
}

#[test]
fn metavars_collects_ids_across_structure() {
    // (λx. ?1)(?2, Nat.add ?3 ?1)
    let term = Term::apply(
        Term::func([("x", Term::type_())], Term::metavar(1)),
        [
            Term::metavar(2),
            Term::prim(Prim::nat_add(Term::metavar(3), Term::metavar(1))),
        ],
    );
    assert_eq!(term.metavars(), BTreeSet::from([1, 2, 3].map(MetavarId)));
}

#[test]
fn metavar_is_inert_under_traversal() {
    // shifting/capture must not disturb a metavariable node
    let m = Term::metavar(4);
    assert_eq!(m.shift(3), m);
    let scope = Scope::close(One, &["x"], Term::metavar(4));
    assert_eq!(scope.open(&[&Term::free_var("y")]), Term::metavar(4));
}

#[test]
fn variant_collects_metavars_and_prints_as_function_call() {
    let ctor = Term::variant("Result", [Term::metavar(1)], "success", [Term::metavar(2)]);
    assert_eq!(ctor.metavars(), BTreeSet::from([1, 2].map(MetavarId)));
    assert_eq!(format!("{ctor}"), "Result/success(?2)");

    let type_ = Term::inductive_type(
        "Result",
        [Term::prim(Prim::NatType), Term::metavar(3)],
        Vec::<Term>::new(),
    );
    assert_eq!(type_.metavars(), BTreeSet::from([3].map(MetavarId)));
    assert_eq!(format!("{type_}"), "Result(Nat, ?3)");
}

#[test]
fn implicit_marks_print_and_default_to_explicit() {
    let ft = Term::func_type_marked(
        [
            (Plicity::Implicit, "T", Term::type_()),
            (Plicity::Explicit, "x", Term::free_var("T")),
        ],
        Term::free_var("T"),
    );
    assert_eq!(format!("{ft}"), "(@T : Type, x : T) -> T");

    // The unmarked builders default every slot to `Explicit`.
    let plain = Term::func_type([("T", Term::type_())], Term::type_());
    match &*plain {
        Subterm::FuncType(FuncType { plicities, .. }) => {
            assert_eq!(plicities, &[Plicity::Explicit]);
        }
        _ => unreachable!(),
    }

    let call = Term::apply_marked(
        Term::free_var("foo"),
        [
            (Plicity::Implicit, Term::free_var("Nat")),
            (Plicity::Explicit, Term::free_var("x")),
        ],
    );
    assert_eq!(format!("{call}"), "foo(@Nat, x)");
}

#[test]
fn inductive_match_case_binders_are_captured() {
    // match r : #m => Type; | success(value) => value;
    let term = Term::inductive_match(
        Term::free_var("r"),
        None,
        Term::type_(),
        [("success", vec!["value"], Term::free_var("value"))],
    );

    let free = term.free_vars();
    assert!(free.contains("r"));
    assert!(!free.contains("value"));
}

#[test]
fn inductive_variants_reach_spans_components() {
    assert_eq!(
        Term::inductive_type("Result", [Term::var(Var::bound(2))], Vec::<Term>::new()).reach(),
        3
    );
    assert_eq!(
        Term::variant(
            "Result",
            [Term::var(Var::bound(0))],
            "success",
            [Term::var(Var::bound(4))],
        )
        .reach(),
        5
    );
}

#[test]
fn reach_basic_values() {
    assert_eq!(Term::type_().reach(), 0);
    assert_eq!(Term::free_var("x").reach(), 0);
    assert_eq!(Term::var(Var::bound(0)).reach(), 1);
    assert_eq!(Term::var(Var::bound(3)).reach(), 4);
    // closed identity function λx.x
    assert_eq!(
        Term::func([("x", Term::type_())], Term::free_var("x")).reach(),
        0
    );
}

#[test]
fn reach_telescope_absorbs_arity() {
    // body references bound index 2 (reach 3); each telescope binder absorbs one.
    // `Scope::constant` places the body without capturing, so the bound index is
    // preserved exactly (unlike `Telescope::cons`, which captures by label).
    let f1 = Term::from(Subterm::Func(Func {
        telescope: Telescope::Cons(
            Term::type_(),
            Scope::constant(One, Telescope::done(Term::var(Var::bound(2)))),
        ),
    }));
    assert_eq!(f1.reach(), 2); // one binder: (2 + 1) - 1

    let f2 = Term::from(Subterm::Func(Func {
        telescope: Telescope::Cons(
            Term::type_(),
            Scope::constant(
                One,
                Telescope::Cons(
                    Term::type_(),
                    Scope::constant(One, Telescope::done(Term::var(Var::bound(2)))),
                ),
            ),
        ),
    }));
    assert_eq!(f2.reach(), 1); // two binders: (2 + 1) - 2
}
