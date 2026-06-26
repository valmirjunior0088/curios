use {super::*, std::collections::BTreeSet};

#[test]
fn close_open_substitutes_label_name() {
    let term =
        Scope::close(One, &["x"], Term::free_var("x")).open(&[&Term::free_var("y")]);

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
