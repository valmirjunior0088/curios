use {super::*, std::collections::BTreeSet};

#[test]
fn close_open_substitutes_label_name() {
    let term = Scope::close(One, &["x"], Term::var(Var::free("x")))
        .open(&[&Term::var(Var::free("y"))]);

    let Subterm::Var(var) = &*term else {
        panic!("unexpected `{term:?}`")
    };

    assert_eq!(var, &Var::free("y"));
}

#[test]
fn close_open_preserves_nested_bind() {
    let term = Scope::close(One, &["x"], Term::func(["y"], Term::var(Var::free("x"))))
        .open(&[&Term::var(Var::free("z"))]);

    let Subterm::Func(body) = &*term else {
        panic!("unexpected `{term:?}`")
    };

    let opened = body.body.open(&[&Term::var(Var::free("w"))]);
    let Subterm::Var(var) = &*opened else {
        panic!("unexpected term")
    };

    assert_eq!(var, &Var::free("z"));
}

#[test]
fn collect_ignores_index_names() {
    let term = Term::func(
        ["x"],
        Term::tuple([
            Term::var(Var::free("x")),
            Term::rec(
                vec![("y", Term::type_(), Term::var(Var::free("z")))],
                Term::tuple([Term::var(Var::free("y")), Term::var(Var::free("w"))]),
            ),
        ]),
    );

    assert_eq!(
        term.free_vars(),
        BTreeSet::from([String::from("w"), String::from("z")])
    );
}
