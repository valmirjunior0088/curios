use {
    super::*,
    crate::{
        core::{Atom, Flt, Int, Nat, Prim, Term, Var},
        ersd, text,
    },
    std::time::Duration,
};

fn context() -> Context {
    Context::new(Duration::from_secs(1))
}

#[test]
fn erase_dependent_tuple_type_over_atom_match_and_tuple_value() {
    let mut context = context();

    let tuple_type = Term::tuple_type([
        ("x", Term::atom_type(["left", "right"])),
        (
            "y",
            Term::match_(
                Term::var(Var::free("x")),
                Some("m"),
                Term::type_(),
                vec![
                    ("left", Term::atom_type(["hot"])),
                    ("right", Term::atom_type(["cold"])),
                ],
            ),
        ),
    ]);

    erase(&mut context, &tuple_type, &Term::type_()).unwrap();

    let tuple = Term::tuple([
        Term::atom(Atom::from("left")),
        Term::atom(Atom::from("hot")),
    ]);

    erase(&mut context, &tuple, &tuple_type).unwrap();

    let tuple = Term::tuple([
        Term::atom(Atom::from("right")),
        Term::atom(Atom::from("cold")),
    ]);

    erase(&mut context, &tuple, &tuple_type).unwrap();
}

#[test]
fn erase_dependent_tuple_type_rejects_wrong_branch_atom() {
    let mut context = context();

    let tuple_type = Term::tuple_type([
        ("x", Term::atom_type(["left", "right"])),
        (
            "y",
            Term::match_(
                Term::var(Var::free("x")),
                Some("m"),
                Term::type_(),
                vec![
                    ("left", Term::atom_type(["hot"])),
                    ("right", Term::atom_type(["cold"])),
                ],
            ),
        ),
    ]);

    let tuple = Term::tuple([
        Term::atom(Atom::from("left")),
        Term::atom(Atom::from("cold")),
    ]);

    assert!(matches!(
        erase(&mut context, &tuple, &tuple_type),
        Err(Error::TypeMismatch { .. })
    ));
}

#[test]
fn erase_match_singleton_lowers_to_match() {
    let type_ = text::to_core(&"'[yes, no]".parse().unwrap(), &text::NullLoader)
        .unwrap()
        .term;

    let term = text::to_core(
        &r#"
                let x : '[unit] = 'unit;
                match x : _ => '[yes, no]
                | 'unit => 'yes
                end
            "#
        .parse()
        .unwrap(),
        &text::NullLoader,
    )
    .unwrap()
    .term;

    let erased = erase(&mut Context::new(Duration::from_secs(1)), &term, &type_).unwrap();

    let ersd::Term::Let(ersd::Let { body, tail, .. }) = erased else {
        panic!("expected let");
    };

    assert!(matches!(*body, ersd::Term::Atom(ersd::Atom { index: 0 })));
    assert!(matches!(*tail, ersd::Term::Match(_)));
}

#[test]
fn type_mismatch_from_expect_carries_span() {
    // A conversion mismatch raised by `expect` (here: `Type` erased against an
    // atom type) must still carry the offending term's span. These errors used
    // to escape `erase`'s span wrapper through the arm-level `?`.
    let term = text::to_core(&"Type".parse().unwrap(), &text::NullLoader)
        .unwrap()
        .term;
    let expected = text::to_core(&"'[a]".parse().unwrap(), &text::NullLoader)
        .unwrap()
        .term;

    let error = erase(&mut context(), &term, &expected).unwrap_err();

    assert!(matches!(error, Error::Located { .. }));
}

#[test]
fn erase_rec_single_identity_function() {
    let mut context = context();

    let func_type = Term::func_type([("x", Term::atom_type(["a"]))], Term::atom_type(["a"]));

    let term = Term::rec(
        vec![(
            "f",
            func_type.clone(),
            Term::func(["x"], Term::var(Var::free("x"))),
        )],
        Term::var(Var::free("f")),
    );

    erase(&mut context, &term, &func_type).unwrap();
}

#[test]
fn erase_preempts_on_cyclic_expected_type() {
    let mut context = context();

    context.define("loop", &Term::var(Var::free("loop")));

    assert!(matches!(
        erase(&mut context, &Term::type_(), &Term::var(Var::free("loop"))),
        Err(Error::ConvertPreempted { .. })
    ));
}

#[test]
fn erase_accepts_term_level_loop_with_stable_type() {
    let mut context = context();

    let type_ = Term::atom_type(["a"]);

    let term = Term::rec(
        vec![("loop", type_.clone(), Term::var(Var::free("loop")))],
        Term::var(Var::free("loop")),
    );

    erase(&mut context, &term, &type_).unwrap();
}

#[test]
fn erase_prim_ops_typecheck() {
    let mut context = context();

    erase(
        &mut context,
        &Subterm::Prim(Prim::int_eql(
            Subterm::Prim(Prim::Int(Int::new(1))),
            Subterm::Prim(Prim::Int(Int::new(1))),
        ))
        .into(),
        &Subterm::Prim(Prim::BlnType).into(),
    )
    .unwrap();

    erase(
        &mut context,
        &Subterm::Prim(Prim::flt_add(
            Subterm::Prim(Prim::Flt(Flt::from_f32(1.5))),
            Subterm::Prim(Prim::Flt(Flt::from_f32(2.0))),
        ))
        .into(),
        &Subterm::Prim(Prim::FltType).into(),
    )
    .unwrap();
}

#[test]
fn erase_func_captures_free_variables_before_opening_body() {
    let atom_type = Term::atom_type(["a"]);
    let tuple_type = Term::tuple_type([("z", atom_type.clone()), ("w", atom_type.clone())]);
    let type_ = Term::func_type([("x", atom_type.clone())], tuple_type);
    let term = Term::func(
        ["x"],
        Term::tuple([Term::var(Var::free("x")), Term::var(Var::free("y"))]),
    );

    let mut context = Context::new(Duration::from_secs(1));
    context.assume("y", &atom_type);

    let erased = erase(&mut context, &term, &type_).unwrap();

    let ersd::Term::Func(ersd::Func { captures, .. }) = erased else {
        panic!("expected erased func");
    };

    assert_eq!(captures.len(), 1);
    assert!(captures.contains(&"y".to_string()));
}

#[test]
fn erase_rejects_wrong_prim_operand_types() {
    assert!(matches!(
        erase(
            &mut Context::new(Duration::from_secs(1)),
            &Subterm::Prim(Prim::int_add(
                Subterm::Prim(Prim::Int(Int::new(1))),
                Subterm::Prim(Prim::Flt(Flt::from_f32(2.0)))
            ))
            .into(),
            &Subterm::Prim(Prim::IntType).into(),
        ),
        Err(Error::TypeMismatch { .. })
    ));
}

#[test]
fn erase_match_and_atom_stress_test() {
    let type_ = text::to_core(&"'[zeta, alpha, mu]".parse().unwrap(), &text::NullLoader)
        .unwrap()
        .term;

    let term = text::to_core(
        &r#"
                let outer : '[zeta, alpha, mu] = 'mu;
                let alpha_case : '[zeta, alpha, mu] = 'alpha;
                let mu_case : '[zeta, alpha, mu] = 'mu;
                let zeta_case : '[zeta, alpha, mu] = 'zeta;
                match outer : subject => '[zeta, alpha, mu]
                | 'zeta =>
                    match alpha_case : nested => '[zeta, alpha, mu]
                    | 'zeta => 'alpha
                    | 'alpha => 'mu
                    | 'mu => 'zeta
                    end
                | 'alpha =>
                    match zeta_case : nested => '[zeta, alpha, mu]
                    | 'zeta => 'mu
                    | 'alpha => 'zeta
                    | 'mu => 'alpha
                    end
                | 'mu =>
                    match mu_case : nested => '[zeta, alpha, mu]
                    | 'zeta => 'zeta
                    | 'alpha => 'alpha
                    | 'mu => 'mu
                    end
                end
            "#
        .parse()
        .unwrap(),
        &text::NullLoader,
    )
    .unwrap()
    .term;

    let erased = erase(&mut Context::new(Duration::from_secs(1)), &term, &type_).unwrap();

    let ersd::Term::Let(ersd::Let {
        name: outer_name,
        body: outer_body,
        tail,
    }) = erased
    else {
        panic!("expected outer let");
    };

    assert_eq!(outer_name, "outer#0");
    assert!(matches!(
        *outer_body,
        ersd::Term::Atom(ersd::Atom { index: 1 })
    ));

    let ersd::Term::Let(ersd::Let {
        name: alpha_name,
        body: alpha_body,
        tail,
    }) = *tail
    else {
        panic!("expected alpha_case let");
    };

    assert_eq!(alpha_name, "alpha_case#1");
    assert!(matches!(
        *alpha_body,
        ersd::Term::Atom(ersd::Atom { index: 0 })
    ));

    let ersd::Term::Let(ersd::Let {
        name: mu_name,
        body: mu_body,
        tail,
    }) = *tail
    else {
        panic!("expected mu_case let");
    };

    assert_eq!(mu_name, "mu_case#2");
    assert!(matches!(
        *mu_body,
        ersd::Term::Atom(ersd::Atom { index: 1 })
    ));

    let ersd::Term::Let(ersd::Let {
        name: zeta_name,
        body: zeta_body,
        tail,
    }) = *tail
    else {
        panic!("expected zeta_case let");
    };

    assert_eq!(zeta_name, "zeta_case#3");
    assert!(matches!(
        *zeta_body,
        ersd::Term::Atom(ersd::Atom { index: 2 })
    ));

    let ersd::Term::Match(ersd::Match { head, cases }) = *tail else {
        panic!("expected outer erased case");
    };

    assert!(matches!(
        *head,
        ersd::Term::Name(name) if name.as_str() == "outer#0"
    ));

    assert_eq!(cases.len(), 3);

    let ersd::Term::Match(ersd::Match {
        head: alpha_head,
        cases: alpha_cases,
    }) = &*cases[0]
    else {
        panic!("expected nested case for 'alpha case");
    };

    assert!(matches!(
        &**alpha_head,
        ersd::Term::Name(name) if name.as_str() == "zeta_case#3"
    ));

    assert_eq!(alpha_cases.len(), 3);
    assert!(matches!(
        *alpha_cases[0],
        ersd::Term::Atom(ersd::Atom { index: 2 })
    ));
    assert!(matches!(
        *alpha_cases[1],
        ersd::Term::Atom(ersd::Atom { index: 0 })
    ));
    assert!(matches!(
        *alpha_cases[2],
        ersd::Term::Atom(ersd::Atom { index: 1 })
    ));

    let ersd::Term::Match(ersd::Match {
        head: mu_head,
        cases: mu_cases,
    }) = &*cases[1]
    else {
        panic!("expected nested case for 'mu case");
    };

    assert!(matches!(
        &**mu_head,
        ersd::Term::Name(name) if name.as_str() == "mu_case#2"
    ));

    assert_eq!(mu_cases.len(), 3);

    assert!(matches!(
        *mu_cases[0],
        ersd::Term::Atom(ersd::Atom { index: 0 })
    ));

    assert!(matches!(
        *mu_cases[1],
        ersd::Term::Atom(ersd::Atom { index: 1 })
    ));

    assert!(matches!(
        *mu_cases[2],
        ersd::Term::Atom(ersd::Atom { index: 2 })
    ));

    let ersd::Term::Match(ersd::Match {
        head: zeta_head,
        cases: zeta_cases,
    }) = &*cases[2]
    else {
        panic!("expected nested case for 'zeta case");
    };

    assert!(matches!(
        &**zeta_head,
        ersd::Term::Name(name) if name.as_str() == "alpha_case#1"
    ));

    assert_eq!(zeta_cases.len(), 3);

    assert!(matches!(
        *zeta_cases[0],
        ersd::Term::Atom(ersd::Atom { index: 1 })
    ));

    assert!(matches!(
        *zeta_cases[1],
        ersd::Term::Atom(ersd::Atom { index: 2 })
    ));

    assert!(matches!(
        *zeta_cases[2],
        ersd::Term::Atom(ersd::Atom { index: 0 })
    ));
}

#[test]
fn erase_arr_nat_type_literal_len_and_get() {
    let mut context = context();

    let arr_nat = Subterm::Prim(Prim::arr_type(Subterm::Prim(Prim::NatType))).into();
    erase(&mut context, &arr_nat, &Term::type_()).unwrap();

    let literal = Subterm::Prim(Prim::arr(vec![
        Subterm::Prim(Prim::Nat(Nat::new(1usize))),
        Subterm::Prim(Prim::Nat(Nat::new(2usize))),
    ]))
    .into();
    erase(&mut context, &literal, &arr_nat).unwrap();

    context.assume("xs", &arr_nat);
    let len = Subterm::Prim(Prim::arr_len(
        Subterm::Prim(Prim::NatType),
        Term::var(Var::free("xs")),
    ))
    .into();
    assert_eq!(
        infer(&mut context, &len).unwrap(),
        Subterm::Prim(Prim::NatType).into()
    );

    let get = Subterm::Prim(Prim::arr_get(
        Subterm::Prim(Prim::NatType),
        Term::var(Var::free("xs")),
        Subterm::Prim(Prim::Nat(Nat::new(0usize))),
    ))
    .into();
    assert_eq!(
        infer(&mut context, &get).unwrap(),
        Subterm::Prim(Prim::NatType).into()
    );
}

#[test]
fn erase_bin_type_literal_len_and_get() {
    let mut context = context();

    let bin_type = Subterm::Prim(Prim::BinType).into();
    erase(&mut context, &bin_type, &Term::type_()).unwrap();

    let literal = Subterm::Prim(Prim::Bin(vec![1, 2, 3])).into();
    assert_eq!(infer(&mut context, &literal).unwrap(), bin_type);
    erase(&mut context, &literal, &bin_type).unwrap();

    context.assume("b", &bin_type);
    let len = Subterm::Prim(Prim::bin_len(Term::var(Var::free("b")))).into();
    assert_eq!(
        infer(&mut context, &len).unwrap(),
        Subterm::Prim(Prim::NatType).into()
    );

    let get = Subterm::Prim(Prim::bin_get(
        Term::var(Var::free("b")),
        Subterm::Prim(Prim::Nat(Nat::new(0usize))),
    ))
    .into();
    assert_eq!(
        infer(&mut context, &get).unwrap(),
        Subterm::Prim(Prim::NatType).into()
    );
}

#[test]
fn erase_bin_append() {
    let mut context = context();

    let bin_type = Subterm::Prim(Prim::BinType).into();
    context.assume("b", &bin_type);
    context.assume("n", &Subterm::Prim(Prim::NatType).into());

    let append = Subterm::Prim(Prim::bin_append(
        Term::var(Var::free("b")),
        Term::var(Var::free("n")),
    ))
    .into();
    assert_eq!(infer(&mut context, &append).unwrap(), bin_type);
    erase(&mut context, &append, &bin_type).unwrap();
}

#[test]
fn erase_bin_eql() {
    let mut context = context();

    let bin_type = Subterm::Prim(Prim::BinType).into();
    let bool_type = Subterm::Prim(Prim::BlnType).into();
    context.assume("a", &bin_type);
    context.assume("b", &bin_type);

    let eql = Subterm::Prim(Prim::bin_eql(
        Term::var(Var::free("a")),
        Term::var(Var::free("b")),
    ))
    .into();
    assert_eq!(infer(&mut context, &eql).unwrap(), bool_type);
    erase(&mut context, &eql, &bool_type).unwrap();
}

#[test]
fn erase_nat_eql_returns_bool_atom() {
    let mut context = context();

    let bool_type = Subterm::Prim(Prim::BlnType).into();

    let eql = Subterm::Prim(Prim::nat_eql(
        Subterm::Prim(Prim::Nat(Nat::new(0usize))),
        Subterm::Prim(Prim::Nat(Nat::new(0usize))),
    ))
    .into();

    assert_eq!(infer(&mut context, &eql).unwrap(), bool_type);
    erase(&mut context, &eql, &bool_type).unwrap();
}

#[test]
fn erase_nat_fold_rejects_non_nat_head() {
    let mut context = context();

    let bool_type = Term::atom_type(["false", "true"]);

    let nat_fold = Term::nat_induction(
        Term::prim(Prim::Int(Int::new(1))),
        Some("m"),
        Term::atom_type(["false", "true"]),
        Term::atom(Atom::from("false")),
        "pred",
        "ih",
        Term::atom(Atom::from("true")),
    );

    assert!(matches!(
        erase(&mut context, &nat_fold, &bool_type),
        Err(Error::NotNatType { .. })
    ));
}

#[test]
fn erase_nat_match_dispatches_to_named_case() {
    let mut context = context();

    let bool_type = Term::atom_type(["false", "true"]);

    let nat_match = Term::nat_dispatch(
        Term::prim(Prim::Nat(Nat::new(5usize))),
        Some("m"),
        Term::atom_type(["false", "true"]),
        [(5u32, Term::atom(Atom::from("true")))],
        Term::atom(Atom::from("false")),
    );

    erase(&mut context, &nat_match, &bool_type).unwrap();
}

#[test]
fn erase_nat_match_rejects_non_nat_head() {
    let mut context = context();

    let bool_type = Term::atom_type(["false", "true"]);

    let nat_match = Term::nat_dispatch(
        Term::prim(Prim::Int(Int::new(0))),
        Some("m"),
        Term::atom_type(["false", "true"]),
        [(0u32, Term::atom(Atom::from("true")))],
        Term::atom(Atom::from("false")),
    );

    assert!(matches!(
        erase(&mut context, &nat_match, &bool_type),
        Err(Error::NotNatType { .. })
    ));
}

#[test]
fn erase_lst_append() {
    let mut context = context();

    let arr_nat = Subterm::Prim(Prim::arr_type(Subterm::Prim(Prim::NatType))).into();
    context.assume("xs", &arr_nat);
    context.assume("n", &Subterm::Prim(Prim::NatType).into());

    let append = Subterm::Prim(Prim::arr_append(
        Subterm::Prim(Prim::NatType),
        Term::var(Var::free("xs")),
        Term::var(Var::free("n")),
    ))
    .into();
    assert_eq!(infer(&mut context, &append).unwrap(), arr_nat);
    erase(&mut context, &append, &arr_nat).unwrap();
}

#[test]
fn erase_three_field_tuple_type_and_value() {
    let mut context = context();

    let tuple_type = Term::tuple_type([
        ("x", Term::atom_type(["a"])),
        ("y", Term::atom_type(["b"])),
        ("z", Term::atom_type(["c"])),
    ]);

    erase(&mut context, &tuple_type, &Term::type_()).unwrap();

    let tuple = Term::tuple([
        Term::atom(Atom::from("a")),
        Term::atom(Atom::from("b")),
        Term::atom(Atom::from("c")),
    ]);

    erase(&mut context, &tuple, &tuple_type).unwrap();
}

#[test]
fn erase_bin_concat() {
    let mut context = context();

    let bin_type = Subterm::Prim(Prim::BinType).into();
    let concat = Subterm::Prim(Prim::bin_concat([
        Subterm::Prim(Prim::Bin(vec![1, 2])),
        Subterm::Prim(Prim::Bin(vec![3, 4])),
    ]))
    .into();

    erase(&mut context, &concat, &bin_type).unwrap();
}

#[test]
fn erase_bin_concat_rejects_wrong_expected_type() {
    let mut context = context();

    let concat = Subterm::Prim(Prim::bin_concat([
        Subterm::Prim(Prim::Bin(vec![1])),
        Subterm::Prim(Prim::Bin(vec![2])),
    ]))
    .into();

    assert!(matches!(
        erase(&mut context, &concat, &Subterm::Prim(Prim::NatType).into()),
        Err(Error::TypeMismatch { .. })
    ));
}

#[test]
fn erase_arr_concat() {
    let mut context = context();

    let arr_nat = Subterm::Prim(Prim::arr_type(Subterm::Prim(Prim::NatType))).into();
    context.assume("xs", &arr_nat);
    context.assume("ys", &arr_nat);

    let concat = Subterm::Prim(Prim::arr_concat(
        Subterm::Prim(Prim::NatType),
        [Term::var(Var::free("xs")), Term::var(Var::free("ys"))],
    ))
    .into();

    erase(&mut context, &concat, &arr_nat).unwrap();
}

#[test]
fn erase_arr_concat_rejects_wrong_expected_type() {
    let mut context = context();

    let arr_nat = Subterm::Prim(Prim::arr_type(Subterm::Prim(Prim::NatType))).into();
    context.assume("xs", &arr_nat);
    context.assume("ys", &arr_nat);

    let concat = Subterm::Prim(Prim::arr_concat(
        Subterm::Prim(Prim::NatType),
        [Term::var(Var::free("xs")), Term::var(Var::free("ys"))],
    ))
    .into();

    assert!(matches!(
        erase(&mut context, &concat, &Subterm::Prim(Prim::NatType).into()),
        Err(Error::TypeMismatch { .. })
    ));
}
