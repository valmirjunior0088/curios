use {
    crate::*,
    curios_abi::RootId,
    curios_base::{Flt, Int},
    std::{collections::BTreeMap, time::Duration},
};

fn context() -> Context {
    Context::new(Duration::from_secs(1))
}

fn nat_lit(n: usize) -> Term {
    Term::prim(Prim::Nat(Nat::new(n)))
}

fn opt_type() -> Term {
    Term::inductive_type("Opt", Vec::<Term>::new(), Vec::<Term>::new())
}

// induct Opt : Type | none() | some(x : Nat) end — `none` is tag 0, `some` tag
// 1 (BTreeMap-sorted). A relevant (Type-sorted) data type, so its match erases
// through the tag-dispatch path rather than collapsing to a single arm.
fn register_opt(context: &mut Context) {
    context
        .register_inductive(
            "Opt",
            Inductive {
                params: Telescope::done(()),
                indices: Telescope::done(()),
                constructors: BTreeMap::from([
                    (
                        Atom::from("none"),
                        InductiveParam {
                            telescope: Telescope::done(opt_type()),
                        },
                    ),
                    (
                        Atom::from("some"),
                        InductiveParam {
                            telescope: Telescope::build(
                                [("x", Term::prim(Prim::NatType))],
                                opt_type(),
                            ),
                        },
                    ),
                ]),
                result_sort: Term::type_(),
                root: RootId::Entry,
            },
        )
        .unwrap();
}

#[test]
fn erase_rec_single_identity_function() {
    let mut context = context();

    let func_type = Term::func_type(
        [("x", Term::prim(Prim::NatType))],
        Term::prim(Prim::NatType),
    );

    let term = Term::rec(
        vec![(
            "f",
            func_type.clone(),
            Term::func([("x", Term::type_())], Term::free_var("x")),
        )],
        Term::free_var("f"),
    );

    erase(&mut context, &term, &func_type).unwrap();
}

#[test]
fn erase_accepts_term_level_loop_with_stable_type() {
    let mut context = context();

    let type_ = Term::prim(Prim::NatType);

    let term = Term::rec(
        vec![("loop", type_.clone(), Term::free_var("loop"))],
        Term::free_var("loop"),
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
    let nat_type = Term::prim(Prim::NatType);
    let tuple_type = Term::tuple_type([("z", nat_type.clone()), ("w", nat_type.clone())]);
    let type_ = Term::func_type([("x", nat_type.clone())], tuple_type);
    let term = Term::func(
        [("x", Term::type_())],
        Term::tuple([Term::free_var("x"), Term::free_var("y")]),
    );

    let mut context = Context::new(Duration::from_secs(1));
    context.assume("y", &nat_type);

    let erased = erase(&mut context, &term, &type_).unwrap();

    let curios_ersd::Subterm::Func(curios_ersd::Func { captures, .. }) = erased.into_subterm()
    else {
        panic!("expected erased func");
    };

    assert_eq!(captures.len(), 1);
    assert!(captures.iter().any(|c| c.name == "y"));
}

#[test]
fn erase_lst_nat_type_literal_len_and_get() {
    let mut context = context();

    let lst_nat = Subterm::Prim(Prim::lst_type(Subterm::Prim(Prim::NatType))).into();
    erase(&mut context, &lst_nat, &Term::type_()).unwrap();

    let literal = Subterm::Prim(Prim::lst(vec![
        Subterm::Prim(Prim::Nat(Nat::new(1usize))),
        Subterm::Prim(Prim::Nat(Nat::new(2usize))),
    ]))
    .into();
    erase(&mut context, &literal, &lst_nat).unwrap();

    context.assume("xs", &lst_nat);
    let len = Subterm::Prim(Prim::lst_len(
        Subterm::Prim(Prim::NatType),
        Term::free_var("xs"),
    ))
    .into();
    assert_eq!(
        infer(&mut context, &len).unwrap(),
        Subterm::Prim(Prim::NatType).into()
    );

    let get = Subterm::Prim(Prim::lst_get(
        Subterm::Prim(Prim::NatType),
        Term::free_var("xs"),
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
    let len = Subterm::Prim(Prim::bin_len(Term::free_var("b"))).into();
    assert_eq!(
        infer(&mut context, &len).unwrap(),
        Subterm::Prim(Prim::NatType).into()
    );

    let get = Subterm::Prim(Prim::bin_get(
        Term::free_var("b"),
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

    let append = Subterm::Prim(Prim::bin_append(Term::free_var("b"), Term::free_var("n"))).into();
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

    let eql = Subterm::Prim(Prim::bin_eql(Term::free_var("a"), Term::free_var("b"))).into();
    assert_eq!(infer(&mut context, &eql).unwrap(), bool_type);
    erase(&mut context, &eql, &bool_type).unwrap();
}

#[test]
fn erase_nat_eql_returns_bln() {
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
fn erase_nat_match_dispatches_to_named_case() {
    let mut context = context();

    let bool_type = Term::prim(Prim::BlnType);

    let nat_match = Term::switch(
        Term::prim(Prim::Nat(Nat::new(5usize))),
        Some("m"),
        Term::prim(Prim::BlnType),
        [(5u32, Term::prim(Prim::Bln(true)))],
        Term::prim(Prim::Bln(false)),
    );

    erase(&mut context, &nat_match, &bool_type).unwrap();
}

#[test]
fn erase_inductive_match_default_is_sparse() {
    let mut context = context();
    register_opt(&mut context);

    // `match some(5) : Nat | none() => 0 | _ => 99 end` — only `none` is
    // enumerated; `some` is covered by the catch-all. Erasure emits a *sparse*
    // dispatch: one case entry (the `none` tag) plus a single `default`, not a
    // dense slot per constructor.
    let term = Term::inductive_match_default(
        Term::variant("Opt", Vec::<Term>::new(), "some", [nat_lit(5)]),
        Some("m"),
        Term::prim(Prim::NatType),
        [("none", Vec::<&str>::new(), nat_lit(0))],
        nat_lit(99),
    );

    let erased = erase(&mut context, &term, &Term::prim(Prim::NatType)).unwrap();

    // The scrutinee is let-bound once, then dispatched on its tag.
    let curios_ersd::Subterm::Let(let_) = &*erased else {
        panic!("expected a scrutinee-binding Let, got:\n{erased}");
    };
    let curios_ersd::Subterm::Match(m) = &*let_.tail else {
        panic!("expected a Match tail, got:\n{}", let_.tail);
    };
    assert_eq!(m.cases.len(), 1, "only the `none` arm should be enumerated");
    assert!(
        m.cases.contains_key(&0),
        "the `none` tag (index 0) is present"
    );
    assert!(
        m.default.is_some(),
        "the catch-all becomes a sparse default"
    );
}

#[test]
fn erase_complete_inductive_match_has_no_default() {
    let mut context = context();
    register_opt(&mut context);

    // A fully-enumerated match keeps every constructor as an explicit arm and
    // needs no default — the sparse-omission-implies-default invariant.
    let term = Term::inductive_match(
        Term::variant("Opt", Vec::<Term>::new(), "some", [nat_lit(5)]),
        Some("m"),
        Term::prim(Prim::NatType),
        [
            ("none", Vec::<&str>::new(), nat_lit(0)),
            ("some", vec!["x"], Term::free_var("x")),
        ],
    );

    let erased = erase(&mut context, &term, &Term::prim(Prim::NatType)).unwrap();
    let curios_ersd::Subterm::Let(let_) = &*erased else {
        panic!("expected a scrutinee-binding Let, got:\n{erased}");
    };
    let curios_ersd::Subterm::Match(m) = &*let_.tail else {
        panic!("expected a Match tail, got:\n{}", let_.tail);
    };
    assert_eq!(m.cases.len(), 2, "both constructors are enumerated");
    assert!(m.default.is_none(), "a complete match needs no default");
}

#[test]
fn erase_lst_append() {
    let mut context = context();

    let lst_nat = Subterm::Prim(Prim::lst_type(Subterm::Prim(Prim::NatType))).into();
    context.assume("xs", &lst_nat);
    context.assume("n", &Subterm::Prim(Prim::NatType).into());

    let append = Subterm::Prim(Prim::lst_append(
        Subterm::Prim(Prim::NatType),
        Term::free_var("xs"),
        Term::free_var("n"),
    ))
    .into();
    assert_eq!(infer(&mut context, &append).unwrap(), lst_nat);
    erase(&mut context, &append, &lst_nat).unwrap();
}

#[test]
fn erase_three_field_tuple_type_and_value() {
    let mut context = context();

    let tuple_type = Term::tuple_type([
        ("x", Term::prim(Prim::NatType)),
        ("y", Term::prim(Prim::IntType)),
        ("z", Term::prim(Prim::BlnType)),
    ]);

    erase(&mut context, &tuple_type, &Term::type_()).unwrap();

    let tuple = Term::tuple([
        Term::prim(Prim::Nat(Nat::new(1usize))),
        Term::prim(Prim::Int(Int::new(2))),
        Term::prim(Prim::Bln(true)),
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
fn erase_lst_concat() {
    let mut context = context();

    let lst_nat = Subterm::Prim(Prim::lst_type(Subterm::Prim(Prim::NatType))).into();
    context.assume("xs", &lst_nat);
    context.assume("ys", &lst_nat);

    let concat = Subterm::Prim(Prim::lst_concat(
        Subterm::Prim(Prim::NatType),
        [Term::free_var("xs"), Term::free_var("ys")],
    ))
    .into();

    erase(&mut context, &concat, &lst_nat).unwrap();
}
