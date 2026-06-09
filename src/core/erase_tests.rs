use {
    super::*,
    crate::{
        core::{Flt, Int, Nat, Prim, Term, Var},
        ersd,
    },
    std::time::Duration,
};

fn context() -> Context {
    Context::new(Duration::from_secs(1))
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
            Term::func([("x", Term::type_())], Term::var(Var::free("x"))),
        )],
        Term::var(Var::free("f")),
    );

    erase(&mut context, &term, &func_type).unwrap();
}

#[test]
fn erase_accepts_term_level_loop_with_stable_type() {
    let mut context = context();

    let type_ = Term::prim(Prim::NatType);

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
    let nat_type = Term::prim(Prim::NatType);
    let tuple_type = Term::tuple_type([("z", nat_type.clone()), ("w", nat_type.clone())]);
    let type_ = Term::func_type([("x", nat_type.clone())], tuple_type);
    let term = Term::func(
        [("x", Term::type_())],
        Term::tuple([Term::var(Var::free("x")), Term::var(Var::free("y"))]),
    );

    let mut context = Context::new(Duration::from_secs(1));
    context.assume("y", &nat_type);

    let erased = erase(&mut context, &term, &type_).unwrap();

    let ersd::Subterm::Func(ersd::Func { captures, .. }) = erased.into_subterm() else {
        panic!("expected erased func");
    };

    assert_eq!(captures.len(), 1);
    assert!(captures.iter().any(|c| c.name == "y"));
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

    let nat_match = Term::nat_dispatch(
        Term::prim(Prim::Nat(Nat::new(5usize))),
        Some("m"),
        Term::prim(Prim::BlnType),
        [(5u32, Term::prim(Prim::Bln(true)))],
        Term::prim(Prim::Bln(false)),
    );

    erase(&mut context, &nat_match, &bool_type).unwrap();
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
