use {
    super::*,
    crate::core::{Atom, Int, Nat, Prim, Var},
    std::time::Duration,
};

fn context() -> Context {
    Context::new(Duration::from_millis(10))
}

fn conv(context: &mut Context, this: &Term, that: &Term) -> Result<bool, ReduceError> {
    convert(context, &Term::type_(), this, that)
}

#[test]
fn convert_func_type_is_alpha_equivalent() {
    let mut context = context();

    let this = Term::func_type([("x", Term::type_())], Term::var(Var::free("x")));

    let that = Term::func_type([("y", Term::type_())], Term::var(Var::free("y")));

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_func_is_alpha_equivalent() {
    let mut context = context();

    let this = Term::func(["x"], Term::var(Var::free("x")));

    let that = Term::func(["y"], Term::var(Var::free("y")));

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_match_compares_matches_and_motive() {
    let mut context = context();

    let this = Term::match_(
        Term::atom(Atom::from("a")),
        Some("m"),
        Term::type_(),
        vec![
            ("a", Term::atom(Atom::from("yes"))),
            ("b", Term::atom(Atom::from("no"))),
        ],
    );

    let that = Term::match_(
        Term::atom(Atom::from("a")),
        Some("n"),
        Term::type_(),
        vec![
            ("a", Term::atom(Atom::from("yes"))),
            ("b", Term::atom(Atom::from("no"))),
        ],
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_recurses_into_operands() {
    let mut context = context();

    let this = Term::func(
        ["x"],
        Subterm::Prim(Prim::int_add(
            Term::var(Var::free("x")),
            Subterm::Prim(Prim::Int(Int::new(1))),
        )),
    );

    let that = Term::func(
        ["y"],
        Subterm::Prim(Prim::int_add(
            Term::var(Var::free("y")),
            Subterm::Prim(Prim::Int(Int::new(1))),
        )),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_distinguishes_operator_kind() {
    let mut context = context();

    let this = Term::func(
        ["x"],
        Subterm::Prim(Prim::int_add(
            Term::var(Var::free("x")),
            Subterm::Prim(Prim::Int(Int::new(1))),
        )),
    );

    let that = Term::func(
        ["x"],
        Subterm::Prim(Prim::int_sub(
            Term::var(Var::free("x")),
            Subterm::Prim(Prim::Int(Int::new(1))),
        )),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

#[test]
fn convert_rec_is_alpha_equivalent() {
    let mut context = context();

    let this = Term::rec(
        vec![("x", Term::type_(), Term::var(Var::free("x")))],
        Term::var(Var::free("x")),
    );

    let that = Term::rec(
        vec![("y", Term::type_(), Term::var(Var::free("y")))],
        Term::var(Var::free("y")),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_nat_add_recurses_into_operands() {
    let mut context = context();

    let this = Term::func(
        ["x"],
        Subterm::Prim(Prim::nat_add(
            Term::var(Var::free("x")),
            Subterm::Prim(Prim::Nat(Nat::new(1usize))),
        )),
    );

    let that = Term::func(
        ["y"],
        Subterm::Prim(Prim::nat_add(
            Term::var(Var::free("y")),
            Subterm::Prim(Prim::Nat(Nat::new(1usize))),
        )),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_flt_neg_recurses_into_operand() {
    let mut context = context();

    let this = Term::func(
        ["x"],
        Subterm::Prim(Prim::flt_neg(Term::var(Var::free("x")))),
    );

    let that = Term::func(
        ["y"],
        Subterm::Prim(Prim::flt_neg(Term::var(Var::free("y")))),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_nat_to_int_recurses_into_operand() {
    let mut context = context();

    let this = Term::func(
        ["x"],
        Subterm::Prim(Prim::nat_to_int(Term::var(Var::free("x")))),
    );

    let that = Term::func(
        ["y"],
        Subterm::Prim(Prim::nat_to_int(Term::var(Var::free("y")))),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_arr_compares_element_wise() {
    let mut context = context();

    let this = Subterm::Prim(Prim::arr(vec![
        Subterm::Prim(Prim::Nat(Nat::new(1usize))),
        Subterm::Prim(Prim::Nat(Nat::new(2usize))),
    ]))
    .into();

    let that = Subterm::Prim(Prim::arr(vec![
        Subterm::Prim(Prim::Nat(Nat::new(1usize))),
        Subterm::Prim(Prim::Nat(Nat::new(2usize))),
    ]))
    .into();

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_arr_rejects_different_lengths() {
    let mut context = context();

    let this = Subterm::Prim(Prim::arr(vec![Subterm::Prim(Prim::Nat(Nat::new(1usize)))])).into();

    let that = Subterm::Prim(Prim::arr(vec![
        Subterm::Prim(Prim::Nat(Nat::new(1usize))),
        Subterm::Prim(Prim::Nat(Nat::new(2usize))),
    ]))
    .into();

    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

#[test]
fn convert_prim_bin_type_is_equal_to_itself() {
    let mut context = context();

    let this = Subterm::Prim(Prim::BinType).into();
    let that = Subterm::Prim(Prim::BinType).into();

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_bin_literal_compares_bytes() {
    let mut context = context();

    assert_eq!(
        conv(
            &mut context,
            &Subterm::Prim(Prim::Bin(vec![1, 2])).into(),
            &Subterm::Prim(Prim::Bin(vec![1, 2])).into(),
        ),
        Ok(true)
    );

    assert_eq!(
        conv(
            &mut context,
            &Subterm::Prim(Prim::Bin(vec![1, 2])).into(),
            &Subterm::Prim(Prim::Bin(vec![1, 3])).into(),
        ),
        Ok(false)
    );
}

#[test]
fn convert_prim_bin_len_recurses_into_operand() {
    let mut context = context();

    let this = Term::func(
        ["x"],
        Subterm::Prim(Prim::bin_len(Term::var(Var::free("x")))),
    );
    let that = Term::func(
        ["y"],
        Subterm::Prim(Prim::bin_len(Term::var(Var::free("y")))),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_bin_get_recurses_into_operands() {
    let mut context = context();

    let this = Term::func(
        ["x"],
        Term::func(
            ["a"],
            Subterm::Prim(Prim::bin_get(
                Term::var(Var::free("x")),
                Term::var(Var::free("a")),
            )),
        ),
    );

    let that = Term::func(
        ["y"],
        Term::func(
            ["b"],
            Subterm::Prim(Prim::bin_get(
                Term::var(Var::free("y")),
                Term::var(Var::free("b")),
            )),
        ),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_bin_concat_recurses_into_operands() {
    let mut context = context();

    let this = Term::func(
        ["x"],
        Term::func(
            ["a"],
            Subterm::Prim(Prim::bin_concat([
                Term::var(Var::free("x")),
                Term::var(Var::free("a")),
            ])),
        ),
    );

    let that = Term::func(
        ["y"],
        Term::func(
            ["b"],
            Subterm::Prim(Prim::bin_concat([
                Term::var(Var::free("y")),
                Term::var(Var::free("b")),
            ])),
        ),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_bin_slice_recurses_into_operands() {
    let mut context = context();

    let this = Term::func(
        ["x"],
        Term::func(
            ["a"],
            Term::func(
                ["p"],
                Subterm::Prim(Prim::bin_slice(
                    Term::var(Var::free("x")),
                    Term::var(Var::free("a")),
                    Term::var(Var::free("p")),
                )),
            ),
        ),
    );

    let that = Term::func(
        ["y"],
        Term::func(
            ["b"],
            Term::func(
                ["q"],
                Subterm::Prim(Prim::bin_slice(
                    Term::var(Var::free("y")),
                    Term::var(Var::free("b")),
                    Term::var(Var::free("q")),
                )),
            ),
        ),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_tuple_equal() {
    let mut context = context();

    let this = Term::tuple([Term::atom(Atom::from("x")), Term::atom(Atom::from("y"))]);
    let that = Term::tuple([Term::atom(Atom::from("x")), Term::atom(Atom::from("y"))]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_tuple_unequal_field() {
    let mut context = context();

    let this = Term::tuple([Term::atom(Atom::from("x")), Term::atom(Atom::from("y"))]);
    let that = Term::tuple([Term::atom(Atom::from("x")), Term::atom(Atom::from("z"))]);

    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

#[test]
fn convert_proj_same_index_and_head() {
    let mut context = context();

    let this = Term::proj(Term::var(Var::free("r")), 0);
    let that = Term::proj(Term::var(Var::free("r")), 0);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_proj_different_index_is_false() {
    let mut context = context();

    let this = Term::proj(Term::var(Var::free("r")), 0);
    let that = Term::proj(Term::var(Var::free("r")), 1);

    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

#[test]
fn convert_eta_tuple_neutral_with_known_type() {
    let mut context = context();

    let tuple_type: Term = Term::tuple_type([
        ("x", Term::atom_type(["a", "b"])),
        ("y", Term::atom_type(["c", "d"])),
    ]);

    let r: Term = Term::var(Var::free("r"));
    let s: Term = Term::var(Var::free("s"));

    assert_eq!(convert(&mut context, &tuple_type, &r, &r), Ok(true));

    assert_eq!(convert(&mut context, &tuple_type, &r, &s), Ok(false));
}

#[test]
fn convert_partial_projection_tuple_at_narrow_type() {
    let mut context = context();

    // p = (a, b), q = (a, c) — both 2-tuples agreeing on field 0, differing on field 1.
    context.define(
        "p",
        &Term::tuple([Term::atom(Atom::from("a")), Term::atom(Atom::from("b"))]),
    );
    context.define(
        "q",
        &Term::tuple([Term::atom(Atom::from("a")), Term::atom(Atom::from("c"))]),
    );

    // Term::type_() is a 1-field tuple type {A : {a}}.
    let type_: Term = Term::tuple_type([("x", Term::atom_type(["a"]))]);

    // this = (p.0), that = (q.0). At the 1-field type both denote (a),
    // so conversion should return true.
    let this: Term = Term::tuple([Term::proj(Term::var(Var::free("p")), 0)]);
    let that: Term = Term::tuple([Term::proj(Term::var(Var::free("q")), 0)]);

    // Even though eta_reduce_tuple widens each 1-tuple to its bare base
    // (`Var p`, `Var q`), the convert loop then routes the neutral pair
    // through `eta_expand_neutral`, which re-projects according to the
    // TRUE type telescope (1 field). Each `proj(_, 0)` then reduces to
    // `a`, so the comparison succeeds — the bug is masked here.
    assert_eq!(convert(&mut context, &type_, &this, &that), Ok(true));
}

#[test]
fn convert_times_out_on_pathological_inputs() {
    let mut context = context();

    context.define("loop", &Term::var(Var::free("loop")));

    let this = Term::tuple_type([
        (
            "x",
            Term::apply(
                Term::func(["z"], Term::var(Var::free("z"))),
                [Term::var(Var::free("loop"))],
            ),
        ),
        ("y", Term::var(Var::free("x"))),
    ]);

    let that = Term::tuple_type([
        ("x", Term::var(Var::free("loop"))),
        ("y", Term::var(Var::free("x"))),
    ]);

    assert_eq!(
        conv(&mut context, &this, &that),
        Err(ReduceError::Preempted)
    );
}
