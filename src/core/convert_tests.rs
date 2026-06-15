use {
    super::*,
    crate::core::{Atom, Inductive, Int, Nat, Prim, Structure, Var},
    std::{collections::BTreeMap, time::Duration},
};

fn context() -> Context {
    Context::new(Duration::from_millis(10))
}

fn conv(context: &mut Context, this: &Term, that: &Term) -> Result<bool, ReduceError> {
    convert(context, &Term::type_(), this, that)
}

fn nat(n: usize) -> Term {
    Term::prim(Prim::Nat(Nat::new(n)))
}

/// Build a lambda whose argument domains are irrelevant to conversion (which
/// compares only bodies); each parameter gets a placeholder `Type` domain.
fn func<const N: usize>(labels: [&str; N], body: impl Into<Term>) -> Term {
    Term::func(labels.map(|l| (l, Term::type_())), body.into())
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

    let this = func(["x"], Term::var(Var::free("x")));

    let that = func(["y"], Term::var(Var::free("y")));

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_union_match_compares_cases_and_motive() {
    let mut context = context();

    let make = |motive_label: &str, binder: &str| {
        Term::union_match(
            Term::var(Var::free("r")),
            Some(motive_label),
            Term::prim(Prim::NatType),
            [
                ("none", Vec::<&str>::new(), nat(0)),
                ("some", vec![binder], Term::var(Var::free(binder))),
            ],
        )
    };

    // Alpha-equivalent binders and motive labels are convertible.
    assert_eq!(
        conv(&mut context, &make("m", "x"), &make("n", "y")),
        Ok(true)
    );

    let different = Term::union_match(
        Term::var(Var::free("r")),
        Some("m"),
        Term::prim(Prim::NatType),
        [
            ("none", Vec::<&str>::new(), nat(1)),
            ("some", vec!["x"], Term::var(Var::free("x"))),
        ],
    );

    assert_eq!(conv(&mut context, &make("m", "x"), &different), Ok(false));
}

#[test]
fn convert_prim_recurses_into_operands() {
    let mut context = context();

    let this = func(
        ["x"],
        Subterm::Prim(Prim::int_add(
            Term::var(Var::free("x")),
            Subterm::Prim(Prim::Int(Int::new(1))),
        )),
    );

    let that = func(
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

    let this = func(
        ["x"],
        Subterm::Prim(Prim::int_add(
            Term::var(Var::free("x")),
            Subterm::Prim(Prim::Int(Int::new(1))),
        )),
    );

    let that = func(
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

    let this = func(
        ["x"],
        Subterm::Prim(Prim::nat_add(
            Term::var(Var::free("x")),
            Subterm::Prim(Prim::Nat(Nat::new(1usize))),
        )),
    );

    let that = func(
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

    let this = func(
        ["x"],
        Subterm::Prim(Prim::flt_neg(Term::var(Var::free("x")))),
    );

    let that = func(
        ["y"],
        Subterm::Prim(Prim::flt_neg(Term::var(Var::free("y")))),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_nat_to_int_recurses_into_operand() {
    let mut context = context();

    let this = func(
        ["x"],
        Subterm::Prim(Prim::nat_to_int(Term::var(Var::free("x")))),
    );

    let that = func(
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

    let this = func(
        ["x"],
        Subterm::Prim(Prim::bin_len(Term::var(Var::free("x")))),
    );
    let that = func(
        ["y"],
        Subterm::Prim(Prim::bin_len(Term::var(Var::free("y")))),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_bin_get_recurses_into_operands() {
    let mut context = context();

    let this = func(
        ["x"],
        func(
            ["a"],
            Subterm::Prim(Prim::bin_get(
                Term::var(Var::free("x")),
                Term::var(Var::free("a")),
            )),
        ),
    );

    let that = func(
        ["y"],
        func(
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

    let this = func(
        ["x"],
        func(
            ["a"],
            Subterm::Prim(Prim::bin_concat([
                Term::var(Var::free("x")),
                Term::var(Var::free("a")),
            ])),
        ),
    );

    let that = func(
        ["y"],
        func(
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

    let this = func(
        ["x"],
        func(
            ["a"],
            func(
                ["p"],
                Subterm::Prim(Prim::bin_slice(
                    Term::var(Var::free("x")),
                    Term::var(Var::free("a")),
                    Term::var(Var::free("p")),
                )),
            ),
        ),
    );

    let that = func(
        ["y"],
        func(
            ["b"],
            func(
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

    let this = Term::tuple([nat(1), nat(2)]);
    let that = Term::tuple([nat(1), nat(2)]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_tuple_unequal_field() {
    let mut context = context();

    let this = Term::tuple([nat(1), nat(2)]);
    let that = Term::tuple([nat(1), nat(3)]);

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
        ("x", Term::prim(Prim::NatType)),
        ("y", Term::prim(Prim::BlnType)),
    ]);

    let r: Term = Term::var(Var::free("r"));
    let s: Term = Term::var(Var::free("s"));

    assert_eq!(convert(&mut context, &tuple_type, &r, &r), Ok(true));

    assert_eq!(convert(&mut context, &tuple_type, &r, &s), Ok(false));
}

#[test]
fn convert_partial_projection_tuple_at_narrow_type() {
    let mut context = context();

    // p = (1, 2), q = (1, 3) — both 2-tuples agreeing on field 0, differing on field 1.
    context.define("p", &Term::tuple([nat(1), nat(2)]));
    context.define("q", &Term::tuple([nat(1), nat(3)]));

    // A 1-field tuple type {x : Nat}.
    let type_: Term = Term::tuple_type([("x", Term::prim(Prim::NatType))]);

    // this = (p.0), that = (q.0). At the 1-field type both denote (a),
    // so conversion should return true.
    let this: Term = Term::tuple([Term::proj(Term::var(Var::free("p")), 0)]);
    let that: Term = Term::tuple([Term::proj(Term::var(Var::free("q")), 0)]);

    // Even though eta_reduce_tuple widens each 1-tuple to its bare base
    // (`Var p`, `Var q`), the convert loop then routes the neutral pair
    // through `eta_expand_neutral`, which re-projects according to the
    // TRUE type telescope (1 field). Each `proj(_, 0)` then reduces to
    // `1`, so the comparison succeeds — the bug is masked here.
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
                func(["z"], Term::var(Var::free("z"))),
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

#[test]
fn convert_unit_typed_neutrals_in_type_argument() {
    let mut context = context();

    // F : (()) -> Type ; r, s : ()   (all neutral assumptions).
    // r ≡ s by η for the empty tuple (unit / proof irrelevance), so F r ≡ F s.
    // `conv` compares at `Type`, exactly as the pipeline does via `expect`.
    context.assume(
        "F",
        &Term::func_type([("_", Term::tuple_type_unit())], Term::type_()),
    );
    context.assume("r", &Term::tuple_type_unit());
    context.assume("s", &Term::tuple_type_unit());

    let f = Term::var(Var::free("F"));
    let r = Term::var(Var::free("r"));
    let s = Term::var(Var::free("s"));

    let this = Term::apply(f.clone(), [r]); // F r
    let that = Term::apply(f, [s]); // F s

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

// A struct's fields compare at their declared types, recovered from the
// registry — so a proof-irrelevant (unit-typed) field equates distinct
// neutrals, and two structs differing only there are convertible.
#[test]
fn convert_struct_unit_field_is_irrelevant() {
    let mut context = context();

    // struct Wrap { x : Nat, u : () }
    context.register_structure(
        "Wrap",
        Structure {
            params: Telescope::done(()),
            fields: Telescope::build(
                [
                    ("x", Term::prim(Prim::NatType)),
                    ("u", Term::tuple_type_unit()),
                ],
                (),
            ),
            module: String::new(),
            rep_public: true,
        },
    );

    context.assume("r", &Term::tuple_type_unit());
    context.assume("s", &Term::tuple_type_unit());

    let r = Term::var(Var::free("r"));
    let s = Term::var(Var::free("s"));

    // Wrap { 1, r } and Wrap { 1, s } differ only in the unit field's neutral.
    let this = Term::struct_("Wrap", Vec::<Term>::new(), [nat(1), r]);
    let that = Term::struct_("Wrap", Vec::<Term>::new(), [nat(1), s]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

// Likewise a variant's payload compares at its constructor's declared types,
// so a unit-typed payload field is proof-irrelevant.
#[test]
fn convert_variant_unit_payload_is_irrelevant() {
    let mut context = context();

    // union Wrap | wrap(x : Nat, u : ()) end
    context.register_inductive(
        "Wrap",
        Inductive {
            params: Telescope::done(()),
            indices: Telescope::done(()),
            constructors: BTreeMap::from([(
                Atom::from("wrap"),
                Telescope::build(
                    [
                        ("x", Term::prim(Prim::NatType)),
                        ("u", Term::tuple_type_unit()),
                    ],
                    Term::union_type("Wrap", Vec::<Term>::new(), Vec::<Term>::new()),
                ),
            )]),
        },
    );

    context.assume("r", &Term::tuple_type_unit());
    context.assume("s", &Term::tuple_type_unit());

    let r = Term::var(Var::free("r"));
    let s = Term::var(Var::free("s"));

    // wrap(1, r) and wrap(1, s) differ only in the unit payload's neutral.
    let this = Term::variant("Wrap", Vec::<Term>::new(), "wrap", [nat(1), r]);
    let that = Term::variant("Wrap", Vec::<Term>::new(), "wrap", [nat(1), s]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

// === Metavariables / unification ===========================================

#[test]
fn solve_flex_rigid_commits_solution() {
    let mut context = context();
    context.birth_metavar(0, Vec::new(), Term::type_());

    // ?0 ≟ Nat  (at type Type)
    let nat = Term::prim(Prim::NatType);
    assert_eq!(conv(&mut context, &Term::metavar(0), &nat), Ok(true));
    assert_eq!(context.metavar_solution(0), Some(&nat));
}

#[test]
fn solve_is_symmetric() {
    let mut context = context();
    context.birth_metavar(0, Vec::new(), Term::type_());

    let nat = Term::prim(Prim::NatType);
    // rigid on the left, flex on the right
    assert_eq!(conv(&mut context, &nat, &Term::metavar(0)), Ok(true));
    assert_eq!(context.metavar_solution(0), Some(&nat));
}

#[test]
fn occurs_check_rejects_cyclic_solution() {
    let mut context = context();
    context.birth_metavar(0, Vec::new(), Term::type_());

    // ?0 ≟ (x : ?0) -> Nat  — the candidate mentions ?0 itself.
    let cyclic = Term::func_type([("x", Term::metavar(0))], Term::prim(Prim::NatType));
    assert_eq!(conv(&mut context, &Term::metavar(0), &cyclic), Ok(false));
    assert_eq!(context.metavar_solution(0), None);
}

#[test]
fn scope_check_rejects_out_of_context_variable() {
    let mut context = context();
    // Birth with empty Γ: no variable is in scope for ?0.
    context.birth_metavar(0, Vec::new(), Term::type_());

    // ?0 ≟ x  — `x` is not available to ?0.
    let x = Term::var(Var::free("x"));
    assert_eq!(conv(&mut context, &Term::metavar(0), &x), Ok(false));
    assert_eq!(context.metavar_solution(0), None);
}

#[test]
fn scope_check_allows_in_context_variable() {
    let mut context = context();
    // Γ = (x : Type); result is Type, and the candidate `x` is in scope.
    context.assume("x", &Term::type_());
    context.birth_metavar(0, vec![("x".to_string(), Term::type_())], Term::type_());

    let x = Term::var(Var::free("x"));
    let occurrence = Term::metavar_birthed(0, None, vec![x.clone()]);
    assert_eq!(conv(&mut context, &occurrence, &x), Ok(true));
    assert_eq!(context.metavar_solution(0), Some(&x));
}

#[test]
fn revalidation_admits_checkable_but_not_inferable_candidate() {
    let mut context = context();
    // ?0 : (x : Nat, y : Nat) — a tuple type, born in empty Γ.
    let pair_type = Term::tuple_type([
        ("x", Term::prim(Prim::NatType)),
        ("y", Term::prim(Prim::NatType)),
    ]);
    context.birth_metavar(0, Vec::new(), pair_type);

    // ?0 ≟ (1, 2). A bare tuple has no synthesizable type (`elaborate_tuple`
    // is Check-only), so synthesize-then-convert re-validation rejected it;
    // checking it against the frozen tuple result type admits it.
    let pair = Term::tuple([nat(1), nat(2)]);
    assert_eq!(conv(&mut context, &Term::metavar(0), &pair), Ok(true));
    assert_eq!(context.metavar_solution(0), Some(&pair));
}

#[test]
fn revalidation_rejects_ill_typed_candidate_through_checking() {
    let mut context = context();
    // ?0 : (x : Nat, y : Nat).
    let pair_type = Term::tuple_type([
        ("x", Term::prim(Prim::NatType)),
        ("y", Term::prim(Prim::NatType)),
    ]);
    context.birth_metavar(0, Vec::new(), pair_type);

    // ?0 ≟ (1, 2, 3): a three-field tuple does not check against a two-field
    // tuple type, so checking still rejects the candidate and commits nothing.
    let wrong = Term::tuple([nat(1), nat(2), nat(3)]);
    assert_eq!(conv(&mut context, &Term::metavar(0), &wrong), Ok(false));
    assert_eq!(context.metavar_solution(0), None);
}

#[test]
fn flex_flex_equal_id_short_circuits() {
    let mut context = context();
    context.birth_metavar(0, Vec::new(), Term::type_());

    // ?0 ≟ ?0 is trivially true and leaves the metavariable unsolved.
    assert_eq!(
        conv(&mut context, &Term::metavar(0), &Term::metavar(0)),
        Ok(true)
    );
    assert_eq!(context.metavar_solution(0), None);
}

#[test]
fn flex_flex_distinct_is_residual() {
    let mut context = context();
    context.birth_metavar(0, Vec::new(), Term::type_());
    context.birth_metavar(1, Vec::new(), Term::type_());

    // ?0 ≟ ?1 postpones with no way to progress — a residual constraint.
    assert_eq!(
        conv(&mut context, &Term::metavar(0), &Term::metavar(1)),
        Ok(false)
    );
}

#[test]
fn embedded_metavar_postpones_to_residual() {
    let mut context = context();
    context.birth_metavar(0, Vec::new(), Term::type_());
    context.birth_metavar(1, Vec::new(), Term::type_());

    // ?0 ≟ (x : ?1) -> Nat — ?1 is an unsolved embedded metavariable, so the
    // solve is postponed; nothing solves ?1, so it stays residual.
    let candidate = Term::func_type([("x", Term::metavar(1))], Term::prim(Prim::NatType));
    assert_eq!(conv(&mut context, &Term::metavar(0), &candidate), Ok(false));
    assert_eq!(context.metavar_solution(0), None);
}

#[test]
fn revalidation_rejects_ill_typed_solution() {
    let mut context = context();
    // ?0 : Nat under empty Γ. A candidate of type Type (e.g. `Bln`) does not
    // type-check against Nat, so re-validation rejects it.
    context.birth_metavar(0, Vec::new(), Term::prim(Prim::NatType));

    let bln = Term::prim(Prim::BlnType);
    assert_eq!(conv(&mut context, &Term::metavar(0), &bln), Ok(false));
    assert_eq!(context.metavar_solution(0), None);
}

#[test]
fn revalidation_suppresses_refinements_rejecting_a_refined_solution() {
    // The §12 regression. Γ = (t : Type) with a counterfactual match-arm
    // refinement `t := Nat` in force (as inside `bln_match b { true => ... }`,
    // where the family `T(b) ⇝ Nat`). `?0 : t` is born under the *frozen*
    // Γ = (t : Type) — its result type depends on the refined head, mirroring
    // `m : T(b)`.
    let mut context = context();
    context.assume("t", &Term::type_());
    context.refine("t", &Term::prim(Prim::NatType));
    context.birth_metavar(
        0,
        vec![("t".to_string(), Term::type_())],
        Term::var(Var::free("t")),
    );

    // `?0 ≟ 5` at type `t`. Locally (refinement on) `t ⇝ Nat` and `5 : t` holds,
    // but re-validation suppresses refinements, leaving `t` abstract, so `5 : t`
    // fails and the solution is rejected — the program is unsound otherwise.
    let t = Term::var(Var::free("t"));
    let occurrence = Term::metavar_birthed(0, None, vec![t.clone()]);
    let five = Term::prim(Prim::Nat(Nat::new(5usize)));
    assert_eq!(convert(&mut context, &t, &occurrence, &five), Ok(false));
    assert_eq!(context.metavar_solution(0), None);
}

#[test]
fn revalidation_accepts_a_refinement_independent_solution() {
    // The §12 twin. The same refinement `t := Nat` is in force, but `?0`'s result
    // type is `Nat` directly — it does not depend on the refined head. Re-validation
    // checks `5 : Nat` with refinements suppressed (none are needed) and commits.
    let mut context = context();
    context.assume("t", &Term::type_());
    context.refine("t", &Term::prim(Prim::NatType));
    context.birth_metavar(
        0,
        vec![("t".to_string(), Term::type_())],
        Term::prim(Prim::NatType),
    );

    let nat = Term::prim(Prim::NatType);
    let occurrence = Term::metavar_birthed(0, None, vec![Term::var(Var::free("t"))]);
    let five = Term::prim(Prim::Nat(Nat::new(5usize)));
    assert_eq!(convert(&mut context, &nat, &occurrence, &five), Ok(true));
    assert_eq!(context.metavar_solution(0), Some(&five));
}

// === Spine inversion (contextual metavariables) =============================

fn nat_type() -> Term {
    Term::prim(Prim::NatType)
}

#[test]
fn solve_inverts_a_renaming() {
    let mut context = context();
    // ?0 born under Γ = [a : Nat]; this occurrence's spine maps `a` to the
    // live name `y` (the enclosing binders were re-closed and reopened).
    context.birth_metavar(0, vec![("a".into(), nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(0, None, vec![Term::var(Var::free("y"))]);

    // ?0[y] ≟ y — inverting the renaming stores the solution in birth-named
    // form: `a`, not `y`.
    assert_eq!(
        conv(&mut context, &occurrence, &Term::var(Var::free("y"))),
        Ok(true)
    );
    assert_eq!(
        context.metavar_solution(0),
        Some(&Term::var(Var::free("a")))
    );
}

#[test]
fn solve_through_an_identity_spine_matches_legacy() {
    let mut context = context();
    context.birth_metavar(0, vec![("a".into(), nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(0, None, vec![Term::var(Var::free("a"))]);

    // The identity spine behaves exactly like the empty (legacy bare-hole)
    // spine: the candidate is stored unchanged.
    assert_eq!(conv(&mut context, &occurrence, &nat(1)), Ok(true));
    assert_eq!(context.metavar_solution(0), Some(&nat(1)));
}

#[test]
fn solve_postpones_a_duplicated_renaming() {
    let mut context = context();
    context.birth_metavar(
        0,
        vec![("a".into(), nat_type()), ("b".into(), nat_type())],
        nat_type(),
    );
    // Both entries are the same live name: which birth binder `y` stands for
    // is ambiguous, so a candidate mentioning it is undecided, not unequal.
    let occurrence = Term::metavar_birthed(
        0,
        None,
        vec![Term::var(Var::free("y")), Term::var(Var::free("y"))],
    );

    let outcome = convert_outcome(
        &mut context,
        &Term::type_(),
        &occurrence,
        &Term::var(Var::free("y")),
    );
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(0), None);
}

#[test]
fn solve_prunes_dependence_on_a_non_pattern_entry() {
    let mut context = context();
    context.birth_metavar(
        0,
        vec![("a".into(), nat_type()), ("b".into(), nat_type())],
        nat_type(),
    );
    // First slot a pattern variable, second a compound term: the candidate
    // may depend on the first but not (yet) on the second.
    let compound: Term = Subterm::Prim(Prim::nat_add(Term::var(Var::free("z")), nat(1))).into();
    let occurrence =
        Term::metavar_birthed(0, None, vec![Term::var(Var::free("y")), compound.clone()]);

    // ?0[y, z+1] ≟ y — solvable through the pattern slot alone.
    assert_eq!(
        conv(&mut context, &occurrence, &Term::var(Var::free("y"))),
        Ok(true)
    );
    assert_eq!(
        context.metavar_solution(0),
        Some(&Term::var(Var::free("a")))
    );
}

#[test]
fn solve_postpones_a_candidate_reaching_through_a_non_pattern_entry() {
    let mut context = context();
    context.birth_metavar(
        0,
        vec![("a".into(), nat_type()), ("b".into(), nat_type())],
        nat_type(),
    );
    let compound: Term = Subterm::Prim(Prim::nat_add(Term::var(Var::free("z")), nat(1))).into();
    let occurrence = Term::metavar_birthed(0, None, vec![Term::var(Var::free("y")), compound]);

    // ?0[y, z+1] ≟ z — `z` is reachable only through the non-pattern slot
    // (and is not an occurrence of the whole entry): undecided.
    let outcome = convert_outcome(
        &mut context,
        &Term::type_(),
        &occurrence,
        &Term::var(Var::free("z")),
    );
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(0), None);
}

#[test]
fn solve_rejects_an_out_of_image_variable() {
    let mut context = context();
    context.birth_metavar(0, vec![("a".into(), nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(0, None, vec![Term::var(Var::free("y"))]);

    // ?0[y] ≟ z — `z` corresponds to no birth binder and never can: a hard
    // mismatch, not a postponement.
    let outcome = convert_outcome(
        &mut context,
        &Term::type_(),
        &occurrence,
        &Term::var(Var::free("z")),
    );
    assert!(matches!(outcome, Ok(Outcome::Mismatch)));
    assert_eq!(context.metavar_solution(0), None);
}

#[test]
fn solve_classifies_a_solved_metavariable_spine_entry_by_its_value() {
    let mut context = context();
    // ?0 is already solved to its own binder, so an occurrence ?0[y] stands
    // for `y` — a perfectly good pattern variable hiding behind a node.
    context.birth_metavar(0, vec![("a".into(), nat_type())], nat_type());
    context.solve_metavar(0, Term::var(Var::free("a")));
    let entry = Term::metavar_birthed(0, None, vec![Term::var(Var::free("y"))]);

    context.birth_metavar(1, vec![("b".into(), nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(1, None, vec![entry]);

    // ?1[?0[y]] ≟ y — the entry resolves to `y` and inverts to `b`.
    assert_eq!(
        conv(&mut context, &occurrence, &Term::var(Var::free("y"))),
        Ok(true)
    );
    assert_eq!(
        context.metavar_solution(1),
        Some(&Term::var(Var::free("b")))
    );
}

#[test]
fn solve_abstracts_a_non_pattern_occurrence() {
    let mut context = context();
    context.birth_metavar(
        0,
        vec![("a".into(), nat_type()), ("b".into(), nat_type())],
        nat_type(),
    );
    // A reduce-stable compound (a tuple is a normal form), matched by the
    // raw spelling; the reduced-spelling case is the next test.
    let compound = Term::tuple([Term::var(Var::free("z"))]);
    let occurrence =
        Term::metavar_birthed(0, None, vec![Term::var(Var::free("y")), compound.clone()]);

    // ?0[y, (z,)] ≟ (z,) — the candidate *is* an occurrence of the
    // non-pattern entry, which abstracts to its birth binder `b`.
    assert_eq!(conv(&mut context, &occurrence, &compound), Ok(true));
    assert_eq!(
        context.metavar_solution(0),
        Some(&Term::var(Var::free("b")))
    );
}

// === Parked-constraint retries ==============================================

#[test]
fn parked_goals_retry_under_their_frozen_refinements() {
    let mut context = context();

    // Park (inside an arm-like frame) a goal that converts only through the
    // frame's counterfactual refinement: `b` reduces to `Nat` via `refine`,
    // not via any definition.
    context.with_frame(|context| {
        context.assume("b", &Term::type_());
        context.refine("b", &nat_type());
        context.park(
            ParkedWork::Conversion(Goal {
                type_: Term::type_(),
                this: Term::var(Var::free("b")),
                that: nat_type(),
            }),
            Term::var(Var::free("b")),
        );
    });

    // The frame is gone; the drain retries under the frozen one, where the
    // refinement still holds and the goal converts.
    assert!(drain_parked(&mut context).is_ok());
}

#[test]
fn parked_goals_without_their_refinement_mismatch() {
    let mut context = context();

    // Control: the same goal parked without the refinement cannot convert,
    // and the drain reports it at its origin.
    context.with_frame(|context| {
        context.assume("b", &Term::type_());
        context.park(
            ParkedWork::Conversion(Goal {
                type_: Term::type_(),
                this: Term::var(Var::free("b")),
                that: nat_type(),
            }),
            Term::var(Var::free("b")),
        );
    });

    assert!(drain_parked(&mut context).is_err());
}

#[test]
fn solve_abstracts_a_reduced_spelling_occurrence() {
    let mut context = context();
    context.birth_metavar(
        0,
        vec![("a".into(), nat_type()), ("b".into(), nat_type())],
        nat_type(),
    );
    // `z + 1` successor-peels under reduction, and the candidate side arrives
    // reduced — each subject contributes both spellings, so the occurrence
    // still abstracts, and the round-trip verification accepts the pair by
    // definitional (not syntactic) equality.
    let compound: Term = Subterm::Prim(Prim::nat_add(Term::var(Var::free("z")), nat(1))).into();
    let occurrence =
        Term::metavar_birthed(0, None, vec![Term::var(Var::free("y")), compound.clone()]);

    assert_eq!(conv(&mut context, &occurrence, &compound), Ok(true));
    assert_eq!(
        context.metavar_solution(0),
        Some(&Term::var(Var::free("b")))
    );
}

#[test]
fn flex_flex_same_id_converts_through_equal_spines() {
    let mut context = context();
    context.birth_metavar(0, vec![("a".into(), nat_type())], nat_type());

    // Two occurrences of the same unsolved metavariable whose spines differ
    // syntactically but agree definitionally (`1 + 1` reduces to `2`): the
    // congruence probe discharges the goal without solving anything.
    let sum: Term = Subterm::Prim(Prim::nat_add(nat(1), nat(1))).into();
    let this = Term::metavar_birthed(0, None, vec![sum]);
    let that = Term::metavar_birthed(0, None, vec![nat(2)]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
    assert_eq!(context.metavar_solution(0), None);
}

#[test]
fn flex_flex_same_id_with_disagreeing_spines_stays_blocked() {
    let mut context = context();
    context.birth_metavar(0, vec![("a".into(), nat_type())], nat_type());

    // Disagreeing spines are not *unequal* — the solution may ignore the
    // slot — so the pair parks rather than mismatching.
    let this = Term::metavar_birthed(0, None, vec![nat(1)]);
    let that = Term::metavar_birthed(0, None, vec![nat(2)]);

    let outcome = convert_outcome(&mut context, &Term::type_(), &this, &that);
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
}

#[test]
fn flex_flex_distinct_heads_with_a_common_solution_stays_blocked() {
    // The intersection wontfix's witness, pinned: two *distinct* unsolved
    // metavariables over compatible telescopes, met through the same live
    // name. Flex–flex assignment (`?0 := ?1` through the renaming) would
    // discharge this; v1 does no intersection, so the pair parks and — with
    // nothing else to pin either head — stays undecided. When intersection
    // is built, this test should flip to `Converts` with `?0` solved to an
    // occurrence of `?1` (and this comment retired).
    let mut context = context();
    context.birth_metavar(0, vec![("a".into(), nat_type())], nat_type());
    context.birth_metavar(1, vec![("b".into(), nat_type())], nat_type());

    let this = Term::metavar_birthed(0, None, vec![Term::var(Var::free("x"))]);
    let that = Term::metavar_birthed(1, None, vec![Term::var(Var::free("x"))]);

    let outcome = convert_outcome(&mut context, &Term::type_(), &this, &that);
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(0), None);
    assert_eq!(context.metavar_solution(1), None);
}

#[test]
fn rollback_solutions_unwinds_to_the_mark() {
    let mut context = context();
    context.birth_metavar(0, vec![("a".into(), nat_type())], nat_type());
    context.birth_metavar(1, vec![("a".into(), nat_type())], nat_type());

    context.solve_metavar(0, nat(1));
    let mark = context.solution_mark();
    context.solve_metavar(1, nat(2));

    context.rollback_solutions(mark);

    // The solution past the mark is unwound; the one before it survives. This
    // is the bracket `solve` wraps around re-validation, so a rejected
    // candidate's nested solves leave no fingerprints.
    assert_eq!(context.metavar_solution(0), Some(&nat(1)));
    assert_eq!(context.metavar_solution(1), None);
}

#[test]
fn stuck_prim_on_a_metavar_parks_instead_of_mismatching() {
    let mut context = context();
    context.birth_metavar(0, vec![("a".into(), nat_type())], nat_type());
    let m = Term::metavar_birthed(0, None, vec![Term::var(Var::free("a"))]);
    let stuck: Term = Subterm::Prim(Prim::NatSub(m.clone(), nat(1))).into();

    // `?0 - 1 ≈ 0` is undecided, not unequal: solving `?0` may fold the
    // subtraction. (`NatAdd` escapes via successor peeling; the other
    // operators rely on this parking.)
    let outcome = convert_outcome(&mut context, &Term::type_(), &stuck, &nat(0));
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(0), None);

    // Within one run, a sibling goal pins `?0 := 1`; the parked subtraction
    // is retried, folds to `0`, and converts.
    let this = Term::tuple([stuck, m]);
    let that = Term::tuple([nat(0), nat(1)]);
    assert_eq!(conv(&mut context, &this, &that), Ok(true));
    assert_eq!(context.metavar_solution(0), Some(&nat(1)));
}

#[test]
fn rigid_head_mismatch_with_a_metavar_inside_still_fails_fast() {
    let mut context = context();
    context.birth_metavar(0, vec![("a".into(), nat_type())], nat_type());
    let m = Term::metavar_birthed(0, None, vec![Term::var(Var::free("a"))]);

    // A union type against `Nat` is provably unequal whatever `?0` becomes —
    // the heads are rigid — so the mismatch stays hard (and is reported at
    // the use site, not deferred to the drain).
    let union = Term::union_type("Vec", [m], Vec::<Term>::new());
    let outcome = convert_outcome(&mut context, &Term::type_(), &union, &nat_type());
    assert!(matches!(outcome, Ok(Outcome::Mismatch)));
}

#[test]
fn arm_refinement_does_not_taint_a_committed_solution() {
    let mut context = context();
    context.assume("n", &nat_type());
    context.birth_metavar(0, vec![("n".into(), nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(0, None, vec![Term::var(Var::free("n"))]);

    // Inside a frame that counterfactually refines `n := 0` (a match arm),
    // the goal `?0[n] ≈ n` still discharges — but the *committed* solution is
    // the refinement-free `n`, not the arm-local `0`: a metavariable must not
    // be pinned to a value that holds only counterfactually inside the arm.
    let converts = context.with_frame(|context| {
        context.refine("n", &nat(0));
        conv(context, &occurrence, &Term::var(Var::free("n")))
    });
    assert_eq!(converts, Ok(true));
    assert_eq!(
        context.metavar_solution(0),
        Some(&Term::var(Var::free("n")))
    );
}

#[test]
fn eta_at_unit_trusts_the_goal_type_label() {
    let mut context = context();

    // Pinned wart, internal to the conversion API: when one side is the unit
    // tuple literal `()`, `eta_expand_tuple` enqueues one goal per field —
    // zero — and succeeds *without ever confirming the goal's type reduces to
    // `{}`. So the kernel, asked directly, judges `() ≈ 1` at type `Nat`.
    // Elaboration never produces a heterotyped goal (both sides of every
    // `expect`/index comparison were checked at the same type), so this is
    // not reachable from the surface language — but the conversion entry
    // point is only sound under that caller invariant. If η-at-unit ever
    // gates on the type actually being a 0-ary tuple type, flip this to
    // `Ok(false)`.
    assert_eq!(
        convert(&mut context, &nat_type(), &Term::tuple_unit(), &nat(1)),
        Ok(true)
    );
}
