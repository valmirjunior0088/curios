use {
    crate::*,
    curios_base::{Grain, Int, PackedBin, Plicity, Qualifier, RootId},
    std::{collections::BTreeMap, time::Duration},
};

fn context() -> Context {
    Context::new(Duration::from_millis(10))
}

fn conv(context: &mut Context, this: &Term, that: &Term) -> Result<bool, ReduceError> {
    convert(context, &Term::type_ground(), this, that)
}

fn nat(n: usize) -> Term {
    Term::prim(Prim::Nat(Nat::new(n)))
}

#[test]
fn value_conversion_does_not_unfold_terms_differing_only_by_universes() {
    let mut context = context();
    context.define(
        "partial",
        &Term::func(
            [("ignored", Term::prim(Prim::NatType))],
            Term::prim(Prim::bin_get(
                Grain::X,
                Term::prim(Prim::Bin(Grain::X, PackedBin::from_bytes(Vec::<u8>::new()))),
                nat(0),
            )),
        ),
        None,
    );
    let applied = |universe| {
        Term::apply(
            Term::universe_inst(
                Term::free_var("partial"),
                vec![Level::meta(UniverseMetaId(universe))],
            ),
            [nat(0)],
        )
    };

    assert_eq!(
        convert(
            &mut context,
            &Term::prim(Prim::ByteType),
            &applied(0),
            &applied(1),
        ),
        Ok(true)
    );
}

/// Build a lambda whose argument domains are irrelevant to conversion (which
/// compares only bodies); each parameter gets a placeholder `Type` domain.
fn func<const N: usize>(labels: [&str; N], body: impl Into<Term>) -> Term {
    Term::func(labels.map(|l| (l, Term::type_ground())), body.into())
}

#[test]
fn convert_func_type_is_alpha_equivalent() {
    let mut context = context();

    let this = Term::func_type([("x", Term::type_ground())], Term::free_var("x"));

    let that = Term::func_type([("y", Term::type_ground())], Term::free_var("y"));

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_func_is_alpha_equivalent() {
    let mut context = context();

    let this = func(["x"], Term::free_var("x"));

    let that = func(["y"], Term::free_var("y"));

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_func_type_distinguishes_plicity() {
    let mut context = context();

    // Three telescopes with identical domains and results, differing only in the
    // one binder's plicity.
    let explicit = Term::func_type([("x", Term::type_ground())], Term::type_ground());
    let implicit = Term::func_type_marked(
        [(Plicity::Implicit, "x", Term::type_ground())],
        Term::type_ground(),
    );
    let witness = Term::func_type_marked(
        [(Plicity::Witness, "x", Term::type_ground())],
        Term::type_ground(),
    );

    // Plicity is part of function-type identity: every pairwise mix is
    // non-convertible even though the dependent telescopes agree.
    assert_eq!(conv(&mut context, &explicit, &implicit), Ok(false));
    assert_eq!(conv(&mut context, &explicit, &witness), Ok(false));
    assert_eq!(conv(&mut context, &implicit, &witness), Ok(false));

    // Same plicity, alpha-renamed binder: still convertible.
    let implicit_y = Term::func_type_marked(
        [(Plicity::Implicit, "y", Term::type_ground())],
        Term::type_ground(),
    );
    assert_eq!(conv(&mut context, &implicit, &implicit_y), Ok(true));
}

#[test]
fn convert_inductive_match_compares_cases_and_motive() {
    let mut context = context();

    let make = |motive_label: &str, binder: &str| {
        Term::induct_match(
            Term::free_var("r"),
            Some(motive_label),
            Term::prim(Prim::NatType),
            [
                ("none", Vec::<&str>::new(), nat(0)),
                ("some", vec![binder], Term::free_var(binder)),
            ],
        )
    };

    // Alpha-equivalent binders and motive labels are convertible.
    assert_eq!(
        conv(&mut context, &make("m", "x"), &make("n", "y")),
        Ok(true)
    );

    let different = Term::induct_match(
        Term::free_var("r"),
        Some("m"),
        Term::prim(Prim::NatType),
        [
            ("none", Vec::<&str>::new(), nat(1)),
            ("some", vec!["x"], Term::free_var("x")),
        ],
    );

    assert_eq!(conv(&mut context, &make("m", "x"), &different), Ok(false));
}

#[test]
fn convert_inductive_match_compares_default() {
    let mut context = context();

    let with_default = |d: usize| {
        Term::induct_match_default(
            Term::free_var("r"),
            Some("m"),
            Term::prim(Prim::NatType),
            [("none", Vec::<&str>::new(), nat(0))],
            nat(d),
        )
    };

    // Same enumerated arm and same default: convertible.
    assert_eq!(
        conv(&mut context, &with_default(9), &with_default(9)),
        Ok(true)
    );

    // Same arm, different default body: not convertible — the default is a real
    // arm, not erased provenance.
    assert_eq!(
        conv(&mut context, &with_default(9), &with_default(8)),
        Ok(false)
    );

    // A defaulted match never converts with an otherwise-identical bare one:
    // presence of the catch-all is itself a difference.
    let bare = Term::induct_match(
        Term::free_var("r"),
        Some("m"),
        Term::prim(Prim::NatType),
        [("none", Vec::<&str>::new(), nat(0))],
    );
    assert_eq!(conv(&mut context, &with_default(9), &bare), Ok(false));
}

#[test]
fn convert_prim_recurses_into_operands() {
    let mut context = context();

    let this = func(
        ["x"],
        Subterm::Prim(Prim::int_add(
            Term::free_var("x"),
            Subterm::Prim(Prim::Int(Int::new(1))),
        )),
    );

    let that = func(
        ["y"],
        Subterm::Prim(Prim::int_add(
            Term::free_var("y"),
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
            Term::free_var("x"),
            Subterm::Prim(Prim::Int(Int::new(1))),
        )),
    );

    let that = func(
        ["x"],
        Subterm::Prim(Prim::int_sub(
            Term::free_var("x"),
            Subterm::Prim(Prim::Int(Int::new(1))),
        )),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

// === Folded recursive calls (match-guarded delta) ===========================

/// `λx. match x | none() => <none_value> | some(p) => head(p) end` — stuck at
/// a neutral scrutinee, with a `head`-call in an arm to make unfolding
/// self-feeding.
fn recursive_matcher(head: &str, none_value: usize) -> Term {
    Term::func(
        [("x", Term::prim(Prim::NatType))],
        Term::induct_match(
            Term::free_var("x"),
            Some("m"),
            Term::prim(Prim::NatType),
            [
                ("none", Vec::<&str>::new(), nat(none_value)),
                (
                    "some",
                    vec!["p"],
                    Term::apply(Term::free_var(head), [Term::free_var("p")]),
                ),
            ],
        ),
    )
}

#[test]
fn convert_folded_recursive_call_against_its_unfolding() {
    let mut context = context();
    context.define("f", &recursive_matcher("f", 0), None);

    let folded = Term::apply(Term::free_var("f"), [Term::free_var("a")]);

    // The literal stuck body, spelled reducibly (the η-redex around `p`) so
    // the sides are not syntactically equal: the folded call meets a
    // non-apply shape, lazy delta unfolds it once, and the two stuck
    // matches compare structurally.
    let unfolded = Term::induct_match(
        Term::free_var("a"),
        Some("m"),
        Term::prim(Prim::NatType),
        [
            ("none", Vec::<&str>::new(), nat(0)),
            (
                "some",
                vec!["p"],
                Term::apply(
                    Term::free_var("f"),
                    [Term::apply(
                        func(["z"], Term::free_var("z")),
                        [Term::free_var("p")],
                    )],
                ),
            ),
        ],
    );

    assert_eq!(conv(&mut context, &folded, &unfolded), Ok(true));
}

#[test]
fn convert_same_recursive_head_compares_spines() {
    let mut context = context();
    context.define("f", &recursive_matcher("f", 0), None);

    // Convertible (but not syntactically equal) spines: true.
    let this = Term::apply(Term::free_var("f"), [Term::free_var("a")]);
    let that = Term::apply(
        Term::free_var("f"),
        [Term::apply(
            func(["z"], Term::free_var("z")),
            [Term::free_var("a")],
        )],
    );
    assert_eq!(conv(&mut context, &this, &that), Ok(true));

    // Mismatching spines: committal false, no unfolding retry.
    let other = Term::apply(Term::free_var("f"), [Term::free_var("b")]);
    assert_eq!(conv(&mut context, &this, &other), Ok(false));
}

fn structural_rec_member(body: Term) -> Term {
    let nat_type = Term::prim(Prim::NatType);
    let rec = Term::rec(
        [(
            "f",
            Term::func_type([("x", nat_type.clone())], nat_type),
            Term::func([("x", Term::prim(Prim::NatType))], body),
        )],
        Term::free_var("f"),
    );
    let Subterm::Rec(rec) = Term::unwrap_or_clone(rec) else {
        unreachable!()
    };
    Term::rec_member(rec.group, 0)
}

#[test]
fn same_structural_recursive_head_does_not_assume_injective_spines() {
    let mut context = context();
    let constant = structural_rec_member(nat(0));
    let this = Term::apply(constant.clone(), [Term::free_var("a")]);
    let that = Term::apply(constant, [Term::free_var("b")]);
    assert_eq!(conv(&mut context, &this, &that), Ok(true));

    let identity = structural_rec_member(Term::free_var("x"));
    let this = Term::apply(identity.clone(), [Term::free_var("a")]);
    let that = Term::apply(identity, [Term::free_var("b")]);
    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

#[test]
fn convert_distinct_recursive_heads_with_identical_bodies_converge_coinductively() {
    let mut context = context();
    // Identical bodies, distinct names: each round unfolds both sides to the
    // same stuck match and re-opens its `some` arm at a fresh binder — the
    // recurrence differs from the previous round only in that opening
    // entropy, so the canonicalized history recognizes the cycle and assumes
    // it. No finite disagreement exists: the functions are bisimilar. Before
    // goal canonicalization this pair spun to `Err(Preempted)`.
    context.define("f", &recursive_matcher("f", 0), None);
    context.define("g", &recursive_matcher("g", 0), None);

    let this = Term::apply(Term::free_var("f"), [Term::free_var("a")]);
    let that = Term::apply(Term::free_var("g"), [Term::free_var("a")]);
    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_distinct_recursive_heads_with_differing_bodies_is_false() {
    let mut context = context();
    // The `none` arms disagree: the first unfolding round surfaces the
    // finite disagreement on a sibling goal, well before any deadline.
    context.define("f", &recursive_matcher("f", 0), None);
    context.define("g", &recursive_matcher("g", 1), None);

    let this = Term::apply(Term::free_var("f"), [Term::free_var("a")]);
    let that = Term::apply(Term::free_var("g"), [Term::free_var("a")]);
    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

#[test]
fn convert_growing_recursive_unfolding_spends_the_deadline() {
    let mut context = context();
    // `λx. match x | none() => head(s(x)) | some(p) => 0 end` never recurs —
    // every unfolding round's arm goal is structurally new, one more `s` on
    // the folded argument — so no cycle exists to detect and the comparison
    // rightly spends the deadline: the accepted cost of fully general
    // recursion. (The growth rides the match arm so each round refolds and
    // returns to the drain queue; bare `f = λx. f(s(x))` growth would nest
    // inside one `reduce` call instead.)
    let growing = |head: &str| {
        Term::func(
            [("x", Term::prim(Prim::NatType))],
            Term::induct_match(
                Term::free_var("x"),
                Some("m"),
                Term::prim(Prim::NatType),
                [
                    (
                        "none",
                        Vec::<&str>::new(),
                        Term::apply(
                            Term::free_var(head),
                            [Term::apply(Term::free_var("s"), [Term::free_var("x")])],
                        ),
                    ),
                    ("some", vec!["p"], nat(0)),
                ],
            ),
        )
    };
    context.define("f", &growing("f"), None);
    context.define("g", &growing("g"), None);

    let this = Term::apply(Term::free_var("f"), [Term::free_var("a")]);
    let that = Term::apply(Term::free_var("g"), [Term::free_var("a")]);
    assert_eq!(
        conv(&mut context, &this, &that),
        Err(ReduceError::Preempted)
    );
}

#[test]
fn convert_recursive_values_are_bisimilar() {
    let mut context = context();
    // Two distinct recursive value definitions unfolding to the same
    // constructor shape: the payload goal recurs exactly (no openings
    // involved), history cuts it, and the streams are equal coinductively.
    let stream = |name: &str| {
        Term::variant(
            "E",
            Vec::<Term>::new(),
            "cons",
            [nat(1), Term::free_var(name)],
        )
    };
    context.define("xs", &stream("xs"), None);
    context.define("ys", &stream("ys"), None);

    assert_eq!(
        conv(&mut context, &Term::free_var("xs"), &Term::free_var("ys")),
        Ok(true)
    );
}

#[test]
fn convert_folded_recursive_call_against_neutral_head_is_false() {
    let mut context = context();
    context.define("f", &recursive_matcher("f", 0), None);

    // The recursive side unfolds to its stuck match, the neutral side
    // cannot unfold at all: a structural mismatch, decided well within the
    // deadline.
    let this = Term::apply(Term::free_var("f"), [Term::free_var("a")]);
    let neutral = Term::apply(Term::free_var("h"), [Term::free_var("a")]);
    assert_eq!(conv(&mut context, &this, &neutral), Ok(false));
}

// === `Rec` (local groups, still a term-level construct — no lambda-lifting
// in this design) ============================================================

#[test]
fn convert_rec_is_alpha_equivalent() {
    let mut context = context();

    let this = Term::rec(
        vec![("x", Term::type_ground(), Term::free_var("x"))],
        Term::free_var("x"),
    );

    let that = Term::rec(
        vec![("y", Term::type_ground(), Term::free_var("y"))],
        Term::free_var("y"),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_nat_add_recurses_into_operands() {
    let mut context = context();

    let this = func(
        ["x"],
        Subterm::Prim(Prim::nat_add(
            Term::free_var("x"),
            Subterm::Prim(Prim::Nat(Nat::new(1usize))),
        )),
    );

    let that = func(
        ["y"],
        Subterm::Prim(Prim::nat_add(
            Term::free_var("y"),
            Subterm::Prim(Prim::Nat(Nat::new(1usize))),
        )),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_flt_neg_recurses_into_operand() {
    let mut context = context();

    let this = func(["x"], Subterm::Prim(Prim::flt_neg(Term::free_var("x"))));

    let that = func(["y"], Subterm::Prim(Prim::flt_neg(Term::free_var("y"))));

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_nat_to_int_recurses_into_operand() {
    let mut context = context();

    let this = func(["x"], Subterm::Prim(Prim::nat_to_int(Term::free_var("x"))));

    let that = func(["y"], Subterm::Prim(Prim::nat_to_int(Term::free_var("y"))));

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_lst_compares_element_wise() {
    let mut context = context();

    let this = Subterm::Prim(Prim::lst(vec![
        Subterm::Prim(Prim::Nat(Nat::new(1usize))),
        Subterm::Prim(Prim::Nat(Nat::new(2usize))),
    ]))
    .into();

    let that = Subterm::Prim(Prim::lst(vec![
        Subterm::Prim(Prim::Nat(Nat::new(1usize))),
        Subterm::Prim(Prim::Nat(Nat::new(2usize))),
    ]))
    .into();

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_lst_rejects_different_lengths() {
    let mut context = context();

    let this = Subterm::Prim(Prim::lst(vec![Subterm::Prim(Prim::Nat(Nat::new(1usize)))])).into();

    let that = Subterm::Prim(Prim::lst(vec![
        Subterm::Prim(Prim::Nat(Nat::new(1usize))),
        Subterm::Prim(Prim::Nat(Nat::new(2usize))),
    ]))
    .into();

    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

#[test]
fn convert_prim_bin_type_is_equal_to_itself() {
    let mut context = context();

    let this = Subterm::Prim(Prim::BinType(Grain::X)).into();
    let that = Subterm::Prim(Prim::BinType(Grain::X)).into();

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_prim_bin_literal_compares_bytes() {
    let mut context = context();

    assert_eq!(
        conv(
            &mut context,
            &Subterm::Prim(Prim::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2]))).into(),
            &Subterm::Prim(Prim::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2]))).into(),
        ),
        Ok(true)
    );

    assert_eq!(
        conv(
            &mut context,
            &Subterm::Prim(Prim::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2]))).into(),
            &Subterm::Prim(Prim::Bin(Grain::X, PackedBin::from_bytes(vec![1, 3]))).into(),
        ),
        Ok(false)
    );
}

#[test]
fn convert_prim_bin_len_recurses_into_operand() {
    let mut context = context();

    let this = func(
        ["x"],
        Subterm::Prim(Prim::bin_len(Grain::X, Term::free_var("x"))),
    );
    let that = func(
        ["y"],
        Subterm::Prim(Prim::bin_len(Grain::X, Term::free_var("y"))),
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
                Grain::X,
                Term::free_var("x"),
                Term::free_var("a"),
            )),
        ),
    );

    let that = func(
        ["y"],
        func(
            ["b"],
            Subterm::Prim(Prim::bin_get(
                Grain::X,
                Term::free_var("y"),
                Term::free_var("b"),
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
            Subterm::Prim(Prim::bin_concat(
                Grain::X,
                [Term::free_var("x"), Term::free_var("a")],
            )),
        ),
    );

    let that = func(
        ["y"],
        func(
            ["b"],
            Subterm::Prim(Prim::bin_concat(
                Grain::X,
                [Term::free_var("y"), Term::free_var("b")],
            )),
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
                    Grain::X,
                    Term::free_var("x"),
                    Term::free_var("a"),
                    Term::free_var("p"),
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
                    Grain::X,
                    Term::free_var("y"),
                    Term::free_var("b"),
                    Term::free_var("q"),
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

    let this = Term::proj(Term::free_var("r"), 0);
    let that = Term::proj(Term::free_var("r"), 0);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn convert_proj_different_index_is_false() {
    let mut context = context();

    let this = Term::proj(Term::free_var("r"), 0);
    let that = Term::proj(Term::free_var("r"), 1);

    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

#[test]
fn convert_eta_tuple_neutral_with_known_type() {
    let mut context = context();

    let tuple_type: Term = Term::tuple_type([
        ("x", Term::prim(Prim::NatType)),
        ("y", Term::prim(Prim::BoolType)),
    ]);

    let r: Term = Term::free_var("r");
    let s: Term = Term::free_var("s");

    assert_eq!(convert(&mut context, &tuple_type, &r, &r), Ok(true));

    assert_eq!(convert(&mut context, &tuple_type, &r, &s), Ok(false));
}

#[test]
fn convert_partial_projection_tuple_at_narrow_type() {
    let mut context = context();

    // p = (1, 2), q = (1, 3) — both 2-tuples agreeing on field 0, differing on field 1.
    context.define("p", &Term::tuple([nat(1), nat(2)]), None);
    context.define("q", &Term::tuple([nat(1), nat(3)]), None);

    // A 1-field tuple type {x : Nat}.
    let type_: Term = Term::tuple_type([("x", Term::prim(Prim::NatType))]);

    // this = (p.0), that = (q.0). At the 1-field type both denote (a),
    // so conversion should return true.
    let this: Term = Term::tuple([Term::proj(Term::free_var("p"), 0)]);
    let that: Term = Term::tuple([Term::proj(Term::free_var("q"), 0)]);

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

    context.define("loop", &Term::free_var("loop"), None);

    let this = Term::tuple_type([
        (
            "x",
            Term::apply(func(["z"], Term::free_var("z")), [Term::free_var("loop")]),
        ),
        ("y", Term::free_var("x")),
    ]);

    let that = Term::tuple_type([("x", Term::free_var("loop")), ("y", Term::free_var("x"))]);

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
        &Term::func_type([("_", Term::tuple_type_unit())], Term::type_ground()),
    );
    context.assume("r", &Term::tuple_type_unit());
    context.assume("s", &Term::tuple_type_unit());

    let f = Term::free_var("F");
    let r = Term::free_var("r");
    let s = Term::free_var("s");

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
    context
        .register_struct(
            "Wrap",
            StructDecl {
                universe_context: UniverseContext::empty(),
                params: Telescope::done(()),
                fields: Telescope::build(
                    [
                        ("x", Term::prim(Prim::NatType)),
                        ("u", Term::tuple_type_unit()),
                    ],
                    (),
                ),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                root: RootId::Entry,
                rep_public: true,
            },
        )
        .unwrap();

    context.assume("r", &Term::tuple_type_unit());
    context.assume("s", &Term::tuple_type_unit());

    let r = Term::free_var("r");
    let s = Term::free_var("s");

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

    // induct Wrap | wrap(x : Nat, u : ()) end
    context
        .register_induct(
            "Wrap",
            InductDecl {
                universe_context: UniverseContext::empty(),
                params: Telescope::done(()),
                indices: Telescope::done(()),
                constructors: BTreeMap::from([(
                    Atom::from("wrap"),
                    InductParam {
                        telescope: Telescope::build(
                            [
                                ("x", Term::prim(Prim::NatType)),
                                ("u", Term::tuple_type_unit()),
                            ],
                            Term::induct_type("Wrap", Vec::<Term>::new(), Vec::<Term>::new()),
                        ),
                        plicities: vec![Plicity::Explicit, Plicity::Explicit],
                    },
                )]),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                root: RootId::Entry,
                rep_public: true,
            },
        )
        .unwrap();

    context.assume("r", &Term::tuple_type_unit());
    context.assume("s", &Term::tuple_type_unit());

    let r = Term::free_var("r");
    let s = Term::free_var("s");

    // wrap(1, r) and wrap(1, s) differ only in the unit payload's neutral.
    let this = Term::variant("Wrap", Vec::<Term>::new(), "wrap", [nat(1), r]);
    let that = Term::variant("Wrap", Vec::<Term>::new(), "wrap", [nat(1), s]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

// === Metavariables / unification ===========================================

#[test]
fn solve_flex_rigid_commits_solution() {
    let mut context = context();
    context.birth_metavar(MetaId(0), Vec::new(), Term::type_ground());

    // ?0 ≟ Nat  (at type Type)
    let nat = Term::prim(Prim::NatType);
    assert_eq!(conv(&mut context, &Term::metavar(0), &nat), Ok(true));
    assert_eq!(context.metavar_solution(MetaId(0)), Some(&nat));
}

#[test]
fn solve_is_symmetric() {
    let mut context = context();
    context.birth_metavar(MetaId(0), Vec::new(), Term::type_ground());

    let nat = Term::prim(Prim::NatType);
    // rigid on the left, flex on the right
    assert_eq!(conv(&mut context, &nat, &Term::metavar(0)), Ok(true));
    assert_eq!(context.metavar_solution(MetaId(0)), Some(&nat));
}

#[test]
fn occurs_check_rejects_cyclic_solution() {
    let mut context = context();
    context.birth_metavar(MetaId(0), Vec::new(), Term::type_ground());

    // ?0 ≟ (x : ?0) -> Nat  — the candidate mentions ?0 itself.
    let cyclic = Term::func_type([("x", Term::metavar(0))], Term::prim(Prim::NatType));
    assert_eq!(conv(&mut context, &Term::metavar(0), &cyclic), Ok(false));
    assert_eq!(context.metavar_solution(MetaId(0)), None);
}

#[test]
fn scope_check_rejects_out_of_context_variable() {
    let mut context = context();
    // Birth with empty Γ: no variable is in scope for ?0.
    context.birth_metavar(MetaId(0), Vec::new(), Term::type_ground());

    // ?0 ≟ x  — `x` is not available to ?0.
    let x = Term::free_var("x");
    assert_eq!(conv(&mut context, &Term::metavar(0), &x), Ok(false));
    assert_eq!(context.metavar_solution(MetaId(0)), None);
}

#[test]
fn scope_check_allows_in_context_variable() {
    let mut context = context();
    // Γ = (x : Type); result is Type, and the candidate `x` is in scope.
    context.assume("x", &Term::type_ground());
    context.birth_metavar(
        MetaId(0),
        vec![("x".to_string(), Term::type_ground())],
        Term::type_ground(),
    );

    let x = Term::free_var("x");
    let occurrence = Term::metavar_birthed(0, None, vec![x.clone()]);
    assert_eq!(conv(&mut context, &occurrence, &x), Ok(true));
    assert_eq!(context.metavar_solution(MetaId(0)), Some(&x));
}

#[test]
fn revalidation_admits_checkable_but_not_inferable_candidate() {
    let mut context = context();
    // ?0 : (x : Nat, y : Nat) — a tuple type, born in empty Γ.
    let pair_type = Term::tuple_type([
        ("x", Term::prim(Prim::NatType)),
        ("y", Term::prim(Prim::NatType)),
    ]);
    context.birth_metavar(MetaId(0), Vec::new(), pair_type);

    // ?0 ≟ (1, 2). A bare tuple has no synthesizable type (`elaborate_tuple`
    // is Check-only), so synthesize-then-convert re-validation rejected it;
    // checking it against the frozen tuple result type admits it.
    let pair = Term::tuple([nat(1), nat(2)]);
    assert_eq!(conv(&mut context, &Term::metavar(0), &pair), Ok(true));
    assert_eq!(context.metavar_solution(MetaId(0)), Some(&pair));
}

#[test]
fn revalidation_rejects_ill_typed_candidate_through_checking() {
    let mut context = context();
    // ?0 : (x : Nat, y : Nat).
    let pair_type = Term::tuple_type([
        ("x", Term::prim(Prim::NatType)),
        ("y", Term::prim(Prim::NatType)),
    ]);
    context.birth_metavar(MetaId(0), Vec::new(), pair_type);

    // ?0 ≟ (1, 2, 3): a three-field tuple does not check against a two-field
    // tuple type, so checking still rejects the candidate and commits nothing.
    let wrong = Term::tuple([nat(1), nat(2), nat(3)]);
    assert_eq!(conv(&mut context, &Term::metavar(0), &wrong), Ok(false));
    assert_eq!(context.metavar_solution(MetaId(0)), None);
}

#[test]
fn flex_flex_equal_id_short_circuits() {
    let mut context = context();
    context.birth_metavar(MetaId(0), Vec::new(), Term::type_ground());

    // ?0 ≟ ?0 is trivially true and leaves the metavariable unsolved.
    assert_eq!(
        conv(&mut context, &Term::metavar(0), &Term::metavar(0)),
        Ok(true)
    );
    assert_eq!(context.metavar_solution(MetaId(0)), None);
}

#[test]
fn flex_flex_distinct_is_residual() {
    let mut context = context();
    context.birth_metavar(MetaId(0), Vec::new(), Term::type_ground());
    context.birth_metavar(MetaId(1), Vec::new(), Term::type_ground());

    // ?0 ≟ ?1 postpones with no way to progress — a residual constraint.
    assert_eq!(
        conv(&mut context, &Term::metavar(0), &Term::metavar(1)),
        Ok(false)
    );
}

#[test]
fn conversion_cannot_solve_a_protected_recursive_slot() {
    let mut context = context();
    let (id, slot) = context.fresh_rec_slot(Term::type_ground());
    let nat_type = Term::prim(Prim::NatType);

    assert!(matches!(
        convert_outcome(&mut context, &Term::type_ground(), &slot, &nat_type),
        Ok(Outcome::Blocked(_))
    ));
    assert!(context.metavar_solution(id).is_none());

    context.fill_rec_slot(id, nat_type.clone());
    assert_eq!(reduce(&mut context, slot), Ok(nat_type));
}

#[test]
fn embedded_metavar_postpones_to_residual() {
    let mut context = context();
    context.birth_metavar(MetaId(0), Vec::new(), Term::type_ground());
    context.birth_metavar(MetaId(1), Vec::new(), Term::type_ground());

    // ?0 ≟ (x : ?1) -> Nat — ?1 is an unsolved embedded metavariable, so the
    // solve is postponed; nothing solves ?1, so it stays residual.
    let candidate = Term::func_type([("x", Term::metavar(1))], Term::prim(Prim::NatType));
    assert_eq!(conv(&mut context, &Term::metavar(0), &candidate), Ok(false));
    assert_eq!(context.metavar_solution(MetaId(0)), None);
}

#[test]
fn revalidation_rejects_ill_typed_solution() {
    let mut context = context();
    // ?0 : Nat under empty Γ. A candidate of type Type (e.g. `Bool`) does not
    // type-check against Nat, so re-validation rejects it.
    context.birth_metavar(MetaId(0), Vec::new(), Term::prim(Prim::NatType));

    let bool_ = Term::prim(Prim::BoolType);
    assert_eq!(conv(&mut context, &Term::metavar(0), &bool_), Ok(false));
    assert_eq!(context.metavar_solution(MetaId(0)), None);
}

#[test]
fn revalidation_suppresses_refinements_rejecting_a_refined_solution() {
    // The §12 regression. Γ = (t : Type) with a counterfactual match-arm
    // refinement `t := Nat` in force (as inside `bool_match b { true => ... }`,
    // where the family `T(b) ⇝ Nat`). `?0 : t` is born under the *frozen*
    // Γ = (t : Type) — its result type depends on the refined head, mirroring
    // `m : T(b)`.
    let mut context = context();
    context.assume("t", &Term::type_ground());
    context.refine("t", &Term::prim(Prim::NatType));
    context.birth_metavar(
        MetaId(0),
        vec![("t".to_string(), Term::type_ground())],
        Term::free_var("t"),
    );

    // `?0 ≟ 5` at type `t`. Locally (refinement on) `t ⇝ Nat` and `5 : t` holds,
    // but re-validation suppresses refinements, leaving `t` abstract, so `5 : t`
    // fails and the solution is rejected — the program is unsound otherwise.
    let t = Term::free_var("t");
    let occurrence = Term::metavar_birthed(0, None, vec![t.clone()]);
    let five = Term::prim(Prim::Nat(Nat::new(5usize)));
    assert_eq!(convert(&mut context, &t, &occurrence, &five), Ok(false));
    assert_eq!(context.metavar_solution(MetaId(0)), None);
}

#[test]
fn revalidation_accepts_a_refinement_independent_solution() {
    // The §12 twin. The same refinement `t := Nat` is in force, but `?0`'s result
    // type is `Nat` directly — it does not depend on the refined head. Re-validation
    // checks `5 : Nat` with refinements suppressed (none are needed) and commits.
    let mut context = context();
    context.assume("t", &Term::type_ground());
    context.refine("t", &Term::prim(Prim::NatType));
    context.birth_metavar(
        MetaId(0),
        vec![("t".to_string(), Term::type_ground())],
        Term::prim(Prim::NatType),
    );

    let nat = Term::prim(Prim::NatType);
    let occurrence = Term::metavar_birthed(0, None, vec![Term::free_var("t")]);
    let five = Term::prim(Prim::Nat(Nat::new(5usize)));
    assert_eq!(convert(&mut context, &nat, &occurrence, &five), Ok(true));
    assert_eq!(context.metavar_solution(MetaId(0)), Some(&five));
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
    context.birth_metavar(MetaId(0), vec![("a".into(), nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(0, None, vec![Term::free_var("y")]);

    // ?0[y] ≟ y — inverting the renaming stores the solution in birth-named
    // form: `a`, not `y`.
    assert_eq!(
        conv(&mut context, &occurrence, &Term::free_var("y")),
        Ok(true)
    );
    assert_eq!(
        context.metavar_solution(MetaId(0)),
        Some(&Term::free_var("a"))
    );
}

#[test]
fn solve_through_an_identity_spine_matches_legacy() {
    let mut context = context();
    context.birth_metavar(MetaId(0), vec![("a".into(), nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(0, None, vec![Term::free_var("a")]);

    // The identity spine behaves exactly like the empty (legacy bare-hole)
    // spine: the candidate is stored unchanged.
    assert_eq!(conv(&mut context, &occurrence, &nat(1)), Ok(true));
    assert_eq!(context.metavar_solution(MetaId(0)), Some(&nat(1)));
}

#[test]
fn solve_postpones_a_duplicated_renaming() {
    let mut context = context();
    context.birth_metavar(
        MetaId(0),
        vec![("a".into(), nat_type()), ("b".into(), nat_type())],
        nat_type(),
    );
    // Both entries are the same live name: which birth binder `y` stands for
    // is ambiguous, so a candidate mentioning it is undecided, not unequal.
    let occurrence = Term::metavar_birthed(0, None, vec![Term::free_var("y"), Term::free_var("y")]);

    let outcome = convert_outcome(
        &mut context,
        &Term::type_ground(),
        &occurrence,
        &Term::free_var("y"),
    );
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetaId(0)), None);
}

#[test]
fn solve_prunes_dependence_on_a_non_pattern_entry() {
    let mut context = context();
    context.birth_metavar(
        MetaId(0),
        vec![("a".into(), nat_type()), ("b".into(), nat_type())],
        nat_type(),
    );
    // First slot a pattern variable, second a compound term: the candidate
    // may depend on the first but not (yet) on the second.
    let compound: Term = Subterm::Prim(Prim::nat_add(Term::free_var("z"), nat(1))).into();
    let occurrence = Term::metavar_birthed(0, None, vec![Term::free_var("y"), compound.clone()]);

    // ?0[y, z+1] ≟ y — solvable through the pattern slot alone.
    assert_eq!(
        conv(&mut context, &occurrence, &Term::free_var("y")),
        Ok(true)
    );
    assert_eq!(
        context.metavar_solution(MetaId(0)),
        Some(&Term::free_var("a"))
    );
}

#[test]
fn solve_postpones_a_candidate_reaching_through_a_non_pattern_entry() {
    let mut context = context();
    context.birth_metavar(
        MetaId(0),
        vec![("a".into(), nat_type()), ("b".into(), nat_type())],
        nat_type(),
    );
    let compound: Term = Subterm::Prim(Prim::nat_add(Term::free_var("z"), nat(1))).into();
    let occurrence = Term::metavar_birthed(0, None, vec![Term::free_var("y"), compound]);

    // ?0[y, z+1] ≟ z — `z` is reachable only through the non-pattern slot
    // (and is not an occurrence of the whole entry): undecided.
    let outcome = convert_outcome(
        &mut context,
        &Term::type_ground(),
        &occurrence,
        &Term::free_var("z"),
    );
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetaId(0)), None);
}

#[test]
fn solve_rejects_an_out_of_image_variable() {
    let mut context = context();
    context.birth_metavar(MetaId(0), vec![("a".into(), nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(0, None, vec![Term::free_var("y")]);

    // ?0[y] ≟ z — `z` corresponds to no birth binder and never can: a hard
    // mismatch, not a postponement.
    let outcome = convert_outcome(
        &mut context,
        &Term::type_ground(),
        &occurrence,
        &Term::free_var("z"),
    );
    assert!(matches!(outcome, Ok(Outcome::Mismatch)));
    assert_eq!(context.metavar_solution(MetaId(0)), None);
}

#[test]
fn solve_classifies_a_solved_metavariable_spine_entry_by_its_value() {
    let mut context = context();
    // ?0 is already solved to its own binder, so an occurrence ?0[y] stands
    // for `y` — a perfectly good pattern variable hiding behind a node.
    context.birth_metavar(MetaId(0), vec![("a".into(), nat_type())], nat_type());
    context.solve_metavar(MetaId(0), Term::free_var("a"));
    let entry = Term::metavar_birthed(0, None, vec![Term::free_var("y")]);

    context.birth_metavar(MetaId(1), vec![("b".into(), nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(1, None, vec![entry]);

    // ?1[?0[y]] ≟ y — the entry resolves to `y` and inverts to `b`.
    assert_eq!(
        conv(&mut context, &occurrence, &Term::free_var("y")),
        Ok(true)
    );
    assert_eq!(
        context.metavar_solution(MetaId(1)),
        Some(&Term::free_var("b"))
    );
}

#[test]
fn solve_abstracts_a_non_pattern_occurrence() {
    let mut context = context();
    context.birth_metavar(
        MetaId(0),
        vec![("a".into(), nat_type()), ("b".into(), nat_type())],
        nat_type(),
    );
    // A reduce-stable compound (a tuple is a normal form), matched by the
    // raw spelling; the reduced-spelling case is the next test.
    let compound = Term::tuple([Term::free_var("z")]);
    let occurrence = Term::metavar_birthed(0, None, vec![Term::free_var("y"), compound.clone()]);

    // ?0[y, (z,)] ≟ (z,) — the candidate *is* an occurrence of the
    // non-pattern entry, which abstracts to its birth binder `b`.
    assert_eq!(conv(&mut context, &occurrence, &compound), Ok(true));
    assert_eq!(
        context.metavar_solution(MetaId(0)),
        Some(&Term::free_var("b"))
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
        context.assume("b", &Term::type_ground());
        context.refine("b", &nat_type());
        context.park(
            ParkedWork::Conversion(Goal {
                type_: Term::type_ground(),
                this: Term::free_var("b"),
                that: nat_type(),
            }),
            Term::free_var("b"),
        );
    });

    // The frame is gone; the drain retries under the frozen one, where the
    // refinement still holds and the goal converts.
    assert!(context.drain_parked().is_ok());
}

#[test]
fn parked_goals_without_their_refinement_mismatch() {
    let mut context = context();

    // Control: the same goal parked without the refinement cannot convert,
    // and the drain reports it at its origin.
    context.with_frame(|context| {
        context.assume("b", &Term::type_ground());
        context.park(
            ParkedWork::Conversion(Goal {
                type_: Term::type_ground(),
                this: Term::free_var("b"),
                that: nat_type(),
            }),
            Term::free_var("b"),
        );
    });

    assert!(context.drain_parked().is_err());
}

#[test]
fn solve_abstracts_a_reduced_spelling_occurrence() {
    let mut context = context();
    context.birth_metavar(
        MetaId(0),
        vec![("a".into(), nat_type()), ("b".into(), nat_type())],
        nat_type(),
    );
    // `z + 1` successor-peels under reduction, and the candidate side arrives
    // reduced — each subject contributes both spellings, so the occurrence
    // still abstracts, and the round-trip verification accepts the pair by
    // definitional (not syntactic) equality.
    let compound: Term = Subterm::Prim(Prim::nat_add(Term::free_var("z"), nat(1))).into();
    let occurrence = Term::metavar_birthed(0, None, vec![Term::free_var("y"), compound.clone()]);

    assert_eq!(conv(&mut context, &occurrence, &compound), Ok(true));
    assert_eq!(
        context.metavar_solution(MetaId(0)),
        Some(&Term::free_var("b"))
    );
}

#[test]
fn flex_flex_same_id_converts_through_equal_spines() {
    let mut context = context();
    context.birth_metavar(MetaId(0), vec![("a".into(), nat_type())], nat_type());

    // Two occurrences of the same unsolved metavariable whose spines differ
    // syntactically but agree definitionally (`1 + 1` reduces to `2`): the
    // congruence probe discharges the goal without solving anything.
    let sum: Term = Subterm::Prim(Prim::nat_add(nat(1), nat(1))).into();
    let this = Term::metavar_birthed(0, None, vec![sum]);
    let that = Term::metavar_birthed(0, None, vec![nat(2)]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
    assert_eq!(context.metavar_solution(MetaId(0)), None);
}

#[test]
fn flex_flex_same_id_with_disagreeing_spines_stays_blocked() {
    let mut context = context();
    context.birth_metavar(MetaId(0), vec![("a".into(), nat_type())], nat_type());

    // Disagreeing spines are not *unequal* — the solution may ignore the
    // slot — so the pair parks rather than mismatching.
    let this = Term::metavar_birthed(0, None, vec![nat(1)]);
    let that = Term::metavar_birthed(0, None, vec![nat(2)]);

    let outcome = convert_outcome(&mut context, &Term::type_ground(), &this, &that);
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
    context.birth_metavar(MetaId(0), vec![("a".into(), nat_type())], nat_type());
    context.birth_metavar(MetaId(1), vec![("b".into(), nat_type())], nat_type());

    let this = Term::metavar_birthed(0, None, vec![Term::free_var("x")]);
    let that = Term::metavar_birthed(1, None, vec![Term::free_var("x")]);

    let outcome = convert_outcome(&mut context, &Term::type_ground(), &this, &that);
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetaId(0)), None);
    assert_eq!(context.metavar_solution(MetaId(1)), None);
}

#[test]
fn rollback_solutions_unwinds_to_the_mark() {
    let mut context = context();
    context.birth_metavar(MetaId(0), vec![("a".into(), nat_type())], nat_type());
    context.birth_metavar(MetaId(1), vec![("a".into(), nat_type())], nat_type());

    context.solve_metavar(MetaId(0), nat(1));
    let mark = context.solution_mark();
    context.solve_metavar(MetaId(1), nat(2));

    context.rollback_solutions(mark);

    // The solution past the mark is unwound; the one before it survives. This
    // is the bracket `solve` wraps around re-validation, so a rejected
    // candidate's nested solves leave no fingerprints.
    assert_eq!(context.metavar_solution(MetaId(0)), Some(&nat(1)));
    assert_eq!(context.metavar_solution(MetaId(1)), None);
}

#[test]
fn stuck_prim_on_a_metavar_parks_instead_of_mismatching() {
    let mut context = context();
    context.birth_metavar(MetaId(0), vec![("a".into(), nat_type())], nat_type());
    let m = Term::metavar_birthed(0, None, vec![Term::free_var("a")]);
    let stuck: Term = Subterm::Prim(Prim::NatSub(m.clone(), nat(1))).into();

    // `?0 - 1 ≈ 0` is undecided, not unequal: solving `?0` may fold the
    // subtraction. (`NatAdd` escapes via successor peeling; the other
    // operators rely on this parking.)
    let outcome = convert_outcome(&mut context, &Term::type_ground(), &stuck, &nat(0));
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetaId(0)), None);

    // Within one run, a sibling goal pins `?0 := 1`; the parked subtraction
    // is retried, folds to `0`, and converts.
    let this = Term::tuple([stuck, m]);
    let that = Term::tuple([nat(0), nat(1)]);
    assert_eq!(conv(&mut context, &this, &that), Ok(true));
    assert_eq!(context.metavar_solution(MetaId(0)), Some(&nat(1)));
}

#[test]
fn rigid_head_mismatch_with_a_metavar_inside_still_fails_fast() {
    let mut context = context();
    context.birth_metavar(MetaId(0), vec![("a".into(), nat_type())], nat_type());
    let m = Term::metavar_birthed(0, None, vec![Term::free_var("a")]);

    // An inductive type against `Nat` is provably unequal whatever `?0` becomes —
    // the heads are rigid — so the mismatch stays hard (and is reported at
    // the use site, not deferred to the drain).
    let induct_decl = Term::induct_type("Vec", [m], Vec::<Term>::new());
    let outcome = convert_outcome(
        &mut context,
        &Term::type_ground(),
        &induct_decl,
        &nat_type(),
    );
    assert!(matches!(outcome, Ok(Outcome::Mismatch)));
}

#[test]
fn arm_refinement_does_not_taint_a_committed_solution() {
    let mut context = context();
    context.assume("n", &nat_type());
    context.birth_metavar(MetaId(0), vec![("n".into(), nat_type())], nat_type());
    let occurrence = Term::metavar_birthed(0, None, vec![Term::free_var("n")]);

    // Inside a frame that counterfactually refines `n := 0` (a match arm),
    // the goal `?0[n] ≈ n` still discharges — but the *committed* solution is
    // the refinement-free `n`, not the arm-local `0`: a metavariable must not
    // be pinned to a value that holds only counterfactually inside the arm.
    let converts = context.with_frame(|context| {
        context.refine("n", &nat(0));
        conv(context, &occurrence, &Term::free_var("n"))
    });
    assert_eq!(converts, Ok(true));
    assert_eq!(
        context.metavar_solution(MetaId(0)),
        Some(&Term::free_var("n"))
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
        convert(
            &mut context,
            &nat_type(),
            &Term::tuple(Vec::<Term>::new()),
            &nat(1)
        ),
        Ok(true)
    );
}

// === Flex-apply imitation (higher-kinded metavariables) =====================

/// Register a `Lst`-shaped inductive: one parameter, no indices.
fn register_lst(context: &mut Context) {
    context
        .register_induct(
            "Lst",
            InductDecl {
                universe_context: UniverseContext::empty(),
                params: Telescope::build([("A", Term::type_ground())], ()),
                indices: Telescope::build([("A", Term::type_ground())], ()),
                constructors: BTreeMap::new(),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                root: RootId::Entry,
                rep_public: true,
            },
        )
        .unwrap();
}

/// Register a `Vec`-shaped inductive: one parameter, one `Nat` index.
fn register_vec(context: &mut Context) {
    context
        .register_induct(
            "Vec",
            InductDecl {
                universe_context: UniverseContext::empty(),
                params: Telescope::build([("T", Term::type_ground())], ()),
                indices: Telescope::build(
                    [("T", Term::type_ground()), ("n", Term::prim(Prim::NatType))],
                    (),
                ),
                constructors: BTreeMap::new(),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                root: RootId::Entry,
                rep_public: true,
            },
        )
        .unwrap();
}

/// The kind `(Type) -> Type`.
fn type_to_type() -> Term {
    Term::func_type([("A", Term::type_ground())], Term::type_ground())
}

#[test]
fn imitation_solves_flex_apply_against_inductive() {
    let mut context = context();
    register_lst(&mut context);
    context.birth_metavar(MetaId(0), Vec::new(), type_to_type());

    // ?0(Nat) ≟ Lst(Nat)  — commits ?0 := λA. Lst(A).
    let flex = Term::apply(Term::metavar(0), [nat_type()]);
    let rigid = Term::induct_type("Lst", [nat_type()], Vec::<Term>::new());
    assert_eq!(conv(&mut context, &flex, &rigid), Ok(true));
    assert!(context.metavar_solution(MetaId(0)).is_some());

    // The committed solution is the imitation, not the constant: applied to a
    // different argument it yields Lst of *that* argument.
    let at_bln = Term::apply(Term::metavar(0), [Term::prim(Prim::BoolType)]);
    let lst_bln = Term::induct_type("Lst", [Term::prim(Prim::BoolType)], Vec::<Term>::new());
    assert_eq!(conv(&mut context, &at_bln, &lst_bln), Ok(true));
}

#[test]
fn imitation_is_symmetric() {
    let mut context = context();
    register_lst(&mut context);
    context.birth_metavar(MetaId(0), Vec::new(), type_to_type());

    // Rigid on the left, stuck application on the right.
    let flex = Term::apply(Term::metavar(0), [nat_type()]);
    let rigid = Term::induct_type("Lst", [nat_type()], Vec::<Term>::new());
    assert_eq!(conv(&mut context, &rigid, &flex), Ok(true));
    assert!(context.metavar_solution(MetaId(0)).is_some());
}

#[test]
fn imitation_equates_arguments_pairwise() {
    let mut context = context();
    register_lst(&mut context);
    context.birth_metavar(MetaId(0), Vec::new(), type_to_type());
    context.birth_metavar(MetaId(1), Vec::new(), Term::type_ground());

    // ?0(?1) ≟ Lst(Nat) — the imitation solves ?0, the pairwise equation ?1.
    let flex = Term::apply(Term::metavar(0), [Term::metavar(1)]);
    let rigid = Term::induct_type("Lst", [nat_type()], Vec::<Term>::new());
    assert_eq!(conv(&mut context, &flex, &rigid), Ok(true));
    assert!(context.metavar_solution(MetaId(0)).is_some());
    assert_eq!(context.metavar_solution(MetaId(1)), Some(&nat_type()));
}

#[test]
fn imitation_splits_params_and_indices() {
    let mut context = context();
    register_vec(&mut context);
    context.birth_metavar(
        MetaId(0),
        Vec::new(),
        Term::func_type(
            [("T", Term::type_ground()), ("n", Term::prim(Prim::NatType))],
            Term::type_ground(),
        ),
    );

    // ?0(Nat, 3) ≟ Vec(Nat, 3) — arity 2 = 1 param + 1 index; the candidate's
    // body must mirror the rigid node's split or re-validation rejects it.
    let flex = Term::apply(Term::metavar(0), [nat_type(), nat(3)]);
    let rigid = Term::induct_type("Vec", [nat_type()], [nat(3)]);
    assert_eq!(conv(&mut context, &flex, &rigid), Ok(true));
    assert!(context.metavar_solution(MetaId(0)).is_some());

    let at_two = Term::apply(Term::metavar(0), [Term::prim(Prim::BoolType), nat(2)]);
    let vec_two = Term::induct_type("Vec", [Term::prim(Prim::BoolType)], [nat(2)]);
    assert_eq!(conv(&mut context, &at_two, &vec_two), Ok(true));
}

#[test]
fn imitation_solves_against_struct_type() {
    let mut context = context();
    context
        .register_struct(
            "Pair",
            StructDecl {
                universe_context: UniverseContext::empty(),
                params: Telescope::build(
                    [("A", Term::type_ground()), ("B", Term::type_ground())],
                    (),
                ),
                fields: Telescope::build(
                    [("A", Term::type_ground()), ("B", Term::type_ground())],
                    (),
                ),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                root: RootId::Entry,
                rep_public: true,
            },
        )
        .unwrap();
    context.birth_metavar(
        MetaId(0),
        Vec::new(),
        Term::func_type(
            [("A", Term::type_ground()), ("B", Term::type_ground())],
            Term::type_ground(),
        ),
    );

    let flex = Term::apply(Term::metavar(0), [nat_type(), nat_type()]);
    let rigid = Term::struct_type("Pair", [nat_type(), nat_type()]);
    assert_eq!(conv(&mut context, &flex, &rigid), Ok(true));
    assert!(context.metavar_solution(MetaId(0)).is_some());
}

#[test]
fn imitation_arity_mismatch_blocks() {
    let mut context = context();
    register_vec(&mut context);
    context.birth_metavar(MetaId(0), Vec::new(), type_to_type());

    // ?0(Nat) ≟ Vec(Nat, 3) — apply arity 1 against constructor arity 2:
    // v1 has no partial-application solutions, so the goal blocks (it is not
    // provably unequal — a constant solution could exist).
    let flex = Term::apply(Term::metavar(0), [nat_type()]);
    let rigid = Term::induct_type("Vec", [nat_type()], [nat(3)]);
    let outcome = convert_outcome(&mut context, &Term::type_ground(), &flex, &rigid);
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetaId(0)), None);
}

#[test]
fn imitation_non_function_birth_type_blocks() {
    let mut context = context();
    register_lst(&mut context);
    // ?0's frozen type is not a function type: no candidate can be built.
    context.birth_metavar(MetaId(0), Vec::new(), Term::type_ground());

    let flex = Term::apply(Term::metavar(0), [nat_type()]);
    let rigid = Term::induct_type("Lst", [nat_type()], Vec::<Term>::new());
    let outcome = convert_outcome(&mut context, &Term::type_ground(), &flex, &rigid);
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetaId(0)), None);
}

#[test]
fn imitation_leaves_rigid_apply_pairs_alone() {
    let mut context = context();
    register_lst(&mut context);
    context.assume("f", &type_to_type());

    // A *rigid* stuck application against a nominal type is not the imitation
    // case: the guard falls back to the neutral path, which cannot equate
    // them — a definite mismatch, exactly as before the rule existed.
    let stuck = Term::apply(Term::free_var("f"), [nat_type()]);
    let rigid = Term::induct_type("Lst", [nat_type()], Vec::<Term>::new());
    assert_eq!(conv(&mut context, &stuck, &rigid), Ok(false));
}

#[test]
fn imitation_solves_flex_apply_against_prim_former() {
    let mut context = context();
    context.birth_metavar(MetaId(0), Vec::new(), type_to_type());

    // ?0(?1) ≟ Lst(Nat) — the imitation solves ?0 := λT. Lst(T), the pairwise
    // equation ?1 := Nat. This is what pins `M := Lst` for `Monad(Lst)`.
    context.birth_metavar(MetaId(1), Vec::new(), Term::type_ground());
    let flex = Term::apply(Term::metavar(0), [Term::metavar(1)]);
    let rigid = Term::prim(Prim::LstType(nat_type()));
    assert_eq!(conv(&mut context, &flex, &rigid), Ok(true));
    assert!(context.metavar_solution(MetaId(0)).is_some());
    assert_eq!(context.metavar_solution(MetaId(1)), Some(&nat_type()));
}
