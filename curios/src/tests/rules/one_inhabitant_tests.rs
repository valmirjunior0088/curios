//! Two terms at a type with no field or one inhabitant convert, and at a relevant field, a struct that reaches itself, or an elimination, arm binder or constructor at a relevant type, stay apart.

use {super::test_support::*, crate::tests::test_support::*, curios_elab::Error};

// **Unit eta is the type's, in both checkers.** A type with no field — the empty Σ, a nominal struct that declares none — has one inhabitant, so any two terms convert at it, and each checker decides the goal by its type ahead of every structural rule. The elaborator reached the rule only between two sides no structural rule claimed and the certifier had it against a literal alone, so two variables elaborated and were refused by the certifier, and two applications or two stuck matches were refused by the elaborator, which held each equal to a variable. The first two programs are the ones that reached the certifier's refusal; the rest are the shapes, and the two eta rules that carry a goal to a unit.
//
// The refusal beside it is what keeps the acceptance from reading as "any two records convert": one relevant field keeps two neutrals apart.
#[test]
fn any_two_terms_convert_at_a_type_with_no_field() {
    assert_eq!(run(ANY_TWO_TERMS_CONVERT_AT_A_TYPE_WITH_NO_FIELD), b"1");
}

// Its control is the same pair at a record of units.
#[test]
fn two_neutrals_at_a_record_with_a_relevant_field_stay_apart() {
    control(
        r#"
        use /std/{Eq, Nat};

        let apart(r : {{}, {}}, s : {{}, {}}) -> Eq(@({{}, {}}))(r, s) = Eq/refl();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "let apart(r : {{}, {}}, s : {{}, {}}) -> Eq(@({{}, {}}))(r, s)",
        "let apart(r : {Nat, {}}, s : {Nat, {}}) -> Eq(@({Nat, {}}))(r, s)",
    )])
    .by_elaborator_in("/apart", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// **A nominal struct has no eta by its type, and what eta would decide is read off the type.** Two neutrals at a struct have no literal to open, so neither checker expands them: any two terms convert at a struct every field of which has one inhabitant — a unit, a proof, a function into a unit, a record or a struct of such — and are left to their heads otherwise. The elaborator projected two neutrals at a struct and compared the projections with no type, which decided nothing, and the certifier compared their heads; once a lookup typed those projections the elaborator accepted two variables at a struct of a unit and the certifier refused them, which is the first program.
//
// The refusals beside it are the rule's bounds: one relevant field keeps two neutrals apart, under whatever nesting, and a struct that reaches itself answers no where it is met again, which is what ends the walk — by its own fields or through another struct's, and asked inside its own declaration as after it.
//
// A struct nested in its own parameter is met again too, and is judged again: both checkers refused two variables at `Pair(Pair({}))` where they converged at `Pair(Other({}))`, a verdict that followed which name wrapped a unit.
#[test]
fn any_two_terms_convert_at_a_struct_with_one_inhabitant() {
    assert_eq!(
        run(ANY_TWO_TERMS_CONVERT_AT_A_STRUCT_WITH_ONE_INHABITANT),
        b"1"
    );
}

// Each control is the same pair where the struct has one inhabitant, or, where it reaches itself through another, where the group is propositions.
#[test]
fn two_neutrals_at_a_struct_with_a_relevant_field_stay_apart() {
    control(
        r#"
        use /std/{Eq, Nat};

        struct V: pub Type { n: {}, u: {} }

        let apart(r : V, s : V) -> Eq(@(V))(r, s) = Eq/refl();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("n: {}", "n: Nat")])
    .by_elaborator_in("/apart", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

#[test]
fn two_neutrals_at_a_struct_that_reaches_itself_stay_apart() {
    control(
        r#"
        use /std/{Eq, Nat};

        struct R: pub Type { next: (Nat) -> {} }

        let apart(r : R, s : R) -> Eq(@(R))(r, s) = Eq/refl();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("next: (Nat) -> {}", "next: (Nat) -> R")])
    .by_elaborator_in("/apart", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

#[test]
fn two_neutrals_at_a_struct_nested_over_a_relevant_field_stay_apart() {
    control(
        r#"
        use /std/{Eq, Nat};

        struct Pair(A: Type): pub Type { a: A, b: A }

        let apart(r : Pair(Pair({})), s : Pair(Pair({})))
            -> Eq(@(Pair(Pair({}))))(r, s)
            = Eq/refl();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("let apart(r : Pair(Pair({})), s : Pair(Pair({})))\n            -> Eq(@(Pair(Pair({}))))(r, s)", "let apart(r : Pair(Pair(Nat)), s : Pair(Pair(Nat)))\n            -> Eq(@(Pair(Pair(Nat))))(r, s)"),
    ])
    .by_elaborator_in("/apart", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

#[test]
fn two_neutrals_at_structs_that_reach_each_other_stay_apart() {
    control(
        r#"
        use /std/{Eq, Nat};

        struct Left: pub Prop { right: (Nat) -> Right }
        and Right: pub Prop { left: (Nat) -> Left }

        let apart(r : Left, s : Left) -> Eq(@(Left))(r, s) = Eq/refl();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("struct Left: pub Prop", "struct Left: pub Type"),
        ("and Right: pub Prop", "and Right: pub Type"),
    ])
    .by_elaborator_in("/apart", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

#[test]
fn two_neutrals_at_a_struct_that_reaches_itself_stay_apart_inside_its_declaration() {
    control(
        r#"
        use /std/{Eq, Nat};

        struct R: pub Prop {
            g: (Nat) -> R,
            h: Eq(@(R))(g(0), g(0)),
            k: Eq(@(Eq(@(R))(g(0), g(1))))(h, h)
        }

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("struct R: pub Prop {", "struct R: pub Type {")])
    .by_elaborator_in("/R", |error| matches!(error, Error::TypeMismatch { .. }));
}

// **A goal's type is read forced in both checkers.** A struct declared with `and` is referred to through its group, and a type a recursive definition computes is a call of it: weak-head reduction leaves both folded, and what a type directs is keyed on the type's own shape and sort. Read in weak-head form, two proofs of a proposition declared in a group and two variables at a struct of units a definition computes are compared by their heads where the same type written out converges, and where a lookup, which forces what it reads, converges at a child no type was handed for. A group is a cycle, and a struct that reaches itself has no one inhabitant by its shape, so the group's members here are propositions.
//
// The refusal beside it is a group's member with a relevant field.
#[test]
fn a_goals_type_is_read_forced() {
    assert_eq!(run(A_GOALS_TYPE_IS_READ_FORCED), b"1");
}

// Its control is the same group declared as propositions, its fields proofs: declared as types, the group is a cycle, which keeps two neutrals apart whatever the field holds.
#[test]
fn two_neutrals_at_a_groups_member_with_a_relevant_field_stay_apart() {
    control(
        r#"
        use /std/{Eq, Nat};

        struct Outer: pub Prop { inner: Inner, u: Eq()(0, 0) }
        and Inner: pub Prop { n: Eq()(0, 0), outer: () -> Outer }

        let apart(r : Outer, s : Outer) -> Eq(@(Outer))(r, s) = Eq/refl();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        (
            "struct Outer: pub Prop { inner: Inner, u: Eq()(0, 0) }",
            "struct Outer: pub Type { inner: Inner, u: {} }",
        ),
        (
            "and Inner: pub Prop { n: Eq()(0, 0), outer: () -> Outer }",
            "and Inner: pub Type { n: Nat, outer: () -> Outer }",
        ),
    ])
    .by_elaborator_in("/apart", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// **Where a child is compared with no type, what a type directs between two neutrals is read off the type a lookup gives both.** A tuple literal's component inside a stuck elimination's arm is such a child, and two variables there at a record of units, at a function into a unit or at a struct of a unit were refused by both checkers, which each equate them wherever a type reaches them. Neither side is expanded: eta between two neutrals decides nothing the type's shape does not, and the goal it would pose at a looked-up type is the one in hand, posed again without end. The last program is two proofs of a proposition whose implicit the elaborator solved, which it refused where the same proposition written out converged.
//
// The refusal beside it is the same pair at a record with a relevant field.
#[test]
fn two_neutrals_convert_where_a_lookup_gives_them_a_type_with_one_inhabitant() {
    assert_eq!(
        run(TWO_NEUTRALS_CONVERT_WHERE_A_LOOKUP_GIVES_THEM_A_TYPE_WITH_ONE_INHABITANT),
        b"1"
    );
}

// Its control is the same pair at a record of units.
#[test]
fn two_neutrals_stay_apart_where_a_lookup_gives_them_a_type_with_a_relevant_field() {
    control(
        r#"
        use /std/{Eq, Nat, Bool};

        let apart(b : Bool, r : {{}, {}}, s : {{}, {}})
            -> Eq(@(Nat))((match b: (_) => {{{}, {}}, Nat} | true => (r, 1) | false => (r, 2) end).1,
                          (match b: (_) => {{{}, {}}, Nat} | true => (s, 1) | false => (s, 2) end).1)
            = Eq/refl();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("r : {{}, {}}, s : {{}, {}}", "r : {Nat, {}}, s : {Nat, {}}"),
        ("{{{}, {}}, Nat} | true => (r, 1)", "{{Nat, {}}, Nat} | true => (r, 1)"),
        ("{{{}, {}}, Nat} | true => (s, 1)", "{{Nat, {}}, Nat} | true => (s, 1)"),
    ])
    .by_elaborator_in("/apart", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// **A lookup types every side a position can hold without inferring, and a binder conversion opens carries its type.** Where a child is compared with no type, each checker read what a type directs off the type a lookup gave both sides, and three sides had none. A stuck elimination, so that one at a proposition met neither a variable at it nor an elimination of another scrutinee: its type is its result at its scrutinee. A constructor's value, so that a proof built by one did not meet a proof a variable names: its type is its declaration's. And a binder an arm opened, which the certifier assumed at a stand-in and the elaborator recorded no type for: it is opened at its constructor's field type, so two proofs an arm binds are one. Each program sets its pair as a tuple literal's component in a stuck elimination's arm, a child no type reaches.
//
// The refusal beside each is the same pair at a relevant type.
#[test]
fn a_stuck_elimination_is_typed_by_its_result_at_its_scrutinee() {
    assert_eq!(
        run(A_STUCK_ELIMINATION_IS_TYPED_BY_ITS_RESULT_AT_ITS_SCRUTINEE),
        b"1"
    );
}

// Each control is the same pair at a proposition.
#[test]
fn a_stuck_elimination_at_a_relevant_type_stays_apart_from_a_variable() {
    control(
        r#"
        use /std/{Eq, Nat, Bool};

        let apart(b : Bool, c : Bool, m : Eq()(0, 0), n : Eq()(0, 0))
            -> Eq(@(Nat))((match c: (_) => {Eq()(0, 0), Nat}
                           | true => (match b: (_) => Eq()(0, 0) | true => m | false => n end, 1)
                           | false => (m, 2) end).1,
                          (match c: (_) => {Eq()(0, 0), Nat} | true => (m, 1) | false => (m, 2) end).1)
            = Eq/refl();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("m : Eq()(0, 0), n : Eq()(0, 0)", "m : Nat, n : Nat"),
        ("{Eq()(0, 0), Nat}\n", "{Nat, Nat}\n"),
        ("(_) => Eq()(0, 0) | true => m", "(_) => Nat | true => m"),
        ("{Eq()(0, 0), Nat} | true => (m, 1)", "{Nat, Nat} | true => (m, 1)"),
    ])
    .by_elaborator_in("/apart", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

#[test]
fn a_constructors_value_is_typed_by_its_declaration() {
    assert_eq!(run(A_CONSTRUCTORS_VALUE_IS_TYPED_BY_ITS_DECLARATION), b"1");
}

#[test]
fn a_constructors_value_at_a_relevant_family_stays_apart_from_a_variable() {
    control(
        r#"
        use /std/{Eq, Nat, Bool, Option};

        let apart(b : Bool, o : Eq()(0, 0))
            -> Eq(@(Nat))((match b: (_) => {Eq()(0, 0), Nat} | true => (o, 1) | false => (o, 2) end).1,
                          (match b: (_) => {Eq()(0, 0), Nat} | true => (Eq/refl(), 1) | false => (Eq/refl(), 2) end).1)
            = Eq/refl();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("o : Eq()(0, 0)", "o : Option(Nat)"),
        ("{Eq()(0, 0), Nat} | true => (o, 1)", "{Option(Nat), Nat} | true => (o, 1)"),
        ("{Eq()(0, 0), Nat} | true => (Eq/refl(), 1) | false => (Eq/refl(), 2)", "{Option(Nat), Nat} | true => (Option/some(0), 1) | false => (Option/some(0), 2)"),
    ])
    .by_elaborator_in("/apart", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

#[test]
fn an_arms_binders_are_opened_at_the_types_their_constructor_gives_them() {
    assert_eq!(
        run(AN_ARMS_BINDERS_ARE_OPENED_AT_THE_TYPES_THEIR_CONSTRUCTOR_GIVES_THEM),
        b"1"
    );
}

#[test]
fn two_binders_of_an_arm_at_a_relevant_type_stay_apart() {
    control(
        r#"
        use /std/{Eq, Nat};

        induct Two : pub Type
        | two(m : Eq()(0, 0), n : Eq()(0, 0))
        end

        let apart(t : Two)
            -> Eq(@(Nat))((match t: (_) => {Eq()(0, 0), Nat} | two(m, n) => (m, 1) end).1,
                          (match t: (_) => {Eq()(0, 0), Nat} | two(m, n) => (n, 1) end).1)
            = Eq/refl();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        (
            "two(m : Eq()(0, 0), n : Eq()(0, 0))",
            "two(m : Nat, n : Nat)",
        ),
        (
            "{Eq()(0, 0), Nat} | two(m, n) => (m, 1)",
            "{Nat, Nat} | two(m, n) => (m, 1)",
        ),
        (
            "{Eq()(0, 0), Nat} | two(m, n) => (n, 1)",
            "{Nat, Nat} | two(m, n) => (n, 1)",
        ),
    ])
    .by_elaborator_in("/apart", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// **What one inhabitant reads of a struct is every field it holds, a hidden one among them.** `S`'s plain field is a unit and its hidden `k` a `Nat`, so two neutrals at `S` stay apart, where read off the plain fields alone `cast` would carry `F` from `S { @k = 1, u = () }` to `S { @k = 0, u = () }`, `True` to `False`. The control is the same pair at a struct whose hidden field is a unit too. `curios-cert`'s `recheck::shape_tests` puts the route to the certifier under this name.
#[test]
fn two_neutrals_at_a_struct_whose_relevant_field_is_hidden_stay_apart() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{Holds, True, False};

        struct S: pub Type { @k: {}, u: {} }

        let F(t: S) -> Prop = Holds(true);

        let cast(r: S, s: S, G: (S) -> Prop, y: G(r)) -> G(s) = y;

        let _held: F(S { @k = (), u = () }) = cast(S { @k = (), u = () }, S { @k = (), u = () }, F, True/qed());

        /std/print("1")
        "#,
        b"1",
    )
    .refused(&[
        ("@k: {}", "@k: Nat"),
        ("Holds(true)", "Holds(match t.k | 1 => true | _ => false end)"),
        ("let _held: F(S { @k = (), u = () }) = cast(S { @k = (), u = () }, S { @k = (), u = () }, F, True/qed())", "let _forged: False = cast(S { @k = 1, u = () }, S { @k = 0, u = () }, F, True/qed())"),
    ])
    .by_elaborator_in("/cast", |error| matches!(error, Error::TypeMismatch { .. }));
}
