//! What a proposition may carry and what is one: no informative field, method or superclass, no type field or payload over its own sort, and the records, functions and lists that are or are not propositions.

use {super::test_support::*, crate::tests::test_support::*, curios_elab::Error};

// `Prop` non-informativeness, which is what makes proof irrelevance safe: a proposition whose inhabitants differ observably is not a subsingleton, so identifying them would identify the data they carry. A hidden field is a field like any other, whatever position a literal gives it. The controls carry a proof, and the hidden field in a `Type`.
#[test]
fn a_proposition_may_not_carry_informative_fields() {
    control(
        r#"
        use /std/{Nat, Eq};

        struct Bad : pub Prop {
            value : Eq()(0, 0)
        }

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("{Nat, Eq}", "{Nat}"),
        ("value : Eq()(0, 0)", "value : Nat"),
    ])
    .by_elaborator_in("/Bad", |error| {
        matches!(error, Error::InformativePropStruct { name, field, .. } if *name == global("/Bad") && field == "value")
    });
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{Holds};

        struct Hid : pub Type { @n : Nat, Holds(n == n) }

        let read(h : Hid) -> Nat = h.n;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("struct Hid : pub Type", "struct Hid : pub Prop"),
    ])
    .by_elaborator_in("/Hid", |error| {
        matches!(error, Error::InformativePropStruct { name, field, .. } if *name == global("/Hid") && field == "n")
    });
}

// A concept is a structure, so the same rule must reach a `Prop`-sorted concept whose method returns data. Worth its own fixture: the concept path generates its record entry rather than declaring it, so it could regress independently. The control's method returns a proof.
#[test]
fn a_proposition_concept_may_not_carry_informative_methods() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        concept Bad(A : Type) : pub Prop {
            get(A) -> True,
        }

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("get(A) -> True,", "get(A) -> Nat,")])
    .by_elaborator_in("/Bad", |error| {
        matches!(error, Error::InformativePropStruct { .. })
    });
}

// A superclass edge is a field of the generated record too, one no method path names: two witnesses of a `Prop` concept over a data concept would be one value by irrelevance and hold two dictionaries a projection tells apart. The control's concept is a `Type`, whose superclass may carry data.
#[test]
fn a_proposition_concept_may_not_carry_an_informative_superclass() {
    control(
        r#"
        use /std/{Show};

        concept Carry(A : Type) : pub Type {
            use Show(A),
        }

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(": pub Type {", ": pub Prop {")])
    .by_elaborator_in("/Carry", |error| {
        matches!(error, Error::InformativePropStruct { .. })
    });
}

// `Prop` non-informativeness at the same half, and the shorter route: a field whose type is a universe holds a type as data, and a projection reads it back out meeting no elimination guard at all, so two convertible propositions hand `.0` two different types.
//
// Both gates ask `carries_information`. Its control in `curios-cert`, `a_proposition_may_carry_a_proof`, keeps a genuine proof field legal, and types that field as a `Prop`-sorted family rather than as the universe `Prop`: typed at the universe, it would assert the admitted shape and pass for the wrong reason. The control's field is a proof.
#[test]
fn a_proposition_may_not_carry_a_type_field() {
    control(
        r#"
        use /std/{Eq};

        struct Bad : pub Prop {
            carried : Eq()(0, 0)
        }

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("carried : Eq()(0, 0)", "carried : Type")])
    .by_elaborator_in("/Bad", |error| {
        matches!(error, Error::InformativePropStruct { .. })
    });
}

// Definitional proof irrelevance, at the premise the rule is stated over rather than at the rule: *which* types are propositions. `Prop` is the type of propositions, so anything admitted at `Prop` is one as far as every later rule is concerned, and irrelevance then identifies its inhabitants without looking at them.
//
// A list of proofs is not a proposition however little its elements carry — a list has a length, so two of them are distinguishable where their elements are not, and `Sort::of` says exactly that in both checkers. The *typing* rule for a parameterized intrinsic former follows that one rule rather than reporting the element's sort as the former's, which would infer such a list at `Prop` on both sides — and nothing here needs the former written in a `Prop` position: `@X : Prop` is solved by unification, and the solution is the reduced `ListType` node. The list is one of `Option(True)`, a type that holds a proof.
//
// From there every step is the ordinary machinery. `all_equal` is sound and stays accepted below — its conclusion is discharged because irrelevance identifies any two inhabitants of the proposition `X`. Instantiating `X` at the list's type would equate a one-element list with the empty one; congruence through `List/len` carries that to `1` and `0`, and a transport takes `()` into `False`.
//
// Both controls are load-bearing, because the two ways to "close" this without fixing it are to stop believing `Prop` and to stop believing `List`. Irrelevance must still identify two genuinely different inhabitants of a real proposition, and a list of evidence must still be an ordinary list with a length. Both are run: the first has both checkers accept the lemma and its instantiation, and its call, whose every argument erases, reaches the runtime as the program it is. This test's own control applies `all_equal` at the proposition `True`.
#[test]
fn a_list_of_proofs_is_not_a_proposition() {
    control(
        r#"
        use /std/{List, Option};
        use /std/Bool/{True};

        induct Same(@P: Prop): (P, P) -> pub Prop | same(@p: P): (p, p) end

        let all_equal(@X : Prop, x : X, y : X) -> Same()(x, y) =
            Same/same();

        let one : List(Option(True)) = [Option/some(True/qed())];
        let none : List(Option(True)) = [];

        let bad : Same()(True/qed(), True/qed()) =
            all_equal(True/qed(), True/qed());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("let bad : Same()(True/qed(), True/qed()) =\n            all_equal(True/qed(), True/qed());", "let bad : Same()(one, none) =\n            all_equal(one, none);"),
    ])
    .by_elaborator_in("/bad", |error| {
        matches!(error, Error::ImplicitAtOtherSort { binder: Some(binder), proposition: false, .. } if binder == "P")
    });
}

#[test]
fn irrelevance_still_identifies_a_propositions_inhabitants() {
    assert_eq!(
        run(IRRELEVANCE_STILL_IDENTIFIES_A_PROPOSITIONS_INHABITANTS),
        b"1"
    );
}

#[test]
fn a_list_of_evidence_is_still_a_list() {
    assert_eq!(run(A_LIST_OF_EVIDENCE_IS_STILL_A_LIST), b"1");
}

// What a proposition may carry. Both of its rules are *accepting* — a Π into a proposition is a proposition whatever it quantifies over, and a record of propositions is one — so they widen what counts as a proof, and everything irrelevance and erasure do downstream turns on that verdict.
//
// The pair below is `tuple_sort`. A record is `Prop`-sorted only where every component pushed no level, which is what makes an anonymous Σ non-informative *by formation* where a declared `struct` needs `check_non_informative` to make it so — and the two predicates are the same one, so the declaration gate is what these fixtures read the formation verdict through. The empty record is the carve-out and has to fall the other way: `{}` is the unit type an effect returns, so calling it a proposition would erase a value the runtime needs.
#[test]
fn a_record_of_propositions_is_a_proposition() {
    assert_eq!(run(A_RECORD_OF_PROPOSITIONS_IS_A_PROPOSITION), b"1");
}

// The control is a record of propositions.
#[test]
fn the_empty_record_is_not_a_proposition() {
    control(
        r#"
        use /std/{Nat, Eq};

        struct Holder : pub Prop {
            field : {Eq()(0, 0), Eq()(1, 1)}
        }

        /std/print(Nat/to_str(1))
        "#,
        b"1",
    )
    .refused(&[("field : {Eq()(0, 0), Eq()(1, 1)}", "field : {}")])
    .by_elaborator_in("/Holder", |error| {
        matches!(error, Error::InformativePropStruct { .. })
    });
}

// `func_sort`: a function into a proposition is a proposition whatever it quantifies over, at the join of its domains' levels and its own, so quantifying over a universe yields a proposition a level above it, never one among those it ranges over. The rule's soundness is not its own all the same: a family that is its own double function space is kept out by positivity — see the positivity fixture below.
#[test]
fn a_function_into_a_proposition_is_a_proposition() {
    assert_eq!(run(A_FUNCTION_INTO_A_PROPOSITION_IS_A_PROPOSITION), b"1");
}

// The control's function lands in a proposition.
#[test]
fn a_function_into_a_type_is_not_a_proposition() {
    control(
        r#"
        use /std/{Nat, Eq};

        struct Holder : pub Prop {
            field : (A : Type) -> Eq()(0, 0)
        }

        /std/print(Nat/to_str(1))
        "#,
        b"1",
    )
    .refused(&[(
        "field : (A : Type) -> Eq()(0, 0)",
        "field : (A : Type) -> A",
    )])
    .by_elaborator_in("/Holder", |error| {
        matches!(error, Error::InformativePropStruct { .. })
    });
}

// A proposition is known by its head: both checkers answer `Prop` for an application of a declaration whose codomain is one, with no argument reduced, and conversion's irrelevance gate reads that answer. `curios-cert`'s `classification_tests` hold the answer and that nothing is run for it; what makes the answer true is checked where the declaration's body is, so it is pinned here. `P` below says `Prop` and hands a `Nat` back at `false`, so `Sort::of(P(false))` would answer `Prop` for a type whose two inhabitants a program tells apart, and irrelevance would hold `0` and `7` one value there. The control, which keeps the refusal from being one of the head rule itself: where every arm is a proposition the declaration stands, and the answer the head gives is used — `p` and `q` convert at `P(false)` by irrelevance alone, the gate reading the head and reducing nothing.
#[test]
fn a_declaration_into_prop_may_not_hand_a_relevant_value_back() {
    control(
        r#"
        use /std/{Nat, Bool, Eq};
        use /std/Bool/{True};

        pub let P(b : Bool) -> Prop =
            match b: (_) => Prop
            | true => True
            | false => True
            end;

        pub let one(p : P(false), q : P(false)) -> Eq()(p, q) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#,
        b"1",
    )
    .refused(&[
        ("| false => True", "| false => Nat"),
        (
            "        pub let one(p : P(false), q : P(false)) -> Eq()(p, q) = Eq/refl();\n\n",
            "",
        ),
    ])
    .by_elaborator_in(
        "/P",
        |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "Prop"),
    );
}

// A proposition is sized as a type is. Unsized, `Bad` held a payload quantifying over the sort it sat in, the payload applied at `Bad` handed back the value it was given, and the application read as a descent: the ticket in `xboard`'s totality part. The control applies the payload at a proposition below.
#[test]
fn a_payload_quantifying_over_its_own_propositions_sort_is_refused() {
    control(
        r#"
        use /std/Bool/{True, False};

        induct Bad: Prop
        | c(f: (A: Prop) -> (A) -> A)
        end

        let bad(b: Bad, p: True) -> True = match b | c(f) => f(True)(p) end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "let bad(b: Bad, p: True) -> True = match b | c(f) => f(True)(p) end;",
        "let bad(b: Bad) -> False = match b | c(f) => bad(f(Bad)(b)) end;",
    )])
    .by_elaborator_in("/bad", |error| {
        matches!(error, Error::UniverseInconsistency { .. })
    });
}

// The neighbouring route in `Type`, which no exemption opens: a payload an index pins counts at its level like any other. The control applies the payload at a type below.
#[test]
fn a_large_payload_pinned_by_an_index_is_refused() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{False};

        induct I: (f: (A: Type) -> (A) -> A) -> Type
        | c(@f: (A: Type) -> (A) -> A): (f)
        end

        let bad(f: (A: Type) -> (A) -> A, b: I(f)) -> Nat =
            match b | c(@g) => g(Nat)(0) end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "-> Nat =\n            match b | c(@g) => g(Nat)(0) end;",
        "-> False =\n            match b | c(@g) => bad(g, g(I(g))(b)) end;",
    )])
    .by_elaborator_in("/bad", |error| {
        matches!(error, Error::UniverseInconsistency { .. })
    });
}

// The neighbouring route through a data family: its payload is a proof of a proposition one level up, which counts at that level, so the family cannot be squashed into the propositions the payload ranges over. The control applies the payload at a proposition below.
#[test]
fn a_proof_payload_of_a_data_family_counts_at_its_level() {
    control(
        r#"
        use /std/Bool/{True, False};

        induct T: Type
        | c(f: (A: Prop) -> (A) -> A)
        end

        induct Sq(A: Type): Prop
        | sq(a: A)
        end

        let bad(t: T, p: True) -> True =
            match t | c(f) => f(True)(p) end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("let bad(t: T, p: True) -> True =\n            match t | c(f) => f(True)(p) end;", "let bad(t: T) -> False =\n            match t | c(f) => match f(Sq(T))(Sq/sq(t)) | sq(u) => bad(u) end end;"),
    ])
    .by_elaborator_in("/bad", |error| {
        matches!(error, Error::UniverseInconsistency { .. })
    });
}

// The control: the family is admitted at the level its payload fits under, and the payload is applied at a proposition below it.
#[test]
fn a_proposition_holding_a_polymorphic_proof_is_admitted_at_its_level() {
    assert_eq!(
        run(A_PROPOSITION_HOLDING_A_POLYMORPHIC_PROOF_IS_ADMITTED),
        b"1"
    );
}

// The same sizing through a record rather than an `induct`: a `Prop`-sorted `struct` holding `(A: Prop) -> (A) -> A` sits at the level of the propositions its field ranges over, so projecting the field and applying it at the struct itself asks that `Prop` to be strictly below itself. The `induct` neighbours above refuse the declaration where its constructor is sized; the struct is admitted and the paradox is stopped at the self-instantiation, through `tuple_sort`'s field sizing rather than constructor sizing. The control is the same field applied at a proposition below it.
#[test]
fn a_struct_proposition_quantifying_over_its_own_sort_is_refused() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True, False};

        struct Bad: Prop
        { f: (A: Prop) -> (A) -> A }

        let through(b: Bad, p: True) -> True = b.0(True)(p);

        /std/print(Nat/to_str(1))
        "#,
        b"1",
    )
    .refused(&[(
        "let through(b: Bad, p: True) -> True = b.0(True)(p);",
        "let bad(b: Bad) -> False = bad(b.0(Bad)(b));",
    )])
    .by_elaborator_in("/bad", |error| {
        matches!(error, Error::UniverseInconsistency { .. })
    });
}
