//! A value standing where data is expected, read past the type parameters it carries: a list beside a proof of its length, a vector, a proposition a stuck `match` computes, a callback, and what is no datum.

use {super::test_support::*, crate::test_support::*, curios_core::Polarity, curios_elab::Error};

// `Box`'s `T` stands in its list, strictly, and as `#ListLen`'s element type inside an index of `Eq` at `Nat`, a datum: one value at every instance of `T`. So `N`, holding a `Box` of itself, is admitted. The control stores `T` left of an arrow in the list.
#[test]
fn a_list_beside_a_proof_of_its_length_is_strict_in_its_element() {
    control(
        r#"
        use /std/{Nat, List, Eq};

        struct Box(T: Type): pub Type { items: List(T), k: Nat, @ok: Eq()(List/length(items), k) }

        induct N: pub Type
        | tip()
        | node(kids: Box(N))
        end

        /std/print("box")
        "#,
        b"box",
    )
    .refused(&[("items: List(T)", "items: List((T) -> Nat)")])
    .by_elaborator_in(
        "/N",
        |error| matches!(error, Error::NotStrictlyPositive { name, .. } if *name == global("/N")),
    );
}

// `/std/Vec` is `Box` with its count a parameter, so a family holds a vector of itself. The control holds a vector of its refutations.
#[test]
fn a_family_holds_a_vector_of_itself() {
    control(
        r#"
        use /std/{Nat, Vec};

        induct Rose: pub Type
        | tip()
        | node(k: Nat, kids: Vec(Rose, k))
        end

        /std/print("rose")
        "#,
        b"rose",
    )
    .refused(&[("kids: Vec(Rose, k)", "kids: Vec((Rose) -> Nat, k)")])
    .by_elaborator_in(
        "/Rose",
        |error| matches!(error, Error::NotStrictlyPositive { name, .. } if *name == global("/Rose")),
    );
}

// `Good` is a proposition a `match` on `M`'s field computes, no instance deciding it: read through its arms, the `some` arm's comparison stands in an index of `Eq` at `Bool`, a datum, so `V` is strict in `M` through its root alone and `T`, holding an `M` of itself, is admitted. The control's arm stores `V` left of an arrow.
#[test]
fn a_proposition_a_stuck_match_computes_over_a_list_is_read_past_its_element() {
    control(
        r#"
        use /std/{Nat, List, Option, Bool};

        let Good(@V: Type, r: Option(List(V))) -> Prop =
            match r | none() => Bool/True | some(l) => Bool/Holds(List/length(l) < 9) end;

        struct M(V: Type): pub Type { root: Option(List(V)), @ok: Good(root) }

        induct T: pub Type
        | leaf()
        | hold(m: M(T))
        end

        /std/print("good")
        "#,
        b"good",
    )
    .refused(&[("root: Option(List(V))", "root: Option(List((V) -> Bool))")])
    .by_elaborator_in(
        "/T",
        |error| matches!(error, Error::NotStrictlyPositive { name, .. } if *name == global("/T")),
    );
}

// A callback passed to a function a datum calls names the type parameter at its binder: the whole call is one value at every instance, the callback with it, so `N`, holding a `C` of itself, is admitted.
#[test]
fn a_predicate_written_with_a_callback_is_read_past_its_binder() {
    let source = r#"
        use /std/{List, Bool};

        struct C(T: Type): pub Type { items: List(T), @ok: Bool/Holds(List/any(items, (x) => true)) }

        induct N: pub Type
        | tip()
        | node(c: C(N))
        end

        /std/print("callback")
        "#;
    assert_eq!(run(source), b"callback");
}

// A parameter answering a type once applied is a type parameter as one declared at a sort is: a datum can apply it but never take apart what it answers. So `F`, standing only inside a datum, the count a recursive `Measure` makes of `k`, whose every call hands it on, reads `Unused`; read as a value parameter, it would read `Mixed`.
#[test]
fn a_parameter_answering_a_type_is_read_past_in_a_datum() {
    let polarities = family_polarities(
        r#"
        use /std/{Nat, Eq};

        pub let Measure(@G: (Nat) -> Type, k: Nat) -> Nat =
            match k | 0 => 0 | m + 1 => Measure(@G, m) end;

        pub struct Fam(F: (Nat) -> Type, n: Nat): pub Type { k: Nat, @ok: Eq()(Measure(@F, k), n) }

        /std/print("")
        "#,
    );
    assert_eq!(polarities["/Fam"], vec![Polarity::Unused, Polarity::Mixed]);
}

// What is no datum keeps today's reading: an index at a type a variable stands for, which a proof could equate with a sort; an index at a family one of whose payloads is a sort; a value parameter, whose value a datum depends on; and a tuple holding a type beside a number, whose number a datum can project and compute on. Each reads `Mixed` where reading it as a datum would read `Unused`.
#[test]
fn what_is_no_datum_is_read_as_before() {
    let polarities = family_polarities(
        r#"
        use /std/{Nat, List, Eq};

        pub struct Variable(T: Type): pub Type { A: Type, f: (@X: Type) -> A, @ok: Eq()(f(@T), f(@T)) }

        pub induct Any: pub Type
        | any(@A: Type, a: A)
        end

        pub struct Sorted(T: Type): pub Type { @ok: Eq()(Any/any(@List(T), []), Any/any(@List(T), [])) }

        pub struct Counted(T: Type, n: Nat): pub Type { items: List(T), @ok: Eq()(List/length(items), n) }

        pub struct Tupled(p: {Type, Nat}): pub Type { @ok: Eq()(p.1, p.1) }

        /std/print("")
        "#,
    );
    assert_eq!(polarities["/Variable"], vec![Polarity::Mixed]);
    assert_eq!(polarities["/Sorted"], vec![Polarity::Mixed]);
    assert_eq!(
        polarities["/Counted"],
        vec![Polarity::Strict, Polarity::Mixed]
    );
    assert_eq!(polarities["/Tupled"], vec![Polarity::Mixed]);
}
