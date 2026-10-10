//! A sort its declaration leaves to each caller: what one written `Sort` closes as, what a use states, and where the word is refused.

use {
    super::test_support::sort_parameters,
    crate::tests::{error, run, typecheck},
};

// One definition serves a number and a proof: each use states the sort it stands at, and the proof handed to it weighs nothing the number's use reads.
#[test]
fn one_definition_serves_a_number_and_a_proof() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{True};
        let keep(@A: Sort, a: A) -> A = a;
        let proof: True = keep(True/qed());
        /std/print(Nat/to_str(keep(41) + 1))
        "#;

    assert_eq!(run(source), b"42");
}

// Each written `Sort` is a parameter of its declaration, as each written `Type` is a level of its own; a declaration that writes none leaves none.
#[test]
fn each_written_sort_is_a_parameter_of_its_declaration() {
    let source = r#"
        use /std/{Nat};
        let none(@A: Type, a: A) -> A = a;
        let one(@A: Sort, a: A) -> A = a;
        let two(@A: Sort, @B: Sort, a: A, b: B) -> A = a;
        /std/print("")
        "#;
    let sorts = sort_parameters(source);

    assert_eq!(sorts["/none"], 0, "{sorts:?}");
    assert_eq!(sorts["/one"], 1, "{sorts:?}");
    assert_eq!(sorts["/two"], 2, "{sorts:?}");
}

// Two written sorts the declaration requires equal are one parameter: an equation relates two types of one sort, so `A` and `B` stand at one.
#[test]
fn two_written_sorts_a_declaration_identifies_are_one_parameter() {
    let source = r#"
        use /std/{Nat, Eq};
        let cast(@A: Sort, @B: Sort, e: Eq()(A, B), a: A) -> B = match e | refl() => a end;
        /std/print(Nat/to_str(cast(Eq/refl(), 5)))
        "#;

    assert_eq!(sort_parameters(source)["/cast"], 1);
    assert_eq!(run(source), b"5");
}

// A family over a sort holds a number at one instance and a proof at another, an inductive and a structure alike.
#[test]
fn a_family_over_a_sort_holds_a_number_and_a_proof() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{True};
        induct Maybe(A: Sort): pub Type
        | some(A)
        | none()
        end
        struct Kept(A: Sort): pub Type { value: A }
        let found: Maybe(True) = Maybe/some(True/qed());
        let count(m: Maybe(True)) -> Nat = match m | some(_) => 1 | none() => 0 end;
        let number: Kept(Nat) = Kept { value = 41 };
        let proof: Kept(True) = Kept { value = True/qed() };
        /std/print(Nat/to_str(number.value + count(found)))
        "#;

    assert_eq!(run(source), b"42");
}

// A predicate into a sort its caller states is one transport for data and for proofs, and one induction on a measure for both: the definitions `/std` writes twice.
#[test]
fn a_predicate_into_a_sort_serves_data_and_proofs() {
    let source = r#"
        use /std/{Nat, Eq};
        use /std/Bool/{True, False, Holds};
        let carry(@A: Type, @x: A, @y: A, P: (A) -> Sort, p: Eq()(x, y), v: P(x)) -> P(y) =
            match p | refl() => v end;
        let none_below(m: Nat, k: Nat, below: Holds(m < k), within: Holds(k <= 0)) -> False =
            False/refuted();
        let down(
            @S: Type,
            measure: (S) -> Nat,
            P: (S) -> Sort,
            step: (s: S, ih: (t: S, r: Holds(measure(t) < measure(s))) -> P(t)) -> P(s),
            s: S,
        ) -> P(s) =
            let go(k: Nat, s: S, within: Holds(measure(s) <= k)) -> P(s) =
                match k
                | 0 => step(s, (t, r) => match none_below(measure(t), measure(s), r, within) end)
                | kp + 1 => step(s, (t, r) => go(kp, t, True/proved()))
                end;
            go(measure(s), s, Nat/le/refl(measure(s)));
        let three: Eq()(3, 3) = Eq/refl();
        let data: Nat = carry((n: Nat) => Nat, three, 38);
        let proof: Holds(Nat/eql(3, 3)) =
            carry((n: Nat) => Holds(Nat/eql(n, 3)), three, True/qed());
        let total: Nat = down((n: Nat) => n, (n: Nat) => Nat, (s, ih) => s, 4);
        let holds: True = down((n: Nat) => n, (n: Nat) => True, (s, ih) => True/qed(), 4);
        /std/print(Nat/to_str(data + total))
        "#;

    assert_eq!(run(source), b"42");
}

// A definition that calls itself leaves a sort to its caller as any other does.
#[test]
fn a_recursive_definition_leaves_a_sort_to_its_caller() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{True};
        let after(@A: Sort, n: Nat, a: A) -> A =
            match n
            | 0 => a
            | m + 1 => after(m, a)
            end;
        let proof: True = after(3, True/qed());
        /std/print(Nat/to_str(after(3, 42)))
        "#;

    assert_eq!(run(source), b"42");
}

// A `Sort` the declaration's own terms decide is no caller's to state: refused at the term that decided it, with the sort it can only be.
#[test]
fn a_sort_its_declaration_decides_is_refused_with_what_it_is() {
    let a_type = error(
        r#"
        let f(A: Sort) -> Type = A;
        /std/print("")
        "#,
    );
    assert!(
        a_type.contains("this decides a `Sort`") && a_type.contains("it can only be `Type`"),
        "{a_type}"
    );

    // A proposition with two constructors is matched into a proposition alone, so the match decides the sort its result stands at.
    let a_proposition = error(
        r#"
        induct Two: pub Prop
        | a()
        | b()
        end
        let pick(@A: Sort, t: Two, x: A, y: A) -> A = match t | a() => x | b() => y end;
        /std/print("")
        "#,
    );
    assert!(
        a_proposition.contains("it can only be `Prop`"),
        "{a_proposition}"
    );
}

// The word stands where a caller reads it: a body, a local definition, a concept and a witness each hold one sort, and a family's own sort is `Type` or `Prop`.
#[test]
fn a_sort_is_written_where_a_caller_states_it() {
    for (site, source) in [
        (
            "a body",
            r#"
            use /std/{Nat};
            let g: Nat = ((A: Sort) => 1)(Nat);
            /std/print("")
            "#,
        ),
        (
            "a local definition",
            r#"
            use /std/{Nat};
            let f(n: Nat) -> Nat =
                let id(@A: Sort, a: A) -> A = a;
                id(n);
            /std/print("")
            "#,
        ),
        (
            "a concept",
            r#"
            use /std/{Str};
            concept Named(A: Sort): pub Type {
                name(A) -> Str,
            }
            /std/print("")
            "#,
        ),
        (
            "a witness",
            r#"
            use /std/{Str, Show};
            induct Wrap(A: Type): pub Type
            | wrap(A)
            end
            satisfy Show(Wrap(Sort)) {
                show = (w) => "wrap",
            }
            /std/print("")
            "#,
        ),
    ] {
        let report = error(source);
        assert!(
            report.contains("`Sort` is left to a caller")
                && report.contains(&format!("this one stands in {site}")),
            "{site}: {report}"
        );
    }

    let own_sort = error(
        r#"
        induct Open(A: Type): pub Sort
        | mk(A)
        end
        /std/print("")
        "#,
    );
    assert!(own_sort.contains("Expected keyword 'Prop'"), "{own_sort}");
}

// A tuple type over two sorts each left to a caller is a proposition where both are stated `Prop` and a type otherwise: no one sort is its own, and it is refused where it is written.
#[test]
fn a_tuple_type_over_two_sorts_left_to_a_caller_is_refused() {
    let report = error(
        r#"
        let both(@A: Sort, @B: Sort, a: A, b: B) -> {A, B} = (a, b);
        /std/print("")
        "#,
    );
    assert!(
        report.contains("this tuple type stands over two sorts each left to a caller"),
        "{report}"
    );

    // Over one sort left to a caller and a type, the tuple is a type whatever the caller states.
    let one_sort = r#"
        use /std/{Nat};
        let beside(@A: Sort, a: A, n: Nat) -> {A, Nat} = (a, n);
        /std/print(Nat/to_str(beside(1, 41).1 + 1))
        "#;
    assert_eq!(run(one_sort), b"42");
}

// A value at a sort left to a caller is a proof at one use, so the declaration is checked with it neither: two such values are not one, as two proofs would be, and what computes one terminates, as a proof must.
#[test]
fn a_value_at_a_sort_left_to_a_caller_is_known_neither_proof_nor_data() {
    let not_irrelevant = error(
        r#"
        use /std/{Eq};
        let same(@A: Sort, a: A, b: A) -> Eq()(a, b) = Eq/refl();
        /std/print("")
        "#,
    );
    assert!(not_irrelevant.contains("type mismatch"), "{not_irrelevant}");

    let partial = error(
        r#"
        use /std/{Nat};
        let spin(@A: Sort, n: Nat) -> A = spin(n);
        /std/print("")
        "#,
    );
    assert!(partial.contains("not known to terminate"), "{partial}");
}

// A declaration is checked once at every way of stating the sorts it leaves, so it leaves a bounded number.
#[test]
fn a_declaration_leaves_a_bounded_number_of_sorts() {
    let nine = r#"
        let nine(
            @A: Sort, @B: Sort, @C: Sort, @D: Sort, @E: Sort, @F: Sort, @G: Sort, @H: Sort, @I: Sort,
            a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I,
        ) -> A = a;
        /std/print("")
        "#;
    let report = error(nine);
    assert!(report.contains("leaves 9 sorts to its caller"), "{report}");

    let eight = r#"
        let eight(
            @A: Sort, @B: Sort, @C: Sort, @D: Sort, @E: Sort, @F: Sort, @G: Sort, @H: Sort,
            a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H,
        ) -> A = a;
        /std/print("")
        "#;
    assert_eq!(typecheck(eight), Ok(()));
}
