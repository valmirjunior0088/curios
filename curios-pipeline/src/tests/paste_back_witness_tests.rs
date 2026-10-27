//! A type a goal reports reads back under a witness: a local or a table one, reached past a binder or a superclass, the edge resolution finds, and an answer declined where none reads back.

use {super::test_support::*, curios_utilities::fill};

// The programs below hold a dictionary in a structure declared under a premise, `Box`, whose dictionary conversion compares: a report that left out a witness resolution would not put back is a paste that is refused.

#[test]
fn a_type_under_a_local_witness_reads_back() {
    // `Box(A, use x)` under the binder `use Show(A)`: the argument is the binder resolution finds, so it is left out, and the binder, referenced nowhere else, prints unnamed.
    let source = r#"
        use /std/{Nat, Show};
        pub struct Box(A: Type, use Show(A)): pub Type { value: A }
        pub let v(@A: Type, use Show(A), x: Box(A)) -> Nat = 0;
        pub let t: ? = v;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_type_under_a_global_witness_reads_back() {
    // Nothing in scope reaches `Show`, so resolution goes to the table, and coherence makes its answer the witness the report would otherwise have named by its identity.
    let source = r#"
        use /std/{Nat, Show};
        pub struct Box(A: Type, use Show(A)): pub Type { value: A }
        pub let u(x: Box(Nat)) -> Nat = 0;
        pub let t: ? = u;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_table_witness_reads_back_past_a_binder_at_another_argument() {
    // `Box(Nat)` under `use Show(A)`: the binder is a `Show` of `A` and answers no goal at `Nat`, so resolution goes to the table, as it does where no binder stands.
    let source = r#"
        use /std/{Nat, Show};
        pub struct Box(A: Type, use Show(A)): pub Type { value: A }
        pub let v(@A: Type, use Show(A), x: Box(Nat)) -> Nat = 0;
        pub let t: ? = v;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_table_witness_reads_back_past_a_superclass_at_another_argument() {
    // `Ord(A)` reaches `Eql` through its edge, at `A`: no path in scope answers `Eql(Nat)`, and one at `A` itself is still the binder's own.
    for wrapped in ["Nat", "A"] {
        let source = fill!(
            r#"
            use /std/{Nat, Ord};
            use /std/ops/{Eql};
            pub struct Box(A: Type, use Eql(A)): pub Type { value: A }
            pub let v(@A: Type, use Ord(A), x: Box(${wrapped})) -> Nat = 0;
            pub let t: ? = v;
            /std/print("")
        "#,
            wrapped
        );

        reads_back(&source).unwrap_or_else(|error| panic!("{wrapped}: {error}"));
    }
}

#[test]
fn a_table_witness_reads_back_past_a_binder_of_a_type_constructor() {
    // `Io` is a name `/sys` binds to its type former, and `M` a binder: a `Monad` of the one answers no goal at the other, whichever of the two the binder is of.
    for (binders, wrapped) in [("use Monad(M)", "Io"), ("use Monad(M), use Monad(Io)", "M")] {
        let source = fill!(
            r#"
            use /std/{Nat, Monad, Io};
            pub struct Box(M: (Type) -> Type, use Monad(M)): pub Type { value: M(Nat) }
            pub let v(@M: (Type) -> Type, ${binders}, x: Box(${wrapped})) -> Nat = 0;
            pub let t: ? = v;
            /std/print("")
        "#,
            binders,
            wrapped
        );

        reads_back(&source).unwrap_or_else(|error| panic!("{binders}, {wrapped}: {error}"));
    }
}

#[test]
fn an_edge_at_a_type_constructor_is_passed_over_for_the_one_at_the_binder() {
    // `Lift(Io, M)` reaches `Monad` twice, at `Io` and at `M`: the goal at `M` is the second edge's, the first being a `Monad` of a type former.
    let source = r#"
        use /std/{Nat, Monad, Io};
        use /std/Monad/{Lift};
        pub struct Box(M: (Type) -> Type, use Monad(M)): pub Type { value: M(Nat) }
        pub let v(@M: (Type) -> Type, use Lift(Io, M), x: Box(M)) -> Nat = 0;
        pub let t: ? = v;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_binder_at_an_alias_of_a_type_is_read_by_what_the_alias_names() {
    // `Text` is a definition bound to `Str`, so a `Show` of it answers no goal at `Nat`.
    let source = r#"
        use /std/{Nat, Str, Show};
        pub let Text: Type = Str;
        pub struct Box(A: Type, use Show(A)): pub Type { value: A }
        pub let v(use Show(Text), x: Box(Nat)) -> Nat = 0;
        pub let t: ? = v;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_binder_at_an_argument_that_computes_is_passed_over() {
    // `Same(A)` is `A` once it is reduced, which resolution does where the goal stands: the binder is a `Show` of `A` and answers no goal at `Nat`, so the table's witness is left out.
    let source = r#"
        use /std/{Nat, Show};
        pub let Same(A: Type) -> Type = A;
        pub struct Box(A: Type, use Show(A)): pub Type { value: A }
        pub let v(@A: Type, use Show(Same(A)), x: Box(Nat)) -> Nat = 0;
        pub let t: ? = v;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_binder_at_another_application_of_one_former_is_passed_over() {
    // `Show(Option(A))` and the goal `Show(Option(Nat))` are one former at arguments conversion tells apart, so resolution goes to the table.
    let source = r#"
        use /std/{Nat, Show, Option};
        pub struct Box(A: Type, use Show(A)): pub Type { value: A }
        pub let v(@A: Type, use Show(Option(A)), x: Box(Option(Nat))) -> Nat = 0;
        pub let t: ? = v;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_binder_through_an_alias_of_a_concept_is_the_one_resolution_finds() {
    // `MyShow(Nat)` reduces to `Show(Nat)`, so the binder answers the goal at `Nat`, and the literal's dictionary, which is that binder, is left out.
    let source = r#"
        use /std/{Nat, Show};
        pub let MyShow(A: Type) -> Type = Show(A);
        pub struct Box(A: Type, use Show(A)): pub Type { value: A }
        pub let w(use MyShow(Nat)) -> Nat = let y: ? = Box { value = 0 }; 0;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_table_witness_where_an_arm_solves_the_binders_argument_is_declined() {
    // Inside the `refl` arm `A` is `Nat`, so resolution answers a goal at `Nat` with the binder `use Show(A)`, where `x`'s type holds the table's witness: no program writes that type there.
    let source = r#"
        use /std/{Nat, Show, Eq};
        pub struct Box(A: Type, use Show(A)): pub Type { value: A }
        pub let f(@A: Type, use Show(A), e: Eq()(A, Nat), x: Box(Nat)) -> Nat =
            match e | refl() => let y: ? = x; 0 end;
        /std/print("")
    "#;

    assert!(declined(source));
}

#[test]
fn a_table_witness_under_a_binder_through_an_alias_of_its_concept_is_declined() {
    // `MyShow(Nat)` reduces to `Show(Nat)`, so resolution answers a goal at `Nat` with that binder, where `u`'s type holds the table's witness.
    let source = r#"
        use /std/{Nat, Show};
        pub let MyShow(A: Type) -> Type = Show(A);
        pub struct Box(A: Type, use Show(A)): pub Type { value: A }
        pub let u(x: Box(Nat)) -> Nat = 0;
        pub let w(use MyShow(Nat)) -> Nat = let y: ? = u; 0;
        /std/print("")
    "#;

    assert!(declined(source));
}

#[test]
fn a_readers_binder_resolution_passes_over_is_declined() {
    // `b`'s type holds `w`'s binder, and inside `g` resolution answers a goal at `Nat` with `g`'s own: no program names the outer one there.
    let source = r#"
        use /std/{Nat, Show};
        pub struct Box(A: Type, use Show(A)): pub Type { value: A }
        pub let w(use Show(Nat)) -> Nat =
            let b = Box { value = 0 };
            let g(use Show(Nat)) -> Nat = let y: ? = b; 0;
            0;
        /std/print("")
    "#;

    assert!(declined(source));
}

#[test]
fn two_edges_of_one_binder_to_one_concept_are_told_by_what_each_reaches() {
    // `Both(A, B)` reaches `Show` twice, at `A` and at `B`: two paths of one length, of which the goal's is one.
    for wrapped in ["A", "B"] {
        let source = fill!(
            r#"
            use /std/{Nat, Show};
            pub concept Both(A: Type, B: Type): pub Type {
                use Show(A),
                use Show(B),
            }
            pub struct Box(A: Type, use Show(A)): pub Type { value: A }
            pub let v(@A: Type, @B: Type, use Both(A, B), x: Box(${wrapped})) -> Nat = 0;
            pub let t: ? = v;
            /std/print("")
        "#,
            wrapped
        );

        reads_back(&source).unwrap_or_else(|error| panic!("{wrapped}: {error}"));
    }
}

#[test]
fn an_edge_of_the_programs_own_concept_reads_back() {
    // `One` is declared by the program the report is of, and resolution reaches `Show(A)` through its edge where the goal stands.
    let source = r#"
        use /std/{Nat, Show};
        pub concept One(A: Type): pub Type {
            use Show(A),
        }
        pub struct Box(A: Type, use Show(A)): pub Type { value: A }
        pub let v(@A: Type, use One(A), x: Box(A)) -> Nat = 0;
        pub let t: ? = v;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_method_reached_through_the_inner_of_two_binders_is_written_as_its_call() {
    // `Key(K)` and `Key(J)` each reach `Hash` by an edge: the goal at `J` is the inner binder's, and its path the one resolution takes.
    let error = compile(
        r#"
        use /std/{Nat, Bytes, Hash, Eq, Io};
        use /std/Map/{Key};
        pub let outer(@K: Type, use Key(K), k: K) -> Nat =
            let inner(@J: Type, use Key(J), j: J) -> Nat =
                let p: Eq()(Hash/hash(j), x[]) = Eq/refl();
                0;
            0;
        Io/pure(())
        "#,
    )
    .map(|_| ())
    .unwrap_err();

    assert!(error.contains("Hash/hash(j)"), "{error}");
    assert!(!error.contains("as the compiler holds it"), "{error}");
}

#[test]
fn a_method_whose_type_names_an_earlier_field_reads_back() {
    // `Tot/div`'s `@ok` is typed by the concept's own `Ok`, a projection off the wrapper's witness, which prints as the call a program writes.
    let source = r#"
        pub concept Tot(A: Type): pub Type {
            Ok(A) -> Prop,
            div(a: A, b: A, @ok: Ok(b)) -> A,
        }
        pub let t: ? = Tot/div;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_law_over_its_concepts_methods_reads_back() {
    let source = r#"
        use /std/{Eq};
        pub concept Idem(A: Type): pub Type {
            op(A) -> A,
            law(x: A) -> Eq()(op(op(x)), op(x)),
        }
        pub let t: ? = Idem/law;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}
