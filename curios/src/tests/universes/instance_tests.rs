//! Two instances of one name: the levels an occurrence carries, and what two of them compare as.

use {
    super::test_support::{
        definition_variances, family_variances, signatures, universe_parameters,
    },
    crate::tests::{error, run, typecheck, typecheck_within},
    curios_pipeline::DEFAULT_STEP_BUDGET,
};

/// A signature that instantiates `/std/List/zip` twice: the declared result type spells the global at the levels of `A` and `B`, while `zip_len`'s instantiated result — captured into `Eq/trans`'s solved middle term — carries an inlined copy of the same group at the levels of `a`'s and `b`'s `List` types, related to the former only by cumulativity. `same` is an identity whose universe instance is what pulls the two spellings apart.
const TWO_INSTANCES_OF_ZIP: &str = r#"
    use /std/{Nat, List, Eq, Io};
    use /std/Bool/{True, False, Holds};
    let same(@T: Type, l: List(T)) -> List(T) = l;
    let zero_not_succ(@n: Nat, e: Eq()(0, n + 1)) -> False =
        Eq/subst((m: Nat) => Holds(Nat/eql(0, m)), e, True/qed());
    let succ_cancel(@a: Nat, @b: Nat, e: Eq()(a + 1, b + 1)) -> Eq()(a, b) =
        Eq/cong((w: Nat) => w - 1, e);
    pub let zip_len(@A: Type, @B: Type, a: List(A), b: List(B), p: Eq()(List/len(b), List/len(a)))
        -> Eq()(List/len(List/zip(a, b)), List/len(a)) =
        (match a: (l) =>
                (c: List(B), q: Eq()(List/len(c), List/len(l))) -> Eq()(List/len(List/zip(l, c)), List/len(l))
        | [] => (_, _) => Eq/refl()
        | [x, ..xs]; ih =>
            (c, q) =>
                (match c: (d) =>
                        (Eq()(List/len(d), List/len(xs) + 1))
                            -> Eq()(List/len(List/zip([x, ..xs], d)), List/len(xs) + 1)
                | [] => (r) => match zero_not_succ(r) end
                | [_, ..ys] => (r) => Eq/cong((y: Nat) => y + 1, ih(ys, succ_cancel(r)))
                end)(q)
        end)(b, p);
    pub let use_call(@A: Type, @B: Type, n: Nat, a: List(A), b: List(B),
                     ca: Eq()(List/len(same(a)), n), cb: Eq()(List/len(b), n))
        -> Eq()(List/len(List/zip(same(a), b)), n) =
        Eq/trans(zip_len(same(a), b, Eq/trans(cb, Eq/sym(ca))), ca);
    Io/pure(())
    "#;

// A `match` arm is checked at the motive opened on the constructor value the scrutinee is refined to, and that value is what a metavariable in the arm's expected type gets solved to — here `Eq/refl()`'s `@z`, against `Eq()(len(xs), len(xs))` with `xs := L/cons(x, rest)`. The family is universe-polymorphic through its `A: Type`, so the occurrence needs its level instance; built without one, it would zonk into the definition, where the elaborator's own arity check would refuse a program that is plainly well-typed. Both arms are `Eq/refl()` on purpose: no `rec`, no `Eq/cong`, nothing but the refinement itself. Over a *prelude* family the detector differs — the elaborator's arity check knows only the module's own inductives, so a level-less prelude constructor would pass it and the kernel would refuse the definition instead — and `/std` has no `Type`-valued universe-polymorphic indexed family to state a fixture over, so that rung is held by the build: `/std/Eq`'s own `sym`, `trans`, `cong` and `subst` each match on that universe-polymorphic family and answer with `Eq/refl()` in the arm, and `cargo xtask clippy` certifies every `/std` module with the kernel on each build.
#[test]
fn a_refined_scrutinee_carries_the_family_universe_levels() {
    let source = r#"
        use /std/{Nat, Eq};
        induct L(A: Type): pub Type
        | nil()
        | cons(A, L(A))
        end
        let len(xs: L(Nat)) -> Nat =
            match xs
            | nil() => 0
            | cons(_, rest) => len(rest) + 1
            end;
        let len_self(xs: L(Nat)) -> Eq()(len(xs), len(xs)) =
            match xs
            | nil() => Eq/refl()
            | cons(x, rest) => Eq/refl()
            end;
        /std/print(Nat/to_str(len(L/cons(1, L/cons(2, L/nil())))))
        "#;

    assert_eq!(run(source), b"2");
}

// A solution committed while a recursive group elaborates is stamped with the group's instance when the group generalizes, as its signatures and registry entries are. `Eq()(a, a)` and `same2()(a, a)` in a constructor's payload solve the hidden `@A` to the family being declared, spelled from before it had an instance; unstamped, the recorded type of the call filling the hidden list would keep that spelling past generalization, where the proof-totality check refuses an instance with no levels. The written `@Wit` is the control, stamped with the payload it sits in.
#[test]
fn a_solution_naming_its_own_group_is_stamped_with_the_groups_instance() {
    for payload in ["Eq()(a, a)", "same2()(a, a)", "Eq(@Wit)(a, a)"] {
        let source = format!(
            r#"
            use /std/{{Eq}};
            pub let same2(@A : Type) -> (x : A, y : A) -> Prop = (x, y) => Eq(@A)(x, y);
            induct Wit : pub Type
            | base()
            | tied(a : Wit, p : {payload})
            end
            /std/print("declared")
            "#
        );
        assert_eq!(run(&source), b"declared", "{payload}");
    }
}

// `use_call`'s two spellings of `zip` are two instances of one recursive group, related only by `u ≤ x1`, `v ≤ z1` if left apart, which the kernel would refuse. The elaborator identifies the two instances where they meet, so the declared type's `zip` is spelled at the list levels the body already carries and the kernel accepts the program by identity. Nothing merges: the signature keeps the two levels its writer chose, `A`'s and `B`'s, which is what the count pins — the identification chose one spelling for one occurrence rather than making two parameters one. Every other level is an occurrence's, and finalization settles each at the level its argument or its floor gives it (`UniverseSolver::finalize`): `List(A)` at `A`'s, `Eq()(List/len(b), n)` at zero, which is why the count is two.
#[test]
fn a_signature_instantiating_one_recursive_definition_twice_certifies_with_its_levels_identified() {
    typecheck_within(DEFAULT_STEP_BUDGET, TWO_INSTANCES_OF_ZIP)
        .expect("both checkers accept the program");

    let parameters = universe_parameters(TWO_INSTANCES_OF_ZIP);
    assert_eq!(
        parameters.get("/use_call"),
        Some(&2),
        "identifying the two spellings changed how polymorphic the signature is: {parameters:?}",
    );
}

// **A family's payload is read once the witness it resolves through has elaborated, so its vector is its final telescopes'.** `Holder`'s level only types its parameter, and `Sized/Carrier(@Nat)` resolves through a witness written after the family: the family is elaborated after the witness, its payload is solved when its group finalizes, and the level reads irrelevant. A witness whose carrier is built from the family has no such order, and the three are refused together by name, an `induct` and a `struct` alike — where an `induct` once kept the payload unsolved, read every level invariant, and was accepted, and a `struct` in its place was refused for a witness that was written right there.
#[test]
fn a_family_whose_payload_resolves_through_a_later_witness_reads_its_final_telescopes() {
    let program = |family: &str, carrier: &str| {
        format!(
            r#"
            use /std/{{Nat, Str}};
            pub concept Sized(T: Type): pub Type {{ Carrier: Type }}
            {family}
            pub struct Later: pub Type {{ inner: Holder(Str) }}
            satisfy Sized(Nat) {{ Carrier = {carrier} }}
            /std/print("settled")
            "#
        )
    };
    let induct = "pub induct Holder(A: Type): pub Type | hold(a: A, held: Sized/Carrier(@Nat)) end";
    let struct_ = "pub struct Holder(A: Type): pub Type { a: A, held: Sized/Carrier(@Nat) }";

    let ordered = family_variances(&program(induct, "Str"));
    assert_eq!(
        ordered.get("/Holder").map(String::as_str),
        Some("*"),
        "a level that only types a parameter stopped reading irrelevant: {ordered:?}",
    );
    // The struct is accepted as the inductive is; only an inductive family's vector is listed.
    family_variances(&program(struct_, "Str"));

    for family in [induct, struct_] {
        let report = error(&program(family, "Later"));
        assert!(
            report.contains("need one another")
                && report.contains("'Holder'")
                && report.contains("'Later'")
                && report.contains("the witness of Sized(Nat)"),
            "expected the family, its payload's type and the witness refused as one cycle:\n{report}"
        );
    }
}

// **A bare former passed as a family is held at one instance.** `through(Io, …)` checks both arguments against `Io` at the instance it was passed at, and the refusal reads as that instance being committed before `Io` unfolds to a former that carries no level. Eta-expanded, `F(Nat)` is a redex and both sides are reduced, so the same program is accepted. `/sys/List` and an alias of `Io` are refused alike.
#[test]
fn a_bare_former_passed_as_a_family_is_held_at_one_instance() {
    let through = |declarations: &str, family: &str, small: &str, large: &str| {
        format!(
            r#"
            use /std/{{Nat, Io, List}};
            {declarations}
            let through(F: (Type) -> Type, x: F(Nat), y: F(Type)) -> Nat = 0;
            let probe(n: Nat) -> Nat = through({family}, {small}, {large});
            /std/print("held")
            "#
        )
    };
    let io = "let small(n: Nat) -> Io(Nat) = Io/pure(n);";
    let list = "let small(n: Nat) -> List(Nat) = [n];";
    let alias = "let Act(A: Type) -> Type = Io(A);
        let small(n: Nat) -> Act(Nat) = Io/pure(n);
        let large(n: Nat) -> Act(Type) = Io/pure(Nat);";

    for bare in [
        through(io, "Io", "small(n)", "Io/pure(Nat)"),
        through(list, "List", "small(n)", "[Nat]"),
        through(alias, "Act", "small(n)", "large(n)"),
    ] {
        let message = typecheck(&bare).expect_err("a bare former is held at one instance");
        assert!(message.contains("strictly below itself"), "got: {message}");
    }

    for expanded in [
        through(io, "(A) => Io(A)", "small(n)", "Io/pure(Nat)"),
        through(list, "(A) => List(A)", "small(n)", "[Nat]"),
    ] {
        assert_eq!(typecheck(&expanded), Ok(()));
    }
}

// **An ordinary function passed bare is held at one instance, as a former is.** `through(const_nat, …)` states `p` over `G(Nat, 0)`, which is `const_nat(Nat, 0)` at the instance `G` was passed at, a level above zero since `G` is applied to `Type` as well. `witness()` proves it over `const_nat` at zero, and the two spellings, apart in a level alone, are committed equal before `const_nat` unfolds to a `0` that holds none. Eta-expanded, `G(Nat, 0)` is a redex and both sides are reduced. The roadmap's *A universe level settled before its evidence is in* owns the refusal.
#[test]
fn an_ordinary_function_passed_bare_is_held_at_one_instance() {
    let through = |function: &str| {
        format!(
            r#"
            use /std/{{Nat, Eq}};
            let const_nat(A: Type, x: A) -> Nat = 0;
            let witness() -> Eq()(const_nat(Nat, 0), const_nat(Nat, 0)) = Eq/refl();
            let through(G: (A: Type, x: A) -> Nat, p: Eq()(G(Nat, 0), G(Nat, 0)), q: Eq()(G(Type, Nat), G(Type, Nat))) -> Nat = 0;
            let probe() -> Nat = through({function}, witness(), Eq/refl());
            /std/print("held")
            "#
        )
    };

    let message =
        typecheck(&through("const_nat")).expect_err("a bare function is held at one instance");
    assert!(message.contains("strictly below itself"), "got: {message}");

    assert_eq!(typecheck(&through("(A, x) => const_nat(A, x)")), Ok(()));
}

// **Both checkers hold the pair a bare former is refused on to be one type.** `first`'s body is `x.0`, at `Io` over `Nat` at the level `Both` passes its family at, one above zero since `Both` applies it to `Type` as well, and its declared type is `Io(Nat)` at zero. The body's type arrives as a projection's, a spelling apart from the declared one in more than levels, so both sides are reduced, nothing is committed, and the kernel certifies the definition by unfolding `Io` on both sides.
#[test]
fn two_instances_of_a_former_convert_where_their_spellings_differ_in_more_than_levels() {
    let source = r#"
        use /std/{Nat, Bool, Io};
        let small(n: Nat) -> Io(Nat) = Io/pure(n);
        let Both(F: (Type) -> Type) -> Type = {F(Nat), F(Type)};
        let first(x: Both(Io)) -> Io(Nat) = x.0;
        pub let pick(b: Bool, n: Nat, x: Both(Io)) -> Io(Nat) =
            match b | true => small(n) | false => first(x) end;
        /std/print("picked")
        "#;

    let signatures = signatures(source);
    let first = &signatures["/first"];
    assert!(
        first.contains("Io.{1}") && first.contains("Io.{0}"),
        "the two instances no longer sit at two decided levels, so the program holds nothing: {first}",
    );
    assert_eq!(run(source), b"picked");
}

// **A definition carries which of its levels its value holds.** Its body is read at its binders, so a parameter's annotation is unread and what is left is what unfolding two instances would compare: `const_nat` reduces to a numeral and `Act` to a former the level is gone from, each irrelevant in its one level, where `Value`'s former builds a node its family is invariant in. `Curried` answers a lambda, and the level its binder is annotated at is held beside the unread one of `A`: two lambdas met at no function type are compared binder by binder.
#[test]
fn a_definition_carries_the_levels_its_value_holds() {
    let source = r#"
        use /std/{Nat, Str, Io, Result};
        pub let const_nat(A: Type, x: A) -> Nat = 0;
        pub let Act(A: Type) -> Type = Io(A);
        pub struct Value: pub Type { A: Type, read: (Str) -> Result(Str, A) }
        pub let Curried(A: Type) -> (Type) -> Type = (B: Type) => A;
        pub let plain(n: Nat) -> Nat = n;
        /std/print("carried")
        "#;

    let vectors = definition_variances(source);

    assert_eq!(vectors["/const_nat"], "*", "{vectors:?}");
    assert_eq!(vectors["/Act"], "*", "{vectors:?}");
    assert_eq!(vectors["/Value"], "=", "{vectors:?}");
    assert_eq!(vectors["/plain"], "", "{vectors:?}");
    let curried = &vectors["/Curried"];
    assert!(
        curried.contains('*') && curried.contains('='),
        "a lambda left in the reduct is read with its binder, and its parameter without: {vectors:?}",
    );
}
