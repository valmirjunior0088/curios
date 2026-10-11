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

// A `match` arm is checked at the motive opened on the constructor value the scrutinee is refined to, and that value is what a metavariable in the arm's expected type gets solved to — here `Eq/refl()`'s `@z`, against `Eq()(len(xs), len(xs))` with `xs := L/cons(x, rest)`. The family is universe-polymorphic through its `A: Type`, so the occurrence needs its level instance; built without one, it would zonk into the definition, where the elaborator's own arity check would refuse a program that is plainly well-typed. Both arms are `Eq/refl()` on purpose: no `rec`, no `Eq/cong`, nothing but the refinement itself. Over a *prelude* family the detector differs — the elaborator's arity check knows only the module's own inductives, so a level-less prelude constructor would pass it and the kernel would refuse the definition instead — and `/std` has no `Type`-valued universe-polymorphic indexed family to state a fixture over, so that rung is held by the build: `/std/Eq`'s own `sym`, `trans`, `cong` and `subst` each match on that universe-polymorphic family and answer with `Eq/refl()` in the arm, and `cargo xtask workspace lint` certifies every `/std` module with the kernel on each build.
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

// `use_call`'s two spellings of `zip` are two instances of one recursive group, related only by `u ≤ x1`, `v ≤ z1` if left apart, which the kernel would refuse. The elaborator identifies the two instances where they meet, so the declared type's `zip` is spelled at the levels the body already carries and the kernel accepts the program by identity. Nothing merges: the signature keeps the two levels its writer chose, `A`'s and `B`'s, and the two of the instance of `zip` its result names, which is what the count pins — the identification chose one spelling for one occurrence rather than making two parameters one. Two calls of a recursive definition are compared at every level, so that instance's levels stand where conversion compares them and are the signature's callers' to choose. Every other level is an occurrence's at a position its family is irrelevant in, and finalization settles each at the level its argument or its floor gives it (`UniverseSolver::finalize`): `List(A)` at `A`'s, `Eq()(List/len(b), n)` at zero, which is why the count is four.
#[test]
fn a_signature_instantiating_one_recursive_definition_twice_certifies_with_its_levels_identified() {
    typecheck_within(DEFAULT_STEP_BUDGET, TWO_INSTANCES_OF_ZIP)
        .expect("both checkers accept the program");

    let parameters = universe_parameters(TWO_INSTANCES_OF_ZIP);
    assert_eq!(
        parameters.get("/use_call"),
        Some(&4),
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

// **A bare former passed as a family takes an instance at each payload.** `through(Io, …)` checks both arguments against `Io` at the instance it was passed at, a level above zero since `F` is applied to `Type` as well, and `small(n)` is `Io` at zero. `Io`'s value holds no level, so the two spellings are one term and nothing is required of the pair, which is what the eta-expanded family always was. `/sys/List` and an alias of `Io` read alike.
#[test]
fn a_bare_former_passed_as_a_family_takes_an_instance_at_each_payload() {
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

    for family in [
        through(io, "Io", "small(n)", "Io/pure(Nat)"),
        through(list, "List", "small(n)", "[Nat]"),
        through(alias, "Act", "small(n)", "large(n)"),
        through(io, "(A) => Io(A)", "small(n)", "Io/pure(Nat)"),
        through(list, "(A) => List(A)", "small(n)", "[Nat]"),
    ] {
        assert_eq!(typecheck(&family), Ok(()), "{family}");
    }
}

// **An ordinary function passed bare takes an instance at each argument, as a former does.** `through(const_nat, …)` states `p` over `G(Nat, 0)`, which is `const_nat(Nat, 0)` at the instance `G` was passed at, a level above zero since `G` is applied to `Type` as well. `witness()` proves it over `const_nat` at zero, and the two spellings are apart in a level `const_nat`'s value, a `0`, does not hold: one term, as the eta-expanded function's two redexes always were.
#[test]
fn an_ordinary_function_passed_bare_takes_an_instance_at_each_argument() {
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

    assert_eq!(typecheck(&through("const_nat")), Ok(()));
    assert_eq!(typecheck(&through("(A, x) => const_nat(A, x)")), Ok(()));
}

// **Both checkers hold two instances of a former apart in its level to be one type.** `first`'s body is `x.0`, at `Io` over `Nat` at the level `Both` passes its family at, one above the level of the `Type` `Both` applies it to as well, which is `Both`'s caller's to choose, and its declared type is `Io(Nat)` at zero. The body's type arrives as a projection's, a spelling apart from the declared one in more than levels, so both sides are reduced, nothing is committed, and the kernel certifies the definition by unfolding `Io` on both sides.
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
        first.contains("Io.{Type; u+1}") && first.contains("Io.{Type; 0}"),
        "the two instances no longer sit at two levels no instance makes one, so the program holds nothing: {first}",
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

// **A case equation read at another instance holds the two to one.** `even_at(U, n)` is stuck on `n`, and each `U` written mints an instance of its own: the scrutinee's closes at zero and the arm's, which holds a `Type`, at one. The arm's proof rests on the equation the scrutinee recorded, so the two are one instance or the arm proves nothing. The elaborator commits them equal where it reads the equation, and the kernel's key, which keeps its levels, answers the term it is handed. With `Nat` for the inhabitant the two close together unasked, which is the control.
#[test]
fn a_case_equation_read_at_another_instance_holds_the_two_to_one() {
    let program = |inhabitant: &str| {
        format!(
            r#"
            use /std/{{Nat, Bool, Eq}};
            let U: Type = Type;
            let even_at(A: Type, n: Nat) -> Bool =
                match n
                | 0 => true
                | m + 1 => Bool/not(even_at(A, m))
                end;
            let needs(T: Type, inhabitant: T, n: Nat, proof: Eq()(even_at(T, n), true)) -> Nat = n;
            pub let apart(n: Nat) -> Nat =
                match even_at(U, n)
                | true => needs(U, {inhabitant}, n, Eq/refl())
                | false => 0
                end;
            /std/print(Nat/to_str(apart(4)))
            "#
        )
    };

    for inhabitant in ["Nat", "Type"] {
        assert_eq!(run(&program(inhabitant)), b"4", "{inhabitant}");
    }
}

/// A recursive definition over a family whose level only sizes its parameter: nothing ties the level a written call takes to another call's.
const EVEN: &str = r#"
    use /std/{Bool, Io};
    use /std/Bool/{Holds};
    induct Tree(K: Type): Type | tip(K) | join(Tree(K), Tree(K)) end
    let even(@K: Type, t: Tree(K)) -> Bool =
        match t | tip(_) => true | join(l, r) => even(l) && even(r) end;
    "#;

// **A statement is one with itself written again under an operator.** The type `held` is declared at spells its implicit arguments and its operator's witness as the metavariables elaboration solved, and the result type spells them substituted. Read as solved, the two are apart in levels alone, which the shortcut holds one; read as they stand, they reduce to an operation whose operands nothing pairs.
#[test]
fn a_statement_is_one_with_itself_written_again_under_an_operator() {
    for statement in [
        "Holds(even(l) && even(r))",
        "Holds(even(l) || even(r))",
        "Holds(even(l) != even(r))",
        "Holds(even(l) == even(r))",
    ] {
        let source = format!(
            "{EVEN}\n let p(@K: Type, l: Tree(K), r: Tree(K), held: {statement}) -> {statement} = held;\n Io/pure(())"
        );
        typecheck(&source).unwrap_or_else(|error| panic!("`{statement}`: {error}"));
    }

    // An alias of the statement, and a function that hands its proof back.
    let aliased = format!(
        "{EVEN}\n let Both(@K: Type, l: Tree(K), r: Tree(K)) -> Prop = Holds(even(l) && even(r));\n let p(@K: Type, l: Tree(K), r: Tree(K), held: Both(l, r)) -> Both(l, r) = held;\n Io/pure(())"
    );
    typecheck(&aliased).expect("an alias of the statement");
    let handed = format!(
        "{EVEN}\n let same(@P: Prop, proof: P) -> P = proof;\n let p(@K: Type, l: Tree(K), r: Tree(K), held: Holds(even(l) && even(r))) -> Holds(even(l) && even(r)) = same(held);\n Io/pure(())"
    );
    typecheck(&handed).expect("the proof handed back by a function");
}
