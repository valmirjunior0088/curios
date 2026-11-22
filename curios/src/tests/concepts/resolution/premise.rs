//! What a premise and a witness's declaration may be: a premise resolved recursively or through a constant's edge, and refused where it is no smaller than its head, at a proposition, or at a concept nothing mentions; a witness's head computed, keyed under a binder or applying no concept.

use {crate::tests::test_support::*, curios_common::fill};

// A premised witness: `Show(List(A))`'s needs a `Show(A)` to show its elements. The resolver instantiates its telescope — `@A := ?B` with premise goal `Show(?B)` — unifies `Show(List(?B)) ≡ Show(List(Nat))` to solve `?B := Nat`, then resolves the premise to the `Show(Nat)` witness.
#[test]
fn premised_witness_resolves_recursively() {
    let source = r#"
        use /std/{Nat, Str, List};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        satisfy (@A : Type, use Show(A)) => Show(List(A)) {
            show(l) =
                List/fold(l, "[", (x, acc) => Str/concat(acc, Show/show(x)))
        }
        let l : List(Nat) = [1, 2, 3];
        /std/print(Show/show(l))
        "#;

    assert_eq!(run(source), b"[123");
}

// A witness is known by its key before anything elaborates, so its signature spells it: a head that only computation reaches is refused where the witness is declared, with the key it would have registered under and what to write.
#[test]
fn a_witness_whose_head_is_computed_is_refused_at_its_declaration() {
    let source = r#"
        use /std/{Nat, Bool, Show, Str};
        let Chosen(flag: Bool) -> Type = match flag | true => Nat | false => Bool end;
        satisfy Show(Chosen(true)) { show(n) = "chosen" }
        /std/print("no")
        "#;

    let report = error(source);
    assert!(
        report.contains(
            "this witness of `Show` is for the head `Nat`, which its signature does not spell"
        ),
        "expected the refusal to name the key the signature computes:\n{report}"
    );
    assert!(
        report.contains("write `Nat` where the signature computes it"),
        "expected the way out:\n{report}"
    );
}

// A definition applied under a function's binder is not read through, so the head it names is the definition itself, which keys no declared type: the witness is refused as unkeyable, not as one keyed at the definition with advice to write what is already written. A name bound to the type former is a definition alike.
#[test]
fn a_witness_keyed_through_a_definition_under_its_binder_cannot_be_keyed() {
    for through in [
        "let through(S: Type, A: Type) -> Type = Box(S, A);",
        "let through : (Type, Type) -> Type = Box;",
    ] {
        let source = fill!(
            r#"
            use /std/{Nat, Str};
            induct Box(S: Type, A: Type): Type | box(A) end
            concept Tag(F: (Type) -> Type): Type { name: Nat }
            ${through}
            satisfy (@S: Type) => Tag((A: Type) => through(S, A)) { name = 1 }
            /std/print("no")
            "#,
            through,
        );

        let report = error(&source);
        assert!(
            report.contains("unkeyed witness"),
            "expected the unkeyable report:\n{report}"
        );
        assert!(
            !report.contains("head 'through'") && !report.contains("write 'through'"),
            "expected no key at the definition:\n{report}"
        );
    }
}

// A premise may name a constant beside a binder, `Lift(Io, M)` under a head `Lift(Io, (A) => Try(M, E, A))`: it is strictly smaller than the head, so resolution through it still decreases. The transformer's `Io` edge is then written once for every base, and here it resolves at base `Io` through the prelude's identity edge, `Lift(Io, Io)`.
#[test]
fn a_premise_naming_a_constant_beside_a_binder_resolves_through_the_constant_edge() {
    let source = r#"
        use /std/{Monad, Result, Io, Nat, Str, print};
        use /std/Monad/{Lift};
        pub struct Try(M: (Type) -> Type, E: Type, A: Type): Type { M(Result(E, A)) }
        let pure(@M: (Type) -> Type, @E: Type, @A: Type, use Monad(M), a: A) -> Try(M, E, A) =
            Try { Monad/pure(Result/success(a)) };
        let bind(@M: (Type) -> Type, @E: Type, @A: Type, @B: Type, use Monad(M), m: Try(M, E, A), f: (A) -> Try(M, E, B)) -> Try(M, E, B) =
            Try { Monad/bind(m.0, (r: Result(E, A)) => match r | success(a) => f(a).0 | failure(e) => Monad/pure(Result/failure(e)) end) };
        satisfy (@M: (Type) -> Type, @E: Type, use Monad(M)) => Monad((A: Type) => Try(M, E, A)) {
            pure(@A, a) = pure(a),
            bind(@A, @B, m, f) = bind(m, f),
        }
        satisfy (@M: (Type) -> Type, @E: Type, use Monad(M), use Lift(Io, M)) => Lift(Io, (A: Type) => Try(M, E, A)) {
            lift(@A, m) = Try { Monad/bind(Lift/lift(m), (a: A) => Monad/pure(Result/success(a))) },
        }
        let body() -> Try(Io, Nat, Nat) =
            let _ = print("a")!;
            let _ = Io/yield()!;
            let _ = print("b")!;
            pure(3);
        let program() -> Io({}) =
            let r = body().0!;
            match r
            | success(n) => print(Nat/to_str(n))
            | failure(_) => print("failed")
            end;
        program()
        "#;

    assert_eq!(run(source), b"ab3");
}

// A premise no smaller than the head would let resolution recurse into an equal goal forever, so it is refused where it is declared.
#[test]
fn a_premise_no_smaller_than_its_head_is_refused() {
    let source = r#"
        use /std/{Nat, Str, List};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy (@A: Type, use Show(List(A))) => Show(List(A)) {
            show(l) = Show/show(l)
        }
        /std/print("unreached")
        "#;

    let message = error(source);
    assert!(
        message.contains("non-regular premise"),
        "expected the premise refusal, got: {message}"
    );
}

// Resolution answers a `use` slot with a concept's witness and nothing else, so a `use` parameter at any other type is refused where it is declared rather than at every call that omits it. A proof was most likely meant to be discharged, which an implicit parameter does, and the refusal says so.
#[test]
fn a_use_parameter_at_a_proposition_is_refused_where_it_is_declared() {
    let report = error(
        r#"
        use /std/{Nat};
        use /std/Bool/{Holds};
        let f(n: Nat, use Holds(n < 10)) -> Nat = n;
        let g: Nat = f(3);
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains("the type of a `use` parameter is a concept application")
            && report.contains("found: `Holds(n < 10)`")
            && report.contains("write `@` in place of `use`")
            && !report.contains("no witness"),
        "unexpected report:\n{report}"
    );
}

// A witness telescope is a signature like any other, so its premises meet the same rule.
#[test]
fn a_witness_premise_at_a_proposition_is_refused() {
    let report = error(
        r#"
        use /std/{Nat, Show};
        use /std/Bool/{Holds};
        struct Foo: Type { Nat }
        satisfy (@A: Type, use Holds(0 < 1)) => Show(Foo) {
            show(_x) = "foo"
        }
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains("the type of a `use` parameter is a concept application")
            && report.contains("found: `Holds(0 < 1)`"),
        "unexpected report:\n{report}"
    );
}

// A lambda's `use` member states no type, so one that no expected function type states is refused by that rule, where an unannotated plain binder is asked for its annotation.
#[test]
fn an_inferred_lambda_has_no_use_member() {
    let report = error(
        r#"
        use /std/{Nat};
        let g = (use _, n: Nat) => n;
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains("this lambda writes `use _` and nothing states the member")
            && !report.contains("annotate the parameter"),
        "unexpected report:\n{report}"
    );
}

// The rule judges what the type reduces to, so an alias of a concept application is one.
#[test]
fn a_use_parameter_through_an_alias_of_a_concept_application_resolves() {
    let source = r#"
        use /std/{Nat, Show, Str};
        let ShowNat: Type = Show(Nat);
        let f(use ShowNat, n: Nat) -> Str = Show/show(n);
        /std/print(f(3))
        "#;

    assert_eq!(run(source), b"3");
}

// An `@` parameter is filled by unification and a `use` parameter by resolution, so an `@` parameter at a concept's type that no later type mentions has nothing to fill it: refused where it is declared, with the mark that would. One a later type mentions is determined by that type and stays.
#[test]
fn an_implicit_parameter_at_a_concept_nothing_mentions_is_refused_where_it_is_declared() {
    let report = error(
        r#"
        use /std/{Nat, Str, Show};
        let shown(@A: Type, @s: Show(A), x: A) -> Str = "x";
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains(
            "an `@` parameter at a concept's type that no later type mentions is never filled"
        ) && report.contains("found: `Show(A)`")
            && report.contains("write `use` in place of `@`"),
        "unexpected report:\n{report}"
    );

    let determined = r#"
        use /std/{Nat, Bool, Str, print};
        pub concept Key(K: Type): pub Type {
            same(K, K) -> Bool
        }
        satisfy Key(Nat) {
            same(a, b) = a == b
        }
        pub struct Slot(K: Type, use Key(K), V: Type): pub Type {
            key: K,
            value: V
        }
        let under(@K: Type, @from: Key(K), @V: Type, at: Slot(K, use from, V), key: K) -> Bool =
            Key/same(@_, use from, at.key, key);
        let never: Key(Nat) = Key { same(a, b) = false };
        let bare: Slot(Nat, use never, Str) = Slot { key = 2, value = "two" };
        print(Bool/to_str(under(bare, 2)))
        "#;

    assert_eq!(run(determined), b"false");
}

// An operator is the projection of the witness its operands resolve, the carrier's table entry only where no premise answers first: under a local `use Eql(Nat)`, `x == x` is the premise's comparison, which no reduction decides, and a guard on it is no fact the bound search reads, since the premise's `eql` need not be `Nat`'s.
#[test]
fn an_operator_under_a_local_premise_decides_nothing_the_carriers_witness_would() {
    let reduced = r#"
        use /std/{Nat};
        use /std/ops/{Eql};
        use /std/Bool/{True, Holds};
        let _under(use Eql(Nat), x: Nat) -> Holds(x == x) = True/qed();
        /std/print("")
        "#;
    let report = error(reduced);
    assert!(
        report.contains("type mismatch\n│  inferred: `True`\n│  expected: `Holds(x == x)`"),
        "{report}"
    );

    let guarded = r#"
        use /std/{Nat, List};
        use /std/ops/{Eql};
        let _under(use Eql(Nat), x: Nat, y: Nat) -> Nat =
            match x == y | true => List/get([1], y - x) | false => 0 end;
        /std/print("")
        "#;
    let report = error(guarded);
    assert!(
        report.contains("nothing discharged `/std/Bool/Holds(y - x < List/len(@Nat, [1]))`")
            && report.contains("not read: a guard, `x == y`"),
        "{report}"
    );
}

// A witness whose annotation applies a type that is no concept is refused as naming none, at the annotation, rather than as a function applied; a concept that takes no parameter, applied, keeps the refusal its application meets, since its head is a concept.
#[test]
fn a_witness_applying_no_concept_is_refused_as_naming_none() {
    let refused = |declarations: &str| {
        error(&fill!(
            "use /std/{Nat, Bool};\n${declarations}\n/std/print(\"\")\n",
            declarations
        ))
    };

    let intrinsic = refused("satisfy Nat(Nat) { }");
    assert!(
        intrinsic.contains("not a concept")
            && intrinsic.contains("`Nat(Nat)` applies a head that is no concept"),
        "{intrinsic}"
    );
    let parameterless =
        refused("concept Flag: Type { on: Bool, }\nsatisfy Flag(Nat) { on = true }");
    assert!(
        !parameterless.contains("applies a head that is no concept"),
        "{parameterless}"
    );
}
