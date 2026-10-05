//! Resolving a witness through the table: a method wrapper, a premise, an explicit override, several parameters, a later declaration and the operator concepts — and what a premise may be.

use crate::tests::{error, run};

// The base case: a concept, a witness keyed on a rigid nominal head, and a call through the generated method wrapper. `Show/show(n)` saturates `@A` with a metavar and the `use` slot with a witness goal; solving `A := Nat` from `n` wakes the goal, which the global table resolves to the `Show(Nat)` witness.
#[test]
fn concept_witness_resolves_through_wrapper() {
    let source = r#"
        use /std/{Nat, Str};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        let n : Nat = 42;
        /std/print(Show/show(n))
        "#;

    assert_eq!(run(source), b"42");
}

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

// An explicit `use` argument overrides table resolution: a local dictionary value (an ordinary `let` of the concept's record type) is passed at the call site and used instead of the registered `Show(Nat)` witness.
#[test]
fn explicit_use_argument_overrides() {
    let source = r#"
        use /std/{Nat, Str};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        let parens : Show(Nat) =
            Show(Nat) { show = (n) => Str/concat("(", Str/concat(Nat/to_str(n), ")")) };
        let n : Nat = 7;
        /std/print(Show/show(@_, use parens, n))
        "#;

    assert_eq!(run(source), b"(7)");
}

// The prelude-provided `Show` concept and its witnesses resolve, proving the cached-prelude replay path registers concepts and witnesses.
#[test]
fn prelude_show_resolves() {
    let source = r#"
        use /std/{Nat, Show};
        let n : Nat = 42;
        /std/print(Show/show(n))
        "#;

    assert_eq!(run(source), b"42");
}

// The prelude `Eql` concept resolves through the value-level witnesses.
#[test]
fn prelude_eql_resolves() {
    let source = r#"
        use /std/{Nat, Bool};
        use /std/ops/{Eql};
        let a : Nat = 5;
        let b : Nat = 5;
        /std/print(Bool/to_str(Eql/eql(a, b)))
        "#;

    assert_eq!(run(source), b"true");
}

// Multi-parameter concepts key on the tuple of every parameter head: two witnesses may share a first head as long as the full parameter tuple differs, and each resolves once both parameters are pinned.
#[test]
fn multi_param_witnesses_share_a_first_head() {
    let source = r#"
        use /std/{Nat, Bool, Str};
        pub concept Into(A : Type, B : Type) : pub Type {
            into(A) -> B
        }
        satisfy Into(Nat, Str) {
            into(n) = Nat/to_str(n)
        }
        satisfy Into(Nat, Bool) {
            into(n) = Nat/eql(n, 1)
        }
        let s : Str = Into/into(2);
        let b : Bool = Into/into(2);
        /std/print(Bool/to_str(b))
        "#;

    assert_eq!(run(source), b"false");
}

// Every concept parameter participates in the witness key, so a goal whose second parameter is never pinned parks and surfaces as an error at the end of the module — no accidental inference from the witness.
#[test]
fn open_parameter_does_not_infer_from_the_witness() {
    let source = r#"
        use /std/{Nat, Str};
        pub concept Into(A : Type, B : Type) : pub Type {
            into(A) -> B
        }
        satisfy Into(Nat, Str) {
            into(n) = Nat/to_str(n)
        }
        pub let discard(@A : Type, x : A) -> Nat = 0;
        /std/print(Nat/to_str(discard(Into/into(1))))
        "#;

    let message = error(source).to_lowercase();
    assert!(message.contains("witness") || message.contains("infer"));
}

// The operator concepts: `Add/add` resolves on an intrinsic type through the `/std` witness (also proving the cached-prelude replay path registers the operator concepts and their `/std` witnesses), on a user struct through a user witness, and in generic code through a local `use Add(A)` premise.
#[test]
fn syn_add_concept_resolves_everywhere() {
    let source = r#"
        use /std/{Nat, Str};
        use /std/ops/{Add};
        struct Point : pub Type { x : Nat, y : Nat }
        satisfy Add(Point) {
            add(a, b) = Point { x = Nat/add(a.x, b.x), y = Nat/add(a.y, b.y) }
        }
        pub let double(@A : Type, use Add(A), v : A) -> A = Add/add(v, v);
        let p : Point = double(Point { x = 3, y = 4 });
        let n : Nat = Add/add(20, 1);
        /std/print(Nat/to_str(Nat/add(p.x, n)))
        "#;

    assert_eq!(run(source), b"27");
}

// `Eql` and `Cmp` resolve across intrinsics with the witnesses homed beside each type — `Eql(Nat)`/`Cmp(Nat)` in `/std/Nat`, `Eql(Str)` in `/std/Str`, `Cmp(Flt)` in `/std/Flt` — rather than in the operator-concept facades, which keep only the concept re-exports.
#[test]
fn eql_and_cmp_resolve_across_intrinsics() {
    let source = r#"
        use /std/{Nat, Flt, Bool, Str};
        use /std/ops/{Eql, Cmp};
        let a : Bool = Eql/eql(2, 2);
        let b : Bool = Eql/eql("abc", "abc");
        let c : Bool = Cmp/lt(1.0, 2.0);
        let d : Bool = Cmp/ge(3, 3);
        /std/print(Bool/to_str(Bool/and(Bool/and(a, b), Bool/and(c, d))))
        "#;

    assert_eq!(run(source), b"true");
}

// A witness declared *after* a value that uses it still resolves: the unit's witnesses are known by key before any of them elaborates, so the use-site goal finds the later `satisfy`, and the value is elaborated once the witness has been. This ordering freedom is what lets a `/std` witness live beside its type — a type module's own value functions may call an operator before the module's trailing witness block, the way `/std/Nat`'s `min`/`cmp` use `<`/`==` ahead of `Cmp(Nat)`.
#[test]
fn forward_declared_witness_resolves() {
    let source = r#"
        use /std/{Nat, Bool, Str};
        pub concept Eqx(A : Type) : pub Type {
            eqx(A, A) -> Bool
        }
        pub let uses_eqx(a : Nat, b : Nat) -> Bool = Eqx/eqx(a, b);
        satisfy Eqx(Nat) {
            eqx(a, b) = Nat/eql(a, b)
        }
        /std/print(Bool/to_str(uses_eqx(3, 3)))
        "#;

    assert_eq!(run(source), b"true");
}

// A witness spelled through a name for its type registers where the type's own would: `Alias` is `Box`, so the witness is known under `Show(Box)` before it elaborates, and a goal written before it finds it.
#[test]
fn a_witness_spelled_through_a_name_for_its_type_is_keyed_at_the_type() {
    let source = r#"
        use /std/{Nat, Show, Str};
        pub struct Box: pub Type { value: Nat }
        let Alias: Type = Box;
        let shown: Str = Show/show(Box { value = 1 });
        satisfy Show(Alias) { show(b) = "boxed" }
        /std/print(shown)
        "#;

    assert_eq!(run(source), b"boxed");
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
            "this witness of 'Show' is for head 'Nat', which its signature does not spell"
        ),
        "expected the refusal to name the key the signature computes:\n{report}"
    );
    assert!(
        report.contains("write 'Nat' where the signature computes it"),
        "expected the way out:\n{report}"
    );
}

// A premise may name a constant beside a binder, `Lift(Io, M)` under a head `Lift(Io, (A) => Try(M, E, A))`: it is strictly smaller than the head, so resolution through it still decreases. The transformer's `Io` edge is then written once for every base, and here it resolves at base `Async` through the prelude's `Lift(Io, Async)`.
#[test]
fn a_premise_naming_a_constant_beside_a_binder_resolves_through_the_constant_edge() {
    let source = r#"
        use /std/{Monad, Result, Io, Async, Nat, Str, print};
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
        satisfy (@E: Type) => Lift(Async, (A: Type) => Try(Async, E, A)) {
            lift(@A, m) = Try { Async/map(m, (a: A) => Result/success(a)) },
        }
        let body: Try(Async, Nat, Nat) =
            let _ = print("a")!;
            let _ = Async/yield_now!;
            let _ = print("b")!;
            pure(3);
        let fiber: Async({}) =
            let r = body.0!;
            match r
            | success(n) => Async/lift(print(Nat/to_str(n)))
            | failure(_) => Async/lift(print("failed"))
            end;
        Async/run(fiber)
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
        report.contains("a 'use' parameter's type must be a concept application")
            && report.contains("found: Holds(n < 10)")
            && report.contains("write '@' in place of 'use'")
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
        report.contains("a 'use' parameter's type must be a concept application")
            && report.contains("found: Holds(0 < 1)"),
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
            "an '@' parameter at a concept's type that no later type mentions can never be filled"
        ) && report.contains("found: Show(A)")
            && report.contains("write 'use' in place of '@'"),
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
