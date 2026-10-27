//! Scoping: a later binder's annotation sees the earlier ones and the pattern leaves before it, and a binder or member left unnamed is reached by no name.

use crate::tests::{error, run};

// === Scoping: a later annotation names the binders before it. ====================

// The motivating shape: `q`'s annotation names `s` and `t`, the two binders preceding it. Lowering scopes each annotation over the parameters declared before it, so this resolves against the lambda's own telescope rather than the ambient scope (where `s` and `t` are unbound).
#[test]
fn lambda_annotation_names_an_earlier_binder() {
    let source = r#"
        use /std/{Nat, Eq};
        let flip : (@A : Type, s : A, t : A, q : Eq()(s, t)) -> Eq()(t, s) =
            (@A, s : A, t : A, q : Eq()(s, t)) =>
                match q
                | refl(@z) => Eq/refl()
                end;
        let _ : Eq()(2, 2) = flip(2, 2, Eq/refl());
        /std/print(Nat/to_str(2))
        "#;
    assert_eq!(run(source), b"2");
}

// The same telescope written as a Π type and as the lambda checked against it: both accept the dependency, so an annotated lambda is not a second, weaker grammar for the same thing.
#[test]
fn func_type_and_lambda_accept_the_same_dependent_telescope() {
    let source = r#"
        use /std/{Nat, Eq};
        let apply(f : (s : Nat, t : Nat, q : Eq()(s, t)) -> Nat) -> Nat =
            f(7, 7, Eq/refl());
        let written : (s : Nat, t : Nat, q : Eq()(s, t)) -> Nat =
            (s : Nat, t : Nat, q : Eq()(s, t)) => s;
        /std/print(Nat/to_str(apply(written)))
        "#;
    assert_eq!(run(source), b"7");
}

// A compound parameter binds no leaf name at the core binder — its leaves are projections off a synthetic binder — so a later annotation naming one of them only resolves if that pattern's field bindings scope over the domain too.
#[test]
fn lambda_annotation_names_a_tuple_pattern_leaf() {
    let source = r#"
        use /std/{Nat, Eq};
        let first : (pair : {Nat, Nat}, q : Eq()(pair.0, pair.1)) -> Nat =
            ((lo, hi), q : Eq()(lo, hi)) => lo;
        /std/print(Nat/to_str(first((4, 4), Eq/refl())))
        "#;
    assert_eq!(run(source), b"4");
}

// The struct-pattern spelling of the same thing: punned fields are ordinary leaf names and must be in scope for a later annotation as well.
#[test]
fn lambda_annotation_names_a_struct_pattern_leaf() {
    let source = r#"
        use /std/{Nat, Eq};
        struct Span : pub Type {
            lo : Nat,
            hi : Nat,
        }
        let high : (span : Span, q : Eq()(span.lo, span.hi)) -> Nat =
            (Span { lo, hi }, q : Eq()(lo, hi)) => hi;
        /std/print(Nat/to_str(high(Span { lo = 9, hi = 9 }, Eq/refl())))
        "#;
    assert_eq!(run(source), b"9");
}

// The shadowing decision, pinned. A module binding `T : Type = Bool` is in scope, and the lambda's first parameter is also named `T`. Passing this test *is* the pin: the expected domain of `value` is the parameter `T`, which the call instantiates at `Nat`, so an annotation resolving to the module binding would be `Bool` and mismatch. An earlier parameter shadows a like-named module binding inside a later annotation, exactly as it already does inside the body.
#[test]
fn an_earlier_binder_shadows_a_module_binding_in_a_later_annotation() {
    let source = r#"
        use /std/{Nat, Bool};
        let T : Type = Bool;
        let identity : (T : Type, value : T) -> T = (T, value : T) => value;
        /std/print(Nat/to_str(identity(Nat, 3)))
        "#;
    assert_eq!(run(source), b"3");
}

// A bound a definition's body never names is written `@_: T`. It is the implicit argument a named one would be: the call discharges it, and the body has it as a fact in scope, so the bound the body's own call needs follows from it.
#[test]
fn a_definition_takes_a_bound_it_never_names() {
    let source = r#"
        use /std/{Nat, Str, print};
        use /std/Bool/{Holds};
        let positive(n: Nat, @_: Holds(0 < n)) -> Nat = n;
        let twice(n: Nat, @_: Holds(0 < n)) -> Nat = positive(n) + positive(n);
        let second(_: Nat, n: Nat) -> Nat = n;
        print(Nat/to_str(twice(second(7, 3))))
        "#;

    assert_eq!(run(source), b"6");
}

// A payload takes the unnamed `@` member too: construction discharges the bound, and a pattern binds it under the mark its slot was declared with.
#[test]
fn a_payload_takes_an_unnamed_implicit_member() {
    let source = r#"
        use /std/{Nat, Str, print};
        use /std/Bool/{Holds};
        induct Positive: pub Type
        | at(n: Nat, @Holds(0 < n))
        end
        let value(p: Positive) -> Nat = match p | at(n, @_) => n end;
        print(Nat/to_str(value(Positive/at(4))))
        "#;

    assert_eq!(run(source), b"4");
}

// A member written without a name has none: lowering mints no placeholder for an unnamed payload or a positional field, so no later type and no projection can spell one.
#[test]
fn an_unnamed_member_is_reached_by_no_name() {
    let payload = error(
        r#"
        use /std/{Nat, Str, print};
        use /std/Bool/{Holds};
        induct Positive: pub Type
        | at(Nat, Holds(0 < _0))
        end
        print("no")
        "#,
    );
    assert!(payload.contains("`_0` is not in scope"), "got: {payload}");

    let field = error(
        r#"
        use /std/{Nat, Str, print};
        struct Meters: pub Type { Nat }
        let m: Meters = Meters { 4 };
        print(Nat/to_str(m._0))
        "#,
    );
    assert!(
        field.contains("'_0'") && field.contains("no field"),
        "got: {field}"
    );
}

// A `foreign` declaration writes no operand name, so its type has none: the row is bound as the function of its operands, and a report spells the type as the declaration did.
#[test]
fn a_foreign_declarations_operands_have_no_names() {
    let message = error(
        r#"
        use /std/{Nat, Bytes, Str, print};
        foreign frobnicate(Nat, Bytes) -> Nat;
        let f: Str = frobnicate;
        print("no")
        "#,
    );

    assert!(
        message.contains("inferred: (Nat, Bytes) -> /std/Io(Nat)"),
        "got: {message}"
    );
}
