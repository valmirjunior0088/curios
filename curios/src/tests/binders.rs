//! End-to-end tests for binder *scoping*: a lambda's parameter list is a
//! dependent telescope, so a later binder's annotation sees the earlier
//! binders, exactly as a Π type's later domains do. Plicity of those binders is
//! `plicity.rs`; irrefutable pattern shapes are `aggregates.rs`.

use super::run;

// The motivating shape: `q`'s annotation names `s` and `t`, the two binders
// preceding it. Lowering scopes each annotation over the parameters declared
// before it, so this resolves against the lambda's own telescope rather than
// the ambient scope (where `s` and `t` are unbound).
#[test]
fn lambda_annotation_names_an_earlier_binder() {
    let source = r#"
        use /std/{Nat, Eq};
        let flip : (@A : Type, s : A, t : A, q : Eq(s, t)) -> Eq(t, s) =
            (@A, s : A, t : A, q : Eq(s, t)) =>
                match q
                | refl(@z) => Eq/refl()
                end;
        let _ : Eq(2, 2) = flip(2, 2, Eq/refl());
        /std/print(Nat/to_str(2))
        "#;
    assert_eq!(run(source), b"2");
}

// The same telescope written as a Π type and as the lambda checked against it:
// both accept the dependency, so an annotated lambda is not a second, weaker
// grammar for the same thing.
#[test]
fn func_type_and_lambda_accept_the_same_dependent_telescope() {
    let source = r#"
        use /std/{Nat, Eq};
        let apply(f : (s : Nat, t : Nat, q : Eq(s, t)) -> Nat) -> Nat =
            f(7, 7, Eq/refl());
        let written : (s : Nat, t : Nat, q : Eq(s, t)) -> Nat =
            (s : Nat, t : Nat, q : Eq(s, t)) => s;
        /std/print(Nat/to_str(apply(written)))
        "#;
    assert_eq!(run(source), b"7");
}

// A compound parameter binds no leaf name at the core binder — its leaves are
// projections off a synthetic binder — so a later annotation naming one of them
// only resolves if that pattern's field bindings scope over the domain too.
#[test]
fn lambda_annotation_names_a_tuple_pattern_leaf() {
    let source = r#"
        use /std/{Nat, Eq};
        let first : (pair : {Nat, Nat}, q : Eq(pair.0, pair.1)) -> Nat =
            ((lo, hi), q : Eq(lo, hi)) => lo;
        /std/print(Nat/to_str(first((4, 4), Eq/refl())))
        "#;
    assert_eq!(run(source), b"4");
}

// The struct-pattern spelling of the same thing: punned fields are ordinary
// leaf names and must be in scope for a later annotation as well.
#[test]
fn lambda_annotation_names_a_struct_pattern_leaf() {
    let source = r#"
        use /std/{Nat, Eq};
        struct Span : pub Type {
            lo : Nat,
            hi : Nat,
        }
        let high : (span : Span, q : Eq(span.lo, span.hi)) -> Nat =
            (Span { lo, hi }, q : Eq(lo, hi)) => hi;
        /std/print(Nat/to_str(high(Span { lo = 9, hi = 9 }, Eq/refl())))
        "#;
    assert_eq!(run(source), b"9");
}

// The shadowing decision, pinned. A module binding `T : Type = Bool` is in
// scope, and the lambda's first parameter is also named `T`. Passing this test
// *is* the pin: the expected domain of `value` is the parameter `T`, which the
// call instantiates at `Nat`, so an annotation resolving to the module binding
// would be `Bool` and mismatch. An earlier parameter shadows a like-named
// module binding inside a later annotation, exactly as it already does inside
// the body.
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
