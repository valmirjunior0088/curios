//! Binder scoping and plicity: a lambda's parameter list is a dependent telescope, and each of its binders is explicit, implicit or a witness slot.
//!
//! Scoping is that a later binder's annotation sees the earlier binders, exactly as a Π type's later domains do. Plicity is which of those binders a call site must supply, which elaboration inserts, and which a constructor pattern may mark. Irrefutable pattern shapes are `aggregates.rs`.

use super::{error, run};

// === Scoping: a later annotation names the binders before it. ====================

// The motivating shape: `q`'s annotation names `s` and `t`, the two binders preceding it. Lowering scopes each annotation over the parameters declared before it, so this resolves against the lambda's own telescope rather than the ambient scope (where `s` and `t` are unbound).
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

// The same telescope written as a Π type and as the lambda checked against it: both accept the dependency, so an annotated lambda is not a second, weaker grammar for the same thing.
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

// A compound parameter binds no leaf name at the core binder — its leaves are projections off a synthetic binder — so a later annotation naming one of them only resolves if that pattern's field bindings scope over the domain too.
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

// The struct-pattern spelling of the same thing: punned fields are ordinary leaf names and must be in scope for a later annotation as well.
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

// === Plicity: insertion of omitted hidden binders, and exact checking of written ones. ====

// A lambda checked against `(@A : Type, x : A) -> A` may omit the implicit binder entirely: elaboration inserts `@A` and binds the sole explicit slot.
#[test]
fn lambda_inserts_an_omitted_implicit_binder() {
    let source = r#"
        use /std/{Nat, Str};
        let id : (@A : Type, x : A) -> A = (x) => x;
        /std/print(Nat/to_str(id(5)))
        "#;
    assert_eq!(run(source), b"5");
}

// The same definition may instead write the implicit binder explicitly; both spellings elaborate to the same canonical function.
#[test]
fn lambda_may_write_the_implicit_binder() {
    let source = r#"
        use /std/{Nat, Str};
        let id : (@A : Type, x : A) -> A = (@A, x) => x;
        /std/print(Nat/to_str(id(5)))
        "#;
    assert_eq!(run(source), b"5");
}

// A lambda checked against a type with both an implicit and a witness binder may omit both. The inserted witness binder still joins witness resolution, so the body resolves `Show(A)` through it.
#[test]
fn lambda_inserts_an_omitted_witness_binder() {
    let source = r#"
        use /std/{Nat, Str, Show};
        let showit : (@A : Type, use Show(A), x : A) -> Str = (x) => Show/show(x);
        /std/print(showit(7))
        "#;
    assert_eq!(run(source), b"7");
}

// The witness binder may be written and named with `use`; the body may then reference it directly.
#[test]
fn lambda_may_write_the_witness_binder() {
    let source = r#"
        use /std/{Nat, Str, Show};
        let showit : (@A : Type, use Show(A), x : A) -> Str = (@A, use s, x) => Show/show(x);
        /std/print(showit(7))
        "#;
    assert_eq!(run(source), b"7");
}

// A plain binder can never bind a hidden slot: written against `(@A, x) -> A`, the first plain binder claims the sole explicit slot, so the second is surplus.
#[test]
fn lambda_plain_binder_never_binds_a_hidden_slot() {
    let source = r#"
        use /std/{Nat, Str};
        let bad : (@A : Type, x : A) -> A = (a, x) => x;
        /std/print(Nat/to_str(bad(5)))
        "#;
    // The implicit is inserted before `a`, so `a` binds `x` and `x` is surplus, which is what the refusal says — a count would say `2, 2`.
    assert!(
        error(source).contains("claims no parameter"),
        "{}",
        error(source)
    );
}

// The surplus refusal names the surplus, because no count pair can name this fault.
//
// Alignment is positional by plicity, so a lambda and its expected type can agree on their totals *and* on their explicit counts and still fail to align. Comparing totals reported `expected 3, got 3` for the first of these — self-contradictory, and silent about the rule that decided it.
#[test]
fn a_lambda_binder_that_claims_no_parameter_is_named_as_surplus() {
    let surplus = |source: &str| {
        let rendered = error(source);
        assert!(
            rendered.contains("claims no parameter") || rendered.contains("claim no parameter"),
            "{rendered}"
        );
        rendered
    };

    // syntax.md's own refused spelling: `A` binds the sole explicit slot and the rest are surplus.
    surplus(
        r#"
        use /std/{Str, Show};
        let bad : (@A : Type, use Show(A), value : A) -> Str = (A, show, value) => Show/show(value);
        /std/print(bad(5))
        "#,
    );

    // One explicit parameter, one explicit binder, and still a surplus: `@A` claims nothing.
    surplus(
        r#"
        use /std/{Nat};
        let bad : (x : Nat) -> Nat = (x, @A) => x;
        /std/print(Nat/to_str(bad(5)))
        "#,
    );
}

// A lambda short of an explicit parameter counts the explicit ones, which are the only parameters it may write.
//
// Counting the total said `expected 3` for a type with two explicit parameters, and acting on it means writing the third — the spelling refused just above.
#[test]
fn a_lambda_short_of_a_parameter_counts_the_explicit_ones() {
    let source = r#"
        use /std/{Nat};
        let bad : (@A : Type, x : Nat, y : Nat) -> Nat = (x) => x;
        /std/print(Nat/to_str(bad(5, 6)))
        "#;
    assert!(
        error(source).contains("expected 2, got 1"),
        "{}",
        error(source)
    );
}

// A marked binder that reaches an explicit slot is a plicity mismatch: writing `@x` for a plain parameter is rejected, naming the required spelling.
#[test]
fn lambda_marked_binder_on_explicit_slot_is_rejected() {
    let source = r#"
        use /std/{Nat, Str};
        let bad : (x : Nat) -> Nat = (@x) => x;
        /std/print(Nat/to_str(bad(5)))
        "#;
    assert!(
        error(source).contains("explicit parameter"),
        "{}",
        error(source)
    );
}

// A constructor pattern must mark an implicit payload slot with `@`. `Vec/cons` declares its length index implicit, so the arm binds it with `@`.
#[test]
fn constructor_pattern_matches_an_implicit_payload() {
    let source = r#"
        use /std/{Nat, Vec, Str};
        let head3(v : Vec(Nat, 3)) -> Nat =
            match v : (_, _) => Nat
            | cons(@m, x, xs) => x
            end;
        /std/print(Nat/to_str(head3(Vec/cons(1, Vec/cons(2, Vec/cons(3, Vec/nil()))))))
        "#;
    assert_eq!(run(source), b"1");
}

// Matching an implicit payload with a plain binder is rejected — the pattern must carry `@`.
#[test]
fn constructor_pattern_plain_on_implicit_payload_is_rejected() {
    let source = r#"
        use /std/{Nat, Vec, Str};
        let head3(v : Vec(Nat, 3)) -> Nat =
            match v : (_, _) => Nat
            | cons(m, x, xs) => x
            end;
        /std/print(Nat/to_str(head3(Vec/cons(1, Vec/cons(2, Vec/cons(3, Vec/nil()))))))
        "#;
    assert!(
        error(source).contains("implicit parameter"),
        "{}",
        error(source)
    );
}

// Marking a plain payload slot with `@` is likewise rejected.
#[test]
fn constructor_pattern_mark_on_explicit_payload_is_rejected() {
    let source = r#"
        use /std/{Nat, Vec, Str};
        let head3(v : Vec(Nat, 3)) -> Nat =
            match v : (_, _) => Nat
            | cons(@m, @x, xs) => x
            end;
        /std/print(Nat/to_str(head3(Vec/cons(1, Vec/cons(2, Vec/cons(3, Vec/nil()))))))
        "#;
    assert!(
        error(source).contains("explicit parameter"),
        "{}",
        error(source)
    );
}

// A bare reference whose type leads with an implicit binder, checked against a rigid non-arrow expectation, has the hidden prefix inserted at the reference. Plicity is part of function identity, so this configuration was a guaranteed mismatch — insertion only rescues errors.
#[test]
fn bare_reference_inserts_hidden_arguments_at_a_rigid_expectation() {
    let source = r#"
        use /std/{Nat, Str, Option};
        let none_of(@A : Type) -> Option(A) =
            Option/none();
        let x : Option(Nat) =
            match false
            | true => Option/some(1)
            | false => none_of
            end;
        let shown : Str =
            match x
            | some(n) => Nat/to_str(n)
            | none() => "none"
            end;
        /std/print(shown)
        "#;
    assert_eq!(run(source), b"none");
}

// The exemption: a bare reference assigned at its own hidden-headed function type keeps the polymorphic value — insertion fires only where that type could never convert.
#[test]
fn bare_reference_keeps_its_type_at_a_hidden_expectation() {
    let source = r#"
        use /std/{Nat, Str};
        let ident(@A : Type, a : A) -> A =
            a;
        let keep : (@A : Type, a : A) -> A =
            ident;
        /std/print(Nat/to_str(keep(9)))
        "#;
    assert_eq!(run(source), b"9");
}
