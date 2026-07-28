//! End-to-end tests for binder plicity: automatic insertion of omitted hidden
//! lambda binders (Phase 3), exact checking of written function binders
//! (Phase 1), and exact checking of constructor-pattern plicity (Phase 2).

use super::run;
use crate::run_text;
use curios_runtime::MockHost;

fn error(source: &str) -> String {
    let (system, _io) = MockHost::builder().build();
    match run_text(source, system) {
        Ok(_) => panic!("expected an error, program succeeded"),
        Err(error) => error.to_string(),
    }
}

// A lambda checked against `(@A : Type, x : A) -> A` may omit the implicit
// binder entirely: elaboration inserts `@A` and binds the sole explicit slot.
#[test]
fn lambda_inserts_an_omitted_implicit_binder() {
    let source = r#"
        use /std/{Nat, Str};
        let id : (@A : Type, x : A) -> A = (x) => x;
        /std/print(Nat/to_str(id(5)))
        "#;
    assert_eq!(run(source), b"5");
}

// The same definition may instead write the implicit binder explicitly; both
// spellings elaborate to the same canonical function.
#[test]
fn lambda_may_write_the_implicit_binder() {
    let source = r#"
        use /std/{Nat, Str};
        let id : (@A : Type, x : A) -> A = (@A, x) => x;
        /std/print(Nat/to_str(id(5)))
        "#;
    assert_eq!(run(source), b"5");
}

// A lambda checked against a type with both an implicit and a witness binder may
// omit both. The inserted witness binder still joins witness resolution, so the
// body resolves `Show(A)` through it.
#[test]
fn lambda_inserts_an_omitted_witness_binder() {
    let source = r#"
        use /std/{Nat, Str, Show};
        let showit : (@A : Type, use Show(A), x : A) -> Str = (x) => Show/show(x);
        /std/print(showit(7))
        "#;
    assert_eq!(run(source), b"7");
}

// The witness binder may be written and named with `use`; the body may then
// reference it directly.
#[test]
fn lambda_may_write_the_witness_binder() {
    let source = r#"
        use /std/{Nat, Str, Show};
        let showit : (@A : Type, use Show(A), x : A) -> Str = (@A, use s, x) => Show/show(x);
        /std/print(showit(7))
        "#;
    assert_eq!(run(source), b"7");
}

// A plain binder can never bind a hidden slot: written against `(@A, x) -> A`,
// the first plain binder claims the sole explicit slot, so the second is surplus.
#[test]
fn lambda_plain_binder_never_binds_a_hidden_slot() {
    let source = r#"
        use /std/{Nat, Str};
        let bad : (@A : Type, x : A) -> A = (a, x) => x;
        /std/print(Nat/to_str(bad(5)))
        "#;
    // The implicit is inserted before `a`, so `a` binds `x` and `x` is surplus.
    assert!(error(source).contains("arguments"), "{}", error(source));
}

// A marked binder that reaches an explicit slot is a plicity mismatch: writing
// `@x` for a plain parameter is rejected, naming the required spelling.
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

// A constructor pattern must mark an implicit payload slot with `@`. `Vec/cons`
// declares its length index implicit, so the arm binds it with `@`.
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

// Matching an implicit payload with a plain binder is rejected — the pattern
// must carry `@`.
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
