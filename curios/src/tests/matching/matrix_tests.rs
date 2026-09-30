//! The matrix form's own rules: consistent arity, no duplicate row or tag, and where a motive may sit.

use crate::tests::{error, run};

// Two rows write a tuple pattern of different arity in the same column — there is no single shape to explode into projections.
#[test]
fn match_rejects_inconsistent_tuple_arity() {
    let source = r#"
        use /std/{Nat};
        let f(p : { Nat, Nat }) -> Nat =
            match p
            | (x, y) => x
            | (x,) => x
            end;
        /std/print(Nat/to_str(f((3, 4))))
        "#;

    let error = error(source);
    assert!(
        error.contains("disagree on shape"),
        "unexpected error: {error}"
    );
}

// Two rows are identical in every column, several levels deep — an overlapping arm the full-enumeration matrix has no priority order to resolve.
#[test]
fn match_rejects_duplicate_row() {
    let source = r#"
        use /std/{Option, Nat};
        let f(a : Option(Nat), b : Option(Nat)) -> Nat =
            match (a, b)
            | (some(x), some(y)) => x + y
            | (some(x), some(y)) => x
            | (some(x), none()) => x
            | (none(), some(y)) => y
            | (none(), none()) => 0
            end;
        /std/print(Nat/to_str(f(Option/some(3), Option/some(4))))
        "#;

    let error = error(source);
    assert!(
        error.contains("duplicate or overlapping"),
        "unexpected error: {error}"
    );
}

// A literal repeated constructor tag in a flat, single-column match, refused rather than collapsed to whichever arm's tag survives collection, dropping the other.
#[test]
fn match_rejects_duplicate_flat_tag() {
    let source = r#"
        use /std/{Option, Nat};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(a) => a
            | some(b) => b
            | none() => 0
            end;
        /std/print(Nat/to_str(f(Option/some(3))))
        "#;

    let error = error(source);
    assert!(
        error.contains("duplicate or overlapping"),
        "unexpected error: {error}"
    );
}

// A plain binder row (a named catch-all, which the full-enumeration matrix refuses) mixed with a concrete constructor row in the same column.
#[test]
fn match_rejects_mixed_binder_and_ctor_column() {
    let source = r#"
        use /std/{Option, Nat};
        let f(o : Option(Nat)) -> Nat =
            match o
            | x => 0
            | some(y) => y
            end;
        /std/print(Nat/to_str(f(Option/some(3))))
        "#;

    let error = error(source);
    assert!(
        error.contains("disagree on shape"),
        "unexpected error: {error}"
    );
}

// A dependent motive requires the head to dispatch on a constructor tag directly — there is no core `Match` node for a tuple-headed match to attach it to.
#[test]
fn match_rejects_a_motive_on_tuple_head() {
    // A tuple-headed matrix explodes into projections and builds no core `Match` node, so there is no eliminator for a motive to be checked against. Rejected rather than silently discarded.
    let source = r#"
        use /std/{Nat};
        let f(p : { Nat, Nat }) -> Nat =
            match p : (q) => Nat
            | (x, y) => x
            end;
        /std/print(Nat/to_str(f((3, 4))))
        "#;

    let error = error(source);
    assert!(
        error.contains("written motive"),
        "unexpected error: {error}"
    );
}

// A `Nat` column missing its `0` case entirely, reported by the case it lacks. These four hardcoded carriers have no core-side exhaustiveness mechanism to fall back on, unlike an ordinary constructor tag.
#[test]
fn match_rejects_incomplete_nat_pattern() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat =
            match n
            | n2 + 1; ih => n2
            end;
        /std/print(Nat/to_str(f(3)))
        "#;

    let error = error(source);
    assert!(
        error.contains("must also cover `0`"),
        "unexpected error: {error}"
    );
}

// A dependent motive is legal on a top-level `Nat` head too, not just a constructor head. The arms are written succ-case-first, which changes nothing: the full-enumeration matrix gives rows no priority order.
#[test]
fn match_allows_dependent_motive_on_nat_head() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat =
            match n : (m) => Nat
            | m + 1; ih => m
            | 0 => 0
            end;
        /std/print(Nat/to_str(f(3)))
        "#;

    assert_eq!(run(source), b"2");
}
