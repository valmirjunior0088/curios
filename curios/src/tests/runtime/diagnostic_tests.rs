//! What a runtime diagnostic names: source binder names, shortened globals, and infix index arithmetic.

use super::super::error;

#[test]
fn uses_source_binder_names() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat = n;
        let bad : Nat = f;
        bad
        "#;

    let error = error(source);
    assert!(
        error.contains("inferred: (n: Nat) -> Nat"),
        "binder lost its source name: {error}"
    );
    assert!(!error.contains('#'), "fresh-name suffix leaked: {error}");
}

// Two binders sharing a source name (shadowing) stay distinct in the message via a minimal numeric suffix — `n` and `n2` — instead of both reading `n` (axis (a) collision handling).
#[test]
fn disambiguates_shadowed_binders() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> ((n : Nat) -> Nat) = (k : Nat) => n;
        let bad : Nat = f;
        bad
        "#;

    let error = error(source);
    assert!(
        error.contains("inferred: (n: Nat) -> (n2: Nat) -> Nat"),
        "shadowed binders not disambiguated: {error}"
    );
    assert!(!error.contains('#'), "fresh-name suffix leaked: {error}");
}

// Globals print under their shortest in-scope spelling, not their fully qualified canonical path (axis (b)): `Vec` and `Nat`, never `std/Vec/Vec` or `sys/Nat`.
#[test]
fn shortens_global_names() {
    let source = r#"
        use /std/{Nat, Vec};
        let bad(n : Nat, v : Vec(Nat, n)) -> Nat = v;
        bad
        "#;

    let error = error(source);
    assert!(
        error.contains("inferred: Vec(Nat, n)"),
        "globals not shortened: {error}"
    );
    assert!(
        !error.contains("std/Vec"),
        "qualified inductive path leaked: {error}"
    );
    assert!(
        !error.contains("sys/"),
        "qualified intrinsic path leaked: {error}"
    );
}

// A mismatch report deep-normalizes both sides: the arithmetic in an index position is elaborated as concept-method dispatch (`+` ≙ `Add/add`), which, once resolution picks the intrinsic `Nat` witness, would otherwise surface as the compiler-internal `(sys/witness@N).0(0, 1)`. Normalizing collapses the literal case to its value (`1`), leaving no witness machinery in the message.
#[test]
fn collapses_witness_dispatch_in_index() {
    let source = r#"
        use /std/{Nat, Vec};
        let bad(@n : Nat) -> Vec(Nat, n) = Vec/cons(0, Vec/nil());
        bad
        "#;

    let error = error(source);
    assert!(
        error.contains("inferred: Vec(Nat, 1)"),
        "witness dispatch not collapsed to its value: {error}"
    );
    assert!(
        !error.contains("witness"),
        "internal witness name leaked: {error}"
    );
}

// The residual symbolic arithmetic a normalized index keeps is spelled in surface infix form, not the internal `Nat.add`/`Nat.succ` intrinsic spelling: the `n + m` and `n + 1` the source would have written.
#[test]
fn spells_index_arithmetic_infix() {
    let source = r#"
        use /std/{Nat, Vec};
        let bad(@n : Nat, @m : Nat, v : Vec(Nat, n), w : Vec(Nat, m)) -> Vec(Nat, n) =
            Vec/cons(0, Vec/append(v, w));
        bad
        "#;

    let error = error(source);
    assert!(
        error.contains("inferred: Vec(Nat, (n + m) + 1)"),
        "index arithmetic not spelled infix: {error}"
    );
}

// A name whose unfolding stalls keeps its name. `double` is a `rec`, so unfolding `double(n)` on a variable reaches the folded call's neutral, which the printer can only spell as the whole recursive group — twice, once per reference — with the author's `n` renamed against the binders the body brought in. The head stays as written and only the arguments normalize; the `2 + 3` and witness-collapse fixtures above are what still unfolds.
#[test]
fn keeps_the_name_of_a_stalled_unfolding() {
    let source = r#"
        use /std/{Nat, Eq};
        let double(n: Nat) -> Nat = match n | 0 => 0 | p + 1 => double(p) + 2 end;
        let claim(n: Nat) -> Eq(double(n), n * 2) = Eq/refl();
        claim
        "#;

    let error = error(source);
    assert!(
        error.contains("inferred: Eq(@Nat, double(n), double(n))"),
        "stalled unfolding not kept by name: {error}"
    );
    assert!(
        error.contains("expected: Eq(@Nat, double(n), "),
        "stalled unfolding not kept by name: {error}"
    );
    assert!(
        !error.contains("fold") && !error.contains("rec "),
        "recursive body leaked into the report: {error}"
    );
}
