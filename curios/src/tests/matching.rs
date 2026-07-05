use {curios_rt::MockHost, std::time::Duration};

// Regression test for a bug found while building the matrix pattern compiler:
// minting a synthetic binder for a single, unnested constructor arm (rather
// than reusing the written name directly) produced a core binder whose only
// label was that gensym — which the erasure pass's hint-based fresh naming
// then chained into another gensym, compounding until a reference outran its
// own binding. A plain flat match must still lower with no such indirection.
#[test]
fn flat_option_match_lowers_without_synthetic_indirection() {
    let source = r#"
        use /std/{Option, Nat, Io};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(y) => y
            | none() => 0
            end;
        Io/print(Nat/to_str(f(Option/some(5))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"5");
}

// The spec's own motivating example: a single tupled head, fully enumerated
// over two independent `Option`-shaped columns.
#[test]
fn nested_ctor_pattern_dispatches_by_shape() {
    let source = r#"
        use /std/{Option, Nat, Io};
        let f(a : Option(Nat), b : Option(Nat)) -> Nat =
            match (a, b)
            | (some(x), some(y)) => x + y
            | (some(x), none()) => x
            | (none(), some(y)) => y
            | (none(), none()) => 0
            end;
        Io/print(Nat/to_str(f(Option/some(3), Option/some(4))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

// A tuple value used as a match target directly — no constructor tag at all
// — desugars to plain projection, never a core `Match` node.
#[test]
fn tuple_match_target_projects_fields() {
    let source = r#"
        use /std/{Nat, Io};
        let f(p : { Nat, Nat }) -> Nat =
            match p
            | (x, y) => x + y
            end;
        Io/print(Nat/to_str(f((3, 4))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

// A struct value used as a match target directly, including field-punning.
#[test]
fn struct_match_target_projects_fields() {
    let source = r#"
        use /std/{Nat, Io};
        pub record Pair(A : Type, B : Type) : Type { fst : A, snd : B }
        let f(p : Pair(Nat, Nat)) -> Nat =
            match p
            | Pair { fst, snd } => fst + snd
            end;
        Io/print(Nat/to_str(f(Pair { fst = 3, snd = 4 })))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

// A struct match-arm pattern desugars to the same `proj`/`proj_label` calls
// an ordinary projection uses, so representation privacy is inherited
// automatically and unmodified — matching `struct_private_projection_rejected`
// in `structs.rs`, but reached through a match arm instead of `.0`.
#[test]
fn struct_arm_privacy_is_enforced() {
    let source = r#"
        use /std/{Nat, Io};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius : Type { Nat }
            pub let of_nat(n : Nat) -> Celsius = Celsius { n };
        end
        let c : Celsius/Celsius = Celsius/of_nat(42);
        match c
        | Celsius/Celsius { n } => Io/print(Nat/to_str(n))
        end
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("field") && error.contains("private"),
        "unexpected error: {error}"
    );
}

// Two rows write a tuple pattern of different arity in the same column —
// there is no single shape to explode into projections.
#[test]
fn matrix_match_rejects_inconsistent_tuple_arity() {
    let source = r#"
        use /std/{Nat, Io};
        let f(p : { Nat, Nat }) -> Nat =
            match p
            | (x, y) => x
            | (x,) => x
            end;
        Io/print(Nat/to_str(f((3, 4))))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("disagree on shape"),
        "unexpected error: {error}"
    );
}

// Two rows are identical in every column, several levels deep — an
// overlapping arm Path A's full-enumeration model has no priority order to
// resolve.
#[test]
fn matrix_match_rejects_duplicate_row() {
    let source = r#"
        use /std/{Option, Nat, Io};
        let f(a : Option(Nat), b : Option(Nat)) -> Nat =
            match (a, b)
            | (some(x), some(y)) => x + y
            | (some(x), some(y)) => x
            | (some(x), none()) => x
            | (none(), some(y)) => y
            | (none(), none()) => 0
            end;
        Io/print(Nat/to_str(f(Option/some(3), Option/some(4))))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("duplicate or overlapping"),
        "unexpected error: {error}"
    );
}

// A literal repeated constructor tag in a flat, single-column match — the
// pre-existing bug this work also fixed (it used to silently collapse to
// whichever arm's tag survived `BTreeMap` collection, dropping the other).
#[test]
fn matrix_match_rejects_duplicate_flat_tag() {
    let source = r#"
        use /std/{Option, Nat, Io};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(a) => a
            | some(b) => b
            | none() => 0
            end;
        Io/print(Nat/to_str(f(Option/some(3))))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("duplicate or overlapping"),
        "unexpected error: {error}"
    );
}

// A plain binder row (Path A's forbidden catch-all) mixed with a concrete
// constructor row in the same column.
#[test]
fn matrix_match_rejects_mixed_binder_and_ctor_column() {
    let source = r#"
        use /std/{Option, Nat, Io};
        let f(o : Option(Nat)) -> Nat =
            match o
            | x => 0
            | some(y) => y
            end;
        Io/print(Nat/to_str(f(Option/some(3))))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("disagree on shape"),
        "unexpected error: {error}"
    );
}

// A dependent motive requires the head to dispatch on a constructor tag
// directly — there is no core `Match` node for a tuple-headed match to
// attach it to.
#[test]
fn matrix_match_rejects_dependent_motive_on_tuple_head() {
    let source = r#"
        use /std/{Nat, Io};
        let f(p : { Nat, Nat }) -> Nat =
            match p : (q) => Nat
            | (x, y) => x
            end;
        Io/print(Nat/to_str(f((3, 4))))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("dependent motive"),
        "unexpected error: {error}"
    );
}

#[test]
fn effectful_match_scrutinee_runs_once() {
    let source = r#"
        use /std/{File, Io, Task};
        match Task/block_on(File/with("log.txt", Io/Mode/append(), (f) => File/write(f, /std/Str/to_bin("x"))))
        | success(_) => Io/print("ok")
        | failure(_) => Io/print("error")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"ok");
    assert_eq!(io.file(b"log.txt"), Some(b"x".to_vec()));
}
