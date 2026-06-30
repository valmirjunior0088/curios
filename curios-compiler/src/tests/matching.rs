use {curios_runtime::MockHost, std::time::Duration};

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

// The public file ops run on the opaque handle inside the bracket: `File/read`
// pulls bytes from the `File` that `using` hands the body, and `using` closes
// it afterwards.
#[test]
fn matrix_nested_constructor_pattern() {
    let source = r#"
        use /std/{Nat, Io};
        use /std/Lst/*;
        let xs : Lst(Nat) = cons(4, cons(5, nil()));
        let out : Nat =
            match xs
            | cons(x, cons(y, _)) => Nat/add(x, y)
            | cons(x, nil())      => x
            | nil()               => 0
            end;
        Io/print(Nat/to_str(out))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"9");
}

// A `Nat` literal nested in a constructor payload compiles to a `switch`: the
// `0`-headed list takes the special arm, any other head the binder default.
#[test]
fn matrix_nat_literal_in_nested_column() {
    let source = r#"
        use /std/{Nat, Io};
        use /std/Lst/*;
        let special : Lst(Nat) = cons(0, cons(5, nil()));
        let other : Lst(Nat)   = cons(7, nil());
        let head_code(xs : Lst(Nat)) -> Nat =
            match xs
            | cons(0, _) => 100
            | cons(x, _) => x
            | nil()      => 0
            end;
        Io/print(Nat/to_str(Nat/add(head_code(special), head_code(other))))
        "#;

    // special -> 100 (head is 0), other -> 7 (head binder). 100 + 7 = 107.
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"107");
}

// A top-level `Nat` match with a *named* default falls through to the matrix —
// the dedicated nat-match form only accepts an anonymous `| _ =>`, so binding the
// non-literal case is new. It compiles to a `switch` whose default binds `k`.
#[test]
fn matrix_nat_literal_named_default() {
    let source = r#"
        use /std/{Nat, Io};
        let label(n : Nat) -> Nat =
            match n
            | 0 => 100
            | 1 => 200
            | k => Nat/add(k, 1000)
            end;
        Io/print(Nat/to_str(Nat/add(Nat/add(label(0), label(1)), label(7))))
        "#;

    // 100 + 200 + 1007 = 1307.
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"1307");
}

// A `Nat` literal nested inside a struct field pattern: the struct column expands
// to its labels, and the `tag` sub-column dispatches via `switch`.
#[test]
fn matrix_nat_literal_in_struct_field() {
    let source = r#"
        use /std/{Nat, Io};
        pub record Tagged { tag : Nat, val : Nat }
        let read(t : Tagged) -> Nat =
            match t
            | Tagged { tag = 0, val = v } => v
            | Tagged { tag = _, val = _ } => 999
            end;
        Io/print(Nat/to_str(read(Tagged { tag = 0, val = 42 })))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"42");
}

// A `_` fallthrough at an inductive column expands into the *unlisted* constructors:
// here it covers `nil()` (and any non-matching `cons`), which needs the
// constructor's arity from the registry.
#[test]
fn matrix_wildcard_expands_unlisted_constructors() {
    let source = r#"
        use /std/{Nat, Io};
        use /std/Lst/*;
        let head_or_zero(xs : Lst(Nat)) -> Nat =
            match xs
            | cons(x, _) => x
            | _          => 0
            end;
        let full : Lst(Nat)  = cons(9, nil());
        let empty : Lst(Nat) = nil();
        Io/print(Nat/to_str(Nat/add(head_or_zero(full), head_or_zero(empty))))
        "#;

    // full -> 9, empty -> 0 (the `_` materializes the nil arm). 9 + 0 = 9.
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"9");
}

// Two rows may share a head constructor, distinguished by a nested literal — the
// whole point of an ordered matrix over a per-constructor map.
#[test]
fn matrix_repeated_constructor_head() {
    let source = r#"
        use /std/{Nat, Io};
        use /std/Lst/*;
        let classify(xs : Lst(Nat)) -> Nat =
            match xs
            | cons(0, _) => 1
            | cons(1, _) => 2
            | cons(_, _) => 3
            | nil()      => 0
            end;
        let a : Lst(Nat) = cons(0, nil());
        let b : Lst(Nat) = cons(1, nil());
        let c : Lst(Nat) = cons(8, nil());
        Io/print(Nat/to_str(Nat/add(Nat/add(classify(a), classify(b)), classify(c))))
        "#;

    // 1 + 2 + 3 = 6.
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"6");
}

// Multiple scrutinees fall out of a tuple scrutinee: `(a, b)` with refutable
// fields is a one-row matrix that expands into two `Bln` columns.
#[test]
fn matrix_multi_scrutinee_via_tuple() {
    let source = r#"
        use /std/{Nat, Io, Bln};
        let combine(a : Bln, b : Bln) -> Nat =
            match (a, b)
            | (true, true)  => 3
            | (true, false) => 2
            | (false, _)    => 1
            end;
        Io/print(Nat/to_str(combine(true, false)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"2");
}

// Coverage is left to core: an inductive match that lists neither every constructor
// nor a `_` fallthrough is rejected as non-exhaustive (the `nil` arm is missing).
#[test]
fn matrix_non_exhaustive_missing_constructor_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        use /std/Lst/*;
        let head(xs : Lst(Nat)) -> Nat =
            match xs
            | cons(0, _) => 100
            | cons(x, _) => x
            end;
        Io/print(Nat/to_str(head(nil())))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("missing match case") && error.contains("nil"),
        "unexpected error: {error}"
    );
}

// Expanding a `_` at an inductive column needs the constructor's inductive; when the
// constructors are not in scope (no `use`), the tag cannot be resolved and the
// match is rejected with an actionable error.
#[test]
fn matrix_wildcard_unresolved_constructor_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        induct Shape | dot() | line(Nat) end
        let area(s : Shape) -> Nat =
            match s
            | line(n) => n
            | _       => 0
            end;
        Io/print(Nat/to_str(area(Shape/dot())))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("line") && error.contains("resolve"),
        "unexpected error: {error}"
    );
}

// === Str (std/Str) ======================================================

// `"..."` is a `Str` primitive value (UTF-8 by construction); `Io/print` writes
// a `Str` straight to stdout.
#[test]
fn let_tuple_destructures() {
    // `let (a, b) = …` binds each leaf by projection off a fresh temp. The
    // annotation types that temp — a bare tuple literal is uninferable on its
    // own (the same constraint a non-destructuring `let t = (+3, +4)` hits).
    let source = r#"
        let (a, b) : { std/Int, std/Int } = (+3, +4);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(b)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"+4");
}

#[test]
fn nested_let_tuple_destructures() {
    // Nested tuple patterns project recursively: `c` is `t.1.1`. Only the outer
    // binding needs an annotation — the inner `(b, c)` projects off `t.1`, whose
    // type the elaborator infers from the projection (unlike a bare literal).
    let source = r#"
        let (a, (b, c)) : { std/Int, { std/Int, std/Int } } = (+1, (+2, +3));
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(c)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"+3");
}

#[test]
fn let_tuple_destructures_without_annotation() {
    // PROTOTYPE CHECK: with Infer-mode tuple synthesis, a bare tuple literal no
    // longer needs an annotation — `(+3, +4)` infers `{ std/Int, std/Int }`.
    let source = r#"
        let (a, b) = (+3, +4);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(b)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"+4");
}

#[test]
fn let_three_tuple_destructures() {
    // A genuine 3-tuple (not a nested pair): exercises projection at index 2 and
    // a three-pattern binder. `c` is `t.2`.
    let source = r#"
        let (a, b, c) : { std/Int, std/Int, std/Int } = (+10, +20, +30);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(c)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"+30");
}

#[test]
fn func_tuple_param_destructures() {
    // A function-definition-sugar parameter destructures its argument; the
    // Π-binder is anonymous, so the result type cannot mention the whole pair.
    let source = r#"
        let snd((a, b) : { std/Int, std/Int }) -> std/Int = b;
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(snd((+7, +8)))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"+8");
}

#[test]
fn lambda_tuple_param_destructures() {
    // A bare lambda taking one pair parameter needs its own parens: `((a, b))`.
    let source = r#"
        let fst : (_ : { std/Int, std/Int }) -> std/Int = ((a, b)) => a;
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(fst((+5, +6)))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"+5");
}

#[test]
fn match_arm_tuple_destructures() {
    // A constructor whose payload is a tuple destructures inside the arm binder.
    let source = r#"
        induct Boxed
        | box({ std/Int, std/Int })
        end
        let value : Boxed = Boxed/box((+9, +1));
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(
            match value : std/Int
            | box((x, y)) => x
            end
        )))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"+9");
}

// Client network IO (Phase A): `connect` rides the `Hdl` byte stream, so
// `Tcp/call` writes a request and drains the scripted response to EOF.
