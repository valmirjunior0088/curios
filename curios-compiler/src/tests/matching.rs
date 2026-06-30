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

// Client network IO (Phase A): `connect` rides the `Hdl` byte stream, so
// `Tcp/call` writes a request and drains the scripted response to EOF.
