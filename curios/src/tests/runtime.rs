use {
    super::run,
    curios_runtime::{ForeignBindings, MockHost},
    std::{path::Path, time::Duration},
};

#[test]
fn nullary_closure_survives_erasure_and_codegen() {
    // A nullary closure stored in an inductive field and called indirectly via a
    // `call_ref` — the erasure+codegen path that needed `clsr_arities`. Zero-arity
    // closures survive it, which is what lets the suspension/continuation thunks
    // drop their dummy unit argument (`() -> T` rather than `({}) -> T`). Output
    // proves the suspended effect fired on `force`.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Handle, Str};
        induct Susp(A : Type) : Type
        | now(A)
        | later(() -> Susp(A))
        end
        rec force(@A : Type, s : Susp(A)) -> A =
            match s : A
            | now(a) => a
            | later(k) => force(k())
            end;
        let prog : Susp({}) =
            Susp/later(() =>
                let w = Handle/write(Handle/stdout, Str/to_bytes("ok"));
                Susp/now(()));
        let r = force(prog);
        Handle/write(Handle/stdout, Str/to_bytes("!"))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"ok!");
}

#[test]
fn end_to_end() {
    let source = r#"
        induct Pair : Type
        | left(std/Int)
        | right(std/Flt)
        end
        let pair : Pair = Pair/left(+42);
        let score : (_ : Pair) -> std/Int = (p) =>
            match p : std/Int
            | left(_) => +42
            | right(_) => +7
            end;
        std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(std/Int/to_str(score(pair))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"+42");
}

// Local binders shadow like-named *module* bindings, and a local name never
// leaks past its lexical scope. Inside `mod Foo` the module binding is `Foo/go`:
// an inner `let go` must shadow it (so `shadowed` is 3, not the captured 7),
// while a `go` that is a sibling of an inner `let go = 3` — reached only after
// that scope closes — must resolve back to `Foo/go` (so `sibling` is 7, not a
// leaked, unbound bare `go`). Encoded as 3*10 + 7 = 37, so the unlawful-capture
// regression reads 77 and a scope leak fails to compile.
#[test]
fn local_binders_shadow_module_bindings_without_leaking() {
    let source = r#"
        use /std/{Nat, Handle, Str};
        mod Foo
            pub let go : /std/Nat = 7;
            pub let shadowed : /std/Nat =
                let go : /std/Nat = 3;
                go;
            pub let sibling : /std/Nat =
                let probe : /std/Nat = (let go : /std/Nat = 3; go);
                go;
        end
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Nat/add(Nat/mul(Foo/shadowed, 10), Foo/sibling))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"37");
}

// Named fields end to end: a dependent record (the vector's length indexes its
// type) constructed with written names, consumed through `.label` and `.index`
// access on the same value — both resolve to the same positional projection.
#[test]
fn triangular_sum() {
    let source = r#"
        let result : std/Nat =
            match 5 : std/Nat
            | 0 => 0
            | pred + 1; ih => std/Nat/add(ih, pred)
            end;
        std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(std/Nat/to_str(result)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"10");
}

#[test]
fn multi_arg_function() {
    let source = r#"
        let add : (std/Int, std/Int) -> std/Int = (x, y) => std/Int/add(x, y);
        std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(std/Int/to_str(add(+3, +4))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"+7");
}

#[test]
fn curried_function() {
    let source = r#"
        let add : (std/Int) -> (std/Int) -> std/Int = (x) => (y) => std/Int/add(x, y);
        std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(std/Int/to_str(add(+3)(+4))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"+7");
}

#[test]
fn bang_dispatches_through_a_user_monad_witness() {
    // A user-declared Identity monad: `Box(A)` wraps a value, its witness's
    // `bind` just applies the continuation. Each `!` desugars to
    // `/syn/Monad/bind(action, cont)`; the action's `Box(Nat)` type pins
    // `M := Box` (flex-apply imitation) and resolves `monad_box` — the same
    // path a std monad takes, exercised end-to-end on a user type.
    let source = r#"
        use /std/{Nat, Handle, Str, Monad};
        pub struct Box(A : Type) : pub Type { unbox : A }
        satisfy Monad(Box) {
            pure(@A, x) = Box { unbox = x },
            bind(@A, @B, m, f) = f(m.unbox)
        }
        let a : Box(Nat) = Box { unbox = 3 };
        let b : Box(Nat) = Box { unbox = 4 };
        let result : Box(Nat) = Monad/pure(Nat/add(a!, b!));
        /std/print(Nat/to_str(result.unbox))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

#[test]
fn bang_std_parse_threads_bangs_left_to_right() {
    // The real `std/Parse` monad, sequenced with bare `!` — each site resolves
    // the `Monad(Parse)` witness from the action's type.
    // Two `any_byte!`s read consecutive bytes; reflecting through `Byte/to_nat`
    // and using a *non-commutative* `Nat/sub`
    // pins the evaluation order: on "BA" the first byte is 'B' (66) and the second
    // 'A' (65), so the result is 66 - 65 = 1 (the reversed order would saturate to 0).
    let source = r#"
        use /std/{Parse, Byte, Nat, Result, Handle};

        let parser : Parse/Parse(Nat) =
            Parse/pure(Nat/sub(Byte/to_nat(Parse/any_byte!), Byte/to_nat(Parse/any_byte!)));

        match Parse/run(parser, /std/Str/to_bytes("BA")) : {}
        | success(n) => /std/print(Nat/to_str(n))
        | failure(msg) => /std/print(msg)
        end
        "#;

    let base = Path::new(env!("CARGO_MANIFEST_DIR"));
    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("failed to parse source");
    let loader = curios_text::RootSource::file_system(base.to_path_buf());

    let (system, io) = MockHost::builder().build();
    crate::run_entrypoint(Duration::from_secs(10), &entrypoint, loader, system)
        .expect("expected result");
    assert_eq!(io.output(), b"1");
}

#[test]
fn bang_region_mixes_action_types() {
    // A single region sequences two actions of *different* payload types: a
    // `Parse(Bytes)` (`take_while`) and a `Parse(Byte)` (`any_byte`). Each `!`
    // site elaborates its own `/syn/Monad/bind` application with fresh
    // implicits (`?A := Bytes` for the first, `?A := Byte` for the second), while
    // the shared continuation typing forces one monad for the region. On "AB":
    // `take_while(is_a)` reads "A" (stops at 'B'), then `any_byte` reads 'B'
    // (66); `Bytes/append("A", 66)` is "AB".
    let source = r#"
        use /std/{Parse, Byte, Bytes, Bool, Result, Handle, Str};

        let is_a : (Byte) -> Bool = (b) => b == 0x41;

        let parser : Parse/Parse(Bytes) =
            Parse/pure(Bytes/append(Parse/take_while(is_a)!, Parse/any_byte!));

        match Parse/run(parser, /std/Str/to_bytes("AB")) : {}
        | success(s) =>
            match Str/of_bytes(s) : {}
            | some(t) => /std/print(t)
            | none() => /std/print("invalid utf-8")
            end
        | failure(msg) => /std/print(msg)
        end
        "#;

    let base = Path::new(env!("CARGO_MANIFEST_DIR"));
    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("failed to parse source");
    let loader = curios_text::RootSource::file_system(base.to_path_buf());

    let (system, io) = MockHost::builder().build();
    crate::run_entrypoint(Duration::from_secs(10), &entrypoint, loader, system)
        .expect("expected result");
    assert_eq!(io.output(), b"AB");
}

#[test]
fn folds_constant_arg_through_let_function() {
    // `let f(x) = Nat/add(x, 1); f(3)` must fold end-to-end to a literal `4`
    // returned through main's bodyless return continuation.
    let source = r#"
        use /std/{Nat};
        let f(x : Nat) -> Nat = Nat/add(x, 1);
        f(3)
        "#;

    let entrypoint = source.parse::<curios_text::Entrypoint>().unwrap();

    let mut optimized = None;
    curios_pipeline::compile_entrypoint(
        Duration::from_secs(10),
        &entrypoint,
        curios_text::RootSource::none(),
        |stage| {
            if let curios_pipeline::Stage::ContOptm(module) = stage {
                optimized = Some(module.clone());
            }
        },
    )
    .expect("compile succeeded");

    let optimized = optimized.expect("Stage::ContOptm observed");
    let entry = optimized.entry().expect("module has entry");
    let main = optimized
        .function(entry)
        .expect("entry function is defined");
    let returns_four = optimized.nodes().iter().flatten().any(|node| {
        matches!(
            node,
            curios_cont::CpsNode::ApplyCont(curios_cont::CpsEdge { target, args })
                if *target == main.return_cont
                    && matches!(args.as_slice(), [curios_cont::CpsAtom::Literal(
                        curios_cont::CpsLiteral::Nat(4)
                    )])
        )
    });
    assert!(
        returns_four,
        "expected main to return literal 4 through {}, got:\n{optimized}",
        main.return_cont,
    );
}

#[test]
fn fmt_print_partial_evaluation_reduces_residual() {
    // End-to-end residue guard for the staging stack on
    // `Fmt/print("%s is %d")(name)(30)` with a *runtime* `%s` argument. The
    // ersd `evaluate` pass folds the closed prefix — the format-string parse
    // (Parse combinators and the segment UTF-8 revalidation included) runs at
    // compile time and `Fmt/print(lit)` reifies as the curried hole-filling
    // closure over a constant `Fmt` spine. What stays runtime is exactly the
    // runtime work: specialized `go_with` over the spine, the `%s` path
    // (`Str/trim` and stdin UTF-8 validation through `classify`), and the `%d`
    // path (`Nat/to_str`'s digit producer). The single-entry `go_with` spine is
    // then contified into the entry, so the boundary is pinned by the surviving
    // `%d` digit producer together with the absence of the generic `Fmt/print`
    // driver and the compile-time `Parse` combinators, without depending on a
    // legacy backend function-count metric.
    let source = r#"
        use /std/{Str, Handle, Bytes, Fmt};

        match Handle/read(Handle/stdin, 1024) : {}
        | chunk(bytes) =>
            match Str/of_bytes(bytes) : {}
            | some(s) => Fmt/print("%s is %d")(Str/trim(s))(30)
            | none() => /std/print("invalid input")
            end
        | eof() => /std/print("invalid input")
        | error(_) => /std/print("invalid input")
        end
        "#;

    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("failed to parse source")
        .with_type("()".parse().unwrap());

    let mut cont_optm = None;

    let (wasm_module, _foreigns) = curios_pipeline::compile_entrypoint(
        Duration::from_secs(15),
        &entrypoint,
        curios_text::RootSource::none(),
        |stage| {
            if let curios_pipeline::Stage::ContOptm(module) = stage {
                cont_optm = Some(format!("{module}"));
            }
        },
    )
    .expect("compile succeeded");

    let cont = cont_optm.expect("Stage::ContOptm observed");
    assert!(
        cont.contains("/std/Nat/to_str")
            && !cont.contains("/std/Fmt/print")
            && !cont.contains("/std/Parse/"),
        "expected only the specialized formatting spine after staging, got:\n{cont}",
    );

    let (system, io) = MockHost::builder().stdin_lines(["Alice"]).build();
    crate::run_wasm(&wasm_module, system, ForeignBindings::empty()).expect("execution succeeded");
    assert_eq!(io.output(), b"Alice is 30");
}

#[test]
fn fmt_print_runtime_args_specializes_spine() {
    // The mixed case: a literal format string with runtime hole arguments.
    // The ersd `evaluate` pass folds the parse to a constant `Fmt` spine, and
    // `specialize` unrolls `go_with` over it — the ersd-optm module carries
    // the minted spine items and neither the format-string parser nor the
    // generic fold survives to codegen.
    let source = r#"
        use /std/{Str, Handle, Bytes, Fmt};

        match Handle/read(Handle/stdin, 1024) : {}
        | chunk(bytes) =>
            match Str/of_bytes(bytes) : {}
            | some(s) => Fmt/print("%s is %d")(Str/trim(s))(30)
            | none() => /std/print("invalid input")
            end
        | eof() => /std/print("invalid input")
        | error(_) => /std/print("invalid input")
        end
        "#;

    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("failed to parse source")
        .with_type("()".parse().unwrap());

    let mut ersd_optm = None;

    let (wasm_module, _foreigns) = curios_pipeline::compile_entrypoint(
        Duration::from_secs(15),
        &entrypoint,
        curios_text::RootSource::none(),
        |stage| {
            if let curios_pipeline::Stage::ErsdOptm(module) = stage {
                ersd_optm = Some(format!("{module}"));
            }
        },
    )
    .expect("compile succeeded");

    let ersd = ersd_optm.expect("Stage::ErsdOptm observed");
    assert!(
        ersd.contains("$/std/Fmt/go_with("),
        "expected the spine-specialized fold called, got:\n{ersd}",
    );
    assert!(
        !ersd.contains("parse_fmt") && !ersd.contains("/std/Parse/") && !ersd.contains("rec ~"),
        "expected the parser and the generic fold pruned, got:\n{ersd}",
    );

    let (system, io) = MockHost::builder().stdin_lines(["Bob"]).build();
    crate::run_wasm(&wasm_module, system, ForeignBindings::empty()).expect("execution succeeded");
    assert_eq!(io.output(), b"Bob is 30");
}

#[test]
fn fmt_print_err_formats_to_stderr() {
    // Same staging as `Fmt/print`, routed through `/std/print_err`. MockIo
    // captures stdout and stderr concatenated in write order, so the ordering
    // also shows the stderr write really happened between the stdout ones.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Fmt, Handle};
        let a = /std/print("before;");
        let b = Fmt/print_err("%s: %d;")("code")(3);
        /std/print("after")
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"before;code: 3;after");
}

#[test]
fn fmt_print_constant_args_collapses_at_ersd() {
    // The fully-constant case: every input to `Fmt/print` is a literal, so the
    // ersd `evaluate` pass runs the *entire* program at compile time and
    // residualizes the one effect boundary — the ersd-optm module's body is a
    // single `#/std/print(<final bytes>)` call, the `Parse` combinator web
    // and `Fmt`'s parser are pruned, and only the `/std/print` → `Handle/write`
    // plumbing reaches codegen.
    let source = r#"
        use /std/{Fmt};

        Fmt/print("x = %d, s = %s\n")(42)("hello")
        "#;

    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("failed to parse source")
        .with_type("()".parse().unwrap());

    let mut ersd_optm = None;
    let mut cont_optm = None;

    let (wasm_module, _foreigns) = curios_pipeline::compile_entrypoint(
        Duration::from_secs(15),
        &entrypoint,
        curios_text::RootSource::none(),
        |stage| match stage {
            curios_pipeline::Stage::ErsdOptm(module) => ersd_optm = Some(format!("{module}")),
            curios_pipeline::Stage::ContOptm(module) => cont_optm = Some(format!("{module}")),
            _ => {}
        },
    )
    .expect("compile succeeded");

    let ersd = ersd_optm.expect("Stage::ErsdOptm observed");
    // "x = 42, s = hello\n", already formatted, as the residual call's operand.
    // Dead spine leftovers linger in the entry block (pruning drops items, not
    // block statements) — the Cont sweep below is where they must be gone.
    assert!(
        ersd.contains("$/std/print(x\"78203d2034322c2073203d2068656c6c6f0a\")"),
        "expected the folded print residual, got:\n{ersd}",
    );
    assert!(
        !ersd.contains("/std/Parse/"),
        "expected the parser web pruned, got:\n{ersd}",
    );

    let cont = cont_optm.expect("Stage::ContOptm observed");
    assert!(
        !cont.contains("/std/Fmt/") && !cont.contains("/std/Parse/") && !cont.contains("[lambda]"),
        "expected the constant program to collapse to the write loop, got:\n{cont}",
    );

    let (system, io) = MockHost::builder().build();
    crate::run_wasm(&wasm_module, system, ForeignBindings::empty()).expect("execution succeeded");
    assert_eq!(io.output(), b"x = 42, s = hello\n");
}

#[test]
fn diagnostic_uses_source_binder_names() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat = n;
        let bad : Nat = f;
        bad
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("inferred: (n : Nat) -> Nat"),
        "binder lost its source name: {error}"
    );
    assert!(!error.contains('#'), "fresh-name suffix leaked: {error}");
}

// Two binders sharing a source name (shadowing) stay distinct in the message
// via a minimal numeric suffix — `n` and `n2` — instead of both reading `n`
// (axis (a) collision handling).
#[test]
fn diagnostic_disambiguates_shadowed_binders() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> ((n : Nat) -> Nat) = (k : Nat) => n;
        let bad : Nat = f;
        bad
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("inferred: (n : Nat) -> (n2 : Nat) -> Nat"),
        "shadowed binders not disambiguated: {error}"
    );
    assert!(!error.contains('#'), "fresh-name suffix leaked: {error}");
}

// Globals print under their shortest in-scope spelling, not their fully
// qualified canonical path (axis (b)): `Vec` and `Nat`, never `std/Vec/Vec`
// or `sys/Nat`.
#[test]
fn diagnostic_shortens_global_names() {
    let source = r#"
        use /std/{Nat, Vec};
        let bad(n : Nat, v : Vec(Nat, n)) -> Nat = v;
        bad
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
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
        "qualified prim path leaked: {error}"
    );
}

// A mismatch report deep-normalizes both sides: the arithmetic in an index
// position is elaborated as concept-method dispatch (`+` ≙ `Add/add`), which,
// once resolution picks the primitive `Nat` witness, would otherwise surface as
// the compiler-internal `(sys/witness#N).0(0, 1)`. Normalizing collapses the
// literal case to its value (`1`), leaving no witness machinery in the message.
#[test]
fn diagnostic_collapses_witness_dispatch_in_index() {
    let source = r#"
        use /std/{Nat, Vec};
        let bad(@n : Nat) -> Vec(Nat, n) = Vec/cons(0, Vec/nil());
        bad
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("inferred: Vec(Nat, 1)"),
        "witness dispatch not collapsed to its value: {error}"
    );
    assert!(
        !error.contains("witness"),
        "internal witness name leaked: {error}"
    );
}

// The residual symbolic arithmetic a normalized index keeps is spelled in
// surface infix form, not the internal `Nat.add`/`Nat.succ` primitive spelling:
// the `n + m` and `n + 1` the source would have written.
#[test]
fn diagnostic_spells_index_arithmetic_infix() {
    let source = r#"
        use /std/{Nat, Vec};
        let bad(@n : Nat, @m : Nat, v : Vec(Nat, n), w : Vec(Nat, m)) -> Vec(Nat, n) =
            Vec/cons(0, Vec/append(v, w));
        bad
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("inferred: Vec(Nat, (n + m) + 1)"),
        "index arithmetic not spelled infix: {error}"
    );
}

// A struct type is nominal: it never converts with a structural tuple type of
// the same fields.
#[test]
fn random_bin_returns_requested_length() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"std/Handle/write(std/Handle/stdout, /std/rand/bin(8))"#,
        system,
    )
    .expect("expected result");

    let output = io.output();
    assert_eq!(output.len(), 8);
}

#[test]
fn nat_of_str_returns_option() {
    // `123` parses; `12a` (non-digit) and the empty string are `none`, taking
    // the `unwrap_or` defaults — `123 + 7 + 9`.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Nat, Str, Option, Handle};
        let ok = Option/unwrap_or(Nat/of_str("123"), 0);
        let bad = Option/unwrap_or(Nat/of_str("12a"), 7);
        let empty = Option/unwrap_or(Nat/of_str(""), 9);
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Nat/add(Nat/add(ok, bad), empty))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"139");
}

#[test]
fn int_of_str_returns_option() {
    // `-5` and `+7` parse (compared by magnitude); `x` is `none` → default `+3`.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Nat, Int, Str, Option, Handle};
        let neg = Int/abs(Option/unwrap_or(Int/of_str("-5"), +0));
        let pos = Int/abs(Option/unwrap_or(Int/of_str("+7"), +0));
        let bad = Int/abs(Option/unwrap_or(Int/of_str("x"), +3));
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Nat/add(Nat/add(neg, pos), bad))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"15");
}

#[test]
fn flt_of_str_returns_option() {
    // `12.0`, `.5` (empty integer part), and `1e3` parse; `abc` is `none` →
    // default `+4.0`. Values are truncated to `Nat` for an exact assertion:
    // `12 + (0.5*2) + 1000 + 4`.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Nat, Flt, Str, Option, Handle};
        let whole = Flt/to_nat(Option/unwrap_or(Flt/of_str("12.0"), +0.0));
        let half = Flt/to_nat(Flt/mul(Option/unwrap_or(Flt/of_str(".5"), +0.0), +2.0));
        let exp = Flt/to_nat(Option/unwrap_or(Flt/of_str("1e3"), +0.0));
        let bad = Flt/to_nat(Option/unwrap_or(Flt/of_str("abc"), +4.0));
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Nat/add(Nat/add(whole, half), Nat/add(exp, bad)))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"1017");
}

#[test]
fn option_result_char_helpers() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Option, Result, Char, Nat, Str, Handle};
        let opt = Option/unwrap_or(Option/map(Option/some(4), (x : Nat) => Nat/add(x, 1)), 0);
        let res0 : Result(Nat, Nat) = Result/success(5);
        let res = Result/unwrap_or(Result/map_success(res0, (x : Nat) => Nat/mul(x, 2)), 0);
        let up = Char/to_ascii_upper('a');
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Nat/add(Nat/add(opt, res), Char/to_nat(up)))))
        "#,
        system,
    )
    .expect("expected result");

    // opt = 5, res = 10, up = 'A' = 65  ->  80
    assert_eq!(io.output(), b"80");
}

#[test]
fn clock_diff_of_two_distinct_now_readings() {
    // Two scripted wall readings 30 s + 400 ns apart. `time/Instant/now`
    // referenced twice must perform two *distinct* host calls (the
    // nullary-effect distinctness the struct-head reduction relies on), so the
    // diff is the gap between them, not zero.
    let (system, io) = MockHost::builder()
        .wall([(1, 100, 500), (1, 130, 900)])
        .build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        let a = /std/time/Instant/now();
        let b = /std/time/Instant/now();
        let d = /std/time/Instant/diff(b, a);
        std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(/std/Nat/to_str(/std/time/Duration/secs(d))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"30");
}

#[test]
fn clock_mono_reads_scripted_elapsed() {
    let (system, io) = MockHost::builder().mono([(2, 7)]).build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        let e = /std/time/Instant/elapsed();
        std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(/std/Nat/to_str(/std/time/Duration/secs(e))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"2");
}

#[test]
fn cell_get_returns_init_value() {
    // Round-trip: mint a cell then read it back.
    assert_eq!(
        run(r#"
            use /std/{Cell, Handle, Nat, Str};
            let n : Nat = 42;
            let cell = Cell/new(n);
            /std/print(Nat/to_str(Cell/get(cell)))
        "#),
        b"42",
    );
}

#[test]
fn cell_set_overwrites_value() {
    // Write then read: the getter sees the new value, not the init.
    assert_eq!(
        run(r#"
            use /std/{Cell, Handle, Nat, Str};
            let z : Nat = 0;
            let cell = Cell/new(z);
            let _ = Cell/set(cell, 99);
            /std/print(Nat/to_str(Cell/get(cell)))
        "#),
        b"99",
    );
}

#[test]
fn cell_two_cells_are_distinct() {
    // Two cells minted with the same value are independent heap objects.
    // Setting one must not affect the other.
    assert_eq!(
        run(r#"
            use /std/{Cell, Handle, Nat, Str};
            let n : Nat = 7;
            let a = Cell/new(n);
            let b = Cell/new(n);
            let _ = Cell/set(a, 1);
            /std/print(Nat/to_str(Cell/get(b)))
        "#),
        b"7",
    );
}

#[test]
fn match_reads_an_effectful_scrutinee_once() {
    // Erasure aliases a non-variable scrutinee before projecting: the `k`
    // binder below is `head - 1` over the *alias*, not a re-erased
    // `Cell/get(c) - 1` — which would re-read the cell after the arm's
    // `Cell/set`, making `k` 0 - 1 (monus) = 0 and `x` 1 instead of 5.
    assert_eq!(
        run(r#"
            use /std/{Cell, Handle, Nat, Str};
            let n : Nat = 5;
            let c = Cell/new(n);
            let x = match Cell/get(c)
                | 0 => 0
                | k + 1; ih =>
                    let _ = Cell/set(c, 0);
                    k + 1
                end;
            /std/print(Nat/to_str(x))
        "#),
        b"5",
    );
}

#[test]
fn accumulation_loops_are_linear_by_construction() {
    // The rope representation's whole promise: a naive 100k-step `Bytes/concat`
    // accumulation loop is O(n) with no optimizer recognition anywhere — each
    // step is one node allocation, and the single read at the end forces once.
    // The pre-rope representation copied the accumulator per step (Θ(n²), tens
    // of minutes at this size); a regression fails on the timeout. The final
    // slice + print also pins the force → memo → host-write path end to end.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(60),
        r#"
        use /std/{Handle, Bytes, Nat, Str};
        rec go(i : Nat, acc : Bytes) -> Bytes =
            match i
            | 0 => acc
            | k + 1; ih => go(k, Bytes/concat(acc, Str/to_bytes("0123456789")))
            end;
        let built = go(100000, x\);
        let head = Bytes/slice(built, 0, 10);
        let _ = Handle/write(Handle/stdout, head);
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Bytes/len(built))))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"01234567891000000");
}

#[test]
fn peel_loops_are_linear_by_construction() {
    // The window (`view`) shape's whole promise, the consumption-side mirror of
    // `accumulation_loops_are_linear_by_construction`: a naive head/tail peel
    // over 100k bytes is O(n) with no optimizer recognition anywhere — the
    // first read forces once, then every tail is an O(1) collapsed window and
    // every head an O(1) read-through. The tail escapes through a `Cell` each
    // step, so no compile-time pass (worker_wrapper's cursor, slice
    // forwarding) can rescue it: a copying slice would be Θ(n²) and fail on
    // the timeout. Matching directly on `Cell/get(c)` also leans on erasure's
    // scrutinee alias — the cell must be read once per match, not once per
    // projection (the head read lands *after* the `Cell/set` otherwise).
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(60),
        r#"
        use /std/{Handle, Byte, Bytes, Nat, Str, Cell};
        rec build(i : Nat, acc : Bytes) -> Bytes =
            match i
            | 0 => acc
            | k + 1; ih => build(k, Bytes/concat(acc, Str/to_bytes("0123456789")))
            end;
        let built = build(10000, x\);
        let c = Cell/new(built);
        rec drain(fuel : Nat, acc : Nat) -> Nat =
            match fuel
            | 0 => acc
            | f + 1; ih =>
                match Cell/get(c)
                | x\ => acc
                | x\h\..t; ih2 =>
                    let _ = Cell/set(c, t);
                    drain(f, acc + (Byte/to_nat(h) - 48))
                end
            end;
        let total = drain(Bytes/len(built) + 1, 0);
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(total)))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"450000");
}

// A local `rec` nested inside another local `rec`'s body: `go` (inner) is an
// ordinary term-level construct here — never lambda-lifted, never spliced
// anywhere — so it just works, elaborated and erased in place exactly where
// written. Runtime-tainted so codegen cannot const-fold it away.
#[test]
fn nested_local_rec_runs_correctly() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Handle, Nat, Str, Bytes};
        rec f(n : Nat) -> Nat =
            (rec go(i : Nat) -> Nat =
                match i
                | 0 => 0
                | k + 1; ih => go(k) + 1
                end;
             go(n));
        /std/print(Nat/to_str(f(Bytes/len(/std/rand/bin(4)))))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"4");
}

// A local `rec` nested inside a top-level `rec` member, calling that
// enclosing member by name: since nothing gets lambda-lifted or spliced as a
// separate item, there is no forward-reference to worry about — `go` just
// resolves `f` through ordinary lexical/context scoping, exactly where it's
// written.
#[test]
fn local_rec_calls_enclosing_rec_member() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Handle, Nat, Str, Bytes};
        rec f(n : Nat) -> Nat =
            (rec go(i : Nat) -> Nat =
                match i
                | 0 => 0
                | k + 1; ih => f(k) + go(k)
                end;
             go(n));
        /std/print(Nat/to_str(f(Bytes/len(/std/rand/bin(3)))))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"0");
}

// A non-capturing, self-referential value `rec` (`loop : Nat = loop`) that
// the program never calls: this is exactly the shape that silently
// miscompiled under lambda-lifting (a self-aliased value slot dropped by the
// optimizer's copy-propagation) — here it stays a term-level `Rec`, erased in
// place, and its mere existence has no effect on the rest of the program.
#[test]
fn self_referential_value_rec_never_forced_compiles_and_runs() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Handle, Nat, Str, Bytes};
        let make(n : Nat) -> Nat =
            rec loop : Nat = loop;
            n;
        /std/print(Nat/to_str(make(Bytes/len(/std/rand/bin(5)))))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"5");
}

// A sibling signature may demand the result shape of a recursive type family
// while the group is still being checked. Protected slots prevent conversion
// from solving the knot, but shape-demanding reduction can still unfold a
// filled slot productively: `val : T(2)` reaches `Nat`. Indexed inductive
// families lower to this same shape, so the prelude depends on the distinction.
#[test]
fn recursive_group_signature_reduces_concrete_type_family() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Handle, Nat, Str, Bytes};
        rec T(n : Nat) -> Type =
            match n
            | 0 => Nat
            | k + 1; ih => T(k)
            end
        and val : T(2) =
            Bytes/len(/std/rand/bin(3));
        /std/print(Nat/to_str(val))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"3");
}
