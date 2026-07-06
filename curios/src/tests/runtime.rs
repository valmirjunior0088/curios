use {
    super::run,
    curios_rt::{ForeignBindings, MockHost},
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
        use /std/{Io, Str};
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
                let w = Io/write(Io/stdout, Str/to_bin("ok"));
                Susp/now(()));
        let r = force(prog);
        Io/write(Io/stdout, Str/to_bin("!"))
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
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(score(pair))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"+42");
}

#[test]
fn local_binders_shadow_module_bindings_without_leaking() {
    let source = r#"
        use /std/{Nat, Io, Str};
        mod Foo
            pub let go : /std/Nat = 7;
            pub let shadowed : /std/Nat =
                let go : /std/Nat = 3;
                go;
            pub let sibling : /std/Nat =
                let probe : /std/Nat = (let go : /std/Nat = 3; go);
                go;
        end
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Nat/add(Nat/mul(Foo/shadowed, 10), Foo/sibling))))
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
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Nat/to_str(result)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"10");
}

#[test]
fn multi_arg_function() {
    let source = r#"
        let add : (std/Int, std/Int) -> std/Int = (x, y) => std/Int/add(x, y);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(add(+3, +4))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"+7");
}

#[test]
fn curried_function() {
    let source = r#"
        let add : (std/Int) -> (std/Int) -> std/Int = (x) => (y) => std/Int/add(x, y);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(add(+3)(+4))))
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
        use /std/{Nat, Io, Str, Monad};
        pub record Box(A : Type) : Type { unbox : A }
        satisfy Monad(Box) {
            pure(A, x) = Box { unbox = x },
            bind(A, B, m, f) = f(m.unbox)
        }
        let a : Box(Nat) = Box { unbox = 3 };
        let b : Box(Nat) = Box { unbox = 4 };
        let result : Box(Nat) = Monad/pure(Nat/add(a!, b!));
        Io/print(Nat/to_str(result.unbox))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

#[test]
fn bang_std_parse_threads_bangs_left_to_right() {
    // The real `std/Parse` monad, sequenced with bare `!` — each site resolves
    // the `Monad(Parse)` witness from the action's type.
    // Two `any_byte!`s read consecutive bytes; using a *non-commutative* `Nat/sub`
    // pins the evaluation order: on "BA" the first byte is 'B' (66) and the second
    // 'A' (65), so the result is 66 - 65 = 1 (the reversed order would saturate to 0).
    let source = r#"
        use /std/{Parse, Nat, Result, Io};

        let parser : Parse/Parse(Nat) =
            Parse/pure(Nat/sub(Parse/any_byte!, Parse/any_byte!));

        match Parse/run(parser, /std/Str/to_bin("BA")) : {}
        | success(n) => Io/print(Nat/to_str(n))
        | failure(msg) => Io/print(msg)
        end
        "#;

    let base = Path::new(env!("CARGO_MANIFEST_DIR"));
    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("failed to parse source");
    let loader = curios_text::RootSource::FileSystem(base.to_path_buf());

    let (system, io) = MockHost::builder().build();
    crate::run_entrypoint(Duration::from_secs(10), &entrypoint, loader, system)
        .expect("expected result");
    assert_eq!(io.output(), b"1");
}

#[test]
fn bang_region_mixes_action_types() {
    // A single region sequences two actions of *different* payload types: a
    // `Parse(Bin)` (`take_while`) and a `Parse(Nat)` (`any_byte`). Each `!`
    // site elaborates its own `/syn/Monad/bind` application with fresh
    // implicits (`?A := Bin` for the first, `?A := Nat` for the second), while
    // the shared continuation typing forces one monad for the region. On "AB":
    // `take_while(is_a)` reads "A" (stops at 'B'), then `any_byte` reads 'B'
    // (66); `Bin/append("A", 66)` is "AB".
    let source = r#"
        use /std/{Parse, Nat, Bin, Bln, Result, Io, Str};

        let is_a : (Nat) -> Bln = (b) => match b : Bln | 'A' => true | _ => false end;

        let parser : Parse/Parse(Bin) =
            Parse/pure(Bin/append(Parse/take_while(is_a)!, Parse/any_byte!));

        match Parse/run(parser, /std/Str/to_bin("AB")) : {}
        | success(s) =>
            match Str/of_bin(s) : {}
            | some(t) => Io/print(t)
            | none() => Io/print("invalid utf-8")
            end
        | failure(msg) => Io/print(msg)
        end
        "#;

    let base = Path::new(env!("CARGO_MANIFEST_DIR"));
    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("failed to parse source");
    let loader = curios_text::RootSource::FileSystem(base.to_path_buf());

    let (system, io) = MockHost::builder().build();
    crate::run_entrypoint(Duration::from_secs(10), &entrypoint, loader, system)
        .expect("expected result");
    assert_eq!(io.output(), b"AB");
}

#[test]
fn folds_constant_arg_through_let_function() {
    // `let f(x) = Nat/add(x, 1); f(3)` must fold end-to-end to a literal `4` in
    // `main`. Without the interim DCE before `inline_calls`, `specialize_calls`
    // leaves a dead closure body in `module.clsrs` whose direct call to the
    // lifted clone of `f` inflates the inliner's call-site count, blocking the
    // splice that ultimately lets constant folding see `3` next to the successor.
    let source = r#"
        use /std/{Nat};
        let f(x : Nat) -> Nat = Nat/add(x, 1);
        f(3)
        "#;

    let entrypoint = source.parse::<curios_text::Entrypoint>().unwrap();

    let mut main_func: Option<curios_cont::Func> = None;
    curios_pipeline::compile_entrypoint(
        Duration::from_secs(10),
        &entrypoint,
        curios_text::RootSource::None,
        |stage| {
            if let curios_pipeline::Stage::ContOptm(module) = stage {
                let entry = module.entry().expect("module has entry").clone();
                let (_, func) = module
                    .funcs()
                    .iter()
                    .find(|(name, _)| name == &entry)
                    .expect("entry function present in module");
                main_func = Some(func.clone());
            }
        },
    )
    .expect("compile succeeded");

    let main = main_func.expect("Stage::ContOptm observed");

    assert!(
        main.region.preallocs.is_empty(),
        "expected main to have no preallocs, got {:?}",
        main.region.preallocs,
    );
    assert!(
        main.region.blocks.is_empty(),
        "expected main to have no nested blocks, got {} block(s)",
        main.region.blocks.len(),
    );

    let folded: Vec<&curios_cont::ValueName> = main
        .region
        .values
        .iter()
        .filter_map(|(name, val)| match val {
            curios_cont::Value::Pure(curios_cont::Data::Nat(4)) => Some(name),
            _ => None,
        })
        .collect();
    assert_eq!(
        folded.len(),
        1,
        "expected exactly one Pure(Data::Nat(4)) in main, got values {:?}",
        main.region.values,
    );
    let folded_name = folded[0].clone();

    match &main.region.tail {
        curios_cont::Tail::Jump(jump) => {
            assert_eq!(
                jump.target, main.resume,
                "expected main to jump to its resume sentinel",
            );
            assert_eq!(
                jump.params,
                vec![folded_name],
                "expected main to return the folded Pure(Data::Nat(4))",
            );
        }
        other => panic!("expected resume jump in main, got {other:?}"),
    }
}

#[test]
fn fmt_print_partial_evaluation_reduces_residual() {
    // End-to-end residue guard for the staging stack on
    // `Fmt/print("%s is %d")(name)(30)` with a *runtime* `%s` argument. The
    // ersd `evaluate` pass folds the closed prefix — the format-string parse
    // (Parse combinators and the segment UTF-8 revalidation included) runs at
    // compile time and `Fmt/print(lit)` reifies as the curried hole-filling
    // closure over a constant `Fmt` spine. What stays runtime is exactly the
    // runtime work: `go_with` over the spine, the `%s` path (`Str/trim` and
    // the stdin UTF-8 validation through `classify`), and the `%d` path
    // (`Nat/to_str`'s digit producer); cont's `evaluate_pure_calls` and
    // size-bounded multi-site inlining then dissolve the wrappers around
    // those. The assert pins a comfortable upper bound on the residual funcs
    // while leaving headroom for legitimate std/Fmt drift.
    let source = r#"
        use /std/{Str, Io, Bin, Fmt};

        match Io/read(Io/stdin, 1024) : {}
        | chunk(bytes) =>
            match Str/of_bin(bytes) : {}
            | some(s) => Fmt/print("%s is %d")(Str/trim(s))(30)
            | none() => Io/print("invalid input")
            end
        | eof() => Io/print("invalid input")
        | error(_) => Io/print("invalid input")
        end
        "#;

    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("failed to parse source")
        .with_type("()".parse().unwrap());

    let mut cont_optm_funcs: Option<usize> = None;

    let (wasm_module, _foreigns) = curios_pipeline::compile_entrypoint(
        Duration::from_secs(15),
        &entrypoint,
        curios_text::RootSource::None,
        |stage| {
            if let curios_pipeline::Stage::ContOptm(module) = stage {
                cont_optm_funcs = Some(module.funcs().len());
            }
        },
    )
    .expect("compile succeeded");

    let funcs = cont_optm_funcs.expect("Stage::ContOptm observed");
    assert!(
        funcs <= 10,
        "expected at most 10 residual funcs after ersd staging and cont \
         partial evaluation, got {funcs}",
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
        use /std/{Str, Io, Bin, Fmt};

        match Io/read(Io/stdin, 1024) : {}
        | chunk(bytes) =>
            match Str/of_bin(bytes) : {}
            | some(s) => Fmt/print("%s is %d")(Str/trim(s))(30)
            | none() => Io/print("invalid input")
            end
        | eof() => Io/print("invalid input")
        | error(_) => Io/print("invalid input")
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
        curios_text::RootSource::None,
        |stage| {
            if let curios_pipeline::Stage::ErsdOptm(module) = stage {
                ersd_optm = Some(format!("{module}"));
            }
        },
    )
    .expect("compile succeeded");

    let ersd = ersd_optm.expect("Stage::ErsdOptm observed");
    assert!(
        ersd.contains("#/std/Fmt/go_with@s0("),
        "expected the spine-specialized fold called, got:\n{ersd}",
    );
    assert!(
        !ersd.contains("parse_fmt") && !ersd.contains("rec #/std/Fmt/go_with ="),
        "expected the parser and the generic fold pruned, got:\n{ersd}",
    );

    let (system, io) = MockHost::builder().stdin_lines(["Bob"]).build();
    crate::run_wasm(&wasm_module, system, ForeignBindings::empty()).expect("execution succeeded");
    assert_eq!(io.output(), b"Bob is 30");
}

#[test]
fn fmt_print_err_formats_to_stderr() {
    // Same staging as `Fmt/print`, routed through `Io/print_err`. MockIo
    // captures stdout and stderr concatenated in write order, so the ordering
    // also shows the stderr write really happened between the stdout ones.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Fmt, Io};
        let a = Io/print("before;");
        let b = Fmt/print_err("%s: %d;")("code")(3);
        Io/print("after")
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
    // single `#/std/Io/print(<final bytes>)` call, the `Parse` combinator web
    // and `Fmt`'s parser are pruned, and only the `Io/print` → `Io/write`
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
    let mut cont_optm_funcs = None;

    let (wasm_module, _foreigns) = curios_pipeline::compile_entrypoint(
        Duration::from_secs(15),
        &entrypoint,
        curios_text::RootSource::None,
        |stage| match stage {
            curios_pipeline::Stage::ErsdOptm(module) => ersd_optm = Some(format!("{module}")),
            curios_pipeline::Stage::ContOptm(module) => cont_optm_funcs = Some(module.funcs().len()),
            _ => {}
        },
    )
    .expect("compile succeeded");

    let ersd = ersd_optm.expect("Stage::ErsdOptm observed");
    // "x = 42, s = hello\n", already formatted, as the residual call's operand.
    assert!(
        ersd.contains("#/std/Io/print(\\78\\20\\3d\\20\\34\\32\\2c\\20\\73\\20\\3d\\20\\68\\65\\6c\\6c\\6f\\0a)"),
        "expected the folded print residual, got:\n{ersd}",
    );
    assert!(
        !ersd.contains("/std/Fmt/") && !ersd.contains("/std/Parse/"),
        "expected the parser web pruned, got:\n{ersd}",
    );

    let funcs = cont_optm_funcs.expect("Stage::ContOptm observed");
    assert!(
        funcs <= 4,
        "expected the constant program to collapse to the write loop, got {funcs} funcs",
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
        r#"std/Io/write(std/Io/stdout, /std/Rand/bin(8))"#,
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
        use /std/{Nat, Str, Option, Io};
        let ok = Option/unwrap_or(Nat/of_str("123"), 0);
        let bad = Option/unwrap_or(Nat/of_str("12a"), 7);
        let empty = Option/unwrap_or(Nat/of_str(""), 9);
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Nat/add(Nat/add(ok, bad), empty))))
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
        use /std/{Nat, Int, Str, Option, Io};
        let neg = Int/abs(Option/unwrap_or(Int/of_str("-5"), +0));
        let pos = Int/abs(Option/unwrap_or(Int/of_str("+7"), +0));
        let bad = Int/abs(Option/unwrap_or(Int/of_str("x"), +3));
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Nat/add(Nat/add(neg, pos), bad))))
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
        use /std/{Nat, Flt, Str, Option, Io};
        let whole = Flt/to_nat(Option/unwrap_or(Flt/of_str("12.0"), +0.0));
        let half = Flt/to_nat(Flt/mul(Option/unwrap_or(Flt/of_str(".5"), +0.0), +2.0));
        let exp = Flt/to_nat(Option/unwrap_or(Flt/of_str("1e3"), +0.0));
        let bad = Flt/to_nat(Option/unwrap_or(Flt/of_str("abc"), +4.0));
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Nat/add(Nat/add(whole, half), Nat/add(exp, bad)))))
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
        use /std/{Option, Result, Char, Nat, Str, Io};
        let opt = Option/unwrap_or(Option/map((x : Nat) => Nat/add(x, 1), Option/some(4)), 0);
        let res0 : Result(Nat, Nat) = Result/success(5);
        let res = Result/unwrap_or(Result/map_success((x : Nat) => Nat/mul(x, 2), res0), 0);
        let up = Char/to_upper('a');
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Nat/add(Nat/add(opt, res), up))))
        "#,
        system,
    )
    .expect("expected result");

    // opt = 5, res = 10, up = 'A' = 65  ->  80
    assert_eq!(io.output(), b"80");
}

#[test]
fn clock_diff_of_two_distinct_now_readings() {
    // Two scripted wall readings 30 s + 400 ns apart. `Time/now` referenced
    // twice must perform two *distinct* host calls (the nullary-effect
    // distinctness the struct-head reduction relies on), so the diff is the
    // gap between them, not zero.
    let (system, io) = MockHost::builder()
        .wall([(1, 100, 500), (1, 130, 900)])
        .build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        let a = /std/Time/now();
        let b = /std/Time/now();
        let d = /std/Time/diff(b, a);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(/std/Nat/to_str(/std/Time/secs(d))))
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
        let e = /std/Time/elapsed();
        std/Io/write(std/Io/stdout, /std/Str/to_bin(/std/Nat/to_str(/std/Time/secs(e))))
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
            use /std/{Cell, Io, Nat, Str};
            let n : Nat = 42;
            let cell = Cell/new(n);
            Io/print(Nat/to_str(Cell/get(cell)))
        "#),
        b"42",
    );
}

#[test]
fn cell_set_overwrites_value() {
    // Write then read: the getter sees the new value, not the init.
    assert_eq!(
        run(r#"
            use /std/{Cell, Io, Nat, Str};
            let z : Nat = 0;
            let cell = Cell/new(z);
            let _ = Cell/set(cell, 99);
            Io/print(Nat/to_str(Cell/get(cell)))
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
            use /std/{Cell, Io, Nat, Str};
            let n : Nat = 7;
            let a = Cell/new(n);
            let b = Cell/new(n);
            let _ = Cell/set(a, 1);
            Io/print(Nat/to_str(Cell/get(b)))
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
            use /std/{Cell, Io, Nat, Str};
            let n : Nat = 5;
            let c = Cell/new(n);
            let x = match Cell/get(c)
                | 0 => 0
                | k + 1; ih =>
                    let _ = Cell/set(c, 0);
                    k + 1
                end;
            Io/print(Nat/to_str(x))
        "#),
        b"5",
    );
}

#[test]
fn accumulation_loops_are_linear_by_construction() {
    // The rope representation's whole promise: a naive 100k-step `Bin/concat`
    // accumulation loop is O(n) with no optimizer recognition anywhere — each
    // step is one node allocation, and the single read at the end forces once.
    // The pre-rope representation copied the accumulator per step (Θ(n²), tens
    // of minutes at this size); a regression fails on the timeout. The final
    // slice + print also pins the force → memo → host-write path end to end.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(60),
        r#"
        use /std/{Io, Bin, Nat, Str};
        rec go(i : Nat, acc : Bin) -> Bin =
            match i
            | 0 => acc
            | k + 1; ih => go(k, Bin/concat(acc, Str/to_bin("0123456789")))
            end;
        let built = go(100000, \\);
        let head = Bin/slice(built, 0, 10);
        let _ = Io/write(Io/stdout, head);
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Bin/len(built))))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"01234567891000000");
}

#[test]
fn peel_loops_are_linear_by_construction() {
    // The window (`sub`) shape's whole promise, the consumption-side mirror of
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
        use /std/{Io, Bin, Nat, Str, Cell};
        rec build(i : Nat, acc : Bin) -> Bin =
            match i
            | 0 => acc
            | k + 1; ih => build(k, Bin/concat(acc, Str/to_bin("0123456789")))
            end;
        let built = build(10000, \\);
        let c = Cell/new(built);
        rec drain(fuel : Nat, acc : Nat) -> Nat =
            match fuel
            | 0 => acc
            | f + 1; ih =>
                match Cell/get(c)
                | \\ => acc
                | \h\..t; ih2 =>
                    let _ = Cell/set(c, t);
                    drain(f, acc + (h - 48))
                end
            end;
        let total = drain(Bin/len(built) + 1, 0);
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(total)))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"450000");
}
