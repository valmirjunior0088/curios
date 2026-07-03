use {
    super::run,
    curios_rt::MockHost,
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
        .parse::<crate::text::Entrypoint>()
        .expect("failed to parse source");
    let loader = crate::text::FileLoader::new(base);

    let (system, io) = MockHost::builder().build();
    crate::run_entrypoint(Duration::from_secs(10), &entrypoint, &loader, system)
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
        .parse::<crate::text::Entrypoint>()
        .expect("failed to parse source");
    let loader = crate::text::FileLoader::new(base);

    let (system, io) = MockHost::builder().build();
    crate::run_entrypoint(Duration::from_secs(10), &entrypoint, &loader, system)
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
    use crate::{cont, text};

    let source = r#"
        use /std/{Nat};
        let f(x : Nat) -> Nat = Nat/add(x, 1);
        f(3)
        "#;

    let entrypoint = source.parse::<text::Entrypoint>().unwrap();

    let mut main_func: Option<cont::Func> = None;
    crate::compile_entrypoint(
        Duration::from_secs(10),
        &entrypoint,
        &text::NullLoader,
        |stage| {
            if let crate::Stage::ContOptm(module) = stage {
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

    let folded: Vec<&cont::ValueName> = main
        .region
        .values
        .iter()
        .filter_map(|(name, val)| match val {
            cont::Value::Pure(cont::Data::Nat(4)) => Some(name),
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
        cont::Tail::Jump(jump) => {
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
fn printf_partial_evaluation_reduces_residual() {
    // End-to-end smoke for §2 (`evaluate_pure_calls`) and §3 (size-bounded
    // multi-site inlining) on `Fmt/printf("%s is %d")(name)(30)`. §2 interprets
    // pure sub-bodies of the parser combinator at compile time; §3 then
    // dissolves the residual primitive wrappers at every call site (including the
    // `Str/of_bin` validation guarding the runtime `%s` argument). Together they
    // collapse the post-§1 residue (≈14 funcs) down to a handful — the assert pins
    // a comfortable upper bound while leaving headroom for legitimate std/Fmt drift.
    // Proof-carrying `Str` routes both runtime paths through recursive, unfoldable
    // validators: the `%d` (`Nat/to_str`) path through its decimal digit producer
    // (digit/single_digit/Str/concat), and the `%s` (`Str/trim`) path through the
    // codepoint-peeling proof-carrying `slice` (drop_n/take_n/drop1/take1/tl_proof).
    // Both carry their UTF-8 proof and can't be folded even for a constant, so a
    // handful of extra residual funcs over the pre-`/syn/Str` baseline are expected.
    // The shared `classify` (the single UTF-8 layout source consumed by both the
    // validator `step` and the runtime decoder) now has two reachable call sites, so
    // size-bounded multi-site inlining keeps it as one residual func rather than
    // folding it into a sole caller — one extra func, the intended cost of the dedup.
    let source = r#"
        use /std/{Str, Io, Bin, Fmt};

        match Io/read(Io/stdin, 1024) : {}
        | chunk(bytes) =>
            match Str/of_bin(bytes) : {}
            | some(s) => Fmt/printf("%s is %d")(Str/trim(s))(30)
            | none() => Io/print("invalid input")
            end
        | eof() => Io/print("invalid input")
        | error(_) => Io/print("invalid input")
        end
        "#;

    let entrypoint = source
        .parse::<crate::text::Entrypoint>()
        .expect("failed to parse source")
        .with_type("()".parse().unwrap());

    let mut cont_optm_funcs: Option<usize> = None;

    let wasm_module = crate::compile_entrypoint(
        Duration::from_secs(15),
        &entrypoint,
        &crate::text::NullLoader,
        |stage| {
            if let crate::Stage::ContOptm(module) = stage {
                cont_optm_funcs = Some(module.funcs().len());
            }
        },
    )
    .expect("compile succeeded");

    let funcs = cont_optm_funcs.expect("Stage::ContOptm observed");
    assert!(
        funcs <= 13,
        "expected at most 13 residual funcs after partial evaluation and \
         size-bounded multi-site inlining, got {funcs}",
    );

    let (system, io) = MockHost::builder().stdin_lines(["Alice"]).build();
    crate::run_wasm(&wasm_module, system).expect("execution succeeded");
    assert_eq!(io.output(), b"Alice is 30");
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
