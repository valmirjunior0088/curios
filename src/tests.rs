use {
    super::ChannelHost,
    std::{path::Path, time::Duration},
};

#[test]
fn end_to_end() {
    let source = r#"
        union Pair
        | left(sys/Int)
        | right(sys/Flt)
        end
        let pair : Pair = Pair/left(+42);
        let score : (_ : Pair) -> sys/Int = (p) =>
            match p : sys/Int
            | left(_) => +42
            | right(_) => +7
            end;
        sys/Io/write(sys/Io/stdout, sys/Int/to_str(score(pair)))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"+42".to_vec()]
    );
}

#[test]
fn flt_to_le_bin_prints_raw_bytes() {
    let source = r#"
        sys/Io/write(sys/Io/stdout, sys/Flt/to_le_bin(+1.5))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![1.5f32.to_le_bytes().to_vec()]
    );
}

#[test]
fn io_write() {
    let (system, receiver) = ChannelHost::out();
    crate::run_text(
        Duration::from_secs(5),
        r#"sys/Io/write(sys/Io/stdout, "hello")"#,
        system,
    )
    .expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"hello".to_vec()]
    );
}

#[test]
fn io_write_stderr() {
    let (system, receiver) = ChannelHost::out();
    crate::run_text(
        Duration::from_secs(5),
        r#"sys/Io/write(sys/Io/stderr, "oops")"#,
        system,
    )
    .expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"oops".to_vec()]
    );
}

#[test]
fn io_read() {
    let (system, receiver) = ChannelHost::in_out(["hello"]);
    crate::run_text(
        Duration::from_secs(5),
        r#"sys/Io/write(sys/Io/stdout, sys/Io/read(sys/Io/stdin, 1024))"#,
        system,
    )
    .expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"hello\n".to_vec()]
    );
}

// Named fields end to end: a dependent record (the vector's length indexes its
// type) constructed with written names, consumed through `.label` and `.index`
// access on the same value — both resolve to the same positional projection.
#[test]
fn named_fields_run_end_to_end() {
    let source = r#"
        use /std/{Vec, Nat, Io};
        let p : { n : Nat, v : Vec(Nat, n) } =
            (n = 2, v = Vec/cons(30, Vec/cons(12, Vec/nil())));
        rec total(@k : Nat, v : Vec(Nat, k), acc : Nat) -> Nat =
            match v : Nat
            | nil() => acc
            | cons(m, x, xs) => total(xs, Nat/add(acc, x))
            end;
        Io/print(Nat/to_str(Nat/add(total(p.v, 0), Nat/mul(p.0, 0))))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"42".to_vec()]
    );
}

// `read(h, n)` is POSIX-shaped: each call returns 1..n available bytes (here
// one injected line per refill, served in `n`-byte slices), and empty means
// EOF — exercised by the third read.
#[test]
fn io_read_short_reads_and_eof() {
    let source = r#"
        use /std/{Io};
        let a = Io/read(Io/stdin, 2);
        let ra = Io/write(Io/stdout, a);
        let b = Io/read(Io/stdin, 2);
        let rb = Io/write(Io/stdout, b);
        let c = Io/read(Io/stdin, 2);
        Io/write(Io/stdout, c)
        "#;

    let (system, receiver) = ChannelHost::in_out(["abc"]);
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"ab".to_vec(), b"c\n".to_vec(), b"".to_vec()]
    );
}

#[test]
fn std_io_read_line_sequences_lines() {
    let source = r#"
        use /std/{Io, Option, Bin};
        let program : Io/Buf({}) =
            with Io/bind
                let first = Io/read_line!;
                let second = Io/read_line!;
                match first : Io/Buf({})
                | some(a) =>
                    match second : Io/Buf({})
                    | some(b) => Io/pure(Io/print(Bin/concat(a, b)))
                    | none() => Io/pure(Io/print("missing"))
                    end
                | none() => Io/pure(Io/print("missing"))
                end;
        Io/run(program, Io/stdin)
        "#;

    let (system, receiver) = ChannelHost::in_out(["alpha", "beta"]);
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"alpha\nbeta\n".to_vec()]
    );
}

#[test]
fn std_io_read_line_signals_eof_with_none() {
    let source = r#"
        use /std/{Io, Option};
        let program : Io/Buf({}) =
            with Io/bind
                let first = Io/read_line!;
                let second = Io/read_line!;
                match second : Io/Buf({})
                | some(_) => Io/pure(Io/print("line"))
                | none() => Io/pure(Io/print("eof"))
                end;
        Io/run(program, Io/stdin)
        "#;

    let (system, receiver) = ChannelHost::in_out(["only"]);
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"eof".to_vec()]
    );
}

// A line longer than `read_line`'s 1024-byte refill chunk forces the buffer
// to absorb one full chunk, miss the newline, and refill before slicing.
#[test]
fn std_io_read_line_spans_refills() {
    let source = r#"
        use /std/{Io, Option, Bin, Nat};
        let program : Io/Buf({}) =
            with Io/bind
                let line = Io/read_line!;
                match line : Io/Buf({})
                | some(bytes) => Io/pure(Io/print(Nat/to_str(Bin/len(bytes))))
                | none() => Io/pure(Io/print("eof"))
                end;
        Io/run(program, Io/stdin)
        "#;

    let long_line = "a".repeat(1500);
    let (system, receiver) = ChannelHost::in_out([long_line.as_str()]);
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"1501".to_vec()]
    );
}

#[test]
fn triangular_sum() {
    let source = r#"
        let result : sys/Nat =
            match 5 : sys/Nat
            | 0 => 0
            | pred + 1, ih => sys/Nat/add(ih, pred)
            end;
        sys/Io/write(sys/Io/stdout, sys/Nat/to_str(result))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"10".to_vec()]
    );
}

#[test]
fn match_omitted_motive_infers() {
    // The same induction as `triangular_sum`, but with the motive omitted. It is
    // non-dependent (every arm has type `sys/Nat`), so the synthesized metavar
    // motive is solved by the arms — no explicit `: sys/Nat` needed.
    let source = r#"
        let result : sys/Nat =
            match 5
            | 0 => 0
            | pred + 1, ih => sys/Nat/add(ih, pred)
            end;
        sys/Io/write(sys/Io/stdout, sys/Nat/to_str(result))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"10".to_vec()]
    );
}

#[test]
fn multi_arg_function() {
    let source = r#"
        let add : (sys/Int, sys/Int) -> sys/Int = (x, y) => sys/Int/add(x, y);
        sys/Io/write(sys/Io/stdout, sys/Int/to_str(add(+3, +4)))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"+7".to_vec()]
    );
}

#[test]
fn curried_function() {
    let source = r#"
        let add : sys/Int -> sys/Int -> sys/Int = (x) => (y) => sys/Int/add(x, y);
        sys/Io/write(sys/Io/stdout, sys/Int/to_str(add(+3)(+4)))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"+7".to_vec()]
    );
}

#[test]
fn with_identity_monad_sequences_bangs() {
    // A minimal Identity monad over `sys/Nat`: `bind(m, f) = f(m)`. The compiler is
    // monad-agnostic — `with bind` applies the binary `bind` to `(action, cont)` per
    // `!` site — so the sugar `add(a!, b!)` threads each banged value through a fresh
    // continuation and evaluates to `add(a, b)`.
    let source = r#"
        let bind : (sys/Nat, (sys/Nat) -> sys/Nat) -> sys/Nat = (m, f) => f(m);
        let a : sys/Nat = 3;
        let b : sys/Nat = 4;
        let result : sys/Nat = with bind sys/Nat/add(a!, b!);
        sys/Io/write(sys/Io/stdout, sys/Nat/to_str(result))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"7".to_vec()]);
}

#[test]
fn with_std_parse_threads_bangs_left_to_right() {
    // The real `std/Parse` monad. `with Parse/bind` partially applies the curried
    // bind, fixing its leading `Type` arguments with `?` holes — and because the bind is
    // re-elaborated per `!` site, each site mints its own holes (solved by inference).
    // `Parse/bind` stays in head position, so no annotations are needed.
    // Two `any_byte!`s read consecutive bytes; using a *non-commutative* `Nat/sub`
    // pins the evaluation order: on "BA" the first byte is 'B' (66) and the second
    // 'A' (65), so the result is 66 - 65 = 1 (the reversed order would saturate to 0).
    let source = r#"
        use /std/{Parse, Nat, Result, Io};

        let parser : Parse/Parse(Nat) =
            with Parse/bind
            Parse/pure(Nat/sub(Parse/any_byte!, Parse/any_byte!));

        match Parse/run(parser, "BA") : {}
        | success(n) => Io/print(Nat/to_str(n))
        | failure(msg) => Io/print(msg)
        end
        "#;

    let base = Path::new(env!("CARGO_MANIFEST_DIR"));
    let entrypoint = source
        .parse::<crate::text::Entrypoint>()
        .expect("failed to parse source");
    let loader = crate::text::FileLoader::new(base);

    let (system, receiver) = ChannelHost::out();
    crate::run_entrypoint(Duration::from_secs(10), &entrypoint, &loader, system)
        .expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"1".to_vec()]);
}

#[test]
fn with_region_mixes_action_types() {
    // A single region sequences two actions of *different* payload types: a
    // `Parse(Bin)` (`take_while`) and a `Parse(Nat)` (`any_byte`). This works only
    // because `with Parse/bind` is re-elaborated per `!` site, so each site gets
    // its own holes (`?A := Bin` for the first, `?A := Nat` for the second). A single
    // shared bind value would force one `A` and reject this. On "AB": `take_while(is_a)`
    // reads "A" (stops at 'B'), then `any_byte` reads 'B' (66); `Bin/append("A", 66)`
    // is "AB".
    let source = r#"
        use /std/{Parse, Nat, Bin, Bln, Result, Io};

        let is_a : (Nat) -> Bln = (b) => match b : Bln | 'A' => true | _ => false end;

        let parser : Parse/Parse(Bin) =
            with Parse/bind
            Parse/pure(Bin/append(Parse/take_while(is_a)!, Parse/any_byte!));

        match Parse/run(parser, "AB") : {}
        | success(s) => Io/print(s)
        | failure(msg) => Io/print(msg)
        end
        "#;

    let base = Path::new(env!("CARGO_MANIFEST_DIR"));
    let entrypoint = source
        .parse::<crate::text::Entrypoint>()
        .expect("failed to parse source");
    let loader = crate::text::FileLoader::new(base);

    let (system, receiver) = ChannelHost::out();
    crate::run_entrypoint(Duration::from_secs(10), &entrypoint, &loader, system)
        .expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"AB".to_vec()]
    );
}

#[test]
fn vec_cons_with_nat_succ() {
    let source = r#"
        rec Vec(T : Type, n : sys/Nat) -> Type =
            match n : Type
            | 0 => {}
            | pred + 1, ih => { T, ih }
            end;

        let cons(T : Type, n : sys/Nat, x : T, xs : Vec(T, n)) -> Vec(T, sys/Nat/succ(n)) =
            (x, xs);

        let head(T : Type, n : sys/Nat, xs : Vec(T, sys/Nat/succ(n))) -> T =
            xs.0;

        let v : Vec(sys/Nat, 1) = cons(sys/Nat, 0, 42, ());
        sys/Io/write(sys/Io/stdout, sys/Nat/to_str(head(sys/Nat, 0, v)))
    "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"42".to_vec()]
    );
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
        use /sys/{Nat};
        let f(x : Nat) -> Nat = Nat/add(x, 1);
        f(3)
        "#;

    let entrypoint = source.parse::<text::Entrypoint>().unwrap();

    let mut main_func: Option<cont::Func> = None;
    crate::compile_entrypoint(
        Duration::from_secs(5),
        &entrypoint,
        &text::NullLoader,
        |stage| {
            if let crate::Stage::Optm(module) = stage {
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

    let main = main_func.expect("Stage::Optm observed");

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
    // dissolves the residual primitive wrappers at every call site. Together
    // they collapse the post-§1 residue (≈14 funcs) down to a handful — the
    // assert pins a comfortable upper bound while leaving headroom for
    // legitimate std/Fmt drift.
    let source = r#"
        use /std/{Str, Io, Bin, Fmt};

        let name = Str/trim(Io/read(Io/stdin, 1024));
        Fmt/printf("%s is %d")(name)(30)
        "#;

    let entrypoint = source
        .parse::<crate::text::Entrypoint>()
        .expect("failed to parse source")
        .with_type("()".parse().unwrap());

    let mut optm_funcs: Option<usize> = None;

    let wasm_module = crate::compile_entrypoint(
        Duration::from_secs(15),
        &entrypoint,
        &crate::text::NullLoader,
        |stage| {
            if let crate::Stage::Optm(module) = stage {
                optm_funcs = Some(module.funcs().len());
            }
        },
    )
    .expect("compile succeeded");

    let funcs = optm_funcs.expect("Stage::Optm observed");
    assert!(
        funcs < 5,
        "expected fewer than 5 residual funcs after partial evaluation and \
         size-bounded multi-site inlining, got {funcs}",
    );

    let (system, receiver) = ChannelHost::in_out(["Alice"]);
    crate::run_wasm(&wasm_module, system).expect("execution succeeded");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"Alice is 30".to_vec()]
    );
}

#[test]
fn indexed_vec_append_executes() {
    // Rung A of the indexed-union ladder, *executed*: `append`'s motive binds
    // the length index (`(v : Vec(T, k)) => Vec(T, Nat/add(k, m))`), the
    // `cons` arm meets it through the definitional successor-peeling of
    // `Nat/add`, and the implicit index arguments of the recursive call are
    // solved to the arm's *first* binder. Running (not just compiling) guards
    // the zonk realignment of multi-binder arm scopes: with the in-group
    // order flipped, the solved indices silently referenced the wrong binder
    // and the program trapped at runtime.
    let source = r#"
        use /sys/{Nat, Bin, Io};
        union Vec(T : Type) : (n : Nat)
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
        end
        rec append(@T : Type, @n : Nat, @m : Nat, v : Vec(T, n), w : Vec(T, m)) -> Vec(T, Nat/add(n, m)) =
            match v : (v : Vec(T, k)) => Vec(T, Nat/add(k, m))
            | nil() => w
            | cons(j, x, xs) => Vec/cons(x, append(xs, w))
            end;
        rec total(@n : Nat, v : Vec(Nat, n), acc : Nat) -> Nat =
            match v : Nat
            | nil() => acc
            | cons(m, x, xs) => total(xs, Nat/add(acc, x))
            end;
        let a : Vec(Nat, 2) = Vec/cons(1, Vec/cons(2, Vec/nil()));
        let b : Vec(Nat, 1) = Vec/cons(4, Vec/nil());
        let c : Vec(Nat, 3) = append(a, b);
        Io/write(Io/stdout, Nat/to_str(total(c, 0)))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"7".to_vec()]);
}
