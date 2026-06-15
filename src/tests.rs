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
        sys/Io/write(sys/Io/stdout, /sys/Str/to_bin(sys/Int/to_str(score(pair))))
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
        r#"sys/Io/write(sys/Io/stdout, /sys/Str/to_bin("hello"))"#,
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
        r#"sys/Io/write(sys/Io/stderr, /sys/Str/to_bin("oops"))"#,
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
        r#"sys/Io/write(sys/Io/stdout, sys/Io/read(sys/Io/stdin, 1024).bytes)"#,
        system,
    )
    .expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"hello\n".to_vec()]
    );
}

#[test]
fn bin_empty_is_the_empty_sequence() {
    // `Bin/empty` is the literal-free empty byte sequence (so foundational
    // modules can stop spelling it `""`); concatenating it is the identity.
    let (system, receiver) = ChannelHost::out();
    crate::run_text(
        Duration::from_secs(5),
        r#"sys/Io/write(sys/Io/stdout, sys/Bin/concat(sys/Bin/empty, /sys/Str/to_bin("ok")))"#,
        system,
    )
    .expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"ok".to_vec()]);
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
// one injected line per refill, served in `n`-byte slices) with status 0, and
// EOF is status 1 with empty bytes — exercised by the third read.
#[test]
fn io_read_short_reads_and_eof() {
    let source = r#"
        use /std/{Io, Nat};
        let a = Io/read(Io/stdin, 2);
        let ra = Io/write(Io/stdout, a.bytes);
        let b = Io/read(Io/stdin, 2);
        let rb = Io/write(Io/stdout, b.bytes);
        let c = Io/read(Io/stdin, 2);
        let rc = Io/write(Io/stdout, c.bytes);
        Io/write(Io/stdout, /sys/Str/to_bin(Nat/to_str(c.status)))
        "#;

    let (system, receiver) = ChannelHost::in_out(["abc"]);
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"ab".to_vec(), b"c\n".to_vec(), b"".to_vec(), b"1".to_vec()]
    );
}

#[test]
fn file_read_all_reads_a_seeded_file() {
    let source = r#"
        use /std/{File, Io};
        match File/read_all(/sys/Str/to_bin("data.txt"))
        | success(contents) => Io/print(/sys/Str/of_bin(contents))
        | failure(_) => Io/print("error")
        end
        "#;

    let (system, receiver, _fs) =
        ChannelHost::with_fs::<&[u8], _, _, _, _>([], [("data.txt", "file contents")]);
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"file contents".to_vec()]
    );
}

#[test]
fn file_read_all_of_a_missing_path_is_not_found() {
    let source = r#"
        use /std/{File, Io};
        match File/read_all(/sys/Str/to_bin("nope.txt"))
        | success(_) => Io/print("contents")
        | failure(e) =>
            match e : {}
            | not_found() => Io/print("not found")
            | permission_denied() => Io/print("denied")
            | exists() => Io/print("exists")
            | other(_) => Io/print("other")
            end
        end
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"not found".to_vec()]
    );
}

#[test]
fn file_with_write_mode_persists_through_close() {
    let source = r#"
        use /std/{File, Io};
        match File/with(/sys/Str/to_bin("out.txt"), File/Mode/write(), (f) => File/write(f, /sys/Str/to_bin("written")))
        | success(_) => Io/print("ok")
        | failure(_) => Io/print("error")
        end
        "#;

    let (system, receiver, fs) = ChannelHost::with_fs::<&[u8], _, &[u8], &[u8], _>([], []);
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"ok".to_vec()]
    );
    assert_eq!(
        fs.lock().unwrap().get(b"out.txt".as_slice()),
        Some(&b"written".to_vec())
    );
}

// Matching on an effectful scrutinee must evaluate it exactly once — the
// erased union match binds the scrutinee in a `let` and projects from it.
// Append mode makes a second evaluation visible: it would append twice.
#[test]
fn effectful_match_scrutinee_runs_once() {
    let source = r#"
        use /std/{File, Io};
        match File/with(/sys/Str/to_bin("log.txt"), File/Mode/append(), (f) => File/write(f, /sys/Str/to_bin("x")))
        | success(_) => Io/print("ok")
        | failure(_) => Io/print("error")
        end
        "#;

    let (system, receiver, fs) = ChannelHost::with_fs::<&[u8], _, &[u8], &[u8], _>([], []);
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"ok".to_vec()]
    );
    assert_eq!(
        fs.lock().unwrap().get(b"log.txt".as_slice()),
        Some(&b"x".to_vec())
    );
}

// The public file ops run on the opaque handle inside the bracket: `File/read`
// pulls bytes from the `File` that `using` hands the body, and `using` closes
// it afterwards.
#[test]
fn file_read_pulls_bytes_inside_the_bracket() {
    let source = r#"
        use /std/{File, Io, Str};
        match File/with(/sys/Str/to_bin("lines.txt"), File/Mode/read(), (f) => File/read(f, 1024).bytes)
        | success(bytes) => Io/print(/sys/Str/of_bin(bytes))
        | failure(_) => Io/print("error")
        end
        "#;

    let (system, receiver, _fs) =
        ChannelHost::with_fs::<&[u8], _, _, _, _>([], [("lines.txt", "first\nsecond\n")]);
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"first\nsecond\n".to_vec()]
    );
}

#[test]
fn std_io_read_line_sequences_lines() {
    let source = r#"
        use /std/{Io, Option, Bin};
        let program : Io/Buf({}) =
            let ! = Io/bind;
            let first = Io/read_line!;
            let second = Io/read_line!;
            match first : Io/Buf({})
            | some(a) =>
                match second : Io/Buf({})
                | some(b) => Io/pure(Io/print(/sys/Str/of_bin(Bin/concat(a, b))))
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
            let ! = Io/bind;
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
            let ! = Io/bind;
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
        sys/Io/write(sys/Io/stdout, /sys/Str/to_bin(sys/Nat/to_str(result)))
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
        sys/Io/write(sys/Io/stdout, /sys/Str/to_bin(sys/Nat/to_str(result)))
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
        sys/Io/write(sys/Io/stdout, /sys/Str/to_bin(sys/Int/to_str(add(+3, +4))))
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
        sys/Io/write(sys/Io/stdout, /sys/Str/to_bin(sys/Int/to_str(add(+3)(+4))))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"+7".to_vec()]
    );
}

#[test]
fn let_bang_identity_monad_sequences_bangs() {
    // A minimal Identity monad over `sys/Nat`: `bind(m, f) = f(m)`. The compiler is
    // monad-agnostic — `let ! = bind;` applies the binary `bind` to `(action, cont)`
    // per `!` site — so the sugar `add(a!, b!)` threads each banged value through a
    // fresh continuation and evaluates to `add(a, b)`.
    let source = r#"
        let bind : (sys/Nat, (sys/Nat) -> sys/Nat) -> sys/Nat = (m, f) => f(m);
        let a : sys/Nat = 3;
        let b : sys/Nat = 4;
        let result : sys/Nat = let ! = bind; sys/Nat/add(a!, b!);
        sys/Io/write(sys/Io/stdout, /sys/Str/to_bin(sys/Nat/to_str(result)))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"7".to_vec()]);
}

#[test]
fn let_bang_std_parse_threads_bangs_left_to_right() {
    // The real `std/Parse` monad. `let ! = Parse/bind;` partially applies the curried
    // bind, fixing its leading `Type` arguments with `?` holes — and because the bind is
    // re-elaborated per `!` site, each site mints its own holes (solved by inference).
    // `Parse/bind` stays in head position, so no annotations are needed.
    // Two `any_byte!`s read consecutive bytes; using a *non-commutative* `Nat/sub`
    // pins the evaluation order: on "BA" the first byte is 'B' (66) and the second
    // 'A' (65), so the result is 66 - 65 = 1 (the reversed order would saturate to 0).
    let source = r#"
        use /std/{Parse, Nat, Result, Io};

        let parser : Parse/Parse(Nat) =
            let ! = Parse/bind;
            Parse/pure(Nat/sub(Parse/any_byte!, Parse/any_byte!));

        match Parse/run(parser, /sys/Str/to_bin("BA")) : {}
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
fn let_bang_region_mixes_action_types() {
    // A single region sequences two actions of *different* payload types: a
    // `Parse(Bin)` (`take_while`) and a `Parse(Nat)` (`any_byte`). This works only
    // because `let ! = Parse/bind;` is re-elaborated per `!` site, so each site gets
    // its own holes (`?A := Bin` for the first, `?A := Nat` for the second). A single
    // shared bind value would force one `A` and reject this. On "AB": `take_while(is_a)`
    // reads "A" (stops at 'B'), then `any_byte` reads 'B' (66); `Bin/append("A", 66)`
    // is "AB".
    let source = r#"
        use /std/{Parse, Nat, Bin, Bln, Result, Io};

        let is_a : (Nat) -> Bln = (b) => match b : Bln | 'A' => true | _ => false end;

        let parser : Parse/Parse(Bin) =
            let ! = Parse/bind;
            Parse/pure(Bin/append(Parse/take_while(is_a)!, Parse/any_byte!));

        match Parse/run(parser, /sys/Str/to_bin("AB")) : {}
        | success(s) => Io/print(/sys/Str/of_bin(s))
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
        sys/Io/write(sys/Io/stdout, /sys/Str/to_bin(sys/Nat/to_str(head(sys/Nat, 0, v))))
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
    // multi-site inlining) on `Fmt/printf("%s is %d")(/sys/Str/of_bin(name))(30)`. §2 interprets
    // pure sub-bodies of the parser combinator at compile time; §3 then
    // dissolves the residual primitive wrappers at every call site. Together
    // they collapse the post-§1 residue (≈14 funcs) down to a handful — the
    // assert pins a comfortable upper bound while leaving headroom for
    // legitimate std/Fmt drift.
    let source = r#"
        use /std/{Str, Io, Bin, Fmt};

        let name = Str/trim(Io/read(Io/stdin, 1024).bytes);
        Fmt/printf("%s is %d")(/sys/Str/of_bin(name))(30)
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
        Io/write(Io/stdout, /sys/Str/to_bin(Nat/to_str(total(c, 0))))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"7".to_vec()]);
}

#[test]
fn implicit_union_type_param_executes() {
    // A `@`-marked union parameter is implicit at the type constructor too:
    // `Eq2(2, 2)` infers `A` from the indices, `Eq2(@Nat, 3, 3)` pins it, and
    // the eliminator's motive type-pattern still spells every slot. Running
    // (not just checking) also guards metavariable spines through the
    // Π-domain close/reopen round trip: a solved implicit type-arg's solution
    // names a sibling binder, and without the delayed substitution the two
    // spellings of the same domain compare as distinct.
    let source = r#"
        use /sys/{Nat, Bin, Io};
        union Eq2(@A : Type) : (x : A, y : A)
        | refl(@z : A) : (z, z)
        end
        let sym2(@A : Type, @x : A, @y : A, p : Eq2(x, y)) -> Eq2(y, x) =
            match p : (q : Eq2(A, s, t)) => Eq2(t, s)
            | refl(z) => Eq2/refl()
            end;
        let pinned : Eq2(@Nat, 3, 3) = Eq2/refl();
        let proof : Eq2(2, 2) = Eq2/refl();
        let inferred : Eq2(2, 2) = sym2(proof);
        match inferred : Nat
        | refl(z) => Io/write(Io/stdout, /sys/Str/to_bin(Nat/to_str(z)))
        end
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"2".to_vec()]);
}

#[test]
fn implicit_union_type_param_rejects_explicit_spelling() {
    // With `@A` implicit, the old explicit spelling queues `Nat` into the
    // explicit slots — one argument too many, an error rather than a silent
    // reinterpretation. (`Eq2(@Nat, 2, 2)` is the pinned spelling.)
    let source = r#"
        use /sys/{Nat, Io};
        union Eq2(@A : Type) : (x : A, y : A)
        | refl(@z : A) : (z, z)
        end
        let bad : Eq2(Nat, 2, 2) = Eq2/refl();
        Io/write(Io/stdout, /sys/Str/to_bin("no"))
        "#;

    let (system, _receiver) = ChannelHost::out();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

#[test]
fn parked_constraints_let_nested_constructor_metas_resolve() {
    // `sym2(Eq2/refl())` — the argument's fresh metas meet the domain's fresh
    // metas as flex–flex pairs embedded under the union type. Before the
    // constraint store (§8) the argument's `expect` failed at quiescence,
    // seconds before the result-type unification would have pinned
    // everything. Now the pairs park, the output `expect` solves the domain
    // metas against the annotation, and the wake retries the parked pairs.
    let source = r#"
        use /sys/{Nat, Io};
        union Eq2(@A : Type) : (x : A, y : A)
        | refl(@z : A) : (z, z)
        end
        let sym2(@A : Type, @x : A, @y : A, p : Eq2(x, y)) -> Eq2(y, x) =
            match p : (q : Eq2(A, s, t)) => Eq2(t, s)
            | refl(z) => Eq2/refl()
            end;
        let direct : Eq2(2, 2) = sym2(Eq2/refl());
        let chained : Eq2(3, 3) = sym2(sym2(Eq2/refl()));
        match chained : Nat
        | refl(z) => Io/write(Io/stdout, /sys/Str/to_bin(Nat/to_str(z)))
        end
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"3".to_vec()]);
}

#[test]
fn parked_constraints_still_reject_the_unsolvable() {
    // An undecidable-at-first constraint that never resolves must still fail —
    // at the item drain, attributed to its origin. `refl` forces both indices
    // equal; `2` and `3` are not.
    let source = r#"
        use /sys/{Nat, Io};
        union Eq2(@A : Type) : (x : A, y : A)
        | refl(@z : A) : (z, z)
        end
        let bad : Eq2(2, 3) = Eq2/refl();
        Io/write(Io/stdout, /sys/Str/to_bin("no"))
        "#;

    let (system, _receiver) = ChannelHost::out();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

#[test]
fn omitted_motive_infers_over_a_compound_scrutinee() {
    // The motive hole's scope is opened with the scrutinee — a non-pattern
    // spine entry when the scrutinee is compound. Occurrence abstraction in
    // `solve` rewrites the scrutinee's occurrences in the expected type to
    // the motive binder, so the dependent motive infers where it previously
    // had to be spelled.
    let source = r#"
        use /std/{Nat, Vec, Io};
        rec build(n : Nat) -> Vec(Nat, n) =
            match n : (m) => Vec(Nat, m)
            | 0 => Vec/nil()
            | pred + 1, ih => Vec/cons(0, ih)
            end;
        let d(k : Nat) -> Vec(Nat, Nat/add(k, k)) =
            match Nat/add(k, k)
            | 0 => Vec/nil()
            | pred + 1, ih => build(Nat/succ(pred))
            end;
        Io/print(Nat/to_str(Vec/len(d(2))))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"4".to_vec()]);
}

#[test]
fn bare_tuple_continuation_tail_infers() {
    // The recorded dead-end from the result-directed elaboration work: a bare
    // tuple in a monadic continuation's tail, its expected type a metavariable
    // pinned only by the *outer* apply's result unification. The in-apply
    // postponement defers the tuple, the constraint store parks the flex–flex
    // codomain pair across the inner apply, and the outer pin wakes both.
    let source = r#"
        use /std/{Parse, Nat, Bin, Io};
        let pairer : Parse({ Nat, Nat }) =
            Parse/bind(Parse/any_byte, (a) => Parse/pure((a, a)));
        rec with_sugar : Parse({ Nat, Nat }) =
            let ! = Parse/bind;
            let a = Parse/any_byte!;
            Parse/pure((a, 0));
        match Parse/run(pairer, /sys/Str/to_bin("hi"))
        | success(pair) => Io/print(Nat/to_str(pair.0))
        | failure(_) => Io/print("error")
        end
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"104".to_vec()]
    );
}

#[test]
fn checking_problem_parks_until_an_outer_pin_lands() {
    // The constraint store's own window: the inner apply's output expect
    // parks (provisional success), so the postponed tuple re-check meets a
    // still-unsolved expected type — it now parks as a *checking problem*
    // behind a placeholder metavariable, and the outer annotation's pin wakes
    // it. Before ParkedWork::Checking this was a NotATupleType error.
    let source = r#"
        use /std/{Nat, Lst, Io};
        let mk(@A : Type, a : A) -> Lst(A) = Lst/cons(a, Lst/nil());
        let use_(@B : Type, l : Lst(B)) -> Lst(B) = l;
        let v : Lst({ Nat, Nat }) = use_(mk((1, 2)));
        match v : Nat
        | nil() => 0
        | cons(p, rest) => Io/write(Io/stdout, /sys/Str/to_bin(Nat/to_str(p.1)))
        end
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"2".to_vec()]);
}

#[test]
fn checking_problem_without_a_pin_still_rejects() {
    // A checking problem whose expected type is never pinned drains as a
    // cannot-infer at the tuple's own span — parked, not silently accepted.
    let source = r#"
        use /std/{Nat, Io};
        let swallow(@A : Type, a : A) -> Nat = 0;
        let n : Nat = swallow((1, 2));
        Io/write(Io/stdout, /sys/Str/to_bin(Nat/to_str(n)))
        "#;

    let (system, _receiver) = ChannelHost::out();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

// === Structs (SYNTAX.md) ================================================

// A transparent record: build with a pinned head, project by label and by
// index — both resolve to the same positional projection.
#[test]
fn struct_transparent_pair_projects() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair(Nat, Nat) { fst = 2, snd = 5 };
        Io/print(Nat/to_str(Nat/add(p.fst, p.1)))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"7".to_vec()]);
}

// The bare-name head infers the parameters from the fields (and the expected
// type at the binding).
#[test]
fn struct_parameter_inference_at_construction() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 4, snd = 3 };
        Io/print(Nat/to_str(Nat/mul(p.fst, p.snd)))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"12".to_vec()]);
}

// A zero-cost newtype: a single positional field, projected with `.0`. It
// erases to its bare field, so the projection elides at runtime.
#[test]
fn struct_newtype_projects() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Meters pub { Nat }
        let m : Meters = Meters { 5 };
        Io/print(Nat/to_str(m.0))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"5".to_vec()]);
}

// A dependent field: a later field's type mentions an earlier field (the
// vector's length indexes its type).
#[test]
fn struct_dependent_fields_run_end_to_end() {
    let source = r#"
        use /std/{Vec, Nat, Io};
        pub struct Sized pub { n : Nat, v : Vec(Nat, n) }
        let s : Sized = Sized { n = 2, v = Vec/cons(30, Vec/cons(12, Vec/nil())) };
        rec total(@k : Nat, v : Vec(Nat, k), acc : Nat) -> Nat =
            match v : Nat
            | nil() => acc
            | cons(m, x, xs) => total(xs, Nat/add(acc, x))
            end;
        Io/print(Nat/to_str(total(s.v, 0)))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"42".to_vec()]);
}

// The motivating case: an abstract type — public type, hidden representation —
// usable only through exported smart constructors/accessors in its module.
#[test]
fn struct_abstract_smart_constructor_round_trips() {
    let source = r#"
        use /std/{Nat, Io};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius { Nat }
            pub let of_nat(n : Nat) -> Celsius = Celsius { n };
            pub let to_nat(c : Celsius) -> Nat = c.0;
        end
        Io/print(Nat/to_str(Celsius/to_nat(Celsius/of_nat(42))))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"42".to_vec()]);
}

// Constructing a private-representation struct from outside its declaring
// module is rejected (`PrivateRepresentation`).
#[test]
fn struct_private_construction_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius { Nat }
        end
        let c : Celsius/Celsius = Celsius/Celsius { 42 };
        Io/print("no")
        "#;

    let (system, _receiver) = ChannelHost::out();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(error.contains("representation"), "unexpected error: {error}");
}

// Projecting a private-representation struct's field from outside its module is
// rejected (`PrivateField`), even when the value was obtained legitimately.
#[test]
fn struct_private_projection_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius { Nat }
            pub let of_nat(n : Nat) -> Celsius = Celsius { n };
        end
        let c : Celsius/Celsius = Celsius/of_nat(42);
        Io/print(Nat/to_str(c.0))
        "#;

    let (system, _receiver) = ChannelHost::out();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(
        error.contains("field") && error.contains("private"),
        "unexpected error: {error}"
    );
}

// A struct type is nominal: it never converts with a structural tuple type of
// the same fields.
#[test]
fn struct_is_not_a_tuple() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : { fst : Nat, snd : Nat } = Pair { fst = 1, snd = 2 };
        Io/print("no")
        "#;

    let (system, _receiver) = ChannelHost::out();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

// A struct literal must supply exactly the declared fields, in order.
#[test]
fn struct_wrong_field_count_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1 };
        Io/print("no")
        "#;

    let (system, _receiver) = ChannelHost::out();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

// Written field labels are validated positionally — no reordering.
#[test]
fn struct_field_label_out_of_order_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { snd = 1, fst = 2 };
        Io/print("no")
        "#;

    let (system, _receiver) = ChannelHost::out();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

// A struct literal whose head names a non-struct binding is rejected as
// `NotAStructType` (its type is reported), not misreported as unbound.
#[test]
fn struct_literal_non_struct_head_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        let Foo : Nat = 3;
        let bad : Nat = Foo { x = 1 };
        Io/print("no")
        "#;

    let (system, _receiver) = ChannelHost::out();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(error.contains("struct type"), "unexpected error: {error}");
}

// === Str (std/Str) ======================================================

// `"..."` is a `Str` primitive value (UTF-8 by construction); `Io/print` writes
// a `Str` straight to stdout.
#[test]
fn str_literal_prints_its_bytes() {
    let source = r#"
        use /std/{Str, Io};
        let s : Str = "hello";
        Io/print(s)
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"hello".to_vec()]);
}

// `Str/of_bin` is the checked constructor: it runs `is_utf8` and yields `some`
// for well-formed UTF-8. `é` is the bytes C3 A9, a valid 2-byte sequence.
#[test]
fn str_of_bin_accepts_multibyte_utf8() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bin(\c3\a9) : {}
        | some(s) => Io/print(s)
        | none() => Io/print("bad")
        end
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![vec![0xc3, 0xa9]]);
}

// An invalid lead byte fails `is_utf8`, so `Str/of_bin` returns `none`.
#[test]
fn str_of_bin_rejects_invalid_utf8() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bin(\ff) : {}
        | some(s) => Io/print(s)
        | none() => Io/print("rejected")
        end
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"rejected".to_vec()]
    );
}

// A truncated multi-byte sequence (a 2-byte lead with no continuation) fails the
// continuation-byte check, so `of_bin` returns `none`.
#[test]
fn str_of_bin_rejects_truncated_multibyte() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bin(\c3) : {}
        | some(s) => Io/print(s)
        | none() => Io/print("rejected")
        end
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"rejected".to_vec()]
    );
}

// A *non-productive* inner `rec` forced in a type position must degrade to the
// reduce deadline (an error), never hang or panic — the regression guard for
// inner-`rec` reduction at the type level (a `Subterm::Rec` demanded by an
// eliminator is now forced, not left stuck).
#[test]
fn nonproductive_inner_rec_in_type_position_is_preempted() {
    let source = r#"
        use /sys/{Bln};
        let spin : Bln =
            rec go : Bln = go;
            go;
        let bad : Type =
            match spin : Type
            | true => {}
            | false => {}
            end;
        let x : bad = ();
        0
        "#;

    let (system, _receiver) = ChannelHost::out();
    assert!(crate::run_text(Duration::from_secs(1), source, system).is_err());
}

