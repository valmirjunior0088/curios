use {
    super::MockHost,
    std::{path::Path, time::Duration},
};

#[test]
fn nullary_closure_survives_erasure_and_codegen() {
    // A nullary closure stored in a union field and called indirectly via a
    // `call_ref` — the erasure+codegen path that needed `clsr_arities`. Zero-arity
    // closures survive it, which is what lets the suspension/continuation thunks
    // drop their dummy unit argument (`() -> T` rather than `({}) -> T`). Output
    // proves the suspended effect fired on `force`.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Io, Str};
        union Susp(A : Type)
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
        union Pair
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
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+42"
    );
}

#[test]
fn flt_to_le_bin_prints_raw_bytes() {
    let source = r#"
        std/Io/write(std/Io/stdout, std/Flt/to_le_bin(+1.5))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        1.5f32.to_le_bytes()
    );
}

#[test]
fn io_write() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"std/Io/write(std/Io/stdout, /std/Str/to_bin("hello"))"#,
        system,
    )
    .expect("expected result");
    assert_eq!(
        io.output(),
        b"hello"
    );
}

#[test]
fn io_write_stderr() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"std/Io/write(std/Io/stderr, /std/Str/to_bin("oops"))"#,
        system,
    )
    .expect("expected result");
    assert_eq!(
        io.output(),
        b"oops"
    );
}

#[test]
fn io_read() {
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        match std/Io/read(std/Io/stdin, 1024) : {}
        | chunk(b) => let w = std/Io/write(std/Io/stdout, b); ()
        | eof() => ()
        | error(_) => ()
        end
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(
        io.output(),
        b"hello\n"
    );
}

#[test]
fn empty_bin_literal_is_the_empty_sequence() {
    // The empty `Bin` literal concatenated with a value is the identity.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"std/Io/write(std/Io/stdout, std/Bin/concat(\\, /std/Str/to_bin("ok")))"#,
        system,
    )
    .expect("expected result");
    assert_eq!(
        io.output(),
        b"ok"
    );
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
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"37");
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

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"42"
    );
}

// `Io/read(h, n)` is the typed blocking read: each call yields a `chunk` of
// 1..n available bytes (here one injected line per refill, served in `n`-byte
// slices), and the third read past the data yields `eof`.
#[test]
fn io_read_short_reads_and_eof() {
    let source = r#"
        use /std/{Io};
        let show(r : Io/Read) -> {} =
            match r : {}
            | chunk(b) => let _ = Io/write(Io/stdout, b); ()
            | eof() => Io/print("1")
            | error(_) => Io/print("e")
            end;
        let _ = show(Io/read(Io/stdin, 2));
        let _ = show(Io/read(Io/stdin, 2));
        show(Io/read(Io/stdin, 2))
        "#;

    let (system, io) = MockHost::builder().stdin_lines(["abc"]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"abc\n1");
}

#[test]
fn file_read_all_reads_a_seeded_file() {
    let source = r#"
        use /std/{File, Io, Task};
        match Task/block_on(File/read_all("data.txt"))
        | success(contents) => Io/write(Io/stdout, contents)
        | failure(_) => Io/write(Io/stdout, /std/Str/to_bin("error"))
        end
        "#;

    let (system, io) =
        MockHost::builder().files([("data.txt", "file contents")]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"file contents"
    );
}

#[test]
fn file_read_all_of_a_missing_path_is_not_found() {
    let source = r#"
        use /std/{File, Io, Task};
        match Task/block_on(File/read_all("nope.txt"))
        | success(_) => Io/print("contents")
        | failure(e) =>
            match e : {}
            | not_found() => Io/print("not found")
            | permission_denied() => Io/print("denied")
            | exists() => Io/print("exists")
            | refused() => Io/print("refused")
            | tls() => Io/print("tls")
            | other(_) => Io/print("other")
            end
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"not found"
    );
}

#[test]
fn file_with_write_mode_persists_through_close() {
    let source = r#"
        use /std/{File, Io, Task};
        match Task/block_on(File/with("out.txt", Io/Mode/write(), (f) => File/write(f, /std/Str/to_bin("written"))))
        | success(_) => Io/print("ok")
        | failure(_) => Io/print("error")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"ok"
    );
    assert_eq!(io.file(b"out.txt"), Some(b"written".to_vec()));
}

// Matching on an effectful scrutinee must evaluate it exactly once — the
// erased union match binds the scrutinee in a `let` and projects from it.
// Append mode makes a second evaluation visible: it would append twice.
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
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"ok"
    );
    assert_eq!(io.file(b"log.txt"), Some(b"x".to_vec()));
}

// The public file ops run on the opaque handle inside the bracket: `File/read`
// pulls bytes from the `File` that `using` hands the body, and `using` closes
// it afterwards.
#[test]
fn file_read_pulls_bytes_inside_the_bracket() {
    let source = r#"
        use /std/{File, Io, Str, Bin, Task};
        match Task/block_on(File/with("lines.txt", Io/Mode/read(), (f) =>
            Task/bind(File/read(f, 1024), (r) =>
                match r : Task(Bin)
                | chunk(b) => Task/pure(b)
                | eof() => Task/pure(\\)
                | error(_) => Task/pure(\\)
                end)))
        | success(bytes) => Io/write(Io/stdout, bytes)
        | failure(_) => Io/write(Io/stdout, Str/to_bin("error"))
        end
        "#;

    let (system, io) =
        MockHost::builder().files([("lines.txt", "first\nsecond\n")]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"first\nsecond\n"
    );
}

#[test]
fn std_io_read_line_sequences_lines() {
    let source = r#"
        use /std/{Io, Reader, Option, Bin, Str};
        let program : Reader({}) =
            let ! = Reader/bind;
            let first = Reader/read_line!;
            let second = Reader/read_line!;
            match first : Reader({})
            | some(a) =>
                match second : Reader({})
                | some(b) =>
                    match Str/of_bin(Bin/concat(a, b)) : Reader({})
                    | some(s) => Reader/pure(Io/print(s))
                    | none() => Reader/pure(Io/print("invalid utf-8"))
                    end
                | none() => Reader/pure(Io/print("missing"))
                end
            | none() => Reader/pure(Io/print("missing"))
            end;
        Reader/run(program, Io/stdin)
        "#;

    let (system, io) = MockHost::builder().stdin_lines(["alpha", "beta"]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"alpha\nbeta\n"
    );
}

#[test]
fn std_io_read_line_signals_eof_with_none() {
    let source = r#"
        use /std/{Io, Reader, Option};
        let program : Reader({}) =
            let ! = Reader/bind;
            let first = Reader/read_line!;
            let second = Reader/read_line!;
            match second : Reader({})
            | some(_) => Reader/pure(Io/print("line"))
            | none() => Reader/pure(Io/print("eof"))
            end;
        Reader/run(program, Io/stdin)
        "#;

    let (system, io) = MockHost::builder().stdin_lines(["only"]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"eof"
    );
}

// A line longer than `read_line`'s 1024-byte refill chunk forces the buffer
// to absorb one full chunk, miss the newline, and refill before slicing.
#[test]
fn std_io_read_line_spans_refills() {
    let source = r#"
        use /std/{Io, Reader, Option, Bin, Nat};
        let program : Reader({}) =
            let ! = Reader/bind;
            let line = Reader/read_line!;
            match line : Reader({})
            | some(bytes) => Reader/pure(Io/print(Nat/to_str(Bin/len(bytes))))
            | none() => Reader/pure(Io/print("eof"))
            end;
        Reader/run(program, Io/stdin)
        "#;

    let long_line = "a".repeat(1500);
    let (system, io) = MockHost::builder().stdin_lines([long_line.as_str()]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"1501"
    );
}

#[test]
fn triangular_sum() {
    let source = r#"
        let result : std/Nat =
            match 5 : std/Nat
            | 0 => 0
            | pred + 1, ih => std/Nat/add(ih, pred)
            end;
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Nat/to_str(result)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"10"
    );
}

#[test]
fn match_omitted_motive_infers() {
    // The same induction as `triangular_sum`, but with the motive omitted. It is
    // non-dependent (every arm has type `std/Nat`), so the synthesized metavar
    // motive is solved by the arms — no explicit `: std/Nat` needed.
    let source = r#"
        let result : std/Nat =
            match 5
            | 0 => 0
            | pred + 1, ih => std/Nat/add(ih, pred)
            end;
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Nat/to_str(result)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"10"
    );
}

#[test]
fn multi_arg_function() {
    let source = r#"
        let add : (std/Int, std/Int) -> std/Int = (x, y) => std/Int/add(x, y);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(add(+3, +4))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+7"
    );
}

#[test]
fn curried_function() {
    let source = r#"
        let add : (std/Int) -> (std/Int) -> std/Int = (x) => (y) => std/Int/add(x, y);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(add(+3)(+4))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+7"
    );
}

#[test]
fn let_bang_identity_monad_sequences_bangs() {
    // A minimal Identity monad over `std/Nat`: `bind(m, f) = f(m)`. The compiler is
    // monad-agnostic — `let ! = bind;` applies the binary `bind` to `(action, cont)`
    // per `!` site — so the sugar `add(a!, b!)` threads each banged value through a
    // fresh continuation and evaluates to `add(a, b)`.
    let source = r#"
        let bind : (std/Nat, (std/Nat) -> std/Nat) -> std/Nat = (m, f) => f(m);
        let a : std/Nat = 3;
        let b : std/Nat = 4;
        let result : std/Nat = let ! = bind; std/Nat/add(a!, b!);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Nat/to_str(result)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
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
fn let_bang_region_mixes_action_types() {
    // A single region sequences two actions of *different* payload types: a
    // `Parse(Bin)` (`take_while`) and a `Parse(Nat)` (`any_byte`). This works only
    // because `let ! = Parse/bind;` is re-elaborated per `!` site, so each site gets
    // its own holes (`?A := Bin` for the first, `?A := Nat` for the second). A single
    // shared bind value would force one `A` and reject this. On "AB": `take_while(is_a)`
    // reads "A" (stops at 'B'), then `any_byte` reads 'B' (66); `Bin/append("A", 66)`
    // is "AB".
    let source = r#"
        use /std/{Parse, Nat, Bin, Bln, Result, Io, Str};

        let is_a : (Nat) -> Bln = (b) => match b : Bln | 'A' => true | _ => false end;

        let parser : Parse/Parse(Bin) =
            let ! = Parse/bind;
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
    assert_eq!(
        io.output(),
        b"AB"
    );
}

#[test]
fn vec_cons_with_nat_succ() {
    let source = r#"
        rec Vec(T : Type, n : std/Nat) -> Type =
            match n : Type
            | 0 => {}
            | pred + 1, ih => { T, ih }
            end;

        let cons(T : Type, n : std/Nat, x : T, xs : Vec(T, n)) -> Vec(T, std/Nat/succ(n)) =
            (x, xs);

        let head(T : Type, n : std/Nat, xs : Vec(T, std/Nat/succ(n))) -> T =
            xs.0;

        let v : Vec(std/Nat, 1) = cons(std/Nat, 0, 42, ());
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Nat/to_str(head(std/Nat, 0, v))))
    "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"42"
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
        use /std/{Nat};
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
    // dissolves the residual primitive wrappers at every call site (including the
    // `Str/of_bin` validation guarding the runtime `%s` argument). Together they
    // collapse the post-§1 residue (≈14 funcs) down to a handful — the assert pins
    // a comfortable upper bound while leaving headroom for legitimate std/Fmt drift.
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

    let (system, io) = MockHost::builder().stdin_lines(["Alice"]).build();
    crate::run_wasm(&wasm_module, system).expect("execution succeeded");
    assert_eq!(
        io.output(),
        b"Alice is 30"
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
        use /std/{Nat, Bin, Io};
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
        Io/write(Io/stdout, /std/Str/to_bin(Nat/to_str(total(c, 0))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
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
        use /std/{Nat, Bin, Io};
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
        match inferred : {}
        | refl(z) => let _ = Io/write(Io/stdout, /std/Str/to_bin(Nat/to_str(z))); ()
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"2");
}

#[test]
fn implicit_union_type_param_rejects_explicit_spelling() {
    // With `@A` implicit, the old explicit spelling queues `Nat` into the
    // explicit slots — one argument too many, an error rather than a silent
    // reinterpretation. (`Eq2(@Nat, 2, 2)` is the pinned spelling.)
    let source = r#"
        use /std/{Nat, Io};
        union Eq2(@A : Type) : (x : A, y : A)
        | refl(@z : A) : (z, z)
        end
        let bad : Eq2(Nat, 2, 2) = Eq2/refl();
        Io/write(Io/stdout, /std/Str/to_bin("no"))
        "#;

    let (system, _io) = MockHost::builder().build();
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
        use /std/{Nat, Io};
        union Eq2(@A : Type) : (x : A, y : A)
        | refl(@z : A) : (z, z)
        end
        let sym2(@A : Type, @x : A, @y : A, p : Eq2(x, y)) -> Eq2(y, x) =
            match p : (q : Eq2(A, s, t)) => Eq2(t, s)
            | refl(z) => Eq2/refl()
            end;
        let direct : Eq2(2, 2) = sym2(Eq2/refl());
        let chained : Eq2(3, 3) = sym2(sym2(Eq2/refl()));
        match chained : {}
        | refl(z) => let _ = Io/write(Io/stdout, /std/Str/to_bin(Nat/to_str(z))); ()
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"3");
}

#[test]
fn parked_constraints_still_reject_the_unsolvable() {
    // An undecidable-at-first constraint that never resolves must still fail —
    // at the item drain, attributed to its origin. `refl` forces both indices
    // equal; `2` and `3` are not.
    let source = r#"
        use /std/{Nat, Io};
        union Eq2(@A : Type) : (x : A, y : A)
        | refl(@z : A) : (z, z)
        end
        let bad : Eq2(2, 3) = Eq2/refl();
        Io/write(Io/stdout, /std/Str/to_bin("no"))
        "#;

    let (system, _io) = MockHost::builder().build();
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

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"4");
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
        match Parse/run(pairer, /std/Str/to_bin("hi"))
        | success(pair) => Io/print(Nat/to_str(pair.0))
        | failure(_) => Io/print("error")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"104"
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
        match v : {}
        | nil() => ()
        | cons(p, rest) => let _ = Io/write(Io/stdout, /std/Str/to_bin(Nat/to_str(p.1))); ()
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"2");
}

#[test]
fn checking_problem_without_a_pin_still_rejects() {
    // A checking problem whose expected type is never pinned drains as a
    // cannot-infer at the tuple's own span — parked, not silently accepted.
    let source = r#"
        use /std/{Nat, Io};
        let swallow(@A : Type, a : A) -> Nat = 0;
        let n : Nat = swallow((1, 2));
        Io/write(Io/stdout, /std/Str/to_bin(Nat/to_str(n)))
        "#;

    let (system, _io) = MockHost::builder().build();
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

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
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

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"12"
    );
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

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"5");
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

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"42"
    );
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

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"42"
    );
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

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(
        error.contains("representation"),
        "unexpected error: {error}"
    );
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

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(
        error.contains("field") && error.contains("private"),
        "unexpected error: {error}"
    );
}

// Diagnostics name binders with the source names the user wrote, not the
// `hint#counter` gensyms elaboration opens them under (axis (a)): the inferred
// function type must read `(n : Nat)`, never `(n#3 : Nat)`.
#[test]
fn diagnostic_uses_source_binder_names() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat = n;
        let bad : Nat = f;
        bad
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
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
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
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
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(
        error.contains("inferred: Vec(Nat, n)"),
        "globals not shortened: {error}"
    );
    assert!(
        !error.contains("std/Vec"),
        "qualified union path leaked: {error}"
    );
    assert!(
        !error.contains("sys/"),
        "qualified prim path leaked: {error}"
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

    let (system, _io) = MockHost::builder().build();
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

    let (system, _io) = MockHost::builder().build();
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

    let (system, _io) = MockHost::builder().build();
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

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(error.contains("struct type"), "unexpected error: {error}");
}

// === struct destructuring ===============================================

// A struct pattern in a `let` binds each named field by label (a pun binds the
// field's own name); the head's parameters are inferred.
#[test]
fn struct_destructure_pun_binds_fields_by_label() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 2, snd = 5 };
        let Pair { fst, snd } = p;
        Io/print(Nat/to_str(Nat/add(fst, snd)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

// A rename field `fst = a` binds the field to a fresh name.
#[test]
fn struct_destructure_rename_binds_new_names() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 2, snd = 5 };
        let Pair { fst = a, snd = b } = p;
        Io/print(Nat/to_str(Nat/mul(a, b)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"10"
    );
}

// Naming a subset of fields is allowed — the rest are simply ignored.
#[test]
fn struct_destructure_partial_ignores_unlisted_fields() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 9, snd = 5 };
        let Pair { fst } = p;
        Io/print(Nat/to_str(fst))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"9");
}

// Patterns nest: a renamed field can itself be a struct pattern.
#[test]
fn struct_destructure_nested_struct_pattern() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Inner pub { a : Nat, b : Nat }
        pub struct Outer pub { it : Inner, c : Nat }
        let o : Outer = Outer { it = Inner { a = 1, b = 2 }, c = 3 };
        let Outer { it = Inner { a, b }, c } = o;
        Io/print(Nat/to_str(Nat/add(Nat/add(a, b), c)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"6");
}

// The head is checked nominally: destructuring a `Pair` as a structurally
// identical but distinctly-named `Other` is rejected (not silently projected).
#[test]
fn struct_destructure_wrong_head_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        pub struct Other pub { fst : Nat, snd : Nat }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 2 };
        let Other { fst, snd } = p;
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

// A struct pattern works as an un-annotated lambda parameter — the domain comes
// from the head, parameters inferred against the expected function type.
#[test]
fn struct_destructure_in_lambda_parameter() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let sum : (_ : Pair(Nat, Nat)) -> Nat = (Pair { fst, snd }) => Nat/add(fst, snd);
        let p : Pair(Nat, Nat) = Pair { fst = 6, snd = 1 };
        Io/print(Nat/to_str(sum(p)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

// A struct pattern destructures a constructor's payload in a match arm.
#[test]
fn struct_destructure_in_match_arm() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        union Wrap | wrap(Pair(Nat, Nat)) end
        let w : Wrap = Wrap/wrap(Pair { fst = 3, snd = 8 });
        let out : Nat =
            match w : Nat
            | wrap(Pair { fst, snd }) => Nat/add(fst, snd)
            end;
        Io/print(Nat/to_str(out))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"11"
    );
}

// Destructuring projects through the representation-privacy boundary: a
// private-representation struct's field cannot be pulled out from another module.
#[test]
fn struct_destructure_private_field_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius { value : Nat }
            pub let of_nat(n : Nat) -> Celsius = Celsius { value = n };
        end
        let c : Celsius/Celsius = Celsius/of_nat(42);
        let Celsius/Celsius { value } = c;
        Io/print(Nat/to_str(value))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(
        error.contains("field") && error.contains("private"),
        "unexpected error: {error}"
    );
}

// === Pattern matrix compilation =========================================

// A nested constructor pattern dispatches on the tail of the list as well as its
// head — `cons(x, cons(y, _))` reads the first two elements in one arm.
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
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
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
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"107"
    );
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
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"1307"
    );
}

// A `Nat` literal nested inside a struct field pattern: the struct column expands
// to its labels, and the `tag` sub-column dispatches via `switch`.
#[test]
fn matrix_nat_literal_in_struct_field() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Tagged pub { tag : Nat, val : Nat }
        let read(t : Tagged) -> Nat =
            match t
            | Tagged { tag = 0, val = v } => v
            | Tagged { tag = _, val = _ } => 999
            end;
        Io/print(Nat/to_str(read(Tagged { tag = 0, val = 42 })))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"42"
    );
}

// A `_` fallthrough at a union column expands into the *unlisted* constructors:
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
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
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
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
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
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"2");
}

// Coverage is left to core: a union match that lists neither every constructor
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
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(
        error.contains("missing match case") && error.contains("nil"),
        "unexpected error: {error}"
    );
}

// Expanding a `_` at a union column needs the constructor's union; when the
// constructors are not in scope (no `use`), the tag cannot be resolved and the
// match is rejected with an actionable error.
#[test]
fn matrix_wildcard_unresolved_constructor_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        union Shape | dot() | line(Nat) end
        let area(s : Shape) -> Nat =
            match s
            | line(n) => n
            | _       => 0
            end;
        Io/print(Nat/to_str(area(Shape/dot())))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(
        error.contains("line") && error.contains("resolve"),
        "unexpected error: {error}"
    );
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

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"hello"
    );
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

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), [0xc3, 0xa9]);
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

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"rejected"
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

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"rejected"
    );
}

// `Str/len` and `Str/get` count and index by codepoint, not byte. The string is
// `a€😀` — a 1-byte, a 3-byte, and a 4-byte scalar — so its length is 3 and the
// codepoints decode to U+0061 (97), U+20AC (8364), and U+1F600 (128512).
#[test]
fn str_get_indexes_codepoints_of_every_width() {
    let source = r#"
        use /std/{Str, Nat, Io};
        match Str/of_bin(\61\e2\82\ac\f0\9f\98\80) : {}
        | some(s) =>
            Io/print(Str/concat_all([
                Nat/to_str(Str/len(s)), ",",
                Nat/to_str(Str/get(s, 0)), ",",
                Nat/to_str(Str/get(s, 1)), ",",
                Nat/to_str(Str/get(s, 2))
            ]))
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"3,97,8364,128512"
    );
}

// `Str/slice` cuts at codepoint boundaries, so slicing `[1, 2)` out of `a€😀`
// yields the whole 3-byte euro sign — never a split sequence.
#[test]
fn str_slice_cuts_on_codepoint_boundaries() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bin(\61\e2\82\ac\f0\9f\98\80) : {}
        | some(s) => Io/print(Str/slice(s, 1, 2))
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), [0xe2, 0x82, 0xac]);
}

// `Str/trim` is string-typed and strips only the leading/trailing ASCII
// whitespace, leaving the interior multibyte scalar (`café`, with a 2-byte `é`)
// intact.
#[test]
fn str_trim_keeps_interior_multibyte() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bin(\20\20\63\61\66\c3\a9\20\20) : {}
        | some(s) => Io/print(Str/trim(s))
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), [0x63, 0x61, 0x66, 0xc3, 0xa9]);
}

// An all-whitespace string trims to empty: `trim_start` overshoots `trim_end`,
// and the `Nat/min` guard collapses the slice to nothing rather than trapping.
#[test]
fn str_trim_all_whitespace_is_empty() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bin(\20\09\20) : {}
        | some(s) => Io/print(Str/concat(Str/trim(s), "!"))
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"!"
    );
}

// A *non-productive* inner `rec` forced in a type position must degrade to the
// reduce deadline (an error), never hang or panic — the regression guard for
// inner-`rec` reduction at the type level (a `Subterm::Rec` demanded by an
// eliminator is now forced, not left stuck).
#[test]
fn nonproductive_inner_rec_in_type_position_is_preempted() {
    let source = r#"
        use /std/{Bln};
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

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(1), source, system).is_err());
}

#[test]
fn random_bin_returns_requested_length() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"std/Io/write(std/Io/stdout, /std/Rand/bin(8))"#,
        system,
    )
    .expect("expected result");

    let output = io.output();
    assert_eq!(output.len(), 8);
}

#[test]
fn bln_logic_and_of_str() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Bln, Str, Option, Io};
        let computed = Bln/and(Bln/or(false, true), Bln/not(false));
        let parsed = match Bln/of_str("false") : Bln
            | some(b) => b
            | none() => true
            end;
        Io/write(Io/stdout, Str/to_bin(Str/concat(Bln/to_str(computed), Bln/to_str(parsed))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"truefalse"
    );
}

#[test]
fn bln_xor_executes() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Bln, Str, Io};
        let a = Bln/xor(true, false);
        let b = Bln/xor(true, true);
        Io/write(Io/stdout, Str/to_bin(Str/concat(Bln/to_str(a), Bln/to_str(b))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"truefalse"
    );
}

#[test]
fn nat_bitwise_ops_execute() {
    // The first input byte is `A` (65); reading it from the host keeps the
    // operand opaque to the optimizer, so each op is lowered to its WebAssembly
    // instruction and executed for real rather than folded at compile time. This
    // is what exercises the truncating `shl` (65 << 25 sets bit 31, which the
    // i31 carrier drops to leave 2^25).
    let (system, io) = MockHost::builder().stdin_lines(["A"]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Io, Bin, Nat, Str};
        let bytes = match Io/read(Io/stdin, 16) : Bin
            | chunk(b) => b
            | eof() => \\
            | error(_) => \\
            end;
        let x = Bin/get(bytes, 0);
        let r = Str/concat_all([
            Nat/to_str(Nat/and(x, 15)), ",",
            Nat/to_str(Nat/or(x, 128)), ",",
            Nat/to_str(Nat/xor(x, 255)), ",",
            Nat/to_str(Nat/shl(x, 25)), ",",
            Nat/to_str(Nat/shr(x, 1))
        ]);
        Io/write(Io/stdout, Str/to_bin(r))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"1,193,190,33554432,32"
    );
}

#[test]
fn int_bitwise_ops_execute() {
    // `x` is the host byte `A` (65) read as an `Int`, kept opaque to the
    // optimizer so each op lowers to its WebAssembly instruction. This exercises
    // the Int-distinctive behaviors: a truncating `shl` that lands on bit 30 and
    // so reloads negative (65 << 24), an arithmetic (sign-preserving) `shr` on a
    // negative operand (-65 >> 1 = -33), and the `xor`-based `not` (-x - 1).
    let (system, io) = MockHost::builder().stdin_lines(["A"]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Io, Bin, Nat, Int, Str};
        let bytes = match Io/read(Io/stdin, 16) : Bin
            | chunk(b) => b
            | eof() => \\
            | error(_) => \\
            end;
        let x = Nat/to_int(Bin/get(bytes, 0));
        let neg = Int/sub(+0, x);
        let r = Str/concat_all([
            Int/to_str(Int/and(x, +15)), ",",
            Int/to_str(Int/or(x, +128)), ",",
            Int/to_str(Int/xor(x, +255)), ",",
            Int/to_str(Int/shl(x, +24)), ",",
            Int/to_str(Int/shr(neg, +1)), ",",
            Int/to_str(Int/not(x))
        ]);
        Io/write(Io/stdout, Str/to_bin(r))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"+1,+193,+190,-1056964608,-33,-66"
    );
}

#[test]
fn nat_of_str_returns_option() {
    // `123` parses; `12a` (non-digit) and the empty string are `none`, taking
    // the `unwrap_or` defaults — `123 + 7 + 9`.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
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

    assert_eq!(
        io.output(),
        b"139"
    );
}

#[test]
fn int_of_str_returns_option() {
    // `-5` and `+7` parse (compared by magnitude); `x` is `none` → default `+3`.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
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

    assert_eq!(
        io.output(),
        b"15"
    );
}

#[test]
fn flt_of_str_returns_option() {
    // `12.0`, `.5` (empty integer part), and `1e3` parse; `abc` is `none` →
    // default `+4.0`. Values are truncated to `Nat` for an exact assertion:
    // `12 + (0.5*2) + 1000 + 4`.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
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

    assert_eq!(
        io.output(),
        b"1017"
    );
}

#[test]
fn option_result_char_helpers() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Option, Result, Char, Nat, Str, Io};
        let opt = Option/unwrap_or(Option/map((x : Nat) => Nat/add(x, 1), Option/some(4)), 0);
        let res0 : Result(Nat, Nat) = Result/success(5);
        let res = Result/unwrap_or(Result/map((x : Nat) => Nat/mul(x, 2), res0), 0);
        let up = Char/to_upper('a');
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Nat/add(Nat/add(opt, res), up))))
        "#,
        system,
    )
    .expect("expected result");

    // opt = 5, res = 10, up = 'A' = 65  ->  80
    assert_eq!(
        io.output(),
        b"80"
    );
}

#[test]
fn clock_diff_of_two_distinct_now_readings() {
    // Two scripted wall readings 30 s + 400 ns apart. `Time/now` referenced
    // twice must perform two *distinct* host calls (the nullary-effect
    // distinctness the struct-head reduction relies on), so the diff is the
    // gap between them, not zero.
    let (system, io) =
        MockHost::builder().wall([(1, 100, 500), (1, 130, 900)]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        let a = /std/Time/now();
        let b = /std/Time/now();
        let d = /std/Time/diff(b, a);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(/std/Nat/to_str(/std/Time/secs(d))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"30"
    );
}

#[test]
fn clock_mono_reads_scripted_elapsed() {
    let (system, io) = MockHost::builder().mono([(2, 7)]).build();
    crate::run_text(
        Duration::from_secs(5),
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
fn proc_args_indexes_the_argv_snapshot() {
    // argv crosses as a host-built `Arr(Bin)`; indexing it round-trips one entry.
    let (system, io) =
        MockHost::builder().args(["prog", "hello", "world"]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"std/Io/write(std/Io/stdout, /std/Arr/get(/std/Proc/args, 1))"#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"hello"
    );
}

#[test]
fn proc_env_found_unwraps_to_some() {
    let (system, io) = MockHost::builder().env([("HOME", "/root")]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        match /std/Proc/env("HOME") : {}
        | some(v) => let _ = std/Io/write(std/Io/stdout, v); ()
        | none() => let _ = std/Io/write(std/Io/stdout, /std/Str/to_bin("missing")); ()
        end
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"/root"
    );
}

#[test]
fn proc_env_absent_is_none() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        match /std/Proc/env("NOPE") : {}
        | some(v) => let _ = std/Io/write(std/Io/stdout, v); ()
        | none() => let _ = std/Io/write(std/Io/stdout, /std/Str/to_bin("missing")); ()
        end
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"missing"
    );
}

#[test]
fn proc_exit_halts_with_code() {
    // exit traps: it surfaces its code *and* the trailing write never runs.
    let entrypoint = r#"
        let _ : std/Void = /std/Proc/exit(7);
        std/Io/write(std/Io/stdout, /std/Str/to_bin("unreachable"))
        "#
    .parse::<crate::text::Entrypoint>()
    .expect("failed to parse source");

    let module = crate::compile_entrypoint(
        Duration::from_secs(5),
        &entrypoint,
        &crate::text::NullLoader,
        |_| {},
    )
    .expect("compile succeeded");

    let (system, io) = MockHost::builder().build();
    let code = crate::run_wasm(&module, system).expect("execution succeeded");

    assert_eq!(code, 7);
    assert!(io.output().is_empty());
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
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+4"
    );
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
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+3"
    );
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
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+4"
    );
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
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+30"
    );
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
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+8"
    );
}

#[test]
fn lambda_tuple_param_destructures() {
    // A bare lambda taking one pair parameter needs its own parens: `((a, b))`.
    let source = r#"
        let fst : (_ : { std/Int, std/Int }) -> std/Int = ((a, b)) => a;
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(fst((+5, +6)))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+5"
    );
}

#[test]
fn match_arm_tuple_destructures() {
    // A constructor whose payload is a tuple destructures inside the arm binder.
    let source = r#"
        union Boxed
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
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+9"
    );
}

// Client network IO (Phase A): `connect` rides the `Hdl` byte stream, so
// `Net/call` writes a request and drains the scripted response to EOF.
#[test]
fn net_call_round_trips_a_scripted_endpoint() {
    let source = r#"
        use /std/{Net, Io, Str, Task};
        match Task/block_on(Net/call(Net/default, "example.com", 80, Str/to_bin("GET /\r\n\r\n")))
        | success(response) => Io/write(Io/stdout, response)
        | failure(_) => Io/write(Io/stdout, Str/to_bin("error"))
        end
        "#;

    let (system, io) = MockHost::builder()
        .net([("example.com:80", "HTTP/1.0 200 OK\r\n\r\nhello")])
        .build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"HTTP/1.0 200 OK\r\n\r\nhello"
    );
}

// Connecting to an endpoint that was never scripted is refused, and the status
// decodes to `Net/refused`.
#[test]
fn net_call_to_an_unscripted_endpoint_is_refused() {
    let source = r#"
        use /std/{Net, Io, Task};
        match Task/block_on(Net/call(Net/default, "example.com", 80, /std/Str/to_bin("ping")))
        | success(_) => Io/print("connected")
        | failure(e) =>
            match e : {}
            | refused() => Io/print("refused")
            | tls() => Io/print("tls")
            | not_found() => Io/print("not found")
            | permission_denied() => Io/print("denied")
            | exists() => Io/print("exists")
            | other(_) => Io/print("other")
            end
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"refused"
    );
}

// A custom `Config` with an optional `Duration` timeout flows through the
// bracket; `Net/read` pulls bytes from the socket the body is handed.
#[test]
fn net_with_custom_timeout_config_reads_response() {
    let source = r#"
        use /std/{Net, Io, Str, Bin, Option, Time, Task};
        let settings = Net/Settings {
            connect_timeout = Option/some(Time/of_millis(500)),
            read_timeout = Option/none(),
            write_timeout = Option/none(),
            tls = false
        };
        match Task/block_on(Net/with(settings, "db.internal", 5432, (s) =>
            Task/bind(Net/read(s, 64), (r) =>
                match r : Task(Bin)
                | chunk(b) => Task/pure(b)
                | eof() => Task/pure(\\)
                | error(_) => Task/pure(\\)
                end)))
        | success(bytes) => Io/write(Io/stdout, bytes)
        | failure(_) => Io/write(Io/stdout, Str/to_bin("error"))
        end
        "#;

    let (system, io) = MockHost::builder().net([("db.internal:5432", "PONG")]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"PONG"
    );
}

// Server network IO (Stage A): `serve` binds a listener, pulls the scripted
// inbound connection, and runs the handler per connection — which reads the
// request off the socket and writes a response the host captures. The exhausted
// inbound queue then fails the next `accept`, ending the loop and closing the
// bracketed listener.
#[test]
fn net_serve_handles_a_scripted_inbound_connection() {
    let source = r#"
        use /std/{Net, Io, Str, Bin, Task};
        match Task/block_on(Net/serve("0.0.0.0", 8080, (c) =>
            Task/bind(Net/read(c, 64), (r) =>
                match r : Task({})
                | chunk(bytes) =>
                    Task/bind(Net/write(c, Bin/concat(Str/to_bin("echo: "), bytes)), (wrote) => Task/pure(()))
                | eof() => Task/pure(())
                | error(_) => Task/pure(())
                end))) : {}
        | success(u) => ()
        | failure(_) => Io/print("listen failed")
        end
        "#;

    let (system, io) = MockHost::builder().inbound(["ping"]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.captures(), vec![b"echo: ping".to_vec()]);
}

// TLS client (Stage A): `Net/with` with `tls = true` upgrades the connected
// socket via `start_tls` before the body runs. The mock host serves the
// scripted endpoint cleartext (no real handshake under test), so the upgrade is
// a no-op identity and the round-trip still succeeds — exercising the wiring,
// types, and prim threading end to end through codegen.
#[test]
fn net_with_tls_upgrades_and_reads() {
    let source = r#"
        use /std/{Net, Io, Str, Bin, Option, Task};
        let settings = Net/Settings {
            connect_timeout = Option/none(),
            read_timeout = Option/none(),
            write_timeout = Option/none(),
            tls = true
        };
        match Task/block_on(Net/with(settings, "secure.example", 443, (s) =>
            Task/bind(Net/read(s, 64), (r) =>
                match r : Task(Bin)
                | chunk(b) => Task/pure(b)
                | eof() => Task/pure(\\)
                | error(_) => Task/pure(\\)
                end)))
        | success(bytes) => Io/write(Io/stdout, bytes)
        | failure(_) => Io/write(Io/stdout, Str/to_bin("error"))
        end
        "#;

    let (system, io) = MockHost::builder()
        .net([("secure.example:443", "SECURE-PONG")])
        .build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"SECURE-PONG");
}

// Server TLS termination (Stage A): `serve_tls` builds a config token, then
// upgrades each accepted connection via `start_tls_server` before the handler
// runs. The mock host runs cleartext, so the upgrade is a no-op identity and
// the handler echoes the scripted request the host captures.
#[test]
fn net_serve_tls_handles_a_scripted_inbound_connection() {
    let source = r#"
        use /std/{Net, Io, Str, Bin, Task};
        match Task/block_on(Net/serve_tls("0.0.0.0", 8443, Str/to_bin("CERT"), Str/to_bin("KEY"), (c) =>
            Task/bind(Net/read(c, 64), (r) =>
                match r : Task({})
                | chunk(bytes) =>
                    Task/bind(Net/write(c, Bin/concat(Str/to_bin("tls: "), bytes)), (wrote) => Task/pure(()))
                | eof() => Task/pure(())
                | error(_) => Task/pure(())
                end))) : {}
        | success(u) => ()
        | failure(_) => Io/print("serve failed")
        end
        "#;

    let (system, io) = MockHost::builder().inbound(["ping"]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.captures(), vec![b"tls: ping".to_vec()]);
}

// HTTP client (Phase B): `Http/perform` renders a `Request`, sends it through
// `/std/Net`, and runs the `/std/Parse`-based response parser over the reply —
// exercising the byte-scanning parser end to end through codegen.
#[test]
fn http_perform_parses_a_scripted_response() {
    let source = r#"
        use /std/{Http, Io, Str, Nat, Task};
        match Task/block_on(Http/perform(Http/get("example.com", 80, "/"))) : {}
        | success(response) =>
            let ct = match Http/header(response, "Content-Type") : Str
                | some(value) => value
                | none() => "none"
                end;
            match Str/of_bin(response.body) : {}
            | some(body) =>
                let _ = Io/write(Io/stdout, Str/to_bin(Str/concat_all([
                    Nat/to_str(response.status.code), " ", ct, " ", body
                ]))); ()
            | none() => let _ = Io/write(Io/stdout, Str/to_bin("bad body")); ()
            end
        | failure(_) => let _ = Io/write(Io/stdout, Str/to_bin("error")); ()
        end
        "#;

    // The trailing bytes past `Content-Length: 5` must be dropped by the body
    // framing, leaving the body exactly "hello".
    let (system, io) = MockHost::builder()
        .net([(
            "example.com:80",
            "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 5\r\n\r\nhello AND MORE",
        )])
        .build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"200 text/plain hello"
    );
}

#[test]
fn task_scheduler_parks_polls_and_resumes() {
    // The `/std/Task` event loop end to end: the root fiber yields a `wait` on
    // stdin-READ and parks, `run` marshals the parked handle/interest into
    // `Io/poll` (the mock reports it ready), and resumes the continuation — which
    // performs the write. Exercises the novel path of a union variant carrying a
    // closure through erasure and codegen.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str};
        let prog : Task({}) =
            Task/wait(Io/stdin, 1, () =>
                let wrote = Io/write(Io/stdout, Str/to_bin("ok"));
                Task/done(()));
        Task/run(prog)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"ok");
}

#[test]
fn task_bind_reads_and_echoes() {
    // The monad surface: a `with`-bind do-block over `Task/bind`, sequencing the
    // `read` leaf (which completes without parking under the mock) into `write`,
    // driven to its value by `block_on`. Exercises `bind`, the leaf actions, and
    // do-notation against the new module.
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io};
        let prog : Task({}) =
            let ! = Task/bind;
            let r = Task/read(Io/stdin, 1024)!;
            match r : Task({})
            | chunk(bytes) =>
                let wrote = Io/write(Io/stdout, bytes);
                Task/pure(())
            | eof() => Task/pure(())
            | error(_) => Task/pure(())
            end;
        Task/block_on(prog)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello\n");
}

#[test]
fn task_block_on_multiplexes_children_with_a_typed_root() {
    // `block_on` returns a typed value AND multiplexes spawned children: the root
    // spawns a child, both park on stdin-READ, a single `poll` wakes both, the
    // child's write fires (concurrency, not the old sequential drain), the root's
    // write fires, and `block_on` hands back the root's `Nat`. Output proves all
    // three: child effect, root effect, returned value.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str, Nat, Lst};
        let child : Task({}) =
            Task/wait(Io/stdin, 1, () =>
                let w = Io/write(Io/stdout, Str/to_bin("child;"));
                Task/done(()));
        let root : Task(Nat) =
            Task/spawn(Lst/nil(), child, () =>
                Task/wait(Io/stdin, 1, () =>
                    let w = Io/write(Io/stdout, Str/to_bin("root;"));
                    Task/pure(7)));
        let result = Task/block_on(root);
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(result)))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"child;root;7");
}

#[test]
fn nursery_awaits_a_spawned_child() {
    // Structured concurrency, the join side: the nursery body finishes immediately
    // after spawning a child that parks on stdin. Because the child is *owned* by
    // the nursery, `run` does not return until the child has been polled awake and
    // performed its write. (Without the nursery the root would finish first and the
    // child would be dropped — see `block_on_drops_a_parked_child_when_root_done`.)
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str};
        let child : Task({}) =
            Task/wait(Io/stdin, 1, () =>
                let w = Io/write(Io/stdout, Str/to_bin("child;"));
                Task/done(()));
        Task/run(Task/nursery(Task/go(child)))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"child;");
}

#[test]
fn nested_nurseries_each_await_their_own_scope() {
    // Structured concurrency composes: a nursery inside a fiber that is itself owned
    // by an outer nursery. The grandchild is spawned into the INNER scope, so the
    // inner `join` does not let `child` continue until the grandchild has run; the
    // outer `join` does not let the root continue until `child` has run. Each scope
    // awaits only its own fibers, and the orderings stack: "gc;" before "child;"
    // (inner await), "child;" before "done;" (outer await).
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str};
        let grandchild : Task({}) =
            Task/wait(Io/stdin, 1, () =>
                let w = Io/write(Io/stdout, Str/to_bin("gc;"));
                Task/done(()));
        let child : Task({}) =
            Task/bind(Task/nursery(Task/go(grandchild)), (inner) =>
                let w = Io/write(Io/stdout, Str/to_bin("child;"));
                Task/done(()));
        let main : Task({}) =
            Task/bind(Task/nursery(Task/go(child)), (outer) =>
                let w = Io/write(Io/stdout, Str/to_bin("done;"));
                Task/pure(()));
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"gc;child;done;");
}

#[test]
fn scoped_cancels_a_helper_when_the_body_returns() {
    // Structured concurrency, the cancel-on-exit side: `scoped` runs its body with
    // a background helper, then cancels the helper the instant the body returns.
    // The helper would write "helper;" once polled awake, but the body completes
    // synchronously first, so `close` cancels the helper before it ever runs — only
    // the body's "body;" reaches stdout, and the body's value is still delivered.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str};
        let helper : Task({}) =
            Task/wait(Io/stdin, 1, () =>
                let w = Io/write(Io/stdout, Str/to_bin("helper;"));
                Task/done(()));
        let main : Task({}) =
            Task/scoped(
                Task/bind(Task/go(helper), (started) =>
                    let w = Io/write(Io/stdout, Str/to_bin("body;"));
                    Task/pure(())));
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"body;");
}

#[test]
fn go_using_runs_the_guard_when_the_child_is_reaped_before_it_steps() {
    // The fd-leak fix in `Net/serve`: a connection accepted in the parent is handed
    // to a spawned worker that must close it. A plain `go` spawns the worker with an
    // EMPTY guard list, so the worker only registers its `close` on its first step —
    // a scope cancel that reaps it before then drops the resource unclosed.
    // `go_using` seeds the worker's handle-keyed guard at birth, so the release runs
    // even on reap-before-step. Here `scoped` cancels the worker the instant the body
    // returns — before the worker (which would park on stdin and write "worker;")
    // ever steps — yet "released;" still reaches stdout. With a plain `go` this would
    // print only "body;".
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str};
        let release : () -> {} =
            () =>
                let w = Io/write(Io/stdout, Str/to_bin("released;"));
                ();
        let worker : Task({}) =
            Task/wait(Io/stdin, 1, () =>
                let w = Io/write(Io/stdout, Str/to_bin("worker;"));
                Task/done(()));
        let main : Task({}) =
            Task/scoped(
                Task/bind(Task/go_using(Io/stdin, release, worker), (started) =>
                    let w = Io/write(Io/stdout, Str/to_bin("body;"));
                    Task/pure(())));
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"body;released;");
}

#[test]
fn block_on_drops_a_parked_child_when_root_done() {
    // Prompt drop and no deadlock: a fire-and-forget child (no nursery) parks on
    // stdin, but the root writes and finishes first. The root's completion cancels
    // the root group, dropping the still-parked child promptly instead of blocking
    // forever in `Io/poll` on work nothing will ever join. Only "root;" is written,
    // and `run` returns rather than hanging — the failure mode the old rootless
    // `run` had.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str};
        let child : Task({}) =
            Task/wait(Io/stdin, 1, () =>
                let w = Io/write(Io/stdout, Str/to_bin("child;"));
                Task/done(()));
        let main : Task({}) =
            Task/bind(Task/go(child), (started) =>
                let w = Io/write(Io/stdout, Str/to_bin("root;"));
                Task/pure(()));
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"root;");
}

#[test]
fn run_schedules_a_heterogeneous_fiber_list() {
    // Two children of different result types (`Task(Nat)` and `Task({})`) spawned
    // into one nursery, boxed as `Fiber`s in a single ready queue; both park, a
    // single poll wakes them, and both continuations fire. Differing result types
    // coexisting in one queue is the point of the flattened, `Fiber`-based
    // scheduler; the nursery owns the children, so `run` awaits both before it
    // returns instead of dropping them.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str, Nat};
        let one : Task(Nat) =
            Task/wait(Io/stdin, 1, () =>
                let w = Io/write(Io/stdout, Str/to_bin("one;"));
                Task/pure(1));
        let two : Task({}) =
            Task/wait(Io/stdin, 1, () =>
                let w = Io/write(Io/stdout, Str/to_bin("two;"));
                Task/done(()));
        let main : Task({}) =
            Task/nursery(
                Task/bind(Task/go(Task/bind(one, (n) => Task/pure(()))), (started) =>
                    Task/go(two)));
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"one;two;");
}

#[test]
fn label_projection_resolves_on_a_type_valued_field() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str, Nat};
        let Box : Type = { A : Type, t : Task(A) };
        let step(b : Box) -> Box =
            match b.t : Box
            | done(a) => (b.A, Task/done(a))
            | wait(h, ev, k) => (b.A, k())
            | spawn(gs, c, k) => (b.A, k())
            | acquire(h, fin, k) => (b.A, k())
            | release(h, k) => (b.A, k())
            | scope(k) => (b.A, k())
            | join(k) => (b.A, k())
            | cancel(k) => (b.A, k())
            end;
        let boxed : Box = (Nat, Task/pure(7));
        let stepped = step(boxed);
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"ok");
}

#[test]
fn heterogeneous_existential_task_list_through_a_generic_map() {
    // A `Lst` of existential-boxed tasks of DIFFERENT result types, mapped by a
    // generic HOF whose body does an indirect closure call on a continuation
    // pulled out of the box. The arity-1 closure definition is inlined away by
    // the specializer, leaving the `call_ref` with no surviving definition — the
    // codegen path that needs the call-site arity registered for `envr`/`clsr`.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str, Nat, Lst};
        let Box : Type = { A : Type, t : Task(A) };
        let boxes : Lst(Box) =
            Lst/cons((Nat, Task/pure(7)),
            Lst/cons(({}, Task/pure(())),
            Lst/nil()));
        let stepped = Lst/map((b : Box) =>
            match b.t : Box
            | done(a) => (b.A, Task/done(a))
            | wait(h, ev, k) => (b.A, k())
            | spawn(gs, c, k) => (b.A, k())
            | acquire(h, fin, k) => (b.A, k())
            | release(h, k) => (b.A, k())
            | scope(k) => (b.A, k())
            | join(k) => (b.A, k())
            | cancel(k) => (b.A, k())
            end, boxes);
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Lst/len(stepped))))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"2");
}
