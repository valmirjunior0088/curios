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
        sys/Io/print(sys/Int/to_str(score(pair)))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"+42".to_vec()]
    );
}

#[test]
fn io_print() {
    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), r#"sys/Io/print("hello")"#, system)
        .expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"hello".to_vec()]
    );
}

#[test]
fn io_read() {
    let (system, receiver) = ChannelHost::in_out(["hello"]);
    crate::run_text(
        Duration::from_secs(5),
        r#"sys/Io/print(sys/Io/read())"#,
        system,
    )
    .expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"hello\n".to_vec()]
    );
}

#[test]
fn triangular_sum() {
    let source = r#"
        let result : sys/Nat =
            match 5 : _ => sys/Nat
            | 0 => 0
            | pred + 1, ih => sys/Nat/add(ih, pred)
            end;
        sys/Io/print(sys/Nat/to_str(result))
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
    // motive is solved by the arms — no explicit `: _ => sys/Nat` needed.
    let source = r#"
        let result : sys/Nat =
            match 5
            | 0 => 0
            | pred + 1, ih => sys/Nat/add(ih, pred)
            end;
        sys/Io/print(sys/Nat/to_str(result))
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
        sys/Io/print(sys/Int/to_str(add(+3, +4)))
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
        sys/Io/print(sys/Int/to_str(add(+3)(+4)))
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
        sys/Io/print(sys/Nat/to_str(result))
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

        let cons(T : Type, n : sys/Nat, x : T, xs : Vec(T, n)) -> Vec(T, n + 1) =
            (x, xs);

        let head(T : Type, n : sys/Nat, xs : Vec(T, n + 1)) -> T =
            xs.0;

        let v : Vec(sys/Nat, 1) = cons(sys/Nat, 0, 42, ());
        sys/Io/print(sys/Nat/to_str(head(sys/Nat, 0, v)))
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
    // splice that ultimately lets constant folding see `3` next to the `+ 1`.
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

        let name = Str/trim(Io/read());
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
