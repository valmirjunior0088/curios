use {
    curios::{Stage, compile_entrypoint, core, text},
    std::time::{Duration, Instant},
};

fn main() {
    let timeout = Duration::from_secs(15);

    // Structs (SYNTAX.md): a transparent record, a zero-cost newtype, and the
    // motivating abstract type — a public type whose representation (a `Bin`) is
    // hidden, reachable only through the exported smart constructor/accessor in
    // its own module. All three build, project, and run; the newtype erases to
    // its bare field, so `Meters` is byte-identical to `Nat` at runtime.
    let source = r#"
        use /std/{Bin, Nat, Str, Io};

        pub record Pair(A : Type, B : Type) { fst : A, snd : B }
        pub record Meters { Nat }

        mod Token
            use /std/{Bin};
            pub struct Token { Bin }
            pub let of_bin(b : Bin) -> Token = Token { b };
            pub let to_bin(t : Token) -> Bin = t.0;
        end

        let p : Pair(Nat, Str) = Pair { fst = 7, snd = "!" };
        let m : Meters = Meters { 5 };
        let t : Token/Token = Token/of_bin(Str/to_bin("hi"));

        let _ = Io/write(Io/stdout, Token/to_bin(t));
        Io/print(Nat/to_str(Nat/add(p.fst, m.0)))
        "#;

    let mut last = Instant::now();

    let entrypoint = source
        .parse::<text::Entrypoint>()
        .expect("failed to parse source")
        .with_type("()".parse().unwrap());

    let wasm_module = compile_entrypoint(timeout, &entrypoint, &text::NullLoader, |stage| {
        let now = Instant::now();
        let elapsed = now - last;
        last = now;

        println!(
            "{}: {elapsed:?}",
            match stage {
                Stage::Text(_) => "text",
                Stage::Core(_) => "core",
                Stage::Ersd(_) => "ersd",
                Stage::ErsdOptm(_) => "ersd-optm",
                Stage::Cont(_) => "cont",
                Stage::ContOptm(_) => "cont-optm",
                Stage::Wasm(_) => "wasm",
            }
        );
    })
    .expect("expected wasm module");

    let (system, io) = curios::MockHost::builder().build();
    let t = Instant::now();
    curios::run_wasm(&wasm_module, system).expect("expected result");
    println!("run:  {:?}", t.elapsed());
    assert_eq!(io.output(), b"hi12");

    // The representation boundary: building a private-representation struct from
    // outside its declaring module is a compile-time `PrivateRepresentation`.
    let ill_typed = r#"
        use /std/{Bin};

        mod Token
            use /std/{Bin};
            pub struct Token { Bin }
        end

        let bad : Token/Token = Token/Token { /std/Str/to_bin("x") };
        bad
        "#;

    let entrypoint = ill_typed
        .parse::<text::Entrypoint>()
        .expect("failed to parse ill-typed source");

    let (module, metavars) = text::to_core(&entrypoint, &text::prelude(&text::NullLoader))
        .expect("expected lowered module");
    let result = core::elaborate_module(
        &mut core::Context::new(timeout),
        &module,
        metavars,
        core::Mode::Infer,
    );

    assert!(matches!(
        &result,
        Err(core::Error::Located { error, .. })
            if matches!(error.as_ref(), core::Error::PrivateRepresentation { .. })
    ));

    println!("ok");
}
