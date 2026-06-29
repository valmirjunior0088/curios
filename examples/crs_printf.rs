use {
    curios::{Stage, compile_entrypoint, core, text},
    std::time::{Duration, Instant},
};

fn main() {
    let timeout = Duration::from_secs(15);

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

    let (system, io) = curios::MockHost::builder().stdin_lines(["Alice"]).build();
    let t = Instant::now();
    curios::run_wasm(&wasm_module, system).expect("expected result");
    println!("run:  {:?}", t.elapsed());
    assert_eq!(io.output(), b"Alice is 30");

    let ill_typed = r#"
        /std/Fmt/printf("%d")("Alice")
        "#;

    let entrypoint = ill_typed
        .parse::<text::Entrypoint>()
        .expect("failed to parse ill-typed source");

    let (module, metavars) = text::to_core(&entrypoint, &text::prelude(&text::NullLoader))
        .expect("expected lowered module");
    // Elaborating the module type-checks every top-level item, then the body
    // `printf("%d")("Alice")` — where the mismatch surfaces.
    let result = core::elaborate_module(
        &mut core::Context::new(timeout),
        &module,
        metavars,
        core::Mode::Infer,
    );

    assert!(matches!(
        &result,
        Err(core::Error::Located { error, .. })
            if matches!(error.as_ref(), core::Error::TypeMismatch { .. })
    ));
}
