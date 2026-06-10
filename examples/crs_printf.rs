use {
    curios::{Stage, compile_entrypoint, core, text},
    std::time::{Duration, Instant},
};

fn main() {
    let timeout = Duration::from_secs(15);

    let source = r#"
        use /std/{Str, Io, Bin, Fmt};

        let name = Str/trim(Io/read(Io/stdin, 1024));
        Fmt/printf("%s is %d")(name)(30)
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
                Stage::Cont(_) => "cont",
                Stage::Optm(_) => "optm",
                Stage::Wasm(_) => "wasm",
            }
        );
    })
    .expect("expected wasm module");

    let (system, receiver) = curios::ChannelHost::in_out(["Alice"]);
    let t = Instant::now();
    curios::run_wasm(&wasm_module, system).expect("expected result");
    println!("run:  {:?}", t.elapsed());
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"Alice is 30".to_vec()]
    );

    let ill_typed = r#"
        /std/Fmt/printf("%d")("Alice")
        "#;

    let entrypoint = ill_typed
        .parse::<text::Entrypoint>()
        .expect("failed to parse ill-typed source");

    let module = text::to_core(&entrypoint, &text::prelude(&text::NullLoader))
        .expect("expected lowered module");
    // Elaborating the module type-checks every top-level item, then the body
    // `printf("%d")("Alice")` — where the mismatch surfaces.
    let result =
        core::elaborate_module(&mut core::Context::new(timeout), &module, core::Mode::Infer);

    assert!(matches!(
        &result,
        Err(core::Error::Located { error, .. })
            if matches!(error.as_ref(), core::Error::TypeMismatch { .. })
    ));
}
