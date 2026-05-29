use {
    curios::{Stage, compile, core, text},
    std::{
        path::Path,
        time::{Duration, Instant},
    },
};

fn main() {
    let base = Path::new(file!()).parent().unwrap().join("crs");
    let loader = text::FileLoader::new(&base);
    let timeout = Duration::from_secs(15);

    let source = r#"
        pub mod std;
        use /std/{Str, Io, Bin};

        pub mod fmt;

        let name : Bin = Str/trim(Io/read());
        fmt/printf("%s is %d")(name)(30)
        "#;

    let mut last = Instant::now();

    let wasm_module = compile(timeout, &loader, Some("()"), source, |stage| {
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
        pub mod std;

        pub mod fmt;

        fmt/printf("%d")("Alice")
        "#;

    let entrypoint = ill_typed
        .parse::<text::Entrypoint>()
        .expect("failed to parse ill-typed source")
        .with_prelude();

    let core_term = text::to_core(&entrypoint, &loader).expect("expected core term");
    let core_type = core::infer(&mut core::Context::new(timeout), &core_term);

    assert!(matches!(
        &core_type,
        Err(core::Error::Located { error, .. })
            if matches!(error.as_ref(), core::Error::TypeMismatch { .. })
    ));
}
