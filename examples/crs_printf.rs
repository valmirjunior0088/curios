use {
    curios::{cont, core, ersd, text},
    std::{path::Path, time::Duration, time::Instant},
};

fn main() {
    let base = Path::new(file!()).parent().unwrap().join("crs");
    let loader = text::FileLoader::new(&base);
    let timeout = Duration::from_secs(15);

    let source = r#"
        pub mod std;
        use /std/{Str, Io, Bin};

        pub mod fmt;

        let name : Bin = Str/trim(Io/read(()));
        fmt/printf("%s is %d")(name)(30)
        "#;

    let t = Instant::now();
    let text_entrypoint: text::Entrypoint = source
        .parse::<text::Entrypoint>()
        .expect("failed to parse source")
        .with_prelude();
    println!("text:  {:?}", t.elapsed());

    let t = Instant::now();
    let core_term = text::to_core(&text_entrypoint, &loader).expect("expected core term");
    println!("core:  {:?}", t.elapsed());

    let t = Instant::now();
    let ersd_term = core::erase(
        &mut core::Context::new(timeout),
        &core_term,
        &core::TupleType::unit().into(),
    )
    .unwrap_or_else(|e| panic!("failed to erase term: {e}"));
    println!("ersd:  {:?}", t.elapsed());

    let t = Instant::now();
    let cont_module = ersd::to_cont(&ersd_term);
    println!("cont:  {:?}", t.elapsed());

    let t = Instant::now();
    let wasm_module = cont::to_wasm(&cont_module);
    println!("wasm:  {:?}", t.elapsed());

    let (system, receiver) = curios::ChannelProvider::io(vec![b"Alice\n".to_vec()]);
    let t = Instant::now();
    curios::run_wasm(&wasm_module, system).expect("expected result");
    println!("run:   {:?}", t.elapsed());
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"Alice is 30".to_vec()]
    );

    let ill_typed = r#"
        pub mod std;

        pub mod fmt;

        fmt/printf("%d")("Alice")
        "#;

    let text_entrypoint = ill_typed
        .parse::<text::Entrypoint>()
        .expect("failed to parse ill-typed source")
        .with_prelude();
    let core_term = text::to_core(&text_entrypoint, &loader).expect("expected core term");
    let t = Instant::now();
    let result = core::infer(&mut core::Context::new(timeout), &core_term);
    println!("ill-typed infer: {:?}", t.elapsed());
    assert!(matches!(
        &result,
        Err(core::Error::Located { error, .. })
            if matches!(error.as_ref(), core::Error::TypeMismatch { .. })
    ));
}
