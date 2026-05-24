use {
    curios::{cont, core, ersd, text},
    std::{path::Path, time::Duration},
};

fn main() {
    let base = Path::new(file!()).parent().unwrap().join("crs");
    let loader = text::FileLoader::new(&base);
    let timeout = Duration::from_secs(5);

    let source = r#"
        pub mod fmt;
        fmt/printf "%s is %d" "Alice" 30
        "#;

    let text_entrypoint: text::Entrypoint = source.parse().expect("failed to parse source");
    let core_term = text::to_core(&text_entrypoint, &loader);
    let type_ = core::infer(&mut core::Context::new(timeout), &core_term)
        .unwrap_or_else(|e| panic!("failed to infer type: {e}"));
    let ersd_term = core::erase(&mut core::Context::new(timeout), &core_term, &type_)
        .unwrap_or_else(|e| panic!("failed to erase term: {e}"));
    let cont_module = ersd::to_cont(&ersd_term);
    let wasm_module = cont::to_wasm(&cont_module);
    let (system, receiver) = curios::ChannelProvider::out();
    curios::run_wasm(&wasm_module, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"Alice is 30".to_vec()]
    );

    let ill_typed = r#"
        pub mod fmt;
        fmt/printf "%d" "Alice"
        "#;

    let text_entrypoint = ill_typed
        .parse::<text::Entrypoint>()
        .expect("failed to parse ill-typed source");
    let core_term = text::to_core(&text_entrypoint, &loader);
    let result = core::infer(&mut core::Context::new(timeout), &core_term);
    assert!(matches!(result, Err(core::Error::TypeMismatch { .. })));
}
