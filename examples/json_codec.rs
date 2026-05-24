use {
    curios::{cont, core, ersd, text},
    std::{path::Path, time::Duration},
};

fn main() {
    let path = Path::new(file!())
        .parent()
        .unwrap()
        .join("json_codec/main.crs");

    let source = std::fs::read_to_string(&path).expect("failed to read main.crs");
    let base = path.parent().unwrap();
    let loader = text::FileLoader::new(base);

    let text_entrypoint: text::Entrypoint = source.parse().expect("failed to parse source");
    println!("=== text ===");
    println!("{text_entrypoint}");

    let core_term = text::to_core(&text_entrypoint, &loader);
    println!();
    println!("=== core ===");
    println!("{core_term}");

    let timeout = Duration::from_secs(5);
    let type_ =
        core::infer(&mut core::Context::new(timeout), &core_term).expect("failed to infer type");

    let ersd_term = core::erase(&mut core::Context::new(timeout), &core_term, &type_)
        .expect("failed to erase term");
    println!();
    println!("=== ersd ===");
    println!("{ersd_term}");

    let cont_module = ersd::to_cont(&ersd_term);
    println!();
    println!("=== cont ===");
    println!("{cont_module}");

    let wasm_module = cont::to_wasm(&cont_module);
    println!();
    println!("=== wasm ===");
    println!("{wasm_module}");

    let (system, receiver) = curios::ChannelProvider::out();
    curios::run_wasm(&wasm_module, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"{\"name\":\"Alice\",\"score\":9.5,\"active\":true,\"tags\":[\"x\",\"y\"],\"extra\":null}".to_vec()]
    );
}
