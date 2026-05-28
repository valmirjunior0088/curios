use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let text_entrypoint = r#"
        use /sys/{Int};
        let triple : {Int, Int, Int} = (+1, +2, +3);
        Int/add(triple.0, Int/add(triple.1, triple.2))
        "#
    .parse::<text::Entrypoint>()
    .expect("expected text term")
    .with_prelude();

    println!("=== text ===");
    println!("{text_entrypoint}");

    let core_term =
        text::to_core(&text_entrypoint, &curios::text::PanicLoader).expect("expected core term");

    println!();
    println!("=== core ===");
    println!("{core_term}");

    let type_ = core::infer(&mut core::Context::new(Duration::from_secs(5)), &core_term)
        .expect("expected type");

    let ersd_term = core::erase(
        &mut core::Context::new(Duration::from_secs(5)),
        &core_term,
        &type_,
    )
    .expect("expected erased term");

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

    println!();
    println!("=== result ===");
    curios::run_wasm(&wasm_module, curios::StdioProvider).unwrap();
}
