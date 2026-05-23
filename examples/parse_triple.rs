use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let text_entrypoint = r#"
        let triple : {Int, Int, Int} = (+1, +2, +3);
        split triple : _ => Int; | (a, b, c) =>
        Int.add a (Int.add b c)
        "#
    .parse::<text::Entrypoint>()
    .expect("expected text term");

    println!("=== text ===");
    println!("{text_entrypoint}");

    let core_term = text::to_core(&text_entrypoint, &curios::text::PanicLoader);

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
    curios::run_wasm(&wasm_module, curios::pipe_to_stdout()).unwrap();
}
