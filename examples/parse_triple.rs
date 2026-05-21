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


    let core_term = text::to_core(&text_entrypoint);

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

    println!();
    println!("=== wasm ===");
    println!("{}", cont::to_wasm(&cont_module));
}
