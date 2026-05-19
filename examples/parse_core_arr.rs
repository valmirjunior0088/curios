use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let text_term = r#"
        let xs : Arr Nat = [10n, 20n, 30n];
        let len : Nat = Arr.len xs;
        let first : Nat = Arr.get xs 0n;
        let rest : Arr Nat = Arr.slice xs 1n 3n;
        let doubled : Arr Nat = Arr.concat xs, xs;
        Arr.len doubled
        "#
    .parse::<text::Term>()
    .expect("expected text term");

    println!("=== text ===");
    println!("{text_term}");

    let core_term = text::elaborate(&text_term);

    println!();
    println!("=== core ===");
    println!("{core_term}");

    let ersd_term = core::erase(
        &mut core::Context::new(Duration::from_secs(1)),
        &core_term,
        &text::elaborate(&"Nat".parse().expect("expected result type")),
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
