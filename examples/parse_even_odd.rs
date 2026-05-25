use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let text_entrypoint = r#"
        rec is_even : Nat -> Bln = n =>
            match n : _ => Bln;
            | 0 => true;
            | pred ih => is_odd(pred);
        and is_odd : Nat -> Bln = n =>
            match n : _ => Bln;
            | 0 => false;
            | pred ih => is_even(pred);;
        is_even(10)
        "#
    .parse::<text::Entrypoint>()
    .expect("expected text term");

    println!("=== text ===");
    println!("{text_entrypoint}");

    let core_term =
        text::to_core(&text_entrypoint, &curios::text::PanicLoader).expect("expected core term");

    println!();
    println!("=== core ===");
    println!("{core_term}");

    let ersd_term = core::erase(
        &mut core::Context::new(Duration::from_secs(5)),
        &core_term,
        &text::to_core(
            &"Bln".parse().expect("expected result type"),
            &curios::text::PanicLoader,
        )
        .expect("expected result type"),
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
