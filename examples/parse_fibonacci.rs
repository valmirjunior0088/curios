use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let text_term = r#"
        rec fib_pair : Nat -> {Int, Int} = n =>
            Nat.match n : _ => {Int, Int};
            | 0n => (0i, 1i);
            | pred ih =>
                split ih : _ => {Int, Int}; | (a, b) =>
                (b, Int.add a b);;
        split fib_pair 10n : _ => Int; | (a, b) =>
        a
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
        &mut core::Context::new(Duration::from_secs(5)),
        &core_term,
        &text::elaborate(&"Int".parse().expect("expected result type")),
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
