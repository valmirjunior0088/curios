use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let text_entrypoint = r#"
        rec fib_pair : Nat -> {Int, Int} = n =>
            Nat.fold n : _ => {Int, Int};
            | 0 => (+0, +1);
            | pred ih =>
                split ih : _ => {Int, Int}; | (a, b) =>
                (b, Int.add a b);;
        split fib_pair 10 : _ => Int; | (a, b) =>
        a
        "#
    .parse::<text::Entrypoint>()
    .expect("expected text term");


    let core_term = text::to_core(&text_entrypoint);

    println!("=== core ===");
    println!("{core_term}");

    let ersd_term = core::erase(
        &mut core::Context::new(Duration::from_secs(5)),
        &core_term,
        &text::to_core(&"Int".parse().expect("expected result type")),
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
