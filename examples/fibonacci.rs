use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let term = text::elaborate(
        &"
        rec fib_pair : Nat -> {Int, Int} = n =>
            Nat.match n : _ => {Int, Int};
            | 0n => (0i, 1i);
            | pred ih =>
                split ih : _ => {Int, Int}; | (a, b) =>
                (b, Int.add a b);;
        split fib_pair 10n : _ => Int; | (a, b) =>
        a
        "
        .parse()
        .expect("expected core term"),
    );

    let cont_module = ersd::to_cont(
        &core::erase(
            &mut core::Context::new(Duration::from_secs(5)),
            &term,
            &text::elaborate(&"Int".parse().expect("expected result type")),
        )
        .expect("expected erased term"),
    );

    println!("=== cont ===");
    println!("{cont_module}");

    println!();
    println!("=== wasm ===");
    println!("{}", cont::to_wasm(&cont_module));
}
