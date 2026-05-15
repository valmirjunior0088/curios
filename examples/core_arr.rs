use {
    curios::{cont, core, ersd},
    std::time::Duration,
};

fn main() {
    let term = "
        let xs : Arr Nat = [10n, 20n, 30n];
        let len : Nat = Arr.len xs;
        let first : Nat = Arr.get xs 0n;
        let rest : Arr Nat = Arr.slice xs 1n 3n;
        let doubled : Arr Nat = Arr.concat xs, xs;
        Arr.len doubled
        "
    .parse()
    .expect("expected core term");

    let erased = core::erase(
        &mut core::Context::new(Duration::from_secs(1)),
        &term,
        &"Nat".parse().expect("expected result type"),
    )
    .expect("expected erased term");

    let cont_module = ersd::to_cont(&erased);

    println!("=== cont ===");
    println!("{cont_module}");

    println!();
    println!("=== wasm ===");
    println!("{}", cont::to_wasm(&cont_module));
}
