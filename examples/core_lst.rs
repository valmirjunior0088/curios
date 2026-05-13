use {
    curios::{cont, core, ersd},
    std::time::Duration,
};

fn main() {
    let term = "
        let xs : Lst Nat = [10n, 20n, 30n];
        let len : Nat = Lst.len xs;
        let first : Nat = Lst.get 0n xs;
        let rest : Lst Nat = Lst.slice 1n 3n xs;
        let doubled : Lst Nat = Lst.concat xs xs;
        Lst.len doubled
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
