use {
    curios::{Stage, compile_entrypoint},
    std::time::Duration,
};

fn main() {
    let source = r#"
        use /sys/{Nat, Arr};
        let xs : Arr(Nat) = [10, 20, 30];
        let len : Nat = Arr/len(Nat, xs);
        let first : Nat = Arr/get(Nat, xs, 0);
        let rest : Arr(Nat) = Arr/slice(Nat, xs, 1, 3);
        let doubled : Arr(Nat) = Arr/concat(Nat, xs, xs);
        Arr/len(Nat, doubled)
        "#;

    let entrypoint = source
        .parse::<curios::text::Entrypoint>()
        .unwrap()
        .with_type("/sys/Nat".parse().unwrap());

    let wasm_module = compile_entrypoint(
        Duration::from_secs(1),
        &entrypoint,
        &curios::text::NullLoader,
        |stage| match stage {
            Stage::Text(entrypoint) => {
                println!("=== text ===");
                println!("{entrypoint}");
            }
            Stage::Core(term) => {
                println!();
                println!("=== core ===");
                println!("{term}");
            }
            Stage::Ersd(term) => {
                println!();
                println!("=== ersd ===");
                println!("{term}");
            }
            Stage::Cont(module) => {
                println!();
                println!("=== cont ===");
                println!("{module}");
            }
            Stage::Optm(module) => {
                println!();
                println!("=== optm ===");
                println!("{module}");
            }
            Stage::Wasm(module) => {
                println!();
                println!("=== wasm ===");
                println!("{module}");
            }
        },
    )
    .expect("expected wasm module");

    println!();
    println!("=== result ===");
    curios::run_wasm(&wasm_module, curios::StdioHost).unwrap();
}
