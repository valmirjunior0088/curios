use {
    curios::{Stage, compile_entrypoint},
    std::time::Duration,
};

fn main() {
    let source = r#"
        use /std/{Nat, Arr, True};
        let xs : Arr(Nat) = [|10, 20, 30|];
        let len : Nat = Arr/len(xs);
        let first : Nat = Arr/at(xs, 0, True/qed());
        let rest : Arr(Nat) = Arr/slice(xs, 1, 3);
        let doubled : Arr(Nat) = Arr/concat(xs, xs);
        Arr/len(doubled)
        "#;

    let entrypoint = source
        .parse::<curios::text::Entrypoint>()
        .unwrap()
        .with_type("/std/Nat".parse().unwrap());

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
            Stage::ErsdOptm(term) => {
                println!();
                println!("=== ersd-optm ===");
                println!("{term}");
            }
            Stage::Cont(module) => {
                println!();
                println!("=== cont ===");
                println!("{module}");
            }
            Stage::ContOptm(module) => {
                println!();
                println!("=== cont-optm ===");
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
    curios::run_wasm(&wasm_module, curios_rt::OsHost::new()).unwrap();
}
