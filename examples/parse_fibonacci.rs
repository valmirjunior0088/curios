use {
    curios::{Stage, compile_entrypoint},
    std::time::Duration,
};

fn main() {
    let source = r#"
        use /std/{Nat, Int};
        rec fib_pair : (Nat) -> {Int, Int} = (n) =>
            match n : {Int, Int}
            | 0 => (+0, +1)
            | pred + 1, ih =>
                (ih.1, Int/add(ih.0, ih.1))
            end;
        fib_pair(10).0
        "#;

    let entrypoint = source
        .parse::<curios::text::Entrypoint>()
        .unwrap()
        .with_type("/std/Int".parse().unwrap());

    let wasm_module = compile_entrypoint(
        Duration::from_secs(5),
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
    curios::run_wasm(&wasm_module, curios::OsHost::new()).unwrap();
}
