use {
    curios::{Stage, compile_entrypoint},
    std::time::Duration,
};

fn main() {
    let source = r#"
        use /std/{Int};
        let triple : {Int, Int, Int} = (+1, +2, +3);
        Int/add(triple.0, Int/add(triple.1, triple.2))
        "#;

    let entrypoint = source.parse::<curios::text::Entrypoint>().unwrap();

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
