use {
    curios::{Stage, compile},
    std::time::Duration,
};

fn main() {
    let source = r#"
        use /sys/{Nat, Bln};
        rec is_even : Nat -> Bln = n =>
            match n : _ => Bln
            | 0 => true
            | pred ih => is_odd(pred)
            end
        and is_odd : Nat -> Bln = n =>
            match n : _ => Bln
            | 0 => false
            | pred ih => is_even(pred)
            end;
        is_even(10)
        "#;

    let wasm_module = compile(
        Duration::from_secs(5),
        &curios::text::PanicLoader,
        Some("/sys/Bln"),
        source,
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
