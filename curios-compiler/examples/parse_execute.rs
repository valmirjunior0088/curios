use {
    curios::{Stage, compile_entrypoint},
    std::time::Duration,
};

fn main() {
    let source = r#"
        use /std/{Int, Flt};
        rec id : (Type) -> Type = (x) => x;
        let witness : Type = id(Int);
        induct Pair : Type
        | left(Int)
        | right(Flt)
        end
        let value : Pair = Pair/left(Int/mul(+20, +2));
        let decoded : Int =
            match value : Int
            | left(_) => Int/add(+40, +2)
            | right(_) => +7
            end;
        let make : (Int) -> {witness, Flt} = (x) =>
            (x, Flt/add(+0.25, +0.5));
        make(decoded)
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
    curios_compiler::run_wasm(&wasm_module, curios_runtime::OsHost::new()).unwrap();
}
