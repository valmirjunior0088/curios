use {
    curios::{Stage, compile},
    std::time::Duration,
};

fn main() {
    let source = r#"
        use /sys/{Int, Flt};
        rec id : Type -> Type = x => x;
        let witness : Type = id(Int);
        let pair_ty : Type = {
            label : '[left, right],
            match label : _ => Type
            | 'left => Int
            | 'right => Flt
            end };
        let value : pair_ty = ('left, Int/mul(+20, +2));
        let decoded : Int =
            match value.0 : _ => Int
            | 'left => Int/add(+40, +2)
            | 'right => +7
            end;
        let make : Int -> {witness, Flt} = x =>
            (x, Flt/add(+0.25, +0.5));
        make(decoded)
        "#;

    let wasm_module = compile(
        Duration::from_secs(5),
        &curios::text::PanicLoader,
        None,
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
