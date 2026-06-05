use {
    curios::{Stage, compile_entrypoint},
    std::time::Duration,
};

fn main() {
    let source = r#"
        use /sys/{Int};
        rec IntList : Type = {
            label : '[nil, cons],
            match label : _ => Type
            | 'nil => Int
            | 'cons => {Int, IntList}
            end };
        rec sum : IntList -> Int = (list) =>
            match list.0 : _ => Int
            | 'nil => +0
            | 'cons =>
                Int/add(list.1.0, sum(list.1.1))
            end;
        let xs : IntList =
            ('cons, (+1, ('cons, (+2, ('cons, (+3, ('nil, +0)))))));
        sum(xs)
        "#;

    let entrypoint = source
        .parse::<curios::text::Entrypoint>()
        .unwrap()
        .with_type("/sys/Int".parse().unwrap())
        .with_prelude();

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
    curios::run_wasm(&wasm_module, curios::StdioHost).unwrap();
}
