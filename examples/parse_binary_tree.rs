use {
    curios::{Stage, compile},
    std::time::Duration,
};

fn main() {
    let source = r#"
        use /sys/{Int};
        rec Tree : Type = {
            label : '[leaf, node],
            match label : _ => Type
            | 'leaf => Int
            | 'node => {Int, Tree, Tree}
            end };
        rec sum : Tree -> Int = (t) =>
            match t.0 : _ => Int
            | 'leaf => t.1
            | 'node =>
                Int/add(t.1.0, Int/add(sum(t.1.1), sum(t.1.2)))
            end;
        let tree : Tree =
            ('node, (+1,
                ('node, (+2, ('leaf, +3), ('leaf, +4))),
                ('node, (+5, ('leaf, +6), ('leaf, +7)))));
        sum(tree)
        "#;

    let wasm_module = compile(
        Duration::from_secs(5),
        &curios::text::PanicLoader,
        Some("/sys/Int"),
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
