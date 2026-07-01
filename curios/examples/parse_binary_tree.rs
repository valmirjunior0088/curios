use {
    curios::{Stage, compile_entrypoint},
    std::time::Duration,
};

fn main() {
    let source = r#"
        use /std/{Int};
        induct Tree : Type
        | leaf(Int)
        | node(Int, Tree, Tree)
        end
        rec sum : (Tree) -> Int = (t) =>
            match t : Int
            | leaf(n) => n
            | node(n, l, r) =>
                Int/add(n, Int/add(sum(l), sum(r)))
            end;
        let tree : Tree =
            Tree/node(+1,
                Tree/node(+2, Tree/leaf(+3), Tree/leaf(+4)),
                Tree/node(+5, Tree/leaf(+6), Tree/leaf(+7)));
        sum(tree)
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
