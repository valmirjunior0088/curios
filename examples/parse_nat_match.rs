use {
    curios::{Stage, compile_entrypoint},
    std::time::Duration,
};

// Dispatches on a byte value using three JSON-relevant ASCII cases:
// '"' (34), '[' (91), '{' (123).  With BYTE = 91 ('['), the expected
// result is 'lbracket.  The sparse cases exercise the binary-search
// WASM codegen path end-to-end from surface syntax.
fn main() {
    let source = r#"
        induct Token
        | quote()
        | lbracket()
        | lbrace()
        | other()
        end
        match 91 : Token
        | '"' => Token/quote()
        | '[' => Token/lbracket()
        | '{' => Token/lbrace()
        | _ => Token/other()
        end
        "#;

    let entrypoint = source
        .parse::<curios::text::Entrypoint>()
        .unwrap()
        .with_type("Token".parse().unwrap());

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
    curios::run_wasm(&wasm_module, curios::OsHost::new()).unwrap();
}
