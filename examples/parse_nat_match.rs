use {
    curios::{Stage, compile},
    std::time::Duration,
};

// Dispatches on a byte value using three JSON-relevant ASCII cases:
// '"' (34), '[' (91), '{' (123).  With BYTE = 91 ('['), the expected
// result is 'lbracket.  The sparse cases exercise the binary-search
// WASM codegen path end-to-end from surface syntax.
fn main() {
    let source = r#"
        match 91 : _ => '[quote, lbracket, lbrace, other]
        | '"' => 'quote
        | '[' => 'lbracket
        | '{' => 'lbrace
        | _ => 'other
        end
        "#;

    let wasm_module = compile(
        Duration::from_secs(5),
        &curios::text::PanicLoader,
        Some("'[quote, lbracket, lbrace, other]"),
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
