use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

// Dispatches on a byte value using three JSON-relevant ASCII cases:
// '"' (34), '[' (91), '{' (123).  With BYTE = 91 ('['), the expected
// result is 'lbracket.  The sparse cases exercise the binary-search
// WASM codegen path end-to-end from surface syntax.
fn main() {
    let text_term = r#"
        Nat.match 91 : _ => '[quote, lbracket, lbrace, other];
        | '"' => 'quote;
        | '[' => 'lbracket;
        | '{' => 'lbrace;
        | _ => 'other;
        "#
    .parse::<text::Term>()
    .expect("expected text term");

    println!("=== text ===");
    println!("{text_term}");

    let core_term = text::elaborate(&text_term);

    println!();
    println!("=== core ===");
    println!("{core_term}");

    let result_type = text::elaborate(
        &"'[quote, lbracket, lbrace, other]"
            .parse()
            .expect("expected result type"),
    );

    let ersd_term = core::erase(
        &mut core::Context::new(Duration::from_secs(5)),
        &core_term,
        &result_type,
    )
    .expect("expected erased term");

    println!();
    println!("=== ersd ===");
    println!("{ersd_term}");

    let cont_module = ersd::to_cont(&ersd_term);

    println!();
    println!("=== cont ===");
    println!("{cont_module}");

    println!();
    println!("=== wasm ===");
    println!("{}", cont::to_wasm(&cont_module));
}
