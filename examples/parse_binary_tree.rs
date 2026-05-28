use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let text_entrypoint = r#"
        use /sys/{Int};
        rec Tree : Type = {
            label : '[leaf, node],
            match label : _ => Type
            | 'leaf => Int
            | 'node => {Int, Tree, Tree}
            end };
        rec sum : Tree -> Int = t =>
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
        "#
    .parse::<text::Entrypoint>()
    .expect("expected text term")
    .with_prelude();

    println!("=== text ===");
    println!("{text_entrypoint}");

    let core_term =
        text::to_core(&text_entrypoint, &curios::text::PanicLoader).expect("expected core term");

    println!();
    println!("=== core ===");
    println!("{core_term}");

    let ersd_term = core::erase(
        &mut core::Context::new(Duration::from_secs(5)),
        &core_term,
        &text::to_core(
            &"use /sys/{Int}; Int"
                .parse::<text::Entrypoint>()
                .expect("expected result type")
                .with_prelude(),
            &curios::text::PanicLoader,
        )
        .expect("expected result type"),
    )
    .expect("expected erased term");

    println!();
    println!("=== ersd ===");
    println!("{ersd_term}");

    let cont_module = ersd::to_cont(&ersd_term);

    println!();
    println!("=== cont ===");
    println!("{cont_module}");

    let wasm_module = cont::to_wasm(&cont_module);

    println!();
    println!("=== wasm ===");
    println!("{wasm_module}");

    println!();
    println!("=== result ===");
    curios::run_wasm(&wasm_module, curios::StdioProvider).unwrap();
}
