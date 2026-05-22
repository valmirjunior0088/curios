use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let text_entrypoint = r#"
        rec Tree : Type = {
            label : '[leaf, node],
            match label : _ => Type;
            | 'leaf => Int;
            | 'node => {Int, Tree, Tree}; };
        rec sum : Tree -> Int = t =>
            split t : _ => Int; | (label, value) =>
            match label : _ => Int;
            | 'leaf => value;
            | 'node =>
                split value : _ => Int; | (v, left, right) =>
                Int.add v (Int.add (sum left) (sum right));;
        let tree : Tree =
            ('node, (+1,
                ('node, (+2, ('leaf, +3), ('leaf, +4))),
                ('node, (+5, ('leaf, +6), ('leaf, +7)))));
        sum tree
        "#
    .parse::<text::Entrypoint>()
    .expect("expected text term");

    let core_term = text::to_core(&text_entrypoint);

    println!("=== core ===");
    println!("{core_term}");

    let ersd_term = core::erase(
        &mut core::Context::new(Duration::from_secs(5)),
        &core_term,
        &text::to_core(&"Int".parse().expect("expected result type")),
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
