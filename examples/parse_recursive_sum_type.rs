use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let text_entrypoint = r#"
        rec IntList : Type = {
            label : '[nil, cons],
            match label : _ => Type;
            | 'nil => Int;
            | 'cons => {Int, IntList}; };
        rec sum : IntList -> Int = list =>
            split list : _ => Int; | (label, value) =>
            match label : _ => Int;
            | 'nil => +0;
            | 'cons =>
                split value : _ => Int; | (head, tail) =>
                Int.add head (sum tail);;
        let xs : IntList =
            ('cons, (+1, ('cons, (+2, ('cons, (+3, ('nil, +0)))))));
        sum xs
        "#
    .parse::<text::Entrypoint>()
    .expect("expected text term");

    println!("=== text ===");
    println!("{text_entrypoint}");

    let core_term = text::to_core(&text_entrypoint, &curios::text::PanicLoader);

    println!();
    println!("=== core ===");
    println!("{core_term}");

    let ersd_term = core::erase(
        &mut core::Context::new(Duration::from_secs(5)),
        &core_term,
        &text::to_core(
            &"Int".parse().expect("expected result type"),
            &curios::text::PanicLoader,
        ),
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
    println!("{}", curios::run_wasm(&wasm_module).unwrap());
}
