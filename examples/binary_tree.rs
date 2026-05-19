use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let term = text::elaborate(
        &"
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
            ('node, (1i,
                ('node, (2i, ('leaf, 3i), ('leaf, 4i))),
                ('node, (5i, ('leaf, 6i), ('leaf, 7i)))));
        sum tree
        "
        .parse()
        .expect("expected core term"),
    );

    let cont_module = ersd::to_cont(
        &core::erase(
            &mut core::Context::new(Duration::from_secs(5)),
            &term,
            &text::elaborate(&"Int".parse().expect("expected result type")),
        )
        .expect("expected erased term"),
    );

    println!("=== cont ===");
    println!("{cont_module}");

    println!();
    println!("=== wasm ===");
    println!("{}", cont::to_wasm(&cont_module));
}
