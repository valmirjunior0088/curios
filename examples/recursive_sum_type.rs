use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {

    let term = text::elaborate(
        &"
        rec IntList : Type = {
            label : '[nil, cons],
            match label : _ => Type;
            | 'nil => Int;
            | 'cons => {Int, IntList}; };
        rec sum : IntList -> Int = list =>
            split list : _ => Int; | (label, value) =>
            match label : _ => Int;
            | 'nil => 0i;
            | 'cons =>
                split value : _ => Int; | (head, tail) =>
                Int.add head (sum tail);;
        let xs : IntList =
            ('cons, (1i, ('cons, (2i, ('cons, (3i, ('nil, 0i)))))));
        sum xs
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
