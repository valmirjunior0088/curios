use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let text_entrypoint = r#"
        let pair_ty : Type = {
            label : '[left, right],
            match label : _ => Type;
            | 'left => Int;
            | 'right => Flt; };
        let pair : pair_ty = ('left, +42);
        let score : pair_ty -> Int = p =>
            split p : _ => Int; | (label, value) =>
            match label : _ => Int;
            | 'left => +42;
            | 'right => +7;;
        let my_list : Arr Nat = [1, 2, 3];
        let my_bin : Bin = \01\02\03;
        let my_str : Bin = "hello";
        let list_len : Nat = Arr.len my_list;
        let bin_len : Nat = Bin.len my_bin;
        let str_len : Nat = Bin.len my_str;
        Int.add (score pair) (Nat.to_int (Nat.add list_len (Nat.add bin_len str_len)))
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
        &mut core::Context::new(Duration::from_secs(1)),
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
