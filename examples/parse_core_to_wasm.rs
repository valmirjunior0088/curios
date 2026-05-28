use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let text_entrypoint = r#"
        use /sys/{Nat, Int, Flt, Bin, Arr};
        let pair_ty : Type = {
            label : '[left, right],
            match label : _ => Type
            | 'left => Int
            | 'right => Flt
            end };
        let pair : pair_ty = ('left, +42);
        let score : pair_ty -> Int = p =>
            match p.0 : _ => Int
            | 'left => +42
            | 'right => +7
            end;
        let my_list : Arr(Nat) = [1, 2, 3];
        let my_bin : Bin = \01\02\03;
        let my_str : Bin = "hello";
        let list_len : Nat = Arr/len(Nat, my_list);
        let bin_len : Nat = Bin/len(my_bin);
        let str_len : Nat = Bin/len(my_str);
        Int/add(score(pair), Nat/to_int(Nat/add(list_len, Nat/add(bin_len, str_len))))
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
        &mut core::Context::new(Duration::from_secs(1)),
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
