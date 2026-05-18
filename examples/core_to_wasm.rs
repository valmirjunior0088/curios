use {
    curios::{cont, core, ersd},
    std::time::Duration,
};

fn main() {
    let term = r#"
        let pair_ty : Type = {
            label : '[left, right],
            match label : _ => Type;
            | 'left => Int;
            | 'right => Flt; };
        let pair : pair_ty = ('left, 42i);
        let score : (_ : pair_ty) -> Int = p =>
            split p : _ => Int; | (label, payload) =>
            match label : _ => Int;
            | 'left => 42i;
            | 'right => 7i;;
        let my_list : Arr Nat = [1n, 2n, 3n];
        let my_bin : Bin = \01\02\03;
        let my_str : Bin = "hello";
        let list_len : Nat = Arr.len my_list;
        let bin_len : Nat = Bin.len my_bin;
        let str_len : Nat = Bin.len my_str;
        Int.add (score pair) (Nat.to_int (Nat.add list_len (Nat.add bin_len str_len)))
        "#
    .parse()
    .expect("expected core term");

    let cont_module = ersd::to_cont(
        &core::erase(
            &mut core::Context::new(Duration::from_secs(1)),
            &term,
            &"Int".parse().expect("expected result type"),
        )
        .expect("expected erased term"),
    );

    println!("=== cont ===");
    println!("{cont_module}");

    println!();
    println!("=== wasm ===");
    println!("{}", cont::to_wasm(&cont_module));
}
