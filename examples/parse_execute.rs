use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let text_entrypoint = r#"
        rec id : Type -> Type = x => x;
        let witness : Type = id Int;
        let pair_ty : Type = {
            label : '[left, right],
            match label : _ => Type;
            | 'left => Int;
            | 'right => Flt; };
        let value : pair_ty = ('left, Int.mul +20 +2);
        let decoded : Int =
            split value : _ => Int; | (label, value) =>
            match label : _ => Int;
            | 'left => Int.add +40 +2;
            | 'right => +7;;
        let make : Int -> {witness, Flt} = x =>
            (x, Flt.add +0.25 +0.5);
        make decoded
        "#
    .parse::<text::Entrypoint>()
    .expect("expected text term");

    println!("=== text ===");
    println!("{text_entrypoint}");

    let core_term = text::to_core(&text_entrypoint, &curios::text::PanicLoader);

    println!();
    println!("=== core ===");
    println!("{core_term}");

    let type_ = core::infer(&mut core::Context::new(Duration::from_secs(5)), &core_term)
        .expect("expected type");

    let ersd_term = core::erase(
        &mut core::Context::new(Duration::from_secs(5)),
        &core_term,
        &type_,
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
    println!("{}", curios::run_wasm(&wasm_module, |_| {}).unwrap());
}
