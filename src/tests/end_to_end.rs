use {
    crate::{cont, core, ersd, text},
    std::time::Duration,
};

#[test]
fn pipeline_lowers_and_runs_core_term() {
    let text_entrypoint = r#"
        let pair_ty : Type = {
            label : '[left, right],
            match label : _ => Type;
            | 'left => Int;
            | 'right => Flt; };
        let pair : pair_ty = ('left, +42);
        let score : (_ : pair_ty) -> Int = p =>
            match p.0 : _ => Int;
            | 'left => +42;
            | 'right => +7;;
        score pair
        "#
    .parse::<text::Entrypoint>()
    .expect("expected text term");

    println!("=== text ===");
    println!("{text_entrypoint}");

    let core_term = text::to_core(&text_entrypoint, &crate::text::PanicLoader);

    println!();
    println!("=== core ===");
    println!("{core_term}");

    let ersd_term = core::erase(
        &mut core::Context::new(Duration::from_secs(5)),
        &core_term,
        &text::to_core(
            &"Int".parse().expect("expected result type"),
            &crate::text::PanicLoader,
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

    let result = crate::run_wasm(&wasm_module).expect("expected result");

    println!();
    println!("=== result ===");
    println!("{result}");

    assert_eq!(result, "#0 = i31(bits=0x0000002a, value=42)");
}
