use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let text_entrypoint = r#"
        let Bln : Type = '[false, true];
        rec is_even : Nat -> Bln = n =>
            Nat.fold n : _ => Bln;
            | 0 => 'true;
            | pred ih => is_odd pred;
        and is_odd : Nat -> Bln = n =>
            Nat.fold n : _ => Bln;
            | 0 => 'false;
            | pred ih => is_even pred;;
        is_even 10
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
            &"'[false, true]".parse().expect("expected result type"),
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
    println!("{}", curios::run_wasm(&wasm_module, |_| {}).unwrap());
}
