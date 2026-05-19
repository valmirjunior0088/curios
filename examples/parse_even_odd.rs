use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {
    let text_term = r#"
        let Bln : Type = '[false, true];
        rec is_even : Nat -> Bln = n =>
            Nat.match n : _ => Bln;
            | 0n => 'true;
            | pred ih => is_odd pred;
        and is_odd : Nat -> Bln = n =>
            Nat.match n : _ => Bln;
            | 0n => 'false;
            | pred ih => is_even pred;;
        is_even 10n
        "#
    .parse::<text::Term>()
    .expect("expected text term");

    println!("=== text ===");
    println!("{text_term}");

    let core_term = text::elaborate(&text_term);

    println!();
    println!("=== core ===");
    println!("{core_term}");

    let ersd_term = core::erase(
        &mut core::Context::new(Duration::from_secs(5)),
        &core_term,
        &text::elaborate(&"'[false, true]".parse().expect("expected result type")),
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
