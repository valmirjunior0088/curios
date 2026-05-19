use {
    curios::{cont, core, ersd, text},
    std::time::Duration,
};

fn main() {

    let term = text::elaborate(
        &"
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
        "
        .parse()
        .expect("expected core term"),
    );

    let cont_module = ersd::to_cont(
        &core::erase(
            &mut core::Context::new(Duration::from_secs(5)),
            &term,
            &text::elaborate(
                &"'[false, true]".parse().expect("expected result type"),
            ),
        )
        .expect("expected erased term"),
    );

    println!("=== cont ===");
    println!("{cont_module}");

    println!();
    println!("=== wasm ===");
    println!("{}", cont::to_wasm(&cont_module));
}
