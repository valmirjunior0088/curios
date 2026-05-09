use {
    curios::{cont, core},
    std::time::Duration,
};

fn main() {
    let term = "
        let pair_ty : Type =
          (tag : '{left, right},
            match tag with k => Type;
            case 'left => Int;
            case 'right => Flt;);
        let pair : pair_ty = ('left, 42);
        let score : (p : pair_ty) -> Int = p =>
            let (tag, payload) with q => Int = p;
            match tag with k => Int;
            case 'left => 42;
            case 'right => 7;;
        score pair
        "
    .parse()
    .expect("expected core term");

    println!(
        "{}",
        cont::to_wasm(&core::to_cont(
            &core::erase(
                &mut core::Context::new(Duration::from_secs(1)),
                &term,
                &"Int".parse().expect("expected result type")
            )
            .expect("expected erased term"),
        ))
    );
}
