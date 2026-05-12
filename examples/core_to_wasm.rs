use {
    curios::{cont, core, ersd},
    std::time::Duration,
};

fn main() {
    let term = "
        let pair_ty : Type =
          (tag : '[left, right],
            match tag with _ => Type;
            case 'left => Int;
            case 'right => Flt;);
        let pair : pair_ty = ('left, 42i);
        let score : (_ : pair_ty) -> Int = p =>
          let (tag, payload) with _ => Int = p;
          match tag with _ => Int;
          case 'left => 42i;
          case 'right => 7i;;
        score pair
        "
    .parse()
    .expect("expected core term");

    println!(
        "{}",
        cont::to_wasm(&ersd::to_cont(
            &core::erase(
                &mut core::Context::new(Duration::from_secs(1)),
                &term,
                &"Int".parse().expect("expected result type")
            )
            .expect("expected erased term"),
        ))
    );
}
