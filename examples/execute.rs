use {curios::execute, std::time::Duration};

fn main() {
    let source = "
        let { id : (x : Type) -> Type = x => x };
        let witness : Type = id Int;
        let pair_ty : Type =
          (tag : '[left, right],
            match tag with k => Type;
            case 'left => Int;
            case 'right => Flt;);
        let payload : pair_ty = ('left, Int.mul 20i 2i);
        let decoded : Int =
          let (tag, value) with q => Int = payload;
          match tag with k => Int;
          case 'left => Int.add 40i 2i;
          case 'right => 7i;;
        let make : (x : Int) -> (n : witness, Flt) = x => (x, Flt.add 0.25 0.5);
        make decoded
        ";

    println!(
        "{}",
        execute(Duration::from_secs(1), source).expect("expected successful execution")
    );
}
