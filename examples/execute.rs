use {curios::execute, std::time::Duration};

fn main() {
    let source = "
        let { id : (_ : Type) -> Type = x => x };
        let witness : Type = id Int;
        let pair_ty : Type =
            (tag : '[left, right],
                match tag with _ => Type;
                | 'left => Int;
                | 'right => Flt;);
        let payload : pair_ty = ('left, Int.mul 20i 2i);
        let decoded : Int =
            let (tag, value) with _ => Int = payload;
            match tag with _ => Int;
            | 'left => Int.add 40i 2i;
            | 'right => 7i;;
        let make : (_ : Int) -> (_ : witness, Flt) = x =>
            (x, Flt.add 0.25 0.5);
        make decoded
        ";

    println!(
        "{}",
        execute(Duration::from_secs(1), source).expect("expected successful execution")
    );
}
