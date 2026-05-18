use {curios::execute, std::time::Duration};

fn main() {
    let source = "
        let { id : (_ : Type) -> Type = x => x };
        let witness : Type = id Int;
        let pair_ty : Type = {
            label : '[left, right],
            match label : _ => Type;
            | 'left => Int;
            | 'right => Flt; };
        let payload : pair_ty = ('left, Int.mul 20i 2i);
        let decoded : Int =
            split payload : _ => Int; | (label, value) =>
            match label : _ => Int;
            | 'left => Int.add 40i 2i;
            | 'right => 7i;;
        let make : (_ : Int) -> {witness, Flt} = x =>
            (x, Flt.add 0.25 0.5);
        make decoded
        ";

    println!(
        "{}",
        execute(Duration::from_secs(1), source).expect("expected successful execution")
    );
}
