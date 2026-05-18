use {curios::execute, std::time::Duration};

fn main() {
    let source = "
        let triple : {Int, Int, Int} = (1i, 2i, 3i);
        split triple : _ => Int; | (a, b, c) =>
        Int.add a (Int.add b c)
        ";

    println!(
        "{}",
        execute(Duration::from_secs(1), source).expect("expected successful execution")
    );
}
