// walk: fold a mixed-width UTF-8 text N times into a rolling hash, each fold seeded by the last.
// Each fold starts from the accumulator the fold before it produced, so no implementation may hoist the walk out of the loop and decode the text once. Small constants keep every intermediate within Curios's i31 runtime integers, so Curios and Rust compute identical values (see README). Input N from argv.
// One source, compiled twice: native (rustc -O) and wasm (wasm32-wasip2).
fn main() {
    let n: u64 = std::env::args().nth(1).unwrap().parse().unwrap();
    let text = format!("{}{}", n, "é→𝄞".repeat(16));
    let mut acc: u64 = 0;
    for _ in 0..n {
        acc = text.chars().fold(acc, |a, c| (a * 31 + c as u64) % 1000003);
    }
    println!("{}", acc);
}
