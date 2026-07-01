// lcg: iterate a small LCG `x = (75 * x) mod 65537` N times.
// Small constants keep every intermediate within Curios's i31 runtime integers,
// so all languages compute identical values (see README). Input N from argv.
// One source, compiled twice: native (rustc -O) and wasm (wasm32-wasip1).
fn main() {
    let n: u64 = std::env::args().nth(1).unwrap().parse().unwrap();
    let mut x: u64 = 1;
    for _ in 0..n {
        x = (75 * x) % 65537;
    }
    println!("{}", x);
}
