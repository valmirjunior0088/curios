// spines: N LCG-keyed inserts into a map, then fold the values. One source,
// compiled twice: native (rustc -O) and wasm (wasm32-wasip1).
//
// The obvious Rust map is std's HashMap keyed by the integer itself: open
// addressing in flat storage, no per-insert allocation once grown, no boundary
// encoding. That is the imperative floor the persistent-map contestants are
// read against, and the table compares map algorithms as much as memory
// management — its header says so.
use std::collections::HashMap;

fn main() {
    let n: u64 = std::env::args().nth(1).unwrap().parse().unwrap();
    let mut x = (n + 1) % 65537;
    let mut m: HashMap<u64, u64> = HashMap::new();
    for i in 0..n {
        x = 75 * x % 65537;
        m.insert(x, i % 1000003);
    }
    let mut acc = 0u64;
    for v in m.values() {
        acc = (acc + v) % 1000003;
    }
    println!("{acc}");
}
