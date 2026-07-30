// binary-trees: build a perfect tree of depth D with unique per-node payloads, then sum them. Distinct payloads keep every node unique so nothing is shared. One source, compiled twice: native (rustc -O) and wasm (wasm32-wasip1).
enum Tree {
    Leaf(u64),
    Node(u64, Box<Tree>, Box<Tree>),
}

fn build(d: u64, v: u64) -> Tree {
    if d == 0 {
        Tree::Leaf(v)
    } else {
        Tree::Node(v, Box::new(build(d - 1, v * 2)), Box::new(build(d - 1, v * 2 + 1)))
    }
}

fn sum(t: &Tree) -> u64 {
    match t {
        Tree::Leaf(v) => *v % 1000003,
        Tree::Node(v, l, r) => (v + sum(l) + sum(r)) % 1000003,
    }
}

fn main() {
    let d: u64 = std::env::args().nth(1).unwrap().parse().unwrap();
    println!("{}", sum(&build(d, 1)));
}
