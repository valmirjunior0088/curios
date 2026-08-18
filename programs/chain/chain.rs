// chain: build a cons list of 10 000 cells once, then transform it K times, every
// round rebuilding each cell from a predecessor that dies with it. One source,
// compiled twice: native (rustc -O) and wasm (wasm32-wasip1).
//
// Every walk consumes its input list node by node, so each cell is freed at the
// point its successor is built and nothing is ever dropped recursively. That is
// the natural spelling here, and it also keeps a 10 000-deep drop off the wasm
// shadow stack; it is not a hand-optimization of the measurement.
const CELLS: u64 = 10_000;

struct Node {
    value: u64,
    next: Chain,
}

type Chain = Option<Box<Node>>;

fn build(n: u64, mut x: u64) -> Chain {
    let mut acc: Chain = None;
    for _ in 0..n {
        acc = Some(Box::new(Node { value: x, next: acc }));
        x = (75 * x) % 65537;
    }
    acc
}

fn step(mut rest: Chain) -> Chain {
    let mut acc: Chain = None;
    while let Some(node) = rest {
        let node = *node;
        rest = node.next;
        acc = Some(Box::new(Node { value: (75 * node.value + 13) % 65537, next: acc }));
    }
    acc
}

fn total(mut chain: Chain) -> u64 {
    let mut acc = 0u64;
    while let Some(node) = chain {
        let node = *node;
        chain = node.next;
        acc = (acc + node.value) % 1000003;
    }
    acc
}

fn main() {
    let k: u64 = std::env::args().nth(1).unwrap().parse().unwrap();
    let mut chain = build(CELLS, (k + 1) % 65537);
    for _ in 0..k {
        chain = step(chain);
    }
    println!("{}", total(chain));
}
