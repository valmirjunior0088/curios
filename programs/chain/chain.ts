// chain: build a cons list of 10 000 cells once, then transform it K times, every
// round rebuilding each cell from a predecessor that dies with it.
// AssemblyScript -> wasm (its own GC), on wasmtime.
//
// The @assemblyscript/wasi-shim patches the built-in `process`/`console` (no
// imports); top-level statements compile into the WASI `_start`. Compiled with
// cwd at the shim's install dir so its `lib: ./assembly` glob resolves.
//
// Every walk is a loop rather than a recursion: asc's default shadow stack is far
// smaller than 10 000 frames, and the loop is the natural spelling regardless.
const CELLS: i32 = 10000;

class Node {
  v: u64;
  next: Node | null;
  constructor(v: u64, next: Node | null) {
    this.v = v;
    this.next = next;
  }
}

function build(n: i32, x: u64): Node | null {
  let acc: Node | null = null;
  for (let i = 0; i < n; i++) {
    acc = new Node(x, acc);
    x = (75 * x) % 65537;
  }
  return acc;
}

function step(rest: Node | null): Node | null {
  let acc: Node | null = null;
  while (rest != null) {
    const node = <Node>rest;
    acc = new Node((75 * node.v + 13) % 65537, acc);
    rest = node.next;
  }
  return acc;
}

function total(chain: Node | null): u64 {
  let acc: u64 = 0;
  while (chain != null) {
    const node = <Node>chain;
    acc = (acc + node.v) % 1000003;
    chain = node.next;
  }
  return acc;
}

// argv[0] is the module path; the value we passed is the last entry.
const k = i32(parseInt(process.argv[process.argv.length - 1], 10));
let chain = build(CELLS, u64((k + 1) % 65537));
for (let i = 0; i < k; i++) chain = step(chain);
console.log(total(chain).toString());
