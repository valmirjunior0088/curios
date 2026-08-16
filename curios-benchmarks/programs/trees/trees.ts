// trees: build a perfect tree of depth D with unique per-node payloads,
// then sum them (mod 1000003). AssemblyScript -> wasm (its own GC), on wasmtime.
// The @assemblyscript/wasi-shim patches the built-in `process`/`console` (no
// imports); top-level statements compile into the WASI `_start`. Compiled with
// cwd at the shim's install dir so its `lib: ./assembly` glob resolves.
class Tree {
  v: u64;
  l: Tree | null;
  r: Tree | null;
  constructor(v: u64, l: Tree | null, r: Tree | null) {
    this.v = v;
    this.l = l;
    this.r = r;
  }
}

function build(d: i32, v: u64): Tree {
  if (d == 0) return new Tree(v, null, null);
  return new Tree(v, build(d - 1, v * 2), build(d - 1, v * 2 + 1));
}

function sum(t: Tree): u64 {
  if (t.l == null) return t.v % 1000003;
  return (t.v + sum(<Tree>t.l) + sum(<Tree>t.r)) % 1000003;
}

// argv[0] is the module path; the value we passed is the last entry.
const d = i32(parseInt(process.argv[process.argv.length - 1], 10));
console.log(sum(build(d, 1)).toString());
