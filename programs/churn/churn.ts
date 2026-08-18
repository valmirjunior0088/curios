// churn: thread a six-field record through N LCG-fed steps, two fields updated
// per step. AssemblyScript -> wasm (its own GC), on wasmtime.
//
// The imperative spelling mutates class fields in place, so the single Churn
// object is the only allocation in the program.
//
// The @assemblyscript/wasi-shim patches the built-in `process`/`console` (no
// imports); top-level statements compile into the WASI `_start`. Compiled with
// cwd at the shim's install dir so its `lib: ./assembly` glob resolves.
const P: u64 = 1000003;

class Churn {
  a: u64;
  b: u64;
  c: u64;
  d: u64;
  e: u64;
  f: u64;
  constructor() {
    this.a = 1;
    this.b = 2;
    this.c = 3;
    this.d = 4;
    this.e = 5;
    this.f = 6;
  }
}

// argv[0] is the module path; the value we passed is the last entry.
const n = i32(parseInt(process.argv[process.argv.length - 1], 10));
let x: u64 = u64((n + 1) % 65537);
const r = new Churn();
let p = 0;
for (let i = 0; i < n; i++) {
  x = (75 * x) % 65537;
  if (p == 0) {
    r.a = (r.c + r.e + x) % P;
    r.b = (r.d + r.f + x) % P;
    p = 1;
  } else if (p == 1) {
    r.c = (r.e + r.a + x) % P;
    r.d = (r.f + r.b + x) % P;
    p = 2;
  } else {
    r.e = (r.a + r.c + x) % P;
    r.f = (r.b + r.d + x) % P;
    p = 0;
  }
}
console.log(r.a.toString());
