// spines: N LCG-keyed inserts into a map, then fold the values. AssemblyScript's
// built-in Map is a mutable hash map keyed by the integer itself — no boundary
// encoding, no per-insert structure rebuild.
//
// The @assemblyscript/wasi-shim patches the built-in `process`/`console` (no
// imports); top-level statements compile into the WASI `_start`. Compiled with
// cwd at the shim's install dir so its `lib: ./assembly` glob resolves.
const P: u64 = 1000003;

// argv[0] is the module path; the value we passed is the last entry.
const n = i32(parseInt(process.argv[process.argv.length - 1], 10));
let x: u64 = u64((n + 1) % 65537);
const m = new Map<u64, u64>();
for (let i = 0; i < n; i++) {
  x = (75 * x) % 65537;
  m.set(x, u64(i) % P);
}
let acc: u64 = 0;
const vals = m.values();
for (let j = 0; j < vals.length; j++) {
  acc = (acc + vals[j]) % P;
}
console.log(acc.toString());
