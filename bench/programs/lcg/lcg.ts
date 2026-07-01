// lcg: iterate a small LCG `x = (75 * x) mod 65537` N times.
// Small constants keep every intermediate within Curios's i31 runtime ints,
// so all languages compute identical values (see README).
// AssemblyScript -> wasm, run on wasmtime via @assemblyscript/wasi-shim: the shim
// patches the compiler's built-in `process`/`console` (no imports), and top-level
// statements compile into the WASI `_start`. Compiled with cwd at the shim's
// install dir so its `lib: ./assembly` glob resolves (see entrypoint.sh).
const A: u64 = 75;
const M: u64 = 65537;

// argv[0] is the module path; the value we passed is the last entry.
const n = u32(parseInt(process.argv[process.argv.length - 1], 10));

let x: u64 = 1;
for (let i: u32 = 0; i < n; i++) {
  x = (A * x) % M;
}
console.log(x.toString());
