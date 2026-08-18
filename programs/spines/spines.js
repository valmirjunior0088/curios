// spines: N LCG-keyed inserts into a map, then fold the values. The obvious
// JavaScript map is the built-in Map keyed by the number itself — a mutable
// hash map, no boundary encoding. Values stay under 2^20 and keys under 2^17,
// so every Number here is exact.
const n = parseInt(process.argv[2], 10);
const P = 1000003;

let x = (n + 1) % 65537;
const m = new Map();
for (let i = 0; i < n; i++) {
  x = (75 * x) % 65537;
  m.set(x, i % P);
}
let acc = 0;
for (const v of m.values()) acc = (acc + v) % P;
console.log(acc);
