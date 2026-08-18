// lcg: iterate a small LCG `x = (75 * x) mod 65537` N times.
// Small constants keep every intermediate within Curios's i31 runtime ints,
// so all languages compute identical values (see README).
const n = parseInt(process.argv[2], 10);
let x = 1;
for (let i = 0; i < n; i++) {
  x = (75 * x) % 65537;
}
console.log(x);
