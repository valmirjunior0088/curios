// binary-trees: build a perfect tree of depth D with unique per-node payloads,
// then sum them. Sums stay under 2^53 (exact) for D up to ~24.
const d = parseInt(process.argv[2], 10);

function build(d, v) {
  return d === 0 ? { v } : { v, l: build(d - 1, v * 2), r: build(d - 1, v * 2 + 1) };
}

function sum(t) {
  return t.l === undefined ? t.v % 1000003 : (t.v + sum(t.l) + sum(t.r)) % 1000003;
}

console.log(sum(build(d, 1)));
