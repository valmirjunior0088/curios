// chain: build a cons list of 10 000 cells once, then transform it K times, every
// round rebuilding each cell from a predecessor that dies with it. Values stay
// under 2^17 and the sum under 2^20, so every Number here is exact.
const CELLS = 10000;
const k = parseInt(process.argv[2], 10);

function build(n, x) {
  let acc = null;
  for (let i = 0; i < n; i++) {
    acc = { v: x, next: acc };
    x = (75 * x) % 65537;
  }
  return acc;
}

function step(rest) {
  let acc = null;
  while (rest !== null) {
    acc = { v: (75 * rest.v + 13) % 65537, next: acc };
    rest = rest.next;
  }
  return acc;
}

function total(chain) {
  let acc = 0;
  while (chain !== null) {
    acc = (acc + chain.v) % 1000003;
    chain = chain.next;
  }
  return acc;
}

let chain = build(CELLS, (k + 1) % 65537);
for (let i = 0; i < k; i++) chain = step(chain);
console.log(total(chain));
