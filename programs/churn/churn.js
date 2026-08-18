// churn: thread a six-field record through N LCG-fed steps, two fields updated
// per step. The imperative spelling mutates the object in place, so V8 sees one
// long-lived shape and no allocation. Values stay under 2^21 + 2^17, so every
// Number here is exact.
const n = parseInt(process.argv[2], 10);
const P = 1000003;

let x = (n + 1) % 65537;
const r = { a: 1, b: 2, c: 3, d: 4, e: 5, f: 6 };
let p = 0;
for (let i = 0; i < n; i++) {
  x = (75 * x) % 65537;
  if (p === 0) {
    r.a = (r.c + r.e + x) % P;
    r.b = (r.d + r.f + x) % P;
    p = 1;
  } else if (p === 1) {
    r.c = (r.e + r.a + x) % P;
    r.d = (r.f + r.b + x) % P;
    p = 2;
  } else {
    r.e = (r.a + r.c + x) % P;
    r.f = (r.b + r.d + x) % P;
    p = 0;
  }
}
console.log(r.a);
