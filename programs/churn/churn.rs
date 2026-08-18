// churn: thread a six-field record through N LCG-fed steps, two fields updated
// per step. One source, compiled twice: native (rustc -O) and wasm (wasm32-wasip1).
//
// The imperative spelling mutates two fields of a stack struct in place, so this
// column allocates nothing anywhere — it is the mutation floor the pure
// contestants' record updates are read against.
const P: u64 = 1000003;

struct Churn {
    a: u64,
    b: u64,
    c: u64,
    d: u64,
    e: u64,
    f: u64,
}

fn main() {
    let n: u64 = std::env::args().nth(1).unwrap().parse().unwrap();
    let mut x = (n + 1) % 65537;
    let mut r = Churn { a: 1, b: 2, c: 3, d: 4, e: 5, f: 6 };
    let mut p = 0u32;
    for _ in 0..n {
        x = 75 * x % 65537;
        match p {
            0 => { r.a = (r.c + r.e + x) % P; r.b = (r.d + r.f + x) % P; p = 1; }
            1 => { r.c = (r.e + r.a + x) % P; r.d = (r.f + r.b + x) % P; p = 2; }
            _ => { r.e = (r.a + r.c + x) % P; r.f = (r.b + r.d + x) % P; p = 0; }
        }
    }
    println!("{}", r.a);
}
