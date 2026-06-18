//! Wall-clock scaling of the JSON parser+encoder over runtime input. The guest program is
//! identical across input sizes — only stdin differs — so per-element cost is the *slope* of
//! time vs. size, cancelling the fixed JIT/instantiate overhead. Two programs: decode-only,
//! and decode+re-encode, so the encoder's contribution shows up as the gap between them.

use {
    curios::{MockHost, compile_entrypoint, text},
    std::time::{Duration, Instant},
};

const DECODE: &str = r#"
    use /std/{Json, Io, Bin, Nat, Result};
    let input : Bin =
        match Io/read(Io/stdin, 1048576) : Bin
        | chunk(b) => b
        | eof() => \\
        | error(_) => \\
        end;
    match Json/decode(input, 0) : {}
    | success(pair) => Io/print(Nat/to_str(pair.0))
    | failure(msg) => Io/print(msg)
    end
"#;

const ROUNDTRIP: &str = r#"
    use /std/{Json, Io, Bin, Nat, Result};
    let input : Bin =
        match Io/read(Io/stdin, 1048576) : Bin
        | chunk(b) => b
        | eof() => \\
        | error(_) => \\
        end;
    match Json/decode(input, 0) : {}
    | success(pair) => Io/print(Nat/to_str(Bin/len(Json/encode(pair.1))))
    | failure(msg) => Io/print(msg)
    end
"#;

/// A JSON array of `n` small integers: `[0,0,...,0]`.
fn make_input(n: usize) -> String {
    let mut s = String::from("[");
    for i in 0..n {
        if i > 0 {
            s.push(',');
        }
        s.push('0');
    }
    s.push(']');
    s
}

fn bench(label: &str, src: &str, sizes: &[usize]) {
    let entrypoint = src
        .parse::<text::Entrypoint>()
        .expect("parse")
        .with_type("()".parse().unwrap());
    let module = compile_entrypoint(
        Duration::from_secs(30),
        &entrypoint,
        &text::NullLoader,
        |_| {},
    )
    .expect("compile");

    println!("\n[{label}]");
    println!("{:>8}  {:>8}  {:>12}", "elems", "bytes", "time");
    let mut points: Vec<(f64, f64)> = vec![];
    for &n in sizes {
        let input = make_input(n);
        let bytes = input.len();
        let mut best = Duration::MAX;
        for _ in 0..3 {
            let (host, _io) = MockHost::builder().stdin_lines([input.as_str()]).build();
            let t = Instant::now();
            curios::run_wasm(&module, host).expect("run");
            best = best.min(t.elapsed());
        }
        println!("{n:>8}  {bytes:>8}  {best:>12?}");
        points.push((bytes as f64, best.as_secs_f64() * 1e6));
    }
    // Per-element cost over the smallest non-trivial point — flat ⇒ linear.
    if points.len() >= 2 {
        let base = points[0].1;
        for &(b, us) in &points[1..] {
            println!(
                "  ~{:.3} µs/byte over baseline at {:.0} bytes",
                (us - base) / b,
                b
            );
        }
    }
}

fn main() {
    let sizes = [0usize, 256, 1024, 4096];
    bench("decode only", DECODE, &sizes);
    bench("decode + re-encode", ROUNDTRIP, &sizes);
}
