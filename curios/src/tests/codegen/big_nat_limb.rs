//! The BigNat small-limb instrument: the marginal cost of one `BigNat/succ` — `raw` ripple plus the `of_bits` trim walk — with the magnitude on each side of the i31 immediate boundary. The map-wall coda put `Bits` ≤26 bits on the i31, and `BigNat` is exactly a `Bits` magnitude, so a small counter should ripple in registers where a boxed one walks a rope; this probe is the figure for that claim, in the `stored_prelude_measurements` pattern — the command, the date, and what it last printed, kept beside the code so the number cannot drift from the thing that would check it.

use {
    crate::to_cwasm,
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_runtime::{ForeignBindings, MockHost, run_bytes},
    curios_text::{Entrypoint, RootSource},
    std::time::Instant,
};

/// The workload: N `succ` steps from a seeded counter. N arrives on stdin so the spin cannot fold; the seed is the source's one variable, substituted for `SEED` — the two arms differ in nothing else, so the slope contrast is the carrier alone.
const SPIN: &str = r#"
use /std/{Str, Nat, BigNat, Option, Io};

rec spin(n: Nat, acc: BigNat) -> BigNat =
    match n: (_) => BigNat
    | 0 => acc
    | k + 1 => spin(k, BigNat/succ(acc))
    end;

let input = /std/read()!;
match input: (_) => Io({})
| some(bytes) =>
    match Str/of_bytes(bytes): (_) => Io({})
    | some(s) =>
        match Nat/of_str(Str/trim(s)): (_) => Io({})
        | some(n) =>
            let result = spin(n, BigNat/of_nat(SEED));
            /std/print(Str/concat(Nat/to_str(BigNat/bit_len(result)), "\n"))
        | none() => /std/print("bad input\n")
        end
    | none() => /std/print("invalid utf-8\n")
    end
| none() => /std/print("no input\n")
end
"#;

fn cwasm_of(source: &str) -> Vec<u8> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())
        .expect("the workload parses");
    let (module, _foreigns) = compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .expect("the workload compiles");

    to_cwasm(&module).expect("the workload precompiles")
}

/// One run: milliseconds around `run_bytes` — deserialize, instantiate, and the whole call — and what the program printed.
fn run(cwasm: &[u8], n: u64) -> (f64, Vec<u8>) {
    let (host, io) = MockHost::builder().stdin_lines([n.to_string()]).build();
    let start = Instant::now();
    let code = run_bytes(cwasm, host, ForeignBindings::empty()).expect("the workload runs");
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    assert_eq!(code, 0, "the workload exits cleanly");

    (elapsed, io.output())
}

/// Best of five: a timing floor is what the slope subtracts.
fn timed(cwasm: &[u8], n: u64) -> f64 {
    (0..5)
        .map(|_| run(cwasm, n).0)
        .fold(f64::INFINITY, f64::min)
}

/// Nanoseconds per `BigNat/succ`, as the slope between two Ns so deserialization, instantiation, and the fixed phases cancel — taken twice, with a 20-bit seed the whole spin keeps under the 26-bit immediate ceiling and a 31-bit seed every step boxes. The widths differ (20-21 against 31 bits), so a per-bit trim charge accounts for at most ~1.5× of any gap; the rest is the carrier.
///
/// # How to read it
///
/// Numbers are only comparable in `--release`:
///
/// ```sh
/// cargo test --release --package curios --lib -- --ignored --nocapture big_nat_small_limb_slope
/// ```
///
/// The `bit_len` anchors (20 and 31 at N=1000) fail the probe before any figure is read if the workload mistranslates.
///
/// # Figures, 2026-08-20
///
/// First taking, on the campaign-close build (`f0be7878`), x86-64 dev box, release: small **4084 ns/succ**, boxed **7278 ns/succ**. What the figure decided: the immediate carrier is worth a real 1.8× on `succ`, but both arms are call-and-trim bound, not carrier bound — `succ` is `of_bits(raw(bits))`, and `of_bits` runs a full O(width) `trim` fold per increment even though `succ.crs` already proves `raw_trimmed`, the lemma that would let it take the `of_trimmed_bits` door and skip the walk. The small-limb payoff this probe was written for is real but capped by that redundant fold; retake after any `succ` restatement.
///
/// # Retaken after the `succ` restatement, 2026-08-20
///
/// Same box, same command, with `succ` through the `of_trimmed_bits` door: small **645 ns/succ** (6.3×), boxed **465 ns/succ** (15.7×). What the retake decided: the trim walk, not the carrier, was the wall — with it gone an increment is the amortized-O(1) ripple and effectively width-independent, and the arms *invert*: the boxed ripple shares its untouched rope tail per cons where the immediate path re-derives its word through a couple of helper calls, so the immediate's remaining margin is per-cons call overhead, not the walk this probe was aimed at. The premise paragraph above describes the first taking's world; this section is why its small-vs-boxed framing no longer measures a trim.
#[test]
#[ignore = "measurement: reports timings rather than asserting"]
fn big_nat_small_limb_slope() {
    println!("== big nat: small-limb succ slope");

    for (label, seed, anchor) in [
        ("small (2^19 seed, immediate)", "524288", "20"),
        ("boxed (2^30 seed, rope)", "1073741824", "31"),
    ] {
        let cwasm = cwasm_of(&SPIN.replace("SEED", seed));

        let (_, output) = run(&cwasm, 1_000);
        assert_eq!(
            output,
            format!("{anchor}\n").into_bytes(),
            "bit_len anchor for {label}"
        );

        let (n1, n2) = (200_000u64, 600_000u64);
        let (t1, t2) = (timed(&cwasm, n1), timed(&cwasm, n2));
        println!(
            "  {label}: N={n1}: {t1:.1} ms, N={n2}: {t2:.1} ms, slope {:.0} ns/succ",
            (t2 - t1) * 1_000_000.0 / (n2 - n1) as f64,
        );
    }
}
