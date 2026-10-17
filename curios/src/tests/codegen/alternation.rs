//! Runtime measurements of a list grown an element a step and read in turn: at its first element (`rope_push_first.crs`) or by a window past it (`rope_push_drop.crs`), each of which a flattening reaches, against `rope_push_peek.crs`, which grows it alike and reads only the element it has just appended. Collections are counted from the engine's own records through `curios_runtime::test_support::collected`, as `churn.rs` counts them, and runs are timed interleaved.

use {
    super::{compiled_of, run},
    curios_runtime::{Compiled, test_support::collected},
};

const PUSH_FIRST: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/rope_push_first.crs"
));

const PUSH_DROP: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/rope_push_drop.crs"
));

const PUSH_PEEK: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/rope_push_peek.crs"
));

/// The sizes the slope is read across, each twice the one before; collections are counted at the last.
const SIZES: [u64; 3] = [16_000, 32_000, 64_000];

/// Collections during one run at `n`, from the engine's own records. The counted run's duration is discarded: the raised log level prices every suppressed record, so timing and counting are separate runs.
fn collections(compiled: &Compiled, n: u64) -> usize {
    collected(|| run(compiled, n)).1.collections
}

/// The median of `readings` and their span.
fn median(mut readings: Vec<f64>) -> (f64, f64, f64) {
    readings.sort_by(f64::total_cmp);
    (
        readings[readings.len() / 2],
        readings[0],
        readings[readings.len() - 1],
    )
}

/// The three programs, each anchored at `N = 8` so a mistranslation fails before any figure is read.
fn programs() -> [(&'static str, Compiled); 3] {
    let programs = [
        ("push, then read the first", compiled_of(PUSH_FIRST)),
        ("push, then drop the first", compiled_of(PUSH_DROP)),
        ("push, then peek (control)", compiled_of(PUSH_PEEK)),
    ];
    for ((label, compiled), anchor) in programs.iter().zip([&b"600\n"[..], b"28\n", b"138773\n"]) {
        assert_eq!(run(compiled, 8).1, anchor, "{label} at N = 8");
    }
    programs
}

/// # What this measures, and how to retake it
///
/// ```sh
/// cargo test --package curios --release -- codegen::alternation::rope_alternation_collections --ignored --nocapture
/// ```
///
/// Mixed. Counted: the collections each program's run announces at the largest size, which transport across machines. Timed: the median of five interleaved runs at each size with their span, milliseconds around instantiation and the whole call; the slope between sizes is the reading, and the figures need a quiet machine.
///
/// # What it last printed
///
/// Taken on top of `e3bfb4396` on an x86-64 Linux machine under load, wasmtime 47.0.3, the command above:
///
/// ```text
/// == rope alternation, collections at N = 64000 and times
///   push, then read the first: 248 collections
///   push, then drop the first: 497 collections
///   push, then peek (control): 0 collections
///   N = 16000, push, then read the first: 214.5 ms (179.4–321.2)
///   N = 16000, push, then drop the first: 445.8 ms (359.0–577.2)
///   N = 16000, push, then peek (control): 8.0 ms (5.7–9.6)
///   N = 32000, push, then read the first: 437.2 ms (379.8–886.2)
///   N = 32000, push, then drop the first: 932.0 ms (734.8–1655.5)
///   N = 32000, push, then peek (control): 10.4 ms (5.8–16.8)
///   N = 64000, push, then read the first: 3455.8 ms (2464.7–3578.0)
///   N = 64000, push, then drop the first: 5700.8 ms (4672.2–6902.6)
///   N = 64000, push, then peek (control): 28.8 ms (14.9–45.1)
/// ```
///
/// # The reading
///
/// Each flattening copies its whole run: the first program flattens at every second step and the second at every step, so they collect in proportion to the square of N while the control, which flattens nothing, never collects.
#[test]
#[ignore = "measurement, mixed: counts the alternations' collections and times them rather than asserting"]
fn rope_alternation_collections() {
    let programs = programs();
    let largest = SIZES[SIZES.len() - 1];

    println!("== rope alternation, collections at N = {largest} and times");
    for (label, compiled) in &programs {
        println!("  {label}: {} collections", collections(compiled, largest));
    }
    for n in SIZES {
        let mut readings = vec![Vec::new(); programs.len()];
        for _ in 0..5 {
            for (times, (_, compiled)) in readings.iter_mut().zip(&programs) {
                times.push(run(compiled, n).0);
            }
        }
        for ((label, _), times) in programs.iter().zip(readings) {
            let (middle, low, high) = median(times);
            println!("  N = {n}, {label}: {middle:.1} ms ({low:.1}–{high:.1})");
        }
    }
}
