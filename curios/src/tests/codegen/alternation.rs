//! Runtime measurements behind `curios-emit/README.md`'s decision "A flattening extends the payload its first piece wrote last", and the bound they hold: a list grown an element a step and read in turn: at its first element (`rope_push_first.crs`) or by a window past it (`rope_push_drop.crs`), each of which a flattening reaches, against `rope_push_peek.crs`, which grows it alike and reads only the element it has just appended. Collections are counted from the engine's own records through `curios_native::test_support::collected`, as `churn.rs` counts them, and runs are timed interleaved.

use {
    super::test_support::*,
    curios_native::{Compiled, test_support::*},
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

/// Collections during one run at `n`, from the engine's own records.
fn collections(compiled: &Compiled, n: u64) -> usize {
    collected(|| run(compiled, n)).1.collections
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

/// # What this measures
///
/// `cargo xbench measure rope_alternation_collections` takes it. Each program is read at the largest size for what the collector did over a run ([`read_collected`]), `<program> at 64000/collections` and `/grown`, and at each size for its time, `<program> at <N>`, the runs turned round-robin five times ([`turn_runs`]); the slope between sizes is the reading.
///
/// # What it last printed
///
/// Taken on top of `9c543476a` on an x86-64 Linux machine at a load average under two, wasmtime 47.0.3, the command above:
///
/// ```text
/// == rope alternation, collections at N = 64000 and times
///   push, then read the first: 0 collections
///   push, then drop the first: 1 collections
///   push, then peek (control): 0 collections
///   N = 16000, push, then read the first: 2.8 ms (2.5–2.9)
///   N = 16000, push, then drop the first: 3.8 ms (3.8–3.9)
///   N = 16000, push, then peek (control): 1.6 ms (1.5–1.7)
///   N = 32000, push, then read the first: 5.1 ms (4.9–5.5)
///   N = 32000, push, then drop the first: 7.2 ms (7.1–7.8)
///   N = 32000, push, then peek (control): 2.9 ms (2.8–3.2)
///   N = 64000, push, then read the first: 9.9 ms (9.5–10.3)
///   N = 64000, push, then drop the first: 14.5 ms (14.3–14.7)
///   N = 64000, push, then peek (control): 5.5 ms (5.3–5.7)
/// ```
///
/// # The reading
///
/// The first program flattens at every second step and the second at every step, and each flattening writes into the room its first piece's payload holds, so each element is copied a bounded number of times: both collect about as rarely as the control, which flattens nothing, and their time doubles with N as its does. A flattening that copied its whole run made them collect hundreds of times at this size, in proportion to the square of N.
#[test]
#[ignore = "measurement: taken by cargo xbench measure"]
fn rope_alternation_collections() {
    let programs = programs();
    let largest = SIZES[SIZES.len() - 1];

    for (label, compiled) in &programs {
        read_collected(label, compiled, largest);
    }
    let runs = programs
        .iter()
        .flat_map(|(label, compiled)| SIZES.map(|n| (*label, compiled, n)))
        .collect::<Vec<_>>();
    turn_runs(5, &runs);
}

/// An alternation of appends with reads of what a flattening reaches copies each element a bounded number of times, so at the largest size it collects about as rarely as its control, which flattens nothing — where one whose flattenings copied the whole run collected hundreds of times. Collection counts are deterministic, which is what makes the bound assertable.
#[test]
fn an_alternation_collects_about_as_rarely_as_its_control() {
    let [first, past, control] = programs();
    let largest = SIZES[SIZES.len() - 1];
    let floor = collections(&control.1, largest);

    for (label, compiled) in [first, past] {
        let count = collections(&compiled, largest);
        assert!(
            count <= floor + 4,
            "{label}: {count} collections at N = {largest}, its control {floor}"
        );
    }
}
