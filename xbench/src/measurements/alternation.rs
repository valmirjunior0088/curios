//! What the collector does as a rope alternates pushes with reads at its front, and the slope of a run's time across sizes.

use {super::*, curios::test_support::*};

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
pub fn rope_alternation_collections() {
    let programs = alternation_programs();
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
