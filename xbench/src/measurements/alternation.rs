//! What the collector does as a rope alternates pushes with reads at its front, and the slope of a run's time across sizes.

use {super::*, curios::test_support::*};

/// # What this measures
///
/// `cargo xbench measure rope_alternation_collections` takes it. Each program is read at the largest size for what the collector did over a run ([`read_collected`]), `<program> at 64000/collections` and `/grown`, and at each size for its time, `<program> at <N>`, the runs turned round-robin five times ([`turn_runs`]); the slope between sizes is the reading.
///
/// # Its records
///
/// `cargo xbench report rope_alternation_collections` reads them: each figure's readings by group, and what each sitting was taken under.
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
