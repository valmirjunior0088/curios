//! What the runtime measurements share: the readings a run gives, the runs a measurement compares turned round-robin.

use {
    curios::{TALLIES, test_support::*},
    curios_native::{Compiled, test_support::*},
    xbench::{Measure, phase, read, turn},
};

/// Read a run of `compiled` at `n` as a phase, the figures `<label> at <n>`: its time — its host built, the program instantiated, and the whole call — what it allocated, and the instructions it retired.
pub fn read_run(label: &str, compiled: &Compiled, n: u64) {
    phase(TALLIES, || run_at(compiled, n)).read(&format!("{label} at {n}"), Some(n));
}

/// Read what the engine's collector did over a run of `compiled` at `n`, a run of its own and untimed, since a count raises the log level and so prices every record the engine writes: `<label> at <n>/collections`, the collections it began, and `<label> at <n>/grown`, the bytes its heap grew by.
pub fn read_collected(label: &str, compiled: &Compiled, n: u64) {
    let (_, collected) = collected(|| run_at(compiled, n));
    let name = format!("{label} at {n}");

    read(
        &format!("{name}/collections"),
        Measure::Collections,
        Some(n),
        collected.collections as u64,
    );
    read(
        &format!("{name}/grown"),
        Measure::Bytes,
        Some(n),
        collected.grown,
    );
}

/// Read each of `runs`, a label, its program and the size it runs at, as [`read_run`] reads one, the runs turned round-robin `rounds` times after an untimed round.
pub fn turn_runs(rounds: usize, runs: &[(&str, &Compiled, u64)]) {
    let mut reads = runs
        .iter()
        .map(|&(label, compiled, n)| move || read_run(label, compiled, n))
        .collect::<Vec<_>>();
    let mut turned = reads
        .iter_mut()
        .map(|read| read as &mut dyn FnMut())
        .collect::<Vec<_>>();

    turn(rounds, &mut turned);
}
