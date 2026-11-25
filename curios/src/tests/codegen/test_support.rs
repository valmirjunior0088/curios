//! What `codegen`'s runtime measurements share: a program compiled as the product compiles one, a run of it at a size, and the readings a run gives, the runs a measurement compares turned round-robin.

use {
    crate::test_support::*,
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_native::{Compiled, ForeignBindings, test_support::*},
    curios_pipeline::compile_with_prelude,
    curios_surface::Entrypoint,
    std::time::{Duration, Instant},
    xbench::{Measure, read, read_elapsed, turn},
};

/// `source` compiled against the prelude and through Binaryen, as the product compiles a program.
pub fn compiled_of(source: &str) -> Compiled {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())
        .expect("the workload parses");
    let (module, foreigns) =
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})
            .expect("the workload compiles");

    to_compiled(&module, foreigns).expect("the workload compiles")
}

/// One run, its size handed as the program's one argument: how long it took — instantiate, and the whole call — and what the program printed.
pub fn run(compiled: &Compiled, n: u64) -> (Duration, Vec<u8>) {
    let (host, io) = MockHost::builder()
        .args(["program".to_string(), n.to_string()])
        .build();
    let start = Instant::now();
    let code = compiled
        .run(host, ForeignBindings::empty())
        .expect("the workload runs");
    let elapsed = start.elapsed();
    assert_eq!(code, 0, "the workload exits cleanly");

    (elapsed, io.output())
}

/// Read a run of `compiled` at `n` as the figure `<label> at <n>`, its time.
pub fn read_run(label: &str, compiled: &Compiled, n: u64) {
    read_elapsed(&format!("{label} at {n}"), Some(n), run(compiled, n).0);
}

/// Read what the engine's collector did over a run of `compiled` at `n`, a run of its own and untimed, since a count raises the log level and so prices every record the engine writes: `<label> at <n>/collections`, the collections it began, and `<label> at <n>/grown`, the bytes its heap grew by.
pub fn read_collected(label: &str, compiled: &Compiled, n: u64) {
    let (_, collected) = collected(|| run(compiled, n));
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
