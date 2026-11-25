//! What the corpus costs as Curios emits it, before Binaryen: each program's module, and a run of each workload as a phase.

use {
    crate::{Measure, WORKLOADS, phase, read},
    curios::TALLIES,
    curios_abi::ForeignStore,
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_native::{Compiled, ForeignBindings, test_support::*},
    curios_pipeline::compile_with_prelude,
    curios_surface::Entrypoint,
    curios_wasm::{Module, Names, to_bytes},
    std::{fs, path::Path},
};

/// Where the corpus is kept.
const PROGRAMS: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../programs");

/// The programs the corpus is run at beside the contest's workloads, each at the size it is run at: three string walks, a float loop, a sort, and two round trips, each building a document of about 100 KB and decoding it.
const RUN: [(&str, u64); 7] = [
    ("parse_digits", 1_000_000),
    ("parse_bindless", 1_000_000),
    ("parse_multibyte", 300_000),
    ("flt_hot_loop", 3_000_000),
    ("sort_scattered", 300_000),
    ("toml_round_trip", 800),
    ("json_round_trip", 820),
];

/// How many runs a program is read at at its size: what a run retires moves with what it draws.
const RUNS: usize = 3;

/// Every program of `programs/`, by its name, with its source: each `.crs` file, and each workload's directory by the Curios program it holds, `<name>/<name>.crs`.
fn corpus() -> Vec<(String, String)> {
    let mut programs = fs::read_dir(PROGRAMS)
        .expect("the corpus's directory")
        .map(|entry| entry.expect("a corpus entry").path())
        .filter_map(|path| {
            let name = path.file_stem()?.to_str()?.to_string();
            let source = match path.is_dir() {
                true => path.join(format!("{name}.crs")),
                false if path.extension()? == "crs" => path,
                false => return None,
            };

            Some((name, fs::read_to_string(source).expect("a corpus program")))
        })
        .collect::<Vec<_>>();
    programs.sort();

    programs
}

/// `source` compiled against the prelude: the module Curios emits, and what answers its foreign rows.
fn emitted(source: &str) -> (Module, ForeignStore) {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())
        .expect("a corpus program parses");

    compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})
        .expect("a corpus program compiles")
}

/// What the corpus costs as Curios emits it: what a copy adds to a module is read here, as [A copy is planned within its allowance](../../../curios-cps/README.md#a-copy-is-planned-within-its-allowance) reads it, Binaryen skipped, since what Binaryen makes of a module is no measurement of the pipeline.
///
/// `cargo xbench measure corpus_measurements` takes it: `<program>/emitted bytes`, the bytes of each program's module as the product writes one before Binaryen, its default names in it; and `<program> at <size>`, a run as a phase — its time, what it allocated, its looks and the instructions it retired, the engine's thread among them — its module handed to Cranelift as Curios emits it, three runs at its size: the contest's workloads at the sizes it takes them at, each held to its anchor, and the programs [`RUN`] lists, each run held to the first run's answer. `/std`'s test program is left out (decided by the user on 2026-11-25).
pub fn corpus_measurements() {
    for (name, source) in corpus() {
        let (module, _) = emitted(&source);
        read(
            &format!("{name}/emitted bytes"),
            Measure::Count,
            None,
            to_bytes(&module, &Names::default()).len() as u64,
        );
    }

    let workloads = WORKLOADS
        .iter()
        .map(|workload| (workload.name, workload.size, Some(workload.anchor)));
    let programs = RUN.into_iter().map(|(name, size)| (name, size, None));
    start_test_engine();
    for (name, size, anchor) in workloads.chain(programs) {
        let source = match anchor {
            Some(_) => Path::new(PROGRAMS).join(name).join(format!("{name}.crs")),
            None => Path::new(PROGRAMS).join(format!("{name}.crs")),
        };
        let (module, foreigns) = emitted(&fs::read_to_string(source).expect("a corpus program"));
        let compiled = Compiled::of(
            curios_cranelift::compile(&to_bytes(&module, &Names::default()))
                .expect("an emitted module compiles to machine code"),
            foreigns,
        );

        let mut answered = anchor.map(str::to_string);
        for _ in 0..RUNS {
            let (host, io) = MockHost::builder()
                .args(["program".to_string(), size.to_string()])
                .build();
            let ran = phase(TALLIES, || {
                compiled
                    .run(host, ForeignBindings::empty())
                    .expect("a corpus program runs")
            });
            assert_eq!(ran.answer, 0, "{name} exits cleanly");

            let printed = String::from_utf8_lossy(&io.output()).trim().to_string();
            let answer = answered.get_or_insert_with(|| printed.clone());
            assert_eq!(*answer, printed, "{name} at {size} answers as it did");
            ran.read(&format!("{name} at {size}"), Some(size));
        }
    }
}
