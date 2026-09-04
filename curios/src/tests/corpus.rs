//! The Curios corpus: each `curios/src/tests/corpus/<unit>.crs` mounted as a unit, compiled once as its own test program, and every test it declares run in an instantiation of its own.
//!
//! What this replaces is one full compile per library fixture. A unit's tests share one `compile_tests_with_units` and one [`to_cwasm`], so the prelude-linked baseline each fixture used to pay alone is paid once for the whole unit, and a run of the precompiled module is milliseconds. The units stay separate for two reasons that pull the same way: cargo runs them in parallel, and a compile error costs one unit's results rather than the corpus entire.
//!
//! Nothing here reaches `curios-package`. A unit is mounted from a header and a directory directly, so the corpus needs no manifest and is not a project — `wonder` and `curios test` do not reach these files, and `cargo test` is the channel, exactly as it is for the `/std` sources they exercise.

use {
    crate::to_cwasm,
    curios_pipeline::{DEFAULT_STEP_BUDGET, EntryTail, compile_tests_with_units},
    curios_runtime::{ForeignBindings, MockHost, run_bytes},
    curios_text::{Entrypoint, RootSource},
    curios_utilities::RootKind,
    std::{
        fs,
        path::{Path, PathBuf},
    },
};

/// Where the corpus units live, beside the driver that mounts them. The `.crs` tree sits under `corpus/` next to `corpus.rs`, which declares no `mod` of its own, so nothing here is reached by Rust's module resolution.
fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("src")
        .join("tests")
        .join("corpus")
}

/// Compile `unit` as its own test program — the synthesized `Test/main` tail over its registered tests, with an empty entry above it — and run every test it declares.
///
/// Failures are collected rather than raised at the first. Each test runs in an instantiation of its own, so one failing says nothing about the rest, and a run that stopped early would hide every test after it — which is the granularity a per-fixture Rust test used to give for free.
fn run_unit(unit: &str) {
    let root = root();
    let mounted = RootSource::mounted(
        unit,
        RootKind::Ordinary,
        root.join(format!("{unit}.crs")),
        root.join(unit),
    );
    let (entrypoint, loader, _source) =
        Entrypoint::supplied("corpus", "()").expect("the empty entry parses");

    // No cache: a test must not file payloads into a project store.
    let (module, _foreigns, records) = compile_tests_with_units(
        DEFAULT_STEP_BUDGET,
        &[mounted],
        &entrypoint,
        &loader,
        None,
        EntryTail::LastUnitTests,
        |_| {},
        |_| {},
    )
    .unwrap_or_else(|error| panic!("`{unit}` failed to compile:\n{error}"));
    let cwasm = to_cwasm(&module).expect("the test program precompiles");

    assert!(!records.is_empty(), "`{unit}` declares no tests");

    let mut failures = String::new();
    for (index, record) in records.iter().enumerate() {
        let (system, io) = MockHost::builder()
            .args([b"corpus".as_slice(), index.to_string().as_bytes()])
            .build();
        // SAFETY: `cwasm` was precompiled in this process, immediately above.
        let outcome = unsafe { run_bytes(&cwasm, system, ForeignBindings::empty()) };
        let reported = String::from_utf8_lossy(&io.output()).into_owned();

        match outcome {
            // The guest printed `path: proved` or `path: passed` and returned.
            Ok(0) => {}
            // The guest printed its own outcome line and its report. The body as written is appended when the record carries one; a mounted unit's spans do not survive the fold, so for this corpus it is empty and the path above is what names the failure.
            Ok(_) => {
                failures.push_str(&reported);
                if !record.body.is_empty() {
                    failures.push_str(&format!("{}\n", record.body));
                }
            }
            // A trap or a stray exit never reaches the guest's printing, so the line is written here.
            Err(error) => {
                failures.push_str(&format!("{reported}{}: {error}\n", record.path));
            }
        }
    }

    assert!(failures.is_empty(), "\n{failures}");
}

/// One `#[test]` per corpus unit, and the roster [`every_corpus_unit_is_mounted`] checks the tree against, declared from one list so the two cannot disagree.
macro_rules! corpus {
    ($($unit:ident),* $(,)?) => {
        $(
            #[test]
            fn $unit() {
                run_unit(stringify!($unit));
            }
        )*

        const MOUNTED: &[&str] = &[$(stringify!($unit)),*];
    };
}

corpus! { strings, data, aggregates }

/// A unit header with no row in `corpus!` would be compiled by nothing and silently pass, which is the one failure mode this arrangement has that a per-fixture Rust test does not.
#[test]
fn every_corpus_unit_is_mounted() {
    let mut headers = fs::read_dir(root())
        .expect("the corpus directory is readable")
        .map(|entry| entry.expect("the entry is readable").path())
        .filter(|path| path.extension().is_some_and(|extension| extension == "crs"))
        .map(|path| {
            path.file_stem()
                .expect("a `.crs` file has a stem")
                .to_string_lossy()
                .into_owned()
        })
        .collect::<Vec<_>>();
    headers.sort();

    let mut mounted = MOUNTED
        .iter()
        .map(|unit| (*unit).to_owned())
        .collect::<Vec<_>>();
    mounted.sort();

    assert_eq!(headers, mounted);
}
