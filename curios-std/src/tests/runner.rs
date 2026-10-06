//! The corpus runner: one unit mounted from its header and its directory, compiled through the whole pipeline and run under the mock host.

use {
    crate::std_directory,
    curios::to_cwasm,
    curios_pipeline::{DEFAULT_STEP_BUDGET, EntryTail, Fold},
    curios_runtime::{ForeignBindings, MockHost, run_bytes},
    curios_text::{Entrypoint, RootSource},
    curios_utilities::RootKind,
    std::path::PathBuf,
};

/// Where the corpus units live: beside this file, inside the library's own directory, under no name a header of `/std` declares.
pub(super) fn root() -> PathBuf {
    std_directory().join("tests")
}

/// Compile `unit` as its own test program — the synthesized `Test/main` tail over its registered tests, with an empty entry above it — and run every test it declares.
///
/// Failures are collected rather than raised at the first. Each test runs in an instantiation of its own, so one failing says nothing about the rest, and a run that stopped early would hide every test after it — which is the granularity a per-fixture Rust test used to give for free.
pub(super) fn run_unit(unit: &str) {
    let root = root();
    let mounted = RootSource::mounted(
        unit,
        RootKind::Ordinary,
        root.join(format!("{unit}.crs")),
        root.join(unit),
    );
    let entrypoint = Entrypoint::trivial();
    let loader = RootSource::none();

    // No cache: a test must not file payloads into a project store.
    let (module, _foreigns, records) = Fold::new(DEFAULT_STEP_BUDGET, &[mounted], None)
        .tests(
            &entrypoint,
            &loader,
            EntryTail::LastUnitTests,
            |_| {},
            |_| {},
        )
        .unwrap_or_else(|error| panic!("`{unit}` failed to compile:\n{error}"));
    let cwasm = to_cwasm(&module).expect("the test program precompiles");

    assert!(!records.is_empty(), "`{unit}` declares no tests");

    let mut passed = 0usize;
    let mut failures = String::new();
    for (index, record) in records.iter().enumerate() {
        let (system, io) = MockHost::builder()
            .args([b"corpus".as_slice(), index.to_string().as_bytes()])
            .build();
        // SAFETY: `cwasm` was precompiled in this process, immediately above.
        let outcome = unsafe { run_bytes(&cwasm, system, ForeignBindings::empty()) };
        let reported = String::from_utf8_lossy(&io.output()).into_owned();

        match outcome {
            // The guest printed `path: passed` and returned.
            Ok(0) => passed += 1,
            // The guest printed its own outcome line and its report; the body as written is what only the records know, appended beneath it.
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

    eprintln!("{unit}: {passed} passed");

    assert!(failures.is_empty(), "\n{failures}");
}
