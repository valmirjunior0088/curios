//! What `--profile` asks of an invocation: the stream it files, which `curios profile` folds, and the map a sampling profiler names a hosted program's functions by, written only where it is asked; and the prelude's build under a recorder, a measurement `cargo xtask profile prelude` runs.

use {
    curios::CABINET,
    curios_common::test_support::*,
    curios_prelude_build::compile,
    curios_profile::{CountingAllocator, Destination, fold_at, trace},
    std::{
        fs,
        path::{Path, PathBuf},
        process::{Command, Output, Stdio},
    },
};

// A program installs the counting allocator, and this binary's measurement reads what it counts.
#[global_allocator]
static ALLOCATOR: CountingAllocator = CountingAllocator;

/// A program a run hosts, which prints one line.
const PROGRAM: &str = "/std/print(\"profiled\\n\")\n";

/// `curios run` over [`PROGRAM`] in `root`, with `before` written ahead of the command: what it answered, and where the engine's agent files its process's map.
fn run(root: &Path, before: &[&str]) -> (Output, PathBuf) {
    fs::create_dir_all(root).unwrap();
    let program = root.join("program.crs");
    fs::write(&program, PROGRAM).unwrap();
    let child = Command::new(env!("CARGO_BIN_EXE_curios"))
        .env_remove(CABINET)
        .current_dir(root)
        .args(before)
        .arg("run")
        .arg(&program)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    // The one place the agent files a map, which offers no other: the process's id names it.
    let map = PathBuf::from(format!("/tmp/perf-{}.map", child.id()));

    (child.wait_with_output().unwrap(), map)
}

// A run under `--profile` files its stream where the argument says, and the compiler that filed it folds it: every build carries the recorder.
#[test]
fn a_profiled_run_files_a_stream_its_compiler_folds() {
    let root = Temporary::new("profile", "stream");
    let stream = root.join("stream.pftrace");
    let (output, map) = run(&root, &["--profile", stream.to_str().unwrap()]);
    let _ = fs::remove_file(&map);
    assert!(output.status.success(), "{output:?}");

    let folded = Command::new(env!("CARGO_BIN_EXE_curios"))
        .arg("profile")
        .arg(&stream)
        .output()
        .unwrap();
    assert!(folded.status.success(), "{folded:?}");
    let summary = String::from_utf8(folded.stdout).unwrap();
    assert!(summary.contains("compile_entrypoint"), "{summary}");
}

// The map the engine writes under `--profile` names the guest's emitted functions, which a sampler attributes its samples by.
#[test]
fn a_profiled_run_maps_the_guest_s_functions() {
    let root = Temporary::new("profile", "mapped");
    let stream = root.join("stream.pftrace");
    let (output, map) = run(&root, &["--profile", stream.to_str().unwrap()]);
    let written = fs::read_to_string(&map);
    let _ = fs::remove_file(&map);
    assert!(output.status.success(), "{output:?}");

    let written = written.unwrap_or_else(|error| panic!("{}: {error}", map.display()));
    assert!(
        written.lines().any(|line| line.contains("func/")),
        "{written}"
    );
}

// A run nobody profiles writes no map: the engine's agent is asked by `--profile` alone.
#[test]
fn a_run_unasked_writes_no_map() {
    let root = Temporary::new("profile", "unmapped");
    let (output, map) = run(&root, &[]);
    let unasked = map.exists();
    let _ = fs::remove_file(&map);
    assert!(output.status.success(), "{output:?}");

    assert!(!unasked, "{} was written unasked", map.display());
}

/// The prelude's build under a recorder: [`compile`] does what `curios-prelude`'s build script does, every root lowered, elaborated, erased and certified, its stream filed at `.artifacts/prelude.pftrace` beside this crate's manifest and its fold beside the stream, at `.artifacts/prelude.folded.tsv`, and printed. `cargo xtask profile prelude` runs it.
///
/// **A measurement is asked, and no build is.** A build script takes no argument, so nothing can ask one for a stream, and one that recorded for carrying its instrumentation would record every build; and a program of the build's own crate would put the recorder in the prelude's image key. The images it builds go to a directory of its own, removed when it is done: the ones the compiler embeds are the build script's.
#[test]
#[ignore = "measurement, mixed: files the prelude's build's stream and prints its fold"]
fn prelude_build_profile() {
    let artifacts = Path::new(env!("CARGO_MANIFEST_DIR")).join(".artifacts");
    let (stream, folded) = (
        artifacts.join("prelude.pftrace"),
        artifacts.join("prelude.folded.tsv"),
    );
    let out = Temporary::new("profile", "prelude");
    fs::create_dir_all(&out).unwrap();

    trace(Destination::File(stream.clone()), || {
        compile(&out);
    })
    .unwrap_or_else(|error| panic!("{}: {error}", stream.display()));

    let report = fold_at(&stream)
        .unwrap_or_else(|error| panic!("{}: {error}", stream.display()))
        .render();
    fs::write(&folded, &report).unwrap_or_else(|error| panic!("{}: {error}", folded.display()));
    print!("{report}");
    println!(
        "\nstream: {}\nfolded: {}",
        stream.display(),
        folded.display()
    );
}
