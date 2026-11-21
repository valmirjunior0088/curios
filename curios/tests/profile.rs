//! What `--profile` asks of an invocation: the stream it files, which `curios profile` folds, and the map a sampling profiler names a hosted program's functions by, written only where it is asked.

use {
    curios::CABINET,
    curios_common::test_support::*,
    std::{
        fs,
        path::{Path, PathBuf},
        process::{Command, Output, Stdio},
    },
};

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
    let stream = root.join("stream.tsv");
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
    let stream = root.join("stream.tsv");
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
