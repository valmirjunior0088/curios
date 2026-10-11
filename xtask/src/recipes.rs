//! Each recipe here is its steps and nothing else. What selects one is [`main`](crate)'s rule table, which names every recipe and dispatches it to a single call from this file.
//!
//! The vocabulary they are written in sits beside them: [`places`](crate::places) for where a thing lives, [`commands`](crate::commands) for how a tool is invoked, [`filing`](crate::filing) for how a build product is filed, [`constants`](crate::constants) for what a recipe needs of a tool cargo does not bring. A recipe leaves this file when it acquires decisions of its own, as [`installer`](mod@crate::installer) has.

use {
    crate::{
        BROWSER_TRIPLE, HOST_TRIPLE, NEXTEST_INSTALL, NODE_FLOOR, artifact, ask, ask_cargo,
        bindgen_web, built, cargo, file_with_inputs, inputs, root, run, target_directory,
    },
    std::{fs, path::Path, process::Command},
};

#[cfg(test)]
mod tests;

/// The package the launcher is built from, under its default features alone.
const LAUNCHER: &str = "curios-runtime";

pub(crate) fn runtime() -> Result<(), String> {
    cargo(&[
        "build",
        "--release",
        "--package",
        LAUNCHER,
        "--target",
        HOST_TRIPLE,
    ])?;

    file_with_inputs(
        &built(HOST_TRIPLE, LAUNCHER),
        &artifact("curios", HOST_TRIPLE),
        &inputs("curios", HOST_TRIPLE),
    )?;

    Ok(())
}

/// One cargo check over the whole workspace, or over the one package it was given, with `before` naming the subcommand and `after` what follows the scope.
///
/// Which packages exist is cargo's question and not a list kept here. A name the workspace does not hold is refused before anything is built, though not by name: `--all-features` is what cargo notices first, so it reports a feature selection outside the workspace.
pub(crate) fn scoped(package: Option<&str>, before: &[&str], after: &[&str]) -> Result<(), String> {
    cargo(&[before, scope(package).as_slice(), after].concat())
}

/// The packages a workspace check covers: the one it was given, or every member.
fn scope(package: Option<&str>) -> Vec<&str> {
    match package {
        Some(package) => vec!["--package", package],
        None => vec!["--workspace"],
    }
}

/// Lint the workspace, or one package, over every target and feature with warnings denied, and the launcher once more as it ships, where the selection holds it.
///
/// **The launcher is linted as [`runtime`] builds it.** Every feature turns on the backend the launcher is built without, so what its own build alone leaves unread, an import or a field only a backend reads, is seen by no lint over every feature, and `runtime` denies nothing. The second pass is the launcher's package under its default features, in a cargo invocation of its own for the reason `runtime` has one, over its library and its binary: its tests need a backend.
pub(crate) fn lint(package: Option<&str>) -> Result<(), String> {
    scoped(
        package,
        &["clippy"],
        &["--all-targets", "--all-features", "--", "-Dwarnings"],
    )?;

    match holds_launcher(package) {
        true => cargo(&["clippy", "--package", LAUNCHER, "--", "-Dwarnings"]),
        false => Ok(()),
    }
}

/// Whether a selection holds the launcher's package: the whole workspace does, and so does the package by its name.
fn holds_launcher(package: Option<&str>) -> bool {
    package.is_none_or(|package| package == LAUNCHER)
}

/// The workspace's tests, or one package's, under nextest: every target and feature, every failure reported rather than the first, narrowed to the tests whose path contains `filter`.
///
/// **Nextest rather than `cargo test`, for a process to a test.** Binaryen's settings are the process's and its optimizer serializes behind a lock, so tests that share a process queue for it, and a test that aborts — Binaryen answering a module it cannot parse with a C++ `assert` — takes every other result of its binary with it. Under nextest each test is a process of its own: the suite runs on every core, and an abort fails by name. Doctests are not its to run, which is why `doctest` stays cargo's.
///
/// **A missing nextest is refused before anything is built**, by [`nextest_ready`], with the command that installs it.
pub(crate) fn test(package: Option<&str>, filter: Option<&str>) -> Result<(), String> {
    nextest_ready()?;

    cargo(&test_arguments(package, filter))
}

/// The `cargo nextest run` command line for one selection, which is the whole workspace when nothing narrows it.
///
/// The filter is last because nextest reads what follows its options as the names to match, and it is one narrowing rather than a tail: the recipe takes a single name and places it. A selection that matches nothing warns rather than fails, because a package with no test of its own is rightly empty, and nextest's own default would fail the run that named it.
fn test_arguments<'a>(package: Option<&'a str>, filter: Option<&'a str>) -> Vec<&'a str> {
    ["nextest", "run"]
        .into_iter()
        .chain(scope(package))
        .chain([
            "--all-targets",
            "--all-features",
            "--no-fail-fast",
            "--no-tests=warn",
        ])
        .chain(filter)
        .collect()
}

/// Refuse unless the cargo that runs the suite has nextest, saying so and giving the command that installs it.
///
/// Presence is read off `cargo --list` rather than found by running the subcommand: cargo answers a subcommand it does not have with an error of its own, and the refusal here is the one that says what to do.
fn nextest_ready() -> Result<(), String> {
    match lists_nextest(&ask_cargo(&["--list"])?) {
        true => Ok(()),
        false => Err(nextest_refusal()),
    }
}

/// Whether `cargo --list` names nextest among the commands it can run. Each command is a line led by its name, so a command merely named after nextest is not it.
fn lists_nextest(listing: &str) -> bool {
    listing
        .lines()
        .any(|line| line.split_whitespace().next() == Some("nextest"))
}

/// That the suite cannot run without nextest, and the command that installs it — on a line of its own, indented, because it is there to be selected and pasted rather than read, as the installer's advice is.
fn nextest_refusal() -> String {
    format!(
        "test needs cargo-nextest, and it is not installed. Install it with:\n\n    {NEXTEST_INSTALL}"
    )
}

pub(crate) fn build() -> Result<(), String> {
    runtime()?;

    cargo(&["build", "--release", "--package", "curios"])?;

    Ok(())
}

pub(crate) fn js() -> Result<(), String> {
    cargo(&[
        "build",
        "--release",
        "--package",
        "curios-js",
        "--target",
        BROWSER_TRIPLE,
    ])?;

    bindgen_web(
        &built(BROWSER_TRIPLE, "curios_js.wasm"),
        &artifact("curios-js", BROWSER_TRIPLE),
    )?;

    Ok(())
}

/// The browser bundle's own suite: `js` builds and files the bundle, and Node's built-in test runner runs every `curios-js/tests/*.test.mjs` against what was filed. The pattern is Node's to expand rather than a shell's, so the recipe spawns no shell; the runner and the engine are Node's own, so the suite has no package to install. A Node older than [`NODE_FLOOR`] is refused before anything is built, naming the version it found, since under one the suite fails in ways that do not say why.
pub(crate) fn js_test() -> Result<(), String> {
    let version = ask(Command::new("node"), &["--version"])?;
    let version = version.trim();
    let major = version
        .trim_start_matches('v')
        .split('.')
        .next()
        .and_then(|major| major.parse::<u32>().ok());

    if major.is_none_or(|major| major < NODE_FLOOR) {
        return Err(format!(
            "js-test needs Node {NODE_FLOOR} or later, and `node --version` answered {version}"
        ));
    }

    js()?;

    run(
        Command::new("node"),
        &["--test", "curios-js/tests/*.test.mjs"],
    )?;

    Ok(())
}

/// The one spelling of the rustdoc build: the gate's, the check workflow's and the release's, so a broken intra-doc link fails all three the same way. It keeps going past a crate that fails, so one run reports every crate's broken links rather than stopping at the first. Private items are documented because these crates state their invariants on `pub(crate)` items, and the root redirect is what makes the tree a site: `target/doc/` has no landing page of its own.
pub(crate) fn docs() -> Result<(), String> {
    runtime()?;

    cargo(&[
        "doc",
        "--workspace",
        "--no-deps",
        "--document-private-items",
        "--keep-going",
    ])?;

    let landing = target_directory().join("doc").join("index.html");
    fs::write(
        &landing,
        "<!DOCTYPE html><meta http-equiv=\"refresh\" content=\"0; url=curios/index.html\">\n",
    )
    .map_err(|error| format!("{}: {error}", landing.display()))?;

    Ok(())
}

/// The cargo profile [`profile`] builds the compiler under.
#[derive(Clone, Copy, Debug, clap::ValueEnum)]
pub(crate) enum BuildProfile {
    /// The build iterating already has, so a question about a change costs no second compile of the workspace, and a slowdown reproduces in the build it was found in.
    Debug,
    /// The shipped compiler's, whose timings are the ones a specification quotes.
    Release,
}

impl BuildProfile {
    fn flags(self) -> &'static [&'static str] {
        match self {
            Self::Debug => &[],
            Self::Release => &["--release"],
        }
    }
}

/// Run `source` under a profiling build, then ask that same build to read back the stream it filed.
///
/// **The compiler is not asked to profile anything.** A `profile` build files every span and event it makes, whatever subcommand ran, so this recipe selects a subject rather than a mode.
///
/// **Two invocations of one binary, and this recipe knows only the path.** Folding is not the profiled run's last step, because a stream is worth reading exactly when the run did not finish and such a run never reaches its last step — so the read is a separate pass. It is a pass the *compiler* makes rather than this crate, because the stream's format is the writer's, and a reader here would be a second spelling of it that nothing checks. What this crate spells is the destination, once, and hands it to both halves.
///
/// **Debug unless asked otherwise.** `curios` has no feature but `profile`, so the debug build is the one `--all-features` already made while iterating, and a question asked of a change costs the change; `--profile release` is for a figure a specification quotes, which is the shipped compiler's.
pub(crate) fn profile(source: &Path, build: BuildProfile) -> Result<(), String> {
    runtime()?;

    // The one place the stream's location is spelled. The compiler takes it as an argument and keeps no default, so what is written and what is read back cannot drift — and it is derived here the way every other path in this crate is.
    let stream = root().join("curios/.artifacts/profile.tsv");
    let stream = stream.to_string_lossy();
    let compiler = |arguments: &[&str]| {
        let mut command = vec!["run"];
        command.extend(build.flags());
        command.extend(["--package", "curios", "--features", "profile", "--"]);
        command.extend(arguments);
        cargo(&command)
    };

    compiler(&["--profile", &stream, "run", &source.to_string_lossy()])?;
    compiler(&["profile", &stream])?;

    println!("\nstream: {stream}");

    Ok(())
}

/// The half of the prelude's build [`profile_prelude`] runs under a recorder.
#[derive(Clone, Copy, Debug, clap::ValueEnum)]
pub(crate) enum PreludeHalf {
    /// `/sys` and `/std` lowered, elaborated, erased and archived: `curios-prelude-archive`'s build.
    Elaboration,
    /// The archived images walked by the kernel: `curios-prelude`'s build.
    Certification,
}

impl PreludeHalf {
    /// The package whose build this half is, and the program beside its build script that runs the half under a recorder.
    fn program(self) -> (&'static str, &'static str) {
        match self {
            Self::Elaboration => ("curios-prelude-archive", "profile-elaboration"),
            Self::Certification => ("curios-prelude", "profile-certification"),
        }
    }
}

/// Run one half of the prelude's build under a recorder, its stream filed at `stream`.
///
/// **A program is asked, and no build is.** The prelude's two build scripts record nothing: a build script takes no argument, so nothing can ask one. Each half's work is a module its build script shares with a program standing beside it, which runs that work again under `curios-profile`'s recorder, into a directory of its own, and prints the fold of what it filed.
///
/// **Release**, which compiles the compiler's front half at the level a build script's copy is compiled at under either profile, so the program measures what the build runs.
///
/// `stream` is taken from where the recipe was run, as a path typed there means, and handed on whole: cargo runs the program from the workspace root.
pub(crate) fn profile_prelude(half: PreludeHalf, stream: &Path) -> Result<(), String> {
    let (package, program) = half.program();
    let stream =
        std::path::absolute(stream).map_err(|error| format!("{}: {error}", stream.display()))?;

    cargo(&[
        "run",
        "--release",
        "--package",
        package,
        "--bin",
        program,
        "--features",
        "profile",
        "--",
        &stream.to_string_lossy(),
    ])
}

pub(crate) fn clean() -> Result<(), String> {
    run(Command::new("git"), &["clean", "-xffd"])?;

    Ok(())
}
