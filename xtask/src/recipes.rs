//! Each recipe here is its steps and nothing else. What selects one is [`main`](crate)'s rule table, which names every recipe and dispatches it to a single call from this file.
//!
//! The vocabulary they are written in sits beside them: [`places`](crate::places) for where a thing lives, [`commands`](crate::commands) for how a tool is invoked, [`filing`](crate::filing) for how a build product is filed, [`constants`](crate::constants) for what a recipe needs of a tool cargo does not bring. A recipe leaves this file when it acquires decisions of its own, as [`installer`](mod@crate::installer) has.

use {
    crate::{
        NEXTEST_FLOOR, NEXTEST_INSTALL, NODE_FLOOR,
        commands::{ask, ask_cargo, bindgen_web, cargo, run},
        filing::file_with_inputs,
        places::{BROWSER_TRIPLE, HOST_TRIPLE, artifact, built, inputs, root, target_directory},
    },
    std::{fs, path::Path, process::Command, str::FromStr},
};

#[cfg(test)]
mod tests;

pub(crate) fn runtime() -> Result<(), String> {
    cargo(&[
        "build",
        "--release",
        "--package",
        "curios-runtime",
        "--target",
        HOST_TRIPLE,
    ])?;

    file_with_inputs(
        &built(HOST_TRIPLE, "curios-runtime"),
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

/// One of `total` even slices of a test selection, counted from one: what `--shard M/N` names.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Shard {
    index: u32,
    total: u32,
}

impl FromStr for Shard {
    type Err = String;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        let parts = text
            .split_once('/')
            .and_then(|(index, total)| Some((index.parse().ok()?, total.parse().ok()?)));

        match parts {
            Some((index, total)) if (1..=total).contains(&index) => Ok(Self { index, total }),
            _ => Err("a shard is M/N, the Mth of N slices, with M from 1 to N".into()),
        }
    }
}

/// The workspace's tests, or one package's, under nextest: every target and feature, every failure reported rather than the first, narrowed to the tests whose path contains `filter` and then to one `shard` of what that leaves.
///
/// **Nextest rather than `cargo test`, for the shard.** A shard is cut from the lists the test binaries themselves answer with, so the check workflow runs the suite as parallel jobs with no list of tests kept anywhere: a test written today is in exactly one shard tomorrow. Nextest also runs each test as a process of its own, so a test that aborts — Binaryen answering a module it cannot parse with a C++ `assert` — fails by name instead of taking every other result with it. Doctests are not its to run, which is why `doctest` stays cargo's.
///
/// **A nextest the suite cannot run under is refused before anything is built**, by [`nextest_ready`], with the command that fixes it.
pub(crate) fn test(
    package: Option<&str>,
    filter: Option<&str>,
    shard: Option<Shard>,
) -> Result<(), String> {
    nextest_ready()?;

    let arguments = test_arguments(package, filter, shard);

    cargo(&arguments.iter().map(String::as_str).collect::<Vec<_>>())
}

/// The `cargo nextest run` command line for one selection, which is the whole workspace when nothing narrows it.
///
/// The filter is last because nextest reads what follows its options as the names to match, and it is one narrowing rather than a tail: the recipe takes a single name and places it. A selection that matches nothing warns rather than fails, because a shard of a package with fewer tests than shards is rightly empty, and nextest's own default would fail the job that drew it.
fn test_arguments(
    package: Option<&str>,
    filter: Option<&str>,
    shard: Option<Shard>,
) -> Vec<String> {
    let mut arguments = ["nextest", "run"]
        .into_iter()
        .chain(scope(package))
        .chain([
            "--all-targets",
            "--all-features",
            "--no-fail-fast",
            "--no-tests=warn",
        ])
        .map(String::from)
        .collect::<Vec<_>>();

    if let Some(Shard { index, total }) = shard {
        arguments.extend(["--partition".into(), format!("slice:{index}/{total}")]);
    }

    arguments.extend(filter.map(String::from));

    arguments
}

/// Refuse unless the cargo that runs the suite has a nextest at [`NEXTEST_FLOOR`] or later, saying what is missing and the command that fixes it.
///
/// Presence is read off `cargo --list` rather than found by running the subcommand: cargo answers a subcommand it does not have with an error of its own, and the refusal here is the one that says what to do.
fn nextest_ready() -> Result<(), String> {
    if !lists_nextest(&ask_cargo(&["--list"])?) {
        return Err(nextest_refusal("it is not installed", "Install"));
    }

    let answer = ask_cargo(&["nextest", "--version"])?;

    if !admits_nextest(&answer) {
        let answered = answer.split_whitespace().nth(1).unwrap_or(answer.trim());

        return Err(nextest_refusal(
            &format!("`cargo nextest --version` answered {answered}"),
            "Update",
        ));
    }

    Ok(())
}

/// Whether `cargo --list` names nextest among the commands it can run. Each command is a line led by its name, so a command merely named after nextest is not it.
fn lists_nextest(listing: &str) -> bool {
    listing
        .lines()
        .any(|line| line.split_whitespace().next() == Some("nextest"))
}

/// Whether the nextest that answered `--version` is one the suite runs under. An answer this cannot read a version from is refused with the rest, since nothing says it has `slice:`.
fn admits_nextest(answer: &str) -> bool {
    nextest_version(answer).is_some_and(|version| version >= NEXTEST_FLOOR)
}

/// The three numbers of the version nextest answers `--version` with, which leads its first line as `cargo-nextest 0.9.146 (8af696ddc 2026-09-21)`. A pre-release counts as the release it precedes.
fn nextest_version(answer: &str) -> Option<[u32; 3]> {
    let version = answer.split_whitespace().nth(1)?;
    let release = version.split(['-', '+']).next()?;
    let mut numbers = release.split('.').map(|number| number.parse().ok());
    let read = [numbers.next()??, numbers.next()??, numbers.next()??];

    numbers.next().is_none().then_some(read)
}

/// Why the suite cannot run under the nextest that was `found`, and the command that fixes it — on a line of its own, indented, because it is there to be selected and pasted rather than read, as the installer's advice is.
fn nextest_refusal(found: &str, remedy: &str) -> String {
    let floor = NEXTEST_FLOOR.map(|number| number.to_string()).join(".");

    format!(
        "test needs cargo-nextest {floor} or later, and {found}. {remedy} it with:\n\n    {NEXTEST_INSTALL}"
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
/// **Two invocations of one binary, and this recipe knows only the path.** Folding is not the profiled run's last step, because a stream is worth reading exactly when the run did not finish and such a run never reaches its last step — so the read is a separate pass. It is a pass the *compiler* makes rather than this crate, because which files one rotated stream occupies is the writer's decision, and restating it here would be a second spelling of a convention nothing checks. What this crate spells is the destination, once, and hands it to both halves.
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

pub(crate) fn benchmarks(tag: &str) -> Result<(), String> {
    run(
        Command::new("docker"),
        &[
            "build",
            "--platform",
            "linux/arm64",
            "--file",
            "benchmarks/Dockerfile",
            "--tag",
            tag,
            ".",
        ],
    )?;

    run(
        Command::new("docker"),
        &["run", "--rm", "--cpuset-cpus", "0", tag],
    )?;

    Ok(())
}

pub(crate) fn clean() -> Result<(), String> {
    run(Command::new("git"), &["clean", "-xffd"])?;

    Ok(())
}
