//! What each step runs, and the four commands that are not steps.
//!
//! [`Step::run`] is an exhaustive match, so a step with a row and nothing to run does not compile. What selects a step is [`main`](crate)'s command line, read off the table in [`steps`](crate::steps).
//!
//! The vocabulary a step is written in sits beside it: [`places`](crate::places) for where a thing lives, [`commands`](crate::commands) for how a tool is invoked, [`filing`](crate::filing) for how a build product is filed, [`tools`](crate::tools) for what a step needs of a tool cargo does not bring. A command leaves this file when it acquires decisions of its own, as [`installer`](mod@crate::installer) has.

use {
    crate::{
        BIOME, BROWSER_TRIPLE, HOST_TRIPLE, Step, Tree, artifact, ask, bindgen_web, built, cargo,
        file_with_inputs, grammar, inputs, modified, root, run, target_directory, vscode, zed,
    },
    std::{fs, io::ErrorKind, path::Path, process::Command},
};

#[cfg(test)]
mod tests;

/// What a person narrowed an action by. A bare subject and the gate narrow nothing and write nothing: [`Request::whole`].
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct Request {
    /// Whether a format step writes nothing and fails where a file would change.
    pub(crate) check: bool,
    /// The one package of the workspace a lint or a test is narrowed to.
    pub(crate) package: Option<String>,
    /// The text a test's path must hold for it to run.
    pub(crate) filter: Option<String>,
}

impl Request {
    /// Nothing narrowed, formatting as a check: what a bare subject and the gate ask of every step.
    pub(crate) fn whole() -> Self {
        Self {
            check: true,
            package: None,
            filter: None,
        }
    }

    /// A format action, writing unless it is asked to check.
    pub(crate) fn formatting(check: bool) -> Self {
        Self {
            check,
            ..Self::whole()
        }
    }

    /// An action narrowed to a package, a test to the paths holding `filter`.
    pub(crate) fn narrowed(package: Option<String>, filter: Option<String>) -> Self {
        Self {
            package,
            filter,
            ..Self::whole()
        }
    }
}

impl Step {
    /// Run the step as `request` narrows it.
    pub(crate) fn run(self, request: &Request) -> Result<(), String> {
        let package = request.package.as_deref();

        match self {
            Step::LauncherBuild => launcher_build(),
            Step::WorkspaceFormat => {
                cargo(&formatting(request.check))?;

                loose_formatting(request.check)
            }
            Step::WorkspaceFormatWeb => biome_formatting(Tree::Workspace, request.check),
            Step::WorkspaceLint => cargo(&linting(package)),
            // A package narrows the lint to its Rust, so what the workspace holds beside Rust is not its to answer for.
            Step::WorkspaceLintWeb => match package {
                Some(_) => Ok(()),
                None => biome_linting(Tree::Workspace),
            },
            Step::WorkspaceTest => cargo(&testing(package, request.filter.as_deref())),
            Step::WorkspaceDoctest => cargo(&doctesting(package)),
            Step::InstrumentedLint => {
                cargo(&["clippy", "--workspace", "--all-targets", "--all-features"])
            }
            Step::DocsBuild => docs_build(),
            Step::CompilerBuild => cargo(&["build", "--release", "--package", "curios"]),
            // Whoever narrows the run asks about a package's tests or one test's, which the cache of another crate's build script is not.
            Step::WorkspaceBinaryenCache => match (package, request.filter.as_deref()) {
                (None, None) => binaryen_cache(),
                _ => Ok(()),
            },
            Step::BundleBuild => bundle_build(),
            Step::BundleTest => run(
                Command::new("node"),
                &["--test", "curios-js/tests/*.test.mjs"],
            ),
            Step::GrammarInstall => grammar(&["clean-install"]),
            Step::GrammarFormat => biome_formatting(Tree::Grammar, request.check),
            Step::GrammarLint => biome_linting(Tree::Grammar),
            Step::GrammarTest => grammar(&["test"]),
            Step::VscodeInstall => vscode(&["clean-install"]),
            Step::VscodeFormat => biome_formatting(Tree::Vscode, request.check),
            Step::VscodeLint => biome_linting(Tree::Vscode),
            Step::VscodeTest => vscode(&["test"]),
            Step::VscodeBuild => vscode(&["run", "package"]),
            Step::ZedFormat => zed(&formatting(request.check)),
            Step::ZedLint => zed(&["clippy", "--all-targets", "--target", EXTENSION_TRIPLE]),
            Step::ZedBuild => zed(&["build", "--release", "--target", EXTENSION_TRIPLE]),
            Step::ZedTest => zed(&["test"]),
        }
    }
}

/// The package the launcher is built from, under its default features alone.
const LAUNCHER: &str = "curios-runtime";

/// The triple Zed loads an extension for.
const EXTENSION_TRIPLE: &str = "wasm32-wasip2";

/// The launcher, built in a cargo invocation of its own so workspace feature unification cannot reach it, and filed where `curios/build.rs` embeds it.
fn launcher_build() -> Result<(), String> {
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
    )
}

/// The packages a workspace check covers: the one it was given, or every member.
fn scope(package: Option<&str>) -> Vec<&str> {
    match package {
        Some(package) => vec!["--package", package],
        None => vec!["--workspace"],
    }
}

/// The `cargo fmt` command line: every package of the workspace it runs in, written unless `check`.
fn formatting(check: bool) -> Vec<&'static str> {
    match check {
        true => vec!["fmt", "--all", "--", "--check"],
        false => vec!["fmt", "--all"],
    }
}

/// The workspace's Rust that belongs to no package, which `cargo fmt` therefore never visits: the measurement corpus's Rust twins. Formatted as the workspace's packages are, under its edition.
fn loose_formatting(check: bool) -> Result<(), String> {
    let mut git = Command::new("git");
    git.args(["ls-files", "--", "programs/*.rs"]);
    let listing = ask(git, &[])?;
    let loose = listing.lines().collect::<Vec<_>>();
    if loose.is_empty() {
        return Ok(());
    }

    let edition = workspace_edition()?;
    let mut arguments = vec!["--edition", edition.as_str()];
    if check {
        arguments.push("--check");
    }
    arguments.extend(loose);

    run(Command::new("rustfmt"), &arguments)
}

/// The edition every package of the workspace inherits, read from the manifest that states it.
fn workspace_edition() -> Result<String, String> {
    let manifest = root().join("Cargo.toml");
    let text = fs::read_to_string(&manifest)
        .map_err(|error| format!("{}: {error}", manifest.display()))?;

    text.parse::<toml_edit::DocumentMut>()
        .map_err(|error| format!("{}: {error}", manifest.display()))?
        .get("workspace")
        .and_then(|workspace| workspace.get("package"))
        .and_then(|package| package.get("edition"))
        .and_then(|edition| edition.as_str())
        .map(str::to_string)
        .ok_or_else(|| "Cargo.toml has no [workspace.package] edition".to_string())
}

/// Biome over the JavaScript, JSON and CSS `tree` holds, as `arguments` say. It is run through `npx` at the one version [`BIOME`] names, from the repository's root, where `biome.json` says which files it reads; the tree's paths are what keeps it to the tree.
fn biome(tree: Tree, arguments: &[&str]) -> Result<(), String> {
    let paths = tree.paths()?;
    let mut line = vec!["--yes", BIOME];
    line.extend(arguments);
    line.extend(paths.iter().map(String::as_str));

    run(Command::new("npx"), &line)
}

/// Biome's formatter over `tree`, written unless `check`.
fn biome_formatting(tree: Tree, check: bool) -> Result<(), String> {
    match check {
        true => biome(tree, &["format"]),
        false => biome(tree, &["format", "--write"]),
    }
}

/// Biome's linter over `tree`, a warning failing as an error does: the workspace's Rust is held the same way by its lint table.
fn biome_linting(tree: Tree) -> Result<(), String> {
    biome(tree, &["lint", "--error-on-warnings"])
}

/// The `cargo clippy` command line for one selection: every target, as the workspace ships. Warnings are denied by the workspace's lint table, so no flag says so here.
fn linting(package: Option<&str>) -> Vec<&str> {
    ["clippy"]
        .into_iter()
        .chain(scope(package))
        .chain(["--all-targets"])
        .collect()
}

/// The `cargo nextest run` command line for one selection, which is the whole workspace when nothing narrows it: every target as the workspace ships, every failure reported rather than the first.
///
/// **Nextest rather than `cargo test`, for a process to a test.** Binaryen's settings are the process's and its optimizer serializes behind a lock, so tests that share a process queue for it, and a test that aborts — Binaryen answering a module it cannot parse with a C++ `assert` — takes every other result of its binary with it. Under nextest each test is a process of its own: the suite runs on every core, and an abort fails by name.
///
/// The filter is last because nextest reads what follows its options as the names to match, and it is one narrowing rather than a tail: the action takes a single name and places it. A selection that matches nothing warns rather than fails, because a package with no test of its own is rightly empty, and nextest's own default would fail the run that named it.
fn testing<'a>(package: Option<&'a str>, filter: Option<&'a str>) -> Vec<&'a str> {
    ["nextest", "run"]
        .into_iter()
        .chain(scope(package))
        .chain(["--all-targets", "--no-fail-fast", "--no-tests=warn"])
        .chain(filter)
        .collect()
}

/// The `cargo test --doc` command line for one selection: the examples nextest does not run.
fn doctesting(package: Option<&str>) -> Vec<&str> {
    ["test"]
        .into_iter()
        .chain(scope(package))
        .chain(["--doc"])
        .collect()
}

/// The browser bundle: `curios-js` built for the bare Wasm target, and its bindings generated beside it.
fn bundle_build() -> Result<(), String> {
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
    )
}

/// The one spelling of the rustdoc build: the gate's, the check workflow's and the release's, so a broken intra-doc link fails all three the same way. It documents every feature, so an item only a feature compiles has its links read too, and keeps going past a crate that fails, so one run reports every crate's broken links rather than stopping at the first. Private items are documented because these crates state their invariants on `pub(crate)` items, and the root redirect is what makes the tree a site: `target/doc/` has no landing page of its own.
fn docs_build() -> Result<(), String> {
    cargo(&[
        "doc",
        "--workspace",
        "--no-deps",
        "--document-private-items",
        "--all-features",
        "--keep-going",
    ])?;

    let landing = target_directory().join("doc").join("index.html");
    fs::write(
        &landing,
        "<!DOCTYPE html><meta http-equiv=\"refresh\" content=\"0; url=curios/index.html\">\n",
    )
    .map_err(|error| format!("{}: {error}", landing.display()))
}

/// Build Binaryen from its source and hold that a second cargo mode reuses the build.
///
/// The cache is removed first, so the build that follows is the one from source. Linting the package is another cargo mode with a build-script fingerprint of its own: it must find the cache and run no CMake, which the cache's marker says by keeping its modification time.
fn binaryen_cache() -> Result<(), String> {
    let cache = root()
        .join("curios-binaryen")
        .join(".artifacts")
        .join(HOST_TRIPLE);
    match fs::remove_dir_all(&cache) {
        Ok(()) => {}
        Err(error) if error.kind() == ErrorKind::NotFound => {}
        Err(error) => return Err(format!("{}: {error}", cache.display())),
    }

    cargo(&["build", "--package", "curios-binaryen"])?;

    let marker = cache.join(BINARYEN_MARKER);
    let built =
        modified(&marker).ok_or_else(|| format!("the build left no {}", marker.display()))?;

    cargo(&linting(Some("curios-binaryen")))?;

    match modified(&marker) == Some(built) {
        true => Ok(()),
        false => Err(format!(
            "linting curios-binaryen rebuilt Binaryen: {} was written again",
            marker.display()
        )),
    }
}

/// The file `curios-binaryen/build.rs` writes last into a cache entry, which is how it knows an entry is whole.
const BINARYEN_MARKER: &str = "done";

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
    launcher_build()?;

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
