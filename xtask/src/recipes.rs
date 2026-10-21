//! What each step asks the runner to do, and the commands that are not steps.
//!
//! [`Step::plan`] is an exhaustive match, so a step with a row and no plan does not compile. A plan is data — the [`Task`]s the step needs done, read off the request and the checkout as it stands — and the [`runner`](crate::runner) performs it: what selects a step is [`main`](crate)'s command line, read off the table in [`steps`](crate::steps).
//!
//! The vocabulary a plan is written in sits beside it: [`tasks`](crate::tasks) for what a plan holds, [`places`](crate::places) for where a thing lives, [`tools`](crate::tools) for the programs a step runs, [`trees`](crate::trees) for which paths a tree holds. A command leaves this file when it acquires decisions of its own, as [`installer`](mod@crate::installer) has.

use {
    crate::{
        Arg, BROWSER_TRIPLE, Checkout, HOST_TRIPLE, Product, Run, Selection, Step, Task, Tool,
        Tree, built, cargo, inputs, perform, root, run, target_directory, texts,
    },
    std::{fs, path::Path, process::Command},
};

#[cfg(test)]
mod tests;

/// What a person narrowed an action by. A bare subject narrows nothing and writes nothing: [`Request::whole`].
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct Request {
    /// Whether a format step writes nothing and fails where a file would change.
    pub(crate) check: bool,
    /// The one package of the workspace a lint or a test is narrowed to.
    pub(crate) package: Option<String>,
    /// What a test's path must hold for it to run; with [`Request::binary`], one of them is enough, and nothing narrows a run when both are empty.
    pub(crate) filter: Vec<String>,
    /// The test binaries whose tests run, by name.
    pub(crate) binary: Vec<String>,
}

impl Request {
    /// Nothing narrowed, formatting as a check: what a bare subject asks of every step.
    pub(crate) fn whole() -> Self {
        Self {
            check: true,
            package: None,
            filter: Vec::new(),
            binary: Vec::new(),
        }
    }

    /// A format action, writing unless it is asked to check.
    pub(crate) fn formatting(check: bool) -> Self {
        Self {
            check,
            ..Self::whole()
        }
    }

    /// An action narrowed to a package built alone, and a test run to the tests a path of `filter` or a binary of `binary` selects.
    pub(crate) fn narrowed(
        package: Option<String>,
        filter: Vec<String>,
        binary: Vec<String>,
    ) -> Self {
        Self {
            package,
            filter,
            binary,
            ..Self::whole()
        }
    }

    /// Whether nothing narrows a test run by name.
    fn unselected(&self) -> bool {
        self.filter.is_empty() && self.binary.is_empty()
    }
}

impl Step {
    /// What the step asks the runner to do, as `request` narrows it, read off `checkout` as it stands.
    pub(crate) fn plan(self, request: &Request, checkout: &Checkout) -> Result<Vec<Task>, String> {
        let package = request.package.as_deref();

        let plan = match self {
            Step::LauncherBuild => launcher_build(),
            Step::WorkspaceFormat => {
                let mut plan =
                    vec![Run::cargo(Tree::Workspace, &formatting(request.check), None, &[]).into()];
                plan.extend(loose_formatting(request.check, checkout)?);

                plan
            }
            Step::WorkspaceFormatWeb => {
                vec![biome_formatting(Tree::Workspace, request.check, checkout)?]
            }
            Step::WorkspaceLint => vec![linting(package).into()],
            // A package narrows the lint to its Rust, so what the workspace holds beside Rust is not its to answer for.
            Step::WorkspaceLintWeb => match package {
                Some(_) => Vec::new(),
                None => vec![biome_linting(Tree::Workspace, checkout)?],
            },
            Step::WorkspaceTest => vec![testing(package, request)?.into()],
            // Whoever narrows the run asks about a package's tests or some test's, which the cache of another crate's build script is not.
            Step::WorkspaceBinaryenCache => match (package, request.unselected()) {
                (None, true) => binaryen_cache(),
                _ => Vec::new(),
            },
            Step::InstrumentedLint => vec![
                Run::cargo(
                    Tree::Workspace,
                    &["clippy"],
                    Some(Selection::Shipped),
                    &["--all-targets", "--all-features", KEEP_GOING],
                )
                .into(),
            ],
            Step::InstrumentedTest => vec![recorder_testing().into()],
            Step::DocsBuild => docs_build(),
            Step::CompilerBuild => vec![
                Run::cargo(
                    Tree::Workspace,
                    &["build", "--release"],
                    Some(Selection::package("curios")),
                    &[KEEP_GOING],
                )
                .leaving(Product::Compiler)
                .into(),
            ],
            // The compiler under test is the one `compiler build` filed, named to the suite rather than found on PATH, where an installed one would answer instead.
            Step::CompilerTest => vec![
                Run::new(
                    Tool::Node,
                    Tree::Workspace,
                    texts(["--test", "curios-browser/src/page.test.mjs"]),
                )
                .with("CURIOS", Arg::Product(Product::Compiler))
                .into(),
            ],
            Step::BundleBuild => bundle_build(),
            // The bundle under test is the one `bundle build` filed, named to the suite as the compiler is to the page's.
            Step::BundleTest => vec![
                Run::new(
                    Tool::Node,
                    Tree::Workspace,
                    texts(["--test", "curios-browser/src/harness.test.mjs"]),
                )
                .with("CURIOS_JS", Arg::Product(Product::Bundle))
                .into(),
            ],
            Step::GrammarInstall => vec![installing(Tree::Grammar)],
            Step::GrammarFormat => vec![biome_formatting(Tree::Grammar, request.check, checkout)?],
            Step::GrammarLint => vec![biome_linting(Tree::Grammar, checkout)?],
            Step::GrammarBuild => vec![
                Run::npm(Tree::Grammar, &["run", "generate"])
                    .leaving(Product::Parser)
                    .into(),
            ],
            Step::GrammarTest => vec![Run::npm(Tree::Grammar, &["test"]).into()],
            Step::VscodeInstall => vec![installing(Tree::Vscode)],
            Step::VscodeFormat => vec![biome_formatting(Tree::Vscode, request.check, checkout)?],
            Step::VscodeLint => vec![biome_linting(Tree::Vscode, checkout)?],
            Step::VscodeTest => vec![Run::npm(Tree::Vscode, &["test"]).into()],
            Step::VscodeBuild => vec![Run::npm(Tree::Vscode, &["run", "package"]).into()],
            Step::ZedFormat => {
                vec![Run::cargo(Tree::Zed, &formatting(request.check), None, &[]).into()]
            }
            Step::ZedLint => vec![
                Run::cargo(
                    Tree::Zed,
                    &["clippy", "--all-targets", "--target", EXTENSION_TRIPLE],
                    None,
                    &[],
                )
                .into(),
            ],
            Step::ZedBuild => vec![
                Run::cargo(
                    Tree::Zed,
                    &["build", "--release", "--target", EXTENSION_TRIPLE],
                    None,
                    &[],
                )
                .into(),
            ],
            // The extension's test hashes the grammar where `extension.toml` says Zed installs it from, a path the extension states rather than one it is handed.
            Step::ZedTest => vec![
                Run::cargo(Tree::Zed, &["test"], None, &[])
                    .finding(Product::Parser)
                    .into(),
            ],
            Step::ToolsLint => vec![
                Run::cargo(
                    Tree::Workspace,
                    &["clippy"],
                    Some(Selection::tools()),
                    &["--all-targets", KEEP_GOING],
                )
                .into(),
            ],
            Step::ToolsTest => {
                vec![Run::nextest(Selection::tools(), &["--all-targets", "--no-fail-fast"]).into()]
            }
        };

        Ok(plan)
    }
}

/// An npm tree's tools installed from its lock file, as they are every run that reads them: `npm clean-install` is exact, and it takes a second or two.
fn installing(tree: Tree) -> Task {
    Run::npm(tree, &["clean-install"])
        .leaving(Product::Installed(tree))
        .into()
}

/// The package the launcher is built from, under its default features alone.
const LAUNCHER: &str = "curios-native";

/// What a step that compiles several of the workspace's crates hands cargo: go on past a crate that fails, so one run reports every crate's errors rather than the first's. A test run says the same of its tests with nextest's own flag.
const KEEP_GOING: &str = "--keep-going";

/// The triple Zed loads an extension for.
const EXTENSION_TRIPLE: &str = "wasm32-wasip2";

/// The launcher, built in a cargo invocation of its own so workspace feature unification cannot reach it, and filed where `curios/build.rs` embeds it.
fn launcher_build() -> Vec<Task> {
    vec![
        Run::cargo(
            Tree::Workspace,
            &["build", "--release"],
            Some(Selection::package(LAUNCHER)),
            &["--target", HOST_TRIPLE, KEEP_GOING],
        )
        .into(),
        Task::File {
            built: built(HOST_TRIPLE, LAUNCHER),
            product: Product::Launcher,
            listed: inputs("curios", HOST_TRIPLE),
        },
    ]
}

/// The packages a workspace check covers: the one it was given, or every member but the tools, which `tools` checks apart.
fn scope(package: Option<&str>) -> Selection {
    match package {
        Some(package) => Selection::package(package),
        None => Selection::Shipped,
    }
}

/// The `cargo fmt` command line: every package of the workspace it runs in, written unless `check`.
fn formatting(check: bool) -> Vec<&'static str> {
    match check {
        true => vec!["fmt", "--all", "--", "--check"],
        false => vec!["fmt", "--all"],
    }
}

/// The workspace's Rust that belongs to no package, which `cargo fmt` therefore never visits: the measurement corpus's Rust twins, every tracked `.rs` file under `programs/`. Formatted as the workspace's packages are, under its edition.
fn loose_formatting(check: bool, checkout: &Checkout) -> Result<Option<Task>, String> {
    let loose = checkout
        .tracked()?
        .iter()
        .filter(|path| path.starts_with("programs/") && path.ends_with(".rs"))
        .map(String::as_str)
        .collect::<Vec<_>>();
    if loose.is_empty() {
        return Ok(None);
    }

    let edition = workspace_edition()?;
    let mut arguments = vec!["--edition", edition.as_str()];
    if check {
        arguments.push("--check");
    }
    arguments.extend(loose);

    Ok(Some(
        Run::new(Tool::Rustfmt, Tree::Workspace, texts(arguments)).into(),
    ))
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

/// Biome over the JavaScript, JSON and CSS `tree` holds, as `arguments` say, run from the repository's root, where `biome.json` says which files it reads; the tree's paths are what keeps it to the tree.
fn biome(tree: Tree, arguments: &[&str], checkout: &Checkout) -> Result<Task, String> {
    let paths = tree.paths(checkout.tracked()?.iter().map(String::as_str));
    let mut line = texts(arguments.iter().copied());
    line.extend(texts(paths.iter().map(String::as_str)));

    Ok(Run::new(Tool::Biome, Tree::Workspace, line).into())
}

/// Biome's formatter over `tree`, written unless `check`.
fn biome_formatting(tree: Tree, check: bool, checkout: &Checkout) -> Result<Task, String> {
    match check {
        true => biome(tree, &["format"], checkout),
        false => biome(tree, &["format", "--write"], checkout),
    }
}

/// Biome's linter over `tree`, a warning failing as an error does: the workspace's Rust is held the same way by its lint table.
fn biome_linting(tree: Tree, checkout: &Checkout) -> Result<Task, String> {
    biome(tree, &["lint", "--error-on-warnings"], checkout)
}

/// The `cargo clippy` run for one selection: every target, as the workspace ships, past a crate that fails. Warnings are denied by the workspace's lint table, so no flag says so here.
fn linting(package: Option<&str>) -> Run {
    Run::cargo(
        Tree::Workspace,
        &["clippy"],
        Some(scope(package)),
        &["--all-targets", KEEP_GOING],
    )
}

/// The nextest run for one selection, the whole workspace when nothing narrows it: every target as the workspace ships, every failure reported rather than the first.
///
/// **Nextest rather than `cargo test`, for a process to a test.** Binaryen's settings are the process's and its optimizer serializes behind a lock, so tests that share a process queue for it, and a test that aborts — Binaryen answering a module it cannot parse with a C++ `assert` — takes every other result of its binary with it. Under nextest each test is a process of its own: the suite runs on every core, and an abort fails by name.
///
/// **A name picks tests out of one build and never changes it.** A filter becomes nextest's matcher of a test's path and a binary its matcher of a binary's name, so the bundle suite, whose tests carry no word of their binary's name, is `--binary bundle`; all of them are one union, one run however many are named, and a binary no build has is nextest's own refusal. A package is the other narrowing and a build of its own, which a job on one machine asks for and a question about a few tests should not. A selection that matches nothing warns rather than fails, because a package with no test of its own is rightly empty, and nextest's own default would fail the run that named it.
fn testing(package: Option<&str>, request: &Request) -> Result<Run, String> {
    let selected = match request.unselected() {
        true => None,
        false => Some(selecting(request)?),
    };
    let mut options = vec!["--all-targets", "--no-fail-fast", "--no-tests=warn"];
    if let Some(selected) = &selected {
        options.extend(["-E", selected.as_str()]);
    }

    Ok(Run::nextest(scope(package), &options))
}

/// The filterset that runs a test one of `request`'s filters is part of the path of, or one of its binaries is the name of. A name holding a character the filterset's grammar reads — a parenthesis or a comma — is refused, since no test's path or binary's name holds one.
fn selecting(request: &Request) -> Result<String, String> {
    let names = request.filter.iter().chain(&request.binary);

    match names.clone().find(|name| name.contains(['(', ')', ','])) {
        Some(name) => Err(format!(
            "{name:?} holds a parenthesis or a comma, which no test's path or binary's name does"
        )),
        None => Ok(request
            .filter
            .iter()
            .map(|filter| format!("test(~{filter})"))
            .chain(
                request
                    .binary
                    .iter()
                    .map(|binary| format!("binary(={binary})")),
            )
            .collect::<Vec<_>>()
            .join(" | ")),
    }
}

/// The package that holds the recorder.
const RECORDER: &str = "curios-profile";

/// The feature of [`RECORDER`] that compiles the recorder, and so its tests.
const RECORDING: &str = "enabled";

/// The nextest run of the recorder's tests: [`RECORDER`] alone under [`RECORDING`], the one configuration that compiles them and one no shipped build of the workspace is.
fn recorder_testing() -> Run {
    Run::nextest(
        Selection::package(RECORDER),
        &["--features", RECORDING, "--no-fail-fast"],
    )
}

/// The browser bundle: `curios-js` built for the bare Wasm target, and its bindings generated beside it.
fn bundle_build() -> Vec<Task> {
    vec![
        Run::cargo(
            Tree::Workspace,
            &["build", "--release"],
            Some(Selection::package("curios-js")),
            &["--target", BROWSER_TRIPLE, KEEP_GOING],
        )
        .into(),
        Task::Bindgen {
            module: built(BROWSER_TRIPLE, "curios_js.wasm"),
            product: Product::Bundle,
        },
    ]
}

/// The one spelling of the rustdoc build: a contributor's, the check workflow's and the release's, so a broken intra-doc link fails all three the same way. It documents every feature, so an item only a feature compiles has its links read too, and keeps going past a crate that fails, so one run reports every crate's broken links rather than stopping at the first. Private items are documented because these crates state their invariants on `pub(crate)` items, and the root redirect is what makes the tree a site: `target/doc/` has no landing page of its own.
fn docs_build() -> Vec<Task> {
    vec![
        Run::cargo(
            Tree::Workspace,
            &["doc"],
            Some(Selection::Every),
            &[
                "--no-deps",
                "--document-private-items",
                "--all-features",
                KEEP_GOING,
            ],
        )
        .into(),
        Task::Write {
            path: target_directory().join("doc").join("index.html"),
            text: "<!DOCTYPE html><meta http-equiv=\"refresh\" content=\"0; url=curios/index.html\">\n",
        },
    ]
}

/// Hold that a second cargo mode reuses Binaryen's build.
///
/// Nothing is removed first: the cache's key holds a hash of the build script, so the first build after an edit to it is the one from source, whichever step makes it, and this writes only what any build writes. Linting the package is another cargo mode with a build-script fingerprint of its own: it must find the cache and run no CMake, which the cache's marker says by keeping its modification time.
fn binaryen_cache() -> Vec<Task> {
    let package = "curios-binaryen";

    vec![
        Run::cargo(
            Tree::Workspace,
            &["build"],
            Some(Selection::package(package)),
            &[],
        )
        .into(),
        Task::Keeps {
            path: root()
                .join(package)
                .join(".artifacts")
                .join(HOST_TRIPLE)
                .join(BINARYEN_MARKER),
            during: Box::new(linting(Some(package)).into()),
            rewritten: "linting curios-binaryen rebuilt Binaryen",
        },
    ]
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
    perform(&[Step::LauncherBuild], &Request::whole())?;

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
    // A worktree under `.claude/worktrees` is a nested checkout `-ff` would delete with whatever another session has not committed in it.
    run(
        Command::new("git"),
        &["clean", "-xffd", "--exclude", ".claude/worktrees"],
    )?;

    Ok(())
}
