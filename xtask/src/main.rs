//! The repository's steps, as `cargo xtask <subject> [action]`.
//!
//! **A step is an action on a subject, and the table of them is the one thing authored.** [`steps`] states every step once: what it is about, what it does, what it is run after, what it needs and the one thing only it catches. A subject's actions, a bare subject's sequence, the gate's order and what the check workflow must run are read off that table, so `/run-full-gate`, the workflow and a contributor run one spelling of each step and no two of them can drift.
//!
//! **A subject comes first, because a person works on one thing.** `cargo xtask vscode` runs everything about the VS Code extension, each step writing a log of its own; `cargo xtask vscode test` runs one action and streams it. An action a subject does not have is the parser's own error. `cargo xtask gate` is every subject.
//!
//! **A step is cargo with flags, and whatever cargo does not do.** Every step spawns `cargo` — or npm, in an editor tree — as a separate process, and then copies a file or generates the browser bindings, where there is such a thing to do. Nothing is a build script: a build script runs before its crate compiles and so cannot post-process that crate's output, and a nested `cargo` inside one contends for the target-directory lock. A process that `cargo run` has already launched holds no lock, so its nested builds are ordinary.
//!
//! **A step takes no arguments it passes on.** An action may take a parameter it places itself — a package to narrow a check to, a name to narrow a test run by, a release's version — but never a tail it hands to the tool unread.
//!
//! **The launcher's isolation is the spawn.** `launcher build` builds `curios-runtime` in its own `cargo` invocation, so workspace feature unification cannot reach it. `curios/build.rs` embeds what the step copies to `curios/.artifacts/<triple>` and refuses to build without it, and every step that builds the compiler has the launcher's build run first: it costs nothing when nothing changed, since cargo decides whether the launcher needs rebuilding and [`file_with_inputs()`](filing::file_with_inputs) skips the copy when the filed bytes are already the built ones.
//!
//! **The bindings generator is a dependency.** `bundle build` calls `wasm-bindgen-cli-support`, the crate the `wasm-bindgen` command line wraps; why, and what keeps its version honest, is the README's decision.
//!
//! **The test runner is a tool a contributor installs.** A test step runs under `cargo-nextest` and refuses before building when it is absent, naming the command that installs it; why nextest is the README's decision.
//!
//! **Four commands are not steps.** `installer` renders the release's install script, `release` cuts a release, `profile` and `profile-prelude` run a build under a recorder, and `clean` removes what git does not track. None is part of the gate, so none has a row; each keeps the parameters it had.
//!
//! **A dependency of nothing.** This crate is reached only through the `xtask` alias in `.cargo/config.toml`, and no crate may depend on it: its dependency tree exists to build the workspace, not to be part of it.

mod places;
use places::*;

mod tools;
use tools::*;

mod commands;
use commands::*;

mod filing;
use filing::*;

mod trees;
use trees::*;

mod steps;
use steps::*;

mod subjects;
use subjects::*;

mod recipes;
use recipes::*;

mod gate;
use gate::*;

mod installer;
use installer::*;

mod release;
use release::*;

use {
    clap::{CommandFactory, FromArgMatches, Parser, Subcommand, ValueEnum},
    std::{path::PathBuf, process::ExitCode},
};

/// The base a step's path condition is read against where the gate is not told one: local `main`, which is where a branch started and, on `main` itself, what nothing has changed since.
const BASE: &str = "main";

#[derive(Debug, Parser)]
#[command(
    name = "cargo xtask",
    bin_name = "cargo xtask",
    version,
    about = "The repository's steps: a subject, then an action of it",
    help_template = "\
{name} {version}
{about-with-newline}
{usage-heading} {usage}

{all-args}{after-help}"
)]
struct Cli {
    #[command(subcommand)]
    asked: Asked,
}

#[derive(Debug, Subcommand)]
enum Asked {
    #[command(flatten)]
    About(About),

    #[command(
        about = "Run every step, in the order the table derives, each writing target/gate/<step>.log"
    )]
    Gate {
        #[arg(
            long,
            value_name = "REVISION",
            default_value = BASE,
            help = "The revision a step's path condition is read against"
        )]
        base: String,
    },

    /// One step by its row's name: how the gate starts each step as a process of its own. Hidden, since a person types the subject and the action.
    #[command(hide = true)]
    Step {
        #[arg(value_enum)]
        step: Step,
    },

    #[command(
        about = "Render the installer script for one release version under xtask/.artifacts/install.sh"
    )]
    Installer {
        #[arg(
            value_name = "VERSION",
            help = "The release's version: the part of its tag after release/"
        )]
        version: String,
    },

    #[command(
        about = "Bump the workspace version, commit it, tag the release, and push main and the tag"
    )]
    Release {
        #[arg(
            value_name = "BUMP",
            help = "patch, minor, major, or the version itself; omitted, the three candidates are named"
        )]
        bump: Option<String>,
    },

    #[command(about = "Run one program under a profiling build, and fold what it filed")]
    Profile {
        #[arg(
            value_name = "PATH",
            default_value = "programs/hello_world.crs",
            help = "Path to the .crs entrypoint file"
        )]
        source: PathBuf,

        #[arg(
            long,
            value_enum,
            default_value_t = BuildProfile::Debug,
            help = "The cargo profile the compiler is built under"
        )]
        profile: BuildProfile,
    },

    #[command(
        about = "Run one half of the prelude's build under a recorder, and fold what it filed"
    )]
    ProfilePrelude {
        #[arg(value_enum, value_name = "HALF", help = "The half of the build to run")]
        half: PreludeHalf,

        #[arg(value_name = "PATH", help = "Where the stream is filed")]
        stream: PathBuf,
    },

    #[command(about = "Remove everything git does not track, including the build products")]
    Clean,
}

/// A subject, bare or with one action of it. Each is described by the table ([`described`]), so none carries a description here.
#[derive(Debug, Subcommand)]
enum About {
    Workspace {
        #[command(subcommand)]
        action: Option<WorkspaceAction>,
    },

    Instrumented {
        #[command(subcommand)]
        action: Option<InstrumentedAction>,
    },

    Compiler {
        #[command(subcommand)]
        action: Option<CompilerAction>,
    },

    Launcher {
        #[command(subcommand)]
        action: Option<LauncherAction>,
    },

    Bundle {
        #[command(subcommand)]
        action: Option<BundleAction>,
    },

    Docs {
        #[command(subcommand)]
        action: Option<DocsAction>,
    },

    Grammar {
        #[command(subcommand)]
        action: Option<GrammarAction>,
    },

    Vscode {
        #[command(subcommand)]
        action: Option<VscodeAction>,
    },

    Zed {
        #[command(subcommand)]
        action: Option<ZedAction>,
    },
}

/// The steps a subject's command line asks for: every step about it where it stands bare, and otherwise one action's, with what the person narrowed it by.
fn selected<A: Asks>(subject: Subject, action: Option<A>) -> (Vec<Step>, Option<Request>) {
    match action {
        None => (Step::about(subject), None),
        Some(action) => {
            let (action, request) = action.asked();

            (Step::of(subject, action), Some(request))
        }
    }
}

impl About {
    /// The steps the command line asks for, and what narrows them where it names an action.
    fn steps(self) -> (Vec<Step>, Option<Request>) {
        match self {
            About::Workspace { action } => selected(Subject::Workspace, action),
            About::Instrumented { action } => selected(Subject::Instrumented, action),
            About::Compiler { action } => selected(Subject::Compiler, action),
            About::Launcher { action } => selected(Subject::Launcher, action),
            About::Bundle { action } => selected(Subject::Bundle, action),
            About::Docs { action } => selected(Subject::Docs, action),
            About::Grammar { action } => selected(Subject::Grammar, action),
            About::Vscode { action } => selected(Subject::Vscode, action),
            About::Zed { action } => selected(Subject::Zed, action),
        }
    }
}

/// The command line with each subject and each action described by the table: a subject by the steps it has, an action by what its steps do.
fn described() -> clap::Command {
    let mut command = Cli::command();

    for subject in Subject::value_variants() {
        let about = format!(
            "{}: every step about it, each writing a log, or one action of it",
            subject.is()
        );
        command = command.mut_subcommand(spelled(*subject), |described| {
            let mut described = described.about(about);
            for action in Action::value_variants() {
                let steps = Step::of(*subject, *action);
                if steps.is_empty() {
                    continue;
                }

                let does = steps
                    .iter()
                    .map(|step| step.row().does)
                    .collect::<Vec<_>>()
                    .join("; ");
                described = described.mut_subcommand(spelled(*action), |action| action.about(does));
            }

            described
        });
    }

    command
}

fn main() -> ExitCode {
    let asked = described().get_matches();
    let asked = Cli::from_arg_matches(&asked)
        .unwrap_or_else(|error| error.exit())
        .asked;

    // A bare subject is many steps, each a process with a log of its own; one action is streamed.
    let outcome = match asked {
        Asked::About(about) => match about.steps() {
            (steps, None) => gate(&steps, BASE),
            (steps, Some(request)) => perform(&steps, &request, BASE),
        },
        Asked::Gate { base } => gate(Step::value_variants(), &base),
        Asked::Step { step } => step.run(&Request::whole()),
        Asked::Installer { version } => installer(&version),
        Asked::Release { bump } => release(bump.as_deref()),
        Asked::Profile {
            source,
            profile: build,
        } => profile(&source, build),
        Asked::ProfilePrelude { half, stream } => profile_prelude(half, &stream),
        Asked::Clean => clean(),
    };

    match outcome {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("{message}");

            ExitCode::FAILURE
        }
    }
}
