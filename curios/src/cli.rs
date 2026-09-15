//! The clap command-line surface: the `Cli` root and its `Mode` subcommands. Parsing only — the dispatch on the parsed value lives in `main.rs`, and what a TARGET's help says is computed from its command's contract, so the help cannot describe an argument its command admits another way.

use {
    crate::{COMPILE, COST, DIAGNOSTICS, LINT, RUN, STAGE, TESTS},
    clap::{Args, Parser, Subcommand},
    curios_pipeline::Stage,
    std::{ffi::OsString, path::PathBuf, sync::LazyLock},
};

/// [`curios_pipeline::Stage::NAMES`] joined with `, `, computed once on first use — `wonder stage`'s help text.
static NAMES: LazyLock<String> = LazyLock::new(|| Stage::NAMES.join(", "));

/// Which manifest governs, for a command that resolves against a package.
#[derive(Debug, Args)]
pub(crate) struct ManifestFlag {
    /// The explicit override for scripting. It reaches only the governing package's manifest, never the umbrella question — see `documentation/usage.md`'s Which manifest governs.
    #[arg(
        long = "manifest",
        value_name = "PATH",
        help = "Use this curios.toml as the governing package's, instead of the working directory's"
    )]
    pub(crate) manifest: Option<PathBuf>,
}

/// What a command that elaborates reads: the work a declaration may spend, and which manifest governs.
#[derive(Debug, Args)]
pub(crate) struct Elaboration {
    #[arg(
        long,
        default_value_t = curios_pipeline::DEFAULT_STEP_BUDGET,
        value_name = "UNITS",
        help = "Units of reduction work each declaration may spend while type checking"
    )]
    pub(crate) budget: u64,

    #[command(flatten)]
    pub(crate) manifest: ManifestFlag,
}

#[derive(Debug, Subcommand)]
pub(crate) enum Mode {
    /// What the four forms mean is `documentation/usage.md`'s Running and compiling. The dispatch is lexical and probes no disk: the four spaces cannot overlap, so nothing here needs to look before deciding.
    #[command(about = "Execute an executable, a .crs file, or standard input")]
    Run {
        #[arg(value_name = "TARGET", help = RUN.target_help())]
        target: Option<String>,

        #[command(flatten)]
        elaboration: Elaboration,

        #[arg(
            trailing_var_arg = true,
            allow_hyphen_values = true,
            value_name = "ARGS",
            help = "Arguments passed to the program (read via /std/proc/args)"
        )]
        args: Vec<OsString>,
    },

    /// Dispatched through the same code as `run`, so the two cannot drift apart, and then narrowed to the named form: a built executable is filed under the package that declares it, and a loose file or standard input has no package to be filed under.
    #[command(about = "Compile a declared executable to a native executable")]
    Compile {
        #[arg(value_name = "TARGET", help = COMPILE.target_help())]
        target: Option<String>,

        #[arg(
            short = 'o',
            long = "output",
            value_name = "PATH",
            help = "Write the executable to PATH (default: under the store, beside the governing manifest)"
        )]
        output_path: Option<PathBuf>,

        #[command(flatten)]
        elaboration: Elaboration,
    },

    /// No target, or a file: a library is the one thing with an interface, and the governing package has at most one — the file form reads a unit already archived, a verdict slot's or the prelude image, which is how the standard library is documented without a package. Where the pages go is `documentation/usage.md`'s Documenting.
    #[command(about = "Write a library's interface as pages")]
    Document {
        #[arg(
            value_name = "FILE",
            help = "A file holding an archived unit: a verdict slot under a store, or the prelude image (default: the governing package's library)"
        )]
        target: Option<PathBuf>,

        #[arg(
            short = 'o',
            long = "output",
            value_name = "DIR",
            help = "Write the pages under DIR (default: under the store, beside the governing manifest; required for a file)"
        )]
        output_path: Option<PathBuf>,

        #[command(flatten)]
        elaboration: Elaboration,
    },

    /// Always the governing package entire, and the optional argument is a filter rather than a target — the reasoning is `documentation/usage.md`'s Testing.
    #[command(about = "Run the governing package's declared tests")]
    Test {
        #[arg(
            value_name = "FILTER",
            help = "A path prefix selecting which tests run, e.g. /app/Map (default: every test)"
        )]
        filter: Option<String>,

        #[command(flatten)]
        elaboration: Elaboration,
    },

    /// The store's tool, and the only thing in this toolchain that reaches the network. Acceptance is by hash, so what transport delivered the bytes does not matter — which is exactly why fetching can live in one place rather than being a capability the compiler carries.
    #[command(about = "Materialize what the manifests reference")]
    Curate {
        #[command(flatten)]
        manifest: ManifestFlag,
    },

    /// Last of the machinery rather than first: it writes what everything else reads, so it can only be right once there is something for it to be right about.
    #[command(about = "Start a package in DIR, named after it")]
    New {
        #[arg(
            value_name = "DIR",
            help = "The directory to create; its name is the package's"
        )]
        directory: PathBuf,
    },

    /// The gate over what `wonder diagnostics` reports: every diagnostic, goal and lint of the target, and — for the package entire — the dependencies nothing reached, with the exit code turning on them. The reasoning is `documentation/usage.md`'s Linting.
    #[command(
        about = "Report every unused import, binder, declaration and dependency; exit 1 when any"
    )]
    Lint {
        #[arg(value_name = "TARGET", help = LINT.target_help())]
        target: Option<String>,

        #[command(flatten)]
        elaboration: Elaboration,
    },

    #[command(about = "Format .crs files canonically, in place")]
    Format {
        #[arg(
            value_name = "PATHS",
            required = true,
            help = "The .crs files to format"
        )]
        paths: Vec<PathBuf>,

        #[arg(
            long,
            help = "Write nothing; exit nonzero when any file would change (for CI)"
        )]
        check: bool,
    },

    /// Questions about a program, answered by the compilation that would build it. The query is first and the target last, so `wonder diagnostics app` and `wonder stage core app` read as the sentence they are; `server` sits in the query position because it is the same questions asked over a protocol.
    #[command(about = "Ask what the compiler knows about a program")]
    Wonder {
        #[command(subcommand)]
        query: Query,
    },
}

/// One question each, of fixed arity. A target takes the four forms `run` takes, with the one difference `documentation/usage.md`'s Asking about a program states; the placement itself is `curios_package::Selection`'s.
#[derive(Debug, Subcommand)]
pub(crate) enum Query {
    #[command(
        about = "Every diagnostic and goal, located; exit 0 once answered, whatever the answer"
    )]
    Diagnostics {
        #[arg(value_name = "TARGET", help = DIAGNOSTICS.target_help())]
        target: Option<String>,

        #[command(flatten)]
        elaboration: Elaboration,
    },

    #[command(about = "Every test the target declares, one path per line; nothing executes")]
    Tests {
        #[arg(value_name = "TARGET", help = TESTS.target_help())]
        target: Option<String>,

        #[command(flatten)]
        elaboration: Elaboration,
    },

    /// What the optimizer did to each declaration, which is a question about the compilation and never about a run — so it sits here beside the other things the compiler already decided, rather than behind a flag on `run`.
    #[command(
        about = "What became of each declaration by the time the optimizer settled, one row per line"
    )]
    Cost {
        #[arg(value_name = "TARGET", help = COST.target_help())]
        target: Option<String>,

        #[command(flatten)]
        elaboration: Elaboration,
    },

    #[command(about = "The program's representation at one rung of the pipeline, reprinted")]
    Stage {
        #[arg(value_name = "STAGE", help = format!("One of: {}", *NAMES))]
        name: String,

        #[arg(value_name = "TARGET", help = STAGE.target_help())]
        target: Option<String>,

        #[command(flatten)]
        elaboration: Elaboration,
    },

    #[command(
        about = "Answer an editor over the language server protocol on standard input and output"
    )]
    Server {
        #[command(flatten)]
        elaboration: Elaboration,
    },
}

#[derive(Debug, Parser)]
// The stock template omits the version, so `--help` and `--version` answer different questions and a bug report quoting help output says nothing about which build produced it. Naming it here puts it on the one page anybody reads first.
#[command(
    version,
    about,
    help_template = "\
{name} {version}
{about-with-newline}
{usage-heading} {usage}

{all-args}{after-help}"
)]
pub(crate) struct Cli {
    /// `--budget` where it stood before it belonged to the commands that elaborate: parsed only to be refused with the spelling that works, and hidden, because it is no flag of this position.
    #[arg(long = "budget", value_name = "UNITS", hide = true)]
    pub(crate) misplaced_budget: Option<OsString>,

    /// `--manifest` where it stood before it belonged to the commands that resolve against a package, parsed and hidden for the same reason.
    #[arg(long = "manifest", value_name = "PATH", hide = true)]
    pub(crate) misplaced_manifest: Option<OsString>,

    // Present only in profiling builds, and inert until asked for: the feature compiles the instrumentation in, and this decides whether anything listens to it. Without that, a build with the feature on would record on *every* invocation — including the eight the integration suite spawns under `--all-features`, all of them onto one path.
    //
    // It takes the destination rather than defaulting to one, so no path is spelled in the compiler at all. The reader chooses where the stream goes and reads it back from there, which is one spelling instead of two that have to agree. Global, because what it measures is the invocation rather than one command, so it may stand on either side of the command.
    //
    // A plain comment rather than documentation, because clap prints a field's documentation of more than one paragraph as the flag's long help, and a global flag's help is printed under every command.
    #[cfg(feature = "profile")]
    #[arg(
        long = "profile",
        value_name = "PATH",
        global = true,
        help = "Write one record per span and event to PATH, rotating at 512 MiB"
    )]
    pub(crate) profile: Option<PathBuf>,

    #[command(subcommand)]
    pub(crate) mode: Mode,
}
