//! The clap command-line surface: the `Cli` root and its `Mode` subcommands. Parsing only — the dispatch on the parsed value lives in `main.rs`.

use {
    clap::{Parser, Subcommand},
    curios_pipeline::Stage,
    std::{ffi::OsString, path::PathBuf, sync::LazyLock},
};

/// [`curios_pipeline::Stage::NAMES`] joined with `, `, computed once on first use — `wonder stage`'s help text.
static NAMES: LazyLock<String> = LazyLock::new(|| Stage::NAMES.join(", "));

/// What a TARGET names, for every subcommand that takes one and means the governing package's sole or `default` executable by none. The lexical rule is `curios_package::Form`'s; the sentence is written once so five subcommands cannot describe it five ways.
const TARGET_HELP: &str = "A declared executable's name, a path to a .crs file, or `-` for standard input (default: the governing package's sole or `default` executable)";

/// The same, for a query that takes the governing package entire when nothing is named.
const TARGET_HELP_PACKAGE: &str = "A declared executable's name, a path to a .crs file, or `-` for standard input (default: the governing package entire)";

/// The named form alone, for the one subcommand that writes a product and so needs a package to file it under.
const TARGET_HELP_EXECUTABLE: &str =
    "A declared executable's name (default: the governing package's sole or `default` executable)";

#[derive(Debug, Subcommand)]
pub(crate) enum Mode {
    /// What the four forms mean is `documentation/usage.md`'s Running and compiling. The dispatch is lexical and probes no disk: the four spaces cannot overlap, so nothing here needs to look before deciding.
    #[command(about = "Execute an executable, a .crs file, or standard input")]
    Run {
        #[arg(
            value_name = "TARGET",
            help = TARGET_HELP
        )]
        target: Option<String>,

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
        #[arg(
            value_name = "TARGET",
            help = TARGET_HELP_EXECUTABLE
        )]
        target: Option<String>,

        #[arg(
            short = 'o',
            long = "output",
            value_name = "PATH",
            help = "Write the executable to PATH (default: under the store, beside the governing manifest)"
        )]
        output_path: Option<PathBuf>,
    },

    /// Always the governing package entire, and the optional argument is a filter rather than a target — the reasoning is `documentation/usage.md`'s Testing.
    #[command(about = "Run the governing package's declared tests")]
    Test {
        #[arg(
            value_name = "FILTER",
            help = "A path prefix selecting which tests run, e.g. /app/Map (default: every test)"
        )]
        filter: Option<String>,
    },

    /// The store's tool, and the only thing in this toolchain that reaches the network. Acceptance is by hash, so what transport delivered the bytes does not matter — which is exactly why fetching can live in one place rather than being a capability the compiler carries.
    #[command(about = "Materialize what the manifests reference")]
    Curate,

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
        #[arg(
            value_name = "TARGET",
            help = TARGET_HELP_PACKAGE
        )]
        target: Option<String>,
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

    /// Present only in profiling builds: the mode exists exactly when the spans it collects do.
    #[cfg(feature = "profile")]
    #[command(about = "Profile one compilation and print per-span aggregates")]
    Profile {
        #[arg(value_name = "PATH", help = "Path to the .crs entrypoint file")]
        input_path: PathBuf,
    },
}

/// One question each, of fixed arity. A target takes the four forms `run` takes, with the one difference `documentation/usage.md`'s Asking about a program states; the placement itself is `curios_package::Membership`.
#[derive(Debug, Subcommand)]
pub(crate) enum Query {
    #[command(
        about = "Every diagnostic and goal, located; exit 0 once answered, whatever the answer"
    )]
    Diagnostics {
        #[arg(
            value_name = "TARGET",
            help = TARGET_HELP_PACKAGE
        )]
        target: Option<String>,
    },

    #[command(about = "Every test the target declares, one path per line; nothing executes")]
    Tests {
        #[arg(
            value_name = "TARGET",
            help = TARGET_HELP_PACKAGE
        )]
        target: Option<String>,
    },

    #[command(about = "The program's representation at one rung of the pipeline, reprinted")]
    Stage {
        #[arg(value_name = "STAGE", help = format!("One of: {}", *NAMES))]
        name: String,

        #[arg(
            value_name = "TARGET",
            help = TARGET_HELP
        )]
        target: Option<String>,
    },

    #[command(
        about = "Answer an editor over the language server protocol on standard input and output"
    )]
    Server,
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
    #[arg(
        long,
        default_value_t = curios_pipeline::DEFAULT_STEP_BUDGET,
        value_name = "UNITS",
        help = "Units of reduction work each declaration may spend while type checking"
    )]
    pub(crate) budget: u64,

    /// The order these arrive in *is* the dependency order — nothing here resolves or sorts, which is what keeps this a hand-written stand-in for the manifest rather than a small one. What the flag deliberately does *not* take is a prefix: the directory holds the package's own `curios.toml`, and a package's name is declared there and nowhere else.
    #[arg(
        long = "unit",
        value_name = "DIR",
        help = "Mount the package in DIR before the entry program; repeat for more, in dependency order"
    )]
    pub(crate) units: Vec<PathBuf>,

    /// The explicit override for scripting. It reaches only the governing package's manifest, never the umbrella question — see `documentation/usage.md`'s Which manifest governs.
    #[arg(
        long = "manifest",
        value_name = "PATH",
        help = "Use this curios.toml as the governing package's, instead of the working directory's"
    )]
    pub(crate) manifest: Option<PathBuf>,

    #[command(subcommand)]
    pub(crate) mode: Mode,
}
