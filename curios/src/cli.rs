//! The clap command-line surface: the `Cli` root and its `Mode` subcommands. Parsing only — the dispatch on the parsed value lives in `main.rs`.

use {
    clap::{Parser, Subcommand},
    curios_pipeline::Stage,
    std::{path::PathBuf, sync::LazyLock},
};

/// [`curios_pipeline::Stage::NAMES`] joined with `, `, computed once on first use — `wonder stage`'s help text.
static NAMES: LazyLock<String> = LazyLock::new(|| Stage::NAMES.join(", "));

#[derive(Debug, Subcommand)]
pub(crate) enum Mode {
    /// Four forms, dispatched lexically: no argument is the governing package's default executable, an identifier is one it declares by name, `-` is the program on standard input, and anything ending in `.crs` or holding a path separator is a bare file — the last two standalone everywhere, captured by no manifest.
    #[command(about = "Execute an executable, a .crs file, or standard input")]
    Run {
        #[arg(
            value_name = "TARGET",
            help = "A declared executable's name, a path to a .crs file, or `-` for standard input (default: the governing package's sole or `default` executable)"
        )]
        target: Option<String>,

        #[arg(
            trailing_var_arg = true,
            allow_hyphen_values = true,
            value_name = "ARGS",
            help = "Arguments passed to the program (read via /std/proc/args)"
        )]
        args: Vec<String>,
    },

    /// The same four forms `run` dispatches, for the same reason: what a bare invocation means inside a package should not depend on which subcommand asked.
    #[command(
        about = "Compile an executable, a .crs file, or standard input, to a native executable"
    )]
    Compile {
        #[arg(
            value_name = "TARGET",
            help = "A declared executable's name, a path to a .crs file, or `-` for standard input (default: the governing package's sole or `default` executable)"
        )]
        target: Option<String>,

        #[arg(
            short = 'o',
            long = "output",
            value_name = "PATH",
            help = "Write the executable to PATH (default: the executable's declared name, or the input file's stem; required for `-`)"
        )]
        output_path: Option<PathBuf>,
    },

    /// Always the governing package entire — its library, then each executable — because a test's identity is its path, and a path means the same thing whichever subcommand asks. The optional argument is a filter, not a target.
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

/// One question each, of fixed arity. A target takes the four forms `run` takes, dispatched lexically — but a file is placed in the unit that declares it rather than compiled alone, since nothing here executes and what is at stake is only whether the answer is true (see `curios_package::Membership`).
#[derive(Debug, Subcommand)]
pub(crate) enum Query {
    #[command(
        about = "Every diagnostic and goal, located; exit 0 once answered, whatever the answer"
    )]
    Diagnostics {
        #[arg(
            value_name = "TARGET",
            help = "A declared executable's name, a path to a .crs file, or `-` for standard input (default: the governing package entire)"
        )]
        target: Option<String>,
    },

    #[command(about = "Every test the target declares, one path per line; nothing executes")]
    Tests {
        #[arg(
            value_name = "TARGET",
            help = "A declared executable's name, a path to a .crs file, or `-` for standard input (default: the governing package entire)"
        )]
        target: Option<String>,
    },

    #[command(about = "The program's representation at one rung of the pipeline, reprinted")]
    Stage {
        #[arg(value_name = "STAGE", help = format!("One of: {}", *NAMES))]
        name: String,

        #[arg(
            value_name = "TARGET",
            help = "A declared executable's name, a path to a .crs file, or `-` for standard input (default: the governing package's sole or `default` executable)"
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

    /// The explicit override for scripting. It overrides exactly which manifest is the package's: which umbrella governs is still enumeration's answer, because a manifest cannot declare itself governed.
    #[arg(
        long = "manifest",
        value_name = "PATH",
        help = "Use this curios.toml as the governing package's, instead of the working directory's"
    )]
    pub(crate) manifest: Option<PathBuf>,

    #[command(subcommand)]
    pub(crate) mode: Mode,
}
