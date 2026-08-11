//! The clap command-line surface: the `Cli` root and its `Mode` subcommands. Parsing only — the dispatch on the parsed value lives in `main.rs`.

use {
    clap::{Parser, Subcommand},
    curios_pipeline::Stage,
    std::{path::PathBuf, sync::LazyLock},
};

/// [`curios_pipeline::Stage::NAMES`] joined with `,`, computed once on first use — the `--print` flag's default and help text.
static NAMES: LazyLock<String> = LazyLock::new(|| Stage::NAMES.join(","));

#[derive(Debug, Subcommand)]
pub(crate) enum Mode {
    /// Three forms, dispatched lexically: no argument is the governing package's default executable, an identifier is one it declares by name, and anything ending in `.crs` or holding a path separator is a bare file — standalone everywhere, captured by no manifest.
    #[command(about = "Execute an executable, or a .crs file")]
    Run {
        #[arg(
            value_name = "TARGET",
            help = "A declared executable's name, or a path to a .crs file (default: the governing package's sole or `default` executable)"
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

    /// The same three forms `run` dispatches, for the same reason: what a bare invocation means inside a package should not depend on which subcommand asked.
    #[command(about = "Compile an executable, or a .crs file, to a native executable")]
    Compile {
        #[arg(
            value_name = "TARGET",
            help = "A declared executable's name, or a path to a .crs file (default: the governing package's sole or `default` executable)"
        )]
        target: Option<String>,

        #[arg(
            short = 'o',
            long = "output",
            value_name = "PATH",
            help = "Write the executable to PATH (default: the executable's declared name, or the input file's stem)"
        )]
        output_path: Option<PathBuf>,
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

        #[arg(
            long,
            help = "Start a library rather than a program (a package is one or the other until it declares otherwise)"
        )]
        lib: bool,
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

    /// Present only in profiling builds: the mode exists exactly when the spans it collects do.
    #[cfg(feature = "profile")]
    #[command(about = "Profile one compilation and print per-span aggregates")]
    Profile {
        #[arg(value_name = "PATH", help = "Path to the .crs entrypoint file")]
        input_path: PathBuf,
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
    #[arg(
        long,
        default_value_t = curios_pipeline::DEFAULT_STEP_BUDGET,
        value_name = "STEPS",
        help = "Reduction steps each declaration may spend while type checking"
    )]
    pub(crate) budget: u64,

    #[arg(
        long,
        value_name = "STAGES",
        num_args = 0..=1,
        require_equals = true,
        default_missing_value = NAMES.as_str(),
        help = format!(
            "Print selected IRs to stderr (comma-separated: {}; bare --print prints all)",
            *NAMES
        )
    )]
    pub(crate) print: Option<String>,

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
