//! The clap command-line surface: the `Cli` root and its `Mode` subcommands.
//! Parsing only — the dispatch on the parsed value lives in `main.rs`.

use {
    clap::{Parser, Subcommand},
    curios_pipeline::Stage,
    std::{path::PathBuf, sync::LazyLock},
};

/// [`curios_pipeline::Stage::NAMES`] joined with `,`, computed once on first
/// use — the `--print` flag's default and help text.
static NAMES: LazyLock<String> = LazyLock::new(|| Stage::NAMES.join(","));

#[derive(Debug, Subcommand)]
pub(crate) enum Mode {
    #[command(about = "Compile and execute the entrypoint")]
    Run {
        #[arg(value_name = "PATH", help = "Path to the .crs entrypoint file")]
        input_path: PathBuf,

        #[arg(
            trailing_var_arg = true,
            allow_hyphen_values = true,
            value_name = "ARGS",
            help = "Arguments passed to the program (read via /std/proc/args)"
        )]
        args: Vec<String>,
    },

    #[command(about = "Compile the entrypoint to a native executable")]
    Compile {
        #[arg(value_name = "PATH", help = "Path to the .crs entrypoint file")]
        input_path: PathBuf,

        #[arg(
            short = 'o',
            long = "output",
            value_name = "PATH",
            help = "Write the executable to PATH (default: the input file stem)"
        )]
        output_path: Option<PathBuf>,
    },

    /// Present only in profiling builds: the mode exists exactly when the
    /// spans it collects do.
    #[cfg(feature = "profile")]
    #[command(about = "Profile one compilation and print per-span aggregates")]
    Profile {
        #[arg(value_name = "PATH", help = "Path to the .crs entrypoint file")]
        input_path: PathBuf,
    },
}

#[derive(Debug, Parser)]
#[command(version, about)]
pub(crate) struct Cli {
    #[arg(
        long,
        default_value_t = curios::DEFAULT_STEP_BUDGET,
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

    #[command(subcommand)]
    pub(crate) mode: Mode,
}
