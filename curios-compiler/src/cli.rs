//! The clap command-line surface: the `Cli` root, its `Mode` subcommands, and the
//! timeout parser. Parsing only — the dispatch on the parsed value lives in
//! `main.rs`.

use {
    clap::{Parser, Subcommand},
    std::{path::PathBuf, time::Duration},
};

fn parse_timeout(input: &str) -> Result<Duration, String> {
    input
        .parse::<u64>()
        .map(Duration::from_millis)
        .map_err(|error| format!("invalid timeout in milliseconds: {error}"))
}

#[derive(Debug, Subcommand)]
pub enum Mode {
    #[command(about = "Compile and execute the entrypoint")]
    Run {
        #[arg(value_name = "PATH", help = "Path to the .crs entrypoint file")]
        input_path: PathBuf,

        #[arg(
            trailing_var_arg = true,
            allow_hyphen_values = true,
            value_name = "ARGS",
            help = "Arguments passed to the program (read via /std/Proc/args)"
        )]
        args: Vec<String>,
    },

    #[command(about = "Type-check the entrypoint without executing")]
    Check {
        #[arg(value_name = "PATH", help = "Path to the .crs entrypoint file")]
        input_path: PathBuf,
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
}

#[derive(Debug, Parser)]
#[command(version, about)]
pub struct Cli {
    #[arg(long, default_value = "1000", value_name = "MILLIS", value_parser = parse_timeout, help = "Type-checker reduction timeout in milliseconds")]
    pub timeout: Duration,

    #[arg(
        long,
        value_name = "STAGES",
        num_args = 0..=1,
        default_missing_value = "text,core,ersd,ersd-optm,cont,cont-optm,wasm",
        help = "Print selected IRs to stderr (comma-separated: text,core,ersd,ersd-optm,cont,cont-optm,wasm; bare --print prints all)"
    )]
    pub print: Option<String>,

    #[command(subcommand)]
    pub mode: Mode,
}
