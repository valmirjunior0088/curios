//! Developer-facing entrypoint for profiling one Curios compilation.
//!
//! Profiles `compile_entrypoint`. To profile a path it does not already
//! exercise, point `run` at the relevant entrypoint for the duration of the
//! investigation, then restore it.

use {
    clap::Parser,
    curios::{DEFAULT_STEP_BUDGET, ProfileReport, capture},
    curios_pipeline::compile_entrypoint,
    std::{path::PathBuf, process::ExitCode},
};

#[derive(Parser)]
#[command(about = "Profile one Curios compilation")]
struct Args {
    /// Curios entrypoint to compile.
    input_path: PathBuf,

    /// Reduction steps each declaration may spend while type checking.
    #[arg(long, default_value_t = DEFAULT_STEP_BUDGET)]
    budget: u64,
}

fn run() -> Result<(), String> {
    let Args { input_path, budget } = Args::parse();
    let (entrypoint, loader) = curios::load(&input_path)?;

    let (compilation, report) = capture(|| compile_entrypoint(budget, &entrypoint, loader, |_| {}));

    print_report(&report);
    compilation.map(|_| ())
}

fn print_report(report: &ProfileReport) {
    println!("total_ms\tcalls\tmin_ms\tmax_ms\ttarget\tname");

    for summary in report.summaries() {
        println!(
            "{:.3}\t{}\t{:.3}\t{:.3}\t{}\t{}",
            summary.total().as_secs_f64() * 1_000.0,
            summary.calls(),
            summary.min().as_secs_f64() * 1_000.0,
            summary.max().as_secs_f64() * 1_000.0,
            summary.target(),
            summary.name(),
        );
    }
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error}");

            ExitCode::FAILURE
        }
    }
}
