//! Developer-facing entrypoint for profiling one Curios compilation.
//!
//! Profiles `compile_entrypoint`. To profile a path it does not already
//! exercise, point `run` at the relevant entrypoint for the duration of the
//! investigation, then restore it.

use {
    clap::Parser,
    curios::{ProfileReport, capture},
    curios_pipeline::compile_entrypoint,
    std::{path::PathBuf, process::ExitCode, time::Duration},
};

#[derive(Parser)]
#[command(about = "Profile one Curios compilation")]
struct Args {
    /// Curios entrypoint to compile.
    input_path: PathBuf,

    /// Compiler timeout in seconds.
    #[arg(long, default_value_t = 300)]
    timeout: u64,
}

fn run() -> Result<(), String> {
    let Args {
        input_path,
        timeout,
    } = Args::parse();
    let (entrypoint, loader) = curios::load(&input_path)?;

    let (compilation, report) =
        capture(|| compile_entrypoint(Duration::from_secs(timeout), &entrypoint, loader, |_| {}));

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
