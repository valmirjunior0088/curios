//! The benchmark bench, as a cargo subcommand, and the readings it has taken, under `readings/`.
//!
//! `cargo xbench collect` builds Curios and Rust on every workload of the corpus, holds all three contestants to the answers those workloads are known to have, times them interleaved, and prints the reading as the module that records it — standard output carries the module alone, so `cargo xbench collect > xbench/src/readings/reading_NN.rs` files a capture and nothing else. `cargo xbench report` computes what the readings say; it runs anywhere.

mod readings;
use readings::*;

use {
    clap::{Parser, Subcommand},
    std::process::ExitCode,
    xbench::{one, report},
};

#[derive(Debug, Parser)]
#[command(
    name = "cargo xbench",
    bin_name = "cargo xbench",
    version,
    about = "The benchmark bench",
    help_template = "\
{name} {version}
{about-with-newline}
{usage-heading} {usage}

{all-args}{after-help}"
)]
struct Cli {
    #[command(subcommand)]
    command: Bench,
}

#[derive(Debug, Subcommand)]
enum Bench {
    #[command(
        about = "Take a reading: build the contestants, hold them to their answers, and time them"
    )]
    Collect,

    #[command(about = "Print what the readings say, or one reading in full")]
    Report {
        #[arg(value_name = "READING", help = "The reading to print in full")]
        reading: Option<usize>,
    },
}

fn main() -> ExitCode {
    let outcome = match Cli::parse().command {
        // The next reading is numbered after the ones already filed, which is something only the binary knows.
        Bench::Collect => xbench::collect(READINGS.len()),
        Bench::Report { reading: None } => {
            print!("{}", report(READINGS));
            Ok(())
        }
        Bench::Report {
            reading: Some(number),
        } => match READINGS.get(number) {
            Some(reading) => {
                print!("{}", one(number, reading));
                Ok(())
            }
            None => Err(format!("there is no reading {number:02}")),
        },
    };

    match outcome {
        Ok(()) => ExitCode::SUCCESS,
        Err(why) => {
            eprintln!("{why}");
            ExitCode::FAILURE
        }
    }
}
