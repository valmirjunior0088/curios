//! The benchmark bench, as a cargo subcommand, and the readings it has taken, one a module beside this one.
//!
//! `cargo xbench collect` builds Curios and Rust on every workload of the corpus, holds all three contestants to the answers those workloads are known to have, times them interleaved, and prints the reading as the module that records it — standard output carries the module alone, so `cargo xbench collect > xbench/src/reading_NN.rs` files a capture and nothing else. `cargo xbench report` computes what the readings say; it runs anywhere.

#[cfg(test)]
mod tests;

mod support;
use support::*;

mod reading_00;
use reading_00::*;

mod reading_01;
use reading_01::*;

mod reading_02;
use reading_02::*;

mod reading_03;
use reading_03::*;

mod reading_04;
use reading_04::*;

use {
    clap::{Parser, Subcommand},
    std::process::ExitCode,
};

/// Every reading taken, oldest first: a reading's number is its index here.
///
/// The bench begins with none, as the board began with no ticket. A reading is filed from what a sitting printed, never written by hand, and the ones a vanished harness took are not carried in: none was taken under a protocol this bench records, so none could be compared with a reading taken now.
const READINGS: &[&Reading] = &[
    &READING_00,
    &READING_01,
    &READING_02,
    &READING_03,
    &READING_04,
];

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
        Bench::Collect => collect(READINGS.len()),
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
