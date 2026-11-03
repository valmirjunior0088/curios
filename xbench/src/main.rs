//! The benchmark bench, as a cargo subcommand, and the readings it has taken, each a TOML file under `xbench/readings/` that the build embeds.
//!
//! `cargo xbench collect` builds Curios and Rust on every workload of the corpus, holds all three contestants to the answers those workloads are known to have, times them interleaved, and prints the reading as the TOML file that records it — standard output carries the reading alone, so `cargo xbench collect > xbench/readings/NN.toml` files a capture and nothing else. `cargo xbench report` computes what the readings say; it runs anywhere.
//!
//! It reads in the order a sitting uses it:
//!
//! - [`held`] holds every tool to the pin the file that owns it states, before anything is built.
//! - [`capture`] asks the host where the reading is being taken.
//! - [`collect`] builds the contestants, holds them to the answers their workloads are known to have, times them interleaved, and prints a [`Reading`] as the file that records it.
//! - [`report()`] computes every median, span, ratio and refusal from the readings, since none of them is stored.

#[cfg(test)]
mod tests;

mod platform;
use platform::*;

mod reading;
use reading::*;

mod pins;
use pins::*;

mod contest;
use contest::*;

mod report;
use report::*;

use {
    clap::{Parser, Subcommand},
    std::process::ExitCode,
};

include!(concat!(env!("OUT_DIR"), "/filed.rs"));

/// Every reading filed, oldest first: a reading's number is its index here and its file's name.
///
/// The bench begins with none, as the board began with no ticket. A reading is filed from what a sitting printed, never written by hand, and the ones a vanished harness took are not carried in: none was taken under a protocol this bench records, so none could be compared with a reading taken now.
fn readings() -> Result<Vec<Reading>, String> {
    FILED
        .iter()
        .enumerate()
        .map(|(number, text)| {
            Reading::read(text).map_err(|why| format!("reading {number:02} is malformed: {why}"))
        })
        .collect()
}

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
    let command = Cli::parse().command;
    let outcome = readings().and_then(|readings| match command {
        // The next reading is numbered after the ones already filed, which is something only the binary knows.
        Bench::Collect => collect(readings.len()),
        Bench::Report { reading: None } => {
            print!("{}", report(&readings));
            Ok(())
        }
        Bench::Report {
            reading: Some(number),
        } => match readings.get(number) {
            Some(reading) => {
                print!("{}", one(number, reading));
                Ok(())
            }
            None => Err(format!("there is no reading {number:02}")),
        },
    });

    match outcome {
        Ok(()) => ExitCode::SUCCESS,
        Err(why) => {
            eprintln!("{why}");
            ExitCode::FAILURE
        }
    }
}
