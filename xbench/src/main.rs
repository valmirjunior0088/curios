//! The benchmark bench, as a cargo subcommand, and the timings it has taken, each a TOML file under `xbench/timings/` that the build embeds.
//!
//! `cargo xbench collect` builds Curios and Rust on every workload of the corpus, holds all three contestants to the answers those workloads are known to have, times them interleaved, and prints the timing as the TOML file that records it — standard output carries the timing alone, so `cargo xbench collect > xbench/timings/NN.toml` files a capture and nothing else. `cargo xbench report` computes what the timings say; it runs anywhere.
//!
//! It reads in the order a sitting uses it:
//!
//! - [`held`] holds every tool to the pin the file that owns it states, before anything is built.
//! - [`capture`] asks the host where the timing is being taken.
//! - [`collect`] builds the contestants, holds them to the answers their workloads are known to have, times them interleaved, and prints a [`Timing`] as the file that records it.
//! - [`report()`] computes every median, span, ratio and refusal from the timings, since none of them is stored.

#[cfg(test)]
mod tests;

mod platform;
use platform::*;

mod timing;
use timing::*;

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

/// Every timing filed, oldest first: a timing's number is its index here and its file's name.
///
/// The bench begins with none, as the board began with no ticket. A timing is filed from what a sitting printed, never written by hand, and the ones a vanished harness took are not carried in: none was taken under a protocol this bench records, so none could be compared with a timing taken now.
fn timings() -> Result<Vec<Timing>, String> {
    FILED
        .iter()
        .enumerate()
        .map(|(number, text)| {
            Timing::read(text).map_err(|why| format!("timing {number:02} is malformed: {why}"))
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
        about = "Take a timing: build the contestants, hold them to their answers, and time them"
    )]
    Collect,

    #[command(about = "Print what the timings say, or one timing in full")]
    Report {
        #[arg(value_name = "TIMING", help = "The timing to print in full")]
        timing: Option<usize>,
    },
}

fn main() -> ExitCode {
    let command = Cli::parse().command;
    let outcome = timings().and_then(|timings| match command {
        // The next timing is numbered after the ones already filed, which is something only the binary knows.
        Bench::Collect => collect(timings.len()),
        Bench::Report { timing: None } => {
            print!("{}", report(&timings));
            Ok(())
        }
        Bench::Report {
            timing: Some(number),
        } => match timings.get(number) {
            Some(timing) => {
                print!("{}", one(number, timing));
                Ok(())
            }
            None => Err(format!("there is no timing {number:02}")),
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
