//! The benchmark bench, as a cargo subcommand, and the records it has filed, each a TOML file under `xbench/records/<measurement>/` that the build embeds.
//!
//! `cargo xbench measure contest` builds Curios and Rust on every workload of the corpus, holds all three contestants to the answers those workloads are known to have, times them interleaved, and files the sitting as the next record of the contest; `cargo xbench measure <NAME>` takes any other, one of the bench's [`measurements`], by its name, sitting it in a release build of the bench a process a group. `cargo xbench report <MEASUREMENT>` computes what a measurement's records say; it runs anywhere.
//!
//! It reads in the order a sitting uses it:
//!
//! - [`held`] holds every tool to the pin the file that owns it states, before anything is built.
//! - [`capture`] asks the host where the record is being taken.
//! - [`contest_sitting`] builds the contestants, holds them to the answers their workloads are known to have, times them interleaved, and files a [`Record`]; [`measurement_sitting`] builds the bench in release and sits a measurement in it, a process a group.
//! - [`report()`] and [`figures_report`] compute every median, span, ratio and refusal from the records, since none of them is stored.

#[cfg(test)]
mod tests;

mod pins;
use pins::*;

mod sitting;
use sitting::*;

mod contest;
use contest::*;

mod measurements;
use measurements::*;

mod runner;
use runner::*;

mod report;
use report::*;

use {
    clap::{Parser, Subcommand},
    curios_profile::CountingAllocator,
    std::process::ExitCode,
    xbench::{Figure, Line, Measure, Pin, Record, capture},
};

// A program installs the counting allocator, and the bench is the one its measurements run in: what they read of memory is what it counts.
#[global_allocator]
static ALLOCATOR: CountingAllocator = CountingAllocator;

include!(concat!(env!("OUT_DIR"), "/filed.rs"));

/// The measurement whose records the contest files, the one the bench takes in the process it was asked in: every other is sat in a release build of the bench.
const CONTEST: &str = "contest";

/// Every record of `measurement` filed, oldest first: a record's number is its index here and its file's name.
///
/// A measurement begins with none, as the board began with no ticket. A record is filed by the sitting that took it, never written by hand, and the ones a vanished harness took are not carried in: none was taken under a protocol this bench records, so none could be compared with a record taken now.
fn records(measurement: &str) -> Result<Vec<Record>, String> {
    FILED
        .iter()
        .filter(|(filed, _)| *filed == measurement)
        .flat_map(|(_, texts)| texts.iter())
        .enumerate()
        .map(|(number, text)| {
            Record::read(text)
                .map_err(|why| format!("record {number:02} of {measurement} is malformed: {why}"))
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
    #[command(about = "Take a sitting of a measurement and file it as its next record")]
    Measure {
        #[arg(
            value_name = "MEASUREMENT",
            help = "The measurement to take: `contest`, Curios and Rust on the corpus, or another of the bench's by its name"
        )]
        measurement: String,
    },

    // What a sitting runs in each process it starts, which nobody asks for by hand.
    #[command(hide = true, about = "Take a measurement once, printing its readings")]
    Sit {
        #[arg(value_name = "MEASUREMENT", help = "The measurement to take")]
        measurement: String,
    },

    #[command(about = "Print what a measurement's records say, or one record in full")]
    Report {
        #[arg(
            value_name = "MEASUREMENT",
            help = "The measurement whose records to read"
        )]
        measurement: String,
        #[arg(value_name = "RECORD", help = "The record to print in full")]
        record: Option<usize>,
    },
}

fn main() -> ExitCode {
    let outcome = match Cli::parse().command {
        // The next record is numbered after the ones already filed, which is something only the binary knows.
        Bench::Measure { measurement } => {
            records(&measurement).and_then(|filed| match measurement.as_str() {
                CONTEST => contest_sitting(filed.len()),
                _ => measurement_sitting(&measurement, filed.len()),
            })
        }
        Bench::Sit { measurement } => {
            measurement_named(&measurement).map(|measurement| (measurement.take)())
        }
        Bench::Report {
            measurement,
            record: None,
        } => records(&measurement).map(|filed| match measurement.as_str() {
            CONTEST => print!("{}", report(&filed)),
            _ => print!("{}", figures_report(&measurement, &filed)),
        }),
        Bench::Report {
            measurement,
            record: Some(number),
        } => records(&measurement).and_then(|filed| {
            match (filed.get(number), measurement.as_str()) {
                (Some(record), CONTEST) => {
                    print!("{}", one(number, record));
                    Ok(())
                }
                (Some(record), _) => {
                    print!("{}", figures_one(number, record));
                    Ok(())
                }
                (None, _) => Err(format!("there is no record {number:02} of {measurement}")),
            }
        }),
    };

    match outcome {
        Ok(()) => ExitCode::SUCCESS,
        Err(why) => {
            eprintln!("{why}");
            ExitCode::FAILURE
        }
    }
}
