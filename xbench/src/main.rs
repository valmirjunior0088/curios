//! The benchmark bench, as a cargo subcommand, and the records it has filed, each a TOML file under `xbench/records/<measurement>/` that the build embeds.
//!
//! `cargo xbench measure contest` builds Curios and Rust on every workload of the corpus, holds all three contestants to the answers those workloads are known to have, times them interleaved, and files the sitting as the next record of the contest. `cargo xbench report contest` computes what the contest's records say; it runs anywhere.
//!
//! It reads in the order a sitting uses it:
//!
//! - [`held`] holds every tool to the pin the file that owns it states, before anything is built.
//! - [`capture`] asks the host where the record is being taken.
//! - [`measure`] builds the contestants, holds them to the answers their workloads are known to have, times them interleaved, and files a [`Record`].
//! - [`report()`] computes every median, span, ratio and refusal from the records, since none of them is stored.

#[cfg(test)]
mod tests;

mod pins;
use pins::*;

mod contest;
use contest::*;

mod report;
use report::*;

use {
    clap::{Parser, Subcommand},
    std::process::ExitCode,
    xbench::{Figure, Pin, Record, Unit, capture},
};

include!(concat!(env!("OUT_DIR"), "/filed.rs"));

/// The measurement whose records the contest files, and the one the bench takes itself: every other measurement is a test beside the code it measures.
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
            help = "The measurement to take: `contest`, Curios and Rust on the corpus"
        )]
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

/// Refuse a measurement the bench takes no sitting of.
fn known(measurement: &str) -> Result<(), String> {
    match measurement == CONTEST {
        true => Ok(()),
        false => Err(format!(
            "there is no measurement {measurement}; the one the bench takes is {CONTEST}"
        )),
    }
}

fn main() -> ExitCode {
    let outcome = match Cli::parse().command {
        // The next record is numbered after the ones already filed, which is something only the binary knows.
        Bench::Measure { measurement } => known(&measurement)
            .and_then(|()| records(&measurement))
            .and_then(|filed| measure(filed.len())),
        Bench::Report {
            measurement,
            record: None,
        } => known(&measurement)
            .and_then(|()| records(&measurement))
            .map(|filed| print!("{}", report(&filed))),
        Bench::Report {
            measurement,
            record: Some(number),
        } => known(&measurement)
            .and_then(|()| records(&measurement))
            .and_then(|filed| match filed.get(number) {
                Some(record) => {
                    print!("{}", one(number, record));
                    Ok(())
                }
                None => Err(format!("there is no record {number:02} of {measurement}")),
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
