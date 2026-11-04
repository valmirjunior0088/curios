//! The soundness board, as a cargo subcommand, and the board itself, one part of the judgment a module beside this one.
//!
//! `cargo xboard report` puts every ticket to the checkers, prints the report, and fails if a ticket regressed or was shut without a fix named. `cargo xboard probe` puts one program, or one module written as Core text, to them as a proof of `False`, off the board: what a hunt asks before there is a ticket to write.
//!
//! What the board is written in and run by is the modules beside the parts, and why it is built this way is the README's. They read in the order a run uses them:
//!
//! - [`Part`], [`Ticket`] and [`Witness`] are what is written on the board, a witness carrying the [`Proof`] of `False` it alleges.
//! - [`Expected`], written by [`refused!`](crate::refused), is the refusal a witness expects once its flaw is closed.
//! - [`Proof::answer`] puts a witness to the checkers at `False`, and an [`Answer`] is what they said, each [`Refusal`] the checker's own error.
//! - [`Report`] runs every ticket and says where each one stands, a [`Standing`].
//! - [`Probe`] puts one proof that is on no ticket, and says what the checkers made of it.

#[cfg(test)]
mod tests;

mod date;
use date::*;

mod ticket;
use ticket::*;

mod expect;
use expect::*;

mod put;
use put::*;

mod report;
use report::*;

mod probe;
use probe::*;

// Each part exports one `PART`, and each ticket file one `TICKET`, so a part's name and a ticket's file are what tell them apart.
mod admission;
mod conversion;
mod elimination;
mod formation;
mod introduction;
mod totality;

use {
    clap::{Parser, Subcommand},
    std::{
        fs,
        io::{self, Read},
        path::{Path, PathBuf},
        process::ExitCode,
    },
};

/// The board itself: every part of the judgment, each with the tickets filed under it.
const PARTS: &[&Part] = &[
    &formation::PART,
    &introduction::PART,
    &elimination::PART,
    &conversion::PART,
    &totality::PART,
    &admission::PART,
];

#[derive(Debug, Parser)]
#[command(
    name = "cargo xboard",
    bin_name = "cargo xboard",
    version,
    about = "The soundness board",
    help_template = "\
{name} {version}
{about-with-newline}
{usage-heading} {usage}

{all-args}{after-help}"
)]
struct Cli {
    #[command(subcommand)]
    command: Board,
}

#[derive(Debug, Subcommand)]
enum Board {
    #[command(about = "Put every ticket to the checkers, and say where each one stands")]
    Report,

    #[command(
        about = "Put one program, or one module written as Core text, to the checkers as a proof of False, and say whether it was admitted",
        after_help = "Exits 0 where the proof was admitted, 1 where it was refused, and 2 where it could not be put."
    )]
    Probe {
        #[arg(
            value_name = "CANDIDATE",
            help = "The file whose final term is the proof, over the standard library alone: Core text where its name ends in .crsc, and a program otherwise; - is standard input, a program"
        )]
        candidate: PathBuf,
    },
}

fn main() -> ExitCode {
    match Cli::parse().command {
        Board::Report => {
            let report = Report::run(PARTS);

            print!("{report}");

            match report.fails() {
                true => ExitCode::FAILURE,
                false => ExitCode::SUCCESS,
            }
        }
        Board::Probe { candidate } => match read(&candidate) {
            Ok(text) => {
                let probe = Probe::put(match candidate.extension() == Some("crsc".as_ref()) {
                    true => Proof::Module(&text),
                    false => Proof::Program(&text),
                });

                print!("{probe}");

                // As a search exits: found is success.
                match probe.admitted() {
                    true => ExitCode::SUCCESS,
                    false => ExitCode::FAILURE,
                }
            }
            Err(why) => {
                eprintln!("{}: {why}", candidate.display());

                ExitCode::from(2)
            }
        },
    }
}

/// The candidate's text: the file at `candidate`, or standard input for `-`.
fn read(candidate: &Path) -> io::Result<String> {
    match candidate.as_os_str() == "-" {
        true => {
            let mut source = String::new();
            io::stdin().read_to_string(&mut source)?;

            Ok(source)
        }
        false => fs::read_to_string(candidate),
    }
}
