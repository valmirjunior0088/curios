//! The soundness board, as a cargo subcommand, and the board itself, under `board/`.
//!
//! `cargo xboard report` puts every ticket to the checkers, prints the report, and fails if a ticket regressed or was shut without a fix named. `cargo xboard probe` puts one program to them as a proof of `False`, off the board: what a hunt asks before there is a ticket to write.

mod board;
use board::*;

use {
    clap::{Parser, Subcommand},
    std::{
        fs,
        io::{self, Read},
        path::{Path, PathBuf},
        process::ExitCode,
    },
    xboard::{Probe, Report},
};

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
        about = "Put one program to the checkers as a proof of False, and say whether it was admitted",
        after_help = "Exits 0 where the program was admitted, 1 where it was refused, and 2 where it could not be put."
    )]
    Probe {
        #[arg(
            value_name = "PROGRAM",
            help = "The file whose final term is the proof, over the standard library alone; - is standard input"
        )]
        program: PathBuf,
    },
}

fn main() -> ExitCode {
    match Cli::parse().command {
        Board::Report => {
            let report = Report::run(BOARD);

            print!("{report}");

            match report.fails() {
                true => ExitCode::FAILURE,
                false => ExitCode::SUCCESS,
            }
        }
        Board::Probe { program } => match read(&program) {
            Ok(source) => {
                let probe = Probe::put(&source);

                print!("{probe}");

                // As a search exits: found is success.
                match probe.admitted() {
                    true => ExitCode::SUCCESS,
                    false => ExitCode::FAILURE,
                }
            }
            Err(why) => {
                eprintln!("{}: {why}", program.display());

                ExitCode::from(2)
            }
        },
    }
}

/// The program's text: the file at `program`, or standard input for `-`.
fn read(program: &Path) -> io::Result<String> {
    match program.as_os_str() == "-" {
        true => {
            let mut source = String::new();
            io::stdin().read_to_string(&mut source)?;

            Ok(source)
        }
        false => fs::read_to_string(program),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The suite fails where `cargo xboard report` would.
    #[test]
    fn the_board_holds() {
        let report = Report::run(BOARD);

        assert!(!report.fails(), "\n{report}");
    }
}
