//! The soundness board, as a cargo subcommand, and the code that reads and runs it. The board itself is its records, `xboard/tickets/`, embedded when the binary is built (`build.rs`).
//!
//! `cargo xboard report` puts every ticket to the checkers, prints the report, and fails if a ticket regressed or was shut without a fix named, or if a record breaks the words the loader knows. `cargo xboard probe` puts one program, one module written as Core text, or one ticket by its `metadata.toml` to them, off the board: what a hunt asks before there is a ticket to commit.
//!
//! Why the board is built this way is the README's. Its modules read in the order a run uses them:
//!
//! - [`load_board`] reads the records into [`Part`]s, [`Ticket`]s and [`Witness`]es, a witness carrying the [`Proof`] of `False` it alleges and the [`Expected`] refusal of it once its flaw is closed.
//! - [`Proof::answer`] puts a witness to the checkers at `False`, and an [`Answer`] is what they said, each [`Refusal`] the checker's own error.
//! - [`Report`] runs every ticket and says where each one stands, a [`Standing`], a [`Row`] for each.
//! - [`Probe`] puts one proof that is on no ticket, and says what the checkers made of it.

#[cfg(test)]
mod tests;

mod date;
use date::*;

mod load;
use load::*;

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

use {
    clap::{Parser, Subcommand},
    std::{
        collections::BTreeMap,
        fs,
        path::{Path, PathBuf},
        process::ExitCode,
    },
};

/// The board's records: every file under `xboard/tickets/`, by its path below it, as they stood when the binary was built.
const TICKETS: &[(&str, &str)] = include!(concat!(env!("OUT_DIR"), "/tickets.rs"));

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
        about = "Put one program, one module written as Core text, or one ticket to the checkers as a proof of False, and say whether it was admitted",
        after_help = "Exits 0 where a proof was admitted, 1 where it was refused, and 2 where it could not be put."
    )]
    Probe {
        #[arg(
            value_name = "CANDIDATE",
            help = "The file whose final term is the proof, over the standard library alone: a program where its name ends in .crs, and Core text where it ends in .crsc; or a ticket's metadata.toml, every witness of it, its texts read beside it"
        )]
        candidate: PathBuf,
    },
}

fn main() -> ExitCode {
    match Cli::parse().command {
        Board::Report => {
            let parts = match load_board(TICKETS.iter().copied()) {
                Ok(parts) => parts,
                Err(why) => {
                    eprintln!("xboard/tickets/{why}");

                    return ExitCode::FAILURE;
                }
            };
            let report = Report::run(&parts);

            print!("{report}");

            match report.fails() {
                true => ExitCode::FAILURE,
                false => ExitCode::SUCCESS,
            }
        }
        Board::Probe { candidate } => {
            // As a search exits: found is success.
            let admitted = match candidate_at(&candidate) {
                Ok(Candidate::Proof(proof)) => {
                    let probe = Probe::put(&proof);
                    print!("{probe}");

                    probe.admitted()
                }
                Ok(Candidate::Ticket { part, ticket }) => {
                    let row = Row::put(&part, &ticket);
                    print!("{row}");

                    row.admitted()
                }
                Err(why) => {
                    eprintln!("{why}");

                    return ExitCode::from(2);
                }
            };

            match admitted {
                true => ExitCode::SUCCESS,
                false => ExitCode::FAILURE,
            }
        }
    }
}

/// What a probe is handed, read off the file it is named by.
enum Candidate {
    /// A program or a module written as Core text, put over the prelude alone.
    Proof(Proof),
    /// A ticket, every witness of it, with the name of the part it is filed under.
    Ticket { part: String, ticket: Ticket },
}

/// The candidate at `path`: a program where its name ends in `.crs`, Core text where it ends in `.crsc`, and a ticket where it is a `metadata.toml`, read with the texts beside it as the board reads one it embeds.
fn candidate_at(path: &Path) -> Result<Candidate, String> {
    let text = || fs::read_to_string(path).map_err(|why| format!("{}: {why}", path.display()));
    if path.file_name().is_some_and(|name| name == METADATA) {
        let directory = path.parent().ok_or_else(|| {
            format!(
                "{}: a ticket's record stands in its directory",
                path.display()
            )
        })?;
        let (part, ticket) = ticket_at(directory)?;

        return Ok(Candidate::Ticket { part, ticket });
    }

    match path.extension().and_then(|extension| extension.to_str()) {
        Some("crs") => Ok(Candidate::Proof(Proof::Program(text()?))),
        Some("crsc") => Ok(Candidate::Proof(Proof::Module(text()?))),
        _ => Err(format!(
            "{}: a candidate is a program, `.crs`, Core text, `.crsc`, or a ticket's `{METADATA}`",
            path.display()
        )),
    }
}

/// The ticket filed in the directory `directory`, read from its files as the board reads one it embeds, and the name of the part its parent directory is.
fn ticket_at(directory: &Path) -> Result<(String, Ticket), String> {
    let name = |path: &Path| {
        path.file_name()
            .and_then(|name| name.to_str())
            .map(str::to_string)
            .ok_or_else(|| format!("{}: a ticket's directory is named in UTF-8", path.display()))
    };
    let directory = directory
        .canonicalize()
        .map_err(|why| format!("{}: {why}", directory.display()))?;
    let part = directory
        .parent()
        .map(name)
        .transpose()?
        .unwrap_or_default();

    let mut files = Vec::new();
    for entry in
        fs::read_dir(&directory).map_err(|why| format!("{}: {why}", directory.display()))?
    {
        let path = entry
            .map_err(|why| format!("{}: {why}", directory.display()))?
            .path();
        let text = fs::read_to_string(&path).map_err(|why| format!("{}: {why}", path.display()))?;
        files.push((name(&path)?, text));
    }
    let files = files
        .iter()
        .map(|(file, text)| (file.as_str(), text.as_str()))
        .collect::<BTreeMap<_, _>>();
    let ticket = load_ticket(&name(&directory)?, &files).map_err(|why| format!("{part}/{why}"))?;

    Ok((part, ticket))
}
