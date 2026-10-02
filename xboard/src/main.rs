//! The soundness board, and the command that puts it to the checkers: `cargo xboard` prints the report and fails if a ticket regressed or was shut without a fix named.

mod board;
use board::*;

use {std::process::ExitCode, xboard::Report};

fn main() -> ExitCode {
    let report = Report::run(BOARD);

    print!("{report}");

    match report.fails() {
        true => ExitCode::FAILURE,
        false => ExitCode::SUCCESS,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The suite fails where the command would.
    #[test]
    fn the_board_holds() {
        let report = Report::run(BOARD);

        assert!(!report.fails(), "\n{report}");
    }
}
