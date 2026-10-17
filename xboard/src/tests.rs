//! The board, held by the suite as `cargo xboard report` holds it.

use super::*;

/// The suite fails where `cargo xboard report` would.
#[test]
fn the_board_holds() {
    let report = Report::run(BOARD);

    assert!(!report.fails(), "\n{report}");
}
