//! The board, held by the suite as `cargo xboard report` holds it.

use super::*;

/// The suite fails where `cargo xboard report` would: a record the loader refuses, or a ticket that does not stand.
#[test]
fn the_board_holds() {
    let parts = load_board(TICKETS.iter().copied()).unwrap_or_else(|why| panic!("{why}"));
    let report = Report::run(&parts);

    assert!(!report.fails(), "\n{report}");
}
