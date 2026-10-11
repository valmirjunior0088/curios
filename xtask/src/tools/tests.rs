//! How a tool's presence is read, and what a refusal says.

use super::*;

#[test]
fn nextest_is_listed_by_its_own_name_and_no_other() {
    let listing = "Installed Commands:\n    new                  Create a new cargo package at <path>\n    nextest\n    x                    alias: run --package xtask --\n";

    assert!(lists_nextest(listing));
    assert!(!lists_nextest(&listing.replace("nextest", "nextest-next")));
    assert!(!lists_nextest(
        "    search               Search packages, cargo-nextest among them\n"
    ));
}

#[test]
fn a_refusal_puts_the_fix_on_a_line_of_its_own() {
    assert_eq!(
        nextest_refusal(),
        format!(
            "cargo-nextest is needed, and it is not installed. Install it with:\n\n    {NEXTEST_INSTALL}"
        )
    );
}

#[test]
fn a_node_version_is_read_by_its_major() {
    assert_eq!(node_major("v22.4.1"), Some(22));
    assert_eq!(node_major("24.0.0"), Some(24));
    assert_eq!(node_major("22\n"), Some(22));
    assert_eq!(node_major("lts"), None);
}

#[test]
fn the_floor_the_tree_states_is_a_version() {
    assert!(node_major(NODE_VERSION).is_some(), "{NODE_VERSION}");
}
