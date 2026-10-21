//! What a refusal says of a tool that is not installed.

use super::*;

#[test]
fn a_refusal_puts_the_fix_on_a_line_of_its_own() {
    assert_eq!(
        Tool::Nextest.missing(),
        format!(
            "cargo-nextest is needed, and it is not installed. Install it with:\n\n    {NEXTEST_INSTALL}"
        )
    );
}

#[test]
fn a_tool_no_one_command_installs_is_named_alone() {
    assert_eq!(
        Tool::Node.missing(),
        "Node is needed, and it is not installed."
    );
}
