//! Encoding a module and parsing it back, which is what every case here asserts through.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use crate::*;

/// Printing a parsed module, parsing that text, and printing it again must reach the same text. The fixed point is what pins a construct's text form against both halves at once: a printer and a parser that disagree cannot both survive it.
pub(super) fn round_trips(source: &str) {
    let first = source
        .parse::<Module>()
        .expect("expected first module")
        .to_string();

    let second = first
        .parse::<Module>()
        .expect("expected second module")
        .to_string();

    assert_eq!(first, second);
}
