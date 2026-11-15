//! How a ladder keeps one line: within its share of the width, as part of whatever holds it, and never with a headed arm's guards where a width lays it out.

use {
    super::{FLAT_MATCH_SHARE, test_support::*},
    crate::*,
};

/// A ladder past its share of a width of 100 and short of the line.
const LONG: &str =
    "match value | some(inner) => combine(inner, other) | none() => fallback(value) end";

// A ladder on one line is read as one decision, so it keeps one only within its share of the width, however much room the line has.
#[test]
fn a_ladder_stays_on_one_line_only_within_its_share_of_the_width() {
    let share = 100 * FLAT_MATCH_SHARE / 100;
    assert!(share < LONG.len() && LONG.len() < 100);
    assert_eq!(
        render(LONG, 100),
        "match value\n| some(inner) => combine(inner, other)\n| none() => fallback(value)\nend"
    );

    let short = "match b | true => f | false => g end";
    let width = (short.len() * 100).div_ceil(FLAT_MATCH_SHARE);
    assert_eq!(render(short, width), short);
    assert_eq!(
        render(short, width - 1),
        "match b\n| true => f\n| false => g\nend"
    );
}

// What holds a ladder laid out flat holds it flat too, so a call around a ladder past its share breaks where the call alone would fit.
#[test]
fn a_call_around_a_long_ladder_breaks_with_it() {
    let source = format!("f({LONG})");
    assert!(source.len() < 100);
    assert_eq!(
        render(&source, 100),
        "f(\n    match value\n    | some(inner) => combine(inner, other)\n    | none() => fallback(value)\n    end)"
    );
}

// Guards after a headed arm's patterns put a second thing between a pattern and its body, so a ladder that writes one takes a line for each arm at any width. A match with no head writes a guard where a headed one writes its patterns, and keeps its line.
#[test]
fn a_headed_arms_guard_breaks_a_ladder_at_any_width() {
    assert_eq!(
        render("match o | some(n), n > 9 => 2 | _ => 0 end", 100),
        "match o\n| some(n), n > 9 => 2\n| _ => 0\nend"
    );
    assert_eq!(
        render("match | b => f | _ => g end", 100),
        "match | b => f | _ => g end"
    );
}

// With no width, as a report prints a term, every ladder keeps its one line.
#[test]
fn with_no_width_every_ladder_keeps_its_line() {
    for source in [LONG, "match n | 0 => 1 | k + 1 => 2 end"] {
        assert_eq!(source.parse::<Term>().unwrap().to_string(), source);
    }
}
