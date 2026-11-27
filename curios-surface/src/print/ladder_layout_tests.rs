//! How a ladder keeps one line: within the columns a decision is read in, as part of whatever holds it, and never with a headed arm's guards where a width lays it out.

use {super::test_support::*, crate::*, curios_module::SYNTAX};

/// A ladder past the columns a decision is read in and short of a line of 100.
const LONG: &str =
    "match value | some(inner) => combine(inner, other) | none() => fallback(value) end";

// A ladder on one line is read as one decision, so it keeps one only within the columns a reader takes in at once, however much room the line has, and within them wherever it fits: the span is the reader's, so a page narrower than it decides by what fits, as the site's code panel does.
#[test]
fn a_ladder_stays_on_one_line_only_within_the_columns_a_decision_is_read_in() {
    assert!(DEFAULT_FLAT_MATCH_WIDTH < LONG.len() && LONG.len() < 100);
    assert_eq!(
        render(LONG, 100),
        "match value\n| some(inner) => combine(inner, other)\n| none() => fallback(value)\nend"
    );

    let short = "match n | 0 => true | p + 1 => even(p) end";
    for width in [short.len(), 50, 60, 100] {
        assert_eq!(render(short, width), short, "at {width}");
    }
    assert_eq!(
        render(short, short.len() - 1),
        "match n\n| 0 => true\n| p + 1 => even(p)\nend"
    );
}

// What holds a ladder laid out flat holds it flat too, so a call around a ladder past its columns breaks where the call alone would fit.
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
        assert_eq!(
            source.parse::<Term>().unwrap().printed(&SYNTAX).to_string(),
            source
        );
    }
}

// A ladder's columns run from where its line's text begins, so one after a long head is read with the head it ends: it takes a line of its own, where it is measured again, and keeps one there within them.
#[test]
fn a_ladder_past_its_columns_after_a_head_takes_a_line_of_its_own() {
    let source = "let go(k: Nat, i: Nat) -> List(Nat) => match k | 0 => [] | kp + 1 => [i, ..go(kp, i + 1)] end;\ngo(2, 0)";
    assert_eq!(
        render(source, 100),
        "let go(k: Nat, i: Nat) -> List(Nat) =>\n    match k | 0 => [] | kp + 1 => [i, ..go(kp, i + 1)] end;\ngo(2, 0)"
    );
}

// A head short enough that the line's text stays within the columns keeps its ladder on its line.
#[test]
fn a_ladder_within_its_columns_after_a_short_head_keeps_the_heads_line() {
    let source = "let b = match n | 0 => true | p + 1 => even(p) end;\nb";
    assert_eq!(render(source, 100), source);
}

// The break a ladder past its columns takes is the one nearest it: a call that ends in a lambda keeps its line, and the lambda's body takes the next.
#[test]
fn a_ladder_past_its_columns_breaks_the_group_nearest_it() {
    let source = "fold(l, [], (x, acc) => match pred(x) | true => [..acc, x] | false => acc end)";
    assert!(source.len() < 100);
    assert_eq!(
        render(source, 100),
        "fold(l, [], (x, acc) =>\n    match pred(x) | true => [..acc, x] | false => acc end)"
    );
}

// A group broken under one laid out flat indents one level past where its line's text begins: the flat groups around it took no level of its line.
#[test]
fn a_body_broken_under_a_flat_definition_indents_one_level_past_its_line() {
    let source = "let nothing: Across(l) = (iu) => match iu | some(v) => absent(v, extra) | none() => fallback end;\nnothing";
    assert!(source.lines().next().is_some_and(|line| line.len() < 100));
    assert_eq!(
        render(source, 100),
        "let nothing: Across(l) = (iu) =>\n    match iu | some(v) => absent(v, extra) | none() => fallback end;\nnothing"
    );
}
