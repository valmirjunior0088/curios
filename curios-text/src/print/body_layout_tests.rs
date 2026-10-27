//! How the rest prints as written: a numeral pattern's radix and width, a sort's word, a body riding its arrow or equals, an operator chain's continuations, and a local group's `and`.

use super::test_support::render;

#[test]
fn a_numeral_pattern_prints_in_the_radix_and_width_it_was_written_in() {
    let ladder = "match c | 0x10 => 1 | 007 => 2 | _ => 0 end";

    assert_eq!(render(ladder, 80), ladder);
}

#[test]
fn a_sort_prints_as_the_word_it_was_written_with() {
    assert_eq!(
        render("(A: Sort, P: (A) -> Prop) -> Type", 80),
        "(A: Sort, P: (A) -> Prop) -> Type"
    );
}

#[test]
fn a_lambda_body_rides_the_arrow_and_breaks_when_it_overflows() {
    assert_eq!(render("(x) => x", 80), "(x) => x");
    assert_eq!(
        render("(x) => some_quite_long_call(x, x)", 20),
        "(x) =>\n    some_quite_long_call(\n        x,\n        x)"
    );
}

#[test]
fn a_match_arm_body_rides_its_arrow() {
    // An arm ladder that fits shares one line — the spelling the corpus writes for a two-arm decision, which breaking unconditionally would stretch to five lines.
    assert_eq!(
        render("match b | true => 1 | false => 0 end", 80),
        "match b | true => 1 | false => 0 end"
    );
    // One column short of the flat form, and the ladder breaks: every arm to its own line, at the ladder's column rather than indented under it.
    assert_eq!(
        render("match b | true => 1 | false => 0 end", 35),
        "match b\n| true => 1\n| false => 0\nend"
    );
    assert_eq!(
        render(
            "match b | true => something_rather_long(1) | false => 0 end",
            24
        ),
        "match b\n| true =>\n    something_rather_long(\n        1)\n| false => 0\nend"
    );
}

#[test]
fn an_overflowing_operator_chain_leads_continuations_with_the_operator() {
    // Each precedence link is its own group: at width 12 the inner `alpha + bravo` (13 columns) overflows too, so the whole chain aligns one operand per line, operators leading.
    assert_eq!(
        render("alpha + bravo + charlie", 12),
        "alpha\n    + bravo\n    + charlie"
    );
    // One column wider and the inner link fits flat.
    assert_eq!(
        render("alpha + bravo + charlie", 13),
        "alpha + bravo\n    + charlie"
    );
}

#[test]
fn a_local_let_body_rides_its_equals() {
    assert_eq!(render("let x = 1; x", 80), "let x = 1;\nx");
}

#[test]
fn a_local_let_group_breaks_before_each_and() {
    assert_eq!(
        render("let a : Type = b and b : Type = a; a", 80),
        "let a: Type = b\nand b: Type = a;\na"
    );
}
