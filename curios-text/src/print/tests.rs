use {
    super::print_term,
    crate::Term,
    curios_print::{Printer, run_printer_within},
    std::{cell::Cell, fmt},
};

/// Parse `source` as a term and render it within `width` at the formatter's indent step.
fn render(source: &str, width: usize) -> String {
    let term: Term = source.parse().expect("fixture parses");
    struct Within(Cell<Option<(Printer, usize)>>);
    impl fmt::Display for Within {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            let (printer, width) = self.0.take().expect("rendered once");
            run_printer_within(printer, formatter, 4, width)
        }
    }
    Within(Cell::new(Some((print_term(term), width)))).to_string()
}

#[test]
fn a_fitting_application_stays_flat() {
    assert_eq!(render("f(alpha, bravo)", 80), "f(alpha, bravo)");
}

#[test]
fn an_overflowing_application_breaks_one_argument_per_line_with_a_riding_closer() {
    // No trailing comma and no line of its own for the closer: the last argument already ends the call, and saying so with `,` and a lone `)` spends two lines.
    assert_eq!(render("f(alpha, bravo)", 10), "f(\n    alpha,\n    bravo)");
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
    // An arm ladder that fits shares one line — the spelling the corpus writes for a two-arm decision. It used to break unconditionally, which made every such decision five lines.
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
