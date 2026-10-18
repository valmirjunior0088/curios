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
fn an_overflowing_record_breaks_one_field_per_line_with_its_closer_riding_after_a_space() {
    assert_eq!(
        render("Point { alpha = 1, bravo = 2 }", 80),
        "Point { alpha = 1, bravo = 2 }"
    );
    assert_eq!(
        render("Point { alpha = 1, bravo = 2 }", 20),
        "Point {\n    alpha = 1,\n    bravo = 2 }"
    );
}

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
fn a_fitting_application_stays_flat() {
    assert_eq!(render("f(alpha, bravo)", 80), "f(alpha, bravo)");
}

#[test]
fn a_list_packs_each_run_before_wrapping() {
    assert_eq!(
        render("[alpha, beta, gamma, delta, epsilon, zeta, eta, theta]", 30),
        "[alpha, beta, gamma, delta,\n    epsilon, zeta, eta, theta]"
    );
    assert_eq!(render("[a, b, c]", 80), "[a, b, c]");
    assert_eq!(render("[a, b, c]", 8), "[a, b,\n    c]");
}

#[test]
fn wrapped_list_indentation_is_independent_of_the_openers_column() {
    for head in ["f", "long_name"] {
        let source = format!("{head}([alpha, bravo, charlie, delta])");
        let output = render(&source, 24);
        assert_eq!(
            output,
            format!("{head}([alpha, bravo,\n    charlie, delta])")
        );
    }
}

#[test]
fn a_wrapping_list_keeps_its_items_breakable() {
    let source = "[small, build(alpha, bravo, charlie), next, last]";
    let output = render(source, 24);
    assert!(output.contains("build(\n"), "{output}");
    assert!(
        output.lines().all(|line| line.chars().count() <= 24),
        "{output}"
    );
    assert_eq!(render(&output, 24), output);
}

#[test]
fn a_multiline_list_separates_structure_and_packs_each_simple_run() {
    let source = "[alpha, beta, build(short), gamma, delta, other(value)]";
    let output = render(source, 24);
    assert_eq!(
        output,
        "[\n    alpha, beta,\n    build(short),\n    gamma, delta,\n    other(value)]"
    );
    assert_eq!(render(&output, 24), output);
    assert_eq!(
        render("[build(short), other(value)]", 80),
        "[build(short), other(value)]"
    );
}

#[test]
fn nested_lists_and_spreads_retain_their_structure_when_wrapping() {
    let source = "[[alpha, bravo, charlie], ..rest, [delta, echo, foxtrot]]";
    let output = render(source, 24);
    assert!(
        output.lines().all(|line| line.chars().count() <= 24),
        "{output}"
    );
    assert_eq!(
        source.parse::<Term>().unwrap(),
        output.parse::<Term>().unwrap()
    );
    assert_eq!(render(&output, 24), output);
}

#[test]
fn every_list_form_packs_atoms_and_separates_spreads_and_calls() {
    for open in ["[", "x["] {
        let source = format!("{open}1, 2, ..tail, 3, 4, build(alpha, bravo, charlie), 5, 6]");
        let output = render(&source, 24);
        assert!(
            output.contains("\n    1, 2,\n    ..tail,\n    3, 4,\n"),
            "{output}"
        );
        assert!(output.contains("build(\n"), "{output}");
        assert!(output.ends_with("\n    5, 6]"), "{output}");
        assert!(
            output.lines().all(|line| line.chars().count() <= 24),
            "{output}"
        );
        assert_eq!(
            source.parse::<Term>().unwrap(),
            output.parse::<Term>().unwrap()
        );
        assert_eq!(render(&output, 24), output);
        assert_eq!(
            render(&format!("{open}1, ..tail, 2]"), 80),
            format!("{open}1, ..tail, 2]")
        );
    }
}

#[test]
fn compact_literal_and_projection_entries_pack_together() {
    for open in ["[", "x["] {
        assert_eq!(
            render(&format!("{open}'a', \"b\", c.d, [], x[], ()]"), 80),
            format!("{open}'a', \"b\", c.d, [], x[], ()]")
        );
    }
}

#[test]
fn an_overflowing_application_breaks_one_argument_per_line_with_a_riding_closer() {
    // No trailing comma and no line of its own for the closer: the last argument already ends the call, and saying so with `,` and a lone `)` spends two lines.
    assert_eq!(render("f(alpha, bravo)", 10), "f(\n    alpha,\n    bravo)");
}

#[test]
fn a_trailing_literal_or_lambda_after_atoms_hugs_the_call() {
    // The literal's opener stays on the head's line and its elements sit one level in, for a list, a packed literal and a tuple alike.
    assert_eq!(
        render("f(alpha, [bravo, charlie])", 12),
        "f(alpha, [\n    bravo,\n    charlie])"
    );
    // A packed run of atoms wraps as it does anywhere, its first element riding the opener.
    assert_eq!(render("f(x[1, 2, 3])", 8), "f(x[1,\n    2,\n    3])");
    assert_eq!(
        render("f((alpha, bravo))", 10),
        "f((\n    alpha,\n    bravo))"
    );
    // A lambda hugs the same way: its binders on the head's line, its body one level in below the arrow.
    assert_eq!(
        render("List/map(names, (name) => Str/concat(name, suffix))", 30),
        "List/map(names, (name) =>\n    Str/concat(name, suffix))"
    );
    // A structured leading argument keeps the call's ordinary break opportunities.
    assert_eq!(
        render("f(g(alpha), [bravo])", 14),
        "f(\n    g(alpha),\n    [bravo])"
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

#[test]
fn a_match_operand_or_callee_prints_bare() {
    // `end` closes it, so the parentheses a `let` or a lambda needs in these positions would only be noise.
    assert_eq!(
        render("1 + match b | true => 1 | false => 0 end", 80),
        "1 + match b | true => 1 | false => 0 end"
    );
    assert_eq!(
        render("(match b | true => f | false => g end)(x)", 80),
        "match b | true => f | false => g end(x)"
    );
}

#[test]
fn a_ladders_call_participates_in_its_flat_layout() {
    for (ladder, broken) in [
        (
            "match b | true => f | false => g end",
            "match b\n| true => f\n| false => g\nend",
        ),
        (
            "choose | b => f | _ => g end",
            "choose\n| b => f\n| _ => g\nend",
        ),
    ] {
        let source = format!("{ladder}(p)");
        let width = source.len();
        assert_eq!(render(&source, width), source);
        assert_eq!(render(&source, width - 1), format!("{broken}(p)"));
        assert_eq!(source.parse::<Term>().unwrap().to_string(), source);
    }
}

#[test]
fn long_arguments_can_wrap_after_a_ladders_arms_break() {
    for (ladder, broken) in [
        (
            "match b | true => f | false => g end",
            "match b\n| true => f\n| false => g\nend",
        ),
        (
            "choose | b => f | _ => g end",
            "choose\n| b => f\n| _ => g\nend",
        ),
    ] {
        assert_eq!(
            render(
                &format!("{ladder}(first_long_argument, second_long_argument)"),
                32
            ),
            format!("{broken}(\n    first_long_argument,\n    second_long_argument)")
        );
    }
}

#[test]
fn a_curried_call_that_fits_stays_on_one_line() {
    assert_eq!(render("f(a, b)(c)(d)", 80), "f(a, b)(c)(d)");
}

#[test]
fn a_curried_call_that_breaks_ends_a_line_at_every_seam() {
    assert_eq!(
        render("format(alpha, beta)(gamma, epsilon)(delta)", 24),
        "format(alpha, beta)(\n    gamma,\n    epsilon)(\n    delta)"
    );
}

#[test]
fn a_curried_calls_first_list_breaks_inside_itself_only_where_it_cannot_fit() {
    assert_eq!(
        render("format(alphabetical, betatron)(gamma)", 24),
        "format(\n    alphabetical,\n    betatron)(\n    gamma)"
    );
}

#[test]
fn what_follows_a_curried_call_rides_its_last_closer() {
    assert_eq!(
        render("format(alpha, beta)(gamma)!", 24),
        "format(alpha, beta)(\n    gamma)!"
    );
}

#[test]
fn a_curried_call_hugs_its_last_argument_after_nothing_but_atoms() {
    assert_eq!(
        render("f(a, b)(c)((x) => body_of_the_lambda)", 24),
        "f(a, b)(c)((x) =>\n    body_of_the_lambda)"
    );
}

#[test]
fn a_curried_call_breaks_at_its_seams_before_a_lambda_that_follows_more_than_atoms() {
    assert_eq!(
        render("f(g(a), b)((x) => body_of_the_lambda)", 24),
        "f(g(a), b)(\n    (x) =>\n        body_of_the_lambda)"
    );
}

#[test]
fn every_call_in_a_postfix_chain_participates_in_the_ladders_layout() {
    for (ladder, broken) in [
        (
            "match b | true => f | false => g end",
            "match b\n| true => f\n| false => g\nend",
        ),
        (
            "choose | b => f | _ => g end",
            "choose\n| b => f\n| _ => g\nend",
        ),
    ] {
        for suffix in [
            "(p)(q)",
            ".field(p)",
            "(p)!(q)",
            "!(p)",
            "(p).field(q)",
            "(@A, use witness, p)",
        ] {
            let source = format!("{ladder}{suffix}");
            assert_eq!(
                render(&source, source.len() - 1),
                format!("{broken}{suffix}")
            );
        }
    }
}

#[test]
fn a_forced_ladder_keeps_its_call_attached() {
    for (source, expected) in [
        (
            "match n | 0 => f | 1 => g | 2 => h | _ => i end(p)",
            "match n\n| 0 => f\n| 1 => g\n| 2 => h\n| _ => i\nend(p)",
        ),
        (
            "choose | a => f | b => g | c => h | _ => i end(p)",
            "choose\n| a => f\n| b => g\n| c => h\n| _ => i\nend(p)",
        ),
        (
            "match b | true => match c | true => f | false => g end | false => h end(p)",
            "match b\n| true => match c | true => f | false => g end\n| false => h\nend(p)",
        ),
        (
            "choose | b => choose | c => f | _ => g end | _ => h end(p)",
            "choose\n| b => choose | c => f | _ => g end\n| _ => h\nend(p)",
        ),
    ] {
        assert_eq!(render(source, 200), expected);
    }
}
