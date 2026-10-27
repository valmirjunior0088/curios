//! How a call prints: flat where it fits, an argument a line where it overflows, a trailing literal or lambda hugging it, and a curried call or a ladder breaking at its seams.

use {super::test_support::render, crate::Term};

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
