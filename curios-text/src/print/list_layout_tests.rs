//! How a list, a record and a packed literal print: each run packed before wrapping, structure kept apart, and a closer riding after its last entry.

use {super::test_support::render, crate::Term};

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
