//! What a declaration's own text states of a definition it declares: the head a page would print it by, and the prose above it.

use super::{Heading, heading};

/// What `declaration` states of the definition named at the `nth` occurrence of `name` in it.
fn stated(declaration: &str, name: &str, nth: usize) -> Option<(String, Vec<String>)> {
    let (label, _) = declaration
        .match_indices(name)
        .nth(nth)
        .expect("the declaration writes it");

    heading(declaration, label).map(|Heading { head, prose }| (head, prose))
}

fn said(head: &str, prose: &[&str]) -> Option<(String, Vec<String>)> {
    Some((
        head.to_string(),
        prose.iter().map(|line| line.to_string()).collect(),
    ))
}

/// A definition no consumer sees is stated as one a page shows is: the head is everything before the body, and the prose is the block above it, a line an entry.
#[test]
fn a_private_definition_is_stated_by_its_head_and_the_prose_above_it() {
    const DOUBLE: &str =
        "--- Twice `n`.\n---\n--- Never less than it.\nlet double(n: Nat) -> Nat =\n    n + n;";

    assert_eq!(
        stated(DOUBLE, "double", 0),
        said(
            "let double(n: Nat) -> Nat",
            &["Twice `n`.", "", "Never less than it."]
        )
    );
}

/// A constructor stands apart from its type, so its head says what it builds, and a method is stated as its concept's block lists it.
#[test]
fn a_member_is_stated_as_it_stands_apart_from_its_owner() {
    const OPTION: &str =
        "pub induct Option(A: Type): pub Type\n--- A value.\n| some(A)\n| none()\nend";
    assert_eq!(
        stated(OPTION, "Option", 0),
        said("pub induct Option(A: Type): pub Type", &[])
    );
    assert_eq!(
        stated(OPTION, "some", 0),
        said("some(A) -> Option(A)", &["A value."])
    );

    const SHOW: &str =
        "pub concept Show(A: Type): pub Type {\n    --- Its text.\n    show(A) -> Str,\n}";
    assert_eq!(
        stated(SHOW, "Show", 0),
        said("pub concept Show(A: Type): pub Type", &[])
    );
    assert_eq!(
        stated(SHOW, "show", 0),
        said("show(A) -> Str", &["Its text."])
    );
}

/// A witness is named by its head, a test by its name, and a member of a group by its own.
#[test]
fn a_witness_a_test_and_a_group_member_are_each_stated() {
    const WITNESS: &str =
        "--- A number's digits.\nsatisfy Show(Nat) {\n    show(n) = to_str(n),\n}";
    assert_eq!(
        stated(WITNESS, "Show(Nat)", 0),
        said("satisfy Show(Nat)", &["A number's digits."])
    );

    assert_eq!(
        stated("test holds =\n    assert(true);", "holds", 0),
        said("test holds", &[])
    );

    const GROUP: &str =
        "let even(n: Nat) -> Bool =\n    odd(n)\nand odd(n: Nat) -> Bool =\n    even(n);";
    assert_eq!(
        stated(GROUP, "odd", 1),
        said("let odd(n: Nat) -> Bool", &[])
    );
}

/// A place that is no definition's name states nothing, and neither does a text that is no item.
#[test]
fn a_place_no_definition_is_named_at_states_nothing() {
    const DOUBLE: &str = "let double(n: Nat) -> Nat =\n    n + n;";

    assert_eq!(stated(DOUBLE, "Nat", 0), None);
    assert_eq!(heading("double(n: Nat) -> Nat", 0), None);
}
