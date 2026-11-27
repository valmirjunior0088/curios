//! What a declaration's own text states of a definition it declares: the head a page would print it by, and the prose above it.

use super::*;

/// What `declaration` states of the definition named at the `nth` occurrence of `name` in it.
fn stated(declaration: &str, name: &str, nth: usize) -> Option<(String, Vec<String>)> {
    let (label, _) = declaration
        .match_indices(name)
        .nth(nth)
        .expect("the declaration writes it");

    heading(declaration, label).map(|Heading { head, prose, .. }| (head, prose))
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
        "--- Twice `n`.\n---\n--- Never less than it.\nlet double(n: Nat) -> Nat =>\n    n + n;";

    assert_eq!(
        stated(DOUBLE, "double", 0),
        said(
            "let double(n: Nat) -> Nat",
            &["Twice `n`.", "", "Never less than it."]
        )
    );
    // A block shows it in one entry: the prose, each line behind the dashes that open one, above the head.
    assert_eq!(
        heading(DOUBLE, DOUBLE.find("double").expect("it is written"))
            .expect("the name is a definition's")
            .entry(),
        "--- Twice `n`.\n---\n--- Never less than it.\nlet double(n: Nat) -> Nat"
    );
}

/// A member is stated within its owner: the text is its owner's block, the member is its place among the block's members, and the prose is its own. So it is written between its owner's head and closing word, its siblings left out, and a block's entry holds its prose above its own line.
#[test]
fn a_member_is_stated_within_its_owner() {
    let heading_of = |declaration: &str, name: &str| {
        let (label, _) = declaration
            .match_indices(name)
            .next()
            .expect("the declaration writes it");

        heading(declaration, label).expect("the name is a definition's")
    };

    const OPTION: &str =
        "pub induct Option(A: Type): pub Type\n--- A value.\n| some(A)\n| none()\nend";
    let option = heading_of(OPTION, "Option");
    assert_eq!(
        (option.head.as_str(), option.member),
        (
            "pub induct Option(A: Type): pub Type\n| some(A)\n| none()\nend",
            None
        )
    );
    let some = heading_of(OPTION, "some");
    assert_eq!(
        (some.head.as_str(), some.prose.as_slice(), some.member),
        (
            "pub induct Option(A: Type): pub Type\n| some(A)\n| none()\nend",
            &["A value.".to_string()][..],
            Some(0)
        )
    );
    assert_eq!(
        some.written(),
        "pub induct Option(A: Type): pub Type\n| some(A)\nend"
    );
    assert_eq!(
        some.entry(),
        "pub induct Option(A: Type): pub Type\n--- A value.\n| some(A)\nend"
    );

    // A method's prose stands at the indent the method is written at.
    const SHOW: &str =
        "pub concept Show(A: Type): pub Type {\n    --- Its text.\n    show(A) -> Str,\n}";
    assert_eq!(heading_of(SHOW, "show").entry(), SHOW);

    // A constructor is written as its family's block lists it, with the indices it targets, and not as it would stand alone, with the family it builds.
    const VEC: &str = "pub induct Vec(T: Type): (Nat) -> pub Type\n| nil(): (0)\n| cons(@n: Nat, head: T, tail: Vec(T)(n)): (n + 1)\nend";
    assert_eq!(
        heading_of(VEC, "cons").written(),
        "pub induct Vec(T: Type): (Nat) -> pub Type\n| cons(@n: Nat, head: T, tail: Vec(T)(n)): (n + 1)\nend"
    );

    // A member of a type whose representation is kept is written in the block its source writes, which whoever reads that source sees.
    const KEPT: &str = "pub induct Shade: Type\n| light()\n| dark(Nat)\nend";
    assert_eq!(
        heading_of(KEPT, "dark").written(),
        "pub induct Shade: Type\n| dark(Nat)\nend"
    );
    assert_eq!(
        heading_of(KEPT, "dark").entry(),
        "pub induct Shade: Type\n| dark(Nat)\nend"
    );
    // The type's own entry is its head alone, as a consumer's listing writes it and its page shows it: nothing shows the members of a kept representation but an answer about one of them.
    let kept = heading_of(KEPT, "Shade");
    assert_eq!(kept.entry(), "pub induct Shade: Type");
    assert_eq!(kept.written(), "pub induct Shade: Type");
    // A sealed concept keeps its methods, which a consumer calls by name either way.
    const SEALED: &str = "pub concept Sized(T: Type): Type {\n    size(T) -> Nat,\n}";
    assert_eq!(heading_of(SEALED, "Sized").entry(), SEALED);
}

/// A witness is named by its head, a test by its name, and a member of a group by its own.
#[test]
fn a_witness_a_test_and_a_group_member_are_each_stated() {
    const WITNESS: &str =
        "--- A number's digits.\nsatisfy Show(Nat) {\n    show(n) => to_str(n),\n}";
    assert_eq!(
        stated(WITNESS, "Show(Nat)", 0),
        said("satisfy Show(Nat)", &["A number's digits."])
    );
    // A witness whose body is `..` alone is one the compiler writes, and one that supplies its fields is not.
    let derived = |declaration: &str, head: &str| {
        heading(declaration, declaration.find(head).expect("it is written"))
            .expect("the head is a witness's")
            .derived
    };
    assert!(derived("satisfy Spell(Point) { .. }", "Spell(Point)"));
    assert!(!derived(WITNESS, "Show(Nat)"));

    assert_eq!(
        stated("test holds =>\n    assert(true);", "holds", 0),
        said("test holds", &[])
    );

    const GROUP: &str =
        "let even(n: Nat) -> Bool =>\n    odd(n)\nand odd(n: Nat) -> Bool =>\n    even(n);";
    assert_eq!(
        stated(GROUP, "odd", 1),
        said("let odd(n: Nat) -> Bool", &[])
    );
}

/// A declaration is written as its source writes it with what a body elides left out: a family whose representation shows is whole, its constructors each behind a bar and closed by `end`, and one whose representation is kept is its head alone; a structure and a concept write their members in braces, each with its comma; a definition is its head.
#[test]
fn a_declaration_is_written_whole_where_its_representation_shows() {
    let written = |declaration: &str, name: &str| {
        let (label, _) = declaration
            .match_indices(name)
            .next()
            .expect("the declaration writes it");

        heading(declaration, label).map(|heading| heading.written())
    };
    const SHADE: &str = "pub induct Shade: pub Type\n| light()\n| dark(Nat)\nend";
    const PAIR: &str = "pub struct Pair(A: Type): pub Type {\n    first: A,\n    second: Nat,\n}";
    const SIZED: &str = "pub concept Sized(T: Type): Type {\n    size(T) -> Nat,\n}";

    assert_eq!(written(SHADE, "Shade").as_deref(), Some(SHADE));
    assert_eq!(
        written("pub induct Shade: Type\n| light()\nend", "Shade").as_deref(),
        Some("pub induct Shade: Type")
    );
    assert_eq!(written(PAIR, "Pair").as_deref(), Some(PAIR));
    assert_eq!(written(SIZED, "Sized").as_deref(), Some(SIZED));
    assert_eq!(
        written("pub let double(n: Nat) -> Nat =>\n    n + n;", "double").as_deref(),
        Some("pub let double(n: Nat) -> Nat")
    );
}

/// A place that is no definition's name states nothing, and neither does a text that is no item.
#[test]
fn a_place_no_definition_is_named_at_states_nothing() {
    const DOUBLE: &str = "let double(n: Nat) -> Nat =>\n    n + n;";

    assert_eq!(stated(DOUBLE, "Nat", 0), None);
    assert_eq!(heading("double(n: Nat) -> Nat", 0), None);
}
