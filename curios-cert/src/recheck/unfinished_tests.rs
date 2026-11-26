//! A module elaboration did not finish, refused by the walk itself where it is unfinished: a metavariable, a node only elaboration builds, an unsolved level or a bound variable no scope binds, in every place a module holds a term the walk reads. The walk's own refusal is the one each place is held to, since soundness rests on it and on nothing ahead of it. A definition's stated value sort is no such place: the walk states the sort itself and reads the statement for nothing ([`identity_tests`](super::identity_tests)).

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_common::{Qualifier, fill},
    curios_core::{Global, Program},
};

/// Each place a module holds a term: what it is, the declaration a refusal there stands at, and the text with `${residue}` where the unfinished term goes.
const PLACES: &[(&str, Option<&str>, &str)] = &[
    (
        "a definition's type",
        Some("d"),
        "let /d : ${residue} = 0;\n0",
    ),
    (
        "a definition's body",
        Some("d"),
        "let /d : #NatType = ${residue};\n0",
    ),
    (
        "a group member's type",
        Some("f"),
        "let /f : ${residue} = 0\nand /g : #NatType = 0;\n0",
    ),
    (
        "a group member's body",
        Some("f"),
        "let /f : #NatType = ${residue}\nand /g : #NatType = 0;\n0",
    ),
    (
        "a family's parameter",
        Some("F"),
        "induct /F(n: ${residue}): pub Type\nend\n0",
    ),
    (
        "a family's sort",
        Some("F"),
        "induct /F: pub (${residue})\nend\n0",
    ),
    (
        "a case's payload",
        Some("F"),
        "induct /F: pub Type\n| c(x: ${residue})\nend\n0",
    ),
    (
        "a structure's parameter",
        Some("S"),
        "struct /S(n: ${residue}): pub Type { m: #NatType }\n0",
    ),
    (
        "a structure's sort",
        Some("S"),
        "struct /S: pub (${residue}) { m: #NatType }\n0",
    ),
    ("the closing term", None, "${residue}"),
];

/// `text` with `residue` in its place, after the propositions the closing term is put at.
fn placed(text: &str, residue: &str) -> String {
    let text = fill!(text, residue);

    fill!("${PROPOSITIONS}\n${text}", PROPOSITIONS, text)
}

/// The declaration a place names, as a refusal stands at it.
fn standing(name: Option<&str>) -> Option<Global> {
    name.map(|name| Global::Authored(Qualifier::from([name])))
}

/// Every place where `residue` is not refused at its declaration by an error `refuses` admits, each with what the walk said.
fn unrefused(residue: &str, refuses: impl Fn(&Error) -> bool) -> Vec<String> {
    PLACES
        .iter()
        .filter_map(|(place, name, text)| {
            let verdicts = core_text_refusals(&placed(text, residue));
            let held = verdicts
                .iter()
                .any(|verdict| verdict.name == standing(*name) && refuses(&verdict.error));

            (!held).then(|| format!("{place}: {verdicts:?}"))
        })
        .collect()
}

/// A metavariable is elaboration's, in whatever place it survives.
#[test]
fn a_metavariable_is_refused_in_every_place_a_module_holds_a_term() {
    let missed = unrefused("#{id = 0} ?", |error| matches!(error, Error::NotCore(_)));
    assert!(missed.is_empty(), "{}", missed.join("\n"));
}

/// A node only elaboration builds, a derived witness's body asked for, is elaboration's in whatever place it survives.
#[test]
fn an_elaboration_only_node_is_refused_in_every_place_a_module_holds_a_term() {
    let missed = unrefused("#{node = Derive} ?", |error| {
        matches!(error, Error::NotCore(_))
    });
    assert!(missed.is_empty(), "{}", missed.join("\n"));
}

/// A level elaboration left unsolved is elaboration's, in whatever place the sort holding it survives.
#[test]
fn an_unsolved_level_is_refused_in_every_place_a_module_holds_a_term() {
    let missed = unrefused("#{level = #{id = 0} ?} Type", |error| {
        matches!(error, Error::NotCore(_))
    });
    assert!(missed.is_empty(), "{}", missed.join("\n"));
}

/// A bound variable no scope binds reads nothing, in whatever place it stands.
#[test]
fn a_loose_bound_variable_is_refused_in_every_place_a_module_holds_a_term() {
    let missed = unrefused("#{binding = Bound(0)} _", |error| {
        matches!(error, Error::NotClosed { .. })
    });
    assert!(missed.is_empty(), "{}", missed.join("\n"));
}

/// A program written in Curios/Core closing with its term and the type it states on the line beneath it.
fn stated(text: &str) -> Program {
    let file = text
        .parse::<curios_surface::CoreFile>()
        .unwrap_or_else(|error| panic!("{}", error.format()));
    let (definitions, entrypoint) =
        curios_surface::to_core(&file, None).unwrap_or_else(|error| panic!("{error}"));

    Program {
        definitions,
        entrypoint: entrypoint.expect("the text closes with its term"),
    }
}

/// The type a program's closing term is put at, unfinished, is refused where the walk reads it, as the term's own residue is.
#[test]
fn an_unfinished_type_the_closing_term_is_put_at_is_refused() {
    let not_core = |error: &Error| matches!(error, Error::NotCore(_));
    let not_closed = |error: &Error| matches!(error, Error::NotClosed { .. });
    for (residue, refuses) in [
        ("#{id = 0} ?", &not_core as &dyn Fn(&Error) -> bool),
        ("#{node = Derive} ?", &not_core),
        ("#{level = #{id = 0} ?} Type", &not_core),
        ("#{binding = Bound(0)} _", &not_closed),
    ] {
        let program = stated(&placed("0\n: ${residue}", residue));
        let verdicts = fixture_refusals(&program, BUDGET, &Globals::default(), SYNTAX);

        assert!(
            verdicts
                .iter()
                .any(|verdict| verdict.name.is_none() && refuses(&verdict.error)),
            "{residue}: {verdicts:?}"
        );
    }
}
