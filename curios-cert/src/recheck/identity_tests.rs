//! Identities a walk is handed: a free local is refused before the certifier mints a binder that could alias it, and a bound variable no scope binds before a rule reads it as one.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_common::{Qualifier, fill},
    curios_core::{Free, Global, Item},
};

/// The indices a stray local is tried at: every one the certifier mints first, so a walk that let the local through would open a binder carrying the same identity at one of them.
const COLLIDING: std::ops::Range<u64> = 0..4;

fn held() -> Global {
    Global::Authored(Qualifier::from(["held"]))
}

/// The stray local at `index`, as the texts below write it.
fn stray(index: u64) -> Free {
    Free::local(index, Some("stray"))
}

/// Whether the walk refused `name` for the stray local at `index`.
fn unbound(verdicts: &[Verdict], name: Option<Global>, index: u64) -> bool {
    verdicts.iter().any(|verdict| {
        verdict.name == name
            && matches!(&verdict.error, Error::Unbound(found) if *found == stray(index))
    })
}

/// Whether the walk refused `name` for a bound variable no scope binds.
fn unclosed(verdicts: &[Verdict], name: Option<Global>) -> bool {
    verdicts
        .iter()
        .any(|verdict| verdict.name == name && matches!(&verdict.error, Error::NotClosed { .. }))
}

/// A definition mentioning a local no binder of its opens is refused, at every index the certifier could mint for the binder it opens.
///
/// The certifier's counter starts at zero, so the local is one it can hand out again: opening `x` at the stray's index turns `(x : Nat) => stray` into the identity, which inhabits `(x : Nat) -> Nat`, and the judgment — which then sees a bound variable — accepts. The refusal is taken at the boundary for that reason. Mutation-checked: dropping it certifies the definition at the colliding index.
#[test]
fn a_definition_mentioning_a_free_local_is_refused() {
    for index in COLLIDING {
        let verdicts = text_verdicts(&fill!(
            r#"
let /held: (x: #NatType) -> #NatType =
    (x: #NatType) => #{binding = Free(${index})} stray;
"#,
            index
        ));

        assert!(
            unbound(&verdicts, Some(held()), index),
            "a definition mentioning free local {index} was not refused for it: {verdicts:?}",
        );
    }
}

/// The sort a definition states its value inhabits is a position of the definition, held to the boundary its type and body are: a stray local there is refused, at every index the certifier could mint.
#[test]
fn a_stated_value_sort_mentioning_a_free_local_is_refused() {
    for index in COLLIDING {
        let verdicts = text_verdicts(&fill!(
            r#"
#{value_sort = ((y: #NatType) => Type)(#{binding = Free(${index})} stray)}
let /held: Type = #NatType;
"#,
            index
        ));

        assert!(
            unbound(&verdicts, Some(held()), index),
            "a value sort mentioning free local {index} was not refused for it: {verdicts:?}",
        );
    }
}

/// What a module states of a definition's value sort is read by nothing: the walk states the sort the value inhabits itself (`certifier::stating`). A statement holding a metavariable reduction discards, one carrying a universe parameter past the scheme, and one naming the wrong sort are each admitted, and `held : Type = Nat` is stated `Type 0` whatever it said.
#[test]
fn a_stated_value_sort_is_read_by_nothing() {
    for (said, stated) in [
        (
            "a metavariable",
            "((y: #NatType) => Type)(#{id = 7, origin = Hole} ?)",
        ),
        (
            "an escaping level",
            "((y: Type) => Type)(#{level = v} Type)",
        ),
        ("the wrong sort", "Prop"),
    ] {
        let rechecked = text_certified(&fill!(
            "#{value_sort = ${stated}}\nlet /held: Type = #NatType;",
            stated
        ));
        assert_eq!(rechecked.verdicts(), Vec::new(), "{said}");
        let Item::Let(definition) = &rechecked.definitions().items[0] else {
            panic!("the module holds one definition");
        };
        assert_eq!(definition.value_sort, Some(term("Type")), "{said}");
    }
}

/// The entry is held to the same boundary as the module's items, since it is judged in the same walk by the same counter. Mutation-checked as above.
#[test]
fn an_entry_mentioning_a_free_local_is_refused() {
    for index in COLLIDING {
        let verdicts = text_verdicts(&fill!(
            "((x: #NatType) => #{binding = Free(${index})} stray)(0)\n: #NatType",
            index
        ));

        assert!(
            unbound(&verdicts, None, index),
            "an entry mentioning free local {index} was not refused for it: {verdicts:?}",
        );
    }
}

/// Every position of a definition is held closed: a bound variable no scope in it binds is refused rather than aborted on.
///
/// `Var::bound` is public, so a module built by hand carries an occurrence no binder of its own opens. No boundary saw one, because every one asks `Mentions`, which is built from `var.as_free()` and answers nothing for a bound occurrence — and a position no rule types, a stated value sort, is met by nothing at all, where [the board](../../../documentation/design/soundness/the-soundness-board.md) requires a refusal. The last case is the one no arity check sees either: the index is loose *past* a binder, so the scope it sits under is a legitimate one and only its depth is wrong. Mutation-checked: with the boundary dropped, the stated value sort is admitted, and a body and a type are refused by `infer`'s own `Var` rule instead.
#[test]
fn a_definition_position_holding_a_loose_bound_variable_is_refused() {
    for (position, text) in [
        ("a body", "let /held: #NatType = #{binding = Bound(0)} _;"),
        ("a type", "let /held: #{binding = Bound(0)} _ = #NatType;"),
        (
            "a stated value sort",
            "#{value_sort = #{binding = Bound(0)} _}\nlet /held: Type = #NatType;",
        ),
        // `let x : Nat = 0; <index 1>`: the block binds one binder and its tail reaches past it, which is why the tail's own arity is no answer to this.
        (
            "a body past a binder",
            "let /held: #NatType =\n    let x: #NatType = 0;\n    #{binding = Bound(0)} _;",
        ),
    ] {
        let verdicts = text_verdicts(text);

        assert!(
            unclosed(&verdicts, Some(held())),
            "{position} holding a loose bound variable was not refused for it: {verdicts:?}",
        );
    }
}

/// The entry is held closed as the module's items are, since it is judged in the same walk by the same rule.
#[test]
fn an_entry_holding_a_loose_bound_variable_is_refused() {
    let verdicts = text_verdicts("#{binding = Bound(0)} _\n: #NatType");

    assert!(
        unclosed(&verdicts, None),
        "an entry holding a loose bound variable was not refused for it: {verdicts:?}",
    );
}
