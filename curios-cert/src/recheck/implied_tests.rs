//! What a declaration's form implies, as the certifier takes it, written as Core text and put with nothing in scope: a family's former and constructors are definitions it checks as any other, under names declared once, typed by what the module holds under each name and never by what the form would have made, and a concept's method is no definition at all.

use {
    super::test_support::*,
    crate::*,
    curios_common::{Qualifier, fill},
    curios_core::Global,
};

/// A concept whose one method answers `False`.
const BOTTOMED: &str = "concept /C: pub Type { bot: /std/Bool/False }";

/// `/C/bot`, the path the method is named by.
fn method() -> Global {
    Global::Authored(Qualifier::from(["C", "bot"]))
}

/// A method named by its path is the node lowering writes before elaboration settles it on a dictionary, so the certifier meets it as elaboration-only: no constant stands at the method's type for it to be typed by.
#[test]
fn a_method_named_by_its_path_is_refused_as_elaboration_only() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${BOTTOMED}\n#{node = Method}\n/C/bot",
        PROPOSITIONS,
        BOTTOMED
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name.is_none() && matches!(verdict.error, Error::NotCore(_))),
        "{verdicts:?}"
    );
}

/// A method is its concept's field, and no definition: named as a declaration, its path names nothing the certifier holds.
#[test]
fn a_method_named_as_a_declaration_names_nothing() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${BOTTOMED}\n/C/bot",
        PROPOSITIONS,
        BOTTOMED
    ));

    assert!(
        verdicts.iter().any(|verdict| verdict.name.is_none()
            && matches!(verdict.error, Error::Undeclared(name) if name == method())),
        "{verdicts:?}"
    );
}

/// A family's one case, and a definition the text writes under the name its constructor would take, stating that it builds `False`: it stands in the implied constructor's place, its body the case's injection.
const CONSTRUCTOR_STATED_AT_FALSE: &str = r#"
induct /T: pub Prop
| mk()
end
let /T/mk : /std/Bool/False =
    #{node = Variant}
    /T/mk();
/T/mk
"#;

/// A definition under a constructor's name is checked as any definition is, and refused at the type it states: the closing term, which reads it at that type, is refused for nothing of its own, and the module for the definition alone.
#[test]
fn a_definition_under_a_constructors_name_is_checked_at_the_type_it_states() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${CONSTRUCTOR_STATED_AT_FALSE}",
        PROPOSITIONS,
        CONSTRUCTOR_STATED_AT_FALSE
    ));
    let constructor = Global::Authored(Qualifier::from(["T", "mk"]));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(name), error: Error::Mismatch { .. } }] if *name == constructor
        ),
        "{verdicts:?}"
    );
}

/// A family of no case whose former the text states as another proposition, one with a proof, and a match on a proof of the former's name over the family's no case: were the name read as the node its entry builds, the match would be a proof of anything.
const FORMER_STATED_OTHERWISE: &str = r#"
#{type_ = Prop, body = /std/Bool/True}
induct /E: pub Prop
end
((x: /E) =>
    #{result = Ambient(/std/Bool/False)}
    match x
    end)(
    #{node = Variant}
    /std/Bool/True/qed()
)
"#;

/// A former's name is what its definition states, checked as any definition is, and the family's node is what its entry states: the name unfolds to `True`, so the match is one on `True` with no arm for `qed`, and nothing is refused under `/E`.
#[test]
fn a_former_stated_otherwise_is_read_as_it_is_stated() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${FORMER_STATED_OTHERWISE}",
        PROPOSITIONS,
        FORMER_STATED_OTHERWISE
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict {
                name: None,
                error: Error::MissingArm { .. }
            }]
        ),
        "{verdicts:?}"
    );
}

/// A family stating two cases under one tag, the second carrying a proof of `False`, closed with a match covering the tag once: each case implies a constructor under the same name.
const TWO_CASES_ONE_TAG: &str = r#"
induct /D: pub Type
| mk()
| mk(h: /std/Bool/False)
end
#{result = Ambient(/std/Bool/False)}
match
    #{node = Variant}
    /D/mk()
| mk(h) => h
end
"#;

/// The second constructor a form implies under a name is a second declaration under it, refused before anything of the module is judged.
#[test]
fn two_constructors_a_form_implies_under_one_name_are_refused_for_it() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${TWO_CASES_ONE_TAG}",
        PROPOSITIONS,
        TWO_CASES_ONE_TAG
    ));
    let constructor = Global::Authored(Qualifier::from(["D", "mk"]));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(owner), error: Error::Redeclared(redeclared) }]
                if *owner == constructor && *redeclared == constructor
        ),
        "{verdicts:?}"
    );
}
