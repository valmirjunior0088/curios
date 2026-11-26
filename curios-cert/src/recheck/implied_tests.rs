//! What a declaration's form implies, as the certifier takes it, written in Curios/Core and put with nothing in scope: a family's former and its constructors are names the form declares, made as its entry makes them, so no definition stands under one and no decorator states one otherwise; and a concept's method is no definition at all.

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
    let verdicts = core_text_refusals(&fill!(
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
    let verdicts = core_text_refusals(&fill!(
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

/// A family's one case, and a definition the text writes under its constructor's name, stating that it builds `False`, its body the case's injection.
const CONSTRUCTOR_STATED_AT_FALSE: &str = r#"
induct /T: pub Prop
| mk()
end
let /T/mk : /std/Bool/False =
    #{node = Variant}
    /T/mk();
/T/mk
"#;

/// A definition under a constructor's name declares the name a second time, the family's form declaring it already, and the walk refuses the module whole for it before judging anything.
#[test]
fn a_definition_under_a_constructors_name_is_a_second_declaration_of_it() {
    let verdicts = core_text_refusals(&fill!(
        "${PROPOSITIONS}\n${CONSTRUCTOR_STATED_AT_FALSE}",
        PROPOSITIONS,
        CONSTRUCTOR_STATED_AT_FALSE
    ));
    let constructor = Global::Authored(Qualifier::from(["T", "mk"]));

    assert_eq!(
        verdicts,
        [Refusal {
            name: Some(constructor),
            error: Error::Redeclared(constructor),
        }]
    );
}

/// A family of no case whose former the text would state as another proposition, one with a proof, and a match on a proof of the former's name over the family's no case: were the name read as the statement, the match would be one on `True`, and were it read as the node with the statement's proof, a proof of anything.
const FORMER_STATED_OTHERWISE: &str = r#"
#{type_ = Prop, body = /std/Bool/True}
induct /E: pub Prop
end
((x: /E) =>
    #{goal = /std/Bool/False}
    match x
    end)(
    #{node = Variant}
    /std/Bool/True/qed()
)
"#;

/// A former is made of its entry, so a form's decorator states no part of one: a type or a body stated for it is refused where the text is read, and no module holds the family.
#[test]
fn a_former_stated_otherwise_is_refused_as_it_is_read() {
    let read = put()
        .read(&fill!(
            "${PROPOSITIONS}\n${FORMER_STATED_OTHERWISE}",
            PROPOSITIONS,
            FORMER_STATED_OTHERWISE
        ))
        .map(|_| ());

    assert!(
        matches!(
            &read,
            Err(crate::test_support::Unread::Read(curios_surface::ReadError {
                refusal: curios_surface::Refusal::UnknownField(field),
                ..
            })) if field == "type_" || field == "body"
        ),
        "{read:?}"
    );
}

/// A family stating two cases under one tag, the second carrying a proof of `False`, closed with a match covering the tag once: each case implies a constructor under the same name.
const TWO_CASES_ONE_TAG: &str = r#"
induct /D: pub Type
| mk()
| mk(h: /std/Bool/False)
end
#{goal = /std/Bool/False}
match
    #{node = Variant}
    /D/mk()
| mk(h) => h
end
"#;

/// A family's cases are under distinct tags ([`curios_core::Constructors`]), so a form stating a second case under a tag implies no second constructor under its name: the reader refuses the case where it stands, and no module holds the family.
#[test]
fn two_cases_a_form_states_under_one_tag_are_refused_as_it_is_read() {
    let read = put()
        .read(&fill!(
            "${PROPOSITIONS}\n${TWO_CASES_ONE_TAG}",
            PROPOSITIONS,
            TWO_CASES_ONE_TAG
        ))
        .map(|_| ());

    assert!(
        matches!(
            &read,
            Err(crate::test_support::Unread::Read(curios_surface::ReadError {
                refusal: curios_surface::Refusal::RepeatedCase(tag),
                span: Some(_),
            })) if tag == "mk"
        ),
        "{read:?}"
    );
}

/// A former's name is typed as its entry spells it: `/Box`, a record of one parameter, is a function from a type to a type, so a definition of that type stands at it, and one claiming it is a type is refused under its own name.
#[test]
fn a_former_is_typed_as_its_entry_spells_it() {
    let verdicts = text_refusals(
        r#"
struct /Box(A: Type): pub Type {
    held: A,
}

let /boxing: (A: Type) -> Type = /Box;

let /claimed: Type = /Box;
"#,
    );

    assert!(
        matches!(
            &verdicts[..],
            [Refusal { name: Some(name), .. }] if *name == Global::Authored(Qualifier::from(["claimed"]))
        ),
        "{verdicts:?}"
    );
}
