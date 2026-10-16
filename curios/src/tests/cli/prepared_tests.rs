//! Prepared routing and lookup evidence cannot be detached from their grammar.

use super::super::{error, run};

const DECLARATION: &str = r#"
    use /std/{Cli, Map, Option};
    use /std/Cli/{Grammar, Prepared};
    use /std/Cli/Grammar/{Named};
    let grammar = Grammar/flag(Named { long = "port", short = Option/none(), help = "" });
    let original = Cli/prepare(grammar);
"#;

#[test]
fn a_public_preparation_can_reuse_its_actual_correspondence_evidence() {
    let source = format!(
        r#"{DECLARATION}
        let same: Prepared({{}}) = Prepared {{
            grammar = original.grammar,
            metadata = original.metadata,
            lookup = original.lookup,
            @metadata_matches = original.metadata_matches,
            @lookup_matches = original.lookup_matches }};
        ()
    "#
    );
    assert_eq!(run(&source), b"");
}

#[test]
fn changing_metadata_requires_new_correspondence_evidence() {
    let refusal = error(&format!(
        r#"{DECLARATION}
        let bad = Prepared {{ ..original,
            metadata = Prepared/Metadata {{ named = [], positional = [] }} }};
        ()
    "#
    ));
    assert!(refusal.contains("metadata_matches"), "{refusal}");
}

#[test]
fn changing_lookup_requires_new_correspondence_evidence() {
    let refusal = error(&format!(
        r#"{DECLARATION}
        let bad = Prepared {{ ..original, lookup = Map/empty(), @metadata_matches = original.metadata_matches }};
        ()
    "#
    ));
    assert!(refusal.contains("lookup_matches"), "{refusal}");
}

#[test]
fn metadata_of_another_grammar_requires_new_correspondence_evidence() {
    let refusal = error(&format!(
        r#"{DECLARATION}
        let other = Grammar/flag(Named {{ long = "different", short = Option/none(), help = "" }});
        let bad = Prepared {{ ..original, metadata = Cli/prepare(other).metadata }};
        ()
    "#
    ));
    assert!(refusal.contains("metadata_matches"), "{refusal}");
}

#[test]
fn an_identifier_cannot_reach_past_its_root_bound() {
    let refusal = error(
        r#"
        use /std/Cli/Prepared/{Id};
        let bad: Id(2) = Id { value = 2 };
        ()
    "#,
    );
    assert!(
        refusal.contains("within") && refusal.contains("False"),
        "{refusal}"
    );
}
