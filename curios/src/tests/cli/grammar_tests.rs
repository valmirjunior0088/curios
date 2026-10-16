//! Public grammar construction and dependent proof fields across the standard-library boundary.

use super::super::{error, run};

#[test]
fn a_public_grammar_literal_can_supply_its_structural_evidence() {
    assert_eq!(
        run(r#"
        use /std/{Cli, Option, Nat, List, Io, print};
        use /std/Cli/{Grammar};
        use /std/Cli/Grammar/{Node, Named};
        use /std/Bool/{True};
        let declared: Grammar({}) = Grammar {
            tree = Node/flag(Named { long = "port", short = Option/none(), help = "" }),
            @sound = True/qed() };
        print(Nat/to_str(List/len(Grammar/aliases(declared.tree))))
    "#),
        b"1"
    );
}

#[test]
fn a_duplicate_name_literal_requires_evidence_that_cannot_be_supplied() {
    let refusal = error(
        r#"
        use /std/{Option};
        use /std/Cli/{Grammar};
        use /std/Cli/Grammar/{Node, Named};
        let names: Named = Named { long = "same", short = Option/none(), help = "" };
        let bad: Grammar({{}, {}}) = Grammar { tree = Node/product(Node/flag(names), Node/flag(names)) };
        ()
    "#,
    );
    assert!(refusal.contains("hidden field 'sound'"), "{refusal}");
}

#[test]
fn changing_a_tree_does_not_keep_stale_soundness_evidence() {
    let refusal = error(
        r#"
        use /std/{Option};
        use /std/Cli/{Grammar};
        use /std/Cli/Grammar/{Node, Named};
        let original: Grammar({}) = Grammar/flag(Named { long = "port", short = Option/none(), help = "" });
        let bad: Grammar({}) = Grammar { ..original, tree = Node/flag(Named { long = "", short = Option/none(), help = "" }) };
        ()
    "#,
    );
    assert!(
        refusal.contains("hidden field 'sound'") && refusal.contains("False"),
        "{refusal}"
    );
}

#[test]
fn a_static_declaration_rejects_two_nullable_positional_suffixes() {
    let refusal = error(
        r#"
        use /std/{Cli};
        use /std/Cli/{Grammar};
        use /std/Cli/Grammar/{Positional};
        let first = Grammar/rest(Positional { name = "first", help = "" }, Cli/str);
        let second = Grammar/rest(Positional { name = "second", help = "" }, Cli/str);
        let bad = Grammar/product(first, second);
        ()
    "#,
    );
    assert!(
        refusal.contains("ordered") && refusal.contains("False"),
        "{refusal}"
    );
}

#[test]
fn a_static_declaration_rejects_repetition_of_a_composite_group() {
    let refusal = error(
        r#"
        use /std/{Option};
        use /std/Cli/{Grammar};
        use /std/Cli/Grammar/{Named};
        let first = Grammar/flag(Named { long = "first", short = Option/none(), help = "" });
        let second = Grammar/flag(Named { long = "second", short = Option/none(), help = "" });
        let bad = Grammar/repeat(Grammar/product(first, second));
        ()
    "#,
    );
    assert!(
        refusal.contains("leaf") && refusal.contains("False"),
        "{refusal}"
    );
}

#[test]
fn a_static_record_rejects_duplicate_labels_even_for_positional_fields() {
    let refusal = error(
        r#"
        use /std/{Cli};
        let bad = Cli/record([Cli/positional("same", Cli/str, ""), Cli/positional("same", Cli/str, "")]);
        ()
    "#,
    );
    assert!(
        refusal.contains("valid_fields") && refusal.contains("False"),
        "{refusal}"
    );
}
