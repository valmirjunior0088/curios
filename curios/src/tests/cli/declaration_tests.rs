//! Transparent command trees require structural evidence for the exact declaration they contain.

use super::super::{error, run};

#[test]
fn a_public_command_tree_literal_can_supply_its_structural_evidence() {
    assert_eq!(
        run(r#"
        use /std/{Cli, Option, print};
        use /std/Cli/{Grammar};
        use /std/Bool/{True};
        let parent(child: Cli) -> Cli =
            Cli { tree = Cli/group("app", "", Option/none(), [child.tree]),
                @sound = (True/qed(), (True/qed(), child.sound, True/qed())) };
        let leaf: Cli = Cli {
            tree = Cli/leaf("child", "", Option/none(), Grammar/pure(()), (_) => print("ran")),
            @sound = (True/qed(), (True/qed(), True/qed(), True/qed())) };
        let app = parent(leaf);
        match Cli/select(app, ["child"])
        | chose(selected, _) => print(Cli/usage(["app"], selected))
        | _ => print("not selected")
        end
    "#),
        b"Usage: app child"
    );
}

#[test]
fn an_invalid_command_tree_literal_requires_evidence_that_cannot_be_supplied() {
    let refusal = error(
        r#"
        use /std/{Cli, Option, Io};
        use /std/Cli/{Grammar};
        let bad: Cli = Cli { tree = Cli/leaf("bad name", "", Option/none(), Grammar/pure(()), (_) => Io/pure(())) };
        ()
    "#,
    );
    assert!(
        refusal.contains("hidden field 'sound'") && refusal.contains("Cli/Sound"),
        "{refusal}"
    );
}

#[test]
fn changing_a_command_tree_does_not_keep_stale_soundness_evidence() {
    let refusal = error(
        r#"
        use /std/{Cli, Option, Io};
        use /std/Cli/{Grammar};
        let child = Cli/leaf("child", "", Option/none(), Grammar/pure(()), (_) => Io/pure(()));
        let original = Cli/declare(Cli/group("app", "", Option/none(), [child]));
        let bad = Cli { ..original, tree = Cli/group("app", "", Option/none(), [child, child]) };
        ()
    "#,
    );
    assert!(
        refusal.contains("hidden field 'sound'") && refusal.contains("Cli/Sound"),
        "{refusal}"
    );
}

#[test]
fn a_static_command_declaration_rejects_an_invalid_unselected_sibling() {
    let refusal = error(
        r#"
        use /std/{Cli, Option, Io};
        use /std/Cli/{Grammar};
        let good = Cli/leaf("good", "", Option/none(), Grammar/pure(()), (_) => Io/pure(()));
        let bad = Cli/leaf("bad name", "", Option/none(), Grammar/pure(()), (_) => Io/pure(()));
        let app = Cli/declare(Cli/group("app", "", Option/none(), [good, bad]));
        ()
    "#,
    );
    assert!(
        refusal.contains("Declared") && refusal.contains("False"),
        "{refusal}"
    );
}

#[test]
fn a_static_command_declaration_rejects_active_builtin_collisions() {
    let refusal = error(
        r#"
        use /std/{Cli, Option, Io};
        use /std/Cli/{Grammar};
        use /std/Cli/Grammar/{Named};
        let grammar = Grammar/switch(Named { long = "version", short = Option/none(), help = "" });
        let app = Cli/declare(Cli/leaf("app", "", Option/some("1"), grammar, (_) => Io/pure(())));
        ()
    "#,
    );
    assert!(
        refusal.contains("Declared") && refusal.contains("False"),
        "{refusal}"
    );
}

#[test]
fn standalone_certified_interpretation_requires_builtin_compatibility() {
    let refusal = error(
        r#"
        use /std/{Cli, Option};
        use /std/Bool/{True};
        use /std/Cli/{Grammar};
        use /std/Cli/Grammar/{Named};
        let prepared = Cli/Prepared/prepare(Grammar/switch(Named { long = "help", short = Option/none(), help = "" }));
        let reading = Cli/interpret/certified(prepared, ["--help"], Option/none(), @(True/qed(), True/qed(), True/qed()));
        ()
    "#,
    );
    assert!(
        refusal.contains("type mismatch") && refusal.contains("False"),
        "{refusal}"
    );
}
