//! What `/std/Cli` refuses at compile time. The surface it *accepts* is the corpus unit's; a refusal is not a `Test` a program can declare, so these two stay here.

use super::error;

/// A name the specification does not contain is refused where it is written: `Has` walks to `False` and the bound holding `get` up cannot be discharged. The binding is unannotated so the bound is what fails — an expected type would report `Lookup`'s `{}` first and say nothing about why.
#[test]
fn a_misspelled_name_is_refused_rather_than_answered() {
    let refused = error(
        r#"
        use /std/{Cli, Str, Nat, List, print};

        let spec: List(Cli/Arg) = [Cli/default("port", Cli/nat, 8080, "Port to listen on")];
        let port: Nat = 1;
        let sample: Cli/Values(spec) = Cli/Values/cons(port, Cli/Values/nil());

        let read = Cli/get(sample, "prot");
        print("unreachable")
        "#,
    );

    assert!(
        refused.contains("Has"),
        "the refusal names the bound nothing discharged:\n{refused}"
    );
}

/// `WellFormed` is decided by reduction, so a specification naming one long name twice is refused at the entry that demands it rather than at the first line it would misparse.
#[test]
fn a_duplicate_long_name_is_refused_at_well_formed() {
    let refused = error(
        r#"
        use /std/{Cli, Str, Option, Io, print};

        let twice: Cli =
            Cli {
                name = "dup",
                about = "Two entries share a long name",
                version = Option/none(),
                args = [Cli/flag("verbose", "once"), Cli/flag("verbose", "twice")],
                run(_) = print("never"),
                commands = [],
            };

        let demand(c: Cli, @ok: Cli/WellFormed(c)) -> Io({}) = print(c.name);

        demand(twice)
        "#,
    );

    assert!(
        refused.contains("WellFormed"),
        "the refusal names the bound nothing discharged:\n{refused}"
    );
}
