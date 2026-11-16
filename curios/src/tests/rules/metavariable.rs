//! No metavariable survives zonking into a position a checker reads.

use {super::test_support::*, crate::tests::test_support::*, curios_elab::Error};

// **A solution keeps the universe instance its spelling had.** `k`'s `@x` is born at the call, outside the arm whose scrutinee, `h(Eq()(0, 0))`, is refined to the case, and solved inside it against `w`'s type — with that refinement suppressed, since it was not born under it, so its solution holds outside the arm. Born inside, it would be solved under the refinement and never reach the suppressed spelling this pins. Under suppression the elaborator's reducer keeps the probe as spelled rather than the refinement's key, which erases universe instances by design, being a spelling to look up by: a value committed from the key would hold a bare `/std/Eq/Eq`, which the certifier refuses as an occurrence stating no instance.
#[test]
fn an_inferred_value_under_a_refined_proof_keeps_its_universe_instance() {
    assert_eq!(
        run(AN_INFERRED_VALUE_UNDER_A_REFINED_PROOF_KEEPS_ITS_UNIVERSE_INSTANCE),
        b"1"
    );
}

// Strict zonking's *extent*, which is where its soundness sits. Every declaration is zonked strictly as it is published (`zonk_published`), and the entry where its unit is finalized (`zonk_entry`), and every "was not inferred" diagnostic is the rule firing, so what matters is how far it reaches rather than whether it runs. The assumption is that no unsolved metavariable survives into the module, and the module has exactly four term-bearing places for one to survive in: a definition's type, a definition's body (with the entrypoint body walked separately from both), an `induct` registry telescope, and a `struct` field telescope. The fields strict zonking deliberately skips carry `Vec<String>`, `Vec<(usize, Global)>` and `BTreeSet<Global>`, so that concept metadata and witness markers hold no terms of their own is exact rather than approximate.
//
// The extent is where the soundness sits, because the assumption's second clause is that nothing can *later* be solved to a partial or negatively-occurring term. Positivity and the totality classification are decided of a declaration as it is published, on the item and entries zonking returned, and positivity reads a `Metavar` through `opaque`: its spine children at `Mixed`, and never its solution, which does not exist yet. A metavariable surviving into a registry telescope would therefore be analyzed as a hole while the term it is later solved to is analyzed not at all. Refusal before they are decided is what closes that, not the ordering by itself.
//
// Each fixture plants one unconstrained implicit in one of the four places, and all four are refused; the control is what keeps the row from being read as "declarations may not mention implicits" — it supplies the same argument in all four positions and requires the program to run. Each is derived from its own position with the argument supplied.
#[test]
fn a_metavariable_does_not_survive_into_an_induct_telescope() {
    control(
        r#"
        use /std/{Nat};

        let drop(@A : Type, n : Nat) -> Nat = n;

        induct Bad : pub Type
        | c(x : /std/Eq()(drop(@Nat, 0), 0))
        end

        /std/Io/pure(())
        "#,
        b"",
    )
    .refused(&[("drop(@Nat, 0)", "drop(0)")])
    .by_elaborator_in("/Bad", |error| {
        matches!(error, Error::UninferredImplicit { .. })
    });
}

#[test]
fn a_metavariable_does_not_survive_into_a_struct_field() {
    control(
        r#"
        use /std/{Nat};

        let drop(@A : Type, n : Nat) -> Nat = n;

        struct Bad : pub Type {
            x : /std/Eq()(drop(@Nat, 0), 0)
        }

        /std/Io/pure(())
        "#,
        b"",
    )
    .refused(&[("drop(@Nat, 0)", "drop(0)")])
    .by_elaborator_in("/Bad", |error| {
        matches!(error, Error::UninferredImplicit { .. })
    });
}

#[test]
fn a_metavariable_does_not_survive_a_definitions_type() {
    control(
        r#"
        use /std/{Nat};

        let drop(@A : Type, n : Nat) -> Nat = n;

        let f(x : /std/Eq()(drop(@Nat, 0), 0)) -> Nat = 0;

        /std/Io/pure(())
        "#,
        b"",
    )
    .refused(&[("drop(@Nat, 0)", "drop(0)")])
    .by_elaborator_in("/f", |error| {
        matches!(error, Error::UninferredImplicit { .. })
    });
}

#[test]
fn a_metavariable_does_not_survive_the_entrypoint_body() {
    control(
        r#"
        use /std/{Nat};

        let drop(@A : Type, n : Nat) -> Nat = n;

        /std/print(Nat/to_str(drop(@Nat, 0)))
        "#,
        b"0",
    )
    .refused(&[("drop(@Nat, 0)", "drop(0)")])
    .by_elaborator_in("entry", |error| {
        matches!(error, Error::UninferredImplicit { .. })
    });
}

#[test]
fn a_solved_metavariable_still_reaches_every_zonked_position() {
    assert_eq!(
        run(&format!(
            "{AN_UNCONSTRAINED_IMPLICIT}{A_SOLVED_METAVARIABLE_IN_EVERY_POSITION}"
        )),
        b"0"
    );
}
