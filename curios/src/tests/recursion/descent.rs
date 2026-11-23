//! What descends: a call through a local definition, graded wherever it stands and refused where it does not descend, a type reaching partial definitions, and a split of a lambda's binder applied on the spot.

use {crate::tests::test_support::*, curios_common::fill};

#[test]
fn a_descent_through_a_local_definition_is_graded_wherever_the_call_stands() {
    // The call's argument names a `let` bound before it. In the tail the walk reads it as its value; in a later binding's value it once read the binding as stored, and refused a proof the certifier accepts.
    let source = r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};
        let halved(n: Nat) -> Holds(0 <= n) =
            match n < 2
            | true => True/qed()
            | false => let half = n / 2; let _inner = halved(half); True/qed()
            end;
        let depth(n: Nat) -> Nat =
            match n < 2
            | true => 0
            | false => let half = n / 2; let below = depth(half); below + 1
            end;
        let Chosen: Type = match depth(5) | 2 => Nat | _ => Bool end;
        let chosen: Chosen = 7;
        /std/print(Nat/to_str(chosen + depth(9)))
        "#;

    assert_eq!(run(source), b"10");
}

#[test]
fn a_call_through_a_local_definition_that_does_not_descend_is_refused() {
    let source = r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};
        let grown(n: Nat) -> Holds(0 <= n) =
            match n < 2
            | true => True/qed()
            | false => let more = n + 1; let _inner = grown(more); True/qed()
            end;
        /std/print("unreachable")
        "#;

    assert!(error(source).contains("not known to terminate"));
}

/// A proof reaching two partial definitions is refused by the one written first, whichever spells first: renaming either moves nothing.
#[test]
fn a_proof_reaching_two_partial_definitions_names_the_first_written() {
    let refusal = |arguments: &str| {
        error(&fill!(
            r#"
            use /std/{Nat, Io};
            pub let zeta(n: Nat) -> Nat = zeta(n);
            pub let alpha(n: Nat) -> Nat = alpha(n);
            pub let F(_a: Nat, _b: Nat) -> Nat = 0;
            pub let x: /std/Bool/True =
                match F(${arguments}) | 0 => /std/Bool/True/qed() | _ => /std/Bool/True/qed() end;
            Io/pure(())
            "#,
            arguments,
        ))
    };

    assert!(refusal("zeta(0), alpha(0)").contains("reaches `zeta`"));
    assert!(refusal("alpha(0), zeta(0)").contains("reaches `alpha`"));
}

/// A proof that splits a lambda's binder, the lambda applied on the spot to the parameter or to a payload of it, descends on what the binder stands for, and both checkers hold it.
#[test]
fn a_proof_splitting_an_applied_lambdas_binder_descends_on_what_it_stands_for() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{True};
        pub let below(n: Nat) -> True =
            ((x: Nat) => match x | 0 => True/qed() | k + 1 => below(k) end)(n);
        pub let twice(n: Nat) -> True =
            match n
            | 0 => True/qed()
            | k + 1 => ((x: Nat) => match x | 0 => True/qed() | j + 1 => twice(j) end)(k)
            end;
        /std/print("held")
        "#;

    assert_eq!(run(source), b"held");
}
