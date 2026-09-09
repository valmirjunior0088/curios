//! The subsumption relation as a surface program reaches it: cumulative codomains, invariant domains.
//!
//! The rule is `documentation/soundness/per-term-rules/subsumption-and-level-entailment.md`, and until the elaborator decided the relation structurally these were the fixtures it could not have: checking a λ against a Π pushes the comparison to the leaves, so a head-only rule sufficed and no program formed the Π being subsumed. Passing a *name* forms it, which is what these compile.
//!
//! Both checkers decide the same relation, and `curios_cert`'s `kernel::infer::sort_tests` puts the same two propositions to the kernel under these names. Rename both or neither.

use crate::tests::{error, run};

// The direction the language needs. `Bool/Holds` is `(b : Bool) -> Prop`, handed to a slot wanting `(Bool) -> Type`; `/std/BigNat`'s `canonical_of_is_trimmed` passes it to `Eq/subst` exactly so.
#[test]
fn a_function_types_codomain_is_cumulative() {
    let source = r#"
        use /std/{Bool, True, print};

        let apply(motive: (Bool) -> Type, b: Bool) -> Type =
            motive(b);

        let witnessed: apply(Bool/Holds, true) =
            True/qed();

        let _ = witnessed;
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// The other half of the same fork, and the one that can be wrong in the admitting direction: a covariant domain would hand a function that takes only propositions a type. Asserted separately from the codomain fixture because invariance is a conjunction of refusals — a relation drifted to covariance still refuses the other direction.
#[test]
fn a_function_types_domain_is_invariant() {
    let wider_domain = r#"
        use /std/{Bool, print};

        let takes(motive: (Prop) -> Type) -> Type =
            motive(Bool/Holds(true));

        let given(t: Type) -> Type =
            t;

        let _bad: Type = takes(given);
        print("")
        "#;
    let report = error(wider_domain);
    assert!(
        report.contains("type mismatch")
            && report.contains("inferred: (t: Type) -> Type")
            && report.contains("expected: (Prop) -> Type"),
        "{report}"
    );

    let narrower_domain = r#"
        use /std/{Bool, print};

        let takes(motive: (Type) -> Type) -> Type =
            motive(Type);

        let given(p: Prop) -> Type =
            p;

        let _bad: Type = takes(given);
        print("")
        "#;
    let report = error(narrower_domain);
    assert!(report.contains("type mismatch"), "{report}");
}

// The control. A relation that had stopped descending into function types would satisfy neither fixture above while the head rule stayed green, and a relation that refused every function type would satisfy the domain fixture alone.
#[test]
fn the_head_rules_still_decide_a_bare_sort() {
    let source = r#"
        use /std/{Bool, print};

        let holds: Type =
            Bool/Holds(true);

        let _ = holds;
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}
