//! A `Prop`-sorted concept resolves and then erases, and a witness must prove its concept's law.

use super::super::{error, run};

// A `Prop`-sorted concept: the witness is proof content and erases completely, and the method result is consumed in an erased argument slot. The runtime path never sees the concept apparatus.
#[test]
fn prop_concept_resolves_and_erases() {
    let source = r#"
        use /std/{Nat, Str, Eq, Handle};
        pub concept Refl(A : Type) : pub Prop {
            proof(x : A) -> Eq(x, x)
        }
        satisfy Refl(Nat) {
            proof(x) = Eq/refl()
        }
        let ignore_proof(p : Eq(2, 2), n : Nat) -> Nat = n;
        /std/print(Nat/to_str(ignore_proof(Refl/proof(2), 3)))
        "#;

    assert_eq!(run(source), b"3");
}

// A proof-returning method wrapper demanded by a top-level binding. The wrapper call returns an erased method, so the outer application's callee is proof content rather than a function — `erase_apply` collapses it to the unit constant (value-driven: a direct function reference like `/std/proc/exit` keeps its call). Regression test: this used to survive erasure as an application of an erased callee and panic `into_cont`.
#[test]
fn prop_method_in_top_level_binding_collapses() {
    let source = r#"
        use /std/{Nat, Eq, Handle};
        pub concept Refl(A : Type) : pub Prop {
            proof(x : A) -> Eq(x, x)
        }
        satisfy Refl(Nat) {
            proof(x) = Eq/refl()
        }
        let probe(@A : Type, x : A, use Refl(A)) -> Eq(x, x) = Refl/proof(x);
        let direct : Eq(2, 2) = Refl/proof(2);
        let routed : Eq(3, 3) = probe(3);
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// The `Type`-sorted twin: the concept record is kept, but the method's result is still a proposition, so the wrapper application collapses identically. Regression test: this used to reach runtime as a call of an erased unit and trap.
#[test]
fn type_concept_prop_method_binding_collapses() {
    let source = r#"
        use /std/{Nat, Eq, Handle};
        pub concept Refl(A : Type) : pub Type {
            proof(x : A) -> Eq(x, x)
        }
        satisfy Refl(Nat) {
            proof(x) = Eq/refl()
        }
        let evidence : Eq(2, 2) = Refl/proof(2);
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// The laws pattern: a `Prop` concept whose field quantifies over another concept with a `use` parameter (a verified interface). The witness supplies a proof (binding the `use` slot positionally), resolution supplies both witnesses at the call, and everything erases.
#[test]
fn prop_laws_concept_resolves() {
    let source = r#"
        use /std/{Nat, Str, Show, Eq, Handle};
        pub concept ShowLaws(A : Type) : pub Prop {
            stable(use Show(A), x : A) -> Eq(Show/show(x), Show/show(x))
        }
        satisfy ShowLaws(Nat) {
            stable(use w, x) = Eq/refl()
        }
        let take(q : Eq(Show/show(7), Show/show(7)), n : Nat) -> Nat = n;
        /std/print(Nat/to_str(take(ShowLaws/stable(7), 42)))
        "#;

    assert_eq!(run(source), b"42");
}

// A concept's field telescope is dependent, so a field may constrain the fields declared before it. `law` is a proposition about `op`, which makes registering a witness an obligation to *prove* the law of that witness's own implementation. Identity is idempotent by reduction, so `Eq/refl` discharges it.
#[test]
fn a_witness_must_prove_its_concepts_law() {
    let source = r#"
        use /std/{Nat, Eq, Handle, Str};
        pub concept Idem(A : Type) : pub Type {
            op(A) -> A,
            law(x : A) -> Eq(op(op(x)), op(x)),
        }
        satisfy Idem(Nat) {
            op(x) = x,
            law(x) = Eq/refl(),
        }
        let n : Nat = 42;
        /std/print(Nat/to_str(Idem/op(n)))
        "#;

    assert_eq!(run(source), b"42");
}

// The same law against an implementation that breaks it: `op(x) = x + 1` reduces `op(op(x))` to `x + 2` where the law demands `x + 1`, so the witness must be refused. The assertion names `type mismatch` rather than merely requiring some error, because this program failed with `unbound variable` while method wrappers re-lowered their field types in a scope binding no sibling — an error that arrives before the law is ever checked, and would otherwise pass for the wrong reason.
#[test]
fn a_witness_violating_its_concepts_law_is_rejected() {
    let source = r#"
        use /std/{Nat, Eq, Handle, Str};
        pub concept Idem(A : Type) : pub Type {
            op(A) -> A,
            law(x : A) -> Eq(op(op(x)), op(x)),
        }
        satisfy Idem(Nat) {
            op(x) = x + 1,
            law(x) = Eq/refl(),
        }
        let n : Nat = 1;
        /std/print(Nat/to_str(Idem/op(n)))
        "#;

    let message = error(source);
    assert!(message.contains("type mismatch"), "{message}");
}
