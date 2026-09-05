//! A sealed concept admits no foreign witness, and still resolves and projects across modules.

use crate::tests::{error, run};

// A sealed concept (`: Type`, no `pub` on the representation) rejects a witness declared outside its module: the satisfy body is a dictionary literal, and construction requires the representation.
#[test]
fn concept_rejects_foreign_satisfy() {
    let source = r#"
        use /std/{Nat, Str};
        mod Guard
            use /std/{Nat, Str};
            pub concept Tag(A : Type) : Type {
                tag(A) -> Str
            }
            satisfy Tag(Nat) {
                tag(n) = Nat/to_str(n)
            }
        end
        use Guard/{Tag};
        satisfy Tag(Str) {
            tag(s) = s
        }
        /std/print("no")
        "#;

    assert!(error(source).contains("private"));
}

// A sealed concept also rejects a forged dictionary literal outside its module — the local-override idiom is only available on transparent concepts.
#[test]
fn concept_rejects_foreign_dictionary_literal() {
    let source = r#"
        use /std/{Nat, Str};
        mod Guard
            use /std/{Nat, Str};
            pub concept Tag(A : Type) : Type {
                tag(A) -> Str
            }
            satisfy Tag(Nat) {
                tag(n) = Nat/to_str(n)
            }
        end
        use Guard/{Tag};
        let forged : Tag(Nat) = Tag { tag(n) = "forged" };
        /std/print("no")
        "#;

    assert!(error(source).contains("private"));
}

// Sealing gates construction only: cross-module `use` parameters, global resolution, and the generated method wrappers (owner-module items) all keep working.
#[test]
fn concept_resolves_and_projects_cross_module() {
    let source = r#"
        use /std/{Nat, Str};
        mod Guard
            use /std/{Nat, Str};
            pub concept Tag(A : Type) : Type {
                tag(A) -> Str
            }
            satisfy Tag(Nat) {
                tag(n) = Nat/to_str(n)
            }
        end
        use Guard/{Tag};
        let describe(@A : Type, use Tag(A), x : A) -> Str = Tag/tag(x);
        /std/print(describe(42))
        "#;

    assert_eq!(run(source), b"42");
}

// A sealed concept with a superclass edge, resolved cross-module: the elaborator discharges the inner goal by projecting the local witness — a machinery-built projection of a private representation spliced into the consumer's body — and erasure re-derives its type with privacy suppressed. Regression test for the suppression bracket: with privacy enforced at erasure this program is spuriously rejected.
#[test]
fn concept_superclass_resolves_cross_module() {
    let source = r#"
        use /std/{Nat, Bool};
        mod Guard
            use /std/{Nat, Bool};
            pub concept Eq2(A : Type) : Type {
                eq2(A, A) -> Bool
            }
            pub concept Ord2(A : Type) : Type {
                use Eq2(A),
                le(A, A) -> Bool
            }
            satisfy Eq2(Nat) {
                eq2(a, b) = Nat/eql(a, b)
            }
            satisfy Ord2(Nat) {
                le(a, b) = true
            }
        end
        use Guard/{Eq2, Ord2};
        let same(@A : Type, use Ord2(A), x : A) -> Bool = Eq2/eq2(x, x);
        /std/print(Bool/to_str(same(7)))
        "#;

    assert_eq!(run(source), b"true");
}

// A sealed `Prop` concept is an owner-certified marker: only the owner mints witnesses, consumers demand the certificate as an erased premise, and the whole apparatus erases.
#[test]
fn prop_concept_certifies() {
    let source = r#"
        use /std/{Nat, Str, Eq};
        mod Guard
            use /std/{Nat, Eq};
            pub concept Certified(A : Type) : Prop {
                proof(x : A) -> Eq(x, x)
            }
            satisfy Certified(Nat) {
                proof(x) = Eq/refl()
            }
        end
        use Guard/{Certified};
        let ignore(p : Eq(2, 2), n : Nat) -> Nat = n;
        /std/print(Nat/to_str(ignore(Certified/proof(2), 3)))
        "#;

    assert_eq!(run(source), b"3");
}
