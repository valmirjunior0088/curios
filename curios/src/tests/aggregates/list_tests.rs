//! `List/each` under `Io`, whose claim is the order its effects reach the host. The pure `List` surface — predicates, searches, builders, `traverse` and the stable sort — is the corpus's `/aggregates/list`.

use crate::tests::run;

// `each` sequences under a `Monad`, and over `Io` the effects run in element order — which only host output can witness.
#[test]
fn each_runs_its_effects_in_element_order() {
    let source = r#"
        use /std/{Nat, List, Io};
        List/each([1, 2, 3], (x: Nat) => /std/print(Nat/to_str(x)))
        "#;

    assert_eq!(run(source), b"123");
}
