//! `List/each` under `Io`, whose claim is the order its effects reach the host. The pure `List` surface — predicates, searches, builders, `traverse` and the stable sort — is the corpus's `/aggregates/list`.

use {
    super::test_support::{ARRIVAL, LIST, reads_in_every_shape},
    crate::tests::{run, run_text},
    curios_runtime::MockHost,
};

// `each` sequences under a `Monad`, and over `Io` the effects run in element order — which only host output can witness.
#[test]
fn each_runs_its_effects_in_element_order() {
    let source = r#"
        use /std/{Nat, List, Io};
        List/each([1, 2, 3], (x: Nat) => /std/print(Nat/to_str(x)))
        "#;

    assert_eq!(run(source), b"123");
}

/// A list reads as its flat copy does, whatever its node's children are: the read that answers from a flat child is the packed grains' too, and every element of each shape, read upward and then downward, is the settled list's.
#[test]
fn a_list_reads_as_its_flat_copy_in_every_shape_its_children_take() {
    let (system, io) = MockHost::builder().stdin_chunks(vec![ARRIVAL]).build();

    run_text(&reads_in_every_shape(&LIST), system).expect("the reads run");
    assert_eq!(io.output(), b"111111111");
}
