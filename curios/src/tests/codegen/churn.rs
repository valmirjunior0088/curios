//! What `curios-native/README.md`'s decision "The heap is sized ahead of its churn" holds at every run: `churn`'s threaded-record loop allocates nothing, the empirical pin on its exit from the churn class, and a churning `chain` is heard to collect. Both count the engine's collections through `curios_native::test_support::collected`, which hears the engine's own announcements; the decompositions under differently sized heaps are the bench's.

use {crate::test_support::*, curios_native::test_support::*};

/// The cross-language workloads, read from the corpus so the probes measure the programs the results files time.
const CHAIN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/chain/chain.crs"
));

const CHURN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/churn/churn.crs"
));

/// The `churn` workload's threaded record travels as fields: over a million spread-update steps the emitted program allocates so little that the collector never runs, because continuation scalar replacement and the known-function field split erase the reconstruction the source spells. The workload's Curios column therefore prices dispatch and checked arithmetic rather than allocation, and a record update worth measuring is a record at rest — what `spines` and the census reach. Collection counts are deterministic, which is what makes the absence assertable rather than merely measurable; the anchor is cross-checked against the Rust and Node contestants.
#[test]
fn threaded_record_allocates_nothing() {
    let compiled = compiled_of(CHURN);

    let (output, collected) = collected(|| run_at(&compiled, 1_000_000));

    assert_eq!(output, b"863718\n", "churn(1000000) anchor");
    assert_eq!(
        collected.collections, 0,
        "the spread-update loop reached the collector"
    );
}

/// The count's positive control. `threaded_record_allocates_nothing` asserts an absence, and an absence read off a count that hears nothing holds whatever the program does, as one taken on the calling thread does of a module the engine runs on another. So a workload that reaches the collector is heard to: the chain's churn, whose collections `chain_collection_decomposition` decomposes.
#[test]
fn a_churning_chain_is_heard_to_collect() {
    let compiled = compiled_of(CHAIN);

    let (_, collected) = collected(|| run_at(&compiled, ROUNDS));

    assert!(
        collected.collections > 0,
        "the chain churned through {ROUNDS} rounds and no collection was heard"
    );
}
