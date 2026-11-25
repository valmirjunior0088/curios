//! The look count under a recorder: what the checkers sample of it, declaration by declaration, is part of what a reading around their check reads.

use {
    crate::{test_support::*, *},
    curios_common::test_support::*,
    curios_core::looks,
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_pipeline::test_support::*,
    curios_profile::{Destination, fold_at, trace},
    curios_surface::Entrypoint,
};

// A reading of the look count takes nothing from another: the checkers' `term::looks` samples, read at every declaration's close while a recorder listens, are parts of the count a reading around the whole check reads, which never falls.
#[test]
fn a_check_s_samples_of_its_looks_are_part_of_a_reading_around_it() {
    let entrypoint = tower(Shape::Chain(Stands::Function), 12)
        .parse::<Entrypoint>()
        .expect("a tower parses");
    // The prelude is restored once a thread, and its restore is no part of the check.
    let _ = typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none());

    let root = Temporary::new("instrumented", "looks");
    let stream = root.join("stream.pftrace");
    let before = looks();
    let checked = trace(Destination::File(stream.clone()), TALLIES, || {
        typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none())
    })
    .expect("the stream opens");
    let around = looks()
        .checked_sub(before)
        .expect("the look count never falls");
    assert!(checked.is_ok(), "the tower checks");

    let sampled = fold_at(&stream)
        .expect("the stream folds")
        .samples
        .iter()
        .filter(|sample| sample.name == "term::looks")
        .map(|sample| sample.total)
        .sum::<u64>();
    assert!(sampled > 0, "the checkers sampled no looks");
    assert!(
        around >= sampled,
        "a reading around the check read {around} looks where its samples read {sampled}"
    );
}
