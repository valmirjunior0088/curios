//! The bound behind `curios-emit/README.md`'s decision "A flattening extends the payload its first piece wrote last", whose figures the bench's `rope_alternation_collections` reads: a list grown an element a step and read in turn: at its first element (`rope_push_first.crs`) or by a window past it (`rope_push_drop.crs`), each of which a flattening reaches, against `rope_push_peek.crs`, which grows it alike and reads only the element it has just appended. Collections are counted from the engine's own records through `curios_native::test_support::collected`, as `churn.rs` counts them.

use {
    crate::test_support::*,
    curios_native::{Compiled, test_support::*},
};

/// Collections during one run at `n`, from the engine's own records.
fn collections(compiled: &Compiled, n: u64) -> usize {
    collected(|| run_at(compiled, n)).1.collections
}

/// An alternation of appends with reads of what a flattening reaches copies each element a bounded number of times, so at the largest size it collects about as rarely as its control, which flattens nothing — where one whose flattenings copied the whole run collected hundreds of times. Collection counts are deterministic, which is what makes the bound assertable.
#[test]
fn an_alternation_collects_about_as_rarely_as_its_control() {
    let [first, past, control] = alternation_programs();
    let largest = SIZES[SIZES.len() - 1];
    let floor = collections(&control.1, largest);

    for (label, compiled) in [first, past] {
        let count = collections(&compiled, largest);
        assert!(
            count <= floor + 4,
            "{label}: {count} collections at N = {largest}, its control {floor}"
        );
    }
}
