//! Every function of the shared layer a judgment of this crate calls, named here and nowhere else in it.
//!
//! What a verdict reaches of the layer both checkers share is this list: a function of `curios-analysis`, `curios-core`, `curios-algebra` or `curios-num` enters a judgment's reach by a line added here, and this module's test fails where another module of the crate names one for itself. What holds each stands where the function is — the audit of conversion's laws, a differential against the path it shortens, a test that its cap refuses — and the grade it is held to is `documentation/design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md`'s.
//!
//! **Functions, and not the representation.** The types of `curios-core` are what a term *is*, imported wherever one is built or read, and their methods come with them: a term's substitution, a telescope's opening, a level's arithmetic. No judgment fails to reach them, so they are not listed.
//!
//! **One instrument.** `take_looks` is the profile's count of what walks cost, read under the `profile` feature and by no verdict. It is here so the rule has no exception.

#[cfg(test)]
mod tests;

pub(crate) use {
    curios_analysis::{
        another_case, answers, answers_classed, atoms_within, case_target_indices, classable,
        connectives_agree, connectives_agree_classed, convert_classed, convert_intrinsics,
        could_reduce_to, decide, grade, member_arities, nonzero_by, pinned_instance,
        positivity_vectors, records_case_equation, refold, retyped, scrutinee_solution,
        solve_indices, spine, variance_vectors, wholly_pinned, yields_a_sort,
    },
    curios_core::{
        accelerable, foreign_signature, free_locals_outside, instantiate_qualities,
        instantiate_universes_scoped, open_quality, probe_spellings, reduce_closed,
        reduce_intrinsic, rewrite_universe_levels_scoped_shared, universe_metas,
    },
};

#[cfg(feature = "profile")]
pub(crate) use curios_core::take_looks;
