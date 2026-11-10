//! The analyses both Curios checkers run, and the seam they run behind.
//!
//! Each is a total function of the terms and declarations it is handed, and what differs between the two checkers' inputs arrives through [`Env`]: reduction, the local context and unfolding through a local's definition, fresh binders, and the registry fallback for declarations outside the analyzed set. `curios-elab` implements it over its elaboration `Context`; `curios-cert` implements it over its `Kernel`. Why the rules are shared rather than duplicated, and what that costs, is `README.md`'s decision, and `documentation/design/soundness/` argues them on that understanding.
//!
//! These rules admit terms, so they are inside the trusted base, which `cargo tree -p curios-cert -e normal` enumerates; why they are a crate of their own is `README.md`'s decision.

mod judge;
use judge::*;
pub use judge::{Env, Judge};

mod case_equation;
pub use case_equation::{
    Answered, RESOLVED_SPELLING_LAYERS, ScrutineeRead, another_case, answers, answers_classed,
    could_reduce_to, holds_no_type, read_scrutinee, read_scrutinee_classed, records_case_equation,
};

mod classify;
use classify::*;
pub use classify::{
    Classes, Classification, Within, apart_in_instance, atoms_in, atoms_within, classed_in,
    is_node, one_up_to_instances, read_operands, refold,
};

mod conversion;
use conversion::*;
pub use conversion::{
    Agreement, Congruence, Driver, Obligation, Outcome, Pass, connectives_agree,
    connectives_agree_classed, convert_classed, convert_intrinsics, foreign_congruence,
};

mod invert;
pub use invert::{
    Invert, Pinned, case_target_indices, invert_indices, pinned_by_targets, pinned_instance,
    solve_indices, wholly_pinned,
};

mod specialize;
pub use specialize::{reads_solved, retyped, scrutinee_solution};

mod unfolding;
use unfolding::*;

mod positivity;
use positivity::*;
pub use positivity::{
    Declarations, DeclaredPart, MemberLabel, NotPositive, PositivityRefusal, positivity_vectors,
    struct_reaches_itself,
};

mod variance;
pub use variance::{HeldLevels, VarianceVectors, held_by, held_levels, variance_vectors};

mod totality;
pub use totality::{
    Call, SizeContext, binder_bounds, blocker, decide, grade, group_applications, group_totality,
    handed, known_to_terminate, member_arities, nonzero_by, refined_by_arm, spine,
    yields_a_sort_where,
};

mod erased;
pub use erased::Erased;

#[cfg(test)]
mod tests;

// A namespace rather than a root export, for `curios-native`'s `test_support` reason: `curios_analysis::test_support::SYNTAX` says at its use site that the caller reached for scaffolding rather than product API. The path is the warning label.
#[cfg(feature = "test-support")]
pub mod test_support;
