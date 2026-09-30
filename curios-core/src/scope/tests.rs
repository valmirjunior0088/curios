//! What the traversal modes cost over shared structure.

use {
    super::*,
    crate::{InstanceHead, Term},
};

/// **The universe-erased projection visits a shared subterm once**, which is the detector for a projection walking a DAG as the tree it expands to.
///
/// `compare_nat` matches two summands up to universe instances, projecting both through [`project_erased_universes`] at every comparison, and a reduct is a DAG whose tree expansion doubles per level. Unmemoized, the projection walks `2ⁿ` while the unit counter — which prices transitions and constructions, not re-walks of one node — sees a linear program, so a bound over a computed binary numeral has linear units and an exponential wall clock no budget can refuse.
///
/// **Counted rather than timed, so the defect fails it instead of running it.** A timed detector — a bound over a widening `BigNat` subject — binds a width to a machine: against the defect, sixteen further bits grind a workstation to a halt, and eight are too few to clear the fixed cost of the check around them. Here sixteen levels of self-application are a 17-node DAG with `2¹⁶` paths to its leaf — an instance, since a subterm with no universe data is not walked by this projection at all — reached once memoized and 65 536 times not — milliseconds and a few megabytes either way, on any machine. Reproduce by setting `erasing_universes`'s memo to `Memo::None`.
#[test]
fn the_erased_projection_visits_a_shared_subterm_once() {
    let mut term = Term::instance(
        InstanceHead::Var(Var::free(Free::local(0, Some("x")))),
        vec![Level::constant(0)],
    );
    for _ in 0..16 {
        term = Term::apply(term.clone(), [term]);
    }

    let mut visits = 0;
    let _ = term.traverse(&mut Visit::erasing_universes(|_, _| {
        visits += 1;
        None
    }));

    assert_eq!(
        visits, 1,
        "a shared subterm was walked once per path through it"
    );
}
