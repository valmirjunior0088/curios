//! What the traversal modes cost over shared structure.

use {super::*, crate::*, curios_common::Qualifier};

/// **The universe-erased projection visits a shared subterm once**, which is the detector for a projection walking a DAG as the tree it expands to.
///
/// `compare_nat` matches two summands up to universe instances, projecting both through [`project_erased_universes`] at every comparison, and a reduct is a DAG whose tree expansion doubles per level. Unmemoized, the projection walks `2ⁿ` while the unit counter — which prices transitions and constructions, not re-walks of one node — sees a linear program, so a bound over a computed binary numeral has linear units and an exponential wall clock no budget can refuse.
///
/// **Read off the sharing the projection keeps rather than timed, so the defect fails it instead of running it.** A timed detector — a bound over a widening `BigNat` subject — binds a width to a machine: against the defect, sixteen further bits grind a workstation to a halt, and eight are too few to clear the fixed cost of the check around them. Here sixteen levels of self-application are a 17-node DAG with `2¹⁶` paths to its leaf — an instance, since a subterm with no universe data is not walked by this projection at all. Walked once, each node is rebuilt once, and both operands of every application in the projection are one node; walked once per path, each path rebuilds its own — milliseconds and a few megabytes either way, on any machine. Reproduce by setting `erasing_universes`'s memo to `Memo::None`.
#[test]
fn the_erased_projection_visits_a_shared_subterm_once() {
    let mut term = Term::instance(
        Global::Authored(Qualifier::from(["x"])),
        vec![Level::constant(0)],
    );
    for _ in 0..16 {
        term = Term::apply(term.clone(), [term]);
    }

    let mut projected = project_erased_universes(&term);
    for _ in 0..16 {
        let Subterm::Apply(Apply { head, arguments }) = &*projected else {
            panic!("the projection keeps the term's shape");
        };
        assert_eq!(
            head.identity(),
            arguments[0].term.identity(),
            "a shared subterm was walked once per path through it"
        );
        projected = head.clone();
    }
}
