//! What both checkers share about an arm's case equation: which spellings of a scrutinee carry one, how far a dispatch is opened to resolve its spelling, and which recorded equation a probe could reach.

#[cfg(test)]
mod tests;

use curios_core::Term;

/// How many application layers a dispatched scrutinee is opened through to reach the spelling it resolves to: each step consumes one layer of an elaborated dispatch, and a spine that has not settled in this many is not a dispatch.
///
/// One number because the checkers answer the same occurrences at the same point only while they resolve the same spellings: the kernel's `resolved_spelling`, the elaborator's `spine_whnf` and its `open_until` all stop here.
pub const RESOLVED_SPELLING_LAYERS: usize = 16;

/// Whether an arm's case equation may be recorded under `spelling` — the scrutinee as the kernel is handed it, local definitions substituted, or a spelling a dispatch resolves it to: only where it mentions a local.
///
/// **The kernel's evaluation memos are why.** A local-free term's reduct is remembered for the whole declaration, so an equation about one would leave an entry resting on an equation the arm's exit retracts; a local-bearing term's entry is cleared with the equations in force. The rule costs nothing a program needs: a local-free scrutinee reduces to its case value rather than sticking, so an arm under one is either the arm reduction takes or dead.
///
/// **Both checkers call it, and the elaborator on the kernel's spelling.** An equation one checker records and the other does not reads one arm two ways, and the arm is always a dead one: a proof the elaborator accepted there, under a guard over a top-level name, a global's projection, a local definition of a closed term or a dispatch whose method ignores its locals, is one the kernel refuses.
pub fn records_case_equation(spelling: &Term) -> bool {
    spelling.has_local_free()
}

/// Whether reducing `key` could possibly produce `candidate` — a necessary condition, tested without reducing anything.
///
/// Reduction substitutes only closed definition bodies and subterms of the term it is reducing, so it can introduce a *global* name and can drop a local, but can never introduce a local the term did not already mention. A candidate naming a binder the key does not is therefore one no reduct of the key will ever equal, whatever the key reduces to.
///
/// That makes this a filter and not a rule: every candidate an eager key would match still passes it, because such a candidate *is* a reduct of the key. What it excludes is the traffic — every stuck form produced under a binder some other judgment opened, which is most of what a probe at a stuck reduct sees.
///
/// Globals are deliberately not tested, and the asymmetry is the point: a reduct's globals are not bounded by the key's, so testing them would exclude exactly the unfoldings a settlement exists to perform.
///
/// Being a filter, relaxing it to admit everything changes no verdict and moves no fixture — what it moves is `curios`' `scrutinee_refinement_measurements`, from flat back to the exponential this whole key exists to remove. Tightening it does change verdicts, silently, by dropping refinements; `curios-cert`'s `whnf::equations_tests::a_reduct_that_drops_a_local_is_still_reached` is the guard on that direction.
pub fn could_reduce_to(key: &Term, candidate: &Term) -> bool {
    let allowed = key.free_vars_shared();

    candidate
        .free_vars_shared()
        .iter()
        .filter(|name| name.is_local())
        .all(|name| allowed.contains(name))
}
