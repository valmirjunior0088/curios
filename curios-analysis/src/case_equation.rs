//! Which spellings of a scrutinee carry an arm's case equation: the one rule both checkers record equations by.

#[cfg(test)]
mod tests;

use curios_core::Term;

/// Whether an arm's case equation may be recorded under `spelling` — the scrutinee as the kernel is handed it, local definitions substituted, or a spelling a dispatch resolves it to: only where it mentions a local.
///
/// **The kernel's evaluation memos are why.** A local-free term's reduct is remembered for the whole declaration, so an equation about one would leave an entry resting on an equation the arm's exit retracts; a local-bearing term's entry is cleared with the equations in force. The rule costs nothing a program needs: a local-free scrutinee reduces to its case value rather than sticking, so an arm under one is either the arm reduction takes or dead.
///
/// **Both checkers call it, and the elaborator on the kernel's spelling.** An equation one checker records and the other does not reads one arm two ways, and the arm is always a dead one: a proof the elaborator accepted there, under a guard over a top-level name, a global's projection, a local definition of a closed term or a dispatch whose method ignores its locals, is one the kernel refuses.
pub fn records_case_equation(spelling: &Term) -> bool {
    spelling.has_local_free()
}
