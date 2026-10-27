//! What a suite runs of the compile and no compile does: lowering and elaboration alone, without the rest of the fold.
//!
//! Compiled for this crate's tests alone, so no build that ships holds it.

use super::*;

/// The type-checking prologue of [`compile_entrypoint`] (and the tests' typecheck-only path): lower to core, elaborate (checking against the entrypoint's type when it carries one, else synthesizing), then zonk metavariable solutions in so the module is meta-free — the `elaborate → zonk` half of the `elaborate → zonk → erase` data flow. Elaboration is authoritative: it returns a rebuilt module (lambda domains solved, binders re-closed), and it is *that* module — not the lowered one — that zonk makes meta-free. `zonk` is also where an unsolved hole is rejected, so a program that merely *type-checks* is fully validated by the time this returns. Elaboration and zonking share one context (the solutions live in its solution store); the returned module is self-contained, so the caller's `erase` runs over a fresh one.
///
/// The `/sys`/`/std` prelude is neither lowered nor elaborated per call: prepared Text state is merged with the user graph, then the archived Core prelude is replayed into the elaboration context and only the entry's own items are type-checked.
pub fn elaborate_and_zonk<O>(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
    tail: EntryTail,
    observe: &mut O,
) -> Result<Elaborated, CompileError>
where
    O: FnMut(Stage<'_>),
{
    let lowered = lower_entry(predecessors, syntax, entrypoint, loader, observe)?;
    elaborate_lowered(budget, predecessors, syntax, lowered, tail, observe)
        .1
        .map_err(|refused| refused.reported)
        .and_then(|(elaborated, obligations)| match obligations.is_empty() {
            true => Ok(elaborated),
            // Raised as a compilation raises them: every one, as it reports.
            false => Err(CompileError::Failure(
                obligations
                    .into_iter()
                    .flat_map(|obligation| obligation.reported)
                    .collect(),
            )),
        })
}
