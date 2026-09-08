//! What became of each source declaration, read off the Cont graph on either side of the optimizer.
//!
//! A function carries the name of the declaration it descends from — [`CpsFunction::debug_name`], set by erasure and copied onto every clone the optimizer makes, per `documentation/design/toolchain/one-naming-scheme-for-compiler-identities.md`. So the optimizer's effect on a declaration is a difference of two counts: how many functions bore its name before, and how many bear it after.
//!
//! **Nothing here instruments a pass.** No rewrite records what it did, and none has to: `curios-pipeline` already observes the graph before and after optimization, so a caller that counts both sides learns every fate below without the optimizer knowing it is being watched. That is what keeps the measured program the shipped program — the failure GHC's `-fprof-late` exists to undo, avoided by not annotating rather than by annotating late.
//!
//! **What a difference cannot tell apart** is named in [`Outcome::Absorbed`]: inlining and pruning both remove a name, and separating them needs the passes to say which, not the counts.

use {
    crate::{CpsFunction, CpsModule},
    std::collections::BTreeMap,
};

/// What the optimizer did to one source declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
    /// As many functions bear the name as did before: the declaration is in the compiled program once per way it was already written.
    Survived,
    /// More functions bear the name than did before, because a pass cloned it — specialization on a known argument, on a call pattern, or on a branch. `copies` is how many stand where one did.
    Specialized {
        /// Functions bearing the name after optimization, against one before.
        copies: usize,
    },
    /// No function bears the name any more: it was inlined into its callers, or pruned as unreachable once something else was.
    ///
    /// **This is a finding, not a gap.** A declaration that is absorbed costs nothing of its own at run time — whatever it does is paid for wherever it went — and that is usually the answer someone profiling it was looking for.
    Absorbed,
}

/// One declaration's row: the name as the author wrote it, and what became of it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fate {
    /// The declaration's fully qualified name, or an anonymous function's derived `owner/n`.
    pub name: String,
    /// What the optimizer did to it.
    pub outcome: Outcome,
}

/// How many functions in `module` descend from each named source declaration.
///
/// A function with no name descends from no declaration a reader could look up, so it is not counted. Erasure derives a name for every function it lifts — an anonymous one becomes `owner/n` — so an unnamed function is a compiler-minted shell rather than anything written.
pub fn descendants(module: &CpsModule) -> BTreeMap<String, usize> {
    let mut counts = BTreeMap::new();

    for function in module.functions().iter().flatten() {
        let CpsFunction {
            debug_name: Some(name),
            ..
        } = function
        else {
            continue;
        };

        *counts.entry(name.clone()).or_insert(0) += 1;
    }

    counts
}

/// What the optimizer did to every declaration, from the [`descendants`] counts on either side of it.
///
/// Ordered by name, because a profile that reproduces is the point and a map's order is not one.
pub fn fates(before: &BTreeMap<String, usize>, after: &BTreeMap<String, usize>) -> Vec<Fate> {
    before
        .iter()
        .map(|(name, &was)| {
            let now = after.get(name).copied().unwrap_or(0);
            let outcome = match now {
                0 => Outcome::Absorbed,
                now if now > was => Outcome::Specialized { copies: now },
                _ => Outcome::Survived,
            };

            Fate {
                name: name.clone(),
                outcome,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests;
