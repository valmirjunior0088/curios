//! What became of each function of a source declaration's text, read off the CPS graph on either side of the optimizer.
//!
//! A function carries the function of a declaration's text it was made of — its [`Lineage`], stamped by erasure and kept on every copy the optimizer makes — so the optimizer's effect on one is a difference of two counts: how many functions of its lineage there were before, and how many there are after. Code of no declaration's text, the runtime's and a crossing's machinery, carries none, and is counted by its hint, which is all that tells it apart; a hint is otherwise display only, so two locals of one name in two declarations are two rows.
//!
//! **Nothing here instruments a pass.** No rewrite records what it did, and none has to: `curios-pipeline` already observes the graph before and after optimization, so a caller that counts both sides learns every fate below without the optimizer knowing it is being watched. That is what keeps the measured program the shipped program — the failure GHC's `-fprof-late` exists to undo, avoided by not annotating rather than by annotating late.
//!
//! **What a difference cannot tell apart** is named in [`Outcome::Absorbed`]: inlining and pruning both remove a function, and separating them needs the passes to say which, not the counts.

use {
    crate::Module,
    curios_common::Lineage,
    std::collections::{BTreeMap, BTreeSet},
};

/// What the optimizer did to one function of a source declaration's text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
    /// As many functions are of its lineage as were before: it is in the compiled program once per way it was already written.
    Survived,
    /// More functions are of its lineage than were before, because a pass cloned it — specialization on a known argument, on a call pattern, or on a branch. `copies` is how many stand where one did.
    Specialized {
        /// Functions of its lineage after optimization, against one before.
        copies: usize,
    },
    /// No function is of its lineage any more: it was inlined into its callers, or pruned as unreachable once something else was.
    ///
    /// **This is a finding, not a gap.** A function that is absorbed costs nothing of its own at run time — whatever it does is paid for wherever it went — and that is usually the answer someone profiling it was looking for.
    Absorbed,
}

/// One row: the function as a reader finds it, and what became of it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fate {
    /// The hint the first function counted under it bore before the optimizer: a declaration's own path, an anonymous function's derived `owner/n`, a local's binder, or the hint of the runtime's code a row of no lineage counts.
    pub name: String,
    /// What the optimizer did to it.
    pub outcome: Outcome,
}

/// What a row of the survey counts: the functions of one lineage, or, for code of no declaration's text, the functions bearing one hint.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Counted {
    Lineage(Lineage),
    Hint(String),
}

/// How many functions in `module` each row counts, beside the hint the first of them bears.
///
/// A heap twin the frame bound makes runs its original on the heap and is no copy of it, so it is not counted. Nor is a function with no hint, whatever it was made of: a row is labelled by its hint, and an unnamed function — the stand-in `(_) => 0` erasure makes of a type, a compiler-minted shell — has none to be told by.
pub fn descendants(module: &Module) -> BTreeMap<Counted, (String, usize)> {
    let twins = module
        .function_ids()
        .filter_map(|function| module.twin(function))
        .collect::<BTreeSet<_>>();
    let mut counts = BTreeMap::new();

    for id in module.function_ids() {
        let Some(function) = module.function(id).filter(|_| !twins.contains(&id)) else {
            continue;
        };
        let Some(name) = &function.debug_name else {
            continue;
        };
        let counted = function
            .lineage
            .map_or_else(|| Counted::Hint(name.clone()), Counted::Lineage);

        counts.entry(counted).or_insert((name.clone(), 0)).1 += 1;
    }

    counts
}

/// What the optimizer did to every row counted before it, from the [`descendants`] counts on either side of it.
///
/// Ordered by name, because a profile that reproduces is the point and a map's order is not one; rows of one name stand in the order of what they count.
pub fn fates(
    before: &BTreeMap<Counted, (String, usize)>,
    after: &BTreeMap<Counted, (String, usize)>,
) -> Vec<Fate> {
    let mut fates = before
        .iter()
        .map(|(counted, (name, was))| {
            let now = after.get(counted).map_or(0, |(_, now)| *now);
            let outcome = match now {
                0 => Outcome::Absorbed,
                now if now > *was => Outcome::Specialized { copies: now },
                _ => Outcome::Survived,
            };

            Fate {
                name: name.clone(),
                outcome,
            }
        })
        .collect::<Vec<_>>();
    fates.sort_by(|one, other| one.name.cmp(&other.name));

    fates
}

#[cfg(test)]
mod tests;
