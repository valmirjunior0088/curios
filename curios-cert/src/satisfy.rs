//! The verdict on a universe context a walk is about to assume: whether it is closed, and whether it can be satisfied at all.
//!
//! The certifier *assumes* an item's [`UniverseContext`] while checking it — that is what lets a correct polymorphic definition through, since its recorded constraints are exactly the hypotheses its level questions need. An unsatisfiable set is therefore not a harmless oddity but a hypothesis set from which everything follows: `entails` starts proving whatever it is asked, `check_instance` stops discharging anything, and the universe discipline that keeps the paradox out stops applying.
//!
//! Both halves are decided once, on the data or in the shared layer. Whether a context is *closed* — every parameter index below the declared count and no level holding a metavariable — is [`UniverseContext::is_closed`], decided on the data it is about. Whether it is satisfiable is `curios-analysis`'s `satisfiable`, the decision both checkers read (`README.md`'s *The universe decisions are the shared layer's*).

use {crate::Error, curios_analysis::satisfiable, curios_core::UniverseContext};

/// What is wrong with a universe context a walk is about to assume, if anything.
///
/// Closure first: a context naming what it does not declare cannot be instantiated, so asking whether it has a solution would be asking about nothing.
///
/// Every context a walk assumes is decided here, wherever it is assumed from: a registry entry's and a definition's, each before [`certify_definitions`](crate::certify_definitions) judges anything. A group carries no context of its own, so a `rec` is no position of its own either — its members' levels are the enclosing declaration's, whose context was decided with every other. An unsatisfiable set assumed anywhere is the same hypothesis set from which everything follows, so the clause is one function rather than one per place that assumes one.
pub fn universe_verdict(context: &UniverseContext) -> Option<Error> {
    curios_common::profile!("universe_verdict");
    if !context.is_closed() {
        return Some(Error::UnclosedUniverses);
    }
    if !satisfiable(&context.constraints) {
        return Some(Error::UnsatisfiableUniverses);
    }

    None
}
