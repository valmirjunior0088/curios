//! The stance a surface judgment is made in.
//!
//! Two facts about *where* elaboration stands, each set for an extent and put back when the extent ends: the module whose item is being elaborated, which the representation-privacy checks judge a use site from, and whether the prover that discharges a bound from the facts in scope is running. Neither is a fact about a term: a context that elaborates nothing — one that only reads finished terms — is in no stance at all, and the privacy checks, which are a property of *surface* elaboration, are structurally off there.

use curios_utilities::Qualifier;

/// Where surface elaboration stands.
#[derive(Debug)]
pub(crate) struct Posture {
    /// The module whose item is currently being elaborated — the qualifier prefix of that item's name (a fresh context starts at the root, the empty qualifier). `None` means there is no surface use site to judge from, which suppresses the privacy checks structurally: machinery that re-derives types from already-elaborated terms — the metavariable oracle — walks compiler-built projections (witness splices, eta-expansions) that must not be re-adjudicated. A machinery path that forgets its bracket fails loudly (a spurious privacy error), never silently.
    island: Option<Qualifier>,
    /// Whether the procedure that proves a bound from the facts in scope is running. It does not run inside itself: what it elaborates is its own candidate, and a bound one of the candidate's operands carries is not the one it was asked about.
    entailing: bool,
}

impl Default for Posture {
    /// At the root module, with the prover idle.
    fn default() -> Self {
        Self {
            island: Some(Qualifier::empty()),
            entailing: false,
        }
    }
}

impl Posture {
    /// The module whose item is being elaborated, or `None` when no surface item is.
    pub(crate) fn island(&self) -> Option<&Qualifier> {
        self.island.as_ref()
    }

    /// Stand in `island`, or in none, answering where the context stood: the two halves of the bracket that suppresses the privacy checks, and the setter an item boundary uses.
    pub(crate) fn stand(&mut self, island: Option<Qualifier>) -> Option<Qualifier> {
        std::mem::replace(&mut self.island, island)
    }

    /// Whether the prover is running.
    pub(crate) fn entailing(&self) -> bool {
        self.entailing
    }

    /// Start or stop the prover's extent, answering whether it was running.
    pub(crate) fn set_entailing(&mut self, entailing: bool) -> bool {
        std::mem::replace(&mut self.entailing, entailing)
    }
}
