//! The deterministic fuel that bounds compile-time evaluation.
//!
//! Every limit is a fixed constant, so a compile is reproducible regardless of machine or timing. A format-string parse over a short literal runs hundreds of steps and its UTF-8 revalidation a few thousand, so the per-candidate step budget is generous while the shared pass pool caps the whole-module cost. The call-depth cap bounds native recursion (each interpreted call recurses into a host Rust frame); the reify caps bound one replacement. Exhaustion bails to leave the candidate untouched — never a panic.

use super::value::Bail;

/// Per-candidate step budget.
pub(super) const STEP_BUDGET: usize = 50_000;

/// Shared step pool for the whole pass — the hard compile-time ceiling.
pub(super) const PASS_BUDGET: usize = 500_000;

/// Call-nesting cap: each interpreted call recurses into a host Rust frame.
pub(super) const MAX_CALL_DEPTH: usize = 256;

/// Caps on one replacement: materialized nodes, and packed payload bytes plus list elements.
pub(super) const MAX_REIFY_NODES: usize = 2_048;
pub(super) const MAX_REIFY_BYTES: usize = 65_536;

/// The step fuel shared across a pass, plus the per-candidate counters.
pub(super) struct Budget {
    pool: usize,
    steps: usize,
    depth: usize,
}

impl Budget {
    pub(super) fn new() -> Self {
        Self {
            pool: PASS_BUDGET,
            steps: 0,
            depth: 0,
        }
    }

    /// Reset the per-candidate counters; the shared pool carries over.
    pub(super) fn restart(&mut self) {
        self.steps = 0;
        self.depth = 0;
    }

    pub(super) fn charge(&mut self) -> Result<(), Bail> {
        if self.pool == 0 || self.steps >= STEP_BUDGET {
            return Err(Bail::Fuel);
        }
        self.pool -= 1;
        self.steps += 1;
        Ok(())
    }

    pub(super) fn enter(&mut self) -> Result<(), Bail> {
        if self.depth >= MAX_CALL_DEPTH {
            return Err(Bail::Depth);
        }
        self.depth += 1;
        Ok(())
    }

    pub(super) fn leave(&mut self) {
        self.depth -= 1;
    }
}

/// The node and payload caps on one reified replacement.
pub(super) struct ReifyBudget {
    nodes: usize,
    payload: usize,
}

impl ReifyBudget {
    pub(super) fn new() -> Self {
        Self {
            nodes: MAX_REIFY_NODES,
            payload: MAX_REIFY_BYTES,
        }
    }

    pub(super) fn node(&mut self) -> Result<(), Bail> {
        if self.nodes == 0 {
            return Err(Bail::TooBig);
        }
        self.nodes -= 1;
        Ok(())
    }

    pub(super) fn payload(&mut self, amount: usize) -> Result<(), Bail> {
        if amount > self.payload {
            return Err(Bail::TooBig);
        }
        self.payload -= amount;
        Ok(())
    }
}
