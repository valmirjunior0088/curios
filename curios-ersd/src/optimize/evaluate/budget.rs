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

/// The heaviest copy a description closure may reify. A performance is small by construction — a write is one foreign call, a bind two forces, a pure a return — so a description whose copy extent runs past this is carrying program code: a sequencing chain's continuation drags its whole suffix along as extent, and copying that per level per round is the compounding this cap exists to stop. The staged-formatter residuals the collapse pins protect weigh a few dozen nodes; a chain's first link weighs its program.
pub(crate) const DESCRIPTION_COPY_NODE_LIMIT: usize = 128;

/// Shared node pool for a whole reification pass — the growth analogue of [`PASS_BUDGET`].
///
/// [`MAX_REIFY_NODES`] bounds one replacement; nothing bounded how many replacements a pass performs, so a round could reify thousands of times and multiply the module, and the next round then walked the multiplied module. Measured before this existed, on a one-line program whose prelude had been rewritten in combinator style: 23,822 live values after round 0, 62,879 after round 1, and 1,539,000 after round 2, with the round times tracking the size at 1.5 s, 5.8 s and 30.7 s. The eight-round loop above never finished.
///
/// Steps were already pooled across a pass and growth was not, which is the asymmetry this closes. Exhaustion stops further replacements for the pass; partial evaluation that folds less is always sound, so the bound costs optimization rather than correctness.
pub(super) const PASS_REIFY_BUDGET: usize = 100_000;

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

/// What one replacement may materialize: its own shape cap, its payload cap, and its slice of the pass pool.
///
/// The two node quantities answer different questions and neither substitutes for the other. `nodes` bounds the *shape* of the value being built, so a single value cannot be arbitrarily wide; `pool` is what the whole pass has left, so replacements cannot multiply the module between them. A deep-copied closure region is not part of a value's shape, so it charges the pool alone -- but it must still *refuse* when the pool cannot afford it, which is the half that made the difference between bounding growth and merely observing it.
pub(super) struct ReifyBudget {
    nodes: usize,
    payload: usize,
    pool: usize,
    spent: usize,
}

impl ReifyBudget {
    /// One replacement's own caps, drawing on all of [`MAX_REIFY_NODES`].
    pub(super) fn new() -> Self {
        Self::within(MAX_REIFY_NODES)
    }

    /// One replacement bounded by both its own cap and what the pass's shared pool has left.
    pub(super) fn within(pool: usize) -> Self {
        Self::within_capped(pool, MAX_REIFY_NODES)
    }

    /// [`Self::within`] under a tighter node cap — the description-containing candidate's budget, [`DESCRIPTION_COPY_NODE_LIMIT`].
    pub(super) fn within_capped(pool: usize, nodes: usize) -> Self {
        Self {
            nodes,
            payload: MAX_REIFY_BYTES,
            pool,
            spent: 0,
        }
    }

    /// Nodes this replacement materialized, to charge against the pass pool.
    pub(super) fn spent(&self) -> usize {
        self.spent
    }

    /// Charge `amount` against both this replacement's cap and the pass pool.
    ///
    /// A deep-copied closure region is charged here rather than to the pool alone, and that is a *measured* choice rather than a tidy one. Charging it to the pool alone declines fewer replacements, which sounds better and is much worse: the declines are what stop `deep_copy_function` running over large regions, and removing them took one program's lowering from 3.2 s to 63 s while the module reached 204,000 statements. The cap refuses early and cheaply; that is its value here.
    pub(super) fn bulk(&mut self, amount: usize) -> Result<(), Bail> {
        if amount > self.nodes || amount > self.pool {
            return Err(Bail::TooBig);
        }
        self.nodes -= amount;
        self.pool -= amount;
        self.spent += amount;
        Ok(())
    }

    pub(super) fn node(&mut self) -> Result<(), Bail> {
        if self.nodes == 0 || self.pool == 0 {
            return Err(Bail::TooBig);
        }
        self.nodes -= 1;
        self.pool -= 1;
        self.spent += 1;
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
