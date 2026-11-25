//! The deterministic fuel that bounds compile-time evaluation.
//!
//! Every limit is a fixed constant, so a compile is reproducible regardless of machine or timing. A format-string parse over a short literal runs hundreds of steps and its UTF-8 revalidation a few thousand, so the per-candidate step budget is generous while the shared pass pool caps the whole-module cost. The call-depth cap bounds native recursion (each interpreted call that waits on its answer recurses into a host Rust frame, where a tail call takes its caller's place); the reify caps bound one replacement. Exhaustion bails to leave the candidate untouched — never a panic.

use super::Bail;

/// Per-candidate step budget.
pub const STEP_BUDGET: usize = 50_000;

/// Shared step pool for the whole pass — the hard compile-time ceiling.
pub const PASS_BUDGET: usize = 500_000;

/// Call-nesting cap: each interpreted call that waits on its answer recurses into a host Rust frame.
///
/// A call in its function body's tail takes no frame — it takes its caller's place in the loop running the body — so a walk written as a tail recursion meets the step budget and never this. What it bounds is a recursion whose calls wait, one deeper than this left unfolded; no candidate in `programs/` reaches it, the depth bails each round's profile ledger samples staying at zero.
pub const MAX_CALL_DEPTH: usize = 256;

/// Caps on one replacement: materialized nodes, and packed payload bytes plus list elements.
pub const MAX_REIFY_NODES: usize = 2_048;
pub const MAX_REIFY_BYTES: usize = 65_536;

/// Shared node pool for a whole reification pass — the growth analogue of [`PASS_BUDGET`].
///
/// [`MAX_REIFY_NODES`] bounds one replacement and says nothing about how many replacements a pass performs. Without this pool a round can reify thousands of times and multiply the module, and the next round walks the multiplied module: over a prelude written in combinator style the module grows by orders of magnitude within three rounds, and the eight-round loop never finishes.
///
/// Steps are pooled across a pass, and this pools growth the same way. Exhaustion stops further replacements for the pass; partial evaluation that folds less is always sound, so the bound costs optimization rather than correctness.
pub const PASS_REIFY_BUDGET: usize = 100_000;

/// The widest scalar a fold may build, in bits — [`MAX_REIFY_BYTES`] read in the unit a numeral grows in.
///
/// The erased carriers are unbounded, so multiplication and the left shifts can be asked to build without bound while costing one step each; a step budget cannot see that and a width can. A constant wider than one replacement may reify could not be placed in the module even if it were built, so this is the point past which folding is work with no possible use. Exhaustion here is a *decline*, like every other bound in this module: partial evaluation that folds less is always sound.
pub const MAX_FOLD_BITS: u64 = MAX_REIFY_BYTES as u64 * 8;

/// The longest list a fold may build, in elements — [`MAX_REIFY_BYTES`] read in the unit a list grows in, an element charged a replacement as a byte of payload is.
///
/// A concatenation costs one step whatever it joins, so a chain of them doubles a list past any memory there is in a few dozen steps, as a chain of products does a numeral. A `Bytes` is held to [`MAX_FOLD_BITS`] where the oracle folds it, and exhaustion here is a decline as it is there.
pub const MAX_FOLD_ELEMENTS: usize = MAX_REIFY_BYTES;

/// The step fuel shared across a pass, plus the per-candidate counters.
pub struct Budget {
    pool: usize,
    steps: usize,
    depth: usize,
}

impl Budget {
    pub fn new() -> Self {
        Self {
            pool: PASS_BUDGET,
            steps: 0,
            depth: 0,
        }
    }

    /// Reset the per-candidate counters; the shared pool carries over.
    pub fn restart(&mut self) {
        self.steps = 0;
        self.depth = 0;
    }

    pub fn charge(&mut self) -> Result<(), Bail> {
        if self.pool == 0 || self.steps >= STEP_BUDGET {
            return Err(Bail::Fuel);
        }
        self.pool -= 1;
        self.steps += 1;
        Ok(())
    }

    pub fn enter(&mut self) -> Result<(), Bail> {
        if self.depth >= MAX_CALL_DEPTH {
            return Err(Bail::Depth);
        }
        self.depth += 1;
        Ok(())
    }

    pub fn leave(&mut self) {
        self.depth -= 1;
    }
}

/// What one replacement may materialize: its own shape cap, its payload cap, and its slice of the pass pool.
///
/// The two node quantities answer different questions and neither substitutes for the other. `nodes` bounds the *shape* of the value being built, so a single value cannot be arbitrarily wide; `pool` is what the whole pass has left, so replacements cannot multiply the module between them. A deep-copied closure region is not part of a value's shape, so it charges the pool alone -- but it must still *refuse* when the pool cannot afford it, which is the half that bounds growth rather than merely observing it.
pub struct ReifyBudget {
    nodes: usize,
    payload: usize,
    pool: usize,
    spent: usize,
}

impl ReifyBudget {
    /// One replacement's own caps, drawing on all of [`MAX_REIFY_NODES`].
    pub fn new() -> Self {
        Self::within(MAX_REIFY_NODES)
    }

    /// One replacement bounded by both its own cap and what the pass's shared pool has left.
    pub fn within(pool: usize) -> Self {
        Self {
            nodes: MAX_REIFY_NODES,
            payload: MAX_REIFY_BYTES,
            pool,
            spent: 0,
        }
    }

    /// Nodes this replacement materialized, to charge against the pass pool.
    pub fn spent(&self) -> usize {
        self.spent
    }

    /// Charge `amount` against both this replacement's cap and the pass pool.
    ///
    /// A deep-copied closure region is charged here rather than to the pool alone. Charging it to the pool alone would decline fewer replacements, which sounds better and is much worse: the declines are what stop `deep_copy_function` running over large regions, and without them lowering grows faster than the module it lowers. The cap refuses early and cheaply; that is its value here.
    pub fn bulk(&mut self, amount: usize) -> Result<(), Bail> {
        if amount > self.nodes || amount > self.pool {
            return Err(Bail::TooBig);
        }
        self.nodes -= amount;
        self.pool -= amount;
        self.spent += amount;
        Ok(())
    }

    pub fn node(&mut self) -> Result<(), Bail> {
        if self.nodes == 0 || self.pool == 0 {
            return Err(Bail::TooBig);
        }
        self.nodes -= 1;
        self.pool -= 1;
        self.spent += 1;
        Ok(())
    }

    pub fn payload(&mut self, amount: usize) -> Result<(), Bail> {
        if amount > self.payload {
            return Err(Bail::TooBig);
        }
        self.payload -= amount;
        Ok(())
    }
}
