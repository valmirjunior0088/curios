//! What a judgment actually spent, for a measurement to read.
//!
//! [`Cost`]'s `Category` names what a *refused* charge was for, and deliberately promises nothing about dominance — "this is the category of the refused charge, not of whatever consumed the most budget over the declaration". This is the other question, answered for the one row where it can be answered exactly and for free.
//!
//! # Why depth alone, and why that is enough
//!
//! [`Cost::FRAME`] is charged once per *new peak* of guarded reduction depth, so a judgment's whole depth bill is its peak times the frame price — an identity, not an estimate, and readable off a counter both checkers already keep. Every other row would need a cumulative accumulator on the hottest path in the trusted base, which `FRAME_UNITS`' own documentation gives as the reason it is a recipe rather than a probe. So this splits a judgment two ways, exactly: what depth cost, and what everything else did.
//!
//! That split is the one that was needed. Depth is the only row whose size is set by the reduction *strategy* rather than by the term, so it is the only one where the two checkers can disagree about the same program — and they did, by a factor of twenty, which is what this type was built to see.
//!
//! # It is an observation, never a control
//!
//! Nothing in either checker reads a [`Consumption`]. It is threaded out of a walk the way `Kernel::retained` is, so a default can be set against a figure rather than a guess, and so a probe can state what a program costs without bisecting a budget from outside the compiler.

use super::Cost;

/// What one declaration's judgment consumed: the units it spent, and the deepest guarded reduction level it reached.
///
/// A whole-module walk reports its *heaviest* declaration rather than a sum, because the budget is per declaration and that is the figure a default has to clear. [`Consumption::heavier_of`] is the fold.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Consumption {
    units: u64,
    peak_depth: usize,
}

impl Consumption {
    /// A judgment that spent `units` and reached `peak_depth` levels.
    pub fn new(units: u64, peak_depth: usize) -> Self {
        Self { units, peak_depth }
    }

    /// Everything this judgment spent, in the units a budget is denominated in.
    pub fn units(&self) -> u64 {
        self.units
    }

    /// The deepest guarded reduction level it reached.
    pub fn peak_depth(&self) -> usize {
        self.peak_depth
    }

    /// What that depth cost it — the peak times [`Cost::FRAME`], which is exactly what the frame row charged because it charges once per new peak.
    pub fn frame_units(&self) -> u64 {
        (self.peak_depth as u64).saturating_mul(Cost::FRAME.get())
    }

    /// What it spent on everything other than depth.
    ///
    /// Saturating at zero rather than asserting: the identity above holds for a judgment that ran to completion, and a judgment that was *refused* stopped partway through a charge, so the two counters can disagree by less than one frame at exactly one point.
    pub fn other_units(&self) -> u64 {
        self.units.saturating_sub(self.frame_units())
    }

    /// The heavier of two judgments, by units spent — each carrying its own depth, so the reported peak is the peak *of the declaration being reported*, not the deepest anything reached.
    pub fn heavier_of(self, other: Self) -> Self {
        match other.units > self.units {
            true => other,
            false => self,
        }
    }
}

#[cfg(test)]
mod tests;
