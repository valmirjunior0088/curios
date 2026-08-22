//! The compilation-scoped allowance for optional cache and memo storage.
//!
//! The work budget is per declaration and decides *acceptance*. This is neither: it is one allowance for the whole compilation, and exhausting it refuses an **insertion** rather than a program. A cache that stops accepting entries leaves evaluation correct and cold; a budget that stops accepting work refuses the program. Keeping those two on separate counters is what lets the second stay a fact about the declaration under judgment.
//!
//! # Why it exists at all
//!
//! The work budget is restored at every declaration boundary, and the elaborator's reduction cache deliberately is not — it survives item boundaries so that closed reducts stay warm across the definitions reduction and erasure mint. Those two lifetimes compose into a bound of *declarations times budget*, which is no bound at all for a module of many heavy ones. Per-declaration charging bounds one declaration; this bounds the module.
//!
//! # What it costs to exhaust
//!
//! Nothing semantic, and one thing that is worth stating rather than claiming away. The reduction loop probes its cache *before* it charges, so a hit already costs nothing and a cold cache already costs re-derivation against the work budget. A declaration that would have hit a warm cache can therefore exhaust its own budget once retention has stopped. That is the elaborator's existing warmth-dependence rather than a new one, and the default is measured with enough headroom that ordinary compilation never reaches it.
//!
//! # Cumulative, never refunded
//!
//! An entry that is invalidated, replaced, or cleared does not give its allowance back. That keeps the bound deterministic and independent of destruction order, and it avoids needing exact shared-ownership accounting for values several entries may point at. Double-counting shared payload across entries is permitted for the same reason: this quota controls an optimization, and a conservative bound is safer than an exemption nobody can prove.

use super::Cost;

/// How much optional storage one compilation may retain, in the same logical units work is charged in.
///
/// **Measured for headroom rather than for tightness**, because crossing it costs a later declaration its own budget in re-derivation — see the module documentation. Taken 2026-08-15 by `curios-prelude-archive`'s `stored_prelude_measurements`, which reports both sides over the whole standard library:
///
/// | What | Retained |
/// | --- | --- |
/// | certifying one whole unit — the kernel's three memo tables | 108 530 138 units |
/// | re-erasing one whole unit — the elaborator's reduction cache | 3 524 199 units |
///
/// This is nine times the larger of them. That is a real ceiling rather than an unreachable one — at the eight logical bytes a unit names, it bounds retained storage at about eight gigabytes — while leaving the fixed prelude, which is the heaviest thing the compiler ever holds, using eleven percent of it.
///
/// **The kernel's figure was 24 444 443 until its memo was made to reach every reduction level rather than only the two `Reducer` entry points.** Four and a half times the entries for a 5.3× drop in what a `Str` literal's check charges, and no measurable change in process memory — the counter double-counts payload shared between entries, deliberately, which is why it moved so much further than the machine did.
///
/// **The accumulator-chain shape this documentation used to record is gone with the closed machine, on the kernel entirely and on the elaborator in part.** Arguments were substituted unreduced, so a tail recursion threading an accumulator built a term one link deeper per element, and every memo entry keyed on a term containing the chain had a footprint of the walk so far — a `Str` literal of 4 000 characters consumed 774 million units of this allowance by itself. The machine substitutes values, so the chain is never built: the kernel's retention over the same literal ladder is now flat at under a quarter-million units at every size, which `str_literal_cost_measurements` records beside the old ladder.
///
/// **The elaborator's residue had a different source, and it is closed.** Its retention grew quadratically in a literal's length — about three-quarters of its pre-machine figure, saturating this quota near five thousand characters — because its conversion unfolded the validity scan one step per round rather than forcing it, storing each round's folded spelling with the scan's state unreduced inside it. It forces now, as the kernel always did, and the same ladder retains about four units a character; `str_literal_cost_measurements` carries both tables.
pub const DEFAULT_RETENTION_QUOTA: u64 = 1_000_000_000;

/// What a compilation has left to retain.
#[derive(Debug)]
pub struct Retention {
    quota: u64,
    remaining: u64,
}

impl Retention {
    /// An allowance of `quota` units, for one compilation.
    pub fn new(quota: u64) -> Self {
        Self {
            quota,
            remaining: quota,
        }
    }

    /// Whether `cost` may be retained — spending it when it may, and leaving the allowance untouched when it may not.
    ///
    /// The verdict and the spend are one operation because they cannot be allowed to disagree: a caller that asked and then inserted anyway would be retaining storage this never counted, and a caller that spent and then declined would have paid for an entry nobody holds. A saturated cost is refused like any other charge that cannot be afforded.
    pub fn admits(&mut self, cost: Cost) -> bool {
        if cost.is_refused() {
            return false;
        }

        match self.remaining.checked_sub(cost.get()) {
            Some(remaining) => {
                self.remaining = remaining;
                true
            }
            None => false,
        }
    }

    /// How much of the allowance this compilation has consumed — the figure a measurement probe reports and a default is set against.
    pub fn spent(&self) -> u64 {
        self.quota - self.remaining
    }
}

#[cfg(test)]
mod tests;
