//! Non-refundable expansion fuel, charged the weight of the complete copied extent and its scaffolding.

use std::cmp::Ordering;

/// A module-wide allowance of copied weight ([`CopyPlan::weight`](crate::CopyPlan::weight)). Pruning never refunds work already performed.
#[derive(Debug, PartialEq, Eq)]
pub struct CopyBudget(usize);

impl CopyBudget {
    pub fn new(weight: usize) -> Self {
        Self(weight)
    }

    pub fn admits(&self, weight: usize) -> bool {
        weight > 0 && weight <= self.0
    }

    /// Spend an admitted copy's actual weight, after its rewrite succeeds.
    pub fn charge(&mut self, weight: usize) {
        self.0 = self.0.checked_sub(weight).expect("copy was admitted");
    }
}

/// More sites per copied weight wins; equal ratios retain the first site's identity order.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CopyPriority {
    pub sites: usize,
    pub weight: usize,
    pub first: usize,
}

impl Ord for CopyPriority {
    fn cmp(&self, other: &Self) -> Ordering {
        ((other.sites as u128) * (self.weight as u128))
            .cmp(&((self.sites as u128) * (other.weight as u128)))
            .then_with(|| self.first.cmp(&other.first))
            .then_with(|| self.weight.cmp(&other.weight))
            .then_with(|| self.sites.cmp(&other.sites))
    }
}

impl PartialOrd for CopyPriority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
