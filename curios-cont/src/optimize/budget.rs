//! Non-refundable expansion fuel, charged for the complete copied extent and its scaffolding.

use std::cmp::Ordering;

/// A module-wide allowance of newly copied nodes. Pruning never refunds work already performed.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct CopyBudget(usize);

impl CopyBudget {
    pub(crate) fn new(nodes: usize) -> Self {
        Self(nodes)
    }

    pub(crate) fn admits(&self, nodes: usize) -> bool {
        nodes > 0 && nodes <= self.0
    }

    /// Spend an admitted copy's actual node count, after its rewrite succeeds.
    pub(crate) fn charge(&mut self, nodes: usize) {
        self.0 = self.0.checked_sub(nodes).expect("copy was admitted");
    }
}

/// Higher sharing per copied node wins; equal ratios retain the first site's identity order.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct CopyPriority {
    pub(crate) sites: usize,
    pub(crate) nodes: usize,
    pub(crate) first: usize,
}

impl Ord for CopyPriority {
    fn cmp(&self, other: &Self) -> Ordering {
        ((other.sites as u128) * (self.nodes as u128))
            .cmp(&((self.sites as u128) * (other.nodes as u128)))
            .then_with(|| self.first.cmp(&other.first))
            .then_with(|| self.nodes.cmp(&other.nodes))
            .then_with(|| self.sites.cmp(&other.sites))
    }
}

impl PartialOrd for CopyPriority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
