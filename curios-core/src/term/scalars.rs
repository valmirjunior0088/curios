//! The packed home of a node's eager scalar derivations.
//!
//! [`Scalars`] is the value — one node's position-independent facts, as named fields. [`ScalarCache`] is where a node memoizes them: two words, filled at most once, and always all together, which is the invariant the single `fill`-taking-the-whole-value entry point makes structural. The bit layout is this module's private business; nothing outside it sees a shift or a mask.
//!
//! **What does not fit is read as the most it could be.** A newest local whose index is past its packed width reads back as newer than any binder: a capture trusts the newest local to pass over a subterm, and an overstated one only has it walked.

#[cfg(test)]
mod tests;

use {
    super::{Bound, Subterm},
    std::{
        cell::Cell,
        collections::hash_map::DefaultHasher,
        hash::{Hash, Hasher},
    },
};

/// One node's position-independent derivations: the structural hash, the loose-index `reach`, the newest local, and the five containment flags. Computed from the node's subterm with every child's own cache already filled, so [`Scalars::of`] costs O(children).
#[derive(Clone, Copy)]
pub(crate) struct Scalars {
    pub(crate) reach: usize,
    /// The newest binder occurring free, by its index, or `None` where no local does.
    pub(crate) newest_local: Option<u64>,
    pub(crate) has_metavar: bool,
    pub(crate) has_transient: bool,
    pub(crate) has_universe_meta: bool,
    pub(crate) has_universe_data: bool,
    pub(crate) has_group: bool,
    pub(crate) hash: u64,
}

impl Scalars {
    /// Derive one node's scalars from its subterm, reading each child's already-filled cache in O(1).
    pub(crate) fn of(subterm: &Subterm) -> Self {
        let mut hasher = DefaultHasher::new();
        subterm.hash(&mut hasher);

        Self {
            reach: subterm.reach(),
            newest_local: subterm.newest_local(),
            has_metavar: subterm.has_metavar(),
            has_transient: subterm.has_transient(),
            has_universe_meta: subterm.has_universe_meta(),
            has_universe_data: subterm.has_universe_data(),
            has_group: subterm.has_group(),
            hash: hasher.finish(),
        }
    }
}

const FILLED: u64 = 1 << 63;
const HAS_METAVAR: u64 = 1 << 0;
const HAS_UNIVERSE_META: u64 = 1 << 1;
const HAS_UNIVERSE_DATA: u64 = 1 << 2;
const HAS_TRANSIENT: u64 = 1 << 3;
const HAS_GROUP: u64 = 1 << 4;
const REACH_SHIFT: u32 = 5;
const REACH_BITS: u32 = 28;
const NEWEST_SHIFT: u32 = REACH_SHIFT + REACH_BITS;
const NEWEST_BITS: u32 = 28;
/// The packed newest local of an index too wide for its field, read back as newer than every binder. Below it, zero is no local and `n` is the index `n - 1`.
const NEWEST_PAST: u64 = (1 << NEWEST_BITS) - 1;

/// A node's memoized [`Scalars`], packed into two `Cell<u64>` words: the flags, `reach`, the newest local and a filled bit in `packed`, and the full hash in `hash`, whose validity the shared filled bit governs — a hash of any value (zero included) is legitimate once filled. `Default` is the unfilled state, which is also what an archived node restores to (the field is `rkyv`-skipped).
#[derive(Default)]
pub(crate) struct ScalarCache {
    packed: Cell<u64>,
    hash: Cell<u64>,
}

impl ScalarCache {
    pub(crate) fn is_filled(&self) -> bool {
        self.packed.get() & FILLED != 0
    }

    pub(crate) fn get(&self) -> Option<Scalars> {
        let packed = self.packed.get();
        (packed & FILLED != 0).then(|| Scalars {
            reach: ((packed >> REACH_SHIFT) & ((1 << REACH_BITS) - 1)) as usize,
            newest_local: match (packed >> NEWEST_SHIFT) & NEWEST_PAST {
                0 => None,
                NEWEST_PAST => Some(u64::MAX),
                newest => Some(newest - 1),
            },
            has_metavar: packed & HAS_METAVAR != 0,
            has_transient: packed & HAS_TRANSIENT != 0,
            has_universe_meta: packed & HAS_UNIVERSE_META != 0,
            has_universe_data: packed & HAS_UNIVERSE_DATA != 0,
            has_group: packed & HAS_GROUP != 0,
            hash: self.hash.get(),
        })
    }

    /// Memoize the node's scalars — the whole value, once. `reach` must fit its packed width, which any representable term satisfies: it is bounded by the term's binder depth, and understating it would let a pruning traversal skip a subtree it still has to rewrite.
    pub(crate) fn fill(&self, scalars: Scalars) {
        assert!(!self.is_filled(), "a scalar cache fills at most once");
        assert!(
            (scalars.reach as u64) < 1 << REACH_BITS,
            "reach exceeds its packed width"
        );

        let newest = match scalars.newest_local {
            None => 0,
            Some(index) => index.saturating_add(1).min(NEWEST_PAST),
        };
        let mut packed = FILLED | (scalars.reach as u64) << REACH_SHIFT | newest << NEWEST_SHIFT;
        if scalars.has_metavar {
            packed |= HAS_METAVAR;
        }
        if scalars.has_transient {
            packed |= HAS_TRANSIENT;
        }
        if scalars.has_universe_meta {
            packed |= HAS_UNIVERSE_META;
        }
        if scalars.has_universe_data {
            packed |= HAS_UNIVERSE_DATA;
        }
        if scalars.has_group {
            packed |= HAS_GROUP;
        }

        self.hash.set(scalars.hash);
        self.packed.set(packed);
    }
}
