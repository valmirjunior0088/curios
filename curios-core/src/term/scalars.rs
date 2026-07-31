//! The packed home of a node's eager scalar derivations.
//!
//! [`Scalars`] is the value — one node's span-independent facts, as named fields. [`ScalarCache`] is where a node memoizes them: two words, filled at most once, and always all together, which is the invariant the single `fill`-taking-the-whole-value entry point makes structural. The bit layout is this module's private business; nothing outside it sees a shift or a mask.

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

/// One node's span-independent derivations: the structural hash, the loose-index `reach`, and the four containment flags. Computed from the node's subterm with every child's own cache already filled, so [`Scalars::of`] costs O(children).
#[derive(Clone, Copy)]
pub(crate) struct Scalars {
    pub(crate) reach: usize,
    pub(crate) has_local_free: bool,
    pub(crate) has_metavar: bool,
    pub(crate) has_universe_meta: bool,
    pub(crate) has_universe_data: bool,
    pub(crate) hash: u64,
}

impl Scalars {
    /// Derive one node's scalars from its subterm, reading each child's already-filled cache in O(1).
    pub(crate) fn of(subterm: &Subterm) -> Self {
        let mut hasher = DefaultHasher::new();
        subterm.hash(&mut hasher);

        Self {
            reach: subterm.reach(),
            has_local_free: subterm.has_local_free(),
            has_metavar: subterm.has_metavar(),
            has_universe_meta: subterm.has_universe_meta(),
            has_universe_data: subterm.has_universe_data(),
            hash: hasher.finish(),
        }
    }
}

const FILLED: u64 = 1 << 63;
const HAS_LOCAL_FREE: u64 = 1 << 0;
const HAS_METAVAR: u64 = 1 << 1;
const HAS_UNIVERSE_META: u64 = 1 << 2;
const HAS_UNIVERSE_DATA: u64 = 1 << 3;
const REACH_SHIFT: u32 = 4;
const REACH_BITS: u32 = 59;

/// A node's memoized [`Scalars`], packed into two `Cell<u64>` words: the flags, `reach`, and a filled bit in `packed`, and the full hash in `hash`, whose validity the shared filled bit governs — a hash of any value (zero included) is legitimate once filled. `Default` is the unfilled state, which is also what an archived node restores to (the field is `rkyv`-skipped).
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
            reach: (packed >> REACH_SHIFT & (1 << REACH_BITS) - 1) as usize,
            has_local_free: packed & HAS_LOCAL_FREE != 0,
            has_metavar: packed & HAS_METAVAR != 0,
            has_universe_meta: packed & HAS_UNIVERSE_META != 0,
            has_universe_data: packed & HAS_UNIVERSE_DATA != 0,
            hash: self.hash.get(),
        })
    }

    /// Memoize the node's scalars — the whole value, once. `reach` must fit its 59 bits, which any representable term satisfies: it is bounded by the term's binder depth, and understating it would let a pruning traversal skip a subtree it still has to rewrite.
    pub(crate) fn fill(&self, scalars: Scalars) {
        debug_assert!(!self.is_filled(), "a scalar cache fills at most once");
        debug_assert!(
            (scalars.reach as u64) < 1 << REACH_BITS,
            "reach exceeds its packed width"
        );

        let mut packed = FILLED | (scalars.reach as u64) << REACH_SHIFT;
        if scalars.has_local_free {
            packed |= HAS_LOCAL_FREE;
        }
        if scalars.has_metavar {
            packed |= HAS_METAVAR;
        }
        if scalars.has_universe_meta {
            packed |= HAS_UNIVERSE_META;
        }
        if scalars.has_universe_data {
            packed |= HAS_UNIVERSE_DATA;
        }

        self.hash.set(scalars.hash);
        self.packed.set(packed);
    }
}
