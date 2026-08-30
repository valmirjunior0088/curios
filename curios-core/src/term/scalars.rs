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

/// One node's span-independent derivations: the structural hash, the loose-index `reach`, the five containment flags, and the logical footprint of everything the node reaches. Computed from the node's subterm with every child's own cache already filled, so [`Scalars::of`] costs O(children).
#[derive(Clone, Copy)]
pub(crate) struct Scalars {
    pub(crate) reach: usize,
    pub(crate) has_local_free: bool,
    pub(crate) has_metavar: bool,
    pub(crate) has_transient: bool,
    pub(crate) has_universe_meta: bool,
    pub(crate) has_universe_data: bool,
    pub(crate) footprint: u64,
    pub(crate) hash: u64,
}

/// What one term node costs before its payload and its children — the same fixed charge [`Cost::term`](crate::Cost::term) states, spelled here because this module is below it in the crate's own dependency order and a second constant would be a second thing to drift.
const NODE_UNITS: u64 = 8;

impl Scalars {
    /// Derive one node's scalars from its subterm, reading each child's already-filled cache in O(1).
    pub(crate) fn of(subterm: &Subterm) -> Self {
        let mut hasher = DefaultHasher::new();
        subterm.hash(&mut hasher);

        Self {
            reach: subterm.reach(),
            has_local_free: subterm.has_local_free(),
            has_metavar: subterm.has_metavar(),
            has_transient: subterm.has_transient(),
            has_universe_meta: subterm.has_universe_meta(),
            has_universe_data: subterm.has_universe_data(),
            footprint: footprint_of(subterm),
            hash: hasher.finish(),
        }
    }
}

/// The logical units a node and everything under it occupy: the node itself, its own payload, and every child's already-computed footprint.
///
/// **O(children), because each child's is already cached** — the same property that makes `reach` affordable here, and the reason this can be asked in O(1) at a cache-insertion site rather than walked. That is what the specification means by a cached saturating logical-footprint summary on immutable values.
///
/// **Shared children are counted once per parent, not once per node.** A DAG's footprint therefore reads as the tree it unfolds to, which overcounts. That is the safe direction for the retention quota this feeds: the quota bounds an optional optimization, and the specification permits double-counting shared payload explicitly rather than resting a bound on an unprovable exemption.
///
/// Saturating, so a term too large to measure reports the largest measurable footprint rather than wrapping to a small one. Nothing that could reach saturation can be built under a budget that charges construction.
fn footprint_of(subterm: &Subterm) -> u64 {
    let mut total = NODE_UNITS.saturating_add(match subterm {
        Subterm::Intrinsic(intrinsic) => intrinsic.payload_units(),
        _ => 0,
    });

    subterm.any_child_term(&mut |child| {
        total = total.saturating_add(child.footprint());
        false
    });

    total
}

const FILLED: u64 = 1 << 63;
const HAS_LOCAL_FREE: u64 = 1 << 0;
const HAS_METAVAR: u64 = 1 << 1;
const HAS_UNIVERSE_META: u64 = 1 << 2;
const HAS_UNIVERSE_DATA: u64 = 1 << 3;
const HAS_TRANSIENT: u64 = 1 << 4;
const REACH_SHIFT: u32 = 5;
const REACH_BITS: u32 = 28;
const FOOTPRINT_SHIFT: u32 = REACH_SHIFT + REACH_BITS;
const FOOTPRINT_BITS: u32 = 30;
const FOOTPRINT_MAX: u64 = (1 << FOOTPRINT_BITS) - 1;

/// A node's memoized [`Scalars`], packed into two `Cell<u64>` words: the flags, `reach`, the footprint, and a filled bit in `packed`, and the full hash in `hash`, whose validity the shared filled bit governs — a hash of any value (zero included) is legitimate once filled. `Default` is the unfilled state, which is also what an archived node restores to (the field is `rkyv`-skipped).
///
/// **Two words rather than three, and the footprint is what made that a question.** A third `Cell<u64>` would cost eight bytes on every node of every term the compiler holds, which the fixed prelude alone counts in millions. The two figures share one word instead: `reach` is bounded by binder depth and keeps 28 bits, which is a quarter billion nested binders and unreachable on any stack; the footprint takes 30 and *saturates* rather than wrapping, which the specification names as an acceptable strategy and which is the safe direction for a quota that bounds an optimization.
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
            has_local_free: packed & HAS_LOCAL_FREE != 0,
            has_metavar: packed & HAS_METAVAR != 0,
            has_transient: packed & HAS_TRANSIENT != 0,
            has_universe_meta: packed & HAS_UNIVERSE_META != 0,
            has_universe_data: packed & HAS_UNIVERSE_DATA != 0,
            footprint: (packed >> FOOTPRINT_SHIFT) & FOOTPRINT_MAX,
            hash: self.hash.get(),
        })
    }

    /// Memoize the node's scalars — the whole value, once. `reach` must fit its 29 bits, which any representable term satisfies: it is bounded by the term's binder depth, and understating it would let a pruning traversal skip a subtree it still has to rewrite.
    ///
    /// The footprint is *clamped* rather than asserted, because it is the one figure here that legitimately can exceed its field. Clamping reports the largest measurable footprint, which reads to the retention quota as "at least this much" — the direction that stops an insertion rather than admitting one.
    pub(crate) fn fill(&self, scalars: Scalars) {
        debug_assert!(!self.is_filled(), "a scalar cache fills at most once");
        debug_assert!(
            (scalars.reach as u64) < 1 << REACH_BITS,
            "reach exceeds its packed width"
        );

        let mut packed = FILLED
            | (scalars.reach as u64) << REACH_SHIFT
            | scalars.footprint.min(FOOTPRINT_MAX) << FOOTPRINT_SHIFT;
        if scalars.has_local_free {
            packed |= HAS_LOCAL_FREE;
        }
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

        self.hash.set(scalars.hash);
        self.packed.set(packed);
    }
}
