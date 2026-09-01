//! The typed, tombstoning arena shared by the identity-addressed IRs.
//!
//! One slot vector per entity kind, addressed by a `u32`-backed [`ArenaId`]: identities are minted monotonically and never reused, removal writes `None` (a tombstone) instead of moving slots, and iteration order is identity order — construction is deterministic, so identities are too. This is the representation contract `curios-ersd` and `curios-cont` share, written once here; `README.md` states why every identity space has one source.
//!
//! The arena deliberately supports the *reserve* pattern: a slot minted empty (`reserve`) so recursive bodies can reference their own or a sibling's identity before the definition exists (`define` fills it exactly once). Before a module is finished, an empty slot is a pending reservation; after, it is an ordinary tombstone — the arena does not distinguish them, the owning builder's finish check does.

#[cfg(test)]
mod tests;

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
    marker::PhantomData,
};

/// A `u32`-backed arena identity, implemented by every [`id!`](crate::id) newtype. `from_index` is the one narrowing intrinsic — loud on exhaustion, never wrapping — and both ways of minting an identity go through it: an arena's [`mint`](Arena::mint)/[`reserve`](Arena::reserve), and the `Entropy` gensym the `id!(Foo, "f", mint)` form implements. Nothing else calls it.
///
/// The discipline that buys is one *source* per identity space rather than one caller: an arena-backed identity is minted only by its arena, a gensym identity has no arena at all, and no identity type is both. Two sources over one space would hand out one index twice, which is the failure this rules out.
pub trait ArenaId: Copy + Display {
    #[doc(hidden)]
    fn from_index(index: usize) -> Self;

    /// The identity's raw arena index.
    fn index(self) -> usize;
}

/// A tombstoning slot arena over one entity kind. See the module documentation for the identity contract.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct Arena<I, T> {
    slots: Vec<Option<T>>,
    id: PhantomData<I>,
}

impl<I, T> Default for Arena<I, T> {
    fn default() -> Self {
        Self {
            slots: Vec::new(),
            id: PhantomData,
        }
    }
}

impl<I: ArenaId, T> Arena<I, T> {
    pub fn new() -> Self {
        Self::default()
    }

    /// Mint a fresh identity holding `value`.
    pub fn mint(&mut self, value: T) -> I {
        let id = I::from_index(self.slots.len());
        self.slots.push(Some(value));
        id
    }

    /// Mint a fresh identity whose definition follows later ([`Arena::define`]), so recursive structures can reference an identity before its value exists.
    pub fn reserve(&mut self) -> I {
        let id = I::from_index(self.slots.len());
        self.slots.push(None);
        id
    }

    /// Fill a reserved slot — exactly once, and only for an identity this arena minted.
    pub fn define(&mut self, id: I, value: T) {
        let slot = self
            .slots
            .get_mut(id.index())
            .unwrap_or_else(|| panic!("unknown identity {id}"));
        assert!(slot.is_none(), "{id} is already defined");
        *slot = Some(value);
    }

    /// Replace a *live* slot's value in place — the install intrinsic for rewrites; a tombstoned or reserved slot is a caller bug.
    pub fn set(&mut self, id: I, value: T) {
        let slot = self
            .slots
            .get_mut(id.index())
            .unwrap_or_else(|| panic!("unknown identity {id}"));
        assert!(slot.is_some(), "{id} is not live");
        *slot = Some(value);
    }

    pub fn get(&self, id: I) -> Option<&T> {
        self.slots.get(id.index()).and_then(Option::as_ref)
    }

    pub fn get_mut(&mut self, id: I) -> Option<&mut T> {
        self.slots.get_mut(id.index()).and_then(Option::as_mut)
    }

    /// Tombstone one slot, returning what it held. The identity is never reused.
    pub fn remove(&mut self, id: I) -> Option<T> {
        self.slots.get_mut(id.index())?.take()
    }

    /// Tombstone every live slot outside `keep`.
    pub fn retain(&mut self, keep: &BTreeSet<I>)
    where
        I: Ord,
    {
        for (index, slot) in self.slots.iter_mut().enumerate() {
            if slot.is_some() && !keep.contains(&I::from_index(index)) {
                *slot = None;
            }
        }
    }

    /// The live entries, in identity order.
    pub fn iter_live(&self) -> impl Iterator<Item = (I, &T)> {
        self.slots
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| slot.as_ref().map(|value| (I::from_index(index), value)))
    }

    /// The live entries with mutable access, in identity order.
    pub fn iter_live_mut(&mut self) -> impl Iterator<Item = (I, &mut T)> {
        self.slots
            .iter_mut()
            .enumerate()
            .filter_map(|(index, slot)| slot.as_mut().map(|value| (I::from_index(index), value)))
    }

    /// The live identities, in identity order — the external handle surface for consumers that hold a module but cannot mint.
    pub fn live_ids(&self) -> impl Iterator<Item = I> + '_ {
        self.iter_live().map(|(id, _)| id)
    }

    /// The raw slot vector, tombstones included — the zero-churn surface for existing slice-shaped accessors and slot-indexed walks.
    pub fn slots(&self) -> &[Option<T>] {
        &self.slots
    }

    /// Total slots ever minted, live or not — the exclusive upper bound of the identity space handed out so far.
    pub fn len(&self) -> usize {
        self.slots.len()
    }

    pub fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }

    /// Drop every tombstone, packing the live entries into a dense prefix, and report where each identity moved.
    ///
    /// **Slots move.** Every other operation here promises they never do, and the identity discipline rests on that — so this is the one pass that invalidates outstanding identities. A consumer must rewrite every identity it stores through the returned map, including any held outside this arena, and the map is *total* over live identities: a lookup that misses is a gap in the caller's walk, not an identity from elsewhere. That distinction matters because the failure is silent — a stale index still addresses a live slot, just the wrong one.
    ///
    /// Only valid on a *finished* arena. A reserved-but-undefined slot is indistinguishable from a tombstone here (see the module documentation), so compacting mid-construction drops pending reservations without saying so.
    pub fn compact(&mut self) -> BTreeMap<I, I>
    where
        I: Ord,
    {
        let mut moved = BTreeMap::new();
        let mut packed = Vec::with_capacity(self.live_count());

        for (index, slot) in std::mem::take(&mut self.slots).into_iter().enumerate() {
            let Some(value) = slot else { continue };
            moved.insert(I::from_index(index), I::from_index(packed.len()));
            packed.push(Some(value));
        }

        self.slots = packed;
        moved
    }

    pub fn live_count(&self) -> usize {
        self.slots.iter().filter(|slot| slot.is_some()).count()
    }

    pub fn tombstone_count(&self) -> usize {
        self.slots.iter().filter(|slot| slot.is_none()).count()
    }

    /// Assert that no tombstone remains, naming the arena in the failure.
    ///
    /// A postcondition, not a safety net, and worth stating because it is easy to read as more than it is. [`Arena::compact`] packs by construction and [`Arena::set`] refuses a dead slot, so on the code as written this cannot fail; what it guards is the next edit — a compaction that grows a case it does not pack, or a consumer that compacts some of its arenas and forgets one.
    ///
    /// It does *not* catch the failure compaction actually risks. A missed identity leaves the arena exactly this dense and still addresses a live slot, just the wrong one; the structural walk that owns that question is the caller's own verifier.
    pub fn assert_packed(&self, what: &str) {
        assert_eq!(
            self.tombstone_count(),
            0,
            "the {what} arena carries tombstones across its {} slots, where it must be packed",
            self.slots.len(),
        );
    }
}
