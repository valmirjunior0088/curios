//! What earlier units established, as the thing a later one is compiled against.

use {
    crate::Unit, curios_abi::ForeignStore, curios_core::Module, curios_elab::ErasedArena,
    curios_text::PreparedText,
};

/// The units already compiled, in dependency order.
///
/// Borrowed rather than merged, and re-derived per fold step rather than held across one: a slice borrow is free to rebuild, and the driver needs its vector back to push the unit it just produced.
///
/// Named for what they are to the unit being compiled rather than for name resolution, though resolving against them is most of what they are for: `curios-core`'s [`Scope`](curios_core::Scope) is a *binder* scope, and two public types spelled alike one crate apart is a collision a reader has to resolve every time. Predecessors are what these are — the units before this one, in the order they were folded — and it is the word the workspace's prose already uses for them.
#[derive(Clone, Copy)]
pub struct Predecessors<'a> {
    units: &'a [&'a Unit],
}

impl<'a> Predecessors<'a> {
    /// Everything `units` established, in dependency order. With no predecessors nothing is in scope, so the unit compiled against them defines every name it mentions.
    pub fn over(units: &'a [&'a Unit]) -> Self {
        Self { units }
    }

    /// The units themselves, in dependency order.
    ///
    /// For a consumer that needs more than one projection at a time — the kernel's environment is built from each unit's module *and* its certification together, and this crate cannot build it itself without depending on the kernel it is defined to stay below.
    pub fn units(&self) -> &'a [&'a Unit] {
        self.units
    }

    /// The resolution state of each predecessor, for `curios-text`.
    ///
    /// A slice of that crate's own opaque type, not of anything unpacked here — which is what lets its tables stay private while still being layered rather than copied.
    pub fn text(&self) -> Vec<&'a PreparedText> {
        self.units.iter().map(|unit| unit.text()).collect()
    }

    /// The elaborated module of each predecessor, for `curios-elab`'s `Established` and for the kernel's environment.
    pub fn cores(&self) -> Vec<&'a Module> {
        self.units.iter().map(|unit| unit.core()).collect()
    }

    /// Every `foreign` row the predecessors declare, in dependency order — the union an embedder binds against.
    pub fn foreigns(&self) -> ForeignStore {
        let mut foreigns = ForeignStore::new();
        for unit in self.units {
            foreigns.absorb(unit.foreigns());
        }

        foreigns
    }

    /// The arena every erasure so far has accumulated — **one** value, not one per unit, because each unit's erasure resumes over what the previous one produced. No predecessors yield the empty arena, which `ErsdBuilder::resume` reduces to a fresh builder.
    pub fn arena(&self) -> ErasedArena {
        self.units
            .last()
            .map(|unit| unit.arena())
            .unwrap_or_default()
    }
}
