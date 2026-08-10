//! What earlier units established, as the thing a later one is compiled against.

use {crate::Unit, curios_core::Module, curios_elab::ErasedUnit, curios_text::PreparedPrelude};

/// The units already compiled, in dependency order.
///
/// Borrowed rather than merged, and re-derived per fold step rather than held across one: a slice borrow is free to rebuild, and the driver needs its vector back to push the unit it just produced.
#[derive(Clone, Copy)]
pub struct Scope<'a> {
    units: &'a [Unit],
}

impl<'a> Scope<'a> {
    /// Nothing in scope: this unit is the first, and defines every name it mentions.
    pub fn nothing() -> Self {
        Self { units: &[] }
    }

    /// Everything `units` established, in dependency order.
    pub fn over(units: &'a [Unit]) -> Self {
        Self { units }
    }

    /// Whether nothing is in scope.
    pub fn is_empty(&self) -> bool {
        self.units.is_empty()
    }

    /// The resolution state of each unit in scope, for `curios-text`.
    ///
    /// A slice of that crate's own opaque type, not of anything unpacked here — which is what lets its tables stay private while still being layered rather than copied.
    pub fn text(&self) -> Vec<&'a PreparedPrelude> {
        self.units.iter().map(Unit::text).collect()
    }

    /// The elaborated module of each unit in scope, for `curios-elab`'s `Established` and for the kernel's environment.
    pub fn cores(&self) -> Vec<&'a Module> {
        self.units.iter().map(Unit::core).collect()
    }

    /// The arena every erasure so far has accumulated — **one** value, not one per unit, because each unit's erasure resumes over what the previous one produced. An empty scope yields the empty arena, which `ErsdBuilder::resume` reduces to a fresh builder.
    pub fn arena(&self) -> ErasedUnit {
        self.units.last().map(Unit::ersd).unwrap_or_default()
    }
}

impl Default for Scope<'_> {
    fn default() -> Self {
        Self::nothing()
    }
}
