//! How a nominal type's instances compare in one universe level.
//!
//! Representation, not analysis, for the reason `polarity` gives: the vector this describes is stored on `InductDecl`/`StructDecl` and rides the prelude archive, so the type belongs with the registry entries that carry it, while what *computes* it stays in `curios-analysis`'s `variance`.

/// Whether two instances of a family may differ in one of its universe levels and still be one type.
///
/// Two points and no third. A covariant level — `Value.{u} ≤ Value.{v}`, a level typing a field — would need subsumption to reach nominal types, and no program asks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Variance {
    /// Nothing an instance holds mentions the level, so two instances apart in it alone have the same inhabitants: compared at nothing.
    Irrelevant,
    /// Compared for equality.
    Invariant,
}
