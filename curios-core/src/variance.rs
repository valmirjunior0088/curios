//! How two instances of one name compare in one universe level: a nominal type's, and a definition's.
//!
//! Representation, not analysis, for the reason `polarity` gives: the vector this describes is stored on `InductDecl`/`StructDecl` and on `Definition` and rides the prelude archive, so the type belongs with the entries that carry it, while what *computes* it stays in `curios-analysis`'s `variance`.

use super::{Global, InductDecl, StructDecl};

/// What a walk aligning two terms' levels is told of the nominal families and of the definitions, by a caller that compares instances by variance. `()` tells it nothing, and every level is then aligned.
///
/// The declarations are read where they live: a registry hands them over, and the walk reads a vector and an arity off each, with no second statement of either.
pub trait Families {
    fn induct(&self, name: &Global) -> Option<&InductDecl>;

    fn struct_(&self, name: &Global) -> Option<&StructDecl>;

    /// The vector of a definition that is no family's former, with how many arguments one spine applies it in full: `None` where this registry holds none for `name`, which is how a caller that reads no definition answers.
    fn definition(&self, _: &Global) -> Option<(&[Variance], usize)> {
        None
    }

    /// `name`'s variance in its `index`th universe parameter: invariant for a name this registry does not hold.
    fn variance(&self, name: &Global, index: usize) -> Variance {
        self.induct(name)
            .map(|declaration| declaration.variance(index))
            .or_else(|| {
                self.struct_(name)
                    .map(|declaration| declaration.variance(index))
            })
            .or_else(|| {
                self.definition(name)
                    .map(|(vector, _)| vector.get(index).copied().unwrap_or(Variance::Invariant))
            })
            .unwrap_or(Variance::Invariant)
    }

    /// How many parameters and how many indices a full application of `name` supplies.
    fn shape(&self, name: &Global) -> Option<(usize, usize)> {
        self.induct(name)
            .map(|declaration| (declaration.param_count(), declaration.index_count()))
            .or_else(|| {
                self.struct_(name)
                    .map(|declaration| (declaration.param_count(), 0))
            })
    }

    /// Whether the walk reads an `induct`'s former applied in full by its group's projection as the node it builds. A reading that does not keeps the two groups aligned level by level, which is what a caller that must refuse on any level asks for.
    fn reads_formers(&self) -> bool {
        true
    }

    /// Whether the walk sets apart, as curried, a former or a definition short of its full application whose two instances are apart in a level its vector calls irrelevant. Two such are one term where they meet at their function type, eta applying both to the same binders, and are compared level by level where they meet at any other; so a reading that sets them apart leaves the pair to a caller that goes on to the type it stands at, and one that does not aligns every level of them.
    fn sets_curried_apart(&self) -> bool {
        true
    }
}

impl Families for () {
    fn induct(&self, _: &Global) -> Option<&InductDecl> {
        None
    }

    fn struct_(&self, _: &Global) -> Option<&StructDecl> {
        None
    }

    fn reads_formers(&self) -> bool {
        false
    }

    fn sets_curried_apart(&self) -> bool {
        false
    }
}

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
