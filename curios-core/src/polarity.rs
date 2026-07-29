//! The polarity lattice: how a type former uses one parameter.
//!
//! Representation, not analysis. The vector this describes is stored on
//! `InductDecl`/`StructDecl` and rides the prelude archive, so the type and
//! its lattice operations belong with the registry entries that carry them,
//! while the occurrence relation that *computes* them stays in `positivity`.

use std::fmt;

/// How a type former uses one parameter, or at what polarity one declaration
/// occurs inside another.
///
/// Five points, ordered `Unused ⊑ Strict ⊑ Pos ⊑ Mixed` and
/// `Unused ⊑ Neg ⊑ Mixed`, with `Strict` and `Neg` incomparable. Only `Strict`
/// and `Unused` are accepting; the middle grades exist because composition has
/// to distinguish them (`Neg ∘ Neg = Pos`) and because "occurs negatively" and
/// "occurs positively but not strictly" are different user errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Polarity {
    /// The parameter does not occur.
    Unused,
    /// Occurs, never under a function arrow.
    Strict,
    /// Occurs under an even number of arrows.
    Pos,
    /// Occurs under an odd number of arrows.
    Neg,
    /// Occurs both ways, or the analysis cannot tell.
    Mixed,
}

impl Polarity {
    /// Whether an occurrence at this polarity is admissible in a recursive
    /// position — the whole acceptance test, applied to one entry.
    pub fn accepting(self) -> bool {
        matches!(self, Polarity::Unused | Polarity::Strict)
    }

    /// Least upper bound: how several occurrences of the same target combine.
    pub fn join(self, other: Polarity) -> Polarity {
        use Polarity::*;
        match (self, other) {
            (Unused, p) | (p, Unused) => p,
            (Mixed, _) | (_, Mixed) => Mixed,
            (p, q) if p == q => p,
            // `Strict ⊑ Pos`, so a strict and a positive occurrence join to
            // positive; every other distinct pair spans the two incomparable
            // branches and lands at the top.
            (Strict, Pos) | (Pos, Strict) => Pos,
            _ => Mixed,
        }
    }

    /// Sign multiplication: a target occurring at `self` inside an argument
    /// whose former uses that argument at `outer` occurs at `outer ∘ self`.
    ///
    /// `Unused` annihilates (the argument is never looked at, so nothing inside
    /// it occurs), `Strict` is the identity, and `Mixed` absorbs every
    /// non-`Unused` argument.
    pub fn compose(self, inner: Polarity) -> Polarity {
        use Polarity::*;
        match (self, inner) {
            (Unused, _) | (_, Unused) => Unused,
            (Strict, p) | (p, Strict) => p,
            (Mixed, _) | (_, Mixed) => Mixed,
            (Pos, Pos) | (Neg, Neg) => Pos,
            (Pos, Neg) | (Neg, Pos) => Neg,
        }
    }

    /// Descend into a function type's domain: what was positive is now
    /// negative and vice versa. `Strict` becomes `Neg` — a strict occurrence
    /// is a positive one that additionally never crossed an arrow, and it has
    /// now crossed one.
    pub fn flip(self) -> Polarity {
        use Polarity::*;
        match self {
            Unused => Unused,
            Strict | Pos => Neg,
            Neg => Pos,
            Mixed => Mixed,
        }
    }
}

impl fmt::Display for Polarity {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let described = match self {
            Polarity::Unused => "not at all",
            Polarity::Strict => "strictly positively",
            Polarity::Pos => "positively, but not strictly",
            Polarity::Neg => "negatively",
            Polarity::Mixed => "in a position the checker cannot see through",
        };
        formatter.write_str(described)
    }
}
