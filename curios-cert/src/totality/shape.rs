//! A call argument or a parameter read as a constructor tree — the term the size order is a proper-subterm order on.
//!
//! Every leaf is either a binder identity minted by the traversal that built it, or a closed literal shape. That is what makes the comparison trustworthy: two trees compare equal only when they name the same binders, so a same-named constructor of a different type would still have to reach the same binders to be mistaken for one.

use {
    super::Size,
    curios_core::{Atom, Free, Global},
};

/// The constructor a [`Shape`] node stands for.
///
/// Carriers are kept apart so a `Bin` cons can never be mistaken for an `Lst` cons. Well-typed code could not confuse them, but the size order is only as trustworthy as the identities it compares.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Tag {
    /// An inductive constructor, by tag alone.
    ///
    /// The owning type is deliberately not part of the identity, because a freshly minted arm binder has no recorded type to read it from. Dropping it cannot manufacture a false decrease: every leaf of a shape is a binder identity minted by this very traversal, so two trees compare equal only when they name the same binders, and a same-named constructor of a different type would still have to reach the same binders to be mistaken for one.
    Variant(Atom),
    /// A nominal structure literal.
    Struct(Global),
    /// An anonymous tuple.
    Tuple,
    /// One generator of a free-monoid carrier: `Nat`'s successor, a `Bin` byte, an `Lst` element.
    Cons(Carriers),
    /// A free-monoid carrier's identity: zero, `b[]`, `[]`.
    Empty(Carriers),
    /// A boolean literal.
    Bool(bool),
}

/// Which native free monoid a [`Tag::Cons`] or [`Tag::Empty`] belongs to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Carriers {
    Unary,
    Bin,
    Lst,
}

/// A call argument or a parameter, read as a constructor tree over binder atoms — the term on which the size order is a proper-subterm order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Shape {
    /// A binder the analysis tracks but cannot see inside.
    Atom(Free),
    /// A constructor applied to its arguments.
    Node(Tag, Vec<Shape>),
    /// A `Nat` strictly below the binder it names, established arithmetically rather than structurally: `n / k` for a literal `k >= 2`, or `n - k` for a literal `k >= 1`, at a point where `n` is known nonzero.
    ///
    /// It is a claim *about* another shape rather than a shape of its own, so it is inert everywhere the constructor order is read — never equal to anything, never a subterm of anything — and is consulted only by [`Shape::against`].
    Smaller(Free),
    /// Anything else. Never equal to, and never a subterm of, anything — including another `Opaque`, since two unreadable terms are not thereby the same term.
    Opaque,
}

impl Shape {
    /// Whether these are the same value. `Opaque` is deliberately unequal to itself: it means "not read", not "read and found identical".
    pub(super) fn same_as(&self, other: &Shape) -> bool {
        match (self, other) {
            (Shape::Atom(left), Shape::Atom(right)) => left == right,
            (Shape::Node(left, left_kids), Shape::Node(right, right_kids)) => {
                left == right
                    && left_kids.len() == right_kids.len()
                    && left_kids
                        .iter()
                        .zip(right_kids)
                        .all(|(left, right)| left.same_as(right))
            }
            _ => false,
        }
    }

    /// Whether `self` occurs strictly inside `whole`.
    pub(super) fn proper_subterm_of(&self, whole: &Shape) -> bool {
        match whole {
            Shape::Node(_, kids) => kids
                .iter()
                .any(|kid| self.same_as(kid) || self.proper_subterm_of(kid)),
            _ => false,
        }
    }

    /// This argument's size against a parameter whose expanded value is `parameter` — the whole size order, applied to one entry.
    pub(super) fn against(&self, parameter: &Shape) -> Size {
        // An arithmetic decrease names the binder it is below, so it grades only against a parameter still standing for exactly that binder. A parameter an enclosing arm has refined to a constructor is a different value, and withholding the claim there is the conservative reading rather than a missed case.
        if let Shape::Smaller(below) = self {
            return match parameter {
                Shape::Atom(atom) if atom == below => Size::Less,
                _ => Size::Unknown,
            };
        }
        if self.same_as(parameter) {
            return Size::Same;
        }
        if self.proper_subterm_of(parameter) {
            return Size::Less;
        }
        Size::Unknown
    }
}
