//! A call argument or a parameter read as a constructor tree — the term the size order is a proper-subterm order on.
//!
//! Every leaf is either a binder identity minted by the traversal that built it, or a closed literal shape. That is what makes the comparison trustworthy: two trees compare equal only when they name the same binders, so a same-named constructor of a different type would still have to reach the same binders to be mistaken for one.
//!
//! Free-monoid layers are held as packed **runs** — [`Shape::UnaryRun`], [`Shape::ElemRun`] — mirroring the carriers' own packed representation: `Nat::Succ` carries its spine as a count and `Bin` its bytes as a value, so a literal's shape costs its representation rather than its value. The subterm order on a run is decided by count arithmetic and suffix agreement; expanding a run into one nested node per layer would make both the construction and every comparison recurse as deep as the literal is large, which for a `Nat` is unbounded by the source that spelled it.

use {
    super::Size,
    curios_core::{Atom, Free, Global},
    num_bigint::BigUint,
};

/// The constructor a [`Shape`] node stands for.
///
/// Carriers are kept apart so a `Bin` layer can never be mistaken for an `Lst` layer. Well-typed code could not confuse them, but the size order is only as trustworthy as the identities it compares.
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
    /// A free-monoid carrier's identity: zero, `b[]`, `[]`.
    Empty(Carriers),
    /// A boolean literal.
    Bool(bool),
}

/// Which native free monoid a run or a [`Tag::Empty`] belongs to.
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
    /// A packed run of `Nat` successors: `count` unary layers over `tail`, the shape-level mirror of `Nat::Succ`'s packed spine. Canonical by construction — built only through [`Shape::unary_run`], so the count is nonzero and the tail is never itself a unary run.
    UnaryRun { count: BigUint, tail: Box<Shape> },
    /// A packed run of `Bin`/`Lst` elements over `tail`, front first — one head shape per element, held breadth-wise. The carrier is [`Carriers::Bin`] or [`Carriers::Lst`]; `Nat`'s payload-less layers are [`Shape::UnaryRun`]. Canonical by construction — built only through [`Shape::elem_run`], so the heads are nonempty and the tail is never an elem run of the same carrier.
    ElemRun {
        carrier: Carriers,
        heads: Vec<Shape>,
        tail: Box<Shape>,
    },
    /// A `Nat` strictly below the binder it names, established arithmetically rather than structurally: `n / k` for a literal `k >= 2`, or `n - k` for a literal `k >= 1`, at a point where `n` is known nonzero.
    ///
    /// It is a claim *about* another shape rather than a shape of its own, so it is inert everywhere the constructor order is read — never equal to anything, never a subterm of anything — and is consulted only by [`Shape::against`].
    Smaller(Free),
    /// Anything else. Never equal to, and never a subterm of, anything — including another `Opaque`, since two unreadable terms are not thereby the same term.
    Opaque,
}

impl Shape {
    /// `count` successors over `tail`. Zero successors are the tail itself, and a run landing on another run merges, so the canonicity the comparisons assume is established here rather than at every builder.
    pub(super) fn unary_run(count: BigUint, tail: Shape) -> Shape {
        if count == BigUint::from(0usize) {
            return tail;
        }
        match tail {
            Shape::UnaryRun { count: inner, tail } => Shape::UnaryRun {
                count: count + inner,
                tail,
            },
            tail => Shape::UnaryRun {
                count,
                tail: Box::new(tail),
            },
        }
    }

    /// `heads` elements over `tail`, front first — the [`Shape::unary_run`] rule for the element carriers.
    pub(super) fn elem_run(carrier: Carriers, heads: Vec<Shape>, tail: Shape) -> Shape {
        if heads.is_empty() {
            return tail;
        }
        match tail {
            Shape::ElemRun {
                carrier: inner,
                heads: rest,
                tail,
            } if inner == carrier => {
                let mut heads = heads;
                heads.extend(rest);
                Shape::ElemRun {
                    carrier,
                    heads,
                    tail,
                }
            }
            tail => Shape::ElemRun {
                carrier,
                heads,
                tail: Box::new(tail),
            },
        }
    }

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
            (
                Shape::UnaryRun {
                    count: left,
                    tail: left_tail,
                },
                Shape::UnaryRun {
                    count: right,
                    tail: right_tail,
                },
            ) => left == right && left_tail.same_as(right_tail),
            (
                Shape::ElemRun {
                    carrier: left,
                    heads: left_heads,
                    tail: left_tail,
                },
                Shape::ElemRun {
                    carrier: right,
                    heads: right_heads,
                    tail: right_tail,
                },
            ) => {
                left == right
                    && left_heads.len() == right_heads.len()
                    && left_heads
                        .iter()
                        .zip(right_heads)
                        .all(|(left, right)| left.same_as(right))
                    && left_tail.same_as(right_tail)
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
            // The proper subterms of `count` successors over `tail` are the strictly shorter runs over that same tail — `self` is one exactly when it is a unary run agreeing with `tail`, canonicity having merged any run `tail` could otherwise start with — and `tail` itself with everything inside it.
            Shape::UnaryRun { count, tail } => {
                if let Shape::UnaryRun {
                    count: mine,
                    tail: my_tail,
                } = self
                    && mine < count
                    && my_tail.same_as(tail)
                {
                    return true;
                }
                self.same_as(tail) || self.proper_subterm_of(tail)
            }
            // The proper subterms of an element run are each head with everything inside it, the strictly shorter suffix runs over the same tail, and the tail itself with everything inside it.
            Shape::ElemRun {
                carrier,
                heads,
                tail,
            } => {
                if heads
                    .iter()
                    .any(|head| self.same_as(head) || self.proper_subterm_of(head))
                {
                    return true;
                }
                if let Shape::ElemRun {
                    carrier: mine,
                    heads: my_heads,
                    tail: my_tail,
                } = self
                    && mine == carrier
                    && my_heads.len() < heads.len()
                    && my_heads
                        .iter()
                        .zip(&heads[heads.len() - my_heads.len()..])
                        .all(|(left, right)| left.same_as(right))
                    && my_tail.same_as(tail)
                {
                    return true;
                }
                self.same_as(tail) || self.proper_subterm_of(tail)
            }
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
