//! The pairs of heads a conversion problem stands inside the unfolding of.
//!
//! Two folded calls their heads and their spines do not decide are unfolded once, both of them, and compared again. Where that comparison poses the same two heads again, a second unfolding reproduces the first: the pair is apart there, and what it would take to hold it is an induction, which a lemma states (`documentation/design/soundness/conversion-recurrence.md`).

use {curios_core::Term, std::rc::Rc};

/// The pairs of heads unfolded against each other on the way to a conversion problem, innermost first.
///
/// It is carried with the problem, as [`Binders`](super::Binders) are, and for their reason: the elaborator's conversion is a worklist that parks what it cannot decide, so what a problem stands inside is no call stack, and a problem retried later is retried inside what it was posed inside. A comparison a run opens for a part of the problem in hand is posed on that problem's pairs.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub(crate) struct Pairs(Option<Rc<Pair>>);

#[derive(Debug, PartialEq, Eq, Hash)]
struct Pair {
    this: Term,
    that: Term,
    outer: Pairs,
}

impl Pairs {
    /// These pairs with `this` against `that` unfolded innermost.
    pub(super) fn unfolding(&self, this: Term, that: Term) -> Pairs {
        Pairs(Some(Rc::new(Pair {
            this,
            that,
            outer: self.clone(),
        })))
    }

    /// Whether `this` against `that` is among these pairs, whichever side each was on.
    pub(super) fn hold(&self, this: &Term, that: &Term) -> bool {
        let mut pairs = self;
        while let Some(pair) = &pairs.0 {
            if (pair.this == *this && pair.that == *that)
                || (pair.this == *that && pair.that == *this)
            {
                return true;
            }
            pairs = &pair.outer;
        }

        false
    }
}
