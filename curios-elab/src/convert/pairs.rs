//! The unfoldings a conversion problem stands inside.
//!
//! Two folded calls their heads and their spines do not decide are unfolded once, both of them, and compared again. Where that comparison poses the same two heads again, a second unfolding reproduces the first: the pair is apart there, and what it would take to hold it is an induction, which a lemma states (`documentation/design/soundness/conversion-recurrence.md`).
//!
//! A recursive definition opened alone, against a term with nothing to open, is no pair, and is recorded all the same. What a problem stands inside is part of what a run remembers having posed ([`Posed`](super::Posed)), so a problem posed inside an unfolding is never taken for the same problem posed outside it: skipped as posed already, it would be held by itself.

use {
    curios_core::Term,
    std::{
        hash::{DefaultHasher, Hash, Hasher},
        rc::Rc,
    },
};

/// The unfoldings on the way to a conversion problem, innermost first: the pairs of heads unfolded against each other, and the heads unfolded alone.
///
/// It is carried with the problem, as [`Binders`](super::Binders) are, and for their reason: the elaborator's conversion is a worklist that parks what it cannot decide, so what a problem stands inside is no call stack, and a problem retried later is retried inside what it was posed inside. A comparison a run opens for a part of the problem in hand is posed on that problem's pairs.
///
/// **Compared and hashed in what two chains do not share.** A problem hands its chain on to every problem it poses, so two chains are most often one allocation, and each link keeps the hash of the chain from it outward: a chain is a key at the cost of its innermost link, however long the walk that built it.
#[derive(Debug, Clone, Default)]
pub(crate) struct Pairs(Option<Rc<Unfolded>>);

#[derive(Debug)]
struct Unfolded {
    this: Term,
    /// The head `this` was unfolded against, or `None` where it was unfolded alone.
    that: Option<Term>,
    outer: Pairs,
    /// The innermost pair outside this unfolding, past every head unfolded alone: what [`Pairs::hold`] walks, so a pair is looked for among the pairs.
    pairs: Pairs,
    /// The hash of the chain from here outward.
    hash: u64,
}

impl Pairs {
    /// These unfoldings with `this` against `that` unfolded innermost.
    pub(super) fn unfolding(&self, this: Term, that: Term) -> Pairs {
        self.inside(this, Some(that))
    }

    /// These unfoldings with `head` unfolded alone innermost: no pair, so it holds nothing apart, and what tells a problem posed inside it from the same problem posed outside.
    pub(super) fn opening(&self, head: Term) -> Pairs {
        self.inside(head, None)
    }

    fn inside(&self, this: Term, that: Option<Term>) -> Pairs {
        let mut hasher = DefaultHasher::new();
        this.hash(&mut hasher);
        that.hash(&mut hasher);
        self.hash(&mut hasher);

        Pairs(Some(Rc::new(Unfolded {
            this,
            that,
            outer: self.clone(),
            pairs: self.innermost_pair(),
            hash: hasher.finish(),
        })))
    }

    /// These unfoldings from their innermost pair outward.
    fn innermost_pair(&self) -> Pairs {
        match &self.0 {
            Some(unfolded) if unfolded.that.is_none() => unfolded.pairs.clone(),
            _ => self.clone(),
        }
    }

    /// Whether `this` against `that` is among these pairs, whichever side each was on.
    pub(super) fn hold(&self, this: &Term, that: &Term) -> bool {
        let mut pairs = self.innermost_pair();
        while let Some(pair) = pairs.0 {
            if let Some(against) = &pair.that
                && ((pair.this == *this && *against == *that)
                    || (pair.this == *that && *against == *this))
            {
                return true;
            }
            pairs = pair.pairs.clone();
        }

        false
    }
}

impl Drop for Unfolded {
    /// Released link by link. A chain is as long as the walk that built it, which a recursion opened alone without end makes as long as its budget, and dropped by recursion it would be as deep.
    fn drop(&mut self) {
        let mut outer = self.outer.0.take();
        while let Some(link) = outer {
            outer = match Rc::try_unwrap(link) {
                Ok(mut unfolded) => unfolded.outer.0.take(),
                Err(_) => None,
            };
        }
    }
}

impl PartialEq for Pairs {
    fn eq(&self, other: &Self) -> bool {
        let (mut this, mut that) = (self, other);
        loop {
            match (&this.0, &that.0) {
                (None, None) => return true,
                (Some(left), Some(right)) => {
                    if Rc::ptr_eq(left, right) {
                        return true;
                    }
                    if left.hash != right.hash || left.this != right.this || left.that != right.that
                    {
                        return false;
                    }
                    (this, that) = (&left.outer, &right.outer);
                }
                _ => return false,
            }
        }
    }
}

impl Eq for Pairs {}

impl Hash for Pairs {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_ref().map(|unfolded| unfolded.hash).hash(state);
    }
}
