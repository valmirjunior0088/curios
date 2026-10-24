//! The atoms of one algebraic operation: the terms `curios-algebra` reasons over, each handed to it as a handle.
//!
//! **Atom identity is the carrier's, and it is stated here, once.** `curios-algebra` never asks whether two handles might denote one value, so what "the same atom" means at a carrier is exactly which terms [`same_atom`] holds one: the terms a table maps to one handle ([`Atoms::handle`]), and the operands a reader that keeps no table compares directly. The rank each handle carries is the order it is put in: the term's shape, then the age of each identity it holds ([`Term::rank`]), so the order reads no count of what was minted before.
//!
//! **An atom of a carrier whose values hold no type is the same up to universe instances** ([`Carrier::holds_types`]). Two occurrences of one polymorphic name are instantiated independently, and Core offers no elimination from a type or a level into a number, a truth value, a byte, a float or a packed sequence of them, so a level is never part of the answer to "are these the same value": `len(xs)` written twice is one atom, which is what lets a bound over it cancel against itself, and `even(t)` written twice is one leaf of a conjunction. The projection is those carriers' licence and nothing wider: it does not make terms equal in general, it is not how a refinement is keyed, and an element of a list is known by its term as written, a list holding values of any type.
//!
//! **A product's factors are atoms as written** ([`Atoms::exact`]), ranked as the terms they are: distribution builds each product's spine from its factors in that order, so two spellings of one factor must stay two factors or the rebuilt spine would change which one it spells. The sum those products are merged into then keys them by the carrier, as every sum is.
//!
//! **A truth value that is a comparison is known by what it states.** A `Nat` or `Int` comparison is an atom of `Bool` known by its linear view ([`crate::LinearViews`]), aligned and read at a polarity (`curios-algebra`'s `LinearForm::polarized`), which is how conversion reads two comparisons: `x < y`, `x + 1 <= y` and `Nat/to_int(x) < Nat/to_int(y)` state one proposition, and `y <= x` and `y < x + 1` its negation. So [`same_atom`] holds two comparisons one where their aligned views agree, a table hands out one handle a proposition ([`Atoms::handle`]), a proposition and its negation being two, and one handle a polarized view with the polarity beside it ([`Atoms::signed`]), what a reader asks whose formulas hold a negation as a node of their own. Every reader of a `Bool` atom reads it through these, so none knows a comparison by how it is spelled. A view with no monomial left is decided by its constant, which the fold answers, and is known as its term.
//!
//! **A collision never merges.** Every identity keys on a term or a view compared by equality, so two keys whose hashes collide are told apart: a collision costs a probe, and at most the order two atoms of one rank are put in.
//!
//! A table lives as long as the operation that made it, and its handles mean nothing outside that operation.

#[cfg(test)]
mod tests;

use {
    super::{Aligned, LinearViews, Subterm, Term},
    curios_algebra::{Atom, Carrier, LinearForm, Operation, Rank},
    std::{
        collections::{HashMap, hash_map::DefaultHasher},
        hash::{Hash, Hasher},
    },
};

/// Whether `this` and `that` are one atom of `carrier`: one term, or, where the carrier's values hold no type, one term up to universe instances, and at `Bool` two comparisons that state one proposition.
pub fn same_atom(carrier: Carrier, this: &Term, that: &Term) -> bool {
    this == that
        || (!carrier.holds_types() && this.erased_universes() == that.erased_universes())
        || (carrier == Carrier::Boolean && one_proposition(this, that))
}

/// Whether two terms are `Nat` or `Int` comparisons whose views, read by one reader, agree once aligned.
fn one_proposition(this: &Term, that: &Term) -> bool {
    let (Subterm::Intrinsic(this), Subterm::Intrinsic(that)) = (&**this, &**that) else {
        return false;
    };
    matches!(
        LinearViews::default().align(this, that),
        Some(Aligned::Same)
    )
}

/// What a table keys a handle by: a term, as its carrier knows one, or what a truth value that is a comparison states.
#[derive(Clone, PartialEq, Eq, Hash)]
enum Key {
    Term(Term),
    /// A comparison's polarized view, and whether the comparison states its negation.
    View {
        relation: Operation,
        form: LinearForm,
        negated: bool,
    },
}

/// The handles one operation has handed out, keyed by the carrier's identity, with the first term each stood for.
#[derive(Default)]
pub(crate) struct Atoms {
    index: HashMap<Key, Atom>,
    terms: Vec<Term>,
    /// The reader every comparison this table keys is viewed by, so the atoms of two views are one reader's handles and the views compare.
    views: Option<Box<LinearViews>>,
}

impl Atoms {
    /// The handle for `term` as an atom of `carrier`: one handle for the terms [`same_atom`] holds one, ranked as what they are known by, so the rank is as blind to instances as the identity is.
    pub(crate) fn handle(&mut self, carrier: Carrier, term: &Term) -> Atom {
        if carrier == Carrier::Boolean
            && let Some((relation, form, negated)) = self.polarized(term)
        {
            return self.keyed(
                Key::View {
                    relation,
                    form,
                    negated,
                },
                term,
            );
        }
        match carrier.holds_types() {
            true => self.keyed(Key::Term(term.clone()), term),
            false => self.keyed(Key::Term(term.erased_universes()), term),
        }
    }

    /// The handle for `term` as an atom of `Bool` read at a polarity, beside whether `term` is that handle's negation: a comparison and its negation, however each is spelled, are one handle at two polarities, and any other term is [`Atoms::handle`]'s, read positive.
    pub(crate) fn signed(&mut self, term: &Term) -> (Atom, bool) {
        match self.polarized(term) {
            Some((relation, form, negated)) => {
                let key = Key::View {
                    relation,
                    form,
                    negated: false,
                };
                (self.keyed(key, term), negated)
            }
            None => (self.handle(Carrier::Boolean, term), false),
        }
    }

    /// The handle for `term` as written: one handle per term, ranked as itself.
    pub(crate) fn exact(&mut self, term: &Term) -> Atom {
        self.keyed(Key::Term(term.clone()), term)
    }

    /// The first term `atom` was handed out for.
    pub(crate) fn term(&self, atom: &Atom) -> &Term {
        &self.terms[atom.index() as usize]
    }

    /// `term`'s view read at its polarity by this table's reader, where `term` is a comparison whose view keeps a monomial.
    fn polarized(&mut self, term: &Term) -> Option<(Operation, LinearForm, bool)> {
        let Subterm::Intrinsic(comparison) = &**term else {
            return None;
        };
        self.views
            .get_or_insert_with(Box::default)
            .polarized(comparison)
    }

    fn keyed(&mut self, key: Key, term: &Term) -> Atom {
        if let Some(atom) = self.index.get(&key) {
            return atom.clone();
        }
        let rank = match &key {
            Key::Term(known) => known.rank(),
            Key::View {
                relation,
                form,
                negated,
            } => view_rank(*relation, form, *negated),
        };
        let atom = Atom::new(self.terms.len() as u32, rank);
        self.index.insert(key, atom.clone());
        self.terms.push(term.clone());
        atom
    }
}

/// A view's rank, read off what it states through its atoms' ranks and never through the order its reader handed them out in: one hash, the same in every run.
fn view_rank(relation: Operation, form: &LinearForm, negated: bool) -> Rank {
    let mut hasher = DefaultHasher::new();
    (relation, negated, &form.constant).hash(&mut hasher);
    for (coefficient, monomial) in &form.terms {
        coefficient.hash(&mut hasher);
        for atom in monomial.atoms() {
            atom.rank().hash(&mut hasher);
        }
    }
    Rank::from(hasher.finish())
}
