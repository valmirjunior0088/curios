//! What a term is ranked by among the atoms of one reading: its shape, then the age of the identities it holds.
//!
//! **An identity is compared, never read as a value.** A binder, a metavariable, a universe metavariable and a quality metavariable are each a position in a counter its declaration mints from, and how far the counter had run when an identity was minted follows every walk that minted before it: a mismatch's display built and discarded, a closed run answered from a memo or made again, a probe that opened a binder. So a rank reads an identity only by comparing it with another of its kind, which is its age, the one property of a counter's value no identity minted since moves. For locals that stand in one term, the older is the one whose scope encloses the other's or comes before it in a telescope, since a scope is opened from the body of the one enclosing it: so age orders locals by where their binders stand, and orders a metavariable, which has no binder, the same way.
//!
//! **A term's shape is its spelling with each identity spelled by where a walk first meets it**, so `f(x, y)` and `f(y, z)` have one shape and `f(x, x)` another, and the shape is that spelling's structural hash. Two terms of one shape are ordered by the ages of the identities they hold, in the order the walk first met them: the shape says which positions hold one identity, and the ages say which identities those are.
//!
//! A rank orders and never identifies: two terms of one rank are one term up to a collision of the shape's hash, which costs at most the order two atoms are put in.

use {
    super::{Subterm, Term},
    crate::{
        Bound, Free, Level, LevelHead, MetavarId, Quality, QualityMetaId, UniverseMetaId, Var,
        Visit, rewrite_universe_levels_scoped_shared, rewrite_universe_qualities,
    },
    curios_algebra::Rank,
    std::{cell::RefCell, collections::HashMap, convert::Infallible, rc::Rc},
};

/// The kind an identity is of. Two identities of two kinds are told apart by their kinds, and only two of one kind are compared by age: the shape spells each identity as its kind, so two terms of one shape hold identities of one kind at each place of their ages.
const BINDER: usize = 0;
const METAVARIABLE: usize = 1;
const UNIVERSE: usize = 2;
const QUALITY: usize = 3;

/// The identities a walk met, each by its key, with the place it takes among those of its kind met before it.
#[derive(Default)]
struct Met {
    places: HashMap<(usize, u64), usize>,
    /// How many identities of each kind have been met.
    counts: [usize; 4],
    /// Each identity, in the order it was first met.
    ages: Vec<u64>,
}

impl Met {
    /// The place `identity`, of `kind`, takes among the identities of that kind: the first met is the first place, wherever it stands.
    fn place(&mut self, kind: usize, identity: u64) -> usize {
        if let Some(place) = self.places.get(&(kind, identity)) {
            return *place;
        }
        let count = &mut self.counts[kind];
        let place = *count;
        *count += 1;
        self.places.insert((kind, identity), place);
        self.ages.push(identity);
        place
    }
}

/// `term`'s rank, its shape first and then the ages of the identities it holds. A term that holds none is ranked by its structural hash.
pub(super) fn ranked(term: &Term) -> Rank {
    let met = Rc::new(RefCell::new(Met::default()));
    let spelled = respelled(term, &met);

    let at_level = Rc::clone(&met);
    let Ok(spelled) = rewrite_universe_levels_scoped_shared(&spelled, move |_, level: &Level| {
        Ok::<_, Infallible>(
            level
                .substitute(|head| match head {
                    LevelHead::Meta(meta) => Some(Level::meta(UniverseMetaId(
                        at_level.borrow_mut().place(UNIVERSE, meta.0 as u64),
                    ))),
                    LevelHead::Param(_) => None,
                })
                .expect("a head respelled as a head at its own offset overflows nothing"),
        )
    });
    let at_quality = Rc::clone(&met);
    let Ok(spelled) = rewrite_universe_qualities(&spelled, move |quality: &Quality| {
        Ok::<_, Infallible>(match quality {
            Quality::Meta(meta) => Quality::Meta(QualityMetaId(
                at_quality.borrow_mut().place(QUALITY, meta.0 as u64),
            )),
            other => *other,
        })
    });

    let mut keys = vec![spelled.structural_hash()];
    keys.extend(met.borrow().ages.iter().copied());
    Rank::of(&keys)
}

/// `term` with each local and each metavariable spelled by its place among those of its kind a walk of the term meets first, a metavariable's spine walked after it.
fn respelled(term: &Term, met: &Rc<RefCell<Met>>) -> Term {
    let at_var = Rc::clone(met);
    let at_term = Rc::clone(met);
    term.traverse(&mut Visit::rewriting_shared(
        move |_, var: &Var| {
            let index = var.as_free()?.local_index()?;
            let place = at_var.borrow_mut().place(BINDER, index);
            Some(Subterm::Var(Var::free(Free::local(
                u64::try_from(place).expect("a place among a term's binders is an identity"),
                None,
            ))))
        },
        Box::new(move |_, term: &Term| {
            let Subterm::Metavar(metavar) = &**term else {
                return None;
            };
            let place = at_term
                .borrow_mut()
                .place(METAVARIABLE, metavar.id.0 as u64);
            let spine = metavar
                .spine
                .iter()
                .map(|argument| respelled(argument, &at_term))
                .collect::<Vec<_>>();
            Some(Term::metavar_birthed(
                MetavarId(place),
                metavar.origin.clone(),
                spine,
            ))
        }),
    ))
}
