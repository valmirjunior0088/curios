//! Which half of the erased program a position belongs to, and what a proof reaches as the checkers reduce it.
//!
//! The classification is shared; the obligations that consume it are not. `curios-cert`'s `Totals` and `check_positions` decide type totality and proof totality from its own typing walk, and `curios-elab` decides them from its elaborator's check sites — two independent answers to one question, deliberately. What both need to name is the same pair of halves, so the pair lives here, and so does the one reading of a proof as reduced that both obligations' proof halves ask ([`reached_as_reduced`]), each over its own driver and its own record of what is not known to terminate.

use {
    crate::Driver,
    curios_common::recurse,
    curios_core::{Bound, Exhaustion, Foreign, Global, Instance, Match, Subterm, Term},
    std::collections::HashSet,
};

/// Which half of the erased program a position belongs to. Both are deleted, so both must terminate; the distinction is only what a diagnostic should call it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Erased {
    Proof,
    Type,
}

impl std::fmt::Display for Erased {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Erased::Proof => write!(formatter, "proof"),
            Erased::Type => write!(formatter, "type"),
        }
    }
}

/// The first definition not known to terminate that `term` reaches as the checkers reduce it, `partial` saying of a definition, by the driver's own record, whether it is one: `None` where reduction discards every one `term` names.
///
/// A part under no binder is forced through [`Driver::force`] and read again as it reduces, so a definition reduction discards is no reach, and a reduct headed by one, which no checker unfolds or whose unfolding stalled, is. A part that reads a binder `term` writes is read by the names it writes: forced, a recursion that descends on the binder would unfold once for every arm the walk opened, without end. A match's scrutinee is read as a part of its own before the match is reduced, so the step that reads past a proposition's closed scrutinee passes over none that loops. A reduct that keeps a host row that diverges reaches the first definition the part that reduced to it names.
///
/// The budget's refusal is handed back, for the caller to read the term by the names it writes; any other failure to reduce a part reads it so here.
pub fn reached_as_reduced<D: Driver>(
    driver: &mut D,
    term: &Term,
    partial: &dyn Fn(&D, &Global) -> bool,
) -> Result<Option<Global>, D::Error> {
    Reading {
        driver,
        partial,
        read: HashSet::new(),
    }
    .reached(term)
}

/// One walk of [`reached_as_reduced`]: the parts it has read, each found to reach nothing, so a part shared across a term is reduced once.
struct Reading<'a, D: Driver> {
    driver: &'a mut D,
    partial: &'a dyn Fn(&D, &Global) -> bool,
    read: HashSet<Term>,
}

impl<D: Driver> Reading<'_, D> {
    fn reached(&mut self, term: &Term) -> Result<Option<Global>, D::Error> {
        recurse(|| self.reached_within(term))
    }

    fn reached_within(&mut self, term: &Term) -> Result<Option<Global>, D::Error> {
        let Some(named) = term.first_declaration(|name| (self.partial)(&*self.driver, name)) else {
            return Ok(None);
        };
        if term.reach() != 0 {
            return Ok(Some(named));
        }
        if !self.read.insert(term.clone()) {
            return Ok(None);
        }
        if let Some(scrutinee) = spine_scrutinee(term)
            && let Some(reached) = self.reached(scrutinee)?
        {
            return Ok(Some(reached));
        }

        let reduct = match self.driver.force(term) {
            Ok(reduct) => reduct,
            Err(error) if error.is_exhausted() => return Err(error),
            Err(_) => return Ok(Some(named)),
        };
        match &*reduct {
            Subterm::Instance(Instance { name, .. }) if (self.partial)(&*self.driver, name) => {
                return Ok(Some(*name));
            }
            Subterm::Foreign(Foreign { function, .. }) if function.diverges() => {
                return Ok(Some(named));
            }
            _ => {}
        }

        let mut reached = Ok(None);
        reduct.any_child_term(&mut |part| {
            reached = self.reached(part);
            !matches!(reached, Ok(None))
        });
        reached
    }
}

/// The scrutinee of the match `term` is headed by, through its applications and projections: what reduction reads first, and what it reads past where it steps over a proposition's closed match.
fn spine_scrutinee(term: &Term) -> Option<&Term> {
    let mut head = term;
    loop {
        match &**head {
            Subterm::Apply(apply) => head = &apply.head,
            Subterm::Proj(projection) => head = &projection.head,
            Subterm::Match(Match {
                head: scrutinee, ..
            }) => return Some(scrutinee),
            _ => return None,
        }
    }
}
