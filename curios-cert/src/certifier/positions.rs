//! The erased positions one item's check recorded — the seed type totality and proof totality run on.
//!
//! This is the certifier's *output*, not an input: nothing here is consulted to decide whether a term is well-typed. It is collected during the typing walk because that is the only place the answer is available, and drained per item by `Certifier::take_checked`.
//!
//! **Classified when recorded, never afterwards.** A position's type routinely mentions the binders the item opened, and those are retracted the moment the item's check returns, so a later pass cannot ask for their sorts at all — it can only fail, and a quiet failure would leave positions silently unconstrained.
//!
//! Two things make that workable. Which half a type's positions belong to is remembered beside the certifier's sorts and under their lives ([`Memos`](super::Memos)), so classifying at every record site costs one question per type for as long as what its sort was read off stands — and a type an arm's equation makes a proposition is classified under each arm's own, where a memo kept here for the whole item would hand one arm's answer to the next. And a classification that could not be decided is kept and surfaced with the drain, since a recording site returns nothing and cannot report it — this component's job rather than a caller's. Deciding a position's erased half records nothing of its own: it classifies in a reading, which offers no recording ([`ByValue`](super::ByValue)).

use {
    super::Error,
    curios_analysis::Erased,
    curios_core::{Free, Term},
    std::mem,
};

/// One erased position an item's check recorded.
pub struct Position {
    pub term: Term,
    pub erased: Erased,
    /// Whether typing `term` closed a group that does not descend — noted where the group was typed, since the group is typed with the binders around it opened and `term` holds it closed.
    pub encloses_partial: bool,
}

#[derive(Default)]
pub struct Positions {
    recorded: Vec<Position>,
    /// The first classification that could not be decided.
    failure: Option<Error>,
    /// Set while a reader reads ([`Certifier::read`](super::Certifier::read)), whose judgments are no item's check.
    reading: bool,
}

impl Positions {
    /// Whether a reader reads, whose judgments are no item's check: the record is dropped.
    pub fn suppressed(&self) -> bool {
        self.reading
    }

    /// Drop every record while `reading`, handing back whether records were already being dropped for a reader.
    pub fn hold_for_reading(&mut self, reading: bool) -> bool {
        mem::replace(&mut self.reading, reading)
    }

    /// Hand back what a type classified as, keeping the first failure to surface with the drain.
    pub fn settle(&mut self, outcome: Result<Option<Erased>, Error>) -> Option<Erased> {
        match outcome {
            Ok(erased) => erased,
            Err(error) => {
                self.failure.get_or_insert(error);
                None
            }
        }
    }

    /// Record `term` at `erased`, handing back where it was recorded for [`Positions::enclose_partial`].
    pub fn push(&mut self, term: &Term, erased: Erased) -> usize {
        self.recorded.push(Position {
            term: term.clone(),
            erased,
            encloses_partial: false,
        });

        self.recorded.len() - 1
    }

    /// The position recorded at `index` enclosed a group that does not descend.
    pub fn enclose_partial(&mut self, index: usize) {
        if let Some(position) = self.recorded.get_mut(index) {
            position.encloses_partial = true;
        }
    }

    /// Each position recorded since `since` whose term names one of `members`, a group's that does not descend, read as it stands outside the group: each member by its name there, `outside`, where it has one — an item's member is its declaration — so the position is judged as any other naming a definition not known to terminate; and where it has none, a group in a term, enclosing that group, which a reading as reduced would unfold without end. The members, assumed while the group's signatures and bodies are checked, stand in those positions as locals that name nothing as locals that hold no group.
    pub fn close_over_group(&mut self, since: usize, members: &[Free], outside: Option<&[Term]>) {
        let named = outside.map(|outside| {
            members
                .iter()
                .copied()
                .zip(outside.iter().cloned())
                .collect::<Vec<_>>()
        });
        for position in self.recorded.iter_mut().skip(since) {
            if !members
                .iter()
                .any(|member| position.term.mentions_free(member))
            {
                continue;
            }
            match &named {
                Some(named) => position.term = position.term.substitute(named),
                None => position.encloses_partial = true,
            }
        }
    }

    /// How many positions are recorded and whether a classification has failed, for a walk that may be refused to be taken back to.
    pub fn recorded(&self) -> (usize, bool) {
        (self.recorded.len(), self.failure.is_some())
    }

    /// Take back what a refused walk recorded since `recorded`: its positions, and a classification that failed inside it.
    pub fn take_back(&mut self, (positions, failed): (usize, bool)) {
        self.recorded.truncate(positions);
        if !failed {
            self.failure = None;
        }
    }

    /// Take this item's positions and any classification that could not be decided, leaving both empty for the next item.
    pub fn drain(&mut self) -> (Vec<Position>, Option<Error>) {
        (mem::take(&mut self.recorded), self.failure.take())
    }
}
