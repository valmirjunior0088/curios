//! What a refused bound's report says about the procedure: the facts it considered, each with where it came from, the propositions in scope it could not read, and an assignment of the atoms that satisfies the facts and falsifies the goal where the search produced one.
//!
//! Terms, not strings: the report is spelled where it is displayed, for the reader the bound's own spelling is for, so a fact reads as the author would write it.

use {
    super::{Facts, Origin, Rational},
    curios_algebra::Monomial,
    curios_core::{LinearViews, Term},
};

/// Why the procedure proved nothing.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Refusal {
    /// The facts it considered, each spelled as the comparison it states, beside where it came from. That a natural is at least zero, and what an operation defines, are left out: they hold of every program, and naming them buries the facts a reader can act on.
    pub considered: Vec<(Origin, Term)>,
    /// The propositions in scope stating a decision the view does not read.
    pub unread: Vec<(Origin, Term)>,
    /// What the search concluded.
    pub conclusion: Conclusion,
    /// Whether the bound was an empty proposition, which only a contradiction among the facts proves: a counterexample then shows them all holding, not a goal failing.
    pub absurd: bool,
}

/// The search's conclusion, as the report states it.
#[derive(Clone, Debug, Default, PartialEq)]
pub enum Conclusion {
    /// Nothing was searched: the goal is no decision the fragment reads.
    #[default]
    Unsearched,
    /// Every atom a value that satisfies the facts and falsifies the goal, written as an integer or a fraction. Empty where an unknown was a product, since the search reads a product as an unknown of its own, and a value it gives one may be no product of its factors'.
    Counterexample(Vec<(Term, String)>),
    /// The search ran out of its cap, a count of derived rows.
    Exhausted(usize),
    /// A certificate was found and a name its proof applies is not in scope: inside `/std`, an item compiled before the vocabulary.
    Unwritten,
    /// A certificate was found and its proof did not check: the procedure's mistake, reported as the refusal it has to be.
    Rejected,
}

/// How a search ended, before it is spelled for the report.
pub(super) enum SearchOutcome {
    Counterexample(Vec<(Monomial, Rational)>),
    Exhausted(usize),
    Unwritten,
    Rejected,
}

impl Refusal {
    /// Whether the refusal says anything at all: a bound the procedure read nothing in reports exactly as it would with no procedure.
    pub fn is_silent(&self) -> bool {
        self.considered.is_empty()
            && self.unread.is_empty()
            && self.conclusion == Conclusion::Unsearched
    }

    /// Every term the refusal spells, for the reader's spelling to cover.
    pub fn terms(&self) -> impl Iterator<Item = &Term> {
        let values = match &self.conclusion {
            Conclusion::Counterexample(assignment) => assignment.as_slice(),
            _ => &[],
        };
        self.considered
            .iter()
            .chain(&self.unread)
            .flat_map(|(from, statement)| from.named().into_iter().chain([statement]))
            .chain(values.iter().map(|(atom, _)| atom))
    }

    /// The report of a search over `facts`, read by `views`, that ended in `outcome`.
    pub(super) fn of(
        facts: &Facts,
        views: &LinearViews,
        outcome: SearchOutcome,
        absurd: bool,
    ) -> Self {
        let considered = facts
            .facts
            .iter()
            .filter(|fact| fact.origin.is_reported())
            .map(|fact| (fact.origin.clone(), fact.stated.clone()))
            .collect();
        let outcome = match outcome {
            SearchOutcome::Counterexample(assignment) => {
                Conclusion::Counterexample(counterexample(views, &assignment))
            }
            SearchOutcome::Exhausted(cap) => Conclusion::Exhausted(cap),
            SearchOutcome::Unwritten => Conclusion::Unwritten,
            SearchOutcome::Rejected => Conclusion::Rejected,
        };
        Refusal {
            considered,
            unread: facts.unread.clone(),
            conclusion: outcome,
            absurd,
        }
    }
}

/// Each atom's term and value, where every unknown is an atom; nothing where one is a product.
fn counterexample(views: &LinearViews, assignment: &[(Monomial, Rational)]) -> Vec<(Term, String)> {
    if assignment
        .iter()
        .any(|(monomial, _)| monomial.atoms().len() != 1)
    {
        return Vec::new();
    }
    // In the order the atoms were read — the goal's first — rather than the order they were eliminated in.
    let mut values = assignment
        .iter()
        .map(|(monomial, value)| (monomial.atoms()[0], value))
        .collect::<Vec<_>>();
    values.sort_by_key(|(atom, _)| atom.index());
    values
        .into_iter()
        .map(|(atom, value)| (views.term(atom).clone(), value.to_string()))
        .collect()
}
