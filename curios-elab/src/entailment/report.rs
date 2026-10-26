//! What a refused bound's report says about the procedure: the facts it considered, each with where it came from, the propositions in scope it could not read, and an assignment of the atoms that satisfies the facts and falsifies the goal where the search produced one.
//!
//! Terms, not strings: the report is spelled where it is displayed, for the reader the bound's own spelling is for, so a fact reads as the author would write it.

use {
    super::{Facts, Origin, Rational, respelled},
    crate::{Context, reduce_with},
    curios_algebra::Monomial,
    curios_core::{Free, Intrinsic, LinearViews, Nat, Subterm, Term},
    curios_num::{Integer, Natural},
};

/// Why the procedure proved nothing.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Refusal {
    /// The facts it considered, each spelled as the comparison it states, beside where it came from. That a natural is at least zero, and what an operation defines, are left out: they hold of every program, and naming them buries the facts a reader can act on.
    considered: Vec<(Origin, Term)>,
    /// The propositions in scope stating a decision the view does not read.
    unread: Vec<(Origin, Term)>,
    /// What the search concluded.
    conclusion: Conclusion,
    /// Whether the bound was an empty proposition, which only a contradiction among the facts proves: a counterexample then shows them all holding, not a goal failing.
    absurd: bool,
    /// Whether the counterexample is one no program takes (`refuted`): a report then states it, with the heading above it, as what the search found, each atom read as a number of its own.
    refuted: bool,
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
pub enum SearchOutcome {
    Counterexample(Vec<(Monomial, Rational)>),
    Exhausted(usize),
    Unwritten,
    Rejected,
}

impl Refusal {
    pub fn considered(&self) -> &[(Origin, Term)] {
        &self.considered
    }

    pub fn unread(&self) -> &[(Origin, Term)] {
        &self.unread
    }

    pub fn conclusion(&self) -> &Conclusion {
        &self.conclusion
    }

    pub fn absurd(&self) -> bool {
        self.absurd
    }

    pub fn refuted(&self) -> bool {
        self.refuted
    }

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

    /// [`Refusal::terms`], each to be written over.
    pub fn terms_mut(&mut self) -> impl Iterator<Item = &mut Term> {
        let Self {
            considered,
            unread,
            conclusion,
            ..
        } = self;
        let values: &mut [(Term, String)] = match conclusion {
            Conclusion::Counterexample(assignment) => assignment.as_mut_slice(),
            _ => &mut [],
        };
        considered
            .iter_mut()
            .chain(unread)
            .flat_map(|(from, statement)| from.named_mut().into_iter().chain([statement]))
            .chain(values.iter_mut().map(|(atom, _)| atom))
    }

    /// The refusal with its counterexample's atoms written through `spelled`, each reduct as the operand stated beside it, in one pass ([`respelled`]): an outer reduct is replaced before one inside it, whichever reading found either.
    pub(super) fn respelled(self, spelled: &[(Term, Term)]) -> Self {
        self.rewritten(|atom| respelled(spelled, atom))
    }

    /// The refusal with each of its counterexample's atoms as `write` writes it.
    pub(super) fn rewritten(mut self, mut write: impl FnMut(&Term) -> Term) -> Self {
        if let Conclusion::Counterexample(values) = &mut self.conclusion {
            for (atom, _) in values {
                *atom = write(atom);
            }
        }
        self
    }

    /// The report of a search over `facts`, read by `views`, that ended in `outcome`.
    pub(super) fn of(
        facts: &Facts,
        views: &LinearViews,
        outcome: SearchOutcome,
        absurd: bool,
        refuted: bool,
    ) -> Self {
        // A fact two hypotheses state as one, as a structure update's base and the term it stands for do, is listed once.
        let mut considered: Vec<(Origin, Term)> = Vec::new();
        // A disequality read as the two cases its sides' order makes is a fact of neither alone, and is named with the scope's.
        let cases = facts
            .splits()
            .iter()
            .flat_map(|split| split.holds().iter().chain(split.fails()))
            .filter(|fact| fact.origin.named().is_some());
        let scope = facts
            .facts()
            .iter()
            .filter(|fact| fact.origin.is_reported());
        for fact in scope.chain(cases) {
            let stated = (fact.origin.clone(), fact.stated.clone());
            if !considered.contains(&stated) {
                considered.push(stated);
            }
        }
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
            unread: facts.unread().to_vec(),
            conclusion: outcome,
            absurd,
            refuted,
        }
    }
}

/// Each atom's term and value, where every unknown is an atom; nothing where one is a product. An atom is the reduct the view read, written as its operands state it once every spelling a report has is known ([`Refusal::respelled`]).
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
        .map(|(monomial, value)| (monomial.atoms()[0].clone(), value))
        .collect::<Vec<_>>();
    values.sort_by_key(|(atom, _)| atom.index());
    values
        .into_iter()
        .map(|(atom, value)| (views.term(&atom).clone(), value.to_string()))
        .collect()
}

/// Whether no program takes `outcome`'s values, where it is a counterexample: a value that is no integer, or an atom that, with each variable's value put in, reduces to a literal other than its own value. An assignment this cannot refute is read as the search read it: an atom that names a local given no value, or that reduces to no literal, is not checked.
///
/// Asked once the search is over and only for a report, as what a report names an atom by is ([`Facts::beneath`]); a reduction that fails refutes nothing.
pub fn refuted(context: &mut Context, views: &LinearViews, outcome: &SearchOutcome) -> bool {
    let SearchOutcome::Counterexample(assignment) = outcome else {
        return false;
    };
    let mut values = Vec::new();
    for (monomial, value) in assignment {
        let Some(value) = value.integer() else {
            return true;
        };
        if let [atom] = monomial.atoms() {
            values.push((views.term(atom).clone(), value.clone()));
        }
    }

    // Each variable at its value, as a literal of the carrier its binder is typed at.
    let mut solutions: Vec<(Free, Term)> = Vec::new();
    for (term, value) in &values {
        if let Subterm::Var(var) = &**term
            && let Some(free) = var.as_free()
            && free.local_index().is_some()
            && let Some(literal) = literal(context, free, value)
        {
            solutions.push((*free, literal));
        }
    }

    values.iter().any(|(term, value)| {
        if matches!(&**term, Subterm::Var(_)) {
            return false;
        }
        let closed = term.substitute(&solutions);
        if closed.has_local_free() {
            return false;
        }
        reduce_with(context, &closed)
            .ok()
            .and_then(|reduced| integer(&reduced))
            .is_some_and(|reduced| reduced != *value)
    })
}

/// `value` as a literal of the carrier `free` is assumed at, a `Nat` or an `Int`, and none at any other or at a value the carrier does not hold.
fn literal(context: &mut Context, free: &Free, value: &Integer) -> Option<Term> {
    let type_ = context.assumption(free)?.clone();
    match &*reduce_with(context, &type_).ok()? {
        Subterm::Intrinsic(Intrinsic::NatType) => Some(Term::intrinsic(Intrinsic::Nat(Nat::new(
            Natural::try_from(value).ok()?,
        )))),
        Subterm::Intrinsic(Intrinsic::IntType) => {
            Some(Term::intrinsic(Intrinsic::Int(value.clone())))
        }
        _ => None,
    }
}

/// The integer a `Nat` or `Int` literal is.
fn integer(term: &Term) -> Option<Integer> {
    match &**term {
        Subterm::Intrinsic(Intrinsic::Nat(nat)) => nat.to_natural().map(Integer::from),
        Subterm::Intrinsic(Intrinsic::Int(value)) => Some(value.clone()),
        _ => None,
    }
}
