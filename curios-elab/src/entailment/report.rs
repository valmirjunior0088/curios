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
    /// Of a bound that is a conjunction, the conjunct the rest of the refusal is about: the first one not proved, at the depth its conjunction nests it.
    conjunct: Option<Term>,
    /// The decision the bound asks for, as the bound spells it, where it is one: what a guard that decides the bound writes.
    goal: Option<Term>,
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

    pub fn conjunct(&self) -> Option<&Term> {
        self.conjunct.as_ref()
    }

    pub fn goal(&self) -> Option<&Term> {
        self.goal.as_ref()
    }

    /// This refusal, of a bound that asks for the decision `goal`.
    pub(super) fn of_goal(mut self, goal: Option<Term>) -> Self {
        self.goal = goal;
        self
    }

    /// Whether the refusal says anything at all: a bound the procedure read nothing in reports exactly as it would with no procedure. A conjunction's refusal names the conjunct it is about, so it says that much.
    pub fn is_silent(&self) -> bool {
        self.considered.is_empty()
            && self.unread.is_empty()
            && self.conclusion == Conclusion::Unsearched
            && self.conjunct.is_none()
    }

    /// A refusal of a proof that was written and did not check, with nothing else to say.
    pub(super) fn rejected() -> Self {
        Refusal {
            conclusion: Conclusion::Rejected,
            ..Refusal::default()
        }
    }

    /// This refusal, of the conjunct `conjunct`, as the refusal of a conjunction holding it: a conjunct it already names, nested deeper, is kept.
    pub(super) fn of_conjunct(mut self, conjunct: &Term) -> Self {
        self.conjunct.get_or_insert_with(|| conjunct.clone());
        self
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
            .chain(&self.conjunct)
            .chain(&self.goal)
    }

    /// [`Refusal::terms`], each to be written over.
    pub fn terms_mut(&mut self) -> impl Iterator<Item = &mut Term> {
        let Self {
            considered,
            unread,
            conclusion,
            conjunct,
            goal,
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
            .chain(conjunct)
            .chain(goal)
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
        let considered = considered(facts);
        let outcome = match outcome {
            SearchOutcome::Counterexample(assignment) => {
                Conclusion::Counterexample(counterexample(views, &assignment, refuted))
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
            conjunct: None,
            goal: None,
        }
    }
}

/// The facts of `facts` a report names, each with where it came from: those a reader can act on, and a disequality's two cases, each once.
pub(super) fn considered(facts: &Facts) -> Vec<(Origin, Term)> {
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
    // The guards are read innermost first, from the arm the bound stands in outwards; a report names them as the program writes them, outermost first, after the hypotheses.
    let (guards, others): (Vec<_>, Vec<_>) = considered
        .into_iter()
        .partition(|(origin, _)| matches!(origin, Origin::Guard(..)));
    others.into_iter().chain(guards.into_iter().rev()).collect()
}

/// What the procedure that proves a bound from the facts in scope says of a written goal that is a bound, for the goal's report: the procedure is what would have filled the goal had it been left out, so it answers the goal too.
#[derive(Clone, Debug, PartialEq)]
pub enum Answer {
    /// The bound holds, by reduction or from the facts in scope, which are named as a refusal names those it considered.
    Follows(Vec<(Origin, Term)>),
    /// It does not, and why.
    Refused(Refusal),
}

impl Answer {
    /// Every term the answer spells, for the reader's spelling to cover.
    pub fn terms(&self) -> impl Iterator<Item = &Term> {
        let (facts, refusal) = match self {
            Answer::Follows(facts) => (facts.as_slice(), None),
            Answer::Refused(refusal) => (&[][..], Some(refusal)),
        };
        facts
            .iter()
            .flat_map(|(from, statement)| from.named().into_iter().chain([statement]))
            .chain(refusal.into_iter().flat_map(Refusal::terms))
    }

    /// [`Answer::terms`], each to be written over.
    pub fn terms_mut(&mut self) -> impl Iterator<Item = &mut Term> {
        let (facts, refusal): (&mut [(Origin, Term)], _) = match self {
            Answer::Follows(facts) => (facts.as_mut_slice(), None),
            Answer::Refused(refusal) => (&mut [][..], Some(refusal)),
        };
        facts
            .iter_mut()
            .flat_map(|(from, statement)| from.named_mut().into_iter().chain([statement]))
            .chain(refusal.into_iter().flat_map(Refusal::terms_mut))
    }
}

/// Each atom's term and value, where every unknown is an atom; nothing where one is a product. An atom is the reduct the view read, written as its operands state it once every spelling a report has is known ([`Refusal::respelled`]). `refuted` says that no program takes the values, which every atom is then named to show.
fn counterexample(
    views: &LinearViews,
    assignment: &[(Monomial, Rational)],
    refuted: bool,
) -> Vec<(Term, String)> {
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
    let values = values
        .into_iter()
        .map(|(atom, value)| (views.term(&atom).clone(), value.to_string()))
        .collect::<Vec<_>>();
    // An operation of the carriers' arithmetic over another atom — `List/length(xs) - 1` beside `List/length(xs)` — is what the operation makes of that one, and in values a program takes, its value follows from theirs: named beside them, the two read as a contradiction to a reader who does not know how the operation is defined. Where no program takes the values, the atom is what shows it, `Nat/pow(2, n) = 1` beside `n = 1`, and stays. A call is no such operation, whatever it names inside it: the search reads it as an unknown of its own, and so does its reader.
    let held = |atom: &Term| {
        !refuted
            && matches!(&**atom, Subterm::Intrinsic(_))
            && values
                .iter()
                .any(|(other, _)| other != atom && atom.replace_term(other, atom) != *atom)
    };
    values
        .iter()
        .filter(|(atom, _)| !held(atom))
        .cloned()
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
