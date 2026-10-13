//! The procedure that proves a bound from the facts in scope, where the fill, `trivially_inhabited`, has no answer.
//!
//! **It writes a proof and elaborates it as written code is.** Both checkers recheck what it wrote, so a wrong proof is a term that does not check and the procedure's mistakes are refusals: nothing it adds is trusted. It changes no conversion rule, no refinement and no solving choice, and assigns only the hole it was asked about, with a term whose type is the hole's.
//!
//! **It runs only where the elaborator would otherwise report the bound**: at insertion, with the arm's refinements live, and at a parked bound's retry once the bound waits on nothing, under the refinements its slot was born under — which is what re-validation judges a solution by, so a guard is a fact for a bound born in its arm and not for one born outside it. A bound waiting only on the slot a `rec` group's member is known by while the group is checked is asked at both, and stays parked if refused: the slot's solution is the body the bound sits in, and a proof from the facts is written over the member's name rather than its slot. It does not run after an item closes, which would lose the guards, since a guard here is a refinement and not a binder.
//!
//! **What it proves.**
//!
//! - A decision conversion equates with `true` and reduction does not, `b || Bool/not(b)`: [`tautology`].
//! - A proposition a fact in scope states outright — a hypothesis, or a proof field one level down, whose statement converts with the bound with nothing solved: [`stated_by_a_fact`]. The fact is the proof, whatever the proposition is: a decision the linear view reads nothing in, or an inductive proposition the procedure never opens, such as an equation.
//! - A `Nat` or `Int` comparison that follows from the facts in scope ([`Reader`]) by linear arithmetic over the rationals, a strict integer fact strengthened to its successor, within the search's cap ([`refute`]): a `<` or a `<=` as it stands, a disequality through the equation its negation is, and an equality through the two cases its negation makes ([`negated`]). A consequence that holds only over the integers and needs a cut is refused, as `linarith` and `omega` without its dark and grey shadows refuse it.
//! - Over `Nat`, through the operations whose definitions are linear facts: a quotient or remainder through the quotient's bounds, a truncated subtraction through the case split `omega` makes, a `min` or a `max` through the split on its operands' order and an `Int`'s `abs` or `sign` through the splits on its operand's sign, each opened only where the search needs it ([`plan`]), and a power of a base conversion holds at least one, as the `pow(2, m)` a shift by a symbolic count multiplies by is, through its being at least one.
//! - Where linear arithmetic finds an assignment, through the products of pairs of facts and the negated goal, as `nlinarith` does ([`products`]): what a multiplier that is no literal needs, `Nat/div_mod`'s among them.
//! - Where the products leave one too, through the disequalities the scope states, each as the order comparison conversion holds it to be ([`disequalities`]): `total / (n + m)` under `Holds(n != 0)`. Asked last, so a bound the facts prove is proved as it was before a disequality was read at all.
//! - An empty proposition the facts refute: how `False/refuted` reaches the procedure, and `True/proved` wherever its proposition is one.
//!
//! **What failure is.** The bound's refusal, as the fill leaves it, naming the facts the procedure considered, those it could not read, and — where the search produced one — an assignment of the atoms that satisfies the facts and falsifies the goal ([`Refusal`]).
//!
//! **What it is not.** The fill stays what its documentation says it is, a unique answer and no search. This procedure is a separate, fallible step after it.

mod facts;
use facts::*;

mod proof;
use proof::*;

mod report;
pub use report::*;

mod search;
use search::*;

#[cfg(test)]
mod tests;

use {
    crate::{
        Context, Error, Mode, elaborate, named_members, reduce_with, same_uncommitted,
        zonk_solved_term_metas,
    },
    curios_algebra::{LinearForm, Monomial},
    curios_analysis::Classes,
    curios_core::{Cases, Free, Global, LinearViews, Match, Probe, Subterm, Term, Var},
    curios_utilities::SyntaxName,
};

pub use facts::Origin;

/// What the procedure concluded about a bound.
pub(crate) enum Entailed {
    /// A proof of the bound, elaborated against it.
    Proved(Term),
    /// No proof, and why: what the report the hole becomes says beside the bound.
    Refused(Refusal),
}

/// What a bound asks for, read off its reduct.
enum Goal {
    /// `Holds(e)`: the decision `e` is `true`.
    Decision(Term),
    /// A proposition with no constructor: the facts must refute each other.
    Absurd,
}

/// The goal `reduced` states, or `None` for a proposition the procedure reads nothing in.
fn goal_of(context: &mut Context, reduced: &Term) -> Result<Option<Goal>, Error> {
    if let Some(decision) = decision_of(context, reduced)? {
        return Ok(Some(Goal::Decision(decision)));
    }
    Ok(is_empty(context, reduced).then_some(Goal::Absurd))
}

/// Whether `reduced` is a proposition with no constructor.
fn is_empty(context: &Context, reduced: &Term) -> bool {
    matches!(&**reduced, Subterm::InductType(induct)
        if context
            .induct_decl(&induct.name)
            .is_some_and(|decl| decl.constructor_order().next().is_none()))
}

/// The decision `reduced` states is `true`, or `None` for a proposition the procedure reads nothing in. `/sys` states `Holds(b)` as `match b | true => True | false => False end`, so a bound it did not decide reduces to that match stuck on its decision. A weak head normal form leaves a match's arms as elaborated, so the true arm is reduced before it is compared with the truth.
fn decision_of(context: &mut Context, reduced: &Term) -> Result<Option<Term>, Error> {
    let Subterm::Match(Match {
        head,
        cases: Cases::Bool { true_case, .. },
        ..
    }) = &**reduced
    else {
        return Ok(None);
    };
    let truth = Global::Authored(context.syntax().proof.true_type.qualifier());
    let arm = reduce_with(context, true_case)?;
    let holds = matches!(&*arm, Subterm::InductType(induct) if induct.name == truth);
    Ok(holds.then(|| head.clone()))
}

/// A proof of `bound`, whose reduct `reduced` the fill found no inhabitant in, from the facts in scope — under whatever refinements are live, which the caller has made the ones a solution for the bound's slot is judged under.
///
/// An exhausted budget propagates: running out is not evidence the bound is false, and collapsing it into a refusal would report the author's fault at the argument for what is the resource limit.
pub(crate) fn entail(
    context: &mut Context,
    bound: &Term,
    reduced: &Term,
) -> Result<Entailed, Error> {
    curios_profile::profile!("entailment::entail");
    if context.entailing() {
        return Ok(Entailed::Refused(Refusal::default()));
    }
    // What the search reads in the bound, where it reads anything: a fact that states the bound proves it either way.
    let goal = goal_of(context, reduced)?;
    // The decision as the bound spells it, which the proof is written over where the bound's reduct is only read.
    let stated = held(context, bound)?;

    let entailed = context.with_entailing(|context| {
        if let Some(Goal::Decision(decision)) = &goal
            && let Some(candidate) = tautology(context, stated.as_ref().unwrap_or(decision))
            && let Some(proof) = check(context, &candidate, bound)?
        {
            return Ok(Entailed::Proved(proof));
        }
        if let Some(proof) = stated_by_a_fact(context, bound)? {
            return Ok(Entailed::Proved(proof));
        }
        match &goal {
            Some(goal) => linear(context, goal, stated.as_ref(), bound),
            None => Ok(Entailed::Refused(Refusal::default())),
        }
    })?;
    // The binders the proof reads are used, though the author wrote no reference to them.
    if let Entailed::Proved(proof) = &entailed {
        context.credit(proof);
    }
    Ok(match entailed {
        Entailed::Refused(refusal) => {
            // A bound that is a metavariable elaboration has solved, as the statement `True/proved()` leaves implicit is, opens to no `Holds`: the search read its reduct, and a counterexample would name an atom by the body it unfolds to. What the solution states is read here, for the report alone: the proof is written over what the search read, and one written over the solution does not check. A reading that runs out leaves the report as the search made it.
            let refusal = match stated {
                Some(_) => refusal,
                None => {
                    let solved = zonk_solved_term_metas(context, bound);
                    match stated_spellings(context, &solved) {
                        Ok(spelled) => refusal.respelled(&spelled),
                        Err(_) => refusal,
                    }
                }
            };
            // An atom is written as the operand it was read from states it, where a metavariable elaboration has since solved may still stand: substituted, as it is in the bound the report names above it.
            Entailed::Refused(refusal.rewritten(|atom| zonk_solved_term_metas(context, atom)))
        }
        entailed => entailed,
    })
}

/// The fact that states `bound` outright, as its proof: the first hypothesis, or proof field one level down, in scope order, whose statement converts with the bound and checks against it.
///
/// **Conversion alone, and nothing solved.** A statement is put to the bound only where the two convert with no metavariable assigned and no universe constrained, so the step makes no solving choice: a bound still waiting on its subject is stated by no fact yet, and is asked again once the subject is known. Congruence and a fact's consequences are no part of it — within its fragment they are the linear half's, and past it the author's.
///
/// **Any proposition.** The bound need be nothing the search reads: a proposition is proof-irrelevant, so the fact that states it is as good a proof as any, and which fact is taken decides nothing.
fn stated_by_a_fact(context: &mut Context, bound: &Term) -> Result<Option<Term>, Error> {
    curios_profile::profile!("entailment::stated_by_a_fact");
    for (name, type_) in binders(context) {
        for (statement, proof, _) in statements(context, &name, &type_)? {
            let states = same_uncommitted(context, &statement, bound).map_err(|error| {
                Error::from_reduce(error, |refusal| {
                    Error::reduce_exhausted(bound.clone(), refusal)
                })
            })?;
            if states && let Some(proof) = check(context, &proof, bound)? {
                return Ok(Some(proof));
            }
        }
    }
    Ok(None)
}

/// The linear half: the goal and the facts read by one reader, the search over them and the negated goal, and the proof the certificate stands for. An absurd goal has no target and no negation: the facts must refute each other alone.
fn linear(
    context: &mut Context,
    goal: &Goal,
    stated: Option<&Term>,
    bound: &Term,
) -> Result<Entailed, Error> {
    let mut views = LinearViews::default();
    let mut classes = Classes::default();
    let mut reader = Reader::new(&mut views, &mut classes);
    // A decision that is no `Nat` or `Int` comparison is not a goal of the fragment. The goal is read first, so its atoms are handed out before any fact's.
    let target = match goal {
        Goal::Decision(decision) => match reader.target(context, stated, decision)? {
            Some(target) => Some(target),
            None => return Ok(Entailed::Refused(Refusal::default())),
        },
        Goal::Absurd => None,
    };
    let mut facts = reader.collect(context, target.as_ref())?;
    // What a refutation sums beside the facts: the other order of a `<` or a `<=`, the equation's two bounds of a `!=`, and of an `==` nothing, its disequality being the first split.
    let negated = match &target {
        Some(target) => negated(context, &mut views, &mut classes, target, &mut facts)?,
        None => Vec::new(),
    };
    let absurd = target.is_none();

    let mut budget = DERIVED_ROWS;
    let searched = match search(&facts, &negated, &mut budget) {
        Plan::Certified(certificate) => Ok(certificate),
        Plan::Exhausted => Err(SearchOutcome::Exhausted(DERIVED_ROWS)),
        // Where linear arithmetic finds an assignment, the facts' products are asked, under what is left of the cap. The linear assignment is the one reported: a product search reads a product as an unknown, so its own assignment may be no values of the atoms at all.
        Plan::Satisfied(assignment) => {
            let linear = SearchOutcome::Counterexample(assignment);
            let products = products(context, &mut views, &mut classes, &facts, &negated)?;
            let multiplied = !products.is_empty();
            let with_products = match multiplied {
                true => {
                    facts.facts.extend(products);
                    Some(search(&facts, &negated, &mut budget))
                }
                false => None,
            };
            match with_products {
                Some(Plan::Certified(certificate)) => Ok(certificate),
                Some(Plan::Exhausted) => Err(SearchOutcome::Exhausted(DERIVED_ROWS)),
                // Last, what the scope's disequalities add: read only where nothing above settles the search, so a bound the facts prove is proved as it was before a disequality was read at all.
                Some(Plan::Satisfied(_)) | None => {
                    let target = target.as_ref();
                    match disequalities(context, &mut views, &mut classes, &mut facts, target)? {
                        true => match search(&facts, &negated, &mut budget) {
                            Plan::Certified(certificate) => Ok(certificate),
                            Plan::Exhausted => Err(SearchOutcome::Exhausted(DERIVED_ROWS)),
                            Plan::Satisfied(assignment) if !multiplied => {
                                Err(SearchOutcome::Counterexample(assignment))
                            }
                            // A product is an unknown of its own to the search, so the atoms' values are those of a search over no product — and not the linear search's, which read no disequality and may give one the lie.
                            Plan::Satisfied(_) => Err(SearchOutcome::Counterexample(valued(
                                &facts,
                                &negated,
                                &mut budget,
                            ))),
                        },
                        false => Err(linear),
                    }
                }
            }
        }
    };
    // What the cap is set against: the rows one bound's search derived, products and cases included.
    curios_profile::sample!("entailment::derived", DERIVED_ROWS - budget);
    let refused = |views: &LinearViews, outcome: SearchOutcome| -> Result<Entailed, Error> {
        Ok(Entailed::Refused(Refusal::of(
            &facts, views, outcome, absurd,
        )))
    };
    let certificate = match searched {
        Ok(certificate) => certificate,
        Err(outcome) => return refused(&views, outcome),
    };
    let written = write(context, &facts, target.as_ref(), &negated, &certificate);
    let candidate = match written {
        // In one spelling: the operands a proof names come from the bound and the hypotheses as stated, where an operator's witness may still stand as the metavariable its resolution solved, and from the reducts the facts were read through, where it is the witness. A fold pairs by identity — Euclid's recombination pairs a quotient with a remainder of one dividend — so a proof naming one call both ways would not check.
        Written::Proof(candidate) => named_members(
            context,
            &zonk_solved_term_metas(context, &facts.respell(&candidate)),
        ),
        Written::Unwritten => return refused(&views, SearchOutcome::Unwritten),
    };
    // What a written proof costs its checkers grows with its size: the nodes of each proof, as written and before either checks it.
    curios_profile::sample!("entailment::written", {
        let mut nodes = 0usize;
        candidate.walk(
            &mut nodes,
            |nodes, _| {
                *nodes += 1;
                curios_core::Enter::Descend
            },
            |_, _, _| (),
        );
        nodes
    });
    match check(context, &candidate, bound)? {
        Some(proof) => Ok(Entailed::Proved(proof)),
        // A certificate whose proof does not check is the procedure's mistake, surfaced as the refusal it has to be and named as what it is.
        None => refused(&views, SearchOutcome::Rejected),
    }
}

/// Search `facts`, their splits and the negated goal's, spending from `budget`.
fn search(facts: &Facts, negated: &[Fact], budget: &mut usize) -> Plan {
    plan(
        &forms_of(&facts.facts),
        &forms_of(negated),
        &arms_of(facts),
        budget,
    )
}

/// The values a search over `facts` less their products leaves the atoms at, spending from `budget`: what a refusal reports where the search over the products left an assignment, a product being an unknown of its own there. Nothing where the cap runs out first.
fn valued(facts: &Facts, negated: &[Fact], budget: &mut usize) -> Vec<(Monomial, Rational)> {
    let linear = facts
        .facts
        .iter()
        .filter(|fact| !matches!(fact.origin, Origin::Product(..)))
        .map(|fact| fact.form.clone())
        .collect::<Vec<_>>();
    match plan(&linear, &forms_of(negated), &arms_of(facts), budget) {
        Plan::Satisfied(assignment) => assignment,
        Plan::Certified(_) | Plan::Exhausted => Vec::new(),
    }
}

/// The forms of `facts`, in their order.
fn forms_of(facts: &[Fact]) -> Vec<LinearForm> {
    facts.iter().map(|fact| fact.form.clone()).collect()
}

/// The case splits of `facts`, as the forms each case adds.
fn arms_of(facts: &Facts) -> Vec<Arms> {
    facts
        .splits
        .iter()
        .map(|split| Arms {
            holds: forms_of(&split.holds),
            fails: forms_of(&split.fails),
        })
        .collect()
}

/// `Bool/holds_of_eq(decision, Eq/refl())`: the decision holds because it is `true`, which conversion's probe-side decisions settle where reduction does not — `b || Bool/not(b)`. `None` where the two names are not in scope.
fn tautology(context: &Context, decision: &Term) -> Option<Term> {
    let entailment = context.syntax().entailment;
    if !in_scope(context, &[entailment.holds_of_eq, entailment.refl]) {
        return None;
    }
    let refl = Term::apply(global(entailment.refl), Vec::<Term>::new());
    Some(Term::apply(
        global(entailment.holds_of_eq),
        [decision.clone(), refl],
    ))
}

/// Elaborate `candidate` against `bound` as written code is, under the refinements live now, keeping what it solved when it checks and undoing it when it does not.
///
/// Inside the oracle package: parking is suppressed, so a candidate that would wait on a metavariable is a mismatch rather than a leak, and representation privacy is not re-adjudicated for the operands a candidate carries over from the bound and the facts, which elaboration already judged. The fields a fact projects are read only where the item may open them ([`Reader`]).
fn check(context: &mut Context, candidate: &Term, bound: &Term) -> Result<Option<Term>, Error> {
    curios_profile::profile!("entailment::check");
    let mark = context.solution_mark();
    let result =
        context.with_oracle(|context| elaborate(context, candidate, Mode::Check(bound.clone())));
    let verdict = result
        .probed()
        .map(|checked| checked.map(|(proof, _)| proof));
    if !matches!(verdict, Ok(Some(_))) {
        context.rollback_solutions(mark);
    }
    context.end_solutions(mark);
    verdict
}

/// Whether a proof form may apply every name in `names` ([`Context::proof_may_apply`]): a proof form writes only names in scope, which is how a declaration of `/std` the vocabulary reads behaves as it would with no procedure.
fn in_scope(context: &Context, names: &[SyntaxName]) -> bool {
    names
        .iter()
        .all(|name| context.proof_may_apply(&Global::Authored(name.qualifier())))
}

/// A reference to the global `name` spells.
fn global(name: SyntaxName) -> Term {
    Term::var(Var::free(Free::global(name.qualifier())))
}
