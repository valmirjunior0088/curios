//! The procedure that proves a bound from the facts in scope, where the fill, `trivially_inhabited`, has no answer.
//!
//! **It writes a proof and elaborates it as written code is.** Both checkers recheck what it wrote, so a wrong proof is a term that does not check and the procedure's mistakes are refusals: nothing it adds is trusted. It changes no conversion rule, no refinement and no solving choice, and assigns only the hole it was asked about, with a term whose type is the hole's.
//!
//! **It runs only where the elaborator would otherwise report the bound, or the case a match leaves to no arm**: at a leaf no arm reaches, with its case's refinements live, as the matrix compiler enters it; at insertion, with the arm's refinements live, and at a parked bound's retry once the bound waits on nothing, under the refinements its slot was born under — which is what re-validation judges a solution by, so a guard is a fact for a bound born in its arm and not for one born outside it. A bound waiting only on the slot a `rec` group's member is known by while the group is checked is asked at both, and stays parked if refused: the slot's solution is the body the bound sits in, and a proof from the facts is written over the member's name rather than its slot. It does not run after an item closes, which would lose the guards, since a guard here is a refinement and not a binder.
//!
//! **What it proves.**
//!
//! - A proposition a fact in scope states outright — a hypothesis, a proof field one level down, or a conjunct of a conjunction either of them or a live guard's true arm states, its heads opened, whose statement converts with the bound with nothing solved: [`stated_by_a_fact`]. The fact is the proof, whatever the proposition is: a decision the linear view reads nothing in, or an inductive proposition the procedure never opens, such as an equation.
//! - A `Nat` or `Int` comparison that follows from the facts in scope ([`Reader`]) by linear arithmetic over the rationals, a strict integer fact strengthened to its successor, within the search's cap ([`refute`]): a `<` or a `<=` as it stands, a disequality through the equation its negation is, and an equality through the two cases its negation makes ([`negated`]), `Bool/not` of each read as the comparison true exactly where it fails, in a fact, a guard or the goal alike. A consequence that holds only over the integers and needs a cut is refused, as `linarith` and `omega` without its dark and grey shadows refuse it.
//! - Over `Nat`, through the operations whose definitions are linear facts: a quotient or remainder through the quotient's bounds, at `Int` too, a truncated subtraction through the case split `omega` makes, a `min` or a `max` through the split on its operands' order and an `Int`'s `abs` or `sign` through the splits on its operand's sign, each opened only where the search needs it ([`plan`]) — a subtraction, a `min` and a `max` being asked first through what both of their cases state, so a sum of them that turns on no case opens none — and a power of a base conversion holds at least one, as the `pow(2, m)` a shift by a symbolic count multiplies by is, through its being at least one.
//! - Where linear arithmetic finds an assignment, through the products of pairs of facts and the negated goal, as `nlinarith` does ([`products`]): what a multiplier that is no literal needs, `Nat/div_mod`'s among them. A case a split opens multiplies its own facts by them and by one another ([`case_products`]), so the fact a case adds — each order the disequality an equality goal is refuted through makes — enters a product as an order's negated goal does.
//! - Where the products leave one too, through the disequalities the scope states, each as the order comparison conversion holds it to be ([`disequalities`]): `total / (n + m)` under `Holds(n != 0)`. Asked last, so a bound the facts prove is proved as it was before a disequality was read at all.
//! - An empty proposition the facts refute: a bound at `False`, which is what `proved(@False)` asks for and what closes a leaf of a match no arm reaches.
//! - A conjunction of any of these, conjunct by conjunct, the proofs joined by `and/both` ([`conjoined`]).
//! - An equation at a type that decides its equality, through the decision: `Eq()(a, b)` where `a == b` has a witness is asked as `Holds(a == b)`, a bound of its own proved as any of these is, and answered by `Eql`'s law over that proof ([`equated`]). A type with no witness, as `Flt`, states no decision, and its equation is read as any proposition the procedure reads nothing in.
//! - A disjunction of comparisons, refuted whole: every side but the last read as failing and the last the goal, the proof split on each side in turn ([`disjoined`]).
//!
//! **What failure is.** The bound's refusal, as the fill leaves it, naming the facts the procedure considered, those it could not read, and — where the search produced one — an assignment of the atoms that satisfies the facts and falsifies the goal ([`Refusal`]).
//!
//! **What it is not.** The fill stays what its documentation says it is, a unique answer and no search. This procedure is a separate, fallible step after it.

mod facts;
pub use facts::Origin;
use facts::*;

mod proof;
use proof::*;

mod report;
use report::*;
pub use report::{Answer, Conclusion, Refusal};

mod search;
use search::*;

#[cfg(test)]
mod tests;

use {
    crate::{
        Context, Error, Expectation, Mode, elaborate, named_members, reduce_with, same_uncommitted,
        trivially_inhabited, zonk_solved_term_metas,
    },
    curios_algebra::{LinearForm, Monomial},
    curios_analysis::Classes,
    curios_common::{InfixOp, Plicity, SyntaxName},
    curios_core::{Cases, Global, LinearViews, Match, Probe, Subterm, Term},
};

/// What the procedure concluded about a bound.
pub enum Entailed {
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
    /// `Holds(e)` of a disjunction `e`: refuted whole, its last side the goal and every other side's failure a fact.
    Disjunction(Disjunction),
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
pub fn entail(context: &mut Context, bound: &Term, reduced: &Term) -> Result<Entailed, Error> {
    curios_common::profile!("entailment::entail");
    if context.entailing() {
        return Ok(Entailed::Refused(Refusal::default()));
    }
    // What the search reads in the bound, where it reads anything: a fact that states the bound proves it either way.
    let goal = goal_of(context, reduced)?;
    // The decision as the bound spells it, which the proof is written over where the bound's reduct is only read.
    let stated = held(context, bound)?;
    // A conjunction is proved conjunct by conjunct, each by this procedure, and the proofs joined as a reader would join them.
    if let Some(Goal::Decision(decision)) = &goal
        && let Some((first, second)) = conjunction(context, stated.as_ref().unwrap_or(decision))?
    {
        return conjoined(context, bound, reduced, &first, &second);
    }
    // An equation at a type that decides its equality is proved as the decision is, and crossed by the concept's law.
    if goal.is_none()
        && let Some(entailed) = equated(context, bound, reduced)?
    {
        return Ok(entailed);
    }
    // A disjunction is refuted whole, as `omega` refutes a goal's negation: the sides' failures are read together and the proof splits on each side in turn.
    let goal = match goal {
        Some(Goal::Decision(decision)) => {
            match disjunction(context, stated.as_ref().unwrap_or(&decision))? {
                Some(tree) => Some(Goal::Disjunction(tree)),
                None => Some(Goal::Decision(decision)),
            }
        }
        goal => goal,
    };

    let entailed = context.with_entailing(|context| {
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
            // A fact and an atom are written as the statement they were read from states them, where a metavariable elaboration has since solved may still stand — an operator's witness in a local definition's annotation among them: substituted, as it is in the bound the report names above it.
            let mut refusal = refusal;
            for term in refusal.terms_mut() {
                *term = zonk_solved_term_metas(context, term);
            }
            Entailed::Refused(refusal)
        }
        entailed => entailed,
    })
}

/// The facts in scope a report names beside a bound that holds, each with where it came from: what the procedure reads of the hypotheses, their proof fields and the live guards, as a refusal names the facts it considered ([`considered`]).
pub fn facts_in_scope(context: &mut Context) -> Result<Vec<(Origin, Term)>, Error> {
    curios_common::profile!("entailment::facts_in_scope");
    let facts = context.with_entailing(|context| {
        let mut views = LinearViews::default();
        let mut classes = Classes::default();
        Reader::new(&mut views, &mut classes).collect(context, None)
    })?;
    Ok(considered(&facts)
        .into_iter()
        .map(|(from, statement)| (from, zonk_solved_term_metas(context, &statement)))
        .collect())
}

/// The fact that states `bound` outright, as its proof: the first hypothesis, proof field one level down, or conjunct of a conjunction either states, in scope order, and then the first conjunct a live guard's true arm decides ([`conjuncts`], [`guard_conjuncts`]), whose statement converts with the bound and checks against it.
///
/// **Conversion alone, and nothing solved.** A statement is put to the bound only where the two convert with no metavariable assigned and no universe constrained, so the step makes no solving choice: a bound still waiting on its subject is stated by no fact yet, and is asked again once the subject is known. Congruence and a fact's consequences are no part of it — within its fragment they are the linear half's, and past it the author's — but a conjunct is no consequence: a conjunction its heads open to states each conjunct as its own statement, at any type, as the linear half reads it for its comparisons.
///
/// **Any proposition.** The bound need be nothing the search reads: a proposition is proof-irrelevant, so the fact that states it is as good a proof as any, and which fact is taken decides nothing.
fn stated_by_a_fact(context: &mut Context, bound: &Term) -> Result<Option<Term>, Error> {
    curios_common::profile!("entailment::stated_by_a_fact");
    for (name, type_) in binders(context) {
        for statement in statements(context, &name, &type_)? {
            if let Some(proof) = stating(context, &statement, bound)? {
                return Ok(Some(proof));
            }
            for conjunct in conjuncts(context, &statement)? {
                if let Some(proof) = stating(context, &conjunct, bound)? {
                    return Ok(Some(proof));
                }
            }
        }
    }
    for conjunct in guard_conjuncts(context)? {
        if let Some(proof) = stating(context, &conjunct, bound)? {
            return Ok(Some(proof));
        }
    }
    Ok(None)
}

/// `statement`'s proof, checked against `bound`, where what it states converts with the bound with nothing solved.
fn stating(
    context: &mut Context,
    statement: &Statement,
    bound: &Term,
) -> Result<Option<Term>, Error> {
    let states = same_uncommitted(context, &statement.stated, bound).map_err(|error| {
        Error::from_reduce(error, |refusal| {
            Error::reduce_exhausted(bound.clone(), refusal)
        })
    })?;
    match states {
        true => check(context, &statement.proof, bound),
        false => Ok(None),
    }
}

/// A bound that is a conjunction, proved by a fact that states it whole or by a proof of each conjunct joined with `and/both` — the proof a reader writes, `Bool/and/both(a, b, proved(), proved())`. Each conjunct is a bound of its own, stated as `bound` states its decision where that opens to `Holds`, and otherwise as `reduced` is, the match `Holds` reduces to, over the conjunct. A conjunct not proved refuses the whole with its own refusal, which names it; with `and/both` out of scope the bound is refused as it would be with no procedure.
fn conjoined(
    context: &mut Context,
    bound: &Term,
    reduced: &Term,
    first: &Term,
    second: &Term,
) -> Result<Entailed, Error> {
    let both = context.syntax().conjunction.both;
    let proof = match context.with_entailing(|context| stated_by_a_fact(context, bound))? {
        Some(proof) => proof,
        None => {
            if !in_scope(context, &[both]) {
                return Ok(Entailed::Refused(Refusal::default()));
            }
            let mut proofs = Vec::with_capacity(2);
            for conjunct in [first, second] {
                let stated = match holding(context, bound, conjunct)? {
                    Some(stated) => stated,
                    None => over(reduced, conjunct),
                };
                // A conjunct is a bound of its own, asked of the fill first as every bound is: conversion may decide it, as it decides `9216 <= 9216 + c`, where the conjunction it stands in was decided by neither.
                let (reduct, inhabitant) = trivially_inhabited(context, &stated)?;
                if let Some(inhabitant) = inhabitant {
                    proofs.push(inhabitant);
                    continue;
                }
                match entail(context, &stated, &reduct)? {
                    Entailed::Proved(proof) => proofs.push(proof),
                    Entailed::Refused(refusal) => {
                        let conjunct = zonk_solved_term_metas(context, conjunct);
                        return Ok(Entailed::Refused(refusal.of_conjunct(&conjunct)));
                    }
                }
            }
            let joined = [first.clone(), second.clone()].into_iter().chain(proofs);
            match check(context, &Term::apply(global(both), joined), bound)? {
                Some(proof) => proof,
                None => return Ok(Entailed::Refused(Refusal::rejected())),
            }
        }
    };
    context.credit(&proof);
    Ok(Entailed::Proved(proof))
}

/// A bound that is an equation at a type that decides its equality, proved by a fact that states it or through the decision — the proof a reader writes, `Eql/eq_of_eql(a, b, proved())`: `Holds(a == b)` is a bound of its own, asked of the fill and then of this procedure, and `Eql`'s law takes its proof to the equation. `None` where `reduced` is no equation, where the law is not in scope, and where `==` has no witness at the type, as at `Flt`: the equation is then read as any proposition the procedure reads nothing in.
fn equated(context: &mut Context, bound: &Term, reduced: &Term) -> Result<Option<Entailed>, Error> {
    let Subterm::InductType(equation) = &**reduced else {
        return Ok(None);
    };
    let equality = Global::Authored(context.syntax().equality.family.qualifier());
    let ([carrier], [left, right], true) = (
        equation.params.as_slice(),
        equation.indices.as_slice(),
        equation.name == equality,
    ) else {
        return Ok(None);
    };
    let law = context.syntax().eql.eq_of_eql;
    if !in_scope(context, &[law.concept]) {
        return Ok(None);
    }
    if let Some(proof) = context.with_entailing(|context| stated_by_a_fact(context, bound))? {
        context.credit(&proof);
        return Ok(Some(Entailed::Proved(proof)));
    }
    // The decision, written as a program writes it and elaborated as one, its witness the one the sides' type resolves.
    let holds = context.syntax().proof.holds;
    let decision = Term::infix(InfixOp::Eql, left.clone(), right.clone());
    let Some(decided) = stated(context, &Term::apply(global(holds), [decision]))? else {
        return Ok(None);
    };
    let (reduct, inhabitant) = trivially_inhabited(context, &decided)?;
    let proof = match inhabitant {
        Some(inhabitant) => inhabitant,
        None => match entail(context, &decided, &reduct)? {
            Entailed::Proved(proof) => proof,
            Entailed::Refused(refusal) => return Ok(Some(Entailed::Refused(refusal))),
        },
    };
    // The law's call names the type, so its witness slot is resolved where it is born: a candidate is checked where nothing waits, and a slot left to the first argument would.
    let crossed = Term::apply_marked(
        Term::registered_method(law),
        [
            (Plicity::Implicit, carrier.clone()),
            (Plicity::Explicit, left.clone()),
            (Plicity::Explicit, right.clone()),
            (Plicity::Explicit, proof),
        ],
    );
    Ok(Some(match check(context, &crossed, bound)? {
        Some(proof) => {
            context.credit(&proof);
            Entailed::Proved(proof)
        }
        None => Entailed::Refused(Refusal::rejected()),
    }))
}

/// `written`, a proposition as a program writes it, elaborated as one under the refinements live now: `None` where it does not elaborate, an `==` at a type with no witness among them. What it solved is kept where it elaborates and undone where it does not, as [`check`] keeps a candidate's.
fn stated(context: &mut Context, written: &Term) -> Result<Option<Term>, Error> {
    let mark = context.solution_mark();
    let result = context.with_oracle(|context| elaborate(context, written, Mode::Infer));
    let verdict = result
        .probed()
        .map(|elaborated| elaborated.map(|(statement, _)| statement));
    if !matches!(verdict, Ok(Some(_))) {
        context.rollback_solutions(mark);
    }
    context.end_solutions(mark);
    verdict
}

/// `reduced`, the match `Holds` reduces to, over `decision` in place of the one it decided.
fn over(reduced: &Term, decision: &Term) -> Term {
    match &**reduced {
        Subterm::Match(matched) => Term::from(Subterm::Match(Match {
            head: decision.clone(),
            ..matched.clone()
        })),
        _ => reduced.clone(),
    }
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
        // Its last side is the goal and every other side's failure a fact, each read as a guard's false arm is; a side that is no comparison of the fragment leaves the disjunction no goal of it.
        Goal::Disjunction(tree) => {
            let entailment = context.syntax().entailment;
            if !in_scope(context, &[entailment.or_first, entailment.or_second]) {
                return Ok(Entailed::Refused(Refusal::default()));
            }
            let sides = tree.sides();
            let Some((&last, others)) = sides.split_last() else {
                return Ok(Entailed::Refused(Refusal::default()));
            };
            let Some(target) = reader.target(context, Some(last), last)? else {
                return Ok(Entailed::Refused(Refusal::default()));
            };
            for side in others {
                if !reader.failing(context, side)? {
                    return Ok(Entailed::Refused(Refusal::default()));
                }
            }
            Some(target)
        }
    };
    let mut facts = reader.collect(context, target.as_ref())?;
    // What a refutation sums beside the facts: the other order of a `<` or a `<=`, the equation's two bounds of a `!=`, and of an `==` nothing, its disequality being the first split.
    let negated = match &target {
        Some(target) => negated(context, &mut views, &mut classes, target, &mut facts)?,
        None => Vec::new(),
    };
    let absurd = target.is_none();

    // Each stage searches under a cap of its own: the facts and their splits, their products, the disequalities. A stage asked where the one before it left an assignment says nothing of the bound by running out, so one that does is passed over, and the next reads what the stage before it read.
    let mut derived = 0;
    let searched = match staged(&facts, &negated, &mut derived) {
        Plan::Certified(certificate) => Ok(certificate),
        Plan::Exhausted => Err(SearchOutcome::Exhausted(DERIVED_ROWS)),
        // Where linear arithmetic finds an assignment, the facts' products are asked. The linear assignment is the one reported: a product search reads a product as an unknown, so its own assignment may be no values of the atoms at all.
        Plan::Satisfied(assignment) => {
            let linear = SearchOutcome::Counterexample(assignment);
            // The facts as the linear search read them, which the disequalities are read over where the products run out.
            let unmultiplied = facts.clone();
            let products = products(context, &mut views, &mut classes, &facts, &negated)?;
            let cased = case_products(context, &mut views, &mut classes, &mut facts, &negated)?;
            let with_products = match !products.is_empty() || cased {
                true => {
                    facts.extend(products);
                    Some(staged(&facts, &negated, &mut derived))
                }
                false => None,
            };
            let multiplied = match with_products {
                Some(Plan::Exhausted) => {
                    facts = unmultiplied;
                    false
                }
                Some(Plan::Certified(_) | Plan::Satisfied(_)) => true,
                None => false,
            };
            match with_products {
                Some(Plan::Certified(certificate)) => Ok(certificate),
                // Last, what the scope's disequalities add: read only where nothing above settles the search, so a bound the facts prove is proved as it was before a disequality was read at all.
                Some(Plan::Satisfied(_) | Plan::Exhausted) | None => {
                    let target = target.as_ref();
                    match disequalities(context, &mut views, &mut classes, &mut facts, target)? {
                        true => match staged(&facts, &negated, &mut derived) {
                            Plan::Certified(certificate) => Ok(certificate),
                            Plan::Exhausted => Err(SearchOutcome::Exhausted(DERIVED_ROWS)),
                            Plan::Satisfied(assignment) if !multiplied => {
                                Err(SearchOutcome::Counterexample(assignment))
                            }
                            // A product is an unknown of its own to the search, so the atoms' values are those of a search over no product — and not the linear search's, which read no disequality and may give one the lie.
                            Plan::Satisfied(_) => Err(SearchOutcome::Counterexample(valued(
                                &facts,
                                &negated,
                                &mut derived,
                            ))),
                        },
                        false => Err(linear),
                    }
                }
            }
        }
    };
    // What the cap is set against: the rows one bound's stages derived, products and cases included.
    curios_common::sample!("entailment::derived", derived);
    // A counterexample's atoms named in one pass through every spelling the report has, so an outer reduct is replaced before one inside it whichever reading found either: the operands as stated, and the operands written beneath them, which are asked only now the search is over. A bound that is a metavariable elaboration has solved, as the statement `proved()` leaves implicit is, opens to no `Holds`: the search read its reduct, and what the solution states is read here, for the report alone, since the proof is written over what the search read and one written over the solution does not check. A reading that runs out leaves its spellings out.
    let refused = |context: &mut Context, views: &LinearViews, outcome: SearchOutcome| {
        let refuted = refuted(context, views, &outcome);
        // The decision a guard would write, as the bound states it: a statement elaboration solved, as `proved()` leaves one implicit, by what its solution states, so a guard is written as the claim is and never as its reduct, which unfolds what the claim names.
        let decided = match goal {
            Goal::Decision(decision) => Some(match stated {
                Some(stated) => stated.clone(),
                None => held(context, &zonk_solved_term_metas(context, bound))
                    .ok()
                    .flatten()
                    .unwrap_or_else(|| decision.clone()),
            }),
            Goal::Absurd | Goal::Disjunction(_) => None,
        };
        let refusal = Refusal::of(&facts, views, outcome, absurd, refuted).of_goal(decided);
        let inferred = match stated {
            Some(_) => Vec::new(),
            None => {
                let solved = zonk_solved_term_metas(context, bound);
                stated_spellings(context, &solved).unwrap_or_default()
            }
        };
        let spellings = facts.report_spellings(context, inferred);
        Ok::<_, Error>(Entailed::Refused(refusal.respelled(&spellings)))
    };
    let certificate = match searched {
        Ok(certificate) => certificate,
        Err(outcome) => return refused(context, &views, outcome),
    };
    let written = match (
        goal,
        write(context, &facts, target.as_ref(), &negated, &certificate),
    ) {
        (Goal::Disjunction(tree), Written::Proof(last)) => {
            Written::Proof(disjoined(context, tree, last))
        }
        (_, written) => written,
    };
    let candidate = match written {
        // In one spelling: the operands a proof names come from the bound and the hypotheses as stated, where an operator's witness may still stand as the metavariable its resolution solved, and from the reducts the facts were read through, where it is the witness. A fold pairs by identity — Euclid's recombination pairs a quotient with a remainder of one dividend — so a proof naming one call both ways would not check.
        Written::Proof(candidate) => named_members(
            context,
            &zonk_solved_term_metas(context, &facts.respell(&candidate)),
        ),
        Written::Unwritten => return refused(context, &views, SearchOutcome::Unwritten),
    };
    // What a written proof costs its checkers grows with its size: the nodes of each proof, as written and before either checks it.
    curios_common::sample!("entailment::written", {
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
        None => refused(context, &views, SearchOutcome::Rejected),
    }
}

/// One stage of a bound's search: `facts`, their splits and the negated goal's, under a cap of its own, the rows it derived added to `derived`.
fn staged(facts: &Facts, negated: &[Fact], derived: &mut usize) -> Plan {
    let mut budget = DERIVED_ROWS;
    let plan = search(facts, negated, &mut budget);
    *derived += DERIVED_ROWS - budget;
    plan
}

/// Search `facts`, their splits and the negated goal's, spending from `budget`.
fn search(facts: &Facts, negated: &[Fact], budget: &mut usize) -> Plan {
    plan(
        &forms_of(facts.facts()),
        &forms_of(negated),
        &forms_of(facts.common()),
        &arms_of(facts),
        budget,
    )
}

/// The values a search over `facts` less their products leaves the atoms at, under a cap of its own, the rows it derived added to `derived`: what a refusal reports where the search over the products left an assignment, a product being an unknown of its own there. Nothing where the cap runs out first.
fn valued(facts: &Facts, negated: &[Fact], derived: &mut usize) -> Vec<(Monomial, Rational)> {
    let linear = facts
        .facts()
        .iter()
        .filter(|fact| !matches!(fact.origin, Origin::Product(..)))
        .map(|fact| fact.form.clone())
        .collect::<Vec<_>>();
    let common = forms_of(facts.common());
    let mut budget = DERIVED_ROWS;
    let searched = plan(
        &linear,
        &forms_of(negated),
        &common,
        &arms_of(facts),
        &mut budget,
    );
    *derived += DERIVED_ROWS - budget;
    match searched {
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
        .splits()
        .iter()
        .map(|split| Arms {
            holds: forms_of(split.holds()),
            fails: forms_of(split.fails()),
        })
        .collect()
}

/// Elaborate `candidate` against `bound` as written code is, under the refinements live now, keeping what it solved when it checks and undoing it when it does not.
///
/// Inside the oracle package: parking is suppressed, so a candidate that would wait on a metavariable is a mismatch rather than a leak, and representation privacy is not re-adjudicated for the operands a candidate carries over from the bound and the facts, which elaboration already judged. The fields a fact projects are read only where the item may open them ([`Reader`]).
fn check(context: &mut Context, candidate: &Term, bound: &Term) -> Result<Option<Term>, Error> {
    curios_common::profile!("entailment::check");
    let mark = context.solution_mark();
    let result = context.with_oracle(|context| {
        elaborate(
            context,
            candidate,
            Mode::Check(Expectation::own(bound.clone())),
        )
    });
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
    Term::written(Global::Authored(name.qualifier()))
}
