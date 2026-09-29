//! The procedure that proves a bound from the facts in scope, where the fill ([`trivially_inhabited`](crate::elaborate)) has no answer.
//!
//! **It writes a proof and elaborates it as written code is.** Both checkers recheck what it wrote, so a wrong proof is a term that does not check and the procedure's mistakes are refusals: nothing it adds is trusted. It changes no conversion rule, no refinement and no solving choice, and assigns only the hole it was asked about, with a term whose type is the hole's.
//!
//! **It runs only where the elaborator would otherwise report the bound**: at insertion, with the arm's refinements live, and at a parked bound's retry once the bound waits on nothing, under the refinements its slot was born under — which is what re-validation judges a solution by, so a guard is a fact for a bound born in its arm and not for one born outside it. It does not run after an item closes, which would lose the guards, since a guard here is a refinement and not a binder.
//!
//! **What it proves.** A decision conversion equates with `true` and reduction does not, `b || Bool/not(b)`: [`tautology`].
//!
//! **What it is not.** The fill stays what its documentation says it is, a unique answer and no search. This procedure is a separate, fallible step after it.

use {
    crate::{Context, Error, Mode, elaborate, reduce_with},
    curios_core::{Cases, Free, Global, Match, Subterm, Term, Var},
    curios_utilities::SyntaxName,
};

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

/// A proof of `bound`, whose reduct `reduced` the fill found no inhabitant in, from the facts in scope — under whatever refinements are live, which the caller has made the ones a solution for the bound's slot is judged under. `None` where the procedure proves nothing.
///
/// An exhausted budget propagates: running out is not evidence the bound is false, and collapsing it into a refusal would report the author's fault at the argument for what is the resource limit.
pub(crate) fn entail(
    context: &mut Context,
    bound: &Term,
    reduced: &Term,
) -> Result<Option<Term>, Error> {
    curios_profile::profile!("entailment::entail");
    if context.entailing() {
        return Ok(None);
    }
    let Some(decision) = decision_of(context, reduced)? else {
        return Ok(None);
    };

    context.with_entailing(|context| match tautology(context, &decision) {
        Some(candidate) => check(context, &candidate, bound),
        None => Ok(None),
    })
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
/// Inside the oracle package: parking is suppressed, so a candidate that would wait on a metavariable is a mismatch rather than a leak, and representation privacy is not re-adjudicated for the operands a candidate carries over from the bound, which elaboration already judged.
fn check(context: &mut Context, candidate: &Term, bound: &Term) -> Result<Option<Term>, Error> {
    curios_profile::profile!("entailment::check");
    let refinements = context.refinement_snapshot();
    let mark = context.solution_mark();
    let result = context.with_oracle(&refinements, |context| {
        elaborate(context, candidate, Mode::Check(bound.clone()))
    });
    let verdict = match result {
        Ok((proof, _)) => Ok(Some(proof)),
        Err(error) if error.is_exhausted() => Err(error),
        Err(_) => Ok(None),
    };
    if !matches!(verdict, Ok(Some(_))) {
        context.rollback_solutions(mark);
    }
    context.end_solutions(mark);
    verdict
}

/// Whether every name in `names` is assumed in the context: a proof form writes only names in scope, which is how an item of `/std` compiled before its vocabulary keeps the behavior it had without it. It is the lookup a reference itself makes.
fn in_scope(context: &Context, names: &[SyntaxName]) -> bool {
    names.iter().all(|name| {
        context
            .assumption(&Free::global(name.qualifier()))
            .is_some()
    })
}

/// A reference to the global `name` spells.
fn global(name: SyntaxName) -> Term {
    Term::var(Var::free(Free::global(name.qualifier())))
}
