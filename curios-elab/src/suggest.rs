//! Candidate suggestions for unsolved written goals — the `? ≈` lines of a goal report.
//!
//! *Local fits* only, computed by machinery elaboration already runs: a scope binder whose type converts to the goal type (the sandboxed `probe_match` witness resolution uses), and a constructor the goal's indices admit (the shared `invert_indices` unifier match elaboration runs for omitted arms, here for the opposite verdict). Suggestions are observation-only text the compiler re-checks when the author pastes them, so a wrong candidate costs nothing and checking semantics are untouched.
//!
//! Every attempt runs inside the `solution_mark`/`rollback_solutions` bracket, and a hit is materialized (committed solutions spliced) *before* rollback — the pinned values the display shows would otherwise die with the transaction. Any error skips its candidate: the pass is infallible.
//!
//! A constructor candidate spells only its explicit payload — implicits re-infer when the author pastes it, exactly as they would when writing it by hand — with a shared `?`-named hole standing in for each explicit slot inversion left unsolved. Representation privacy is deliberately not consulted: a candidate for a sealed type outside its module simply fails the author's re-check.

use {
    super::{Context, Probe, check, probe_match, reduce_with, zonk_solved_term_metas},
    curios_base::Plicity,
    curios_cert::{Invert, case_target_indices, invert_indices},
    curios_core::{Free, Global, InductType, StructType, Subterm, Term, Var},
    std::collections::BTreeMap,
};

/// Whether the fully-spelled `candidate` checks against `goal_type` in the goal's own scope — the definitive fit gate for a hole-free candidate, and what turns the paste-and-recheck promise into a machine guarantee. Runs as an oracle (parking, refinements, and privacy suppressed — `Blocked` is a mismatch) inside a transaction: every solution the attempt lands is rolled back.
fn verifies(
    context: &mut Context,
    telescope: &[(Free, Term)],
    candidate: &Term,
    goal_type: &Term,
) -> bool {
    let mark = context.solution_mark();
    let verdict = context.with_oracle(|context| {
        context.with_frame(|context| {
            for (name, type_) in telescope {
                context.assume(name, type_);
            }
            check(context, candidate, goal_type.clone()).is_ok()
        })
    });
    context.rollback_solutions(mark);
    verdict
}

/// The rendered candidate cap per goal.
const CANDIDATES: usize = 3;

/// Suggest local fits for one unsolved goal, from its birth record: the frozen `telescope` and the expected `goal_type`. Deterministic — telescope order, then constructor declaration order — and never failing; an attempt that errors contributes nothing.
pub(crate) fn suggest_local_fits(
    context: &mut Context,
    telescope: &[(Free, Term)],
    goal_type: &Term,
) -> Vec<Term> {
    let mut candidates = Vec::new();
    // One shared hole identity per goal, so every unsolved slot spells the same bare `?`.
    let hole_name = context.fresh(Some("?"));
    let hole = Term::free_var(&hole_name);

    // Scope fits: a binder whose type converts to the goal type is itself a candidate.
    for (name, type_) in telescope {
        if candidates.len() == CANDIDATES {
            return candidates;
        }
        if matches!(probe_match(context, type_, goal_type), Ok(Probe::Yes)) {
            candidates.push(Term::free_var(name));
        }
    }

    let Ok(reduced) = reduce_with(context, goal_type) else {
        return candidates;
    };
    match &*reduced {
        // Constructor fits: keep each constructor the goal's indices admit, with inversion's forced values filled in.
        Subterm::InductType(InductType {
            name,
            params,
            indices,
            ..
        }) => {
            let Some(induct_decl) = context.induct_decl(name) else {
                return candidates;
            };
            let tags: Vec<_> = induct_decl
                .constructors
                .iter()
                .map(|(tag, param)| (tag.clone(), param.plicities.clone()))
                .collect();

            // A constructor candidate is spelled as its value-constructor *function* call — the pasteable form. The family is always an authored global; witnesses declare no inductives.
            let Global::Authored(family) = name else {
                return candidates;
            };

            for (tag, plicities) in tags {
                if candidates.len() == CANDIDATES {
                    break;
                }
                let Some(ctor_telescope) = context
                    .induct_decl(name)
                    .and_then(|decl| decl.instantiate(&tag, params))
                else {
                    continue;
                };
                let labels: Vec<Free> = (0..ctor_telescope.len())
                    .map(|_| context.fresh(None))
                    .collect();
                let vars: Vec<Term> = labels.iter().map(Term::free_var).collect();
                let targets = case_target_indices(ctor_telescope, &vars);

                let mark = context.solution_mark();
                let candidate = match invert_indices(context, indices, &targets, &labels) {
                    Ok(Invert::Solved(solutions)) => {
                        let solutions: BTreeMap<Free, Term> = solutions.into_iter().collect();
                        // Explicit slots only: a pasted candidate re-infers its implicits, exactly as a written one would. A slot inversion left unsolved becomes a hole.
                        let mut holes = 0usize;
                        let payload: Vec<Term> = labels
                            .iter()
                            .zip(&plicities)
                            .filter(|(_, plicity)| **plicity == Plicity::Explicit)
                            .map(|(label, _)| {
                                solutions.get(label).cloned().unwrap_or_else(|| {
                                    holes += 1;
                                    hole.clone()
                                })
                            })
                            .collect();
                        let constructor = Term::var(Var::free(Free::Global(Global::Authored(
                            family.with(&tag.as_string()),
                        ))));
                        let built = Term::apply(constructor, payload);
                        // Inversion *refuses* positions it cannot decide (metavariable-headed or opaque indices), so `Solved` alone is not a fit. A fully-spelled candidate must survive the definitive gate — a sandboxed check against the goal — which is also what makes the paste-and-recheck promise a machine guarantee. A candidate with visible holes is an advisory refinement and rides on `Impossible` filtering alone.
                        if holes > 0 || verifies(context, telescope, &built, goal_type) {
                            Some(zonk_solved_term_metas(context, &built))
                        } else {
                            None
                        }
                    }
                    Ok(Invert::Impossible) | Err(_) => None,
                };
                context.rollback_solutions(mark);

                if let Some(candidate) = candidate {
                    candidates.push(candidate);
                }
            }
        }
        // A struct-typed goal suggests its literal shape, one hole per field.
        Subterm::StructType(StructType { name, params, .. }) => {
            if candidates.len() < CANDIDATES
                && let Some(struct_decl) = context.struct_decl(name)
            {
                let field_count = struct_decl.fields_at(params).len();
                let fields: Vec<Term> = (0..field_count).map(|_| hole.clone()).collect();
                candidates.push(Term::struct_(name.clone(), params.iter().cloned(), fields));
            }
        }
        _ => {}
    }

    candidates
}
