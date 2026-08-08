//! Candidate suggestions for unsolved written goals — the `? ≈` lines of a goal report.
//!
//! Two families, both computed by machinery elaboration already runs. *Local fits*: a scope binder whose type converts to the goal type (the sandboxed `probe_match` witness resolution uses), and a constructor the goal's indices admit (the shared `invert_indices` unifier match elaboration runs for omitted arms, here for the opposite verdict). *Application fits*: a function from the goal's scope, the entry module's definitions, or the globals the module already references, whose instantiated output type converts to the goal — the witness-table instantiation generalized to an arbitrary candidate, so arguments the unification pins display filled (`mk(3)`). Suggestions are observation-only text the compiler re-checks when the author pastes them, so a wrong candidate costs nothing and checking semantics are untouched.
//!
//! Every attempt runs inside the `solution_mark`/`rollback_solutions` bracket, and a hit is materialized (committed solutions spliced) *before* rollback — the pinned values the display shows would otherwise die with the transaction. Any error skips its candidate: the pass is infallible. Attempts are capped at [`ATTEMPTS`] per goal and the rendered list at [`CANDIDATES`], ranked complete-first, then fewest holes, then pool order.
//!
//! A candidate spells only explicit arguments — hidden slots re-infer when the author pastes it, exactly as they would when writing it by hand — with a shared `?`-named hole standing in for each explicit slot the attempt left unsolved. Hole-free candidates must additionally survive [`verifies`], a sandboxed oracle check in the goal's own scope, which turns the paste-and-recheck promise into a machine guarantee. Representation privacy is deliberately not consulted: a candidate for a sealed type outside its module simply fails the author's re-check.

use {
    super::{
        Context, Outcome, Probe, check, convert_outcome, probe_match, reduce_with,
        zonk_solved_term_metas,
    },
    curios_base::Plicity,
    curios_cert::{Invert, case_target_indices, invert_indices},
    curios_core::{
        Free, FuncType, Global, InductType, Item, Module, StructType, Subterm, Telescope, Term, Var,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// The rendered candidate cap per goal.
const CANDIDATES: usize = 3;

/// The attempt cap per goal across the application-fit pools — a hard bound on error-path work, not a completeness promise.
const ATTEMPTS: usize = 128;

/// A found fit: the display term, its residual hole count, and its pool rank (scope values, constructors, scope functions, module definitions, referenced globals — in that order).
struct Candidate {
    term: Term,
    holes: usize,
    pool: usize,
}

/// Suggest candidates for one unsolved goal, from its birth record (the frozen `telescope` and expected `goal_type`), the module (the application-fit pools), and the goal's owning definition (excluded from those pools: suggesting the definition a goal sits inside would be circular for a plain `let`). Deterministic and never failing; an attempt that errors contributes nothing.
pub(crate) fn suggest_candidates(
    context: &mut Context,
    telescope: &[(Free, Term)],
    goal_type: &Term,
    module: &Module,
    owner: Option<&Global>,
) -> Vec<Term> {
    let mut candidates: Vec<Candidate> = Vec::new();
    // One shared hole identity per goal, so every unsolved slot spells the same bare `?`.
    let hole_name = context.fresh(Some("?"));
    let hole = Term::free_var(&hole_name);

    // Pool 0 — scope fits: a binder whose type converts to the goal type is itself a candidate. An unnameable binder is skipped: the paste-and-recheck promise needs a spelling the author can write.
    for (name, type_) in telescope {
        if !name.nameable() {
            continue;
        }
        if matches!(probe_match(context, type_, goal_type), Ok(Probe::Yes)) {
            candidates.push(Candidate {
                term: Term::free_var(name),
                holes: 0,
                pool: 0,
            });
        }
    }

    // Pool 1 — constructor and struct-literal fits on the reduced goal type.
    if let Ok(reduced) = reduce_with(context, goal_type) {
        constructor_fits(
            context,
            telescope,
            goal_type,
            &reduced,
            &hole,
            &mut candidates,
        );
    }

    // Pools 2–4 — application fits: scope binders with function types, then the module's own definitions, then the globals its items reference. Unnameable heads are skipped for the same reason as pool 0.
    let mut attempts = 0usize;
    for (name, type_) in telescope {
        if !name.nameable() {
            continue;
        }
        if attempts >= ATTEMPTS {
            break;
        }
        attempts += 1;
        if let Some((term, holes)) =
            apply_fit(context, &Term::free_var(name), type_, goal_type, &hole)
        {
            candidates.push(Candidate {
                term,
                holes,
                pool: 2,
            });
        }
    }
    for (pool, (name, type_)) in module_pool(context, module, owner) {
        if attempts >= ATTEMPTS {
            break;
        }
        attempts += 1;
        let head = Term::var(Var::free(Free::Global(name)));
        if let Some((term, holes)) = apply_fit(context, &head, &type_, goal_type, &hole) {
            candidates.push(Candidate { term, holes, pool });
        }
    }

    // Complete fits first, then fewest holes, then pool order; discovery order breaks the remaining ties (the sort is stable). Structural dedup follows: a referenced global can rediscover a constructor the local pass already found.
    candidates.sort_by_key(|candidate| (candidate.holes, candidate.pool));
    let mut seen: Vec<Term> = Vec::new();
    candidates
        .into_iter()
        .filter(|candidate| {
            if seen.contains(&candidate.term) {
                false
            } else {
                seen.push(candidate.term.clone());
                true
            }
        })
        .take(CANDIDATES)
        .map(|candidate| candidate.term)
        .collect()
}

/// Pools 3 and 4: the entry module's own definitions, then every other global its items reference, each with its recorded type. The goal's owning definition is excluded.
fn module_pool(
    context: &Context,
    module: &Module,
    owner: Option<&Global>,
) -> Vec<(usize, (Global, Term))> {
    let mut pool = Vec::new();
    let mut own = BTreeSet::new();

    let mut definitions = |definition: &curios_core::Definition| {
        own.insert(definition.name.clone());
        if Some(&definition.name) != owner {
            pool.push((3, (definition.name.clone(), definition.type_.clone())));
        }
    };
    for item in &module.items {
        match item {
            Item::Let(definition) => definitions(definition),
            Item::Rec(rec) => rec.definitions().iter().for_each(&mut definitions),
        }
    }

    let mut referenced = BTreeSet::new();
    let mut collect = |term: &Term| {
        for free in term.free_vars() {
            if let Free::Global(global) = free {
                referenced.insert(global);
            }
        }
    };
    for item in &module.items {
        match item {
            Item::Let(definition) => {
                collect(&definition.type_);
                collect(&definition.body);
            }
            Item::Rec(rec) => {
                for definition in rec.definitions() {
                    collect(&definition.type_);
                    collect(&definition.body);
                }
            }
        }
    }
    collect(&module.body);
    for global in referenced {
        if own.contains(&global) || Some(&global) == owner {
            continue;
        }
        let free = Free::Global(global.clone());
        if let Some(type_) = context.assumption(&free) {
            pool.push((4, (global, type_.clone())));
        }
    }

    pool
}

/// One application attempt: when `head_type` reduces to a function type, instantiate its telescope with fresh metavariables, probe the instantiated output against the goal, and on a definite fit hand back the applied candidate with its hole count — materialized before rollback, so pinned arguments display filled. Hidden slots are omitted from the spelling (they re-infer on paste); an unsolved explicit slot spells the shared hole.
fn apply_fit(
    context: &mut Context,
    head: &Term,
    head_type: &Term,
    goal_type: &Term,
    hole: &Term,
) -> Option<(Term, usize)> {
    let Ok(reduced) = reduce_with(context, head_type) else {
        return None;
    };
    let Subterm::FuncType(FuncType {
        telescope,
        plicities,
    }) = &*reduced
    else {
        return None;
    };

    let mark = context.solution_mark();
    let mut args: Vec<Term> = Vec::new();
    let mut cursor = telescope.clone();
    let output = loop {
        match cursor {
            Telescope::Done(output) => break *output,
            Telescope::Cons(domain, rest) => {
                let arg = context.fresh_unmarked_metavar(domain, None);
                cursor = rest.open(&[&arg]);
                args.push(arg);
            }
        }
    };

    let fit = match convert_outcome(context, &Term::type_ground(), &output, goal_type) {
        Ok(Outcome::Converts) => {
            let mut holes = 0usize;
            let kept: Vec<Term> = args
                .iter()
                .zip(plicities)
                .filter_map(|(arg, plicity)| {
                    let arg = zonk_solved_term_metas(context, arg);
                    let unsolved =
                        arg.any_metavar(&mut |id| context.metavar_solution(id).is_none());
                    match (plicity, unsolved) {
                        // Hidden slots re-infer on paste, solved or not.
                        (Plicity::Implicit | Plicity::Witness, _) => None,
                        (Plicity::Explicit, true) => {
                            holes += 1;
                            Some(hole.clone())
                        }
                        (Plicity::Explicit, false) => Some(arg),
                    }
                })
                .collect();
            let built = zonk_solved_term_metas(context, &Term::apply(head.clone(), kept));
            Some((built, holes))
        }
        _ => None,
    };
    context.rollback_solutions(mark);
    fit
}

/// Pool 1: constructor fits for an inductive goal, and the literal shape for a struct goal.
fn constructor_fits(
    context: &mut Context,
    telescope: &[(Free, Term)],
    goal_type: &Term,
    reduced: &Term,
    hole: &Term,
    candidates: &mut Vec<Candidate>,
) {
    match &**reduced {
        Subterm::InductType(InductType {
            name,
            params,
            indices,
            ..
        }) => {
            let Some(induct_decl) = context.induct_decl(name) else {
                return;
            };
            let tags: Vec<_> = induct_decl
                .constructors
                .iter()
                .map(|(tag, param)| (tag.clone(), param.plicities.clone()))
                .collect();

            // A constructor candidate is spelled as its value-constructor *function* call — the pasteable form. The family is always an authored global; witnesses declare no inductives.
            let Global::Authored(family) = name else {
                return;
            };

            for (tag, plicities) in tags {
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
                            Some((zonk_solved_term_metas(context, &built), holes))
                        } else {
                            None
                        }
                    }
                    Ok(Invert::Impossible) | Err(_) => None,
                };
                context.rollback_solutions(mark);

                if let Some((term, holes)) = candidate {
                    candidates.push(Candidate {
                        term,
                        holes,
                        pool: 1,
                    });
                }
            }
        }
        // A struct-typed goal suggests its literal shape, one hole per field.
        Subterm::StructType(StructType { name, params, .. }) => {
            if let Some(struct_decl) = context.struct_decl(name) {
                let field_count = struct_decl.fields_at(params).len();
                let fields: Vec<Term> = (0..field_count).map(|_| hole.clone()).collect();
                candidates.push(Candidate {
                    term: Term::struct_(name.clone(), params.iter().cloned(), fields),
                    holes: field_count,
                    pool: 1,
                });
            }
        }
        _ => {}
    }
}

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
