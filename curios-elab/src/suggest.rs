//! Candidate suggestions for unsolved written goals — the `? ≈` lines of a goal report.
//!
//! Two families, both computed by machinery elaboration already runs. *Local fits*: a scope binder whose type converts to the goal type (the sandboxed `probe_match` witness resolution uses), and a constructor the goal's indices admit (the shared `invert_indices` unifier match elaboration runs for omitted arms, here for the opposite verdict). *Application fits*: a function from the goal's scope, the entry module's definitions, the globals the module already references, or the bindings its `use` declarations imported, whose instantiated output type converts to the goal — the witness-table instantiation generalized to an arbitrary candidate, so arguments the unification pins display filled (`mk(3)`). An explicit slot the output leaves unpinned is then offered each scope binder in turn, and the first whose type fits is taken — `Eq/sym(h)` for `Eq(7, k)` from `h : Eq(k, 7)` — since a lemma's proof argument is never determined by the goal and almost always *is* a hypothesis in scope. Suggestions are observation-only text the compiler re-checks when the author pastes them, so a wrong candidate costs nothing and checking semantics are untouched.
//!
//! A fit is kept only when it says something: a candidate whose head has explicit parameters, none of which the goal pinned or the scope filled, is `Eq/sym(?)` — true of every equation, and an arity rather than a suggestion — so it is dropped. And a fit whose output conversion is undecided is kept when its blockers are exactly the explicit slots still open: `Eq/cong(?, ih)` has its hypothesis placed and its function genuinely unknown, which is the refinement a reader wants, where the same hole-free candidate would have had to convert outright. Pool 1 admits a holed constructor on the same reasoning.
//!
//! Every attempt runs inside the `solution_mark`/`rollback_solutions` bracket, and a hit is materialized (committed solutions spliced) *before* rollback — the pinned values the display shows would otherwise die with the transaction. Any error skips its candidate: the pass is infallible. Attempts are capped at [`ATTEMPTS`] per goal and the rendered list at [`CANDIDATES`], ranked complete-first, then fewest holes, then pool order.
//!
//! A candidate spells only explicit arguments — hidden slots re-infer when the author pastes it, exactly as they would when writing it by hand — with a shared `?`-named hole standing in for each explicit slot the attempt left unsolved. A hole-free constructor fit must additionally survive [`verifies`], a sandboxed oracle check in the goal's own scope, because index inversion refuses positions it cannot decide and `Solved` alone is not a fit; an application fit needs no second check, since the conversion that established it is definitive. Either way the paste-and-recheck promise is a machine guarantee. Representation privacy is deliberately not consulted: a candidate for a sealed type outside its module simply fails the author's re-check.
//!
//! The whole pass runs under the goal's own scope — its birth telescope assumed into a frame — because the report runs on the bare context after elaboration, and a metavariable minted there is born closed: a solution mentioning a scope binder (`mk(k)` against `Eq(k, k)`) then fails the solver's scope check and the fit silently postpones. Under the frame the fit's metavariables carry the telescope as their birth context, so the same solution inverts. Without it, only a closed goal ever saw an application fit — which is every goal outside a function body, and almost no goal in a proof.

use {
    super::{
        Context, Outcome, Probe, check, convert_outcome, probe_match, reduce_with,
        zonk_solved_term_metas,
    },
    curios_analysis::{Invert, case_target_indices, invert_indices},
    curios_core::{
        Apply, Free, Global, InductType, Item, Module, StructType, Subterm, Telescope, Term, Var,
    },
    curios_utilities::Plicity,
    std::collections::{BTreeMap, BTreeSet},
};

/// The rendered candidate cap per goal.
const CANDIDATES: usize = 3;

/// The attempt cap per goal across the application-fit pools — a hard bound on error-path work, not a completeness promise.
const ATTEMPTS: usize = 128;

/// A found fit: the display term, its residual hole count, and its pool rank (scope values, constructors, scope functions, module definitions, referenced globals, imported bindings — in that order).
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
    context.with_frame(|context| {
        for (name, type_) in telescope {
            context.assume(name, type_);
        }
        suggest_in_scope(context, telescope, goal_type, module, owner)
    })
}

/// [`suggest_candidates`] with the goal's telescope already assumed into the current frame.
fn suggest_in_scope(
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
        constructor_fits(context, goal_type, &reduced, &hole, &mut candidates);
    }

    // Pools 2–5 — application fits: scope binders with function types, then the module's own definitions, then the globals its items reference, then the bindings its `use` declarations imported. Unnameable heads are skipped for the same reason as pool 0. The imports come last so the attempt cap truncates them first: a program imports far more than it mentions.
    let mut attempts = 0usize;
    for (name, type_) in telescope {
        if !name.nameable() {
            continue;
        }
        if attempts >= ATTEMPTS {
            break;
        }
        attempts += 1;
        if let Some((term, holes)) = apply_fit(
            context,
            telescope,
            &Term::free_var(name),
            type_,
            goal_type,
            &hole,
        ) {
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
        if let Some((term, holes)) = apply_fit(context, telescope, &head, &type_, goal_type, &hole)
        {
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

/// Pools 3 to 5: the entry module's own definitions, then every other global its items reference, then every binding in scope of the owning definition through a `use` that neither earlier pool holds, each with its recorded type. The goal's owning definition is excluded.
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
    if let Some(entry) = &module.entry {
        collect(&entry.body);
    }
    for global in &referenced {
        if own.contains(global) || Some(global) == owner {
            continue;
        }
        let free = Free::Global(global.clone());
        if let Some(type_) = context.assumption(&free) {
            pool.push((4, (global.clone(), type_.clone())));
        }
    }

    // The imports in scope where the goal's definition was written — not the unit's, since `use` binds from its own position to the end of its body and a candidate offered above its import would not paste there. A global imported twice in one scope is tried once.
    let mut imported = BTreeSet::new();
    for import in context.imports().in_scope_at(owner) {
        let global = &import.global;
        if own.contains(global)
            || referenced.contains(global)
            || Some(global) == owner
            || !imported.insert(global.clone())
        {
            continue;
        }
        let free = Free::Global(global.clone());
        if let Some(type_) = context.assumption(&free) {
            pool.push((5, (global.clone(), type_.clone())));
        }
    }

    pool
}

/// One application attempt: when `head_type` reduces to a function type, instantiate its telescope with fresh metavariables, probe the instantiated output against the goal, offer each explicit slot the output left unpinned to the scope's binders, and hand back the applied candidate with its hole count when the fit is definite, or undecided on exactly the holes — materialized before rollback, so pinned arguments display filled. Hidden slots are omitted from the spelling (they re-infer on paste); an unsolved explicit slot spells the shared hole.
///
/// The output is probed *before* any slot is filled, so a slot the goal determines keeps the goal's value (`mk(3)` against `Eq(3, 3)` with a `k : Nat` in scope stays `mk(3)`, not `mk(k)`), and a binder is offered only to what the goal left open. The first binder whose type fits a slot is taken, in binding order; a later hypothesis that would also have fit is not a second candidate, since the cap is three lines and the reader can see the scope. Every trial is its own transaction inside the attempt's, so a rejected binder leaves no solution behind for the next.
fn apply_fit(
    context: &mut Context,
    telescope: &[(Free, Term)],
    head: &Term,
    head_type: &Term,
    goal_type: &Term,
    hole: &Term,
) -> Option<(Term, usize)> {
    let Ok(reduced) = reduce_with(context, head_type) else {
        return None;
    };
    let Subterm::FuncType(head_func_type) = &*reduced else {
        return None;
    };
    let params = &head_func_type.telescope;
    let plicities = head_func_type.plicities();

    let mark = context.solution_mark();
    let mut args: Vec<(Term, Term)> = Vec::new();
    let mut cursor = params.clone();
    let output = loop {
        match cursor {
            Telescope::Done(output) => break *output,
            Telescope::Cons(domain, rest) => {
                let arg = context.fresh_hole_metavar(domain.clone(), None);
                cursor = rest.open(&[&arg]);
                args.push((arg, domain));
            }
        }
    };

    let fit = apply_fit_within(
        context, telescope, head, &args, plicities, &output, goal_type, hole,
    );
    context.rollback_solutions(mark);
    context.end_solutions(mark);
    fit
}

/// The body of [`apply_fit`], inside its transaction.
#[allow(clippy::too_many_arguments)]
fn apply_fit_within(
    context: &mut Context,
    telescope: &[(Free, Term)],
    head: &Term,
    args: &[(Term, Term)],
    plicities: &[Plicity],
    output: &Term,
    goal_type: &Term,
    hole: &Term,
) -> Option<(Term, usize)> {
    let unsolved = |context: &Context, arg: &Term| {
        zonk_solved_term_metas(context, arg)
            .any_metavar(&mut |id| context.metavar_solution(id).is_none())
    };

    // A lemma whose result type is headed by one of its own parameters — `subst`'s `P(y)` — meets any goal at all once its proof slot is filled, by the imitation `P := (_) => goal`. Such a fit says nothing unless the conversion decides it outright, so an undecided one is refused below.
    let flex_result = match &**output {
        Subterm::Apply(Apply { head, .. }) => head.metavars(),
        _ => output.metavars(),
    }
    .into_iter()
    .any(|id| args.iter().any(|(arg, _)| arg.metavars().contains(&id)));

    // The goal first: what it pins, it pins.
    let mut outcome = convert_outcome(context, &Term::type_ground(), output, goal_type).ok()?;
    if matches!(outcome, Outcome::Mismatch) {
        return None;
    }

    // Then the scope, for each explicit slot still open, in slot order. A binder fits a slot when its type converts to the slot's domain and the slot's metavariable then converts to the binder — two committed conversions, so the domain's own metavariables (`x` and `y` in `p : Eq(x, y)`) pin from the hypothesis and the output is re-probed below with them in hand.
    let mut filled = false;
    for (arg, domain) in args
        .iter()
        .zip(plicities)
        .filter_map(|(slot, plicity)| (*plicity == Plicity::Explicit).then_some(slot))
    {
        if !unsolved(context, arg) {
            continue;
        }
        for (name, type_) in telescope {
            if !name.nameable() {
                continue;
            }
            let trial = context.solution_mark();
            let domain = zonk_solved_term_metas(context, domain);
            let binder = Term::free_var(name);
            let fits = matches!(
                convert_outcome(context, &Term::type_ground(), type_, &domain),
                Ok(Outcome::Converts)
            ) && matches!(
                convert_outcome(context, &domain, arg, &binder),
                Ok(Outcome::Converts)
            );
            if fits {
                context.end_solutions(trial);
                filled = true;
                break;
            }
            context.rollback_solutions(trial);
            context.end_solutions(trial);
        }
    }
    if filled || matches!(outcome, Outcome::Blocked(_)) {
        outcome = convert_outcome(context, &Term::type_ground(), output, goal_type).ok()?;
    }

    let mut holes = 0usize;
    let mut pinned = 0usize;
    let mut hole_ids = BTreeSet::new();
    let kept: Vec<Term> = args
        .iter()
        .zip(plicities)
        .filter_map(|(slot, plicity)| {
            let (arg, _) = slot;
            let arg = zonk_solved_term_metas(context, arg);
            match (plicity, unsolved(context, &arg)) {
                // Hidden slots re-infer on paste, solved or not.
                (Plicity::Implicit | Plicity::Witness, _) => None,
                (Plicity::Explicit, true) => {
                    holes += 1;
                    hole_ids.extend(arg.metavars());
                    Some(hole.clone())
                }
                (Plicity::Explicit, false) => {
                    pinned += 1;
                    Some(arg)
                }
            }
        })
        .collect();

    match outcome {
        Outcome::Converts => {}
        // Undecided on exactly the open slots is the advisory refinement; undecided on anything else — a hidden slot the goal never reached — is a fit nothing distinguishes from a mismatch.
        Outcome::Blocked(goals) => {
            if holes == 0 || flex_result {
                return None;
            }
            let blockers: BTreeSet<_> = goals
                .iter()
                .flat_map(|goal| [&goal.this, &goal.that, &goal.type_])
                .flat_map(|term| zonk_solved_term_metas(context, term).metavars())
                .filter(|id| context.metavar_solution(*id).is_none())
                .collect();
            if blockers.is_empty() || !blockers.is_subset(&hole_ids) {
                return None;
            }
        }
        Outcome::Mismatch => return None,
    }

    // An explicit parameter list nothing pinned is an arity, not a suggestion.
    if holes > 0 && pinned == 0 {
        return None;
    }

    let built = zonk_solved_term_metas(context, &Term::apply(head.clone(), kept));
    Some((built, holes))
}

/// Pool 1: constructor fits for an inductive goal, and the literal shape for a struct goal.
fn constructor_fits(
    context: &mut Context,
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
                .map(|(tag, param)| (tag.clone(), param.plicities().to_vec()))
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
                        if holes > 0 || verifies(context, &built, goal_type) {
                            Some((zonk_solved_term_metas(context, &built), holes))
                        } else {
                            None
                        }
                    }
                    Ok(Invert::Impossible) | Err(_) => None,
                };
                context.rollback_solutions(mark);
                context.end_solutions(mark);

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

/// Whether the fully-spelled `candidate` checks against `goal_type` in the goal's own scope — already the current frame — the definitive fit gate for a hole-free constructor candidate, and what turns the paste-and-recheck promise into a machine guarantee. Runs as an oracle (parking, refinements, and privacy suppressed — `Blocked` is a mismatch) inside a transaction: every solution the attempt lands is rolled back.
fn verifies(context: &mut Context, candidate: &Term, goal_type: &Term) -> bool {
    let mark = context.solution_mark();
    let verdict =
        context.with_oracle(|context| check(context, candidate, goal_type.clone()).is_ok());
    context.rollback_solutions(mark);
    context.end_solutions(mark);
    verdict
}
