//! Witness resolution: filling omitted `use`-plicity arguments (instance arguments). A goal is a metavariable standing in a `use` slot together with its (concept application) type. Resolution is deterministic, in strict order:
//!
//! 1. **Local scope, direct** — the `use` binders in Γ, innermost-first; first match wins (shadowing, like names). 2. **Local scope, superclass projection** — projections of local `use` binders through the (acyclic) superclass graph, breadth-first by depth; two matches at the same minimal depth are ambiguous. 3. **Global table** — pure lookup by `(concept, tuple of the rigid heads of every parameter)`; a hit instantiates the witness's telescope (fresh metavariables for `@` binders, recursive goals for `use` premises) and unifies its result type against the goal. No projections here. 4. **Flex head** — any parameter still headed by a metavariable parks the goal, woken when a watched metavariable solves.
//!
//! A rigid, keyable head with no table entry *defers* rather than failing: items elaborate in order, and a later item may register the witness. The deferred store is retried after every item and drained — erroring — once the whole module has elaborated.

use {
    super::{
        Context, Error, HeadKey, Outcome, ParkedGoal, ParkedWork, Witness, WitnessKey,
        convert_outcome, reduce_with,
    },
    curios_base::{Plicity, Qualifier, RootId},
    curios_core::{
        Field, Free, Global, ImplicitOrigin, Level, MetaId, Metavar, StructType, Subterm,
        Telescope, Term, UniverseContext, WitnessOrigin,
    },
    std::collections::{BTreeSet, HashSet},
};

/// The outcome of one resolution attempt.
enum Resolution {
    /// A witness term for the goal; solutions its unification committed stay.
    Solved(Term),
    /// The goal's key is still flexible — park, watching its metavariables.
    Flex,
    /// The key is rigid and keyable but the table has no entry (yet) — defer.
    Missing,
    /// Definitely unresolvable: rigid non-keyable head, or a table hit whose remaining parameters do not unify. The caller reports `NoWitness`.
    NoMatch,
}

/// Best-effort display form of a goal for diagnostics: substitute the solutions that have landed, keeping the raw spelling if any hole survives.
fn display_goal(context: &Context, goal: &Term) -> Term {
    super::zonk(context, goal).unwrap_or_else(|_| goal.clone())
}

fn no_witness_error(context: &Context, goal: &Term, provenance: &WitnessOrigin) -> Error {
    Error::no_witness(
        display_goal(context, goal),
        provenance.func.clone(),
        provenance.binder.clone(),
    )
}

/// Whether the (reduced) term is headed by an unsolved metavariable — including a stuck application of one, the higher-kinded case (`?M(?A)`).
fn flex_head(context: &Context, term: &Term) -> bool {
    match &**term {
        Subterm::Metavar(Metavar { id, .. }) => context.metavar_solution(*id).is_none(),
        Subterm::Apply(apply) => flex_head(context, &apply.head),
        _ => false,
    }
}

/// The goal type as a concept application: its (reduced) `StructType` name and parameters, when that name is a registered concept.
fn as_concept_app(context: &Context, goal_whnf: &Term) -> Option<(Global, Vec<Level>, Vec<Term>)> {
    let Subterm::StructType(StructType {
        name,
        universes,
        params,
    }) = &**goal_whnf
    else {
        return None;
    };
    context
        .concept(name)
        .map(|_| (name.clone(), universes.clone(), params.clone()))
}

enum Probe {
    Yes,
    No,
    Undecided,
}

/// Whether `candidate` converts with `goal`, *without* committing: solutions the comparison lands are rolled back regardless of the verdict. Used to test every same-depth superclass projection before committing the unique match.
fn probe_match(context: &mut Context, candidate: &Term, goal: &Term) -> Result<Probe, Error> {
    let mark = context.solution_mark();
    let outcome =
        convert_outcome(context, &Term::type_ground(), candidate, goal).map_err(|error| {
            Error::from_reduce(error, || {
                Error::convert_exhausted(candidate.clone(), goal.clone())
            })
        })?;
    context.rollback_solutions(mark);

    Ok(match outcome {
        Outcome::Converts => Probe::Yes,
        Outcome::Mismatch => Probe::No,
        Outcome::Blocked(_) => Probe::Undecided,
    })
}

/// Like [`probe_match`], but a positive verdict keeps its solutions — the unification is what pins the goal's open parameters to the candidate's.
fn commit_match(context: &mut Context, candidate: &Term, goal: &Term) -> Result<Probe, Error> {
    let mark = context.solution_mark();
    let outcome =
        convert_outcome(context, &Term::type_ground(), candidate, goal).map_err(|error| {
            Error::from_reduce(error, || {
                Error::convert_exhausted(candidate.clone(), goal.clone())
            })
        })?;

    Ok(match outcome {
        Outcome::Converts => Probe::Yes,
        Outcome::Mismatch => {
            context.rollback_solutions(mark);
            Probe::No
        }
        Outcome::Blocked(_) => {
            context.rollback_solutions(mark);
            Probe::Undecided
        }
    })
}

/// Run the resolution algorithm for `goal`. `origin` anchors spans and parked premise goals. Solutions committed by a successful match stay in force.
fn resolve_witness(context: &mut Context, goal: &Term, origin: &Term) -> Result<Resolution, Error> {
    let goal_whnf = reduce_with(context, goal)?;

    // A goal whose whole type is still a hole offers nothing to match on.
    if flex_head(context, &goal_whnf) {
        return Ok(Resolution::Flex);
    }

    let concept_app = as_concept_app(context, &goal_whnf);

    // Step 4 gates steps 1–3 for concept goals: a flex parameter must park rather than match eagerly, or an in-scope binder of the same concept would overcommit the hole (`Show(?T)` grabbing a local `Show(Nat)` while `?T` was headed for `Bin`). Every parameter gates — a first-param-only gate would let `Into(Nat, ?B)` reach the table with an incomplete key and wrongly defer.
    if let Some((_, _, params)) = &concept_app {
        for param in params {
            let head = reduce_with(context, param)?;
            if flex_head(context, &head) {
                return Ok(Resolution::Flex);
            }
        }
    }

    // A non-concept goal still carrying holes is likewise premature.
    if concept_app.is_none()
        && goal_whnf.any_metavar(&mut |id| context.metavar_solution(id).is_none())
    {
        return Ok(Resolution::Flex);
    }

    let mut saw_undecided = false;

    // Step 1: local `use` binders, innermost-first, first match wins.
    //
    // Retrying a parked goal restores its frozen frame's witness binders on top of whatever scope is live at retry time, so a binder that was already in scope at park time can appear twice. Binder names are globally fresh and unique, so a repeated name denotes the *same* binder; dedup by name (the retained occurrence is arbitrary but identical) to keep the superclass search from reporting a projection against itself as ambiguous.
    let mut binders = context.witness_scope().to_vec();
    let mut seen = HashSet::new();
    binders.retain(|(name, _)| seen.insert(name.clone()));
    for (name, type_) in binders.iter().rev() {
        match commit_match(context, type_, &goal_whnf)? {
            Probe::Yes => return Ok(Resolution::Solved(Term::free_var(name))),
            Probe::Undecided => saw_undecided = true,
            Probe::No => {}
        }
    }

    // Steps 2–3 need a concept goal; anything else has no projections and no table to consult.
    let Some((concept_name, universes, params)) = concept_app else {
        return Ok(match saw_undecided {
            true => Resolution::Flex,
            false => Resolution::NoMatch,
        });
    };
    let concept = context
        .concept(&concept_name)
        .cloned()
        .expect("the goal was classified as a registered concept");
    context
        .universes_mut()
        .instantiate_at(&concept.universe_context, &universes)
        .map_err(Error::from)?;

    // Step 2: superclass projections of local binders, breadth-first by depth. The graph is acyclic (checked at registration), so this is finite; two matches at the same minimal depth are ambiguous.
    let mut frontier: Vec<Term> = Vec::new();
    for (name, type_) in binders.iter().rev() {
        let reduced = reduce_with(context, type_)?;
        if as_concept_app(context, &reduced).is_some() {
            frontier.push(Term::free_var(name));
        }
    }

    while !frontier.is_empty() {
        let mut next = Vec::new();
        let mut matched: Vec<(Term, Term)> = Vec::new();

        for node in &frontier {
            for (projection, field_type) in superclass_projections(context, node)? {
                match probe_match(context, &field_type, &goal_whnf)? {
                    Probe::Yes => matched.push((projection.clone(), field_type.clone())),
                    Probe::Undecided => saw_undecided = true,
                    Probe::No => {}
                }
                next.push(projection);
            }
        }

        match matched.len() {
            0 => {}
            1 => {
                let (projection, field_type) = matched.into_iter().next().unwrap();
                // Re-run committing: the probe rolled its solutions back.
                match commit_match(context, &field_type, &goal_whnf)? {
                    Probe::Yes => return Ok(Resolution::Solved(projection)),
                    _ => unreachable!("a probed match commits deterministically"),
                }
            }
            _ => {
                return Err(Error::ambiguous_witness(
                    display_goal(context, &goal_whnf),
                    matched[0].0.clone(),
                    matched[1].0.clone(),
                ));
            }
        }

        frontier = next;
    }

    // Step 3: the global table — pure lookup by (concept, tuple of every parameter head).
    let mut heads = Vec::with_capacity(params.len());
    for param in &params {
        let head = reduce_with(context, param)?;
        match HeadKey::of_whnf(&head) {
            Some(head) => heads.push(head),
            None => {
                heads.clear();
                break;
            }
        }
    }
    if heads.is_empty() {
        // A non-keyable parameter head, or a concept with nothing to key on.
        return Ok(match saw_undecided {
            true => Resolution::Flex,
            false => Resolution::NoMatch,
        });
    }
    let key = WitnessKey(heads);

    let Some(witness) = context.witness(&concept_name, &key).cloned() else {
        return Ok(Resolution::Missing);
    };

    instantiate(context, &witness, &goal_whnf, origin)
}

/// The immediate superclass projections of a node whose type reduces to a concept application: `(projection term, projected field type)` per `use`-marked field.
fn superclass_projections(context: &mut Context, node: &Term) -> Result<Vec<(Term, Term)>, Error> {
    // The node's type: a local binder's assumption, or the previously computed field type — re-derive it via inference-free means: nodes are either a free var (assumption lookup) or a projection whose type we compute here.
    let node_type = node_type(context, node)?;
    let reduced = reduce_with(context, &node_type)?;
    let Subterm::StructType(StructType {
        name,
        universes,
        params,
    }) = &*reduced
    else {
        return Ok(Vec::new());
    };
    let Some(concept) = context.concept(name).cloned() else {
        return Ok(Vec::new());
    };
    let Some(struct_decl) = context.struct_decl(name).cloned() else {
        return Ok(Vec::new());
    };

    let fields = context.instantiate_universe_bound_at(
        &struct_decl.universe_context,
        &struct_decl.fields,
        universes,
    )?;
    let telescope = fields.open_params(params);
    let mut out = Vec::with_capacity(concept.supers.len());
    for (index, _) in &concept.supers {
        let field_type = telescope
            .clone()
            .nth(*index, |j| Term::proj(node.clone(), j))
            .expect("superclass field index is within the concept's telescope");
        out.push((Term::proj(node.clone(), *index), field_type));
    }

    Ok(out)
}

/// The type of a step-2 node: an assumption for a variable, or the projected field type for a projection (recursing on its head).
fn node_type(context: &mut Context, node: &Term) -> Result<Term, Error> {
    match &**node {
        Subterm::Var(var) => context
            .instantiate_assumption(var.unwrap())?
            .map(|(type_, _)| type_)
            .ok_or_else(|| Error::unbound_variable(node.clone())),
        Subterm::Proj(proj) => {
            let Field::Index(index) = proj.field else {
                unreachable!("step-2 projections are built positionally");
            };
            let head_type = node_type(context, &proj.head)?;
            let reduced = reduce_with(context, &head_type)?;
            let Subterm::StructType(StructType {
                name,
                universes,
                params,
            }) = &*reduced
            else {
                return Err(Error::not_a_tuple(Term::unwrap_or_clone(reduced)));
            };
            let struct_decl = context
                .struct_decl(name)
                .cloned()
                .ok_or_else(|| Error::unknown_declaration(name.symbol()))?;
            let fields = context.instantiate_universe_bound_at(
                &struct_decl.universe_context,
                &struct_decl.fields,
                universes,
            )?;
            Ok(fields
                .open_params(params)
                .nth(index, |j| Term::proj(proj.head.clone(), j))
                .expect("projection index within telescope"))
        }
        _ => unreachable!("step-2 nodes are variables or their projections"),
    }
}

/// Instantiate a table hit: fresh metavariables for `@` binders, recursive goals for `use` premises, then unify the result type against the goal (which checks the parameters past the head). Premise goals run through the same algorithm; a premise that parks leaves its metavariable in the built term, spliced once it solves.
fn instantiate(
    context: &mut Context,
    witness: &Witness,
    goal: &Term,
    origin: &Term,
) -> Result<Resolution, Error> {
    let mark = context.solution_mark();
    let (signature, universes) =
        context.instantiate_universe_bound(&witness.universe_context, &witness.signature)?;
    let minted = universes.clone();
    let name = Free::from(&witness.name);
    let head = if universes.is_empty() {
        Term::free_var(&name)
    } else {
        Term::universe_inst(Term::free_var(&name), universes)
    };
    let span = origin.span();

    let (args, premises, terminal) = match &*signature {
        Subterm::FuncType(ft) => {
            let mut args: Vec<(Plicity, Term)> = Vec::with_capacity(ft.plicities.len());
            let mut premises: Vec<(MetaId, Term, WitnessOrigin)> = Vec::new();
            let mut tele = ft.telescope.clone();
            for plicity in &ft.plicities {
                let Telescope::Cons(ty, rest) = tele else {
                    unreachable!("plicities parallel the telescope");
                };
                let binder = rest.first_hint().unwrap_or("_").to_string();
                let arg = match plicity {
                    Plicity::Implicit => context.fresh_metavar(
                        ty.clone(),
                        span.clone(),
                        ImplicitOrigin {
                            func: witness.name.symbol(),
                            binder,
                        },
                    ),
                    Plicity::Witness => {
                        let provenance = WitnessOrigin {
                            func: witness.name.symbol(),
                            binder,
                        };
                        let (id, metavar) = context.fresh_witness_metavar(
                            ty.clone(),
                            span.clone(),
                            provenance.clone(),
                        );
                        premises.push((id, ty.clone(), provenance));
                        metavar
                    }
                    Plicity::Explicit => {
                        unreachable!("registration rejects explicit witness parameters")
                    }
                };
                tele = rest.open(&[&arg]);
                args.push((*plicity, arg));
            }
            let Telescope::Done(terminal) = tele else {
                unreachable!("plicities parallel the telescope");
            };
            (args, premises, *terminal)
        }
        _ => (Vec::new(), Vec::new(), signature),
    };

    // Instantiated premise types must reflect solutions the terminal unification lands, so unify first, then resolve premises.
    match commit_match(context, &terminal, goal)? {
        Probe::Yes => {}
        Probe::No => {
            context.rollback_solutions(mark);
            return Ok(Resolution::NoMatch);
        }
        Probe::Undecided => {
            context.rollback_solutions(mark);
            return Ok(Resolution::Flex);
        }
    }

    // The witness inhabits *this* goal and no other, so the levels its scheme introduced here are determined by the goal's — they are not free. Conversion alone does not say so: cumulativity makes a concept application's universe arguments a bound rather than an equation, and `solve_flexible_in` deliberately leaves a bounded-but-unsolved level for declaration finalization to generalize. That is sound only while the consuming declaration is still open. A goal that *defers* — the normal case for a `/syn` declaration whose witness lives in `/std`, since the two roots import mutually — resolves long after its consumer finalized, and the level it mints then has nothing left to close it. Pin the instantiation to what the goal already fixes. This must run *before* the premises: a premise goal is stated in terms of these levels, so pinning first is what makes the same argument hold recursively for every witness the premises pull in.
    if !minted.is_empty() {
        let terminal = reduce_with(context, &terminal)?;
        // A terminal that is not a concept application pins nothing, but its levels still have to be closed, so the minimizing pass runs either way.
        let (instance, determined) = match (
            as_concept_app(context, &terminal),
            as_concept_app(context, goal),
        ) {
            (Some((_, from_witness, _)), Some((_, from_goal, _)))
                if from_witness.len() == from_goal.len() =>
            {
                (from_witness, from_goal)
            }
            _ => (Vec::new(), Vec::new()),
        };
        context.close_universe_instance(&minted, &instance, &determined)?;
    }
    for (id, type_, provenance) in premises {
        attempt_witness_goal(context, id, &type_, provenance, origin)?;
    }

    let term = match args.is_empty() {
        true => head,
        false => Term::apply_marked(head, args),
    };

    Ok(Resolution::Solved(term))
}

/// Attempt a freshly minted witness goal: solve it now, park it on a flex key, or defer it on a missing table entry. A definite failure is an error at `origin`'s span.
pub(crate) fn attempt_witness_goal(
    context: &mut Context,
    slot: MetaId,
    goal: &Term,
    provenance: WitnessOrigin,
    origin: &Term,
) -> Result<(), Error> {
    match resolve_witness(context, goal, origin)? {
        Resolution::Solved(term) => {
            context.solve_metavar(slot, term);
            Ok(())
        }
        Resolution::Flex => {
            context.park(
                ParkedWork::Witness {
                    slot,
                    goal: goal.clone(),
                    provenance,
                },
                origin.clone(),
            );
            Ok(())
        }
        Resolution::Missing => {
            let frame = context.freeze_frame();
            context.defer_witness(ParkedGoal {
                work: ParkedWork::Witness {
                    slot,
                    goal: goal.clone(),
                    provenance,
                },
                origin: origin.clone(),
                frame,
                watching: BTreeSet::new(),
            });
            Ok(())
        }
        Resolution::NoMatch => {
            Err(no_witness_error(context, goal, &provenance).at_opt(origin.span()))
        }
    }
}

/// Retry a parked or deferred witness goal under its frozen frame. Called by `retry_parked`'s wake path and the deferred-goal sweeps.
pub(crate) fn retry_witness(
    context: &mut Context,
    slot: MetaId,
    goal: Term,
    provenance: WitnessOrigin,
    origin: Term,
    frame: super::FrozenFrame,
) -> Result<(), Error> {
    let resolution = context.with_frame(|context| {
        context.restore_frame(&frame);
        resolve_witness(context, &goal, &origin)
    })?;

    match resolution {
        Resolution::Solved(term) => {
            context.solve_metavar(slot, term);
            Ok(())
        }
        Resolution::Flex => {
            context.repark(
                ParkedWork::Witness {
                    slot,
                    goal,
                    provenance,
                },
                origin,
                frame,
            );
            Ok(())
        }
        Resolution::Missing => {
            context.defer_witness(ParkedGoal {
                work: ParkedWork::Witness {
                    slot,
                    goal,
                    provenance,
                },
                origin,
                frame,
                watching: BTreeSet::new(),
            });
            Ok(())
        }
        Resolution::NoMatch => {
            Err(no_witness_error(context, &goal, &provenance).at_opt(origin.span()))
        }
    }
}

/// Retry every deferred witness goal — after an item, when new witnesses may have registered. Goals that stay unresolvable re-defer; solutions that land wake parked constraints.
pub(crate) fn retry_deferred_witnesses(context: &mut Context) -> Result<(), Error> {
    let deferred = context.take_deferred_witnesses();
    if deferred.is_empty() {
        return Ok(());
    }

    for parked in deferred {
        let ParkedGoal {
            work:
                ParkedWork::Witness {
                    slot,
                    goal,
                    provenance,
                },
            origin,
            frame,
            ..
        } = parked
        else {
            unreachable!("only witness goals defer");
        };
        retry_witness(context, slot, goal, provenance, origin, frame)?;

        // The deferred store is retried only *between* items, so the declaration that raised this goal has already finalized: its universe scheme is fixed, and no later pass will generalize or minimize a level introduced now. `instantiate` pins the witness's instance against the goal for exactly that reason. Check it held. Without this, a level that slips through is reported by `zonk` at the end of the module as an anonymous `?uN` escaping, naming neither the goal that introduced it nor the witness it came from.
        if let Some(solution) = context.metavar_solution(slot).cloned()
            && solution.any_universe_meta(|meta| {
                context
                    .universes()
                    .zonk(&Level::meta(meta))
                    .is_ok_and(|level| !level.metas().collect::<Vec<_>>().is_empty())
            })
        {
            return Err(Error::UniverseInvariant(format!(
                "a deferred witness left an unsolved universe level in {solution}"
            )));
        }
    }

    context.retry_parked()
}

/// The end-of-module sweep: retry once more, then report any survivor — the whole program has elaborated, so a still-missing table entry will never register.
pub(crate) fn finish_deferred_witnesses(context: &mut Context) -> Result<(), Error> {
    retry_deferred_witnesses(context)?;

    if let Some(parked) = context.take_deferred_witnesses().into_iter().next() {
        let ParkedGoal {
            work: ParkedWork::Witness {
                goal, provenance, ..
            },
            origin,
            ..
        } = parked
        else {
            unreachable!("only witness goals defer");
        };
        return Err(no_witness_error(context, &goal, &provenance).at_opt(origin.span()));
    }

    Ok(())
}

/// Register an elaborated definition as a witness: validate its telescope (no explicit binders, regular premises), key it on the tuple of rigid heads of the concept's parameters, and insert it into the program-wide table — rejecting a duplicate key. `signature` is the definition's *elaborated* type; `root` is its declaring `Definition`'s own precomputed root (consulted by the orphan-rule check below, never re-derived here). Registration ignores `pub`: visibility governs the name, never table membership.
pub(crate) fn register_witness(
    context: &mut Context,
    name: &Global,
    signature: &Term,
    universe_context: UniverseContext,
    module: &Qualifier,
    root: RootId,
) -> Result<(), Error> {
    let reduced = reduce_with(context, signature)?;

    // Peel the telescope, opening each binder with its own label as a neutral free variable (elaborated binder labels are entropy-fresh, so they cannot collide).
    let mut binders: Vec<(Plicity, Free, Term)> = Vec::new();
    let terminal = match &*reduced {
        Subterm::FuncType(ft) => {
            let mut tele = ft.telescope.clone();
            for plicity in &ft.plicities {
                let Telescope::Cons(ty, rest) = tele else {
                    unreachable!("plicities parallel the telescope");
                };
                if matches!(plicity, Plicity::Explicit) {
                    return Err(Error::explicit_witness_param(name.symbol()));
                }
                let binder = context.fresh(rest.first_hint());
                tele = rest.open(&[&Term::free_var(&binder)]);
                binders.push((*plicity, binder, ty.clone()));
            }
            let Telescope::Done(terminal) = tele else {
                unreachable!("plicities parallel the telescope");
            };
            *terminal
        }
        _ => reduced.clone(),
    };

    let terminal = reduce_with(context, &terminal)?;
    let Subterm::StructType(StructType {
        name: concept_name,
        params,
        ..
    }) = &*terminal
    else {
        return Err(Error::not_a_concept(name.symbol(), terminal.clone()));
    };
    if context.concept(concept_name).is_none() {
        return Err(Error::not_a_concept(name.symbol(), terminal.clone()));
    }

    // Key on every parameter: each must reduce to a rigid, keyable head.
    if params.is_empty() {
        return Err(Error::invalid_witness_head(
            name.symbol(),
            0,
            terminal.clone(),
        ));
    }
    let mut heads = Vec::with_capacity(params.len());
    for (position, param) in params.iter().enumerate() {
        let head = reduce_with(context, param)?;
        let Some(head) = HeadKey::of_whnf(&head) else {
            return Err(Error::invalid_witness_head(name.symbol(), position, head));
        };
        heads.push(head);
    }
    let key = WitnessKey(heads);

    // Termination (Haskell-98-lite): every `use` premise applies a concept to *variables bound by this witness's own telescope* — resolution through it is then structurally decreasing, with no fuel or tabling.
    let binder_names: BTreeSet<&Free> = binders.iter().map(|(_, n, _)| n).collect();
    for (plicity, _, type_) in &binders {
        if !matches!(plicity, Plicity::Witness) {
            continue;
        }
        let premise = reduce_with(context, type_)?;
        let Subterm::StructType(StructType {
            name: premise_concept,
            params: premise_args,
            ..
        }) = &*premise
        else {
            return Err(Error::non_regular_witness_premise(
                name.symbol(),
                premise.clone(),
            ));
        };
        if context.concept(premise_concept).is_none() {
            return Err(Error::non_regular_witness_premise(
                name.symbol(),
                premise.clone(),
            ));
        }
        let regular = premise_args.iter().all(|arg| match &**arg {
            Subterm::Var(var) => var
                .as_free()
                .is_some_and(|free| binder_names.contains(free)),
            _ => false,
        });
        if !regular {
            return Err(Error::non_regular_witness_premise(
                name.symbol(),
                premise.clone(),
            ));
        }
    }

    // The orphan rule: a witness may be declared only where the concept it witnesses, or at least one rigid type in its key, is already declared — never by a third root unrelated to both. Without this, two unrelated roots could each legally `satisfy` the same `(concept, key)` pair, a collision that is unfixable once both are linked into one program (see `documentation/ROADMAP.md`'s Type System section). Checked before the duplicate-key insert below: "not allowed to register this at all" is the more fundamental violation than "and it also collides."
    //
    // A privileged root (`sys`/`syn`/`std`) is exempt: the three are one coordinated standard library, not independent unrelated packages, so e.g. a `/std`-declared `Eql(Str)` witness bridging `/sys`'s `Eql` concept and `/syn`'s `Str` type is exactly the sanctioned pattern (`/std` facades for `/sys`/`/syn`-homed concepts and types), not an orphan instance. The check only bites an ordinary root — the entry program today, an untrusted external package once one exists.
    let concept_root = context
        .concept(concept_name)
        .expect("the concept was looked up above")
        .root;
    if !root.kind().is_privileged()
        && root != concept_root
        && !key.0.iter().any(|head| context.root_of_head(head) == root)
    {
        return Err(Error::orphan_witness(
            concept_name.symbol(),
            key,
            module.clone(),
        ));
    }

    if let Some(first_module) = context.insert_witness(
        concept_name.clone(),
        key.clone(),
        Witness {
            name: name.clone(),
            module: module.clone(),
            universe_context,
            signature: signature.clone(),
            root,
        },
    ) {
        return Err(Error::duplicate_witness(
            concept_name.symbol(),
            key,
            first_module,
            module.clone(),
        ));
    }

    Ok(())
}

/// Validate the seeded concept registry: every superclass edge targets a registered, different concept, and the graph is acyclic.
pub(crate) fn check_concept_registry(context: &Context) -> Result<(), Error> {
    let concepts = context.concepts();

    for (name, concept) in concepts {
        for (_, target) in &concept.supers {
            if target == name {
                return Err(Error::cyclic_superclass(name.symbol()));
            }
            if !concepts.contains_key(target) {
                return Err(Error::unknown_superclass(name.symbol(), target.symbol()));
            }
        }
    }

    // Three-color DFS over the superclass edges.
    fn visit(
        concepts: &std::collections::BTreeMap<Global, super::Concept>,
        name: &Global,
        visiting: &mut BTreeSet<Global>,
        done: &mut BTreeSet<Global>,
    ) -> Result<(), Error> {
        if done.contains(name) {
            return Ok(());
        }
        if !visiting.insert(name.clone()) {
            return Err(Error::cyclic_superclass(name.symbol()));
        }
        if let Some(concept) = concepts.get(name) {
            for (_, target) in &concept.supers {
                visit(concepts, target, visiting, done)?;
            }
        }
        visiting.remove(name);
        done.insert(name.clone());
        Ok(())
    }

    let mut visiting = BTreeSet::new();
    let mut done = BTreeSet::new();
    for name in concepts.keys() {
        visit(concepts, name, &mut visiting, &mut done)?;
    }

    Ok(())
}
