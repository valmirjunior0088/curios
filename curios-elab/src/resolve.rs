//! Witness resolution: filling omitted `use`-plicity arguments (instance arguments). A goal is a metavariable standing in a `use` slot together with its (concept application) type. Resolution is deterministic, in strict order:
//!
//! 1. **Local scope, direct** — the `use` binders in Γ, innermost-first; first match wins (shadowing, like names).
//! 2. **Local scope, superclass projection** — projections of local `use` binders through the (acyclic) superclass graph, breadth-first by depth; two matches at the same minimal depth are ambiguous.
//! 3. **Global table** — pure lookup by `(concept, tuple of the rigid heads of every parameter)`; a hit instantiates the witness's telescope (fresh metavariables for `@` binders, recursive goals for `use` premises) and unifies its result type against the goal. No projections here.
//! 4. **Flex head** — any parameter still headed by a metavariable parks the goal, woken when a watched metavariable solves.
//!
//! A rigid, keyable head with no table entry *defers* rather than failing: items elaborate in order, and a later item may register the witness. The deferred store is retried after every item and drained — erroring — once the whole module has elaborated.
//!
//! **A goal is attempted only while its slot is open.** The slot is a metavariable like any other, so unification may solve it first — against an expected type or an argument's type that names a dictionary, as a type declared under a `use` premise does — and that solution is the answer: a value built under one dictionary is read under the same one. Resolving the goal anyway would write the table's entry over it, which the elaborator would accept and the kernel refuse, the argument's type no longer converting with the parameter's. Every door that commits a resolution asks first: the first attempt, a parked or deferred retry, and the end-of-module sweep.
//!
//! **Rule 1 beating rule 3 is why a concept cannot state a law about the *registered* witness.** A field whose own telescope takes `use C(A)` states its law over an arbitrary `C` rather than the one the program registered, so no witness can discharge it — which rules out checking a copy of a resolved witness from inside the concept that copies it. Removing the copy is the move that remains: a superclass edge, whose slot resolution fills.

use {
    super::{
        Callee, Context, EmbeddingDiagnosis, Error, FrozenFrame, HeadKey, ItemStamp, Outcome,
        ParkedProblem, ParkedWork, ShapeDiagnosis, Witness, WitnessKey, attempt_discharge,
        convert_outcome, reduce_with, resolved_for_display,
    },
    crate::{SlotPositions, is_prop, premise_label},
    curios_core::{
        Advance, CalleeId, ConceptDecl, Enter, Field, Free, Global, ImplicitOrigin, Instance,
        InstanceHead, Level, Metavar, MetavarId, StructType, Subterm, Term, UniverseContext,
        WitnessOrigin,
    },
    curios_utilities::{Mount, Plicity, Qualifier, Span},
    std::collections::{BTreeMap, BTreeSet, HashSet, VecDeque},
};

/// The outcome of one resolution attempt.
enum Resolution {
    /// A witness term for the goal; solutions its unification committed stay.
    Solved(Term),
    /// The goal's key is still flexible — park, watching its metavariables.
    Flex,
    /// The key is rigid and keyable but the table has no entry (yet) — defer.
    Missing,
    /// The key is one a refused declaration's witness stood under: the goal is that refusal's dependent, and the caller withholds rather than reports.
    Poisoned,
    /// Definitely unresolvable: rigid non-keyable head, or a table hit whose remaining parameters do not unify. The caller reports `NoWitness`.
    NoMatch,
}

/// A deferred witness goal that will never resolve, attributed to the item that raised it.
pub(crate) struct DeferredRefusal {
    pub item: ItemStamp,
    pub error: Error,
}

/// Best-effort display form of a goal for diagnostics — the renderer every mismatch report uses (`resolved_for_display`), so a goal is spelled as the rest of the reports spell a type. A bare strict zonk would render a nominal type through its recursive-group projection — `no witness of Spell(rec #0: Type = Opaque; #0) found` — because a zonked solution spells a stuck recursive call as the `Rec` node itself until the refold gives it back its name.
fn display_goal(context: &mut Context, goal: &Term) -> Term {
    resolved_for_display(context, goal)
}

/// Read an insertion provenance back into who it names ([`Callee`]). The discrimination is the [`CalleeId`]'s own, so this is a total match rather than a parse: an operator carries its [`InfixOp`](curios_utilities::InfixOp), and a witness carries the identity the coherence table is keyed by, which the report renames to a concept and key. Error-path only: the witness lookup scans the table.
pub(crate) fn callee(context: &Context, func: &CalleeId) -> Callee {
    match func {
        CalleeId::Operator(op) => {
            let method = context.syntax().operator.concept_field(*op);
            Callee::Operator {
                op: *op,
                method: Global::Authored(method.concept.qualifier().with(method.field)),
            }
        }
        // A witness with no table entry is left as the bare identity: coherence has nothing to rename it by, and a report naming the global beats one naming nothing.
        CalleeId::Witness(name) => context
            .witness_keyed_entries()
            .find(|(_, _, witness)| &witness.name == name)
            .map(|(concept, key, _)| Callee::Witness {
                concept: *concept,
                key: key.clone(),
            })
            .unwrap_or_else(|| Callee::Function(Free::Global(*name))),
        CalleeId::Constructor { owner, tag } => Callee::Constructor {
            owner: *owner,
            tag: tag.clone(),
        },
        CalleeId::Function(name) => Callee::Function(*name),
        CalleeId::Structure(name) => Callee::Structure(*name),
        CalleeId::Anonymous => Callee::Anonymous,
    }
}

fn no_witness_error(
    context: &mut Context,
    goal: &Term,
    provenance: &WitnessOrigin,
    site: Option<&Span>,
) -> Error {
    let embedding = diagnose_embedding(context, goal, site);
    let shape = diagnose_shape(context, goal);
    Error::no_witness(
        display_goal(context, goal),
        callee(context, &provenance.func),
        provenance.binder.clone(),
        embedding,
        shape,
    )
}

/// When `goal` keys on a *labeled* tuple shape and the same key with every label dropped does have a witness, the shape-specific diagnosis its missing-witness report carries: the two keys, so the reader meets the rule — labels are part of a tuple type's identity — rather than a bare miss. That is the surprise keying on an anonymous shape has; every other way to miss the table is a miss for the ordinary reason. Error-path only; a diagnosis that cannot be computed is simply absent.
pub(crate) fn diagnose_shape(context: &mut Context, goal: &Term) -> Option<Box<ShapeDiagnosis>> {
    let goal = reduce_with(context, goal).ok()?;
    let (concept_name, _, params) = as_concept_app(context, &goal)?;

    let mut wanted = Vec::with_capacity(params.len());
    for param in &params {
        let head = reduce_with(context, param).ok()?;
        wanted.push(HeadKey::of_whnf(&head)?);
    }

    let bare: Vec<HeadKey> = wanted
        .iter()
        .map(|head| match head {
            HeadKey::TupleType(labels) => HeadKey::TupleType(vec![String::new(); labels.len()]),
            head => head.clone(),
        })
        .collect();

    // Equal keys mean nothing was dropped: no parameter is an anonymous shape, or every one is already bare, and the goal simply misses.
    if bare == wanted {
        return None;
    }

    let bare = WitnessKey(bare);
    context.witness(&concept_name, &bare)?;

    Some(Box::new(ShapeDiagnosis {
        wanted: WitnessKey(wanted),
        bare,
    }))
}

/// When `goal` is an application of the registry's `Lift` concept, the embedding-specific diagnosis its missing-witness report carries: the two monads in display form, whether the source is a monad at all, and any chain of declared edges between the pair — each fact the report needs to steer the fix (declare the edge, fix the action, or spell the composite) without the reader reconstructing the table. Where the goal came from an auto-lift at `site`, the monads are the ones it named as written ([`EmbeddingSite`](crate::EmbeddingSite)), and so is the goal the report leads with. Error-path only; a diagnosis that cannot be computed is simply absent.
pub(crate) fn diagnose_embedding(
    context: &mut Context,
    goal: &Term,
    site: Option<&Span>,
) -> Option<EmbeddingDiagnosis> {
    let goal = reduce_with(context, goal).ok()?;
    let (concept_name, _, params) = as_concept_app(context, &goal)?;
    let Global::Authored(concept_path) = &concept_name else {
        return None;
    };
    if *concept_path != context.syntax().lift.lift.concept.qualifier() {
        return None;
    }
    let [source, target] = params.as_slice() else {
        return None;
    };
    let source_whnf = reduce_with(context, source).ok()?;
    let target_whnf = reduce_with(context, target).ok()?;
    let source_key = HeadKey::of_whnf(&source_whnf);
    let target_key = HeadKey::of_whnf(&target_whnf);

    // Monad-hood of the source: any registered Monad witness under its head. The concept's name derives from the registry's bind wrapper — its namespace is the concept.
    let monad_concept = Global::Authored(context.syntax().monad.bind.qualifier().without_last());
    let source_is_monad = match &source_key {
        Some(key) => context
            .witness(&monad_concept, &WitnessKey(vec![key.clone()]))
            .is_some(),
        None => false,
    };

    let chain = match (&source_key, &target_key) {
        (Some(from), Some(to)) if from != to => lift_chain(context, &concept_name, from, to),
        _ => Vec::new(),
    };

    let written = site.and_then(|span| context.embedding_site(span)).cloned();
    let written_source = written
        .as_ref()
        .and_then(|site| declared_result(context, &site.action))
        .and_then(|result| written_monad(&result));
    let source = match written_source {
        Some(source) => display_goal(context, &source),
        None => display_goal(context, &source_whnf),
    };
    let target = match &written {
        Some(site) => display_goal(context, &site.region),
        None => display_goal(context, &target_whnf),
    };
    let written_goal = written.and_then(|_| match &*goal {
        Subterm::StructType(struct_type) => {
            let mut struct_type = struct_type.clone();
            struct_type.params = vec![source.clone(), target.clone()];
            Some(Box::new(Term::from(Subterm::StructType(struct_type))))
        }
        _ => None,
    });

    Some(EmbeddingDiagnosis {
        source: Box::new(source),
        target: Box::new(target),
        source_is_monad,
        chain,
        goal: written_goal,
    })
}

/// `F(c̄, v)` as the monad it names, `F(c̄)`: an application of a name — at a universe instance or not — without its value slot, its explicit arguments alone, which is what a report shows of a monad. `None` unless `type_` applies a name.
pub(crate) fn written_monad(type_: &Term) -> Option<Term> {
    let Subterm::Apply(apply) = &**type_ else {
        return None;
    };
    named(&apply.head)?;
    let mut explicit: Vec<Term> = apply
        .params()
        .zip(apply.plicities())
        .filter(|(_, plicity)| matches!(plicity, Plicity::Explicit))
        .map(|(argument, _)| argument.clone())
        .collect();
    explicit.pop()?;
    Some(match explicit.is_empty() {
        true => apply.head.clone(),
        false => Term::apply(apply.head.clone(), explicit),
    })
}

/// The name `head` is, at a universe instance or not.
fn named(head: &Term) -> Option<&Free> {
    match &**head {
        Subterm::Var(var) => var.as_free(),
        Subterm::Instance(Instance {
            head: InstanceHead::Var(var),
            ..
        }) => var.as_free(),
        _ => None,
    }
}

/// The result `action`'s head declares, as written: the head is a name, its declared arrows are opened without reducing anything, and the result mentions none of their binders. `None` otherwise, and the report shows the monad unification solved. Error-path only, since opening the arrows mints binders.
fn declared_result(context: &mut Context, action: &Term) -> Option<Term> {
    let mut head = action;
    while let Subterm::Apply(apply) = &**head {
        head = &apply.head;
    }
    let mut result = context.assumption(named(head)?)?.clone();
    let mut binders = Vec::new();
    while let Subterm::FuncType(func_type) = &*result {
        let mut cursor = func_type.telescope.cursor();
        while cursor.entry().is_some() {
            binders.push(cursor.advance_fresh(|hint| context.fresh(hint)));
        }
        result = cursor.body().expect("a cursor past every entry");
    }
    let mentioned = result.free_vars();
    binders
        .iter()
        .all(|binder| !mentioned.contains(binder))
        .then_some(result)
}

/// A shortest chain of declared `Lift` edges from `from` to `to`, each hop rendered as its key and declaring module — breadth-first over the witness table's keys, so the result is minimal and deterministic.
fn lift_chain(
    context: &Context,
    lift: &Global,
    from: &HeadKey,
    to: &HeadKey,
) -> Vec<(String, Qualifier)> {
    let mut edges: Vec<(HeadKey, HeadKey, String, Qualifier)> = Vec::new();
    for (concept, key, witness) in context.witness_keyed_entries() {
        if concept != lift {
            continue;
        }
        let [m, n] = key.0.as_slice() else {
            continue;
        };
        edges.push((m.clone(), n.clone(), key.to_string(), witness.module));
    }

    let mut parents: BTreeMap<HeadKey, usize> = BTreeMap::new();
    let mut queue = VecDeque::from([from.clone()]);
    while let Some(node) = queue.pop_front() {
        if node == *to {
            break;
        }
        for (index, (m, n, _, _)) in edges.iter().enumerate() {
            if *m == node && !parents.contains_key(n) && *n != *from {
                parents.insert(n.clone(), index);
                queue.push_back(n.clone());
            }
        }
    }

    let mut hops = Vec::new();
    let mut node = to.clone();
    while let Some(&index) = parents.get(&node) {
        let (m, _, pair, module) = &edges[index];
        hops.push((pair.clone(), *module));
        node = m.clone();
    }
    if node != *from {
        return Vec::new();
    }
    hops.reverse();
    hops
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
        .map(|_| (*name, universes.clone(), params.clone()))
}

pub(crate) enum Probe {
    Yes,
    No,
    Undecided,
}

/// Whether `candidate` converts with `goal`, *without* committing: solutions the comparison lands are rolled back regardless of the verdict. Used to test every same-depth superclass projection before committing the unique match, and by the goal-suggestion pass to test scope binders against a goal type.
pub(crate) fn probe_match(
    context: &mut Context,
    candidate: &Term,
    goal: &Term,
) -> Result<Probe, Error> {
    // Hand-paired rather than bracketed by a closure: this sits on the witness-resolution recursion, where a closure body costs a stack frame per nested premise.
    let mark = context.solution_mark();
    let outcome = match convert_outcome(context, &Term::type_ground(), candidate, goal) {
        Ok(outcome) => outcome,
        Err(error) => {
            context.end_solutions(mark);
            return Err(Error::from_reduce(error, |refusal| {
                Error::convert_exhausted(candidate.clone(), goal.clone(), refusal)
            }));
        }
    };
    context.rollback_solutions(mark);
    context.end_solutions(mark);

    Ok(match outcome {
        Outcome::Converts => Probe::Yes,
        Outcome::Mismatch(_) => Probe::No,
        Outcome::Blocked(_) => Probe::Undecided,
    })
}

/// Like [`probe_match`], but a positive verdict keeps its solutions — the unification is what pins the goal's open parameters to the candidate's.
fn commit_match(context: &mut Context, candidate: &Term, goal: &Term) -> Result<Probe, Error> {
    // Hand-paired for the same reason as [`probe_match`]: this is on the recursion.
    let mark = context.solution_mark();
    let outcome = match convert_outcome(context, &Term::type_ground(), candidate, goal) {
        Ok(outcome) => outcome,
        Err(error) => {
            context.end_solutions(mark);
            return Err(Error::from_reduce(error, |refusal| {
                Error::convert_exhausted(candidate.clone(), goal.clone(), refusal)
            }));
        }
    };

    let probe = match outcome {
        Outcome::Converts => Probe::Yes,
        Outcome::Mismatch(_) => {
            context.rollback_solutions(mark);
            Probe::No
        }
        Outcome::Blocked(_) => {
            context.rollback_solutions(mark);
            Probe::Undecided
        }
    };
    context.end_solutions(mark);

    Ok(probe)
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
    binders.retain(|(name, _)| seen.insert(*name));
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
        return Ok(match context.is_poisoned_witness(&concept_name, &key) {
            true => Resolution::Poisoned,
            false => Resolution::Missing,
        });
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
    if context.concept(name).is_none() {
        return Ok(Vec::new());
    }
    let Some(struct_decl) = context.struct_decl(name).cloned() else {
        return Ok(Vec::new());
    };

    let arity = context.instantiate_universe_bound_at(
        &struct_decl.universe_context,
        &struct_decl.arity,
        universes,
    )?;
    let telescope = arity.open(&params.iter().collect::<Vec<_>>());
    // An edge is a `use` field of the concept's telescope, so where the edges stand is the telescope's to say.
    let mut out = Vec::new();
    for (index, mark) in telescope.marks().into_iter().enumerate() {
        if mark != Plicity::Witness {
            continue;
        }
        let field_type = telescope
            .clone()
            .field_type_from(node, index)
            .expect("a field's position is within its telescope");
        out.push((Term::proj(node.clone(), index), field_type));
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
            let arity = context.instantiate_universe_bound_at(
                &struct_decl.universe_context,
                &struct_decl.arity,
                universes,
            )?;
            Ok(arity
                .open(&params.iter().collect::<Vec<_>>())
                .field_type_from(&proj.head, index)
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
    // Hand-paired for the same reason as [`probe_match`]: this is the recursion, one level per nested premise, and this body is large.
    let mark = context.solution_mark();
    let (signature, universes) =
        context.instantiate_universe_bound(&witness.universe_context, &witness.signature)?;
    let minted = universes.clone();
    let name = Free::from(&witness.name);
    let head = if universes.is_empty() {
        Term::free_var(&name)
    } else {
        Term::instance_of(&name, universes)
    };
    let span = origin.span();

    let (args, premises, bounds, terminal) = match &*signature {
        Subterm::FuncType(ft) => {
            let mut args: Vec<(Plicity, Term)> = Vec::with_capacity(ft.plicities().len());
            let mut premises: Vec<(MetavarId, Term, WitnessOrigin)> = Vec::new();
            let mut bounds: Vec<(MetavarId, Term, ImplicitOrigin)> = Vec::new();
            let mut positions = SlotPositions::default();
            let mut cursor = ft.telescope.cursor();
            while let Some(plicity) = cursor.mark() {
                let (hint, ty) = cursor.entry().expect("a mark stands at an entry");
                let position = positions.next(plicity);
                let binder = hint.unwrap_or("_").to_string();
                let arg = match plicity {
                    Plicity::Implicit => {
                        let proposition =
                            curios_core::Probe::probed(is_prop(context, &ty))?.unwrap_or(false);
                        let provenance = ImplicitOrigin {
                            func: CalleeId::Witness(witness.name),
                            binder,
                        };
                        let (slot, hole) = context.fresh_metavar(
                            ty.clone(),
                            span.clone(),
                            provenance.clone(),
                            proposition,
                            None,
                        );
                        if proposition {
                            bounds.push((slot, ty.clone(), provenance));
                        }
                        hole
                    }
                    Plicity::Witness => {
                        let provenance = WitnessOrigin {
                            func: CalleeId::Witness(witness.name),
                            binder: premise_label(position),
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
                cursor.advance(arg.clone());
                args.push((plicity, arg));
            }
            let terminal = cursor.body().expect("a cursor past every entry");
            (args, premises, bounds, terminal)
        }
        _ => (Vec::new(), Vec::new(), Vec::new(), signature),
    };

    // Instantiated premise types must reflect solutions the terminal unification lands, so unify first, then resolve premises.
    match commit_match(context, &terminal, goal)? {
        Probe::Yes => {}
        Probe::No => {
            context.rollback_solutions(mark);
            context.end_solutions(mark);
            return Ok(Resolution::NoMatch);
        }
        Probe::Undecided => {
            context.rollback_solutions(mark);
            context.end_solutions(mark);
            return Ok(Resolution::Flex);
        }
    }

    // The witness inhabits *this* goal and no other, so the levels its scheme introduced here that the goal's application names are determined by the goal's — they are not free. Conversion alone does not say so: it equates the two applications' levels, but an equation it cannot turn into an alias — against a level already generalized, or a maximum — stays two constraints. So the instantiation is pinned to what the goal already fixes, *before* the premises: a premise goal is stated in terms of these levels, so pinning first is what makes the same argument hold recursively for every witness the premises pull in. What the goal leaves open belongs to the declaration that raised it, whose finalization settles it with everything else the declaration said — an action read after this witness resolved may still bound a method's level. A goal that *defers* — the normal case for a declaration whose witness is registered later in the same unit — resolves long after its consumer finalized, and the level it mints then has nothing left to close it, so it is closed here (`Context::close_universe_instance` tells the two apart).
    if !minted.is_empty() {
        let terminal = reduce_with(context, &terminal)?;
        // A terminal that is not a concept application pins nothing, but once its consumer has finalized its levels still have to be closed, so the call is made either way.
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
    // A bound in the telescope is decided by the parameters the goal just pinned, so it is tried only now; one still waiting on a metavariable is parked like a premise on a flex key.
    for (slot, bound, provenance) in bounds {
        if attempt_discharge(context, slot, &bound, &provenance)? && !context.parking_suppressed() {
            context.park(
                ParkedWork::Discharge {
                    slot,
                    bound,
                    provenance,
                },
                origin.clone(),
            );
        }
    }

    let term = match args.is_empty() {
        true => head,
        false => Term::apply_marked(head, args),
    };

    context.end_solutions(mark);
    Ok(Resolution::Solved(term))
}

/// Attempt a freshly minted witness goal: solve it now, park it on a flex key, or defer it on a missing table entry. A definite failure is an error at `origin`'s span. A slot unification has already solved is left as it stands.
pub(crate) fn attempt_witness_goal(
    context: &mut Context,
    slot: MetavarId,
    goal: &Term,
    provenance: WitnessOrigin,
    origin: &Term,
) -> Result<(), Error> {
    if context.metavar_solution(slot).is_some() {
        return Ok(());
    }

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
            context.defer_witness(ParkedProblem {
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
        Resolution::Poisoned => Err(Error::Poisoned),
        Resolution::NoMatch => {
            Err(
                no_witness_error(context, goal, &provenance, origin.span().as_ref())
                    .at_opt(origin.span()),
            )
        }
    }
}

/// Retry a parked or deferred witness goal under its frozen frame. Called by `retry_parked`'s wake path and the deferred-goal sweeps. A goal whose slot was solved while it waited — the metavariable it was parked on and the slot solved by one unification — is dropped, as [`attempt_witness_goal`] drops it.
pub(crate) fn retry_witness(
    context: &mut Context,
    slot: MetavarId,
    goal: Term,
    provenance: WitnessOrigin,
    origin: Term,
    frame: FrozenFrame,
) -> Result<(), Error> {
    if context.metavar_solution(slot).is_some() {
        return Ok(());
    }

    let resolution =
        context.with_retry_frame(&frame, |context| resolve_witness(context, &goal, &origin))?;

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
            context.defer_witness(ParkedProblem {
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
        Resolution::Poisoned => Err(Error::Poisoned),
        Resolution::NoMatch => {
            Err(
                no_witness_error(context, &goal, &provenance, origin.span().as_ref())
                    .at_opt(origin.span()),
            )
        }
    }
}

/// Retry every deferred witness goal — after an item, when new witnesses may have registered. Goals that stay unresolvable re-defer; solutions that land wake parked constraints. A goal that fails is handed back attributed to the item that raised it rather than raised here, and the goals after it are still retried: that item has finalized, so its failure is its own and stops nothing else's retry. The error raised is the current item's — a parked constraint of its own that a landed solution woke and refused.
pub(crate) fn retry_deferred_witnesses(
    context: &mut Context,
) -> Result<Vec<DeferredRefusal>, Error> {
    let deferred = context.take_deferred_witnesses();
    if deferred.is_empty() {
        return Ok(Vec::new());
    }

    let mut refusals = Vec::new();
    for (item, parked) in deferred {
        let ParkedProblem {
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
        // As the item that raised it, so a re-deferral keeps that attribution.
        let retried = context.with_item(item, |context| {
            retry_witness(context, slot, goal, provenance, origin, frame)
        });
        if let Err(error) = retried {
            refusals.push(DeferredRefusal { item, error });
            continue;
        }

        // The deferred store is retried only *between* items, so the declaration that raised this goal has already finalized: its universe scheme is fixed, and no later pass will generalize or minimize a level introduced now. `instantiate` pins the witness's instance against the goal for exactly that reason. Check it held. Without this, a level that slips through is reported by `zonk` at the end of the module as an anonymous `?uN` escaping, naming neither the goal that introduced it nor the witness it came from.
        if let Some(solution) = context.metavar_solution(slot).cloned()
            && solution.any_universe_meta(|meta| {
                context
                    .universes()
                    .zonk(&Level::meta(meta))
                    .is_ok_and(|level| !level.metas().collect::<Vec<_>>().is_empty())
            })
        {
            refusals.push(DeferredRefusal {
                item,
                error: Error::UniverseInvariant(format!(
                    "a deferred witness left an unsolved universe level in {solution}"
                )),
            });
        }
    }

    context.retry_parked()?;

    Ok(refusals)
}

/// The end-of-module sweep: retry once more, then hand back every survivor as its item's refusal — the whole program has elaborated, so a still-missing table entry will never register.
pub(crate) fn finish_deferred_witnesses(
    context: &mut Context,
) -> Result<Vec<DeferredRefusal>, Error> {
    let mut refusals = retry_deferred_witnesses(context)?;

    for (item, parked) in context.take_deferred_witnesses() {
        let ParkedProblem {
            work:
                ParkedWork::Witness {
                    slot,
                    goal,
                    provenance,
                },
            origin,
            ..
        } = parked
        else {
            unreachable!("only witness goals defer");
        };
        // The retry above woke the parked constraints, and one of them may have solved this slot after its goal was deferred again.
        if context.metavar_solution(slot).is_some() {
            continue;
        }
        refusals.push(DeferredRefusal {
            item,
            error: no_witness_error(context, &goal, &provenance, origin.span().as_ref())
                .at_opt(origin.span()),
        });
    }

    Ok(refusals)
}

/// What a witness declaration's type says: its telescope with every binder opened, the concept it witnesses with that application's parameters, and the key it registers under — the tuple of rigid heads of those parameters.
pub(crate) struct WitnessSignature {
    /// Each binder as plicity, opened free variable and declared type, in written order.
    pub(crate) binders: Vec<(Plicity, Free, Term)>,
    pub(crate) concept: Global,
    /// The concept application's parameters, under the opened binders.
    pub(crate) params: Vec<Term>,
    pub(crate) key: WitnessKey,
}

/// Read `signature` as a witness declaration's type; see [`WitnessSignature`]. What a key *is* is defined here and nowhere else, which is what lets a second caller holding a witness's signature answer the question without restating the answer — `recovery::withhold`, which poisons the key of a witness its item never got to register.
///
/// `signature` is *elaborated*, and must be: reduction leaves a lowered concept application as the `Apply` it was written as, since a concept's name is rigid and unfolds to no `StructType` — building the node the heads are read off is elaboration's, not reduction's. Every refusal below is one `register_witness` raises, and a caller that only wants the key discards them.
pub(crate) fn read_witness_signature(
    context: &mut Context,
    signature: &Term,
) -> Result<WitnessSignature, Error> {
    let reduced = reduce_with(context, signature)?;

    // Peel the telescope, opening each binder with its own label as a neutral free variable (elaborated binder labels are entropy-fresh, so they cannot collide).
    let mut binders: Vec<(Plicity, Free, Term)> = Vec::new();
    let terminal = match &*reduced {
        Subterm::FuncType(ft) => {
            let mut cursor = ft.telescope.cursor();
            while let Some(plicity) = cursor.mark() {
                let (_, ty) = cursor.entry().expect("a mark stands at an entry");
                if matches!(plicity, Plicity::Explicit) {
                    return Err(Error::ExplicitWitnessParam);
                }
                let binder = cursor.advance_fresh(|hint| context.fresh(hint));
                binders.push((plicity, binder, ty));
            }
            cursor.body().expect("a cursor past every entry")
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
        return Err(Error::witness_not_a_concept(terminal.clone()));
    };
    if context.concept(concept_name).is_none() {
        return Err(Error::witness_not_a_concept(terminal.clone()));
    }

    // Key on every parameter: each must reduce to a rigid, keyable head. A parameterless concept has no head to key on at all — it is supplied through a local `use` binder, never the global table.
    if params.is_empty() {
        return Err(Error::parameterless_witness_concept(concept_name.symbol()));
    }
    let mut heads = Vec::with_capacity(params.len());
    for (position, param) in params.iter().enumerate() {
        let head = reduce_with(context, param)?;
        let Some(head) = HeadKey::of_whnf(&head) else {
            return Err(Error::invalid_witness_head(position, head));
        };
        heads.push(head);
    }

    Ok(WitnessSignature {
        binders,
        concept: *concept_name,
        params: params.clone(),
        key: WitnessKey(heads),
    })
}

/// Register an elaborated definition as a witness: validate its telescope (no explicit binders, regular premises), key it on the tuple of rigid heads of the concept's parameters, and insert it into the program-wide table — rejecting a duplicate key. `signature` is the definition's *elaborated* type, and `module` its declaring `Definition`'s own island, which the orphan-rule check below resolves to a mount. Registration ignores `pub`: visibility governs the name, never table membership.
pub(crate) fn register_witness(
    context: &mut Context,
    name: &Global,
    signature: &Term,
    universe_context: UniverseContext,
    module: &Qualifier,
) -> Result<(), Error> {
    let WitnessSignature {
        binders,
        concept: concept_name,
        params,
        key,
    } = read_witness_signature(context, signature)?;

    // Termination (Paterson's conditions): every `use` premise is strictly smaller than the concept application it serves — its variables are this witness's own binders, none of them occurs more often than in the head, and it has fewer nodes in all — so resolution through it is structurally decreasing, with no fuel or tabling. A premise may name a constant beside a binder, `Lift(Io, M)` under a head `Lift(Io, (A) => Try(M, E, A))`, which a variables-only rule would refuse for nothing: the constant weighs one node and decreases like any other.
    let binder_names: BTreeSet<&Free> = binders.iter().map(|(_, n, _)| n).collect();
    let (head_size, head_occurrences) = measure(&params, &binder_names);
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
            return Err(Error::non_regular_witness_premise(premise.clone()));
        };
        if context.concept(premise_concept).is_none() {
            return Err(Error::non_regular_witness_premise(premise.clone()));
        }
        let bound = premise_args.iter().all(|arg| {
            arg.free_vars_shared()
                .iter()
                .all(|free| matches!(free, Free::Global(_)) || binder_names.contains(free))
        });
        let (premise_size, premise_occurrences) = measure(premise_args, &binder_names);
        let decreasing = premise_size < head_size
            && premise_occurrences.iter().all(|(free, count)| {
                head_occurrences
                    .get(free)
                    .is_some_and(|in_head| count <= in_head)
            });
        if !(bound && decreasing) {
            return Err(Error::non_regular_witness_premise(premise.clone()));
        }
    }

    // The orphan rule: a witness may be declared only where the concept it witnesses, or at least one rigid type in its key, is already declared — never by a third root unrelated to both. Without this, two unrelated roots could each legally `satisfy` the same `(concept, key)` pair, a collision that is unfixable once both are linked into one program (see `documentation/design/surface/concepts-resolve-with-global-coherence.md`). Checked before the duplicate-key insert below: "not allowed to register this at all" is the more fundamental violation than "and it also collides."
    //
    // **No root is exempt.** Exempting `/std`, on the grounds that it and `/sys` are one coordinated standard library rather than independent packages, would decide nothing: every concept `/std` witnesses is `/std`'s own, so clause one admits every one of its witnesses on its own terms, including the tuple-keyed ones and the ones keyed on a `/sys`-homed carrier whose head owns nothing. An exemption that decides nothing is a rule nobody can check.
    //
    // Ownership is compared by *mount prefix*, which is what makes the rule bite between two ordinary units at all: two packages are two mounts, which is exactly where two independent authors could collide.
    //
    // A declaration owned by no mount matches nothing, including another unowned one: `owns` answers `false` unless both sides name a mount. That is the conservative direction — such a witness can only be refused, never admitted by an accidental `None == None`.
    let declaring = context.mount_of(&Global::Authored(*module));
    let owns = |other: Option<&Mount>| match (declaring, other) {
        (Some(here), Some(there)) => here.prefix == there.prefix,
        _ => false,
    };
    if !owns(context.mount_of(&concept_name))
        && !key.0.iter().any(|head| owns(context.mount_of_head(head)))
    {
        return Err(Error::orphan_witness(concept_name, key, *module));
    }

    if let Some(first_module) = context.insert_witness(
        concept_name,
        key.clone(),
        Witness {
            name: *name,
            module: *module,
            universe_context,
            signature: signature.clone(),
        },
    ) {
        return Err(Error::duplicate_witness(
            concept_name,
            key,
            first_module,
            *module,
        ));
    }

    Ok(())
}

/// Paterson's measure of a concept application's arguments: their node count and how often each of the witness's own binders occurs in them, counted per path rather than per shared node so a subterm used twice weighs twice — the size a witness head must strictly exceed each of its premises in.
fn measure(args: &[Term], binders: &BTreeSet<&Free>) -> (usize, BTreeMap<Free, usize>) {
    let mut size = 0;
    let mut occurrences = BTreeMap::new();

    for arg in args {
        let (arg_size, arg_occurrences) = arg.walk(
            &mut (),
            |_, _| Enter::<(usize, BTreeMap<Free, usize>)>::Descend,
            |_, term, children| {
                let mut size = 1;
                let mut occurrences = BTreeMap::new();

                for (child_size, child_occurrences) in children {
                    size += child_size;

                    for (free, count) in child_occurrences {
                        *occurrences.entry(free).or_insert(0) += count;
                    }
                }

                if let Subterm::Var(var) = &**term
                    && let Some(free) = var.as_free()
                    && binders.contains(free)
                {
                    *occurrences.entry(*free).or_insert(0) += 1;
                }

                (size, occurrences)
            },
        );

        size += arg_size;

        for (free, count) in arg_occurrences {
            *occurrences.entry(free).or_insert(0) += count;
        }
    }

    (size, occurrences)
}

/// What each superclass edge of the concept `name` reaches: the concept its `use` field's type reduces to an application of. Read under the frame the fields were checked in, since an edge's type may name the concept's parameters.
///
/// Where an edge stands is its field's mark; this is the other half, and it is read off the elaborated type rather than the written head, so an alias of a concept application is an edge, as it is a premise.
pub(crate) fn superclass_targets(
    context: &mut Context,
    name: &Global,
    edges: &[Term],
) -> Result<Vec<Global>, Error> {
    edges
        .iter()
        .map(|edge| {
            let reduced = reduce_with(context, edge)?;
            match &*reduced {
                Subterm::StructType(reached) if context.concept(&reached.name).is_some() => {
                    Ok(reached.name)
                }
                _ => {
                    Err(Error::unknown_superclass(name.symbol(), edge.clone()).at_opt(edge.span()))
                }
            }
        })
        .collect()
}

/// Record what the superclass edges of the concept `name` reach, and hold the superclass graph to its rule with them in it.
pub(crate) fn record_superclasses(
    context: &mut Context,
    name: &Global,
    supers: Vec<Global>,
) -> Result<(), Error> {
    let Some(concept) = context.concept(name).cloned() else {
        return Ok(());
    };
    context.update_concept(name, ConceptDecl { supers, ..concept });

    check_concept_registry(context)
}

/// Validate the concept registry as it stands: every superclass edge recorded reaches a registered concept, and the graph they form is acyclic. A concept whose fields are not elaborated yet has no edge recorded, and is held when its own are ([`record_superclasses`]) — so a cycle is refused where its last edge arrives.
pub(crate) fn check_concept_registry(context: &Context) -> Result<(), Error> {
    let concepts = context.concepts();

    for (name, concept) in concepts {
        for target in &concept.supers {
            if !concepts.contains_key(target) {
                return Err(Error::unknown_superclass(
                    name.symbol(),
                    Term::free_var(&Free::Global(*target)),
                ));
            }
        }
    }

    // Three-color DFS over the superclass edges. A direct self-edge is just the shortest cycle: it revisits its own name while `visiting` still holds it, so no separate check precedes the walk. Every target was proven present above, which is what lets the lookup expect.
    fn visit(
        concepts: &BTreeMap<Global, ConceptDecl>,
        name: &Global,
        visiting: &mut BTreeSet<Global>,
        done: &mut BTreeSet<Global>,
    ) -> Result<(), Error> {
        if done.contains(name) {
            return Ok(());
        }
        if !visiting.insert(*name) {
            return Err(Error::cyclic_superclass(name.symbol()));
        }
        let concept = concepts
            .get(name)
            .expect("every superclass target is a registered concept");
        for target in &concept.supers {
            visit(concepts, target, visiting, done)?;
        }
        visiting.remove(name);
        done.insert(*name);
        Ok(())
    }

    let mut visiting = BTreeSet::new();
    let mut done = BTreeSet::new();
    for name in concepts.keys() {
        visit(concepts, name, &mut visiting, &mut done)?;
    }

    Ok(())
}
