#[cfg(test)]
mod tests;

use {
    super::{BinderTypes, Context, Error, GoalReport, UniverseSolver, universe_context_validate},
    curios_core::{
        Apply, Bound, Carrier, Cases, ConceptDecl, Definition, DefinitionKind, Free, Func,
        FuncType, Global, InductDecl, InductParam, InductType, Intrinsic, Item, Let, LetBinding,
        Level, LevelHead, Match, MetaId, Metavar, MetavarOrigin, Module, Nat, Proj, Rec, RecGroup,
        RecItem, RecMemberScopes, Struct, StructDecl, StructType, Subterm, Telescope, Term, Tuple,
        TupleType, UniverseContext, UniverseError, UniverseInst, UniverseMetaId, Variant, Visit,
        project_erased_universes, rewrite_universe_levels_scoped, shift_universe_params,
        universe_metas,
    },
    curios_utilities::{Grain, Span},
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet},
        rc::Rc,
        sync::Arc,
    },
};

/// Substitute every solved metavariable in `term` by its (recursively zonked) solution, yielding a meta-free term. An unsolved metavariable is a residual hole the elaborator never pinned down — reported as `cannot_infer` at its occurrence span. The result flows downstream to `erase`, which is then guaranteed never to meet a `Subterm::Metavar`.
///
/// Substitution replaces a metavariable node by its solution. A solution is spelled with the birth telescope's names and is *not* in general a closed term; the occurrence's spine (its delayed substitution) records what each birth binder corresponds to at the splice site, so `zonk_term` resolves by rewriting the solution through it. Every solved occurrence carries its spine — `elaborate_apply` opens telescopes with rebuilt arguments, so no bare copy of a birthed hole survives to be spliced.
pub(crate) fn zonk(context: &Context, term: &Term) -> Result<Term, Error> {
    zonk_term(context, term)
}

/// Materialize the solutions already committed for term metavariables without rejecting holes that remain legitimately deferred. Universe levels are preserved verbatim: declaration finalization rewrites them only after this pass has exposed the levels hidden in solved term-meta solutions.
pub(crate) fn zonk_solved_term_metas<B: Bound>(context: &Context, value: &B) -> B {
    if !value.has_metavar() {
        return value.clone();
    }

    type Solution = (Vec<Free>, Term);

    fn materialize<B: Bound>(value: &B, solutions: Rc<BTreeMap<MetaId, Solution>>) -> B {
        let rewrite_solutions = Rc::clone(&solutions);
        let mut visit = Visit::rewriting(
            |_, _| None,
            Box::new(move |_, term| {
                let Subterm::Metavar(Metavar { id, spine, origin }) = &**term else {
                    return None;
                };
                if matches!(origin, Some(MetavarOrigin::Goal)) {
                    return None;
                }
                let (labels, solution) = rewrite_solutions.get(id)?;
                assert_eq!(
                    spine.len(),
                    labels.len(),
                    "a solved metavariable's occurrence carries its full spine"
                );
                let resolved = materialize(solution, Rc::clone(&rewrite_solutions));
                let spine = spine
                    .iter()
                    .map(|term| materialize(term, Rc::clone(&rewrite_solutions)))
                    .collect::<Vec<_>>();
                let labels = labels.iter().collect::<Vec<_>>();
                let spine = spine.iter().collect::<Vec<_>>();
                let zonked = resolved.capture(&labels).release(&spine);
                Some(match term.span() {
                    Some(span) => zonked.with_span(span),
                    None => zonked,
                })
            }),
        );
        value.traverse(&mut visit)
    }

    let found = Rc::new(RefCell::new(BTreeSet::new()));
    let collected = Rc::clone(&found);
    let mut visit = Visit::rewriting(
        |_, _| None,
        Box::new(move |_, term| {
            collected.borrow_mut().extend(term.metavars());
            None
        }),
    );
    let _: B = value.traverse(&mut visit);
    let mut solutions = BTreeMap::new();
    let mut seen = BTreeSet::new();
    let mut pending = found.borrow().iter().copied().collect::<Vec<_>>();
    while let Some(id) = pending.pop() {
        if !seen.insert(id) {
            continue;
        }
        let Some(entry) = context.metavar_entry(id) else {
            continue;
        };
        let Some(solution) = &entry.solution else {
            continue;
        };
        pending.extend(solution.metavars());
        solutions.insert(
            id,
            (
                entry
                    .telescope
                    .iter()
                    .map(|(name, _)| name.clone())
                    .collect(),
                solution.clone(),
            ),
        );
    }
    materialize(value, Rc::new(solutions))
}

/// Zonk a whole [`Module`]: substitute metavariable solutions throughout every top-level item plus the entrypoint body and annotation, yielding a meta-free module for `erase`.
pub fn zonk_module(context: &Context, module: &Module) -> Result<Module, Error> {
    curios_profile::profile!("zonk_module");
    let items = module
        .items
        .iter()
        .map(|item| zonk_item(context, item))
        .collect::<Result<Vec<_>, Error>>()?;

    let body = module
        .body
        .as_ref()
        .map(|body| zonk_term(context, body))
        .transpose()?;

    let type_ = module
        .type_
        .as_ref()
        .map(|type_| zonk_term(context, type_))
        .transpose()?;

    // The registry's telescopes flow into `erase`, which runs meta-free with its own (solution-less) context — so they are zonked like everything else.
    let induct_decls = module
        .induct_decls
        .iter()
        .map(|(name, induct_decl)| {
            Ok((
                name.clone(),
                InductDecl {
                    universe_context: induct_decl.universe_context.clone(),
                    arity: zonk_arity(context, &induct_decl.arity)?,
                    constructors: induct_decl
                        .constructors
                        .iter()
                        .map(|(tag, param)| {
                            Ok((
                                tag.clone(),
                                InductParam {
                                    telescope: zonk_signature(context, &param.telescope)?,
                                    plicities: param.plicities.clone(),
                                },
                            ))
                        })
                        .collect::<Result<_, Error>>()?,
                    result_sort: zonk_term(context, &induct_decl.result_sort)?,
                    module: induct_decl.module.clone(),
                    rep_public: induct_decl.rep_public,
                    polarities: induct_decl.polarities.clone(),
                },
            ))
        })
        .collect::<Result<_, Error>>()?;

    // Struct field telescopes flow into `erase` the same way — zonk them too.
    let struct_decls = module
        .struct_decls
        .iter()
        .map(|(name, struct_decl)| {
            Ok((
                name.clone(),
                StructDecl {
                    universe_context: struct_decl.universe_context.clone(),
                    arity: zonk_arity(context, &struct_decl.arity)?,
                    result_sort: zonk_term(context, &struct_decl.result_sort)?,
                    module: struct_decl.module.clone(),
                    rep_public: struct_decl.rep_public,
                    polarities: struct_decl.polarities.clone(),
                },
            ))
        })
        .collect::<Result<_, Error>>()?;

    let module = Module {
        items,
        mounts: module.mounts.clone(),
        universe_seeds: Vec::new(),
        induct_decls,
        struct_decls,
        // Witness markers carry no terms; a concept carries only its parameter telescope, zonked here — its field telescopes live in `struct_decls`, zonked above.
        concepts: module
            .concepts
            .iter()
            .map(|(name, concept)| {
                Ok((
                    name.clone(),
                    ConceptDecl {
                        universe_context: concept.universe_context.clone(),
                        params: zonk_field_telescope(context, &concept.params)?,
                        fields: concept.fields.clone(),
                        supers: concept.supers.clone(),
                    },
                ))
            })
            .collect::<Result<_, Error>>()?,
        witnesses: module.witnesses.clone(),
        binder_floor: module.binder_floor,
        type_,
        body,
    };
    validate_universes(&module)?;
    Ok(module)
}

pub(crate) fn validate_bound_universes<B: Bound>(
    value: &B,
    parameter_count: usize,
    owner: &str,
) -> Result<(), Error> {
    let nested_error = Rc::new(RefCell::new(None));
    let error = Rc::clone(&nested_error);
    let mut visit = Visit::rewriting_universes(
        |_, _| None,
        Box::new(move |_, term| {
            let contexts: Vec<&UniverseContext> = match &**term {
                Subterm::Rec(rec) => vec![rec.group.universe_context()],
                _ => Vec::new(),
            };
            for context in contexts {
                if error.borrow().is_none()
                    && let Err(found) = universe_context_validate(context)
                {
                    *error.borrow_mut() = Some(found);
                }
            }
            None
        }),
    );
    let _: B = value.traverse(&mut visit);
    if let Some(error) = nested_error.borrow_mut().take() {
        return Err(Error::UniverseInvariant(format!(
            "{owner}: nested universe context is invalid: {error}"
        )));
    }

    let owner = owner.to_string();
    let _: B = rewrite_universe_levels_scoped(value, move |depth, level| {
        let visible_parameter_count = depth
            .checked_add(parameter_count)
            .ok_or_else(|| format!("{owner}: universe binder depth overflow"))?;
        if level.is_closed(visible_parameter_count) {
            Ok(level.clone())
        } else {
            // Name the numbers. A level prints its parameters as letters that cycle (`u`..`z`, then `u1`), so neither the offending index nor the bound it broke can be read off `{level}`, and the bound itself is a sum: a nested scheme's own parameters sit innermost and the enclosing binders it may still reference follow. Which of the two is short is the whole diagnosis, and recovering it otherwise costs a rebuild.
            let detail = if level.metas().next().is_some() {
                "it still holds an unsolved universe metavariable".to_string()
            } else {
                let escaping = level
                    .params()
                    .filter(|param| param.0 >= visible_parameter_count)
                    .map(|param| param.0.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "parameter index {escaping} is not below {visible_parameter_count} \
                     ({parameter_count} declared, {depth} from enclosing universe binders)"
                )
            };
            Err(format!(
                "{owner}: level {level} escapes its universe parameter context: {detail}"
            ))
        }
    })
    .map_err(Error::UniverseInvariant)?;
    Ok(())
}

fn validate_instance_arities<B: Bound>(
    value: &B,
    definitions: &BTreeMap<Global, usize>,
    inducts: &BTreeMap<Global, usize>,
    structs: &BTreeMap<Global, usize>,
    owner: &str,
) -> Result<(), Error> {
    let definitions = definitions.clone();
    let inducts = inducts.clone();
    let structs = structs.clone();
    let owner = owner.to_string();
    let error = Rc::new(RefCell::new(None));
    let found = Rc::clone(&error);
    // Inspection only — the hook always returns `None` and the rebuilt value is discarded. Memoized on node identity so a structurally shared term is checked once per distinct node rather than once per occurrence: an unmemoized rewrite here rebuilt, and immediately dropped, one node per occurrence, which on a lowered string literal is O(n^2) of pure garbage.
    let mut visit = Visit::rewriting_shared(
        |_, _| None,
        Box::new(move |_, term| {
            if found.borrow().is_some() {
                return None;
            }
            let mismatch = |kind: &str, name: &str, expected: usize, got: usize| {
                (expected != got).then(|| {
                    format!(
                        "{owner}: {kind} {name} has {got} universe arguments but its context expects {expected}"
                    )
                })
            };
            let invalid = match &**term {
                Subterm::UniverseInst(instance) => match instance.head.as_rec_proj() {
                    Some((group, _)) => mismatch(
                        "recursive instance",
                        "<local rec>",
                        group.universe_context().parameter_count,
                        instance.levels.len(),
                    ),
                    None => match &*instance.head {
                        Subterm::Var(var) => {
                            var.as_free().and_then(Free::as_global).and_then(|name| {
                                definitions.get(name).and_then(|expected| {
                                    mismatch(
                                        "definition instance",
                                        &name.symbol(),
                                        *expected,
                                        instance.levels.len(),
                                    )
                                })
                            })
                        }
                        _ => Some(format!(
                            "{owner}: a universe instance wraps neither a variable nor a projection of a recursive group"
                        )),
                    },
                },
                Subterm::InductType(induct) => inducts.get(&induct.name).and_then(|expected| {
                    mismatch(
                        "inductive occurrence",
                        &induct.name.symbol(),
                        *expected,
                        induct.universes.len(),
                    )
                }),
                Subterm::Variant(variant) => inducts.get(&variant.name).and_then(|expected| {
                    mismatch(
                        "constructor occurrence",
                        &variant.name.symbol(),
                        *expected,
                        variant.universes.len(),
                    )
                }),
                Subterm::StructType(struct_) => structs.get(&struct_.name).and_then(|expected| {
                    mismatch(
                        "structure occurrence",
                        &struct_.name.symbol(),
                        *expected,
                        struct_.universes.len(),
                    )
                }),
                Subterm::Struct(struct_) => structs.get(&struct_.name).and_then(|expected| {
                    mismatch(
                        "structure value",
                        &struct_.name.symbol(),
                        *expected,
                        struct_.universes.len(),
                    )
                }),
                _ => None,
            };
            if let Some(invalid) = invalid {
                *found.borrow_mut() = Some(invalid);
            }
            None
        }),
    );
    let _: B = value.traverse(&mut visit);
    match error.borrow_mut().take() {
        Some(error) => Err(Error::UniverseInvariant(error)),
        None => Ok(()),
    }
}

fn validate_module_instance_arities(module: &Module) -> Result<(), Error> {
    let mut definition_schemes = BTreeMap::new();
    for item in &module.items {
        match item {
            Item::Let(definition) => {
                definition_schemes.insert(
                    definition.name.clone(),
                    (definition.kind.clone(), definition.universe_context.clone()),
                );
            }
            Item::Rec(rec) => {
                for definition in rec.definitions() {
                    definition_schemes.insert(
                        definition.name.clone(),
                        (definition.kind, definition.universe_context),
                    );
                }
            }
        }
    }
    let definitions = definition_schemes
        .iter()
        .map(|(name, (_, context))| (name.clone(), context.parameter_count))
        .collect::<BTreeMap<_, _>>();
    let inducts = module
        .induct_decls
        .iter()
        .map(|(name, declaration)| (name.clone(), declaration.universe_context.parameter_count))
        .collect::<BTreeMap<_, _>>();
    let structs = module
        .struct_decls
        .iter()
        .map(|(name, declaration)| (name.clone(), declaration.universe_context.parameter_count))
        .collect::<BTreeMap<_, _>>();
    macro_rules! validate {
        ($value:expr, $owner:expr $(,)?) => {
            validate_instance_arities($value, &definitions, &inducts, &structs, $owner)
        };
    }

    for item in &module.items {
        for definition in item.definitions() {
            validate!(
                &definition.type_,
                &format!("definition {} type", definition.name),
            )?;
            validate!(
                &definition.body,
                &format!("definition {} body", definition.name),
            )?;
        }
    }
    for (name, declaration) in &module.induct_decls {
        validate!(&declaration.arity, &format!("inductive {name} arity"))?;
        validate!(
            &declaration.result_sort,
            &format!("inductive {name} result sort"),
        )?;
        for (constructor, signature) in &declaration.constructors {
            validate!(
                &signature.telescope,
                &format!("inductive {name} constructor {constructor}"),
            )?;
        }

        if let Some((kind, actual)) = definition_schemes.get(name)
            && (kind != &DefinitionKind::InductiveType || actual != &declaration.universe_context)
        {
            return Err(Error::UniverseInvariant(format!(
                "inductive {name} and its type-former definition have different universe contexts"
            )));
        }
    }
    for (name, declaration) in &module.struct_decls {
        validate!(&declaration.arity, &format!("structure {name} arity"))?;
        validate!(
            &declaration.result_sort,
            &format!("structure {name} result sort"),
        )?;
        if let Some((kind, actual)) = definition_schemes.get(name)
            && (!matches!(
                kind,
                DefinitionKind::StructType | DefinitionKind::ConceptType
            ) || actual != &declaration.universe_context)
        {
            return Err(Error::UniverseInvariant(format!(
                "structure {name} and its type-former definition have different universe contexts"
            )));
        }
    }
    for (name, concept) in &module.concepts {
        validate!(&concept.params, &format!("concept {name} parameters"))?;
        let Some(struct_decl) = module.struct_decls.get(name) else {
            return Err(Error::UniverseInvariant(format!(
                "concept {name} has no structure registry entry"
            )));
        };
        if concept.universe_context != struct_decl.universe_context {
            return Err(Error::UniverseInvariant(format!(
                "concept {name} and its structure registry have different universe contexts"
            )));
        }
    }
    // Both directions of the generated-member correspondence, read off the definition's own recorded origin. A constructor names the inductive it belongs to *and* which case it is, so this checks the registry entry exists and actually declares that case — without joining an owner and a tag back into a name to look up.
    for (name, (kind, _)) in &definition_schemes {
        match kind {
            DefinitionKind::InductiveConstructor { owner, tag } => {
                let owner = Global::Authored(owner.clone());
                let Some(declaration) = module.induct_decls.get(&owner) else {
                    return Err(Error::UniverseInvariant(format!(
                        "constructor definition {name} names missing owner {owner}"
                    )));
                };
                if !declaration.declares(tag) {
                    return Err(Error::UniverseInvariant(format!(
                        "constructor definition {name} names owner {owner}, which declares no case '{tag}'"
                    )));
                }
            }
            DefinitionKind::ConceptMethod { owner } => {
                let owner = Global::Authored(owner.clone());
                if !module.concepts.contains_key(&owner) {
                    return Err(Error::UniverseInvariant(format!(
                        "concept method {name} names missing owner {owner}"
                    )));
                }
            }
            DefinitionKind::Authored
            | DefinitionKind::InductiveType
            | DefinitionKind::StructType
            | DefinitionKind::ConceptType
            | DefinitionKind::Witness => {}
        }
    }
    if let Some(body) = &module.body {
        validate!(body, "module body")?;
    }
    if let Some(type_) = &module.type_ {
        validate!(type_, "module body annotation")?;
    }
    Ok(())
}

pub fn validate_universes(module: &Module) -> Result<(), Error> {
    for item in &module.items {
        for definition in item.definitions() {
            universe_context_validate(&definition.universe_context).map_err(|error| {
                Error::UniverseInvariant(format!(
                    "definition {} has an invalid universe context: {error}",
                    definition.name
                ))
            })?;
            let parameter_count = definition.universe_context.parameter_count;
            validate_bound_universes(
                &definition.type_,
                parameter_count,
                &format!("definition {} type", definition.name),
            )?;
            validate_bound_universes(
                &definition.body,
                parameter_count,
                &format!("definition {} body", definition.name),
            )?;
        }
    }
    for (name, induct_decl) in &module.induct_decls {
        universe_context_validate(&induct_decl.universe_context).map_err(|error| {
            Error::UniverseInvariant(format!(
                "inductive {name} has an invalid universe context: {error}"
            ))
        })?;
        let parameter_count = induct_decl.universe_context.parameter_count;
        validate_bound_universes(
            &induct_decl.arity,
            parameter_count,
            &format!("inductive {name} arity"),
        )?;
        validate_bound_universes(
            &induct_decl.result_sort,
            parameter_count,
            &format!("inductive {name} result sort"),
        )?;
        for (constructor_name, constructor) in &induct_decl.constructors {
            validate_bound_universes(
                &constructor.telescope,
                parameter_count,
                &format!("inductive {name} constructor {constructor_name}"),
            )?;
        }
    }
    for (name, struct_decl) in &module.struct_decls {
        universe_context_validate(&struct_decl.universe_context).map_err(|error| {
            Error::UniverseInvariant(format!(
                "structure {name} has an invalid universe context: {error}"
            ))
        })?;
        let parameter_count = struct_decl.universe_context.parameter_count;
        validate_bound_universes(
            &struct_decl.arity,
            parameter_count,
            &format!("structure {name} arity"),
        )?;
        validate_bound_universes(
            &struct_decl.result_sort,
            parameter_count,
            &format!("structure {name} result sort"),
        )?;
        if let Some(concept) = module.concepts.get(name)
            && concept.universe_context != struct_decl.universe_context
        {
            return Err(Error::UniverseInvariant(format!(
                "concept and structure universe contexts differ for {name}"
            )));
        }
    }
    for (name, concept) in &module.concepts {
        universe_context_validate(&concept.universe_context).map_err(|error| {
            Error::UniverseInvariant(format!(
                "concept {name} has an invalid universe context: {error}"
            ))
        })?;
        validate_bound_universes(
            &concept.params,
            concept.universe_context.parameter_count,
            &format!("concept {name} parameters"),
        )?;
    }
    if let Some(body) = &module.body {
        validate_bound_universes(body, 0, "module body")?;
    }
    if let Some(type_) = &module.type_ {
        validate_bound_universes(type_, 0, "module body annotation")?;
    }
    if !module.universe_seeds.is_empty() {
        return Err(Error::UniverseInvariant(
            "finalized module retained lowering-time universe seeds".into(),
        ));
    }
    validate_module_instance_arities(module)?;
    Ok(())
}

/// Validate the lowering-time universe allocator contract before replaying a prepared Text module. Every meta reachable from lowered Core must have a corresponding seed below the recorded allocator floor.
pub fn validate_lowered_universe_seeds(module: &Module, floor: usize) -> Result<(), Error> {
    if module.universe_seeds.len() != floor {
        return Err(Error::UniverseInvariant(format!(
            "lowered universe seed table has {} entries but its allocator floor is {floor}",
            module.universe_seeds.len()
        )));
    }

    let mut metas = BTreeSet::new();
    macro_rules! collect {
        ($value:expr) => {
            metas.extend(universe_metas($value))
        };
    }

    for item in &module.items {
        for definition in item.definitions() {
            collect!(&definition.type_);
            collect!(&definition.body);
            for constraint in &definition.universe_context.constraints {
                metas.extend(constraint.lower.metas());
                metas.extend(constraint.upper.metas());
            }
        }
    }
    for declaration in module.induct_decls.values() {
        collect!(&declaration.arity);
        collect!(&declaration.result_sort);
        for constructor in declaration.signatures() {
            collect!(&constructor.telescope);
        }
    }
    for declaration in module.struct_decls.values() {
        collect!(&declaration.arity);
        collect!(&declaration.result_sort);
    }
    for concept in module.concepts.values() {
        collect!(&concept.params);
    }
    if let Some(body) = &module.body {
        collect!(body);
    }
    if let Some(type_) = &module.type_ {
        collect!(type_);
    }

    if let Some(meta) = metas.into_iter().find(|meta| meta.0 >= floor) {
        return Err(Error::UniverseInvariant(format!(
            "lowered universe meta {meta} has no seed below allocator floor {floor}"
        )));
    }
    Ok(())
}

fn zonk_item(context: &Context, item: &Item) -> Result<Item, Error> {
    match item {
        Item::Let(def) => Ok(Item::Let(zonk_definition(context, def)?)),
        Item::Rec(rec) => Ok(Item::Rec(RecItem::try_new(
            rec.definitions()
                .iter()
                .map(|def| zonk_definition(context, def))
                .collect::<Result<Vec<_>, Error>>()?,
        )?)),
    }
}

fn zonk_definition(context: &Context, def: &Definition) -> Result<Definition, Error> {
    let type_ = zonk_term(context, &def.type_)?;
    let body = zonk_term(context, &def.body)?;
    Ok(Definition {
        name: def.name.clone(),
        kind: def.kind.clone(),
        universe_context: def.universe_context.clone(),
        island: def.island.clone(),
        totality: def.totality,
        type_,
        body,
    })
}

/// Collect every written goal one successful elaboration reached — the same set strict zonking would meet — as display-ready reports.
///
/// The walk mirrors [`zonk_module`]'s coverage in its order (items in declaration order, then the entrypoint body and annotation, then the registry telescopes that flow into erase), recording each `Goal`-origin metavariable once with its first occurrence's span. Goals can also hide inside committed solutions of ordinary metavariables the module references — strict zonk would splice through them — so referenced solutions are scanned transitively afterwards, in discovery order.
///
/// Each report's scope, type, and solution render through the tolerant [`zonk_solved_term_metas`], so committed substitutions appear while goal-origin and unsolved metavariables stay visible as neutral terms; universe instances are then erased ([`project_erased_universes`]) and operator witness projections folded back to infix, so every reported term is spelled the way the source could write it. An unsolved goal additionally carries sandboxed candidate suggestions ([`suggest_candidates`](super::suggest_candidates)), displayed through the same pipeline.
pub(crate) fn collect_goal_reports(context: &mut Context, module: &Module) -> Vec<GoalReport> {
    /// Collected goal sites in discovery order: each goal's id, its first occurrence's span, and its owning definition when it sits inside one (the suggestion pools exclude the owner), shared into the scan closures.
    type GoalSites = Rc<RefCell<Vec<(MetaId, Option<Span>, Option<Global>)>>>;

    let goals: GoalSites = Rc::new(RefCell::new(Vec::new()));
    let seen_goals = Rc::new(RefCell::new(BTreeSet::new()));
    // Append-only discovery log of ordinary metavariables, drained below by index so solution scans that append more keep a stable order.
    let referenced: Rc<RefCell<Vec<MetaId>>> = Rc::new(RefCell::new(Vec::new()));

    fn scan<B: Bound>(
        value: &B,
        owner: Option<&Global>,
        goals: &GoalSites,
        seen_goals: &Rc<RefCell<BTreeSet<MetaId>>>,
        referenced: &Rc<RefCell<Vec<MetaId>>>,
    ) {
        let goals = Rc::clone(goals);
        let seen_goals = Rc::clone(seen_goals);
        let referenced = Rc::clone(referenced);
        let owner = owner.cloned();
        let mut visit = Visit::rewriting(
            |_, _| None,
            Box::new(move |_, term| {
                if let Subterm::Metavar(Metavar { id, origin, .. }) = &**term {
                    match origin {
                        Some(MetavarOrigin::Goal) => {
                            if seen_goals.borrow_mut().insert(*id) {
                                goals.borrow_mut().push((*id, term.span(), owner.clone()));
                            }
                        }
                        _ => referenced.borrow_mut().push(*id),
                    }
                }
                None
            }),
        );
        let _: B = value.traverse(&mut visit);
    }

    let mut scan_definition = |def: &Definition| {
        scan(
            &def.type_,
            Some(&def.name),
            &goals,
            &seen_goals,
            &referenced,
        );
        scan(&def.body, Some(&def.name), &goals, &seen_goals, &referenced);
    };
    for item in &module.items {
        match item {
            Item::Let(def) => scan_definition(def),
            Item::Rec(rec) => rec.definitions().iter().for_each(&mut scan_definition),
        }
    }
    if let Some(body) = &module.body {
        scan(body, None, &goals, &seen_goals, &referenced);
    }
    if let Some(type_) = &module.type_ {
        scan(type_, None, &goals, &seen_goals, &referenced);
    }
    for induct_decl in module.induct_decls.values() {
        scan(&induct_decl.arity, None, &goals, &seen_goals, &referenced);
        for (_, param) in &induct_decl.constructors {
            scan(&param.telescope, None, &goals, &seen_goals, &referenced);
        }
        scan(
            &induct_decl.result_sort,
            None,
            &goals,
            &seen_goals,
            &referenced,
        );
    }
    for struct_decl in module.struct_decls.values() {
        scan(&struct_decl.arity, None, &goals, &seen_goals, &referenced);
        scan(
            &struct_decl.result_sort,
            None,
            &goals,
            &seen_goals,
            &referenced,
        );
    }
    for concept in module.concepts.values() {
        scan(&concept.params, None, &goals, &seen_goals, &referenced);
    }

    let mut scanned = BTreeSet::new();
    let mut index = 0;
    loop {
        let id = {
            let referenced = referenced.borrow();
            match referenced.get(index) {
                Some(id) => *id,
                None => break,
            }
        };
        index += 1;
        if !scanned.insert(id) {
            continue;
        }
        if let Some(solution) = context.metavar_solution(id) {
            scan(solution, None, &goals, &seen_goals, &referenced);
        }
    }

    // Suggestions run first, on the mutable context — each attempt sandboxed and rolled back — before the display phase borrows it immutably. Solved goals get none: a suggestion beside a `? =` answer is noise. `restore_budget` puts the attempts on the same footing as the finalization passes.
    let goal_sites: Vec<(MetaId, Option<Span>, Option<Global>)> = goals.borrow().clone();
    context.restore_budget();
    let mut all_candidates: Vec<Vec<Term>> = Vec::with_capacity(goal_sites.len());
    for (id, _, owner) in &goal_sites {
        if context.metavar_solution(*id).is_some() {
            all_candidates.push(Vec::new());
            continue;
        }
        let (telescope, result) = {
            let entry = context
                .metavar_entry(*id)
                .expect("a collected goal has a birth entry");
            (Rc::clone(&entry.telescope), entry.result.clone())
        };
        all_candidates.push(super::suggest_candidates(
            context,
            &telescope,
            &result,
            module,
            owner.as_ref(),
        ));
    }

    // Materialize committed substitutions tolerantly, erase universe instances (the surface language cannot even spell `.{…}`, so a report never shows one — solved or unsolved), then fold operator witness projections back to their infix spelling — solved witnesses arrive from the splice as globals, unsolved ones keep their origin, abstract ones are binders of the goal's own scope, and the fold handles all three.
    let operators = super::operator_table(context);
    let context = &*context;
    let display = |binders: &BinderTypes, term: &Term| {
        super::denoise_for_display(
            &operators,
            binders,
            &project_erased_universes(&zonk_solved_term_metas(context, term)),
        )
    };
    goal_sites
        .iter()
        .zip(all_candidates)
        .map(|((id, span, _), candidates)| {
            // Every goal occurrence in the elaborated module was rebuilt by `elaborate_metavar`, which births on first sight in either mode — so the entry exists.
            let entry = context
                .metavar_entry(*id)
                .expect("a collected goal has a birth entry");
            // The goal's own birth telescope is the scope every term in its report is spelled against, so it is also the scope an abstract witness resolves through.
            let binders: BinderTypes = Rc::new(
                entry
                    .telescope
                    .iter()
                    .map(|(name, type_)| (name.clone(), type_.clone()))
                    .collect(),
            );
            GoalReport {
                span: span.clone(),
                scope: entry
                    .telescope
                    .iter()
                    .map(|(name, type_)| (Term::free_var(name), display(&binders, type_)))
                    .collect(),
                goal: display(&binders, &entry.result),
                solution: context
                    .metavar_solution(*id)
                    .map(|term| display(&binders, term)),
                candidates: candidates
                    .iter()
                    .map(|term| display(&binders, term))
                    .collect(),
            }
        })
        .collect()
}

/// The report a written goal `?` errors out with: the local scope frozen at its birth, the goal's type, and the solution elaboration committed (if any) — each zonked for *display*, keeping the raw spelling where unsolved holes survive (the same tolerance the no-witness report uses).
fn goal_report(context: &Context, id: MetaId) -> Error {
    let display = |term: &Term| zonk_term(context, term).unwrap_or_else(|_| term.clone());
    match context.metavar_entry(id) {
        Some(entry) => Error::goal(
            entry
                .telescope
                .iter()
                .map(|(name, type_)| (Term::free_var(name), display(type_)))
                .collect(),
            display(&entry.result),
            context.metavar_solution(id).map(display),
        ),
        // A goal elaboration never reached was never birthed, so there is no scope or type to report — unreachable in practice, since every kept item elaborates.
        None => Error::CannotInfer,
    }
}

fn zonk_term(context: &Context, term: &Term) -> Result<Term, Error> {
    // A metavariable node *is* the substitution site: replace it by its solution, recursively zonked (the solution may itself mention solved metavariables).
    if let Subterm::Metavar(Metavar { id, spine, origin }) = &**term {
        // A written goal `?` never splices — the whole point of writing it was the report. Solved or not, error with what elaboration determined: the frozen scope, the goal's type, and the solution when one landed.
        if matches!(origin, Some(MetavarOrigin::Goal)) {
            return Err(goal_report(context, *id).at_opt(term.span()));
        }

        let solution = context.metavar_solution(*id).ok_or_else(|| {
            // An unsolved metavariable the *elaborator* minted (an omitted implicit or witness argument) is reported by the binder it filled — the provenance rides on the node itself — not as a bare hole: the user never wrote this metavariable, so a generic "cannot infer" would point at nothing they can see.
            let error = match origin {
                Some(MetavarOrigin::Implicit(origin)) => {
                    Error::uninferred_implicit(origin.func.clone(), origin.binder.clone())
                }
                Some(MetavarOrigin::Witness(origin)) => {
                    // The birth record's `result` is the goal type; display it through whatever solutions landed, keeping the raw spelling if holes survive.
                    let goal = context
                        .metavar_entry(*id)
                        .map(|entry| entry.result.clone())
                        .unwrap_or_else(Term::type_ground);
                    let goal = zonk_term(context, &goal).unwrap_or(goal);
                    // No embedding diagnosis on this path: zonk holds the context immutably, and a `Lift` goal that survives to the splice report has already been reported richer by the resolution drains.
                    Error::no_witness(goal, origin.func.clone(), origin.binder.clone(), None)
                }
                None => Error::CannotInfer,
                // Handled by the unconditional report above.
                Some(MetavarOrigin::Goal) => unreachable!("a goal never reaches the splice path"),
            };
            match term.span() {
                Some(span) => error.at(span),
                None => error,
            }
        })?;

        // Resolve the solution in its own (named) frame first: nested solved metavariables are substituted before the spine splice below.
        let resolved = zonk_term(context, solution)?;

        // A contextual occurrence: the spine records, in this site's own (already de-Bruijn-correct) form, what each birth binder corresponds to here. Zonk the spine entries (they may embed solved metavariables), then splice the solution through them — birth names captured, spine terms released.
        let entry = context
            .metavar_entry(*id)
            .expect("a solved metavariable has a birth entry");
        assert_eq!(
            spine.len(),
            entry.telescope.len(),
            "a solved metavariable's occurrence carries its full spine"
        );
        let binders = entry
            .telescope
            .iter()
            .map(|(name, _)| name)
            .collect::<Vec<_>>();
        let spine = zonk_terms(context, spine)?;
        let refs = spine.iter().collect::<Vec<_>>();
        let zonked = resolved.capture(&binders).release(&refs);

        // Carry the hole's span only if the solution carries none of its own.
        return Ok(match term.span() {
            Some(span) => zonked.with_span(span),
            None => zonked,
        });
    }

    // Fast path: a subtree with neither term nor universe metavariables is already fully zonked. A solved term metavariable's solution can be term-meta-free while still carrying levels that declaration finalization solved, so universe metas must keep descending.
    //
    // Both halves read cached per-node bits. Testing the metavariable *set* for emptiness instead answers the same question — the set is empty exactly when the bit is false — but re-walks the whole subtree at every level on the way down, which is quadratic on a deep spine.
    if !term.has_metavar() && !term.has_universe_meta() {
        return Ok(term.clone());
    }

    let inner = zonk_subterm(context, term)?;

    Ok(match term.span() {
        Some(span) => Term::spanned(span, inner),
        None => Term::from(inner),
    })
}

fn zonk_terms(context: &Context, terms: &[Term]) -> Result<Vec<Term>, Error> {
    terms.iter().map(|t| zonk_term(context, t)).collect()
}

fn zonk_levels(context: &Context, levels: &[Level]) -> Result<Vec<Level>, Error> {
    levels
        .iter()
        .map(|level| context.universes().zonk(level).map_err(Error::from))
        .collect()
}

fn zonk_subterm(context: &Context, term: &Term) -> Result<Subterm, Error> {
    Ok(match &**term {
        Subterm::Type(level) => {
            Subterm::Type(context.universes().zonk(level).map_err(Error::from)?)
        }
        Subterm::Prop => Subterm::Prop,
        Subterm::Foreign(function, args) => {
            Subterm::Foreign(Arc::clone(function), zonk_terms(context, args)?)
        }
        Subterm::Var(var) => Subterm::Var(var.clone()),
        Subterm::UniverseInst(instance) => Subterm::UniverseInst(UniverseInst {
            head: zonk_term(context, &instance.head)?,
            levels: instance
                .levels
                .iter()
                .map(|level| context.universes().zonk(level).map_err(Error::from))
                .collect::<Result<_, _>>()?,
        }),

        // Transients are consumed by `elaborate` before zonk ever runs.
        Subterm::Transient(_) => unreachable!("transient node survived elaboration into zonk"),

        Subterm::Intrinsic(intrinsic) => Subterm::Intrinsic(zonk_intrinsic(context, intrinsic)?),

        Subterm::Func(Func {
            telescope,
            plicities,
        }) => Subterm::Func(Func {
            telescope: zonk_telescope(context, telescope)?,
            plicities: plicities.clone(),
        }),

        Subterm::FuncType(FuncType {
            telescope,
            plicities,
        }) => Subterm::FuncType(FuncType {
            telescope: zonk_telescope(context, telescope)?,
            plicities: plicities.clone(),
        }),

        Subterm::Apply(Apply {
            head,
            params,
            plicities,
        }) => Subterm::Apply(Apply {
            head: zonk_term(context, head)?,
            params: zonk_terms(context, params)?,
            plicities: plicities.clone(),
        }),

        Subterm::TupleType(TupleType { telescope }) => Subterm::TupleType(TupleType {
            telescope: zonk_field_telescope(context, telescope)?,
        }),

        Subterm::Tuple(Tuple { fields, names }) => Subterm::Tuple(Tuple {
            fields: zonk_terms(context, fields)?,
            names: names.clone(),
        }),

        Subterm::Proj(Proj { head, field }) => Subterm::Proj(Proj {
            head: zonk_term(context, head)?,
            field: field.clone(),
        }),

        Subterm::InductType(InductType {
            name,
            universes,
            params,
            indices,
        }) => Subterm::InductType(InductType {
            name: name.clone(),
            universes: zonk_levels(context, universes)?,
            params: zonk_terms(context, params)?,
            indices: zonk_terms(context, indices)?,
        }),

        Subterm::Variant(Variant {
            name,
            universes,
            params,
            tag,
            payload,
        }) => Subterm::Variant(Variant {
            name: name.clone(),
            universes: zonk_levels(context, universes)?,
            params: zonk_terms(context, params)?,
            tag: tag.clone(),
            payload: zonk_terms(context, payload)?,
        }),

        Subterm::StructType(StructType {
            name,
            universes,
            params,
        }) => Subterm::StructType(StructType {
            name: name.clone(),
            universes: zonk_levels(context, universes)?,
            params: zonk_terms(context, params)?,
        }),

        Subterm::Struct(Struct {
            name,
            universes,
            params,
            fields,
            entries,
        }) => Subterm::Struct(Struct {
            name: name.clone(),
            universes: zonk_levels(context, universes)?,
            params: zonk_terms(context, params)?,
            fields: zonk_terms(context, fields)?,
            entries: entries.clone(),
        }),

        Subterm::Match(Match {
            head,
            motive,
            cases,
        }) => Subterm::Match(Match {
            head: zonk_term(context, head)?,
            motive: motive.try_map_body(|b| zonk_term(context, b))?,
            cases: match cases {
                Cases::Bool {
                    false_case,
                    true_case,
                } => Cases::Bool {
                    false_case: zonk_term(context, false_case)?,
                    true_case: zonk_term(context, true_case)?,
                },
                Cases::FreeMonoid { carrier } => Cases::FreeMonoid {
                    carrier: match carrier {
                        Carrier::Nat {
                            empty_case,
                            cons_case,
                        } => Carrier::Nat {
                            empty_case: zonk_term(context, empty_case)?,
                            cons_case: cons_case.try_map_body(|b| zonk_term(context, b))?,
                        },
                        Carrier::Bin {
                            grain,
                            empty_case,
                            cons_case,
                        } => Carrier::Bin {
                            grain: *grain,
                            empty_case: zonk_term(context, empty_case)?,
                            cons_case: cons_case.try_map_body(|b| zonk_term(context, b))?,
                        },
                        Carrier::List {
                            elem,
                            empty_case,
                            cons_case,
                        } => Carrier::List {
                            elem: zonk_term(context, elem)?,
                            empty_case: zonk_term(context, empty_case)?,
                            cons_case: cons_case.try_map_body(|b| zonk_term(context, b))?,
                        },
                    },
                },
                Cases::Switch { cases, default } => Cases::Switch {
                    cases: cases
                        .iter()
                        .map(|(n, body)| Ok((*n, zonk_term(context, body)?)))
                        .collect::<Result<_, Error>>()?,
                    default: zonk_term(context, default)?,
                },
                Cases::Induct { cases, default } => Cases::Induct {
                    cases: cases
                        .iter()
                        .map(|(atom, arm)| {
                            Ok((
                                atom.clone(),
                                arm.with_body(arm.body.try_map_body(|b| zonk_term(context, b))?),
                            ))
                        })
                        .collect::<Result<_, Error>>()?,
                    default: default
                        .as_ref()
                        .map(|d| zonk_term(context, d))
                        .transpose()?,
                },
            },
        }),

        Subterm::Let(Let { bindings, tail }) => Subterm::Let(Let {
            bindings: bindings
                .iter()
                .map(|binding| {
                    Ok(LetBinding::new(
                        zonk_term(context, binding.type_())?,
                        zonk_term(context, binding.value())?,
                    ))
                })
                .collect::<Result<_, Error>>()?,
            tail: tail.try_map_body(|b| zonk_term(context, b))?,
        }),

        Subterm::Rec(Rec { group, tail }) => Subterm::Rec(Rec {
            group: RecGroup::new(
                group
                    .iter()
                    .map(|member| {
                        Ok(RecMemberScopes {
                            type_: member.type_.try_map_body(|t| zonk_term(context, t))?,
                            body: member.body.try_map_body(|b| zonk_term(context, b))?,
                        })
                    })
                    .collect::<Result<_, Error>>()?,
            )
            .with_universe_context(group.universe_context().clone()),
            tail: tail.try_map_body(|b| zonk_term(context, b))?,
        }),

        // Handled in `zonk_term` before dispatch.
        Subterm::Metavar(_) => unreachable!("metavariable handled by `zonk_term`"),
    })
}

/// Zonk an intrinsic's term operands. Mirrors [`Intrinsic::traverse`]'s rebuild, but fallibly substitutes metavariable solutions rather than de Bruijn shifting.
fn zonk_intrinsic(context: &Context, intrinsic: &Intrinsic) -> Result<Intrinsic, Error> {
    Ok(match intrinsic {
        Intrinsic::BoolType
        | Intrinsic::Bool(_)
        | Intrinsic::NatType
        | Intrinsic::Nat(Nat::Zero)
        | Intrinsic::ByteType
        | Intrinsic::Byte(_)
        | Intrinsic::IntType
        | Intrinsic::Int(_)
        | Intrinsic::FltType
        | Intrinsic::Flt(_)
        | Intrinsic::BinType(Grain::X)
        | Intrinsic::Bin(Grain::X, _)
        | Intrinsic::BinType(Grain::B)
        | Intrinsic::Bin(Grain::B, _)
        | Intrinsic::HandleType
        | Intrinsic::Handle(_) => intrinsic.clone(),

        Intrinsic::Nat(Nat::Succ(spine, inner)) => {
            Intrinsic::Nat(Nat::Succ(spine.clone(), zonk_term(context, inner)?))
        }

        Intrinsic::NatEql(a, b) => {
            Intrinsic::NatEql(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::HandleEql(a, b) => {
            Intrinsic::HandleEql(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::NatNeq(a, b) => {
            Intrinsic::NatNeq(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::NatAdd(a, b) => {
            Intrinsic::NatAdd(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::NatSub(a, b) => {
            Intrinsic::NatSub(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::NatMul(a, b) => {
            Intrinsic::NatMul(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::NatLt(a, b) => Intrinsic::NatLt(zonk_term(context, a)?, zonk_term(context, b)?),
        Intrinsic::NatDiv {
            dividend,
            divisor,
            non_zero,
        } => Intrinsic::NatDiv {
            dividend: zonk_term(context, dividend)?,
            divisor: zonk_term(context, divisor)?,
            non_zero: zonk_term(context, non_zero)?,
        },
        Intrinsic::NatRem {
            dividend,
            divisor,
            non_zero,
        } => Intrinsic::NatRem {
            dividend: zonk_term(context, dividend)?,
            divisor: zonk_term(context, divisor)?,
            non_zero: zonk_term(context, non_zero)?,
        },
        Intrinsic::NatGt(a, b) => Intrinsic::NatGt(zonk_term(context, a)?, zonk_term(context, b)?),
        Intrinsic::NatLte(a, b) => {
            Intrinsic::NatLte(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::NatGte(a, b) => {
            Intrinsic::NatGte(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::NatAnd(a, b) => {
            Intrinsic::NatAnd(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::NatOr(a, b) => Intrinsic::NatOr(zonk_term(context, a)?, zonk_term(context, b)?),
        Intrinsic::NatXor(a, b) => {
            Intrinsic::NatXor(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::NatShl(a, b) => {
            Intrinsic::NatShl(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::NatShr(a, b) => {
            Intrinsic::NatShr(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::NatRotl(a, b) => {
            Intrinsic::NatRotl(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::NatRotr(a, b) => {
            Intrinsic::NatRotr(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::NatClz(a) => Intrinsic::NatClz(zonk_term(context, a)?),
        Intrinsic::NatCtz(a) => Intrinsic::NatCtz(zonk_term(context, a)?),
        Intrinsic::NatPopcnt(a) => Intrinsic::NatPopcnt(zonk_term(context, a)?),
        Intrinsic::ByteToNat(t) => Intrinsic::ByteToNat(zonk_term(context, t)?),
        Intrinsic::NatToByte(t) => Intrinsic::NatToByte(zonk_term(context, t)?),
        Intrinsic::ByteEql(a, b) => {
            Intrinsic::ByteEql(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::ByteLt(a, b) => {
            Intrinsic::ByteLt(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::ByteLte(a, b) => {
            Intrinsic::ByteLte(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::ByteGt(a, b) => {
            Intrinsic::ByteGt(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::ByteGte(a, b) => {
            Intrinsic::ByteGte(zonk_term(context, a)?, zonk_term(context, b)?)
        }

        Intrinsic::BoolAnd(a, b) => {
            Intrinsic::BoolAnd(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::BoolOr(a, b) => {
            Intrinsic::BoolOr(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::BoolXor(a, b) => {
            Intrinsic::BoolXor(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::BoolEql(a, b) => {
            Intrinsic::BoolEql(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::BoolNeq(a, b) => {
            Intrinsic::BoolNeq(zonk_term(context, a)?, zonk_term(context, b)?)
        }

        Intrinsic::IntEql(a, b) => {
            Intrinsic::IntEql(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::IntNeq(a, b) => {
            Intrinsic::IntNeq(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::IntAdd(a, b) => {
            Intrinsic::IntAdd(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::IntSub(a, b) => {
            Intrinsic::IntSub(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::IntMul(a, b) => {
            Intrinsic::IntMul(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::IntDiv {
            dividend,
            divisor,
            non_zero,
        } => Intrinsic::IntDiv {
            dividend: zonk_term(context, dividend)?,
            divisor: zonk_term(context, divisor)?,
            non_zero: zonk_term(context, non_zero)?,
        },
        Intrinsic::IntRem {
            dividend,
            divisor,
            non_zero,
        } => Intrinsic::IntRem {
            dividend: zonk_term(context, dividend)?,
            divisor: zonk_term(context, divisor)?,
            non_zero: zonk_term(context, non_zero)?,
        },
        Intrinsic::IntLt(a, b) => Intrinsic::IntLt(zonk_term(context, a)?, zonk_term(context, b)?),
        Intrinsic::IntGt(a, b) => Intrinsic::IntGt(zonk_term(context, a)?, zonk_term(context, b)?),
        Intrinsic::IntLte(a, b) => {
            Intrinsic::IntLte(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::IntGte(a, b) => {
            Intrinsic::IntGte(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::IntAnd(a, b) => {
            Intrinsic::IntAnd(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::IntOr(a, b) => Intrinsic::IntOr(zonk_term(context, a)?, zonk_term(context, b)?),
        Intrinsic::IntXor(a, b) => {
            Intrinsic::IntXor(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::IntShl(a, b) => {
            Intrinsic::IntShl(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::IntShr(a, b) => {
            Intrinsic::IntShr(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::IntRotl(a, b) => {
            Intrinsic::IntRotl(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::IntRotr(a, b) => {
            Intrinsic::IntRotr(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::IntClz(a) => Intrinsic::IntClz(zonk_term(context, a)?),
        Intrinsic::IntCtz(a) => Intrinsic::IntCtz(zonk_term(context, a)?),
        Intrinsic::IntPopcnt(a) => Intrinsic::IntPopcnt(zonk_term(context, a)?),

        Intrinsic::FltAdd(a, b) => {
            Intrinsic::FltAdd(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::FltSub(a, b) => {
            Intrinsic::FltSub(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::FltMul(a, b) => {
            Intrinsic::FltMul(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::FltDiv(a, b) => {
            Intrinsic::FltDiv(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::FltRem(a, b) => {
            Intrinsic::FltRem(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::FltEql(a, b) => {
            Intrinsic::FltEql(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::FltNeq(a, b) => {
            Intrinsic::FltNeq(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::FltLt(a, b) => Intrinsic::FltLt(zonk_term(context, a)?, zonk_term(context, b)?),
        Intrinsic::FltGt(a, b) => Intrinsic::FltGt(zonk_term(context, a)?, zonk_term(context, b)?),
        Intrinsic::FltLte(a, b) => {
            Intrinsic::FltLte(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::FltGte(a, b) => {
            Intrinsic::FltGte(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::FltMin(a, b) => {
            Intrinsic::FltMin(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::FltMax(a, b) => {
            Intrinsic::FltMax(zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::FltCopysign(a, b) => {
            Intrinsic::FltCopysign(zonk_term(context, a)?, zonk_term(context, b)?)
        }

        Intrinsic::FltNeg(t) => Intrinsic::FltNeg(zonk_term(context, t)?),
        Intrinsic::FltAbs(t) => Intrinsic::FltAbs(zonk_term(context, t)?),
        Intrinsic::FltSqrt(t) => Intrinsic::FltSqrt(zonk_term(context, t)?),
        Intrinsic::FltFloor(t) => Intrinsic::FltFloor(zonk_term(context, t)?),
        Intrinsic::FltCeil(t) => Intrinsic::FltCeil(zonk_term(context, t)?),
        Intrinsic::FltTrunc(t) => Intrinsic::FltTrunc(zonk_term(context, t)?),
        Intrinsic::FltNearest(t) => Intrinsic::FltNearest(zonk_term(context, t)?),

        Intrinsic::FltToLeBytes(t) => Intrinsic::FltToLeBytes(zonk_term(context, t)?),
        Intrinsic::FltOfLeBytes { bin, four_bytes } => Intrinsic::FltOfLeBytes {
            bin: zonk_term(context, bin)?,
            four_bytes: zonk_term(context, four_bytes)?,
        },
        Intrinsic::NatToInt(t) => Intrinsic::NatToInt(zonk_term(context, t)?),
        Intrinsic::NatToFlt(t) => Intrinsic::NatToFlt(zonk_term(context, t)?),
        Intrinsic::IntToNat(t) => Intrinsic::IntToNat(zonk_term(context, t)?),
        Intrinsic::IntToFlt(t) => Intrinsic::IntToFlt(zonk_term(context, t)?),
        Intrinsic::FltToNat(t) => Intrinsic::FltToNat(zonk_term(context, t)?),
        Intrinsic::FltToInt(t) => Intrinsic::FltToInt(zonk_term(context, t)?),

        Intrinsic::BinLen(Grain::X, t) => Intrinsic::BinLen(Grain::X, zonk_term(context, t)?),
        Intrinsic::BinEql(Grain::X, a, b) => {
            Intrinsic::BinEql(Grain::X, zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::BinGet {
            grain: Grain::X,
            bin: a,
            index: b,
        } => Intrinsic::BinGet {
            grain: Grain::X,
            bin: zonk_term(context, a)?,
            index: zonk_term(context, b)?,
        },
        Intrinsic::BinAppend {
            grain: Grain::X,
            bin: a,
            element: b,
        } => Intrinsic::BinAppend {
            grain: Grain::X,
            bin: zonk_term(context, a)?,
            element: zonk_term(context, b)?,
        },
        Intrinsic::BinSlice {
            grain: Grain::X,
            bin: a,
            start: b,
            end: c,
        } => Intrinsic::BinSlice {
            grain: Grain::X,
            bin: zonk_term(context, a)?,
            start: zonk_term(context, b)?,
            end: zonk_term(context, c)?,
        },
        Intrinsic::BinConcat {
            grain: Grain::X,
            operands: terms,
        } => Intrinsic::BinConcat {
            grain: Grain::X,
            operands: zonk_terms(context, terms)?,
        },
        Intrinsic::BinLen(Grain::B, t) => Intrinsic::BinLen(Grain::B, zonk_term(context, t)?),
        Intrinsic::BinEql(Grain::B, a, b) => {
            Intrinsic::BinEql(Grain::B, zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Intrinsic::BinGet {
            grain: Grain::B,
            bin: a,
            index: b,
        } => Intrinsic::BinGet {
            grain: Grain::B,
            bin: zonk_term(context, a)?,
            index: zonk_term(context, b)?,
        },
        Intrinsic::BinAppend {
            grain: Grain::B,
            bin: a,
            element: b,
        } => Intrinsic::BinAppend {
            grain: Grain::B,
            bin: zonk_term(context, a)?,
            element: zonk_term(context, b)?,
        },
        Intrinsic::BinSlice {
            grain: Grain::B,
            bin: a,
            start: b,
            end: c,
        } => Intrinsic::BinSlice {
            grain: Grain::B,
            bin: zonk_term(context, a)?,
            start: zonk_term(context, b)?,
            end: zonk_term(context, c)?,
        },
        Intrinsic::BinConcat {
            grain: Grain::B,
            operands: terms,
        } => Intrinsic::BinConcat {
            grain: Grain::B,
            operands: zonk_terms(context, terms)?,
        },

        Intrinsic::ListType(t) => Intrinsic::ListType(zonk_term(context, t)?),
        Intrinsic::List {
            element: elem,
            items: elems,
        } => Intrinsic::List {
            element: zonk_term(context, elem)?,
            items: zonk_terms(context, elems)?,
        },
        Intrinsic::ListLen {
            element: a,
            list: b,
        } => Intrinsic::ListLen {
            element: zonk_term(context, a)?,
            list: zonk_term(context, b)?,
        },
        Intrinsic::ListGet {
            element: a,
            list: b,
            index: c,
        } => Intrinsic::ListGet {
            element: zonk_term(context, a)?,
            list: zonk_term(context, b)?,
            index: zonk_term(context, c)?,
        },
        Intrinsic::ListAppend {
            element: a,
            list: b,
            item: c,
        } => Intrinsic::ListAppend {
            element: zonk_term(context, a)?,
            list: zonk_term(context, b)?,
            item: zonk_term(context, c)?,
        },
        Intrinsic::ListSlice {
            element: a,
            list: b,
            start: c,
            end: d,
        } => Intrinsic::ListSlice {
            element: zonk_term(context, a)?,
            list: zonk_term(context, b)?,
            start: zonk_term(context, c)?,
            end: zonk_term(context, d)?,
        },
        Intrinsic::ListConcat {
            element: ty,
            operands,
        } => Intrinsic::ListConcat {
            element: zonk_term(context, ty)?,
            operands: zonk_terms(context, operands)?,
        },
        Intrinsic::ListMap {
            from: a,
            to: b,
            list,
            function: f,
        } => Intrinsic::ListMap {
            from: zonk_term(context, a)?,
            to: zonk_term(context, b)?,
            list: zonk_term(context, list)?,
            function: zonk_term(context, f)?,
        },

        Intrinsic::ProcExit(code) => Intrinsic::ProcExit(zonk_term(context, code)?),
        Intrinsic::CellType(a) => Intrinsic::CellType(zonk_term(context, a)?),
        Intrinsic::Cell {
            element: a,
            initial: b,
        } => Intrinsic::Cell {
            element: zonk_term(context, a)?,
            initial: zonk_term(context, b)?,
        },
        Intrinsic::CellSet {
            element: a,
            cell: b,
            value: c,
        } => Intrinsic::CellSet {
            element: zonk_term(context, a)?,
            cell: zonk_term(context, b)?,
            value: zonk_term(context, c)?,
        },
        Intrinsic::CellGet {
            element: a,
            cell: b,
        } => Intrinsic::CellGet {
            element: zonk_term(context, a)?,
            cell: zonk_term(context, b)?,
        },
        Intrinsic::IoType(a) => Intrinsic::IoType(zonk_term(context, a)?),
        Intrinsic::IoPure {
            result: a,
            value: b,
        } => Intrinsic::IoPure {
            result: zonk_term(context, a)?,
            value: zonk_term(context, b)?,
        },
        Intrinsic::IoBind {
            from: a,
            to: b,
            action,
            continuation: f,
        } => Intrinsic::IoBind {
            from: zonk_term(context, a)?,
            to: zonk_term(context, b)?,
            action: zonk_term(context, action)?,
            continuation: zonk_term(context, f)?,
        },
    })
}

/// Like [`zonk_telescope`], for a constructor signature: the payload domains, and the index targets it terminates in.
pub(crate) fn zonk_signature(
    context: &Context,
    telescope: &Telescope<Vec<Term>>,
) -> Result<Telescope<Vec<Term>>, Error> {
    match telescope {
        Telescope::Done(targets) => Ok(Telescope::Done(Box::new(
            targets
                .iter()
                .map(|target| zonk_term(context, target))
                .collect::<Result<_, Error>>()?,
        ))),
        Telescope::Cons(ty, rest) => Ok(Telescope::Cons(
            zonk_term(context, ty)?,
            rest.try_map_body(|inner| zonk_signature(context, inner))?,
        )),
    }
}

/// Zonk a function/Π telescope (`Func`/`FuncType`): its parameter types and its trailing body/return type, which is a real term to recurse into.
///
/// A free function rather than an inherent method on [`Telescope`]: the telescope is representation, this is the elaborator's metavariable machinery, and only the latter may name [`Context`].
pub(crate) fn zonk_telescope(
    context: &Context,
    telescope: &Telescope<Term>,
) -> Result<Telescope<Term>, Error> {
    match telescope {
        Telescope::Done(body) => Ok(Telescope::Done(zonk_term(context, body)?.into())),
        Telescope::Cons(ty, rest) => Ok(Telescope::Cons(
            zonk_term(context, ty)?,
            rest.try_map_body(|inner| zonk_telescope(context, inner))?,
        )),
    }
}

/// Like [`zonk_field_telescope`], for a declaration's nested arity: the parameter domains, then the telescope they terminate in — a family's indices, a structure's fields.
pub(crate) fn zonk_arity(
    context: &Context,
    arity: &Telescope<Telescope<()>>,
) -> Result<Telescope<Telescope<()>>, Error> {
    match arity {
        Telescope::Done(indices) => Ok(Telescope::Done(Box::new(zonk_field_telescope(
            context, indices,
        )?))),
        Telescope::Cons(ty, rest) => Ok(Telescope::Cons(
            zonk_term(context, ty)?,
            rest.try_map_body(|inner| zonk_arity(context, inner))?,
        )),
    }
}

/// Zonk a Σ telescope (`TupleType`): only its field types — its `Done` body is `()`, which carries no metavariables and is rebuilt as-is. The companion of [`zonk_telescope`], and a free function for the same reason.
pub(crate) fn zonk_field_telescope(
    context: &Context,
    telescope: &Telescope<()>,
) -> Result<Telescope<()>, Error> {
    match telescope {
        Telescope::Done(_) => Ok(Telescope::Done(Box::new(()))),
        Telescope::Cons(ty, rest) => Ok(Telescope::Cons(
            zonk_term(context, ty)?,
            rest.try_map_body(|inner| zonk_field_telescope(context, inner))?,
        )),
    }
}

/// Replace every solved universe metavariable under `value`'s binders by its recursively zonked solution, shifting each solution past the binders it crosses.
///
/// Lives here rather than beside the scoped-rewrite traversals it calls: those are representation, while [`UniverseSolver`] is elaboration state.
pub(crate) fn zonk_universe_levels_scoped<B: Bound>(
    value: &B,
    solver: &UniverseSolver,
) -> Result<B, UniverseError> {
    fn zonk_solution(
        solver: &UniverseSolver,
        level: &Level,
        visiting: &mut BTreeSet<UniverseMetaId>,
    ) -> Result<Level, UniverseError> {
        let mut replacements = BTreeMap::new();
        for meta in level.metas() {
            if let Some(solution) = solver.solution(meta)
                && visiting.insert(meta)
            {
                let zonked = zonk_solution(solver, solution, visiting)?;
                visiting.remove(&meta);
                replacements.insert(meta, zonked);
            }
        }
        level.substitute(|head| match head {
            LevelHead::Param(_) => None,
            LevelHead::Meta(meta) => replacements.get(&meta).cloned(),
        })
    }

    let solver = solver.clone();
    rewrite_universe_levels_scoped(value, move |depth, level| {
        let mut replacements = BTreeMap::new();
        for meta in level.metas() {
            if let Some(solution) = solver.solution(meta) {
                let zonked = zonk_solution(&solver, solution, &mut BTreeSet::from([meta]))?;
                replacements.insert(meta, shift_universe_params(&zonked, depth)?);
            }
        }
        level.substitute(|head| match head {
            LevelHead::Param(_) => None,
            LevelHead::Meta(meta) => replacements.get(&meta).cloned(),
        })
    })
}
