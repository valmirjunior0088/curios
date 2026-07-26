#[cfg(test)]
mod tests;

use {
    super::{
        Apply, Bound, Carrier, Cases, Context, Definition, DefinitionKind, Error, Func, FuncType,
        InductDecl, InductParam, InductType, Item, Let, Level, Match, MetaId, Metavar,
        MetavarOrigin, Module, Nat, Prim, Proj, Rec, RecItem, Struct, StructDecl, StructType,
        Subterm, Term, Tuple, TupleType, UniverseContext, UniverseInst, Variant, Visit,
    },
    curios_base::Grain,
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet},
        rc::Rc,
        sync::Arc,
    },
};

/// Substitute every solved metavariable in `term` by its (recursively zonked)
/// solution, yielding a meta-free term (§9). An unsolved metavariable is a
/// residual hole the elaborator never pinned down — reported as `cannot_infer`
/// at its occurrence span. The result flows downstream to `erase`, which is then
/// guaranteed never to meet a `Subterm::Metavar`.
///
/// Substitution replaces a metavariable node by its solution. A solution is
/// spelled with the birth telescope's names and is *not* in general a closed
/// term; the occurrence's spine (its delayed substitution) records what each
/// birth binder corresponds to at the splice site, so `zonk_term` resolves
/// by rewriting the solution through it. Every solved occurrence carries its
/// spine — `elaborate_apply` opens telescopes with rebuilt arguments, so no
/// bare copy of a birthed hole survives to be spliced.
pub(crate) fn zonk(context: &Context, term: &Term) -> Result<Term, Error> {
    zonk_term(context, term)
}

/// Materialize the solutions already committed for term metavariables without
/// rejecting holes that remain legitimately deferred. Universe levels are
/// preserved verbatim: declaration finalization rewrites them only after this
/// pass has exposed the levels hidden in solved term-meta solutions.
pub(crate) fn zonk_solved_term_metas<B: Bound>(context: &Context, value: &B) -> B {
    if !value.has_metavar() {
        return value.clone();
    }

    type Solution = (Vec<String>, Term);

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
                let labels = labels.iter().map(String::as_str).collect::<Vec<_>>();
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

/// Zonk a whole [`Module`]: substitute metavariable solutions throughout every
/// top-level item plus the entrypoint body and annotation, yielding a meta-free
/// module for `erase` (§9).
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub fn zonk_module(context: &Context, module: &Module) -> Result<Module, Error> {
    let items = module
        .items
        .iter()
        .map(|item| zonk_item(context, item))
        .collect::<Result<Vec<_>, Error>>()?;

    let body = zonk_term(context, &module.body)?;

    let type_ = module
        .type_
        .as_ref()
        .map(|type_| zonk_term(context, type_))
        .transpose()?;

    // The registry's telescopes flow into `erase`, which runs meta-free with
    // its own (solution-less) context — so they are zonked like everything else.
    let induct_decls = module
        .induct_decls
        .iter()
        .map(|(name, induct_decl)| {
            Ok((
                name.clone(),
                InductDecl {
                    universe_context: induct_decl.universe_context.clone(),
                    params: induct_decl.params.zonk(context)?,
                    indices: induct_decl.indices.zonk(context)?,
                    constructors: induct_decl
                        .constructors
                        .iter()
                        .map(|(tag, param)| {
                            Ok((
                                tag.clone(),
                                InductParam {
                                    telescope: param.telescope.zonk(context)?,
                                    plicities: param.plicities.clone(),
                                },
                            ))
                        })
                        .collect::<Result<_, Error>>()?,
                    result_sort: zonk_term(context, &induct_decl.result_sort)?,
                    module: induct_decl.module.clone(),
                    root: induct_decl.root,
                    rep_public: induct_decl.rep_public,
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
                    params: struct_decl.params.zonk(context)?,
                    fields: struct_decl.fields.zonk(context)?,
                    result_sort: zonk_term(context, &struct_decl.result_sort)?,
                    module: struct_decl.module.clone(),
                    root: struct_decl.root,
                    rep_public: struct_decl.rep_public,
                },
            ))
        })
        .collect::<Result<_, Error>>()?;

    let module = Module {
        items,
        universe_seeds: Vec::new(),
        induct_decls,
        struct_decls,
        // Concept metadata and witness markers carry no terms of their own
        // (each concept's telescopes live in `struct_decls`, zonked above).
        concepts: module
            .concepts
            .iter()
            .map(|(name, concept)| {
                Ok((
                    name.clone(),
                    super::Concept {
                        universe_context: concept.universe_context.clone(),
                        params: concept.params.zonk(context)?,
                        fields: concept.fields.clone(),
                        supers: concept.supers.clone(),
                        root: concept.root,
                    },
                ))
            })
            .collect::<Result<_, Error>>()?,
        witnesses: module.witnesses.clone(),
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
                Subterm::RecMember(member) => vec![member.group.universe_context()],
                _ => Vec::new(),
            };
            for context in contexts {
                if error.borrow().is_none()
                    && let Err(found) = context.validate()
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
    let _: B = super::rewrite_universe_levels_scoped(value, move |depth, level| {
        let visible_parameter_count = depth
            .checked_add(parameter_count)
            .ok_or_else(|| format!("{owner}: universe binder depth overflow"))?;
        if level.is_closed(visible_parameter_count) {
            Ok(level.clone())
        } else {
            // Name the numbers. A level prints its parameters as letters that
            // cycle (`u`..`z`, then `u1`), so neither the offending index nor
            // the bound it broke can be read off `{level}`, and the bound
            // itself is a sum: a nested scheme's own parameters sit innermost
            // and the enclosing binders it may still reference follow. Which
            // of the two is short is the whole diagnosis, and recovering it
            // otherwise costs a rebuild.
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
    definitions: &BTreeMap<String, usize>,
    inducts: &BTreeMap<String, usize>,
    structs: &BTreeMap<String, usize>,
    owner: &str,
) -> Result<(), Error> {
    let definitions = definitions.clone();
    let inducts = inducts.clone();
    let structs = structs.clone();
    let owner = owner.to_string();
    let error = Rc::new(RefCell::new(None));
    let found = Rc::clone(&error);
    // Inspection only — the hook always returns `None` and the rebuilt value is
    // discarded. Memoized on node identity so a structurally shared term is
    // checked once per distinct node rather than once per occurrence: an
    // unmemoized rewrite here rebuilt, and immediately dropped, one node per
    // occurrence, which on a lowered string literal is O(n^2) of pure garbage.
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
                Subterm::UniverseInst(instance) => match &*instance.head {
                    Subterm::Var(var) => var.as_free().and_then(|name| {
                        definitions.get(name).and_then(|expected| {
                            mismatch(
                                "definition instance",
                                name,
                                *expected,
                                instance.levels.len(),
                            )
                        })
                    }),
                    Subterm::RecMember(member) => mismatch(
                        "recursive instance",
                        "<local rec>",
                        member.group.universe_context().parameter_count,
                        instance.levels.len(),
                    ),
                    _ => Some(format!(
                        "{owner}: a universe instance wraps neither a variable nor a recursive member"
                    )),
                },
                Subterm::InductType(induct) => inducts.get(&induct.name).and_then(|expected| {
                    mismatch(
                        "inductive occurrence",
                        &induct.name,
                        *expected,
                        induct.universes.len(),
                    )
                }),
                Subterm::Variant(variant) => inducts.get(&variant.name).and_then(|expected| {
                    mismatch(
                        "constructor occurrence",
                        &variant.name,
                        *expected,
                        variant.universes.len(),
                    )
                }),
                Subterm::StructType(struct_) => structs.get(&struct_.name).and_then(|expected| {
                    mismatch(
                        "structure occurrence",
                        &struct_.name,
                        *expected,
                        struct_.universes.len(),
                    )
                }),
                Subterm::Struct(struct_) => structs.get(&struct_.name).and_then(|expected| {
                    mismatch(
                        "structure value",
                        &struct_.name,
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
        for definition in match item {
            Item::Let(definition) => vec![definition.clone()],
            Item::Rec(rec) => rec.definitions(),
        } {
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
        validate!(&declaration.params, &format!("inductive {name} parameters"))?;
        validate!(&declaration.indices, &format!("inductive {name} indices"))?;
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
        validate!(&declaration.params, &format!("structure {name} parameters"))?;
        validate!(&declaration.fields, &format!("structure {name} fields"))?;
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
    // Both directions of the generated-member correspondence, read off the
    // definition's own recorded origin. A constructor names the inductive it
    // belongs to *and* which case it is, so this checks the registry entry
    // exists and actually declares that case — without joining an owner and a
    // tag back into a name to look up.
    for (name, (kind, _)) in &definition_schemes {
        match kind {
            DefinitionKind::InductiveConstructor { owner, tag } => {
                let owner = owner.join();
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
                let owner = owner.join();
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
    validate!(&module.body, "module body")?;
    if let Some(type_) = &module.type_ {
        validate!(type_, "module body annotation")?;
    }
    Ok(())
}

pub fn validate_universes(module: &Module) -> Result<(), Error> {
    for item in &module.items {
        for definition in match item {
            Item::Let(definition) => vec![definition.clone()],
            Item::Rec(rec) => rec.definitions(),
        } {
            definition.universe_context.validate().map_err(|error| {
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
        induct_decl.universe_context.validate().map_err(|error| {
            Error::UniverseInvariant(format!(
                "inductive {name} has an invalid universe context: {error}"
            ))
        })?;
        let parameter_count = induct_decl.universe_context.parameter_count;
        validate_bound_universes(
            &induct_decl.params,
            parameter_count,
            &format!("inductive {name} parameters"),
        )?;
        validate_bound_universes(
            &induct_decl.indices,
            parameter_count,
            &format!("inductive {name} indices"),
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
        struct_decl.universe_context.validate().map_err(|error| {
            Error::UniverseInvariant(format!(
                "structure {name} has an invalid universe context: {error}"
            ))
        })?;
        let parameter_count = struct_decl.universe_context.parameter_count;
        validate_bound_universes(
            &struct_decl.params,
            parameter_count,
            &format!("structure {name} parameters"),
        )?;
        validate_bound_universes(
            &struct_decl.fields,
            parameter_count,
            &format!("structure {name} fields"),
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
        concept.universe_context.validate().map_err(|error| {
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
    validate_bound_universes(&module.body, 0, "module body")?;
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

/// Validate the lowering-time universe allocator contract before replaying a
/// prepared Text module. Every meta reachable from lowered Core must have a
/// corresponding seed below the recorded allocator floor.
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
            metas.extend(super::universe_metas($value))
        };
    }

    for item in &module.items {
        for definition in match item {
            Item::Let(definition) => vec![definition.clone()],
            Item::Rec(rec) => rec.definitions(),
        } {
            collect!(&definition.type_);
            collect!(&definition.body);
            for constraint in &definition.universe_context.constraints {
                metas.extend(constraint.lower.metas());
                metas.extend(constraint.upper.metas());
            }
        }
    }
    for declaration in module.induct_decls.values() {
        collect!(&declaration.params);
        collect!(&declaration.indices);
        collect!(&declaration.result_sort);
        for constructor in declaration.constructors.values() {
            collect!(&constructor.telescope);
        }
    }
    for declaration in module.struct_decls.values() {
        collect!(&declaration.params);
        collect!(&declaration.fields);
        collect!(&declaration.result_sort);
    }
    for concept in module.concepts.values() {
        collect!(&concept.params);
    }
    collect!(&module.body);
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
        root: def.root,
        type_,
        body,
    })
}

/// The report a written goal `?` errors out with: the local scope frozen at
/// its birth, the goal's type, and the solution elaboration committed (if
/// any) — each zonked for *display*, keeping the raw spelling where unsolved
/// holes survive (the same tolerance the no-witness report uses).
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
        // A goal elaboration never reached was never birthed, so there is no
        // scope or type to report — unreachable in practice, since every kept
        // item elaborates.
        None => Error::CannotInfer,
    }
}

pub(crate) fn zonk_term(context: &Context, term: &Term) -> Result<Term, Error> {
    // A metavariable node *is* the substitution site: replace it by its solution,
    // recursively zonked (the solution may itself mention solved metavariables).
    if let Subterm::Metavar(Metavar { id, spine, origin }) = &**term {
        // A written goal `?` never splices — the whole point of writing it was
        // the report. Solved or not, error with what elaboration determined:
        // the frozen scope, the goal's type, and the solution when one landed.
        if matches!(origin, Some(MetavarOrigin::Goal)) {
            return Err(goal_report(context, *id).at_opt(term.span()));
        }

        let solution = context.metavar_solution(*id).ok_or_else(|| {
            // An unsolved metavariable the *elaborator* minted (an omitted
            // implicit or witness argument) is reported by the binder it
            // filled — the provenance rides on the node itself — not as a
            // bare hole: the user never wrote this metavariable, so a generic
            // "cannot infer" would point at nothing they can see.
            let error = match origin {
                Some(MetavarOrigin::Implicit(origin)) => {
                    Error::uninferred_implicit(origin.func.clone(), origin.binder.clone())
                }
                Some(MetavarOrigin::Witness(origin)) => {
                    // The birth record's `result` is the goal type; display it
                    // through whatever solutions landed, keeping the raw
                    // spelling if holes survive.
                    let goal = context
                        .metavar_entry(*id)
                        .map(|entry| entry.result.clone())
                        .unwrap_or_else(Term::type_ground);
                    let goal = zonk_term(context, &goal).unwrap_or(goal);
                    Error::no_witness(goal, origin.func.clone(), origin.binder.clone())
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

        // Resolve the solution in its own (named) frame first: nested solved
        // metavariables are substituted before the spine splice below.
        let resolved = zonk_term(context, solution)?;

        // A contextual occurrence: the spine records, in this site's own
        // (already de-Bruijn-correct) form, what each birth binder
        // corresponds to here. Zonk the spine entries (they may embed
        // solved metavariables), then splice the solution through them —
        // birth names captured, spine terms released.
        let entry = context
            .metavar_entry(*id)
            .expect("a solved metavariable has a birth entry");
        assert_eq!(
            spine.len(),
            entry.telescope.len(),
            "a solved metavariable's occurrence carries its full spine"
        );
        let labels = entry
            .telescope
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>();
        let spine = zonk_terms(context, spine)?;
        let refs = spine.iter().collect::<Vec<_>>();
        let zonked = resolved.capture(&labels).release(&refs);

        // Carry the hole's span only if the solution carries none of its own.
        return Ok(match term.span() {
            Some(span) => zonked.with_span(span),
            None => zonked,
        });
    }

    // Fast path: a subtree with neither term nor universe metavariables is
    // already fully zonked. A solved term metavariable's solution can be
    // term-meta-free while still carrying levels that declaration
    // finalization solved, so universe metas must keep descending.
    //
    // Both halves read cached per-node bits. Testing the metavariable *set*
    // for emptiness instead answers the same question — the set is empty
    // exactly when the bit is false — but re-walks the whole subtree at every
    // level on the way down, which is quadratic on a deep spine.
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
        Subterm::Var(var) => Subterm::Var(var.clone()),
        Subterm::UniverseInst(instance) => Subterm::UniverseInst(UniverseInst {
            head: zonk_term(context, &instance.head)?,
            levels: instance
                .levels
                .iter()
                .map(|level| context.universes().zonk(level).map_err(Error::from))
                .collect::<Result<_, _>>()?,
        }),

        // `Infix`/`NumLit` are elaboration-transient: `elaborate` replaces every
        // occurrence with a concrete `Prim` before zonk ever runs.
        Subterm::Infix(_) => unreachable!("infix node survived elaboration into zonk"),
        Subterm::NumLit(_) => unreachable!("numeric-literal node survived elaboration into zonk"),

        Subterm::Prim(prim) => Subterm::Prim(zonk_prim(context, prim)?),

        Subterm::Func(Func {
            telescope,
            plicities,
        }) => Subterm::Func(Func {
            telescope: telescope.zonk(context)?,
            plicities: plicities.clone(),
        }),

        Subterm::FuncType(FuncType {
            telescope,
            plicities,
        }) => Subterm::FuncType(FuncType {
            telescope: telescope.zonk(context)?,
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
            telescope: telescope.zonk(context)?,
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
            motive: motive.map_body(|b| zonk_term(context, b))?,
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
                            cons_case: cons_case.map_body(|b| zonk_term(context, b))?,
                        },
                        Carrier::Bin {
                            grain,
                            empty_case,
                            cons_case,
                        } => Carrier::Bin {
                            grain: *grain,
                            empty_case: zonk_term(context, empty_case)?,
                            cons_case: cons_case.map_body(|b| zonk_term(context, b))?,
                        },
                        Carrier::Lst {
                            elem,
                            empty_case,
                            cons_case,
                        } => Carrier::Lst {
                            elem: zonk_term(context, elem)?,
                            empty_case: zonk_term(context, empty_case)?,
                            cons_case: cons_case.map_body(|b| zonk_term(context, b))?,
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
                                arm.with_body(arm.body.map_body(|b| zonk_term(context, b))?),
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
                    Ok(super::LetBinding::new(
                        zonk_term(context, binding.type_())?,
                        zonk_term(context, binding.value())?,
                    ))
                })
                .collect::<Result<_, Error>>()?,
            tail: tail.map_body(|b| zonk_term(context, b))?,
        }),

        Subterm::Rec(Rec { group, tail }) => Subterm::Rec(Rec {
            group: super::RecGroup::new(
                group
                    .iter()
                    .map(|member| {
                        Ok(super::RecMemberScopes {
                            type_: member.type_.map_body(|t| zonk_term(context, t))?,
                            body: member.body.map_body(|b| zonk_term(context, b))?,
                        })
                    })
                    .collect::<Result<_, Error>>()?,
            )
            .with_universe_context(group.universe_context().clone()),
            tail: tail.map_body(|b| zonk_term(context, b))?,
        }),

        Subterm::RecMember(member) => Subterm::RecMember(super::RecMember {
            group: super::RecGroup::new(
                member
                    .group
                    .iter()
                    .map(|scopes| {
                        Ok(super::RecMemberScopes {
                            type_: scopes.type_.map_body(|t| zonk_term(context, t))?,
                            body: scopes.body.map_body(|b| zonk_term(context, b))?,
                        })
                    })
                    .collect::<Result<_, Error>>()?,
            )
            .with_universe_context(member.group.universe_context().clone()),
            index: member.index,
        }),

        // Handled in `zonk_term` before dispatch.
        Subterm::Metavar(_) => unreachable!("metavariable handled by `zonk_term`"),
    })
}

/// Zonk a primitive's term operands. Mirrors `traverse_prim`'s rebuild, but
/// fallibly substitutes metavariable solutions rather than de Bruijn shifting.
fn zonk_prim(context: &Context, prim: &Prim) -> Result<Prim, Error> {
    Ok(match prim {
        Prim::BoolType
        | Prim::Bool(_)
        | Prim::NatType
        | Prim::Nat(Nat::Zero)
        | Prim::ByteType
        | Prim::Byte(_)
        | Prim::IntType
        | Prim::Int(_)
        | Prim::FltType
        | Prim::Flt(_)
        | Prim::BinType(Grain::X)
        | Prim::Bin(Grain::X, _)
        | Prim::BinType(Grain::B)
        | Prim::Bin(Grain::B, _)
        | Prim::HandleType
        | Prim::Handle(_) => prim.clone(),

        Prim::Nat(Nat::Succ(spine, inner)) => {
            Prim::Nat(Nat::Succ(spine.clone(), zonk_term(context, inner)?))
        }

        Prim::NatEql(a, b) => Prim::NatEql(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::HandleEql(a, b) => Prim::HandleEql(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatNeq(a, b) => Prim::NatNeq(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatAdd(a, b) => Prim::NatAdd(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatSub(a, b) => Prim::NatSub(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatMul(a, b) => Prim::NatMul(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatLt(a, b) => Prim::NatLt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatDiv(a, b) => Prim::NatDiv(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatRem(a, b) => Prim::NatRem(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatGt(a, b) => Prim::NatGt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatLte(a, b) => Prim::NatLte(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatGte(a, b) => Prim::NatGte(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatAnd(a, b) => Prim::NatAnd(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatOr(a, b) => Prim::NatOr(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatXor(a, b) => Prim::NatXor(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatShl(a, b) => Prim::NatShl(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatShr(a, b) => Prim::NatShr(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatRotl(a, b) => Prim::NatRotl(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatRotr(a, b) => Prim::NatRotr(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::NatClz(a) => Prim::NatClz(zonk_term(context, a)?),
        Prim::NatCtz(a) => Prim::NatCtz(zonk_term(context, a)?),
        Prim::NatPopcnt(a) => Prim::NatPopcnt(zonk_term(context, a)?),
        Prim::ByteToNat(t) => Prim::ByteToNat(zonk_term(context, t)?),
        Prim::NatToByte(t) => Prim::NatToByte(zonk_term(context, t)?),
        Prim::ByteEql(a, b) => Prim::ByteEql(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::ByteLt(a, b) => Prim::ByteLt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::ByteLte(a, b) => Prim::ByteLte(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::ByteGt(a, b) => Prim::ByteGt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::ByteGte(a, b) => Prim::ByteGte(zonk_term(context, a)?, zonk_term(context, b)?),

        Prim::BoolAnd(a, b) => Prim::BoolAnd(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::BoolOr(a, b) => Prim::BoolOr(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::BoolXor(a, b) => Prim::BoolXor(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::BoolEql(a, b) => Prim::BoolEql(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::BoolNeq(a, b) => Prim::BoolNeq(zonk_term(context, a)?, zonk_term(context, b)?),

        Prim::IntEql(a, b) => Prim::IntEql(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntNeq(a, b) => Prim::IntNeq(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntAdd(a, b) => Prim::IntAdd(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntSub(a, b) => Prim::IntSub(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntMul(a, b) => Prim::IntMul(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntDiv(a, b) => Prim::IntDiv(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntRem(a, b) => Prim::IntRem(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntLt(a, b) => Prim::IntLt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntGt(a, b) => Prim::IntGt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntLte(a, b) => Prim::IntLte(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntGte(a, b) => Prim::IntGte(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntAnd(a, b) => Prim::IntAnd(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntOr(a, b) => Prim::IntOr(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntXor(a, b) => Prim::IntXor(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntShl(a, b) => Prim::IntShl(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntShr(a, b) => Prim::IntShr(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntRotl(a, b) => Prim::IntRotl(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntRotr(a, b) => Prim::IntRotr(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::IntClz(a) => Prim::IntClz(zonk_term(context, a)?),
        Prim::IntCtz(a) => Prim::IntCtz(zonk_term(context, a)?),
        Prim::IntPopcnt(a) => Prim::IntPopcnt(zonk_term(context, a)?),

        Prim::FltAdd(a, b) => Prim::FltAdd(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltSub(a, b) => Prim::FltSub(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltMul(a, b) => Prim::FltMul(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltDiv(a, b) => Prim::FltDiv(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltRem(a, b) => Prim::FltRem(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltEql(a, b) => Prim::FltEql(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltNeq(a, b) => Prim::FltNeq(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltLt(a, b) => Prim::FltLt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltGt(a, b) => Prim::FltGt(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltLte(a, b) => Prim::FltLte(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltGte(a, b) => Prim::FltGte(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltMin(a, b) => Prim::FltMin(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltMax(a, b) => Prim::FltMax(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::FltCopysign(a, b) => {
            Prim::FltCopysign(zonk_term(context, a)?, zonk_term(context, b)?)
        }

        Prim::FltNeg(t) => Prim::FltNeg(zonk_term(context, t)?),
        Prim::FltAbs(t) => Prim::FltAbs(zonk_term(context, t)?),
        Prim::FltSqrt(t) => Prim::FltSqrt(zonk_term(context, t)?),
        Prim::FltFloor(t) => Prim::FltFloor(zonk_term(context, t)?),
        Prim::FltCeil(t) => Prim::FltCeil(zonk_term(context, t)?),
        Prim::FltTrunc(t) => Prim::FltTrunc(zonk_term(context, t)?),
        Prim::FltNearest(t) => Prim::FltNearest(zonk_term(context, t)?),

        Prim::FltToLeBytes(t) => Prim::FltToLeBytes(zonk_term(context, t)?),
        Prim::FltOfLeBytes(t) => Prim::FltOfLeBytes(zonk_term(context, t)?),
        Prim::NatToInt(t) => Prim::NatToInt(zonk_term(context, t)?),
        Prim::NatToFlt(t) => Prim::NatToFlt(zonk_term(context, t)?),
        Prim::IntToNat(t) => Prim::IntToNat(zonk_term(context, t)?),
        Prim::IntToFlt(t) => Prim::IntToFlt(zonk_term(context, t)?),
        Prim::FltToNat(t) => Prim::FltToNat(zonk_term(context, t)?),
        Prim::FltToInt(t) => Prim::FltToInt(zonk_term(context, t)?),

        Prim::BinLen(Grain::X, t) => Prim::BinLen(Grain::X, zonk_term(context, t)?),
        Prim::BinEql(Grain::X, a, b) => {
            Prim::BinEql(Grain::X, zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Prim::BinGet(Grain::X, a, b) => {
            Prim::BinGet(Grain::X, zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Prim::BinAppend(Grain::X, a, b) => {
            Prim::BinAppend(Grain::X, zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Prim::BinSlice(Grain::X, a, b, c) => Prim::BinSlice(
            Grain::X,
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
        ),
        Prim::BinConcat(Grain::X, terms) => Prim::BinConcat(Grain::X, zonk_terms(context, terms)?),
        Prim::BinLen(Grain::B, t) => Prim::BinLen(Grain::B, zonk_term(context, t)?),
        Prim::BinEql(Grain::B, a, b) => {
            Prim::BinEql(Grain::B, zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Prim::BinGet(Grain::B, a, b) => {
            Prim::BinGet(Grain::B, zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Prim::BinAppend(Grain::B, a, b) => {
            Prim::BinAppend(Grain::B, zonk_term(context, a)?, zonk_term(context, b)?)
        }
        Prim::BinSlice(Grain::B, a, b, c) => Prim::BinSlice(
            Grain::B,
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
        ),
        Prim::BinConcat(Grain::B, terms) => Prim::BinConcat(Grain::B, zonk_terms(context, terms)?),

        Prim::LstType(t) => Prim::LstType(zonk_term(context, t)?),
        Prim::Lst(elems) => Prim::Lst(zonk_terms(context, elems)?),
        Prim::LstLen(a, b) => Prim::LstLen(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::LstGet(a, b, c) => Prim::LstGet(
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
        ),
        Prim::LstAppend(a, b, c) => Prim::LstAppend(
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
        ),
        Prim::LstSlice(a, b, c, d) => Prim::LstSlice(
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
            zonk_term(context, d)?,
        ),
        Prim::LstConcat(ty, operands) => {
            Prim::LstConcat(zonk_term(context, ty)?, zonk_terms(context, operands)?)
        }
        Prim::LstMap(a, b, lst, f) => Prim::LstMap(
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, lst)?,
            zonk_term(context, f)?,
        ),

        Prim::Foreign(function, args) => {
            Prim::Foreign(Arc::clone(function), zonk_terms(context, args)?)
        }
        Prim::Exit(a, b) => Prim::Exit(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::CellType(a) => Prim::CellType(zonk_term(context, a)?),
        Prim::Cell(a, b) => Prim::Cell(zonk_term(context, a)?, zonk_term(context, b)?),
        Prim::CellSet(a, b, c) => Prim::CellSet(
            zonk_term(context, a)?,
            zonk_term(context, b)?,
            zonk_term(context, c)?,
        ),
        Prim::CellGet(a, b) => Prim::CellGet(zonk_term(context, a)?, zonk_term(context, b)?),
    })
}
