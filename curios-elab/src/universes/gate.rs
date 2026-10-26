//! The gate a finished module's universes pass: every context closed under the scheme it is stated in, and every instance of a name at the arity its scheme declares.

#[cfg(test)]
mod tests;

use {
    crate::{Error, universe_context_validate},
    curios_core::{
        Bound, ConceptDecl, DefinitionKind, Entrypoint, Free, Global, InductDecl, InstanceHead,
        Item, Minted, Module, Place, Places, StructDecl, Subterm, UniverseContext, UniverseMetaId,
        Visit, rewrite_universe_levels_scoped_shared, universe_metas,
    },
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet},
        rc::Rc,
    },
};

fn validate_bound_universes<B: Bound>(
    value: &B,
    parameter_count: usize,
    owner: &str,
) -> Result<(), Error> {
    let nested_error = Rc::new(RefCell::new(None));
    let error = Rc::clone(&nested_error);
    let mut visit = Visit::rewriting_universes_shared(
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
    let _: B = rewrite_universe_levels_scoped_shared(value, move |depth, level| {
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
    schemes: &Schemes,
    owner: &str,
) -> Result<(), Error> {
    // Shared with the hook, never copied: a table holds a count for every name of the module, and this runs once for each term a module holds.
    let definitions = Rc::clone(&schemes.definitions);
    let inducts = Rc::clone(&schemes.inducts);
    let structs = Rc::clone(&schemes.structs);
    let owner = owner.to_string();
    let error = Rc::new(RefCell::new(None));
    let found = Rc::clone(&error);
    // Inspection only — the hook always returns `None` and the rebuilt value is discarded. Memoized on node identity so a structurally shared term is checked once per distinct node rather than once per occurrence: an unmemoized rewrite here would rebuild, and immediately drop, one node per occurrence, which on a lowered string literal is O(n^2) of pure garbage.
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
                Subterm::Instance(instance) => match &instance.head {
                    InstanceHead::RecProj(group, _) => mismatch(
                        "recursive instance",
                        "<local rec>",
                        group.universe_context().parameter_count,
                        instance.universes.levels.len(),
                    ),
                    InstanceHead::Var(var) => {
                        var.as_free().and_then(Free::as_global).and_then(|name| {
                            definitions.get(name).and_then(|expected| {
                                mismatch(
                                    "definition instance",
                                    &name.faithful().to_string(),
                                    *expected,
                                    instance.universes.levels.len(),
                                )
                            })
                        })
                    }
                },
                Subterm::InductType(induct) => inducts.get(&induct.name).and_then(|expected| {
                    mismatch(
                        "inductive occurrence",
                        &induct.name.faithful().to_string(),
                        *expected,
                        induct.universes.levels.len(),
                    )
                }),
                Subterm::Variant(variant) => inducts.get(&variant.name).and_then(|expected| {
                    mismatch(
                        "constructor occurrence",
                        &variant.name.faithful().to_string(),
                        *expected,
                        variant.universes.levels.len(),
                    )
                }),
                Subterm::StructType(struct_) => structs.get(&struct_.name).and_then(|expected| {
                    mismatch(
                        "structure occurrence",
                        &struct_.name.faithful().to_string(),
                        *expected,
                        struct_.universes.levels.len(),
                    )
                }),
                Subterm::Struct(struct_) => structs.get(&struct_.name).and_then(|expected| {
                    mismatch(
                        "structure value",
                        &struct_.name.faithful().to_string(),
                        *expected,
                        struct_.universes.levels.len(),
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

/// The universe parameter counts a module's definitions and registry entries declare, which an occurrence's instance is held to.
struct Schemes {
    definitions: Rc<BTreeMap<Global, usize>>,
    inducts: Rc<BTreeMap<Global, usize>>,
    structs: Rc<BTreeMap<Global, usize>>,
}

impl Schemes {
    /// The counts `items`, some of `module`'s, are held to: those of every name they reach, read where `places` says `module` holds each.
    fn among(module: &Module, places: &Places, items: &[&Item]) -> Self {
        let (mut definitions, mut inducts, mut structs) =
            (BTreeMap::new(), BTreeMap::new(), BTreeMap::new());
        for name in items.iter().flat_map(|item| module.reaches(item)) {
            if let Some(place) = places.place(&name) {
                let count = match (place, &module.items[place.item()]) {
                    (Place::Let(_), Item::Let(definition)) => {
                        definition.universe_context.parameter_count
                    }
                    (_, Item::Rec(rec)) => rec.group.universe_context().parameter_count,
                    (Place::Member { .. }, Item::Let(_)) => {
                        unreachable!("a member's place is a group's")
                    }
                };
                definitions.insert(name, count);
            }
            if let Some(declaration) = module.induct_decl(places, &name) {
                inducts.insert(name, declaration.universe_context.parameter_count);
            }
            if let Some(declaration) = module.struct_decl(places, &name) {
                structs.insert(name, declaration.universe_context.parameter_count);
            }
        }

        Self {
            definitions: Rc::new(definitions),
            inducts: Rc::new(inducts),
            structs: Rc::new(structs),
        }
    }

    fn of(module: &Module) -> Self {
        Self {
            definitions: Rc::new(
                definition_schemes(module)
                    .map(|(name, _, context)| (name, context.parameter_count))
                    .collect(),
            ),
            inducts: Rc::new(
                module
                    .induct_decls()
                    .iter()
                    .map(|(name, declaration)| {
                        (*name, declaration.universe_context.parameter_count)
                    })
                    .collect(),
            ),
            structs: Rc::new(
                module
                    .struct_decls()
                    .iter()
                    .map(|(name, declaration)| {
                        (*name, declaration.universe_context.parameter_count)
                    })
                    .collect(),
            ),
        }
    }

    fn validate<B: Bound>(&self, value: &B, owner: &str) -> Result<(), Error> {
        validate_instance_arities(value, self, owner)
    }
}

/// Each name `module` defines, with how it was introduced and the universe context it is generalized under: a definition's own, a group member's its group's. Read off what an item states of its members, so no group is opened to ask.
fn definition_schemes(
    module: &Module,
) -> impl Iterator<Item = (Global, &DefinitionKind, &UniverseContext)> {
    module.items.iter().flat_map(|item| match item {
        Item::Let(definition) => vec![(
            definition.name,
            &definition.kind,
            &definition.universe_context,
        )],
        Item::Rec(rec) => rec
            .definitions
            .iter()
            .map(|definition| {
                (
                    definition.name,
                    &definition.kind,
                    rec.group.universe_context(),
                )
            })
            .collect(),
    })
}

/// The universe context `item` generalizes the name `name` it declares under: a definition's own, a group member's its group's.
fn scheme_in<'a>(item: &'a Item, name: &Global) -> Option<&'a UniverseContext> {
    match item {
        Item::Let(definition) => (definition.name == *name).then_some(&definition.universe_context),
        Item::Rec(rec) => rec
            .definitions
            .iter()
            .any(|definition| definition.name == *name)
            .then(|| rec.group.universe_context()),
    }
}

fn validate_module_instance_arities(
    module: &Module,
    places: &Places,
    items: &[&Item],
) -> Result<(), Error> {
    let schemes = Schemes::among(module, places, items);
    macro_rules! validate {
        ($value:expr, $owner:expr $(,)?) => {
            schemes.validate($value, $owner)
        };
    }

    for item in items {
        for definition in item.definitions() {
            validate!(
                &definition.type_,
                &format!("definition {} type", definition.name.faithful()),
            )?;
            validate!(
                &definition.body,
                &format!("definition {} body", definition.name.faithful()),
            )?;
        }
    }
    let (inducts, structs, concepts) = entries_of(items);
    for (name, (item, declaration)) in &inducts {
        let shown = name.faithful();
        validate!(&declaration.arity, &format!("inductive {shown} arity"))?;
        validate!(
            &declaration.result_sort,
            &format!("inductive {shown} result sort"),
        )?;
        for (constructor, signature) in &declaration.constructors {
            validate!(
                &signature.telescope,
                &format!("inductive {shown} constructor {constructor}"),
            )?;
        }

        if scheme_in(item, name).is_some_and(|actual| *actual != declaration.universe_context) {
            return Err(Error::UniverseInvariant(format!(
                "inductive {shown} and its type-former definition have different universe contexts"
            )));
        }
    }
    for (name, (item, declaration)) in &structs {
        let shown = name.faithful();
        validate!(&declaration.arity, &format!("structure {shown} arity"))?;
        validate!(
            &declaration.result_sort,
            &format!("structure {shown} result sort"),
        )?;
        if scheme_in(item, name).is_some_and(|actual| *actual != declaration.universe_context) {
            return Err(Error::UniverseInvariant(format!(
                "structure {shown} and its type-former definition have different universe contexts"
            )));
        }
    }
    // A concept carries its record, so there is no concept without one to refuse.
    for (name, (_, concept)) in &concepts {
        let shown = name.faithful();
        validate!(&concept.params, &format!("concept {shown} parameters"))?;
        let record = structs
            .get(name)
            .map(|(_, record)| *record)
            .expect("a concept's former carries its record");
        if concept.universe_context != record.universe_context {
            return Err(Error::UniverseInvariant(format!(
                "concept {shown} and its structure registry have different universe contexts"
            )));
        }
    }
    // Both directions of the generated-member correspondence, read off the definition's own recorded origin. A constructor names the inductive it belongs to *and* which case it is, so this checks the registry entry exists and actually declares that case — without joining an owner and a tag back into a name to look up.
    for definition in items.iter().flat_map(|item| item.definitions()) {
        let shown = definition.name.faithful();
        match &definition.kind {
            DefinitionKind::InductiveConstructor { owner, tag } => {
                let owner = Global::Authored(*owner);
                let Some(declaration) = module.induct_decl(places, &owner) else {
                    return Err(Error::UniverseInvariant(format!(
                        "constructor definition {shown} names missing owner {}",
                        owner.faithful()
                    )));
                };
                if !declaration.declares(tag) {
                    return Err(Error::UniverseInvariant(format!(
                        "constructor definition {shown} names owner {}, which declares no case '{tag}'",
                        owner.faithful()
                    )));
                }
            }
            DefinitionKind::ConceptMethod { owner } => {
                let owner = Global::Authored(*owner);
                if module.concept(places, &owner).is_none() {
                    return Err(Error::UniverseInvariant(format!(
                        "concept method {shown} names missing owner {}",
                        owner.faithful()
                    )));
                }
            }
            DefinitionKind::Authored
            | DefinitionKind::Nominal(_)
            | DefinitionKind::Witness
            | DefinitionKind::Test => {}
        }
    }
    Ok(())
}

/// The registry entries `items` carry, each under the name of its former with the item that declares it, by name: the families, the records — a structure's or a concept's — and the concepts.
type Entries<'a, T> = BTreeMap<Global, (&'a Item, &'a T)>;

/// [`Entries`] of `items`.
fn entries_of<'a>(
    items: &[&'a Item],
) -> (
    Entries<'a, InductDecl>,
    Entries<'a, StructDecl>,
    Entries<'a, ConceptDecl>,
) {
    let (mut inducts, mut structs, mut concepts) =
        (BTreeMap::new(), BTreeMap::new(), BTreeMap::new());
    for item in items {
        for (name, nominal) in item.nominals() {
            if let Some(declaration) = nominal.inductive() {
                inducts.insert(*name, (*item, declaration));
            }
            if let Some(record) = nominal.record() {
                structs.insert(*name, (*item, record));
            }
            if let Some(concept) = nominal.concept() {
                concepts.insert(*name, (*item, concept));
            }
        }
    }

    (inducts, structs, concepts)
}

/// Hold every universe `module` states to the scheme it is stated under: each definition's and registry entry's context, the levels its terms mention, and each instance's arity against the scheme of the name it instantiates.
pub fn validate_universes(module: &Module) -> Result<(), Error> {
    validate_universes_in(
        module,
        &Places::of(module),
        &module.items.iter().collect::<Vec<_>>(),
    )
}

/// [`validate_universes`] for `items`, some of `module`'s, each held to the schemes `module` declares where `places` says it holds each name: what a unit compiled over a baseline validates of what it made again, every other item having been validated where the baseline was compiled.
pub fn validate_universes_in(
    module: &Module,
    places: &Places,
    items: &[&Item],
) -> Result<(), Error> {
    if items.is_empty() {
        return Ok(());
    }

    for item in items {
        for definition in item.definitions() {
            universe_context_validate(&definition.universe_context).map_err(|error| {
                Error::UniverseInvariant(format!(
                    "definition {} has an invalid universe context: {error}",
                    definition.name.faithful()
                ))
            })?;
            let parameter_count = definition.universe_context.parameter_count;
            validate_bound_universes(
                &definition.type_,
                parameter_count,
                &format!("definition {} type", definition.name.faithful()),
            )?;
            validate_bound_universes(
                &definition.body,
                parameter_count,
                &format!("definition {} body", definition.name.faithful()),
            )?;
        }
    }
    let (inducts, structs, concepts) = entries_of(items);
    for (name, (_, induct_decl)) in &inducts {
        let shown = name.faithful();
        universe_context_validate(&induct_decl.universe_context).map_err(|error| {
            Error::UniverseInvariant(format!(
                "inductive {shown} has an invalid universe context: {error}"
            ))
        })?;
        let parameter_count = induct_decl.universe_context.parameter_count;
        validate_bound_universes(
            &induct_decl.arity,
            parameter_count,
            &format!("inductive {shown} arity"),
        )?;
        validate_bound_universes(
            &induct_decl.result_sort,
            parameter_count,
            &format!("inductive {shown} result sort"),
        )?;
        for (constructor_name, constructor) in &induct_decl.constructors {
            validate_bound_universes(
                &constructor.telescope,
                parameter_count,
                &format!("inductive {shown} constructor {constructor_name}"),
            )?;
        }
    }
    for (name, (_, struct_decl)) in &structs {
        let shown = name.faithful();
        universe_context_validate(&struct_decl.universe_context).map_err(|error| {
            Error::UniverseInvariant(format!(
                "structure {shown} has an invalid universe context: {error}"
            ))
        })?;
        let parameter_count = struct_decl.universe_context.parameter_count;
        validate_bound_universes(
            &struct_decl.arity,
            parameter_count,
            &format!("structure {shown} arity"),
        )?;
        validate_bound_universes(
            &struct_decl.result_sort,
            parameter_count,
            &format!("structure {shown} result sort"),
        )?;
        if let Some((_, concept)) = concepts.get(name)
            && concept.universe_context != struct_decl.universe_context
        {
            return Err(Error::UniverseInvariant(format!(
                "concept and structure universe contexts differ for {shown}"
            )));
        }
    }
    for (name, (_, concept)) in &concepts {
        let shown = name.faithful();
        universe_context_validate(&concept.universe_context).map_err(|error| {
            Error::UniverseInvariant(format!(
                "concept {shown} has an invalid universe context: {error}"
            ))
        })?;
        validate_bound_universes(
            &concept.params,
            concept.universe_context.parameter_count,
            &format!("concept {shown} parameters"),
        )?;
    }
    validate_module_instance_arities(module, places, items)?;
    Ok(())
}

/// [`validate_universes`] for a program's entry: its body and the type it states are under no scheme, and every instance in them is held to the one `module` declares for the name it instantiates.
pub fn validate_entry_universes(module: &Module, entry: &Entrypoint) -> Result<(), Error> {
    let schemes = Schemes::of(module);
    validate_bound_universes(&entry.body, 0, "module body")?;
    schemes.validate(&entry.body, "module body")?;
    if let Some(type_) = &entry.type_ {
        validate_bound_universes(type_, 0, "module body type")?;
        schemes.validate(type_, "module body type")?;
    }
    Ok(())
}

/// Validate the lowering-time universe allocator contract before replaying a prepared Text module. Every meta reachable from a lowered definition or registry entry must have a seed among those of the declaration it was lowered from, which the lowering fills from index zero for each.
pub fn validate_lowered_universe_seeds(module: &Module, minted: &Minted) -> Result<(), Error> {
    let mut metas: BTreeMap<Global, BTreeSet<UniverseMetaId>> = BTreeMap::new();
    macro_rules! collect {
        ($name:expr, $value:expr) => {
            metas
                .entry($name)
                .or_default()
                .extend(universe_metas($value))
        };
    }

    for item in &module.items {
        for definition in item.definitions() {
            collect!(definition.name, &definition.type_);
            collect!(definition.name, &definition.body);
            for constraint in &definition.universe_context.constraints {
                let metas = metas.entry(definition.name).or_default();
                metas.extend(constraint.lower.metas());
                metas.extend(constraint.upper.metas());
            }
        }
    }
    for (name, declaration) in &module.induct_decls() {
        collect!(*name, &declaration.arity);
        collect!(*name, &declaration.result_sort);
        for constructor in declaration.signatures() {
            collect!(*name, &constructor.telescope);
        }
    }
    for (name, declaration) in &module.struct_decls() {
        collect!(*name, &declaration.arity);
        collect!(*name, &declaration.result_sort);
    }
    for (name, concept) in &module.concepts() {
        collect!(*name, &concept.params);
    }

    for (name, metas) in metas {
        let shown = name.faithful();
        let seeded = minted.of(&name).universes.len();
        if let Some(meta) = metas.into_iter().find(|meta| meta.0 >= seeded) {
            return Err(Error::UniverseInvariant(format!(
                "{shown}: lowered universe meta {meta} has no seed among its declaration's {seeded}"
            )));
        }
    }
    Ok(())
}
