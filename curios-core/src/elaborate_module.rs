use {
    super::{
        Context, Definition, Error, Inductive, InductiveParam, Item, Mode, Module, Structure,
        Subterm, Telescope, Term, check, check_concept_registry, check_telescope_entries,
        drain_parked, elaborate, finish_deferred_witnesses, is_prop, reduce_with, register_witness,
        retry_deferred_witnesses, zonk, zonk_module,
    },
    std::collections::BTreeMap,
};

/// Rebuild a registry entry's `params`/`indices` telescopes with *elaborated*
/// types. `to_core` records the declaration's lowered spellings, and a lowered
/// type must never leak into later reduction: implicit insertion saturates
/// applications during elaboration, and an under-applied index type (e.g.
/// `Eq(0, 0)` against `Eq`'s 3-ary type constructor) would open a telescope at
/// the wrong arity the first time `reduce` meets the registry copy.
///
/// Called from `elaborate_module_rec` after the group's signatures are
/// reassumed rebuilt and *before* any body is checked — index types may
/// mention the group's own members (resolved through the assumed signatures),
/// and the type-constructor bodies' `InductiveType` nodes check their arguments
/// against this very telescope. A name with no registry entry is an ordinary
/// binding; no-op.
fn elaborate_inductive_indices(context: &mut Context, name: &str) -> Result<(), Error> {
    let Some(inductive) = context.inductive(name).cloned() else {
        return Ok(());
    };

    let n_params = inductive.params.len();
    let labels = inductive
        .indices
        .labels()
        .iter()
        .map(|label| label.to_string())
        .collect::<Vec<_>>();

    // Walk the full (params-first) index telescope, checking each entry type
    // against `Type` under the earlier binders.
    let (entries, ()) = context
        .with_frame(|context| check_telescope_entries(context, inductive.indices.clone()))?;

    let label_refs = labels.iter().map(String::as_str).collect::<Vec<_>>();
    let params =
        Telescope::build(entries[..n_params].iter().cloned(), ()).relabel(&label_refs[..n_params]);
    let indices = Telescope::build(entries, ()).relabel(&label_refs);

    context.register_inductive(
        name,
        Inductive {
            params,
            indices,
            constructors: inductive.constructors,
            result_sort: inductive.result_sort,
        },
    );

    Ok(())
}

/// Rebuild a registry entry's constructor signatures with *elaborated* types —
/// the second phase of the registry rebuild (see
/// [`elaborate_inductive_indices`]). Payload types may apply the inductive group's
/// type constructors, so this runs from `elaborate_module_rec` only after the
/// group's rebuilt bodies are defined; each terminal — the constructed
/// `InductiveType` normal form — routes through `elaborate_inductive_type`, which
/// checks the parameters and the case's target indices against the
/// already-rebuilt index telescope and returns another `InductiveType` node, the
/// shape `case_target_indices` and the match elaborators rely on.
fn elaborate_inductive_constructors(context: &mut Context, name: &str) -> Result<(), Error> {
    let Some(inductive) = context.inductive(name).cloned() else {
        return Ok(());
    };

    let mut constructors = BTreeMap::new();
    for (tag, param) in &inductive.constructors {
        let signature = &param.telescope;
        let labels = signature
            .labels()
            .iter()
            .map(|label| label.to_string())
            .collect::<Vec<_>>();

        let (entries, terminal) = context.with_frame(|context| {
            let (entries, terminal) = check_telescope_entries(context, signature.clone())?;
            let terminal = check(context, &terminal, Term::type_())?;
            Ok::<_, Error>((entries, terminal))
        })?;

        let label_refs = labels.iter().map(String::as_str).collect::<Vec<_>>();
        constructors.insert(
            tag.clone(),
            InductiveParam {
                telescope: Telescope::build(entries, terminal).relabel(&label_refs),
            },
        );
    }

    context.register_inductive(
        name,
        Inductive {
            params: inductive.params,
            indices: inductive.indices,
            constructors,
            result_sort: inductive.result_sort,
        },
    );

    Ok(())
}

/// Rebuild a struct's registry telescopes with *elaborated* types, so the field
/// types `erase` and later construction sites consult are saturated (implicit
/// insertion) and reduce correctly — the struct analogue of
/// [`elaborate_inductive_indices`], over the single (params-first) field
/// telescope. Called from `elaborate_module_let` once the type-former is
/// defined (field types may mention the struct itself and earlier items). A
/// name with no registry entry is an ordinary binding; no-op.
fn elaborate_structure(context: &mut Context, name: &str) -> Result<(), Error> {
    let Some(structure) = context.structure(name).cloned() else {
        return Ok(());
    };

    let n_params = structure.params.len();
    let labels = structure
        .fields
        .labels()
        .iter()
        .map(|label| label.to_string())
        .collect::<Vec<_>>();

    let declared_prop = matches!(
        &*reduce_with(context, &structure.result_sort)?,
        Subterm::Prop
    );

    let (entries, ()) = context.with_frame(|context| -> Result<_, Error> {
        let (entries, ()) = check_telescope_entries(context, structure.fields.clone())?;

        // Soundness of a `Prop`-sorted struct: a `Prop` is governed by proof
        // irrelevance, yet projection is an *unguarded* eliminator — it reads a
        // field out of a value the theory believes is interchangeable with any
        // other. That is consistent only when no field is informative, the
        // singleton-elimination condition (`elaborate_match::singleton_eliminable`)
        // checked here at declaration time rather than per projection. A struct
        // carries no indices, so nothing is forced and the condition reduces to:
        // every field type is itself a proposition. With this enforced, every
        // projection lands in a `Prop`, so `elaborate_proj` needs no guard.
        if declared_prop {
            for (i, (_, ty)) in entries[n_params..].iter().enumerate() {
                if !is_prop(context, ty)? {
                    let field = labels[n_params + i].clone();
                    return Err(Error::informative_prop_struct(name, field, ty.clone()));
                }
            }
        }

        Ok((entries, ()))
    })?;

    let label_refs = labels.iter().map(String::as_str).collect::<Vec<_>>();
    let params =
        Telescope::build(entries[..n_params].iter().cloned(), ()).relabel(&label_refs[..n_params]);
    let fields = Telescope::build(entries, ()).relabel(&label_refs);

    context.register_structure(
        name,
        Structure {
            params,
            fields,
            result_sort: structure.result_sort,
            module: structure.module,
            rep_public: structure.rep_public,
        },
    );

    Ok(())
}

/// Type-check a single non-recursive top-level definition, `define` it into the
/// *current* (persistent base) frame, and return its rebuilt form. The flat
/// analogue of `elaborate_let`'s per-binding work, minus the `with_frame`/tail
/// recursion: the binding must stay in scope for every later item and the
/// entrypoint body. The *rebuilt* body is `define`d (implicit insertion makes
/// the lowered one no longer interchangeable; see the comment below), and the
/// rebuilt `Definition` flows on to `zonk`/`erase`.
fn elaborate_module_let(context: &mut Context, def: &Definition) -> Result<Definition, Error> {
    let type_ = check(context, &def.type_, Term::type_())?;

    // A witness declaration registers into the program-wide table as soon as
    // its signature is known — *before* its body elaborates, so a recursive
    // witness (a `Show(Tree)` whose fields show subtrees) can resolve through
    // its own entry.
    if context.is_witness_declaration(&def.name) {
        register_witness(context, &def.name, &type_)
            .map_err(|error| error.at_opt(def.type_.span()))?;
    }

    let body = check(context, &def.body, type_.clone())?;

    // Define the *rebuilt* body at the *rebuilt* type, not the lowered ones:
    // implicit-argument insertion saturates applications during elaboration,
    // and the untyped reducer (type-level evaluation in later items' types)
    // would meet a lowered form's under-applied calls and open a telescope at
    // the wrong arity. Pre-insertion the two were interchangeable; no longer.
    context.define_assuming(&def.name, &type_, &body);

    // A struct's type-former lowers to a standalone `let`; rebuild its registry
    // telescopes now that the former is defined (no-op for an ordinary let).
    elaborate_structure(context, &def.name)?;

    Ok(Definition {
        name: def.name.clone(),
        type_,
        body,
    })
}

/// Type-check a top-level `rec` group, `define` every member into the current
/// frame, and return their rebuilt forms. The flat analogue of `elaborate_rec` —
/// assume all signatures, check the types, define all bodies, then check the
/// bodies — but with no de Bruijn open/close: members already reference each
/// other by free name.
fn elaborate_module_rec(
    context: &mut Context,
    defs: &[Definition],
) -> Result<Vec<Definition>, Error> {
    for def in defs {
        context.assume(&def.name, &def.type_);
    }

    let mut types = Vec::with_capacity(defs.len());
    for def in defs {
        types.push(check(context, &def.type_, Term::type_())?);
    }

    // Upgrade the assumptions to the *rebuilt* signatures before any body is
    // checked (see `elaborate_rec`): a lowered (under-applied) type must not
    // leak into later reduction. The lowered forms were only needed above,
    // while the signatures checked each other.
    for (def, type_) in defs.iter().zip(&types) {
        context.reassume(&def.name, type_);
    }

    // An inductive's type bindings always lower as one `rec` group whose member
    // names are the registry keys. Rebuild the registry index telescopes here
    // — after the rebuilt signatures are assumed (index types may mention the
    // group), before any body's `InductiveType` node checks against them.
    for def in defs {
        elaborate_inductive_indices(context, &def.name)?;
    }

    for def in defs {
        context.define(&def.name, &def.body);
    }

    let mut bodies = Vec::with_capacity(defs.len());
    for (def, type_) in defs.iter().zip(&types) {
        bodies.push(check(context, &def.body, type_.clone())?);
    }

    // Re-define every member with its rebuilt body: insertion saturates
    // applications during elaboration, and later items' type-level evaluation
    // must not reduce through the lowered (under-applied) originals. The
    // originals were only needed above, while the members checked each other.
    for (def, body) in defs.iter().zip(&bodies) {
        context.define(&def.name, body);
    }

    // Registry rebuild, phase two: constructor payload types may apply the
    // group's type constructors, so their signatures (and `InductiveType`
    // terminals) elaborate only now that the rebuilt bodies are defined.
    for def in defs {
        elaborate_inductive_constructors(context, &def.name)?;
    }

    Ok(defs
        .iter()
        .zip(types)
        .zip(bodies)
        .map(|((def, type_), body)| Definition {
            name: def.name.clone(),
            type_,
            body,
        })
        .collect())
}

/// Elaborate a whole [`Module`] (§9). Each top-level item is checked and `define`d
/// *cumulatively in the persistent base frame* — never a popped `with_frame` —
/// so every definition stays in scope for later items, the entrypoint `body`, and
/// (through `mode`) its type annotation. Returns the rebuilt module (lambda
/// domains solved, binders re-closed) alongside the body's type, reduced through
/// the accumulated definitions.
///
/// Elaboration is authoritative: the returned module — not the lowered input — is
/// what `zonk_module` then makes meta-free for `erase`.
pub fn elaborate_module(
    context: &mut Context,
    module: &Module,
    metavar_floor: usize,
    mode: Mode,
) -> Result<(Module, Term), Error> {
    // Seed the context's inductive registry before any item is checked: a
    // inductive's type-constructor and value-constructor definitions reference
    // their own registry entry (`elaborate_inductive_type` / `elaborate_variant`).
    for (name, inductive) in &module.inductives {
        context.register_inductive(name, inductive.clone());
    }

    // Likewise seed the struct registry — `elaborate_struct`/`elaborate_proj`
    // consult it (and `elaborate_structure` rebuilds each entry's telescopes).
    for (name, structure) in &module.structures {
        context.register_structure(name, structure.clone());
    }

    // Concept metadata and witness markers, alongside — witness *table*
    // entries register per item (`elaborate_module_let`), once the elaborated
    // head exists. With every concept present, the superclass graph can be
    // validated up front.
    for (name, concept) in &module.concepts {
        context.register_concept(name, concept.clone());
    }
    for name in &module.witnesses {
        context.mark_witness_declaration(name);
    }
    check_concept_registry(context)?;

    // Implicit-argument insertion mints metavariables during elaboration;
    // floor the counter above `to_core`'s (which returns the count alongside
    // the lowered module) so the id spaces never collide.
    context.seed_metavars(metavar_floor);

    let mut items = Vec::with_capacity(module.items.len());
    for item in &module.items {
        // The use-site module for the struct projection privacy check (§7) is
        // the qualifier prefix of the item's name (a `rec` group shares one);
        // the entrypoint body below runs under the root module.
        let item_module = match item {
            Item::Let(def) => module_of(&def.name),
            Item::Rec(defs) => defs.first().map(|d| module_of(&d.name)).unwrap_or(""),
        };
        context.set_island(item_module.to_string());

        items.push(match item {
            Item::Let(def) => Item::Let(elaborate_module_let(context, def)?),
            Item::Rec(defs) => Item::Rec(elaborate_module_rec(context, defs)?),
        });
        // Witness goals deferred on a missing table entry may be unblocked by
        // a witness this item registered — retry them before the drain, so
        // their solutions wake any constraints parked on them.
        retry_deferred_witnesses(context)?;
        // Constraints parked during this item must resolve within it: drain
        // here so an unresolvable one is attributed to its own definition and
        // frozen frames do not accumulate across items (§8).
        drain_parked(context)?;
    }

    context.set_island(String::new());
    let (body, body_type) = elaborate(context, &module.body, mode)?;
    // The whole program has elaborated: a witness goal still deferred will
    // never find a table entry — report it now.
    finish_deferred_witnesses(context)?;
    drain_parked(context)?;
    let body_type = reduce_with(context, &body_type)?;

    // The output module carries the *rebuilt* registry entries (pulled back
    // from the context, where the per-group rebuild re-registered them), so
    // `zonk_module` and `erase` see elaborated telescopes. An entry whose
    // declaring item was pruned keeps its lowered form — nothing consults it.
    let inductives = module
        .inductives
        .keys()
        .map(|name| {
            let inductive = context
                .inductive(name)
                .expect("every module entry was registered above")
                .clone();
            (name.clone(), inductive)
        })
        .collect();

    // Same for the struct registry: pull back the entries rebuilt by
    // `elaborate_structure` so `zonk_module`/`erase` see elaborated telescopes.
    let structures = module
        .structures
        .keys()
        .map(|name| {
            let structure = context
                .structure(name)
                .expect("every module entry was registered above")
                .clone();
            (name.clone(), structure)
        })
        .collect();

    let module = Module {
        items,
        inductives,
        structures,
        concepts: module.concepts.clone(),
        witnesses: module.witnesses.clone(),
        type_: module.type_.clone(),
        body,
    };

    Ok((module, body_type))
}

/// Elaborate a [`Module`] whose `sys`/`syn`/`std` prelude prefix is already
/// elaborated, reusing the cached result instead of re-type-checking it.
///
/// `prelude` is the elaborated + zonked prelude-only module — its `items` are
/// the whole prelude in dependency order (its trivial `body`/`type_` are
/// ignored). The lowered `module` still carries the *whole* program as
/// `text::to_core` produced it, and the prelude is its **leading prefix**: with
/// the prune gone every program lowers the same prelude, and since prelude items
/// depend only on each other they always topologically sort ahead of the user
/// items. So this replays the cached prelude into `context` (registering its
/// registries and `define`-ing its items — cheap map inserts, no checking) and
/// then elaborates only the items past that prefix plus the entrypoint body,
/// before zonking that user portion and splicing it onto the (already zonked)
/// prelude.
///
/// Sound because the prelude is program-independent: its items never see user
/// code, and — since top-level definitions are excluded from a metavariable's Γ
/// ([`Context::identity_snapshot`]) — a user item elaborates against the
/// identical local context it would under a from-scratch [`elaborate_module`], so
/// the solutions (and the zonked output) are identical. The cached prelude is
/// meta-free, so its ids never collide with the user metavariable range that
/// `seed_metavars(metavar_floor)` floors.
pub fn elaborate_and_zonk_with_prelude(
    context: &mut Context,
    prelude: &Module,
    module: &Module,
    metavar_floor: usize,
    mode: Mode,
) -> Result<(Module, Term), Error> {
    // Seed the registries — cached prelude entries verbatim, then the user's
    // (rebuilt by elaboration below). Keep the user keys to pull their rebuilt
    // forms back out afterwards.
    for (name, inductive) in &prelude.inductives {
        context.register_inductive(name, inductive.clone());
    }
    for (name, structure) in &prelude.structures {
        context.register_structure(name, structure.clone());
    }
    for (name, concept) in &prelude.concepts {
        context.register_concept(name, concept.clone());
    }

    let user_inductive_keys = module
        .inductives
        .keys()
        .filter(|name| !prelude.inductives.contains_key(*name))
        .cloned()
        .collect::<Vec<String>>();
    let user_structure_keys = module
        .structures
        .keys()
        .filter(|name| !prelude.structures.contains_key(*name))
        .cloned()
        .collect::<Vec<String>>();
    for name in &user_inductive_keys {
        context.register_inductive(name, module.inductives[name].clone());
    }
    for name in &user_structure_keys {
        context.register_structure(name, module.structures[name].clone());
    }
    for (name, concept) in &module.concepts {
        if !prelude.concepts.contains_key(name) {
            context.register_concept(name, concept.clone());
        }
    }
    for name in &module.witnesses {
        context.mark_witness_declaration(name);
    }
    check_concept_registry(context)?;

    // Replay the cached prelude into the persistent base frame: `define_assuming`
    // reproduces exactly the state `elaborate_module_let`/`_rec` leave behind
    // (assume the type, define the body), but with no re-checking — these terms
    // are already elaborated. A prelude witness re-registers its (already
    // elaborated) signature into the witness table, which is per-elaboration
    // state and not cached on the module.
    for item in &prelude.items {
        match item {
            Item::Let(def) => {
                context.define_assuming(&def.name, &def.type_, &def.body);
                if prelude.witnesses.contains(&def.name) {
                    register_witness(context, &def.name, &def.type_)?;
                }
            }
            Item::Rec(defs) => {
                for def in defs {
                    context.define_assuming(&def.name, &def.type_, &def.body);
                }
            }
        }
    }

    // User-minted metavariables sit strictly above `to_core`'s ids (which already
    // include the prelude's range); the cached prelude is meta-free, so nothing
    // collides.
    context.seed_metavars(metavar_floor);

    // Elaborate only the user items — everything past the cached prelude prefix.
    let mut user_items = Vec::new();
    for item in module.items.iter().skip(prelude.items.len()) {
        let item_module = match item {
            Item::Let(def) => module_of(&def.name),
            Item::Rec(defs) => defs.first().map(|d| module_of(&d.name)).unwrap_or(""),
        };
        context.set_island(item_module.to_string());

        user_items.push(match item {
            Item::Let(def) => Item::Let(elaborate_module_let(context, def)?),
            Item::Rec(defs) => Item::Rec(elaborate_module_rec(context, defs)?),
        });
        retry_deferred_witnesses(context)?;
        drain_parked(context)?;
    }

    context.set_island(String::new());
    let (body, body_type) = elaborate(context, &module.body, mode)?;
    finish_deferred_witnesses(context)?;
    drain_parked(context)?;
    let body_type = reduce_with(context, &body_type)?;

    // Pull the rebuilt user registry entries back out (mirrors `elaborate_module`).
    let user_inductives = user_inductive_keys
        .into_iter()
        .map(|name| {
            let inductive = context
                .inductive(&name)
                .expect("user entry registered")
                .clone();
            (name, inductive)
        })
        .collect();
    let user_structures = user_structure_keys
        .into_iter()
        .map(|name| {
            let structure = context
                .structure(&name)
                .expect("user entry registered")
                .clone();
            (name, structure)
        })
        .collect();

    // Zonk only the user portion (the cached prelude is already zonked), then
    // splice: cached prelude prefix ++ zonked user items / registries.
    let user_concepts = module
        .concepts
        .iter()
        .filter(|(name, _)| !prelude.concepts.contains_key(*name))
        .map(|(name, concept)| (name.clone(), concept.clone()))
        .collect();
    let user_witnesses = module
        .witnesses
        .iter()
        .filter(|name| !prelude.witnesses.contains(*name))
        .cloned()
        .collect();

    let user_module = Module {
        items: user_items,
        inductives: user_inductives,
        structures: user_structures,
        concepts: user_concepts,
        witnesses: user_witnesses,
        type_: module.type_.clone(),
        body,
    };
    let user_module = zonk_module(context, &user_module)?;
    let body_type = zonk(context, &body_type)?;

    let mut items = prelude.items.clone();
    items.extend(user_module.items);
    let mut inductives = prelude.inductives.clone();
    inductives.extend(user_module.inductives);
    let mut structures = prelude.structures.clone();
    structures.extend(user_module.structures);
    let mut concepts = prelude.concepts.clone();
    concepts.extend(user_module.concepts);
    let mut witnesses = prelude.witnesses.clone();
    witnesses.extend(user_module.witnesses);

    let module = Module {
        items,
        inductives,
        structures,
        concepts,
        witnesses,
        type_: user_module.type_,
        body: user_module.body,
    };

    Ok((module, body_type))
}

/// The module an item belongs to: the qualifier prefix of its fully-qualified
/// name (`Foo/Bar` for `Foo/Bar/f`; the empty string for a root-level `f`). The
/// flat `Module` stores no separate module field — the name is the source of
/// truth. Used to set the per-item `island` for the struct privacy check (§7),
/// in both `elaborate_module` and `erase_module`.
pub fn module_of(name: &str) -> &str {
    match name.rfind('/') {
        Some(slash) => &name[..slash],
        None => "",
    }
}
