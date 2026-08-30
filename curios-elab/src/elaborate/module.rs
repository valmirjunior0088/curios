use {
    super::{Context, Error, Mode, check, elaborate},
    crate::{
        Established, Zonked, check_concept_registry, check_is_sort, check_positivity,
        check_proof_totality, check_rec_item_totality, check_type_totality,
        check_written_type_totality, collect_goal_reports, finish_deferred_witnesses, is_prop,
        record_definition_totality, record_totality, reduce_with, register_witness,
        retry_deferred_witnesses, sort_term, zonk, zonk_arity, zonk_module, zonk_solved_term_metas,
    },
    curios_analysis::group_totality,
    curios_core::{
        Bound, ConceptDecl, Definition, DefinitionKind, Free, FuncType, Global, InductDecl,
        InductParam, Item, Level, Module, RecItem, SelfReference, StructDecl, Subterm, Telescope,
        Term, Totality, UniverseConstraintKind, UniverseConstraintOrigin, UniverseContext,
        UniverseMetaId, Visit, stamp_declaration_instance, universe_metas,
    },
    curios_utilities::{Qualifier, grown},
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet},
        rc::Rc,
    },
};

/// The universe arguments `value` applies to `name`'s declaration, at the first occurrence that carries them.
///
/// An application reaches finalization in either of two forms: the nominal normal form, which carries its instance in [`StructType`]'s or [`InductType`]'s own universe vector, or an explicit instance on an unreduced occurrence of the type-former. Both name the same levels in the same order, so either answers the question.
///
/// The two nominal cases are separate arms rather than one, because the node a declaration normalizes to is what distinguishes a concept from an inductive here. Nothing about the *name* does — deciding by inspecting `name` would be guessing at a fact the term already states.
fn declaration_instance(value: &Term, name: &Global) -> Option<Vec<Level>> {
    let found = Rc::new(RefCell::new(None));
    let sink = Rc::clone(&found);
    let name = name.clone();
    let mut visit = Visit::rewriting(
        |_, _| None,
        Box::new(move |_, term: &Term| {
            let levels = match &**term {
                Subterm::StructType(struct_type) if struct_type.name == name => {
                    Some(struct_type.universes.clone())
                }
                Subterm::InductType(induct_type) if induct_type.name == name => {
                    Some(induct_type.universes.clone())
                }
                Subterm::Instance(instance)
                    if instance
                        .head
                        .head_name()
                        .and_then(Free::as_global)
                        .is_some_and(|head| *head == name) =>
                {
                    Some(instance.levels.clone())
                }
                _ => None,
            };
            if let Some(levels) = levels
                && sink.borrow().is_none()
            {
                *sink.borrow_mut() = Some(levels);
            }
            None
        }),
    );
    let _: Term = value.traverse(&mut visit);
    found.borrow_mut().take()
}

/// [`add_declaration_sizing`] for a nested arity: the parameters, then the telescope they terminate in, sized inside the parameters' own scope. Only the parameters carry the extra rung.
fn add_arity_sizing(
    context: &mut Context,
    declaration: &Global,
    arity: &Telescope<Telescope<()>>,
    uniform_count: usize,
    result_sort: &Term,
    kind: UniverseConstraintKind,
) -> Result<(), Error> {
    let Subterm::Type(result_level) = &*reduce_with(context, result_sort)? else {
        return Ok(());
    };
    let result_level = result_level.clone();

    context.with_frame(|context| {
        let terminal = telescope_sizing(
            context,
            declaration,
            arity,
            uniform_count,
            &result_level,
            &kind,
        )?;
        telescope_sizing(context, declaration, &terminal, 0, &result_level, &kind)?;
        Ok(())
    })
}

/// Walk a (params-first) telescope, checking each binder's type against `Type` under the earlier binders (fresh-gensym, assume), and return the rebuilt `(label, type)` entries alongside the telescope's terminal — opened under those binders. Runs in the caller's frame; the same gensym-then-relabel discipline as `elaborate_tuple_type`.
fn check_telescope_entries<B: Bound>(
    context: &mut Context,
    mut telescope: Telescope<B>,
) -> Result<(Vec<(Free, Term)>, B), Error> {
    let mut entries = Vec::new();
    loop {
        match telescope {
            Telescope::Done(body) => break Ok((entries, *body)),
            Telescope::Cons(ty, rest) => {
                let rebuilt = crate::check_is_sort(context, &ty)?.0;
                let label = context.fresh(rest.first_hint());
                context.assume(&label, &rebuilt);
                telescope = rest.open(&[&Term::free_var(&label)]);
                entries.push((label, rebuilt));
            }
        }
    }
}

/// Open an already-elaborated telescope: assume one fresh binder per entry and hand the entries back, without checking them again.
///
/// The counterpart to [`check_telescope_entries`] for a telescope that has been through elaboration once already. Re-checking such a telescope is not merely wasted work — it elaborates terms that carry universe instances a *second* time, and what a caller then files is a set of instances no earlier check ever agreed to.
fn assume_telescope_entries<B: Bound>(
    context: &mut Context,
    mut telescope: Telescope<B>,
) -> (Vec<(Free, Term)>, B) {
    let mut entries = Vec::new();
    loop {
        match telescope {
            Telescope::Done(body) => break (entries, *body),
            Telescope::Cons(ty, rest) => {
                let label = context.fresh(rest.first_hint());
                context.assume(&label, &ty);
                telescope = rest.open(&[&Term::free_var(&label)]);
                entries.push((label, ty));
            }
        }
    }
}

/// Record what a telescope's domains impose on `result_level`, in the caller's frame, and hand back the terminal opened under the binders assumed for them. The first `uniform_count` domains are parameters and get one rung of slack; the rest must fit under the result level itself. Split out of [`add_declaration_sizing`] so a nested arity can size its terminal inside its parameters' own scope.
fn telescope_sizing<B: Bound>(
    context: &mut Context,
    declaration: &Global,
    telescope: &Telescope<B>,
    uniform_count: usize,
    result_level: &Level,
    kind: &UniverseConstraintKind,
) -> Result<B, Error> {
    let mut position = 0;
    let mut telescope = telescope.clone();
    loop {
        let (domain, rest) = match telescope {
            Telescope::Done(terminal) => break Ok(*terminal),
            Telescope::Cons(domain, rest) => (domain, rest),
        };

        let domain_sort = sort_term(context, &domain)?;
        let domain_sort = reduce_with(context, &domain_sort)?;
        if let Subterm::Type(domain_level) = &*domain_sort {
            let upper = if position < uniform_count {
                result_level.checked_add(1).map_err(Error::from)?
            } else {
                result_level.clone()
            };
            context
                .universes_mut()
                .add_leq(
                    domain_level.clone(),
                    upper,
                    UniverseConstraintOrigin {
                        span: domain.span(),
                        kind: kind.clone(),
                        declaration: Some(declaration.to_string()),
                        binder: rest.first_hint().map(str::to_string),
                    },
                )
                .map_err(Error::from)?;
        }

        let label = context.fresh(rest.first_hint());
        context.assume(&label, &domain);
        telescope = rest.open(&[&Term::free_var(&label)]);
        position += 1;
    }
}

fn add_declaration_sizing<B: Bound>(
    context: &mut Context,
    declaration: &Global,
    telescope: &Telescope<B>,
    uniform_count: usize,
    result_sort: &Term,
    kind: UniverseConstraintKind,
) -> Result<(), Error> {
    let Subterm::Type(result_level) = &*reduce_with(context, result_sort)? else {
        return Ok(());
    };
    let result_level = result_level.clone();

    context.with_frame(|context| {
        telescope_sizing(
            context,
            declaration,
            telescope,
            uniform_count,
            &result_level,
            &kind,
        )?;
        Ok(())
    })
}

/// Rebuild a registry entry's `params`/`indices` telescopes with *elaborated* types. `into_core` records the declaration's lowered spellings, and a lowered type must never leak into later reduction: implicit insertion saturates applications during elaboration, and an under-applied index type (e.g. `Eq(0, 0)` against `Eq`'s 3-ary type constructor) would open a telescope at the wrong arity the first time `reduce` meets the registry copy.
///
/// Called from `elaborate_module_rec` after the group's signatures are reassumed rebuilt and *before* any body is checked — index types may mention the group's own members (resolved through the assumed signatures), and the type-constructor bodies' `InductType` nodes check their arguments against this very telescope. A name with no registry entry is an ordinary binding; no-op.
fn elaborate_induct_indices(context: &mut Context, name: &Global) -> Result<(), Error> {
    let Some(induct_decl) = context.induct_decl(name).cloned() else {
        return Ok(());
    };

    let owned = |labels: Vec<&str>| {
        labels
            .iter()
            .map(|label| label.to_string())
            .collect::<Vec<_>>()
    };
    let param_labels = owned(induct_decl.arity.labels());
    let index_labels = {
        let mut walk = &induct_decl.arity;
        loop {
            match walk {
                Telescope::Cons(_, rest) => walk = rest.body(),
                Telescope::Done(indices) => break owned(indices.labels()),
            }
        }
    };

    // Walk the parameters, then the index telescope they terminate in, checking each entry type against `Type` under the binders before it.
    let (param_entries, index_entries) = context.with_frame(|context| {
        let (params, inner) = check_telescope_entries(context, induct_decl.arity.clone())?;
        let (indices, ()) = check_telescope_entries(context, inner)?;

        Ok::<_, Error>((params, indices))
    })?;

    let param_refs = param_labels.iter().map(String::as_str).collect::<Vec<_>>();
    let index_refs = index_labels.iter().map(String::as_str).collect::<Vec<_>>();
    let arity = Telescope::build(
        param_entries,
        Telescope::build(index_entries, ()).relabel(&index_refs),
    )
    .relabel(&param_refs);

    context.update_induct(
        name,
        InductDecl {
            universe_context: induct_decl.universe_context,
            arity,
            constructors: induct_decl.constructors,
            result_sort: induct_decl.result_sort,
            module: induct_decl.module,
            rep_public: induct_decl.rep_public,
            polarities: induct_decl.polarities,
        },
    );

    Ok(())
}

/// Rebuild a registry entry's constructor signatures with *elaborated* types — the second phase of the registry rebuild (see [`elaborate_induct_indices`]). Payload types may apply the inductive group's type constructors, so this runs from `elaborate_module_rec` only after the group's rebuilt bodies are defined; each terminal — the constructed `InductType` normal form — routes through `elaborate_induct_type`, which checks the parameters and the case's target indices against the already-rebuilt index telescope and returns another `InductType` node, the shape `case_target_indices` and the match elaborators rely on.
fn elaborate_induct_constructors(context: &mut Context, name: &Global) -> Result<(), Error> {
    let Some(induct_decl) = context.induct_decl(name).cloned() else {
        return Ok(());
    };

    let mut constructors = Vec::new();
    for (tag, param) in &induct_decl.constructors {
        let signature = &param.telescope;
        let labels = signature
            .labels()
            .iter()
            .map(|label| label.to_string())
            .collect::<Vec<_>>();

        let (entries, targets) = context.with_frame(|context| {
            let (entries, targets) = check_telescope_entries(context, signature.clone())?;

            // The targets are still checked by `elaborate_induct_type`, which is what compares them against the rebuilt index telescope — so the constructed type is rebuilt here from what the declaration fixes (this family, at the constructor's own parameter binders) and the targets the signature states, elaborated, and its indices taken back.
            let params = entries
                .iter()
                .take(induct_decl.param_count())
                .map(|(binder, _)| Term::free_var(binder))
                .collect::<Vec<_>>();
            let constructed =
                crate::check_is_sort(context, &Term::induct_type(name.clone(), params, targets))?.0;

            match &*constructed {
                Subterm::InductType(constructed) => {
                    Ok::<_, Error>((entries, constructed.indices.clone()))
                }
                _ => Err(Error::UniverseInvariant(
                    "a constructor's terminal elaborates to its own family".into(),
                )),
            }
        })?;

        let label_refs = labels.iter().map(String::as_str).collect::<Vec<_>>();
        constructors.push((
            tag.clone(),
            InductParam {
                telescope: Telescope::build(entries, targets).relabel(&label_refs),
                // Plicity is metadata parallel to the telescope; elaboration re-checks the types but never changes the calling convention.
                plicities: param.plicities.clone(),
            },
        ));
    }

    context.update_induct(
        name,
        InductDecl {
            universe_context: induct_decl.universe_context,
            arity: induct_decl.arity,
            constructors,
            result_sort: induct_decl.result_sort,
            module: induct_decl.module,
            rep_public: induct_decl.rep_public,
            polarities: induct_decl.polarities,
        },
    );

    Ok(())
}

/// Rebuild a struct's registry telescopes with *elaborated* types, so the field types `erase` and later construction sites consult are saturated (implicit insertion) and reduce correctly — the struct analogue of [`elaborate_induct_indices`], over the single (params-first) field telescope. Called from `elaborate_module_let` once the type-former is defined (field types may mention the struct itself and earlier items). A name with no registry entry is an ordinary binding; no-op.
fn elaborate_struct(context: &mut Context, name: &Global) -> Result<(), Error> {
    let Some(struct_decl) = context.struct_decl(name).cloned() else {
        return Ok(());
    };

    // Obligation (T)'s early net, on the one written type position that rides on no definition's type. Every other declaration position reaches it through some `Definition`: a parameter through the type former's own type, an `induct` payload through its constructor wrapper's, a `concept` method through the method wrapper's. A field is projected rather than constructed, so no wrapper names it, and until this ran the field type below was elaborated with nothing having looked at it — a productive `Shape(inf)` then unfolded until the stack died.
    //
    // Read off the *lowered* telescope, before `check_telescope_entries` touches it, which is the whole point of an early net. Entries stay closed under the binders before them; only globals matter here, so nothing needs opening.
    let mut written = &struct_decl.arity;
    let fields = loop {
        match written {
            Telescope::Cons(entry, rest) => {
                check_written_type_totality(context, entry, &format!("a field of '{name}'"))?;
                written = rest.body();
            }
            Telescope::Done(fields) => break &**fields,
        }
    };
    let mut written = fields;
    while let Telescope::Cons(entry, rest) = written {
        check_written_type_totality(context, entry, &format!("a field of '{name}'"))?;
        written = rest.body();
    }

    let owned = |labels: Vec<&str>| {
        labels
            .iter()
            .map(|label| label.to_string())
            .collect::<Vec<_>>()
    };
    let param_labels = owned(struct_decl.arity.labels());
    let field_labels = owned(struct_decl.fields().labels());

    let declared_prop = matches!(
        &*reduce_with(context, &struct_decl.result_sort)?,
        Subterm::Prop
    );

    // Open the parameters, then check the field telescope they terminate in against `Type` under the binders before it.
    //
    // The parameters are opened rather than checked: `share_struct_params` elaborated them before the former's body was, and they are the terms that body was checked against. Elaborating them again would file a second set of universe instances beside the ones already in play.
    let (param_entries, field_entries) = context.with_frame(|context| -> Result<_, Error> {
        let (params, inner) = assume_telescope_entries(context, struct_decl.arity.clone());
        let (fields, ()) = check_telescope_entries(context, inner)?;

        // Soundness of a `Prop`-sorted struct: a `Prop` is governed by proof irrelevance, yet projection is an *unguarded* eliminator — it reads a field out of a value the theory believes is interchangeable with any other. That is consistent only when no field is informative, the singleton-elimination condition (`elaborate_match::singleton_eliminable`) checked here at declaration time rather than per projection. A struct carries no indices, so nothing is forced and the condition reduces to: every field type is itself a proposition. With this enforced, every projection lands in a `Prop`, so `elaborate_proj` needs no guard.
        if declared_prop {
            for ((_, ty), label) in fields.iter().zip(&field_labels) {
                if !is_prop(context, ty)? {
                    return Err(Error::informative_prop_struct(
                        name.symbol(),
                        label.clone(),
                        ty.clone(),
                    ));
                }
            }
        }

        Ok((params, fields))
    })?;

    let param_refs = param_labels.iter().map(String::as_str).collect::<Vec<_>>();
    let field_refs = field_labels.iter().map(String::as_str).collect::<Vec<_>>();
    let arity = Telescope::build(
        param_entries,
        Telescope::build(field_entries, ()).relabel(&field_refs),
    )
    .relabel(&param_refs);
    add_arity_sizing(
        context,
        name,
        &arity,
        struct_decl.param_count(),
        &struct_decl.result_sort,
        UniverseConstraintKind::FieldSizing,
    )?;

    context.update_struct(
        name,
        StructDecl {
            universe_context: struct_decl.universe_context,
            arity,
            result_sort: struct_decl.result_sort,
            module: struct_decl.module,
            rep_public: struct_decl.rep_public,
            polarities: struct_decl.polarities,
        },
    );

    Ok(())
}

/// The levels of a declaration's result sort that occur nowhere a use site can reach — the levels no occurrence could ever choose.
///
/// `id : (A : Type u) -> A -> A` is genuinely polymorphic: a caller picks `A`, and with it `u`. `Le : Nat -> Nat -> Type u` is not. Nothing at a use site supplies that `u`, so generalizing it mints a parameter every occurrence must instantiate for no benefit — which is how a proof about naturals ends up universe-polymorphic, and how a string literal's per-byte constructor applications each mint fresh levels.
///
/// Minimizing instead is sound for two reasons. Cumulativity already lets a declaration sitting at `Type 0` be used where a higher universe is expected, so the parameter buys no expressiveness. And `minimize` takes least solutions *subject to the recorded constraints*, so a level that genuinely depends on an argument still lands there: `List(A : Type u) : Type v` keeps `v = u`, because constructor sizing constrains it, rather than collapsing to zero.
///
/// A level occurring in both a binder domain and the result sort stays in the interface — the caller chooses it, and the result merely mentions it.
fn result_sort_only_metas(context: &Context, type_: &Term) -> BTreeSet<UniverseMetaId> {
    fn peel<'a>(term: &'a Term, domains: &mut Vec<&'a Term>) -> &'a Term {
        match &**term {
            Subterm::FuncType(FuncType { telescope, .. }) => {
                let mut current = telescope;
                loop {
                    match current {
                        Telescope::Cons(domain, rest) => {
                            domains.push(domain);
                            current = rest.body();
                        }
                        Telescope::Done(body) => return peel(body, domains),
                    }
                }
            }
            _ => term,
        }
    }

    let mut domains = Vec::new();
    let terminal = peel(type_, &mut domains);
    let mut result = context.universe_metas_in(terminal);
    for domain in domains {
        for meta in context.universe_metas_in(domain) {
            result.remove(&meta);
        }
    }
    result
}

fn finalize_definition(
    context: &mut Context,
    name: &Global,
    kind: &DefinitionKind,
    type_: Term,
    body: Term,
    self_reference: SelfReference,
) -> Result<(UniverseContext, Term, Term), Error> {
    curios_profile::profile!("finalize_definition");
    let type_ = zonk_solved_term_metas(context, &type_);
    let body = zonk_solved_term_metas(context, &body);

    // A method wrapper is not independently polymorphic. Its own type mentions only a *subset* of its concept's levels — `pure` names some, `bind` others — so generalizing it alone yields fewer parameters than the concept has (`Monad` 5 against `pure` 2, `bind` 1). But the type also carries the concept applied at *all* of them, through the `use w : C(..)` binder, and the levels outside the wrapper's own generalized set then have nothing to name them: `level w escapes its universe parameter context`. So the wrapper must inherit the concept's whole context. Lean and Rocq both hand a projection its structure's universe parameters verbatim, for the same reason. The owner comes from `into_core`, which records it where the wrapper is generated; re-deriving it by splitting `name` would misread an ordinary definition that merely happens to sit under a concept's namespace. Inheriting the context is only half the work: those levels are still metas, and ordinary finalization — the one thing that would solve them — is exactly what must not run here, because it would mint the wrapper's own parameters in its own order. `finalize_at_instance` binds the concept application's arguments to the inherited parameters positionally instead.
    if let DefinitionKind::ConceptMethod { owner } = kind {
        let owner = Global::Authored(owner.clone());
        let universe_context = context
            .concept(&owner)
            .ok_or_else(|| {
                Error::UniverseInvariant(format!(
                    "{name}: method wrapper names an unregistered concept '{}'",
                    owner.symbol()
                ))
            })?
            .universe_context
            .clone();
        let instance = declaration_instance(&type_, &owner).ok_or_else(|| {
            Error::UniverseInvariant(format!(
                "{name}: method wrapper does not apply its concept '{}'",
                owner.symbol()
            ))
        })?;
        let mut metas = context.universe_metas_in(&type_);
        metas.extend(context.universe_metas_in(&body));
        context.finalize_universe_metas_at_instance(
            metas,
            &instance,
            universe_context.parameter_count,
        )?;
        let type_ = context.zonk_universe_levels(&type_)?;
        let body = context.zonk_universe_levels(&body)?;
        return Ok((universe_context, type_, body));
    }
    // A value constructor stands in the same relation to its inductive as a method wrapper does to its concept, and fails the same way. `Result(S, F)` has two universe parameters; `success` mentions only `S` in its payload, so generalizing it alone mints its own `Param(0..1)` in meta-id order and displaces the inductive's — which `generalize` then shifts up past the declared count, leaving `Param(2)`/`Param(3)` with nothing to name them. A constructor is not independently polymorphic: its universes *are* the inductive's, so it inherits that context and binds its instance to those parameters positionally.
    //
    // Like `ConceptMethod`, the owner is a field `into_core` records where the constructor is generated, not something recovered from `name`.
    if let DefinitionKind::InductiveConstructor { owner, .. } = kind {
        let owner = Global::Authored(owner.clone());
        let universe_context = context
            .induct_decl(&owner)
            .ok_or_else(|| {
                Error::UniverseInvariant(format!(
                    "{name}: constructor names an unregistered inductive '{}'",
                    owner.symbol()
                ))
            })?
            .universe_context
            .clone();
        // A constructor of a *parameterless* inductive results in a bare self-reference: an occurrence inside the group is monomorphic in the group's own universes, and `stamp_declaration_instance` only gives it an explicit instance after this finalization runs. So a missing instance is not a malformed constructor — it denotes the identity instance, which is what the stamp would write.
        let instance = declaration_instance(&type_, &owner)
            .unwrap_or_else(|| universe_context.identity_instance());
        let mut metas = context.universe_metas_in(&type_);
        metas.extend(context.universe_metas_in(&body));
        context.finalize_universe_metas_at_instance(
            metas,
            &instance,
            universe_context.parameter_count,
        )?;
        let type_ = context.zonk_universe_levels(&type_)?;
        let body = context.zonk_universe_levels(&body)?;
        return Ok((universe_context, type_, body));
    }
    if let Some(struct_decl) = context.struct_decl(name).cloned() {
        let arity = zonk_arity(context, &struct_decl.arity)?;
        let result_sort = zonk(context, &struct_decl.result_sort)?;
        context.update_struct(
            name,
            StructDecl {
                universe_context: struct_decl.universe_context,
                arity,
                result_sort,
                module: struct_decl.module,
                rep_public: struct_decl.rep_public,
                polarities: struct_decl.polarities,
            },
        );
    }
    if let Some(concept) = context.concept(name).cloned() {
        context.update_concept(
            name,
            ConceptDecl {
                universe_context: concept.universe_context,
                params: zonk_solved_term_metas(context, &concept.params),
                fields: concept.fields,
                supers: concept.supers,
            },
        );
    }

    // The signature and any registry telescope form the declaration's interface: a use site instantiates exactly these. Levels reachable only through the body are internal classifiers and are minimized instead, as are the levels of the result sort — see [`result_sort_only_metas`].
    let determined = result_sort_only_metas(context, &type_);
    let mut interface = context.universe_metas_in(&type_);
    interface.retain(|meta| !determined.contains(meta));
    if let Some(struct_decl) = context.struct_decl(name) {
        interface.extend(universe_metas(&struct_decl.arity));
        interface.extend(struct_decl.result_sort.universe_metas());
    }
    if let Some(concept) = context.concept(name) {
        interface.extend(universe_metas(&concept.params));
    }
    let mut internal = context.universe_metas_in(&body);
    internal.extend(determined);

    let universe_context = context.finalize_universe_metas(interface, internal)?;
    let levels = universe_context.identity_instance();
    let owned = BTreeSet::from([name.clone()]);
    // A non-recursive definition's own name stays free in its signature, body, and registry entry: nothing captures it, so each occurrence carries the instance itself. A definition whose body names *itself* is about to be captured by a single-member group instead, and its occurrences reach that binder — see [`elaborate_module_let`].
    let free = self_reference;
    let type_ = stamp_declaration_instance(
        &context.zonk_universe_levels(&type_)?,
        &owned,
        free,
        &levels,
    );
    let body =
        stamp_declaration_instance(&context.zonk_universe_levels(&body)?, &owned, free, &levels);

    if let Some(struct_decl) = context.struct_decl(name).cloned() {
        context.update_struct(
            name,
            StructDecl {
                universe_context: universe_context.clone(),
                arity: stamp_declaration_instance(
                    &context.zonk_universe_levels(&struct_decl.arity)?,
                    &owned,
                    free,
                    &levels,
                ),
                result_sort: stamp_declaration_instance(
                    &context.zonk_universe_levels(&struct_decl.result_sort)?,
                    &owned,
                    free,
                    &levels,
                ),
                module: struct_decl.module,
                rep_public: struct_decl.rep_public,
                polarities: struct_decl.polarities,
            },
        );
    }
    if let Some(concept) = context.concept(name).cloned() {
        context.update_concept(
            name,
            ConceptDecl {
                universe_context: universe_context.clone(),
                params: stamp_declaration_instance(
                    &context.zonk_universe_levels(&concept.params)?,
                    &owned,
                    free,
                    &levels,
                ),
                fields: concept.fields,
                supers: concept.supers,
            },
        );
    }

    Ok((universe_context, type_, body))
}

/// Replace a struct's registry parameter telescope with the one its type-former's declared type already carries, before that former's body is checked.
///
/// The lowerer files a `StructDecl` whose arity is the *written* telescope, untouched by elaboration (`curios-text/src/into_core.rs`, `universe_context: empty()`). The former's body is the `StructType` node over its own parameters, and `elaborate_struct_type` checks those parameters against that arity — against terms carrying no universe instances, while the binders the body meets come from `type_`, which was just elaborated and does carry them.
///
/// The two can never be reconciled, and not for want of trying: [`Frames::var_reduct`](crate::context) refuses to unfold a universe-polymorphic global reached through a bare `Var`, because there is no instance to substitute into its body. So the raw side is irreducible by construction, and conversion is being handed a problem no reduction could decide. Elaborating the telescope a second time here would only mint a *third* set of instances; taking the one the former's type already holds makes the comparison compare a term with itself.
///
/// Parameters only. A field type may mention the struct itself, so the field telescope stays with [`elaborate_struct`], which runs once the former is defined; a parameter type cannot, since the struct is not yet a type where its own parameters are written.
fn share_struct_params(context: &mut Context, name: &Global, type_: &Term) {
    let Some(struct_decl) = context.struct_decl(name).cloned() else {
        return;
    };
    let count = struct_decl.param_count();
    // A nullary struct has no parameters to share, and its former is declared at the result sort rather than at a function type.
    if count == 0 {
        return;
    }

    // Both invariants below belong to the lowerer, and both are stated rather than tolerated because `elaborate_struct` now *opens* this telescope instead of re-checking it: a silent return would leave the raw arity in place with nothing downstream left to notice.
    let Subterm::FuncType(FuncType { telescope, .. }) = &**type_ else {
        unreachable!("a parameterized struct's type-former is declared at a function type");
    };

    let mut written = telescope.clone();
    let mut lowered = struct_decl.arity.clone();
    let mut params = Vec::with_capacity(count);
    for _ in 0..count {
        let (Telescope::Cons(domain, written_rest), Telescope::Cons(_, lowered_rest)) =
            (written, lowered)
        else {
            unreachable!(
                "the former's telescope and the registry arity lower from one parameter list"
            );
        };
        // One binder opens both sides, so a later domain and the field telescope below refer to the same one.
        let binder = context.fresh(lowered_rest.first_hint());
        let occurrence = Term::free_var(&binder);
        written = written_rest.open(&[&occurrence]);
        lowered = lowered_rest.open(&[&occurrence]);
        params.push((binder, domain));
    }

    let Telescope::Done(fields) = lowered else {
        unreachable!("the arity's parameters are exactly `param_count` entries");
    };

    context.update_struct(
        name,
        StructDecl {
            universe_context: struct_decl.universe_context,
            arity: Telescope::build(params, *fields),
            result_sort: struct_decl.result_sort,
            module: struct_decl.module,
            rep_public: struct_decl.rep_public,
            polarities: struct_decl.polarities,
        },
    );
}

/// Type-check a single non-recursive top-level definition, `define` it into the *current* (persistent base) frame, and return its rebuilt form. The flat analogue of `elaborate_let`'s per-binding work, minus the `with_frame`/tail recursion: the binding must stay in scope for every later item and the entrypoint body. The *rebuilt* body is `define`d (implicit insertion makes the lowered one no longer interchangeable; see the comment below), and the rebuilt `Definition` flows on to `zonk`/`erase`.
fn elaborate_module_let(context: &mut Context, def: &Definition) -> Result<Item, Error> {
    // Γ and the definition store key on the free-variable identity; the nominal registries key on the definition's own name.
    let name = Free::from(&def.name);
    let outer_site = context.set_checked_site(&format!("'{}'", def.name));
    check_written_type_totality(context, &def.type_, &format!("the type of '{}'", def.name))?;
    let type_ = crate::check_is_sort(context, &def.type_)?.0;

    // A witness declaration registers into the program-wide table as soon as its signature is known — *before* its body elaborates, so a recursive witness (a `Show(Tree)` whose fields show subtrees) can resolve through its own entry.
    if context.is_witness_declaration(&def.name) {
        register_witness(
            context,
            &def.name,
            &type_,
            def.universe_context.clone(),
            &def.island,
        )
        .map_err(|error| error.at_opt(def.type_.span()))?;
    }

    // Before the body: a struct former's body checks its parameters against the registry arity, which is lowered raw. Share the elaborated telescope the type above just produced, so the two sides are one term.
    share_struct_params(context, &def.name, &type_);

    let body = check(context, &def.body, type_.clone())?;
    context.sweep_parked()?;

    // Define the *rebuilt* body at the *rebuilt* type, not the lowered ones: implicit-argument insertion saturates applications during elaboration, and the untyped reducer (type-level evaluation in later items' types) would meet a lowered form's under-applied calls and open a telescope at the wrong arity. Pre-insertion the two were interchangeable; no longer.
    context.define_assuming(&name, &type_, &body, Some(&def.kind));

    // A struct's type-former lowers to a standalone `let`; rebuild its registry telescopes now that the former is defined (no-op for an ordinary let).
    elaborate_struct(context, &def.name)?;

    // A witness may resolve through its own table entry — the registration above exists to let it — and a body that does names itself. Cross-definition value recursion is unexpressible outside a `rec` item by construction, so such a witness becomes a single-member group below, and its self-references are bound by that group's binder rather than free.
    let recursive_witness = context.is_witness_declaration(&def.name) && {
        // Read the *zonked* body: the self-reference arrives as a witness metavariable that resolution solved, so an unzonked body does not mention the name yet. `finalize_definition` zonks again, which costs nothing and keeps that its own business.
        zonk_solved_term_metas(context, &body)
            .free_vars()
            .contains(&name)
    };
    let self_reference = match recursive_witness {
        true => SelfReference::Bound,
        false => SelfReference::Free,
    };

    let (universe_context, type_, body) =
        finalize_definition(context, &def.name, &def.kind, type_, body, self_reference)?;
    context.reassume(&name, &type_);
    context.define(&name, &body, Some(&def.kind));
    context.set_assumption_universe_context(&name, universe_context.clone());
    if context.is_witness_declaration(&def.name) {
        context.update_witness_scheme(&def.name, universe_context.clone(), type_.clone());
    }

    context.restore_checked_site(outer_site);

    let definition = Definition {
        name: def.name.clone(),
        kind: def.kind.clone(),
        universe_context,
        island: def.island.clone(),
        totality: def.totality,
        type_,
        body,
    };

    if !recursive_witness {
        // Classify now, so a later item's *written* type can be refused before it is reduced. A `let` is its own group, hence total as far as the group verdict is concerned.
        record_definition_totality(context, &definition, Totality::Total);
        return Ok(Item::Let(definition));
    }

    // The witness is a recursive group of one. Its members' occurrences were stamped `Bound` above, which is what `RecItem::try_new` then captures; the context binding moves from the self-mentioning body to the group's own projection, so a later item reducing this witness meets the fold rather than a free name the kernel would refuse.
    let rec = RecItem::try_new(vec![definition]).map_err(Error::from)?;
    let member = Term::rec_proj(rec.group.clone(), 0);
    context.define(&name, &member, Some(&def.kind));

    // Classified as the group it now is, not assumed total the way a `let` may be: a witness that evaluates itself descends on nothing, and the erasure obligations are entitled to know.
    let verdict = group_totality(context, &rec.group);
    for definition in rec.definitions() {
        record_definition_totality(context, &definition, verdict);
    }

    Ok(Item::Rec(rec))
}

/// Type-check a flat top-level `rec` item and return its rebuilt structural group. The input is opened over the export names for elaboration; the output captures those names back into one [`RecItem`] and publishes each export as its folded structural member.
fn elaborate_module_rec(context: &mut Context, rec: &RecItem) -> Result<RecItem, Error> {
    let defs = rec.definitions();
    // See `elaborate_module_let`: Γ keys on identities, the registries on names.
    let names = defs
        .iter()
        .map(|def| Free::from(&def.name))
        .collect::<Vec<_>>();
    for (def, name) in defs.iter().zip(&names) {
        context.assume(name, &def.type_);
    }

    let mut types = Vec::with_capacity(defs.len());
    for def in &defs {
        check_written_type_totality(context, &def.type_, &format!("the type of '{}'", def.name))?;
        types.push(crate::check_is_sort(context, &def.type_)?.0);
    }

    // Upgrade the assumptions to the *rebuilt* signatures before any body is checked (see `elaborate_rec`): a lowered (under-applied) type must not leak into later reduction. The lowered forms were only needed above, while the signatures checked each other.
    for (name, type_) in names.iter().zip(&types) {
        context.reassume(name, type_);
    }

    // A `satisfy … and …` group registers every member's signature before any body elaborates, as `elaborate_module_let` does for one witness: the bodies resolve through one another's entries, and through their own.
    for (def, type_) in defs.iter().zip(&types) {
        if context.is_witness_declaration(&def.name) {
            register_witness(
                context,
                &def.name,
                type_,
                def.universe_context.clone(),
                &def.island,
            )
            .map_err(|error| error.at_opt(def.type_.span()))?;
        }
    }

    // A struct group's formers check their parameters against registry arities lowered raw, exactly as a lone former does — see `share_struct_params`.
    for (def, type_) in defs.iter().zip(&types) {
        share_struct_params(context, &def.name, type_);
    }

    // An inductive's type bindings always lower as one `rec` group whose member names are the registry keys. Rebuild the registry index telescopes here — after the rebuilt signatures are assumed (index types may mention the group), before any body's `InductType` node checks against them.
    for def in &defs {
        elaborate_induct_indices(context, &def.name)?;
    }

    let slots = defs
        .iter()
        .zip(&names)
        .zip(&types)
        .map(|((def, name), type_)| {
            let (id, slot) = context.fresh_rec_slot(type_.clone());
            context.define(name, &slot, Some(&def.kind));
            id
        })
        .collect::<Vec<_>>();

    let mut bodies = Vec::with_capacity(defs.len());
    for ((def, type_), slot) in defs.iter().zip(&types).zip(slots) {
        let outer_site = context.set_checked_site(&format!("'{}'", def.name));
        let body = check(context, &def.body, type_.clone())?;
        context.restore_checked_site(outer_site);
        context.fill_rec_slot(slot, body.clone());
        bodies.push(body);
    }
    context.retry_parked()?;

    // Registry rebuild, phase two: constructor payload types may apply the group's type constructors, so their signatures (and `InductType` terminals) elaborate only now that the rebuilt bodies are defined.
    for def in &defs {
        elaborate_induct_constructors(context, &def.name)?;
    }
    // A struct or concept group's field telescopes elaborate now that every former is defined, since each may name the others.
    for def in &defs {
        elaborate_struct(context, &def.name)?;
    }
    for def in &defs {
        if let Some(induct_decl) = context.induct_decl(&def.name).cloned() {
            for constructor in induct_decl.signatures() {
                add_declaration_sizing(
                    context,
                    &def.name,
                    &constructor.telescope,
                    induct_decl.param_count(),
                    &induct_decl.result_sort,
                    UniverseConstraintKind::ConstructorSizing,
                )?;
            }
        }
    }
    context.sweep_parked()?;

    let types = types
        .iter()
        .map(|type_| zonk_solved_term_metas(context, type_))
        .collect::<Vec<_>>();
    let bodies = bodies
        .iter()
        .map(|body| zonk_solved_term_metas(context, body))
        .collect::<Vec<_>>();
    for def in &defs {
        let Some(induct_decl) = context.induct_decl(&def.name).cloned() else {
            continue;
        };
        context.update_induct(
            &def.name,
            InductDecl {
                universe_context: induct_decl.universe_context,
                arity: zonk_solved_term_metas(context, &induct_decl.arity),
                constructors: induct_decl
                    .constructors
                    .into_iter()
                    .map(|(tag, constructor)| {
                        (
                            tag,
                            InductParam {
                                telescope: zonk_solved_term_metas(context, &constructor.telescope),
                                plicities: constructor.plicities,
                            },
                        )
                    })
                    .collect(),
                result_sort: zonk_solved_term_metas(context, &induct_decl.result_sort),
                module: induct_decl.module,
                rep_public: induct_decl.rep_public,
                polarities: induct_decl.polarities,
            },
        );
    }
    for def in &defs {
        if let Some(struct_decl) = context.struct_decl(&def.name).cloned() {
            let arity = zonk_arity(context, &struct_decl.arity)?;
            let result_sort = zonk(context, &struct_decl.result_sort)?;
            context.update_struct(
                &def.name,
                StructDecl {
                    universe_context: struct_decl.universe_context,
                    arity,
                    result_sort,
                    module: struct_decl.module,
                    rep_public: struct_decl.rep_public,
                    polarities: struct_decl.polarities,
                },
            );
        }
        if let Some(concept) = context.concept(&def.name).cloned() {
            context.update_concept(
                &def.name,
                ConceptDecl {
                    universe_context: concept.universe_context,
                    params: zonk_solved_term_metas(context, &concept.params),
                    fields: concept.fields,
                    supers: concept.supers,
                },
            );
        }
    }

    // As for a single definition, the group's interface is its member signatures and registry telescopes; levels reachable only through a member body are internal and minimized rather than generalized, and so are the levels of a member's result sort that no member's domain mentions — see [`result_sort_only_metas`]. The rule is the one `finalize_definition` applies, read over the group: a level one member keeps in its interface stays there for all of them, since the group shares a context, and only a level every member confines to its result sort is determined. A group of one is then generalized exactly as the same signature would be as a `let`, which is what keeps the path a definition takes from showing in its scheme.
    let mut interface = types
        .iter()
        .flat_map(|type_| context.universe_metas_in(type_))
        .collect::<BTreeSet<_>>();
    let mut determined = BTreeSet::new();
    let mut kept = BTreeSet::new();
    for type_ in &types {
        let member_determined = result_sort_only_metas(context, type_);
        for meta in context.universe_metas_in(type_) {
            if !member_determined.contains(&meta) {
                kept.insert(meta);
            }
        }
        determined.extend(member_determined);
    }
    determined.retain(|meta| !kept.contains(meta));
    interface.retain(|meta| !determined.contains(meta));
    for def in &defs {
        if let Some(induct_decl) = context.induct_decl(&def.name) {
            interface.extend(universe_metas(&induct_decl.arity));
            interface.extend(induct_decl.result_sort.universe_metas());
            for constructor in induct_decl.signatures() {
                interface.extend(universe_metas(&constructor.telescope));
            }
        }
        if let Some(struct_decl) = context.struct_decl(&def.name) {
            interface.extend(universe_metas(&struct_decl.arity));
            interface.extend(struct_decl.result_sort.universe_metas());
        }
        if let Some(concept) = context.concept(&def.name) {
            interface.extend(universe_metas(&concept.params));
        }
    }
    let mut internal = bodies
        .iter()
        .flat_map(|body| context.universe_metas_in(body))
        .collect::<BTreeSet<_>>();
    internal.extend(determined);
    let universe_context = context.finalize_universe_metas(interface, internal)?;

    // The group's members are monomorphic in their own universes, so every occurrence of one member inside the group — in a signature, a body, or a rebuilt registry telescope — denotes the group's own instance. Those occurrences were elaborated before the parameters existed and carry no instance at all until this rewrite gives them one.
    let instance = universe_context.identity_instance();
    let owned = defs
        .iter()
        .map(|def| def.name.clone())
        .collect::<BTreeSet<_>>();
    fn stamp<B: Bound>(
        context: &Context,
        value: &B,
        owned: &BTreeSet<Global>,
        self_reference: SelfReference,
        instance: &[Level],
    ) -> Result<B, Error> {
        Ok(stamp_declaration_instance(
            &context.zonk_universe_levels(value)?,
            owned,
            self_reference,
            instance,
        ))
    }

    // `RecItem::try_new` captures the member names into the group's binder, so the signatures and bodies reach it with their self-references bound.
    let types = types
        .iter()
        .map(|type_| stamp(context, type_, &owned, SelfReference::Bound, &instance))
        .collect::<Result<Vec<_>, _>>()?;
    let bodies = bodies
        .iter()
        .map(|body| stamp(context, body, &owned, SelfReference::Bound, &instance))
        .collect::<Result<Vec<_>, _>>()?;

    for def in &defs {
        let Some(induct_decl) = context.induct_decl(&def.name).cloned() else {
            continue;
        };
        let constructors = induct_decl
            .constructors
            .into_iter()
            .map(|(tag, constructor)| {
                Ok((
                    tag,
                    InductParam {
                        // A registry telescope is stored outside the group and instantiated per use site, so its self-references are free and must carry the instance themselves.
                        telescope: stamp(
                            context,
                            &constructor.telescope,
                            &owned,
                            SelfReference::Free,
                            &instance,
                        )?,
                        plicities: constructor.plicities,
                    },
                ))
            })
            .collect::<Result<_, Error>>()?;
        let free = SelfReference::Free;
        let arity = stamp(context, &induct_decl.arity, &owned, free, &instance)?;
        let result_sort = stamp(context, &induct_decl.result_sort, &owned, free, &instance)?;
        context.update_induct(
            &def.name,
            InductDecl {
                universe_context: universe_context.clone(),
                arity,
                constructors,
                result_sort,
                module: induct_decl.module,
                rep_public: induct_decl.rep_public,
                polarities: induct_decl.polarities,
            },
        );
    }
    // A struct or concept registry entry is stamped as an inductive's is: stored outside the group, so its self-references are free and carry the instance themselves.
    for def in &defs {
        let free = SelfReference::Free;
        if let Some(struct_decl) = context.struct_decl(&def.name).cloned() {
            context.update_struct(
                &def.name,
                StructDecl {
                    universe_context: universe_context.clone(),
                    arity: stamp(context, &struct_decl.arity, &owned, free, &instance)?,
                    result_sort: stamp(context, &struct_decl.result_sort, &owned, free, &instance)?,
                    module: struct_decl.module,
                    rep_public: struct_decl.rep_public,
                    polarities: struct_decl.polarities,
                },
            );
        }
        if let Some(concept) = context.concept(&def.name).cloned() {
            context.update_concept(
                &def.name,
                ConceptDecl {
                    universe_context: universe_context.clone(),
                    params: stamp(context, &concept.params, &owned, free, &instance)?,
                    fields: concept.fields,
                    supers: concept.supers,
                },
            );
        }
    }

    let definitions = defs
        .iter()
        .zip(types)
        .zip(bodies)
        .map(|((def, type_), body)| Definition {
            name: def.name.clone(),
            kind: def.kind.clone(),
            universe_context: universe_context.clone(),
            island: def.island.clone(),
            totality: def.totality,
            type_,
            body,
        })
        .collect();
    let rec = RecItem::try_new(definitions)?;
    // Before the members are defined into the context, because the shape this rejects makes reducing a *use* of one of them overflow the stack.
    check_rec_item_totality(context, &rec)?;

    for (index, definition) in rec.definitions.iter().enumerate() {
        let name = Free::from(&definition.name);
        context.reassume(&name, &rec.group.member_type(index));
        context.define(
            &name,
            &Term::rec_proj(rec.group.clone(), index),
            Some(&definition.kind),
        );
        context.set_assumption_universe_context(&name, universe_context.clone());
        if context.is_witness_declaration(&definition.name) {
            context.update_witness_scheme(
                &definition.name,
                universe_context.clone(),
                rec.group.member_type(index),
            );
        }
    }

    // Classify the whole group at once: its members may mention each other, so no member's verdict is settled until the group's descent is.
    let group = group_totality(context, &rec.group);
    for definition in rec.definitions() {
        record_definition_totality(context, &definition, group);
    }

    Ok(rec)
}

/// Elaborate one persistent module item and perform every item-boundary obligation. Both module drivers use this path so universe transactions, parked work, witnesses, privacy islands, and error attribution cannot drift.
fn elaborate_module_item(context: &mut Context, item: &Item) -> Result<Item, Error> {
    curios_profile::profile!("elaborate_module_item");
    let item_module = match item {
        Item::Let(definition) => definition.island.clone(),
        Item::Rec(rec) => rec.island(),
    };
    context.set_island(item_module);

    let item_names = item.describe();
    // One span per top-level item, aggregated per item rather than across all of them: the fixed prelude is 1079 declarations whose costs differ by three orders of magnitude, and a single averaged row cannot say which one a pass is actually spending on.
    //
    // This used to be a hand-rolled span carrying `name` and `steps` as fields, on the belief that its close event attributed elapsed time by name. It did not: `capture`'s collector keeps a span's *metadata* and discards its attribute values, implements no `on_record` at all, and visits exactly one event field — so both were written and dropped on every one of those 1079 declarations. `profile_group!` is that attribution actually implemented.
    curios_profile::profile_group!("declaration", item_names);

    // The step budget is per declaration, so it is restored here rather than drained across the module: whether this item typechecks must not depend on how much the items before it happened to spend.
    context.restore_budget();

    let elaborated = match item {
        Item::Let(definition) => elaborate_module_let(context, definition),
        Item::Rec(rec) => elaborate_module_rec(context, rec).map(Item::Rec),
    }
    // Attribute *every* failure to the item that caused it, not only universe invariants. A whole-module diagnostic with no declaration name — an exhausted budget or an effect reduced at the type level — costs a full prelude rebuild to localize, which is the expensive way to learn one string.
    .map_err(|error| error.in_declaration(&item_names))?;

    retry_deferred_witnesses(context)?;
    context.drain_parked()?;
    context.finish_universe_transaction();

    Ok(elaborated)
}

/// Refuse two witnesses that resolve each other, naming the cycle rather than leaving it to the kernel.
///
/// A witness may recurse through its own entry — `elaborate_module_let` registers the declaration before the body for that purpose, and such a witness becomes a group of one. A cycle between *two* has no such binding: the lowerer orders a witness before the uses that dispatch through it (`curios-text/src/into_core/order.rs`), and when two witnesses each want the other first that preference deadlocks and is dropped, so one is emitted naming a witness that does not exist yet. The kernel then refuses it as `unbound name /witness@N`, which names an anonymous id and says nothing a reader can act on.
///
/// Checked over the *emitted* order, which is the order the kernel will walk. Both directions are required before the report claims a cycle: a witness merely referencing a later one is ordinary and already works, because the ordering pass puts the referent first.
fn check_witness_cycles(context: &mut Context, items: &[Item]) -> Result<(), Error> {
    // Each witness definition with the item that holds it, and its zonked body: a witness reached through the deferred store arrives as a *solved metavariable*, so an unzonked body does not mention the name yet — the module's own zonk runs later, in `finalize_and_check`. A reference inside a group is bound by that group rather than pointing outward, which is why the item position travels along.
    let mut declared: Vec<(Free, usize, Definition, Term)> = Vec::new();
    for (position, item) in items.iter().enumerate() {
        let definitions = match item {
            Item::Let(definition) => vec![definition.clone()],
            Item::Rec(rec) => rec.definitions(),
        };
        for definition in definitions {
            if context.is_witness_declaration(&definition.name) {
                let body = zonk_solved_term_metas(context, &definition.body);
                declared.push((Free::from(&definition.name), position, definition, body));
            }
        }
    }

    for (this_name, this_at, this, this_body) in &declared {
        for (that_name, that_at, that, that_body) in &declared {
            // One report per cycle, raised from the earlier member, and never for two members of one group.
            if this_at >= that_at
                || !this_body.free_vars().contains(that_name)
                || !that_body.free_vars().contains(this_name)
            {
                continue;
            }

            return Err(Error::witness_cycle(this.type_.clone(), that.type_.clone())
                .at_opt(this.type_.span()));
        }
    }

    Ok(())
}

/// Elaborate a [`Module`] against an already-elaborated scope, returning what it added. Each top-level item is checked and `define`d *cumulatively in the persistent base frame* — never a popped `with_frame` — so every definition stays in scope for later items, the entrypoint `body`, and (through `mode`) its type annotation. Returns the rebuilt suffix alongside the body's type, reduced through the accumulated definitions.
///
/// Elaboration is authoritative: the returned module — not the lowered input — is what `zonk_module` then makes meta-free for `erase`.
///
/// **One protocol serves both entry points**, and that is the point. `established` is [`Established::nothing`] for a from-scratch elaboration and the cached prelude for a replay; every step below degenerates to the whole-module reading when it is empty, so there is no second implementation to keep in agreement. There used to be: the replay path transcribed this function's thirteen steps inline and threaded the prelude through them, and the two copies agreeing was maintained by reading. That is the shape every configuration-dependent defect in this subsystem has had — a rule correct in the configuration its author was looking at and wrong in the other one.
///
/// The scope diverges at exactly one point: after [`check_concept_registry`], its definitions are *replayed* into the context rather than elaborated.
fn elaborate_module_suffix(
    context: &mut Context,
    established: Established<'_>,
    module: &Module,
    metavar_floor: usize,
    universe_floor: usize,
    mode: Mode,
) -> Result<(Module, Option<Term>), Error> {
    curios_profile::profile!("elaborate_module_suffix");
    // What is already in scope goes in before any item is checked; `register_*` rejects a duplicate key, and the unit declares only its own, so the two cannot collide.
    established.seed_registries(context)?;
    // The unit's own mounts, after its scope's. Mount sets are pairwise disjoint, so the order only fixes which entry a lookup finds first among equals, and there are none.
    context.mount(&module.mounts);

    // Kept so the rebuilt entries can be pulled back out below. `module` declares only its own, prefix or no prefix, so this is every key it has.
    let induct_keys = module.induct_decls.keys().cloned().collect::<Vec<_>>();
    let struct_keys = module.struct_decls.keys().cloned().collect::<Vec<_>>();

    for name in &induct_keys {
        context.register_induct(name, module.induct_decls[name].clone())?;
    }
    for name in &struct_keys {
        context.register_struct(name, module.struct_decls[name].clone())?;
    }
    // Concept metadata and witness markers, alongside — witness *table* entries register per item (`elaborate_module_let`), once the elaborated head exists. With every concept present, the superclass graph can be validated up front.
    for (name, concept) in &module.concepts {
        context.register_concept(name, concept.clone())?;
    }
    for name in &module.witnesses {
        context.mark_witness_declaration(name);
    }
    check_concept_registry(context)?;

    established.replay_definitions(context)?;

    // Implicit-argument insertion mints metavariables during elaboration; floor the counter above `into_core`'s (which returns the count alongside the lowered module) so the id spaces never collide. A cached prefix is meta-free, so its ids never collide with the user range either.
    context.seed_metavars(metavar_floor);
    context.set_local_floor(module.binder_floor);
    context.seed_universes(&module.universe_seeds, universe_floor);

    // Every item, because `module` carries only its own: the prefix arrived as scope through the replay above rather than as a run of leading items to skip.
    let mut items = Vec::with_capacity(module.items.len());
    for item in &module.items {
        items.push(elaborate_module_item(context, item)?);
    }

    // After every item, because a cycle is only visible once both halves have resolved: the second witness's goal against the first is deferred and lands between items.
    check_witness_cycles(context, &items)?;

    context.set_island(Qualifier::empty());
    // The entrypoint expression is not an item, so it gets its own budget on the same footing as one.
    context.restore_budget();
    // The annotation is a written type like any item's, and elaborating it is what makes it usable as an expectation: a universe-polymorphic head arrives instantiated, and an application of a type former reduces to the intrinsic it denotes. Left raw it stayed exactly as lowered, so `List(Nat)` reached conversion as an `Apply` no unfolding could reconcile with the inferred `Intrinsic::ListType` — a mismatch reported between two spellings of the same type. Elaborating here rather than in the caller keeps it in the frame every item was just defined into, which is the scope its globals resolve against.
    //
    // The rebuilt module carries the elaborated spelling, because `Module::type_` is what the kernel rechecks the entrypoint against and what `zonk` walks: a raw annotation would put the two checkers on different terms and hand zonk one that never passed through elaboration. Only a *written* annotation is kept, so a synthesized expectation leaves `type_` as absent as the program wrote it.
    let (mode, annotation) = match mode {
        Mode::Check(expected) => {
            let elaborated = check_is_sort(context, &expected)?.0;
            let annotation = module.type_.is_some().then(|| elaborated.clone());
            (Mode::Check(elaborated), annotation)
        }
        Mode::Infer => (Mode::Infer, None),
    };
    // A unit with no entrypoint has none to elaborate and no type for one — the two are the same fact, which is why they travel together from here to the kernel.
    let (body, body_type) = match &module.body {
        Some(body) => {
            let (body, body_type) = elaborate(context, body, mode)?;
            (Some(body), Some(body_type))
        }
        None => (None, None),
    };
    // The whole program has elaborated: a witness goal still deferred will never find a table entry — report it now.
    finish_deferred_witnesses(context)?;
    context.drain_parked()?;
    let body_type = body_type
        .map(|body_type| reduce_with(context, &body_type))
        .transpose()?;

    // The output carries the *rebuilt* registry entries (pulled back from the context, where the per-group rebuild re-registered them), so `zonk_module` and `erase` see elaborated telescopes. An entry whose declaring item was pruned keeps its lowered form — nothing consults it.
    let induct_decls = induct_keys
        .into_iter()
        .map(|name| {
            let induct_decl = context
                .induct_decl(&name)
                .expect("every suffix entry was registered above")
                .clone();
            (name, induct_decl)
        })
        .collect();
    let struct_decls = struct_keys
        .into_iter()
        .map(|name| {
            let struct_decl = context
                .struct_decl(&name)
                .expect("every suffix entry was registered above")
                .clone();
            (name, struct_decl)
        })
        .collect();
    let concepts = module
        .concepts
        .keys()
        .map(|name| {
            (
                name.clone(),
                context
                    .concept(name)
                    .expect("every suffix concept was registered above")
                    .clone(),
            )
        })
        .collect();
    let witnesses = module.witnesses.clone();

    let module = Module {
        items,
        mounts: module.mounts.clone(),
        universe_seeds: module.universe_seeds.clone(),
        induct_decls,
        struct_decls,
        concepts,
        witnesses,
        binder_floor: module.binder_floor,
        type_: annotation,
        body,
    };

    Ok((module, body_type))
}

/// Finalize an elaborated module and run the **soundness perimeter** over it.
///
/// This is the single place every whole-module check the consistency claim rests on is applied, and every entry point that produces an elaborated module must come through it. Keeping the sequence in one function is not tidiness: the checks were previously written out at each entry point, so "what does soundness depend on?" was answered by diffing two call sites, and a check added to one and not the other would have degraded the claim silently for every real compilation.
///
/// The order is load-bearing, in three places:
///
/// - `default_universes` then `zonk_module` come first, which is what makes everything after metavariable-free by construction — `zonk_module` errors on an unsolved hole, so no metavariable can later be solved to a partial or negatively-occurring term. `zonk_module` also runs `validate_universes`.
/// - [`Context::restore_budget`] precedes the passes because they reduce, and each has to spend on the same footing as an item rather than on whatever the last item left.
/// - [`record_totality`] precedes both gates, which read the flags it stamps.
///
/// `check_positivity` is independent of the rest and could sit anywhere after the zonk. `inherited` carries the classifications of a replayed prefix, whose own verdicts were settled when its archive was built; it is empty for a from-scratch elaboration, where the module defines every name it mentions.
fn finalize_and_check(
    context: &mut Context,
    mut module: Module,
    body_type: Option<Term>,
    inherited: &BTreeMap<Global, Totality>,
) -> Result<(Module, Option<Term>, Vec<Error>), Error> {
    curios_profile::profile!("finalize_and_check");
    let mut entry_terms = Vec::new();
    let has_body = module.body.is_some();
    let has_annotation = module.type_.is_some();
    if let Some(body) = &module.body {
        entry_terms.push(body.clone());
    }
    if let Some(type_) = &module.type_ {
        entry_terms.push(type_.clone());
    }
    if let Some(body_type) = body_type {
        entry_terms.push(body_type);
    }
    let mut entry_terms = context
        .default_universes(&entry_terms.iter().collect::<Vec<_>>())?
        .into_iter();
    if has_body {
        module.body = Some(entry_terms.next().expect("entry body was finalized"));
    }
    if has_annotation {
        module.type_ = Some(entry_terms.next().expect("entry annotation was finalized"));
    }
    let body_type = entry_terms.next();

    // Written goals report as one complete batch before zonking: collection meets exactly the set strict zonk would (committed solutions included), so a goal-bearing program fails with every goal located rather than with the first (`collect_goal_reports`).
    let goal_reports = collect_goal_reports(context, &module);
    if !goal_reports.is_empty() {
        return Err(Error::goals(goal_reports));
    }

    let mut module = zonk_module(context, &module)?;
    let body_type = body_type
        .map(|body_type| zonk(context, &body_type))
        .transpose()?;
    context.restore_budget();

    // Positivity gates the zonked registries rather than running inside elaboration: the telescopes it reads are final here, and meta-free, so an unsolved hole reports as an unsolved hole instead of as an unseeable occurrence. At a replay the module in hand is the suffix alone, which is what this must see — the replayed prefix carries the vectors its archive was built with, and since prefix items cannot mention the suffix they are sinks of the occurrence relation, so no cycle crosses the boundary.
    check_positivity(context, &mut module)?;
    record_totality(context, &mut module, inherited);

    // Reported rather than raised. `curios-cert` decides these same two obligations independently, and a fixture this checker refuses must still be able to reach it — a short circuit here would return no module at all, leaving "would the kernel have caught it?" unobservable, which is exactly the quadrant the trusted base most needs to see. The public entry points raise the first verdict, so nothing on the compile path is weakened.
    // One cache across both: they are seeded from the same recorded entries, so a cache each zonks every distinct recorded type twice.
    let mut zonked = Zonked::default();
    let obligations = [
        check_type_totality(context, &module, inherited, &mut zonked),
        check_proof_totality(context, &module, inherited, &mut zonked),
    ]
    .into_iter()
    .filter_map(Result::err)
    .collect();

    Ok((module, body_type, obligations))
}

/// The first erasure-obligation verdict, as an error — how every caller but the two-checker fixture harness consumes [`finalize_and_check`]'s report.
fn raise(outcome: (Module, Option<Term>, Vec<Error>)) -> Result<(Module, Option<Term>), Error> {
    let (module, body_type, obligations) = outcome;
    match obligations.into_iter().next() {
        Some(error) => Err(error),
        None => Ok((module, body_type)),
    }
}

/// Elaborate a whole [`Module`] with no cached prefix, then zonk and check it.
///
/// The paired operation exists so a cached module and its body type can never come from different metavariable stores.
pub fn elaborate_and_zonk_module(
    context: &mut Context,
    module: &Module,
    metavar_floor: usize,
    universe_floor: usize,
    mode: Mode,
) -> Result<(Module, Option<Term>), Error> {
    curios_profile::profile!("elaborate_and_zonk_module");
    grown(|| elaborate_and_zonk_module_within(context, module, metavar_floor, universe_floor, mode))
}

fn elaborate_and_zonk_module_within(
    context: &mut Context,
    module: &Module,
    metavar_floor: usize,
    universe_floor: usize,
    mode: Mode,
) -> Result<(Module, Option<Term>), Error> {
    let (module, body_type) = elaborate_module_suffix(
        context,
        Established::nothing(),
        module,
        metavar_floor,
        universe_floor,
        mode,
    )?;
    // Nothing is inherited: `module` is the whole program, so every name it mentions it also defines.
    raise(finalize_and_check(
        context,
        module,
        body_type,
        &BTreeMap::new(),
    )?)
}

/// Elaborate one [`Module`] against the units already established, then zonk and check it.
///
/// `established` is that scope: the modules elaborated ahead of this one, in dependency order and *borrowed* rather than merged. What a predecessor contributes is its nominal registries, its mounts and its recorded totality verdicts, seeded into the [`Context`] before the unit's first item is checked — never its items.
///
/// Sound because a scope is unit-independent: its items never see this unit's code, and — since top-level definitions are excluded from a metavariable's Γ (`Context::identity_snapshot`) — an item elaborates against the identical local context it would with no scope at all, so the solutions (and the zonked output) are identical.
///
/// **The returned module holds this unit's items and no one else's.** That is a contract rather than an artifact of how the elaboration happens to be written: [`crate::erase_unit`] erases a unit *onto* what its scope already erased, and re-deriving the standard library on every compilation is exactly what carrying it here would cost. The one quantity that combines is the binder floor, which is a bound rather than a set and is taken as the maximum of the scope's and this unit's.
pub fn elaborate_and_zonk_unit(
    context: &mut Context,
    established: Established<'_>,
    module: &Module,
    metavar_floor: usize,
    universe_floor: usize,
    mode: Mode,
) -> Result<(Module, Option<Term>), Error> {
    let (module, body_type, obligations) = elaborate_and_zonk_unit_reporting(
        context,
        established,
        module,
        metavar_floor,
        universe_floor,
        mode,
    )?;

    raise((module, body_type, obligations))
}

/// [`elaborate_and_zonk_unit`] with the erasure obligations *reported* rather than raised.
///
/// Elaboration is no less checked: everything that decides whether the program is well-typed still short-circuits. What is handed back instead of thrown is the pair of obligations `curios-cert` also decides — (T) and (V) — and the reason is that a fixture this checker refuses must still be able to reach the kernel. Raising leaves no module, so whether the kernel would have refused the same program is unobservable, and that is precisely the disagreement worth seeing: an obligation only this side enforces is the trusted base resting on an elaborator-only analysis. The two-checker fixture harness in `curios` is the caller; every other caller wants [`elaborate_and_zonk_unit`].
pub fn elaborate_and_zonk_unit_reporting(
    context: &mut Context,
    established: Established<'_>,
    module: &Module,
    metavar_floor: usize,
    universe_floor: usize,
    mode: Mode,
) -> Result<(Module, Option<Term>, Vec<Error>), Error> {
    curios_profile::profile!("elaborate_and_zonk_with_prelude");
    let (suffix, body_type) = elaborate_module_suffix(
        context,
        established,
        module,
        metavar_floor,
        universe_floor,
        mode,
    )?;
    // The scope's own stamps come out of the archive already closed, so inheriting them is what lets a user proof see that `/std/Async/bind` is partial without walking `/std` again.
    let inherited = established.recorded_totality();
    let (suffix, body_type, obligations) =
        finalize_and_check(context, suffix, body_type, &inherited)?;

    // Nothing is merged back in. The entry's items, the entry's declarations: what the prelude contributes is scope, and every consumer past this point takes it as such — `Globals` at the certifier, a replayed context at erasure. A whole-module pass that needs the complete declaration set gets it by being handed both halves (`curios_analysis::Declarations`), not by being handed one map somebody concatenated.
    //
    // The binder floor is the exception, and it is a bound rather than a set: the entry's terms are elaborated against prelude terms whose binders were minted in an earlier compiler run, so the floor has to clear both. Combining by maximum can only ever widen.
    let module = Module {
        binder_floor: established.binder_floor().max(suffix.binder_floor),
        ..suffix
    };

    Ok((module, body_type, obligations))
}
