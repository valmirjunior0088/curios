use {
    super::{Bound, Context, Error, Mode, check, elaborate},
    crate::{
        Concept, Definition, DefinitionKind, Free, FuncType, Global, InductDecl, InductParam, Item,
        Level, Module, RecItem, SelfReference, StructDecl, Subterm, Telescope, Term,
        UniverseConstraintKind, UniverseConstraintOrigin, UniverseContext, UniverseMetaId, Visit,
        check_concept_registry, finish_deferred_witnesses, is_prop, reduce_with, register_witness,
        retry_deferred_witnesses, sort_term, zonk, zonk_module, zonk_solved_term_metas,
    },
    curios_base::Qualifier,
    std::{cell::RefCell, collections::BTreeSet, rc::Rc},
};

/// The universe arguments `value` applies to `name`'s declaration, at the first
/// occurrence that carries them.
///
/// An application reaches finalization in either of two forms: the nominal
/// normal form, which carries its instance in [`StructType`]'s or
/// [`InductType`]'s own universe vector, or an explicit instance on an
/// unreduced occurrence of the type-former. Both name the same levels in the
/// same order, so either answers the question.
///
/// The two nominal cases are separate arms rather than one, because the node a
/// declaration normalizes to is what distinguishes a concept from an inductive
/// here. Nothing about the *name* does — deciding by inspecting `name` would be
/// guessing at a fact the term already states.
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
                Subterm::UniverseInst(instance)
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

/// Walk a (params-first) telescope, checking each binder's type against `Type`
/// under the earlier binders (fresh-gensym, assume), and return the rebuilt
/// `(label, type)` entries alongside the telescope's terminal — opened under
/// those binders. Runs in the caller's frame; the same gensym-then-relabel
/// discipline as `elaborate_tuple_type`.
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
        let mut position = 0;
        let mut telescope = telescope.clone();
        while let Telescope::Cons(domain, rest) = telescope {
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
        Ok(())
    })
}

/// Rebuild a registry entry's `params`/`indices` telescopes with *elaborated*
/// types. `into_core` records the declaration's lowered spellings, and a lowered
/// type must never leak into later reduction: implicit insertion saturates
/// applications during elaboration, and an under-applied index type (e.g.
/// `Eq(0, 0)` against `Eq`'s 3-ary type constructor) would open a telescope at
/// the wrong arity the first time `reduce` meets the registry copy.
///
/// Called from `elaborate_module_rec` after the group's signatures are
/// reassumed rebuilt and *before* any body is checked — index types may
/// mention the group's own members (resolved through the assumed signatures),
/// and the type-constructor bodies' `InductType` nodes check their arguments
/// against this very telescope. A name with no registry entry is an ordinary
/// binding; no-op.
fn elaborate_induct_indices(context: &mut Context, name: &Global) -> Result<(), Error> {
    let Some(induct_decl) = context.induct_decl(name).cloned() else {
        return Ok(());
    };

    let n_params = induct_decl.params.len();
    let labels = induct_decl
        .indices
        .labels()
        .iter()
        .map(|label| label.to_string())
        .collect::<Vec<_>>();

    // Walk the full (params-first) index telescope, checking each entry type
    // against `Type` under the earlier binders.
    let (entries, ()) = context
        .with_frame(|context| check_telescope_entries(context, induct_decl.indices.clone()))?;

    let label_refs = labels.iter().map(String::as_str).collect::<Vec<_>>();
    let params =
        Telescope::build(entries[..n_params].iter().cloned(), ()).relabel(&label_refs[..n_params]);
    let indices = Telescope::build(entries, ()).relabel(&label_refs);

    context.update_induct(
        name,
        InductDecl {
            universe_context: induct_decl.universe_context,
            params,
            indices,
            constructors: induct_decl.constructors,
            result_sort: induct_decl.result_sort,
            module: induct_decl.module,
            root: induct_decl.root,
            rep_public: induct_decl.rep_public,
        },
    );

    Ok(())
}

/// Rebuild a registry entry's constructor signatures with *elaborated* types —
/// the second phase of the registry rebuild (see
/// [`elaborate_induct_indices`]). Payload types may apply the inductive group's
/// type constructors, so this runs from `elaborate_module_rec` only after the
/// group's rebuilt bodies are defined; each terminal — the constructed
/// `InductType` normal form — routes through `elaborate_induct_type`, which
/// checks the parameters and the case's target indices against the
/// already-rebuilt index telescope and returns another `InductType` node, the
/// shape `case_target_indices` and the match elaborators rely on.
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

        let (entries, terminal) = context.with_frame(|context| {
            let (entries, terminal) = check_telescope_entries(context, signature.clone())?;
            let terminal = crate::check_is_sort(context, &terminal)?.0;
            Ok::<_, Error>((entries, terminal))
        })?;

        let label_refs = labels.iter().map(String::as_str).collect::<Vec<_>>();
        constructors.push((
            tag.clone(),
            InductParam {
                telescope: Telescope::build(entries, terminal).relabel(&label_refs),
                // Plicity is metadata parallel to the telescope; elaboration
                // re-checks the types but never changes the calling convention.
                plicities: param.plicities.clone(),
            },
        ));
    }

    context.update_induct(
        name,
        InductDecl {
            universe_context: induct_decl.universe_context,
            params: induct_decl.params,
            indices: induct_decl.indices,
            constructors,
            result_sort: induct_decl.result_sort,
            module: induct_decl.module,
            root: induct_decl.root,
            rep_public: induct_decl.rep_public,
        },
    );

    Ok(())
}

/// Rebuild a struct's registry telescopes with *elaborated* types, so the field
/// types `erase` and later construction sites consult are saturated (implicit
/// insertion) and reduce correctly — the struct analogue of
/// [`elaborate_induct_indices`], over the single (params-first) field
/// telescope. Called from `elaborate_module_let` once the type-former is
/// defined (field types may mention the struct itself and earlier items). A
/// name with no registry entry is an ordinary binding; no-op.
fn elaborate_struct(context: &mut Context, name: &Global) -> Result<(), Error> {
    let Some(struct_decl) = context.struct_decl(name).cloned() else {
        return Ok(());
    };

    let n_params = struct_decl.params.len();
    let labels = struct_decl
        .fields
        .labels()
        .iter()
        .map(|label| label.to_string())
        .collect::<Vec<_>>();

    let declared_prop = matches!(
        &*reduce_with(context, &struct_decl.result_sort)?,
        Subterm::Prop
    );

    let (entries, ()) = context.with_frame(|context| -> Result<_, Error> {
        let (entries, ()) = check_telescope_entries(context, struct_decl.fields.clone())?;

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
                    return Err(Error::informative_prop_struct(
                        name.symbol(),
                        field,
                        ty.clone(),
                    ));
                }
            }
        }

        Ok((entries, ()))
    })?;

    let label_refs = labels.iter().map(String::as_str).collect::<Vec<_>>();
    let params =
        Telescope::build(entries[..n_params].iter().cloned(), ()).relabel(&label_refs[..n_params]);
    let fields = Telescope::build(entries, ()).relabel(&label_refs);
    add_declaration_sizing(
        context,
        name,
        &fields,
        n_params,
        &struct_decl.result_sort,
        UniverseConstraintKind::FieldSizing,
    )?;

    context.update_struct(
        name,
        StructDecl {
            universe_context: struct_decl.universe_context,
            params,
            fields,
            result_sort: struct_decl.result_sort,
            module: struct_decl.module,
            root: struct_decl.root,
            rep_public: struct_decl.rep_public,
        },
    );

    Ok(())
}

/// The levels of a declaration's result sort that occur nowhere a use site can
/// reach — the levels no occurrence could ever choose.
///
/// `id : (A : Type u) -> A -> A` is genuinely polymorphic: a caller picks `A`,
/// and with it `u`. `Lte : Nat -> Nat -> Type u` is not. Nothing at a use site
/// supplies that `u`, so generalizing it mints a parameter every occurrence
/// must instantiate for no benefit — which is how a proof about naturals ends
/// up universe-polymorphic, and how a string literal's per-byte constructor
/// applications each mint fresh levels.
///
/// Minimizing instead is sound for two reasons. Cumulativity already lets a
/// declaration sitting at `Type 0` be used where a higher universe is expected,
/// so the parameter buys no expressiveness. And `minimize` takes least
/// solutions *subject to the recorded constraints*, so a level that genuinely
/// depends on an argument still lands there: `List(A : Type u) : Type v` keeps
/// `v = u`, because constructor sizing constrains it, rather than collapsing to
/// zero.
///
/// A level occurring in both a binder domain and the result sort stays in the
/// interface — the caller chooses it, and the result merely mentions it.
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
) -> Result<(UniverseContext, Term, Term), Error> {
    let type_ = zonk_solved_term_metas(context, &type_);
    let body = zonk_solved_term_metas(context, &body);

    // A method wrapper is not independently polymorphic. Its own type mentions
    // only a *subset* of its concept's levels — `pure` names some, `bind`
    // others — so generalizing it alone yields fewer parameters than the
    // concept has (`Monad` 5 against `pure` 2, `bind` 1). But the type also
    // carries the concept applied at *all* of them, through the `use w : C(..)`
    // binder, and the levels outside the wrapper's own generalized set then
    // have nothing to name them: `level w escapes its universe parameter
    // context`. So the wrapper must inherit the concept's whole context. Lean
    // and Rocq both hand a projection its structure's universe parameters
    // verbatim, for the same reason.
    // The owner comes from `into_core`, which records it where the wrapper is
    // generated; re-deriving it by splitting `name` would misread an ordinary
    // definition that merely happens to sit under a concept's namespace.
    // Inheriting the context is only half the work: those levels are still
    // metas, and ordinary finalization — the one thing that would solve them —
    // is exactly what must not run here, because it would mint the wrapper's
    // own parameters in its own order. `finalize_at_instance` binds the concept
    // application's arguments to the inherited parameters positionally instead.
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
    // A value constructor stands in the same relation to its inductive as a
    // method wrapper does to its concept, and fails the same way. `Result(S, F)`
    // has two universe parameters; `success` mentions only `S` in its payload,
    // so generalizing it alone mints its own `Param(0..1)` in meta-id order and
    // displaces the inductive's — which `generalize` then shifts up past the
    // declared count, leaving `Param(2)`/`Param(3)` with nothing to name them.
    // A constructor is not independently polymorphic: its universes *are* the
    // inductive's, so it inherits that context and binds its instance to those
    // parameters positionally.
    //
    // Like `ConceptMethod`, the owner is a field `into_core` records where the
    // constructor is generated, not something recovered from `name`.
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
        // A constructor of a *parameterless* inductive results in a bare
        // self-reference: an occurrence inside the group is monomorphic in the
        // group's own universes, and `stamp_declaration_instance` only gives it
        // an explicit instance after this finalization runs. So a missing
        // instance is not a malformed constructor — it denotes the identity
        // instance, which is what the stamp would write.
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
        let params = struct_decl.params.zonk(context)?;
        let fields = struct_decl.fields.zonk(context)?;
        let result_sort = zonk(context, &struct_decl.result_sort)?;
        context.update_struct(
            name,
            StructDecl {
                universe_context: struct_decl.universe_context,
                params,
                fields,
                result_sort,
                module: struct_decl.module,
                root: struct_decl.root,
                rep_public: struct_decl.rep_public,
            },
        );
    }
    if let Some(concept) = context.concept(name).cloned() {
        context.update_concept(
            name,
            Concept {
                universe_context: concept.universe_context,
                params: zonk_solved_term_metas(context, &concept.params),
                fields: concept.fields,
                supers: concept.supers,
                root: concept.root,
            },
        );
    }

    // The signature and any registry telescope form the declaration's
    // interface: a use site instantiates exactly these. Levels reachable only
    // through the body are internal classifiers and are minimized instead, as
    // are the levels of the result sort — see [`result_sort_only_metas`].
    let determined = result_sort_only_metas(context, &type_);
    let mut interface = context.universe_metas_in(&type_);
    interface.retain(|meta| !determined.contains(meta));
    if let Some(struct_decl) = context.struct_decl(name) {
        interface.extend(crate::universe_metas(&struct_decl.params));
        interface.extend(crate::universe_metas(&struct_decl.fields));
        interface.extend(struct_decl.result_sort.universe_metas());
    }
    if let Some(concept) = context.concept(name) {
        interface.extend(crate::universe_metas(&concept.params));
    }
    let mut internal = context.universe_metas_in(&body);
    internal.extend(determined);

    let universe_context = context.finalize_universe_metas(interface, internal)?;
    let levels = universe_context.identity_instance();
    let owned = BTreeSet::from([name.clone()]);
    // A non-recursive definition's own name stays free in its signature, body,
    // and registry entry: nothing captures it, so each occurrence carries the
    // instance itself.
    let free = SelfReference::Free;
    let type_ = crate::stamp_declaration_instance(
        &context.zonk_universe_levels(&type_)?,
        &owned,
        free,
        &levels,
    );
    let body = crate::stamp_declaration_instance(
        &context.zonk_universe_levels(&body)?,
        &owned,
        free,
        &levels,
    );

    if let Some(struct_decl) = context.struct_decl(name).cloned() {
        context.update_struct(
            name,
            StructDecl {
                universe_context: universe_context.clone(),
                params: crate::stamp_declaration_instance(
                    &context.zonk_universe_levels(&struct_decl.params)?,
                    &owned,
                    free,
                    &levels,
                ),
                fields: crate::stamp_declaration_instance(
                    &context.zonk_universe_levels(&struct_decl.fields)?,
                    &owned,
                    free,
                    &levels,
                ),
                result_sort: crate::stamp_declaration_instance(
                    &context.zonk_universe_levels(&struct_decl.result_sort)?,
                    &owned,
                    free,
                    &levels,
                ),
                module: struct_decl.module,
                root: struct_decl.root,
                rep_public: struct_decl.rep_public,
            },
        );
    }
    if let Some(concept) = context.concept(name).cloned() {
        context.update_concept(
            name,
            Concept {
                universe_context: universe_context.clone(),
                params: crate::stamp_declaration_instance(
                    &context.zonk_universe_levels(&concept.params)?,
                    &owned,
                    free,
                    &levels,
                ),
                fields: concept.fields,
                supers: concept.supers,
                root: concept.root,
            },
        );
    }

    Ok((universe_context, type_, body))
}

/// Type-check a single non-recursive top-level definition, `define` it into the
/// *current* (persistent base) frame, and return its rebuilt form. The flat
/// analogue of `elaborate_let`'s per-binding work, minus the `with_frame`/tail
/// recursion: the binding must stay in scope for every later item and the
/// entrypoint body. The *rebuilt* body is `define`d (implicit insertion makes
/// the lowered one no longer interchangeable; see the comment below), and the
/// rebuilt `Definition` flows on to `zonk`/`erase`.
fn elaborate_module_let(context: &mut Context, def: &Definition) -> Result<Definition, Error> {
    // Γ and the definition store key on the free-variable identity; the nominal
    // registries key on the definition's own name.
    let name = Free::from(&def.name);
    let type_ = crate::check_is_sort(context, &def.type_)?.0;

    // A witness declaration registers into the program-wide table as soon as
    // its signature is known — *before* its body elaborates, so a recursive
    // witness (a `Show(Tree)` whose fields show subtrees) can resolve through
    // its own entry.
    if context.is_witness_declaration(&def.name) {
        register_witness(
            context,
            &def.name,
            &type_,
            def.universe_context.clone(),
            &def.island,
            def.root,
        )
        .map_err(|error| error.at_opt(def.type_.span()))?;
    }

    let body = check(context, &def.body, type_.clone())?;
    context.sweep_parked()?;

    // Define the *rebuilt* body at the *rebuilt* type, not the lowered ones:
    // implicit-argument insertion saturates applications during elaboration,
    // and the untyped reducer (type-level evaluation in later items' types)
    // would meet a lowered form's under-applied calls and open a telescope at
    // the wrong arity. Pre-insertion the two were interchangeable; no longer.
    context.define_assuming(&name, &type_, &body, Some(&def.kind));

    // A struct's type-former lowers to a standalone `let`; rebuild its registry
    // telescopes now that the former is defined (no-op for an ordinary let).
    elaborate_struct(context, &def.name)?;

    let (universe_context, type_, body) =
        finalize_definition(context, &def.name, &def.kind, type_, body)?;
    context.reassume(&name, &type_);
    context.define(&name, &body, Some(&def.kind));
    context.set_assumption_universe_context(&name, universe_context.clone());
    if context.is_witness_declaration(&def.name) {
        context.update_witness_scheme(&def.name, universe_context.clone(), type_.clone());
    }

    Ok(Definition {
        name: def.name.clone(),
        kind: def.kind.clone(),
        universe_context,
        island: def.island.clone(),
        root: def.root,
        type_,
        body,
    })
}

/// Type-check a flat top-level `rec` item and return its rebuilt structural
/// group. The input is opened over the export names for elaboration; the output
/// captures those names back into one [`RecItem`] and publishes each export as
/// its folded structural member.
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
        types.push(crate::check_is_sort(context, &def.type_)?.0);
    }

    // Upgrade the assumptions to the *rebuilt* signatures before any body is
    // checked (see `elaborate_rec`): a lowered (under-applied) type must not
    // leak into later reduction. The lowered forms were only needed above,
    // while the signatures checked each other.
    for (name, type_) in names.iter().zip(&types) {
        context.reassume(name, type_);
    }

    // An inductive's type bindings always lower as one `rec` group whose member
    // names are the registry keys. Rebuild the registry index telescopes here
    // — after the rebuilt signatures are assumed (index types may mention the
    // group), before any body's `InductType` node checks against them.
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
        let body = check(context, &def.body, type_.clone())?;
        context.fill_rec_slot(slot, body.clone());
        bodies.push(body);
    }
    context.retry_parked()?;

    // Registry rebuild, phase two: constructor payload types may apply the
    // group's type constructors, so their signatures (and `InductType`
    // terminals) elaborate only now that the rebuilt bodies are defined.
    for def in &defs {
        elaborate_induct_constructors(context, &def.name)?;
    }
    for def in &defs {
        if let Some(induct_decl) = context.induct_decl(&def.name).cloned() {
            for constructor in induct_decl.signatures() {
                add_declaration_sizing(
                    context,
                    &def.name,
                    &constructor.telescope,
                    induct_decl.params.len(),
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
                params: zonk_solved_term_metas(context, &induct_decl.params),
                indices: zonk_solved_term_metas(context, &induct_decl.indices),
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
                root: induct_decl.root,
                rep_public: induct_decl.rep_public,
            },
        );
    }

    // As for a single definition, the group's interface is its member
    // signatures and registry telescopes; levels reachable only through a
    // member body are internal and minimized rather than generalized.
    let mut interface = types
        .iter()
        .flat_map(|type_| context.universe_metas_in(type_))
        .collect::<BTreeSet<_>>();
    for def in &defs {
        if let Some(induct_decl) = context.induct_decl(&def.name) {
            interface.extend(crate::universe_metas(&induct_decl.params));
            interface.extend(crate::universe_metas(&induct_decl.indices));
            interface.extend(induct_decl.result_sort.universe_metas());
            for constructor in induct_decl.signatures() {
                interface.extend(crate::universe_metas(&constructor.telescope));
            }
        }
    }
    let internal = bodies
        .iter()
        .flat_map(|body| context.universe_metas_in(body))
        .collect::<BTreeSet<_>>();
    let universe_context = context.finalize_universe_metas(interface, internal)?;

    // The group's members are monomorphic in their own universes, so every
    // occurrence of one member inside the group — in a signature, a body, or a
    // rebuilt registry telescope — denotes the group's own instance. Those
    // occurrences were elaborated before the parameters existed and carry no
    // instance at all until this rewrite gives them one.
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
        Ok(crate::stamp_declaration_instance(
            &context.zonk_universe_levels(value)?,
            owned,
            self_reference,
            instance,
        ))
    }

    // `RecItem::try_new` captures the member names into the group's binder, so
    // the signatures and bodies reach it with their self-references bound.
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
                        // A registry telescope is stored outside the group and
                        // instantiated per use site, so its self-references
                        // are free and must carry the instance themselves.
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
        let params = stamp(context, &induct_decl.params, &owned, free, &instance)?;
        let indices = stamp(context, &induct_decl.indices, &owned, free, &instance)?;
        let result_sort = stamp(context, &induct_decl.result_sort, &owned, free, &instance)?;
        context.update_induct(
            &def.name,
            InductDecl {
                universe_context: universe_context.clone(),
                params,
                indices,
                constructors,
                result_sort,
                module: induct_decl.module,
                root: induct_decl.root,
                rep_public: induct_decl.rep_public,
            },
        );
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
            root: def.root,
            type_,
            body,
        })
        .collect();
    let rec = RecItem::try_new(definitions)?;

    for (index, definition) in rec.definitions.iter().enumerate() {
        let name = Free::from(&definition.name);
        context.reassume(&name, &rec.group.member_type(index));
        context.define(
            &name,
            &Term::rec_member(rec.group.clone(), index),
            Some(&definition.kind),
        );
        context.set_assumption_universe_context(&name, universe_context.clone());
    }

    Ok(rec)
}

/// Elaborate one persistent module item and perform every item-boundary
/// obligation. Both module drivers use this path so universe transactions,
/// parked work, witnesses, privacy islands, and error attribution cannot
/// drift.
fn elaborate_module_item(context: &mut Context, item: &Item) -> Result<Item, Error> {
    let item_module = match item {
        Item::Let(definition) => definition.island.clone(),
        Item::Rec(rec) => rec.island(),
    };
    context.set_island(item_module);

    let item_names = item.describe();
    // One span per top-level item is the natural unit for the fixed prelude:
    // its close event attributes elapsed time to a declaration by name, which
    // is the breakdown a whole-module timing cannot give.
    #[cfg(feature = "profile")]
    let _declaration = tracing::info_span!(
        target: "curios_core::declaration",
        "declaration",
        name = %item_names,
    )
    .entered();
    let elaborated = match item {
        Item::Let(definition) => elaborate_module_let(context, definition).map(Item::Let),
        Item::Rec(rec) => elaborate_module_rec(context, rec).map(Item::Rec),
    }
    // Attribute *every* failure to the item that caused it, not only universe
    // invariants. A whole-module diagnostic with no declaration name — a bare
    // deadline or an effect reduced at the type level — costs a full prelude
    // rebuild to localize, which is the expensive way to learn one string.
    .map_err(|error| error.in_declaration(&item_names))?;

    retry_deferred_witnesses(context)?;
    context.drain_parked()?;
    context.finish_universe_transaction();
    Ok(elaborated)
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
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub fn elaborate_module(
    context: &mut Context,
    module: &Module,
    metavar_floor: usize,
    universe_floor: usize,
    mode: Mode,
) -> Result<(Module, Term), Error> {
    // Seed the context's inductive registry before any item is checked: an
    // inductive's type-constructor and value-constructor definitions reference
    // their own registry entry (`elaborate_induct_type` / `elaborate_variant`).
    for (name, induct_decl) in &module.induct_decls {
        context.register_induct(name, induct_decl.clone())?;
    }

    // Likewise seed the struct registry — `elaborate_struct`/`elaborate_proj`
    // consult it (and `elaborate_struct` rebuilds each entry's telescopes).
    for (name, struct_decl) in &module.struct_decls {
        context.register_struct(name, struct_decl.clone())?;
    }

    // Concept metadata and witness markers, alongside — witness *table*
    // entries register per item (`elaborate_module_let`), once the elaborated
    // head exists. With every concept present, the superclass graph can be
    // validated up front.
    for (name, concept) in &module.concepts {
        context.register_concept(name, concept.clone())?;
    }
    for name in &module.witnesses {
        context.mark_witness_declaration(name);
    }
    check_concept_registry(context)?;

    // Implicit-argument insertion mints metavariables during elaboration;
    // floor the counter above `into_core`'s (which returns the count alongside
    // the lowered module) so the id spaces never collide.
    context.seed_metavars(metavar_floor);
    context.set_local_floor(module.binder_floor);
    context.seed_universes(&module.universe_seeds, universe_floor);

    let mut items = Vec::with_capacity(module.items.len());
    for item in &module.items {
        items.push(elaborate_module_item(context, item)?);
    }

    context.set_island(Qualifier::empty());
    let (body, body_type) = elaborate(context, &module.body, mode)?;
    // The whole program has elaborated: a witness goal still deferred will
    // never find a table entry — report it now.
    finish_deferred_witnesses(context)?;
    context.drain_parked()?;
    let body_type = reduce_with(context, &body_type)?;

    // The output module carries the *rebuilt* registry entries (pulled back
    // from the context, where the per-group rebuild re-registered them), so
    // `zonk_module` and `erase` see elaborated telescopes. An entry whose
    // declaring item was pruned keeps its lowered form — nothing consults it.
    let induct_decls = module
        .induct_decls
        .keys()
        .map(|name| {
            let induct_decl = context
                .induct_decl(name)
                .expect("every module entry was registered above")
                .clone();
            (name.clone(), induct_decl)
        })
        .collect();

    // Same for the struct registry: pull back the entries rebuilt by
    // `elaborate_struct` so `zonk_module`/`erase` see elaborated telescopes.
    let struct_decls = module
        .struct_decls
        .keys()
        .map(|name| {
            let struct_decl = context
                .struct_decl(name)
                .expect("every module entry was registered above")
                .clone();
            (name.clone(), struct_decl)
        })
        .collect();

    let module = Module {
        items,
        universe_seeds: module.universe_seeds.clone(),
        induct_decls,
        struct_decls,
        concepts: context.concepts().clone(),
        witnesses: module.witnesses.clone(),
        binder_floor: module.binder_floor,
        type_: module.type_.clone(),
        body,
    };

    Ok((module, body_type))
}

/// Elaborate and zonk a module and its inferred body type together. Artifact
/// construction uses this paired operation so the cached module and body type
/// can never come from different metavariable stores.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub fn elaborate_and_zonk_module(
    context: &mut Context,
    module: &Module,
    metavar_floor: usize,
    universe_floor: usize,
    mode: Mode,
) -> Result<(Module, Term), Error> {
    let (mut module, body_type) =
        elaborate_module(context, module, metavar_floor, universe_floor, mode)?;
    let mut entry_terms = vec![module.body.clone()];
    let has_annotation = module.type_.is_some();
    if let Some(type_) = &module.type_ {
        entry_terms.push(type_.clone());
    }
    entry_terms.push(body_type);
    let mut entry_terms = context
        .default_universes(&entry_terms.iter().collect::<Vec<_>>())?
        .into_iter();
    module.body = entry_terms.next().expect("entry body was finalized");
    if has_annotation {
        module.type_ = Some(entry_terms.next().expect("entry annotation was finalized"));
    }
    let body_type = entry_terms.next().expect("entry body type was finalized");
    let module = zonk_module(context, &module)?;
    let body_type = zonk(context, &body_type)?;
    Ok((module, body_type))
}

/// Elaborate a [`Module`] whose `sys`/`syn`/`std` prelude prefix is already
/// elaborated, reusing the cached result instead of re-type-checking it.
///
/// `prelude` is the elaborated + zonked prelude-only module — its `items` are
/// the whole prelude in dependency order (its trivial `body`/`type_` are
/// ignored). The lowered `module` still carries the *whole* program as
/// `text::into_core` produced it, and the prelude is its **leading prefix**: with
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
/// (`Context::identity_snapshot`) — a user item elaborates against the
/// identical local context it would under a from-scratch [`elaborate_module`], so
/// the solutions (and the zonked output) are identical. The cached prelude is
/// meta-free, so its ids never collide with the user metavariable range that
/// `seed_metavars(metavar_floor)` floors.
///
/// The returned module keeps that shape: its items are `prelude`'s own, cloned
/// unchanged and in order, followed by the user's, and its registries are
/// `prelude`'s extended in place. That is a contract, not an artifact of how the
/// splice happens to be written — [`crate::erase_module_with_prelude`]
/// skips the prefix by `prelude.items.len()` and reuses the prelude's already
/// projected terms for it rather than re-deriving the standard library on every
/// compilation.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub fn elaborate_and_zonk_with_prelude(
    context: &mut Context,
    prelude: &Module,
    module: &Module,
    metavar_floor: usize,
    universe_floor: usize,
    mode: Mode,
) -> Result<(Module, Term), Error> {
    // Seed the registries — cached prelude entries verbatim, then the user's
    // (rebuilt by elaboration below). Keep the user keys to pull their rebuilt
    // forms back out afterwards.
    for (name, induct_decl) in &prelude.induct_decls {
        context.register_induct(name, induct_decl.clone())?;
    }
    for (name, struct_decl) in &prelude.struct_decls {
        context.register_struct(name, struct_decl.clone())?;
    }
    for (name, concept) in &prelude.concepts {
        context.register_concept(name, concept.clone())?;
    }

    let user_induct_keys = module
        .induct_decls
        .keys()
        .filter(|name| !prelude.induct_decls.contains_key(*name))
        .cloned()
        .collect::<Vec<Global>>();
    let user_struct_keys = module
        .struct_decls
        .keys()
        .filter(|name| !prelude.struct_decls.contains_key(*name))
        .cloned()
        .collect::<Vec<Global>>();
    for name in &user_induct_keys {
        context.register_induct(name, module.induct_decls[name].clone())?;
    }
    for name in &user_struct_keys {
        context.register_struct(name, module.struct_decls[name].clone())?;
    }
    for (name, concept) in &module.concepts {
        if !prelude.concepts.contains_key(name) {
            context.register_concept(name, concept.clone())?;
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
                context.define_assuming_scheme(
                    &Free::from(&def.name),
                    &def.type_,
                    &def.body,
                    Some(&def.kind),
                    def.universe_context.clone(),
                );
                if prelude.witnesses.contains(&def.name) {
                    register_witness(
                        context,
                        &def.name,
                        &def.type_,
                        def.universe_context.clone(),
                        &def.island,
                        def.root,
                    )?;
                }
            }
            Item::Rec(rec) => {
                for (index, definition) in rec.definitions.iter().enumerate() {
                    let name = Free::from(&definition.name);
                    context.assume(&name, &rec.group.member_type(index));
                    context.set_assumption_universe_context(
                        &name,
                        rec.group.universe_context().clone(),
                    );
                    context.define(
                        &name,
                        &Term::rec_member(rec.group.clone(), index),
                        Some(&definition.kind),
                    );
                }
            }
        }
    }

    // User-minted metavariables sit strictly above `into_core`'s ids (which already
    // include the prelude's range); the cached prelude is meta-free, so nothing
    // collides.
    context.seed_metavars(metavar_floor);
    context.set_local_floor(module.binder_floor);
    context.seed_universes(&module.universe_seeds, universe_floor);

    // Elaborate only the user items — everything past the cached prelude prefix.
    let mut user_items = Vec::new();
    for item in module.items.iter().skip(prelude.items.len()) {
        user_items.push(elaborate_module_item(context, item)?);
    }

    context.set_island(Qualifier::empty());
    let (body, body_type) = elaborate(context, &module.body, mode)?;
    finish_deferred_witnesses(context)?;
    context.drain_parked()?;
    let body_type = reduce_with(context, &body_type)?;

    // Pull the rebuilt user registry entries back out (mirrors `elaborate_module`).
    let user_induct_decls = user_induct_keys
        .into_iter()
        .map(|name| {
            let induct_decl = context
                .induct_decl(&name)
                .expect("user entry registered")
                .clone();
            (name, induct_decl)
        })
        .collect();
    let user_struct_decls = user_struct_keys
        .into_iter()
        .map(|name| {
            let struct_decl = context
                .struct_decl(&name)
                .expect("user entry registered")
                .clone();
            (name, struct_decl)
        })
        .collect();

    // Zonk only the user portion (the cached prelude is already zonked), then
    // splice: cached prelude prefix ++ zonked user items / registries.
    let user_concepts = module
        .concepts
        .keys()
        .filter(|name| !prelude.concepts.contains_key(*name))
        .map(|name| {
            (
                name.clone(),
                context
                    .concept(name)
                    .expect("user concept registered")
                    .clone(),
            )
        })
        .collect();
    let user_witnesses = module
        .witnesses
        .iter()
        .filter(|name| !prelude.witnesses.contains(*name))
        .cloned()
        .collect();

    let mut user_module = Module {
        items: user_items,
        universe_seeds: module.universe_seeds.clone(),
        induct_decls: user_induct_decls,
        struct_decls: user_struct_decls,
        concepts: user_concepts,
        witnesses: user_witnesses,
        binder_floor: module.binder_floor,
        type_: module.type_.clone(),
        body,
    };
    let mut entry_terms = vec![user_module.body.clone()];
    let has_annotation = user_module.type_.is_some();
    if let Some(type_) = &user_module.type_ {
        entry_terms.push(type_.clone());
    }
    entry_terms.push(body_type);
    let mut entry_terms = context
        .default_universes(&entry_terms.iter().collect::<Vec<_>>())?
        .into_iter();
    user_module.body = entry_terms.next().expect("entry body was finalized");
    if has_annotation {
        user_module.type_ = Some(entry_terms.next().expect("entry annotation was finalized"));
    }
    let body_type = entry_terms.next().expect("entry body type was finalized");
    let user_module = zonk_module(context, &user_module)?;
    let body_type = zonk(context, &body_type)?;

    let mut items = prelude.items.clone();
    items.extend(user_module.items);
    let mut induct_decls = prelude.induct_decls.clone();
    induct_decls.extend(user_module.induct_decls);
    let mut struct_decls = prelude.struct_decls.clone();
    struct_decls.extend(user_module.struct_decls);
    let mut concepts = prelude.concepts.clone();
    concepts.extend(user_module.concepts);
    let mut witnesses = prelude.witnesses.clone();
    witnesses.extend(user_module.witnesses);

    let module = Module {
        items,
        universe_seeds: user_module.universe_seeds,
        induct_decls,
        struct_decls,
        concepts,
        witnesses,
        binder_floor: prelude.binder_floor.max(user_module.binder_floor),
        type_: user_module.type_,
        body: user_module.body,
    };

    Ok((module, body_type))
}
