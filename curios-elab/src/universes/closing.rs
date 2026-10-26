//! How a declaration's levels close into its scheme: by its kind, a method wrapper and a constructor inheriting their owner's, and the levels a family's instances hold sorted from those it leaves to its callers.

#[cfg(test)]
mod tests;

use {
    crate::{
        Context, Error, body_levels, compared_levels, family_levels, zonk, zonk_arity,
        zonk_solved_term_metas,
    },
    curios_core::{
        Bound, ConceptDecl, DefinitionKind, Free, FuncType, Global, SelfReference, StructDecl,
        Subterm, Telescope, Term, UniverseContext, UniverseMetaId, Universes, Visit,
        stamp_declaration_instance, universe_metas,
    },
    std::{cell::RefCell, collections::BTreeSet, rc::Rc},
};

/// The universe arguments `value` applies to `name`'s declaration, at the first occurrence that carries them.
///
/// An application reaches finalization in either of two forms: the nominal normal form, which carries its instance in [`StructType`](curios_core::StructType)'s or [`InductType`](curios_core::InductType)'s own universe vector, or an explicit instance on an unreduced occurrence of the type-former. Both name the same levels in the same order, so either answers the question.
///
/// The two nominal cases are separate arms rather than one, because the node a declaration normalizes to is what distinguishes a concept from an inductive here. Nothing about the *name* does — deciding by inspecting `name` would be guessing at a fact the term already states.
///
/// Memoized on node identity: the hook only latches the first occurrence, which a memoized walk still reaches first, and a finalized value can hold a solution stored as a reduct, whose tree can be exponential in its depth.
pub fn declaration_instance(value: &Term, name: &Global) -> Option<Universes> {
    let found = Rc::new(RefCell::new(None));
    let sink = Rc::clone(&found);
    let name = *name;
    let mut visit = Visit::rewriting_shared(
        |_, _| None,
        Box::new(move |_, term: &Term| {
            let stated = match &**term {
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
                    Some(instance.universes.clone())
                }
                _ => None,
            };
            if let Some(stated) = stated
                && sink.borrow().is_none()
            {
                *sink.borrow_mut() = Some(stated);
            }
            None
        }),
    );
    let _: Term = value.traverse(&mut visit);
    found.borrow_mut().take()
}

/// The levels of a declaration's result sort that occur nowhere a use site can reach — the levels no occurrence could ever choose.
///
/// `id : (A : Type u) -> A -> A` is genuinely polymorphic: a caller picks `A`, and with it `u`. `Le : Nat -> Nat -> Type u` is not. Nothing at a use site supplies that `u`, so generalizing it mints a parameter every occurrence must instantiate for no benefit — which is how a proof about naturals ends up universe-polymorphic, and how a string literal's per-byte constructor applications each mint fresh levels.
///
/// Minimizing instead is sound for two reasons. Cumulativity already lets a declaration sitting at `Type 0` be used where a higher universe is expected, so the parameter buys no expressiveness. And `minimize` takes least solutions *subject to the recorded constraints*, so a level that genuinely depends on an argument still lands there rather than collapsing to zero.
///
/// A level occurring in both a binder domain and the result sort stays in the interface — the caller chooses it, and the result merely mentions it.
///
/// Only a terminal that *is* a sort states a result sort. A terminal applying a name carries that occurrence's instance, which `UniverseSolver::finalize` settles by the occurrence's bounds; read as a result sort, a witness goal's levels would go to zero whether a caller could choose them or not.
pub fn result_sort_only_metas(context: &Context, type_: &Term) -> BTreeSet<UniverseMetaId> {
    fn peel<'a>(term: &'a Term, domains: &mut Vec<&'a Term>) -> &'a Term {
        match &**term {
            Subterm::FuncType(FuncType { telescope, .. }) => {
                let mut current = telescope;
                loop {
                    match current {
                        Telescope::Cons(_, domain, rest, ..) => {
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
    if !matches!(&**terminal, Subterm::Sort(..)) {
        return BTreeSet::new();
    }
    let mut result = context.universe_metas_in(terminal);
    for domain in domains {
        for meta in context.universe_metas_in(domain) {
            result.remove(&meta);
        }
    }
    result
}

/// The levels a family's parameters are typed at: the outer telescope of its arity, the indices or fields it closes over apart.
fn parameter_metas(arity: &Telescope<Telescope<()>>) -> BTreeSet<UniverseMetaId> {
    let mut metas = BTreeSet::new();
    let mut current = arity;
    while let Telescope::Cons(_, domain, rest, ..) = current {
        metas.extend(domain.universe_metas());
        current = rest.body();
    }

    metas
}

/// Sort the levels of the registry entries `names` declare, the families of one group, into the group's interface and its internal levels, answering the ones an instance holds.
///
/// A family's level is its callers' where a parameter is typed at it, where its sort stands, or where an instance holds it, in an index type, a payload, a field or a target ([`family_levels`]). A level an instance holds is compared wherever two instances meet, so it is answered to be held as a signature's compared level is ([`compared_levels`]), a bound on it being its callers' to meet. Any other level its telescopes mention is one no two instances differ in: it is minimized as a body's is, so no occurrence of the family mints a level for it and no signature naming the family carries one.
pub fn sort_family_levels(
    context: &mut Context,
    names: &[Global],
    interface: &mut BTreeSet<UniverseMetaId>,
    internal: &mut BTreeSet<UniverseMetaId>,
) -> BTreeSet<UniverseMetaId> {
    let mut mentioned = BTreeSet::new();
    let mut telescopes = BTreeSet::new();
    for name in names {
        if let Some(induct_decl) = context.induct_decl(name) {
            mentioned.extend(parameter_metas(&induct_decl.arity));
            mentioned.extend(induct_decl.result_sort.universe_metas());
            telescopes.extend(universe_metas(&induct_decl.arity));
            for constructor in induct_decl.signatures() {
                telescopes.extend(universe_metas(&constructor.telescope));
            }
        }
        if let Some(struct_decl) = context.struct_decl(name) {
            mentioned.extend(parameter_metas(&struct_decl.arity));
            mentioned.extend(struct_decl.result_sort.universe_metas());
            telescopes.extend(universe_metas(&struct_decl.arity));
        }
        if let Some(concept) = context.concept(name) {
            mentioned.extend(universe_metas(&concept.params));
        }
    }

    interface.extend(mentioned.iter().copied());
    if telescopes.is_empty() {
        return BTreeSet::new();
    }
    // Where a telescope may hold any level, every one of its levels stays the group's interface, settled as an occurrence's is.
    let Some(held) = family_levels(context, names) else {
        interface.extend(telescopes);
        return BTreeSet::new();
    };
    for meta in &telescopes {
        match mentioned.contains(meta) || held.contains(meta) {
            true => interface.insert(*meta),
            false => internal.insert(*meta),
        };
    }

    held.intersection(&telescopes).copied().collect()
}

pub fn finalize_definition(
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
        let owner = Global::Authored(*owner);
        let universe_context = context
            .concept(&owner)
            .ok_or_else(|| {
                Error::UniverseInvariant(format!(
                    "{}: method wrapper names an unregistered concept '{}'",
                    name.faithful(),
                    owner.faithful()
                ))
            })?
            .universe_context
            .clone();
        let instance = declaration_instance(&type_, &owner).ok_or_else(|| {
            Error::UniverseInvariant(format!(
                "{}: method wrapper does not apply its concept '{}'",
                name.faithful(),
                owner.faithful()
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
    // A value constructor stands in the same relation to its inductive as a method wrapper does to its concept, and fails the same way. `Result(F, S)` has two universe parameters; `success` mentions only `S` in its payload, so generalizing it alone mints its own `Param(0..1)` in meta-id order and displaces the inductive's — which `generalize` then shifts up past the declared count, leaving `Param(2)`/`Param(3)` with nothing to name them. A constructor is not independently polymorphic: its universes *are* the inductive's, so it inherits that context and binds its instance to those parameters positionally.
    //
    // Like `ConceptMethod`, the owner is a field `into_core` records where the constructor is generated, not something recovered from `name`.
    if let DefinitionKind::InductiveConstructor { owner, .. } = kind {
        let owner = Global::Authored(*owner);
        let universe_context = context
            .induct_decl(&owner)
            .ok_or_else(|| {
                Error::UniverseInvariant(format!(
                    "{}: constructor names an unregistered inductive '{}'",
                    name.faithful(),
                    owner.faithful()
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
                variances: struct_decl.variances,
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
    let mut internal = context.universe_metas_in(&body);
    internal.extend(determined.iter().copied());
    let mut compared = sort_family_levels(context, &[*name], &mut interface, &mut internal);
    compared.extend(compared_levels(context, &type_));
    // What a type-valued definition's body holds is what an occurrence of it unfolds to, so it is held as the signature's is, and a level the body alone mentions joins the interface with it. The result sort's own level stays determined, the body restating it where it says what it answers.
    let unfolded = body_levels(context, &type_, &body, &determined);
    interface.extend(unfolded.iter().copied());
    compared.extend(unfolded);

    let universe_context = context.finalize_universe_metas(interface, internal, compared)?;
    let instance = universe_context.identity_instance();
    let owned = BTreeSet::from([*name]);
    // A non-recursive definition's own name stays free in its signature, body, and registry entry: nothing captures it, so each occurrence carries the instance itself. A definition whose body names *itself* is about to be captured by a single-member group instead, and its occurrences reach that binder — see [`elaborate_module_let`].
    let free = self_reference;
    let type_ = stamp_declaration_instance(
        &context.zonk_universe_levels(&type_)?,
        &owned,
        free,
        &instance,
    );
    let body = stamp_declaration_instance(
        &context.zonk_universe_levels(&body)?,
        &owned,
        free,
        &instance,
    );

    if let Some(struct_decl) = context.struct_decl(name).cloned() {
        context.update_struct(
            name,
            StructDecl {
                universe_context: universe_context.clone(),
                arity: stamp_declaration_instance(
                    &context.zonk_universe_levels(&struct_decl.arity)?,
                    &owned,
                    free,
                    &instance,
                ),
                result_sort: stamp_declaration_instance(
                    &context.zonk_universe_levels(&struct_decl.result_sort)?,
                    &owned,
                    free,
                    &instance,
                ),
                module: struct_decl.module,
                rep_public: struct_decl.rep_public,
                polarities: struct_decl.polarities,
                variances: struct_decl.variances,
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
                    &instance,
                ),
                fields: concept.fields,
                supers: concept.supers,
            },
        );
    }

    Ok((universe_context, type_, body))
}
