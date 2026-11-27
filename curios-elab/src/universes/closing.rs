//! How a declaration's levels close into its scheme, and the levels a family's instances hold sorted from those it leaves to its callers.

use {
    crate::{Context, Error, body_levels, compared_levels, family_levels, zonk_solved_term_metas},
    curios_core::{
        FuncType, Global, Subterm, Telescope, Term, UniverseContext, UniverseMetaId,
        stamp_declaration_instance, universe_metas,
    },
    std::collections::BTreeSet,
};

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
    if !matches!(&**terminal, Subterm::Sort(_)) {
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
            mentioned.extend(parameter_metas(induct_decl.arity()));
            mentioned.extend(induct_decl.result_sort().universe_metas());
            telescopes.extend(universe_metas(induct_decl.arity()));
            for constructor in induct_decl.signatures() {
                telescopes.extend(universe_metas(constructor.payload()));
            }
        }
        if let Some(struct_decl) = context.struct_decl(name) {
            mentioned.extend(parameter_metas(&struct_decl.arity));
            mentioned.extend(struct_decl.result_sort.universe_metas());
            telescopes.extend(universe_metas(&struct_decl.arity));
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
    type_: Term,
    body: Term,
) -> Result<(UniverseContext, Term, Term), Error> {
    curios_tracing::profile!("finalize_definition");
    let type_ = zonk_solved_term_metas(context, &type_);
    let body = zonk_solved_term_metas(context, &body);

    // The signature forms the declaration's interface: a use site instantiates exactly it. Levels reachable only through the body are internal classifiers and are minimized instead, as are the levels of the result sort — see [`result_sort_only_metas`].
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

    let owned = BTreeSet::from([*name]);
    let universe_context =
        context.finalize_universe_metas(&owned, interface, internal, compared)?;
    let instance = universe_context.identity_instance();
    // A definition's own nominal forms, in its signature and body, denote its own instance; a witness whose body names itself names it by the local its group of one closes over — see [`elaborate_module_let`].
    let type_ =
        stamp_declaration_instance(&context.zonk_universe_levels(&type_)?, &owned, &instance);
    let body = stamp_declaration_instance(&context.zonk_universe_levels(&body)?, &owned, &instance);

    Ok((universe_context, type_, body))
}
