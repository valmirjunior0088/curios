//! Synthesizing the type of a neutral spine, and the universe-level identification that decides a problem whose sides differ in nothing else.
//!
//! Both are the *non*-structural half of conversion: [`synth_neutral`] answers what type a head-and-arguments term inhabits, which is what type-directed comparison needs before it can compare anything, and [`identify_universe_levels`] disposes of a problem by committing level equalities rather than by descending. The structural comparison that calls them lives with [`Convert`].

use {
    super::*,
    curios_core::{
        Argument, Free, FuncType, Instance, InstanceHead, Proj, StructType, Subterm, Term,
        UniverseConstraintKind, UniverseConstraintOrigin, strip_universe_levels,
    },
};

/// Decide a problem whose sides differ only in universe levels by *identifying* the levels: commit every differing pair as the same `Conversion` equality constraint the structural path emits for a type's vectors, and accept — after the commitment the two spellings are one term, which is the license the acceptance stands on. Neither side is reduced, which is the point: two spellings of one computation are identified without running it, so registering the fact does not cost the fact's own subject — a partial definition at two fresh instances, an accumulation under a strict operation.
///
/// This replaces a rule that compared the two sides through `project_erased_universes` and accepted on projection equality, on the premise that a universe instance cannot affect computation. That premise is false (see `documentation/soundness/what-the-kernel-consults/the-refinement-key.md`): `Type u` embeds a level *in a term*, so a definition carrying a level into a constructor payload reduces to genuinely different values at two instances — and the projection accepted such pairs with no residue for the declaration boundary to refuse. Identification leaves the residue.
///
/// What it answers is one of [`Identification`]'s verdicts. Declines — answering anything but `Identified`, with **nothing inserted**, so the structural path below judges the problem instead — on a pair of unequal ground levels, where there is nothing to identify and the problem may still hold by value, and on a differing pair under a universe binder, whose bound parameters the ambient solver cannot constrain. Every pair is checked before any is committed, because a decline that had already inserted would not be a fall-through.
pub(super) fn identify_universe_levels(
    context: &mut Context,
    this: &Term,
    that: &Term,
) -> Result<Identification, ReduceError> {
    let pending = match align_universe_levels(context, this, that)? {
        Alignment::Pending(pending) => pending,
        Alignment::Distinct => return Ok(Identification::Distinct),
        Alignment::UnderBinder => return Ok(Identification::UnderBinder),
        Alignment::GroundUnequal => return Ok(Identification::GroundUnequal),
    };

    for (this_level, that_level) in pending {
        context
            .universes_mut()
            .add_eq(
                this_level,
                that_level,
                UniverseConstraintOrigin::new(UniverseConstraintKind::Conversion),
            )
            .map_err(ReduceError::Universe)?;
    }

    Ok(Identification::Identified)
}

/// Whether two spellings of one term disagree on a level that is already *decided* on both sides.
///
/// The refusing half of [`align_universe_levels`], and the one a *store* can use. Conversion answers its question by committing the differing pairs equal, which is licensed because a goal asked it; a refinement probe is a search over candidate keys, and committing a universe equality on the strength of a speculative match would constrain the declaration from a lookup. This commits nothing and only ever declines, so it is safe to ask wherever a key has already matched.
///
/// `false` for everything that is not a decided disagreement — sides differing in more than levels, a pair under a universe binder, a pair either of whose sides is still undecided. That is deliberate and is what keeps the answer the *refusing* direction only: an undecided level may yet be solved either way, and collapsing those is exactly what the refinement key is for.
pub(crate) fn levels_clash_on_a_decided_instance(
    context: &Context,
    this: &Term,
    that: &Term,
) -> Result<bool, ReduceError> {
    Ok(matches!(
        align_universe_levels(context, this, that)?,
        Alignment::GroundUnequal
    ))
}

/// Align two spellings' levels positionally and classify the first pair that decides the question, committing nothing.
///
/// One traversal per side, shared with the kernel: every level collected with its universe-binder depth, a ground `Type 0` included, and replaced by a sentinel — the skeletons then compare equal exactly when the sides differ in nothing but levels, and that equality is what aligns the two collections positionally. A universes-only walk skipped the `Type 0`, which once aligned `(Type 0, Type u)` with `(Type u, Type 0)` on one level paired with itself.
///
/// Every pair is checked before any verdict that would insert, because a decline that had already inserted would not be a fall-through — which is why the commitment lives in [`identify_universe_levels`] and the walk here hands back what it *would* commit.
fn align_universe_levels(
    context: &Context,
    this: &Term,
    that: &Term,
) -> Result<Alignment, ReduceError> {
    let (this_stripped, this_levels) = strip_universe_levels(this);
    let (that_stripped, that_levels) = strip_universe_levels(that);

    if this_stripped != that_stripped || this_levels.len() != that_levels.len() {
        return Ok(Alignment::Distinct);
    }

    let mut pending = Vec::new();
    for ((this_depth, this_level), (that_depth, that_level)) in this_levels.iter().zip(&that_levels)
    {
        if this_level == that_level {
            continue;
        }
        if *this_depth > 0 || *that_depth > 0 {
            return Ok(Alignment::UnderBinder);
        }

        let this_level = context
            .universes()
            .zonk(this_level)
            .map_err(ReduceError::Universe)?;
        let that_level = context
            .universes()
            .zonk(that_level)
            .map_err(ReduceError::Universe)?;
        if this_level == that_level {
            continue;
        }
        if this_level.atoms.is_empty() && that_level.atoms.is_empty() {
            return Ok(Alignment::GroundUnequal);
        }

        pending.push((this_level, that_level));
    }

    Ok(Alignment::Pending(pending))
}

/// What [`align_universe_levels`] found, before anybody decides what to do about it.
enum Alignment {
    Distinct,
    UnderBinder,
    GroundUnequal,
    /// The sides differ in levels alone, and these are the zonked pairs a commitment would have to join.
    Pending(Vec<(Level, Level)>),
}

/// What [`identify_universe_levels`] found: the sides are one term now that their levels are committed equal; they differ in more than levels; a differing pair is two unequal ground levels, which no commitment can join; or a differing pair sits under a universe binder, whose bound parameters the ambient solver cannot constrain.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Identification {
    Identified,
    Distinct,
    GroundUnequal,
    UnderBinder,
}

/// Binders opened locally by [`Sort::of`] while walking a telescope, innermost last.
///
/// These deliberately do *not* go into the [`Context`]. `Sort::of` runs on every conversion problem (through `is_prop`), and `Context::assume` bumps `mutation_stamp`, which is what validates the memoization caches — assuming here would invalidate them continuously and starve a coinductive comparison of its budget. Keeping the binders local also keeps `Sort::of` observationally read-only, which the conversion history relies on: labels minted here are never recorded in `Convert::minted`, so they must never reach a problem. They cannot, because `Sort::of` returns a `Sort`.
pub(crate) type Opened = [(Free, Term)];

/// Synthesize the type of a neutral (a `Var`/`Apply`/`Proj` spine) *without* validating its subterms. Returns `None` when the head is out of scope or the spine is not a typeable neutral — callers fall back conservatively. Built only from the same intrinsics `infer` uses (`Context::assumption`, `reduce`, `Telescope::open`/`nth`), so there is no duplicated typing judgment to drift from `infer`.
pub(crate) fn synth_neutral(
    context: &mut Context,
    opened: &Opened,
    term: &Term,
) -> Result<Option<Term>, ReduceError> {
    // A projection's type is carried by its own group — no lookup, and no unfolding, so this cannot re-enter the group it names.
    if let Some((group, index)) = term.as_rec_proj() {
        return Ok(Some(group.member_type(index)));
    }

    match &**term {
        Subterm::Var(var) => {
            let name = var.unwrap();
            // Locally opened binders shadow the context, innermost first.
            if let Some((_, type_)) = opened.iter().rev().find(|(bound, _)| bound == name) {
                return Ok(Some(type_.clone()));
            }
            context
                .instantiate_assumption_universes(name)
                .map(|instance| instance.map(|(type_, _)| type_))
                .map_err(ReduceError::Universe)
        }
        // A universe-polymorphic head, at the levels this occurrence chose. The scheme is read *uninstantiated* and substituted at `levels`; going through the `Var` arm below would instead instantiate it at fresh levels and then have nothing left to substitute.
        Subterm::Instance(Instance { head, levels }) => match head {
            InstanceHead::RecProj(group, index) => {
                let group = group
                    .instantiate_universes(levels)
                    .map_err(ReduceError::Universe)?;

                Ok(Some(group.member_type(*index)))
            }
            InstanceHead::Var(var) => {
                let name = var.unwrap();
                if let Some((_, type_)) = opened.iter().rev().find(|(bound, _)| bound == name) {
                    return Ok(Some(type_.clone()));
                }
                let Some(scheme) = context.assumption(name).cloned() else {
                    return Ok(None);
                };
                instantiate_universe_levels_scoped(&scheme, levels)
                    .map(Some)
                    .map_err(ReduceError::Universe)
            }
        },

        Subterm::Apply(apply) => {
            let Some(head_type) = synth_neutral(context, opened, &apply.head)? else {
                return Ok(None);
            };
            let params = apply.params().cloned().collect::<Vec<_>>();

            match Term::unwrap_or_clone(reduce(context, head_type)?) {
                Subterm::FuncType(FuncType { telescope, .. })
                    if telescope.len() == params.len() =>
                {
                    let refs = params.iter().collect::<Vec<_>>();
                    Ok(Some(telescope.open(&refs)))
                }
                // A partially applied spine still has a type: the residual function type, with the supplied arguments substituted into the entries that remain.
                Subterm::FuncType(func_type) if func_type.telescope.len() > params.len() => {
                    let residual_plicities = func_type.plicities()[params.len()..].to_vec();
                    let residual = func_type.telescope.open_params(&params);
                    Ok(Some(
                        Subterm::FuncType(FuncType::new(residual, residual_plicities)).into(),
                    ))
                }
                _ => Ok(None),
            }
        }
        Subterm::Proj(Proj {
            head,
            field: Field::Index(index),
        }) => {
            let Some(head_type) = synth_neutral(context, opened, head)? else {
                return Ok(None);
            };

            match Term::unwrap_or_clone(reduce(context, head_type)?) {
                Subterm::TupleType(TupleType { telescope, .. }) => {
                    Ok(telescope.field_type_from(head, *index))
                }
                // A nominal structure's field types live on its declaration, instantiated at the head's universes and then at its parameters — the same two steps `elaborate_proj` takes. Concept dispatch is a projection out of a witness dictionary, so this arm is what types a method call.
                Subterm::StructType(StructType {
                    name,
                    universes,
                    params,
                }) => {
                    let Some(declaration) = context.struct_decl(&name).cloned() else {
                        return Ok(None);
                    };
                    if declaration.param_count() != params.len() {
                        return Ok(None);
                    }
                    let arity = instantiate_bound_at(
                        context,
                        &declaration.universe_context,
                        &declaration.arity,
                        &universes,
                    )?;
                    Ok(arity
                        .open(&params.iter().collect::<Vec<_>>())
                        .field_type_from(head, *index))
                }
                _ => Ok(None),
            }
        }
        _ => Ok(None),
    }
}

/// Recover the parameter types of an application from the head's function type, opening each successive entry with the actual arguments (dependency). `None` when the head's type is unavailable or not a `FuncType` of matching arity — callers fall back to comparing arguments at `Term::type_ground()`.
pub(super) fn apply_param_types(
    context: &mut Context,
    head: &Term,
    arguments: &[Argument],
) -> Result<Option<Vec<Term>>, ReduceError> {
    let Some(head_type) = synth_neutral(context, &[], head)? else {
        return Ok(None);
    };

    let telescope = match Term::unwrap_or_clone(reduce(context, head_type)?) {
        Subterm::FuncType(FuncType { telescope, .. }) if telescope.len() == arguments.len() => {
            telescope
        }
        _ => return Ok(None),
    };

    let params = arguments
        .iter()
        .map(|argument| argument.term.clone())
        .collect::<Vec<_>>();
    let mut types = Vec::with_capacity(params.len());
    telescope.walk(&params, |_, _, ty| {
        types.push(ty.clone());
        Ok(())
    })?;

    Ok(Some(types))
}

/// Report a site that could not determine a universe level and fell back to `Type 0`. Diagnostic only: it changes nothing, and exists to answer whether the concept-wrapper universe failures originate in these fallbacks rather than in how a wrapper is generalized.
#[cfg(feature = "profile")]
pub(super) fn probe_level_fallback(site: &'static str, type_: &Term) {
    curios_profile::note!(
        target: "curios_elab::sort",
        site,
        type_ = %type_,
        "level defaulted to 0",
    );
}

#[cfg(not(feature = "profile"))]
pub(super) fn probe_level_fallback(_site: &'static str, _type_: &Term) {}
