//! Synthesizing the type of a neutral spine, and the universe-level identification that decides a problem whose sides differ in nothing else.
//!
//! Both are the *non*-structural half of conversion: [`synth_neutral`] answers what type a head-and-arguments term inhabits, which is what type-directed comparison needs before it can compare anything, and [`identify_universe_levels`] disposes of a problem by committing level equalities rather than by descending. The structural comparison that calls them lives with [`Convert`](super::Convert).

#[cfg(test)]
mod tests;

use {
    crate::{Context, EveryPosition, Structural, reduce, reduce_forced},
    curios_core::{
        Argument, Families, Field, Free, FuncType, InductType, Instance, InstanceHead, Level,
        Match, MatchResult, Proj, Quality, ReduceError, StructType, Subterm, Term, TupleType,
        UniverseConstraintKind, UniverseConstraintOrigin, instantiate_universes_scoped,
    },
    std::{collections::HashMap, rc::Rc},
};

/// Decide a problem whose sides differ only in universe levels by *identifying* the levels: commit every differing pair as the same `Conversion` equality constraint the structural path emits for a type's vectors, and accept — after the commitment the two spellings are one term, which is the license the acceptance stands on. Neither side is reduced, which is the point: two spellings of one computation are identified without running it, so registering the fact does not cost the fact's own subject — a partial definition at two fresh instances, an accumulation under a strict operation.
///
/// Comparing the two sides through `project_erased_universes` and accepting on projection equality would rest on the premise that a universe instance cannot affect computation, and that premise is false (see `documentation/design/soundness/case-equations-and-their-key.md`): `Type u` embeds a level *in a term*, so a definition carrying a level into a constructor payload reduces to genuinely different values at two instances, and the projection would accept such pairs with no residue for the declaration boundary to refuse. Identification leaves the residue.
///
/// What it answers is one of [`Identification`]'s verdicts. Declines — answering anything but `Identified`, with **nothing inserted**, so the structural path below judges the problem instead — on a pair of unequal ground levels, where there is nothing to identify and the problem may still hold by value, on a differing pair under a universe binder, whose bound parameters the ambient solver cannot constrain, and on a pair a question asked with nothing committed withholds ([`Context::withholds`]), which is read by its value as two ground levels are. Every pair is checked before any is committed, because a decline that had already inserted would not be a fall-through.
///
/// A level a value does not hold is no difference to identify: the walk is handed the context's registries, a nominal family's vector and a definition's, and sets apart the pair at a position either calls irrelevant, so two instances apart in it alone are identified with nothing committed. Where the sides are identified, each pair set apart is recorded as a weak equation ([`UniverseSolver::add_weak_eq`](crate::UniverseSolver::add_weak_eq)), which the declaration's closing joins where it can.
///
/// A head short of its full application, apart in such a level, is one term where the pair meets at its function type and two at any other, which `curried` says how to read. [`Curried::Declined`] answers [`Identification::Curried`] with nothing inserted, for a caller that goes on to the pair's type: reduction and eta decide it there, as the kernel decides it, and the verdict hands back the pairs it left, which are a weak equation each once the caller holds the two sides one. [`Curried::Compared`] keeps every level of it, for a caller no type directs.
pub(super) fn identify_universe_levels(
    context: &mut Context,
    this: &Term,
    that: &Term,
    curried: Curried,
) -> Result<Identification, ReduceError> {
    let alignment = match curried {
        Curried::Declined => align_universe_levels(context, this, that, &*context)?,
        Curried::Compared => align_universe_levels(context, this, that, &Structural(context))?,
    };
    let (compared, irrelevant) = match alignment {
        Alignment::Pending {
            compared,
            irrelevant,
            ..
        } => (compared, irrelevant),
        Alignment::Distinct => return Ok(Identification::Distinct),
        Alignment::UnderBinder => return Ok(Identification::UnderBinder),
        Alignment::GroundUnequal => return Ok(Identification::GroundUnequal),
        Alignment::Curried { apart, .. } => {
            curios_profile::sample!("identify::curried", apart.len());

            return Ok(Identification::Curried(apart));
        }
    };

    // Every pair is asked before any is committed, as the walk checks them: a question asked with nothing committed joins none, and the pair is left to its value.
    for (this_level, that_level) in &compared {
        if context.withholds(this_level, that_level)? {
            return Ok(Identification::Withheld);
        }
    }

    // How many pairs an identification equates and how many it records weak, per call that identifies, and above how many it leaves to a curried head's type per call that declines for one: the magnitudes a change to what it commits moves.
    curios_profile::sample!("identify::equated", compared.len());
    curios_profile::sample!("identify::weak", irrelevant.len());

    for (this_level, that_level) in compared {
        context
            .universes_mut()
            .add_eq(
                this_level,
                that_level,
                UniverseConstraintOrigin::new(UniverseConstraintKind::Conversion),
            )
            .map_err(ReduceError::Universe)?;
    }
    for (this_level, that_level) in irrelevant {
        context.universes_mut().add_weak_eq(this_level, that_level);
    }

    Ok(Identification::Identified)
}

/// Hold two spellings of one refinement key to one universe instance, answering whether they can be: every level the two differ in is committed equal, as [`identify_universe_levels`] commits two sides apart in levels alone, and every sort is held one ([`UniverseSolver::unify_qualities`](crate::UniverseSolver::unify_qualities)). Two unequal ground levels, two sorts that each state one and state two, a pair under a universe binder, which the ambient solver has no hold on, and two sides apart in more than their universes decline the key with nothing inserted.
///
/// A key erases its universes, every occurrence of a polymorphic scrutinee minting an instance of its own and the key being compared while they are open (`reduce`'s `shallow_scrutinee`). A hit then rests on the two instances being one: what it hands back is the arm's value for the scrutinee as the arm recorded it, and `Type u` embeds a level in a term, so two instances of one stuck call can be two values. Left unsaid, the two close wherever their own bounds put them, and where that is apart the elaborator has refined a term the kernel's key, which keeps its levels, does not answer. Committed, they close together, or the declaration is refused where its levels close.
///
/// **A sort is part of the instance.** A stuck call reached through a definition that has closed holds every sort its instance states decided, while the key the arm records, written in the declaration still being checked, holds a metavariable for each sort nothing has yet decided: a proof written `Eq/refl()` at both is `Type` in the one and open in the other. Two spellings apart in that alone are one instance once it is solved, as two apart in an open level are, so the walk pairs sorts beside levels and the commitment solves them with the levels.
///
/// It reads no family's variance: the kernel's key is the scrutinee compared with every level, so a key let through on an irrelevant one would answer a term the kernel reduces before it answers. Past a key the guard is [`levels_clash_to_conversion`].
///
/// This is the one write a reduction makes to the solver, so a hit that makes it is noted as a question put to conversion is ([`Context::note_question`]): a scope rolled back past it withdraws the equations, and with them every reduct taken meanwhile. A question asked with nothing committed that meets one answers no, as it does wherever answering yes would commit (`same_uncommitted`).
pub(crate) fn hold_at_one_instance(
    context: &mut Context,
    this: &Term,
    that: &Term,
) -> Result<bool, ReduceError> {
    let Alignment::Pending {
        compared, sorts, ..
    } = align_universe_levels(context, this, that, &EveryPosition)?
    else {
        return Ok(false);
    };
    if compared.is_empty() && sorts.is_empty() {
        return Ok(true);
    }

    // How many pairs a key's hit commits, per hit that commits any.
    curios_profile::sample!("identify::key", compared.len() + sorts.len());
    let mark = context.solution_mark();
    let held = compared.into_iter().all(|(this_level, that_level)| {
        context
            .universes_mut()
            .add_eq(
                this_level,
                that_level,
                UniverseConstraintOrigin::new(UniverseConstraintKind::Conversion),
            )
            .is_ok()
    }) && sorts.into_iter().all(|(this_sort, that_sort)| {
        context
            .universes_mut()
            .unify_qualities(this_sort, that_sort, None)
    });
    if !held {
        context.rollback_solutions(mark);
    }
    context.end_solutions(mark);
    if held {
        context.note_question();
    }

    Ok(held)
}

/// Whether two spellings disagree on a level decided on both sides, as conversion reads the pair: a level a nominal family or a definition applied in full is irrelevant in is no disagreement, two instances apart in it alone being one term. It commits nothing and only ever declines. The guard past a key, where a stuck form is put to an equation's reduced spelling and both checkers answer by what conversion holds one — the kernel by asking its own, so a pair this lets through is one the kernel answers as well. A curried head apart in two decided levels is a clash here, no type being at hand to say otherwise.
pub(crate) fn levels_clash_to_conversion(
    context: &Context,
    this: &Term,
    that: &Term,
) -> Result<bool, ReduceError> {
    Ok(matches!(
        align_universe_levels(context, this, that, context)?,
        Alignment::GroundUnequal | Alignment::Curried { decided: true, .. }
    ))
}

/// Align two spellings' levels positionally and classify the first pair that decides the question, committing nothing.
///
/// One walk over the pair, shared with the kernel: [`Term::level_differences`] answers whether the sides differ in nothing but levels and which level pairs they differ in, a ground `Type 0` counted as a level, and it walks the graph rather than the tree, so identifying two spellings costs what the spellings are and not what they unfold to.
///
/// Every pair is checked before any verdict that would insert, because a decline that had already inserted would not be a fall-through — which is why the commitment lives in [`identify_universe_levels`] and the walk here hands back what it *would* commit.
///
/// `families` is the caller's reading of the vectors: conversion's, under which the pair at an irrelevant position is set apart, the structural arms', under which a curried head keeps every level, or `()`, under which every level is compared. A pair set apart under a universe binder is left out altogether, the ambient solver having no hold on a bound parameter.
///
/// A curried pair that still differs once zonked outranks a pending commitment, never a refusal: nothing can be committed for a pair whose question is still open, while two unequal ground levels at a compared position decide against the fast path whatever a curried head turns out to be.
fn align_universe_levels(
    context: &Context,
    this: &Term,
    that: &Term,
    families: &impl Families,
) -> Result<Alignment, ReduceError> {
    let Some(differences) = this.level_differences(that, |_| false, families) else {
        return Ok(Alignment::Distinct);
    };

    let irrelevant = differences
        .irrelevant
        .into_iter()
        .filter(|(depth, _, _)| *depth == 0)
        .map(|(_, this_level, that_level)| (this_level, that_level))
        .collect();
    let mut compared = Vec::new();
    for (depth, this_level, that_level) in differences.compared {
        if depth > 0 {
            return Ok(Alignment::UnderBinder);
        }

        let this_level = context
            .universes()
            .zonk(&this_level)
            .map_err(ReduceError::Universe)?;
        let that_level = context
            .universes()
            .zonk(&that_level)
            .map_err(ReduceError::Universe)?;
        if this_level == that_level {
            continue;
        }
        if this_level.atoms.is_empty() && that_level.atoms.is_empty() {
            return Ok(Alignment::GroundUnequal);
        }

        compared.push((this_level, that_level));
    }

    let mut apart = Vec::new();
    let mut under_binder = false;
    let mut decided = false;
    for (depth, this_level, that_level) in differences.curried {
        if depth > 0 {
            under_binder = true;
            continue;
        }

        let this_level = context
            .universes()
            .zonk(&this_level)
            .map_err(ReduceError::Universe)?;
        let that_level = context
            .universes()
            .zonk(&that_level)
            .map_err(ReduceError::Universe)?;
        if this_level == that_level {
            continue;
        }
        decided |= this_level.atoms.is_empty() && that_level.atoms.is_empty();
        apart.push((this_level, that_level));
    }
    if under_binder || !apart.is_empty() {
        return Ok(Alignment::Curried { apart, decided });
    }

    Ok(Alignment::Pending {
        compared,
        irrelevant,
        sorts: differences.sorts,
    })
}

/// What [`align_universe_levels`] found, before anybody decides what to do about it.
enum Alignment {
    Distinct,
    UnderBinder,
    GroundUnequal,
    /// A head short of its full application is apart in levels that only its type can call a difference or none: the zonked pairs outside every universe binder, and `decided` where one of them is two unequal ground levels.
    Curried {
        apart: Vec<(Level, Level)>,
        decided: bool,
    },
    /// The sides differ in their universes alone: the zonked pairs a commitment would have to join, the pairs at irrelevant positions, which it may, and the sorts they differ in, where the reading pairs sorts.
    Pending {
        compared: Vec<(Level, Level)>,
        irrelevant: Vec<(Level, Level)>,
        sorts: Vec<(Quality, Quality)>,
    },
}

/// What [`identify_universe_levels`] found: the sides are one term now that their levels are committed equal; they differ in more than levels; a differing pair is two unequal ground levels, which no commitment can join; a differing pair sits under a universe binder, whose bound parameters the ambient solver cannot constrain; a differing pair is withheld, the question in hand committing nothing; or a differing pair sits at a curried head, which the pair's type decides, and these are the pairs left to it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Identification {
    Identified,
    Distinct,
    GroundUnequal,
    UnderBinder,
    Withheld,
    Curried(Vec<(Level, Level)>),
}

/// How [`identify_universe_levels`] reads a head short of its full application that is apart in a level its vector calls irrelevant.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Curried {
    /// Left to the pair's type, nothing committed: the caller goes on to reduce the pair and to apply eta at it.
    Declined,
    /// Compared level by level: no type directs the caller, and two instances reduction leaves standing are one where every level is.
    Compared,
}

/// Binders opened locally by [`Sort::of`](super::Sort::of) while walking a telescope, innermost last, each found by its name.
///
/// These deliberately do *not* go into the [`Context`]. `Sort::of` runs on every conversion problem (through `is_prop`), and `Context::assume` bumps `mutation_stamp`, which is what validates the memoization caches — assuming here would invalidate them continuously and starve a comparison of its budget. Keeping the binders local also keeps `Sort::of` observationally read-only: a label minted here is among no problem's [`Binders`], so it must never reach one. It cannot, because `Sort::of` returns a `Sort`.
///
/// **A binder is found by its name, and a type asked which it names by the names it holds.** A record's fields are each opened beside the ones before it, and each field's type is asked whether it names any of them: read off a list, field by field, that is the square of the fields.
#[derive(Debug, Clone, Default)]
pub(crate) struct Opened {
    /// Each binder at its type, innermost last, beside the position the name stood at before it where it shadows one.
    binders: Vec<(Free, Term, Option<usize>)>,
    positions: HashMap<Free, usize>,
}

impl Opened {
    /// How many binders are open: the mark [`Opened::truncate`] closes back to.
    pub(crate) fn len(&self) -> usize {
        self.binders.len()
    }

    /// Open `binder` at `type_`, innermost.
    pub(crate) fn push(&mut self, binder: Free, type_: Term) {
        let shadowed = self.positions.insert(binder, self.binders.len());
        self.binders.push((binder, type_, shadowed));
    }

    /// Close the innermost binder.
    pub(crate) fn pop(&mut self) {
        if let Some((binder, _, shadowed)) = self.binders.pop() {
            match shadowed {
                Some(position) => self.positions.insert(binder, position),
                None => self.positions.remove(&binder),
            };
        }
    }

    /// Close every binder opened past `mark`.
    pub(crate) fn truncate(&mut self, mark: usize) {
        while self.binders.len() > mark {
            self.pop();
        }
    }

    /// The type `name` was opened at, where it is open.
    pub(crate) fn type_of(&self, name: &Free) -> Option<&Term> {
        let position = *self.positions.get(name)?;

        Some(&self.binders[position].1)
    }

    /// Whether `name` is open.
    pub(crate) fn binds(&self, name: &Free) -> bool {
        self.positions.contains_key(name)
    }

    /// Whether `term` names an open binder, read off the names it holds or the binders open, whichever are fewer.
    pub(crate) fn names_one_in(&self, term: &Term) -> bool {
        if self.binders.is_empty() || !term.has_local_free() {
            return false;
        }
        let frees = term.free_vars_shared();

        match frees.len() < self.binders.len() {
            true => frees.iter().any(|name| self.binds(name)),
            false => self
                .binders
                .iter()
                .any(|(binder, _, _)| frees.contains(binder)),
        }
    }
}

/// The binders a conversion problem is posed under, each at the type its position gave it, innermost first.
///
/// Conversion opens a binder at a fresh label and compares under it, and a lookup that meets the label reads its type here, as the kernel reads a binder it assumed. It is carried with the problem and installed nowhere. The elaborator's conversion is a worklist that parks what it cannot decide, so a problem outlives the step that opened its binders, and one retried later is retried under the binders it was posed under: no verdict follows whether a problem parked. A write to the [`Context`] would also invalidate its caches at every binder, which is why [`Opened`] is threaded too.
///
/// A label is fresh, so no binder here shadows another, or a name the context holds.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub(crate) struct Binders(Option<Rc<Binder>>);

#[derive(Debug, PartialEq, Eq, Hash)]
struct Binder {
    name: Free,
    type_: Term,
    outer: Binders,
}

impl Binders {
    /// These binders with `name` opened innermost at `type_`.
    pub(super) fn opening(&self, name: Free, type_: Term) -> Binders {
        Binders(Some(Rc::new(Binder {
            name,
            type_,
            outer: self.clone(),
        })))
    }

    /// Whether `term` names one of these binders.
    pub(super) fn names_one_in(&self, term: &Term) -> bool {
        let mut binders = self;
        while let Some(binder) = &binders.0 {
            if term.mentions_free(&binder.name) {
                return true;
            }
            binders = &binder.outer;
        }

        false
    }

    /// The type `name` was opened at, where it is one of these.
    fn type_of(&self, name: &Free) -> Option<&Term> {
        let mut binders = self;
        while let Some(binder) = &binders.0 {
            if binder.name == *name {
                return Some(&binder.type_);
            }
            binders = &binder.outer;
        }

        None
    }
}

/// Synthesize the type of a neutral (a `Var`/`Apply`/`Proj` spine over a variable or a metavariable, or a stuck elimination) *without* validating its subterms. Returns `None` when the head is out of scope or the spine is not a typeable neutral — callers fall back conservatively. Built only from the same intrinsics `infer` uses (`Context::assumption`, `reduce`, `Telescope::open`/`nth`), so there is no duplicated typing judgment to drift from `infer`.
///
/// A variable is read off the binders a walk opened (`opened`), then the context, then the binders the problem in hand is posed under (`binders`).
pub(crate) fn synth_neutral(
    context: &mut Context,
    binders: &Binders,
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
            // Locally opened binders shadow the context.
            if let Some(type_) = opened.type_of(name) {
                return Ok(Some(type_.clone()));
            }
            match context
                .instantiate_assumption_universes(name, None)
                .map_err(ReduceError::Universe)?
            {
                Some((type_, _)) => Ok(Some(type_)),
                None => Ok(binders.type_of(name).cloned()),
            }
        }
        // A universe-polymorphic head, at the levels this occurrence chose. The scheme is read *uninstantiated* and substituted at `levels`; going through the `Var` arm below would instead instantiate it at fresh levels and then have nothing left to substitute.
        Subterm::Instance(Instance { head, universes }) => match head {
            InstanceHead::RecProj(group, index) => {
                let group = context.opened_instance(group, universes)?;

                Ok(Some(group.member_type(*index)))
            }
            InstanceHead::Var(var) => {
                let name = var.unwrap();
                if let Some(type_) = opened.type_of(name) {
                    return Ok(Some(type_.clone()));
                }
                let Some(scheme) = context.assumption(name).cloned() else {
                    return Ok(None);
                };
                instantiate_universes_scoped(&scheme, universes)
                    .map(Some)
                    .map_err(ReduceError::Universe)
            }
        },

        Subterm::Apply(apply) => {
            let Some(head_type) = synth_neutral(context, binders, opened, &apply.head)? else {
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
                    let telescope = func_type.telescope.open_params(&params);
                    Ok(Some(Subterm::FuncType(FuncType { telescope }).into()))
                }
                _ => Ok(None),
            }
        }
        // A slot read off what it is a slot of, as the kernel's lookup reads it: a written form and a slot of anything else have no type here.
        Subterm::Proj(Proj { head, field }) => {
            let Some(head_type) = synth_neutral(context, binders, opened, head)? else {
                return Ok(None);
            };

            match (Term::unwrap_or_clone(reduce(context, head_type)?), field) {
                (Subterm::TupleType(TupleType { telescope, .. }), Field::Tuple(_)) => {
                    Ok(telescope.field_type_from(head, field))
                }
                // A nominal structure's field types live on its declaration, instantiated at the head's universes and then at its parameters — the same two steps `elaborate_proj` takes. Concept dispatch is a projection out of a witness dictionary, so this arm is what types a method call.
                (
                    Subterm::StructType(StructType {
                        name,
                        universes,
                        params,
                    }),
                    Field::Struct(structure, _),
                ) if name == *structure => {
                    if context
                        .struct_decl(&name)
                        .is_none_or(|declaration| declaration.param_count() != params.len())
                    {
                        return Ok(None);
                    }
                    context
                        .struct_facet(&name, &universes, |at| at.field_type(&params, head, field))
                        .expect("the declaration was read above")
                        .map_err(ReduceError::Universe)
                }
                _ => Ok(None),
            }
        }
        // A stuck elimination's type is its result at its scrutinee: its ambient goal as written, or its motive opened at the indices the scrutinee's type carries and then at the scrutinee. The elimination was checked against that result where it was typed, so this reads what typing established, as a variable's type is read off its binder; the kernel's lookup reads the same.
        Subterm::Match(Match { head, result, .. }) => {
            let mut indices = Vec::new();
            if let MatchResult::Family(motive) = result {
                if motive.arity() > 1 {
                    let Some(head_type) = synth_neutral(context, binders, opened, head)? else {
                        return Ok(None);
                    };
                    if let Subterm::InductType(InductType {
                        indices: actual, ..
                    }) = &*reduce_forced(context, head_type)?
                    {
                        indices.clone_from(actual);
                    }
                }
                if indices.len() + 1 != motive.arity() {
                    return Ok(None);
                }
            }

            Ok(Some(result.of(head, &indices)))
        }
        // A metavariable's type is the one it was born at, which no solution changes, so a neutral it heads is typed before it is solved: a method read off a witness resolution has not answered yet, whose bound is a proposition by the concept's word whichever witness arrives.
        Subterm::Metavar(metavar) => Ok(context.metavar_type_at(metavar)),
        _ => Ok(None),
    }
}

/// Recover the parameter types of an application from the head's function type, opening each successive entry with the actual arguments (dependency). `None` when the head's type is unavailable or not a `FuncType` of matching arity — callers fall back to comparing arguments at `Term::type_ground()`.
pub(super) fn apply_param_types(
    context: &mut Context,
    binders: &Binders,
    head: &Term,
    arguments: &[Argument],
) -> Result<Option<Vec<Term>>, ReduceError> {
    let Some(head_type) = synth_neutral(context, binders, &Opened::default(), head)? else {
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

/// Report, under `profile`, a site that could not determine a universe level and fell back to `Type 0`. Diagnostic only: it changes nothing, and it is what tells a level these fallbacks chose from one a declaration's generalization did.
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

/// Report, under `profile`, that the reader named `reader` took its answer for a sort its declaration leaves to a caller ([`UniverseSolver::left_to_caller`](crate::UniverseSolver::left_to_caller)). Each reader answers for such a sort in the direction it calls safe, and the notes under `curios_elab::sort` that carry a `reader` are what counts which of them meet one, and how often. Diagnostic only: it changes nothing.
#[cfg(feature = "profile")]
pub(crate) fn probe_open_sort(context: &Context, reader: &'static str, quality: Quality) {
    if context.universes().left_to_caller(quality) {
        curios_profile::note!(
            target: "curios_elab::sort",
            reader,
            "answered for a sort left to a caller",
        );
    }
}

#[cfg(not(feature = "profile"))]
pub(crate) fn probe_open_sort(_context: &Context, _reader: &'static str, _quality: Quality) {}
