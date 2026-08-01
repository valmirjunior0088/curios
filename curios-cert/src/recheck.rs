//! Running the independent kernel over a whole module.
//!
//! This is the seam where the second opinion is actually asked for. Everything upstream — elaboration, unification, zonking, witness resolution — has already decided the module is well-typed; [`recheck_module`] decides again from the terms alone, and it lives in this crate so that deciding is something `cargo tree -p curios-cert` can account for.
//!
//! # Reading a disagreement
//!
//! A refusal here is *not* automatically an elaborator bug, and treating it as one would be the wrong reflex. The kernel is deliberately incomplete in several places — coverage is unverified, free-monoid elimination arms are unchecked, conversion compares some positions syntactically — and each of those refuses valid programs. So a disagreement is a question, and the two answers are "the kernel needs strengthening here" and "the elaborator admitted something it should not have". Both are worth knowing, which is why this runs at all.
//!
//! What a disagreement is *never* is noise to be suppressed. If a rule here has to be weakened to make a real module pass, that weakening is a decision about the trusted base and belongs in `documentation/DESIGN.md`.
//!
//! # On the compile path
//!
//! Every compilation calls this. `compile_entrypoint` runs [`recheck_module_suffix`] and fails the build on a refusal, judging the user's items while the archived prelude prefix is defined on the archive's word — the ground for that word being `curios-prelude`'s build script, which runs the full [`recheck_module_verdicts`] walk when the archive is constructed and fails the build on any refusal. An archive that exists is one whose every item the kernel accepted.

use {
    super::{
        Coverage, Erased, Kernel, KernelError, check_definition, check_entrypoint,
        check_induct_decl, check_positions, check_rec_group, check_struct_decl, closed,
        derived_binder_floor, partial_definitions, positivity_vectors, satisfiable,
    },
    curios_core::{
        Bound, Definition, Free, Global, InductDecl, Item, Level, MetaId, Module, StructDecl, Term,
        rewrite_universe_levels_scoped, universe_metas,
    },
    std::collections::{BTreeSet, HashMap, HashSet},
};

/// `module`'s items in dependency order: every item after the ones it mentions.
///
/// The kernel checks items in sequence, defining each as it goes, so an item that mentions a name defined later is `Unbound`. `Module::items` is *not* in that order. `into_core` does sort topologically, but it sorts the surface program — and a concept-dispatched call names a *method*, not the witness that satisfies it. `/syn/Char/Below` uses `<` at `Nat`, and the edge to the witness carrying that `Cmp` instance is created by witness resolution during elaboration, long after the lowering sort could have seen it. So the sort has to be redone here, over the elaborated module, where the edge exists.
///
/// Deterministic: the lowest-index ready item goes first. On a cycle the lowest remaining item breaks the deadlock, matching `into_core`'s sort — the kernel then refuses it as `Unbound`, which is the correct outcome for a genuinely circular non-recursive item and needs no separate error.
fn dependency_order(module: &Module) -> Vec<usize> {
    let mut owner: HashMap<&Global, usize> = HashMap::new();
    for (index, item) in module.items.iter().enumerate() {
        for name in item.declared_names() {
            owner.insert(name, index);
        }
    }

    // A recursive group mentions its own members; that is what makes it a group, not a dependency on something earlier.
    let dependencies = module
        .items
        .iter()
        .enumerate()
        .map(|(index, item)| {
            let names: BTreeSet<Global> = match item {
                Item::Let(definition) => definition.mentions(),
                Item::Rec(rec) => rec
                    .definitions()
                    .iter()
                    .flat_map(Definition::mentions)
                    .collect(),
            };

            names
                .iter()
                .filter_map(|name| owner.get(name).copied())
                .filter(|&target| target != index)
                .collect::<HashSet<usize>>()
        })
        .collect::<Vec<_>>();

    let mut emitted: HashSet<usize> = HashSet::with_capacity(module.items.len());
    let mut order = Vec::with_capacity(module.items.len());

    while order.len() < module.items.len() {
        let ready = (0..module.items.len())
            .find(|index| {
                !emitted.contains(index)
                    && dependencies[*index]
                        .iter()
                        .all(|target| emitted.contains(target))
            })
            .or_else(|| (0..module.items.len()).find(|index| !emitted.contains(index)))
            .expect("an item remains while the order is incomplete");

        emitted.insert(ready);
        order.push(ready);
    }

    order
}

/// One item the kernel refused.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Verdict {
    /// The item that failed — a recursive group is named by its first member, since a group is checked and refused as a unit. `None` is the entrypoint expression, which has no name to export.
    pub name: Option<Global>,
    pub error: KernelError,
}

/// Re-check `module` with the independent kernel.
///
/// `budget` is the reduction allowance each item gets, the same figure the elaborator's own `Context` is built with.
pub fn recheck_module(module: &Module, budget: u64) -> Result<(), KernelError> {
    match recheck_module_verdicts(module, budget).into_iter().next() {
        Some(verdict) => Err(verdict.error),
        None => Ok(()),
    }
}

/// Every item the kernel refuses, rather than only the first.
///
/// # Why this is the primitive
///
/// The kernel is incomplete in known places, so a walk over a real module stops at the first of them and says nothing about what lies past it. Discovering those one build at a time is how a checker gets patched in the order its gaps happen to be encountered, rather than in the order they matter. This exists so the gaps can be *counted* before any of them is designed for — the same move that settled every earlier question in this effort.
///
/// # Why the verdicts are independent
///
/// [`check_definition`] and [`check_rec_group`] both return before their `Kernel::define` step, so a refused item has defined nothing. Running that same define anyway is what keeps this from degenerating into a cascade: every item enters the environment at its declared type with its real body whether or not it checked, so each later item is judged against exactly what it would have been judged against in a fully passing walk.
///
/// Nothing else survives an item. [`Kernel`] holds no caches, its conversion history is built fresh per comparison, and every binder it opens is retracted on the failing path as well as the succeeding one. So recovery here is exact rather than approximate, and a verdict late in the list is worth as much as the first.
///
/// # What it does not tell you
///
/// The count is per *item*, not per disagreement: an item stops at its own first refusal, so one item with three problems reports one. Good for classifying what is missing, wrong for estimating how much is left.
pub fn recheck_module_verdicts(module: &Module, budget: u64) -> Vec<Verdict> {
    verdicts_from(Kernel::new(budget), module, 0)
}

/// [`recheck_module_verdicts`] judging only the items at index `checked_from` and later, defining every earlier item on the archive's word.
///
/// The prefix is the archive-replayed prelude, and the faith placed in it is in the archive's *construction*, not in any per-compile claim: the prelude build runs the full walk over exactly this prefix and fails the build on any refusal, so an archive that exists is one whose items the kernel accepted. Re-judging them per compile would re-answer a settled question at ~24× the cost of the whole rest of the pipeline.
///
/// Everything module-wide still runs unconditionally — the entrypoint check, strict positivity, and declaration sizing — because the registry is spliced and those passes cost milliseconds; only the per-item typing judgment honors the boundary.
pub fn recheck_module_suffix(module: &Module, budget: u64, checked_from: usize) -> Vec<Verdict> {
    verdicts_from(Kernel::new(budget), module, checked_from)
}

/// [`recheck_module_verdicts`] with the kernel's evaluation memos off.
///
/// Exists for one purpose: asserting that memoization changes no verdict — the property that makes a memo an evaluation strategy rather than a store.
pub fn recheck_module_verdicts_uncached(module: &Module, budget: u64) -> Vec<Verdict> {
    verdicts_from(Kernel::uncached(budget), module, 0)
}

/// One item's erased positions, carried with the name a refusal should be reported against.
type ItemPositions = (Option<Global>, Vec<(Term, Erased)>);

/// The first metavariable an `induct` registry entry carries, in the order the entry stores its parts.
fn induct_metavar(declaration: &InductDecl) -> Option<MetaId> {
    let mut found = None;
    {
        let mut note = |id: MetaId| {
            found = Some(id);
            true
        };

        let _ = declaration.arity.any_metavar(&mut note)
            || declaration.result_sort.any_metavar(&mut note)
            || declaration
                .signatures()
                .any(|constructor| constructor.telescope.any_metavar(&mut note));
    }

    found
}

/// [`induct_metavar`] for a `struct` registry entry.
fn struct_metavar(declaration: &StructDecl) -> Option<MetaId> {
    let mut found = None;
    {
        let mut note = |id: MetaId| {
            found = Some(id);
            true
        };

        let _ = declaration.arity.any_metavar(&mut note)
            || declaration.result_sort.any_metavar(&mut note);
    }

    found
}

/// The first unsolved *universe* metavariable one of `value`'s levels holds.
///
/// A level holding one is elaboration residue exactly as a `Metavar` node is, and no judgment refuses it: `Sort::of` reads `Type(?u)` and answers `Type(?u + 1)` without ever asking whether the level is ground, and [`closed`] inspects a `UniverseContext`'s *constraints* — the only place this crate looked for a meta level — never a level sitting inside a term. So this is not a question of which terms the walk reaches; it is the level algebra having no opinion about an unsolved level, which is why the refusal belongs at the boundary rather than inside a judgment.
fn universe_residue<B: Bound>(value: &B) -> Option<KernelError> {
    universe_metas(value)
        .into_iter()
        .next()
        .map(|meta| KernelError::NotCore(Term::type_at(Level::meta(meta))))
}

/// The first universe parameter one of `value`'s levels names that a scheme of `parameter_count` parameters does not have.
///
/// A declaration's universe scheme promises that every level it mentions is ground or one of the parameters it declares, so a use site fully determines it. [`universe_residue`] above checks half of that promise — no unsolved metavariable — and this checks the other half, which nothing here checked: no parameter index past the declaration's own count. `closed` sees a `UniverseContext`'s constraints and not a level sitting in a term, so the two are the same omission at different depths.
///
/// It is a soundness rule rather than a tidiness one because of what instantiation does with an out-of-range index: `instantiate_universe_levels_scoped` substitutes what the instance supplies and *renumbers* the rest down by the instance's width. For a well-scoped term that is the correct de Bruijn shift, since an index at or above the width names an enclosing binder. For an ill-scoped one it is a capture — `Type.{param 1}` and `Type.{param 0}` both instantiate at `[param 0]` to the same level — so two distinct levels become one and every cumulativity question after that is answered about the wrong one.
///
/// Scoped rather than flat: a nested scheme binds its own parameters innermost, so the bound at any point is the enclosing binder depth plus the declaration's count, exactly as `curios-elab`'s `validate_bound_universes` computes it. Metavariables are left to [`universe_residue`] so the two diagnostics stay distinct.
fn universe_escape<B: Bound>(value: &B, parameter_count: usize) -> Option<KernelError> {
    rewrite_universe_levels_scoped(value, move |depth, level| {
        let visible = depth.checked_add(parameter_count).ok_or(())?;
        match level.params().any(|param| param.0 >= visible) {
            true => Err(()),
            false => Ok(level.clone()),
        }
    })
    .err()
    .map(|()| KernelError::UnclosedUniverses)
}

/// Every kind of elaboration residue an `induct` registry entry can carry.
fn induct_residue(declaration: &InductDecl) -> Option<KernelError> {
    induct_metavar(declaration)
        .map(|id| KernelError::NotCore(Term::metavar(id)))
        .or_else(|| universe_residue(&declaration.arity))
        .or_else(|| universe_residue(&declaration.result_sort))
        .or_else(|| {
            declaration
                .signatures()
                .find_map(|constructor| universe_residue(&constructor.telescope))
        })
        .or_else(|| {
            let count = declaration.universe_context.parameter_count;
            universe_escape(&declaration.arity, count)
                .or_else(|| universe_escape(&declaration.result_sort, count))
                .or_else(|| {
                    declaration
                        .signatures()
                        .find_map(|constructor| universe_escape(&constructor.telescope, count))
                })
        })
}

/// [`induct_residue`] for a `struct` registry entry.
fn struct_residue(declaration: &StructDecl) -> Option<KernelError> {
    struct_metavar(declaration)
        .map(|id| KernelError::NotCore(Term::metavar(id)))
        .or_else(|| universe_residue(&declaration.arity))
        .or_else(|| universe_residue(&declaration.result_sort))
        .or_else(|| {
            let count = declaration.universe_context.parameter_count;
            universe_escape(&declaration.arity, count)
                .or_else(|| universe_escape(&declaration.result_sort, count))
        })
}

// Safety: the memos below are keyed on `Term`, whose `OnceCell` scalar caches trip Clippy's interior-mutability warning. The logical value is immutable, and hashing and equality stay stable across those caches filling.
#[allow(clippy::mutable_key_type)]
fn verdicts_from(mut kernel: Kernel, module: &Module, checked_from: usize) -> Vec<Verdict> {
    let mut verdicts = Vec::new();
    // What each item's check recorded, kept per item so a refusal names the item it came from. Classified as it drains rather than retained whole: only the positions the obligations are about survive, and sort-hood is asked once per distinct type.
    let mut positions: Vec<ItemPositions> = Vec::new();

    // Binder identities are one space shared across the lowerer, the elaborator, and the archived prelude. Seeding above the module's high-water mark is what keeps a binder the kernel mints — while comparing under a telescope, or eta-contracting — from aliasing one already in a term, which would be a capture.
    //
    // The mark is derived here rather than taken from `Module::binder_floor`, which nothing checks. The two are combined by maximum because a floor is a bound and not a verdict: widening can never refuse a module that was fine, so a position this walk fails to reach degrades to the carried value instead of to a capture.
    kernel.set_local_floor(module.binder_floor.max(derived_binder_floor(module)));

    // Every universe context this walk will assume, decided before any of it is assumed. An unsatisfiable set makes `entails` answer anything, so a later refusal would be reported against whichever item happened to ask a level question first rather than against the declaration that carries the contradiction.
    let contexts = module
        .induct_decls
        .iter()
        .map(|(name, declaration)| (name.clone(), &declaration.universe_context))
        .chain(
            module
                .struct_decls
                .iter()
                .map(|(name, declaration)| (name.clone(), &declaration.universe_context)),
        )
        .collect::<Vec<_>>();
    for (name, context) in contexts {
        if let Some(error) = universe_verdict(context) {
            verdicts.push(Verdict {
                name: Some(name),
                error,
            });
        }
    }
    for item in &module.items {
        for definition in match item {
            Item::Let(definition) => vec![definition.clone()],
            Item::Rec(rec) => rec.definitions(),
        } {
            if let Some(error) = universe_verdict(&definition.universe_context) {
                verdicts.push(Verdict {
                    name: Some(definition.name.clone()),
                    error,
                });
            }
        }
    }

    // A registry entry is data that no judgment in this walk types. `check_sizing` walks a constructor telescope's *domains* and stops at the terminal, so the index targets a constructor states reach index inversion and the arm rule without ever having been checked, and `check_induct_decl` leaves them to the `rec` group a declaration lowers to — a lowering nothing here confirms exists. `infer` and `convert` refuse an elaboration-only node wherever a judgment meets one; this is the boundary pass that decides the same thing for the positions no judgment visits, so that "no unsolved metavariable survives" is this walk's own verdict rather than the elaborator's word.
    for (name, declaration) in &module.induct_decls {
        if let Some(error) = induct_residue(declaration) {
            verdicts.push(Verdict {
                name: Some(name.clone()),
                error,
            });
        }
    }
    for (name, declaration) in &module.struct_decls {
        if let Some(error) = struct_residue(declaration) {
            verdicts.push(Verdict {
                name: Some(name.clone()),
                error,
            });
        }
    }
    // An item's own terms are walked, so a `Metavar` node in one is refused where a judgment meets it. A level is not: the walk types `Type(?u)` without objecting, so the same boundary decides it here.
    for item in &module.items {
        for definition in match item {
            Item::Let(definition) => vec![definition.clone()],
            Item::Rec(rec) => rec.definitions(),
        } {
            let count = definition.universe_context.parameter_count;
            if let Some(error) = universe_residue(&definition.type_)
                .or_else(|| universe_residue(&definition.body))
                .or_else(|| universe_escape(&definition.type_, count))
                .or_else(|| universe_escape(&definition.body, count))
            {
                verdicts.push(Verdict {
                    name: Some(definition.name.clone()),
                    error,
                });
            }
        }
    }
    // The module's own type and body stand under no scheme, so every parameter index in them escapes.
    if let Some(error) = module
        .type_
        .as_ref()
        .and_then(universe_residue)
        .or_else(|| universe_residue(&module.body))
        .or_else(|| {
            module
                .type_
                .as_ref()
                .and_then(|type_| universe_escape(type_, 0))
        })
        .or_else(|| universe_escape(&module.body, 0))
    {
        verdicts.push(Verdict { name: None, error });
    }

    // The nominal registry first: a definition's type may name any declaration in the module, including one whose own definitions come later.
    for (name, declaration) in &module.induct_decls {
        kernel.declare_induct(name, declaration);
    }
    for (name, declaration) in &module.struct_decls {
        kernel.declare_struct(name, declaration);
    }

    for index in dependency_order(module) {
        let item = &module.items[index];

        // A prefix item enters the environment exactly as a refused item would — defined at its declared type with its real body — so every judged item downstream sees what a fully judged walk would have shown it.
        if index < checked_from {
            match item {
                Item::Let(definition) => {
                    kernel.define(
                        &Free::from(&definition.name),
                        &definition.type_,
                        &definition.body,
                        &definition.universe_context,
                    );
                }
                Item::Rec(rec) => {
                    let universes = rec.group.universe_context().clone();
                    for (member, name) in item.declared_names().into_iter().enumerate() {
                        kernel.define(
                            &Free::from(name),
                            &rec.group.member_type(member),
                            &Term::rec_proj(rec.group.clone(), member),
                            &universes,
                        );
                    }
                }
            }
            continue;
        }

        let item_name = item.declared_names().first().map(|&name| name.clone());

        match item {
            Item::Let(definition) => {
                let outcome = check_definition(
                    &mut kernel,
                    &Free::from(&definition.name),
                    &definition.type_,
                    &definition.body,
                    &definition.universe_context,
                );

                if let Err(error) = outcome {
                    verdicts.push(Verdict {
                        name: Some(definition.name.clone()),
                        error,
                    });
                    kernel.define(
                        &Free::from(&definition.name),
                        &definition.type_,
                        &definition.body,
                        &definition.universe_context,
                    );
                }
            }
            Item::Rec(rec) => {
                let names = item
                    .declared_names()
                    .into_iter()
                    .map(Free::from)
                    .collect::<Vec<_>>();
                let universes = rec.group.universe_context().clone();

                let outcome = check_rec_group(&mut kernel, &names, &rec.group, &universes);

                if let Err(error) = outcome {
                    verdicts.push(Verdict {
                        name: item.declared_names().first().map(|&name| name.clone()),
                        error,
                    });
                    // The define `check_rec_group` performs on success: each export is the folded selection of the member it names.
                    for (member, name) in names.iter().enumerate() {
                        kernel.define(
                            name,
                            &rec.group.member_type(member),
                            &Term::rec_proj(rec.group.clone(), member),
                            &universes,
                        );
                    }
                }
            }
        }

        let (drained, failure) = kernel.take_checked();
        positions.push((item_name.clone(), drained));
        if let Some(error) = failure {
            verdicts.push(Verdict {
                name: item_name,
                error,
            });
        }
    }

    if let Err(error) = check_entrypoint(&mut kernel, &module.body, module.type_.as_ref()) {
        verdicts.push(Verdict { name: None, error });
    }

    let (drained, failure) = kernel.take_checked();
    positions.push((None, drained));
    if let Some(error) = failure {
        verdicts.push(Verdict { name: None, error });
    }

    // Obligations (T) and (V), after the item walk for the same reason declaration acceptance runs there: the classification closes over what every definition mentions, and the environment is only complete once every item has been defined.
    let (partial, disagreements) = partial_definitions(&mut kernel, module, checked_from);
    for (name, error) in disagreements {
        verdicts.push(Verdict {
            name: Some(name),
            error,
        });
    }
    let mut local_memo = HashMap::new();
    for (name, item_positions) in &positions {
        if let Err(error) = check_positions(&mut kernel, item_positions, &partial, &mut local_memo)
        {
            verdicts.push(Verdict {
                name: name.clone(),
                error,
            });
        }
    }

    // Declaration acceptance, after the item walk rather than before it: a registry telescope may mention any top-level definition — a type alias, a type constructor's own `rec` group — and those names are only defined as the walk proceeds. Every item defines whether or not it checked, so by this point the environment is complete. Strict positivity runs over the *full* declaration set — the whole spliced program — so the analysis recomputes every vector rather than reading any from the archive; then the size condition, the clause the item walk cannot supply, because it computes each signature's sort and compares it to nothing.
    if let Err(refusal) = positivity_vectors(
        &mut kernel,
        &module.induct_decls,
        &module.struct_decls,
        Coverage::Complete,
    ) {
        verdicts.push(Verdict {
            name: Some(refusal.name.clone()),
            error: KernelError::NotPositive {
                name: refusal.name,
                part: refusal.part,
                polarity: refusal.polarity,
            },
        });
    }
    for (name, declaration) in &module.induct_decls {
        if let Err(error) = check_induct_decl(&mut kernel, declaration) {
            verdicts.push(Verdict {
                name: Some(name.clone()),
                error,
            });
        }
    }
    for (name, declaration) in &module.struct_decls {
        if let Err(error) = check_struct_decl(&mut kernel, declaration) {
            verdicts.push(Verdict {
                name: Some(name.clone()),
                error,
            });
        }
    }

    verdicts
}

#[cfg(test)]
mod tests;

/// What is wrong with a universe context the walk is about to assume, if anything.
///
/// Closure first: a context naming what it does not declare cannot be instantiated, so asking whether it has a solution would be asking about nothing.
fn universe_verdict(context: &curios_core::UniverseContext) -> Option<KernelError> {
    if !closed(context) {
        return Some(KernelError::UnclosedUniverses);
    }
    if !satisfiable(&context.constraints) {
        return Some(KernelError::UnsatisfiableUniverses);
    }

    None
}
