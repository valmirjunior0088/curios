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
        Erased, Kernel, KernelError, Sort, check_definition, check_entrypoint, check_induct_decl,
        check_positions, check_rec_group, check_struct_decl, partial_definitions,
        positivity_vectors,
    },
    curios_core::{Definition, Free, Global, Item, Module, Reducer as _, Subterm, Term},
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

// Safety: the memos below are keyed on `Term`, whose `OnceCell` scalar caches trip Clippy's interior-mutability warning. The logical value is immutable, and hashing and equality stay stable across those caches filling.
#[allow(clippy::mutable_key_type)]
fn verdicts_from(mut kernel: Kernel, module: &Module, checked_from: usize) -> Vec<Verdict> {
    let mut verdicts = Vec::new();
    // What each item's check recorded, kept per item so a refusal names the item it came from. Classified as it drains rather than retained whole: only the positions the obligations are about survive, and sort-hood is asked once per distinct type.
    let mut positions: Vec<ItemPositions> = Vec::new();
    let mut erasure_memo: HashMap<Term, Option<Erased>> = HashMap::new();

    // Binder identities are one space shared across the lowerer, the elaborator, and the archived prelude. Seeding above the module's high-water mark is what keeps a binder the kernel mints — while comparing under a telescope, or eta-contracting — from aliasing one already in a term, which would be a capture.
    kernel.set_local_floor(module.binder_floor);

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
                            &Term::rec_member(rec.group.clone(), member),
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
                            &Term::rec_member(rec.group.clone(), member),
                            &universes,
                        );
                    }
                }
            }
        }

        let drained = kernel.take_checked();
        positions.push((
            item_name,
            classify_positions(&mut kernel, drained, &mut erasure_memo),
        ));
    }

    if let Err(error) = check_entrypoint(&mut kernel, &module.body, module.type_.as_ref()) {
        verdicts.push(Verdict { name: None, error });
    }

    let drained = kernel.take_checked();
    positions.push((
        None,
        classify_positions(&mut kernel, drained, &mut erasure_memo),
    ));

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
    if let Err(refusal) =
        positivity_vectors(&mut kernel, &module.induct_decls, &module.struct_decls)
    {
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

/// Which erased half each recorded position belongs to, or `None` for a term the obligations do not reach.
///
/// A term checked against a *sort* is itself a type; a term checked against a `Prop`-sorted type is a proof. Memoized by type, because a module checks vastly more terms than it has distinct types and hash-consing keeps that set small.
#[allow(clippy::mutable_key_type)]
fn classify_positions(
    kernel: &mut Kernel,
    checked: Vec<(Term, Term)>,
    memo: &mut HashMap<Term, Option<Erased>>,
) -> Vec<(Term, Erased)> {
    let mut positions = Vec::new();

    for (term, type_) in checked {
        let erased = match memo.get(&type_) {
            Some(erased) => *erased,
            None => {
                let erased = erased_half(kernel, &type_);
                memo.insert(type_.clone(), erased);
                erased
            }
        };
        if let Some(erased) = erased {
            positions.push((term, erased));
        }
    }

    positions
}

/// The erased half a term checked against `type_` belongs to. A reduction or sorting failure yields `None`: the item walk reports the real error, and a position this cannot classify is one the obligations decline to constrain.
fn erased_half(kernel: &mut Kernel, type_: &Term) -> Option<Erased> {
    match kernel.reduce_forced(type_.clone()) {
        Ok(reduced) if matches!(&*reduced, Subterm::Type(_) | Subterm::Prop) => Some(Erased::Type),
        Ok(_) => Sort::of(kernel, type_)
            .ok()
            .and_then(|sort| sort.is_prop().then_some(Erased::Proof)),
        Err(_) => None,
    }
}
