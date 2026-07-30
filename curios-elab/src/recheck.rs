//! Running the independent kernel over a module this stage has accepted.
//!
//! This is the seam where the second opinion is actually asked for. Everything upstream — elaboration, unification, zonking, witness resolution — has already decided the module is well-typed; [`recheck_module`] hands the result to `curios-core`'s kernel, which decides again from the terms alone.
//!
//! # Reading a disagreement
//!
//! A refusal here is *not* automatically an elaborator bug, and treating it as one would be the wrong reflex. The kernel is deliberately incomplete in several places — coverage is unverified, free-monoid elimination arms are unchecked, conversion compares some positions syntactically — and each of those refuses valid programs. So a disagreement is a question, and the two answers are "the kernel needs strengthening here" and "the elaborator admitted something it should not have". Both are worth knowing, which is why this runs at all.
//!
//! What a disagreement is *never* is noise to be suppressed. If a rule here has to be weakened to make a real module pass, that weakening is a decision about the trusted base and belongs in `documentation/DESIGN.md`.
//!
//! # Not on the compile path
//!
//! Nothing in the pipeline calls this. The kernel does not yet accept the whole standard library, so wiring it into every build would refuse programs that are fine — and a checker that has to be bypassed is worth nothing. It is an API and a test surface until the gaps named above are closed.

use {
    super::totality::mentioned,
    curios_cert::{
        Kernel, KernelError, check_definition, check_entrypoint, check_induct_decl,
        check_rec_group, check_struct_decl, positivity_vectors,
    },
    curios_core::{Free, Global, Item, Module, Term},
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
                Item::Let(definition) => mentioned(definition),
                Item::Rec(rec) => rec.definitions().iter().flat_map(mentioned).collect(),
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

fn verdicts_from(mut kernel: Kernel, module: &Module, checked_from: usize) -> Vec<Verdict> {
    let mut verdicts = Vec::new();

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
    }

    if let Err(error) = check_entrypoint(&mut kernel, &module.body, module.type_.as_ref()) {
        verdicts.push(Verdict { name: None, error });
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
