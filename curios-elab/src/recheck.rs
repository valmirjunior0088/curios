//! Running the independent kernel over a module this stage has accepted.
//!
//! This is the seam where the second opinion is actually asked for. Everything
//! upstream — elaboration, unification, zonking, witness resolution — has
//! already decided the module is well-typed; [`recheck_module`] hands the
//! result to `curios-core`'s kernel, which decides again from the terms alone.
//!
//! # Reading a disagreement
//!
//! A refusal here is *not* automatically an elaborator bug, and treating it as
//! one would be the wrong reflex. The kernel is deliberately incomplete in
//! several places — coverage is unverified, free-monoid elimination arms are
//! unchecked, conversion compares some positions syntactically — and each of
//! those refuses valid programs. So a disagreement is a question, and the two
//! answers are "the kernel needs strengthening here" and "the elaborator
//! admitted something it should not have". Both are worth knowing, which is why
//! this runs at all.
//!
//! What a disagreement is *never* is noise to be suppressed. If a rule here has
//! to be weakened to make a real module pass, that weakening is a decision
//! about the trusted base and belongs in `documentation/DESIGN.md`.
//!
//! # Not on the compile path
//!
//! Nothing in the pipeline calls this. The kernel does not yet accept the whole
//! standard library, so wiring it into every build would refuse programs that
//! are fine — and a checker that has to be bypassed is worth nothing. It is an
//! API and a test surface until the gaps named above are closed.

use {
    super::{Item, Module, totality::mentioned},
    curios_core::{
        Free, Global, Kernel, KernelError,
        kernel::{check_definition, check_entrypoint, check_rec_group},
    },
    std::collections::{BTreeSet, HashMap, HashSet},
};

/// `module`'s items in dependency order: every item after the ones it mentions.
///
/// The kernel checks items in sequence, defining each as it goes, so an item
/// that mentions a name defined later is `Unbound`. `Module::items` is *not* in
/// that order. `into_core` does sort topologically, but it sorts the surface
/// program — and a concept-dispatched call names a *method*, not the witness
/// that satisfies it. `/syn/Char/Below` uses `<` at `Nat`, and the edge to the
/// witness carrying that `Cmp` instance is created by witness resolution during
/// elaboration, long after the lowering sort could have seen it. So the sort has
/// to be redone here, over the elaborated module, where the edge exists.
///
/// Deterministic: the lowest-index ready item goes first. On a cycle the lowest
/// remaining item breaks the deadlock, matching `into_core`'s sort — the kernel
/// then refuses it as `Unbound`, which is the correct outcome for a genuinely
/// circular non-recursive item and needs no separate error.
fn dependency_order(module: &Module) -> Vec<usize> {
    let mut owner: HashMap<&Global, usize> = HashMap::new();
    for (index, item) in module.items.iter().enumerate() {
        for name in item.declared_names() {
            owner.insert(name, index);
        }
    }

    // A recursive group mentions its own members; that is what makes it a
    // group, not a dependency on something earlier.
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

/// Re-check `module` with the independent kernel.
///
/// `budget` is the reduction allowance each item gets, the same figure the
/// elaborator's own `Context` is built with.
pub fn recheck_module(module: &Module, budget: u64) -> Result<(), KernelError> {
    let mut kernel = Kernel::new(budget);

    // Binder identities are one space shared across the lowerer, the
    // elaborator, and the archived prelude. Seeding above the module's
    // high-water mark is what keeps a binder the kernel mints — while
    // comparing under a telescope, or eta-contracting — from aliasing one
    // already in a term, which would be a capture.
    kernel.set_local_floor(module.binder_floor);

    // The nominal registry first: a definition's type may name any declaration
    // in the module, including one whose own definitions come later.
    for (name, declaration) in &module.induct_decls {
        kernel.declare_induct(name, declaration);
    }
    for (name, declaration) in &module.struct_decls {
        kernel.declare_struct(name, declaration);
    }

    for index in dependency_order(module) {
        let item = &module.items[index];

        match item {
            Item::Let(definition) => check_definition(
                &mut kernel,
                &Free::from(&definition.name),
                &definition.type_,
                &definition.body,
                &definition.universe_context,
            )?,
            Item::Rec(rec) => {
                let names = item
                    .declared_names()
                    .into_iter()
                    .map(Free::from)
                    .collect::<Vec<_>>();
                let universes = rec.group.universe_context().clone();

                check_rec_group(&mut kernel, &names, &rec.group, &universes)?;
            }
        }
    }

    check_entrypoint(&mut kernel, &module.body, module.type_.as_ref())
}
