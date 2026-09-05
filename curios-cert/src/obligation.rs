//! Obligations (T) and (V): everything the erased half of a program reaches must be total, decided here from the module alone.
//!
//! Erasure deletes proofs and types. A proof that may not terminate therefore proves anything, and a type that may not terminate reties the negative knot strict positivity exists to forbid — so both halves carry a termination obligation that the halves the machine actually runs do not.
//!
//! # Seeded from the kernel's own typing
//!
//! `curios-elab` seeds these from a hook its elaborator fires at every check site, on the argument that a later pass can only re-derive which terms are propositions incompletely. That argument is about a *syntactic walk*, and it does not reach this crate: the kernel is itself a typechecker, and it types every term in the module. So both positions come from one record kept during its own walk — a term checked against a `Prop`-sorted type is a proof, and a term checked against a sort is a type — and the coverage is exactly the coverage of the walk that produced it.
//!
//! Deciding it here rather than believing the elaborator is the point. This is the one obligation the trusted base used to take on another crate's word, which made an elaborator-only analysis the single defense for a whole class of `False`.
//!
//! # What is already in scope
//!
//! A compile judges only the user's items, so the prelude's classification arrives on [`Definition::totality`](curios_core::Definition::totality) rather than being recomputed. That field is not taken on faith: the walk that runs when the archive is built starts from an empty environment, recomputes every flag, and refuses a definition whose recorded verdict is more generous than the kernel's own. Trusting it afterwards is trusting a verdict this crate already reached — the same structure as the rest of the archive-verdict pattern.

use {
    super::{Globals, Kernel, KernelError, Sort},
    curios_analysis::{Erased, group_totality},
    curios_core::{Enter, Global, Intrinsic, Item, Module, Rec, Reducer, Subterm, Term, Totality},
    std::collections::{BTreeMap, BTreeSet, HashMap},
};

/// Whether a term is partial *in itself*, with no name to blame: an inline `rec` group that does not descend, or an `Intrinsic::ProcExit`.
///
/// Post-order over the term's DAG on the shared [`Term::walk`] driver. The memo is structural and caller-owned, carried across the whole module rather than per walk — definitions share subterms heavily, and a node settled for one is settled for all.
fn locally_partial(kernel: &mut Kernel, term: &Term, memo: &mut HashMap<Term, bool>) -> bool {
    let mut state = (kernel, memo);
    term.walk(
        &mut state,
        |state, term| match state.1.get(term) {
            Some(&partial) => Enter::Skip(partial),
            None => Enter::Descend,
        },
        |state, term, mut children| {
            let mut partial = matches!(&**term, Subterm::Intrinsic(Intrinsic::ProcExit { .. }));
            if let Subterm::Rec(Rec { group, .. }) = &**term {
                partial = partial || group_totality(state.0, group) == Totality::Partial;
            }
            let partial = partial || children.any(|child| child);
            state.1.insert(term.clone(), partial);
            partial
        },
    )
}

/// Every definition in `module` that is not known to terminate, closed transitively over what each one mentions.
///
/// What `globals` already answers for is read rather than recomputed: its non-total set seeds the closure, and an item it declares has its flags read from [`Definition::totality`](curios_core::Definition::totality). That is what keeps a compile from re-analyzing the standard library, and it is certified rather than believed — the walk from an empty environment recomputes every flag and refuses a definition whose recorded verdict is more generous than the kernel's own, so an archive that exists carries verdicts this crate reached.
///
/// The stamp comparison runs after the closure, against the closed set, because a stamp *asserts* the closure: it is what `Globals::of` seeds a later walk's non-total set from, so a `Total` on a definition partial only through its mentions is exactly as generous as one on a diverging body. Compared against the local half alone — which this once was — that lie passed the filing walk, and a proof reaching the mis-stamped definition was then certified on the compile path with nothing anywhere refusing the route.
///
/// **Seeding from the environment is load-bearing rather than an optimization.** The closure is over what a definition *mentions*, and once the already-judged items stop being carried inside `module` there is nothing left in this walk that knows `/std/Async/bind` is partial. A user proof reaching it would then close over a name absent from the set and read as total, which is exactly the identification (T) and (V) exist to prevent.
///
/// The selection is by name for the same reason it is by name in [`recheck_module_verdicts`](crate::recheck_module_verdicts), and skipping is again the direction that needs the argument: an item declaring nothing is recomputed rather than passed over.
///
/// The closure iterates to a fixpoint rather than assuming one pass suffices: items are stored in binding order, and a definition may mention one stored after it.
pub(crate) fn partial_definitions(
    kernel: &mut Kernel,
    module: &Module,
    globals: &Globals,
) -> (BTreeSet<Global>, Vec<(Global, KernelError)>) {
    let mut mentions: BTreeMap<Global, BTreeSet<Global>> = BTreeMap::new();
    let mut partial: BTreeSet<Global> = globals.partial().clone();
    let mut stamped_total: Vec<Global> = Vec::new();
    let mut memo = HashMap::new();

    for item in &module.items {
        let definitions = item.definitions();
        let names = item.declared_names();
        let carried = !names.is_empty() && names.into_iter().all(|name| globals.in_scope(name));
        // A group that does not descend makes every member partial, whatever each body looks like on its own.
        let rejected = match item {
            Item::Rec(rec) if !carried => group_totality(kernel, &rec.group) == Totality::Partial,
            _ => false,
        };

        for definition in definitions {
            let local = if carried {
                !definition.totality.is_total()
            } else {
                if definition.totality.is_total() {
                    stamped_total.push(definition.name.clone());
                }
                rejected
                    || locally_partial(kernel, &definition.body, &mut memo)
                    || locally_partial(kernel, &definition.type_, &mut memo)
            };
            if local {
                partial.insert(definition.name.clone());
            }
            mentions.insert(definition.name.clone(), definition.mentions());
        }
    }

    loop {
        let mut changed = false;
        for (name, reached) in &mentions {
            if partial.contains(name) {
                continue;
            }
            if reached.iter().any(|other| partial.contains(other)) {
                partial.insert(name.clone());
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    // See the doc above: the stamp asserts the closure, so it is compared against the closed set. A partial mention is named where one exists; a definition partial in itself alone has nothing to blame.
    let disagreements = stamped_total
        .into_iter()
        .filter(|name| partial.contains(name))
        .map(|name| {
            let reached = mentions
                .get(&name)
                .and_then(|reached| reached.iter().find(|other| partial.contains(*other)))
                .cloned();
            (
                name,
                KernelError::NotTotal {
                    erased: Erased::Proof,
                    reached,
                },
            )
        })
        .collect();

    (partial, disagreements)
}

/// Obligations (T) and (V) over the positions one item's check recorded: each must reach nothing partial, and must not be partial in itself.
pub(crate) fn check_positions(
    kernel: &mut Kernel,
    positions: &[(Term, Erased)],
    partial: &BTreeSet<Global>,
    memo: &mut HashMap<Term, bool>,
) -> Result<(), KernelError> {
    for (term, erased) in positions {
        if let Some(reached) = term
            .free_vars()
            .iter()
            .filter_map(|free| free.as_global())
            .find(|name| partial.contains(name))
        {
            return Err(KernelError::NotTotal {
                erased: *erased,
                reached: Some(reached.clone()),
            });
        }
        if locally_partial(kernel, term, memo) {
            return Err(KernelError::NotTotal {
                erased: *erased,
                reached: None,
            });
        }
    }

    Ok(())
}

/// The erased half a term judged at `type_` belongs to, or `None` when the type is relevant and the obligations have nothing to say about it.
///
/// Decided where the position is recorded, while the binders its type mentions are still assumed. A failure propagates rather than reading as "unconstrained": everywhere else in this crate an exhausted budget refuses the item, and this is not the place to make a resource limit read as a pass.
pub(crate) fn erased_half(
    kernel: &mut Kernel,
    type_: &Term,
) -> Result<Option<Erased>, KernelError> {
    let reduced = kernel.reduce_forced(type_.clone())?;
    // A term at a sort is a type, and erasure deletes it wholesale. This is the one question the structural test answers — what the *runtime* observes — and it is not the question [`carries_information`](crate::Sort) asks, which is what *conversion* observes and where a type counts in full. Reading the two as one predicate certified a closed inhabitant of `False`.
    if matches!(&*reduced, Subterm::Type(_) | Subterm::Prop) {
        return Ok(Some(Erased::Type));
    }

    Ok(Sort::of(kernel, type_)?.is_prop().then_some(Erased::Proof))
}
