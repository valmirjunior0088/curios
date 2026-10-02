//! Obligations (T) and (V): everything the erased half of a program reaches must be total, decided here from the module alone.
//!
//! Erasure deletes proofs and types. A proof that may not terminate therefore proves anything, and a type that may not terminate reties the negative knot strict positivity exists to forbid — so both halves carry a termination obligation that the halves the machine actually runs do not.
//!
//! # Seeded from the kernel's own typing
//!
//! `curios-elab` seeds these from a hook its elaborator fires at every check site, on the argument that a later pass can only re-derive which terms are propositions incompletely. That argument is about a *syntactic walk*, and it does not reach this crate: the kernel is itself a typechecker, and it types every term in the module. So both positions come from one record kept during its own walk — a term checked against a `Prop`-sorted type is a proof, and a term checked against a sort is a type — and the coverage is exactly the coverage of the walk that produced it.
//!
//! Deciding it here rather than believing the elaborator is the point: taking this obligation on another crate's word would make an elaborator-only analysis the single defense for a whole class of `False`.
//!
//! # What is already in scope
//!
//! A compile judges only the unit's own items, so the classification of what is already in scope arrives rather than being recomputed — as the certifier's own record, filed with each unit by the walk that judged it ([`Certification`](curios_core::Certification)). Trusting it is trusting a verdict this crate already reached about those exact terms, the same structure as the rest of the archive-verdict pattern. A unit mounted without a record covering it is classified here from its items, exactly as a judged item is. Nothing reads the totality elaboration stamps on a carried [`Definition`](curios_core::Definition): the stamp on an item this walk judges is compared against the walk's own verdict, which is where the two checkers disagree when they do, and a carried one is consulted by nothing.

use {
    super::{Error, Globals, Kernel, Position, Sort},
    curios_analysis::Erased,
    curios_core::{Enter, Free, Global, Item, Module, RecGroup, Reducer, Subterm, Term, Totality},
    std::collections::{BTreeMap, BTreeSet, HashMap},
};

/// Whether `group`'s calls, as this walk typed them, close to a descent on every cycle — the verdict `check_group` recorded.
///
/// A group this walk never checked has no verdict and does not descend: its calls were never typed here, and nothing else is a source of them. That is the case for the items of a unit mounted without a covering record, whose bodies this walk does not judge, and it is the refusing direction.
fn descends(kernel: &Kernel, group: &RecGroup) -> bool {
    kernel.group_verdict(group) == Some(Totality::Total)
}

/// Whether a term calls a host row that diverges — partial in itself, with no name to blame.
///
/// Post-order over the term's DAG on the shared [`Term::walk`] driver. The memo is structural and caller-owned, carried across the whole module rather than per walk — definitions share subterms heavily, and a node settled for one is settled for all.
///
/// The other way a term is partial in itself, an inline group that does not descend, is not read here: the walk notes it where it typed the group, against the definition and the positions that enclosed it, because the group is typed with the binders around it opened and a term holds it closed — the two spellings meet in no lookup.
fn calls_a_diverging_row(term: &Term, memo: &mut HashMap<Term, bool>) -> bool {
    term.walk(
        memo,
        |memo, term| match memo.get(term) {
            Some(&diverges) => Enter::Skip(diverges),
            None => Enter::Descend,
        },
        |memo, term, mut children| {
            let diverges = matches!(&**term, Subterm::Foreign(function, _) if function.diverges())
                || children.any(|child| child);
            memo.insert(term.clone(), diverges);
            diverges
        },
    )
}

/// Whether a term holds a `rec` group anywhere — which, for an item this walk did not type, is a group with no verdict here, and so one that does not descend.
fn holds_a_group(term: &Term) -> bool {
    term.walk(
        &mut (),
        |_, _| Enter::Descend,
        |_, term, mut children| matches!(&**term, Subterm::Rec(_)) || children.any(|child| child),
    )
}

/// Every definition in `module` that is not known to terminate, closed transitively over what each one mentions.
///
/// What `globals` already answers for is read rather than recomputed: its non-total set, which is the certifier's record of each unit it mounted, seeds the closure, and an item it declares is passed over. That is what keeps a compile from re-analyzing the standard library. What it mounted without a covering record is classified here first, from its items, exactly as a judged item is — and a stamp on one of those is not this walk's to compare, since the walk judges nothing of it.
///
/// The stamp comparison on a judged item runs after the closure, against the closed set, because a stamp *asserts* the closure — elaboration's classification closes over mentions as this one does — so a `Total` on a definition partial only through its mentions is exactly as generous as one on a diverging body. No later walk reads a stamp, so a disagreement costs nothing downstream; it is reported because two checkers disagreeing is the signal the second one exists to give.
///
/// **Seeding from the environment is load-bearing rather than an optimization.** The closure is over what a definition *mentions*, and the already-judged items are not carried inside `module`, so nothing else in this walk knows `/std/Async/bind` is partial. A user proof reaching it would then close over a name absent from the set and read as total, which is exactly the identification (T) and (V) exist to prevent.
///
/// The selection is by name for the same reason it is by name in [`recheck_module_verdicts`](crate::recheck_module_verdicts), and skipping is again the direction that needs the argument: an item declaring nothing is recomputed rather than passed over.
///
/// The closure iterates to a fixpoint rather than assuming one pass suffices: items are stored in binding order, and a definition may mention one stored after it.
pub(crate) fn partial_definitions(
    kernel: &mut Kernel,
    module: &Module,
    globals: &Globals,
) -> (BTreeSet<Global>, Vec<(Global, Error)>) {
    curios_profile::profile!("partial_definitions");
    let mut mentions: BTreeMap<Global, BTreeSet<Global>> = BTreeMap::new();
    let mut partial: BTreeSet<Global> = globals.partial().clone();
    let mut stamped_total: Vec<Global> = Vec::new();
    let mut memo = HashMap::new();
    // `typed` says this walk checked `item`, so what its check enclosed was noted as it went; an item it did not check — one of a unit mounted without a covering record — is read from its terms instead, every group in them one with no verdict here.
    let mut classify = |item: &Item, typed: bool| {
        // A group that does not descend makes every member partial, whatever each body looks like on its own.
        let rejected = match item {
            Item::Rec(rec) => !descends(kernel, &rec.group),
            Item::Let(_) => false,
        };

        for (index, definition) in item.definitions().into_iter().enumerate() {
            let encloses = match (typed, item) {
                (true, Item::Rec(rec)) => kernel.member_encloses_partial(&rec.group, index),
                (true, Item::Let(_)) => {
                    kernel.definition_encloses_partial(&Free::from(&definition.name))
                }
                (false, _) => holds_a_group(&definition.body) || holds_a_group(&definition.type_),
            };
            if rejected
                || encloses
                || calls_a_diverging_row(&definition.body, &mut memo)
                || calls_a_diverging_row(&definition.type_, &mut memo)
            {
                partial.insert(definition.name);
            }
            mentions.insert(definition.name, definition.mentions());
        }
    };

    for item in globals.unclassified() {
        classify(item, false);
    }
    for item in &module.items {
        let names = item.declared_names();
        if !names.is_empty() && names.into_iter().all(|name| globals.in_scope(name)) {
            continue;
        }
        stamped_total.extend(
            item.definitions()
                .into_iter()
                .filter(|definition| definition.totality.is_total())
                .map(|definition| definition.name),
        );
        classify(item, true);
    }

    loop {
        let mut changed = false;
        for (name, reached) in &mentions {
            if partial.contains(name) {
                continue;
            }
            if reached.iter().any(|other| partial.contains(other)) {
                partial.insert(*name);
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
                Error::NotTotal {
                    erased: Erased::Proof,
                    reached,
                },
            )
        })
        .collect();

    (partial, disagreements)
}

/// Obligations (T) and (V) over the positions one item's check recorded: each must reach nothing partial, and must not be partial in itself — enclose no group that does not descend, as the walk noted where it typed one, and call no host row that diverges.
pub(crate) fn check_positions(
    positions: &[Position],
    partial: &BTreeSet<Global>,
    memo: &mut HashMap<Term, bool>,
) -> Result<(), Error> {
    curios_profile::profile!("check_positions");
    for position in positions {
        if let Some(reached) = position
            .term
            .free_vars()
            .iter()
            .filter_map(|free| free.as_global())
            .find(|name| partial.contains(name))
        {
            return Err(Error::NotTotal {
                erased: position.erased,
                reached: Some(*reached),
            });
        }
        if position.encloses_partial || calls_a_diverging_row(&position.term, memo) {
            return Err(Error::NotTotal {
                erased: position.erased,
                reached: None,
            });
        }
    }

    Ok(())
}

/// The erased half a term judged at `type_` belongs to, or `None` when the type is relevant and the obligations have nothing to say about it.
///
/// Decided where the position is recorded, while the binders its type mentions are still assumed. A failure propagates rather than reading as "unconstrained": everywhere else in this crate an exhausted budget refuses the item, and this is not the place to make a resource limit read as a pass.
pub(crate) fn erased_half(kernel: &mut Kernel, type_: &Term) -> Result<Option<Erased>, Error> {
    let reduced = kernel.reduce_forced(type_.clone())?;
    // A term at a sort is a type, and erasure deletes it wholesale. This is the one question the structural test answers — what the *runtime* observes — and it is not the question [`carries_information`](crate::Sort) asks, which is what *conversion* observes and where a type counts in full. Reading the two as one predicate would certify a closed inhabitant of `False`.
    if matches!(&*reduced, Subterm::Type(_) | Subterm::Prop) {
        return Ok(Some(Erased::Type));
    }

    Ok(Sort::of(kernel, type_)?.is_prop().then_some(Erased::Proof))
}
