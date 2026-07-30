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
//! # The archived prefix
//!
//! A compile judges only the user's items, so the prelude's classification arrives on [`Definition::totality`] rather than being recomputed. That field is not taken on faith: the full walk that runs when the archive is built recomputes every flag and refuses a definition whose recorded verdict is more generous than the kernel's own. Trusting it afterwards is trusting a verdict this crate already reached — the same structure as the rest of the archive-verdict pattern.

use {
    super::{Kernel, KernelError, group_totality},
    curios_core::{Global, Item, Module, Prim, Rec, Subterm, Term, Totality},
    std::collections::{BTreeMap, BTreeSet, HashMap},
};

/// Which half of the erased program a position belongs to. Both are deleted, so both must terminate; the distinction is only what a diagnostic should call it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Erased {
    Proof,
    Type,
}

impl std::fmt::Display for Erased {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Erased::Proof => write!(formatter, "proof"),
            Erased::Type => write!(formatter, "type"),
        }
    }
}

/// Whether a term is partial *in itself*, with no name to blame: an inline `rec` group that does not descend, or a `Prim::Exit`.
///
/// Post-order over the term's DAG, memoized across the whole module — definitions share subterms heavily, and a node settled for one is settled for all.
// Safety: the memo is keyed on `Term`, whose `OnceCell` scalar caches trip Clippy's interior-mutability warning. The logical value is immutable, and hashing and equality stay stable across those caches filling.
#[allow(clippy::mutable_key_type)]
fn locally_partial(kernel: &mut Kernel, term: &Term, memo: &mut HashMap<Term, bool>) -> bool {
    let mut pending = vec![(term.clone(), false)];

    while let Some((node, combining)) = pending.pop() {
        if memo.contains_key(&node) {
            continue;
        }

        if !combining {
            pending.push((node.clone(), true));
            let subterm: &Subterm = &node;
            subterm.any_child_term(&mut |child| {
                if !memo.contains_key(child) {
                    pending.push((child.clone(), false));
                }
                false
            });
            continue;
        }

        let mut partial = matches!(&*node, Subterm::Prim(Prim::Exit(..)));
        if let Subterm::Rec(Rec { group, .. }) = &*node {
            let group = group.clone();
            partial = partial || group_totality(kernel, &group) == Totality::Partial;
        }
        if !partial {
            let subterm: &Subterm = &node;
            subterm.any_child_term(&mut |child| {
                let child_partial = memo.get(child).copied().unwrap_or(false);
                partial = partial || child_partial;
                child_partial
            });
        }

        memo.insert(node, partial);
    }

    memo.get(term).copied().unwrap_or(false)
}

/// Every definition in `module` that is not known to terminate, closed transitively over what each one mentions.
///
/// Items before `checked_from` are the archived prefix: their flags are read from [`Definition::totality`] rather than recomputed, which is what keeps a compile from re-analyzing the standard library. That field is certified rather than believed — the full walk below (`checked_from` of zero) recomputes every flag and refuses a definition whose recorded verdict is more generous than the kernel's own, so an archive that exists carries verdicts this crate reached.
///
/// The closure iterates to a fixpoint rather than assuming one pass suffices: items are stored in binding order, and a definition may mention one stored after it.
#[allow(clippy::mutable_key_type)]
pub fn partial_definitions(
    kernel: &mut Kernel,
    module: &Module,
    checked_from: usize,
) -> (BTreeSet<Global>, Vec<(Global, KernelError)>) {
    let mut mentions: BTreeMap<Global, BTreeSet<Global>> = BTreeMap::new();
    let mut partial: BTreeSet<Global> = BTreeSet::new();
    let mut disagreements = Vec::new();
    let mut memo = HashMap::new();

    for (index, item) in module.items.iter().enumerate() {
        let definitions = match item {
            Item::Let(definition) => vec![definition.clone()],
            Item::Rec(rec) => rec.definitions(),
        };
        // A group that does not descend makes every member partial, whatever each body looks like on its own.
        let rejected = match item {
            Item::Rec(rec) if index >= checked_from => {
                group_totality(kernel, &rec.group) == Totality::Partial
            }
            _ => false,
        };

        for definition in definitions {
            let local = if index < checked_from {
                !definition.totality.is_total()
            } else {
                let computed = rejected
                    || locally_partial(kernel, &definition.body, &mut memo)
                    || locally_partial(kernel, &definition.type_, &mut memo);
                if computed && definition.totality.is_total() {
                    disagreements.push((
                        definition.name.clone(),
                        KernelError::NotTotal {
                            erased: Erased::Proof,
                            reached: None,
                        },
                    ));
                }
                computed
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
            return (partial, disagreements);
        }
    }
}

/// Obligations (T) and (V) over the positions one item's check recorded: each must reach nothing partial, and must not be partial in itself.
#[allow(clippy::mutable_key_type)]
pub fn check_positions(
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
