//! Size-change termination: which `rec` groups descend, and therefore which
//! definitions erasure may safely delete.
//!
//! Curios keeps general recursion. What it cannot keep is general recursion in
//! the places erasure *removes*, because a divergent type breaks type formation
//! and a divergent proof proves anything, while a program that loops is only a
//! program that loops. This module supplies the fact both gates need: whether
//! a recursive group terminates on every call path.
//!
//! A group is accepted by the size-change principle (Lee–Jones–Ben-Amram, in
//! the shape Abel's `foetus` gave it for dependent types). Each recursive call
//! contributes a **call matrix** grading every callee argument against every
//! caller parameter as strictly smaller, equal, or unrelated. The matrices are
//! closed transitively under composition, and the group is accepted when every
//! **idempotent** matrix in that closure carries a strict decrease on its
//! diagonal — an idempotent matrix describes a call path that can repeat
//! forever, so a decrease on it is a decrease that cannot be sustained.
//!
//! Size-change rather than structural recursion because the corpus needs it.
//! `/std/BigNat/add/raw` descends on *either* of two `Bits` arguments depending
//! on the arm, `add/raw_assoc` does the same over three, and `add/raw_comm`
//! needs the mutual closure across two members. A rule keyed to one designated
//! argument rejects all of them, and a fold cannot express them either, because
//! a fold cannot short-circuit.
//!
//! **Match refinement is part of the size order, not an optimization.** A
//! parameter scrutinized by an enclosing arm is expanded through that arm's
//! constructor before it is compared, so in `add/raw`'s empty-`x` arm the
//! literal argument `b\` grades *equal* to `x` rather than unknown. Without
//! that, the two nil-arm matrices compose to an all-unknown matrix, which is
//! idempotent with no decrease anywhere on its diagonal, and `add/raw` is
//! rejected on a call path that cannot actually occur.
//!
//! Rejection is a **classification, not an error**. `/std/Async` is
//! corecursive, `/std/Json/decode`'s parsers are nullary and productive, and
//! `/std/BigNat/convert/to_str_go` recurses on a computed quotient; none passes
//! this check and none needs to. They stay usable everywhere erasure keeps
//! them. Only [`crate::check_type_totality`] and [`crate::check_proof_totality`]
//! turn a `Partial` classification into a rejection, and only for the positions
//! that erase.

mod reach;
use reach::*;

#[cfg(test)]
mod tests;

use {
    super::{Context, Definition, Error, Item, Module, RecItem, is_prop, zonk},
    curios_cert::{Totality, group_totality, yields_a_sort},
    curios_core::{Global, Prim, Rec, RecGroup, Subterm, Term},
    std::collections::{BTreeMap, BTreeSet, HashMap},
};
/// Which half of erasure an obligation guards.
///
/// The two obligations run one analysis over one partiality relation, seeded
/// twice. This says which seeding rejected, because the two failures want
/// different advice: a type must be total or type formation may not terminate,
/// a proof must be total or it proves anything.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Erased {
    Type,
    Proof,
}

impl std::fmt::Display for Erased {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Erased::Type => write!(formatter, "type"),
            Erased::Proof => write!(formatter, "proof"),
        }
    }
}
/// Every top-level definition's totality.
///
/// Group rejection is only the local part. A definition is also `Partial` if it
/// mentions [`Prim::Exit`] — which erasure drops, so an exit behind a nullary
/// proof never fires — or if it mentions anything already `Partial`. That last
/// clause is a transitive closure, and it is what makes the flag a usable
/// cross-module summary: "this prelude definition is partial" means something
/// partial is in its closure, so a user proof mentioning it inherits the same.
///
/// `inherited` carries the totality of definitions `module` references but does
/// not define — the replayed prelude prefix. Because each of its flags is
/// already a closure, the walk stops at that boundary instead of re-analyzing
/// the standard library on every compilation. Pass an empty map when `module`
/// is the whole program.
/// Classify one definition against the verdicts already recorded, and record it.
///
/// The whole-module pass needs a fixpoint because it sees every item at once.
/// Items arrive here in dependency order, so everything a definition mentions is
/// already classified and a single pass suffices. A name with no verdict is a
/// member of the group currently elaborating, which [`group_totality`] settles
/// for the group as a whole.
pub fn record_definition_totality(context: &mut Context, definition: &Definition, group: Totality) {
    let partial = !group.is_total()
        || definition_is_locally_partial(context, definition, &mut LocalMemo::new())
        || mentioned(definition).iter().any(|name| {
            context
                .definition_totality(name)
                .is_some_and(|totality| !totality.is_total())
        });
    let totality = match partial {
        true => Totality::Partial,
        false => Totality::Total,
    };
    context.record_definition_totality(&definition.name, totality);
}

/// Obligation (T), at the one point a non-productive type-level loop can still
/// be diagnosed: before the type is reduced.
///
/// The whole-module gate runs post-zonk, which is after elaboration has already
/// performed the type-level reduction it exists to make safe. A type that
/// reaches a partial definition and *is productive* survives elaboration and the
/// gate rejects it; one that is not productive spins until the step budget dies,
/// and the program is refused for apparently running out of a resource that no
/// amount of would have helped. This refuses it first, by name, for the reason
/// it is actually wrong.
///
/// It reads the *lowered* type, before elaboration touches it, so the mentions
/// it sees are the written ones. That is why it is an early net rather than a
/// replacement: the post-zonk gate still runs, and still sees everything
/// elaboration produces.
pub fn check_written_type_totality(
    context: &mut Context,
    type_: &Term,
    site: &str,
) -> Result<(), Error> {
    let offender = type_
        .free_vars()
        .into_iter()
        .filter_map(|free| free.as_global().cloned())
        .find(|name| {
            context
                .definition_totality(name)
                .is_some_and(|totality| !totality.is_total())
        });

    match offender {
        None => Ok(()),
        Some(offender) => Err(Error::PartialInErasedPosition {
            erased: Erased::Type,
            site: site.to_string(),
            offender: Some(offender.to_string()),
        }),
    }
}

// Safety: the memo is keyed on `Term`, whose `OnceCell` scalar caches trip
// Clippy's interior-mutability warning. The logical value is immutable, and
// hashing and equality stay stable across those caches filling — the same
// caveat `checked_proof_positions` carries.
#[allow(clippy::mutable_key_type)]
pub fn classify_module(
    context: &mut Context,
    module: &Module,
    inherited: &BTreeMap<Global, Totality>,
) -> BTreeMap<Global, Totality> {
    let mut local: BTreeMap<Global, bool> = BTreeMap::new();
    let mut mentions: BTreeMap<Global, BTreeSet<Global>> = BTreeMap::new();
    // One cache for the whole module: definitions share subterms heavily, and a
    // node classified for one is classified for all.
    let mut memo = LocalMemo::new();

    for item in &module.items {
        match item {
            Item::Let(definition) => {
                let partial = definition_is_locally_partial(context, definition, &mut memo);
                local.insert(definition.name.clone(), partial);
                mentions.insert(definition.name.clone(), mentioned(definition));
            }
            Item::Rec(rec) => {
                let rejected = group_totality(context, &rec.group) == Totality::Partial;
                for definition in rec.definitions() {
                    let partial =
                        rejected || definition_is_locally_partial(context, &definition, &mut memo);
                    local.insert(definition.name.clone(), partial);
                    mentions.insert(definition.name.clone(), mentioned(&definition));
                }
            }
        }
    }

    let mut partial: BTreeSet<Global> = local
        .iter()
        .filter(|(_, value)| **value)
        .map(|(name, _)| name.clone())
        .collect();

    // A name neither defined here nor inherited is treated as total. Every
    // `Global` a zonked module mentions is defined either in it or in the
    // prefix it was spliced onto, so that case is unreachable rather than an
    // approximation.
    let reaches_partial = |partial: &BTreeSet<Global>, reached: &BTreeSet<Global>| {
        reached.iter().any(|other| {
            partial.contains(other)
                || inherited
                    .get(other)
                    .is_some_and(|totality| !totality.is_total())
        })
    };

    loop {
        let mut changed = false;
        for (name, reached) in &mentions {
            if partial.contains(name) {
                continue;
            }
            if reaches_partial(&partial, reached) {
                partial.insert(name.clone());
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    local
        .keys()
        .map(|name| {
            let totality = match partial.contains(name) {
                true => Totality::Partial,
                false => Totality::Total,
            };
            (name.clone(), totality)
        })
        .collect()
}

/// Classify `module` and stamp each definition's [`Definition::totality`].
///
/// Runs post-zonk, beside [`crate::check_positivity`], and for the same reason:
/// the terms it reads are final and meta-free here, so no metavariable can be
/// solved to a partial term after the fact.
///
/// This is the only place the flag is written, and both gates read what it
/// wrote rather than re-deriving it — classification reduces, so doing it once
/// per module is the difference between one pass over the group corpus and
/// three.
pub fn record_totality(
    context: &mut Context,
    module: &mut Module,
    inherited: &BTreeMap<Global, Totality>,
) {
    curios_profile::profile!("record_totality");
    let classified = classify_module(context, module, inherited);
    let of = |name: &Global| classified.get(name).copied().unwrap_or_default();

    for item in &mut module.items {
        match item {
            Item::Let(definition) => definition.totality = of(&definition.name),
            Item::Rec(rec) => {
                for member in &mut rec.definitions {
                    member.totality = of(&member.name);
                }
            }
        }
    }
}

/// The totality [`record_totality`] stamped onto every definition in `module`.
///
/// This is how the replay path inherits the prelude's verdicts: the archive
/// stores the stamped module, so a user compilation reads the flags instead of
/// re-analyzing the standard library.
pub fn recorded_totality(module: &Module) -> BTreeMap<Global, Totality> {
    let mut recorded = BTreeMap::new();
    for item in &module.items {
        match item {
            Item::Let(definition) => {
                recorded.insert(definition.name.clone(), definition.totality);
            }
            Item::Rec(rec) => {
                for member in &rec.definitions {
                    recorded.insert(member.name.clone(), member.totality);
                }
            }
        }
    }
    recorded
}

/// The verdict for every name in play: `module`'s own stamps over `inherited`.
fn settled(module: &Module, inherited: &BTreeMap<Global, Totality>) -> BTreeMap<Global, Totality> {
    let mut settled = inherited.clone();
    settled.extend(recorded_totality(module));
    settled
}

/// Every way the given positions fail to be total, with the site of each.
///
/// Two ways, and a position can fail either: it reaches a definition already
/// classified `Partial`, or it *is* partial with no name to blame — an inline
/// `rec` that does not descend, or a `Prim::Exit`.
// Safety: the memo is keyed on `Term`, whose `OnceCell` scalar caches trip
// Clippy's interior-mutability warning. The logical value is immutable, and
// hashing and equality stay stable across those caches filling — the same
// caveat `checked_proof_positions` carries.
#[allow(clippy::mutable_key_type)]
fn faults(
    context: &mut Context,
    module: &Module,
    positions: &[Position],
    inherited: &BTreeMap<Global, Totality>,
) -> Vec<(String, Fault)> {
    let seeded = seeds(positions);
    let reached = reachable(module, seeded);
    let settled = settled(module, inherited);
    let named = offenders(&reached, &settled);

    let mut faults = Vec::new();
    // One cache across every position: (V) seeds a literal's derivation once
    // per link, and those links share their tails.
    let mut memo = LocalMemo::new();
    for position in positions {
        if term_is_locally_partial(context, &position.term, &mut memo) {
            faults.push((position.site.clone(), Fault::Inline));
        }
    }
    // One named fault per offending definition, attributed to the first
    // position that names it. The closure is over the union of the seeds, so
    // the exact position is a best effort; which definition is partial is not.
    for name in named {
        let site = positions
            .iter()
            .find(|position| {
                position
                    .term
                    .free_vars()
                    .iter()
                    .filter_map(|free| free.as_global())
                    .any(|global| *global == name)
            })
            .map(|position| position.site.clone())
            .unwrap_or_else(|| "a position reached from here".to_string());
        faults.push((site, Fault::Named(name)));
    }
    faults
}

/// Obligation **(T)**: everything a type position reaches must be total.
///
/// A divergent type breaks type formation, and — through
/// `rec Bad : Type = Sink(Bad)` — ties the negative knot strict positivity
/// exists to forbid without ever writing an `induct`. Runs post-zonk, after
/// [`record_totality`], whose flags it reads.
pub fn check_type_totality(
    context: &mut Context,
    module: &Module,
    inherited: &BTreeMap<Global, Totality>,
) -> Result<(), Error> {
    curios_profile::profile!("check_type_totality");
    let mut positions = type_positions(module);
    positions.extend(checked_type_positions(context)?);
    report(context, module, &positions, inherited, Erased::Type)
}

/// The terms elaboration settled *at a sort* — a term whose type is `Type` or
/// `Prop` is itself a type, wherever it was written.
///
/// This is the half [`type_positions`] structurally cannot reach. That walk is a
/// syntactic reading of where types are *written*: an annotation, a motive, a
/// declaration telescope, a definition whose type ends in a sort. A type passed
/// as an *argument* is written nowhere it looks — `annotations` has no case for
/// an application's parameters — so `ignore(@Shape(inf), 5)` handed a partial
/// value to a type-level function with nothing seeded, while the same type in an
/// annotation was rejected.
///
/// The two seedings are **incomparable**, not nested, which a count of each
/// obscured: the walk sees definition bodies whose type ends in a sort, and no
/// typing judgment classifies those as type positions; this sees type arguments,
/// which no syntactic walk can find without re-deriving the head's telescope —
/// the re-derivation that cost (V) six defects. Taking the union costs one pass
/// over already-recorded terms and needs neither.
fn checked_type_positions(context: &mut Context) -> Result<Vec<Position>, Error> {
    let settled = context.checked().to_vec();
    let mut positions = Vec::new();

    for (term, type_, site) in settled {
        let type_ = zonk(context, &type_)?;
        // The term *is* a type exactly when its own type is a sort. A universe
        // instance wraps one without changing that.
        let sort = match &*type_ {
            Subterm::Type(_) | Subterm::Prop => true,
            Subterm::UniverseInst(instance) => {
                matches!(&*instance.head, Subterm::Type(_) | Subterm::Prop)
            }
            _ => false,
        };
        if sort {
            positions.push(Position {
                term: zonk(context, &term)?,
                site: format!("a type in {site}"),
            });
        }
    }

    Ok(positions)
}

/// Obligation **(V)**: everything a `Prop`-checked term reaches must be total.
///
/// A divergent proof proves anything, and erasure deletes it — so an `exit`
/// behind a proposition never fires and the program continues with a forged
/// invariant. Independent of [`check_type_totality`]: neither obligation
/// subsumes the other.
pub fn check_proof_totality(
    context: &mut Context,
    module: &Module,
    inherited: &BTreeMap<Global, Totality>,
) -> Result<(), Error> {
    curios_profile::profile!("check_proof_totality");
    let positions = checked_proof_positions(context)?;
    report(context, module, &positions, inherited, Erased::Proof)
}

/// Obligation (V)'s seed: every term elaboration settled at a `Prop`-sorted
/// type.
///
/// Drains what [`Context::record_checked`] collected and keeps the
/// propositions. This is the whole of (V)'s seeding. There is no walk, because
/// there is nothing to look for — elaboration already decided, for every term
/// in the program, what type that term has, and the only thing a later pass can
/// do with that question is re-derive it, incompletely.
///
/// Two properties follow from the seed rather than from care. **Coverage** is a
/// consequence of elaboration being a typechecker: a position it never settles
/// is a position the program was never typed at. And the **prelude boundary** is
/// a consequence of replay — the archived prefix is defined into the context
/// rather than elaborated, so it settles nothing and seeds nothing, and its
/// verdicts arrive through [`recorded_totality`] instead.
///
/// Sort-hood is decided here rather than at the hook because a type may still
/// carry unsolved metavariables while elaborating. Post-zonk every solution is
/// materialized, so [`is_prop`](crate::is_prop) is asked once per *distinct*
/// type, and hash-consing keeps that set far smaller than the term count.
// Safety: the memo is keyed on `Term`, which carries `OnceCell` scalar caches
// and so trips Clippy's interior-mutability warning. The logical value is fully
// immutable, and hashing and equality stay stable across those caches filling.
#[allow(clippy::mutable_key_type)]
fn checked_proof_positions(context: &mut Context) -> Result<Vec<Position>, Error> {
    let checked = context.take_checked();
    let mut memo: HashMap<Term, bool> = HashMap::new();
    let mut positions = Vec::new();

    for (term, type_, site) in checked {
        let type_ = zonk(context, &type_)?;
        let prop = match memo.get(&type_) {
            Some(prop) => *prop,
            None => {
                let prop = is_prop(context, &type_)?;
                memo.insert(type_, prop);
                prop
            }
        };
        if prop {
            positions.push(Position {
                term: zonk(context, &term)?,
                site: format!("a proof in {site}"),
            });
        }
    }

    Ok(positions)
}

fn report(
    context: &mut Context,
    module: &Module,
    positions: &[Position],
    inherited: &BTreeMap<Global, Totality>,
    erased: Erased,
) -> Result<(), Error> {
    match faults(context, module, positions, inherited)
        .into_iter()
        .next()
    {
        None => Ok(()),
        Some((site, Fault::Named(name))) => Err(Error::PartialInErasedPosition {
            erased,
            site,
            offender: Some(name.to_string()),
        }),
        Some((site, Fault::Inline)) => Err(Error::PartialInErasedPosition {
            erased,
            site,
            offender: None,
        }),
    }
}

/// Obligation (T), in the one place it cannot wait for the post-zonk pass.
///
/// A member of a partial group may not have a sort in an *extractable*
/// position of its type: its codomain after peeling arrows, or any component
/// reachable by projection. Parameter positions are exempt, so an ordinary
/// partial polymorphic program keeping `@A : Type` is untouched.
///
/// This exists because `rec Bad : Type = (Bad) -> False` overflows the
/// compiler's stack while elaborating its first *use* — reducing `Bad` rebuilds
/// an arrow whose domain is `Bad` — which is long before any whole-module gate
/// runs. Without it the compiler aborts where it should diagnose. The closure
/// form is what makes the language sound; this is what makes it answer.
pub fn check_rec_totality(
    context: &mut Context,
    group: &RecGroup,
    names: &[String],
) -> Result<(), Error> {
    let extractable = group
        .iter()
        .enumerate()
        .filter(|(_, member)| yields_a_sort(member.type_.body()))
        .map(|(index, _)| index)
        .collect::<Vec<_>>();

    if extractable.is_empty() || group_totality(context, group).is_total() {
        return Ok(());
    }

    let site = extractable
        .into_iter()
        .filter_map(|index| names.get(index))
        .map(|name| format!("'{name}'"))
        .collect::<Vec<_>>()
        .join(", ");

    Err(Error::PartialInErasedPosition {
        erased: Erased::Type,
        site: format!("the recursive definition {site}"),
        offender: None,
    })
}

/// [`check_rec_totality`] for a top-level `rec`, which knows its own names.
pub fn check_rec_item_totality(context: &mut Context, rec: &RecItem) -> Result<(), Error> {
    let names = rec
        .definitions()
        .into_iter()
        .map(|definition| definition.name.to_string())
        .collect::<Vec<_>>();
    check_rec_totality(context, &rec.group, &names)
}
/// Whether this term is partial on its own account, with no name to blame: it
/// contains a `rec` group that does not descend, or an exit.
#[allow(clippy::mutable_key_type)]
fn term_is_locally_partial(context: &mut Context, term: &Term, memo: &mut LocalMemo) -> bool {
    locally_partial(context, term, memo)
}

/// Per-node verdicts for [`locally_partial`], carried across the terms one pass
/// classifies.
///
/// Keyed on the term, whose hash is cached on the node and whose equality is
/// already a worklist walk, so a lookup does not re-traverse what it is looking
/// up. See [`checked_proof_positions`] for the same key and the same caveat.
#[allow(clippy::mutable_key_type)]
type LocalMemo = HashMap<Term, bool>;

/// Whether `term` contains a `Prim::Exit` or a `rec` group that does not
/// descend — the "is this term partial on its own account" test both
/// obligations apply one level below a definition.
///
/// **Iterative and memoized, and both are load-bearing.** (V) seeds one
/// position per link of a `Str` literal's UTF-8 derivation, and those links
/// share their tails, so the native per-node recursion this replaces cost one
/// stack frame per byte *and* re-walked the shared tail once per position —
/// quadratic in the literal's length. Measured on a 640-byte literal, that was
/// 2.0s of a 2.1s compile, and a 10KiB literal overflowed the stack outright.
/// Depth is not steps, so the reduction budget cannot bound either one.
///
/// The memo is what makes the sharing pay: hash-consing gives the tails one
/// node, so each distinct node is classified once however many positions reach
/// it. Per-position answers are unchanged — the cache records each node's own
/// verdict, not whether some earlier position already reported it.
#[allow(clippy::mutable_key_type)]
fn locally_partial(context: &mut Context, term: &Term, memo: &mut LocalMemo) -> bool {
    // Post-order over the term's DAG: a node is pushed once to expand its
    // children and once to combine their verdicts, and the stack ordering is
    // what guarantees every child is settled before the combine runs.
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
            partial = partial || group_totality(context, &group) == Totality::Partial;
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

/// Whether this definition is partial on its own account: it mentions an exit,
/// or it contains a local `rec` group that does not descend.
#[allow(clippy::mutable_key_type)]
fn definition_is_locally_partial(
    context: &mut Context,
    definition: &Definition,
    memo: &mut LocalMemo,
) -> bool {
    locally_partial(context, &definition.body, memo)
        || locally_partial(context, &definition.type_, memo)
}

/// Every top-level name this definition mentions, by free variable.
pub(crate) fn mentioned(definition: &Definition) -> BTreeSet<Global> {
    definition
        .body
        .free_vars()
        .into_iter()
        .chain(definition.type_.free_vars())
        .filter_map(|free| free.as_global().cloned())
        .collect()
}
