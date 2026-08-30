//! The elaborator's driver for the shared size-change totality analysis.
//!
//! The analysis itself lives in `curios-analysis/src/totality.rs` (see that module for the size-change principle and its rationale) and is run by both checkers; what belongs here is the driving: seeding the two erased-half obligations — (T) from written type positions, (V) from the proof positions elaboration recorded — and classifying every top-level definition transitively against the group verdicts, with `ProcExit` and inherited prelude partiality folded in.
//!
//! Rejection is a **classification, not an error**. `/std/Async` is corecursive, `/std/Json/decode`'s parsers are nullary and productive, and `/std/BigNat/convert/to_str_go` recurses on a computed quotient; none passes this check and none needs to. They stay usable everywhere erasure keeps them. Only [`crate::check_type_totality`] and [`crate::check_proof_totality`] turn a `Partial` classification into a rejection, and only for the positions that erase.

mod reach;
use reach::*;

#[cfg(test)]
mod tests;

/// Which half of erasure an obligation guards — `curios-cert`'s type, re-exported rather than restated.
///
/// The two obligations run one analysis over one partiality relation, seeded twice, and this says which seeding rejected: a type must be total or type formation may not terminate, a proof must be total or it proves anything. Both checkers state that same distinction and each carries it in its own diagnostic, so a second declaration of it was two vocabularies that could drift while describing one thing. The certifier's is the one that survives, because it is the half `KernelError::NotTotal` already exports.
pub use curios_analysis::Erased;
use {
    super::{Context, Error, is_prop, zonk},
    curios_analysis::{group_totality, yields_a_sort},
    curios_core::{
        Definition, Enter, Global, Intrinsic, Item, Module, Rec, RecGroup, RecItem, Subterm, Term,
        Totality,
    },
    std::{
        collections::{BTreeMap, BTreeSet, HashMap},
        rc::Rc,
    },
};

/// Every top-level definition's totality.
///
/// Group rejection is only the local part. A definition is also `Partial` if it mentions [`Intrinsic::ProcExit`] — which erasure drops, so an exit behind a nullary proof never fires — or if it mentions anything already `Partial`. That last clause is a transitive closure, and it is what makes the flag a usable cross-module summary: "this prelude definition is partial" means something partial is in its closure, so a user proof mentioning it inherits the same.
///
/// `inherited` carries the totality of definitions `module` references but does not define — the replayed prelude prefix. Because each of its flags is already a closure, the walk stops at that boundary instead of re-analyzing the standard library on every compilation. Pass an empty map when `module` is the whole program. Classify one definition against the verdicts already recorded, and record it.
///
/// The whole-module pass needs a fixpoint because it sees every item at once. Items arrive here in dependency order, so everything a definition mentions is already classified and a single pass suffices. A name with no verdict is a member of the group currently elaborating, which [`group_totality`] settles for the group as a whole.
pub fn record_definition_totality(context: &mut Context, definition: &Definition, group: Totality) {
    let partial = !group.is_total()
        || definition_is_locally_partial(context, definition, &mut LocalMemo::new())
        || definition.mentions().iter().any(|name| {
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

/// Obligation (T), at the one point a non-productive type-level loop can still be diagnosed: before the type is reduced.
///
/// The whole-module gate runs post-zonk, which is after elaboration has already performed the type-level reduction it exists to make safe. A type that reaches a partial definition and *is productive* survives elaboration and the gate rejects it; one that is not productive spins until the step budget dies, and the program is refused for apparently running out of a resource no amount of which would have helped. This refuses it first, by name, for the reason it is actually wrong.
///
/// It reads the *lowered* type, before elaboration touches it, so the mentions it sees are the written ones. That is why it is an early net rather than a replacement: the post-zonk gate still runs, and still sees everything elaboration produces.
/// The first global `term` names by spelling whose recorded totality is not total — the syntactic reading behind [`check_written_type_totality`], shared with the bound site that re-reports an exhausted discharge by the same name.
pub(crate) fn partial_offender(context: &Context, term: &Term) -> Option<Global> {
    term.free_vars()
        .into_iter()
        .filter_map(|free| free.as_global().cloned())
        .find(|name| {
            context
                .definition_totality(name)
                .is_some_and(|totality| !totality.is_total())
        })
}

/// An exhausted discharge of `proposition`, re-reported by the partial definition it names when it names one, and the exhaustion itself otherwise. A bound whose subject does not terminate used to spend the budget and report that, where the same subject in a declared type is refused by name before anything is reduced; the check still runs — a subject that terminates discharges, whatever the analysis classified it — and only a spent budget is re-read for a name. The obligation is `documentation/design/language/totality-of-the-erased-program.md`'s.
pub(crate) fn exhausted_bound(
    context: &Context,
    error: Error,
    proposition: &Term,
    site: String,
) -> Error {
    match (&error, partial_offender(context, proposition)) {
        (Error::ReduceExhausted { .. } | Error::ConvertExhausted { .. }, Some(offender)) => {
            Error::PartialInErasedPosition {
                erased: Erased::Proof,
                site,
                offender: Some(offender.to_string()),
            }
        }
        _ => error,
    }
}

pub fn check_written_type_totality(
    context: &mut Context,
    type_: &Term,
    site: &str,
) -> Result<(), Error> {
    match partial_offender(context, type_) {
        None => Ok(()),
        Some(offender) => Err(Error::PartialInErasedPosition {
            erased: Erased::Type,
            site: site.to_string(),
            offender: Some(offender.to_string()),
        }),
    }
}

pub fn classify_module(
    context: &mut Context,
    module: &Module,
    inherited: &BTreeMap<Global, Totality>,
) -> BTreeMap<Global, Totality> {
    let mut local: BTreeMap<Global, bool> = BTreeMap::new();
    let mut mentions: BTreeMap<Global, BTreeSet<Global>> = BTreeMap::new();
    // One cache for the whole module: definitions share subterms heavily, and a node classified for one is classified for all.
    let mut memo = LocalMemo::new();

    for item in &module.items {
        match item {
            Item::Let(definition) => {
                let partial = definition_is_locally_partial(context, definition, &mut memo);
                local.insert(definition.name.clone(), partial);
                mentions.insert(definition.name.clone(), definition.mentions());
            }
            Item::Rec(rec) => {
                let rejected = group_totality(context, &rec.group) == Totality::Partial;
                for definition in rec.definitions() {
                    let partial =
                        rejected || definition_is_locally_partial(context, &definition, &mut memo);
                    local.insert(definition.name.clone(), partial);
                    mentions.insert(definition.name.clone(), definition.mentions());
                }
            }
        }
    }

    let mut partial: BTreeSet<Global> = local
        .iter()
        .filter(|(_, value)| **value)
        .map(|(name, _)| name.clone())
        .collect();

    // A name neither defined here nor inherited is treated as total. Every `Global` a zonked module mentions is defined either in it or in one of the units it was elaborated against, and `inherited` is flattened over all of them — so that case is unreachable rather than an approximation, whatever the scope holds.
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
/// Runs post-zonk, beside [`crate::check_positivity`], and for the same reason: the terms it reads are final and meta-free here, so no metavariable can be solved to a partial term after the fact.
///
/// This is the only place the flag is written, and both gates read what it wrote rather than re-deriving it — classification reduces, so doing it once per module is the difference between one pass over the group corpus and three.
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
/// This is how the replay path inherits the prelude's verdicts: the archive stores the stamped module, so a user compilation reads the flags instead of re-analyzing the standard library.
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
/// Two ways, and a position can fail either: it reaches a definition already classified `Partial`, or it *is* partial with no name to blame — an inline `rec` that does not descend, or an `Intrinsic::ProcExit`.
fn faults(
    context: &mut Context,
    module: &Module,
    positions: &[Position],
    inherited: &BTreeMap<Global, Totality>,
) -> Vec<(String, Fault)> {
    curios_profile::sample!("totality::faults_positions", positions.len());
    let seeded = seeds(positions);
    let reached = reachable(module, seeded);
    curios_profile::sample!("totality::reached", reached.len());
    let settled = settled(module, inherited);
    let named = offenders(&reached, &settled);

    let mut faults = Vec::new();
    // One cache across every position: (V) seeds a literal's derivation once per link, and those links share their tails.
    let mut memo = LocalMemo::new();
    for position in positions {
        if locally_partial(context, &position.term, &mut memo) {
            faults.push((position.site.to_string(), Fault::Inline));
        }
    }
    // One named fault per offending definition, attributed to the first position that names it. The closure is over the union of the seeds, so the exact position is a best effort; which definition is partial is not.
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
            .map(|position| position.site.to_string())
            .unwrap_or_else(|| "a position reached from here".to_string());
        faults.push((site, Fault::Named(name)));
    }
    faults
}

/// Every term either erased-half obligation has zonked, shared across both of them and across both the types they test and the terms they keep.
///
/// Not an optimization detail but a contract. (T) and (V) are seeded from the *same* [`Context::record_checked`](crate::Context) entries, and each needs the zonked type of every entry plus the zonked term of every entry it keeps — so a cache local to either does the same work twice. Measured over the prelude: 185,271 entries collapse to 31,955 distinct recorded types, zonked at 27.6 s in one pass and 27.3 s in the other, the same work to three digits; and 59,818 kept positions collapse to 17,051 distinct terms.
///
/// One map serves types and terms alike because zonk is a *function of its argument*: a term appearing in both populations has one zonked form, and sharing the entry is correct rather than merely convenient. Sound to share across the passes for the reason either could memoize alone — the solution set is final once `zonk_module` has run.
///
/// The cost is memory. This is held across both obligations rather than dropped between them, and the prelude's peak rose 52 MiB when it started spanning them. That is the trade; the cache is not free, it is cheaper than the work it removes.
pub type Zonked = HashMap<Term, Term>;

/// `term` zonked, through `cache`.
///
/// The four call sites that need this are two types and two terms across the two obligations; before this they were four copies of the same match, two of which had no cache at all.
fn zonked(context: &Context, cache: &mut Zonked, term: &Term) -> Result<Term, Error> {
    if let Some(done) = cache.get(term) {
        return Ok(done.clone());
    }

    let done = zonk(context, term)?;
    cache.insert(term.clone(), done.clone());
    Ok(done)
}

/// Obligation **(T)**: everything a type position reaches must be total.
///
/// A divergent type breaks type formation, and — through `rec Bad : Type = Sink(Bad)` — ties the negative knot strict positivity exists to forbid without ever writing an `induct`. Runs post-zonk, after [`record_totality`], whose flags it reads.
pub fn check_type_totality(
    context: &mut Context,
    module: &Module,
    inherited: &BTreeMap<Global, Totality>,
    cache: &mut Zonked,
) -> Result<(), Error> {
    curios_profile::profile!("check_type_totality");
    let mut positions = type_positions(module);
    positions.extend(checked_type_positions(context, cache)?);
    report(context, module, &positions, inherited, Erased::Type)
}

/// The terms elaboration settled *at a sort* — a term whose type is `Type` or `Prop` is itself a type, wherever it was written.
///
/// This is the half [`type_positions`] structurally cannot reach. That walk is a syntactic reading of where types are *written*: an annotation, a motive, a declaration telescope, a definition whose type ends in a sort. A type passed as an *argument* is written nowhere it looks — `annotations` has no case for an application's parameters — so `ignore(@Shape(inf), 5)` handed a partial value to a type-level function with nothing seeded, while the same type in an annotation was rejected.
///
/// The two seedings are **incomparable**, not nested, which a count of each obscured: the walk sees definition bodies whose type ends in a sort, and no typing judgment classifies those as type positions; this sees type arguments, which no syntactic walk can find without re-deriving the head's telescope — the re-derivation that cost (V) six defects. Taking the union costs one pass over already-recorded terms and needs neither.
fn checked_type_positions(
    context: &mut Context,
    cache: &mut Zonked,
) -> Result<Vec<Position>, Error> {
    let settled = context.checked().to_vec();
    // One label per elaboration site, not per position: `Context::record_checked` already stores an `Rc<str>` site, and many recorded terms share one.
    let mut sites: HashMap<Rc<str>, Rc<str>> = HashMap::new();
    let mut positions = Vec::new();
    curios_profile::sample!("totality::checked_entries", settled.len());

    for (term, type_, site) in settled {
        // Once per distinct recorded type rather than once per recorded term — see `checked_proof_positions`, which memoizes the same call for the same reason. This walk reads `checked` rather than draining it, because (V)'s seeding still needs it.
        let type_ = zonked(context, cache, &type_)?;
        // The term *is* a type exactly when its own type is a sort. An instance head is a variable or a projection, never a sort, so a wrapped type is not one.
        let sort = matches!(&*type_, Subterm::Type(_) | Subterm::Prop);
        if sort {
            positions.push(Position {
                term: zonked(context, cache, &term)?,
                site: Rc::clone(
                    sites
                        .entry(site.clone())
                        .or_insert_with(|| Rc::from(format!("a type in {site}"))),
                ),
            });
        }
    }

    curios_profile::sample!("totality::type_distinct_raw", cache.len());
    curios_profile::sample!("totality::type_sites", sites.len());
    curios_profile::sample!("totality::checked_type_positions", positions.len());
    Ok(positions)
}

/// Obligation **(V)**: everything a `Prop`-checked term reaches must be total.
///
/// A divergent proof proves anything, and erasure deletes it — so an `exit` behind a proposition never fires and the program continues with a forged invariant. Independent of [`check_type_totality`]: neither obligation subsumes the other.
pub fn check_proof_totality(
    context: &mut Context,
    module: &Module,
    inherited: &BTreeMap<Global, Totality>,
    cache: &mut Zonked,
) -> Result<(), Error> {
    curios_profile::profile!("check_proof_totality");
    let positions = checked_proof_positions(context, cache)?;
    report(context, module, &positions, inherited, Erased::Proof)
}

/// Obligation (V)'s seed: every term elaboration settled at a `Prop`-sorted type.
///
/// Drains what [`Context::record_checked`] collected and keeps the propositions. This is the whole of (V)'s seeding. There is no walk, because there is nothing to look for — elaboration already decided, for every term in the program, what type that term has, and the only thing a later pass can do with that question is re-derive it, incompletely.
///
/// Two properties follow from the seed rather than from care. **Coverage** is a consequence of elaboration being a typechecker: a position it never settles is a position the program was never typed at. And the **prelude boundary** is a consequence of replay — the archived prefix is defined into the context rather than elaborated, so it settles nothing and seeds nothing, and its verdicts arrive through [`recorded_totality`] instead.
///
/// Sort-hood is decided here rather than at the hook because a type may still carry unsolved metavariables while elaborating. Post-zonk every solution is materialized, so [`is_prop`](crate::is_prop) is asked once per *distinct* type, and hash-consing keeps that set far smaller than the term count.
fn checked_proof_positions(
    context: &mut Context,
    cache: &mut Zonked,
) -> Result<Vec<Position>, Error> {
    let checked = context.take_checked();
    // One label per elaboration site, not per position: `Context::record_checked` already stores an `Rc<str>` site, and many recorded terms share one.
    let mut sites: HashMap<Rc<str>, Rc<str>> = HashMap::new();
    let mut memo: HashMap<Term, bool> = HashMap::new();
    let mut positions = Vec::new();

    for (term, type_, site) in checked {
        // Two memos, keyed on the two different terms, because they save two different things. `memo` answers `is_prop` once per *zonked* type, as the doc above says. `zonked` answers the zonk once per *raw* type, which is the coarser question and the one that was being re-asked: a recorded type is the type elaboration wrote down, and thousands of terms are checked against the same one — 185,271 entries over the prelude, measured before this memo existed. Keying the outer memo on the raw type rather than folding both into one map is what keeps `is_prop`'s dedup exactly as wide as it was — raw types that differ but zonk alike still share a verdict.
        //
        // Sound because zonk is a pure function of the solution set, and the solution set is final here: `zonk_module` has run, and `is_prop` sees only zonked — hence metavariable-free — types, so it cannot solve anything that would make an earlier answer stale.
        let type_ = zonked(context, cache, &type_)?;
        let prop = match memo.get(&type_) {
            Some(prop) => *prop,
            None => {
                let prop =
                    curios_profile::profile_span!("totality::is_prop", is_prop(context, &type_))?;
                memo.insert(type_, prop);
                prop
            }
        };
        if prop {
            positions.push(Position {
                term: zonked(context, cache, &term)?,
                site: Rc::clone(
                    sites
                        .entry(site.clone())
                        .or_insert_with(|| Rc::from(format!("a proof in {site}"))),
                ),
            });
        }
    }

    curios_profile::sample!("totality::proof_distinct_raw", cache.len());
    curios_profile::sample!("totality::proof_distinct_zonked", memo.len());
    curios_profile::sample!("totality::proof_sites", sites.len());
    curios_profile::sample!("totality::checked_proof_positions", positions.len());
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
/// A member of a partial group may not have a sort in an *extractable* position of its type: its codomain after peeling arrows, or any component reachable by projection. Parameter positions are exempt, so an ordinary partial polymorphic program keeping `@A : Type` is untouched.
///
/// This exists because `rec Bad : Type = (Bad) -> False` overflows the compiler's stack while elaborating its first *use* — reducing `Bad` rebuilds an arrow whose domain is `Bad` — which is long before any whole-module gate runs. Without it the compiler aborts where it should diagnose. The closure form is what makes the language sound; this is what makes it answer.
pub fn check_rec_totality(
    context: &mut Context,
    group: &RecGroup,
    names: &[String],
) -> Result<(), Error> {
    // `RecGroup::member_type` rather than the member's scope body: the body carries loose indices where it names the group, and the question is now asked of a term that reduces. It is also the spelling `curios-cert`'s own gate asks about, so the two checkers put the same question to the same term.
    let mut extractable = Vec::new();
    for index in 0..group.length() {
        if yields_a_sort(context, &group.member_type(index)) {
            extractable.push(index);
        }
    }

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

/// Per-node verdicts for [`locally_partial`], carried across the terms one pass classifies.
///
/// Keyed on the term, whose hash is cached on the node and whose equality is already a worklist walk, so a lookup does not re-traverse what it is looking up. See [`checked_proof_positions`] for the same key and the same caveat.
type LocalMemo = HashMap<Term, bool>;

/// Whether `term` contains an `Intrinsic::ProcExit` or a `rec` group that does not descend — the "is this term partial on its own account" test both obligations apply one level below a definition.
///
/// **Iterative and memoized, and both are load-bearing.** (V) seeds one position per link of a `Str` literal's UTF-8 derivation, and those links share their tails, so the native per-node recursion this replaces cost one stack frame per byte *and* re-walked the shared tail once per position — quadratic in the literal's length. Measured on a 640-byte literal, that was 2.0s of a 2.1s compile, and a 10KiB literal overflowed the stack outright. Depth is not steps, so the reduction budget cannot bound either one.
///
/// The memo is what makes the sharing pay: hash-consing gives the tails one node, so each distinct node is classified once however many positions reach it. Per-position answers are unchanged — the cache records each node's own verdict, not whether some earlier position already reported it.
fn locally_partial(context: &mut Context, term: &Term, memo: &mut LocalMemo) -> bool {
    // Post-order over the term's DAG on the shared `Term::walk` driver: a memo hit settles at enter, and the exit combine sees every child's verdict.
    let mut state = (context, memo);
    term.walk(
        &mut state,
        |state, term| match state.1.get(term) {
            Some(&partial) => Enter::Skip(partial),
            None => Enter::Descend,
        },
        |state, term, mut children| {
            let mut partial = matches!(&**term, Subterm::Intrinsic(Intrinsic::ProcExit(..)));
            if let Subterm::Rec(Rec { group, .. }) = &**term {
                partial = partial || group_totality(state.0, group) == Totality::Partial;
            }
            let partial = partial || children.any(|child| child);
            state.1.insert(term.clone(), partial);
            partial
        },
    )
}

/// Whether this definition is partial on its own account: it mentions an exit, or it contains a local `rec` group that does not descend.
fn definition_is_locally_partial(
    context: &mut Context,
    definition: &Definition,
    memo: &mut LocalMemo,
) -> bool {
    locally_partial(context, &definition.body, memo)
        || locally_partial(context, &definition.type_, memo)
}
