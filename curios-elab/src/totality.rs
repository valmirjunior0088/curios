//! The elaborator's driver for the shared size-change totality analysis.
//!
//! The analysis itself lives in `curios-analysis/src/totality.rs` (see that module for the size-change principle and its rationale) and is run by both checkers; what belongs here is the driving, a declaration at a time as each is published: classifying its definitions against the group verdicts, with diverging host calls and the partiality of what they mention folded in, and seeding the two erased-half obligations — (T) from its written type positions, (V) from the proof positions its elaboration recorded.
//!
//! Rejection is a **classification, not an error**. `/std/Json/decode`'s parsers are nullary and productive; none passes this check and none needs to. They stay usable everywhere erasure keeps them. Only [`crate::check_type_totality`] and [`crate::check_proof_totality`] turn a `Partial` classification into a rejection, and only for the positions that erase.

mod reach;
use reach::*;

#[cfg(test)]
mod tests;

use {
    super::{Context, ErasedSite, Error, ZonkPass, proof_position, reduce, zonk_solved_term_metas},
    curios_analysis::{Erased, blocker, group_totality, known_to_terminate, yields_a_sort_where},
    curios_core::{
        Definition, Definitions, Enter, Entrypoint, Extent, Free, Global, Item, MetavarId, Rec,
        Sharing, Subterm, Term, Totality,
    },
    std::{
        collections::{BTreeSet, HashMap, HashSet},
        ops::ControlFlow,
        rc::Rc,
    },
};

/// Classify one definition against the verdicts already recorded, and record it, by the rule `classify_totality` states, while its declaration is still elaborating: what the rest of that declaration reads it by, so a written type or a bound that names it is refused by name before it is reduced. The verdict every other declaration reads is the one `classify_totality` records as the item is published.
///
/// Everything a definition mentions outside its own group has finished, so it is already classified and a single pass suffices. A name with no verdict is a member of the group currently elaborating, which [`group_totality`] settles for the group as a whole.
pub fn record_definition_totality(
    context: &mut Context,
    definition: &Definition,
    group: Totality,
) -> Result<(), Error> {
    let partial = !group.is_total()
        || definition_is_locally_partial(context, definition, &mut LocalMemo::new())?
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
    Ok(())
}

/// Where `term` names `name`: the first occurrence that kept a position, outermost first, each node of its graph visited once, and the term's own position where no occurrence kept one. An occurrence at universe levels is its instance, which carries the position the name was written at.
///
/// Where a totality refusal is placed: at the name it blames, which the reader can change, rather than at the whole of the position that reaches it.
fn occurrence(term: &Term, name: &Global) -> Option<Extent> {
    let found = term.try_walk(
        &mut HashSet::new(),
        |seen: &mut HashSet<Term>, term| {
            let named = match &**term {
                Subterm::Instance(instance) => Some(instance.name),
                _ => None,
            };
            match (named, term.extent()) {
                (Some(named), Some(extent)) if named == *name => ControlFlow::Break(extent),
                _ if seen.insert(term.clone()) => ControlFlow::Continue(Enter::Descend),
                _ => ControlFlow::Continue(Enter::Skip(())),
            }
        },
        |_, _, _| (),
    );

    match found {
        ControlFlow::Break(extent) => Some(extent),
        ControlFlow::Continue(()) => term.extent(),
    }
}

/// The first declaration `term` references whose recorded totality is not total, first as written ([`Term::first_declaration`]) — the syntactic reading behind [`check_written_type_totality`], shared with the bound site that re-reports an exhausted discharge by the same name.
fn partial_offender(context: &Context, term: &Term) -> Option<Global> {
    term.first_declaration(|name| {
        context
            .definition_totality(name)
            .is_some_and(|totality| !totality.is_total())
    })
}

/// An exhausted discharge of `proposition`, re-reported by the partial definition it names when it names one, and the exhaustion itself otherwise. A bound whose subject does not terminate would otherwise spend the budget and report that, where the same subject in a declared type is refused by name before anything is reduced; the check still runs — a subject that terminates discharges, whatever the analysis classified it — and only a spent budget is re-read for a name. The obligation is `documentation/design/soundness/totality-of-the-erased-program.md`'s.
pub fn exhausted_bound(
    context: &Context,
    error: Error,
    proposition: &Term,
    site: ErasedSite,
) -> Error {
    match (&error, partial_offender(context, proposition)) {
        (Error::ReduceExhausted { .. } | Error::ConvertExhausted { .. }, Some(offender)) => {
            Error::PartialInErasedPosition {
                erased: Erased::Proof,
                site,
                offender: Some(offender),
            }
        }
        _ => error,
    }
}

/// Obligation (T), at the one point a non-productive type-level loop can still be diagnosed: before the type is reduced.
///
/// The whole-module gate runs post-zonk, which is after elaboration has already performed the type-level reduction it exists to make safe. A type that reaches a partial definition and *is productive* survives elaboration and the gate rejects it; one that is not productive spins until the step budget dies, and the program is refused for apparently running out of a resource no amount of which would have helped. This refuses it first, by name, for the reason it is actually wrong.
///
/// It reads the *lowered* type, before elaboration touches it, so the mentions it sees are the written ones. That is why it is an early net rather than a replacement: the post-zonk gate still runs, and still sees everything elaboration produces.
pub fn check_written_type_totality(
    context: &mut Context,
    type_: &Term,
    site: ErasedSite,
) -> Result<(), Error> {
    match partial_offender(context, type_) {
        None => Ok(()),
        Some(offender) => Err(Error::PartialInErasedPosition {
            erased: Erased::Type,
            site,
            offender: Some(offender),
        }
        .at_opt(occurrence(type_, &offender))),
    }
}

/// Classify a published item and record each of its definitions' verdict: the one every other declaration reads, and the one the unit files ([`crate::Classified::totality`]).
///
/// Group rejection is only the local part. A definition is also `Partial` if it calls a host row that diverges — which erasure drops, so an exit behind a nullary proof never fires — or if it mentions anything already `Partial`. That last clause is a transitive closure, and it is what makes the flag a usable summary across declarations and across units: "this definition is partial" means something partial is in its closure, so a proof mentioning it inherits the same.
///
/// Runs as the item is published, where its terms are final and meta-free, so no metavariable can be solved to a partial term after the fact. Everything the item mentions outside itself has finished, a declaration elaborating after what it reads, so its verdict is recorded and is a closure already, and the walk stops there: at a predecessor unit's definition, which is why the standard library is not analyzed again on every compilation, and at an earlier declaration's of this unit alike. Among the members of one group the closure is iterated, since each may mention the others. A name with no verdict is treated as total: every `Global` a published item mentions is defined by a finished declaration or by the item itself, so that case is unreachable rather than an approximation.
pub fn classify_totality(context: &mut Context, item: &Item) -> Result<(), Error> {
    curios_profile::profile!("record_totality");
    // A group that does not descend makes every member partial, whatever each body looks like on its own.
    let rejected = match item {
        Item::Let(_) => false,
        Item::Rec(rec) => group_totality(context, &rec.group)? == Totality::Partial,
    };
    let definitions = item.definitions();
    let own = definitions
        .iter()
        .map(|definition| definition.name)
        .collect::<BTreeSet<_>>();
    // One cache for the item: its definitions share subterms, and a node classified for one is classified for all.
    let mut memo = LocalMemo::new();
    let mut partial = BTreeSet::new();
    let mut mentions = Vec::with_capacity(definitions.len());
    for definition in &definitions {
        let named = definition.mentions();
        let reaches_partial = named.iter().any(|name| {
            !own.contains(name)
                && context
                    .definition_totality(name)
                    .is_some_and(|totality| !totality.is_total())
        });
        if rejected
            || reaches_partial
            || definition_is_locally_partial(context, definition, &mut memo)?
        {
            partial.insert(definition.name);
        }
        mentions.push((definition.name, named));
    }
    loop {
        let before = partial.len();
        for (name, named) in &mentions {
            if named.iter().any(|other| partial.contains(other)) {
                partial.insert(*name);
            }
        }
        if partial.len() == before {
            break;
        }
    }

    let of = |name: &Global| match partial.contains(name) {
        true => Totality::Partial,
        false => Totality::Total,
    };
    for name in &own {
        context.record_definition_totality(name, of(name));
    }

    Ok(())
}

/// Every way the given positions fail to be total, with the site of each and where it was written, for a position that kept one.
///
/// Two ways, and a position can fail either: it names a definition classified `Partial`, or it *is* partial with no name to blame — an inline `rec` that does not descend, or a call to a host row that diverges. A verdict is a closure already ([`classify_totality`]), so what a position reaches through a total definition is total, and the definitions it names are all there is to ask.
fn faults(
    context: &mut Context,
    positions: &[Position],
) -> Result<Vec<(ErasedSite, Fault, Option<Extent>)>, Error> {
    curios_profile::sample!("totality::faults_positions", positions.len());
    let named = seeds(positions)
        .into_iter()
        .filter(|name| {
            context
                .definition_totality(name)
                .is_some_and(|totality| !totality.is_total())
        })
        .collect::<Vec<_>>();

    let mut faults = Vec::new();
    // One cache across every position: (V) seeds a literal's derivation once per link, and those links share their tails. It is keyed by the positions' canonical nodes, so a type zonked apart from an equal one is that one's key and is found by identity.
    let keys = Sharing::new();
    let mut memo = LocalMemo::new();
    for position in positions {
        if locally_partial(context, &keys.share(&position.term), &mut memo)? {
            faults.push((
                ErasedSite::clone(&position.site),
                Fault::Inline,
                position.term.extent(),
            ));
        }
    }
    // One named fault per offending definition, attributed to the first position that names it.
    for name in named {
        let position = positions
            .iter()
            .find(|position| position.term.mentions_declaration(&name))
            .expect("a seed is a name some position writes");
        faults.push((
            ErasedSite::clone(&position.site),
            Fault::Named(name),
            occurrence(&position.term, &name),
        ));
    }
    Ok(faults)
}

/// Every term either erased-half obligation has zonked, shared across both of them and across both the types they test and the terms they keep.
///
/// Not an optimization detail but a contract. (T) and (V) are seeded from the *same* [`Context::record_checked`](crate::Context) entries, and each needs the zonked type of every entry plus the zonked term of every entry it keeps — so a cache local to either does the same work twice, and hash-consing collapses the recorded entries to far fewer distinct types and terms.
///
/// One map serves types and terms alike because zonk is a *function of its argument*: a term appearing in both populations has one zonked form, and sharing the entry is correct rather than merely convenient. Sound to share across the two obligations of one declaration, or of the entry, for the reason either could memoize alone — the solution set is final once what they read has been published, or the module zonked.
///
/// The cost is memory: this is held across both obligations rather than dropped between them. That is the trade; the cache is not free, it is cheaper than the work it removes.
///
/// It holds one zonk pass besides, so a node the recorded terms share is zonked once across them all: a type nested in another is a recorded type of its own, and zonked apart each would walk the ones inside it again.
#[derive(Default)]
pub struct Zonked {
    done: HashMap<Term, Term>,
    pass: ZonkPass,
    /// Whether each zonked type reduces to a sort, asked once a type: many recorded terms share one.
    sorts: HashMap<Term, bool>,
}

/// `term` zonked, through `cache`: the two types and two terms the two obligations zonk.
fn zonked(context: &Context, cache: &mut Zonked, term: &Term) -> Result<Term, Error> {
    if let Some(done) = cache.done.get(term) {
        return Ok(done.clone());
    }

    let done = cache.pass.zonk(context, term)?;
    cache.done.insert(term.clone(), done.clone());
    Ok(done)
}

/// Obligation **(T)**: everything a type position reaches must be total.
///
/// A divergent type breaks type formation, and — through `rec Bad : Type = Sink(Bad)` — ties the negative knot strict positivity exists to forbid without ever writing an `induct`. Decided for one declaration as it is published, over `module` holding its item and the entries it declares, and for the entry once the module is zonked; either way after [`classify_totality`], whose verdicts it reads, and over the terms recorded since `mark`.
pub fn check_type_totality(
    context: &mut Context,
    definitions: &Definitions,
    entry: Option<&Entrypoint>,
    mark: usize,
    cache: &mut Zonked,
) -> Result<(), Error> {
    curios_profile::profile!("check_type_totality");
    let mut positions = type_positions(definitions, entry);
    positions.extend(checked_type_positions(context, mark, cache)?);
    report(context, &positions, Erased::Type)
}

/// The terms elaboration settled *at a sort* — a term whose type is `Type` or `Prop` is itself a type, wherever it was written.
///
/// This is the half [`type_positions`] structurally cannot reach. That walk is a syntactic reading of where types are *written*: an annotation, a motive, a declaration telescope, a definition whose type ends in a sort. A type passed as an *argument* is written nowhere it looks — `annotations` has no case for an application's parameters — so `ignore(@Shape(inf), 5)` would hand a partial value to a type-level function with nothing seeded, while the same type in an annotation is rejected.
///
/// The two seedings are **incomparable**, not nested: the walk sees definition bodies whose type ends in a sort, and no typing judgment classifies those as type positions; this sees type arguments, which no syntactic walk can find without re-deriving the head's telescope. Taking the union costs one pass over already-recorded terms and needs neither.
fn checked_type_positions(
    context: &mut Context,
    mark: usize,
    cache: &mut Zonked,
) -> Result<Vec<Position>, Error> {
    let settled = context.checked()[mark..].to_vec();
    let labels = context.checked_labels(mark);
    // One site per definition, not per position: many recorded terms were settled in one.
    let mut sites: HashMap<Option<Global>, Rc<ErasedSite>> = HashMap::new();
    let mut positions = Vec::new();
    curios_profile::sample!("totality::checked_entries", settled.len());

    for (term, type_, site) in settled {
        // Once per distinct recorded type rather than once per recorded term — see `checked_proof_positions`, which memoizes the same call for the same reason. This walk reads `checked` rather than draining it, because (V)'s seeding still needs it.
        let type_ = zonked(context, cache, &type_)?;
        // The term *is* a type exactly when its own type is a sort. An instance head is a variable or a projection, never a sort, so a wrapped type is not one.
        let sort = reduces_to_a_sort(context, cache, &type_);
        if sort {
            positions.push(Position {
                term: by_value(&zonked(context, cache, &term)?, &labels),
                site: Rc::clone(sites.entry(site).or_insert_with(|| {
                    Rc::new(ErasedSite::Within {
                        definition: site,
                        settled: Some("a type"),
                    })
                })),
            });
        }
    }

    curios_profile::sample!("totality::type_distinct_raw", cache.done.len());
    curios_profile::sample!("totality::type_sites", sites.len());
    curios_profile::sample!("totality::checked_type_positions", positions.len());
    Ok(positions)
}

/// Whether `type_` is a sort, as written or once it reduces: a type a definition computes, `U(j + 1)` where `U` matches on its argument, is one the certifier reads as a sort, which reading the written form alone misses. A type that does not reduce within the budget is read as none, and the certifier, which reduces it again, refuses what it then finds.
fn reduces_to_a_sort(context: &mut Context, cache: &mut Zonked, type_: &Term) -> bool {
    if matches!(&**type_, Subterm::Sort(_)) {
        return true;
    }
    if let Some(&sort) = cache.sorts.get(type_) {
        return sort;
    }
    let sort =
        reduce(context, type_.clone()).is_ok_and(|reduced| matches!(&*reduced, Subterm::Sort(_)));
    cache.sorts.insert(type_.clone(), sort);
    sort
}

/// `term` with each local binding it names read as what the binding stands for: what a position reaches is what the certifier reads it by, which a label hides — a member of a local group stands for the group, and a `let`'s binding for whatever its value reaches. The values name no binding, each read by value where its block ended, so one substitution reads them all.
fn by_value(term: &Term, labels: &[(Free, Term)]) -> Term {
    match term.has_local_free() {
        true => term.substitute(labels),
        false => term.clone(),
    }
}

/// Obligation **(V)**: everything a `Prop`-checked term reaches must be total.
///
/// A divergent proof proves anything, and erasure deletes it — so an `exit` behind a proposition never fires and the program continues with a forged invariant. Independent of [`check_type_totality`]: neither obligation subsumes the other. Decided where that one is, over the terms recorded since `mark`, which it takes.
pub fn check_proof_totality(
    context: &mut Context,
    mark: usize,
    cache: &mut Zonked,
) -> Result<(), Error> {
    curios_profile::profile!("check_proof_totality");
    let positions = checked_proof_positions(context, mark, cache)?;
    report(context, &positions, Erased::Proof)
}

/// Obligation (V)'s seed: every term elaboration settled at a `Prop`-sorted type.
///
/// Takes what [`Context::record_checked`] collected since `mark` and keeps the propositions. This is the whole of (V)'s seeding. There is no walk, because there is nothing to look for — elaboration already decided, for every term in the program, what type that term has, and the only thing a later pass can do with that question is re-derive it, incompletely.
///
/// Two properties follow from the seed rather than from care. **Coverage** is a consequence of elaboration being a typechecker: a position it never settles is a position the program was never typed at. And the **prelude boundary** is a consequence of replay — the archived prefix is defined into the context rather than elaborated, so it settles nothing and seeds nothing, and its verdicts arrive in what its unit filed of them instead ([`classify_totality`]).
///
/// Sort-hood is decided here rather than at the hook because a type may still carry unsolved metavariables while elaborating. Once the declaration is published every solution is materialized, so [`proof_position`] is asked once per *distinct* type, and hash-consing keeps that set far smaller than the term count.
fn checked_proof_positions(
    context: &mut Context,
    mark: usize,
    cache: &mut Zonked,
) -> Result<Vec<Position>, Error> {
    let labels = context.checked_labels(mark);
    let checked = context.split_checked(mark);
    // One site per definition and kind of proof, not per position: many recorded terms were settled in one.
    let mut sites: HashMap<(Option<Global>, &'static str), Rc<ErasedSite>> = HashMap::new();
    let mut memo: HashMap<Term, Option<&'static str>> = HashMap::new();
    let mut positions = Vec::new();

    for (term, type_, site) in checked {
        // Two memos, keyed on the two different terms, because they save two different things. `memo` answers `proof_position` once per *zonked* type, as the doc above says. `zonked` answers the zonk once per *raw* type, which is the coarser question: a recorded type is the type elaboration wrote down, and thousands of terms are checked against the same one. Keying the outer memo on the raw type rather than folding both into one map keeps `proof_position`'s dedup as wide as the zonked types — raw types that differ but zonk alike still share a verdict.
        //
        // Sound because zonk is a pure function of the solution set, and the solution set is final here: what is read has been published, and `proof_position` sees only zonked — hence metavariable-free — types, so it cannot solve anything that would make an earlier answer stale.
        let type_ = zonked(context, cache, &type_)?;
        let held = match memo.get(&type_) {
            Some(held) => *held,
            None => {
                let held = curios_profile::profile!("totality::proof_position" => proof_position(context, &type_))?;
                memo.insert(type_, held);
                held
            }
        };
        if let Some(what) = held {
            positions.push(Position {
                term: by_value(&zonked(context, cache, &term)?, &labels),
                site: Rc::clone(sites.entry((site, what)).or_insert_with(|| {
                    Rc::new(ErasedSite::Within {
                        definition: site,
                        settled: Some(what),
                    })
                })),
            });
        }
    }

    curios_profile::sample!("totality::proof_distinct_raw", cache.done.len());
    curios_profile::sample!("totality::proof_distinct_zonked", memo.len());
    curios_profile::sample!("totality::proof_sites", sites.len());
    curios_profile::sample!("totality::checked_proof_positions", positions.len());
    Ok(positions)
}

fn report(context: &mut Context, positions: &[Position], erased: Erased) -> Result<(), Error> {
    match faults(context, positions)?.into_iter().next() {
        None => Ok(()),
        Some((site, fault, extent)) => Err(Error::PartialInErasedPosition {
            erased,
            site,
            offender: match fault {
                Fault::Named(name) => Some(name),
                Fault::Inline => None,
            },
        }
        .at_opt(extent)),
    }
}

/// A recursive group's members as its signatures and bodies read them, a turn at a time, as `curios-cert`'s `check_group` takes them: a member's name is defined as its slot, which reads through once the member's turn finds it known to terminate (`curios_analysis::known_to_terminate`) and stands for nothing until then ([`Context::is_withheld_rec_slot`]). A signature or a body reads a member written after it by waiting on the slot, and no reduction unfolds a member that may not terminate.
///
/// Obligation (T) is asked here too, in the one place it cannot wait for the post-zonk pass: a group with a member withheld may not have a sort in an *extractable* position of a member's type, its codomain after peeling arrows or any component reachable by projection. Parameter positions are exempt, so an ordinary partial polymorphic program keeping `@A : Type` is untouched. It exists because `let Bad: Type = (Bad) -> False;` unfolds without end where its first *use* reads it, long before any whole-module gate runs, and without it the compiler reports a spent budget where it should name the definition. The closure form is what makes the language sound; this is what makes it answer.
pub struct Turns {
    /// Each member's name, a local group's label or a top-level member's local, defined as its slot.
    names: Vec<Free>,
    /// Each member's signature over the names.
    types: Vec<Term>,
    slots: Vec<MetavarId>,
    /// Each body elaborated so far, as its turn materialized it.
    bodies: Vec<Option<Term>>,
    known: Vec<bool>,
}

impl Turns {
    pub fn new(names: Vec<Free>, types: Vec<Term>, slots: Vec<MetavarId>) -> Self {
        let members = names.len();
        Self {
            names,
            types,
            slots,
            bodies: vec![None; members],
            known: vec![false; members],
        }
    }

    /// Member `index`'s turn, `body` its body elaborated, before its slot is filled: filling the slot wakes what waits on it, which must find it released where it is. A member not known to terminate is withheld for good, and where a member's type yields a sort as it reads now, the group owes descent and is refused here, named by `site` for the members whose types do. A type that settles nothing yet is asked again once every turn is taken ([`Turns::close`]): a later body may settle a level a signature holds.
    pub fn take(
        &mut self,
        context: &mut Context,
        index: usize,
        body: &Term,
        site: &dyn Fn(&[usize]) -> ErasedSite,
    ) -> Result<(), Error> {
        self.bodies[index] = Some(zonk_solved_term_metas(context, body));
        if self.known_at_turn(context, &self.bodies, &self.known, index)? {
            self.known[index] = true;
            context.release_rec_slot(self.slots[index]);
            return Ok(());
        }

        self.refuse_where_owed(context, &|_| false, site)
    }

    /// Once every turn is taken and what the bodies parked is retried. Every turn is taken again over the bodies as they stand now: a solution found since a member's turn may have given its body a call, so the members withheld are the ones the certifier withholds over the terms this group closes to. With one withheld, the group owes descent where a member's type yields a sort, or waits on a member withheld, which may be a sort once that member's value is and never will be read.
    pub fn close(
        &self,
        context: &mut Context,
        site: &dyn Fn(&[usize]) -> ErasedSite,
    ) -> Result<(), Error> {
        let bodies = self
            .bodies
            .iter()
            .map(|body| {
                body.as_ref()
                    .map(|body| zonk_solved_term_metas(context, body))
            })
            .collect::<Vec<_>>();
        let mut known = vec![false; self.names.len()];
        for index in 0..self.names.len() {
            known[index] = self.known_at_turn(context, &bodies, &known, index)?;
        }
        let withheld = (0..self.names.len())
            .filter(|member| !known[*member])
            .map(|member| (self.names[member], self.slots[member]))
            .collect::<Vec<_>>();
        if withheld.is_empty() {
            return Ok(());
        }

        let names_withheld = |reduced: Option<&Term>| {
            reduced.is_none_or(|reduced| {
                blocker(reduced).is_some_and(|leaf| {
                    withheld.iter().any(|(name, slot)| match &**leaf {
                        Subterm::Metavar(metavar) => metavar.id == *slot,
                        _ => leaf.mentions_free(name),
                    })
                })
            })
        };
        self.refuse_where_owed(context, &names_withheld, site)
    }

    /// Whether member `index` is known to terminate at its turn, over `bodies` with `known` the members known before it.
    fn known_at_turn(
        &self,
        context: &mut Context,
        bodies: &[Option<Term>],
        known: &[bool],
        index: usize,
    ) -> Result<bool, Error> {
        let entering = (0..self.names.len())
            .filter(|member| known[*member] || *member == index)
            .filter_map(|member| {
                let body = bodies[member].clone()?;
                Some((self.names[member], self.types[member].clone(), body))
            })
            .collect();

        known_to_terminate(context, entering)
    }

    /// Refuse the group where a member's type yields a sort, `undetermined` saying of a codomain that settles nothing whether it may yet be one.
    fn refuse_where_owed(
        &self,
        context: &mut Context,
        undetermined: &dyn Fn(Option<&Term>) -> bool,
        site: &dyn Fn(&[usize]) -> ErasedSite,
    ) -> Result<(), Error> {
        let mut extractable = Vec::new();
        for (index, type_) in self.types.iter().enumerate() {
            if yields_a_sort_where(context, type_, undetermined)? {
                extractable.push(index);
            }
        }

        match extractable.is_empty() {
            true => Ok(()),
            false => Err(Error::PartialInErasedPosition {
                erased: Erased::Type,
                site: site(&extractable),
                offender: None,
            }),
        }
    }
}

/// Per-node verdicts for [`locally_partial`], carried across the terms one pass classifies.
///
/// Keyed on the term, whose hash is cached on the node and whose equality is already a worklist walk, so a lookup does not re-traverse what it is looking up. Where the terms classified were built apart — the positions' types, each zonked from its own recorded term — they are keyed by their canonical nodes, since two equal terms in two allocations are compared node by node at every probe that finds one.
type LocalMemo = HashMap<Term, bool>;

/// Whether `term` contains a call to a diverging host row or a `rec` group that does not descend — the "is this term partial on its own account" test both obligations apply one level below a definition.
///
/// **Iterative and memoized, and both are load-bearing.** (V) seeds one position per link of a `Str` literal's UTF-8 derivation, and those links share their tails, so a native per-node recursion would cost one stack frame per byte *and* re-walk the shared tail once per position — quadratic in the literal's length. Depth is not steps, so the reduction budget cannot bound either one.
///
/// The memo is what makes the sharing pay: hash-consing gives the tails one node, so each distinct node is classified once however many positions reach it. Per-position answers are unchanged — the cache records each node's own verdict, not whether some earlier position already reported it.
fn locally_partial(
    context: &mut Context,
    term: &Term,
    memo: &mut LocalMemo,
) -> Result<bool, Error> {
    // Post-order over the term's DAG on the shared `Term::try_walk` driver: a memo hit settles at enter, and the exit combine sees every child's verdict. A `rec` group is classified at enter, the one place the walk can stop, so the budget's refusal ends the walk rather than reading as a verdict; a group that does not descend settles its node there, and one that does leaves the node to its children.
    let mut state = (context, memo);
    let walked = term.try_walk(
        &mut state,
        |state, term| {
            if let Some(&partial) = state.1.get(term) {
                return ControlFlow::Continue(Enter::Skip(partial));
            }
            if let Subterm::Rec(Rec { group, .. }) = &**term {
                match group_totality(state.0, group) {
                    Err(spent) => return ControlFlow::Break(spent),
                    Ok(Totality::Partial) => {
                        state.1.insert(term.clone(), true);
                        return ControlFlow::Continue(Enter::Skip(true));
                    }
                    Ok(Totality::Total) => {}
                }
            }
            ControlFlow::Continue(Enter::Descend)
        },
        |state, term, mut children| {
            let partial = matches!(&**term, Subterm::Foreign(curios_core::Foreign { function, .. }) if function.diverges())
                || children.any(|child| child);
            state.1.insert(term.clone(), partial);
            partial
        },
    );
    match walked {
        ControlFlow::Continue(partial) => Ok(partial),
        ControlFlow::Break(spent) => Err(spent),
    }
}

/// Whether this definition is partial on its own account: it mentions an exit, or it contains a local `rec` group that does not descend.
fn definition_is_locally_partial(
    context: &mut Context,
    definition: &Definition,
    memo: &mut LocalMemo,
) -> Result<bool, Error> {
    Ok(locally_partial(context, &definition.body, memo)?
        || locally_partial(context, &definition.type_, memo)?)
}
