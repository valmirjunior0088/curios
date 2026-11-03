//! Scaffolding other crates' suites build Core fixtures with.
//!
//! A namespace rather than a root export, for `curios-native`'s `test_support` reason: a call through `curios_core::test_support` says at its use site that the caller reached for scaffolding rather than product API. The path is the warning label.
//!
//! **Behind `test-support`, not `#[cfg(test)]`.** The callers are other crates' suites, and that cfg is set only while *this* crate is its own test harness — so a `cfg(test)` item would be invisible to them. The gate is also what keeps a shape no compiler stage produces out of every build that ships.

use {
    crate::*,
    curios_common::Qualifier,
    std::{
        collections::{BTreeMap, BTreeSet, HashSet},
        rc::Rc,
    },
};

/// A group assembled from scopes already closed, holding nothing between their arity and the member count — the shape [`RecGroup::closing`] exists so that no production caller can build.
///
/// For a fixture that forges what a restored archive could carry and puts it to the seam that reads it, or to the walk that refuses it. Nothing a compiler stage produces reaches here, which is why it is behind this gate rather than beside the constructor.
pub fn rec_group_of_scopes(members: Vec<RecMemberScopes>) -> RecGroup {
    RecGroup::from_scopes(members)
}

/// A match node at an ambient goal — the expected type as it stands where the match is written, inhabited by each arm under that case's specialization, over any scrutinee; see [`MatchResult::Ambient`]. The elaborator builds one where it decides the result, so the node whole is a fixture's to ask for.
pub fn match_ambient(head: impl Into<Term>, goal: impl Into<Term>, cases: Cases) -> Term {
    Term::from(Subterm::Match(Match {
        head: head.into(),
        result: MatchResult::Ambient(goal.into()),
        cases,
    }))
}

/// The formers stored at one place, each after the formers of the families its entry reaches, as the order a unit is stored in has them: a walk files what it computes of a family as it accepts it, and reads the next family's entry by what it filed. Two that reach each other keep the order they came in.
fn after_what_each_reaches(mut pending: Vec<(Global, BTreeSet<Global>, Item)>) -> Vec<Item> {
    let mut stored = Vec::with_capacity(pending.len());
    while !pending.is_empty() {
        let next = (0..pending.len())
            .find(|&at| {
                pending
                    .iter()
                    .enumerate()
                    .all(|(other, (name, ..))| other == at || !pending[at].1.contains(name))
            })
            .unwrap_or(0);
        stored.push(pending.remove(next).2);
    }

    stored
}

/// `module` declaring each of `inducts`, `structs` and `concepts`: carried by the definition of its name where the module holds one, and where it holds none by the former spelled from it ([`Definition::former`]), a family's in a group of its own as the lowering writes one, stored as a module stores an item ([`Module::ordered`]): after the items its entry names, so ahead of the module's own or just past the last of them it reaches. A concept takes the record `structs` holds under its name.
///
/// How a fixture that names a registry entry builds its module: an entry is carried by a former and by nothing else, so a fixture that wants one and no former of its own is handed the one the entry spells.
pub fn declaring(
    module: Module,
    inducts: impl IntoIterator<Item = (Global, InductDecl)>,
    structs: impl IntoIterator<Item = (Global, StructDecl)>,
    concepts: impl IntoIterator<Item = (Global, ConceptDecl)>,
) -> Module {
    let mut records = structs.into_iter().collect::<BTreeMap<_, _>>();
    let mut entries = BTreeMap::new();
    for (name, concept) in concepts {
        let record = records
            .remove(&name)
            .expect("a concept is declared with the record its witnesses are values of");
        entries.insert(name, Nominal::Concept { concept, record });
    }
    entries.extend(
        records
            .into_iter()
            .map(|(name, record)| (name, Nominal::Struct(record))),
    );
    for (name, declaration) in inducts {
        let earlier = entries.insert(name, Nominal::Inductive(declaration));
        assert!(
            earlier.is_none(),
            "{} is declared by one entry",
            name.faithful()
        );
    }

    let carried = |name: &Global, kind: &mut DefinitionKind, entries: &mut BTreeMap<_, _>| {
        if let Some(nominal) = entries.remove(name) {
            *kind = DefinitionKind::Nominal(Rc::new(nominal));
        }
    };
    let mut items = module.items.to_vec();
    for item in &mut items {
        match item {
            Item::Let(definition) => carried(&definition.name, &mut definition.kind, &mut entries),
            Item::Rec(rec) => {
                for member in &mut rec.definitions {
                    carried(&member.name, &mut member.kind, &mut entries);
                }
            }
        }
    }
    // Each former by the place it is stored ahead of: a walk takes the items as they come, and an entry may name a definition the module holds.
    let mut formers: BTreeMap<usize, Vec<(Global, BTreeSet<Global>, Item)>> = BTreeMap::new();
    for (name, nominal) in entries {
        let reach = nominal.reaches();
        let place = items
            .iter()
            .rposition(|item| {
                item.declared_names()
                    .into_iter()
                    .any(|declared| reach.contains(declared))
            })
            .map_or(0, |last| last + 1);
        let grouped = matches!(nominal, Nominal::Inductive(_));
        let island = name
            .qualifier()
            .map(Qualifier::without_last)
            .unwrap_or_default();
        let former = Definition::former(name, island, nominal);

        let item = match grouped {
            true => Item::Rec(RecItem::new(vec![(
                fresh(name.qualifier().map(Qualifier::last)),
                former,
            )])),
            false => Item::Let(Box::new(former)),
        };
        formers.entry(place).or_default().push((name, reach, item));
    }
    let mut stored = Vec::new();
    for (place, item) in items.into_iter().enumerate() {
        stored.extend(after_what_each_reaches(
            formers.remove(&place).unwrap_or_default(),
        ));
        stored.push(item);
    }
    stored.extend(formers.into_values().flat_map(after_what_each_reaches));

    Module {
        items: stored.into(),
        ..module
    }
}

/// A binder no fixture spells: the reduction suites name their symbols by small indices, so one minted far above them aliases none, and a process-wide counter keeps two openings in one test apart.
pub fn fresh(hint: Option<&str>) -> Free {
    static MINTED: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1_000_000);

    Free::local(
        MINTED.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        hint,
    )
}

/// How a suite spells one of the comparison intrinsics, paired at its use site with the symbol to name it by: the grids that hold a verdict to the carrier's values range over the four relations, and a bare function type among them reads as the shape of the roster rather than as one relation.
pub type Relation = fn(Term, Term) -> Intrinsic;

/// A reducer that folds intrinsics all the way down, for the gates that must *evaluate* a rebuilt term rather than inspect its shape. `reduce_intrinsic` already reduces its own operands through this seam, so one pass suffices for the terms the suites build.
pub struct Folding;

impl Reducer for Folding {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        match &*term {
            Subterm::Intrinsic(intrinsic) => Ok(reduce_intrinsic(self, intrinsic)?.into()),
            _ => Ok(term),
        }
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        self.reduce(term)
    }

    /// Unbudgeted: these gates are about what a fold *decides*, and a limit would only decide it a second time.
    fn spend(&mut self, _cost: Cost) -> Result<(), ReduceError> {
        Ok(())
    }

    fn fresh_binder(&mut self, hint: Option<&str>) -> Free {
        fresh(hint)
    }
}

/// The names a lowering filed as resolving to nothing, as the refusal they are: each rendered as elaboration reports it, a line apart, declaration by declaration in the order of their names, the entry's last. `Ok` where it filed none.
pub fn unresolved(minted: &Minted) -> Result<(), String> {
    let mut seen = HashSet::new();
    let filed = minted
        .each()
        .map(|(_, mints)| mints)
        .filter(|mints| seen.insert(std::ptr::from_ref(*mints)))
        .chain([&minted.entry])
        .flat_map(|mints| mints.unbound.values())
        .map(ToString::to_string)
        .collect::<Vec<_>>();

    match filed.is_empty() {
        true => Ok(()),
        false => Err(filed.join("\n")),
    }
}
