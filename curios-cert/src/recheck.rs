//! Running the independent kernel over a whole module.
//!
//! This is the seam where the second opinion is actually asked for. Everything upstream — elaboration, unification, zonking, witness resolution — has already decided the module is well-typed; [`recheck_definitions`] decides again from the terms alone, and it lives in this crate so that deciding is something `cargo tree -p curios-cert` can account for.
//!
//! # Reading a disagreement
//!
//! A refusal here is *not* automatically an elaborator bug, and treating it as one would be the wrong reflex. The kernel is deliberately incomplete in one place — conversion compares a stuck elimination's scrutinee, motive and arms, and a projection's or an instance's head, at `Type` rather than at their real types, forfeiting type-directed eta and goal-level irrelevance there — and that refuses valid programs. So a disagreement is a question, and the two answers are "the kernel needs strengthening here" and "the elaborator admitted something it should not have". Both are worth knowing, which is why this runs at all.
//!
//! What a disagreement is *never* is noise to be suppressed. If a rule here has to be weakened to make a real module pass, that weakening is a decision about the trusted base and belongs in a decision file of its own under `documentation/design/soundness/`, beside `an-independent-kernel-re-checks-what-the-elaborator-accepts.md`.
//!
//! # On the compile path
//!
//! Every compilation calls this. `curios-pipeline` certifies each unit it compiles — [`certify_definitions`] for a unit, [`certify_definitions_over`] for one compiled over a baseline, [`certify_program`] for the entry — with the units in scope as its [`Globals`], and fails the build on a refusal, judging the unit's own items while the scope's are taken on the word of the walk that certified each of them: a unit is certified by construction, and the prelude's word is `curios-prelude`'s build script, which runs the same walk over each root, against the roots before it, and fails the build on any refusal. An archive that exists is one whose every item the kernel accepted.
//!
//! What is *not* re-judged is the environment, and nothing of it is handed in again. Before anything is judged the walk refuses a module for every name it declares a second time — a name any namespace of the environment holds, whatever the module declares under it, and a second item under one name — whole, as [`Error::Redeclared`], because a name is one declaration and every pass below files by name. So every declaration a module carries is one this walk judges, or one whose verdict it adopts from a baseline. Nothing reads a length, and nothing requires the environment's units to sit anywhere in particular — which is the difference between an environment and a prefix.

#[cfg(test)]
mod adoption_tests;
#[cfg(test)]
mod assignment_tests;
#[cfg(test)]
mod budget_tests;
#[cfg(test)]
mod case_tests;
#[cfg(test)]
mod closed_computation_tests;
#[cfg(test)]
mod conversion_tests;
#[cfg(test)]
mod elimination_tests;
#[cfg(test)]
mod entry_tests;
#[cfg(test)]
mod equation_tests;
#[cfg(test)]
mod foreign_tests;
#[cfg(test)]
mod identity_tests;
#[cfg(test)]
mod implied_tests;
#[cfg(test)]
mod independence_tests;
#[cfg(test)]
mod intrinsic_tests;
#[cfg(test)]
mod occurrence_tests;
#[cfg(test)]
mod paradox_tests;
#[cfg(test)]
mod plicity_tests;
#[cfg(test)]
mod positivity_tests;
#[cfg(test)]
mod proposition_tests;
#[cfg(test)]
mod reads_tests;
#[cfg(test)]
mod recursion_tests;
#[cfg(test)]
mod registry_tests;
#[cfg(test)]
mod scope_tests;
#[cfg(test)]
mod shape_tests;
#[cfg(test)]
mod sort_tests;
#[cfg(test)]
mod standing_tests;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod totality_tests;
#[cfg(test)]
mod universes_tests;
#[cfg(test)]
mod unjudged_tests;
#[cfg(test)]
mod variance_tests;

use {
    super::{
        Adopting, Baseline, Error, Globals, Kernel, PositionVerdicts, Stated, Totals, Unvisited,
        bytes_of, carried_marks, check_case_counts, check_definition, check_entrypoint,
        check_induct_decl, check_positions, check_rec_group, check_struct_decl, define_as_stated,
        free_locals_of, held_digest, open_positions_of, open_quality, positivity_vectors,
        read_digest, rewrite_universe_levels, state_definition, state_rec_group, stated_entrypoint,
        stated_marks, universe_metas, universe_verdict, unstated_entry, variance_vectors,
    },
    curios_analysis::{Declarations, PositivityRefusal},
    curios_common::{Candidates, Facet, SyntaxRegistry, grown},
    curios_core::{
        Abroad, Bound, Certification, Certified, Changes, Concluded, Definition, Definitions,
        Entrypoint, Global, HeldReads, InductDecl, Item, ItemRun, Level, MetavarId, Notation,
        Places, Polarity, Program, ReadRecord, ReadTable, Reads, StructDecl, Term, Totality,
        Variance, Zonked,
    },
    std::{
        cell::{OnceCell, RefCell},
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        mem,
        rc::Rc,
    },
};

/// One refusal the kernel reached.
///
/// **Not one per item, and not always an item.** A declaration is judged several ways, so one failing two of them yields two verdicts; and a registry entry is refused under its own name, which is no item of [`Definitions::items`]. A caller counting these against `items.len()` is comparing two different rosters.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Verdict {
    /// What failed: an item, or the registry entry a declaration pass refused — a recursive group is named by its first member, since a group is checked and refused as a unit. `None` is the entrypoint expression, which has no name to export.
    pub name: Option<Global>,
    pub error: Error,
}

impl Verdict {
    /// The refusal as a reader is told it, written by `notation` ([`Error::rendered`]), and, where the declaration was refused at one assignment of the sorts it leaves to its caller, the type `definitions` holds under the verdict's name restated at that assignment.
    pub fn rendered(&self, definitions: &Definitions, notation: &dyn Notation) -> String {
        let stated = match (&self.error, self.name) {
            (Error::AtSorts { .. }, Some(name)) => definitions
                .items
                .iter()
                .flat_map(Item::definitions)
                .find(|definition| definition.name == name)
                .map(|definition| definition.type_),
            _ => None,
        };

        self.error.rendered_stating(stated.as_ref(), notation)
    }
}

/// What one whole-module walk reached: every refusal, the record of what it concluded about the definitions it judged, and the module as it stated it.
#[derive(Debug)]
pub struct Rechecked {
    /// Every refusal the walk reached; empty exactly when the kernel accepted the module.
    pub verdicts: Vec<Verdict>,
    /// Each judged definition's totality, closed over everything it mentions, and each accepted registry entry's polarity vector — what a later walk reads once the unit is in its scope. A definition the walk refused is classified all the same, since the walk defines it anyway; a unit carrying a refusal is never filed, so no record of one is ever read.
    pub certification: Certification,
    /// The module handed in, with what this walk stated of each item it judged or took in place of what the module said (`kernel::stating`): which binders and payloads bind a proof, which names are proofs, and the sort a type-valued definition's value inhabits. An item it did not judge again, adopted or never visited, states what the walk that filed the baseline stated of the baseline's item in its place, under its own binders' names (`kernel::stating::carried_marks`), and every other part of every item is the module's. What a unit stores and erases, so the walk over a later unit reads this one by what this walk stated.
    pub definitions: Definitions,
    /// The term a program closes with, as the walk stated its binders where it checked it, and `None` for a unit: what a program is erased from.
    pub entrypoint: Option<Entrypoint>,
}

/// Re-check `module` with the independent kernel, with `globals` already in scope.
///
/// `budget` is the reduction allowance each item gets, the same figure the elaborator's own `Context` is built with. [`Globals::default()`] is the whole-module walk: nothing in scope, so every item is judged.
///
/// The [`Zonked`] input is interface-level evidence, not a delegation: the kernel keeps its own metavariable refusals, so nothing here trusts the wrapper's constructor.
pub fn recheck_definitions(
    definitions: &Zonked<Definitions>,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Result<(), Error> {
    match recheck_definitions_verdicts(definitions, budget, globals, syntax)
        .into_iter()
        .next()
    {
        Some(verdict) => Err(verdict.error),
        None => Ok(()),
    }
}

/// Every item the kernel refuses, rather than only the first.
///
/// # Why this is the intrinsic
///
/// The kernel is incomplete in known places, so a walk over a real module stops at the first of them and says nothing about what lies past it. Discovering those one build at a time is how a checker gets patched in the order its gaps happen to be encountered, rather than in the order they matter. This exists so the gaps can be *counted* before any of them is designed for.
///
/// # Why the verdicts are independent
///
/// `check_definition` and `check_rec_group` both return before their `Kernel::define` step, so a refused item has defined nothing. Running that same define anyway is what keeps this from degenerating into a cascade: every item enters the environment at its declared type with its real body whether or not it checked, so each later item is judged against exactly what it would have been judged against in a fully passing walk.
///
/// Nothing else survives an item. [`Kernel`]'s memos are cleared at every declaration boundary, the pairs its conversion is unfolding are built fresh per comparison, and every binder it opens is retracted on the failing path as well as the succeeding one. So recovery here is exact rather than approximate, and a verdict late in the list is worth as much as the first.
///
/// # What it does not tell you
///
/// The count is per *item*, not per disagreement: an item stops at its own first refusal, so one item with three problems reports one. Good for classifying what is missing, wrong for estimating how much is left.
///
/// # What `globals` buys, and what it does not excuse
///
/// What `globals` holds is read where it stands and never judged again, and a module declaring a name it holds is refused before any of it is judged, whatever it declares under the name. On the compile path that environment is the units in scope, and the faith placed in it is in each unit's *construction*, not in any per-compile claim: a unit is certified by construction, and the prelude build runs this same walk over each root, against the roots before it, and fails the build on any refusal, so an archive that exists is one whose items the kernel accepted. Re-judging them per compile would re-answer a settled question at the cost of walking the whole scope.
///
/// Every judgment is a declaration's own: its item's typing, its classification, its erased positions, and, where it declares registry entries, their sizing, their strict positivity and their variance, each read against what the environment and the declarations judged before it hold. The entrypoint is checked last.
pub fn recheck_definitions_verdicts(
    definitions: &Zonked<Definitions>,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Vec<Verdict> {
    certify_definitions(definitions, budget, globals, syntax)
        .0
        .verdicts
}

/// [`recheck_definitions_verdicts`], with the record the walk leaves of what it concluded — see [`Rechecked`] — and the walk's own kernel, which holds the unit's definitions as the walk defined them, the units in scope it mounted and the vectors it filed: what the unit's erasure reads through ([`Kernel::read`]), and what a measurement asks a finished walk ([`Kernel::heaviest_declaration`]).
pub fn certify_definitions<'s>(
    definitions: &Zonked<Definitions>,
    budget: u64,
    globals: &'s Globals<'s>,
    syntax: SyntaxRegistry,
) -> (Rechecked, Kernel<'s>) {
    let mut kernel = Kernel::new(budget, syntax);
    let rechecked = verdicts_into(
        &mut kernel,
        definitions.as_definitions(),
        None,
        globals,
        None,
    );

    (rechecked, kernel)
}

/// [`certify_definitions`] for a unit compiled over a baseline: the walk is handed the earlier walk's module and record whole and adopts each of its verdicts that still stands, judging every other declaration — see [`Baseline`]. Which declarations those are is this walk's to decide, from the reads the earlier one filed, and no caller's. `places` is where `module` holds each name.
///
/// **`candidates` is what a store at the unit's address holds of verdicts filed before**, by the item each judged: a declaration the walk would judge takes a verdict from a record there where every part it read digests as it did, by the certifier's own digest ([`complete`](crate::complete)).
pub fn certify_definitions_over<'s>(
    definitions: &Zonked<Definitions>,
    budget: u64,
    globals: &'s Globals<'s>,
    baseline: Baseline<'s>,
    (places, candidates): (&'s Places, Option<&'s dyn Candidates>),
    syntax: SyntaxRegistry,
) -> (Rechecked, Kernel<'s>) {
    let mut kernel = Kernel::new(budget, syntax);
    let rechecked = verdicts_into(
        &mut kernel,
        definitions.as_definitions(),
        None,
        globals,
        Some(Over {
            baseline,
            places,
            candidates,
        }),
    );

    (rechecked, kernel)
}

/// [`certify_definitions`] for a program: its module walked as any unit's, and the term it closes with checked against the type it states.
pub fn certify_program<'s>(
    program: &Zonked<Program>,
    budget: u64,
    globals: &'s Globals<'s>,
    syntax: SyntaxRegistry,
) -> (Rechecked, Kernel<'s>) {
    let program = program.as_program();
    let mut kernel = Kernel::new(budget, syntax);
    let rechecked = verdicts_into(
        &mut kernel,
        &program.definitions,
        Some(&program.entrypoint),
        globals,
        None,
    );

    (rechecked, kernel)
}

/// [`recheck_definitions_verdicts`] with the kernel's evaluation memos off.
///
/// Exists for one purpose: asserting that memoization changes no *semantic* verdict — the property that makes a memo an evaluation strategy rather than a store. A resource verdict may differ, since a term-keyed hit is free.
pub fn recheck_definitions_verdicts_uncached(
    definitions: &Zonked<Definitions>,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Vec<Verdict> {
    verdicts_from(
        Kernel::uncached(budget, syntax),
        definitions.as_definitions(),
        None,
        globals,
    )
    .verdicts
}

fn induct_metavar(declaration: &InductDecl) -> Option<MetavarId> {
    let mut found = None;
    let mut note = |id: MetavarId| {
        found = Some(id);
        true
    };

    if !declaration.arity.any_metavar(&mut note) && !declaration.result_sort.any_metavar(&mut note)
    {
        for constructor in declaration.signatures() {
            if constructor.telescope.any_metavar(&mut note) {
                break;
            }
        }
    }

    found
}

/// [`induct_metavar`] for a `struct` registry entry.
fn struct_metavar(declaration: &StructDecl) -> Option<MetavarId> {
    let mut found = None;
    let mut note = |id: MetavarId| {
        found = Some(id);
        true
    };

    if !declaration.arity.any_metavar(&mut note) {
        declaration.result_sort.any_metavar(&mut note);
    }

    found
}

/// The first unsolved *universe* metavariable one of `value`'s levels holds.
///
/// A level holding one is elaboration residue exactly as a `Metavar` node is, and no judgment refuses it: `Sort::of` reads `Type(?u)` and answers `Type(?u + 1)` without ever asking whether the level is ground, and [`closed`](curios_core::UniverseContext::is_closed) inspects a `UniverseContext`'s *constraints*, never a level sitting inside a term. So this is not a question of which terms the walk reaches; it is the level algebra having no opinion about an unsolved level, which is why the refusal belongs at the boundary rather than inside a judgment.
///
/// `sorts` is how many sorts `value`'s declaration leaves to its caller, which [`open_sort`] reads.
fn universe_residue<B: Bound>(value: &B, sorts: usize) -> Option<Error> {
    universe_metas(value)
        .into_iter()
        .next()
        .map(|meta| Error::NotCore(Term::type_at(Level::meta(meta))))
        .or_else(|| open_sort(value, sorts))
}

/// The first sort or instance of `value` stating a quality its declaration, which leaves `sorts` to its caller, does not bind, as the refusal of it.
///
/// A quality metavariable is residue as a level's is. A quality parameter at or past the declaration's count is one nothing binds: no assignment the declaration is judged at reaches it, so the term would be judged, and used, at a sort that states neither `Prop` nor `Type`.
fn open_sort<B: Bound>(value: &B, sorts: usize) -> Option<Error> {
    open_quality(value, sorts).map(|quality| Error::OpenSort(Term::sort_at(quality, Level::zero())))
}

/// The first universe parameter one of `value`'s levels names that a scheme of `parameter_count` parameters does not have.
///
/// A declaration's universe scheme promises that every level it mentions is ground or one of the parameters it declares, so a use site fully determines it. [`universe_residue`] above checks half of that promise — no unsolved metavariable — and this checks the other half: no parameter index past the declaration's own count. `closed` sees a `UniverseContext`'s constraints and not a level sitting in a term, so the two are the same omission at different depths.
///
/// It is a soundness rule rather than a tidiness one because of what instantiation does with an out-of-range index: `instantiate_universes` substitutes what the instance supplies and leaves any other index as it stands, so a parameter past the scheme reaches the use site naming the use site's own parameter of that index. That is a capture: `Type.{param 1}` in a one-parameter scheme stays `Type.{param 1}` at the instance `[v]`, a level of the reader's that the scheme never had, and every cumulativity question after that is answered about it.
///
/// Flat: a level names only the parameters of the declaration it stands in, so the bound is that declaration's count wherever the level stands, as `curios-elab`'s `validate_bound_universes` reads it. Metavariables are left to [`universe_residue`] so the two diagnostics stay distinct.
fn universe_escape<B: Bound>(value: &B, parameter_count: usize) -> Option<Error> {
    rewrite_universe_levels(value, move |level| {
        match level.params().any(|param| param.0 >= parameter_count) {
            true => Err(()),
            false => Ok(level.clone()),
        }
    })
    .err()
    .map(|()| Error::UnclosedUniverses)
}

/// For each former the item at `place` carries, the first name its domains reach that is declared by that item or by an item reaching it back: what the former is indexed by that does not stand before it.
fn indexed_by_its_cycle(
    definitions: &Definitions,
    place: usize,
    places: &OnceCell<HashMap<Global, usize>>,
) -> Vec<(Global, Global)> {
    let item = &definitions.items[place];
    let own = item.declared_names();
    let mut indexed = Vec::new();
    for (name, nominal) in item.nominals() {
        let member = nominal
            .former_domains()
            .into_iter()
            .flat_map(Term::reaches)
            .find(|reached| {
                own.contains(&reached)
                    || places
                        .get_or_init(|| place_of(definitions))
                        .get(reached)
                        .is_some_and(|&other| reaches_place(definitions, places, other, place))
            });
        if let Some(member) = member {
            indexed.push((*name, member));
        }
    }

    indexed
}

/// The place of the item that declares each name of the module.
fn place_of(definitions: &Definitions) -> HashMap<Global, usize> {
    definitions
        .items
        .places()
        .flat_map(|place| {
            definitions
                .items
                .names(place)
                .into_iter()
                .map(move |name| (*name, place))
        })
        .collect()
}

/// Whether the item at `from` reaches the item at `target`, through what each item's terms and registry entries name.
fn reaches_place(
    definitions: &Definitions,
    places: &OnceCell<HashMap<Global, usize>>,
    from: usize,
    target: usize,
) -> bool {
    let places = places.get_or_init(|| place_of(definitions));
    let mut seen = HashSet::from([from]);
    let mut stack = vec![from];
    while let Some(at) = stack.pop() {
        for name in definitions.reaches(&definitions.items[at]) {
            match places.get(&name) {
                Some(&next) if next == target => return true,
                Some(&next) if seen.insert(next) => stack.push(next),
                _ => {}
            }
        }
    }

    false
}

/// Every kind of elaboration residue an `induct` registry entry can carry.
fn induct_residue(declaration: &InductDecl) -> Option<Error> {
    let sorts = declaration.universe_context.quality_count;
    induct_metavar(declaration)
        .map(|id| Error::NotCore(Term::hole(id)))
        .or_else(|| universe_residue(&declaration.arity, sorts))
        .or_else(|| universe_residue(&declaration.result_sort, sorts))
        .or_else(|| {
            declaration
                .signatures()
                .find_map(|constructor| universe_residue(&constructor.telescope, sorts))
        })
        .or_else(|| {
            let count = declaration.universe_context.parameter_count;
            universe_escape(&declaration.arity, count)
                .or_else(|| universe_escape(&declaration.result_sort, count))
                .or_else(|| {
                    declaration
                        .signatures()
                        .find_map(|constructor| universe_escape(&constructor.telescope, count))
                })
        })
}

/// [`induct_residue`] for a `struct` registry entry.
fn struct_residue(declaration: &StructDecl) -> Option<Error> {
    let sorts = declaration.universe_context.quality_count;
    struct_metavar(declaration)
        .map(|id| Error::NotCore(Term::hole(id)))
        .or_else(|| universe_residue(&declaration.arity, sorts))
        .or_else(|| universe_residue(&declaration.result_sort, sorts))
        .or_else(|| {
            let count = declaration.universe_context.parameter_count;
            universe_escape(&declaration.arity, count)
                .or_else(|| universe_escape(&declaration.result_sort, count))
        })
}

/// Every name `module` declares a second time, each refused once, before anything of the module is judged.
///
/// **A name is one declaration.** The walk defines a name once, the kernel's environment holds one entry under it, and every analysis over the declaration set files a declaration under its name. A second declaration under a name is therefore never one more thing to judge: written over the first it changes what an accepted declaration states, and declaring a second nominal under it leaves one of the two unanalysed. So a module is refused for a name an earlier item or member declares and for a name a unit of `globals` declares; an entry is held by its former, so a name under two entries is a name under two items.
///
/// **Whatever it declares under a held name.** What the environment holds was judged by the walk that built it and is read where it stands, so a module has no reason to carry one of its declarations, and one that does is refused as any other redeclaration is: no comparison decides that two declarations under one name are one.
///
/// **Refused whole.** A module that redeclares offers the rest of itself no environment to be judged in, so nothing of it is: the verdicts handed back are its redeclarations and nothing else, each naming its declaration, a group by its first member as everywhere.
fn redeclarations(definitions: &Definitions, globals: &Globals) -> Vec<Verdict> {
    curios_profile::profile!("redeclarations");
    let mut refused: BTreeSet<Global> = BTreeSet::new();
    let mut redeclared = Vec::new();
    let mut refuse = |name: &Global| {
        if refused.insert(*name) {
            redeclared.push(Verdict {
                name: Some(*name),
                error: Error::Redeclared(*name),
            });
        }
    };

    let mut declared: HashSet<&Global> = HashSet::new();
    for item in &definitions.items {
        let names = item.declared_names();
        let Some(first) = names.first().copied() else {
            continue;
        };
        if let Some(name) = names.iter().copied().find(|name| !declared.insert(*name)) {
            refuse(name);
            continue;
        }
        if names.iter().any(|name| globals.in_scope(name)) {
            refuse(first);
        }
    }

    redeclared
}

/// The entries of one kind a module declares, under the names `among` holds, in key order.
fn entries_among<'a, D>(
    registry: &'a BTreeMap<Global, D>,
    among: &'a BTreeSet<Global>,
) -> impl Iterator<Item = (&'a Global, &'a D)> {
    registry.iter().filter(|(name, _)| among.contains(*name))
}

fn verdicts_from<'s>(
    mut kernel: Kernel<'s>,
    definitions: &Definitions,
    entry: Option<&Entrypoint>,
    globals: &'s Globals<'s>,
) -> Rechecked {
    verdicts_into(&mut kernel, definitions, entry, globals, None)
}

fn verdicts_into<'s>(
    kernel: &mut Kernel<'s>,
    definitions: &Definitions,
    entry: Option<&Entrypoint>,
    globals: &'s Globals<'s>,
    over: Option<Over<'s>>,
) -> Rechecked {
    grown(|| verdicts_within(kernel, definitions, entry, globals, over))
}

/// A walk over a baseline: the earlier walk, handed over whole, where the module being judged holds each name, and what a store holds of verdicts filed before.
#[derive(Clone, Copy)]
pub(crate) struct Over<'s> {
    pub(crate) baseline: Baseline<'s>,
    pub(crate) places: &'s Places,
    pub(crate) candidates: Option<&'s dyn Candidates>,
}

/// The walk: `module`'s items, then — for a program — the term `entry` closes it with. Over a baseline, the walk visits the items whose terms or registry entries are not the baseline's and the declarations whose filed reads could reach them ([`Changed`]): it judges the first, and of the others adopts each verdict that stands in its turn and judges the rest ([`Baseline`]); every other declaration is the baseline's, read where the baseline holds it once the walk has passed it, as a walk that adopted it there would read it.
fn verdicts_within<'s>(
    kernel: &mut Kernel<'s>,
    definitions: &Definitions,
    entry: Option<&Entrypoint>,
    globals: &'s Globals<'s>,
    over: Option<Over<'s>>,
) -> Rechecked {
    curios_profile::profile!("recheck_definitions");
    // A baseline is read only where its record covers the module it was filed with, as a mounted record is (`Globals::partial`) and a stored one (`part_digest`): a record naming fewer definitions than its module holds was not made by a walk over it, and a name it leaves out would read as total wherever this walk does not visit it, since nothing classifies the unvisited from their terms. Without one, the walk judges the unit whole.
    let over = over.filter(|over| {
        over.baseline
            .certification
            .covers(over.baseline.definitions)
    });
    // Whether each part the baseline's walk read of a unit that moved is the part in scope now: asked once, a part being read by every declaration that names it.
    let abroad = RefCell::new(HashMap::<(Global, bool), bool>::new());
    let mut changed = over.map(|over| Changed::of(definitions, globals, over, &abroad));
    // A module that declares a name twice, its environment's among them, is refused here, whole, before anything of it is judged. Over a baseline every item is asked only where an item it changed may have declared a held name.
    if changed.as_ref().is_none_or(|changed| changed.redeclares) {
        let redeclared = redeclarations(definitions, globals);
        if !redeclared.is_empty() {
            return Rechecked {
                verdicts: redeclared,
                certification: Certification::default(),
                definitions: definitions.clone(),
                entrypoint: entry.cloned(),
            };
        }
    }
    let mut verdicts = Vec::new();

    // The items the walk visits, by place, in the order the module stores them: every item, where it walks over no baseline.
    let walk = match &changed {
        Some(changed) => changed.visited.iter().copied().collect::<Vec<_>>(),
        None => definitions.items.places().collect(),
    };
    let visited = changed
        .as_mut()
        .map(|changed| mem::take(&mut changed.visited_names));
    // Whether the walk holds the terms of the item at a place to the rules a declaration's own terms decide — its universe contexts, the levels and binders it mentions, elaboration's residue: every item's over no baseline, and over one the items whose terms are not the baseline's, the walk that filed the baseline having held its own to them.
    let own_terms = |place: &usize| {
        changed
            .as_ref()
            .is_none_or(|changed| changed.items.contains(place))
    };
    let judged_items = || {
        walk.iter()
            .filter(|place| own_terms(place))
            .map(|place| &definitions.items[*place])
    };
    let mut adopting = over.map(|over| Adopting::over(over.baseline));
    // The registry entries the visited items' formers carry, each under the name that declares it, and those whose terms the walk holds to their rules.
    let (mut inducts, mut structs) = (BTreeMap::new(), BTreeMap::new());
    let mut checked = BTreeSet::new();
    for place in &walk {
        for (name, nominal) in definitions.items[*place].nominals() {
            if let Some(declaration) = nominal.inductive() {
                inducts.insert(*name, declaration);
            }
            if let Some(record) = nominal.record() {
                structs.insert(*name, record);
            }
            if own_terms(place) {
                checked.insert(*name);
            }
        }
    }

    // A case whose counts are not its family's is refused here, whole, as a redeclaration is: every reader of a case opens its parameters at the family's, the family's own positivity and an arm judged before it among them, so no judgment of the module may run first.
    let miscounted = entries_among(&inducts, &checked)
        .filter_map(|(name, declaration)| {
            check_case_counts(declaration).err().map(|error| Verdict {
                name: Some(*name),
                error,
            })
        })
        .collect::<Vec<_>>();
    if !miscounted.is_empty() {
        return Rechecked {
            verdicts: miscounted,
            certification: Certification::default(),
            definitions: definitions.clone(),
            entrypoint: entry.cloned(),
        };
    }

    // The environment is the kernel's starting scope, not something consulted beside it: an item already in scope stands exactly as a refused item does — at its declared type, with its real body — so every judged item downstream sees what a walk that judged everything would have shown it. A baseline is beneath the walk as the scope is, answering for the declarations whose verdict the walk adopts, and for every declaration it does not visit once it has passed it.
    kernel.seed(globals);
    if let (Some(over), Some(visited)) = (over, visited) {
        kernel.over(over.baseline, Unvisited::new(over.places, visited));
    }

    // The kernel mints its binders from zero, so a free local in a term it judges is one it could mint again while comparing under a telescope or eta-contracting — a capture, after which the judgment sees a bound variable and never an unbound one. So a local is refused here, at the boundary, over every position a term can sit in but those of the declarations the baseline holds term for term, which the walk that filed the baseline held to this rule.
    for (owner, local) in free_locals_of(judged_items()) {
        verdicts.push(Verdict {
            name: owner,
            error: Error::Unbound(local),
        });
    }
    // A bound variable no scope binds is refused over the same positions, and no judgment could take the refusal instead: `Mentions` answers nothing for one, so a rule could read it only as the variable it is not — `infer`'s refuses it, and every reader past it answers as for a term it cannot type — and the index denotes whatever the depth it is read at gives it, so what a rule would judge is not what the module carries.
    for (owner, reach) in open_positions_of(judged_items()) {
        verdicts.push(Verdict {
            name: owner,
            error: Error::NotClosed { reach },
        });
    }
    if let Some(entry) = entry {
        for local in entry.free_locals() {
            verdicts.push(Verdict {
                name: None,
                error: Error::Unbound(local),
            });
        }
        let reach = entry.reach();
        if reach != 0 {
            verdicts.push(Verdict {
                name: None,
                error: Error::NotClosed { reach },
            });
        }
    }

    // Every universe context this walk will assume, decided before any of it is assumed. An unsatisfiable set makes `entails` answer anything, so a later refusal would be reported against whichever item happened to ask a level question first rather than against the declaration that carries the contradiction.
    let contexts = entries_among(&inducts, &checked)
        .map(|(name, declaration)| (*name, &declaration.universe_context))
        .chain(
            entries_among(&structs, &checked)
                .map(|(name, declaration)| (*name, &declaration.universe_context)),
        )
        .collect::<Vec<_>>();
    for (name, context) in contexts {
        if let Some(error) = universe_verdict(context) {
            verdicts.push(Verdict {
                name: Some(name),
                error,
            });
        }
    }
    for item in judged_items() {
        for definition in item.definitions() {
            if let Some(error) = universe_verdict(&definition.universe_context) {
                verdicts.push(Verdict {
                    name: Some(definition.name),
                    error,
                });
            }
        }
    }

    // A registry entry is data; what it declares — a literal result sort, sized constructors, and each index target typed against the arity at the constructor's own parameters — is `check_induct_decl`'s to check, below. `infer` and `convert` refuse an elaboration-only node wherever a judgment meets one; this is the boundary pass that decides the same thing for the positions no judgment visits, so that "no unsolved metavariable survives" is this walk's own verdict rather than the elaborator's word.
    for (name, declaration) in entries_among(&inducts, &checked) {
        if let Some(error) = induct_residue(declaration) {
            verdicts.push(Verdict {
                name: Some(*name),
                error,
            });
        }
    }
    for (name, declaration) in entries_among(&structs, &checked) {
        if let Some(error) = struct_residue(declaration) {
            verdicts.push(Verdict {
                name: Some(*name),
                error,
            });
        }
    }
    // A former's domains, a family's parameters and indices and a record's or a concept's parameters, name no declaration that reaches it back, itself included: positivity walks no index binder type, which is sound only because nothing a declaration's own cycle declares is in scope there, and a family indexed by a family that holds it is induction-induction, which no decision admits. Over a baseline every item the walk visits is asked, adopted or judged: whether a former's domains name a declaration that reaches it back turns on what the items around it hold, so a step that makes an unchanged family's index hold it moves the answer and not the family's terms.
    let places = OnceCell::new();
    for place in walk.iter().copied() {
        for (name, member) in indexed_by_its_cycle(definitions, place, &places) {
            verdicts.push(Verdict {
                name: Some(name),
                error: Error::IndexedByItsGroup { member },
            });
        }
    }
    // An item's own terms are walked, so a `Metavar` node in one is refused where a judgment meets it. A level is not: the walk types `Type(?u)` without objecting, so the same boundary decides it here.
    for item in judged_items() {
        for definition in item.definitions() {
            let count = definition.universe_context.parameter_count;
            let sorts = definition.universe_context.quality_count;
            if let Some(error) = universe_residue(&definition.type_, sorts)
                .or_else(|| universe_residue(&definition.body, sorts))
                .or_else(|| universe_escape(&definition.type_, count))
                .or_else(|| universe_escape(&definition.body, count))
            {
                verdicts.push(Verdict {
                    name: Some(definition.name),
                    error,
                });
            }
        }
    }
    // The module's own entry stands under no scheme, so every parameter index in it escapes, and every sort it states is one.
    if let Some(entry) = entry
        && let Some(error) = entry
            .type_
            .as_ref()
            .and_then(|type_| universe_residue(type_, 0))
            .or_else(|| universe_residue(&entry.body, 0))
            .or_else(|| {
                entry
                    .type_
                    .as_ref()
                    .and_then(|type_| universe_escape(type_, 0))
            })
            .or_else(|| universe_escape(&entry.body, 0))
    {
        verdicts.push(Verdict { name: None, error });
    }

    // The nominal registry first: a definition's type may name any declaration in the module, including one whose own definitions come later. The environment refuses a name it holds, which the refusal above rules out for every write this walk makes; a refusal is reported all the same, being the one thing a walk makes of a write it cannot perform. Over a baseline the entries of the declarations the walk does not visit are the baseline's, which answers for each from the start.
    // Each family stating no payload a proof, until the declaration that declares it is judged and states its own (`kernel::stating`): the walk reads nothing a module says of a mark.
    for (name, declaration) in &inducts {
        if let Err(error) = kernel.declare_induct(name, &unstated_entry(declaration)) {
            verdicts.push(Verdict {
                name: Some(*name),
                error,
            });
        }
    }
    for (name, declaration) in &structs {
        if let Err(error) = kernel.declare_struct(name, declaration) {
            verdicts.push(Verdict {
                name: Some(*name),
                error,
            });
        }
    }

    // A unit in scope whose record files no vector for an entry of its own: this walk's to compute, before any judgment composes through one.
    file_scope_polarities(kernel, &mut verdicts);
    file_scope_variances(kernel, &mut verdicts);

    // What each judged definition read of other items, taken at each judgment's end so it holds that judgment's reads alone.
    let mut reads: BTreeMap<Global, Reads> = BTreeMap::new();
    kernel.take_reads();
    // The definitions not known to terminate, as the walk comes to know them: the environment's, then each item's as it is judged.
    let mut totals = Totals::seeded(kernel, globals);
    let mut local_memo = PositionVerdicts::default();
    // The vector the walk filed for each registry entry it accepted.
    let mut polarities: BTreeMap<Global, Vec<Polarity>> = BTreeMap::new();
    let mut variances: BTreeMap<Global, Vec<Variance>> = BTreeMap::new();
    // The names of the declarations the walk judged, whose reads are its own.
    let mut judged = Vec::new();
    // The records the walk took a verdict from, of a store, by the name each was filed under.
    let mut taken = BTreeMap::<Global, HeldReads>::new();
    // The names of the declarations the walk has settled, judged or adopted: what a record a store holds may be held to.
    let mut settled = BTreeSet::<Global>::new();
    // The module as the walk states it, settled item by item: an item it judges or takes as it states it, and every other as the baseline's walk stated it. An item of the baseline's the elaborator made again arrives saying what the elaborator stated, so it takes what that walk stated of the item the baseline holds in its place, keeping its own names and places (`kernel::stating`). It is what the walk digests a declaration of its own unit by, so a read is told by what this kernel stated and never by what a module arriving says.
    let mut stated = definitions.clone();
    if let (Some(over), Some(changed)) = (over, &changed) {
        for place in changed.touched.difference(&changed.items) {
            let item = &definitions.items[*place];
            if let Some(held) = held_item(over.baseline, item) {
                put(&mut stated, *place, carried_marks(item, held));
            }
        }
    }

    // The items as the module stores them, which is every item after the items it reaches where the module was made by the elaborator (`Definitions::ordered`). The walk sorts nothing: an item that names one stored after it finds the name unbound, and a family it stores in one accepted after it finds no vector filed, each of which refuses.
    for place in &walk {
        let item = &definitions.items[*place];
        let item_name = item.declared_names().first().map(|&name| *name);
        kernel.at(*place);

        // A declaration whose terms are the baseline's and whose every recorded read stands takes the baseline's verdict, and stands as the walk leaves any item it does not judge. A read of a declaration the walk does not visit stands where the walk has passed it.
        if let Some(adopting) = &mut adopting
            && !own_terms(place)
            && let Some(certified) = adopting.adopts(item, |name, body| {
                kernel.passed(name)
                    || read_stands(globals, adopting.baseline(), &abroad, name, body)
            })
        {
            let adopted = adopt(kernel, adopting.baseline(), item, &certified);
            verdicts.extend(adopted.refusals);
            reads.extend(adopted.reads);
            for name in adopted.partial {
                totals.adopt(name);
            }
            for (name, vector) in adopted.vectors {
                polarities.insert(name, vector);
            }
            for (name, vector) in adopted.variances {
                variances.insert(name, vector);
            }
            adopting.adopted(item);
            // Its families stand as the baseline's walk stated them, as the item does.
            for (name, declaration) in carried(&stated.items[*place]).0 {
                kernel.restate_induct(&name, &declaration);
            }
            settled.extend(item.declared_names().into_iter().copied());
            continue;
        }

        // A declaration the walk would judge takes the verdict of a record a store holds of the item, where every part the record read digests as it did.
        if let Some(over) = over
            && let Some(candidates) = over.candidates
            && let Some(records) = {
                let now = Settled {
                    definitions,
                    stated: &stated,
                    places: over.places,
                    baseline: over.baseline,
                    settled: &settled,
                    totals: &totals,
                    polarities: &polarities,
                    variances: &variances,
                };
                stored(candidates, globals, kernel, &now, *place, item)
            }
        {
            let (taken_item, refusals) = take(kernel, item);
            verdicts.extend(refusals);
            put(&mut stated, *place, taken_item);
            for (name, record) in records {
                reads.insert(name, (**record).clone());
                let made = record.made().expect("a record taken is complete");
                if made.totality != Totality::Total {
                    totals.adopt(name);
                }
                if let Some((vector, variance)) = made.vectors.clone() {
                    kernel.file_polarities(&name, vector.clone());
                    kernel.file_variances(&name, variance.clone());
                    polarities.insert(name, vector);
                    variances.insert(name, variance);
                }
                judged.push(name);
                taken.insert(name, record);
            }
            if let Some(adopting) = &mut adopting {
                adopting.judged(
                    (&stated, over.places),
                    item,
                    |name| totals.partial(kernel, name),
                    &polarities,
                    &variances,
                );
            }
            settled.extend(item.declared_names().into_iter().copied());
            continue;
        }

        // One span per judged item, grouped as the elaborator's `declaration` span is, so an item's cost in the kernel is read beside its cost in the elaborator by the same key.
        curios_profile::profile!("certify_declaration", group = %item.describe());

        // What the walk states of the item's binders and of its families' payloads, before the closed machine runs on any of its terms; its families are held as stated from here on.
        kernel.restore_budget();
        let marked = stated_marks(kernel, item);
        for (name, declaration) in carried(&marked).0 {
            kernel.restate_induct(&name, &declaration);
        }

        let item = match marked {
            Item::Let(definition) => {
                let before = kernel.partial_groups();
                let outcome = check_definition(
                    kernel,
                    &definition.name,
                    &definition.kind,
                    &definition.type_,
                    &definition.body,
                    &definition.universe_context,
                );
                // Whether the definition holds a group that does not descend, noted where the group was typed — see `Kernel::definition_encloses_partial`.
                if kernel.partial_groups() > before {
                    kernel.note_enclosing_partial(&definition.name);
                }

                let (proof, value_sort) = match outcome {
                    Ok(Stated { proof, value_sort }) => (proof, value_sort),
                    Err(error) => {
                        verdicts.push(Verdict {
                            name: Some(definition.name),
                            error,
                        });
                        // A refused definition stands at its type with its body, and unfolds.
                        if let Err(error) = define_as_stated(
                            kernel,
                            &definition.name,
                            &definition.type_,
                            &definition.body,
                            false,
                            None,
                            &definition.universe_context,
                        ) {
                            verdicts.push(Verdict {
                                name: Some(definition.name),
                                error,
                            });
                        }

                        (false, None)
                    }
                };

                Item::Let(Box::new(Definition {
                    proof,
                    value_sort,
                    ..*definition
                }))
            }
            Item::Rec(mut rec) => {
                let members = rec
                    .definitions
                    .iter()
                    .map(|definition| (definition.name, &definition.kind))
                    .collect::<Vec<_>>();
                let universes = rec.universe_context.clone();

                let proofs = match check_rec_group(kernel, &members, &rec.group, &universes) {
                    Ok(proofs) => proofs,
                    Err(error) => {
                        verdicts.push(Verdict {
                            name: item_name,
                            error,
                        });
                        // The define `check_rec_group` performs on success: each export is the folded selection of the member it names.
                        for (member, (name, _)) in members.iter().enumerate() {
                            if let Err(error) = define_as_stated(
                                kernel,
                                name,
                                &rec.group.member_type(member),
                                &Term::rec_proj(rec.group.clone(), member),
                                false,
                                None,
                                &universes,
                            ) {
                                verdicts.push(Verdict {
                                    name: item_name,
                                    error,
                                });
                            }
                        }

                        vec![false; members.len()]
                    }
                };
                for (definition, proof) in rec.definitions.iter_mut().zip(proofs) {
                    definition.proof = proof;
                }

                Item::Rec(rec)
            }
        };
        let item = &item;

        // The item's classification. Every definition it mentions was judged before it and is classified already, so the set its positions are read against below is closed where they are read.
        totals.classify(kernel, item);

        // A group is judged as one item, so each of its members read what the group did; and what the classification read of what it mentions is the item's too.
        let item_reads = kernel.take_reads();
        for name in item.declared_names() {
            reads.insert(*name, item_reads.clone());
            judged.push(*name);
        }

        // The registry entries the item's formers carry, accepted with it: an item follows every name its entries' telescopes mention, so each is defined by now.
        let (carried_inducts, carried_structs) = carried(item);
        accept(
            kernel,
            &carried_inducts,
            &carried_structs,
            &mut verdicts,
            &mut reads,
            &mut polarities,
            &mut variances,
        );

        // Obligations (T) and (V) over every position the item's judgment recorded, its entries' typings among them, so each is refused under the item that declares it rather than under the next one judged. An entry's positions reach only names declared before it, each classified by now.
        let (drained, failure) = kernel.take_checked();
        if let Some(error) = failure {
            verdicts.push(Verdict {
                name: item_name,
                error,
            });
        }
        if let Err(error) = check_positions(kernel, &drained, &totals, &mut local_memo) {
            verdicts.push(Verdict {
                name: item_name,
                error,
            });
        }

        // How the item came out against the baseline's, for the declarations that read it.
        put(&mut stated, *place, item.clone());
        if let (Some(adopting), Some(over)) = (&mut adopting, over) {
            adopting.judged(
                (&stated, over.places),
                item,
                |name| totals.partial(kernel, name),
                &polarities,
                &variances,
            );
        }
        settled.extend(item.declared_names().into_iter().copied());
    }
    // Past every item: the entry reads them all.
    kernel.at(usize::MAX);

    // A unit with no entrypoint has nothing here to judge — being the entry is what having one *means*, and a scope unit is not it.
    kernel.restore_budget();
    let entry = entry.map(|entry| stated_entrypoint(kernel, entry));
    if let Some(entry) = &entry {
        let checked = match &entry.type_ {
            Some(type_) => check_entrypoint(kernel, &entry.body, type_),
            None => Err(Error::UntypedEntry),
        };
        if let Err(error) = checked {
            verdicts.push(Verdict { name: None, error });
        }
    }

    // The entrypoint is no definition, and nothing reads what it read: a later unit never names it.
    kernel.take_reads();
    let (drained, failure) = kernel.take_checked();
    if let Some(error) = failure {
        verdicts.push(Verdict { name: None, error });
    }
    if let Err(error) = check_positions(kernel, &drained, &totals, &mut local_memo) {
        verdicts.push(Verdict { name: None, error });
    }

    let totality = |name: &Global| match totals.partial(kernel, name) {
        true => Totality::Partial,
        false => Totality::Total,
    };
    let certification = match (over, changed) {
        (Some(over), Some(changed)) => revised(
            globals,
            over.baseline,
            &changed,
            Judged {
                names: &judged,
                taken: &taken,
                reads: &reads,
                totality: &totality,
                polarities: &polarities,
                variances: &variances,
            },
        ),
        _ => {
            // What the walk read of each declaration of another unit, by the digest of the part read.
            let mut read_abroad = BTreeMap::<Global, Abroad>::new();
            for read in reads.values() {
                for (names, body) in [(&read.signatures, false), (&read.bodies, true)] {
                    for name in names {
                        if !globals.in_scope(name) || !globals.recorded(name) {
                            continue;
                        }
                        let entry = read_abroad.entry(*name).or_default();
                        let part = match body {
                            true => &mut entry.body,
                            false => &mut entry.signature,
                        };
                        if part.is_none() {
                            *part = read_digest(globals, name, body).ok().flatten();
                        }
                    }
                }
            }

            // The record of what this walk concluded: every judged definition's totality, from the set each was classified into, with what its judgment read, and every accepted entry's polarity vector — so a later walk reading it needs nothing of this one's.
            Certification::of(
                definitions
                    .items
                    .iter()
                    .flat_map(Item::declared_names)
                    .map(|name| {
                        let reads = reads.remove(name).unwrap_or_default();
                        (
                            *name,
                            Certified {
                                totality: totality(name),
                                reads,
                            },
                        )
                    }),
                polarities,
                variances,
                read_abroad,
            )
        }
    };

    Rechecked {
        verdicts,
        certification,
        definitions: stated,
        entrypoint: entry,
    }
}

/// What a walk over a baseline concluded of the declarations it judged: their names, what each read, how each is classified, and the vectors of the entries it accepted.
struct Judged<'w> {
    names: &'w [Global],
    /// The records of a store the walk took the verdicts of some of `names` from, which are filed as they were taken.
    taken: &'w BTreeMap<Global, HeldReads>,
    reads: &'w BTreeMap<Global, Reads>,
    totality: &'w dyn Fn(&Global) -> Totality,
    polarities: &'w BTreeMap<Global, Vec<Polarity>>,
    variances: &'w BTreeMap<Global, Vec<Variance>>,
}

/// The record a walk over `baseline` files: the baseline's, revised where the walk judged a declaration or the module no longer holds one ([`Certification::revised`]), with who read what of it.
///
/// **What was read of other units, kept by its changes.** The baseline's record holds a digest of each part its walk read of a declaration of another unit, and what reads each changed only where a declaration was judged or dropped: a part a judged declaration reads is recorded, digested again where nothing was recorded of it or the unit declaring it moved; a part a declaration judged or dropped read, which no declaration reads now, is taken out. A part an adopted or unvisited declaration reads of a unit that moved digests to what it did, or the declaration would have been judged, so its record stands.
fn revised(
    globals: &Globals,
    baseline: Baseline<'_>,
    changed: &Changed,
    judged: Judged<'_>,
) -> Certification {
    let mut revision = baseline.certification.revised();
    for name in judged.names {
        match judged.taken.get(name) {
            Some(record) => revision.adopted(*name, Rc::clone(record)),
            None => revision.judged(
                *name,
                Certified {
                    totality: (judged.totality)(name),
                    reads: judged.reads.get(name).cloned().unwrap_or_default(),
                },
            ),
        }
        let vectors = judged
            .polarities
            .get(name)
            .zip(judged.variances.get(name))
            .map(|(polarities, variances)| (polarities.clone(), variances.clone()));
        revision.vectors(*name, vectors);
    }
    for name in &changed.lost {
        revision.dropped(name);
    }

    // Who read what of the record this walk files: the baseline's, with the edges of every declaration judged, or no longer held, replaced.
    let mut read_by = baseline.certification.read_by().clone();
    for name in judged.names.iter().chain(&changed.lost) {
        read_by.replace(
            *name,
            baseline.certification.reads(name),
            judged.reads.get(name),
        );
    }

    let recorded = |name: &Global| globals.in_scope(name) && globals.recorded(name);
    for name in judged.names {
        let Some(read) = judged.reads.get(name) else {
            continue;
        };
        for (names, body) in [(&read.signatures, false), (&read.bodies, true)] {
            for read in names.iter().filter(|read| recorded(read)) {
                let mut entry = revision.abroad(read).cloned().unwrap_or_default();
                let part = match body {
                    true => &mut entry.body,
                    false => &mut entry.signature,
                };
                if part.is_none() || !globals.unmoved(read) {
                    *part = read_digest(globals, read, body).ok().flatten();
                }
                revision.read_abroad(*read, Some(entry));
            }
        }
    }
    for name in judged.names.iter().chain(&changed.lost) {
        let Some(earlier) = baseline.certification.reads(name) else {
            continue;
        };
        for (names, body) in [(&earlier.signatures, false), (&earlier.bodies, true)] {
            for read in names.iter().filter(|read| recorded(read)) {
                let readers = match body {
                    true => read_by.body(read),
                    false => read_by.signature(read),
                };
                let Some(mut entry) = revision
                    .abroad(read)
                    .cloned()
                    .filter(|_| readers.is_empty())
                else {
                    continue;
                };
                match body {
                    true => entry.body = None,
                    false => entry.signature = None,
                }
                let still = !read_by.signature(read).is_empty() || !read_by.body(read).is_empty();
                revision.read_abroad(*read, still.then_some(entry));
            }
        }
    }

    revision.filed().reading(read_by)
}

/// What a walk over a baseline finds changed of the module it judges, and what that could reach: by its own comparison of the two modules ([`Baseline::holds`]) and its own record read backwards ([`ReadBy`](curios_core::ReadBy)).
///
/// **Found from the edit, by the certifier's own record.** Where the module is a version of the baseline's, the two are compared by what they do not share ([`Items::changed_since`](curios_core::Items::changed_since)), and only the items held where the baseline held another or none are asked whether they are the baseline's; otherwise every item is. An item that is not, a name the baseline declared and the module does not, and, of a unit in scope that moved, each part read whose digest moved ([`read_stands`]) are where what was read can have moved. A declaration whose filed reads name one of them is reached, and so is one whose reads name a declaration reached, and so is one held where the baseline held another or none whose filed reads name a declaration of the module stored after it, which a walk in the module's order meets before what it read is settled. A declaration none reaches reads only what stands, so asking its reads would adopt it: nothing another component computed decides that, and the record is the one this crate filed.
struct Changed {
    /// The places the module holds another item at than the baseline does, or every place where the two modules are not one a version of the other.
    touched: BTreeSet<usize>,
    /// The places of the items whose terms or registry entries are not the baseline's.
    items: BTreeSet<usize>,
    /// The names the baseline declared and the module does not.
    lost: Vec<Global>,
    /// The places of the declarations the walk visits: the items above, and every declaration an edit could reach.
    visited: BTreeSet<usize>,
    /// The names the visited declarations declare.
    visited_names: HashSet<Global>,
    /// Whether an item the module holds where the baseline held another or none declares a name another item or a unit in scope declares, or nothing says which items those are: then every item is asked whether it redeclares a name.
    redeclares: bool,
}

impl Changed {
    fn of(
        definitions: &Definitions,
        globals: &Globals,
        over: Over<'_>,
        abroad: &RefCell<HashMap<(Global, bool), bool>>,
    ) -> Self {
        let (baseline, places) = (over.baseline, over.places);
        let read_by = baseline.certification.read_by();
        // The places the module holds another item at than the baseline does, the names the baseline held at the places that changed, and whether an item at one of those places may have declared a held name: every place and every name, where the two modules are not one a version of the other.
        let (touched, earlier, redeclares) =
            match definitions.items.changed_since(&baseline.definitions.items) {
                Some(changes) => {
                    let earlier = changes
                        .replaced
                        .iter()
                        .chain(&changes.removed)
                        .flat_map(|place| baseline.definitions.items.names(*place))
                        .copied()
                        .collect::<Vec<_>>();
                    let touched = changes
                        .replaced
                        .iter()
                        .chain(&changes.inserted)
                        .copied()
                        .collect::<BTreeSet<_>>();
                    let redeclares = !globals.settled()
                        || declares_a_held_name(definitions, globals, baseline, &touched, &changes);
                    (touched, earlier, redeclares)
                }
                None => (
                    definitions.items.places().collect::<BTreeSet<_>>(),
                    baseline
                        .definitions
                        .items
                        .places()
                        .flat_map(|place| baseline.definitions.items.names(place))
                        .copied()
                        .collect(),
                    true,
                ),
            };
        let items = touched
            .iter()
            .copied()
            .filter(|place| !baseline.holds(&definitions.items[*place]))
            .collect::<BTreeSet<_>>();
        let lost = earlier
            .into_iter()
            .filter(|name| places.place(name).is_none())
            .collect::<Vec<_>>();

        let mut changed = Changed {
            visited: BTreeSet::new(),
            visited_names: HashSet::new(),
            touched,
            items,
            lost,
            redeclares,
        };
        let mut frontier = changed.lost.clone();
        for place in changed.items.clone() {
            changed.visit(definitions, place, &mut frontier);
        }
        // A declaration the baseline holds, now stored ahead of one it read, is no adoption a walk in the module's order makes: in its turn what it read is not settled yet, so it is asked, as that walk would ask it.
        for place in changed.touched.clone() {
            let reads_ahead = definitions.items.names(place).into_iter().any(|name| {
                baseline.certification.reads(name).is_some_and(|reads| {
                    reads
                        .signatures
                        .iter()
                        .chain(&reads.bodies)
                        .any(|read| places.place(read).is_some_and(|held| held.item() > place))
                })
            });
            if reads_ahead {
                changed.visit(definitions, place, &mut frontier);
            }
        }
        if !globals.settled() {
            let foreign = read_by
                .names()
                .filter(|name| {
                    places.place(name).is_none() && baseline.places.place(name).is_none()
                })
                .copied()
                .collect::<Vec<_>>();
            for name in foreign {
                for (body, readers) in [
                    (false, read_by.signature(&name)),
                    (true, read_by.body(&name)),
                ] {
                    if readers.is_empty() || read_stands(globals, baseline, abroad, &name, body) {
                        continue;
                    }
                    for reader in readers {
                        if let Some(place) = places.place(reader) {
                            changed.visit(definitions, place.item(), &mut frontier);
                        }
                    }
                }
            }
        }
        while let Some(name) = frontier.pop() {
            for reader in read_by.signature(&name).iter().chain(read_by.body(&name)) {
                if let Some(place) = places.place(reader) {
                    changed.visit(definitions, place.item(), &mut frontier);
                }
            }
        }
        changed
    }

    /// Visit the declaration at `place`, whole, and hand each of its names on to `frontier`, where it is not visited yet.
    fn visit(&mut self, definitions: &Definitions, place: usize, frontier: &mut Vec<Global>) {
        if !self.visited.insert(place) {
            return;
        }
        for name in definitions.items.names(place) {
            self.visited_names.insert(*name);
            frontier.push(*name);
        }
    }
}

/// Whether an item at one of `touched`, the places `module` holds another item at than `baseline` does, declares a name a unit in scope declares, or another item of the module does: another of those, or one the baseline holds at a place `changes` leaves as it was.
fn declares_a_held_name(
    definitions: &Definitions,
    globals: &Globals,
    baseline: Baseline<'_>,
    touched: &BTreeSet<usize>,
    changes: &Changes,
) -> bool {
    let changed = changes
        .replaced
        .iter()
        .chain(&changes.removed)
        .copied()
        .collect::<HashSet<_>>();
    let mut declared = HashSet::new();

    touched.iter().any(|place| {
        definitions.items.names(*place).into_iter().any(|name| {
            !declared.insert(*name)
                || globals.in_scope(name)
                || baseline
                    .places
                    .place(name)
                    .is_some_and(|held| !changed.contains(&held.item()))
        })
    })
}

/// Whether what the baseline's walk read of `name`, which the unit being judged does not declare, stands: a unit in scope declares it, and that unit is the one the walk read, or a record covers it and the part read, its body where `body` and its signature otherwise, digests now to what the baseline's record holds of it (`read_digest`). Nothing digests where nothing is archived, so there a read of a unit that moved does not stand. `asked` keeps each answer for the walk.
///
/// **A part of a unit no record covers does not stand.** Its digest holds no verdict or vector a walk concluded of it, the ones a walk reaches from its terms being this walk's own (`Totals::seeded`, `file_scope_polarities`), so it would digest alike under what the reading walk was given and under what this walk reaches.
fn read_stands(
    globals: &Globals,
    baseline: Baseline<'_>,
    asked: &RefCell<HashMap<(Global, bool), bool>>,
    name: &Global,
    body: bool,
) -> bool {
    if !globals.in_scope(name) {
        return false;
    }
    if globals.unmoved(name) {
        return true;
    }
    if !globals.classified(name) {
        return false;
    }
    if let Some(stands) = asked.borrow().get(&(*name, body)) {
        return *stands;
    }

    let stands = {
        let then = baseline
            .certification
            .abroad(name)
            .and_then(|read| match body {
                true => read.body.as_ref(),
                false => read.signature.as_ref(),
            });

        matches!(
            (then, read_digest(globals, name, body)),
            (Some(then), Ok(Some(now))) if *then == now
        )
    };
    asked.borrow_mut().insert((*name, body), stands);

    stands
}

/// What a walk has settled where it asks whether a record a store holds stands: its module, where the module holds each name, the baseline beneath it, the names settled so far, and the verdicts and vectors it holds of them.
struct Settled<'w> {
    /// The module as it arrived, whose items a store files its records by.
    definitions: &'w Definitions,
    /// The module as the walk has stated it so far, which a part of it is digested in.
    stated: &'w Definitions,
    places: &'w Places,
    baseline: Baseline<'w>,
    settled: &'w BTreeSet<Global>,
    totals: &'w Totals<'w>,
    polarities: &'w BTreeMap<Global, Vec<Polarity>>,
    variances: &'w BTreeMap<Global, Vec<Variance>>,
}

/// The records a store holds of a judgment of `item`, at `place`, by the item's identity, one for each name it declares, where every part each read digests now as it did ([`ReadRecord`]): `None` where some name has none that stands.
///
/// **Read by the certifier's own digest, of what is settled.** A part of a declaration of the walk's unit is digested as the walk settled it, or as the baseline holds it where the walk passed it unvisited; one not settled by now is one the order does not account for, and no record that read it stands. A part of another unit is digested where it stands, a fixed unit's is recorded nowhere, and no record that read a unit no record covers stands. A part of the item's own is read under the verdict the record holds of it, which is what taking it makes the walk's.
fn stored(
    candidates: &dyn Candidates,
    globals: &Globals,
    kernel: &Kernel,
    now: &Settled<'_>,
    place: usize,
    item: &Item,
) -> Option<Vec<(Global, HeldReads)>> {
    let names = item.declared_names();
    let identity = match now.definitions.items.part(place) {
        Some(stored) => stored.digest,
        None => ItemRun::identity(item).ok()?,
    };
    curios_profile::profile!("certify::candidates");
    let found = candidates
        .candidates(Facet::Judgment, &identity)
        .into_iter()
        .filter_map(|object| {
            let member = object
                .member
                .filter(|member| member.kind == ReadTable::KIND)?;
            let (name, reads, made) =
                ReadTable::member(object.bytes.as_slice(), member.index).ok()?;
            let made = made.filter(|made| {
                made.input == identity
                    && made.signatures.len() == reads.signatures.len()
                    && made.bodies.len() == reads.bodies.len()
            })?;

            Some((
                name,
                Rc::new(ReadRecord::restored(reads, Some(made), object.digest)),
            ))
        })
        .collect::<Vec<_>>();
    // What a record says of a name of the item's own is read under the verdict the first record of that name holds: where records disagree, a part read digests otherwise, and none stands.
    let mut own = HashMap::<Global, &Concluded>::new();
    for (name, record) in &found {
        if let Some(made) = record.made() {
            own.entry(*name).or_insert(made);
        }
    }

    names
        .iter()
        .map(|name| {
            found
                .iter()
                .find(|(found, record)| {
                    found == *name && stands(globals, kernel, now, &names, &own, record)
                })
                .map(|(name, record)| (*name, Rc::clone(record)))
        })
        .collect()
}

/// Whether every part `record` read digests now as it did. See [`stored`].
fn stands(
    globals: &Globals,
    kernel: &Kernel,
    now: &Settled<'_>,
    names: &[&Global],
    own: &HashMap<Global, &Concluded>,
    record: &ReadRecord,
) -> bool {
    let Some(made) = record.made() else {
        return false;
    };
    let digest = |read: &Global, body: bool| -> Option<Option<Option<[u8; 32]>>> {
        let unit = now.places.place(read).is_some();
        if unit && names.contains(&read) {
            let concluded = own.get(read)?;
            let verdict = (
                concluded.totality != Totality::Total,
                concluded
                    .vectors
                    .as_ref()
                    .map(|(polarities, _)| polarities.as_slice()),
                concluded
                    .vectors
                    .as_ref()
                    .map(|(_, variances)| variances.as_slice()),
            );
            return held_digest(now.stated, now.places, read, verdict, body)
                .ok()
                .map(|digest| Some(digest.as_deref().and_then(bytes_of)));
        }
        if unit {
            let verdict = match (now.settled.contains(read), kernel.passed(read)) {
                (true, _) => (
                    now.totals.partial(kernel, read),
                    now.polarities.get(read).map(Vec::as_slice),
                    now.variances.get(read).map(Vec::as_slice),
                ),
                (false, true) => (
                    now.baseline.certification.totality(read) != Some(Totality::Total),
                    now.baseline.certification.polarities(read),
                    now.baseline.certification.variances(read),
                ),
                (false, false) => return None,
            };
            return held_digest(now.stated, now.places, read, verdict, body)
                .ok()
                .map(|digest| Some(digest.as_deref().and_then(bytes_of)));
        }
        if globals.in_scope(read) && !globals.recorded(read) {
            return Some(None);
        }
        // A part of a unit no record covers holds no verdict or vector a walk concluded of it, as `read_stands` reads one.
        if globals.in_scope(read) && !globals.classified(read) {
            return None;
        }

        read_digest(globals, read, body)
            .ok()
            .map(|digest| digest.as_deref().map(bytes_of))
    };

    record
        .signatures
        .iter()
        .zip(&made.signatures)
        .all(|(read, recorded)| digest(read, false).is_some_and(|now| now == *recorded))
        && record
            .bodies
            .iter()
            .zip(&made.bodies)
            .all(|(read, recorded)| digest(read, true).is_some_and(|now| now == *recorded))
}

/// Take the verdict a store's records hold of `item`: what the walk states of it, as it states what it judges, and each name standing at its type with its real body, as the walk leaves an item it does not judge; what the walk refuses of it is handed back with it.
fn take(kernel: &mut Kernel, item: &Item) -> (Item, Vec<Verdict>) {
    let mut refusals = Vec::new();
    kernel.restore_budget();
    let stated = stated_marks(kernel, item);
    for (name, declaration) in carried(&stated).0 {
        kernel.restate_induct(&name, &declaration);
    }

    let stated = match stated {
        Item::Let(definition) => {
            let (proof, value_sort) = match state_definition(
                kernel,
                &definition.name,
                &definition.kind,
                &definition.type_,
                &definition.body,
                &definition.universe_context,
            ) {
                Ok(Stated { proof, value_sort }) => (proof, value_sort),
                Err(error) => {
                    refusals.push(Verdict {
                        name: Some(definition.name),
                        error,
                    });
                    (false, None)
                }
            };

            Item::Let(Box::new(Definition {
                proof,
                value_sort,
                ..*definition
            }))
        }
        Item::Rec(mut rec) => {
            let members = rec
                .definitions
                .iter()
                .map(|definition| (definition.name, &definition.kind))
                .collect::<Vec<_>>();
            let proofs = state_rec_group(kernel, &members, &rec.group, &rec.universe_context)
                .unwrap_or_else(|error| {
                    refusals.push(Verdict {
                        name: members.first().map(|(name, _)| *name),
                        error,
                    });
                    vec![false; members.len()]
                });
            for (definition, proof) in rec.definitions.iter_mut().zip(proofs) {
                definition.proof = proof;
            }

            Item::Rec(rec)
        }
    };

    (stated, refusals)
}

/// The item `baseline` holds where it holds the first name `item` declares: the one whose statements an item the walk does not judge takes.
fn held_item<'b>(baseline: Baseline<'b>, item: &Item) -> Option<&'b Item> {
    let name = item.declared_names().into_iter().next()?;
    let place = baseline.places.place(name)?;

    Some(&baseline.definitions.items[place.item()])
}

/// `item` at `place` of `definitions`, in place of what it held there and at the place it had: a version keeps sharing its other items with the module it was made from.
fn put(definitions: &mut Definitions, place: usize, item: Item) {
    match definitions.items.is_version() {
        true => definitions.items.replace(place, item),
        false => definitions.items[place] = item,
    }
}

/// What adopting one item's verdict files: the refusals met on the way, each name's reads as the baseline's record holds them, the names that record calls partial, and the polarity vector of each registry entry the item declares.
struct Adopted {
    refusals: Vec<Verdict>,
    reads: Vec<(Global, Reads)>,
    partial: Vec<Global>,
    vectors: Vec<(Global, Vec<Polarity>)>,
    variances: Vec<(Global, Vec<Variance>)>,
}

/// Take the baseline's verdict on `item`: each of its names stands as the walk leaves an item it does not judge, at its type with its real body, read where the baseline holds it; its registry entries' polarity vectors are filed in the environment; and what the baseline's record holds for it is handed back.
///
/// `certified` is the baseline's record of each name `item` declares, in the order it declares them ([`Adopting::adopts`]).
fn adopt(
    kernel: &mut Kernel,
    baseline: Baseline<'_>,
    item: &Item,
    certified: &[(Totality, &Reads)],
) -> Adopted {
    let names = item.declared_names();
    let mut adopted = Adopted {
        refusals: names
            .iter()
            .filter_map(|name| kernel.adopt(name).err())
            .map(|error| Verdict {
                name: names.first().map(|name| **name),
                error,
            })
            .collect(),
        reads: Vec::new(),
        partial: Vec::new(),
        vectors: Vec::new(),
        variances: Vec::new(),
    };

    for (name, (totality, reads)) in names.into_iter().zip(certified) {
        adopted.reads.push((*name, (*reads).clone()));
        if *totality != Totality::Total {
            adopted.partial.push(*name);
        }
        if item.nominal(name).is_some() {
            let vector = baseline
                .certification
                .polarities(name)
                .expect("an adopted declaration's registry entries were filed vectors")
                .to_vec();
            kernel.file_polarities(name, vector.clone());
            adopted.vectors.push((*name, vector));
            let vector = baseline
                .certification
                .variances(name)
                .expect("an adopted declaration's registry entries were filed vectors")
                .to_vec();
            kernel.file_variances(name, vector.clone());
            adopted.variances.push((*name, vector));
        }
    }

    adopted
}

/// The registry entries `item`'s formers carry, as the two maps a set of declarations is accepted as: a family's, and a record's, a structure's or the one a concept's witnesses are values of.
fn carried(item: &Item) -> (BTreeMap<Global, InductDecl>, BTreeMap<Global, StructDecl>) {
    let mut inducts = BTreeMap::new();
    let mut structs = BTreeMap::new();
    for (name, nominal) in item.nominals() {
        if let Some(declaration) = nominal.inductive() {
            inducts.insert(*name, declaration.clone());
        }
        if let Some(record) = nominal.record() {
            structs.insert(*name, record.clone());
        }
    }

    (inducts, structs)
}

/// The verdict a refused positivity analysis is reported as.
fn positivity_verdict(refusal: PositivityRefusal<Error>) -> Verdict {
    match refusal {
        PositivityRefusal::NotPositive(refusal) => Verdict {
            name: Some(refusal.name),
            error: Error::NotPositive {
                name: refusal.name,
                part: refusal.part,
                polarity: refusal.polarity,
            },
        },
        PositivityRefusal::Exhausted { name, error } => Verdict {
            name: Some(name),
            error,
        },
    }
}

/// Compute and file the polarity vector of every registry entry in scope that no record files one for: the entries of a unit mounted without a record a walk made of it. Analyzed together, since nothing says which of them mention which, and before anything is judged, since a judged family composes through them.
fn file_scope_polarities(kernel: &mut Kernel, verdicts: &mut Vec<Verdict>) {
    let (inducts, structs) = kernel.unvectored();
    if inducts.is_empty() && structs.is_empty() {
        return;
    }

    kernel.restore_budget();
    match positivity_vectors(kernel, Declarations::of(&inducts, &structs)) {
        Ok(vectors) => {
            for (name, vector) in vectors {
                kernel.file_polarities(&name, vector);
            }
        }
        Err(refusal) => verdicts.push(positivity_verdict(refusal)),
    }
    // What this read is no judged item's.
    kernel.take_reads();
}

/// Compute and file the variance vector of every registry entry in scope that no record files one for, as [`file_scope_polarities`] files their polarity vectors, and before anything is judged, since a comparison of two instances reads one. A walk the budget cuts short files nothing, and its entries answer invariant.
fn file_scope_variances(kernel: &mut Kernel, verdicts: &mut Vec<Verdict>) {
    let (inducts, structs) = kernel.unvaried();
    if inducts.is_empty() && structs.is_empty() {
        return;
    }

    kernel.restore_budget();
    let found = variance_vectors(kernel, Declarations::of(&inducts, &structs));
    match found.exhausted {
        Some((walked, error)) => verdicts.push(Verdict {
            name: Some(walked),
            error,
        }),
        None => {
            for (name, vector) in found.vectors {
                kernel.file_variances(&name, vector);
            }
        }
    }
    // What this read is no judged item's.
    kernel.take_reads();
}

/// Accept the registry entries one declaration declares: their strict positivity, their variance and their sizing, each a judgment of its own on a budget restored for it, so whether one fits is a fact about these entries and never about what the module's others cost.
///
/// **Over these entries alone.** A group's formers are one item, so the families that mention one another arrive together; every other family the telescopes mention belongs to a declaration judged before this one — the item follows what its entries reach — or to a unit in scope, and answers from what the kernel holds of it: the vectors it filed for strict positivity and for variance.
///
/// **Strict positivity** is recomputed here and filed, in the environment for the declarations judged after this one and in `polarities` for the walk's record; the vector an entry carries is elaboration's and is read by nothing. An entry the analysis refuses is filed no vector, and answers `Mixed` from then on.
///
/// **Variance** is computed here and filed, as strict positivity is: in the environment, where every comparison of two instances judged after this one reads it, and in `variances` for the walk's record. The vector an entry carries is elaboration's, which the elaborator of a later unit compares by, so an irrelevance it claims past the computed one refuses the entry, and a carried invariance the walk would have granted is denied nothing. A walk the budget cut short files nothing and is reported as the budget's. A family of a unit in scope answers from the vector its unit's record files.
///
/// **Sizing** — a literal result sort, sized constructors, each index target typed against the arity — is the clause the item's typing cannot supply, because it computes each signature's sort and compares it to nothing.
///
/// What the three read joins what typing each former did: a registry entry is accepted as part of its type former, which every declaration has under its own name.
fn accept(
    kernel: &mut Kernel,
    inducts: &BTreeMap<Global, InductDecl>,
    structs: &BTreeMap<Global, StructDecl>,
    verdicts: &mut Vec<Verdict>,
    reads: &mut BTreeMap<Global, Reads>,
    polarities: &mut BTreeMap<Global, Vec<Polarity>>,
    variances: &mut BTreeMap<Global, Vec<Variance>>,
) {
    if inducts.is_empty() && structs.is_empty() {
        return;
    }

    kernel.restore_budget();
    match positivity_vectors(kernel, Declarations::of(inducts, structs)) {
        Ok(vectors) => {
            for (name, vector) in vectors {
                kernel.file_polarities(&name, vector.clone());
                polarities.insert(name, vector);
            }
        }
        Err(refusal) => verdicts.push(positivity_verdict(refusal)),
    }

    kernel.restore_budget();
    let recomputed = variance_vectors(kernel, Declarations::of(inducts, structs));
    match recomputed.exhausted {
        // A refused reduction read its term as invariant throughout, so what the walk computed past it may be the budget's: nothing is filed, the entries answer invariant from then on, and the budget is what is reported.
        Some((walked, error)) => verdicts.push(Verdict {
            name: Some(walked),
            error,
        }),
        None => {
            let carried = inducts
                .iter()
                .map(|(name, declaration)| (name, &declaration.variances))
                .chain(
                    structs
                        .iter()
                        .map(|(name, declaration)| (name, &declaration.variances)),
                );
            // The carried vector is the one the elaborator of a later unit reads, so an irrelevance it claims past the walk's own is refused where it is declared.
            for (name, carried) in carried {
                let granted = recomputed.vectors.get(name);
                let denied = carried.iter().enumerate().find(|(level, variance)| {
                    **variance == Variance::Irrelevant
                        && granted.and_then(|vector| vector.get(*level))
                            != Some(&Variance::Irrelevant)
                });
                if let Some((level, _)) = denied {
                    verdicts.push(Verdict {
                        name: Some(*name),
                        error: Error::VarianceDenied { name: *name, level },
                    });
                }
            }
            for (name, vector) in recomputed.vectors {
                kernel.file_variances(&name, vector.clone());
                variances.insert(name, vector);
            }
        }
    }

    kernel.restore_budget();
    for (name, declaration) in inducts {
        if let Err(error) = check_induct_decl(kernel, declaration) {
            verdicts.push(Verdict {
                name: Some(*name),
                error,
            });
        }
    }
    for (name, declaration) in structs {
        if let Err(error) = check_struct_decl(kernel, declaration) {
            verdicts.push(Verdict {
                name: Some(*name),
                error,
            });
        }
    }

    let read = kernel.take_reads();
    for name in inducts.keys().chain(structs.keys()) {
        if let Some(reads) = reads.get_mut(name) {
            reads.extend(read.clone());
        }
    }
}
