//! Running the independent kernel over a whole module.
//!
//! This is the seam where the second opinion is actually asked for. Everything upstream — elaboration, unification, zonking, witness resolution — has already decided the module is well-typed; [`recheck_module`] decides again from the terms alone, and it lives in this crate so that deciding is something `cargo tree -p curios-cert` can account for.
//!
//! # Reading a disagreement
//!
//! A refusal here is *not* automatically an elaborator bug, and treating it as one would be the wrong reflex. The kernel is deliberately incomplete in one place — conversion compares a stuck elimination's scrutinee, motive and arms, and a projection's or an instance's head, at `Type` rather than at their real types, forfeiting type-directed eta and goal-level irrelevance there — and that refuses valid programs. So a disagreement is a question, and the two answers are "the kernel needs strengthening here" and "the elaborator admitted something it should not have". Both are worth knowing, which is why this runs at all.
//!
//! What a disagreement is *never* is noise to be suppressed. If a rule here has to be weakened to make a real module pass, that weakening is a decision about the trusted base and belongs in a decision file of its own under `documentation/design/soundness/`, beside `an-independent-kernel-re-checks-what-the-elaborator-accepts.md`.
//!
//! # On the compile path
//!
//! Every compilation calls this. `curios-pipeline` certifies each unit it compiles — [`certify_module`] for a unit, [`certify_module_over`] for one compiled over a baseline, [`certify_program`] for the entry — with the units in scope as its [`Globals`], and fails the build on a refusal, judging the unit's own items while the scope's are taken on the word of the walk that certified each of them: a unit is certified by construction, and the prelude's word is `curios-prelude`'s build script, which runs the same walk over each root, against the roots before it, and fails the build on any refusal. An archive that exists is one whose every item the kernel accepted.
//!
//! What is *not* re-judged is the environment, and nothing of it is handed in again. Before anything is judged the walk refuses a module for every name it declares a second time — a name any namespace of the environment holds, whatever the module declares under it, and a second item under one name — whole, as [`Error::Redeclared`], because a name is one declaration and every pass below files by name. So every declaration a module carries is one this walk judges, or one whose verdict it adopts from a baseline. Nothing reads a length, and nothing requires the environment's units to sit anywhere in particular — which is the difference between an environment and a prefix.

#[cfg(test)]
mod adoption_tests;
#[cfg(test)]
mod budget_tests;
#[cfg(test)]
mod elimination_tests;
#[cfg(test)]
mod entry_tests;
#[cfg(test)]
mod foreign_tests;
#[cfg(test)]
mod identity_tests;
#[cfg(test)]
mod occurrence_tests;
#[cfg(test)]
mod plicity_tests;
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
mod test_support;
#[cfg(test)]
mod totality_tests;
#[cfg(test)]
mod universes_tests;

use {
    super::{
        Adopting, Baseline, Error, Globals, Kernel, Totals, check_definition, check_entrypoint,
        check_induct_decl, check_positions, check_rec_group, check_struct_decl, define_as_stated,
        free_locals_outside, positivity_vectors, rewrite_universe_levels_scoped_shared,
        satisfiable, universe_metas, variance_vectors,
    },
    curios_analysis::{Declarations, Erased, PositivityRefusal},
    curios_core::{
        Bound, Certification, Certified, Entrypoint, Free, Global, InductDecl, Item, Level,
        MetavarId, Module, Polarity, Program, Reads, StructDecl, Term, Totality, UniverseContext,
        Variance, Zonked,
    },
    curios_utilities::{SyntaxRegistry, grown},
    std::collections::{BTreeMap, BTreeSet, HashMap, HashSet},
};

/// One refusal the kernel reached.
///
/// **Not one per item, and not always an item.** A declaration is judged several ways, so one failing two of them yields two verdicts; and a registry entry is refused under its own name, which is no item of [`Module::items`]. A caller counting these against `items.len()` is comparing two different rosters.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Verdict {
    /// What failed: an item, or the registry entry a declaration pass refused — a recursive group is named by its first member, since a group is checked and refused as a unit. `None` is the entrypoint expression, which has no name to export.
    pub name: Option<Global>,
    pub error: Error,
}

/// What one whole-module walk reached: every refusal, and the record of what it concluded about the definitions it judged.
#[derive(Debug)]
pub struct Rechecked {
    /// Every refusal the walk reached; empty exactly when the kernel accepted the module.
    pub verdicts: Vec<Verdict>,
    /// Each judged definition's totality, closed over everything it mentions, and each accepted registry entry's polarity vector — what a later walk reads once the unit is in its scope. A definition the walk refused is classified all the same, since the walk defines it anyway; a unit carrying a refusal is never filed, so no record of one is ever read.
    pub certification: Certification,
}

/// Re-check `module` with the independent kernel, with `globals` already in scope.
///
/// `budget` is the reduction allowance each item gets, the same figure the elaborator's own `Context` is built with. [`Globals::default()`] is the whole-module walk: nothing in scope, so every item is judged.
///
/// The [`Zonked`] input is interface-level evidence, not a delegation: the kernel keeps its own metavariable refusals, so nothing here trusts the wrapper's constructor.
pub fn recheck_module(
    module: &Zonked<Module>,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Result<(), Error> {
    match recheck_module_verdicts(module, budget, globals, syntax)
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
/// Nothing else survives an item. [`Kernel`]'s memos are cleared at every declaration boundary, its conversion history is built fresh per comparison, and every binder it opens is retracted on the failing path as well as the succeeding one. So recovery here is exact rather than approximate, and a verdict late in the list is worth as much as the first.
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
pub fn recheck_module_verdicts(
    module: &Zonked<Module>,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Vec<Verdict> {
    certify_module(module, budget, globals, syntax).verdicts
}

/// [`recheck_module_verdicts`], with the record the walk leaves of what it concluded — see [`Rechecked`].
pub fn certify_module(
    module: &Zonked<Module>,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Rechecked {
    verdicts_from(
        Kernel::new(budget, syntax),
        module.as_module(),
        None,
        globals,
    )
}

/// [`certify_module`] for a unit compiled over a baseline: the walk is handed the earlier walk's module and record whole and adopts each of its verdicts that still stands, judging every other declaration — see [`Baseline`]. Which declarations those are is this walk's to decide, from the reads the earlier one filed, and no caller's.
pub fn certify_module_over(
    module: &Zonked<Module>,
    budget: u64,
    globals: &Globals,
    baseline: Baseline<'_>,
    syntax: SyntaxRegistry,
) -> Rechecked {
    let mut kernel = Kernel::new(budget, syntax);

    verdicts_into(
        &mut kernel,
        module.as_module(),
        None,
        globals,
        Some(baseline),
    )
}

/// [`certify_module`] for a program: its module walked as any unit's, and the term it closes with checked against the type it states.
pub fn certify_program(
    program: &Zonked<Program>,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Rechecked {
    let program = program.as_program();

    verdicts_from(
        Kernel::new(budget, syntax),
        &program.module,
        Some(&program.entry),
        globals,
    )
}

/// [`recheck_module_verdicts`] with the kernel's evaluation memos off.
///
/// Exists for one purpose: asserting that memoization changes no *semantic* verdict — the property that makes a memo an evaluation strategy rather than a store. A resource verdict may differ, since a term-keyed hit is free.
pub fn recheck_module_verdicts_uncached(
    module: &Zonked<Module>,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Vec<Verdict> {
    verdicts_from(
        Kernel::uncached(budget, syntax),
        module.as_module(),
        None,
        globals,
    )
    .verdicts
}

/// What a whole-module walk consumed, beside what it reached — the walk's own kernel, handed back for a measurement to read.
///
/// Exists for one purpose too: `DEFAULT_STEP_BUDGET` has to be set against what a real module actually costs, and nothing else can see that figure — the kernel a walk builds is otherwise its own. [`Kernel::heaviest_declaration`] is the heaviest single judgment it made. The record is there for a measurement walking several units in order, which mounts each with it as a compilation does, so the next walk reads the classification rather than deriving it at a cost no compilation pays.
///
/// It hands back the kernel rather than a tuple of figures so that asking a new question of a finished walk costs a reader rather than a signature. A measurement's entry point, never a control; nothing in the compiler reads what it returns.
pub fn recheck_module_measured<'s>(
    module: &Zonked<Module>,
    budget: u64,
    globals: &'s Globals<'s>,
    syntax: SyntaxRegistry,
) -> (Rechecked, Kernel<'s>) {
    let mut kernel = Kernel::new(budget, syntax);
    let rechecked = verdicts_into(&mut kernel, module.as_module(), None, globals, None);

    (rechecked, kernel)
}

/// [`recheck_module_measured`] for a program — [`certify_program`]'s walk, with its kernel handed back.
pub fn recheck_program_measured<'s>(
    program: &Zonked<Program>,
    budget: u64,
    globals: &'s Globals<'s>,
    syntax: SyntaxRegistry,
) -> (Rechecked, Kernel<'s>) {
    let program = program.as_program();
    let mut kernel = Kernel::new(budget, syntax);
    let rechecked = verdicts_into(
        &mut kernel,
        &program.module,
        Some(&program.entry),
        globals,
        None,
    );

    (rechecked, kernel)
}

/// The first metavariable an `induct` registry entry carries, in the order the entry stores its parts.
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
fn universe_residue<B: Bound>(value: &B) -> Option<Error> {
    universe_metas(value)
        .into_iter()
        .next()
        .map(|meta| Error::NotCore(Term::type_at(Level::meta(meta))))
}

/// The first universe parameter one of `value`'s levels names that a scheme of `parameter_count` parameters does not have.
///
/// A declaration's universe scheme promises that every level it mentions is ground or one of the parameters it declares, so a use site fully determines it. [`universe_residue`] above checks half of that promise — no unsolved metavariable — and this checks the other half: no parameter index past the declaration's own count. `closed` sees a `UniverseContext`'s constraints and not a level sitting in a term, so the two are the same omission at different depths.
///
/// It is a soundness rule rather than a tidiness one because of what instantiation does with an out-of-range index: `instantiate_universe_levels_scoped` substitutes what the instance supplies and *renumbers* the rest down by the instance's width. For a well-scoped term that is the correct de Bruijn shift, since an index at or above the width names an enclosing binder. For an ill-scoped one it is a capture — `Type.{param 1}` and `Type.{param 0}` both instantiate at `[param 0]` to the same level — so two distinct levels become one and every cumulativity question after that is answered about the wrong one.
///
/// Scoped rather than flat: a nested scheme binds its own parameters innermost, so the bound at any point is the enclosing binder depth plus the declaration's count, exactly as `curios-elab`'s `validate_bound_universes` computes it. Metavariables are left to [`universe_residue`] so the two diagnostics stay distinct.
fn universe_escape<B: Bound>(value: &B, parameter_count: usize) -> Option<Error> {
    rewrite_universe_levels_scoped_shared(value, move |depth, level| {
        let visible = depth.checked_add(parameter_count).ok_or(())?;
        match level.params().any(|param| param.0 >= visible) {
            true => Err(()),
            false => Ok(level.clone()),
        }
    })
    .err()
    .map(|()| Error::UnclosedUniverses)
}

/// Every kind of elaboration residue an `induct` registry entry can carry.
fn induct_residue(declaration: &InductDecl) -> Option<Error> {
    induct_metavar(declaration)
        .map(|id| Error::NotCore(Term::hole(id)))
        .or_else(|| universe_residue(&declaration.arity))
        .or_else(|| universe_residue(&declaration.result_sort))
        .or_else(|| {
            declaration
                .signatures()
                .find_map(|constructor| universe_residue(&constructor.telescope))
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
    struct_metavar(declaration)
        .map(|id| Error::NotCore(Term::hole(id)))
        .or_else(|| universe_residue(&declaration.arity))
        .or_else(|| universe_residue(&declaration.result_sort))
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
fn redeclarations(module: &Module, globals: &Globals) -> Vec<Verdict> {
    curios_profile::profile!("redeclarations");
    let mut refused: BTreeSet<Global> = BTreeSet::new();
    let mut redeclared = Vec::new();
    let mut refuse = |name: &Global| {
        if refused.insert(*name) {
            redeclared.push(Verdict {
                name: Some(*name),
                error: Error::Redeclared(Free::from(name)),
            });
        }
    };

    let mut declared: HashSet<&Global> = HashSet::new();
    for item in &module.items {
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
    module: &Module,
    entry: Option<&Entrypoint>,
    globals: &'s Globals<'s>,
) -> Rechecked {
    verdicts_into(&mut kernel, module, entry, globals, None)
}

fn verdicts_into<'s>(
    kernel: &mut Kernel<'s>,
    module: &Module,
    entry: Option<&Entrypoint>,
    globals: &'s Globals<'s>,
    baseline: Option<Baseline<'s>>,
) -> Rechecked {
    grown(|| verdicts_within(kernel, module, entry, globals, baseline))
}

/// The walk: `module`'s items, then — for a program — the term `entry` closes it with. Over a `baseline`, each item whose verdict stands is adopted in its turn and every other is judged ([`Baseline`]).
fn verdicts_within<'s>(
    kernel: &mut Kernel<'s>,
    module: &Module,
    entry: Option<&Entrypoint>,
    globals: &'s Globals<'s>,
    baseline: Option<Baseline<'s>>,
) -> Rechecked {
    curios_profile::profile!("recheck_module");
    // A module that declares a name twice, its environment's among them, is refused here, whole, before anything of it is judged.
    let redeclared = redeclarations(module, globals);
    if !redeclared.is_empty() {
        return Rechecked {
            verdicts: redeclared,
            certification: Certification::default(),
        };
    }
    let mut verdicts = Vec::new();

    // The declarations whose terms are the baseline's: which of the module's items they are, by place, and the names they declare. The walk that filed the baseline held those terms to every rule a declaration's own terms decide — its universe contexts, the levels and binders it mentions, elaboration's residue — so the boundary passes below judge the rest, whether or not a verdict is adopted.
    let mut adopting = baseline.map(Adopting::over);
    // The registry entries the module's formers carry, each under the name that declares it: read off the items once, for the walk.
    let (inducts, structs) = (module.induct_decls(), module.struct_decls());
    let mut kept = vec![false; module.items.len()];
    let mut unchanged: HashSet<Global> = HashSet::new();
    if let Some(baseline) = baseline {
        for (index, item) in module.items.iter().enumerate() {
            if baseline.holds(item) {
                kept[index] = true;
                unchanged.extend(item.declared_names().into_iter().copied());
            }
        }
    }
    let changed = || {
        module
            .items
            .iter()
            .enumerate()
            .filter(|(index, _)| !kept[*index])
            .map(|(_, item)| item)
    };
    let changed_entries = |names: &mut dyn Iterator<Item = &Global>| {
        names
            .filter(|name| !unchanged.contains(*name))
            .copied()
            .collect::<BTreeSet<_>>()
    };
    let changed_inducts = changed_entries(&mut inducts.keys());
    let changed_structs = changed_entries(&mut structs.keys());

    // The environment is the kernel's starting scope, not something consulted beside it: an item already in scope stands exactly as a refused item does — at its declared type, with its real body — so every judged item downstream sees what a walk that judged everything would have shown it. A baseline is beneath the walk as the scope is, answering for the declarations whose verdict the walk adopts.
    kernel.seed(globals);
    if let Some(baseline) = baseline {
        kernel.over(baseline);
    }

    // The kernel mints its binders from zero, so a free local in a term it judges is one it could mint again while comparing under a telescope or eta-contracting — a capture, after which the judgment sees a bound variable and never an unbound one. So a local is refused here, at the boundary, over every position a term can sit in but those of the declarations the baseline holds term for term, which the walk that filed the baseline held to this rule.
    for (owner, local) in free_locals_outside(module, |name| unchanged.contains(name)) {
        verdicts.push(Verdict {
            name: owner,
            error: Error::Unbound(local),
        });
    }
    if let Some(entry) = entry {
        for local in entry.free_locals() {
            verdicts.push(Verdict {
                name: None,
                error: Error::Unbound(local),
            });
        }
    }

    // Every universe context this walk will assume, decided before any of it is assumed. An unsatisfiable set makes `entails` answer anything, so a later refusal would be reported against whichever item happened to ask a level question first rather than against the declaration that carries the contradiction.
    let contexts = entries_among(&inducts, &changed_inducts)
        .map(|(name, declaration)| (*name, &declaration.universe_context))
        .chain(
            entries_among(&structs, &changed_structs)
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
    for item in changed() {
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
    for (name, declaration) in entries_among(&inducts, &changed_inducts) {
        if let Some(error) = induct_residue(declaration) {
            verdicts.push(Verdict {
                name: Some(*name),
                error,
            });
        }
    }
    for (name, declaration) in entries_among(&structs, &changed_structs) {
        if let Some(error) = struct_residue(declaration) {
            verdicts.push(Verdict {
                name: Some(*name),
                error,
            });
        }
    }
    // An item's own terms are walked, so a `Metavar` node in one is refused where a judgment meets it. A level is not: the walk types `Type(?u)` without objecting, so the same boundary decides it here.
    for item in changed() {
        for definition in item.definitions() {
            let count = definition.universe_context.parameter_count;
            if let Some(error) = universe_residue(&definition.type_)
                .or_else(|| universe_residue(&definition.body))
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
    // The module's own entry stands under no scheme, so every parameter index in it escapes.
    if let Some(entry) = entry
        && let Some(error) = entry
            .type_
            .as_ref()
            .and_then(universe_residue)
            .or_else(|| universe_residue(&entry.body))
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

    // The nominal registry first: a definition's type may name any declaration in the module, including one whose own definitions come later. The environment refuses a name it holds, which the refusal above rules out for every write this walk makes; a refusal is reported all the same, being the one thing a walk makes of a write it cannot perform.
    for (name, declaration) in &inducts {
        if let Err(error) = kernel.declare_induct(name, declaration) {
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

    // What each judged definition read of other items, taken at each judgment's end so it holds that judgment's reads alone.
    let mut reads: BTreeMap<Global, Reads> = BTreeMap::new();
    kernel.take_reads();
    // The definitions not known to terminate, as the walk comes to know them: the environment's, then each item's as it is judged.
    let mut totals = Totals::seeded(kernel, globals);
    let mut local_memo = HashMap::new();
    // The vector the walk filed for each registry entry it accepted.
    let mut polarities: BTreeMap<Global, Vec<Polarity>> = BTreeMap::new();

    // The items as the module stores them, which is every item after the items it reaches where the module was made by the elaborator (`Module::ordered`). The walk sorts nothing: an item that names one stored after it finds the name unbound, and a family it stores in one accepted after it finds no vector filed, each of which refuses.
    for (index, item) in module.items.iter().enumerate() {
        let item_name = item.declared_names().first().map(|&name| *name);

        // A declaration whose terms are the baseline's and whose every recorded read stands takes the baseline's verdict, and stands as the walk leaves any item it does not judge.
        if let Some(adopting) = &mut adopting
            && kept[index]
            && let Some(certified) = adopting.adopts(item, |name| globals.in_scope(name))
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
            adopting.adopted(item);
            continue;
        }

        // One span per judged item, grouped as the elaborator's `declaration` span is, so an item's cost in the kernel is read beside its cost in the elaborator by the same key.
        curios_profile::profile!("certify_declaration", group = %item.describe());

        match item {
            Item::Let(definition) => {
                let before = kernel.partial_groups();
                let outcome = check_definition(
                    kernel,
                    &Free::from(&definition.name),
                    &definition.type_,
                    &definition.body,
                    definition.proof,
                    &definition.universe_context,
                );
                // Whether the definition holds a group that does not descend, noted where the group was typed — see `Kernel::definition_encloses_partial`.
                if kernel.partial_groups() > before {
                    kernel.note_enclosing_partial(&Free::from(&definition.name));
                }

                if let Err(error) = outcome {
                    verdicts.push(Verdict {
                        name: Some(definition.name),
                        error,
                    });
                    if let Err(error) = define_as_stated(
                        kernel,
                        &Free::from(&definition.name),
                        &definition.type_,
                        &definition.body,
                        definition.proof,
                        &definition.universe_context,
                    ) {
                        verdicts.push(Verdict {
                            name: Some(definition.name),
                            error,
                        });
                    }
                }
            }
            Item::Rec(rec) => {
                let members = rec
                    .definitions
                    .iter()
                    .map(|definition| (Free::from(&definition.name), definition.proof))
                    .collect::<Vec<_>>();
                let universes = rec.group.universe_context().clone();

                let outcome = check_rec_group(kernel, &members, &rec.group, &universes);

                if let Err(error) = outcome {
                    verdicts.push(Verdict {
                        name: item_name,
                        error,
                    });
                    // The define `check_rec_group` performs on success: each export is the folded selection of the member it names.
                    for (member, (name, proof)) in members.iter().enumerate() {
                        if let Err(error) = define_as_stated(
                            kernel,
                            name,
                            &rec.group.member_type(member),
                            &Term::rec_proj(rec.group.clone(), member),
                            *proof,
                            &universes,
                        ) {
                            verdicts.push(Verdict {
                                name: item_name,
                                error,
                            });
                        }
                    }
                }
            }
        }

        // A group is judged as one item, so each of its members read what the group did.
        let item_reads = kernel.take_reads();
        for name in item.declared_names() {
            reads.insert(*name, item_reads.clone());
        }

        let (drained, failure) = kernel.take_checked();
        if let Some(error) = failure {
            verdicts.push(Verdict {
                name: item_name,
                error,
            });
        }

        // The item's classification, then obligations (T) and (V) over the positions its check recorded. Every definition it mentions was judged before it and is classified already, so the set its positions are read against is closed where they are read.
        for (name, error) in totals.classify(kernel, item) {
            verdicts.push(Verdict {
                name: Some(name),
                error,
            });
        }
        if let Err(error) = check_positions(&drained, &totals, &mut local_memo) {
            verdicts.push(Verdict {
                name: item_name,
                error,
            });
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
        );

        // How the item came out against the baseline's, for the declarations that read it.
        if let Some(adopting) = &mut adopting {
            adopting.judged(item, |name| totals.partial(name), &polarities);
        }
    }

    // A unit with no entrypoint has nothing here to judge — being the entry is what having one *means*, and a scope unit is not it.
    if let Some(entry) = entry {
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
    if let Err(error) = check_positions(&drained, &totals, &mut local_memo) {
        verdicts.push(Verdict { name: None, error });
    }

    // The record of what this walk concluded: every judged definition's totality, from the set each was classified into, with what its judgment read, and every accepted entry's polarity vector — so a later walk reading it needs nothing of this one's.
    let certification = Certification::of(
        module
            .items
            .iter()
            .flat_map(Item::declared_names)
            .map(|name| {
                let totality = match totals.partial(name) {
                    true => Totality::Partial,
                    false => Totality::Total,
                };
                let reads = reads.remove(name).unwrap_or_default();
                (*name, Certified { totality, reads })
            }),
        polarities,
    );

    Rechecked {
        verdicts,
        certification,
    }
}

/// What adopting one item's verdict files: the refusals met on the way, each name's reads as the baseline's record holds them, the names that record calls partial, and the polarity vector of each registry entry the item declares.
struct Adopted {
    refusals: Vec<Verdict>,
    reads: Vec<(Global, Reads)>,
    partial: Vec<Global>,
    vectors: Vec<(Global, Vec<Polarity>)>,
}

/// Take the baseline's verdict on `item`: each of its names stands as the walk leaves an item it does not judge, at its type with its real body, read where the baseline holds it; its registry entries' polarity vectors are filed in the environment; and what the baseline's record holds for it is handed back. A stamp that says `Total` where the adopted verdict does not is refused as it is for a judged item, the stamp being this text's and the verdict the baseline's.
///
/// `certified` is the baseline's record of each name `item` declares, in the order it declares them ([`Adopting::adopts`]).
fn adopt(
    kernel: &mut Kernel,
    baseline: Baseline<'_>,
    item: &Item,
    certified: &[(Totality, &Reads)],
) -> Adopted {
    let names = item.declared_names();
    let stamps = match item {
        Item::Let(definition) => vec![definition.totality],
        Item::Rec(rec) => rec
            .definitions
            .iter()
            .map(|definition| definition.totality)
            .collect(),
    };
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
    };

    for ((name, stamp), (totality, reads)) in names.into_iter().zip(stamps).zip(certified) {
        adopted.reads.push((*name, (*reads).clone()));
        if *totality != Totality::Total {
            adopted.partial.push(*name);
            if stamp.is_total() {
                adopted.refusals.push(Verdict {
                    name: Some(*name),
                    error: Error::NotTotal {
                        erased: Erased::Proof,
                        reached: None,
                    },
                });
            }
        }
        if item.nominal(name).is_some() {
            let vector = baseline
                .certification
                .polarities(name)
                .expect("an adopted declaration's registry entries were filed vectors")
                .to_vec();
            kernel.file_polarities(name, vector.clone());
            adopted.vectors.push((*name, vector));
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

/// Accept the registry entries one declaration declares: their strict positivity, their variance and their sizing, each a judgment of its own on a budget restored for it, so whether one fits is a fact about these entries and never about what the module's others cost.
///
/// **Over these entries alone.** A group's formers are one item, so the families that mention one another arrive together; every other family the telescopes mention belongs to a declaration judged before this one — the item follows what its entries reach — or to a unit in scope, and answers from what the kernel holds of it: the vector it filed for strict positivity, the vector the walk reconciled for variance.
///
/// **Strict positivity** is recomputed here and filed, in the environment for the declarations judged after this one and in `polarities` for the walk's record; the vector an entry carries is elaboration's and is read by nothing. An entry the analysis refuses is filed no vector, and answers `Mixed` from then on.
///
/// **Variance** is reconciled as it is decided: the item's own check compared instances on each carried vector's word, so the vectors are recomputed, and a carried irrelevance the recomputation denies refuses the entry. A carried invariance it would have granted costs an acceptance and admits nothing. A family of a unit in scope answers from the vector it carries, which was held to its own declaration when that unit was certified.
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
    let mut exhausted = recomputed.exhausted;
    let carried = inducts
        .iter()
        .map(|(name, declaration)| (name, &declaration.variances))
        .chain(
            structs
                .iter()
                .map(|(name, declaration)| (name, &declaration.variances)),
        );
    for (name, carried) in carried {
        let granted = recomputed.vectors.get(name);
        let denied = carried.iter().enumerate().find(|(level, variance)| {
            **variance == Variance::Irrelevant
                && granted.and_then(|vector| vector.get(*level)) != Some(&Variance::Irrelevant)
        });
        let Some((level, _)) = denied else {
            continue;
        };
        // A refused reduction read its term as invariant throughout, so the denial may be the budget's alone, and the budget is what is reported.
        verdicts.push(match exhausted.take() {
            Some((walked, error)) => Verdict {
                name: Some(walked),
                error,
            },
            None => Verdict {
                name: Some(*name),
                error: Error::VarianceDenied { name: *name, level },
            },
        });
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

/// What is wrong with a universe context the walk is about to assume, if anything.
///
/// Closure first: a context naming what it does not declare cannot be instantiated, so asking whether it has a solution would be asking about nothing.
fn universe_verdict(context: &UniverseContext) -> Option<Error> {
    curios_profile::profile!("universe_verdict");
    if !context.is_closed() {
        return Some(Error::UnclosedUniverses);
    }
    if !satisfiable(&context.constraints) {
        return Some(Error::UnsatisfiableUniverses);
    }

    None
}
