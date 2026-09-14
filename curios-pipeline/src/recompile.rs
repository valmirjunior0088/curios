//! One unit compiled over a baseline: the items its new text changed, closed over what the baseline's elaboration reached, elaborated and judged alone, with everything outside the closure replayed from the baseline as a predecessor's items are.
//!
//! **A baseline is a [`Unit`] compiled from an earlier text of the same sources**, handed in by whatever the fold consults; nothing here knows where it came from. What a reused item rests on is the verdict recorded when the baseline was judged, exactly as a whole unit taken from a store does, and the argument is [Cached verdicts](../../documentation/soundness/admission-without-judgment/cached-verdicts.md) applied per item: the key is the item's lowered form together with everything its reach closure covers. That is why the closure is transitive — conversion unfolds bodies, so a dependent's judgment reaches through what it names, and a closure that stopped one edge short would be a key omitting an input.
//!
//! **The diff is over lowered items, modulo what a lowering mints.** A lowering numbers every written `Type` and every elided annotation in lowering order across the unit, so the same declaration lowered after different neighbours carries different ids at the same positions; [`Term::equal_modulo_metas`] identifies them by position instead. Everything else compares exactly — spans and binder names excepted, as always — so a moved declaration is not a change and a renamed parameter is not one either.

use {
    crate::{CompileError, globals, kernel_refusal, with_broken},
    curios_cert::{Verdict, recheck_module_verdicts},
    curios_core::{
        Bound, ConceptDecl, Global, InductDecl, Item, MetaRenaming, Module, StructDecl, Telescope,
        Term, Zonked, derived_binder_floor,
    },
    curios_elab::{
        Context, Established, Mode, Recompile, Resumed, Tail, elaborate_and_zonk_unit_over,
        erase_unit,
    },
    curios_text::{UnitSource, into_core_unit},
    curios_unit::{Prefix, Unit},
    curios_utilities::SyntaxRegistry,
    std::collections::{BTreeMap, BTreeSet},
};

/// Compile one unit against `scope` over `baseline`: lower whole, diff, elaborate and judge the closure, erase whole.
///
/// The result is a unit like [`compile_unit`](crate::compile_unit)'s — the same lowering, a core module holding every item in the lowering's order, the same erasure onto the scope's arena — differing in which items were elaborated and judged now and which were taken from the baseline. Erasure is whole because the arena is the prefix's and appends: what an incremental erasure would save is a later question.
pub fn compile_unit_over(
    budget: u64,
    scope: Prefix<'_>,
    syntax: &SyntaxRegistry,
    source: &UnitSource<'_>,
    baseline: &Unit,
) -> Result<Unit, CompileError> {
    curios_profile::profile!("compile_unit_over");
    let text = scope.text();
    let cores = scope.cores();

    let lowered = into_core_unit(source, &text, syntax)
        .map_err(|error| CompileError::Failure(vec![error.report()]))?;

    let closure = invalidated(baseline, lowered.core());
    // A reused item is exactly as the baseline elaborated it. The entry is never reused — it is the one item with no name to be reached by — and the seed table is the new lowering's, which the closure module carries whole.
    let reused = Module {
        entry: None,
        universe_seeds: Vec::new(),
        ..baseline.core().restricted(|name| !closure.contains(name))
    };
    let changed = lowered.core().restricted(|name| closure.contains(name));

    let mut context = Context::new(budget, *syntax);
    context.set_imports(lowered.imports().clone());
    context.set_broken(lowered.broken_names());
    let (core, _body_type) = with_broken(
        lowered.broken(),
        elaborate_and_zonk_unit_over(
            &mut context,
            Established::over(&cores),
            Recompile {
                reused: &reused,
                closure: &changed,
                lowered: lowered.core(),
            },
            lowered.metavariable_floor(),
            lowered.universe_floor(),
            Mode::Infer,
            Tail::Written,
        )
        .map_err(|error| {
            CompileError::of(&error, |member| {
                member.reports_with_hints(
                    lowered.core(),
                    &cores,
                    syntax,
                    lowered.unbound(),
                    lowered.imports(),
                )
            })
        }),
    )?;

    let core =
        Zonked::project(&core).map_err(|refusal| CompileError::failure(refusal.to_string()))?;

    if let Some(verdict) = recheck_over(
        &core,
        budget,
        scope,
        &reused,
        baseline.binder_floor(),
        syntax,
    )
    .into_iter()
    .next()
    {
        return Err(kernel_refusal(&verdict, core.as_module(), &cores, syntax));
    }

    let ersd = erase_unit(
        &mut Context::new(budget, *syntax),
        Resumed::of(&cores, scope.arena()),
        &core,
        None,
    )
    .map_err(|error| CompileError::Failure(error.reports_with(core.as_module(), &cores, syntax)))?;

    let core = core.into_module();
    let binder_floor = derived_binder_floor(&core);

    Ok(Unit::new(lowered, core, ersd, binder_floor))
}

/// [`recheck`](crate::recheck) with `reused` in scope beside the units: an earlier compilation's items an item-level recompile replayed, judged by the walk that filed the baseline, so this walk judges the closure alone — by name, exactly as it skips a mounted unit's items. `reused_floor` is the baseline's, a bound over every binder the reused terms mention.
///
/// The reused registry entries are both mounted and kept in `module`: the walk declares every entry the module carries, overwriting the mounted copy with an equal one, and its positivity pass extends the environment's registries with the module's, resolving a name in both to the module's own.
pub(crate) fn recheck_over(
    module: &Zonked<Module>,
    budget: u64,
    scope: Prefix<'_>,
    reused: &Module,
    reused_floor: usize,
    syntax: &SyntaxRegistry,
) -> Vec<Verdict> {
    let mut globals = globals(scope);
    globals.mount(reused, reused_floor);

    recheck_module_verdicts(module, budget, &globals, *syntax)
}

/// The names the new text invalidates: every declared name whose lowered item differs from the baseline's modulo the identities lowering mints, every registry key whose entry differs, every name only one side declares, and every name whose witness or test membership moved — closed backwards over the baseline's elaborated graph.
pub(crate) fn invalidated(baseline: &Unit, lowered: &Module) -> BTreeSet<Global> {
    let seeds = changed_names(baseline.text().core(), lowered);

    reverse_closure(baseline.core(), seeds)
}

/// The names an edit changed, read off the two lowered modules; see [`invalidated`].
///
/// Items are paired by their first declared name, and a pair is unchanged when it declares the same names, agrees on every scalar field and has every term position equal modulo one renaming — so a metavariable shared between an item's type and body is shared on both sides. A mount that moved invalidates everything, since every name's spelling depends on it.
fn changed_names(before: &Module, after: &Module) -> BTreeSet<Global> {
    if before.mounts != after.mounts {
        return declared(before).union(&declared(after)).cloned().collect();
    }

    let (these, those) = (by_name(before), by_name(after));
    let mut changed = BTreeSet::new();

    for (name, item) in &those {
        match these.get(name) {
            Some(previous) if item_unchanged(previous, item) => {}
            _ => changed.extend(item.declared_names().into_iter().cloned()),
        }
    }
    for (name, item) in &these {
        if !those.contains_key(name) {
            changed.extend(item.declared_names().into_iter().cloned());
        }
    }

    changed.extend(entries_changed(
        &before.induct_decls,
        &after.induct_decls,
        induct_unchanged,
    ));
    changed.extend(entries_changed(
        &before.struct_decls,
        &after.struct_decls,
        struct_unchanged,
    ));
    changed.extend(entries_changed(
        &before.concepts,
        &after.concepts,
        concept_unchanged,
    ));
    changed.extend(
        before
            .witnesses
            .symmetric_difference(&after.witnesses)
            .cloned(),
    );
    let tests = |module: &Module| module.tests.iter().cloned().collect::<BTreeSet<_>>();
    changed.extend(tests(before).symmetric_difference(&tests(after)).cloned());

    changed
}

/// The reverse transitive closure of `seeds` over what `core`'s items reach: every item that reaches a seed, transitively, by every name it declares. A seed `core` does not declare — an added name — is in the closure with no dependents.
fn reverse_closure(core: &Module, seeds: BTreeSet<Global>) -> BTreeSet<Global> {
    let owner = core
        .items
        .iter()
        .enumerate()
        .flat_map(|(index, item)| {
            item.declared_names()
                .into_iter()
                .map(move |name| (name.clone(), index))
        })
        .collect::<BTreeMap<Global, usize>>();
    let mut dependents: BTreeMap<Global, Vec<usize>> = BTreeMap::new();
    for (index, item) in core.items.iter().enumerate() {
        for name in core.reaches(item) {
            if owner.contains_key(&name) {
                dependents.entry(name).or_default().push(index);
            }
        }
    }

    let mut closure = seeds;
    let mut work = closure.iter().cloned().collect::<Vec<_>>();
    while let Some(name) = work.pop() {
        for &index in dependents.get(&name).map_or(&[][..], Vec::as_slice) {
            for declared in core.items[index].declared_names() {
                if closure.insert(declared.clone()) {
                    work.push(declared.clone());
                }
            }
        }
    }

    closure
}

fn declared(module: &Module) -> BTreeSet<Global> {
    module
        .items
        .iter()
        .flat_map(Item::declared_names)
        .cloned()
        .collect()
}

fn by_name(module: &Module) -> BTreeMap<Global, &Item> {
    module
        .items
        .iter()
        .map(|item| {
            let name = item
                .declared_names()
                .first()
                .cloned()
                .cloned()
                .expect("an item declares a name");
            (name, item)
        })
        .collect()
}

/// The keys on either side whose entries are not `unchanged`, or that only one side holds.
fn entries_changed<'a, D>(
    before: &'a BTreeMap<Global, D>,
    after: &'a BTreeMap<Global, D>,
    unchanged: impl Fn(&D, &D) -> bool + 'a,
) -> impl Iterator<Item = Global> + 'a {
    before
        .keys()
        .chain(after.keys())
        .filter(move |name| match (before.get(*name), after.get(*name)) {
            (Some(this), Some(that)) => !unchanged(this, that),
            _ => true,
        })
        .cloned()
}

fn item_unchanged(before: &Item, after: &Item) -> bool {
    let mut renaming = MetaRenaming::default();

    match (before, after) {
        (Item::Let(this), Item::Let(that)) => {
            this.name == that.name
                && this.kind == that.kind
                && this.island == that.island
                && this.universe_context == that.universe_context
                && this.totality == that.totality
                && this.type_.equal_modulo_metas(&that.type_, &mut renaming)
                && this.body.equal_modulo_metas(&that.body, &mut renaming)
        }
        (Item::Rec(this), Item::Rec(that)) => {
            this.definitions == that.definitions
                && this.group.universe_context() == that.group.universe_context()
                && this.group.iter().len() == that.group.iter().len()
                && this
                    .group
                    .iter()
                    .zip(that.group.iter())
                    .all(|(this, that)| {
                        this.type_.arity() == that.type_.arity()
                            && this.body.arity() == that.body.arity()
                            && this
                                .type_
                                .body()
                                .equal_modulo_metas(that.type_.body(), &mut renaming)
                            && this
                                .body
                                .body()
                                .equal_modulo_metas(that.body.body(), &mut renaming)
                    })
        }
        _ => false,
    }
}

fn induct_unchanged(this: &InductDecl, that: &InductDecl) -> bool {
    let mut renaming = MetaRenaming::default();

    this.universe_context == that.universe_context
        && this.module == that.module
        && this.rep_public == that.rep_public
        && this.polarities == that.polarities
        && this.constructors.len() == that.constructors.len()
        && arity_unchanged(&this.arity, &that.arity, &mut renaming)
        && this
            .result_sort
            .equal_modulo_metas(&that.result_sort, &mut renaming)
        && this.constructors.iter().zip(&that.constructors).all(
            |((this_tag, this_payload), (that_tag, that_payload))| {
                this_tag == that_tag
                    && this_payload.plicities() == that_payload.plicities()
                    && telescope_unchanged(
                        &this_payload.telescope,
                        &that_payload.telescope,
                        &mut renaming,
                        &mut |these: &Vec<Term>, those: &Vec<Term>, renaming| {
                            these.len() == those.len()
                                && these
                                    .iter()
                                    .zip(those)
                                    .all(|(this, that)| this.equal_modulo_metas(that, renaming))
                        },
                    )
            },
        )
}

fn struct_unchanged(this: &StructDecl, that: &StructDecl) -> bool {
    let mut renaming = MetaRenaming::default();

    this.universe_context == that.universe_context
        && this.module == that.module
        && this.rep_public == that.rep_public
        && this.polarities == that.polarities
        && arity_unchanged(&this.arity, &that.arity, &mut renaming)
        && this
            .result_sort
            .equal_modulo_metas(&that.result_sort, &mut renaming)
}

fn concept_unchanged(this: &ConceptDecl, that: &ConceptDecl) -> bool {
    let mut renaming = MetaRenaming::default();

    this.universe_context == that.universe_context
        && this.fields == that.fields
        && this.supers == that.supers
        && fields_unchanged(&this.params, &that.params, &mut renaming)
}

fn fields_unchanged(
    this: &Telescope<()>,
    that: &Telescope<()>,
    renaming: &mut MetaRenaming,
) -> bool {
    telescope_unchanged(this, that, renaming, &mut |_: &(), _: &(), _| true)
}

fn arity_unchanged(
    this: &Telescope<Telescope<()>>,
    that: &Telescope<Telescope<()>>,
    renaming: &mut MetaRenaming,
) -> bool {
    telescope_unchanged(
        this,
        that,
        renaming,
        &mut |these: &Telescope<()>, those: &Telescope<()>, renaming| {
            fields_unchanged(these, those, renaming)
        },
    )
}

/// Whether two telescopes agree entry by entry modulo `renaming`, ending in payloads `done` agrees on.
fn telescope_unchanged<B: Bound>(
    before: &Telescope<B>,
    after: &Telescope<B>,
    renaming: &mut MetaRenaming,
    done: &mut dyn FnMut(&B, &B, &mut MetaRenaming) -> bool,
) -> bool {
    let (mut before, mut after) = (before, after);
    loop {
        match (before, after) {
            (Telescope::Cons(this, this_rest), Telescope::Cons(that, that_rest)) => {
                if this_rest.arity() != that_rest.arity()
                    || !this.equal_modulo_metas(that, renaming)
                {
                    return false;
                }
                before = this_rest.body();
                after = that_rest.body();
            }
            (Telescope::Done(this), Telescope::Done(that)) => return done(this, that, renaming),
            _ => return false,
        }
    }
}
