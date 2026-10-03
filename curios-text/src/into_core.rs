//! Lowering one unit's surface modules to a [`curios_core::Module`], over the units already lowered.
//!
//! [`into_core_unit`] runs a unit through these passes, in order:
//!
//! 1. **Discovery** (`Resolved::of`): the prefixes the unit claims checked disjoint from the predecessors', then every module they declare loaded through its source.
//! 2. **Interface resolution** (`interface::resolve_unit`): each module's declarations seeded into its interface, then every `pub use` resolved to a fixed point, before any body is lowered.
//! 3. **Lowering** (`process_items`): each item's names resolved against those interfaces, its sugar undone and its matches compiled (`match_compile`), then the entry's final term.
//! 4. **Audit** (`audit`, `lint`): what the public interface exposes, and the declarations nothing reaches.
//! 5. **Ordering** (`order`): the unit's own items sorted so each one's value dependencies come before it.
//! 6. **Documentation** (`document`): the unit's documentation record, where it documents a mount.

mod audit;
use audit::*;

mod context;
use context::*;

mod lowerer;
use lowerer::*;

mod match_compile;
use match_compile::*;

mod order;
use order::*;

mod interface;
use interface::*;

mod lint;
use lint::*;

mod scoped;
use scoped::*;

mod document;
use document::*;
use {
    crate::{
        Apply, Argument, Entrypoint, Error, GroupItem, Label, LetSignature, Lint, LintedBinder,
        Module, Name, RootSource, StructLit, StructLitEntry, Subterm, Term, TopItem, TupleField,
        UseGroup, foreign_signature, func_sugar_lambda, func_sugar_type_params, ordered,
    },
    curios_abi::ForeignStore,
    curios_document::Documentation,
    curios_utilities::{
        Entropy, Mount, Plicity, Qualifier, Report, RootKind, Span, SyntaxRegistry,
    },
    std::{
        cell::{Cell, RefCell},
        collections::{BTreeMap, BTreeSet, HashMap},
        rc::Rc,
    },
};

#[cfg(test)]
mod binding_tests;
#[cfg(test)]
mod exposure_tests;
#[cfg(test)]
mod foreign_tests;
#[cfg(test)]
mod lint_tests;
#[cfg(test)]
mod lower_tests;
#[cfg(test)]
mod ordering_tests;
#[cfg(test)]
mod re_export_tests;
#[cfg(test)]
mod sys_tests;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod universe_tests;
#[cfg(test)]
mod use_tests;
#[cfg(test)]
mod visibility_tests;

/// Every prefix the compilation mounts, and which of them the unit being compiled may name.
///
/// **Two lists rather than one, because a prefix a unit cannot name still exists.** It is in the fold — it holds the arena the erasure resumes over — and it owns names whose spelling decides which unit declares them. So `mounts` stays complete for every question about *what a name is*, and `visible` answers the one question about *who may write it*. Collapsing them would either hide a mount from `Mount::owning`, which decides coherence, or make an undeclared prefix indistinguishable from a prefix nobody mounted, which is the diagnostic this type exists to keep.
///
/// Carried as one value because it is threaded through the whole of resolution: the walk that follows a `use` to its provider asks both questions at every hop, and two parallel parameters through ten functions drift.
#[derive(Clone, Copy)]
pub(super) struct Reach<'a> {
    mounts: &'a [Mount],
    visible: &'a [Mount],
}

impl<'a> Reach<'a> {
    /// `mounts` is every prefix in the compilation; `visible` is the subset this unit declared, plus its own.
    pub(super) fn over(mounts: &'a [Mount], visible: &'a [Mount]) -> Self {
        Self { mounts, visible }
    }

    /// Every prefix the compilation mounts — what owns a name, never what may write one.
    pub(super) fn mounts(&self) -> &'a [Mount] {
        self.mounts
    }

    /// Reject a reference that *resolves into* a prefix this unit cannot name.
    ///
    /// `resolved` is the segments of the qualifier the reference resolved to — not the raw spelled path — so absolute and relative spellings are guarded identically. A target inside a visible prefix, or inside the reader's own, passes through.
    ///
    /// **Visibility is the whole rule, and the root's kind only chooses the wording.** `/sys` is closed because no unit but the standard library declares a dependency on it, and the standard library does — the rule a package's manifest already states, with no privilege comparison between roots beside it. Whether the prefix is a closed root decides which refusal a reader gets — "use the `/std` module" rather than "declare it" — because telling a program to declare `/sys` would be advice it cannot take.
    ///
    /// **The prefix stays discoverable and the reference is refused**, rather than the prefix being hidden and the name reported unbound. A hidden prefix makes `use /sys/{Nat}` read as a typo; a refused one says which unit holds the name and why this one may not write it, which is the difference between a diagnostic and a riddle.
    fn guard(&self, _consumer: &Qualifier, resolved: &[String]) -> Result<(), Error> {
        let Some(root) = resolved.first() else {
            return Ok(());
        };

        let prefix = Qualifier::from([root.as_str()]);
        // Not a mount at all: an ordinary name under the compilation root, or one that will fail to resolve on its own terms.
        if !self.mounts.iter().any(|mount| mount.prefix == prefix) {
            return Ok(());
        }

        // A prefix the unit declared, or its own — `visible` carries both, because a unit does not declare a dependency on itself.
        if self.visible.iter().any(|mount| mount.prefix == prefix) {
            return Ok(());
        }

        match is_internal_root(self.mounts, root) {
            true => Err(Error::InternalRootModule {
                segment: root.clone(),
            }),
            false => Err(Error::UndeclaredPrefix {
                prefix: root.clone(),
            }),
        }
    }
}

/// Every absolute path this unit may write for each global — each declaration at its own path, each export at the path its module gives it — with who may write it and whether it lies in the unit's own mounts; a path into a prefix the unit may not name is dropped. What a report spells a name by, through [`curios_core::Spellings::spell`].
fn writable_paths(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    reach: &Reach<'_>,
    own: &[Mount],
) -> BTreeMap<curios_core::Global, Vec<curios_core::WritablePath>> {
    let audiences = Audiences::compute(public, table);
    // Own, and not inside a predecessor's mount: the entry claims the empty prefix, under which every other unit's names also lie.
    let owned = |path: &Qualifier| {
        own.iter().any(|mount| path.is_within(&mount.prefix))
            && !reach
                .mounts()
                .iter()
                .filter(|mount| !mount.prefix.is_root() && !own.contains(mount))
                .any(|mount| path.is_within(&mount.prefix))
    };

    let mut paths: BTreeMap<curios_core::Global, Vec<curios_core::WritablePath>> = BTreeMap::new();
    let mut record = |target: &Qualifier, path: Qualifier, audience: Vec<Qualifier>| {
        if reach.guard(&Qualifier::empty(), path.segments()).is_err() {
            return;
        }
        let own = owned(&path);
        paths
            .entry(curios_core::Global::Authored(*target))
            .or_default()
            .push(curios_core::WritablePath {
                path,
                audience,
                own,
            });
    };

    for (module, info) in table.iter() {
        for (label, _) in info.bindings() {
            let path = module.with(label);
            record(&path, path, audiences.binding(&path));
        }
    }
    for (module, interface) in public.iter() {
        for (label, entry) in &interface.bindings {
            let path = module.with(label);
            if path != entry.target {
                record(&entry.target, path, audiences.module(module));
            }
        }
    }
    paths
}

// Whether `label` names an internal root: discoverable so the unit that declares it can resolve it by absolute path, and refused to every other. Asked of the mount itself rather than of the name it owns, because only a whole mount is internal — a module inside one is reachable exactly as far as its mount is.
fn is_internal_root(mounts: &[Mount], label: &str) -> bool {
    let prefix = Qualifier::from([label]);

    mounts
        .iter()
        .any(|mount| mount.prefix == prefix && mount.kind == RootKind::Internal)
}

/// The compilation root's children: the prefix each mount in `mounts` claims.
///
/// **The root is the only namespace a mount implies, and that is a consequence of a name being one word.** A prefix is a single segment — the entry's is empty, which is what makes it the entry — so nothing lies between the root and a mount for a mount to bring into existence. Every other namespace in a compilation is one somebody declared. A set rather than a sequence, because a scope's mounts and the unit's own may name the same prefix and neither declared it.
fn mounted_children<'m>(mounts: impl Iterator<Item = &'m Mount>) -> BTreeSet<String> {
    mounts
        .flat_map(|mount| mount.prefix.iter())
        .map(str::to_string)
        .collect()
}

/// Everything the unit being compiled claims a prefix for, each paired with how a refusal should name it.
///
/// Its mounts, and — when it is the entry — the top-level modules it declares, because `mod myorg` claims `/myorg` against every other unit exactly as a mount would. The entry's own mount is the empty prefix, which claims nothing: every name lies within the compilation root.
fn claims(source: &UnitSource<'_>, own: &[Mount]) -> Vec<(String, Qualifier)> {
    let mounts = own
        .iter()
        .filter(|mount| !mount.prefix.is_root())
        .map(|mount| (format!("`{}`", mount.prefix.join()), mount.prefix));

    let modules = source.root_items().iter().filter_map(|item| match item {
        TopItem::Mod(declaration) => Some((
            format!("`mod {}` in the entry program", declaration.label),
            Qualifier::from([declaration.label.clone()]),
        )),
        _ => None,
    });

    mounts.chain(modules).collect()
}

struct Resolved<'a> {
    modules: HashMap<Qualifier, Rc<Module>>,
    /// Where each `mod` the unit declares was written, by the module it declares — what the `unused-declaration` lint underlines for a whole dead module.
    mod_spans: HashMap<Qualifier, Span>,
    /// This unit's own module graph, over what the units in scope established. Every insertion below targets a module the unit declares; reads cross the boundary, which is why this is layered rather than copied. See [`Scoped`].
    table: Scoped<'a, ModuleInfo>,
}

/// One unit's text-stage state — its resolution tables, its lowered module and what the lowering recorded beside it — opaque outside this crate and carried on the unit for the units compiled after it.
#[derive(Clone)]
#[curios_archive::archived]
pub struct PreparedText {
    mounts: Vec<Mount>,
    /// The `foreign` rows this unit declares. Collected by the same walk that lowers its items, and carried here rather than dropped: a unit that declares one has to reach the link, and the prelude declaring none is a fact about the prelude, not about the shape.
    foreigns: ForeignStore,
    table: BTreeMap<Qualifier, ModuleInfo>,
    public: BTreeMap<Qualifier, PublicInterface>,
    core: curios_core::Module,
    /// What this unit's lowering minted, which its elaboration's counters start above. A count within the unit: nothing here resumes above a predecessor.
    minted: curios_core::Minted,
    /// Every bare name that resolved to nothing, by the binder it lowered to, with what it could have meant — see `Context::unbound_binder`. Empty for any unit that compiles, the prelude included.
    unbound: BTreeMap<curios_core::Free, Vec<Qualifier>>,
    /// How this unit's names can be written: every binding a `use` brought into scope, with the spelling a reader wrote it under and, per definition, the ones in scope where it was written — see `Context::imports` — and every absolute path the unit may write for each global. What a goal report's candidate pool reaches beyond the names the program already mentions, and what every report spells a name by.
    spellings: curios_core::Spellings,
    /// Every lint the lowering found, in reading order, less each `unused-binder` lint elaboration credited — see [`Lint`]. Carried with the unit because a lint depends on exactly what the unit's identity in the store depends on: its own sources and its predecessors' interfaces.
    lints: Vec<Lint>,
    /// Every written binder a proof the elaborator wrote read — see [`LintedBinder`]. What a unit compiled over this one as its baseline credits in a declaration it does not elaborate again.
    credited: Vec<LintedBinder>,
    /// The prefix of every mount some reference of this unit was *written* under — see `Context::note_spelled`.
    reached: BTreeSet<Qualifier>,
    /// Every item the parser could not read, in reading order — see [`BrokenItem`]. Empty for any unit that compiles.
    broken: Vec<BrokenItem>,
    /// This unit's interface for its consumers, when its resolver marked a mount as documented — built here, as the last thing the lowering does, so it travels with the unit. See `document`.
    documentation: Option<Documentation>,
}

/// An item the parser could not read, as the lowering carries it: the parser's report, and the name its head declared, registered so a reference to it resolves to this name — which the elaborator withholds, as it withholds every dependent of a refusal — rather than reporting as unbound. `None` where the head did not parse as far as a name, whose references then report as the unbound names they are.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct BrokenItem {
    pub declares: Option<curios_core::Global>,
    pub report: Report,
}

impl PreparedText {
    pub fn lints(&self) -> &[Lint] {
        &self.lints
    }

    /// Drop the `unused-binder` lint of every binder in `credited`: read by a proof the elaborator wrote, which no written name reaches, so used. Called once the unit has elaborated, since only elaboration writes such a proof.
    pub fn credit(&mut self, credited: &BTreeSet<(Option<curios_core::Global>, u32)>) {
        self.credit_where(|binder| credited.contains(&(binder.declaration, binder.ordinal)));
    }

    /// Credit, in every declaration `reused` keeps as `baseline` elaborated it, the binders `baseline`'s elaboration credited: a declaration a recompile does not elaborate again has its lowering unchanged, so its binders sit where they sat.
    pub fn credit_reused(
        &mut self,
        baseline: &PreparedText,
        reused: impl Fn(&curios_core::Global) -> bool,
    ) {
        let credited = baseline
            .credited
            .iter()
            .filter(|binder| binder.declaration.as_ref().is_some_and(&reused))
            .collect::<BTreeSet<_>>();
        self.credit_where(|binder| credited.contains(binder));
    }

    /// Drop the `unused-binder` lint of every binder `read` holds of, and record which each was.
    fn credit_where(&mut self, read: impl Fn(&LintedBinder) -> bool) {
        let (credited, kept) = std::mem::take(&mut self.lints)
            .into_iter()
            .partition::<Vec<_>, _>(|lint| lint.binder.as_ref().is_some_and(&read));
        self.lints = kept;
        self.credited
            .extend(credited.into_iter().filter_map(|lint| lint.binder));
    }

    /// Every item the parser could not read. A unit holding one is refused whatever elaboration said of the rest, and what it said is reported beside these.
    pub fn broken(&self) -> &[BrokenItem] {
        &self.broken
    }

    /// The names the broken items declared — what the elaborator withholds dependents of.
    pub fn broken_names(&self) -> BTreeSet<curios_core::Global> {
        self.broken
            .iter()
            .filter_map(|item| item.declares)
            .collect()
    }

    pub fn reached(&self) -> &BTreeSet<Qualifier> {
        &self.reached
    }

    pub fn core(&self) -> &curios_core::Module {
        &self.core
    }

    /// This unit's interface for its consumers, or `None` for a unit whose resolver marked nothing as documented — an entry program, or a prelude mount nobody reaches for by name.
    pub fn documentation(&self) -> Option<&Documentation> {
        self.documentation.as_ref()
    }

    /// The `foreign` rows this unit declares.
    pub fn foreigns(&self) -> &ForeignStore {
        &self.foreigns
    }

    /// This prepared prelude with its lowered module hash-consed against `sharing`. Pass the same table used for the elaborated module so equal structures collapse across the two snapshots, not merely within each.
    ///
    /// The rest of a `PreparedText` is resolution metadata and what the lowering minted — no terms — so the lowered module is the whole of what there is to share.
    pub fn shared(self, sharing: &curios_core::Sharing) -> Self {
        Self {
            core: self.core.shared(sharing),
            ..self
        }
    }

    /// What this unit's lowering minted — see the field.
    pub fn minted(&self) -> &curios_core::Minted {
        &self.minted
    }

    /// What each unresolved bare name could have meant, by the binder it lowered to — the table `curios-elab`'s `unbound variable` report reads its suggestion from.
    pub fn unbound(&self) -> &BTreeMap<curios_core::Free, Vec<Qualifier>> {
        &self.unbound
    }

    /// What each `use` brought into scope, where, and under which spelling — the table `curios-elab`'s goal suggestions draw imported candidates from and spell them by.
    pub fn imports(&self) -> &curios_core::Imports {
        &self.spellings.imports
    }

    /// How this unit's names can be written, where — what every report spells a name by.
    pub fn spellings(&self) -> &curios_core::Spellings {
        &self.spellings
    }
}

impl<'a> Resolved<'a> {
    /// Discover every module `source` declares, over what the predecessors already established.
    ///
    /// **One walk for the entry and a mounted unit.** They differ in where a root's items come from and in which prefixes the compilation root lists as children, both of which are answered below rather than duplicated: two copies of a tree walk agree only by being read, which is the shape every configuration-dependent defect in this stage has had.
    ///
    /// No synthesized `mod sys;`-style declarations here: the compilation root's own `ModuleInfo` is built from the entry's raw items, then every mounted prefix is registered as its child *explicitly* — a deliberate fact, not something recovered later by pattern-matching a qualifier's leading string segment. `insert_child` (which rejects any collision, not just pub/pub) is what catches a user's own `mod std` colliding with that registration, in either direction.
    fn of(
        source: &UnitSource<'_>,
        predecessor_tables: &'a [&'a BTreeMap<Qualifier, ModuleInfo>],
        predecessor_mounts: &[Mount],
        own: &[Mount],
    ) -> Result<Self, Error> {
        let mut resolved = Resolved {
            modules: HashMap::new(),
            mod_spans: HashMap::new(),
            table: Scoped::over(predecessor_tables),
        };

        // The compilation root: the entry's own module when the entry is what is being lowered, and otherwise a synthetic one belonging to no unit — which is why its children are *every* mounted prefix rather than only this unit's.
        //
        // Writing it lands in this unit's own layer, which shadows whatever the predecessors' layer said, so listing only `own` here would silently hide the predecessors' mounts — `/std` among them — from a unit being compiled against them, which the test that says a unit reaches a mounted name holds.
        let mut root_info = scan_module_info(source.root_items())?;
        for child in mounted_children(predecessor_mounts.iter().chain(own)) {
            root_info.insert_child(&Label::from(child), true)?;
        }
        resolved.table.insert(Qualifier::empty(), root_info);

        // The entry's children hang off the root, whose `ModuleInfo` was just built by hand; a mounted unit has no root items, so this recurses over nothing for it.
        resolved.discover_children(source.root_items(), &Qualifier::empty(), source)?;

        for mount in own.iter().filter(|mount| !mount.prefix.is_root()) {
            let header = Rc::new(source.source.load(&mount.prefix)?);
            resolved.modules.insert(mount.prefix, Rc::clone(&header));
            resolved.discover(&header.items, &mount.prefix, source)?;
        }

        Ok(resolved)
    }

    // `mod` declarations only name children, so the module graph is a tree: every qualifier is reached exactly once and no cycles are possible. Hence the walk needs neither a visited-set nor a cache hit-check — just load each file module once and recurse.
    fn discover(
        &mut self,
        items: &[TopItem],
        prefix: &Qualifier,
        source: &UnitSource<'_>,
    ) -> Result<(), Error> {
        self.table.insert(*prefix, scan_module_info(items)?);
        self.discover_children(items, prefix, source)
    }

    // The child-recursion half of `discover`, split out so `of` can build the compilation root's `ModuleInfo` itself (with every mounted prefix pre-registered as a child) and recurse into its children without a second, unconditional `scan_module_info` call clobbering that registration.
    fn discover_children(
        &mut self,
        items: &[TopItem],
        prefix: &Qualifier,
        source: &UnitSource<'_>,
    ) -> Result<(), Error> {
        for item in items {
            if let TopItem::Mod(module_item) = item {
                let path = prefix.with(&module_item.label);
                if let Some(span) = module_item.label.span() {
                    self.mod_spans.insert(path, span.clone());
                }

                match &module_item.module {
                    Some(module) => self.discover(&module.items, &path, source)?,
                    None => {
                        let module = Rc::new(source.source.load(&path).map_err(|error| {
                            match &module_item.span {
                                Some(span) => error.at(span.clone()),
                                None => error,
                            }
                        })?);

                        self.modules.insert(path, Rc::clone(&module));
                        self.discover(&module.items, &path, source)?;
                    }
                }
            }
        }

        Ok(())
    }
}

fn scan_module_info(items: &[TopItem]) -> Result<ModuleInfo, Error> {
    let mut info = ModuleInfo::new();

    for item in items {
        match item {
            TopItem::Mod(m) => info.insert_child(&m.label, m.vis_pub)?,
            TopItem::Let(ls) => {
                for l in ls {
                    info.insert_binding(&l.label, l.vis_pub)?;
                }
            }
            TopItem::Induct(group) => {
                for u in group {
                    info.insert_induct_child(&u.label, u.vis_pub, u.rep_pub)?;
                    info.insert_binding(&u.label, u.vis_pub)?;
                }
            }
            // A struct declares one binding (the type-former), like a `let` — there are no value constructors and no nested namespace, so no child module.
            TopItem::Struct(group) => {
                for s in group {
                    info.insert_binding(&s.label, s.vis_pub)?;
                }
            }
            // A concept declares the type-former binding *and* a nested namespace (its method wrappers), like an inductive.
            TopItem::Concept(group) => {
                for c in group {
                    info.insert_child(&c.label, c.vis_pub)?;
                    info.insert_binding(&c.label, c.vis_pub)?;
                }
            }
            // A witness is anonymous: it declares no binding and occupies no lexical scope — its backing definition gets a compiler name.
            TopItem::Witness(_) => {}
            // A `foreign` declaration is an ordinary binding, like a `let` — it has no body of its own, but it is called the same way.
            TopItem::Foreign(f) => info.insert_binding(&f.label, f.vis_pub)?,
            // A test binds its name privately — referable within its subtree, colliding with a like-named sibling, never `pub`.
            TopItem::Test(t) => info.insert_binding(&t.label, false)?,
            // A broken item binds the name its head declared, public so a reference from anywhere resolves to it and is withheld rather than reported unbound; nothing can use the binding.
            TopItem::Broken(b) => {
                if let Some(label) = &b.declares {
                    info.insert_binding(label, true)?;
                }
            }
            _ => {}
        }
    }

    Ok(info)
}

// The surface concept application `C(args)` for a witness's declared type: the witnessed concept applied to the annotation's arguments (as written, so explicit).
//
// **Spanned over the written `C(args)`, because this is the only thing a `satisfy` refusal can point at.** A witness is anonymous, so nothing else in its declaration names it: `elaborate_module_let` locates a registration failure with `error.at_opt(def.type_.span())`, and this synthesized node is that type. Left spanless, every duplicate, orphan, unkeyable and irregular-premise refusal would arrive with no line at all — and a duplicate between two witnesses of one module would name that module twice and nothing else, which a reader cannot act on. The head name and each argument carry the spans this joins; `Term::with_span` keeps an innermost span already present, so the arguments are untouched.
fn witness_concept_application(concept: &Name, args: &[Term]) -> Term {
    let head: Term = Subterm::Name(concept.clone()).into();
    let written = |head: Term, last: Option<&Span>| match (concept.span(), last) {
        (Some(open), Some(close)) => {
            head.with_span(Span::new(open.source.clone(), open.start, close.end))
        }
        (Some(open), None) => head.with_span(open.clone()),
        (None, _) => head,
    };

    if args.is_empty() {
        return written(head, None);
    }

    let applied: Term = Subterm::Apply(Apply {
        head,
        arguments: args
            .iter()
            .map(|arg| Argument {
                term: arg.clone(),
                plicity: Plicity::Explicit,
            })
            .collect(),
    })
    .into();

    // The last argument's own span closes the range. An argument the parser gave none — a desugar's product — leaves the head's span alone, which still names the declaration.
    written(applied, args.last().and_then(Term::span))
}

impl Term {
    // The head name of a concept-application term (a path, optionally applied) — used to read the super concept off a `use`-marked field's type. `None` if the type is not shaped like a concept application. This is into_core-specific vocabulary (concept applications are a `into_core` pass concept), so it lives here rather than on `Term`'s own `impl` in `term.rs`.
    fn concept_app_head(&self) -> Option<Name> {
        match self.as_subterm() {
            Subterm::Name(name) => Some(name.clone()),
            Subterm::Apply(apply) => apply.head.concept_app_head(),
            _ => None,
        }
    }
}

// Resolve a super concept's head to its qualified core name — the same rule `Lowerer`'s term-reference arm uses, minus the local-binder shadowing (a declaration-site super edge has no enclosing value scope).
fn resolve_concept_head(context: &Context, name: &Name) -> Result<curios_core::Global, Error> {
    let qualifier = if name.is_abs() || !name.is_single() {
        context.resolve_term_name(name)?
    } else {
        match context.bindings().get(name.head()) {
            Some(qualifier) => {
                context.note_binding_use(name.head());
                *qualifier
            }
            None => Qualifier::from([name.head()]),
        }
    };
    Ok(curios_core::Global::Authored(qualifier))
}

#[allow(clippy::too_many_arguments)]
fn process_items(
    top_items: &[TopItem],
    context: &mut Context,
    flat_items: &mut Vec<FlatItem>,
    induct_decls: &mut BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: &mut BTreeMap<curios_core::Global, curios_core::StructDecl>,
    concepts: &mut BTreeMap<curios_core::Global, curios_core::ConceptDecl>,
    witnesses: &mut BTreeSet<curios_core::Global>,
    tests: &mut Vec<curios_core::Global>,
    foreigns: &mut ForeignStore,
    broken: &mut Vec<BrokenItem>,
    modules: &HashMap<Qualifier, Rc<Module>>,
) -> Result<(), Error> {
    for top_item in top_items {
        match top_item {
            TopItem::Mod(m) => {
                context.insert_scope(m.label.to_string(), context.prefixed(&m.label))?
            }
            TopItem::Broken(b) => {
                if let Some(label) = &b.declares {
                    context.insert_binding(label.to_string(), context.prefixed(label))?;
                }
            }
            TopItem::Let(labels) => {
                for l in labels {
                    context.insert_binding(l.label.to_string(), context.prefixed(&l.label))?;
                }
            }
            TopItem::Induct(group) => {
                for u in group {
                    context.insert_scope(u.label.to_string(), context.prefixed(&u.label))?;
                    context.insert_binding(u.label.to_string(), context.prefixed(&u.label))?;
                }
            }
            // The type-former binding only — like a `let` (no constructor namespace).
            TopItem::Struct(group) => {
                for s in group {
                    context.insert_binding(s.label.to_string(), context.prefixed(&s.label))?;
                }
            }
            // A concept declares its type-former binding and a nested namespace for the method wrappers, like an inductive.
            TopItem::Concept(group) => {
                for c in group {
                    context.insert_scope(c.label.to_string(), context.prefixed(&c.label))?;
                    context.insert_binding(c.label.to_string(), context.prefixed(&c.label))?;
                }
            }
            // A witness is anonymous — no binding, no scope entry.
            TopItem::Witness(_) => {}
            TopItem::Foreign(f) => {
                context.insert_binding(f.label.to_string(), context.prefixed(&f.label))?
            }
            TopItem::Test(t) => {
                context.insert_binding(t.label.to_string(), context.prefixed(&t.label))?
            }
            _ => {}
        }
    }

    for top_item in top_items {
        match top_item {
            // Nothing lowers: the item is carried as its report, under the name it declared, for the compile boundary to refuse the unit by and the elaborator to withhold dependents of.
            TopItem::Broken(b) => broken.push(BrokenItem {
                declares: b
                    .declares
                    .as_ref()
                    .map(|label| curios_core::Global::Authored(context.prefixed(label))),
                report: b.report.clone(),
            }),
            TopItem::Mod(mod_item) => match &mod_item.module {
                Some(module) => {
                    process_items(
                        &module.items,
                        &mut context.nested(&mod_item.label),
                        flat_items,
                        induct_decls,
                        struct_decls,
                        concepts,
                        witnesses,
                        tests,
                        foreigns,
                        broken,
                        modules,
                    )?;
                }
                None => {
                    let path = context.prefixed(&mod_item.label);
                    // Discovery is exhaustive over this same tree, so every file-backed module is already cached under this qualifier.
                    let module = modules.get(&path).expect("module loaded during discovery");

                    process_items(
                        &module.items,
                        &mut context.nested(&mod_item.label),
                        flat_items,
                        induct_decls,
                        struct_decls,
                        concepts,
                        witnesses,
                        tests,
                        foreigns,
                        broken,
                        modules,
                    )?;
                }
            },
            TopItem::Use(use_item) => {
                // The lexical import effect of `use`/`pub use`: source-ordered, point-of-use scoping. The interface (export) effect of `pub use` is precomputed by the `pub use` fixed point (`interface::resolve_unit`), not here.
                match &use_item.group {
                    UseGroup::Named(items) => {
                        for item in items {
                            let full = use_item.name.with_label(item.label());
                            context.open_site(UseSite {
                                span: item.label().span().cloned(),
                                what: UseSiteKind::Selector(item.label().clone()),
                                exported: use_item.vis_pub,
                                used: false,
                            });

                            match item {
                                GroupItem::Mod(_) => {
                                    context.resolve_module_use(&full)?;
                                }
                                GroupItem::Let(_) => {
                                    context.resolve_binding_use(&full)?;
                                }
                                GroupItem::Both(_) => {
                                    context.resolve_both_use(&full)?;
                                }
                            }
                            context.close_site();
                        }
                    }
                    UseGroup::Glob => {
                        context.open_site(UseSite {
                            span: use_item.span.clone(),
                            what: UseSiteKind::Glob(format!("{}/*", use_item.name.join())),
                            exported: use_item.vis_pub,
                            used: false,
                        });
                        context.resolve_glob(&use_item.name)?;
                        context.close_site();
                    }
                }
            }
            // A `let` item is a group of one or more definitions. It lowers to a `rec` item when it is a declared group or when its one member names itself — read off the lowered terms, so a definition's own name is in scope of its type and body without anything said in the source — and to a plain `let` item otherwise. The kernel needs the distinction and the programmer does not: a `Rec` binds its members' names, a `Let` leaves a self-reference unbound.
            TopItem::Let(ls) => {
                let mut items = ls
                    .iter()
                    .map(|let_item| {
                        let name = curios_core::Global::Authored(context.prefixed(&let_item.label));
                        context.record_import_scope(Some(&name));
                        let lower = Lowerer::new(context, Some(name));
                        let signature = lower.signature(&let_item.signature);
                        let type_ = lower.signature_type(&signature, None)?;
                        Ok(FlatLet {
                            kind: curios_core::DefinitionKind::Authored,
                            span: let_item.label.span().cloned(),
                            name: curios_core::Global::Authored(context.prefixed(&let_item.label)),
                            island: context.island(),
                            type_,
                            body: lower.signature_value(&signature, |body| lower.value(body))?,
                        })
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                let recursive = items.len() > 1 || items.iter().any(|let_| let_.mentions_itself());
                flat_items.push(match recursive {
                    true => FlatItem::Rec(items),
                    false => FlatItem::Let(items.pop().expect("a `let` item has a member")),
                });
            }
            // A test takes no parameters, so it is not function sugar — but it lowers to a `() -> Test` thunk, because `Test/main` holds the whole schedule and must force only the one it selected. The output is emitted as core directly off the registry slot, since a synthesized `Var` carries the resolved identity and nothing here depends on the declaration being importable.
            TopItem::Test(test) => {
                let name = curios_core::Global::Authored(context.prefixed(&test.label));
                context.record_import_scope(Some(&name));
                let lower = Lowerer::new(context, Some(name));
                let output = curios_core::Term::var(curios_core::Var::free(
                    curios_core::Free::global(context.syntax().test.test_type.qualifier()),
                ));
                // The empty telescope is not vestigial: it is the thunk. A test lowers to `() -> Test` so `Test/main` can hold it unforced and run only the one it selected.
                let type_ = lower.func_type_under(&func_sugar_type_params(&[]), || Ok(output))?;
                let body = func_sugar_lambda(&[], &test.body);
                let name = curios_core::Global::Authored(context.prefixed(&test.label));
                tests.push(name);
                flat_items.push(FlatItem::Let(FlatLet {
                    kind: curios_core::DefinitionKind::Test,
                    span: None,
                    name,
                    island: context.island(),
                    type_,
                    body: lower.value(&body)?,
                }));
            }
            TopItem::Foreign(f) => {
                // All FFI-specific bookkeeping (the `ForeignFunction`, its registration, and `host_fn`'s wire-typed signature shape) stays inside `sys_module`'s `host_rows`; from here a `foreign` declaration lowers exactly like an ordinary `TopItem::Let`.
                let path = context.prefixed(&f.label);
                let signature = foreign_signature(f, foreigns, path.join());

                let name = curios_core::Global::Authored(path);
                context.record_import_scope(Some(&name));
                let lower = Lowerer::new(context, Some(name));
                let type_ = lower.term(&signature.type_())?;
                flat_items.push(FlatItem::Let(FlatLet {
                    kind: curios_core::DefinitionKind::Authored,
                    span: f.label.span().cloned(),
                    name: curios_core::Global::Authored(path),
                    island: context.island(),
                    type_,
                    body: lower.value(&signature.body())?,
                }));
            }
            TopItem::Induct(group) => {
                // Step 1: type bindings as one rec group. An inductive's type binding wraps an intrinsic `InductType` normal form in a `Func` over its type parameters and indices (so `Result(Bin, Nat)` beta-reduces to `InductType { Result, [Bin, Nat] }` and `Vec(Bin, 3)` to `InductType { Vec, [Bin], [3] }`), and its shape is recorded in the inductive registry.
                let type_flat_items = group
                    .iter()
                    .map(|u| {
                        let name = curios_core::Global::Authored(context.prefixed(&u.label));
                        context.record_import_scope(Some(&name));
                        let lower = Lowerer::new(context, Some(name));

                        // Parameters and indices are minted before any of their types is lowered, and each type sees the binders before it — a later index type naming an earlier parameter must mean *that* binder.
                        let head_binders =
                            lower.mint(u.params.iter().map(|(_, n, _)| n.clone()).chain(
                                u.indices.iter().enumerate().map(|(i, (n, _))| {
                                    n.clone().unwrap_or_else(|| format!("_{i}"))
                                }),
                            ));
                        let (param_binders, index_binders) = head_binders.split_at(u.params.len());

                        let param_tys = u
                            .params
                            .iter()
                            .enumerate()
                            .map(|(i, (p, _, t))| {
                                let ty = lower.bound(&head_binders[..i], || lower.input_type(t))?;
                                Ok((*p, param_binders[i].1, ty))
                            })
                            .collect::<Result<Vec<_>, Error>>()?;
                        // The registry and the `InductType` normal form are positional; plicity matters only on the generated type-constructor function.
                        let param_tys_unmarked = param_tys
                            .iter()
                            .map(|(_, n, t)| (*n, t.clone()))
                            .collect::<Vec<_>>();

                        let param_vars = param_binders
                            .iter()
                            .map(|(_, id)| curios_core::Term::var(curios_core::Var::free(*id)))
                            .collect::<Vec<_>>();

                        // The head's index telescope. Unnamed entries got a positional placeholder above — the name only matters for dependency capture among the index types.
                        let index_tys = u
                            .indices
                            .iter()
                            .enumerate()
                            .map(|(i, (_, t))| {
                                let seen = u.params.len() + i;
                                let ty =
                                    lower.bound(&head_binders[..seen], || lower.input_type(t))?;
                                Ok((index_binders[i].1, ty))
                            })
                            .collect::<Result<Vec<_>, Error>>()?;

                        let index_vars = index_binders
                            .iter()
                            .map(|(_, id)| curios_core::Term::var(curios_core::Var::free(*id)))
                            .collect::<Vec<_>>();

                        // Registry entry: the parameter telescope plus each constructor's full signature `(params..., payload...) -> InductType { name, params, indices }`, where the terminal's indices are that *case's* target expressions over its payload binders. `Telescope::build` captures the parameter and payload labels in the payload types and the terminal, mirroring `func_type`.
                        let constructors = u
                            .cases
                            .iter()
                            .map(|c| {
                                let payload_binders =
                                    lower.mint(c.payload.iter().enumerate().map(|(i, param)| {
                                        param.label.clone().unwrap_or_else(|| format!("_{i}"))
                                    }));
                                let mut scope = param_binders.to_vec();
                                let fields = c
                                    .payload
                                    .iter()
                                    .enumerate()
                                    .map(|(i, param)| {
                                        let ty = lower
                                            .bound(&scope, || lower.input_type(&param.type_))?;
                                        scope.push(payload_binders[i].clone());
                                        Ok((payload_binders[i].1, ty))
                                    })
                                    .collect::<Result<Vec<_>, Error>>()?;

                                let target = lower.bound(&scope, || {
                                    c.target
                                        .iter()
                                        .flatten()
                                        .map(|t| lower.term(t))
                                        .collect::<Result<Vec<_>, Error>>()
                                })?;

                                // The signature terminates in the index targets alone: the family and its parameters are fixed by the declaration, so a terminal carries nothing else.
                                let telescope = curios_core::Telescope::build(
                                    param_tys_unmarked.iter().cloned().chain(fields),
                                    target,
                                );

                                // The value constructor's calling convention: every leading declaration parameter is implicit, each payload keeps its declared mark — the same source `ctor_type` uses.
                                let plicities = u
                                    .params
                                    .iter()
                                    .map(|_| Plicity::Implicit)
                                    .chain(c.payload.iter().map(|param| param.plicity))
                                    .collect::<Vec<_>>();

                                Ok((
                                    curios_core::Atom::from(c.label.as_str()),
                                    curios_core::InductParam::new(telescope, plicities),
                                ))
                            })
                            // Collected in written order: a constructor's position here is the runtime tag `erase` gives it (`InductDecl::constructors`), so the sequence is the declaration's, not a collation of its labels.
                            .collect::<Result<Vec<_>, Error>>()?;

                        // The declared result sort (`Type`/`Prop`) — closed, so it lowers in the base context. It is both the registry entry's sort and the type-constructor's codomain.
                        let result_sort = lower.term(&u.result_sort)?;

                        induct_decls.insert(
                            name,
                            curios_core::InductDecl {
                                universe_context: curios_core::UniverseContext::empty(),
                                arity: curios_core::Telescope::build(
                                    param_tys_unmarked.clone(),
                                    curios_core::Telescope::build(index_tys.iter().cloned(), ()),
                                ),
                                constructors,
                                result_sort: result_sort.clone(),
                                module: context.island(),
                                rep_public: u.rep_pub,
                                // Positivity has not run yet: `curios-elab` computes each declaration's parameter polarities after elaboration and writes them back here.
                                polarities: Vec::new(),
                            },
                        );

                        let induct_decl =
                            curios_core::Term::induct_type(name, param_vars, index_vars);

                        // The type constructor takes its parameters and then its indices, one call each: `Vec : (T : Type) -> (n : Nat) -> Type`, applied `Vec(T)(n)`, so `Vec(T)` is the family its matches eliminate — see documentation/design/theory/a-call-fills-one-parameter-group.md. A family with only one of the two takes it in one call, and a nullary one is its normal form outright. Parameters keep their declared marks (`@` makes one implicit at use sites); indices are always explicit.
                        let index_binders = index_tys
                            .iter()
                            .cloned()
                            .map(|(n, t)| (Plicity::Explicit, n, t))
                            .collect::<Vec<_>>();
                        let (type_, body) = [param_tys.clone(), index_binders]
                            .into_iter()
                            .filter(|group| !group.is_empty())
                            .rev()
                            .fold((result_sort, induct_decl), |(type_, body), group| {
                                (
                                    curios_core::Term::func_type_marked(group.clone(), type_),
                                    curios_core::Term::func_marked(group, body),
                                )
                            });
                        Ok(FlatLet {
                            kind: curios_core::DefinitionKind::InductiveType,
                            span: u.label.span().cloned(),
                            name: curios_core::Global::Authored(context.prefixed(&u.label)),
                            island: context.island(),
                            type_,
                            body,
                        })
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                // Unconditionally, where a `struct`, `concept` or `witness` group tests `formers.len()` and a `let` group tests `mentions_itself` besides — so a lone non-recursive `induct` carries a `rec` group of one that means nothing, and every occurrence of `Option`, `Result` or `Bool` reduces through it.
                //
                // Making this conditional is not the two-line change the neighbours make it look like, and both halves were measured rather than reasoned. `mentions_itself` reads `free_vars_shared()`, but an inductive's recursion lives in its *registry entry* — `InductType` holds `name: Global` as a field, not a `Var` — so the test is false for every inductive, recursive ones included, and reaching for it lowers a recursive inductive such as `/std/Cli/Values` as a `Let` that loses `elaborate_module_rec`'s `context.assume` and leaves its own name unbound while the registry telescopes rebuild. Reading the reach through `order::induct_free_vars` instead clears that and then fails at `/std/Async/Future/State` with `universe instance has 1 arguments but its scheme expects 0`: the group is also where a universe-polymorphic inductive's levels are generalized, and as a `Let` the scheme comes out monomorphic while occurrences still pass a level.
                //
                // So the wrapper does double duty, and dropping it needs `elaborate_module_let` to do both jobs. Nothing depends on that — a folded spelling reduces correctly, `curios-elab`'s `unfold_rec_apply` applying a group that dissolved to its member's value — which leaves this an optimization with no measurement behind it.
                flat_items.push(FlatItem::Rec(type_flat_items));

                // Step 2: constructor bindings. Each is a function whose body injects the variant, an intrinsic `Variant` normal form.
                for u in group {
                    for c in &u.cases {
                        let name = curios_core::Global::Authored(
                            context.prefixed(&u.label).with(&c.label),
                        );
                        context.record_import_scope(Some(&name));
                        let lower = Lowerer::new(context, Some(name));

                        // Per-case payload binder names: the declared name, or a positional placeholder.
                        let payload_name = |i: usize, n: &Option<String>| {
                            n.clone().unwrap_or_else(|| format!("_{i}"))
                        };

                        // Output type term `T`, `T(A, ...)`, `T(target...)`, or — indexed with parameters — the case's full terminal `T(A, ...)(target...)`: a name ref applied the way a use site writes it, one call for the parameters and one for the target's index expressions.
                        let parameters: Vec<Argument> = u
                            .params
                            .iter()
                            .map(|(p, n, _)| Argument {
                                term: Subterm::Name(Name::from(vec![n.clone()])).into(),
                                // Each argument's mark must match its binder on the type constructor (the two-queue rule): an `@`-marked parameter is filled from the implicit queue.
                                plicity: *p,
                            })
                            .collect();
                        let targets: Vec<Argument> = c
                            .target
                            .iter()
                            .flatten()
                            .map(|t| Argument {
                                term: t.clone(),
                                plicity: Plicity::Explicit,
                            })
                            .collect();
                        let output_type: Term = [parameters, targets]
                            .into_iter()
                            .filter(|group| !group.is_empty())
                            .fold(
                                Subterm::Name(Name::from(vec![u.label.clone()])).into(),
                                |head, arguments| Subterm::Apply(Apply { head, arguments }).into(),
                            );

                        // Constructor type: (params..., _0 : T_0, ...) -> T. Every inductive parameter is implicit at the value constructor — `Result/success(42)` infers them, the call-site `@` supplies one positionally — while the payload binders keep their declared marks (`@m` makes one implicit; the default is explicit).
                        let binders = lower.mint(
                            u.params.iter().map(|(_, n, _)| n.clone()).chain(
                                c.payload
                                    .iter()
                                    .enumerate()
                                    .map(|(i, param)| payload_name(i, &param.label)),
                            ),
                        );
                        let plicities = u
                            .params
                            .iter()
                            .map(|_| Plicity::Implicit)
                            .chain(c.payload.iter().map(|param| param.plicity))
                            .collect::<Vec<_>>();
                        let written = u
                            .params
                            .iter()
                            .map(|(_, _, t)| t)
                            .chain(c.payload.iter().map(|param| &param.type_))
                            .collect::<Vec<_>>();
                        let param_tys = written
                            .iter()
                            .enumerate()
                            .map(|(i, t)| {
                                let ty = lower.bound(&binders[..i], || lower.input_type(t))?;
                                Ok((plicities[i], binders[i].1, ty))
                            })
                            .collect::<Result<Vec<_>, Error>>()?;
                        let payload_binders = &binders[u.params.len()..];
                        let param_binders = &binders[..u.params.len()];
                        // Erasure is sort-driven: `erase_func` drops the same proof/type payload params that `erase_variant` drops from the tuple — the constructor function's arity and its injected variant's arity stay in lockstep.
                        let ctor_type = curios_core::Term::func_type_marked(
                            param_tys.clone(),
                            lower.bound(&binders, || lower.term(&output_type))?,
                        );
                        // Constructor body: (params..., _0, ...) => the variant's injection, an intrinsic `Variant` normal form.
                        let args: Vec<curios_core::Term> = payload_binders
                            .iter()
                            .map(|(_, id)| curios_core::Term::var(curios_core::Var::free(*id)))
                            .collect();
                        let inject = curios_core::Term::variant(
                            curios_core::Global::Authored(context.prefixed(&u.label)),
                            param_binders
                                .iter()
                                .map(|(_, id)| curios_core::Term::var(curios_core::Var::free(*id))),
                            curios_core::Atom::from(c.label.as_str()),
                            args,
                        );
                        // The value constructor carries the same calling convention as `ctor_type`: every inductive parameter is implicit, each payload keeps its declared mark.
                        let ctor_body = curios_core::Term::func_marked(param_tys, inject);

                        flat_items.push(FlatItem::Let(FlatLet {
                            span: None,
                            kind: curios_core::DefinitionKind::InductiveConstructor {
                                owner: context.prefixed(&u.label),
                                tag: curios_core::Atom::from(c.label.as_str()),
                            },
                            name: curios_core::Global::Authored(
                                context.prefixed(&u.label).with(&c.label),
                            ),
                            island: context.island(),
                            type_: ctor_type,
                            body: ctor_body,
                        }));
                    }
                }
            }
            // A struct lowers to a single type-former `let` plus a registry entry — no value-constructor binding (the literal elaborates directly) and no indices.
            // A group of structures lowers its formers into one `rec` item, as an `induct` group does, so each member's fields may name the others; a lone structure stays a `let`, its own name reached through its registry telescope.
            TopItem::Struct(group) => {
                let mut formers = Vec::with_capacity(group.len());
                for s in group {
                    let name = curios_core::Global::Authored(context.prefixed(&s.label));
                    context.record_import_scope(Some(&name));
                    let lower = Lowerer::new(context, Some(name));

                    // Declaring module: the type-former's qualifier prefix — identical to core's per-item `island` — for the representation-privacy checks.
                    let module = context.prefixed(&s.label).without_last();

                    let param_binders = lower.mint(s.params.iter().map(|(_, n, _)| n.clone()));
                    let param_tys = s
                        .params
                        .iter()
                        .enumerate()
                        .map(|(i, (p, _, t))| {
                            let ty = lower.bound(&param_binders[..i], || lower.input_type(t))?;
                            Ok((*p, param_binders[i].1, ty))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;
                    let param_tys_unmarked = param_tys
                        .iter()
                        .map(|(_, n, t)| (*n, t.clone()))
                        .collect::<Vec<_>>();
                    let param_vars = param_binders
                        .iter()
                        .map(|(_, id)| curios_core::Term::var(curios_core::Var::free(*id)))
                        .collect::<Vec<_>>();

                    // A repeated label is refused at the one that arrived second, as a repeated declaration is. Nothing else catches it: a structure's fields are not module declarations, so they never reach `insert_binding`'s check, and the telescope below would keep both — a literal would set them both and every `.label` read the first, silently. The tuple-type twin of this is `curios_elab`'s `DuplicateTupleLabel`, which a `struct` never reaches because its fields never elaborate as one.
                    let mut declared = BTreeSet::new();
                    for field in &s.fields {
                        if let Some(label) = &field.param.label
                            && !declared.insert(label.as_str())
                        {
                            return Err(duplicate_field(label));
                        }
                    }

                    // Field types, with declared or positional (`_i`) names so a later field type can depend on an earlier field. The signature sugar `f(params) -> T` is undone here.
                    let field_binders =
                        lower.mint(s.fields.iter().enumerate().map(|(i, field)| {
                            field
                                .param
                                .label
                                .as_deref()
                                .map(str::to_string)
                                .unwrap_or_else(|| format!("_{i}"))
                        }));
                    let mut field_scope = param_binders.clone();
                    let field_tys = s
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, field)| {
                            let ty = lower.bound(&field_scope, || {
                                lower.input_type(&field.param.desugared_type())
                            })?;
                            field_scope.push(field_binders[i].clone());
                            Ok((field_binders[i].1, ty))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;

                    // Registry entry: the parameter telescope, and the full field telescope (parameter binders first — field types may mention them — then field binders), as in `Inductive::indices`. The declared result sort (`Type`/`Prop`) — closed; both the registry entry's sort and the type-former's codomain.
                    let result_sort = lower.term(&s.result_sort)?;

                    struct_decls.insert(
                        name,
                        curios_core::StructDecl {
                            universe_context: curios_core::UniverseContext::empty(),
                            arity: curios_core::Telescope::build(
                                param_tys_unmarked.clone(),
                                curios_core::Telescope::build(field_tys, ()),
                            ),
                            result_sort: result_sort.clone(),
                            module,
                            rep_public: s.rep_pub,
                            // Positivity has not run yet: `curios-elab` computes each declaration's parameter polarities after elaboration and writes them back here.
                            polarities: Vec::new(),
                        },
                    );

                    // The type-former: `Pair : (A : Type, B : Type) -> Type` whose body is the `StructType` normal form (the bare node when parameterless), so `Pair(Nat, Bin)` reduces to `StructType { Pair, [Nat, Bin] }`. No value constructor.
                    let struct_type = curios_core::Term::struct_type(name, param_vars);
                    let (type_, body) = if param_tys.is_empty() {
                        (result_sort, struct_type)
                    } else {
                        (
                            curios_core::Term::func_type_marked(param_tys.clone(), result_sort),
                            curios_core::Term::func_marked(param_tys, struct_type),
                        )
                    };

                    formers.push(FlatLet {
                        kind: curios_core::DefinitionKind::StructType,
                        span: s.label.span().cloned(),
                        name: curios_core::Global::Authored(context.prefixed(&s.label)),
                        island: context.island(),
                        type_,
                        body,
                    });
                }
                flat_items.push(match formers.len() {
                    1 => FlatItem::Let(formers.pop().expect("a `struct` item has a member")),
                    _ => FlatItem::Rec(formers),
                });
            }
            // A concept lowers to a representation-public nominal `StructDecl` and its type-former `let` — plus a concept-registry entry (field labels, superclass edges, the parameter telescope) and one method-wrapper `let` per field, synthed into the concept's own namespace.
            // A concept group lowers its formers into one `rec` item as a struct group does; the method wrappers stay `let` items of their own, since each names only its former.
            TopItem::Concept(group) => {
                let mut formers = Vec::with_capacity(group.len());
                for concept in group {
                    let name = curios_core::Global::Authored(context.prefixed(&concept.label));
                    let module = context.prefixed(&concept.label).without_last();

                    context.record_import_scope(Some(&name));
                    let lower = Lowerer::new(context, Some(name));
                    let param_binders =
                        lower.mint(concept.params.iter().map(|(_, n, _)| n.clone()));
                    let param_tys = concept
                        .params
                        .iter()
                        .enumerate()
                        .map(|(i, (p, _, t))| {
                            let ty = lower.bound(&param_binders[..i], || lower.input_type(t))?;
                            Ok((*p, param_binders[i].1, ty))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;
                    let param_tys_unmarked = param_tys
                        .iter()
                        .map(|(_, n, t)| (*n, t.clone()))
                        .collect::<Vec<_>>();
                    let param_vars = param_binders
                        .iter()
                        .map(|(_, id)| curios_core::Term::var(curios_core::Var::free(*id)))
                        .collect::<Vec<_>>();

                    // Superclass fields are anonymous in the surface syntax; mint a unique internal label per super so the record telescope and the registry's field list stay well-formed. The name is never surfaced — a superclass is reached by resolution, keyed by index, and never projected or wrapped by name.
                    let field_labels = concept
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, field)| {
                            if field.is_super {
                                format!("_super{i}")
                            } else {
                                field.label.to_string()
                            }
                        })
                        .collect::<Vec<_>>();

                    // Field types, lowered under the parameter scope (a method field's label is the binder for later fields; a super field's minted label is inert). The signature sugar `f(params) -> T` is undone here.
                    let field_binders = lower.mint(field_labels.iter().cloned());
                    let mut field_scope = param_binders.clone();
                    let field_tys = concept
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, field)| {
                            let ty = lower.bound(&field_scope, || {
                                lower.input_type(&field.desugared_type())
                            })?;
                            field_scope.push(field_binders[i].clone());
                            Ok((field_binders[i].1, ty))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;

                    let result_sort = lower.term(&concept.result_sort)?;

                    // The record shape drives struct literals, projections, and — through `field_type_from` below — the declared type of every method wrapper.
                    let arity = curios_core::Telescope::build(
                        param_tys_unmarked.clone(),
                        curios_core::Telescope::build(field_tys, ()),
                    );
                    struct_decls.insert(
                        name,
                        curios_core::StructDecl {
                            universe_context: curios_core::UniverseContext::empty(),
                            arity: arity.clone(),
                            result_sort: result_sort.clone(),
                            module,
                            rep_public: concept.rep_pub,
                            // Positivity has not run yet: `curios-elab` computes each declaration's parameter polarities after elaboration and writes them back here.
                            polarities: Vec::new(),
                        },
                    );

                    // Superclass edges: each `use`-marked field names a super concept by its (resolved, qualified) head.
                    let supers = concept
                        .fields
                        .iter()
                        .enumerate()
                        .filter(|(_, field)| field.is_super)
                        .map(|(idx, field)| {
                            let head = field.type_.concept_app_head().ok_or_else(|| {
                                Error::MalformedSuperField {
                                    concept: concept.label.to_string(),
                                }
                            })?;
                            Ok((idx, resolve_concept_head(context, &head)?))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;

                    concepts.insert(
                        name,
                        curios_core::ConceptDecl {
                            universe_context: curios_core::UniverseContext::empty(),
                            params: curios_core::Telescope::build(param_tys_unmarked.clone(), ()),
                            fields: field_labels.clone(),
                            supers,
                        },
                    );

                    // The type-former, exactly like a representation-public struct's.
                    let struct_type = curios_core::Term::struct_type(name, param_vars.clone());
                    let (type_, body) = if param_tys.is_empty() {
                        (result_sort, struct_type)
                    } else {
                        (
                            curios_core::Term::func_type_marked(param_tys.clone(), result_sort),
                            curios_core::Term::func_marked(param_tys.clone(), struct_type),
                        )
                    };
                    formers.push(FlatLet {
                        kind: curios_core::DefinitionKind::ConceptType,
                        span: concept.label.span().cloned(),
                        name: curios_core::Global::Authored(context.prefixed(&concept.label)),
                        island: context.island(),
                        type_,
                        body,
                    });

                    // Method wrappers: for each *method* field `f`, pub let C/f(@p₁ : P₁, …, use w : C(p₁, …)) -> F = w.f; — and where `F` is a function type `(x₁ : X₁, …) -> R`, its parameters join the wrapper's one group instead: pub let C/f(@p₁ : P₁, …, use w : C(p₁, …), x₁ : X₁, …) -> R = w.f(x₁, …);
                    //
                    // One group because a call fills exactly one: `show(A) -> Str` declares one parameter list, so `Show/show(value)` is one call, where a wrapper returning the field as a value would be called `Show/show()(value)` — the concept's parameters and the witness are context the declaration never writes as a call. A field that is not written as a function, `Carrier : Type` or a type alias of a function, is the value it holds, reached by the call that supplies the witness: `Sized/Carrier(@Nat)`.
                    //
                    // Built in core rather than as surface AST, because `F` is not the field's *written* type: the record telescope above binds each field's label for the fields after it, so a field type may name the fields before it, and the wrapper has to state it with every such name opened at its own projection off `w`. Restating the written type instead leaves those names bound by nothing — well-formed only while no concept has a dependent field telescope. Reading it out of the telescope also means the wrapper inherits the record's universe metas by construction, rather than by re-lowering the same spans under a role forced to match.
                    //
                    // Type and body are constructed together so both close over the one `w`, and both index the field positionally. Superclass fields are anonymous and get no wrapper: an instance of the outer concept already yields the inner one by resolution.
                    let param_refs = param_vars.iter().collect::<Vec<_>>();
                    for (index, field) in concept
                        .fields
                        .iter()
                        .enumerate()
                        .filter(|(_, field)| !field.is_super)
                    {
                        // `index` is the field's position in the *whole* telescope, superclass slots included. Counting only the fields that get wrappers would read every method after a superclass one slot early.
                        let witness_id = lower.mint(["w".to_string()]).remove(0).1;
                        let witness = curios_core::Term::var(curios_core::Var::free(witness_id));

                        let params = param_tys
                            .iter()
                            .map(|(_, binder, type_)| (Plicity::Implicit, *binder, type_.clone()))
                            .chain(std::iter::once((
                                Plicity::Witness,
                                witness_id,
                                curios_core::Term::struct_type(name, param_vars.clone()),
                            )))
                            .collect::<Vec<_>>();

                        let field_type = arity
                            .open(&param_refs)
                            .field_type_from(&witness, index)
                            .expect("a concept's own field index is within its record telescope");
                        let method = curios_core::Term::proj(witness, index);

                        let (type_, body) = match &*field_type {
                            curios_core::Subterm::FuncType(function) => {
                                let mut cursor = function.telescope.cursor();
                                let mut own = Vec::with_capacity(function.plicities().len());
                                while let Some((hint, domain)) = cursor.entry() {
                                    let binder = lower
                                        .mint([hint.unwrap_or_default().to_string()])
                                        .remove(0)
                                        .1;
                                    cursor.advance(curios_core::Term::free_var(&binder));
                                    own.push((function.plicities()[own.len()], binder, domain));
                                }
                                let output = cursor.body().expect("a cursor past every entry");
                                let arguments = own
                                    .iter()
                                    .map(|(plicity, binder, _)| {
                                        (*plicity, curios_core::Term::free_var(binder))
                                    })
                                    .collect::<Vec<_>>();
                                let group = params.into_iter().chain(own).collect::<Vec<_>>();
                                (
                                    curios_core::Term::func_type_marked(group.clone(), output),
                                    curios_core::Term::func_marked(
                                        group,
                                        curios_core::Term::apply_marked(method, arguments),
                                    ),
                                )
                            }
                            _ => (
                                curios_core::Term::func_type_marked(params.clone(), field_type),
                                curios_core::Term::func_marked(params, method),
                            ),
                        };

                        flat_items.push(FlatItem::Let(FlatLet {
                            span: None,
                            kind: curios_core::DefinitionKind::ConceptMethod {
                                owner: context.prefixed(&concept.label),
                            },
                            name: curios_core::Global::Authored(
                                context.prefixed(&concept.label).with(&field.label),
                            ),
                            island: context.island(),
                            type_,
                            body,
                        }));
                    }
                }
                flat_items.push(match formers.len() {
                    1 => FlatItem::Let(formers.pop().expect("a `concept` item has a member")),
                    _ => FlatItem::Rec(formers),
                });
            }
            // A witness desugars to an anonymous top-level definition satisfy (tele) -> C(args) = C(args) { f = e, … }; and marks it for registration in the program-wide witness table. It gets an *identity*, not a manufactured name: a `satisfy` block has no name a programmer wrote, and the module a diagnostic reports for it comes from `Definition::island`.
            // A group `satisfy … and …` lowers to one `rec` item, so its members' anonymous names are bound in one another; a lone witness stays a `let` item, and may still resolve through its own entry — that repair is elaboration's, since a witness references itself by resolution rather than by name.
            TopItem::Witness(group) => {
                let mut items = group
                    .iter()
                    .map(|witness| {
                        let name = curios_core::Global::Witness(context.fresh_witness());
                        context.record_import_scope(Some(&name));

                        let concept_app =
                            witness_concept_application(&witness.concept, &witness.args);
                        // A written body is the concept literal over its fields alone, so every `use`-marked position is left to resolution; a body-less one is the `Derive` transient, spanned at the concept application so a refusal lands on the declaration. Either way the telescope below wraps it identically.
                        let body: Term = match &witness.body {
                            Some(fields) => Subterm::StructLit(StructLit {
                                head: witness.concept.clone(),
                                params: witness.args.clone(),
                                entries: fields
                                    .iter()
                                    .map(|field| {
                                        StructLitEntry::Field(TupleField {
                                            label: Some(field.label.clone()),
                                            func_params: field.func_params.clone(),
                                            value: field.value.clone(),
                                        })
                                    })
                                    .collect(),
                            })
                            .into(),
                            None => {
                                let derive: Term = Subterm::Derive.into();
                                match witness.args.first().and_then(|arg| arg.span()) {
                                    Some(span) => derive.with_span(span.clone()),
                                    None => derive,
                                }
                            }
                        };

                        // Kept across the move into the signature: under a telescope the declared type is a `FuncType` built here, and a refusal about the *witness* — duplicate, orphan, unkeyable, irregular premise — belongs on the concept application it wraps rather than nowhere.
                        let declared = concept_app.span().cloned();
                        let signature = if witness.params.is_empty() {
                            LetSignature::Name {
                                type_: Some(concept_app),
                                body,
                            }
                        } else {
                            LetSignature::Func {
                                params: witness.params.clone(),
                                output: concept_app,
                                body,
                            }
                        };

                        let lower = Lowerer::new(context, Some(name));
                        let signature = lower.signature(&signature);
                        let item = FlatLet {
                            kind: curios_core::DefinitionKind::Witness,
                            span: None,
                            name,
                            island: context.island(),
                            // Without a telescope the type is the application itself, which carries this span already.
                            type_: lower.signature_type(&signature, declared.as_ref())?,
                            body: lower.signature_value(&signature, |body| lower.value(body))?,
                        };
                        witnesses.insert(name);
                        Ok(item)
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                flat_items.push(match items.len() {
                    1 => FlatItem::Let(items.pop().expect("a `satisfy` item has a member")),
                    _ => FlatItem::Rec(items),
                });
            }
        }
    }

    Ok(())
}

/// What a unit is lowered from: the modules under the prefixes it claims, and — for the one unit that has one — its entrypoint.
///
/// **One resolver.** A tree parsed from a file graph, as the entry program is, and one handed over already parsed, as the fixed prelude is, differ in nothing but where a module body comes from, which is exactly the question a [`RootSource`] answers. The one genuine difference is this: an executable carries a tail expression and owns the empty prefix, and a library does neither.
pub struct UnitSource<'a> {
    entrypoint: Option<&'a Entrypoint>,
    source: &'a RootSource,
    /// The prefixes this unit declared a dependency on, and so the only ones its names may resolve into — or `None` for every unit in scope.
    ///
    /// `None` is not "nothing declared" but "the caller did not decide", which is every caller that has no manifest to read one out of: a fold whose order is the whole of its dependency information cannot narrow, so the default has to be every predecessor. A unit that *does* declare them names them here, and a predecessor it did not name is then in the fold — contributing its identities, its universe seeds and its erased operands — while being unspellable.
    visible: Option<Vec<Qualifier>>,
}

impl<'a> UnitSource<'a> {
    /// The entry program, its own modules resolved through `source`.
    pub fn entry(entrypoint: &'a Entrypoint, source: &'a RootSource) -> Self {
        Self {
            entrypoint: Some(entrypoint),
            source,
            visible: source.declared().map(<[Qualifier]>::to_vec),
        }
    }

    /// A unit with no entrypoint, under the prefixes `source` claims.
    pub fn mounted(source: &'a RootSource) -> Self {
        Self {
            entrypoint: None,
            source,
            visible: source.declared().map(<[Qualifier]>::to_vec),
        }
    }

    /// The same unit, seeing only `prefixes` of what is in scope — what a declared dependency list narrows it to.
    ///
    /// Takes the prefixes rather than the units, because a dependency is declared by name: the caller knows which prefixes a manifest listed and not which position each occupies in a fold it did not build.
    pub fn seeing(self, prefixes: Vec<Qualifier>) -> Self {
        Self {
            visible: Some(prefixes),
            ..self
        }
    }

    /// The mounts of `predecessors` this source may name, plus `own`.
    ///
    /// Filtered per *mount* rather than per unit: what a manifest declares is a prefix, and a unit claiming two prefixes would otherwise hand over the one nobody asked for along with the one somebody did. `own` is always included, since a unit does not declare a dependency on itself.
    ///
    /// **Declaring nothing means every open prefix, not every prefix.** A closed root — `/sys`, the compiler's own, which no manifest can name because it has no path — is in the fold of every compilation and in the default set of none. So the honest reading of "the caller did not decide" is "everything a program may name", and the standard library reaches `/sys` by being the one unit that declares it.
    ///
    /// Narrowing *resolution*, never auditing: the nominal audit reads the whole of `predecessors`, because a declaration in an unspellable predecessor still exists and a public exposure of it is still one.
    fn visible_mounts(&self, predecessors: &[&PreparedText], own: &[Mount]) -> Vec<Mount> {
        predecessors
            .iter()
            .flat_map(|unit| unit.mounts.iter())
            .filter(|mount| match &self.visible {
                Some(visible) => visible.contains(&mount.prefix),
                None => mount.kind != RootKind::Internal,
            })
            .chain(own.iter())
            .cloned()
            .collect()
    }

    /// The prefixes this source claims.
    ///
    /// The entry claims the empty one and nothing else, and that is stated here rather than read off the resolver: owning the empty prefix is what *makes* a unit the entry, so it cannot be something the way its files were found decided.
    fn mounts(&self) -> Vec<Mount> {
        match self.entrypoint {
            Some(_) => vec![Mount::new(Qualifier::empty(), RootKind::Ordinary)],
            None => self.source.mounts(),
        }
    }

    /// The directories this unit's modules are read from. See [`RootSource::directories`].
    pub fn directories(&self) -> Vec<&std::path::Path> {
        self.source.directories()
    }

    /// Every file this unit has read. See [`RootSource::reads`].
    pub fn reads(&self) -> Vec<(std::path::PathBuf, std::sync::Arc<curios_utilities::Source>)> {
        self.source.reads()
    }

    /// The prefixes this unit claims, which decide how its names are spelled and so which unit it is.
    pub fn claims(&self) -> Vec<Mount> {
        self.mounts()
    }

    /// The prefixes this unit declared a dependency on, when it declared any — the other half of which unit this is.
    ///
    /// Two units of one source and the same predecessors differing only in what they declared are two lowerings, because a name resolves in one and is refused in the other. So a store addresses them apart, which is what this is read for.
    pub fn declared(&self) -> Option<&[Qualifier]> {
        self.visible.as_deref()
    }

    /// The prefix this unit claims, as a name to report it by — `/json` for a mounted package.
    ///
    /// The root for the entry, which owns the empty prefix: a caller that wants to *name* the entry knows what was asked for and this does not, so it supplies its own.
    ///
    /// A [`Qualifier`] rather than the text of one, so a reporting caller renders the leading `/` where every other name renders it instead of receiving it already spelled.
    pub fn prefix(&self) -> Qualifier {
        self.mounts()
            .first()
            .map(|mount| mount.prefix)
            .unwrap_or_default()
    }

    /// The mount this unit documents, with its description, when its resolver marked one. An entry documents nothing: a program has no consumer.
    fn documented(&self) -> Option<(Qualifier, Option<String>)> {
        match self.entrypoint {
            Some(_) => None,
            None => self.source.documented_mount(),
        }
    }

    /// The entrypoint this source carries, for the one unit that has one.
    fn entrypoint(&self) -> Option<&Entrypoint> {
        self.entrypoint
    }

    /// The items of the compilation root: the entry's own, and none for a unit with no entrypoint — whose headers sit under its own prefixes, the root belonging to no unit at all.
    fn root_items(&self) -> &'a [TopItem] {
        self.entrypoint
            .map_or(&[], |entrypoint| &entrypoint.module.items)
    }
}

/// Lower one unit against the units already lowered.
///
/// **One walk for every configuration.** Where a unit's items sit, whether anything is already in scope, and where four counters start are all arguments here; a copy of the walk per configuration would agree with the others only by being read, which is the shape every configuration-dependent defect in this stage has had.
///
/// `predecessors` are in dependency order. Reads span them and the unit's own; writes only ever touch the unit's own, which is what makes a layer sufficient rather than a copy.
///
/// An entry source's final term is lowered too — a refusal in it is this lowering's — and handed back only by the entry's own spelling, [`into_core_with_prelude`]: a unit is a [`curios_core::Module`] alone.
pub fn into_core_unit(
    source: &UnitSource<'_>,
    predecessors: &[&PreparedText],
    syntax: &SyntaxRegistry,
) -> Result<PreparedText, Error> {
    lower_unit(source, predecessors, syntax).map(|(unit, _)| unit)
}

/// [`into_core_unit`], with the final term an entry source closes with, lowered beside the unit.
fn lower_unit(
    source: &UnitSource<'_>,
    predecessors: &[&PreparedText],
    syntax: &SyntaxRegistry,
) -> Result<(PreparedText, Option<curios_core::Entrypoint>), Error> {
    curios_profile::profile!("into_core_unit");
    curios_utilities::grown(|| into_core_unit_within(source, predecessors, syntax))
}

fn into_core_unit_within(
    source: &UnitSource<'_>,
    predecessors: &[&PreparedText],
    syntax: &SyntaxRegistry,
) -> Result<(PreparedText, Option<curios_core::Entrypoint>), Error> {
    // Every predecessor, in every reading but one. Per-dependency visibility narrows nothing here: a prefix this unit did not declare stays discoverable and its names stay resolvable, and what refuses is the reference itself — see `Reach::guard`. Hiding the tables instead would turn an undeclared dependency into an unbound name, which is the one diagnostic an undeclared dependency must not produce.
    let predecessor_tables = predecessors
        .iter()
        .map(|unit| &unit.table)
        .collect::<Vec<_>>();
    let predecessor_public = predecessors
        .iter()
        .map(|unit| &unit.public)
        .collect::<Vec<_>>();
    let predecessor_modules = predecessors
        .iter()
        .map(|unit| &unit.core)
        .collect::<Vec<_>>();
    let predecessor_mounts = predecessors
        .iter()
        .flat_map(|unit| unit.mounts.iter().cloned())
        .collect::<Vec<_>>();

    // The prefixes this unit claims. The entry claims the empty one and nothing else; a mounted unit claims what its source does.
    let own = source.mounts();

    // Claimed prefixes must be distinct, and this is decided before discovery — otherwise the collision surfaces from `insert_child` as an ordinary duplicate declaration, which names the label but not what else claimed it.
    //
    // Distinctness *is* disjointness here, because a prefix is one segment: no mount can lie beneath another, and the entry's own `mod json` claims `/json` against a mounted package of that name exactly as a second mount would. The empty prefix takes no part — every name lies within the compilation root by construction, which is what makes it the root.
    //
    // Mount-set disjointness is what `Scoped`'s shadowing rule, the registries' duplicate-key rejection and the `ffi` import namespace all rest on, so it is checked once here rather than assumed three times.
    for (claim, prefix) in claims(source, &own) {
        if let Some(earlier) = predecessor_mounts
            .iter()
            .find(|earlier| !earlier.prefix.is_root() && earlier.prefix == prefix)
        {
            return Err(Error::MountCollision {
                claim,
                claimed: earlier.prefix.join(),
                claimant: "a unit already in scope".to_string(),
            });
        }
    }

    let Resolved {
        mut table,
        modules,
        mod_spans,
    } = Resolved::of(source, &predecessor_tables, &predecessor_mounts, &own)?;

    // Every prefix this compilation mounts — the predecessors', then this unit's. Resolution asks the whole set; the lowered module records only `own`, because a module states what its own unit provides.
    let mounts = predecessor_mounts
        .iter()
        .cloned()
        .chain(own.iter().cloned())
        .collect::<Vec<_>>();
    // The subset this unit declared, plus its own. The default is every predecessor, so this differs from `mounts` only for a caller that had a manifest to read one out of.
    let visible_mounts = source.visible_mounts(predecessors, &own);
    let reach = Reach::over(&mounts, &visible_mounts);

    let public = interface::resolve_unit(
        source,
        &own,
        &modules,
        &mut table,
        reach,
        Scoped::over(&predecessor_public),
    )?;

    // Every counter starts at zero. No term in scope carries a local, a metavariable or a universe metavariable — a stored unit is refused one — so nothing minted here can alias an identity already there, and what this unit mints depends on nothing compiled before it.
    let metavars = Entropy::<usize>::new();
    let universes = Entropy::<usize>::new();
    let binders = Entropy::<usize>::new();
    // No floor: a witness's ordinal is scoped to its declaring module, which lies within this unit's mounts, and those are disjoint from every predecessor's, so nothing it mints can collide with anything already stored.
    let witness_ids = RefCell::new(BTreeMap::new());
    let unbound = RefCell::new(BTreeMap::new());
    let imports = RefCell::new(curios_core::Imports::default());
    let sites = RefCell::new(Vec::new());
    let reached = RefCell::new(BTreeSet::new());
    let lints = RefCell::new(Vec::new());

    let universe_role = Cell::new(curios_core::UniverseRole::Flexible);
    // This unit's own seed table, from index zero, in step with `universes`.
    let universe_seeds = RefCell::new(Vec::new());
    let universe_allocations = RefCell::new(HashMap::new());

    let mut context = Context::new(
        &table,
        &public,
        reach,
        &metavars,
        &universes,
        &universe_role,
        &universe_seeds,
        &universe_allocations,
        &binders,
        &witness_ids,
        &unbound,
        &imports,
        &sites,
        &reached,
        &lints,
        syntax,
    );
    // Every named prefix in the compilation binds its own one-segment name. No two can repeat it: the disjointness check above refuses a unit claiming what a predecessor already holds, and the predecessors' own mounts were pairwise disjoint when each was compiled. The entry's prefix is the empty one, which has no name to bind.
    for mount in &mounts {
        if !mount.prefix.is_root() {
            context.insert_scope(mount.prefix.head().to_string(), mount.prefix)?;
        }
    }

    let mut flat_items = Vec::new();
    // This unit's own, never the predecessors' extended in place. What a predecessor declares stays its own, and the one pass here that asks about one — the public-exposure audit, whose alias walk may land on a predecessor's type — takes them to query rather than as entries copied into these maps. The dependency sort below never needed it: it looks a declaration up only for names an item itself declares.
    let mut induct_decls = BTreeMap::new();
    let mut struct_decls = BTreeMap::new();
    let mut concepts = BTreeMap::new();
    let mut witnesses = BTreeSet::new();
    let mut tests = Vec::new();
    let mut foreigns = ForeignStore::new();
    let mut broken = Vec::new();

    // The compilation root's own items — the entry's, and none for a unit with no entrypoint — then one pass per prefix this unit claims. Exactly one of the two does any work, because owning the empty prefix is what makes a unit the entry.
    process_items(
        source.root_items(),
        &mut context,
        &mut flat_items,
        &mut induct_decls,
        &mut struct_decls,
        &mut concepts,
        &mut witnesses,
        &mut tests,
        &mut foreigns,
        &mut broken,
        &modules,
    )?;

    for mount in own.iter().filter(|mount| !mount.prefix.is_root()) {
        let content = modules
            .get(&mount.prefix)
            .expect("a mounted prefix was loaded during discovery");

        let mut nested = context.nested(mount.prefix.head());

        process_items(
            &content.items,
            &mut nested,
            &mut flat_items,
            &mut induct_decls,
            &mut struct_decls,
            &mut concepts,
            &mut witnesses,
            &mut tests,
            &mut foreigns,
            &mut broken,
            &modules,
        )?;
    }

    // The entrypoint, for the one unit that has one. Its tail closes the root body, so the imports in scope there are the last the root saw.
    context.record_import_scope(None);
    let entry = {
        // Scoped so the lowerer drops, and reports its lints, before the tables the context borrows are moved into the result below.
        let lower = Lowerer::new(&context, None);
        match source.entrypoint() {
            Some(entrypoint) => Some(curios_core::Entrypoint {
                body: lower.value(&entrypoint.tail)?,
                // The grammar has no position for the type an entry is judged at: whoever compiles it states one.
                type_: None,
            }),
            None => None,
        }
    };

    audit_public_exposures(
        &public,
        &table,
        &flat_items,
        NominalScope::new(&predecessor_modules, &induct_decls, &struct_decls),
    )?;

    let dead = unused_declarations(&Declarations {
        items: &flat_items,
        entry: entry.as_ref(),
        table: &table,
        public: &public,
        own: &own,
        mod_spans: &mod_spans,
        induct_decls: &induct_decls,
        struct_decls: &struct_decls,
        syntax,
    });

    // This unit's own items alone. A predecessor reaches later stages as an *environment* they are seeded from — `Globals` at the certifier, a replayed context at elaboration and erasure — and copying its items into every compilation only ever existed so those stages could then skip them again by index. See `documentation/design/compilation/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md`.
    let items = order_flat_items(
        flat_items,
        &predecessor_modules,
        &induct_decls,
        &struct_decls,
        syntax,
    )?
    .into_iter()
    .map(FlatItem::into_core)
    .collect();

    // Read last, when the export view is final and every definition's import scope has been recorded, and before the tables below are taken out of their scoped views.
    let documentation = source.documented().map(|(prefix, description)| {
        // The predecessors' own records, for the declarations this unit adopts out of a root a consumer cannot name. A unit keeps no surface tree, so the record it carried away is the only place its declarations survive — see `document`'s `records`.
        let records = predecessors
            .iter()
            .filter_map(|unit| unit.documentation())
            .collect::<Vec<_>>();

        document(
            &modules,
            &table,
            &public,
            &imports.borrow(),
            &prefix,
            &visible_mounts,
            &records,
            description,
        )
    });

    let paths = writable_paths(&public, &table, &reach, &own);
    let unit = PreparedText {
        mounts: own.clone(),
        foreigns,
        table: table.into_own().into_iter().collect(),
        public: public.into_own().into_iter().collect(),
        core: curios_core::Module {
            items,
            mounts: own,
            induct_decls,
            struct_decls,
            concepts,
            witnesses,
            tests,
        },
        minted: curios_core::Minted {
            binders: binders.count(),
            metavariables: metavars.count(),
            universes: universe_seeds.into_inner(),
        },
        unbound: unbound.into_inner(),
        spellings: curios_core::Spellings {
            imports: imports.into_inner(),
            paths,
        },
        broken,
        lints: ordered(
            unused_imports(sites.into_inner())
                .into_iter()
                .chain(lints.into_inner())
                .chain(dead)
                .collect(),
        ),
        credited: Vec::new(),
        reached: reached.into_inner(),
        documentation,
    };

    Ok((unit, entry))
}

/// Lower a whole [`Entrypoint`] with nothing in scope, as `curios-text`'s own stage tests do.
pub fn into_core(
    entrypoint: &Entrypoint,
    loader: &RootSource,
    syntax: &SyntaxRegistry,
) -> Result<(curios_core::Program, curios_core::Minted, ForeignStore), Error> {
    let (unit, entry) = lower_unit(&UnitSource::entry(entrypoint, loader), &[], syntax)?;

    Ok((
        curios_core::Program {
            module: unit.core,
            entry: entry.expect("an entry source's parse holds its final term"),
        },
        unit.minted,
        unit.foreigns,
    ))
}

/// Resolve and lower one fixed root once for build-time archival, against the fixed roots already lowered.
///
/// `predecessors` is empty for the first root and holds the roots before it for every later one: the fixed prelude is a fold like any other, so the root that references another is lowered after it rather than beside it.
pub fn prepare_prelude(
    input: &RootSource,
    predecessors: &[&PreparedText],
    syntax: &SyntaxRegistry,
) -> Result<PreparedText, Error> {
    into_core_unit(&UnitSource::mounted(input), predecessors, syntax)
}

/// The entry program lowered: its module and the term it closes with, what its lowering minted, which elaboration's counters start above, its `foreign` rows, the unresolved-name table its `unbound variable` reports read from, and its lints.
pub struct LoweredEntry {
    pub program: curios_core::Program,
    pub minted: curios_core::Minted,
    pub foreigns: ForeignStore,
    /// See [`PreparedText::unbound`].
    pub unbound: BTreeMap<curios_core::Free, Vec<Qualifier>>,
    /// See [`PreparedText::spellings`].
    pub spellings: curios_core::Spellings,
    /// See [`PreparedText::lints`].
    pub lints: Vec<Lint>,
    /// See [`PreparedText::broken`].
    pub broken: Vec<BrokenItem>,
    /// See [`PreparedText::reached`].
    pub reached: BTreeSet<Qualifier>,
}

/// The `unused-import` lints: every site no reference resolved through, a re-export excepted.
fn unused_imports(sites: Vec<UseSite>) -> Vec<Lint> {
    sites
        .into_iter()
        .filter(|site| !site.used && !site.exported)
        .map(|site| match &site.what {
            UseSiteKind::Selector(label) => Lint::unused_import(label),
            UseSiteKind::Glob(path) => Lint::unused_glob(site.span.as_ref(), path),
        })
        .collect()
}

/// Lower the entry program against the units already lowered.
pub fn into_core_with_prelude(
    entrypoint: &Entrypoint,
    loader: &RootSource,
    predecessors: &[&PreparedText],
    syntax: &SyntaxRegistry,
) -> Result<LoweredEntry, Error> {
    let (unit, entry) = lower_unit(&UnitSource::entry(entrypoint, loader), predecessors, syntax)?;

    Ok(LoweredEntry {
        program: curios_core::Program {
            module: unit.core,
            entry: entry.expect("an entry source's parse holds its final term"),
        },
        minted: unit.minted,
        foreigns: unit.foreigns,
        unbound: unit.unbound,
        spellings: unit.spellings,
        lints: unit.lints,
        reached: unit.reached,
        broken: unit.broken,
    })
}
