use {
    super::PublicInterface,
    super::Scoped,
    crate::{Error, Label, Lint, Name},
    curios_utilities::{Entropy, InfixOp, Mount, Qualifier, Span, SyntaxRegistry},
    std::{
        cell::{Cell, RefCell},
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    },
};

/// One `use` selector or glob as resolution met it: what it imports, whether it re-exports, and whether any reference resolved through it — the fact the `unused-import` lint reads. Shared across the unit's contexts as the import table is, since a lint is about the unit.
pub(super) struct UseSite {
    pub span: Option<Span>,
    pub what: UseSiteKind,
    /// A `pub use` is a re-export, which is its own use.
    pub exported: bool,
    pub used: bool,
}

pub(super) enum UseSiteKind {
    Selector(Label),
    /// The glob's path as written, `/std/*`, for the message.
    Glob(String),
}

#[derive(Clone)]
pub(super) struct FlatLet {
    pub name: curios_core::Global,
    pub kind: curios_core::DefinitionKind,
    /// Where the declaration's name was written — what the `unused-declaration` lint underlines. `None` for a definition the compiler named: a constructor, a method wrapper, a witness, a test, or a `foreign` row the prelude synthesized.
    pub span: Option<Span>,
    pub island: Qualifier,
    pub type_: curios_core::Term,
    pub body: curios_core::Term,
}

impl FlatLet {
    pub(super) fn into_core(self) -> curios_core::Definition {
        curios_core::Definition {
            island: self.island,
            name: self.name,
            kind: self.kind,
            universe_context: curios_core::UniverseContext::empty(),
            // Lowering cannot know this. `curios_elab::record_totality` computes the definition's totality after elaboration and zonking and writes it back here.
            totality: curios_core::Totality::default(),
            type_: self.type_,
            body: self.body,
        }
    }

    /// Whether this definition's type or body names the definition itself — what makes a lone `let` the recursive group of one the kernel needs it to be.
    pub(super) fn mentions_itself(&self) -> bool {
        self.type_
            .free_vars_shared()
            .iter()
            .chain(self.body.free_vars_shared())
            .any(|free| free.as_global() == Some(&self.name))
    }
}

#[derive(Clone)]
pub(super) enum FlatItem {
    Let(FlatLet),
    Rec(Vec<FlatLet>),
}

impl FlatItem {
    pub(super) fn names(&self) -> Vec<curios_core::Global> {
        match self {
            FlatItem::Let(let_) => vec![let_.name.clone()],
            FlatItem::Rec(lets) => lets.iter().map(|let_| let_.name.clone()).collect(),
        }
    }

    /// The item's members: one for a `let`, every one of a group.
    pub(super) fn lets(&self) -> &[FlatLet] {
        match self {
            FlatItem::Let(let_) => std::slice::from_ref(let_),
            FlatItem::Rec(lets) => lets.as_slice(),
        }
    }

    /// Every global this item names — the reachability prune's edges (see [`FlatItem::names`]). Locals are dropped: an item's binders are its own business, and reachability is a question about definitions.
    pub(super) fn free_vars(&self) -> HashSet<curios_core::Global> {
        let lets = match self {
            FlatItem::Let(let_) => std::slice::from_ref(let_),
            FlatItem::Rec(lets) => lets.as_slice(),
        };

        lets.iter()
            .flat_map(|let_| {
                // Construction head names (`Struct`/`Variant`/type-former normal forms) are reachability edges too — a body that *builds* a struct (the string-literal meta-emitter's `/std/Str/Str`) must keep its backing type-former and field-type definitions alive even though no `Var` names them. See `Subterm::construction_names`.
                let_.type_
                    .free_vars()
                    .into_iter()
                    .chain(let_.body.free_vars())
                    .filter_map(|name| name.as_global().cloned())
                    .chain(let_.type_.construction_names())
                    .chain(let_.body.construction_names())
            })
            .collect()
    }

    /// Every infix operator this item's terms dispatch through — the operator half of `order_flat_items`' witness edges. The wrapper half rides on [`FlatItem::free_vars`], since a written `C/method` reference is an ordinary global.
    pub(super) fn infix_ops(&self) -> HashSet<InfixOp> {
        let lets = match self {
            FlatItem::Let(let_) => std::slice::from_ref(let_),
            FlatItem::Rec(lets) => lets.as_slice(),
        };

        lets.iter()
            .flat_map(|let_| {
                let_.type_
                    .infix_ops()
                    .into_iter()
                    .chain(let_.body.infix_ops())
            })
            .collect()
    }

    pub(super) fn into_core(self) -> curios_core::Item {
        match self {
            FlatItem::Let(let_) => curios_core::Item::Let(let_.into_core()),
            FlatItem::Rec(items) => curios_core::Item::Rec(curios_core::RecItem::new(
                items.into_iter().map(FlatLet::into_core).collect(),
            )),
        }
    }
}

// The direct interface of a module: every declared label (public *and* private) in each namespace, with its visibility. This is the per-module body view used for lexical scope during elaboration, and to tell private from absent when a public lookup misses.
#[derive(Clone, Copy)]
#[curios_archive::archived]
enum ChildInfo {
    Ordinary { vis_pub: bool },
    InductConstructors { vis_pub: bool, rep_pub: bool },
}

impl ChildInfo {
    fn is_public(self) -> bool {
        match self {
            ChildInfo::Ordinary { vis_pub } => vis_pub,
            ChildInfo::InductConstructors { vis_pub, rep_pub } => vis_pub && rep_pub,
        }
    }

    fn is_opaque_constructor_namespace(self) -> bool {
        matches!(self, ChildInfo::InductConstructors { rep_pub: false, .. })
    }
}

#[derive(Clone)]
#[curios_archive::archived]
pub(crate) struct ModuleInfo {
    #[archived_with(crate::OrderedMap)]
    children: HashMap<String, ChildInfo>,
    #[archived_with(crate::OrderedMap)]
    bindings: HashMap<String, bool>,
}

impl ModuleInfo {
    pub(super) fn new() -> Self {
        Self {
            children: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    pub(super) fn insert_child(&mut self, label: &Label, vis_pub: bool) -> Result<(), Error> {
        if self.children.contains_key(label.as_str()) {
            return Err(duplicate(label));
        }

        self.children
            .insert(label.to_string(), ChildInfo::Ordinary { vis_pub });
        Ok(())
    }

    pub(super) fn insert_induct_child(
        &mut self,
        label: &Label,
        vis_pub: bool,
        rep_pub: bool,
    ) -> Result<(), Error> {
        if self.children.contains_key(label.as_str()) {
            return Err(duplicate(label));
        }

        self.children.insert(
            label.to_string(),
            ChildInfo::InductConstructors { vis_pub, rep_pub },
        );
        Ok(())
    }

    pub(super) fn insert_binding(&mut self, label: &Label, vis_pub: bool) -> Result<(), Error> {
        if self.bindings.contains_key(label.as_str()) {
            return Err(duplicate(label));
        }

        self.bindings.insert(label.to_string(), vis_pub);
        Ok(())
    }

    pub(crate) fn get_child(&self, label: &str) -> Option<bool> {
        self.children.get(label).copied().map(ChildInfo::is_public)
    }

    pub(super) fn is_opaque_constructor_child(&self, label: &str) -> bool {
        self.children
            .get(label)
            .copied()
            .is_some_and(ChildInfo::is_opaque_constructor_namespace)
    }

    pub(crate) fn get_binding(&self, label: &str) -> Option<bool> {
        self.bindings.get(label).copied()
    }

    /// Every declared child module with its own visibility bit, for the audience fixed point — which needs the private ones too, since they are visible within this module's subtree.
    pub(super) fn children(&self) -> impl Iterator<Item = (&str, bool)> {
        self.children
            .iter()
            .map(|(label, info)| (label.as_str(), info.is_public()))
    }

    /// Every declared binding with its own visibility bit. See [`Self::children`].
    pub(super) fn bindings(&self) -> impl Iterator<Item = (&str, bool)> {
        self.bindings
            .iter()
            .map(|(label, vis_pub)| (label.as_str(), *vis_pub))
    }

    pub(super) fn public_children(&self) -> Vec<String> {
        self.children
            .iter()
            .filter(|(_, info)| info.is_public())
            .map(|(label, _)| label.clone())
            .collect()
    }

    pub(super) fn public_bindings(&self) -> Vec<String> {
        self.bindings
            .iter()
            .filter(|(_, vis_pub)| **vis_pub)
            .map(|(label, _)| label.clone())
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct UseResolved {
    module: Option<Qualifier>,
    binding: Option<Qualifier>,
}

// The per-body elaboration context. `table`/`public` are frozen interface views, shared read-only across all nested contexts. `qualifiers`/`bindings` are the lexical scope of the module body being elaborated, populated source-ordered by declarations and `use` imports.
pub(super) struct Context<'a> {
    prefix: Qualifier,
    // Every prefix this compilation mounts and which of them this unit may name — see `super::Reach`. Shared read-only by every nested context, like `table`/`public`: which mount owns a module is `Mount::owning` over its qualifier, so nesting carries nothing about roots and nothing re-derives one from a string.
    reach: super::Reach<'a>,
    table: &'a Scoped<'a, ModuleInfo>,
    public: &'a Scoped<'a, PublicInterface>,
    qualifiers: HashMap<String, Qualifier>,
    bindings: HashMap<String, Qualifier>,
    // Shared, program-global metavariable-id counter. The whole program folds into one `curios_core::Term`, so holes in different module bodies (each its own `Context` via `nested`) must draw from the same monotonic source. Shared by reference (like `table`/`public`) and `Cell`-backed so it survives `Lowerer`'s immutable `&Context` borrow.
    metavars: &'a Entropy,
    universes: &'a Entropy,
    universe_role: &'a Cell<curios_core::UniverseRole>,
    universe_seeds: &'a RefCell<Vec<curios_core::UniverseSeed>>,
    universe_allocations: &'a RefCell<HashMap<Span, curios_core::UniverseMetaId>>,
    // Shared counter for every binder identity a lowered term closes over. Threaded (not a process-global atomic) for determinism: two runs over the same source must mint the same identities, or terms that should be equal would differ.
    binders: &'a Entropy,
    // One ordinal counter per mount. A `satisfy` declaration is anonymous, so its identity is minted rather than written — and it is scoped to the mount that declares it, because an ordinal alone would mean something only in the compilation that handed it out.
    witnesses: &'a RefCell<BTreeMap<Qualifier, u32>>,
    // Every bare name that resolved to nothing, keyed by the binder identity it lowered to, with the public bindings in scope of that name. Shared across nested contexts like the counters, because the table is the unit's: `curios-elab` reports the unbound binder, and this is what lets its report say what the reader probably meant.
    unbound: &'a RefCell<BTreeMap<curios_core::Free, Vec<Qualifier>>>,
    // Every binding a `use` brought into some lexical scope of this unit, with the path the reader wrote it under, and per definition the ones in scope where it was written — see `curios_core::Imports`. The entries are shared across nested contexts like `unbound`, and for the same reason: Core keeps the canonical name, and only this stage knows the spelling that resolves at the use site, which is what a goal report's candidate must be pasteable under. The scope is this body's alone: `use` binds from its position to the end of the body it is written in, and a nested body starts empty, so `in_scope` is per context and a snapshot of it is taken at each item.
    imports: &'a RefCell<curios_core::Imports>,
    in_scope: Vec<usize>,
    // Every `use` selector and glob of the unit, in the order resolution met them, and per lexical scope the site each imported label came through — a reference resolving through a label marks its site used. Declarations enter `bindings` and `qualifiers` without a site, so nothing marks for them.
    sites: &'a RefCell<Vec<UseSite>>,
    current_site: Option<usize>,
    binding_sites: HashMap<String, usize>,
    qualifier_sites: HashMap<String, usize>,
    // The prefix of every mount some reference of this unit resolved into — what decides whether a declared dependency is used. Recorded where a target is resolved, never where it is spelled, so an absolute path and an import count alike.
    reached: &'a RefCell<BTreeSet<Qualifier>>,
    // Every lint a `Lowerer` of this unit decided — see `Lowerer::flush`.
    lints: &'a RefCell<Vec<Lint>>,
    syntax: &'a SyntaxRegistry,
}

impl<'a> Context<'a> {
    #[allow(clippy::too_many_arguments)]
    pub(super) fn new(
        table: &'a Scoped<'a, ModuleInfo>,
        public: &'a Scoped<'a, PublicInterface>,
        reach: super::Reach<'a>,
        metavars: &'a Entropy,
        universes: &'a Entropy,
        universe_role: &'a Cell<curios_core::UniverseRole>,
        universe_seeds: &'a RefCell<Vec<curios_core::UniverseSeed>>,
        universe_allocations: &'a RefCell<HashMap<Span, curios_core::UniverseMetaId>>,
        binders: &'a Entropy,
        witnesses: &'a RefCell<BTreeMap<Qualifier, u32>>,
        unbound: &'a RefCell<BTreeMap<curios_core::Free, Vec<Qualifier>>>,
        imports: &'a RefCell<curios_core::Imports>,
        sites: &'a RefCell<Vec<UseSite>>,
        reached: &'a RefCell<BTreeSet<Qualifier>>,
        lints: &'a RefCell<Vec<Lint>>,
        syntax: &'a SyntaxRegistry,
    ) -> Context<'a> {
        Context {
            prefix: Qualifier::empty(),
            reach,
            table,
            public,
            qualifiers: HashMap::new(),
            bindings: HashMap::new(),
            metavars,
            universes,
            universe_role,
            universe_seeds,
            universe_allocations,
            binders,
            witnesses,
            unbound,
            imports,
            in_scope: Vec::new(),
            sites,
            current_site: None,
            binding_sites: HashMap::new(),
            qualifier_sites: HashMap::new(),
            reached,
            lints,
            syntax,
        }
    }

    pub(super) fn nested(&self, label: &str) -> Context<'a> {
        Context {
            prefix: self.prefix.with(label),
            reach: self.reach,
            table: self.table,
            public: self.public,
            qualifiers: HashMap::new(),
            bindings: HashMap::new(),
            metavars: self.metavars,
            universes: self.universes,
            universe_role: self.universe_role,
            universe_seeds: self.universe_seeds,
            universe_allocations: self.universe_allocations,
            binders: self.binders,
            witnesses: self.witnesses,
            unbound: self.unbound,
            imports: self.imports,
            in_scope: Vec::new(),
            sites: self.sites,
            current_site: None,
            binding_sites: HashMap::new(),
            qualifier_sites: HashMap::new(),
            reached: self.reached,
            lints: self.lints,
            syntax: self.syntax,
        }
    }

    pub(super) fn report(&self, lints: impl IntoIterator<Item = Lint>) {
        self.lints.borrow_mut().extend(lints);
    }

    /// Open the `use` site every label imported until [`Self::close_site`] is credited to.
    pub(super) fn open_site(&mut self, site: UseSite) {
        let mut sites = self.sites.borrow_mut();
        sites.push(site);
        self.current_site = Some(sites.len() - 1);
    }

    pub(super) fn close_site(&mut self) {
        self.current_site = None;
    }

    /// A reference resolved through the binding `label` brought into this scope: its `use`, if one did, is used.
    pub(super) fn note_binding_use(&self, label: &str) {
        if let Some(&site) = self.binding_sites.get(label) {
            self.sites.borrow_mut()[site].used = true;
        }
    }

    fn note_qualifier_use(&self, label: &str) {
        if let Some(&site) = self.qualifier_sites.get(label) {
            self.sites.borrow_mut()[site].used = true;
        }
    }

    /// A reference was *written* under `spelled`: the mount owning that path is reached. The entry's empty prefix owns every name nothing else claims, and is recorded like any other; the consumer knows which prefixes it asked about.
    ///
    /// **The path the reader wrote, never the one it resolved to.** A re-export means the two differ: `/std/Nat` is `pub use /sys/{Nat}`, so resolving `use /std/{Nat}` lands on `/sys/Nat` and a record of the *target* would say every program reached `/sys`. What a declared dependency is answerable to is what the reader spelled, which is also the only thing they can change.
    fn note_spelled(&self, spelled: &Qualifier) {
        if let Some(mount) = Mount::owning(self.reach.mounts(), spelled) {
            self.reached.borrow_mut().insert(mount.prefix.clone());
        }
    }

    /// Mint a fresh, program-globally-unique metavariable id for a surface hole.
    pub(super) fn fresh_metavar(&self) -> usize {
        self.metavars.fresh()
    }

    pub(super) fn fresh_universe(&self, span: Option<&Span>) -> curios_core::Level {
        if let Some(span) = span
            && let Some(id) = self.universe_allocations.borrow().get(span)
        {
            assert_eq!(
                self.universe_seeds.borrow()[id.0].role,
                self.universe_role(),
                "one written Type was lowered under conflicting universe roles"
            );
            return curios_core::Level::meta(*id);
        }

        let id = self.universes.fresh();
        let mut seeds = self.universe_seeds.borrow_mut();
        assert_eq!(id, seeds.len(), "universe seeds parallel their dense ids");
        seeds.push(curios_core::UniverseSeed {
            role: self.universe_role(),
            origin: span
                .cloned()
                .map(|span| curios_core::UniverseConstraintOrigin {
                    span: Some(span),
                    kind: curios_core::UniverseConstraintKind::WrittenType,
                    declaration: (!self.prefix.is_root()).then(|| self.prefix.join()),
                    binder: None,
                }),
        });
        let id = curios_core::UniverseMetaId(id);
        if let Some(span) = span {
            self.universe_allocations
                .borrow_mut()
                .insert(span.clone(), id);
        }
        curios_core::Level::meta(id)
    }

    pub(super) fn universe_role(&self) -> curios_core::UniverseRole {
        self.universe_role.get()
    }

    pub(super) fn with_universe_role<T>(
        &self,
        role: curios_core::UniverseRole,
        f: impl FnOnce() -> Result<T, Error>,
    ) -> Result<T, Error> {
        let old = self.universe_role.replace(role);
        let result = f();
        self.universe_role.set(old);
        result
    }

    /// Mint a witness identity under the mount that declares it.
    ///
    /// A `satisfy` declaration is anonymous by design, so it gets an identity rather than a manufactured name — see [`curios_core::Global::Witness`]. The ordinal is per mount rather than per program: two units both counting from zero is exactly what makes a bare ordinal unstorable, and the mount is what makes the pair disjoint without either unit knowing the other exists.
    ///
    /// The mount is the one this context's prefix lies within. A prefix owned by nothing is the synthetic compilation root, which only arises while no unit has claimed anything yet.
    pub(super) fn fresh_witness(&self) -> curios_core::WitnessId {
        let mount = Mount::owning(self.reach.mounts(), &self.prefix)
            .map(|mount| mount.prefix.clone())
            .unwrap_or_default();

        let mut witnesses = self.witnesses.borrow_mut();
        let ordinal = witnesses.entry(mount.clone()).or_default();
        let minted = curios_core::WitnessId::new(mount, *ordinal);
        *ordinal += 1;

        minted
    }

    /// Mint a binder identity, rendering as `hint`.
    ///
    /// Every binder a lowered term closes over comes from here, including the continuation binders `!` desugaring introduces. `curios-elab` mints more while elaborating and seeds its counter above [`PreparedText::binder_floor`](super::PreparedText::binder_floor), so the two sources share one identity space without colliding.
    pub(super) fn fresh_binder(&self, hint: Option<&str>) -> curios_core::Free {
        curios_core::Free::local(
            u32::try_from(self.binders.fresh()).expect("binder space exhausted"),
            hint,
        )
    }

    pub(super) fn syntax(&self) -> SyntaxRegistry {
        *self.syntax
    }

    pub(super) fn prefixed(&self, label: &str) -> Qualifier {
        self.prefix.with(label)
    }

    /// The exact source module declarations lowered in this context belong to.
    pub(super) fn island(&self) -> Qualifier {
        self.prefix.clone()
    }

    pub(super) fn bindings(&self) -> &HashMap<String, Qualifier> {
        &self.bindings
    }

    /// Every public interface in scope that `carries` `label`, as the absolute path `label` names there — the candidates an unresolved name's report spells the way out with. Read off the public interfaces, where a `pub use` is a binding of the re-exporting module, rather than the declaration table, because the facade's spelling is the one a reader is told everything comes from: `/std/Bool` is `pub use Bool/{let Bool}`, and Core knows only the `/sys/Bool/Bool` it stands for. Only the shortest paths are kept — a longer one is the same name through a deeper module — in the path's own order, which is deterministic and nothing more.
    ///
    /// A candidate this module may not reference is dropped first, because a report that offered one would spell a way out the next compile refuses — and refuses with the redirect that belonged in *this* message. Dropping before the shortest paths are kept rather than after is what makes the rule reachability instead of depth: `/sys/Nat` stands beside `/std/Nat` at the same length, and a reachable route below an internal one is still worth offering.
    fn candidates(
        &self,
        label: &str,
        carries: impl Fn(&PublicInterface) -> bool,
    ) -> Vec<Qualifier> {
        let mut found = self
            .public
            .iter()
            .filter(|(_, interface)| carries(interface))
            .map(|(module, _)| module.with(label))
            .filter(|path| self.reach.guard(&self.prefix, path.segments()).is_ok())
            .collect::<Vec<_>>();
        found.sort_by_key(|path| (path.segments().len(), path.join()));
        let shortest = found.first().map(|path| path.segments().len());
        found.retain(|path| Some(path.segments().len()) == shortest);
        found
    }

    /// Mint the binder an unresolved bare name lowers to, and record beside it every binding in scope that could have been meant — see [`Context::candidates`].
    pub(super) fn unbound_binder(&self, label: &str) -> curios_core::Free {
        let found = self.candidates(label, |interface| interface.bindings.contains_key(label));

        let binder = self.fresh_binder(Some(label));
        self.unbound.borrow_mut().insert(binder.clone(), found);
        binder
    }

    /// Every public child module in scope that carries `label` — what an unresolved qualifier could have meant, gathered as [`Context::unbound_binder`] gathers a binding's candidates.
    fn unbound_qualifier(&self, label: &str) -> Vec<Qualifier> {
        self.candidates(label, |interface| interface.children.contains_key(label))
    }

    pub(super) fn insert_scope(&mut self, qualifier: String, name: Qualifier) -> Result<(), Error> {
        if self.qualifiers.contains_key(&qualifier) {
            return Err(Error::QualifierConflict { qualifier });
        }

        self.qualifiers.insert(qualifier, name);
        Ok(())
    }

    pub(super) fn insert_binding(&mut self, label: String, name: Qualifier) -> Result<(), Error> {
        if self.bindings.contains_key(&label) {
            return Err(Error::BindingConflict { label });
        }

        self.bindings.insert(label, name);
        Ok(())
    }

    // Walk from `start` through `segments` as child modules visible to this module, following each entry's re-export target. A failing segment is classified against the direct table: present-but-private vs. absent.
    fn walk_children(&self, start: Qualifier, segments: &[String]) -> Result<Qualifier, Error> {
        let mut current = start;

        for segment in segments {
            match super::interface::visible_child(
                self.public,
                self.table,
                &self.prefix,
                &current,
                segment,
            ) {
                Some(target) => current = target,
                None => return Err(self.child_error(&current, segment)),
            }
        }

        Ok(current)
    }

    fn child_error(&self, module: &Qualifier, segment: &str) -> Error {
        match self
            .table
            .get(module)
            .and_then(|info| info.get_child(segment))
        {
            Some(false) => Error::PrivateChildModule {
                segment: segment.to_string(),
            },
            _ => Error::ChildModuleNotFound {
                segment: segment.to_string(),
            },
        }
    }

    // Resolve the module named by `name`'s first `upto` segments: from the module root (absolute) or the lexically-bound head qualifier (relative — the head is consumed as the start, so the walk runs over `segments[1..upto]`).
    //
    // **What is guarded is the reach the author spelled, not where the name they wrote happens to live.** An absolute path names its root outright, and a relative one whose head is a module of this scope reaches the same place by another spelling — both are guarded at the head, so `sys/Nat/add` is refused exactly as `/sys/Nat/add` is. A head an *import* put in scope is not: that `use` was vetted where it was written, against the facade the standard library offers, and a type re-exported out of an internal root carries its constructors with it. Walking into them is reaching through the facade rather than past it, so `Scalar/below` under `use /std/Char/{Scalar}` is the consumer's to write even though the declaration sits in `/sys`.
    fn resolve_module_prefix(&self, name: &Name, upto: usize) -> Result<Qualifier, Error> {
        let segments = name.qualifier().segments();

        let resolved = if name.is_abs() {
            let resolved = self.walk_children(Qualifier::empty(), &segments[..upto])?;
            self.reach.guard(&self.prefix, resolved.segments())?;
            resolved
        } else {
            let head = name.head();
            let start = self
                .qualifiers
                .get(head)
                .ok_or_else(|| Error::UnresolvedQualifier {
                    qualifier: head.to_string(),
                    candidates: self.unbound_qualifier(head),
                })?
                .clone();
            self.note_qualifier_use(head);

            // An imported label carries the site its `use` came through; a declaration and an ambient sibling module carry none, and those are the heads a guard still answers for.
            if !self.qualifier_sites.contains_key(head) {
                self.reach.guard(&self.prefix, start.segments())?;
            }

            self.walk_children(start, &segments[1..upto])?
        };

        Ok(resolved)
    }

    // The module that should contain `name`'s final segment, plus that segment.
    fn resolve_parent_path(&self, name: &Name) -> Result<(Qualifier, String), Error> {
        let last = name.qualifier().segments().len() - 1;
        let parent = self.resolve_module_prefix(name, last)?;
        Ok((parent, name.last().to_string()))
    }

    // Import the module child `label` out of `parent`, registering it as a qualifier in the current lexical scope.
    fn import_module_label(&mut self, parent: &Qualifier, label: &str) -> Result<Qualifier, Error> {
        match super::interface::visible_child(self.public, self.table, &self.prefix, parent, label)
        {
            Some(target) => {
                self.insert_scope(label.to_string(), target.clone())?;
                if let Some(site) = self.current_site {
                    self.qualifier_sites.insert(label.to_string(), site);
                }
                self.note_spelled(parent);
                self.record_module_import(&target, label);
                Ok(target)
            }
            None => Err(
                match self.table.get(parent).and_then(|i| i.get_child(label)) {
                    Some(false) => Error::PrivateChildModule {
                        segment: label.to_string(),
                    },
                    _ => Error::NotAModule {
                        label: label.to_string(),
                        parent: parent.join(),
                    },
                },
            ),
        }
    }

    // Import the binding `label` out of `parent`, registering it in the current lexical scope.
    fn import_binding_label(
        &mut self,
        parent: &Qualifier,
        label: &str,
    ) -> Result<Qualifier, Error> {
        match super::interface::visible_binding(
            self.public,
            self.table,
            &self.prefix,
            parent,
            label,
        ) {
            Some(target) => {
                self.insert_binding(label.to_string(), target.clone())?;
                if let Some(site) = self.current_site {
                    self.binding_sites.insert(label.to_string(), site);
                }
                self.note_spelled(parent);
                self.record_import(&target, label.to_string());
                Ok(target)
            }
            None => Err(
                match self.table.get(parent).and_then(|i| i.get_binding(label)) {
                    Some(false) => Error::PrivateBinding {
                        binding: label.to_string(),
                    },
                    _ => Error::NotABinding {
                        label: label.to_string(),
                        parent: parent.join(),
                    },
                },
            ),
        }
    }

    // Import both the module and binding slots of `label` — used by glob and the `Both` group item. Either or both may be absent.
    fn import_dual_label(&mut self, parent: &Qualifier, label: &str) -> Result<UseResolved, Error> {
        let module =
            super::interface::visible_child(self.public, self.table, &self.prefix, parent, label);

        let binding =
            super::interface::visible_binding(self.public, self.table, &self.prefix, parent, label);

        let mut result = UseResolved {
            module: None,
            binding: None,
        };

        if let Some(target) = module {
            self.insert_scope(label.to_string(), target.clone())?;
            if let Some(site) = self.current_site {
                self.qualifier_sites.insert(label.to_string(), site);
            }
            self.note_spelled(parent);
            self.record_module_import(&target, label);
            result.module = Some(target);
        }

        if let Some(target) = binding {
            self.insert_binding(label.to_string(), target.clone())?;
            if let Some(site) = self.current_site {
                self.binding_sites.insert(label.to_string(), site);
            }
            self.note_spelled(parent);
            self.record_import(&target, label.to_string());
            result.binding = Some(target);
        }

        Ok(result)
    }

    // Record that `target` is in scope of this body under `spelling`, from here on. A target already in scope under a spelling no longer than this one is not recorded again — both resolve, and the shorter is the one a reader would write; a shorter spelling arriving later is a second entry, so an item between the two sees only the first.
    fn record_import(&mut self, target: &Qualifier, spelling: String) {
        let global = curios_core::Global::Authored(target.clone());
        let mut imports = self.imports.borrow_mut();
        let shadowed = self.in_scope.iter().any(|index| {
            let existing = &imports.entries[*index];
            existing.global == global && existing.spelling.len() <= spelling.len()
        });
        if shadowed {
            return;
        }
        imports
            .entries
            .push(curios_core::Import { global, spelling });
        self.in_scope.push(imports.entries.len() - 1);
    }

    // Record every public binding of the module imported under `label`, each spelled through it — `Eq/cong` for `use /std/{Eq}`. Direct bindings only: a deeper member is reached by a longer path the reader has not written, and its own module's import is the place it would be recorded.
    fn record_module_import(&mut self, module: &Qualifier, label: &str) {
        let Some(interface) = self.public.get(module) else {
            return;
        };
        let mut labels = interface.bindings.keys().cloned().collect::<Vec<_>>();
        labels.sort();
        for binding in labels {
            if let Some(target) = super::interface::visible_binding(
                self.public,
                self.table,
                &self.prefix,
                module,
                &binding,
            ) {
                self.record_import(&target, format!("{label}/{binding}"));
            }
        }
    }

    // Snapshot the imports in scope of this body as the view of the declaration `owner` — what a goal inside it may be offered, and what a page resolves the names in its signature through. `None` is the entrypoint tail, which closes the root body.
    //
    // Keyed by `Global` rather than by `Qualifier`, because a `satisfy` is anonymous: its identity is a `Global::Witness` and there is no name to hand over. Every other declaration wraps its own qualifier at the call site, which is the same key this used to build.
    pub(super) fn record_import_scope(&self, owner: Option<&curios_core::Global>) {
        let mut imports = self.imports.borrow_mut();
        match owner {
            Some(owner) => {
                imports.by_item.insert(owner.clone(), self.in_scope.clone());
            }
            None => imports.tail = self.in_scope.clone(),
        }
    }

    pub(super) fn resolve_module_use(&mut self, name: &Name) -> Result<Qualifier, Error> {
        let result = (|| {
            let (parent, label) = self.resolve_parent_path(name)?;
            self.import_module_label(&parent, &label)
        })();
        result.map_err(|e| attach(e, name))
    }

    pub(super) fn resolve_binding_use(&mut self, name: &Name) -> Result<Qualifier, Error> {
        let result = (|| {
            let (parent, label) = self.resolve_parent_path(name)?;
            self.import_binding_label(&parent, &label)
        })();
        result.map_err(|e| attach(e, name))
    }

    pub(super) fn resolve_both_use(&mut self, name: &Name) -> Result<UseResolved, Error> {
        let result = (|| {
            let (parent, label) = self.resolve_parent_path(name)?;

            let has_module = super::interface::visible_child(
                self.public,
                self.table,
                &self.prefix,
                &parent,
                &label,
            )
            .is_some();
            let has_binding = super::interface::visible_binding(
                self.public,
                self.table,
                &self.prefix,
                &parent,
                &label,
            )
            .is_some();

            if !has_module && !has_binding {
                let child = self.table.get(&parent).and_then(|i| i.get_child(&label));
                let binding = self.table.get(&parent).and_then(|i| i.get_binding(&label));

                return Err(match (child, binding) {
                    (Some(false), _) => Error::PrivateChildModule {
                        segment: label.clone(),
                    },
                    (_, Some(false)) => Error::PrivateBinding {
                        binding: label.clone(),
                    },
                    _ => Error::NoSuchUseTarget {
                        label: label.clone(),
                        parent: parent.join(),
                    },
                });
            }

            self.import_dual_label(&parent, &label)
        })();
        result.map_err(|e| attach(e, name))
    }

    // A glob `use a/b/*` names a module directly and imports every public child and binding it exposes (including its re-exports), each under its own label.
    pub(super) fn resolve_glob(
        &mut self,
        name: &Name,
    ) -> Result<Vec<(String, UseResolved)>, Error> {
        let result = (|| {
            let module = self.resolve_module_prefix(name, name.qualifier().segments().len())?;

            let interface = self
                .public
                .get(&module)
                .ok_or_else(|| Error::ModuleNotFound {
                    path: module.join(),
                })?;

            let mut labels = interface
                .children
                .keys()
                .chain(interface.bindings.keys())
                .cloned()
                .collect::<Vec<_>>();
            labels.sort();
            labels.dedup();

            labels
                .into_iter()
                .map(|label| {
                    let resolved = self.import_dual_label(&module, &label)?;
                    Ok((label, resolved))
                })
                .collect::<Result<Vec<_>, Error>>()
        })();
        result.map_err(|e| attach(e, name))
    }

    // Resolve a qualified/absolute term reference to its canonical binding target, reading the frozen public interfaces.
    pub(super) fn resolve_term_name(&self, name: &Name) -> Result<Qualifier, Error> {
        let result = (|| {
            let (parent, label) = self.resolve_parent_path(name)?;

            match super::interface::visible_binding(
                self.public,
                self.table,
                &self.prefix,
                &parent,
                &label,
            ) {
                Some(target) => {
                    self.note_spelled(&parent);
                    Ok(target)
                }
                None => Err(
                    match self.table.get(&parent).and_then(|i| i.get_binding(&label)) {
                        Some(false) => Error::PrivateBinding { binding: label },
                        _ => Error::BindingNotFound { binding: label },
                    },
                ),
            }
        })();
        result.map_err(|e| attach(e, name))
    }
}

// The refusal of a resolution, located at the half of the written name it is about: a `use m/{a}` member carries the group's span for its path and the selector's own for its last segment, and only the error says which is wrong. Every other name was written as one run of text, where the two are the same span.
fn attach(error: Error, name: &Name) -> Error {
    let span = match error.names_the_leaf() {
        true => name.leaf_span(),
        false => name.span(),
    };

    match span {
        Some(span) => error.at(span.clone()),
        None => error,
    }
}

// The refusal of a name a module already declares, located at the declaration that arrived second — which is why the inserters take the written [`Label`] rather than a copy of its text.
fn duplicate(label: &Label) -> Error {
    located(
        Error::DuplicateDeclaration {
            label: label.to_string(),
        },
        label,
    )
}

// [`duplicate`] for a label a structure already declares as a *field*. A field is not a module declaration — it occupies a slot in the structure's telescope and never reaches an inserter — so it needs its own refusal, and the message says so.
pub(super) fn duplicate_field(label: &Label) -> Error {
    located(
        Error::DuplicateField {
            label: label.to_string(),
        },
        label,
    )
}

// A refusal placed at the word it is about. A label the compiler spelled rather than parsed carries no span and reports unlocated, which is right for the one caller that has none: a mount claims a prefix on no source line.
fn located(error: Error, label: &Label) -> Error {
    match label.span() {
        Some(span) => error.at(span.clone()),
        None => error,
    }
}
