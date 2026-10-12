use {
    super::{Lookup, PublicInterface, Reach, Scoped, named, visible_binding, visible_child},
    crate::{Error, Label, Lint, Name, OrderedMap},
    curios_utilities::{Entropy, Mount, Qualifier, Span, SyntaxRegistry},
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

/// Where a unit's source declarations lie, as its lowering finds them, and the text of the one being lowered: what turns a written span into the extent a Core term holds (`curios_core::Extent`). The unit's, shared across nested contexts like its counters.
#[derive(Default)]
pub(super) struct Anchoring {
    anchors: RefCell<curios_core::Anchors>,
    /// `None` between declarations, and within one that was built rather than written: a term lowered then holds no position.
    within: RefCell<Option<curios_core::Within>>,
    /// What the declaration being lowered has asked of its scope and reported so far.
    journal: RefCell<Journal>,
}

impl Anchoring {
    /// Begin a declaration: its text and its journal start empty, and the anchors, which are the unit's, stay. Every field is named, as [`Declaring::begin`] names its own.
    fn begin(&self) {
        let Self {
            anchors: _,
            within,
            journal,
        } = self;

        *within.borrow_mut() = None;
        *journal.borrow_mut() = Journal::default();
    }
}

/// Where a written `Sort` stands, as far as whether a caller can state it goes.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(super) enum SortSite {
    /// The signature of a top-level `let`, `induct` or `struct`, which each use reads and states the sort of.
    Signature,
    /// A term no caller reads a sort off: a body, a test, a foreign declaration.
    #[default]
    Body,
    /// A definition local to another, whose every use is inside one declaration.
    Local,
    /// A `concept`, whose members a witness is found for by the key its signature spells.
    Concept,
    /// A `satisfy` declaration, found by its key and written by no caller.
    Witness,
}

impl SortSite {
    /// What a `Sort` refused here is told it stands in: `None` where it is taken.
    fn refused(self) -> Option<&'static str> {
        match self {
            SortSite::Signature => None,
            SortSite::Body => Some("a body"),
            SortSite::Local => Some("a local definition"),
            SortSite::Concept => Some("a concept"),
            SortSite::Witness => Some("a witness"),
        }
    }
}

/// The `Sort`s the declaration being lowered has written, and whether one may be written where its lowering stands.
#[derive(Default)]
pub(super) struct WrittenSorts {
    /// Where each was written, by the id of its quality, from zero for each declaration.
    seeds: RefCell<Vec<Option<curios_core::UniverseConstraintOrigin>>>,
    /// The quality of each written `Sort`, by its span: one written `Sort` is one quality however many times its text is lowered.
    allocations: RefCell<HashMap<Span, curios_core::QualityMetaId>>,
    site: Cell<SortSite>,
}

impl WrittenSorts {
    /// Begin a declaration with nothing written, every field named as [`Declaring::begin`] names its own.
    fn begin(&self) {
        let Self {
            seeds,
            allocations,
            site,
        } = self;

        seeds.borrow_mut().clear();
        allocations.borrow_mut().clear();
        site.set(SortSite::default());
    }
}

/// What a declaration's lowering asked of its scope and wrote beside its items, since [`Context::begin_declaration`]: what is recorded of it beside its items.
#[derive(Default)]
pub(super) struct Journal {
    /// Every name it resolved through its scope, with what each resolved to, in the order it asked.
    pub trace: curios_core::Trace<Lookup, Qualifier>,
    /// The names it recorded an import scope for.
    pub scoped: Vec<curios_core::Global>,
    /// The lints its lowerers reported.
    pub lints: Vec<Lint>,
    /// Whether a name it wrote resolved to nothing. Elaboration refuses such a declaration, and no later lowering keeps what it lowered to.
    pub unbound: bool,
}

impl Anchoring {
    /// Where each declaration this lowering reached lies.
    pub(super) fn into_anchors(self) -> curios_core::Anchors {
        self.anchors.into_inner()
    }

    /// The span `extent` lies on, among the declarations this lowering has reached.
    pub(super) fn resolved(&self, extent: &curios_core::Extent) -> Option<Span> {
        self.anchors.borrow().resolve(extent)
    }

    /// Record that `name` was written at `label`: where a report about it is placed when nothing in it is.
    pub(super) fn name(&self, name: curios_core::Global, label: curios_core::Extent) {
        self.anchors.borrow_mut().name(name, label);
    }
}

#[derive(Clone)]
pub(super) struct FlatLet {
    pub name: curios_core::Global,
    pub kind: curios_core::DefinitionKind,
    /// Where the definition's name was written: what the `unused-declaration` lint underlines, and where a report about the definition is placed when it holds no position of its own. A constructor's is its tag, a method wrapper's its field's label, a test's its name, and a witness's the concept application its `satisfy` states. `None` for one that was built rather than written, a `foreign` row the prelude synthesized among them, and for one a later lowering keeps, whose place was taken where it was lowered.
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
            // Lowering cannot know this. Elaboration computes the definition's totality over its final terms, as its item is published, and writes it here.
            totality: curios_core::Totality::default(),
            // Nor this: elaboration states it as the item is published, with its binders' marks.
            proof: false,
            variances: Vec::new(),
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
    /// A recursive group taken from a baseline as it was closed: no member's terms are opened again, and what the unit-wide passes read of it was taken where it was lowered.
    Kept(curios_core::RecItem),
}

impl FlatItem {
    pub(super) fn names(&self) -> Vec<curios_core::Global> {
        match self {
            FlatItem::Let(let_) => vec![let_.name],
            FlatItem::Rec(lets) => lets.iter().map(|let_| let_.name).collect(),
            FlatItem::Kept(rec) => rec
                .definitions
                .iter()
                .map(|definition| definition.name)
                .collect(),
        }
    }

    /// The item's members as the walk lowered them: one for a `let`, every one of a group, and none of a group kept closed.
    pub(super) fn lets(&self) -> &[FlatLet] {
        match self {
            FlatItem::Let(let_) => std::slice::from_ref(let_),
            FlatItem::Rec(lets) => lets.as_slice(),
            FlatItem::Kept(_) => &[],
        }
    }

    /// [`FlatItem::lets`], to rename a member.
    pub(super) fn lets_mut(&mut self) -> &mut [FlatLet] {
        match self {
            FlatItem::Let(let_) => std::slice::from_mut(let_),
            FlatItem::Rec(lets) => lets.as_mut_slice(),
            FlatItem::Kept(_) => &mut [],
        }
    }

    /// Each definition of the item: its name, its kind and the module declaring it.
    pub(super) fn declared(
        &self,
    ) -> impl Iterator<Item = (curios_core::Global, &curios_core::DefinitionKind, Qualifier)> {
        let kept = match self {
            FlatItem::Kept(rec) => rec.definitions.as_slice(),
            FlatItem::Let(_) | FlatItem::Rec(_) => &[],
        };

        self.lets()
            .iter()
            .map(|let_| (let_.name, &let_.kind, let_.island))
            .chain(
                kept.iter()
                    .map(|definition| (definition.name, &definition.kind, definition.island)),
            )
    }

    /// Every global this item names — the reachability prune's edges (see [`FlatItem::names`]). Locals are dropped: an item's binders are its own business, and reachability is a question about definitions.
    pub(super) fn free_vars(&self) -> HashSet<curios_core::Global> {
        self.lets()
            .iter()
            .flat_map(|let_| named(&let_.type_).chain(named(&let_.body)))
            .collect()
    }

    pub(super) fn into_core(self) -> curios_core::Item {
        match self {
            FlatItem::Let(let_) => curios_core::Item::Let(let_.into_core()),
            FlatItem::Kept(rec) => curios_core::Item::Rec(rec),
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
    #[archived_with(OrderedMap)]
    children: HashMap<String, ChildInfo>,
    #[archived_with(OrderedMap)]
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

/// What the declaration being lowered mints from and has written of universes and sorts: everything that begins afresh where a source declaration does ([`Context::begin_declaration`]). No term in scope carries a local, a metavariable or a universe metavariable — a stored unit is refused one — so nothing minted here can alias an identity already there, and what a declaration mints depends on nothing compiled or written before it.
pub(super) struct Declaring {
    // `Cell`-backed, as every counter here is, so it survives `Lowerer`'s immutable `&Context` borrow.
    metavars: Entropy,
    universes: Entropy,
    // Every binder identity a lowered term closes over. Threaded (not a process-global atomic) for determinism: two runs over the same source must mint the same identities, or terms that should be equal would differ.
    binders: Entropy,
    // The seed table of the declaration being lowered, from index zero, in step with `universes`.
    universe_seeds: RefCell<Vec<curios_core::UniverseSeed>>,
    universe_allocations: RefCell<HashMap<Span, curios_core::UniverseMetaId>>,
    // The `Sort`s the declaration being lowered wrote — see `WrittenSorts`.
    sorts: WrittenSorts,
}

impl Declaring {
    /// Begin afresh. Every field is named, so one added later does not compile until this says what beginning a declaration does to it.
    fn begin(&self) {
        let Self {
            metavars,
            universes,
            binders,
            universe_seeds,
            universe_allocations,
            sorts,
        } = self;

        metavars.reset();
        universes.reset();
        binders.reset();
        universe_seeds.borrow_mut().clear();
        universe_allocations.borrow_mut().clear();
        sorts.begin();
    }
}

/// What every nested context of one unit's lowering shares, behind one reference: what the declaration being lowered mints from, and the unit's own tables, which every declaration writes into and the lowering's caller reads back when it is over.
pub(super) struct Shared {
    declaring: Declaring,
    universe_role: Cell<curios_core::UniverseRole>,
    // What each source declaration lowered so far minted, with the names it lowered to — see `curios_core::Minted`.
    pub(super) minted: RefCell<curios_core::Minted>,
    // One ordinal counter per declaring module. A `satisfy` declaration is anonymous, so its identity is minted rather than written — and it is scoped to the module that declares it, because an ordinal alone would mean something only in the compilation that handed it out.
    witnesses: RefCell<BTreeMap<Qualifier, u32>>,
    // Every bare name that resolved to nothing, keyed by the binder identity it lowered to, with the public bindings in scope of that name: `curios-elab` reports the unbound binder, and this is what lets its report say what the reader probably meant.
    pub(super) unbound: RefCell<BTreeMap<curios_core::Free, Vec<Qualifier>>>,
    // Every binding a `use` brought into some lexical scope of this unit, with the path the reader wrote it under, and per definition the ones in scope where it was written — see `curios_core::Imports`. Core keeps the canonical name, and only this stage knows the spelling that resolves at the use site, which is what a goal report's candidate must be pasteable under.
    pub(super) imports: RefCell<curios_core::Imports>,
    // Every `use` selector and glob of the unit, in the order resolution met them.
    pub(super) sites: RefCell<Vec<UseSite>>,
    // The prefix of every mount some reference of this unit resolved into — what decides whether a declared dependency is used. Recorded where a target is resolved, never where it is spelled, so an absolute path and an import count alike.
    pub(super) reached: RefCell<BTreeSet<Qualifier>>,
    // Every lint a `Lowerer` of this unit decided — see `Lowerer::flush`.
    pub(super) lints: RefCell<Vec<Lint>>,
    // Where the unit's declarations lie, and the one being lowered — see `Anchoring`.
    pub(super) anchoring: Anchoring,
}

impl Shared {
    /// Nothing lowered yet: every counter at zero and every table empty.
    pub(super) fn new() -> Self {
        Self {
            declaring: Declaring {
                metavars: Entropy::new(),
                universes: Entropy::new(),
                binders: Entropy::new(),
                universe_seeds: RefCell::default(),
                universe_allocations: RefCell::default(),
                sorts: WrittenSorts::default(),
            },
            universe_role: Cell::new(curios_core::UniverseRole::Flexible),
            minted: RefCell::default(),
            witnesses: RefCell::default(),
            unbound: RefCell::default(),
            imports: RefCell::default(),
            sites: RefCell::default(),
            reached: RefCell::default(),
            lints: RefCell::default(),
            anchoring: Anchoring::default(),
        }
    }
}

// The per-body elaboration context. `table`/`public` are frozen interface views, shared read-only across all nested contexts. `qualifiers`/`bindings` are the lexical scope of the module body being elaborated, populated source-ordered by declarations and `use` imports.
pub(super) struct Context<'a> {
    prefix: Qualifier,
    // Every prefix this compilation mounts and which of them this unit may name — see `super::Reach`. Shared read-only by every nested context, like `table`/`public`: which mount owns a module is `Mount::owning` over its qualifier, so nesting carries nothing about roots and nothing re-derives one from a string.
    reach: Reach<'a>,
    table: &'a Scoped<'a, ModuleInfo>,
    public: &'a Scoped<'a, PublicInterface>,
    qualifiers: HashMap<String, Qualifier>,
    bindings: HashMap<String, Qualifier>,
    // What the unit's lowering writes and mints from, shared with every nested context — see `Shared`.
    shared: &'a Shared,
    // The scope is this body's alone: `use` binds from its position to the end of the body it is written in, and a nested body starts empty, so `in_scope` is per context and a snapshot of it is taken at each item.
    in_scope: Vec<usize>,
    // Per lexical scope, the site each imported label came through — a reference resolving through a label marks its site used. Declarations enter `bindings` and `qualifiers` without a site, so nothing marks for them.
    current_site: Option<usize>,
    binding_sites: HashMap<String, usize>,
    qualifier_sites: HashMap<String, usize>,
    // Where each qualifier and each binding of this scope was claimed, for the claims a place was written for: what a second claim of the label is shown.
    qualifier_claims: HashMap<String, Span>,
    binding_claims: HashMap<String, Span>,
    syntax: &'a SyntaxRegistry,
}

impl<'a> Context<'a> {
    pub(super) fn new(
        table: &'a Scoped<'a, ModuleInfo>,
        public: &'a Scoped<'a, PublicInterface>,
        reach: Reach<'a>,
        shared: &'a Shared,
        syntax: &'a SyntaxRegistry,
    ) -> Context<'a> {
        Context {
            prefix: Qualifier::empty(),
            reach,
            table,
            public,
            qualifiers: HashMap::new(),
            bindings: HashMap::new(),
            shared,
            in_scope: Vec::new(),
            current_site: None,
            binding_sites: HashMap::new(),
            qualifier_sites: HashMap::new(),
            qualifier_claims: HashMap::new(),
            binding_claims: HashMap::new(),
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
            shared: self.shared,
            in_scope: Vec::new(),
            current_site: None,
            binding_sites: HashMap::new(),
            qualifier_sites: HashMap::new(),
            qualifier_claims: HashMap::new(),
            binding_claims: HashMap::new(),
            syntax: self.syntax,
        }
    }

    pub(super) fn report(&self, lints: impl IntoIterator<Item = Lint>) {
        let lints = lints.into_iter().collect::<Vec<_>>();
        self.shared
            .anchoring
            .journal
            .borrow_mut()
            .lints
            .extend(lints.iter().cloned());
        self.shared.lints.borrow_mut().extend(lints);
    }

    /// Open the `use` site every label imported until [`Self::close_site`] is credited to.
    pub(super) fn open_site(&mut self, site: UseSite) {
        let mut sites = self.shared.sites.borrow_mut();
        sites.push(site);
        self.current_site = Some(sites.len() - 1);
    }

    pub(super) fn close_site(&mut self) {
        self.current_site = None;
    }

    /// A reference resolved through the binding `label` brought into this scope: its `use`, if one did, is used.
    pub(super) fn note_binding_use(&self, label: &str) {
        if let Some(&site) = self.binding_sites.get(label) {
            self.shared.sites.borrow_mut()[site].used = true;
        }
    }

    fn note_qualifier_use(&self, label: &str) {
        if let Some(&site) = self.qualifier_sites.get(label) {
            self.shared.sites.borrow_mut()[site].used = true;
        }
    }

    /// A reference was *written* under `spelled`: the mount owning that path is reached. The entry's empty prefix owns every name nothing else claims, and is recorded like any other; the consumer knows which prefixes it asked about.
    ///
    /// **The path the reader wrote, never the one it resolved to.** A re-export means the two differ: `/std/Nat` is `pub use /sys/{Nat}`, so resolving `use /std/{Nat}` lands on `/sys/Nat` and a record of the *target* would say every program reached `/sys`. What a declared dependency is answerable to is what the reader spelled, which is also the only thing they can change.
    fn note_spelled(&self, spelled: &Qualifier) {
        if let Some(mount) = Mount::owning(self.reach.mounts(), spelled) {
            self.shared.reached.borrow_mut().insert(mount.prefix);
        }
    }

    /// Begin lowering a source declaration: every identity it mints counts from zero, and a written type is one level across the items the declaration lowers to and no further. What a declaration's lowered form holds then depends on nothing written before it.
    pub(super) fn begin_declaration(&self) {
        self.shared.declaring.begin();
        self.shared.anchoring.begin();
    }

    /// What the declaration begun last has asked and reported, taken: its lowering is over.
    pub(super) fn journal(&self) -> Journal {
        std::mem::take(&mut self.shared.anchoring.journal.borrow_mut())
    }

    /// The text the declaration being lowered was sited at, read by the anchor it is a text of with its stamp, and by the text itself. `None` between declarations and within one that was built rather than written.
    pub(super) fn sited<T>(
        &self,
        read: impl FnOnce((curios_core::Anchor, u32), &str) -> T,
    ) -> Option<T> {
        let within = self.shared.anchoring.within.borrow();
        let within = within.as_ref()?;

        Some(read(within.key(), within.text()))
    }

    /// The span `extent` lies on, among the declarations this lowering has reached.
    pub(super) fn resolved(&self, extent: &curios_core::Extent) -> Option<Span> {
        self.shared.anchoring.resolved(extent)
    }

    /// Lower what follows as the text of the source declaration `anchored` names, which `span` covers: every span written within it becomes an extent of it, so nothing the declaration lowers to says where in its file it stands.
    pub(super) fn anchor(&self, anchored: curios_core::Anchored, span: &Span) {
        let within = self
            .shared
            .anchoring
            .anchors
            .borrow_mut()
            .site(curios_core::Anchor::of(anchored), span);
        *self.shared.anchoring.within.borrow_mut() = Some(within);
    }

    /// The extent of `span` in the declaration being lowered: `None` outside one, and for a span that does not lie in its text.
    pub(super) fn extent(&self, span: &Span) -> Option<curios_core::Extent> {
        self.shared.anchoring.within.borrow().as_ref()?.extent(span)
    }

    /// `term` at `span` — innermost wins if it already carries a position, per `curios_core::Term::with_extent`.
    pub(super) fn placed(&self, span: &Span, term: curios_core::Term) -> curios_core::Term {
        match self.extent(span) {
            Some(extent) => term.with_extent(extent),
            None => term,
        }
    }

    /// What has been minted since [`Context::begin_declaration`].
    pub(super) fn mints(&self) -> curios_core::Mints {
        curios_core::Mints {
            binders: self.shared.declaring.binders.count(),
            metavariables: self.shared.declaring.metavars.count(),
            universes: self.shared.declaring.universe_seeds.borrow().clone(),
            qualities: self.shared.declaring.sorts.seeds.borrow().clone(),
        }
    }

    /// End the declaration begun last, which lowered to the items declaring `names`: record what it minted, for every one of them.
    pub(super) fn end_declaration(&self, names: impl IntoIterator<Item = curios_core::Global>) {
        self.keep_declaration(self.mints(), names);
    }

    /// Record that the items declaring `names` are one declaration's, which minted `mints`: [`Context::end_declaration`], for a declaration whose lowering is taken from a baseline with what it minted there.
    pub(super) fn keep_declaration(
        &self,
        mints: curios_core::Mints,
        names: impl IntoIterator<Item = curios_core::Global>,
    ) {
        let mut minted = self.shared.minted.borrow_mut();
        let declaration = minted.declarations.len();
        minted.declarations.push(mints);
        minted
            .from
            .extend(names.into_iter().map(|name| (name, declaration)));
    }

    /// Mint a metavariable id for a surface hole, unique within its declaration.
    pub(super) fn fresh_metavar(&self) -> usize {
        self.shared.declaring.metavars.fresh()
    }

    pub(super) fn fresh_universe(
        &self,
        span: Option<&Span>,
        written: curios_core::UniverseConstraintKind,
    ) -> curios_core::Level {
        if let Some(span) = span
            && let Some(id) = self
                .shared
                .declaring
                .universe_allocations
                .borrow()
                .get(span)
        {
            assert_eq!(
                self.shared.declaring.universe_seeds.borrow()[id.0].role,
                self.universe_role(),
                "one written Type was lowered under conflicting universe roles"
            );
            return curios_core::Level::meta(*id);
        }

        let id = self.shared.declaring.universes.fresh();
        let mut seeds = self.shared.declaring.universe_seeds.borrow_mut();
        assert_eq!(id, seeds.len(), "universe seeds parallel their dense ids");
        seeds.push(curios_core::UniverseSeed {
            role: self.universe_role(),
            origin: span.map(|span| curios_core::UniverseConstraintOrigin {
                extent: self.extent(span),
                kind: written,
                declaration: (!self.prefix.is_root()).then(|| self.prefix.join()),
                binder: None,
            }),
        });
        let id = curios_core::UniverseMetaId(id);
        if let Some(span) = span {
            self.shared
                .declaring
                .universe_allocations
                .borrow_mut()
                .insert(span.clone(), id);
        }
        curios_core::Level::meta(id)
    }

    /// The quality of the `Sort` written at `span`: one for each written `Sort`, shared by every lowering of its text, and refused where no caller states it ([`SortSite`]). A `Sort` whose text was lowered in a signature is taken wherever that text is lowered again, as a parameter's annotation is under the lambda its definition is.
    pub(super) fn written_quality(
        &self,
        span: Option<&Span>,
    ) -> Result<curios_core::Quality, Error> {
        if let Some(span) = span
            && let Some(id) = self.shared.declaring.sorts.allocations.borrow().get(span)
        {
            return Ok(curios_core::Quality::Meta(*id));
        }
        // A `Sort` with no span is one the compiler built, in `/sys`: it stands in a signature by construction, and is lowered again, at no span to find it by, under the lambda its declaration is.
        if span.is_some()
            && let Some(site) = self.shared.declaring.sorts.site.get().refused()
        {
            return Err(Error::SortOutsideSignature { site });
        }

        let mut seeds = self.shared.declaring.sorts.seeds.borrow_mut();
        let id = curios_core::QualityMetaId(seeds.len());
        seeds.push(span.map(|span| curios_core::UniverseConstraintOrigin {
            extent: self.extent(span),
            kind: curios_core::UniverseConstraintKind::WrittenSort,
            declaration: (!self.prefix.is_root()).then(|| self.prefix.join()),
            binder: None,
        }));
        if let Some(span) = span {
            self.shared
                .declaring
                .sorts
                .allocations
                .borrow_mut()
                .insert(span.clone(), id);
        }

        Ok(curios_core::Quality::Meta(id))
    }

    /// Lower what `f` lowers with a written `Sort` standing in `site`.
    pub(super) fn with_sorts<T>(
        &self,
        site: SortSite,
        f: impl FnOnce() -> Result<T, Error>,
    ) -> Result<T, Error> {
        let outer = self.shared.declaring.sorts.site.replace(site);
        let result = f();
        self.shared.declaring.sorts.site.set(outer);
        result
    }

    /// From here to the end of the declaration being lowered, a written `Sort` stands in `site`: for a declaration that is one kind of site throughout.
    pub(super) fn sorts_stand_in(&self, site: SortSite) {
        self.shared.declaring.sorts.site.set(site);
    }

    pub(super) fn universe_role(&self) -> curios_core::UniverseRole {
        self.shared.universe_role.get()
    }

    pub(super) fn with_universe_role<T>(
        &self,
        role: curios_core::UniverseRole,
        f: impl FnOnce() -> Result<T, Error>,
    ) -> Result<T, Error> {
        let old = self.shared.universe_role.replace(role);
        let result = f();
        self.shared.universe_role.set(old);
        result
    }

    /// The identity a witness is lowered under: its place among the witnesses of the module that declares it.
    ///
    /// A `satisfy` declaration is anonymous by design, so it gets an identity rather than a manufactured name — see [`curios_core::Global::Witness`]. This one tells it from its module's others while the unit is lowered and no longer: the name it keeps is what its signature spells (`name_witnesses`), and a witness whose signature spells nothing keeps this one only as far as the refusal that names it.
    ///
    /// The module is this context's own prefix rather than the mount it lies within, which would be disjoint enough to be sound and too coarse to be *placed*: an import scope is keyed by `Global`, and a page asks which declarations belong to the module it renders, to which every witness in the standard library would answer `/std`.
    pub(super) fn fresh_witness(&self) -> curios_core::WitnessId {
        let module = self.prefix;

        let mut witnesses = self.shared.witnesses.borrow_mut();
        let place = witnesses.entry(module).or_default();
        let minted = curios_core::WitnessId::Unspelled {
            module,
            place: *place,
        };
        *place += 1;

        minted
    }

    /// Mint a binder identity, rendering as `hint`.
    ///
    /// Every binder a lowered term closes over comes from here, including the continuation binders `!` desugaring introduces. `curios-elab` mints more while elaborating and starts its counter above this declaration's count ([`curios_core::Mints::binders`]), so the two sources share one identity space per declaration without colliding.
    pub(super) fn fresh_binder(&self, hint: Option<&str>) -> curios_core::Free {
        curios_core::Free::local(
            u32::try_from(self.shared.declaring.binders.fresh()).expect("binder space exhausted"),
            hint,
        )
    }

    /// [`Context::fresh_binder`] for a binder written in source, at `written` among its declaration's written binders — see [`curios_core::Free::local_written`].
    pub(super) fn fresh_written_binder(
        &self,
        hint: Option<&str>,
        written: u32,
    ) -> curios_core::Free {
        curios_core::Free::local_written(
            u32::try_from(self.shared.declaring.binders.fresh()).expect("binder space exhausted"),
            hint,
            written,
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
        self.prefix
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

    /// Mint the binder an unresolved bare name lowers to, and record beside it every binding in scope that could have been meant.
    pub(super) fn unbound_binder(&self, label: &str) -> curios_core::Free {
        let found = self.binding_candidates(label);

        let binder = self.fresh_binder(Some(label));
        self.shared.unbound.borrow_mut().insert(binder, found);
        self.shared.anchoring.journal.borrow_mut().unbound = true;
        binder
    }

    /// Every public binding in scope that carries `label` — what an unresolved name could have meant; see [`Context::candidates`].
    pub(super) fn binding_candidates(&self, label: &str) -> Vec<Qualifier> {
        self.candidates(label, |interface| interface.bindings.contains_key(label))
    }

    /// Every public child module in scope that carries `label` — what an unresolved qualifier could have meant, gathered as [`Context::unbound_binder`] gathers a binding's candidates.
    fn unbound_qualifier(&self, label: &str) -> Vec<Qualifier> {
        self.candidates(label, |interface| interface.children.contains_key(label))
    }

    /// Bring the module `name` into this scope as `qualifier`, claimed at `at` where the claim was written. A label claimed twice is refused with where it was claimed first.
    pub(super) fn insert_scope(
        &mut self,
        qualifier: String,
        at: Option<&Span>,
        name: Qualifier,
    ) -> Result<(), Error> {
        if self.qualifiers.contains_key(&qualifier) {
            let first = self.qualifier_claims.get(&qualifier).cloned();

            return Err(Error::QualifierConflict { qualifier, first });
        }

        if let Some(at) = at {
            self.qualifier_claims.insert(qualifier.clone(), at.clone());
        }
        self.qualifiers.insert(qualifier, name);
        Ok(())
    }

    /// Bring the binding `name` into this scope as `label`, as [`Context::insert_scope`] brings a module.
    pub(super) fn insert_binding(
        &mut self,
        label: String,
        at: Option<&Span>,
        name: Qualifier,
    ) -> Result<(), Error> {
        if self.bindings.contains_key(&label) {
            let first = self.binding_claims.get(&label).cloned();

            return Err(Error::BindingConflict { label, first });
        }

        if let Some(at) = at {
            self.binding_claims.insert(label.clone(), at.clone());
        }
        self.bindings.insert(label, name);
        Ok(())
    }

    // Where the `use` being resolved wrote the selector or the glob it is resolving.
    fn site_span(&self) -> Option<Span> {
        self.shared.sites.borrow()[self.current_site?].span.clone()
    }

    // Walk from `start` through `segments` as child modules visible to this module, following each entry's re-export target. A failing segment is classified against the direct table: present-but-private vs. absent.
    fn walk_children(&self, start: Qualifier, segments: &[String]) -> Result<Qualifier, Error> {
        let mut current = start;

        for segment in segments {
            match visible_child(self.public, self.table, &self.prefix, &current, segment) {
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
    // **What is guarded is the reach the author spelled, not where the name they wrote happens to live.** An absolute path names its root outright, and a relative one whose head is a module of this scope reaches the same place by another spelling — both are guarded at the head, so `sys/Nat/add` is refused exactly as `/sys/Nat/add` is. A head an *import* put in scope is not: that `use` was vetted where it was written, against the facade the standard library offers, and a type re-exported out of an internal root carries its constructors with it. Walking into them is reaching through the facade rather than past it, so `True/qed` under `use /std/{True}` is the consumer's to write even though the declaration sits in `/sys`.
    fn resolve_module_prefix(&self, name: &Name, upto: usize) -> Result<Qualifier, Error> {
        let segments = name.qualifier().segments();

        let resolved = if name.is_abs() {
            let resolved = self.walk_children(Qualifier::empty(), &segments[..upto])?;
            self.reach.guard(&self.prefix, resolved.segments())?;
            resolved
        } else {
            let head = name.head();
            let start = *self
                .qualifiers
                .get(head)
                .ok_or_else(|| Error::UnresolvedQualifier {
                    qualifier: head.to_string(),
                    candidates: self.unbound_qualifier(head),
                })?;
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
        match visible_child(self.public, self.table, &self.prefix, parent, label) {
            Some(target) => {
                self.insert_scope(label.to_string(), self.site_span().as_ref(), target)?;
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
        match visible_binding(self.public, self.table, &self.prefix, parent, label) {
            Some(target) => {
                self.insert_binding(label.to_string(), self.site_span().as_ref(), target)?;
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
        let module = visible_child(self.public, self.table, &self.prefix, parent, label);

        let binding = visible_binding(self.public, self.table, &self.prefix, parent, label);

        let mut result = UseResolved {
            module: None,
            binding: None,
        };

        if let Some(target) = module {
            self.insert_scope(label.to_string(), self.site_span().as_ref(), target)?;
            if let Some(site) = self.current_site {
                self.qualifier_sites.insert(label.to_string(), site);
            }
            self.note_spelled(parent);
            self.record_module_import(&target, label);
            result.module = Some(target);
        }

        if let Some(target) = binding {
            self.insert_binding(label.to_string(), self.site_span().as_ref(), target)?;
            if let Some(site) = self.current_site {
                self.binding_sites.insert(label.to_string(), site);
            }
            self.note_spelled(parent);
            self.record_import(&target, label.to_string());
            result.binding = Some(target);
        }

        Ok(result)
    }

    // Record that the binding `target` is in scope of this body under `spelling`, from here on, as it was written: which of two spellings of one binding a reader is shown is decided where one is spelled (`curios_core::Spellings::spell`).
    fn record_import(&mut self, target: &Qualifier, spelling: String) {
        self.record(curios_core::Import::Binding {
            global: curios_core::Global::Authored(*target),
            spelling,
        });
    }

    // Record that the module imported under `label` is in scope of this body, from here on: its public bindings are, each spelled through the label — `Eq/cong` for `use /std/{Eq}` — and which they are is the module's interface to say, read where the unit is assembled (`module_members`), never here. Direct bindings only: a deeper member is reached by a longer path the reader has not written, and its own module's import is the place it would be recorded.
    fn record_module_import(&mut self, module: &Qualifier, label: &str) {
        self.record(curios_core::Import::Module {
            module: *module,
            label: label.to_string(),
        });
    }

    fn record(&mut self, import: curios_core::Import) {
        let mut imports = self.shared.imports.borrow_mut();
        imports.entries.push(import);
        self.in_scope.push(imports.entries.len() - 1);
    }

    // Snapshot the imports in scope of this body as the view of the declaration `owner` — what a goal inside it may be offered, and what a page resolves the names in its signature through. `None` is the entrypoint tail, which closes the root body.
    //
    // Keyed by `Global` rather than by `Qualifier`, because a `satisfy` is anonymous: its identity is a `Global::Witness` and there is no name to hand over. Every other declaration wraps its own qualifier at the call site.
    pub(super) fn record_import_scope(&self, owner: Option<&curios_core::Global>) {
        let mut imports = self.shared.imports.borrow_mut();
        match owner {
            Some(owner) => {
                imports.by_item.insert(*owner, self.in_scope.clone());
                self.shared
                    .anchoring
                    .journal
                    .borrow_mut()
                    .scoped
                    .push(*owner);
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

            let has_module =
                visible_child(self.public, self.table, &self.prefix, &parent, &label).is_some();
            let has_binding =
                visible_binding(self.public, self.table, &self.prefix, &parent, &label).is_some();

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

            match visible_binding(self.public, self.table, &self.prefix, &parent, &label) {
                Some(target) => {
                    self.note_spelled(&parent);
                    self.asked(name, target);
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

    /// What the bare name `name` is bound to in this module's scope, where it is: a declaration of the module or a label a `use` brought in, whose `use` is then used. A binder of the declaration itself is the lowerer's to find first.
    pub(super) fn resolve_bare(&self, name: &Name) -> Option<Qualifier> {
        let target = *self.bindings.get(name.head())?;
        self.note_binding_use(name.head());
        self.asked(name, target);

        Some(target)
    }

    /// Record that the declaration being lowered resolved `name`, as written, to `found`.
    fn asked(&self, name: &Name, found: Qualifier) {
        let lookup = Lookup {
            absolute: name.is_abs(),
            path: *name.qualifier(),
        };
        self.shared
            .anchoring
            .journal
            .borrow_mut()
            .trace
            .record(lookup, found);
    }

    /// Whether `lookup` resolves to `found` where this context stands, asked through the door the lowerer asked it through: a bare name of this module's scope, any other of the modules it may see. A resolution the scope now refuses answers no.
    pub(super) fn answers(&self, lookup: &Lookup, found: &Qualifier) -> bool {
        let name = Name::new(lookup.absolute, lookup.path);
        let answer = match name.is_abs() || !name.is_single() {
            true => self.resolve_term_name(&name).ok(),
            false => self.resolve_bare(&name),
        };

        answer.as_ref() == Some(found)
    }

    /// Drop what has been asked since the declaration began: the questions of a trace asked again are no questions of the declaration.
    pub(super) fn forget_asked(&self) {
        self.shared.anchoring.journal.borrow_mut().trace = curios_core::Trace::default();
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
