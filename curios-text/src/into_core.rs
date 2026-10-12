//! Lowering one unit's surface modules to a [`curios_core::Module`], over the units already lowered.
//!
//! [`into_core_unit`] runs a unit through these passes, in order:
//!
//! 1. **Discovery** (`Resolved::of`): the prefixes the unit claims checked disjoint from the predecessors', then every module they declare read through its source, for what its items declare (`head`). A file a baseline read as the same text is not parsed: what it declares is what the baseline kept of it.
//! 2. **Interface resolution** (`interface::resolve_unit`): each module's declarations seeded into its interface, then every `pub use` resolved to a fixed point, before any body is lowered.
//! 3. **Lowering** (`process_items`): each item's names resolved against those interfaces, its sugar undone and its matches compiled (`match_compile`), then the entry's final term. A declaration a baseline lowered from the same text, every name it resolved resolving as it did, is taken from the baseline and not lowered (`lowered`), and a file is parsed where the first of its declarations is.
//! 4. **Witness names** (`witness`): each witness given the name its signature spells, which is read through the unit's definitions and so only once every item is lowered.
//! 5. **Audit** (`audit`, `lint`): what the public interface exposes, and the declarations nothing reaches.
//!
//! A unit's documentation record is no pass of these: [`document_unit`] builds it where a command asks for one, from the tables this lowering stored and the surface tree its sources parse to.

mod audit;
use audit::*;

mod lowered;
use lowered::*;

mod context;
use context::*;

mod lowerer;
use lowerer::*;

mod match_compile;
use match_compile::*;

mod interface;
use interface::*;

mod lint;
use lint::*;

mod references;
use references::*;

mod scoped;
use scoped::*;

mod document;
use document::*;

mod head;
use head::*;

mod witness;
use witness::*;

use {
    crate::{
        Apply, Argument, Entrypoint, Error, FuncTypeParam, GroupItem, Label, LetSignature, Lint,
        LintedBinder, Module, Name, RootSource, StructLit, StructLitEntry, Subterm, Term, TopItem,
        TupleField, UseGroup, WithoutItems, func_sugar_lambda, func_sugar_type_params, ordered,
        tree_of,
    },
    curios_abi::{ForeignFunction, ForeignStore},
    curios_archive::{Bytes, joined, parted},
    curios_document::Documentation,
    curios_utilities::{
        Mount, Plicity, Qualifier, Report, RootKind, Source, Span, SyntaxRegistry, grown,
    },
    std::{
        borrow::Cow,
        cell::OnceCell,
        collections::{BTreeMap, BTreeSet, HashMap},
        rc::Rc,
        sync::Arc,
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
mod scan_tests;
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
/// **Two lists rather than one, because a prefix a unit cannot name still exists.** It is in the fold — elaboration and erasure are seeded from it — and it owns names whose spelling decides which unit declares them. So `mounts` stays complete for every question about *what a name is*, and `visible` answers the one question about *who may write it*. Collapsing them would either hide a mount from `Mount::owning`, which decides coherence, or make an undeclared prefix indistinguishable from a prefix nobody mounted, which is the diagnostic this type exists to keep.
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

    /// Why this unit may not write a reference that *resolves into* `resolved`, where it may not: its root is a prefix this unit cannot name.
    ///
    /// `resolved` is the segments of the qualifier the reference resolved to — not the raw spelled path — so absolute and relative spellings are guarded identically. A target inside a visible prefix, or inside the reader's own, passes through.
    ///
    /// **Visibility is the whole rule, and the root's kind only chooses the wording.** `/sys` is closed because no unit but the standard library declares a dependency on it, and the standard library does — the rule a package's manifest already states, with no privilege comparison between roots beside it. Whether the prefix is a closed root decides which reason a reader is given — that the root is internal rather than "declare it" — because telling a program to declare `/sys` would be advice it cannot take.
    ///
    /// **The prefix stays discoverable and the reference is refused**, rather than the prefix being hidden and the name reported unbound. A hidden prefix makes `use /sys/{Nat}` read as a typo; a refused one says which unit holds the name and why this one may not write it, which is the difference between a diagnostic and a riddle. The refusal is the one a name that does not resolve gets ([`out_of_reach`]), for this reason, so it names what the unit may write instead as any such refusal does.
    fn refused(&self, resolved: &[String]) -> Option<curios_core::Missing> {
        let root = resolved.first()?;

        let prefix = Qualifier::from([root.as_str()]);
        // Not a mount at all: an ordinary name under the compilation root, or one that will fail to resolve on its own terms.
        if !self.mounts.iter().any(|mount| mount.prefix == prefix) {
            return None;
        }

        // A prefix the unit declared, or its own — `visible` carries both, because a unit does not declare a dependency on itself.
        if self.visible.iter().any(|mount| mount.prefix == prefix) {
            return None;
        }

        Some(match is_internal_root(self.mounts, root) {
            true => curios_core::Missing::Internal,
            false => curios_core::Missing::Undeclared,
        })
    }
}

/// Every absolute path a unit gives a global — each declaration of its own at its own path, each export at the path its module gives it, whichever unit declares what it exports — with who may write it. A function of the unit's own tables and the audiences it decided, so it is made once for the unit ([`PreparedText::paths`]): which paths a reader's unit may name, and which it reaches relatively, are the reader's, read where a report spells a name ([`curios_core::Spellings::spell`]).
fn writable_paths(
    table: &BTreeMap<Qualifier, ModuleInfo>,
    public: &BTreeMap<Qualifier, PublicInterface>,
    audiences: &Audiences,
) -> BTreeMap<curios_core::Global, Vec<curios_core::WritablePath>> {
    curios_profile::profile!("writable_paths");
    let mut paths: BTreeMap<curios_core::Global, Vec<curios_core::WritablePath>> = BTreeMap::new();
    let mut record = |target: &Qualifier, path: Qualifier, audience: Vec<Qualifier>| {
        paths
            .entry(curios_core::Global::Authored(*target))
            .or_default()
            .push(curios_core::WritablePath { path, audience });
    };

    for (module, info) in table {
        for (label, _) in info.bindings() {
            let path = module.with(label);
            record(&path, path, audiences.binding(&path));
        }
    }
    for (module, interface) in public {
        #[expect(
            clippy::iter_over_hash_type,
            reason = "each global's paths are sorted below"
        )]
        for (label, entry) in &interface.bindings {
            let path = module.with(label);
            if path != entry.target {
                record(&entry.target, path, audiences.module(module));
            }
        }
    }

    // A declaration's own path, then each path it is exported at, by path: a module's bindings are walked in a hash's order, which differs from one compilation to the next, and a reader takes the first of two spellings equally short.
    for (global, written) in &mut paths {
        written.sort_by(|left, right| {
            let exported = |path: &Qualifier| curios_core::Global::Authored(*path) != *global;

            (exported(&left.path), &left.path).cmp(&(exported(&right.path), &right.path))
        });
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

/// One file-backed module as a lowering holds it: what its items declare, and their trees where anything has needed them.
pub(super) struct Loaded {
    module: Qualifier,
    /// What the module's items declare: read off its tree, or a baseline's scan of the same text.
    pub heads: Rc<Vec<Headed>>,
    /// The text the module was read from. `None` for a module supplied already parsed.
    source: Option<Arc<Source>>,
    /// The module's tree: in hand where the file was parsed for its heads, and otherwise parsed when a declaration of it is first lowered.
    tree: OnceCell<Module>,
}

impl Loaded {
    /// A module whose tree is in hand.
    fn parsed(module: Qualifier, tree: Module, source: Option<Arc<Source>>) -> Self {
        Self {
            module,
            heads: Rc::new(Headed::of(&tree)),
            source,
            tree: OnceCell::from(tree),
        }
    }

    /// The module `module` of `source`, read without being parsed where `scans` hold what the very text declared.
    fn of(
        source: &UnitSource<'_>,
        scans: Option<&BTreeMap<Qualifier, Scan>>,
        module: &Qualifier,
    ) -> Result<Self, Error> {
        let Some(text) = source.source.read(module)? else {
            return Ok(Self::parsed(*module, source.source.load(module)?, None));
        };

        if let Some(scan) = scans.and_then(|scans| scans.get(module))
            && scan.source.text == text.text
        {
            // A scan this thread made lies in the very source the file was read as, and is shared. One restored from a store lies in a source of its own, so its heads are placed in the one just read.
            let heads = match Arc::ptr_eq(&scan.source, &text) {
                true => Rc::clone(&scan.heads),
                false => Rc::new(
                    scan.heads
                        .iter()
                        .map(|headed| headed.rebased(&text))
                        .collect(),
                ),
            };

            return Ok(Self {
                module: *module,
                heads,
                source: Some(text),
                tree: OnceCell::new(),
            });
        }

        let tree = tree_of(&text).map_err(|cause| Error::ModuleLoadFailed {
            label: module.join(),
            cause: Box::new(cause),
        })?;

        Ok(Self::parsed(*module, tree, Some(text)))
    }

    /// The module's tree, parsed now where its heads were a scan's.
    fn tree(&self) -> Result<&Module, Error> {
        if let Some(tree) = self.tree.get() {
            return Ok(tree);
        }

        let source = self
            .source
            .as_ref()
            .expect("a module with no tree in hand was read from a file");
        let tree = tree_of(source).map_err(|cause| Error::ModuleLoadFailed {
            label: self.module.join(),
            cause: Box::new(cause),
        })?;

        Ok(self.tree.get_or_init(|| tree))
    }

    /// The module's tree, handed over.
    fn into_tree(self) -> Result<Module, Error> {
        self.tree()?;

        Ok(self
            .tree
            .into_inner()
            .expect("a tree just asked for is in hand"))
    }

    /// What the unit keeps of the module's file for a later lowering: its heads, in the text they were read from. `None` for a module supplied already parsed.
    fn scan(&self) -> Option<Scan> {
        Some(Scan {
            source: Arc::clone(self.source.as_ref()?),
            heads: Rc::clone(&self.heads),
        })
    }
}

/// Where the trees of a run of heads are: a file's module, the entry's items, or an inline module's, at its place among its parent's. Asked only where a declaration is lowered, which is what parses a file a scan stood in for.
#[derive(Clone, Copy)]
enum Body<'a> {
    File(&'a Loaded),
    Items(&'a [TopItem]),
    Inline(&'a Body<'a>, usize),
}

impl<'a> Body<'a> {
    fn items(&self) -> Result<&'a [TopItem], Error> {
        match self {
            Body::File(loaded) => Ok(&loaded.tree()?.items),
            Body::Items(items) => Ok(items),
            Body::Inline(parent, index) => match &parent.items()?[*index] {
                TopItem::Mod(crate::TopMod {
                    module: Some(module),
                    ..
                }) => Ok(&module.items),
                _ => unreachable!("an inline module's head stands where its tree does"),
            },
        }
    }
}

struct Resolved<'a> {
    modules: HashMap<Qualifier, Rc<Loaded>>,
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
    /// Who can see each declaration of this unit: decided where it was lowered, or the first time something asks of a restored one, and kept. Never stored, being read off `table` and `public`.
    #[archived_with(curios_archive::Skip)]
    audiences: OnceCell<Audiences>,
    /// Every absolute path this unit gives a global, made the first time a report spells a name over it and kept — see [`PreparedText::paths`]. Never stored, being read off `table`, `public` and `audiences`.
    #[archived_with(curios_archive::Skip)]
    paths: OnceCell<Rc<BTreeMap<curios_core::Global, Vec<curios_core::WritablePath>>>>,
    /// The lowered module: whole where the unit was lowered in this process, which is what its elaboration reads, and holding no item where it was restored.
    ///
    /// **Stored without its items.** What every unit lowered after this one reads of it is the registry entries its lowering wrote, so that is what a unit is restored with; an item is read by a later lowering of the same sources, and by a unit whose witness names its declaration ([`PreparedText::written`]), so each is stored on its own, beside the unit, and read by whoever asks for it ([`PreparedText::lowered_item`]).
    #[archived_with(WithoutItems)]
    core: curios_core::Module,
    /// Where the unit's lowered items hold each name it declares, with the kind it declares it as: what finds a declaration's lowered form by its name, and says what it is with nothing read.
    declared: BTreeMap<curios_core::Global, LoweredAt>,
    /// The unit's lowered items where it was restored, each the bytes it was stored as until something reads it: `core` holds them where the unit was lowered in this process.
    #[archived_with(curios_archive::Skip)]
    items: Option<Vec<curios_archive::Deferred<curios_core::Item>>>,
    /// The bytes a baseline stored an item as, for each item this unit took from one whole ([`PreparedText::taken_from`]), by the item's place: what the item is stored as again, since it is the baseline's. Empty for a unit lowered over none.
    #[archived_with(curios_archive::Skip)]
    carried: Vec<Option<curios_archive::Bytes>>,
    /// What this unit's lowering minted, which its elaboration's counters start above, with the names of each declaration that resolved to nothing. A count within the unit: nothing here resumes above a predecessor.
    minted: curios_core::Minted,
    /// Every binding a `use` brought into scope, with the spelling a reader wrote it under and, per definition, the ones in scope where it was written — see `Context::imports`. What a goal report's candidate pool reaches beyond the names the program already mentions, and half of what a report spells a name by ([`PreparedText::spellings`]).
    ///
    /// **Stored apart from the unit** ([`PreparedText::imports_bytes`]): they are read by the unit's own reports and its documentation, and by nothing compiled after it.
    #[archived_with(curios_archive::Skip)]
    imports: curios_archive::Deferred<curios_core::Imports>,
    /// The mounts this unit may name: the ones it declared a dependency on, or every open one in scope where it declared none, and its own. What its references were guarded by, and what drops a path it may not write from the table a report spells by.
    visible: Vec<Mount>,
    /// Every lint the lowering found, in reading order, less each `unused-binder` lint elaboration credited — see [`Lint`]. Carried with the unit because a lint depends on exactly what the unit's identity in the store depends on: its own sources and its predecessors' interfaces.
    lints: Vec<Lint>,
    /// Every written binder a proof the elaborator wrote read — see [`LintedBinder`]. What a unit compiled over this one as its baseline credits in a declaration it does not elaborate again. A set, since membership is all that is asked of it: its order is then the binders' own, by declaration and place, whichever pass credited which, and a unit compiled over a baseline stores the one a whole compile stores.
    credited: BTreeSet<LintedBinder>,
    /// The prefix of every mount some reference of this unit was *written* under — see `Context::note_spelled`.
    reached: BTreeSet<Qualifier>,
    /// Every item the parser could not read, in reading order — see [`BrokenItem`]. Empty for any unit that compiles.
    broken: Vec<BrokenItem>,
    /// Where each source declaration this unit was lowered from lies: what a position in one of its terms is resolved against where a report is made — see `curios_core::Anchors`.
    ///
    /// **Stored apart from the unit** ([`PreparedText::anchors_bytes`]): a report placed in one of the unit's declarations reads them, and a lowering over the unit, and no compilation that reports nothing.
    #[archived_with(curios_archive::Skip)]
    anchors: curios_archive::Deferred<curios_core::Anchors>,
    /// What each source declaration lowered to beside its items, with what it asked of its scope, held by pointer since a unit lowered over this one keeps the ones that stand, and what each of the unit's files declared — see `lowered`.
    ///
    /// **The unit's baseline half: what a later lowering of the same sources reads, and nothing compiled after this unit does.** So it is no part of what the unit is stored as for its successors. It is stored beside that ([`PreparedText::baseline_bytes`]), handed back as bytes where the unit is restored ([`PreparedText::with_baseline`]), and read out of them when a lowering first asks.
    #[archived_with(curios_archive::Skip)]
    baseline: curios_archive::Deferred<BaselineHalf>,
}

/// Where a unit's lowered items hold a name it declares, and the kind it declares it as.
#[derive(Debug, Clone)]
#[curios_archive::archived]
struct LoweredAt {
    /// The item, by its place among the unit's.
    item: u32,
    declared: curios_core::Declared,
    /// Whether the item is the definition itself, where it would otherwise be a group the name is a member of.
    alone: bool,
}

/// Where `items` hold each name they declare — see [`LoweredAt`].
fn declared_in(items: &[curios_core::Item]) -> BTreeMap<curios_core::Global, LoweredAt> {
    let mut declared = BTreeMap::new();
    for (place, item) in items.iter().enumerate() {
        let item_at = u32::try_from(place).expect("a unit's items are counted in a u32");
        match item {
            curios_core::Item::Let(definition) => {
                declared.insert(
                    definition.name,
                    LoweredAt {
                        item: item_at,
                        declared: definition.kind.declared(),
                        alone: true,
                    },
                );
            }
            curios_core::Item::Rec(rec) => {
                for definition in &rec.definitions {
                    declared.insert(
                        definition.name,
                        LoweredAt {
                            item: item_at,
                            declared: definition.kind.declared(),
                            alone: false,
                        },
                    );
                }
            }
        }
    }

    declared
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

    /// Where each source declaration of this unit lies. See the field.
    pub fn anchors(&self) -> &curios_core::Anchors {
        self.anchors
            .get()
            .unwrap_or_else(|error| panic!("a unit's anchors failed to restore: {error}"))
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
        let credited = &baseline.credited;
        self.credit_where(|binder| {
            binder.declaration.as_ref().is_some_and(&reused) && credited.contains(binder)
        });
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

    /// The lowered module of a unit lowered in this process: what its elaboration reads. A restored unit reads an item where one is asked for (`lowered_item`), a former with the entry it carries (`nominal`), so asking it for the module whole is a fault.
    pub fn core(&self) -> &curios_core::Module {
        assert!(
            self.items.is_none(),
            "a restored unit's lowered module holds no item"
        );

        &self.core
    }

    /// The item at `place` of the ones this unit lowered to, read out of its bytes now where the unit was restored and nothing has read it before.
    pub(crate) fn lowered_item(&self, place: usize) -> &curios_core::Item {
        let Some(items) = &self.items else {
            return &self.core.items[place];
        };
        items[place]
            .get()
            .unwrap_or_else(|error| panic!("a unit's lowered item failed to restore: {error}"))
    }

    /// The entry this unit's lowering wrote for the nominal declaration `name`, read at the former that carries it: the one lowered item, read out of its bytes now where the unit was restored and nothing has read it before. `None` for a name the unit declares no nominal under.
    pub(crate) fn nominal(&self, name: &curios_core::Global) -> Option<&curios_core::Nominal> {
        let declared = self.declared.get(name)?;
        if !declared.declared.is_former() {
            return None;
        }

        self.lowered_item(declared.item as usize)
            .nominals()
            .into_iter()
            .find_map(|(held, nominal)| (held == name).then_some(nominal))
    }

    /// The item this unit's lowering declares `name` by, read out of its bytes now where the unit was restored and nothing has read it before. `None` for a name the unit does not declare.
    pub fn lowered(&self, name: &curios_core::Global) -> Option<&curios_core::Item> {
        self.declared
            .get(name)
            .map(|declared| self.lowered_item(declared.item as usize))
    }

    /// How this unit declares `name`, as a witness's key reads it: its kind, and its lowered body where it is a definition written on its own. `None` for a name the unit does not declare.
    ///
    /// **Nothing is read to say what kind a name is, and one item to say how a `let` is written.** A key is read through the definitions a signature names, which are few, and most of them type formers, whose kind is the whole answer.
    pub(crate) fn written(
        &self,
        name: &curios_core::Global,
    ) -> Option<(curios_core::Declared, Option<&curios_core::Term>)> {
        let declared = self.declared.get(name)?;
        let body = match declared.declared {
            curios_core::Declared::Authored if declared.alone => {
                match self.lowered_item(declared.item as usize) {
                    curios_core::Item::Let(definition) => Some(&definition.body),
                    curios_core::Item::Rec(_) => None,
                }
            }
            _ => None,
        };

        Some((declared.declared, body))
    }

    /// The `foreign` rows this unit declares.
    pub fn foreigns(&self) -> &ForeignStore {
        &self.foreigns
    }

    /// What this unit's lowering minted — see the field.
    pub fn minted(&self) -> &curios_core::Minted {
        &self.minted
    }

    /// What each source declaration of this unit lowered to beside its items — see the field. Read out of the unit's baseline half now, where the unit was restored and no lowering has asked before.
    fn lowerings(&self) -> &[Rc<LoweredDeclaration>] {
        &self.half().declarations
    }

    /// What each file of this unit declared, by the module it backs — see [`Scan`].
    fn scans(&self) -> &BTreeMap<Qualifier, Scan> {
        &self.half().scans
    }

    /// The unit's baseline half, read out of its bytes now where the unit was restored and nothing has asked before.
    fn half(&self) -> &BaselineHalf {
        self.baseline
            .get()
            .unwrap_or_else(|error| panic!("a unit's baseline half failed to restore: {error}"))
    }

    /// The bytes of this unit's baseline half: what a later lowering of the same sources keeps this unit's lowerings by, stored apart from what the unit is for its successors.
    pub fn baseline_bytes(&self) -> Result<Cow<'_, [u8]>, String> {
        self.baseline.bytes()
    }

    /// This unit with `bytes` as its baseline half, read when a lowering over the unit first asks: what a unit restored without one is given, since restoring it leaves the half empty.
    pub fn with_baseline(mut self, bytes: impl Into<Bytes>) -> Self {
        self.baseline = curios_archive::Deferred::stored(bytes);
        self
    }

    /// The bytes of this unit's import scopes, stored apart from what the unit is for its successors: the ones they were restored from, or their archive, made now.
    pub fn imports_bytes(&self) -> Result<Cow<'_, [u8]>, String> {
        self.imports.bytes()
    }

    /// The bytes of where this unit's declarations lie, stored apart as its import scopes are.
    pub fn anchors_bytes(&self) -> Result<Cow<'_, [u8]>, String> {
        self.anchors.bytes()
    }

    /// This unit with `imports` as its import scopes and `anchors` as where its declarations lie ([`PreparedText::imports_bytes`], [`PreparedText::anchors_bytes`]), each read when something first asks: what a restored unit is given, since restoring it leaves it with neither.
    pub fn with_reported(mut self, imports: impl Into<Bytes>, anchors: impl Into<Bytes>) -> Self {
        self.imports = curios_archive::Deferred::stored(imports);
        self.anchors = curios_archive::Deferred::stored(anchors);
        self
    }

    /// The bytes of this unit's lowered items, each stored on its own behind how many they are and how long each is ([`joined`]): the ones an item was restored from where the unit was, and its archive, made now, where it was lowered here.
    pub fn items_bytes(&self) -> Result<Vec<u8>, String> {
        match &self.items {
            Some(items) => items
                .iter()
                .map(curios_archive::Deferred::bytes)
                .collect::<Result<Vec<_>, _>>()
                .map(|parts| joined(&parts)),
            None => self
                .core
                .items
                .iter()
                .enumerate()
                .map(|(place, item)| match self.carried.get(place) {
                    Some(Some(carried)) => {
                        debug_assert!(
                            curios_archive::to_bytes(item)
                                .is_ok_and(|made| *made == *carried.as_slice()),
                            "an item taken from a baseline whole is stored as the baseline stored it"
                        );

                        Ok(Cow::Borrowed(carried.as_slice()))
                    }
                    _ => curios_archive::to_bytes(item).map(|made| Cow::Owned(made.to_vec())),
                })
                .collect::<Result<Vec<_>, String>>()
                .map(|parts| joined(&parts)),
        }
    }

    /// This unit with `bytes` as its lowered items ([`PreparedText::items_bytes`]), each read when something first asks for it: what a restored unit is given, since restoring it leaves its module without one. `Err` is bytes that hold no such items, or not one for each the unit declares.
    pub fn with_items(mut self, bytes: impl Into<Bytes>) -> Result<Self, String> {
        let items = parted(&bytes.into()).ok_or("not a unit's lowered items")?;
        if self
            .declared
            .values()
            .any(|declared| declared.item as usize >= items.len())
        {
            return Err("a unit's lowered items hold fewer than it declares".to_string());
        }

        self.core.items = Vec::new().into();
        self.items = Some(
            items
                .into_iter()
                .map(curios_archive::Deferred::stored)
                .collect(),
        );

        Ok(self)
    }

    /// The names this unit declares by a lowering it took from `baseline` whole: each one's item is the baseline's own under the same name, with the registry entries the baseline holds for it. What a recompile has no need to compare.
    ///
    /// **By the record, never by the text.** A declaration is the baseline's where this unit holds the very record the baseline does, which only a kept lowering leaves behind; and under the same name, since a kept witness is named anew and may take the name another held.
    pub fn kept_from(&self, baseline: &PreparedText) -> BTreeSet<curios_core::Global> {
        self.taken_from(baseline)
            .into_iter()
            .flat_map(|(ours, _)| self.lowered_item(ours).declared_names())
            .copied()
            .collect()
    }

    /// The places of the items this unit took from `baseline` whole, each beside the baseline's place for it: an item of a declaration whose record is the baseline's own, under the names the baseline holds it under.
    fn taken_from(&self, baseline: &PreparedText) -> Vec<(usize, usize)> {
        let mut held = HashMap::with_capacity(baseline.lowerings().len());
        let mut first = 0;
        for declaration in baseline.lowerings() {
            held.insert(Rc::as_ptr(declaration), first);
            first += declaration.facts().len();
        }

        let mut taken = Vec::new();
        let mut first = 0;
        for declaration in self.lowerings() {
            let count = declaration.facts().len();
            if let Some(&from) = held.get(&Rc::as_ptr(declaration)) {
                for place in 0..count {
                    let ours = self.lowered_item(first + place);
                    let theirs = baseline.lowered_item(from + place);
                    if ours.declared_names() == theirs.declared_names() {
                        taken.push((first + place, from + place));
                    }
                }
            }
            first += count;
        }

        taken
    }

    /// The bytes the item at `place` was stored as, where this unit holds them: the ones it was restored from, or the ones it carries of a baseline.
    fn item_stored(&self, place: usize) -> Option<curios_archive::Bytes> {
        match &self.items {
            Some(items) => items[place].as_stored().cloned(),
            None => self.carried.get(place).cloned().flatten(),
        }
    }

    /// Carry, for each item this unit took from `baseline` whole, the bytes the baseline stored it as: what [`PreparedText::items_bytes`] stores it as again.
    fn carry(&mut self, baseline: &PreparedText) {
        let mut carried = vec![None; self.core.items.len()];
        for (ours, theirs) in self.taken_from(baseline) {
            carried[ours] = baseline.item_stored(theirs);
        }
        self.carried = carried;
    }

    /// What each `use` brought into scope, where, and under which spelling — the table `curios-elab`'s goal suggestions draw imported candidates from and spell them by.
    pub fn imports(&self) -> &curios_core::Imports {
        self.imports
            .get()
            .unwrap_or_else(|error| panic!("a unit's import scopes failed to restore: {error}"))
    }

    /// Who can see each declaration of this unit. See the field.
    fn audiences(&self) -> &Audiences {
        self.audiences
            .get_or_init(|| Audiences::of(&self.table, &self.public))
    }

    /// Every absolute path this unit gives a global, with who may write it — see [`writable_paths`]. Made once for the unit: every report of every compilation it is in scope of reads the one table.
    fn paths(&self) -> Rc<BTreeMap<curios_core::Global, Vec<curios_core::WritablePath>>> {
        let paths = self
            .paths
            .get_or_init(|| Rc::new(writable_paths(&self.table, &self.public, self.audiences())));

        Rc::clone(paths)
    }

    /// How this unit's names can be written, where — what every report spells a name by: its imports, and the absolute paths each unit of its scope gives a global, `predecessors` being the units it was lowered against.
    ///
    /// **A unit's paths are made where a report first asks, once for the unit, and stored by none.** They are a function of the direct interface and export view the unit already stores, and a report is their one reader: a compilation that reports nothing makes none, and one that reports makes only the ones no report before it asked for.
    pub fn spellings(&self, predecessors: &[&PreparedText]) -> curios_core::Spellings {
        spellings_over(
            self.imports(),
            self.paths(),
            &self.mounts,
            &self.visible,
            predecessors,
        )
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
        root: &[Headed],
        scans: Option<&BTreeMap<Qualifier, Scan>>,
        predecessor_tables: &'a [&'a BTreeMap<Qualifier, ModuleInfo>],
        predecessor_mounts: &[Mount],
        own: &[Mount],
    ) -> Result<Self, Error> {
        curios_profile::profile!("resolved::of");
        let mut resolved = Resolved {
            modules: HashMap::new(),
            mod_spans: HashMap::new(),
            table: Scoped::over(predecessor_tables),
        };

        // The compilation root: the entry's own module when the entry is what is being lowered, and otherwise a synthetic one belonging to no unit — which is why its children are *every* mounted prefix rather than only this unit's.
        //
        // Writing it lands in this unit's own layer, which shadows whatever the predecessors' layer said, so listing only `own` here would silently hide the predecessors' mounts — `/std` among them — from a unit being compiled against them, which the test that says a unit reaches a mounted name holds.
        let mut root_info = scan_module_info(root)?;
        for child in mounted_children(predecessor_mounts.iter().chain(own)) {
            root_info.insert_child(&Label::from(child), true)?;
        }
        resolved.table.insert(Qualifier::empty(), root_info);

        // The entry's children hang off the root, whose `ModuleInfo` was just built by hand; a mounted unit has no root items, so this recurses over nothing for it.
        resolved.discover_children(root, &Qualifier::empty(), source, scans)?;

        for mount in own.iter().filter(|mount| !mount.prefix.is_root()) {
            let header = Rc::new(Loaded::of(source, scans, &mount.prefix)?);
            resolved.modules.insert(mount.prefix, Rc::clone(&header));
            resolved.discover(&header.heads, &mount.prefix, source, scans)?;
        }

        Ok(resolved)
    }

    // `mod` declarations only name children, so the module graph is a tree: every qualifier is reached exactly once and no cycles are possible. Hence the walk needs neither a visited-set nor a cache hit-check — just load each file module once and recurse.
    fn discover(
        &mut self,
        heads: &[Headed],
        prefix: &Qualifier,
        source: &UnitSource<'_>,
        scans: Option<&BTreeMap<Qualifier, Scan>>,
    ) -> Result<(), Error> {
        self.table.insert(*prefix, scan_module_info(heads)?);
        self.discover_children(heads, prefix, source, scans)
    }

    // The child-recursion half of `discover`, split out so `of` can build the compilation root's `ModuleInfo` itself (with every mounted prefix pre-registered as a child) and recurse into its children without a second, unconditional `scan_module_info` call clobbering that registration.
    fn discover_children(
        &mut self,
        heads: &[Headed],
        prefix: &Qualifier,
        source: &UnitSource<'_>,
        scans: Option<&BTreeMap<Qualifier, Scan>>,
    ) -> Result<(), Error> {
        for headed in heads {
            if let Head::Mod {
                label,
                span,
                inline,
                ..
            } = &headed.head
            {
                let path = prefix.with(label);
                if let Some(written) = label.span() {
                    self.mod_spans.insert(path, written.clone());
                }

                match inline {
                    Some(heads) => self.discover(heads, &path, source, scans)?,
                    None => {
                        let module =
                            Loaded::of(source, scans, &path).map_err(|error| match span {
                                Some(span) => error.at(span.clone()),
                                None => error,
                            })?;
                        let module = Rc::new(module);

                        self.modules.insert(path, Rc::clone(&module));
                        self.discover(&module.heads, &path, source, scans)?;
                    }
                }
            }
        }

        Ok(())
    }
}

fn scan_module_info(heads: &[Headed]) -> Result<ModuleInfo, Error> {
    let mut info = ModuleInfo::new();

    for headed in heads {
        match &headed.head {
            Head::Mod { label, vis_pub, .. } => info.insert_child(label, *vis_pub)?,
            Head::Let(group) => {
                for let_ in group {
                    info.insert_binding(&let_.label, let_.vis_pub)?;
                }
            }
            Head::Induct(group) => {
                for induct in group {
                    info.insert_induct_child(&induct.label, induct.vis_pub, induct.rep_pub)?;
                    info.insert_binding(&induct.label, induct.vis_pub)?;
                }
            }
            // A struct declares one binding (the type-former), like a `let` — there are no value constructors and no nested namespace, so no child module.
            Head::Struct(group) => {
                for struct_ in group {
                    info.insert_binding(&struct_.label, struct_.vis_pub)?;
                }
            }
            // A concept declares the type-former binding *and* a nested namespace (its method wrappers), like an inductive.
            Head::Concept(group) => {
                for concept in group {
                    info.insert_child(&concept.label, concept.vis_pub)?;
                    info.insert_binding(&concept.label, concept.vis_pub)?;
                }
            }
            // A witness is anonymous: it declares no binding and occupies no lexical scope — its backing definition gets a compiler name.
            Head::Witness => {}
            // A `foreign` declaration is an ordinary binding, like a `let` — it has no body of its own, but it is called the same way.
            Head::Foreign(foreign) => info.insert_binding(&foreign.label, foreign.vis_pub)?,
            // A test binds its name privately — referable within its subtree, colliding with a like-named sibling, never `pub`.
            Head::Test(label) => info.insert_binding(label, false)?,
            // A broken item binds the name its head declared, public so a reference from anywhere resolves to it and is withheld rather than reported unbound; nothing can use the binding.
            Head::Broken { declares, .. } => {
                if let Some(label) = declares {
                    info.insert_binding(label, true)?;
                }
            }
            Head::Use(_) => {}
        }
    }

    Ok(info)
}

// The surface concept application `C(args)` for a witness's declared type: the witnessed concept applied to the annotation's arguments under the marks they were written with, as any application is — a list with nothing written included, which is the application `C()` and not the bare name.
//
// **Spanned over the written `C(args)`, because this is the only thing a `satisfy` refusal can point at.** A witness is anonymous, so nothing else in its declaration names it: `elaborate_module_let` locates a registration failure with `error.at_opt(def.type_.span())`, and this synthesized node is that type. Left spanless, every duplicate, orphan, unkeyable and irregular-premise refusal would arrive with no line at all — and a duplicate between two witnesses of one module would name that module twice and nothing else, which a reader cannot act on. `written` is the head as the parser read it, through the parenthesis that closes its arguments; `Term::with_span` keeps an innermost span already present, so the arguments are untouched.
fn witness_concept_application(
    concept: &Name,
    args: Option<&[Argument]>,
    written: Option<&Span>,
) -> Term {
    let head: Term = Subterm::Name(concept.clone()).into();
    let applied: Term = match args {
        Some(args) => Subterm::Apply(Apply {
            head,
            arguments: args.to_vec(),
        })
        .into(),
        None => head,
    };

    match written {
        Some(span) => applied.with_span(span.clone()),
        None => applied,
    }
}

/// The name a declaration's parameter is minted under: its own, or none for a `use` parameter, which occupies a binder and names nothing.
fn param_name(param: &FuncTypeParam) -> String {
    param.label.clone().unwrap_or_default()
}

/// What a declaration's parameter binds as at a value constructor: a `use` parameter stays the witness slot it is at the type constructor, and every other is implicit, inferred from the payload or the expected type.
fn constructor_plicity(declared: Plicity) -> Plicity {
    match declared {
        Plicity::Witness => Plicity::Witness,
        Plicity::Explicit | Plicity::Implicit => Plicity::Implicit,
    }
}

/// What `top_item`'s text is anchored as: the declaration of the first name it declares, or one of its module's unnamed declarations where it declares none. `None` for an item no term is lowered from.
fn anchored(head: &Head, context: &Context) -> Option<curios_core::Anchored> {
    let named = |label: &Label| curios_core::Anchored::Declaration(context.prefixed(label));

    match head {
        Head::Let(group) => group.first().map(|let_| named(&let_.label)),
        Head::Induct(group) => group.first().map(|induct| named(&induct.label)),
        Head::Struct(group) => group.first().map(|struct_| named(&struct_.label)),
        Head::Concept(group) => group.first().map(|concept| named(&concept.label)),
        Head::Witness => Some(curios_core::Anchored::Unnamed(context.island())),
        Head::Foreign(foreign) => Some(named(&foreign.label)),
        Head::Test(label) => Some(named(label)),
        Head::Mod { .. } | Head::Use(_) | Head::Broken { .. } => None,
    }
}

/// What a unit's declarations lower to, as the walk over its modules gathers it.
///
/// This unit's own, never the predecessors' extended in place. What a predecessor declares stays its own, and the one pass that asks about one — the public-exposure audit, whose alias walk may land on a predecessor's type — takes them to query rather than to own.
#[derive(Default)]
struct Lowering<'a> {
    /// The unit an earlier text of these sources lowered to, when this one is lowered over it.
    baseline: Option<&'a Baseline<'a>>,
    items: Vec<FlatItem>,
    /// What each declaration lowered to beside its items, in the walk's order, holding what the unit-wide passes read of them — see [`LoweredDeclaration`].
    declarations: Vec<Rc<LoweredDeclaration>>,
    induct_decls: BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: BTreeMap<curios_core::Global, curios_core::StructDecl>,
    concepts: BTreeMap<curios_core::Global, curios_core::ConceptDecl>,
    witnesses: BTreeSet<curios_core::Global>,
    tests: Vec<curios_core::Global>,
    foreigns: ForeignStore,
    broken: Vec<BrokenItem>,
}

impl Lowering<'_> {
    /// Record what the declaration `context` has just lowered to, the unit's items from `from` on: what the unit-wide passes read of each, taken while its registry entries and the text it was written in are at hand, and what the declaration asked of its scope.
    fn note(&mut self, context: &Context, from: usize) {
        let facts = self.items[from..]
            .iter()
            .map(|item| {
                LoweredItem::of(
                    item,
                    |span| context.extent(span),
                    &self.induct_decls,
                    &self.struct_decls,
                    &context.syntax(),
                )
            })
            .collect();
        self.declarations.push(Rc::new(LoweredDeclaration::made(
            context,
            &self.items[from..],
            facts,
        )));
    }
}

fn process_items(
    heads: &[Headed],
    body: Body<'_>,
    context: &mut Context,
    lowered: &mut Lowering,
    modules: &HashMap<Qualifier, Rc<Loaded>>,
) -> Result<(), Error> {
    for headed in heads {
        match &headed.head {
            Head::Mod { label, .. } => {
                context.insert_scope(label.to_string(), label.span(), context.prefixed(label))?
            }
            Head::Broken { declares, .. } => {
                if let Some(label) = declares {
                    context.insert_binding(
                        label.to_string(),
                        label.span(),
                        context.prefixed(label),
                    )?;
                }
            }
            Head::Let(group) => {
                for let_ in group {
                    context.insert_binding(
                        let_.label.to_string(),
                        let_.label.span(),
                        context.prefixed(&let_.label),
                    )?;
                }
            }
            Head::Induct(group) => {
                for induct in group {
                    context.insert_scope(
                        induct.label.to_string(),
                        induct.label.span(),
                        context.prefixed(&induct.label),
                    )?;
                    context.insert_binding(
                        induct.label.to_string(),
                        induct.label.span(),
                        context.prefixed(&induct.label),
                    )?;
                }
            }
            // The type-former binding only — like a `let` (no constructor namespace).
            Head::Struct(group) => {
                for struct_ in group {
                    context.insert_binding(
                        struct_.label.to_string(),
                        struct_.label.span(),
                        context.prefixed(&struct_.label),
                    )?;
                }
            }
            // A concept declares its type-former binding and a nested namespace for the method wrappers, like an inductive.
            Head::Concept(group) => {
                for concept in group {
                    context.insert_scope(
                        concept.label.to_string(),
                        concept.label.span(),
                        context.prefixed(&concept.label),
                    )?;
                    context.insert_binding(
                        concept.label.to_string(),
                        concept.label.span(),
                        context.prefixed(&concept.label),
                    )?;
                }
            }
            // A witness is anonymous — no binding, no scope entry.
            Head::Witness | Head::Use(_) => {}
            Head::Foreign(foreign) => context.insert_binding(
                foreign.label.to_string(),
                foreign.label.span(),
                context.prefixed(&foreign.label),
            )?,
            Head::Test(label) => {
                context.insert_binding(label.to_string(), label.span(), context.prefixed(label))?
            }
        }
    }

    for (index, headed) in heads.iter().enumerate() {
        // A source declaration mints in a space of its own, shared by every item it lowers to; a nested module's declarations each take theirs.
        let declares = !matches!(headed.head, Head::Mod { .. });
        let lowered_from = lowered.items.len();
        if declares {
            context.begin_declaration();
            // And is written in a text of its own: what it lowers to holds positions in that text, wherever the file holds it. A module built rather than parsed has no text, and its terms no position.
            if let (Some(anchored), Some(span)) = (anchored(&headed.head, context), &headed.own) {
                context.anchor(anchored, span);
            }
            // A declaration the baseline lowered from this text, every name it resolved standing, is the baseline's.
            if let Some(baseline) = lowered.baseline
                && let Some(standing) = baseline.standing(context)
            {
                baseline.kept(standing, context, lowered);
                continue;
            }
        }
        match &headed.head {
            // Nothing lowers: the item is carried as its report, under the name it declared, for the compile boundary to refuse the unit by and the elaborator to withhold dependents of.
            Head::Broken { declares, report } => {
                lowered.broken.push(BrokenItem {
                    declares: declares
                        .as_ref()
                        .map(|label| curios_core::Global::Authored(context.prefixed(label))),
                    report: report.clone(),
                });
                continue;
            }
            Head::Mod { label, inline, .. } => {
                match inline {
                    Some(heads) => process_items(
                        heads,
                        Body::Inline(&body, index),
                        &mut context.nested(label),
                        lowered,
                        modules,
                    )?,
                    None => {
                        let path = context.prefixed(label);
                        // Discovery is exhaustive over this same tree, so every file-backed module is already cached under this qualifier.
                        let module = modules.get(&path).expect("module loaded during discovery");

                        process_items(
                            &module.heads,
                            Body::File(module),
                            &mut context.nested(label),
                            lowered,
                            modules,
                        )?;
                    }
                }
                continue;
            }
            Head::Use(use_item) => {
                curios_profile::profile!("resolve_use");
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
                continue;
            }
            _ => {}
        }
        // Only a declaration reaches here, and only here is its tree read.
        match &body.items()?[index] {
            TopItem::Broken(_) | TopItem::Mod(_) | TopItem::Use(_) => {
                unreachable!("a head stands where its item does")
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
                        let type_ = context.with_sorts(SortSite::Signature, || {
                            lower.signature_type(&signature, None)
                        })?;
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
                lowered.items.push(match recursive {
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
                lowered.tests.push(name);
                lowered.items.push(FlatItem::Let(FlatLet {
                    kind: curios_core::DefinitionKind::Test,
                    span: test.label.span().cloned(),
                    name,
                    island: context.island(),
                    type_,
                    body: lower.value(&body)?,
                }));
            }
            TopItem::Foreign(f) => {
                // A `foreign` is the function of its row. The row is registered under the name the declaration binds, which is its import, and the declaration's type and body are both read off it.
                let path = context.prefixed(&f.label);
                let function = ForeignFunction {
                    name: path.join(),
                    signature: f.signature.clone(),
                };
                lowered.foreigns.register(function.clone());

                let name = curios_core::Global::Authored(path);
                context.record_import_scope(Some(&name));
                let lower = Lowerer::new(context, Some(name));
                let (type_, body) = lower.foreign(&Arc::new(function), name);
                lowered.items.push(FlatItem::Let(FlatLet {
                    kind: curios_core::DefinitionKind::Authored,
                    span: f.label.span().cloned(),
                    name,
                    island: context.island(),
                    type_,
                    body,
                }));
            }
            TopItem::Induct(group) => {
                // An inductive's lowering is its signature throughout: parameters, indices and constructor telescopes.
                context.sorts_stand_in(SortSite::Signature);
                // Step 1: type bindings as one rec group. An inductive's type binding wraps an intrinsic `InductType` normal form in a `Func` over its type parameters and indices (so `Result(Bin, Nat)` beta-reduces to `InductType { Result, [Bin, Nat] }` and `Vec(Bin, 3)` to `InductType { Vec, [Bin], [3] }`), and its shape is recorded in the inductive registry.
                let type_flat_items = group
                    .iter()
                    .map(|u| {
                        let name = curios_core::Global::Authored(context.prefixed(&u.label));
                        context.record_import_scope(Some(&name));
                        let lower = Lowerer::new(context, Some(name));

                        // Parameters and indices are minted before any of their types is lowered, and each type sees the binders before it — a later index type naming an earlier parameter must mean *that* binder.
                        let head_binders =
                            lower.mint(u.params.iter().map(param_name).chain(
                                u.indices.iter().map(|(n, _)| n.clone().unwrap_or_default()),
                            ));
                        let (param_binders, index_binders) = head_binders.split_at(u.params.len());

                        let param_tys = u
                            .params
                            .iter()
                            .enumerate()
                            .map(|(i, param)| {
                                let ty = lower
                                    .bound(&head_binders[..i], || lower.input_type(&param.type_))?;
                                Ok((param.plicity, param_binders[i].1, ty))
                            })
                            .collect::<Result<Vec<_>, Error>>()?;
                        // The registry's telescope and the `InductType` normal form are positional; the marks ride beside the telescope and on the generated type-constructor function.
                        let param_tys_unmarked = param_tys
                            .iter()
                            .map(|(_, n, t)| (*n, t.clone()))
                            .collect::<Vec<_>>();

                        let param_vars = param_binders
                            .iter()
                            .map(|(_, id)| curios_core::Term::var(curios_core::Var::free(*id)))
                            .collect::<Vec<_>>();

                        // The head's index telescope. An unnamed entry has no name, so no later index type depends on it.
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
                                let payload_binders = lower.mint(
                                    c.payload
                                        .iter()
                                        .map(|param| param.label.clone().unwrap_or_default()),
                                );
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

                                // The value constructor's calling convention: every leading declaration parameter is hidden, each payload keeps its declared mark — the same source `ctor_type` uses.
                                let plicities = u
                                    .params
                                    .iter()
                                    .map(|param| constructor_plicity(param.plicity))
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

                        lowered.induct_decls.insert(
                            name,
                            curios_core::InductDecl {
                                universe_context: curios_core::UniverseContext::empty(),
                                arity: curios_core::Telescope::build_marked(
                                    param_tys.clone(),
                                    curios_core::Telescope::build(index_tys.iter().cloned(), ()),
                                ),
                                constructors,
                                result_sort: result_sort.clone(),
                                module: context.island(),
                                rep_public: u.rep_pub,
                                // Positivity has not run yet: `curios-elab` computes each declaration's parameter polarities after elaboration and writes them back here.
                                polarities: Vec::new(),
                                variances: Vec::new(),
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
                            kind: curios_core::DefinitionKind::Nominal(Rc::new(
                                curios_core::Nominal::Inductive(
                                    lowered.induct_decls[&name].clone(),
                                ),
                            )),
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
                // Making this conditional is not the two-line change the neighbours make it look like, and both halves were measured rather than reasoned. `mentions_itself` reads `free_vars_shared()`, but an inductive's recursion lives in its *registry entry* — `InductType` holds `name: Global` as a field, not a `Var` — so the test is false for every inductive, recursive ones included, and reaching for it lowers a recursive inductive such as `/std/Cli/Values` as a `Let` that loses `elaborate_module_rec`'s `context.assume` and leaves its own name unbound while the registry telescopes rebuild. Reading the reach through `references::induct_free_vars` instead clears that and then fails at `/std/Async/Future/State` with `universe instance has 1 arguments but its scheme expects 0`: the group is also where a universe-polymorphic inductive's levels are generalized, and as a `Let` the scheme comes out monomorphic while occurrences still pass a level.
                //
                // So the wrapper does double duty, and dropping it needs `elaborate_module_let` to do both jobs. Nothing depends on that — a folded spelling reduces correctly, `curios-elab`'s `unfold_rec_apply` applying a group that dissolved to its member's value — which leaves this an optimization with no measurement behind it.
                lowered.items.push(FlatItem::Rec(type_flat_items));

                // Step 2: constructor bindings. Each is a function whose body injects the variant, an intrinsic `Variant` normal form.
                for u in group {
                    for c in &u.cases {
                        let name = curios_core::Global::Authored(
                            context.prefixed(&u.label).with(&c.label),
                        );
                        context.record_import_scope(Some(&name));
                        let lower = Lowerer::new(context, Some(name));

                        // Output type term `T`, `T(A, ...)`, `T(target...)`, or — indexed with parameters — the case's full terminal `T(A, ...)(target...)`: a name ref applied the way a use site writes it, one call for the parameters and one for the target's index expressions. A `use` parameter has no name to write and is left out as a use site leaves it: the slot resolves to the constructor's own premise, the nearest witness in scope.
                        let parameters: Vec<Argument> = u
                            .params
                            .iter()
                            .filter_map(|param| {
                                Some(Argument {
                                    term: Subterm::Name(Name::from(vec![param.label.clone()?]))
                                        .into(),
                                    // Each argument's mark must match its binder on the type constructor (the two-queue rule): an `@`-marked parameter is filled from the implicit queue.
                                    plicity: param.plicity,
                                })
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

                        // Constructor type: (params..., payload...) -> T. Every plain or `@` inductive parameter is implicit at the value constructor — `Result/success(42)` infers them, the call-site `@` supplies one positionally — and a `use` parameter stays a witness slot, while the payload binders keep their declared marks (`@m` makes one implicit; the default is explicit).
                        let binders = lower.mint(
                            u.params.iter().map(param_name).chain(
                                c.payload
                                    .iter()
                                    .map(|param| param.label.clone().unwrap_or_default()),
                            ),
                        );
                        let plicities = u
                            .params
                            .iter()
                            .map(|param| constructor_plicity(param.plicity))
                            .chain(c.payload.iter().map(|param| param.plicity))
                            .collect::<Vec<_>>();
                        let written = u
                            .params
                            .iter()
                            .map(|param| &param.type_)
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
                        // Constructor body: (params..., payload...) => the variant's injection, an intrinsic `Variant` normal form.
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
                        // The value constructor carries the same calling convention as `ctor_type`: every inductive parameter is hidden, each payload keeps its declared mark.
                        let ctor_body = curios_core::Term::func_marked(param_tys, inject);

                        lowered.items.push(FlatItem::Let(FlatLet {
                            span: c.label.span().cloned(),
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
                // A structure's lowering is its signature throughout: parameters and fields.
                context.sorts_stand_in(SortSite::Signature);
                let mut formers = Vec::with_capacity(group.len());
                for s in group {
                    let name = curios_core::Global::Authored(context.prefixed(&s.label));
                    context.record_import_scope(Some(&name));
                    let lower = Lowerer::new(context, Some(name));

                    // Declaring module: the type-former's qualifier prefix — identical to core's per-item `island` — for the representation-privacy checks.
                    let module = context.prefixed(&s.label).without_last();

                    let param_binders = lower.mint(s.params.iter().map(param_name));
                    let param_tys = s
                        .params
                        .iter()
                        .enumerate()
                        .map(|(i, param)| {
                            let ty = lower
                                .bound(&param_binders[..i], || lower.input_type(&param.type_))?;
                            Ok((param.plicity, param_binders[i].1, ty))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;
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

                    // Field types, each under the fields before it, so a later field type can depend on an earlier field that has a name. The signature sugar `f(params) -> T` is undone here.
                    let field_binders = lower.mint(s.fields.iter().map(|field| {
                        field
                            .param
                            .label
                            .as_deref()
                            .map(str::to_string)
                            .unwrap_or_default()
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
                            Ok((field.plicity, field_binders[i].1, ty))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;

                    // Registry entry: the parameter telescope, and the full field telescope (parameter binders first — field types may mention them — then field binders), as in `Inductive::indices`. The declared result sort (`Type`/`Prop`) — closed; both the registry entry's sort and the type-former's codomain.
                    let result_sort = lower.term(&s.result_sort)?;

                    lowered.struct_decls.insert(
                        name,
                        curios_core::StructDecl {
                            universe_context: curios_core::UniverseContext::empty(),
                            arity: curios_core::Telescope::build_marked(
                                param_tys.clone(),
                                curios_core::Telescope::build_marked(field_tys, ()),
                            ),
                            result_sort: result_sort.clone(),
                            module,
                            rep_public: s.rep_pub,
                            // Positivity has not run yet: `curios-elab` computes each declaration's parameter polarities after elaboration and writes them back here.
                            polarities: Vec::new(),
                            variances: Vec::new(),
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
                        kind: curios_core::DefinitionKind::Nominal(Rc::new(
                            curios_core::Nominal::Struct(lowered.struct_decls[&name].clone()),
                        )),
                        span: s.label.span().cloned(),
                        name: curios_core::Global::Authored(context.prefixed(&s.label)),
                        island: context.island(),
                        type_,
                        body,
                    });
                }
                lowered.items.push(match formers.len() {
                    1 => FlatItem::Let(formers.pop().expect("a `struct` item has a member")),
                    _ => FlatItem::Rec(formers),
                });
            }
            // A concept lowers to a representation-public nominal `StructDecl` and its type-former `let` — plus a concept-registry entry (field labels, superclass edges, the parameter telescope) and one method-wrapper `let` per field, synthed into the concept's own namespace.
            // A concept group lowers its formers into one `rec` item as a struct group does; the method wrappers stay `let` items of their own, since each names only its former.
            TopItem::Concept(group) => {
                context.sorts_stand_in(SortSite::Concept);
                let mut formers = Vec::with_capacity(group.len());
                for concept in group {
                    let name = curios_core::Global::Authored(context.prefixed(&concept.label));
                    let module = context.prefixed(&concept.label).without_last();

                    // A concept's premise is a superclass field, which every dictionary carries and resolution reaches through; a `use` parameter would instead make the premise's dictionary part of the concept's own identity, so two witnesses of one concept at one type could differ in nothing a key reads.
                    if let Some(param) = concept
                        .params
                        .iter()
                        .find(|param| param.plicity == Plicity::Witness)
                    {
                        let error = Error::ConceptUseParameter {
                            concept: concept.label.to_string(),
                        };
                        return Err(match param.type_.span() {
                            Some(span) => error.at(span.clone()),
                            None => error,
                        });
                    }

                    context.record_import_scope(Some(&name));
                    let lower = Lowerer::new(context, Some(name));
                    let param_binders = lower.mint(concept.params.iter().map(param_name));
                    let param_tys = concept
                        .params
                        .iter()
                        .enumerate()
                        .map(|(i, param)| {
                            let ty = lower
                                .bound(&param_binders[..i], || lower.input_type(&param.type_))?;
                            Ok((param.plicity, param_binders[i].1, ty))
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

                    // A superclass field is anonymous: it has no label, so nothing projects it or names it in a later field's type, and resolution reaches it by index.
                    let field_labels = concept
                        .fields
                        .iter()
                        .map(|field| {
                            field
                                .label
                                .as_ref()
                                .map(|label| label.to_string())
                                .unwrap_or_default()
                        })
                        .collect::<Vec<_>>();

                    // Field types, lowered under the parameter scope (a method field's label is the binder for later fields; a superclass field binds nothing). The signature sugar `f(params) -> T` is undone here.
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
                            // A superclass edge is the field telescope's `use` member.
                            let mark = match field.is_super() {
                                true => Plicity::Witness,
                                false => Plicity::Explicit,
                            };
                            Ok((mark, field_binders[i].1, ty))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;

                    let result_sort = lower.term(&concept.result_sort)?;

                    // The record shape drives struct literals, projections, and — through `field_type_from` below — the declared type of every method wrapper.
                    let arity = curios_core::Telescope::build_marked(
                        param_tys.clone(),
                        curios_core::Telescope::build_marked(field_tys, ()),
                    );
                    lowered.struct_decls.insert(
                        name,
                        curios_core::StructDecl {
                            universe_context: curios_core::UniverseContext::empty(),
                            arity: arity.clone(),
                            result_sort: result_sort.clone(),
                            module,
                            rep_public: concept.rep_pub,
                            // Positivity has not run yet: `curios-elab` computes each declaration's parameter polarities after elaboration and writes them back here.
                            polarities: Vec::new(),
                            variances: Vec::new(),
                        },
                    );

                    lowered.concepts.insert(
                        name,
                        curios_core::ConceptDecl {
                            universe_context: curios_core::UniverseContext::empty(),
                            params: curios_core::Telescope::build(param_tys_unmarked.clone(), ()),
                            fields: field_labels.clone(),
                            // What each `use` field reaches is read off its elaborated type, where the fields are elaborated.
                            supers: Vec::new(),
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
                        kind: curios_core::DefinitionKind::Nominal(Rc::new(
                            curios_core::Nominal::Concept {
                                concept: lowered.concepts[&name].clone(),
                                record: lowered.struct_decls[&name].clone(),
                            },
                        )),
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
                    for (index, label) in concept
                        .fields
                        .iter()
                        .enumerate()
                        .filter_map(|(index, field)| Some((index, field.label.as_ref()?)))
                    {
                        // `index` is the field's position in the *whole* telescope, superclass slots included. Counting only the fields that get wrappers would read every method after a superclass one slot early.
                        let witness_id = lower.mint([String::new()]).remove(0).1;
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
                                let mut own = Vec::with_capacity(function.telescope.len());
                                while let Some((hint, domain)) = cursor.entry() {
                                    let mark = cursor.mark().expect("a mark stands at an entry");
                                    let binder = lower
                                        .mint([hint.unwrap_or_default().to_string()])
                                        .remove(0)
                                        .1;
                                    cursor.advance(curios_core::Term::free_var(&binder));
                                    own.push((mark, binder, domain));
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

                        lowered.items.push(FlatItem::Let(FlatLet {
                            span: label.span().cloned(),
                            kind: curios_core::DefinitionKind::ConceptMethod {
                                owner: context.prefixed(&concept.label),
                            },
                            name: curios_core::Global::Authored(
                                context.prefixed(&concept.label).with(label),
                            ),
                            island: context.island(),
                            type_,
                            body,
                        }));
                    }
                }
                lowered.items.push(match formers.len() {
                    1 => FlatItem::Let(formers.pop().expect("a `concept` item has a member")),
                    _ => FlatItem::Rec(formers),
                });
            }
            // A witness desugars to an anonymous top-level definition satisfy (tele) -> C(args) = C { f = e, … }; and marks it for registration in the program-wide witness table. It gets an *identity*, not a manufactured name: a `satisfy` block has no name a programmer wrote, and the module a diagnostic reports for it comes from `Definition::island`. The identity it is lowered under is its place in its module; the one it keeps is what its signature spells, read once every item is lowered (`name_witnesses`).
            // A group `satisfy … and …` lowers to one `rec` item, so its members' anonymous names are bound in one another; a lone witness stays a `let` item, and may still resolve through its own entry — that repair is elaboration's, since a witness references itself by resolution rather than by name.
            TopItem::Witness(group) => {
                context.sorts_stand_in(SortSite::Witness);
                let mut items = group
                    .iter()
                    .map(|witness| {
                        let name = curios_core::Global::Witness(context.fresh_witness());
                        context.record_import_scope(Some(&name));

                        let concept_app = witness_concept_application(
                            &witness.concept,
                            witness.args.as_deref(),
                            witness.head.as_ref(),
                        );
                        // A written body is the concept literal over its fields alone, so every `use`-marked position is left to resolution, under a bare head: the declared type states the arguments once, and the literal is checked against it. A derived one is the `Derive` transient, spanned at the concept application so a refusal lands on the declaration. Either way the telescope below wraps it identically.
                        let body: Term = match &witness.body {
                            Some(fields) => Subterm::StructLit(StructLit {
                                head: witness.concept.clone(),
                                params: None,
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
                                match witness
                                    .args
                                    .iter()
                                    .flatten()
                                    .next()
                                    .and_then(|arg| arg.term.span())
                                {
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
                            span: declared.clone(),
                            name,
                            island: context.island(),
                            // Without a telescope the type is the application itself, which carries this span already.
                            type_: lower.signature_type(&signature, declared.as_ref())?,
                            body: lower.signature_value(&signature, |body| lower.value(body))?,
                        };
                        lowered.witnesses.insert(name);
                        Ok(item)
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                lowered.items.push(match items.len() {
                    1 => FlatItem::Let(items.pop().expect("a `satisfy` item has a member")),
                    _ => FlatItem::Rec(items),
                });
            }
        }
        if declares && lowered.items.len() > lowered_from {
            context.end_declaration(
                lowered.items[lowered_from..]
                    .iter()
                    .flat_map(FlatItem::names),
            );
            lowered.note(context, lowered_from);
        }
    }

    Ok(())
}

/// The public bindings of each module some entry of `entries` imports: each label with what it resolves to, by label. A module's interface answers a public binding's target whoever asks (`visible_binding`), so the list is the module's and nothing of the scope that imported it.
fn module_members(
    entries: &[curios_core::Import],
    public: &Scoped<'_, PublicInterface>,
) -> BTreeMap<Qualifier, Vec<(String, curios_core::Global)>> {
    let mut members = BTreeMap::new();
    for import in entries {
        let curios_core::Import::Module { module, .. } = import else {
            continue;
        };
        if members.contains_key(module) {
            continue;
        }
        let Some(interface) = public.get(module) else {
            continue;
        };
        let mut bindings = interface
            .bindings
            .iter()
            .map(|(label, entry)| (label.clone(), curios_core::Global::Authored(entry.target)))
            .collect::<Vec<_>>();
        bindings.sort();
        members.insert(*module, bindings);
    }

    members
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
    pub fn reads(&self) -> Vec<(std::path::PathBuf, std::sync::Arc<Source>)> {
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

    /// The `foreign` declarations this unit's package says a module answers, each beside the manifest row that says so ([`RootSource::answering`]).
    pub fn answered(&self) -> &[(String, String)] {
        self.source.answered()
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
    pub fn entrypoint(&self) -> Option<&'a Entrypoint> {
        self.entrypoint
    }

    /// Whether this is the entry program: the unit that carries a final term and owns the empty prefix.
    pub fn is_entry(&self) -> bool {
        self.entrypoint.is_some()
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
    lower_unit(source, predecessors, syntax, None).map(|(unit, _)| unit)
}

/// [`into_core_unit`] over `baseline`, the unit an earlier text of the same sources lowered to: a declaration the baseline lowered from the text it is written in now, every name it resolved resolving as it did, is the baseline's and is not lowered again. The unit is the one lowering the whole of it gives.
pub fn into_core_unit_over(
    source: &UnitSource<'_>,
    predecessors: &[&PreparedText],
    syntax: &SyntaxRegistry,
    baseline: &PreparedText,
) -> Result<PreparedText, Error> {
    lower_unit(source, predecessors, syntax, Some(baseline)).map(|(unit, _)| unit)
}

/// Lower the entry program `source` is, against the units already lowered and over `baseline` where there is one: its declarations as the unit they are ([`into_core_unit`], [`into_core_unit_over`]), and the term it closes with beside them.
///
/// The term is lowered on every compile: it is no declaration, and nothing of a unit holds it.
pub fn into_core_entry(
    source: &UnitSource<'_>,
    predecessors: &[&PreparedText],
    syntax: &SyntaxRegistry,
    baseline: Option<&PreparedText>,
) -> Result<(PreparedText, curios_core::Entrypoint), Error> {
    let (unit, entry) = lower_unit(source, predecessors, syntax, baseline)?;

    Ok((
        unit,
        entry.expect("an entry source's parse holds its final term"),
    ))
}

/// [`into_core_unit`], with the final term an entry source closes with, lowered beside the unit.
fn lower_unit(
    source: &UnitSource<'_>,
    predecessors: &[&PreparedText],
    syntax: &SyntaxRegistry,
    baseline: Option<&PreparedText>,
) -> Result<(PreparedText, Option<curios_core::Entrypoint>), Error> {
    curios_profile::profile!("into_core_unit");
    grown(|| into_core_unit_within(source, predecessors, syntax, baseline))
}

fn into_core_unit_within(
    source: &UnitSource<'_>,
    predecessors: &[&PreparedText],
    syntax: &SyntaxRegistry,
    baseline: Option<&PreparedText>,
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

    // What the entry's own items declare: the compilation root's, and none for a unit with no entrypoint.
    let root = source
        .entrypoint()
        .map(|entrypoint| Headed::of(&entrypoint.module))
        .unwrap_or_default();
    // What the baseline kept of each file. A baseline restored from a store holds its half as bytes until this, the lowering's first read of it.
    let baseline_scans =
        curios_profile::profile!("into_core_unit::baseline" => baseline.map(PreparedText::scans));

    let Resolved {
        mut table,
        modules,
        mod_spans,
    } = Resolved::of(
        source,
        &root,
        baseline_scans,
        &predecessor_tables,
        &predecessor_mounts,
        &own,
    )?;

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
        &root,
        &own,
        &modules,
        &mut table,
        reach,
        Scoped::over(&predecessor_public),
    )?;

    let shared = Shared::new();
    let mut context = Context::new(&table, &public, reach, &shared, syntax);
    // Every named prefix in the compilation binds its own one-segment name. No two can repeat it: the disjointness check above refuses a unit claiming what a predecessor already holds, and the predecessors' own mounts were pairwise disjoint when each was compiled. The entry's prefix is the empty one, which has no name to bind.
    for mount in &mounts {
        if !mount.prefix.is_root() {
            context.insert_scope(mount.prefix.head().to_string(), None, mount.prefix)?;
        }
    }

    let baseline = baseline.map(Baseline::of);
    let mut lowered = Lowering {
        baseline: baseline.as_ref(),
        ..Lowering::default()
    };

    // The compilation root's own items — the entry's, and none for a unit with no entrypoint — then one pass per prefix this unit claims. Exactly one of the two does any work, because owning the empty prefix is what makes a unit the entry.
    if let Some(entrypoint) = source.entrypoint() {
        curios_profile::profile!("process_items" => process_items(
            &root,
            Body::Items(&entrypoint.module.items),
            &mut context,
            &mut lowered,
            &modules,
        ))?;
    }

    for mount in own.iter().filter(|mount| !mount.prefix.is_root()) {
        let content = modules
            .get(&mount.prefix)
            .expect("a mounted prefix was loaded during discovery");

        let mut nested = context.nested(mount.prefix.head());

        curios_profile::profile!("process_items" => process_items(
            &content.heads,
            Body::File(content),
            &mut nested,
            &mut lowered,
            &modules,
        ))?;
    }

    let Lowering {
        baseline: _,
        items: mut flat_items,
        declarations,
        induct_decls,
        struct_decls,
        concepts,
        mut witnesses,
        tests,
        foreigns,
        broken,
    } = lowered;
    // What each file declared, kept for the lowering that reads the same text again.
    let scans = modules
        .iter()
        .filter_map(|(module, loaded)| Some((*module, loaded.scan()?)))
        .collect::<BTreeMap<_, _>>();
    let facts = declarations
        .iter()
        .flat_map(|declaration| declaration.facts())
        .collect::<Vec<_>>();
    assert_eq!(
        facts.len(),
        flat_items.len(),
        "every lowered item belongs to a declaration"
    );

    // Every item is lowered, so every definition a witness's signature may name is at hand.
    name_witnesses(
        Named {
            items: &mut flat_items,
            witnesses: &mut witnesses,
            minted: &mut shared.minted.borrow_mut(),
            imports: &mut shared.imports.borrow_mut(),
            lints: &mut shared.lints.borrow_mut(),
        },
        &struct_decls,
        &concepts,
        predecessors,
    );

    // The entrypoint, for the one unit that has one. Its tail closes the root body, so the imports in scope there are the last the root saw.
    context.record_import_scope(None);
    // Every scope is recorded: the members of the modules they import are the interfaces' to state, once for the unit.
    {
        let mut imports = shared.imports.borrow_mut();
        let members = module_members(&imports.entries, &public);
        imports.members = members;
    }
    // The entry mints in a space of its own, as a declaration does, and is written in a text of its own.
    context.begin_declaration();
    if let Some(span) = source
        .entrypoint()
        .and_then(|entrypoint| entrypoint.tail.span())
    {
        context.anchor(curios_core::Anchored::Entry, span);
    }
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
    let entry_mints = context.mints();

    // Decided here, of the unit's own layers, and kept with the unit: the audit below and every report after read the one answer.
    let audiences = Audiences::of(table.own(), public.own());
    let in_scope = AudienceScope::over(&audiences, predecessors);
    debug_assert!(
        in_scope.widens_nothing(public.own()),
        "an export of another unit's name shows it to nobody its own unit does not"
    );
    // How a name is written by a reader standing at one of the unit's items: what a refusal of the audit spells the item and what it exposes by, as every report of the unit does once it is lowered ([`PreparedText::spellings`]). Made where the audit refuses, from the tables as they stand, and by no audit that passes.
    let spelled = |item: &Qualifier, name: &curios_core::Global| {
        let own_table = table
            .own()
            .iter()
            .map(|(module, info)| (*module, info.clone()));
        let own_public = public
            .own()
            .iter()
            .map(|(module, interface)| (*module, interface.clone()));
        let paths = writable_paths(&own_table.collect(), &own_public.collect(), &audiences);
        let spellings = spellings_over(
            &shared.imports.borrow(),
            Rc::new(paths),
            &own,
            &visible_mounts,
            predecessors,
        );
        let owner = curios_core::Global::Authored(*item);

        spellings
            .spell(&item.without_last(), Some(&owner), name)
            .unwrap_or_else(|| name.faithful().to_string())
    };
    audit_public_exposures(
        &public,
        in_scope,
        &flat_items,
        &facts,
        NominalScope::new(predecessors, &induct_decls, &struct_decls),
        &|extent| shared.anchoring.resolved(extent),
        &spelled,
    )?;

    let dead = unused_declarations(&Declarations {
        items: &flat_items,
        facts: &facts,
        placed: &|extent| shared.anchoring.resolved(extent),
        entry: entry.as_ref(),
        table: &table,
        public: &public,
        own: &own,
        mod_spans: &mod_spans,
    });

    // Where each definition is named, by the name it leaves the lowering under, which for a witness is the one its signature gave it above.
    let definitions = flat_items
        .iter()
        .zip(&facts)
        .flat_map(|(item, lowered)| item.declared().zip(&lowered.definitions));
    for ((name, _, _), declared) in definitions {
        if let Some(label) = declared.label {
            shared.anchoring.name(name, label);
        }
    }

    // This unit's own items alone. A predecessor reaches later stages as an *environment* they are seeded from — `Globals` at the certifier, a replayed context at elaboration and erasure — and copying its items into every compilation only ever existed so those stages could then skip them again by index. See `documentation/design/compilation/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md`.
    //
    // As they are written. No item is moved for another's sake: elaboration asks for each declaration when it is read, wherever it sits, refuses the ones that need one another by name, and stores the module in an order of its own (`curios_elab`'s `Demand`, `curios_core::Module::ordered`).
    let items = flat_items
        .into_iter()
        .map(FlatItem::into_core)
        .collect::<Vec<_>>();

    curios_profile::sample!(
        "into_core_unit::imports",
        shared.imports.borrow().entries.len()
    );
    let mut unit = PreparedText {
        mounts: own.clone(),
        foreigns,
        table: table.into_own().into_iter().collect(),
        public: public.into_own().into_iter().collect(),
        audiences: OnceCell::from(audiences),
        paths: OnceCell::new(),
        declared: declared_in(&items),
        items: None,
        carried: Vec::new(),
        core: curios_core::Module {
            items: items.into(),
            mounts: own,
            witnesses,
            tests,
        },
        minted: curios_core::Minted {
            entry: entry_mints,
            ..shared.minted.into_inner()
        },
        imports: curios_archive::Deferred::held(shared.imports.into_inner()),
        visible: visible_mounts,
        broken,
        lints: ordered(
            unused_imports(shared.sites.into_inner())
                .into_iter()
                .chain(shared.lints.into_inner())
                .chain(dead)
                .collect(),
        ),
        credited: BTreeSet::new(),
        reached: shared.reached.into_inner(),
        anchors: curios_archive::Deferred::held(shared.anchoring.into_anchors()),
        baseline: curios_archive::Deferred::held(BaselineHalf {
            declarations,
            scans,
        }),
    };
    if let Some(baseline) = &baseline {
        unit.carry(baseline.unit());
    }

    Ok((unit, entry))
}

/// Lower a whole [`Entrypoint`] with nothing in scope, as `curios-text`'s own stage tests do.
pub fn into_core(
    entrypoint: &Entrypoint,
    loader: &RootSource,
    syntax: &SyntaxRegistry,
) -> Result<(curios_core::Program, curios_core::Minted, ForeignStore), Error> {
    let (unit, entry) = lower_unit(&UnitSource::entry(entrypoint, loader), &[], syntax, None)?;

    Ok((
        curios_core::Program {
            module: unit.core,
            entry: entry.expect("an entry source's parse holds its final term"),
        },
        unit.minted,
        unit.foreigns,
    ))
}

/// The interface of the unit `unit` was lowered into from `source`, for its consumers, or `None` where the source marks no mount as documented: an entry program, which has no consumer.
///
/// **Built where a command asks for it, and carried by no unit.** Two commands read a record, so no compilation builds one and no image holds one. What a page shows is still read off the compilation that built the library: the direct interface, the export view and each definition's import scope are the ones `unit`'s lowering stored, and the surface tree is discovery's, read again through `source`, which parses a text once for each thread, so a command that lowered the unit a moment ago parses nothing twice.
///
/// `predecessors` are the units `unit` was lowered against, in that order. `records` are the records of the ones this unit adopts a root of — a root a consumer cannot name and this unit can, whose declarations reach a consumer only through it. An adopted root is another unit, read from another source, so its record is made by this door before its adopter's is.
///
/// # Panics
///
/// Where the unit adopts a root no record of `records` documents: its declarations would be missing from every page that exposes them, with nothing said.
pub fn document_unit(
    source: &UnitSource<'_>,
    unit: &PreparedText,
    predecessors: &[&PreparedText],
    records: &[&Documentation],
) -> Result<Option<Documentation>, Error> {
    let Some((prefix, description)) = source.documented() else {
        return Ok(None);
    };

    let predecessor_tables = predecessors
        .iter()
        .map(|earlier| &earlier.table)
        .collect::<Vec<_>>();
    let predecessor_mounts = predecessors
        .iter()
        .flat_map(|earlier| earlier.mounts.iter().cloned())
        .collect::<Vec<_>>();
    // The surface tree alone is taken of this: the tables are the ones the lowering stored, which the export view's fixed point wrote into after discovery had built them.
    let Resolved { modules, .. } = Resolved::of(
        source,
        &[],
        None,
        &predecessor_tables,
        &predecessor_mounts,
        &unit.mounts,
    )?;
    let modules = modules
        .into_iter()
        .map(|(module, loaded)| {
            let loaded = Rc::into_inner(loaded).expect("discovery hands its modules over whole");

            Ok((module, Rc::new(loaded.into_tree()?)))
        })
        .collect::<Result<HashMap<_, _>, Error>>()?;

    // The unit's own tables as the nearest base, where the lowering held them as the layer it wrote: every read answers alike, since mount sets are disjoint.
    let tables = predecessor_tables
        .iter()
        .copied()
        .chain([&unit.table])
        .collect::<Vec<_>>();
    let exports = predecessors
        .iter()
        .map(|earlier| &earlier.public)
        .chain([&unit.public])
        .collect::<Vec<_>>();

    Ok(Some(document(
        &modules,
        &Scoped::over(&tables),
        &Scoped::over(&exports),
        unit.imports(),
        &prefix,
        &unit.visible,
        records,
        description,
    )))
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

/// The entry program lowered: its module and the term it closes with, what its lowering minted, which elaboration's counters start above, its `foreign` rows, and its lints. A name that resolved to nothing is among what its declaration minted (`curios_core::Mints::unbound`).
pub struct LoweredEntry {
    pub program: curios_core::Program,
    pub minted: curios_core::Minted,
    pub foreigns: ForeignStore,
    /// How the entry's names can be written, where — see [`EntryNames`].
    pub names: EntryNames,
    /// See [`PreparedText::lints`].
    pub lints: Vec<Lint>,
    /// See [`PreparedText::broken`].
    pub broken: Vec<BrokenItem>,
    /// See [`PreparedText::reached`].
    pub reached: BTreeSet<Qualifier>,
    /// See [`PreparedText::anchors`].
    pub anchors: curios_core::Anchors,
}

/// How the entry's names can be written, where: what its `use` lines brought into scope, and what the paths a report spells a name by are made from — the entry's direct interface and export view with the audiences its lowering decided, the prefixes it claims and the ones it may name. An entry's lowering hands back no unit, so these are what a unit would have kept of them.
pub struct EntryNames {
    imports: curios_core::Imports,
    table: BTreeMap<Qualifier, ModuleInfo>,
    public: BTreeMap<Qualifier, PublicInterface>,
    audiences: OnceCell<Audiences>,
    mounts: Vec<Mount>,
    visible: Vec<Mount>,
}

impl EntryNames {
    /// See [`PreparedText::imports`].
    pub fn imports(&self) -> &curios_core::Imports {
        &self.imports
    }

    /// See [`PreparedText::spellings`].
    pub fn spellings(&self, predecessors: &[&PreparedText]) -> curios_core::Spellings {
        let audiences = self
            .audiences
            .get_or_init(|| Audiences::of(&self.table, &self.public));

        spellings_over(
            &self.imports,
            Rc::new(writable_paths(&self.table, &self.public, audiences)),
            &self.mounts,
            &self.visible,
            predecessors,
        )
    }
}

/// `imports` as a lowering recorded them, beside the paths each unit in scope gives a global — `predecessors`' and then `paths`, the asking unit's own — with the prefixes in scope that unit may not name, `own` being the ones it claims and `visible` the ones it may name: what [`PreparedText::spellings`] and [`EntryNames::spellings`] answer.
fn spellings_over(
    imports: &curios_core::Imports,
    paths: Rc<BTreeMap<curios_core::Global, Vec<curios_core::WritablePath>>>,
    own: &[Mount],
    visible: &[Mount],
    predecessors: &[&PreparedText],
) -> curios_core::Spellings {
    let mounted = predecessors
        .iter()
        .flat_map(|earlier| earlier.mounts.iter())
        .chain(own);
    // What `Reach::guard` refuses a reference into: a mounted prefix that is not one the unit may name.
    let closed = mounted
        .map(|mount| mount.prefix)
        .filter(|prefix| !prefix.is_root())
        .filter(|prefix| !visible.iter().any(|mount| mount.prefix == *prefix))
        .collect();
    let units = predecessors
        .iter()
        .map(|earlier| curios_core::UnitPaths {
            paths: earlier.paths(),
            own: false,
        })
        .chain([curios_core::UnitPaths { paths, own: true }])
        .collect();

    curios_core::Spellings {
        imports: imports.clone(),
        units,
        closed,
    }
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
    let (unit, entry) = lower_unit(
        &UnitSource::entry(entrypoint, loader),
        predecessors,
        syntax,
        None,
    )?;

    Ok(LoweredEntry {
        program: curios_core::Program {
            module: unit.core,
            entry: entry.expect("an entry source's parse holds its final term"),
        },
        minted: unit.minted,
        foreigns: unit.foreigns,
        names: EntryNames {
            imports: unit
                .imports
                .into_held()
                .expect("a unit lowered in this process holds its import scopes"),
            table: unit.table,
            public: unit.public,
            audiences: unit.audiences,
            mounts: unit.mounts,
            visible: unit.visible,
        },
        lints: unit.lints,
        reached: unit.reached,
        broken: unit.broken,
        anchors: unit
            .anchors
            .into_held()
            .expect("a unit lowered in this process holds its anchors"),
    })
}
