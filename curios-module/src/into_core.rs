//! Lowering one unit's surface modules to a [`curios_core::Definitions`], over the units already lowered.
//!
//! [`into_core_unit`] runs a unit through these passes, in order:
//!
//! 1. **Discovery** (`Resolved::of`): the prefixes the unit claims checked disjoint from the predecessors', then every module they declare read through its source, for what its items declare (`head`). A file a baseline read as the same text is not parsed: what it declares is what the baseline kept of it.
//! 2. **Interface resolution** (`interface::resolve_unit`): each module's declarations seeded into its interface, then every `pub use` resolved to a fixed point, before any body is lowered.
//! 3. **Lowering** (`process_items`): each item's names resolved against those interfaces, its sugar undone and each match written as its matrix (`match_compile`), then the entry's final term. A declaration a baseline lowered from the same text, every name it resolved resolving as it did, is taken from the baseline and not lowered (`lowered`), and a file is parsed where the first of its declarations is.
//! 4. **Witness names** (`witness`): each witness given the name its signature spells, which is read through the unit's definitions and so only once every item is lowered.
//! 5. **Audit** (`audit`, `lint`): what the public interface exposes, and the declarations nothing reaches.
//!
//! A unit's documentation record is no pass of these: [`document_unit`] builds it where a command asks for one, from the tables this lowering stored and the surface tree its sources parse to.

mod audit;
use audit::*;

mod lowered;
use lowered::*;
pub use lowered::{
    ArchivedBaselineHalf, ArchivedConsulted, ArchivedFound, ArchivedHalfEntry, ArchivedLookup,
    ArchivedLoweredDeclaration, ArchivedReached, BaselineHalf, Consulted, Found, HalfEntry, Lookup,
    LookupReaders, LoweredDeclaration, Reached,
};

mod context;
use context::*;
pub use context::{ArchivedChildInfo, ArchivedModuleInfo, ChildInfo, ModuleInfo};

mod lowerer;
use lowerer::*;

mod declaration;
use declaration::*;

mod interface;
use interface::*;
pub use interface::{ArchivedEntry, ArchivedPublicInterface, Audiences, Entry, PublicInterface};

mod lint;
use lint::*;

mod references;
use references::*;
pub use references::{
    AliasEdge, Aliases, ArchivedAliasEdge, ArchivedDeclared, ArchivedLoweredItem, Declared,
    LoweredItem,
};

mod scoped;
use scoped::*;

mod document;
use document::*;
pub use document::{Heading, heading, written};

mod head;
pub use head::*;

mod witness;
use witness::*;

mod mention;
pub use mention::*;

mod revision;
use revision::*;
pub use revision::{Bodies, Revision};

mod reachability;
pub use reachability::Reachability;
use reachability::*;

use {
    crate::{Compiled, Error, PackageRoot, RootSource, tree_of},
    curios_abi::ForeignStore,
    curios_archive::Bytes,
    curios_common::{
        Blob, Digest, Filed, Mount, Persistent, Qualifier, Recorded, Report, RootKind, Said,
        Source, SourceName, Span, SyntaxRegistry, grown,
    },
    curios_document::Documentation,
    curios_surface::{
        Apply, Argument, Entrypoint, GroupItem, Label, Module, Name, Subterm, Term, TopItem,
        UseGroup, spell,
    },
    std::{
        cell::{Cell, OnceCell, RefCell},
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        path::PathBuf,
        rc::Rc,
        sync::Arc,
    },
};

#[cfg(test)]
mod test_support;
#[cfg(test)]
mod tests;

/// Every prefix the compilation mounts, and which of them the unit being compiled may name.
///
/// **Two lists rather than one, because a prefix a unit cannot name still exists.** It is in the fold — elaboration and erasure are seeded from it — and it owns names whose spelling decides which unit declares them. So `mounts` stays complete for every question about *what a name is*, and `visible` answers the one question about *who may write it*. Collapsing them would either hide a mount from `Mount::owning`, which decides coherence, or make an undeclared prefix indistinguishable from a prefix nobody mounted, which is the diagnostic this type exists to keep.
///
/// Carried as one value because it is threaded through the whole of resolution: the walk that follows a `use` to its provider asks both questions at every hop, and two parallel parameters through ten functions drift.
#[derive(Clone, Copy)]
pub struct Reach<'a> {
    mounts: &'a [Mount],
    visible: &'a [Mount],
}

impl<'a> Reach<'a> {
    /// `mounts` is every prefix in the compilation; `visible` is the subset this unit declared, plus its own.
    pub fn over(mounts: &'a [Mount], visible: &'a [Mount]) -> Self {
        Self { mounts, visible }
    }

    /// Every prefix the compilation mounts — what owns a name, never what may write one.
    pub fn mounts(&self) -> &'a [Mount] {
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

/// Every absolute path a unit gives a global — each declaration of its own at its own path, each export at the path its module gives it, whichever unit declares what it exports — with who may write it. A function of the unit's own tables and the audiences it decided, so it is made once for the unit ([`PreparedText::paths`]): which paths a reader's unit may name, and which it reaches relatively, are the reader's, read where a report spells a name ([`spell()`]).
fn writable_paths(
    table: &Table<ModuleInfo>,
    public: &Table<PublicInterface>,
    audiences: &Audiences,
) -> curios_core::Paths {
    curios_common::profile!("writable_paths");
    let mut paths: BTreeMap<curios_core::Global, Vec<curios_core::WritablePath>> = BTreeMap::new();
    let mut record = |target: &Qualifier, path: Qualifier, audience: Vec<Qualifier>| {
        paths
            .entry(curios_core::Global::Authored(*target))
            .or_default()
            .push(curios_core::WritablePath { path, audience });
    };

    for (module, info) in table.iter() {
        for (label, _) in info.bindings() {
            let path = module.with(label);
            record(&path, path, audiences.binding(&path));
        }
    }
    for (module, interface) in public.iter() {
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

    let mut sorted = curios_core::Paths::new();
    for (global, mut written) in paths {
        in_reading_order(&global, &mut written);
        sorted.insert(global, Rc::new(written));
    }

    sorted
}

/// `earlier`, the paths of a unit whose audiences were revised into `audiences`, with the paths of each of `names` read again of `table` and `audiences`: what a unit lowered over that one holds, where its revision read again the audience and the exports of `names` alone ([`Audiences::revised`]). A path is a binding of a module or an export of one, so a module whose table and export view stand gives every name the paths it gave.
fn revised_paths(
    earlier: &curios_core::Paths,
    names: &BTreeSet<Qualifier>,
    table: &Table<ModuleInfo>,
    audiences: &Audiences,
) -> curios_core::Paths {
    let mut paths = earlier.clone();
    for name in names {
        let global = curios_core::Global::Authored(*name);
        let mut written = Vec::new();
        let module = name.without_last();
        if table
            .get(&module)
            .is_some_and(|info| info.get_binding(name.last()).is_some())
        {
            written.push(curios_core::WritablePath {
                path: *name,
                audience: audiences.binding(name),
            });
        }
        for (exporter, label) in audiences.exported_at(name) {
            let path = exporter.with(&label);
            if path != *name {
                written.push(curios_core::WritablePath {
                    path,
                    audience: audiences.module(&exporter),
                });
            }
        }
        match written.is_empty() {
            true => {
                paths.remove(&global);
            }
            false => {
                in_reading_order(&global, &mut written);
                paths.insert(global, Rc::new(written));
            }
        }
    }

    paths
}

/// Every absolute path a unit gives a global, made where a report first asks and kept ([`PreparedText::paths`]): whole, off the unit's tables and audiences ([`writable_paths`]), or as the paths of the unit it was lowered over revised at the names whose audience or exports its lowering read again ([`revised_paths`]).
///
/// **Shared with the units lowered over it.** A unit lowered over another holds that one's cell, so a report over any of them makes the earlier unit's paths once, kept where that unit is, and each later unit revises them: a session that reports at every keystroke over one baseline makes its paths once. A unit whose earlier one made no paths holds what that one would have revised from, with both units' names, so a cell holds at most one earlier cell, and lets it go once its own paths are made.
struct PathsCell {
    table: Table<ModuleInfo>,
    public: Table<PublicInterface>,
    audiences: Audiences,
    /// What these paths revise: `None` for paths made whole, and once they are made.
    earlier: RefCell<Option<Revising>>,
    made: OnceCell<Rc<curios_core::Paths>>,
}

/// What a unit's paths revise: the cell of the unit it was lowered over, and the names whose paths are read again.
struct Revising {
    cell: Rc<PathsCell>,
    names: Rc<BTreeSet<Qualifier>>,
}

impl PathsCell {
    /// The paths of a unit of `table`, `public` and `audiences`, made whole.
    fn whole(
        table: Table<ModuleInfo>,
        public: Table<PublicInterface>,
        audiences: Audiences,
    ) -> Self {
        Self {
            table,
            public,
            audiences,
            earlier: RefCell::new(None),
            made: OnceCell::new(),
        }
    }

    /// The paths of a unit of `table`, `public` and `audiences`, lowered over the unit whose cell `earlier` is, made as its paths revised at `names`.
    fn over(
        earlier: &Rc<PathsCell>,
        names: &BTreeSet<Qualifier>,
        table: Table<ModuleInfo>,
        public: Table<PublicInterface>,
        audiences: Audiences,
    ) -> Self {
        // An earlier cell that made nothing and revises another is passed over, its names taken with these.
        let link = match (earlier.made.get(), &*earlier.earlier.borrow()) {
            (None, Some(before)) => Revising {
                cell: Rc::clone(&before.cell),
                names: Rc::new(before.names.union(names).copied().collect()),
            },
            _ => Revising {
                cell: Rc::clone(earlier),
                names: Rc::new(names.clone()),
            },
        };

        Self {
            earlier: RefCell::new(Some(link)),
            ..Self::whole(table, public, audiences)
        }
    }

    /// The paths, made now where nothing asked before.
    fn paths(&self) -> Rc<curios_core::Paths> {
        let made = self.made.get_or_init(|| {
            let earlier = self.earlier.borrow_mut().take();
            Rc::new(match earlier {
                Some(Revising { cell, names }) => {
                    revised_paths(&cell.paths(), &names, &self.table, &self.audiences)
                }
                None => writable_paths(&self.table, &self.public, &self.audiences),
            })
        });

        Rc::clone(made)
    }
}

/// What a report spells a unit's own names by ([`curios_core::Lexicon`]), made where a report first asks and kept: whole, off the unit's lowered module, or as the lexicon of the unit it was lowered over revised at the names its lowering made or dropped ([`curios_core::Definitions::lexicon_over`]).
///
/// **Shared with the units lowered over it**, as a unit's paths are ([`PathsCell`]), so a session that reports at every keystroke over one baseline reads that baseline's declarations once. A cell holds the unit's items as the version they are, which shares them; a unit whose items are a vector of their own is read where its cell is made, since copying them would read them all.
struct LexiconCell {
    module: curios_core::Definitions,
    places: curios_core::Places,
    /// What this lexicon revises: `None` for one made whole, and once it is made.
    earlier: RefCell<Option<LexiconRevising>>,
    made: OnceCell<Rc<curios_core::Lexicon>>,
}

/// What a unit's lexicon revises: the cell of the unit it was lowered over, and the names whose entries are read again.
struct LexiconRevising {
    cell: Rc<LexiconCell>,
    names: Rc<BTreeSet<curios_core::Global>>,
}

impl LexiconCell {
    /// The lexicon of `unit`, made whole: now where its items are a vector of their own, and where a report asks otherwise.
    fn whole(unit: &PreparedText) -> Self {
        let items = &unit.core().items;
        if !items.is_shared() {
            return Self {
                module: curios_core::Definitions::default(),
                places: curios_core::Places::default(),
                earlier: RefCell::new(None),
                made: OnceCell::from(Rc::new(unit.core().lexicon())),
            };
        }

        Self {
            module: curios_core::Definitions {
                items: items.clone(),
            },
            places: unit.places().clone(),
            earlier: RefCell::new(None),
            made: OnceCell::new(),
        }
    }

    /// The lexicon of a unit of `module`, whose names `places` holds, lowered over the unit whose cell `earlier` is: its lexicon revised at `names`.
    fn over(
        earlier: &Rc<LexiconCell>,
        names: BTreeSet<curios_core::Global>,
        definitions: curios_core::Definitions,
        places: curios_core::Places,
    ) -> Self {
        // An earlier cell that made nothing and revises another is passed over, its names taken with these.
        let link = match (earlier.made.get(), &*earlier.earlier.borrow()) {
            (None, Some(before)) => LexiconRevising {
                cell: Rc::clone(&before.cell),
                names: Rc::new(before.names.union(&names).copied().collect()),
            },
            _ => LexiconRevising {
                cell: Rc::clone(earlier),
                names: Rc::new(names),
            },
        };

        Self {
            module: definitions,
            places,
            earlier: RefCell::new(Some(link)),
            made: OnceCell::new(),
        }
    }

    /// The lexicon, made now where nothing asked before.
    fn lexicon(&self) -> Rc<curios_core::Lexicon> {
        let made = self.made.get_or_init(|| {
            let earlier = self.earlier.borrow_mut().take();
            Rc::new(match earlier {
                Some(LexiconRevising { cell, names }) => {
                    self.module
                        .lexicon_over(&cell.lexicon(), &self.places, &names)
                }
                None => self.module.lexicon(),
            })
        });

        Rc::clone(made)
    }
}

/// `written`, the paths of `global`, put in the order a reader takes them: its own path, then each path it is exported at, by path. A module's bindings are walked in a hash's order, which differs from one compilation to the next, and a reader takes the first of two spellings equally short.
fn in_reading_order(global: &curios_core::Global, written: &mut [curios_core::WritablePath]) {
    written.sort_by(|left, right| {
        let exported = |path: &Qualifier| curios_core::Global::Authored(*path) != *global;

        (exported(&left.path), &left.path).cmp(&(exported(&right.path), &right.path))
    });
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
pub struct Loaded {
    module: Qualifier,
    /// What the module's items declare: read off its tree, or a baseline's scan of the same text.
    heads: Rc<Vec<Headed>>,
    /// The text the module was read from. `None` for a module supplied already parsed.
    source: Option<Arc<Source>>,
    /// The module's tree: in hand where the file was parsed for its heads, and otherwise parsed when a declaration of it is first lowered.
    tree: OnceCell<Module>,
    /// The baseline's scan of the very text, where it stood in for parsing: what the unit keeps of the file again, with the digest it is stored under.
    kept: Option<HeldScan>,
}

impl Loaded {
    /// A module whose tree is in hand.
    fn parsed(module: Qualifier, tree: Module, source: Option<Arc<Source>>) -> Self {
        Self {
            module,
            heads: Rc::new(Headed::of(&tree)),
            source,
            tree: OnceCell::from(tree),
            kept: None,
        }
    }

    /// The module `module` of `source`, read without being parsed where `scans` hold what the very text declared.
    fn of(
        source: &UnitSource<'_>,
        scans: Option<&Scans>,
        module: &Qualifier,
    ) -> Result<Self, Error> {
        let Some(text) = source.source.read(module)? else {
            return Ok(Self::parsed(*module, source.source.load(module)?, None));
        };

        if let Some(scan) = scans.and_then(|scans| scans.get(module))
            && scan.source.text() == text.text()
        {
            // A scan this thread made lies in the very source the file was read as, and is shared. One restored from a cabinet lies in a source of its own, so its heads are placed in the one just read.
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
                kept: Some(Rc::clone(scan)),
            });
        }

        let tree = tree_of(&text).map_err(|cause| Error::ModuleLoadFailed {
            label: module.join(),
            cause: Box::new(cause),
        })?;

        Ok(Self::parsed(*module, tree, Some(text)))
    }

    /// Whether the module's text is other than the one a baseline's scan holds: parsed already, where its scan would have stood in for it.
    fn touched(&self) -> bool {
        self.tree.get().is_some() || self.source.is_none()
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

    /// What the unit keeps of the module's file for a later lowering: its heads, in the text they were read from, as the baseline kept them where its scan of the very text stood in for parsing. `None` for a module supplied already parsed.
    fn scan(&self) -> Option<HeldScan> {
        if let Some(kept) = &self.kept {
            return Some(Rc::clone(kept));
        }

        Some(Rc::new(Filed::new(Scan {
            source: Arc::clone(self.source.as_ref()?),
            heads: Rc::clone(&self.heads),
        })))
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
                TopItem::Mod(curios_surface::TopMod {
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
    /// The modules whose direct interface was read off their text now: every one, where no baseline's was carried, and otherwise each whose text the edit touched or the baseline does not hold, with the compilation root.
    touched: HashSet<Qualifier>,
    /// What the carried table held that the unit no longer declares: modules taken away with every one under them, and the namespaces of the declarations of a touched module, which are read off its text again.
    removed: Vec<Qualifier>,
    /// Each module the unit declares, by where the walk over the module tree meets it.
    order: HashMap<Qualifier, usize>,
}

/// One unit's text-stage state — its resolution tables, its lowered module and what the lowering recorded beside it — opaque outside this crate and carried on the unit for the units compiled after it.
#[derive(Clone)]
#[curios_archive::archived]
pub struct PreparedText {
    mounts: Vec<Mount>,
    /// What the unit was lowered for ([`UnitSource::compiled`]). A lowering for the other over it enters every module, since a module passed unentered is the baseline's whole, its tests with it or none of them.
    compiled: Compiled,
    /// The `foreign` rows this unit declares. Collected by the same walk that lowers its items, and carried here rather than dropped: a unit that declares one has to reach the link, and the prelude declaring none is a fact about the prelude, not about the shape.
    foreigns: ForeignStore,
    #[archived_with(curios_common::InSharedKeyOrder)]
    table: Table<ModuleInfo>,
    #[archived_with(curios_common::InSharedKeyOrder)]
    public: Table<PublicInterface>,
    /// Who can see each declaration of this unit: decided where it was lowered, or the first time something asks of a restored one, and kept. Never stored, being read off `table` and `public`.
    #[archived_with(curios_archive::Skip)]
    audiences: OnceCell<Audiences>,
    /// The unit's aliases, made the first time an audit asks and kept: a unit lowered over this one that changes no alias holds the same. Never stored, being read off the records.
    #[archived_with(curios_archive::Skip)]
    aliases: OnceCell<Rc<Aliases>>,
    /// Every absolute path this unit gives a global, made the first time a report spells a name over it or a unit lowered over it and kept — see [`PathsCell`]. Never stored, being read off `table`, `public` and `audiences`.
    #[archived_with(curios_archive::Skip)]
    paths: OnceCell<Rc<PathsCell>>,
    /// What a report spells this unit's own names by, made the first time a report over it or a unit lowered over it asks and kept — see [`LexiconCell`]. Never stored, being read off the lowered module.
    #[archived_with(curios_archive::Skip)]
    lexicon: OnceCell<Rc<LexiconCell>>,
    /// The lowered module: what its elaboration reads, each item in hand where the unit was lowered in this process and read out of the bytes it was stored as where it was restored.
    ///
    /// **Stored without its items.** What every unit lowered after this one reads of it is what each item declares and the registry entries its lowering wrote, so that is what a unit is restored with; an item is read by a later lowering of the same sources, and by a unit whose witness names its declaration ([`PreparedText::written`]), so each is stored on its own, beside the unit, and read by whoever asks for it ([`PreparedText::lowered_item`]).
    #[archived_with(curios_core::WithoutItems)]
    core: curios_core::Definitions,
    /// Where the unit's lowered items hold each name it declares, made the first time something asks: what finds a declaration's lowered form by its name, and says what it is with nothing read.
    #[archived_with(curios_archive::Skip)]
    places: OnceCell<curios_core::Places>,
    /// The declarations the unit's lowered items make, made the first time something asks: what its elaboration asks a declaration for by name.
    #[archived_with(curios_archive::Skip)]
    declarations: OnceCell<curios_core::Declarations>,
    /// What this unit, lowered over a baseline, holds otherwise than the baseline does ([`PreparedText::lowered_anew`], [`PreparedText::dropped`]). `None` for a unit lowered over none, and for one restored.
    #[archived_with(curios_archive::Skip)]
    revision: Option<Revision>,
    /// What this unit's lowering minted, which its elaboration's counters start above, with the names of each declaration that resolved to nothing. A count within the unit: nothing here resumes above a predecessor. Shared with the elaboration that seeds from it.
    #[archived_with(curios_archive::Shared)]
    minted: Rc<curios_core::Minted>,
    /// Every binding a `use` brought into scope, with the spelling a reader wrote it under and, per definition, the ones in scope where it was written — see `Context::imports`. What a goal report's candidate pool reaches beyond the names the program already mentions, and half of what a report spells a name by ([`PreparedText::spellings`]).
    ///
    /// **Stored apart from the unit**, each body's imports and each module's offer a blob ([`PreparedText::imports_recorded`]): they are read by the unit's own reports and its documentation, and by nothing compiled after it.
    #[archived_with(curios_archive::Skip)]
    imports: Recorded<curios_core::Imports>,
    /// The mounts this unit may name: the ones it declared a dependency on, or every open one in scope where it declared none, and its own. What its references were guarded by, and what drops a path it may not write from the table a report spells by.
    visible: Vec<Mount>,
    /// Every lint resolution alone decides — an import nothing resolved through, a declaration nothing reaches — worded here: what the unit's lints are gathered from where the unit is made, beside the binder lints elaboration decides from the sites each declaration's mints hold ([`curios_core::Mints::unread`]). Held by a unit lowered in this process alone: a unit stores the lints gathered of it, and a lowering over it reads its baseline half's.
    #[archived_with(curios_archive::Skip)]
    lints: Vec<curios_core::Lint>,
    /// The prefix of every mount some reference of this unit was *written* under — see `Context::note_written`.
    reached: BTreeSet<Qualifier>,
    /// Every item the parser could not read, in reading order — see [`BrokenItem`]. Empty for any unit that compiles.
    broken: Vec<BrokenItem>,
    /// Where each source declaration this unit was lowered from lies: what a position in one of its terms is resolved against where a report is made — see `curios_core::Anchors`.
    ///
    /// **Stored apart from the unit**, each source's texts a blob ([`PreparedText::anchors_recorded`]): a report placed in one of the unit's declarations reads them, and a lowering over the unit, and no compilation that reports nothing.
    #[archived_with(curios_archive::Skip)]
    anchors: Recorded<curios_core::Anchors>,
    /// The packages the unit was restored for, each standing where this machine holds it: what each source of its stored texts and of its files' scans is given its file by where they are restored ([`PreparedText::read_for`]). None for a unit lowered here, whose sources hold their files, and for one restored for no source, the prelude's image among them, whose sources hold their names alone.
    #[archived_with(curios_archive::Skip)]
    packages: Vec<PackageRoot>,
    /// What each source declaration lowered to beside its items, with what it asked of its scope, held by pointer since a unit lowered over this one keeps the ones that stand, and what each of the unit's files declared — see `lowered`.
    ///
    /// **The unit's baseline half: what a later lowering of the same sources reads, and nothing compiled after this unit does.** So it is no part of what the unit is stored as for its successors. It is stored beside that, each record and each file's scan a blob ([`PreparedText::baseline_digests`]), handed back where the unit is restored ([`PreparedText::with_baseline`]), and read when a lowering first asks.
    #[archived_with(curios_archive::Skip)]
    baseline: Recorded<BaselineHalf>,
    /// Who read each binding, of the unit's lowered declarations read backwards: made the first time a lowering over the unit asks, or handed over by the lowering that made the unit over a baseline ([`LookupReaders`]).
    #[archived_with(curios_archive::Skip)]
    lookups: OnceCell<LookupReaders>,
    /// Each module body's declarations, by the place of each one's first item, made the first time a lowering over the unit asks, or handed over by the lowering that made the unit over a baseline ([`Bodies`]).
    #[archived_with(curios_archive::Skip)]
    bodies: OnceCell<Bodies>,
    /// The declarations a later lowering may keep, by the text each was lowered from, made the first time a lowering over the unit asks, or handed over by the lowering that made the unit over a baseline.
    #[archived_with(curios_archive::Skip)]
    by_text: OnceCell<Persistent<Key, i64>>,
    /// What the `unused-declaration` lint's roots reach of the unit's items, made the first time a lowering over the unit asks, or handed over by the lowering that made the unit over a baseline ([`Reachability`]).
    #[archived_with(curios_archive::Skip)]
    reachability: OnceCell<Rc<Reachability>>,
    /// How many module bodies import each module, made the first time a lowering over the unit asks, or handed over by the lowering that made the unit over a baseline: what says whether a module is still offered once a body stops importing it.
    #[archived_with(curios_archive::Skip)]
    importers: OnceCell<Persistent<Qualifier, usize>>,
    /// What the entry's terms name, each a root of the lint, where the unit was lowered in this process: nothing for a unit with no entry, and `None` for one restored, whose entry no unit holds.
    #[archived_with(curios_archive::Skip)]
    entry_roots: Option<Rc<BTreeSet<curios_core::Global>>>,
}

/// An item the parser could not read, as the lowering carries it: the parser's report, and the name its head declared, registered so a reference to it resolves to this name — which the elaborator withholds, as it withholds every dependent of a refusal — rather than reporting as unbound. `None` where the head did not parse as far as a name, whose references then report as the unbound names they are.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct BrokenItem {
    pub declares: Option<curios_core::Global>,
    pub report: Report,
}

/// What a module of a unit exposes, read off the unit's lowering ([`PreparedText::exposure`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Exposure {
    /// Whether the `mod` line declaring the module is written `pub`: none for a root, which no line declares.
    pub public: Option<bool>,
    /// Whether every `mod` line down to the module is written `pub`: whether whoever may name its unit reaches it, and so whether a documentation page of the unit shows it, where the unit is documented.
    pub reached: bool,
    /// Each name the module's export view holds with the declaration it names, in the order the module's body writes them — its own declaration where its name stands, and one a `use` exposes where the line names it — and what a glob exposed, which no line names, after them by name.
    pub names: Vec<(String, curios_core::Global)>,
    /// The witnesses the module declares, in the order they are written.
    pub witnesses: Vec<curios_core::Global>,
    /// The child modules its `mod` lines declare `pub`, in the order the lines stand.
    pub children: Vec<Qualifier>,
}

impl PreparedText {
    /// See the field.
    /// See the field.
    pub fn lints(&self) -> &[curios_core::Lint] {
        &self.lints
    }

    /// Where each source declaration of this unit lies, each source holding the file its name resolves to through the packages the unit was restored for. See the field.
    pub fn anchors(&self) -> &curios_core::Anchors {
        self.anchors
            .get_with(|table, entries| {
                curios_core::Anchors::restored_filed(table, entries, &|name| self.file_of(name))
            })
            .unwrap_or_else(|error| panic!("a unit's anchors failed to restore: {error}"))
    }

    /// The file `name` resolves to through the packages the unit was restored for, where it resolves: what each source this unit stores is given where it is restored.
    fn file_of(&self, name: &SourceName) -> Option<PathBuf> {
        self.packages
            .iter()
            .find_map(|package| package.file_of(name))
    }

    /// This unit, restored for a source whose files are read for `packages`: where its declarations lie and what each of its files declared are read again from what it was restored from, each source holding the file its name resolves to on this machine. A unit lowered here holds its sources' files already, and is handed back as it is.
    pub fn read_for(self, packages: Vec<PackageRoot>) -> Self {
        let anchors = match self.anchors.as_stored() {
            Some((table, entries)) => Recorded::stored(table.clone(), entries.to_vec()),
            None => return self,
        };
        let baseline = match self.baseline.as_stored() {
            Some((table, entries)) => Recorded::stored(table.clone(), entries.to_vec()),
            None => self.baseline.clone(),
        };

        Self {
            anchors,
            baseline,
            packages,
            ..self
        }
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

    /// The prefixes this unit claims, and whether each is a root only the compiler supplies: the unit's own, never a definition's, so they are held here beside its modules.
    pub fn mounts(&self) -> &[Mount] {
        &self.mounts
    }

    /// The lowered module: what its elaboration reads, each item of a restored unit read out of its bytes where it is first asked for.
    pub fn core(&self) -> &curios_core::Definitions {
        &self.core
    }

    /// The item at `place` of the ones this unit lowered to, read out of its bytes now where the unit was restored and nothing has read it before.
    pub(crate) fn lowered_item(&self, place: usize) -> &curios_core::Item {
        &self.core.items[place]
    }

    /// Where the unit's lowered items hold each name it declares. See the field.
    pub fn places(&self) -> &curios_core::Places {
        self.places
            .get_or_init(|| curios_core::Places::of(&self.core))
    }

    /// What `name` is declared as at `place`, with nothing read.
    fn declared_at(
        &self,
        name: &curios_core::Global,
        place: curios_core::Place,
    ) -> curios_core::Declared {
        self.core.items.declared_at(name, place)
    }

    /// The entry this unit's lowering wrote for the nominal declaration `name`, read at the former that carries it: the one lowered item, read out of its bytes now where the unit was restored and nothing has read it before. `None` for a name the unit declares no nominal under.
    pub(crate) fn nominal(&self, name: &curios_core::Global) -> Option<&curios_core::Nominal> {
        let place = self.places().place(name)?;
        if !self.declared_at(name, place).is_former() {
            return None;
        }

        self.lowered_item(place.item())
            .nominals()
            .into_iter()
            .find_map(|(held, nominal)| (held == name).then_some(nominal))
    }

    /// What this unit declares `name` as, with nothing read: a definition's kind, or a concept's method, which its concept declares and no definition holds. `None` for a name the unit does not declare.
    pub fn declared_as(&self, name: &curios_core::Global) -> Option<curios_core::Declared> {
        if let Some(place) = self.places().place(name) {
            return Some(self.declared_at(name, place));
        }
        let curios_core::Global::Authored(target) = name else {
            return None;
        };

        concept_method(target, |module| self.table.get(module).map(|info| &**info))
            .map(|_| curios_core::Declared::Method)
    }

    /// Where the `mod` line declaring `module` names it, where this unit's body holding the line wrote one: what a module answers it is declared at.
    pub fn module_place(&self, module: &Qualifier) -> Option<curios_core::Extent> {
        self.imports()
            .lines(&module.without_last())
            .iter()
            .find(|line| {
                line.role == curios_core::LineRole::Declares
                    && line.names == curios_core::LineNamed::Module(*module)
            })
            .map(|line| line.at)
    }

    /// What the module `module` exposes, as this unit's lowering holds it ([`Exposure`]): the one reading of a module no documentation page shows, a private one, one beneath a private one, or a program's own. `None` for a path that names no module of this unit: a family's or a concept's namespace is its declaration's, and a root of another unit is that unit's.
    pub fn exposure(&self, module: &Qualifier) -> Option<Exposure> {
        let info = self.table.get(module)?;
        let public = match self.mounts.iter().any(|mount| mount.prefix == *module) {
            true => None,
            false => match self
                .table
                .get(&module.without_last())?
                .declares(module.last(), false)?
            {
                Declares::Child(ChildInfo::Ordinary { vis_pub }) => Some(vis_pub),
                Declares::Child(_) | Declares::Binding(_) => return None,
            },
        };
        let anchors = [self.anchors()];
        let lines = self.imports().lines(module);
        // Where the body writes the name it exposes `target` under: its own declaration's name, or the first name of a line that names the declaration. None for what a glob exposed, which no line names.
        let written = |target: &Qualifier| {
            let name = curios_core::Global::Authored(*target);

            match target.without_last() == *module {
                true => self.anchors().written(&name).map(|(_, label)| label.start),
                false => lines
                    .iter()
                    .filter(|line| line.names == curios_core::LineNamed::Declaration(name))
                    .filter_map(|line| line.at.resolved(&anchors))
                    .map(|span| span.start)
                    .min(),
            }
        };

        let mut names = self
            .public
            .get(module)
            .into_iter()
            .flat_map(|interface| interface.bindings.iter())
            .map(|(label, entry)| (written(&entry.target), label.clone(), entry.target))
            .collect::<Vec<_>>();
        names.sort_by(|(one, label, _), (other, against, _)| {
            (one.is_none(), one, label).cmp(&(other.is_none(), other, against))
        });
        let mut children = lines
            .iter()
            .filter(|line| line.role == curios_core::LineRole::Declares)
            .filter_map(|line| match line.names {
                curios_core::LineNamed::Module(child)
                    if info.get_child(child.last()) == Some(true) =>
                {
                    Some((line.at.resolved(&anchors).map(|span| span.start), child))
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        children.sort();
        let mut witnesses = self
            .places()
            .witnesses()
            .filter(|witness| match witness {
                curios_core::Global::Witness(id) => id.module() == module,
                curios_core::Global::Authored(_) => false,
            })
            .map(|witness| {
                let written = self
                    .anchors()
                    .written(witness)
                    .map(|(_, label)| label.start);

                (written, *witness)
            })
            .collect::<Vec<_>>();
        witnesses.sort();

        Some(Exposure {
            public,
            reached: reached_from(&Qualifier::empty(), module, |module| {
                self.table.get(module).map(|info| &**info)
            }),
            witnesses: witnesses.into_iter().map(|(_, witness)| witness).collect(),
            names: names
                .into_iter()
                .map(|(_, label, target)| (label, curios_core::Global::Authored(target)))
                .collect(),
            children: children.into_iter().map(|(_, child)| child).collect(),
        })
    }

    /// The declaration the absolute `path` names for a reader at the root, outside every module, over `predecessors` — the units this one was lowered over — and this unit: the walk a written path takes (`visible_path`), so a path some module exports a declaration at names that declaration. `None` where the path names nothing such a reader sees.
    pub fn named_by(&self, predecessors: &[&PreparedText], path: &Qualifier) -> Option<Qualifier> {
        let tables = predecessors
            .iter()
            .map(|earlier| &earlier.table)
            .chain([&self.table])
            .collect::<Vec<_>>();
        let exports = predecessors
            .iter()
            .map(|earlier| &earlier.public)
            .chain([&self.public])
            .collect::<Vec<_>>();
        let segments = path.iter().collect::<Vec<_>>();
        let root = Qualifier::empty();

        visible_path(
            &Scoped::over(&exports),
            &Scoped::over(&tables),
            &root,
            root,
            &segments,
        )
    }

    /// Why the absolute `path` names nothing for the reader [`PreparedText::named_by`] asks for: the resolver's own refusal of the step the walk stops at, a module or a name, with the nearest name a reader may write there, as the resolver's own `unresolved` says it. `None` where the path names something such a reader sees.
    pub fn unnamed_by(
        &self,
        predecessors: &[&PreparedText],
        path: &Qualifier,
    ) -> Option<curios_core::Unresolved> {
        let tables = predecessors
            .iter()
            .map(|earlier| &earlier.table)
            .chain([&self.table])
            .collect::<Vec<_>>();
        let exports = predecessors
            .iter()
            .map(|earlier| &earlier.public)
            .chain([&self.public])
            .collect::<Vec<_>>();
        let (public, table) = (Scoped::over(&exports), Scoped::over(&tables));
        let segments = path.iter().collect::<Vec<_>>();
        let (last, parents) = segments.split_last()?;
        let root = Qualifier::empty();

        let mut current = root;
        for segment in parents {
            match visible_child(&public, &table, &root, &current, segment) {
                Some(child) => current = child,
                None => {
                    return Some(unresolved(
                        &public,
                        &table,
                        &root,
                        &current,
                        segment,
                        curios_core::Sought::Module,
                    ));
                }
            }
        }
        // The last step names a declaration or a module, which a question takes either of.
        match visible_binding(&public, &table, &root, &current, last)
            .or_else(|| visible_child(&public, &table, &root, &current, last))
        {
            Some(_) => None,
            None => Some(unresolved(
                &public,
                &table,
                &root,
                &current,
                last,
                curios_core::Sought::Either,
            )),
        }
    }

    /// Whether `name` is written `pub` where this unit declares it, as a binding of its module. `None` for a name no module of the unit binds.
    pub fn is_public(&self, name: &Qualifier) -> Option<bool> {
        self.table
            .get(&name.without_last())?
            .get_binding(name.last())
    }

    /// The item this unit's lowering declares `name` by, read out of its bytes now where the unit was restored and nothing has read it before. `None` for a name the unit does not declare.
    pub fn lowered(&self, name: &curios_core::Global) -> Option<&curios_core::Item> {
        self.places()
            .place(name)
            .map(|place| self.lowered_item(place.item()))
    }

    /// How this unit declares `name`, as a witness's key reads it: its kind, and its lowered body where it is a definition written on its own. `None` for a name the unit does not declare.
    ///
    /// **Nothing is read to say what kind a name is, and one item to say how a `let` is written.** A key is read through the definitions a signature names, which are few, and most of them type formers, whose kind is the whole answer.
    pub(crate) fn written(
        &self,
        name: &curios_core::Global,
    ) -> Option<(curios_core::Declared, Option<&curios_core::Term>)> {
        let place = self.places().place(name)?;
        let declared = self.declared_at(name, place);
        let body = match (declared, place) {
            (declared, curios_core::Place::Let(_)) if declared.is_defined() => {
                match self.lowered_item(place.item()) {
                    curios_core::Item::Let(definition) => Some(&definition.body),
                    curios_core::Item::Rec(_) | curios_core::Item::Nominal(_) => None,
                }
            }
            _ => None,
        };

        Some((declared, body))
    }

    /// The `foreign` rows this unit declares.
    pub fn foreigns(&self) -> &ForeignStore {
        &self.foreigns
    }

    /// What this unit's lowering minted — see the field.
    pub fn minted(&self) -> &Rc<curios_core::Minted> {
        &self.minted
    }

    /// What each source declaration of this unit lowered to beside its items, by the place of its first item — see the field. Read out of the unit's baseline half now, where the unit was restored and no lowering has asked before.
    fn lowerings(&self) -> &Records {
        &self.half().declarations
    }

    /// What each file of this unit declared, by the module it backs — see [`Scan`].
    fn scans(&self) -> &Scans {
        &self.half().scans
    }

    /// Who read each binding, of the unit's lowered declarations read backwards. See the field.
    fn lookup_readers(&self) -> &LookupReaders {
        self.lookups.get_or_init(|| {
            LookupReaders::of(self.lowerings(), &self.half().uses, &self.half().keyed)
        })
    }

    /// The unit's baseline half, read out of its bytes now where the unit was restored and nothing has asked before, each file's scan lying in a source holding the file its name resolves to as the anchors' sources do.
    fn half(&self) -> &BaselineHalf {
        self.baseline
            .get_with(|table, entries| {
                BaselineHalf::restored_filed(table, entries, &|name| self.file_of(name))
            })
            .unwrap_or_else(|error| panic!("a unit's baseline half failed to restore: {error}"))
    }

    /// What this unit's baseline half was restored from, where it was: its own part and the blob of each entry, what it is stored as again.
    pub fn baseline_stored(&self) -> Option<(&Bytes, &[Blob])> {
        self.baseline.as_stored()
    }

    /// The bytes of this unit's baseline half's own part, stored apart from what the unit is for its successors: the tables of each module body and each witness, each record and scan being a blob of its own.
    pub fn baseline_table(&self) -> Result<Vec<u8>, String> {
        curios_archive::to_bytes(self.half()).map(|bytes| bytes.to_vec())
    }

    /// The digest of each record and each scan of this unit's baseline half, in the order the unit names them: what a later lowering of the same sources keeps this unit's lowerings by, each a blob of its own.
    pub fn baseline_digests(&self) -> Result<Vec<Digest>, String> {
        self.half().digests()
    }

    /// The records and scans of this unit's baseline half that `wanted` asks for, as the blobs a cabinet keeps them as.
    pub fn baseline_blobs(&self, wanted: &dyn Fn(&Digest) -> bool) -> Result<Vec<Blob>, String> {
        self.half().blobs(wanted)
    }

    /// This unit with `table` and `entries` as its baseline half ([`PreparedText::baseline_table`], [`PreparedText::baseline_blobs`]), read when a lowering over the unit first asks: what a unit restored without one is given, since restoring it leaves the half empty.
    pub fn with_baseline(mut self, table: impl Into<Bytes>, entries: Vec<Blob>) -> Self {
        self.baseline = Recorded::stored(table, entries);
        self
    }

    /// This unit's import scopes as it holds them, stored apart from what the unit is for its successors.
    pub fn imports_recorded(&self) -> &Recorded<curios_core::Imports> {
        &self.imports
    }

    /// Where this unit's declarations lie as it holds them, stored apart as its import scopes are, each source's texts a blob.
    pub fn anchors_recorded(&self) -> &Recorded<curios_core::Anchors> {
        &self.anchors
    }

    /// This unit with `imports` as its import scopes and `anchors` as where its declarations lie, each its own part and the blobs of its entries ([`PreparedText::imports_recorded`], [`PreparedText::anchors_recorded`]), each read when something first asks: what a restored unit is given, since restoring it leaves it with neither.
    pub fn with_reported(
        mut self,
        imports: (impl Into<Bytes>, Vec<Blob>),
        anchors: (impl Into<Bytes>, Vec<Blob>),
    ) -> Self {
        self.imports = Recorded::stored(imports.0, imports.1);
        self.anchors = Recorded::stored(anchors.0, anchors.1);
        self
    }

    /// This unit's lowered items, each a blob stored on its own, in the order of the items: the one an item was restored from where the unit holds it as it was read, of this unit or of a baseline it is a version of, and its archive, made and digested now, otherwise.
    pub fn items_blobs(&self) -> Result<Vec<Blob>, String> {
        let items = &self.core.items;
        items
            .places()
            .map(|place| match items.part(place) {
                Some(stored) => {
                    debug_assert!(
                        curios_archive::to_bytes(&items[place])
                            .is_ok_and(|made| *made == *stored.bytes.as_slice()),
                        "an item kept as it was stored is stored as it was"
                    );

                    Ok(stored.clone())
                }
                None => curios_archive::to_bytes(&items[place]).map(|made| Blob::of(made.to_vec())),
            })
            .collect()
    }

    /// This unit with `blobs` as its lowered items ([`PreparedText::items_blobs`]), each read when something first asks for it: what a restored unit is given, since restoring it leaves its module without one. `Err` is not one for each item the unit outlines.
    pub fn with_items(mut self, blobs: Vec<Blob>) -> Result<Self, String> {
        self.core = self.core.with_items(blobs)?;

        Ok(self)
    }

    /// The declarations this unit's lowered module makes ([`curios_core::Declarations`]), made the first time something asks: a unit lowered in this process holds its module whole.
    pub fn declarations(&self) -> &curios_core::Declarations {
        self.declarations
            .get_or_init(|| curios_core::Declarations::of(self.core()))
    }

    /// The names this unit declares by an item it does not hold as the baseline it was lowered over holds it: lowered again or new to the unit, or a kept witness named otherwise. What a recompile over that baseline makes again, every other declaration's item being the baseline's own under the same name, with the registry entries the baseline holds for it. `None` for a unit lowered over none.
    ///
    /// **By what the walk did, never by the text.** A declaration is the baseline's where the walk kept it, which a declaration lowered from the text and resolutions the baseline's was lowered from is; and under the same name, since a kept witness is named anew and may take the name another held.
    pub fn lowered_anew(&self) -> Option<&BTreeSet<curios_core::Global>> {
        self.revision.as_ref().map(|revision| &revision.anew)
    }

    /// The names the baseline this unit was lowered over declares and this unit does not. `None` for a unit lowered over none.
    pub fn dropped(&self) -> Option<&BTreeSet<curios_core::Global>> {
        self.revision.as_ref().map(|revision| &revision.dropped)
    }

    /// What each `use` brought into scope, where, and under which spelling — the table `curios-elab`'s goal suggestions draw imported candidates from and spell them by.
    pub fn imports(&self) -> &curios_core::Imports {
        self.imports
            .get()
            .unwrap_or_else(|error| panic!("a unit's import scopes failed to restore: {error}"))
    }

    /// Each module body's declarations. See the field.
    fn bodies(&self) -> &Bodies {
        self.bodies.get_or_init(|| Bodies::of(self.lowerings()))
    }

    /// The declarations a later lowering may keep, by the text each was lowered from: the place of each one's first item. See the field.
    fn by_text(&self) -> &Persistent<Key, i64> {
        self.by_text.get_or_init(|| texts_of(self.lowerings()))
    }

    /// How many module bodies import each module. See the field.
    fn importers(&self) -> &Persistent<Qualifier, usize> {
        self.importers.get_or_init(|| {
            let mut importers = Persistent::new();
            for import in self.imports().entries() {
                if let curios_core::Import::Module { module, .. } = import {
                    let count = importers.get(module).copied().unwrap_or_default();
                    importers.insert(*module, count + 1);
                }
            }
            importers
        })
    }

    /// What the lint's roots reach of the unit's items. See the field. `None` for an entry restored, whose entry's terms no unit holds.
    fn reachability(&self) -> Option<&Rc<Reachability>> {
        if let Some(held) = self.reachability.get() {
            return Some(held);
        }
        let entry = match (
            &self.entry_roots,
            self.mounts.iter().any(|mount| mount.prefix.is_root()),
        ) {
            (Some(entry), _) => (**entry).clone(),
            (None, false) => BTreeSet::new(),
            (None, true) => return None,
        };
        let items = reachability::Items {
            items: &self.core.items,
            places: self.places(),
            records: self.lowerings(),
        };

        Some(
            self.reachability.get_or_init(|| {
                Rc::new(Reachability::of(items, &self.public, &self.mounts, entry))
            }),
        )
    }

    /// The unit's aliases. See the field.
    fn aliases(&self) -> &Rc<Aliases> {
        self.aliases.get_or_init(|| {
            let items = &self.core.items;
            let names = items.places().map(|place| items.names(place));
            let facts = self
                .lowerings()
                .iter()
                .flat_map(|(_, record)| record.facts());

            Rc::new(aliases(names.zip(facts)))
        })
    }

    /// Who can see each declaration of this unit. See the field.
    fn audiences(&self) -> &Audiences {
        self.audiences
            .get_or_init(|| Audiences::of(&self.table, &self.public))
    }

    /// Every absolute path this unit gives a global, with who may write it — see [`writable_paths`]. Made once for the unit: every report of every compilation it is in scope of reads the one table.
    fn paths(&self) -> Rc<curios_core::Paths> {
        self.paths_cell().paths()
    }

    /// What a report spells this unit's own names by: its lowered module's lexicon ([`curios_core::Definitions::lexicon`]), made once for the unit, or revised of the unit it was lowered over.
    pub fn lexicon(&self) -> Rc<curios_core::Lexicon> {
        self.lexicon_cell().lexicon()
    }

    /// Where this unit's lexicon is made and kept. See the field.
    fn lexicon_cell(&self) -> &Rc<LexiconCell> {
        self.lexicon
            .get_or_init(|| Rc::new(LexiconCell::whole(self)))
    }

    /// Where this unit's paths are made and kept. See the field.
    fn paths_cell(&self) -> &Rc<PathsCell> {
        self.paths.get_or_init(|| {
            Rc::new(PathsCell::whole(
                self.table.clone(),
                self.public.clone(),
                self.audiences().clone(),
            ))
        })
    }

    /// How this unit's names can be written, where — what every report spells a name by: its imports, and the absolute paths each unit of its scope gives a global, `predecessors` being the units it was lowered against.
    ///
    /// **A unit's paths are made where a report first asks, once for the unit, and stored by none.** They are a function of the direct interface and export view the unit already stores, and a report is their one reader: a compilation that reports nothing makes none, and one that reports makes only the ones no report before it asked for.
    pub fn spellings(&self, predecessors: &[&PreparedText]) -> curios_core::Spellings {
        spellings_over(
            self.imports(),
            (self.paths(), Rc::new(self.audiences().module_exports())),
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
    ///
    /// **Over a baseline's table, a module whose text stands keeps the baseline's entry** (`carried`): its direct interface is a function of its text alone.
    fn of(
        source: &UnitSource<'_>,
        root: &[Headed],
        scans: Option<&Scans>,
        predecessor_tables: &'a [&'a Table<ModuleInfo>],
        predecessor_mounts: &[Mount],
        own: &[Mount],
        carried: Option<&Table<ModuleInfo>>,
    ) -> Result<Self, Error> {
        curios_common::profile!("resolved::of");
        let mut resolved = Resolved {
            modules: HashMap::new(),
            mod_spans: HashMap::new(),
            table: Scoped::from(predecessor_tables, carried.cloned().unwrap_or_default()),
            touched: HashSet::new(),
            removed: Vec::new(),
            order: HashMap::new(),
        };

        // The compilation root: the entry's own module when the entry is what is being lowered, and otherwise a synthetic one belonging to no unit — which is why its children are *every* mounted prefix rather than only this unit's.
        //
        // Writing it lands in this unit's own layer, which shadows whatever the predecessors' layer said, so listing only `own` here would silently hide the predecessors' mounts — `/std` among them — from a unit being compiled against them, which the test that says a unit reaches a mounted name holds.
        let mut root_info = scan_module_info(root)?;
        for child in mounted_children(predecessor_mounts.iter().chain(own)) {
            root_info.insert_child(&Label::from(child), true)?;
        }
        let carried_root = carried.and_then(|carried| carried.get(&Qualifier::empty()).cloned());
        resolved.table.insert(Qualifier::empty(), root_info);
        resolved.touched.insert(Qualifier::empty());
        resolved.order.insert(Qualifier::empty(), 0);

        // The entry's children hang off the root, whose `ModuleInfo` was just built by hand; a mounted unit has no root items, so this recurses over nothing for it.
        resolved.discover_children(root, &Qualifier::empty(), false, source, scans)?;

        for mount in own.iter().filter(|mount| !mount.prefix.is_root()) {
            let header = Rc::new(Loaded::of(source, scans, &mount.prefix)?);
            resolved.modules.insert(mount.prefix, Rc::clone(&header));
            let standing = carried.is_some() && !header.touched();
            resolved.discover(&header.heads, &mount.prefix, standing, source, scans)?;
        }

        // What a touched module declared and no longer does: a child module taken away, and every namespace its declarations made, which seeding its export view makes again.
        if let Some(carried) = carried {
            let touched = resolved.touched.iter().copied().collect::<Vec<_>>();
            for module in touched {
                let held = match module.is_root() {
                    true => carried_root.clone(),
                    false => carried.get(&module).cloned(),
                };
                let Some(held) = held else {
                    continue;
                };
                for (label, _) in held.children() {
                    let path = module.with(label);
                    // A mounted prefix the root lists is another unit's, and the carried table holds nothing of it.
                    if !resolved.order.contains_key(&path) && carried.contains_key(&path) {
                        let gone = resolved.table.remove_subtree(&path);
                        resolved.removed.extend(gone);
                    }
                }
            }
        }

        Ok(resolved)
    }

    // `mod` declarations only name children, so the module graph is a tree: every qualifier is reached exactly once and no cycles are possible. Hence the walk needs neither a visited-set nor a cache hit-check — just load each file module once and recurse. A module whose text `standing` says is the one a carried table was made of keeps its entry.
    fn discover(
        &mut self,
        heads: &[Headed],
        prefix: &Qualifier,
        standing: bool,
        source: &UnitSource<'_>,
        scans: Option<&Scans>,
    ) -> Result<(), Error> {
        let order = self.order.len();
        self.order.insert(*prefix, order);
        if !(standing && self.table.own().contains_key(prefix)) {
            self.table.insert(*prefix, scan_module_info(heads)?);
            self.touched.insert(*prefix);
        }
        self.discover_children(heads, prefix, standing, source, scans)
    }

    // The child-recursion half of `discover`, split out so `of` can build the compilation root's `ModuleInfo` itself (with every mounted prefix pre-registered as a child) and recurse into its children without a second, unconditional `scan_module_info` call clobbering that registration.
    fn discover_children(
        &mut self,
        heads: &[Headed],
        prefix: &Qualifier,
        standing: bool,
        source: &UnitSource<'_>,
        scans: Option<&Scans>,
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
                    // An inline module's text is its parent's.
                    Some(heads) => self.discover(heads, &path, standing, source, scans)?,
                    None => {
                        let module =
                            Loaded::of(source, scans, &path).map_err(|error| match span {
                                Some(span) => error.at(span.clone()),
                                None => error,
                            })?;
                        let module = Rc::new(module);
                        let standing = scans.is_some() && !module.touched();

                        self.modules.insert(path, Rc::clone(&module));
                        self.discover(&module.heads, &path, standing, source, scans)?;
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
            // A concept declares the type-former binding *and* a nested namespace its methods bind in, like an inductive's constructors.
            Head::Concept(group) => {
                for concept in group {
                    info.insert_concept_child(&concept.label, concept.vis_pub)?;
                    info.insert_binding(&concept.label, concept.vis_pub)?;
                }
            }
            // A witness is anonymous: it declares no binding and occupies no lexical scope — its backing definition gets a compiler name.
            Head::Witness => {}
            // A `foreign` declaration is an ordinary binding, like a `let` — it has no body of its own, but it is called the same way.
            Head::Foreign(foreign) => info.insert_binding(&foreign.label, foreign.vis_pub)?,
            // A broken item binds the name its head declared, public so a reference from anywhere resolves to it and is withheld rather than reported unbound; nothing can use the binding.
            Head::Broken { declares, .. } => {
                if let Some(label) = declares {
                    info.insert_binding(label, true)?;
                }
            }
            // A test takes its name and binds nothing: no reference resolves to one.
            Head::Test(label) => info.insert_test(label)?,
            Head::Use(_) => {}
        }
    }

    Ok(info)
}

// The surface concept application `C(args)` for a witness's declared type: the witnessed concept applied to the annotation's arguments under the marks they were written with, as any application is — a list with nothing written included, which is the application `C()` and not the bare name.
//
// **The head holds where it was written**, as a name written as a term does: what the lowering resolved at a place is read off the variable it lowers to.
//
// **Spanned over the written `C(args)`, because this is the only thing a `satisfy` refusal can point at.** A witness is anonymous, so nothing else in its declaration names it: `elaborate_module_let` locates a registration failure with `error.at_opt(def.type_.span())`, and this synthesized node is that type. Left spanless, every duplicate, orphan, unkeyable and irregular-premise refusal would arrive with no line at all — and a duplicate between two witnesses of one module would name that module twice and nothing else, which a reader cannot act on. `written` is the head as the parser read it, through the parenthesis that closes its arguments; `Term::with_span` keeps an innermost span already present, so the arguments are untouched.
fn witness_concept_application(
    concept: &Name,
    args: Option<&[Argument]>,
    written: Option<&Span>,
) -> Term {
    let head: Term = Subterm::Name(concept.clone()).into();
    let head = match concept.span() {
        Some(span) => head.with_span(span.clone()),
        None => head,
    };
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
    declarations: Vec<HeldRecord>,
    induct_decls: BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: BTreeMap<curios_core::Global, curios_core::StructDecl>,
    concepts: BTreeMap<curios_core::Global, curios_core::ConceptDecl>,
    /// Whether the unit is lowered with its tests ([`UnitSource::compiled`]): where it is built, the walk passes every `test` declaration as if it were not written.
    tested: bool,
    foreigns: ForeignStore,
    broken: Vec<BrokenItem>,
    /// What the `use` declarations of each module body read, by the module ([`BaselineHalf::uses`]): over a baseline, the baseline's, with each body whose scope does not stand recorded again.
    uses: Table<Vec<Consulted>>,
    /// The bodies whose `use` declarations this walk resolved and recorded again.
    relisted: Vec<Qualifier>,
    /// Each declaration the walk met over the baseline, kept or lowered, in order ([`Step`]).
    steps: Vec<Step>,
    /// The modules the walk enters, over a baseline whose units in scope stand ([`Baseline::entered`]): `None` where it enters every one.
    entered: Option<HashSet<Qualifier>>,
}

impl Lowering<'_> {
    /// Whether the walk passes the module `module` unentered, with everything under it: noted as a step, so what is put in after it stands after it.
    fn passes(&mut self, module: &Qualifier) -> bool {
        let (Some(entered), Some(baseline)) = (&self.entered, self.baseline) else {
            return false;
        };
        if entered.contains(module) {
            return false;
        }
        if let Some(last) = baseline.unit().bodies().last_within(module) {
            self.steps.push(Step {
                kept: None,
                passed: Some(last),
                record: 0,
                from: self.items.len(),
            });
        }

        true
    }

    /// Record what the declaration `context` has just lowered to, the unit's items from `from` on: what the unit-wide passes read of each, taken while its registry entries and the text it was written in are at hand, and what the declaration asked of its scope.
    fn note(&mut self, context: &Context, from: usize, foreign: bool) {
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
        self.declarations
            .push(Rc::new(Filed::new(LoweredDeclaration::made(
                context,
                &self.items[from..],
                facts,
                foreign,
            ))));
    }
}

fn process_items(
    heads: &[Headed],
    body: Body<'_>,
    context: &mut Context,
    lowered: &mut Lowering,
    modules: &HashMap<Qualifier, Rc<Loaded>>,
) -> Result<(), Error> {
    // Every binding the body's `use` declarations read, where its scope does not stand.
    let mut read_by_uses = Vec::new();
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
            // A concept declares its type-former binding and a nested namespace its methods bind in, like an inductive's constructors.
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
            // A witness is anonymous and nothing names a test — no binding, no scope entry.
            Head::Witness | Head::Test(_) | Head::Use(_) => {}
            Head::Foreign(foreign) => context.insert_binding(
                foreign.label.to_string(),
                foreign.label.span(),
                context.prefixed(&foreign.label),
            )?,
        }
    }

    for (index, headed) in heads.iter().enumerate() {
        // A build holds no test: nothing of one is begun, anchored or asked of a baseline.
        if matches!(headed.head, Head::Test(_)) && !lowered.tested {
            continue;
        }
        // A source declaration mints in a space of its own, shared by every item it lowers to; a nested module's declarations each take theirs.
        let declares = !matches!(headed.head, Head::Mod { .. });
        let lowered_from = lowered.items.len();
        let foreigns_from = lowered.foreigns.iter().count();
        if declares {
            context.begin_declaration();
            // And is written in a text of its own: what it lowers to holds positions in that text, wherever the file holds it. A module built rather than parsed has no text, and its terms no position.
            if let (Some(anchored), Some(span)) = (anchored(&headed.head, context), &headed.own) {
                context.anchor(anchored, span);
            }
            // A declaration the baseline lowered from this text, every name it resolved standing, is the baseline's: unasked where no edit can have reached it, and asked otherwise.
            if let Some(baseline) = lowered.baseline
                && let Some(standing) = baseline
                    .unasked(context)
                    .or_else(|| baseline.standing(context))
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
                context.note_mod_line(label);
                let path = context.prefixed(label);
                if lowered.passes(&path) {
                    continue;
                }
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
                curios_common::profile!("resolve_use");
                // What a body whose scope stands reads is the baseline's record of it, carried where the body ends.
                let standing = lowered
                    .baseline
                    .is_some_and(|baseline| baseline.stands(&context.prefix()));
                let quiet = context.hush(standing);
                // The lexical import effect of `use`/`pub use`: source-ordered, point-of-use scoping. The interface (export) effect of `pub use` is precomputed by the `pub use` fixed point (`interface::resolve_unit`), not here.
                match &use_item.group {
                    UseGroup::Named(items) => {
                        for item in items {
                            let full = use_item.name.with_label(item.label());
                            context.open_site(UseSite {
                                body: context.prefix(),
                                line: use_item.span.clone(),
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
                            body: context.prefix(),
                            line: use_item.span.clone(),
                            span: use_item.span.clone(),
                            what: UseSiteKind::Glob(format!("{}/*", use_item.name.join())),
                            exported: use_item.vis_pub,
                            used: false,
                        });
                        context.resolve_glob(&use_item.name)?;
                        context.close_site();
                    }
                }
                context.hush(quiet);
                // What resolving it read is the scope's, which every declaration after it in the body stands in.
                if !standing {
                    read_by_uses.extend(context.journal().consulted);
                }
                continue;
            }
            _ => {}
        }
        // Only a declaration reaches here, and only here is its tree read.
        let item = &body.items()?[index];
        lower_declaration(item, context, lowered)?;
        if declares && lowered.items.len() > lowered_from {
            context.end_declaration(
                lowered.items[lowered_from..]
                    .iter()
                    .flat_map(FlatItem::names),
            );
            let foreign = lowered.foreigns.iter().count() > foreigns_from;
            lowered.note(context, lowered_from, foreign);
            if lowered.baseline.is_some() {
                lowered.steps.push(Step {
                    kept: None,
                    passed: None,
                    record: lowered.declarations.len() - 1,
                    from: lowered_from,
                });
            }
        }
    }
    // What the declarations of this body kept unasked read of its scope marks the `use` sites asking them would have.
    if let Some(baseline) = lowered.baseline {
        baseline.mark_uses(context);
    }
    // What a body's `use` declarations read is recorded again where its scope does not stand, and is the baseline's where it does.
    let prefix = context.prefix();
    if !lowered
        .baseline
        .is_some_and(|baseline| baseline.stands(&prefix))
    {
        match read_by_uses.is_empty() {
            true => {
                lowered.uses.remove(&prefix);
            }
            false => lowered.uses.insert(prefix, Rc::new(read_by_uses)),
        }
        lowered.relisted.push(prefix);
    }
    context.close_body();

    Ok(())
}

/// The public bindings of each module some entry of `entries` imports: each label with what it resolves to, by label. A module's interface answers a public binding's target whoever asks (`visible_binding`), so the list is the module's and nothing of the scope that imported it.
fn module_members<'e>(
    entries: impl Iterator<Item = &'e curios_core::Import>,
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
    /// What the unit is compiled for: what `source` was marked for ([`RootSource::tested`]), or the other kind, for a cabinet looking up the same source's other compile ([`UnitSource::other`]).
    compiled: Compiled,
    /// The prefixes of the archived root a unit of this unit's fold stands in for, where one does ([`UnitSource::over_stand_in`]).
    stand_in: Option<Vec<Qualifier>>,
}

impl<'a> UnitSource<'a> {
    /// The entry program, its own modules resolved through `source`.
    pub fn entry(entrypoint: &'a Entrypoint, source: &'a RootSource) -> Self {
        Self {
            entrypoint: Some(entrypoint),
            source,
            visible: source.declared().map(<[Qualifier]>::to_vec),
            compiled: source.compiled(),
            stand_in: None,
        }
    }

    /// A unit with no entrypoint, under the prefixes `source` claims.
    pub fn mounted(source: &'a RootSource) -> Self {
        Self {
            entrypoint: None,
            source,
            visible: source.declared().map(<[Qualifier]>::to_vec),
            compiled: source.compiled(),
            stand_in: None,
        }
    }

    /// The same unit compiled for the other kind: built where this one is tested, and tested where it is built. The two differ by the source's tests alone, so a cabinet holding one offers it as the baseline of the other.
    pub fn other(&self) -> Self {
        Self {
            entrypoint: self.entrypoint,
            source: self.source,
            visible: self.visible.clone(),
            compiled: match self.compiled {
                Compiled::Built => Compiled::Tested,
                Compiled::Tested => Compiled::Built,
            },
            stand_in: self.stand_in.clone(),
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

    /// The same unit, compiled in a fold whose first unit stands in for the archived root claiming `root`, as a package named `std` does: what it reads under `root` is not the compiler's own, so a cabinet keeps it apart from the same unit compiled over the archived root.
    pub fn over_stand_in(self, root: Vec<Qualifier>) -> Self {
        Self {
            stand_in: Some(root),
            ..self
        }
    }

    /// The prefixes of the archived root a unit of this unit's fold stands in for, where one does: the other half of what it read, beside the units a record names.
    pub fn stand_in(&self) -> Option<&[Qualifier]> {
        self.stand_in.as_deref()
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

    /// The packages this unit's files are read for. See [`RootSource::packages`].
    pub fn packages(&self) -> Vec<PackageRoot> {
        self.source.packages()
    }

    /// The prefixes this unit claims, which decide how its names are spelled and so which unit it is.
    pub fn claims(&self) -> Vec<Mount> {
        self.mounts()
    }

    /// The prefixes this unit declared a dependency on, when it declared any — the other half of which unit this is.
    ///
    /// Two units of one source and the same predecessors differing only in what they declared are two lowerings, because a name resolves in one and is refused in the other. So a cabinet addresses them apart, which is what this is read for.
    pub fn declared(&self) -> Option<&[Qualifier]> {
        self.visible.as_deref()
    }

    /// What this unit is compiled for ([`RootSource::tested`]). A built unit and a tested one are two lowerings of one source, so a cabinet addresses them apart, as it does two units that declared differently.
    pub fn compiled(&self) -> Compiled {
        self.compiled
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
/// An entry source's final term is lowered too — a refusal in it is this lowering's — and handed back only by the entry's own spelling, [`into_core_with_prelude`]: a unit is a [`curios_core::Definitions`] alone.
pub fn into_core_unit(
    source: &UnitSource<'_>,
    predecessors: &[&PreparedText],
    syntax: &SyntaxRegistry,
) -> Result<PreparedText, Error> {
    lower_unit(source, predecessors, syntax, None).map(|(unit, _)| unit)
}

/// [`into_core_unit`] over `baseline`, the unit an earlier text of the same sources lowered to: a declaration the baseline lowered from the text it is written in now, every name it resolved resolving as it did, is the baseline's and is not lowered again. The unit is the one lowering the whole of it gives.
///
/// `settled` says that `predecessors` are the units `baseline` was lowered against: what an edit moved is then found from the edit, and a declaration no edit can have reached is kept unasked. Where it is false, or where the mounts `source` may write are not the ones the baseline's could, every declaration is asked.
pub fn into_core_unit_over(
    source: &UnitSource<'_>,
    predecessors: &[&PreparedText],
    syntax: &SyntaxRegistry,
    baseline: &PreparedText,
    settled: bool,
) -> Result<PreparedText, Error> {
    lower_unit(source, predecessors, syntax, Some((baseline, settled))).map(|(unit, _)| unit)
}

/// Lower the entry program `source` is, against the units already lowered and over `baseline` where there is one, with whether `predecessors` are the units it was lowered against: its declarations as the unit they are ([`into_core_unit`], [`into_core_unit_over`]), and the term it closes with beside them.
///
/// The term is lowered on every compile: it is no declaration, and nothing of a unit holds it.
pub fn into_core_entry(
    source: &UnitSource<'_>,
    predecessors: &[&PreparedText],
    syntax: &SyntaxRegistry,
    baseline: Option<(&PreparedText, bool)>,
) -> Result<(PreparedText, curios_core::Entrypoint), Error> {
    let (unit, entry) = lower_unit(source, predecessors, syntax, baseline)?;

    Ok((
        unit,
        entry.expect("an entry source's parse holds its final term"),
    ))
}

/// What every nested context of one unit's lowering shares, behind one reference: what the declaration being lowered mints from, and the unit's own tables, which every declaration writes into and the lowering's caller reads back when it is over.
///
/// **Declared where the lowering is, its operations beside the contexts' in `context`**: [`lower_unit`] reads its tables back once every declaration is lowered.
struct Shared {
    declaring: Declaring,
    universe_role: Cell<curios_core::UniverseRole>,
    // What each source declaration lowered so far minted, with the names it lowered to — see `curios_core::Minted`.
    minted: RefCell<curios_core::Minted>,
    // One ordinal counter per declaring module. A `satisfy` declaration is anonymous, so its identity is minted rather than written — and it is scoped to the module that declares it, because an ordinal alone would mean something only in the compilation that handed it out.
    witnesses: RefCell<BTreeMap<Qualifier, u32>>,
    // Every binding a `use` brought into some lexical scope of this unit, with the path the reader wrote it under, and per definition the ones in scope where it was written — see `curios_core::Imports`. Core keeps the canonical name, and only this stage knows the spelling that resolves at the use site, which is what a goal report's candidate must be pasteable under.
    imports: RefCell<curios_core::Imports>,
    // Every `use` selector and glob of the unit, in the order resolution met them.
    sites: RefCell<Vec<UseSite>>,
    // The prefix of every mount some reference of this unit was written under — what decides whether a declared dependency is used. Recorded by the path as written, never by what it resolved into, so an absolute path and an import count alike and a re-export reaches the unit offering it.
    reached: RefCell<BTreeSet<Qualifier>>,
    // Where the unit's declarations lie, and the one being lowered — see `Anchoring`.
    anchoring: Anchoring,
    // The module bodies whose `use` declarations this lowering has recorded what they brought into scope of.
    closed: RefCell<HashSet<Qualifier>>,
}

/// [`into_core_unit`], with the final term an entry source closes with, lowered beside the unit.
fn lower_unit(
    source: &UnitSource<'_>,
    predecessors: &[&PreparedText],
    syntax: &SyntaxRegistry,
    baseline: Option<(&PreparedText, bool)>,
) -> Result<(PreparedText, Option<curios_core::Entrypoint>), Error> {
    curios_common::profile!("into_core_unit");
    grown(|| into_core_unit_within(source, predecessors, syntax, baseline))
}

fn into_core_unit_within(
    source: &UnitSource<'_>,
    predecessors: &[&PreparedText],
    syntax: &SyntaxRegistry,
    over: Option<(&PreparedText, bool)>,
) -> Result<(PreparedText, Option<curios_core::Entrypoint>), Error> {
    let baseline = over.map(|(baseline, _)| baseline);
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
    // What the baseline kept of each file. A baseline restored from a cabinet holds its half as bytes until this, the lowering's first read of it.
    let baseline_scans =
        curios_common::profile!("into_core_unit::baseline" => baseline.map(PreparedText::scans));

    let Resolved {
        mut table,
        modules,
        mod_spans,
        touched,
        removed,
        order,
    } = Resolved::of(
        source,
        &root,
        baseline_scans,
        &predecessor_tables,
        &predecessor_mounts,
        &own,
        baseline.map(|baseline| &baseline.table),
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

    // Where the units in scope are the ones the baseline was lowered against, the export view is the baseline's with what the edit reached resolved again; where one moved, every module's is resolved again, since a `pub use` may read it. The mounts the unit may write are read as the units in scope are, by every path a declaration writes, and no record names them, so a baseline lowered where the unit could write others is asked whole too.
    let writable = |mounts: &[Mount]| {
        mounts
            .iter()
            .map(|mount| mount.prefix)
            .collect::<BTreeSet<_>>()
    };
    let settled = over
        .filter(|(baseline, settled)| {
            *settled && writable(&baseline.visible) == writable(&visible_mounts)
        })
        .map(|(baseline, _)| baseline);
    let (public, revised) = match settled {
        Some(baseline) => {
            let (public, revised) = interface::resolve_unit_over(
                &root,
                &modules,
                &order,
                &mut table,
                reach,
                Scoped::from(&predecessor_public, baseline.public.clone()),
                baseline,
                &touched,
                &removed,
            )?;
            (public, Some(revised))
        }
        None => (
            interface::resolve_unit(
                &root,
                &own,
                &modules,
                &mut table,
                reach,
                Scoped::over(&predecessor_public),
            )?,
            None,
        ),
    };

    // Over a baseline, what the lowering mints, imports and anchors starts as the baseline's.
    let shared = baseline.map_or_else(Shared::new, Shared::over);
    let mut context = Context::new(&table, &public, reach, &shared, syntax);
    // Every named prefix in the compilation binds its own one-segment name. No two can repeat it: the disjointness check above refuses a unit claiming what a predecessor already holds, and the predecessors' own mounts were pairwise disjoint when each was compiled. The entry's prefix is the empty one, which has no name to bind.
    for mount in &mounts {
        if !mount.prefix.is_root() {
            context.insert_scope(mount.prefix.head().to_string(), None, mount.prefix)?;
        }
    }

    // What the edit moved of what the lowering reads, found from the edit where the units in scope are the ones the baseline was lowered against: the modules whose text it touched or whose scope a binding that moved is read in, and the bindings whose entries are not the baseline's.
    // Who can see each name: the baseline's audiences, with those of the names the modules whose tables changed declare or export read again.
    let (audiences, reread) = match (settled, &revised) {
        (Some(baseline), Some(revised)) => baseline.audiences().revised(
            &baseline.table,
            &baseline.public,
            table.own(),
            public.own(),
            &revised.changed,
        ),
        _ => (Audiences::of(table.own(), public.own()), None),
    };
    // The re-exports the modules whose tables changed now make otherwise: what the audit reads again of them.
    let reexported = match (settled, &revised) {
        (Some(baseline), Some(revised)) => revised
            .changed
            .iter()
            .flat_map(|module| {
                let now = public.own().get(module);
                let then = baseline.public.get(module);
                now.into_iter()
                    .flat_map(|interface| &interface.bindings)
                    .filter(move |(label, entry)| {
                        then.and_then(|interface| interface.bindings.get(*label)) != Some(*entry)
                    })
                    .map(move |(label, _)| (*module, label.clone()))
            })
            .collect::<BTreeSet<_>>(),
        _ => BTreeSet::new(),
    };
    let revised_changed = revised.as_ref().map(|revised| revised.changed.clone());
    let moved = settled
        .zip(revised)
        .map(|(baseline, revised)| Moved::found(baseline, revised));
    let baseline = baseline.map(|baseline| Baseline::of(baseline).moving(moved));
    let mut lowered = Lowering {
        // Over a baseline of the other kind, a module no edit reached still holds tests to lower or to drop.
        entered: baseline
            .as_ref()
            .filter(|baseline| baseline.unit().compiled == source.compiled())
            .and_then(Baseline::entered),
        baseline: baseline.as_ref(),
        uses: baseline
            .as_ref()
            .map(|baseline| baseline.unit().half().uses.clone())
            .unwrap_or_default(),
        tested: source.compiled() == Compiled::Tested,
        ..Lowering::default()
    };

    // The compilation root's own items — the entry's, and none for a unit with no entrypoint — then one pass per prefix this unit claims. Exactly one of the two does any work, because owning the empty prefix is what makes a unit the entry.
    if let Some(entrypoint) = source.entrypoint() {
        curios_common::profile!("process_items" => process_items(
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
        if lowered.passes(&mount.prefix) {
            continue;
        }

        let mut nested = context.nested(mount.prefix.head());

        curios_common::profile!("process_items" => process_items(
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
        tested: _,
        foreigns,
        broken,
        mut uses,
        relisted,
        steps,
        entered: _,
    } = lowered;
    // A body the unit no longer holds keeps no record of what its `use` declarations read.
    let unwalked = removed
        .iter()
        .filter(|body| uses.contains_key(body))
        .copied()
        .collect::<Vec<_>>();
    for body in &unwalked {
        uses.remove(body);
    }
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

    // The items lowered now, by their place among the walk's, and the baseline's declarations the walk did not keep, with the names they declared.
    let made = steps
        .iter()
        .zip(
            steps
                .iter()
                .skip(1)
                .map(|step| step.from)
                .chain([flat_items.len()]),
        )
        .filter(|(step, _)| step.made())
        .flat_map(|(step, end)| step.from..end)
        .collect::<HashSet<_>>();
    let gone = baseline
        .as_ref()
        .map(|baseline| revision::gone(baseline.unit(), &steps, shared.closed(), &removed))
        .unwrap_or_default();
    let gone_names = gone
        .iter()
        .flat_map(|gone| &gone.names)
        .copied()
        .collect::<HashSet<_>>();

    // Every item is lowered, so every definition a witness's signature may name is at hand: whole, every one; over a baseline, the items lowered now, and the baseline's for every other.
    let (renames, keyed) = {
        curios_common::profile!("name_witnesses");
        let lowered_now = flat_items
            .iter()
            .enumerate()
            .filter(|(index, _)| baseline.is_none() || made.contains(index))
            .flat_map(|(_, item)| keyed_of(item))
            .collect::<HashMap<_, _>>();
        let written = |name: &curios_core::Global| {
            lowered_now.get(name).copied().or_else(|| {
                let baseline = baseline.as_ref()?;
                (!gone_names.contains(name))
                    .then(|| baseline.unit().written(name))
                    .flatten()
            })
        };
        // A concept the walk did not lower is the baseline's.
        let marks = |name: &curios_core::Global| match concepts.contains_key(name) {
            true => Some(struct_decls.get(name).map(|record| record.arity.marks())),
            false => {
                let baseline = baseline.as_ref()?;
                if gone_names.contains(name) {
                    return None;
                }
                match baseline.unit().nominal(name)? {
                    curios_core::Nominal::Concept { record, .. } => {
                        Some(Some(record.arity.marks()))
                    }
                    _ => None,
                }
            }
        };
        let own = Own {
            written: &written,
            marks: &marks,
        };
        // Over a baseline, the witnesses of each module where one is lowered now, whose place may come before a kept one's, and of each where a kept one's key read a declaration lowered now or taken away.
        let rekeyed = baseline
            .as_ref()
            .map(|baseline| {
                let readers = baseline.unit().lookup_readers();
                made.iter()
                    .flat_map(|index| flat_items[*index].names())
                    .chain(gone_names.iter().copied())
                    .flat_map(|name| readers.of_key(&name).to_vec())
                    .filter(|witness| !gone_names.contains(witness))
                    .collect::<HashSet<_>>()
            })
            .unwrap_or_default();
        let modules = made
            .iter()
            .flat_map(|index| flat_items[*index].names())
            .chain(rekeyed.iter().copied())
            .filter_map(|name| match name {
                curios_core::Global::Witness(id) => Some(*id.module()),
                curios_core::Global::Authored(_) => None,
            })
            .collect::<HashSet<_>>();
        let spelled = |name: &curios_core::Global| match name {
            curios_core::Global::Witness(id) if !rekeyed.contains(name) => id
                .spelled()
                .and_then(|(concept, key)| Some((concept.qualifier().copied()?, key))),
            _ => None,
        };
        let naming = Naming {
            modules: &modules,
            spelled: &spelled,
        };
        witness_names(
            &flat_items,
            &own,
            predecessors,
            baseline.as_ref().map(|_| &naming),
        )
    };
    rename_witnesses(
        Named {
            items: &mut flat_items,
            minted: &mut shared.minted.borrow_mut(),
            imports: &mut shared.imports.borrow_mut(),
        },
        &renames,
    );
    // What each witness's key read: the baseline's, with what naming read now in place of what it read of each witness named or taken away.
    let mut held_keys = baseline
        .as_ref()
        .map(|baseline| baseline.unit().half().keyed.clone())
        .unwrap_or_default();
    let mut keys_changed = Vec::new();
    for name in gone_names
        .iter()
        .filter(|name| matches!(name, curios_core::Global::Witness(_)))
    {
        held_keys.remove(name);
        keys_changed.push(*name);
    }
    let moved = renames
        .iter()
        .filter_map(|(held, taken)| Some((*taken, held_keys.remove(held)?)))
        .collect::<Vec<_>>();
    keys_changed.extend(renames.iter().flat_map(|(held, taken)| [*held, *taken]));
    for (taken, read) in moved {
        held_keys.insert(taken, read);
    }
    for (witness, read) in keyed {
        held_keys.insert(witness, Rc::new(read));
        keys_changed.push(witness);
    }
    let lookups = baseline.as_ref().map(|baseline| {
        baseline.readers_of(
            gone.iter().map(|gone| gone.record),
            steps
                .iter()
                .filter(|step| step.made())
                .map(|step| &declarations[step.record]),
            &uses,
            relisted.into_iter().chain(unwalked),
            &held_keys,
            keys_changed,
        )
    });

    // The mounts a name was written under: what the walk reached, and, over a baseline, each a declaration or a body's `use` declarations it passed read.
    let reached = {
        let mut reached = shared.reached.take();
        if let Some(lookups) = &lookups {
            reached.extend(
                mounts
                    .iter()
                    .map(|mount| mount.prefix)
                    .filter(|prefix| lookups.reaches_mount(*prefix)),
            );
        }
        reached
    };

    // The entrypoint, for the one unit that has one. Its tail closes the root body, so the imports in scope there are the last the root saw.
    context.record_import_scope(None);
    // Every scope is recorded: the members of the modules they import are the interfaces' to state, once for the unit. Over a baseline, each module a body walked or taken away imported then or imports now, and each whose interface changed, is offered again, or withdrawn where no body imports it any longer.
    let importers = {
        let mut imports = shared.imports.borrow_mut();
        match (&baseline, &revised_changed) {
            (Some(baseline), Some(changed)) => {
                let earlier = baseline.unit().imports();
                let mut importers = baseline.unit().importers().clone();
                let mut asked = changed.clone();
                let modules = |imports: &[curios_core::Import]| {
                    imports
                        .iter()
                        .filter_map(|import| match import {
                            curios_core::Import::Module { module, .. } => Some(*module),
                            curios_core::Import::Binding { .. } => None,
                        })
                        .collect::<Vec<_>>()
                };
                for body in shared.closed().iter().chain(&removed) {
                    for module in modules(earlier.body(body)) {
                        let count = importers.get(&module).copied().unwrap_or_default();
                        importers.insert(module, count.saturating_sub(1));
                        asked.insert(module);
                    }
                    for module in modules(imports.body(body)) {
                        let count = importers.get(&module).copied().unwrap_or_default();
                        importers.insert(module, count + 1);
                        asked.insert(module);
                    }
                }
                for module in asked {
                    match importers.get(&module).copied().unwrap_or_default() {
                        0 => {
                            importers.remove(&module);
                            imports.withdraw(&module);
                        }
                        _ => {
                            let offered = module_members(
                                [curios_core::Import::Module {
                                    module,
                                    label: String::new(),
                                }]
                                .iter(),
                                &public,
                            );
                            for (module, offered) in offered {
                                imports.offer(module, offered);
                            }
                        }
                    }
                }
                OnceCell::from(importers)
            }
            _ => {
                let members = module_members(imports.entries(), &public);
                for (module, offered) in members {
                    imports.offer(module, offered);
                }
                OnceCell::new()
            }
        }
    };
    // The entry mints in a space of its own, as a declaration does, and is written in a text of its own.
    context.begin_declaration();
    if let Some(span) = source
        .entrypoint()
        .and_then(|entrypoint| entrypoint.tail.span())
    {
        context.anchor(curios_core::Anchored::Entry, span);
    }
    let entry = {
        // Scoped so the lowerer drops, and records its unread sites, before the tables the context borrows are moved into the result below.
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

    // Decided above, of the unit's own layers, and kept with the unit: the audit below and every report after read the one answer.
    let in_scope = AudienceScope::over(&audiences, &own, &visible_mounts, predecessors);
    debug_assert!(
        in_scope.widens_nothing(public.own()),
        "an export of another unit's name shows it to nobody its own unit does not"
    );
    // The audit's refusal of one of the unit's items. It spells the item and what it exposes as a reader standing at the item writes each, as every report of the unit does once it is lowered ([`PreparedText::spellings`]), and a module by its path, as a name that does not resolve is told of one. Made where the audit refuses, from the tables as they stand, and by no audit that passes.
    let refused = |item: &Qualifier, unseen: &Unseen| {
        let paths = writable_paths(table.own(), public.own(), &audiences);
        let spellings = spellings_over(
            &shared.imports.borrow(),
            (Rc::new(paths), Rc::new(audiences.module_exports())),
            &own,
            &visible_mounts,
            predecessors,
        );
        let owner = curios_core::Global::Authored(*item);
        let spelled = |name: &curios_core::Global| {
            spell(&spellings, &item.without_last(), Some(&owner), name)
                .unwrap_or_else(|| name.faithful().to_string())
        };
        let behind = unseen.behind(&|module, label| table.get(module)?.get_child(label));

        Error::PrivateItemInPublicInterface {
            item: spelled(&owner),
            referent: spelled(unseen.referent()),
            behind: behind.map(|module| module.join()),
        }
    };
    // Over a baseline, what could fail now that did not: each definition lowered now, each re-export made otherwise, and each export of a former lowered now or taken away, where no alias was lowered or taken away and no name the baseline declared is seen otherwise. A whole audit otherwise.
    let alias = |lowered: &LoweredItem| {
        lowered
            .definitions
            .iter()
            .any(|declared| declared.alias.is_some())
    };
    let aliases_moved = made.iter().any(|index| alias(facts[*index]))
        || gone
            .iter()
            .any(|gone| gone.record.facts().iter().any(alias));
    // The aliases: the baseline's where no alias was lowered or taken away, those of the unit's items otherwise, read off the unit once it is made over a baseline.
    let whole_aliases = match (&baseline, aliases_moved) {
        (None, _) => Some(Rc::new(aliases(
            flat_items
                .iter()
                .map(|item| item.declared().map(|(name, _, _)| name).collect())
                .zip(facts.iter().copied()),
        ))),
        (Some(baseline), false) => Some(Rc::clone(baseline.unit().aliases())),
        (Some(_), true) => None,
    };
    let reexports = match (&baseline, reread.clone()) {
        (Some(baseline), Some(reread)) if !aliases_moved => {
            let made_names = made
                .iter()
                .flat_map(|index| flat_items[*index].names())
                .collect::<HashSet<_>>();
            let earlier = baseline.unit().audiences();
            // A name seen otherwise is one the unit declares now and the baseline did not, which nothing kept reads.
            let seen_otherwise = reread.iter().any(|name| {
                let global = curios_core::Global::Authored(*name);
                !earlier.same_as(&audiences, name)
                    && (baseline.unit().declared_as(&global).is_some()
                        || !made_names.contains(&global))
            });
            (!seen_otherwise).then(|| {
                let formers = made
                    .iter()
                    .flat_map(|index| flat_items[*index].declared())
                    .filter(|(_, declared, _)| declared.is_former())
                    .map(|(name, _, _)| name)
                    .chain(
                        gone.iter()
                            .flat_map(|gone| &gone.names)
                            .filter(|name| {
                                baseline
                                    .unit()
                                    .declared_as(name)
                                    .is_some_and(|declared| declared.is_former())
                            })
                            .copied(),
                    )
                    .filter_map(|name| name.qualifier().copied())
                    .collect::<Vec<_>>();
                reexported
                    .iter()
                    .cloned()
                    .chain(
                        formers
                            .iter()
                            .flat_map(|former| audiences.exported_at(former)),
                    )
                    .collect::<BTreeSet<_>>()
            })
        }
        _ => None,
    };
    // The definitions the audit reads, by name in the order the items stand: whole, every one; over a baseline, those lowered now where nothing else could fail, and every one of the unit once it is made otherwise.
    let mut lowered_now = made.iter().copied().collect::<Vec<_>>();
    lowered_now.sort_unstable();
    let audited = match (&baseline, &reexports) {
        (None, _) => Some(
            flat_items
                .iter()
                .zip(&facts)
                .flat_map(|(item, lowered)| {
                    item.declared()
                        .map(|(name, _, _)| name)
                        .zip(&lowered.definitions)
                })
                .collect::<Vec<_>>(),
        ),
        (Some(_), Some(_)) => Some(
            lowered_now
                .iter()
                .flat_map(|index| {
                    flat_items[*index]
                        .declared()
                        .map(|(name, _, _)| name)
                        .zip(&facts[*index].definitions)
                })
                .collect::<Vec<_>>(),
        ),
        (Some(_), None) => None,
    };

    // Whole, what no root reaches is found by a walk from every root; over a baseline, it is the baseline's decided again where the edit could have moved it, once the unit is a version of the baseline's.
    let whole_dead = match &baseline {
        None => unused_declarations(&Declarations {
            items: &flat_items,
            facts: &facts,
            entry: entry.as_ref(),
            table: &table,
            public: &public,
            own: &own,
            mod_spans: &mod_spans,
            place_module: &|module| {
                let written = mod_spans.get(module)?;
                shared
                    .anchoring
                    .line(module.without_last(), written, written)
            },
        }),
        Some(_) => Vec::new(),
    };
    let made_first = made
        .iter()
        .filter_map(|index| flat_items[*index].names().first().copied())
        .collect::<Vec<_>>();
    let entry_roots = entry_names(entry.as_ref());

    // Where each definition is named, by the name it leaves the lowering under, which for a witness is the one its signature gave it above: whole, every one; over a baseline, each lowered now and each kept witness named otherwise, every other's being the baseline's.
    let taken = renames.values().copied().collect::<HashSet<_>>();
    for (index, (item, lowered)) in flat_items.iter().zip(&facts).enumerate() {
        if baseline.is_some()
            && !made.contains(&index)
            && !item.names().iter().any(|name| taken.contains(name))
        {
            continue;
        }
        for ((name, _, _), declared) in item.declared().zip(&lowered.definitions) {
            if let Some(label) = declared.label {
                shared.anchoring.name(name, label);
            }
        }
        for (name, label) in &lowered.members {
            if let Some(label) = label {
                shared.anchoring.name(*name, *label);
            }
        }
        for (structure, fields) in &lowered.fields {
            shared.anchoring.name_fields(*structure, fields.clone());
        }
    }

    // This unit's own items alone. A predecessor reaches later stages as an *environment* they are seeded from — `Globals` at the certifier, a replayed context at elaboration and erasure — and copying its items into every compilation only ever existed so those stages could then skip them again by index. See `documentation/design/compilation/a-unit-holds-its-own-definitions-and-the-prelude-is-an-environment.md`.
    //
    // As they are written. No item is moved for another's sake: elaboration asks for each declaration when it is read, wherever it sits, refuses the ones that need one another by name, and stores the module in an order of its own (`curios_elab`'s `Demand`, `curios_core::Definitions::ordered`).
    let made = {
        let minted = shared.minted.borrow();
        flat_items
            .into_iter()
            .map(|item| item.into_core(&minted))
            .collect::<Vec<_>>()
    };
    // Over a baseline, the unit is a version of the baseline's: its items, records, names and declarations, and what it minted, imported and anchored, the baseline's with what the walk changed.
    let (items, records, places, declared, bodies, texts, revision, renamed) = match &baseline {
        Some(baseline) => {
            let revised = revise(baseline.unit(), &steps, &declarations, &made, &gone);
            shared.settle(
                &revised.renamed,
                &revised.revision.dropped,
                &texts_gone(&gone, &declarations),
                &removed,
            );

            (
                revised.items,
                revised.records,
                OnceCell::from(revised.places),
                OnceCell::from(revised.declarations),
                OnceCell::from(revised.bodies),
                OnceCell::from(revised.texts),
                Some(revised.revision),
                revised.renamed,
            )
        }
        None => (
            made.into(),
            in_walk_order(declarations.iter().cloned()),
            OnceCell::new(),
            OnceCell::new(),
            OnceCell::new(),
            OnceCell::new(),
            None,
            Vec::new(),
        ),
    };
    // Over a baseline, the unit's rows are its declarations' in the order they stand: each lowered now the walk's, every other the baseline's, whether or not the walk entered its module.
    let foreigns = match (&baseline, bodies.get()) {
        (Some(baseline), Some(bodies)) => {
            bodies.foreign_rows(&items, &foreigns, baseline.unit().foreigns())
        }
        _ => foreigns,
    };

    // What each body's `use` declarations were found to leave unused: the baseline's, with each body the walk passed found again and each it no longer holds taken away.
    let mut body_lints = baseline
        .as_ref()
        .map(|baseline| baseline.unit().half().lints.clone())
        .unwrap_or_default();
    {
        let mut imports = BTreeMap::<Qualifier, Vec<UseSite>>::new();
        for site in shared.sites.take() {
            imports.entry(site.body).or_default().push(site);
        }
        for body in shared.closed() {
            let lints =
                unused_imports(imports.remove(&body).unwrap_or_default(), &shared.anchoring);
            match lints.is_empty() {
                true => {
                    body_lints.remove(&body);
                }
                false => body_lints.insert(body, Rc::new(lints)),
            }
        }
        for body in &removed {
            body_lints.remove(body);
        }
    }

    // The audit, over the unit made: what an edit could have made fail over a baseline, and every definition and re-export otherwise.
    {
        let view = places.get().map(|places| reachability::Items {
            items: &items,
            places,
            records: &records,
        });
        let held = &items;
        let every = || {
            let view = view.expect("a unit made over a baseline holds where it places each name");
            held.places()
                .flat_map(move |place| {
                    held.names(place)
                        .into_iter()
                        .zip(&view.facts(place).definitions)
                })
                .collect::<Vec<_>>()
        };
        let aliases = whole_aliases.clone().unwrap_or_else(|| {
            let view = view.expect("a unit made over a baseline holds where it places each name");
            Rc::new(aliases(
                items
                    .places()
                    .map(|place| (items.names(place), view.facts(place))),
            ))
        });
        let definitions = audited.clone().unwrap_or_else(every);
        audit_public_exposures(
            &public,
            in_scope,
            &definitions,
            &aliases,
            NominalScope::new(
                predecessors,
                &induct_decls,
                &struct_decls,
                baseline
                    .as_ref()
                    .map(|baseline| (baseline.unit(), &gone_names)),
            ),
            &|extent| shared.anchoring.resolved(extent),
            &|name| shared.anchoring.label(name),
            &refused,
            reexports.as_ref(),
        )?;
    }
    let aliases = match whole_aliases {
        Some(aliases) => aliases,
        None => {
            let view = reachability::Items {
                items: &items,
                places: places
                    .get()
                    .expect("a unit made over a baseline holds where it places each name"),
                records: &records,
            };
            Rc::new(aliases(
                items
                    .places()
                    .map(|place| (items.names(place), view.facts(place))),
            ))
        }
    };

    // Over a baseline, what no root reaches decided again where the edit could have moved it, and every item decided again where the modules open to a consumer may have.
    let (reachability, dead) = match &baseline {
        Some(baseline) => {
            let earlier = baseline.unit();
            let now = reachability::Items {
                items: &items,
                places: places
                    .get()
                    .expect("a version holds where it places each name"),
                records: &records,
            };
            let made_places = made_first
                .iter()
                .filter_map(|name| now.places.place(name).map(|place| place.item()))
                .collect::<Vec<_>>();
            let state = match (earlier.reachability(), &reread) {
                (Some(state), Some(exported)) => state.revised(
                    reachability::Items {
                        items: &earlier.core().items,
                        places: earlier.places(),
                        records: earlier.lowerings(),
                    },
                    now,
                    &gone,
                    &made_places,
                    &renamed,
                    exported,
                    &|name| audiences.exported_at(name),
                    entry_roots.clone(),
                ),
                _ => Reachability::of(now, public.own(), &own, entry_roots.clone()),
            };
            let dead = state.lints(
                now,
                &|module, label| table.get(module)?.get_child(label),
                &mod_spans,
                &|module| {
                    let written = mod_spans.get(module)?;
                    shared
                        .anchoring
                        .line(module.without_last(), written, written)
                },
            );
            (OnceCell::from(Rc::new(state)), dead)
        }
        None => (OnceCell::new(), whole_dead),
    };
    // Over a baseline, its paths with those of every name whose audience or exports were read again revised; otherwise made whole. Either where a report first asks.
    let paths = match (settled, &reread) {
        (Some(baseline), Some(names)) => OnceCell::from(Rc::new(PathsCell::over(
            baseline.paths_cell(),
            names,
            table.own().clone(),
            public.own().clone(),
            audiences.clone(),
        ))),
        _ => OnceCell::new(),
    };
    // Over a baseline, its lexicon with the entries of every name lowered anew or dropped read again; otherwise made whole. Either where a report first asks.
    let lexicon = match (&baseline, &revision) {
        (Some(baseline), Some(revision)) if items.is_shared() => {
            OnceCell::from(Rc::new(LexiconCell::over(
                baseline.unit().lexicon_cell(),
                revision.anew.union(&revision.dropped).copied().collect(),
                curios_core::Definitions {
                    items: items.clone(),
                },
                places
                    .get()
                    .cloned()
                    .expect("a unit lowered over a baseline holds where its items hold each name"),
            )))
        }
        _ => OnceCell::new(),
    };

    // Each line a lint stands in is kept, and every other namespace text goes: one a lint this lowering no longer finds was sited for, or one a baseline held of a line since gone. A zero stands in a declaration's text, never on a line.
    // So is each line a `use` or a `mod` line names something on, which a place there answers.
    let named = shared
        .imports
        .borrow()
        .every_line()
        .map(|line| line.at)
        .collect::<Vec<_>>();
    shared.anchoring.keep_lines(
        body_lints
            .iter()
            .flat_map(|(_, lints)| lints.iter())
            .chain(&dead)
            .filter_map(|lint| lint.at)
            .chain(named),
    );
    let lints = body_lints
        .iter()
        .flat_map(|(_, lints)| lints.iter().cloned())
        .chain(dead)
        .collect();
    let unit = PreparedText {
        mounts: own.clone(),
        compiled: source.compiled(),
        foreigns,
        table: table.into_own(),
        public: public.into_own(),
        audiences: OnceCell::from(audiences),
        aliases: OnceCell::from(aliases),
        paths,
        lexicon,
        places,
        declarations: declared,
        bodies,
        by_text: texts,
        revision,
        core: curios_core::Definitions { items },
        minted: Rc::new({
            let mut minted = shared.minted.into_inner();
            minted.entry = entry_mints;
            minted
        }),
        imports: Recorded::held(shared.imports.into_inner()),
        visible: visible_mounts,
        broken,
        lints,
        reached,
        anchors: Recorded::held(shared.anchoring.into_anchors()),
        packages: Vec::new(),
        baseline: Recorded::held(BaselineHalf {
            declarations: records,
            scans,
            uses,
            keyed: held_keys,
            lints: body_lints,
        }),
        lookups: lookups.map(OnceCell::from).unwrap_or_default(),
        importers,
        reachability,
        entry_roots: Some(Rc::new(entry_roots)),
    };

    Ok((unit, entry))
}

/// The interface of the unit `unit` was lowered into from `source`, for its consumers, or `None` where the source marks no mount as documented: an entry program, which has no consumer.
///
/// **Built where a command asks for it, and carried by no unit.** Two commands read a record, so no compilation builds one and no image holds one. What a page shows is still read off the compilation that built the library: the direct interface, the export view and what each written name resolved to are the ones `unit`'s lowering stored, and the surface tree is discovery's, read again through `source`, which parses a text once for each thread, so a command that lowered the unit a moment ago parses nothing twice.
///
/// `predecessors` are the units `unit` was lowered against, in that order. `definitions` and `certification` are the checked module of `unit` and the certifier's record of it, which say which declarations are proofs and which are proven total: the one thing a page shows that no surface tree states. `records` are the records of the ones this unit adopts a root of — a root a consumer cannot name and this unit can, whose declarations reach a consumer only through it. An adopted root is another unit, read from another source, so its record is made by this door before its adopter's is.
///
/// # Panics
///
/// Where the unit adopts a root no record of `records` documents: its declarations would be missing from every page that exposes them, with nothing said.
pub fn document_unit(
    source: &UnitSource<'_>,
    unit: &PreparedText,
    predecessors: &[&PreparedText],
    records: &[&Documentation],
    definitions: &curios_core::Definitions,
    certification: &curios_core::Certification,
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
        None,
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
    // What each written name resolved to, read off the lowered items as an editor's go-to-definition reads it: the one resolution a page links by.
    let mentions = unit
        .texts(None, |_| true)
        .iter()
        .flat_map(|text| text.mentions())
        .filter_map(|mention| {
            let curios_core::Global::Authored(referent) = mention.name else {
                return None;
            };
            let span = mention.span;

            Some((
                (
                    span.source.name().cloned(),
                    span.source.file().map(ToOwned::to_owned),
                    span.start,
                    span.end,
                ),
                referent,
            ))
        })
        .collect();

    let units = predecessors
        .iter()
        .copied()
        .chain([unit])
        .collect::<Vec<_>>();

    Ok(Some(document(
        &modules,
        &Scoped::over(&tables),
        &Scoped::over(&exports),
        &mentions,
        &prefix,
        &unit.visible,
        records,
        &units,
        &Checked::of(definitions, certification),
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
    pub minted: Rc<curios_core::Minted>,
    pub foreigns: ForeignStore,
    /// How the entry's names can be written, where — see [`EntryNames`].
    pub names: EntryNames,
    /// See [`PreparedText::lints`].
    pub lints: Vec<curios_core::Lint>,
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
    table: Table<ModuleInfo>,
    public: Table<PublicInterface>,
    audiences: OnceCell<Audiences>,
    mounts: Vec<Mount>,
    visible: Vec<Mount>,
}

impl EntryNames {
    /// The prefixes the program's own unit claims.
    pub fn mounts(&self) -> &[Mount] {
        &self.mounts
    }

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
            (
                Rc::new(writable_paths(&self.table, &self.public, audiences)),
                Rc::new(audiences.module_exports()),
            ),
            &self.mounts,
            &self.visible,
            predecessors,
        )
    }
}

/// `imports` as a lowering recorded them, beside the paths each unit in scope gives a global and the modules it exports — `predecessors`' and then `(paths, modules)`, the asking unit's own — with the prefixes in scope that unit may not name, `own` being the ones it claims and `visible` the ones it may name: what [`PreparedText::spellings`] and [`EntryNames::spellings`] answer.
fn spellings_over(
    imports: &curios_core::Imports,
    (paths, modules): (Rc<curios_core::Paths>, Rc<Vec<curios_core::ModuleExport>>),
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
            modules: Rc::new(earlier.audiences().module_exports()),
            own: false,
        })
        .chain([curios_core::UnitPaths {
            paths,
            modules,
            own: true,
        }])
        .collect();

    curios_core::Spellings {
        imports: imports.clone(),
        units,
        closed,
    }
}

/// The `unused-import` lints: every site no reference resolved through, a re-export excepted, the selectors of one `use` declaration in one lint.
fn unused_imports(sites: Vec<UseSite>, anchoring: &Anchoring) -> Vec<curios_core::Lint> {
    // One declaration's unused selectors are one finding, said once and underlined at the first: a lint for each would repeat the line beneath every one.
    let mut declared: Vec<(UseSite, Vec<String>)> = Vec::new();
    for site in sites
        .into_iter()
        .filter(|site| !site.used && !site.exported)
    {
        // A selector is underlined at itself, a glob over its whole `use` declaration, since the glob itself is one character.
        let name = match &site.what {
            UseSiteKind::Selector(label) => label.to_string(),
            UseSiteKind::Glob(path) => path.clone(),
        };
        let same_line = |first: &UseSite| {
            first.body == site.body
                && matches!(first.what, UseSiteKind::Selector(_))
                && matches!(site.what, UseSiteKind::Selector(_))
                && first.line.as_ref().map(|line| (line.start, line.end))
                    == site.line.as_ref().map(|line| (line.start, line.end))
        };
        match declared.last_mut() {
            Some((first, names)) if same_line(first) => names.push(name),
            _ => declared.push((site, vec![name])),
        }
    }
    declared
        .into_iter()
        .map(|(site, names)| {
            let at = site
                .line
                .as_ref()
                .zip(site.span.as_ref())
                .and_then(|(line, span)| anchoring.line(site.body, line, span));
            match names.as_slice() {
                [name] => curios_core::Lint::unused(
                    curios_core::LintKind::UnusedImport,
                    at,
                    "import",
                    name,
                    "delete it",
                ),
                [rest @ .., last] => {
                    let listed = rest
                        .iter()
                        .map(|name| format!("`{name}`"))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let mut said = Said::line("unused imports");
                    said.stating(format_args!("{listed} and `{last}` are never used"));
                    said.stating("delete them");
                    curios_core::Lint {
                        kind: curios_core::LintKind::UnusedImport,
                        said,
                        at,
                    }
                }
                [] => unreachable!("a declaration's group holds the site it began with"),
            }
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
            definitions: unit.core,
            entrypoint: entry.expect("an entry source's parse holds its final term"),
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
