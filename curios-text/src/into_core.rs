//! Lowering one unit's surface modules to a [`curios_core::Module`], over the units already lowered.
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

mod spell;
pub use spell::*;

mod scoped;
use scoped::*;

mod document;
pub use document::*;

mod head;
use head::*;

mod witness;
use witness::*;

mod mention;
pub use mention::*;

mod revision;
use revision::*;

mod reachability;
use reachability::*;

use {
    crate::{
        Apply, Argument, Entrypoint, Error, FuncTypeParam, GroupItem, Label, LetSignature, Lint,
        LintedBinder, Module, Name, RootSource, StructLit, StructLitEntry, Subterm, Term, TopItem,
        TupleField, UseGroup, func_sugar_type_params, ordered, tree_of,
    },
    curios_abi::{ForeignFunction, ForeignStore},
    curios_archive::Bytes,
    curios_document::Documentation,
    curios_utilities::{
        Digest, Filed, Mount, Object, Persistent, Plicity, Qualifier, Recorded, Report, RootKind,
        Source, Span, SyntaxRegistry, grown,
    },
    std::{
        cell::{OnceCell, RefCell},
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        rc::Rc,
        sync::Arc,
    },
};

#[cfg(test)]
mod bang_tests;
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

/// Every absolute path a unit gives a global — each declaration of its own at its own path, each export at the path its module gives it, whichever unit declares what it exports — with who may write it. A function of the unit's own tables and the audiences it decided, so it is made once for the unit ([`PreparedText::paths`]): which paths a reader's unit may name, and which it reaches relatively, are the reader's, read where a report spells a name ([`spell()`]).
fn writable_paths(
    table: &Table<ModuleInfo>,
    public: &Table<PublicInterface>,
    audiences: &Audiences,
) -> curios_core::Paths {
    curios_profile::profile!("writable_paths");
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

/// What a report spells a unit's own names by ([`curios_core::Lexicon`]), made where a report first asks and kept: whole, off the unit's lowered module, or as the lexicon of the unit it was lowered over revised at the names its lowering made or dropped ([`curios_core::Module::lexicon_over`]).
///
/// **Shared with the units lowered over it**, as a unit's paths are ([`PathsCell`]), so a session that reports at every keystroke over one baseline reads that baseline's declarations once. A cell holds the unit's items as the version they are, which shares them; a unit whose items are a vector of their own is read where its cell is made, since copying them would read them all.
struct LexiconCell {
    module: curios_core::Module,
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
                module: curios_core::Module::default(),
                places: curios_core::Places::default(),
                earlier: RefCell::new(None),
                made: OnceCell::from(Rc::new(unit.core().lexicon())),
            };
        }

        Self {
            module: curios_core::Module {
                items: items.clone(),
                ..curios_core::Module::default()
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
        module: curios_core::Module,
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
            module,
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
pub(super) struct Loaded {
    module: Qualifier,
    /// What the module's items declare: read off its tree, or a baseline's scan of the same text.
    pub heads: Rc<Vec<Headed>>,
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
    /// The `foreign` rows this unit declares. Collected by the same walk that lowers its items, and carried here rather than dropped: a unit that declares one has to reach the link, and the prelude declaring none is a fact about the prelude, not about the shape.
    foreigns: ForeignStore,
    #[archived_with(curios_utilities::InSharedKeyOrder)]
    table: Table<ModuleInfo>,
    #[archived_with(curios_utilities::InSharedKeyOrder)]
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
    core: curios_core::Module,
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
    /// **Stored apart from the unit**, each body's imports and each module's offer an object ([`PreparedText::imports_recorded`]): they are read by the unit's own reports and its documentation, and by nothing compiled after it.
    #[archived_with(curios_archive::Skip)]
    imports: Recorded<curios_core::Imports>,
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
    /// **Stored apart from the unit**, each source's texts an object ([`PreparedText::anchors_recorded`]): a report placed in one of the unit's declarations reads them, and a lowering over the unit, and no compilation that reports nothing.
    #[archived_with(curios_archive::Skip)]
    anchors: Recorded<curios_core::Anchors>,
    /// What each source declaration lowered to beside its items, with what it asked of its scope, held by pointer since a unit lowered over this one keeps the ones that stand, and what each of the unit's files declared — see `lowered`.
    ///
    /// **The unit's baseline half: what a later lowering of the same sources reads, and nothing compiled after this unit does.** So it is no part of what the unit is stored as for its successors. It is stored beside that, each record and each file's scan an object ([`PreparedText::baseline_digests`]), handed back where the unit is restored ([`PreparedText::with_baseline`]), and read when a lowering first asks.
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

    /// The binders elaboration credited among those written in the definitions named `names`, each by its definition's name and its place: what a declaration declaring them has a later compile credit where it takes the declaration's elaboration whole.
    pub fn credited_in(&self, names: &[curios_core::Global]) -> Vec<(curios_core::Global, u32)> {
        names
            .iter()
            .flat_map(|name| {
                let at = |ordinal| LintedBinder {
                    declaration: Some(*name),
                    ordinal,
                };
                self.credited
                    .range(at(0)..=at(u32::MAX))
                    .map(|binder| (*name, binder.ordinal))
            })
            .collect()
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

    /// The lowered module: what its elaboration reads, each item of a restored unit read out of its bytes where it is first asked for.
    pub fn core(&self) -> &curios_core::Module {
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
    fn declared_at(&self, place: curios_core::Place) -> curios_core::Declared {
        let member = match place {
            curios_core::Place::Let(_) => 0,
            curios_core::Place::Member { member, .. } => member as usize,
        };

        self.core.items.declared(place.item())[member]
    }

    /// The entry this unit's lowering wrote for the nominal declaration `name`, read at the former that carries it: the one lowered item, read out of its bytes now where the unit was restored and nothing has read it before. `None` for a name the unit declares no nominal under.
    pub(crate) fn nominal(&self, name: &curios_core::Global) -> Option<&curios_core::Nominal> {
        let place = self.places().place(name)?;
        if !self.declared_at(place).is_former() {
            return None;
        }

        self.lowered_item(place.item())
            .nominals()
            .into_iter()
            .find_map(|(held, nominal)| (held == name).then_some(nominal))
    }

    /// What this unit declares `name` as, with nothing read. `None` for a name the unit does not declare.
    pub fn declared_as(&self, name: &curios_core::Global) -> Option<curios_core::Declared> {
        self.places()
            .place(name)
            .map(|place| self.declared_at(place))
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
        let declared = self.declared_at(place);
        let body = match (declared, place) {
            (curios_core::Declared::Authored, curios_core::Place::Let(_)) => {
                match self.lowered_item(place.item()) {
                    curios_core::Item::Let(definition) => Some(&definition.body),
                    curios_core::Item::Rec(_) => None,
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

    /// The unit's baseline half, read out of its bytes now where the unit was restored and nothing has asked before.
    fn half(&self) -> &BaselineHalf {
        self.baseline
            .get()
            .unwrap_or_else(|error| panic!("a unit's baseline half failed to restore: {error}"))
    }

    /// What this unit's baseline half was restored from, where it was: its own part and the object of each entry, what it is stored as again.
    pub fn baseline_stored(&self) -> Option<(&Bytes, &[Object])> {
        self.baseline.as_stored()
    }

    /// The bytes of this unit's baseline half's own part, stored apart from what the unit is for its successors: the tables of each module body and each witness, each record and scan being an object of its own.
    pub fn baseline_table(&self) -> Result<Vec<u8>, String> {
        curios_archive::to_bytes(self.half()).map(|bytes| bytes.to_vec())
    }

    /// The digest of each record and each scan of this unit's baseline half, in the order the unit names them: what a later lowering of the same sources keeps this unit's lowerings by, each an object of its own.
    pub fn baseline_digests(&self) -> Result<Vec<Digest>, String> {
        self.half().digests()
    }

    /// The records and scans of this unit's baseline half that `wanted` asks for, as the objects a store keeps them as.
    pub fn baseline_objects(
        &self,
        wanted: &dyn Fn(&Digest) -> bool,
    ) -> Result<Vec<Object>, String> {
        self.half().objects(wanted)
    }

    /// This unit with `table` and `entries` as its baseline half ([`PreparedText::baseline_table`], [`PreparedText::baseline_objects`]), read when a lowering over the unit first asks: what a unit restored without one is given, since restoring it leaves the half empty.
    pub fn with_baseline(mut self, table: impl Into<Bytes>, entries: Vec<Object>) -> Self {
        self.baseline = Recorded::stored(table, entries);
        self
    }

    /// This unit's import scopes as it holds them, stored apart from what the unit is for its successors.
    pub fn imports_recorded(&self) -> &Recorded<curios_core::Imports> {
        &self.imports
    }

    /// Where this unit's declarations lie as it holds them, stored apart as its import scopes are, each source's texts an object.
    pub fn anchors_recorded(&self) -> &Recorded<curios_core::Anchors> {
        &self.anchors
    }

    /// This unit with `imports` as its import scopes and `anchors` as where its declarations lie, each its own part and the objects of its entries ([`PreparedText::imports_recorded`], [`PreparedText::anchors_recorded`]), each read when something first asks: what a restored unit is given, since restoring it leaves it with neither.
    pub fn with_reported(
        mut self,
        imports: (impl Into<Bytes>, Vec<Object>),
        anchors: (impl Into<Bytes>, Vec<Object>),
    ) -> Self {
        self.imports = Recorded::stored(imports.0, imports.1);
        self.anchors = Recorded::stored(anchors.0, anchors.1);
        self
    }

    /// This unit's lowered items, each an object stored on its own, in the order of the items: the one an item was restored from where the unit holds it as it was read, of this unit or of a baseline it is a version of, and its archive, made and digested now, otherwise.
    pub fn items_objects(&self) -> Result<Vec<Object>, String> {
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
                None => {
                    curios_archive::to_bytes(&items[place]).map(|made| Object::of(made.to_vec()))
                }
            })
            .collect()
    }

    /// This unit with `objects` as its lowered items ([`PreparedText::items_objects`]), each read when something first asks for it: what a restored unit is given, since restoring it leaves its module without one. `Err` is not one for each item the unit outlines.
    pub fn with_items(mut self, objects: Vec<Object>) -> Result<Self, String> {
        self.core = self.core.with_items(objects)?;

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
            let names = items
                .places()
                .map(|place| items.names(place).into_iter().copied().collect::<Vec<_>>());
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

    /// What a report spells this unit's own names by: its lowered module's lexicon ([`curios_core::Module::lexicon`]), made once for the unit, or revised of the unit it was lowered over.
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
        curios_profile::profile!("resolved::of");
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
    declarations: Vec<HeldRecord>,
    induct_decls: BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: BTreeMap<curios_core::Global, curios_core::StructDecl>,
    concepts: BTreeMap<curios_core::Global, curios_core::ConceptDecl>,
    witnesses: BTreeSet<curios_core::Global>,
    tests: Vec<curios_core::Global>,
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
                curios_profile::profile!("resolve_use");
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
            // A test takes no parameters, so it is not function sugar — but it lowers to a `() -> Test({})` thunk, because `Test/main` holds the whole schedule and must force only the one it selected. The output is emitted as core directly off the registry slot, since a synthesized `Var` carries the resolved identity and nothing here depends on the declaration being importable.
            TopItem::Test(test) => {
                let name = curios_core::Global::Authored(context.prefixed(&test.label));
                context.record_import_scope(Some(&name));
                let lower = Lowerer::new(context, Some(name));
                let output = curios_core::Term::apply(
                    curios_core::Term::var(curios_core::Var::free(curios_core::Free::global(
                        context.syntax().test.test_type.qualifier(),
                    ))),
                    [curios_core::Term::tuple_type_unit()],
                );
                // The empty telescope is not vestigial: it is the thunk. A test lowers to `() -> Test({})` so `Test/main` can hold it unforced and run only the one it selected.
                let type_ = lower.func_type_under(&func_sugar_type_params(&[]), || Ok(output))?;
                let body = curios_core::Term::func_marked(
                    Vec::<(Plicity, curios_core::Free, curios_core::Term)>::new(),
                    lower.value(&test.body)?,
                );
                // The thunk owns the full authored body extent; its interior keeps each bang's narrower extent for diagnostics.
                let body = match test.body.span() {
                    Some(span) => context.placed(span, body),
                    None => body,
                };
                let name = curios_core::Global::Authored(context.prefixed(&test.label));
                lowered.tests.push(name);
                lowered.items.push(FlatItem::Let(FlatLet {
                    kind: curios_core::DefinitionKind::Test,
                    span: test.label.span().cloned(),
                    name,
                    island: context.island(),
                    type_,
                    body,
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
                // Step 1: type bindings as one rec group. An inductive's type binding wraps an intrinsic `InductType` normal form in a `Func` over its type parameters and indices (so `Result(Bytes, Nat)` beta-reduces to `InductType { Result, [Bytes, Nat] }` and `Vec(Bytes, 3)` to `InductType { Vec, [Bytes], [3] }`), and its shape is recorded in the inductive registry.
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

                        let mut head_prefixes = lower.prefixes(&head_binders);
                        let param_tys = u
                            .params
                            .iter()
                            .enumerate()
                            .map(|(i, param)| {
                                let ty =
                                    head_prefixes.under(i, || lower.input_type(&param.type_))?;
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
                                let ty = head_prefixes.under(seen, || lower.input_type(t))?;
                                Ok((index_binders[i].1, ty))
                            })
                            .collect::<Result<Vec<_>, Error>>()?;
                        drop(head_prefixes);

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
                                let scope = [param_binders, &payload_binders].concat();
                                let mut prefixes = lower.prefixes(&scope);
                                let fields = c
                                    .payload
                                    .iter()
                                    .enumerate()
                                    .map(|(i, param)| {
                                        let ty = prefixes.under(param_binders.len() + i, || {
                                            lower.input_type(&param.type_)
                                        })?;
                                        Ok((payload_binders[i].1, ty))
                                    })
                                    .collect::<Result<Vec<_>, Error>>()?;

                                let target = prefixes.under(scope.len(), || {
                                    c.target
                                        .iter()
                                        .flatten()
                                        .map(|t| lower.term(t))
                                        .collect::<Result<Vec<_>, Error>>()
                                })?;
                                drop(prefixes);

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
                                constructors: constructors.into(),
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
                // Making this conditional is not the two-line change the neighbours make it look like, and both halves were measured rather than reasoned. `mentions_itself` reads `free_vars_shared()`, but an inductive's recursion lives in its *registry entry* — `InductType` holds `name: Global` as a field, not a `Var` — so the test is false for every inductive, recursive ones included, and reaching for it lowers a recursive inductive such as `/std/Cli/Values` as a `Let` that loses `elaborate_module_rec`'s `context.assume` and leaves its own name unbound while the registry telescopes rebuild. Reading the reach through `references::induct_free_vars` instead clears that and then fails at the first universe-polymorphic inductive of a group with `universe instance has 1 arguments but its scheme expects 0`: the group is also where a universe-polymorphic inductive's levels are generalized, and as a `Let` the scheme comes out monomorphic while occurrences still pass a level.
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
                        let mut prefixes = lower.prefixes(&binders);
                        let param_tys = written
                            .iter()
                            .enumerate()
                            .map(|(i, t)| {
                                let ty = prefixes.under(i, || lower.input_type(t))?;
                                Ok((plicities[i], binders[i].1, ty))
                            })
                            .collect::<Result<Vec<_>, Error>>()?;
                        let output = prefixes.under(binders.len(), || lower.term(&output_type))?;
                        drop(prefixes);
                        let payload_binders = &binders[u.params.len()..];
                        let param_binders = &binders[..u.params.len()];
                        // Erasure is sort-driven: `erase_func` drops the same proof/type payload params that `erase_variant` drops from the tuple — the constructor function's arity and its injected variant's arity stay in lockstep.
                        let ctor_type =
                            curios_core::Term::func_type_marked(param_tys.clone(), output);
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
                    let mut prefixes = lower.prefixes(&param_binders);
                    let param_tys = s
                        .params
                        .iter()
                        .enumerate()
                        .map(|(i, param)| {
                            let ty = prefixes.under(i, || lower.input_type(&param.type_))?;
                            Ok((param.plicity, param_binders[i].1, ty))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;
                    drop(prefixes);
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
                    let field_scope = [&param_binders[..], &field_binders].concat();
                    let mut prefixes = lower.prefixes(&field_scope);
                    let field_tys = s
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, field)| {
                            let ty = prefixes.under(param_binders.len() + i, || {
                                lower.input_type(&field.param.desugared_type())
                            })?;
                            Ok((field.plicity, field_binders[i].1, ty))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;
                    drop(prefixes);

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

                    // The type-former: `Pair : (A : Type, B : Type) -> Type` whose body is the `StructType` normal form (the bare node when parameterless), so `Pair(Nat, Bytes)` reduces to `StructType { Pair, [Nat, Bytes] }`. No value constructor.
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
                    let mut prefixes = lower.prefixes(&param_binders);
                    let param_tys = concept
                        .params
                        .iter()
                        .enumerate()
                        .map(|(i, param)| {
                            let ty = prefixes.under(i, || lower.input_type(&param.type_))?;
                            Ok((param.plicity, param_binders[i].1, ty))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;
                    drop(prefixes);
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
                    let field_scope = [&param_binders[..], &field_binders].concat();
                    let mut prefixes = lower.prefixes(&field_scope);
                    let field_tys = concept
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, field)| {
                            let ty = prefixes.under(param_binders.len() + i, || {
                                lower.input_type(&field.desugared_type())
                            })?;
                            // A superclass edge is the field telescope's `use` member.
                            let mark = match field.is_super() {
                                true => Plicity::Witness,
                                false => Plicity::Explicit,
                            };
                            Ok((mark, field_binders[i].1, ty))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;
                    drop(prefixes);

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
                    // The one field each wrapper states, read off the record's payload at the parameters: opening the whole telescope for each would make a concept's wrappers cost the square of its fields.
                    let fields = arity.terminal();
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

                        let field_type = fields
                            .field_type_beneath(
                                &param_vars,
                                &witness,
                                &curios_core::Field::Struct(name, index),
                            )
                            .expect("a concept's own field index is within its record telescope");
                        let method = curios_core::Term::proj_struct(witness, name, index);

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
///
/// `settled` says that `predecessors` are the units `baseline` was lowered against: what an edit moved is then found from the edit, and a declaration no edit can have reached is kept unasked. Where it is false, every declaration is asked.
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

/// [`into_core_unit`], with the final term an entry source closes with, lowered beside the unit.
fn lower_unit(
    source: &UnitSource<'_>,
    predecessors: &[&PreparedText],
    syntax: &SyntaxRegistry,
    baseline: Option<(&PreparedText, bool)>,
) -> Result<(PreparedText, Option<curios_core::Entrypoint>), Error> {
    curios_profile::profile!("into_core_unit");
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
    // What the baseline kept of each file. A baseline restored from a store holds its half as bytes until this, the lowering's first read of it.
    let baseline_scans =
        curios_profile::profile!("into_core_unit::baseline" => baseline.map(PreparedText::scans));

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

    // Where the units in scope are the ones the baseline was lowered against, the export view is the baseline's with what the edit reached resolved again; where one moved, every module's is resolved again, since a `pub use` may read it.
    let settled = over
        .filter(|(_, settled)| *settled)
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
        entered: baseline.as_ref().and_then(Baseline::entered),
        baseline: baseline.as_ref(),
        uses: baseline
            .as_ref()
            .map(|baseline| baseline.unit().half().uses.clone())
            .unwrap_or_default(),
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
        if lowered.passes(&mount.prefix) {
            continue;
        }

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
        curios_profile::profile!("name_witnesses");
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
            witnesses: &mut witnesses,
            minted: &mut shared.minted.borrow_mut(),
            imports: &mut shared.imports.borrow_mut(),
            lints: &mut shared.lints.borrow_mut(),
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
            referent: spelled(&unseen.referent),
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
                    .filter(|(_, kind, _)| kind.declared().is_former())
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
            placed: &|extent| shared.anchoring.resolved(extent),
            entry: entry.as_ref(),
            table: &table,
            public: &public,
            own: &own,
            mod_spans: &mod_spans,
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
    }

    // This unit's own items alone. A predecessor reaches later stages as an *environment* they are seeded from — `Globals` at the certifier, a replayed context at elaboration and erasure — and copying its items into every compilation only ever existed so those stages could then skip them again by index. See `documentation/design/compilation/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md`.
    //
    // As they are written. No item is moved for another's sake: elaboration asks for each declaration when it is read, wherever it sits, refuses the ones that need one another by name, and stores the module in an order of its own (`curios_elab`'s `Demand`, `curios_core::Module::ordered`).
    let made = flat_items
        .into_iter()
        .map(FlatItem::into_core)
        .collect::<Vec<_>>();
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

    // What each body's declarations and `use` declarations were found to leave unused: the baseline's, with each body the walk passed found again and each it no longer holds taken away.
    let mut body_lints = baseline
        .as_ref()
        .map(|baseline| baseline.unit().half().lints.clone())
        .unwrap_or_default();
    {
        let mut found = shared.lints.take();
        let mut imports = BTreeMap::<Qualifier, Vec<UseSite>>::new();
        for site in shared.sites.take() {
            imports.entry(site.body).or_default().push(site);
        }
        for body in shared.closed() {
            let lints = unused_imports(imports.remove(&body).unwrap_or_default())
                .into_iter()
                .chain(found.remove(&body).unwrap_or_default())
                .collect::<Vec<_>>();
            match lints.is_empty() {
                true => {
                    body_lints.remove(&body);
                }
                false => body_lints.insert(body, Rc::new(lints)),
            }
        }
        // The entry's term is lowered in the compilation root's body, which a unit with no entry does not walk.
        for (body, lints) in found {
            let mut held = body_lints
                .get(&body)
                .map(|held| (**held).clone())
                .unwrap_or_default();
            held.extend(lints);
            body_lints.insert(body, Rc::new(held));
        }
        for body in &removed {
            body_lints.remove(body);
        }
        // A lint carried of a body the walk passed lies in the text its file is read as now: the source a restored baseline read it from is another, though its text is the one.
        if baseline.is_some() {
            let read = modules
                .values()
                .filter_map(|loaded| loaded.source.clone())
                .filter_map(|source| Some((source.path.clone()?, source)))
                .collect::<HashMap<_, _>>();
            let now = |span: &Span| {
                let source = read.get(span.source.path.as_ref()?)?;
                (!Arc::ptr_eq(source, &span.source) && source.text == span.source.text)
                    .then(|| Span::new(Arc::clone(source), span.start, span.end))
            };
            let moved = |lint: &Lint| {
                lint.report
                    .span
                    .as_ref()
                    .is_some_and(|span| now(span).is_some())
                    || lint
                        .report
                        .notes
                        .iter()
                        .any(|note| now(&note.span).is_some())
            };
            let carried = body_lints
                .iter()
                .filter(|(_, lints)| lints.iter().any(moved))
                .map(|(body, _)| *body)
                .collect::<Vec<_>>();
            for body in carried {
                let Some(lints) = body_lints.get(&body) else {
                    continue;
                };
                let lints = lints
                    .iter()
                    .map(|lint| {
                        let mut lint = lint.clone();
                        if let Some(span) = lint.report.span.as_ref().and_then(now) {
                            lint.report.span = Some(span);
                        }
                        for note in &mut lint.report.notes {
                            if let Some(span) = now(&note.span) {
                                note.span = span;
                            }
                        }
                        lint
                    })
                    .collect::<Vec<_>>();
                body_lints.insert(body, Rc::new(lints));
            }
        }
    }

    // Over a baseline, the witnesses and the tests are the baseline's with what the walk changed, the tests in the order their items stand.
    let (witnesses, tests) = match &baseline {
        Some(baseline) => {
            let earlier = baseline.unit().core();
            let mut held = earlier
                .witnesses
                .iter()
                .filter(|name| !gone_names.contains(*name) && !renames.contains_key(*name))
                .copied()
                .collect::<BTreeSet<_>>();
            held.extend(renames.values().copied());
            held.extend(witnesses);
            let places = places
                .get()
                .expect("a version holds where it places each name");
            let mut tested = earlier
                .tests
                .iter()
                .filter(|name| !gone_names.contains(*name))
                .copied()
                .chain(tests)
                .collect::<BTreeSet<_>>()
                .into_iter()
                .filter_map(|name| Some((places.place(&name)?.item(), name)))
                .collect::<Vec<_>>();
            tested.sort_by_key(|(place, _)| *place);
            (held, tested.into_iter().map(|(_, name)| name).collect())
        }
        None => (witnesses, tests),
    };

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
                        .copied()
                        .zip(&view.facts(place).definitions)
                })
                .collect::<Vec<_>>()
        };
        let aliases = whole_aliases.clone().unwrap_or_else(|| {
            let view = view.expect("a unit made over a baseline holds where it places each name");
            Rc::new(aliases(items.places().map(|place| {
                (
                    items.names(place).into_iter().copied().collect(),
                    view.facts(place),
                )
            })))
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
            Rc::new(aliases(items.places().map(|place| {
                (
                    items.names(place).into_iter().copied().collect(),
                    view.facts(place),
                )
            })))
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
                &|extent| shared.anchoring.resolved(extent),
                &|module, label| table.get(module)?.get_child(label),
                &mod_spans,
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
                curios_core::Module {
                    items: items.clone(),
                    ..curios_core::Module::default()
                },
                places
                    .get()
                    .cloned()
                    .expect("a unit lowered over a baseline holds where its items hold each name"),
            )))
        }
        _ => OnceCell::new(),
    };

    let unit = PreparedText {
        mounts: own.clone(),
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
        core: curios_core::Module {
            items,
            mounts: own,
            witnesses,
            tests,
        },
        minted: Rc::new({
            let mut minted = shared.minted.into_inner();
            minted.entry = entry_mints;
            minted
        }),
        imports: Recorded::held(shared.imports.into_inner()),
        visible: visible_mounts,
        broken,
        lints: ordered(
            body_lints
                .iter()
                .flat_map(|(_, lints)| lints.iter().cloned())
                .chain(dead)
                .collect(),
        ),
        credited: BTreeSet::new(),
        reached,
        anchors: Recorded::held(shared.anchoring.into_anchors()),
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

/// Lower a whole [`Entrypoint`] with nothing in scope, as `curios-text`'s own stage tests do.
pub fn into_core(
    entrypoint: &Entrypoint,
    loader: &RootSource,
    syntax: &SyntaxRegistry,
) -> Result<(curios_core::Program, Rc<curios_core::Minted>, ForeignStore), Error> {
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
/// **Built where a command asks for it, and carried by no unit.** Two commands read a record, so no compilation builds one and no image holds one. What a page shows is still read off the compilation that built the library: the direct interface, the export view and what each written name resolved to are the ones `unit`'s lowering stored, and the surface tree is discovery's, read again through `source`, which parses a text once for each thread, so a command that lowered the unit a moment ago parses nothing twice.
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

            Some(((span.source.path.clone(), span.start, span.end), referent))
        })
        .collect();

    Ok(Some(document(
        &modules,
        &Scoped::over(&tables),
        &Scoped::over(&exports),
        &mentions,
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
    pub minted: Rc<curios_core::Minted>,
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
    table: Table<ModuleInfo>,
    public: Table<PublicInterface>,
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
