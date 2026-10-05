//! Name types for the `core` stage.
//!
//! A name here distinguishes one binding from another and renders for a human. It is not a place to store facts: nothing branches on a name's characters, its prefix, or its collation order. Where a consumer needs structure — which module a definition belongs to, which inductive a constructor came from — that structure is carried as a value by the site that knew it, never recovered by taking a name apart.
//!
//! The rule holds because the *capability* is absent, not because every site remembers it. A spelling carrying structured facts is an undocumented wire format between stages, each fact recovered by a hand-rolled parser whose correctness rests on an invariant stated nowhere near the parse; `README.md` records what that cost. The types below unmerge the facts: [`Free`] discriminates global from local, [`Global`] discriminates an authored path from an anonymous witness, and [`Mint`] separates a binder's identity from its display hint. No path leads from a `Free` to a `&str` except through the printer, so reintroducing behavior-from-spelling means adding a method to a name type — which cannot happen by accident and appears in review as what it is. That is the property to preserve when extending this vocabulary.
//!
//! **A minted identity is private to the unit that minted it.** A local's index and a metavariable's id are positions in counters that start at zero for each unit ([`Minted`]); a witness's ordinal counts within the module that declares it ([`WitnessId`]). So none may outlive the compilation that assigned it: a scope remembers a binder's hint and never its identity (`Label`), a stored term carries no local and no metavariable (`validate_stored_identities`), and the kernel refuses a module mentioning a local it was not handed (`free_locals_outside`). What crosses from one compilation to another is a [`Global`], whose meaning is its path.

#[cfg(test)]
mod tests;

use {
    crate::UniverseSeed,
    curios_utilities::{InfixOp, Qualifier, Symbol, name},
    std::{cmp::Ordering, collections::BTreeMap, fmt, hash},
};

name!(Atom; archive);

/// A witness's identity: the module that declares it, and its ordinal within that module.
///
/// **Not a program-global counter, and that is the point.** Two units elaborated in separate compilations both mint from zero, so a bare ordinal means something only in the compilation that assigned it — the positional identity a stored unit may not carry. Pairing the ordinal with its declaring module makes two witnesses disjoint by the argument mount disjointness already carries: modules are disjoint within a mount and mounts are disjoint across units, so restoring two independently compiled units together cannot alias one onto the other.
///
/// It also needs no floor: a counter seeded above the archived prelude's watermark would tie a unit's identities to *where it sat*, where per-module ordinals depend on nothing but the unit itself.
///
/// **The module rather than the mount, which is finer than disjointness needs.** It is what lets a witness be *placed*: `by_item` keys an import scope by `Global`, and a documentation page asks which declarations belong to the module it is rendering. A mount answers `/std` for every witness in the standard library, which is no answer at all; the declaring module answers `/std/Tuple`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub struct WitnessId {
    module: Qualifier,
    ordinal: u32,
}

impl WitnessId {
    /// The `ordinal`th witness declared in `module`.
    pub fn new(module: Qualifier, ordinal: u32) -> Self {
        Self { module, ordinal }
    }

    /// The module that declares it.
    pub fn module(&self) -> &Qualifier {
        &self.module
    }
}

impl fmt::Display for WitnessId {
    /// Module-qualified, naming the module a reader would look in rather than the mount.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}/witness@{}", self.module.join(), self.ordinal)
    }
}

/// A local binder's identity: a dense index, plus the hint it renders under — what its author called it, and none where the compiler introduced the binder or its author left it unnamed.
///
/// The index alone is the identity. The hint is display metadata, excluded from equality, ordering, and hashing exactly as a [`Term`](crate::Term)'s span and a [`Scope`](crate::Scope)'s binder names already are — so a hint can neither make two binders collide nor split one binder in two. Carrying it on the identity rather than only at the binding site means a diagnostic can name a variable wherever the occurrence turns up, instead of recovering the written name by cutting a minted spelling apart.
#[derive(Debug, Clone, Copy)]
#[curios_archive::archived]
pub struct Mint {
    index: u32,
    hint: Option<Symbol>,
    /// Where the binder sits among its declaration's written binders, when the lowering wrote it — diagnostic metadata excluded from identity as the hint is, which the scope closing over the binder remembers so a lint can be credited with a proof's reads.
    written: Option<u32>,
}

impl Mint {
    pub(crate) fn new(index: u32, hint: Option<&str>) -> Self {
        Self {
            index,
            hint: hint.map(Symbol::new),
            written: None,
        }
    }

    /// What this binder was called where it was written, if anything — a rendering aid with no bearing on identity.
    ///
    /// Crate-private, so no path leads from a `Free` to a spelling outside the stage that renders it. The variant itself stays public — downstream code holds and compares binders — but it cannot look inside one. The index has no accessor at all: nothing ever needed to read it, only to compare it.
    pub(crate) fn hint(&self) -> Option<&'static str> {
        self.hint.map(|hint| hint.as_str())
    }

    /// The hint as the interned symbol it is kept as, for a scope that remembers it without re-interning its spelling.
    pub(crate) fn hint_symbol(&self) -> Option<Symbol> {
        self.hint
    }

    /// Where the binder sits among its declaration's written binders, when the lowering wrote it.
    pub(crate) fn written(&self) -> Option<u32> {
        self.written
    }
}

impl PartialEq for Mint {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Eq for Mint {}

impl PartialOrd for Mint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Mint {
    fn cmp(&self, other: &Self) -> Ordering {
        self.index.cmp(&other.index)
    }
}

impl hash::Hash for Mint {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

/// A top-level definition's identity: an authored path, or a compiler-generated definition that has no source name at all.
///
/// The two cases are a sum rather than a qualifier with an optional disambiguator, because a witness's declaring module is not its name. Folding both into one field would make [`Qualifier`] mean "module plus the item's own name" for one case and "the declaring module alone" for the other — one field with two readings, which is the defect this vocabulary exists to remove.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub enum Global {
    /// A name a programmer wrote, at its resolved module path.
    Authored(Qualifier),
    /// A `satisfy` declaration. Witnesses are anonymous by design, so this is an identity rather than a manufactured name; the declaring module a diagnostic reports comes from `Definition::island`.
    ///
    /// The identity is the declaring module and an ordinal within it — see [`WitnessId`]. Nothing else distinguishes two witnesses, which is why the module has to be part of it: two units minting bare ordinals from zero would alias, and aliasing one would silently rebind a coherence-table entry.
    Witness(WitnessId),
}

impl Global {
    /// This name's canonical flattened spelling, which a diagnostic names a declaration by; the registries are keyed by the [`Global`] itself.
    pub fn symbol(&self) -> String {
        self.to_string()
    }

    /// The module path a programmer wrote this name at, if they wrote one.
    pub fn qualifier(&self) -> Option<&Qualifier> {
        match self {
            Global::Authored(qualifier) => Some(qualifier),
            Global::Witness(_) => None,
        }
    }

    /// The written path as a test's report line, the runner's records and `wonder tests` all spell it — empty for a witness, which is anonymous by design.
    ///
    /// Shared rather than restated, because the tail baked into the test program, the records the CLI filters on and the paths `wonder tests` prints are one identity read three times: `documentation/usage.md` states that a path means the same thing whichever subcommand asks, and this is what makes that true rather than merely intended.
    pub fn path(&self) -> String {
        self.qualifier().map(Qualifier::join).unwrap_or_default()
    }
}

/// Who an inserted metavariable's call site was applying — the identity a report names, rather than the text it renders to.
///
/// **Three facts, three variants.** Written into one `String` slot — a function's flattened [`Global::symbol`], a witness's, an operator's bare symbol — they would be told apart on the error path by trying `InfixOp::from_symbol` and scanning every registered witness for a matching *rendered* name: behavior recovered from spelling, which this module exists to prevent. And a callee captured as text would never meet the shorten map or the import spellings, so a report could name `/sys/Nat/to_byte` — a path no program is permitted to write.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum CalleeId {
    /// A function or constructor the program names — a top-level definition, or a local binder holding one. A local carries no path to shorten, so only the global case meets the spelling tables.
    Function(Free),
    /// A constructor, carried as the declaration it belongs to together with its own tag, because that pair is how a program writes one: a bare tag is not a name in scope, and the declaration alone does not say which constructor.
    Constructor { owner: Global, tag: String },
    /// A witness, which is anonymous by design: carried as its own identity, and named by concept and key when a report renders it.
    Witness(Global),
    /// An infix operator, which has no path to render at all.
    Operator(InfixOp),
    /// A head with no name to report: a projection out of a recursive group, or a computed function. Reported as `<function>`.
    Anonymous,
    /// A structure, where a literal of it left a hidden field to be filled: the slot is a field and is written in braces, which a report must say where a function's would say an argument.
    Structure(Global),
}

/// What one unit's lowering minted in each space elaboration goes on minting in — the whole of what the lowering hands elaboration beside the module: counts within the unit, which the elaborator's counters start above so nothing it mints is an identity a lowered term already holds, and a seed per universe level, which the solver starts from.
///
/// **Within the unit, never across units.** No stored term carries a local, a metavariable or a universe metavariable (`validate_stored_identities`, `validate_universes`), so nothing a predecessor minted can meet this unit's walk, and every unit's counters start at zero. That is what keeps a unit's stored bytes independent of what was compiled before it.
///
/// **Beside the module, never on it.** A seed is read once, where elaboration seeds its solver, and means nothing to any stage after it; carried on the [`Module`](crate::Module) every stage shares, it would be a field each later one had to prove empty, and one the certifier could reach.
#[derive(Debug, Clone, Default, PartialEq)]
#[curios_archive::archived]
pub struct Minted {
    /// Binder identities. A lowered scope is closed before the elaborator sees it, so the ones that survive into a lowered term are the unbound names — each lowered to a free local that elaboration must report rather than find bound.
    pub binders: usize,
    /// Term metavariables, one per hole the lowering left for elaboration to solve.
    pub metavariables: usize,
    /// One seed per universe level the lowering minted, by its id: the role the solver reads the level's provenance off, and where it was written. Every copy of one written `Type` shares a level, so there are as many seeds as written types, not as occurrences.
    pub universes: Vec<UniverseSeed>,
}

/// A free variable's identity: a top-level definition, or a binder some scope opened.
///
/// The distinction is a discriminant rather than a spelling convention. Asking "is this a local?" is a `matches!` — exact, and impossible to get wrong the way a marker character in a string could be.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub enum Free {
    Global(Global),
    Local(Mint),
}

impl Free {
    /// A local binder with identity `index`, rendering as `hint`.
    ///
    /// An index means something within the unit that minted it and nowhere else: the lowering and the elaborator share one space per unit, the elaborator's counter starting above the lowering's count ([`Minted::binders`]), and no stored term carries one.
    pub fn local(index: u32, hint: Option<&str>) -> Self {
        Free::Local(Mint::new(index, hint))
    }

    /// [`Free::local`] for a binder written in source, at `written` among its declaration's written binders. A local opened from its scope is no such binder: the place stays with the scope, for whoever opens it to read.
    pub fn local_written(index: u32, hint: Option<&str>, written: u32) -> Self {
        Free::Local(Mint {
            written: Some(written),
            ..Mint::new(index, hint)
        })
    }

    /// [`Free::local`] for a caller that already holds the hint as a symbol — a printer reopening a scope under the hint it remembers.
    pub(crate) fn local_hinted(index: u32, hint: Option<Symbol>) -> Self {
        Free::Local(Mint {
            index,
            hint,
            written: None,
        })
    }

    /// A definition at an authored path.
    pub fn global(qualifier: Qualifier) -> Self {
        Free::Global(Global::Authored(qualifier))
    }

    /// The top-level definition this names, if it names one.
    pub fn as_global(&self) -> Option<&Global> {
        match self {
            Free::Global(global) => Some(global),
            Free::Local(_) => None,
        }
    }

    /// The raw counter behind a locally minted name, or `None` for a global.
    ///
    /// What a checker minting binders of its own must stay clear of: the kernel raises its counter above every local it is handed (`Kernel::assume`), and refuses a module or entry mentioning one it was not.
    pub fn local_index(&self) -> Option<u32> {
        self.as_local().map(|mint| mint.index)
    }

    pub(crate) fn as_local(&self) -> Option<&Mint> {
        match self {
            Free::Local(mint) => Some(mint),
            Free::Global(_) => None,
        }
    }

    /// Whether this is a binder some scope opened, as opposed to a top-level definition. The typed replacement for testing a spelling for a marker character — see [`Subterm::has_local_free`](crate::Subterm).
    pub fn is_local(&self) -> bool {
        matches!(self, Free::Local(_))
    }

    /// Whether source can spell this identity: a global's path, or a local's written hint. A hintless local is compiler-minted — an anonymous or invented binder no written expression can reference — so a diagnostic spells it `_` rather than offering it as something to paste.
    pub fn nameable(&self) -> bool {
        match self {
            Free::Global(_) => true,
            Free::Local(mint) => mint.hint().is_some(),
        }
    }

    /// What a diagnostic should call this, if there is anything better than its rendered form: a local's minting hint, or nothing for a global, whose rendering the printer shortens against the module it appears in.
    pub fn hint(&self) -> Option<&str> {
        self.as_local().and_then(Mint::hint)
    }

    /// [`Free::hint`] as the interned symbol it is kept as.
    pub(crate) fn hint_symbol(&self) -> Option<Symbol> {
        self.as_local().and_then(Mint::hint_symbol)
    }
}

/// The archived form keeps the live identity law: the hint is not read.
///
/// A key set that could occur has at most one entry per index — two mints with the same index are one value — so this also agrees with the structural order on every map that survives a round trip.
///
/// Both halves are written by hand for that reason, and **a third comparison surface on [`Mint`] must be written by hand too — a derive there is the bug this note exists to prevent.** A derived comparison reads the hint, so the live and archived orderings would disagree only on maps that round-trip, with nothing to surface the divergence.
#[cfg(feature = "archive")]
mod archived_mint {
    use {
        super::ArchivedMint,
        std::{cmp::Ordering, hash},
    };

    impl PartialEq for ArchivedMint {
        fn eq(&self, other: &Self) -> bool {
            self.index == other.index
        }
    }

    impl Eq for ArchivedMint {}

    impl PartialOrd for ArchivedMint {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    impl Ord for ArchivedMint {
        fn cmp(&self, other: &Self) -> Ordering {
            self.index.cmp(&other.index)
        }
    }

    impl hash::Hash for ArchivedMint {
        fn hash<H: hash::Hasher>(&self, state: &mut H) {
            self.index.hash(state);
        }
    }
}

impl From<&Global> for Free {
    fn from(global: &Global) -> Self {
        Free::Global(*global)
    }
}

impl fmt::Display for Mint {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.hint {
            Some(hint) => write!(formatter, "{hint}#{}", self.index),
            None => write!(formatter, "#{}", self.index),
        }
    }
}

impl fmt::Display for Global {
    /// Debug rendering only. A diagnostic names a global through the printer, which shortens it against the module's other symbols; a witness is named by its declaring module, which the printer takes from the definition.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Global::Authored(qualifier) => formatter.write_str(&qualifier.join()),
            Global::Witness(id) => write!(formatter, "{id}"),
        }
    }
}

impl fmt::Display for Free {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Free::Global(global) => write!(formatter, "{global}"),
            Free::Local(mint) => write!(formatter, "{mint}"),
        }
    }
}

/// One binding a `use` brought into a lexical scope: its canonical name, and the path the reader wrote it under — `/std/Eq/cong` as `Eq/cong` after `use /std/{Eq}`, or as `cong` after a glob. Core holds only the canonical name; the spelling is the text stage's, and it is what a goal report's candidate must be displayed under to be pasteable.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Import {
    pub global: Global,
    pub spelling: String,
}

/// What a unit's `use` declarations brought into scope, and where. `use` is point-of-use: it binds from its own position to the end of the module body it is written in, and a nested module body starts with nothing. So each definition records the imports in scope where it was written, and the entrypoint tail records its own — what a goal inside either may be offered, spelled as it resolves there.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Imports {
    /// Every import, in the order the lowering met them. Appended to only; the scopes below index into it.
    pub entries: Vec<Import>,
    /// Per definition, the indices into `entries` in scope where it was written.
    pub by_item: BTreeMap<Global, Vec<usize>>,
    /// The indices in scope where the entrypoint tail was written — the end of the root body.
    pub tail: Vec<usize>,
}

impl Imports {
    /// The imports in scope at `owner`'s definition, or at the entrypoint tail for a goal no definition owns. A definition the lowering never snapshotted — one it synthesized, which no written goal can sit in — falls back to the tail's view as well.
    pub fn in_scope_at(&self, owner: Option<&Global>) -> impl Iterator<Item = &Import> {
        owner
            .and_then(|owner| self.by_item.get(owner))
            .unwrap_or(&self.tail)
            .iter()
            .map(|index| &self.entries[*index])
    }
}

/// How a unit's names can be written, and where: what each definition's `use` lines brought into scope, and every absolute path the unit may write for each global — its declaration path and each re-export, with the subtrees whose modules may name it. The text stage builds it, because only the text stage sees re-exports and visibility; a report reads it to spell a global the way resolution would find it from where the reader stands ([`Spellings::spell`]).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Spellings {
    pub imports: Imports,
    /// Each canonical global's absolute paths, the prefixes the unit may not name already dropped.
    pub paths: BTreeMap<Global, Vec<WritablePath>>,
}

/// One absolute path reaching a global, and who may write it.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct WritablePath {
    pub path: Qualifier,
    /// The subtree roots whose modules may name the path — a non-`pub` declaration's own module, a `pub` one's module's audience.
    pub audience: Vec<Qualifier>,
    /// Whether the path lies in the unit's own mounts, where a module may also reach it relatively, through its own declarations.
    pub own: bool,
}

impl Spellings {
    /// The shortest spelling that resolves to `global` for a reader in module `island`, written inside `owner` (the entrypoint's final term when `None`): its bare label or a path through a child module, where the reader's own module declares the way there; an import in scope there, as it was written; or an absolute path whose audience includes the reader. `None` when the reader can reach it by none of those — a name only a refusal shows, spelled faithfully by the caller.
    pub fn spell(
        &self,
        island: &Qualifier,
        owner: Option<&Global>,
        global: &Global,
    ) -> Option<String> {
        let paths = self.paths.get(global).into_iter().flatten();
        let reachable =
            paths.filter(|written| written.audience.iter().any(|root| island.is_within(root)));

        let mut candidates = Vec::new();
        for written in reachable {
            if written.own && written.path.is_within(island) && written.path != *island {
                candidates.push(written.path.segments()[island.segments().len()..].join("/"));
            }
            candidates.push(written.path.join());
        }
        candidates.extend(
            self.imports
                .in_scope_at(owner)
                .filter(|import| import.global == *global)
                .map(|import| import.spelling.clone()),
        );

        candidates
            .into_iter()
            .min_by_key(|spelling| (spelling.split('/').count(), spelling.len()))
    }
}
