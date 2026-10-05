//! The finished program: a flat list of top-level [`Item`]s over the nominal registries they are checked against.
//!
//! This is what a checker is handed. Elaboration produces it and erasure consumes it, but the shape itself is representation — a [`Definition`] is a name, a universe context, a type, and a body, and a [`RecItem`] is the same for a recursive group whose members reference each other through one shared [`RecGroup`] binder rather than through free names. Both checkers walk this structure, which is why it lives here rather than beside either of them.
//!
//! Items are stored in binding order and read in dependency order. A [`Module`] additionally carries the registries an item's types may name ([`InductDecl`], [`StructDecl`], [`ConceptDecl`]), the witness set, and the binder high-water mark a checker must seed above. The one compilation with an entrypoint holds a [`Program`], the module beside the entry's type and body.
//!
//! Well-formedness that *judges* rather than describes is not decided here. Whether a universe context is satisfiable is decided by each checker for itself, the elaborator's solver and the certifier's loop check; whether a definition terminates runs the size-change engine in `curios-analysis`, which both checkers drive. [`Totality`] is the classification those judgments record onto a definition, and the enum lives here because the field does.

use {
    super::{
        Atom, Bound, ConceptDecl, Enter, FieldSpelling, Free, FuncType, Global, InductDecl, Many,
        RecGroup, RecMemberScopes, Scope, Sharing, Spelling, StructDecl, Subterm, Telescope, Term,
        UniverseContext, UniverseError, WitnessSpelling, build_shorten,
    },
    curios_utilities::{Mount, Plicity, Qualifier, SyntaxRegistry},
    std::{
        collections::{BTreeMap, BTreeSet, HashSet},
        fmt,
        rc::Rc,
    },
};

/// Whether a definition is known to terminate on every input.
///
/// `Partial` is "not proven total", never "proven divergent": a productive corecursive definition and a genuine infinite loop are both `Partial`, and both remain legal wherever erasure keeps them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[curios_archive::archived]
pub enum Totality {
    /// Every recursive group this definition contains descends, it calls no diverging host row, and neither does anything it reaches.
    Total,
    /// Not proven total. The conservative default: a definition whose classification is unknown is `Partial`, never `Total`.
    #[default]
    Partial,
}

impl Totality {
    pub fn is_total(self) -> bool {
        matches!(self, Totality::Total)
    }
}

/// What the certifier concluded about the definitions one of its walks judged: each one's totality, closed over everything it mentions, and what judging it read of other items.
///
/// Only the certifier's walk makes one — `curios_cert::certify_module` — and a later walk reads it as the verdicts on the definitions it covers, rather than the stamp elaboration writes onto each [`Definition`]. It is filed with the unit whose definitions it covers, and that unit's address is its identity: a stored unit is found only under the compiler that judged it, and the fixed prelude's record is a constant of the build that certified it. It is read only where it [covers](Certification::covers) its unit, never one name at a time — one naming fewer definitions than its unit holds was not made by a walk over that unit, so none of its entries is known to be the closure it claims — and a unit without a covering record is one the reading walk classifies for itself, never one it takes elaboration's word for.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Certification {
    certified: BTreeMap<Global, Certified>,
}

/// What the certifier concluded about one definition it judged.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Certified {
    /// Whether the definition terminates, closed over everything it mentions.
    pub totality: Totality,
    /// What judging it read of other items. A recursive group is judged as one item, so each of its members holds the group's reads; a declaration's registry entry is accepted as part of its type former, whose entry holds what that acceptance read.
    pub reads: Reads,
}

/// The items one judgment read, and how: its type, or its body.
///
/// Recorded where the kernel consults its environment — a name's type or universe scheme, a declaration's registry entry, a definition's body — so a read counts however the judgment reached it. A remembered reduct included: every memo lives one declaration, so the bodies a remembered reduct unfolds were asked for within the judgment that hits it. A body counts as read whenever it is asked for, unfolded or not, because asking is what makes the answer depend on it.
///
/// A definition's classification also reads the verdict of every name it mentions, which the totality closure runs over. That is not a third kind: typing a mention reads its type, so each such verdict read is already a signature read.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Reads {
    /// The items whose type, universe scheme or registry entry was read.
    pub signatures: BTreeSet<Global>,
    /// The definitions whose body was asked for.
    pub bodies: BTreeSet<Global>,
}

impl Reads {
    /// These reads with `other`'s beside them.
    pub fn extend(&mut self, other: Reads) {
        self.signatures.extend(other.signatures);
        self.bodies.extend(other.bodies);
    }
}

impl Certification {
    /// A record of each of `definitions`.
    pub fn of(definitions: impl IntoIterator<Item = (Global, Certified)>) -> Self {
        Self {
            certified: definitions.into_iter().collect(),
        }
    }

    /// The certifier's classification of `name`, when this record covers it.
    pub fn totality(&self, name: &Global) -> Option<Totality> {
        self.certified.get(name).map(|certified| certified.totality)
    }

    /// What judging `name` read of other items, when this record covers it.
    pub fn reads(&self, name: &Global) -> Option<&Reads> {
        self.certified.get(name).map(|certified| &certified.reads)
    }

    /// Whether this record classifies every definition `module` holds.
    pub fn covers(&self, module: &Module) -> bool {
        module
            .items
            .iter()
            .flat_map(Item::definitions)
            .all(|definition| self.certified.contains_key(&definition.name))
    }

    /// Every classification this record holds.
    pub fn iter(&self) -> impl Iterator<Item = (&Global, Totality)> {
        self.certified
            .iter()
            .map(|(name, certified)| (name, certified.totality))
    }

    /// This record with `other`'s entries beside its own, its own winning where both hold a name — how an item-level recompile's record joins the baseline's entries for the items it reused to its own walk's.
    pub fn extended(mut self, other: &Certification) -> Self {
        for (name, certified) in &other.certified {
            self.certified
                .entry(*name)
                .or_insert_with(|| certified.clone());
        }
        self
    }
}

/// How a lowered definition was introduced.
///
/// This is elaboration metadata, not a fact inferred from the flattened qualified name. In particular, a module and a nominal type may share a qualifier without turning ordinary module members into generated nominal members.
///
/// A generated member names its origin in full: `InductiveConstructor` carries both the inductive it belongs to *and* which constructor it is, so the registry correspondence is read off the pair rather than re-synthesized by joining an owner and a tag into a name and looking that name up.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub enum DefinitionKind {
    Authored,
    InductiveType,
    InductiveConstructor { owner: Qualifier, tag: Atom },
    StructType,
    ConceptType,
    ConceptMethod { owner: Qualifier },
    Witness,
    Test,
}

/// A single top-level definition: `name` bound to `body` of declared `type_`.
///
/// A standalone top-level `let` uses free `Var`s keyed by `name`. A definition returned by [`RecItem::definitions`] is the opened view of a scoped recursive member and likewise uses the group's export names; the authoritative recursive type and body remain in [`RecItem::group`].
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub struct Definition {
    pub name: Global,
    pub kind: DefinitionKind,
    pub universe_context: UniverseContext,
    /// This definition's declaring module — `name`'s qualifier prefix, precomputed once by `into_core` (before `name` was flattened) rather than re-derived from it later. Stamped into `Context::island` per item by `elaborate_module_suffix` for the representation-privacy checks, which test subtree containment against it rather than equality; the same value `InductDecl::module` and `StructDecl::module` carry for type declarations. Islands are surface-elaboration state: erasure re-derives types with privacy suppressed and never stamps them.
    pub island: Qualifier,
    /// Whether this definition terminates on every input, together with everything it reaches. Written back by `curios-elab`'s `record_totality` after zonking — like `polarities` on a declaration, and for the same reason: the analysis needs final, meta-free terms, so construction cannot know the answer. It defaults to [`Totality::Partial`], which is what makes a site that forgets to stamp it fail closed rather than open.
    ///
    /// This is the cross-module summary elaboration's own gates read: a user program that mentions a prelude definition inherits the flag rather than re-analyzing the prelude, which is sound because "partial" already means "something partial is in its closure". The certifier reads it only as a claim to contradict, and nothing below erasure reads it: an erased function's termination flag is marked from the certifier's record.
    pub totality: Totality,
    pub type_: Term,
    pub body: Term,
}

/// Export metadata for one member of a flat top-level recursive group. The member's type and body live only in [`RecItem::group`], scoped over every export in the group.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct RecDefinition {
    pub name: Global,
    pub kind: DefinitionKind,
    pub island: Qualifier,
    /// Per member, not per group. The group's *descent* is decided once for all of them, but the transitive closure is not: an accepted group can still have one member that reaches something partial while its sibling does not. See [`Definition::totality`].
    pub totality: Totality,
}

/// A flat top-level recursive item backed by the same structural fixed-point representation as a local [`super::Rec`]. Keeping the export metadata separate preserves the module's flat architecture without retaining a second, free-name copy of each recursive type and body.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub struct RecItem {
    pub definitions: Vec<RecDefinition>,
    pub group: RecGroup,
}

impl RecItem {
    /// This group with universe data projected out of every member, in place.
    ///
    /// The universe context is cleared here as it is on a [`Definition`]: the projection's whole purpose is that no universe data survives into Ersd.
    pub fn projected(&self) -> Self {
        Self {
            definitions: self.definitions.clone(),
            group: self.group.projected(),
        }
    }

    pub fn new(definitions: Vec<Definition>) -> Self {
        Self::try_new(definitions).expect("a recursive group has one valid universe context")
    }

    pub fn try_new(definitions: Vec<Definition>) -> Result<Self, UniverseError> {
        let universe_context = definitions
            .first()
            .map(|definition| definition.universe_context.clone())
            .unwrap_or_default();
        if !definitions
            .iter()
            .all(|definition| definition.universe_context == universe_context)
        {
            return Err(UniverseError::MismatchedRecursiveContexts);
        }
        let names = definitions
            .iter()
            .map(|definition| Free::from(&definition.name))
            .collect::<Vec<_>>();
        let members = names.iter().collect::<Vec<_>>();
        let arity = Many(members.len());
        let group = RecGroup::new(
            definitions
                .iter()
                .map(|definition| RecMemberScopes {
                    type_: Scope::close(arity, &members, definition.type_.clone()),
                    body: Scope::close(arity, &members, definition.body.clone()),
                })
                .collect(),
        )
        .with_universe_context(universe_context);
        let definitions = definitions
            .into_iter()
            .map(|definition| RecDefinition {
                name: definition.name,
                kind: definition.kind,
                island: definition.island,
                totality: definition.totality,
            })
            .collect();

        Ok(Self { definitions, group })
    }

    /// Open the recursive scopes against their exported names.
    ///
    /// The returned definitions are a read-only projection; the authoritative types and bodies remain structurally shared in the group's scheme.
    pub fn definitions(&self) -> Vec<Definition> {
        let names = self
            .definitions
            .iter()
            .map(|definition| Term::free_var(&Free::from(&definition.name)))
            .collect::<Vec<_>>();
        let name_refs = names.iter().collect::<Vec<_>>();

        self.definitions
            .iter()
            .zip(self.group.iter())
            .map(|(definition, member)| Definition {
                name: definition.name,
                kind: definition.kind.clone(),
                universe_context: self.group.universe_context().clone(),
                island: definition.island,
                totality: definition.totality,
                type_: member.type_.open(&name_refs),
                body: member.body.open(&name_refs),
            })
            .collect()
    }

    pub fn island(&self) -> Qualifier {
        self.definitions
            .first()
            .map(|definition| definition.island)
            .unwrap_or_default()
    }
}

impl Definition {
    /// Every top-level name this definition mentions, by free variable.
    pub fn mentions(&self) -> BTreeSet<Global> {
        self.body
            .free_vars()
            .into_iter()
            .chain(self.type_.free_vars())
            .filter_map(|free| free.as_global().cloned())
            .collect()
    }

    /// Every top-level name this definition's elaboration consumed: what it [`mentions`](Self::mentions), plus the nominal heads its constructions and type-former normal forms name. Those live in the registry rather than the variable graph, so `mentions` cannot see them — and an item that builds a `Struct` or matches a `Variant` depends on that declaration exactly as one that names it does.
    ///
    /// This is the edge set an invalidation is closed over and a refusal is poisoned along. [`mentions`](Self::mentions) is what the kernel's order and the totality closure need, which the variable edges alone serve.
    pub fn reaches(&self) -> BTreeSet<Global> {
        let mut names = self.mentions();
        names.extend(construction_heads(&self.type_));
        names.extend(construction_heads(&self.body));
        names
    }

    fn print(&self, formatter: &mut fmt::Formatter<'_>, spelling: &Rc<Spelling>) -> fmt::Result {
        write!(
            formatter,
            "{} : {} = {}",
            self.name,
            self.type_.spelled(spelling),
            self.body.spelled(spelling)
        )
    }
}

/// A top-level item: a single `let` definition, or a `rec` group of mutually-recursive definitions (which may reference each other by `name`).
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub enum Item {
    Let(Definition),
    Rec(RecItem),
}

impl Item {
    /// How a diagnostic names this item.
    ///
    /// An authored declaration is named by its path. A witness has no authored path — that is the point of `satisfy` — so it is named by the module it was declared in, which is the coordinate a reader can actually act on.
    pub fn describe(&self) -> String {
        let described = |definition: &Definition| match definition.name.qualifier() {
            Some(path) => path.join(),
            None => match definition.island.is_root() {
                true => "the witness in the entry module".to_string(),
                false => format!("the witness in '{}'", definition.island.join()),
            },
        };
        match self {
            Item::Let(definition) => described(definition),
            Item::Rec(rec) => rec
                .definitions()
                .iter()
                .map(described)
                .collect::<Vec<_>>()
                .join(", "),
        }
    }

    /// The names exported by this top-level item, in declaration order.
    pub fn declared_names(&self) -> Vec<&Global> {
        match self {
            Item::Let(definition) => vec![&definition.name],
            Item::Rec(rec) => rec
                .definitions
                .iter()
                .map(|definition| &definition.name)
                .collect(),
        }
    }

    /// The definitions this top-level item declares, in the same order as [`Item::declared_names`] — one for a `let`, one per member for a `rec`.
    ///
    /// Written out at each caller, the fan-out would be one more place a new `Item` variant could be missed. It belongs here beside `declared_names` for the same reason that one does: what an item declares is the item's own question.
    ///
    /// Owned rather than borrowed, because a `rec` member's [`Definition`] is *materialized* from the group rather than stored — there is nothing to hand a reference to.
    pub fn definitions(&self) -> Vec<Definition> {
        match self {
            Item::Let(definition) => vec![definition.clone()],
            Item::Rec(rec) => rec.definitions(),
        }
    }
}

/// A program's entrypoint: the expression it closes with, with the type it is judged at. One type rather than two fields, so a type without an entry — a type for no expression — is unspellable rather than asserted away.
#[derive(Debug, Clone, PartialEq)]
pub struct Entrypoint {
    pub body: Term,
    /// The type the body is judged at. Before elaboration it is what whoever compiles the entry states — the program contract, or the empty proposition for an entry put as a proof — and absent only where nothing is stated, for elaboration to infer. Elaboration writes the type the body was judged at either way, so every stage after it reads the entry's type off the entry: [`Zonked::project`] refuses a program whose entry states none, and [`Zonked::entry`] hands it back beside the body.
    pub type_: Option<Term>,
}

/// A program: the unit it is compiled from, and the term it closes with.
///
/// The one compilation that has an entrypoint is the one that holds a `Program`; every other unit — a library, a prelude root, a recompile's reused items — is a [`Module`] alone. An optional entry on every module would be `None` for all of them but one, and each stage reading a module would carry a case for the entry it almost never has. Carried beside the module instead, the entry exists exactly where a program is compiled, and a stored unit cannot hold one.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub module: Module,
    pub entry: Entrypoint,
}

/// The whole of one unit as a *flat* list of top-level `items`, with the registries they are checked against.
///
/// Flat rather than one N-deep nested `Subterm::Let`/`Rec` term: folding a unit into one would make its construction (`Scope::close` over the whole accumulator at each step) and every pass that recursed along its `.tail` spine O(N) in stack, overflowing at prelude depth. `Subterm::Let`/`Rec` remain for genuine *local*, in-expression bindings, which are shallow.
///
/// The default is the empty unit: no item, no mount, no registry entry. Every field is a collection, so it states nothing, and a module built by hand names only what it holds.
#[derive(Debug, Clone, Default, PartialEq)]
#[curios_archive::archived]
pub struct Module {
    pub items: Vec<Item>,
    /// The prefixes this module's compilation unit claims, each with whether it is a root only the compiler supplies.
    ///
    /// Carried here, once, rather than stamped onto every declaration. Which mount owns a declaration is [`Mount::owning`] over the declaration's own name, so a stamp beside the name would only restate the name's leading segment — and, archived, mean something solely in the compilation that wrote it. A later stage that needs a mount's kind reads it out of this list; nothing derives one from a string.
    pub mounts: Vec<Mount>,
    /// Inductive declarations' registry entries, keyed by the type's qualified name. Carried on the module — not on a `Context` — because elaboration and erasure each run with their *own* `Context`; both seed their context's flat inductive store from here on entry.
    pub induct_decls: BTreeMap<Global, InductDecl>,
    /// Struct declarations' registry entries, keyed by the type's qualified name. Carried on the module like `induct_decls` (and for the same reason): elaboration and erasure each seed their own `Context` from here on entry.
    pub struct_decls: BTreeMap<Global, StructDecl>,
    /// Concept declarations' resolution metadata, keyed by the concept's qualified name (each concept's record shape also lives in `struct_decls`). Seeded into the elaboration `Context` on entry; erasure never consults it.
    pub concepts: BTreeMap<Global, ConceptDecl>,
    /// The definition names that are witness declarations. Elaboration registers each into the witness table when its signature elaborates — carried as names (not keys) because the table key needs the *elaborated* head, which only exists once elaboration runs.
    pub witnesses: BTreeSet<Global>,
    /// The definition names that are `test` declarations, in declaration order — the order the synthesized test tail schedules them and the runner reports them, so a `Vec` rather than a set.
    pub tests: Vec<Global>,
}

impl Module {
    /// This module with every term hash-consed against `sharing` — one shared allocation per distinct structure.
    ///
    /// Built for the archived prelude. Elaboration constructs the same types, telescopes, and proof spines independently in definition after definition, and nothing deduplicates them, because `Rc` sharing only ever arises from *cloning* a value: two definitions that build the same type build it twice. Unshared, the prelude's nodes outnumber its distinct structures many times over (the prelude build reports each root's distinct count), and the archive would store that expansion in full and every restored traversal walk it in full.
    ///
    /// Pass the same [`Sharing`] to every snapshot archived together so equal structures collapse across them as well as within each.
    pub fn shared(&self, sharing: &Sharing) -> Module {
        let definition = |definition: &Definition| Definition {
            name: definition.name,
            kind: definition.kind.clone(),
            universe_context: definition.universe_context.clone(),
            island: definition.island,
            totality: definition.totality,
            type_: sharing.share(&definition.type_),
            body: sharing.share(&definition.body),
        };

        Module {
            items: self
                .items
                .iter()
                .map(|item| match item {
                    Item::Let(let_) => Item::Let(definition(let_)),
                    Item::Rec(rec) => Item::Rec(RecItem {
                        definitions: rec
                            .definitions
                            .iter()
                            .map(|member| RecDefinition {
                                name: member.name,
                                kind: member.kind.clone(),
                                island: member.island,
                                totality: member.totality,
                            })
                            .collect(),
                        // Mapped in place rather than opened and re-closed: the round trip rebuilds every node twice and drops every memoized derivation with it, and the rebuilt nodes would escape this very pass.
                        group: rec.group.map_members(|term| sharing.share(term)),
                    }),
                })
                .collect(),
            mounts: self.mounts.clone(),
            induct_decls: self
                .induct_decls
                .iter()
                .map(|(name, declaration)| (*name, declaration.shared(sharing)))
                .collect(),
            struct_decls: self
                .struct_decls
                .iter()
                .map(|(name, declaration)| (*name, declaration.shared(sharing)))
                .collect(),
            concepts: self
                .concepts
                .iter()
                .map(|(name, concept)| (*name, concept.shared(sharing)))
                .collect(),
            witnesses: self.witnesses.clone(),
            tests: self.tests.clone(),
        }
    }

    /// Each nominal declaration's argument plicities, keyed by the family's name — parameters then indices, in the order a use site supplies them.
    ///
    /// Read off the type constructor's own definition, whose declared type is the one or two `FuncType`s lowering nests — the parameters', then an indexed family's indices' — ending in the sort: the parameters keep their declared marks and the indices are always explicit. That type is the marks' one home a checker establishes: `InductType` carries none (for a fixed name they are a function of the name, so storing them per-occurrence would be derived data that conversion must then either compare pointlessly or exclude from `Hash`, and excluding them lets hash-consing collapse differently-marked equal nodes), and the marks a registry entry's arity states for the elaborator ([`InductDecl::arity`], [`StructDecl::arity`]) are checked by no one, so a report does not read them.
    ///
    /// Both item arms are walked: an inductive's type constructor is a `rec` item, since it refers to itself, while structs and concepts are plain `let`s. A nullary declaration has no `FuncType` wrapper at all and contributes nothing.
    pub fn nominal_plicities(&self) -> BTreeMap<Global, Vec<Plicity>> {
        let mut marks = BTreeMap::new();

        let mut record = |def: &Definition| {
            if !matches!(
                def.kind,
                DefinitionKind::InductiveType
                    | DefinitionKind::StructType
                    | DefinitionKind::ConceptType
            ) {
                return;
            }
            // A type former's result is a sort, so the walk ends at the first node that is not a function type.
            let mut collected = Vec::new();
            let mut type_ = &def.type_;
            while let Subterm::FuncType(FuncType { telescope }) = &**type_ {
                collected.extend(telescope.marks());
                type_ = telescope.terminal();
            }
            if !collected.is_empty() {
                marks.insert(def.name, collected);
            }
        };

        for item in &self.items {
            match item {
                Item::Let(def) => record(def),
                Item::Rec(rec) => rec.definitions().iter().for_each(&mut record),
            }
        }

        marks
    }

    /// What a report spells this unit's witnesses against (axis (h)): each concept's fields — a superclass edge's concept, or a method's wrapper, whether that wrapper takes the method's own parameters in its group, and the operator `syntax` names for it — and each witness's declared type. `syntax` is the registry's because this crate may not spell a prelude declaration.
    pub fn witness_spelling(&self, syntax: &SyntaxRegistry) -> WitnessSpelling {
        let definitions = self
            .items
            .iter()
            .flat_map(|item| match item {
                Item::Let(def) => vec![def.clone()],
                Item::Rec(rec) => rec.definitions(),
            })
            .map(|def| (def.name, def))
            .collect::<BTreeMap<_, _>>();

        let mut spelling = WitnessSpelling::default();
        for (name, concept) in &self.concepts {
            let Global::Authored(path) = name else {
                continue;
            };
            let parameters = concept.params.len();
            let edges = self
                .struct_decls
                .get(name)
                .map(|declaration| concept.edges(declaration.fields()))
                .unwrap_or_default();
            let fields = concept
                .fields
                .iter()
                .enumerate()
                .map(|(index, label)| {
                    if let Some((_, reached)) = edges.iter().find(|(position, _)| *position == index)
                    {
                        return FieldSpelling::Super(*reached);
                    }
                    let wrapper = Global::Authored(path.with(label));
                    // A method's wrapper takes the method's parameters beside the concept's and its witness; any other field's takes those alone.
                    let merged = definitions.get(&wrapper).is_some_and(|def| {
                        matches!(&*def.type_, Subterm::FuncType(function) if function.plicities().len() > parameters + 1)
                    });
                    FieldSpelling::Method {
                        wrapper,
                        merged,
                        operator: syntax.operator.operator_for(path, label),
                    }
                })
                .collect();
            spelling.concepts.insert(*name, fields);
        }

        for def in definitions.into_values() {
            if matches!(def.kind, DefinitionKind::Witness) {
                spelling.witnesses.insert(def.name, def.type_);
            }
        }

        spelling
    }

    /// Every top-level name `item`'s elaboration consumed: what the item [reaches](Item::reaches) through its definitions, plus what the registry entries it declares reach — a struct's field types, an inductive's constructor payloads and a concept's parameters live only in the entry, and an item whose entry names another declaration depends on it as its body would.
    pub fn reaches(&self, item: &Item) -> BTreeSet<Global> {
        let mut names = item.reaches();

        for name in item.declared_names() {
            if let Some(declaration) = self.induct_decls.get(name) {
                names.extend(declaration.reaches());
            }
            if let Some(declaration) = self.struct_decls.get(name) {
                names.extend(declaration.reaches());
            }
            if let Some(concept) = self.concepts.get(name) {
                names.extend(concept.reaches());
            }
        }

        names
    }

    /// This module narrowed to the names `keep` admits: the items every one of whose declared names it admits, the registry entries keyed by such a name, and the witness and test markers naming one, with the mounts, the seed table, the floor and the entry carried whole.
    ///
    /// A recursive group is one node — its members share one scheme — so a predicate admitting some of a group's names and not others is a caller's mistake, refused rather than resolved either way.
    pub fn restricted(&self, keep: impl Fn(&Global) -> bool) -> Module {
        let items = self
            .items
            .iter()
            .filter(|item| {
                let names = item.declared_names();
                let kept = names.iter().filter(|name| keep(name)).count();
                assert!(
                    kept == 0 || kept == names.len(),
                    "a recursive group is restricted whole: {}",
                    item.describe()
                );

                kept > 0
            })
            .cloned()
            .collect();
        let entries = |declarations: &BTreeMap<Global, InductDecl>| {
            declarations
                .iter()
                .filter(|(name, _)| keep(name))
                .map(|(name, declaration)| (*name, declaration.clone()))
                .collect()
        };

        Module {
            items,
            mounts: self.mounts.clone(),
            induct_decls: entries(&self.induct_decls),
            struct_decls: self
                .struct_decls
                .iter()
                .filter(|(name, _)| keep(name))
                .map(|(name, declaration)| (*name, declaration.clone()))
                .collect(),
            concepts: self
                .concepts
                .iter()
                .filter(|(name, _)| keep(name))
                .map(|(name, concept)| (*name, concept.clone()))
                .collect(),
            witnesses: self
                .witnesses
                .iter()
                .filter(|name| keep(name))
                .cloned()
                .collect(),
            tests: self
                .tests
                .iter()
                .filter(|name| keep(name))
                .cloned()
                .collect(),
        }
    }

    /// Every global qualified name in `self`: each definition (`let`/`rec`), each inductive type, each struct type. The universe a global is shortened *against*.
    pub fn module_symbols(&self) -> Vec<Global> {
        let mut symbols = Vec::new();
        for item in &self.items {
            match item {
                Item::Let(def) => symbols.push(def.name),
                Item::Rec(rec) => {
                    symbols.extend(rec.definitions.iter().map(|definition| definition.name))
                }
            }
        }
        symbols.extend(self.induct_decls.keys().cloned());
        symbols.extend(self.struct_decls.keys().cloned());
        symbols
    }
}

impl Module {
    /// The spelling this module prints its terms in.
    ///
    /// Shortened against this module's own symbols (axis (b)) and nothing else, which is the one shortening site in the workspace that does not union its scope — both checkers' `format_with` take `&[&Module]` and merge. Deliberate, on three grounds. A value printing itself has no scope to be handed without ceasing to be `Display`. No ambiguity can follow from the narrower table: `build_shorten` records only names that actually shorten, and `Spelling::symbol` falls back to the full path, so a name from outside the unit prints qualified rather than misleadingly short. And a dump is read *about* the compiler, where a qualified `/std/Str/concat` beside a bare `append` says which unit each came from — the distinction a scope-wide table would erase. Its universes stay visible for the same reason a diagnostic suppresses them.
    fn spelling(&self) -> Rc<Spelling> {
        Rc::new(
            Spelling::default().with_short_names(Rc::new(build_shorten(&self.module_symbols()))),
        )
    }

    /// The items, one per line — printed by *iterating* the flat items (never re-folding into a nested term), so `wonder stage core` stays O(N) and never builds the nested term a deep module overflows on.
    fn print_items(
        &self,
        formatter: &mut fmt::Formatter<'_>,
        spelling: &Rc<Spelling>,
    ) -> fmt::Result {
        for item in &self.items {
            match item {
                Item::Let(def) => {
                    write!(formatter, "let ")?;
                    def.print(formatter, spelling)?;
                    writeln!(formatter, ";")?;
                }
                Item::Rec(rec) => {
                    write!(formatter, "rec ")?;
                    for (index, def) in rec.definitions().iter().enumerate() {
                        if index > 0 {
                            write!(formatter, "and ")?;
                        }
                        def.print(formatter, spelling)?;
                        write!(formatter, " ")?;
                    }
                    writeln!(formatter, ";")?;
                }
            }
        }

        Ok(())
    }
}

impl fmt::Display for Module {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.print_items(formatter, &self.spelling())
    }
}

impl fmt::Display for Program {
    /// The module's items, then the entry in the module's spelling, with the type it states beneath it.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let spelling = self.module.spelling();
        self.module.print_items(formatter, &spelling)?;
        write!(formatter, "{}", self.entry.body.spelled(&spelling))?;
        if let Some(type_) = &self.entry.type_ {
            write!(formatter, "\n: {}", type_.spelled(&spelling))?;
        }

        Ok(())
    }
}

impl Term {
    /// The top-level names this term reaches: its global free variables, and the nominal heads of its constructions and type-former normal forms, which live in the registry rather than the variable graph.
    pub fn reaches(&self) -> BTreeSet<Global> {
        term_reaches(self)
    }
}

impl Entrypoint {
    /// Every free local the entry's body or stated type mentions — what a walk judging the entry beside its module refuses, as [`free_locals_outside`] is for the module.
    pub fn free_locals(&self) -> BTreeSet<Free> {
        std::iter::once(&self.body)
            .chain(&self.type_)
            .flat_map(Term::free_vars)
            .filter(Free::is_local)
            .collect()
    }

    /// The top-level names the entry reaches, through its body and the type it states.
    pub fn reaches(&self) -> BTreeSet<Global> {
        let mut names = term_reaches(&self.body);
        names.extend(self.type_.iter().flat_map(term_reaches));
        names
    }
}

impl Item {
    /// The top-level names this item's definitions reach — [`Definition::reaches`] for a `let`; for a `rec`, its members read under the group's own binders rather than opened, so a member's reference to a sibling is not an edge to itself.
    pub fn reaches(&self) -> BTreeSet<Global> {
        match self {
            Item::Let(definition) => definition.reaches(),
            Item::Rec(rec) => {
                let mut names = BTreeSet::new();
                for member in rec.group.iter() {
                    names.extend(term_reaches(member.type_.body()));
                    names.extend(term_reaches(member.body.body()));
                }
                names
            }
        }
    }
}

impl InductDecl {
    /// The top-level names this declaration reaches: through its arity, its result sort and its constructors' signatures.
    pub fn reaches(&self) -> BTreeSet<Global> {
        let mut names = arity_reaches(&self.arity);
        names.extend(term_reaches(&self.result_sort));
        for (_, constructor) in &self.constructors {
            names.extend(payload_reaches(&constructor.telescope));
        }
        names
    }
}

impl StructDecl {
    /// The top-level names this declaration reaches: through its arity, fields included, and its result sort.
    pub fn reaches(&self) -> BTreeSet<Global> {
        let mut names = arity_reaches(&self.arity);
        names.extend(term_reaches(&self.result_sort));
        names
    }
}

impl ConceptDecl {
    /// The top-level names this declaration reaches through its parameters; its fields are its record entry's.
    pub fn reaches(&self) -> BTreeSet<Global> {
        fields_reaches(&self.params)
    }
}

/// The names `term` reaches: its global free variables and its construction heads.
fn term_reaches(term: &Term) -> BTreeSet<Global> {
    let mut names = term
        .free_vars()
        .into_iter()
        .filter_map(|free| free.as_global().cloned())
        .collect::<BTreeSet<_>>();
    names.extend(construction_heads(term));

    names
}

/// The nominal heads `term`'s constructions and type-former normal forms name.
///
/// On the shared walk driver, deduplicated on node identity, for the reason every walk over shared structure is: a string literal's scan chain is linear in nodes and quadratic in paths, and a walk that revisits shared nodes pays the square.
fn construction_heads(term: &Term) -> BTreeSet<Global> {
    let mut state: (HashSet<Term>, BTreeSet<Global>) = (HashSet::new(), BTreeSet::new());
    term.walk(
        &mut state,
        |state, term| {
            if !state.0.insert(term.clone()) {
                return Enter::Skip(());
            }
            match &**term {
                Subterm::InductType(node) => {
                    state.1.insert(node.name);
                }
                Subterm::Variant(node) => {
                    state.1.insert(node.name);
                }
                Subterm::StructType(node) => {
                    state.1.insert(node.name);
                }
                Subterm::Struct(node) => {
                    state.1.insert(node.name);
                }
                _ => {}
            }
            Enter::Descend
        },
        |_, _, _| (),
    );

    state.1
}

/// A telescope's entry types in order, and what it ends in.
fn entries<B: Bound>(telescope: &Telescope<B>) -> (Vec<&Term>, &B) {
    let mut types = Vec::new();
    let mut rest = telescope;
    loop {
        match rest {
            Telescope::Cons(_, type_, scope) => {
                types.push(type_);
                rest = scope.body();
            }
            Telescope::Done(done) => return (types, done),
        }
    }
}

/// The names a field telescope reaches.
fn fields_reaches(fields: &Telescope<()>) -> BTreeSet<Global> {
    entries(fields)
        .0
        .into_iter()
        .flat_map(term_reaches)
        .collect()
}

/// The names a declaration's arity reaches: its parameters, then the telescope they end in.
fn arity_reaches(arity: &Telescope<Telescope<()>>) -> BTreeSet<Global> {
    let (params, fields) = entries(arity);
    params
        .into_iter()
        .flat_map(term_reaches)
        .chain(fields_reaches(fields))
        .collect()
}

/// The names a constructor's signature reaches: its payloads, then the index targets they end in.
fn payload_reaches(signature: &Telescope<Vec<Term>>) -> BTreeSet<Global> {
    let (payloads, targets) = entries(signature);
    payloads
        .into_iter()
        .chain(targets.iter())
        .flat_map(term_reaches)
        .collect()
}

/// One question a walk asks of whatever sits at a module position.
///
/// A [`Bound`] behind a trait object rather than a generic parameter, so [`module_positions`] can offer one list of positions to more than one collector. Two reads, because two identities are findable by looking at a term: the local binder indices it mentions, and whether a metavariable node survives in it.
trait Carried {
    fn free_vars(&self) -> BTreeSet<Free>;
    fn has_metavar(&self) -> bool;
}

impl<B: Bound> Carried for B {
    fn free_vars(&self) -> BTreeSet<Free> {
        Bound::free_vars(self)
    }

    fn has_metavar(&self) -> bool {
        Bound::has_metavar(self)
    }
}

/// Every position in `module` that can hold a bound value, offered to `visit` with the top-level name owning it, skipping whatever `in_scope` already answers for.
///
/// One list, read by every walk that asks what a module carries — the kernel's free-local refusal below it and the storage refusal beside that. A second copy is how a position quietly stops being covered, which is the failure the enumeration exists to prevent, so a new question about a module's contents is a new `visit` and never a new walk. `None` is the entrypoint, which belongs to the module rather than to any name it declares.
fn module_positions(
    module: &Module,
    in_scope: impl Fn(&Global) -> bool,
    mut visit: impl FnMut(Option<&Global>, &dyn Carried),
) {
    let covered = |names: Vec<&Global>| !names.is_empty() && names.into_iter().all(&in_scope);

    for item in module
        .items
        .iter()
        .filter(|item| !covered(item.declared_names()))
    {
        for definition in item.definitions() {
            visit(Some(&definition.name), &definition.type_);
            visit(Some(&definition.name), &definition.body);
        }
    }

    for (name, declaration) in module
        .induct_decls
        .iter()
        .filter(|(name, _)| !in_scope(name))
    {
        visit(Some(name), &declaration.arity);
        visit(Some(name), &declaration.result_sort);
        for (_, constructor) in &declaration.constructors {
            visit(Some(name), &constructor.telescope);
        }
    }

    for (name, declaration) in module
        .struct_decls
        .iter()
        .filter(|(name, _)| !in_scope(name))
    {
        visit(Some(name), &declaration.arity);
        visit(Some(name), &declaration.result_sort);
    }

    for (name, concept) in module.concepts.iter().filter(|(name, _)| !in_scope(name)) {
        visit(Some(name), &concept.params);
    }
}

/// An identity a stored unit may not carry: one meaningful only in the compilation that assigned it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Positional {
    /// A free local binder. Its index came from one compilation's binder counter, and every walk mints from a counter of its own that starts at zero — so a local surviving into stored output is an index two compilations can both hand out.
    FreeLocal { owner: Option<Global>, index: u32 },
    /// A term metavariable. Zonking is contracted to substitute every solution and to refuse an unsolved hole, so one reaching here is that contract broken rather than a hole still to be solved.
    Metavar { owner: Option<Global> },
    /// A witness this module declares, scoped to a mount it does not own. Its ordinal counts *within* a mount, so one carrying somebody else's is an ordinal two compilations can both hand out — the aliasing that would silently rebind a coherence-table entry.
    UnscopedWitness { witness: Global },
}

impl fmt::Display for Positional {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (owner, carried) = match self {
            Positional::FreeLocal { owner, index } => (owner, format!("free local binder {index}")),
            Positional::Metavar { owner } => (owner, "an unsolved metavariable".to_string()),
            Positional::UnscopedWitness { witness } => {
                return write!(
                    formatter,
                    "{witness} is declared here and scoped to a mount this module does not own"
                );
            }
        };

        match owner {
            Some(name) => write!(formatter, "{name} carries {carried}"),
            None => write!(formatter, "the entrypoint carries {carried}"),
        }
    }
}

/// Refuse `module` if it carries an identity meaningful only in the compilation that produced it.
///
/// **A unit may be stored only if it carries no positional identity.** Storing one is how rustc came to need `cnum_map` — an index another compilation reads and then has to remap — and it is the property deciding whether a stored unit is portable at all.
///
/// Three of the classes are refused here. The remaining one, an unsolved universe metavariable, is refused at the same seam by `curios-elab`'s `validate_universes`, which names it in as many words; restating it would be a second implementation of one predicate rather than a second opinion about it, which is the standing [`UniverseContext::is_closed`] holds for the same reason.
///
/// The witness class is asked differently from the other two, and deliberately. Those are read off the module's *terms*, because a metavariable or a free local anywhere in one is disqualifying. A witness reference is not: a stored unit legitimately mentions witnesses its predecessors declared, scoped to *their* mounts. What must hold is that every witness this module **declares** is scoped to a mount it owns, which is a question about `Module::witnesses` rather than about any position — so it is asked over the declarations and not through the walk.
///
/// It refuses the free locals [`free_locals_outside`] reports, over the same positions, at the other seam: the kernel refuses one where it walks a module, and this refuses one where a module is stored. Neither has a safe direction to degrade in — a local reaching either aliases silently with a binder some later walk mints, which admits rather than crashes. It still *describes* rather than judges by this module's rule — whether a node is a metavariable, and whether a variable is local, are properties of the representation, taking no reduction, no conversion and no `Env`.
pub fn validate_stored_identities(module: &Module) -> Result<(), Positional> {
    let mut found: Option<Positional> = None;

    module_positions(
        module,
        |_| false,
        |owner, carried| {
            if found.is_some() {
                return;
            }

            if carried.has_metavar() {
                found = Some(Positional::Metavar {
                    owner: owner.cloned(),
                });
                return;
            }

            if let Some(index) = carried
                .free_vars()
                .into_iter()
                .find_map(|free| free.local_index())
            {
                found = Some(Positional::FreeLocal {
                    owner: owner.cloned(),
                    index,
                });
            }
        },
    );

    if let Some(witness) = module.witnesses.iter().find(|witness| match witness {
        // Containment rather than equality: the identity names the *module* that declares it, which lies within one of this unit's mounts rather than being one.
        Global::Witness(id) => Mount::owning(&module.mounts, id.module()).is_none(),
        Global::Authored(_) => false,
    }) {
        return Err(Positional::UnscopedWitness { witness: *witness });
    }

    found.map_or(Ok(()), Err)
}

/// Every free local `module`'s terms mention outside what `in_scope` already answers for, each with the top-level name owning the position it was found in.
///
/// **What a checker refuses before it mints a binder of its own.** Both checkers mint from a counter no predecessor raised — the kernel's from zero, the elaborator's from its own unit's lowering count — because no term a walk is handed from elsewhere carries a local ([`validate_stored_identities`]). A free local reaching a walk is therefore one its counter could hand out again, and the binder it mints while comparing under a telescope or eta-contracting would silently identify two terms that differ. The judgment could not see it: by the time it looks the local up, the local is bound. So the refusal is taken at the boundary, over every position a term can sit in, rather than where a judgment meets it.
///
/// It lives here by this module's own rule, the one stated at the top: it *describes* rather than judges. Whether a variable is local asks nothing of a kernel — no reduction, no conversion, no `Env` — so it is a property of the data, and a second implementation would be a second run of the same function rather than a second opinion. That is the standing `UniverseContext::is_closed` has for the same reason.
///
/// The predicate is over *names* rather than over a position, which is what lets one environment answer for four namespaces at once: a name identifies one top-level thing within a module, and an environment populates every namespace it holds from the same source. An item is skipped only when *every* name it declares is already answered for, since the walk that answered for it refused a local of its own.
pub fn free_locals_outside(
    module: &Module,
    in_scope: impl Fn(&Global) -> bool,
) -> BTreeSet<(Option<Global>, Free)> {
    let mut found = BTreeSet::new();

    module_positions(module, in_scope, |owner, carried| {
        found.extend(
            carried
                .free_vars()
                .into_iter()
                .filter(Free::is_local)
                .map(|local| (owner.copied(), local)),
        );
    });

    found
}

/// Evidence that a module is finished with elaboration's own syntax — the kernel's whole `NotCore` class: no `Metavar` and no `Transient` node survives in any term-bearing position, and a program's entry states the type it was judged at. `curios-elab`'s elaboration and zonk are the passes that make a module satisfy this; the validating [`Zonked::project`] is how any holder of a `Module` re-establishes it at a stage boundary, cheaply, because `has_metavar` and `has_transient` are per-node cached derivations.
///
/// The wrapper is interface-level evidence, never a license to trust: the kernel keeps its own metavariable refusals, so a `Zonked` constructed wrongly is still caught where soundness lives.
#[derive(Debug, Clone)]
pub struct Zonked<T>(T);

/// What [`Zonked`] can be evidence of: something elaboration and zonk are answerable for, which names the first place it is not finished.
pub trait Zonkable: Clone {
    /// The first place elaboration-only syntax survives — or, for a program, where its entry states no type — or `None` when there is none.
    fn unfinished(&self) -> Option<String>;
}

impl Zonkable for Module {
    fn unfinished(&self) -> Option<String> {
        zonked_refusal(self)
    }
}

impl Zonkable for Program {
    /// The module's first unfinished place, then the entry's: a program is finished when its module is and its entry states the type it was judged at, meta-free.
    fn unfinished(&self) -> Option<String> {
        zonked_refusal(&self.module).or_else(|| {
            let unfinished = |term: &Term| term.has_metavar() || term.has_transient();
            match &self.entry.type_ {
                None => Some("the entrypoint states no type it was judged at".to_owned()),
                Some(type_) if unfinished(&self.entry.body) || unfinished(type_) => {
                    Some("elaboration-only syntax survives in the entrypoint".to_owned())
                }
                Some(_) => None,
            }
        })
    }
}

impl<T: Zonkable> Zonked<T> {
    /// Validate that `value` is zonked and take evidence of it. The clone is cheap — items share their terms by `Rc`.
    pub fn project(value: &T) -> Result<Self, ZonkedRefusal> {
        match value.unfinished() {
            Some(place) => Err(ZonkedRefusal { place }),
            None => Ok(Self(value.clone())),
        }
    }
}

impl Zonked<Module> {
    pub fn as_module(&self) -> &Module {
        &self.0
    }

    pub fn into_module(self) -> Module {
        self.0
    }

    /// The carried module rewritten by a transform that preserves meta-freedom. The preservation claim is the caller's; debug builds re-validate it.
    pub fn map(self, rewrite: impl FnOnce(Module) -> Module) -> Self {
        let rewritten = rewrite(self.0);
        debug_assert!(
            zonked_refusal(&rewritten).is_none(),
            "a Zonked::map rewrite must preserve meta-freedom"
        );
        Self(rewritten)
    }
}

impl Zonked<Program> {
    pub fn as_program(&self) -> &Program {
        &self.0
    }

    /// The program's module, with the evidence its projection established. The clone is cheap — items share their terms by `Rc`.
    pub fn module(&self) -> Zonked<Module> {
        Zonked(self.0.module.clone())
    }

    /// The entrypoint's body and the type it was judged at.
    pub fn entry(&self) -> (&Term, &Term) {
        let entry = &self.0.entry;
        let type_ = entry
            .type_
            .as_ref()
            .expect("projection refuses an entry that states no type");
        (&entry.body, type_)
    }
}

/// Why [`Zonked::project`] refused: the first term-bearing place where a metavariable or a transient survived, or the uncleared seeds.
#[derive(Debug)]
pub struct ZonkedRefusal {
    place: String,
}

impl fmt::Display for ZonkedRefusal {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "the module is not zonked: {}", self.place)
    }
}

/// The term-bearing places of a module elaboration and zonk are answerable for — a definition's type and body and the registries' telescopes — walked with the cached `has_metavar`/`has_transient` bits, first offender wins.
fn zonked_refusal(module: &Module) -> Option<String> {
    fn unfinished(term: &Term) -> bool {
        term.has_metavar() || term.has_transient()
    }
    fn unfinished_bound<B: Bound>(value: &B) -> bool {
        value.has_metavar() || value.has_transient()
    }

    for item in &module.items {
        let survives = match item {
            Item::Let(definition) => unfinished(&definition.type_) || unfinished(&definition.body),
            Item::Rec(rec) => rec
                .group
                .iter()
                .any(|member| unfinished(member.type_.body()) || unfinished(member.body.body())),
        };
        if survives {
            return Some(format!(
                "elaboration-only syntax survives in {}",
                item.describe()
            ));
        }
    }
    for (name, decl) in &module.induct_decls {
        if unfinished_bound(&decl.arity)
            || unfinished(&decl.result_sort)
            || decl
                .constructors
                .iter()
                .any(|(_, param)| unfinished_bound(&param.telescope))
        {
            return Some(format!(
                "elaboration-only syntax survives in the registry entry for {name}"
            ));
        }
    }
    for (name, decl) in &module.struct_decls {
        if unfinished_bound(&decl.arity) || unfinished(&decl.result_sort) {
            return Some(format!(
                "elaboration-only syntax survives in the registry entry for {name}"
            ));
        }
    }
    None
}

#[cfg(test)]
mod tests;
