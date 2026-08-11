//! The finished program: a flat list of top-level [`Item`]s over the nominal registries they are checked against.
//!
//! This is what a checker is handed. Elaboration produces it and erasure consumes it, but the shape itself is representation — a [`Definition`] is a name, a universe context, a type, and a body, and a [`RecItem`] is the same for a recursive group whose members reference each other through one shared [`RecGroup`] binder rather than through free names. Both checkers walk this structure, which is why it lives here rather than beside either of them.
//!
//! Items are stored in binding order and read in dependency order. A [`Module`] additionally carries the registries an item's types may name ([`InductDecl`], [`StructDecl`], [`ConceptDecl`]), the witness set, the binder high-water mark a checker must seed above, and the entrypoint's own type and body.
//!
//! Well-formedness that *judges* rather than describes is not decided here. Whether a universe context is satisfiable runs a solver and belongs to `curios-elab`; whether a definition terminates runs the size-change engine and belongs to `curios-cert`. [`Totality`] is the classification those judgments record onto a definition, and the enum lives here because the field does.

use {
    super::{
        Atom, Bound, ConceptDecl, Free, FuncType, Global, InductDecl, Many, RecGroup,
        RecMemberScopes, Scope, Sharing, Spelling, StructDecl, Subterm, Term, UniverseContext,
        UniverseError, UniverseSeed, build_shorten, project_erased_universes,
    },
    curios_base::{Mount, Plicity, Qualifier},
    std::{
        collections::{BTreeMap, BTreeSet},
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
    /// Every recursive group this definition contains descends, it does not mention `Intrinsic::ProcExit`, and neither does anything it reaches.
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
    /// This definition's declaring module — `name`'s qualifier prefix, precomputed once by `into_core` (before `name` was flattened) rather than re-derived from it later. Stamped into `Context::island` per item by `elaborate_module_suffix` for the representation-privacy checks, which test subtree containment against it rather than equality; the same value `Structure::module` carries for type declarations. Islands are surface-elaboration state: erasure re-derives types with privacy suppressed and never stamps them.
    pub island: Qualifier,
    /// Whether this definition terminates on every input, together with everything it reaches. Written back by `crate::record_totality` after zonking — like `polarities` on a declaration, and for the same reason: the analysis needs final, meta-free terms, so construction cannot know the answer. It defaults to [`Totality::Partial`], which is what makes a site that forgets to stamp it fail closed rather than open.
    ///
    /// This is the cross-module summary the erasure gates read. A user program that mentions a prelude definition inherits the flag rather than re-analyzing the prelude, which is sound because "partial" already means "something partial is in its closure".
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
            group: self
                .group
                .map_members(project_erased_universes)
                .with_universe_context(UniverseContext::empty()),
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
                name: definition.name.clone(),
                kind: definition.kind.clone(),
                universe_context: self.group.universe_context().clone(),
                island: definition.island.clone(),
                totality: definition.totality,
                type_: member.type_.open(&name_refs),
                body: member.body.open(&name_refs),
            })
            .collect()
    }

    pub fn island(&self) -> Qualifier {
        self.definitions
            .first()
            .map(|definition| definition.island.clone())
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
    /// The fan-out this replaces was written out at eight sites across three crates, which is eight places a new `Item` variant could be missed. It belongs here beside `declared_names` for the same reason that one does: what an item declares is the item's own question.
    ///
    /// Owned rather than borrowed, because a `rec` member's [`Definition`] is *materialized* from the group rather than stored — there is nothing to hand a reference to.
    pub fn definitions(&self) -> Vec<Definition> {
        match self {
            Item::Let(definition) => vec![definition.clone()],
            Item::Rec(rec) => rec.definitions(),
        }
    }
}

/// The whole program as a *flat* list of top-level `items`, the entrypoint `body`, and its optional `type_` annotation.
///
/// This replaces the single, N-deep nested `Subterm::Let`/`Rec` term that `text::into_core` used to fold the entire prelude into — the construction (`Scope::close` over the whole accumulator at each step) and every pass that recursed along its `.tail` spine were both O(N) in stack and overflowed at prelude depth. `Subterm::Let`/`Rec` remain for genuine *local*, in-expression bindings, which are shallow.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub struct Module {
    pub items: Vec<Item>,
    /// The prefixes this module's compilation unit claims, and the privilege tier each carries.
    ///
    /// Carried here, once, rather than stamped onto every declaration. Which mount owns a declaration is [`Mount::owning`] over the declaration's own name, so a stamp beside the name only ever restated the name's leading segment — and being archived, it meant something solely in the compilation that wrote it. A later stage that needs a privilege tier reads it out of this list; nothing derives one from a string.
    pub mounts: Vec<Mount>,
    /// Lowering-time metadata for every universe metavariable id in this module. Finalized, zonked modules clear this vector.
    pub universe_seeds: Vec<UniverseSeed>,
    /// Inductive declarations' registry entries, keyed by the type's qualified name. Carried on the module — not on a `Context` — because elaboration and erasure each run with their *own* `Context` (see `run::compile`); both seed their context's flat inductive store from here on entry.
    pub induct_decls: BTreeMap<Global, InductDecl>,
    /// Struct declarations' registry entries, keyed by the type's qualified name. Carried on the module like `induct_decls` (and for the same reason): elaboration and erasure each seed their own `Context` from here on entry.
    pub struct_decls: BTreeMap<Global, StructDecl>,
    /// Concept declarations' resolution metadata, keyed by the concept's qualified name (each concept's record shape also lives in `struct_decls`). Seeded into the elaboration `Context` on entry; erasure never consults it.
    pub concepts: BTreeMap<Global, ConceptDecl>,
    /// The definition names that are witness declarations. Elaboration registers each into the witness table when its signature elaborates — carried as names (not keys) because the table key needs the *elaborated* head, which only exists once elaboration runs.
    pub witnesses: BTreeSet<Global>,
    /// One past the highest binder index `into_core` minted for this module.
    ///
    /// Binder identities are one space shared with `Context::fresh`, so elaboration seeds its counter here (`Context::set_local_floor`). The archived prelude carries its own high-water mark for the same reason: a replayed term's binders were minted in an earlier compiler run, and a fresh mint that aliased one of them would silently capture.
    pub binder_floor: usize,
    pub type_: Option<Term>,
    /// The entrypoint expression, for the one unit in a compilation that has one.
    ///
    /// `None` is what makes a unit *not* the entry, and it is the only thing that does: a unit with no successors owns the entrypoint, and every other unit is a scope its successors are compiled against. The prelude used to store `Nat::Zero` here and have its build certify that dummy — a value standing in for "there is none", the same shape `RootId::Entry` had before it became a mount.
    pub body: Option<Term>,
}

impl Module {
    /// This module with every term hash-consed against `sharing` — one shared allocation per distinct structure.
    ///
    /// Built for the archived prelude. Elaboration constructs the same types, telescopes, and proof spines independently in definition after definition, and nothing deduplicates them, because `Rc` sharing only ever arises from *cloning* a value: two definitions that build the same type build it twice. Measured over the prelude, 389,264 nodes covered 19,908 distinct structures — a 19.6x expansion that the archive stores in full and every restored traversal then walks in full.
    ///
    /// Pass the same [`Sharing`] to every snapshot archived together so equal structures collapse across them as well as within each.
    pub fn shared(&self, sharing: &Sharing) -> Module {
        let definition = |definition: &Definition| Definition {
            name: definition.name.clone(),
            kind: definition.kind.clone(),
            universe_context: definition.universe_context.clone(),
            island: definition.island.clone(),
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
                                name: member.name.clone(),
                                kind: member.kind.clone(),
                                island: member.island.clone(),
                                totality: member.totality,
                            })
                            .collect(),
                        // Mapped in place rather than opened and re-closed: the round trip rebuilds every node twice and drops every memoized derivation with it, and the rebuilt nodes would escape this very pass.
                        group: rec.group.map_members(|term| sharing.share(term)),
                    }),
                })
                .collect(),
            mounts: self.mounts.clone(),
            universe_seeds: self.universe_seeds.clone(),
            induct_decls: self
                .induct_decls
                .iter()
                .map(|(name, declaration)| (name.clone(), declaration.shared(sharing)))
                .collect(),
            struct_decls: self
                .struct_decls
                .iter()
                .map(|(name, declaration)| (name.clone(), declaration.shared(sharing)))
                .collect(),
            concepts: self
                .concepts
                .iter()
                .map(|(name, concept)| (name.clone(), concept.shared(sharing)))
                .collect(),
            witnesses: self.witnesses.clone(),
            binder_floor: self.binder_floor,
            type_: self.type_.as_ref().map(|type_| sharing.share(type_)),
            body: self.body.as_ref().map(|body| sharing.share(body)),
        }
    }

    /// Re-fold the flat module into the legacy nested `Let`/`Rec` `Term` it replaced (items are already in binding order). Test-only: lets the `into_core`/`erase` suites keep asserting against the historical shape — and keep feeding a single `Term` to `erase` — without rewriting every expectation. Drops `type_` (the old `run` helper only returned the term). Not `#[cfg(test)]`: its callers live in `curios`'s test suite, a different crate, where that cfg would never activate.
    pub fn into_nested_term(self) -> Term {
        let body = self
            .body
            .expect("into_nested_term is for a module with an entrypoint");

        self.items
            .into_iter()
            .rev()
            .fold(body, |acc, item| match item {
                Item::Let(def) => Term::let_(&Free::from(&def.name), def.type_, def.body, acc),
                Item::Rec(rec) => Term::rec(
                    rec.definitions()
                        .into_iter()
                        .map(|def| (Free::from(&def.name), def.type_, def.body)),
                    acc,
                ),
            })
    }

    /// Each nominal declaration's argument plicities, keyed by the family's name — parameters then indices, in the order a use site supplies them.
    ///
    /// Read off the type constructor's own definition, whose declared type is the `FuncType` lowering built from `param_tys ++ index_tys`: the parameters keep their declared marks and the indices are always explicit. That is the only place the marks survive — `InductType` carries none (for a fixed name they are a function of the name, so storing them per-occurrence would be derived data that conversion must then either compare pointlessly or exclude from `Hash`, and excluding them lets hash-consing collapse differently-marked equal nodes), and neither `InductDecl::arity` nor `Telescope` has a slot for them.
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
            if let Subterm::FuncType(FuncType { plicities, .. }) = &*def.type_ {
                marks.insert(def.name.clone(), plicities.clone());
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

    /// Every global qualified name in `self`: each definition (`let`/`rec`), each inductive type, each struct type. The universe a global is shortened *against*.
    pub fn module_symbols(&self) -> Vec<Global> {
        let mut symbols = Vec::new();
        for item in &self.items {
            match item {
                Item::Let(def) => symbols.push(def.name.clone()),
                Item::Rec(rec) => symbols.extend(
                    rec.definitions
                        .iter()
                        .map(|definition| definition.name.clone()),
                ),
            }
        }
        symbols.extend(self.induct_decls.keys().cloned());
        symbols.extend(self.struct_decls.keys().cloned());
        symbols
    }
}

impl fmt::Display for Module {
    // Printed by *iterating* the flat items (never re-folding into a nested term), so `--print core` stays O(N) and cannot re-trigger the prelude-depth overflow this representation removed.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Shortened against this module's own symbols (axis (b)) and nothing else, which is the one shortening site in the workspace that does not union its scope — both checkers' `format_with` take `&[&Module]` and merge. Deliberate, on three grounds. A value printing itself has no scope to be handed without ceasing to be `Display`. No ambiguity can follow from the narrower table: `build_shorten` records only names that actually shorten, and `Spelling::symbol` falls back to the full path, so a name from outside the unit prints qualified rather than misleadingly short. And a dump is read *about* the compiler, where a qualified `/std/Str/concat` beside a bare `append` says which unit each came from — the distinction a scope-wide table would erase. Its universes stay visible for the same reason a diagnostic suppresses them.
        let spelling = Rc::new(
            Spelling::default().with_short_names(Rc::new(build_shorten(&self.module_symbols()))),
        );

        for item in &self.items {
            match item {
                Item::Let(def) => {
                    write!(formatter, "let ")?;
                    def.print(formatter, &spelling)?;
                    writeln!(formatter, ";")?;
                }
                Item::Rec(rec) => {
                    write!(formatter, "rec ")?;
                    for (index, def) in rec.definitions().iter().enumerate() {
                        if index > 0 {
                            write!(formatter, "and ")?;
                        }
                        def.print(formatter, &spelling)?;
                        write!(formatter, " ")?;
                    }
                    writeln!(formatter, ";")?;
                }
            }
        }

        if let Some(body) = &self.body {
            write!(formatter, "{}", body.spelled(&spelling))?;
        }

        if let Some(type_) = &self.type_ {
            write!(formatter, "\n: {}", type_.spelled(&spelling))?;
        }

        Ok(())
    }
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
/// One list, read by every walk that asks what a module carries — the floor below it and the storage refusal beside that. A second copy is how a position quietly stops being covered, which is the failure the enumeration exists to prevent, so a new question about a module's contents is a new `visit` and never a new walk. `None` is the entrypoint, which belongs to the module rather than to any name it declares.
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

    if let Some(type_) = &module.type_ {
        visit(None, type_);
    }
    if let Some(body) = &module.body {
        visit(None, body);
    }
}

/// An identity a stored unit may not carry: one meaningful only in the compilation that assigned it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Positional {
    /// A free local binder. Its index came from one compilation's binder counter, and a compilation restoring the unit seeds its own counter from a floor — so a local surviving into stored output is an index two compilations can both hand out.
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
/// It refuses where [`derived_binder_floor`] reports, over the same positions, and the difference is what each answer is for. A floor is a bound, so a gap in that walk degrades to a wider floor and to nothing worse. An identity reaching a stored unit has no safe direction to degrade in: it aliases silently in whatever compilation restores two such units together, which admits rather than crashes. It still *describes* rather than judges by this module's rule — whether a node is a metavariable, and whether a variable is local, are properties of the representation, taking no reduction, no conversion and no `Env`.
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
        Global::Witness(id) => !module
            .mounts
            .iter()
            .any(|mount| &mount.prefix == id.mount()),
        Global::Authored(_) => false,
    }) {
        return Err(Positional::UnscopedWitness {
            witness: witness.clone(),
        });
    }

    found.map_or(Ok(()), Err)
}

/// One above the highest local binder index any of `module`'s terms mentions — the lowest floor at which a binder a checker mints cannot alias one already in the program.
///
/// Derived rather than believed. [`Module::binder_floor`] carries the elaborator's answer, and nothing checks it, while capture-avoidance depends on it: a checker that opens binders of its own — eta, telescope comparison — and mints one that aliases a free local already in a term silently identifies two terms that differ. Since a floor is a *bound* rather than a verdict, a caller takes the maximum of the two: widening is always safe, so a gap in this walk degrades to the carried value rather than to something worse, and no refusal is needed.
///
/// It lives here by this module's own rule, the one stated at the top: it *describes* rather than judges. Reading the highest index a term mentions asks nothing of a kernel — no reduction, no conversion, no `Env` — so it is a property of the data, and a second implementation would be a second run of the same function rather than a second opinion. That is the standing `UniverseContext::is_closed` has for the same reason.
///
/// Every position that can hold a free local is covered, including ones that in practice never do: each item's type and body, every registry telescope and declared result sort, and the entrypoint's own type and body. Deciding a field cannot matter is the reasoning this walk exists to replace, which is why the positions are enumerated once in [`module_positions`] and read from there rather than listed again here.
pub fn derived_binder_floor(module: &Module) -> usize {
    derived_binder_floor_outside(module, |_| false)
}

/// [`derived_binder_floor`] over only what `in_scope` does not already answer for — the items whose every declared name it holds, and the declarations it names.
///
/// A caller that already has an environment established by an earlier walk has that walk's floor beside it, as a constant computed where the environment was built; the caller maximizes the two. This is the same widening argument the function above rests on, only with one of the two bounds read instead of walked: a floor is a bound, so combining by maximum is safe whatever either side covers.
///
/// The predicate is over *names* rather than over a position, which is what lets one environment answer for four namespaces at once: a name identifies one top-level thing within a module, and an environment populates every namespace it holds from the same source. Skipping is the direction that needs the argument — including an item can only raise the floor, and a higher floor costs freshness rather than correctness — so an item is skipped only when *every* name it declares is already answered for.
pub fn derived_binder_floor_outside(module: &Module, in_scope: impl Fn(&Global) -> bool) -> usize {
    let mut highest: Option<u32> = None;

    module_positions(module, in_scope, |_, carried| {
        for free in carried.free_vars() {
            if let Some(index) = free.local_index() {
                highest = Some(highest.map_or(index, |seen: u32| seen.max(index)));
            }
        }
    });

    highest.map_or(0, |index| index as usize + 1)
}

#[cfg(test)]
mod tests {
    use {super::*, crate::WitnessId, curios_base::RootKind};

    fn definition(name: &str, universe_context: UniverseContext) -> Definition {
        let global = Global::Authored(Qualifier::from([name]));
        Definition {
            name: global.clone(),
            kind: DefinitionKind::Authored,
            universe_context,
            island: Qualifier::empty(),
            totality: Totality::default(),
            type_: Term::type_ground(),
            body: Term::free_var(&Free::from(&global)),
        }
    }

    #[test]
    fn top_level_rec_item_captures_its_exports_into_the_shared_group() {
        let rec = RecItem::new(vec![definition("loop", UniverseContext::empty())]);

        assert!(
            rec.group
                .iter()
                .next()
                .unwrap()
                .body
                .body()
                .free_vars()
                .is_empty()
        );
        assert_eq!(
            rec.group.member_body(0).as_rec_proj(),
            Some((&rec.group, 0)),
            "a member's body sees itself as a projection of its own group"
        );

        let opened = rec.definitions();
        assert_eq!(
            opened[0].body,
            Term::free_var(&Free::global(Qualifier::from(["loop"])))
        );
    }

    #[test]
    fn recursive_members_cannot_silently_discard_different_universe_contexts() {
        let polymorphic = UniverseContext {
            parameter_count: 1,
            constraints: Vec::new(),
        };

        assert_eq!(
            RecItem::try_new(vec![
                definition("left", UniverseContext::empty()),
                definition("right", polymorphic),
            ])
            .unwrap_err(),
            UniverseError::MismatchedRecursiveContexts,
        );
    }

    /// The floor must clear every local a module's terms mention, whatever `Module::binder_floor` claims.
    ///
    /// A binder the kernel mints while comparing under a telescope or eta-contracting aliases a free local the moment the floor is too low, and two terms that differ stop being distinguishable. The carried number is the elaborator's word and nothing checks it, so the walk derives its own and the caller takes the larger.
    #[test]
    fn the_floor_clears_every_local_a_term_mentions() {
        let mentioned = Free::local(4_242, Some("y"));
        let definition = Definition {
            name: Global::Authored(Qualifier::from(["held"])),
            kind: DefinitionKind::Authored,
            universe_context: UniverseContext::empty(),
            island: Qualifier::default(),
            totality: Totality::Total,
            type_: Term::intrinsic(crate::Intrinsic::NatType),
            body: Term::free_var(&mentioned),
        };

        let module = Module {
            items: vec![Item::Let(definition)],
            mounts: Vec::new(),
            universe_seeds: Vec::new(),
            induct_decls: BTreeMap::new(),
            struct_decls: BTreeMap::new(),
            concepts: BTreeMap::new(),
            witnesses: BTreeSet::new(),
            // The understated claim the walk must not believe.
            binder_floor: 0,
            type_: None,
            body: Some(Term::intrinsic(crate::Intrinsic::NatType)),
        };

        assert_eq!(derived_binder_floor(&module), 4_243);
    }

    /// A module carrying `body` in its one definition, and `entrypoint` as the program's own body.
    fn stored(body: Term, entrypoint: Term) -> Module {
        Module {
            items: vec![Item::Let(Definition {
                name: Global::Authored(Qualifier::from(["held"])),
                kind: DefinitionKind::Authored,
                universe_context: UniverseContext::empty(),
                island: Qualifier::default(),
                totality: Totality::Total,
                type_: Term::intrinsic(crate::Intrinsic::NatType),
                body,
            })],
            mounts: Vec::new(),
            universe_seeds: Vec::new(),
            induct_decls: BTreeMap::new(),
            struct_decls: BTreeMap::new(),
            concepts: BTreeMap::new(),
            witnesses: BTreeSet::new(),
            binder_floor: 0,
            type_: None,
            body: Some(entrypoint),
        }
    }

    /// The refusals are the whole of the check's value: one that only ever meets conforming input asserts nothing about what it would do with the other kind.
    ///
    /// Each of these is an identity minted by one compilation's counter. Stored, it is read by a compilation that mints from its own counter and would hand the same index out again — which aliases silently rather than crashing, and is why the seam that writes a unit refuses rather than reports.
    #[test]
    fn a_stored_unit_may_not_carry_a_free_local() {
        let module = stored(
            Term::free_var(&Free::local(7, Some("y"))),
            Term::intrinsic(crate::Intrinsic::NatType),
        );

        assert_eq!(
            validate_stored_identities(&module),
            Err(Positional::FreeLocal {
                owner: Some(Global::Authored(Qualifier::from(["held"]))),
                index: 7,
            })
        );
    }

    #[test]
    fn a_stored_unit_may_not_carry_a_metavariable() {
        let module = stored(
            Term::metavar(crate::MetaId::from(3)),
            Term::intrinsic(crate::Intrinsic::NatType),
        );

        assert_eq!(
            validate_stored_identities(&module),
            Err(Positional::Metavar {
                owner: Some(Global::Authored(Qualifier::from(["held"]))),
            })
        );
    }

    /// Every position the floor walk covers is a position this one covers, because both read the same list. The entrypoint is the position most easily left out of a hand-written list: it belongs to no declared name, so it is the one a walk over `items` misses.
    #[test]
    fn a_stored_unit_may_not_carry_one_in_its_entrypoint() {
        let module = stored(
            Term::intrinsic(crate::Intrinsic::NatType),
            Term::free_var(&Free::local(1, None)),
        );

        assert_eq!(
            validate_stored_identities(&module),
            Err(Positional::FreeLocal {
                owner: None,
                index: 1,
            })
        );
    }

    /// A witness declared here, scoped to a mount this module does not own.
    ///
    /// Before B1 there was nothing to check: an identity was a bare ordinal, so "unscoped" named no state a module could be in. What makes it checkable is that the ordinal now counts *within* a mount — and a module declaring a witness under somebody else's mount is claiming an ordinal in a space it does not own, which two compilations would both hand out.
    #[test]
    fn a_stored_unit_may_not_declare_a_witness_under_a_mount_it_does_not_own() {
        let mut module = stored(
            Term::intrinsic(crate::Intrinsic::NatType),
            Term::intrinsic(crate::Intrinsic::NatType),
        );
        module.mounts = vec![Mount::new(Qualifier::from(["mine"]), RootKind::Ordinary)];
        let witness = Global::Witness(WitnessId::new(Qualifier::from(["theirs"]), 0));
        module.witnesses.insert(witness.clone());

        assert_eq!(
            validate_stored_identities(&module),
            Err(Positional::UnscopedWitness { witness })
        );
    }

    /// The control that keeps the refusal about *declaring*, not about mentioning.
    ///
    /// A stored unit legitimately names witnesses its predecessors declared, scoped to their mounts — every unit compiled against `/std` does. Reading this question off the terms instead of off the declarations would refuse all of them, which is why it is asked over `Module::witnesses` and deliberately not through the position walk.
    #[test]
    fn a_stored_unit_may_name_a_witness_another_mount_declared() {
        let mut module = stored(
            Term::free_var(&Free::Global(Global::Witness(WitnessId::new(
                Qualifier::from(["theirs"]),
                3,
            )))),
            Term::intrinsic(crate::Intrinsic::NatType),
        );
        module.mounts = vec![Mount::new(Qualifier::from(["mine"]), RootKind::Ordinary)];

        assert_eq!(validate_stored_identities(&module), Ok(()));
    }

    /// The control. A free *global* is how one definition names another and is in every stored unit there has ever been, so a check that refused it would refuse the prelude — which is what makes this the test that the refusals above are aimed at something narrower than "a free variable".
    #[test]
    fn a_stored_unit_may_carry_a_global_it_names() {
        let module = stored(
            Term::free_var(&Free::global(Qualifier::from(["elsewhere"]))),
            Term::free_var(&Free::global(Qualifier::from(["elsewhere"]))),
        );

        assert_eq!(validate_stored_identities(&module), Ok(()));
    }
}
