//! The finished program: a flat list of top-level [`Item`]s over the nominal registries they are checked against.
//!
//! This is what a checker is handed. Elaboration produces it and erasure consumes it, but the shape itself is representation — a [`Definition`] is a name, a universe context, a type, and a body, and a [`RecItem`] is the same for a recursive group whose members reference each other through one shared [`RecGroup`] binder rather than through free names. Both checkers walk this structure, which is why it lives here rather than beside either of them.
//!
//! Items are stored in binding order and read in dependency order. A [`Module`] additionally carries the registries an item's types may name ([`InductDecl`], [`StructDecl`], [`ConceptDecl`]), the witness set, the binder high-water mark a checker must seed above, and the entrypoint's own type and body.
//!
//! Well-formedness that *judges* rather than describes is not decided here. Whether a universe context is satisfiable runs a solver and belongs to `curios-elab`; whether a definition terminates runs the size-change engine and belongs to `curios-cert`. [`Totality`] is the classification those judgments record onto a definition, and the enum lives here because the field does.

use {
    super::{
        Atom, ConceptDecl, Free, FuncType, Global, InductDecl, Many, RecGroup, RecMemberScopes,
        Scope, Sharing, Spelling, StructDecl, Subterm, Term, UniverseContext, UniverseError,
        UniverseSeed, build_shorten, project_erased_universes,
    },
    curios_base::{Plicity, Qualifier, RootId},
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
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
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
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
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
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Definition {
    pub name: Global,
    pub kind: DefinitionKind,
    pub universe_context: UniverseContext,
    /// This definition's declaring module — `name`'s qualifier prefix, precomputed once by `into_core` (before `name` was flattened) rather than re-derived from it later. Stamped into `Context::island` per item by `elaborate_module_suffix` for the representation-privacy checks, which test subtree containment against it rather than equality; the same value `Structure::module` carries for type declarations. Islands are surface-elaboration state: erasure re-derives types with privacy suppressed and never stamps them.
    pub island: Qualifier,
    /// This definition's declaring root — `island`'s leading segment, precomputed once by `into_core` the same way `ConceptDecl`/`StructDecl`/ `InductDecl` are, so `Context::set_island` (and, downstream, the orphan-rule check) never has to re-derive it from `island` itself.
    pub root: RootId,
    /// Whether this definition terminates on every input, together with everything it reaches. Written back by `crate::record_totality` after zonking — like `polarities` on a declaration, and for the same reason: the analysis needs final, meta-free terms, so construction cannot know the answer. It defaults to [`Totality::Partial`], which is what makes a site that forgets to stamp it fail closed rather than open.
    ///
    /// This is the cross-module summary the erasure gates read. A user program that mentions a prelude definition inherits the flag rather than re-analyzing the prelude, which is sound because "partial" already means "something partial is in its closure".
    pub totality: Totality,
    pub type_: Term,
    pub body: Term,
}

/// Export metadata for one member of a flat top-level recursive group. The member's type and body live only in [`RecItem::group`], scoped over every export in the group.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct RecDefinition {
    pub name: Global,
    pub kind: DefinitionKind,
    pub island: Qualifier,
    pub root: RootId,
    /// Per member, not per group. The group's *descent* is decided once for all of them, but the transitive closure is not: an accepted group can still have one member that reaches something partial while its sibling does not. See [`Definition::totality`].
    pub totality: Totality,
}

/// A flat top-level recursive item backed by the same structural fixed-point representation as a local [`super::Rec`]. Keeping the export metadata separate preserves the module's flat architecture without retaining a second, free-name copy of each recursive type and body.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
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
                root: definition.root,
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
                root: definition.root,
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
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
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
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Module {
    pub items: Vec<Item>,
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
    pub body: Term,
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
            root: definition.root,
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
                                root: member.root,
                                totality: member.totality,
                            })
                            .collect(),
                        // Mapped in place rather than opened and re-closed: the round trip rebuilds every node twice and drops every memoized derivation with it, and the rebuilt nodes would escape this very pass.
                        group: rec.group.map_members(|term| sharing.share(term)),
                    }),
                })
                .collect(),
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
            body: sharing.share(&self.body),
        }
    }

    /// Re-fold the flat module into the legacy nested `Let`/`Rec` `Term` it replaced (items are already in binding order). Test-only: lets the `into_core`/`erase` suites keep asserting against the historical shape — and keep feeding a single `Term` to `erase` — without rewriting every expectation. Drops `type_` (the old `run` helper only returned the term). Not `#[cfg(test)]`: its callers live in `curios`'s test suite, a different crate, where that cfg would never activate.
    pub fn into_nested_term(self) -> Term {
        self.items
            .into_iter()
            .rev()
            .fold(self.body, |acc, item| match item {
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
        // Shortened against this module's own symbols (axis (b)) and nothing else: a dump is read *about* the compiler, so its universes stay visible where a diagnostic would suppress them.
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

        write!(formatter, "{}", self.body.spelled(&spelling))?;

        if let Some(type_) = &self.type_ {
            write!(formatter, "\n: {}", type_.spelled(&spelling))?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn definition(name: &str, universe_context: UniverseContext) -> Definition {
        let global = Global::Authored(Qualifier::from([name]));
        Definition {
            name: global.clone(),
            kind: DefinitionKind::Authored,
            universe_context,
            island: Qualifier::empty(),
            root: RootId::Entry,
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
}
