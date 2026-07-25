use {
    super::{
        Concept, InductDecl, Many, RecGroup, RecMemberScopes, Scope, StructDecl, Term,
        UniverseContext, UniverseSeed, build_shorten, with_short_names,
    },
    curios_base::{Qualifier, RootId},
    std::{
        collections::{BTreeMap, BTreeSet},
        fmt,
        rc::Rc,
    },
};

/// How a lowered definition was introduced.
///
/// This is elaboration metadata, not a fact inferred from the flattened
/// qualified name. In particular, a module and a nominal type may share a
/// qualifier without turning ordinary module members into generated nominal
/// members.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum DefinitionKind {
    Authored,
    InductiveType,
    InductiveConstructor { owner: String },
    StructType,
    ConceptType,
    ConceptMethod { owner: String },
    Witness,
}

/// A single top-level definition: `name` bound to `body` of declared `type_`.
///
/// A standalone top-level `let` uses free `Var`s keyed by `name`. A definition
/// returned by [`RecItem::definitions`] is the opened view of a scoped
/// recursive member and likewise uses the group's export names; the
/// authoritative recursive type and body remain in [`RecItem::group`].
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Definition {
    pub name: String,
    pub kind: DefinitionKind,
    pub universe_context: UniverseContext,
    /// This definition's declaring module — `name`'s qualifier prefix,
    /// precomputed once by `into_core` (before `name` was flattened) rather
    /// than re-derived from it later. Stamped into `Context::island` per item
    /// by `elaborate_module` for the representation-privacy checks (§7), which
    /// test subtree containment against it rather than equality;
    /// the same value `Structure::module` carries for type declarations.
    /// Islands are surface-elaboration state: erasure re-derives types with
    /// privacy suppressed and never stamps them.
    pub island: Qualifier,
    /// This definition's declaring root — `island`'s leading segment,
    /// precomputed once by `into_core` the same way `Concept`/`StructDecl`/
    /// `InductDecl` are, so `Context::set_island` (and, downstream, the
    /// orphan-rule check) never has to re-derive it from `island` itself.
    pub root: RootId,
    pub type_: Term,
    pub body: Term,
}

/// Export metadata for one member of a flat top-level recursive group. The
/// member's type and body live only in [`RecItem::group`], scoped over every
/// export in the group.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub(crate) struct RecDefinition {
    pub name: String,
    pub kind: DefinitionKind,
    pub island: Qualifier,
    pub root: RootId,
}

/// A flat top-level recursive item backed by the same structural fixed-point
/// representation as a local [`super::Rec`]. Keeping the export metadata
/// separate preserves the module's flat architecture without retaining a
/// second, free-name copy of each recursive type and body.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct RecItem {
    pub(crate) definitions: Vec<RecDefinition>,
    pub(crate) group: RecGroup,
}

impl RecItem {
    pub fn new(definitions: Vec<Definition>) -> Self {
        Self::try_new(definitions).expect("a recursive group has one valid universe context")
    }

    pub fn try_new(definitions: Vec<Definition>) -> Result<Self, super::UniverseError> {
        let universe_context = definitions
            .first()
            .map(|definition| definition.universe_context.clone())
            .unwrap_or_default();
        universe_context.validate()?;
        if !definitions
            .iter()
            .all(|definition| definition.universe_context == universe_context)
        {
            return Err(super::UniverseError::MismatchedRecursiveContexts);
        }
        let labels = definitions
            .iter()
            .map(|definition| definition.name.as_str())
            .collect::<Vec<_>>();
        let arity = Many(labels.len());
        let group = RecGroup::new(
            definitions
                .iter()
                .map(|definition| RecMemberScopes {
                    type_: Scope::close(arity, &labels, definition.type_.clone()),
                    body: Scope::close(arity, &labels, definition.body.clone()),
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
            })
            .collect();

        Ok(Self { definitions, group })
    }

    /// Open the recursive scopes against their exported names.
    ///
    /// The returned definitions are a read-only projection; the authoritative
    /// types and bodies remain structurally shared in the group's scheme.
    pub fn definitions(&self) -> Vec<Definition> {
        let names = self
            .definitions
            .iter()
            .map(|definition| Term::free_var(&definition.name))
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
                type_: member.type_.open(&name_refs),
                body: member.body.open(&name_refs),
            })
            .collect()
    }

    pub(crate) fn island(&self) -> Qualifier {
        self.definitions
            .first()
            .map(|definition| definition.island.clone())
            .unwrap_or_default()
    }
}

impl Definition {
    fn print(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{} : {} = {}", self.name, self.type_, self.body)
    }
}

/// A top-level item: a single `let` definition, or a `rec` group of
/// mutually-recursive definitions (which may reference each other by `name`).
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
    /// The names exported by this top-level item, in declaration order.
    pub fn declared_names(&self) -> Vec<&str> {
        match self {
            Item::Let(definition) => vec![definition.name.as_str()],
            Item::Rec(rec) => rec
                .definitions
                .iter()
                .map(|definition| definition.name.as_str())
                .collect(),
        }
    }
}

/// The whole program as a *flat* list of top-level `items`, the entrypoint
/// `body`, and its optional `type_` annotation.
///
/// This replaces the single, N-deep nested `Subterm::Let`/`Rec` term that
/// `text::into_core` used to fold the entire prelude into — the construction
/// (`Scope::close` over the whole accumulator at each step) and every pass that
/// recursed along its `.tail` spine were both O(N) in stack and overflowed at
/// prelude depth (BUG.md). `Subterm::Let`/`Rec` remain for genuine *local*,
/// in-expression bindings, which are shallow.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Module {
    pub items: Vec<Item>,
    /// Lowering-time metadata for every universe metavariable id in this
    /// module. Finalized, zonked modules clear this vector.
    pub universe_seeds: Vec<UniverseSeed>,
    /// Inductive declarations' registry entries, keyed by the type's qualified
    /// name. Carried on the module — not on a `Context` — because elaboration
    /// and erasure each run with their *own* `Context` (see `run::compile`);
    /// both seed their context's flat inductive store from here on entry.
    pub induct_decls: BTreeMap<String, InductDecl>,
    /// Struct declarations' registry entries, keyed by the type's qualified
    /// name. Carried on the module like `induct_decls` (and for the same reason):
    /// elaboration and erasure each seed their own `Context` from here on entry.
    pub struct_decls: BTreeMap<String, StructDecl>,
    /// Concept declarations' resolution metadata, keyed by the concept's
    /// qualified name (each concept's record shape also lives in
    /// `struct_decls`). Seeded into the elaboration `Context` on entry; erasure
    /// never consults it.
    pub concepts: BTreeMap<String, Concept>,
    /// The definition names that are witness declarations. Elaboration
    /// registers each into the witness table when its signature elaborates —
    /// carried as names (not keys) because the table key needs the
    /// *elaborated* head, which only exists once elaboration runs.
    pub witnesses: BTreeSet<String>,
    pub type_: Option<Term>,
    pub body: Term,
}

impl Module {
    /// Re-fold the flat module into the legacy nested `Let`/`Rec` `Term` it
    /// replaced (items are already in binding order). Test-only: lets the
    /// `into_core`/`erase` suites keep asserting against the historical shape — and
    /// keep feeding a single `Term` to `erase` — without rewriting every
    /// expectation. Drops `type_` (the old `run` helper only returned the term).
    /// Not `#[cfg(test)]`: its callers live in `curios`'s test suite, a
    /// different crate, where that cfg would never activate.
    pub fn into_nested_term(self) -> Term {
        self.items
            .into_iter()
            .rev()
            .fold(self.body, |acc, item| match item {
                Item::Let(def) => Term::let_(def.name, def.type_, def.body, acc),
                Item::Rec(rec) => Term::rec(
                    rec.definitions()
                        .into_iter()
                        .map(|def| (def.name, def.type_, def.body)),
                    acc,
                ),
            })
    }

    /// Every global qualified name in `self`: each definition (`let`/`rec`), each
    /// inductive type, each struct type. The universe a global is shortened *against*.
    pub(crate) fn module_symbols(&self) -> Vec<String> {
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
    // Printed by *iterating* the flat items (never re-folding into a nested term),
    // so `--print core` stays O(N) and cannot re-trigger the prelude-depth
    // overflow this representation removed.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        with_short_names(Rc::new(build_shorten(&self.module_symbols())), || {
            for item in &self.items {
                match item {
                    Item::Let(def) => {
                        write!(formatter, "let ")?;
                        def.print(formatter)?;
                        writeln!(formatter, ";")?;
                    }
                    Item::Rec(rec) => {
                        write!(formatter, "rec ")?;
                        for (index, def) in rec.definitions().iter().enumerate() {
                            if index > 0 {
                                write!(formatter, "and ")?;
                            }
                            def.print(formatter)?;
                            write!(formatter, " ")?;
                        }
                        writeln!(formatter, ";")?;
                    }
                }
            }

            write!(formatter, "{}", self.body)?;

            if let Some(type_) = &self.type_ {
                write!(formatter, "\n: {type_}")?;
            }

            Ok(())
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn definition(name: &str, universe_context: UniverseContext) -> Definition {
        Definition {
            name: name.into(),
            kind: DefinitionKind::Authored,
            universe_context,
            island: Qualifier::empty(),
            root: RootId::Entry,
            type_: Term::type_ground(),
            body: Term::free_var(name),
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
        assert!(matches!(
            &*rec.group.member_body(0),
            super::super::Subterm::RecMember(member)
                if member.index == 0 && member.group == rec.group
        ));

        let opened = rec.definitions();
        assert_eq!(opened[0].body, Term::free_var("loop"));
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
            crate::UniverseError::MismatchedRecursiveContexts,
        );
    }
}
