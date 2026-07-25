use {
    super::PublicInterface,
    crate::{Error, Name, SyntaxRegistry},
    curios_base::{Entropy, Qualifier, RootId, Span},
    curios_core::{
        DefinitionKind, Level, UniverseConstraintKind, UniverseConstraintOrigin, UniverseContext,
        UniverseMetaId, UniverseRole, UniverseSeed,
    },
    std::{
        cell::{Cell, RefCell},
        collections::{HashMap, HashSet},
    },
};

#[derive(Clone)]
pub(super) struct FlatLet {
    pub name: Qualifier,
    pub kind: DefinitionKind,
    pub island: Qualifier,
    pub root: RootId,
    pub type_: curios_core::Term,
    pub body: curios_core::Term,
}

impl FlatLet {
    pub(super) fn into_core(self) -> curios_core::Definition {
        curios_core::Definition {
            root: self.root,
            island: self.island,
            name: self.name.join(),
            kind: self.kind,
            universe_context: UniverseContext::empty(),
            type_: self.type_,
            body: self.body,
        }
    }
}

#[derive(Clone)]
pub(super) enum FlatItem {
    Let(FlatLet),
    Rec(Vec<FlatLet>),
}

impl FlatItem {
    /// Whether this flat item belongs to the fixed embedded prelude — every
    /// let in it is declared under a privileged root
    /// (`RootId::of_segment(..).kind()`). Checked on the *structured*
    /// qualifier's root segment, before names are flattened to strings.
    pub(super) fn in_prelude(&self) -> bool {
        let lets = match self {
            FlatItem::Let(let_) => std::slice::from_ref(let_),
            FlatItem::Rec(lets) => lets.as_slice(),
        };

        !lets.is_empty() && lets.iter().all(|let_| let_.root.kind().is_privileged())
    }

    pub(super) fn names(&self) -> Vec<String> {
        match self {
            FlatItem::Let(let_) => vec![let_.name.join()],
            FlatItem::Rec(lets) => lets.iter().map(|let_| let_.name.join()).collect(),
        }
    }

    pub(super) fn free_vars(&self) -> HashSet<String> {
        let lets = match self {
            FlatItem::Let(let_) => std::slice::from_ref(let_),
            FlatItem::Rec(lets) => lets.as_slice(),
        };

        lets.iter()
            .flat_map(|let_| {
                // Construction head names (`Struct`/`Variant`/type-former normal
                // forms) are reachability edges too — a body that *builds* a
                // struct (the string-literal meta-emitter's `/syn/Str/Str`) must
                // keep its backing type-former and field-type definitions alive
                // even though no `Var` names them. See `Subterm::construction_names`.
                let_.type_
                    .free_vars()
                    .into_iter()
                    .chain(let_.body.free_vars())
                    .chain(let_.type_.construction_names())
                    .chain(let_.body.construction_names())
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

// The direct interface of a module: every declared label (public *and* private)
// in each namespace, with its visibility. This is the per-module body view used
// for lexical scope during elaboration, and to tell private from absent when a
// public lookup misses.
#[derive(Clone, Copy)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
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
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub(super) struct ModuleInfo {
    pub(super) root: RootId,
    #[cfg_attr(feature = "archive", rkyv(with = crate::OrderedMap))]
    children: HashMap<String, ChildInfo>,
    #[cfg_attr(feature = "archive", rkyv(with = crate::OrderedMap))]
    bindings: HashMap<String, bool>,
}

impl ModuleInfo {
    pub(super) fn new(root: RootId) -> Self {
        Self {
            root,
            children: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    pub(super) fn insert_child(&mut self, label: String, vis_pub: bool) -> Result<(), Error> {
        if self.children.contains_key(&label) {
            return Err(Error::DuplicatePublicDeclaration { label });
        }

        self.children.insert(label, ChildInfo::Ordinary { vis_pub });
        Ok(())
    }

    pub(super) fn insert_induct_child(
        &mut self,
        label: String,
        vis_pub: bool,
        rep_pub: bool,
    ) -> Result<(), Error> {
        if self.children.contains_key(&label) {
            return Err(Error::DuplicatePublicDeclaration { label });
        }

        self.children
            .insert(label, ChildInfo::InductConstructors { vis_pub, rep_pub });
        Ok(())
    }

    pub(super) fn insert_binding(&mut self, label: String, vis_pub: bool) -> Result<(), Error> {
        if self.bindings.contains_key(&label) {
            return Err(Error::DuplicatePublicDeclaration { label });
        }

        self.bindings.insert(label, vis_pub);
        Ok(())
    }

    pub(super) fn get_child(&self, label: &str) -> Option<bool> {
        self.children.get(label).copied().map(ChildInfo::is_public)
    }

    pub(super) fn is_opaque_constructor_child(&self, label: &str) -> bool {
        self.children
            .get(label)
            .copied()
            .is_some_and(ChildInfo::is_opaque_constructor_namespace)
    }

    pub(super) fn get_binding(&self, label: &str) -> Option<bool> {
        self.bindings.get(label).copied()
    }

    /// Every declared child module with its own visibility bit, for the
    /// audience fixed point — which needs the private ones too, since they are
    /// visible within this module's subtree.
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

// The per-body elaboration context. `table`/`public` are frozen interface views,
// shared read-only across all nested contexts. `qualifiers`/`bindings` are the
// lexical scope of the module body being elaborated, populated source-ordered by
// declarations and `use` imports.
pub(super) struct Context<'a> {
    prefix: Qualifier,
    // The root this module's own subtree belongs to — set explicitly once,
    // where a root is mounted (`Context::new`/`nested_root`), and inherited
    // unchanged by ordinary nesting (`nested`). Never re-derived from
    // `prefix`'s string content past that point.
    root: RootId,
    table: &'a HashMap<Qualifier, ModuleInfo>,
    public: &'a HashMap<Qualifier, PublicInterface>,
    qualifiers: HashMap<String, Qualifier>,
    bindings: HashMap<String, Qualifier>,
    // Shared, program-global metavariable-id counter. The whole program folds
    // into one `curios_core::Term`, so holes in different module bodies (each its own
    // `Context` via `nested`) must draw from the same monotonic source. Shared
    // by reference (like `table`/`public`) and `Cell`-backed so it survives
    // `Lowerer`'s immutable `&Context` borrow.
    metavars: &'a Entropy,
    universes: &'a Entropy,
    universe_role: &'a Cell<UniverseRole>,
    universe_seeds: &'a RefCell<Vec<UniverseSeed>>,
    universe_allocations: &'a RefCell<HashMap<Span, UniverseMetaId>>,
    // Sibling counter for fresh continuation-binder names minted while desugaring
    // `!` regions. Threaded (not a process-global atomic) for determinism:
    // `curios_core::Scope`'s `PartialEq` compares binder *names*, so term-equality in
    // tests must be order-stable.
    binders: &'a Entropy,
    syntax: &'a SyntaxRegistry,
}

impl<'a> Context<'a> {
    #[allow(clippy::too_many_arguments)]
    pub(super) fn new(
        table: &'a HashMap<Qualifier, ModuleInfo>,
        public: &'a HashMap<Qualifier, PublicInterface>,
        root: RootId,
        metavars: &'a Entropy,
        universes: &'a Entropy,
        universe_role: &'a Cell<UniverseRole>,
        universe_seeds: &'a RefCell<Vec<UniverseSeed>>,
        universe_allocations: &'a RefCell<HashMap<Span, UniverseMetaId>>,
        binders: &'a Entropy,
        syntax: &'a SyntaxRegistry,
    ) -> Context<'a> {
        Context {
            prefix: Qualifier::empty(),
            root,
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
            syntax,
        }
    }

    pub(super) fn nested(&self, label: &str) -> Context<'a> {
        Context {
            prefix: self.prefix.with(label),
            root: self.root,
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
            syntax: self.syntax,
        }
    }

    /// Like `nested`, but for descending into one of the fixed roots
    /// (`sys`/`syn`/`std`) mounted under the entry program: `root` overrides
    /// rather than inherits, since the fixed root's own subtree belongs to a
    /// different root than its mounting context. Used only at the three
    /// `into_core::FIXED_ROOTS` mount points — every deeper `nested` call
    /// within that subtree inherits the overridden root unchanged.
    pub(super) fn nested_root(&self, label: &str, root: RootId) -> Context<'a> {
        Context {
            root,
            ..self.nested(label)
        }
    }

    /// The `RootId` this module itself belongs to — the value every `FlatLet`
    /// declared directly in this module's body stamps as its own root.
    pub(super) fn root(&self) -> RootId {
        self.root
    }

    /// Mint a fresh, program-globally-unique metavariable id for a surface hole.
    pub(super) fn fresh_metavar(&self) -> usize {
        self.metavars.fresh()
    }

    pub(super) fn fresh_universe(&self, span: Option<&Span>) -> Level {
        if let Some(span) = span
            && let Some(id) = self.universe_allocations.borrow().get(span)
        {
            assert_eq!(
                self.universe_seeds.borrow()[id.0].role,
                self.universe_role(),
                "one written Type was lowered under conflicting universe roles"
            );
            return Level::meta(*id);
        }

        let id = self.universes.fresh();
        let mut seeds = self.universe_seeds.borrow_mut();
        assert_eq!(id, seeds.len(), "universe seeds parallel their dense ids");
        seeds.push(UniverseSeed {
            role: self.universe_role(),
            origin: span.cloned().map(|span| UniverseConstraintOrigin {
                span: Some(span),
                kind: UniverseConstraintKind::WrittenType,
                declaration: (!self.prefix.segments().is_empty()).then(|| self.prefix.join()),
                binder: None,
            }),
        });
        let id = UniverseMetaId(id);
        if let Some(span) = span {
            self.universe_allocations
                .borrow_mut()
                .insert(span.clone(), id);
        }
        Level::meta(id)
    }

    pub(super) fn universe_role(&self) -> UniverseRole {
        self.universe_role.get()
    }

    pub(super) fn with_universe_role<T>(
        &self,
        role: UniverseRole,
        f: impl FnOnce() -> Result<T, Error>,
    ) -> Result<T, Error> {
        let old = self.universe_role.replace(role);
        let result = f();
        self.universe_role.set(old);
        result
    }

    /// Mint a fresh continuation-binder name for `!` desugaring. The `#`
    /// sigil is illegal in source identifiers, so it cannot collide with user
    /// names or qualified references.
    pub(super) fn fresh_binder(&self) -> String {
        format!("#{}", self.binders.fresh())
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

    // Walk from `start` through `segments` as child modules visible to this
    // module, following each entry's re-export target. A failing segment is
    // classified against the direct table: present-but-private vs. absent.
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

    // Resolve the module named by `name`'s first `upto` segments: from the module
    // root (absolute) or the lexically-bound head qualifier (relative — the head
    // is consumed as the start, so the walk runs over `segments[1..upto]`). Guards
    // the *resolved* module, so a relative spelling is rejected exactly as the
    // absolute one is.
    fn resolve_module_prefix(&self, name: &Name, upto: usize) -> Result<Qualifier, Error> {
        let segments = name.qualifier().segments();

        let resolved = if name.is_abs() {
            self.walk_children(Qualifier::empty(), &segments[..upto])?
        } else {
            let head = name.head();
            let start = self
                .qualifiers
                .get(head)
                .ok_or_else(|| Error::UnresolvedQualifier {
                    qualifier: head.to_string(),
                })?
                .clone();

            self.walk_children(start, &segments[1..upto])?
        };

        super::guard_internal_root(self.table, &self.prefix, resolved.segments())?;
        Ok(resolved)
    }

    // The module that should contain `name`'s final segment, plus that segment.
    fn resolve_parent_path(&self, name: &Name) -> Result<(Qualifier, String), Error> {
        let last = name.qualifier().segments().len() - 1;
        let parent = self.resolve_module_prefix(name, last)?;
        Ok((parent, name.last().to_string()))
    }

    // Import the module child `label` out of `parent`, registering it as a
    // qualifier in the current lexical scope.
    fn import_module_label(&mut self, parent: &Qualifier, label: &str) -> Result<Qualifier, Error> {
        match super::interface::visible_child(self.public, self.table, &self.prefix, parent, label)
        {
            Some(target) => {
                self.insert_scope(label.to_string(), target.clone())?;
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

    // Import the binding `label` out of `parent`, registering it in the current
    // lexical scope.
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

    // Import both the module and binding slots of `label` — used by glob and the
    // `Both` group item. Either or both may be absent.
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
            result.module = Some(target);
        }

        if let Some(target) = binding {
            self.insert_binding(label.to_string(), target.clone())?;
            result.binding = Some(target);
        }

        Ok(result)
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

    // A glob `use a/b/*` names a module directly and imports every public child
    // and binding it exposes (including its re-exports), each under its own label.
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

    // Resolve a qualified/absolute term reference to its canonical binding
    // target, reading the frozen public interfaces.
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
                Some(target) => Ok(target),
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

fn attach(error: Error, name: &Name) -> Error {
    match name.span() {
        Some(span) => error.at(span.clone()),
        None => error,
    }
}
