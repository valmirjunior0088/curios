use {
    super::PublicInterface,
    super::Scoped,
    crate::{Error, Name},
    curios_utilities::{Entropy, Mount, Qualifier, Span, SyntaxRegistry},
    std::{
        cell::{Cell, RefCell},
        collections::{BTreeMap, HashMap, HashSet},
    },
};

#[derive(Clone)]
pub(super) struct FlatLet {
    pub name: curios_core::Global,
    pub kind: curios_core::DefinitionKind,
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
            // Lowering cannot know this. `curios_elab::record_totality` computes the definition's totality after elaboration and zonking and writes it back here.
            totality: curios_core::Totality::default(),
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
    /// Whether this flat item belongs to the fixed embedded prelude — every let in it is declared under a privileged mount. Asked of each let's declaring module against the mount table, so the answer comes from what this compilation actually mounted rather than from the spelling of a leading segment.
    pub(super) fn in_prelude(&self, mounts: &[Mount]) -> bool {
        let lets = match self {
            FlatItem::Let(let_) => std::slice::from_ref(let_),
            FlatItem::Rec(lets) => lets.as_slice(),
        };

        !lets.is_empty()
            && lets
                .iter()
                .all(|let_| Mount::privileged(mounts, &let_.island))
    }

    pub(super) fn names(&self) -> Vec<curios_core::Global> {
        match self {
            FlatItem::Let(let_) => vec![let_.name.clone()],
            FlatItem::Rec(lets) => lets.iter().map(|let_| let_.name.clone()).collect(),
        }
    }

    /// Every global this item names — the reachability prune's edges (see [`FlatItem::names`]). Locals are dropped: an item's binders are its own business, and reachability is a question about definitions.
    pub(super) fn free_vars(&self) -> HashSet<curios_core::Global> {
        let lets = match self {
            FlatItem::Let(let_) => std::slice::from_ref(let_),
            FlatItem::Rec(lets) => lets.as_slice(),
        };

        lets.iter()
            .flat_map(|let_| {
                // Construction head names (`Struct`/`Variant`/type-former normal forms) are reachability edges too — a body that *builds* a struct (the string-literal meta-emitter's `/syn/Str/Str`) must keep its backing type-former and field-type definitions alive even though no `Var` names them. See `Subterm::construction_names`.
                let_.type_
                    .free_vars()
                    .into_iter()
                    .chain(let_.body.free_vars())
                    .filter_map(|name| name.as_global().cloned())
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
pub(super) struct ModuleInfo {
    #[archived_with(crate::OrderedMap)]
    children: HashMap<String, ChildInfo>,
    #[archived_with(crate::OrderedMap)]
    bindings: HashMap<String, bool>,
}

impl ModuleInfo {
    pub(super) fn new() -> Self {
        Self {
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

// The per-body elaboration context. `table`/`public` are frozen interface views, shared read-only across all nested contexts. `qualifiers`/`bindings` are the lexical scope of the module body being elaborated, populated source-ordered by declarations and `use` imports.
pub(super) struct Context<'a> {
    prefix: Qualifier,
    // Every prefix this compilation mounts, with its privilege tier. Shared read-only by every nested context, like `table`/`public`: which mount owns a module is `Mount::owning` over its qualifier, so nesting carries nothing about roots and nothing re-derives one from a string.
    mounts: &'a [Mount],
    table: &'a Scoped<'a, ModuleInfo>,
    public: &'a Scoped<'a, PublicInterface>,
    qualifiers: HashMap<String, Qualifier>,
    bindings: HashMap<String, Qualifier>,
    // Shared, program-global metavariable-id counter. The whole program folds into one `curios_core::Term`, so holes in different module bodies (each its own `Context` via `nested`) must draw from the same monotonic source. Shared by reference (like `table`/`public`) and `Cell`-backed so it survives `Lowerer`'s immutable `&Context` borrow.
    metavars: &'a Entropy,
    universes: &'a Entropy,
    universe_role: &'a Cell<curios_core::UniverseRole>,
    universe_seeds: &'a RefCell<Vec<curios_core::UniverseSeed>>,
    universe_allocations: &'a RefCell<HashMap<Span, curios_core::UniverseMetaId>>,
    // Shared counter for every binder identity a lowered term closes over. Threaded (not a process-global atomic) for determinism: two runs over the same source must mint the same identities, or terms that should be equal would differ.
    binders: &'a Entropy,
    // One ordinal counter per mount. A `satisfy` declaration is anonymous, so its identity is minted rather than written — and it is scoped to the mount that declares it, because an ordinal alone would mean something only in the compilation that handed it out.
    witnesses: &'a RefCell<BTreeMap<Qualifier, u32>>,
    // Every bare name that resolved to nothing, keyed by the binder identity it lowered to, with the public bindings in scope of that name. Shared across nested contexts like the counters, because the table is the unit's: `curios-elab` reports the unbound binder, and this is what lets its report say what the reader probably meant.
    unbound: &'a RefCell<BTreeMap<curios_core::Free, Vec<Qualifier>>>,
    syntax: &'a SyntaxRegistry,
}

impl<'a> Context<'a> {
    #[allow(clippy::too_many_arguments)]
    pub(super) fn new(
        table: &'a Scoped<'a, ModuleInfo>,
        public: &'a Scoped<'a, PublicInterface>,
        mounts: &'a [Mount],
        metavars: &'a Entropy,
        universes: &'a Entropy,
        universe_role: &'a Cell<curios_core::UniverseRole>,
        universe_seeds: &'a RefCell<Vec<curios_core::UniverseSeed>>,
        universe_allocations: &'a RefCell<HashMap<Span, curios_core::UniverseMetaId>>,
        binders: &'a Entropy,
        witnesses: &'a RefCell<BTreeMap<Qualifier, u32>>,
        unbound: &'a RefCell<BTreeMap<curios_core::Free, Vec<Qualifier>>>,
        syntax: &'a SyntaxRegistry,
    ) -> Context<'a> {
        Context {
            prefix: Qualifier::empty(),
            mounts,
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
            witnesses,
            unbound,
            syntax,
        }
    }

    pub(super) fn nested(&self, label: &str) -> Context<'a> {
        Context {
            prefix: self.prefix.with(label),
            mounts: self.mounts,
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
            witnesses: self.witnesses,
            unbound: self.unbound,
            syntax: self.syntax,
        }
    }

    /// Mint a fresh, program-globally-unique metavariable id for a surface hole.
    pub(super) fn fresh_metavar(&self) -> usize {
        self.metavars.fresh()
    }

    pub(super) fn fresh_universe(&self, span: Option<&Span>) -> curios_core::Level {
        if let Some(span) = span
            && let Some(id) = self.universe_allocations.borrow().get(span)
        {
            assert_eq!(
                self.universe_seeds.borrow()[id.0].role,
                self.universe_role(),
                "one written Type was lowered under conflicting universe roles"
            );
            return curios_core::Level::meta(*id);
        }

        let id = self.universes.fresh();
        let mut seeds = self.universe_seeds.borrow_mut();
        assert_eq!(id, seeds.len(), "universe seeds parallel their dense ids");
        seeds.push(curios_core::UniverseSeed {
            role: self.universe_role(),
            origin: span
                .cloned()
                .map(|span| curios_core::UniverseConstraintOrigin {
                    span: Some(span),
                    kind: curios_core::UniverseConstraintKind::WrittenType,
                    declaration: (!self.prefix.is_root()).then(|| self.prefix.join()),
                    binder: None,
                }),
        });
        let id = curios_core::UniverseMetaId(id);
        if let Some(span) = span {
            self.universe_allocations
                .borrow_mut()
                .insert(span.clone(), id);
        }
        curios_core::Level::meta(id)
    }

    pub(super) fn universe_role(&self) -> curios_core::UniverseRole {
        self.universe_role.get()
    }

    pub(super) fn with_universe_role<T>(
        &self,
        role: curios_core::UniverseRole,
        f: impl FnOnce() -> Result<T, Error>,
    ) -> Result<T, Error> {
        let old = self.universe_role.replace(role);
        let result = f();
        self.universe_role.set(old);
        result
    }

    /// Mint a witness identity under the mount that declares it.
    ///
    /// A `satisfy` declaration is anonymous by design, so it gets an identity rather than a manufactured name — see [`curios_core::Global::Witness`]. The ordinal is per mount rather than per program: two units both counting from zero is exactly what makes a bare ordinal unstorable, and the mount is what makes the pair disjoint without either unit knowing the other exists.
    ///
    /// The mount is the one this context's prefix lies within. A prefix owned by nothing is the synthetic compilation root, which only arises while no unit has claimed anything yet.
    pub(super) fn fresh_witness(&self) -> curios_core::WitnessId {
        let mount = Mount::owning(self.mounts, &self.prefix)
            .map(|mount| mount.prefix.clone())
            .unwrap_or_default();

        let mut witnesses = self.witnesses.borrow_mut();
        let ordinal = witnesses.entry(mount.clone()).or_default();
        let minted = curios_core::WitnessId::new(mount, *ordinal);
        *ordinal += 1;

        minted
    }

    /// Mint a binder identity, rendering as `hint`.
    ///
    /// Every binder a lowered term closes over comes from here, including the continuation binders `!` desugaring introduces. `curios-elab` mints more while elaborating and seeds its counter above [`PreparedText::binder_floor`](super::PreparedText::binder_floor), so the two sources share one identity space without colliding.
    pub(super) fn fresh_binder(&self, hint: Option<&str>) -> curios_core::Free {
        curios_core::Free::local(
            u32::try_from(self.binders.fresh()).expect("binder space exhausted"),
            hint,
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
        self.prefix.clone()
    }

    pub(super) fn bindings(&self) -> &HashMap<String, Qualifier> {
        &self.bindings
    }

    /// Mint the binder an unresolved bare name lowers to, and record beside it every public binding in scope that carries the name — as the absolute path a reader could write. Read off the public interfaces, where a `pub use` is a binding of the re-exporting module, rather than the declaration table, because the facade's spelling is the one a reader is told everything comes from: `/std/Bool` is `pub use Bool/{let Bool}`, and Core knows only the `/sys/Bool/Bool` it stands for. Only the shortest paths are kept — a longer one is the same name through a deeper module — in the path's own order, which is deterministic and nothing more.
    pub(super) fn unbound_binder(&self, label: &str) -> curios_core::Free {
        let mut found = self
            .public
            .iter()
            .filter(|(_, interface)| interface.bindings.contains_key(label))
            .map(|(module, _)| module.with(label))
            .collect::<Vec<_>>();
        found.sort_by_key(|path| (path.segments().len(), path.join()));
        let shortest = found.first().map(|path| path.segments().len());
        found.retain(|path| Some(path.segments().len()) == shortest);

        let binder = self.fresh_binder(Some(label));
        self.unbound.borrow_mut().insert(binder.clone(), found);
        binder
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

    // Walk from `start` through `segments` as child modules visible to this module, following each entry's re-export target. A failing segment is classified against the direct table: present-but-private vs. absent.
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

    // Resolve the module named by `name`'s first `upto` segments: from the module root (absolute) or the lexically-bound head qualifier (relative — the head is consumed as the start, so the walk runs over `segments[1..upto]`). Guards the *resolved* module, so a relative spelling is rejected exactly as the absolute one is.
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

        super::guard_internal_root(self.mounts, &self.prefix, resolved.segments())?;
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

    // Import the binding `label` out of `parent`, registering it in the current lexical scope.
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

    // Import both the module and binding slots of `label` — used by glob and the `Both` group item. Either or both may be absent.
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
