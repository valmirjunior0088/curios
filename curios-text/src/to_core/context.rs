use {
    super::PublicInterface,
    crate::{Error, Name},
    curios_abi::{RootId, Roster},
    curios_base::Entropy,
    curios_core::Qualifier,
    std::collections::HashMap,
};

#[derive(Clone)]
pub(super) struct FlatLet {
    pub name: Qualifier,
    pub type_: curios_core::Term,
    pub body: curios_core::Term,
}

#[derive(Clone)]
pub(super) enum FlatItem {
    Let(FlatLet),
    Rec(Vec<FlatLet>),
}

// The direct interface of a module: every declared label (public *and* private)
// in each namespace, with its visibility. This is the per-module body view used
// for lexical scope during elaboration, and to tell private from absent when a
// public lookup misses.
pub(super) struct ModuleInfo {
    children: HashMap<String, bool>,
    bindings: HashMap<String, bool>,
}

impl ModuleInfo {
    pub(super) fn new() -> Self {
        Self {
            children: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    pub(super) fn insert_child(&mut self, label: String, is_pub: bool) -> Result<(), Error> {
        if is_pub && matches!(self.children.get(&label), Some(true)) {
            return Err(Error::DuplicatePublicDeclaration { label });
        }

        self.children.insert(label, is_pub);
        Ok(())
    }

    pub(super) fn insert_binding(&mut self, label: String, is_pub: bool) -> Result<(), Error> {
        if is_pub && matches!(self.bindings.get(&label), Some(true)) {
            return Err(Error::DuplicatePublicDeclaration { label });
        }

        self.bindings.insert(label, is_pub);
        Ok(())
    }

    pub(super) fn get_child(&self, label: &str) -> Option<bool> {
        self.children.get(label).copied()
    }

    pub(super) fn get_binding(&self, label: &str) -> Option<bool> {
        self.bindings.get(label).copied()
    }

    pub(super) fn public_children(&self) -> Vec<String> {
        self.children
            .iter()
            .filter(|(_, is_pub)| **is_pub)
            .map(|(label, _)| label.clone())
            .collect()
    }

    pub(super) fn public_bindings(&self) -> Vec<String> {
        self.bindings
            .iter()
            .filter(|(_, is_pub)| **is_pub)
            .map(|(label, _)| label.clone())
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct UseResolved {
    pub module: Option<Qualifier>,
    pub binding: Option<Qualifier>,
}

// The per-body elaboration context. `table`/`public` are frozen interface views,
// shared read-only across all nested contexts. `qualifiers`/`bindings` are the
// lexical scope of the module body being elaborated, populated source-ordered by
// declarations and `use` imports.
pub(super) struct Context<'a> {
    prefix: Qualifier,
    table: &'a HashMap<Qualifier, ModuleInfo>,
    public: &'a HashMap<Qualifier, PublicInterface>,
    // The compilation's root roster — shared read-only, like `table`/`public`,
    // so every nested `Context` resolves a leading segment to the same ids.
    roster: &'a Roster,
    qualifiers: HashMap<String, Qualifier>,
    bindings: HashMap<String, Qualifier>,
    // Shared, program-global metavariable-id counter. The whole program folds
    // into one `curios_core::Term`, so holes in different module bodies (each its own
    // `Context` via `nested`) must draw from the same monotonic source. Shared
    // by reference (like `table`/`public`) and `Cell`-backed so it survives
    // `Lower`'s immutable `&Context` borrow.
    metavars: &'a Entropy,
    // Sibling counter for fresh continuation-binder names minted while desugaring
    // `!` regions. Threaded (not a process-global atomic) for determinism:
    // `curios_core::Scope`'s `PartialEq` compares binder *names*, so term-equality in
    // tests must be order-stable.
    binders: &'a Entropy,
}

fn attach(error: Error, name: &Name) -> Error {
    match name.span() {
        Some(span) => error.at(span.clone()),
        None => error,
    }
}

impl<'a> Context<'a> {
    pub(super) fn new(
        table: &'a HashMap<Qualifier, ModuleInfo>,
        public: &'a HashMap<Qualifier, PublicInterface>,
        roster: &'a Roster,
        metavars: &'a Entropy,
        binders: &'a Entropy,
    ) -> Context<'a> {
        Context {
            prefix: Qualifier::empty(),
            table,
            public,
            roster,
            qualifiers: HashMap::new(),
            bindings: HashMap::new(),
            metavars,
            binders,
        }
    }

    pub(super) fn nested(&self, label: &str) -> Context<'a> {
        Context {
            prefix: self.prefix.with(label),
            table: self.table,
            public: self.public,
            roster: self.roster,
            qualifiers: HashMap::new(),
            bindings: HashMap::new(),
            metavars: self.metavars,
            binders: self.binders,
        }
    }

    /// The `RootId` `label`'s declaration (under the current module prefix)
    /// belongs to — the roster-resolved replacement for the old
    /// `context.prefixed(label).root_id()`, which read straight off
    /// `RootId::of_segment`'s hardcoded match.
    pub(super) fn root_of(&self, label: &str) -> RootId {
        self.roster.resolve(self.prefixed(label).root_segment())
    }

    /// Mint a fresh, program-globally-unique metavariable id for a surface hole.
    pub(super) fn fresh_metavar(&self) -> usize {
        self.metavars.fresh()
    }

    /// Mint a fresh continuation-binder name for `!` desugaring. The `#`
    /// sigil is illegal in source identifiers, so it cannot collide with user
    /// names or qualified references.
    pub(super) fn fresh_binder(&self) -> String {
        format!("#{}", self.binders.fresh())
    }

    pub(super) fn prefixed(&self, label: &str) -> Qualifier {
        self.prefix.with(label)
    }

    pub(super) fn bindings(&self) -> &HashMap<String, Qualifier> {
        &self.bindings
    }

    /// A `pub` item's declared signature must not reference an item that is
    /// not itself publicly reachable. Cross-module references were already
    /// vetted by resolution (every module hop and the final binding walk the
    /// public tables), so the only privately-resolvable references a lowered
    /// signature can carry are (1) the item's own module's bindings (lexical
    /// scope) and (2) items inside the item's own child modules (the head
    /// segment resolves lexically). Those are what this audits: `signature` is
    /// the item's lowered type, whose free variables are exactly its resolved
    /// global references.
    pub(super) fn check_public_interface(
        &self,
        item: &str,
        signature: &curios_core::Term,
    ) -> Result<(), Error> {
        for reference in signature.free_vars() {
            // Only canonical (absolute) names refer to module items; bare
            // names are local binders or unresolved (reported later by core).
            let Some(path) = reference.strip_prefix('/') else {
                continue;
            };
            let segments: Vec<&str> = path.split('/').collect();
            let Some((label, parents)) = segments.split_last() else {
                continue;
            };

            // Skip foreign referents: only names at or below the current
            // module can have resolved through a private path.
            let prefix = self.prefix.segments();
            if parents.len() < prefix.len()
                || parents.iter().zip(prefix).any(|(a, b)| *a != b.as_str())
            {
                continue;
            }

            // Every child-module hop below the current module must be public...
            let mut module = self.prefix.clone();
            let mut referent = None;
            for hop in &parents[prefix.len()..] {
                if referent.is_none()
                    && self.table.get(&module).and_then(|info| info.get_child(hop)) == Some(false)
                {
                    referent = Some(module.with(hop).join());
                }
                module = module.with(hop);
            }

            // ...and so must the binding itself. Namespaces without a module
            // table entry (constructor and concept-method namespaces) share
            // their parent's visibility, which the hop walk already covered.
            if referent.is_none()
                && self
                    .table
                    .get(&module)
                    .and_then(|info| info.get_binding(label))
                    == Some(false)
            {
                referent = Some(reference.clone());
            }

            if let Some(referent) = referent {
                return Err(Error::PrivateItemInPublicInterface {
                    item: self.prefixed(item).join(),
                    referent,
                });
            }
        }

        Ok(())
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

    // Walk from `start` through `segments` as public child modules, following
    // each entry's re-export target. A failing segment is classified against the
    // direct table: present-but-private vs. absent.
    fn walk_children(&self, start: Qualifier, segments: &[String]) -> Result<Qualifier, Error> {
        let mut current = start;

        for segment in segments {
            match self
                .public
                .get(&current)
                .and_then(|i| i.children.get(segment))
            {
                Some(entry) => current = entry.target.clone(),
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

        super::guard_internal_root(&self.prefix, resolved.segments())?;
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
        match self.public.get(parent).and_then(|i| i.children.get(label)) {
            Some(entry) => {
                let target = entry.target.clone();
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
        match self.public.get(parent).and_then(|i| i.bindings.get(label)) {
            Some(entry) => {
                let target = entry.target.clone();
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
        let module = self
            .public
            .get(parent)
            .and_then(|i| i.children.get(label))
            .map(|entry| entry.target.clone());

        let binding = self
            .public
            .get(parent)
            .and_then(|i| i.bindings.get(label))
            .map(|entry| entry.target.clone());

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

            let has_module = self
                .public
                .get(&parent)
                .is_some_and(|i| i.children.contains_key(&label));
            let has_binding = self
                .public
                .get(&parent)
                .is_some_and(|i| i.bindings.contains_key(&label));

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

            match self
                .public
                .get(&parent)
                .and_then(|i| i.bindings.get(&label))
            {
                Some(entry) => Ok(entry.target.clone()),
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
