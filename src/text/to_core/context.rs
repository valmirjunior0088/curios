use {
    super::interface::PublicInterface,
    crate::{
        core,
        text::{Error, Name, Qualifier},
    },
    std::{cell::Cell, collections::HashMap},
};

#[derive(Clone)]
pub struct FlatLet {
    pub name: Qualifier,
    pub type_: core::Term,
    pub body: core::Term,
}

#[derive(Clone)]
pub enum FlatItem {
    Let(FlatLet),
    Rec(Vec<FlatLet>),
}

// The direct interface of a module: every declared label (public *and* private)
// in each namespace, with its visibility. This is the per-module body view used
// for lexical scope during elaboration, and to tell private from absent when a
// public lookup misses.
pub struct ModuleInfo {
    children: HashMap<String, bool>,
    bindings: HashMap<String, bool>,
}

impl ModuleInfo {
    pub fn new() -> Self {
        Self {
            children: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    pub fn insert_child(&mut self, label: String, is_pub: bool) -> Result<(), Error> {
        if is_pub && matches!(self.children.get(&label), Some(true)) {
            return Err(Error::DuplicatePublicDeclaration { label });
        }

        self.children.insert(label, is_pub);
        Ok(())
    }

    pub fn insert_binding(&mut self, label: String, is_pub: bool) -> Result<(), Error> {
        if is_pub && matches!(self.bindings.get(&label), Some(true)) {
            return Err(Error::DuplicatePublicDeclaration { label });
        }

        self.bindings.insert(label, is_pub);
        Ok(())
    }

    pub fn get_child(&self, label: &str) -> Option<bool> {
        self.children.get(label).copied()
    }

    pub fn get_binding(&self, label: &str) -> Option<bool> {
        self.bindings.get(label).copied()
    }

    pub fn public_children(&self) -> Vec<String> {
        self.children
            .iter()
            .filter(|(_, is_pub)| **is_pub)
            .map(|(label, _)| label.clone())
            .collect()
    }

    pub fn public_bindings(&self) -> Vec<String> {
        self.bindings
            .iter()
            .filter(|(_, is_pub)| **is_pub)
            .map(|(label, _)| label.clone())
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseResolved {
    pub module: Option<Qualifier>,
    pub binding: Option<Qualifier>,
}

// The per-body elaboration context. `table`/`public` are frozen interface views,
// shared read-only across all nested contexts. `qualifiers`/`bindings` are the
// lexical scope of the module body being elaborated, populated source-ordered by
// declarations and `use` imports.
pub struct Context<'a> {
    prefix: Qualifier,
    table: &'a HashMap<Qualifier, ModuleInfo>,
    public: &'a HashMap<Qualifier, PublicInterface>,
    qualifiers: HashMap<String, Qualifier>,
    bindings: HashMap<String, Qualifier>,
    // Shared, program-global metavariable-id counter. The whole program folds
    // into one `core::Term`, so holes in different module bodies (each its own
    // `Context` via `nested`) must draw from the same monotonic source. Shared
    // by reference (like `table`/`public`) and `Cell`-backed so it survives
    // `Elaborate`'s immutable `&Context` borrow.
    metavars: &'a Cell<usize>,
}

fn attach(error: Error, name: &Name) -> Error {
    match name.span() {
        Some(span) => error.at(span.clone()),
        None => error,
    }
}

impl<'a> Context<'a> {
    pub fn new(
        table: &'a HashMap<Qualifier, ModuleInfo>,
        public: &'a HashMap<Qualifier, PublicInterface>,
        metavars: &'a Cell<usize>,
    ) -> Context<'a> {
        Context {
            prefix: Qualifier::empty(),
            table,
            public,
            qualifiers: HashMap::new(),
            bindings: HashMap::new(),
            metavars,
        }
    }

    pub fn nested(&self, label: &str) -> Context<'a> {
        Context {
            prefix: self.prefix.with(label),
            table: self.table,
            public: self.public,
            qualifiers: HashMap::new(),
            bindings: HashMap::new(),
            metavars: self.metavars,
        }
    }

    /// Mint a fresh, program-globally-unique metavariable id for a surface hole.
    pub fn fresh_metavar(&self) -> usize {
        let id = self.metavars.get();
        self.metavars.set(id + 1);
        id
    }

    pub fn prefixed(&self, label: &str) -> Qualifier {
        self.prefix.with(label)
    }

    pub fn bindings(&self) -> &HashMap<String, Qualifier> {
        &self.bindings
    }

    pub fn insert_scope(&mut self, qualifier: String, name: Qualifier) -> Result<(), Error> {
        if self.qualifiers.contains_key(&qualifier) {
            return Err(Error::QualifierConflict { qualifier });
        }

        self.qualifiers.insert(qualifier, name);
        Ok(())
    }

    pub fn insert_binding(&mut self, label: String, name: Qualifier) -> Result<(), Error> {
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

    // The module that should contain `name`'s final segment, plus that segment.
    // Absolute names walk from the root; relative names from the lexically-bound
    // head qualifier.
    fn resolve_parent_path(&self, name: &Name) -> Result<(Qualifier, String), Error> {
        let label = name.last().to_string();
        let segments = name.qualifier().segments();
        let last = segments.len() - 1;

        let parent = if name.is_abs() {
            self.walk_children(Qualifier::empty(), &segments[..last])?
        } else {
            let head = name.head();
            let start = self
                .qualifiers
                .get(head)
                .ok_or_else(|| Error::UnresolvedQualifier {
                    qualifier: head.to_string(),
                })?
                .clone();

            self.walk_children(start, &segments[1..last])?
        };

        Ok((parent, label))
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

    pub fn resolve_module_use(&mut self, name: &Name) -> Result<Qualifier, Error> {
        let result = (|| {
            let (parent, label) = self.resolve_parent_path(name)?;
            self.import_module_label(&parent, &label)
        })();
        result.map_err(|e| attach(e, name))
    }

    pub fn resolve_binding_use(&mut self, name: &Name) -> Result<Qualifier, Error> {
        let result = (|| {
            let (parent, label) = self.resolve_parent_path(name)?;
            self.import_binding_label(&parent, &label)
        })();
        result.map_err(|e| attach(e, name))
    }

    pub fn resolve_both_use(&mut self, name: &Name) -> Result<UseResolved, Error> {
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
    pub fn resolve_glob(&mut self, name: &Name) -> Result<Vec<(String, UseResolved)>, Error> {
        let result = (|| {
            let segments = name.qualifier().segments();

            let module = if name.is_abs() {
                self.walk_children(Qualifier::empty(), segments)?
            } else {
                let head = name.head();
                let start = self
                    .qualifiers
                    .get(head)
                    .ok_or_else(|| Error::UnresolvedQualifier {
                        qualifier: head.to_string(),
                    })?
                    .clone();

                self.walk_children(start, &segments[1..])?
            };

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
    pub fn resolve_term_name(&self, name: &Name) -> Result<Qualifier, Error> {
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
