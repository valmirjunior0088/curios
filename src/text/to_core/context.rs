use {
    crate::{
        core,
        text::{Error, Name, Qualifier},
    },
    std::collections::HashMap,
};

pub struct FlatLet {
    pub name: Qualifier,
    pub type_: core::Term,
    pub body: core::Term,
}

pub enum FlatItem {
    Let(FlatLet),
    Rec(Vec<FlatLet>),
}

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

    pub fn insert_child(&mut self, label: String, is_pub: bool) {
        self.children.insert(label, is_pub);
    }

    pub fn insert_binding(&mut self, label: String, is_pub: bool) {
        self.bindings.insert(label, is_pub);
    }

    pub fn get_child(&self, label: &str) -> Option<bool> {
        self.children.get(label).copied()
    }

    pub fn get_binding(&self, label: &str) -> Option<bool> {
        self.bindings.get(label).copied()
    }

    pub fn public_labels(&self) -> Vec<String> {
        let mut labels = self
            .children
            .iter()
            .chain(self.bindings.iter())
            .filter(|(_, is_pub)| **is_pub)
            .map(|(label, _)| label.clone())
            .collect::<Vec<_>>();

        labels.sort();
        labels.dedup();
        labels
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseResolved {
    pub module: Option<Qualifier>,
    pub binding: Option<Qualifier>,
}

pub struct Context<'a> {
    prefix: Qualifier,
    table: &'a mut HashMap<Qualifier, ModuleInfo>,
    module_aliases: &'a mut HashMap<Qualifier, Qualifier>,
    binding_aliases: &'a mut HashMap<Qualifier, Qualifier>,
    qualifiers: HashMap<String, Qualifier>,
    bindings: HashMap<String, Qualifier>,
}

fn attach(error: Error, name: &Name) -> Error {
    match name.span() {
        Some(span) => error.at(span.clone()),
        None => error,
    }
}

impl<'a> Context<'a> {
    pub fn new(
        table: &'a mut HashMap<Qualifier, ModuleInfo>,
        module_aliases: &'a mut HashMap<Qualifier, Qualifier>,
        binding_aliases: &'a mut HashMap<Qualifier, Qualifier>,
    ) -> Context<'a> {
        Context {
            prefix: Qualifier::empty(),
            table,
            module_aliases,
            binding_aliases,
            qualifiers: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    pub fn nested(&mut self, label: &str) -> Context<'_> {
        Context {
            prefix: self.prefix.with(label),
            table: &mut *self.table,
            module_aliases: &mut *self.module_aliases,
            binding_aliases: &mut *self.binding_aliases,
            qualifiers: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    pub fn prefixed(&self, label: &str) -> Qualifier {
        self.prefix.with(label)
    }

    pub fn qualifiers(&self) -> &HashMap<String, Qualifier> {
        &self.qualifiers
    }

    pub fn bindings(&self) -> &HashMap<String, Qualifier> {
        &self.bindings
    }

    pub fn table(&self) -> &HashMap<Qualifier, ModuleInfo> {
        &*self.table
    }

    pub fn module_aliases(&self) -> &HashMap<Qualifier, Qualifier> {
        &*self.module_aliases
    }

    pub fn binding_aliases(&self) -> &HashMap<Qualifier, Qualifier> {
        &*self.binding_aliases
    }

    pub fn register_alias(&mut self, qualifier: &str) {
        self.module_aliases.insert(
            self.prefix.with(qualifier),
            self.qualifiers[qualifier].clone(),
        );
    }

    pub fn register_binding_alias(&mut self, label: &str) {
        self.binding_aliases
            .insert(self.prefix.with(label), self.bindings[label].clone());
    }

    pub fn finalize(&mut self, info: ModuleInfo) {
        self.table.insert(self.prefix.clone(), info);
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

    // Import the module child `label` out of the module at `parent_path`, registering
    // it as a qualifier in the current scope.
    fn import_module_label(
        &mut self,
        parent_path: &Qualifier,
        label: &str,
    ) -> Result<Qualifier, Error> {
        let child = self
            .table
            .get(parent_path)
            .ok_or_else(|| Error::ModuleNotFound {
                path: parent_path.join(),
            })?
            .get_child(label);

        match child {
            Some(true) => {
                let mut resolved = parent_path.with(label);

                if let Some(canonical) = self.module_aliases.get(&resolved) {
                    resolved = canonical.clone();
                }

                if !self.table.contains_key(&resolved) {
                    return Err(Error::ModuleNotFound {
                        path: resolved.join(),
                    });
                }

                self.insert_scope(label.to_string(), resolved.clone())?;
                Ok(resolved)
            }
            Some(false) => Err(Error::PrivateChildModule {
                segment: label.to_string(),
            }),
            None => Err(Error::NotAModule {
                label: label.to_string(),
                parent: parent_path.join(),
            }),
        }
    }

    // Import the binding `label` out of the module at `parent_path`, registering it
    // as a binding in the current scope.
    fn import_binding_label(
        &mut self,
        parent_path: &Qualifier,
        label: &str,
    ) -> Result<Qualifier, Error> {
        let binding = self
            .table
            .get(parent_path)
            .ok_or_else(|| Error::ModuleNotFound {
                path: parent_path.join(),
            })?
            .get_binding(label);

        match binding {
            Some(true) => {
                let mut resolved = parent_path.with(label);

                if let Some(canonical) = self.binding_aliases.get(&resolved) {
                    resolved = canonical.clone();
                }

                self.insert_binding(label.to_string(), resolved.clone())?;
                Ok(resolved)
            }
            Some(false) => Err(Error::PrivateBinding {
                binding: label.to_string(),
            }),
            None => Err(Error::NotABinding {
                label: label.to_string(),
                parent: parent_path.join(),
            }),
        }
    }

    // Import both module and binding slots — used by glob and by the `Both`
    // group item. Either or both may be absent: callers that require at least
    // one (e.g. an explicit `Both` import) should check `result` afterwards.
    fn import_dual_label(
        &mut self,
        parent_path: &Qualifier,
        label: &str,
    ) -> Result<UseResolved, Error> {
        let (child, binding) = {
            let parent_info = self
                .table
                .get(parent_path)
                .ok_or_else(|| Error::ModuleNotFound {
                    path: parent_path.join(),
                })?;

            (parent_info.get_child(label), parent_info.get_binding(label))
        };

        let mut result = UseResolved {
            module: None,
            binding: None,
        };

        if let Some(true) = child {
            let mut resolved = parent_path.with(label);

            if let Some(canonical) = self.module_aliases.get(&resolved) {
                resolved = canonical.clone();
            }

            if !self.table.contains_key(&resolved) {
                return Err(Error::ModuleNotFound {
                    path: resolved.join(),
                });
            }

            self.insert_scope(label.to_string(), resolved.clone())?;
            result.module = Some(resolved);
        }

        if let Some(true) = binding {
            let mut resolved = parent_path.with(label);

            if let Some(canonical) = self.binding_aliases.get(&resolved) {
                resolved = canonical.clone();
            }

            self.insert_binding(label.to_string(), resolved.clone())?;
            result.binding = Some(resolved);
        }

        Ok(result)
    }

    fn resolve_parent_path(&self, name: &Name) -> Result<(Qualifier, String), Error> {
        let is_abs = name.is_abs();
        let label = name.last().to_string();

        let parent_path = if is_abs {
            let mut current = Qualifier::empty();

            if name.is_single() {
                current
            } else {
                let head = name.head();

                let root_info = self
                    .table
                    .get(&Qualifier::empty())
                    .expect("root module info not present");

                let is_pub =
                    root_info
                        .get_child(head)
                        .ok_or_else(|| Error::ChildModuleNotFound {
                            segment: head.to_string(),
                        })?;

                if !is_pub {
                    return Err(Error::PrivateChildModule {
                        segment: head.to_string(),
                    });
                }

                current = Qualifier::from([head]);

                if !self.table.contains_key(&current) {
                    return Err(Error::ModuleNotFound {
                        path: head.to_string(),
                    });
                }

                for seg in name.interior() {
                    let info = self
                        .table
                        .get(&current)
                        .ok_or_else(|| Error::ModuleNotFound {
                            path: current.join(),
                        })?;

                    let is_pub = info
                        .get_child(seg)
                        .ok_or_else(|| Error::ChildModuleNotFound {
                            segment: seg.to_string(),
                        })?;

                    if !is_pub {
                        return Err(Error::PrivateChildModule {
                            segment: seg.to_string(),
                        });
                    }

                    current = current.with(seg);

                    if let Some(canonical) = self.module_aliases.get(&current) {
                        current = canonical.clone();
                    }

                    if !self.table.contains_key(&current) {
                        return Err(Error::ModuleNotFound {
                            path: current.join(),
                        });
                    }
                }

                current
            }
        } else {
            let first = name.head();

            let mut current = self
                .qualifiers
                .get(first)
                .ok_or_else(|| Error::UnresolvedQualifier {
                    qualifier: first.to_string(),
                })?
                .clone();

            for seg in name.interior() {
                let info = self
                    .table
                    .get(&current)
                    .ok_or_else(|| Error::ModuleNotFound {
                        path: current.join(),
                    })?;

                let is_pub = info
                    .get_child(seg)
                    .ok_or_else(|| Error::ChildModuleNotFound {
                        segment: seg.to_string(),
                    })?;

                if !is_pub {
                    return Err(Error::PrivateChildModule {
                        segment: seg.to_string(),
                    });
                }

                current = current.with(seg);

                if let Some(canonical) = self.module_aliases.get(&current) {
                    current = canonical.clone();
                }
            }

            if !self.table.contains_key(&current) {
                return Err(Error::ModuleNotFound {
                    path: current.join(),
                });
            }

            current
        };

        Ok((parent_path, label))
    }

    pub fn resolve_module_use(&mut self, name: &Name) -> Result<Qualifier, Error> {
        let result = (|| {
            let (parent_path, label) = self.resolve_parent_path(name)?;
            self.import_module_label(&parent_path, &label)
        })();
        result.map_err(|e| attach(e, name))
    }

    pub fn resolve_binding_use(&mut self, name: &Name) -> Result<Qualifier, Error> {
        let result = (|| {
            let (parent_path, label) = self.resolve_parent_path(name)?;
            self.import_binding_label(&parent_path, &label)
        })();
        result.map_err(|e| attach(e, name))
    }

    pub fn resolve_both_use(&mut self, name: &Name) -> Result<UseResolved, Error> {
        let result = (|| {
            let (parent_path, label) = self.resolve_parent_path(name)?;

            let (child, binding) = {
                let parent_info =
                    self.table
                        .get(&parent_path)
                        .ok_or_else(|| Error::ModuleNotFound {
                            path: parent_path.join(),
                        })?;

                (
                    parent_info.get_child(&label),
                    parent_info.get_binding(&label),
                )
            };

            if !matches!(child, Some(true)) && !matches!(binding, Some(true)) {
                return Err(match (child, binding) {
                    (Some(false), _) => Error::PrivateChildModule {
                        segment: label.clone(),
                    },
                    (_, Some(false)) => Error::PrivateBinding {
                        binding: label.clone(),
                    },
                    _ => Error::NoSuchUseTarget {
                        label: label.clone(),
                        parent: parent_path.join(),
                    },
                });
            }

            self.import_dual_label(&parent_path, &label)
        })();
        result.map_err(|e| attach(e, name))
    }

    // A glob `use a/b/*` names a module directly and imports every public child
    // and binding it exposes, each under its own label. Walks the full path to
    // the named module, then defers each label to `import_dual_label`.
    pub fn resolve_glob(&mut self, name: &Name) -> Result<Vec<(String, UseResolved)>, Error> {
        let result = (|| {
            let mut current = if name.is_abs() {
                Qualifier::empty()
            } else {
                let first = name.head();

                self.qualifiers
                    .get(first)
                    .ok_or_else(|| Error::UnresolvedQualifier {
                        qualifier: first.to_string(),
                    })?
                    .clone()
            };

            let walk = if name.is_abs() {
                name.qualifier().segments()
            } else {
                &name.qualifier().segments()[1..]
            };

            for seg in walk {
                let info = self
                    .table
                    .get(&current)
                    .ok_or_else(|| Error::ModuleNotFound {
                        path: current.join(),
                    })?;

                let is_pub = info
                    .get_child(seg)
                    .ok_or_else(|| Error::ChildModuleNotFound {
                        segment: seg.to_string(),
                    })?;

                if !is_pub {
                    return Err(Error::PrivateChildModule {
                        segment: seg.to_string(),
                    });
                }

                current = current.with(seg);

                if let Some(canonical) = self.module_aliases.get(&current) {
                    current = canonical.clone();
                }

                if !self.table.contains_key(&current) {
                    return Err(Error::ModuleNotFound {
                        path: current.join(),
                    });
                }
            }

            let labels = self
                .table
                .get(&current)
                .ok_or_else(|| Error::ModuleNotFound {
                    path: current.join(),
                })?
                .public_labels();

            labels
                .into_iter()
                .map(|label| {
                    let resolved = self.import_dual_label(&current, &label)?;
                    Ok((label, resolved))
                })
                .collect::<Result<Vec<_>, Error>>()
        })();
        result.map_err(|e| attach(e, name))
    }

    pub fn export_child(&mut self, label: String) {
        self.table
            .get_mut(&self.prefix)
            .expect("module info not present for current prefix")
            .insert_child(label, true);
    }

    pub fn export_binding(&mut self, label: String) {
        self.table
            .get_mut(&self.prefix)
            .expect("module info not present for current prefix")
            .insert_binding(label, true);
    }
}
