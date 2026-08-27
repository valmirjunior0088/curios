//! Auditing what a module's public interface exposes: that no public declaration's type reaches a private name, and that no re-export widens an item's audience past what its own module granted.
//!
//! The two walks share one question — who may see this referent? — asked at two seams. [`audit_dependencies`] asks it of every name a public declaration's type mentions; [`audit_public_exposures`] asks it of every name a module re-exports. Both resolve a referent through the alias chain first, so a public alias to a private nominal is caught where it is written rather than where it is used.

use {
    super::{
        order::{AliasEdge, flat_aliases},
        *,
    },
    curios_utilities::Qualifier,
    std::collections::HashMap,
};

/// Follow a directly attached representation provenance or a chain of bare, transparent type aliases to the underlying nominal registry entry.
fn exposed_nominal(
    entry: &Entry,
    aliases: &HashMap<curios_core::Global, AliasEdge>,
    scope: NominalScope<'_>,
) -> Option<(curios_core::Global, Vec<AliasEdge>)> {
    let mut current = curios_core::Global::Authored(
        entry
            .representation
            .as_ref()
            .unwrap_or(&entry.target)
            .clone(),
    );
    let mut seen = HashSet::new();
    let mut traversed = Vec::new();

    loop {
        if scope.declares(&current) {
            return Some((current, traversed));
        }
        if !seen.insert(current.clone()) {
            return None;
        }
        let edge = aliases.get(&current)?.clone();
        current = edge.target.clone();
        traversed.push(edge);
    }
}

/// Every nominal declaration an alias chain can land on: the unit's own, over the ones its scope already made visible.
///
/// A *scope*, not a merged map. The audit walks alias edges until it reaches something nominal, and an alias may legitimately point at a type from an earlier unit — so the question crosses the boundary and is answered by asking every half. Merging them upstream answers it too, and that is what this replaced: a map whose correctness here depended on somebody else having concatenated the prelude into it, with nothing saying so. See `documentation/design/toolchain/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md`.
#[derive(Clone, Copy)]
pub(super) struct NominalScope<'a> {
    /// The units already lowered, in dependency order. Empty when this lowering *is* the first and there is nothing beneath it.
    bases: &'a [&'a curios_core::Module],
    induct_decls: &'a BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: &'a BTreeMap<curios_core::Global, curios_core::StructDecl>,
}

impl<'a> NominalScope<'a> {
    pub(super) fn new(
        bases: &'a [&'a curios_core::Module],
        induct_decls: &'a BTreeMap<curios_core::Global, curios_core::InductDecl>,
        struct_decls: &'a BTreeMap<curios_core::Global, curios_core::StructDecl>,
    ) -> Self {
        Self {
            bases,
            induct_decls,
            struct_decls,
        }
    }

    /// Whether `name` names an `induct` or a `struct` anywhere in scope.
    fn declares(&self, name: &curios_core::Global) -> bool {
        self.induct(name).is_some() || self.struct_(name).is_some()
    }

    fn induct(&self, name: &curios_core::Global) -> Option<&'a curios_core::InductDecl> {
        self.induct_decls.get(name).or_else(|| {
            self.bases
                .iter()
                .rev()
                .find_map(|base| base.induct_decls.get(name))
        })
    }

    fn struct_(&self, name: &curios_core::Global) -> Option<&'a curios_core::StructDecl> {
        self.struct_decls.get(name).or_else(|| {
            self.bases
                .iter()
                .rev()
                .find_map(|base| base.struct_decls.get(name))
        })
    }
}

/// Invert the alias map to its transitive closure: for each canonical name, the bare transparent aliases that reach it. A name is as visible as the widest alias that stands for it, so an exported alias carries its target's audience even when the target itself is never exported.
fn alias_sources(
    aliases: &HashMap<curios_core::Global, AliasEdge>,
) -> HashMap<curios_core::Global, HashSet<curios_core::Global>> {
    let mut sources: HashMap<curios_core::Global, HashSet<curios_core::Global>> = HashMap::new();

    for (name, edge) in aliases {
        sources
            .entry(edge.target.clone())
            .or_default()
            .insert(name.clone());
    }

    loop {
        let mut changed = false;
        let pairs: Vec<(curios_core::Global, Vec<curios_core::Global>)> = sources
            .iter()
            .map(|(target, names)| (target.clone(), names.iter().cloned().collect()))
            .collect();

        for (target, names) in pairs {
            for name in names {
                let Some(indirect) = sources.get(&name).cloned() else {
                    continue;
                };
                let direct = sources.entry(target.clone()).or_default();
                for hop in indirect {
                    changed |= direct.insert(hop);
                }
            }
        }

        if !changed {
            break;
        }
    }

    sources
}

/// The top-level definitions among `names`. A binder is nobody's dependency: it is introduced and discharged inside the very signature being audited.
fn globals(
    names: impl IntoIterator<Item = curios_core::Free>,
) -> impl Iterator<Item = curios_core::Global> {
    names
        .into_iter()
        .filter_map(|name| name.as_global().cloned())
}

/// Everyone who can see `referent`, whether by its own name or through a transparent alias that stands for it.
fn referent_audience(
    audiences: &Audiences,
    sources: &HashMap<curios_core::Global, HashSet<curios_core::Global>>,
    referent: &curios_core::Global,
) -> Vec<Qualifier> {
    let Some(qualifier) = referent.qualifier() else {
        return Vec::new();
    };
    let mut audience = audiences.binding(qualifier);

    // A hop is matched by identity, and its qualifier is read off the name rather than split back out of a rendering.
    for alias in sources.get(referent).into_iter().flatten() {
        let Some(qualifier) = alias.qualifier() else {
            continue;
        };
        audience.extend(audiences.binding(qualifier));
    }

    audience
}

/// Every consumer of `item` — an item exposed to `exposure` — must be able to see everything `item`'s signature names. Checked against audiences rather than the declaration path, so an item re-exported out of a private module counts as visible exactly where the re-export puts it.
fn audit_dependencies(
    audiences: &Audiences,
    sources: &HashMap<curios_core::Global, HashSet<curios_core::Global>>,
    exposure: &[Qualifier],
    item: &str,
    dependencies: impl IntoIterator<Item = curios_core::Global>,
) -> Result<(), Error> {
    for referent in dependencies {
        let reach = referent_audience(audiences, sources, &referent);
        if !Audiences::covers(exposure, &reach) {
            return Err(Error::PrivateItemInPublicInterface {
                item: item.to_string(),
                referent: referent.symbol(),
            });
        }
    }

    Ok(())
}

/// Audit every declared signature and every exposed representation against the audience of the item carrying it. This runs after lowering because registry telescopes contain the complete signatures and transparent aliases have become canonical free-variable references. Re-export entries retain their representation provenance through the fixed point, so no `pub use` can upgrade an opaque declaration.
///
/// The declared type of every definition is audited here rather than during lowering: only the converged interface graph knows where a name ends up visible, so a signature naming an item re-exported out of a private child is accepted, while one naming something its own consumers cannot reach is not.
pub(super) fn audit_public_exposures(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    items: &[FlatItem],
    scope: NominalScope<'_>,
) -> Result<(), Error> {
    let aliases = flat_aliases(items);
    let sources = alias_sources(&aliases);
    let audiences = Audiences::compute(public, table);

    for let_ in items.iter().flat_map(|item| match item {
        FlatItem::Let(let_) => std::slice::from_ref(let_),
        FlatItem::Rec(lets) => lets.as_slice(),
    }) {
        // Only definitions the source actually wrote. A member synthesized into a nested namespace — an inductive's constructor, a concept's method wrapper — sits below its declaring module rather than in it, and its signature is the declaration's business, not an interface the author wrote: a constructor facade may legitimately hand out values of a type the consumer cannot name.
        //
        // A witness has no authored path at all, which is the same answer arrived at structurally: "who can see this by its name" is not a question an anonymous declaration has. Its reach is the coherence table's, governed by the orphan rule at registration.
        let Some(path) = let_.name.qualifier() else {
            continue;
        };
        if path.without_last() != let_.island {
            continue;
        }

        let exposure = audiences.binding(path);
        audit_dependencies(
            &audiences,
            &sources,
            &exposure,
            &let_.name.symbol(),
            globals(let_.type_.free_vars()),
        )?;
    }

    for (module, interface) in public.own() {
        for (label, entry) in &interface.bindings {
            let Some((nominal, traversed)) = exposed_nominal(entry, &aliases, scope) else {
                continue;
            };
            let item = module.with(label).join();
            let exposure = audiences.module(module);

            for alias in traversed {
                if let Some(dependencies) = alias.dependencies {
                    audit_dependencies(&audiences, &sources, &exposure, &item, dependencies)?;
                }
            }

            if let Some(induct_decl) = scope.induct(&nominal) {
                let nominal_dependencies = globals(induct_decl.arity.free_vars());
                audit_dependencies(&audiences, &sources, &exposure, &item, nominal_dependencies)?;

                if induct_decl.rep_public {
                    audit_dependencies(
                        &audiences,
                        &sources,
                        &exposure,
                        &item,
                        globals(
                            induct_decl
                                .constructors
                                .iter()
                                .flat_map(|(_, case)| case.telescope.free_vars()),
                        ),
                    )?;
                }
            } else if let Some(struct_decl) = scope.struct_(&nominal) {
                // The parameter domains alone: `arity.free_vars()` would reach the fields it terminates in, and the two are audited under different rules — parameters belong to the nominal type's public face, fields to its representation.
                let mut walk = &struct_decl.arity;
                let mut param_dependencies = Vec::new();
                while let curios_core::Telescope::Cons(domain, rest) = walk {
                    param_dependencies.extend(domain.free_vars());
                    walk = rest.body();
                }
                audit_dependencies(
                    &audiences,
                    &sources,
                    &exposure,
                    &item,
                    globals(param_dependencies),
                )?;

                if struct_decl.rep_public {
                    audit_dependencies(
                        &audiences,
                        &sources,
                        &exposure,
                        &item,
                        globals(struct_decl.fields().free_vars()),
                    )?;
                }
            }
        }
    }

    Ok(())
}
