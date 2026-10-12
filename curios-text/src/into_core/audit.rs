//! Auditing what a module's public interface exposes: that no public declaration's type reaches a private name, and that no re-export widens an item's audience past what its own module granted.
//!
//! The two walks share one question — who may see this referent? — asked at two seams. [`audit_dependencies`] asks it of every name a public declaration's type mentions; [`audit_public_exposures`] asks it of every name a module re-exports. Both resolve a referent through the alias chain first, so a public alias to a private nominal is caught where it is written rather than where it is used.

use {
    super::{
        AliasEdge, AudienceScope, Audiences, Entry, FlatItem, LoweredItem, PreparedText,
        PublicInterface, Scoped, aliases, globals,
    },
    crate::Error,
    curios_core::Bound,
    curios_utilities::{Qualifier, Span},
    std::collections::{BTreeMap, BTreeSet, HashSet},
};

/// Follow a directly attached representation provenance or a chain of bare, transparent type aliases to the underlying nominal registry entry.
fn exposed_nominal<'a>(
    entry: &Entry,
    aliases: &BTreeMap<curios_core::Global, &'a AliasEdge>,
    scope: NominalScope<'_>,
) -> Option<(curios_core::Global, Vec<&'a AliasEdge>)> {
    let mut current =
        curios_core::Global::Authored(*entry.representation.as_ref().unwrap_or(&entry.target));
    let mut seen = HashSet::new();
    let mut traversed = Vec::new();

    loop {
        if scope.declares(&current) {
            return Some((current, traversed));
        }
        if !seen.insert(current) {
            return None;
        }
        let edge = *aliases.get(&current)?;
        current = edge.target;
        traversed.push(edge);
    }
}

/// Every nominal declaration an alias chain can land on: the unit's own, over the ones its predecessors already made visible.
///
/// A *scope*, not a merged map. The audit walks alias edges until it reaches something nominal, and an alias may legitimately point at a type from an earlier unit — so the question crosses the boundary and is answered by asking every half. Merging them upstream would answer it too, with a map whose correctness here depended on somebody else having concatenated the prelude into it and nothing saying so. See `documentation/design/compilation/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md`.
#[derive(Clone, Copy)]
pub(super) struct NominalScope<'a> {
    /// The units already lowered, in dependency order. Empty when this lowering *is* the first and there is nothing beneath it.
    predecessors: &'a [&'a PreparedText],
    induct_decls: &'a BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: &'a BTreeMap<curios_core::Global, curios_core::StructDecl>,
}

impl<'a> NominalScope<'a> {
    pub(super) fn new(
        predecessors: &'a [&'a PreparedText],
        induct_decls: &'a BTreeMap<curios_core::Global, curios_core::InductDecl>,
        struct_decls: &'a BTreeMap<curios_core::Global, curios_core::StructDecl>,
    ) -> Self {
        Self {
            predecessors,
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
            self.predecessors
                .iter()
                .rev()
                .find_map(|base| base.nominal(name)?.inductive())
        })
    }

    fn struct_(&self, name: &curios_core::Global) -> Option<&'a curios_core::StructDecl> {
        self.struct_decls.get(name).or_else(|| {
            self.predecessors
                .iter()
                .rev()
                .find_map(|base| base.nominal(name)?.record())
        })
    }
}

/// Invert the alias map to its transitive closure: for each canonical name, the bare transparent aliases that reach it. A name is as visible as the widest alias that stands for it, so an exported alias carries its target's audience even when the target itself is never exported.
fn alias_sources(
    aliases: &BTreeMap<curios_core::Global, &AliasEdge>,
) -> BTreeMap<curios_core::Global, BTreeSet<curios_core::Global>> {
    let mut sources: BTreeMap<curios_core::Global, BTreeSet<curios_core::Global>> = BTreeMap::new();

    for (name, edge) in aliases {
        sources.entry(edge.target).or_default().insert(*name);
    }

    loop {
        let mut changed = false;
        let pairs: Vec<(curios_core::Global, Vec<curios_core::Global>)> = sources
            .iter()
            .map(|(target, names)| (*target, names.iter().cloned().collect()))
            .collect();

        for (target, names) in pairs {
            for name in names {
                let Some(indirect) = sources.get(&name).cloned() else {
                    continue;
                };
                let direct = sources.entry(target).or_default();
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

/// Everyone who can see `referent`, whether by its own name or through a transparent alias that stands for it.
fn referent_audience(
    audiences: AudienceScope<'_>,
    sources: &BTreeMap<curios_core::Global, BTreeSet<curios_core::Global>>,
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

/// A name some reader of an item cannot see, with the subtrees whose modules can.
pub(super) struct Unseen {
    pub(super) referent: curios_core::Global,
    reach: Vec<Qualifier>,
}

impl Unseen {
    /// The module that keeps the name from the readers it does not reach: the one on the name's own path that stands directly under the widest subtree it reaches, where that module is written without `pub`. `child` answers whether a module declares a child of a label, and whether it wrote `pub` on it.
    ///
    /// `None` where what stands there is the name itself, a declaration written without `pub`, and where the reach stops for a reason its own path does not show, an export elsewhere.
    ///
    /// **The widest subtree, not the innermost private module.** A name declared in one private module and exported out of it into another reaches the second one's audience: writing `pub` on the first changes nothing, and it is the second that a reader is told of.
    pub(super) fn behind(
        &self,
        child: &dyn Fn(&Qualifier, &str) -> Option<bool>,
    ) -> Option<Qualifier> {
        let path = self.referent.qualifier()?;
        let widest = self
            .reach
            .iter()
            .filter(|root| path.is_within(root) && path != *root)
            .min_by_key(|root| root.segments().len())?;
        let below = &path.segments()[widest.segments().len()..];
        let [label, _, ..] = below else {
            return None;
        };

        (child(widest, label) == Some(false)).then(|| widest.with(label))
    }
}

/// Every consumer of an item exposed to `exposure` must be able to see everything the item's signature names, its `dependencies`: the first that one of them cannot see is handed back. Checked against audiences rather than the declaration path, so an item re-exported out of a private module counts as visible exactly where the re-export puts it.
fn audit_dependencies(
    audiences: AudienceScope<'_>,
    sources: &BTreeMap<curios_core::Global, BTreeSet<curios_core::Global>>,
    exposure: &[Qualifier],
    dependencies: impl IntoIterator<Item = curios_core::Global>,
) -> Result<(), Unseen> {
    for referent in dependencies {
        let reach = referent_audience(audiences, sources, &referent);
        if !Audiences::covers(exposure, &reach) {
            return Err(Unseen { referent, reach });
        }
    }

    Ok(())
}

/// Audit every declared signature and every exposed representation against the audience of the item carrying it. This runs after lowering because registry telescopes contain the complete signatures and transparent aliases have become canonical free-variable references. Re-export entries retain their representation provenance through the fixed point, so no `pub use` can upgrade an opaque declaration.
///
/// The declared type of every definition is audited here rather than during lowering: only the converged interface graph knows where a name ends up visible, so a signature naming an item re-exported out of a private child is accepted, while one naming something its own consumers cannot reach is not.
///
/// A refusal is placed at the name of the item that exposes: the declaration's own, or, for a binding a module re-exports, the declaration it stands for where this unit wrote it. `placed` gives the span an extent of one of the unit's declarations lies on. `refused` makes the refusal of the item whose path it is handed, for the name one of its readers cannot see: it holds the tables that say how a reader standing at the item writes each, and which declaration keeps the name from the rest.
pub(super) fn audit_public_exposures(
    public: &Scoped<'_, PublicInterface>,
    audiences: AudienceScope<'_>,
    items: &[FlatItem],
    facts: &[&LoweredItem],
    scope: NominalScope<'_>,
    placed: &dyn Fn(&curios_core::Extent) -> Option<Span>,
    refused: &dyn Fn(&Qualifier, &Unseen) -> Error,
) -> Result<(), Error> {
    curios_profile::profile!("audit_public_exposures");
    let aliases = aliases(items, facts);
    let sources = alias_sources(&aliases);

    let definitions = || {
        items
            .iter()
            .zip(facts)
            .flat_map(|(item, lowered)| item.declared().zip(&lowered.definitions))
    };
    // Read where a refusal is raised, and by no audit that passes.
    let named = |path: Qualifier| {
        let name = curios_core::Global::Authored(path);
        let (_, declared) = definitions().find(|((declared, _, _), _)| *declared == name)?;

        placed(declared.label.as_ref()?)
    };
    let at = |error: Error, place: Option<Span>| match place {
        Some(span) => error.at(span),
        None => error,
    };

    for ((name, _, island), declared) in definitions() {
        // Only definitions the source actually wrote. A member synthesized into a nested namespace — an inductive's constructor, a concept's method wrapper — sits below its declaring module rather than in it, and is audited through the declaration that produced it.
        //
        // A witness has no authored path at all, which is the same answer arrived at structurally: "who can see this by its name" is not a question an anonymous declaration has. Its reach is the coherence check's.
        let Some(path) = name.qualifier() else {
            continue;
        };
        if path.without_last() != island {
            continue;
        }

        let exposure = audiences.binding(path);
        audit_dependencies(
            audiences,
            &sources,
            &exposure,
            declared.signature.iter().copied(),
        )
        .map_err(|unseen| {
            at(
                refused(path, &unseen),
                declared.label.as_ref().and_then(placed),
            )
        })?;
    }

    // By module and by label, so the exposure reported is the first the names give and not the first a hash's order does.
    let mut modules = public.own().iter().collect::<Vec<_>>();
    modules.sort_by(|left, right| left.0.cmp(right.0));
    for (module, interface) in modules {
        let mut bindings = interface.bindings.iter().collect::<Vec<_>>();
        bindings.sort_by(|left, right| left.0.cmp(right.0));
        for (label, entry) in bindings {
            let Some((nominal, traversed)) = exposed_nominal(entry, &aliases, scope) else {
                continue;
            };
            let item = module.with(label);
            let exposure = audiences.module(module);
            let exposed = |unseen: Unseen| {
                at(
                    refused(&item, &unseen),
                    named(item).or_else(|| named(entry.target)),
                )
            };

            for alias in traversed {
                if let Some(dependencies) = &alias.dependencies {
                    audit_dependencies(
                        audiences,
                        &sources,
                        &exposure,
                        dependencies.iter().copied(),
                    )
                    .map_err(exposed)?;
                }
            }

            if let Some(induct_decl) = scope.induct(&nominal) {
                let nominal_dependencies = globals(induct_decl.arity.free_vars());
                audit_dependencies(audiences, &sources, &exposure, nominal_dependencies)
                    .map_err(exposed)?;

                if induct_decl.rep_public {
                    audit_dependencies(
                        audiences,
                        &sources,
                        &exposure,
                        globals(
                            induct_decl
                                .constructors
                                .iter()
                                .flat_map(|(_, case)| case.telescope.free_vars()),
                        ),
                    )
                    .map_err(exposed)?;
                }
            } else if let Some(struct_decl) = scope.struct_(&nominal) {
                // The parameter domains alone: `arity.free_vars()` would reach the fields it terminates in, and the two are audited under different rules — parameters belong to the nominal type's public face, fields to its representation.
                let mut walk = &struct_decl.arity;
                let mut param_dependencies = Vec::new();
                while let curios_core::Telescope::Cons(_, domain, rest) = walk {
                    param_dependencies.extend(domain.free_vars());
                    walk = rest.body();
                }
                audit_dependencies(audiences, &sources, &exposure, globals(param_dependencies))
                    .map_err(exposed)?;

                if struct_decl.rep_public {
                    audit_dependencies(
                        audiences,
                        &sources,
                        &exposure,
                        globals(struct_decl.fields().free_vars()),
                    )
                    .map_err(exposed)?;
                }
            }
        }
    }

    Ok(())
}
