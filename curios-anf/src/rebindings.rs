//! Computation-free binding resolution over one immutable module snapshot.
//!
//! An alias and a projection from a visible product can name the function or constant already held there. Effect summaries and reconstruction share this reading rather than independently interpreting dictionary projections. Schema identities must agree, unresolved projections keep their original atom, and computed recursive members are a boundary: their initializers are never inspected. The snapshot is discarded before the module changes.

#[cfg(test)]
mod tests;

use {
    crate::{Atom, Module, ProductId, Rhs, Statement, ValueId},
    std::collections::{BTreeMap, BTreeSet},
};

#[derive(Debug, Clone, Default)]
pub(crate) struct Rebindings {
    aliases: BTreeMap<ValueId, Atom>,
    products: BTreeMap<ValueId, (ProductId, Vec<Atom>)>,
    projections: BTreeMap<ValueId, (ProductId, Atom, u32)>,
    computed: BTreeSet<ValueId>,
}

impl Rebindings {
    /// An alias's original operand, keeping projection provenance for reconstruction.
    pub(crate) fn alias(&self, value: ValueId) -> Option<Atom> {
        self.aliases.get(&value).copied()
    }

    /// Whether reading this atom forces a computed recursive member.
    pub(crate) fn is_computed(&self, atom: Atom) -> bool {
        matches!(atom, Atom::Value(value) if self.computed.contains(&value))
    }

    pub(crate) fn analyze(module: &Module) -> Self {
        let mut bindings = Self::default();
        for statement in module.statements().iter().flatten() {
            let Statement::Let { result, rhs } = statement else {
                continue;
            };
            match rhs {
                Rhs::Alias(atom) => {
                    bindings.aliases.insert(*result, *atom);
                }
                Rhs::Product { schema, fields } => {
                    bindings.products.insert(*result, (*schema, fields.clone()));
                }
                Rhs::Project {
                    schema,
                    product,
                    field,
                } => {
                    bindings
                        .projections
                        .insert(*result, (*schema, *product, *field));
                }
                _ => {}
            }
        }
        for group in module.rec_groups().iter().flatten() {
            bindings
                .computed
                .extend(group.values.iter().map(|member| member.value));
        }
        bindings
    }

    /// Follow aliases and visible product projections without entering a computation. Pending projections are applied innermost first; a mismatch preserves the queried atom. The visited set bounds malformed cycles as well as well-scoped DAGs.
    pub(crate) fn resolve(&self, atom: Atom) -> Atom {
        let mut current = atom;
        let mut pending = Vec::<(ProductId, u32)>::new();
        let mut seen = BTreeSet::new();
        while let Atom::Value(value) = current {
            if !seen.insert(value) || self.computed.contains(&value) {
                break;
            }
            if let Some(&next) = self.aliases.get(&value) {
                current = next;
            } else if let Some(&(schema, product, field)) = self.projections.get(&value) {
                pending.push((schema, field));
                current = product;
            } else if let Some(&(schema, field)) = pending.last()
                && let Some((built, fields)) = self.products.get(&value)
                && *built == schema
                && let Some(&next) = fields.get(field as usize)
            {
                pending.pop();
                current = next;
            } else {
                break;
            }
        }
        if pending.is_empty() { current } else { atom }
    }
}
