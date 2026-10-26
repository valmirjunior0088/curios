//! Recover an existing value from administrative reconstruction before layouts obscure its identity.
//!
//! Plans are substitutions to dominating atoms: a complete product's base or an exhaustive match's scrutinee. Nested identities settle through a dependency worklist over one immutable snapshot. Applying them preserves calls and effect order, then removes only administrative bindings the substitutions strand. Computed recursive members are forcing boundaries, including in aliases and constructor wrappers. Orphaned arms and removed statements remain arena slots until the driver's final prune and compaction.

mod facts;
use facts::*;

mod templates;
use templates::*;

#[cfg(test)]
mod tests;

use {
    crate::{
        Atom, BlockId, Constant, ConstructorId, FunctionId, Module, ProductId, Rebindings, Remap,
        Rhs, Statement, StatementId, Terminator, ValueId,
    },
    std::collections::{BTreeMap, BTreeSet},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Shape {
    Product(ProductId),
    Constructor(ConstructorId),
}

struct Construction {
    shape: Shape,
    fields: Vec<Atom>,
}

struct Reconstruction<'m> {
    module: &'m Module,
    bindings: Rebindings,
    definitions: BTreeMap<ValueId, (StatementId, &'m Rhs)>,
    templates: BTreeMap<FunctionId, ConstructionTemplate>,
    facts: ValueFacts,
    replacements: BTreeMap<ValueId, Atom>,
}

impl<'m> Reconstruction<'m> {
    fn new(module: &'m Module) -> Self {
        let bindings = Rebindings::analyze(module);
        let definitions = module
            .statements()
            .iter()
            .enumerate()
            .filter_map(|(i, slot)| {
                let Statement::Let { result, rhs } = slot.as_ref()? else {
                    return None;
                };
                Some((*result, (StatementId(i as u32), rhs)))
            })
            .collect();
        let templates = construction_templates(module, &bindings);
        let facts = ValueFacts::analyze(module, &bindings, &templates);
        Self {
            module,
            bindings,
            definitions,
            templates,
            facts,
            replacements: BTreeMap::new(),
        }
    }

    /// A retained alias reading a computed member is itself a force, so it stays bound and cannot be substituted away.
    fn follow(&self, mut atom: Atom) -> Atom {
        let mut seen = BTreeSet::new();
        while let Atom::Value(value) = atom {
            if !seen.insert(value) || self.bindings.is_computed(atom) {
                break;
            }
            let next = self
                .replacements
                .get(&value)
                .copied()
                .or_else(|| self.bindings.alias(value));
            let Some(next) = next else { break };
            if self.bindings.is_computed(next) {
                break;
            }
            atom = next;
        }
        atom
    }

    fn construction(&self, atom: Atom) -> Option<Construction> {
        let Atom::Value(value) = self.follow(atom) else {
            return None;
        };
        let (_, rhs) = self.definitions.get(&value)?;
        let (shape, fields) = match rhs {
            Rhs::Product { schema, fields } => (Shape::Product(*schema), fields.clone()),
            Rhs::Construct {
                constructor,
                fields,
            } => (Shape::Constructor(*constructor), fields.clone()),
            Rhs::Apply { callee, arguments } => {
                let Atom::Function(function) = self.bindings.resolve(self.follow(*callee)) else {
                    return None;
                };
                let template = self.templates.get(&function)?;
                (template.shape, template.instantiate(arguments)?)
            }
            _ => return None,
        };
        Some(Construction {
            shape,
            fields: fields.into_iter().map(|atom| self.follow(atom)).collect(),
        })
    }

    fn projection(&self, atom: Atom) -> Option<(ProductId, Atom, usize)> {
        let Atom::Value(value) = self.follow(atom) else {
            return None;
        };
        let (
            _,
            Rhs::Project {
                schema,
                product,
                field,
            },
        ) = self.definitions.get(&value)?
        else {
            return None;
        };
        Some((*schema, self.follow(*product), *field as usize))
    }

    fn product(&self, schema: ProductId, fields: &[Atom]) -> Option<Atom> {
        let base = fields.iter().enumerate().find_map(|(i, &field)| {
            let (read, base, index) = self.projection(field)?;
            (read == schema && i == index).then_some(base)
        })?;
        if self.bindings.is_computed(base) {
            return None;
        }
        fields
            .iter()
            .enumerate()
            .all(|(i, &field)| self.projection(field) == Some((schema, base, i))
                || matches!(self.follow(field), Atom::Constant(id) if matches!(self.module.constant(id), Some(Constant::Unit)))
                    && self.facts.unit_field(base, schema, i))
            .then_some(base)
    }

    fn administrative(&self, rhs: &Rhs) -> bool {
        if rhs
            .operands()
            .iter()
            .any(|&atom| self.bindings.is_computed(atom))
        {
            return false;
        }
        match rhs {
            Rhs::Alias(_) | Rhs::Project { .. } | Rhs::Product { .. } | Rhs::Construct { .. } => {
                true
            }
            Rhs::Apply { callee, .. } => {
                matches!(self.bindings.resolve(*callee), Atom::Function(function) if self.templates.contains_key(&function))
            }
            _ => false,
        }
    }

    fn returned(&self, block: BlockId) -> Option<Atom> {
        let block = self.module.block(block)?;
        if !block.statements.iter().all(|&id| matches!(self.module.statement(id), Some(Statement::Let { result, rhs }) if self.replacements.contains_key(result) || self.administrative(rhs))) { return None; }
        let Terminator::Return(atom) = block.terminator else {
            return None;
        };
        Some(self.follow(atom))
    }

    fn identity(&self, rhs: &Rhs) -> Option<Atom> {
        match rhs {
            Rhs::Product { schema, fields } => self.product(*schema, fields),
            Rhs::MatchVariant {
                scrutinee,
                arms,
                default,
                ..
            } => {
                let scrutinee = self.follow(*scrutinee);
                if arms.is_empty() || self.bindings.is_computed(scrutinee) {
                    return None;
                }
                for arm in arms {
                    let result = self.construction(self.returned(arm.block)?)?;
                    if result.shape != Shape::Constructor(arm.constructor)
                        || result.fields
                            != arm
                                .bindings
                                .iter()
                                .map(|&value| self.follow(Atom::Value(value)))
                                .collect::<Vec<_>>()
                    {
                        return None;
                    }
                }
                if let Some(default) = default
                    && self.returned(*default)? != scrutinee
                {
                    return None;
                }
                Some(scrutinee)
            }
            _ => None,
        }
    }
}

/// Shrink all reconstruction identities, including identities exposed by another one, and say whether any were found. Verification follows the driver's prune: removed arms still own arena slots until then.
pub(super) fn eliminate_reconstruction(module: &mut Module) -> bool {
    curios_profile::profile!("eliminate_reconstruction");
    let mut snapshot = Reconstruction::new(module);
    let mut users = BTreeMap::<ValueId, BTreeSet<StatementId>>::new();
    let mut owners = BTreeMap::<BlockId, StatementId>::new();
    for &(id, rhs) in snapshot.definitions.values() {
        for atom in rhs.operands() {
            if let Atom::Value(value) = atom {
                users.entry(value).or_default().insert(id);
            }
        }
        for block in rhs.sub_blocks() {
            owners.insert(block, id);
        }
    }
    for (&block, &owner) in &owners {
        let block = module.block(block).expect("an owned block is live");
        for atom in block.terminator.atoms() {
            if let Atom::Value(value) = atom {
                users.entry(value).or_default().insert(owner);
            }
        }
        for &id in &block.statements {
            if let Some(Statement::Let { result, .. }) = module.statement(id) {
                users.entry(*result).or_default().insert(owner);
            }
        }
    }
    let mut work = snapshot
        .definitions
        .values()
        .map(|&(id, _)| id)
        .collect::<BTreeSet<_>>();
    let mut observed = BTreeMap::new();
    let mut identities = BTreeSet::new();
    while let Some(id) = work.pop_first() {
        let Some(Statement::Let { result, rhs }) = module.statement(id) else {
            continue;
        };
        if !snapshot.replacements.contains_key(result)
            && let Some(base) = snapshot.identity(rhs)
        {
            snapshot.replacements.insert(*result, base);
            identities.insert(*result);
        }
        let resolved = snapshot.follow(Atom::Value(*result));
        let signature = (
            resolved,
            rhs.operands()
                .into_iter()
                .map(|atom| snapshot.follow(atom))
                .collect::<Vec<_>>(),
        );
        if observed.insert(*result, signature.clone()) != Some(signature) {
            work.extend(users.get(result).into_iter().flatten().copied());
        }
    }
    if identities.is_empty() {
        return false;
    }
    let replacements = snapshot
        .definitions
        .keys()
        .filter_map(|&value| {
            let resolved = snapshot.follow(Atom::Value(value));
            (resolved != Atom::Value(value)).then_some((value, resolved))
        })
        .collect::<BTreeMap<_, _>>();
    let removable = snapshot
        .definitions
        .iter()
        .filter_map(|(result, &(id, rhs))| {
            (snapshot.replacements.contains_key(result) || snapshot.administrative(rhs))
                .then_some(id)
        })
        .collect();
    let old_uses = owned_uses(module).1;
    drop(snapshot);
    substitute(module, &replacements);
    remove_stranded(module, &replacements, &removable, &old_uses);
    true
}

fn substitute(module: &mut Module, substitution: &BTreeMap<ValueId, Atom>) {
    let remap = Remap {
        values: &BTreeMap::new(),
        blocks: &BTreeMap::new(),
        functions: &BTreeMap::new(),
        rec_groups: &BTreeMap::new(),
        statements: &BTreeMap::new(),
        substitution,
        redirect: None,
        tables: None,
    };
    let statements = module
        .statements()
        .iter()
        .enumerate()
        .filter_map(|(i, slot)| {
            let mut statement = remap.statement(slot.as_ref()?);
            if let Statement::Let { result, rhs } = &mut statement
                && let Some(&replacement) = substitution.get(result)
            {
                *rhs = Rhs::Alias(replacement);
            }
            Some((StatementId(i as u32), statement))
        })
        .collect::<Vec<_>>();
    let blocks = module
        .blocks()
        .iter()
        .enumerate()
        .filter_map(|(i, slot)| {
            Some((
                BlockId(i as u32),
                remap.terminator(&slot.as_ref()?.terminator),
            ))
        })
        .collect::<Vec<_>>();
    for (id, statement) in statements {
        module.set_statement(id, statement);
    }
    for (id, terminator) in blocks {
        module.set_block_terminator(id, terminator);
    }
}

/// Count the current ownership graph, excluding arms the rewrite orphaned. Function references do not enter bodies; their lexical binding does.
fn owned_uses(module: &Module) -> (BTreeSet<StatementId>, BTreeMap<ValueId, usize>) {
    let mut statements = module.items().to_vec();
    let mut blocks = module.entry().into_iter().collect::<Vec<_>>();
    let mut seen = BTreeSet::new();
    let mut seen_blocks = BTreeSet::new();
    let mut uses = BTreeMap::new();
    loop {
        if let Some(id) = statements.pop() {
            if !seen.insert(id) {
                continue;
            }
            match module.statement(id).expect("an owned statement is live") {
                Statement::Let { rhs, .. } => {
                    for atom in rhs.operands() {
                        if let Atom::Value(value) = atom {
                            *uses.entry(value).or_default() += 1;
                        }
                    }
                    blocks.extend(rhs.sub_blocks());
                }
                Statement::Functions { functions } => blocks.extend(
                    functions
                        .iter()
                        .filter_map(|&id| module.function(id).map(|f| f.body)),
                ),
                Statement::Rec { group } => {
                    let group = module.rec_group(*group).expect("a group is live");
                    blocks.extend(group.values.iter().map(|member| member.init));
                    blocks.extend(
                        group
                            .functions
                            .iter()
                            .filter_map(|&id| module.function(id).map(|f| f.body)),
                    );
                }
            }
        } else if let Some(id) = blocks.pop() {
            if !seen_blocks.insert(id) {
                continue;
            }
            let block = module.block(id).expect("an owned block is live");
            statements.extend(&block.statements);
            for atom in block.terminator.atoms() {
                if let Atom::Value(value) = atom {
                    *uses.entry(value).or_default() += 1;
                }
            }
        } else {
            break;
        }
    }
    (seen, uses)
}

fn remove_stranded(
    module: &mut Module,
    replacements: &BTreeMap<ValueId, Atom>,
    removable: &BTreeSet<StatementId>,
    old_uses: &BTreeMap<ValueId, usize>,
) {
    let (owned, mut uses) = owned_uses(module);
    let definitions = owned
        .iter()
        .filter_map(|&id| {
            let Statement::Let { result, rhs } = module.statement(id)? else {
                return None;
            };
            Some((*result, (id, rhs.operands())))
        })
        .collect::<BTreeMap<_, _>>();
    let mut work = definitions
        .keys()
        .filter(|value| {
            replacements.contains_key(value) || old_uses.get(value).copied().unwrap_or(0) > 0
        })
        .copied()
        .collect::<BTreeSet<_>>();
    let mut removed = BTreeSet::new();
    while let Some(value) = work.pop_first() {
        let Some((id, operands)) = definitions.get(&value) else {
            continue;
        };
        if uses.get(&value).copied().unwrap_or(0) != 0
            || !removable.contains(id)
            || !removed.insert(*id)
        {
            continue;
        }
        for &atom in operands {
            if let Atom::Value(value) = atom {
                let count = uses.get_mut(&value).expect("a counted operand has a use");
                *count -= 1;
                work.insert(value);
            }
        }
    }
    let blocks = module
        .blocks()
        .iter()
        .enumerate()
        .filter_map(|(i, slot)| {
            let block = slot.as_ref()?;
            Some((
                BlockId(i as u32),
                block
                    .statements
                    .iter()
                    .filter(|id| !removed.contains(id))
                    .copied()
                    .collect(),
            ))
        })
        .collect::<Vec<_>>();
    for (id, statements) in blocks {
        module.set_block_statements(id, statements);
    }
    module.set_items(
        module
            .items()
            .iter()
            .filter(|id| !removed.contains(id))
            .copied()
            .collect(),
    );
}
