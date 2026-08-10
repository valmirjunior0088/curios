//! Derived analyses — computed on demand from the current module state as one immutable snapshot, never maintained as shadow state.
//!
//! The snapshot holds exactly the facts the specification names consumers for: per-value use counts, per-function lexical free values (functions store no capture lists — this is where captures come from), the function reference graph (an edge for a referenced function atom and for a function bound within another's body, so recursion through nesting is visible), and its strongly connected components with a recursion test. All maps are ordered, every walk is an explicit worklist, and the same module always produces an equal snapshot.
//!
//! Precondition: the module verifies. Analysis of an unverified module may produce arbitrary (but still terminating) results.

#[cfg(test)]
mod tests;

use {
    super::{Atom, BlockId, FunctionId, Module, Statement, StatementId, ValueId},
    std::collections::{BTreeMap, BTreeSet},
};

/// An immutable analysis snapshot of one module state.
#[derive(Debug, PartialEq, Eq)]
pub struct Analysis {
    /// Use counts, indexed by `ValueId`. A dense arena index, so a `Vec` is both faster and *more* obviously deterministic than a map: it cannot be iterated in any order but id order. Never iterated in practice — read only through [`Analysis::value_uses`].
    value_uses: Vec<usize>,
    free_values: BTreeMap<FunctionId, BTreeSet<ValueId>>,
    references: BTreeMap<FunctionId, BTreeSet<FunctionId>>,
    components: Vec<Vec<FunctionId>>,
    component_of: BTreeMap<FunctionId, usize>,
}

/// What one region's walk collected. A region is a function's immediate body subtree — its blocks and their sub-blocks, but not the bodies of functions bound inside it — or the module's top level (items plus entry).
#[derive(Default)]
struct Region {
    used: BTreeSet<ValueId>,
    bound: BTreeSet<ValueId>,
    references: BTreeSet<FunctionId>,
    children: Vec<FunctionId>,
}

impl Analysis {
    /// Analyze the module's current state.
    pub fn analyze(module: &Module) -> Self {
        curios_profile::profile!("analyze_module");
        let mut value_uses = vec![0usize; module.values().len()];

        // Walk the top level and every live function as separate regions. The top level contributes use counts but is not itself a function region.
        walk_region(
            module,
            module.items().iter().copied(),
            module.entry().into_iter().collect(),
            &mut value_uses,
        );
        let mut regions: BTreeMap<FunctionId, Region> = BTreeMap::new();
        for (index, slot) in module.functions().iter().enumerate() {
            let Some(function) = slot else { continue };
            let id = FunctionId(index as u32);
            let mut region = walk_region(
                module,
                std::iter::empty(),
                vec![function.body],
                &mut value_uses,
            );
            region.bound.extend(function.params.iter().copied());
            regions.insert(id, region);
        }

        // Nesting: the region a function is bound in is its parent; functions bound at the top level are roots. Depths order the free-value propagation so children resolve before their parents.
        let mut parents: BTreeMap<FunctionId, FunctionId> = BTreeMap::new();
        for (&id, region) in &regions {
            for &child in &region.children {
                parents.insert(child, id);
            }
        }
        let mut depths: BTreeMap<FunctionId, usize> = BTreeMap::new();
        let mut queue: Vec<(FunctionId, usize)> = regions
            .keys()
            .filter(|id| !parents.contains_key(id))
            .map(|&id| (id, 0))
            .collect();
        while let Some((id, depth)) = queue.pop() {
            depths.insert(id, depth);
            queue.extend(
                regions[&id]
                    .children
                    .iter()
                    .map(|&child| (child, depth + 1)),
            );
        }

        // Free values, deepest first: a function frees what its region uses and what the functions bound inside it free, minus what its region (and parameter list) binds.
        let mut order: Vec<FunctionId> = regions.keys().copied().collect();
        order.sort_by_key(|id| std::cmp::Reverse((depths.get(id).copied().unwrap_or(0), *id)));
        let mut free_values: BTreeMap<FunctionId, BTreeSet<ValueId>> = BTreeMap::new();
        for id in order {
            let region = &regions[&id];
            let mut free: BTreeSet<ValueId> = region.used.clone();
            for &child in &region.children {
                free.extend(free_values.get(&child).into_iter().flatten().copied());
            }
            free.retain(|value| !region.bound.contains(value));
            free_values.insert(id, free);
        }

        let references: BTreeMap<FunctionId, BTreeSet<FunctionId>> = regions
            .iter()
            .map(|(&id, region)| (id, region.references.clone()))
            .collect();

        let components = condense(&references);
        let component_of = components
            .iter()
            .enumerate()
            .flat_map(|(index, members)| members.iter().map(move |&member| (member, index)))
            .collect();

        Self {
            value_uses,
            free_values,
            references,
            components,
            component_of,
        }
    }

    /// How many times a value is referenced as an operand or terminator atom anywhere in the module. Definitions are not uses; an unreferenced value is absent.
    pub fn value_uses(&self, value: ValueId) -> usize {
        self.value_uses.get(value.index()).copied().unwrap_or(0)
    }

    /// The values a function references but does not bind — its derived captures, including values reached only through functions bound inside its body.
    pub fn free_values(&self, function: FunctionId) -> &BTreeSet<ValueId> {
        static EMPTY: BTreeSet<ValueId> = BTreeSet::new();
        self.free_values.get(&function).unwrap_or(&EMPTY)
    }

    /// The functions a function references — by atom, or by binding them within its body.
    pub fn references(&self, function: FunctionId) -> &BTreeSet<FunctionId> {
        static EMPTY: BTreeSet<FunctionId> = BTreeSet::new();
        self.references.get(&function).unwrap_or(&EMPTY)
    }

    /// The strongly connected components of the reference graph, each sorted, in deterministic condensation (reverse topological) order.
    pub fn components(&self) -> &[Vec<FunctionId>] {
        &self.components
    }

    /// The index into [`components`](Self::components) of the component a function belongs to.
    pub fn component_of(&self, function: FunctionId) -> Option<usize> {
        self.component_of.get(&function).copied()
    }

    /// Whether a component is recursive: more than one member, or a member referencing itself.
    pub fn is_recursive(&self, component: usize) -> bool {
        let members = &self.components[component];
        members.len() > 1
            || members
                .iter()
                .any(|&member| self.references(member).contains(&member))
    }
}

/// Walk one region iteratively: the given statements (the top level's items), then every block reachable through statement sub-blocks and recursive-group initializers — never entering a bound function's body.
fn walk_region(
    module: &Module,
    items: impl IntoIterator<Item = StatementId>,
    blocks: Vec<BlockId>,
    value_uses: &mut [usize],
) -> Region {
    let mut region = Region::default();
    let mut statements: Vec<StatementId> = items.into_iter().collect();
    let mut blocks = blocks;

    let mut use_atom = |region: &mut Region, atom: Atom| match atom {
        Atom::Value(value) => {
            if let Some(count) = value_uses.get_mut(value.index()) {
                *count += 1;
            }
            region.used.insert(value);
        }
        Atom::Function(function) => {
            region.references.insert(function);
        }
        Atom::Constant(_) => {}
    };

    loop {
        if let Some(statement) = statements.pop() {
            match module.statement(statement).expect("live statement") {
                Statement::Let { result, rhs } => {
                    region.bound.insert(*result);
                    for atom in rhs.operands() {
                        use_atom(&mut region, atom);
                    }
                    region.bound.extend(rhs.binders());
                    blocks.extend(rhs.sub_blocks());
                }
                Statement::Functions { functions } => {
                    region.references.extend(functions.iter().copied());
                    region.children.extend(functions.iter().copied());
                }
                Statement::Rec { group } => {
                    let group = module.rec_group(*group).expect("live group");
                    region.references.extend(group.functions.iter().copied());
                    region.children.extend(group.functions.iter().copied());
                    for member in &group.values {
                        region.bound.insert(member.value);
                        blocks.push(member.init);
                    }
                }
            }
            continue;
        }
        let Some(block) = blocks.pop() else { break };
        let block = module.block(block).expect("live block");
        statements.extend(block.statements.iter().copied());
        if let Some(atom) = block.terminator.atom() {
            use_atom(&mut region, atom);
        }
    }
    region
}

/// Iterative Tarjan condensation over the reference graph. Nodes iterate in identity order and neighbors in sorted order, so component membership and order are deterministic; each component is emitted sorted, in completion (reverse topological) order.
fn condense(references: &BTreeMap<FunctionId, BTreeSet<FunctionId>>) -> Vec<Vec<FunctionId>> {
    #[derive(Default, Clone)]
    struct NodeState {
        index: Option<usize>,
        low_link: usize,
        on_stack: bool,
    }

    let mut states: BTreeMap<FunctionId, NodeState> = references
        .keys()
        .map(|&id| (id, NodeState::default()))
        .collect();
    let mut components = Vec::new();
    let mut stack: Vec<FunctionId> = Vec::new();
    let mut next_index = 0usize;

    for &root in references.keys() {
        if states[&root].index.is_some() {
            continue;
        }
        // Explicit DFS frames: (node, position within its neighbor list).
        let mut frames: Vec<(FunctionId, usize)> = vec![(root, 0)];
        while let Some(last) = frames.last_mut() {
            let (node, position) = *last;
            last.1 += 1;
            if position == 0 {
                let state = states.get_mut(&node).expect("known node");
                state.index = Some(next_index);
                state.low_link = next_index;
                state.on_stack = true;
                next_index += 1;
                stack.push(node);
            }
            let neighbor = references[&node]
                .iter()
                .copied()
                .filter(|neighbor| states.contains_key(neighbor))
                .nth(position);
            if let Some(neighbor) = neighbor {
                match states[&neighbor].index {
                    None => frames.push((neighbor, 0)),
                    Some(index) => {
                        if states[&neighbor].on_stack {
                            let low = states[&node].low_link.min(index);
                            states.get_mut(&node).expect("known node").low_link = low;
                        }
                    }
                }
                continue;
            }
            frames.pop();
            if let Some(&(parent, _)) = frames.last() {
                let low = states[&parent].low_link.min(states[&node].low_link);
                states.get_mut(&parent).expect("known node").low_link = low;
            }
            if states[&node].low_link == states[&node].index.expect("visited") {
                let mut component = Vec::new();
                while let Some(member) = stack.pop() {
                    states.get_mut(&member).expect("known node").on_stack = false;
                    component.push(member);
                    if member == node {
                        break;
                    }
                }
                component.sort();
                components.push(component);
            }
        }
    }
    components
}
