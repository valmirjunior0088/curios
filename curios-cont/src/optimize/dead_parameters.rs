//! Dead-parameter cleanup over one demand snapshot, with continuation edits preceding the escape snapshot that admits function edits.

use {
    crate::{
        Atom, Callee, Demand, Edge, Module, Node, UseTarget, ValueId, atoms, demand_of, demands,
        visit_atoms_mut,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// Drop every entity's unread parameters, and the arguments every caller passes into them.
///
/// Deadness is read from [`crate::demand`]'s lattice rather than from a use count, which is the same question asked at the bottom point of a richer order — the one whose `Projected` point a return protocol needs. The lattice defers an argument's demand to the receiving parameter, so `Unused` here reaches further than a zero use count: a value threaded only into parameters nobody reads is dead however many edges carry it, and this pass deletes such a chain whole. The deletion stays well-formed because a parameter is always removed together with the argument every incoming edge passes into it, so no occurrence survives its binding.
///
/// One snapshot of the lattice serves every entity, because removing a parameter only ever removes uses: a verdict of `Unused` cannot be falsified by an earlier removal in the same sweep, and each entity's edit touches its own parameter list and the edges or calls into it alone. One entity per call would fire on nearly every round of a `Toml/decode` compile, each round of every pass buying one continuation's cleanup.
pub(crate) fn eliminate_dead_parameters(module: &mut Module) -> bool {
    let demands = demands(module);
    let dead_value = |value: &ValueId| demand_of(&demands, *value) == Demand::Unused;
    let dead_indices = |params: &[ValueId]| {
        params
            .iter()
            .enumerate()
            .filter_map(|(index, value)| dead_value(value).then_some(index))
            .collect::<BTreeSet<_>>()
    };
    let mut changed = false;

    // Precompute the continuations used as a return target in one pass, rather than rescanning every node for each continuation.
    let return_targets = module
        .nodes()
        .iter()
        .flatten()
        .filter_map(|node| match node {
            Node::ApplyFun { return_to, .. }
            | Node::Foreign { return_to, .. }
            | Node::Cell { return_to, .. }
            | Node::Channel { return_to, .. }
            | Node::Intrinsic { return_to, .. } => Some(*return_to),
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    let continuations = module
        .live_continuations()
        .filter(|(id, _)| !return_targets.contains(id))
        .filter_map(|(id, definition)| {
            let dead = dead_indices(&definition.params);
            (!dead.is_empty()).then_some((id, dead))
        })
        .collect::<BTreeMap<_, _>>();
    let mut removed = BTreeSet::new();
    for (&continuation, dead) in &continuations {
        removed.extend(remove_parameter_indices(
            &mut module.continuation_mut(continuation).unwrap().params,
            dead,
        ));
        module.remove_params_from_record(continuation, dead);
    }
    if !continuations.is_empty() {
        let repair = |edge: &mut Edge| {
            if let Some(dead) = continuations.get(&edge.target) {
                remove_parameter_indices(&mut edge.args, dead);
            }
        };
        for (_, node) in module.live_nodes_mut() {
            match node {
                Node::ApplyCont(edge) => repair(edge),
                Node::Switch { cases, default, .. } => {
                    for edge in cases.values_mut().chain(default.iter_mut()) {
                        repair(edge);
                    }
                }
                _ => {}
            }
            fill_removed_operands(node, &removed);
        }
        for value in removed {
            module.remove_value(value);
        }
        changed = true;
    }

    let escaping = module
        .nodes()
        .iter()
        .flatten()
        .flat_map(atoms)
        .filter_map(|atom| match atom {
            Atom::Fun(function) => Some(*function),
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    let functions = module
        .live_functions()
        .filter(|(id, _)| !escaping.contains(id))
        .filter_map(|(id, definition)| {
            let dead = dead_indices(&definition.params);
            (!dead.is_empty()).then_some((id, dead))
        })
        .collect::<Vec<_>>();
    for (function, dead) in functions {
        let removed =
            remove_parameter_indices(&mut module.function_mut(function).unwrap().params, &dead);
        for (_, node) in module.live_nodes_mut() {
            if let Node::ApplyFun {
                callee: Callee::Known(callee),
                args,
                ..
            } = node
                && *callee == function
            {
                remove_parameter_indices(args, &dead);
            }
        }
        // The same inert occurrences as the continuation half above — see its comment.
        for &value in &removed {
            module.replace_atom(UseTarget::Value(value), Atom::Filler);
        }
        for value in removed {
            module.remove_value(value);
        }
        changed = true;
    }
    changed
}
/// `Unused` excludes consumption, not occurrences: a removed binding may still be passed into a retained signature whose receiving parameter is also unused. A filler preserves that arity without retaining a dead name. The demand of an indirect callee is `Applied`, so the operand visitor's omission of it cannot leave a removed binding behind.
fn fill_removed_operands(node: &mut Node, removed: &BTreeSet<ValueId>) {
    if let Node::ApplyFun {
        callee: Callee::Closure(value),
        ..
    } = node
    {
        debug_assert!(!removed.contains(value));
    }
    visit_atoms_mut(node, &mut |atom| {
        if let Atom::Value(value) = atom
            && removed.contains(value)
        {
            *atom = Atom::Filler;
        }
    });
}

pub(crate) fn remove_parameter_indices<T>(
    values: &mut Vec<T>,
    removed: &BTreeSet<usize>,
) -> Vec<T> {
    let mut removed_values = Vec::new();
    let mut retained = Vec::with_capacity(values.len() - removed.len());
    for (index, value) in std::mem::take(values).into_iter().enumerate() {
        if removed.contains(&index) {
            removed_values.push(value);
        } else {
            retained.push(value);
        }
    }
    *values = retained;
    removed_values
}

#[cfg(test)]
mod tests;
