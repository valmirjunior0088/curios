//! Dead-parameter cleanup over one demand snapshot, with continuation edits preceding the escape snapshot that admits function edits.

use {
    crate::{Atom, Callee, Demand, Module, Node, UseTarget, ValueId, atoms, demand_of, demands},
    std::collections::BTreeSet,
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
        .collect::<Vec<_>>();
    for (continuation, dead) in continuations {
        let removed = remove_parameter_indices(
            &mut module.continuation_mut(continuation).unwrap().params,
            &dead,
        );
        module.remove_params_from_record(continuation, &dead);
        for (_, node) in module.live_nodes_mut() {
            match node {
                Node::ApplyCont(edge) if edge.target == continuation => {
                    remove_parameter_indices(&mut edge.args, &dead);
                }
                Node::Switch { cases, default, .. } => {
                    for edge in cases.values_mut().chain(default.iter_mut()) {
                        if edge.target == continuation {
                            remove_parameter_indices(&mut edge.args, &dead);
                        }
                    }
                }
                _ => {}
            }
        }
        // `Unused` means never *consumed*, not never occurring: the deferral arms of the demand walk leave a dropped parameter standing as a known call's argument, or in an edge into some other continuation whose receiving parameter is itself unused. Those occurrences are proven inert, so they become fillers — arity intact, and the receiving side's own dead-parameter drop erases them on a later round.
        for &value in &removed {
            module.replace_atom(UseTarget::Value(value), Atom::Filler);
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
