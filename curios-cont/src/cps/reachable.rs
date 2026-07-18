use {
    super::*,
    std::collections::{BTreeSet, VecDeque},
};

pub(super) fn prune_unreachable(module: &mut CpsModule) -> bool {
    let Some(entry) = module.entry else {
        return false;
    };
    let mut functions = BTreeSet::new();
    let mut continuations = BTreeSet::new();
    let mut nodes = BTreeSet::new();
    let mut function_work = VecDeque::from([entry]);
    let mut node_work = VecDeque::new();

    while !function_work.is_empty() || !node_work.is_empty() {
        while let Some(function) = function_work.pop_front() {
            if functions.insert(function) {
                node_work.push_back(module.function(function).unwrap().body);
            }
        }
        let Some(node_id) = node_work.pop_front() else {
            continue;
        };
        if !nodes.insert(node_id) {
            continue;
        }
        let node = module.node(node_id).unwrap();
        for atom in atoms(node) {
            if let CpsAtom::Fun(function) = atom {
                function_work.push_back(*function);
            }
        }
        let mut queue_cont = |target: CpsContId| {
            if let Some(continuation) = module.continuation(target)
                && continuations.insert(target)
            {
                node_work.push_back(continuation.body);
            }
        };
        match node {
            CpsNode::LetValue { next, .. } | CpsNode::LetPrim { next, .. } => {
                node_work.push_back(*next)
            }
            CpsNode::LetFun { body, .. } => node_work.push_back(*body),
            CpsNode::LetCont { body, .. } => node_work.push_back(*body),
            CpsNode::RecInit {
                functions, body, ..
            } => {
                function_work.extend(functions.iter().copied());
                node_work.push_back(*body);
            }
            CpsNode::ApplyFun {
                callee, return_to, ..
            } => {
                if let CpsCallee::Known(function) = callee {
                    function_work.push_back(*function);
                }
                queue_cont(*return_to);
            }
            CpsNode::ApplyCont(edge) => queue_cont(edge.target),
            CpsNode::Switch { cases, default, .. } => {
                for edge in cases.values().chain(default.iter()) {
                    queue_cont(edge.target);
                }
            }
            CpsNode::Foreign { return_to, .. }
            | CpsNode::Cell { return_to, .. }
            | CpsNode::Intrinsic { return_to, .. } => queue_cont(*return_to),
            CpsNode::Exit { .. } | CpsNode::Unreachable => {}
        }
    }

    let old = (
        module.functions.iter().flatten().count(),
        module.continuations.iter().flatten().count(),
        module.nodes.iter().flatten().count(),
        module.values.iter().flatten().count(),
    );
    for (index, function) in module.functions.iter_mut().enumerate() {
        if !functions.contains(&CpsFunId(index as u32)) {
            *function = None;
        }
    }
    for (index, continuation) in module.continuations.iter_mut().enumerate() {
        if !continuations.contains(&CpsContId(index as u32)) {
            *continuation = None;
        }
    }
    for (index, node) in module.nodes.iter_mut().enumerate() {
        if !nodes.contains(&CpsNodeId(index as u32)) {
            *node = None;
            continue;
        }
        match node.as_mut().unwrap() {
            CpsNode::LetFun {
                functions: members, ..
            }
            | CpsNode::RecInit {
                functions: members, ..
            } => members.retain(|function| functions.contains(function)),
            CpsNode::LetCont {
                continuations: members,
                ..
            } => members.retain(|continuation| continuations.contains(continuation)),
            _ => {}
        }
    }
    let mut values = BTreeSet::new();
    for function in module.functions.iter().flatten() {
        values.extend(function.params.iter().copied());
    }
    for continuation in module.continuations.iter().flatten() {
        values.extend(continuation.params.iter().copied());
    }
    for node in module.nodes.iter().flatten() {
        match node {
            CpsNode::LetValue { result, .. } | CpsNode::LetPrim { result, .. } => {
                values.insert(*result);
            }
            CpsNode::RecInit {
                values: recursive_values,
                ..
            } => values.extend(recursive_values.iter().copied()),
            _ => {}
        }
    }
    for (index, value) in module.values.iter_mut().enumerate() {
        if !values.contains(&CpsValueId(index as u32)) {
            *value = None;
        }
    }
    old != (
        module.functions.iter().flatten().count(),
        module.continuations.iter().flatten().count(),
        module.nodes.iter().flatten().count(),
        module.values.iter().flatten().count(),
    )
}
