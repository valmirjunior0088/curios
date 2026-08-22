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
            CpsNode::LetValue { next, .. } | CpsNode::LetIntrinsic { next, .. } => {
                node_work.push_back(*next)
            }
            CpsNode::LetFun { body, .. } => node_work.push_back(*body),
            CpsNode::LetCont { body, .. } => node_work.push_back(*body),
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
        module.functions.live_count(),
        module.continuations.live_count(),
        module.nodes.live_count(),
        module.values.live_count(),
    );
    module.functions.retain(&functions);
    module.continuations.retain(&continuations);
    module
        .field_groups
        .retain(|continuation, _| continuations.contains(continuation));
    module.nodes.retain(&nodes);
    for (_, node) in module.nodes.iter_live_mut() {
        match node {
            CpsNode::LetFun {
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
    for (_, function) in module.functions.iter_live() {
        values.extend(function.params.iter().copied());
    }
    for (_, continuation) in module.continuations.iter_live() {
        values.extend(continuation.params.iter().copied());
    }
    for (_, node) in module.nodes.iter_live() {
        match node {
            CpsNode::LetValue { result, .. } | CpsNode::LetIntrinsic { result, .. } => {
                values.insert(*result);
            }
            _ => {}
        }
    }
    module.values.retain(&values);
    old != (
        module.functions.live_count(),
        module.continuations.live_count(),
        module.nodes.live_count(),
        module.values.live_count(),
    )
}
