use {
    super::*,
    std::collections::{BTreeSet, VecDeque},
};

pub(super) fn prune_unreachable(module: &mut Module) -> bool {
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
            if let Atom::Fun(function) = atom {
                function_work.push_back(*function);
            }
        }
        let mut queue_cont = |target: ContinuationId| {
            if let Some(continuation) = module.continuation(target)
                && continuations.insert(target)
            {
                node_work.push_back(continuation.body);
            }
        };
        match node {
            Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. } => {
                node_work.push_back(*next)
            }
            Node::LetFun { body, .. } => node_work.push_back(*body),
            Node::LetCont { body, .. } => node_work.push_back(*body),
            Node::ApplyFun {
                callee, return_to, ..
            } => {
                if let Callee::Known(function) = callee {
                    function_work.push_back(*function);
                }
                queue_cont(*return_to);
            }
            Node::ApplyCont(edge) => queue_cont(edge.target),
            Node::Switch { cases, default, .. } => {
                for edge in cases.values().chain(default.iter()) {
                    queue_cont(edge.target);
                }
            }
            Node::Foreign { return_to, .. }
            | Node::Cell { return_to, .. }
            | Node::Intrinsic { return_to, .. } => queue_cont(*return_to),
            Node::Exit { .. } | Node::Panic(_) | Node::Unreachable => {}
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
            Node::LetFun {
                functions: members, ..
            } => members.retain(|function| functions.contains(function)),
            Node::LetCont {
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
            Node::LetValue { result, .. } | Node::LetIntrinsic { result, .. } => {
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
