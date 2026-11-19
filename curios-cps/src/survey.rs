//! What a CPS module holds of each function of a declaration's text, read off the graph alone.
//!
//! A function carries the function of a declaration's text it was made of — its [`Lineage`] — and so does every continuation the lowering made of its body, kept by every copy and moved with the code a pass moves. So what a module holds of one is read off the module: the functions of its lineage, the heap twin of each, and the functions of another lineage a continuation of its lineage stands in, where a pass put its code in another's body. What became of it under the optimizer is the difference between what the graphs either side of it hold.
//!
//! **Nothing here instruments a pass.** No rewrite records what it did, and none has to: `curios-pipeline` already observes the graph before and after optimization, so a caller that reads both sides learns what became of each function without the optimizer knowing it is being watched. That is what keeps the measured program the shipped program — the failure GHC's `-fprof-late` exists to undo, avoided by not annotating rather than by annotating late.
//!
//! **What a graph cannot tell apart:** straight-line code inlined into a caller leaves no continuation of its lineage behind, so a function inlined whole is held by nothing, as one pruned is; telling them apart needs the passes to say which.

use {
    crate::{FunctionId, Module, Node, ValueExpr, function_nodes},
    curios_common::Lineage,
    std::collections::{BTreeMap, BTreeSet},
};

/// What a module holds of one lineage.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Held {
    /// The functions of the lineage, a pass's copies among them, in the module's order: what runs it, heap twins and what they hold aside.
    pub functions: Vec<FunctionId>,
    /// The heap twin the frame bound made of each of them that is on a recursion, which runs it on the heap and is no copy a pass made.
    pub twins: Vec<Twin>,
    /// The functions of another lineage, or of none, whose own body binds a continuation of this one, or within whose heap twin a function of it is bound: where a pass put its code.
    pub standing_in: BTreeSet<FunctionId>,
}

/// A heap twin, with the functions of its lineage the frame bound lifted out of its continuations and binds within it, so a frame of it can wait on the heap.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Twin {
    pub function: FunctionId,
    pub lifted: Vec<FunctionId>,
}

/// What `module` holds of every lineage it holds anything of.
pub fn held(module: &Module) -> BTreeMap<Lineage, Held> {
    let twins = module
        .function_ids()
        .filter_map(|function| module.twin(function))
        .collect::<BTreeSet<_>>();
    // The function whose own body binds each function a body binds.
    let binders = module
        .function_ids()
        .flat_map(|binder| {
            function_nodes(module, binder)
                .into_iter()
                .filter_map(|node| match module.node(node) {
                    Some(Node::LetFun { functions, .. }) => Some(functions.clone()),
                    _ => None,
                })
                .flatten()
                .map(move |function| (function, binder))
        })
        .collect::<BTreeMap<_, _>>();
    let twin_binding = |mut function| loop {
        let binder = *binders.get(&function)?;
        if twins.contains(&binder) {
            return Some(binder);
        }
        function = binder;
    };
    let mut held = BTreeMap::<Lineage, Held>::new();
    let mut lifted = BTreeMap::<FunctionId, Vec<FunctionId>>::new();

    for id in module.function_ids() {
        let Some(function) = module.function(id) else {
            continue;
        };
        match (function.lineage, twin_binding(id)) {
            (Some(_), _) if twins.contains(&id) => {}
            (Some(lineage), Some(twin))
                if module.function(twin).and_then(|twin| twin.lineage) == Some(lineage) =>
            {
                lifted.entry(twin).or_default().push(id);
            }
            (Some(lineage), Some(twin)) => {
                held.entry(lineage).or_default().standing_in.insert(twin);
            }
            (Some(lineage), None) => {
                let held = held.entry(lineage).or_default();
                held.functions.push(id);
                held.twins.extend(module.twin(id).map(|function| Twin {
                    function,
                    lifted: Vec::new(),
                }));
            }
            (None, _) => {}
        }
        for node in function_nodes(module, id) {
            let Some(Node::LetCont { continuations, .. }) = module.node(node) else {
                continue;
            };
            let standing = continuations
                .iter()
                .filter_map(|&continuation| module.continuation(continuation)?.lineage)
                .filter(|&lineage| Some(lineage) != function.lineage);
            for lineage in standing {
                held.entry(lineage).or_default().standing_in.insert(id);
            }
        }
    }
    for twin in held.values_mut().flat_map(|held| &mut held.twins) {
        twin.lifted = lifted.remove(&twin.function).unwrap_or_default();
    }

    held
}

/// How many values `function`'s own body builds in the memory of one it holds dead ([`ValueExpr::Rebuilt`]), a nested function's aside.
pub fn rebuilds(module: &Module, function: FunctionId) -> usize {
    function_nodes(module, function)
        .into_iter()
        .filter(|&node| {
            matches!(
                module.node(node),
                Some(Node::LetValue {
                    value: ValueExpr::Rebuilt { .. },
                    ..
                })
            )
        })
        .count()
}

#[cfg(test)]
mod tests;
