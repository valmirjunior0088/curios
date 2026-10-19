//! Collapsing a nest of lambdas into one function of all their parameters, where every call applies it in turn.
//!
//! A level of a nest binds the next lambda and returns it, and does nothing else; the next lambda is named nowhere but that return. Returned-closure uncurrying absorbs one level a round, so a nest n levels deep would take n rounds of the optimizer, each a walk of the module; and handing each absorbed argument on through every level would give each level the arguments of every level inside it, the square of the nest. Collapsed instead, the function takes every level's parameters at once and its body becomes the innermost level's, each level's body moved rather than copied, and each call passes the arguments its resumes applied.

#[cfg(test)]
mod tests;

use {
    super::values_bound_in,
    crate::{
        Atom, Callee, ContinuationId, Edge, FunctionId, Module, Node, NodeId, ValueId, atoms,
        continuation_entries, function_nodes, nodes_from, retarget_transfers,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// Collapse every nest whose calls all apply it in turn, to the depth every call applies, answering whether one was.
///
/// Nests are independent: a level is bound inside the one before it, a call's resumes are entered by calls of its function alone, and a collapse moves bodies without rewriting them, so one snapshot admits them all.
pub(crate) fn uncurry_nests(module: &mut Module) -> bool {
    let facts = Facts::of(module);
    let entries = continuation_entries(module);
    let nests = module
        .function_ids()
        .filter_map(|function| plan(module, &facts, &entries, function))
        .collect::<Vec<_>>();
    let changed = !nests.is_empty();
    for nest in nests {
        collapse(module, nest);
    }
    changed
}

/// What a snapshot of the module says of its names, read once.
#[derive(Default)]
struct Facts {
    /// How many times each value is named, as [`Module::value_use_counts`] counts.
    uses: BTreeMap<ValueId, usize>,
    /// How many nodes name each function as a value.
    named: BTreeMap<FunctionId, usize>,
    /// The nodes calling each function by name.
    calls: BTreeMap<FunctionId, Vec<NodeId>>,
}

impl Facts {
    fn of(module: &Module) -> Self {
        let mut facts = Self {
            uses: module.value_use_counts(),
            ..Self::default()
        };
        for (id, node) in module.live_nodes() {
            for atom in atoms(node) {
                if let Atom::Fun(function) = atom {
                    *facts.named.entry(*function).or_default() += 1;
                }
            }
            if let Node::ApplyFun {
                callee: Callee::Known(function),
                ..
            } = node
            {
                facts.calls.entry(*function).or_default().push(id);
            }
        }
        facts
    }
}

/// One nest to collapse: the function, the levels it absorbs, what each call passes besides its own arguments, and the applications each call's resumes made.
struct Nest {
    function: FunctionId,
    levels: Vec<FunctionId>,
    calls: Vec<(NodeId, Vec<Atom>)>,
    applications: BTreeMap<ContinuationId, Vec<Application>>,
}

/// One application a resume makes of what the call before it returned.
#[derive(Clone)]
struct Application {
    node: NodeId,
    args: Vec<Atom>,
    result: ValueId,
    after: ContinuationId,
}

fn plan(
    module: &Module,
    facts: &Facts,
    entries: &BTreeMap<ContinuationId, Vec<Option<FunctionId>>>,
    function: FunctionId,
) -> Option<Nest> {
    if module.entry() == Some(function) || facts.named.contains_key(&function) {
        return None;
    }
    let levels = levels(module, facts, function);
    let calls = facts.calls.get(&function)?;
    let mut depth = levels.len();
    let mut admitted = Vec::new();
    for &call in calls {
        let (first, applied) = applications(module, facts, entries, function, &levels, call)?;
        depth = depth.min(applied.len());
        admitted.push((call, first, applied));
    }
    if depth == 0 {
        return None;
    }

    let mut nest = Nest {
        function,
        levels: levels[..depth].to_vec(),
        calls: Vec::new(),
        applications: BTreeMap::new(),
    };
    for (call, first, mut applied) in admitted {
        applied.truncate(depth);
        let passed = applied
            .iter()
            .flat_map(|application| application.args.iter().cloned())
            .collect();
        nest.calls.push((call, passed));
        nest.applications.insert(first, applied);
    }
    Some(nest)
}

/// The lambdas `function` returns one inside the next: each level's body binds the next and returns it, and the next is named nowhere else.
fn levels(module: &Module, facts: &Facts, function: FunctionId) -> Vec<FunctionId> {
    let mut levels = Vec::new();
    let mut level = function;
    while let Some(definition) = module.function(level) {
        let Some(Node::LetFun { functions, body }) = module.node(definition.body) else {
            break;
        };
        let [next] = functions.as_slice() else {
            break;
        };
        let Some(Node::ApplyCont(Edge { target, args })) = module.node(*body) else {
            break;
        };
        if *target != definition.return_cont
            || args.as_slice() != [Atom::Fun(*next)]
            || facts.named.get(next) != Some(&1)
            || facts.calls.contains_key(next)
        {
            break;
        }
        levels.push(*next);
        level = *next;
    }
    levels
}

/// The applications `call`'s resumes make in turn of what it returns, one a level, with the resume the call returns to — or `None` where that resume is entered by anything but calls of `function`.
///
/// A resume's one use of what it receives is one application, reached from its head through `LetCont`s alone, so nothing it evaluates moves past the application; the next resume is entered by that application alone; and every argument stands where the call does. The applications stop at the first resume that is not so.
fn applications(
    module: &Module,
    facts: &Facts,
    entries: &BTreeMap<ContinuationId, Vec<Option<FunctionId>>>,
    function: FunctionId,
    levels: &[FunctionId],
    call: NodeId,
) -> Option<(ContinuationId, Vec<Application>)> {
    let Some(Node::ApplyFun {
        callee: Callee::Known(callee),
        return_to: first,
        ..
    }) = module.node(call)
    else {
        return None;
    };
    let resume = module.continuation(*first)?;
    if *callee != function
        || !entries
            .get(first)?
            .iter()
            .all(|entry| *entry == Some(function))
    {
        return None;
    }
    let (values, functions) = bound_within(module, resume.body);

    let mut applied = Vec::new();
    let mut at = *first;
    for (depth, level) in levels.iter().enumerate() {
        let Some(resume) = module.continuation(at) else {
            break;
        };
        let [result] = resume.params.as_slice() else {
            break;
        };
        if depth > 0 && entries.get(&at).map_or(0, Vec::len) != 1
            || facts.uses.get(result) != Some(&1)
        {
            break;
        }
        let node = past_continuations(module, resume.body);
        let Some(Node::ApplyFun {
            callee: Callee::Closure(applied_value),
            args,
            return_to: after,
        }) = module.node(node)
        else {
            break;
        };
        let stands = args.iter().all(|arg| match arg {
            Atom::Value(value) => !values.contains(value),
            Atom::Fun(named) => !functions.contains(named),
            Atom::Literal(_) | Atom::Filler => true,
        });
        if applied_value != result || !module.applies(*level, args.len()) || !stands {
            break;
        }
        applied.push(Application {
            node,
            args: args.clone(),
            result: *result,
            after: *after,
        });
        at = *after;
    }
    Some((*first, applied))
}

/// The node `body` reaches by binding continuations and doing nothing else.
fn past_continuations(module: &Module, body: NodeId) -> NodeId {
    let mut node = body;
    while let Some(Node::LetCont { body, .. }) = module.node(node) {
        node = *body;
    }
    node
}

/// The values and the functions bound beneath `body`, which the call above it cannot name.
fn bound_within(module: &Module, body: NodeId) -> (BTreeSet<ValueId>, BTreeSet<FunctionId>) {
    let values = values_bound_in(module, body);
    let mut functions = BTreeSet::new();
    for node in nodes_from(module, body) {
        if let Some(Node::LetFun {
            functions: bound, ..
        }) = module.node(node)
        {
            functions.extend(bound.iter().copied());
        }
    }
    (values, functions)
}

fn collapse(module: &mut Module, nest: Nest) {
    let Nest {
        function,
        levels,
        calls,
        applications,
    } = nest;
    let definition = module.function(function).cloned().expect("a nest is live");
    let innermost = *levels.last().expect("a nest has a level");
    let inner = module
        .function(innermost)
        .cloned()
        .expect("a level is live");

    for node in function_nodes(module, innermost) {
        let mut rewritten = module.node(node).expect("a level's node is live").clone();
        retarget_transfers(&mut rewritten, inner.return_cont, definition.return_cont);
        module.set_node(node, rewritten);
    }

    let mut params = definition.params.clone();
    let mut wrappers = vec![definition.body];
    for (index, level) in levels.iter().enumerate() {
        let level_definition = module.function(*level).expect("a level is live");
        params.extend(level_definition.params.iter().copied());
        if index + 1 < levels.len() {
            wrappers.push(level_definition.body);
        }
    }
    for wrapper in wrappers {
        if let Some(Node::LetFun { body, .. }) = module.remove_node(wrapper) {
            module.remove_node(body);
        }
    }
    for level in &levels {
        module.remove_function(*level);
    }
    let rewritten = module.function_mut(function).expect("a nest is live");
    rewritten.params = params;
    rewritten.body = inner.body;

    for (call, passed) in calls {
        if let Some(Node::ApplyFun { args, .. }) = module.node_mut(call) {
            args.extend(passed);
        }
    }
    // What the call now returns is the answer every application made, so each resume hands it on to the next.
    for application in applications.into_values().flatten() {
        module.set_node(
            application.node,
            Node::ApplyCont(Edge {
                target: application.after,
                args: vec![Atom::Value(application.result)],
            }),
        );
    }
}
