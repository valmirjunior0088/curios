mod scc;
pub(crate) use scc::*;

mod pattern;
pub(crate) use pattern::*;

mod call;
pub(crate) use call::*;

mod jump;
pub(crate) use jump::*;

use {
    crate::{
        Atom, CallAnalysis, Callee, FunctionId, Lattice, Literal, Module, Node, NodeId, Slot,
        Solver, ValueExpr, ValueId, resolve_atom,
    },
    std::collections::{BTreeMap, BTreeSet},
};

#[derive(Clone, PartialEq)]
pub(crate) enum Knowledge {
    Unknown,
    Known(Atom),
    Conflict,
}
impl Knowledge {
    /// Fold one transfer in: `None` is a transfer whose arguments cannot be seen — an escaping reference, an operation delivering its result — and it is a conflict from whatever state, not only from `Known`. Leaving `Unknown` standing would let a join that is first an operation's `return_to` and then a jump's target learn the jump's literal as if it were the only way in, so the order the arena happens to list the two would decide what the program computes.
    fn merge(&mut self, incoming: Option<&Atom>) {
        match (&*self, incoming) {
            (Self::Conflict, _) => {}
            (_, None) => *self = Self::Conflict,
            (Self::Unknown, Some(atom)) => *self = Self::Known(atom.clone()),
            (Self::Known(current), Some(incoming)) if current == incoming => {}
            (Self::Known(_), Some(_)) => *self = Self::Conflict,
        }
    }
}

/// Ordered `Unknown < Known(_) < Conflict`.
///
/// Note how this differs from [`Knowledge::merge`], which is *not* this join and must not be confused with it: `merge` reads its `None` as "a caller I cannot observe" and lets that force a `Conflict`, whereas here `Unknown` is the identity, so a forwarded parameter still resolving to `Unknown` contributes nothing rather than poisoning the result. The two disagree on exactly one case — `merge` leaves `(Unknown, None)` at `Unknown` — and that case is why the observation step and the fixpoint cannot share one operation.
impl Lattice for Knowledge {
    fn bottom() -> Self {
        Knowledge::Unknown
    }

    fn join(&mut self, incoming: Knowledge) {
        *self = match (std::mem::replace(self, Knowledge::Unknown), incoming) {
            (Knowledge::Conflict, _) | (_, Knowledge::Conflict) => Knowledge::Conflict,
            (Knowledge::Unknown, other) | (other, Knowledge::Unknown) => other,
            (Knowledge::Known(current), Knowledge::Known(incoming)) => {
                if current == incoming {
                    Knowledge::Known(current)
                } else {
                    Knowledge::Conflict
                }
            }
        }
    }
}
/// Compute the parameters of recursive SCC members that are provably a single literal or function reference at every entry, so they can be substituted in place and dropped as dead. This is a monotone constant-propagation fixpoint over the whole known-callee call graph, restricted to literal/function atoms with parameter forwarding, ordered `Unknown < Known < Conflict`.
///
/// Only members of an eligible SCC participate: the SCC must be recursive and must contain no escaping member and not the program entry, because an escaping or host-called function receives arguments this analysis cannot observe. `known_literals` seeds resolution of caller values already known to be constant.
///
/// A function reference counts only where the member's body may name it (`CallAnalysis::lexical_scope`); one from outside that scope leaves the parameter unknown rather than forwarding a call the body cannot legally make. That scope is the walk the verifier performs, so a reference bound earlier in an enclosing body chain counts as much as one in the member's own group — the site passing the reference and the body receiving it sit at different points of that chain, and only the receiving body's point decides.
pub(crate) fn scc_invariant_knowns(
    module: &Module,
    analysis: &CallAnalysis,
    known_literals: &BTreeMap<ValueId, Atom>,
) -> BTreeMap<ValueId, Atom> {
    let mut params_of: BTreeMap<FunctionId, Vec<ValueId>> = BTreeMap::new();
    for members in eligible_sccs(module, analysis) {
        for &function in &members {
            params_of.insert(function, module.function(function).unwrap().params.clone());
        }
    }
    if params_of.is_empty() {
        return BTreeMap::new();
    }

    let mut constraints: Vec<(FunctionId, Vec<Atom>)> = Vec::new();
    for (_, node) in module.live_nodes() {
        if let Node::ApplyFun {
            callee: Callee::Known(callee),
            args,
            ..
        } = node
            && params_of.contains_key(callee)
        {
            constraints.push((*callee, args.clone()));
        }
    }

    let class = invariant_fixpoint(&params_of, &constraints, known_literals);

    // A function reference is forwarded into a member only where the member's body may name it. `rewrite_atoms` turns the member's closure call on the parameter into a known call, and a recursive callee is exactly the one the inliner then declines to bring into scope — so a reference from outside the member's lexical scope would stand as an out-of-scope call at the round's close. Out of scope, the closure call stays a closure call.
    let owner_of_param: BTreeMap<ValueId, FunctionId> = params_of
        .iter()
        .flat_map(|(function, params)| params.iter().map(move |param| (*param, *function)))
        .collect();
    useful_knowns(class)
        .into_iter()
        .filter(|(param, atom)| match atom {
            Atom::Fun(function) => analysis
                .lexical_scope
                .get(&owner_of_param[param])
                .is_some_and(|scope| scope.contains(function)),
            Atom::Literal(_) | Atom::Value(_) | Atom::Filler => true,
        })
        .collect()
}
/// The members of every SCC eligible for known-argument analysis: recursive, and containing neither an escaping member nor the program entry, because those receive arguments the analysis cannot observe.
pub(crate) fn eligible_sccs(module: &Module, analysis: &CallAnalysis) -> Vec<Vec<FunctionId>> {
    analysis
        .sccs
        .members
        .iter()
        .filter(|members| {
            let recursive = members.len() > 1
                || members.iter().any(|function| {
                    analysis
                        .call_graph
                        .get(function)
                        .is_some_and(|edges| edges.contains(function))
                });
            let observable = members.iter().all(|function| {
                !analysis.escaping.contains(function) && Some(*function) != module.entry()
            });
            recursive && observable
        })
        .cloned()
        .collect()
}
/// Run the monotone `Unknown < Known < Conflict` join to a fixpoint over the given parameter positions and call constraints.
pub(crate) fn invariant_fixpoint(
    params_of: &BTreeMap<FunctionId, Vec<ValueId>>,
    constraints: &[(FunctionId, Vec<Atom>)],
    known_literals: &BTreeMap<ValueId, Atom>,
) -> BTreeMap<ValueId, Knowledge> {
    Solver::solve(params_of.values().flatten().copied(), |rules| {
        for (callee, args) in constraints {
            let Some(params) = params_of.get(callee) else {
                continue;
            };
            rules.add(move |solver| {
                for (index, arg) in args.iter().enumerate() {
                    let Some(&param) = params.get(index) else {
                        continue;
                    };
                    let incoming = resolve_atom(arg, solver, known_literals);
                    solver.join(param, incoming);
                }
            });
        }
    })
}
/// Extract the parameters resolved to a single literal or function reference.
pub(crate) fn useful_knowns(class: BTreeMap<ValueId, Knowledge>) -> BTreeMap<ValueId, Atom> {
    let mut result = BTreeMap::new();
    for (param, knowledge) in class {
        if let Knowledge::Known(atom @ (Atom::Literal(_) | Atom::Fun(_))) = knowledge {
            result.insert(param, atom);
        }
    }
    result
}
/// The `LetValue`-bound constructions of a variant family whose tag is known: a construction of a row that carries [`Slot::Tag`] at slot zero, with a `Nat` literal standing there. These are the constructor call patterns branch specialization can bake into a callee.
///
/// A family's row alone carries a tag. A product row's first field and a structural tuple's are data, so a literal there selects nothing: a product has one pattern, its construction, which is `split_workers`' to deliver.
pub(crate) fn tagged_row_values(module: &Module) -> TaggedRows {
    let mut result = BTreeMap::new();
    for (_, node) in module.live_nodes() {
        let Node::LetValue {
            result: value,
            value: ValueExpr::Row(row, fields),
            ..
        } = node
        else {
            continue;
        };
        if module.row(*row).slots.first() != Some(&Slot::Tag) {
            continue;
        }
        if let Some(Atom::Literal(Literal::Nat(tag))) = fields.first()
            && let Some(tag) = u32::try_from(tag).ok()
        {
            result.insert(*value, (tag, fields.clone(), *row));
        }
    }
    result
}
/// The literal results of `LetValue` bindings, used to resolve caller values already known to be constant.
pub(crate) fn literal_value_map(module: &Module) -> BTreeMap<ValueId, Atom> {
    let mut literals = BTreeMap::new();
    for (_, node) in module.live_nodes() {
        if let Node::LetValue {
            result,
            value: ValueExpr::Literal(literal),
            ..
        } = node
        {
            literals.insert(*result, Atom::Literal(literal.clone()));
        }
    }
    literals
}
/// The lexical introduction of each function, indexed once for a specialization sweep.
pub(crate) fn function_introductions(module: &Module) -> BTreeMap<FunctionId, NodeId> {
    let mut introductions = BTreeMap::new();
    for (id, node) in module.live_nodes() {
        if let Node::LetFun { functions, .. } = node {
            introductions.extend(functions.iter().map(|function| (*function, id)));
        }
    }
    introductions
}
/// The single `LetFun` node introducing every member, or `None` if the members are split across nodes. The clones are added to this node so they share the members' lexical scope.
pub(crate) fn introducing_letfun(
    module: &Module,
    members: &BTreeSet<FunctionId>,
) -> Option<NodeId> {
    for (id, node) in module.live_nodes() {
        if let Node::LetFun { functions, .. } = node {
            let introduced: BTreeSet<FunctionId> = functions.iter().copied().collect();
            if members.is_subset(&introduced) {
                return Some(id);
            }
        }
    }
    None
}
pub(crate) fn merge_inputs(inputs: &mut [Knowledge], args: Option<&[Atom]>) {
    for (index, input) in inputs.iter_mut().enumerate() {
        input.merge(args.and_then(|args| args.get(index)));
    }
}
pub(crate) fn record_known_literals(
    params: &[ValueId],
    inputs: &[Knowledge],
    known: &mut BTreeMap<ValueId, Atom>,
) {
    for (&param, input) in params.iter().zip(inputs) {
        if let Knowledge::Known(Atom::Literal(literal)) = input {
            known.insert(param, Atom::Literal(literal.clone()));
        }
    }
}

#[cfg(test)]
mod tests;
