//! Where a value comes from, followed forward: whether every flow reaching it is an exact tuple construction of one arity, or something the rewrite cannot see through.
//!
//! The demand lattice is the backward half — how a received value is *used*. This is the forward half the value-lifetime decision separates from it: whether a parameter's every incoming edge carries a visible construction or an alias of one, which is what makes a merged flow *exclusive* and a scalar-replacement rewrite able to say what fields exist on every path. The two halves have separate tests and separate lattices, even though the eventual rewrite consumes them together.
//!
//! A region is entered by constructions but circulates through aliases: a loop arm that passes the loop's own parameter back unchanged contributes that parameter's own fact, which the fixpoint resolves to the constructions that entered it — so an every-edge-constructs reading is not expressible here by design, because it would decline exactly the loops the specification exists for.
//!
//! Boundaries are stated by injection rather than by omission: a resume parameter receives whatever an unsplit return interface delivers, an escaping function's parameters receive whatever unknown callers pass, the entry's parameters belong to the host, and a knot-tied value is a closure — each is seeded `Opaque` so a bottom that survives the round means *unreached*, never *assumed exact*.

use {
    super::{
        CpsAtom, CpsCallee, CpsEdge, CpsModule, CpsNode, CpsValueExpr, CpsValueId, Lattice, Solver,
        analysis::analyze_calls,
    },
    std::collections::BTreeMap,
};

/// What flows into a value, ordered `Unreached < Exact(_) < Opaque`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Origin {
    /// No flow reached it in the round — an unentered parameter or an unreachable binding.
    Unreached,
    /// Every flow reaching it is a tuple construction of this arity, or an alias of one.
    Exact(usize),
    /// Some flow is not a visible construction — a call result, a literal, a closure, a projection — or two arities merged, which no single shape describes.
    Opaque,
}

impl Lattice for Origin {
    fn bottom() -> Self {
        Origin::Unreached
    }

    fn join(&mut self, incoming: Self) {
        *self = match (*self, incoming) {
            (Origin::Opaque, _) | (_, Origin::Opaque) => Origin::Opaque,
            (Origin::Unreached, other) | (other, Origin::Unreached) => other,
            (Origin::Exact(left), Origin::Exact(right)) if left == right => Origin::Exact(left),
            (Origin::Exact(_), Origin::Exact(_)) => Origin::Opaque,
        }
    }
}

/// Join `atom`'s fact into `target`: a value hands its own fact forward, and anything that is not a value is not a construction.
fn flow(solver: &mut Solver<Origin>, atom: &CpsAtom, target: CpsValueId) {
    let incoming = match atom {
        CpsAtom::Value(value) => solver.facts().get(value).copied().unwrap_or(Origin::Opaque),
        CpsAtom::Fun(_) | CpsAtom::Literal(_) => Origin::Opaque,
    };
    solver.join(target, incoming);
}

/// Push an edge's arguments into its target's parameters. A target without a definition is a return sentinel; its values are delivered through the return interface, whose resume parameters are already seeded opaque.
fn flow_edge(module: &CpsModule, solver: &mut Solver<Origin>, edge: &CpsEdge) {
    let Some(continuation) = module.continuation(edge.target) else {
        return;
    };
    for (atom, param) in edge.args.iter().zip(continuation.params.iter().copied()) {
        flow(solver, atom, param);
    }
}

/// What flows into every value, to its least fixpoint.
pub(crate) fn origins(module: &CpsModule) -> BTreeMap<CpsValueId, Origin> {
    let escaping = analyze_calls(module).escaping;
    let seeds = module.values.live_ids().collect::<Vec<_>>();

    Solver::solve(seeds, |solver| {
        // The injected boundaries, restated every round so a later join cannot narrow them.
        for (function, definition) in module.functions.iter_live() {
            if escaping.contains(&function) || module.entry() == Some(function) {
                for param in &definition.params {
                    solver.join(*param, Origin::Opaque);
                }
            }
        }

        for (_, node) in module.nodes.iter_live() {
            match node {
                CpsNode::LetValue { result, value, .. } => {
                    let origin = match value {
                        CpsValueExpr::Tuple(atoms) => Origin::Exact(atoms.len()),
                        CpsValueExpr::List(_) | CpsValueExpr::Literal(_) => Origin::Opaque,
                    };
                    solver.join(*result, origin);
                }
                CpsNode::LetIntrinsic { result, .. } => {
                    solver.join(*result, Origin::Opaque);
                }
                CpsNode::ApplyFun {
                    callee,
                    args,
                    return_to,
                } => {
                    if let Some(resume) = module.continuation(*return_to) {
                        for param in &resume.params {
                            solver.join(*param, Origin::Opaque);
                        }
                    }
                    if let CpsCallee::Known(callee) = callee
                        && let Some(function) = module.function(*callee)
                    {
                        for (atom, param) in args.iter().zip(function.params.iter().copied()) {
                            flow(solver, atom, param);
                        }
                    }
                }
                CpsNode::ApplyCont(edge) => flow_edge(module, solver, edge),
                CpsNode::Switch { cases, default, .. } => {
                    for edge in cases.values().chain(default.as_ref()) {
                        flow_edge(module, solver, edge);
                    }
                }
                CpsNode::Foreign { return_to, .. }
                | CpsNode::Cell { return_to, .. }
                | CpsNode::Intrinsic { return_to, .. } => {
                    if let Some(resume) = module.continuation(*return_to) {
                        for param in &resume.params {
                            solver.join(*param, Origin::Opaque);
                        }
                    }
                }
                CpsNode::RecInit { values, .. } => {
                    for value in values {
                        solver.join(*value, Origin::Opaque);
                    }
                }
                CpsNode::LetFun { .. }
                | CpsNode::LetCont { .. }
                | CpsNode::Exit { .. }
                | CpsNode::Unreachable => {}
            }
        }
    })
}

#[cfg(test)]
mod tests;
