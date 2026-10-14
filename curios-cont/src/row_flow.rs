//! Which rows a value may be a struct of, followed forward through every flow — into a callee's parameters, and from its returns to wherever its calls resume.
//!
//! The fact is a may-set. A construction contributes its own row; a number, a word, a sequence, a closure, a literal and null contribute none; and what this analysis cannot see into — a reference read out of a value, an indirect call's result, a cell's or a channel's — contributes every row. A set never says that a value *is* a row, only which rows it could be, so the one question it answers is the one a row test asks when the answer is no: a value no flow builds a row for is not a struct of it. What a test asks of a forced description's answer is the case it exists for — an action that never suspends answers no suspension, and its bind's test folds.
//!
//! A function's returns arrive wherever its calls resume, and a call in tail position resumes wherever its caller's calls do: the targets close over the tail calls once, before the rounds, since inlining a call changes the graph only between passes. The boundaries are seeded with every row each round, as `origins` seeds them opaque: an escaping function's parameters and the entry's are its callers', which this module does not see.

use {
    super::{
        Atom, Callee, ContinuationId, FunctionId, Lattice, Module, Node, RowId, Solver, ValueExpr,
        ValueId, analyze_calls,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// The rows a value may be a struct of: these, or any.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Rows {
    Of(BTreeSet<RowId>),
    Any,
}

impl Rows {
    /// Whether some flow reaching the value may build `row`.
    pub(crate) fn may_be(&self, row: RowId) -> bool {
        match self {
            Rows::Of(rows) => rows.contains(&row),
            Rows::Any => true,
        }
    }

    fn none() -> Self {
        Rows::Of(BTreeSet::new())
    }
}

impl Lattice for Rows {
    fn bottom() -> Self {
        Rows::none()
    }

    fn join(&mut self, incoming: Self) {
        *self = match (std::mem::replace(self, Rows::Any), incoming) {
            (Rows::Any, _) | (_, Rows::Any) => Rows::Any,
            (Rows::Of(mut left), Rows::Of(right)) => {
                left.extend(right);
                Rows::Of(left)
            }
        };
    }
}

/// Where each function's returns arrive: the continuations its calls resume, and, through a call in tail position, the ones its caller's returns arrive at.
fn resume_targets(
    module: &Module,
    returns: &BTreeMap<ContinuationId, FunctionId>,
) -> BTreeMap<FunctionId, BTreeSet<ContinuationId>> {
    let mut targets = BTreeMap::<FunctionId, BTreeSet<ContinuationId>>::new();
    let mut tails = BTreeMap::<FunctionId, BTreeSet<FunctionId>>::new();
    for (_, node) in module.live_nodes() {
        let Node::ApplyFun {
            callee: Callee::Known(callee),
            return_to,
            ..
        } = node
        else {
            continue;
        };
        match returns.get(return_to) {
            Some(&caller) => {
                tails.entry(*callee).or_default().insert(caller);
            }
            None => {
                targets.entry(*callee).or_default().insert(*return_to);
            }
        }
    }

    // A tail call's targets are its caller's, which may come from a tail call in turn, so the sets grow to a fixpoint.
    let mut changed = true;
    while changed {
        changed = false;
        for (callee, callers) in &tails {
            let inherited = callers
                .iter()
                .filter_map(|caller| targets.get(caller))
                .flatten()
                .copied()
                .collect::<Vec<_>>();
            let own = targets.entry(*callee).or_default();
            for target in inherited {
                changed |= own.insert(target);
            }
        }
    }

    targets
}

/// The rows each value may be a struct of, to their least fixpoint.
pub(crate) fn rows_reaching(module: &Module) -> BTreeMap<ValueId, Rows> {
    let escaping = analyze_calls(module).escaping;
    let returns = module
        .live_functions()
        .map(|(function, definition)| (definition.return_cont, function))
        .collect::<BTreeMap<_, _>>();
    let targets = resume_targets(module, &returns);
    let seeds = module.value_ids().collect::<Vec<_>>();

    // What arrives at `target`, one fact a parameter: its own parameters where it is defined, and where it is a function's return, those of every continuation its calls resume.
    let deliver =
        |solver: &mut Solver<Rows>, target: ContinuationId, facts: &dyn Fn(usize) -> Rows| {
            let receivers = match returns.get(&target) {
                Some(function) => targets
                    .get(function)
                    .into_iter()
                    .flatten()
                    .copied()
                    .collect(),
                None => vec![target],
            };
            for receiver in receivers {
                if let Some(continuation) = module.continuation(receiver) {
                    for (index, param) in continuation.params.iter().enumerate() {
                        solver.join(*param, facts(index));
                    }
                }
            }
        };

    Solver::solve(seeds, |solver| {
        let fact = |solver: &Solver<Rows>, atom: &Atom| match atom {
            Atom::Value(value) => solver.facts().get(value).cloned().unwrap_or(Rows::Any),
            Atom::Fun(_) | Atom::Literal(_) | Atom::Filler => Rows::none(),
        };

        for (function, definition) in module.live_functions() {
            if escaping.contains(&function) || module.entry() == Some(function) {
                for param in &definition.params {
                    solver.join(*param, Rows::Any);
                }
            }
        }

        for (_, node) in module.live_nodes() {
            match node {
                Node::LetValue { result, value, .. } => {
                    let rows = match value {
                        ValueExpr::Row(row, _) => Rows::Of(BTreeSet::from([*row])),
                        ValueExpr::Tuple(_) | ValueExpr::List(_) | ValueExpr::Literal(_) => {
                            Rows::none()
                        }
                    };
                    solver.join(*result, rows);
                }
                Node::LetIntrinsic { result, op, .. } => {
                    let rows = match op.reads_reference() {
                        true => Rows::Any,
                        false => Rows::none(),
                    };
                    solver.join(*result, rows);
                }
                Node::ApplyFun {
                    callee,
                    args,
                    return_to,
                } => match callee {
                    // A known callee's parameters take what its call passes; its returns reach `return_to` through its own return edges.
                    Callee::Known(callee) => {
                        if let Some(function) = module.function(*callee) {
                            let passed = args
                                .iter()
                                .map(|atom| fact(solver, atom))
                                .collect::<Vec<_>>();
                            for (rows, param) in passed.into_iter().zip(&function.params) {
                                solver.join(*param, rows);
                            }
                        }
                    }
                    Callee::Closure(_) => deliver(solver, *return_to, &|_| Rows::Any),
                },
                Node::ApplyCont(edge) => {
                    let passed = edge
                        .args
                        .iter()
                        .map(|atom| fact(solver, atom))
                        .collect::<Vec<_>>();
                    deliver(solver, edge.target, &|index| {
                        passed.get(index).cloned().unwrap_or(Rows::Any)
                    });
                }
                Node::Switch { cases, default, .. } => {
                    for edge in cases.values().chain(default.as_ref()) {
                        let passed = edge
                            .args
                            .iter()
                            .map(|atom| fact(solver, atom))
                            .collect::<Vec<_>>();
                        deliver(solver, edge.target, &|index| {
                            passed.get(index).cloned().unwrap_or(Rows::Any)
                        });
                    }
                }
                // A host's reply is its wire values, and a mapped list is a list; a cell or a channel holds whatever was put in it.
                Node::Foreign { return_to, .. } | Node::Intrinsic { return_to, .. } => {
                    deliver(solver, *return_to, &|_| Rows::none())
                }
                Node::Cell { return_to, .. } | Node::Channel { return_to, .. } => {
                    deliver(solver, *return_to, &|_| Rows::Any)
                }
                Node::LetFun { .. }
                | Node::LetCont { .. }
                | Node::Halt { .. }
                | Node::Panic(_)
                | Node::Unreachable => {}
            }
        }
    })
}

#[cfg(test)]
mod tests;
