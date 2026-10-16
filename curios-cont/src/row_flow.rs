//! Which rows a value may be a struct of, followed forward through every flow — into a callee's parameters, and from its returns to wherever its calls resume.
//!
//! The fact is a may-set. A construction contributes its own row; a number, a word, a sequence, a closure, a literal and null contribute none; and what this analysis cannot see into — a reference read out of a value, an indirect call's result, a cell's or a channel's — contributes every row. A set never says that a value *is* a row, only which rows it could be, so the one question it answers is the one a row test asks when the answer is no: a value no flow builds a row for is not a struct of it. What a test asks of a forced description's answer is the case it exists for — an action that never suspends answers no suspension, and its bind's test folds.
//!
//! Each function-return position has its own fact, even where no caller receives it. Known calls deliver those facts to their resumes, including a caller's own return positions for tail calls, so recursion and ordinary value flow settle together. The boundaries are seeded with every row each round, as `origins` seeds them opaque: an escaping function's parameters and the entry's are its callers', which this module does not see. Only facts that reach the entry's returns determine its suspension capability; an internally consumed suspension does not escape merely because it was constructed.

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

/// Function results bind no value; their positions have identities distinct from all ordinary values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum RowKey {
    Value(ValueId),
    Returned(FunctionId, usize),
}

impl Module {
    /// The registered suspension row when an entry result may carry it; `None` when return flow excludes it, or this module has no entry or suspension row.
    ///
    /// This reads the current graph without caching or changing its row vocabulary. Unknown results retain the row; a computation that never returns contributes no returned row and need not be proved terminating.
    pub fn entry_suspension(&self) -> Option<RowId> {
        let entry = self.entry()?;
        let row = self.suspension()?;
        let facts = row_facts(self);
        let mut returned = facts
            .iter()
            .filter_map(|(key, rows)| match key {
                RowKey::Returned(function, _) if *function == entry => Some(rows),
                _ => None,
            })
            .peekable();
        match returned.peek().is_none() || returned.any(|rows| rows.may_be(row)) {
            true => Some(row),
            false => None,
        }
    }
}

/// The rows each ordinary value may be a struct of, including facts propagated through function results.
pub(crate) fn rows_reaching(module: &Module) -> BTreeMap<ValueId, Rows> {
    row_facts(module)
        .into_iter()
        .filter_map(|(key, rows)| match key {
            RowKey::Value(value) => Some((value, rows)),
            RowKey::Returned(_, _) => None,
        })
        .collect()
}

/// Value and return-position facts solved together, so a recursive call reads the same least fixpoint as a continuation transfer.
fn row_facts(module: &Module) -> BTreeMap<RowKey, Rows> {
    let escaping = analyze_calls(module).escaping;
    let arities = module.return_arities();
    let receivers = module
        .live_continuations()
        .map(|(id, definition)| {
            (
                id,
                definition
                    .params
                    .iter()
                    .copied()
                    .map(RowKey::Value)
                    .collect::<Vec<_>>(),
            )
        })
        .chain(module.live_functions().map(|(id, definition)| {
            (
                definition.return_cont,
                (0..arities[&id])
                    .map(|index| RowKey::Returned(id, index))
                    .collect(),
            )
        }))
        .collect::<BTreeMap<_, _>>();
    let seeds = receivers
        .values()
        .flatten()
        .copied()
        .chain(module.value_ids().map(RowKey::Value));

    let deliver = |solver: &mut Solver<Rows, RowKey>,
                   target: ContinuationId,
                   facts: &dyn Fn(usize) -> Rows| {
        if let Some(keys) = receivers.get(&target) {
            for (index, key) in keys.iter().enumerate() {
                solver.join(*key, facts(index));
            }
        }
    };

    Solver::solve(seeds, |solver| {
        let fact = |solver: &Solver<Rows, RowKey>, atom: &Atom| match atom {
            Atom::Value(value) => solver
                .facts()
                .get(&RowKey::Value(*value))
                .cloned()
                .unwrap_or(Rows::Any),
            Atom::Fun(_) | Atom::Literal(_) | Atom::Filler => Rows::none(),
        };

        for (function, definition) in module.live_functions() {
            if escaping.contains(&function) || module.entry() == Some(function) {
                for param in &definition.params {
                    solver.join(RowKey::Value(*param), Rows::Any);
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
                    solver.join(RowKey::Value(*result), rows);
                }
                Node::LetIntrinsic { result, op, .. } => {
                    let rows = match op.reads_reference() {
                        true => Rows::Any,
                        false => Rows::none(),
                    };
                    solver.join(RowKey::Value(*result), rows);
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
                                solver.join(RowKey::Value(*param), rows);
                            }
                            let returned = (0..arities[callee])
                                .map(|index| {
                                    solver
                                        .facts()
                                        .get(&RowKey::Returned(*callee, index))
                                        .cloned()
                                        .unwrap_or(Rows::Any)
                                })
                                .collect::<Vec<_>>();
                            deliver(solver, *return_to, &|index| {
                                returned.get(index).cloned().unwrap_or(Rows::Any)
                            });
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

#[cfg(test)]
mod entry_tests;
