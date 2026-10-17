//! The monoid worker/wrapper: a recursion whose answer is its own call's answer combined under a monoid becomes a loop over an accumulator — tail recursion modulo a monoid context, as Leijen and Lorenzen derive it (*Tail Recursion Modulo Context*, POPL 2023, §4.4).
//!
//! A recursion whose result feeds a monoid combine (`count(t) + 1`, `x == y && eql(xs, ys)`) is no tail call: the combine runs after the recursion returns, so every level keeps a frame and pays a return. But `w(x, acc)`, with the combine's addend folded into `acc` ahead of the call, is a loop:
//!
//! ```text
//!   rec f(x) = … f(t) ⊕ k …
//!   ⟶  f(x)      = w(x, e)            (the thin wrapper, e the identity)
//!      rec w(x, acc) = … w(t, acc ⊕ k) …   (base arms return acc ⊕ v)
//! ```
//!
//! A cost transform, not a correctness one: a module runs on a stack of its own, which holds a recursion left as written at any depth a program meets ([`curios-runtime`'s README](../../../curios-runtime/README.md#a-module-runs-on-a-stack-of-its-own)), and what the rewrite removes is a frame and a return an element, and the stack the depth touches. It is sound-but-incomplete: outside the recognized envelope it is a no-op, never a miscompile.
//!
//! **The envelope, structural in ANF.** A leaf tail block ends in a self-call followed by a chain of combines of one monoid — each reading the answer of the one before it, the call's first, exactly once, every other operand a value from before the call — and returns the chain's last answer; or it ends in a bare tail self-call; or it is a base. Only the combines move across the call, so nothing observable is reordered: Koka's liberal context, whose operands after the hole are values. Every combine leaf shares one monoid, and every self-reference in the function's whole region stands on the spine.
//!
//! **Which operation is a monoid is the oracle's** ([`Semantics::monoid_identity`]), and every one commutes, so the addends of both sides of a recursion fold into one accumulator. The licence is associativity and an erasure-stable identity, because nothing here is partial. The rewrite reverses the order the addends are combined in — the written recursion folds them innermost-out, `((v ⊕ kₙ) ⊕ …) ⊕ k₁`, and the worker threads them `k₁` first — so the *partial* results differ even though the total does not. A bounded carrier would need a third condition, monotone definedness, since a reversed order could refuse where the written one computes — a multiplication would turn `((1 * 0) * 2¹⁶) * 2¹⁶` into a trap. `Nat` and `Int` are unbounded at run time ([Nat and Int are an i31 until they outgrow it](../../../documentation/design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md)) and the `Bool` operations total, so every partial of every order is defined, and a reversed order costs at most the size of what its partials build.

#[cfg(test)]
mod tests;

use {
    crate::{
        Atom, Block, BlockId, Constant, Function, FunctionId, Module, Operation, Rhs, Semantics,
        Statement, StatementId, Terminator, control_blocks,
    },
    std::collections::BTreeSet,
};

/// The monoid a recursion's combines share: a scalar operation the oracle names a monoid, with its identity.
#[derive(Clone, PartialEq, Eq)]
struct Row {
    operation: Operation,
    identity: Constant,
}

impl Row {
    fn of_operation(operation: Operation) -> Option<Row> {
        let identity = Semantics::monoid_identity(operation)?;
        Some(Row {
            operation,
            identity,
        })
    }

    /// The monoid a right-hand side combines in, and its two operands: a direct operation, or a two-argument call that resolves through *forwarder* functions to one. The corpus combines through concept witnesses and `/sys` (`a + b` is `Add/add(a, b)`, a forwarder onto `Nat/add`, a forwarder onto the intrinsic; `a && b` is `/sys/Bool/and`), so the resolver is what makes the envelope reach idiomatic code at all; unfolding a pure forwarder chain preserves meaning exactly.
    fn of_rhs(module: &Module, rhs: &Rhs) -> Option<(Row, [Atom; 2])> {
        match rhs {
            Rhs::Operation {
                operation,
                operands,
            } => {
                let row = Self::of_operation(*operation)?;
                let [left, right] = operands.as_slice() else {
                    return None;
                };
                Some((row, [*left, *right]))
            }
            Rhs::Apply {
                callee: Atom::Function(callee),
                arguments,
            } => {
                let [left, right] = arguments.as_slice() else {
                    return None;
                };
                let row = Self::of_operation(resolve_forwarder(module, *callee, 8)?)?;
                Some((row, [*left, *right]))
            }
            _ => None,
        }
    }

    /// `left ⊕ right`.
    fn build(&self, left: Atom, right: Atom) -> Rhs {
        Rhs::Operation {
            operation: self.operation,
            operands: vec![left, right],
        }
    }

    /// Whether `atom` is this monoid's identity, which a base arm's combine may drop.
    fn is_identity(&self, module: &Module, atom: Atom) -> bool {
        match atom {
            Atom::Constant(constant) => module.constant(constant) == Some(&self.identity),
            _ => false,
        }
    }
}

/// The scalar operation a binary forwarder chain bottoms out in: a function whose body is exactly one statement over its two parameters in order — the operation itself, or a saturated call to the next forwarder.
fn resolve_forwarder(module: &Module, function: FunctionId, budget: usize) -> Option<Operation> {
    if budget == 0 {
        return None;
    }
    let definition = module.function(function)?;
    let [p0, p1] = definition.params.as_slice() else {
        return None;
    };
    let block = module.block(definition.body)?;
    let [statement] = block.statements.as_slice() else {
        return None;
    };
    let Some(Statement::Let { result, rhs }) = module.statement(*statement) else {
        return None;
    };
    if !matches!(block.terminator, Terminator::Return(atom) if atom == Atom::Value(*result)) {
        return None;
    }
    let forwards = |operands: &[Atom]| operands == [Atom::Value(*p0), Atom::Value(*p1)];
    match rhs {
        Rhs::Operation {
            operation,
            operands,
        } if forwards(operands) => Some(*operation),
        Rhs::Apply {
            callee: Atom::Function(next),
            arguments,
        } if forwards(arguments) => resolve_forwarder(module, *next, budget - 1),
        _ => None,
    }
}

/// How one leaf tail block ends.
enum Leaf {
    /// `…, a = f(args), b₁ = ⊕(a, k₁), …, bₙ = ⊕(bₙ₋₁, kₙ); return bₙ` — a self-call closed by a chain of combines, whose addends are values from before the call.
    Combine {
        block: BlockId,
        call: StatementId,
        addends: Vec<Atom>,
    },
    /// `a = f(args); return a`.
    Bare { call: StatementId },
    /// Any other returning leaf: combine the accumulator with its result.
    Base { block: BlockId },
}

/// Rebase every recognized function, module-wide.
pub(super) fn rebase_monoid_recursion(module: &mut Module) {
    curios_profile::profile!("rebase_monoid_recursion");
    // Targets: self-recursive functions bound by a `Functions` statement (anywhere — item or block), recognized one at a time. Collect first; rewriting appends statements and functions but never disturbs another target's blocks.
    let targets: Vec<(StatementId, FunctionId)> = module
        .statements()
        .iter()
        .enumerate()
        .filter_map(|(index, slot)| match slot {
            Some(Statement::Functions { functions }) => match functions.as_slice() {
                [function] => Some((StatementId(index as u32), *function)),
                _ => None,
            },
            _ => None,
        })
        .collect();

    for (binding, function) in targets {
        rebase_one(module, binding, function);
    }
}

fn rebase_one(module: &mut Module, binding: StatementId, function: FunctionId) {
    let Some(definition) = module.function(function) else {
        return;
    };
    let body = definition.body;
    let params = definition.params.clone();
    let debug_name = definition.debug_name.clone();

    // Recognize the tail spine.
    let Some((row, leaves)) = recognize(module, function, body) else {
        return;
    };

    // The wrapper keeps the function's identity: fresh parameters, a new body that seeds the worker with the monoid identity. The worker takes the original parameters plus the accumulator and the original body.
    let worker = module.reserve_function();
    let acc = module.add_value(Some("acc".into()));
    let identity = module.intern_constant(row.identity.clone());

    let wrapper_params: Vec<_> = params
        .iter()
        .map(|&param| {
            let name = module
                .value(param)
                .and_then(|value| value.debug_name.clone());
            module.add_value(name)
        })
        .collect();
    let call_result = module.add_value(None);
    let call = module.add_statement(Statement::Let {
        result: call_result,
        rhs: Rhs::Apply {
            callee: Atom::Function(worker),
            arguments: wrapper_params
                .iter()
                .map(|&param| Atom::Value(param))
                .chain([Atom::Constant(identity)])
                .collect(),
        },
    });
    let wrapper_body = module.add_block(Block {
        statements: vec![call],
        terminator: Terminator::Return(Atom::Value(call_result)),
    });

    let mut worker_params = params;
    worker_params.push(acc);
    let worker_name = debug_name.as_ref().map(|name| format!("{name}@w"));
    // A description is a zero-parameter thunk and never reaches the accumulator split, so both halves are ordinary computations. The termination verdict is the split definition's and is carried to both halves: re-basing a recursion moves where the accumulator lives, never whether the recursion descends.
    let total = module
        .function(function)
        .is_some_and(|function| function.total);
    module.define_function(
        worker,
        Function {
            debug_name: worker_name,
            params: worker_params,
            body,
            description: false,
            total,
        },
    );
    module.set_function(
        function,
        Function {
            debug_name,
            params: wrapper_params,
            body: wrapper_body,
            description: false,
            total,
        },
    );

    // Rewrite the leaf tail blocks of the (now worker-owned) body.
    let acc_atom = Atom::Value(acc);
    for leaf in leaves {
        match leaf {
            Leaf::Combine {
                block,
                call,
                addends,
            } => {
                let Some(Statement::Let {
                    result: call_result,
                    rhs: Rhs::Apply { arguments, .. },
                }) = module.statement(call).cloned()
                else {
                    continue;
                };
                let Some(position) = module
                    .block(block)
                    .and_then(|definition| definition.statements.iter().position(|&s| s == call))
                else {
                    continue;
                };
                // The addends fold into the accumulator ahead of the call, which becomes a tail call to the worker threading it; the chain's own statements are left in no block, and the prune after the rebase drops them.
                let mut statements = module
                    .block(block)
                    .map(|definition| definition.statements[..position].to_vec())
                    .unwrap_or_default();
                let mut partial = acc_atom;
                for addend in addends {
                    let folded = module.add_value(None);
                    statements.push(module.add_statement(Statement::Let {
                        result: folded,
                        rhs: row.build(partial, addend),
                    }));
                    partial = Atom::Value(folded);
                }
                let mut arguments = arguments;
                arguments.push(partial);
                module.set_statement(
                    call,
                    Statement::Let {
                        result: call_result,
                        rhs: Rhs::Apply {
                            callee: Atom::Function(worker),
                            arguments,
                        },
                    },
                );
                statements.push(call);
                module.set_block_statements(block, statements);
                module.set_block_terminator(block, Terminator::Return(Atom::Value(call_result)));
            }
            Leaf::Bare { call } => {
                let Some(Statement::Let {
                    result,
                    rhs: Rhs::Apply { arguments, .. },
                }) = module.statement(call).cloned()
                else {
                    continue;
                };
                let mut arguments = arguments;
                arguments.push(acc_atom);
                module.set_statement(
                    call,
                    Statement::Let {
                        result,
                        rhs: Rhs::Apply {
                            callee: Atom::Function(worker),
                            arguments,
                        },
                    },
                );
            }
            Leaf::Base { block } => {
                let Some(returned) =
                    module
                        .block(block)
                        .and_then(|definition| match definition.terminator {
                            Terminator::Return(returned) => Some(returned),
                            _ => None,
                        })
                else {
                    continue;
                };
                if row.is_identity(module, returned) {
                    module.set_block_terminator(block, Terminator::Return(acc_atom));
                    continue;
                }
                let Some(mut statements) = module
                    .block(block)
                    .map(|definition| definition.statements.clone())
                else {
                    continue;
                };
                let combined = module.add_value(None);
                statements.push(module.add_statement(Statement::Let {
                    result: combined,
                    rhs: row.build(acc_atom, returned),
                }));
                module.set_block_statements(block, statements);
                module.set_block_terminator(block, Terminator::Return(Atom::Value(combined)));
            }
        }
    }

    // Bind the worker beside its wrapper.
    module.set_statement(
        binding,
        Statement::Functions {
            functions: vec![function, worker],
        },
    );
}

/// Recognize a function's tail spine: one uniform monoid, at least one combine leaf, and every self-call in the function's whole region accounted for on the spine.
fn recognize(module: &Module, function: FunctionId, body: BlockId) -> Option<(Row, Vec<Leaf>)> {
    let mut leaves = Vec::new();
    let mut row: Option<Row> = None;
    let mut combines = 0usize;
    let mut spine_calls = 0usize;

    // Walk the tail chain: a block whose return is the result of a trailing dispatch recurses into its arms; anything else is a leaf.
    let mut work = vec![body];
    let mut seen = BTreeSet::new();
    while let Some(block) = work.pop() {
        if !seen.insert(block) {
            continue;
        }
        let definition = module.block(block)?;
        let Terminator::Return(returned) = definition.terminator else {
            // An exiting or unreachable leaf carries no result to combine.
            continue;
        };

        let last = definition
            .statements
            .last()
            .and_then(|&statement| module.statement(statement).map(|s| (statement, s)));

        // A trailing dispatch whose result the block returns: its arms are tail positions.
        //
        // **This list is a pattern, so nothing checks it is complete.** A dispatch form missing from it is not walked into, so the recursion under it keeps its deferred context, stays non-tail, and costs a frame an element where a loop costs none. `FoldSequence` is absent because its step is not a tail position — the accumulator is consumed after it. A new dispatch form belongs here unless it is a loop.
        if let Some((
            _,
            Statement::Let {
                result,
                rhs:
                    rhs @ (Rhs::SwitchBool { .. }
                    | Rhs::SwitchNat { .. }
                    | Rhs::MatchVariant { .. }
                    | Rhs::UnconsSequence { .. }),
            },
        )) = last
            && returned == Atom::Value(*result)
        {
            work.extend(rhs.sub_blocks());
            continue;
        }

        // Leaf: classify its ending.
        match ending(module, function, definition, returned) {
            Ending::Combine {
                call,
                row: found,
                addends,
            } => {
                match &row {
                    None => row = Some(found),
                    Some(previous) if *previous == found => {}
                    Some(_) => return None,
                }
                combines += 1;
                spine_calls += 1;
                leaves.push(Leaf::Combine {
                    block,
                    call,
                    addends,
                });
            }
            Ending::Bare { call } => {
                spine_calls += 1;
                leaves.push(Leaf::Bare { call });
            }
            Ending::Base => leaves.push(Leaf::Base { block }),
        }
    }

    if combines == 0 {
        return None;
    }
    let row = row?;

    // Every self-reference in the function's whole region must be one of the spine calls: a self-call off the spine (or the function escaping as a value from its own body) declines the transform.
    if count_self_references(module, function) != spine_calls {
        return None;
    }

    Some((row, leaves))
}

/// How a leaf block that returns `returned` ends.
enum Ending {
    Combine {
        call: StatementId,
        row: Row,
        addends: Vec<Atom>,
    },
    Bare {
        call: StatementId,
    },
    Base,
}

/// Read a leaf's ending: its last self-call, and every statement after it a combine of one monoid that reads the answer before it exactly once and nothing else the call or the chain computed. Anything else is a base, and a self-call it holds is then off the spine, which declines the whole function.
fn ending(module: &Module, function: FunctionId, block: &Block, returned: Atom) -> Ending {
    let is_self_call = |&statement: &StatementId| {
        matches!(
            module.statement(statement),
            Some(Statement::Let {
                rhs: Rhs::Apply {
                    callee: Atom::Function(callee),
                    ..
                },
                ..
            }) if *callee == function
        )
    };
    let Some(position) = block.statements.iter().rposition(is_self_call) else {
        return Ending::Base;
    };
    let call = block.statements[position];
    let Some(Statement::Let { result, .. }) = module.statement(call) else {
        return Ending::Base;
    };

    // The answer each link of the chain reads, and every answer the call and the chain computed, none of which a link may read but the one it is handed.
    let mut hole = Atom::Value(*result);
    let mut computed = vec![hole];
    let mut row: Option<Row> = None;
    let mut addends = Vec::new();
    for statement in &block.statements[position + 1..] {
        let Some(Statement::Let { result, rhs }) = module.statement(*statement) else {
            return Ending::Base;
        };
        let Some((found, [left, right])) = Row::of_rhs(module, rhs) else {
            return Ending::Base;
        };
        if row.as_ref().is_some_and(|row| *row != found) {
            return Ending::Base;
        }
        let addend = match (left == hole, right == hole) {
            (true, false) => right,
            (false, true) => left,
            _ => return Ending::Base,
        };
        if computed.contains(&addend) {
            return Ending::Base;
        }
        row = Some(found);
        addends.push(addend);
        hole = Atom::Value(*result);
        computed.push(hole);
    }
    if returned != hole {
        return Ending::Base;
    }
    match row {
        Some(row) => Ending::Combine { call, row, addends },
        None => Ending::Bare { call },
    }
}

/// Count every reference to `function` inside its own region, including nested function bodies.
fn count_self_references(module: &Module, function: FunctionId) -> usize {
    let mut count = 0usize;
    let mut functions = vec![function];
    let mut seen = BTreeSet::new();
    while let Some(current) = functions.pop() {
        if !seen.insert(current) {
            continue;
        }
        let Some(definition) = module.function(current) else {
            continue;
        };
        for block in control_blocks(module, definition.body) {
            let Some(block) = module.block(block) else {
                continue;
            };
            for &statement in &block.statements {
                match module.statement(statement) {
                    Some(Statement::Let { rhs, .. }) => {
                        for atom in rhs.operands() {
                            if atom == Atom::Function(function) {
                                count += 1;
                            }
                        }
                    }
                    Some(Statement::Functions {
                        functions: nested, ..
                    }) => functions.extend(nested),
                    Some(Statement::Rec { group }) => {
                        if let Some(group) = module.rec_group(*group) {
                            functions.extend(&group.functions);
                        }
                    }
                    None => {}
                }
            }
            if block.terminator.atoms().contains(&Atom::Function(function)) {
                count += 1;
            }
        }
    }
    count
}
