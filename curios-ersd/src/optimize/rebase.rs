//! The monoid worker/wrapper: reassociate a monoid-deferred self-recursion into a tail-threaded accumulator.
//!
//! A recursion whose result feeds a monoid combine (`count_w(t) + 1`, `chunk ++ rest(t)`) cannot be a tail call: the combine sits after the recursion, so the runtime grows a native frame per element and overflows on corpus-sized input. But `w(x, acc)` with the combine's addend moved into `acc` *is* a loop:
//!
//! ```text
//!   rec f(x) = … f(t) ⊕ k …
//!   ⟶  f(x)      = w(x, e)            (the thin wrapper, e the identity)
//!      rec w(x, acc) = … w(t, acc ⊕ k) …   (base arms return acc ⊕ v)
//! ```
//!
//! This is a correctness transform, not an optimization — without it the deferred-context corpus overflows the native stack — and it is sound-but-incomplete: outside the recognized envelope it is a no-op, never a miscompile. The envelope, structural in ANF: a leaf tail block ends in exactly `[…, a = f(args), b = ⊕(a, k)]` returning `b` (the addend defined before the call, so moving only the *pure combine* across the recursion reorders nothing observable), a bare tail self-call, or a base; one uniform registered monoid; a non-commutative monoid only with the recursion on the right. Associativity plus an erasure-stable identity is exactly what the reassociation consumes — the only algebraic fact the oracle carries, because this is its only consumer.

#[cfg(test)]
mod tests;

use {
    crate::{
        Atom, Block, BlockId, Constant, Function, FunctionId, Module, Operation, Rhs, Statement,
        StatementId, Terminator, walk::control_blocks,
    },
    std::collections::BTreeSet,
};

/// A monoid registered for accumulator reassociation, keyed in the erased operators. No `And` row: boolean and bitwise `and` share an erased operator with different identities, so no single seed is sound for both. No append rows: `BinAppend`/`ListAppend` append an *element* to a sequence — heterogeneous, so the accumulator rewrite's carriers do not line up (the legacy engine's append rows fired on shapes this corpus does not contain; the gate below is the arbiter if one ever appears).
#[derive(Clone, Copy, PartialEq, Eq)]
enum Monoid {
    NatAdd,
    NatMul,
    NatOr,
    IntAdd,
    IntMul,
    IntOr,
}

impl Monoid {
    fn of_operation(operation: Operation) -> Option<Monoid> {
        Some(match operation {
            Operation::NatAdd => Monoid::NatAdd,
            Operation::NatMul => Monoid::NatMul,
            Operation::NatOr => Monoid::NatOr,
            Operation::IntAdd => Monoid::IntAdd,
            Operation::IntMul => Monoid::IntMul,
            Operation::IntOr => Monoid::IntOr,
            _ => return None,
        })
    }

    /// The monoid a combine right-hand side performs: a direct operation, or a two-argument call that resolves through *forwarder* functions to one. The corpus combines through concept witnesses (`a + b` is `Add/add(a, b)`, a forwarder onto `Nat/add`, a forwarder onto the intrinsic), so the resolver is what makes the envelope reach idiomatic code at all; unfolding a pure forwarder chain preserves meaning exactly.
    fn of_rhs(module: &Module, rhs: &Rhs) -> Option<(Monoid, Atom, Atom)> {
        match rhs {
            Rhs::Operation {
                operation,
                operands,
            } => {
                let monoid = Self::of_operation(*operation)?;
                Some((monoid, operands[0], operands[1]))
            }
            Rhs::Apply {
                callee: Atom::Function(callee),
                arguments,
            } if arguments.len() == 2 => {
                let operation = resolve_forwarder(module, *callee, 8)?;
                let monoid = Self::of_operation(operation)?;
                Some((monoid, arguments[0], arguments[1]))
            }
            _ => None,
        }
    }

    /// The identity element seeded into the accumulator.
    fn identity(self) -> Constant {
        match self {
            Monoid::NatAdd | Monoid::NatOr => Constant::Nat(0),
            Monoid::NatMul => Constant::Nat(1),
            Monoid::IntAdd | Monoid::IntOr => Constant::Int(0),
            Monoid::IntMul => Constant::Int(1),
        }
    }

    /// Every registered monoid commutes. A future non-commutative row (an append over one carrier) must re-introduce the placement rule: the recursion only on the right, the addend folded as `acc ⊕ k`.
    fn commutative(self) -> bool {
        let _ = self;
        true
    }

    /// `left ⊕ right`.
    fn build(self, left: Atom, right: Atom) -> Rhs {
        match self {
            Monoid::NatAdd => operation(Operation::NatAdd, left, right),
            Monoid::NatMul => operation(Operation::NatMul, left, right),
            Monoid::NatOr => operation(Operation::NatOr, left, right),
            Monoid::IntAdd => operation(Operation::IntAdd, left, right),
            Monoid::IntMul => operation(Operation::IntMul, left, right),
            Monoid::IntOr => operation(Operation::IntOr, left, right),
        }
    }
}

fn operation(operation: Operation, left: Atom, right: Atom) -> Rhs {
    Rhs::Operation {
        operation,
        operands: vec![left, right],
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
    if block.terminator.atom() != Some(Atom::Value(*result)) {
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
    /// `a = f(args); b = ⊕(…a…); return b` — the addend on the other side.
    Combine {
        block: BlockId,
        call: StatementId,
        combine: StatementId,
        addend: Atom,
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
    let Some((monoid, leaves)) = recognize(module, function, body) else {
        return;
    };

    // The wrapper keeps the function's identity: fresh parameters, a new body that seeds the worker with the monoid identity. The worker takes the original parameters plus the accumulator and the original body.
    let worker = module.reserve_function();
    let acc = module.add_value(Some("acc".into()));
    let identity = module.intern_constant(monoid.identity());

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
    module.define_function(
        worker,
        Function {
            debug_name: worker_name,
            params: worker_params,
            body,
        },
    );
    module.set_function(
        function,
        Function {
            debug_name,
            params: wrapper_params,
            body: wrapper_body,
        },
    );

    // Rewrite the leaf tail blocks of the (now worker-owned) body.
    let acc_atom = Atom::Value(acc);
    for leaf in leaves {
        match leaf {
            Leaf::Combine {
                block,
                call,
                combine,
                addend,
            } => {
                let Some(Statement::Let {
                    result: call_result,
                    rhs: Rhs::Apply { arguments, .. },
                }) = module.statement(call).cloned()
                else {
                    continue;
                };
                let Some(Statement::Let {
                    result: combine_result,
                    ..
                }) = module.statement(combine).cloned()
                else {
                    continue;
                };
                // The combine now runs first, folding the addend into the accumulator; the call becomes a tail call to the worker threading it, and the block returns the call's result.
                module.set_statement(
                    combine,
                    Statement::Let {
                        result: combine_result,
                        rhs: monoid.build(acc_atom, addend),
                    },
                );
                let mut arguments = arguments;
                arguments.push(Atom::Value(combine_result));
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
                let Some(definition) = module.block(block) else {
                    continue;
                };
                let mut statements = definition.statements.clone();
                // Swap the adjacent pair `[call, combine]` to `[combine, call]`.
                if let Some(position) = statements.iter().position(|&s| s == call) {
                    statements.swap(position, position + 1);
                }
                let terminator = Terminator::Return(Atom::Value(call_result));
                module.set_block_statements(block, statements);
                module.set_block_terminator(block, terminator);
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
                let Some((returned, mut statements)) =
                    module
                        .block(block)
                        .and_then(|definition| match definition.terminator {
                            Terminator::Return(returned) => {
                                Some((returned, definition.statements.clone()))
                            }
                            _ => None,
                        })
                else {
                    continue;
                };
                let combined = module.add_value(None);
                let statement = module.add_statement(Statement::Let {
                    result: combined,
                    rhs: monoid.build(acc_atom, returned),
                });
                statements.push(statement);
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

/// Recognize a function's tail spine: one uniform monoid, at least one combine, every self-call in the function's whole region accounted for on the spine, and the addend defined before the call (structurally: the call and combine are the block's last two statements).
fn recognize(module: &Module, function: FunctionId, body: BlockId) -> Option<(Monoid, Vec<Leaf>)> {
    let mut leaves = Vec::new();
    let mut monoid: Option<Monoid> = None;
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
        if let Some((
            _,
            Statement::Let {
                result,
                rhs:
                    rhs @ (Rhs::SwitchBool { .. } | Rhs::SwitchNat { .. } | Rhs::MatchVariant { .. }),
            },
        )) = last
            && returned == Atom::Value(*result)
        {
            work.extend(rhs.sub_blocks());
            continue;
        }

        // Leaf: classify its ending.
        let statements = &definition.statements;
        let count = statements.len();

        // `…, a = f(args), b = ⊕(a, k); return b`
        if count >= 2
            && let Some(Statement::Let {
                result: combine_result,
                rhs: combine_rhs,
            }) = module.statement(statements[count - 1])
            && returned == Atom::Value(*combine_result)
            && let Some((found, left, right)) = Monoid::of_rhs(module, combine_rhs)
            && let Some(Statement::Let {
                result: call_result,
                rhs: Rhs::Apply { callee, .. },
            }) = module.statement(statements[count - 2])
            && *callee == Atom::Function(function)
        {
            let call_atom = Atom::Value(*call_result);
            let left_self = left == call_atom;
            let right_self = right == call_atom;
            if left_self ^ right_self {
                if !found.commutative() && left_self {
                    // `f(rest) ++ k` needs a difference list; out of scope.
                    return None;
                }
                match monoid {
                    None => monoid = Some(found),
                    Some(previous) if previous == found => {}
                    Some(_) => return None,
                }
                combines += 1;
                spine_calls += 1;
                leaves.push(Leaf::Combine {
                    block,
                    call: statements[count - 2],
                    combine: statements[count - 1],
                    addend: if left_self { right } else { left },
                });
                continue;
            }
        }

        // `a = f(args); return a`
        if count >= 1
            && let Some(Statement::Let {
                result,
                rhs: Rhs::Apply { callee, .. },
            }) = module.statement(statements[count - 1])
            && returned == Atom::Value(*result)
            && *callee == Atom::Function(function)
        {
            spine_calls += 1;
            leaves.push(Leaf::Bare {
                call: statements[count - 1],
            });
            continue;
        }

        leaves.push(Leaf::Base { block });
    }

    if combines == 0 {
        return None;
    }
    let monoid = monoid?;

    // Every self-reference in the function's whole region must be one of the spine calls: a self-call off the spine (or the function escaping as a value from its own body) declines the transform.
    if count_self_references(module, function) != spine_calls {
        return None;
    }

    Some((monoid, leaves))
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
            if block.terminator.atom() == Some(Atom::Function(function)) {
                count += 1;
            }
        }
    }
    count
}
