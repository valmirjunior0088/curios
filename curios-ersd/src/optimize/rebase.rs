//! The monoid worker/wrapper: a recursion whose answer is its own call's answer combined under a monoid becomes a loop over accumulators — tail recursion modulo a monoid context, as Leijen and Lorenzen derive it (*Tail Recursion Modulo Context*, POPL 2023, §4.4–4.5). Why, and what it rejects, is [`curios-ersd`'s README](../../README.md#a-recursion-closed-by-a-monoid-is-a-loop).
//!
//! A recursion whose result feeds a monoid combine (`count(t) + 1`, `x == y && eql(xs, ys)`, `[h, ..take(t, k)]`) is no tail call: the combine runs after the recursion returns, so every level keeps a frame and pays a return. But a worker that folds the combine's other operands into an accumulator ahead of the call is a loop:
//!
//! ```text
//!   rec f(x) = … l ⊕ f(t) ⊕ r …
//!   ⟶  f(x)          = w(x, e, e)                  (the thin wrapper, e the identity)
//!      rec w(x, a, b) = … w(t, a ⊕ l, r ⊕ b) …     (base arms return a ⊕ v ⊕ b)
//! ```
//!
//! A cost transform, not a correctness one: a module runs on a stack of its own, which holds a recursion left as written at any depth a program meets ([`curios-runtime`'s README](../../../curios-runtime/README.md#a-module-runs-on-a-stack-of-its-own)), and what the rewrite removes is a frame and a return an element, and the stack the depth touches. It is sound-but-incomplete: outside the recognized envelope it is a no-op, never a miscompile.
//!
//! **The envelope, structural in ANF.** A leaf tail block ends in a self-call followed by a chain of combines of one monoid — each reading the answer of the one before it, the call's first, exactly once and in a place the monoid's product takes, every other operand a value from before the call — and returns the chain's last answer; or it ends in a bare tail self-call; or it is a base. The chain is one word `l ⊕ □ ⊕ r` of the monoid, its later links wrapping the earlier: `l₂ ⊕ (l₁ ⊕ □ ⊕ r₁) ⊕ r₂`. Only the combines move across the call, so nothing observable is reordered: Koka's liberal context, whose operands after the hole are values. Every combine leaf shares one monoid, and every self-reference in the function's whole region stands on the spine.
//!
//! **The placement rule.** A monoid that commutes folds both sides of every leaf into one accumulator, `a ⊕ l ⊕ r`. One that does not — a sequence's concatenation — threads the left side's accumulator where a leaf has a left side, the right side's where one has a right side, and both where both occur, as Koka's product context does: a builder written `[h, ..f(t)]` takes `a ++ [h]`, and one written `[..f(t), x]` takes `[x] ++ b`.
//!
//! **Which operation is a monoid is the oracle's** ([`Semantics::monoid_identity`], [`Semantics::free_monoid_part`]): every scalar monoid commutes, and a sequence's concatenation, with an append as its product by one generator, does not. The licence is associativity and an erasure-stable identity, because nothing here is partial. The rewrite reverses the order the operands are combined in — the written recursion folds them innermost-out, `((v ⊕ kₙ) ⊕ …) ⊕ k₁`, and the worker threads them `k₁` first — so the *partial* results differ even though the total does not. A bounded carrier would need a third condition, monotone definedness, since a reversed order could refuse where the written one computes — a multiplication would turn `((1 * 0) * 2¹⁶) * 2¹⁶` into a trap. `Nat` and `Int` are unbounded at run time ([Nat and Int are an i31 until they outgrow it](../../../documentation/design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md)), and the `Bool` operations and the concatenations total, so every partial of every order is defined, and a reversed order costs at most the size of what its partials build.

#[cfg(test)]
mod tests;

use {
    crate::{
        Atom, Block, BlockId, Constant, FreeMonoidPart, Function, FunctionId, Module, Operation,
        Rhs, Semantics, SequenceGrain, SequenceOp, Statement, StatementId, Terminator,
        control_blocks,
    },
    curios_num::Binary,
    std::collections::BTreeSet,
};

/// The monoid a recursion's combines share.
#[derive(Clone, PartialEq, Eq)]
enum Row {
    /// A scalar operation the oracle names a monoid, with its identity. It commutes.
    Scalar {
        operation: Operation,
        identity: Constant,
    },
    /// A sequence grain's free monoid: concatenation over the empty sequence. It does not commute.
    Sequence(SequenceGrain),
}

/// One operand of a context's side: a value of the monoid's carrier, or, at a sequence, one element of it.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Piece {
    Whole(Atom),
    Element(Atom),
}

/// The context a chain of combines puts around its hole, `left ⊕ □ ⊕ right`, each side in order.
#[derive(Default)]
struct Context {
    left: Vec<Piece>,
    right: Vec<Piece>,
}

/// What a right-hand side computes, read through forwarders.
enum Combination {
    Scalar(Operation),
    Sequence(SequenceOp),
}

impl Row {
    /// The monoid a right-hand side combines in and the context it puts around `hole`, where it is one: a combine of a monoid the oracle names that reads `hole` exactly once, in a place the monoid's product takes, and nothing else in `computed`.
    ///
    /// The corpus combines through concept witnesses and `/sys` (`a + b` is `Add/add(a, b)`, a forwarder onto `Nat/add`, a forwarder onto the intrinsic; `a && b` is `/sys/Bool/and`; `Str/concat` forwards onto `Bytes/concat`), so the resolver is what makes the envelope reach idiomatic code at all; unfolding a pure forwarder chain preserves meaning exactly.
    fn link(module: &Module, rhs: &Rhs, hole: Atom, computed: &[Atom]) -> Option<(Row, Context)> {
        let (combination, operands) = match rhs {
            Rhs::Operation {
                operation,
                operands,
            } => (Combination::Scalar(*operation), operands.as_slice()),
            Rhs::Sequence {
                operation,
                operands,
            } => (Combination::Sequence(*operation), operands.as_slice()),
            Rhs::Apply {
                callee: Atom::Function(callee),
                arguments,
            } if arguments.len() == 2 => {
                (resolve_forwarder(module, *callee, 8)?, arguments.as_slice())
            }
            _ => return None,
        };
        let mut holes = operands
            .iter()
            .enumerate()
            .filter(|(_, operand)| **operand == hole)
            .map(|(index, _)| index);
        let (Some(at), None) = (holes.next(), holes.next()) else {
            return None;
        };
        let (before, after) = (&operands[..at], &operands[at + 1..]);
        if before
            .iter()
            .chain(after)
            .any(|operand| computed.contains(operand))
        {
            return None;
        }
        let wholes = |operands: &[Atom]| operands.iter().map(|&k| Piece::Whole(k)).collect();
        match combination {
            Combination::Scalar(operation) => {
                let identity = Semantics::monoid_identity(operation)?;
                let context = Context {
                    left: before
                        .iter()
                        .chain(after)
                        .map(|&k| Piece::Whole(k))
                        .collect(),
                    right: Vec::new(),
                };
                Some((
                    Row::Scalar {
                        operation,
                        identity,
                    },
                    context,
                ))
            }
            Combination::Sequence(operation) => {
                let (grain, part) = Semantics::free_monoid_part(operation)?;
                let context = match (part, before, after) {
                    (FreeMonoidPart::Product, _, _) => Context {
                        left: wholes(before),
                        right: wholes(after),
                    },
                    (FreeMonoidPart::Append, [], [element]) => Context {
                        left: Vec::new(),
                        right: vec![Piece::Element(*element)],
                    },
                    // An append's element is no place the product takes: a hole there keeps the recursion's answer whole, a constructor's context.
                    (FreeMonoidPart::Append, _, _) => return None,
                };
                Some((Row::Sequence(grain), context))
            }
        }
    }

    /// Whether the monoid's operands may be exchanged, so that one accumulator takes both sides.
    fn commutes(&self) -> bool {
        matches!(self, Row::Scalar { .. })
    }

    /// The identity, bound by a statement appended to `statements` where it has no constant, as a list has none.
    fn identity(&self, module: &mut Module, statements: &mut Vec<StatementId>) -> Atom {
        match self {
            Row::Scalar { identity, .. } => {
                Atom::Constant(module.intern_constant(identity.clone()))
            }
            Row::Sequence(SequenceGrain::Bytes) => {
                Atom::Constant(module.intern_constant(Constant::Bytes(Binary::empty())))
            }
            Row::Sequence(SequenceGrain::List) => {
                sequence(module, statements, SequenceOp::ListBuild, Vec::new())
            }
        }
    }

    /// Whether `atom`, what a base arm answers, is the identity, which the arm's combine drops: a constant equal to it, or an empty list `block` builds.
    fn is_identity(&self, module: &Module, block: &Block, atom: Atom) -> bool {
        match (self, atom) {
            (Row::Scalar { identity, .. }, Atom::Constant(constant)) => {
                module.constant(constant) == Some(identity)
            }
            (Row::Sequence(SequenceGrain::Bytes), Atom::Constant(constant)) => matches!(
                module.constant(constant),
                Some(Constant::Bytes( run)) if *run == Binary::empty()
            ),
            (Row::Sequence(SequenceGrain::List), Atom::Value(value)) => {
                block.statements.iter().any(|&statement| {
                    matches!(
                        module.statement(statement),
                        Some(Statement::Let {
                            result,
                            rhs: Rhs::Sequence {
                                operation: SequenceOp::ListBuild,
                                operands,
                            },
                        }) if *result == value && operands.is_empty()
                    )
                })
            }
            _ => false,
        }
    }

    /// The pieces as values of the carrier, in order, each run of consecutive elements built into one sequence by statements appended to `statements`.
    fn values(
        &self,
        module: &mut Module,
        statements: &mut Vec<StatementId>,
        pieces: &[Piece],
    ) -> Vec<Atom> {
        pieces
            .chunk_by(|a, b| matches!((a, b), (Piece::Element(_), Piece::Element(_))))
            .map(|run| match run {
                [Piece::Whole(whole)] => *whole,
                elements => {
                    let elements = elements.iter().map(|piece| match piece {
                        Piece::Element(element) | Piece::Whole(element) => *element,
                    });
                    match self {
                        Row::Sequence(SequenceGrain::List) => sequence(
                            module,
                            statements,
                            SequenceOp::ListBuild,
                            elements.collect(),
                        ),
                        Row::Sequence(SequenceGrain::Bytes) => elements.fold(
                            Atom::Constant(
                                module.intern_constant(Constant::Bytes(Binary::empty())),
                            ),
                            |run, element| {
                                sequence(
                                    module,
                                    statements,
                                    SequenceOp::BytesAppend,
                                    vec![run, element],
                                )
                            },
                        ),
                        Row::Scalar { .. } => unreachable!("a scalar context holds no element"),
                    }
                }
            })
            .collect()
    }

    /// `operands` combined in order by statements appended to `statements`: a chain of the operation at a scalar, one concatenation at a sequence, and the operand itself where there is one.
    fn combine(
        &self,
        module: &mut Module,
        statements: &mut Vec<StatementId>,
        operands: Vec<Atom>,
    ) -> Atom {
        if let [only] = operands.as_slice() {
            return *only;
        }
        match self {
            Row::Scalar { operation, .. } => {
                let mut operands = operands.into_iter();
                let first = operands
                    .next()
                    .expect("a combine has an accumulator to fold into");
                operands.fold(first, |left, right| {
                    let folded = module.add_value(None);
                    statements.push(module.add_statement(Statement::Let {
                        result: folded,
                        rhs: Rhs::Operation {
                            operation: *operation,
                            operands: vec![left, right],
                        },
                    }));
                    Atom::Value(folded)
                })
            }
            Row::Sequence(SequenceGrain::List) => {
                sequence(module, statements, SequenceOp::ListConcat, operands)
            }
            Row::Sequence(SequenceGrain::Bytes) => {
                sequence(module, statements, SequenceOp::BytesConcat, operands)
            }
        }
    }
}

/// A sequence operation over `operands`, bound by a statement appended to `statements`.
fn sequence(
    module: &mut Module,
    statements: &mut Vec<StatementId>,
    operation: SequenceOp,
    operands: Vec<Atom>,
) -> Atom {
    let result = module.add_value(None);
    statements.push(module.add_statement(Statement::Let {
        result,
        rhs: Rhs::Sequence {
            operation,
            operands,
        },
    }));
    Atom::Value(result)
}

/// The operation a binary forwarder chain bottoms out in: a function whose body is exactly one statement over its two parameters in order — the operation itself, or a saturated call to the next forwarder.
fn resolve_forwarder(module: &Module, function: FunctionId, budget: usize) -> Option<Combination> {
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
        } if forwards(operands) => Some(Combination::Scalar(*operation)),
        Rhs::Sequence {
            operation,
            operands,
        } if forwards(operands) => Some(Combination::Sequence(*operation)),
        Rhs::Apply {
            callee: Atom::Function(next),
            arguments,
        } if forwards(arguments) => resolve_forwarder(module, *next, budget - 1),
        _ => None,
    }
}

/// How one leaf tail block ends.
enum Leaf {
    /// `…, a = f(args), b₁ = C₁[a], …, bₙ = Cₙ[bₙ₋₁]; return bₙ` — a self-call closed by a chain of combines, read as one context.
    Combine {
        block: BlockId,
        call: StatementId,
        context: Context,
    },
    /// `a = f(args); return a`.
    Bare { call: StatementId },
    /// Any other returning leaf: its answer goes between the accumulators.
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

    // The accumulators the worker threads: one for a monoid that commutes, and for one that does not, the left side's where a leaf has a left side and the right side's where one has a right side.
    let sided = |side: fn(&Context) -> &Vec<Piece>| {
        leaves
            .iter()
            .any(|leaf| matches!(leaf, Leaf::Combine { context, .. } if !side(context).is_empty()))
    };
    let (threads_left, threads_right) = match row.commutes() {
        true => (true, false),
        false => (
            sided(|context| &context.left),
            sided(|context| &context.right),
        ),
    };
    let left = threads_left.then(|| module.add_value(Some("acc".into())));
    let right = threads_right.then(|| module.add_value(Some("acc_after".into())));
    let accumulators: Vec<Atom> = left.into_iter().chain(right).map(Atom::Value).collect();

    // The wrapper keeps the function's identity: fresh parameters, a new body that seeds each accumulator with the monoid's identity. The worker takes the original parameters plus the accumulators, and the original body.
    let worker = module.reserve_function();
    let wrapper_params: Vec<_> = params
        .iter()
        .map(|&param| {
            let name = module
                .value(param)
                .and_then(|value| value.debug_name.clone());
            module.add_value(name)
        })
        .collect();
    let mut wrapper_statements = Vec::new();
    let seed = row.identity(module, &mut wrapper_statements);
    let call_result = module.add_value(None);
    wrapper_statements.push(
        module.add_statement(Statement::Let {
            result: call_result,
            rhs: Rhs::Apply {
                callee: Atom::Function(worker),
                arguments: wrapper_params
                    .iter()
                    .map(|&param| Atom::Value(param))
                    .chain(accumulators.iter().map(|_| seed))
                    .collect(),
            },
        }),
    );
    let wrapper_body = module.add_block(Block {
        statements: wrapper_statements,
        terminator: Terminator::Return(Atom::Value(call_result)),
    });

    let mut worker_params = params;
    worker_params.extend(left.into_iter().chain(right));
    let worker_name = debug_name.as_ref().map(|name| format!("{name}@w"));
    // A description is a zero-parameter thunk and never reaches the accumulator split, so both halves are ordinary computations. The termination verdict is the split definition's and is carried to both halves: re-basing a recursion moves where the accumulators live, never whether the recursion descends.
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
    for leaf in leaves {
        match leaf {
            Leaf::Combine {
                block,
                call,
                context,
            } => {
                let Some(Statement::Let {
                    result: call_result,
                    rhs: Rhs::Apply { arguments, .. },
                }) = module.statement(call).cloned()
                else {
                    continue;
                };
                let Some(mut statements) = module.block(block).and_then(|definition| {
                    let position = definition.statements.iter().position(|&s| s == call)?;
                    Some(definition.statements[..position].to_vec())
                }) else {
                    continue;
                };
                // The context folds into the accumulators ahead of the call, which becomes a tail call to the worker threading them: `a ⊕ l` on the left and `r ⊕ b` on the right, or, where the monoid commutes, `a ⊕ l ⊕ r` in its one accumulator. The chain's own statements are left in no block, and the prune after the rebase drops them.
                let mut arguments = arguments;
                if let Some(left) = left {
                    let pieces: Vec<Piece> = match row.commutes() {
                        true => context.left.iter().chain(&context.right).copied().collect(),
                        false => context.left,
                    };
                    let mut operands = vec![Atom::Value(left)];
                    operands.extend(row.values(module, &mut statements, &pieces));
                    arguments.push(row.combine(module, &mut statements, operands));
                }
                if let Some(right) = right {
                    let mut operands = row.values(module, &mut statements, &context.right);
                    operands.push(Atom::Value(right));
                    arguments.push(row.combine(module, &mut statements, operands));
                }
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
                arguments.extend(&accumulators);
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
                let Some(definition) = module.block(block) else {
                    continue;
                };
                let Terminator::Return(returned) = definition.terminator else {
                    continue;
                };
                // The answer goes between the accumulators, `a ⊕ v ⊕ b`, and drops out where it is the identity.
                let answer = (!row.is_identity(module, definition, returned)).then_some(returned);
                let operands: Vec<Atom> = left
                    .map(Atom::Value)
                    .into_iter()
                    .chain(answer)
                    .chain(right.map(Atom::Value))
                    .collect();
                if operands.is_empty() || operands == [returned] {
                    continue;
                }
                let mut statements = definition.statements.clone();
                let combined = row.combine(module, &mut statements, operands);
                module.set_block_statements(block, statements);
                module.set_block_terminator(block, Terminator::Return(combined));
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
                context,
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
                    context,
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
        context: Context,
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
    let mut context = Context::default();
    for statement in &block.statements[position + 1..] {
        let Some(Statement::Let { result, rhs }) = module.statement(*statement) else {
            return Ending::Base;
        };
        let Some((found, link)) = Row::link(module, rhs, hole, &computed) else {
            return Ending::Base;
        };
        if row.as_ref().is_some_and(|row| *row != found) {
            return Ending::Base;
        }
        row = Some(found);
        // A later link wraps the earlier: its left side goes before what the chain has gathered, and its right side after.
        context.left.splice(0..0, link.left);
        context.right.extend(link.right);
        hole = Atom::Value(*result);
        computed.push(hole);
    }
    if returned != hole {
        return Ending::Base;
    }
    match row {
        Some(row) => Ending::Combine { call, row, context },
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
