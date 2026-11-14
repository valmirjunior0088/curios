//! A recursion's frames, bounded on the native stack and continued on the heap: the rule that a recursion is as deep as the heap allows ([`documentation/design/compilation/a-recursion-is-as-deep-as-the-heap-allows.md`](../../documentation/design/compilation/a-recursion-is-as-deep-as-the-heap-allows.md)).
//!
//! **Only a cycle grows the stack with the input.** A chain of calls on no cycle of the call graph is as deep as the program text allows, so the calls [`bound_frames`] guards are the non-tail calls between two members of one strongly connected component of the known-callee graph. A tail call keeps no frame and is left alone.
//!
//! **A guarded call asks the frame budget first.** [`IntrinsicCall::FrameEnter`] takes the callee's weight from one budget the whole program shares, where it fits, and the call goes native as it was, handing the weight back by [`IntrinsicCall::FrameLeave`] once it returns; where it does not fit, the call goes to the callee's twin with a continuation that hands the answer straight back, so everything deeper runs on the heap inside that one native call.
//!
//! **A twin runs its function on the heap.** It is the function's body taking one more parameter, the continuation `k` its answer goes to. A guarded call in it becomes a tail call of the callee's twin, handing over the rest of the body as a closure; a return becomes a tail call of `k`. Every continuation of the body is lifted to a function bound where its `LetCont` stood, so the rest of the body after any guarded call is a value a closure can carry, its scope the scope the continuation had, and a jump becomes a tail call of what it jumped to. A call to a function on no cycle stays native, its frame popped before the twin goes on.
//!
//! **A frame's weight is a stand-in for its bytes.** No stage reads an engine's frame size, so a call weighs a fixed overhead plus one for each value its callee's body binds, the slots its locals are allocated from ([`frame_weight`]).

#[cfg(test)]
mod tests;

use {
    super::{
        Atom, Callee, Continuation, ContinuationId, CopyPlan, Edge, Function, FunctionId,
        Intrinsic, IntrinsicCall, Mapping, Module, Node, NodeId, ValueExpr, ValueId, analyze_calls,
        clone_node, copy_bodies, function_nodes,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// What the native frames of guarded calls may weigh at once, in [`frame_weight`]'s units, across the whole program.
pub const FRAME_BUDGET: u32 = 65_536;

/// What a frame weighs before the values its body binds.
const FRAME_OVERHEAD: u32 = 20;

/// Bound the native frames a recursion holds: guard each non-tail call between two members of one call-graph cycle, and give every member of a guarded cycle its twin.
pub fn bound_frames(module: &mut Module) {
    let calls = analyze_calls(module);
    let component = |function: FunctionId| calls.sccs.component_of.get(&function).copied();
    let cyclic = |function: FunctionId| {
        component(function).is_some_and(|id| {
            calls.sccs.members[id].len() > 1
                || calls
                    .call_graph
                    .get(&function)
                    .is_some_and(|callees| callees.contains(&function))
        })
    };

    // The guarded sites, read off the module as the optimizer left it, each with its owner and callee.
    let mut guarded: Vec<(FunctionId, NodeId, FunctionId)> = Vec::new();
    for (owner, _) in module.live_functions() {
        if !cyclic(owner) {
            continue;
        }
        let sentinel = module.function(owner).unwrap().return_cont;
        for node in function_nodes(module, owner) {
            if let Node::ApplyFun {
                callee: Callee::Known(callee),
                return_to,
                ..
            } = module.node(node).unwrap()
                && *return_to != sentinel
                && component(*callee) == component(owner)
            {
                guarded.push((owner, node, *callee));
            }
        }
    }
    if guarded.is_empty() {
        return;
    }

    // A twin is bound beside its original, so a component one of whose members no `LetFun` binds — the entry — keeps its frames native.
    let binders: BTreeMap<FunctionId, NodeId> = module
        .live_nodes()
        .filter_map(|(node, definition)| match definition {
            Node::LetFun { functions, .. } => Some((node, functions.clone())),
            _ => None,
        })
        .flat_map(|(node, functions)| functions.into_iter().map(move |function| (function, node)))
        .collect();
    let bindable = |owner: FunctionId| {
        component(owner).is_some_and(|id| {
            calls.sccs.members[id]
                .iter()
                .all(|member| binders.contains_key(member))
        })
    };
    guarded.retain(|(owner, _, _)| bindable(*owner));
    if guarded.is_empty() {
        return;
    }

    let members: BTreeSet<FunctionId> = guarded
        .iter()
        .flat_map(|(owner, _, _)| {
            let id = component(*owner).expect("a guarded owner lies on a component");
            calls.sccs.members[id].iter().copied()
        })
        .collect();
    let arities = module.return_arities();
    let weights: BTreeMap<FunctionId, u32> = members
        .iter()
        .map(|&member| (member, frame_weight(module, member)))
        .collect();

    // Every twin is copied before any original is guarded, so no twin carries a guard.
    let twins: BTreeMap<FunctionId, FunctionId> = members
        .iter()
        .map(|&member| (member, copy_alone(module, member, binders[&member])))
        .collect();
    let same_component = |a: FunctionId, b: FunctionId| component(a) == component(b);
    for (&original, &twin) in &twins {
        Twin {
            module,
            original,
            twins: &twins,
            arities: &arities,
            same_component: &same_component,
        }
        .lower(twin);
    }
    for (_, node, callee) in guarded {
        guard(
            module,
            node,
            callee,
            twins[&callee],
            weights[&callee],
            arities.get(&callee).copied().unwrap_or(1),
        );
    }
}

/// What a native call of `function` takes from the frame budget: a fixed overhead and one for each value its body binds, its parameters, every `let` and every continuation's parameters, never more than a quarter of the budget.
fn frame_weight(module: &Module, function: FunctionId) -> u32 {
    let definition = module.function(function).unwrap();
    let mut values = definition.params.len();
    for node in function_nodes(module, function) {
        match module.node(node).unwrap() {
            Node::LetValue { .. } | Node::LetIntrinsic { .. } => values += 1,
            Node::LetCont { continuations, .. } => {
                for continuation in continuations {
                    if let Some(continuation) = module.continuation(*continuation) {
                        values += continuation.params.len();
                    }
                }
            }
            _ => {}
        }
    }

    let weight = FRAME_OVERHEAD.saturating_add(u32::try_from(values).unwrap_or(u32::MAX));
    weight.min(FRAME_BUDGET / 4)
}

/// The hint a function of the heap path carries, `heap/` ahead of the name it was copied or lifted from, so a reader of the emitted module tells the code past the budget from the native code it repeats.
fn heap_named(name: Option<&str>) -> String {
    format!("heap/{}", name.unwrap_or_default())
}

/// A copy of `function` and everything defined within it, at fresh identities, bound in the group `function` is bound in. Every copied node names `function` where the original did, and not the copy: the functions nested in the copy are ordinary closures, and the copy's own calls are rewritten into its twin's by [`Twin`], which reads them off the originals they name.
fn copy_alone(module: &mut Module, function: FunctionId, binder: NodeId) -> FunctionId {
    let plan = CopyPlan::functions_with_limit(module, BTreeSet::from([function]), usize::MAX)
        .expect("a function's extent is copied whole when no limit applies");
    let copies = copy_bodies(module, plan);
    let copy = copies.functions[&function];
    if let Some(Node::LetFun { functions, .. }) = module.node_mut(binder) {
        functions.push(copy);
    }

    let back = |id: FunctionId| if id == copy { function } else { id };
    for &copied in copies.functions.values() {
        for node in function_nodes(module, copied) {
            let rewritten = clone_node(
                module.node(node).unwrap(),
                &Mapping {
                    value: &|value| value,
                    atom: &|atom| match atom {
                        Atom::Fun(id) => Atom::Fun(back(*id)),
                        other => other.clone(),
                    },
                    cont: &|cont| cont,
                    callee: &|callee| match callee {
                        Callee::Known(id) => Callee::Known(back(*id)),
                        other => other.clone(),
                    },
                    function: &|id| id,
                    node: &|node| node,
                },
            );
            module.set_node(node, rewritten);
        }
    }

    copy
}

/// The rewrite of one function's copy into its twin.
struct Twin<'a> {
    module: &'a mut Module,
    /// The function the copy was made of.
    original: FunctionId,
    twins: &'a BTreeMap<FunctionId, FunctionId>,
    arities: &'a BTreeMap<FunctionId, usize>,
    same_component: &'a dyn Fn(FunctionId, FunctionId) -> bool,
}

impl Twin<'_> {
    /// Rewrite `twin`, a copy of [`Twin::original`], into the twin that runs it on the heap.
    fn lower(mut self, twin: FunctionId) {
        let k = self.module.add_value(Some("k".to_string()));
        let (sentinel, body) = {
            let definition = self.module.function_mut(twin).unwrap();
            definition.params.push(k);
            definition.droppable = false;
            definition.debug_name = Some(heap_named(definition.debug_name.as_deref()));
            (definition.return_cont, definition.body)
        };
        let returns = self.arities.get(&self.original).copied().unwrap_or(1);

        // Each continuation the body binds, lifted to the function bound in its place.
        let mut lifted: BTreeMap<ContinuationId, FunctionId> = BTreeMap::new();
        let mut work = vec![(body, sentinel)];
        let mut seen = BTreeSet::new();
        while let Some((node, current)) = work.pop() {
            if !seen.insert(node) {
                continue;
            }
            match self.module.node(node).unwrap().clone() {
                Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. } => {
                    work.push((next, current));
                }
                Node::LetFun { body, .. } => work.push((body, current)),
                Node::LetCont {
                    continuations,
                    body,
                } => {
                    let functions = continuations
                        .iter()
                        .map(|&id| {
                            let continuation = self.module.continuation(id).unwrap().clone();
                            let return_cont = self.module.reserve_continuation();
                            let function = self.module.add_function(Function {
                                debug_name: Some(heap_named(continuation.debug_name.as_deref())),
                                params: continuation.params.clone(),
                                return_cont,
                                body: continuation.body,
                                droppable: false,
                            });
                            lifted.insert(id, function);
                            work.push((continuation.body, return_cont));
                            function
                        })
                        .collect();
                    self.module.set_node(node, Node::LetFun { functions, body });
                    work.push((body, current));
                }
                terminator => {
                    let rewritten =
                        self.terminator(terminator, sentinel, current, k, returns, &lifted);
                    self.module.set_node(node, rewritten);
                }
            }
        }

        // The lifted continuations are bound nowhere now, and the rewrite read their arity until it was done.
        for continuation in lifted.into_keys() {
            self.module.remove_continuation(continuation);
        }
    }

    /// One terminator of the twin's body, standing in the function whose return is `current`: a jump to a lifted continuation calls its function, a return calls `k`, and anything resuming a continuation resumes a thin one that does either.
    fn terminator(
        &mut self,
        node: Node,
        returned: ContinuationId,
        current: ContinuationId,
        k: ValueId,
        returns: usize,
        lifted: &BTreeMap<ContinuationId, FunctionId>,
    ) -> Node {
        let to = |target: ContinuationId, args: Vec<Atom>| -> Node {
            match target == returned {
                true => Node::ApplyFun {
                    callee: Callee::Closure(k),
                    args,
                    return_to: current,
                },
                false => Node::ApplyFun {
                    callee: Callee::Known(lifted[&target]),
                    args,
                    return_to: current,
                },
            }
        };

        match node {
            Node::ApplyCont(Edge { target, args }) => to(target, args),
            Node::Switch {
                scrutinee,
                cases,
                default,
            } => {
                let mut thins = Vec::new();
                let mut thin_edge = |edge: Edge| {
                    let params: Vec<ValueId> = edge
                        .args
                        .iter()
                        .map(|_| self.module.add_value(None))
                        .collect();
                    let body = self.module.add_node(to(
                        edge.target,
                        params.iter().copied().map(Atom::Value).collect(),
                    ));
                    let thin = self.module.add_continuation(Continuation {
                        debug_name: None,
                        params,
                        body,
                    });
                    thins.push(thin);
                    Edge {
                        target: thin,
                        args: edge.args,
                    }
                };
                let cases = cases
                    .into_iter()
                    .map(|(tag, edge)| (tag, thin_edge(edge)))
                    .collect();
                let default = default.map(&mut thin_edge);
                let switch = self.module.add_node(Node::Switch {
                    scrutinee,
                    cases,
                    default,
                });
                Node::LetCont {
                    continuations: thins,
                    body: switch,
                }
            }
            Node::ApplyFun {
                callee,
                mut args,
                return_to,
            } => {
                let twin = match &callee {
                    Callee::Known(callee) => self.twins.get(callee).copied(),
                    Callee::Closure(_) => None,
                };
                match (twin, return_to == returned) {
                    // A tail call of a member with a twin hands its own continuation on.
                    (Some(twin), true) => {
                        args.push(Atom::Value(k));
                        Node::ApplyFun {
                            callee: Callee::Known(twin),
                            args,
                            return_to: current,
                        }
                    }
                    // A guarded call hands the rest of the body on, as the function its continuation was lifted to.
                    (Some(twin), false) if matches!(callee, Callee::Known(callee) if (self.same_component)(callee, self.original)) =>
                    {
                        args.push(Atom::Fun(lifted[&return_to]));
                        Node::ApplyFun {
                            callee: Callee::Known(twin),
                            args,
                            return_to: current,
                        }
                    }
                    _ => {
                        let arity = match (&callee, return_to == returned) {
                            (_, true) => returns,
                            (_, false) => self
                                .module
                                .continuation(return_to)
                                .map_or(1, |c| c.params.len()),
                        };
                        self.thin(
                            Node::ApplyFun {
                                callee,
                                args,
                                return_to,
                            },
                            return_to,
                            arity,
                            &to,
                        )
                    }
                }
            }
            Node::Foreign {
                function,
                args,
                return_to,
            } => {
                let arity = function.signature().imported_results().len();
                self.thin(
                    Node::Foreign {
                        function,
                        args,
                        return_to,
                    },
                    return_to,
                    arity,
                    &to,
                )
            }
            Node::Cell {
                op,
                args,
                return_to,
            } => self.thin(
                Node::Cell {
                    op,
                    args,
                    return_to,
                },
                return_to,
                op.result_arity(),
                &to,
            ),
            Node::Channel {
                op,
                args,
                return_to,
            } => self.thin(
                Node::Channel {
                    op,
                    args,
                    return_to,
                },
                return_to,
                op.result_arity(),
                &to,
            ),
            Node::Intrinsic {
                op,
                args,
                return_to,
            } => self.thin(
                Node::Intrinsic {
                    op,
                    args,
                    return_to,
                },
                return_to,
                op.result_arity(),
                &to,
            ),
            other @ (Node::Halt { .. } | Node::Panic(_) | Node::Unreachable) => other,
            Node::LetValue { .. }
            | Node::LetIntrinsic { .. }
            | Node::LetFun { .. }
            | Node::LetCont { .. } => unreachable!("a binding is no terminator"),
        }
    }

    /// `node`, which resumes `return_to` with `arity` values, made to resume a thin continuation instead that hands them where `to` sends `return_to`'s.
    fn thin(
        &mut self,
        node: Node,
        return_to: ContinuationId,
        arity: usize,
        to: &dyn Fn(ContinuationId, Vec<Atom>) -> Node,
    ) -> Node {
        let params: Vec<ValueId> = (0..arity).map(|_| self.module.add_value(None)).collect();
        let body = self.module.add_node(to(
            return_to,
            params.iter().copied().map(Atom::Value).collect(),
        ));
        let thin = self.module.add_continuation(Continuation {
            debug_name: None,
            params,
            body,
        });
        let resumed = match node {
            Node::ApplyFun { callee, args, .. } => Node::ApplyFun {
                callee,
                args,
                return_to: thin,
            },
            Node::Foreign { function, args, .. } => Node::Foreign {
                function,
                args,
                return_to: thin,
            },
            Node::Cell { op, args, .. } => Node::Cell {
                op,
                args,
                return_to: thin,
            },
            Node::Channel { op, args, .. } => Node::Channel {
                op,
                args,
                return_to: thin,
            },
            Node::Intrinsic { op, args, .. } => Node::Intrinsic {
                op,
                args,
                return_to: thin,
            },
            other => other,
        };
        let body = self.module.add_node(resumed);
        Node::LetCont {
            continuations: vec![thin],
            body,
        }
    }
}

/// Guard the native call at `node`, of `callee` weighing `weight` and answering `returns` values: native where the budget admits it, handing the weight back once it returns, and otherwise the callee's twin with a continuation that hands its answer back.
fn guard(
    module: &mut Module,
    node: NodeId,
    callee: FunctionId,
    twin: FunctionId,
    weight: u32,
    returns: usize,
) {
    let Node::ApplyFun {
        args, return_to, ..
    } = module.node(node).unwrap().clone()
    else {
        unreachable!("a guarded site is a call");
    };

    // Native: the call as it was, resuming where the weight is handed back.
    let answers: Vec<ValueId> = (0..returns).map(|_| module.add_value(None)).collect();
    let resume = module.add_node(Node::ApplyCont(Edge {
        target: return_to,
        args: answers.iter().copied().map(Atom::Value).collect(),
    }));
    let back = module.add_continuation(Continuation {
        debug_name: None,
        params: Vec::new(),
        body: resume,
    });
    let leave_call = module.add_node(Node::Intrinsic {
        op: IntrinsicCall::FrameLeave { weight },
        args: Vec::new(),
        return_to: back,
    });
    let leave_body = module.add_node(Node::LetCont {
        continuations: vec![back],
        body: leave_call,
    });
    let leave = module.add_continuation(Continuation {
        debug_name: None,
        params: answers,
        body: leave_body,
    });
    let native_call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: args.clone(),
        return_to: leave,
    });
    let native = module.add_continuation(Continuation {
        debug_name: None,
        params: Vec::new(),
        body: native_call,
    });

    // Heap: the twin, handed a continuation that answers what the call would have.
    let (handed, resumed) = handing_back(module, return_to, returns);
    let mut heap_args = args;
    heap_args.push(Atom::Fun(handed));
    let heap_call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(twin),
        args: heap_args,
        return_to: resumed,
    });
    let heap_call = match resumed == return_to {
        true => heap_call,
        false => module.add_node(Node::LetCont {
            continuations: vec![resumed],
            body: heap_call,
        }),
    };
    let heap_body = module.add_node(Node::LetFun {
        functions: vec![handed],
        body: heap_call,
    });
    let heap = module.add_continuation(Continuation {
        debug_name: None,
        params: Vec::new(),
        body: heap_body,
    });

    // The choice between them, read off the budget.
    let admitted = module.add_value(Some("admitted".to_string()));
    let choose = module.add_node(Node::Switch {
        scrutinee: Atom::Value(admitted),
        cases: BTreeMap::from([(
            1,
            Edge {
                target: native,
                args: Vec::new(),
            },
        )]),
        default: Some(Edge {
            target: heap,
            args: Vec::new(),
        }),
    });
    let chosen = module.add_continuation(Continuation {
        debug_name: None,
        params: vec![admitted],
        body: choose,
    });
    let enter = module.add_node(Node::Intrinsic {
        op: IntrinsicCall::FrameEnter { weight },
        args: Vec::new(),
        return_to: chosen,
    });
    module.set_node(
        node,
        Node::LetCont {
            continuations: vec![chosen, native, leave, heap],
            body: enter,
        },
    );
}

/// The continuation closure a native call hands a twin, and the continuation the call resumes: the closure answers its `returns` values as the one value a closure answers, a tuple of them unless there is one, and the continuation takes that tuple apart into `return_to`.
fn handing_back(
    module: &mut Module,
    return_to: ContinuationId,
    returns: usize,
) -> (FunctionId, ContinuationId) {
    let params: Vec<ValueId> = (0..returns).map(|_| module.add_value(None)).collect();
    let return_cont = module.reserve_continuation();
    let body = match returns {
        1 => module.add_node(Node::ApplyCont(Edge {
            target: return_cont,
            args: vec![Atom::Value(params[0])],
        })),
        _ => {
            let packed = module.add_value(None);
            let answer = module.add_node(Node::ApplyCont(Edge {
                target: return_cont,
                args: vec![Atom::Value(packed)],
            }));
            module.add_node(Node::LetValue {
                result: packed,
                value: ValueExpr::Tuple(params.iter().copied().map(Atom::Value).collect()),
                next: answer,
            })
        }
    };
    let handed = module.add_function(Function {
        debug_name: Some(heap_named(Some("answer"))),
        params,
        return_cont,
        body,
        droppable: false,
    });
    if returns == 1 {
        return (handed, return_to);
    }

    let packed = module.add_value(None);
    let fields: Vec<ValueId> = (0..returns).map(|_| module.add_value(None)).collect();
    let mut next = module.add_node(Node::ApplyCont(Edge {
        target: return_to,
        args: fields.iter().copied().map(Atom::Value).collect(),
    }));
    for (index, field) in fields.iter().enumerate().rev() {
        next = module.add_node(Node::LetIntrinsic {
            result: *field,
            op: Intrinsic::TupleGet(index),
            args: vec![Atom::Value(packed)],
            next,
        });
    }
    let unpacked = module.add_continuation(Continuation {
        debug_name: None,
        params: vec![packed],
        body: next,
    });
    (handed, unpacked)
}
