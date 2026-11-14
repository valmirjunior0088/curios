//! A recursion's frames kept on the heap: the rule that a recursion is as deep as the heap allows ([`documentation/design/compilation/a-recursion-is-as-deep-as-the-heap-allows.md`](../../documentation/design/compilation/a-recursion-is-as-deep-as-the-heap-allows.md)).
//!
//! **Only a cycle grows the stack with the input.** A chain of calls on no cycle of the call graph is as deep as the program text allows, so the functions [`bound_frames`] rewrites are the members of a strongly connected component one of whose calls may close it without being a tail call. The graph is the known calls and, for a call through a closure or a list map, an edge to every function a closure of that many parameters may hold, since nothing here tells which one it holds. A component whose every call around it is a tail call keeps no frame and is left alone; a list map keeps its helper's frame under every element, so it closes a cycle wherever it stands.
//!
//! **A member runs on the heap.** Its body becomes its heap body, the function's twin: the same body taking one more parameter, the continuation `k` its answer goes to. A call in it that may close the cycle goes to the heap, handing over the rest of the body as a closure; a tail call does where what it may reach has a heap body, handing `k` on; a return becomes a tail call of `k`. Every continuation of the body is lifted to a function bound where its `LetCont` stood, so the rest of the body after any call is a value a closure can carry, its scope the scope the continuation had, and a jump becomes a tail call of what it jumped to. A call that closes no cycle stays native, its frame popped before the body goes on. A known callee's heap body is its twin; a closure's is the entry [`IntrinsicCall::HeapApply`] reaches, which the emitter dispatches beside the closure's own; a list map's is a loop of those ([`heap_map`]).
//!
//! **The member itself is the entry from native code.** What calls a member from off its cycle — a function on no cycle, a closure call there, the host — calls it as it always did, and the member calls its twin with a continuation that hands the answer back, so a recursion entered from native code holds one native frame however deep it goes. [`Module::twin`] names each member's twin, which is what a closure's heap entry enters.

#[cfg(test)]
mod tests;

use {
    super::{
        Atom, CallAnalysis, Callee, Continuation, ContinuationId, CopyPlan, Edge, Function,
        FunctionId, Intrinsic, IntrinsicCall, Literal, Mapping, Module, Node, NodeId, ValueExpr,
        ValueId, analyze_calls, analyze_sccs, clone_node, copy_bodies, function_nodes,
        prune_unreachable,
    },
    curios_num::Natural,
    std::collections::{BTreeMap, BTreeSet},
};

/// What a call may reach: the function it names, any function a closure of that many parameters holds, or, for a list map, any a closure of one parameter holds.
#[derive(Debug, Clone, Copy)]
enum Reach {
    Known(FunctionId),
    Closure(usize),
    Map,
}

impl Reach {
    /// What `node` may call and the continuation it resumes, where it is a call.
    fn of(node: &Node) -> Option<(Self, ContinuationId)> {
        match node {
            Node::ApplyFun {
                callee: Callee::Known(callee),
                return_to,
                ..
            } => Some((Reach::Known(*callee), *return_to)),
            Node::ApplyFun {
                callee: Callee::Closure(_),
                args,
                return_to,
            } => Some((Reach::Closure(args.len()), *return_to)),
            Node::Intrinsic {
                op: IntrinsicCall::ListMap,
                return_to,
                ..
            } => Some((Reach::Map, *return_to)),
            _ => None,
        }
    }
}

/// The cycles of the call graph, read off the module before any twin is written.
struct Cycles {
    component_of: BTreeMap<FunctionId, usize>,
    members: Vec<Vec<FunctionId>>,
    /// The functions a closure of each number of parameters may hold: every function an atom names.
    held: BTreeMap<usize, BTreeSet<FunctionId>>,
}

impl Cycles {
    fn of(module: &Module, calls: &CallAnalysis) -> Self {
        let mut held: BTreeMap<usize, BTreeSet<FunctionId>> = BTreeMap::new();
        for &function in &calls.escaping {
            if let Some(definition) = module.function(function) {
                held.entry(definition.params.len())
                    .or_default()
                    .insert(function);
            }
        }
        let mut graph = calls.call_graph.clone();
        for (owner, _) in module.live_functions() {
            for node in function_nodes(module, owner) {
                let held = match Reach::of(module.node(node).unwrap()) {
                    Some((Reach::Closure(arity), _)) => held.get(&arity),
                    Some((Reach::Map, _)) => held.get(&1),
                    Some((Reach::Known(_), _)) | None => continue,
                };
                graph
                    .entry(owner)
                    .or_default()
                    .extend(held.into_iter().flatten());
            }
        }
        let sccs = analyze_sccs(&graph);
        Self {
            component_of: sccs.component_of,
            members: sccs.members,
            held,
        }
    }

    /// Every function a call that may reach `reach` may call.
    fn reached(&self, reach: Reach) -> impl Iterator<Item = FunctionId> + '_ {
        let (known, held) = match reach {
            Reach::Known(callee) => (Some(callee), None),
            Reach::Closure(arity) => (None, self.held.get(&arity)),
            Reach::Map => (None, self.held.get(&1)),
        };
        known.into_iter().chain(held.into_iter().flatten().copied())
    }

    /// Whether a call from `owner` that may reach `reach` may close a cycle through `owner`: whether it may call a member of `owner`'s component, `owner` itself among them.
    fn within(&self, owner: FunctionId, reach: Reach) -> bool {
        let Some(component) = self.component_of.get(&owner) else {
            return false;
        };
        self.reached(reach)
            .any(|callee| self.component_of.get(&callee) == Some(component))
    }

    /// The members of `function`'s component.
    fn component(&self, function: FunctionId) -> &[FunctionId] {
        self.component_of
            .get(&function)
            .map_or(&[], |&id| &self.members[id])
    }
}

/// Keep the frames of every recursion on the heap: each member of a call-graph cycle that one of its calls may close without being a tail call runs as its twin, and the member is left the entry into the twin from native code.
pub fn bound_frames(module: &mut Module) {
    let calls = analyze_calls(module);
    let cycles = Cycles::of(module, &calls);

    // The components a call may close other than by a tail call, read off the module as the optimizer left it.
    let mut closing: BTreeSet<FunctionId> = BTreeSet::new();
    for (owner, _) in module.live_functions() {
        let sentinel = module.function(owner).unwrap().return_cont;
        for node in function_nodes(module, owner) {
            if let Some((reach, return_to)) = Reach::of(module.node(node).unwrap())
                && (return_to != sentinel || matches!(reach, Reach::Map))
                && cycles.within(owner, reach)
            {
                closing.insert(owner);
            }
        }
    }

    // A twin is bound beside its member, which every member is but the entry, and nothing calls the entry, so it closes no cycle.
    let binders: BTreeMap<FunctionId, NodeId> = module
        .live_nodes()
        .filter_map(|(node, definition)| match definition {
            Node::LetFun { functions, .. } => Some((node, functions.clone())),
            _ => None,
        })
        .flat_map(|(node, functions)| functions.into_iter().map(move |function| (function, node)))
        .collect();
    let members: BTreeSet<FunctionId> = closing
        .iter()
        .flat_map(|owner| cycles.component(*owner).iter().copied())
        .collect();
    if members.is_empty() {
        return;
    }
    let arities = module.return_arities();

    // Every twin is copied from its member's body before any member is made an entry.
    let twins: BTreeMap<FunctionId, FunctionId> = members
        .iter()
        .map(|&member| {
            let binder = binders
                .get(&member)
                .expect("a function on a cycle is bound by a group");
            (member, copy_alone(module, member, *binder))
        })
        .collect();
    for (&original, &twin) in &twins {
        Twin {
            module,
            original,
            twins: &twins,
            arities: &arities,
            cycles: &cycles,
        }
        .lower(twin);
    }
    for (&member, &twin) in &twins {
        enter(
            module,
            member,
            twin,
            arities.get(&member).copied().unwrap_or(1),
        );
    }

    // What a member's own body held is reached from nowhere now.
    prune_unreachable(module);
    for (member, twin) in twins {
        if module.function(member).is_some() {
            module.record_twin(member, twin);
        }
    }
}

/// Make `member` the entry into `twin` from native code: its body becomes the call of its twin on its own parameters and a continuation that hands the `returns` values back.
fn enter(module: &mut Module, member: FunctionId, twin: FunctionId, returns: usize) {
    let (params, return_cont) = {
        let definition = module.function(member).unwrap();
        (definition.params.clone(), definition.return_cont)
    };
    let (handed, resumed) = handing_back(module, return_cont, returns);
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(twin),
        args: params
            .iter()
            .copied()
            .map(Atom::Value)
            .chain([Atom::Fun(handed)])
            .collect(),
        return_to: resumed,
    });
    let call = match resumed == return_cont {
        true => call,
        false => module.add_node(Node::LetCont {
            continuations: vec![resumed],
            body: call,
        }),
    };
    let body = module.add_node(Node::LetFun {
        functions: vec![handed],
        body: call,
    });
    module.function_mut(member).unwrap().body = body;
}

/// The hint a function of the heap path carries, `heap/` ahead of the name it was copied or lifted from, so a reader of the emitted module tells a heap body from the entry it is entered by.
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
    cycles: &'a Cycles,
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

    /// One terminator of the twin's body, standing in the function whose return is `current`: a jump to a lifted continuation calls its function, a return calls `k`, a call that may close a cycle goes to the heap, and anything else resuming a continuation resumes a thin one that does either.
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
            call @ (Node::ApplyFun { .. }
            | Node::Intrinsic {
                op: IntrinsicCall::ListMap,
                ..
            }) => {
                let (reach, return_to) = Reach::of(&call).expect("a call reaches what it calls");
                let tail = return_to == returned;
                // A tail call hands its own continuation on wherever what it may reach has a twin; any other call goes to the heap where it may close a cycle through this twin's original, handing on the rest of the body as the function its continuation was lifted to.
                let heap = match tail {
                    true => self
                        .cycles
                        .reached(reach)
                        .any(|callee| self.twins.contains_key(&callee)),
                    false => self.cycles.within(self.original, reach),
                };
                if heap {
                    let then = match tail {
                        true => Atom::Value(k),
                        false => Atom::Fun(lifted[&return_to]),
                    };
                    return on_heap(self.module, call, reach, self.twins, then, current);
                }
                let arity = match (&call, tail) {
                    (_, true) => returns,
                    (Node::Intrinsic { .. }, false) => 1,
                    (_, false) => self
                        .module
                        .continuation(return_to)
                        .map_or(1, |continuation| continuation.params.len()),
                };
                self.thin(call, return_to, arity, &to)
            }
            Node::Foreign {
                ref function,
                return_to,
                ..
            } => {
                let arity = function.signature().imported_results().len();
                self.thin(node, return_to, arity, &to)
            }
            Node::Cell { op, return_to, .. } => self.thin(node, return_to, op.result_arity(), &to),
            Node::Channel { op, return_to, .. } => {
                self.thin(node, return_to, op.result_arity(), &to)
            }
            Node::Intrinsic { return_to, .. } => self.thin(node, return_to, 1, &to),
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
        let body = self.module.add_node(resuming(node, thin));
        Node::LetCont {
            continuations: vec![thin],
            body,
        }
    }
}

/// `node`, a call or an operation, resuming `to` in place of its own continuation.
fn resuming(node: Node, to: ContinuationId) -> Node {
    match node {
        Node::ApplyFun { callee, args, .. } => Node::ApplyFun {
            callee,
            args,
            return_to: to,
        },
        Node::Foreign { function, args, .. } => Node::Foreign {
            function,
            args,
            return_to: to,
        },
        Node::Cell { op, args, .. } => Node::Cell {
            op,
            args,
            return_to: to,
        },
        Node::Channel { op, args, .. } => Node::Channel {
            op,
            args,
            return_to: to,
        },
        Node::Intrinsic { op, args, .. } => Node::Intrinsic {
            op,
            args,
            return_to: to,
        },
        other => other,
    }
}

/// What runs `call`, a call that may reach `reach`, on the heap, handing its answer to `then` and resuming `to`: the callee's twin, the closure's heap entry, or a map's loop.
fn on_heap(
    module: &mut Module,
    call: Node,
    reach: Reach,
    twins: &BTreeMap<FunctionId, FunctionId>,
    then: Atom,
    to: ContinuationId,
) -> Node {
    match (reach, call) {
        (Reach::Known(callee), Node::ApplyFun { mut args, .. }) => {
            args.push(then);
            Node::ApplyFun {
                callee: Callee::Known(twins[&callee]),
                args,
                return_to: to,
            }
        }
        (
            Reach::Closure(_),
            Node::ApplyFun {
                callee: Callee::Closure(closure),
                args,
                ..
            },
        ) => Node::Intrinsic {
            op: IntrinsicCall::HeapApply,
            args: std::iter::once(Atom::Value(closure))
                .chain(args)
                .chain([then])
                .collect(),
            return_to: to,
        },
        (Reach::Map, Node::Intrinsic { args, .. }) => {
            let [list, mapper] =
                <[Atom; 2]>::try_from(args).expect("a map takes a list and a mapper");
            heap_map(module, list, mapper, then, to)
        }
        (reach, call) => unreachable!("{reach:?} is not what {call:?} reaches"),
    }
}

/// A list map run on the heap: a loop over the list's positions handing each element to the mapper's heap entry, the rest of the loop as its continuation, and the mapped list to `then` once every element is mapped. The loop resumes `to`.
fn heap_map(module: &mut Module, list: Atom, mapper: Atom, then: Atom, to: ContinuationId) -> Node {
    let looping = module.reserve_function();
    let position = module.add_value(Some("position".to_string()));
    let mapped = module.add_value(Some("mapped".to_string()));
    let k = module.add_value(Some("k".to_string()));
    let returned = module.reserve_continuation();

    // Past the last element: the mapped list, to the continuation.
    let done_body = module.add_node(Node::ApplyFun {
        callee: Callee::Closure(k),
        args: vec![Atom::Value(mapped)],
        return_to: returned,
    });
    let done = module.add_continuation(Continuation {
        debug_name: None,
        params: Vec::new(),
        body: done_body,
    });

    // At an element: its image, to a closure that appends it and loops on.
    let image = module.add_value(None);
    let longer = module.add_value(None);
    let next = module.add_value(None);
    let resumed = module.reserve_continuation();
    let again = module.add_node(Node::ApplyFun {
        callee: Callee::Known(looping),
        args: vec![Atom::Value(next), Atom::Value(longer), Atom::Value(k)],
        return_to: resumed,
    });
    let step = module.add_node(Node::LetIntrinsic {
        result: next,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Value(position),
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
        ],
        next: again,
    });
    let append = module.add_node(Node::LetIntrinsic {
        result: longer,
        op: Intrinsic::ListAppend,
        args: vec![Atom::Value(mapped), Atom::Value(image)],
        next: step,
    });
    let resume = module.add_function(Function {
        debug_name: Some(heap_named(Some("map/resume"))),
        params: vec![image],
        return_cont: resumed,
        body: append,
        droppable: false,
    });
    let element = module.add_value(None);
    let apply = module.add_node(Node::Intrinsic {
        op: IntrinsicCall::HeapApply,
        args: vec![mapper, Atom::Value(element), Atom::Fun(resume)],
        return_to: returned,
    });
    let bound = module.add_node(Node::LetFun {
        functions: vec![resume],
        body: apply,
    });
    let read = module.add_node(Node::LetIntrinsic {
        result: element,
        op: Intrinsic::ListGet,
        args: vec![list.clone(), Atom::Value(position)],
        next: bound,
    });
    let more = module.add_continuation(Continuation {
        debug_name: None,
        params: Vec::new(),
        body: read,
    });

    let length = module.add_value(None);
    let below = module.add_value(None);
    let switch = module.add_node(Node::Switch {
        scrutinee: Atom::Value(below),
        cases: BTreeMap::from([(
            1,
            Edge {
                target: more,
                args: Vec::new(),
            },
        )]),
        default: Some(Edge {
            target: done,
            args: Vec::new(),
        }),
    });
    let branch = module.add_node(Node::LetCont {
        continuations: vec![more, done],
        body: switch,
    });
    let compare = module.add_node(Node::LetIntrinsic {
        result: below,
        op: Intrinsic::NatLt,
        args: vec![Atom::Value(position), Atom::Value(length)],
        next: branch,
    });
    let body = module.add_node(Node::LetIntrinsic {
        result: length,
        op: Intrinsic::ListLen,
        args: vec![list],
        next: compare,
    });
    module.define_function(
        looping,
        Function {
            debug_name: Some(heap_named(Some("map"))),
            params: vec![position, mapped, k],
            return_cont: returned,
            body,
            droppable: false,
        },
    );

    let empty = module.add_value(None);
    let start = module.add_node(Node::ApplyFun {
        callee: Callee::Known(looping),
        args: vec![
            Atom::Literal(Literal::Nat(Natural::zero())),
            Atom::Value(empty),
            then,
        ],
        return_to: to,
    });
    let start = module.add_node(Node::LetValue {
        result: empty,
        value: ValueExpr::List(Vec::new()),
        next: start,
    });
    Node::LetFun {
        functions: vec![looping],
        body: start,
    }
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
