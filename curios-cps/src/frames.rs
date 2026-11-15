//! A recursion's frames kept on the heap: the rule that a recursion is as deep as the heap allows ([`documentation/design/compilation/a-recursion-is-as-deep-as-the-heap-allows.md`](../../documentation/design/compilation/a-recursion-is-as-deep-as-the-heap-allows.md)).
//!
//! **Only a cycle grows the stack with the input.** A chain of calls on no cycle of the call graph is as deep as the program text allows, so the calls [`bound_frames`] guards are the calls that may close a cycle, a strongly connected component, through the function making them, other than by a tail call. The graph is the known calls and, for a call through a closure or a list map, an edge to every function its callee or its mapper may be ([`ValueFlow`]), every function some atom names that takes as many parameters where that may be any. Twins copy calls at fresh identities, and each copy answers by the site it was copied from. A tail call keeps no frame and is left alone; a list map keeps its helper's frame under every element, so it is guarded wherever it stands.
//!
//! **A guarded call asks the frame budget first.** [`IntrinsicCall::FrameEnter`] takes the callee's weight from one budget the whole program shares, where it fits, and the call runs natively as it is written, handing the weight back by [`IntrinsicCall::FrameLeave`] once it returns; where it does not fit, the call goes to the heap with a continuation that hands the answer straight back, so everything deeper runs on the heap inside that one native call. The native code is the code as the optimizer left it, the guard aside. A known callee's heap is its twin; a closure's is the entry [`IntrinsicCall::HeapApply`] reaches, which the emitter dispatches beside the closure's own; a list map's is a loop of those ([`heap_map`]).
//!
//! **A twin runs its function on the heap.** It is a copy of the function's body taking one more parameter, the continuation `k` its answer goes to, and a return becomes a tail call of `k`. A call in it that may close the cycle goes to the heap, handing over the rest of the body as a closure, and a tail call does where what it may reach has a twin, handing `k` on; a call that closes no cycle stays native, its frame popped before the twin goes on. A continuation such a call resumes is lifted to a function bound where its `LetCont` stood, and so is each one such a function's body reaches outside itself, so the rest of the body after the call is a function, its scope the scope the continuation had, and a jump to it becomes a tail call; every other continuation stays one, a loop's among them. [`Module::twin`] names each function's twin, which is what a closure's heap entry enters.
//!
//! **A nested component goes first.** A member defined within another is guarded and twinned before the member around it is copied, so the copy carries it guarded and with its twin, which the copy's own twin names.
//!
//! **It runs after the rebuild in place.** A twin is a copy of a body the rebuild has proved, and a call runs it or its original, never both; the heap path moves no read past another and adds no reader of any value, a value live across a call held by the continuation it hands on, which runs once, where the native frame held it. So every rebuild keeps the conditions it was proved under on the native code, where the proof is exact.
//!
//! **A frame's weight is a stand-in for its bytes.** No stage reads an engine's frame size, so a call weighs a fixed overhead plus one for each value its callee's body binds, the slots its locals are allocated from ([`frame_weight`]); a call that may reach several weighs the heaviest. The budget is an assumption about the engines a module runs on, stated with its margin in the decision this module implements: where an engine's stack holds less than the budget's frames, a recursion stops on the engine's own trap. The heap path stands in for a fresh stack segment, which a module will switch to once the engines it runs on switch stacks.

#[cfg(test)]
mod tests;

use {
    super::{
        Atom, CallAnalysis, Callee, Continuation, ContinuationId, Copies, CopyPlan, Droppable,
        Edge, Function, FunctionId, Intrinsic, IntrinsicCall, Literal, Mapping, MayBe, Module,
        Node, NodeId, ValueExpr, ValueFlow, ValueId, analyze_calls, analyze_sccs, clone_node,
        copy_bodies, function_nodes, prune_unreachable,
    },
    curios_num::Natural,
    std::collections::{BTreeMap, BTreeSet},
};

/// What the native frames of the calls the budget admits may weigh at once across the whole program, a frame weighing a fixed overhead and one for each value its function's body binds: a small part of a page's stack for the code with the fattest frames a module runs as, the module Binaryen has not optimized, by the protocol and the figures of [A recursion is as deep as the heap allows](../../documentation/design/compilation/a-recursion-is-as-deep-as-the-heap-allows.md).
pub const FRAME_BUDGET: u32 = 4_096;

/// What a frame weighs before the values its body binds.
const FRAME_OVERHEAD: u32 = 20;

/// What a call may reach: the function it names, or what the callee of the closure call or the mapper of the list map at the node may be ([`Cycles::closure`]).
#[derive(Debug, Clone, Copy)]
enum Reach {
    Known(FunctionId),
    Closure(usize, NodeId),
    Map(NodeId),
}

impl Reach {
    /// What `node` may call and the continuation it resumes, where it is a call.
    fn of(id: NodeId, node: &Node) -> Option<(Self, ContinuationId)> {
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
            } => Some((Reach::Closure(args.len(), id), *return_to)),
            Node::Intrinsic {
                op: IntrinsicCall::ListMap,
                return_to,
                ..
            } => Some((Reach::Map(id), *return_to)),
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
    /// The original of each copy of a member a twin holds, which the copy stands for on its cycle.
    copied: BTreeMap<FunctionId, FunctionId>,
    /// What the callee of each closure call and the mapper of each list map may be, read before anything is copied.
    callees: BTreeMap<NodeId, MayBe<FunctionId>>,
    /// The site each call minted at a fresh identity stands for: a copy of a body's, the native call a guard reissues, a twin's call resumed through a thin continuation.
    origin: BTreeMap<NodeId, NodeId>,
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
        let flow = ValueFlow::of(module);
        let callees = module
            .live_nodes()
            .filter_map(|(id, node)| {
                let callee = match node {
                    Node::ApplyFun {
                        callee: Callee::Closure(closure),
                        ..
                    } => Atom::Value(*closure),
                    Node::Intrinsic {
                        op: IntrinsicCall::ListMap,
                        args,
                        ..
                    } => args[1].clone(),
                    _ => return None,
                };
                Some((id, flow.functions(&callee)))
            })
            .collect();
        let mut cycles = Self {
            component_of: BTreeMap::new(),
            members: Vec::new(),
            held,
            copied: BTreeMap::new(),
            callees,
            origin: BTreeMap::new(),
        };
        let mut graph = calls.call_graph.clone();
        for (owner, _) in module.live_functions() {
            for node in function_nodes(module, owner) {
                if let Some((reach @ (Reach::Closure(..) | Reach::Map(_)), _)) =
                    Reach::of(node, module.node(node).unwrap())
                {
                    let reached = cycles.reached(reach);
                    graph.entry(owner).or_default().extend(reached);
                }
            }
        }
        let sccs = analyze_sccs(&graph);
        cycles.component_of = sccs.component_of;
        cycles.members = sccs.members;
        cycles
    }

    /// The functions a closure call of `arity` parameters at `site` may reach: what its callee may be, or every function some atom names that takes as many where that is any. A site no analysis read and no copy names answers as any.
    fn closure(&self, arity: usize, site: NodeId) -> Vec<FunctionId> {
        let held = self.held.get(&arity);
        let site = self.origin.get(&site).copied().unwrap_or(site);
        match self.callees.get(&site) {
            Some(MayBe::Of(functions)) => functions
                .iter()
                .copied()
                .filter(|function| held.is_some_and(|held| held.contains(function)))
                .collect(),
            Some(MayBe::Any) | None => held.into_iter().flatten().copied().collect(),
        }
    }

    /// Record that the call at `copy` stands for the site the call at `original` stands for.
    fn copied_node(&mut self, original: NodeId, copy: NodeId) {
        let origin = self.origin.get(&original).copied().unwrap_or(original);
        self.origin.insert(copy, origin);
    }

    /// The member `function` stands for: itself, or the original a copy was made of.
    fn original(&self, function: FunctionId) -> FunctionId {
        self.copied.get(&function).copied().unwrap_or(function)
    }

    /// Every function a call that may reach `reach` may call, a copy by the member it stands for.
    fn reached(&self, reach: Reach) -> Vec<FunctionId> {
        match reach {
            Reach::Known(callee) => vec![self.original(callee)],
            Reach::Closure(arity, site) => self.closure(arity, site),
            Reach::Map(site) => self.closure(1, site),
        }
    }

    /// Whether a call from `owner` that may reach `reach` may close a cycle through `owner`: whether it may call a member of `owner`'s component, `owner` itself among them.
    fn within(&self, owner: FunctionId, reach: Reach) -> bool {
        let Some(component) = self.component_of.get(&self.original(owner)) else {
            return false;
        };
        self.reached(reach)
            .into_iter()
            .any(|callee| self.component_of.get(&callee) == Some(component))
    }

    /// Whether `a` and `b` stand for members of one component.
    fn together(&self, a: FunctionId, b: FunctionId) -> bool {
        let component = |function| self.component_of.get(&self.original(function));
        component(a).is_some() && component(a) == component(b)
    }

    /// The members of `function`'s component.
    fn component(&self, function: FunctionId) -> &[FunctionId] {
        self.component_of
            .get(&self.original(function))
            .map_or(&[], |&id| &self.members[id])
    }
}

/// Bound the native frames a recursion holds: guard each call that may close a cycle of the call graph without being a tail call, native while the frame budget admits it and to the callee's twin past it, and give every member of a guarded cycle its twin.
pub fn bound_frames(module: &mut Module) {
    let calls = analyze_calls(module);
    let mut cycles = Cycles::of(module, &calls);

    // The components a call may close other than by a tail call, read off the module as the optimizer left it.
    let mut components: Vec<Vec<FunctionId>> = module
        .live_functions()
        .map(|(owner, _)| owner)
        .filter(|&owner| !closing_sites(module, &cycles, owner).is_empty())
        .map(|owner| cycles.component(owner).to_vec())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();
    if components.is_empty() {
        return;
    }
    let arities = module.return_arities();
    let mut weights: BTreeMap<FunctionId, u32> = components
        .iter()
        .flatten()
        .map(|&member| (member, frame_weight(module, member)))
        .collect();

    // A component nested deeper goes first, so a twin copied from a function another member is defined within copies that member guarded and with its own twin beside it.
    let binders = binders(module);
    let depth = |function: FunctionId| {
        let mut depth = 0usize;
        let mut current = function;
        while let Some(owner) = binders
            .get(&current)
            .and_then(|binder| calls.node_owners.get(binder))
        {
            depth += 1;
            current = *owner;
        }
        depth
    };
    components.sort_by_key(|component| {
        std::cmp::Reverse(component.iter().map(|&member| depth(member)).max())
    });

    let mut twins: BTreeMap<FunctionId, FunctionId> = BTreeMap::new();
    for component in components {
        let mut batch = component;
        while !batch.is_empty() {
            batch = round(
                module,
                &mut cycles,
                &mut twins,
                &mut weights,
                &arities,
                batch,
            );
        }
    }

    // A twin a round copied before it was lowered is reached from nowhere, the copy of its member it stood beside twinned in a round of its own; a twin only a closure's heap entry enters is reached through its function, which the prune follows.
    for (function, twin) in twins {
        module.record_twin(function, twin);
    }
    prune_unreachable(module);
}

/// Twin and guard `batch`, members of one component or copies standing for them, all copied before any is lowered or guarded, so no twin carries a guard of its own cycle; and answer the copies of the component's members the twins hold, which the next round takes.
fn round(
    module: &mut Module,
    cycles: &mut Cycles,
    twins: &mut BTreeMap<FunctionId, FunctionId>,
    weights: &mut BTreeMap<FunctionId, u32>,
    arities: &BTreeMap<FunctionId, usize>,
    batch: Vec<FunctionId>,
) -> Vec<FunctionId> {
    let binders = binders(module);
    let earlier: BTreeSet<FunctionId> = twins.keys().copied().collect();
    let mut next = Vec::new();
    for &function in &batch {
        weights
            .entry(function)
            .or_insert_with(|| frame_weight(module, function));
        let binder = binders
            .get(&function)
            .expect("a function on a cycle is bound by a group");
        let (twin, copies) = copy_alone(module, function, *binder);
        for (&original, &copy) in &copies.nodes {
            cycles.copied_node(original, copy);
        }
        twins.insert(function, twin);
        for (&nested, &copy) in &copies.functions {
            if nested == function {
                continue;
            }
            // A member twinned in an earlier round is copied with its lowered twin, which the copy is entered as; a member of this component is copied as it was written, and twinned in a round of its own.
            match twins.get(&nested) {
                Some(nested_twin) if earlier.contains(&nested) => {
                    if let Some(&twin_copy) = copies.functions.get(nested_twin) {
                        twins.insert(copy, twin_copy);
                    }
                }
                _ if cycles.together(nested, function) => {
                    let original = cycles.original(nested);
                    cycles.copied.insert(copy, original);
                    next.push(copy);
                }
                _ => {}
            }
        }
    }
    for &original in &batch {
        let mut moved = Vec::new();
        Twin {
            module,
            original,
            twins,
            arities,
            cycles,
            moved: &mut moved,
        }
        .lower(twins[&original]);
        for (original, copy) in moved {
            cycles.copied_node(original, copy);
        }
    }
    for &function in &batch {
        for (node, reach) in closing_sites(module, cycles, function) {
            // A known callee goes to the heap through its twin, and a copy not twinned yet is called natively, guarding itself.
            if let Reach::Known(callee) = reach
                && !twins.contains_key(&callee)
            {
                continue;
            }
            // A call that may reach several members weighs the heaviest, and a map its helper's frame besides.
            let weight = cycles
                .reached(reach)
                .into_iter()
                .filter_map(|callee| weights.get(&callee).copied())
                .max()
                .unwrap_or(FRAME_OVERHEAD);
            let weight = match reach {
                Reach::Map(_) => weight.saturating_add(FRAME_OVERHEAD),
                Reach::Known(_) | Reach::Closure(..) => weight,
            };
            let returns = match reach {
                Reach::Known(callee) => arities.get(&cycles.original(callee)).copied().unwrap_or(1),
                Reach::Closure(..) | Reach::Map(_) => 1,
            };
            let native = guard(
                module,
                node,
                reach,
                twins,
                weight.min(FRAME_BUDGET / 4),
                returns,
            );
            cycles.copied_node(node, native);
        }
    }
    next
}

/// The calls of `owner`'s body that may close its cycle other than by a tail call, each with what it may reach; a list map keeps its helper's frame in tail position too.
fn closing_sites(module: &Module, cycles: &Cycles, owner: FunctionId) -> Vec<(NodeId, Reach)> {
    let sentinel = module.function(owner).unwrap().return_cont;
    function_nodes(module, owner)
        .into_iter()
        .filter_map(|node| {
            let (reach, return_to) = Reach::of(node, module.node(node).unwrap())?;
            ((return_to != sentinel || matches!(reach, Reach::Map(_)))
                && cycles.within(owner, reach))
            .then_some((node, reach))
        })
        .collect()
}

/// The group each function is bound in. A twin is bound beside its original, which every member is but the entry, and nothing calls the entry, so it closes no cycle.
fn binders(module: &Module) -> BTreeMap<FunctionId, NodeId> {
    module
        .live_nodes()
        .filter_map(|(node, definition)| match definition {
            Node::LetFun { functions, .. } => Some((node, functions.clone())),
            _ => None,
        })
        .flat_map(|(node, functions)| functions.into_iter().map(move |function| (function, node)))
        .collect()
}

/// A copy of `function` and everything defined within it, at fresh identities, bound in the group `function` is bound in, with the copy of each function and each node the extent holds. Every copied node names `function` where the original did, and not the copy: the copy's own calls are rewritten into its twin's by [`Twin`], which reads them off the originals they name, and a function nested in it is an ordinary function, guarded and twinned already where it is a member.
fn copy_alone(module: &mut Module, function: FunctionId, binder: NodeId) -> (FunctionId, Copies) {
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

    (copy, copies)
}

/// Guard the call at `node`, which may reach `reach` and answers `returns` values, its frame weighing `weight`: native as it is written where the budget admits it, handing the weight back once it returns, and otherwise on the heap with a continuation that hands its answer back. Answers the node the native call is reissued at.
fn guard(
    module: &mut Module,
    node: NodeId,
    reach: Reach,
    twins: &BTreeMap<FunctionId, FunctionId>,
    weight: u32,
    returns: usize,
) -> NodeId {
    let call = module.node(node).unwrap().clone();
    let (_, return_to) = Reach::of(node, &call).expect("a guarded site is a call");

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
    let native_call = module.add_node(resuming(call.clone(), leave));
    let native = module.add_continuation(Continuation {
        debug_name: None,
        params: Vec::new(),
        body: native_call,
    });

    // Heap: handed a continuation that answers what the call would have.
    let (handed, resumed) = handing_back(module, return_to, returns);
    let heap_call = on_heap(module, call, reach, twins, Atom::Fun(handed), resumed);
    let heap_call = module.add_node(heap_call);
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
    native_call
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

/// The hint a function of the heap path carries, `heap/` ahead of the name it was copied or lifted from, so a reader of the emitted module tells a heap body from the entry it is entered by.
fn heap_named(name: Option<&str>) -> String {
    format!("heap/{}", name.unwrap_or_default())
}

/// The rewrite of one function's copy into its twin.
struct Twin<'a> {
    module: &'a mut Module,
    /// The function the copy was made of.
    original: FunctionId,
    twins: &'a BTreeMap<FunctionId, FunctionId>,
    arities: &'a BTreeMap<FunctionId, usize>,
    cycles: &'a Cycles,
    /// Each call the rewrite reissues at a fresh node, by the node it stood at, so [`Cycles`] answers it by its site.
    moved: &'a mut Vec<(NodeId, NodeId)>,
}

impl Twin<'_> {
    /// Whether a call that may reach `reach` closes the cycle through [`Twin::original`] on the heap: it may reach a member of the cycle, and a known callee has a twin to go to, a copy not twinned yet being called natively, where it guards itself.
    fn closes(&self, reach: Reach) -> bool {
        self.cycles.within(self.original, reach)
            && match reach {
                Reach::Known(callee) => self.twins.contains_key(&callee),
                Reach::Closure(..) | Reach::Map(_) => true,
            }
    }

    /// Rewrite `twin`, a copy of [`Twin::original`], into the twin that runs it on the heap.
    fn lower(mut self, twin: FunctionId) {
        let k = self.module.add_value(Some("k".to_string()));
        let (sentinel, body) = {
            let definition = self.module.function_mut(twin).unwrap();
            definition.params.push(k);
            definition.droppable = Droppable::Never;
            definition.debug_name = Some(heap_named(definition.debug_name.as_deref()));
            (definition.return_cont, definition.body)
        };
        let returns = self
            .arities
            .get(&self.cycles.original(self.original))
            .copied()
            .unwrap_or(1);
        let lifting = self.liftings(body, sentinel);

        // Each continuation that must be a function, lifted to the function bound in its place; the rest stay continuations.
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
                    let (lift, keep): (Vec<ContinuationId>, Vec<ContinuationId>) = continuations
                        .iter()
                        .partition(|continuation| lifting.contains(continuation));
                    for &id in &keep {
                        work.push((self.module.continuation(id).unwrap().body, current));
                    }
                    work.push((body, current));
                    if lift.is_empty() {
                        continue;
                    }
                    let functions = lift
                        .iter()
                        .map(|&id| {
                            let continuation = self.module.continuation(id).unwrap().clone();
                            let return_cont = self.module.reserve_continuation();
                            let function = self.module.add_function(Function {
                                debug_name: Some(heap_named(continuation.debug_name.as_deref())),
                                params: continuation.params.clone(),
                                return_cont,
                                body: continuation.body,
                                droppable: Droppable::Never,
                            });
                            lifted.insert(id, function);
                            work.push((continuation.body, return_cont));
                            function
                        })
                        .collect();
                    // A lifted function reaches no continuation bound beside it, so the ones kept are bound inside the group, where the body and they themselves call the functions.
                    let body = match keep.is_empty() {
                        true => body,
                        false => self.module.add_node(Node::LetCont {
                            continuations: keep,
                            body,
                        }),
                    };
                    self.module.set_node(node, Node::LetFun { functions, body });
                }
                _ => {
                    let rewritten = self.terminator(node, sentinel, current, k, returns, &lifted);
                    self.module.set_node(node, rewritten);
                }
            }
        }

        // The lifted continuations are bound nowhere now, and the rewrite read their arity until it was done.
        for continuation in lifted.into_keys() {
            self.module.remove_continuation(continuation);
        }
    }

    /// The continuations of the twin's body that must become functions: each one a call that may close the cycle resumes, since the heap hands it on as a closure, and each one a function's body reaches from inside it while it is bound outside, since a function jumps to no continuation but its own. Everything else stays a continuation, a loop's among them.
    fn liftings(&self, body: NodeId, returned: ContinuationId) -> BTreeSet<ContinuationId> {
        let mut lifting = BTreeSet::new();
        loop {
            let before = lifting.len();
            // Where each continuation is bound and every node is: the twin's own body, or the lifted continuation whose body holds it.
            let mut bound_in: BTreeMap<ContinuationId, Option<ContinuationId>> = BTreeMap::new();
            let mut reached: Vec<(Option<ContinuationId>, ContinuationId)> = Vec::new();
            let mut work = vec![(body, None)];
            while let Some((node, region)) = work.pop() {
                match self.module.node(node).unwrap() {
                    Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. } => {
                        work.push((*next, region));
                    }
                    Node::LetFun { body, .. } => work.push((*body, region)),
                    Node::LetCont {
                        continuations,
                        body,
                    } => {
                        for &continuation in continuations {
                            bound_in.insert(continuation, region);
                            let inner = match lifting.contains(&continuation) {
                                true => Some(continuation),
                                false => region,
                            };
                            work.push((
                                self.module.continuation(continuation).unwrap().body,
                                inner,
                            ));
                        }
                        work.push((*body, region));
                    }
                    terminator => {
                        if let Some((reach, return_to)) = Reach::of(node, terminator)
                            && return_to != returned
                            && self.closes(reach)
                        {
                            lifting.insert(return_to);
                        }
                        reached.extend(
                            resumed(terminator)
                                .into_iter()
                                .filter(|&target| target != returned)
                                .map(|target| (region, target)),
                        );
                    }
                }
            }
            for (region, target) in reached {
                if bound_in.get(&target).is_some_and(|&bound| bound != region) {
                    lifting.insert(target);
                }
            }
            if lifting.len() == before {
                return lifting;
            }
        }
    }

    /// The terminator at `id` of the twin's body, standing in the function whose return is `current`: a jump to a lifted continuation calls its function, a return calls `k`, a call that may close a cycle goes to the heap, anything resuming a lifted continuation or the return resumes a thin one that does either, and anything else stays as it is.
    fn terminator(
        &mut self,
        id: NodeId,
        returned: ContinuationId,
        current: ContinuationId,
        k: ValueId,
        returns: usize,
        lifted: &BTreeMap<ContinuationId, FunctionId>,
    ) -> Node {
        let node = self.module.node(id).unwrap().clone();
        let moved = |target: ContinuationId| target == returned || lifted.contains_key(&target);
        let to = |target: ContinuationId, args: Vec<Atom>| -> Node {
            match (target == returned, lifted.get(&target)) {
                (true, _) => Node::ApplyFun {
                    callee: Callee::Closure(k),
                    args,
                    return_to: current,
                },
                (false, Some(&function)) => Node::ApplyFun {
                    callee: Callee::Known(function),
                    args,
                    return_to: current,
                },
                (false, None) => Node::ApplyCont(Edge { target, args }),
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
                    if !moved(edge.target) {
                        return edge;
                    }
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
                let switch = Node::Switch {
                    scrutinee,
                    cases,
                    default,
                };
                if thins.is_empty() {
                    return switch;
                }
                let switch = self.module.add_node(switch);
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
                let (reach, return_to) =
                    Reach::of(id, &call).expect("a call reaches what it calls");
                let tail = return_to == returned;
                // A tail call hands its own continuation on wherever what it may reach has a twin; any other call goes to the heap where it may close the cycle through this twin's original, handing on the rest of the body as the function its continuation was lifted to.
                let heap = match (tail, reach) {
                    (true, Reach::Known(callee)) => self.twins.contains_key(&callee),
                    (true, _) => self
                        .cycles
                        .reached(reach)
                        .into_iter()
                        .any(|callee| self.twins.contains_key(&callee)),
                    (false, _) => self.closes(reach),
                };
                if heap {
                    let then = match tail {
                        true => Atom::Value(k),
                        false => Atom::Fun(lifted[&return_to]),
                    };
                    return on_heap(self.module, call, reach, self.twins, then, current);
                }
                if !moved(return_to) {
                    return call;
                }
                let arity = match (&call, tail) {
                    (_, true) => returns,
                    (Node::Intrinsic { .. }, false) => 1,
                    (_, false) => self
                        .module
                        .continuation(return_to)
                        .map_or(1, |continuation| continuation.params.len()),
                };
                self.thin(id, call, return_to, arity, &to)
            }
            Node::Foreign { return_to, .. }
            | Node::Cell { return_to, .. }
            | Node::Channel { return_to, .. }
            | Node::Intrinsic { return_to, .. }
                if !moved(return_to) =>
            {
                node
            }
            Node::Foreign {
                ref function,
                return_to,
                ..
            } => {
                let arity = function.signature().imported_results().len();
                self.thin(id, node, return_to, arity, &to)
            }
            Node::Cell { op, return_to, .. } => {
                self.thin(id, node, return_to, op.result_arity(), &to)
            }
            Node::Channel { op, return_to, .. } => {
                self.thin(id, node, return_to, op.result_arity(), &to)
            }
            Node::Intrinsic { return_to, .. } => self.thin(id, node, return_to, 1, &to),
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
        id: NodeId,
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
        self.moved.push((id, body));
        Node::LetCont {
            continuations: vec![thin],
            body,
        }
    }
}

/// Every continuation the terminator `node` transfers to: what it jumps to, each edge of a switch, or what a call or an operation resumes.
fn resumed(node: &Node) -> Vec<ContinuationId> {
    match node {
        Node::ApplyCont(edge) => vec![edge.target],
        Node::Switch { cases, default, .. } => cases
            .values()
            .chain(default)
            .map(|edge| edge.target)
            .collect(),
        Node::ApplyFun { return_to, .. }
        | Node::Foreign { return_to, .. }
        | Node::Cell { return_to, .. }
        | Node::Channel { return_to, .. }
        | Node::Intrinsic { return_to, .. } => vec![*return_to],
        Node::Halt { .. }
        | Node::Panic(_)
        | Node::Unreachable
        | Node::LetValue { .. }
        | Node::LetIntrinsic { .. }
        | Node::LetFun { .. }
        | Node::LetCont { .. } => Vec::new(),
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
            Reach::Closure(..),
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
        (Reach::Map(_), Node::Intrinsic { args, .. }) => {
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
        droppable: Droppable::Never,
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
            droppable: Droppable::Never,
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
        droppable: Droppable::Never,
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
