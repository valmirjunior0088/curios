//! Private closed control-flow graph produced after high-CPS optimization.
//!
//! Functions have explicit blocks and block parameters. The CPS return continuation never becomes a block: a jump to the current function's bodyless sentinel is translated directly to [`MachineTerminator::Return`].

use {
    curios_abi::ForeignFunction,
    curios_utilities::{Entropy, id},
    std::{
        collections::{BTreeMap, BTreeSet, VecDeque},
        fmt,
        sync::Arc,
    },
};

mod structurize;
pub(crate) use structurize::*;

// Sigils follow the naming scheme shared with `curios-anf` and `curios-wasm` — see `documentation/design/tools/a-printer-states-each-fact-once-where-it-is-bound.md`.
id!(MachineBlockId, "~b", mint);
id!(MachineValueId, "~v", mint);

#[derive(Debug, Clone)]
pub(crate) enum MachineOperand {
    Value(MachineValueId),
    Literal(curios_cps::Literal),
    /// [`curios_cps::Atom::Filler`] carried through unchanged: null, unless the destination turns out to be a register-held parameter, which is known only once `represent` has run.
    Filler,
}

#[derive(Debug, Clone)]
pub(crate) enum MachineConstruct {
    Literal(curios_cps::Literal),
    List(Vec<MachineOperand>),
    Tuple(Vec<MachineOperand>),
    Row(curios_cps::RowId, Vec<MachineOperand>),
}

#[derive(Debug, Clone)]
pub(crate) enum MachineInstruction {
    Construct {
        result: MachineValueId,
        value: MachineConstruct,
    },
    Intrinsic {
        result: MachineValueId,
        op: curios_cps::Intrinsic,
        args: Vec<MachineOperand>,
    },
    /// Retained-ABI closure wrapper around direct code. The wrapper unpacks these captures and tail-calls `function`.
    MakeClosure {
        result: MachineValueId,
        function: curios_cps::FunctionId,
        captures: Vec<MachineOperand>,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct MachineEdge {
    target: MachineBlockId,
    args: Vec<MachineOperand>,
}

#[derive(Debug, Clone)]
pub(crate) enum MachineTerminator {
    /// Hand every operand back to the caller. The vector is the return continuation's argument list, which the CPS verifier does not constrain to one — so a protocol delivering a constructor as its fields needs no widening here, only a producer that builds more than one.
    Return(Vec<MachineOperand>),
    Jump(MachineEdge),
    Switch {
        scrutinee: MachineOperand,
        cases: BTreeMap<u32, MachineEdge>,
        default: Option<MachineEdge>,
    },
    DirectCall {
        function: curios_cps::FunctionId,
        args: Vec<MachineOperand>,
        resume: MachineBlockId,
    },
    TailDirectCall {
        function: curios_cps::FunctionId,
        args: Vec<MachineOperand>,
    },
    IndirectCall {
        closure: MachineValueId,
        args: Vec<MachineOperand>,
        resume: MachineBlockId,
    },
    TailIndirectCall {
        closure: MachineValueId,
        args: Vec<MachineOperand>,
    },
    Foreign {
        function: Arc<ForeignFunction>,
        args: Vec<MachineOperand>,
        resume: MachineBlockId,
    },
    ForeignReturn {
        function: Arc<ForeignFunction>,
        args: Vec<MachineOperand>,
    },
    Cell {
        op: curios_cps::CellOp,
        args: Vec<MachineOperand>,
        resume: MachineBlockId,
    },
    CellReturn {
        op: curios_cps::CellOp,
        args: Vec<MachineOperand>,
    },
    Channel {
        op: curios_cps::ChannelOp,
        args: Vec<MachineOperand>,
        resume: MachineBlockId,
    },
    ChannelReturn {
        op: curios_cps::ChannelOp,
        args: Vec<MachineOperand>,
    },
    Intrinsic {
        op: curios_cps::IntrinsicCall,
        args: Vec<MachineOperand>,
        resume: MachineBlockId,
    },
    IntrinsicReturn {
        op: curios_cps::IntrinsicCall,
        args: Vec<MachineOperand>,
    },
    /// A call to a host row that diverges; see [`curios_cps::Node::Halt`].
    Halt {
        function: Arc<ForeignFunction>,
        args: Vec<MachineOperand>,
    },
    /// A deliberate failure of the given class; see [`curios_cps::Node::Panic`].
    Panic(curios_cps::Panic),
    Unreachable,
}

#[derive(Debug, Clone)]
pub(crate) struct MachineBlock {
    params: Vec<MachineValueId>,
    instructions: Vec<MachineInstruction>,
    terminator: MachineTerminator,
}

#[derive(Debug, Clone)]
pub(crate) struct MachineFunction {
    free_values: Vec<MachineValueId>,
    params: Vec<MachineValueId>,
    /// What every `Return` in this function carries, one carrier a value — a register's, or `None` for a reference — read off the representation decided over the CPS module rather than recounted here: a caller has to know it to size the block that resumes the call and to receive each value as it is held, and a block cannot be sized from a terminator it has not reached yet.
    results: Vec<Option<curios_cps::Repr>>,
    entry: MachineBlockId,
    blocks: BTreeMap<MachineBlockId, MachineBlock>,
    block_scopes: BTreeMap<MachineBlockId, Vec<MachineBlockId>>,
}

#[derive(Debug, Clone)]
pub(crate) struct MachineWrapper {
    function: curios_cps::FunctionId,
    captures: Vec<MachineValueId>,
    arity: usize,
}

#[derive(Debug, Clone)]
pub(crate) struct MachineModule {
    functions: BTreeMap<curios_cps::FunctionId, MachineFunction>,
    wrappers: BTreeMap<curios_cps::FunctionId, MachineWrapper>,
    entry: curios_cps::FunctionId,
    /// Each function's source hint, carried from the CPS module so emission names can spell a function's origin (`func/{index}$hint`). A hint never affects identity — the `curios_cps::FunctionId` index does — so a missing entry only omits the hint.
    function_hints: BTreeMap<curios_cps::FunctionId, String>,
    /// Every nominal row the CPS module declared, with its debug name and slot carriers — carried whole rather than collected from constructions, so a row whose constructions were all optimized away still declares a type for the projections that may outlive them.
    rows: Vec<(curios_cps::RowId, curios_cps::Row)>,
    /// The suspension row only when CPS's residual return flow admits it at the entry; the complete row roster is independent.
    entry_suspension: Option<curios_cps::RowId>,
}

impl MachineModule {
    /// The source hint of a function, if it carries one.
    pub(crate) fn function_hint(&self, id: curios_cps::FunctionId) -> Option<&str> {
        self.function_hints.get(&id).map(String::as_str)
    }

    pub(crate) fn rows(&self) -> &[(curios_cps::RowId, curios_cps::Row)] {
        &self.rows
    }

    pub(crate) fn entry_suspension(&self) -> Option<curios_cps::RowId> {
        self.entry_suspension
    }
}

#[derive(Debug)]
pub(crate) struct MachineVerifyError(String);

impl fmt::Display for MachineVerifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

/// Lower `source` to the closed machine CFG, each function handing back its results at the storage `results` decided for them.
pub(crate) fn lower(
    source: &curios_cps::Module,
    results: &BTreeMap<curios_cps::FunctionId, Vec<curios_cps::Storage>>,
) -> MachineModule {
    source
        .verify()
        .expect("invalid high CPS at machine boundary");
    let mut free_values = source
        .functions()
        .iter()
        .enumerate()
        .filter_map(|(index, function)| {
            function.as_ref()?;
            let function = curios_cps::FunctionId::from_index(index);
            Some((function, free_runtime_values(source, function)))
        })
        .collect::<BTreeMap<_, _>>();
    let owned_values = free_values
        .keys()
        .copied()
        .map(|function| (function, owned_runtime_values(source, function)))
        .collect::<BTreeMap<_, _>>();
    let dependencies = free_values
        .keys()
        .copied()
        .map(|function| (function, referenced_functions(source, function)))
        .collect::<BTreeMap<_, _>>();
    // A function needs what it reads and what each function it reaches needs, less what it binds itself, to a least fixpoint taken a function at a time: one is asked again only where what a function it reaches needs has grown. Asked whole until a round changes nothing, a nesting of n functions, each reaching the one inside it, would take n rounds over every function.
    let mut dependents = BTreeMap::<curios_cps::FunctionId, Vec<curios_cps::FunctionId>>::new();
    for (function, reached) in &dependencies {
        for dependency in reached {
            dependents.entry(*dependency).or_default().push(*function);
        }
    }
    let mut pending = free_values.keys().copied().collect::<VecDeque<_>>();
    let mut queued = pending.iter().copied().collect::<BTreeSet<_>>();
    while let Some(function) = pending.pop_front() {
        queued.remove(&function);
        let mut required = free_values[&function]
            .iter()
            .copied()
            .collect::<BTreeSet<_>>();
        for dependency in &dependencies[&function] {
            required.extend(free_values[dependency].iter().copied());
        }
        required.retain(|value| !owned_values[&function].contains(value));
        let required = required.into_iter().collect::<Vec<_>>();
        if required == free_values[&function] {
            continue;
        }
        free_values.insert(function, required);
        for dependent in dependents.get(&function).into_iter().flatten() {
            if queued.insert(*dependent) {
                pending.push_back(*dependent);
            }
        }
    }
    let escaping = source.escaping_functions();
    let wrappers = escaping
        .iter()
        .map(|function| {
            let definition = source.function(*function).unwrap();
            (
                *function,
                MachineWrapper {
                    function: *function,
                    captures: free_values[function].clone(),
                    arity: definition.params.len(),
                },
            )
        })
        .collect();

    // One counter for the whole module: a value the lowering mints is named past every CPS value and every other minted one, so an emission name identifies one value module-wide.
    let value_entropy = Entropy::new();
    value_entropy.seed(source.values().len() as u64);
    let mut functions = BTreeMap::new();
    for (index, function) in source.functions().iter().enumerate() {
        let Some(function) = function else { continue };
        let id = curios_cps::FunctionId::from_index(index);
        let results = results
            .get(&id)
            .map(|slots| slots.iter().map(|slot| slot.raw_carrier()).collect())
            .unwrap_or_else(|| vec![None]);
        let lowered = MachineFunctionLowerer::new(
            source,
            id,
            function,
            &free_values,
            results,
            &value_entropy,
        )
        .lower();
        functions.insert(id, lowered);
    }
    let function_hints = source
        .functions()
        .iter()
        .enumerate()
        .filter_map(|(index, function)| {
            let name = function.as_ref()?.debug_name.clone()?;
            Some((curios_cps::FunctionId::from_index(index), name))
        })
        .collect();
    let rows = source.rows().map(|(id, row)| (id, row.clone())).collect();
    let module = MachineModule {
        functions,
        wrappers,
        entry: source.entry().unwrap(),
        function_hints,
        rows,
        entry_suspension: source.entry_suspension(),
    };
    module.verify().expect("invalid closed machine CFG");
    module
}

fn free_runtime_values(
    source: &curios_cps::Module,
    function: curios_cps::FunctionId,
) -> Vec<MachineValueId> {
    let definition = source.function(function).unwrap();
    let mut bound = definition
        .params
        .iter()
        .map(|id| value_id(*id))
        .collect::<BTreeSet<_>>();
    let mut used = BTreeSet::new();
    let mut work = vec![definition.body];
    let mut visited = BTreeSet::new();
    while let Some(node_id) = work.pop() {
        if !visited.insert(node_id) {
            continue;
        }
        let node = source.node(node_id).unwrap();
        for atom in curios_cps::atoms(node) {
            if let curios_cps::Atom::Value(value) = atom {
                used.insert(value_id(*value));
            }
        }
        if let curios_cps::Node::ApplyFun {
            callee: curios_cps::Callee::Closure(value),
            ..
        } = node
        {
            used.insert(value_id(*value));
        }
        match node {
            curios_cps::Node::LetValue { result, next, .. }
            | curios_cps::Node::LetIntrinsic { result, next, .. } => {
                bound.insert(value_id(*result));
                work.push(*next);
            }
            curios_cps::Node::LetFun { body, .. } => work.push(*body),
            curios_cps::Node::LetCont {
                continuations,
                body,
            } => {
                work.push(*body);
                for continuation in continuations {
                    let continuation = source.continuation(*continuation).unwrap();
                    bound.extend(continuation.params.iter().map(|id| value_id(*id)));
                    work.push(continuation.body);
                }
            }
            _ => {}
        }
    }
    used.difference(&bound).copied().collect()
}

fn owned_runtime_values(
    source: &curios_cps::Module,
    function: curios_cps::FunctionId,
) -> BTreeSet<MachineValueId> {
    let definition = source.function(function).unwrap();
    let mut owned = definition
        .params
        .iter()
        .map(|id| value_id(*id))
        .collect::<BTreeSet<_>>();
    for node_id in function_nodes(source, function) {
        match source.node(node_id).unwrap() {
            curios_cps::Node::LetValue { result, .. }
            | curios_cps::Node::LetIntrinsic { result, .. } => {
                owned.insert(value_id(*result));
            }
            curios_cps::Node::LetCont { continuations, .. } => {
                for continuation in continuations {
                    owned.extend(
                        source
                            .continuation(*continuation)
                            .unwrap()
                            .params
                            .iter()
                            .map(|id| value_id(*id)),
                    );
                }
            }
            _ => {}
        }
    }
    owned
}

fn referenced_functions(
    source: &curios_cps::Module,
    function: curios_cps::FunctionId,
) -> BTreeSet<curios_cps::FunctionId> {
    let mut dependencies = BTreeSet::new();
    for node_id in function_nodes(source, function) {
        let node = source.node(node_id).unwrap();
        if let curios_cps::Node::ApplyFun {
            callee: curios_cps::Callee::Known(function),
            ..
        } = node
        {
            dependencies.insert(*function);
        }
        for atom in curios_cps::atoms(node) {
            if let curios_cps::Atom::Fun(function) = atom {
                dependencies.insert(*function);
            }
        }
    }
    dependencies
}

fn function_nodes(
    source: &curios_cps::Module,
    function: curios_cps::FunctionId,
) -> Vec<curios_cps::NodeId> {
    let mut nodes = Vec::new();
    let mut work = vec![source.function(function).unwrap().body];
    let mut visited = BTreeSet::new();
    while let Some(node_id) = work.pop() {
        if !visited.insert(node_id) {
            continue;
        }
        nodes.push(node_id);
        match source.node(node_id).unwrap() {
            curios_cps::Node::LetValue { next, .. }
            | curios_cps::Node::LetIntrinsic { next, .. } => work.push(*next),
            curios_cps::Node::LetFun { body, .. } => work.push(*body),
            curios_cps::Node::LetCont {
                continuations,
                body,
            } => {
                work.push(*body);
                for continuation in continuations {
                    work.push(source.continuation(*continuation).unwrap().body);
                }
            }
            _ => {}
        }
    }
    nodes
}

pub(crate) struct MachineFunctionLowerer<'a> {
    source: &'a curios_cps::Module,
    id: curios_cps::FunctionId,
    function: &'a curios_cps::Function,
    free_values: &'a BTreeMap<curios_cps::FunctionId, Vec<MachineValueId>>,
    results: Vec<Option<curios_cps::Repr>>,
    blocks: BTreeMap<MachineBlockId, MachineBlock>,
    block_scopes: BTreeMap<MachineBlockId, Vec<MachineBlockId>>,
    continuation_blocks: BTreeMap<curios_cps::ContinuationId, MachineBlockId>,
    materialized_closures: BTreeMap<curios_cps::FunctionId, MachineValueId>,
    work: VecDeque<(MachineBlockId, curios_cps::NodeId, Vec<MachineValueId>)>,
    block_entropy: Entropy<MachineBlockId>,
    value_entropy: &'a Entropy<MachineValueId>,
}

impl<'a> MachineFunctionLowerer<'a> {
    fn new(
        source: &'a curios_cps::Module,
        id: curios_cps::FunctionId,
        function: &'a curios_cps::Function,
        free_values: &'a BTreeMap<curios_cps::FunctionId, Vec<MachineValueId>>,
        results: Vec<Option<curios_cps::Repr>>,
        value_entropy: &'a Entropy<MachineValueId>,
    ) -> Self {
        let block_entropy = Entropy::new();
        block_entropy.seed(1);
        Self {
            source,
            id,
            function,
            free_values,
            results,
            blocks: BTreeMap::new(),
            block_scopes: BTreeMap::new(),
            continuation_blocks: BTreeMap::new(),
            materialized_closures: BTreeMap::new(),
            work: VecDeque::new(),
            block_entropy,
            value_entropy,
        }
    }

    fn lower(mut self) -> MachineFunction {
        let entry = MachineBlockId(0);
        self.work.push_back((entry, self.function.body, vec![]));
        while let Some((block, node, params)) = self.work.pop_front() {
            if self.blocks.contains_key(&block) {
                continue;
            }
            let block_body = self.lower_block(block, node, params);
            self.blocks.insert(block, block_body);
        }
        MachineFunction {
            free_values: self.free_values[&self.id].clone(),
            params: self
                .function
                .params
                .iter()
                .map(|id| value_id(*id))
                .collect(),
            results: self.results,
            entry,
            blocks: self.blocks,
            block_scopes: self.block_scopes,
        }
    }

    fn lower_block(
        &mut self,
        block: MachineBlockId,
        mut node_id: curios_cps::NodeId,
        params: Vec<MachineValueId>,
    ) -> MachineBlock {
        // Closure materializations are reused only within the block that defines them: cross-block values flow through explicit block parameters, so a materialized closure is in scope only for the rest of its own block.
        self.materialized_closures.clear();
        let mut instructions = Vec::new();
        loop {
            match self.source.node(node_id).unwrap() {
                curios_cps::Node::LetValue {
                    result,
                    value,
                    next,
                } => {
                    let value = match value {
                        curios_cps::ValueExpr::Literal(literal) => {
                            MachineConstruct::Literal(literal.clone())
                        }
                        curios_cps::ValueExpr::List(values) => MachineConstruct::List(
                            values
                                .iter()
                                .map(|atom| self.lower_atom(atom, &mut instructions))
                                .collect(),
                        ),
                        curios_cps::ValueExpr::Tuple(values) => MachineConstruct::Tuple(
                            values
                                .iter()
                                .map(|atom| self.lower_atom(atom, &mut instructions))
                                .collect(),
                        ),
                        curios_cps::ValueExpr::Row(row, values) => MachineConstruct::Row(
                            *row,
                            values
                                .iter()
                                .map(|atom| self.lower_atom(atom, &mut instructions))
                                .collect(),
                        ),
                    };
                    instructions.push(MachineInstruction::Construct {
                        result: value_id(*result),
                        value,
                    });
                    node_id = *next;
                }
                curios_cps::Node::LetIntrinsic {
                    result,
                    op,
                    args,
                    next,
                } => {
                    let args = self.lower_atoms(args, &mut instructions);
                    instructions.push(MachineInstruction::Intrinsic {
                        result: value_id(*result),
                        op: *op,
                        args,
                    });
                    node_id = *next;
                }
                curios_cps::Node::LetFun { body, .. } => node_id = *body,
                curios_cps::Node::LetCont {
                    continuations,
                    body,
                } => {
                    let members = continuations
                        .iter()
                        .map(|continuation| self.queue_continuation(*continuation))
                        .collect::<Vec<_>>();
                    let scope = self.block_entropy.fresh();
                    self.work.push_back((scope, *body, vec![]));
                    self.block_scopes.entry(block).or_default().push(scope);
                    self.block_scopes.entry(scope).or_default().extend(members);
                    return MachineBlock {
                        params,
                        instructions,
                        terminator: MachineTerminator::Jump(MachineEdge {
                            target: scope,
                            args: vec![],
                        }),
                    };
                }
                terminal => {
                    let terminator = self.lower_terminator(terminal, &mut instructions);
                    return MachineBlock {
                        params,
                        instructions,
                        terminator,
                    };
                }
            }
        }
    }

    fn queue_continuation(&mut self, continuation: curios_cps::ContinuationId) -> MachineBlockId {
        if let Some(block) = self.continuation_blocks.get(&continuation) {
            return *block;
        }
        let block = self.block_entropy.fresh();
        self.continuation_blocks.insert(continuation, block);
        let continuation = self.source.continuation(continuation).unwrap();
        self.work.push_back((
            block,
            continuation.body,
            continuation.params.iter().map(|id| value_id(*id)).collect(),
        ));
        block
    }

    fn lower_edge(
        &mut self,
        edge: &curios_cps::Edge,
        instructions: &mut Vec<MachineInstruction>,
    ) -> MachineEdge {
        MachineEdge {
            target: self.queue_continuation(edge.target),
            args: self.lower_atoms(&edge.args, instructions),
        }
    }

    fn lower_terminator(
        &mut self,
        node: &curios_cps::Node,
        instructions: &mut Vec<MachineInstruction>,
    ) -> MachineTerminator {
        match node {
            curios_cps::Node::ApplyCont(edge) if edge.target == self.function.return_cont => {
                MachineTerminator::Return(
                    edge.args
                        .iter()
                        .map(|arg| self.lower_atom(arg, instructions))
                        .collect(),
                )
            }
            curios_cps::Node::ApplyCont(edge) => {
                MachineTerminator::Jump(self.lower_edge(edge, instructions))
            }
            curios_cps::Node::Switch {
                scrutinee,
                cases,
                default,
            } => MachineTerminator::Switch {
                scrutinee: self.lower_atom(scrutinee, instructions),
                cases: cases
                    .iter()
                    .map(|(tag, edge)| (*tag, self.lower_edge(edge, instructions)))
                    .collect(),
                default: default
                    .as_ref()
                    .map(|edge| self.lower_edge(edge, instructions)),
            },
            curios_cps::Node::ApplyFun {
                callee,
                args,
                return_to,
            } => {
                let returns = *return_to == self.function.return_cont;
                match callee {
                    curios_cps::Callee::Known(function) => {
                        let mut lowered = self.free_values[function]
                            .iter()
                            .copied()
                            .map(MachineOperand::Value)
                            .collect::<Vec<_>>();
                        lowered.extend(self.lower_atoms(args, instructions));
                        if returns {
                            MachineTerminator::TailDirectCall {
                                function: *function,
                                args: lowered,
                            }
                        } else {
                            MachineTerminator::DirectCall {
                                function: *function,
                                args: lowered,
                                resume: self.queue_continuation(*return_to),
                            }
                        }
                    }
                    curios_cps::Callee::Closure(closure) => {
                        let args = self.lower_atoms(args, instructions);
                        if returns {
                            MachineTerminator::TailIndirectCall {
                                closure: value_id(*closure),
                                args,
                            }
                        } else {
                            MachineTerminator::IndirectCall {
                                closure: value_id(*closure),
                                args,
                                resume: self.queue_continuation(*return_to),
                            }
                        }
                    }
                }
            }
            curios_cps::Node::Foreign {
                function,
                args,
                return_to,
            } => {
                let args = self.lower_atoms(args, instructions);
                if *return_to == self.function.return_cont {
                    MachineTerminator::ForeignReturn {
                        function: function.clone(),
                        args,
                    }
                } else {
                    MachineTerminator::Foreign {
                        function: function.clone(),
                        args,
                        resume: self.queue_continuation(*return_to),
                    }
                }
            }
            curios_cps::Node::Cell {
                op,
                args,
                return_to,
            } => {
                let args = self.lower_atoms(args, instructions);
                if *return_to == self.function.return_cont {
                    MachineTerminator::CellReturn { op: *op, args }
                } else {
                    MachineTerminator::Cell {
                        op: *op,
                        args,
                        resume: self.queue_continuation(*return_to),
                    }
                }
            }
            curios_cps::Node::Channel {
                op,
                args,
                return_to,
            } => {
                let args = self.lower_atoms(args, instructions);
                if *return_to == self.function.return_cont {
                    MachineTerminator::ChannelReturn { op: *op, args }
                } else {
                    MachineTerminator::Channel {
                        op: *op,
                        args,
                        resume: self.queue_continuation(*return_to),
                    }
                }
            }
            curios_cps::Node::Intrinsic {
                op,
                args,
                return_to,
            } => {
                let args = self.lower_atoms(args, instructions);
                if *return_to == self.function.return_cont {
                    MachineTerminator::IntrinsicReturn { op: *op, args }
                } else {
                    MachineTerminator::Intrinsic {
                        op: *op,
                        args,
                        resume: self.queue_continuation(*return_to),
                    }
                }
            }
            curios_cps::Node::Halt { function, args } => MachineTerminator::Halt {
                function: function.clone(),
                args: self.lower_atoms(args, instructions),
            },
            curios_cps::Node::Panic(panic) => MachineTerminator::Panic(*panic),
            curios_cps::Node::Unreachable => MachineTerminator::Unreachable,
            _ => unreachable!("non-terminal CPS node reached terminal lowering"),
        }
    }

    fn lower_atoms(
        &mut self,
        atoms: &[curios_cps::Atom],
        instructions: &mut Vec<MachineInstruction>,
    ) -> Vec<MachineOperand> {
        atoms
            .iter()
            .map(|atom| self.lower_atom(atom, instructions))
            .collect()
    }

    fn lower_atom(
        &mut self,
        atom: &curios_cps::Atom,
        instructions: &mut Vec<MachineInstruction>,
    ) -> MachineOperand {
        match atom {
            curios_cps::Atom::Value(value) => MachineOperand::Value(value_id(*value)),
            curios_cps::Atom::Literal(literal) => MachineOperand::Literal(literal.clone()),
            curios_cps::Atom::Filler => MachineOperand::Filler,
            curios_cps::Atom::Fun(function) => {
                if let Some(existing) = self.materialized_closures.get(function) {
                    return MachineOperand::Value(*existing);
                }
                let result = self.value_entropy.fresh();
                let captures = self.free_values[function]
                    .iter()
                    .copied()
                    .map(MachineOperand::Value)
                    .collect();
                instructions.push(MachineInstruction::MakeClosure {
                    result,
                    function: *function,
                    captures,
                });
                self.materialized_closures.insert(*function, result);
                MachineOperand::Value(result)
            }
        }
    }
}

pub(crate) fn value_id(value: curios_cps::ValueId) -> MachineValueId {
    MachineValueId(value.index() as u32)
}

impl MachineModule {
    pub(crate) fn verify(&self) -> Result<(), MachineVerifyError> {
        let entry = self
            .functions
            .get(&self.entry)
            .ok_or_else(|| MachineVerifyError("machine entry function is undefined".into()))?;
        if !entry.params.is_empty() || !entry.free_values.is_empty() {
            return Err(MachineVerifyError(
                "machine entry function must be closed and nullary".into(),
            ));
        }
        for (id, function) in &self.functions {
            if !function.blocks.contains_key(&function.entry) {
                return Err(MachineVerifyError(format!("{id} has no entry block")));
            }
            Self::verify_block_scopes(*id, function)?;
            self.verify_closure_construction(*id, function)?;
            for block in function.blocks.values() {
                self.verify_block(*id, function, block)?;
            }
        }
        for (function, wrapper) in &self.wrappers {
            if function != &wrapper.function || !self.functions.contains_key(function) {
                return Err(MachineVerifyError(
                    "closure wrapper targets an undefined function".into(),
                ));
            }
            let definition = &self.functions[function];
            if wrapper.captures != definition.free_values
                || wrapper.arity != definition.params.len()
            {
                return Err(MachineVerifyError(
                    "closure wrapper ABI does not match direct code".into(),
                ));
            }
        }
        Ok(())
    }

    fn verify_block_scopes(
        owner: curios_cps::FunctionId,
        function: &MachineFunction,
    ) -> Result<(), MachineVerifyError> {
        let mut parents = BTreeMap::new();
        for (parent, children) in &function.block_scopes {
            if !function.blocks.contains_key(parent) {
                return Err(MachineVerifyError(format!(
                    "{owner} block scope parent {parent} is undefined"
                )));
            }
            for child in children {
                if !function.blocks.contains_key(child) {
                    return Err(MachineVerifyError(format!(
                        "{owner} block scope child {child} is undefined"
                    )));
                }
                if let Some(previous) = parents.insert(*child, *parent) {
                    return Err(MachineVerifyError(format!(
                        "{owner} block {child} belongs to both {previous} and {parent}"
                    )));
                }
            }
        }
        if parents.contains_key(&function.entry) {
            return Err(MachineVerifyError(format!(
                "{owner} entry block cannot belong to a nested scope"
            )));
        }
        // Each block is followed to the entry once: a chain is walked until it meets a block already known to reach it, and every block on the way is then known to as well. Followed from every block afresh, blocks nested n deep would walk the square of the nesting.
        let mut rooted = BTreeSet::from([function.entry]);
        for block in function.blocks.keys().copied() {
            if block != function.entry && !parents.contains_key(&block) {
                return Err(MachineVerifyError(format!(
                    "{owner} non-entry block {block} has no lexical owner"
                )));
            }
            let mut path = Vec::new();
            let mut on_path = BTreeSet::new();
            let mut current = block;
            while !rooted.contains(&current) {
                if !on_path.insert(current) {
                    return Err(MachineVerifyError(format!(
                        "{owner} block scopes contain a cycle at {current}"
                    )));
                }
                path.push(current);
                current = match parents.get(&current) {
                    Some(parent) => *parent,
                    None => {
                        return Err(MachineVerifyError(format!(
                            "{owner} block {block} is not nested under the entry block"
                        )));
                    }
                };
            }
            rooted.extend(path);
        }
        Ok(())
    }

    fn verify_closure_construction(
        &self,
        owner: curios_cps::FunctionId,
        function: &MachineFunction,
    ) -> Result<(), MachineVerifyError> {
        for instruction in function
            .blocks
            .values()
            .flat_map(|block| &block.instructions)
        {
            if let MachineInstruction::MakeClosure {
                function, captures, ..
            } = instruction
            {
                let definition = self.functions.get(function).ok_or_else(|| {
                    MachineVerifyError(format!("{owner} constructs undefined {function}"))
                })?;
                if !self.wrappers.contains_key(function)
                    || captures.len() != definition.free_values.len()
                {
                    return Err(MachineVerifyError(format!(
                        "{owner} closure construction for {function} has an invalid environment"
                    )));
                }
            }
        }
        Ok(())
    }

    fn verify_block(
        &self,
        owner: curios_cps::FunctionId,
        function: &MachineFunction,
        block: &MachineBlock,
    ) -> Result<(), MachineVerifyError> {
        for instruction in &block.instructions {
            match instruction {
                MachineInstruction::Intrinsic { op, args, .. } if args.len() != op.arity() => {
                    return Err(MachineVerifyError(format!(
                        "{owner} machine intrinsic {op:?} has wrong arity"
                    )));
                }
                MachineInstruction::MakeClosure { function, .. }
                    if !self.functions.contains_key(function) =>
                {
                    return Err(MachineVerifyError(format!(
                        "{owner} constructs undefined function {function}"
                    )));
                }
                _ => {}
            }
        }
        match &block.terminator {
            MachineTerminator::Return(operands) if operands.len() != function.results.len() => {
                return Err(MachineVerifyError(format!(
                    "{owner} returns {} values while its function returns {}",
                    operands.len(),
                    function.results.len()
                )));
            }
            MachineTerminator::Return(_) => {}
            MachineTerminator::Jump(edge) => verify_edge(function, edge)?,
            MachineTerminator::Switch { cases, default, .. } => {
                for edge in cases.values().chain(default.iter()) {
                    verify_edge(function, edge)?;
                }
            }
            MachineTerminator::DirectCall {
                function: callee,
                args,
                resume,
            } => {
                let callee = self.functions.get(callee).ok_or_else(|| {
                    MachineVerifyError(format!("{owner} directly calls an undefined function"))
                })?;
                if args.len() != callee.free_values.len() + callee.params.len() {
                    return Err(MachineVerifyError(format!(
                        "{owner} direct call argument count does not match closed signature"
                    )));
                }
                verify_block_resume(function, *resume, callee.results.len())?;
            }
            MachineTerminator::TailDirectCall {
                function: callee,
                args,
            } => {
                let callee = self.functions.get(callee).ok_or_else(|| {
                    MachineVerifyError(format!("{owner} tail-calls an undefined function"))
                })?;
                if args.len() != callee.free_values.len() + callee.params.len() {
                    return Err(MachineVerifyError(format!(
                        "{owner} tail-call argument count does not match closed signature"
                    )));
                }
                // A tail call is emitted as `return_call`, which hands the callee's results straight out of this function — so the two signatures must agree on results, each at one carrier, as well as on arguments.
                if callee.results != function.results {
                    return Err(MachineVerifyError(format!(
                        "{owner} tail-calls a function returning {:?} while returning {:?}",
                        callee.results, function.results
                    )));
                }
            }
            MachineTerminator::IndirectCall { resume, .. } => {
                verify_block_resume(function, *resume, 1)?
            }
            MachineTerminator::Intrinsic {
                op: curios_cps::IntrinsicCall::ListMap,
                args,
                resume,
            } => {
                if args.len() != 2 {
                    return Err(MachineVerifyError(format!(
                        "{owner} ListMap has the wrong operand count"
                    )));
                }
                verify_block_resume(function, *resume, 1)?;
            }
            // Every tail position hands its own results straight out of the function, as a tail direct call does, so each is held to the function's result count: a closure call and a `ListMap` produce one, a host or cell operation what its row says.
            MachineTerminator::TailIndirectCall { .. } => tail_returns(owner, function, 1)?,
            MachineTerminator::IntrinsicReturn {
                op: curios_cps::IntrinsicCall::ListMap,
                args,
            } if args.len() != 2 => {
                return Err(MachineVerifyError(format!(
                    "{owner} tail ListMap has the wrong operand count"
                )));
            }
            MachineTerminator::IntrinsicReturn { .. } => tail_returns(owner, function, 1)?,
            MachineTerminator::Foreign {
                function: foreign,
                args,
                resume,
                ..
            } => {
                if args.len() != foreign.signature().imported_params().len() {
                    return Err(MachineVerifyError(format!(
                        "{owner} foreign call argument count does not match its ABI"
                    )));
                }
                verify_block_resume(
                    function,
                    *resume,
                    foreign.signature().imported_results().len(),
                )?;
            }
            MachineTerminator::ForeignReturn {
                function: foreign,
                args,
            } => {
                if args.len() != foreign.signature().imported_params().len() {
                    return Err(MachineVerifyError(format!(
                        "{owner} foreign tail call argument count does not match its ABI"
                    )));
                }
                tail_returns(
                    owner,
                    function,
                    foreign.signature().imported_results().len(),
                )?;
            }
            MachineTerminator::Cell { op, args, resume } => {
                if args.len() != op.operand_arity() {
                    return Err(MachineVerifyError(format!(
                        "{owner} cell operation has the wrong operand count"
                    )));
                }
                verify_block_resume(function, *resume, op.result_arity())?
            }
            MachineTerminator::CellReturn { op, args } => {
                if args.len() != op.operand_arity() {
                    return Err(MachineVerifyError(format!(
                        "{owner} cell return has the wrong operand count"
                    )));
                }
                tail_returns(owner, function, op.result_arity())?;
            }
            MachineTerminator::Channel { op, args, resume } => {
                if args.len() != op.operand_arity() {
                    return Err(MachineVerifyError(format!(
                        "{owner} channel operation has the wrong operand count"
                    )));
                }
                verify_block_resume(function, *resume, op.result_arity())?
            }
            MachineTerminator::ChannelReturn { op, args } => {
                if args.len() != op.operand_arity() {
                    return Err(MachineVerifyError(format!(
                        "{owner} channel return has the wrong operand count"
                    )));
                }
                tail_returns(owner, function, op.result_arity())?;
            }
            MachineTerminator::Halt {
                function: foreign,
                args,
            } => {
                if args.len() != foreign.signature().imported_params().len() {
                    return Err(MachineVerifyError(format!(
                        "{owner} halting call argument count does not match its ABI"
                    )));
                }
            }
            MachineTerminator::Panic(_) | MachineTerminator::Unreachable => {}
        }
        Ok(())
    }
}

/// A tail position produces `results` references straight out of `function`, so the function must hand back that many, each a reference — the check `TailDirectCall` makes against its callee, stated once for the positions an operation fixes rather than a callee. The representation leaves such a function's class boxed; a raw result here is that decision broken.
fn tail_returns(
    owner: curios_cps::FunctionId,
    function: &MachineFunction,
    results: usize,
) -> Result<(), MachineVerifyError> {
    if results != function.results.len() || function.results.iter().any(Option::is_some) {
        return Err(MachineVerifyError(format!(
            "{owner} returns {results} references from a tail position while its function returns {:?}",
            function.results
        )));
    }
    Ok(())
}

fn verify_edge(function: &MachineFunction, edge: &MachineEdge) -> Result<(), MachineVerifyError> {
    let target = function
        .blocks
        .get(&edge.target)
        .ok_or_else(|| MachineVerifyError(format!("jump targets undefined {}", edge.target)))?;
    if edge.args.len() != target.params.len() {
        return Err(MachineVerifyError(format!(
            "jump to {} carries {} values; expected {}",
            edge.target,
            edge.args.len(),
            target.params.len()
        )));
    }
    Ok(())
}

fn verify_block_resume(
    function: &MachineFunction,
    target: MachineBlockId,
    arity: usize,
) -> Result<(), MachineVerifyError> {
    let params = function
        .blocks
        .get(&target)
        .ok_or_else(|| MachineVerifyError(format!("resume targets undefined {target}")))?
        .params
        .len();
    if params != arity {
        return Err(MachineVerifyError(format!(
            "resume {target} expects {params} values, operation returns {arity}"
        )));
    }
    Ok(())
}

#[cfg(test)]
mod tests;
