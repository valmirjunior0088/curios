//! Private closed control-flow graph produced after high-CPS optimization.
//!
//! Functions have explicit blocks and block parameters. The CPS return
//! continuation never becomes a block: a jump to the current function's
//! bodyless sentinel is translated directly to [`MachineTerminator::Return`].

use {
    crate::{
        CpsAtom, CpsCallee, CpsCellOp, CpsContId, CpsEdge, CpsFunId, CpsFunction, CpsIntrinsicOp,
        CpsLiteral, CpsModule, CpsNode, CpsNodeId, CpsPrimOp, CpsValueExpr, CpsValueId, atoms,
    },
    curios_abi::ForeignFunction,
    curios_base::{Entropy, id},
    std::{
        collections::{BTreeMap, BTreeSet, VecDeque},
        fmt,
        sync::Arc,
    },
};

mod structurize;
pub(crate) use structurize::structurize;

// Sigils follow the naming scheme shared with `curios-ersd` and `curios-wasm`
// — see "One naming scheme for compiler identities" in `documentation/DESIGN.md`.
id!(MachineBlockId, "~b", mint);
id!(MachineValueId, "~v", mint);

#[derive(Debug, Clone)]
pub(crate) enum MachineOperand {
    Value(MachineValueId),
    Literal(CpsLiteral),
}

#[derive(Debug, Clone)]
pub(crate) enum MachineConstruct {
    Literal(CpsLiteral),
    List(Vec<MachineOperand>),
    Tuple(Vec<MachineOperand>),
}

#[derive(Debug, Clone)]
pub(crate) enum MachineInstruction {
    Construct {
        result: MachineValueId,
        value: MachineConstruct,
    },
    Prim {
        result: MachineValueId,
        op: CpsPrimOp,
        args: Vec<MachineOperand>,
    },
    /// Retained-ABI closure wrapper around direct code. The wrapper unpacks
    /// these captures and tail-calls `function`.
    MakeClosure {
        result: MachineValueId,
        function: CpsFunId,
        captures: Vec<MachineOperand>,
    },
    /// Explicit mixed-initialization fallback; ordinary recursive functions
    /// never use either instruction.
    FallbackShell {
        result: MachineValueId,
        function: CpsFunId,
    },
    FallbackFill {
        shell: MachineValueId,
        function: CpsFunId,
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
    Return(MachineOperand),
    Jump(MachineEdge),
    Switch {
        scrutinee: MachineOperand,
        cases: BTreeMap<u32, MachineEdge>,
        default: Option<MachineEdge>,
    },
    DirectCall {
        function: CpsFunId,
        args: Vec<MachineOperand>,
        resume: MachineBlockId,
    },
    TailDirectCall {
        function: CpsFunId,
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
        op: CpsCellOp,
        args: Vec<MachineOperand>,
        resume: MachineBlockId,
    },
    CellReturn {
        op: CpsCellOp,
        args: Vec<MachineOperand>,
    },
    Intrinsic {
        op: CpsIntrinsicOp,
        args: Vec<MachineOperand>,
        resume: MachineBlockId,
    },
    IntrinsicReturn {
        op: CpsIntrinsicOp,
        args: Vec<MachineOperand>,
    },
    Exit(Option<MachineOperand>),
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
    entry: MachineBlockId,
    blocks: BTreeMap<MachineBlockId, MachineBlock>,
    block_scopes: BTreeMap<MachineBlockId, Vec<MachineBlockId>>,
}

#[derive(Debug, Clone)]
pub(crate) struct MachineWrapper {
    function: CpsFunId,
    captures: Vec<MachineValueId>,
    arity: usize,
}

#[derive(Debug, Clone)]
pub(crate) struct MachineModule {
    functions: BTreeMap<CpsFunId, MachineFunction>,
    wrappers: BTreeMap<CpsFunId, MachineWrapper>,
    entry: CpsFunId,
    /// Each function's source hint, carried from the Cont module so emission names
    /// can spell a function's origin (`func/{index}$hint`). A hint never affects
    /// identity — the `CpsFunId` index does — so a missing entry only omits the hint.
    function_hints: BTreeMap<CpsFunId, String>,
}

impl MachineModule {
    /// The source hint of a function, if it carries one.
    pub(crate) fn function_hint(&self, id: CpsFunId) -> Option<&str> {
        self.function_hints.get(&id).map(String::as_str)
    }
}

#[derive(Debug)]
pub(crate) struct MachineVerifyError(String);

impl fmt::Display for MachineVerifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

pub(crate) fn lower(source: &CpsModule) -> MachineModule {
    source
        .verify()
        .expect("invalid high CPS at machine boundary");
    let mut free_values = source
        .functions()
        .iter()
        .enumerate()
        .filter_map(|(index, function)| {
            function.as_ref()?;
            let function = CpsFunId::from_index(index);
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
    loop {
        let previous = free_values.clone();
        for (function, required) in &mut free_values {
            let mut transitive = required.iter().copied().collect::<BTreeSet<_>>();
            for dependency in &dependencies[function] {
                transitive.extend(previous[dependency].iter().copied());
            }
            transitive.retain(|value| !owned_values[function].contains(value));
            *required = transitive.into_iter().collect();
        }
        if free_values == previous {
            break;
        }
    }
    let escaping = escaping_functions(source);
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

    let mut functions = BTreeMap::new();
    for (index, function) in source.functions().iter().enumerate() {
        let Some(function) = function else { continue };
        let id = CpsFunId::from_index(index);
        let lowered = MachineFunctionLowerer::new(source, id, function, &free_values).lower();
        functions.insert(id, lowered);
    }
    let function_hints = source
        .functions()
        .iter()
        .enumerate()
        .filter_map(|(index, function)| {
            let name = function.as_ref()?.debug_name.clone()?;
            Some((CpsFunId::from_index(index), name))
        })
        .collect();
    let module = MachineModule {
        functions,
        wrappers,
        entry: source.entry().unwrap(),
        function_hints,
    };
    module.verify().expect("invalid closed machine CFG");
    module
}

fn escaping_functions(source: &CpsModule) -> BTreeSet<CpsFunId> {
    let mut escaping = BTreeSet::new();
    for node in source.nodes().iter().flatten() {
        for atom in atoms(node) {
            if let CpsAtom::Fun(function) = atom {
                escaping.insert(*function);
            }
        }
    }
    escaping
}

fn free_runtime_values(source: &CpsModule, function: CpsFunId) -> Vec<MachineValueId> {
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
        for atom in atoms(node) {
            if let CpsAtom::Value(value) = atom {
                used.insert(value_id(*value));
            }
        }
        if let CpsNode::ApplyFun {
            callee: CpsCallee::Closure(value),
            ..
        } = node
        {
            used.insert(value_id(*value));
        }
        match node {
            CpsNode::LetValue { result, next, .. } | CpsNode::LetPrim { result, next, .. } => {
                bound.insert(value_id(*result));
                work.push(*next);
            }
            CpsNode::LetFun { body, .. } | CpsNode::RecInit { body, .. } => work.push(*body),
            CpsNode::LetCont {
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

fn owned_runtime_values(source: &CpsModule, function: CpsFunId) -> BTreeSet<MachineValueId> {
    let definition = source.function(function).unwrap();
    let mut owned = definition
        .params
        .iter()
        .map(|id| value_id(*id))
        .collect::<BTreeSet<_>>();
    for node_id in function_nodes(source, function) {
        match source.node(node_id).unwrap() {
            CpsNode::LetValue { result, .. } | CpsNode::LetPrim { result, .. } => {
                owned.insert(value_id(*result));
            }
            CpsNode::LetCont { continuations, .. } => {
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

fn referenced_functions(source: &CpsModule, function: CpsFunId) -> BTreeSet<CpsFunId> {
    let mut dependencies = BTreeSet::new();
    for node_id in function_nodes(source, function) {
        let node = source.node(node_id).unwrap();
        if let CpsNode::ApplyFun {
            callee: CpsCallee::Known(function),
            ..
        } = node
        {
            dependencies.insert(*function);
        }
        for atom in atoms(node) {
            if let CpsAtom::Fun(function) = atom {
                dependencies.insert(*function);
            }
        }
    }
    dependencies
}

fn function_nodes(source: &CpsModule, function: CpsFunId) -> Vec<CpsNodeId> {
    let mut nodes = Vec::new();
    let mut work = vec![source.function(function).unwrap().body];
    let mut visited = BTreeSet::new();
    while let Some(node_id) = work.pop() {
        if !visited.insert(node_id) {
            continue;
        }
        nodes.push(node_id);
        match source.node(node_id).unwrap() {
            CpsNode::LetValue { next, .. } | CpsNode::LetPrim { next, .. } => work.push(*next),
            CpsNode::LetFun { body, .. } | CpsNode::RecInit { body, .. } => work.push(*body),
            CpsNode::LetCont {
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
    source: &'a CpsModule,
    id: CpsFunId,
    function: &'a CpsFunction,
    free_values: &'a BTreeMap<CpsFunId, Vec<MachineValueId>>,
    blocks: BTreeMap<MachineBlockId, MachineBlock>,
    block_scopes: BTreeMap<MachineBlockId, Vec<MachineBlockId>>,
    continuation_blocks: BTreeMap<CpsContId, MachineBlockId>,
    recursive_closures: BTreeMap<CpsFunId, MachineValueId>,
    materialized_closures: BTreeMap<CpsFunId, MachineValueId>,
    ready_fills: BTreeMap<CpsNodeId, Vec<(MachineValueId, CpsFunId)>>,
    work: VecDeque<(MachineBlockId, CpsNodeId, Vec<MachineValueId>)>,
    block_entropy: Entropy<MachineBlockId>,
    value_entropy: Entropy<MachineValueId>,
}

impl<'a> MachineFunctionLowerer<'a> {
    fn new(
        source: &'a CpsModule,
        id: CpsFunId,
        function: &'a CpsFunction,
        free_values: &'a BTreeMap<CpsFunId, Vec<MachineValueId>>,
    ) -> Self {
        let block_entropy = Entropy::new();
        block_entropy.seed(1);
        let value_entropy = Entropy::new();
        value_entropy.seed(source.values().len());
        Self {
            source,
            id,
            function,
            free_values,
            blocks: BTreeMap::new(),
            block_scopes: BTreeMap::new(),
            continuation_blocks: BTreeMap::new(),
            recursive_closures: BTreeMap::new(),
            materialized_closures: BTreeMap::new(),
            ready_fills: BTreeMap::new(),
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
            entry,
            blocks: self.blocks,
            block_scopes: self.block_scopes,
        }
    }

    fn lower_block(
        &mut self,
        block: MachineBlockId,
        mut node_id: CpsNodeId,
        params: Vec<MachineValueId>,
    ) -> MachineBlock {
        // Closure materializations are reused only within the block that defines
        // them: cross-block values flow through explicit block parameters, so a
        // materialized closure is in scope only for the rest of its own block.
        self.materialized_closures.clear();
        let mut instructions = Vec::new();
        loop {
            if let Some(fills) = self.ready_fills.remove(&node_id) {
                for (shell, function) in fills {
                    let captures = self.free_values[&function]
                        .iter()
                        .copied()
                        .map(MachineOperand::Value)
                        .collect();
                    instructions.push(MachineInstruction::FallbackFill {
                        shell,
                        function,
                        captures,
                    });
                }
            }
            match self.source.node(node_id).unwrap() {
                CpsNode::LetValue {
                    result,
                    value,
                    next,
                } => {
                    let value = match value {
                        CpsValueExpr::Literal(literal) => {
                            MachineConstruct::Literal(literal.clone())
                        }
                        CpsValueExpr::List(values) => MachineConstruct::List(
                            values
                                .iter()
                                .map(|atom| self.lower_atom(atom, &mut instructions))
                                .collect(),
                        ),
                        CpsValueExpr::Tuple(values) => MachineConstruct::Tuple(
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
                CpsNode::LetPrim {
                    result,
                    op,
                    args,
                    next,
                } => {
                    let args = self.lower_atoms(args, &mut instructions);
                    instructions.push(MachineInstruction::Prim {
                        result: value_id(*result),
                        op: *op,
                        args,
                    });
                    node_id = *next;
                }
                CpsNode::LetFun { body, .. } => node_id = *body,
                CpsNode::LetCont {
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
                CpsNode::RecInit {
                    functions,
                    values,
                    ready,
                    body,
                } => {
                    let escaping = initialization_function_atoms(self.source, *body, *ready);
                    let computed = values
                        .iter()
                        .map(|value| value_id(*value))
                        .collect::<BTreeSet<_>>();
                    for function in functions.iter().filter(|function| {
                        escaping.contains(function)
                            && self.free_values[function]
                                .iter()
                                .any(|value| computed.contains(value))
                    }) {
                        let result = self.value_entropy.fresh();
                        assert!(
                            self.recursive_closures.insert(*function, result).is_none(),
                            "recursive function belongs to more than one initializer"
                        );
                        instructions.push(MachineInstruction::FallbackShell {
                            result,
                            function: *function,
                        });
                        self.ready_fills
                            .entry(*ready)
                            .or_default()
                            .push((result, *function));
                    }
                    node_id = *body;
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

    fn queue_continuation(&mut self, continuation: CpsContId) -> MachineBlockId {
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
        edge: &CpsEdge,
        instructions: &mut Vec<MachineInstruction>,
    ) -> MachineEdge {
        MachineEdge {
            target: self.queue_continuation(edge.target),
            args: self.lower_atoms(&edge.args, instructions),
        }
    }

    fn lower_terminator(
        &mut self,
        node: &CpsNode,
        instructions: &mut Vec<MachineInstruction>,
    ) -> MachineTerminator {
        match node {
            CpsNode::ApplyCont(edge) if edge.target == self.function.return_cont => {
                MachineTerminator::Return(self.lower_atom(&edge.args[0], instructions))
            }
            CpsNode::ApplyCont(edge) => {
                MachineTerminator::Jump(self.lower_edge(edge, instructions))
            }
            CpsNode::Switch {
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
            CpsNode::ApplyFun {
                callee,
                args,
                return_to,
            } => {
                let returns = *return_to == self.function.return_cont;
                match callee {
                    CpsCallee::Known(function) => {
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
                    CpsCallee::Closure(closure) => {
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
            CpsNode::Foreign {
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
            CpsNode::Cell {
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
            CpsNode::Intrinsic {
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
            CpsNode::Exit { value } => MachineTerminator::Exit(
                value
                    .as_ref()
                    .map(|value| self.lower_atom(value, instructions)),
            ),
            CpsNode::Unreachable => MachineTerminator::Unreachable,
            _ => unreachable!("non-terminal CPS node reached terminal lowering"),
        }
    }

    fn lower_atoms(
        &mut self,
        atoms: &[CpsAtom],
        instructions: &mut Vec<MachineInstruction>,
    ) -> Vec<MachineOperand> {
        atoms
            .iter()
            .map(|atom| self.lower_atom(atom, instructions))
            .collect()
    }

    fn lower_atom(
        &mut self,
        atom: &CpsAtom,
        instructions: &mut Vec<MachineInstruction>,
    ) -> MachineOperand {
        match atom {
            CpsAtom::Value(value) => MachineOperand::Value(value_id(*value)),
            CpsAtom::Literal(literal) => MachineOperand::Literal(literal.clone()),
            CpsAtom::Fun(function) => {
                if let Some(shell) = self.recursive_closures.get(function) {
                    return MachineOperand::Value(*shell);
                }
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

fn value_id(value: CpsValueId) -> MachineValueId {
    MachineValueId(value.index() as u32)
}

fn initialization_function_atoms(
    source: &CpsModule,
    body: CpsNodeId,
    ready: CpsNodeId,
) -> BTreeSet<CpsFunId> {
    let mut functions = BTreeSet::new();
    let mut work = vec![body];
    let mut visited = BTreeSet::new();
    while let Some(node_id) = work.pop() {
        if node_id == ready || !visited.insert(node_id) {
            continue;
        }
        let node = source.node(node_id).unwrap();
        for atom in atoms(node) {
            if let CpsAtom::Fun(function) = atom {
                functions.insert(*function);
            }
        }
        match node {
            CpsNode::LetValue { next, .. } | CpsNode::LetPrim { next, .. } => work.push(*next),
            CpsNode::LetFun { body, .. } | CpsNode::RecInit { body, .. } => work.push(*body),
            CpsNode::LetCont {
                continuations,
                body,
            } => {
                work.push(*body);
                for continuation in continuations {
                    if let Some(continuation) = source.continuation(*continuation) {
                        work.push(continuation.body);
                    }
                }
            }
            CpsNode::ApplyFun { return_to, .. }
            | CpsNode::Foreign { return_to, .. }
            | CpsNode::Cell { return_to, .. }
            | CpsNode::Intrinsic { return_to, .. } => {
                if let Some(continuation) = source.continuation(*return_to) {
                    work.push(continuation.body);
                }
            }
            CpsNode::ApplyCont(edge) => {
                if let Some(continuation) = source.continuation(edge.target) {
                    work.push(continuation.body);
                }
            }
            CpsNode::Switch { cases, default, .. } => {
                for edge in cases.values().chain(default.iter()) {
                    if let Some(continuation) = source.continuation(edge.target) {
                        work.push(continuation.body);
                    }
                }
            }
            CpsNode::Exit { .. } | CpsNode::Unreachable => {}
        }
    }
    functions
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
        owner: CpsFunId,
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
        for block in function.blocks.keys().copied() {
            if block != function.entry && !parents.contains_key(&block) {
                return Err(MachineVerifyError(format!(
                    "{owner} non-entry block {block} has no lexical owner"
                )));
            }
            let mut current = block;
            let mut seen = BTreeSet::new();
            while let Some(parent) = parents.get(&current) {
                if !seen.insert(current) {
                    return Err(MachineVerifyError(format!(
                        "{owner} block scopes contain a cycle at {current}"
                    )));
                }
                current = *parent;
            }
            if current != function.entry {
                return Err(MachineVerifyError(format!(
                    "{owner} block {block} is not nested under the entry block"
                )));
            }
        }
        Ok(())
    }

    fn verify_closure_construction(
        &self,
        owner: CpsFunId,
        function: &MachineFunction,
    ) -> Result<(), MachineVerifyError> {
        let mut shells = BTreeMap::new();
        let mut fills = BTreeSet::new();
        for instruction in function
            .blocks
            .values()
            .flat_map(|block| &block.instructions)
        {
            match instruction {
                MachineInstruction::MakeClosure {
                    function, captures, ..
                } => {
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
                MachineInstruction::FallbackShell { result, function } => {
                    if !self.wrappers.contains_key(function) {
                        return Err(MachineVerifyError(format!(
                            "{owner} fallback shell for non-escaping {function}"
                        )));
                    }
                    if shells.insert(*result, *function).is_some() {
                        return Err(MachineVerifyError(format!(
                            "{owner} defines fallback shell {result} more than once"
                        )));
                    }
                }
                MachineInstruction::FallbackFill {
                    shell,
                    function,
                    captures,
                } => {
                    if shells.get(shell) != Some(function) || !fills.insert(*shell) {
                        return Err(MachineVerifyError(format!(
                            "{owner} fallback fill does not uniquely match shell {shell}"
                        )));
                    }
                    if captures.len() != self.functions[function].free_values.len() {
                        return Err(MachineVerifyError(format!(
                            "{owner} fallback fill for {function} has an invalid environment"
                        )));
                    }
                }
                MachineInstruction::Construct { .. } | MachineInstruction::Prim { .. } => {}
            }
        }
        if shells.keys().copied().collect::<BTreeSet<_>>() != fills {
            return Err(MachineVerifyError(format!(
                "{owner} has an unfilled recursive closure shell"
            )));
        }
        Ok(())
    }

    fn verify_block(
        &self,
        owner: CpsFunId,
        function: &MachineFunction,
        block: &MachineBlock,
    ) -> Result<(), MachineVerifyError> {
        for instruction in &block.instructions {
            match instruction {
                MachineInstruction::Prim { op, args, .. } if args.len() != op.arity() => {
                    return Err(MachineVerifyError(format!(
                        "{owner} machine primitive {op:?} has wrong arity"
                    )));
                }
                MachineInstruction::MakeClosure { function, .. }
                | MachineInstruction::FallbackShell { function, .. }
                | MachineInstruction::FallbackFill { function, .. }
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
                verify_block_resume(function, *resume, 1)?;
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
            }
            MachineTerminator::IndirectCall { resume, .. } => {
                verify_block_resume(function, *resume, 1)?
            }
            MachineTerminator::Intrinsic {
                op: CpsIntrinsicOp::LstMap,
                args,
                resume,
            } => {
                if args.len() != 2 {
                    return Err(MachineVerifyError(format!(
                        "{owner} LstMap has the wrong operand count"
                    )));
                }
                verify_block_resume(function, *resume, 1)?;
            }
            MachineTerminator::TailIndirectCall { .. } => {}
            MachineTerminator::IntrinsicReturn {
                op: CpsIntrinsicOp::LstMap,
                args,
            } if args.len() != 2 => {
                return Err(MachineVerifyError(format!(
                    "{owner} tail LstMap has the wrong operand count"
                )));
            }
            MachineTerminator::IntrinsicReturn { .. } => {}
            MachineTerminator::Foreign {
                function: foreign,
                args,
                resume,
                ..
            } => {
                if args.len() != foreign.signature.params.len() {
                    return Err(MachineVerifyError(format!(
                        "{owner} foreign call argument count does not match its ABI"
                    )));
                }
                verify_block_resume(function, *resume, foreign.signature.results.len())?;
            }
            MachineTerminator::ForeignReturn {
                function: foreign,
                args,
            } if foreign.signature.results.len() != 1 => {
                return Err(MachineVerifyError(
                    "foreign return must produce one language value".into(),
                ));
            }
            MachineTerminator::ForeignReturn { function, args } => {
                if args.len() != function.signature.params.len() {
                    return Err(MachineVerifyError(format!(
                        "{owner} foreign tail call argument count does not match its ABI"
                    )));
                }
            }
            MachineTerminator::Cell { op, args, resume } => {
                if args.len() != op.operand_arity() {
                    return Err(MachineVerifyError(format!(
                        "{owner} cell operation has the wrong operand count"
                    )));
                }
                verify_block_resume(function, *resume, op.result_arity())?
            }
            MachineTerminator::CellReturn { op, args }
                if op.result_arity() != 1 || args.len() != op.operand_arity() =>
            {
                return Err(MachineVerifyError(
                    "cell return has an invalid operand or result arity".into(),
                ));
            }
            MachineTerminator::CellReturn { .. } => {}
            MachineTerminator::Exit(_) | MachineTerminator::Unreachable => {}
        }
        Ok(())
    }
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
mod tests {
    use {
        super::{
            MachineFunction, MachineInstruction, MachineModule, MachineOperand, MachineTerminator,
            lower, value_id,
        },
        crate::{
            CpsAtom, CpsCallee, CpsContinuation, CpsEdge, CpsFunId, CpsFunction, CpsLiteral,
            CpsModule, CpsNode, into_wasm,
            into_wasm::{EmissionHostTarget, EmissionTail},
        },
    };

    #[test]
    fn return_sentinel_becomes_machine_return_without_a_block() {
        let mut source = CpsModule::new();
        let function = source.reserve_function();
        let return_cont = source.reserve_continuation();
        let body = source.add_node(CpsNode::ApplyCont(CpsEdge {
            target: return_cont,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(7))],
        }));
        source.define_function(
            function,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont,
                body,
            },
        );
        source.set_entry(function);

        let machine = lower(&source);
        let function = &machine.functions[&function];
        assert_eq!(function.blocks.len(), 1);
        assert!(matches!(
            function.blocks[&function.entry].terminator,
            MachineTerminator::Return(MachineOperand::Literal(CpsLiteral::Nat(7)))
        ));
    }

    #[test]
    fn call_to_return_sentinel_becomes_tail_call_without_resume_state() {
        let mut source = CpsModule::new();
        let main = source.reserve_function();
        let callee = source.reserve_function();

        let callee_return = source.reserve_continuation();
        let callee_body = source.add_node(CpsNode::ApplyCont(CpsEdge {
            target: callee_return,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
        }));
        source.define_function(
            callee,
            CpsFunction {
                debug_name: Some("callee".into()),
                params: vec![],
                return_cont: callee_return,
                body: callee_body,
            },
        );

        let main_return = source.reserve_continuation();
        let main_body = source.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(callee),
            args: vec![],
            return_to: main_return,
        });
        let main_body = source.add_node(CpsNode::LetFun {
            functions: vec![callee],
            body: main_body,
        });
        source.define_function(
            main,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont: main_return,
                body: main_body,
            },
        );
        source.set_entry(main);

        let machine = lower(&source);
        let main = &machine.functions[&main];
        assert_eq!(main.blocks.len(), 1);
        assert!(matches!(
            main.blocks[&main.entry].terminator,
            MachineTerminator::TailDirectCall { function, .. } if function == callee
        ));
    }

    #[test]
    fn exit_stays_direct_termination_through_structurization() {
        let mut source = CpsModule::new();
        let main = source.reserve_function();
        let return_cont = source.reserve_continuation();
        let body = source.add_node(CpsNode::Exit {
            value: Some(CpsAtom::Literal(CpsLiteral::Nat(7))),
        });
        source.define_function(
            main,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont,
                body,
            },
        );
        source.set_entry(main);

        let machine = lower(&source);
        let structured = super::structurize(&machine);
        let (_, function) = structured.funcs().first().unwrap();
        assert!(matches!(
            function.region.tail,
            EmissionTail::Host(EmissionHostTarget::Exit { .. })
        ));
    }

    #[test]
    fn residual_rec_init_fills_its_fresh_shell_only_at_the_ready_point() {
        let mut source = CpsModule::new();
        let main = source.reserve_function();
        let function = source.reserve_function();
        let passive = source.reserve_function();
        let computed = source.add_value(Some("computed capture".into()));

        let function_return = source.reserve_continuation();
        let function_body = source.add_node(CpsNode::ApplyCont(CpsEdge {
            target: function_return,
            args: vec![CpsAtom::Value(computed)],
        }));
        source.define_function(
            function,
            CpsFunction {
                debug_name: Some("recursive closure".into()),
                params: vec![],
                return_cont: function_return,
                body: function_body,
            },
        );
        let passive_return = source.reserve_continuation();
        let passive_body = source.add_node(CpsNode::ApplyCont(CpsEdge {
            target: passive_return,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
        }));
        source.define_function(
            passive,
            CpsFunction {
                debug_name: Some("non-participating sibling".into()),
                params: vec![],
                return_cont: passive_return,
                body: passive_body,
            },
        );

        let main_return = source.reserve_continuation();
        let ready = source.add_node(CpsNode::ApplyCont(CpsEdge {
            target: main_return,
            args: vec![CpsAtom::Fun(function)],
        }));
        let bind_computed = source.add_continuation(CpsContinuation {
            debug_name: Some("bind computed capture".into()),
            params: vec![computed],
            body: ready,
        });
        let initialize = source.add_node(CpsNode::ApplyCont(CpsEdge {
            target: bind_computed,
            args: vec![CpsAtom::Fun(function)],
        }));
        let initialization = source.add_node(CpsNode::LetCont {
            continuations: vec![bind_computed],
            body: initialize,
        });
        let body = source.add_node(CpsNode::RecInit {
            functions: vec![function, passive],
            values: vec![computed],
            ready,
            body: initialization,
        });
        source.define_function(
            main,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont: main_return,
                body,
            },
        );
        source.set_entry(main);

        let machine = lower(&source);
        let main = &machine.functions[&main];
        let shells = main
            .blocks
            .values()
            .flat_map(|block| &block.instructions)
            .filter_map(|instruction| match instruction {
                MachineInstruction::FallbackShell { result, function } => {
                    Some((*result, *function))
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        let fills = main
            .blocks
            .values()
            .flat_map(|block| &block.instructions)
            .filter_map(|instruction| match instruction {
                MachineInstruction::FallbackFill {
                    shell,
                    function,
                    captures,
                } => Some((*shell, *function, captures.clone())),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert_eq!(shells.len(), 1);
        assert_eq!(fills.len(), 1);
        assert_eq!(shells[0].1, function);
        assert_eq!(shells[0], (fills[0].0, fills[0].1));
        assert!(matches!(
            fills[0].2.as_slice(),
            [MachineOperand::Value(value)] if *value == value_id(computed)
        ));
        assert!(main.blocks.values().any(|block| {
            matches!(
                block.terminator,
                MachineTerminator::Return(MachineOperand::Value(value)) if value == shells[0].0
            )
        }));
        let _wasm = into_wasm(&source);
    }

    fn machine_make_closures(function: &MachineFunction, target: CpsFunId) -> usize {
        function
            .blocks
            .values()
            .flat_map(|block| &block.instructions)
            .filter(|instruction| {
                matches!(
                    instruction,
                    MachineInstruction::MakeClosure { function, .. } if *function == target
                )
            })
            .count()
    }

    fn machine_shell_count(machine: &MachineModule) -> usize {
        machine
            .functions
            .values()
            .flat_map(|function| function.blocks.values())
            .flat_map(|block| &block.instructions)
            .filter(|instruction| matches!(instruction, MachineInstruction::FallbackShell { .. }))
            .count()
    }

    #[test]
    fn repeated_first_class_use_materializes_one_closure() {
        let mut source = CpsModule::new();
        let main = source.reserve_function();
        let target = source.reserve_function();
        let consumer = source.reserve_function();

        let target_return = source.reserve_continuation();
        let target_body = source.add_node(CpsNode::ApplyCont(CpsEdge {
            target: target_return,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
        }));
        source.define_function(
            target,
            CpsFunction {
                debug_name: Some("target".into()),
                params: vec![],
                return_cont: target_return,
                body: target_body,
            },
        );

        let first = source.add_value(Some("first".into()));
        let second = source.add_value(Some("second".into()));
        let consumer_return = source.reserve_continuation();
        let consumer_body = source.add_node(CpsNode::ApplyCont(CpsEdge {
            target: consumer_return,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
        }));
        source.define_function(
            consumer,
            CpsFunction {
                debug_name: Some("consumer".into()),
                params: vec![first, second],
                return_cont: consumer_return,
                body: consumer_body,
            },
        );

        let main_return = source.reserve_continuation();
        let call = source.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(consumer),
            args: vec![CpsAtom::Fun(target), CpsAtom::Fun(target)],
            return_to: main_return,
        });
        let main_body = source.add_node(CpsNode::LetFun {
            functions: vec![target, consumer],
            body: call,
        });
        source.define_function(
            main,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont: main_return,
                body: main_body,
            },
        );
        source.set_entry(main);

        let machine = lower(&source);
        assert_eq!(machine_make_closures(&machine.functions[&main], target), 1);
    }

    #[test]
    fn mixed_direct_and_escaping_use_keeps_the_call_direct() {
        let mut source = CpsModule::new();
        let main = source.reserve_function();
        let target = source.reserve_function();

        let target_return = source.reserve_continuation();
        let target_body = source.add_node(CpsNode::ApplyCont(CpsEdge {
            target: target_return,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
        }));
        source.define_function(
            target,
            CpsFunction {
                debug_name: Some("target".into()),
                params: vec![],
                return_cont: target_return,
                body: target_body,
            },
        );

        let main_return = source.reserve_continuation();
        let result = source.add_value(Some("result".into()));
        let escape = source.add_node(CpsNode::ApplyCont(CpsEdge {
            target: main_return,
            args: vec![CpsAtom::Fun(target)],
        }));
        let resume = source.add_continuation(CpsContinuation {
            debug_name: Some("resume".into()),
            params: vec![result],
            body: escape,
        });
        let call = source.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(target),
            args: vec![],
            return_to: resume,
        });
        let with_resume = source.add_node(CpsNode::LetCont {
            continuations: vec![resume],
            body: call,
        });
        let main_body = source.add_node(CpsNode::LetFun {
            functions: vec![target],
            body: with_resume,
        });
        source.define_function(
            main,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont: main_return,
                body: main_body,
            },
        );
        source.set_entry(main);

        let machine = lower(&source);
        let main = &machine.functions[&main];
        assert!(main.blocks.values().any(|block| matches!(
            &block.terminator,
            MachineTerminator::DirectCall { function, .. } if *function == target
        )));
        assert_eq!(machine_make_closures(main, target), 1);
    }

    #[test]
    fn sibling_recursion_creates_no_shell() {
        let mut source = CpsModule::new();
        let main = source.reserve_function();
        let ping = source.reserve_function();
        let pong = source.reserve_function();

        let ping_return = source.reserve_continuation();
        let ping_body = source.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(pong),
            args: vec![],
            return_to: ping_return,
        });
        source.define_function(
            ping,
            CpsFunction {
                debug_name: Some("ping".into()),
                params: vec![],
                return_cont: ping_return,
                body: ping_body,
            },
        );

        let pong_return = source.reserve_continuation();
        let pong_body = source.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(ping),
            args: vec![],
            return_to: pong_return,
        });
        source.define_function(
            pong,
            CpsFunction {
                debug_name: Some("pong".into()),
                params: vec![],
                return_cont: pong_return,
                body: pong_body,
            },
        );

        let main_return = source.reserve_continuation();
        let call = source.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(ping),
            args: vec![],
            return_to: main_return,
        });
        let main_body = source.add_node(CpsNode::LetFun {
            functions: vec![ping, pong],
            body: call,
        });
        source.define_function(
            main,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont: main_return,
                body: main_body,
            },
        );
        source.set_entry(main);

        let machine = lower(&source);
        assert_eq!(machine_shell_count(&machine), 0);
    }

    #[test]
    fn ordinary_recursion_creates_no_shell() {
        let mut source = CpsModule::new();
        let main = source.reserve_function();
        let repeat = source.reserve_function();

        let repeat_return = source.reserve_continuation();
        let repeat_body = source.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(repeat),
            args: vec![],
            return_to: repeat_return,
        });
        source.define_function(
            repeat,
            CpsFunction {
                debug_name: Some("repeat".into()),
                params: vec![],
                return_cont: repeat_return,
                body: repeat_body,
            },
        );

        let main_return = source.reserve_continuation();
        let call = source.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(repeat),
            args: vec![],
            return_to: main_return,
        });
        let main_body = source.add_node(CpsNode::LetFun {
            functions: vec![repeat],
            body: call,
        });
        source.define_function(
            main,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont: main_return,
                body: main_body,
            },
        );
        source.set_entry(main);

        let machine = lower(&source);
        assert_eq!(machine_shell_count(&machine), 0);
    }

    /// A nullary `main` that immediately exits — the smallest valid machine
    /// module, used to seed the verifier-rejection tests.
    fn exiting_main() -> (CpsModule, CpsFunId) {
        let mut source = CpsModule::new();
        let main = source.reserve_function();
        let return_cont = source.reserve_continuation();
        let body = source.add_node(CpsNode::Exit {
            value: Some(CpsAtom::Literal(CpsLiteral::Nat(0))),
        });
        source.define_function(
            main,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont,
                body,
            },
        );
        source.set_entry(main);
        (source, main)
    }

    #[test]
    fn verify_rejects_a_function_without_its_entry_block() {
        let (source, _) = exiting_main();
        let mut machine = lower(&source);

        let entry = machine.entry;
        let function = machine.functions.get_mut(&entry).unwrap();
        let entry_block = function.entry;
        function.blocks.remove(&entry_block);

        let error = machine.verify().unwrap_err();
        assert!(
            error.to_string().contains("has no entry block"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn verify_rejects_a_nested_block_with_no_lexical_owner() {
        // A `LetCont` continuation becomes a block nested under the entry; drop
        // the scope table and it is left without a lexical owner.
        let mut source = CpsModule::new();
        let main = source.reserve_function();
        let return_cont = source.reserve_continuation();
        let bound = source.add_value(Some("bound".into()));
        let exit = source.add_node(CpsNode::Exit {
            value: Some(CpsAtom::Value(bound)),
        });
        let resume = source.add_continuation(CpsContinuation {
            debug_name: Some("resume".into()),
            params: vec![bound],
            body: exit,
        });
        let enter = source.add_node(CpsNode::ApplyCont(CpsEdge {
            target: resume,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
        }));
        let body = source.add_node(CpsNode::LetCont {
            continuations: vec![resume],
            body: enter,
        });
        source.define_function(
            main,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont,
                body,
            },
        );
        source.set_entry(main);

        let mut machine = lower(&source);
        let entry = machine.entry;
        let function = machine.functions.get_mut(&entry).unwrap();
        assert!(
            function.blocks.len() >= 2,
            "fixture must lower to a nested block"
        );
        function.block_scopes.clear();

        let error = machine.verify().unwrap_err();
        assert!(
            error.to_string().contains("has no lexical owner"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn verify_rejects_a_fallback_shell_for_a_non_escaping_function() {
        // The residual mixed-init fallback is reserved for escaping closures; a
        // shell over a function with no wrapper is a function-only knot leaking
        // into fallback lowering, which the verifier must reject.
        let (mut source, _) = exiting_main();
        let mut machine = lower(&source);

        let result = value_id(source.add_value(None));
        let entry = machine.entry;
        let function = machine.functions.get_mut(&entry).unwrap();
        let entry_block = function.entry;
        function
            .blocks
            .get_mut(&entry_block)
            .unwrap()
            .instructions
            .push(MachineInstruction::FallbackShell {
                result,
                function: entry,
            });

        let error = machine.verify().unwrap_err();
        assert!(
            error
                .to_string()
                .contains("fallback shell for non-escaping"),
            "unexpected error: {error}"
        );
    }
}
