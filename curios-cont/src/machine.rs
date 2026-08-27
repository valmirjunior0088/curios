//! Private closed control-flow graph produced after high-CPS optimization.
//!
//! Functions have explicit blocks and block parameters. The CPS return continuation never becomes a block: a jump to the current function's bodyless sentinel is translated directly to [`MachineTerminator::Return`].

use {
    crate::{
        CpsAtom, CpsCallee, CpsCellOp, CpsContId, CpsEdge, CpsFunId, CpsFunction, CpsIntrinsic,
        CpsIntrinsicCall, CpsLiteral, CpsModule, CpsNode, CpsNodeId, CpsRow, CpsRowId,
        CpsValueExpr, CpsValueId, atoms,
    },
    curios_abi::ForeignFunction,
    curios_utilities::{Entropy, id},
    std::{
        collections::{BTreeMap, BTreeSet, VecDeque},
        fmt,
        sync::Arc,
    },
};

mod structurize;
pub(crate) use structurize::{structurize, value_name};

// Sigils follow the naming scheme shared with `curios-ersd` and `curios-wasm` — see `documentation/design/toolchain/one-naming-scheme-for-compiler-identities.md`.
id!(MachineBlockId, "~b", mint);
id!(MachineValueId, "~v", mint);

#[derive(Debug, Clone)]
pub(crate) enum MachineOperand {
    Value(MachineValueId),
    Literal(CpsLiteral),
    /// [`CpsAtom::Filler`] carried through unchanged: still no value, still waiting for the destination's carrier to be known.
    Filler,
}

#[derive(Debug, Clone)]
pub(crate) enum MachineConstruct {
    Literal(CpsLiteral),
    List(Vec<MachineOperand>),
    Tuple(Vec<MachineOperand>),
    Row(CpsRowId, Vec<MachineOperand>),
}

#[derive(Debug, Clone)]
pub(crate) enum MachineInstruction {
    Construct {
        result: MachineValueId,
        value: MachineConstruct,
    },
    Intrinsic {
        result: MachineValueId,
        op: CpsIntrinsic,
        args: Vec<MachineOperand>,
    },
    /// Retained-ABI closure wrapper around direct code. The wrapper unpacks these captures and tail-calls `function`.
    MakeClosure {
        result: MachineValueId,
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
    /// Hand every operand back to the caller. The vector is the return continuation's argument list, which the CPS verifier has never constrained to one — so a protocol delivering a constructor as its fields needs no widening here, only a producer that builds more than one.
    Return(Vec<MachineOperand>),
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
        op: CpsIntrinsicCall,
        args: Vec<MachineOperand>,
        resume: MachineBlockId,
    },
    IntrinsicReturn {
        op: CpsIntrinsicCall,
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
    /// How many values every `Return` in this function carries, read off the Cont module rather than recounted here — a caller has to know it to size the block that resumes the call, and a block cannot be sized from a terminator it has not reached yet.
    results: usize,
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
    /// Each function's source hint, carried from the Cont module so emission names can spell a function's origin (`func/{index}$hint`). A hint never affects identity — the `CpsFunId` index does — so a missing entry only omits the hint.
    function_hints: BTreeMap<CpsFunId, String>,
    /// Every nominal row the Cont module declared, with its debug name and slot carriers — carried whole rather than collected from constructions, so a row whose constructions were all optimized away still declares a type for the projections that may outlive them.
    rows: Vec<(CpsRowId, CpsRow)>,
}

impl MachineModule {
    /// The source hint of a function, if it carries one.
    pub(crate) fn function_hint(&self, id: CpsFunId) -> Option<&str> {
        self.function_hints.get(&id).map(String::as_str)
    }

    pub(crate) fn rows(&self) -> &[(CpsRowId, CpsRow)] {
        &self.rows
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

    let arities = source.return_arities();
    let mut functions = BTreeMap::new();
    for (index, function) in source.functions().iter().enumerate() {
        let Some(function) = function else { continue };
        let id = CpsFunId::from_index(index);
        let results = arities.get(&id).copied().unwrap_or(1);
        let lowered =
            MachineFunctionLowerer::new(source, id, function, &free_values, results).lower();
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
    let rows = source.rows().map(|(id, row)| (id, row.clone())).collect();
    let module = MachineModule {
        functions,
        wrappers,
        entry: source.entry().unwrap(),
        function_hints,
        rows,
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
            CpsNode::LetValue { result, next, .. } | CpsNode::LetIntrinsic { result, next, .. } => {
                bound.insert(value_id(*result));
                work.push(*next);
            }
            CpsNode::LetFun { body, .. } => work.push(*body),
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
            CpsNode::LetValue { result, .. } | CpsNode::LetIntrinsic { result, .. } => {
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
            CpsNode::LetValue { next, .. } | CpsNode::LetIntrinsic { next, .. } => work.push(*next),
            CpsNode::LetFun { body, .. } => work.push(*body),
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
    results: usize,
    blocks: BTreeMap<MachineBlockId, MachineBlock>,
    block_scopes: BTreeMap<MachineBlockId, Vec<MachineBlockId>>,
    continuation_blocks: BTreeMap<CpsContId, MachineBlockId>,
    materialized_closures: BTreeMap<CpsFunId, MachineValueId>,
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
        results: usize,
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
        mut node_id: CpsNodeId,
        params: Vec<MachineValueId>,
    ) -> MachineBlock {
        // Closure materializations are reused only within the block that defines them: cross-block values flow through explicit block parameters, so a materialized closure is in scope only for the rest of its own block.
        self.materialized_closures.clear();
        let mut instructions = Vec::new();
        loop {
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
                        CpsValueExpr::Row(row, values) => MachineConstruct::Row(
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
                CpsNode::LetIntrinsic {
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
                MachineTerminator::Return(
                    edge.args
                        .iter()
                        .map(|arg| self.lower_atom(arg, instructions))
                        .collect(),
                )
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
            CpsAtom::Filler => MachineOperand::Filler,
            CpsAtom::Fun(function) => {
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

pub(crate) fn value_id(value: CpsValueId) -> MachineValueId {
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
        owner: CpsFunId,
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
            MachineTerminator::Return(operands) if operands.len() != function.results => {
                return Err(MachineVerifyError(format!(
                    "{owner} returns {} values while its function returns {}",
                    operands.len(),
                    function.results
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
                verify_block_resume(function, *resume, callee.results)?;
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
                // A tail call is emitted as `return_call`, which hands the callee's results straight out of this function — so the two signatures must agree on results as well as on arguments.
                if callee.results != function.results {
                    return Err(MachineVerifyError(format!(
                        "{owner} tail-calls a function returning {} values while returning {}",
                        callee.results, function.results
                    )));
                }
            }
            MachineTerminator::IndirectCall { resume, .. } => {
                verify_block_resume(function, *resume, 1)?
            }
            MachineTerminator::Intrinsic {
                op: CpsIntrinsicCall::ListMap,
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
            MachineTerminator::TailIndirectCall { .. } => {}
            MachineTerminator::IntrinsicReturn {
                op: CpsIntrinsicCall::ListMap,
                args,
            } if args.len() != 2 => {
                return Err(MachineVerifyError(format!(
                    "{owner} tail ListMap has the wrong operand count"
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
mod tests;
