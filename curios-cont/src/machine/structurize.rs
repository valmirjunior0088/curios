//! Deterministic structurization from closed machine CFG into the private Wasm emission tree.

use {
    super::{
        MachineBlock, MachineBlockId, MachineConstruct, MachineEdge, MachineFunction,
        MachineInstruction, MachineModule, MachineOperand, MachineTerminator, MachineValueId,
        MachineWrapper,
    },
    crate::{
        CpsCellOp, CpsFunId, CpsIntrinsicCall, CpsLiteral,
        into_wasm::{
            EmissionArgument, EmissionBlock, EmissionBlockName, EmissionBody, EmissionCallTarget,
            EmissionCellTarget, EmissionClosure, EmissionClosureName, EmissionCode, EmissionData,
            EmissionFunction, EmissionFunctionName, EmissionHostTarget, EmissionJumpTarget,
            EmissionMatchTarget, EmissionModule, EmissionTail, EmissionValue, EmissionValueName,
        },
    },
    curios_base::Entropy,
    std::collections::{BTreeMap, BTreeSet},
};

pub(crate) fn structurize(machine: &MachineModule) -> EmissionModule {
    let mut output = EmissionModule::new();
    for (function, wrapper) in &machine.wrappers {
        output.add_clsr(
            wrapper_name(machine, *function),
            convert_wrapper(machine, wrapper),
        );
    }
    for (id, function) in &machine.functions {
        let resume = return_name(*id);
        let region = MachineFunctionBridge::new(machine, *id, function, resume.clone()).convert();
        let params = function
            .free_values
            .iter()
            .chain(&function.params)
            .copied()
            .map(value_name)
            .map(EmissionArgument::from)
            .collect();
        output.add_func(
            function_name(machine, *id),
            EmissionFunction {
                params,
                // One value, because every return terminator carries one until a protocol builds more.
                results: 1,
                resume,
                region,
            },
        );
    }
    output.set_entry(function_name(machine, machine.entry));
    output
}

fn convert_wrapper(machine: &MachineModule, wrapper: &MachineWrapper) -> EmissionClosure {
    let resume = EmissionBlockName::from(format!("return/wrapper/{}", wrapper.function.index()));
    let fields = wrapper
        .captures
        .iter()
        .copied()
        .map(value_name)
        .map(EmissionArgument::from)
        .collect::<Vec<_>>();
    let params = (0..wrapper.arity)
        .map(|index| {
            EmissionValueName::from(format!("wrapper/{}/arg/{index}", wrapper.function.index()))
        })
        .map(EmissionArgument::from)
        .collect::<Vec<_>>();
    let call_params = fields
        .iter()
        .chain(&params)
        .map(|argument| argument.name.clone())
        .collect();
    EmissionClosure {
        fields,
        params,
        resume: resume.clone(),
        region: EmissionBody {
            shells: vec![],
            values: vec![],
            blocks: vec![],
            tail: EmissionTail::Call(EmissionCallTarget::Direct {
                target: direct_name(machine, wrapper.function),
                params: call_params,
                resume,
            }),
        },
    }
}

struct MachineFunctionBridge<'a> {
    machine: &'a MachineModule,
    function_id: CpsFunId,
    function: &'a MachineFunction,
    resume: EmissionBlockName,
    literal_entropy: Entropy,
    block_captures: BTreeMap<MachineBlockId, Vec<MachineValueId>>,
}

impl<'a> MachineFunctionBridge<'a> {
    fn new(
        machine: &'a MachineModule,
        function_id: CpsFunId,
        function: &'a MachineFunction,
        resume: EmissionBlockName,
    ) -> Self {
        let block_captures = block_captures(function);
        Self {
            machine,
            function_id,
            function,
            resume,
            literal_entropy: Entropy::new(),
            block_captures,
        }
    }

    fn convert(mut self) -> EmissionBody {
        self.convert_block(self.function.entry)
    }

    fn convert_block(&mut self, id: MachineBlockId) -> EmissionBody {
        let block = &self.function.blocks[&id];
        let mut shells = Vec::new();
        let mut values = Vec::new();
        let mut blocks = Vec::new();
        for instruction in &block.instructions {
            match instruction {
                MachineInstruction::Construct { result, value } => {
                    let value = match value {
                        MachineConstruct::Literal(literal) => {
                            EmissionValue::Pure(literal_data(literal))
                        }
                        MachineConstruct::List(elements) => EmissionValue::Pure(
                            EmissionData::List(self.operands(elements, &mut values)),
                        ),
                        MachineConstruct::Tuple(elements) => EmissionValue::Pure(
                            EmissionData::Tpl(self.operands(elements, &mut values)),
                        ),
                    };
                    values.push((value_name(*result), value));
                }
                MachineInstruction::Intrinsic { result, op, args } => {
                    let args = self.operands(args, &mut values);
                    values.push((
                        value_name(*result),
                        EmissionValue::Eval(EmissionCode::Intrinsic(*op, args)),
                    ));
                }
                MachineInstruction::MakeClosure {
                    result,
                    function,
                    captures,
                } => {
                    let captures = self.operands(captures, &mut values);
                    values.push((
                        value_name(*result),
                        EmissionValue::Pure(EmissionData::Closure(
                            wrapper_name(self.machine, *function),
                            captures,
                        )),
                    ));
                }
                MachineInstruction::FallbackShell { result, function } => {
                    shells.push((value_name(*result), wrapper_name(self.machine, *function)));
                }
                MachineInstruction::FallbackFill {
                    shell,
                    function,
                    captures,
                } => {
                    let captures = self.operands(captures, &mut values);
                    values.push((
                        value_name(*shell),
                        EmissionValue::Pure(EmissionData::Closure(
                            wrapper_name(self.machine, *function),
                            captures,
                        )),
                    ));
                }
            }
        }
        let tail = self.terminator(&block.terminator, &mut values, &mut blocks);
        let scoped = self
            .function
            .block_scopes
            .get(&id)
            .cloned()
            .unwrap_or_default();
        blocks.extend(
            scoped
                .into_iter()
                .map(|block| self.convert_scoped_block(block)),
        );
        EmissionBody {
            shells,
            values,
            blocks,
            tail,
        }
    }

    fn convert_scoped_block(
        &mut self,
        block: MachineBlockId,
    ) -> (EmissionBlockName, EmissionBlock) {
        let source = &self.function.blocks[&block];
        (
            self.block_name(block),
            EmissionBlock {
                params: source
                    .params
                    .iter()
                    .chain(&self.block_captures[&block])
                    .copied()
                    .map(value_name)
                    .collect(),
                region: self.convert_block(block),
            },
        )
    }

    fn terminator(
        &mut self,
        terminator: &MachineTerminator,
        values: &mut Vec<(EmissionValueName, EmissionValue)>,
        blocks: &mut Vec<(EmissionBlockName, EmissionBlock)>,
    ) -> EmissionTail {
        match terminator {
            MachineTerminator::Return(operands) => EmissionTail::Jump(EmissionJumpTarget {
                target: self.resume.clone(),
                params: operands
                    .iter()
                    .map(|operand| self.operand(operand, values))
                    .collect(),
            }),
            MachineTerminator::Jump(edge) => EmissionTail::Jump(self.edge(edge, values)),
            MachineTerminator::Switch {
                scrutinee,
                cases,
                default,
            } => EmissionTail::Match(EmissionMatchTarget {
                operand: self.operand(scrutinee, values),
                cases: cases
                    .iter()
                    .map(|(tag, edge)| (*tag, self.edge(edge, values)))
                    .collect(),
                default: default.as_ref().map(|edge| self.edge(edge, values)),
            }),
            MachineTerminator::DirectCall {
                function,
                args,
                resume,
            } => EmissionTail::Call(EmissionCallTarget::Direct {
                target: direct_name(self.machine, *function),
                params: self.operands(args, values),
                resume: self.resume_target(*resume, 1, blocks),
            }),
            MachineTerminator::TailDirectCall { function, args } => {
                EmissionTail::Call(EmissionCallTarget::Direct {
                    target: direct_name(self.machine, *function),
                    params: self.operands(args, values),
                    resume: self.resume.clone(),
                })
            }
            MachineTerminator::IndirectCall {
                closure,
                args,
                resume,
            } => EmissionTail::Call(EmissionCallTarget::Indirect {
                target: value_name(*closure),
                params: self.operands(args, values),
                resume: self.resume_target(*resume, 1, blocks),
            }),
            MachineTerminator::TailIndirectCall { closure, args } => {
                EmissionTail::Call(EmissionCallTarget::Indirect {
                    target: value_name(*closure),
                    params: self.operands(args, values),
                    resume: self.resume.clone(),
                })
            }
            MachineTerminator::Foreign {
                function,
                args,
                resume,
            } => EmissionTail::Host(EmissionHostTarget::Foreign {
                function: function.clone(),
                operands: self.operands(args, values),
                resume: self.resume_target(*resume, function.signature.results.len(), blocks),
            }),
            MachineTerminator::ForeignReturn { function, args } => {
                EmissionTail::Host(EmissionHostTarget::Foreign {
                    function: function.clone(),
                    operands: self.operands(args, values),
                    resume: self.resume.clone(),
                })
            }
            MachineTerminator::Cell { op, args, resume } => {
                let resume = self.resume_target(*resume, op.result_arity(), blocks);
                self.cell(*op, args, resume, values)
            }
            MachineTerminator::CellReturn { op, args } => {
                self.cell(*op, args, self.resume.clone(), values)
            }
            MachineTerminator::Intrinsic {
                op: CpsIntrinsicCall::ListMap,
                args,
                resume,
            } => {
                let resume = self.resume_target(*resume, 1, blocks);
                self.list_map(args, resume, values)
            }
            MachineTerminator::IntrinsicReturn {
                op: CpsIntrinsicCall::ListMap,
                args,
            } => self.list_map(args, self.resume.clone(), values),
            MachineTerminator::Exit(value) => {
                let code = match value {
                    Some(value) => self.operand(value, values),
                    None => self.literal(&CpsLiteral::Nat(0), values),
                };
                EmissionTail::Host(EmissionHostTarget::Exit { code })
            }
            MachineTerminator::Unreachable => EmissionTail::Unreachable,
        }
    }

    fn resume_target(
        &mut self,
        target: MachineBlockId,
        result_arity: usize,
        blocks: &mut Vec<(EmissionBlockName, EmissionBlock)>,
    ) -> EmissionBlockName {
        let captures = &self.block_captures[&target];
        if captures.is_empty() {
            return self.block_name(target);
        }

        let synthetic = self.literal_entropy.fresh();
        let resume =
            EmissionBlockName::from(format!("resume/{}/{}", self.function_id.index(), synthetic));
        let results = (0..result_arity)
            .map(|index| {
                EmissionValueName::from(format!(
                    "resume/{}/{}/result/{index}",
                    self.function_id.index(),
                    synthetic
                ))
            })
            .collect::<Vec<_>>();
        let mut params = results.clone();
        params.extend(captures.iter().copied().map(value_name));
        blocks.push((
            resume.clone(),
            EmissionBlock {
                params: results,
                region: EmissionBody {
                    shells: vec![],
                    values: vec![],
                    blocks: vec![],
                    tail: EmissionTail::Jump(EmissionJumpTarget {
                        target: self.block_name(target),
                        params,
                    }),
                },
            },
        ));
        resume
    }

    fn cell(
        &mut self,
        op: CpsCellOp,
        args: &[MachineOperand],
        resume: EmissionBlockName,
        values: &mut Vec<(EmissionValueName, EmissionValue)>,
    ) -> EmissionTail {
        let args = self.operands(args, values);
        EmissionTail::Cell(match op {
            CpsCellOp::New => EmissionCellTarget::New {
                init: args[0].clone(),
                resume,
            },
            CpsCellOp::Set => EmissionCellTarget::Set {
                cell: args[0].clone(),
                value: args[1].clone(),
                resume,
            },
            CpsCellOp::Get => EmissionCellTarget::Get {
                cell: args[0].clone(),
                resume,
            },
        })
    }

    fn list_map(
        &mut self,
        args: &[MachineOperand],
        resume: EmissionBlockName,
        values: &mut Vec<(EmissionValueName, EmissionValue)>,
    ) -> EmissionTail {
        let args = self.operands(args, values);
        let synthetic = self.literal_entropy.fresh();
        let result = EmissionValueName::from(format!(
            "intrinsic/{}/{}",
            self.function_id.index(),
            synthetic
        ));
        values.push((
            result.clone(),
            EmissionValue::Eval(EmissionCode::ListMap(args[0].clone(), args[1].clone())),
        ));
        EmissionTail::Jump(EmissionJumpTarget {
            target: resume,
            params: vec![result],
        })
    }

    fn edge(
        &mut self,
        edge: &MachineEdge,
        values: &mut Vec<(EmissionValueName, EmissionValue)>,
    ) -> EmissionJumpTarget {
        let mut params = self.operands(&edge.args, values);
        params.extend(
            self.block_captures[&edge.target]
                .iter()
                .copied()
                .map(value_name),
        );
        EmissionJumpTarget {
            target: self.block_name(edge.target),
            params,
        }
    }

    fn operands(
        &mut self,
        operands: &[MachineOperand],
        values: &mut Vec<(EmissionValueName, EmissionValue)>,
    ) -> Vec<EmissionValueName> {
        operands
            .iter()
            .map(|operand| self.operand(operand, values))
            .collect()
    }

    fn operand(
        &mut self,
        operand: &MachineOperand,
        values: &mut Vec<(EmissionValueName, EmissionValue)>,
    ) -> EmissionValueName {
        match operand {
            MachineOperand::Value(value) => value_name(*value),
            MachineOperand::Literal(literal) => self.literal(literal, values),
        }
    }

    fn literal(
        &mut self,
        literal: &CpsLiteral,
        values: &mut Vec<(EmissionValueName, EmissionValue)>,
    ) -> EmissionValueName {
        let synthetic = self.literal_entropy.fresh();
        let name = EmissionValueName::from(format!(
            "literal/{}/{}",
            self.function_id.index(),
            synthetic
        ));
        values.push((name.clone(), EmissionValue::Pure(literal_data(literal))));
        name
    }

    fn block_name(&self, block: MachineBlockId) -> EmissionBlockName {
        EmissionBlockName::from(format!("block/{}/{}", self.function_id.index(), block.0))
    }
}

fn block_captures(function: &MachineFunction) -> BTreeMap<MachineBlockId, Vec<MachineValueId>> {
    let function_scope = function
        .free_values
        .iter()
        .chain(&function.params)
        .copied()
        .collect::<BTreeSet<_>>();
    let mut captures = function
        .blocks
        .keys()
        .copied()
        .map(|block| (block, BTreeSet::new()))
        .collect::<BTreeMap<_, _>>();

    loop {
        let previous = captures.clone();
        for (block_id, block) in &function.blocks {
            let mut bound = function_scope.clone();
            bound.extend(&block.params);
            for instruction in &block.instructions {
                match instruction {
                    MachineInstruction::Construct { result, .. }
                    | MachineInstruction::Intrinsic { result, .. }
                    | MachineInstruction::MakeClosure { result, .. }
                    | MachineInstruction::FallbackShell { result, .. } => {
                        bound.insert(*result);
                    }
                    MachineInstruction::FallbackFill { .. } => {}
                }
            }

            let mut required = block_operand_values(block);
            for successor in block_successors(&block.terminator) {
                required.extend(previous[&successor].iter().copied());
            }
            required.retain(|value| !bound.contains(value));
            captures.insert(*block_id, required);
        }
        if captures == previous {
            break;
        }
    }

    captures
        .into_iter()
        .map(|(block, values)| (block, values.into_iter().collect()))
        .collect()
}

fn block_operand_values(block: &MachineBlock) -> BTreeSet<MachineValueId> {
    let mut values = BTreeSet::new();
    let mut insert = |operand: &MachineOperand| {
        if let MachineOperand::Value(value) = operand {
            values.insert(*value);
        }
    };
    for instruction in &block.instructions {
        match instruction {
            MachineInstruction::Construct { value, .. } => match value {
                MachineConstruct::Literal(_) => {}
                MachineConstruct::List(operands) | MachineConstruct::Tuple(operands) => {
                    operands.iter().for_each(&mut insert);
                }
            },
            MachineInstruction::Intrinsic { args, .. }
            | MachineInstruction::MakeClosure { captures: args, .. } => {
                args.iter().for_each(&mut insert);
            }
            MachineInstruction::FallbackFill {
                shell, captures, ..
            } => {
                insert(&MachineOperand::Value(*shell));
                captures.iter().for_each(&mut insert);
            }
            MachineInstruction::FallbackShell { .. } => {}
        }
    }
    match &block.terminator {
        MachineTerminator::Return(operands) => operands.iter().for_each(&mut insert),
        MachineTerminator::Jump(edge) => edge.args.iter().for_each(&mut insert),
        MachineTerminator::Switch {
            scrutinee,
            cases,
            default,
        } => {
            insert(scrutinee);
            for edge in cases.values().chain(default.iter()) {
                edge.args.iter().for_each(&mut insert);
            }
        }
        MachineTerminator::DirectCall { args, .. }
        | MachineTerminator::TailDirectCall { args, .. }
        | MachineTerminator::Foreign { args, .. }
        | MachineTerminator::ForeignReturn { args, .. }
        | MachineTerminator::Cell { args, .. }
        | MachineTerminator::CellReturn { args, .. }
        | MachineTerminator::Intrinsic { args, .. }
        | MachineTerminator::IntrinsicReturn { args, .. } => args.iter().for_each(&mut insert),
        MachineTerminator::IndirectCall { closure, args, .. }
        | MachineTerminator::TailIndirectCall { closure, args } => {
            insert(&MachineOperand::Value(*closure));
            args.iter().for_each(&mut insert);
        }
        MachineTerminator::Exit(operand) => operand.iter().for_each(&mut insert),
        MachineTerminator::Unreachable => {}
    }
    values
}

fn block_successors(terminator: &MachineTerminator) -> Vec<MachineBlockId> {
    match terminator {
        MachineTerminator::Jump(edge) => vec![edge.target],
        MachineTerminator::Switch { cases, default, .. } => cases
            .values()
            .chain(default.iter())
            .map(|edge| edge.target)
            .collect(),
        MachineTerminator::DirectCall { resume, .. }
        | MachineTerminator::IndirectCall { resume, .. }
        | MachineTerminator::Foreign { resume, .. }
        | MachineTerminator::Cell { resume, .. }
        | MachineTerminator::Intrinsic { resume, .. } => vec![*resume],
        _ => vec![],
    }
}

fn literal_data(literal: &CpsLiteral) -> EmissionData {
    match literal {
        CpsLiteral::Nat(value) => EmissionData::Nat(*value),
        CpsLiteral::Int(value) => EmissionData::Int(*value),
        CpsLiteral::Flt(value) => EmissionData::Flt(*value),
        CpsLiteral::Bin(grain, value) => EmissionData::Bin(*grain, value.clone()),
    }
}

/// Spell an emission entity as `{index}$hint`, or bare `{index}` when it carries no source hint. The `$` marks a user-origin name (the wasm scheme's convention), while the leading index is the uniquifier that actually carries identity.
fn hinted(index: usize, hint: Option<&str>) -> String {
    match hint {
        Some(hint) => format!("{index}${hint}"),
        None => index.to_string(),
    }
}
pub(crate) fn value_name(value: MachineValueId) -> EmissionValueName {
    EmissionValueName::from(format!("m{}", value.0))
}
fn direct_name(machine: &MachineModule, function: CpsFunId) -> EmissionFunctionName {
    EmissionFunctionName::from(hinted(function.index(), machine.function_hint(function)))
}
fn wrapper_name(machine: &MachineModule, function: CpsFunId) -> EmissionClosureName {
    EmissionClosureName::from(hinted(function.index(), machine.function_hint(function)))
}
fn return_name(function: CpsFunId) -> EmissionBlockName {
    EmissionBlockName::from(format!("return/{}", function.index()))
}
fn function_name(machine: &MachineModule, function: CpsFunId) -> EmissionFunctionName {
    if function == machine.entry {
        EmissionFunctionName::from("main")
    } else {
        direct_name(machine, function)
    }
}
