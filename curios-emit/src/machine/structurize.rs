//! Deterministic structurization from closed machine CFG into the private Wasm emission tree.

use {
    super::{
        MachineBlockId, MachineConstruct, MachineEdge, MachineFunction, MachineInstruction,
        MachineModule, MachineOperand, MachineTerminator, MachineValueId, MachineWrapper,
    },
    crate::{
        EmissionArg, EmissionBinder, EmissionBlock, EmissionBlockName, EmissionBody,
        EmissionBoxTarget, EmissionCallTarget, EmissionCellTarget, EmissionClosure,
        EmissionClosureName, EmissionCode, EmissionData, EmissionFrameTarget, EmissionFunction,
        EmissionFunctionName, EmissionHostTarget, EmissionJumpTarget, EmissionMatchTarget,
        EmissionModule, EmissionTail, EmissionValue, EmissionValueName,
    },
    curios_common::Entropy,
    curios_num::Natural,
    std::collections::{BTreeMap, BTreeSet},
};

pub fn structurize(machine: &MachineModule) -> EmissionModule {
    let mut output = EmissionModule::new();
    output.set_rows(machine.rows().to_vec());
    output.set_members(machine.members().to_vec());
    output.set_entry_suspension(machine.entry_suspension());
    output.set_rebuilt(rebuilt_slots(machine));
    for (function, wrapper) in &machine.wrappers {
        output.add_clsr(
            wrapper_name(machine, *function),
            convert_wrapper(machine, wrapper),
        );
    }
    for (function, wrapper) in &machine.wrappers {
        if let Some(twin) = wrapper.twin {
            output.add_heap_clsr(
                wrapper_name(machine, *function),
                convert_heap_wrapper(machine, wrapper, twin),
            );
        }
    }
    for (id, function) in &machine.functions {
        let resume = return_name(*id);
        let (region, minted) =
            MachineFunctionBridge::new(machine, *id, function, resume.clone()).convert();
        for (name, carrier) in minted {
            output.add_minted_register(name, carrier);
        }
        let params = function
            .free_values
            .iter()
            .chain(&function.params)
            .copied()
            .map(value_name)
            .map(EmissionBinder::from)
            .collect();
        output.add_func(
            function_name(machine, *id),
            EmissionFunction {
                params,
                results: function.results.clone(),
                resume,
                region,
            },
        );
    }
    output.set_entry(function_name(machine, machine.entry));
    output
}

/// The slots of each row some rebuild writes: the ones its struct declares mutable.
fn rebuilt_slots(machine: &MachineModule) -> BTreeMap<curios_cps::RowId, BTreeSet<usize>> {
    let mut rebuilt = BTreeMap::<_, BTreeSet<_>>::new();
    for function in machine.functions.values() {
        for block in function.blocks.values() {
            for instruction in &block.instructions {
                if let MachineInstruction::Rebuild { row, slots, .. } = instruction {
                    rebuilt.entry(*row).or_default().extend(slots);
                }
            }
        }
    }
    rebuilt
}

fn convert_wrapper(machine: &MachineModule, wrapper: &MachineWrapper) -> EmissionClosure {
    let resume = EmissionBlockName::from(format!("return/wrapper/{}", wrapper.function.index()));
    let fields = wrapper
        .captures
        .iter()
        .copied()
        .map(value_name)
        .map(EmissionBinder::from)
        .collect::<Vec<_>>();
    let params = (0..wrapper.arity)
        .map(|index| {
            EmissionValueName::from(format!("wrapper/{}/arg/{index}", wrapper.function.index()))
        })
        .map(EmissionBinder::from)
        .collect::<Vec<_>>();
    let call_params = fields
        .iter()
        .chain(&params)
        .map(|argument| EmissionArg::Value(argument.name.clone()))
        .collect();
    EmissionClosure {
        fields,
        params,
        resume: resume.clone(),
        region: EmissionBody::new(
            vec![],
            vec![],
            EmissionTail::Call(EmissionCallTarget::Direct {
                target: direct_name(machine, wrapper.function),
                params: call_params,
                resume,
            }),
        ),
    }
}

/// The heap entry of `wrapper`'s closure: the closure's environment read as the closure reads it, handed with the arguments and the continuation to `twin`, which takes what of the environment it needs.
fn convert_heap_wrapper(
    machine: &MachineModule,
    wrapper: &MachineWrapper,
    twin: curios_cps::FunctionId,
) -> EmissionClosure {
    let index = wrapper.function.index();
    let resume = EmissionBlockName::from(format!("return/heap/{index}"));
    let fields = wrapper
        .captures
        .iter()
        .copied()
        .map(value_name)
        .map(EmissionBinder::from)
        .collect::<Vec<_>>();
    let params = (0..=wrapper.arity)
        .map(|position| match position == wrapper.arity {
            true => EmissionValueName::from(format!("heap/{index}/k")),
            false => EmissionValueName::from(format!("heap/{index}/arg/{position}")),
        })
        .map(EmissionBinder::from)
        .collect::<Vec<_>>();
    let call_params = machine.functions[&twin]
        .free_values
        .iter()
        .copied()
        .map(value_name)
        .chain(params.iter().map(|param| param.name.clone()))
        .map(EmissionArg::Value)
        .collect();
    EmissionClosure {
        fields,
        params,
        resume: resume.clone(),
        region: EmissionBody::new(
            vec![],
            vec![],
            EmissionTail::Call(EmissionCallTarget::Direct {
                target: direct_name(machine, twin),
                params: call_params,
                resume,
            }),
        ),
    }
}

struct MachineFunctionBridge<'a> {
    machine: &'a MachineModule,
    function_id: curios_cps::FunctionId,
    function: &'a MachineFunction,
    resume: EmissionBlockName,
    literal_entropy: Entropy,
    /// The values this bridge minted that a register holds, at their carriers. See [`EmissionModule::minted_registers`].
    minted: Vec<(EmissionValueName, curios_cps::Repr)>,
}

impl<'a> MachineFunctionBridge<'a> {
    fn new(
        machine: &'a MachineModule,
        function_id: curios_cps::FunctionId,
        function: &'a MachineFunction,
        resume: EmissionBlockName,
    ) -> Self {
        Self {
            machine,
            function_id,
            function,
            resume,
            literal_entropy: Entropy::new(),
            minted: Vec::new(),
        }
    }

    /// The function's body, and the values minted for it that a register holds.
    fn convert(mut self) -> (EmissionBody, Vec<(EmissionValueName, curios_cps::Repr)>) {
        let body = self.convert_block(self.function.entry);
        (body, self.minted)
    }

    fn convert_block(&mut self, id: MachineBlockId) -> EmissionBody {
        let block = &self.function.blocks[&id];
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
                            EmissionData::Tuple(self.operands(elements, &mut values)),
                        ),
                        // A variant's slots are the second position that may defer a filler: the slot's carrier is the row's to declare, so materialising a zero here would pick the wrong one exactly where a slot is typed.
                        MachineConstruct::Row(row, elements) => EmissionValue::Pure(
                            EmissionData::Row(*row, self.jump_args(elements, &mut values)),
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
                // A rebuild's slots defer their fillers as a construction's do, the carrier being the row's to declare.
                MachineInstruction::Rebuild {
                    result,
                    row,
                    of,
                    slots,
                    values: written,
                } => {
                    let of = self.operand(of, &mut values);
                    let written = self.jump_args(written, &mut values);
                    values.push((
                        value_name(*result),
                        EmissionValue::Eval(EmissionCode::Rebuild {
                            row: *row,
                            of,
                            slots: slots.clone(),
                            values: written,
                        }),
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
        EmissionBody::new(values, blocks, tail)
    }

    fn convert_scoped_block(
        &mut self,
        block: MachineBlockId,
    ) -> (EmissionBlockName, EmissionBlock) {
        let source = &self.function.blocks[&block];
        (
            self.block_name(block),
            EmissionBlock {
                params: source.params.iter().copied().map(value_name).collect(),
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
                params: self.jump_args(operands, values),
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
            } => {
                let machine = self.machine;
                EmissionTail::Call(EmissionCallTarget::Direct {
                    target: direct_name(machine, *function),
                    params: self.jump_args(args, values),
                    resume: self.resume_target(
                        *resume,
                        &machine.functions[function].results,
                        blocks,
                    ),
                })
            }
            MachineTerminator::TailDirectCall { function, args } => {
                EmissionTail::Call(EmissionCallTarget::Direct {
                    target: direct_name(self.machine, *function),
                    params: self.jump_args(args, values),
                    resume: self.resume.clone(),
                })
            }
            MachineTerminator::IndirectCall {
                closure,
                args,
                resume,
            } => EmissionTail::Call(EmissionCallTarget::Indirect {
                target: value_name(*closure),
                params: self.jump_args(args, values),
                resume: self.resume_target(*resume, &[None], blocks),
            }),
            MachineTerminator::TailIndirectCall { closure, args } => {
                EmissionTail::Call(EmissionCallTarget::Indirect {
                    target: value_name(*closure),
                    params: self.jump_args(args, values),
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
                resume: self.resume_target(
                    *resume,
                    &vec![None; function.signature().imported_results().len()],
                    blocks,
                ),
            }),
            MachineTerminator::ForeignReturn { function, args } => {
                EmissionTail::Host(EmissionHostTarget::Foreign {
                    function: function.clone(),
                    operands: self.operands(args, values),
                    resume: self.resume.clone(),
                })
            }
            MachineTerminator::Cell { op, args, resume } => {
                let resume = self.resume_target(*resume, &vec![None; op.result_arity()], blocks);
                self.cell(*op, args, resume, values)
            }
            MachineTerminator::CellReturn { op, args } => {
                self.cell(*op, args, self.resume.clone(), values)
            }
            MachineTerminator::Box { op, args, resume } => {
                let resume = self.resume_target(*resume, &vec![None; op.result_arity()], blocks);
                self.box_(*op, args, resume, values)
            }
            MachineTerminator::BoxReturn { op, args } => {
                self.box_(*op, args, self.resume.clone(), values)
            }
            MachineTerminator::Intrinsic {
                op: curios_cps::IntrinsicCall::ListMap,
                args,
                resume,
            } => {
                let resume = self.resume_target(*resume, &[None], blocks);
                self.list_map(args, resume, values)
            }
            MachineTerminator::IntrinsicReturn {
                op: curios_cps::IntrinsicCall::ListMap,
                args,
            } => self.list_map(args, self.resume.clone(), values),
            MachineTerminator::Intrinsic {
                op: curios_cps::IntrinsicCall::HeapApply,
                args,
                resume,
            } => {
                let resume = self.resume_target(*resume, &[None], blocks);
                self.heap_apply(args, resume, values)
            }
            MachineTerminator::IntrinsicReturn {
                op: curios_cps::IntrinsicCall::HeapApply,
                args,
            } => self.heap_apply(args, self.resume.clone(), values),
            MachineTerminator::Intrinsic {
                op:
                    op @ (curios_cps::IntrinsicCall::FrameEnter { .. }
                    | curios_cps::IntrinsicCall::FrameLeave { .. }),
                resume,
                ..
            } => {
                let resume = self.resume_target(*resume, &vec![None; op.result_arity()], blocks);
                EmissionTail::Frame(EmissionFrameTarget { op: *op, resume })
            }
            MachineTerminator::IntrinsicReturn {
                op:
                    op @ (curios_cps::IntrinsicCall::FrameEnter { .. }
                    | curios_cps::IntrinsicCall::FrameLeave { .. }),
                ..
            } => EmissionTail::Frame(EmissionFrameTarget {
                op: *op,
                resume: self.resume.clone(),
            }),
            MachineTerminator::Halt { function, args } => {
                EmissionTail::Host(EmissionHostTarget::Halt {
                    function: function.clone(),
                    operands: self.operands(args, values),
                })
            }
            MachineTerminator::Panic(panic) => EmissionTail::Panic(*panic),
            MachineTerminator::Unreachable => EmissionTail::Unreachable,
        }
    }

    /// The block a call or an operation resumes into, handing over results held at `carriers`: a block minted in front of `target` takes the results as parameters of its own, each held as it arrives, and jumps on to `target`, the edge loading each as the parameter it feeds is held.
    fn resume_target(
        &mut self,
        target: MachineBlockId,
        carriers: &[Option<curios_cps::Repr>],
        blocks: &mut Vec<(EmissionBlockName, EmissionBlock)>,
    ) -> EmissionBlockName {
        let synthetic = self.literal_entropy.fresh();
        let resume =
            EmissionBlockName::from(format!("resume/{}/{}", self.function_id.index(), synthetic));
        let results = carriers
            .iter()
            .enumerate()
            .map(|(index, carrier)| {
                let name = EmissionValueName::from(format!(
                    "resume/{}/{}/result/{index}",
                    self.function_id.index(),
                    synthetic
                ));
                if let Some(carrier) = carrier {
                    self.minted.push((name.clone(), *carrier));
                }
                name
            })
            .collect::<Vec<_>>();
        let params = results.iter().cloned().map(EmissionArg::Value).collect();
        blocks.push((
            resume.clone(),
            EmissionBlock {
                params: results,
                region: EmissionBody::new(
                    vec![],
                    vec![],
                    EmissionTail::Jump(EmissionJumpTarget {
                        target: self.block_name(target),
                        params,
                    }),
                ),
            },
        ));
        resume
    }

    fn cell(
        &mut self,
        op: curios_cps::CellOp,
        args: &[MachineOperand],
        resume: EmissionBlockName,
        values: &mut Vec<(EmissionValueName, EmissionValue)>,
    ) -> EmissionTail {
        let args = self.operands(args, values);
        EmissionTail::Cell(match op {
            curios_cps::CellOp::Reserve => EmissionCellTarget::Reserve { resume },
            curios_cps::CellOp::Fill => EmissionCellTarget::Fill {
                cell: args[0].clone(),
                value: args[1].clone(),
                resume,
            },
            curios_cps::CellOp::Poll => EmissionCellTarget::Poll {
                cell: args[0].clone(),
                resume,
            },
        })
    }

    fn box_(
        &mut self,
        op: curios_cps::BoxOp,
        args: &[MachineOperand],
        resume: EmissionBlockName,
        values: &mut Vec<(EmissionValueName, EmissionValue)>,
    ) -> EmissionTail {
        EmissionTail::Box(EmissionBoxTarget {
            op,
            args: self.operands(args, values),
            resume,
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
            params: vec![EmissionArg::Value(result)],
        })
    }

    /// The call of a closure's heap entry, `args` the closure, its arguments and then the continuation.
    fn heap_apply(
        &mut self,
        args: &[MachineOperand],
        resume: EmissionBlockName,
        values: &mut Vec<(EmissionValueName, EmissionValue)>,
    ) -> EmissionTail {
        let [MachineOperand::Value(closure), params @ ..] = args else {
            unreachable!("a heap call's first operand is its closure");
        };
        EmissionTail::Call(EmissionCallTarget::Heap {
            target: value_name(*closure),
            params: self.jump_args(params, values),
            resume,
        })
    }

    fn edge(
        &mut self,
        edge: &MachineEdge,
        values: &mut Vec<(EmissionValueName, EmissionValue)>,
    ) -> EmissionJumpTarget {
        EmissionJumpTarget {
            target: self.block_name(edge.target),
            params: self.jump_args(&edge.args, values),
        }
    }

    /// The arguments an edge or a call passes or a variant construction stores, with a filler left unmaterialised: these are the positions whose destination carrier is decided later, so they are the ones that may defer.
    fn jump_args(
        &mut self,
        operands: &[MachineOperand],
        values: &mut Vec<(EmissionValueName, EmissionValue)>,
    ) -> Vec<EmissionArg> {
        operands
            .iter()
            .map(|operand| match operand {
                MachineOperand::Filler => EmissionArg::Filler,
                operand => EmissionArg::Value(self.operand(operand, values)),
            })
            .collect()
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
            // Every transfer defers its fillers through `jump_args`; the positions left to this arm read a scalar — an intrinsic operand, a host operand, an exit code — and a filler never reaches one.
            MachineOperand::Filler => {
                self.literal(&curios_cps::Literal::Nat(Natural::zero()), values)
            }
        }
    }

    fn literal(
        &mut self,
        literal: &curios_cps::Literal,
        values: &mut Vec<(EmissionValueName, EmissionValue)>,
    ) -> EmissionValueName {
        let synthetic = self.literal_entropy.fresh();
        let name = EmissionValueName::from(format!(
            "literal/{}/{}",
            self.function_id.index(),
            synthetic
        ));
        // A `Nat` the i31 holds is its own word: held in a register, it reaches an operation's fast path as one, neither boxed nor tested.
        if let curios_cps::Literal::Nat(value) = literal
            && curios_cps::nat_is_small(value)
        {
            self.minted.push((name.clone(), curios_cps::Repr::Nat));
        }
        values.push((name.clone(), EmissionValue::Pure(literal_data(literal))));
        name
    }

    fn block_name(&self, block: MachineBlockId) -> EmissionBlockName {
        EmissionBlockName::from(format!("block/{}/{}", self.function_id.index(), block.0))
    }
}

fn literal_data(literal: &curios_cps::Literal) -> EmissionData {
    match literal {
        curios_cps::Literal::Nat(value) => EmissionData::Nat(value.clone()),
        curios_cps::Literal::Int(value) => EmissionData::Int(value.clone()),
        curios_cps::Literal::Flt(value) => EmissionData::Flt(f64::from(*value)),
        curios_cps::Literal::Bytes(value) => EmissionData::Bytes(value.clone()),
    }
}

/// Spell an emission entity as `{index}$hint`, or bare `{index}` when it carries no source hint. The `$` marks a user-origin name (the wasm scheme's convention), while the leading index is the uniquifier that actually carries identity.
fn hinted(index: usize, hint: Option<&str>) -> String {
    match hint {
        Some(hint) => format!("{index}${hint}"),
        None => index.to_string(),
    }
}
pub fn value_name(value: MachineValueId) -> EmissionValueName {
    EmissionValueName::from(format!("m{}", value.0))
}
/// What the emitter calls the CPS function `function`, of `hint`: `main` where it is the program's entry, and `{index}$hint` otherwise — the spelling of its body and, entry or not, of the closure it is made into where it escapes as one ([`emitted_functions`](crate::emitted_functions)).
pub fn emitted_name(function: curios_cps::FunctionId, hint: Option<&str>, entry: bool) -> String {
    match entry {
        true => "main".to_string(),
        false => hinted(function.index(), hint),
    }
}
fn direct_name(machine: &MachineModule, function: curios_cps::FunctionId) -> EmissionFunctionName {
    EmissionFunctionName::from(emitted_name(
        function,
        machine.function_hint(function),
        false,
    ))
}
fn wrapper_name(machine: &MachineModule, function: curios_cps::FunctionId) -> EmissionClosureName {
    EmissionClosureName::from(emitted_name(
        function,
        machine.function_hint(function),
        false,
    ))
}
fn return_name(function: curios_cps::FunctionId) -> EmissionBlockName {
    EmissionBlockName::from(format!("return/{}", function.index()))
}
fn function_name(
    machine: &MachineModule,
    function: curios_cps::FunctionId,
) -> EmissionFunctionName {
    EmissionFunctionName::from(emitted_name(
        function,
        machine.function_hint(function),
        function == machine.entry,
    ))
}
