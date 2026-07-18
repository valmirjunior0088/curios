use {
    crate::{
        BlockData, ClsrData, EmissionBlockName, EmissionCallTarget, EmissionCellTarget,
        EmissionFunctionName, EmissionHostTarget, EmissionJumpTarget, EmissionMatchTarget,
        EmissionTail, EmissionValueName, FieldData, Frame, FuncData, LocalData, Table,
    },
    curios_abi::WireType,
    curios_base::Entropy,
    std::{
        collections::{BTreeMap, HashMap},
        iter,
    },
};

fn is_sequential_from_zero(cases: &BTreeMap<u32, EmissionJumpTarget>) -> bool {
    cases.keys().enumerate().all(|(i, &k)| k == i as u32)
}

#[derive(Debug)]
pub(crate) enum Context<'a, 'b> {
    Const {
        table: &'a Table<'a>,
    },
    Closure {
        table: &'a Table<'a>,
        data: &'a ClsrData<'a>,
        entropy: Entropy,
        locals: &'b mut Vec<(curios_wasm::LocalName, curios_wasm::ValType)>,
        frames: Vec<Frame<'a>>,
    },
    Function {
        table: &'a Table<'a>,
        data: &'a FuncData<'a>,
        entropy: Entropy,
        locals: &'b mut Vec<(curios_wasm::LocalName, curios_wasm::ValType)>,
        frames: Vec<Frame<'a>>,
    },
}

impl<'a, 'b> Context<'a, 'b> {
    pub(crate) fn new_const(table: &'a Table<'a>) -> Self {
        Self::Const { table }
    }

    pub(crate) fn new_clsr(
        table: &'a Table<'a>,
        data: &'a ClsrData<'a>,
        locals: &'b mut Vec<(curios_wasm::LocalName, curios_wasm::ValType)>,
    ) -> Self {
        Self::Closure {
            table,
            data,
            entropy: Entropy::<usize>::new(),
            locals,
            frames: Vec::new(),
        }
    }

    pub(crate) fn new_func(
        table: &'a Table<'a>,
        data: &'a FuncData<'a>,
        locals: &'b mut Vec<(curios_wasm::LocalName, curios_wasm::ValType)>,
    ) -> Self {
        Self::Function {
            table,
            data,
            entropy: Entropy::<usize>::new(),
            locals,
            frames: Vec::new(),
        }
    }

    pub(crate) fn table(&self) -> &'a Table<'a> {
        match self {
            Self::Const { table } | Self::Closure { table, .. } | Self::Function { table, .. } => {
                table
            }
        }
    }

    pub(crate) fn find_field(&self, value_name: &EmissionValueName) -> Option<FieldData> {
        match self {
            Self::Const { .. } | Self::Function { .. } => None,
            Self::Closure { data, .. } => data.find_field(value_name),
        }
    }

    pub(crate) fn params(&self) -> HashMap<&'a EmissionValueName, LocalData> {
        match self {
            Self::Const { .. } => panic!("`Context` lacks params"),
            Self::Closure { data, .. } => data
                .params()
                .into_iter()
                .map(|(value_name, local_name)| (value_name, LocalData::new(local_name, false)))
                .collect(),
            Self::Function { data, .. } => data
                .params()
                .into_iter()
                .map(|(value_name, local_name)| (value_name, LocalData::new(local_name, false)))
                .collect(),
        }
    }

    pub(crate) fn is_resume(&self, block_name: &EmissionBlockName) -> bool {
        match self {
            Self::Const { .. } => false,
            Self::Closure { data, .. } => data.is_resume(block_name),
            Self::Function { data, .. } => data.is_resume(block_name),
        }
    }

    pub(crate) fn push_local(
        &mut self,
        string: &str,
        val_type: curios_wasm::ValType,
    ) -> curios_wasm::LocalName {
        match self {
            Self::Const { .. } => panic!("`Context` lacks locals"),
            Self::Closure {
                entropy, locals, ..
            }
            | Self::Function {
                entropy, locals, ..
            } => {
                let entropy = entropy.fresh();
                let local_name = if string.is_empty() {
                    curios_wasm::LocalName::from(format!("{entropy}"))
                } else {
                    curios_wasm::LocalName::from(format!("{entropy}${string}"))
                };

                locals.push((local_name.clone(), val_type));

                local_name
            }
        }
    }

    pub(crate) fn enter_frame(&mut self, frame: Frame<'a>) {
        match self {
            Self::Const { .. } => panic!("`Context` lacks frame stack"),
            Self::Closure { frames, .. } | Self::Function { frames, .. } => frames.push(frame),
        }
    }

    pub(crate) fn leave_frame(&mut self) -> Vec<curios_wasm::Instr> {
        match self {
            Self::Const { .. } => panic!("`Context` lacks frame stack"),
            Self::Closure { frames, .. } | Self::Function { frames, .. } => {
                frames.pop().expect("`Context` lacks frame").instrs
            }
        }
    }

    pub(crate) fn this_frame(&mut self) -> Option<&mut Frame<'a>> {
        match self {
            Self::Const { .. } => None,
            Self::Closure { frames, .. } | Self::Function { frames, .. } => frames.last_mut(),
        }
    }

    pub(crate) fn find_local(&self, value_name: &EmissionValueName) -> Option<LocalData> {
        match self {
            Self::Const { .. } => None,
            Self::Closure { frames, .. } | Self::Function { frames, .. } => {
                frames.iter().rev().find_map(|frame| {
                    frame
                        .values
                        .get(value_name)
                        .map(|local_name| LocalData::new(local_name.clone(), true))
                        .or_else(|| frame.params.get(value_name).cloned())
                })
            }
        }
    }

    pub(crate) fn is_shell(&self, value_name: &EmissionValueName) -> bool {
        match self {
            Self::Const { .. } => false,
            Self::Closure { frames, .. } | Self::Function { frames, .. } => {
                frames.iter().any(|frame| frame.shells.contains(value_name))
            }
        }
    }

    pub(crate) fn find_block(&self, block_name: &EmissionBlockName) -> &BlockData<'a> {
        match self {
            Self::Const { .. } => panic!("`Context` lacks frame stack"),
            Self::Closure { frames, .. } | Self::Function { frames, .. } => frames
                .iter()
                .rev()
                .find_map(|frame| {
                    frame
                        .blocks
                        .iter()
                        .find_map(|(frame_block_name, block_data)| {
                            (&block_name == frame_block_name).then_some(block_data)
                        })
                })
                .unwrap_or_else(|| panic!("`Context` lacks block `{}`", block_name)),
        }
    }

    fn load_as_instrs(&self, load_as: LoadAs, is_nullable: bool) -> Vec<curios_wasm::Instr> {
        match load_as {
            LoadAs::Null => vec![],
            LoadAs::NonNull => match is_nullable {
                true => vec![curios_wasm::Instr::RefAsNonNull],
                false => vec![],
            },
            LoadAs::Concrete(type_name) => {
                vec![curios_wasm::Instr::RefCast {
                    ref_type: curios_wasm::RefType {
                        is_nullable: false,
                        heap_type: curios_wasm::HeapType::Concrete(type_name),
                    },
                }]
            }
            LoadAs::Nat => {
                vec![
                    curios_wasm::Instr::RefCast {
                        ref_type: Table::int_type(false),
                    },
                    curios_wasm::Instr::I31GetU,
                ]
            }
            LoadAs::Int => {
                vec![
                    curios_wasm::Instr::RefCast {
                        ref_type: Table::int_type(false),
                    },
                    curios_wasm::Instr::I31GetS,
                ]
            }
            LoadAs::Flt => {
                vec![
                    curios_wasm::Instr::RefCast {
                        ref_type: curios_wasm::RefType {
                            is_nullable: false,
                            heap_type: curios_wasm::HeapType::Concrete(self.table().flt_type()),
                        },
                    },
                    curios_wasm::Instr::StructGet {
                        type_name: self.table().flt_type(),
                        field_name: self.table().special_field(),
                    },
                ]
            }
            LoadAs::Bin => {
                vec![curios_wasm::Instr::RefCast {
                    ref_type: curios_wasm::RefType {
                        is_nullable: false,
                        heap_type: curios_wasm::HeapType::Concrete(self.table().bin_rope_type()),
                    },
                }]
            }
            LoadAs::Lst => {
                vec![curios_wasm::Instr::RefCast {
                    ref_type: curios_wasm::RefType {
                        is_nullable: false,
                        heap_type: curios_wasm::HeapType::Concrete(self.table().lst_rope_type()),
                    },
                }]
            }
        }
    }

    pub(crate) fn load_value_instrs(
        &self,
        value_name: &'a EmissionValueName,
        load_as: LoadAs,
    ) -> Vec<curios_wasm::Instr> {
        let mut output = Vec::new();

        if let Some(field_data) = self.find_field(value_name) {
            output.extend([
                curios_wasm::Instr::LocalGet {
                    local_name: self.table().special_local(),
                },
                curios_wasm::Instr::RefCast {
                    ref_type: curios_wasm::RefType {
                        is_nullable: false,
                        heap_type: curios_wasm::HeapType::Concrete(field_data.type_name()),
                    },
                },
                curios_wasm::Instr::StructGet {
                    type_name: field_data.type_name(),
                    field_name: field_data.field_name(),
                },
            ]);

            output.extend(self.load_as_instrs(load_as, true));
        } else if let Some(local_data) = self.find_local(value_name) {
            output.push(curios_wasm::Instr::LocalGet {
                local_name: local_data.local_name,
            });

            output.extend(self.load_as_instrs(load_as, local_data.is_nullable));
        } else {
            output.push(curios_wasm::Instr::GlobalGet {
                global_name: self.table().find_const(value_name),
            });

            output.extend(self.load_as_instrs(load_as, false));
        }

        output
    }

    pub(crate) fn jump_instrs(&self, target: &'a EmissionJumpTarget) -> Vec<curios_wasm::Instr> {
        let mut output = Vec::new();

        for value_name in &target.params {
            output.extend(self.load_value_instrs(value_name, LoadAs::NonNull));
        }

        if self.is_resume(&target.target) {
            if target.params.len() != 1 {
                panic!(
                    "resume block `{}` expects 1 param, got {}",
                    target.target,
                    target.params.len(),
                );
            }

            output.push(curios_wasm::Instr::Return);
        } else {
            let block_data = self.find_block(&target.target);
            output.extend(block_data.enter(target.params.len()));
        }

        output
    }

    pub(crate) fn match_instrs(&self, target: &'a EmissionMatchTarget) -> Vec<curios_wasm::Instr> {
        if target.cases.is_empty() && target.default.is_none() {
            return vec![curios_wasm::Instr::Unreachable];
        }

        let default_instructions = match &target.default {
            Some(target) => self.jump_instrs(target),
            None => vec![curios_wasm::Instr::Unreachable],
        };

        let sorted: Vec<(u32, &EmissionJumpTarget)> =
            target.cases.iter().map(|(&k, v)| (k, v)).collect();

        if is_sequential_from_zero(&target.cases) {
            if let [(_, jump_target)] = sorted.as_slice() {
                self.load_value_instrs(&target.operand, LoadAs::Nat)
                    .into_iter()
                    .chain([
                        curios_wasm::Instr::I32Eqz,
                        curios_wasm::Instr::If {
                            label_name: self.table().special_label(),
                            block_type: curios_wasm::BlockType::Empty,
                            then_instructions: self.jump_instrs(jump_target),
                            else_instructions: default_instructions,
                        },
                    ])
                    .collect()
            } else {
                let label_names = sorted
                    .iter()
                    .enumerate()
                    .map(|(index, (_, jump_target))| {
                        (
                            curios_wasm::LabelName::from(format!("case${index}")),
                            self.jump_instrs(jump_target),
                        )
                    })
                    .collect::<Vec<_>>();

                let label_name = curios_wasm::LabelName::from("tail");

                let instructions = self
                    .load_value_instrs(&target.operand, LoadAs::Nat)
                    .into_iter()
                    .chain([curios_wasm::Instr::BrTable {
                        label_names: label_names
                            .iter()
                            .map(|(label_name, _)| label_name.clone())
                            .collect(),
                        label_name: label_name.clone(),
                    }])
                    .collect();

                label_names
                    .into_iter()
                    .chain([(label_name, default_instructions)])
                    .rev()
                    .fold(instructions, |instructions, (block_label, block_body)| {
                        iter::once(curios_wasm::Instr::Block {
                            label_name: block_label,
                            block_type: curios_wasm::BlockType::Empty,
                            instructions,
                        })
                        .chain(block_body)
                        .collect()
                    })
            }
        } else {
            self.binary_search_instrs(&target.operand, &sorted, default_instructions)
        }
    }

    fn binary_search_instrs(
        &self,
        operand: &'a EmissionValueName,
        cases: &[(u32, &'a EmissionJumpTarget)],
        default_instructions: Vec<curios_wasm::Instr>,
    ) -> Vec<curios_wasm::Instr> {
        match cases {
            [] => default_instructions,
            [(value, jump_target)] => self
                .load_value_instrs(operand, LoadAs::Nat)
                .into_iter()
                .chain([
                    curios_wasm::Instr::I32Const {
                        value: *value as i32,
                    },
                    curios_wasm::Instr::I32Eq,
                    curios_wasm::Instr::If {
                        label_name: curios_wasm::LabelName::from("eq"),
                        block_type: curios_wasm::BlockType::Empty,
                        then_instructions: self.jump_instrs(jump_target),
                        else_instructions: default_instructions,
                    },
                ])
                .collect(),
            _ => {
                let mid = cases.len() / 2;
                let (pivot, _) = cases[mid];
                let left =
                    self.binary_search_instrs(operand, &cases[..mid], default_instructions.clone());
                let right = self.binary_search_instrs(operand, &cases[mid..], default_instructions);
                self.load_value_instrs(operand, LoadAs::Nat)
                    .into_iter()
                    .chain([
                        curios_wasm::Instr::I32Const {
                            value: pivot as i32,
                        },
                        curios_wasm::Instr::I32LtU,
                        curios_wasm::Instr::If {
                            label_name: curios_wasm::LabelName::from("lt"),
                            block_type: curios_wasm::BlockType::Empty,
                            then_instructions: left,
                            else_instructions: right,
                        },
                    ])
                    .collect()
            }
        }
    }

    pub(crate) fn call_direct_instrs(
        &self,
        target: &'a EmissionFunctionName,
        params: &'a [EmissionValueName],
        resume: &'a EmissionBlockName,
    ) -> Vec<curios_wasm::Instr> {
        let mut output = Vec::new();

        if params.len() != self.table().find_func(target).arity() {
            panic!(
                "call to `{}` expects {} params, got {}",
                target,
                self.table().find_func(target).arity(),
                params.len(),
            );
        }

        for value_name in params {
            output.extend(self.load_value_instrs(value_name, LoadAs::NonNull));
        }

        if self.is_resume(resume) {
            output.push(curios_wasm::Instr::ReturnCall {
                func_name: self.table().find_func(target).func_name(),
            });
        } else {
            output.push(curios_wasm::Instr::Call {
                func_name: self.table().find_func(target).func_name(),
            });

            output.extend(self.find_block(resume).enter(1));
        }

        output
    }

    pub(crate) fn call_indirect_instrs(
        &self,
        target: &'a EmissionValueName,
        params: &'a [EmissionValueName],
        resume: &'a EmissionBlockName,
    ) -> Vec<curios_wasm::Instr> {
        let mut output = Vec::new();
        let arity = params.len();
        let envr_type = self.table().find_envr_type(arity);
        let clsr_type = self.table().find_clsr_type(arity);

        output.extend(self.load_value_instrs(target, LoadAs::NonNull));

        for value_name in params {
            output.extend(self.load_value_instrs(value_name, LoadAs::NonNull));
        }

        output.extend(self.load_value_instrs(target, LoadAs::Concrete(envr_type.clone())));

        output.push(curios_wasm::Instr::StructGet {
            type_name: envr_type,
            field_name: self.table().special_field(),
        });

        output.push(curios_wasm::Instr::RefAsNonNull);

        if self.is_resume(resume) {
            output.push(curios_wasm::Instr::ReturnCallRef {
                type_name: clsr_type,
            });
        } else {
            output.push(curios_wasm::Instr::CallRef {
                type_name: clsr_type,
            });

            let block_data = self.find_block(resume);
            output.extend(block_data.enter(1));
        }

        output
    }

    pub(crate) fn tail_instrs(&self, tail: &'a EmissionTail) -> Vec<curios_wasm::Instr> {
        match tail {
            EmissionTail::Jump(target) => self.jump_instrs(target),
            EmissionTail::Match(target) => self.match_instrs(target),
            EmissionTail::Call(EmissionCallTarget::Direct {
                target,
                params,
                resume,
            }) => self.call_direct_instrs(target, params, resume),
            EmissionTail::Call(EmissionCallTarget::Indirect {
                target,
                params,
                resume,
            }) => self.call_indirect_instrs(target, params, resume),
            EmissionTail::Host(host) => self.host_instrs(host),
            EmissionTail::Cell(cell) => self.cell_instrs(cell),
            EmissionTail::Unreachable => vec![curios_wasm::Instr::Unreachable],
        }
    }

    /// Resume after a host op that returns `results` stack values into a
    /// record-defining block. Such a resume always defines a value, so it can
    /// never be the bare-forwarder sentinel.
    fn host_multi_resume(
        &self,
        output: &mut Vec<curios_wasm::Instr>,
        resume: &EmissionBlockName,
        results: usize,
    ) {
        assert!(
            !self.is_resume(resume),
            "multi-result host resume cannot be the sentinel"
        );
        output.extend(self.find_block(resume).enter(results));
    }

    /// Resume after a host op whose single result already matches the
    /// function's anyref return shape: return it directly on the sentinel, else
    /// enter the resume block with one value.
    fn host_single_resume(&self, output: &mut Vec<curios_wasm::Instr>, resume: &EmissionBlockName) {
        if self.is_resume(resume) {
            output.push(curios_wasm::Instr::Return);
        } else {
            output.extend(self.find_block(resume).enter(1));
        }
    }

    /// Resume after a host op with no payload: materialise a unit for the
    /// single-value return sentinel, else enter the resume block with no
    /// values.
    fn host_unit_resume(&self, output: &mut Vec<curios_wasm::Instr>, resume: &EmissionBlockName) {
        if self.is_resume(resume) {
            output.push(curios_wasm::Instr::StructNew {
                type_name: self.table().find_tpl_type(0),
            });
            output.push(curios_wasm::Instr::Return);
        } else {
            output.extend(self.find_block(resume).enter(0));
        }
    }

    /// The rope→wire step for one host argument: a reference param crosses as
    /// its flat payload, so the loaded rope is forced first — deeply for
    /// `Lst(Bin)`/`Lst(Io)`, whose *elements* the host lifts as raw `$bytes`.
    fn wire_force_instrs(&self, wire_type: &WireType) -> Vec<curios_wasm::Instr> {
        let force = match wire_type {
            WireType::Nat | WireType::Bln | WireType::Int => return vec![],
            WireType::Bin | WireType::Io => self.table().bytes_force_func(),
            WireType::Lst(inner) => match **inner {
                WireType::Bin | WireType::Io => self.table().lst_bin_force_func(),
                _ => self.table().lst_force_func(),
            },
        };

        vec![curios_wasm::Instr::Call { func_name: force }]
    }

    /// The wire→rope step for one host result: a reference re-enters as a
    /// host-built flat payload and is embedded into a fresh leaf — deeply for
    /// `Lst(Bin)`, whose elements the host lowered as raw `$bytes`.
    fn wire_embed_instrs(&self, wire_type: &WireType) -> Vec<curios_wasm::Instr> {
        let embed = match wire_type {
            WireType::Nat | WireType::Bln | WireType::Int => return vec![],
            WireType::Bin | WireType::Io => self.table().bytes_embed_func(),
            WireType::Lst(inner) => match **inner {
                WireType::Bin | WireType::Io => self.table().lst_bin_embed_func(),
                _ => self.table().lst_embed_func(),
            },
        };

        vec![curios_wasm::Instr::Call { func_name: embed }]
    }

    /// Emit a host primitive call in tail position, then branch to its resume.
    /// Models `call_direct_instrs`: load operands, call the host import, then
    /// either fall through to the function's return (when the resume happens
    /// to be the sentinel) or set up the dispatcher and branch into the resume
    /// block.
    pub(crate) fn host_instrs(&self, host: &'a EmissionHostTarget) -> Vec<curios_wasm::Instr> {
        let mut output = Vec::new();

        match host {
            EmissionHostTarget::Foreign {
                function,
                operands,
                resume,
            } => {
                let signature = &function.signature;

                debug_assert_eq!(
                    operands.len(),
                    signature.params.len(),
                    "{} operand count does not match its signature",
                    function.name
                );

                for (operand, (_, wire_type)) in operands.iter().zip(&signature.params) {
                    output.extend(self.load_value_instrs(operand, wire_type.into()));
                    output.extend(self.wire_force_instrs(wire_type));
                }

                output.push(curios_wasm::Instr::Call {
                    func_name: self.table().host_func(function),
                });

                // Embed a reference result back into a rope. Only the *final*
                // result may be a reference: an earlier one would sit under
                // later stack values, and embedding it would need juggling
                // through locals. Every host signature keeps references last.
                for (_, wire_type) in signature.results.iter().rev().skip(1) {
                    debug_assert!(
                        matches!(wire_type, WireType::Nat | WireType::Bln | WireType::Int),
                        "{} carries a reference result before its last",
                        function.name
                    );
                }

                if let Some((_, wire_type)) = signature.results.last() {
                    output.extend(self.wire_embed_instrs(wire_type));
                }

                match signature.results.len() {
                    0 => self.host_unit_resume(&mut output, resume),
                    1 => self.host_single_resume(&mut output, resume),
                    results => self.host_multi_resume(&mut output, resume, results),
                }
            }
            EmissionHostTarget::IoExit { code } => {
                output.extend(self.load_value_instrs(code, LoadAs::Nat));
                output.push(curios_wasm::Instr::Call {
                    func_name: self.table().io_exit_func().clone(),
                });

                output.push(curios_wasm::Instr::Unreachable);
            }
        }

        output
    }

    pub(crate) fn cell_instrs(&self, cell: &'a EmissionCellTarget) -> Vec<curios_wasm::Instr> {
        let mut output = Vec::new();

        match cell {
            EmissionCellTarget::New { init, resume } => {
                output.extend(self.load_value_instrs(init, LoadAs::NonNull));
                output.push(curios_wasm::Instr::StructNew {
                    type_name: self.table().cell_type(),
                });
                self.host_single_resume(&mut output, resume);
            }
            EmissionCellTarget::Set {
                cell,
                value,
                resume,
            } => {
                output.extend(
                    self.load_value_instrs(cell, LoadAs::Concrete(self.table().cell_type())),
                );
                output.extend(self.load_value_instrs(value, LoadAs::NonNull));
                output.push(curios_wasm::Instr::StructSet {
                    type_name: self.table().cell_type(),
                    field_name: self.table().special_field(),
                });
                self.host_unit_resume(&mut output, resume);
            }
            EmissionCellTarget::Get { cell, resume } => {
                output.extend(
                    self.load_value_instrs(cell, LoadAs::Concrete(self.table().cell_type())),
                );
                output.push(curios_wasm::Instr::StructGet {
                    type_name: self.table().cell_type(),
                    field_name: self.table().special_field(),
                });
                self.host_single_resume(&mut output, resume);
            }
        }

        output
    }
}

#[derive(Debug, Clone)]
pub(crate) enum LoadAs {
    Null,
    NonNull,
    Concrete(curios_wasm::TypeName),
    Nat,
    Int,
    Flt,
    Bin,
    Lst,
}

/// How a host-import operand of the given wire type is loaded at the call
/// site: `Nat`/`Bln` unbox their i31 carrier unsigned to a raw i32, `Int`
/// unboxes signed (the `poll(2)` timeout convention), and the reference
/// shapes cast to their rope base type (a handle is its `Bin` token) — the
/// force step to the flat wire payload follows in `wire_force_instrs`.
impl From<&WireType> for LoadAs {
    fn from(wire_type: &WireType) -> LoadAs {
        match wire_type {
            WireType::Nat | WireType::Bln => LoadAs::Nat,
            WireType::Int => LoadAs::Int,
            WireType::Bin | WireType::Io => LoadAs::Bin,
            WireType::Lst(_) => LoadAs::Lst,
        }
    }
}
