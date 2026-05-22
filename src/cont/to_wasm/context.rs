use {
    super::{BlockData, ClsrData, FieldData, Frame, FuncData, LocalData, Table},
    crate::{cont, wasm},
    std::{
        collections::{BTreeMap, HashMap},
        iter, mem,
    },
};

fn is_sequential_from_zero(cases: &BTreeMap<u32, cont::JumpTarget>) -> bool {
    cases.keys().enumerate().all(|(i, &k)| k == i as u32)
}

#[derive(Debug)]
pub enum Context<'a, 'b> {
    Const {
        table: &'a Table<'a>,
    },
    Clsr {
        table: &'a Table<'a>,
        data: &'a ClsrData<'a>,
        entropy: usize,
        locals: &'b mut Vec<(wasm::LocalName, wasm::ValType)>,
        frames: Vec<Frame<'a>>,
    },
    Func {
        table: &'a Table<'a>,
        data: &'a FuncData<'a>,
        entropy: usize,
        locals: &'b mut Vec<(wasm::LocalName, wasm::ValType)>,
        frames: Vec<Frame<'a>>,
    },
}

#[derive(Debug)]
pub enum LoadAs {
    Null,
    NonNull,
    Concrete(wasm::TypeName),
    Int,
    Flt,
    Bin,
    Arr,
}

impl<'a, 'b> Context<'a, 'b> {
    pub fn new_const(table: &'a Table<'a>) -> Self {
        Self::Const { table }
    }

    pub fn new_clsr(
        table: &'a Table<'a>,
        data: &'a ClsrData<'a>,
        locals: &'b mut Vec<(wasm::LocalName, wasm::ValType)>,
    ) -> Self {
        Self::Clsr {
            table,
            data,
            entropy: 0,
            locals,
            frames: Vec::new(),
        }
    }

    pub fn new_func(
        table: &'a Table<'a>,
        data: &'a FuncData<'a>,
        locals: &'b mut Vec<(wasm::LocalName, wasm::ValType)>,
    ) -> Self {
        Self::Func {
            table,
            data,
            entropy: 0,
            locals,
            frames: Vec::new(),
        }
    }

    pub fn table(&self) -> &'a Table<'a> {
        match self {
            Self::Const { table } | Self::Clsr { table, .. } | Self::Func { table, .. } => table,
        }
    }

    pub fn find_field(&self, value_name: &cont::ValueName) -> Option<FieldData> {
        match self {
            Self::Const { .. } | Self::Func { .. } => None,
            Self::Clsr { data, .. } => data.find_field(value_name),
        }
    }

    pub fn params(&self) -> HashMap<&'a cont::ValueName, LocalData> {
        match self {
            Self::Const { .. } => panic!("`Context` lacks params"),
            Self::Clsr { data, .. } => data
                .params()
                .into_iter()
                .map(|(value_name, local_name)| (value_name, LocalData::new(local_name, false)))
                .collect(),
            Self::Func { data, .. } => data
                .params()
                .into_iter()
                .map(|(value_name, local_name)| (value_name, LocalData::new(local_name, false)))
                .collect(),
        }
    }

    pub fn is_resume(&self, block_name: &cont::BlockName) -> bool {
        match self {
            Self::Const { .. } => false,
            Self::Clsr { data, .. } => data.is_resume(block_name),
            Self::Func { data, .. } => data.is_resume(block_name),
        }
    }

    pub fn push_local(&mut self, string: &str, val_type: wasm::ValType) -> wasm::LocalName {
        match self {
            Self::Const { .. } => panic!("`Context` lacks locals"),
            Self::Clsr {
                entropy, locals, ..
            }
            | Self::Func {
                entropy, locals, ..
            } => {
                let entropy = mem::replace(entropy, *entropy + 1);
                let local_name = if string.is_empty() {
                    wasm::LocalName::from(format!("{entropy}"))
                } else {
                    wasm::LocalName::from(format!("{entropy}${string}"))
                };

                locals.push((local_name.clone(), val_type));

                local_name
            }
        }
    }

    pub fn enter_frame(&mut self, frame: Frame<'a>) {
        match self {
            Self::Const { .. } => panic!("`Context` lacks frame stack"),
            Self::Clsr { frames, .. } | Self::Func { frames, .. } => frames.push(frame),
        }
    }

    pub fn leave_frame(&mut self) -> Vec<wasm::Instr> {
        match self {
            Self::Const { .. } => panic!("`Context` lacks frame stack"),
            Self::Clsr { frames, .. } | Self::Func { frames, .. } => {
                frames.pop().expect("`Context` lacks frame").instrs
            }
        }
    }

    pub fn this_frame(&mut self) -> Option<&mut Frame<'a>> {
        match self {
            Self::Const { .. } => None,
            Self::Clsr { frames, .. } | Self::Func { frames, .. } => frames.last_mut(),
        }
    }

    pub fn find_local(&self, value_name: &cont::ValueName) -> Option<LocalData> {
        match self {
            Self::Const { .. } => None,
            Self::Clsr { frames, .. } | Self::Func { frames, .. } => {
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

    pub fn find_block(&self, block_name: &cont::BlockName) -> &BlockData<'a> {
        match self {
            Self::Const { .. } => panic!("`Context` lacks frame stack"),
            Self::Clsr { frames, .. } | Self::Func { frames, .. } => frames
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

    fn load_as_instrs(&self, load_as: LoadAs, is_nullable: bool) -> Vec<wasm::Instr> {
        match load_as {
            LoadAs::Null => vec![],
            LoadAs::NonNull => match is_nullable {
                true => vec![wasm::Instr::RefAsNonNull],
                false => vec![],
            },
            LoadAs::Concrete(type_name) => {
                vec![wasm::Instr::RefCast {
                    ref_type: wasm::RefType {
                        is_nullable: false,
                        heap_type: wasm::HeapType::Concrete(type_name),
                    },
                }]
            }
            LoadAs::Int => {
                vec![
                    wasm::Instr::RefCast {
                        ref_type: self.table().int_type(false),
                    },
                    wasm::Instr::I31GetS,
                ]
            }
            LoadAs::Flt => {
                vec![
                    wasm::Instr::RefCast {
                        ref_type: wasm::RefType {
                            is_nullable: false,
                            heap_type: wasm::HeapType::Concrete(self.table().flt_type()),
                        },
                    },
                    wasm::Instr::StructGet {
                        type_name: self.table().flt_type(),
                        field_name: self.table().special_field(),
                    },
                ]
            }
            LoadAs::Bin => {
                vec![wasm::Instr::RefCast {
                    ref_type: wasm::RefType {
                        is_nullable: false,
                        heap_type: wasm::HeapType::Concrete(self.table().bin_type()),
                    },
                }]
            }
            LoadAs::Arr => {
                vec![wasm::Instr::RefCast {
                    ref_type: wasm::RefType {
                        is_nullable: false,
                        heap_type: wasm::HeapType::Concrete(self.table().arr_type()),
                    },
                }]
            }
        }
    }

    pub fn load_value_instrs(
        &self,
        value_name: &'a cont::ValueName,
        load_as: LoadAs,
    ) -> Vec<wasm::Instr> {
        let mut output = Vec::new();

        if let Some(field_data) = self.find_field(value_name) {
            output.extend([
                wasm::Instr::LocalGet {
                    local_name: self.table().special_local(),
                },
                wasm::Instr::RefCast {
                    ref_type: wasm::RefType {
                        is_nullable: false,
                        heap_type: wasm::HeapType::Concrete(field_data.type_name()),
                    },
                },
                wasm::Instr::StructGet {
                    type_name: field_data.type_name(),
                    field_name: field_data.field_name(),
                },
            ]);

            output.extend(self.load_as_instrs(load_as, true));
        } else if let Some(local_data) = self.find_local(value_name) {
            output.push(wasm::Instr::LocalGet {
                local_name: local_data.local_name,
            });

            output.extend(self.load_as_instrs(load_as, local_data.is_nullable));
        } else {
            output.push(wasm::Instr::GlobalGet {
                global_name: self.table().find_const(value_name),
            });

            output.extend(self.load_as_instrs(load_as, false));
        }

        output
    }

    pub fn jump_instrs(&self, target: &'a cont::JumpTarget) -> Vec<wasm::Instr> {
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

            output.push(wasm::Instr::Return);
        } else {
            let block_data = self.find_block(&target.target);

            if target.params.len() != block_data.params.len() {
                panic!(
                    "block `{}` expects {} params, got {}",
                    target.target,
                    block_data.params.len(),
                    target.params.len(),
                );
            }

            for (_, local_data) in block_data.params.iter().rev() {
                output.push(wasm::Instr::LocalSet {
                    local_name: local_data.local_name.clone(),
                });
            }

            output.push(wasm::Instr::I32Const {
                value: block_data.index as i32,
            });

            output.push(wasm::Instr::LocalSet {
                local_name: block_data.dispatcher_local.clone(),
            });

            output.push(wasm::Instr::Br {
                label_name: block_data.dispatcher_label.clone(),
            });
        }

        output
    }

    pub fn match_instrs(&self, target: &'a cont::MatchTarget) -> Vec<wasm::Instr> {
        if target.cases.is_empty() && target.default.is_none() {
            return vec![wasm::Instr::Unreachable];
        }

        let default_instructions = match &target.default {
            Some(target) => self.jump_instrs(target),
            None => vec![wasm::Instr::Unreachable],
        };

        let sorted: Vec<(u32, &cont::JumpTarget)> =
            target.cases.iter().map(|(&k, v)| (k, v)).collect();

        if is_sequential_from_zero(&target.cases) {
            if let [(_, jump_target)] = sorted.as_slice() {
                self.load_value_instrs(&target.operand, LoadAs::Int)
                    .into_iter()
                    .chain([
                        wasm::Instr::I32Eqz,
                        wasm::Instr::If {
                            label_name: wasm::LabelName::from("0"),
                            block_type: wasm::BlockType::Empty,
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
                            wasm::LabelName::from(format!("{index}")),
                            self.jump_instrs(jump_target),
                        )
                    })
                    .collect::<Vec<_>>();

                let label_name = wasm::LabelName::from("tail");

                let instructions = self
                    .load_value_instrs(&target.operand, LoadAs::Int)
                    .into_iter()
                    .chain([wasm::Instr::BrTable {
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
                        iter::once(wasm::Instr::Block {
                            label_name: block_label,
                            block_type: wasm::BlockType::Empty,
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
        operand: &'a cont::ValueName,
        cases: &[(u32, &'a cont::JumpTarget)],
        default_instructions: Vec<wasm::Instr>,
    ) -> Vec<wasm::Instr> {
        match cases {
            [] => default_instructions,
            [(value, jump_target)] => self
                .load_value_instrs(operand, LoadAs::Int)
                .into_iter()
                .chain([
                    wasm::Instr::I32Const {
                        value: *value as i32,
                    },
                    wasm::Instr::I32Eq,
                    wasm::Instr::If {
                        label_name: wasm::LabelName::from("eq"),
                        block_type: wasm::BlockType::Empty,
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
                self.load_value_instrs(operand, LoadAs::Int)
                    .into_iter()
                    .chain([
                        wasm::Instr::I32Const {
                            value: pivot as i32,
                        },
                        wasm::Instr::I32LtU,
                        wasm::Instr::If {
                            label_name: wasm::LabelName::from("lt"),
                            block_type: wasm::BlockType::Empty,
                            then_instructions: left,
                            else_instructions: right,
                        },
                    ])
                    .collect()
            }
        }
    }

    pub fn call_direct_instrs(
        &self,
        target: &'a cont::FuncName,
        params: &'a [cont::ValueName],
        resume: &'a cont::BlockName,
    ) -> Vec<wasm::Instr> {
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
            output.push(wasm::Instr::ReturnCall {
                func_name: self.table().find_func(target).func_name(),
            });
        } else {
            output.push(wasm::Instr::Call {
                func_name: self.table().find_func(target).func_name(),
            });

            let block_data = self.find_block(resume);

            if block_data.params.len() != 1 {
                panic!(
                    "block `{}` expects {} params, got {}",
                    resume,
                    block_data.params.len(),
                    1,
                );
            }

            for (_, local_data) in block_data.params.iter().rev() {
                output.push(wasm::Instr::LocalSet {
                    local_name: local_data.local_name.clone(),
                });
            }

            output.push(wasm::Instr::I32Const {
                value: block_data.index as i32,
            });

            output.push(wasm::Instr::LocalSet {
                local_name: block_data.dispatcher_local.clone(),
            });

            output.push(wasm::Instr::Br {
                label_name: block_data.dispatcher_label.clone(),
            });
        }

        output
    }

    pub fn call_indirect_instrs(
        &self,
        target: &'a cont::ValueName,
        params: &'a [cont::ValueName],
        resume: &'a cont::BlockName,
    ) -> Vec<wasm::Instr> {
        let mut output = Vec::new();
        let arity = params.len();
        let envr_type = self.table().find_envr_type(arity);
        let clsr_type = self.table().find_clsr_type(arity);

        output.extend(self.load_value_instrs(target, LoadAs::NonNull));

        for value_name in params {
            output.extend(self.load_value_instrs(value_name, LoadAs::NonNull));
        }

        output.extend(self.load_value_instrs(target, LoadAs::Concrete(envr_type.clone())));

        output.push(wasm::Instr::StructGet {
            type_name: envr_type,
            field_name: self.table().special_field(),
        });

        output.push(wasm::Instr::RefAsNonNull);

        if self.is_resume(resume) {
            output.push(wasm::Instr::ReturnCallRef {
                type_name: clsr_type,
            });
        } else {
            output.push(wasm::Instr::CallRef {
                type_name: clsr_type,
            });

            let block_data = self.find_block(resume);

            if block_data.params.len() != 1 {
                panic!(
                    "block `{}` expects {} params, got {}",
                    resume,
                    block_data.params.len(),
                    1,
                );
            }

            for (_, local_data) in block_data.params.iter().rev() {
                output.push(wasm::Instr::LocalSet {
                    local_name: local_data.local_name.clone(),
                });
            }

            output.push(wasm::Instr::I32Const {
                value: block_data.index as i32,
            });

            output.push(wasm::Instr::LocalSet {
                local_name: block_data.dispatcher_local.clone(),
            });

            output.push(wasm::Instr::Br {
                label_name: block_data.dispatcher_label.clone(),
            });
        }

        output
    }

    pub fn tail_instrs(&self, tail: &'a cont::Tail) -> Vec<wasm::Instr> {
        match tail {
            cont::Tail::Jump(target) => self.jump_instrs(target),
            cont::Tail::Match(target) => self.match_instrs(target),
            cont::Tail::Call(cont::CallTarget::Direct {
                target,
                params,
                resume,
            }) => self.call_direct_instrs(target, params, resume),
            cont::Tail::Call(cont::CallTarget::Indirect {
                target,
                params,
                resume,
            }) => self.call_indirect_instrs(target, params, resume),
        }
    }

    pub fn flow_instrs(
        &self,
        dispatcher_local: wasm::LocalName,
        dispatcher_label: wasm::LabelName,
        regions: Vec<(wasm::LabelName, Vec<wasm::Instr>)>,
        tail: &'a cont::Tail,
    ) -> Vec<wasm::Instr> {
        let label_name = wasm::LabelName::from("tail");

        let instructions = vec![
            wasm::Instr::LocalGet {
                local_name: dispatcher_local.clone(),
            },
            wasm::Instr::BrTable {
                label_names: regions
                    .iter()
                    .map(|(block_label, _)| block_label.clone())
                    .collect(),
                label_name: label_name.clone(),
            },
        ];

        let instructions = regions
            .into_iter()
            .chain([(label_name, self.tail_instrs(tail))])
            .rev()
            .fold(instructions, |instructions, (block_label, block_body)| {
                iter::once(wasm::Instr::Block {
                    label_name: block_label.clone(),
                    block_type: wasm::BlockType::Empty,
                    instructions,
                })
                .chain(block_body)
                .collect()
            });

        vec![
            wasm::Instr::I32Const { value: -1 },
            wasm::Instr::LocalSet {
                local_name: dispatcher_local,
            },
            wasm::Instr::Loop {
                label_name: dispatcher_label,
                block_type: wasm::BlockType::Empty,
                instructions,
            },
            wasm::Instr::Unreachable,
        ]
    }
}
