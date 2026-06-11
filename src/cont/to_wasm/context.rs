use {
    super::{BlockData, ClsrData, FieldData, Frame, FuncData, LocalData, Table},
    crate::{Entropy, cont, wasm},
    std::{
        collections::{BTreeMap, HashMap},
        iter,
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
        entropy: Entropy,
        locals: &'b mut Vec<(wasm::LocalName, wasm::ValType)>,
        frames: Vec<Frame<'a>>,
    },
    Func {
        table: &'a Table<'a>,
        data: &'a FuncData<'a>,
        entropy: Entropy,
        locals: &'b mut Vec<(wasm::LocalName, wasm::ValType)>,
        frames: Vec<Frame<'a>>,
    },
}

#[derive(Debug, Clone)]
pub enum LoadAs {
    Null,
    NonNull,
    Concrete(wasm::TypeName),
    Nat,
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
            entropy: Entropy::<usize>::new(),
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
            entropy: Entropy::<usize>::new(),
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
                let entropy = entropy.fresh();
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

    pub fn is_prealloc(&self, value_name: &cont::ValueName) -> bool {
        match self {
            Self::Const { .. } => false,
            Self::Clsr { frames, .. } | Self::Func { frames, .. } => frames
                .iter()
                .any(|frame| frame.preallocs.contains(value_name)),
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
            LoadAs::Nat => {
                vec![
                    wasm::Instr::RefCast {
                        ref_type: self.table().int_type(false),
                    },
                    wasm::Instr::I31GetU,
                ]
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
            output.extend(block_data.enter(target.params.len()));
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
                self.load_value_instrs(&target.operand, LoadAs::Nat)
                    .into_iter()
                    .chain([
                        wasm::Instr::I32Eqz,
                        wasm::Instr::If {
                            label_name: self.table().special_label(),
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
                            wasm::LabelName::from(format!("case${index}")),
                            self.jump_instrs(jump_target),
                        )
                    })
                    .collect::<Vec<_>>();

                let label_name = wasm::LabelName::from("tail");

                let instructions = self
                    .load_value_instrs(&target.operand, LoadAs::Nat)
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
                .load_value_instrs(operand, LoadAs::Nat)
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
                self.load_value_instrs(operand, LoadAs::Nat)
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

            output.extend(self.find_block(resume).enter(1));
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
            output.extend(block_data.enter(1));
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
            cont::Tail::Host(host) => self.host_instrs(host),
            cont::Tail::Unreachable => vec![wasm::Instr::Unreachable],
        }
    }

    /// Emit a host primitive call in tail position, then branch to its resume.
    /// Models `call_direct_instrs`: load operands, call the host import, then
    /// either fall through to the function's return (when the resume happens
    /// to be the sentinel) or set up the dispatcher and branch into the resume
    /// block.
    pub fn host_instrs(&self, host: &'a cont::HostTarget) -> Vec<wasm::Instr> {
        let mut output = Vec::new();

        match host {
            cont::HostTarget::IoRead {
                handle,
                count,
                resume,
            } => {
                output.extend(self.load_value_instrs(handle, LoadAs::Nat));
                output.extend(self.load_value_instrs(count, LoadAs::Nat));
                output.push(wasm::Instr::Call {
                    func_name: self.table().io_read_func().clone(),
                });

                // A two-result host op's resume block defines a value (the
                // packed status record), so it is never a bare forwarder and
                // can never be threaded onto the single-value return sentinel.
                assert!(
                    !self.is_resume(resume),
                    "two-result host resume cannot be the sentinel"
                );
                let block_data = self.find_block(resume);
                output.extend(block_data.enter(2));
            }
            cont::HostTarget::IoWrite {
                handle,
                bytes,
                resume,
            } => {
                output.extend(self.load_value_instrs(handle, LoadAs::Nat));
                output.extend(self.load_value_instrs(bytes, LoadAs::Bin));
                output.push(wasm::Instr::Call {
                    func_name: self.table().io_write_func().clone(),
                });

                if self.is_resume(resume) {
                    // The import returns the status pre-boxed as an i31 ref
                    // (scalar results cross the boundary boxed so they can
                    // land in anyref block params), so it already matches the
                    // function's single-anyref return shape.
                    output.push(wasm::Instr::Return);
                } else {
                    let block_data = self.find_block(resume);
                    output.extend(block_data.enter(1));
                }
            }
            cont::HostTarget::IoOpen { path, mode, resume } => {
                output.extend(self.load_value_instrs(path, LoadAs::Bin));
                output.extend(self.load_value_instrs(mode, LoadAs::Nat));
                output.push(wasm::Instr::Call {
                    func_name: self.table().io_open_func().clone(),
                });

                // Same two-result invariant as `IoRead`.
                assert!(
                    !self.is_resume(resume),
                    "two-result host resume cannot be the sentinel"
                );
                let block_data = self.find_block(resume);
                output.extend(block_data.enter(2));
            }
            cont::HostTarget::IoClose { handle, resume } => {
                output.extend(self.load_value_instrs(handle, LoadAs::Nat));
                output.push(wasm::Instr::Call {
                    func_name: self.table().io_close_func().clone(),
                });

                if self.is_resume(resume) {
                    // No payload: materialise a unit for the single-value
                    // return sentinel.
                    output.push(wasm::Instr::StructNew {
                        type_name: self.table().find_tpl_type(0),
                    });
                    output.push(wasm::Instr::Return);
                } else {
                    let block_data = self.find_block(resume);
                    output.extend(block_data.enter(0));
                }
            }
        }

        output
    }

    pub fn bloink_instrs(
        &self,
        bloink_local: wasm::LocalName,
        bloink_label: wasm::LabelName,
        regions: Vec<(wasm::LabelName, Vec<wasm::Instr>)>,
        tail: &'a cont::Tail,
    ) -> Vec<wasm::Instr> {
        let label_name = wasm::LabelName::from("tail");

        let instructions = vec![
            wasm::Instr::LocalGet {
                local_name: bloink_local.clone(),
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
                local_name: bloink_local,
            },
            wasm::Instr::Loop {
                label_name: bloink_label,
                block_type: wasm::BlockType::Empty,
                instructions,
            },
            wasm::Instr::Unreachable,
        ]
    }
}
