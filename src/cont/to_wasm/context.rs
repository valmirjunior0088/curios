use {
    super::{BlockData, ClsrData, FieldData, FuncData, ModuleData, Scope},
    crate::{cont, wasm},
    std::{collections::HashMap, iter, mem},
};

pub enum Context<'a, 'b> {
    Const {
        metadata: &'a ModuleData<'a>,
    },
    Clsr {
        metadata: &'a ModuleData<'a>,
        data: &'a ClsrData<'a>,
        entropy: usize,
        locals: &'b mut Vec<(wasm::LocalName, wasm::ValType)>,
        scopes: Vec<Scope<'a>>,
    },
    Func {
        metadata: &'a ModuleData<'a>,
        data: &'a FuncData<'a>,
        entropy: usize,
        locals: &'b mut Vec<(wasm::LocalName, wasm::ValType)>,
        scopes: Vec<Scope<'a>>,
    },
}

pub enum LoadAs {
    NonNull,
    Concrete(wasm::TypeName),
    Int,
    Flt,
}

impl<'a, 'b> Context<'a, 'b> {
    pub fn new_const(metadata: &'a ModuleData<'a>) -> Self {
        Self::Const { metadata }
    }

    pub fn new_clsr(
        metadata: &'a ModuleData<'a>,
        data: &'a ClsrData<'a>,
        locals: &'b mut Vec<(wasm::LocalName, wasm::ValType)>,
    ) -> Self {
        Self::Clsr {
            metadata,
            data,
            entropy: 0,
            locals,
            scopes: Vec::new(),
        }
    }

    pub fn new_func(
        metadata: &'a ModuleData<'a>,
        data: &'a FuncData<'a>,
        locals: &'b mut Vec<(wasm::LocalName, wasm::ValType)>,
    ) -> Self {
        Self::Func {
            metadata,
            data,
            entropy: 0,
            locals,
            scopes: Vec::new(),
        }
    }

    pub fn metadata(&self) -> &'a ModuleData<'a> {
        match self {
            Self::Const { metadata }
            | Self::Clsr { metadata, .. }
            | Self::Func { metadata, .. } => metadata,
        }
    }

    pub fn find_field(&self, value_name: &cont::ValueName) -> Option<FieldData> {
        match self {
            Self::Const { .. } | Self::Func { .. } => None,
            Self::Clsr { data, .. } => data.find_field(value_name),
        }
    }

    pub fn params(&self) -> HashMap<&'a cont::ValueName, (wasm::LocalName, bool)> {
        match self {
            Self::Const { .. } => panic!("`Context` lacks params"),
            Self::Clsr { data, .. } => data
                .params()
                .into_iter()
                .map(|(value_name, local_name)| (value_name, (local_name, false)))
                .collect(),
            Self::Func { data, .. } => data
                .params()
                .into_iter()
                .map(|(value_name, local_name)| (value_name, (local_name, false)))
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

    pub fn enter_scope(&mut self, scope: Scope<'a>) {
        match self {
            Self::Const { .. } => panic!("`Context` lacks scope stack"),
            Self::Clsr { scopes, .. } | Self::Func { scopes, .. } => scopes.push(scope),
        }
    }

    pub fn leave_scope(&mut self) -> Vec<wasm::Instr> {
        match self {
            Self::Const { .. } => panic!("`Context` lacks scope stack"),
            Self::Clsr { scopes, .. } | Self::Func { scopes, .. } => {
                scopes.pop().expect("`Context` lacks scope").instrs
            }
        }
    }

    pub fn this_scope(&mut self) -> Option<&mut Scope<'a>> {
        match self {
            Self::Const { .. } => None,
            Self::Clsr { scopes, .. } | Self::Func { scopes, .. } => scopes.last_mut(),
        }
    }

    pub fn find_local(&self, value_name: &cont::ValueName) -> Option<(wasm::LocalName, bool)> {
        match self {
            Self::Const { .. } => None,
            Self::Clsr { scopes, .. } | Self::Func { scopes, .. } => {
                scopes.iter().rev().find_map(|scope| {
                    scope
                        .values
                        .get(value_name)
                        .map(|local_name| (local_name.clone(), false))
                        .or_else(|| scope.params.get(value_name).cloned())
                })
            }
        }
    }

    pub fn find_block(&self, block_name: &cont::BlockName) -> &BlockData<'a> {
        match self {
            Self::Const { .. } => panic!("`Context` lacks scope stack"),
            Self::Clsr { scopes, .. } | Self::Func { scopes, .. } => scopes
                .iter()
                .rev()
                .find_map(|scope| {
                    scope
                        .blocks
                        .iter()
                        .find_map(|(scope_block_name, block_data)| {
                            (&block_name == scope_block_name).then_some(block_data)
                        })
                })
                .unwrap_or_else(|| panic!("`Context` lacks block `{}`", block_name.string)),
        }
    }

    fn load_as_instrs(&self, load_as: LoadAs, is_nullable: bool) -> Vec<wasm::Instr> {
        match load_as {
            LoadAs::NonNull if is_nullable => vec![wasm::Instr::RefAsNonNull],
            LoadAs::NonNull => vec![],
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
                        ref_type: self.metadata().int_ref_type(false),
                    },
                    wasm::Instr::I31GetS,
                ]
            }
            LoadAs::Flt => {
                vec![
                    wasm::Instr::RefCast {
                        ref_type: wasm::RefType {
                            is_nullable: false,
                            heap_type: wasm::HeapType::Concrete(self.metadata().flt_type()),
                        },
                    },
                    wasm::Instr::StructGet {
                        type_name: self.metadata().flt_type(),
                        field_name: self.metadata().special_field(),
                    },
                ]
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
                    local_name: self.metadata().special_local(),
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

            output.extend(self.load_as_instrs(load_as, false));
        } else if let Some((local_name, is_nullable)) = self.find_local(value_name) {
            output.push(wasm::Instr::LocalGet { local_name });

            output.extend(self.load_as_instrs(load_as, is_nullable));
        } else {
            output.push(wasm::Instr::GlobalGet {
                global_name: self.metadata().find_const(value_name),
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
                    target.target.string,
                    target.params.len(),
                );
            }

            output.push(wasm::Instr::Return);
        } else {
            let block_data = self.find_block(&target.target);

            if target.params.len() != block_data.params.len() {
                panic!(
                    "block `{}` expects {} params, got {}",
                    target.target.string,
                    block_data.params.len(),
                    target.params.len(),
                );
            }

            for (_, (local_name, _)) in block_data.params.iter().rev() {
                output.push(wasm::Instr::LocalSet {
                    local_name: local_name.clone(),
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

    pub fn case_instrs(&self, target: &'a cont::CaseTarget) -> Vec<wasm::Instr> {
        let default_instructions = match &target.default {
            Some(target) => self.jump_instrs(target),
            None => vec![wasm::Instr::Unreachable],
        };

        if let [jump_target] = &target.targets[..] {
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
            let label_names = target
                .targets
                .iter()
                .enumerate()
                .map(|(index, jump_target)| {
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
    }

    pub fn call_direct_instrs(
        &self,
        target: &'a cont::FuncName,
        params: &'a [cont::ValueName],
        resume: &'a cont::BlockName,
    ) -> Vec<wasm::Instr> {
        let mut output = Vec::new();

        if params.len() != self.metadata().find_func(target).arity() {
            panic!(
                "call to `{}` expects {} params, got {}",
                target.string,
                self.metadata().find_func(target).arity(),
                params.len(),
            );
        }

        for value_name in params {
            output.extend(self.load_value_instrs(value_name, LoadAs::NonNull));
        }

        if self.is_resume(resume) {
            output.push(wasm::Instr::ReturnCall {
                func_name: self.metadata().find_func(target).func_name(),
            });
        } else {
            output.push(wasm::Instr::Call {
                func_name: self.metadata().find_func(target).func_name(),
            });

            let block_data = self.find_block(resume);

            if block_data.params.len() != 1 {
                panic!(
                    "block `{}` expects {} params, got {}",
                    resume.string,
                    block_data.params.len(),
                    1,
                );
            }

            for (_, (local_name, _)) in block_data.params.iter().rev() {
                output.push(wasm::Instr::LocalSet {
                    local_name: local_name.clone(),
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
        let envr_type = self.metadata().find_envr_type(arity);
        let clsr_type = self.metadata().find_clsr_type(arity);

        output.extend(self.load_value_instrs(target, LoadAs::NonNull));

        for value_name in params {
            output.extend(self.load_value_instrs(value_name, LoadAs::NonNull));
        }

        output.extend(self.load_value_instrs(target, LoadAs::Concrete(envr_type.clone())));

        output.push(wasm::Instr::StructGet {
            type_name: envr_type,
            field_name: self.metadata().special_field(),
        });

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
                    resume.string,
                    block_data.params.len(),
                    1,
                );
            }

            for (_, (local_name, _)) in block_data.params.iter().rev() {
                output.push(wasm::Instr::LocalSet {
                    local_name: local_name.clone(),
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
            cont::Tail::Case(target) => self.case_instrs(target),
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
