use {
    super::{BlockData, ClsrData, FieldData, Frame, FuncData, LocalData, Table},
    curios_abi::WireType,
    curios_base::Entropy,
    curios_wasm::{BlockType, HeapType, Instr, LabelName, LocalName, RefType, TypeName, ValType},
    std::{
        collections::{BTreeMap, HashMap},
        iter,
    },
};

fn is_sequential_from_zero(cases: &BTreeMap<u32, crate::JumpTarget>) -> bool {
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
        locals: &'b mut Vec<(LocalName, ValType)>,
        frames: Vec<Frame<'a>>,
    },
    Func {
        table: &'a Table<'a>,
        data: &'a FuncData<'a>,
        entropy: Entropy,
        locals: &'b mut Vec<(LocalName, ValType)>,
        frames: Vec<Frame<'a>>,
    },
}

#[derive(Debug, Clone)]
pub enum LoadAs {
    Null,
    NonNull,
    Concrete(TypeName),
    Nat,
    Int,
    Flt,
    Bin,
    Arr,
}

/// How a host-import operand of the given wire type is loaded at the call
/// site: `Nat`/`Bln` unbox their i31 carrier unsigned to a raw i32, `Int`
/// unboxes signed (the `poll(2)` timeout convention), and the reference
/// shapes cast to their rope base type (a handle is its `Bin` token) — the
/// force step to the flat wire payload follows in `wire_force_instrs`.
fn wire_load_as(wire_type: &WireType) -> LoadAs {
    match wire_type {
        WireType::Nat | WireType::Bln => LoadAs::Nat,
        WireType::Int => LoadAs::Int,
        WireType::Bin | WireType::Io => LoadAs::Bin,
        WireType::Arr(_) => LoadAs::Arr,
    }
}

impl<'a, 'b> Context<'a, 'b> {
    pub fn new_const(table: &'a Table<'a>) -> Self {
        Self::Const { table }
    }

    pub fn new_clsr(
        table: &'a Table<'a>,
        data: &'a ClsrData<'a>,
        locals: &'b mut Vec<(LocalName, ValType)>,
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
        locals: &'b mut Vec<(LocalName, ValType)>,
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

    pub fn find_field(&self, value_name: &crate::ValueName) -> Option<FieldData> {
        match self {
            Self::Const { .. } | Self::Func { .. } => None,
            Self::Clsr { data, .. } => data.find_field(value_name),
        }
    }

    pub fn params(&self) -> HashMap<&'a crate::ValueName, LocalData> {
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

    pub fn is_resume(&self, block_name: &crate::BlockName) -> bool {
        match self {
            Self::Const { .. } => false,
            Self::Clsr { data, .. } => data.is_resume(block_name),
            Self::Func { data, .. } => data.is_resume(block_name),
        }
    }

    pub fn push_local(&mut self, string: &str, val_type: ValType) -> LocalName {
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
                    LocalName::from(format!("{entropy}"))
                } else {
                    LocalName::from(format!("{entropy}${string}"))
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

    pub fn leave_frame(&mut self) -> Vec<Instr> {
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

    pub fn find_local(&self, value_name: &crate::ValueName) -> Option<LocalData> {
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

    pub fn is_prealloc(&self, value_name: &crate::ValueName) -> bool {
        match self {
            Self::Const { .. } => false,
            Self::Clsr { frames, .. } | Self::Func { frames, .. } => frames
                .iter()
                .any(|frame| frame.preallocs.contains(value_name)),
        }
    }

    pub fn find_block(&self, block_name: &crate::BlockName) -> &BlockData<'a> {
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

    fn load_as_instrs(&self, load_as: LoadAs, is_nullable: bool) -> Vec<Instr> {
        match load_as {
            LoadAs::Null => vec![],
            LoadAs::NonNull => match is_nullable {
                true => vec![Instr::RefAsNonNull],
                false => vec![],
            },
            LoadAs::Concrete(type_name) => {
                vec![Instr::RefCast {
                    ref_type: RefType {
                        is_nullable: false,
                        heap_type: HeapType::Concrete(type_name),
                    },
                }]
            }
            LoadAs::Nat => {
                vec![
                    Instr::RefCast {
                        ref_type: self.table().int_type(false),
                    },
                    Instr::I31GetU,
                ]
            }
            LoadAs::Int => {
                vec![
                    Instr::RefCast {
                        ref_type: self.table().int_type(false),
                    },
                    Instr::I31GetS,
                ]
            }
            LoadAs::Flt => {
                vec![
                    Instr::RefCast {
                        ref_type: RefType {
                            is_nullable: false,
                            heap_type: HeapType::Concrete(self.table().flt_type()),
                        },
                    },
                    Instr::StructGet {
                        type_name: self.table().flt_type(),
                        field_name: self.table().special_field(),
                    },
                ]
            }
            LoadAs::Bin => {
                vec![Instr::RefCast {
                    ref_type: RefType {
                        is_nullable: false,
                        heap_type: HeapType::Concrete(self.table().bin_type()),
                    },
                }]
            }
            LoadAs::Arr => {
                vec![Instr::RefCast {
                    ref_type: RefType {
                        is_nullable: false,
                        heap_type: HeapType::Concrete(self.table().arr_type()),
                    },
                }]
            }
        }
    }

    pub fn load_value_instrs(
        &self,
        value_name: &'a crate::ValueName,
        load_as: LoadAs,
    ) -> Vec<Instr> {
        let mut output = Vec::new();

        if let Some(field_data) = self.find_field(value_name) {
            output.extend([
                Instr::LocalGet {
                    local_name: self.table().special_local(),
                },
                Instr::RefCast {
                    ref_type: RefType {
                        is_nullable: false,
                        heap_type: HeapType::Concrete(field_data.type_name()),
                    },
                },
                Instr::StructGet {
                    type_name: field_data.type_name(),
                    field_name: field_data.field_name(),
                },
            ]);

            output.extend(self.load_as_instrs(load_as, true));
        } else if let Some(local_data) = self.find_local(value_name) {
            output.push(Instr::LocalGet {
                local_name: local_data.local_name,
            });

            output.extend(self.load_as_instrs(load_as, local_data.is_nullable));
        } else {
            output.push(Instr::GlobalGet {
                global_name: self.table().find_const(value_name),
            });

            output.extend(self.load_as_instrs(load_as, false));
        }

        output
    }

    pub fn jump_instrs(&self, target: &'a crate::JumpTarget) -> Vec<Instr> {
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

            output.push(Instr::Return);
        } else {
            let block_data = self.find_block(&target.target);
            output.extend(block_data.enter(target.params.len()));
        }

        output
    }

    pub fn match_instrs(&self, target: &'a crate::MatchTarget) -> Vec<Instr> {
        if target.cases.is_empty() && target.default.is_none() {
            return vec![Instr::Unreachable];
        }

        let default_instructions = match &target.default {
            Some(target) => self.jump_instrs(target),
            None => vec![Instr::Unreachable],
        };

        let sorted: Vec<(u32, &crate::JumpTarget)> =
            target.cases.iter().map(|(&k, v)| (k, v)).collect();

        if is_sequential_from_zero(&target.cases) {
            if let [(_, jump_target)] = sorted.as_slice() {
                self.load_value_instrs(&target.operand, LoadAs::Nat)
                    .into_iter()
                    .chain([
                        Instr::I32Eqz,
                        Instr::If {
                            label_name: self.table().special_label(),
                            block_type: BlockType::Empty,
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
                            LabelName::from(format!("case${index}")),
                            self.jump_instrs(jump_target),
                        )
                    })
                    .collect::<Vec<_>>();

                let label_name = LabelName::from("tail");

                let instructions = self
                    .load_value_instrs(&target.operand, LoadAs::Nat)
                    .into_iter()
                    .chain([Instr::BrTable {
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
                        iter::once(Instr::Block {
                            label_name: block_label,
                            block_type: BlockType::Empty,
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
        operand: &'a crate::ValueName,
        cases: &[(u32, &'a crate::JumpTarget)],
        default_instructions: Vec<Instr>,
    ) -> Vec<Instr> {
        match cases {
            [] => default_instructions,
            [(value, jump_target)] => self
                .load_value_instrs(operand, LoadAs::Nat)
                .into_iter()
                .chain([
                    Instr::I32Const {
                        value: *value as i32,
                    },
                    Instr::I32Eq,
                    Instr::If {
                        label_name: LabelName::from("eq"),
                        block_type: BlockType::Empty,
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
                        Instr::I32Const {
                            value: pivot as i32,
                        },
                        Instr::I32LtU,
                        Instr::If {
                            label_name: LabelName::from("lt"),
                            block_type: BlockType::Empty,
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
        target: &'a crate::FuncName,
        params: &'a [crate::ValueName],
        resume: &'a crate::BlockName,
    ) -> Vec<Instr> {
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
            output.push(Instr::ReturnCall {
                func_name: self.table().find_func(target).func_name(),
            });
        } else {
            output.push(Instr::Call {
                func_name: self.table().find_func(target).func_name(),
            });

            output.extend(self.find_block(resume).enter(1));
        }

        output
    }

    pub fn call_indirect_instrs(
        &self,
        target: &'a crate::ValueName,
        params: &'a [crate::ValueName],
        resume: &'a crate::BlockName,
    ) -> Vec<Instr> {
        let mut output = Vec::new();
        let arity = params.len();
        let envr_type = self.table().find_envr_type(arity);
        let clsr_type = self.table().find_clsr_type(arity);

        output.extend(self.load_value_instrs(target, LoadAs::NonNull));

        for value_name in params {
            output.extend(self.load_value_instrs(value_name, LoadAs::NonNull));
        }

        output.extend(self.load_value_instrs(target, LoadAs::Concrete(envr_type.clone())));

        output.push(Instr::StructGet {
            type_name: envr_type,
            field_name: self.table().special_field(),
        });

        output.push(Instr::RefAsNonNull);

        if self.is_resume(resume) {
            output.push(Instr::ReturnCallRef {
                type_name: clsr_type,
            });
        } else {
            output.push(Instr::CallRef {
                type_name: clsr_type,
            });

            let block_data = self.find_block(resume);
            output.extend(block_data.enter(1));
        }

        output
    }

    pub fn tail_instrs(&self, tail: &'a crate::Tail) -> Vec<Instr> {
        match tail {
            crate::Tail::Jump(target) => self.jump_instrs(target),
            crate::Tail::Match(target) => self.match_instrs(target),
            crate::Tail::Call(crate::CallTarget::Direct {
                target,
                params,
                resume,
            }) => self.call_direct_instrs(target, params, resume),
            crate::Tail::Call(crate::CallTarget::Indirect {
                target,
                params,
                resume,
            }) => self.call_indirect_instrs(target, params, resume),
            crate::Tail::Host(host) => self.host_instrs(host),
            crate::Tail::Cell(cell) => self.cell_instrs(cell),
            crate::Tail::Unreachable => vec![Instr::Unreachable],
        }
    }

    /// Resume after a host op that returns `results` stack values into a
    /// record-defining block. Such a resume always defines a value, so it can
    /// never be the bare-forwarder sentinel.
    fn host_multi_resume(
        &self,
        output: &mut Vec<Instr>,
        resume: &crate::BlockName,
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
    fn host_single_resume(&self, output: &mut Vec<Instr>, resume: &crate::BlockName) {
        if self.is_resume(resume) {
            output.push(Instr::Return);
        } else {
            output.extend(self.find_block(resume).enter(1));
        }
    }

    /// Resume after a host op with no payload: materialise a unit for the
    /// single-value return sentinel, else enter the resume block with no
    /// values.
    fn host_unit_resume(&self, output: &mut Vec<Instr>, resume: &crate::BlockName) {
        if self.is_resume(resume) {
            output.push(Instr::StructNew {
                type_name: self.table().find_tpl_type(0),
            });
            output.push(Instr::Return);
        } else {
            output.extend(self.find_block(resume).enter(0));
        }
    }

    /// The rope→wire step for one host argument: a reference param crosses as
    /// its flat payload, so the loaded rope is forced first — deeply for
    /// `Arr(Bin)`/`Arr(Io)`, whose *elements* the host lifts as raw `$bytes`.
    fn wire_force_instrs(&self, wire_type: &WireType) -> Vec<Instr> {
        let force = match wire_type {
            WireType::Nat | WireType::Bln | WireType::Int => return vec![],
            WireType::Bin | WireType::Io => self.table().bin_force_func(),
            WireType::Arr(inner) => match **inner {
                WireType::Bin | WireType::Io => self.table().arr_bin_force_func(),
                _ => self.table().arr_force_func(),
            },
        };

        vec![Instr::Call { func_name: force }]
    }

    /// The wire→rope step for one host result: a reference re-enters as a
    /// host-built flat payload and is wrapped into a fresh leaf — deeply for
    /// `Arr(Bin)`, whose elements the host lowered as raw `$bytes`.
    fn wire_wrap_instrs(&self, wire_type: &WireType) -> Vec<Instr> {
        let wrap = match wire_type {
            WireType::Nat | WireType::Bln | WireType::Int => return vec![],
            WireType::Bin | WireType::Io => self.table().bin_wrap_func(),
            WireType::Arr(inner) => match **inner {
                WireType::Bin | WireType::Io => self.table().arr_bin_wrap_func(),
                _ => self.table().arr_wrap_func(),
            },
        };

        vec![Instr::Call { func_name: wrap }]
    }

    /// Emit a host primitive call in tail position, then branch to its resume.
    /// Models `call_direct_instrs`: load operands, call the host import, then
    /// either fall through to the function's return (when the resume happens
    /// to be the sentinel) or set up the dispatcher and branch into the resume
    /// block.
    pub fn host_instrs(&self, host: &'a crate::HostTarget) -> Vec<Instr> {
        let mut output = Vec::new();

        match host {
            crate::HostTarget::Foreign {
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
                    output.extend(self.load_value_instrs(operand, wire_load_as(wire_type)));
                    output.extend(self.wire_force_instrs(wire_type));
                }

                output.push(Instr::Call {
                    func_name: self.table().host_func(function),
                });

                // Wrap a reference result back into a rope. Only the *final*
                // result may be a reference: an earlier one would sit under
                // later stack values, and rewrapping it would need juggling
                // through locals. Every host signature keeps references last.
                for (_, wire_type) in signature.results.iter().rev().skip(1) {
                    debug_assert!(
                        matches!(wire_type, WireType::Nat | WireType::Bln | WireType::Int),
                        "{} carries a reference result before its last",
                        function.name
                    );
                }

                if let Some((_, wire_type)) = signature.results.last() {
                    output.extend(self.wire_wrap_instrs(wire_type));
                }

                match signature.results.len() {
                    0 => self.host_unit_resume(&mut output, resume),
                    1 => self.host_single_resume(&mut output, resume),
                    results => self.host_multi_resume(&mut output, resume, results),
                }
            }
            crate::HostTarget::IoExit { code, resume } => {
                output.extend(self.load_value_instrs(code, LoadAs::Nat));
                output.push(Instr::Call {
                    func_name: self.table().io_exit_func().clone(),
                });

                // The host traps, so control never returns; the resume path is
                // dead code but must stay well-typed, exactly like `IoClose`.
                self.host_unit_resume(&mut output, resume);
            }
        }

        output
    }

    pub fn cell_instrs(&self, cell: &'a crate::CellTarget) -> Vec<Instr> {
        let mut output = Vec::new();

        match cell {
            crate::CellTarget::New { init, resume } => {
                output.extend(self.load_value_instrs(init, LoadAs::NonNull));
                output.push(Instr::StructNew {
                    type_name: self.table().cell_type(),
                });
                self.host_single_resume(&mut output, resume);
            }
            crate::CellTarget::Set {
                cell,
                value,
                resume,
            } => {
                output.extend(
                    self.load_value_instrs(cell, LoadAs::Concrete(self.table().cell_type())),
                );
                output.extend(self.load_value_instrs(value, LoadAs::NonNull));
                output.push(Instr::StructSet {
                    type_name: self.table().cell_type(),
                    field_name: self.table().special_field(),
                });
                self.host_unit_resume(&mut output, resume);
            }
            crate::CellTarget::Get { cell, resume } => {
                output.extend(
                    self.load_value_instrs(cell, LoadAs::Concrete(self.table().cell_type())),
                );
                output.push(Instr::StructGet {
                    type_name: self.table().cell_type(),
                    field_name: self.table().special_field(),
                });
                self.host_single_resume(&mut output, resume);
            }
        }

        output
    }

    pub fn bloink_instrs(
        &self,
        bloink_local: LocalName,
        bloink_label: LabelName,
        regions: Vec<(LabelName, Vec<Instr>)>,
        tail: &'a crate::Tail,
    ) -> Vec<Instr> {
        let label_name = LabelName::from("tail");

        let instructions = vec![
            Instr::LocalGet {
                local_name: bloink_local.clone(),
            },
            Instr::BrTable {
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
                iter::once(Instr::Block {
                    label_name: block_label.clone(),
                    block_type: BlockType::Empty,
                    instructions,
                })
                .chain(block_body)
                .collect()
            });

        vec![
            Instr::I32Const { value: -1 },
            Instr::LocalSet {
                local_name: bloink_local,
            },
            Instr::Loop {
                label_name: bloink_label,
                block_type: BlockType::Empty,
                instructions,
            },
            Instr::Unreachable,
        ]
    }
}
