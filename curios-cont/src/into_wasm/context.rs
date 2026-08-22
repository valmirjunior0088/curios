use {
    super::{
        BlockData, ClsrData, EmissionArg, EmissionBlockName, EmissionCallTarget,
        EmissionCellTarget, EmissionFunctionName, EmissionHostTarget, EmissionJumpTarget,
        EmissionMatchTarget, EmissionTail, EmissionValueName, FieldData, Frame, FuncData,
        LocalData, Table,
    },
    crate::{CpsSlot, Repr},
    curios_abi::{WireLeaf, WireType},
    curios_utilities::{Entropy, Grain},
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

    /// Whether the definition of `value_name` is a `Tuple` or `List` construction — bound in a region in scope, or hoisted to a module const.
    ///
    /// Both halves are needed: `hoist` lifts a closed aggregate out of its region into a global, so a check reading frames alone answers `false` for exactly the constant ones.
    fn is_aggregate(&self, value_name: &EmissionValueName) -> bool {
        if self.table().is_aggregate_const(value_name) {
            return true;
        }

        match self {
            Self::Const { .. } => false,
            Self::Closure { frames, .. } | Self::Function { frames, .. } => frames
                .iter()
                .any(|frame| frame.aggregates.contains(value_name)),
        }
    }

    /// Refuse to hand an aggregate construction to a register.
    ///
    /// A `Tuple` or `List` construction is a heap reference, so loading one at a scalar carrier emits `ref.cast (ref i31)` over a `struct.new`. That traps at run time, in a function far from whatever decided the carrier, which is exactly how it went unnoticed.
    ///
    /// **The representation analysis does not rule this out, which is why the check is here.** An aggregate's own definition offers `Offer::Never`, so no demand settles raw on *it* — but a continuation parameter it flows into is `Offer::Open` and settles raw from its own uses, whatever reaches it, and the edge then coerces every argument to that carrier before anything looks at what it is. So the invariant is the *door's*: `curios-ersd`'s lowering must never route an aggregate into a parameter something reads as a scalar. It broke once, when an immediate arm's binder was aliased to its scrutinee and the payload had no definition of its own to refuse the demand; the only symptom was a program that trapped for every input but zero. Both halves of the population are checked — a region binding and a `hoist`ed const — and both are pinned by `should_panic` fixtures in this module's tests.
    ///
    /// A panic rather than a diagnostic, per this workspace's rule for invariants: a program's fault is reported to its author, but this crate's own broken contract is not something to emit code for.
    fn refuse_raw_aggregate(&self, value_name: &EmissionValueName, load_as: &LoadAs) {
        let raw = matches!(load_as, LoadAs::Nat | LoadAs::Int | LoadAs::Flt);

        assert!(
            !(raw && self.is_aggregate(value_name)),
            "`{value_name}` is a `Tuple`/`List`/`Variant` construction loaded at the raw carrier {load_as:?}"
        );
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

    pub(crate) fn load_as_instrs(
        &self,
        load_as: LoadAs,
        is_nullable: bool,
    ) -> Vec<curios_wasm::Instr> {
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
            LoadAs::ConcreteOrNull(type_name) => {
                vec![curios_wasm::Instr::RefCast {
                    ref_type: curios_wasm::RefType {
                        is_nullable: true,
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
            // A packed value is small-canonical — an immediate inside its grain's envelope, a rope past it — so a position demanding the rope goes through its grain's box helper, which materialises an immediate and casts (trapping on null) exactly as the bare cast here used to. The grain rides the load because the two immediate layouts share no runtime discrimination.
            LoadAs::Bin(grain) => {
                let func_name = match grain {
                    Grain::X => self.table().bytes_box_func(),
                    Grain::B => self.table().bits_box_func(),
                };
                vec![curios_wasm::Instr::Call { func_name }]
            }
            LoadAs::List => {
                vec![curios_wasm::Instr::RefCast {
                    ref_type: curios_wasm::RefType {
                        is_nullable: false,
                        heap_type: curios_wasm::HeapType::Concrete(self.table().list_rope_type()),
                    },
                }]
            }
        }
    }

    /// Coerce a value already on the stack in the register carrier its local is declared at, to what the reading position demands.
    ///
    /// The positions the analysis decided the carrier *for* want exactly what the register holds, and cost nothing — that is the whole point of deciding it. Every other position boxes the value back and then reads it the ordinary way, which is the "coercion at the disagreeing use" the analysis is built around: one `ref.i31` or one `struct.new`, set against the `ref.cast` plus `i31.get_u` that holding it boxed would have cost at *every* arithmetic use.
    fn raw_as_instrs(&self, carrier: Repr, load_as: LoadAs) -> Vec<curios_wasm::Instr> {
        match (carrier, &load_as) {
            (Repr::Nat, LoadAs::Nat) | (Repr::Int, LoadAs::Int) | (Repr::Flt, LoadAs::Flt) => {
                vec![]
            }
            _ => box_instr(&carrier, self.table())
                .into_iter()
                .chain(self.load_as_instrs(load_as, false))
                .collect(),
        }
    }

    /// How a value must sit on the stack to be stored into `param`'s local: in its register carrier when that local is one, and as a reference otherwise.
    fn param_load(&self, param: &EmissionValueName) -> LoadAs {
        match self.table().raw_carrier(param) {
            Some(carrier) => LoadAs::of(&carrier),
            None => LoadAs::NonNull,
        }
    }

    /// Enter a resume block with `arity` values already on the stack as references.
    ///
    /// A resume block's parameters are held boxed by construction: the representation analysis withdraws the offer on every continuation a call, a host import, a cell operation or a call-shaped intrinsic returns to, precisely because the emitter hands those results over as references and has no cheaper store to make. The assert states that rather than trusting it — a violation is a broken analysis, and it would otherwise surface as a wasm validation failure with nothing pointing back here.
    /// How many results the call feeding this resume block hands over — the block's own parameter count, rather than a fixed one, so a callee returning a constructor as its fields needs no second source of truth.
    fn resume_arity(&self, resume: &EmissionBlockName) -> usize {
        self.find_block(resume).params().len()
    }

    fn resume_instrs(&self, resume: &EmissionBlockName, arity: usize) -> Vec<curios_wasm::Instr> {
        let block_data = self.find_block(resume);

        debug_assert!(
            block_data
                .params()
                .iter()
                .all(|(name, _)| self.table().raw_carrier(name).is_none()),
            "resume block `{resume}` holds a parameter in a register, where a result arrives as a reference",
        );

        block_data.enter(arity)
    }

    pub(crate) fn load_value_instrs(
        &self,
        value_name: &'a EmissionValueName,
        load_as: LoadAs,
    ) -> Vec<curios_wasm::Instr> {
        self.refuse_raw_aggregate(value_name, &load_as);

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

            // Only a local can be held in a register. A closure field, a module const and a function parameter each arrive through a position that is a reference by declaration, which is what the representation analysis withholds its offer on.
            match self.table().raw_carrier(value_name) {
                Some(carrier) => output.extend(self.raw_as_instrs(carrier, load_as)),
                None => output.extend(self.load_as_instrs(load_as, local_data.is_nullable)),
            }
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

        // The bare-forwarder sentinel is the function's own return, so whatever it carries leaves boxed however it was held. The count is the target's own, not a fixed one: a function returning a constructor as its fields returns as many values as the shape has, and the arity a jump carries is the arity the return delivers.
        if self.is_resume(&target.target) {
            for arg in &target.params {
                output.extend(match arg {
                    EmissionArg::Value(value_name) => {
                        self.load_value_instrs(value_name, LoadAs::NonNull)
                    }
                    // A resume block's parameters are held boxed by construction, so a return filler's carrier was never in question.
                    EmissionArg::Filler => zero_instrs(None),
                });
            }
            output.push(curios_wasm::Instr::Return);

            return output;
        }

        // Each argument is loaded the way the parameter it feeds is held, so an edge between two register-held parameters moves a register to a register. That is what carries a decision around a loop: without it the back edge would box on the way out and unbox on the way in, every iteration.
        let block_data = self.find_block(&target.target);
        for (index, arg) in target.params.iter().enumerate() {
            let param = block_data.params().get(index).map(|(param, _)| param);

            output.extend(match arg {
                EmissionArg::Value(value_name) => {
                    let load = match param {
                        Some(param) => self.param_load(param),
                        None => LoadAs::NonNull,
                    };
                    self.load_value_instrs(value_name, load)
                }
                // The one position whose carrier could not be known upstream. A filler inhabits the slot rather than merely going unread there, so it is built at the carrier the parameter's local is declared as — raw where the analysis raised it, boxed otherwise.
                EmissionArg::Filler => {
                    zero_instrs(param.and_then(|param| self.table().raw_carrier(param)))
                }
            });
        }

        output.extend(block_data.enter(target.params.len()));

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
            } else if let ([(_, zero_target), (_, one_target)], None) =
                (sorted.as_slice(), &target.default)
            {
                // Cases `{0, 1}` with nothing else reachable: the operand is 0 or 1 by construction, so a conditional branch decides it. This is every `Bool` match — `Rhs::SwitchBool` lowers as exactly these cases with no default — and every exhaustive two-constructor tag. A `br_table` here is a bounds check, a `csel`, a dependent load and an indirect branch where `if` is one compare, and nothing below the emitter narrows it: Cranelift's aarch64 rule builds a `JTSequence` whatever the table's size, and Binaryen leaves a two-entry table alone.
                //
                // Two cases *with* a default is three outcomes, not two, so it falls through to the compare chain below rather than here.
                self.load_value_instrs(&target.operand, LoadAs::Nat)
                    .into_iter()
                    .chain([curios_wasm::Instr::If {
                        label_name: self.table().special_label(),
                        block_type: curios_wasm::BlockType::Empty,
                        then_instructions: self.jump_instrs(one_target),
                        else_instructions: self.jump_instrs(zero_target),
                    }])
                    .collect()
            } else if sorted.len() == 2 {
                self.binary_search_instrs(&target.operand, &sorted, default_instructions)
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

            output.extend(self.resume_instrs(resume, self.resume_arity(resume)));
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

        // The special field is the body's `i32` table index; `call_indirect` reads the funcref out of the table itself, so nothing here materializes one. A shell's zeroed field selects the null slot and traps, exactly as the null funcref did.
        output.push(curios_wasm::Instr::StructGet {
            type_name: envr_type,
            field_name: self.table().special_field(),
        });

        if self.is_resume(resume) {
            output.push(curios_wasm::Instr::ReturnCallIndirect {
                table_name: self.table().clsr_table(arity),
                type_name: clsr_type,
            });
        } else {
            output.push(curios_wasm::Instr::CallIndirect {
                table_name: self.table().clsr_table(arity),
                type_name: clsr_type,
            });

            output.extend(self.resume_instrs(resume, self.resume_arity(resume)));
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

    /// Resume after a host op that returns `results` stack values into a record-defining block. Such a resume always defines a value, so it can never be the bare-forwarder sentinel.
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
        output.extend(self.resume_instrs(resume, results));
    }

    /// Resume after a host op whose single result already matches the function's anyref return shape: return it directly on the sentinel, else enter the resume block with one value.
    fn host_single_resume(&self, output: &mut Vec<curios_wasm::Instr>, resume: &EmissionBlockName) {
        if self.is_resume(resume) {
            output.push(curios_wasm::Instr::Return);
        } else {
            output.extend(self.resume_instrs(resume, self.resume_arity(resume)));
        }
    }

    /// Resume after a host op with no payload: materialise a unit for the single-value return sentinel, else enter the resume block with no values.
    fn host_unit_resume(&self, output: &mut Vec<curios_wasm::Instr>, resume: &EmissionBlockName) {
        if self.is_resume(resume) {
            output.push(curios_wasm::Instr::StructNew {
                type_name: self.table().find_tuple_type(0),
            });
            output.push(curios_wasm::Instr::Return);
        } else {
            output.extend(self.resume_instrs(resume, 0));
        }
    }

    /// The rope→wire step for one host argument: a reference param crosses as its flat payload, so the loaded rope is forced first — deeply for `List(Bytes)`/`List(Handle)`, whose *elements* the host lifts as raw `$bytes`.
    fn wire_force_instrs(&self, wire_type: &WireType) -> Vec<curios_wasm::Instr> {
        let force = match wire_type {
            WireType::Nat | WireType::Bool | WireType::Int => return vec![],
            WireType::Bytes | WireType::Handle => self.table().bytes_force_func(),
            WireType::List(inner) => match inner {
                WireLeaf::Bytes | WireLeaf::Handle => self.table().list_bytes_force_func(),
                WireLeaf::Nat | WireLeaf::Bool | WireLeaf::Int => self.table().list_force_func(),
            },
        };

        vec![curios_wasm::Instr::Call { func_name: force }]
    }

    /// The wire→rope step for one host result: a reference re-enters as a host-built flat payload and is embedded into a fresh leaf — deeply for `List(Bytes)`, whose elements the host lowered as raw `$bytes`. A `Bytes` result is then normalised, so a small host answer enters the guest world already canonical; a `Handle` is exempt statically — its token is always four bytes, past the immediate envelope.
    fn wire_embed_instrs(&self, wire_type: &WireType) -> Vec<curios_wasm::Instr> {
        let embed = match wire_type {
            WireType::Nat | WireType::Bool | WireType::Int => return vec![],
            WireType::Bytes => {
                return vec![
                    curios_wasm::Instr::Call {
                        func_name: self.table().bytes_embed_func(),
                    },
                    curios_wasm::Instr::Call {
                        func_name: self.table().bytes_norm_func(),
                    },
                ];
            }
            WireType::Handle => self.table().bytes_embed_func(),
            WireType::List(inner) => match inner {
                WireLeaf::Bytes | WireLeaf::Handle => self.table().list_bytes_embed_func(),
                WireLeaf::Nat | WireLeaf::Bool | WireLeaf::Int => self.table().list_embed_func(),
            },
        };

        vec![curios_wasm::Instr::Call { func_name: embed }]
    }

    /// Emit a host intrinsic call in tail position, then branch to its resume. Models `call_direct_instrs`: load operands, call the host import, then either fall through to the function's return (when the resume happens to be the sentinel) or set up the dispatcher and branch into the resume block.
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

                // Embed a reference result back into a rope. Only the *final* result may be a reference: an earlier one would sit under later stack values, and embedding it would need juggling through locals. Every host signature keeps references last.
                for (_, wire_type) in signature.results.iter().rev().skip(1) {
                    debug_assert!(
                        matches!(wire_type, WireType::Nat | WireType::Bool | WireType::Int),
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
            EmissionHostTarget::Exit { code } => {
                output.extend(self.load_value_instrs(code, LoadAs::Nat));
                output.push(curios_wasm::Instr::Call {
                    func_name: self.table().exit_func().clone(),
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
            EmissionCellTarget::Reserve { resume } => {
                output.push(curios_wasm::Instr::StructNewDefault {
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
                // The field is declared nullable, so its `struct.get` is typed `anyref` — and on the sentinel path this value *is* the function's `(ref any)` result. `New` takes an init and `Set` takes a value, so the one null a cell can hold is a `Reserve`d knot member's before its initializer has stored it, and this coercion is where reading it traps — the loud end of a read the verifier could not see to refuse, where the placeholder it once held computed on in silence. Without the coercion the module is ill-typed besides, which only Binaryen's repair used to hide.
                output.push(curios_wasm::Instr::RefAsNonNull);
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
    /// The same cast, admitting null. A row slot its constructor does not write holds null, and a region split into its slots and rebuilt travels that null back through this store — so the position that fills a typed slot is the one position whose cast must not trap on it.
    ConcreteOrNull(curios_wasm::TypeName),
    Nat,
    Int,
    Flt,
    Bin(Grain),
    List,
}

/// How a value in its register carrier is boxed back into a reference: an `i31ref` for the scalar carriers, the `Flt` struct for `f32`, and nothing at all for a representation that already names one.
///
/// The dual of [`LoadAs::of`], and the reason this reads a [`Repr`] rather than a dedicated two-variant enum: a projection or a list read yields whatever was stored, so "no boxing" is a representation rather than a missing one.
/// The zero of `carrier`, or the boxed zero when the destination holds a reference.
///
/// This is what a filler is materialised as, and the reason it is a function of the *destination* rather than of the filler: a slot's carrier is settled by the representation analysis from the uses of the parameter it feeds, long after the pass that placed the filler. The arms mirror [`Table::local_type`] exactly, including the reference carriers that analysis never answers — a local declared at the top reference type takes the boxed zero, whichever way it got there.
/// The zero of one row slot: the register zero for a scalar carrier, a null for a declared heap type, and the boxed zero for the uniform reference.
///
/// A typed reference slot takes `ref.null none` rather than the boxed zero because the boxed zero is not of its type — and because null is what a filler *means*, where an `i31` zero is a perfectly good `Nat` standing in a position that holds no value at all.
pub(crate) fn slot_zero_instrs(slot: CpsSlot) -> Vec<curios_wasm::Instr> {
    match slot {
        CpsSlot::Tag | CpsSlot::Nat | CpsSlot::Int => {
            vec![curios_wasm::Instr::I32Const { value: 0 }]
        }
        CpsSlot::Flt => vec![curios_wasm::Instr::F32Const { value: 0.0 }],
        CpsSlot::List | CpsSlot::Closure(_) | CpsSlot::Row(_) => {
            vec![curios_wasm::Instr::RefNull {
                heap_type: curios_wasm::HeapType::Abstract(curios_wasm::AbsHeapType::None),
            }]
        }
        CpsSlot::Opaque => zero_instrs(None),
    }
}

pub(crate) fn zero_instrs(carrier: Option<Repr>) -> Vec<curios_wasm::Instr> {
    match carrier {
        Some(Repr::Nat | Repr::Int) => vec![curios_wasm::Instr::I32Const { value: 0 }],
        Some(Repr::Flt) => vec![curios_wasm::Instr::F32Const { value: 0.0 }],
        Some(Repr::Bin(_) | Repr::List | Repr::Ref) | None => vec![
            curios_wasm::Instr::I32Const { value: 0 },
            curios_wasm::Instr::RefI31,
        ],
    }
}

pub(crate) fn box_instr(repr: &Repr, table: &Table) -> Option<curios_wasm::Instr> {
    match repr {
        Repr::Nat | Repr::Int => Some(curios_wasm::Instr::RefI31),
        Repr::Flt => Some(curios_wasm::Instr::StructNew {
            type_name: table.flt_type(),
        }),
        Repr::Bin(_) | Repr::List | Repr::Ref => None,
    }
}

impl LoadAs {
    /// The load that realises a representation the IR states.
    ///
    /// This is the whole of the translation between [`Repr`] — what an operation declares it reads — and the instructions that deliver it. Every operand load in the emitter resolves through here rather than naming a `LoadAs` directly, so the demand has one statement (the intrinsic roster) and one realisation (this function), and the two cannot drift apart.
    ///
    /// `Repr::Ref` maps to `Null` rather than `NonNull`: an uninterpreted operand is passed along exactly as stored, and asserting non-nullness of a value nothing reads would emit an instruction for no reader.
    pub(crate) fn of(repr: &Repr) -> Self {
        match repr {
            Repr::Nat => Self::Nat,
            Repr::Int => Self::Int,
            Repr::Flt => Self::Flt,
            Repr::Bin(grain) => Self::Bin(*grain),
            Repr::List => Self::List,
            Repr::Ref => Self::Null,
        }
    }
}

/// How a host-import operand of the given wire type is loaded at the call site: `Nat`/`Bool` unbox their i31 carrier unsigned to a raw i32, `Int` unboxes signed (the `poll(2)` timeout convention), and the reference shapes cast to their rope base type (a handle is its `Bytes` token) — the force step to the flat wire payload follows in `wire_force_instrs`.
impl From<&WireType> for LoadAs {
    fn from(wire_type: &WireType) -> LoadAs {
        match wire_type {
            WireType::Nat | WireType::Bool => LoadAs::Nat,
            WireType::Int => LoadAs::Int,
            WireType::Bytes | WireType::Handle => LoadAs::Bin(Grain::X),
            WireType::List(_) => LoadAs::List,
        }
    }
}
