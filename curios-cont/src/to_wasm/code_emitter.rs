use {
    super::{Context, LoadAs, RopeData},
    curios_wasm::{BlockType, HeapType, Instr, LabelName, LocalName, NumType, RefType, ValType},
};

/// How a freshly-computed numeric value is boxed: an `i31ref` for `Nat`/`Int`/`Bln`
/// results, or the `Flt` struct for `f32` results.
enum WrapAs {
    I31,
    Flt,
}

#[derive(Debug)]
pub struct CodeEmitter<'a, 'b, 'c> {
    context: &'c mut Context<'a, 'b>,
}

impl<'a, 'b, 'c> CodeEmitter<'a, 'b, 'c> {
    pub fn new(context: &'c mut Context<'a, 'b>) -> Self {
        Self { context }
    }

    fn emit_instr(&mut self, instr: Instr) {
        self.context
            .this_frame()
            .expect("`CodeEmitter` called outside a region")
            .instrs
            .push(instr);
    }

    fn emit_instrs<I>(&mut self, instrs: I)
    where
        I: IntoIterator<Item = Instr>,
    {
        self.context
            .this_frame()
            .expect("`CodeEmitter` called outside a region")
            .instrs
            .extend(instrs);
    }

    /// How a numeric result is boxed before it is stored back into its local.
    fn emit_wrap(&mut self, wrap: WrapAs) {
        let instr = match wrap {
            WrapAs::I31 => Instr::RefI31,
            WrapAs::Flt => Instr::StructNew {
                type_name: self.context.table().flt_type(),
            },
        };

        self.emit_instr(instr);
    }

    /// Lower a one-operand numeric op: load the operand, apply `op`, box the result, store it.
    fn emit_unary_op(
        &mut self,
        result_local: &LocalName,
        operand: &crate::ValueName,
        load: LoadAs,
        op: Instr,
        wrap: WrapAs,
    ) {
        self.emit_instrs(self.context.load_value_instrs(operand, load));
        self.emit_instr(op);
        self.emit_wrap(wrap);
        self.emit_instr(Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// Lower a two-operand numeric op: both operands share `load`, apply `op`, box, store.
    fn emit_binary_op(
        &mut self,
        result_local: &LocalName,
        left: &crate::ValueName,
        right: &crate::ValueName,
        load: LoadAs,
        op: Instr,
        wrap: WrapAs,
    ) {
        self.emit_instrs(self.context.load_value_instrs(left, load.clone()));
        self.emit_instrs(self.context.load_value_instrs(right, load));
        self.emit_instr(op);
        self.emit_wrap(wrap);
        self.emit_instr(Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// Lower an unsigned-`Nat` binary op that may overflow the i31 carrier:
    /// apply `op`, trap (via the special label) if bit 31 of the result is set,
    /// else box with `ref.i31` and store.
    fn emit_checked_nat_op(
        &mut self,
        result_local: &LocalName,
        left: &crate::ValueName,
        right: &crate::ValueName,
        name: &str,
        op: Instr,
    ) {
        let local_name = self.context.push_local(name, ValType::Num(NumType::I32));
        self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Nat));
        self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Nat));
        self.emit_instr(op);
        self.emit_instr(Instr::LocalTee {
            local_name: local_name.clone(),
        });
        self.emit_instr(Instr::I32Const { value: 31 });
        self.emit_instr(Instr::I32ShrU);
        self.emit_instr(Instr::If {
            label_name: self.context.table().special_label(),
            block_type: BlockType::Empty,
            then_instructions: vec![Instr::Unreachable],
            else_instructions: vec![],
        });
        self.emit_instr(Instr::LocalGet { local_name });
        self.emit_instr(Instr::RefI31);
        self.emit_instr(Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// Lower a signed-`Int` binary op that may overflow the i31 carrier: apply
    /// `op`, trap (via the special label) if the result leaves the signed
    /// 31-bit range, else box with `ref.i31` and store.
    fn emit_checked_int_op(
        &mut self,
        result_local: &LocalName,
        left: &crate::ValueName,
        right: &crate::ValueName,
        name: &str,
        op: Instr,
    ) {
        let local_name = self.context.push_local(name, ValType::Num(NumType::I32));
        self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
        self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
        self.emit_instr(op);
        self.emit_instr(Instr::LocalTee {
            local_name: local_name.clone(),
        });
        self.emit_instr(Instr::I32Const { value: 1 });
        self.emit_instr(Instr::I32Shl);
        self.emit_instr(Instr::LocalGet {
            local_name: local_name.clone(),
        });
        self.emit_instr(Instr::I32Xor);
        self.emit_instr(Instr::I32Const { value: 31 });
        self.emit_instr(Instr::I32ShrU);
        self.emit_instr(Instr::If {
            label_name: self.context.table().special_label(),
            block_type: BlockType::Empty,
            then_instructions: vec![Instr::Unreachable],
            else_instructions: vec![],
        });
        self.emit_instr(Instr::LocalGet { local_name });
        self.emit_instr(Instr::RefI31);
        self.emit_instr(Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// `struct.get` on a rope base — the `len`/`tag` reads that never force.
    fn rope_get(&self, rope: &RopeData, field: &curios_wasm::FieldName) -> Instr {
        Instr::StructGet {
            type_name: rope.base.clone(),
            field_name: field.clone(),
        }
    }

    /// Load a rope-carried operand and force it to its flat payload.
    fn force_instrs(
        &self,
        operand: &'a crate::ValueName,
        load: LoadAs,
        force_func: curios_wasm::FuncName,
    ) -> Vec<Instr> {
        let mut instrs = self.context.load_value_instrs(operand, load);
        instrs.push(Instr::Call {
            func_name: force_func,
        });
        instrs
    }

    /// Concatenate two loaded ropes into `dest`: answer the other side when
    /// one is empty (the runtime identity shortcuts keep chains of empty
    /// seeds from deepening), else one O(1) node.
    fn concat_pair_instrs(
        &self,
        lhs: Vec<Instr>,
        rhs: Vec<Instr>,
        dest: &LocalName,
        rope: &RopeData,
    ) -> Vec<Instr> {
        let set_dest = Instr::LocalSet {
            local_name: dest.clone(),
        };

        let mut node = vec![Instr::I32Const { value: 1 }];
        node.extend(lhs.clone());
        node.push(self.rope_get(rope, &rope.len_field));
        node.extend(rhs.clone());
        node.push(self.rope_get(rope, &rope.len_field));
        node.push(Instr::I32Add);
        node.extend(lhs.clone());
        node.extend(rhs.clone());
        node.push(Instr::RefNull {
            heap_type: HeapType::Concrete(rope.payload.clone()),
        });
        node.push(Instr::StructNew {
            type_name: rope.node.clone(),
        });
        node.push(set_dest.clone());

        let mut rhs_empty_check = rhs.clone();
        rhs_empty_check.push(self.rope_get(rope, &rope.len_field));
        rhs_empty_check.push(Instr::I32Eqz);
        rhs_empty_check.push(Instr::If {
            label_name: self.context.table().special_label(),
            block_type: BlockType::Empty,
            then_instructions: {
                let mut then = lhs.clone();
                then.push(set_dest.clone());
                then
            },
            else_instructions: node,
        });

        let mut instrs = lhs;
        instrs.push(self.rope_get(rope, &rope.len_field));
        instrs.push(Instr::I32Eqz);
        instrs.push(Instr::If {
            label_name: self.context.table().special_label(),
            block_type: BlockType::Empty,
            then_instructions: {
                let mut then = rhs;
                then.push(set_dest);
                then
            },
            else_instructions: rhs_empty_check,
        });

        instrs
    }

    /// Lower an n-ary rope concat: the empty case is an empty leaf, a single
    /// operand is an alias, and longer runs fold pairs left-leaning through
    /// `result_local` — n−1 nodes, no copying.
    fn emit_rope_concat(
        &mut self,
        result_local: &LocalName,
        operands: &'a [crate::ValueName],
        load: LoadAs,
        rope: &RopeData,
    ) {
        match operands {
            [] => {
                self.emit_instr(Instr::I32Const { value: 0 });
                self.emit_instr(Instr::I32Const { value: 0 });
                self.emit_instr(Instr::I32Const { value: 0 });
                self.emit_instr(Instr::ArrayNewDefault {
                    type_name: rope.payload.clone(),
                });
                self.emit_instr(Instr::StructNew {
                    type_name: rope.leaf.clone(),
                });
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            [only] => {
                self.emit_instrs(self.context.load_value_instrs(only, load));
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            [first, rest @ ..] => {
                let mut lhs = self.context.load_value_instrs(first, load.clone());

                for operand in rest {
                    let rhs = self.context.load_value_instrs(operand, load.clone());
                    let pair = self.concat_pair_instrs(lhs, rhs, result_local, rope);
                    self.emit_instrs(pair);
                    // Later pairs read the settled accumulator back out of
                    // the result local.
                    lhs = vec![
                        Instr::LocalGet {
                            local_name: result_local.clone(),
                        },
                        Instr::RefCast {
                            ref_type: RefType {
                                is_nullable: false,
                                heap_type: HeapType::Concrete(rope.base.clone()),
                            },
                        },
                    ];
                }
            }
        }
    }

    /// Lower a rope append: a fresh one-element leaf on the right of one node.
    /// The per-element builders (`Json` escaping, UTF-8 emit) are O(1)/step.
    fn emit_rope_append(
        &mut self,
        result_local: &LocalName,
        carrier: &'a crate::ValueName,
        elem_instrs: Vec<Instr>,
        load: LoadAs,
        rope: &RopeData,
    ) {
        self.emit_instr(Instr::I32Const { value: 1 });
        self.emit_instrs(self.context.load_value_instrs(carrier, load.clone()));
        self.emit_instr(self.rope_get(rope, &rope.len_field));
        self.emit_instr(Instr::I32Const { value: 1 });
        self.emit_instr(Instr::I32Add);
        self.emit_instrs(self.context.load_value_instrs(carrier, load));
        self.emit_instr(Instr::I32Const { value: 0 });
        self.emit_instr(Instr::I32Const { value: 1 });
        self.emit_instrs(elem_instrs);
        self.emit_instr(Instr::ArrayNewFixed {
            type_name: rope.payload.clone(),
            length: 1,
        });
        self.emit_instr(Instr::StructNew {
            type_name: rope.leaf.clone(),
        });
        self.emit_instr(Instr::RefNull {
            heap_type: HeapType::Concrete(rope.payload.clone()),
        });
        self.emit_instr(Instr::StructNew {
            type_name: rope.node.clone(),
        });
        self.emit_instr(Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// Lower a rope slice: force the source, copy the window into a fresh
    /// payload built in `result_local`, then wrap it as a leaf in place.
    #[allow(clippy::too_many_arguments)]
    fn emit_rope_slice(
        &mut self,
        result_local: &LocalName,
        value_name: &'a crate::ValueName,
        carrier: &'a crate::ValueName,
        start: &'a crate::ValueName,
        end: &'a crate::ValueName,
        load: LoadAs,
        rope: &RopeData,
        force_func: curios_wasm::FuncName,
    ) {
        self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Nat));
        self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
        self.emit_instr(Instr::I32Sub);
        self.emit_instr(Instr::ArrayNewDefault {
            type_name: rope.payload.clone(),
        });
        self.emit_instr(Instr::LocalSet {
            local_name: result_local.clone(),
        });

        self.emit_instrs(
            self.context
                .load_value_instrs(value_name, LoadAs::Concrete(rope.payload.clone())),
        );
        self.emit_instr(Instr::I32Const { value: 0 });
        self.emit_instrs(self.force_instrs(carrier, load, force_func));
        self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
        self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Nat));
        self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
        self.emit_instr(Instr::I32Sub);
        self.emit_instr(Instr::ArrayCopy {
            source_name: rope.payload.clone(),
            target_name: rope.payload.clone(),
        });

        self.emit_instr(Instr::I32Const { value: 0 });
        self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Nat));
        self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
        self.emit_instr(Instr::I32Sub);
        self.emit_instrs(
            self.context
                .load_value_instrs(value_name, LoadAs::Concrete(rope.payload.clone())),
        );
        self.emit_instr(Instr::StructNew {
            type_name: rope.leaf.clone(),
        });
        self.emit_instr(Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// Map closure `f` over array `src` into a fresh array of the same length
    /// in a single allocation. Force the source once, then one pass: size the
    /// result from the payload's length and fill slot `idx` with
    /// `f(src[idx])` — the closure invoked inline by `call_ref` (its result is
    /// left on the stack, exactly as a non-tail closure call). The scratch
    /// buffer never escapes this helper (it is sealed into a fresh leaf at the
    /// end), so the map stays a pure value at the IR level (no linearity
    /// reasoning) while lowering to a mutating fill.
    fn emit_map(
        &mut self,
        result_local: &LocalName,
        src: &'a crate::ValueName,
        f: &'a crate::ValueName,
    ) {
        let rope = self.context.table().arr_rope();
        let force = self.context.table().force_arr_func();
        let elems_ref = RefType {
            is_nullable: false,
            heap_type: HeapType::Concrete(rope.payload.clone()),
        };
        // `f` is a unary closure `(A) -> B`; reuse the arity-1 closure calling
        // convention (env as the self argument, the funcref in its special field).
        let envr_type = self.context.table().find_envr_type(1);
        let clsr_type = self.context.table().find_clsr_type(1);
        let special_field = self.context.table().special_field();

        let selems_local = self.context.push_local(
            "selems",
            ValType::Ref(RefType {
                is_nullable: true,
                heap_type: HeapType::Concrete(rope.payload.clone()),
            }),
        );
        let count_local = self.context.push_local("count", ValType::Num(NumType::I32));
        let idx_local = self.context.push_local("idx", ValType::Num(NumType::I32));

        let map_loop = LabelName::from(format!("{}_map_loop", result_local));
        let map_step = LabelName::from(format!("{}_map_step", result_local));

        // selems = force(src); count = selems.len
        self.emit_instrs(self.force_instrs(src, LoadAs::Arr, force));
        self.emit_instr(Instr::LocalSet {
            local_name: selems_local.clone(),
        });
        self.emit_instr(Instr::LocalGet {
            local_name: selems_local.clone(),
        });
        self.emit_instr(Instr::ArrayLen);
        self.emit_instr(Instr::LocalSet {
            local_name: count_local.clone(),
        });

        // result = new payload sized `count` (default-filled, overwritten below)
        self.emit_instr(Instr::LocalGet {
            local_name: count_local.clone(),
        });
        self.emit_instr(Instr::ArrayNewDefault {
            type_name: rope.payload.clone(),
        });
        self.emit_instr(Instr::LocalSet {
            local_name: result_local.clone(),
        });

        // idx = 0
        self.emit_instr(Instr::I32Const { value: 0 });
        self.emit_instr(Instr::LocalSet {
            local_name: idx_local.clone(),
        });

        // step: result[idx] = f(selems[idx]); idx += 1; continue
        let mut step_body = vec![
            Instr::LocalGet {
                local_name: result_local.clone(),
            },
            Instr::RefCast {
                ref_type: elems_ref,
            },
            Instr::LocalGet {
                local_name: idx_local.clone(),
            },
        ];
        // value = f(selems[idx]) — the closure as its own self/env argument first,
        // then the element, then the funcref pulled from the env struct.
        step_body.extend(self.context.load_value_instrs(f, LoadAs::NonNull));
        step_body.push(Instr::LocalGet {
            local_name: selems_local,
        });
        step_body.push(Instr::LocalGet {
            local_name: idx_local.clone(),
        });
        step_body.push(Instr::ArrayGet {
            type_name: rope.payload.clone(),
        });
        step_body.push(Instr::RefAsNonNull);
        step_body.extend(
            self.context
                .load_value_instrs(f, LoadAs::Concrete(envr_type.clone())),
        );
        step_body.push(Instr::StructGet {
            type_name: envr_type,
            field_name: special_field,
        });
        step_body.push(Instr::RefAsNonNull);
        step_body.push(Instr::CallRef {
            type_name: clsr_type,
        });
        step_body.push(Instr::ArraySet {
            type_name: rope.payload.clone(),
        });
        // idx += 1; continue
        step_body.extend([
            Instr::LocalGet {
                local_name: idx_local.clone(),
            },
            Instr::I32Const { value: 1 },
            Instr::I32Add,
            Instr::LocalSet {
                local_name: idx_local.clone(),
            },
            Instr::Br {
                label_name: map_loop.clone(),
            },
        ]);

        self.emit_instr(Instr::Loop {
            label_name: map_loop,
            block_type: BlockType::Empty,
            instructions: vec![
                Instr::LocalGet {
                    local_name: idx_local,
                },
                Instr::LocalGet {
                    local_name: count_local.clone(),
                },
                Instr::I32LtU,
                Instr::If {
                    label_name: map_step,
                    block_type: BlockType::Empty,
                    then_instructions: step_body,
                    else_instructions: vec![],
                },
            ],
        });

        // Seal the filled payload into a fresh leaf.
        self.emit_instr(Instr::I32Const { value: 0 });
        self.emit_instr(Instr::LocalGet {
            local_name: count_local,
        });
        self.emit_instr(Instr::LocalGet {
            local_name: result_local.clone(),
        });
        self.emit_instr(Instr::RefCast {
            ref_type: RefType {
                is_nullable: false,
                heap_type: HeapType::Concrete(rope.payload),
            },
        });
        self.emit_instr(Instr::StructNew {
            type_name: rope.leaf,
        });
        self.emit_instr(Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    pub fn emit(&mut self, value_name: &'a crate::ValueName, op: &'a crate::Code) {
        let result_local = self
            .context
            .find_local(value_name)
            .map(|ld| ld.local_name)
            .unwrap_or_else(|| panic!("`CodeEmitter` lacks local `{}`", value_name));

        match op {
            crate::Code::NatEql(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                Instr::I32Eq,
                WrapAs::I31,
            ),
            crate::Code::NatNeq(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                Instr::I32Ne,
                WrapAs::I31,
            ),
            crate::Code::NatAdd(left, right) => {
                self.emit_checked_nat_op(&result_local, left, right, "nat_add", Instr::I32Add)
            }
            crate::Code::NatSub(left, right) => {
                // Monus: 0 if left < right, else left - right.
                // select [val1=0, val2=left-right, cond=left<right] returns val1 when cond != 0.
                self.emit_instr(Instr::I32Const { value: 0 });
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Nat));
                self.emit_instr(Instr::I32Sub);
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Nat));
                self.emit_instr(Instr::I32LtU);
                self.emit_instr(Instr::Select { val_types: vec![] });
                self.emit_instr(Instr::RefI31);
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            crate::Code::NatMul(left, right) => {
                let local_name = self
                    .context
                    .push_local("nat_mul", ValType::Num(NumType::I64));
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Nat));
                self.emit_instr(Instr::I64ExtendI32U);
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Nat));
                self.emit_instr(Instr::I64ExtendI32U);
                self.emit_instr(Instr::I64Mul);
                self.emit_instr(Instr::LocalTee {
                    local_name: local_name.clone(),
                });
                self.emit_instr(Instr::I64Const { value: 31 });
                self.emit_instr(Instr::I64ShrU);
                self.emit_instr(Instr::I32WrapI64);
                self.emit_instr(Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: BlockType::Empty,
                    then_instructions: vec![Instr::Unreachable],
                    else_instructions: vec![],
                });
                self.emit_instr(Instr::LocalGet { local_name });
                self.emit_instr(Instr::I32WrapI64);
                self.emit_instr(Instr::RefI31);
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            crate::Code::NatLt(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                Instr::I32LtU,
                WrapAs::I31,
            ),
            crate::Code::NatDiv(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                Instr::I32DivU,
                WrapAs::I31,
            ),
            crate::Code::NatRem(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                Instr::I32RemU,
                WrapAs::I31,
            ),
            crate::Code::NatGt(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                Instr::I32GtU,
                WrapAs::I31,
            ),
            crate::Code::NatLte(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                Instr::I32LeU,
                WrapAs::I31,
            ),
            crate::Code::NatGte(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                Instr::I32GeU,
                WrapAs::I31,
            ),
            crate::Code::IntEql(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                Instr::I32Eq,
                WrapAs::I31,
            ),
            crate::Code::IntNeq(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                Instr::I32Ne,
                WrapAs::I31,
            ),
            crate::Code::IntAdd(left, right) => {
                self.emit_checked_int_op(&result_local, left, right, "int_add", Instr::I32Add)
            }
            crate::Code::IntSub(left, right) => {
                self.emit_checked_int_op(&result_local, left, right, "int_sub", Instr::I32Sub)
            }
            crate::Code::IntMul(left, right) => {
                let local_name = self
                    .context
                    .push_local("int_mul", ValType::Num(NumType::I64));
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instr(Instr::I64ExtendI32S);
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(Instr::I64ExtendI32S);
                self.emit_instr(Instr::I64Mul);
                self.emit_instr(Instr::LocalTee {
                    local_name: local_name.clone(),
                });
                self.emit_instr(Instr::I64Const { value: 30 });
                self.emit_instr(Instr::I64ShrS);
                self.emit_instr(Instr::LocalGet {
                    local_name: local_name.clone(),
                });
                self.emit_instr(Instr::I64Const { value: 63 });
                self.emit_instr(Instr::I64ShrS);
                self.emit_instr(Instr::I64Ne);
                self.emit_instr(Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: BlockType::Empty,
                    then_instructions: vec![Instr::Unreachable],
                    else_instructions: vec![],
                });
                self.emit_instr(Instr::LocalGet { local_name });
                self.emit_instr(Instr::I32WrapI64);
                self.emit_instr(Instr::RefI31);
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            crate::Code::IntDiv(left, right) => {
                self.emit_checked_int_op(&result_local, left, right, "int_div", Instr::I32DivS)
            }
            crate::Code::IntRem(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                Instr::I32RemS,
                WrapAs::I31,
            ),
            crate::Code::IntLt(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                Instr::I32LtS,
                WrapAs::I31,
            ),
            crate::Code::IntGt(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                Instr::I32GtS,
                WrapAs::I31,
            ),
            crate::Code::IntLte(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                Instr::I32LeS,
                WrapAs::I31,
            ),
            crate::Code::IntGte(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                Instr::I32GeS,
                WrapAs::I31,
            ),
            crate::Code::NatAnd(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                Instr::I32And,
                WrapAs::I31,
            ),
            crate::Code::NatOr(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                Instr::I32Or,
                WrapAs::I31,
            ),
            crate::Code::NatXor(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                Instr::I32Xor,
                WrapAs::I31,
            ),
            // Left shift truncates into the 31-bit carrier rather than trapping:
            // `ref.i31` already drops bit 31, so `i32.shl` + `WrapAs::I31` is a
            // guard-free truncating shift (matching `shr`, which never overflows).
            crate::Code::NatShl(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                Instr::I32Shl,
                WrapAs::I31,
            ),
            crate::Code::NatShr(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                Instr::I32ShrU,
                WrapAs::I31,
            ),
            crate::Code::NatRotl(left, right) => {
                self.emit_checked_nat_op(&result_local, left, right, "nat_rotl", Instr::I32Rotl)
            }
            crate::Code::NatRotr(left, right) => {
                self.emit_checked_nat_op(&result_local, left, right, "nat_rotr", Instr::I32Rotr)
            }
            crate::Code::NatClz(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Nat,
                Instr::I32Clz,
                WrapAs::I31,
            ),
            crate::Code::NatCtz(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Nat,
                Instr::I32Ctz,
                WrapAs::I31,
            ),
            crate::Code::NatPopcnt(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Nat,
                Instr::I32Popcnt,
                WrapAs::I31,
            ),
            crate::Code::NatEqz(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Nat,
                Instr::I32Eqz,
                WrapAs::I31,
            ),
            crate::Code::IntAnd(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                Instr::I32And,
                WrapAs::I31,
            ),
            crate::Code::IntOr(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                Instr::I32Or,
                WrapAs::I31,
            ),
            crate::Code::IntXor(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                Instr::I32Xor,
                WrapAs::I31,
            ),
            // Left shift truncates into the signed 31-bit carrier rather than
            // trapping: `ref.i31` drops bit 31 and the value reloads sign-extended
            // from bit 30, so `i32.shl` + `WrapAs::I31` is a guard-free truncating
            // shift, matching `Nat/shl` and `shr` (which never overflow).
            crate::Code::IntShl(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                Instr::I32Shl,
                WrapAs::I31,
            ),
            crate::Code::IntShr(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                Instr::I32ShrS,
                WrapAs::I31,
            ),
            crate::Code::IntRotl(left, right) => {
                self.emit_checked_int_op(&result_local, left, right, "int_rotl", Instr::I32Rotl)
            }
            crate::Code::IntRotr(left, right) => {
                self.emit_checked_int_op(&result_local, left, right, "int_rotr", Instr::I32Rotr)
            }
            crate::Code::IntClz(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Int,
                Instr::I32Clz,
                WrapAs::I31,
            ),
            crate::Code::IntCtz(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Int,
                Instr::I32Ctz,
                WrapAs::I31,
            ),
            crate::Code::IntPopcnt(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Int,
                Instr::I32Popcnt,
                WrapAs::I31,
            ),
            crate::Code::IntEqz(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Int,
                Instr::I32Eqz,
                WrapAs::I31,
            ),
            crate::Code::FltAdd(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                Instr::F32Add,
                WrapAs::Flt,
            ),
            crate::Code::FltSub(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                Instr::F32Sub,
                WrapAs::Flt,
            ),
            crate::Code::FltMul(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                Instr::F32Mul,
                WrapAs::Flt,
            ),
            crate::Code::FltDiv(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                Instr::F32Div,
                WrapAs::Flt,
            ),
            // WebAssembly has no `f32.rem`, so expand the C `fmod` definition
            // `x - trunc(x / y) * y` inline (`x`/`y` are locals, loaded twice).
            crate::Code::FltRem(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(Instr::F32Div);
                self.emit_instr(Instr::F32Trunc);
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(Instr::F32Mul);
                self.emit_instr(Instr::F32Sub);
                self.emit_wrap(WrapAs::Flt);
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            crate::Code::FltEql(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                Instr::F32Eq,
                WrapAs::I31,
            ),
            crate::Code::FltNeq(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                Instr::F32Ne,
                WrapAs::I31,
            ),
            crate::Code::FltLt(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                Instr::F32Lt,
                WrapAs::I31,
            ),
            crate::Code::FltGt(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                Instr::F32Gt,
                WrapAs::I31,
            ),
            crate::Code::FltLte(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                Instr::F32Le,
                WrapAs::I31,
            ),
            crate::Code::FltGte(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                Instr::F32Ge,
                WrapAs::I31,
            ),
            crate::Code::FltMin(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                Instr::F32Min,
                WrapAs::Flt,
            ),
            crate::Code::FltMax(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                Instr::F32Max,
                WrapAs::Flt,
            ),
            crate::Code::FltNeg(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Flt,
                Instr::F32Neg,
                WrapAs::Flt,
            ),
            crate::Code::FltAbs(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Flt,
                Instr::F32Abs,
                WrapAs::Flt,
            ),
            crate::Code::FltSqrt(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Flt,
                Instr::F32Sqrt,
                WrapAs::Flt,
            ),
            crate::Code::FltFloor(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Flt,
                Instr::F32Floor,
                WrapAs::Flt,
            ),
            crate::Code::FltCeil(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Flt,
                Instr::F32Ceil,
                WrapAs::Flt,
            ),
            crate::Code::FltTrunc(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Flt,
                Instr::F32Trunc,
                WrapAs::Flt,
            ),
            crate::Code::FltNearest(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Flt,
                Instr::F32Nearest,
                WrapAs::Flt,
            ),
            crate::Code::FltCopysign(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                Instr::F32Copysign,
                WrapAs::Flt,
            ),
            crate::Code::NatToInt(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Nat));
                self.emit_instr(Instr::RefI31);
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            crate::Code::NatToFlt(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Nat,
                Instr::F32ConvertI32U,
                WrapAs::Flt,
            ),
            crate::Code::IntToNat(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(Instr::RefI31);
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            crate::Code::IntToFlt(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Int,
                Instr::F32ConvertI32S,
                WrapAs::Flt,
            ),
            crate::Code::FltToLeBin(operand) => {
                // Reinterpret the f32 as its IEEE-754 bit pattern and split it into
                // the four little-endian bytes. The `$bytes` payload is `i8`-packed, so
                // `array.new_fixed` truncates each shifted i32 to its low byte --
                // byte-for-byte `f32::to_le_bytes`, with no host round-trip.
                let bits_local = self
                    .context
                    .push_local("flt_bits", ValType::Num(NumType::I32));
                let rope = self.context.table().bin_rope();
                self.emit_instr(Instr::I32Const { value: 0 });
                self.emit_instr(Instr::I32Const { value: 4 });
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(Instr::I32ReinterpretF32);
                self.emit_instr(Instr::LocalTee {
                    local_name: bits_local.clone(),
                });
                for shift in [8, 16, 24] {
                    self.emit_instr(Instr::LocalGet {
                        local_name: bits_local.clone(),
                    });
                    self.emit_instr(Instr::I32Const { value: shift });
                    self.emit_instr(Instr::I32ShrU);
                }
                self.emit_instr(Instr::ArrayNewFixed {
                    type_name: rope.payload,
                    length: 4,
                });
                self.emit_instr(Instr::StructNew {
                    type_name: rope.leaf,
                });
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            crate::Code::FltToNat(operand) => {
                let local_name = self
                    .context
                    .push_local("flt_to_nat", ValType::Num(NumType::I32));
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(Instr::I32TruncF32U);
                self.emit_instr(Instr::LocalTee {
                    local_name: local_name.clone(),
                });
                self.emit_instr(Instr::I32Const { value: 31 });
                self.emit_instr(Instr::I32ShrU);
                self.emit_instr(Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: BlockType::Empty,
                    then_instructions: vec![Instr::Unreachable],
                    else_instructions: vec![],
                });
                self.emit_instr(Instr::LocalGet { local_name });
                self.emit_instr(Instr::RefI31);
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            crate::Code::FltToInt(operand) => {
                let local_name = self
                    .context
                    .push_local("flt_to_int", ValType::Num(NumType::I32));
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(Instr::I32TruncF32S);
                self.emit_instr(Instr::LocalTee {
                    local_name: local_name.clone(),
                });
                self.emit_instr(Instr::I32Const { value: 1 });
                self.emit_instr(Instr::I32Shl);
                self.emit_instr(Instr::LocalGet {
                    local_name: local_name.clone(),
                });
                self.emit_instr(Instr::I32Xor);
                self.emit_instr(Instr::I32Const { value: 31 });
                self.emit_instr(Instr::I32ShrU);
                self.emit_instr(Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: BlockType::Empty,
                    then_instructions: vec![Instr::Unreachable],
                    else_instructions: vec![],
                });
                self.emit_instr(Instr::LocalGet { local_name });
                self.emit_instr(Instr::RefI31);
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            crate::Code::BinLen(bin) => {
                let rope = self.context.table().bin_rope();
                self.emit_unary_op(
                    &result_local,
                    bin,
                    LoadAs::Bin,
                    self.rope_get(&rope, &rope.len_field),
                    WrapAs::I31,
                );
            }
            crate::Code::BinEql(left, right) => {
                let rope = self.context.table().bin_rope();
                let force = self.context.table().force_bin_func();

                let lbytes_local = self.context.push_local(
                    "lbytes",
                    ValType::Ref(RefType {
                        is_nullable: true,
                        heap_type: HeapType::Concrete(rope.payload.clone()),
                    }),
                );
                let rbytes_local = self.context.push_local(
                    "rbytes",
                    ValType::Ref(RefType {
                        is_nullable: true,
                        heap_type: HeapType::Concrete(rope.payload.clone()),
                    }),
                );
                let idx_local = self.context.push_local("idx", ValType::Num(NumType::I32));
                let result_raw_local = self.context.push_local("eql", ValType::Num(NumType::I32));

                let done_label = LabelName::from(format!("{}_done", result_local));
                let loop_label = LabelName::from(format!("{}_loop", result_local));
                let if_label = LabelName::from(format!("{}_if", result_local));

                let load_left = vec![Instr::LocalGet {
                    local_name: lbytes_local.clone(),
                }];
                let load_right = vec![Instr::LocalGet {
                    local_name: rbytes_local.clone(),
                }];

                // Build the loop body instructions.
                let mut loop_instrs = Vec::new();

                // if idx >= left.len: all bytes matched, result = true, exit block
                loop_instrs.push(Instr::LocalGet {
                    local_name: idx_local.clone(),
                });
                loop_instrs.extend(load_left.clone());
                loop_instrs.push(Instr::ArrayLen);
                loop_instrs.push(Instr::I32GeU);
                loop_instrs.push(Instr::If {
                    label_name: if_label,
                    block_type: BlockType::Empty,
                    then_instructions: vec![
                        Instr::I32Const { value: 1 },
                        Instr::LocalSet {
                            local_name: result_raw_local.clone(),
                        },
                        Instr::Br {
                            label_name: done_label.clone(),
                        },
                    ],
                    else_instructions: vec![],
                });

                // if left[idx] != right[idx]: mismatch, exit block (result stays false)
                loop_instrs.extend(load_left.clone());
                loop_instrs.push(Instr::LocalGet {
                    local_name: idx_local.clone(),
                });
                loop_instrs.push(Instr::ArrayGetU {
                    type_name: rope.payload.clone(),
                });
                loop_instrs.extend(load_right.clone());
                loop_instrs.push(Instr::LocalGet {
                    local_name: idx_local.clone(),
                });
                loop_instrs.push(Instr::ArrayGetU {
                    type_name: rope.payload.clone(),
                });
                loop_instrs.push(Instr::I32Ne);
                loop_instrs.push(Instr::BrIf {
                    label_name: done_label.clone(),
                });

                // idx += 1; continue loop
                loop_instrs.extend([
                    Instr::LocalGet {
                        local_name: idx_local.clone(),
                    },
                    Instr::I32Const { value: 1 },
                    Instr::I32Add,
                    Instr::LocalSet {
                        local_name: idx_local,
                    },
                    Instr::Br {
                        label_name: loop_label.clone(),
                    },
                ]);

                // Build the outer block: length check, force, then loop.
                let mut block_instrs = Vec::new();

                // if left.len != right.len: exit block immediately (result stays
                // false) — the rope lengths answer without forcing either side.
                block_instrs.extend(self.context.load_value_instrs(left, LoadAs::Bin));
                block_instrs.push(self.rope_get(&rope, &rope.len_field));
                block_instrs.extend(self.context.load_value_instrs(right, LoadAs::Bin));
                block_instrs.push(self.rope_get(&rope, &rope.len_field));
                block_instrs.push(Instr::I32Ne);
                block_instrs.push(Instr::BrIf {
                    label_name: done_label.clone(),
                });

                // Equal lengths: force both payloads once for the byte loop.
                block_instrs.extend(self.force_instrs(left, LoadAs::Bin, force.clone()));
                block_instrs.push(Instr::LocalSet {
                    local_name: lbytes_local,
                });
                block_instrs.extend(self.force_instrs(right, LoadAs::Bin, force));
                block_instrs.push(Instr::LocalSet {
                    local_name: rbytes_local,
                });

                block_instrs.push(Instr::Loop {
                    label_name: loop_label,
                    block_type: BlockType::Empty,
                    instructions: loop_instrs,
                });

                // result_raw defaults to 0 (false); emit block; box result as i31ref
                self.emit_instr(Instr::I32Const { value: 0 });
                self.emit_instr(Instr::LocalSet {
                    local_name: result_raw_local.clone(),
                });
                self.emit_instr(Instr::Block {
                    label_name: done_label,
                    block_type: BlockType::Empty,
                    instructions: block_instrs,
                });
                self.emit_instr(Instr::LocalGet {
                    local_name: result_raw_local,
                });
                self.emit_instr(Instr::RefI31);
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local,
                });
            }
            crate::Code::BinGet(bin, idx) => {
                let force = self.context.table().force_bin_func();
                let bytes_type = self.context.table().bytes_type();
                self.emit_instrs(self.force_instrs(bin, LoadAs::Bin, force));
                self.emit_instrs(self.context.load_value_instrs(idx, LoadAs::Nat));
                self.emit_instr(Instr::ArrayGetU {
                    type_name: bytes_type,
                });
                self.emit_instr(Instr::RefI31);
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            crate::Code::BinSlice(bin, start, end) => {
                let rope = self.context.table().bin_rope();
                let force = self.context.table().force_bin_func();
                self.emit_rope_slice(
                    &result_local,
                    value_name,
                    bin,
                    start,
                    end,
                    LoadAs::Bin,
                    &rope,
                    force,
                );
            }
            crate::Code::BinAppend(bin, byte) => {
                let rope = self.context.table().bin_rope();
                let elem_instrs = self.context.load_value_instrs(byte, LoadAs::Nat);
                self.emit_rope_append(&result_local, bin, elem_instrs, LoadAs::Bin, &rope);
            }
            crate::Code::BinConcat(operands) => {
                let rope = self.context.table().bin_rope();
                self.emit_rope_concat(&result_local, operands, LoadAs::Bin, &rope);
            }
            crate::Code::ArrLen(lst) => {
                let rope = self.context.table().arr_rope();
                self.emit_unary_op(
                    &result_local,
                    lst,
                    LoadAs::Arr,
                    self.rope_get(&rope, &rope.len_field),
                    WrapAs::I31,
                );
            }
            crate::Code::ArrGet(lst, idx) => {
                let force = self.context.table().force_arr_func();
                let elems_type = self.context.table().elems_type();
                self.emit_instrs(self.force_instrs(lst, LoadAs::Arr, force));
                self.emit_instrs(self.context.load_value_instrs(idx, LoadAs::Nat));
                self.emit_instr(Instr::ArrayGet {
                    type_name: elems_type,
                });
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            crate::Code::ArrSlice(lst, start, end) => {
                let rope = self.context.table().arr_rope();
                let force = self.context.table().force_arr_func();
                self.emit_rope_slice(
                    &result_local,
                    value_name,
                    lst,
                    start,
                    end,
                    LoadAs::Arr,
                    &rope,
                    force,
                );
            }
            crate::Code::ArrAppend(lst, elem) => {
                let rope = self.context.table().arr_rope();
                let elem_instrs = self.context.load_value_instrs(elem, LoadAs::Null);
                self.emit_rope_append(&result_local, lst, elem_instrs, LoadAs::Arr, &rope);
            }
            crate::Code::ArrConcat(operands) => {
                let rope = self.context.table().arr_rope();
                self.emit_rope_concat(&result_local, operands, LoadAs::Arr, &rope);
            }
            crate::Code::ArrMap(src, f) => self.emit_map(&result_local, src, f),
            crate::Code::TplGet(tuple, index) => {
                let tpl_n_type = self.context.table().find_tpl_type(*index + 1);
                let field_name = self.context.table().tpl_field(*index);

                self.emit_instrs(
                    self.context
                        .load_value_instrs(tuple, LoadAs::Concrete(tpl_n_type.clone())),
                );

                self.emit_instr(Instr::StructGet {
                    type_name: tpl_n_type,
                    field_name,
                });
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local,
                });
            }
        }
    }
}
