use {
    super::{Context, LoadAs},
    curios_wasm::{
        BlockType, HeapType, Instr, LabelName, LocalName, NumType, RefType, TypeName, ValType,
    },
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

    /// Push the inner array `outer[idx]` on the stack, cast to its concrete type.
    /// The outer array is always the generic ref array (`arr_type`), whose elements
    /// are top refs, so each read is followed by a cast to `inner` -- `bin_type`
    /// for `Bin.flatten`, `arr_type` for `Arr.flatten`.
    fn array_get_cast_instrs(
        &self,
        operand: &'a crate::ValueName,
        idx_local: &LocalName,
        arr_type: &TypeName,
        inner: &RefType,
    ) -> Vec<Instr> {
        let mut instrs = self.context.load_value_instrs(operand, LoadAs::Arr);
        instrs.push(Instr::LocalGet {
            local_name: idx_local.clone(),
        });
        instrs.push(Instr::ArrayGet {
            type_name: arr_type.clone(),
        });
        instrs.push(Instr::RefCast {
            ref_type: inner.clone(),
        });
        instrs
    }

    /// Flatten a runtime `Arr` of arrays into one array in a single allocation.
    /// Two passes over the outer array: the first sums the inner lengths to size
    /// one result; the second copies each inner array into it at a running offset --
    /// no intermediate results. `elem_type` is the inner *and* result array type
    /// (`bin_type` for `Bin.flatten`, `arr_type` for `Arr.flatten`); `result_load`
    /// reads the freshly-built result back for the copy.
    fn emit_flatten(
        &mut self,
        result_local: &LocalName,
        value_name: &'a crate::ValueName,
        operand: &'a crate::ValueName,
        elem_type: TypeName,
        result_load: LoadAs,
    ) {
        let arr_type = self.context.table().arr_type();
        let inner = RefType {
            is_nullable: false,
            heap_type: HeapType::Concrete(elem_type.clone()),
        };

        let count_local = self.context.push_local("count", ValType::Num(NumType::I32));
        let idx_local = self.context.push_local("idx", ValType::Num(NumType::I32));
        let total_local = self.context.push_local("total", ValType::Num(NumType::I32));
        let offset_local = self
            .context
            .push_local("offset", ValType::Num(NumType::I32));

        let sum_loop = LabelName::from(format!("{}_sum_loop", result_local));
        let sum_step = LabelName::from(format!("{}_sum_step", result_local));
        let copy_loop = LabelName::from(format!("{}_copy_loop", result_local));
        let copy_step = LabelName::from(format!("{}_copy_step", result_local));

        // count = outer.len
        self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Arr));
        self.emit_instr(Instr::ArrayLen);
        self.emit_instr(Instr::LocalSet {
            local_name: count_local.clone(),
        });

        // --- pass 1: total = sum of inner lengths ---
        // The body sits under an `idx < count` guard and re-enters with `br`; when
        // the guard fails the `loop` falls through its own end, so no separate exit
        // block is needed -- the loop label alone carries the iteration.
        self.emit_instr(Instr::I32Const { value: 0 });
        self.emit_instr(Instr::LocalSet {
            local_name: total_local.clone(),
        });
        self.emit_instr(Instr::I32Const { value: 0 });
        self.emit_instr(Instr::LocalSet {
            local_name: idx_local.clone(),
        });

        // total += outer[idx].len; idx += 1; continue
        let mut sum_step_body = vec![Instr::LocalGet {
            local_name: total_local.clone(),
        }];
        sum_step_body.extend(self.array_get_cast_instrs(operand, &idx_local, &arr_type, &inner));
        sum_step_body.extend([
            Instr::ArrayLen,
            Instr::I32Add,
            Instr::LocalSet {
                local_name: total_local.clone(),
            },
            Instr::LocalGet {
                local_name: idx_local.clone(),
            },
            Instr::I32Const { value: 1 },
            Instr::I32Add,
            Instr::LocalSet {
                local_name: idx_local.clone(),
            },
            Instr::Br {
                label_name: sum_loop.clone(),
            },
        ]);

        self.emit_instr(Instr::Loop {
            label_name: sum_loop,
            block_type: BlockType::Empty,
            instructions: vec![
                Instr::LocalGet {
                    local_name: idx_local.clone(),
                },
                Instr::LocalGet {
                    local_name: count_local.clone(),
                },
                Instr::I32LtU,
                Instr::If {
                    label_name: sum_step,
                    block_type: BlockType::Empty,
                    then_instructions: sum_step_body,
                    else_instructions: vec![],
                },
            ],
        });

        // result = new array sized `total`
        self.emit_instr(Instr::LocalGet {
            local_name: total_local,
        });
        self.emit_instr(Instr::ArrayNewDefault {
            type_name: elem_type.clone(),
        });
        self.emit_instr(Instr::LocalSet {
            local_name: result_local.clone(),
        });

        // --- pass 2: copy each inner array at a running offset ---
        self.emit_instr(Instr::I32Const { value: 0 });
        self.emit_instr(Instr::LocalSet {
            local_name: offset_local.clone(),
        });
        self.emit_instr(Instr::I32Const { value: 0 });
        self.emit_instr(Instr::LocalSet {
            local_name: idx_local.clone(),
        });

        // array.copy: dest = result, dest_off = offset, src = outer[idx],
        // src_off = 0, len = outer[idx].len
        let mut copy_step_body = self.context.load_value_instrs(value_name, result_load);
        copy_step_body.push(Instr::LocalGet {
            local_name: offset_local.clone(),
        });
        copy_step_body.extend(self.array_get_cast_instrs(operand, &idx_local, &arr_type, &inner));
        copy_step_body.push(Instr::I32Const { value: 0 });
        copy_step_body.extend(self.array_get_cast_instrs(operand, &idx_local, &arr_type, &inner));
        copy_step_body.push(Instr::ArrayLen);
        copy_step_body.push(Instr::ArrayCopy {
            source_name: elem_type.clone(),
            target_name: elem_type,
        });
        // offset += outer[idx].len; idx += 1; continue
        copy_step_body.push(Instr::LocalGet {
            local_name: offset_local.clone(),
        });
        copy_step_body.extend(self.array_get_cast_instrs(operand, &idx_local, &arr_type, &inner));
        copy_step_body.extend([
            Instr::ArrayLen,
            Instr::I32Add,
            Instr::LocalSet {
                local_name: offset_local.clone(),
            },
            Instr::LocalGet {
                local_name: idx_local.clone(),
            },
            Instr::I32Const { value: 1 },
            Instr::I32Add,
            Instr::LocalSet {
                local_name: idx_local.clone(),
            },
            Instr::Br {
                label_name: copy_loop.clone(),
            },
        ]);

        self.emit_instr(Instr::Loop {
            label_name: copy_loop,
            block_type: BlockType::Empty,
            instructions: vec![
                Instr::LocalGet {
                    local_name: idx_local.clone(),
                },
                Instr::LocalGet {
                    local_name: count_local.clone(),
                },
                Instr::I32LtU,
                Instr::If {
                    label_name: copy_step,
                    block_type: BlockType::Empty,
                    then_instructions: copy_step_body,
                    else_instructions: vec![],
                },
            ],
        });
    }

    /// Map closure `f` over array `src` into a fresh array of the same length in
    /// a single allocation. One pass: size the result from `src.len`, then fill
    /// slot `idx` with `f(src[idx])` — the closure invoked inline by `call_ref`
    /// (its result is left on the stack, exactly as a non-tail closure call). The
    /// scratch buffer never escapes this helper, so the map stays a pure value at
    /// the IR level (no linearity reasoning) while lowering to a mutating fill.
    fn emit_map(
        &mut self,
        result_local: &LocalName,
        src: &'a crate::ValueName,
        f: &'a crate::ValueName,
    ) {
        let arr_type = self.context.table().arr_type();
        let arr_ref = RefType {
            is_nullable: false,
            heap_type: HeapType::Concrete(arr_type.clone()),
        };
        // `f` is a unary closure `(A) -> B`; reuse the arity-1 closure calling
        // convention (env as the self argument, the funcref in its special field).
        let envr_type = self.context.table().find_envr_type(1);
        let clsr_type = self.context.table().find_clsr_type(1);
        let special_field = self.context.table().special_field();

        let count_local = self.context.push_local("count", ValType::Num(NumType::I32));
        let idx_local = self.context.push_local("idx", ValType::Num(NumType::I32));

        let map_loop = LabelName::from(format!("{}_map_loop", result_local));
        let map_step = LabelName::from(format!("{}_map_step", result_local));

        // count = src.len
        self.emit_instrs(self.context.load_value_instrs(src, LoadAs::Arr));
        self.emit_instr(Instr::ArrayLen);
        self.emit_instr(Instr::LocalSet {
            local_name: count_local.clone(),
        });

        // result = new array sized `count` (default-filled, overwritten below)
        self.emit_instr(Instr::LocalGet {
            local_name: count_local.clone(),
        });
        self.emit_instr(Instr::ArrayNewDefault {
            type_name: arr_type.clone(),
        });
        self.emit_instr(Instr::LocalSet {
            local_name: result_local.clone(),
        });

        // idx = 0
        self.emit_instr(Instr::I32Const { value: 0 });
        self.emit_instr(Instr::LocalSet {
            local_name: idx_local.clone(),
        });

        // step: result[idx] = f(src[idx]); idx += 1; continue
        let mut step_body = vec![
            Instr::LocalGet {
                local_name: result_local.clone(),
            },
            Instr::RefCast {
                ref_type: arr_ref.clone(),
            },
            Instr::LocalGet {
                local_name: idx_local.clone(),
            },
        ];
        // value = f(src[idx]) — the closure as its own self/env argument first,
        // then the element, then the funcref pulled from the env struct.
        step_body.extend(self.context.load_value_instrs(f, LoadAs::NonNull));
        step_body.extend(self.context.load_value_instrs(src, LoadAs::Arr));
        step_body.push(Instr::LocalGet {
            local_name: idx_local.clone(),
        });
        step_body.push(Instr::ArrayGet {
            type_name: arr_type.clone(),
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
            type_name: arr_type,
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
                    local_name: count_local,
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
                // the four little-endian bytes. The `bin` array is `i8`-packed, so
                // `array.new_fixed` truncates each shifted i32 to its low byte --
                // byte-for-byte `f32::to_le_bytes`, with no host round-trip.
                let bits_local = self
                    .context
                    .push_local("flt_bits", ValType::Num(NumType::I32));
                let bin_type = self.context.table().bin_type();
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
                    type_name: bin_type,
                    length: 4,
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
            crate::Code::BinLen(bin) => self.emit_unary_op(
                &result_local,
                bin,
                LoadAs::Bin,
                Instr::ArrayLen,
                WrapAs::I31,
            ),
            crate::Code::BinEql(left, right) => {
                let bin_type = self.context.table().bin_type();

                let idx_local = self.context.push_local("idx", ValType::Num(NumType::I32));
                let result_raw_local = self.context.push_local("eql", ValType::Num(NumType::I32));

                let done_label = LabelName::from(format!("{}_done", result_local));
                let loop_label = LabelName::from(format!("{}_loop", result_local));
                let if_label = LabelName::from(format!("{}_if", result_local));

                let load_left = self.context.load_value_instrs(left, LoadAs::Bin);
                let load_right = self.context.load_value_instrs(right, LoadAs::Bin);

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
                    type_name: bin_type.clone(),
                });
                loop_instrs.extend(load_right.clone());
                loop_instrs.push(Instr::LocalGet {
                    local_name: idx_local.clone(),
                });
                loop_instrs.push(Instr::ArrayGetU {
                    type_name: bin_type,
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

                // Build the outer block: length check then loop.
                let mut block_instrs = Vec::new();

                // if left.len != right.len: exit block immediately (result stays false)
                block_instrs.extend(load_left.clone());
                block_instrs.push(Instr::ArrayLen);
                block_instrs.extend(load_right);
                block_instrs.push(Instr::ArrayLen);
                block_instrs.push(Instr::I32Ne);
                block_instrs.push(Instr::BrIf {
                    label_name: done_label.clone(),
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
                let bin_type = self.context.table().bin_type();
                self.emit_instrs(self.context.load_value_instrs(bin, LoadAs::Bin));
                self.emit_instrs(self.context.load_value_instrs(idx, LoadAs::Nat));
                self.emit_instr(Instr::ArrayGetU {
                    type_name: bin_type,
                });
                self.emit_instr(Instr::RefI31);
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            crate::Code::BinSlice(bin, start, end) => {
                let bin_type = self.context.table().bin_type();
                self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
                self.emit_instr(Instr::I32Sub);
                self.emit_instr(Instr::ArrayNewDefault {
                    type_name: bin_type.clone(),
                });
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });

                self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Bin));
                self.emit_instr(Instr::I32Const { value: 0 });
                self.emit_instrs(self.context.load_value_instrs(bin, LoadAs::Bin));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
                self.emit_instr(Instr::I32Sub);
                self.emit_instr(Instr::ArrayCopy {
                    source_name: bin_type.clone(),
                    target_name: bin_type,
                });
            }
            crate::Code::BinAppend(bin, byte) => {
                let bin_type = self.context.table().bin_type();
                self.emit_instrs(self.context.load_value_instrs(bin, LoadAs::Bin));
                self.emit_instr(Instr::ArrayLen);
                self.emit_instr(Instr::I32Const { value: 1 });
                self.emit_instr(Instr::I32Add);
                self.emit_instr(Instr::ArrayNewDefault {
                    type_name: bin_type.clone(),
                });
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });

                self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Bin));
                self.emit_instr(Instr::I32Const { value: 0 });
                self.emit_instrs(self.context.load_value_instrs(bin, LoadAs::Bin));
                self.emit_instr(Instr::I32Const { value: 0 });
                self.emit_instrs(self.context.load_value_instrs(bin, LoadAs::Bin));
                self.emit_instr(Instr::ArrayLen);
                self.emit_instr(Instr::ArrayCopy {
                    source_name: bin_type.clone(),
                    target_name: bin_type.clone(),
                });

                self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Bin));
                self.emit_instrs(self.context.load_value_instrs(bin, LoadAs::Bin));
                self.emit_instr(Instr::ArrayLen);
                self.emit_instrs(self.context.load_value_instrs(byte, LoadAs::Nat));
                self.emit_instr(Instr::ArraySet {
                    type_name: bin_type,
                });
            }
            crate::Code::BinConcat(operands) => {
                let bin_type = self.context.table().bin_type();

                match operands.as_slice() {
                    [] => {
                        self.emit_instr(Instr::ArrayNewFixed {
                            type_name: bin_type,
                            length: 0,
                        });
                        self.emit_instr(Instr::LocalSet {
                            local_name: result_local,
                        });
                    }
                    [only] => {
                        self.emit_instrs(self.context.load_value_instrs(only, LoadAs::Bin));
                        self.emit_instr(Instr::LocalSet {
                            local_name: result_local,
                        });
                    }
                    [b1, b2] => {
                        self.emit_instrs(self.context.load_value_instrs(b1, LoadAs::Bin));
                        self.emit_instr(Instr::ArrayLen);
                        self.emit_instrs(self.context.load_value_instrs(b2, LoadAs::Bin));
                        self.emit_instr(Instr::ArrayLen);
                        self.emit_instr(Instr::I32Add);
                        self.emit_instr(Instr::ArrayNewDefault {
                            type_name: bin_type.clone(),
                        });
                        self.emit_instr(Instr::LocalSet {
                            local_name: result_local.clone(),
                        });

                        self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Bin));
                        self.emit_instr(Instr::I32Const { value: 0 });
                        self.emit_instrs(self.context.load_value_instrs(b1, LoadAs::Bin));
                        self.emit_instr(Instr::I32Const { value: 0 });
                        self.emit_instrs(self.context.load_value_instrs(b1, LoadAs::Bin));
                        self.emit_instr(Instr::ArrayLen);
                        self.emit_instr(Instr::ArrayCopy {
                            source_name: bin_type.clone(),
                            target_name: bin_type.clone(),
                        });

                        self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Bin));
                        self.emit_instrs(self.context.load_value_instrs(b1, LoadAs::Bin));
                        self.emit_instr(Instr::ArrayLen);
                        self.emit_instrs(self.context.load_value_instrs(b2, LoadAs::Bin));
                        self.emit_instr(Instr::I32Const { value: 0 });
                        self.emit_instrs(self.context.load_value_instrs(b2, LoadAs::Bin));
                        self.emit_instr(Instr::ArrayLen);
                        self.emit_instr(Instr::ArrayCopy {
                            source_name: bin_type.clone(),
                            target_name: bin_type,
                        });
                    }
                    operands => {
                        self.emit_instr(Instr::I32Const { value: 0 });
                        for operand in operands.iter() {
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Bin));
                            self.emit_instr(Instr::ArrayLen);
                            self.emit_instr(Instr::I32Add);
                        }
                        self.emit_instr(Instr::ArrayNewDefault {
                            type_name: bin_type.clone(),
                        });
                        self.emit_instr(Instr::LocalSet {
                            local_name: result_local.clone(),
                        });

                        let offset_local = self
                            .context
                            .push_local("offset", ValType::Num(NumType::I32));

                        for operand in operands.iter() {
                            self.emit_instrs(
                                self.context.load_value_instrs(value_name, LoadAs::Bin),
                            );
                            self.emit_instr(Instr::LocalGet {
                                local_name: offset_local.clone(),
                            });
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Bin));
                            self.emit_instr(Instr::I32Const { value: 0 });
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Bin));
                            self.emit_instr(Instr::ArrayLen);
                            self.emit_instr(Instr::ArrayCopy {
                                source_name: bin_type.clone(),
                                target_name: bin_type.clone(),
                            });

                            self.emit_instr(Instr::LocalGet {
                                local_name: offset_local.clone(),
                            });
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Bin));
                            self.emit_instr(Instr::ArrayLen);
                            self.emit_instr(Instr::I32Add);
                            self.emit_instr(Instr::LocalSet {
                                local_name: offset_local.clone(),
                            });
                        }
                    }
                }
            }
            crate::Code::BinFlatten(operand) => {
                let bin_type = self.context.table().bin_type();
                self.emit_flatten(&result_local, value_name, operand, bin_type, LoadAs::Bin);
            }
            crate::Code::ArrLen(lst) => self.emit_unary_op(
                &result_local,
                lst,
                LoadAs::Arr,
                Instr::ArrayLen,
                WrapAs::I31,
            ),
            crate::Code::ArrGet(lst, idx) => {
                let arr_type = self.context.table().arr_type();
                self.emit_instrs(self.context.load_value_instrs(lst, LoadAs::Arr));
                self.emit_instrs(self.context.load_value_instrs(idx, LoadAs::Nat));
                self.emit_instr(Instr::ArrayGet {
                    type_name: arr_type,
                });
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            crate::Code::ArrSlice(lst, start, end) => {
                let arr_type = self.context.table().arr_type();

                self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
                self.emit_instr(Instr::I32Sub);
                self.emit_instr(Instr::ArrayNewDefault {
                    type_name: arr_type.clone(),
                });
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });

                self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Arr));
                self.emit_instr(Instr::I32Const { value: 0 });
                self.emit_instrs(self.context.load_value_instrs(lst, LoadAs::Arr));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
                self.emit_instr(Instr::I32Sub);
                self.emit_instr(Instr::ArrayCopy {
                    source_name: arr_type.clone(),
                    target_name: arr_type.clone(),
                });
            }
            crate::Code::ArrAppend(lst, elem) => {
                let arr_type = self.context.table().arr_type();
                self.emit_instrs(self.context.load_value_instrs(lst, LoadAs::Arr));
                self.emit_instr(Instr::ArrayLen);
                self.emit_instr(Instr::I32Const { value: 1 });
                self.emit_instr(Instr::I32Add);
                self.emit_instr(Instr::ArrayNewDefault {
                    type_name: arr_type.clone(),
                });
                self.emit_instr(Instr::LocalSet {
                    local_name: result_local.clone(),
                });

                self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Arr));
                self.emit_instr(Instr::I32Const { value: 0 });
                self.emit_instrs(self.context.load_value_instrs(lst, LoadAs::Arr));
                self.emit_instr(Instr::I32Const { value: 0 });
                self.emit_instrs(self.context.load_value_instrs(lst, LoadAs::Arr));
                self.emit_instr(Instr::ArrayLen);
                self.emit_instr(Instr::ArrayCopy {
                    source_name: arr_type.clone(),
                    target_name: arr_type.clone(),
                });

                self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Arr));
                self.emit_instrs(self.context.load_value_instrs(lst, LoadAs::Arr));
                self.emit_instr(Instr::ArrayLen);
                self.emit_instrs(self.context.load_value_instrs(elem, LoadAs::Null));
                self.emit_instr(Instr::ArraySet {
                    type_name: arr_type,
                });
            }
            crate::Code::ArrConcat(operands) => {
                let arr_type = self.context.table().arr_type();

                match operands.as_slice() {
                    [] => {
                        self.emit_instr(Instr::ArrayNewFixed {
                            type_name: arr_type,
                            length: 0,
                        });
                        self.emit_instr(Instr::LocalSet {
                            local_name: result_local,
                        });
                    }
                    [only] => {
                        self.emit_instrs(self.context.load_value_instrs(only, LoadAs::Arr));
                        self.emit_instr(Instr::LocalSet {
                            local_name: result_local,
                        });
                    }
                    [l1, l2] => {
                        self.emit_instrs(self.context.load_value_instrs(l1, LoadAs::Arr));
                        self.emit_instr(Instr::ArrayLen);
                        self.emit_instrs(self.context.load_value_instrs(l2, LoadAs::Arr));
                        self.emit_instr(Instr::ArrayLen);
                        self.emit_instr(Instr::I32Add);
                        self.emit_instr(Instr::ArrayNewDefault {
                            type_name: arr_type.clone(),
                        });
                        self.emit_instr(Instr::LocalSet {
                            local_name: result_local.clone(),
                        });

                        self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Arr));
                        self.emit_instr(Instr::I32Const { value: 0 });
                        self.emit_instrs(self.context.load_value_instrs(l1, LoadAs::Arr));
                        self.emit_instr(Instr::I32Const { value: 0 });
                        self.emit_instrs(self.context.load_value_instrs(l1, LoadAs::Arr));
                        self.emit_instr(Instr::ArrayLen);
                        self.emit_instr(Instr::ArrayCopy {
                            source_name: arr_type.clone(),
                            target_name: arr_type.clone(),
                        });

                        self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Arr));
                        self.emit_instrs(self.context.load_value_instrs(l1, LoadAs::Arr));
                        self.emit_instr(Instr::ArrayLen);
                        self.emit_instrs(self.context.load_value_instrs(l2, LoadAs::Arr));
                        self.emit_instr(Instr::I32Const { value: 0 });
                        self.emit_instrs(self.context.load_value_instrs(l2, LoadAs::Arr));
                        self.emit_instr(Instr::ArrayLen);
                        self.emit_instr(Instr::ArrayCopy {
                            source_name: arr_type.clone(),
                            target_name: arr_type,
                        });
                    }
                    operands => {
                        self.emit_instr(Instr::I32Const { value: 0 });
                        for operand in operands.iter() {
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Arr));
                            self.emit_instr(Instr::ArrayLen);
                            self.emit_instr(Instr::I32Add);
                        }
                        self.emit_instr(Instr::ArrayNewDefault {
                            type_name: arr_type.clone(),
                        });
                        self.emit_instr(Instr::LocalSet {
                            local_name: result_local.clone(),
                        });

                        let offset_local = self
                            .context
                            .push_local("offset", ValType::Num(NumType::I32));

                        for operand in operands.iter() {
                            self.emit_instrs(
                                self.context.load_value_instrs(value_name, LoadAs::Arr),
                            );
                            self.emit_instr(Instr::LocalGet {
                                local_name: offset_local.clone(),
                            });
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Arr));
                            self.emit_instr(Instr::I32Const { value: 0 });
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Arr));
                            self.emit_instr(Instr::ArrayLen);
                            self.emit_instr(Instr::ArrayCopy {
                                source_name: arr_type.clone(),
                                target_name: arr_type.clone(),
                            });

                            self.emit_instr(Instr::LocalGet {
                                local_name: offset_local.clone(),
                            });
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Arr));
                            self.emit_instr(Instr::ArrayLen);
                            self.emit_instr(Instr::I32Add);
                            self.emit_instr(Instr::LocalSet {
                                local_name: offset_local.clone(),
                            });
                        }
                    }
                }
            }
            crate::Code::ArrFlatten(operand) => {
                let arr_type = self.context.table().arr_type();
                self.emit_flatten(&result_local, value_name, operand, arr_type, LoadAs::Arr);
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
