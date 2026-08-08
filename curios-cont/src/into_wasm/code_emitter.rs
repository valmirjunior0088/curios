use {
    super::{Context, EmissionCode, EmissionValueName, LoadAs, RopeData, Table},
    crate::CpsIntrinsicOp,
    curios_base::Grain,
};

/// How a freshly-computed numeric value is boxed: an `i31ref` for `Nat`/`Int`/`Bool` results, or the `Flt` struct for `f32` results.
enum WrapAs {
    I31,
    Flt,
}

#[derive(Debug)]
pub(crate) struct CodeEmitter<'a, 'b, 'c> {
    context: &'c mut Context<'a, 'b>,
}

impl<'a, 'b, 'c> CodeEmitter<'a, 'b, 'c> {
    pub(crate) fn new(context: &'c mut Context<'a, 'b>) -> Self {
        Self { context }
    }

    fn emit_instr(&mut self, instr: curios_wasm::Instr) {
        self.context
            .this_frame()
            .expect("`CodeEmitter` called outside a region")
            .instrs
            .push(instr);
    }

    fn emit_instrs<I>(&mut self, instrs: I)
    where
        I: IntoIterator<Item = curios_wasm::Instr>,
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
            WrapAs::I31 => curios_wasm::Instr::RefI31,
            WrapAs::Flt => curios_wasm::Instr::StructNew {
                type_name: self.context.table().flt_type(),
            },
        };

        self.emit_instr(instr);
    }

    /// Lower a one-operand numeric op: load the operand, apply `op`, box the result, store it.
    fn emit_unary_op(
        &mut self,
        result_local: &curios_wasm::LocalName,
        operand: &EmissionValueName,
        load: LoadAs,
        op: curios_wasm::Instr,
        wrap: WrapAs,
    ) {
        self.emit_instrs(self.context.load_value_instrs(operand, load));
        self.emit_instr(op);
        self.emit_wrap(wrap);
        self.emit_instr(curios_wasm::Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// Lower a two-operand numeric op: both operands share `load`, apply `op`, box, store.
    fn emit_binary_op(
        &mut self,
        result_local: &curios_wasm::LocalName,
        left: &EmissionValueName,
        right: &EmissionValueName,
        load: LoadAs,
        op: curios_wasm::Instr,
        wrap: WrapAs,
    ) {
        self.emit_instrs(self.context.load_value_instrs(left, load.clone()));
        self.emit_instrs(self.context.load_value_instrs(right, load));
        self.emit_instr(op);
        self.emit_wrap(wrap);
        self.emit_instr(curios_wasm::Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// Lower an unsigned-`Nat` binary op that may overflow the i31 carrier: apply `op`, trap (via the special label) if bit 31 of the result is set, else box with `ref.i31` and store.
    fn emit_checked_nat_op(
        &mut self,
        result_local: &curios_wasm::LocalName,
        left: &EmissionValueName,
        right: &EmissionValueName,
        name: &str,
        op: curios_wasm::Instr,
    ) {
        let local_name = self
            .context
            .push_local(name, curios_wasm::ValType::Num(curios_wasm::NumType::I32));
        self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Nat));
        self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Nat));
        self.emit_instr(op);
        self.emit_instr(curios_wasm::Instr::LocalTee {
            local_name: local_name.clone(),
        });
        self.emit_instr(curios_wasm::Instr::I32Const { value: 31 });
        self.emit_instr(curios_wasm::Instr::I32ShrU);
        self.emit_instr(curios_wasm::Instr::If {
            label_name: self.context.table().special_label(),
            block_type: curios_wasm::BlockType::Empty,
            then_instructions: vec![curios_wasm::Instr::Unreachable],
            else_instructions: vec![],
        });
        self.emit_instr(curios_wasm::Instr::LocalGet { local_name });
        self.emit_instr(curios_wasm::Instr::RefI31);
        self.emit_instr(curios_wasm::Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// Lower a signed-`Int` binary op that may overflow the i31 carrier: apply `op`, trap (via the special label) if the result leaves the signed 31-bit range, else box with `ref.i31` and store.
    fn emit_checked_int_op(
        &mut self,
        result_local: &curios_wasm::LocalName,
        left: &EmissionValueName,
        right: &EmissionValueName,
        name: &str,
        op: curios_wasm::Instr,
    ) {
        let local_name = self
            .context
            .push_local(name, curios_wasm::ValType::Num(curios_wasm::NumType::I32));
        self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
        self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
        self.emit_instr(op);
        self.emit_instr(curios_wasm::Instr::LocalTee {
            local_name: local_name.clone(),
        });
        self.emit_instr(curios_wasm::Instr::I32Const { value: 1 });
        self.emit_instr(curios_wasm::Instr::I32Shl);
        self.emit_instr(curios_wasm::Instr::LocalGet {
            local_name: local_name.clone(),
        });
        self.emit_instr(curios_wasm::Instr::I32Xor);
        self.emit_instr(curios_wasm::Instr::I32Const { value: 31 });
        self.emit_instr(curios_wasm::Instr::I32ShrU);
        self.emit_instr(curios_wasm::Instr::If {
            label_name: self.context.table().special_label(),
            block_type: curios_wasm::BlockType::Empty,
            then_instructions: vec![curios_wasm::Instr::Unreachable],
            else_instructions: vec![],
        });
        self.emit_instr(curios_wasm::Instr::LocalGet { local_name });
        self.emit_instr(curios_wasm::Instr::RefI31);
        self.emit_instr(curios_wasm::Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// `struct.get` on a rope base — the `len`/`tag` reads that never force.
    fn rope_get(rope: &RopeData, field: &curios_wasm::FieldName) -> curios_wasm::Instr {
        curios_wasm::Instr::StructGet {
            type_name: rope.base.clone(),
            field_name: field.clone(),
        }
    }

    /// Concatenate two loaded ropes into `dest`: answer the other side when one is empty (the runtime identity shortcuts keep chains of empty seeds from deepening), else one O(1) node.
    fn concat_pair_instrs(
        &self,
        lhs: Vec<curios_wasm::Instr>,
        rhs: Vec<curios_wasm::Instr>,
        dest: &curios_wasm::LocalName,
        rope: &RopeData,
    ) -> Vec<curios_wasm::Instr> {
        let set_dest = curios_wasm::Instr::LocalSet {
            local_name: dest.clone(),
        };

        let mut node = vec![curios_wasm::Instr::I32Const { value: 1 }];
        node.extend(lhs.clone());
        node.push(Self::rope_get(rope, &rope.len_field));
        node.extend(rhs.clone());
        node.push(Self::rope_get(rope, &rope.len_field));
        node.push(curios_wasm::Instr::I32Add);
        node.extend(lhs.clone());
        node.extend(rhs.clone());
        node.push(curios_wasm::Instr::RefNull {
            heap_type: curios_wasm::HeapType::Concrete(rope.payload.clone()),
        });
        node.push(curios_wasm::Instr::StructNew {
            type_name: rope.node.clone(),
        });
        node.push(set_dest.clone());

        let mut rhs_empty_check = rhs.clone();
        rhs_empty_check.push(Self::rope_get(rope, &rope.len_field));
        rhs_empty_check.push(curios_wasm::Instr::I32Eqz);
        rhs_empty_check.push(curios_wasm::Instr::If {
            label_name: self.context.table().special_label(),
            block_type: curios_wasm::BlockType::Empty,
            then_instructions: {
                let mut then = lhs.clone();
                then.push(set_dest.clone());
                then
            },
            else_instructions: node,
        });

        let mut instrs = lhs;
        instrs.push(Self::rope_get(rope, &rope.len_field));
        instrs.push(curios_wasm::Instr::I32Eqz);
        instrs.push(curios_wasm::Instr::If {
            label_name: self.context.table().special_label(),
            block_type: curios_wasm::BlockType::Empty,
            then_instructions: {
                let mut then = rhs;
                then.push(set_dest);
                then
            },
            else_instructions: rhs_empty_check,
        });

        instrs
    }

    /// Lower an n-ary rope concat: the empty case is an empty leaf, a single operand is an alias, and longer runs fold pairs left-leaning through `result_local` — n−1 nodes, no copying.
    fn emit_rope_concat(
        &mut self,
        result_local: &curios_wasm::LocalName,
        operands: &'a [EmissionValueName],
        load: LoadAs,
        rope: &RopeData,
    ) {
        match operands {
            [] => {
                self.emit_instr(curios_wasm::Instr::I32Const { value: 0 });
                self.emit_instr(curios_wasm::Instr::I32Const { value: 0 });
                self.emit_instr(curios_wasm::Instr::I32Const { value: 0 });
                self.emit_instr(curios_wasm::Instr::ArrayNewDefault {
                    type_name: rope.payload.clone(),
                });
                self.emit_instr(curios_wasm::Instr::StructNew {
                    type_name: rope.leaf.clone(),
                });
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            [only] => {
                self.emit_instrs(self.context.load_value_instrs(only, load));
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            [first, rest @ ..] => {
                let mut lhs = self.context.load_value_instrs(first, load.clone());

                for operand in rest {
                    let rhs = self.context.load_value_instrs(operand, load.clone());
                    let pair = self.concat_pair_instrs(lhs, rhs, result_local, rope);
                    self.emit_instrs(pair);
                    // Later pairs read the settled accumulator back out of the result local.
                    lhs = vec![
                        curios_wasm::Instr::LocalGet {
                            local_name: result_local.clone(),
                        },
                        curios_wasm::Instr::RefCast {
                            ref_type: curios_wasm::RefType {
                                is_nullable: false,
                                heap_type: curios_wasm::HeapType::Concrete(rope.base.clone()),
                            },
                        },
                    ];
                }
            }
        }
    }

    /// Lower a rope append: a fresh one-element leaf on the right of one node. The per-element builders (`Json` escaping, UTF-8 emit) are O(1)/step.
    fn emit_rope_append(
        &mut self,
        result_local: &curios_wasm::LocalName,
        carrier: &'a EmissionValueName,
        elem_instrs: Vec<curios_wasm::Instr>,
        load: LoadAs,
        rope: &RopeData,
    ) {
        self.emit_instr(curios_wasm::Instr::I32Const { value: 1 });
        self.emit_instrs(self.context.load_value_instrs(carrier, load.clone()));
        self.emit_instr(Self::rope_get(rope, &rope.len_field));
        self.emit_instr(curios_wasm::Instr::I32Const { value: 1 });
        self.emit_instr(curios_wasm::Instr::I32Add);
        self.emit_instrs(self.context.load_value_instrs(carrier, load));
        self.emit_instr(curios_wasm::Instr::I32Const { value: 0 });
        self.emit_instr(curios_wasm::Instr::I32Const { value: 1 });
        self.emit_instrs(elem_instrs);
        self.emit_instr(curios_wasm::Instr::ArrayNewFixed {
            type_name: rope.payload.clone(),
            length: 1,
        });
        self.emit_instr(curios_wasm::Instr::StructNew {
            type_name: rope.leaf.clone(),
        });
        self.emit_instr(curios_wasm::Instr::RefNull {
            heap_type: curios_wasm::HeapType::Concrete(rope.payload.clone()),
        });
        self.emit_instr(curios_wasm::Instr::StructNew {
            type_name: rope.node.clone(),
        });
        self.emit_instr(curios_wasm::Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// Lower a rope slice: one call to the shared `slice` helper — an O(1) window (`view`) over the source, with the bounds trap and the read-through invariant maintained inside the helper.
    fn emit_rope_slice(
        &mut self,
        result_local: &curios_wasm::LocalName,
        carrier: &'a EmissionValueName,
        start: &'a EmissionValueName,
        end: &'a EmissionValueName,
        load: LoadAs,
        slice_func: curios_wasm::FuncName,
    ) {
        self.emit_instrs(self.context.load_value_instrs(carrier, load));
        self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
        self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Nat));
        self.emit_instr(curios_wasm::Instr::Call {
            func_name: slice_func,
        });
        self.emit_instr(curios_wasm::Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// Lower one `EmissionCode` op into the current frame, writing its result into `value_name`'s local.
    pub(crate) fn emit(&mut self, value_name: &'a EmissionValueName, op: &'a EmissionCode) {
        let result_local = self
            .context
            .find_local(value_name)
            .map(|ld| ld.local_name)
            .unwrap_or_else(|| panic!("`CodeEmitter` lacks local `{}`", value_name));

        match op {
            EmissionCode::Intrinsic(op, args) => self.emit_intrinsic(result_local, *op, args),
            EmissionCode::LstMap(src, f) => {
                let map = self.context.table().lst_map_func();
                let envr_type = self.context.table().find_envr_type(1);
                self.emit_instrs(self.context.load_value_instrs(src, LoadAs::Lst));
                self.emit_instrs(
                    self.context
                        .load_value_instrs(f, LoadAs::Concrete(envr_type)),
                );
                self.emit_instr(curios_wasm::Instr::Call { func_name: map });
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local,
                });
            }
        }
    }

    /// Lower one intrinsic op into the current frame; `args` carries the operands in the order and arity the op fixes, verified at the CPS boundary.
    fn emit_intrinsic(
        &mut self,
        result_local: curios_wasm::LocalName,
        op: CpsIntrinsicOp,
        args: &'a [EmissionValueName],
    ) {
        match op {
            CpsIntrinsicOp::NatEql => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Nat,
                curios_wasm::Instr::I32Eq,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::NatNeq => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Nat,
                curios_wasm::Instr::I32Ne,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::NatAdd => self.emit_checked_nat_op(
                &result_local,
                &args[0],
                &args[1],
                "nat_add",
                curios_wasm::Instr::I32Add,
            ),
            CpsIntrinsicOp::NatSub => {
                let (left, right) = (&args[0], &args[1]);
                // Monus: 0 if left < right, else left - right. select [val1=0, val2=left-right, cond=left<right] returns val1 when cond != 0.
                self.emit_instr(curios_wasm::Instr::I32Const { value: 0 });
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Nat));
                self.emit_instr(curios_wasm::Instr::I32Sub);
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Nat));
                self.emit_instr(curios_wasm::Instr::I32LtU);
                self.emit_instr(curios_wasm::Instr::Select { val_types: vec![] });
                self.emit_instr(curios_wasm::Instr::RefI31);
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsicOp::NatMul => {
                let (left, right) = (&args[0], &args[1]);
                let local_name = self.context.push_local(
                    "nat_mul",
                    curios_wasm::ValType::Num(curios_wasm::NumType::I64),
                );
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Nat));
                self.emit_instr(curios_wasm::Instr::I64ExtendI32U);
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Nat));
                self.emit_instr(curios_wasm::Instr::I64ExtendI32U);
                self.emit_instr(curios_wasm::Instr::I64Mul);
                self.emit_instr(curios_wasm::Instr::LocalTee {
                    local_name: local_name.clone(),
                });
                self.emit_instr(curios_wasm::Instr::I64Const { value: 31 });
                self.emit_instr(curios_wasm::Instr::I64ShrU);
                self.emit_instr(curios_wasm::Instr::I32WrapI64);
                self.emit_instr(curios_wasm::Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: curios_wasm::BlockType::Empty,
                    then_instructions: vec![curios_wasm::Instr::Unreachable],
                    else_instructions: vec![],
                });
                self.emit_instr(curios_wasm::Instr::LocalGet { local_name });
                self.emit_instr(curios_wasm::Instr::I32WrapI64);
                self.emit_instr(curios_wasm::Instr::RefI31);
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsicOp::NatLt => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Nat,
                curios_wasm::Instr::I32LtU,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::NatDiv => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Nat,
                curios_wasm::Instr::I32DivU,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::NatRem => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Nat,
                curios_wasm::Instr::I32RemU,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::NatGt => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Nat,
                curios_wasm::Instr::I32GtU,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::NatLte => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Nat,
                curios_wasm::Instr::I32LeU,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::NatGte => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Nat,
                curios_wasm::Instr::I32GeU,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::IntEql => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Int,
                curios_wasm::Instr::I32Eq,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::IntNeq => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Int,
                curios_wasm::Instr::I32Ne,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::IntAdd => self.emit_checked_int_op(
                &result_local,
                &args[0],
                &args[1],
                "int_add",
                curios_wasm::Instr::I32Add,
            ),
            CpsIntrinsicOp::IntSub => self.emit_checked_int_op(
                &result_local,
                &args[0],
                &args[1],
                "int_sub",
                curios_wasm::Instr::I32Sub,
            ),
            CpsIntrinsicOp::IntMul => {
                let (left, right) = (&args[0], &args[1]);
                let local_name = self.context.push_local(
                    "int_mul",
                    curios_wasm::ValType::Num(curios_wasm::NumType::I64),
                );
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instr(curios_wasm::Instr::I64ExtendI32S);
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(curios_wasm::Instr::I64ExtendI32S);
                self.emit_instr(curios_wasm::Instr::I64Mul);
                self.emit_instr(curios_wasm::Instr::LocalTee {
                    local_name: local_name.clone(),
                });
                self.emit_instr(curios_wasm::Instr::I64Const { value: 30 });
                self.emit_instr(curios_wasm::Instr::I64ShrS);
                self.emit_instr(curios_wasm::Instr::LocalGet {
                    local_name: local_name.clone(),
                });
                self.emit_instr(curios_wasm::Instr::I64Const { value: 63 });
                self.emit_instr(curios_wasm::Instr::I64ShrS);
                self.emit_instr(curios_wasm::Instr::I64Ne);
                self.emit_instr(curios_wasm::Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: curios_wasm::BlockType::Empty,
                    then_instructions: vec![curios_wasm::Instr::Unreachable],
                    else_instructions: vec![],
                });
                self.emit_instr(curios_wasm::Instr::LocalGet { local_name });
                self.emit_instr(curios_wasm::Instr::I32WrapI64);
                self.emit_instr(curios_wasm::Instr::RefI31);
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsicOp::IntDiv => self.emit_checked_int_op(
                &result_local,
                &args[0],
                &args[1],
                "int_div",
                curios_wasm::Instr::I32DivS,
            ),
            CpsIntrinsicOp::IntRem => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Int,
                curios_wasm::Instr::I32RemS,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::IntLt => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Int,
                curios_wasm::Instr::I32LtS,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::IntGt => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Int,
                curios_wasm::Instr::I32GtS,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::IntLte => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Int,
                curios_wasm::Instr::I32LeS,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::IntGte => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Int,
                curios_wasm::Instr::I32GeS,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::NatAnd => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Nat,
                curios_wasm::Instr::I32And,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::NatOr => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Nat,
                curios_wasm::Instr::I32Or,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::NatXor => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Nat,
                curios_wasm::Instr::I32Xor,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::NatShl => self.emit_checked_nat_op(
                &result_local,
                &args[0],
                &args[1],
                "nat_shl",
                curios_wasm::Instr::I32Shl,
            ),
            CpsIntrinsicOp::NatShr => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Nat,
                curios_wasm::Instr::I32ShrU,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::NatRotl => self.emit_checked_nat_op(
                &result_local,
                &args[0],
                &args[1],
                "nat_rotl",
                curios_wasm::Instr::I32Rotl,
            ),
            CpsIntrinsicOp::NatRotr => self.emit_checked_nat_op(
                &result_local,
                &args[0],
                &args[1],
                "nat_rotr",
                curios_wasm::Instr::I32Rotr,
            ),
            CpsIntrinsicOp::NatClz => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Nat,
                curios_wasm::Instr::I32Clz,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::NatCtz => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Nat,
                curios_wasm::Instr::I32Ctz,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::NatPopcnt => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Nat,
                curios_wasm::Instr::I32Popcnt,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::NatEqz => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Nat,
                curios_wasm::Instr::I32Eqz,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::IntAnd => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Int,
                curios_wasm::Instr::I32And,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::IntOr => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Int,
                curios_wasm::Instr::I32Or,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::IntXor => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Int,
                curios_wasm::Instr::I32Xor,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::IntShl => self.emit_checked_int_op(
                &result_local,
                &args[0],
                &args[1],
                "int_shl",
                curios_wasm::Instr::I32Shl,
            ),
            CpsIntrinsicOp::IntShr => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Int,
                curios_wasm::Instr::I32ShrS,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::IntRotl => self.emit_checked_int_op(
                &result_local,
                &args[0],
                &args[1],
                "int_rotl",
                curios_wasm::Instr::I32Rotl,
            ),
            CpsIntrinsicOp::IntRotr => self.emit_checked_int_op(
                &result_local,
                &args[0],
                &args[1],
                "int_rotr",
                curios_wasm::Instr::I32Rotr,
            ),
            CpsIntrinsicOp::IntClz => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Int,
                curios_wasm::Instr::I32Clz,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::IntCtz => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Int,
                curios_wasm::Instr::I32Ctz,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::IntPopcnt => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Int,
                curios_wasm::Instr::I32Popcnt,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::IntEqz => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Int,
                curios_wasm::Instr::I32Eqz,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::FltAdd => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Flt,
                curios_wasm::Instr::F32Add,
                WrapAs::Flt,
            ),
            CpsIntrinsicOp::FltSub => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Flt,
                curios_wasm::Instr::F32Sub,
                WrapAs::Flt,
            ),
            CpsIntrinsicOp::FltMul => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Flt,
                curios_wasm::Instr::F32Mul,
                WrapAs::Flt,
            ),
            CpsIntrinsicOp::FltDiv => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Flt,
                curios_wasm::Instr::F32Div,
                WrapAs::Flt,
            ),
            // WebAssembly has no `f32.rem`, so expand the C `fmod` definition `x - trunc(x / y) * y` inline (`x`/`y` are locals, loaded twice).
            CpsIntrinsicOp::FltRem => {
                let (left, right) = (&args[0], &args[1]);
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(curios_wasm::Instr::F32Div);
                self.emit_instr(curios_wasm::Instr::F32Trunc);
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(curios_wasm::Instr::F32Mul);
                self.emit_instr(curios_wasm::Instr::F32Sub);
                self.emit_wrap(WrapAs::Flt);
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsicOp::FltEql => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Flt,
                curios_wasm::Instr::F32Eq,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::FltNeq => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Flt,
                curios_wasm::Instr::F32Ne,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::FltLt => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Flt,
                curios_wasm::Instr::F32Lt,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::FltGt => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Flt,
                curios_wasm::Instr::F32Gt,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::FltLte => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Flt,
                curios_wasm::Instr::F32Le,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::FltGte => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Flt,
                curios_wasm::Instr::F32Ge,
                WrapAs::I31,
            ),
            CpsIntrinsicOp::FltMin => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Flt,
                curios_wasm::Instr::F32Min,
                WrapAs::Flt,
            ),
            CpsIntrinsicOp::FltMax => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Flt,
                curios_wasm::Instr::F32Max,
                WrapAs::Flt,
            ),
            CpsIntrinsicOp::FltNeg => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Flt,
                curios_wasm::Instr::F32Neg,
                WrapAs::Flt,
            ),
            CpsIntrinsicOp::FltAbs => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Flt,
                curios_wasm::Instr::F32Abs,
                WrapAs::Flt,
            ),
            CpsIntrinsicOp::FltSqrt => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Flt,
                curios_wasm::Instr::F32Sqrt,
                WrapAs::Flt,
            ),
            CpsIntrinsicOp::FltFloor => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Flt,
                curios_wasm::Instr::F32Floor,
                WrapAs::Flt,
            ),
            CpsIntrinsicOp::FltCeil => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Flt,
                curios_wasm::Instr::F32Ceil,
                WrapAs::Flt,
            ),
            CpsIntrinsicOp::FltTrunc => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Flt,
                curios_wasm::Instr::F32Trunc,
                WrapAs::Flt,
            ),
            CpsIntrinsicOp::FltNearest => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Flt,
                curios_wasm::Instr::F32Nearest,
                WrapAs::Flt,
            ),
            CpsIntrinsicOp::FltCopysign => self.emit_binary_op(
                &result_local,
                &args[0],
                &args[1],
                LoadAs::Flt,
                curios_wasm::Instr::F32Copysign,
                WrapAs::Flt,
            ),
            CpsIntrinsicOp::NatToInt => {
                let operand = &args[0];
                // The conversion is a carrier-bit reinterpretation; a `Nat` at or above 2^30 has no signed-i31 representation, so it traps at the boundary rather than silently reloading negative.
                let local_name = self.context.push_local(
                    "nat_to_int",
                    curios_wasm::ValType::Num(curios_wasm::NumType::I32),
                );
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Nat));
                self.emit_instr(curios_wasm::Instr::LocalTee {
                    local_name: local_name.clone(),
                });
                self.emit_instr(curios_wasm::Instr::I32Const { value: 30 });
                self.emit_instr(curios_wasm::Instr::I32ShrU);
                self.emit_instr(curios_wasm::Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: curios_wasm::BlockType::Empty,
                    then_instructions: vec![curios_wasm::Instr::Unreachable],
                    else_instructions: vec![],
                });
                self.emit_instr(curios_wasm::Instr::LocalGet { local_name });
                self.emit_instr(curios_wasm::Instr::RefI31);
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsicOp::NatToFlt => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Nat,
                curios_wasm::Instr::F32ConvertI32U,
                WrapAs::Flt,
            ),
            CpsIntrinsicOp::IntToNat => {
                let operand = &args[0];
                // The conversion is a carrier-bit reinterpretation; a negative `Int` maps to a `Nat` at or above 2^31, which has no i31 representation, so it traps at the boundary rather than silently dropping the sign bit.
                let local_name = self.context.push_local(
                    "int_to_nat",
                    curios_wasm::ValType::Num(curios_wasm::NumType::I32),
                );
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(curios_wasm::Instr::LocalTee {
                    local_name: local_name.clone(),
                });
                self.emit_instr(curios_wasm::Instr::I32Const { value: 31 });
                self.emit_instr(curios_wasm::Instr::I32ShrU);
                self.emit_instr(curios_wasm::Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: curios_wasm::BlockType::Empty,
                    then_instructions: vec![curios_wasm::Instr::Unreachable],
                    else_instructions: vec![],
                });
                self.emit_instr(curios_wasm::Instr::LocalGet { local_name });
                self.emit_instr(curios_wasm::Instr::RefI31);
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsicOp::IntToFlt => self.emit_unary_op(
                &result_local,
                &args[0],
                LoadAs::Int,
                curios_wasm::Instr::F32ConvertI32S,
                WrapAs::Flt,
            ),
            CpsIntrinsicOp::FltToLeBytes => {
                let operand = &args[0];
                // Reinterpret the f32 as its IEEE-754 bit pattern and split it into the four little-endian bytes. The `$bytes` payload is `i8`-packed, so `array.new_fixed` truncates each shifted i32 to its low byte -- byte-for-byte `f32::to_le_bytes`, with no host round-trip.
                let bits_local = self.context.push_local(
                    "flt_bits",
                    curios_wasm::ValType::Num(curios_wasm::NumType::I32),
                );
                let rope = self.context.table().bin_rope();
                self.emit_instr(curios_wasm::Instr::I32Const { value: 0 });
                self.emit_instr(curios_wasm::Instr::I32Const { value: 4 });
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(curios_wasm::Instr::I32ReinterpretF32);
                self.emit_instr(curios_wasm::Instr::LocalTee {
                    local_name: bits_local.clone(),
                });
                for shift in [8, 16, 24] {
                    self.emit_instr(curios_wasm::Instr::LocalGet {
                        local_name: bits_local.clone(),
                    });
                    self.emit_instr(curios_wasm::Instr::I32Const { value: shift });
                    self.emit_instr(curios_wasm::Instr::I32ShrU);
                }
                self.emit_instr(curios_wasm::Instr::ArrayNewFixed {
                    type_name: rope.payload,
                    length: 4,
                });
                self.emit_instr(curios_wasm::Instr::StructNew {
                    type_name: rope.leaf,
                });
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsicOp::FltOfLeBytes => {
                let operand = &args[0];
                // The inverse of `FltToLeBytes`: trap (via the special label) unless the `Bin` is exactly 4 bytes, then OR the bytes back into an i32 -- each `$bytes/read` zero-extends its packed byte -- and reinterpret. Byte-for-byte `f32::from_le_bytes`, no host round-trip.
                let rope = self.context.table().bin_rope();
                let read = self.context.table().bytes_read_func();
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Bytes));
                self.emit_instr(Self::rope_get(&rope, &rope.len_field));
                self.emit_instr(curios_wasm::Instr::I32Const { value: 4 });
                self.emit_instr(curios_wasm::Instr::I32Ne);
                self.emit_instr(curios_wasm::Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: curios_wasm::BlockType::Empty,
                    then_instructions: vec![curios_wasm::Instr::Unreachable],
                    else_instructions: vec![],
                });
                for shift in [0, 8, 16, 24] {
                    self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Bytes));
                    self.emit_instr(curios_wasm::Instr::I32Const { value: shift / 8 });
                    self.emit_instr(curios_wasm::Instr::Call {
                        func_name: read.clone(),
                    });
                    if shift != 0 {
                        self.emit_instr(curios_wasm::Instr::I32Const { value: shift });
                        self.emit_instr(curios_wasm::Instr::I32Shl);
                        self.emit_instr(curios_wasm::Instr::I32Or);
                    }
                }
                self.emit_instr(curios_wasm::Instr::F32ReinterpretI32);
                self.emit_wrap(WrapAs::Flt);
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsicOp::FltToNat => {
                let local_name = self.context.push_local(
                    "flt_to_nat",
                    curios_wasm::ValType::Num(curios_wasm::NumType::I32),
                );
                self.emit_instrs(self.context.load_value_instrs(&args[0], LoadAs::Flt));
                self.emit_instr(curios_wasm::Instr::I32TruncF32U);
                self.emit_instr(curios_wasm::Instr::LocalTee {
                    local_name: local_name.clone(),
                });
                self.emit_instr(curios_wasm::Instr::I32Const { value: 31 });
                self.emit_instr(curios_wasm::Instr::I32ShrU);
                self.emit_instr(curios_wasm::Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: curios_wasm::BlockType::Empty,
                    then_instructions: vec![curios_wasm::Instr::Unreachable],
                    else_instructions: vec![],
                });
                self.emit_instr(curios_wasm::Instr::LocalGet { local_name });
                self.emit_instr(curios_wasm::Instr::RefI31);
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsicOp::FltToInt => {
                let local_name = self.context.push_local(
                    "flt_to_int",
                    curios_wasm::ValType::Num(curios_wasm::NumType::I32),
                );
                self.emit_instrs(self.context.load_value_instrs(&args[0], LoadAs::Flt));
                self.emit_instr(curios_wasm::Instr::I32TruncF32S);
                self.emit_instr(curios_wasm::Instr::LocalTee {
                    local_name: local_name.clone(),
                });
                self.emit_instr(curios_wasm::Instr::I32Const { value: 1 });
                self.emit_instr(curios_wasm::Instr::I32Shl);
                self.emit_instr(curios_wasm::Instr::LocalGet {
                    local_name: local_name.clone(),
                });
                self.emit_instr(curios_wasm::Instr::I32Xor);
                self.emit_instr(curios_wasm::Instr::I32Const { value: 31 });
                self.emit_instr(curios_wasm::Instr::I32ShrU);
                self.emit_instr(curios_wasm::Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: curios_wasm::BlockType::Empty,
                    then_instructions: vec![curios_wasm::Instr::Unreachable],
                    else_instructions: vec![],
                });
                self.emit_instr(curios_wasm::Instr::LocalGet { local_name });
                self.emit_instr(curios_wasm::Instr::RefI31);
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsicOp::BinLen(_) => {
                let rope = self.context.table().bin_rope();
                self.emit_unary_op(
                    &result_local,
                    &args[0],
                    LoadAs::Bytes,
                    Self::rope_get(&rope, &rope.len_field),
                    WrapAs::I31,
                );
            }
            CpsIntrinsicOp::BinEql(grain) => {
                let eql = match grain {
                    Grain::B => self.context.table().bits_eql_func(),
                    Grain::X => self.context.table().bytes_eql_func(),
                };
                self.emit_instrs(self.context.load_value_instrs(&args[0], LoadAs::Bytes));
                self.emit_instrs(self.context.load_value_instrs(&args[1], LoadAs::Bytes));
                self.emit_instr(curios_wasm::Instr::Call { func_name: eql });
                self.emit_instr(curios_wasm::Instr::RefI31);
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsicOp::BinGet(grain) => {
                let read = match grain {
                    Grain::B => self.context.table().bits_read_func(),
                    Grain::X => self.context.table().bytes_read_func(),
                };
                self.emit_instrs(self.context.load_value_instrs(&args[0], LoadAs::Bytes));
                self.emit_instrs(self.context.load_value_instrs(&args[1], LoadAs::Nat));
                self.emit_instr(curios_wasm::Instr::Call { func_name: read });
                self.emit_instr(curios_wasm::Instr::RefI31);
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsicOp::BinSlice(grain) => {
                let slice = match grain {
                    Grain::B => self.context.table().bits_slice_func(),
                    Grain::X => self.context.table().bytes_slice_func(),
                };
                self.emit_rope_slice(
                    &result_local,
                    &args[0],
                    &args[1],
                    &args[2],
                    LoadAs::Bytes,
                    slice,
                );
            }
            CpsIntrinsicOp::BinAppend(_) => {
                let rope = self.context.table().bin_rope();
                let elem_instrs = self.context.load_value_instrs(&args[1], LoadAs::Nat);
                self.emit_rope_append(&result_local, &args[0], elem_instrs, LoadAs::Bytes, &rope);
            }
            CpsIntrinsicOp::BinConcat(_, _) => {
                let rope = self.context.table().bin_rope();
                self.emit_rope_concat(&result_local, args, LoadAs::Bytes, &rope);
            }
            CpsIntrinsicOp::LstLen => {
                let rope = self.context.table().lst_rope();
                self.emit_unary_op(
                    &result_local,
                    &args[0],
                    LoadAs::Lst,
                    Self::rope_get(&rope, &rope.len_field),
                    WrapAs::I31,
                );
            }
            CpsIntrinsicOp::LstGet => {
                let read = self.context.table().lst_read_func();
                self.emit_instrs(self.context.load_value_instrs(&args[0], LoadAs::Lst));
                self.emit_instrs(self.context.load_value_instrs(&args[1], LoadAs::Nat));
                self.emit_instr(curios_wasm::Instr::Call { func_name: read });
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsicOp::LstSlice => {
                let slice = self.context.table().lst_slice_func();
                self.emit_rope_slice(
                    &result_local,
                    &args[0],
                    &args[1],
                    &args[2],
                    LoadAs::Lst,
                    slice,
                );
            }
            CpsIntrinsicOp::LstAppend => {
                let rope = self.context.table().lst_rope();
                let elem_instrs = self.context.load_value_instrs(&args[1], LoadAs::Null);
                self.emit_rope_append(&result_local, &args[0], elem_instrs, LoadAs::Lst, &rope);
            }
            CpsIntrinsicOp::LstConcat(_) => {
                let rope = self.context.table().lst_rope();
                self.emit_rope_concat(&result_local, args, LoadAs::Lst, &rope);
            }
            CpsIntrinsicOp::TplGet(index) => {
                let tpl_n_type = self.context.table().find_tpl_type(index + 1);
                let field_name = Table::tpl_field(index);

                self.emit_instrs(
                    self.context
                        .load_value_instrs(&args[0], LoadAs::Concrete(tpl_n_type.clone())),
                );

                self.emit_instr(curios_wasm::Instr::StructGet {
                    type_name: tpl_n_type,
                    field_name,
                });
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local,
                });
            }
        }
    }
}
