use {
    super::{Context, EmissionCode, EmissionValueName, LoadAs, RopeData, Table, box_instr},
    crate::{CpsIntrinsicOp, Repr},
    curios_utilities::Grain,
};

/// Where one computed value goes: the local it is stored in, and the name the representation analysis decided about.
///
/// The two travel together because every store consults both — the local to write, and the name to know whether that local holds a register or a reference. Passing them separately is what pushed the checked-overflow helpers past seven parameters, and pairing them says more than the parameter count it saves.
struct Dest<'a> {
    value_name: &'a EmissionValueName,
    local: curios_wasm::LocalName,
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

    /// Load one operand at the representation `intrinsic` declares for that position.
    fn emit_operand(&mut self, intrinsic: &CpsIntrinsicOp, index: usize, name: &EmissionValueName) {
        let load = LoadAs::of(&intrinsic.operand_repr(index), self.context.table());
        self.emit_instrs(self.context.load_value_instrs(name, load));
    }

    /// Store a value sitting on the stack in the representation `produced` into `dest`.
    ///
    /// Nothing is coerced here. A local is held in a register only at the carrier its own definition produces — that is what `Offer`, in the representation analysis, states, and what the assert at the head of [`CodeEmitter::emit_intrinsic`] checks — so a definition and its local always agree, and every *disagreeing use* pays for itself at its own site instead.
    fn emit_store(&mut self, dest: &Dest<'_>, produced: &Repr) {
        if self.context.table().raw_carrier(dest.value_name).is_none()
            && let Some(instr) = box_instr(produced, self.context.table())
        {
            self.emit_instr(instr);
        }

        self.emit_instr(curios_wasm::Instr::LocalSet {
            local_name: dest.local.clone(),
        });
    }

    /// Lower a one-operand numeric op: load the operand, apply `instr`, store the result the way its local holds it.
    fn emit_unary_op(
        &mut self,
        dest: &Dest<'_>,
        intrinsic: &CpsIntrinsicOp,
        operand: &EmissionValueName,
        instr: curios_wasm::Instr,
    ) {
        self.emit_operand(intrinsic, 0, operand);
        self.emit_instr(instr);
        self.emit_store(dest, &intrinsic.result_repr());
    }

    /// Lower a two-operand numeric op: load each operand at its declared representation, apply `instr`, store the result the way its local holds it.
    fn emit_binary_op(
        &mut self,
        dest: &Dest<'_>,
        intrinsic: &CpsIntrinsicOp,
        left: &EmissionValueName,
        right: &EmissionValueName,
        instr: curios_wasm::Instr,
    ) {
        self.emit_operand(intrinsic, 0, left);
        self.emit_operand(intrinsic, 1, right);
        self.emit_instr(instr);
        self.emit_store(dest, &intrinsic.result_repr());
    }

    /// Lower an unsigned-`Nat` binary op that may overflow the i31 carrier: apply `op`, trap (via the special label) if bit 31 of the result is set, else store.
    ///
    /// The check stays even when the result is held in a register, which an `i32` would have been wide enough to hold unchecked. It is what maintains the invariant that *every* register-held `Nat` is inside the i31 envelope — and that invariant is what lets `box_instr` be a bare `ref.i31` at each disagreeing use, and what keeps this from changing which programs trap.
    fn emit_checked_nat_op(
        &mut self,
        dest: &Dest<'_>,
        intrinsic: &CpsIntrinsicOp,
        left: &EmissionValueName,
        right: &EmissionValueName,
        name: &str,
        op: curios_wasm::Instr,
    ) {
        // A register-held result is its own scratch: the check reads the value back out of the local it is already destined for, so the op costs no extra local and no boxing.
        let raw = self.context.table().raw_carrier(dest.value_name).is_some();
        let local_name = match raw {
            true => dest.local.clone(),
            false => self
                .context
                .push_local(name, curios_wasm::ValType::Num(curios_wasm::NumType::I32)),
        };

        self.emit_operand(intrinsic, 0, left);
        self.emit_operand(intrinsic, 1, right);
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

        if !raw {
            self.emit_instr(curios_wasm::Instr::LocalGet { local_name });
            self.emit_store(dest, &intrinsic.result_repr());
        }
    }

    /// Lower a signed-`Int` binary op that may overflow the i31 carrier: apply `op`, trap (via the special label) if the result leaves the signed 31-bit range, else store. The check stays for a register-held result for the reason given on [`CodeEmitter::emit_checked_nat_op`].
    fn emit_checked_int_op(
        &mut self,
        dest: &Dest<'_>,
        intrinsic: &CpsIntrinsicOp,
        left: &EmissionValueName,
        right: &EmissionValueName,
        name: &str,
        op: curios_wasm::Instr,
    ) {
        let raw = self.context.table().raw_carrier(dest.value_name).is_some();
        let local_name = match raw {
            true => dest.local.clone(),
            false => self
                .context
                .push_local(name, curios_wasm::ValType::Num(curios_wasm::NumType::I32)),
        };

        self.emit_operand(intrinsic, 0, left);
        self.emit_operand(intrinsic, 1, right);
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

        if !raw {
            self.emit_instr(curios_wasm::Instr::LocalGet { local_name });
            self.emit_store(dest, &intrinsic.result_repr());
        }
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
        count: &'a EmissionValueName,
        load: LoadAs,
        slice_func: curios_wasm::FuncName,
    ) {
        self.emit_instrs(self.context.load_value_instrs(carrier, load));
        self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
        self.emit_instrs(self.context.load_value_instrs(count, LoadAs::Nat));
        self.emit_instr(curios_wasm::Instr::Call {
            func_name: slice_func,
        });
        self.emit_instr(curios_wasm::Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// A suffix: the same helper as a window, with the count the *rope* decides rather than one an operand supplied.
    ///
    /// **This is the only place a compiler-emitted window's extent is derived, and it derives it from the value.** Every window the compiler emits is a suffix — `into_cont`'s peel is the sole producer — and before this each lowering computed `len - start` for itself, which is an agreement between two crates rather than a fact about the rope. A start past the end underflows the subtraction to a count no run could hold, which the slice helper's own bounds test refuses exactly as it refuses the overshoot it is written for.
    fn emit_rope_rest(
        &mut self,
        result_local: &curios_wasm::LocalName,
        carrier: &'a EmissionValueName,
        start: &'a EmissionValueName,
        load: LoadAs,
        rope: &RopeData,
        slice_func: curios_wasm::FuncName,
    ) {
        self.emit_instrs(self.context.load_value_instrs(carrier, load.clone()));
        self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
        self.emit_instrs(self.context.load_value_instrs(carrier, load));
        self.emit_instr(Self::rope_get(rope, &rope.len_field));
        self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
        self.emit_instr(curios_wasm::Instr::I32Sub);
        self.emit_instr(curios_wasm::Instr::Call {
            func_name: slice_func,
        });
        self.emit_instr(curios_wasm::Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// Lower one `EmissionCode` op into the current frame, writing its result into `value_name`'s local.
    pub(crate) fn emit(&mut self, value_name: &'a EmissionValueName, op: &'a EmissionCode) {
        let dest = Dest {
            value_name,
            local: self
                .context
                .find_local(value_name)
                .map(|ld| ld.local_name)
                .unwrap_or_else(|| panic!("`CodeEmitter` lacks local `{}`", value_name)),
        };

        match op {
            EmissionCode::Intrinsic(op, args) => self.emit_intrinsic(&dest, *op, args),
            EmissionCode::ListMap(src, f) => {
                debug_assert!(
                    self.context.table().raw_carrier(value_name).is_none(),
                    "`{value_name}` maps a list into a register, where the result is a rope",
                );
                let map = self.context.table().list_map_func();
                let envr_type = self.context.table().find_envr_type(1);
                self.emit_instrs(self.context.load_value_instrs(src, LoadAs::List));
                self.emit_instrs(
                    self.context
                        .load_value_instrs(f, LoadAs::Concrete(envr_type)),
                );
                self.emit_instr(curios_wasm::Instr::Call { func_name: map });
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: dest.local,
                });
            }
        }
    }

    /// Lower one intrinsic op into the current frame; `args` carries the operands in the order and arity the op fixes, verified at the CPS boundary.
    fn emit_intrinsic(
        &mut self,
        dest: &Dest<'a>,
        op: CpsIntrinsicOp,
        args: &'a [EmissionValueName],
    ) {
        let (value_name, result_local) = (dest.value_name, dest.local.clone());

        // Every store below may assume this: a local held in a register is held at exactly the carrier this op produces, so no path has to coerce on the way in. What makes it true is that the representation analysis only offers a register to a value whose own definition produces that carrier — see `Offer` in `cps::represent`.
        debug_assert!(
            self.context
                .table()
                .raw_carrier(value_name)
                .is_none_or(|carrier| carrier == op.result_repr()),
            "`{value_name}` is held as {:?} where {op:?} produces {:?}",
            self.context.table().raw_carrier(value_name),
            op.result_repr(),
        );

        match op {
            CpsIntrinsicOp::NatEql => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Eq)
            }
            CpsIntrinsicOp::NatNeq => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Ne)
            }
            CpsIntrinsicOp::NatAdd => self.emit_checked_nat_op(
                dest,
                &op,
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
                self.emit_store(dest, &op.result_repr());
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
                self.emit_store(dest, &op.result_repr());
            }
            // The virtual-window bounds guard, kept at the original evaluation point — the eager trap a physical slice would have performed. Its operands are a start and a *count*, so the reversed range the `(start, end)` window also had to reject cannot be spelled, and the extent is the count itself rather than a difference. `s > len || n > len - s` rather than `s + n > len`, because the sum is i32 arithmetic and would wrap; the subtraction underflows only in the case the first test has already decided.
            CpsIntrinsicOp::WindowExtent => {
                let (start, count, len) = (&args[0], &args[1], &args[2]);
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(len, LoadAs::Nat));
                self.emit_instr(curios_wasm::Instr::I32GtU);
                self.emit_instrs(self.context.load_value_instrs(count, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(len, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
                self.emit_instr(curios_wasm::Instr::I32Sub);
                self.emit_instr(curios_wasm::Instr::I32GtU);
                self.emit_instr(curios_wasm::Instr::I32Or);
                self.emit_instr(curios_wasm::Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: curios_wasm::BlockType::Empty,
                    then_instructions: vec![curios_wasm::Instr::Unreachable],
                    else_instructions: vec![],
                });
                self.emit_instrs(self.context.load_value_instrs(count, LoadAs::Nat));
                self.emit_store(dest, &op.result_repr());
            }
            CpsIntrinsicOp::NatLt => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32LtU)
            }
            CpsIntrinsicOp::NatDiv => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32DivU)
            }
            CpsIntrinsicOp::NatRem => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32RemU)
            }
            CpsIntrinsicOp::NatGt => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32GtU)
            }
            CpsIntrinsicOp::NatLe => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32LeU)
            }
            CpsIntrinsicOp::NatGe => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32GeU)
            }
            CpsIntrinsicOp::IntEql => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Eq)
            }
            CpsIntrinsicOp::IntNeq => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Ne)
            }
            CpsIntrinsicOp::IntAdd => self.emit_checked_int_op(
                dest,
                &op,
                &args[0],
                &args[1],
                "int_add",
                curios_wasm::Instr::I32Add,
            ),
            CpsIntrinsicOp::IntSub => self.emit_checked_int_op(
                dest,
                &op,
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
                self.emit_store(dest, &op.result_repr());
            }
            CpsIntrinsicOp::IntDiv => self.emit_checked_int_op(
                dest,
                &op,
                &args[0],
                &args[1],
                "int_div",
                curios_wasm::Instr::I32DivS,
            ),
            CpsIntrinsicOp::IntRem => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32RemS)
            }
            CpsIntrinsicOp::IntLt => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32LtS)
            }
            CpsIntrinsicOp::IntGt => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32GtS)
            }
            CpsIntrinsicOp::IntLe => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32LeS)
            }
            CpsIntrinsicOp::IntGe => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32GeS)
            }
            CpsIntrinsicOp::NatAnd => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32And)
            }
            CpsIntrinsicOp::NatOr => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Or)
            }
            CpsIntrinsicOp::NatXor => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Xor)
            }
            CpsIntrinsicOp::NatShl => self.emit_checked_nat_op(
                dest,
                &op,
                &args[0],
                &args[1],
                "nat_shl",
                curios_wasm::Instr::I32Shl,
            ),
            CpsIntrinsicOp::NatShr => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32ShrU)
            }
            CpsIntrinsicOp::NatRotl => self.emit_checked_nat_op(
                dest,
                &op,
                &args[0],
                &args[1],
                "nat_rotl",
                curios_wasm::Instr::I32Rotl,
            ),
            CpsIntrinsicOp::NatRotr => self.emit_checked_nat_op(
                dest,
                &op,
                &args[0],
                &args[1],
                "nat_rotr",
                curios_wasm::Instr::I32Rotr,
            ),
            CpsIntrinsicOp::NatClz => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::I32Clz)
            }
            CpsIntrinsicOp::NatCtz => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::I32Ctz)
            }
            CpsIntrinsicOp::NatPopcnt => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::I32Popcnt)
            }
            CpsIntrinsicOp::NatEqz => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::I32Eqz)
            }
            CpsIntrinsicOp::IntAnd => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32And)
            }
            CpsIntrinsicOp::IntOr => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Or)
            }
            CpsIntrinsicOp::IntXor => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Xor)
            }
            CpsIntrinsicOp::IntShl => self.emit_checked_int_op(
                dest,
                &op,
                &args[0],
                &args[1],
                "int_shl",
                curios_wasm::Instr::I32Shl,
            ),
            CpsIntrinsicOp::IntShr => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32ShrS)
            }
            CpsIntrinsicOp::IntRotl => self.emit_checked_int_op(
                dest,
                &op,
                &args[0],
                &args[1],
                "int_rotl",
                curios_wasm::Instr::I32Rotl,
            ),
            CpsIntrinsicOp::IntRotr => self.emit_checked_int_op(
                dest,
                &op,
                &args[0],
                &args[1],
                "int_rotr",
                curios_wasm::Instr::I32Rotr,
            ),
            CpsIntrinsicOp::IntClz => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::I32Clz)
            }
            CpsIntrinsicOp::IntCtz => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::I32Ctz)
            }
            CpsIntrinsicOp::IntPopcnt => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::I32Popcnt)
            }
            CpsIntrinsicOp::IntEqz => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::I32Eqz)
            }
            CpsIntrinsicOp::FltAdd => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Add)
            }
            CpsIntrinsicOp::FltSub => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Sub)
            }
            CpsIntrinsicOp::FltMul => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Mul)
            }
            CpsIntrinsicOp::FltDiv => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Div)
            }
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
                self.emit_store(dest, &op.result_repr());
            }
            CpsIntrinsicOp::FltEql => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Eq)
            }
            CpsIntrinsicOp::FltNeq => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Ne)
            }
            CpsIntrinsicOp::FltLt => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Lt)
            }
            CpsIntrinsicOp::FltGt => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Gt)
            }
            CpsIntrinsicOp::FltLe => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Le)
            }
            CpsIntrinsicOp::FltGe => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Ge)
            }
            CpsIntrinsicOp::FltMin => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Min)
            }
            CpsIntrinsicOp::FltMax => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Max)
            }
            CpsIntrinsicOp::FltNeg => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32Neg)
            }
            CpsIntrinsicOp::FltAbs => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32Abs)
            }
            CpsIntrinsicOp::FltSqrt => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32Sqrt)
            }
            CpsIntrinsicOp::FltFloor => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32Floor)
            }
            CpsIntrinsicOp::FltCeil => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32Ceil)
            }
            CpsIntrinsicOp::FltTrunc => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32Trunc)
            }
            CpsIntrinsicOp::FltNearest => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32Nearest)
            }
            CpsIntrinsicOp::FltCopysign => self.emit_binary_op(
                dest,
                &op,
                &args[0],
                &args[1],
                curios_wasm::Instr::F32Copysign,
            ),
            CpsIntrinsicOp::NatToInt => {
                let operand = &args[0];
                // The conversion preserves the number: below 2^30 the i31 bits already spell the same value, and a `Nat` at or above it has no signed-i31 `Int` holding it, so it traps at the boundary rather than silently reloading negative.
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
                self.emit_store(dest, &op.result_repr());
            }
            CpsIntrinsicOp::NatToFlt => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32ConvertI32U)
            }
            CpsIntrinsicOp::IntToNat => {
                let operand = &args[0];
                // The conversion preserves the number: a negative `Int` is a value no `Nat` holds, so it traps at the boundary rather than silently dropping the sign bit.
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
                self.emit_store(dest, &op.result_repr());
            }
            CpsIntrinsicOp::IntToFlt => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32ConvertI32S)
            }
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
                self.emit_store(dest, &op.result_repr());
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
                self.emit_store(dest, &op.result_repr());
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
                self.emit_store(dest, &op.result_repr());
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
                self.emit_store(dest, &op.result_repr());
            }
            CpsIntrinsicOp::BinLen(_) => {
                let rope = self.context.table().bin_rope();
                self.emit_unary_op(dest, &op, &args[0], Self::rope_get(&rope, &rope.len_field));
            }
            CpsIntrinsicOp::BinEql(grain) => {
                let eql = match grain {
                    Grain::B => self.context.table().bits_eql_func(),
                    Grain::X => self.context.table().bytes_eql_func(),
                };
                self.emit_instrs(self.context.load_value_instrs(&args[0], LoadAs::Bytes));
                self.emit_instrs(self.context.load_value_instrs(&args[1], LoadAs::Bytes));
                self.emit_instr(curios_wasm::Instr::Call { func_name: eql });
                self.emit_store(dest, &op.result_repr());
            }
            CpsIntrinsicOp::BinGet(grain) => {
                let read = match grain {
                    Grain::B => self.context.table().bits_read_func(),
                    Grain::X => self.context.table().bytes_read_func(),
                };
                self.emit_instrs(self.context.load_value_instrs(&args[0], LoadAs::Bytes));
                self.emit_instrs(self.context.load_value_instrs(&args[1], LoadAs::Nat));
                self.emit_instr(curios_wasm::Instr::Call { func_name: read });
                self.emit_store(dest, &op.result_repr());
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
            CpsIntrinsicOp::BinRest(grain) => {
                let slice = match grain {
                    Grain::B => self.context.table().bits_slice_func(),
                    Grain::X => self.context.table().bytes_slice_func(),
                };
                let rope = self.context.table().bin_rope();
                self.emit_rope_rest(
                    &result_local,
                    &args[0],
                    &args[1],
                    LoadAs::Bytes,
                    &rope,
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
            CpsIntrinsicOp::ListLen => {
                let rope = self.context.table().list_rope();
                self.emit_unary_op(dest, &op, &args[0], Self::rope_get(&rope, &rope.len_field));
            }
            CpsIntrinsicOp::ListGet => {
                let read = self.context.table().list_read_func();
                self.emit_instrs(self.context.load_value_instrs(&args[0], LoadAs::List));
                self.emit_instrs(self.context.load_value_instrs(&args[1], LoadAs::Nat));
                self.emit_instr(curios_wasm::Instr::Call { func_name: read });
                self.emit_store(dest, &op.result_repr());
            }
            CpsIntrinsicOp::ListSlice => {
                let slice = self.context.table().list_slice_func();
                self.emit_rope_slice(
                    &result_local,
                    &args[0],
                    &args[1],
                    &args[2],
                    LoadAs::List,
                    slice,
                );
            }
            CpsIntrinsicOp::ListRest => {
                let slice = self.context.table().list_slice_func();
                let rope = self.context.table().list_rope();
                self.emit_rope_rest(
                    &result_local,
                    &args[0],
                    &args[1],
                    LoadAs::List,
                    &rope,
                    slice,
                );
            }
            CpsIntrinsicOp::ListAppend => {
                let rope = self.context.table().list_rope();
                let elem_instrs = self.context.load_value_instrs(&args[1], LoadAs::Null);
                self.emit_rope_append(&result_local, &args[0], elem_instrs, LoadAs::List, &rope);
            }
            CpsIntrinsicOp::ListConcat(_) => {
                let rope = self.context.table().list_rope();
                self.emit_rope_concat(&result_local, args, LoadAs::List, &rope);
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
                self.emit_store(dest, &op.result_repr());
            }
            CpsIntrinsicOp::IsImmediate => {
                self.emit_instrs(self.context.load_value_instrs(&args[0], LoadAs::NonNull));
                self.emit_instr(curios_wasm::Instr::RefTest {
                    ref_type: Table::int_type(false),
                });
                self.emit_store(dest, &op.result_repr());
            }
        }
    }
}
