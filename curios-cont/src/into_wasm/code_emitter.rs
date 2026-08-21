use {
    super::{Context, EmissionCode, EmissionValueName, LoadAs, RopeData, Table, box_instr},
    crate::{CpsIntrinsic, CpsSlot, Repr},
    curios_utilities::Grain,
};

/// Where one computed value goes: the local it is stored in, and the name the representation analysis decided about.
///
/// The two travel together because every store consults both — the local to write, and the name to know whether that local holds a register or a reference. Passing them separately is what pushed the checked-overflow helpers past seven parameters, and pairing them says more than the parameter count it saves.
struct Dest<'a> {
    value_name: &'a EmissionValueName,
    local: curios_wasm::LocalName,
}

/// A sequence window lowering's helpers: the shared `slice`, and the normalisation its result owes — `Some` exactly for the byte grain, whose small values ride the i31 and whose every producer therefore answers through `$bytes/norm`.
struct WindowFuncs {
    slice: curios_wasm::FuncName,
    norm: Option<curios_wasm::FuncName>,
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
    fn emit_operand(&mut self, intrinsic: &CpsIntrinsic, index: usize, name: &EmissionValueName) {
        let load = LoadAs::of(&intrinsic.operand_repr(index));
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
        intrinsic: &CpsIntrinsic,
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
        intrinsic: &CpsIntrinsic,
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
        intrinsic: &CpsIntrinsic,
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
        intrinsic: &CpsIntrinsic,
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

    /// Push the shift count for a shift lowering, clamped to 31.
    ///
    /// **The clamp is what makes one test decide every count.** Wasm's shifts reduce their count modulo the operand width — `i32.shl` by 32, `i64.shl` by 64 — so a count of 40 becomes a count of 8 and the result is a value the program never asked for. Clamping instead of masking is sound because 31 is already past the envelope: any nonzero value shifted 31 places leaves it, so the check below reaches the same verdict for 31 as for any larger count, and zero shifted anywhere is still zero.
    fn emit_clamped_shift(
        &mut self,
        intrinsic: &CpsIntrinsic,
        count: &EmissionValueName,
        name: &str,
    ) {
        let count_local = self
            .context
            .push_local(name, curios_wasm::ValType::Num(curios_wasm::NumType::I32));

        self.emit_operand(intrinsic, 1, count);
        self.emit_instr(curios_wasm::Instr::LocalTee {
            local_name: count_local.clone(),
        });
        self.emit_instr(curios_wasm::Instr::I32Const { value: 31 });
        self.emit_instr(curios_wasm::Instr::LocalGet {
            local_name: count_local,
        });
        self.emit_instr(curios_wasm::Instr::I32Const { value: 31 });
        self.emit_instr(curios_wasm::Instr::I32LtU);
        self.emit_instr(curios_wasm::Instr::Select {
            val_types: vec![curios_wasm::ValType::Num(curios_wasm::NumType::I32)],
        });
    }

    /// Lower a left shift, trapping when the shifted value leaves the i31 envelope.
    ///
    /// **Widened for the reason `NatMul` is widened, and it is the same defect underneath.** Shifting in `i32` and testing the result's bit 31 afterwards cannot see bits the shift already discarded: `2³⁰ << 15` is `2⁴⁵`, truncates to zero, and reads as a perfectly good result. Sixty-four bits hold every product a clamped count can produce from a value inside the envelope, so the one test after the shift decides it.
    ///
    /// `signed` selects the sign extension and the range test: unsigned answers whether any bit at or above 31 survived, signed whether the value still sits in `[-2³⁰, 2³⁰)`, which is the same question `emit_checked_int_op` asks of an `i32`.
    fn emit_shift_left(
        &mut self,
        dest: &Dest<'_>,
        intrinsic: &CpsIntrinsic,
        value: &EmissionValueName,
        count: &EmissionValueName,
        name: &str,
        signed: bool,
    ) {
        let wide_local = self
            .context
            .push_local(name, curios_wasm::ValType::Num(curios_wasm::NumType::I64));
        let extend = match signed {
            true => curios_wasm::Instr::I64ExtendI32S,
            false => curios_wasm::Instr::I64ExtendI32U,
        };

        self.emit_operand(intrinsic, 0, value);
        self.emit_instr(extend.clone());
        self.emit_clamped_shift(intrinsic, count, &format!("{name}_count"));
        self.emit_instr(curios_wasm::Instr::I64ExtendI32U);
        self.emit_instr(curios_wasm::Instr::I64Shl);
        self.emit_instr(curios_wasm::Instr::LocalTee {
            local_name: wide_local.clone(),
        });

        match signed {
            // Sign-extending from bit 30 and comparing is the `i64` spelling of `emit_checked_int_op`'s bit-30-agrees-with-bit-31 test.
            true => {
                self.emit_instr(curios_wasm::Instr::I64Const { value: 33 });
                self.emit_instr(curios_wasm::Instr::I64Shl);
                self.emit_instr(curios_wasm::Instr::I64Const { value: 33 });
                self.emit_instr(curios_wasm::Instr::I64ShrS);
                self.emit_instr(curios_wasm::Instr::LocalGet {
                    local_name: wide_local.clone(),
                });
                self.emit_instr(curios_wasm::Instr::I64Ne);
            }
            false => {
                self.emit_instr(curios_wasm::Instr::I64Const { value: 31 });
                self.emit_instr(curios_wasm::Instr::I64ShrU);
                self.emit_instr(curios_wasm::Instr::I32WrapI64);
            }
        }

        self.emit_instr(curios_wasm::Instr::If {
            label_name: self.context.table().special_label(),
            block_type: curios_wasm::BlockType::Empty,
            then_instructions: vec![curios_wasm::Instr::Unreachable],
            else_instructions: vec![],
        });

        self.emit_instr(curios_wasm::Instr::LocalGet {
            local_name: wide_local,
        });
        self.emit_instr(curios_wasm::Instr::I32WrapI64);
        self.emit_store(dest, &intrinsic.result_repr());
    }

    /// Lower a right shift over a clamped count. A quotient of a value inside the envelope is inside it, so there is nothing to check.
    fn emit_shift_right(
        &mut self,
        dest: &Dest<'_>,
        intrinsic: &CpsIntrinsic,
        value: &EmissionValueName,
        count: &EmissionValueName,
        name: &str,
        op: curios_wasm::Instr,
    ) {
        self.emit_operand(intrinsic, 0, value);
        self.emit_clamped_shift(intrinsic, count, name);
        self.emit_instr(op);
        self.emit_store(dest, &intrinsic.result_repr());
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

    /// Lower an n-ary rope concat: the empty case is an empty leaf, a single operand is an alias, and longer runs fold pairs left-leaning through `result_local` — n−1 nodes, no copying. With `norm`, the stored result is re-read and normalised once at the end — every arm but the alias stores a genuine rope (a leaf, a node, or a boxed shortcut alias) — while the alias arm loads uncoerced, since a canonical operand passes through as itself.
    fn emit_rope_concat(
        &mut self,
        result_local: &curios_wasm::LocalName,
        operands: &'a [EmissionValueName],
        load: LoadAs,
        rope: &RopeData,
        norm: Option<curios_wasm::FuncName>,
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
                if let Some(norm) = &norm {
                    self.emit_instr(curios_wasm::Instr::Call {
                        func_name: norm.clone(),
                    });
                }
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            [only] => {
                let load = match norm {
                    Some(_) => LoadAs::Null,
                    None => load,
                };
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
                if let Some(norm) = norm {
                    self.emit_instrs(lhs);
                    self.emit_instr(curios_wasm::Instr::Call { func_name: norm });
                    self.emit_instr(curios_wasm::Instr::LocalSet {
                        local_name: result_local.clone(),
                    });
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
        funcs: WindowFuncs,
    ) {
        self.emit_instrs(self.context.load_value_instrs(carrier, load));
        self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
        self.emit_instrs(self.context.load_value_instrs(count, LoadAs::Nat));
        self.emit_instr(curios_wasm::Instr::Call {
            func_name: funcs.slice,
        });
        if let Some(norm) = funcs.norm {
            self.emit_instr(curios_wasm::Instr::Call { func_name: norm });
        }
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
        funcs: WindowFuncs,
    ) {
        self.emit_instrs(self.context.load_value_instrs(carrier, load.clone()));
        self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
        self.emit_instrs(self.context.load_value_instrs(carrier, load));
        self.emit_instr(Self::rope_get(rope, &rope.len_field));
        self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
        self.emit_instr(curios_wasm::Instr::I32Sub);
        self.emit_instr(curios_wasm::Instr::Call {
            func_name: funcs.slice,
        });
        if let Some(norm) = funcs.norm {
            self.emit_instr(curios_wasm::Instr::Call { func_name: norm });
        }
        self.emit_instr(curios_wasm::Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// The i31 cast every immediate arm opens with.
    fn imm_cast() -> curios_wasm::Instr {
        curios_wasm::Instr::RefCast {
            ref_type: Table::int_type(false),
        }
    }

    /// Lower a packed length with the immediate split: an i31 answers its length field's shift, a rope its length struct field. The test replaces the box call a helper entry would pay, and no memory is touched on the immediate arm.
    fn emit_bin_len(&mut self, grain: Grain, carrier: &'a EmissionValueName) {
        let len_shift = match grain {
            Grain::X => 29,
            Grain::B => 26,
        };
        let rope = self.context.table().bin_rope();
        self.emit_instrs(self.context.load_value_instrs(carrier, LoadAs::NonNull));
        self.emit_instr(curios_wasm::Instr::RefTest {
            ref_type: Table::int_type(false),
        });

        let mut imm_arm = self.context.load_value_instrs(carrier, LoadAs::NonNull);
        imm_arm.extend([
            Self::imm_cast(),
            curios_wasm::Instr::I31GetU,
            curios_wasm::Instr::I32Const { value: len_shift },
            curios_wasm::Instr::I32ShrU,
        ]);

        let mut rope_arm = self.context.load_value_instrs(carrier, LoadAs::NonNull);
        rope_arm.push(curios_wasm::Instr::RefCast {
            ref_type: curios_wasm::RefType {
                is_nullable: false,
                heap_type: curios_wasm::HeapType::Concrete(rope.base.clone()),
            },
        });
        rope_arm.push(Self::rope_get(&rope, &rope.len_field));

        self.emit_instr(curios_wasm::Instr::If {
            label_name: curios_wasm::LabelName::from("bin_len"),
            block_type: curios_wasm::BlockType::Inline(curios_wasm::ValType::Num(
                curios_wasm::NumType::I32,
            )),
            then_instructions: imm_arm,
            else_instructions: rope_arm,
        });
    }

    /// Lower a packed element read with the immediate split: an i31 answers by shift and mask — the bounds trap first, exactly where the helper's leaf arm traps — and a rope takes the byte grain's leaf split in front of the shared helper (the bit grain's leaf arm is the packed extraction, several instructions past the array read, so it stays a call). On an immediate key this is the whole read: no call, no load, no allocation.
    fn emit_bin_get(
        &mut self,
        grain: Grain,
        carrier: &'a EmissionValueName,
        index: &'a EmissionValueName,
    ) {
        let len_shift = match grain {
            Grain::X => 29,
            Grain::B => 26,
        };
        let rope = self.context.table().bin_rope();
        let read = match grain {
            Grain::X => self.context.table().bytes_read_func(),
            Grain::B => self.context.table().bits_read_func(),
        };
        let imm = self.context.push_local(
            "bytes_imm",
            curios_wasm::ValType::Num(curios_wasm::NumType::I32),
        );
        let i32_result =
            curios_wasm::BlockType::Inline(curios_wasm::ValType::Num(curios_wasm::NumType::I32));

        self.emit_instrs(self.context.load_value_instrs(carrier, LoadAs::NonNull));
        self.emit_instr(curios_wasm::Instr::RefTest {
            ref_type: Table::int_type(false),
        });

        let mut imm_arm = self.context.load_value_instrs(carrier, LoadAs::NonNull);
        imm_arm.extend([
            Self::imm_cast(),
            curios_wasm::Instr::I31GetU,
            curios_wasm::Instr::LocalTee {
                local_name: imm.clone(),
            },
        ]);
        imm_arm.extend([
            curios_wasm::Instr::I32Const { value: len_shift },
            curios_wasm::Instr::I32ShrU,
        ]);
        imm_arm.extend(self.context.load_value_instrs(index, LoadAs::Nat));
        imm_arm.extend([
            // len <= i is the out-of-bounds the helper's leaf arm refuses; same trap, same point.
            curios_wasm::Instr::I32LeU,
            curios_wasm::Instr::If {
                label_name: self.context.table().special_label(),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![curios_wasm::Instr::Unreachable],
                else_instructions: vec![],
            },
            curios_wasm::Instr::LocalGet { local_name: imm },
        ]);
        imm_arm.extend(self.context.load_value_instrs(index, LoadAs::Nat));
        match grain {
            // (v >> 8i) & 0xFF.
            Grain::X => imm_arm.extend([
                curios_wasm::Instr::I32Const { value: 3 },
                curios_wasm::Instr::I32Shl,
                curios_wasm::Instr::I32ShrU,
                curios_wasm::Instr::I32Const { value: 0xFF },
                curios_wasm::Instr::I32And,
            ]),
            // (v >> i) & 1.
            Grain::B => imm_arm.extend([
                curios_wasm::Instr::I32ShrU,
                curios_wasm::Instr::I32Const { value: 1 },
                curios_wasm::Instr::I32And,
            ]),
        }

        let cast_rope = curios_wasm::Instr::RefCast {
            ref_type: curios_wasm::RefType {
                is_nullable: false,
                heap_type: curios_wasm::HeapType::Concrete(rope.base.clone()),
            },
        };
        let mut leaf_arm = self.context.load_value_instrs(carrier, LoadAs::NonNull);
        leaf_arm.extend([
            curios_wasm::Instr::RefCast {
                ref_type: curios_wasm::RefType {
                    is_nullable: false,
                    heap_type: curios_wasm::HeapType::Concrete(rope.leaf.clone()),
                },
            },
            curios_wasm::Instr::StructGet {
                type_name: rope.leaf.clone(),
                field_name: rope.payload_field.clone(),
            },
        ]);
        leaf_arm.extend(self.context.load_value_instrs(index, LoadAs::Nat));
        leaf_arm.push(curios_wasm::Instr::ArrayGetU {
            type_name: rope.payload.clone(),
        });
        let mut read_arm = self.context.load_value_instrs(carrier, LoadAs::NonNull);
        read_arm.push(cast_rope.clone());
        read_arm.extend(self.context.load_value_instrs(index, LoadAs::Nat));
        read_arm.push(curios_wasm::Instr::Call { func_name: read });

        let mut rope_arm = self.context.load_value_instrs(carrier, LoadAs::NonNull);
        rope_arm.push(cast_rope);
        match grain {
            Grain::X => {
                rope_arm.push(Self::rope_get(&rope, &rope.tag_field));
                rope_arm.push(curios_wasm::Instr::I32Eqz);
                rope_arm.push(curios_wasm::Instr::If {
                    label_name: curios_wasm::LabelName::from("seq_get"),
                    block_type: i32_result.clone(),
                    then_instructions: leaf_arm,
                    else_instructions: read_arm,
                });
            }
            Grain::B => {
                rope_arm.extend(self.context.load_value_instrs(index, LoadAs::Nat));
                rope_arm.push(curios_wasm::Instr::Call {
                    func_name: self.context.table().bits_read_func(),
                });
            }
        }

        self.emit_instr(curios_wasm::Instr::If {
            label_name: curios_wasm::LabelName::from("bin_get"),
            block_type: i32_result,
            then_instructions: imm_arm,
            else_instructions: rope_arm,
        });
    }

    /// Lower a packed equality with the immediate split: two immediates compare as one `i32` equality — small-canonical means equal values are bit-identical — a mixed pair is unequal by construction, since a canonical rope lies past its grain's envelope and an immediate inside it, and two ropes pay the shared helper.
    fn emit_bin_eql(
        &mut self,
        grain: Grain,
        left: &'a EmissionValueName,
        right: &'a EmissionValueName,
    ) {
        let rope = self.context.table().bin_rope();
        let eql = match grain {
            Grain::X => self.context.table().bytes_eql_func(),
            Grain::B => self.context.table().bits_eql_func(),
        };
        let i31 = || curios_wasm::Instr::RefTest {
            ref_type: Table::int_type(false),
        };
        let unpack = [Self::imm_cast(), curios_wasm::Instr::I31GetU];
        let i32_result =
            curios_wasm::BlockType::Inline(curios_wasm::ValType::Num(curios_wasm::NumType::I32));
        let cast_rope = curios_wasm::Instr::RefCast {
            ref_type: curios_wasm::RefType {
                is_nullable: false,
                heap_type: curios_wasm::HeapType::Concrete(rope.base.clone()),
            },
        };

        self.emit_instrs(self.context.load_value_instrs(left, LoadAs::NonNull));
        self.emit_instr(i31());

        let mut both_imm = self.context.load_value_instrs(left, LoadAs::NonNull);
        both_imm.extend(unpack.clone());
        both_imm.extend(self.context.load_value_instrs(right, LoadAs::NonNull));
        both_imm.extend(unpack.clone());
        both_imm.push(curios_wasm::Instr::I32Eq);

        let mut left_imm = self.context.load_value_instrs(right, LoadAs::NonNull);
        left_imm.push(i31());
        left_imm.push(curios_wasm::Instr::If {
            label_name: curios_wasm::LabelName::from("bin_eql_rhs"),
            block_type: i32_result.clone(),
            then_instructions: both_imm,
            else_instructions: vec![curios_wasm::Instr::I32Const { value: 0 }],
        });

        let mut both_rope = self.context.load_value_instrs(left, LoadAs::NonNull);
        both_rope.push(cast_rope.clone());
        both_rope.extend(self.context.load_value_instrs(right, LoadAs::NonNull));
        both_rope.push(cast_rope);
        both_rope.push(curios_wasm::Instr::Call { func_name: eql });

        let mut left_rope = self.context.load_value_instrs(right, LoadAs::NonNull);
        left_rope.push(i31());
        left_rope.push(curios_wasm::Instr::If {
            label_name: curios_wasm::LabelName::from("bin_eql_mixed"),
            block_type: i32_result.clone(),
            then_instructions: vec![curios_wasm::Instr::I32Const { value: 0 }],
            else_instructions: both_rope,
        });

        self.emit_instr(curios_wasm::Instr::If {
            label_name: curios_wasm::LabelName::from("bin_eql"),
            block_type: i32_result,
            then_instructions: left_imm,
            else_instructions: left_rope,
        });
    }

    /// Lower a packed append with the immediate split. A base inside its grain's envelope appends by arithmetic — mask the element, OR it at the next slot, bump the length — with no allocation at all; a full immediate boxes into a leaf under a fresh node, entering the rope world one element past the envelope; a rope base (past the envelope, by canonicity) builds the ordinary node, whose result can never be small, so no arm normalises.
    fn emit_bin_append(
        &mut self,
        grain: Grain,
        carrier: &'a EmissionValueName,
        elem_instrs: Vec<curios_wasm::Instr>,
    ) {
        let (len_shift, payload_mask, envelope, elem_mask) = match grain {
            Grain::X => (29, 0x00FF_FFFF, 3, 0xFF),
            Grain::B => (26, 0x03FF_FFFF, 26, 1),
        };
        let rope = self.context.table().bin_rope();
        let boxf = match grain {
            Grain::X => self.context.table().bytes_box_func(),
            Grain::B => self.context.table().bits_box_func(),
        };
        let imm = self.context.push_local(
            "bytes_imm",
            curios_wasm::ValType::Num(curios_wasm::NumType::I32),
        );
        let any_result = curios_wasm::BlockType::Inline(Table::top_type(false));

        // A one-element leaf holding the (wrapped) element, shared by both node-building arms.
        let elem_leaf = |elem: Vec<curios_wasm::Instr>| {
            let mut instrs = vec![
                curios_wasm::Instr::I32Const { value: 0 },
                curios_wasm::Instr::I32Const { value: 1 },
            ];
            instrs.extend(elem);
            instrs.push(curios_wasm::Instr::ArrayNewFixed {
                type_name: rope.payload.clone(),
                length: 1,
            });
            instrs.push(curios_wasm::Instr::StructNew {
                type_name: rope.leaf.clone(),
            });
            instrs
        };

        self.emit_instrs(self.context.load_value_instrs(carrier, LoadAs::NonNull));
        self.emit_instr(curios_wasm::Instr::RefTest {
            ref_type: Table::int_type(false),
        });

        // Immediate, full: node over the boxed base and the element leaf.
        let mut overflow = vec![
            curios_wasm::Instr::I32Const { value: 1 },
            curios_wasm::Instr::I32Const {
                value: envelope + 1,
            },
        ];
        overflow.extend(self.context.load_value_instrs(carrier, LoadAs::NonNull));
        overflow.push(curios_wasm::Instr::Call { func_name: boxf });
        overflow.extend(elem_leaf(elem_instrs.clone()));
        overflow.push(curios_wasm::Instr::RefNull {
            heap_type: curios_wasm::HeapType::Concrete(rope.payload.clone()),
        });
        overflow.push(curios_wasm::Instr::StructNew {
            type_name: rope.node.clone(),
        });

        // Immediate, roomy: (payload | elem << position·len) | (len + 1) << len_shift, the position one byte or one bit per the grain.
        let mut grow = vec![
            curios_wasm::Instr::LocalGet {
                local_name: imm.clone(),
            },
            curios_wasm::Instr::I32Const {
                value: payload_mask,
            },
            curios_wasm::Instr::I32And,
        ];
        grow.extend(elem_instrs.clone());
        grow.extend([
            curios_wasm::Instr::I32Const { value: elem_mask },
            curios_wasm::Instr::I32And,
            curios_wasm::Instr::LocalGet {
                local_name: imm.clone(),
            },
            curios_wasm::Instr::I32Const { value: len_shift },
            curios_wasm::Instr::I32ShrU,
        ]);
        if matches!(grain, Grain::X) {
            grow.extend([
                curios_wasm::Instr::I32Const { value: 3 },
                curios_wasm::Instr::I32Shl,
            ]);
        }
        grow.extend([
            curios_wasm::Instr::I32Shl,
            curios_wasm::Instr::I32Or,
            curios_wasm::Instr::LocalGet {
                local_name: imm.clone(),
            },
            curios_wasm::Instr::I32Const { value: len_shift },
            curios_wasm::Instr::I32ShrU,
            curios_wasm::Instr::I32Const { value: 1 },
            curios_wasm::Instr::I32Add,
            curios_wasm::Instr::I32Const { value: len_shift },
            curios_wasm::Instr::I32Shl,
            curios_wasm::Instr::I32Or,
            curios_wasm::Instr::RefI31,
        ]);

        let mut imm_arm = self.context.load_value_instrs(carrier, LoadAs::NonNull);
        imm_arm.extend([
            Self::imm_cast(),
            curios_wasm::Instr::I31GetU,
            curios_wasm::Instr::LocalTee { local_name: imm },
            curios_wasm::Instr::I32Const { value: len_shift },
            curios_wasm::Instr::I32ShrU,
            curios_wasm::Instr::I32Const { value: envelope },
            curios_wasm::Instr::I32Eq,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("bin_append_full"),
                block_type: any_result.clone(),
                then_instructions: overflow,
                else_instructions: grow,
            },
        ]);

        // Rope base: the ordinary node, past the envelope by canonicity.
        let cast_rope = curios_wasm::Instr::RefCast {
            ref_type: curios_wasm::RefType {
                is_nullable: false,
                heap_type: curios_wasm::HeapType::Concrete(rope.base.clone()),
            },
        };
        let mut rope_arm = vec![curios_wasm::Instr::I32Const { value: 1 }];
        rope_arm.extend(self.context.load_value_instrs(carrier, LoadAs::NonNull));
        rope_arm.push(cast_rope.clone());
        rope_arm.push(Self::rope_get(&rope, &rope.len_field));
        rope_arm.push(curios_wasm::Instr::I32Const { value: 1 });
        rope_arm.push(curios_wasm::Instr::I32Add);
        rope_arm.extend(self.context.load_value_instrs(carrier, LoadAs::NonNull));
        rope_arm.push(cast_rope);
        rope_arm.extend(elem_leaf(elem_instrs));
        rope_arm.push(curios_wasm::Instr::RefNull {
            heap_type: curios_wasm::HeapType::Concrete(rope.payload.clone()),
        });
        rope_arm.push(curios_wasm::Instr::StructNew {
            type_name: rope.node.clone(),
        });

        self.emit_instr(curios_wasm::Instr::If {
            label_name: curios_wasm::LabelName::from("bin_append"),
            block_type: any_result,
            then_instructions: imm_arm,
            else_instructions: rope_arm,
        });
    }

    /// Lower a sequence read with the leaf split inline: a leaf answers `payload[i]` without leaving the function, and every other shape pays the shared `read` helper. Binaryen never produces this split from the module at any level setting, and a wider split reaching cached nodes measured as pure noise on the map wall's own workload — both recorded in `map_wall_spines_slope` — so exactly this invariant lives here and no more. The split changes no trap: a leaf's payload length is its logical length, so the inline read refuses exactly the index the helper's leaf arm refuses.
    fn emit_seq_get(
        &mut self,
        carrier: &'a EmissionValueName,
        index: &'a EmissionValueName,
        load: LoadAs,
        rope: &RopeData,
        read_func: curios_wasm::FuncName,
    ) {
        // Packed payloads read as zero-extended bytes, exactly as `emit_read_func` decides for the helper's own body.
        let packed = rope.payload == self.context.table().bytes_type();
        let get_elem = if packed {
            curios_wasm::Instr::ArrayGetU {
                type_name: rope.payload.clone(),
            }
        } else {
            curios_wasm::Instr::ArrayGet {
                type_name: rope.payload.clone(),
            }
        };
        let elem_type = if packed {
            curios_wasm::ValType::Num(curios_wasm::NumType::I32)
        } else {
            Table::top_type(true)
        };

        self.emit_instrs(self.context.load_value_instrs(carrier, load.clone()));
        self.emit_instr(Self::rope_get(rope, &rope.tag_field));
        self.emit_instr(curios_wasm::Instr::I32Eqz);

        let mut leaf_arm = self.context.load_value_instrs(carrier, load.clone());
        leaf_arm.push(curios_wasm::Instr::RefCast {
            ref_type: curios_wasm::RefType {
                is_nullable: false,
                heap_type: curios_wasm::HeapType::Concrete(rope.leaf.clone()),
            },
        });
        leaf_arm.push(curios_wasm::Instr::StructGet {
            type_name: rope.leaf.clone(),
            field_name: rope.payload_field.clone(),
        });
        leaf_arm.extend(self.context.load_value_instrs(index, LoadAs::Nat));
        leaf_arm.push(get_elem);

        let mut read_arm = self.context.load_value_instrs(carrier, load);
        read_arm.extend(self.context.load_value_instrs(index, LoadAs::Nat));
        read_arm.push(curios_wasm::Instr::Call {
            func_name: read_func,
        });

        self.emit_instr(curios_wasm::Instr::If {
            label_name: curios_wasm::LabelName::from("seq_get"),
            block_type: curios_wasm::BlockType::Inline(elem_type),
            then_instructions: leaf_arm,
            else_instructions: read_arm,
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

    /// Read field `index` through the object's *exact* tuple type, trying `arities` in order and casting on the last.
    ///
    /// The tuple types are final and unrelated, so there is no prefix supertype to read a field through any more — the object's own type has to be found. Exhausting every roster arity that could hold the field is what makes this correct without an assumption: it never supposes an object's arity is its constructor's, which `cps/fields.rs` makes false whenever a narrow constructor materialises at its region's width. The order is a preference only.
    fn tuple_get_cascade(
        &self,
        operand: &'a EmissionValueName,
        index: usize,
        arities: &[usize],
    ) -> Vec<curios_wasm::Instr> {
        match arities {
            [] => vec![curios_wasm::Instr::Unreachable],
            [last] => {
                let type_name = self.context.table().find_tuple_type(*last);
                self.context
                    .load_value_instrs(operand, LoadAs::Concrete(type_name.clone()))
                    .into_iter()
                    .chain([curios_wasm::Instr::StructGet {
                        type_name,
                        field_name: Table::tuple_field(index),
                    }])
                    .collect()
            }
            [first, rest @ ..] => {
                let type_name = self.context.table().find_tuple_type(*first);
                let hit: Vec<curios_wasm::Instr> = self
                    .context
                    .load_value_instrs(operand, LoadAs::Concrete(type_name.clone()))
                    .into_iter()
                    .chain([curios_wasm::Instr::StructGet {
                        type_name: type_name.clone(),
                        field_name: Table::tuple_field(index),
                    }])
                    .collect();

                self.context
                    .load_value_instrs(operand, LoadAs::NonNull)
                    .into_iter()
                    .chain([
                        curios_wasm::Instr::RefTest {
                            ref_type: curios_wasm::RefType {
                                is_nullable: false,
                                heap_type: curios_wasm::HeapType::Concrete(type_name),
                            },
                        },
                        curios_wasm::Instr::If {
                            label_name: curios_wasm::LabelName::from("tuple_get"),
                            block_type: curios_wasm::BlockType::Inline(Table::top_type(true)),
                            then_instructions: hit,
                            else_instructions: self.tuple_get_cascade(operand, index, rest),
                        },
                    ])
                    .collect()
            }
        }
    }

    /// Lower one intrinsic op into the current frame; `args` carries the operands in the order and arity the op fixes, verified at the CPS boundary.
    fn emit_intrinsic(&mut self, dest: &Dest<'a>, op: CpsIntrinsic, args: &'a [EmissionValueName]) {
        let (value_name, result_local) = (dest.value_name, dest.local.clone());

        // Every store below may assume this: a local held in a register is held at exactly the carrier this op produces, so no path has to coerce on the way in. What makes it true is that the representation analysis only offers a register to a value whose own definition produces that carrier — see `Offer` in `cps::represent`.
        let result_repr = match op {
            // A row read produces its slot's carrier, which is a fact of the row rather than of the operation.
            CpsIntrinsic::RowGet(row, index) => self.context.table().row_slots(row)[index].repr(),
            _ => op.result_repr(),
        };
        debug_assert!(
            self.context
                .table()
                .raw_carrier(value_name)
                .is_none_or(|carrier| carrier == result_repr),
            "`{value_name}` is held as {:?} where {op:?} produces {result_repr:?}",
            self.context.table().raw_carrier(value_name),
        );

        match op {
            CpsIntrinsic::NatEql => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Eq)
            }
            CpsIntrinsic::NatNeq => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Ne)
            }
            CpsIntrinsic::NatAdd => self.emit_checked_nat_op(
                dest,
                &op,
                &args[0],
                &args[1],
                "nat_add",
                curios_wasm::Instr::I32Add,
            ),
            CpsIntrinsic::NatSub => {
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
            CpsIntrinsic::NatMul => {
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
            CpsIntrinsic::WindowExtent => {
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
            CpsIntrinsic::NatLt => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32LtU)
            }
            CpsIntrinsic::NatDiv => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32DivU)
            }
            CpsIntrinsic::NatRem => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32RemU)
            }
            CpsIntrinsic::NatGt => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32GtU)
            }
            CpsIntrinsic::NatLe => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32LeU)
            }
            CpsIntrinsic::NatGe => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32GeU)
            }
            CpsIntrinsic::IntEql => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Eq)
            }
            CpsIntrinsic::IntNeq => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Ne)
            }
            CpsIntrinsic::IntAdd => self.emit_checked_int_op(
                dest,
                &op,
                &args[0],
                &args[1],
                "int_add",
                curios_wasm::Instr::I32Add,
            ),
            CpsIntrinsic::IntSub => self.emit_checked_int_op(
                dest,
                &op,
                &args[0],
                &args[1],
                "int_sub",
                curios_wasm::Instr::I32Sub,
            ),
            CpsIntrinsic::IntMul => {
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
            CpsIntrinsic::IntDiv => self.emit_checked_int_op(
                dest,
                &op,
                &args[0],
                &args[1],
                "int_div",
                curios_wasm::Instr::I32DivS,
            ),
            CpsIntrinsic::IntRem => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32RemS)
            }
            CpsIntrinsic::IntLt => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32LtS)
            }
            CpsIntrinsic::IntGt => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32GtS)
            }
            CpsIntrinsic::IntLe => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32LeS)
            }
            CpsIntrinsic::IntGe => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32GeS)
            }
            CpsIntrinsic::NatAnd => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32And)
            }
            CpsIntrinsic::NatOr => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Or)
            }
            CpsIntrinsic::NatXor => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Xor)
            }
            CpsIntrinsic::NatShl => {
                self.emit_shift_left(dest, &op, &args[0], &args[1], "nat_shl", false)
            }
            CpsIntrinsic::NatShr => self.emit_shift_right(
                dest,
                &op,
                &args[0],
                &args[1],
                "nat_shr",
                curios_wasm::Instr::I32ShrU,
            ),
            CpsIntrinsic::NatEqz => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::I32Eqz)
            }
            CpsIntrinsic::IntAnd => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32And)
            }
            CpsIntrinsic::IntOr => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Or)
            }
            CpsIntrinsic::IntXor => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::I32Xor)
            }
            CpsIntrinsic::IntShl => {
                self.emit_shift_left(dest, &op, &args[0], &args[1], "int_shl", true)
            }
            CpsIntrinsic::IntShr => self.emit_shift_right(
                dest,
                &op,
                &args[0],
                &args[1],
                "int_shr",
                curios_wasm::Instr::I32ShrS,
            ),
            CpsIntrinsic::IntEqz => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::I32Eqz)
            }
            CpsIntrinsic::FltAdd => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Add)
            }
            CpsIntrinsic::FltSub => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Sub)
            }
            CpsIntrinsic::FltMul => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Mul)
            }
            CpsIntrinsic::FltDiv => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Div)
            }
            // WebAssembly has no `f32.rem`, so expand the C `fmod` definition `x - trunc(x / y) * y` inline (`x`/`y` are locals, loaded twice).
            CpsIntrinsic::FltRem => {
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
            CpsIntrinsic::FltEql => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Eq)
            }
            CpsIntrinsic::FltNeq => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Ne)
            }
            CpsIntrinsic::FltLt => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Lt)
            }
            CpsIntrinsic::FltGt => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Gt)
            }
            CpsIntrinsic::FltLe => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Le)
            }
            CpsIntrinsic::FltGe => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Ge)
            }
            CpsIntrinsic::FltMin => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Min)
            }
            CpsIntrinsic::FltMax => {
                self.emit_binary_op(dest, &op, &args[0], &args[1], curios_wasm::Instr::F32Max)
            }
            CpsIntrinsic::FltNeg => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32Neg)
            }
            CpsIntrinsic::FltAbs => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32Abs)
            }
            CpsIntrinsic::FltSqrt => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32Sqrt)
            }
            CpsIntrinsic::FltFloor => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32Floor)
            }
            CpsIntrinsic::FltCeil => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32Ceil)
            }
            CpsIntrinsic::FltTrunc => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32Trunc)
            }
            CpsIntrinsic::FltNearest => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32Nearest)
            }
            CpsIntrinsic::FltCopysign => self.emit_binary_op(
                dest,
                &op,
                &args[0],
                &args[1],
                curios_wasm::Instr::F32Copysign,
            ),
            CpsIntrinsic::NatToInt => {
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
            CpsIntrinsic::NatToFlt => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32ConvertI32U)
            }
            CpsIntrinsic::IntToNat => {
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
            CpsIntrinsic::IntToFlt => {
                self.emit_unary_op(dest, &op, &args[0], curios_wasm::Instr::F32ConvertI32S)
            }
            CpsIntrinsic::FltToLeBytes => {
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
            CpsIntrinsic::FltOfLeBytes => {
                let operand = &args[0];
                // The inverse of `FltToLeBytes`: trap (via the special label) unless the `Bin` is exactly 4 bytes, then OR the bytes back into an i32 -- each `$bytes/read` zero-extends its packed byte -- and reinterpret. Byte-for-byte `f32::from_le_bytes`, no host round-trip.
                let rope = self.context.table().bin_rope();
                let read = self.context.table().bytes_read_func();
                self.emit_instrs(
                    self.context
                        .load_value_instrs(operand, LoadAs::Bin(Grain::X)),
                );
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
                    self.emit_instrs(
                        self.context
                            .load_value_instrs(operand, LoadAs::Bin(Grain::X)),
                    );
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
            CpsIntrinsic::FltToNat => {
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
            CpsIntrinsic::FltToInt => {
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
            CpsIntrinsic::BinLen(grain) => {
                self.emit_bin_len(grain, &args[0]);
                self.emit_store(dest, &op.result_repr());
            }
            CpsIntrinsic::BinEql(grain) => {
                self.emit_bin_eql(grain, &args[0], &args[1]);
                self.emit_store(dest, &op.result_repr());
            }
            CpsIntrinsic::BinGet(grain) => {
                self.emit_bin_get(grain, &args[0], &args[1]);
                self.emit_store(dest, &op.result_repr());
            }
            CpsIntrinsic::BinSlice(grain) => {
                let funcs = match grain {
                    Grain::B => WindowFuncs {
                        slice: self.context.table().bits_slice_func(),
                        norm: Some(self.context.table().bits_norm_func()),
                    },
                    Grain::X => WindowFuncs {
                        slice: self.context.table().bytes_slice_func(),
                        norm: Some(self.context.table().bytes_norm_func()),
                    },
                };
                self.emit_rope_slice(
                    &result_local,
                    &args[0],
                    &args[1],
                    &args[2],
                    LoadAs::Bin(grain),
                    funcs,
                );
            }
            CpsIntrinsic::BinRest(grain) => {
                let funcs = match grain {
                    Grain::B => WindowFuncs {
                        slice: self.context.table().bits_slice_func(),
                        norm: Some(self.context.table().bits_norm_func()),
                    },
                    Grain::X => WindowFuncs {
                        slice: self.context.table().bytes_slice_func(),
                        norm: Some(self.context.table().bytes_norm_func()),
                    },
                };
                let rope = self.context.table().bin_rope();
                self.emit_rope_rest(
                    &result_local,
                    &args[0],
                    &args[1],
                    LoadAs::Bin(grain),
                    &rope,
                    funcs,
                );
            }
            CpsIntrinsic::BinAppend(grain) => {
                let elem_instrs = self.context.load_value_instrs(&args[1], LoadAs::Nat);
                self.emit_bin_append(grain, &args[0], elem_instrs);
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsic::BinConcat(grain, _) => {
                let norm = match grain {
                    Grain::B => Some(self.context.table().bits_norm_func()),
                    Grain::X => Some(self.context.table().bytes_norm_func()),
                };
                let rope = self.context.table().bin_rope();
                self.emit_rope_concat(&result_local, args, LoadAs::Bin(grain), &rope, norm);
            }
            CpsIntrinsic::BinChunk(grain, arity) => {
                let rope = self.context.table().bin_rope();
                // A small chunk is its immediate, built by ORing each (wrapped) element at its constant slot — no allocation, no call. The envelope and the slot stride are the grain's.
                let (envelope, len_shift, elem_mask, stride) = match grain {
                    Grain::X => (3, 29, 0xFF, 8),
                    Grain::B => (26, 26, 1, 1),
                };
                if arity <= envelope {
                    self.emit_instr(curios_wasm::Instr::I32Const {
                        value: (arity as i32) << len_shift,
                    });
                    for (index, arg) in args.iter().enumerate() {
                        self.emit_instrs(self.context.load_value_instrs(arg, LoadAs::Nat));
                        self.emit_instr(curios_wasm::Instr::I32Const { value: elem_mask });
                        self.emit_instr(curios_wasm::Instr::I32And);
                        if index != 0 {
                            self.emit_instr(curios_wasm::Instr::I32Const {
                                value: (index as i32) * stride,
                            });
                            self.emit_instr(curios_wasm::Instr::I32Shl);
                        }
                        self.emit_instr(curios_wasm::Instr::I32Or);
                    }
                    self.emit_instr(curios_wasm::Instr::RefI31);
                    self.emit_instr(curios_wasm::Instr::LocalSet {
                        local_name: result_local.clone(),
                    });
                    return;
                }
                self.emit_instr(curios_wasm::Instr::I32Const { value: 0 });
                self.emit_instr(curios_wasm::Instr::I32Const {
                    value: arity as i32,
                });
                match grain {
                    Grain::X => {
                        for arg in args {
                            self.emit_instrs(self.context.load_value_instrs(arg, LoadAs::Nat));
                        }
                        self.emit_instr(curios_wasm::Instr::ArrayNewFixed {
                            type_name: rope.payload.clone(),
                            length: arity as u32,
                        });
                    }
                    // Each payload byte ORs its up-to-eight elements at constant positions, LSB-first — the packing `$bits/force` writes and `$bits/read` reads.
                    Grain::B => {
                        for byte in args.chunks(8) {
                            for (bit, arg) in byte.iter().enumerate() {
                                self.emit_instrs(self.context.load_value_instrs(arg, LoadAs::Nat));
                                if bit != 0 {
                                    self.emit_instr(curios_wasm::Instr::I32Const {
                                        value: bit as i32,
                                    });
                                    self.emit_instr(curios_wasm::Instr::I32Shl);
                                    self.emit_instr(curios_wasm::Instr::I32Or);
                                }
                            }
                        }
                        self.emit_instr(curios_wasm::Instr::ArrayNewFixed {
                            type_name: rope.payload.clone(),
                            length: args.len().div_ceil(8) as u32,
                        });
                    }
                }
                self.emit_instr(curios_wasm::Instr::StructNew {
                    type_name: rope.leaf.clone(),
                });
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsic::ListLen => {
                let rope = self.context.table().list_rope();
                self.emit_unary_op(dest, &op, &args[0], Self::rope_get(&rope, &rope.len_field));
            }
            CpsIntrinsic::ListGet => {
                let rope = self.context.table().list_rope();
                let read = self.context.table().list_read_func();
                self.emit_seq_get(&args[0], &args[1], LoadAs::List, &rope, read);
                self.emit_store(dest, &op.result_repr());
            }
            CpsIntrinsic::ListSlice => {
                let funcs = WindowFuncs {
                    slice: self.context.table().list_slice_func(),
                    norm: None,
                };
                self.emit_rope_slice(
                    &result_local,
                    &args[0],
                    &args[1],
                    &args[2],
                    LoadAs::List,
                    funcs,
                );
            }
            CpsIntrinsic::ListRest => {
                let funcs = WindowFuncs {
                    slice: self.context.table().list_slice_func(),
                    norm: None,
                };
                let rope = self.context.table().list_rope();
                self.emit_rope_rest(
                    &result_local,
                    &args[0],
                    &args[1],
                    LoadAs::List,
                    &rope,
                    funcs,
                );
            }
            CpsIntrinsic::ListAppend => {
                let rope = self.context.table().list_rope();
                let elem_instrs = self.context.load_value_instrs(&args[1], LoadAs::Null);
                self.emit_rope_append(&result_local, &args[0], elem_instrs, LoadAs::List, &rope);
            }
            CpsIntrinsic::ListConcat(_) => {
                let rope = self.context.table().list_rope();
                self.emit_rope_concat(&result_local, args, LoadAs::List, &rope, None);
            }
            CpsIntrinsic::ListSettle => {
                let rope = self.context.table().list_rope();
                let force = self.context.table().list_force_func();
                let base_ref = curios_wasm::ValType::Ref(curios_wasm::RefType {
                    is_nullable: false,
                    heap_type: curios_wasm::HeapType::Concrete(rope.base.clone()),
                });
                self.emit_instrs(self.context.load_value_instrs(&args[0], LoadAs::List));
                self.emit_instr(Self::rope_get(&rope, &rope.tag_field));
                self.emit_instr(curios_wasm::Instr::I32Eqz);
                // A leaf answers itself; anything else answers a fresh leaf over its forced payload — an O(1) wrap, since payload arrays are filled once and never rewritten.
                let leaf_arm = self.context.load_value_instrs(&args[0], LoadAs::List);
                let mut build_arm = vec![curios_wasm::Instr::I32Const { value: 0 }];
                build_arm.extend(self.context.load_value_instrs(&args[0], LoadAs::List));
                build_arm.push(Self::rope_get(&rope, &rope.len_field));
                build_arm.extend(self.context.load_value_instrs(&args[0], LoadAs::List));
                build_arm.push(curios_wasm::Instr::Call { func_name: force });
                build_arm.push(curios_wasm::Instr::StructNew {
                    type_name: rope.leaf.clone(),
                });
                self.emit_instr(curios_wasm::Instr::If {
                    label_name: curios_wasm::LabelName::from("settle"),
                    block_type: curios_wasm::BlockType::Inline(base_ref),
                    then_instructions: leaf_arm,
                    else_instructions: build_arm,
                });
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsic::ListFlat(_) => {
                let rope = self.context.table().list_rope();
                let force = self.context.table().list_force_func();
                let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
                let total = self.context.push_local("flat_total", i32_val.clone());
                let off = self.context.push_local("flat_off", i32_val);
                let out = self.context.push_local(
                    "flat_out",
                    curios_wasm::ValType::Ref(curios_wasm::RefType {
                        is_nullable: true,
                        heap_type: curios_wasm::HeapType::Concrete(rope.payload.clone()),
                    }),
                );

                // total = Σ operand lengths; out = a zeroed exact array; off restarts at zero because a scratch local persists across executions of one call site.
                for (index, arg) in args.iter().enumerate() {
                    self.emit_instrs(self.context.load_value_instrs(arg, LoadAs::List));
                    self.emit_instr(Self::rope_get(&rope, &rope.len_field));
                    if index != 0 {
                        self.emit_instr(curios_wasm::Instr::I32Add);
                    }
                }
                if args.is_empty() {
                    self.emit_instr(curios_wasm::Instr::I32Const { value: 0 });
                }
                self.emit_instr(curios_wasm::Instr::LocalTee {
                    local_name: total.clone(),
                });
                self.emit_instr(curios_wasm::Instr::ArrayNewDefault {
                    type_name: rope.payload.clone(),
                });
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: out.clone(),
                });
                self.emit_instr(curios_wasm::Instr::I32Const { value: 0 });
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: off.clone(),
                });

                // Each operand's forced payload is copied at the running offset; a leaf's force answers its payload without walking.
                for arg in args {
                    self.emit_instr(curios_wasm::Instr::LocalGet {
                        local_name: out.clone(),
                    });
                    self.emit_instr(curios_wasm::Instr::LocalGet {
                        local_name: off.clone(),
                    });
                    self.emit_instrs(self.context.load_value_instrs(arg, LoadAs::List));
                    self.emit_instr(curios_wasm::Instr::Call {
                        func_name: force.clone(),
                    });
                    self.emit_instr(curios_wasm::Instr::I32Const { value: 0 });
                    self.emit_instrs(self.context.load_value_instrs(arg, LoadAs::List));
                    self.emit_instr(Self::rope_get(&rope, &rope.len_field));
                    self.emit_instr(curios_wasm::Instr::ArrayCopy {
                        target_name: rope.payload.clone(),
                        source_name: rope.payload.clone(),
                    });
                    self.emit_instr(curios_wasm::Instr::LocalGet {
                        local_name: off.clone(),
                    });
                    self.emit_instrs(self.context.load_value_instrs(arg, LoadAs::List));
                    self.emit_instr(Self::rope_get(&rope, &rope.len_field));
                    self.emit_instr(curios_wasm::Instr::I32Add);
                    self.emit_instr(curios_wasm::Instr::LocalSet {
                        local_name: off.clone(),
                    });
                }

                self.emit_instr(curios_wasm::Instr::I32Const { value: 0 });
                self.emit_instr(curios_wasm::Instr::LocalGet { local_name: total });
                self.emit_instr(curios_wasm::Instr::LocalGet { local_name: out });
                self.emit_instr(curios_wasm::Instr::RefAsNonNull);
                self.emit_instr(curios_wasm::Instr::StructNew {
                    type_name: rope.leaf.clone(),
                });
                self.emit_instr(curios_wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            CpsIntrinsic::TupleGet(index) => {
                // Widest first: widening only ever widens, and in every row measured the wide
                // constructor is the hot one — `fork` over `leaf`, `cons` over `nil`, `some` over
                // `none` — so the first test usually hits. The roster is module-global and small
                // (2 to 5 across the whole corpus), so the chain is short whatever the order.
                let mut arities: Vec<usize> = self
                    .context
                    .table()
                    .tuple_types()
                    .map(|(arity, _)| arity)
                    .filter(|arity| *arity > index)
                    .collect();
                arities.sort_unstable_by(|left, right| right.cmp(left));

                let instrs = self.tuple_get_cascade(&args[0], index, &arities);
                self.emit_instrs(instrs);
                self.emit_store(dest, &op.result_repr());
            }
            // One exact cast, no cascade: a row value's heap type is a fact of the row, because the door pads every construction to the row's width and the row's type is final. This is what the whole keying buys — the roster search `TupleGet` performs above has nothing to search here. The slot then hands back its own carrier: a scalar arrives in a register with nothing to unbox, and the tag comes out of its packed byte through `struct.get_u`.
            CpsIntrinsic::RowGet(row, index) => {
                let family_type = self.context.table().find_row_type(row);
                let slot = self.context.table().row_slots(row)[index];
                self.emit_instrs(
                    self.context
                        .load_value_instrs(&args[0], LoadAs::Concrete(family_type.clone())),
                );
                let field_name = Table::tuple_field(index);
                self.emit_instr(match slot {
                    CpsSlot::Tag => curios_wasm::Instr::StructGetU {
                        type_name: family_type,
                        field_name,
                    },
                    _ => curios_wasm::Instr::StructGet {
                        type_name: family_type,
                        field_name,
                    },
                });
                self.emit_store(dest, &slot.repr());
            }
            CpsIntrinsic::IsImmediate => {
                self.emit_instrs(self.context.load_value_instrs(&args[0], LoadAs::NonNull));
                self.emit_instr(curios_wasm::Instr::RefTest {
                    ref_type: Table::int_type(false),
                });
                self.emit_store(dest, &op.result_repr());
            }
            // The identity on the reference — it computes nothing, and exists so the payload has a definition of its own rather than aliasing the scrutinee. `LoadAs::Null` is what `Repr::Ref` resolves to: the value is handed on exactly as stored, and each use coerces at its own site.
            CpsIntrinsic::ImmediateGet => {
                self.emit_instrs(self.context.load_value_instrs(&args[0], LoadAs::Null));
                self.emit_store(dest, &op.result_repr());
            }
        }
    }
}
