use {
    super::{Context, LoadAs},
    crate::{cont, wasm},
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

    fn emit_instr(&mut self, instr: wasm::Instr) {
        self.context
            .this_frame()
            .expect("`CodeEmitter` called outside a region")
            .instrs
            .push(instr);
    }

    fn emit_instrs<I>(&mut self, instrs: I)
    where
        I: IntoIterator<Item = wasm::Instr>,
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
            WrapAs::I31 => wasm::Instr::RefI31,
            WrapAs::Flt => wasm::Instr::StructNew {
                type_name: self.context.table().flt_type(),
            },
        };

        self.emit_instr(instr);
    }

    /// Lower a one-operand numeric op: load the operand, apply `op`, box the result, store it.
    fn emit_unary_op(
        &mut self,
        result_local: &wasm::LocalName,
        operand: &cont::ValueName,
        load: LoadAs,
        op: wasm::Instr,
        wrap: WrapAs,
    ) {
        self.emit_instrs(self.context.load_value_instrs(operand, load));
        self.emit_instr(op);
        self.emit_wrap(wrap);
        self.emit_instr(wasm::Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// Lower a two-operand numeric op: both operands share `load`, apply `op`, box, store.
    fn emit_binary_op(
        &mut self,
        result_local: &wasm::LocalName,
        left: &cont::ValueName,
        right: &cont::ValueName,
        load: LoadAs,
        op: wasm::Instr,
        wrap: WrapAs,
    ) {
        self.emit_instrs(self.context.load_value_instrs(left, load.clone()));
        self.emit_instrs(self.context.load_value_instrs(right, load));
        self.emit_instr(op);
        self.emit_wrap(wrap);
        self.emit_instr(wasm::Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// Lower an unsigned-`Nat` binary op that may overflow the i31 carrier:
    /// apply `op`, trap (via the special label) if bit 31 of the result is set,
    /// else box with `ref.i31` and store.
    fn emit_checked_nat_op(
        &mut self,
        result_local: &wasm::LocalName,
        left: &cont::ValueName,
        right: &cont::ValueName,
        name: &str,
        op: wasm::Instr,
    ) {
        let local_name = self
            .context
            .push_local(name, wasm::ValType::Num(wasm::NumType::I32));
        self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Nat));
        self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Nat));
        self.emit_instr(op);
        self.emit_instr(wasm::Instr::LocalTee {
            local_name: local_name.clone(),
        });
        self.emit_instr(wasm::Instr::I32Const { value: 31 });
        self.emit_instr(wasm::Instr::I32ShrU);
        self.emit_instr(wasm::Instr::If {
            label_name: self.context.table().special_label(),
            block_type: wasm::BlockType::Empty,
            then_instructions: vec![wasm::Instr::Unreachable],
            else_instructions: vec![],
        });
        self.emit_instr(wasm::Instr::LocalGet { local_name });
        self.emit_instr(wasm::Instr::RefI31);
        self.emit_instr(wasm::Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    /// Lower a signed-`Int` binary op that may overflow the i31 carrier: apply
    /// `op`, trap (via the special label) if the result leaves the signed
    /// 31-bit range, else box with `ref.i31` and store.
    fn emit_checked_int_op(
        &mut self,
        result_local: &wasm::LocalName,
        left: &cont::ValueName,
        right: &cont::ValueName,
        name: &str,
        op: wasm::Instr,
    ) {
        let local_name = self
            .context
            .push_local(name, wasm::ValType::Num(wasm::NumType::I32));
        self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
        self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
        self.emit_instr(op);
        self.emit_instr(wasm::Instr::LocalTee {
            local_name: local_name.clone(),
        });
        self.emit_instr(wasm::Instr::I32Const { value: 1 });
        self.emit_instr(wasm::Instr::I32Shl);
        self.emit_instr(wasm::Instr::LocalGet {
            local_name: local_name.clone(),
        });
        self.emit_instr(wasm::Instr::I32Xor);
        self.emit_instr(wasm::Instr::I32Const { value: 31 });
        self.emit_instr(wasm::Instr::I32ShrU);
        self.emit_instr(wasm::Instr::If {
            label_name: self.context.table().special_label(),
            block_type: wasm::BlockType::Empty,
            then_instructions: vec![wasm::Instr::Unreachable],
            else_instructions: vec![],
        });
        self.emit_instr(wasm::Instr::LocalGet { local_name });
        self.emit_instr(wasm::Instr::RefI31);
        self.emit_instr(wasm::Instr::LocalSet {
            local_name: result_local.clone(),
        });
    }

    pub fn emit(&mut self, value_name: &'a cont::ValueName, op: &'a cont::Code) {
        let result_local = self
            .context
            .find_local(value_name)
            .map(|ld| ld.local_name)
            .unwrap_or_else(|| panic!("`CodeEmitter` lacks local `{}`", value_name));

        match op {
            cont::Code::NatEql(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                wasm::Instr::I32Eq,
                WrapAs::I31,
            ),
            cont::Code::NatNeq(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                wasm::Instr::I32Ne,
                WrapAs::I31,
            ),
            cont::Code::NatAdd(left, right) => {
                self.emit_checked_nat_op(&result_local, left, right, "nat_add", wasm::Instr::I32Add)
            }
            cont::Code::NatSub(left, right) => {
                // Monus: 0 if left < right, else left - right.
                // select [val1=0, val2=left-right, cond=left<right] returns val1 when cond != 0.
                self.emit_instr(wasm::Instr::I32Const { value: 0 });
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Nat));
                self.emit_instr(wasm::Instr::I32Sub);
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Nat));
                self.emit_instr(wasm::Instr::I32LtU);
                self.emit_instr(wasm::Instr::Select { val_types: vec![] });
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatMul(left, right) => {
                let local_name = self
                    .context
                    .push_local("nat_mul", wasm::ValType::Num(wasm::NumType::I64));
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Nat));
                self.emit_instr(wasm::Instr::I64ExtendI32U);
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Nat));
                self.emit_instr(wasm::Instr::I64ExtendI32U);
                self.emit_instr(wasm::Instr::I64Mul);
                self.emit_instr(wasm::Instr::LocalTee {
                    local_name: local_name.clone(),
                });
                self.emit_instr(wasm::Instr::I64Const { value: 31 });
                self.emit_instr(wasm::Instr::I64ShrU);
                self.emit_instr(wasm::Instr::I32WrapI64);
                self.emit_instr(wasm::Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: wasm::BlockType::Empty,
                    then_instructions: vec![wasm::Instr::Unreachable],
                    else_instructions: vec![],
                });
                self.emit_instr(wasm::Instr::LocalGet { local_name });
                self.emit_instr(wasm::Instr::I32WrapI64);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatLt(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                wasm::Instr::I32LtU,
                WrapAs::I31,
            ),
            cont::Code::NatDiv(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                wasm::Instr::I32DivU,
                WrapAs::I31,
            ),
            cont::Code::NatRem(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                wasm::Instr::I32RemU,
                WrapAs::I31,
            ),
            cont::Code::NatGt(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                wasm::Instr::I32GtU,
                WrapAs::I31,
            ),
            cont::Code::NatLte(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                wasm::Instr::I32LeU,
                WrapAs::I31,
            ),
            cont::Code::NatGte(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                wasm::Instr::I32GeU,
                WrapAs::I31,
            ),
            cont::Code::IntEql(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                wasm::Instr::I32Eq,
                WrapAs::I31,
            ),
            cont::Code::IntNeq(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                wasm::Instr::I32Ne,
                WrapAs::I31,
            ),
            cont::Code::IntAdd(left, right) => {
                self.emit_checked_int_op(&result_local, left, right, "int_add", wasm::Instr::I32Add)
            }
            cont::Code::IntSub(left, right) => {
                self.emit_checked_int_op(&result_local, left, right, "int_sub", wasm::Instr::I32Sub)
            }
            cont::Code::IntMul(left, right) => {
                let local_name = self
                    .context
                    .push_local("int_mul", wasm::ValType::Num(wasm::NumType::I64));
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instr(wasm::Instr::I64ExtendI32S);
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I64ExtendI32S);
                self.emit_instr(wasm::Instr::I64Mul);
                self.emit_instr(wasm::Instr::LocalTee {
                    local_name: local_name.clone(),
                });
                self.emit_instr(wasm::Instr::I64Const { value: 30 });
                self.emit_instr(wasm::Instr::I64ShrS);
                self.emit_instr(wasm::Instr::LocalGet {
                    local_name: local_name.clone(),
                });
                self.emit_instr(wasm::Instr::I64Const { value: 63 });
                self.emit_instr(wasm::Instr::I64ShrS);
                self.emit_instr(wasm::Instr::I64Ne);
                self.emit_instr(wasm::Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: wasm::BlockType::Empty,
                    then_instructions: vec![wasm::Instr::Unreachable],
                    else_instructions: vec![],
                });
                self.emit_instr(wasm::Instr::LocalGet { local_name });
                self.emit_instr(wasm::Instr::I32WrapI64);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntDiv(left, right) => {
                self.emit_checked_int_op(&result_local, left, right, "int_div", wasm::Instr::I32DivS)
            }
            cont::Code::IntRem(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                wasm::Instr::I32RemS,
                WrapAs::I31,
            ),
            cont::Code::IntLt(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                wasm::Instr::I32LtS,
                WrapAs::I31,
            ),
            cont::Code::IntGt(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                wasm::Instr::I32GtS,
                WrapAs::I31,
            ),
            cont::Code::IntLte(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                wasm::Instr::I32LeS,
                WrapAs::I31,
            ),
            cont::Code::IntGte(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                wasm::Instr::I32GeS,
                WrapAs::I31,
            ),
            cont::Code::NatAnd(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                wasm::Instr::I32And,
                WrapAs::I31,
            ),
            cont::Code::NatOr(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                wasm::Instr::I32Or,
                WrapAs::I31,
            ),
            cont::Code::NatXor(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                wasm::Instr::I32Xor,
                WrapAs::I31,
            ),
            // Left shift truncates into the 31-bit carrier rather than trapping:
            // `ref.i31` already drops bit 31, so `i32.shl` + `WrapAs::I31` is a
            // guard-free truncating shift (matching `shr`, which never overflows).
            cont::Code::NatShl(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                wasm::Instr::I32Shl,
                WrapAs::I31,
            ),
            cont::Code::NatShr(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Nat,
                wasm::Instr::I32ShrU,
                WrapAs::I31,
            ),
            cont::Code::NatRotl(left, right) => {
                self.emit_checked_nat_op(&result_local, left, right, "nat_rotl", wasm::Instr::I32Rotl)
            }
            cont::Code::NatRotr(left, right) => {
                self.emit_checked_nat_op(&result_local, left, right, "nat_rotr", wasm::Instr::I32Rotr)
            }
            cont::Code::NatClz(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Nat,
                wasm::Instr::I32Clz,
                WrapAs::I31,
            ),
            cont::Code::NatCtz(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Nat,
                wasm::Instr::I32Ctz,
                WrapAs::I31,
            ),
            cont::Code::NatPopcnt(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Nat,
                wasm::Instr::I32Popcnt,
                WrapAs::I31,
            ),
            cont::Code::NatEqz(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Nat,
                wasm::Instr::I32Eqz,
                WrapAs::I31,
            ),
            cont::Code::IntAnd(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                wasm::Instr::I32And,
                WrapAs::I31,
            ),
            cont::Code::IntOr(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                wasm::Instr::I32Or,
                WrapAs::I31,
            ),
            cont::Code::IntXor(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                wasm::Instr::I32Xor,
                WrapAs::I31,
            ),
            // Left shift truncates into the signed 31-bit carrier rather than
            // trapping: `ref.i31` drops bit 31 and the value reloads sign-extended
            // from bit 30, so `i32.shl` + `WrapAs::I31` is a guard-free truncating
            // shift, matching `Nat/shl` and `shr` (which never overflow).
            cont::Code::IntShl(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                wasm::Instr::I32Shl,
                WrapAs::I31,
            ),
            cont::Code::IntShr(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Int,
                wasm::Instr::I32ShrS,
                WrapAs::I31,
            ),
            cont::Code::IntRotl(left, right) => {
                self.emit_checked_int_op(&result_local, left, right, "int_rotl", wasm::Instr::I32Rotl)
            }
            cont::Code::IntRotr(left, right) => {
                self.emit_checked_int_op(&result_local, left, right, "int_rotr", wasm::Instr::I32Rotr)
            }
            cont::Code::IntClz(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Int,
                wasm::Instr::I32Clz,
                WrapAs::I31,
            ),
            cont::Code::IntCtz(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Int,
                wasm::Instr::I32Ctz,
                WrapAs::I31,
            ),
            cont::Code::IntPopcnt(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Int,
                wasm::Instr::I32Popcnt,
                WrapAs::I31,
            ),
            cont::Code::IntEqz(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Int,
                wasm::Instr::I32Eqz,
                WrapAs::I31,
            ),
            cont::Code::FltAdd(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                wasm::Instr::F32Add,
                WrapAs::Flt,
            ),
            cont::Code::FltSub(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                wasm::Instr::F32Sub,
                WrapAs::Flt,
            ),
            cont::Code::FltMul(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                wasm::Instr::F32Mul,
                WrapAs::Flt,
            ),
            cont::Code::FltDiv(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                wasm::Instr::F32Div,
                WrapAs::Flt,
            ),
            cont::Code::FltEql(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                wasm::Instr::F32Eq,
                WrapAs::I31,
            ),
            cont::Code::FltNeq(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                wasm::Instr::F32Ne,
                WrapAs::I31,
            ),
            cont::Code::FltLt(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                wasm::Instr::F32Lt,
                WrapAs::I31,
            ),
            cont::Code::FltGt(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                wasm::Instr::F32Gt,
                WrapAs::I31,
            ),
            cont::Code::FltLte(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                wasm::Instr::F32Le,
                WrapAs::I31,
            ),
            cont::Code::FltGte(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                wasm::Instr::F32Ge,
                WrapAs::I31,
            ),
            cont::Code::FltMin(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                wasm::Instr::F32Min,
                WrapAs::Flt,
            ),
            cont::Code::FltMax(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                wasm::Instr::F32Max,
                WrapAs::Flt,
            ),
            cont::Code::FltNeg(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Flt,
                wasm::Instr::F32Neg,
                WrapAs::Flt,
            ),
            cont::Code::FltAbs(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Flt,
                wasm::Instr::F32Abs,
                WrapAs::Flt,
            ),
            cont::Code::FltSqrt(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Flt,
                wasm::Instr::F32Sqrt,
                WrapAs::Flt,
            ),
            cont::Code::FltFloor(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Flt,
                wasm::Instr::F32Floor,
                WrapAs::Flt,
            ),
            cont::Code::FltCeil(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Flt,
                wasm::Instr::F32Ceil,
                WrapAs::Flt,
            ),
            cont::Code::FltTrunc(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Flt,
                wasm::Instr::F32Trunc,
                WrapAs::Flt,
            ),
            cont::Code::FltNearest(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Flt,
                wasm::Instr::F32Nearest,
                WrapAs::Flt,
            ),
            cont::Code::FltCopysign(left, right) => self.emit_binary_op(
                &result_local,
                left,
                right,
                LoadAs::Flt,
                wasm::Instr::F32Copysign,
                WrapAs::Flt,
            ),
            cont::Code::NatToStr(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Nat));
                self.emit_instr(wasm::Instr::Call {
                    func_name: self.context.table().nat_to_str_func().clone(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatToInt(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Nat));
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatToFlt(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Nat,
                wasm::Instr::F32ConvertI32U,
                WrapAs::Flt,
            ),
            cont::Code::IntToStr(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(wasm::Instr::Call {
                    func_name: self.context.table().int_to_str_func().clone(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntToNat(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntToFlt(operand) => self.emit_unary_op(
                &result_local,
                operand,
                LoadAs::Int,
                wasm::Instr::F32ConvertI32S,
                WrapAs::Flt,
            ),
            cont::Code::FltToStr(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(wasm::Instr::Call {
                    func_name: self.context.table().flt_to_str_func().clone(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltToLeBin(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(wasm::Instr::Call {
                    func_name: self.context.table().flt_to_le_bin_func().clone(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltToNat(operand) => {
                let local_name = self
                    .context
                    .push_local("flt_to_nat", wasm::ValType::Num(wasm::NumType::I32));
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(wasm::Instr::I32TruncF32U);
                self.emit_instr(wasm::Instr::LocalTee {
                    local_name: local_name.clone(),
                });
                self.emit_instr(wasm::Instr::I32Const { value: 31 });
                self.emit_instr(wasm::Instr::I32ShrU);
                self.emit_instr(wasm::Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: wasm::BlockType::Empty,
                    then_instructions: vec![wasm::Instr::Unreachable],
                    else_instructions: vec![],
                });
                self.emit_instr(wasm::Instr::LocalGet { local_name });
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltToInt(operand) => {
                let local_name = self
                    .context
                    .push_local("flt_to_int", wasm::ValType::Num(wasm::NumType::I32));
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(wasm::Instr::I32TruncF32S);
                self.emit_instr(wasm::Instr::LocalTee {
                    local_name: local_name.clone(),
                });
                self.emit_instr(wasm::Instr::I32Const { value: 1 });
                self.emit_instr(wasm::Instr::I32Shl);
                self.emit_instr(wasm::Instr::LocalGet {
                    local_name: local_name.clone(),
                });
                self.emit_instr(wasm::Instr::I32Xor);
                self.emit_instr(wasm::Instr::I32Const { value: 31 });
                self.emit_instr(wasm::Instr::I32ShrU);
                self.emit_instr(wasm::Instr::If {
                    label_name: self.context.table().special_label(),
                    block_type: wasm::BlockType::Empty,
                    then_instructions: vec![wasm::Instr::Unreachable],
                    else_instructions: vec![],
                });
                self.emit_instr(wasm::Instr::LocalGet { local_name });
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::BinLen(bin) => self.emit_unary_op(
                &result_local,
                bin,
                LoadAs::Bin,
                wasm::Instr::ArrayLen,
                WrapAs::I31,
            ),
            cont::Code::BinEql(left, right) => {
                let bin_type = self.context.table().bin_type();

                let idx_local = self
                    .context
                    .push_local("idx", wasm::ValType::Num(wasm::NumType::I32));
                let result_raw_local = self
                    .context
                    .push_local("eql", wasm::ValType::Num(wasm::NumType::I32));

                let done_label = wasm::LabelName::from(format!("{}_done", result_local));
                let loop_label = wasm::LabelName::from(format!("{}_loop", result_local));
                let if_label = wasm::LabelName::from(format!("{}_if", result_local));

                let load_left = self.context.load_value_instrs(left, LoadAs::Bin);
                let load_right = self.context.load_value_instrs(right, LoadAs::Bin);

                // Build the loop body instructions.
                let mut loop_instrs = Vec::new();

                // if idx >= left.len: all bytes matched, result = true, exit block
                loop_instrs.push(wasm::Instr::LocalGet {
                    local_name: idx_local.clone(),
                });
                loop_instrs.extend(load_left.clone());
                loop_instrs.push(wasm::Instr::ArrayLen);
                loop_instrs.push(wasm::Instr::I32GeU);
                loop_instrs.push(wasm::Instr::If {
                    label_name: if_label,
                    block_type: wasm::BlockType::Empty,
                    then_instructions: vec![
                        wasm::Instr::I32Const { value: 1 },
                        wasm::Instr::LocalSet {
                            local_name: result_raw_local.clone(),
                        },
                        wasm::Instr::Br {
                            label_name: done_label.clone(),
                        },
                    ],
                    else_instructions: vec![],
                });

                // if left[idx] != right[idx]: mismatch, exit block (result stays false)
                loop_instrs.extend(load_left.clone());
                loop_instrs.push(wasm::Instr::LocalGet {
                    local_name: idx_local.clone(),
                });
                loop_instrs.push(wasm::Instr::ArrayGetU {
                    type_name: bin_type.clone(),
                });
                loop_instrs.extend(load_right.clone());
                loop_instrs.push(wasm::Instr::LocalGet {
                    local_name: idx_local.clone(),
                });
                loop_instrs.push(wasm::Instr::ArrayGetU {
                    type_name: bin_type,
                });
                loop_instrs.push(wasm::Instr::I32Ne);
                loop_instrs.push(wasm::Instr::BrIf {
                    label_name: done_label.clone(),
                });

                // idx += 1; continue loop
                loop_instrs.extend([
                    wasm::Instr::LocalGet {
                        local_name: idx_local.clone(),
                    },
                    wasm::Instr::I32Const { value: 1 },
                    wasm::Instr::I32Add,
                    wasm::Instr::LocalSet {
                        local_name: idx_local,
                    },
                    wasm::Instr::Br {
                        label_name: loop_label.clone(),
                    },
                ]);

                // Build the outer block: length check then loop.
                let mut block_instrs = Vec::new();

                // if left.len != right.len: exit block immediately (result stays false)
                block_instrs.extend(load_left.clone());
                block_instrs.push(wasm::Instr::ArrayLen);
                block_instrs.extend(load_right);
                block_instrs.push(wasm::Instr::ArrayLen);
                block_instrs.push(wasm::Instr::I32Ne);
                block_instrs.push(wasm::Instr::BrIf {
                    label_name: done_label.clone(),
                });

                block_instrs.push(wasm::Instr::Loop {
                    label_name: loop_label,
                    block_type: wasm::BlockType::Empty,
                    instructions: loop_instrs,
                });

                // result_raw defaults to 0 (false); emit block; box result as i31ref
                self.emit_instr(wasm::Instr::I32Const { value: 0 });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_raw_local.clone(),
                });
                self.emit_instr(wasm::Instr::Block {
                    label_name: done_label,
                    block_type: wasm::BlockType::Empty,
                    instructions: block_instrs,
                });
                self.emit_instr(wasm::Instr::LocalGet {
                    local_name: result_raw_local,
                });
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local,
                });
            }
            cont::Code::BinGet(bin, idx) => {
                let bin_type = self.context.table().bin_type();
                self.emit_instrs(self.context.load_value_instrs(bin, LoadAs::Bin));
                self.emit_instrs(self.context.load_value_instrs(idx, LoadAs::Nat));
                self.emit_instr(wasm::Instr::ArrayGetU {
                    type_name: bin_type,
                });
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::BinSlice(bin, start, end) => {
                let bin_type = self.context.table().bin_type();
                self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
                self.emit_instr(wasm::Instr::I32Sub);
                self.emit_instr(wasm::Instr::ArrayNewDefault {
                    type_name: bin_type.clone(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });

                self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Bin));
                self.emit_instr(wasm::Instr::I32Const { value: 0 });
                self.emit_instrs(self.context.load_value_instrs(bin, LoadAs::Bin));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
                self.emit_instr(wasm::Instr::I32Sub);
                self.emit_instr(wasm::Instr::ArrayCopy {
                    source_name: bin_type.clone(),
                    target_name: bin_type,
                });
            }
            cont::Code::BinAppend(bin, byte) => {
                let bin_type = self.context.table().bin_type();
                self.emit_instrs(self.context.load_value_instrs(bin, LoadAs::Bin));
                self.emit_instr(wasm::Instr::ArrayLen);
                self.emit_instr(wasm::Instr::I32Const { value: 1 });
                self.emit_instr(wasm::Instr::I32Add);
                self.emit_instr(wasm::Instr::ArrayNewDefault {
                    type_name: bin_type.clone(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });

                self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Bin));
                self.emit_instr(wasm::Instr::I32Const { value: 0 });
                self.emit_instrs(self.context.load_value_instrs(bin, LoadAs::Bin));
                self.emit_instr(wasm::Instr::I32Const { value: 0 });
                self.emit_instrs(self.context.load_value_instrs(bin, LoadAs::Bin));
                self.emit_instr(wasm::Instr::ArrayLen);
                self.emit_instr(wasm::Instr::ArrayCopy {
                    source_name: bin_type.clone(),
                    target_name: bin_type.clone(),
                });

                self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Bin));
                self.emit_instrs(self.context.load_value_instrs(bin, LoadAs::Bin));
                self.emit_instr(wasm::Instr::ArrayLen);
                self.emit_instrs(self.context.load_value_instrs(byte, LoadAs::Nat));
                self.emit_instr(wasm::Instr::ArraySet {
                    type_name: bin_type,
                });
            }
            cont::Code::BinConcat(operands) => {
                let bin_type = self.context.table().bin_type();

                match operands.as_slice() {
                    [] => {
                        self.emit_instr(wasm::Instr::ArrayNewFixed {
                            type_name: bin_type,
                            length: 0,
                        });
                        self.emit_instr(wasm::Instr::LocalSet {
                            local_name: result_local,
                        });
                    }
                    [only] => {
                        self.emit_instrs(self.context.load_value_instrs(only, LoadAs::Bin));
                        self.emit_instr(wasm::Instr::LocalSet {
                            local_name: result_local,
                        });
                    }
                    [b1, b2] => {
                        self.emit_instrs(self.context.load_value_instrs(b1, LoadAs::Bin));
                        self.emit_instr(wasm::Instr::ArrayLen);
                        self.emit_instrs(self.context.load_value_instrs(b2, LoadAs::Bin));
                        self.emit_instr(wasm::Instr::ArrayLen);
                        self.emit_instr(wasm::Instr::I32Add);
                        self.emit_instr(wasm::Instr::ArrayNewDefault {
                            type_name: bin_type.clone(),
                        });
                        self.emit_instr(wasm::Instr::LocalSet {
                            local_name: result_local.clone(),
                        });

                        self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Bin));
                        self.emit_instr(wasm::Instr::I32Const { value: 0 });
                        self.emit_instrs(self.context.load_value_instrs(b1, LoadAs::Bin));
                        self.emit_instr(wasm::Instr::I32Const { value: 0 });
                        self.emit_instrs(self.context.load_value_instrs(b1, LoadAs::Bin));
                        self.emit_instr(wasm::Instr::ArrayLen);
                        self.emit_instr(wasm::Instr::ArrayCopy {
                            source_name: bin_type.clone(),
                            target_name: bin_type.clone(),
                        });

                        self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Bin));
                        self.emit_instrs(self.context.load_value_instrs(b1, LoadAs::Bin));
                        self.emit_instr(wasm::Instr::ArrayLen);
                        self.emit_instrs(self.context.load_value_instrs(b2, LoadAs::Bin));
                        self.emit_instr(wasm::Instr::I32Const { value: 0 });
                        self.emit_instrs(self.context.load_value_instrs(b2, LoadAs::Bin));
                        self.emit_instr(wasm::Instr::ArrayLen);
                        self.emit_instr(wasm::Instr::ArrayCopy {
                            source_name: bin_type.clone(),
                            target_name: bin_type,
                        });
                    }
                    operands => {
                        self.emit_instr(wasm::Instr::I32Const { value: 0 });
                        for operand in operands.iter() {
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Bin));
                            self.emit_instr(wasm::Instr::ArrayLen);
                            self.emit_instr(wasm::Instr::I32Add);
                        }
                        self.emit_instr(wasm::Instr::ArrayNewDefault {
                            type_name: bin_type.clone(),
                        });
                        self.emit_instr(wasm::Instr::LocalSet {
                            local_name: result_local.clone(),
                        });

                        let offset_local = self
                            .context
                            .push_local("offset", wasm::ValType::Num(wasm::NumType::I32));

                        for operand in operands.iter() {
                            self.emit_instrs(
                                self.context.load_value_instrs(value_name, LoadAs::Bin),
                            );
                            self.emit_instr(wasm::Instr::LocalGet {
                                local_name: offset_local.clone(),
                            });
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Bin));
                            self.emit_instr(wasm::Instr::I32Const { value: 0 });
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Bin));
                            self.emit_instr(wasm::Instr::ArrayLen);
                            self.emit_instr(wasm::Instr::ArrayCopy {
                                source_name: bin_type.clone(),
                                target_name: bin_type.clone(),
                            });

                            self.emit_instr(wasm::Instr::LocalGet {
                                local_name: offset_local.clone(),
                            });
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Bin));
                            self.emit_instr(wasm::Instr::ArrayLen);
                            self.emit_instr(wasm::Instr::I32Add);
                            self.emit_instr(wasm::Instr::LocalSet {
                                local_name: offset_local.clone(),
                            });
                        }
                    }
                }
            }
            cont::Code::ArrLen(lst) => self.emit_unary_op(
                &result_local,
                lst,
                LoadAs::Arr,
                wasm::Instr::ArrayLen,
                WrapAs::I31,
            ),
            cont::Code::ArrGet(lst, idx) => {
                let arr_type = self.context.table().arr_type();
                self.emit_instrs(self.context.load_value_instrs(lst, LoadAs::Arr));
                self.emit_instrs(self.context.load_value_instrs(idx, LoadAs::Nat));
                self.emit_instr(wasm::Instr::ArrayGet {
                    type_name: arr_type,
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::ArrSlice(lst, start, end) => {
                let arr_type = self.context.table().arr_type();

                self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
                self.emit_instr(wasm::Instr::I32Sub);
                self.emit_instr(wasm::Instr::ArrayNewDefault {
                    type_name: arr_type.clone(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });

                self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Arr));
                self.emit_instr(wasm::Instr::I32Const { value: 0 });
                self.emit_instrs(self.context.load_value_instrs(lst, LoadAs::Arr));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Nat));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Nat));
                self.emit_instr(wasm::Instr::I32Sub);
                self.emit_instr(wasm::Instr::ArrayCopy {
                    source_name: arr_type.clone(),
                    target_name: arr_type.clone(),
                });
            }
            cont::Code::ArrAppend(lst, elem) => {
                let arr_type = self.context.table().arr_type();
                self.emit_instrs(self.context.load_value_instrs(lst, LoadAs::Arr));
                self.emit_instr(wasm::Instr::ArrayLen);
                self.emit_instr(wasm::Instr::I32Const { value: 1 });
                self.emit_instr(wasm::Instr::I32Add);
                self.emit_instr(wasm::Instr::ArrayNewDefault {
                    type_name: arr_type.clone(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });

                self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Arr));
                self.emit_instr(wasm::Instr::I32Const { value: 0 });
                self.emit_instrs(self.context.load_value_instrs(lst, LoadAs::Arr));
                self.emit_instr(wasm::Instr::I32Const { value: 0 });
                self.emit_instrs(self.context.load_value_instrs(lst, LoadAs::Arr));
                self.emit_instr(wasm::Instr::ArrayLen);
                self.emit_instr(wasm::Instr::ArrayCopy {
                    source_name: arr_type.clone(),
                    target_name: arr_type.clone(),
                });

                self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Arr));
                self.emit_instrs(self.context.load_value_instrs(lst, LoadAs::Arr));
                self.emit_instr(wasm::Instr::ArrayLen);
                self.emit_instrs(self.context.load_value_instrs(elem, LoadAs::Null));
                self.emit_instr(wasm::Instr::ArraySet {
                    type_name: arr_type,
                });
            }
            cont::Code::ArrConcat(operands) => {
                let arr_type = self.context.table().arr_type();

                match operands.as_slice() {
                    [] => {
                        self.emit_instr(wasm::Instr::ArrayNewFixed {
                            type_name: arr_type,
                            length: 0,
                        });
                        self.emit_instr(wasm::Instr::LocalSet {
                            local_name: result_local,
                        });
                    }
                    [only] => {
                        self.emit_instrs(self.context.load_value_instrs(only, LoadAs::Arr));
                        self.emit_instr(wasm::Instr::LocalSet {
                            local_name: result_local,
                        });
                    }
                    [l1, l2] => {
                        self.emit_instrs(self.context.load_value_instrs(l1, LoadAs::Arr));
                        self.emit_instr(wasm::Instr::ArrayLen);
                        self.emit_instrs(self.context.load_value_instrs(l2, LoadAs::Arr));
                        self.emit_instr(wasm::Instr::ArrayLen);
                        self.emit_instr(wasm::Instr::I32Add);
                        self.emit_instr(wasm::Instr::ArrayNewDefault {
                            type_name: arr_type.clone(),
                        });
                        self.emit_instr(wasm::Instr::LocalSet {
                            local_name: result_local.clone(),
                        });

                        self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Arr));
                        self.emit_instr(wasm::Instr::I32Const { value: 0 });
                        self.emit_instrs(self.context.load_value_instrs(l1, LoadAs::Arr));
                        self.emit_instr(wasm::Instr::I32Const { value: 0 });
                        self.emit_instrs(self.context.load_value_instrs(l1, LoadAs::Arr));
                        self.emit_instr(wasm::Instr::ArrayLen);
                        self.emit_instr(wasm::Instr::ArrayCopy {
                            source_name: arr_type.clone(),
                            target_name: arr_type.clone(),
                        });

                        self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Arr));
                        self.emit_instrs(self.context.load_value_instrs(l1, LoadAs::Arr));
                        self.emit_instr(wasm::Instr::ArrayLen);
                        self.emit_instrs(self.context.load_value_instrs(l2, LoadAs::Arr));
                        self.emit_instr(wasm::Instr::I32Const { value: 0 });
                        self.emit_instrs(self.context.load_value_instrs(l2, LoadAs::Arr));
                        self.emit_instr(wasm::Instr::ArrayLen);
                        self.emit_instr(wasm::Instr::ArrayCopy {
                            source_name: arr_type.clone(),
                            target_name: arr_type,
                        });
                    }
                    operands => {
                        self.emit_instr(wasm::Instr::I32Const { value: 0 });
                        for operand in operands.iter() {
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Arr));
                            self.emit_instr(wasm::Instr::ArrayLen);
                            self.emit_instr(wasm::Instr::I32Add);
                        }
                        self.emit_instr(wasm::Instr::ArrayNewDefault {
                            type_name: arr_type.clone(),
                        });
                        self.emit_instr(wasm::Instr::LocalSet {
                            local_name: result_local.clone(),
                        });

                        let offset_local = self
                            .context
                            .push_local("offset", wasm::ValType::Num(wasm::NumType::I32));

                        for operand in operands.iter() {
                            self.emit_instrs(
                                self.context.load_value_instrs(value_name, LoadAs::Arr),
                            );
                            self.emit_instr(wasm::Instr::LocalGet {
                                local_name: offset_local.clone(),
                            });
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Arr));
                            self.emit_instr(wasm::Instr::I32Const { value: 0 });
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Arr));
                            self.emit_instr(wasm::Instr::ArrayLen);
                            self.emit_instr(wasm::Instr::ArrayCopy {
                                source_name: arr_type.clone(),
                                target_name: arr_type.clone(),
                            });

                            self.emit_instr(wasm::Instr::LocalGet {
                                local_name: offset_local.clone(),
                            });
                            self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Arr));
                            self.emit_instr(wasm::Instr::ArrayLen);
                            self.emit_instr(wasm::Instr::I32Add);
                            self.emit_instr(wasm::Instr::LocalSet {
                                local_name: offset_local.clone(),
                            });
                        }
                    }
                }
            }
            cont::Code::TplGet(tuple, index) => {
                let tpl_n_type = self.context.table().find_tpl_type(*index + 1);
                let field_name = self.context.table().tpl_field(*index);

                self.emit_instrs(
                    self.context
                        .load_value_instrs(tuple, LoadAs::Concrete(tpl_n_type.clone())),
                );

                self.emit_instr(wasm::Instr::StructGet {
                    type_name: tpl_n_type,
                    field_name,
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local,
                });
            }
        }
    }
}
