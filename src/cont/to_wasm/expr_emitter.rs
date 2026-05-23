use {
    super::{BlockData, Context, Frame, LoadAs, LocalData},
    crate::{cont, wasm},
    std::collections::HashMap,
};

#[derive(Debug)]
pub struct ExprEmitter<'a, 'b> {
    context: Context<'a, 'b>,
    module: &'b mut wasm::Module,
    expr: &'b mut wasm::Expr,
}

impl<'a, 'b> ExprEmitter<'a, 'b> {
    pub fn new(
        context: Context<'a, 'b>,
        module: &'b mut wasm::Module,
        expr: &'b mut wasm::Expr,
    ) -> Self {
        Self {
            context,
            module,
            expr,
        }
    }

    fn emit_instr(&mut self, instr: wasm::Instr) {
        if let Some(frame) = self.context.this_frame() {
            frame.instrs.push(instr);
        } else {
            self.expr.push(instr);
        }
    }

    fn emit_instrs<I>(&mut self, instrs: I)
    where
        I: IntoIterator<Item = wasm::Instr>,
    {
        if let Some(frame) = self.context.this_frame() {
            frame.instrs.extend(instrs);
        } else {
            self.expr.extend(instrs);
        }
    }

    fn leave_last_frame(&mut self) {
        let instrs = self.context.leave_frame();

        if self.context.this_frame().is_some() {
            panic!("`ExprEmitter` expected empty frame stack after leaving root");
        }

        self.expr.extend(instrs);
    }

    pub fn emit_data(&mut self, value_name: &'a cont::ValueName, value: &'a cont::Data) {
        match value {
            &cont::Data::Nat(value) => self.emit_instrs([
                wasm::Instr::I32Const {
                    value: value as i32,
                },
                wasm::Instr::RefI31,
            ]),
            &cont::Data::Int(value) => {
                self.emit_instrs([wasm::Instr::I32Const { value }, wasm::Instr::RefI31])
            }
            &cont::Data::Flt(value) => self.emit_instrs([
                wasm::Instr::F32Const { value },
                wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                },
            ]),
            cont::Data::Arr(elems) => {
                let arr_type = self.context.table().arr_type();

                for elem in elems {
                    self.emit_instrs(self.context.load_value_instrs(elem, LoadAs::Null));
                }

                self.emit_instr(wasm::Instr::ArrayNewFixed {
                    type_name: arr_type,
                    length: elems.len() as u32,
                });
            }
            cont::Data::Tpl(elems) => {
                let tpl_n_type = self.context.table().find_tpl_type(elems.len());

                for elem in elems {
                    self.emit_instrs(self.context.load_value_instrs(elem, LoadAs::Null));
                }

                self.emit_instr(wasm::Instr::StructNew {
                    type_name: tpl_n_type,
                });
            }
            cont::Data::Bin(bytes) => {
                let bin_type = self.context.table().bin_type();
                let data_name = wasm::DataName::from(value_name.as_string());
                self.module.add_data(
                    data_name.clone(),
                    wasm::DataSegment {
                        bytes: bytes.clone(),
                    },
                );
                self.emit_instr(wasm::Instr::I32Const { value: 0 });
                self.emit_instr(wasm::Instr::I32Const {
                    value: bytes.len() as i32,
                });
                self.emit_instr(wasm::Instr::ArrayNewData {
                    type_name: bin_type,
                    data_name,
                });
            }
            cont::Data::Clsr(target, fields) => {
                let clsr_data = self.context.table().find_clsr(target);
                let envr_type = clsr_data.envr_type();

                self.emit_instr(wasm::Instr::RefFunc {
                    func_name: clsr_data.func_name(),
                });

                for field in fields {
                    self.emit_instrs(self.context.load_value_instrs(field, LoadAs::Null));
                }

                self.emit_instr(wasm::Instr::StructNew {
                    type_name: envr_type,
                });
            }
        }
    }

    fn emit_code(&mut self, value_name: &'a cont::ValueName, op: &'a cont::Code) {
        let result_local = self
            .context
            .find_local(value_name)
            .map(|ld| ld.local_name)
            .unwrap_or_else(|| panic!("`ExprEmitter` lacks local `{}`", value_name));

        match op {
            cont::Code::NatEql(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Eq);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatNeq(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Ne);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatAdd(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Add);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatSub(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Sub);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatMul(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Mul);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatLt(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32LtU);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatDiv(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32DivU);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatRem(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32RemU);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatGt(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32GtU);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatLte(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32LeU);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatGte(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32GeU);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntEql(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Eq);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntNeq(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Ne);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntAdd(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Add);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntSub(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Sub);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntMul(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Mul);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntDiv(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32DivS);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntRem(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32RemS);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntLt(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32LtS);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntGt(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32GtS);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntLte(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32LeS);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntGte(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32GeS);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatAnd(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32And);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatOr(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Or);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatXor(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Xor);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatShl(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Shl);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatShr(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32ShrU);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatRotl(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Rotl);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatRotr(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Rotr);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatClz(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Clz);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatCtz(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Ctz);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatPopcnt(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Popcnt);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatEqz(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Eqz);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntAnd(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32And);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntOr(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Or);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntXor(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Xor);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntShl(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Shl);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntShr(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32ShrS);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntRotl(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Rotl);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntRotr(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Rotr);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntClz(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Clz);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntCtz(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Ctz);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntPopcnt(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Popcnt);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::IntEqz(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Eqz);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltAdd(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Add);

                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltSub(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Sub);

                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltMul(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Mul);

                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltDiv(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Div);
                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltEql(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Eq);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltNeq(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Ne);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltLt(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Lt);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltGt(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Gt);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltLte(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Le);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltGte(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Ge);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltMin(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Min);
                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltMax(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Max);
                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltNeg(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Neg);
                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltAbs(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Abs);
                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltSqrt(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Sqrt);
                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltFloor(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Floor);
                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltCeil(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Ceil);
                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltTrunc(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Trunc);
                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltNearest(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Nearest);
                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltCopysign(left, right) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Copysign);
                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatToStr(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(wasm::Instr::Call {
                    func_name: self.context.table().nat_to_str_func().clone(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatToInt(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::NatToFlt(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(wasm::Instr::F32ConvertI32U);
                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
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
            cont::Code::IntToFlt(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Int));
                self.emit_instr(wasm::Instr::F32ConvertI32S);
                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltToStr(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(wasm::Instr::Call {
                    func_name: self.context.table().flt_to_str_func().clone(),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltToNat(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(wasm::Instr::I32TruncF32U);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::FltToInt(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Flt));
                self.emit_instr(wasm::Instr::I32TruncF32S);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::BinLen(bin) => {
                self.emit_instrs(self.context.load_value_instrs(bin, LoadAs::Bin));
                self.emit_instr(wasm::Instr::ArrayLen);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
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
                self.emit_instrs(self.context.load_value_instrs(idx, LoadAs::Int));
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
                self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Int));
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
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Int));
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
                self.emit_instrs(self.context.load_value_instrs(byte, LoadAs::Int));
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
            cont::Code::ArrLen(lst) => {
                self.emit_instrs(self.context.load_value_instrs(lst, LoadAs::Arr));
                self.emit_instr(wasm::Instr::ArrayLen);
                self.emit_instr(wasm::Instr::RefI31);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::ArrGet(lst, idx) => {
                let arr_type = self.context.table().arr_type();
                self.emit_instrs(self.context.load_value_instrs(lst, LoadAs::Arr));
                self.emit_instrs(self.context.load_value_instrs(idx, LoadAs::Int));
                self.emit_instr(wasm::Instr::ArrayGet {
                    type_name: arr_type,
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local.clone(),
                });
            }
            cont::Code::ArrSlice(lst, start, end) => {
                let arr_type = self.context.table().arr_type();

                self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Int));
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
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(end, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(start, LoadAs::Int));
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
                self.emit_proj(tuple, *index);
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local,
                });
            }
            cont::Code::SysPrint(operand) => {
                self.emit_instrs(self.context.load_value_instrs(operand, LoadAs::Bin));
                self.emit_instr(wasm::Instr::Call {
                    func_name: self.context.table().sys_print_func().clone(),
                });
                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().find_tpl_type(0),
                });
                self.emit_instr(wasm::Instr::LocalSet {
                    local_name: result_local,
                });
            }
        }
    }

    fn emit_proj(&mut self, tuple: &'a cont::ValueName, index: usize) {
        let tpl_n_type = self.context.table().find_tpl_type(index + 1);
        let field_name = self.context.table().tpl_field(index);

        self.emit_instrs(
            self.context
                .load_value_instrs(tuple, LoadAs::Concrete(tpl_n_type.clone())),
        );

        self.emit_instr(wasm::Instr::StructGet {
            type_name: tpl_n_type,
            field_name,
        });
    }

    fn emit_preallocate_tpl(&mut self, value_name: &'a cont::ValueName, arity: usize) {
        self.emit_instr(wasm::Instr::StructNewDefault {
            type_name: self.context.table().find_tpl_type(arity),
        });

        self.emit_instr(wasm::Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .unwrap_or_else(|| panic!("`ExprEmitter` lacks local `{}`", value_name)),
        });
    }

    fn emit_preallocate_clsr(
        &mut self,
        value_name: &'a cont::ValueName,
        target: &'a cont::ClsrName,
    ) {
        self.emit_instr(wasm::Instr::StructNewDefault {
            type_name: self.context.table().find_clsr(target).envr_type(),
        });

        self.emit_instr(wasm::Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .unwrap_or_else(|| panic!("`ExprEmitter` lacks local `{}`", value_name)),
        });
    }

    fn emit_let_pure(&mut self, value_name: &'a cont::ValueName, value: &'a cont::Data) {
        self.emit_data(value_name, value);

        self.emit_instr(wasm::Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .unwrap_or_else(|| panic!("`ExprEmitter` lacks local `{}`", value_name)),
        });
    }

    fn emit_backpatch_clsr(
        &mut self,
        value_name: &'a cont::ValueName,
        target: &'a cont::ClsrName,
        fields: &'a [cont::ValueName],
    ) {
        let clsr_data = self.context.table().find_clsr(target);
        let envr_type = clsr_data.envr_type();

        self.emit_instrs(
            self.context
                .load_value_instrs(value_name, LoadAs::Concrete(envr_type.clone())),
        );
        self.emit_instr(wasm::Instr::RefFunc {
            func_name: clsr_data.func_name(),
        });

        self.emit_instr(wasm::Instr::StructSet {
            type_name: envr_type.clone(),
            field_name: self.context.table().special_field(),
        });

        for (field, field_name) in fields.iter().zip(clsr_data.fields()) {
            self.emit_instrs(
                self.context
                    .load_value_instrs(value_name, LoadAs::Concrete(envr_type.clone())),
            );

            self.emit_instrs(self.context.load_value_instrs(field, LoadAs::Null));

            self.emit_instr(wasm::Instr::StructSet {
                type_name: envr_type.clone(),
                field_name,
            });
        }
    }

    fn emit_preallocate_arr(&mut self, value_name: &'a cont::ValueName, len: usize) {
        self.emit_instr(wasm::Instr::I32Const { value: len as i32 });

        self.emit_instr(wasm::Instr::ArrayNewDefault {
            type_name: self.context.table().arr_type(),
        });

        self.emit_instr(wasm::Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .unwrap_or_else(|| panic!("`ExprEmitter` lacks local `{}`", value_name)),
        });
    }

    fn emit_backpatch_arr(
        &mut self,
        value_name: &'a cont::ValueName,
        elems: &'a [cont::ValueName],
    ) {
        let arr_type = self.context.table().arr_type();

        for (index, elem) in elems.iter().enumerate() {
            self.emit_instrs(self.context.load_value_instrs(value_name, LoadAs::Arr));

            self.emit_instr(wasm::Instr::I32Const {
                value: index as i32,
            });
            self.emit_instrs(self.context.load_value_instrs(elem, LoadAs::Null));

            self.emit_instr(wasm::Instr::ArraySet {
                type_name: arr_type.clone(),
            });
        }
    }

    fn emit_backpatch_tpl(
        &mut self,
        value_name: &'a cont::ValueName,
        elems: &'a [cont::ValueName],
    ) {
        let tpl_n_type = self.context.table().find_tpl_type(elems.len());

        for (index, element) in elems.iter().enumerate() {
            self.emit_instrs(
                self.context
                    .load_value_instrs(value_name, LoadAs::Concrete(tpl_n_type.clone())),
            );

            self.emit_instrs(self.context.load_value_instrs(element, LoadAs::Null));

            self.emit_instr(wasm::Instr::StructSet {
                type_name: tpl_n_type.clone(),
                field_name: self.context.table().tpl_field(index),
            });
        }
    }

    fn emit_let_alias(&mut self, value_name: &'a cont::ValueName, source: &'a cont::ValueName) {
        self.emit_instrs(self.context.load_value_instrs(source, LoadAs::Null));

        self.emit_instr(wasm::Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .unwrap_or_else(|| panic!("`ExprEmitter` lacks local `{}`", value_name)),
        });
    }

    fn emit_let_values(&mut self, values: &'a [(cont::ValueName, cont::Value)]) {
        for (value_name, value) in values {
            match value {
                cont::Value::Pure(cont::Data::Arr(elems)) => {
                    self.emit_preallocate_arr(value_name, elems.len())
                }
                cont::Value::Pure(cont::Data::Tpl(elems)) => {
                    self.emit_preallocate_tpl(value_name, elems.len())
                }
                cont::Value::Pure(cont::Data::Clsr(target, _)) => {
                    self.emit_preallocate_clsr(value_name, target)
                }
                _ => {}
            }
        }

        for (value_name, value) in values {
            match value {
                cont::Value::Pure(cont::Data::Arr(elems)) => {
                    self.emit_backpatch_arr(value_name, elems)
                }
                cont::Value::Pure(cont::Data::Tpl(elems)) => {
                    self.emit_backpatch_tpl(value_name, elems)
                }
                cont::Value::Pure(cont::Data::Clsr(target, fields)) => {
                    self.emit_backpatch_clsr(value_name, target, fields)
                }
                cont::Value::Pure(value) => self.emit_let_pure(value_name, value),
                cont::Value::Eval(op) => self.emit_code(value_name, op),
                cont::Value::Alias(source) => self.emit_let_alias(value_name, source),
            }
        }
    }

    fn emit_let_blocks(
        &mut self,
        dispatcher_local: wasm::LocalName,
        dispatcher_label: wasm::LabelName,
        blocks: Vec<(&'a cont::BlockName, BlockData<'a>)>,
        tail: &'a cont::Tail,
    ) {
        let regions = blocks
            .iter()
            .map(|(_, block_data)| {
                self.emit_region(
                    block_data.params.iter().cloned().collect(),
                    block_data.region,
                );

                (block_data.label_name.clone(), self.context.leave_frame())
            })
            .collect();

        self.emit_instrs(self.context.flow_instrs(
            dispatcher_local,
            dispatcher_label,
            regions,
            tail,
        ));
    }

    fn emit_region(
        &mut self,
        params: HashMap<&'a cont::ValueName, LocalData>,
        region: &'a cont::Region,
    ) {
        let values = region
            .values
            .iter()
            .map(|(value_name, _)| {
                let local_name = self
                    .context
                    .push_local(value_name.as_str(), self.context.table().top_type(true));

                (value_name, local_name)
            })
            .collect();

        if region.blocks.is_empty() {
            self.context.enter_frame(Frame::new(params, values, vec![]));
            self.emit_let_values(&region.values);
            self.emit_instrs(self.context.tail_instrs(&region.tail));
        } else {
            let dispatcher_local = self
                .context
                .push_local("", wasm::ValType::Num(wasm::NumType::I32));

            let dispatcher_label = wasm::LabelName::from(dispatcher_local.as_str());

            let blocks = region
                .blocks
                .iter()
                .enumerate()
                .map(|(index, (block_name, block))| {
                    let block_params = block
                        .params
                        .iter()
                        .map(|value_name| {
                            let local_name = self.context.push_local(
                                value_name.as_str(),
                                self.context.table().top_type(true),
                            );

                            (value_name, LocalData::new(local_name, true))
                        })
                        .collect();

                    let block_data = BlockData::new(
                        dispatcher_label.clone(),
                        dispatcher_local.clone(),
                        index,
                        block_name,
                        block_params,
                        &block.region,
                    );

                    (block_name, block_data)
                })
                .collect::<Vec<_>>();

            self.context
                .enter_frame(Frame::new(params, values, blocks.clone()));

            self.emit_let_values(&region.values);
            self.emit_let_blocks(dispatcher_local, dispatcher_label, blocks, &region.tail);
        }
    }

    pub fn emit_root_region(&mut self, region: &'a cont::Region) {
        self.emit_region(self.context.params(), region);
        self.leave_last_frame();
    }
}
