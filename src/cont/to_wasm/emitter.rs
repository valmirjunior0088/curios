use {
    super::{BlockData, Context, LoadAs, Scope},
    crate::{cont, wasm},
    std::collections::HashMap,
};

pub struct Emitter<'a, 'b> {
    context: Context<'a, 'b>,
    expr: &'b mut wasm::Expr,
}

impl<'a, 'b> Emitter<'a, 'b> {
    pub fn new(context: Context<'a, 'b>, expr: &'b mut wasm::Expr) -> Self {
        Self { context, expr }
    }

    pub fn emit_instr(&mut self, instr: wasm::Instr) {
        if let Some(scope) = self.context.this_scope() {
            scope.instrs.push(instr);
        } else {
            self.expr.push(instr);
        }
    }

    pub fn emit_instrs<I>(&mut self, instrs: I)
    where
        I: IntoIterator<Item = wasm::Instr>,
    {
        if let Some(scope) = self.context.this_scope() {
            scope.instrs.extend(instrs);
        } else {
            self.expr.extend(instrs);
        }
    }

    pub fn leave_last_scope(&mut self) {
        let instrs = self.context.leave_scope();

        if self.context.this_scope().is_some() {
            panic!("`Emitter` expected empty scope stack after leaving root");
        }

        self.expr.extend(instrs);
    }

    pub fn emit_const_value(&mut self, value: &cont::ConstValue) {
        match value {
            &cont::ConstValue::Int(value) => {
                self.emit_instrs([wasm::Instr::I32Const { value }, wasm::Instr::RefI31])
            }
            &cont::ConstValue::Flt(value) => self.emit_instrs([
                wasm::Instr::F32Const { value },
                wasm::Instr::StructNew {
                    type_name: self.context.metadata().flt_type(),
                },
            ]),
        }
    }

    pub fn emit_const_op(&mut self, op: &'a cont::ConstOp, params: &'a [cont::ValueName]) {
        match (op, params) {
            (cont::ConstOp::IntEql, [left, right]) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Eq);
                self.emit_instr(wasm::Instr::RefI31);
            }
            (cont::ConstOp::IntAdd, [left, right]) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Add);
                self.emit_instr(wasm::Instr::RefI31);
            }
            (cont::ConstOp::IntSub, [left, right]) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Sub);
                self.emit_instr(wasm::Instr::RefI31);
            }
            (cont::ConstOp::IntMul, [left, right]) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Int));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Int));
                self.emit_instr(wasm::Instr::I32Mul);
                self.emit_instr(wasm::Instr::RefI31);
            }
            (cont::ConstOp::FltAdd, [left, right]) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Add);

                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.metadata().flt_type(),
                });
            }
            (cont::ConstOp::FltSub, [left, right]) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Sub);

                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.metadata().flt_type(),
                });
            }
            (cont::ConstOp::FltMul, [left, right]) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Mul);

                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.metadata().flt_type(),
                });
            }
            (op, params) => panic!(
                "`Emitter` did not expect {} params for const op `{op:?}`",
                params.len()
            ),
        }
    }

    pub fn emit_clsr(&mut self, target: &'a cont::ClsrName, fields: &'a [cont::ValueName]) {
        self.emit_instrs([wasm::Instr::RefFunc {
            func_name: self.context.metadata().find_clsr(target).func_name(),
        }]);

        for field in fields {
            self.emit_instrs(self.context.load_value_instrs(field, LoadAs::NonNull));
        }

        self.emit_instr(wasm::Instr::StructNew {
            type_name: self.context.metadata().find_clsr(target).envr_type(),
        });
    }

    pub fn emit_value(&mut self, value: &'a cont::Value) {
        match value {
            cont::Value::Pure(value) => self.emit_const_value(value),
            cont::Value::Eval(op, params) => self.emit_const_op(op, params),
            cont::Value::Clsr(target, fields) => self.emit_clsr(target, fields),
        }
    }

    pub fn emit_let_values(&mut self, values: &'a [(cont::ValueName, cont::Value)]) {
        for (value_name, value) in values {
            self.emit_value(value);

            self.emit_instr(wasm::Instr::LocalSet {
                local_name: self
                    .context
                    .find_local(value_name)
                    .map(|(local_name, _)| local_name)
                    .expect(&format!("`Emitter` lacks local `{}`", value_name.string)),
            });
        }
    }

    pub fn emit_let_blocks(
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

                (block_data.label_name.clone(), self.context.leave_scope())
            })
            .collect();

        self.emit_instrs(self.context.flow_instrs(
            dispatcher_local,
            dispatcher_label,
            regions,
            tail,
        ));
    }

    pub fn emit_region(
        &mut self,
        params: HashMap<&'a cont::ValueName, (wasm::LocalName, bool)>,
        region: &'a cont::Region,
    ) {
        let values = region
            .values
            .iter()
            .map(|(value_name, _)| {
                let local_name = self.context.push_local(
                    &value_name.string,
                    self.context.metadata().obj_val_type(false),
                );

                (value_name, local_name)
            })
            .collect();

        if region.blocks.is_empty() {
            self.context.enter_scope(Scope::new(params, values, vec![]));
            self.emit_let_values(&region.values);
            self.emit_instrs(self.context.tail_instrs(&region.tail));
        } else {
            let dispatcher_local = self
                .context
                .push_local("", wasm::ValType::Num(wasm::NumType::I32));

            let dispatcher_label = wasm::LabelName::from(&dispatcher_local.string);

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
                                &value_name.string,
                                self.context.metadata().obj_val_type(true),
                            );

                            (value_name, (local_name, true))
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
                .enter_scope(Scope::new(params, values, blocks.clone()));

            self.emit_let_values(&region.values);
            self.emit_let_blocks(dispatcher_local, dispatcher_label, blocks, &region.tail);
        }
    }

    pub fn emit_root_region(&mut self, region: &'a cont::Region) {
        self.emit_region(self.context.params(), region);

        self.leave_last_scope();
    }
}
