use {
    super::{BlockData, Context, Frame, LoadAs, LocalData},
    crate::{cont, wasm},
    std::collections::HashMap,
};

#[derive(Debug)]
pub struct ExprEmitter<'a, 'b> {
    context: Context<'a, 'b>,
    expr: &'b mut wasm::Expr,
}

impl<'a, 'b> ExprEmitter<'a, 'b> {
    pub fn new(context: Context<'a, 'b>, expr: &'b mut wasm::Expr) -> Self {
        Self { context, expr }
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

    pub fn emit_const_value(&mut self, value: &cont::ConstValue) {
        match value {
            cont::ConstValue::Unit => self.emit_instr(wasm::Instr::StructNew {
                type_name: self.context.table().unit_type(),
            }),
            &cont::ConstValue::Int(value) => {
                self.emit_instrs([wasm::Instr::I32Const { value }, wasm::Instr::RefI31])
            }
            &cont::ConstValue::Flt(value) => self.emit_instrs([
                wasm::Instr::F32Const { value },
                wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                },
            ]),
        }
    }

    fn emit_const_op(&mut self, op: &'a cont::ConstOp, params: &'a [cont::ValueName]) {
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
                    type_name: self.context.table().flt_type(),
                });
            }
            (cont::ConstOp::FltSub, [left, right]) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Sub);

                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
            }
            (cont::ConstOp::FltMul, [left, right]) => {
                self.emit_instrs(self.context.load_value_instrs(left, LoadAs::Flt));
                self.emit_instrs(self.context.load_value_instrs(right, LoadAs::Flt));
                self.emit_instr(wasm::Instr::F32Mul);

                self.emit_instr(wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                });
            }
            (op, params) => panic!(
                "`ExprEmitter` did not expect {} params for const op `{op:?}`",
                params.len()
            ),
        }
    }

    fn emit_proj(&mut self, tuple: &'a cont::ValueName, index: usize) {
        let field_name = match index {
            0 => self.context.table().proj_fst_field(),
            1 => self.context.table().proj_snd_field(),
            index => panic!("`ExprEmitter` expected tuple projection index 0 or 1, found {index}"),
        };

        self.emit_instrs(
            self.context
                .load_value_instrs(tuple, LoadAs::Concrete(self.context.table().tpl2_type())),
        );

        self.emit_instr(wasm::Instr::StructGet {
            type_name: self.context.table().tpl2_type(),
            field_name,
        });
    }

    fn emit_preallocate_tpl2(&mut self, value_name: &'a cont::ValueName) {
        self.emit_instr(wasm::Instr::StructNewDefault {
            type_name: self.context.table().tpl2_type(),
        });

        self.emit_instr(wasm::Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .expect(&format!(
                    "`ExprEmitter` lacks local `{}`",
                    value_name.string
                )),
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
                .expect(&format!(
                    "`ExprEmitter` lacks local `{}`",
                    value_name.string
                )),
        });
    }

    fn emit_let_pure(&mut self, value_name: &'a cont::ValueName, value: &cont::ConstValue) {
        self.emit_const_value(value);

        self.emit_instr(wasm::Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .expect(&format!(
                    "`ExprEmitter` lacks local `{}`",
                    value_name.string
                )),
        });
    }

    fn emit_let_eval(
        &mut self,
        value_name: &'a cont::ValueName,
        op: &'a cont::ConstOp,
        params: &'a [cont::ValueName],
    ) {
        self.emit_const_op(op, params);

        self.emit_instr(wasm::Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .expect(&format!(
                    "`ExprEmitter` lacks local `{}`",
                    value_name.string
                )),
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

            self.emit_instrs(self.context.load_value_instrs(field, LoadAs::Raw));

            self.emit_instr(wasm::Instr::StructSet {
                type_name: envr_type.clone(),
                field_name,
            });
        }
    }

    fn emit_backpatch_tpl2(
        &mut self,
        value_name: &'a cont::ValueName,
        first: &'a cont::ValueName,
        second: &'a cont::ValueName,
    ) {
        self.emit_instrs(self.context.load_value_instrs(
            value_name,
            LoadAs::Concrete(self.context.table().tpl2_type()),
        ));

        self.emit_instrs(self.context.load_value_instrs(first, LoadAs::Raw));

        self.emit_instr(wasm::Instr::StructSet {
            type_name: self.context.table().tpl2_type(),
            field_name: self.context.table().proj_fst_field(),
        });

        self.emit_instrs(self.context.load_value_instrs(
            value_name,
            LoadAs::Concrete(self.context.table().tpl2_type()),
        ));

        self.emit_instrs(self.context.load_value_instrs(second, LoadAs::Raw));

        self.emit_instr(wasm::Instr::StructSet {
            type_name: self.context.table().tpl2_type(),
            field_name: self.context.table().proj_snd_field(),
        });
    }

    fn emit_let_proj(
        &mut self,
        value_name: &'a cont::ValueName,
        tuple: &'a cont::ValueName,
        index: usize,
    ) {
        self.emit_proj(tuple, index);

        self.emit_instr(wasm::Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .expect(&format!(
                    "`ExprEmitter` lacks local `{}`",
                    value_name.string
                )),
        });
    }

    fn emit_let_alias(&mut self, value_name: &'a cont::ValueName, source: &'a cont::ValueName) {
        self.emit_instrs(self.context.load_value_instrs(source, LoadAs::Raw));

        self.emit_instr(wasm::Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .expect(&format!(
                    "`ExprEmitter` lacks local `{}`",
                    value_name.string
                )),
        });
    }

    fn emit_let_values(&mut self, values: &'a [(cont::ValueName, cont::Value)]) {
        for (value_name, value) in values {
            match value {
                cont::Value::Tpl2(_, _) => self.emit_preallocate_tpl2(value_name),
                cont::Value::Clsr(target, _) => self.emit_preallocate_clsr(value_name, target),
                _ => {}
            }
        }

        for (value_name, value) in values {
            match value {
                cont::Value::Pure(value) => self.emit_let_pure(value_name, value),
                cont::Value::Eval(op, params) => self.emit_let_eval(value_name, op, params),
                cont::Value::Clsr(target, fields) => {
                    self.emit_backpatch_clsr(value_name, target, fields)
                }
                cont::Value::Tpl2(first, second) => {
                    self.emit_backpatch_tpl2(value_name, first, second)
                }
                cont::Value::Proj(tuple, index) => self.emit_let_proj(value_name, tuple, *index),
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
                    .push_local(&value_name.string, self.context.table().top_type(true));

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
