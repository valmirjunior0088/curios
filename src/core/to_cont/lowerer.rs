use {
    super::{Entropy, Frame, FrameEntropy},
    crate::{cont, core},
};

fn unsupported_letrec_item(term: &core::ErasedTerm) -> ! {
    panic!(
        "`to_cont` does not support this `let rec` item in the MVP: \
         recursive RHSs must lower directly to a `cont::Value`, but the following term \
         requires value-level knot tying in `cont` (for example alias/cell/fixpoint support): \
         {term:?}",
    )
}

fn emit_fresh_value(
    state: &mut FrameEntropy,
    builder: &mut RegionBuilder,
    value: cont::Value,
) -> cont::ValueName {
    let name = state.fresh_value();
    builder.add_value(name.clone(), value);

    name
}

struct RegionBuilder {
    values: Vec<(cont::ValueName, cont::Value)>,
    blocks: Vec<(cont::BlockName, cont::Block)>,
}

impl RegionBuilder {
    fn new() -> Self {
        Self {
            values: vec![],
            blocks: vec![],
        }
    }

    fn add_value(&mut self, name: cont::ValueName, value: cont::Value) {
        self.values.push((name, value));
    }

    fn add_block(&mut self, name: cont::BlockName, block: cont::Block) {
        self.blocks.push((name, block));
    }

    fn finish(self, tail: cont::Tail) -> cont::Region {
        cont::Region {
            values: self.values,
            blocks: self.blocks,
            tail,
        }
    }
}

#[derive(Debug)]
pub struct Lowerer<'a> {
    pub module: &'a mut cont::Module,
    clsrs: Entropy<cont::ClsrName>,
}

impl<'a> Lowerer<'a> {
    pub fn new(module: &'a mut cont::Module) -> Self {
        Self {
            module,
            clsrs: Entropy::<cont::ClsrName>::new(),
        }
    }

    pub fn lower_closure(
        &mut self,
        func: &core::ErasedFunc,
        frame: &Frame,
    ) -> (cont::ClsrName, Vec<cont::ValueName>) {
        let clsr_name = self.clsrs.fresh();
        let (mut entry, resume) = FrameEntropy::new();
        let mut clsr_frame = Frame::new();

        let fields = func
            .captures
            .iter()
            .map(|capture| {
                let field = entry.fresh_value();
                clsr_frame.push(capture.clone(), field.clone());

                field
            })
            .collect::<Vec<_>>();

        let param = entry.fresh_value();
        clsr_frame.push(func.param.clone(), param.clone());

        let mut builder = RegionBuilder::new();
        let tail = self.lower_tail(&func.body, &clsr_frame, &resume, &mut entry, &mut builder);

        self.module.add_clsr(
            clsr_name.clone(),
            cont::Clsr {
                fields: fields.clone(),
                params: vec![param],
                resume,
                region: builder.finish(tail),
            },
        );

        let captured_values = func
            .captures
            .iter()
            .map(|capture| frame.find(capture))
            .collect();

        (clsr_name, captured_values)
    }

    pub fn lower_entry(
        &mut self,
        term: &core::ErasedTerm,
        frame: &Frame,
    ) -> (cont::BlockName, cont::Region) {
        let (mut entry, resume) = FrameEntropy::new();
        let mut builder = RegionBuilder::new();
        let tail = self.lower_tail(term, frame, &resume, &mut entry, &mut builder);

        (resume, builder.finish(tail))
    }
}

impl<'a> Lowerer<'a> {
    fn lower_letrec_bindings(
        &mut self,
        letrec: &core::ErasedLetRec,
        frame: &Frame,
        state: &mut FrameEntropy,
        builder: &mut RegionBuilder,
    ) -> Frame {
        let mut frame = frame.clone();
        let reserved = letrec
            .names
            .iter()
            .map(|name| {
                let reserved = state.fresh_value();
                frame.push(name.clone(), reserved.clone());

                reserved
            })
            .collect::<Vec<_>>();

        letrec
            .items
            .iter()
            .zip(reserved)
            .for_each(|(item, target)| {
                self.lower_letrec_item(item, target, &frame, state, builder)
            });

        frame
    }

    fn lower_letrec_name(
        &mut self,
        term: &core::ErasedTerm,
        frame: &Frame,
        state: &mut FrameEntropy,
        builder: &mut RegionBuilder,
    ) -> cont::ValueName {
        match term {
            core::ErasedTerm::Name(name) => frame.find(&name.string),
            core::ErasedTerm::Prim(core::ErasedPrim::Unit) => {
                emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Unit))
            }
            core::ErasedTerm::Prim(core::ErasedPrim::Bln(value)) => emit_fresh_value(
                state,
                builder,
                cont::Value::Pure(cont::Data::Bln(*value)),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::BlnNot(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::BlnNot(operand)),
                )
            }
            core::ErasedTerm::Prim(core::ErasedPrim::BlnAnd(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::BlnAnd(left, right)),
                )
            }
            core::ErasedTerm::Prim(core::ErasedPrim::BlnOr(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::BlnOr(left, right)),
                )
            }
            core::ErasedTerm::Prim(core::ErasedPrim::Nat(value)) => emit_fresh_value(
                state,
                builder,
                cont::Value::Pure(cont::Data::Nat(*value)),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::NatEql(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatEql(left, right)),
                )
            }
            core::ErasedTerm::Prim(core::ErasedPrim::NatAdd(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatAdd(left, right)),
                )
            }
            core::ErasedTerm::Prim(core::ErasedPrim::NatSub(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatSub(left, right)),
                )
            }
            core::ErasedTerm::Prim(core::ErasedPrim::NatMul(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatMul(left, right)),
                )
            }
            core::ErasedTerm::Prim(core::ErasedPrim::NatLt(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatLt(left, right)),
                )
            }
            core::ErasedTerm::Prim(core::ErasedPrim::Int(value)) => emit_fresh_value(
                state,
                builder,
                cont::Value::Pure(cont::Data::Int(*value)),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::Flt(value)) => emit_fresh_value(
                state,
                builder,
                cont::Value::Pure(cont::Data::Flt(*value)),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::IntEql(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntEql(left, right)),
                )
            }
            core::ErasedTerm::Prim(core::ErasedPrim::IntAdd(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntAdd(left, right)),
                )
            }
            core::ErasedTerm::Prim(core::ErasedPrim::IntSub(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntSub(left, right)),
                )
            }
            core::ErasedTerm::Prim(core::ErasedPrim::IntMul(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntMul(left, right)),
                )
            }
            core::ErasedTerm::Prim(core::ErasedPrim::FltAdd(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltAdd(left, right)),
                )
            }
            core::ErasedTerm::Prim(core::ErasedPrim::FltSub(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltSub(left, right)),
                )
            }
            core::ErasedTerm::Prim(core::ErasedPrim::FltMul(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltMul(left, right)),
                )
            }
            core::ErasedTerm::Func(func) => {
                let (clsr_name, captured_values) = self.lower_closure(func, frame);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Pure(cont::Data::Clsr(clsr_name, captured_values)),
                )
            }
            core::ErasedTerm::Pair(pair) => {
                let fst = self.lower_letrec_name(&pair.fst, frame, state, builder);
                let snd = self.lower_letrec_name(&pair.snd, frame, state, builder);

                emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Tpl(vec![fst, snd])))
            }
            core::ErasedTerm::Atom(atom) => emit_fresh_value(
                state,
                builder,
                cont::Value::Pure(cont::Data::Int(atom.index as i32)),
            ),
            core::ErasedTerm::Let(let_) => {
                let body = self.lower_letrec_name(&let_.body, frame, state, builder);
                let frame = frame.extended([(let_.name.clone(), body)]);

                self.lower_letrec_name(&let_.tail, &frame, state, builder)
            }
            core::ErasedTerm::LetRec(letrec) => {
                let frame = self.lower_letrec_bindings(letrec, frame, state, builder);

                self.lower_letrec_name(&letrec.tail, &frame, state, builder)
            }
            core::ErasedTerm::Apply(_)
            | core::ErasedTerm::Split(_)
            | core::ErasedTerm::Match(_) => unsupported_letrec_item(term),
        }
    }

    fn lower_letrec_item(
        &mut self,
        term: &core::ErasedTerm,
        target: cont::ValueName,
        frame: &Frame,
        state: &mut FrameEntropy,
        builder: &mut RegionBuilder,
    ) {
        match term {
            core::ErasedTerm::Prim(core::ErasedPrim::Unit) => {
                builder.add_value(target, cont::Value::Pure(cont::Data::Unit));
            }
            core::ErasedTerm::Prim(core::ErasedPrim::Bln(value)) => {
                builder.add_value(target, cont::Value::Pure(cont::Data::Bln(*value)));
            }
            core::ErasedTerm::Prim(core::ErasedPrim::BlnNot(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::BlnNot(operand)),
                );
            }
            core::ErasedTerm::Prim(core::ErasedPrim::BlnAnd(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::BlnAnd(left, right)),
                );
            }
            core::ErasedTerm::Prim(core::ErasedPrim::BlnOr(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::BlnOr(left, right)),
                );
            }
            core::ErasedTerm::Prim(core::ErasedPrim::Nat(value)) => {
                builder.add_value(target, cont::Value::Pure(cont::Data::Nat(*value)));
            }
            core::ErasedTerm::Prim(core::ErasedPrim::NatEql(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::NatEql(left, right)),
                );
            }
            core::ErasedTerm::Prim(core::ErasedPrim::NatAdd(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::NatAdd(left, right)),
                );
            }
            core::ErasedTerm::Prim(core::ErasedPrim::NatSub(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::NatSub(left, right)),
                );
            }
            core::ErasedTerm::Prim(core::ErasedPrim::NatMul(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::NatMul(left, right)),
                );
            }
            core::ErasedTerm::Prim(core::ErasedPrim::NatLt(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::NatLt(left, right)),
                );
            }
            core::ErasedTerm::Prim(core::ErasedPrim::Int(value)) => {
                builder.add_value(target, cont::Value::Pure(cont::Data::Int(*value)));
            }
            core::ErasedTerm::Prim(core::ErasedPrim::Flt(value)) => {
                builder.add_value(target, cont::Value::Pure(cont::Data::Flt(*value)));
            }
            core::ErasedTerm::Prim(core::ErasedPrim::IntEql(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::IntEql(left, right)),
                );
            }
            core::ErasedTerm::Prim(core::ErasedPrim::IntAdd(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::IntAdd(left, right)),
                );
            }
            core::ErasedTerm::Prim(core::ErasedPrim::IntSub(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::IntSub(left, right)),
                );
            }
            core::ErasedTerm::Prim(core::ErasedPrim::IntMul(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::IntMul(left, right)),
                );
            }
            core::ErasedTerm::Prim(core::ErasedPrim::FltAdd(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::FltAdd(left, right)),
                );
            }
            core::ErasedTerm::Prim(core::ErasedPrim::FltSub(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::FltSub(left, right)),
                );
            }
            core::ErasedTerm::Prim(core::ErasedPrim::FltMul(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::FltMul(left, right)),
                );
            }
            core::ErasedTerm::Func(func) => {
                let (clsr_name, captured_values) = self.lower_closure(func, frame);
                builder.add_value(target, cont::Value::Pure(cont::Data::Clsr(clsr_name, captured_values)));
            }
            core::ErasedTerm::Pair(pair) => {
                let fst = self.lower_letrec_name(&pair.fst, frame, state, builder);
                let snd = self.lower_letrec_name(&pair.snd, frame, state, builder);
                builder.add_value(target, cont::Value::Pure(cont::Data::Tpl(vec![fst, snd])));
            }
            core::ErasedTerm::Atom(atom) => {
                builder.add_value(
                    target,
                    cont::Value::Pure(cont::Data::Int(atom.index as i32)),
                );
            }
            core::ErasedTerm::Let(let_) => {
                let body = self.lower_letrec_name(&let_.body, frame, state, builder);
                let frame = frame.extended([(let_.name.clone(), body)]);
                self.lower_letrec_item(&let_.tail, target, &frame, state, builder);
            }
            core::ErasedTerm::LetRec(letrec) => {
                let frame = self.lower_letrec_bindings(letrec, frame, state, builder);
                self.lower_letrec_item(&letrec.tail, target, &frame, state, builder);
            }
            core::ErasedTerm::Name(name) => {
                builder.add_value(target, cont::Value::Name(frame.find(&name.string)));
            }
            core::ErasedTerm::Apply(_)
            | core::ErasedTerm::Split(_)
            | core::ErasedTerm::Match(_) => unsupported_letrec_item(term),
        }
    }
}

type Cont<'a> = Box<
    dyn FnOnce(
            &mut Lowerer<'_>,
            &mut FrameEntropy,
            &mut RegionBuilder,
            cont::ValueName,
        ) -> cont::Tail
        + 'a,
>;

impl<'a> Lowerer<'a> {
    fn lower_to_name(
        &mut self,
        term: &core::ErasedTerm,
        frame: &Frame,
        state: &mut FrameEntropy,
        builder: &mut RegionBuilder,
        cont: Cont<'_>,
    ) -> cont::Tail {
        match term {
            core::ErasedTerm::Name(name) => cont(self, state, builder, frame.find(&name.string)),
            core::ErasedTerm::Prim(core::ErasedPrim::Unit) => {
                let value =
                    emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Unit));

                cont(self, state, builder, value)
            }
            core::ErasedTerm::Prim(core::ErasedPrim::Bln(value)) => {
                let value = emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Pure(cont::Data::Bln(*value)),
                );

                cont(self, state, builder, value)
            }
            core::ErasedTerm::Prim(core::ErasedPrim::BlnNot(operand)) => self.lower_to_name(
                operand,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, operand| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::BlnNot(operand)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::BlnAnd(left, right)) => self.lower_to_name(
                left,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, left| {
                    this.lower_to_name(
                        right,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, right| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::BlnAnd(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::BlnOr(left, right)) => self.lower_to_name(
                left,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, left| {
                    this.lower_to_name(
                        right,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, right| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::BlnOr(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::Nat(value)) => {
                let value = emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Pure(cont::Data::Nat(*value)),
                );

                cont(self, state, builder, value)
            }
            core::ErasedTerm::Prim(core::ErasedPrim::NatEql(left, right)) => self.lower_to_name(
                left,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, left| {
                    this.lower_to_name(
                        right,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, right| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::NatEql(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::NatAdd(left, right)) => self.lower_to_name(
                left,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, left| {
                    this.lower_to_name(
                        right,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, right| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::NatAdd(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::NatSub(left, right)) => self.lower_to_name(
                left,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, left| {
                    this.lower_to_name(
                        right,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, right| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::NatSub(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::NatMul(left, right)) => self.lower_to_name(
                left,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, left| {
                    this.lower_to_name(
                        right,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, right| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::NatMul(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::NatLt(left, right)) => self.lower_to_name(
                left,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, left| {
                    this.lower_to_name(
                        right,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, right| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::NatLt(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::Int(value)) => {
                let value = emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Pure(cont::Data::Int(*value)),
                );

                cont(self, state, builder, value)
            }
            core::ErasedTerm::Prim(core::ErasedPrim::Flt(value)) => {
                let value = emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Pure(cont::Data::Flt(*value)),
                );

                cont(self, state, builder, value)
            }
            core::ErasedTerm::Prim(core::ErasedPrim::IntEql(left, right)) => self.lower_to_name(
                left,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, left| {
                    this.lower_to_name(
                        right,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, right| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::IntEql(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::IntAdd(left, right)) => self.lower_to_name(
                left,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, left| {
                    this.lower_to_name(
                        right,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, right| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::IntAdd(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::IntSub(left, right)) => self.lower_to_name(
                left,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, left| {
                    this.lower_to_name(
                        right,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, right| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::IntSub(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::IntMul(left, right)) => self.lower_to_name(
                left,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, left| {
                    this.lower_to_name(
                        right,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, right| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::IntMul(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::FltAdd(left, right)) => self.lower_to_name(
                left,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, left| {
                    this.lower_to_name(
                        right,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, right| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::FltAdd(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::FltSub(left, right)) => self.lower_to_name(
                left,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, left| {
                    this.lower_to_name(
                        right,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, right| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::FltSub(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Prim(core::ErasedPrim::FltMul(left, right)) => self.lower_to_name(
                left,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, left| {
                    this.lower_to_name(
                        right,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, right| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::FltMul(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Func(func) => {
                let (clsr_name, captured_values) = self.lower_closure(func, frame);
                let value = emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Pure(cont::Data::Clsr(clsr_name, captured_values)),
                );

                cont(self, state, builder, value)
            }
            core::ErasedTerm::Pair(pair) => self.lower_to_name(
                &pair.fst,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, fst| {
                    this.lower_to_name(
                        &pair.snd,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, snd| {
                            let value =
                                emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Tpl(vec![fst, snd])));

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Atom(atom) => {
                let value = emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Pure(cont::Data::Int(atom.index as i32)),
                );

                cont(self, state, builder, value)
            }
            core::ErasedTerm::Let(let_) => {
                let name = let_.name.clone();

                self.lower_to_name(
                    &let_.body,
                    frame,
                    state,
                    builder,
                    Box::new(move |this, state, builder, body| {
                        let frame = frame.extended([(name, body)]);

                        this.lower_to_name(&let_.tail, &frame, state, builder, cont)
                    }),
                )
            }
            core::ErasedTerm::LetRec(letrec) => {
                let frame = self.lower_letrec_bindings(letrec, frame, state, builder);
                self.lower_to_name(&letrec.tail, &frame, state, builder, cont)
            }
            core::ErasedTerm::Apply(_)
            | core::ErasedTerm::Split(_)
            | core::ErasedTerm::Match(_) => {
                let block = state.fresh_block();
                let param = state.fresh_value();
                let mut join_builder = RegionBuilder::new();
                let join_tail = cont(self, state, &mut join_builder, param.clone());

                builder.add_block(
                    block.clone(),
                    cont::Block {
                        params: vec![param],
                        region: join_builder.finish(join_tail),
                    },
                );

                self.lower_tail(term, frame, &block, state, builder)
            }
        }
    }

    fn lower_tail(
        &mut self,
        term: &core::ErasedTerm,
        frame: &Frame,
        resume: &cont::BlockName,
        state: &mut FrameEntropy,
        builder: &mut RegionBuilder,
    ) -> cont::Tail {
        match term {
            core::ErasedTerm::Apply(apply) => self.lower_to_name(
                &apply.head,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, head| {
                    this.lower_to_name(
                        &apply.param,
                        frame,
                        state,
                        builder,
                        Box::new(move |_, _, _, param| {
                            cont::Tail::Call(cont::CallTarget::Indirect {
                                target: head,
                                params: vec![param],
                                resume: resume.clone(),
                            })
                        }),
                    )
                }),
            ),
            core::ErasedTerm::Match(match_) => self.lower_to_name(
                &match_.head,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, head| {
                    let mut targets = Vec::with_capacity(match_.cases.len());

                    for case in &match_.cases {
                        let block = state.fresh_block();
                        let mut case_builder = RegionBuilder::new();
                        let tail = this.lower_tail(case, frame, resume, state, &mut case_builder);

                        builder.add_block(
                            block.clone(),
                            cont::Block {
                                params: vec![],
                                region: case_builder.finish(tail),
                            },
                        );

                        targets.push(cont::JumpTarget {
                            target: block,
                            params: vec![],
                        });
                    }

                    cont::Tail::Case(cont::CaseTarget {
                        operand: head,
                        targets,
                        default: None,
                    })
                }),
            ),
            core::ErasedTerm::Split(split) => self.lower_to_name(
                &split.head,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, head| {
                    let fst = state.fresh_value();
                    builder.add_value(fst.clone(), cont::Value::Eval(cont::Code::TplProj(0, head.clone())));

                    let snd = state.fresh_value();
                    builder.add_value(snd.clone(), cont::Value::Eval(cont::Code::TplProj(1, head)));

                    let frame =
                        frame.extended([(split.fst.clone(), fst), (split.snd.clone(), snd)]);

                    this.lower_tail(&split.tail, &frame, resume, state, builder)
                }),
            ),
            _ => self.lower_to_name(
                term,
                frame,
                state,
                builder,
                Box::new(move |_, _, _, value| {
                    cont::Tail::Jump(cont::JumpTarget {
                        target: resume.clone(),
                        params: vec![value],
                    })
                }),
            ),
        }
    }
}
