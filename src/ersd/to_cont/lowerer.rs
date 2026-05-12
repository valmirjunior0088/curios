use {
    super::{Entropy, Frame, FrameEntropy},
    crate::{cont, ersd},
};

fn unsupported_letrec_item(term: &ersd::Term) -> ! {
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
        func: &ersd::Func,
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
        term: &ersd::Term,
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
        letrec: &ersd::LetRec,
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
        term: &ersd::Term,
        frame: &Frame,
        state: &mut FrameEntropy,
        builder: &mut RegionBuilder,
    ) -> cont::ValueName {
        match term {
            ersd::Term::Name(name) => frame.find(&name.string),
            ersd::Term::Erased => {
                emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Unit))
            }
            ersd::Term::Prim(ersd::Prim::Nat(value)) => {
                emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Nat(*value)))
            }
            ersd::Term::Prim(ersd::Prim::NatEql(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatEql(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::NatAdd(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatAdd(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::NatSub(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatSub(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::NatMul(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatMul(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::NatLt(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatLt(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::Int(value)) => {
                emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Int(*value)))
            }
            ersd::Term::Prim(ersd::Prim::Flt(value)) => {
                emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Flt(*value)))
            }
            ersd::Term::Prim(ersd::Prim::IntEql(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntEql(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::IntAdd(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntAdd(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::IntSub(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntSub(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::IntMul(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntMul(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltAdd(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltAdd(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltSub(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltSub(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltMul(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltMul(left, right)),
                )
            }
            ersd::Term::Func(func) => {
                let (clsr_name, captured_values) = self.lower_closure(func, frame);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Pure(cont::Data::Clsr(clsr_name, captured_values)),
                )
            }
            ersd::Term::Pair(pair) => {
                let fst = self.lower_letrec_name(&pair.fst, frame, state, builder);
                let snd = self.lower_letrec_name(&pair.snd, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Pure(cont::Data::Tpl(vec![fst, snd])),
                )
            }
            ersd::Term::Atom(atom) => emit_fresh_value(
                state,
                builder,
                cont::Value::Pure(cont::Data::Nat(atom.index as u32)),
            ),
            ersd::Term::Let(let_) => {
                let body = self.lower_letrec_name(&let_.body, frame, state, builder);
                let frame = frame.extended([(let_.name.clone(), body)]);

                self.lower_letrec_name(&let_.tail, &frame, state, builder)
            }
            ersd::Term::LetRec(letrec) => {
                let frame = self.lower_letrec_bindings(letrec, frame, state, builder);

                self.lower_letrec_name(&letrec.tail, &frame, state, builder)
            }
            ersd::Term::Apply(_)
            | ersd::Term::Split(_)
            | ersd::Term::Match(_) => unsupported_letrec_item(term),
        }
    }

    fn lower_letrec_item(
        &mut self,
        term: &ersd::Term,
        target: cont::ValueName,
        frame: &Frame,
        state: &mut FrameEntropy,
        builder: &mut RegionBuilder,
    ) {
        match term {
            ersd::Term::Erased => {
                builder.add_value(target, cont::Value::Pure(cont::Data::Unit));
            }
            ersd::Term::Prim(ersd::Prim::Nat(value)) => {
                builder.add_value(target, cont::Value::Pure(cont::Data::Nat(*value)));
            }
            ersd::Term::Prim(ersd::Prim::NatEql(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::NatEql(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::NatAdd(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::NatAdd(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::NatSub(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::NatSub(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::NatMul(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::NatMul(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::NatLt(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::NatLt(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::Int(value)) => {
                builder.add_value(target, cont::Value::Pure(cont::Data::Int(*value)));
            }
            ersd::Term::Prim(ersd::Prim::Flt(value)) => {
                builder.add_value(target, cont::Value::Pure(cont::Data::Flt(*value)));
            }
            ersd::Term::Prim(ersd::Prim::IntEql(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::IntEql(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::IntAdd(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::IntAdd(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::IntSub(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::IntSub(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::IntMul(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::IntMul(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::FltAdd(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltAdd(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::FltSub(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltSub(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::FltMul(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltMul(left, right)));
            }
            ersd::Term::Func(func) => {
                let (clsr_name, captured_values) = self.lower_closure(func, frame);
                builder.add_value(
                    target,
                    cont::Value::Pure(cont::Data::Clsr(clsr_name, captured_values)),
                );
            }
            ersd::Term::Pair(pair) => {
                let fst = self.lower_letrec_name(&pair.fst, frame, state, builder);
                let snd = self.lower_letrec_name(&pair.snd, frame, state, builder);
                builder.add_value(target, cont::Value::Pure(cont::Data::Tpl(vec![fst, snd])));
            }
            ersd::Term::Atom(atom) => {
                builder.add_value(
                    target,
                    cont::Value::Pure(cont::Data::Nat(atom.index as u32)),
                );
            }
            ersd::Term::Let(let_) => {
                let body = self.lower_letrec_name(&let_.body, frame, state, builder);
                let frame = frame.extended([(let_.name.clone(), body)]);
                self.lower_letrec_item(&let_.tail, target, &frame, state, builder);
            }
            ersd::Term::LetRec(letrec) => {
                let frame = self.lower_letrec_bindings(letrec, frame, state, builder);
                self.lower_letrec_item(&letrec.tail, target, &frame, state, builder);
            }
            ersd::Term::Name(name) => {
                builder.add_value(target, cont::Value::Alias(frame.find(&name.string)));
            }
            ersd::Term::Apply(_)
            | ersd::Term::Split(_)
            | ersd::Term::Match(_) => unsupported_letrec_item(term),
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
        term: &ersd::Term,
        frame: &Frame,
        state: &mut FrameEntropy,
        builder: &mut RegionBuilder,
        cont: Cont<'_>,
    ) -> cont::Tail {
        match term {
            ersd::Term::Name(name) => cont(self, state, builder, frame.find(&name.string)),
            ersd::Term::Erased => {
                let value = emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Unit));

                cont(self, state, builder, value)
            }
            ersd::Term::Prim(ersd::Prim::Nat(value)) => {
                let value =
                    emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Nat(*value)));

                cont(self, state, builder, value)
            }
            ersd::Term::Prim(ersd::Prim::NatEql(left, right)) => {
                self.lower_to_name(
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
                )
            }
            ersd::Term::Prim(ersd::Prim::NatAdd(left, right)) => {
                self.lower_to_name(
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
                )
            }
            ersd::Term::Prim(ersd::Prim::NatSub(left, right)) => {
                self.lower_to_name(
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
                )
            }
            ersd::Term::Prim(ersd::Prim::NatMul(left, right)) => {
                self.lower_to_name(
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
                )
            }
            ersd::Term::Prim(ersd::Prim::NatLt(left, right)) => {
                self.lower_to_name(
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
                )
            }
            ersd::Term::Prim(ersd::Prim::Int(value)) => {
                let value =
                    emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Int(*value)));

                cont(self, state, builder, value)
            }
            ersd::Term::Prim(ersd::Prim::Flt(value)) => {
                let value =
                    emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Flt(*value)));

                cont(self, state, builder, value)
            }
            ersd::Term::Prim(ersd::Prim::IntEql(left, right)) => {
                self.lower_to_name(
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
                )
            }
            ersd::Term::Prim(ersd::Prim::IntAdd(left, right)) => {
                self.lower_to_name(
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
                )
            }
            ersd::Term::Prim(ersd::Prim::IntSub(left, right)) => {
                self.lower_to_name(
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
                )
            }
            ersd::Term::Prim(ersd::Prim::IntMul(left, right)) => {
                self.lower_to_name(
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
                )
            }
            ersd::Term::Prim(ersd::Prim::FltAdd(left, right)) => {
                self.lower_to_name(
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
                )
            }
            ersd::Term::Prim(ersd::Prim::FltSub(left, right)) => {
                self.lower_to_name(
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
                )
            }
            ersd::Term::Prim(ersd::Prim::FltMul(left, right)) => {
                self.lower_to_name(
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
                )
            }
            ersd::Term::Func(func) => {
                let (clsr_name, captured_values) = self.lower_closure(func, frame);
                let value = emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Pure(cont::Data::Clsr(clsr_name, captured_values)),
                );

                cont(self, state, builder, value)
            }
            ersd::Term::Pair(pair) => self.lower_to_name(
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
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Pure(cont::Data::Tpl(vec![fst, snd])),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Atom(atom) => {
                let value = emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Pure(cont::Data::Nat(atom.index as u32)),
                );

                cont(self, state, builder, value)
            }
            ersd::Term::Let(let_) => {
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
            ersd::Term::LetRec(letrec) => {
                let frame = self.lower_letrec_bindings(letrec, frame, state, builder);
                self.lower_to_name(&letrec.tail, &frame, state, builder, cont)
            }
            ersd::Term::Apply(_)
            | ersd::Term::Split(_)
            | ersd::Term::Match(_) => {
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
        term: &ersd::Term,
        frame: &Frame,
        resume: &cont::BlockName,
        state: &mut FrameEntropy,
        builder: &mut RegionBuilder,
    ) -> cont::Tail {
        match term {
            ersd::Term::Apply(apply) => self.lower_to_name(
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
            ersd::Term::Match(match_) => self.lower_to_name(
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
            ersd::Term::Split(split) => self.lower_to_name(
                &split.head,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, head| {
                    let fst = state.fresh_value();
                    builder.add_value(
                        fst.clone(),
                        cont::Value::Eval(cont::Code::TplGet(0, head.clone())),
                    );

                    let snd = state.fresh_value();
                    builder.add_value(snd.clone(), cont::Value::Eval(cont::Code::TplGet(1, head)));

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
