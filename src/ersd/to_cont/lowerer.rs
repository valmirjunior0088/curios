use {
    super::{
        Backpatch, Cont, ContMany, Emit, Entropy, Frame, FrameEntropy, RecBody, RegionBuilder,
        rec_computed_order, unsupported_sync_rec_item,
    },
    crate::{cont, ersd},
    std::collections::{BTreeMap, HashMap},
};

#[derive(Debug)]
pub struct Lowerer<'a> {
    module: &'a mut cont::Module,
    clsrs: Entropy<cont::ClsrName>,
}

impl<'a> Lowerer<'a> {
    pub fn new(module: &'a mut cont::Module) -> Self {
        Self {
            module,
            clsrs: Entropy::<cont::ClsrName>::new(),
        }
    }

    pub fn lower_entry(
        &mut self,
        term: &ersd::Term,
        frame: &Frame,
    ) -> (cont::BlockName, cont::Region) {
        let (mut entry, resume) = FrameEntropy::new();
        let mut emit = Emit::new(&mut entry);
        let tail = Work {
            lowerer: self,
            emit: &mut emit,
        }
        .lower_tail(term, frame, &resume);

        (resume, emit.finish(tail))
    }
}

/// A unit of lowering work: the shared [`Lowerer`] (module + closure names) paired with the
/// region currently being emitted into. Threading one `Work` instead of a `Lowerer` and an
/// `Emit` separately keeps the per-primitive helpers down to their operands plus a continuation.
pub struct Work<'a, 'm, 'e> {
    lowerer: &'a mut Lowerer<'m>,
    emit: &'a mut Emit<'e>,
}

impl Work<'_, '_, '_> {
    /// Build a nested region with its own `Work`, sharing this work's `Lowerer` and name supply.
    fn in_subregion(&mut self, build: impl FnOnce(&mut Work) -> cont::Tail) -> cont::Region {
        let mut emit = self.emit.subregion();
        let tail = build(&mut Work {
            lowerer: &mut *self.lowerer,
            emit: &mut emit,
        });

        emit.finish(tail)
    }

    pub fn lower_closure(
        &mut self,
        func: &ersd::Func,
        frame: &Frame,
    ) -> (cont::ClsrName, Vec<cont::ValueName>) {
        let clsr_name = self.lowerer.clsrs.fresh();
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

        let params = func
            .params
            .iter()
            .map(|name| {
                let val = entry.fresh_value();
                clsr_frame.push(name.clone(), val.clone());
                val
            })
            .collect::<Vec<_>>();

        let mut emit = Emit::new(&mut entry);
        let tail = Work {
            lowerer: &mut *self.lowerer,
            emit: &mut emit,
        }
        .lower_tail(&func.body, &clsr_frame, &resume);
        let region = emit.finish(tail);

        self.lowerer.module.add_clsr(
            clsr_name.clone(),
            cont::Clsr {
                fields: fields.clone(),
                params,
                resume,
                region,
            },
        );

        let captured_values = func
            .captures
            .iter()
            .map(|capture| frame.find(capture))
            .collect();

        (clsr_name, captured_values)
    }

    pub fn lower_letrec_bindings(&mut self, letrec: &ersd::Rec, frame: &Frame) -> Frame {
        let mut frame = frame.clone();
        let reserved = letrec
            .names
            .iter()
            .map(|name| {
                let reserved = self.emit.fresh_value();
                frame.push(name.clone(), reserved.clone());

                reserved
            })
            .collect::<Vec<_>>();

        for (item, target) in letrec.items.iter().zip(reserved) {
            match self.plan_backpatch(item, &frame) {
                Some(backpatch) => {
                    self.emit.add_prealloc(target.clone(), backpatch.prealloc());
                    self.emit_backpatch(target, &backpatch, &frame);
                }
                None => match item.as_ref() {
                    ersd::Term::Apply(_) | ersd::Term::Match(_) | ersd::Term::NatMatch(_) => {
                        unsupported_sync_rec_item(item)
                    }
                    _ => self.lower_letrec_item(item, target, &frame),
                },
            }
        }

        frame
    }

    pub fn lower_pure_name(&mut self, term: &ersd::Term, frame: &Frame) -> cont::ValueName {
        match term {
            ersd::Term::Name(name) => frame.find(name.as_str()),
            ersd::Term::Erased => self.emit.fresh(cont::Value::Pure(cont::Data::Tpl(vec![]))),
            ersd::Term::Prim(ersd::Prim::Nat(value)) => {
                self.emit.fresh(cont::Value::Pure(cont::Data::Nat(*value)))
            }
            ersd::Term::Prim(ersd::Prim::NatEql(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::NatEql)
            }
            ersd::Term::Prim(ersd::Prim::NatNeq(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::NatNeq)
            }
            ersd::Term::Prim(ersd::Prim::NatAdd(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::NatAdd)
            }
            ersd::Term::Prim(ersd::Prim::NatSub(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::NatSub)
            }
            ersd::Term::Prim(ersd::Prim::NatMul(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::NatMul)
            }
            ersd::Term::Prim(ersd::Prim::NatLt(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::NatLt)
            }
            ersd::Term::Prim(ersd::Prim::NatDiv(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::NatDiv)
            }
            ersd::Term::Prim(ersd::Prim::NatRem(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::NatRem)
            }
            ersd::Term::Prim(ersd::Prim::NatGt(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::NatGt)
            }
            ersd::Term::Prim(ersd::Prim::NatLte(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::NatLte)
            }
            ersd::Term::Prim(ersd::Prim::NatGte(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::NatGte)
            }
            ersd::Term::Prim(ersd::Prim::Int(value)) => {
                self.emit.fresh(cont::Value::Pure(cont::Data::Int(*value)))
            }
            ersd::Term::Prim(ersd::Prim::Flt(value)) => {
                self.emit.fresh(cont::Value::Pure(cont::Data::Flt(*value)))
            }
            ersd::Term::Prim(ersd::Prim::IntEql(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::IntEql)
            }
            ersd::Term::Prim(ersd::Prim::IntNeq(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::IntNeq)
            }
            ersd::Term::Prim(ersd::Prim::IntAdd(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::IntAdd)
            }
            ersd::Term::Prim(ersd::Prim::IntSub(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::IntSub)
            }
            ersd::Term::Prim(ersd::Prim::IntMul(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::IntMul)
            }
            ersd::Term::Prim(ersd::Prim::IntDiv(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::IntDiv)
            }
            ersd::Term::Prim(ersd::Prim::IntRem(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::IntRem)
            }
            ersd::Term::Prim(ersd::Prim::IntLt(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::IntLt)
            }
            ersd::Term::Prim(ersd::Prim::IntGt(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::IntGt)
            }
            ersd::Term::Prim(ersd::Prim::IntLte(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::IntLte)
            }
            ersd::Term::Prim(ersd::Prim::IntGte(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::IntGte)
            }
            ersd::Term::Prim(ersd::Prim::FltAdd(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::FltAdd)
            }
            ersd::Term::Prim(ersd::Prim::FltSub(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::FltSub)
            }
            ersd::Term::Prim(ersd::Prim::FltMul(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::FltMul)
            }
            ersd::Term::Prim(ersd::Prim::FltDiv(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::FltDiv)
            }
            ersd::Term::Prim(ersd::Prim::FltEql(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::FltEql)
            }
            ersd::Term::Prim(ersd::Prim::FltNeq(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::FltNeq)
            }
            ersd::Term::Prim(ersd::Prim::FltLt(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::FltLt)
            }
            ersd::Term::Prim(ersd::Prim::FltGt(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::FltGt)
            }
            ersd::Term::Prim(ersd::Prim::FltLte(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::FltLte)
            }
            ersd::Term::Prim(ersd::Prim::FltGte(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::FltGte)
            }
            ersd::Term::Prim(ersd::Prim::FltMin(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::FltMin)
            }
            ersd::Term::Prim(ersd::Prim::FltMax(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::FltMax)
            }
            ersd::Term::Prim(ersd::Prim::FltNeg(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::FltNeg)
            }
            ersd::Term::Prim(ersd::Prim::FltAbs(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::FltAbs)
            }
            ersd::Term::Prim(ersd::Prim::FltSqrt(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::FltSqrt)
            }
            ersd::Term::Prim(ersd::Prim::FltFloor(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::FltFloor)
            }
            ersd::Term::Prim(ersd::Prim::FltCeil(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::FltCeil)
            }
            ersd::Term::Prim(ersd::Prim::FltTrunc(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::FltTrunc)
            }
            ersd::Term::Prim(ersd::Prim::FltNearest(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::FltNearest)
            }
            ersd::Term::Prim(ersd::Prim::NatToStr(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::NatToStr)
            }
            ersd::Term::Prim(ersd::Prim::IntToStr(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::IntToStr)
            }
            ersd::Term::Prim(ersd::Prim::FltToStr(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::FltToStr)
            }
            ersd::Term::Prim(ersd::Prim::NatToInt(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::NatToInt)
            }
            ersd::Term::Prim(ersd::Prim::NatToFlt(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::NatToFlt)
            }
            ersd::Term::Prim(ersd::Prim::IntToNat(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::IntToNat)
            }
            ersd::Term::Prim(ersd::Prim::IntToFlt(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::IntToFlt)
            }
            ersd::Term::Prim(ersd::Prim::FltToNat(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::FltToNat)
            }
            ersd::Term::Prim(ersd::Prim::FltToInt(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::FltToInt)
            }
            ersd::Term::Prim(ersd::Prim::Bin(bytes)) => self
                .emit
                .fresh(cont::Value::Pure(cont::Data::Bin(bytes.clone()))),
            ersd::Term::Prim(ersd::Prim::BinLen(bin)) => {
                self.lower_pure_unary_code(bin, frame, cont::Code::BinLen)
            }
            ersd::Term::Prim(ersd::Prim::BinEql(left, right)) => {
                self.lower_pure_binary_code(left, right, frame, cont::Code::BinEql)
            }
            ersd::Term::Prim(ersd::Prim::BinGet(bin, idx)) => {
                self.lower_pure_binary_code(bin, idx, frame, cont::Code::BinGet)
            }
            ersd::Term::Prim(ersd::Prim::BinSlice(bin, start, end)) => {
                self.lower_pure_ternary_code(bin, start, end, frame, cont::Code::BinSlice)
            }
            ersd::Term::Prim(ersd::Prim::BinAppend(bin, byte)) => {
                self.lower_pure_binary_code(bin, byte, frame, cont::Code::BinAppend)
            }
            ersd::Term::Prim(ersd::Prim::BinConcat(operands)) => {
                let names = self.lower_pure_names(operands, frame);

                self.emit
                    .fresh(cont::Value::Eval(cont::Code::BinConcat(names)))
            }
            ersd::Term::Prim(ersd::Prim::Arr(elements)) => {
                let names = self.lower_pure_names(elements, frame);

                self.emit.fresh(cont::Value::Pure(cont::Data::Arr(names)))
            }
            ersd::Term::Prim(ersd::Prim::ArrLen(lst)) => {
                self.lower_pure_unary_code(lst, frame, cont::Code::ArrLen)
            }
            ersd::Term::Prim(ersd::Prim::ArrGet(lst, idx)) => {
                self.lower_pure_binary_code(lst, idx, frame, cont::Code::ArrGet)
            }
            ersd::Term::Prim(ersd::Prim::ArrSlice(lst, start, end)) => {
                self.lower_pure_ternary_code(lst, start, end, frame, cont::Code::ArrSlice)
            }
            ersd::Term::Prim(ersd::Prim::ArrAppend(lst, elem)) => {
                self.lower_pure_binary_code(lst, elem, frame, cont::Code::ArrAppend)
            }
            ersd::Term::Prim(ersd::Prim::ArrConcat(operands)) => {
                let names = self.lower_pure_names(operands, frame);

                self.emit
                    .fresh(cont::Value::Eval(cont::Code::ArrConcat(names)))
            }
            ersd::Term::Func(func) => {
                let (clsr_name, captured_values) = self.lower_closure(func, frame);

                self.emit.fresh(cont::Value::Pure(cont::Data::Clsr(
                    clsr_name,
                    captured_values,
                )))
            }
            ersd::Term::Tuple(s) => {
                let field_names = s
                    .fields
                    .iter()
                    .map(|f| self.lower_pure_name(f, frame))
                    .collect::<Vec<_>>();

                self.emit
                    .fresh(cont::Value::Pure(cont::Data::Tpl(field_names)))
            }
            ersd::Term::Prim(ersd::Prim::Unit) => {
                self.emit.fresh(cont::Value::Pure(cont::Data::Tpl(vec![])))
            }
            ersd::Term::Prim(ersd::Prim::IoPrint(operand)) => {
                self.lower_pure_unary_code(operand, frame, cont::Code::IoPrint)
            }
            ersd::Term::Prim(ersd::Prim::IoRead) => {
                self.emit.fresh(cont::Value::Eval(cont::Code::IoRead))
            }
            ersd::Term::Atom(atom) => self
                .emit
                .fresh(cont::Value::Pure(cont::Data::Nat(atom.index as u32))),
            ersd::Term::Let(let_) => {
                let body = self.lower_pure_name(&let_.body, frame);
                let frame = frame.extended([(let_.name.clone(), body)]);

                self.lower_pure_name(&let_.tail, &frame)
            }
            ersd::Term::Rec(letrec) => {
                let frame = self.lower_letrec_bindings(letrec, frame);

                self.lower_pure_name(&letrec.tail, &frame)
            }
            ersd::Term::Proj(proj) => {
                let head = self.lower_pure_name(&proj.head, frame);

                self.emit
                    .fresh(cont::Value::Eval(cont::Code::TplGet(head, proj.index)))
            }
            ersd::Term::Apply(_) | ersd::Term::Match(_) | ersd::Term::NatMatch(_) => {
                unsupported_sync_rec_item(term)
            }
        }
    }

    pub fn lower_letrec_item(&mut self, term: &ersd::Term, target: cont::ValueName, frame: &Frame) {
        match term {
            ersd::Term::Apply(_) | ersd::Term::Match(_) | ersd::Term::NatMatch(_) => {
                unsupported_sync_rec_item(term)
            }
            _ => {
                let value = self.lower_pure_name(term, frame);
                self.emit.add_value(target, cont::Value::Alias(value));
            }
        }
    }

    fn lower_pure_unary_code(
        &mut self,
        operand: &ersd::Term,
        frame: &Frame,
        code: impl FnOnce(cont::ValueName) -> cont::Code,
    ) -> cont::ValueName {
        let operand = self.lower_pure_name(operand, frame);
        self.emit.fresh(cont::Value::Eval(code(operand)))
    }

    fn lower_pure_binary_code(
        &mut self,
        left: &ersd::Term,
        right: &ersd::Term,
        frame: &Frame,
        code: impl FnOnce(cont::ValueName, cont::ValueName) -> cont::Code,
    ) -> cont::ValueName {
        let left = self.lower_pure_name(left, frame);
        let right = self.lower_pure_name(right, frame);
        self.emit.fresh(cont::Value::Eval(code(left, right)))
    }

    fn lower_pure_ternary_code(
        &mut self,
        first: &ersd::Term,
        second: &ersd::Term,
        third: &ersd::Term,
        frame: &Frame,
        code: impl FnOnce(cont::ValueName, cont::ValueName, cont::ValueName) -> cont::Code,
    ) -> cont::ValueName {
        let first = self.lower_pure_name(first, frame);
        let second = self.lower_pure_name(second, frame);
        let third = self.lower_pure_name(third, frame);
        self.emit
            .fresh(cont::Value::Eval(code(first, second, third)))
    }

    fn lower_pure_names(&mut self, terms: &[ersd::Subterm], frame: &Frame) -> Vec<cont::ValueName> {
        terms
            .iter()
            .map(|term| self.lower_pure_name(term, frame))
            .collect()
    }

    fn lower_unary_code<'b>(
        &mut self,
        operand: &'b ersd::Term,
        frame: &'b Frame,
        cont: Cont<'b>,
        code: impl FnOnce(cont::ValueName) -> cont::Code + 'b,
    ) -> cont::Tail {
        self.lower_value_name(
            operand,
            frame,
            Box::new(move |work, operand| {
                let value = work.emit.fresh(cont::Value::Eval(code(operand)));

                cont(work, value)
            }),
        )
    }

    fn lower_binary_code<'b>(
        &mut self,
        left: &'b ersd::Term,
        right: &'b ersd::Term,
        frame: &'b Frame,
        cont: Cont<'b>,
        code: impl FnOnce(cont::ValueName, cont::ValueName) -> cont::Code + 'b,
    ) -> cont::Tail {
        self.lower_value_name(
            left,
            frame,
            Box::new(move |work, left| {
                work.lower_value_name(
                    right,
                    frame,
                    Box::new(move |work, right| {
                        let value = work.emit.fresh(cont::Value::Eval(code(left, right)));

                        cont(work, value)
                    }),
                )
            }),
        )
    }

    fn lower_ternary_code<'b>(
        &mut self,
        first: &'b ersd::Term,
        second: &'b ersd::Term,
        third: &'b ersd::Term,
        frame: &'b Frame,
        cont: Cont<'b>,
        code: impl FnOnce(cont::ValueName, cont::ValueName, cont::ValueName) -> cont::Code + 'b,
    ) -> cont::Tail {
        self.lower_value_name(
            first,
            frame,
            Box::new(move |work, first| {
                work.lower_value_name(
                    second,
                    frame,
                    Box::new(move |work, second| {
                        work.lower_value_name(
                            third,
                            frame,
                            Box::new(move |work, third| {
                                let value = work
                                    .emit
                                    .fresh(cont::Value::Eval(code(first, second, third)));

                                cont(work, value)
                            }),
                        )
                    }),
                )
            }),
        )
    }

    pub fn lower_value_name(
        &mut self,
        term: &ersd::Term,
        frame: &Frame,
        cont: Cont<'_>,
    ) -> cont::Tail {
        match term {
            ersd::Term::Name(name) => cont(self, frame.find(name.as_str())),
            ersd::Term::Erased => {
                let value = self.emit.fresh(cont::Value::Pure(cont::Data::Tpl(vec![])));

                cont(self, value)
            }
            ersd::Term::Prim(ersd::Prim::Nat(value)) => {
                let value = self.emit.fresh(cont::Value::Pure(cont::Data::Nat(*value)));

                cont(self, value)
            }
            ersd::Term::Prim(ersd::Prim::NatEql(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::NatEql)
            }
            ersd::Term::Prim(ersd::Prim::NatAdd(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::NatAdd)
            }
            ersd::Term::Prim(ersd::Prim::NatSub(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::NatSub)
            }
            ersd::Term::Prim(ersd::Prim::NatMul(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::NatMul)
            }
            ersd::Term::Prim(ersd::Prim::NatLt(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::NatLt)
            }
            ersd::Term::Prim(ersd::Prim::NatNeq(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::NatNeq)
            }
            ersd::Term::Prim(ersd::Prim::NatDiv(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::NatDiv)
            }
            ersd::Term::Prim(ersd::Prim::NatRem(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::NatRem)
            }
            ersd::Term::Prim(ersd::Prim::NatGt(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::NatGt)
            }
            ersd::Term::Prim(ersd::Prim::NatLte(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::NatLte)
            }
            ersd::Term::Prim(ersd::Prim::NatGte(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::NatGte)
            }
            ersd::Term::Prim(ersd::Prim::Int(value)) => {
                let value = self.emit.fresh(cont::Value::Pure(cont::Data::Int(*value)));

                cont(self, value)
            }
            ersd::Term::Prim(ersd::Prim::Flt(value)) => {
                let value = self.emit.fresh(cont::Value::Pure(cont::Data::Flt(*value)));

                cont(self, value)
            }
            ersd::Term::Prim(ersd::Prim::IntEql(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::IntEql)
            }
            ersd::Term::Prim(ersd::Prim::IntAdd(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::IntAdd)
            }
            ersd::Term::Prim(ersd::Prim::IntSub(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::IntSub)
            }
            ersd::Term::Prim(ersd::Prim::IntMul(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::IntMul)
            }
            ersd::Term::Prim(ersd::Prim::IntNeq(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::IntNeq)
            }
            ersd::Term::Prim(ersd::Prim::IntDiv(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::IntDiv)
            }
            ersd::Term::Prim(ersd::Prim::IntRem(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::IntRem)
            }
            ersd::Term::Prim(ersd::Prim::IntLt(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::IntLt)
            }
            ersd::Term::Prim(ersd::Prim::IntGt(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::IntGt)
            }
            ersd::Term::Prim(ersd::Prim::IntLte(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::IntLte)
            }
            ersd::Term::Prim(ersd::Prim::IntGte(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::IntGte)
            }
            ersd::Term::Prim(ersd::Prim::FltAdd(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::FltAdd)
            }
            ersd::Term::Prim(ersd::Prim::FltSub(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::FltSub)
            }
            ersd::Term::Prim(ersd::Prim::FltMul(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::FltMul)
            }
            ersd::Term::Prim(ersd::Prim::FltDiv(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::FltDiv)
            }
            ersd::Term::Prim(ersd::Prim::FltEql(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::FltEql)
            }
            ersd::Term::Prim(ersd::Prim::FltNeq(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::FltNeq)
            }
            ersd::Term::Prim(ersd::Prim::FltLt(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::FltLt)
            }
            ersd::Term::Prim(ersd::Prim::FltGt(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::FltGt)
            }
            ersd::Term::Prim(ersd::Prim::FltLte(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::FltLte)
            }
            ersd::Term::Prim(ersd::Prim::FltGte(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::FltGte)
            }
            ersd::Term::Prim(ersd::Prim::FltMin(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::FltMin)
            }
            ersd::Term::Prim(ersd::Prim::FltMax(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::FltMax)
            }
            ersd::Term::Prim(ersd::Prim::FltNeg(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::FltNeg)
            }
            ersd::Term::Prim(ersd::Prim::FltAbs(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::FltAbs)
            }
            ersd::Term::Prim(ersd::Prim::FltSqrt(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::FltSqrt)
            }
            ersd::Term::Prim(ersd::Prim::FltFloor(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::FltFloor)
            }
            ersd::Term::Prim(ersd::Prim::FltCeil(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::FltCeil)
            }
            ersd::Term::Prim(ersd::Prim::FltTrunc(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::FltTrunc)
            }
            ersd::Term::Prim(ersd::Prim::FltNearest(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::FltNearest)
            }
            ersd::Term::Prim(ersd::Prim::NatToStr(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::NatToStr)
            }
            ersd::Term::Prim(ersd::Prim::IntToStr(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::IntToStr)
            }
            ersd::Term::Prim(ersd::Prim::FltToStr(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::FltToStr)
            }
            ersd::Term::Prim(ersd::Prim::NatToInt(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::NatToInt)
            }
            ersd::Term::Prim(ersd::Prim::IntToNat(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::IntToNat)
            }
            ersd::Term::Prim(ersd::Prim::IntToFlt(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::IntToFlt)
            }
            ersd::Term::Prim(ersd::Prim::NatToFlt(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::NatToFlt)
            }
            ersd::Term::Prim(ersd::Prim::FltToInt(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::FltToInt)
            }
            ersd::Term::Prim(ersd::Prim::FltToNat(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::FltToNat)
            }
            ersd::Term::Prim(ersd::Prim::Bin(bytes)) => {
                let value = self
                    .emit
                    .fresh(cont::Value::Pure(cont::Data::Bin(bytes.clone())));

                cont(self, value)
            }
            ersd::Term::Prim(ersd::Prim::BinLen(bin)) => {
                self.lower_unary_code(bin, frame, cont, cont::Code::BinLen)
            }
            ersd::Term::Prim(ersd::Prim::BinEql(left, right)) => {
                self.lower_binary_code(left, right, frame, cont, cont::Code::BinEql)
            }
            ersd::Term::Prim(ersd::Prim::BinGet(bin, idx)) => {
                self.lower_binary_code(bin, idx, frame, cont, cont::Code::BinGet)
            }
            ersd::Term::Prim(ersd::Prim::BinSlice(bin, start, end)) => {
                self.lower_ternary_code(bin, start, end, frame, cont, cont::Code::BinSlice)
            }
            ersd::Term::Prim(ersd::Prim::BinAppend(bin, byte)) => {
                self.lower_binary_code(bin, byte, frame, cont, cont::Code::BinAppend)
            }
            ersd::Term::Prim(ersd::Prim::BinConcat(operands)) => {
                self.lower_bin_concat(operands, frame, vec![], cont)
            }
            ersd::Term::Prim(ersd::Prim::Arr(elements)) => {
                self.lower_lst(elements, frame, vec![], cont)
            }
            ersd::Term::Prim(ersd::Prim::ArrLen(lst)) => {
                self.lower_unary_code(lst, frame, cont, cont::Code::ArrLen)
            }
            ersd::Term::Prim(ersd::Prim::ArrGet(lst, idx)) => {
                self.lower_binary_code(lst, idx, frame, cont, cont::Code::ArrGet)
            }
            ersd::Term::Prim(ersd::Prim::ArrSlice(lst, start, end)) => {
                self.lower_ternary_code(lst, start, end, frame, cont, cont::Code::ArrSlice)
            }
            ersd::Term::Prim(ersd::Prim::ArrAppend(lst, elem)) => {
                self.lower_binary_code(lst, elem, frame, cont, cont::Code::ArrAppend)
            }
            ersd::Term::Prim(ersd::Prim::ArrConcat(operands)) => {
                self.lower_arr_concat(operands, frame, vec![], cont)
            }
            ersd::Term::Func(func) => {
                let (clsr_name, captured_values) = self.lower_closure(func, frame);
                let value = self.emit.fresh(cont::Value::Pure(cont::Data::Clsr(
                    clsr_name,
                    captured_values,
                )));

                cont(self, value)
            }
            ersd::Term::Tuple(s) => self.lower_struct(&s.fields, frame, vec![], cont),
            ersd::Term::Prim(ersd::Prim::Unit) => {
                let value = self.emit.fresh(cont::Value::Pure(cont::Data::Tpl(vec![])));
                cont(self, value)
            }
            ersd::Term::Prim(ersd::Prim::IoPrint(operand)) => {
                self.lower_unary_code(operand, frame, cont, cont::Code::IoPrint)
            }
            ersd::Term::Prim(ersd::Prim::IoRead) => {
                let value = self.emit.fresh(cont::Value::Eval(cont::Code::IoRead));
                cont(self, value)
            }
            ersd::Term::Atom(atom) => {
                let value = self
                    .emit
                    .fresh(cont::Value::Pure(cont::Data::Nat(atom.index as u32)));

                cont(self, value)
            }
            ersd::Term::Let(let_) => {
                let name = let_.name.clone();

                self.lower_value_name(
                    &let_.body,
                    frame,
                    Box::new(move |work, body| {
                        let frame = frame.extended([(name, body)]);

                        work.lower_value_name(&let_.tail, &frame, cont)
                    }),
                )
            }
            ersd::Term::Rec(letrec) => self.lower_rec(
                letrec,
                frame,
                Box::new(move |work, frame| work.lower_value_name(&letrec.tail, frame, cont)),
            ),
            ersd::Term::Proj(proj) => {
                let index = proj.index;

                self.lower_value_name(
                    &proj.head,
                    frame,
                    Box::new(move |work, head| {
                        let v = work
                            .emit
                            .fresh(cont::Value::Eval(cont::Code::TplGet(head, index)));

                        cont(work, v)
                    }),
                )
            }
            ersd::Term::Apply(_) | ersd::Term::Match(_) | ersd::Term::NatMatch(_) => {
                let block = self.emit.fresh_block();
                let param = self.emit.fresh_value();
                let region = self.in_subregion(|work| cont(work, param.clone()));

                self.emit.add_block(
                    block.clone(),
                    cont::Block {
                        params: vec![param],
                        region,
                    },
                );

                self.lower_tail(term, frame, &block)
            }
        }
    }

    fn lower_lst<'b>(
        &mut self,
        elements: &'b [ersd::Subterm],
        frame: &'b Frame,
        mut names: Vec<cont::ValueName>,
        cont: Cont<'b>,
    ) -> cont::Tail {
        match elements {
            [] => {
                let value = self.emit.fresh(cont::Value::Pure(cont::Data::Arr(names)));

                cont(self, value)
            }
            [head, tail @ ..] => self.lower_value_name(
                head,
                frame,
                Box::new(move |work, name| {
                    names.push(name);
                    work.lower_lst(tail, frame, names, cont)
                }),
            ),
        }
    }

    fn lower_bin_concat<'b>(
        &mut self,
        operands: &'b [ersd::Subterm],
        frame: &'b Frame,
        mut names: Vec<cont::ValueName>,
        cont: Cont<'b>,
    ) -> cont::Tail {
        match operands {
            [] => {
                let value = self
                    .emit
                    .fresh(cont::Value::Eval(cont::Code::BinConcat(names)));

                cont(self, value)
            }
            [head, tail @ ..] => self.lower_value_name(
                head,
                frame,
                Box::new(move |work, name| {
                    names.push(name);
                    work.lower_bin_concat(tail, frame, names, cont)
                }),
            ),
        }
    }

    fn lower_arr_concat<'b>(
        &mut self,
        operands: &'b [ersd::Subterm],
        frame: &'b Frame,
        mut names: Vec<cont::ValueName>,
        cont: Cont<'b>,
    ) -> cont::Tail {
        match operands {
            [] => {
                let value = self
                    .emit
                    .fresh(cont::Value::Eval(cont::Code::ArrConcat(names)));

                cont(self, value)
            }
            [head, tail @ ..] => self.lower_value_name(
                head,
                frame,
                Box::new(move |work, name| {
                    names.push(name);
                    work.lower_arr_concat(tail, frame, names, cont)
                }),
            ),
        }
    }

    pub fn lower_names<'b>(
        &mut self,
        params: &'b [ersd::Subterm],
        frame: &'b Frame,
        mut names: Vec<cont::ValueName>,
        cont: ContMany<'b>,
    ) -> cont::Tail {
        match params {
            [] => cont(self, names),
            [head, tail @ ..] => self.lower_value_name(
                head,
                frame,
                Box::new(move |work, name| {
                    names.push(name);
                    work.lower_names(tail, frame, names, cont)
                }),
            ),
        }
    }

    fn lower_struct<'b>(
        &mut self,
        fields: &'b [ersd::Subterm],
        frame: &'b Frame,
        mut names: Vec<cont::ValueName>,
        cont: Cont<'b>,
    ) -> cont::Tail {
        match fields {
            [] => {
                let value = self.emit.fresh(cont::Value::Pure(cont::Data::Tpl(names)));

                cont(self, value)
            }
            [head, tail @ ..] => self.lower_value_name(
                head,
                frame,
                Box::new(move |work, name| {
                    names.push(name);
                    work.lower_struct(tail, frame, names, cont)
                }),
            ),
        }
    }

    /// Classify a `rec` item: backpatches (`Func`/`Tuple`/`Arr`) get a prealloc'd shell so
    /// their identity is available before their fields; a `Func` is lowered here so its
    /// `ClsrName` is fixed. Everything else is "computed" (lowered via `lower_value_name`).
    pub fn plan_backpatch<'b>(
        &mut self,
        item: &'b ersd::Term,
        frame: &Frame,
    ) -> Option<Backpatch<'b>> {
        match item {
            ersd::Term::Func(func) => {
                let (clsr_name, captures) = self.lower_closure(func, frame);

                Some(Backpatch::Clsr(clsr_name, captures))
            }
            ersd::Term::Tuple(tuple) => Some(Backpatch::Tpl(&tuple.fields)),
            ersd::Term::Prim(ersd::Prim::Arr(elems)) => Some(Backpatch::Arr(elems)),
            _ => None,
        }
    }

    pub fn emit_backpatch(
        &mut self,
        target: cont::ValueName,
        backpatch: &Backpatch,
        frame: &Frame,
    ) {
        match backpatch {
            Backpatch::Clsr(clsr_name, captures) => self.emit.add_value(
                target,
                cont::Value::Pure(cont::Data::Clsr(clsr_name.clone(), captures.clone())),
            ),
            Backpatch::Tpl(fields) => {
                let names = fields
                    .iter()
                    .map(|field| self.lower_pure_name(field, frame))
                    .collect();

                self.emit
                    .add_value(target, cont::Value::Pure(cont::Data::Tpl(names)));
            }
            Backpatch::Arr(elems) => {
                let names = elems
                    .iter()
                    .map(|elem| self.lower_pure_name(elem, frame))
                    .collect();

                self.emit
                    .add_value(target, cont::Value::Pure(cont::Data::Arr(names)));
            }
        }
    }

    /// Lower a `rec` group, then `body`. Backpatches are prealloc'd at region entry; call/match
    /// -valued bindings are lowered in dependency order through resume blocks; patches
    /// (which may reference those results) run last, just before `body`.
    pub fn lower_rec<'b>(
        &mut self,
        letrec: &'b ersd::Rec,
        frame: &Frame,
        body: RecBody<'b>,
    ) -> cont::Tail {
        let mut frame = frame.clone();
        let targets = letrec
            .names
            .iter()
            .map(|name| {
                let target = self.emit.fresh_value();
                frame.push(name.clone(), target.clone());

                target
            })
            .collect::<Vec<_>>();

        let mut backpatches: Vec<(cont::ValueName, Backpatch<'b>)> = vec![];
        let mut computed: Vec<(usize, cont::ValueName, &'b ersd::Term)> = vec![];

        for (index, (item, target)) in letrec.items.iter().zip(&targets).enumerate() {
            match self.plan_backpatch(item, &frame) {
                Some(backpatch) => backpatches.push((target.clone(), backpatch)),
                None => computed.push((index, target.clone(), item)),
            }
        }

        let computed_names = computed
            .iter()
            .map(|(index, _, _)| letrec.names[*index].as_str())
            .collect::<Vec<_>>();

        let name_to_pos = computed_names
            .iter()
            .enumerate()
            .map(|(pos, name)| (*name, pos))
            .collect::<HashMap<_, _>>();

        let deps = computed
            .iter()
            .map(|(_, _, rhs)| {
                rhs.free_names()
                    .iter()
                    .filter_map(|name| name_to_pos.get(name.as_str()).copied())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        let order = rec_computed_order(&computed_names, &deps);

        for (target, backpatch) in &backpatches {
            self.emit.add_prealloc(target.clone(), backpatch.prealloc());
        }

        let sorted = order
            .into_iter()
            .map(|pos| {
                let (_, target, rhs) = &computed[pos];
                (target.clone(), *rhs)
            })
            .collect::<Vec<_>>();

        let backpatch_body: RecBody<'b> = Box::new(move |work, frame| {
            for (target, backpatch) in &backpatches {
                work.emit_backpatch(target.clone(), backpatch, frame);
            }

            body(work, frame)
        });

        self.lower_rec_computed(&sorted, &frame, backpatch_body)
    }

    pub fn lower_rec_computed<'b>(
        &mut self,
        computed: &'b [(cont::ValueName, &'b ersd::Term)],
        frame: &'b Frame,
        body: RecBody<'b>,
    ) -> cont::Tail {
        match computed {
            [] => body(self, frame),
            [(target, rhs), rest @ ..] => {
                let target = target.clone();

                self.lower_value_name(
                    rhs,
                    frame,
                    Box::new(move |work, result| {
                        work.emit.add_value(target, cont::Value::Alias(result));
                        work.lower_rec_computed(rest, frame, body)
                    }),
                )
            }
        }
    }

    pub fn lower_tail(
        &mut self,
        term: &ersd::Term,
        frame: &Frame,
        resume: &cont::BlockName,
    ) -> cont::Tail {
        match term {
            ersd::Term::Apply(apply) => self.lower_value_name(
                &apply.head,
                frame,
                Box::new(move |work, head| {
                    work.lower_names(
                        &apply.params,
                        frame,
                        vec![],
                        Box::new(move |_, params| {
                            cont::Tail::Call(cont::CallTarget::Indirect {
                                target: head,
                                params,
                                resume: resume.clone(),
                            })
                        }),
                    )
                }),
            ),
            ersd::Term::Match(m) => self.lower_value_name(
                &m.head,
                frame,
                Box::new(move |work, head| {
                    let mut cases = BTreeMap::new();

                    for (i, branch) in m.cases.iter().enumerate() {
                        let block = work.emit.fresh_block();
                        let region = work.in_subregion(|w| w.lower_tail(branch, frame, resume));

                        work.emit.add_block(
                            block.clone(),
                            cont::Block {
                                params: vec![],
                                region,
                            },
                        );

                        cases.insert(
                            i as u32,
                            cont::JumpTarget {
                                target: block,
                                params: vec![],
                            },
                        );
                    }

                    cont::Tail::Match(cont::MatchTarget {
                        operand: head,
                        cases,
                        default: None,
                    })
                }),
            ),
            ersd::Term::NatMatch(ersd::NatMatch::Induction {
                head: nat_head,
                zero_case,
                pred,
                ih,
                succ_case,
            }) => self.lower_value_name(
                nat_head,
                frame,
                Box::new(move |work, head| {
                    // Constants shared across the loop.
                    let zero_nat = work.emit.fresh(cont::Value::Pure(cont::Data::Nat(0)));
                    let one_nat = work.emit.fresh(cont::Value::Pure(cont::Data::Nat(1)));

                    // Allocate block names up front so they can be referenced cross-block.
                    let loop_block_name = work.emit.fresh_block();
                    let body_block_name = work.emit.fresh_block();
                    let exit_block_name = work.emit.fresh_block();
                    let zero_resume_name = work.emit.fresh_block();

                    // zero_resume(pz): jump loop_block(0, pz)
                    // This is the resume for the zero_case lowering; its result seeds the loop.
                    let pz = work.emit.fresh_value();
                    work.emit.add_block(
                        zero_resume_name.clone(),
                        cont::Block {
                            params: vec![pz.clone()],
                            region: RegionBuilder::new().finish(cont::Tail::Jump(
                                cont::JumpTarget {
                                    target: loop_block_name.clone(),
                                    params: vec![zero_nat, pz],
                                },
                            )),
                        },
                    );

                    // loop_block(i, acc):
                    //   cmp = NatEql(i, n)
                    //   cmp==0 (i≠n) → body_block(i, acc)
                    //   cmp≠0 (i=n) → exit_block(acc)
                    let i = work.emit.fresh_value();
                    let acc = work.emit.fresh_value();
                    let loop_block_region = {
                        let mut sub = work.emit.subregion();
                        let cmp = sub.fresh(cont::Value::Eval(cont::Code::NatEql(i.clone(), head)));
                        sub.finish(cont::Tail::Match(cont::MatchTarget {
                            operand: cmp,
                            cases: BTreeMap::from([(
                                0,
                                cont::JumpTarget {
                                    target: body_block_name.clone(),
                                    params: vec![i.clone(), acc.clone()],
                                },
                            )]),
                            default: Some(cont::JumpTarget {
                                target: exit_block_name.clone(),
                                params: vec![acc.clone()],
                            }),
                        }))
                    };
                    work.emit.add_block(
                        loop_block_name.clone(),
                        cont::Block {
                            params: vec![i.clone(), acc.clone()],
                            region: loop_block_region,
                        },
                    );

                    // body_block(i2, acc2):
                    //   succ_frame = {pred→i2, ih→acc2}
                    //   lower succ_case with body_resume as resume
                    //
                    // body_resume(acc'):
                    //   i' = NatAdd(i2, 1)   -- i2 accessible from enclosing body_block frame
                    //   jump loop_block(i', acc')
                    let i2 = work.emit.fresh_value();
                    let acc2 = work.emit.fresh_value();
                    let body_resume_name = work.emit.fresh_block();
                    let acc_prime = work.emit.fresh_value();

                    let body_resume_region = {
                        let mut sub = work.emit.subregion();
                        let i_prime =
                            sub.fresh(cont::Value::Eval(cont::Code::NatAdd(i2.clone(), one_nat)));
                        sub.finish(cont::Tail::Jump(cont::JumpTarget {
                            target: loop_block_name,
                            params: vec![i_prime, acc_prime.clone()],
                        }))
                    };

                    let succ_frame =
                        frame.extended([(pred.clone(), i2.clone()), (ih.clone(), acc2.clone())]);
                    let body_region = {
                        let mut body = work.emit.subregion();
                        body.add_block(
                            body_resume_name.clone(),
                            cont::Block {
                                params: vec![acc_prime],
                                region: body_resume_region,
                            },
                        );
                        let body_tail = Work {
                            lowerer: &mut *work.lowerer,
                            emit: &mut body,
                        }
                        .lower_tail(
                            succ_case,
                            &succ_frame,
                            &body_resume_name,
                        );

                        body.finish(body_tail)
                    };
                    work.emit.add_block(
                        body_block_name,
                        cont::Block {
                            params: vec![i2, acc2],
                            region: body_region,
                        },
                    );

                    // exit_block(acc_final): return the accumulated result.
                    let acc_final = work.emit.fresh_value();
                    work.emit.add_block(
                        exit_block_name,
                        cont::Block {
                            params: vec![acc_final.clone()],
                            region: RegionBuilder::new().finish(cont::Tail::Jump(
                                cont::JumpTarget {
                                    target: resume.clone(),
                                    params: vec![acc_final],
                                },
                            )),
                        },
                    );

                    // Outer tail: lower zero_case; its result flows into zero_resume → loop.
                    work.lower_tail(zero_case, frame, &zero_resume_name)
                }),
            ),
            ersd::Term::NatMatch(ersd::NatMatch::Dispatch {
                head: nm_head,
                cases: nm_cases,
                default: nm_default,
            }) => self.lower_value_name(
                nm_head,
                frame,
                Box::new(move |work, head| {
                    let mut cases = BTreeMap::new();

                    for (val, branch) in nm_cases.iter() {
                        let block = work.emit.fresh_block();
                        let region = work.in_subregion(|w| w.lower_tail(branch, frame, resume));
                        work.emit.add_block(
                            block.clone(),
                            cont::Block {
                                params: vec![],
                                region,
                            },
                        );
                        cases.insert(
                            *val,
                            cont::JumpTarget {
                                target: block,
                                params: vec![],
                            },
                        );
                    }

                    let default_block = work.emit.fresh_block();
                    let region = work.in_subregion(|w| w.lower_tail(nm_default, frame, resume));
                    work.emit.add_block(
                        default_block.clone(),
                        cont::Block {
                            params: vec![],
                            region,
                        },
                    );

                    cont::Tail::Match(cont::MatchTarget {
                        operand: head,
                        cases,
                        default: Some(cont::JumpTarget {
                            target: default_block,
                            params: vec![],
                        }),
                    })
                }),
            ),
            ersd::Term::Let(let_) => {
                let name = let_.name.clone();
                self.lower_value_name(
                    &let_.body,
                    frame,
                    Box::new(move |work, body| {
                        let frame = frame.extended([(name, body)]);
                        work.lower_tail(&let_.tail, &frame, resume)
                    }),
                )
            }
            ersd::Term::Rec(letrec) => self.lower_rec(
                letrec,
                frame,
                Box::new(move |work, frame| work.lower_tail(&letrec.tail, frame, resume)),
            ),
            _ => self.lower_value_name(
                term,
                frame,
                Box::new(move |_, value| {
                    cont::Tail::Jump(cont::JumpTarget {
                        target: resume.clone(),
                        params: vec![value],
                    })
                }),
            ),
        }
    }
}
