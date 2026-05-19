use {
    super::{Entropy, Frame, FrameEntropy},
    crate::{cont, ersd},
};

fn unsupported_rec_item(term: &ersd::Term) -> ! {
    panic!(
        "`to_cont` does not support this `rec` item in the MVP: \
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
        letrec: &ersd::Rec,
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
            ersd::Term::Prim(ersd::Prim::NatNeq(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatNeq(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::NatDiv(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatDiv(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::NatRem(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatRem(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::NatGt(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatGt(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::NatLte(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatLte(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::NatGte(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatGte(left, right)),
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
            ersd::Term::Prim(ersd::Prim::IntNeq(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntNeq(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::IntDiv(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntDiv(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::IntRem(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntRem(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::IntLt(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntLt(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::IntGt(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntGt(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::IntLte(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntLte(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::IntGte(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntGte(left, right)),
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
            ersd::Term::Prim(ersd::Prim::FltDiv(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltDiv(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltEql(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltEql(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltNeq(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltNeq(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltLt(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltLt(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltGt(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltGt(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltLte(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltLte(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltGte(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltGte(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltMin(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltMin(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltMax(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltMax(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltNeg(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltNeg(operand)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltAbs(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltAbs(operand)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltSqrt(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltSqrt(operand)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltFloor(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltFloor(operand)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltCeil(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltCeil(operand)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltTrunc(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltTrunc(operand)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltNearest(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltNearest(operand)),
                )
            }
            ersd::Term::Prim(ersd::Prim::NatToInt(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatToInt(operand)),
                )
            }
            ersd::Term::Prim(ersd::Prim::IntToNat(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntToNat(operand)),
                )
            }
            ersd::Term::Prim(ersd::Prim::IntToFlt(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::IntToFlt(operand)),
                )
            }
            ersd::Term::Prim(ersd::Prim::NatToFlt(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::NatToFlt(operand)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltToInt(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltToInt(operand)),
                )
            }
            ersd::Term::Prim(ersd::Prim::FltToNat(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::FltToNat(operand)),
                )
            }
            ersd::Term::Prim(ersd::Prim::Bin(bytes)) => emit_fresh_value(
                state,
                builder,
                cont::Value::Pure(cont::Data::Bin(bytes.clone())),
            ),
            ersd::Term::Prim(ersd::Prim::BinLen(bin)) => {
                let bin = self.lower_letrec_name(bin, frame, state, builder);

                emit_fresh_value(state, builder, cont::Value::Eval(cont::Code::BinLen(bin)))
            }
            ersd::Term::Prim(ersd::Prim::BinEql(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::BinEql(left, right)),
                )
            }
            ersd::Term::Prim(ersd::Prim::BinGet(bin, idx)) => {
                let bin = self.lower_letrec_name(bin, frame, state, builder);
                let idx = self.lower_letrec_name(idx, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::BinGet(bin, idx)),
                )
            }
            ersd::Term::Prim(ersd::Prim::BinSlice(bin, start, end)) => {
                let bin = self.lower_letrec_name(bin, frame, state, builder);
                let start = self.lower_letrec_name(start, frame, state, builder);
                let end = self.lower_letrec_name(end, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::BinSlice(bin, start, end)),
                )
            }
            ersd::Term::Prim(ersd::Prim::BinAppend(bin, byte)) => {
                let bin = self.lower_letrec_name(bin, frame, state, builder);
                let byte = self.lower_letrec_name(byte, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::BinAppend(bin, byte)),
                )
            }
            ersd::Term::Prim(ersd::Prim::BinConcat(operands)) => {
                let names = operands
                    .iter()
                    .map(|op| self.lower_letrec_name(op, frame, state, builder))
                    .collect();

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::BinConcat(names)),
                )
            }
            ersd::Term::Prim(ersd::Prim::Arr(elements)) => {
                let names = elements
                    .iter()
                    .map(|e| self.lower_letrec_name(e, frame, state, builder))
                    .collect();

                emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Arr(names)))
            }
            ersd::Term::Prim(ersd::Prim::ArrLen(lst)) => {
                let lst = self.lower_letrec_name(lst, frame, state, builder);

                emit_fresh_value(state, builder, cont::Value::Eval(cont::Code::ArrLen(lst)))
            }
            ersd::Term::Prim(ersd::Prim::ArrGet(lst, idx)) => {
                let lst = self.lower_letrec_name(lst, frame, state, builder);
                let idx = self.lower_letrec_name(idx, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::ArrGet(lst, idx)),
                )
            }
            ersd::Term::Prim(ersd::Prim::ArrSlice(lst, start, end)) => {
                let lst = self.lower_letrec_name(lst, frame, state, builder);
                let start = self.lower_letrec_name(start, frame, state, builder);
                let end = self.lower_letrec_name(end, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::ArrSlice(lst, start, end)),
                )
            }
            ersd::Term::Prim(ersd::Prim::ArrAppend(lst, elem)) => {
                let lst = self.lower_letrec_name(lst, frame, state, builder);
                let elem = self.lower_letrec_name(elem, frame, state, builder);

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::ArrAppend(lst, elem)),
                )
            }
            ersd::Term::Prim(ersd::Prim::ArrConcat(operands)) => {
                let names = operands
                    .iter()
                    .map(|op| self.lower_letrec_name(op, frame, state, builder))
                    .collect();

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::ArrConcat(names)),
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
            ersd::Term::Tuple(s) => {
                let field_names = s
                    .fields
                    .iter()
                    .map(|f| self.lower_letrec_name(f, frame, state, builder))
                    .collect::<Vec<_>>();

                emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Pure(cont::Data::Tpl(field_names)),
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
            ersd::Term::Rec(letrec) => {
                let frame = self.lower_letrec_bindings(letrec, frame, state, builder);

                self.lower_letrec_name(&letrec.tail, &frame, state, builder)
            }
            ersd::Term::Apply(_)
            | ersd::Term::Split(_)
            | ersd::Term::Match(_)
            | ersd::Term::NatMatch(_) => unsupported_rec_item(term),
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
            ersd::Term::Prim(ersd::Prim::NatNeq(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::NatNeq(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::NatDiv(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::NatDiv(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::NatRem(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::NatRem(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::NatGt(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::NatGt(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::NatLte(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::NatLte(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::NatGte(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::NatGte(left, right)));
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
            ersd::Term::Prim(ersd::Prim::IntNeq(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::IntNeq(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::IntDiv(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::IntDiv(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::IntRem(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::IntRem(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::IntLt(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::IntLt(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::IntGt(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::IntGt(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::IntLte(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::IntLte(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::IntGte(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::IntGte(left, right)));
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
            ersd::Term::Prim(ersd::Prim::FltDiv(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltDiv(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::FltEql(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltEql(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::FltNeq(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltNeq(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::FltLt(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltLt(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::FltGt(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltGt(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::FltLte(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltLte(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::FltGte(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltGte(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::FltMin(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltMin(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::FltMax(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltMax(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::FltNeg(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltNeg(operand)));
            }
            ersd::Term::Prim(ersd::Prim::FltAbs(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltAbs(operand)));
            }
            ersd::Term::Prim(ersd::Prim::FltSqrt(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltSqrt(operand)));
            }
            ersd::Term::Prim(ersd::Prim::FltFloor(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltFloor(operand)));
            }
            ersd::Term::Prim(ersd::Prim::FltCeil(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltCeil(operand)));
            }
            ersd::Term::Prim(ersd::Prim::FltTrunc(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltTrunc(operand)));
            }
            ersd::Term::Prim(ersd::Prim::FltNearest(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltNearest(operand)));
            }
            ersd::Term::Prim(ersd::Prim::NatToInt(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::NatToInt(operand)));
            }
            ersd::Term::Prim(ersd::Prim::IntToNat(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::IntToNat(operand)));
            }
            ersd::Term::Prim(ersd::Prim::IntToFlt(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::IntToFlt(operand)));
            }
            ersd::Term::Prim(ersd::Prim::NatToFlt(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::NatToFlt(operand)));
            }
            ersd::Term::Prim(ersd::Prim::FltToInt(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltToInt(operand)));
            }
            ersd::Term::Prim(ersd::Prim::FltToNat(operand)) => {
                let operand = self.lower_letrec_name(operand, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::FltToNat(operand)));
            }
            ersd::Term::Prim(ersd::Prim::Bin(bytes)) => {
                builder.add_value(target, cont::Value::Pure(cont::Data::Bin(bytes.clone())));
            }
            ersd::Term::Prim(ersd::Prim::BinLen(bin)) => {
                let bin = self.lower_letrec_name(bin, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::BinLen(bin)));
            }
            ersd::Term::Prim(ersd::Prim::BinEql(left, right)) => {
                let left = self.lower_letrec_name(left, frame, state, builder);
                let right = self.lower_letrec_name(right, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::BinEql(left, right)));
            }
            ersd::Term::Prim(ersd::Prim::BinGet(bin, idx)) => {
                let bin = self.lower_letrec_name(bin, frame, state, builder);
                let idx = self.lower_letrec_name(idx, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::BinGet(bin, idx)));
            }
            ersd::Term::Prim(ersd::Prim::BinSlice(bin, start, end)) => {
                let bin = self.lower_letrec_name(bin, frame, state, builder);
                let start = self.lower_letrec_name(start, frame, state, builder);
                let end = self.lower_letrec_name(end, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::BinSlice(bin, start, end)),
                );
            }
            ersd::Term::Prim(ersd::Prim::BinAppend(bin, byte)) => {
                let bin = self.lower_letrec_name(bin, frame, state, builder);
                let byte = self.lower_letrec_name(byte, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::BinAppend(bin, byte)));
            }
            ersd::Term::Prim(ersd::Prim::BinConcat(operands)) => {
                let names = operands
                    .iter()
                    .map(|op| self.lower_letrec_name(op, frame, state, builder))
                    .collect();

                builder.add_value(target, cont::Value::Eval(cont::Code::BinConcat(names)));
            }
            ersd::Term::Prim(ersd::Prim::Arr(elements)) => {
                let names = elements
                    .iter()
                    .map(|e| self.lower_letrec_name(e, frame, state, builder))
                    .collect();

                builder.add_value(target, cont::Value::Pure(cont::Data::Arr(names)));
            }
            ersd::Term::Prim(ersd::Prim::ArrLen(lst)) => {
                let lst = self.lower_letrec_name(lst, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::ArrLen(lst)));
            }
            ersd::Term::Prim(ersd::Prim::ArrGet(lst, idx)) => {
                let lst = self.lower_letrec_name(lst, frame, state, builder);
                let idx = self.lower_letrec_name(idx, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::ArrGet(lst, idx)));
            }
            ersd::Term::Prim(ersd::Prim::ArrSlice(lst, start, end)) => {
                let lst = self.lower_letrec_name(lst, frame, state, builder);
                let start = self.lower_letrec_name(start, frame, state, builder);
                let end = self.lower_letrec_name(end, frame, state, builder);

                builder.add_value(
                    target,
                    cont::Value::Eval(cont::Code::ArrSlice(lst, start, end)),
                );
            }
            ersd::Term::Prim(ersd::Prim::ArrAppend(lst, elem)) => {
                let lst = self.lower_letrec_name(lst, frame, state, builder);
                let elem = self.lower_letrec_name(elem, frame, state, builder);

                builder.add_value(target, cont::Value::Eval(cont::Code::ArrAppend(lst, elem)));
            }
            ersd::Term::Prim(ersd::Prim::ArrConcat(operands)) => {
                let names = operands
                    .iter()
                    .map(|op| self.lower_letrec_name(op, frame, state, builder))
                    .collect();

                builder.add_value(target, cont::Value::Eval(cont::Code::ArrConcat(names)));
            }
            ersd::Term::Func(func) => {
                let (clsr_name, captured_values) = self.lower_closure(func, frame);
                builder.add_value(
                    target,
                    cont::Value::Pure(cont::Data::Clsr(clsr_name, captured_values)),
                );
            }
            ersd::Term::Tuple(s) => {
                let field_names = s
                    .fields
                    .iter()
                    .map(|f| self.lower_letrec_name(f, frame, state, builder))
                    .collect::<Vec<_>>();

                builder.add_value(target, cont::Value::Pure(cont::Data::Tpl(field_names)));
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
            ersd::Term::Rec(letrec) => {
                let frame = self.lower_letrec_bindings(letrec, frame, state, builder);
                self.lower_letrec_item(&letrec.tail, target, &frame, state, builder);
            }
            ersd::Term::Name(name) => {
                builder.add_value(target, cont::Value::Alias(frame.find(&name.string)));
            }
            ersd::Term::Apply(_)
            | ersd::Term::Split(_)
            | ersd::Term::Match(_)
            | ersd::Term::NatMatch(_) => unsupported_rec_item(term),
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
            ersd::Term::Prim(ersd::Prim::NatEql(left, right)) => self.lower_to_name(
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
            ersd::Term::Prim(ersd::Prim::NatAdd(left, right)) => self.lower_to_name(
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
            ersd::Term::Prim(ersd::Prim::NatSub(left, right)) => self.lower_to_name(
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
            ersd::Term::Prim(ersd::Prim::NatMul(left, right)) => self.lower_to_name(
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
            ersd::Term::Prim(ersd::Prim::NatLt(left, right)) => self.lower_to_name(
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
            ersd::Term::Prim(ersd::Prim::NatNeq(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::NatNeq(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::NatDiv(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::NatDiv(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::NatRem(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::NatRem(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::NatGt(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::NatGt(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::NatLte(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::NatLte(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::NatGte(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::NatGte(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
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
            ersd::Term::Prim(ersd::Prim::IntEql(left, right)) => self.lower_to_name(
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
            ersd::Term::Prim(ersd::Prim::IntAdd(left, right)) => self.lower_to_name(
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
            ersd::Term::Prim(ersd::Prim::IntSub(left, right)) => self.lower_to_name(
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
            ersd::Term::Prim(ersd::Prim::IntMul(left, right)) => self.lower_to_name(
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
            ersd::Term::Prim(ersd::Prim::IntNeq(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::IntNeq(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::IntDiv(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::IntDiv(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::IntRem(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::IntRem(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::IntLt(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::IntLt(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::IntGt(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::IntGt(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::IntLte(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::IntLte(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::IntGte(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::IntGte(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltAdd(left, right)) => self.lower_to_name(
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
            ersd::Term::Prim(ersd::Prim::FltSub(left, right)) => self.lower_to_name(
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
            ersd::Term::Prim(ersd::Prim::FltMul(left, right)) => self.lower_to_name(
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
            ersd::Term::Prim(ersd::Prim::FltDiv(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::FltDiv(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltEql(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::FltEql(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltNeq(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::FltNeq(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltLt(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::FltLt(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltGt(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::FltGt(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltLte(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::FltLte(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltGte(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::FltGte(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltMin(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::FltMin(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltMax(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::FltMax(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltNeg(operand)) => self.lower_to_name(
                operand,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, operand| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::FltNeg(operand)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltAbs(operand)) => self.lower_to_name(
                operand,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, operand| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::FltAbs(operand)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltSqrt(operand)) => self.lower_to_name(
                operand,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, operand| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::FltSqrt(operand)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltFloor(operand)) => self.lower_to_name(
                operand,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, operand| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::FltFloor(operand)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltCeil(operand)) => self.lower_to_name(
                operand,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, operand| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::FltCeil(operand)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltTrunc(operand)) => self.lower_to_name(
                operand,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, operand| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::FltTrunc(operand)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltNearest(operand)) => self.lower_to_name(
                operand,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, operand| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::FltNearest(operand)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            ersd::Term::Prim(ersd::Prim::NatToInt(operand)) => self.lower_to_name(
                operand,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, operand| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::NatToInt(operand)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            ersd::Term::Prim(ersd::Prim::IntToNat(operand)) => self.lower_to_name(
                operand,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, operand| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::IntToNat(operand)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            ersd::Term::Prim(ersd::Prim::IntToFlt(operand)) => self.lower_to_name(
                operand,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, operand| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::IntToFlt(operand)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            ersd::Term::Prim(ersd::Prim::NatToFlt(operand)) => self.lower_to_name(
                operand,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, operand| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::NatToFlt(operand)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltToInt(operand)) => self.lower_to_name(
                operand,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, operand| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::FltToInt(operand)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            ersd::Term::Prim(ersd::Prim::FltToNat(operand)) => self.lower_to_name(
                operand,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, operand| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::FltToNat(operand)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            ersd::Term::Prim(ersd::Prim::Bin(bytes)) => {
                let value = emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Pure(cont::Data::Bin(bytes.clone())),
                );

                cont(self, state, builder, value)
            }
            ersd::Term::Prim(ersd::Prim::BinLen(bin)) => self.lower_to_name(
                bin,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, bin| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::BinLen(bin)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            ersd::Term::Prim(ersd::Prim::BinEql(left, right)) => self.lower_to_name(
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
                                cont::Value::Eval(cont::Code::BinEql(left, right)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::BinGet(bin, idx)) => self.lower_to_name(
                bin,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, bin| {
                    this.lower_to_name(
                        idx,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, idx| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::BinGet(bin, idx)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::BinSlice(bin, start, end)) => self.lower_to_name(
                bin,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, bin| {
                    this.lower_to_name(
                        start,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, start| {
                            this.lower_to_name(
                                end,
                                frame,
                                state,
                                builder,
                                Box::new(move |this, state, builder, end| {
                                    let value = emit_fresh_value(
                                        state,
                                        builder,
                                        cont::Value::Eval(cont::Code::BinSlice(bin, start, end)),
                                    );

                                    cont(this, state, builder, value)
                                }),
                            )
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::BinAppend(bin, byte)) => self.lower_to_name(
                bin,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, bin| {
                    this.lower_to_name(
                        byte,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, byte| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::BinAppend(bin, byte)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::BinConcat(operands)) => {
                self.lower_bin_concat(operands, frame, state, builder, vec![], cont)
            }
            ersd::Term::Prim(ersd::Prim::Arr(elements)) => {
                self.lower_lst(elements, frame, state, builder, vec![], cont)
            }
            ersd::Term::Prim(ersd::Prim::ArrLen(lst)) => self.lower_to_name(
                lst,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, lst| {
                    let value = emit_fresh_value(
                        state,
                        builder,
                        cont::Value::Eval(cont::Code::ArrLen(lst)),
                    );

                    cont(this, state, builder, value)
                }),
            ),
            ersd::Term::Prim(ersd::Prim::ArrGet(lst, idx)) => self.lower_to_name(
                lst,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, lst| {
                    this.lower_to_name(
                        idx,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, idx| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::ArrGet(lst, idx)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::ArrSlice(lst, start, end)) => self.lower_to_name(
                lst,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, lst| {
                    this.lower_to_name(
                        start,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, start| {
                            this.lower_to_name(
                                end,
                                frame,
                                state,
                                builder,
                                Box::new(move |this, state, builder, end| {
                                    let value = emit_fresh_value(
                                        state,
                                        builder,
                                        cont::Value::Eval(cont::Code::ArrSlice(lst, start, end)),
                                    );

                                    cont(this, state, builder, value)
                                }),
                            )
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::ArrAppend(lst, elem)) => self.lower_to_name(
                lst,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, lst| {
                    this.lower_to_name(
                        elem,
                        frame,
                        state,
                        builder,
                        Box::new(move |this, state, builder, elem| {
                            let value = emit_fresh_value(
                                state,
                                builder,
                                cont::Value::Eval(cont::Code::ArrAppend(lst, elem)),
                            );

                            cont(this, state, builder, value)
                        }),
                    )
                }),
            ),
            ersd::Term::Prim(ersd::Prim::ArrConcat(operands)) => {
                self.lower_arr_concat(operands, frame, state, builder, vec![], cont)
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
            ersd::Term::Tuple(s) => {
                self.lower_struct(&s.fields, frame, state, builder, vec![], cont)
            }
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
            ersd::Term::Rec(letrec) => {
                let frame = self.lower_letrec_bindings(letrec, frame, state, builder);
                self.lower_to_name(&letrec.tail, &frame, state, builder, cont)
            }
            ersd::Term::Apply(_)
            | ersd::Term::Split(_)
            | ersd::Term::Match(_)
            | ersd::Term::NatMatch(_) => {
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

    fn lower_lst<'b>(
        &mut self,
        elements: &'b [ersd::Subterm],
        frame: &'b Frame,
        state: &mut FrameEntropy,
        builder: &mut RegionBuilder,
        mut names: Vec<cont::ValueName>,
        cont: Cont<'b>,
    ) -> cont::Tail {
        match elements {
            [] => {
                let value =
                    emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Arr(names)));

                cont(self, state, builder, value)
            }
            [head, tail @ ..] => self.lower_to_name(
                head,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, name| {
                    names.push(name);
                    this.lower_lst(tail, frame, state, builder, names, cont)
                }),
            ),
        }
    }

    fn lower_bin_concat<'b>(
        &mut self,
        operands: &'b [ersd::Subterm],
        frame: &'b Frame,
        state: &mut FrameEntropy,
        builder: &mut RegionBuilder,
        mut names: Vec<cont::ValueName>,
        cont: Cont<'b>,
    ) -> cont::Tail {
        match operands {
            [] => {
                let value = emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::BinConcat(names)),
                );

                cont(self, state, builder, value)
            }
            [head, tail @ ..] => self.lower_to_name(
                head,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, name| {
                    names.push(name);
                    this.lower_bin_concat(tail, frame, state, builder, names, cont)
                }),
            ),
        }
    }

    fn lower_arr_concat<'b>(
        &mut self,
        operands: &'b [ersd::Subterm],
        frame: &'b Frame,
        state: &mut FrameEntropy,
        builder: &mut RegionBuilder,
        mut names: Vec<cont::ValueName>,
        cont: Cont<'b>,
    ) -> cont::Tail {
        match operands {
            [] => {
                let value = emit_fresh_value(
                    state,
                    builder,
                    cont::Value::Eval(cont::Code::ArrConcat(names)),
                );

                cont(self, state, builder, value)
            }
            [head, tail @ ..] => self.lower_to_name(
                head,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, name| {
                    names.push(name);
                    this.lower_arr_concat(tail, frame, state, builder, names, cont)
                }),
            ),
        }
    }

    fn lower_struct<'b>(
        &mut self,
        fields: &'b [ersd::Subterm],
        frame: &'b Frame,
        state: &mut FrameEntropy,
        builder: &mut RegionBuilder,
        mut names: Vec<cont::ValueName>,
        cont: Cont<'b>,
    ) -> cont::Tail {
        match fields {
            [] => {
                let value =
                    emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Tpl(names)));

                cont(self, state, builder, value)
            }
            [head, tail @ ..] => self.lower_to_name(
                head,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, name| {
                    names.push(name);
                    this.lower_struct(tail, frame, state, builder, names, cont)
                }),
            ),
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
            ersd::Term::Match(m) => self.lower_to_name(
                &m.head,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, head| {
                    let mut targets = Vec::with_capacity(m.cases.len());

                    for branch in &m.cases {
                        let block = state.fresh_block();
                        let mut branch_builder = RegionBuilder::new();
                        let tail =
                            this.lower_tail(branch, frame, resume, state, &mut branch_builder);

                        builder.add_block(
                            block.clone(),
                            cont::Block {
                                params: vec![],
                                region: branch_builder.finish(tail),
                            },
                        );

                        targets.push(cont::JumpTarget {
                            target: block,
                            params: vec![],
                        });
                    }

                    cont::Tail::Match(cont::MatchTarget {
                        operand: head,
                        targets,
                        default: None,
                    })
                }),
            ),
            ersd::Term::NatMatch(nat_match) => self.lower_to_name(
                &nat_match.head,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, head| {
                    // Constants shared across the loop.
                    let zero_nat =
                        emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Nat(0)));
                    let one_nat =
                        emit_fresh_value(state, builder, cont::Value::Pure(cont::Data::Nat(1)));

                    // Allocate block names up front so they can be referenced cross-block.
                    let loop_block_name = state.fresh_block();
                    let body_block_name = state.fresh_block();
                    let exit_block_name = state.fresh_block();
                    let zero_resume_name = state.fresh_block();

                    // zero_resume(pz): jump loop_block(0, pz)
                    // This is the resume for the zero_case lowering; its result seeds the loop.
                    let pz = state.fresh_value();
                    builder.add_block(
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
                    let i = state.fresh_value();
                    let acc = state.fresh_value();
                    let loop_block_region = {
                        let mut b = RegionBuilder::new();
                        let cmp = emit_fresh_value(
                            state,
                            &mut b,
                            cont::Value::Eval(cont::Code::NatEql(i.clone(), head)),
                        );
                        b.finish(cont::Tail::Match(cont::MatchTarget {
                            operand: cmp,
                            targets: vec![cont::JumpTarget {
                                target: body_block_name.clone(),
                                params: vec![i.clone(), acc.clone()],
                            }],
                            default: Some(cont::JumpTarget {
                                target: exit_block_name.clone(),
                                params: vec![acc.clone()],
                            }),
                        }))
                    };
                    builder.add_block(
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
                    let i2 = state.fresh_value();
                    let acc2 = state.fresh_value();
                    let body_resume_name = state.fresh_block();
                    let acc_prime = state.fresh_value();

                    let body_resume_region = {
                        let mut b = RegionBuilder::new();
                        let i_prime = emit_fresh_value(
                            state,
                            &mut b,
                            cont::Value::Eval(cont::Code::NatAdd(i2.clone(), one_nat)),
                        );
                        b.finish(cont::Tail::Jump(cont::JumpTarget {
                            target: loop_block_name,
                            params: vec![i_prime, acc_prime.clone()],
                        }))
                    };

                    let mut body_builder = RegionBuilder::new();
                    body_builder.add_block(
                        body_resume_name.clone(),
                        cont::Block {
                            params: vec![acc_prime],
                            region: body_resume_region,
                        },
                    );

                    let succ_frame = frame.extended([
                        (nat_match.pred.clone(), i2.clone()),
                        (nat_match.ih.clone(), acc2.clone()),
                    ]);
                    let body_tail = this.lower_tail(
                        &nat_match.succ_case,
                        &succ_frame,
                        &body_resume_name,
                        state,
                        &mut body_builder,
                    );
                    builder.add_block(
                        body_block_name,
                        cont::Block {
                            params: vec![i2, acc2],
                            region: body_builder.finish(body_tail),
                        },
                    );

                    // exit_block(acc_final): return the accumulated result.
                    let acc_final = state.fresh_value();
                    builder.add_block(
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
                    this.lower_tail(
                        &nat_match.zero_case,
                        frame,
                        &zero_resume_name,
                        state,
                        builder,
                    )
                }),
            ),
            ersd::Term::Split(split) => self.lower_to_name(
                &split.head,
                frame,
                state,
                builder,
                Box::new(move |this, state, builder, head| {
                    let bindings = split
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, name)| {
                            let v = state.fresh_value();
                            builder.add_value(
                                v.clone(),
                                cont::Value::Eval(cont::Code::TplGet(head.clone(), i)),
                            );
                            (name.clone(), v)
                        })
                        .collect::<Vec<_>>();

                    let frame = frame.extended(bindings);

                    this.lower_tail(&split.tail, &frame, resume, state, builder)
                }),
            ),
            ersd::Term::Let(let_) => {
                let name = let_.name.clone();
                self.lower_to_name(
                    &let_.body,
                    frame,
                    state,
                    builder,
                    Box::new(move |this, state, builder, body| {
                        let frame = frame.extended([(name, body)]);
                        this.lower_tail(&let_.tail, &frame, resume, state, builder)
                    }),
                )
            }
            ersd::Term::Rec(letrec) => {
                let frame = self.lower_letrec_bindings(letrec, frame, state, builder);
                self.lower_tail(&letrec.tail, &frame, resume, state, builder)
            }
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
