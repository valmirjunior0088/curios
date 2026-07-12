use {
    super::{Cont, ContMany, Frame, LowerResult, Work},
    curios_cont::{BlockName, CellTarget, Code, Data, HostTarget, Tail, Value, ValueName},
    num_bigint::BigUint,
    std::sync::Arc,
};

fn lower_pure_unary_code(
    work: &mut Work,
    operand: &crate::Term,
    frame: &Frame,
    code: impl FnOnce(ValueName) -> Code,
) -> LowerResult<ValueName> {
    let operand = work.lower_pure_name(operand, frame)?;

    Ok(work.fresh(Value::Eval(code(operand))))
}

fn lower_pure_binary_code(
    work: &mut Work,
    left: &crate::Term,
    right: &crate::Term,
    frame: &Frame,
    code: impl FnOnce(ValueName, ValueName) -> Code,
) -> LowerResult<ValueName> {
    let left = work.lower_pure_name(left, frame)?;
    let right = work.lower_pure_name(right, frame)?;

    Ok(work.fresh(Value::Eval(code(left, right))))
}

fn lower_pure_ternary_code(
    work: &mut Work,
    first: &crate::Term,
    second: &crate::Term,
    third: &crate::Term,
    frame: &Frame,
    code: impl FnOnce(ValueName, ValueName, ValueName) -> Code,
) -> LowerResult<ValueName> {
    let first = work.lower_pure_name(first, frame)?;
    let second = work.lower_pure_name(second, frame)?;
    let third = work.lower_pure_name(third, frame)?;

    Ok(work.fresh(Value::Eval(code(first, second, third))))
}

fn lower_pure_names(
    work: &mut Work,
    terms: &[crate::Term],
    frame: &Frame,
) -> LowerResult<Vec<ValueName>> {
    terms
        .iter()
        .map(|term| work.lower_pure_name(term, frame))
        .collect()
}

/// A resume block that packs a host op's `arity` results into an `arity`-field
/// record and feeds it to `cont`; returns the block name to plug into the
/// `HostTarget`. Arity 0 yields unit, 2/3 the status records.
fn record_resume<'b>(work: &mut Work, arity: usize, cont: Cont<'b>) -> LowerResult<BlockName> {
    let resume = work.fresh_block();
    let fields = (0..arity).map(|_| work.fresh_value()).collect::<Vec<_>>();

    work.add_resume_block(resume.clone(), fields.clone(), move |inner| {
        let record = inner.fresh(Value::Pure(Data::Tpl(fields)));

        cont.call(inner, record)
    })?;

    Ok(resume)
}

/// A resume block that forwards a host op's single result straight to `cont`.
fn forward_resume<'b>(work: &mut Work, cont: Cont<'b>) -> LowerResult<BlockName> {
    let resume = work.fresh_block();
    let value = work.fresh_value();
    let value_clone = value.clone();

    work.add_resume_block(resume.clone(), vec![value], move |inner| {
        cont.call(inner, value_clone)
    })?;

    Ok(resume)
}

fn lower_unary_code<'b>(
    work: &mut Work,
    operand: &'b crate::Term,
    frame: &'b Frame,
    cont: Cont<'b>,
    code: impl FnOnce(ValueName) -> Code + 'b,
) -> LowerResult<Tail> {
    work.lower_value_name(
        operand,
        frame,
        Cont::new(move |work, operand| {
            let value = work.fresh(Value::Eval(code(operand)));

            cont.call(work, value)
        }),
    )
}

fn lower_binary_code<'b>(
    work: &mut Work,
    left: &'b crate::Term,
    right: &'b crate::Term,
    frame: &'b Frame,
    cont: Cont<'b>,
    code: impl FnOnce(ValueName, ValueName) -> Code + 'b,
) -> LowerResult<Tail> {
    work.lower_value_name(
        left,
        frame,
        Cont::new(move |work, left| {
            work.lower_value_name(
                right,
                frame,
                Cont::new(move |work, right| {
                    let value = work.fresh(Value::Eval(code(left, right)));

                    cont.call(work, value)
                }),
            )
        }),
    )
}

fn lower_ternary_code<'b>(
    work: &mut Work,
    first: &'b crate::Term,
    second: &'b crate::Term,
    third: &'b crate::Term,
    frame: &'b Frame,
    cont: Cont<'b>,
    code: impl FnOnce(ValueName, ValueName, ValueName) -> Code + 'b,
) -> LowerResult<Tail> {
    work.lower_value_name(
        first,
        frame,
        Cont::new(move |work, first| {
            work.lower_value_name(
                second,
                frame,
                Cont::new(move |work, second| {
                    work.lower_value_name(
                        third,
                        frame,
                        Cont::new(move |work, third| {
                            let value = work.fresh(Value::Eval(code(first, second, third)));

                            cont.call(work, value)
                        }),
                    )
                }),
            )
        }),
    )
}

impl Work<'_, '_, '_> {
    pub(super) fn lower_pure_prim(
        &mut self,
        prim: &crate::PurePrim,
        frame: &Frame,
    ) -> LowerResult<ValueName> {
        let work = self;

        Ok(match prim {
            crate::PurePrim::Nat(value) => work.fresh(Value::Pure(Data::Nat(*value))),
            crate::PurePrim::NatEql(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatEql)?
            }
            crate::PurePrim::NatNeq(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatNeq)?
            }
            crate::PurePrim::NatAdd(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatAdd)?
            }
            crate::PurePrim::NatSub(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatSub)?
            }
            crate::PurePrim::NatMul(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatMul)?
            }
            crate::PurePrim::NatLt(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatLt)?
            }
            crate::PurePrim::NatDiv(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatDiv)?
            }
            crate::PurePrim::NatRem(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatRem)?
            }
            crate::PurePrim::NatGt(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatGt)?
            }
            crate::PurePrim::NatLte(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatLte)?
            }
            crate::PurePrim::NatGte(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatGte)?
            }
            crate::PurePrim::NatAnd(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatAnd)?
            }
            crate::PurePrim::NatOr(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatOr)?
            }
            crate::PurePrim::NatXor(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatXor)?
            }
            crate::PurePrim::NatShl(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatShl)?
            }
            crate::PurePrim::NatShr(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::NatShr)?
            }
            crate::PurePrim::Int(value) => work.fresh(Value::Pure(Data::Int(*value))),
            crate::PurePrim::Flt(value) => work.fresh(Value::Pure(Data::Flt(*value))),
            crate::PurePrim::IntEql(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntEql)?
            }
            crate::PurePrim::IntNeq(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntNeq)?
            }
            crate::PurePrim::IntAdd(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntAdd)?
            }
            crate::PurePrim::IntSub(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntSub)?
            }
            crate::PurePrim::IntMul(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntMul)?
            }
            crate::PurePrim::IntDiv(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntDiv)?
            }
            crate::PurePrim::IntRem(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntRem)?
            }
            crate::PurePrim::IntLt(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntLt)?
            }
            crate::PurePrim::IntGt(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntGt)?
            }
            crate::PurePrim::IntLte(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntLte)?
            }
            crate::PurePrim::IntGte(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntGte)?
            }
            crate::PurePrim::IntAnd(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntAnd)?
            }
            crate::PurePrim::IntOr(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntOr)?
            }
            crate::PurePrim::IntXor(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntXor)?
            }
            crate::PurePrim::IntShl(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntShl)?
            }
            crate::PurePrim::IntShr(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::IntShr)?
            }
            crate::PurePrim::FltAdd(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::FltAdd)?
            }
            crate::PurePrim::FltSub(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::FltSub)?
            }
            crate::PurePrim::FltMul(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::FltMul)?
            }
            crate::PurePrim::FltDiv(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::FltDiv)?
            }
            crate::PurePrim::FltRem(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::FltRem)?
            }
            crate::PurePrim::FltEql(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::FltEql)?
            }
            crate::PurePrim::FltNeq(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::FltNeq)?
            }
            crate::PurePrim::FltLt(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::FltLt)?
            }
            crate::PurePrim::FltGt(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::FltGt)?
            }
            crate::PurePrim::FltLte(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::FltLte)?
            }
            crate::PurePrim::FltGte(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::FltGte)?
            }
            crate::PurePrim::FltMin(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::FltMin)?
            }
            crate::PurePrim::FltMax(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::FltMax)?
            }
            crate::PurePrim::FltNeg(operand) => {
                lower_pure_unary_code(work, operand, frame, Code::FltNeg)?
            }
            crate::PurePrim::FltAbs(operand) => {
                lower_pure_unary_code(work, operand, frame, Code::FltAbs)?
            }
            crate::PurePrim::FltSqrt(operand) => {
                lower_pure_unary_code(work, operand, frame, Code::FltSqrt)?
            }
            crate::PurePrim::FltFloor(operand) => {
                lower_pure_unary_code(work, operand, frame, Code::FltFloor)?
            }
            crate::PurePrim::FltCeil(operand) => {
                lower_pure_unary_code(work, operand, frame, Code::FltCeil)?
            }
            crate::PurePrim::FltTrunc(operand) => {
                lower_pure_unary_code(work, operand, frame, Code::FltTrunc)?
            }
            crate::PurePrim::FltNearest(operand) => {
                lower_pure_unary_code(work, operand, frame, Code::FltNearest)?
            }
            crate::PurePrim::FltToLeBin(operand) => {
                lower_pure_unary_code(work, operand, frame, Code::FltToLeBin)?
            }
            crate::PurePrim::FltOfLeBin(operand) => {
                lower_pure_unary_code(work, operand, frame, Code::FltOfLeBin)?
            }
            crate::PurePrim::NatToInt(operand) => {
                lower_pure_unary_code(work, operand, frame, Code::NatToInt)?
            }
            crate::PurePrim::NatToFlt(operand) => {
                lower_pure_unary_code(work, operand, frame, Code::NatToFlt)?
            }
            crate::PurePrim::IntToNat(operand) => {
                lower_pure_unary_code(work, operand, frame, Code::IntToNat)?
            }
            crate::PurePrim::IntToFlt(operand) => {
                lower_pure_unary_code(work, operand, frame, Code::IntToFlt)?
            }
            crate::PurePrim::FltToNat(operand) => {
                lower_pure_unary_code(work, operand, frame, Code::FltToNat)?
            }
            crate::PurePrim::FltToInt(operand) => {
                lower_pure_unary_code(work, operand, frame, Code::FltToInt)?
            }
            crate::PurePrim::Bin(bytes) => work.fresh(Value::Pure(Data::Bin(bytes.clone()))),
            crate::PurePrim::BinLen(bin) => lower_pure_unary_code(work, bin, frame, Code::BinLen)?,
            crate::PurePrim::BinEql(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::BinEql)?
            }
            // Handle identity is byte identity: the rep is bytes from here down.
            crate::PurePrim::IoEql(left, right) => {
                lower_pure_binary_code(work, left, right, frame, Code::BinEql)?
            }
            crate::PurePrim::BinGet(bin, idx) => {
                lower_pure_binary_code(work, bin, idx, frame, Code::BinGet)?
            }
            crate::PurePrim::BinSlice(bin, start, end) => {
                lower_pure_ternary_code(work, bin, start, end, frame, Code::BinSlice)?
            }
            crate::PurePrim::BinAppend(bin, byte) => {
                lower_pure_binary_code(work, bin, byte, frame, Code::BinAppend)?
            }
            crate::PurePrim::BinConcat(operands) => {
                let names = lower_pure_names(work, operands, frame)?;

                work.fresh(Value::Eval(Code::BinConcat(names)))
            }
            crate::PurePrim::Lst(elements) => {
                let names = lower_pure_names(work, elements, frame)?;

                work.fresh(Value::Pure(Data::Lst(names)))
            }
            crate::PurePrim::LstLen(lst) => lower_pure_unary_code(work, lst, frame, Code::LstLen)?,
            crate::PurePrim::LstGet(lst, idx) => {
                lower_pure_binary_code(work, lst, idx, frame, Code::LstGet)?
            }
            crate::PurePrim::LstSlice(lst, start, end) => {
                lower_pure_ternary_code(work, lst, start, end, frame, Code::LstSlice)?
            }
            crate::PurePrim::LstAppend(lst, elem) => {
                lower_pure_binary_code(work, lst, elem, frame, Code::LstAppend)?
            }
            crate::PurePrim::LstConcat(operands) => {
                let names = lower_pure_names(work, operands, frame)?;

                work.fresh(Value::Eval(Code::LstConcat(names)))
            }
            crate::PurePrim::LstMap(src, f) => {
                lower_pure_binary_code(work, src, f, frame, Code::LstMap)?
            }
            // A handle erases to its host token bytes: the LE encoding of the token
            // integer, the same `BigUint::to_bytes_le` the runtime mints and keys on.
            // This is the lone spot in the pipeline that knows a handle is bytes.
            crate::PurePrim::Io(token) => {
                work.fresh(Value::Pure(Data::Bin(BigUint::from(*token).to_bytes_le())))
            }
        })
    }

    pub(super) fn lower_value_prim<'b>(
        &mut self,
        prim: &'b crate::Prim,
        frame: &'b Frame,
        cont: Cont<'b>,
    ) -> LowerResult<Tail> {
        let work = self;

        match prim {
            crate::Prim::Pure(pure_prim) => work.lower_value_pure_prim(pure_prim, frame, cont),
            crate::Prim::Host(crate::HostPrim::Foreign(function, args)) => {
                let function = Arc::clone(function);

                work.lower_names(
                    args,
                    frame,
                    Vec::new(),
                    ContMany::new(move |work, operands| {
                        let resume = match function.signature.results.len() {
                            1 => forward_resume(work, cont)?,
                            arity => record_resume(work, arity, cont)?,
                        };

                        Ok(Tail::Host(HostTarget::Foreign {
                            function,
                            operands,
                            resume,
                        }))
                    }),
                )
            }
            crate::Prim::Host(crate::HostPrim::IoExit(code)) => work.lower_value_name(
                code,
                frame,
                Cont::new(move |work, code| {
                    // exit never returns — the host traps — so the resume is dead
                    // code, kept only for the uniform shape (like `IoClose`).
                    let resume = record_resume(work, 0, cont)?;

                    Ok(Tail::Host(HostTarget::IoExit { code, resume }))
                }),
            ),
            crate::Prim::Cell(crate::CellPrim::New(init)) => work.lower_value_name(
                init,
                frame,
                Cont::new(move |work, init| {
                    let resume = forward_resume(work, cont)?;

                    Ok(Tail::Cell(CellTarget::New { init, resume }))
                }),
            ),
            crate::Prim::Cell(crate::CellPrim::Set(cell, value)) => work.lower_value_name(
                cell,
                frame,
                Cont::new(move |work, cell| {
                    work.lower_value_name(
                        value,
                        frame,
                        Cont::new(move |work, value| {
                            let resume = record_resume(work, 0, cont)?;

                            Ok(Tail::Cell(CellTarget::Set {
                                cell,
                                value,
                                resume,
                            }))
                        }),
                    )
                }),
            ),
            crate::Prim::Cell(crate::CellPrim::Get(cell)) => work.lower_value_name(
                cell,
                frame,
                Cont::new(move |work, cell| {
                    let resume = forward_resume(work, cont)?;

                    Ok(Tail::Cell(CellTarget::Get { cell, resume }))
                }),
            ),
        }
    }

    fn lower_value_pure_prim<'b>(
        &mut self,
        prim: &'b crate::PurePrim,
        frame: &'b Frame,
        cont: Cont<'b>,
    ) -> LowerResult<Tail> {
        let work = self;

        match prim {
            crate::PurePrim::Nat(value) => {
                let value = work.fresh(Value::Pure(Data::Nat(*value)));

                cont.call(work, value)
            }
            // A handle erases to its host token bytes (see the pure-position arm).
            crate::PurePrim::Io(token) => {
                let value = work.fresh(Value::Pure(Data::Bin(BigUint::from(*token).to_bytes_le())));

                cont.call(work, value)
            }
            crate::PurePrim::NatEql(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatEql)
            }
            crate::PurePrim::NatAdd(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatAdd)
            }
            crate::PurePrim::NatSub(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatSub)
            }
            crate::PurePrim::NatMul(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatMul)
            }
            crate::PurePrim::NatLt(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatLt)
            }
            crate::PurePrim::NatNeq(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatNeq)
            }
            crate::PurePrim::NatDiv(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatDiv)
            }
            crate::PurePrim::NatRem(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatRem)
            }
            crate::PurePrim::NatGt(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatGt)
            }
            crate::PurePrim::NatLte(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatLte)
            }
            crate::PurePrim::NatGte(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatGte)
            }
            crate::PurePrim::NatAnd(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatAnd)
            }
            crate::PurePrim::NatOr(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatOr)
            }
            crate::PurePrim::NatXor(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatXor)
            }
            crate::PurePrim::NatShl(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatShl)
            }
            crate::PurePrim::NatShr(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::NatShr)
            }
            crate::PurePrim::Int(value) => {
                let value = work.fresh(Value::Pure(Data::Int(*value)));

                cont.call(work, value)
            }
            crate::PurePrim::Flt(value) => {
                let value = work.fresh(Value::Pure(Data::Flt(*value)));

                cont.call(work, value)
            }
            crate::PurePrim::IntEql(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntEql)
            }
            crate::PurePrim::IntAdd(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntAdd)
            }
            crate::PurePrim::IntSub(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntSub)
            }
            crate::PurePrim::IntMul(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntMul)
            }
            crate::PurePrim::IntNeq(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntNeq)
            }
            crate::PurePrim::IntDiv(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntDiv)
            }
            crate::PurePrim::IntRem(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntRem)
            }
            crate::PurePrim::IntLt(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntLt)
            }
            crate::PurePrim::IntGt(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntGt)
            }
            crate::PurePrim::IntLte(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntLte)
            }
            crate::PurePrim::IntGte(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntGte)
            }
            crate::PurePrim::IntAnd(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntAnd)
            }
            crate::PurePrim::IntOr(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntOr)
            }
            crate::PurePrim::IntXor(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntXor)
            }
            crate::PurePrim::IntShl(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntShl)
            }
            crate::PurePrim::IntShr(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::IntShr)
            }
            crate::PurePrim::FltAdd(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::FltAdd)
            }
            crate::PurePrim::FltSub(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::FltSub)
            }
            crate::PurePrim::FltMul(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::FltMul)
            }
            crate::PurePrim::FltDiv(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::FltDiv)
            }
            crate::PurePrim::FltRem(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::FltRem)
            }
            crate::PurePrim::FltEql(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::FltEql)
            }
            crate::PurePrim::FltNeq(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::FltNeq)
            }
            crate::PurePrim::FltLt(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::FltLt)
            }
            crate::PurePrim::FltGt(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::FltGt)
            }
            crate::PurePrim::FltLte(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::FltLte)
            }
            crate::PurePrim::FltGte(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::FltGte)
            }
            crate::PurePrim::FltMin(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::FltMin)
            }
            crate::PurePrim::FltMax(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::FltMax)
            }
            crate::PurePrim::FltNeg(operand) => {
                lower_unary_code(work, operand, frame, cont, Code::FltNeg)
            }
            crate::PurePrim::FltAbs(operand) => {
                lower_unary_code(work, operand, frame, cont, Code::FltAbs)
            }
            crate::PurePrim::FltSqrt(operand) => {
                lower_unary_code(work, operand, frame, cont, Code::FltSqrt)
            }
            crate::PurePrim::FltFloor(operand) => {
                lower_unary_code(work, operand, frame, cont, Code::FltFloor)
            }
            crate::PurePrim::FltCeil(operand) => {
                lower_unary_code(work, operand, frame, cont, Code::FltCeil)
            }
            crate::PurePrim::FltTrunc(operand) => {
                lower_unary_code(work, operand, frame, cont, Code::FltTrunc)
            }
            crate::PurePrim::FltNearest(operand) => {
                lower_unary_code(work, operand, frame, cont, Code::FltNearest)
            }
            crate::PurePrim::FltToLeBin(operand) => {
                lower_unary_code(work, operand, frame, cont, Code::FltToLeBin)
            }
            crate::PurePrim::FltOfLeBin(operand) => {
                lower_unary_code(work, operand, frame, cont, Code::FltOfLeBin)
            }
            crate::PurePrim::NatToInt(operand) => {
                lower_unary_code(work, operand, frame, cont, Code::NatToInt)
            }
            crate::PurePrim::IntToNat(operand) => {
                lower_unary_code(work, operand, frame, cont, Code::IntToNat)
            }
            crate::PurePrim::IntToFlt(operand) => {
                lower_unary_code(work, operand, frame, cont, Code::IntToFlt)
            }
            crate::PurePrim::NatToFlt(operand) => {
                lower_unary_code(work, operand, frame, cont, Code::NatToFlt)
            }
            crate::PurePrim::FltToInt(operand) => {
                lower_unary_code(work, operand, frame, cont, Code::FltToInt)
            }
            crate::PurePrim::FltToNat(operand) => {
                lower_unary_code(work, operand, frame, cont, Code::FltToNat)
            }
            crate::PurePrim::Bin(bytes) => {
                let value = work.fresh(Value::Pure(Data::Bin(bytes.clone())));

                cont.call(work, value)
            }
            crate::PurePrim::BinLen(bin) => lower_unary_code(work, bin, frame, cont, Code::BinLen),
            crate::PurePrim::BinEql(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::BinEql)
            }
            // Handle identity is byte identity: the rep is bytes from here down.
            crate::PurePrim::IoEql(left, right) => {
                lower_binary_code(work, left, right, frame, cont, Code::BinEql)
            }
            crate::PurePrim::BinGet(bin, idx) => {
                lower_binary_code(work, bin, idx, frame, cont, Code::BinGet)
            }
            crate::PurePrim::BinSlice(bin, start, end) => {
                lower_ternary_code(work, bin, start, end, frame, cont, Code::BinSlice)
            }
            crate::PurePrim::BinAppend(bin, byte) => {
                lower_binary_code(work, bin, byte, frame, cont, Code::BinAppend)
            }
            crate::PurePrim::BinConcat(operands) => work.lower_names(
                operands,
                frame,
                vec![],
                ContMany::new(move |work, names| {
                    let value = work.fresh(Value::Eval(Code::BinConcat(names)));

                    cont.call(work, value)
                }),
            ),
            crate::PurePrim::Lst(elements) => work.lower_names(
                elements,
                frame,
                vec![],
                ContMany::new(move |work, names| {
                    let value = work.fresh(Value::Pure(Data::Lst(names)));

                    cont.call(work, value)
                }),
            ),
            crate::PurePrim::LstLen(lst) => lower_unary_code(work, lst, frame, cont, Code::LstLen),
            crate::PurePrim::LstGet(lst, idx) => {
                lower_binary_code(work, lst, idx, frame, cont, Code::LstGet)
            }
            crate::PurePrim::LstSlice(lst, start, end) => {
                lower_ternary_code(work, lst, start, end, frame, cont, Code::LstSlice)
            }
            crate::PurePrim::LstAppend(lst, elem) => {
                lower_binary_code(work, lst, elem, frame, cont, Code::LstAppend)
            }
            crate::PurePrim::LstConcat(operands) => work.lower_names(
                operands,
                frame,
                vec![],
                ContMany::new(move |work, names| {
                    let value = work.fresh(Value::Eval(Code::LstConcat(names)));

                    cont.call(work, value)
                }),
            ),
            crate::PurePrim::LstMap(src, f) => {
                lower_binary_code(work, src, f, frame, cont, Code::LstMap)
            }
        }
    }
}
