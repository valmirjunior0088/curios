use {
    super::{Cont, Frame, Work},
    crate::{cont, ersd},
};

fn lower_pure_unary_code(
    work: &mut Work,
    operand: &ersd::Term,
    frame: &Frame,
    code: impl FnOnce(cont::ValueName) -> cont::Code,
) -> cont::ValueName {
    let operand = work.lower_pure_name(operand, frame);
    work.fresh(cont::Value::Eval(code(operand)))
}

fn lower_pure_binary_code(
    work: &mut Work,
    left: &ersd::Term,
    right: &ersd::Term,
    frame: &Frame,
    code: impl FnOnce(cont::ValueName, cont::ValueName) -> cont::Code,
) -> cont::ValueName {
    let left = work.lower_pure_name(left, frame);
    let right = work.lower_pure_name(right, frame);
    work.fresh(cont::Value::Eval(code(left, right)))
}

fn lower_pure_ternary_code(
    work: &mut Work,
    first: &ersd::Term,
    second: &ersd::Term,
    third: &ersd::Term,
    frame: &Frame,
    code: impl FnOnce(cont::ValueName, cont::ValueName, cont::ValueName) -> cont::Code,
) -> cont::ValueName {
    let first = work.lower_pure_name(first, frame);
    let second = work.lower_pure_name(second, frame);
    let third = work.lower_pure_name(third, frame);
    work.fresh(cont::Value::Eval(code(first, second, third)))
}

fn lower_pure_names(work: &mut Work, terms: &[ersd::Term], frame: &Frame) -> Vec<cont::ValueName> {
    terms
        .iter()
        .map(|term| work.lower_pure_name(term, frame))
        .collect()
}

fn lower_unary_code<'b>(
    work: &mut Work,
    operand: &'b ersd::Term,
    frame: &'b Frame,
    cont: Cont<'b>,
    code: impl FnOnce(cont::ValueName) -> cont::Code + 'b,
) -> cont::Tail {
    work.lower_value_name(
        operand,
        frame,
        Cont::new(move |work, operand| {
            let value = work.fresh(cont::Value::Eval(code(operand)));

            cont.call(work, value)
        }),
    )
}

fn lower_binary_code<'b>(
    work: &mut Work,
    left: &'b ersd::Term,
    right: &'b ersd::Term,
    frame: &'b Frame,
    cont: Cont<'b>,
    code: impl FnOnce(cont::ValueName, cont::ValueName) -> cont::Code + 'b,
) -> cont::Tail {
    work.lower_value_name(
        left,
        frame,
        Cont::new(move |work, left| {
            work.lower_value_name(
                right,
                frame,
                Cont::new(move |work, right| {
                    let value = work.fresh(cont::Value::Eval(code(left, right)));

                    cont.call(work, value)
                }),
            )
        }),
    )
}

fn lower_ternary_code<'b>(
    work: &mut Work,
    first: &'b ersd::Term,
    second: &'b ersd::Term,
    third: &'b ersd::Term,
    frame: &'b Frame,
    cont: Cont<'b>,
    code: impl FnOnce(cont::ValueName, cont::ValueName, cont::ValueName) -> cont::Code + 'b,
) -> cont::Tail {
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
                            let value = work.fresh(cont::Value::Eval(code(first, second, third)));

                            cont.call(work, value)
                        }),
                    )
                }),
            )
        }),
    )
}

fn lower_lst<'b>(
    work: &mut Work,
    elements: &'b [ersd::Term],
    frame: &'b Frame,
    mut names: Vec<cont::ValueName>,
    cont: Cont<'b>,
) -> cont::Tail {
    match elements {
        [] => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Arr(names)));

            cont.call(work, value)
        }
        [head, tail @ ..] => work.lower_value_name(
            head,
            frame,
            Cont::new(move |work, name| {
                names.push(name);
                lower_lst(work, tail, frame, names, cont)
            }),
        ),
    }
}

fn lower_bin_concat<'b>(
    work: &mut Work,
    operands: &'b [ersd::Term],
    frame: &'b Frame,
    mut names: Vec<cont::ValueName>,
    cont: Cont<'b>,
) -> cont::Tail {
    match operands {
        [] => {
            let value = work.fresh(cont::Value::Eval(cont::Code::BinConcat(names)));

            cont.call(work, value)
        }
        [head, tail @ ..] => work.lower_value_name(
            head,
            frame,
            Cont::new(move |work, name| {
                names.push(name);
                lower_bin_concat(work, tail, frame, names, cont)
            }),
        ),
    }
}

fn lower_arr_concat<'b>(
    work: &mut Work,
    operands: &'b [ersd::Term],
    frame: &'b Frame,
    mut names: Vec<cont::ValueName>,
    cont: Cont<'b>,
) -> cont::Tail {
    match operands {
        [] => {
            let value = work.fresh(cont::Value::Eval(cont::Code::ArrConcat(names)));

            cont.call(work, value)
        }
        [head, tail @ ..] => work.lower_value_name(
            head,
            frame,
            Cont::new(move |work, name| {
                names.push(name);
                lower_arr_concat(work, tail, frame, names, cont)
            }),
        ),
    }
}

pub fn lower_pure_prim(work: &mut Work, prim: &ersd::PurePrim, frame: &Frame) -> cont::ValueName {
    match prim {
        ersd::PurePrim::Nat(value) => work.fresh(cont::Value::Pure(cont::Data::Nat(*value))),
        ersd::PurePrim::NatEql(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatEql)
        }
        ersd::PurePrim::NatNeq(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatNeq)
        }
        ersd::PurePrim::NatAdd(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatAdd)
        }
        ersd::PurePrim::NatSub(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatSub)
        }
        ersd::PurePrim::NatMul(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatMul)
        }
        ersd::PurePrim::NatLt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatLt)
        }
        ersd::PurePrim::NatDiv(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatDiv)
        }
        ersd::PurePrim::NatRem(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatRem)
        }
        ersd::PurePrim::NatGt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatGt)
        }
        ersd::PurePrim::NatLte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatLte)
        }
        ersd::PurePrim::NatGte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatGte)
        }
        ersd::PurePrim::Int(value) => work.fresh(cont::Value::Pure(cont::Data::Int(*value))),
        ersd::PurePrim::Flt(value) => work.fresh(cont::Value::Pure(cont::Data::Flt(*value))),
        ersd::PurePrim::IntEql(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntEql)
        }
        ersd::PurePrim::IntNeq(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntNeq)
        }
        ersd::PurePrim::IntAdd(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntAdd)
        }
        ersd::PurePrim::IntSub(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntSub)
        }
        ersd::PurePrim::IntMul(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntMul)
        }
        ersd::PurePrim::IntDiv(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntDiv)
        }
        ersd::PurePrim::IntRem(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntRem)
        }
        ersd::PurePrim::IntLt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntLt)
        }
        ersd::PurePrim::IntGt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntGt)
        }
        ersd::PurePrim::IntLte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntLte)
        }
        ersd::PurePrim::IntGte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntGte)
        }
        ersd::PurePrim::FltAdd(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltAdd)
        }
        ersd::PurePrim::FltSub(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltSub)
        }
        ersd::PurePrim::FltMul(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltMul)
        }
        ersd::PurePrim::FltDiv(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltDiv)
        }
        ersd::PurePrim::FltEql(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltEql)
        }
        ersd::PurePrim::FltNeq(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltNeq)
        }
        ersd::PurePrim::FltLt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltLt)
        }
        ersd::PurePrim::FltGt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltGt)
        }
        ersd::PurePrim::FltLte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltLte)
        }
        ersd::PurePrim::FltGte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltGte)
        }
        ersd::PurePrim::FltMin(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltMin)
        }
        ersd::PurePrim::FltMax(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltMax)
        }
        ersd::PurePrim::FltNeg(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltNeg)
        }
        ersd::PurePrim::FltAbs(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltAbs)
        }
        ersd::PurePrim::FltSqrt(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltSqrt)
        }
        ersd::PurePrim::FltFloor(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltFloor)
        }
        ersd::PurePrim::FltCeil(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltCeil)
        }
        ersd::PurePrim::FltTrunc(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltTrunc)
        }
        ersd::PurePrim::FltNearest(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltNearest)
        }
        ersd::PurePrim::NatToStr(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::NatToStr)
        }
        ersd::PurePrim::IntToStr(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::IntToStr)
        }
        ersd::PurePrim::FltToStr(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltToStr)
        }
        ersd::PurePrim::FltToLeBin(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltToLeBin)
        }
        ersd::PurePrim::NatToInt(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::NatToInt)
        }
        ersd::PurePrim::NatToFlt(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::NatToFlt)
        }
        ersd::PurePrim::IntToNat(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::IntToNat)
        }
        ersd::PurePrim::IntToFlt(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::IntToFlt)
        }
        ersd::PurePrim::FltToNat(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltToNat)
        }
        ersd::PurePrim::FltToInt(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltToInt)
        }
        ersd::PurePrim::Bin(bytes) => work.fresh(cont::Value::Pure(cont::Data::Bin(bytes.clone()))),
        ersd::PurePrim::BinLen(bin) => lower_pure_unary_code(work, bin, frame, cont::Code::BinLen),
        ersd::PurePrim::BinEql(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::BinEql)
        }
        ersd::PurePrim::BinGet(bin, idx) => {
            lower_pure_binary_code(work, bin, idx, frame, cont::Code::BinGet)
        }
        ersd::PurePrim::BinSlice(bin, start, end) => {
            lower_pure_ternary_code(work, bin, start, end, frame, cont::Code::BinSlice)
        }
        ersd::PurePrim::BinAppend(bin, byte) => {
            lower_pure_binary_code(work, bin, byte, frame, cont::Code::BinAppend)
        }
        ersd::PurePrim::BinConcat(operands) => {
            let names = lower_pure_names(work, operands, frame);

            work.fresh(cont::Value::Eval(cont::Code::BinConcat(names)))
        }
        ersd::PurePrim::Arr(elements) => {
            let names = lower_pure_names(work, elements, frame);

            work.fresh(cont::Value::Pure(cont::Data::Arr(names)))
        }
        ersd::PurePrim::ArrLen(lst) => lower_pure_unary_code(work, lst, frame, cont::Code::ArrLen),
        ersd::PurePrim::ArrGet(lst, idx) => {
            lower_pure_binary_code(work, lst, idx, frame, cont::Code::ArrGet)
        }
        ersd::PurePrim::ArrSlice(lst, start, end) => {
            lower_pure_ternary_code(work, lst, start, end, frame, cont::Code::ArrSlice)
        }
        ersd::PurePrim::ArrAppend(lst, elem) => {
            lower_pure_binary_code(work, lst, elem, frame, cont::Code::ArrAppend)
        }
        ersd::PurePrim::ArrConcat(operands) => {
            let names = lower_pure_names(work, operands, frame);

            work.fresh(cont::Value::Eval(cont::Code::ArrConcat(names)))
        }
        // Handle tokens are plain i32 scalars once types are gone.
        ersd::PurePrim::Io(token) => work.fresh(cont::Value::Pure(cont::Data::Nat(*token))),
        ersd::PurePrim::Unit => work.fresh(cont::Value::Pure(cont::Data::Tpl(vec![]))),
    }
}

pub fn lower_value_prim<'b>(
    work: &mut Work,
    prim: &'b ersd::Prim,
    frame: &'b Frame,
    cont: Cont<'b>,
) -> cont::Tail {
    match prim {
        ersd::Prim::Pure(pure_prim) => lower_value_pure_prim(work, pure_prim, frame, cont),
        ersd::Prim::Host(ersd::HostPrim::IoRead(handle, count)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                work.lower_value_name(
                    count,
                    frame,
                    Cont::new(move |work, count| {
                        // The read `Bin` arrives as the resume block's lone param
                        // and gets threaded straight into the surrounding
                        // continuation.
                        let resume = work.fresh_block();
                        let read = work.fresh_value();
                        let read_clone = read.clone();
                        work.add_resume_block(resume.clone(), vec![read], move |inner| {
                            cont.call(inner, read_clone)
                        });
                        cont::Tail::Host(cont::HostTarget::IoRead {
                            handle,
                            count,
                            resume,
                        })
                    }),
                )
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoWrite(handle, bytes)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                work.lower_value_name(
                    bytes,
                    frame,
                    Cont::new(move |work, bytes| {
                        // After `Io.write` completes the IR continues with `()` —
                        // the resume block materialises that unit and hands it to
                        // the surrounding continuation.
                        let resume = work.fresh_block();
                        work.add_resume_block(resume.clone(), vec![], move |inner| {
                            let unit = inner.fresh(cont::Value::Pure(cont::Data::Tpl(vec![])));
                            cont.call(inner, unit)
                        });
                        cont::Tail::Host(cont::HostTarget::IoWrite {
                            handle,
                            bytes,
                            resume,
                        })
                    }),
                )
            }),
        ),
    }
}

fn lower_value_pure_prim<'b>(
    work: &mut Work,
    prim: &'b ersd::PurePrim,
    frame: &'b Frame,
    cont: Cont<'b>,
) -> cont::Tail {
    match prim {
        ersd::PurePrim::Nat(value) => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Nat(*value)));

            cont.call(work, value)
        }
        // Handle tokens are plain i32 scalars once types are gone.
        ersd::PurePrim::Io(token) => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Nat(*token)));

            cont.call(work, value)
        }
        ersd::PurePrim::NatEql(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatEql)
        }
        ersd::PurePrim::NatAdd(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatAdd)
        }
        ersd::PurePrim::NatSub(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatSub)
        }
        ersd::PurePrim::NatMul(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatMul)
        }
        ersd::PurePrim::NatLt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatLt)
        }
        ersd::PurePrim::NatNeq(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatNeq)
        }
        ersd::PurePrim::NatDiv(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatDiv)
        }
        ersd::PurePrim::NatRem(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatRem)
        }
        ersd::PurePrim::NatGt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatGt)
        }
        ersd::PurePrim::NatLte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatLte)
        }
        ersd::PurePrim::NatGte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatGte)
        }
        ersd::PurePrim::Int(value) => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Int(*value)));

            cont.call(work, value)
        }
        ersd::PurePrim::Flt(value) => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Flt(*value)));

            cont.call(work, value)
        }
        ersd::PurePrim::IntEql(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntEql)
        }
        ersd::PurePrim::IntAdd(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntAdd)
        }
        ersd::PurePrim::IntSub(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntSub)
        }
        ersd::PurePrim::IntMul(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntMul)
        }
        ersd::PurePrim::IntNeq(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntNeq)
        }
        ersd::PurePrim::IntDiv(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntDiv)
        }
        ersd::PurePrim::IntRem(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntRem)
        }
        ersd::PurePrim::IntLt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntLt)
        }
        ersd::PurePrim::IntGt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntGt)
        }
        ersd::PurePrim::IntLte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntLte)
        }
        ersd::PurePrim::IntGte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntGte)
        }
        ersd::PurePrim::FltAdd(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltAdd)
        }
        ersd::PurePrim::FltSub(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltSub)
        }
        ersd::PurePrim::FltMul(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltMul)
        }
        ersd::PurePrim::FltDiv(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltDiv)
        }
        ersd::PurePrim::FltEql(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltEql)
        }
        ersd::PurePrim::FltNeq(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltNeq)
        }
        ersd::PurePrim::FltLt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltLt)
        }
        ersd::PurePrim::FltGt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltGt)
        }
        ersd::PurePrim::FltLte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltLte)
        }
        ersd::PurePrim::FltGte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltGte)
        }
        ersd::PurePrim::FltMin(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltMin)
        }
        ersd::PurePrim::FltMax(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltMax)
        }
        ersd::PurePrim::FltNeg(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltNeg)
        }
        ersd::PurePrim::FltAbs(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltAbs)
        }
        ersd::PurePrim::FltSqrt(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltSqrt)
        }
        ersd::PurePrim::FltFloor(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltFloor)
        }
        ersd::PurePrim::FltCeil(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltCeil)
        }
        ersd::PurePrim::FltTrunc(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltTrunc)
        }
        ersd::PurePrim::FltNearest(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltNearest)
        }
        ersd::PurePrim::NatToStr(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::NatToStr)
        }
        ersd::PurePrim::IntToStr(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::IntToStr)
        }
        ersd::PurePrim::FltToStr(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltToStr)
        }
        ersd::PurePrim::FltToLeBin(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltToLeBin)
        }
        ersd::PurePrim::NatToInt(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::NatToInt)
        }
        ersd::PurePrim::IntToNat(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::IntToNat)
        }
        ersd::PurePrim::IntToFlt(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::IntToFlt)
        }
        ersd::PurePrim::NatToFlt(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::NatToFlt)
        }
        ersd::PurePrim::FltToInt(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltToInt)
        }
        ersd::PurePrim::FltToNat(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltToNat)
        }
        ersd::PurePrim::Bin(bytes) => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Bin(bytes.clone())));

            cont.call(work, value)
        }
        ersd::PurePrim::BinLen(bin) => lower_unary_code(work, bin, frame, cont, cont::Code::BinLen),
        ersd::PurePrim::BinEql(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::BinEql)
        }
        ersd::PurePrim::BinGet(bin, idx) => {
            lower_binary_code(work, bin, idx, frame, cont, cont::Code::BinGet)
        }
        ersd::PurePrim::BinSlice(bin, start, end) => {
            lower_ternary_code(work, bin, start, end, frame, cont, cont::Code::BinSlice)
        }
        ersd::PurePrim::BinAppend(bin, byte) => {
            lower_binary_code(work, bin, byte, frame, cont, cont::Code::BinAppend)
        }
        ersd::PurePrim::BinConcat(operands) => {
            lower_bin_concat(work, operands, frame, vec![], cont)
        }
        ersd::PurePrim::Arr(elements) => lower_lst(work, elements, frame, vec![], cont),
        ersd::PurePrim::ArrLen(lst) => lower_unary_code(work, lst, frame, cont, cont::Code::ArrLen),
        ersd::PurePrim::ArrGet(lst, idx) => {
            lower_binary_code(work, lst, idx, frame, cont, cont::Code::ArrGet)
        }
        ersd::PurePrim::ArrSlice(lst, start, end) => {
            lower_ternary_code(work, lst, start, end, frame, cont, cont::Code::ArrSlice)
        }
        ersd::PurePrim::ArrAppend(lst, elem) => {
            lower_binary_code(work, lst, elem, frame, cont, cont::Code::ArrAppend)
        }
        ersd::PurePrim::ArrConcat(operands) => {
            lower_arr_concat(work, operands, frame, vec![], cont)
        }
        ersd::PurePrim::Unit => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Tpl(vec![])));
            cont.call(work, value)
        }
    }
}
