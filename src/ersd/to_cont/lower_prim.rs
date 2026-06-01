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

fn lower_pure_names(
    work: &mut Work,
    terms: &[ersd::Subterm],
    frame: &Frame,
) -> Vec<cont::ValueName> {
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
        Box::new(move |work, operand| {
            let value = work.fresh(cont::Value::Eval(code(operand)));

            cont(work, value)
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
        Box::new(move |work, left| {
            work.lower_value_name(
                right,
                frame,
                Box::new(move |work, right| {
                    let value = work.fresh(cont::Value::Eval(code(left, right)));

                    cont(work, value)
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
        Box::new(move |work, first| {
            work.lower_value_name(
                second,
                frame,
                Box::new(move |work, second| {
                    work.lower_value_name(
                        third,
                        frame,
                        Box::new(move |work, third| {
                            let value = work.fresh(cont::Value::Eval(code(first, second, third)));

                            cont(work, value)
                        }),
                    )
                }),
            )
        }),
    )
}

fn lower_lst<'b>(
    work: &mut Work,
    elements: &'b [ersd::Subterm],
    frame: &'b Frame,
    mut names: Vec<cont::ValueName>,
    cont: Cont<'b>,
) -> cont::Tail {
    match elements {
        [] => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Arr(names)));

            cont(work, value)
        }
        [head, tail @ ..] => work.lower_value_name(
            head,
            frame,
            Box::new(move |work, name| {
                names.push(name);
                lower_lst(work, tail, frame, names, cont)
            }),
        ),
    }
}

fn lower_bin_concat<'b>(
    work: &mut Work,
    operands: &'b [ersd::Subterm],
    frame: &'b Frame,
    mut names: Vec<cont::ValueName>,
    cont: Cont<'b>,
) -> cont::Tail {
    match operands {
        [] => {
            let value = work.fresh(cont::Value::Eval(cont::Code::BinConcat(names)));

            cont(work, value)
        }
        [head, tail @ ..] => work.lower_value_name(
            head,
            frame,
            Box::new(move |work, name| {
                names.push(name);
                lower_bin_concat(work, tail, frame, names, cont)
            }),
        ),
    }
}

fn lower_arr_concat<'b>(
    work: &mut Work,
    operands: &'b [ersd::Subterm],
    frame: &'b Frame,
    mut names: Vec<cont::ValueName>,
    cont: Cont<'b>,
) -> cont::Tail {
    match operands {
        [] => {
            let value = work.fresh(cont::Value::Eval(cont::Code::ArrConcat(names)));

            cont(work, value)
        }
        [head, tail @ ..] => work.lower_value_name(
            head,
            frame,
            Box::new(move |work, name| {
                names.push(name);
                lower_arr_concat(work, tail, frame, names, cont)
            }),
        ),
    }
}

pub fn lower_pure_prim(work: &mut Work, prim: &ersd::Prim, frame: &Frame) -> cont::ValueName {
    match prim {
        ersd::Prim::Nat(value) => work.fresh(cont::Value::Pure(cont::Data::Nat(*value))),
        ersd::Prim::NatEql(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatEql)
        }
        ersd::Prim::NatNeq(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatNeq)
        }
        ersd::Prim::NatAdd(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatAdd)
        }
        ersd::Prim::NatSub(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatSub)
        }
        ersd::Prim::NatMul(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatMul)
        }
        ersd::Prim::NatLt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatLt)
        }
        ersd::Prim::NatDiv(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatDiv)
        }
        ersd::Prim::NatRem(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatRem)
        }
        ersd::Prim::NatGt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatGt)
        }
        ersd::Prim::NatLte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatLte)
        }
        ersd::Prim::NatGte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatGte)
        }
        ersd::Prim::Int(value) => work.fresh(cont::Value::Pure(cont::Data::Int(*value))),
        ersd::Prim::Flt(value) => work.fresh(cont::Value::Pure(cont::Data::Flt(*value))),
        ersd::Prim::IntEql(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntEql)
        }
        ersd::Prim::IntNeq(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntNeq)
        }
        ersd::Prim::IntAdd(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntAdd)
        }
        ersd::Prim::IntSub(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntSub)
        }
        ersd::Prim::IntMul(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntMul)
        }
        ersd::Prim::IntDiv(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntDiv)
        }
        ersd::Prim::IntRem(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntRem)
        }
        ersd::Prim::IntLt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntLt)
        }
        ersd::Prim::IntGt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntGt)
        }
        ersd::Prim::IntLte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntLte)
        }
        ersd::Prim::IntGte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntGte)
        }
        ersd::Prim::FltAdd(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltAdd)
        }
        ersd::Prim::FltSub(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltSub)
        }
        ersd::Prim::FltMul(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltMul)
        }
        ersd::Prim::FltDiv(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltDiv)
        }
        ersd::Prim::FltEql(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltEql)
        }
        ersd::Prim::FltNeq(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltNeq)
        }
        ersd::Prim::FltLt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltLt)
        }
        ersd::Prim::FltGt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltGt)
        }
        ersd::Prim::FltLte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltLte)
        }
        ersd::Prim::FltGte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltGte)
        }
        ersd::Prim::FltMin(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltMin)
        }
        ersd::Prim::FltMax(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltMax)
        }
        ersd::Prim::FltNeg(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltNeg)
        }
        ersd::Prim::FltAbs(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltAbs)
        }
        ersd::Prim::FltSqrt(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltSqrt)
        }
        ersd::Prim::FltFloor(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltFloor)
        }
        ersd::Prim::FltCeil(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltCeil)
        }
        ersd::Prim::FltTrunc(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltTrunc)
        }
        ersd::Prim::FltNearest(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltNearest)
        }
        ersd::Prim::NatToStr(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::NatToStr)
        }
        ersd::Prim::IntToStr(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::IntToStr)
        }
        ersd::Prim::FltToStr(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltToStr)
        }
        ersd::Prim::NatToInt(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::NatToInt)
        }
        ersd::Prim::NatToFlt(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::NatToFlt)
        }
        ersd::Prim::IntToNat(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::IntToNat)
        }
        ersd::Prim::IntToFlt(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::IntToFlt)
        }
        ersd::Prim::FltToNat(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltToNat)
        }
        ersd::Prim::FltToInt(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltToInt)
        }
        ersd::Prim::Bin(bytes) => work.fresh(cont::Value::Pure(cont::Data::Bin(bytes.clone()))),
        ersd::Prim::BinLen(bin) => lower_pure_unary_code(work, bin, frame, cont::Code::BinLen),
        ersd::Prim::BinEql(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::BinEql)
        }
        ersd::Prim::BinGet(bin, idx) => {
            lower_pure_binary_code(work, bin, idx, frame, cont::Code::BinGet)
        }
        ersd::Prim::BinSlice(bin, start, end) => {
            lower_pure_ternary_code(work, bin, start, end, frame, cont::Code::BinSlice)
        }
        ersd::Prim::BinAppend(bin, byte) => {
            lower_pure_binary_code(work, bin, byte, frame, cont::Code::BinAppend)
        }
        ersd::Prim::BinConcat(operands) => {
            let names = lower_pure_names(work, operands, frame);

            work.fresh(cont::Value::Eval(cont::Code::BinConcat(names)))
        }
        ersd::Prim::Arr(elements) => {
            let names = lower_pure_names(work, elements, frame);

            work.fresh(cont::Value::Pure(cont::Data::Arr(names)))
        }
        ersd::Prim::ArrLen(lst) => lower_pure_unary_code(work, lst, frame, cont::Code::ArrLen),
        ersd::Prim::ArrGet(lst, idx) => {
            lower_pure_binary_code(work, lst, idx, frame, cont::Code::ArrGet)
        }
        ersd::Prim::ArrSlice(lst, start, end) => {
            lower_pure_ternary_code(work, lst, start, end, frame, cont::Code::ArrSlice)
        }
        ersd::Prim::ArrAppend(lst, elem) => {
            lower_pure_binary_code(work, lst, elem, frame, cont::Code::ArrAppend)
        }
        ersd::Prim::ArrConcat(operands) => {
            let names = lower_pure_names(work, operands, frame);

            work.fresh(cont::Value::Eval(cont::Code::ArrConcat(names)))
        }
        ersd::Prim::Unit => work.fresh(cont::Value::Pure(cont::Data::Tpl(vec![]))),
        ersd::Prim::IoPrint(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::IoPrint)
        }
        ersd::Prim::IoRead => work.fresh(cont::Value::Eval(cont::Code::IoRead)),
    }
}

pub fn lower_value_prim<'b>(
    work: &mut Work,
    prim: &'b ersd::Prim,
    frame: &'b Frame,
    cont: Cont<'b>,
) -> cont::Tail {
    match prim {
        ersd::Prim::Nat(value) => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Nat(*value)));

            cont(work, value)
        }
        ersd::Prim::NatEql(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatEql)
        }
        ersd::Prim::NatAdd(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatAdd)
        }
        ersd::Prim::NatSub(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatSub)
        }
        ersd::Prim::NatMul(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatMul)
        }
        ersd::Prim::NatLt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatLt)
        }
        ersd::Prim::NatNeq(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatNeq)
        }
        ersd::Prim::NatDiv(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatDiv)
        }
        ersd::Prim::NatRem(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatRem)
        }
        ersd::Prim::NatGt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatGt)
        }
        ersd::Prim::NatLte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatLte)
        }
        ersd::Prim::NatGte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatGte)
        }
        ersd::Prim::Int(value) => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Int(*value)));

            cont(work, value)
        }
        ersd::Prim::Flt(value) => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Flt(*value)));

            cont(work, value)
        }
        ersd::Prim::IntEql(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntEql)
        }
        ersd::Prim::IntAdd(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntAdd)
        }
        ersd::Prim::IntSub(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntSub)
        }
        ersd::Prim::IntMul(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntMul)
        }
        ersd::Prim::IntNeq(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntNeq)
        }
        ersd::Prim::IntDiv(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntDiv)
        }
        ersd::Prim::IntRem(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntRem)
        }
        ersd::Prim::IntLt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntLt)
        }
        ersd::Prim::IntGt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntGt)
        }
        ersd::Prim::IntLte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntLte)
        }
        ersd::Prim::IntGte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntGte)
        }
        ersd::Prim::FltAdd(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltAdd)
        }
        ersd::Prim::FltSub(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltSub)
        }
        ersd::Prim::FltMul(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltMul)
        }
        ersd::Prim::FltDiv(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltDiv)
        }
        ersd::Prim::FltEql(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltEql)
        }
        ersd::Prim::FltNeq(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltNeq)
        }
        ersd::Prim::FltLt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltLt)
        }
        ersd::Prim::FltGt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltGt)
        }
        ersd::Prim::FltLte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltLte)
        }
        ersd::Prim::FltGte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltGte)
        }
        ersd::Prim::FltMin(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltMin)
        }
        ersd::Prim::FltMax(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltMax)
        }
        ersd::Prim::FltNeg(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltNeg)
        }
        ersd::Prim::FltAbs(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltAbs)
        }
        ersd::Prim::FltSqrt(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltSqrt)
        }
        ersd::Prim::FltFloor(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltFloor)
        }
        ersd::Prim::FltCeil(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltCeil)
        }
        ersd::Prim::FltTrunc(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltTrunc)
        }
        ersd::Prim::FltNearest(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltNearest)
        }
        ersd::Prim::NatToStr(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::NatToStr)
        }
        ersd::Prim::IntToStr(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::IntToStr)
        }
        ersd::Prim::FltToStr(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltToStr)
        }
        ersd::Prim::NatToInt(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::NatToInt)
        }
        ersd::Prim::IntToNat(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::IntToNat)
        }
        ersd::Prim::IntToFlt(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::IntToFlt)
        }
        ersd::Prim::NatToFlt(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::NatToFlt)
        }
        ersd::Prim::FltToInt(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltToInt)
        }
        ersd::Prim::FltToNat(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltToNat)
        }
        ersd::Prim::Bin(bytes) => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Bin(bytes.clone())));

            cont(work, value)
        }
        ersd::Prim::BinLen(bin) => lower_unary_code(work, bin, frame, cont, cont::Code::BinLen),
        ersd::Prim::BinEql(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::BinEql)
        }
        ersd::Prim::BinGet(bin, idx) => {
            lower_binary_code(work, bin, idx, frame, cont, cont::Code::BinGet)
        }
        ersd::Prim::BinSlice(bin, start, end) => {
            lower_ternary_code(work, bin, start, end, frame, cont, cont::Code::BinSlice)
        }
        ersd::Prim::BinAppend(bin, byte) => {
            lower_binary_code(work, bin, byte, frame, cont, cont::Code::BinAppend)
        }
        ersd::Prim::BinConcat(operands) => lower_bin_concat(work, operands, frame, vec![], cont),
        ersd::Prim::Arr(elements) => lower_lst(work, elements, frame, vec![], cont),
        ersd::Prim::ArrLen(lst) => lower_unary_code(work, lst, frame, cont, cont::Code::ArrLen),
        ersd::Prim::ArrGet(lst, idx) => {
            lower_binary_code(work, lst, idx, frame, cont, cont::Code::ArrGet)
        }
        ersd::Prim::ArrSlice(lst, start, end) => {
            lower_ternary_code(work, lst, start, end, frame, cont, cont::Code::ArrSlice)
        }
        ersd::Prim::ArrAppend(lst, elem) => {
            lower_binary_code(work, lst, elem, frame, cont, cont::Code::ArrAppend)
        }
        ersd::Prim::ArrConcat(operands) => lower_arr_concat(work, operands, frame, vec![], cont),
        ersd::Prim::Unit => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Tpl(vec![])));
            cont(work, value)
        }
        ersd::Prim::IoPrint(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::IoPrint)
        }
        ersd::Prim::IoRead => {
            let value = work.fresh(cont::Value::Eval(cont::Code::IoRead));
            cont(work, value)
        }
    }
}
