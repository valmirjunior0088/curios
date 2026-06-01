use {
    super::reduce,
    crate::core::{Context, Flt, Nat, Preempted, Prim, Subterm, Term},
};

/// Reduce both operands of a `Nat` binary primitive, then either `fold` the two literals or
/// `rebuild` the neutral term from the reduced operands.
fn reduce_nat_binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(u32, u32) -> Prim,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, Preempted> {
    let left = reduce(context, left.clone())?;
    let right = reduce(context, right.clone())?;

    Ok(Subterm::Prim(match (left.as_nat(), right.as_nat()) {
        (Some(l), Some(r)) => fold(l, r),
        _ => rebuild(left, right),
    }))
}

/// `Int` counterpart of [`reduce_nat_binary`].
fn reduce_int_binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(i32, i32) -> Prim,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, Preempted> {
    let left = reduce(context, left.clone())?;
    let right = reduce(context, right.clone())?;

    Ok(Subterm::Prim(match (left.as_int(), right.as_int()) {
        (Some(l), Some(r)) => fold(l, r),
        _ => rebuild(left, right),
    }))
}

/// `Flt` counterpart of [`reduce_nat_binary`].
fn reduce_flt_binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Flt, Flt) -> Prim,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, Preempted> {
    let left = reduce(context, left.clone())?;
    let right = reduce(context, right.clone())?;

    Ok(Subterm::Prim(match (left.as_flt(), right.as_flt()) {
        (Some(l), Some(r)) => fold(l, r),
        _ => rebuild(left, right),
    }))
}

/// Reduce the operand of a `Nat` unary primitive, then either `fold` the literal or `rebuild`
/// the neutral term from the reduced operand.
fn reduce_nat_unary(
    context: &mut Context,
    inner: &Term,
    fold: impl FnOnce(u32) -> Prim,
    rebuild: impl FnOnce(Term) -> Prim,
) -> Result<Subterm, Preempted> {
    let inner = reduce(context, inner.clone())?;

    Ok(Subterm::Prim(match inner.as_nat() {
        Some(value) => fold(value),
        None => rebuild(inner),
    }))
}

/// `Int` counterpart of [`reduce_nat_unary`].
fn reduce_int_unary(
    context: &mut Context,
    inner: &Term,
    fold: impl FnOnce(i32) -> Prim,
    rebuild: impl FnOnce(Term) -> Prim,
) -> Result<Subterm, Preempted> {
    let inner = reduce(context, inner.clone())?;

    Ok(Subterm::Prim(match inner.as_int() {
        Some(value) => fold(value),
        None => rebuild(inner),
    }))
}

/// `Flt` counterpart of [`reduce_nat_unary`].
fn reduce_flt_unary(
    context: &mut Context,
    inner: &Term,
    fold: impl FnOnce(Flt) -> Prim,
    rebuild: impl FnOnce(Term) -> Prim,
) -> Result<Subterm, Preempted> {
    let inner = reduce(context, inner.clone())?;

    Ok(Subterm::Prim(match inner.as_flt() {
        Some(value) => fold(value),
        None => rebuild(inner),
    }))
}

pub fn reduce_prim(context: &mut Context, prim: &Prim) -> Result<Subterm, Preempted> {
    match prim {
        Prim::BlnType => Ok(Subterm::Prim(Prim::BlnType)),
        Prim::Bln(value) => Ok(Subterm::Prim(Prim::Bln(*value))),
        Prim::NatType => Ok(Subterm::Prim(Prim::NatType)),
        Prim::Nat(Nat::Zero) => Ok(Subterm::Prim(Prim::Nat(Nat::Zero))),
        Prim::Nat(Nat::Succ(spine, inner)) => {
            let inner = reduce(context, inner.clone())?;
            Ok(match Term::unwrap_or_clone(inner) {
                Subterm::Prim(Prim::Nat(Nat::Succ(j, tail))) => {
                    Prim::Nat(Nat::Succ(spine + j, tail)).into()
                }
                inner => Prim::Nat(Nat::Succ(*spine, Term::new(inner))).into(),
            })
        }
        Prim::NatEql(left, right) => {
            reduce_nat_binary(context, left, right, |l, r| Prim::Bln(l == r), Prim::NatEql)
        }
        Prim::NatNeq(left, right) => {
            reduce_nat_binary(context, left, right, |l, r| Prim::Bln(l != r), Prim::NatNeq)
        }
        Prim::NatAdd(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| Prim::Nat(Nat::new(l.wrapping_add(r))),
            Prim::NatAdd,
        ),
        Prim::NatSub(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| Prim::Nat(Nat::new(l.wrapping_sub(r))),
            Prim::NatSub,
        ),
        Prim::NatMul(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| Prim::Nat(Nat::new(l.wrapping_mul(r))),
            Prim::NatMul,
        ),
        Prim::NatLt(left, right) => {
            reduce_nat_binary(context, left, right, |l, r| Prim::Bln(l < r), Prim::NatLt)
        }
        Prim::NatDiv(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| Prim::Nat(Nat::new(l.wrapping_div(r))),
            Prim::NatDiv,
        ),
        Prim::NatRem(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| Prim::Nat(Nat::new(l.wrapping_rem(r))),
            Prim::NatRem,
        ),
        Prim::NatGt(left, right) => {
            reduce_nat_binary(context, left, right, |l, r| Prim::Bln(l > r), Prim::NatGt)
        }
        Prim::NatLte(left, right) => {
            reduce_nat_binary(context, left, right, |l, r| Prim::Bln(l <= r), Prim::NatLte)
        }
        Prim::NatGte(left, right) => {
            reduce_nat_binary(context, left, right, |l, r| Prim::Bln(l >= r), Prim::NatGte)
        }
        Prim::IntType => Ok(Subterm::Prim(Prim::IntType)),
        Prim::Int(value) => Ok(Subterm::Prim(Prim::Int(*value))),
        Prim::IntEql(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left == right),
            Prim::IntEql,
        ),
        Prim::IntNeq(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left != right),
            Prim::IntNeq,
        ),
        Prim::IntAdd(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Prim::Int(left.wrapping_add(right)),
            Prim::IntAdd,
        ),
        Prim::IntSub(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Prim::Int(left.wrapping_sub(right)),
            Prim::IntSub,
        ),
        Prim::IntMul(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Prim::Int(left.wrapping_mul(right)),
            Prim::IntMul,
        ),
        Prim::IntDiv(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Prim::Int(left.wrapping_div(right)),
            Prim::IntDiv,
        ),
        Prim::IntRem(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Prim::Int(left.wrapping_rem(right)),
            Prim::IntRem,
        ),
        Prim::IntLt(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left < right),
            Prim::IntLt,
        ),
        Prim::IntGt(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left > right),
            Prim::IntGt,
        ),
        Prim::IntLte(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left <= right),
            Prim::IntLte,
        ),
        Prim::IntGte(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left >= right),
            Prim::IntGte,
        ),
        Prim::FltType => Ok(Subterm::Prim(Prim::FltType)),
        Prim::Flt(flt) => Ok(Subterm::Prim(Prim::Flt(*flt))),
        Prim::FltAdd(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Flt(left + right),
            Prim::FltAdd,
        ),
        Prim::FltSub(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Flt(left - right),
            Prim::FltSub,
        ),
        Prim::FltMul(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Flt(left * right),
            Prim::FltMul,
        ),
        Prim::FltDiv(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Flt(left / right),
            Prim::FltDiv,
        ),
        Prim::FltMin(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Flt(left.min(right)),
            Prim::FltMin,
        ),
        Prim::FltMax(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Flt(left.max(right)),
            Prim::FltMax,
        ),
        Prim::FltEql(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left.eql(right)),
            Prim::FltEql,
        ),
        Prim::FltNeq(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left.neq(right)),
            Prim::FltNeq,
        ),
        Prim::FltLt(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left.lt(right)),
            Prim::FltLt,
        ),
        Prim::FltGt(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left.gt(right)),
            Prim::FltGt,
        ),
        Prim::FltLte(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left.lte(right)),
            Prim::FltLte,
        ),
        Prim::FltGte(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left.gte(right)),
            Prim::FltGte,
        ),
        Prim::FltNeg(inner) => {
            reduce_flt_unary(context, inner, |flt| Prim::Flt(-flt), Prim::FltNeg)
        }
        Prim::FltAbs(inner) => {
            reduce_flt_unary(context, inner, |flt| Prim::Flt(flt.abs()), Prim::FltAbs)
        }
        Prim::FltSqrt(inner) => {
            reduce_flt_unary(context, inner, |flt| Prim::Flt(flt.sqrt()), Prim::FltSqrt)
        }
        Prim::FltFloor(inner) => {
            reduce_flt_unary(context, inner, |flt| Prim::Flt(flt.floor()), Prim::FltFloor)
        }
        Prim::FltCeil(inner) => {
            reduce_flt_unary(context, inner, |flt| Prim::Flt(flt.ceil()), Prim::FltCeil)
        }
        Prim::FltTrunc(inner) => {
            reduce_flt_unary(context, inner, |flt| Prim::Flt(flt.trunc()), Prim::FltTrunc)
        }
        Prim::FltNearest(inner) => reduce_flt_unary(
            context,
            inner,
            |flt| Prim::Flt(flt.nearest()),
            Prim::FltNearest,
        ),
        Prim::NatToStr(inner) => reduce_nat_unary(
            context,
            inner,
            |v| Prim::Bin(format!("{v}").into_bytes()),
            Prim::NatToStr,
        ),
        Prim::IntToStr(inner) => reduce_int_unary(
            context,
            inner,
            |v| Prim::Bin(format!("{v}").into_bytes()),
            Prim::IntToStr,
        ),
        Prim::FltToStr(inner) => reduce_flt_unary(
            context,
            inner,
            |v| Prim::Bin(format!("{}", v.to_f32()).into_bytes()),
            Prim::FltToStr,
        ),
        Prim::NatToInt(inner) => {
            reduce_nat_unary(context, inner, |v| Prim::Int(v as i32), Prim::NatToInt)
        }
        Prim::NatToFlt(inner) => reduce_nat_unary(
            context,
            inner,
            |v| Prim::Flt(Flt::from_f32(v as f32)),
            Prim::NatToFlt,
        ),
        Prim::IntToNat(inner) => reduce_int_unary(
            context,
            inner,
            |v| Prim::Nat(Nat::new(v as u32)),
            Prim::IntToNat,
        ),
        Prim::IntToFlt(inner) => reduce_int_unary(
            context,
            inner,
            |v| Prim::Flt(Flt::from_f32(v as f32)),
            Prim::IntToFlt,
        ),
        Prim::FltToNat(inner) => reduce_flt_unary(
            context,
            inner,
            |flt| Prim::Nat(Nat::new(flt.to_f32() as u32)),
            Prim::FltToNat,
        ),
        Prim::FltToInt(inner) => reduce_flt_unary(
            context,
            inner,
            |flt| Prim::Int(flt.to_f32() as i32),
            Prim::FltToInt,
        ),
        Prim::BinType => Ok(Subterm::Prim(Prim::BinType)),
        Prim::Bin(bytes) => Ok(Subterm::Prim(Prim::Bin(bytes.clone()))),
        Prim::BinLen(bin) => {
            let bin = reduce(context, bin.clone())?;
            Ok(match Term::unwrap_or_clone(bin) {
                Subterm::Prim(Prim::Bin(bytes)) => {
                    Subterm::Prim(Prim::Nat(Nat::new(bytes.len() as u32)))
                }
                bin => Subterm::Prim(Prim::bin_len(bin)),
            })
        }
        Prim::BinEql(left, right) => {
            let left = reduce(context, left.clone())?;
            let right = reduce(context, right.clone())?;

            Ok(
                match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                    (Subterm::Prim(Prim::Bin(left)), Subterm::Prim(Prim::Bin(right))) => {
                        Subterm::Prim(Prim::Bln(left == right))
                    }
                    (left, right) => Subterm::Prim(Prim::bin_eql(left, right)),
                },
            )
        }
        Prim::BinGet(bin, index) => {
            let bin = reduce(context, bin.clone())?;
            let index = reduce(context, index.clone())?;
            let i = index.as_nat();
            Ok(match (Term::unwrap_or_clone(bin), i) {
                (Subterm::Prim(Prim::Bin(bytes)), Some(i)) => Subterm::Prim(Prim::Nat(Nat::new(
                    bytes
                        .get(i as usize)
                        .copied()
                        .expect("Bin.get: index out of bounds") as u32,
                ))),
                (bin, _) => Subterm::Prim(Prim::bin_get(bin, index)),
            })
        }
        Prim::BinSlice(bin, start, end) => {
            let bin = reduce(context, bin.clone())?;
            let start = reduce(context, start.clone())?;
            let end = reduce(context, end.clone())?;
            let s = start.as_nat();
            let e = end.as_nat();
            Ok(match (Term::unwrap_or_clone(bin), s, e) {
                (Subterm::Prim(Prim::Bin(bytes)), Some(s), Some(e)) => Subterm::Prim(Prim::Bin(
                    bytes
                        .get(s as usize..e as usize)
                        .expect("Bin.slice: range out of bounds")
                        .to_vec(),
                )),
                (bin, _, _) => Subterm::Prim(Prim::bin_slice(bin, start, end)),
            })
        }
        Prim::BinAppend(bin, byte) => {
            let bin = reduce(context, bin.clone())?;
            let byte = reduce(context, byte.clone())?;
            let n = byte.as_nat();
            Ok(match (Term::unwrap_or_clone(bin), n) {
                (Subterm::Prim(Prim::Bin(mut bytes)), Some(n)) => {
                    bytes.push(n as u8);
                    Subterm::Prim(Prim::Bin(bytes))
                }
                (bin, _) => Subterm::Prim(Prim::bin_append(bin, byte)),
            })
        }
        Prim::BinConcat(operands) => {
            let reduced: Vec<Term> = operands
                .iter()
                .map(|e| reduce(context, e.clone()))
                .collect::<Result<_, _>>()?;
            let merged = reduced.iter().try_fold(Vec::new(), |mut acc, t| {
                if let Subterm::Prim(Prim::Bin(b)) = &**t {
                    acc.extend(b);
                    Some(acc)
                } else {
                    None
                }
            });
            Ok(match merged {
                Some(bytes) => Subterm::Prim(Prim::Bin(bytes)),
                None => Subterm::Prim(Prim::BinConcat(reduced)),
            })
        }
        Prim::ArrType(elem) => {
            let elem = reduce(context, elem.clone())?;
            Ok(Subterm::Prim(Prim::arr_type(elem)))
        }
        Prim::Arr(elems) => {
            let elems = elems
                .iter()
                .map(|e| reduce(context, e.clone()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Subterm::Prim(Prim::Arr(elems)))
        }
        Prim::ArrLen(type_, list) => {
            let type_ = reduce(context, type_.clone())?;
            let list = reduce(context, list.clone())?;
            Ok(match Term::unwrap_or_clone(list) {
                Subterm::Prim(Prim::Arr(elems)) => {
                    Subterm::Prim(Prim::Nat(Nat::new(elems.len() as u32)))
                }
                list => Subterm::Prim(Prim::arr_len(type_, list)),
            })
        }
        Prim::ArrGet(type_, list, index) => {
            let type_ = reduce(context, type_.clone())?;
            let list = reduce(context, list.clone())?;
            let index = reduce(context, index.clone())?;
            let i = index.as_nat();
            Ok(match (Term::unwrap_or_clone(list), i) {
                (Subterm::Prim(Prim::Arr(elems)), Some(i)) => elems
                    .into_iter()
                    .nth(i as usize)
                    .map(Term::unwrap_or_clone)
                    .expect("Arr.get: index out of bounds"),
                (list, _) => Subterm::Prim(Prim::arr_get(type_, list, index)),
            })
        }
        Prim::ArrSlice(type_, list, start, end) => {
            let type_ = reduce(context, type_.clone())?;
            let list = reduce(context, list.clone())?;
            let start = reduce(context, start.clone())?;
            let end = reduce(context, end.clone())?;
            let s = start.as_nat();
            let e = end.as_nat();
            Ok(match (Term::unwrap_or_clone(list), s, e) {
                (Subterm::Prim(Prim::Arr(elems)), Some(s), Some(e)) => Subterm::Prim(Prim::Arr(
                    elems
                        .get(s as usize..e as usize)
                        .expect("Arr.slice: range out of bounds")
                        .to_vec(),
                )),
                (list, _, _) => Subterm::Prim(Prim::arr_slice(type_, list, start, end)),
            })
        }
        Prim::ArrAppend(type_, list, elem) => {
            let type_ = reduce(context, type_.clone())?;
            let list = reduce(context, list.clone())?;
            let elem = reduce(context, elem.clone())?;
            Ok(match Term::unwrap_or_clone(list) {
                Subterm::Prim(Prim::Arr(mut elems)) => {
                    elems.push(elem);
                    Subterm::Prim(Prim::Arr(elems))
                }
                list => Subterm::Prim(Prim::arr_append(type_, list, elem)),
            })
        }
        Prim::ArrConcat(type_, operands) => {
            let type_ = reduce(context, type_.clone())?;
            let reduced: Vec<Term> = operands
                .iter()
                .map(|e| reduce(context, e.clone()))
                .collect::<Result<_, _>>()?;
            let merged = reduced.iter().try_fold(Vec::new(), |mut acc, t| {
                if let Subterm::Prim(Prim::Arr(elems)) = &**t {
                    acc.extend(elems.iter().cloned());
                    Some(acc)
                } else {
                    None
                }
            });
            Ok(match merged {
                Some(elems) => Subterm::Prim(Prim::Arr(elems)),
                None => Subterm::Prim(Prim::arr_concat(type_, reduced)),
            })
        }
        Prim::IoPrint(_) => panic!("IoPrint cannot appear at the type level"),
        Prim::IoRead => panic!("IoRead cannot appear at the type level"),
    }
}
