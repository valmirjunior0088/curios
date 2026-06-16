use {
    super::reduce,
    crate::core::{Context, Flt, Int, Nat, Prim, ReduceError, Subterm, Term},
    num_traits::{ToPrimitive, Zero},
};

/// The low 31 bits of a `Nat` literal — the i31 carrier the runtime represents
/// it with. Mirrors the masking in `NatToInt`'s fold; a symbolic operand has no
/// `BigUint` and yields `None`, so the bitwise op stays a neutral term.
fn nat_bits(n: &Nat) -> Option<u32> {
    Some(n.to_big_uint()?.to_u32().unwrap_or(0) & 0x7FFF_FFFF)
}

/// Reduce both operands of a `Bln` binary primitive, then either `fold` the two
/// literals or `rebuild` the neutral term. `Bln` has no numeric carrier at the
/// type level, so the fold reads the `true`/`false` constructors directly.
fn reduce_bln_binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(bool, bool) -> bool,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let left = reduce(context, left.clone())?;
    let right = reduce(context, right.clone())?;

    Ok(Subterm::Prim(match (left.as_bln(), right.as_bln()) {
        (Some(l), Some(r)) => Prim::Bln(fold(l, r)),
        _ => rebuild(left, right),
    }))
}

/// Reduce both operands of a `Nat` binary primitive, then either `fold` the two literals or
/// `rebuild` the neutral term from the reduced operands.
fn reduce_nat_binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Nat, Nat) -> Option<Prim>,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let left = reduce(context, left.clone())?;
    let right = reduce(context, right.clone())?;

    let folded = match (left.as_nat(), right.as_nat()) {
        (Some(l), Some(r)) => fold(l, r),
        _ => None,
    };

    Ok(Subterm::Prim(match folded {
        Some(prim) => prim,
        None => rebuild(left, right),
    }))
}

/// `Nat/div`/`Nat/rem`: like [`reduce_nat_binary`], but partial — a divisor
/// that reduces to literal zero is a reported error (the type-level mirror of
/// the runtime trap, following `BinGet`'s pattern), never a Rust panic. A
/// symbolic operand still rebuilds the neutral term.
fn reduce_nat_division(
    context: &mut Context,
    left: &Term,
    right: &Term,
    kind: &'static str,
    fold: impl FnOnce(Nat, Nat) -> Option<Nat>,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let span = right.span().or_else(|| left.span());
    let left = reduce(context, left.clone())?;
    let right = reduce(context, right.clone())?;

    if right
        .as_nat()
        .and_then(|divisor| divisor.to_big_uint())
        .is_some_and(|divisor| divisor.is_zero())
    {
        return Err(ReduceError::DivisionByZero { kind, span });
    }

    let folded = match (left.as_nat(), right.as_nat()) {
        (Some(l), Some(r)) => fold(l, r).map(Prim::Nat),
        _ => None,
    };

    Ok(Subterm::Prim(match folded {
        Some(prim) => prim,
        None => rebuild(left, right),
    }))
}

/// `Int` counterpart of [`reduce_nat_binary`].
fn reduce_int_binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Int, Int) -> Prim,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let left = reduce(context, left.clone())?;
    let right = reduce(context, right.clone())?;

    Ok(Subterm::Prim(match (left.as_int(), right.as_int()) {
        (Some(l), Some(r)) => fold(l, r),
        _ => rebuild(left, right),
    }))
}

/// `Int/div`/`Int/rem`: like [`reduce_int_binary`], but a divisor that
/// reduces to literal zero is a reported error — mathematically undefined,
/// following `BinGet`'s pattern. The fold itself is exact and total past
/// that: the type level pretends ℤ (see [`Int`]).
fn reduce_int_division(
    context: &mut Context,
    left: &Term,
    right: &Term,
    kind: &'static str,
    fold: impl FnOnce(Int, Int) -> Option<Int>,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let span = right.span().or_else(|| left.span());
    let left = reduce(context, left.clone())?;
    let right = reduce(context, right.clone())?;

    if right.as_int().is_some_and(|divisor| divisor.is_zero()) {
        return Err(ReduceError::DivisionByZero { kind, span });
    }

    let folded = match (left.as_int(), right.as_int()) {
        (Some(l), Some(r)) => fold(l, r).map(Prim::Int),
        _ => None,
    };

    Ok(Subterm::Prim(match folded {
        Some(prim) => prim,
        None => rebuild(left, right),
    }))
}

/// `Flt` counterpart of [`reduce_nat_binary`].
fn reduce_flt_binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Flt, Flt) -> Prim,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, ReduceError> {
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
    fold: impl FnOnce(Nat) -> Option<Prim>,
    rebuild: impl FnOnce(Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let inner = reduce(context, inner.clone())?;

    Ok(Subterm::Prim(match inner.as_nat().and_then(fold) {
        Some(prim) => prim,
        None => rebuild(inner),
    }))
}

/// `Int` counterpart of [`reduce_nat_unary`]. The fold's `None` rebuilds the
/// neutral term: with `Int` unbounded at the type level, a conversion of a
/// value the target cannot represent simply stays stuck.
fn reduce_int_unary(
    context: &mut Context,
    inner: &Term,
    fold: impl FnOnce(Int) -> Option<Prim>,
    rebuild: impl FnOnce(Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let inner = reduce(context, inner.clone())?;

    Ok(Subterm::Prim(match inner.as_int().and_then(fold) {
        Some(prim) => prim,
        None => rebuild(inner),
    }))
}

/// `Flt` counterpart of [`reduce_nat_unary`].
fn reduce_flt_unary(
    context: &mut Context,
    inner: &Term,
    fold: impl FnOnce(Flt) -> Prim,
    rebuild: impl FnOnce(Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let inner = reduce(context, inner.clone())?;

    Ok(Subterm::Prim(match inner.as_flt() {
        Some(value) => fold(value),
        None => rebuild(inner),
    }))
}

pub fn reduce_prim(context: &mut Context, prim: &Prim) -> Result<Subterm, ReduceError> {
    match prim {
        Prim::BlnType => Ok(Subterm::Prim(Prim::BlnType)),
        Prim::Bln(value) => Ok(Subterm::Prim(Prim::Bln(*value))),
        Prim::BlnAnd(left, right) => {
            reduce_bln_binary(context, left, right, |l, r| l && r, Prim::BlnAnd)
        }
        Prim::BlnOr(left, right) => {
            reduce_bln_binary(context, left, right, |l, r| l || r, Prim::BlnOr)
        }
        Prim::BlnXor(left, right) => {
            reduce_bln_binary(context, left, right, |l, r| l != r, Prim::BlnXor)
        }
        Prim::NatType => Ok(Subterm::Prim(Prim::NatType)),
        Prim::Nat(Nat::Zero) => Ok(Subterm::Prim(Prim::Nat(Nat::Zero))),
        Prim::Nat(Nat::Succ(spine, inner)) => {
            let inner = reduce(context, inner.clone())?;

            Ok(match Term::unwrap_or_clone(inner) {
                Subterm::Prim(Prim::Nat(Nat::Succ(j, tail))) => {
                    Subterm::Prim(Prim::Nat(Nat::Succ(spine.clone() + j, tail)))
                }
                inner => Subterm::Prim(Prim::Nat(Nat::Succ(spine.clone(), Term::from(inner)))),
            })
        }
        Prim::NatEql(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.eql(&r).map(Prim::Bln),
            Prim::NatEql,
        ),
        Prim::NatNeq(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.eql(&r).map(|b| Prim::Bln(!b)),
            Prim::NatNeq,
        ),
        // Addition gets more than literal folding: the unit laws and
        // successor peeling are sound ℕ identities, and making them
        // *definitional* is what lets symbolic index arithmetic converge —
        // `Nat/add(j + 1, m)` reduces to `(Nat/add(j, m)) + 1`, so an indexed
        // constructor's target meets the motive's expected index without any
        // unification. Each step moves a literal spine outward, so the
        // rewrite terminates.
        Prim::NatAdd(left, right) => {
            let left = reduce(context, left.clone())?;
            let right = reduce(context, right.clone())?;

            if let (Some(l), Some(r)) = (left.as_nat(), right.as_nat())
                && let Some(sum) = l.checked_add(r)
            {
                return Ok(Subterm::Prim(Prim::Nat(sum)));
            }

            if matches!(&*left, Subterm::Prim(Prim::Nat(Nat::Zero))) {
                return Ok(Term::unwrap_or_clone(right));
            }
            if matches!(&*right, Subterm::Prim(Prim::Nat(Nat::Zero))) {
                return Ok(Term::unwrap_or_clone(left));
            }

            if let Subterm::Prim(Prim::Nat(Nat::Succ(spine, inner))) = &*left {
                return reduce_prim(
                    context,
                    &Prim::Nat(Nat::Succ(
                        spine.clone(),
                        Term::prim(Prim::nat_add(inner.clone(), right)),
                    )),
                );
            }
            if let Subterm::Prim(Prim::Nat(Nat::Succ(spine, inner))) = &*right {
                return reduce_prim(
                    context,
                    &Prim::Nat(Nat::Succ(
                        spine.clone(),
                        Term::prim(Prim::nat_add(left, inner.clone())),
                    )),
                );
            }

            Ok(Subterm::Prim(Prim::nat_add(left, right)))
        }
        Prim::NatSub(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.checked_sub(r).map(Prim::Nat),
            Prim::NatSub,
        ),
        Prim::NatMul(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.checked_mul(r).map(Prim::Nat),
            Prim::NatMul,
        ),
        Prim::NatLt(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.lt(&r).map(Prim::Bln),
            Prim::NatLt,
        ),
        Prim::NatDiv(left, right) => reduce_nat_division(
            context,
            left,
            right,
            "Nat/div",
            Nat::checked_div,
            Prim::NatDiv,
        ),
        Prim::NatRem(left, right) => reduce_nat_division(
            context,
            left,
            right,
            "Nat/rem",
            Nat::checked_rem,
            Prim::NatRem,
        ),
        Prim::NatGt(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.gt(&r).map(Prim::Bln),
            Prim::NatGt,
        ),
        Prim::NatLte(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.lte(&r).map(Prim::Bln),
            Prim::NatLte,
        ),
        Prim::NatGte(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.gte(&r).map(Prim::Bln),
            Prim::NatGte,
        ),
        // Bitwise ops fold on the 31-bit carrier, matching the runtime: `and`,
        // `or`, and `xor` all stay inside 31 bits; `shl` truncates the result
        // back into the carrier like the backend's `ref.i31`, while `shr` is
        // logical and never overflows.
        Prim::NatAnd(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| Some(Prim::Nat(Nat::new(nat_bits(&l)? & nat_bits(&r)?))),
            Prim::NatAnd,
        ),
        Prim::NatOr(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| Some(Prim::Nat(Nat::new(nat_bits(&l)? | nat_bits(&r)?))),
            Prim::NatOr,
        ),
        Prim::NatXor(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| Some(Prim::Nat(Nat::new(nat_bits(&l)? ^ nat_bits(&r)?))),
            Prim::NatXor,
        ),
        Prim::NatShl(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| {
                Some(Prim::Nat(Nat::new(
                    nat_bits(&l)?.wrapping_shl(nat_bits(&r)?) & 0x7FFF_FFFF,
                )))
            },
            Prim::NatShl,
        ),
        Prim::NatShr(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| {
                Some(Prim::Nat(Nat::new(
                    nat_bits(&l)?.wrapping_shr(nat_bits(&r)?),
                )))
            },
            Prim::NatShr,
        ),
        Prim::IntType => Ok(Subterm::Prim(Prim::IntType)),
        Prim::Int(value) => Ok(Subterm::Prim(Prim::Int(value.clone()))),
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
            |left, right| Prim::Int(left + right),
            Prim::IntAdd,
        ),
        Prim::IntSub(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Prim::Int(left - right),
            Prim::IntSub,
        ),
        Prim::IntMul(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Prim::Int(left * right),
            Prim::IntMul,
        ),
        Prim::IntDiv(left, right) => reduce_int_division(
            context,
            left,
            right,
            "Int/div",
            Int::checked_div,
            Prim::IntDiv,
        ),
        Prim::IntRem(left, right) => reduce_int_division(
            context,
            left,
            right,
            "Int/rem",
            Int::checked_rem,
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
            |v| {
                v.to_big_uint()
                    .map(|b| Prim::Str(format!("{b}").into_bytes()))
            },
            Prim::NatToStr,
        ),
        Prim::IntToStr(inner) => reduce_int_unary(
            context,
            inner,
            |v| Some(Prim::Str(format!("{v}").into_bytes())),
            Prim::IntToStr,
        ),
        Prim::FltToStr(inner) => reduce_flt_unary(
            context,
            inner,
            |v| Prim::Str(format!("{v}").into_bytes()),
            Prim::FltToStr,
        ),
        Prim::FltToLeBin(inner) => reduce_flt_unary(
            context,
            inner,
            |v| Prim::Bin(v.to_f32().to_le_bytes().to_vec()),
            Prim::FltToLeBin,
        ),
        Prim::NatToInt(inner) => reduce_nat_unary(
            context,
            inner,
            |v| {
                let bits = v.to_big_uint()?.to_u32().unwrap_or(0) & 0x7FFF_FFFF;
                let signed = if bits >= 0x4000_0000 {
                    bits as i64 - (1i64 << 31)
                } else {
                    bits as i64
                };
                Some(Prim::Int(Int::new(signed)))
            },
            Prim::NatToInt,
        ),
        Prim::NatToFlt(inner) => reduce_nat_unary(
            context,
            inner,
            |v| {
                Some(Prim::Flt(Flt::from_f32(
                    v.to_big_uint()?.to_f64().unwrap_or(0.0) as f32,
                )))
            },
            Prim::NatToFlt,
        ),
        Prim::IntToNat(inner) => reduce_int_unary(
            context,
            inner,
            |v| Some(Prim::Nat(Nat::new(v.to_i32()? as u32))),
            Prim::IntToNat,
        ),
        Prim::IntToFlt(inner) => reduce_int_unary(
            context,
            inner,
            |v| Some(Prim::Flt(Flt::from_f32(v.to_i32()? as f32))),
            Prim::IntToFlt,
        ),
        Prim::FltToNat(inner) => reduce_flt_unary(
            context,
            inner,
            |flt| Prim::Nat(Nat::new(flt.to_f32() as u32)),
            Prim::FltToNat,
        ),
        // Exact — the type level pretends ℤ, so no finite float is out of
        // range. A float with no integer part at all (NaN, ±inf) folds to
        // nothing and the term stays stuck.
        Prim::FltToInt(inner) => {
            let inner = reduce(context, inner.clone())?;
            Ok(Subterm::Prim(
                match inner.as_flt().and_then(|v| Int::from_f32_trunc(v.to_f32())) {
                    Some(int) => Prim::Int(int),
                    None => Prim::FltToInt(inner),
                },
            ))
        }
        Prim::BinType => Ok(Subterm::Prim(Prim::BinType)),
        Prim::Bin(bytes) => Ok(Subterm::Prim(Prim::Bin(bytes.clone()))),
        Prim::BinLen(bin) => {
            let bin = reduce(context, bin.clone())?;
            Ok(match Term::unwrap_or_clone(bin) {
                Subterm::Prim(Prim::Bin(bytes)) => Subterm::Prim(Prim::Nat(Nat::new(bytes.len()))),
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
            let index_reduced = reduce(context, index.clone())?;
            let i = index_reduced
                .as_nat()
                .and_then(|n| n.to_big_uint()?.to_usize());
            Ok(match (Term::unwrap_or_clone(bin), i) {
                (Subterm::Prim(Prim::Bin(bytes)), Some(i)) => match bytes.get(i).copied() {
                    Some(byte) => Subterm::Prim(Prim::Nat(Nat::new(byte))),
                    None => {
                        return Err(ReduceError::BinGetOutOfBounds {
                            len: bytes.len(),
                            index: i,
                            span: index.span(),
                        });
                    }
                },
                (bin, _) => Subterm::Prim(Prim::bin_get(bin, index_reduced)),
            })
        }
        Prim::BinSlice(bin, start, end) => {
            let bin = reduce(context, bin.clone())?;
            let start_reduced = reduce(context, start.clone())?;
            let end_reduced = reduce(context, end.clone())?;
            let s = start_reduced
                .as_nat()
                .and_then(|n| n.to_big_uint()?.to_usize());
            let e = end_reduced
                .as_nat()
                .and_then(|n| n.to_big_uint()?.to_usize());
            Ok(match (Term::unwrap_or_clone(bin), s, e) {
                (Subterm::Prim(Prim::Bin(bytes)), Some(s), Some(e)) => match bytes.get(s..e) {
                    Some(slice) => Subterm::Prim(Prim::Bin(slice.to_vec())),
                    None => {
                        return Err(ReduceError::BinSliceOutOfRange {
                            len: bytes.len(),
                            start: s,
                            end: e,
                            span: start.span().or_else(|| end.span()),
                        });
                    }
                },
                (bin, _, _) => Subterm::Prim(Prim::bin_slice(bin, start_reduced, end_reduced)),
            })
        }
        Prim::BinAppend(bin, byte) => {
            let bin = reduce(context, bin.clone())?;
            let byte = reduce(context, byte.clone())?;
            let n = byte.as_nat().and_then(|n| n.to_big_uint()?.to_u8());
            Ok(match (Term::unwrap_or_clone(bin), n) {
                (Subterm::Prim(Prim::Bin(mut bytes)), Some(n)) => {
                    bytes.push(n);
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
        Prim::StrType => Ok(Subterm::Prim(Prim::StrType)),
        Prim::Str(bytes) => Ok(Subterm::Prim(Prim::Str(bytes.clone()))),
        Prim::StrToBin(str) => {
            let str = reduce(context, str.clone())?;
            Ok(match Term::unwrap_or_clone(str) {
                // A literal's carrier bytes compute; `Str/to_bin ∘ of_bin` cancels.
                Subterm::Prim(Prim::Str(bytes)) => Subterm::Prim(Prim::Bin(bytes)),
                Subterm::Prim(Prim::StrOfBin(bin)) => Term::unwrap_or_clone(bin),
                str => Subterm::Prim(Prim::str_to_bin(str)),
            })
        }
        Prim::StrOfBin(bin) => {
            let bin = reduce(context, bin.clone())?;
            Ok(match Term::unwrap_or_clone(bin) {
                // A reduced byte literal becomes a `Str` literal; `of_bin ∘ to_bin` cancels.
                Subterm::Prim(Prim::Bin(bytes)) => Subterm::Prim(Prim::Str(bytes)),
                Subterm::Prim(Prim::StrToBin(str)) => Term::unwrap_or_clone(str),
                bin => Subterm::Prim(Prim::str_of_bin(bin)),
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
                Subterm::Prim(Prim::Arr(elems)) => Subterm::Prim(Prim::Nat(Nat::new(elems.len()))),
                list => Subterm::Prim(Prim::arr_len(type_, list)),
            })
        }
        Prim::ArrGet(type_, list, index) => {
            let type_ = reduce(context, type_.clone())?;
            let list = reduce(context, list.clone())?;
            let index_reduced = reduce(context, index.clone())?;
            let i = index_reduced
                .as_nat()
                .and_then(|n| n.to_big_uint()?.to_usize());
            Ok(match (Term::unwrap_or_clone(list), i) {
                (Subterm::Prim(Prim::Arr(elems)), Some(i)) => {
                    let len = elems.len();
                    match elems.into_iter().nth(i).map(Term::unwrap_or_clone) {
                        Some(elem) => elem,
                        None => {
                            return Err(ReduceError::ArrGetOutOfBounds {
                                len,
                                index: i,
                                span: index.span(),
                            });
                        }
                    }
                }
                (list, _) => Subterm::Prim(Prim::arr_get(type_, list, index_reduced)),
            })
        }
        Prim::ArrSlice(type_, list, start, end) => {
            let type_ = reduce(context, type_.clone())?;
            let list = reduce(context, list.clone())?;
            let start_reduced = reduce(context, start.clone())?;
            let end_reduced = reduce(context, end.clone())?;
            let s = start_reduced
                .as_nat()
                .and_then(|n| n.to_big_uint()?.to_usize());
            let e = end_reduced
                .as_nat()
                .and_then(|n| n.to_big_uint()?.to_usize());
            Ok(match (Term::unwrap_or_clone(list), s, e) {
                (Subterm::Prim(Prim::Arr(elems)), Some(s), Some(e)) => match elems.get(s..e) {
                    Some(slice) => Subterm::Prim(Prim::Arr(slice.to_vec())),
                    None => {
                        return Err(ReduceError::ArrSliceOutOfRange {
                            len: elems.len(),
                            start: s,
                            end: e,
                            span: start.span().or_else(|| end.span()),
                        });
                    }
                },
                (list, _, _) => {
                    Subterm::Prim(Prim::arr_slice(type_, list, start_reduced, end_reduced))
                }
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
        // The handle type and handle tokens are inert values, like `Nat`/`Nat(_)`.
        Prim::IoType => Ok(Subterm::Prim(Prim::IoType)),
        Prim::Io(token) => Ok(Subterm::Prim(Prim::Io(*token))),
        Prim::IoRead(handle, _) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoRead",
            span: handle.span(),
        }),
        Prim::IoWrite(handle, _) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoWrite",
            span: handle.span(),
        }),
        Prim::IoOpen(path, _) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoOpen",
            span: path.span(),
        }),
        Prim::IoConnect(host, ..) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoConnect",
            span: host.span(),
        }),
        Prim::IoClose(handle) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoClose",
            span: handle.span(),
        }),
        Prim::IoClockWall => Err(ReduceError::IoAtTypeLevel {
            kind: "IoClockWall",
            span: None,
        }),
        Prim::IoClockMono => Err(ReduceError::IoAtTypeLevel {
            kind: "IoClockMono",
            span: None,
        }),
        Prim::IoRandom(count) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoRandom",
            span: count.span(),
        }),
        // argv is an immutable snapshot: inert, like the handle tokens above. A
        // top-level `args : Arr(Bin)` value force-reduces to this stuck node
        // rather than tripping the IO guard; it becomes a host call only at
        // erasure.
        Prim::IoArgs => Ok(Subterm::Prim(Prim::IoArgs)),
        Prim::IoEnv(name) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoEnv",
            span: name.span(),
        }),
        Prim::IoExit(_, code) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoExit",
            span: code.span(),
        }),
    }
}
