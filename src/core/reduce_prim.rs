use {
    super::reduce,
    crate::core::{
        Context, Flt, Int, Nat, Prim, ReduceError, Subterm, Term, normalize_concat, peel_first_byte,
    },
    num_traits::{ToPrimitive, Zero},
};

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

/// `Int` counterpart of [`reduce_nat_binary`]: fold both literal operands or
/// rebuild the neutral term. The fold is partial for the same reason — the
/// shifts decline a negative or oversized literal shift count (`None`); the
/// total ops just wrap their result in `Some`.
fn reduce_int_binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Int, Int) -> Option<Prim>,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let left = reduce(context, left.clone())?;
    let right = reduce(context, right.clone())?;

    let folded = match (left.as_int(), right.as_int()) {
        (Some(l), Some(r)) => fold(l, r),
        _ => None,
    };

    Ok(Subterm::Prim(
        folded.unwrap_or_else(|| rebuild(left, right)),
    ))
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
        Prim::BlnEql(left, right) => {
            reduce_bln_binary(context, left, right, |l, r| l == r, Prim::BlnEql)
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
        // Handles are opaque runtime tokens with no compile-time literal form,
        // so this only ever reduces its operands and rebuilds -- it never folds.
        Prim::IoEql(left, right) => reduce_nat_binary(context, left, right, |_, _| None, Prim::IoEql),
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
        Prim::NatSub(left, right) => {
            let left = reduce(context, left.clone())?;
            let right = reduce(context, right.clone())?;
            // Both literal: fold with truncating ℕ subtraction.
            if let (Some(l), Some(r)) = (left.as_nat(), right.as_nat())
                && let Some(diff) = l.checked_sub(r)
            {
                return Ok(Subterm::Prim(Prim::Nat(diff)));
            }
            // `(s + inner) - k = (s - k) + inner` when the literal `k` is at or
            // below the successor floor `s` (truncated ℕ, `inner ≥ 0`, so no
            // borrow reaches `inner`). The subtraction twin of `NatAdd`'s
            // successor peeling: it turns the `succ e - 1` bounds the cons slice
            // rule produces back into `e`, so a slice over a symbolic cons keeps
            // reducing instead of stalling on a stuck `Nat/sub`.
            if let Some(k) = right.as_nat().and_then(|n| n.to_big_uint())
                && let Subterm::Prim(Prim::Nat(Nat::Succ(floor, inner))) = &*left
                && *floor >= k
            {
                let diff = floor - &k;
                if diff.is_zero() {
                    return reduce(context, inner.clone()).map(Term::unwrap_or_clone);
                }
                return Ok(Subterm::Prim(Prim::Nat(Nat::Succ(diff, inner.clone()))));
            }
            Ok(Subterm::Prim(Prim::nat_sub(left, right)))
        }
        Prim::NatMul(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.checked_mul(r).map(Prim::Nat),
            Prim::NatMul,
        ),
        Prim::NatLt(left, right) => {
            let left = reduce(context, left.clone())?;
            let right = reduce(context, right.clone())?;
            // Both literal: fold exactly.
            if let (Some(l), Some(r)) = (left.as_nat(), right.as_nat())
                && let Some(folded) = l.lt(&r)
            {
                return Ok(Subterm::Prim(Prim::Bln(folded)));
            }
            // `lt(a, s + inner) = true` when the literal `a` is below the
            // successor floor `s` (`inner ≥ 0`, so the right side is at least
            // `s > a`). The partner of `Bin/len`'s cons rule: it discharges the
            // `lt(0, succ(len t))` guard a codepoint walk raises on a cons.
            if let Some(a) = left.as_nat().and_then(|n| n.to_big_uint())
                && let Subterm::Prim(Prim::Nat(Nat::Succ(floor, _))) = &*right
                && a < *floor
            {
                return Ok(Subterm::Prim(Prim::Bln(true)));
            }
            // `lt(s + inner, b) = false` when the literal `b` is at or below the
            // successor floor `s` (the left side is at least `s ≥ b`).
            if let Some(b) = right.as_nat().and_then(|n| n.to_big_uint())
                && let Subterm::Prim(Prim::Nat(Nat::Succ(floor, _))) = &*left
                && *floor >= b
            {
                return Ok(Subterm::Prim(Prim::Bln(false)));
            }
            // Cancel a common successor structure off both sides — the
            // operation-level partner of the `Unary` eliminator's successor peel,
            // discharging symbolic bounds the literal-floor rules above leave
            // stuck. Decompose each side into `(count, inner)` (`Succ(s, x)` is
            // `(s, x)`, a bare term `t` is `(0, t)`). Two values over the *same*
            // inner compare by their counts: `lt(x + sl, x + sr) = lt(sl, sr)`
            // (e.g. `lt(pred, succ pred)`). Otherwise peel the common count floor:
            // `lt(succ^m a, succ^m b) = lt(a, b)` — both preserve order.
            {
                let zero = num_bigint::BigUint::from(0usize);
                let decompose = |t: &Term| -> (num_bigint::BigUint, Term) {
                    match &**t {
                        Subterm::Prim(Prim::Nat(Nat::Succ(s, inner))) => (s.clone(), inner.clone()),
                        _ => (zero.clone(), t.clone()),
                    }
                };
                let rebuild = |s: num_bigint::BigUint, inner: Term| -> Term {
                    match s == zero {
                        true => inner,
                        false => Term::prim(Prim::Nat(Nat::Succ(s, inner))),
                    }
                };
                let (sl, il) = decompose(&left);
                let (sr, ir) = decompose(&right);
                if il == ir {
                    return Ok(Subterm::Prim(Prim::Bln(sl < sr)));
                }
                let m = sl.clone().min(sr.clone());
                if m > zero {
                    let peeled = Term::prim(Prim::nat_lt(rebuild(sl - &m, il), rebuild(sr - &m, ir)));
                    return reduce(context, peeled).map(Term::unwrap_or_clone);
                }
            }
            Ok(Subterm::Prim(Prim::nat_lt(left, right)))
        }
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
        // Bitwise ops fold on the unbounded ℕ the type level pretends: `and`,
        // `or`, `xor` on the infinite binary expansion, `shl` as `· 2^n` and
        // `shr` as `⌊·/2^n⌋`. The runtime's 31-bit carrier (truncating `shl`,
        // logical `shr`) is imposed only in the backend, never here.
        Prim::NatAnd(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.checked_bitand(r).map(Prim::Nat),
            Prim::NatAnd,
        ),
        Prim::NatOr(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.checked_bitor(r).map(Prim::Nat),
            Prim::NatOr,
        ),
        Prim::NatXor(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.checked_bitxor(r).map(Prim::Nat),
            Prim::NatXor,
        ),
        Prim::NatShl(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.checked_shl(r).map(Prim::Nat),
            Prim::NatShl,
        ),
        Prim::NatShr(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.checked_shr(r).map(Prim::Nat),
            Prim::NatShr,
        ),
        Prim::IntType => Ok(Subterm::Prim(Prim::IntType)),
        Prim::Int(value) => Ok(Subterm::Prim(Prim::Int(value.clone()))),
        Prim::IntEql(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Bln(left == right)),
            Prim::IntEql,
        ),
        Prim::IntNeq(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Bln(left != right)),
            Prim::IntNeq,
        ),
        Prim::IntAdd(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Int(left + right)),
            Prim::IntAdd,
        ),
        Prim::IntSub(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Int(left - right)),
            Prim::IntSub,
        ),
        Prim::IntMul(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Int(left * right)),
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
            |left, right| Some(Prim::Bln(left < right)),
            Prim::IntLt,
        ),
        Prim::IntGt(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Bln(left > right)),
            Prim::IntGt,
        ),
        Prim::IntLte(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Bln(left <= right)),
            Prim::IntLte,
        ),
        Prim::IntGte(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Bln(left >= right)),
            Prim::IntGte,
        ),
        // Bitwise ops fold on the unbounded ℤ the type level pretends: `and`,
        // `or`, `xor` on the infinite two's-complement expansion, `shl` as
        // `· 2^n` and `shr` as the arithmetic `⌊·/2^n⌋`. The runtime's signed
        // 31-bit carrier (truncating `shl`, `shr_s`) is imposed only in the
        // backend, never here.
        Prim::IntAnd(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Int(left & right)),
            Prim::IntAnd,
        ),
        Prim::IntOr(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Int(left | right)),
            Prim::IntOr,
        ),
        Prim::IntXor(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Int(left ^ right)),
            Prim::IntXor,
        ),
        Prim::IntShl(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| left.checked_shl(right).map(Prim::Int),
            Prim::IntShl,
        ),
        Prim::IntShr(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| left.checked_shr(right).map(Prim::Int),
            Prim::IntShr,
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
            match &*bin {
                // A literal run: its byte count.
                Subterm::Prim(Prim::Bin(bytes)) => {
                    Ok(Subterm::Prim(Prim::Nat(Nat::new(bytes.len()))))
                }
                // `len` distributes over concatenation: `len(concat(a, b, ..)) =
                // len(a) + len(b) + ..` — the monoid partner of the `BinConcat`
                // rules, letting a symbolic cons reduce its length to a `succ`
                // spine (`NatAdd`'s successor peeling carries the `1` outward).
                Subterm::Prim(Prim::BinConcat(operands)) => {
                    let sum = operands.iter().rev().fold(
                        Term::prim(Prim::Nat(Nat::Zero)),
                        |acc, operand| {
                            let len = Term::prim(Prim::bin_len(operand.clone()));
                            Term::prim(Prim::nat_add(len, acc))
                        },
                    );
                    reduce(context, sum).map(Term::unwrap_or_clone)
                }
                // `len(append(base, _)) = succ(len base)` — one byte longer, the
                // base case the cons head (`append(\\, h)`) bottoms out on.
                Subterm::Prim(Prim::BinAppend(base, _)) => {
                    let one = Term::prim(Prim::Nat(Nat::new(1usize)));
                    let len = Term::prim(Prim::bin_len(base.clone()));
                    reduce(context, Term::prim(Prim::nat_add(one, len))).map(Term::unwrap_or_clone)
                }
                _ => Ok(Subterm::Prim(Prim::bin_len(Term::unwrap_or_clone(bin)))),
            }
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
            // A concrete index into a literal run.
            if let (Subterm::Prim(Prim::Bin(bytes)), Some(i)) = (&*bin, i) {
                return match bytes.get(i).copied() {
                    Some(byte) => Ok(Subterm::Prim(Prim::Nat(Nat::new(byte)))),
                    None => Err(ReduceError::BinGetOutOfBounds {
                        len: bytes.len(),
                        index: i,
                        span: index.span(),
                    }),
                };
            }
            // The cons head's byte: `get(append(\\, byte), 0) = byte` — the base
            // case of the cons-peel below, and the partner of `BinSlice`'s rules.
            if let Subterm::Prim(Prim::BinAppend(base, byte)) = &*bin {
                if matches!(&**base, Subterm::Prim(Prim::Bin(b)) if b.is_empty())
                    && matches!(&*index_reduced, Subterm::Prim(Prim::Nat(Nat::Zero)))
                {
                    return reduce(context, byte.clone()).map(Term::unwrap_or_clone);
                }
            }
            // A get over a cons spine peels one byte per `0`/`succ` index step:
            //   `get(cons(h, t), 0) = h`   and   `get(cons(h, t), succ k) = get(t, k)`.
            if let Some((head, tail)) = peel_first_byte(&bin) {
                match &*index_reduced {
                    Subterm::Prim(Prim::Nat(Nat::Zero)) => {
                        let zero = Term::prim(Prim::Nat(Nat::Zero));
                        return reduce(context, Term::prim(Prim::bin_get(head, zero)))
                            .map(Term::unwrap_or_clone);
                    }
                    Subterm::Prim(Prim::Nat(Nat::Succ(..))) => {
                        let one = Term::prim(Prim::Nat(Nat::new(1usize)));
                        let prev = Term::prim(Prim::nat_sub(index_reduced.clone(), one));
                        return reduce(context, Term::prim(Prim::bin_get(tail, prev)))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Prim(Prim::bin_get(bin, index_reduced)))
        }
        Prim::BinSlice(bin, start, end) => {
            let bin = reduce(context, bin.clone())?;
            let start_reduced = reduce(context, start.clone())?;
            let end_reduced = reduce(context, end.clone())?;
            // The full slice is the identity: `slice(b, 0, len b) = b`. Sound even
            // for a symbolic `b` — `0..len` is always in range, never trapping —
            // and the runtime partner of `core::spine`'s window-collapse: it lets a
            // bare full-window `BinSlice` reduce to its base, so a `Bin/slice` over
            // the whole value costs no copy and converts against the base directly.
            if matches!(&*start_reduced, Subterm::Prim(Prim::Nat(Nat::Zero)))
                && matches!(&*end_reduced, Subterm::Prim(Prim::BinLen(whole)) if *whole == bin)
            {
                return Ok(Term::unwrap_or_clone(bin));
            }
            // The empty slice is empty: `slice(b, i, i) = \\`. The dual of the
            // full-window identity and equally sound — an empty range yields no
            // bytes regardless of `b`, and never equates two distinct literals.
            // It lets a codepoint take collapse its zero-width base (`take 0`) to
            // the empty string even over a symbolic cons.
            if start_reduced == end_reduced {
                return Ok(Subterm::Prim(Prim::Bin(Vec::new())));
            }
            // A nested slice reassociates: `slice(slice(b, p, q), i, j) =
            // slice(b, p + i, p + j)`. Sound for the in-range bounds real call
            // sites produce; reassociating the window lets a codepoint walk
            // collapse a `slice(drop1(b), ..)` back onto `b`.
            if let Subterm::Prim(Prim::BinSlice(inner, p, _q)) = &*bin {
                let lo = Term::prim(Prim::nat_add(p.clone(), start_reduced.clone()));
                let hi = Term::prim(Prim::nat_add(p.clone(), end_reduced.clone()));
                let flattened = Term::prim(Prim::bin_slice(inner.clone(), lo, hi));
                return reduce(context, flattened).map(Term::unwrap_or_clone);
            }
            let s = start_reduced
                .as_nat()
                .and_then(|n| n.to_big_uint()?.to_usize());
            let e = end_reduced
                .as_nat()
                .and_then(|n| n.to_big_uint()?.to_usize());
            // A concrete slice of a literal run.
            if let (Subterm::Prim(Prim::Bin(bytes)), Some(s), Some(e)) = (&*bin, s, e) {
                return match bytes.get(s..e) {
                    Some(slice) => Ok(Subterm::Prim(Prim::Bin(slice.to_vec()))),
                    None => Err(ReduceError::BinSliceOutOfRange {
                        len: bytes.len(),
                        start: s,
                        end: e,
                        span: start.span().or_else(|| end.span()),
                    }),
                };
            }
            // A slice over a cons spine peels one byte per `0`/`succ` boundary
            // step — the reduction partner of the `Utf8` cons the validity proofs
            // walk:  `slice(cons(h, t), 0, succ e) = h ++ slice(t, 0, e)`  and
            // `slice(cons(h, t), succ s, e) = slice(t, s, e - 1)`.
            if let Some((head, tail)) = peel_first_byte(&bin) {
                let dec = |n: &Term| {
                    let one = Term::prim(Prim::Nat(Nat::new(1usize)));
                    Term::prim(Prim::nat_sub(n.clone(), one))
                };
                match (&*start_reduced, &*end_reduced) {
                    (
                        Subterm::Prim(Prim::Nat(Nat::Zero)),
                        Subterm::Prim(Prim::Nat(Nat::Succ(..))),
                    ) => {
                        let zero = Term::prim(Prim::Nat(Nat::Zero));
                        let rest = Term::prim(Prim::bin_slice(tail, zero, dec(&end_reduced)));
                        let consed = Term::prim(Prim::bin_concat([head, rest]));
                        return reduce(context, consed).map(Term::unwrap_or_clone);
                    }
                    (Subterm::Prim(Prim::Nat(Nat::Succ(..))), _) => {
                        let sliced =
                            Term::prim(Prim::bin_slice(tail, dec(&start_reduced), dec(&end_reduced)));
                        return reduce(context, sliced).map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Prim(Prim::bin_slice(
                bin,
                start_reduced,
                end_reduced,
            )))
        }
        Prim::BinAppend(bin, byte) => {
            let bin = reduce(context, bin.clone())?;
            let byte = reduce(context, byte.clone())?;
            // A concrete byte is taken mod 256 — its low 8 bits — matching the
            // runtime's packed-`i8` store and the optimizer's `as u8`. A symbolic
            // operand has no `as_nat`, so it stays stuck rather than truncating.
            let n = byte
                .as_nat()
                .and_then(|n| n.to_big_uint())
                .map(|big| big.to_bytes_le().first().copied().unwrap_or(0));
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
            // Normalise by the monoid unit/associativity laws — drop the empty
            // bytestring (so `concat(\\, a)`/`concat(a, \\)` collapse to `a`), merge
            // adjacent literal runs, collapse a lone operand. The definitional
            // partner of `peel_bin`'s `\\`-handling (`core::spine`); see
            // `normalize_concat`.
            fn literal(operand: &Term) -> Option<&[u8]> {
                match &**operand {
                    Subterm::Prim(Prim::Bin(bytes)) => Some(bytes.as_slice()),
                    _ => None,
                }
            }
            Ok(normalize_concat(
                reduced,
                literal,
                |bytes| Subterm::Prim(Prim::Bin(bytes)),
                |kept| Subterm::Prim(Prim::BinConcat(kept)),
            ))
        }
        Prim::BinFlatten(operand) => {
            let operand = reduce(context, operand.clone())?;
            match Term::unwrap_or_clone(operand) {
                // A literal outer array flattens to the concatenation of its inner
                // `Bin`s. Reducing the `BinConcat` merges all-literal parts to one
                // byte literal, while symbolic parts (a mapped `to_bin`, a variable)
                // survive as a `BinConcat` rather than getting stuck.
                Subterm::Prim(Prim::Arr(parts)) => {
                    reduce(context, Subterm::Prim(Prim::BinConcat(parts)).into())
                        .map(Term::unwrap_or_clone)
                }
                // `flatten` distributes over array concatenation: a symbolic outer
                // cons `cons(x, xs) = concat([x], xs)` flattens to
                // `concat(flatten([x]), flatten(xs)) = concat(x, flatten(xs))` — the
                // one-step decode the `Bin/flatten` structural proofs
                // (`flatten_closed`) rely on, mirroring the `Bin` eliminator's rule.
                Subterm::Prim(Prim::ArrConcat(_elem, segments)) => {
                    let distributed = segments
                        .into_iter()
                        .map(|seg| Subterm::Prim(Prim::bin_flatten(seg)).into())
                        .collect::<Vec<Term>>();
                    reduce(context, Subterm::Prim(Prim::BinConcat(distributed)).into())
                        .map(Term::unwrap_or_clone)
                }
                operand => Ok(Subterm::Prim(Prim::bin_flatten(operand))),
            }
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
            // The `Arr` twin of `BinConcat` normalisation: drop the empty array (so
            // `concat([], a)`/`concat(a, [])` collapse to `a`), merge adjacent literal
            // runs, collapse a lone operand — the definitional partner of `peel_arr`'s
            // `[]`-handling (`core::spine`); see `normalize_concat`.
            fn literal(operand: &Term) -> Option<&[Term]> {
                match &**operand {
                    Subterm::Prim(Prim::Arr(elems)) => Some(elems.as_slice()),
                    _ => None,
                }
            }
            Ok(normalize_concat(
                reduced,
                literal,
                |elems| Subterm::Prim(Prim::Arr(elems)),
                |kept| Subterm::Prim(Prim::arr_concat(type_, kept)),
            ))
        }
        Prim::ArrFlatten(type_, operand) => {
            let type_ = reduce(context, type_.clone())?;
            let operand = reduce(context, operand.clone())?;
            // A literal outer array of literal inner arrays flattens to one array.
            let merged = match &*operand {
                Subterm::Prim(Prim::Arr(parts)) => parts.iter().try_fold(Vec::new(), |mut acc, t| {
                    if let Subterm::Prim(Prim::Arr(elems)) = &**t {
                        acc.extend(elems.iter().cloned());
                        Some(acc)
                    } else {
                        None
                    }
                }),
                _ => None,
            };
            Ok(match merged {
                Some(elems) => Subterm::Prim(Prim::Arr(elems)),
                None => Subterm::Prim(Prim::arr_flatten(type_, operand)),
            })
        }
        // The eliminator rule, mirroring the native `Arr` `match`: distribute the
        // map over the free-monoid spine so it reduces to the *same* normal form a
        // structural `foldr (::) []` would. `Arr/map(f) = foldr (\x ih. f x :: ih)
        // []` definitionally, so `to_bins = Arr/map(to_bin)` and the `/syn/Str`
        // `flatten` proof reduces identically (`concat([f h], Arr/map(f, t))`). A
        // symbolic array stays stuck after one peel — no O(n) unfold of a variable.
        Prim::ArrMap(a, b, f, arr) => {
            let a = reduce(context, a.clone())?;
            let b = reduce(context, b.clone())?;
            let f = reduce(context, f.clone())?;
            let arr = reduce(context, arr.clone())?;

            Ok(match &*arr {
                // Empty and literal arrays map elementwise; the literal case folds
                // to a literal so concrete maps collapse (bounded by the literal).
                Subterm::Prim(Prim::Arr(elems)) => Subterm::Prim(Prim::Arr(
                    elems
                        .iter()
                        .map(|x| Term::apply(f.clone(), [x.clone()]))
                        .collect(),
                )),
                // Distribute over the monoid generators so a symbolic cons
                // (`concat([h], t)`) peels: `concat(map f [h], map f t)` — the same
                // normal form the native `Arr` eliminator produces.
                Subterm::Prim(Prim::ArrConcat(_elem, segments)) => Subterm::Prim(Prim::ArrConcat(
                    b.clone(),
                    segments
                        .iter()
                        .map(|s| Term::prim(Prim::arr_map(a.clone(), b.clone(), f.clone(), s.clone())))
                        .collect(),
                )),
                Subterm::Prim(Prim::ArrAppend(_elem, base, x)) => Subterm::Prim(Prim::ArrAppend(
                    b.clone(),
                    Term::prim(Prim::arr_map(a.clone(), b.clone(), f.clone(), base.clone())),
                    Term::apply(f.clone(), [x.clone()]),
                )),
                _ => Subterm::Prim(Prim::arr_map(a, b, f, arr)),
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
        Prim::IoLookup(host, _) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoLookup",
            span: host.span(),
        }),
        Prim::IoResolve(handle) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoResolve",
            span: handle.span(),
        }),
        Prim::IoSocket(addr) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoSocket",
            span: addr.span(),
        }),
        Prim::IoBind(handle, ..) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoBind",
            span: handle.span(),
        }),
        Prim::IoConnect(handle, ..) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoConnect",
            span: handle.span(),
        }),
        Prim::IoListen(handle, ..) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoListen",
            span: handle.span(),
        }),
        Prim::IoAccept(handle) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoAccept",
            span: handle.span(),
        }),
        Prim::IoStartTls(handle, ..) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoStartTls",
            span: handle.span(),
        }),
        Prim::IoTlsServerConfig(cert, ..) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoTlsServerConfig",
            span: cert.span(),
        }),
        Prim::IoStartTlsServer(handle, ..) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoStartTlsServer",
            span: handle.span(),
        }),
        Prim::IoSetNonblocking(handle, ..) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoSetNonblocking",
            span: handle.span(),
        }),
        Prim::IoSetRecvTimeout(handle, ..) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoSetRecvTimeout",
            span: handle.span(),
        }),
        Prim::IoSetSendTimeout(handle, ..) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoSetSendTimeout",
            span: handle.span(),
        }),
        Prim::IoSetReuseaddr(handle, ..) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoSetReuseaddr",
            span: handle.span(),
        }),
        Prim::IoPoll(handles, ..) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoPoll",
            span: handles.span(),
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
        Prim::CellType(elem) => {
            let elem = reduce(context, elem.clone())?;
            Ok(Subterm::Prim(Prim::cell_type(elem)))
        }
        Prim::Cell(_, init) => Err(ReduceError::IoAtTypeLevel {
            kind: "Cell",
            span: init.span(),
        }),
        Prim::CellSet(_, cell, _) => Err(ReduceError::IoAtTypeLevel {
            kind: "CellSet",
            span: cell.span(),
        }),
        Prim::CellGet(_, cell) => Err(ReduceError::IoAtTypeLevel {
            kind: "CellGet",
            span: cell.span(),
        }),
    }
}
