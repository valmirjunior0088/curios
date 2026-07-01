use {
    super::Convert,
    crate::{Peel, Prim, ReduceError, Term, peel_arr, peel_bin, peel_nat},
};

pub fn convert_prim(cmp: &mut Convert, this: Prim, that: Prim) -> Result<bool, ReduceError> {
    // Two `Bin`/`Arr` values are the free monoid on their bytes/elements: peel the
    // longest common prefix (`core::spine`). `Stuck` falls through to the
    // structural arms below, which compare like-shaped symbolic operands (slices,
    // appends) and `Arr` element runs the peel leaves opaque — so the peel only
    // ever strengthens conversion, never weakens it.
    if let Some(peel) = peel_bin(&this, &that).or_else(|| peel_arr(&this, &that)) {
        match peel {
            Peel::Equal => return Ok(true),
            Peel::Clash => return Ok(false),
            Peel::Continue(left, right) => {
                cmp.enqueue(Term::type_(), left, right);
                return Ok(true);
            }
            Peel::Stuck => {}
        }
    }

    match (this, that) {
        (Prim::NatType, Prim::NatType)
        | (Prim::IntType, Prim::IntType)
        | (Prim::FltType, Prim::FltType)
        | (Prim::BinType, Prim::BinType)
        | (Prim::IoType, Prim::IoType) => Ok(true),
        (Prim::Io(this), Prim::Io(that)) => Ok(this == that),
        // Two `Nat`s are the free monoid on one generator: peel the shared
        // successor spine and enqueue the residual tails (`core::spine`).
        (Prim::Nat(actual), Prim::Nat(target)) => Ok(match peel_nat(&actual, &target) {
            Peel::Equal => true,
            Peel::Clash | Peel::Stuck => false,
            Peel::Continue(left, right) => {
                cmp.enqueue(Term::type_(), left, right);
                true
            }
        }),
        (Prim::Int(this), Prim::Int(that)) => Ok(this == that),
        (Prim::Flt(this), Prim::Flt(that)) => Ok(this == that),
        (Prim::Bin(this), Prim::Bin(that)) => Ok(this == that),
        (Prim::IoEql(this_left, this_right), Prim::IoEql(that_left, that_right))
        | (Prim::NatEql(this_left, this_right), Prim::NatEql(that_left, that_right))
        | (Prim::NatNeq(this_left, this_right), Prim::NatNeq(that_left, that_right))
        | (Prim::NatAdd(this_left, this_right), Prim::NatAdd(that_left, that_right))
        | (Prim::NatSub(this_left, this_right), Prim::NatSub(that_left, that_right))
        | (Prim::NatMul(this_left, this_right), Prim::NatMul(that_left, that_right))
        | (Prim::NatLt(this_left, this_right), Prim::NatLt(that_left, that_right))
        | (Prim::NatDiv(this_left, this_right), Prim::NatDiv(that_left, that_right))
        | (Prim::NatRem(this_left, this_right), Prim::NatRem(that_left, that_right))
        | (Prim::NatGt(this_left, this_right), Prim::NatGt(that_left, that_right))
        | (Prim::NatLte(this_left, this_right), Prim::NatLte(that_left, that_right))
        | (Prim::NatGte(this_left, this_right), Prim::NatGte(that_left, that_right))
        | (Prim::NatAnd(this_left, this_right), Prim::NatAnd(that_left, that_right))
        | (Prim::NatOr(this_left, this_right), Prim::NatOr(that_left, that_right))
        | (Prim::NatXor(this_left, this_right), Prim::NatXor(that_left, that_right))
        | (Prim::NatShl(this_left, this_right), Prim::NatShl(that_left, that_right))
        | (Prim::NatShr(this_left, this_right), Prim::NatShr(that_left, that_right))
        | (Prim::BlnAnd(this_left, this_right), Prim::BlnAnd(that_left, that_right))
        | (Prim::BlnOr(this_left, this_right), Prim::BlnOr(that_left, that_right))
        | (Prim::BlnXor(this_left, this_right), Prim::BlnXor(that_left, that_right))
        | (Prim::BlnEql(this_left, this_right), Prim::BlnEql(that_left, that_right))
        | (Prim::BlnNeq(this_left, this_right), Prim::BlnNeq(that_left, that_right))
        | (Prim::IntEql(this_left, this_right), Prim::IntEql(that_left, that_right))
        | (Prim::IntNeq(this_left, this_right), Prim::IntNeq(that_left, that_right))
        | (Prim::IntAdd(this_left, this_right), Prim::IntAdd(that_left, that_right))
        | (Prim::IntSub(this_left, this_right), Prim::IntSub(that_left, that_right))
        | (Prim::IntMul(this_left, this_right), Prim::IntMul(that_left, that_right))
        | (Prim::IntDiv(this_left, this_right), Prim::IntDiv(that_left, that_right))
        | (Prim::IntRem(this_left, this_right), Prim::IntRem(that_left, that_right))
        | (Prim::IntLt(this_left, this_right), Prim::IntLt(that_left, that_right))
        | (Prim::IntGt(this_left, this_right), Prim::IntGt(that_left, that_right))
        | (Prim::IntLte(this_left, this_right), Prim::IntLte(that_left, that_right))
        | (Prim::IntGte(this_left, this_right), Prim::IntGte(that_left, that_right))
        | (Prim::IntAnd(this_left, this_right), Prim::IntAnd(that_left, that_right))
        | (Prim::IntOr(this_left, this_right), Prim::IntOr(that_left, that_right))
        | (Prim::IntXor(this_left, this_right), Prim::IntXor(that_left, that_right))
        | (Prim::IntShl(this_left, this_right), Prim::IntShl(that_left, that_right))
        | (Prim::IntShr(this_left, this_right), Prim::IntShr(that_left, that_right))
        | (Prim::FltAdd(this_left, this_right), Prim::FltAdd(that_left, that_right))
        | (Prim::FltSub(this_left, this_right), Prim::FltSub(that_left, that_right))
        | (Prim::FltMul(this_left, this_right), Prim::FltMul(that_left, that_right))
        | (Prim::FltDiv(this_left, this_right), Prim::FltDiv(that_left, that_right))
        | (Prim::FltRem(this_left, this_right), Prim::FltRem(that_left, that_right))
        | (Prim::FltEql(this_left, this_right), Prim::FltEql(that_left, that_right))
        | (Prim::FltNeq(this_left, this_right), Prim::FltNeq(that_left, that_right))
        | (Prim::FltLt(this_left, this_right), Prim::FltLt(that_left, that_right))
        | (Prim::FltGt(this_left, this_right), Prim::FltGt(that_left, that_right))
        | (Prim::FltLte(this_left, this_right), Prim::FltLte(that_left, that_right))
        | (Prim::FltGte(this_left, this_right), Prim::FltGte(that_left, that_right))
        | (Prim::FltMin(this_left, this_right), Prim::FltMin(that_left, that_right))
        | (Prim::FltMax(this_left, this_right), Prim::FltMax(that_left, that_right))
        | (Prim::BinEql(this_left, this_right), Prim::BinEql(that_left, that_right))
        | (Prim::BinGet(this_left, this_right), Prim::BinGet(that_left, that_right))
        | (Prim::BinAppend(this_left, this_right), Prim::BinAppend(that_left, that_right)) => {
            cmp.enqueue(Term::type_(), this_left, that_left);
            cmp.enqueue(Term::type_(), this_right, that_right);

            Ok(true)
        }
        (Prim::FltNeg(this), Prim::FltNeg(that))
        | (Prim::FltAbs(this), Prim::FltAbs(that))
        | (Prim::FltSqrt(this), Prim::FltSqrt(that))
        | (Prim::FltFloor(this), Prim::FltFloor(that))
        | (Prim::FltCeil(this), Prim::FltCeil(that))
        | (Prim::FltTrunc(this), Prim::FltTrunc(that))
        | (Prim::FltNearest(this), Prim::FltNearest(that))
        | (Prim::FltToLeBin(this), Prim::FltToLeBin(that))
        | (Prim::NatToInt(this), Prim::NatToInt(that))
        | (Prim::NatToFlt(this), Prim::NatToFlt(that))
        | (Prim::IntToNat(this), Prim::IntToNat(that))
        | (Prim::IntToFlt(this), Prim::IntToFlt(that))
        | (Prim::FltToNat(this), Prim::FltToNat(that))
        | (Prim::FltToInt(this), Prim::FltToInt(that))
        | (Prim::BinLen(this), Prim::BinLen(that))
        | (Prim::BinFlatten(this), Prim::BinFlatten(that))
        | (Prim::ArrType(this), Prim::ArrType(that))
        | (Prim::CellType(this), Prim::CellType(that)) => {
            cmp.enqueue(Term::type_(), this, that);

            Ok(true)
        }
        (
            Prim::BinSlice(this_bin, this_start, this_end),
            Prim::BinSlice(that_bin, that_start, that_end),
        ) => {
            cmp.enqueue(Term::type_(), this_bin, that_bin);
            cmp.enqueue(Term::type_(), this_start, that_start);
            cmp.enqueue(Term::type_(), this_end, that_end);

            Ok(true)
        }
        (
            Prim::ArrSlice(this_ty, this_list, this_start, this_end),
            Prim::ArrSlice(that_ty, that_list, that_start, that_end),
        ) => {
            cmp.enqueue(Term::type_(), this_ty, that_ty);
            cmp.enqueue(Term::type_(), this_list, that_list);
            cmp.enqueue(Term::type_(), this_start, that_start);
            cmp.enqueue(Term::type_(), this_end, that_end);

            Ok(true)
        }
        (
            Prim::ArrGet(this_ty, this_list, this_index),
            Prim::ArrGet(that_ty, that_list, that_index),
        ) => {
            cmp.enqueue(Term::type_(), this_ty, that_ty);
            cmp.enqueue(Term::type_(), this_list, that_list);
            cmp.enqueue(Term::type_(), this_index, that_index);

            Ok(true)
        }
        (Prim::ArrLen(this_ty, this_list), Prim::ArrLen(that_ty, that_list)) => {
            cmp.enqueue(Term::type_(), this_ty, that_ty);
            cmp.enqueue(Term::type_(), this_list, that_list);

            Ok(true)
        }
        (
            Prim::ArrAppend(this_ty, this_list, this_elem),
            Prim::ArrAppend(that_ty, that_list, that_elem),
        ) => {
            cmp.enqueue(Term::type_(), this_ty, that_ty);
            cmp.enqueue(Term::type_(), this_list, that_list);
            cmp.enqueue(Term::type_(), this_elem, that_elem);

            Ok(true)
        }
        (Prim::Arr(this_ops), Prim::Arr(that_ops))
        | (Prim::BinConcat(this_ops), Prim::BinConcat(that_ops)) => {
            if this_ops.len() != that_ops.len() {
                return Ok(false);
            }

            for (this, that) in this_ops.into_iter().zip(that_ops) {
                cmp.enqueue(Term::type_(), this, that);
            }

            Ok(true)
        }
        (Prim::ArrConcat(this_ty, this_ops), Prim::ArrConcat(that_ty, that_ops)) => {
            if this_ops.len() != that_ops.len() {
                return Ok(false);
            }
            cmp.enqueue(Term::type_(), this_ty, that_ty);
            for (this, that) in this_ops.into_iter().zip(that_ops) {
                cmp.enqueue(Term::type_(), this, that);
            }
            Ok(true)
        }
        (Prim::ArrFlatten(this_ty, this_op), Prim::ArrFlatten(that_ty, that_op)) => {
            cmp.enqueue(Term::type_(), this_ty, that_ty);
            cmp.enqueue(Term::type_(), this_op, that_op);

            Ok(true)
        }
        (_, _) => Ok(false),
    }
}
