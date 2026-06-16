use {
    super::erase,
    crate::{
        core::{Context, Error, Int, Nat, Prim, Subterm, Term, reduce_with},
        ersd,
    },
    num_bigint::BigUint,
    num_traits::ToPrimitive,
};

fn narrow_nat(k: &BigUint) -> Result<u32, Error> {
    k.to_u32().ok_or_else(|| Error::nat_overflow(k.clone()))
}

/// Narrow an unbounded type-level `Int` to `ersd`'s `i32` carrier, like
/// [`narrow_nat`]'s u32. The runtime's own i31 limit is enforced where it
/// appears, in the `cont` → wasm lowering.
fn narrow_int(value: &Int) -> Result<i32, Error> {
    value
        .to_i32()
        .ok_or_else(|| Error::int_overflow(value.clone()))
}

fn prim_type(prim: Prim) -> Term {
    Subterm::Prim(prim).into()
}

fn nat_type() -> Term {
    prim_type(Prim::NatType)
}

fn int_type() -> Term {
    prim_type(Prim::IntType)
}

fn flt_type() -> Term {
    prim_type(Prim::FltType)
}

fn bin_type() -> Term {
    prim_type(Prim::BinType)
}

fn str_type() -> Term {
    prim_type(Prim::StrType)
}

fn bln_type() -> Term {
    prim_type(Prim::BlnType)
}

fn pure(prim: ersd::PurePrim) -> ersd::Term {
    ersd::Subterm::Prim(ersd::Prim::Pure(prim)).into()
}

fn host(prim: ersd::HostPrim) -> ersd::Term {
    ersd::Subterm::Prim(ersd::Prim::Host(prim)).into()
}

/// Lower a primitive to its `ersd` form. Pure downstream lowering: the term is
/// already well-typed (elaborate discharged every obligation) and meta-free
/// (zonk ran), so there is no checking here. `expected` is consumed only where a
/// runtime shape must be read off the type — the element type of an array literal
/// (§9).
pub fn erase_prim(
    context: &mut Context,
    _term: &Term,
    prim: &Prim,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    match prim {
        Prim::BlnType => Ok(ersd::Subterm::Erased.into()),
        &Prim::Bln(value) => Ok(pure(ersd::PurePrim::Nat(if value { 1 } else { 0 }))),
        // `Bln` rides the `0`/`1` i31 carrier, so its logic ops are exactly the
        // `Nat` bit ops on a single bit. `not` flips bit 0 with `xor 1`.
        Prim::BlnAnd(left, right) => Ok(pure(ersd::PurePrim::NatAnd(
            erase(context, left, &bln_type())?,
            erase(context, right, &bln_type())?,
        ))),
        Prim::BlnOr(left, right) => Ok(pure(ersd::PurePrim::NatOr(
            erase(context, left, &bln_type())?,
            erase(context, right, &bln_type())?,
        ))),
        Prim::BlnXor(left, right) => Ok(pure(ersd::PurePrim::NatXor(
            erase(context, left, &bln_type())?,
            erase(context, right, &bln_type())?,
        ))),
        Prim::NatType => Ok(ersd::Subterm::Erased.into()),
        Prim::Nat(Nat::Zero) => Ok(pure(ersd::PurePrim::Nat(0))),
        Prim::Nat(Nat::Succ(spine, inner)) => {
            if matches!(inner.as_ref(), Subterm::Prim(Prim::Nat(Nat::Zero))) {
                Ok(pure(ersd::PurePrim::Nat(narrow_nat(spine)?)))
            } else {
                let inner_ersd = erase(context, inner, &nat_type())?;
                let spine_term = pure(ersd::PurePrim::Nat(narrow_nat(spine)?));
                Ok(pure(ersd::PurePrim::NatAdd(spine_term, inner_ersd)))
            }
        }
        Prim::NatEql(left, right) => Ok(pure(ersd::PurePrim::NatEql(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::NatAdd(left, right) => Ok(pure(ersd::PurePrim::NatAdd(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::NatSub(left, right) => Ok(pure(ersd::PurePrim::NatSub(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::NatMul(left, right) => Ok(pure(ersd::PurePrim::NatMul(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::NatNeq(left, right) => Ok(pure(ersd::PurePrim::NatNeq(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::NatDiv(left, right) => Ok(pure(ersd::PurePrim::NatDiv(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::NatRem(left, right) => Ok(pure(ersd::PurePrim::NatRem(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::NatLt(left, right) => Ok(pure(ersd::PurePrim::NatLt(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::NatGt(left, right) => Ok(pure(ersd::PurePrim::NatGt(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::NatLte(left, right) => Ok(pure(ersd::PurePrim::NatLte(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::NatGte(left, right) => Ok(pure(ersd::PurePrim::NatGte(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::NatAnd(left, right) => Ok(pure(ersd::PurePrim::NatAnd(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::NatOr(left, right) => Ok(pure(ersd::PurePrim::NatOr(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::NatXor(left, right) => Ok(pure(ersd::PurePrim::NatXor(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::NatShl(left, right) => Ok(pure(ersd::PurePrim::NatShl(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::NatShr(left, right) => Ok(pure(ersd::PurePrim::NatShr(
            erase(context, left, &nat_type())?,
            erase(context, right, &nat_type())?,
        ))),
        Prim::IntType => Ok(ersd::Subterm::Erased.into()),
        Prim::Int(value) => Ok(pure(ersd::PurePrim::Int(narrow_int(value)?))),
        Prim::IntEql(left, right) => Ok(pure(ersd::PurePrim::IntEql(
            erase(context, left, &int_type())?,
            erase(context, right, &int_type())?,
        ))),
        Prim::IntNeq(left, right) => Ok(pure(ersd::PurePrim::IntNeq(
            erase(context, left, &int_type())?,
            erase(context, right, &int_type())?,
        ))),
        Prim::IntAdd(left, right) => Ok(pure(ersd::PurePrim::IntAdd(
            erase(context, left, &int_type())?,
            erase(context, right, &int_type())?,
        ))),
        Prim::IntSub(left, right) => Ok(pure(ersd::PurePrim::IntSub(
            erase(context, left, &int_type())?,
            erase(context, right, &int_type())?,
        ))),
        Prim::IntMul(left, right) => Ok(pure(ersd::PurePrim::IntMul(
            erase(context, left, &int_type())?,
            erase(context, right, &int_type())?,
        ))),
        Prim::IntDiv(left, right) => Ok(pure(ersd::PurePrim::IntDiv(
            erase(context, left, &int_type())?,
            erase(context, right, &int_type())?,
        ))),
        Prim::IntRem(left, right) => Ok(pure(ersd::PurePrim::IntRem(
            erase(context, left, &int_type())?,
            erase(context, right, &int_type())?,
        ))),
        Prim::IntLt(left, right) => Ok(pure(ersd::PurePrim::IntLt(
            erase(context, left, &int_type())?,
            erase(context, right, &int_type())?,
        ))),
        Prim::IntGt(left, right) => Ok(pure(ersd::PurePrim::IntGt(
            erase(context, left, &int_type())?,
            erase(context, right, &int_type())?,
        ))),
        Prim::IntLte(left, right) => Ok(pure(ersd::PurePrim::IntLte(
            erase(context, left, &int_type())?,
            erase(context, right, &int_type())?,
        ))),
        Prim::IntGte(left, right) => Ok(pure(ersd::PurePrim::IntGte(
            erase(context, left, &int_type())?,
            erase(context, right, &int_type())?,
        ))),
        Prim::FltType => Ok(ersd::Subterm::Erased.into()),
        &Prim::Flt(flt) => Ok(pure(ersd::PurePrim::Flt(flt.to_f32()))),
        Prim::FltAdd(left, right) => Ok(pure(ersd::PurePrim::FltAdd(
            erase(context, left, &flt_type())?,
            erase(context, right, &flt_type())?,
        ))),
        Prim::FltSub(left, right) => Ok(pure(ersd::PurePrim::FltSub(
            erase(context, left, &flt_type())?,
            erase(context, right, &flt_type())?,
        ))),
        Prim::FltMul(left, right) => Ok(pure(ersd::PurePrim::FltMul(
            erase(context, left, &flt_type())?,
            erase(context, right, &flt_type())?,
        ))),
        Prim::FltNeg(inner) => Ok(pure(ersd::PurePrim::FltNeg(erase(
            context,
            inner,
            &flt_type(),
        )?))),
        Prim::FltAbs(inner) => Ok(pure(ersd::PurePrim::FltAbs(erase(
            context,
            inner,
            &flt_type(),
        )?))),
        Prim::FltSqrt(inner) => Ok(pure(ersd::PurePrim::FltSqrt(erase(
            context,
            inner,
            &flt_type(),
        )?))),
        Prim::FltFloor(inner) => Ok(pure(ersd::PurePrim::FltFloor(erase(
            context,
            inner,
            &flt_type(),
        )?))),
        Prim::FltCeil(inner) => Ok(pure(ersd::PurePrim::FltCeil(erase(
            context,
            inner,
            &flt_type(),
        )?))),
        Prim::FltTrunc(inner) => Ok(pure(ersd::PurePrim::FltTrunc(erase(
            context,
            inner,
            &flt_type(),
        )?))),
        Prim::FltNearest(inner) => Ok(pure(ersd::PurePrim::FltNearest(erase(
            context,
            inner,
            &flt_type(),
        )?))),
        Prim::FltDiv(left, right) => Ok(pure(ersd::PurePrim::FltDiv(
            erase(context, left, &flt_type())?,
            erase(context, right, &flt_type())?,
        ))),
        Prim::FltMin(left, right) => Ok(pure(ersd::PurePrim::FltMin(
            erase(context, left, &flt_type())?,
            erase(context, right, &flt_type())?,
        ))),
        Prim::FltMax(left, right) => Ok(pure(ersd::PurePrim::FltMax(
            erase(context, left, &flt_type())?,
            erase(context, right, &flt_type())?,
        ))),
        Prim::FltEql(left, right) => Ok(pure(ersd::PurePrim::FltEql(
            erase(context, left, &flt_type())?,
            erase(context, right, &flt_type())?,
        ))),
        Prim::FltNeq(left, right) => Ok(pure(ersd::PurePrim::FltNeq(
            erase(context, left, &flt_type())?,
            erase(context, right, &flt_type())?,
        ))),
        Prim::FltLt(left, right) => Ok(pure(ersd::PurePrim::FltLt(
            erase(context, left, &flt_type())?,
            erase(context, right, &flt_type())?,
        ))),
        Prim::FltGt(left, right) => Ok(pure(ersd::PurePrim::FltGt(
            erase(context, left, &flt_type())?,
            erase(context, right, &flt_type())?,
        ))),
        Prim::FltLte(left, right) => Ok(pure(ersd::PurePrim::FltLte(
            erase(context, left, &flt_type())?,
            erase(context, right, &flt_type())?,
        ))),
        Prim::FltGte(left, right) => Ok(pure(ersd::PurePrim::FltGte(
            erase(context, left, &flt_type())?,
            erase(context, right, &flt_type())?,
        ))),
        Prim::NatToStr(inner) => Ok(pure(ersd::PurePrim::NatToStr(erase(
            context,
            inner,
            &nat_type(),
        )?))),
        Prim::IntToStr(inner) => Ok(pure(ersd::PurePrim::IntToStr(erase(
            context,
            inner,
            &int_type(),
        )?))),
        Prim::FltToStr(inner) => Ok(pure(ersd::PurePrim::FltToStr(erase(
            context,
            inner,
            &flt_type(),
        )?))),
        Prim::FltToLeBin(inner) => Ok(pure(ersd::PurePrim::FltToLeBin(erase(
            context,
            inner,
            &flt_type(),
        )?))),
        Prim::NatToInt(inner) => Ok(pure(ersd::PurePrim::NatToInt(erase(
            context,
            inner,
            &nat_type(),
        )?))),
        Prim::IntToNat(inner) => Ok(pure(ersd::PurePrim::IntToNat(erase(
            context,
            inner,
            &int_type(),
        )?))),
        Prim::IntToFlt(inner) => Ok(pure(ersd::PurePrim::IntToFlt(erase(
            context,
            inner,
            &int_type(),
        )?))),
        Prim::NatToFlt(inner) => Ok(pure(ersd::PurePrim::NatToFlt(erase(
            context,
            inner,
            &nat_type(),
        )?))),
        Prim::FltToInt(inner) => Ok(pure(ersd::PurePrim::FltToInt(erase(
            context,
            inner,
            &flt_type(),
        )?))),
        Prim::FltToNat(inner) => Ok(pure(ersd::PurePrim::FltToNat(erase(
            context,
            inner,
            &flt_type(),
        )?))),
        Prim::BinType => Ok(ersd::Subterm::Erased.into()),
        Prim::Bin(bytes) => Ok(pure(ersd::PurePrim::Bin(bytes.clone()))),
        Prim::BinLen(bin) => Ok(pure(ersd::PurePrim::BinLen(erase(
            context,
            bin,
            &bin_type(),
        )?))),
        Prim::BinEql(left, right) => Ok(pure(ersd::PurePrim::BinEql(
            erase(context, left, &bin_type())?,
            erase(context, right, &bin_type())?,
        ))),
        Prim::BinGet(bin, index) => Ok(pure(ersd::PurePrim::BinGet(
            erase(context, bin, &bin_type())?,
            erase(context, index, &nat_type())?,
        ))),
        Prim::BinSlice(bin, start, end) => Ok(pure(ersd::PurePrim::BinSlice(
            erase(context, bin, &bin_type())?,
            erase(context, start, &nat_type())?,
            erase(context, end, &nat_type())?,
        ))),
        Prim::BinAppend(bin, byte) => Ok(pure(ersd::PurePrim::BinAppend(
            erase(context, bin, &bin_type())?,
            erase(context, byte, &nat_type())?,
        ))),
        Prim::BinConcat(operands) => {
            let erased = operands
                .iter()
                .map(|e| erase(context, e, &bin_type()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(pure(ersd::PurePrim::BinConcat(erased)))
        }
        // `Str` shares `Bin`'s runtime representation (a UTF-8 byte buffer), so the
        // type erases like `Bin` and the two conversions are runtime no-ops.
        Prim::StrType => Ok(ersd::Subterm::Erased.into()),
        Prim::Str(bytes) => Ok(pure(ersd::PurePrim::Bin(bytes.clone()))),
        Prim::StrToBin(str) => erase(context, str, &str_type()),
        Prim::StrOfBin(bin) => erase(context, bin, &bin_type()),
        Prim::ArrType(_) => Ok(ersd::Subterm::Erased.into()),
        Prim::Arr(elems) => {
            // Elaborate already checked this literal against an array type (§9);
            // the element type is re-derived here only to lower the elements.
            let elem_type = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
                Subterm::Prim(Prim::ArrType(elem_type)) => elem_type,
                _ => unreachable!("erase: array literal checked against non-array type"),
            };
            let erased_elems = elems
                .iter()
                .map(|e| erase(context, e, &elem_type))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(pure(ersd::PurePrim::Arr(erased_elems)))
        }
        Prim::ArrLen(type_, list) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list_erased = erase(context, list, &expected_list_type)?;
            Ok(pure(ersd::PurePrim::ArrLen(list_erased)))
        }
        Prim::ArrGet(type_, list, index) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list_erased = erase(context, list, &expected_list_type)?;
            let index_erased = erase(context, index, &nat_type())?;
            Ok(pure(ersd::PurePrim::ArrGet(list_erased, index_erased)))
        }
        Prim::ArrSlice(type_, list, start, end) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list_erased = erase(context, list, &expected_list_type)?;
            let start_erased = erase(context, start, &nat_type())?;
            let end_erased = erase(context, end, &nat_type())?;
            Ok(pure(ersd::PurePrim::ArrSlice(
                list_erased,
                start_erased,
                end_erased,
            )))
        }
        Prim::ArrAppend(type_, list, elem) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list_erased = erase(context, list, &expected_list_type)?;
            let elem_erased = erase(context, elem, type_)?;
            Ok(pure(ersd::PurePrim::ArrAppend(list_erased, elem_erased)))
        }
        Prim::ArrConcat(type_, operands) => {
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let erased = operands
                .iter()
                .map(|e| erase(context, e, &expected_list_type))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(pure(ersd::PurePrim::ArrConcat(erased)))
        }
        Prim::IoType => Ok(ersd::Subterm::Erased.into()),
        &Prim::Io(token) => Ok(pure(ersd::PurePrim::Io(token))),
        Prim::IoRead(handle, count) => Ok(host(ersd::HostPrim::IoRead(
            erase(context, handle, &prim_type(Prim::IoType))?,
            erase(context, count, &nat_type())?,
        ))),
        Prim::IoWrite(handle, bytes) => Ok(host(ersd::HostPrim::IoWrite(
            erase(context, handle, &prim_type(Prim::IoType))?,
            erase(context, bytes, &bin_type())?,
        ))),
        Prim::IoOpen(path, mode) => Ok(host(ersd::HostPrim::IoOpen(
            erase(context, path, &bin_type())?,
            erase(context, mode, &nat_type())?,
        ))),
        Prim::IoConnect(address, port, connect_timeout, read_timeout, write_timeout) => {
            Ok(host(ersd::HostPrim::IoConnect(
                erase(context, address, &bin_type())?,
                erase(context, port, &nat_type())?,
                erase(context, connect_timeout, &nat_type())?,
                erase(context, read_timeout, &nat_type())?,
                erase(context, write_timeout, &nat_type())?,
            )))
        }
        Prim::IoClose(handle) => Ok(host(ersd::HostPrim::IoClose(erase(
            context,
            handle,
            &prim_type(Prim::IoType),
        )?))),
        Prim::IoClockWall => Ok(host(ersd::HostPrim::IoClockWall)),
        Prim::IoClockMono => Ok(host(ersd::HostPrim::IoClockMono)),
        Prim::IoRandom(count) => Ok(host(ersd::HostPrim::IoRandom(erase(
            context,
            count,
            &nat_type(),
        )?))),
        Prim::IoArgs => Ok(host(ersd::HostPrim::IoArgs)),
        Prim::IoEnv(name) => Ok(host(ersd::HostPrim::IoEnv(erase(
            context,
            name,
            &bin_type(),
        )?))),
        // The polymorphic result type is type-only; only the code survives.
        Prim::IoExit(_, code) => Ok(host(ersd::HostPrim::IoExit(erase(
            context,
            code,
            &nat_type(),
        )?))),
    }
}
