use {
    super::erase,
    crate::core::{Context, Error, Int, Nat, Prim, Subterm, Term, reduce_with},
    curios_ersd::ersd,
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

fn bln_type() -> Term {
    prim_type(Prim::BlnType)
}

fn io_type() -> Term {
    prim_type(Prim::IoType)
}

fn arr_type(elem: Term) -> Term {
    Subterm::Prim(Prim::ArrType(elem)).into()
}

fn pure(prim: ersd::PurePrim) -> ersd::Term {
    ersd::Subterm::Prim(ersd::Prim::Pure(prim)).into()
}

fn host(prim: ersd::HostPrim) -> ersd::Term {
    ersd::Subterm::Prim(ersd::Prim::Host(prim)).into()
}

fn cell(prim: ersd::CellPrim) -> ersd::Term {
    ersd::Subterm::Prim(ersd::Prim::Cell(prim)).into()
}

/// Erase both operands of a homogeneous binary primitive at `operand`, then
/// rebuild as the target `ersd::PurePrim` via its constructor (`build`). Lets the
/// scalar arms name themselves once instead of spelling the `pure`/`erase`
/// scaffold out per variant.
fn binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    operand: fn() -> Term,
    build: fn(ersd::Term, ersd::Term) -> ersd::PurePrim,
) -> Result<ersd::Term, Error> {
    Ok(pure(build(
        erase(context, left, &operand())?,
        erase(context, right, &operand())?,
    )))
}

/// The unary counterpart of [`binary`], for single-operand scalar primitives.
fn unary(
    context: &mut Context,
    inner: &Term,
    operand: fn() -> Term,
    build: fn(ersd::Term) -> ersd::PurePrim,
) -> Result<ersd::Term, Error> {
    Ok(pure(build(erase(context, inner, &operand())?)))
}

/// The host-prim analogue of [`binary`]: erase both operands and rebuild as a
/// `host`-wrapped `ersd::HostPrim`. Host operands are heterogeneous (a handle
/// against bytes, a count, a flag), so the two operand types are supplied
/// separately rather than shared.
fn host_binary(
    context: &mut Context,
    left: &Term,
    left_type: fn() -> Term,
    right: &Term,
    right_type: fn() -> Term,
    build: fn(ersd::Term, ersd::Term) -> ersd::HostPrim,
) -> Result<ersd::Term, Error> {
    Ok(host(build(
        erase(context, left, &left_type())?,
        erase(context, right, &right_type())?,
    )))
}

/// The host-prim analogue of [`unary`], for single-operand host primitives.
fn host_unary(
    context: &mut Context,
    inner: &Term,
    operand: fn() -> Term,
    build: fn(ersd::Term) -> ersd::HostPrim,
) -> Result<ersd::Term, Error> {
    Ok(host(build(erase(context, inner, &operand())?)))
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
        Prim::BlnAnd(l, r) => binary(context, l, r, bln_type, ersd::PurePrim::NatAnd),
        Prim::BlnOr(l, r) => binary(context, l, r, bln_type, ersd::PurePrim::NatOr),
        Prim::BlnXor(l, r) => binary(context, l, r, bln_type, ersd::PurePrim::NatXor),
        Prim::BlnEql(l, r) => binary(context, l, r, bln_type, ersd::PurePrim::NatEql),
        // `bln != bln` is xor on the `0`/`1` carrier, exactly like `BlnXor`.
        Prim::BlnNeq(l, r) => binary(context, l, r, bln_type, ersd::PurePrim::NatXor),
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
        Prim::NatEql(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatEql),
        // Handle identity stays abstract through `ersd`; the `ersd → cont`
        // lowering is the one place that knows a handle is bytes and turns this
        // into the corresponding `Bin` comparison.
        Prim::IoEql(l, r) => binary(context, l, r, io_type, ersd::PurePrim::IoEql),
        Prim::NatAdd(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatAdd),
        Prim::NatSub(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatSub),
        Prim::NatMul(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatMul),
        Prim::NatNeq(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatNeq),
        Prim::NatDiv(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatDiv),
        Prim::NatRem(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatRem),
        Prim::NatLt(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatLt),
        Prim::NatGt(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatGt),
        Prim::NatLte(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatLte),
        Prim::NatGte(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatGte),
        Prim::NatAnd(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatAnd),
        Prim::NatOr(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatOr),
        Prim::NatXor(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatXor),
        Prim::NatShl(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatShl),
        Prim::NatShr(l, r) => binary(context, l, r, nat_type, ersd::PurePrim::NatShr),
        Prim::IntType => Ok(ersd::Subterm::Erased.into()),
        Prim::Int(value) => Ok(pure(ersd::PurePrim::Int(narrow_int(value)?))),
        Prim::IntEql(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntEql),
        Prim::IntNeq(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntNeq),
        Prim::IntAdd(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntAdd),
        Prim::IntSub(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntSub),
        Prim::IntMul(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntMul),
        Prim::IntDiv(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntDiv),
        Prim::IntRem(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntRem),
        Prim::IntLt(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntLt),
        Prim::IntGt(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntGt),
        Prim::IntLte(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntLte),
        Prim::IntGte(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntGte),
        Prim::IntAnd(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntAnd),
        Prim::IntOr(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntOr),
        Prim::IntXor(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntXor),
        Prim::IntShl(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntShl),
        Prim::IntShr(l, r) => binary(context, l, r, int_type, ersd::PurePrim::IntShr),
        Prim::FltType => Ok(ersd::Subterm::Erased.into()),
        &Prim::Flt(flt) => Ok(pure(ersd::PurePrim::Flt(flt.to_f32()))),
        Prim::FltAdd(l, r) => binary(context, l, r, flt_type, ersd::PurePrim::FltAdd),
        Prim::FltSub(l, r) => binary(context, l, r, flt_type, ersd::PurePrim::FltSub),
        Prim::FltMul(l, r) => binary(context, l, r, flt_type, ersd::PurePrim::FltMul),
        Prim::FltNeg(i) => unary(context, i, flt_type, ersd::PurePrim::FltNeg),
        Prim::FltAbs(i) => unary(context, i, flt_type, ersd::PurePrim::FltAbs),
        Prim::FltSqrt(i) => unary(context, i, flt_type, ersd::PurePrim::FltSqrt),
        Prim::FltFloor(i) => unary(context, i, flt_type, ersd::PurePrim::FltFloor),
        Prim::FltCeil(i) => unary(context, i, flt_type, ersd::PurePrim::FltCeil),
        Prim::FltTrunc(i) => unary(context, i, flt_type, ersd::PurePrim::FltTrunc),
        Prim::FltNearest(i) => unary(context, i, flt_type, ersd::PurePrim::FltNearest),
        Prim::FltDiv(l, r) => binary(context, l, r, flt_type, ersd::PurePrim::FltDiv),
        Prim::FltRem(l, r) => binary(context, l, r, flt_type, ersd::PurePrim::FltRem),
        Prim::FltMin(l, r) => binary(context, l, r, flt_type, ersd::PurePrim::FltMin),
        Prim::FltMax(l, r) => binary(context, l, r, flt_type, ersd::PurePrim::FltMax),
        Prim::FltEql(l, r) => binary(context, l, r, flt_type, ersd::PurePrim::FltEql),
        Prim::FltNeq(l, r) => binary(context, l, r, flt_type, ersd::PurePrim::FltNeq),
        Prim::FltLt(l, r) => binary(context, l, r, flt_type, ersd::PurePrim::FltLt),
        Prim::FltGt(l, r) => binary(context, l, r, flt_type, ersd::PurePrim::FltGt),
        Prim::FltLte(l, r) => binary(context, l, r, flt_type, ersd::PurePrim::FltLte),
        Prim::FltGte(l, r) => binary(context, l, r, flt_type, ersd::PurePrim::FltGte),
        Prim::FltToLeBin(i) => unary(context, i, flt_type, ersd::PurePrim::FltToLeBin),
        Prim::NatToInt(i) => unary(context, i, nat_type, ersd::PurePrim::NatToInt),
        Prim::IntToNat(i) => unary(context, i, int_type, ersd::PurePrim::IntToNat),
        Prim::IntToFlt(i) => unary(context, i, int_type, ersd::PurePrim::IntToFlt),
        Prim::NatToFlt(i) => unary(context, i, nat_type, ersd::PurePrim::NatToFlt),
        Prim::FltToInt(i) => unary(context, i, flt_type, ersd::PurePrim::FltToInt),
        Prim::FltToNat(i) => unary(context, i, flt_type, ersd::PurePrim::FltToNat),
        Prim::BinType => Ok(ersd::Subterm::Erased.into()),
        Prim::Bin(bytes) => Ok(pure(ersd::PurePrim::Bin(bytes.clone()))),
        Prim::BinLen(bin) => unary(context, bin, bin_type, ersd::PurePrim::BinLen),
        Prim::BinEql(l, r) => binary(context, l, r, bin_type, ersd::PurePrim::BinEql),
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
        Prim::BinFlatten(operand) => {
            let outer_type = arr_type(bin_type());
            let operand = erase(context, operand, &outer_type)?;
            Ok(pure(ersd::PurePrim::BinFlatten(operand)))
        }
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
            let expected_list_type = arr_type(type_.clone());
            let list_erased = erase(context, list, &expected_list_type)?;
            Ok(pure(ersd::PurePrim::ArrLen(list_erased)))
        }
        Prim::ArrGet(type_, list, index) => {
            let expected_list_type = arr_type(type_.clone());
            let list_erased = erase(context, list, &expected_list_type)?;
            let index_erased = erase(context, index, &nat_type())?;
            Ok(pure(ersd::PurePrim::ArrGet(list_erased, index_erased)))
        }
        Prim::ArrSlice(type_, list, start, end) => {
            let expected_list_type = arr_type(type_.clone());
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
            let expected_list_type = arr_type(type_.clone());
            let list_erased = erase(context, list, &expected_list_type)?;
            let elem_erased = erase(context, elem, type_)?;
            Ok(pure(ersd::PurePrim::ArrAppend(list_erased, elem_erased)))
        }
        Prim::ArrConcat(type_, operands) => {
            let expected_list_type = arr_type(type_.clone());
            let erased = operands
                .iter()
                .map(|e| erase(context, e, &expected_list_type))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(pure(ersd::PurePrim::ArrConcat(erased)))
        }
        Prim::ArrFlatten(type_, operand) => {
            let outer_type = arr_type(arr_type(type_.clone()));
            let operand = erase(context, operand, &outer_type)?;
            Ok(pure(ersd::PurePrim::ArrFlatten(operand)))
        }
        Prim::ArrMap(a, b, f, arr) => {
            let f_type = Term::func_type([("x", a.clone())], b.clone());
            let f_erased = erase(context, f, &f_type)?;
            let arr_ty = arr_type(a.clone());
            let arr_erased = erase(context, arr, &arr_ty)?;
            Ok(pure(ersd::PurePrim::ArrMap(arr_erased, f_erased)))
        }
        Prim::IoType => Ok(ersd::Subterm::Erased.into()),
        &Prim::Io(token) => Ok(pure(ersd::PurePrim::Io(token))),
        Prim::IoRead(handle, count) => host_binary(
            context,
            handle,
            io_type,
            count,
            nat_type,
            ersd::HostPrim::IoRead,
        ),
        Prim::IoWrite(handle, bytes) => host_binary(
            context,
            handle,
            io_type,
            bytes,
            bin_type,
            ersd::HostPrim::IoWrite,
        ),
        Prim::IoOpen(path, mode) => host_binary(
            context,
            path,
            bin_type,
            mode,
            nat_type,
            ersd::HostPrim::IoOpen,
        ),
        Prim::IoLookup(host_, port) => host_binary(
            context,
            host_,
            bin_type,
            port,
            nat_type,
            ersd::HostPrim::IoLookup,
        ),
        Prim::IoResolve(handle) => host_unary(context, handle, io_type, ersd::HostPrim::IoResolve),
        Prim::IoSocket(addr) => host_unary(context, addr, bin_type, ersd::HostPrim::IoSocket),
        Prim::IoBind(handle, addr) => host_binary(
            context,
            handle,
            io_type,
            addr,
            bin_type,
            ersd::HostPrim::IoBind,
        ),
        Prim::IoConnect(handle, addr) => host_binary(
            context,
            handle,
            io_type,
            addr,
            bin_type,
            ersd::HostPrim::IoConnect,
        ),
        Prim::IoListen(handle, backlog) => host_binary(
            context,
            handle,
            io_type,
            backlog,
            nat_type,
            ersd::HostPrim::IoListen,
        ),
        Prim::IoAccept(handle) => host_unary(context, handle, io_type, ersd::HostPrim::IoAccept),
        Prim::IoStartTls(handle, sni) => host_binary(
            context,
            handle,
            io_type,
            sni,
            bin_type,
            ersd::HostPrim::IoStartTls,
        ),
        Prim::IoTlsServerConfig(cert, key) => host_binary(
            context,
            cert,
            bin_type,
            key,
            bin_type,
            ersd::HostPrim::IoTlsServerConfig,
        ),
        Prim::IoStartTlsServer(handle, cfg) => host_binary(
            context,
            handle,
            io_type,
            cfg,
            io_type,
            ersd::HostPrim::IoStartTlsServer,
        ),
        Prim::IoSetNonblocking(handle, on) => host_binary(
            context,
            handle,
            io_type,
            on,
            bln_type,
            ersd::HostPrim::IoSetNonblocking,
        ),
        Prim::IoSetRecvTimeout(handle, ms) => host_binary(
            context,
            handle,
            io_type,
            ms,
            nat_type,
            ersd::HostPrim::IoSetRecvTimeout,
        ),
        Prim::IoSetSendTimeout(handle, ms) => host_binary(
            context,
            handle,
            io_type,
            ms,
            nat_type,
            ersd::HostPrim::IoSetSendTimeout,
        ),
        Prim::IoSetReuseaddr(handle, on) => host_binary(
            context,
            handle,
            io_type,
            on,
            bln_type,
            ersd::HostPrim::IoSetReuseaddr,
        ),
        Prim::IoPoll(handles, events, timeout) => {
            let arr_io = arr_type(io_type());
            let arr_nat = arr_type(nat_type());
            Ok(host(ersd::HostPrim::IoPoll(
                erase(context, handles, &arr_io)?,
                erase(context, events, &arr_nat)?,
                erase(context, timeout, &int_type())?,
            )))
        }
        Prim::IoClose(handle) => host_unary(context, handle, io_type, ersd::HostPrim::IoClose),
        Prim::IoClockWall => Ok(host(ersd::HostPrim::IoClockWall)),
        Prim::IoClockMono => Ok(host(ersd::HostPrim::IoClockMono)),
        Prim::IoRandom(count) => host_unary(context, count, nat_type, ersd::HostPrim::IoRandom),
        Prim::IoArgs => Ok(host(ersd::HostPrim::IoArgs)),
        Prim::IoEnv(name) => host_unary(context, name, bin_type, ersd::HostPrim::IoEnv),
        // The polymorphic result type is type-only; only the code survives.
        Prim::IoExit(_, code) => host_unary(context, code, nat_type, ersd::HostPrim::IoExit),
        Prim::CellType(_) => Ok(ersd::Subterm::Erased.into()),
        Prim::Cell(type_, init) => Ok(cell(ersd::CellPrim::New(erase(context, init, type_)?))),
        Prim::CellSet(type_, c, v) => {
            let cell_type: Term = Subterm::Prim(Prim::CellType(type_.clone())).into();
            Ok(cell(ersd::CellPrim::Set(
                erase(context, c, &cell_type)?,
                erase(context, v, type_)?,
            )))
        }
        Prim::CellGet(type_, c) => {
            let cell_type: Term = Subterm::Prim(Prim::CellType(type_.clone())).into();
            Ok(cell(ersd::CellPrim::Get(erase(context, c, &cell_type)?)))
        }
    }
}
