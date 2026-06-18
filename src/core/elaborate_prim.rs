use super::{Context, Error, Mode, Prim, Subterm, Term, elaborate, expect, reduce_with};

/// Elaborate both operands of a homogeneous binary primitive at `operand`, then
/// rebuild the variant through its constructor (`build`) and pair it with
/// `result`. Lets each arm name itself once instead of destructuring an
/// OR-pattern and re-matching just to reattach the elaborated operands.
fn binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    operand: &Term,
    result: Term,
    build: fn(Term, Term) -> Prim,
) -> Result<(Prim, Term), Error> {
    let left = elaborate(context, left, Mode::Check(operand.clone()))?.0;
    let right = elaborate(context, right, Mode::Check(operand.clone()))?.0;
    Ok((build(left, right), result))
}

/// The unary counterpart of [`binary`], for the float-unary primitives.
fn unary(
    context: &mut Context,
    inner: &Term,
    operand: &Term,
    result: Term,
    build: fn(Term) -> Prim,
) -> Result<(Prim, Term), Error> {
    let inner = elaborate(context, inner, Mode::Check(operand.clone()))?.0;
    Ok((build(inner), result))
}

/// Synthesize a primitive's type, checking *and rebuilding* its operands. Mirrors
/// the old `infer_prim`, but every operand obligation goes through
/// `elaborate(Check)` and the elaborated operand is kept, so the returned `Prim`
/// is the authoritative (rebuilt) one that flows on to `zonk`/`erase` (§9).
fn synth_prim(context: &mut Context, prim: &Prim) -> Result<(Prim, Term), Error> {
    let nat_type: Term = Subterm::Prim(Prim::NatType).into();
    let int_type: Term = Subterm::Prim(Prim::IntType).into();
    let flt_type: Term = Subterm::Prim(Prim::FltType).into();
    let bln_type: Term = Subterm::Prim(Prim::BlnType).into();
    let bin_type: Term = Subterm::Prim(Prim::BinType).into();
    let str_type: Term = Subterm::Prim(Prim::StrType).into();
    let io_type: Term = Subterm::Prim(Prim::IoType).into();

    Ok(match prim {
        Prim::BlnType => (prim.clone(), Term::type_()),
        Prim::Bln(_) => (prim.clone(), bln_type),
        Prim::NatType => (prim.clone(), Term::type_()),
        Prim::Nat(_) => (prim.clone(), nat_type),
        Prim::NatEql(l, r) => binary(context, l, r, &nat_type, bln_type.clone(), Prim::NatEql)?,
        Prim::NatNeq(l, r) => binary(context, l, r, &nat_type, bln_type.clone(), Prim::NatNeq)?,
        Prim::NatLt(l, r) => binary(context, l, r, &nat_type, bln_type.clone(), Prim::NatLt)?,
        Prim::NatGt(l, r) => binary(context, l, r, &nat_type, bln_type.clone(), Prim::NatGt)?,
        Prim::NatLte(l, r) => binary(context, l, r, &nat_type, bln_type.clone(), Prim::NatLte)?,
        Prim::NatGte(l, r) => binary(context, l, r, &nat_type, bln_type.clone(), Prim::NatGte)?,
        Prim::NatAdd(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatAdd)?,
        Prim::NatSub(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatSub)?,
        Prim::NatMul(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatMul)?,
        Prim::NatDiv(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatDiv)?,
        Prim::NatRem(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatRem)?,
        Prim::NatAnd(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatAnd)?,
        Prim::NatOr(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatOr)?,
        Prim::NatXor(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatXor)?,
        Prim::NatShl(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatShl)?,
        Prim::NatShr(l, r) => binary(context, l, r, &nat_type, nat_type.clone(), Prim::NatShr)?,
        Prim::BlnAnd(l, r) => binary(context, l, r, &bln_type, bln_type.clone(), Prim::BlnAnd)?,
        Prim::BlnOr(l, r) => binary(context, l, r, &bln_type, bln_type.clone(), Prim::BlnOr)?,
        Prim::BlnXor(l, r) => binary(context, l, r, &bln_type, bln_type.clone(), Prim::BlnXor)?,
        Prim::IntType => (prim.clone(), Term::type_()),
        Prim::Int(_) => (prim.clone(), int_type),
        Prim::IntEql(l, r) => binary(context, l, r, &int_type, bln_type.clone(), Prim::IntEql)?,
        Prim::IntNeq(l, r) => binary(context, l, r, &int_type, bln_type.clone(), Prim::IntNeq)?,
        Prim::IntLt(l, r) => binary(context, l, r, &int_type, bln_type.clone(), Prim::IntLt)?,
        Prim::IntGt(l, r) => binary(context, l, r, &int_type, bln_type.clone(), Prim::IntGt)?,
        Prim::IntLte(l, r) => binary(context, l, r, &int_type, bln_type.clone(), Prim::IntLte)?,
        Prim::IntGte(l, r) => binary(context, l, r, &int_type, bln_type.clone(), Prim::IntGte)?,
        Prim::IntAdd(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntAdd)?,
        Prim::IntSub(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntSub)?,
        Prim::IntMul(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntMul)?,
        Prim::IntDiv(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntDiv)?,
        Prim::IntRem(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntRem)?,
        Prim::IntAnd(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntAnd)?,
        Prim::IntOr(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntOr)?,
        Prim::IntXor(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntXor)?,
        Prim::IntShl(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntShl)?,
        Prim::IntShr(l, r) => binary(context, l, r, &int_type, int_type.clone(), Prim::IntShr)?,
        Prim::FltType => (prim.clone(), Term::type_()),
        Prim::Flt(_) => (prim.clone(), flt_type),
        Prim::FltAdd(l, r) => binary(context, l, r, &flt_type, flt_type.clone(), Prim::FltAdd)?,
        Prim::FltSub(l, r) => binary(context, l, r, &flt_type, flt_type.clone(), Prim::FltSub)?,
        Prim::FltMul(l, r) => binary(context, l, r, &flt_type, flt_type.clone(), Prim::FltMul)?,
        Prim::FltDiv(l, r) => binary(context, l, r, &flt_type, flt_type.clone(), Prim::FltDiv)?,
        Prim::FltMin(l, r) => binary(context, l, r, &flt_type, flt_type.clone(), Prim::FltMin)?,
        Prim::FltMax(l, r) => binary(context, l, r, &flt_type, flt_type.clone(), Prim::FltMax)?,
        Prim::FltNeg(i) => unary(context, i, &flt_type, flt_type.clone(), Prim::FltNeg)?,
        Prim::FltAbs(i) => unary(context, i, &flt_type, flt_type.clone(), Prim::FltAbs)?,
        Prim::FltSqrt(i) => unary(context, i, &flt_type, flt_type.clone(), Prim::FltSqrt)?,
        Prim::FltFloor(i) => unary(context, i, &flt_type, flt_type.clone(), Prim::FltFloor)?,
        Prim::FltCeil(i) => unary(context, i, &flt_type, flt_type.clone(), Prim::FltCeil)?,
        Prim::FltTrunc(i) => unary(context, i, &flt_type, flt_type.clone(), Prim::FltTrunc)?,
        Prim::FltNearest(i) => unary(context, i, &flt_type, flt_type.clone(), Prim::FltNearest)?,
        Prim::FltEql(l, r) => binary(context, l, r, &flt_type, bln_type.clone(), Prim::FltEql)?,
        Prim::FltNeq(l, r) => binary(context, l, r, &flt_type, bln_type.clone(), Prim::FltNeq)?,
        Prim::FltLt(l, r) => binary(context, l, r, &flt_type, bln_type.clone(), Prim::FltLt)?,
        Prim::FltGt(l, r) => binary(context, l, r, &flt_type, bln_type.clone(), Prim::FltGt)?,
        Prim::FltLte(l, r) => binary(context, l, r, &flt_type, bln_type.clone(), Prim::FltLte)?,
        Prim::FltGte(l, r) => binary(context, l, r, &flt_type, bln_type.clone(), Prim::FltGte)?,
        Prim::NatToStr(inner) => {
            let inner = elaborate(context, inner, Mode::Check(nat_type))?.0;
            (Prim::NatToStr(inner), str_type)
        }
        Prim::IntToStr(inner) => {
            let inner = elaborate(context, inner, Mode::Check(int_type))?.0;
            (Prim::IntToStr(inner), str_type.clone())
        }
        Prim::FltToStr(inner) => {
            let inner = elaborate(context, inner, Mode::Check(flt_type))?.0;
            (Prim::FltToStr(inner), str_type.clone())
        }
        Prim::FltToLeBin(inner) => {
            let inner = elaborate(context, inner, Mode::Check(flt_type))?.0;
            (Prim::FltToLeBin(inner), bin_type)
        }
        Prim::NatToInt(inner) => {
            let inner = elaborate(context, inner, Mode::Check(nat_type))?.0;
            (Prim::NatToInt(inner), int_type)
        }
        Prim::NatToFlt(inner) => {
            let inner = elaborate(context, inner, Mode::Check(nat_type))?.0;
            (Prim::NatToFlt(inner), flt_type)
        }
        Prim::IntToNat(inner) => {
            let inner = elaborate(context, inner, Mode::Check(int_type))?.0;
            (Prim::IntToNat(inner), nat_type)
        }
        Prim::IntToFlt(inner) => {
            let inner = elaborate(context, inner, Mode::Check(int_type))?.0;
            (Prim::IntToFlt(inner), flt_type)
        }
        Prim::FltToNat(inner) => {
            let inner = elaborate(context, inner, Mode::Check(flt_type))?.0;
            (Prim::FltToNat(inner), nat_type)
        }
        Prim::FltToInt(inner) => {
            let inner = elaborate(context, inner, Mode::Check(flt_type))?.0;
            (Prim::FltToInt(inner), int_type)
        }
        Prim::BinType => (prim.clone(), Term::type_()),
        Prim::Bin(_) => (prim.clone(), bin_type),
        Prim::BinLen(bin) => {
            let (bin, bin_actual) = elaborate(context, bin, Mode::Infer)?;
            let bin_actual = reduce_with(context, &bin_actual)?;
            match &*bin_actual {
                Subterm::Prim(Prim::BinType) => (Prim::BinLen(bin), nat_type),
                other => {
                    return Err(Error::type_mismatch(
                        other.clone(),
                        Subterm::Prim(Prim::BinType),
                    ));
                }
            }
        }
        Prim::BinEql(left, right) => {
            let left = elaborate(context, left, Mode::Check(bin_type.clone()))?.0;
            let right = elaborate(context, right, Mode::Check(bin_type))?.0;
            (Prim::BinEql(left, right), bln_type)
        }
        Prim::BinGet(bin, index) => {
            let (bin, bin_actual) = elaborate(context, bin, Mode::Infer)?;
            let bin_actual = reduce_with(context, &bin_actual)?;
            match &*bin_actual {
                Subterm::Prim(Prim::BinType) => {
                    let index = elaborate(context, index, Mode::Check(nat_type.clone()))?.0;
                    (Prim::BinGet(bin, index), nat_type)
                }
                other => {
                    return Err(Error::type_mismatch(
                        other.clone(),
                        Subterm::Prim(Prim::BinType),
                    ));
                }
            }
        }
        Prim::BinSlice(bin, start, end) => {
            let (bin, bin_actual) = elaborate(context, bin, Mode::Infer)?;
            let bin_actual = reduce_with(context, &bin_actual)?;
            match &*bin_actual {
                Subterm::Prim(Prim::BinType) => {
                    let start = elaborate(context, start, Mode::Check(nat_type.clone()))?.0;
                    let end = elaborate(context, end, Mode::Check(nat_type))?.0;
                    (Prim::BinSlice(bin, start, end), bin_type)
                }
                other => {
                    return Err(Error::type_mismatch(
                        other.clone(),
                        Subterm::Prim(Prim::BinType),
                    ));
                }
            }
        }
        Prim::BinAppend(bin, byte) => {
            let (bin, bin_actual) = elaborate(context, bin, Mode::Infer)?;
            let bin_actual = reduce_with(context, &bin_actual)?;
            match &*bin_actual {
                Subterm::Prim(Prim::BinType) => {
                    let byte = elaborate(context, byte, Mode::Check(nat_type))?.0;
                    (Prim::BinAppend(bin, byte), bin_type)
                }
                other => {
                    return Err(Error::type_mismatch(
                        other.clone(),
                        Subterm::Prim(Prim::BinType),
                    ));
                }
            }
        }
        Prim::BinConcat(operands) => {
            let mut elaborated = Vec::with_capacity(operands.len());
            for operand in operands {
                elaborated.push(elaborate(context, operand, Mode::Check(bin_type.clone()))?.0);
            }
            (Prim::BinConcat(elaborated), bin_type)
        }
        Prim::StrType => (prim.clone(), Term::type_()),
        Prim::Str(_) => (prim.clone(), str_type),
        // `Str/to_bin` reveals the underlying UTF-8 carrier; total and safe.
        Prim::StrToBin(str) => {
            let str = elaborate(context, str, Mode::Check(str_type))?.0;
            (Prim::StrToBin(str), bin_type)
        }
        // The trusted `Bin -> Str` coercion — the /sys substrate beneath the
        // checked `/std/Str/of_bin`. Not part of the /std API; its only caller is
        // `of_bin`, which gates it behind an `is_utf8` check.
        Prim::StrOfBin(bin) => {
            let bin = elaborate(context, bin, Mode::Check(bin_type))?.0;
            (Prim::StrOfBin(bin), str_type)
        }
        Prim::ArrType(elem) => {
            let elem = elaborate(context, elem, Mode::Check(Term::type_()))?.0;
            (Prim::ArrType(elem), Term::type_())
        }
        Prim::Arr(_) => return Err(Error::CannotInferLiteral),
        Prim::ArrLen(type_, list) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type: Term = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list = elaborate(context, list, Mode::Check(list_type))?.0;
            (Prim::ArrLen(type_, list), nat_type)
        }
        Prim::ArrGet(type_, list, index) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type: Term = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list = elaborate(context, list, Mode::Check(list_type))?.0;
            let index = elaborate(context, index, Mode::Check(nat_type))?.0;
            let output = type_.clone();
            (Prim::ArrGet(type_, list, index), output)
        }
        Prim::ArrSlice(type_, list, start, end) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type: Term = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list = elaborate(context, list, Mode::Check(list_type.clone()))?.0;
            let start = elaborate(context, start, Mode::Check(nat_type.clone()))?.0;
            let end = elaborate(context, end, Mode::Check(nat_type))?.0;
            (Prim::ArrSlice(type_, list, start, end), list_type)
        }
        Prim::ArrAppend(type_, list, elem) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type: Term = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let list = elaborate(context, list, Mode::Check(list_type.clone()))?.0;
            let elem = elaborate(context, elem, Mode::Check(type_.clone()))?.0;
            (Prim::ArrAppend(type_, list, elem), list_type)
        }
        Prim::ArrConcat(type_, operands) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type: Term = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            let mut elaborated = Vec::with_capacity(operands.len());
            for operand in operands {
                elaborated.push(elaborate(context, operand, Mode::Check(list_type.clone()))?.0);
            }
            (Prim::ArrConcat(type_, elaborated), list_type)
        }
        Prim::IoType => (prim.clone(), Term::type_()),
        Prim::Io(_) => (prim.clone(), io_type),
        Prim::IoRead(handle, count) => {
            let handle = elaborate(context, handle, Mode::Check(io_type))?.0;
            let count = elaborate(context, count, Mode::Check(nat_type.clone()))?.0;
            // Failable host ops report through a status record: 0 ok, 1 eof
            // (bytes empty ⟺ status 1), 2+ error. Labels are load-bearing —
            // callers project `.status`/`.bytes`.
            (
                Prim::IoRead(handle, count),
                Term::tuple_type([("status", nat_type), ("bytes", bin_type)]),
            )
        }
        Prim::IoWrite(handle, bytes) => {
            let handle = elaborate(context, handle, Mode::Check(io_type))?.0;
            let bytes = elaborate(context, bytes, Mode::Check(bin_type.clone()))?.0;
            // Like `IoRead`, write reports through a status record: `status` (0
            // ok, else error) plus `written`, the number of bytes accepted this
            // call, so a partial non-blocking write resends only its tail.
            (
                Prim::IoWrite(handle, bytes),
                Term::tuple_type([("status", nat_type.clone()), ("written", nat_type)]),
            )
        }
        Prim::IoOpen(path, mode) => {
            let path = elaborate(context, path, Mode::Check(bin_type))?.0;
            let mode = elaborate(context, mode, Mode::Check(nat_type.clone()))?.0;
            (
                Prim::IoOpen(path, mode),
                Term::tuple_type([("status", nat_type), ("handle", io_type)]),
            )
        }
        Prim::IoResolve(host, port) => {
            let host = elaborate(context, host, Mode::Check(bin_type.clone()))?.0;
            let port = elaborate(context, port, Mode::Check(nat_type.clone()))?.0;
            (
                Prim::IoResolve(host, port),
                Term::tuple_type([
                    ("status", nat_type.clone()),
                    ("addresses", Subterm::Prim(Prim::ArrType(bin_type)).into()),
                ]),
            )
        }
        Prim::IoSocket(addr) => {
            let addr = elaborate(context, addr, Mode::Check(bin_type))?.0;
            (
                Prim::IoSocket(addr),
                Term::tuple_type([("status", nat_type), ("handle", io_type)]),
            )
        }
        Prim::IoBind(handle, addr) => {
            let handle = elaborate(context, handle, Mode::Check(io_type))?.0;
            let addr = elaborate(context, addr, Mode::Check(bin_type))?.0;
            (Prim::IoBind(handle, addr), nat_type)
        }
        Prim::IoConnect(handle, addr) => {
            let handle = elaborate(context, handle, Mode::Check(io_type))?.0;
            let addr = elaborate(context, addr, Mode::Check(bin_type))?.0;
            (Prim::IoConnect(handle, addr), nat_type)
        }
        Prim::IoListen(handle, backlog) => {
            let handle = elaborate(context, handle, Mode::Check(io_type))?.0;
            let backlog = elaborate(context, backlog, Mode::Check(nat_type.clone()))?.0;
            (Prim::IoListen(handle, backlog), nat_type)
        }
        Prim::IoAccept(handle) => {
            let handle = elaborate(context, handle, Mode::Check(io_type.clone()))?.0;
            (
                Prim::IoAccept(handle),
                Term::tuple_type([("status", nat_type), ("handle", io_type)]),
            )
        }
        Prim::IoStartTls(handle, sni) => {
            let handle = elaborate(context, handle, Mode::Check(io_type))?.0;
            let sni = elaborate(context, sni, Mode::Check(bin_type))?.0;
            (Prim::IoStartTls(handle, sni), nat_type)
        }
        Prim::IoTlsServerConfig(cert, key) => {
            let cert = elaborate(context, cert, Mode::Check(bin_type.clone()))?.0;
            let key = elaborate(context, key, Mode::Check(bin_type))?.0;
            (
                Prim::IoTlsServerConfig(cert, key),
                Term::tuple_type([("status", nat_type), ("handle", io_type)]),
            )
        }
        Prim::IoStartTlsServer(handle, cfg) => {
            let handle = elaborate(context, handle, Mode::Check(io_type.clone()))?.0;
            let cfg = elaborate(context, cfg, Mode::Check(io_type))?.0;
            (Prim::IoStartTlsServer(handle, cfg), nat_type)
        }
        Prim::IoSetNonblocking(handle, on) => {
            let handle = elaborate(context, handle, Mode::Check(io_type))?.0;
            let on = elaborate(context, on, Mode::Check(bln_type))?.0;
            (Prim::IoSetNonblocking(handle, on), nat_type)
        }
        Prim::IoSetRecvTimeout(handle, ms) => {
            let handle = elaborate(context, handle, Mode::Check(io_type))?.0;
            let ms = elaborate(context, ms, Mode::Check(nat_type.clone()))?.0;
            (Prim::IoSetRecvTimeout(handle, ms), nat_type)
        }
        Prim::IoSetSendTimeout(handle, ms) => {
            let handle = elaborate(context, handle, Mode::Check(io_type))?.0;
            let ms = elaborate(context, ms, Mode::Check(nat_type.clone()))?.0;
            (Prim::IoSetSendTimeout(handle, ms), nat_type)
        }
        Prim::IoSetReuseaddr(handle, on) => {
            let handle = elaborate(context, handle, Mode::Check(io_type))?.0;
            let on = elaborate(context, on, Mode::Check(bln_type))?.0;
            (Prim::IoSetReuseaddr(handle, on), nat_type)
        }
        Prim::IoPoll(handles, events, timeout) => {
            // `handles : Arr(Io)`, `events : Arr(Nat)` (parallel interest masks),
            // `timeout : Int` (poll(2) sign convention); result is the parallel
            // `Arr(Nat)` of revents.
            let arr_io: Term = Subterm::Prim(Prim::ArrType(io_type)).into();
            let arr_nat: Term = Subterm::Prim(Prim::ArrType(nat_type)).into();
            let handles = elaborate(context, handles, Mode::Check(arr_io))?.0;
            let events = elaborate(context, events, Mode::Check(arr_nat.clone()))?.0;
            let timeout = elaborate(context, timeout, Mode::Check(int_type))?.0;
            (Prim::IoPoll(handles, events, timeout), arr_nat)
        }
        Prim::IoClose(handle) => {
            let handle = elaborate(context, handle, Mode::Check(io_type))?.0;
            (Prim::IoClose(handle), Term::tuple_type_unit())
        }
        Prim::IoClockWall => (
            prim.clone(),
            Term::tuple_type([
                ("secs_hi", nat_type.clone()),
                ("secs_lo", nat_type.clone()),
                ("nanos", nat_type.clone()),
            ]),
        ),
        Prim::IoClockMono => (
            prim.clone(),
            Term::tuple_type([("secs", nat_type.clone()), ("nanos", nat_type.clone())]),
        ),
        Prim::IoRandom(count) => {
            let count = elaborate(context, count, Mode::Check(nat_type.clone()))?.0;
            (Prim::IoRandom(count), bin_type)
        }
        // argv: an immutable snapshot of the process arguments.
        Prim::IoArgs => (prim.clone(), Subterm::Prim(Prim::ArrType(bin_type)).into()),
        Prim::IoEnv(name) => {
            let name = elaborate(context, name, Mode::Check(bin_type.clone()))?.0;
            // Found/not-found crosses as a status record: 0 ok, 2 not found.
            (
                Prim::IoEnv(name),
                Term::tuple_type([("status", nat_type), ("value", bin_type)]),
            )
        }
        // `(@A : Type) -> Nat -> A`: exit never returns, so the result type is
        // whatever the caller demands (`/std/Proc/exit` instantiates it at
        // `Void`). The type argument keeps the kernel from naming `/std/Void`.
        Prim::IoExit(type_, code) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let code = elaborate(context, code, Mode::Check(nat_type))?.0;
            (Prim::IoExit(type_.clone(), code), type_)
        }
    })
}

pub fn elaborate_prim(
    context: &mut Context,
    term: &Term,
    prim: &Prim,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // `Arr` is the one naturally-checked primitive: it borrows its element type
    // from `expected` and cannot synthesize.
    if let Prim::Arr(elems) = prim {
        let Mode::Check(expected) = &mode else {
            return Err(Error::CannotInferLiteral);
        };

        let elem_type = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
            Subterm::Prim(Prim::ArrType(elem_type)) => elem_type,
            other => return Err(Error::type_mismatch(other, expected.clone())),
        };

        let mut elaborated = Vec::with_capacity(elems.len());

        for elem in elems {
            elaborated.push(elaborate(context, elem, Mode::Check(elem_type.clone()))?.0);
        }

        return Ok((Term::prim(Prim::Arr(elaborated)), expected.clone()));
    }

    let (prim, type_) = synth_prim(context, prim)?;

    if let Mode::Check(expected) = &mode {
        expect(context, term, &type_, expected)?;
    }

    Ok((Term::prim(prim), type_))
}
