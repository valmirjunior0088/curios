use super::{
    Context, Error, ImplicitOrigin, Mode, Prim, Subterm, Term, elaborate, expect, reduce_with,
    wire_term,
};

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

/// Elaborate `bin`, requiring its inferred type to be `Bin`, and return the
/// rebuilt operand. The `Bin`-indexing/slicing prims read their shape off the
/// operand's type, so they infer it rather than checking against a known one.
fn infer_bin(context: &mut Context, bin: &Term) -> Result<Term, Error> {
    let (bin, actual) = elaborate(context, bin, Mode::Infer)?;
    match &*reduce_with(context, &actual)? {
        Subterm::Prim(Prim::BinType) => Ok(bin),
        other => Err(Error::type_mismatch(
            other.clone(),
            Subterm::Prim(Prim::BinType),
        )),
    }
}

fn arr_type(elem: Term) -> Term {
    Subterm::Prim(Prim::ArrType(elem)).into()
}

/// Check every element of an `Arr` literal against an already-determined element
/// type, returning the rebuilt elements. Shared by the two ways the element type
/// is fixed: borrowed from `expected` when checking, or a fresh metavar when
/// inferring (see [`elaborate_prim`] and [`synth_prim`]'s `Arr` arm).
fn check_arr_elems(
    context: &mut Context,
    elems: &[Term],
    elem_type: &Term,
) -> Result<Vec<Term>, Error> {
    let mut elaborated = Vec::with_capacity(elems.len());

    for elem in elems {
        elaborated.push(elaborate(context, elem, Mode::Check(elem_type.clone()))?.0);
    }

    Ok(elaborated)
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
    let io_type: Term = Subterm::Prim(Prim::IoType).into();

    Ok(match prim {
        Prim::BlnType => (prim.clone(), Term::type_()),
        Prim::Bln(_) => (prim.clone(), bln_type),
        Prim::NatType => (prim.clone(), Term::type_()),
        Prim::Nat(_) => (prim.clone(), nat_type),
        Prim::NatEql(l, r) => binary(context, l, r, &nat_type, bln_type.clone(), Prim::NatEql)?,
        Prim::IoEql(l, r) => binary(context, l, r, &io_type, bln_type.clone(), Prim::IoEql)?,
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
        Prim::BlnEql(l, r) => binary(context, l, r, &bln_type, bln_type.clone(), Prim::BlnEql)?,
        Prim::BlnNeq(l, r) => binary(context, l, r, &bln_type, bln_type.clone(), Prim::BlnNeq)?,
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
        Prim::FltRem(l, r) => binary(context, l, r, &flt_type, flt_type.clone(), Prim::FltRem)?,
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
        // `Flt/to_le_bin` exposes the IEEE-754 bytes (`Bin`); `/std/Flt/to_str`
        // renders them to the proof-carrying `/syn/Str` in Curios (Dragon4).
        Prim::FltToLeBin(i) => unary(context, i, &flt_type, bin_type.clone(), Prim::FltToLeBin)?,
        Prim::NatToInt(i) => unary(context, i, &nat_type, int_type.clone(), Prim::NatToInt)?,
        Prim::NatToFlt(i) => unary(context, i, &nat_type, flt_type.clone(), Prim::NatToFlt)?,
        Prim::IntToNat(i) => unary(context, i, &int_type, nat_type.clone(), Prim::IntToNat)?,
        Prim::IntToFlt(i) => unary(context, i, &int_type, flt_type.clone(), Prim::IntToFlt)?,
        Prim::FltToNat(i) => unary(context, i, &flt_type, nat_type.clone(), Prim::FltToNat)?,
        Prim::FltToInt(i) => unary(context, i, &flt_type, int_type.clone(), Prim::FltToInt)?,
        Prim::BinType => (prim.clone(), Term::type_()),
        Prim::Bin(_) => (prim.clone(), bin_type),
        Prim::BinLen(bin) => {
            let bin = infer_bin(context, bin)?;
            (Prim::BinLen(bin), nat_type)
        }
        Prim::BinEql(left, right) => {
            let left = elaborate(context, left, Mode::Check(bin_type.clone()))?.0;
            let right = elaborate(context, right, Mode::Check(bin_type))?.0;
            (Prim::BinEql(left, right), bln_type)
        }
        Prim::BinGet(bin, index) => {
            let bin = infer_bin(context, bin)?;
            let index = elaborate(context, index, Mode::Check(nat_type.clone()))?.0;
            (Prim::BinGet(bin, index), nat_type)
        }
        Prim::BinSlice(bin, start, end) => {
            let bin = infer_bin(context, bin)?;
            let start = elaborate(context, start, Mode::Check(nat_type.clone()))?.0;
            let end = elaborate(context, end, Mode::Check(nat_type))?.0;
            (Prim::BinSlice(bin, start, end), bin_type)
        }
        Prim::BinAppend(bin, byte) => {
            let bin = infer_bin(context, bin)?;
            let byte = elaborate(context, byte, Mode::Check(nat_type))?.0;
            (Prim::BinAppend(bin, byte), bin_type)
        }
        Prim::BinConcat(operands) => {
            let mut elaborated = Vec::with_capacity(operands.len());
            for operand in operands {
                elaborated.push(elaborate(context, operand, Mode::Check(bin_type.clone()))?.0);
            }
            (Prim::BinConcat(elaborated), bin_type)
        }
        Prim::BinFlatten(operand) => {
            let outer_type = arr_type(bin_type.clone());
            let operand = elaborate(context, operand, Mode::Check(outer_type))?.0;
            (Prim::BinFlatten(operand), bin_type)
        }
        Prim::ArrType(elem) => {
            let elem = elaborate(context, elem, Mode::Check(Term::type_()))?.0;
            (Prim::ArrType(elem), Term::type_())
        }
        // Inferring: the element type is unknown, so mint a fresh metavar — the
        // implicit `@T` a `nil`/`cons` constructor would insert — which the elements
        // solve (an empty `[||]` leaves it for a later unification to ground, exactly
        // as the old `Arr/nil()` did). Checking goes through `elaborate_prim`, which
        // borrows the concrete element type from `expected` before reaching here.
        Prim::Arr(elems) => {
            let elem_type = context.fresh_metavar(
                Term::type_(),
                None,
                ImplicitOrigin {
                    func: "Arr".to_string(),
                    binder: "T".to_string(),
                },
            );
            let elaborated = check_arr_elems(context, elems, &elem_type)?;
            (Prim::Arr(elaborated), arr_type(elem_type))
        }
        Prim::ArrLen(type_, list) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type = arr_type(type_.clone());
            let list = elaborate(context, list, Mode::Check(list_type))?.0;
            (Prim::ArrLen(type_, list), nat_type)
        }
        Prim::ArrGet(type_, list, index) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type = arr_type(type_.clone());
            let list = elaborate(context, list, Mode::Check(list_type))?.0;
            let index = elaborate(context, index, Mode::Check(nat_type))?.0;
            let output = type_.clone();
            (Prim::ArrGet(type_, list, index), output)
        }
        Prim::ArrSlice(type_, list, start, end) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type = arr_type(type_.clone());
            let list = elaborate(context, list, Mode::Check(list_type.clone()))?.0;
            let start = elaborate(context, start, Mode::Check(nat_type.clone()))?.0;
            let end = elaborate(context, end, Mode::Check(nat_type))?.0;
            (Prim::ArrSlice(type_, list, start, end), list_type)
        }
        Prim::ArrAppend(type_, list, elem) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type = arr_type(type_.clone());
            let list = elaborate(context, list, Mode::Check(list_type.clone()))?.0;
            let elem = elaborate(context, elem, Mode::Check(type_.clone()))?.0;
            (Prim::ArrAppend(type_, list, elem), list_type)
        }
        Prim::ArrConcat(type_, operands) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type = arr_type(type_.clone());
            let mut elaborated = Vec::with_capacity(operands.len());
            for operand in operands {
                elaborated.push(elaborate(context, operand, Mode::Check(list_type.clone()))?.0);
            }
            (Prim::ArrConcat(type_, elaborated), list_type)
        }
        Prim::ArrFlatten(type_, operand) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let list_type = arr_type(type_.clone());
            let outer_type = arr_type(list_type.clone());
            let operand = elaborate(context, operand, Mode::Check(outer_type))?.0;
            (Prim::ArrFlatten(type_, operand), list_type)
        }
        Prim::ArrMap(a, b, f, arr) => {
            let a = elaborate(context, a, Mode::Check(Term::type_()))?.0;
            let b = elaborate(context, b, Mode::Check(Term::type_()))?.0;
            let f_type = Term::func_type([("x", a.clone())], b.clone());
            let f = elaborate(context, f, Mode::Check(f_type))?.0;
            let arr_a = arr_type(a.clone());
            let arr = elaborate(context, arr, Mode::Check(arr_a))?.0;
            let arr_b = arr_type(b.clone());
            (Prim::ArrMap(a, b, f, arr), arr_b)
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
        Prim::IoLookup(host, port) => {
            let host = elaborate(context, host, Mode::Check(bin_type))?.0;
            let port = elaborate(context, port, Mode::Check(nat_type.clone()))?.0;
            (
                Prim::IoLookup(host, port),
                Term::tuple_type([("status", nat_type), ("handle", io_type)]),
            )
        }
        Prim::IoResolve(handle) => {
            let handle = elaborate(context, handle, Mode::Check(io_type))?.0;
            (
                Prim::IoResolve(handle),
                Term::tuple_type([
                    ("status", nat_type.clone()),
                    ("addresses", arr_type(bin_type)),
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
            let arr_io = arr_type(io_type);
            let arr_nat = arr_type(nat_type);
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
        Prim::IoArgs => (prim.clone(), arr_type(bin_type)),
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
        // `False`). The type argument keeps the kernel from naming `/std/False`.
        Prim::IoExit(type_, code) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let code = elaborate(context, code, Mode::Check(nat_type))?.0;
            (Prim::IoExit(type_.clone(), code), type_)
        }
        // A table-described host call: each operand checks against its wire
        // type, and the result shape (unit, bare value, named record) is read
        // off the signature. The arity is an invariant of construction (the
        // prelude builds the argument list from the same signature).
        Prim::Foreign(function, args) => {
            let signature = function.signature();

            assert_eq!(
                args.len(),
                signature.params.len(),
                "{} operand count does not match its signature",
                function.name()
            );

            let mut elaborated = Vec::with_capacity(args.len());
            for (arg, (_, wire_type)) in args.iter().zip(signature.params) {
                elaborated.push(elaborate(context, arg, Mode::Check(wire_term(*wire_type)))?.0);
            }

            let result = match signature.results {
                [] => Term::tuple_type_unit(),
                [(_, wire_type)] => wire_term(*wire_type),
                results => Term::tuple_type(
                    results
                        .iter()
                        .map(|(label, wire_type)| (*label, wire_term(*wire_type))),
                ),
            };

            (Prim::Foreign(*function, elaborated), result)
        }
        Prim::CellType(elem) => {
            let elem = elaborate(context, elem, Mode::Check(Term::type_()))?.0;
            (Prim::CellType(elem), Term::type_())
        }
        Prim::Cell(type_, init) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let init = elaborate(context, init, Mode::Check(type_.clone()))?.0;
            let cell_type: Term = Subterm::Prim(Prim::CellType(type_.clone())).into();
            (Prim::Cell(type_, init), cell_type)
        }
        Prim::CellSet(type_, cell, value) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let cell_type: Term = Subterm::Prim(Prim::CellType(type_.clone())).into();
            let cell = elaborate(context, cell, Mode::Check(cell_type))?.0;
            let value = elaborate(context, value, Mode::Check(type_.clone()))?.0;
            (Prim::CellSet(type_, cell, value), Term::tuple_type_unit())
        }
        Prim::CellGet(type_, cell) => {
            let type_ = elaborate(context, type_, Mode::Check(Term::type_()))?.0;
            let cell_type: Term = Subterm::Prim(Prim::CellType(type_.clone())).into();
            let cell = elaborate(context, cell, Mode::Check(cell_type))?.0;
            let output = type_.clone();
            (Prim::CellGet(type_, cell), output)
        }
    })
}

pub fn elaborate_prim(
    context: &mut Context,
    term: &Term,
    prim: &Prim,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // `Arr` is bidirectional. Checking, it borrows the concrete element type from
    // `expected` — definitional, so each element is checked against the known type
    // (better errors, and numeric element literals pick the right numeric type).
    // Inferring, it falls through to `synth_prim`, which mints a fresh element-type
    // metavar instead — mirroring how a `Lst` literal synthesizes its element type.
    if let (Prim::Arr(elems), Mode::Check(expected)) = (prim, &mode) {
        let elem_type = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
            Subterm::Prim(Prim::ArrType(elem_type)) => elem_type,
            other => return Err(Error::type_mismatch(other, expected.clone())),
        };

        let elaborated = check_arr_elems(context, elems, &elem_type)?;

        return Ok((Term::prim(Prim::Arr(elaborated)), expected.clone()));
    }

    let (prim, type_) = synth_prim(context, prim)?;

    if let Mode::Check(expected) = &mode {
        expect(context, term, &type_, expected)?;
    }

    Ok((Term::prim(prim), type_))
}
