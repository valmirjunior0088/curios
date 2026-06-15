use super::{Context, Error, Mode, Prim, Subterm, Term, elaborate, expect, reduce_with};

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

    Ok(match prim {
        Prim::BlnType => (prim.clone(), Term::type_()),
        Prim::Bln(_) => (prim.clone(), bln_type),
        Prim::NatType => (prim.clone(), Term::type_()),
        Prim::Nat(_) => (prim.clone(), nat_type),
        Prim::NatEql(left, right)
        | Prim::NatNeq(left, right)
        | Prim::NatLt(left, right)
        | Prim::NatGt(left, right)
        | Prim::NatLte(left, right)
        | Prim::NatGte(left, right) => {
            let left = elaborate(context, left, Mode::Check(nat_type.clone()))?.0;
            let right = elaborate(context, right, Mode::Check(nat_type))?.0;
            let prim = match prim {
                Prim::NatEql(..) => Prim::NatEql(left, right),
                Prim::NatNeq(..) => Prim::NatNeq(left, right),
                Prim::NatLt(..) => Prim::NatLt(left, right),
                Prim::NatGt(..) => Prim::NatGt(left, right),
                Prim::NatLte(..) => Prim::NatLte(left, right),
                Prim::NatGte(..) => Prim::NatGte(left, right),
                _ => unreachable!(),
            };
            (prim, bln_type)
        }
        Prim::NatAdd(left, right)
        | Prim::NatSub(left, right)
        | Prim::NatMul(left, right)
        | Prim::NatDiv(left, right)
        | Prim::NatRem(left, right) => {
            let left = elaborate(context, left, Mode::Check(nat_type.clone()))?.0;
            let right = elaborate(context, right, Mode::Check(nat_type.clone()))?.0;
            let prim = match prim {
                Prim::NatAdd(..) => Prim::NatAdd(left, right),
                Prim::NatSub(..) => Prim::NatSub(left, right),
                Prim::NatMul(..) => Prim::NatMul(left, right),
                Prim::NatDiv(..) => Prim::NatDiv(left, right),
                Prim::NatRem(..) => Prim::NatRem(left, right),
                _ => unreachable!(),
            };
            (prim, nat_type)
        }
        Prim::IntType => (prim.clone(), Term::type_()),
        Prim::Int(_) => (prim.clone(), int_type),
        Prim::IntEql(left, right)
        | Prim::IntNeq(left, right)
        | Prim::IntLt(left, right)
        | Prim::IntGt(left, right)
        | Prim::IntLte(left, right)
        | Prim::IntGte(left, right) => {
            let left = elaborate(context, left, Mode::Check(int_type.clone()))?.0;
            let right = elaborate(context, right, Mode::Check(int_type))?.0;
            let prim = match prim {
                Prim::IntEql(..) => Prim::IntEql(left, right),
                Prim::IntNeq(..) => Prim::IntNeq(left, right),
                Prim::IntLt(..) => Prim::IntLt(left, right),
                Prim::IntGt(..) => Prim::IntGt(left, right),
                Prim::IntLte(..) => Prim::IntLte(left, right),
                Prim::IntGte(..) => Prim::IntGte(left, right),
                _ => unreachable!(),
            };
            (prim, bln_type)
        }
        Prim::IntAdd(left, right)
        | Prim::IntSub(left, right)
        | Prim::IntMul(left, right)
        | Prim::IntDiv(left, right)
        | Prim::IntRem(left, right) => {
            let left = elaborate(context, left, Mode::Check(int_type.clone()))?.0;
            let right = elaborate(context, right, Mode::Check(int_type.clone()))?.0;
            let prim = match prim {
                Prim::IntAdd(..) => Prim::IntAdd(left, right),
                Prim::IntSub(..) => Prim::IntSub(left, right),
                Prim::IntMul(..) => Prim::IntMul(left, right),
                Prim::IntDiv(..) => Prim::IntDiv(left, right),
                Prim::IntRem(..) => Prim::IntRem(left, right),
                _ => unreachable!(),
            };
            (prim, int_type)
        }
        Prim::FltType => (prim.clone(), Term::type_()),
        Prim::Flt(_) => (prim.clone(), flt_type),
        Prim::FltAdd(left, right)
        | Prim::FltSub(left, right)
        | Prim::FltMul(left, right)
        | Prim::FltDiv(left, right)
        | Prim::FltMin(left, right)
        | Prim::FltMax(left, right) => {
            let left = elaborate(context, left, Mode::Check(flt_type.clone()))?.0;
            let right = elaborate(context, right, Mode::Check(flt_type.clone()))?.0;
            let prim = match prim {
                Prim::FltAdd(..) => Prim::FltAdd(left, right),
                Prim::FltSub(..) => Prim::FltSub(left, right),
                Prim::FltMul(..) => Prim::FltMul(left, right),
                Prim::FltDiv(..) => Prim::FltDiv(left, right),
                Prim::FltMin(..) => Prim::FltMin(left, right),
                Prim::FltMax(..) => Prim::FltMax(left, right),
                _ => unreachable!(),
            };
            (prim, flt_type)
        }
        Prim::FltNeg(inner)
        | Prim::FltAbs(inner)
        | Prim::FltSqrt(inner)
        | Prim::FltFloor(inner)
        | Prim::FltCeil(inner)
        | Prim::FltTrunc(inner)
        | Prim::FltNearest(inner) => {
            let inner = elaborate(context, inner, Mode::Check(flt_type.clone()))?.0;
            let prim = match prim {
                Prim::FltNeg(..) => Prim::FltNeg(inner),
                Prim::FltAbs(..) => Prim::FltAbs(inner),
                Prim::FltSqrt(..) => Prim::FltSqrt(inner),
                Prim::FltFloor(..) => Prim::FltFloor(inner),
                Prim::FltCeil(..) => Prim::FltCeil(inner),
                Prim::FltTrunc(..) => Prim::FltTrunc(inner),
                Prim::FltNearest(..) => Prim::FltNearest(inner),
                _ => unreachable!(),
            };
            (prim, flt_type)
        }
        Prim::FltEql(left, right)
        | Prim::FltNeq(left, right)
        | Prim::FltLt(left, right)
        | Prim::FltGt(left, right)
        | Prim::FltLte(left, right)
        | Prim::FltGte(left, right) => {
            let left = elaborate(context, left, Mode::Check(flt_type.clone()))?.0;
            let right = elaborate(context, right, Mode::Check(flt_type))?.0;
            let prim = match prim {
                Prim::FltEql(..) => Prim::FltEql(left, right),
                Prim::FltNeq(..) => Prim::FltNeq(left, right),
                Prim::FltLt(..) => Prim::FltLt(left, right),
                Prim::FltGt(..) => Prim::FltGt(left, right),
                Prim::FltLte(..) => Prim::FltLte(left, right),
                Prim::FltGte(..) => Prim::FltGte(left, right),
                _ => unreachable!(),
            };
            (prim, bln_type)
        }
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
        Prim::Io(_) => {
            let io_type: Term = Subterm::Prim(Prim::IoType).into();
            (prim.clone(), io_type)
        }
        Prim::IoRead(handle, count) => {
            let io_type: Term = Subterm::Prim(Prim::IoType).into();
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
            let io_type: Term = Subterm::Prim(Prim::IoType).into();
            let handle = elaborate(context, handle, Mode::Check(io_type))?.0;
            let bytes = elaborate(context, bytes, Mode::Check(bin_type))?.0;
            (Prim::IoWrite(handle, bytes), nat_type)
        }
        Prim::IoOpen(path, mode) => {
            let io_type: Term = Subterm::Prim(Prim::IoType).into();
            let path = elaborate(context, path, Mode::Check(bin_type))?.0;
            let mode = elaborate(context, mode, Mode::Check(nat_type.clone()))?.0;
            (
                Prim::IoOpen(path, mode),
                Term::tuple_type([("status", nat_type), ("handle", io_type)]),
            )
        }
        Prim::IoClose(handle) => {
            let io_type: Term = Subterm::Prim(Prim::IoType).into();
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
        Prim::IoArgs => (
            prim.clone(),
            Subterm::Prim(Prim::ArrType(bin_type)).into(),
        ),
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
