//! Typing rules for primitive operations.
//!
//! One rule per operation: what its operands must be, and what it produces. Nothing here is inferred or negotiated — a primitive's signature is fixed by the language, so this module is a table, and the table is the specification.
//!
//! The scalar families are monomorphic and read directly. The container operations carry their element type as an operand, which is what lets them be typed without inventing anything: `Lst/get` is `(T : Type, Lst(T), Nat) -> T`, with `T` present in the term. The one operation that does *not* carry its type is a bare list literal, and the rule for it is the only place here that has to look at an operand to decide a type.

use {
    super::{check, infer},
    crate::{Kernel, KernelError, sort_of_prim},
    curios_base::Grain,
    curios_core::{Prim, Subterm, Term, wire_term},
};

fn bool_type() -> Term {
    Term::prim(Prim::BoolType)
}

fn nat_type() -> Term {
    Term::prim(Prim::NatType)
}

fn byte_type() -> Term {
    Term::prim(Prim::ByteType)
}

fn int_type() -> Term {
    Term::prim(Prim::IntType)
}

fn flt_type() -> Term {
    Term::prim(Prim::FltType)
}

fn handle_type() -> Term {
    Term::prim(Prim::HandleType)
}

fn bin_type(grain: Grain) -> Term {
    Term::prim(Prim::BinType(grain))
}

fn lst_type(element: Term) -> Term {
    Term::prim(Prim::LstType(element))
}

fn cell_type(element: Term) -> Term {
    Term::prim(Prim::CellType(element))
}

fn io_type(result: Term) -> Term {
    Term::prim(Prim::IoType(result))
}

/// The type of `prim`, having checked every operand against the type this operation demands of it.
pub(super) fn infer_prim(kernel: &mut Kernel, prim: &Prim) -> Result<Term, KernelError> {
    // A grain says what a `Bin` is a sequence *of*: bytes at `X`, bits at `B`. Every `Bin` operation is the same rule at two different element types.
    let grain_element = |grain: Grain| match grain {
        Grain::X => byte_type(),
        Grain::B => bool_type(),
    };

    match prim {
        // Type formers are types, and every closed one is small.
        Prim::BoolType
        | Prim::NatType
        | Prim::ByteType
        | Prim::IntType
        | Prim::FltType
        | Prim::BinType(_)
        | Prim::HandleType => Ok(Term::type_ground()),

        // A parameterized former's sort is whatever `Sort::of` computes for it, asked here rather than restated: the element's own sort is *not* the answer, because a list or a cell of proofs has a length or an identity, and a description of proofs has an effect, so none of them is itself a proposition.
        Prim::LstType(element) | Prim::CellType(element) | Prim::IoType(element) => {
            let sort = sort_of_prim(kernel, prim)?;
            let _ = check_is_type(kernel, element)?;

            Ok(sort.term())
        }

        // Literals.
        Prim::Bool(_) => Ok(bool_type()),
        Prim::Nat(_) => Ok(nat_type()),
        Prim::Byte(_) => Ok(byte_type()),
        Prim::Int(_) => Ok(int_type()),
        Prim::Flt(_) => Ok(flt_type()),
        Prim::Bin(grain, _) => Ok(bin_type(*grain)),
        Prim::Handle(_) => Ok(handle_type()),

        // Comparisons: same-typed operands in, a boolean out.
        Prim::BoolEql(l, r) | Prim::BoolNeq(l, r) => binary(kernel, l, r, bool_type(), bool_type()),
        Prim::NatEql(l, r)
        | Prim::NatNeq(l, r)
        | Prim::NatLt(l, r)
        | Prim::NatGt(l, r)
        | Prim::NatLte(l, r)
        | Prim::NatGte(l, r) => binary(kernel, l, r, nat_type(), bool_type()),
        Prim::ByteEql(l, r)
        | Prim::ByteLt(l, r)
        | Prim::ByteLte(l, r)
        | Prim::ByteGt(l, r)
        | Prim::ByteGte(l, r) => binary(kernel, l, r, byte_type(), bool_type()),
        Prim::IntEql(l, r)
        | Prim::IntNeq(l, r)
        | Prim::IntLt(l, r)
        | Prim::IntGt(l, r)
        | Prim::IntLte(l, r)
        | Prim::IntGte(l, r) => binary(kernel, l, r, int_type(), bool_type()),
        Prim::FltEql(l, r)
        | Prim::FltNeq(l, r)
        | Prim::FltLt(l, r)
        | Prim::FltGt(l, r)
        | Prim::FltLte(l, r)
        | Prim::FltGte(l, r) => binary(kernel, l, r, flt_type(), bool_type()),
        Prim::HandleEql(l, r) => binary(kernel, l, r, handle_type(), bool_type()),

        // Arithmetic and bitwise: closed on their carrier.
        Prim::BoolAnd(l, r) | Prim::BoolOr(l, r) | Prim::BoolXor(l, r) => {
            binary(kernel, l, r, bool_type(), bool_type())
        }
        Prim::NatAdd(l, r)
        | Prim::NatSub(l, r)
        | Prim::NatMul(l, r)
        | Prim::NatDiv(l, r)
        | Prim::NatRem(l, r)
        | Prim::NatAnd(l, r)
        | Prim::NatOr(l, r)
        | Prim::NatXor(l, r)
        | Prim::NatShl(l, r)
        | Prim::NatShr(l, r)
        | Prim::NatRotl(l, r)
        | Prim::NatRotr(l, r) => binary(kernel, l, r, nat_type(), nat_type()),
        Prim::IntAdd(l, r)
        | Prim::IntSub(l, r)
        | Prim::IntMul(l, r)
        | Prim::IntDiv(l, r)
        | Prim::IntRem(l, r)
        | Prim::IntAnd(l, r)
        | Prim::IntOr(l, r)
        | Prim::IntXor(l, r)
        | Prim::IntShl(l, r)
        | Prim::IntShr(l, r)
        | Prim::IntRotl(l, r)
        | Prim::IntRotr(l, r) => binary(kernel, l, r, int_type(), int_type()),
        Prim::FltAdd(l, r)
        | Prim::FltSub(l, r)
        | Prim::FltMul(l, r)
        | Prim::FltDiv(l, r)
        | Prim::FltRem(l, r)
        | Prim::FltMin(l, r)
        | Prim::FltMax(l, r)
        | Prim::FltCopysign(l, r) => binary(kernel, l, r, flt_type(), flt_type()),

        Prim::NatClz(i) | Prim::NatCtz(i) | Prim::NatPopcnt(i) => {
            unary(kernel, i, nat_type(), nat_type())
        }
        Prim::IntClz(i) | Prim::IntCtz(i) | Prim::IntPopcnt(i) => {
            unary(kernel, i, int_type(), int_type())
        }
        Prim::FltNeg(i)
        | Prim::FltAbs(i)
        | Prim::FltSqrt(i)
        | Prim::FltFloor(i)
        | Prim::FltCeil(i)
        | Prim::FltTrunc(i)
        | Prim::FltNearest(i) => unary(kernel, i, flt_type(), flt_type()),

        // Conversions.
        Prim::ByteToNat(i) => unary(kernel, i, byte_type(), nat_type()),
        Prim::NatToByte(i) => unary(kernel, i, nat_type(), byte_type()),
        Prim::NatToInt(i) => unary(kernel, i, nat_type(), int_type()),
        Prim::NatToFlt(i) => unary(kernel, i, nat_type(), flt_type()),
        Prim::IntToNat(i) => unary(kernel, i, int_type(), nat_type()),
        Prim::IntToFlt(i) => unary(kernel, i, int_type(), flt_type()),
        Prim::FltToNat(i) => unary(kernel, i, flt_type(), nat_type()),
        Prim::FltToInt(i) => unary(kernel, i, flt_type(), int_type()),
        // The IEEE-754 bytes of a float, and its inverse.
        Prim::FltToLeBytes(i) => unary(kernel, i, flt_type(), bin_type(Grain::X)),
        Prim::FltOfLeBytes(i) => unary(kernel, i, bin_type(Grain::X), flt_type()),

        // `Bin`: a sequence of bytes or of bits, depending on the grain.
        Prim::BinLen(grain, bin) => unary(kernel, bin, bin_type(*grain), nat_type()),
        Prim::BinEql(grain, l, r) => binary(kernel, l, r, bin_type(*grain), bool_type()),
        Prim::BinGet(grain, bin, index) => {
            check(kernel, bin, &bin_type(*grain))?;
            check(kernel, index, &nat_type())?;

            Ok(grain_element(*grain))
        }
        Prim::BinSlice(grain, bin, start, end) => {
            check(kernel, bin, &bin_type(*grain))?;
            check(kernel, start, &nat_type())?;
            check(kernel, end, &nat_type())?;

            Ok(bin_type(*grain))
        }
        Prim::BinAppend(grain, bin, element) => {
            check(kernel, bin, &bin_type(*grain))?;
            check(kernel, element, &grain_element(*grain))?;

            Ok(bin_type(*grain))
        }
        Prim::BinConcat(grain, operands) => {
            for operand in operands {
                check(kernel, operand, &bin_type(*grain))?;
            }

            Ok(bin_type(*grain))
        }

        // A list literal carries its element type like every other `Lst` operation — `[]` included, which is the case that used to be refused for having no element to read a type from.
        Prim::Lst(element, elements) => {
            let element = check_is_type(kernel, element)?;
            for entry in elements {
                check(kernel, entry, &element)?;
            }

            Ok(lst_type(element))
        }
        Prim::LstLen(element, list) => {
            let element = check_is_type(kernel, element)?;
            check(kernel, list, &lst_type(element))?;

            Ok(nat_type())
        }
        Prim::LstGet(element, list, index) => {
            let element = check_is_type(kernel, element)?;
            check(kernel, list, &lst_type(element.clone()))?;
            check(kernel, index, &nat_type())?;

            Ok(element)
        }
        Prim::LstSlice(element, list, start, end) => {
            let element = check_is_type(kernel, element)?;
            check(kernel, list, &lst_type(element.clone()))?;
            check(kernel, start, &nat_type())?;
            check(kernel, end, &nat_type())?;

            Ok(lst_type(element))
        }
        Prim::LstAppend(element, list, value) => {
            let element = check_is_type(kernel, element)?;
            check(kernel, list, &lst_type(element.clone()))?;
            check(kernel, value, &element)?;

            Ok(lst_type(element))
        }
        Prim::LstConcat(element, operands) => {
            let element = check_is_type(kernel, element)?;
            for operand in operands {
                check(kernel, operand, &lst_type(element.clone()))?;
            }

            Ok(lst_type(element))
        }
        Prim::LstMap(from, to, list, function) => {
            let from = check_is_type(kernel, from)?;
            let to = check_is_type(kernel, to)?;
            check(kernel, list, &lst_type(from.clone()))?;

            let binder = kernel.fresh(Some("x"));
            check(
                kernel,
                function,
                &Term::func_type([(binder, from)], to.clone()),
            )?;

            Ok(lst_type(to))
        }

        // A mutable cell. Its identity is what makes a `Cell` of proofs relevant — see `Sort::of`.
        //
        // All three operations are host effects and so describe rather than do. `CellGet` returning `Io(T)` rather than `T` is what makes `match Cell/get(c)` ill-typed, which is the whole of what the purity analysis used to decide by walking the scrutinee.
        Prim::Cell(element, initial) => {
            let element = check_is_type(kernel, element)?;
            check(kernel, initial, &element)?;

            Ok(io_type(cell_type(element)))
        }
        Prim::CellGet(element, cell) => {
            let element = check_is_type(kernel, element)?;
            check(kernel, cell, &cell_type(element.clone()))?;

            Ok(io_type(element))
        }
        Prim::CellSet(element, cell, value) => {
            let element = check_is_type(kernel, element)?;
            check(kernel, cell, &cell_type(element.clone()))?;
            check(kernel, value, &element)?;

            Ok(io_type(Term::tuple_type_unit()))
        }

        // `exit` ends the process, and its result is the unit type rather than whatever the caller demanded.
        //
        // That is the whole of the rule, and it is why there is no side condition here. A term that never returns is unsound exactly when it inhabits a type nothing total inhabits — the forgery is the problem, not the non-return — and restricting *which* type it may be given cannot fix that, because any `Type`-sorted empty inductive eliminates into `Prop` unguarded. Fixing the result at `{}`, which `()` already inhabits, leaves nothing to forge.
        Prim::ProcExit(code) => {
            check(kernel, code, &nat_type())?;

            Ok(io_type(Term::tuple_type_unit()))
        }

        // A host call described by its ABI row: each operand checks against its wire type, and the result shape — unit, a bare value, or a named record — is read off the same signature.
        Prim::Foreign(function, args) => {
            let signature = &function.signature;

            if args.len() != signature.params.len() {
                return Err(KernelError::Arity {
                    expected: signature.params.len(),
                    actual: args.len(),
                });
            }

            let params = signature
                .params
                .iter()
                .map(|(_, wire)| wire_term(wire))
                .collect::<Vec<_>>();
            for (argument, wire) in args.iter().zip(&params) {
                check(kernel, argument, wire)?;
            }

            let results = signature
                .results
                .iter()
                .map(|(label, wire)| (label.clone(), wire_term(wire)))
                .collect::<Vec<_>>();

            Ok(io_type(match results.as_slice() {
                [] => Term::tuple_type_unit(),
                [(_, result)] => result.clone(),
                many => Term::tuple_type(
                    many.iter()
                        .map(|(label, result)| (kernel.fresh(Some(label)), result.clone()))
                        .collect::<Vec<_>>(),
                ),
            }))
        }

        // The two constructors of the opaque effect carrier. There is no third: nothing here or anywhere lowers an `Io(T)` to its `T`, which is what makes every term of non-`Io` type pure by typing.
        Prim::IoPure(result, value) => {
            let result = check_is_type(kernel, result)?;
            check(kernel, value, &result)?;

            Ok(io_type(result))
        }
        Prim::IoBind(from, to, action, continuation) => {
            let from = check_is_type(kernel, from)?;
            let to = check_is_type(kernel, to)?;
            check(kernel, action, &io_type(from.clone()))?;

            let binder = kernel.fresh(Some("x"));
            check(
                kernel,
                continuation,
                &Term::func_type([(binder, from)], io_type(to.clone())),
            )?;

            Ok(io_type(to))
        }
    }
}

/// Check that `term` is a type, and hand it back. A primitive that carries its element type carries a *type*, and taking that on trust is how a container of nonsense would be admitted.
fn check_is_type(kernel: &mut Kernel, term: &Term) -> Result<Term, KernelError> {
    let inferred = infer(kernel, term)?;

    match &*kernel_reduce(kernel, &inferred)? {
        Subterm::Type(_) | Subterm::Prop => Ok(term.clone()),
        _ => Err(KernelError::NotASort(inferred)),
    }
}

fn kernel_reduce(kernel: &mut Kernel, term: &Term) -> Result<Term, KernelError> {
    use curios_core::Reducer;

    Ok(kernel.reduce_forced(term.clone())?)
}

fn unary(
    kernel: &mut Kernel,
    operand: &Term,
    domain: Term,
    result: Term,
) -> Result<Term, KernelError> {
    check(kernel, operand, &domain)?;

    Ok(result)
}

fn binary(
    kernel: &mut Kernel,
    left: &Term,
    right: &Term,
    domain: Term,
    result: Term,
) -> Result<Term, KernelError> {
    check(kernel, left, &domain)?;
    check(kernel, right, &domain)?;

    Ok(result)
}
