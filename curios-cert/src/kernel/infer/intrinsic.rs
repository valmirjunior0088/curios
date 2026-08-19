//! Typing rules for intrinsic operations.
//!
//! One rule per operation: what its operands must be, and what it produces. Nothing here is inferred or negotiated — an intrinsic's signature is fixed by the language, so this module is a table, and the table is the specification.
//!
//! The scalar families are monomorphic and read directly. The container operations carry their element type as an operand, which is what lets them be typed without inventing anything: `List/get` is `(T : Type, List(T), Nat) -> T`, with `T` present in the term. The one operation that does *not* carry its type is a bare list literal, and the rule for it is the only place here that has to look at an operand to decide a type.

use {
    super::{check, infer},
    crate::{Kernel, KernelError, sort_of_intrinsic},
    curios_core::{Intrinsic, Reducer, Subterm, Term},
    curios_utilities::Grain,
};

fn bool_type() -> Term {
    Term::intrinsic(Intrinsic::BoolType)
}

fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn byte_type() -> Term {
    Term::intrinsic(Intrinsic::ByteType)
}

fn int_type() -> Term {
    Term::intrinsic(Intrinsic::IntType)
}

fn flt_type() -> Term {
    Term::intrinsic(Intrinsic::FltType)
}

fn handle_type() -> Term {
    Term::intrinsic(Intrinsic::HandleType)
}

fn bin_type(grain: Grain) -> Term {
    Term::intrinsic(Intrinsic::BinType(grain))
}

fn list_type(element: Term) -> Term {
    Term::intrinsic(Intrinsic::ListType(element))
}

fn cell_type(element: Term) -> Term {
    Term::intrinsic(Intrinsic::CellType(element))
}

fn io_type(result: Term) -> Term {
    Term::intrinsic(Intrinsic::IoType(result))
}

/// The type of `intrinsic`, having checked every operand against the type this operation demands of it.
pub(super) fn infer_intrinsic(
    kernel: &mut Kernel,
    intrinsic: &Intrinsic,
) -> Result<Term, KernelError> {
    // A grain says what a `Bin` is a sequence *of*: bytes at `X`, bits at `B`. Every `Bin` operation is the same rule at two different element types.
    let grain_element = |grain: Grain| match grain {
        Grain::X => byte_type(),
        Grain::B => bool_type(),
    };

    match intrinsic {
        // Type formers are types, and every closed one is small.
        Intrinsic::BoolType
        | Intrinsic::NatType
        | Intrinsic::ByteType
        | Intrinsic::IntType
        | Intrinsic::FltType
        | Intrinsic::BinType(_)
        | Intrinsic::HandleType => Ok(Term::type_ground()),

        // A parameterized former's sort is whatever `Sort::of` computes for it, asked here rather than restated: the element's own sort is *not* the answer, because a list or a cell of proofs has a length or an identity, and a description of proofs has an effect, so none of them is itself a proposition.
        Intrinsic::ListType(element)
        | Intrinsic::CellType(element)
        | Intrinsic::IoType(element) => {
            let sort = sort_of_intrinsic(kernel, intrinsic)?;
            check_is_type(kernel, element)?;

            Ok(sort.term())
        }

        // Literals.
        Intrinsic::Bool(_) => Ok(bool_type()),
        Intrinsic::Nat(_) => Ok(nat_type()),
        Intrinsic::Byte(_) => Ok(byte_type()),
        Intrinsic::Int(_) => Ok(int_type()),
        Intrinsic::Flt(_) => Ok(flt_type()),
        Intrinsic::Bin(grain, _) => Ok(bin_type(*grain)),
        Intrinsic::Handle(_) => Ok(handle_type()),

        // Comparisons: same-typed operands in, a boolean out.
        Intrinsic::BoolEql(l, r) | Intrinsic::BoolNeq(l, r) => {
            binary(kernel, l, r, bool_type(), bool_type())
        }
        Intrinsic::NatEql(l, r)
        | Intrinsic::NatNeq(l, r)
        | Intrinsic::NatLt(l, r)
        | Intrinsic::NatGt(l, r)
        | Intrinsic::NatLte(l, r)
        | Intrinsic::NatGte(l, r) => binary(kernel, l, r, nat_type(), bool_type()),
        Intrinsic::ByteEql(l, r)
        | Intrinsic::ByteLt(l, r)
        | Intrinsic::ByteLte(l, r)
        | Intrinsic::ByteGt(l, r)
        | Intrinsic::ByteGte(l, r) => binary(kernel, l, r, byte_type(), bool_type()),
        Intrinsic::IntEql(l, r)
        | Intrinsic::IntNeq(l, r)
        | Intrinsic::IntLt(l, r)
        | Intrinsic::IntGt(l, r)
        | Intrinsic::IntLte(l, r)
        | Intrinsic::IntGte(l, r) => binary(kernel, l, r, int_type(), bool_type()),
        Intrinsic::FltEql(l, r)
        | Intrinsic::FltNeq(l, r)
        | Intrinsic::FltLt(l, r)
        | Intrinsic::FltGt(l, r)
        | Intrinsic::FltLte(l, r)
        | Intrinsic::FltGte(l, r) => binary(kernel, l, r, flt_type(), bool_type()),
        Intrinsic::HandleEql(l, r) => binary(kernel, l, r, handle_type(), bool_type()),

        // Arithmetic and bitwise: closed on their carrier.
        Intrinsic::BoolAnd(l, r) | Intrinsic::BoolOr(l, r) | Intrinsic::BoolXor(l, r) => {
            binary(kernel, l, r, bool_type(), bool_type())
        }
        Intrinsic::NatAdd(l, r)
        | Intrinsic::NatSub(l, r)
        | Intrinsic::NatMul(l, r)
        | Intrinsic::NatAnd(l, r)
        | Intrinsic::NatOr(l, r)
        | Intrinsic::NatXor(l, r)
        | Intrinsic::NatShl(l, r)
        | Intrinsic::NatShr(l, r)
        | Intrinsic::NatRotl(l, r)
        | Intrinsic::NatRotr(l, r) => binary(kernel, l, r, nat_type(), nat_type()),
        Intrinsic::IntAdd(l, r)
        | Intrinsic::IntSub(l, r)
        | Intrinsic::IntMul(l, r)
        | Intrinsic::IntAnd(l, r)
        | Intrinsic::IntOr(l, r)
        | Intrinsic::IntXor(l, r)
        | Intrinsic::IntShl(l, r)
        | Intrinsic::IntShr(l, r)
        | Intrinsic::IntRotl(l, r)
        | Intrinsic::IntRotr(l, r) => binary(kernel, l, r, int_type(), int_type()),
        Intrinsic::FltAdd(l, r)
        | Intrinsic::FltSub(l, r)
        | Intrinsic::FltMul(l, r)
        | Intrinsic::FltDiv(l, r)
        | Intrinsic::FltRem(l, r)
        | Intrinsic::FltMin(l, r)
        | Intrinsic::FltMax(l, r)
        | Intrinsic::FltCopysign(l, r) => binary(kernel, l, r, flt_type(), flt_type()),

        Intrinsic::NatClz(i) | Intrinsic::NatCtz(i) | Intrinsic::NatPopcnt(i) => {
            unary(kernel, i, nat_type(), nat_type())
        }
        Intrinsic::IntClz(i) | Intrinsic::IntCtz(i) | Intrinsic::IntPopcnt(i) => {
            unary(kernel, i, int_type(), int_type())
        }
        Intrinsic::FltNeg(i)
        | Intrinsic::FltAbs(i)
        | Intrinsic::FltSqrt(i)
        | Intrinsic::FltFloor(i)
        | Intrinsic::FltCeil(i)
        | Intrinsic::FltTrunc(i)
        | Intrinsic::FltNearest(i) => unary(kernel, i, flt_type(), flt_type()),

        // Conversions.
        Intrinsic::ByteToNat(i) => unary(kernel, i, byte_type(), nat_type()),
        Intrinsic::NatToByte(i) => unary(kernel, i, nat_type(), byte_type()),
        Intrinsic::NatToInt(i) => unary(kernel, i, nat_type(), int_type()),
        Intrinsic::NatToFlt(i) => unary(kernel, i, nat_type(), flt_type()),
        Intrinsic::IntToNat(i) => unary(kernel, i, int_type(), nat_type()),
        Intrinsic::IntToFlt(i) => unary(kernel, i, int_type(), flt_type()),
        Intrinsic::FltToNat(i) => unary(kernel, i, flt_type(), nat_type()),
        Intrinsic::FltToInt(i) => unary(kernel, i, flt_type(), int_type()),
        // The IEEE-754 bytes of a float, and its inverse.
        Intrinsic::FltToLeBytes(i) => unary(kernel, i, flt_type(), bin_type(Grain::X)),
        Intrinsic::FltOfLeBytes { bin, .. } => unary(kernel, bin, bin_type(Grain::X), flt_type()),

        // The bounded arithmetic. Their value operands check exactly as the unbounded siblings above do; the bound field is checked separately, against the proposition `Intrinsic::operand_types` names for it.
        Intrinsic::NatDiv {
            dividend, divisor, ..
        }
        | Intrinsic::NatRem {
            dividend, divisor, ..
        } => binary(kernel, dividend, divisor, nat_type(), nat_type()),
        Intrinsic::IntDiv {
            dividend, divisor, ..
        }
        | Intrinsic::IntRem {
            dividend, divisor, ..
        } => binary(kernel, dividend, divisor, int_type(), int_type()),

        // `Bin`: a sequence of bytes or of bits, depending on the grain.
        Intrinsic::BinLen(grain, bin) => unary(kernel, bin, bin_type(*grain), nat_type()),
        Intrinsic::BinEql(grain, l, r) => binary(kernel, l, r, bin_type(*grain), bool_type()),
        Intrinsic::BinGet { grain, bin, index } => {
            check(kernel, bin, &bin_type(*grain))?;
            check(kernel, index, &nat_type())?;

            Ok(grain_element(*grain))
        }
        Intrinsic::BinSlice {
            grain,
            bin,
            start,
            end,
        } => {
            check(kernel, bin, &bin_type(*grain))?;
            check(kernel, start, &nat_type())?;
            check(kernel, end, &nat_type())?;

            Ok(bin_type(*grain))
        }
        Intrinsic::BinAppend {
            grain,
            bin,
            element,
        } => {
            check(kernel, bin, &bin_type(*grain))?;
            check(kernel, element, &grain_element(*grain))?;

            Ok(bin_type(*grain))
        }
        Intrinsic::BinConcat { grain, operands } => {
            for operand in operands {
                check(kernel, operand, &bin_type(*grain))?;
            }

            Ok(bin_type(*grain))
        }

        // A list literal carries its element type like every other `List` operation — `[]` included, which is the case that used to be refused for having no element to read a type from.
        Intrinsic::List {
            element,
            items: elements,
        } => {
            let element = check_is_type(kernel, element)?;
            for entry in elements {
                check(kernel, entry, &element)?;
            }

            Ok(list_type(element))
        }
        Intrinsic::ListLen { element, list } => {
            let element = check_is_type(kernel, element)?;
            check(kernel, list, &list_type(element))?;

            Ok(nat_type())
        }
        Intrinsic::ListGet {
            element,
            list,
            index,
        } => {
            let element = check_is_type(kernel, element)?;
            check(kernel, list, &list_type(element.clone()))?;
            check(kernel, index, &nat_type())?;

            Ok(element)
        }
        Intrinsic::ListSlice {
            element,
            list,
            start,
            end,
        } => {
            let element = check_is_type(kernel, element)?;
            check(kernel, list, &list_type(element.clone()))?;
            check(kernel, start, &nat_type())?;
            check(kernel, end, &nat_type())?;

            Ok(list_type(element))
        }
        Intrinsic::ListAppend {
            element,
            list,
            item: value,
        } => {
            let element = check_is_type(kernel, element)?;
            check(kernel, list, &list_type(element.clone()))?;
            check(kernel, value, &element)?;

            Ok(list_type(element))
        }
        Intrinsic::ListConcat { element, operands } => {
            let element = check_is_type(kernel, element)?;
            for operand in operands {
                check(kernel, operand, &list_type(element.clone()))?;
            }

            Ok(list_type(element))
        }
        Intrinsic::ListMap {
            from,
            to,
            list,
            function,
        } => {
            let from = check_is_type(kernel, from)?;
            let to = check_is_type(kernel, to)?;
            check(kernel, list, &list_type(from.clone()))?;

            let binder = kernel.fresh(Some("x"));
            check(
                kernel,
                function,
                &Term::func_type([(binder, from)], to.clone()),
            )?;

            Ok(list_type(to))
        }

        // A mutable cell. Its identity is what makes a `Cell` of proofs relevant — see `Sort::of`.
        //
        // All three operations are host effects and so describe rather than do. `CellGet` returning `Io(T)` rather than `T` is what makes `match Cell/get(c)` ill-typed, which is the whole of what the purity analysis used to decide by walking the scrutinee.
        Intrinsic::Cell { element, initial } => {
            let element = check_is_type(kernel, element)?;
            check(kernel, initial, &element)?;

            Ok(io_type(cell_type(element)))
        }
        Intrinsic::CellGet { element, cell } => {
            let element = check_is_type(kernel, element)?;
            check(kernel, cell, &cell_type(element.clone()))?;

            Ok(io_type(element))
        }
        Intrinsic::CellSet {
            element,
            cell,
            value,
        } => {
            let element = check_is_type(kernel, element)?;
            check(kernel, cell, &cell_type(element.clone()))?;
            check(kernel, value, &element)?;

            Ok(io_type(Term::tuple_type_unit()))
        }

        // `exit` ends the process, and its result is the unit type rather than whatever the caller demanded.
        //
        // That is the whole of the rule, and it is why there is no side condition here. A term that never returns is unsound exactly when it inhabits a type nothing total inhabits — the forgery is the problem, not the non-return — and restricting *which* type it may be given cannot fix that, because any `Type`-sorted empty inductive eliminates into `Prop` unguarded. Fixing the result at `{}`, which `()` already inhabits, leaves nothing to forge.
        Intrinsic::ProcExit(code) => {
            check(kernel, code, &nat_type())?;

            Ok(io_type(Term::tuple_type_unit()))
        }

        // The two constructors of the opaque effect carrier. There is no third: nothing here or anywhere lowers an `Io(T)` to its `T`, which is what makes every term of non-`Io` type pure by typing.
        Intrinsic::IoPure { result, value } => {
            let result = check_is_type(kernel, result)?;
            check(kernel, value, &result)?;

            Ok(io_type(result))
        }
        Intrinsic::IoBind {
            from,
            to,
            action,
            continuation,
        } => {
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

/// Check that `term` is a type, and hand it back. An intrinsic that carries its element type carries a *type*, and taking that on trust is how a container of nonsense would be admitted.
fn check_is_type(kernel: &mut Kernel, term: &Term) -> Result<Term, KernelError> {
    let inferred = infer(kernel, term)?;

    match &*kernel.reduce_forced(inferred.clone())? {
        Subterm::Type(_) | Subterm::Prop => Ok(term.clone()),
        _ => Err(KernelError::NotASort(inferred)),
    }
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
