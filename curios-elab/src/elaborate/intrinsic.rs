use {
    super::{Context, Error, Mode, elaborate, expect},
    crate::reduce_with,
    curios_core::{ImplicitOrigin, Intrinsic, Subterm, Term},
    curios_utilities::Grain,
};

/// Elaborate both operands of a homogeneous binary intrinsic at `operand`, then rebuild the variant through its constructor (`build`) and pair it with `result`. Lets each arm name itself once instead of destructuring an OR-pattern and re-matching just to reattach the elaborated operands.
fn binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    operand: &Term,
    result: Term,
    build: fn(Term, Term) -> Intrinsic,
) -> Result<(Intrinsic, Term), Error> {
    let left = elaborate(context, left, Mode::Check(operand.clone()))?.0;
    let right = elaborate(context, right, Mode::Check(operand.clone()))?.0;
    Ok((build(left, right), result))
}

/// The unary counterpart of [`binary`], for single-operand scalar/conversion intrinsics.
fn unary(
    context: &mut Context,
    inner: &Term,
    operand: &Term,
    result: Term,
    build: fn(Term) -> Intrinsic,
) -> Result<(Intrinsic, Term), Error> {
    let inner = elaborate(context, inner, Mode::Check(operand.clone()))?.0;
    Ok((build(inner), result))
}

/// Elaborate a packed binary value, requiring its inferred type to be the requested `Bits` or `Bytes` grain, and return the rebuilt operand. The indexing/slicing intrinsics read their shape off the operand's type, so they infer it rather than checking against a known one.
fn infer_bin(context: &mut Context, bin: &Term) -> Result<Term, Error> {
    let (bin, actual) = elaborate(context, bin, Mode::Infer)?;
    match &*reduce_with(context, &actual)? {
        Subterm::Intrinsic(Intrinsic::BinType(Grain::X)) => Ok(bin),
        other => Err(Error::type_mismatch(
            other.clone(),
            Subterm::Intrinsic(Intrinsic::BinType(Grain::X)),
        )),
    }
}

fn list_type(elem: Term) -> Term {
    Subterm::Intrinsic(Intrinsic::ListType(elem)).into()
}

fn io_type(result: Term) -> Term {
    Subterm::Intrinsic(Intrinsic::IoType(result)).into()
}

/// Check every element of a `List` literal against an already-determined element type, returning the rebuilt elements. Shared by the two ways the element type is fixed: borrowed from `expected` when checking, or a fresh metavar when inferring (see [`elaborate_intrinsic`] and [`synth_intrinsic`]'s `List` arm).
fn check_list_elems(
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

/// Synthesize an intrinsic's type, checking *and rebuilding* its operands. Mirrors the old `infer_intrinsic`, but every operand obligation goes through `elaborate(Check)` and the elaborated operand is kept, so the returned `Intrinsic` is the authoritative (rebuilt) one that flows on to `zonk`/`erase`.
fn synth_intrinsic(
    context: &mut Context,
    intrinsic: &Intrinsic,
) -> Result<(Intrinsic, Term), Error> {
    let nat_type: Term = Subterm::Intrinsic(Intrinsic::NatType).into();
    let byte_type: Term = Subterm::Intrinsic(Intrinsic::ByteType).into();
    let int_type: Term = Subterm::Intrinsic(Intrinsic::IntType).into();
    let flt_type: Term = Subterm::Intrinsic(Intrinsic::FltType).into();
    let bool_type: Term = Subterm::Intrinsic(Intrinsic::BoolType).into();
    let bin_type: Term = Subterm::Intrinsic(Intrinsic::BinType(Grain::X)).into();
    let bin_b_type: Term = Subterm::Intrinsic(Intrinsic::BinType(Grain::B)).into();
    let handle_type: Term = Subterm::Intrinsic(Intrinsic::HandleType).into();

    Ok(match intrinsic {
        Intrinsic::BoolType => (intrinsic.clone(), Term::type_ground()),
        Intrinsic::Bool(_) => (intrinsic.clone(), bool_type),
        Intrinsic::NatType => (intrinsic.clone(), Term::type_ground()),
        Intrinsic::Nat(_) => (intrinsic.clone(), nat_type),
        Intrinsic::ByteType => (intrinsic.clone(), Term::type_ground()),
        Intrinsic::Byte(_) => (intrinsic.clone(), byte_type.clone()),
        Intrinsic::ByteToNat(i) => unary(
            context,
            i,
            &byte_type,
            nat_type.clone(),
            Intrinsic::ByteToNat,
        )?,
        Intrinsic::NatToByte(i) => unary(
            context,
            i,
            &nat_type,
            byte_type.clone(),
            Intrinsic::NatToByte,
        )?,
        Intrinsic::ByteEql(l, r) => binary(
            context,
            l,
            r,
            &byte_type,
            bool_type.clone(),
            Intrinsic::ByteEql,
        )?,
        Intrinsic::ByteLt(l, r) => binary(
            context,
            l,
            r,
            &byte_type,
            bool_type.clone(),
            Intrinsic::ByteLt,
        )?,
        Intrinsic::ByteLte(l, r) => binary(
            context,
            l,
            r,
            &byte_type,
            bool_type.clone(),
            Intrinsic::ByteLte,
        )?,
        Intrinsic::ByteGt(l, r) => binary(
            context,
            l,
            r,
            &byte_type,
            bool_type.clone(),
            Intrinsic::ByteGt,
        )?,
        Intrinsic::ByteGte(l, r) => binary(
            context,
            l,
            r,
            &byte_type,
            bool_type.clone(),
            Intrinsic::ByteGte,
        )?,
        Intrinsic::NatEql(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            bool_type.clone(),
            Intrinsic::NatEql,
        )?,
        Intrinsic::HandleEql(l, r) => binary(
            context,
            l,
            r,
            &handle_type,
            bool_type.clone(),
            Intrinsic::HandleEql,
        )?,
        Intrinsic::NatNeq(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            bool_type.clone(),
            Intrinsic::NatNeq,
        )?,
        Intrinsic::NatLt(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            bool_type.clone(),
            Intrinsic::NatLt,
        )?,
        Intrinsic::NatGt(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            bool_type.clone(),
            Intrinsic::NatGt,
        )?,
        Intrinsic::NatLte(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            bool_type.clone(),
            Intrinsic::NatLte,
        )?,
        Intrinsic::NatGte(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            bool_type.clone(),
            Intrinsic::NatGte,
        )?,
        Intrinsic::NatAdd(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            nat_type.clone(),
            Intrinsic::NatAdd,
        )?,
        Intrinsic::NatSub(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            nat_type.clone(),
            Intrinsic::NatSub,
        )?,
        Intrinsic::NatMul(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            nat_type.clone(),
            Intrinsic::NatMul,
        )?,
        Intrinsic::NatDiv(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            nat_type.clone(),
            Intrinsic::NatDiv,
        )?,
        Intrinsic::NatRem(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            nat_type.clone(),
            Intrinsic::NatRem,
        )?,
        Intrinsic::NatAnd(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            nat_type.clone(),
            Intrinsic::NatAnd,
        )?,
        Intrinsic::NatOr(l, r) => {
            binary(context, l, r, &nat_type, nat_type.clone(), Intrinsic::NatOr)?
        }
        Intrinsic::NatXor(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            nat_type.clone(),
            Intrinsic::NatXor,
        )?,
        Intrinsic::NatShl(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            nat_type.clone(),
            Intrinsic::NatShl,
        )?,
        Intrinsic::NatShr(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            nat_type.clone(),
            Intrinsic::NatShr,
        )?,
        Intrinsic::NatRotl(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            nat_type.clone(),
            Intrinsic::NatRotl,
        )?,
        Intrinsic::NatRotr(l, r) => binary(
            context,
            l,
            r,
            &nat_type,
            nat_type.clone(),
            Intrinsic::NatRotr,
        )?,
        Intrinsic::NatClz(i) => unary(context, i, &nat_type, nat_type.clone(), Intrinsic::NatClz)?,
        Intrinsic::NatCtz(i) => unary(context, i, &nat_type, nat_type.clone(), Intrinsic::NatCtz)?,
        Intrinsic::NatPopcnt(i) => unary(
            context,
            i,
            &nat_type,
            nat_type.clone(),
            Intrinsic::NatPopcnt,
        )?,
        Intrinsic::BoolAnd(l, r) => binary(
            context,
            l,
            r,
            &bool_type,
            bool_type.clone(),
            Intrinsic::BoolAnd,
        )?,
        Intrinsic::BoolOr(l, r) => binary(
            context,
            l,
            r,
            &bool_type,
            bool_type.clone(),
            Intrinsic::BoolOr,
        )?,
        Intrinsic::BoolXor(l, r) => binary(
            context,
            l,
            r,
            &bool_type,
            bool_type.clone(),
            Intrinsic::BoolXor,
        )?,
        Intrinsic::BoolEql(l, r) => binary(
            context,
            l,
            r,
            &bool_type,
            bool_type.clone(),
            Intrinsic::BoolEql,
        )?,
        Intrinsic::BoolNeq(l, r) => binary(
            context,
            l,
            r,
            &bool_type,
            bool_type.clone(),
            Intrinsic::BoolNeq,
        )?,
        Intrinsic::IntType => (intrinsic.clone(), Term::type_ground()),
        Intrinsic::Int(_) => (intrinsic.clone(), int_type),
        Intrinsic::IntEql(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            bool_type.clone(),
            Intrinsic::IntEql,
        )?,
        Intrinsic::IntNeq(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            bool_type.clone(),
            Intrinsic::IntNeq,
        )?,
        Intrinsic::IntLt(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            bool_type.clone(),
            Intrinsic::IntLt,
        )?,
        Intrinsic::IntGt(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            bool_type.clone(),
            Intrinsic::IntGt,
        )?,
        Intrinsic::IntLte(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            bool_type.clone(),
            Intrinsic::IntLte,
        )?,
        Intrinsic::IntGte(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            bool_type.clone(),
            Intrinsic::IntGte,
        )?,
        Intrinsic::IntAdd(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            int_type.clone(),
            Intrinsic::IntAdd,
        )?,
        Intrinsic::IntSub(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            int_type.clone(),
            Intrinsic::IntSub,
        )?,
        Intrinsic::IntMul(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            int_type.clone(),
            Intrinsic::IntMul,
        )?,
        Intrinsic::IntDiv(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            int_type.clone(),
            Intrinsic::IntDiv,
        )?,
        Intrinsic::IntRem(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            int_type.clone(),
            Intrinsic::IntRem,
        )?,
        Intrinsic::IntAnd(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            int_type.clone(),
            Intrinsic::IntAnd,
        )?,
        Intrinsic::IntOr(l, r) => {
            binary(context, l, r, &int_type, int_type.clone(), Intrinsic::IntOr)?
        }
        Intrinsic::IntXor(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            int_type.clone(),
            Intrinsic::IntXor,
        )?,
        Intrinsic::IntShl(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            int_type.clone(),
            Intrinsic::IntShl,
        )?,
        Intrinsic::IntShr(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            int_type.clone(),
            Intrinsic::IntShr,
        )?,
        Intrinsic::IntRotl(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            int_type.clone(),
            Intrinsic::IntRotl,
        )?,
        Intrinsic::IntRotr(l, r) => binary(
            context,
            l,
            r,
            &int_type,
            int_type.clone(),
            Intrinsic::IntRotr,
        )?,
        Intrinsic::IntClz(i) => unary(context, i, &int_type, int_type.clone(), Intrinsic::IntClz)?,
        Intrinsic::IntCtz(i) => unary(context, i, &int_type, int_type.clone(), Intrinsic::IntCtz)?,
        Intrinsic::IntPopcnt(i) => unary(
            context,
            i,
            &int_type,
            int_type.clone(),
            Intrinsic::IntPopcnt,
        )?,
        Intrinsic::FltType => (intrinsic.clone(), Term::type_ground()),
        Intrinsic::Flt(_) => (intrinsic.clone(), flt_type),
        Intrinsic::FltAdd(l, r) => binary(
            context,
            l,
            r,
            &flt_type,
            flt_type.clone(),
            Intrinsic::FltAdd,
        )?,
        Intrinsic::FltSub(l, r) => binary(
            context,
            l,
            r,
            &flt_type,
            flt_type.clone(),
            Intrinsic::FltSub,
        )?,
        Intrinsic::FltMul(l, r) => binary(
            context,
            l,
            r,
            &flt_type,
            flt_type.clone(),
            Intrinsic::FltMul,
        )?,
        Intrinsic::FltDiv(l, r) => binary(
            context,
            l,
            r,
            &flt_type,
            flt_type.clone(),
            Intrinsic::FltDiv,
        )?,
        Intrinsic::FltRem(l, r) => binary(
            context,
            l,
            r,
            &flt_type,
            flt_type.clone(),
            Intrinsic::FltRem,
        )?,
        Intrinsic::FltMin(l, r) => binary(
            context,
            l,
            r,
            &flt_type,
            flt_type.clone(),
            Intrinsic::FltMin,
        )?,
        Intrinsic::FltMax(l, r) => binary(
            context,
            l,
            r,
            &flt_type,
            flt_type.clone(),
            Intrinsic::FltMax,
        )?,
        Intrinsic::FltCopysign(l, r) => binary(
            context,
            l,
            r,
            &flt_type,
            flt_type.clone(),
            Intrinsic::FltCopysign,
        )?,
        Intrinsic::FltNeg(i) => unary(context, i, &flt_type, flt_type.clone(), Intrinsic::FltNeg)?,
        Intrinsic::FltAbs(i) => unary(context, i, &flt_type, flt_type.clone(), Intrinsic::FltAbs)?,
        Intrinsic::FltSqrt(i) => {
            unary(context, i, &flt_type, flt_type.clone(), Intrinsic::FltSqrt)?
        }
        Intrinsic::FltFloor(i) => {
            unary(context, i, &flt_type, flt_type.clone(), Intrinsic::FltFloor)?
        }
        Intrinsic::FltCeil(i) => {
            unary(context, i, &flt_type, flt_type.clone(), Intrinsic::FltCeil)?
        }
        Intrinsic::FltTrunc(i) => {
            unary(context, i, &flt_type, flt_type.clone(), Intrinsic::FltTrunc)?
        }
        Intrinsic::FltNearest(i) => unary(
            context,
            i,
            &flt_type,
            flt_type.clone(),
            Intrinsic::FltNearest,
        )?,
        Intrinsic::FltEql(l, r) => binary(
            context,
            l,
            r,
            &flt_type,
            bool_type.clone(),
            Intrinsic::FltEql,
        )?,
        Intrinsic::FltNeq(l, r) => binary(
            context,
            l,
            r,
            &flt_type,
            bool_type.clone(),
            Intrinsic::FltNeq,
        )?,
        Intrinsic::FltLt(l, r) => binary(
            context,
            l,
            r,
            &flt_type,
            bool_type.clone(),
            Intrinsic::FltLt,
        )?,
        Intrinsic::FltGt(l, r) => binary(
            context,
            l,
            r,
            &flt_type,
            bool_type.clone(),
            Intrinsic::FltGt,
        )?,
        Intrinsic::FltLte(l, r) => binary(
            context,
            l,
            r,
            &flt_type,
            bool_type.clone(),
            Intrinsic::FltLte,
        )?,
        Intrinsic::FltGte(l, r) => binary(
            context,
            l,
            r,
            &flt_type,
            bool_type.clone(),
            Intrinsic::FltGte,
        )?,
        // `Flt/to_le_bytes` exposes the IEEE-754 bytes (`Bin`); `/std/Flt/to_str` renders them to the proof-carrying `/syn/Str` in Curios (Dragon4).
        Intrinsic::FltToLeBytes(i) => unary(
            context,
            i,
            &flt_type,
            bin_type.clone(),
            Intrinsic::FltToLeBytes,
        )?,
        // `Flt/of_le_bytes` assembles a float from its IEEE-754 bytes — the inverse of `to_le_bytes`; traps unless the `Bin` is exactly 4 bytes.
        Intrinsic::FltOfLeBytes(i) => unary(
            context,
            i,
            &bin_type,
            flt_type.clone(),
            Intrinsic::FltOfLeBytes,
        )?,
        Intrinsic::NatToInt(i) => {
            unary(context, i, &nat_type, int_type.clone(), Intrinsic::NatToInt)?
        }
        Intrinsic::NatToFlt(i) => {
            unary(context, i, &nat_type, flt_type.clone(), Intrinsic::NatToFlt)?
        }
        Intrinsic::IntToNat(i) => {
            unary(context, i, &int_type, nat_type.clone(), Intrinsic::IntToNat)?
        }
        Intrinsic::IntToFlt(i) => {
            unary(context, i, &int_type, flt_type.clone(), Intrinsic::IntToFlt)?
        }
        Intrinsic::FltToNat(i) => {
            unary(context, i, &flt_type, nat_type.clone(), Intrinsic::FltToNat)?
        }
        Intrinsic::FltToInt(i) => {
            unary(context, i, &flt_type, int_type.clone(), Intrinsic::FltToInt)?
        }
        Intrinsic::BinType(Grain::X) => (intrinsic.clone(), Term::type_ground()),
        Intrinsic::Bin(Grain::X, _) => (intrinsic.clone(), bin_type),
        Intrinsic::BinLen(Grain::X, bin) => {
            let bin = infer_bin(context, bin)?;
            (Intrinsic::BinLen(Grain::X, bin), nat_type)
        }
        Intrinsic::BinEql(Grain::X, left, right) => {
            let left = elaborate(context, left, Mode::Check(bin_type.clone()))?.0;
            let right = elaborate(context, right, Mode::Check(bin_type))?.0;
            (Intrinsic::BinEql(Grain::X, left, right), bool_type)
        }
        Intrinsic::BinGet(Grain::X, bin, index) => {
            let bin = infer_bin(context, bin)?;
            let index = elaborate(context, index, Mode::Check(nat_type.clone()))?.0;
            (
                Intrinsic::BinGet(Grain::X, bin, index),
                Term::intrinsic(Intrinsic::ByteType),
            )
        }
        Intrinsic::BinSlice(Grain::X, bin, start, end) => {
            let bin = infer_bin(context, bin)?;
            let start = elaborate(context, start, Mode::Check(nat_type.clone()))?.0;
            let end = elaborate(context, end, Mode::Check(nat_type))?.0;
            (Intrinsic::BinSlice(Grain::X, bin, start, end), bin_type)
        }
        Intrinsic::BinAppend(Grain::X, bin, byte) => {
            let bin = infer_bin(context, bin)?;
            let byte = elaborate(
                context,
                byte,
                Mode::Check(Term::intrinsic(Intrinsic::ByteType)),
            )?
            .0;
            (Intrinsic::BinAppend(Grain::X, bin, byte), bin_type)
        }
        Intrinsic::BinConcat(Grain::X, operands) => {
            let mut elaborated = Vec::with_capacity(operands.len());
            for operand in operands {
                elaborated.push(elaborate(context, operand, Mode::Check(bin_type.clone()))?.0);
            }
            (Intrinsic::BinConcat(Grain::X, elaborated), bin_type)
        }
        Intrinsic::BinType(Grain::B) => (intrinsic.clone(), Term::type_ground()),
        Intrinsic::Bin(Grain::B, _) => (intrinsic.clone(), bin_b_type.clone()),
        Intrinsic::BinLen(Grain::B, bin) => {
            let bin = elaborate(context, bin, Mode::Check(bin_b_type.clone()))?.0;
            (Intrinsic::BinLen(Grain::B, bin), nat_type)
        }
        Intrinsic::BinEql(Grain::B, left, right) => {
            let left = elaborate(context, left, Mode::Check(bin_b_type.clone()))?.0;
            let right = elaborate(context, right, Mode::Check(bin_b_type))?.0;
            (Intrinsic::BinEql(Grain::B, left, right), bool_type)
        }
        Intrinsic::BinGet(Grain::B, bin, index) => {
            let bin = elaborate(context, bin, Mode::Check(bin_b_type))?.0;
            let index = elaborate(context, index, Mode::Check(nat_type.clone()))?.0;
            (Intrinsic::BinGet(Grain::B, bin, index), bool_type)
        }
        Intrinsic::BinSlice(Grain::B, bin, start, end) => {
            let bin = elaborate(context, bin, Mode::Check(bin_b_type.clone()))?.0;
            let start = elaborate(context, start, Mode::Check(nat_type.clone()))?.0;
            let end = elaborate(context, end, Mode::Check(nat_type))?.0;
            (Intrinsic::BinSlice(Grain::B, bin, start, end), bin_b_type)
        }
        Intrinsic::BinAppend(Grain::B, bin, bit) => {
            let bin = elaborate(context, bin, Mode::Check(bin_b_type.clone()))?.0;
            let bit = elaborate(context, bit, Mode::Check(bool_type))?.0;
            (Intrinsic::BinAppend(Grain::B, bin, bit), bin_b_type)
        }
        Intrinsic::BinConcat(Grain::B, operands) => {
            let mut elaborated = Vec::with_capacity(operands.len());
            for operand in operands {
                elaborated.push(elaborate(context, operand, Mode::Check(bin_b_type.clone()))?.0);
            }
            (Intrinsic::BinConcat(Grain::B, elaborated), bin_b_type)
        }
        // The former's sort is `Sort::of`'s to compute, not the element's own: a list *of* proofs has a length, so it is not itself a proposition however propositional its element is. Restating the rule here typed `List(P)` at `Prop`, which admitted it wherever a proposition was wanted.
        Intrinsic::ListType(elem) => {
            let elem = crate::check_is_sort(context, elem)?.0;
            let former: Term = Subterm::Intrinsic(Intrinsic::ListType(elem.clone())).into();
            let sort = crate::sort_term(context, &former)?;
            (Intrinsic::ListType(elem), sort)
        }
        // Inferring: the element type is unknown, so mint a fresh metavar — the implicit `@T` a `nil`/`cons` constructor would insert — which the elements solve, an empty `[]` leaving it for a later unification to ground. Checking goes through `elaborate_intrinsic`, which borrows the concrete element type from `expected` before reaching here.
        Intrinsic::List(_, elems) => {
            let classifier = context.fresh_classifier_type("list element classifier");
            let elem_type = context.fresh_metavar(
                classifier,
                None,
                ImplicitOrigin {
                    func: "List".to_string(),
                    binder: "T".to_string(),
                },
            );
            let elaborated = check_list_elems(context, elems, &elem_type)?;
            (
                Intrinsic::List(elem_type.clone(), elaborated),
                list_type(elem_type),
            )
        }
        Intrinsic::ListLen(type_, list) => {
            let type_ = crate::check_is_sort(context, type_)?.0;
            let list_type = list_type(type_.clone());
            let list = elaborate(context, list, Mode::Check(list_type))?.0;
            (Intrinsic::ListLen(type_, list), nat_type)
        }
        Intrinsic::ListGet(type_, list, index) => {
            let type_ = crate::check_is_sort(context, type_)?.0;
            let list_type = list_type(type_.clone());
            let list = elaborate(context, list, Mode::Check(list_type))?.0;
            let index = elaborate(context, index, Mode::Check(nat_type))?.0;
            let output = type_.clone();
            (Intrinsic::ListGet(type_, list, index), output)
        }
        Intrinsic::ListSlice(type_, list, start, end) => {
            let type_ = crate::check_is_sort(context, type_)?.0;
            let list_type = list_type(type_.clone());
            let list = elaborate(context, list, Mode::Check(list_type.clone()))?.0;
            let start = elaborate(context, start, Mode::Check(nat_type.clone()))?.0;
            let end = elaborate(context, end, Mode::Check(nat_type))?.0;
            (Intrinsic::ListSlice(type_, list, start, end), list_type)
        }
        Intrinsic::ListAppend(type_, list, elem) => {
            let type_ = crate::check_is_sort(context, type_)?.0;
            let list_type = list_type(type_.clone());
            let list = elaborate(context, list, Mode::Check(list_type.clone()))?.0;
            let elem = elaborate(context, elem, Mode::Check(type_.clone()))?.0;
            (Intrinsic::ListAppend(type_, list, elem), list_type)
        }
        Intrinsic::ListConcat(type_, operands) => {
            let type_ = crate::check_is_sort(context, type_)?.0;
            let list_type = list_type(type_.clone());
            let mut elaborated = Vec::with_capacity(operands.len());
            for operand in operands {
                elaborated.push(elaborate(context, operand, Mode::Check(list_type.clone()))?.0);
            }
            (Intrinsic::ListConcat(type_, elaborated), list_type)
        }
        Intrinsic::ListMap(a, b, list, f) => {
            let a = crate::check_is_sort(context, a)?.0;
            let b = crate::check_is_sort(context, b)?.0;
            let list_a = list_type(a.clone());
            let list = elaborate(context, list, Mode::Check(list_a))?.0;
            let f_type = Term::func_type([(context.fresh(Some("x")), a.clone())], b.clone());
            let f = elaborate(context, f, Mode::Check(f_type))?.0;
            let list_b = list_type(b.clone());
            (Intrinsic::ListMap(a, b, list, f), list_b)
        }
        Intrinsic::HandleType => (intrinsic.clone(), Term::type_ground()),
        Intrinsic::Handle(_) => (intrinsic.clone(), handle_type),
        // `(n : Nat) -> Io({})`: exit ends the process. The result inside the wrapper is unit rather than the caller's choice — see `Intrinsic::ProcExit` in `curios-core` for why fixing it at an inhabited type is what makes the intrinsic sound.
        Intrinsic::ProcExit(code) => {
            let code = elaborate(context, code, Mode::Check(nat_type))?.0;
            (Intrinsic::ProcExit(code), io_type(Term::tuple_type_unit()))
        }
        // The same rule as `ListType` above, and for the same reason: a cell has an identity.
        Intrinsic::CellType(elem) => {
            let elem = crate::check_is_sort(context, elem)?.0;
            let former: Term = Subterm::Intrinsic(Intrinsic::CellType(elem.clone())).into();
            let sort = crate::sort_term(context, &former)?;
            (Intrinsic::CellType(elem), sort)
        }
        // Allocating, reading, and writing a cell are all host effects, so all three describe rather than do. `CellGet` is the one the whole discipline was named for: `match Cell/get(c)` is now ill-typed, because a description has no cases.
        Intrinsic::Cell(type_, init) => {
            let type_ = crate::check_is_sort(context, type_)?.0;
            let init = elaborate(context, init, Mode::Check(type_.clone()))?.0;
            let cell_type: Term = Subterm::Intrinsic(Intrinsic::CellType(type_.clone())).into();
            (Intrinsic::Cell(type_, init), io_type(cell_type))
        }
        Intrinsic::CellSet(type_, cell, value) => {
            let type_ = crate::check_is_sort(context, type_)?.0;
            let cell_type: Term = Subterm::Intrinsic(Intrinsic::CellType(type_.clone())).into();
            let cell = elaborate(context, cell, Mode::Check(cell_type))?.0;
            let value = elaborate(context, value, Mode::Check(type_.clone()))?.0;
            (
                Intrinsic::CellSet(type_, cell, value),
                io_type(Term::tuple_type_unit()),
            )
        }
        Intrinsic::CellGet(type_, cell) => {
            let type_ = crate::check_is_sort(context, type_)?.0;
            let cell_type: Term = Subterm::Intrinsic(Intrinsic::CellType(type_.clone())).into();
            let cell = elaborate(context, cell, Mode::Check(cell_type))?.0;
            let output = io_type(type_.clone());
            (Intrinsic::CellGet(type_, cell), output)
        }
        // The same rule as `ListType` and `CellType` above, and for a third reason: a description has an effect, so it is not proof-irrelevant however propositional its result.
        Intrinsic::IoType(result) => {
            let result = crate::check_is_sort(context, result)?.0;
            let former: Term = Subterm::Intrinsic(Intrinsic::IoType(result.clone())).into();
            let sort = crate::sort_term(context, &former)?;
            (Intrinsic::IoType(result), sort)
        }
        Intrinsic::IoPure(type_, value) => {
            let type_ = crate::check_is_sort(context, type_)?.0;
            let value = elaborate(context, value, Mode::Check(type_.clone()))?.0;
            let io_type = io_type(type_.clone());
            (Intrinsic::IoPure(type_, value), io_type)
        }
        Intrinsic::IoBind(from, to, action, continuation) => {
            let from = crate::check_is_sort(context, from)?.0;
            let to = crate::check_is_sort(context, to)?.0;
            let action = elaborate(context, action, Mode::Check(io_type(from.clone())))?.0;
            let continuation_type = Term::func_type(
                [(context.fresh(Some("x")), from.clone())],
                io_type(to.clone()),
            );
            let continuation = elaborate(context, continuation, Mode::Check(continuation_type))?.0;
            let io_to = io_type(to.clone());
            (Intrinsic::IoBind(from, to, action, continuation), io_to)
        }
    })
}

pub(crate) fn elaborate_intrinsic(
    context: &mut Context,
    term: &Term,
    intrinsic: &Intrinsic,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // `List` is bidirectional. Checking against a concrete `List(T)`, it borrows the element type from `expected` — definitional, so each element is checked against the known type (better errors, and numeric element literals pick the right numeric type). Any other expected shape — a stuck `?M(?A)` awaiting the flex-apply imitation included — falls through to `synth_intrinsic`, which mints a fresh element-type metavar; the check-after-infer unification then equates `List(?T)` with the expected type (pinning `?M := List`, or reporting the genuine mismatch).
    if let (Intrinsic::List(_, elems), Mode::Check(expected)) = (intrinsic, &mode)
        && let Subterm::Intrinsic(Intrinsic::ListType(elem_type)) =
            &*reduce_with(context, expected)?
    {
        let elaborated = check_list_elems(context, elems, elem_type)?;

        return Ok((
            Term::intrinsic(Intrinsic::List(elem_type.clone(), elaborated)),
            expected.clone(),
        ));
    }

    // `ListConcat` mirrors `List`'s bidirectionality — and must, because the lowering of a spread list literal `[a, ..xs, b]` mints a fresh metavar for the element-type slot. Checking against a concrete `List(T)`, solve the slot against `expected` FIRST (`expect` unifies `List(slot)` with it), so the operands — the literal chunks especially — elaborate against the known element type instead of default-solving it from the first element. Any other expected shape falls through to `synth_intrinsic`, exactly as for `List`.
    if let (Intrinsic::ListConcat(type_slot, operands), Mode::Check(expected)) = (intrinsic, &mode)
        && let Subterm::Intrinsic(Intrinsic::ListType(_)) = &*reduce_with(context, expected)?
    {
        let type_slot = crate::check_is_sort(context, type_slot)?.0;
        expect(context, term, &list_type(type_slot.clone()), expected)?;

        let mut elaborated = Vec::with_capacity(operands.len());
        for operand in operands {
            elaborated.push(elaborate(context, operand, Mode::Check(expected.clone()))?.0);
        }

        return Ok((
            Term::intrinsic(Intrinsic::ListConcat(type_slot, elaborated)),
            expected.clone(),
        ));
    }

    let (intrinsic, type_) = synth_intrinsic(context, intrinsic)?;

    if let Mode::Check(expected) = &mode {
        expect(context, term, &type_, expected)?;
    }

    Ok((Term::intrinsic(intrinsic), type_))
}
