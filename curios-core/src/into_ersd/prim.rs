//! The primitive transcription: each Core primitive to its arena identity,
//! shape for shape.
//!
//! No carrier is chosen here: `Bool` values and operations stay `Bool`-shaped,
//! `Byte` stays `Byte`, `Handle` stays an opaque handle constant, and a packed
//! binary's element is its grain's shape (`Byte` for `X`, `Bool` for `B`) —
//! every collapse onto a runtime carrier belongs to the lowering out of the
//! representation. Unbounded type-level numerals narrow to the exact 32-bit
//! domains here (the numeric law's Core border), with overflow reported as an
//! error, never wrapped.

use {
    super::{
        BigUint, Context, Error, Lowering, Nat, Outcome, Prim, Subterm, Term, ToPrimitive, emitted,
        reduce_with, wire_term,
    },
    curios_base::{Grain, Int},
};

fn narrow_nat(value: &BigUint) -> Result<u32, Error> {
    value
        .to_u32()
        .ok_or_else(|| Error::nat_overflow(value.clone()))
}

fn narrow_int(value: &Int) -> Result<i32, Error> {
    value
        .to_i32()
        .ok_or_else(|| Error::int_overflow(value.clone()))
}

fn nat_type() -> Term {
    Term::prim(Prim::NatType)
}

fn bool_type() -> Term {
    Term::prim(Prim::BoolType)
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

fn bin_type(grain: Grain) -> Term {
    Term::prim(Prim::BinType(grain))
}

fn io_type() -> Term {
    Term::prim(Prim::HandleType)
}

fn lst_type(element: Term) -> Term {
    Term::prim(Prim::LstType(element))
}

/// The element shape of a packed binary: its grain's own scalar type.
fn grain_element_type(grain: Grain) -> Term {
    match grain {
        Grain::X => byte_type(),
        Grain::B => bool_type(),
    }
}

impl Lowering {
    /// Erase a constant to its interned atom.
    fn constant(&mut self, constant: curios_ersd::Constant) -> Outcome {
        Outcome::Emitted(curios_ersd::Atom::Constant(self.builder.constant(constant)))
    }

    /// Erase the operands (each against its type, in order) and bind a scalar
    /// operation.
    fn operation(
        &mut self,
        context: &mut Context,
        operation: curios_ersd::Operation,
        operands: &[(&Term, Term)],
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let mut atoms = Vec::with_capacity(operands.len());
        for (term, type_) in operands {
            atoms.push(emitted!(self.walk(context, term, type_, None)?));
        }
        Ok(self.bind(
            hint,
            curios_ersd::Rhs::Operation {
                operation,
                operands: atoms,
            },
        ))
    }

    /// The sequence-family counterpart of [`operation`](Self::operation).
    fn sequence(
        &mut self,
        context: &mut Context,
        operation: curios_ersd::SequenceOp,
        operands: &[(&Term, Term)],
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let mut atoms = Vec::with_capacity(operands.len());
        for (term, type_) in operands {
            atoms.push(emitted!(self.walk(context, term, type_, None)?));
        }
        Ok(self.bind(
            hint,
            curios_ersd::Rhs::Sequence {
                operation,
                operands: atoms,
            },
        ))
    }
}

/// Transcribe one primitive. `expected` is consumed only where a runtime shape
/// must be read off the type — the element type of a list literal.
pub(super) fn erase_prim(
    lowering: &mut Lowering,
    context: &mut Context,
    prim: &Prim,
    expected: &Term,
    hint: Option<&str>,
) -> Result<Outcome, Error> {
    use curios_ersd::{Operation as Op, SequenceOp as Seq};

    /// One scalar-operation arm: each operand erased against one shared
    /// operand type.
    macro_rules! op {
        ($op:expr, $type_:expr, $($operand:expr),+) => {
            lowering.operation(context, $op, &[$(($operand, $type_())),+], hint)
        };
    }

    match prim {
        // Type formers carry nothing to lower.
        Prim::BoolType
        | Prim::NatType
        | Prim::ByteType
        | Prim::IntType
        | Prim::FltType
        | Prim::BinType(_)
        | Prim::LstType(_)
        | Prim::HandleType
        | Prim::CellType(_) => Ok(Outcome::Emitted(lowering.unit())),

        &Prim::Bool(value) => Ok(lowering.constant(curios_ersd::Constant::Bool(value))),
        Prim::BoolAnd(l, r) => op!(Op::BoolAnd, bool_type, l, r),
        Prim::BoolOr(l, r) => op!(Op::BoolOr, bool_type, l, r),
        Prim::BoolXor(l, r) => op!(Op::BoolXor, bool_type, l, r),
        Prim::BoolEql(l, r) => op!(Op::BoolEql, bool_type, l, r),
        Prim::BoolNeq(l, r) => op!(Op::BoolNeq, bool_type, l, r),

        &Prim::Byte(value) => Ok(lowering.constant(curios_ersd::Constant::Byte(value))),
        Prim::ByteToNat(inner) => op!(Op::ByteToNat, byte_type, inner),
        Prim::NatToByte(inner) => op!(Op::NatToByte, nat_type, inner),
        Prim::ByteEql(l, r) => op!(Op::ByteEql, byte_type, l, r),
        Prim::ByteLt(l, r) => op!(Op::ByteLt, byte_type, l, r),
        Prim::ByteLte(l, r) => op!(Op::ByteLte, byte_type, l, r),
        Prim::ByteGt(l, r) => op!(Op::ByteGt, byte_type, l, r),
        Prim::ByteGte(l, r) => op!(Op::ByteGte, byte_type, l, r),

        Prim::Nat(Nat::Zero) => Ok(lowering.constant(curios_ersd::Constant::Nat(0))),
        Prim::Nat(Nat::Succ(spine, inner)) => {
            let spine = narrow_nat(spine)?;
            if matches!(inner.as_ref(), Subterm::Prim(Prim::Nat(Nat::Zero))) {
                return Ok(lowering.constant(curios_ersd::Constant::Nat(spine)));
            }
            let inner_atom = emitted!(lowering.walk(context, inner, &nat_type(), None)?);
            let spine_atom = curios_ersd::Atom::Constant(
                lowering.builder.constant(curios_ersd::Constant::Nat(spine)),
            );
            Ok(lowering.bind(
                hint,
                curios_ersd::Rhs::Operation {
                    operation: Op::NatAdd,
                    operands: vec![spine_atom, inner_atom],
                },
            ))
        }
        Prim::NatEql(l, r) => op!(Op::NatEql, nat_type, l, r),
        Prim::NatNeq(l, r) => op!(Op::NatNeq, nat_type, l, r),
        Prim::NatAdd(l, r) => op!(Op::NatAdd, nat_type, l, r),
        Prim::NatSub(l, r) => op!(Op::NatSub, nat_type, l, r),
        Prim::NatMul(l, r) => op!(Op::NatMul, nat_type, l, r),
        Prim::NatLt(l, r) => op!(Op::NatLt, nat_type, l, r),
        Prim::NatDiv(l, r) => op!(Op::NatDiv, nat_type, l, r),
        Prim::NatRem(l, r) => op!(Op::NatRem, nat_type, l, r),
        Prim::NatGt(l, r) => op!(Op::NatGt, nat_type, l, r),
        Prim::NatLte(l, r) => op!(Op::NatLte, nat_type, l, r),
        Prim::NatGte(l, r) => op!(Op::NatGte, nat_type, l, r),
        Prim::NatAnd(l, r) => op!(Op::NatAnd, nat_type, l, r),
        Prim::NatOr(l, r) => op!(Op::NatOr, nat_type, l, r),
        Prim::NatXor(l, r) => op!(Op::NatXor, nat_type, l, r),
        Prim::NatShl(l, r) => op!(Op::NatShl, nat_type, l, r),
        Prim::NatShr(l, r) => op!(Op::NatShr, nat_type, l, r),
        Prim::NatRotl(l, r) => op!(Op::NatRotl, nat_type, l, r),
        Prim::NatRotr(l, r) => op!(Op::NatRotr, nat_type, l, r),
        Prim::NatClz(i) => op!(Op::NatClz, nat_type, i),
        Prim::NatCtz(i) => op!(Op::NatCtz, nat_type, i),
        Prim::NatPopcnt(i) => op!(Op::NatPopcnt, nat_type, i),

        Prim::Int(value) => Ok(lowering.constant(curios_ersd::Constant::Int(narrow_int(value)?))),
        Prim::IntEql(l, r) => op!(Op::IntEql, int_type, l, r),
        Prim::IntNeq(l, r) => op!(Op::IntNeq, int_type, l, r),
        Prim::IntAdd(l, r) => op!(Op::IntAdd, int_type, l, r),
        Prim::IntSub(l, r) => op!(Op::IntSub, int_type, l, r),
        Prim::IntMul(l, r) => op!(Op::IntMul, int_type, l, r),
        Prim::IntDiv(l, r) => op!(Op::IntDiv, int_type, l, r),
        Prim::IntRem(l, r) => op!(Op::IntRem, int_type, l, r),
        Prim::IntLt(l, r) => op!(Op::IntLt, int_type, l, r),
        Prim::IntGt(l, r) => op!(Op::IntGt, int_type, l, r),
        Prim::IntLte(l, r) => op!(Op::IntLte, int_type, l, r),
        Prim::IntGte(l, r) => op!(Op::IntGte, int_type, l, r),
        Prim::IntAnd(l, r) => op!(Op::IntAnd, int_type, l, r),
        Prim::IntOr(l, r) => op!(Op::IntOr, int_type, l, r),
        Prim::IntXor(l, r) => op!(Op::IntXor, int_type, l, r),
        Prim::IntShl(l, r) => op!(Op::IntShl, int_type, l, r),
        Prim::IntShr(l, r) => op!(Op::IntShr, int_type, l, r),
        Prim::IntRotl(l, r) => op!(Op::IntRotl, int_type, l, r),
        Prim::IntRotr(l, r) => op!(Op::IntRotr, int_type, l, r),
        Prim::IntClz(i) => op!(Op::IntClz, int_type, i),
        Prim::IntCtz(i) => op!(Op::IntCtz, int_type, i),
        Prim::IntPopcnt(i) => op!(Op::IntPopcnt, int_type, i),

        &Prim::Flt(value) => Ok(lowering.constant(curios_ersd::Constant::Flt(value))),
        Prim::FltAdd(l, r) => op!(Op::FltAdd, flt_type, l, r),
        Prim::FltSub(l, r) => op!(Op::FltSub, flt_type, l, r),
        Prim::FltMul(l, r) => op!(Op::FltMul, flt_type, l, r),
        Prim::FltDiv(l, r) => op!(Op::FltDiv, flt_type, l, r),
        Prim::FltRem(l, r) => op!(Op::FltRem, flt_type, l, r),
        Prim::FltEql(l, r) => op!(Op::FltEql, flt_type, l, r),
        Prim::FltNeq(l, r) => op!(Op::FltNeq, flt_type, l, r),
        Prim::FltLt(l, r) => op!(Op::FltLt, flt_type, l, r),
        Prim::FltGt(l, r) => op!(Op::FltGt, flt_type, l, r),
        Prim::FltLte(l, r) => op!(Op::FltLte, flt_type, l, r),
        Prim::FltGte(l, r) => op!(Op::FltGte, flt_type, l, r),
        Prim::FltMin(l, r) => op!(Op::FltMin, flt_type, l, r),
        Prim::FltMax(l, r) => op!(Op::FltMax, flt_type, l, r),
        Prim::FltCopysign(l, r) => op!(Op::FltCopysign, flt_type, l, r),
        Prim::FltNeg(inner) => op!(Op::FltNeg, flt_type, inner),
        Prim::FltAbs(inner) => op!(Op::FltAbs, flt_type, inner),
        Prim::FltSqrt(inner) => op!(Op::FltSqrt, flt_type, inner),
        Prim::FltFloor(inner) => op!(Op::FltFloor, flt_type, inner),
        Prim::FltCeil(inner) => op!(Op::FltCeil, flt_type, inner),
        Prim::FltTrunc(inner) => op!(Op::FltTrunc, flt_type, inner),
        Prim::FltNearest(inner) => op!(Op::FltNearest, flt_type, inner),

        Prim::NatToInt(inner) => op!(Op::NatToInt, nat_type, inner),
        Prim::NatToFlt(inner) => op!(Op::NatToFlt, nat_type, inner),
        Prim::IntToNat(inner) => op!(Op::IntToNat, int_type, inner),
        Prim::IntToFlt(inner) => op!(Op::IntToFlt, int_type, inner),
        Prim::FltToNat(inner) => op!(Op::FltToNat, flt_type, inner),
        Prim::FltToInt(inner) => op!(Op::FltToInt, flt_type, inner),
        Prim::FltToLeBytes(inner) => op!(Op::FltToLeBytes, flt_type, inner),
        Prim::FltOfLeBytes(inner) => lowering.operation(
            context,
            Op::FltOfLeBytes,
            &[(inner, bin_type(Grain::X))],
            hint,
        ),

        Prim::Bin(grain, value) => {
            Ok(lowering.constant(curios_ersd::Constant::Bin(*grain, value.clone())))
        }
        Prim::BinLen(grain, bin) => lowering.sequence(
            context,
            Seq::BinLen(*grain),
            &[(bin, bin_type(*grain))],
            hint,
        ),
        Prim::BinEql(grain, l, r) => lowering.sequence(
            context,
            Seq::BinEql(*grain),
            &[(l, bin_type(*grain)), (r, bin_type(*grain))],
            hint,
        ),
        Prim::BinGet(grain, bin, index) => lowering.sequence(
            context,
            Seq::BinGet(*grain),
            &[(bin, bin_type(*grain)), (index, nat_type())],
            hint,
        ),
        Prim::BinSlice(grain, bin, start, end) => lowering.sequence(
            context,
            Seq::BinSlice(*grain),
            &[
                (bin, bin_type(*grain)),
                (start, nat_type()),
                (end, nat_type()),
            ],
            hint,
        ),
        Prim::BinAppend(grain, bin, element) => lowering.sequence(
            context,
            Seq::BinAppend(*grain),
            &[
                (bin, bin_type(*grain)),
                (element, grain_element_type(*grain)),
            ],
            hint,
        ),
        Prim::BinConcat(grain, operands) => {
            let pairs = operands
                .iter()
                .map(|operand| (operand, bin_type(*grain)))
                .collect::<Vec<_>>();
            lowering.sequence(context, Seq::BinConcat(*grain), &pairs, hint)
        }

        Prim::Lst(elements) => {
            // Elaborate already checked this literal against a list type; the
            // element type is re-derived only to lower the elements.
            let element_type = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
                Subterm::Prim(Prim::LstType(element_type)) => element_type,
                _ => unreachable!("erase: list literal checked against non-list type"),
            };
            let pairs = elements
                .iter()
                .map(|element| (element, element_type.clone()))
                .collect::<Vec<_>>();
            lowering.sequence(context, Seq::LstBuild, &pairs, hint)
        }
        Prim::LstLen(element_type, list) => lowering.sequence(
            context,
            Seq::LstLen,
            &[(list, lst_type(element_type.clone()))],
            hint,
        ),
        Prim::LstGet(element_type, list, index) => lowering.sequence(
            context,
            Seq::LstGet,
            &[(list, lst_type(element_type.clone())), (index, nat_type())],
            hint,
        ),
        Prim::LstSlice(element_type, list, start, end) => lowering.sequence(
            context,
            Seq::LstSlice,
            &[
                (list, lst_type(element_type.clone())),
                (start, nat_type()),
                (end, nat_type()),
            ],
            hint,
        ),
        Prim::LstAppend(element_type, list, element) => lowering.sequence(
            context,
            Seq::LstAppend,
            &[
                (list, lst_type(element_type.clone())),
                (element, element_type.clone()),
            ],
            hint,
        ),
        Prim::LstConcat(element_type, operands) => {
            let pairs = operands
                .iter()
                .map(|operand| (operand, lst_type(element_type.clone())))
                .collect::<Vec<_>>();
            lowering.sequence(context, Seq::LstConcat, &pairs, hint)
        }
        Prim::LstMap(domain, codomain, list, mapper) => {
            let list_atom =
                emitted!(lowering.walk(context, list, &lst_type(domain.clone()), None)?);
            let mapper_type = Term::func_type(
                [(context.fresh(Some("x")), domain.clone())],
                codomain.clone(),
            );
            let mapper_atom = emitted!(lowering.walk(context, mapper, &mapper_type, None)?);
            Ok(lowering.bind(
                hint,
                curios_ersd::Rhs::Intrinsic {
                    intrinsic: curios_ersd::Intrinsic::LstMap,
                    operands: vec![list_atom, mapper_atom],
                },
            ))
        }

        &Prim::Handle(token) => Ok(lowering.constant(curios_ersd::Constant::Handle(token))),
        Prim::HandleEql(l, r) => op!(Op::HandleEql, io_type, l, r),
        // A process exit never yields a value: erase the code, then report the
        // terminator that seals this block. Code after it is dead and is never
        // erased.
        Prim::Exit(_, code) => {
            let code_atom = emitted!(lowering.walk(context, code, &nat_type(), None)?);
            Ok(Outcome::Diverged(curios_ersd::Terminator::Exit(code_atom)))
        }

        // A store-described host call: each operand erases against its wire
        // type, read off the same signature elaboration checked it with.
        Prim::Foreign(function, arguments) => {
            let mut atoms = Vec::with_capacity(arguments.len());
            for (argument, (_, wire_type)) in arguments.iter().zip(&function.signature.params) {
                atoms.push(emitted!(lowering.walk(
                    context,
                    argument,
                    &wire_term(wire_type),
                    None
                )?));
            }
            let foreign = lowering.builder.foreign(std::sync::Arc::clone(function));
            Ok(lowering.bind(
                hint,
                curios_ersd::Rhs::Foreign {
                    foreign,
                    operands: atoms,
                },
            ))
        }

        Prim::Cell(type_, initial) => {
            let initial_atom = emitted!(lowering.walk(context, initial, type_, None)?);
            Ok(lowering.bind(
                hint,
                curios_ersd::Rhs::Cell {
                    operation: curios_ersd::CellOperation::New,
                    operands: vec![initial_atom],
                },
            ))
        }
        Prim::CellSet(type_, cell, value) => {
            let cell_type: Term = Subterm::Prim(Prim::CellType(type_.clone())).into();
            let cell_atom = emitted!(lowering.walk(context, cell, &cell_type, None)?);
            let value_atom = emitted!(lowering.walk(context, value, type_, None)?);
            Ok(lowering.bind(
                hint,
                curios_ersd::Rhs::Cell {
                    operation: curios_ersd::CellOperation::Set,
                    operands: vec![cell_atom, value_atom],
                },
            ))
        }
        Prim::CellGet(type_, cell) => {
            let cell_type: Term = Subterm::Prim(Prim::CellType(type_.clone())).into();
            let cell_atom = emitted!(lowering.walk(context, cell, &cell_type, None)?);
            Ok(lowering.bind(
                hint,
                curios_ersd::Rhs::Cell {
                    operation: curios_ersd::CellOperation::Get,
                    operands: vec![cell_atom],
                },
            ))
        }
    }
}
