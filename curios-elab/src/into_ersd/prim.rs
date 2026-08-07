//! The primitive transcription: each Core primitive to its arena identity, shape for shape.
//!
//! No carrier is chosen here: `Bool` values and operations stay `Bool`-shaped, `Byte` stays `Byte`, `Handle` stays an opaque handle constant, and a packed binary's element is its grain's shape (`Byte` for `X`, `Bool` for `B`) — every collapse onto a runtime carrier belongs to the lowering out of the representation. Unbounded type-level numerals narrow to the exact 32-bit domains here (the numeric law's Core border), with overflow reported as an error, never wrapped.

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

fn handle_type() -> Term {
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

    /// Erase the operands (each against its type, in order) and bind a scalar operation.
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

    /// Erase a description into the zero-argument closure that *is* its runtime representation.
    ///
    /// `body` runs with a fresh block open, so whatever it erases lands inside the thunk rather than at the construction site — which is what makes an `Io` a description: bound once and forced twice, it performs twice, and substituting a definition for its name never changes behavior. Its `Outcome` seals the block, so a diverging body (a process exit) keeps its own terminator instead of returning.
    ///
    /// What belongs inside is the *performance*, not the operands. `IoPure` erases its value eagerly and hands the closure an atom to return; `IoBind` puts the two forces and the continuation's application inside, because those are what must not happen until the description is run.
    fn thunk(
        &mut self,
        hint: Option<&str>,
        body: impl FnOnce(&mut Self) -> Result<Outcome, Error>,
    ) -> Result<Outcome, Error> {
        let function = self.builder.reserve_function();
        self.builder.open_block();
        let outcome = body(self)?;
        let block = self.seal(outcome);
        self.builder
            .define_function(function, hint.map(str::to_string), Vec::new(), block);
        self.builder.let_functions(vec![function]);

        Ok(Outcome::Emitted(curios_ersd::Atom::Function(function)))
    }

    /// Force a description: apply the closure it erased to at zero arguments.
    fn force(&mut self, action: curios_ersd::Atom) -> Outcome {
        self.bind(
            None,
            curios_ersd::Rhs::Apply {
                callee: action,
                arguments: Vec::new(),
            },
        )
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

/// Transcribe one primitive. `expected` is consumed only where a runtime shape must be read off the type — the element type of a list literal.
pub(super) fn erase_prim(
    lowering: &mut Lowering,
    context: &mut Context,
    prim: &Prim,
    expected: &Term,
    hint: Option<&str>,
) -> Result<Outcome, Error> {
    /// One scalar-operation arm: each operand erased against one shared operand type.
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
        | Prim::CellType(_)
        | Prim::IoType(_) => Ok(Outcome::Emitted(lowering.unit())),

        &Prim::Bool(value) => Ok(lowering.constant(curios_ersd::Constant::Bool(value))),
        Prim::BoolAnd(l, r) => op!(curios_ersd::Operation::BoolAnd, bool_type, l, r),
        Prim::BoolOr(l, r) => op!(curios_ersd::Operation::BoolOr, bool_type, l, r),
        Prim::BoolXor(l, r) => op!(curios_ersd::Operation::BoolXor, bool_type, l, r),
        Prim::BoolEql(l, r) => op!(curios_ersd::Operation::BoolEql, bool_type, l, r),
        Prim::BoolNeq(l, r) => op!(curios_ersd::Operation::BoolNeq, bool_type, l, r),

        &Prim::Byte(value) => Ok(lowering.constant(curios_ersd::Constant::Byte(value))),
        Prim::ByteToNat(inner) => op!(curios_ersd::Operation::ByteToNat, byte_type, inner),
        Prim::NatToByte(inner) => op!(curios_ersd::Operation::NatToByte, nat_type, inner),
        Prim::ByteEql(l, r) => op!(curios_ersd::Operation::ByteEql, byte_type, l, r),
        Prim::ByteLt(l, r) => op!(curios_ersd::Operation::ByteLt, byte_type, l, r),
        Prim::ByteLte(l, r) => op!(curios_ersd::Operation::ByteLte, byte_type, l, r),
        Prim::ByteGt(l, r) => op!(curios_ersd::Operation::ByteGt, byte_type, l, r),
        Prim::ByteGte(l, r) => op!(curios_ersd::Operation::ByteGte, byte_type, l, r),

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
                    operation: curios_ersd::Operation::NatAdd,
                    operands: vec![spine_atom, inner_atom],
                },
            ))
        }
        Prim::NatEql(l, r) => op!(curios_ersd::Operation::NatEql, nat_type, l, r),
        Prim::NatNeq(l, r) => op!(curios_ersd::Operation::NatNeq, nat_type, l, r),
        Prim::NatAdd(l, r) => op!(curios_ersd::Operation::NatAdd, nat_type, l, r),
        Prim::NatSub(l, r) => op!(curios_ersd::Operation::NatSub, nat_type, l, r),
        Prim::NatMul(l, r) => op!(curios_ersd::Operation::NatMul, nat_type, l, r),
        Prim::NatLt(l, r) => op!(curios_ersd::Operation::NatLt, nat_type, l, r),
        Prim::NatDiv(l, r) => op!(curios_ersd::Operation::NatDiv, nat_type, l, r),
        Prim::NatRem(l, r) => op!(curios_ersd::Operation::NatRem, nat_type, l, r),
        Prim::NatGt(l, r) => op!(curios_ersd::Operation::NatGt, nat_type, l, r),
        Prim::NatLte(l, r) => op!(curios_ersd::Operation::NatLte, nat_type, l, r),
        Prim::NatGte(l, r) => op!(curios_ersd::Operation::NatGte, nat_type, l, r),
        Prim::NatAnd(l, r) => op!(curios_ersd::Operation::NatAnd, nat_type, l, r),
        Prim::NatOr(l, r) => op!(curios_ersd::Operation::NatOr, nat_type, l, r),
        Prim::NatXor(l, r) => op!(curios_ersd::Operation::NatXor, nat_type, l, r),
        Prim::NatShl(l, r) => op!(curios_ersd::Operation::NatShl, nat_type, l, r),
        Prim::NatShr(l, r) => op!(curios_ersd::Operation::NatShr, nat_type, l, r),
        Prim::NatRotl(l, r) => op!(curios_ersd::Operation::NatRotl, nat_type, l, r),
        Prim::NatRotr(l, r) => op!(curios_ersd::Operation::NatRotr, nat_type, l, r),
        Prim::NatClz(i) => op!(curios_ersd::Operation::NatClz, nat_type, i),
        Prim::NatCtz(i) => op!(curios_ersd::Operation::NatCtz, nat_type, i),
        Prim::NatPopcnt(i) => op!(curios_ersd::Operation::NatPopcnt, nat_type, i),

        Prim::Int(value) => Ok(lowering.constant(curios_ersd::Constant::Int(narrow_int(value)?))),
        Prim::IntEql(l, r) => op!(curios_ersd::Operation::IntEql, int_type, l, r),
        Prim::IntNeq(l, r) => op!(curios_ersd::Operation::IntNeq, int_type, l, r),
        Prim::IntAdd(l, r) => op!(curios_ersd::Operation::IntAdd, int_type, l, r),
        Prim::IntSub(l, r) => op!(curios_ersd::Operation::IntSub, int_type, l, r),
        Prim::IntMul(l, r) => op!(curios_ersd::Operation::IntMul, int_type, l, r),
        Prim::IntDiv(l, r) => op!(curios_ersd::Operation::IntDiv, int_type, l, r),
        Prim::IntRem(l, r) => op!(curios_ersd::Operation::IntRem, int_type, l, r),
        Prim::IntLt(l, r) => op!(curios_ersd::Operation::IntLt, int_type, l, r),
        Prim::IntGt(l, r) => op!(curios_ersd::Operation::IntGt, int_type, l, r),
        Prim::IntLte(l, r) => op!(curios_ersd::Operation::IntLte, int_type, l, r),
        Prim::IntGte(l, r) => op!(curios_ersd::Operation::IntGte, int_type, l, r),
        Prim::IntAnd(l, r) => op!(curios_ersd::Operation::IntAnd, int_type, l, r),
        Prim::IntOr(l, r) => op!(curios_ersd::Operation::IntOr, int_type, l, r),
        Prim::IntXor(l, r) => op!(curios_ersd::Operation::IntXor, int_type, l, r),
        Prim::IntShl(l, r) => op!(curios_ersd::Operation::IntShl, int_type, l, r),
        Prim::IntShr(l, r) => op!(curios_ersd::Operation::IntShr, int_type, l, r),
        Prim::IntRotl(l, r) => op!(curios_ersd::Operation::IntRotl, int_type, l, r),
        Prim::IntRotr(l, r) => op!(curios_ersd::Operation::IntRotr, int_type, l, r),
        Prim::IntClz(i) => op!(curios_ersd::Operation::IntClz, int_type, i),
        Prim::IntCtz(i) => op!(curios_ersd::Operation::IntCtz, int_type, i),
        Prim::IntPopcnt(i) => op!(curios_ersd::Operation::IntPopcnt, int_type, i),

        &Prim::Flt(value) => Ok(lowering.constant(curios_ersd::Constant::Flt(value))),
        Prim::FltAdd(l, r) => op!(curios_ersd::Operation::FltAdd, flt_type, l, r),
        Prim::FltSub(l, r) => op!(curios_ersd::Operation::FltSub, flt_type, l, r),
        Prim::FltMul(l, r) => op!(curios_ersd::Operation::FltMul, flt_type, l, r),
        Prim::FltDiv(l, r) => op!(curios_ersd::Operation::FltDiv, flt_type, l, r),
        Prim::FltRem(l, r) => op!(curios_ersd::Operation::FltRem, flt_type, l, r),
        Prim::FltEql(l, r) => op!(curios_ersd::Operation::FltEql, flt_type, l, r),
        Prim::FltNeq(l, r) => op!(curios_ersd::Operation::FltNeq, flt_type, l, r),
        Prim::FltLt(l, r) => op!(curios_ersd::Operation::FltLt, flt_type, l, r),
        Prim::FltGt(l, r) => op!(curios_ersd::Operation::FltGt, flt_type, l, r),
        Prim::FltLte(l, r) => op!(curios_ersd::Operation::FltLte, flt_type, l, r),
        Prim::FltGte(l, r) => op!(curios_ersd::Operation::FltGte, flt_type, l, r),
        Prim::FltMin(l, r) => op!(curios_ersd::Operation::FltMin, flt_type, l, r),
        Prim::FltMax(l, r) => op!(curios_ersd::Operation::FltMax, flt_type, l, r),
        Prim::FltCopysign(l, r) => op!(curios_ersd::Operation::FltCopysign, flt_type, l, r),
        Prim::FltNeg(inner) => op!(curios_ersd::Operation::FltNeg, flt_type, inner),
        Prim::FltAbs(inner) => op!(curios_ersd::Operation::FltAbs, flt_type, inner),
        Prim::FltSqrt(inner) => op!(curios_ersd::Operation::FltSqrt, flt_type, inner),
        Prim::FltFloor(inner) => op!(curios_ersd::Operation::FltFloor, flt_type, inner),
        Prim::FltCeil(inner) => op!(curios_ersd::Operation::FltCeil, flt_type, inner),
        Prim::FltTrunc(inner) => op!(curios_ersd::Operation::FltTrunc, flt_type, inner),
        Prim::FltNearest(inner) => op!(curios_ersd::Operation::FltNearest, flt_type, inner),

        Prim::NatToInt(inner) => op!(curios_ersd::Operation::NatToInt, nat_type, inner),
        Prim::NatToFlt(inner) => op!(curios_ersd::Operation::NatToFlt, nat_type, inner),
        Prim::IntToNat(inner) => op!(curios_ersd::Operation::IntToNat, int_type, inner),
        Prim::IntToFlt(inner) => op!(curios_ersd::Operation::IntToFlt, int_type, inner),
        Prim::FltToNat(inner) => op!(curios_ersd::Operation::FltToNat, flt_type, inner),
        Prim::FltToInt(inner) => op!(curios_ersd::Operation::FltToInt, flt_type, inner),
        Prim::FltToLeBytes(inner) => op!(curios_ersd::Operation::FltToLeBytes, flt_type, inner),
        Prim::FltOfLeBytes(inner) => lowering.operation(
            context,
            curios_ersd::Operation::FltOfLeBytes,
            &[(inner, bin_type(Grain::X))],
            hint,
        ),

        Prim::Bin(grain, value) => {
            Ok(lowering.constant(curios_ersd::Constant::Bin(*grain, value.clone())))
        }
        Prim::BinLen(grain, bin) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::BinLen(*grain),
            &[(bin, bin_type(*grain))],
            hint,
        ),
        Prim::BinEql(grain, l, r) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::BinEql(*grain),
            &[(l, bin_type(*grain)), (r, bin_type(*grain))],
            hint,
        ),
        Prim::BinGet(grain, bin, index) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::BinGet(*grain),
            &[(bin, bin_type(*grain)), (index, nat_type())],
            hint,
        ),
        Prim::BinSlice(grain, bin, start, end) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::BinSlice(*grain),
            &[
                (bin, bin_type(*grain)),
                (start, nat_type()),
                (end, nat_type()),
            ],
            hint,
        ),
        Prim::BinAppend(grain, bin, element) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::BinAppend(*grain),
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
            lowering.sequence(
                context,
                curios_ersd::SequenceOp::BinConcat(*grain),
                &pairs,
                hint,
            )
        }

        Prim::Lst(_, elements) => {
            // Elaborate already checked this literal against a list type; the element type is re-derived only to lower the elements.
            let element_type = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
                Subterm::Prim(Prim::LstType(element_type)) => element_type,
                _ => unreachable!("erase: list literal checked against non-list type"),
            };
            let pairs = elements
                .iter()
                .map(|element| (element, element_type.clone()))
                .collect::<Vec<_>>();
            lowering.sequence(context, curios_ersd::SequenceOp::LstBuild, &pairs, hint)
        }
        Prim::LstLen(element_type, list) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::LstLen,
            &[(list, lst_type(element_type.clone()))],
            hint,
        ),
        Prim::LstGet(element_type, list, index) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::LstGet,
            &[(list, lst_type(element_type.clone())), (index, nat_type())],
            hint,
        ),
        Prim::LstSlice(element_type, list, start, end) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::LstSlice,
            &[
                (list, lst_type(element_type.clone())),
                (start, nat_type()),
                (end, nat_type()),
            ],
            hint,
        ),
        Prim::LstAppend(element_type, list, element) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::LstAppend,
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
            lowering.sequence(context, curios_ersd::SequenceOp::LstConcat, &pairs, hint)
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
        Prim::HandleEql(l, r) => op!(curios_ersd::Operation::HandleEql, handle_type, l, r),
        // Every operation the host performs is typed `Io`, so every one erases to a thunk: the
        // operands are computed where the description is *built*, and the operation itself happens
        // only when the description is forced. Nothing below changes what the host call is — only
        // where it sits relative to the closure boundary.

        // A process exit never yields a value, so the thunk's block is sealed by the terminator rather than by a return. Code after the *force* is dead; code after the construction is not.
        Prim::Exit(code) => {
            let code_atom = emitted!(lowering.walk(context, code, &nat_type(), None)?);
            lowering.thunk(hint, move |_| {
                Ok(Outcome::Diverged(curios_ersd::Terminator::Exit(code_atom)))
            })
        }

        // A store-described host call: each operand erases against its wire type, read off the same signature elaboration checked it with.
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
            lowering.thunk(hint, move |lowering| {
                Ok(lowering.bind(
                    None,
                    curios_ersd::Rhs::Foreign {
                        foreign,
                        operands: atoms,
                    },
                ))
            })
        }

        Prim::Cell(type_, initial) => {
            let initial_atom = emitted!(lowering.walk(context, initial, type_, None)?);
            lowering.thunk(hint, move |lowering| {
                Ok(lowering.bind(
                    None,
                    curios_ersd::Rhs::Cell {
                        operation: curios_ersd::CellOperation::New,
                        operands: vec![initial_atom],
                    },
                ))
            })
        }
        Prim::CellSet(type_, cell, value) => {
            let cell_type: Term = Subterm::Prim(Prim::CellType(type_.clone())).into();
            let cell_atom = emitted!(lowering.walk(context, cell, &cell_type, None)?);
            let value_atom = emitted!(lowering.walk(context, value, type_, None)?);
            lowering.thunk(hint, move |lowering| {
                Ok(lowering.bind(
                    None,
                    curios_ersd::Rhs::Cell {
                        operation: curios_ersd::CellOperation::Set,
                        operands: vec![cell_atom, value_atom],
                    },
                ))
            })
        }
        Prim::CellGet(type_, cell) => {
            let cell_type: Term = Subterm::Prim(Prim::CellType(type_.clone())).into();
            let cell_atom = emitted!(lowering.walk(context, cell, &cell_type, None)?);
            lowering.thunk(hint, move |lowering| {
                Ok(lowering.bind(
                    None,
                    curios_ersd::Rhs::Cell {
                        operation: curios_ersd::CellOperation::Get,
                        operands: vec![cell_atom],
                    },
                ))
            })
        }

        // The description that performs nothing: a closure yielding an already-computed value.
        //
        // The operand is erased at the construction site like every other primitive's, not inside the thunk. The language is eager: `/sys/Io/pure` is an ordinary call-by-value wrapper, so a surface `Io/pure(e)` has evaluated `e` before this node exists at all, and erasing the operand inside the closure would delay nothing while making this one arm's evaluation order differ from the rest of the roster. What delays a program's effect is `IoBind`.
        Prim::IoPure(type_, value) => {
            let value = emitted!(lowering.walk(context, value, type_, None)?);
            lowering.thunk(hint, move |_| Ok(Outcome::Emitted(value)))
        }
        // The description that performs `action`, then the description `continuation` computes from its result. Both forces are zero-argument applications of the closures the operands erased to.
        Prim::IoBind(from, to, action, continuation) => lowering.thunk(hint, |lowering| {
            let io_from: Term = Subterm::Prim(Prim::IoType(from.clone())).into();
            let action_atom = emitted!(lowering.walk(context, action, &io_from, None)?);
            let result = emitted!(lowering.force(action_atom));

            let io_to: Term = Subterm::Prim(Prim::IoType(to.clone())).into();
            let continuation_type =
                Term::func_type([(context.fresh(Some("x")), from.clone())], io_to);
            let continuation_atom =
                emitted!(lowering.walk(context, continuation, &continuation_type, None)?);
            let next = emitted!(lowering.bind(
                None,
                curios_ersd::Rhs::Apply {
                    callee: continuation_atom,
                    arguments: vec![result],
                },
            ));

            Ok(lowering.force(next))
        }),
    }
}
