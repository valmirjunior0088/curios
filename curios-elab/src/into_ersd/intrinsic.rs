//! The intrinsic transcription: each Core intrinsic to its arena identity, shape for shape.
//!
//! No carrier is chosen here: `Bool` values and operations stay `Bool`-shaped, `Byte` stays `Byte`, `Handle` stays an opaque handle constant, and a packed binary's element is its grain's shape (`Byte` for `X`, `Bool` for `B`) — every collapse onto a runtime carrier belongs to the lowering out of the representation. Unbounded type-level numerals narrow to the exact 32-bit domains here (the numeric law's Core border), with overflow reported as an error, never wrapped.

use {
    super::{
        BigUint, Context, Error, Intrinsic, Lowering, Nat, Outcome, Subterm, Term, ToPrimitive,
        emitted, reduce_with,
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
    Term::intrinsic(Intrinsic::NatType)
}

fn bool_type() -> Term {
    Term::intrinsic(Intrinsic::BoolType)
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

fn bin_type(grain: Grain) -> Term {
    Term::intrinsic(Intrinsic::BinType(grain))
}

fn handle_type() -> Term {
    Term::intrinsic(Intrinsic::HandleType)
}

fn list_type(element: Term) -> Term {
    Term::intrinsic(Intrinsic::ListType(element))
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
    pub(super) fn thunk(
        &mut self,
        hint: Option<&str>,
        body: impl FnOnce(&mut Self) -> Result<Outcome, Error>,
    ) -> Result<Outcome, Error> {
        let function = self.builder.reserve_function();
        // The description's own name, and the owner of anything the performance mints inside it.
        let name = hint.map(str::to_string).or_else(|| self.derived_hint());
        self.builder.open_block();
        let outcome = match name.clone() {
            Some(name) => self.with_owner(name, body)?,
            None => body(self)?,
        };
        let block = self.seal(outcome);
        self.builder
            .define_function(function, name, Vec::new(), block);
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

/// Transcribe one intrinsic. `expected` is consumed only where a runtime shape must be read off the type — the element type of a list literal.
pub(super) fn erase_intrinsic(
    lowering: &mut Lowering,
    context: &mut Context,
    intrinsic: &Intrinsic,
    expected: &Term,
    hint: Option<&str>,
) -> Result<Outcome, Error> {
    /// One scalar-operation arm: each operand erased against one shared operand type.
    macro_rules! op {
        ($op:expr, $type_:expr, $($operand:expr),+) => {
            lowering.operation(context, $op, &[$(($operand, $type_())),+], hint)
        };
    }

    match intrinsic {
        // Type formers carry nothing to lower.
        Intrinsic::BoolType
        | Intrinsic::NatType
        | Intrinsic::ByteType
        | Intrinsic::IntType
        | Intrinsic::FltType
        | Intrinsic::BinType(_)
        | Intrinsic::ListType(_)
        | Intrinsic::HandleType
        | Intrinsic::CellType(_)
        | Intrinsic::IoType(_) => Ok(Outcome::Emitted(lowering.unit())),

        &Intrinsic::Bool(value) => Ok(lowering.constant(curios_ersd::Constant::Bool(value))),
        Intrinsic::BoolAnd(l, r) => op!(curios_ersd::Operation::BoolAnd, bool_type, l, r),
        Intrinsic::BoolOr(l, r) => op!(curios_ersd::Operation::BoolOr, bool_type, l, r),
        Intrinsic::BoolXor(l, r) => op!(curios_ersd::Operation::BoolXor, bool_type, l, r),
        Intrinsic::BoolEql(l, r) => op!(curios_ersd::Operation::BoolEql, bool_type, l, r),
        Intrinsic::BoolNeq(l, r) => op!(curios_ersd::Operation::BoolNeq, bool_type, l, r),

        &Intrinsic::Byte(value) => Ok(lowering.constant(curios_ersd::Constant::Byte(value))),
        Intrinsic::ByteToNat(inner) => op!(curios_ersd::Operation::ByteToNat, byte_type, inner),
        Intrinsic::NatToByte(inner) => op!(curios_ersd::Operation::NatToByte, nat_type, inner),
        Intrinsic::ByteEql(l, r) => op!(curios_ersd::Operation::ByteEql, byte_type, l, r),
        Intrinsic::ByteLt(l, r) => op!(curios_ersd::Operation::ByteLt, byte_type, l, r),
        Intrinsic::ByteLte(l, r) => op!(curios_ersd::Operation::ByteLte, byte_type, l, r),
        Intrinsic::ByteGt(l, r) => op!(curios_ersd::Operation::ByteGt, byte_type, l, r),
        Intrinsic::ByteGte(l, r) => op!(curios_ersd::Operation::ByteGte, byte_type, l, r),

        Intrinsic::Nat(Nat::Zero) => Ok(lowering.constant(curios_ersd::Constant::Nat(0))),
        Intrinsic::Nat(Nat::Succ(spine, inner)) => {
            let spine = narrow_nat(spine)?;
            if matches!(
                inner.as_ref(),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) {
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
        Intrinsic::NatEql(l, r) => op!(curios_ersd::Operation::NatEql, nat_type, l, r),
        Intrinsic::NatNeq(l, r) => op!(curios_ersd::Operation::NatNeq, nat_type, l, r),
        Intrinsic::NatAdd(l, r) => op!(curios_ersd::Operation::NatAdd, nat_type, l, r),
        Intrinsic::NatSub(l, r) => op!(curios_ersd::Operation::NatSub, nat_type, l, r),
        Intrinsic::NatMul(l, r) => op!(curios_ersd::Operation::NatMul, nat_type, l, r),
        Intrinsic::NatLt(l, r) => op!(curios_ersd::Operation::NatLt, nat_type, l, r),
        Intrinsic::NatDiv(l, r) => op!(curios_ersd::Operation::NatDiv, nat_type, l, r),
        Intrinsic::NatRem(l, r) => op!(curios_ersd::Operation::NatRem, nat_type, l, r),
        Intrinsic::NatGt(l, r) => op!(curios_ersd::Operation::NatGt, nat_type, l, r),
        Intrinsic::NatLte(l, r) => op!(curios_ersd::Operation::NatLte, nat_type, l, r),
        Intrinsic::NatGte(l, r) => op!(curios_ersd::Operation::NatGte, nat_type, l, r),
        Intrinsic::NatAnd(l, r) => op!(curios_ersd::Operation::NatAnd, nat_type, l, r),
        Intrinsic::NatOr(l, r) => op!(curios_ersd::Operation::NatOr, nat_type, l, r),
        Intrinsic::NatXor(l, r) => op!(curios_ersd::Operation::NatXor, nat_type, l, r),
        Intrinsic::NatShl(l, r) => op!(curios_ersd::Operation::NatShl, nat_type, l, r),
        Intrinsic::NatShr(l, r) => op!(curios_ersd::Operation::NatShr, nat_type, l, r),
        Intrinsic::NatRotl(l, r) => op!(curios_ersd::Operation::NatRotl, nat_type, l, r),
        Intrinsic::NatRotr(l, r) => op!(curios_ersd::Operation::NatRotr, nat_type, l, r),
        Intrinsic::NatClz(i) => op!(curios_ersd::Operation::NatClz, nat_type, i),
        Intrinsic::NatCtz(i) => op!(curios_ersd::Operation::NatCtz, nat_type, i),
        Intrinsic::NatPopcnt(i) => op!(curios_ersd::Operation::NatPopcnt, nat_type, i),

        Intrinsic::Int(value) => {
            Ok(lowering.constant(curios_ersd::Constant::Int(narrow_int(value)?)))
        }
        Intrinsic::IntEql(l, r) => op!(curios_ersd::Operation::IntEql, int_type, l, r),
        Intrinsic::IntNeq(l, r) => op!(curios_ersd::Operation::IntNeq, int_type, l, r),
        Intrinsic::IntAdd(l, r) => op!(curios_ersd::Operation::IntAdd, int_type, l, r),
        Intrinsic::IntSub(l, r) => op!(curios_ersd::Operation::IntSub, int_type, l, r),
        Intrinsic::IntMul(l, r) => op!(curios_ersd::Operation::IntMul, int_type, l, r),
        Intrinsic::IntDiv(l, r) => op!(curios_ersd::Operation::IntDiv, int_type, l, r),
        Intrinsic::IntRem(l, r) => op!(curios_ersd::Operation::IntRem, int_type, l, r),
        Intrinsic::IntLt(l, r) => op!(curios_ersd::Operation::IntLt, int_type, l, r),
        Intrinsic::IntGt(l, r) => op!(curios_ersd::Operation::IntGt, int_type, l, r),
        Intrinsic::IntLte(l, r) => op!(curios_ersd::Operation::IntLte, int_type, l, r),
        Intrinsic::IntGte(l, r) => op!(curios_ersd::Operation::IntGte, int_type, l, r),
        Intrinsic::IntAnd(l, r) => op!(curios_ersd::Operation::IntAnd, int_type, l, r),
        Intrinsic::IntOr(l, r) => op!(curios_ersd::Operation::IntOr, int_type, l, r),
        Intrinsic::IntXor(l, r) => op!(curios_ersd::Operation::IntXor, int_type, l, r),
        Intrinsic::IntShl(l, r) => op!(curios_ersd::Operation::IntShl, int_type, l, r),
        Intrinsic::IntShr(l, r) => op!(curios_ersd::Operation::IntShr, int_type, l, r),
        Intrinsic::IntRotl(l, r) => op!(curios_ersd::Operation::IntRotl, int_type, l, r),
        Intrinsic::IntRotr(l, r) => op!(curios_ersd::Operation::IntRotr, int_type, l, r),
        Intrinsic::IntClz(i) => op!(curios_ersd::Operation::IntClz, int_type, i),
        Intrinsic::IntCtz(i) => op!(curios_ersd::Operation::IntCtz, int_type, i),
        Intrinsic::IntPopcnt(i) => op!(curios_ersd::Operation::IntPopcnt, int_type, i),

        &Intrinsic::Flt(value) => Ok(lowering.constant(curios_ersd::Constant::Flt(value))),
        Intrinsic::FltAdd(l, r) => op!(curios_ersd::Operation::FltAdd, flt_type, l, r),
        Intrinsic::FltSub(l, r) => op!(curios_ersd::Operation::FltSub, flt_type, l, r),
        Intrinsic::FltMul(l, r) => op!(curios_ersd::Operation::FltMul, flt_type, l, r),
        Intrinsic::FltDiv(l, r) => op!(curios_ersd::Operation::FltDiv, flt_type, l, r),
        Intrinsic::FltRem(l, r) => op!(curios_ersd::Operation::FltRem, flt_type, l, r),
        Intrinsic::FltEql(l, r) => op!(curios_ersd::Operation::FltEql, flt_type, l, r),
        Intrinsic::FltNeq(l, r) => op!(curios_ersd::Operation::FltNeq, flt_type, l, r),
        Intrinsic::FltLt(l, r) => op!(curios_ersd::Operation::FltLt, flt_type, l, r),
        Intrinsic::FltGt(l, r) => op!(curios_ersd::Operation::FltGt, flt_type, l, r),
        Intrinsic::FltLte(l, r) => op!(curios_ersd::Operation::FltLte, flt_type, l, r),
        Intrinsic::FltGte(l, r) => op!(curios_ersd::Operation::FltGte, flt_type, l, r),
        Intrinsic::FltMin(l, r) => op!(curios_ersd::Operation::FltMin, flt_type, l, r),
        Intrinsic::FltMax(l, r) => op!(curios_ersd::Operation::FltMax, flt_type, l, r),
        Intrinsic::FltCopysign(l, r) => op!(curios_ersd::Operation::FltCopysign, flt_type, l, r),
        Intrinsic::FltNeg(inner) => op!(curios_ersd::Operation::FltNeg, flt_type, inner),
        Intrinsic::FltAbs(inner) => op!(curios_ersd::Operation::FltAbs, flt_type, inner),
        Intrinsic::FltSqrt(inner) => op!(curios_ersd::Operation::FltSqrt, flt_type, inner),
        Intrinsic::FltFloor(inner) => op!(curios_ersd::Operation::FltFloor, flt_type, inner),
        Intrinsic::FltCeil(inner) => op!(curios_ersd::Operation::FltCeil, flt_type, inner),
        Intrinsic::FltTrunc(inner) => op!(curios_ersd::Operation::FltTrunc, flt_type, inner),
        Intrinsic::FltNearest(inner) => op!(curios_ersd::Operation::FltNearest, flt_type, inner),

        Intrinsic::NatToInt(inner) => op!(curios_ersd::Operation::NatToInt, nat_type, inner),
        Intrinsic::NatToFlt(inner) => op!(curios_ersd::Operation::NatToFlt, nat_type, inner),
        Intrinsic::IntToNat(inner) => op!(curios_ersd::Operation::IntToNat, int_type, inner),
        Intrinsic::IntToFlt(inner) => op!(curios_ersd::Operation::IntToFlt, int_type, inner),
        Intrinsic::FltToNat(inner) => op!(curios_ersd::Operation::FltToNat, flt_type, inner),
        Intrinsic::FltToInt(inner) => op!(curios_ersd::Operation::FltToInt, flt_type, inner),
        Intrinsic::FltToLeBytes(inner) => {
            op!(curios_ersd::Operation::FltToLeBytes, flt_type, inner)
        }
        Intrinsic::FltOfLeBytes(inner) => lowering.operation(
            context,
            curios_ersd::Operation::FltOfLeBytes,
            &[(inner, bin_type(Grain::X))],
            hint,
        ),

        Intrinsic::Bin(grain, value) => {
            Ok(lowering.constant(curios_ersd::Constant::Bin(*grain, value.clone())))
        }
        Intrinsic::BinLen(grain, bin) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::BinLen(*grain),
            &[(bin, bin_type(*grain))],
            hint,
        ),
        Intrinsic::BinEql(grain, l, r) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::BinEql(*grain),
            &[(l, bin_type(*grain)), (r, bin_type(*grain))],
            hint,
        ),
        Intrinsic::BinGet(grain, bin, index) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::BinGet(*grain),
            &[(bin, bin_type(*grain)), (index, nat_type())],
            hint,
        ),
        Intrinsic::BinSlice(grain, bin, start, end) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::BinSlice(*grain),
            &[
                (bin, bin_type(*grain)),
                (start, nat_type()),
                (end, nat_type()),
            ],
            hint,
        ),
        Intrinsic::BinAppend(grain, bin, element) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::BinAppend(*grain),
            &[
                (bin, bin_type(*grain)),
                (element, grain_element_type(*grain)),
            ],
            hint,
        ),
        Intrinsic::BinConcat(grain, operands) => {
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

        Intrinsic::List(_, elements) => {
            // Elaborate already checked this literal against a list type; the element type is re-derived only to lower the elements.
            let element_type = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
                Subterm::Intrinsic(Intrinsic::ListType(element_type)) => element_type,
                _ => unreachable!("erase: list literal checked against non-list type"),
            };
            let pairs = elements
                .iter()
                .map(|element| (element, element_type.clone()))
                .collect::<Vec<_>>();
            lowering.sequence(context, curios_ersd::SequenceOp::ListBuild, &pairs, hint)
        }
        Intrinsic::ListLen(element_type, list) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::ListLen,
            &[(list, list_type(element_type.clone()))],
            hint,
        ),
        Intrinsic::ListGet(element_type, list, index) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::ListGet,
            &[(list, list_type(element_type.clone())), (index, nat_type())],
            hint,
        ),
        Intrinsic::ListSlice(element_type, list, start, end) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::ListSlice,
            &[
                (list, list_type(element_type.clone())),
                (start, nat_type()),
                (end, nat_type()),
            ],
            hint,
        ),
        Intrinsic::ListAppend(element_type, list, element) => lowering.sequence(
            context,
            curios_ersd::SequenceOp::ListAppend,
            &[
                (list, list_type(element_type.clone())),
                (element, element_type.clone()),
            ],
            hint,
        ),
        Intrinsic::ListConcat(element_type, operands) => {
            let pairs = operands
                .iter()
                .map(|operand| (operand, list_type(element_type.clone())))
                .collect::<Vec<_>>();
            lowering.sequence(context, curios_ersd::SequenceOp::ListConcat, &pairs, hint)
        }
        Intrinsic::ListMap(domain, codomain, list, mapper) => {
            let list_atom =
                emitted!(lowering.walk(context, list, &list_type(domain.clone()), None)?);
            let mapper_type = Term::func_type(
                [(context.fresh(Some("x")), domain.clone())],
                codomain.clone(),
            );
            let mapper_atom = emitted!(lowering.walk(context, mapper, &mapper_type, None)?);
            Ok(lowering.bind(
                hint,
                curios_ersd::Rhs::Intrinsic {
                    intrinsic: curios_ersd::Intrinsic::ListMap,
                    operands: vec![list_atom, mapper_atom],
                },
            ))
        }

        &Intrinsic::Handle(token) => Ok(lowering.constant(curios_ersd::Constant::Handle(token))),
        Intrinsic::HandleEql(l, r) => op!(curios_ersd::Operation::HandleEql, handle_type, l, r),
        // Every operation the host performs is typed `Io`, so every one erases to a thunk: the
        // operands are computed where the description is *built*, and the operation itself happens
        // only when the description is forced. Nothing below changes what the host call is — only
        // where it sits relative to the closure boundary.

        // A process exit never yields a value, so the thunk's block is sealed by the terminator rather than by a return. Code after the *force* is dead; code after the construction is not.
        Intrinsic::ProcExit(code) => {
            let code_atom = emitted!(lowering.walk(context, code, &nat_type(), None)?);
            lowering.thunk(hint.or(Some("io/exit")), move |_| {
                Ok(Outcome::Diverged(curios_ersd::Terminator::Exit(code_atom)))
            })
        }

        Intrinsic::Cell(type_, initial) => {
            let initial_atom = emitted!(lowering.walk(context, initial, type_, None)?);
            lowering.thunk(hint.or(Some("io/cell_new")), move |lowering| {
                Ok(lowering.bind(
                    None,
                    curios_ersd::Rhs::Cell {
                        operation: curios_ersd::CellOperation::New,
                        operands: vec![initial_atom],
                    },
                ))
            })
        }
        Intrinsic::CellSet(type_, cell, value) => {
            let cell_type: Term = Subterm::Intrinsic(Intrinsic::CellType(type_.clone())).into();
            let cell_atom = emitted!(lowering.walk(context, cell, &cell_type, None)?);
            let value_atom = emitted!(lowering.walk(context, value, type_, None)?);
            lowering.thunk(hint.or(Some("io/cell_set")), move |lowering| {
                Ok(lowering.bind(
                    None,
                    curios_ersd::Rhs::Cell {
                        operation: curios_ersd::CellOperation::Set,
                        operands: vec![cell_atom, value_atom],
                    },
                ))
            })
        }
        Intrinsic::CellGet(type_, cell) => {
            let cell_type: Term = Subterm::Intrinsic(Intrinsic::CellType(type_.clone())).into();
            let cell_atom = emitted!(lowering.walk(context, cell, &cell_type, None)?);
            lowering.thunk(hint.or(Some("io/cell_get")), move |lowering| {
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
        // The operand is erased at the construction site like every other intrinsic's, not inside the thunk. The language is eager: `/sys/Io/pure` is an ordinary call-by-value wrapper, so a surface `Io/pure(e)` has evaluated `e` before this node exists at all, and erasing the operand inside the closure would delay nothing while making this one arm's evaluation order differ from the rest of the roster. What delays a program's effect is `IoBind`.
        Intrinsic::IoPure(type_, value) => {
            let value = emitted!(lowering.walk(context, value, type_, None)?);
            lowering.thunk(hint.or(Some("io/pure")), move |_| {
                Ok(Outcome::Emitted(value))
            })
        }
        // The description that performs `action`, then the description `continuation` computes from its result. Both forces are zero-argument applications of the closures the operands erased to.
        Intrinsic::IoBind(from, to, action, continuation) => {
            lowering.thunk(hint.or(Some("io/bind")), |lowering| {
                let io_from: Term = Subterm::Intrinsic(Intrinsic::IoType(from.clone())).into();
                let action_atom = emitted!(lowering.walk(context, action, &io_from, None)?);
                let result = emitted!(lowering.force(action_atom));

                let io_to: Term = Subterm::Intrinsic(Intrinsic::IoType(to.clone())).into();
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
            })
        }
    }
}
