//! The intrinsic transcription: each Core intrinsic to its arena identity, shape for shape.
//!
//! No carrier is chosen here: `Bool` values and operations stay `Bool`-shaped, `Byte` stays `Byte`, `Handle` stays an opaque handle constant, and a packed binary's element is its grain's shape (`Byte` for `X`, `Bool` for `B`) — every collapse onto a runtime carrier belongs to the lowering out of the representation. Type-level numerals cross whole, the erased carriers being unbounded too; the one narrowing here is a dispatch key's, refused rather than wrapped (`narrow_case_key`).

use {
    super::{Error, Intrinsic, Lowering, Nat, Natural, Outcome, Retyping, Subterm, Term, emitted},
    curios_core::{Global, Operand},
    curios_utilities::SyntaxName,
};

/// A natural-dispatch case key as `curios-ersd`'s `NatCase` carries it, refusing one no branch table indexes.
///
/// The one narrowing at the `Nat` half of the Core border. A `Nat` *value* crosses whole — the erased carriers are unbounded too, and only materialization in `curios-emit` refuses one the envelope cannot box — but a dispatch *key* is not a value: it selects an arm, and an arm is a slot in a table the backend builds.
///
/// `pub(super)` because `eliminate`'s switch narrows its case keys through it: Core keys a `Cases::Switch` by `Natural`, so the width is chosen here, at the one boundary that owns it.
pub(super) fn narrow_case_key(value: &Natural) -> Result<u32, Error> {
    u32::try_from(value)
        .ok()
        .ok_or_else(|| Error::nat_case_key_overflow(value.clone()))
}

/// One operand an operation computes with, beside what the roster demands of it.
pub(super) type Demanded<'a> = (&'a Term, Operand);

/// The operands `intrinsic` computes with, in order, each beside what [`Intrinsic::signature`] demands of it.
///
/// The roster is the one statement of an operand's type. Both checkers walk it, so an arm that spelled a type of its own could come to disagree with the one its operand was checked at, and erasure runs after the kernel, where a disagreement is a miscompilation rather than a refusal. Two operands are left out, a run holding nothing of either: one demanded as a type, and the proof of the operation's bound ([`Intrinsic::bound`]) — erasure is where a proof stops existing, and nowhere else has to know.
fn computed<'a>(context: &Retyping, intrinsic: &'a Intrinsic) -> Vec<Demanded<'a>> {
    let bound = intrinsic.bound();

    intrinsic
        .operands()
        .into_iter()
        .zip(intrinsic.signature(&context.syntax()).operands)
        .filter(|(operand, demand)| {
            !matches!(demand, Operand::IsType)
                && !bound.is_some_and(|proof| std::ptr::eq(proof, *operand))
        })
        .collect()
}

/// The operands of an arm that reads each by its place.
fn places<'a, const N: usize>(operands: Vec<Demanded<'a>>) -> [Demanded<'a>; N] {
    operands
        .try_into()
        .expect("erase: the roster states the operation's arity")
}

/// The type a demand states. A function's is assembled from its halves under binders minted here, the roster having no name source of its own.
pub(super) fn demanded_type(context: &mut Retyping, demand: &Operand) -> Term {
    match demand {
        Operand::At(type_) => type_.clone(),
        Operand::Function { domains, codomain } => {
            let params = domains
                .iter()
                .map(|domain| (context.fresh(None), domain.clone()))
                .collect::<Vec<_>>();
            Term::func_type(params, codomain.clone())
        }
        Operand::IsType => {
            unreachable!("erase: an operand demanded as a type is not computed with")
        }
    }
}

impl Lowering<'_> {
    /// Resolve an outcome through the declaration's ordinary registered layout.
    pub(super) fn outcome_constructor(
        &mut self,
        context: &mut Retyping,
        family: SyntaxName,
        constructor: SyntaxName,
    ) -> Result<curios_ersd::ConstructorId, Error> {
        let name = Global::Authored(family.qualifier());
        let row = self.induct_row(context, &name)?;
        let index = context
            .induct_decl(&name)
            .expect("erase: registered outcome family")
            .constructor_index(&constructor.last().into())
            .expect("erase: registered outcome constructor");
        Ok(row.constructors[index].id)
    }

    /// Erase a constant to its interned atom.
    fn constant(&mut self, constant: curios_ersd::Constant) -> Outcome {
        Outcome::Emitted(curios_ersd::Atom::Constant(self.builder.constant(constant)))
    }

    /// Erase one operand against what the roster demands of it.
    fn demanded(
        &mut self,
        context: &mut Retyping,
        (term, demand): &Demanded<'_>,
    ) -> Result<Outcome, Error> {
        let type_ = demanded_type(context, demand);
        self.walk(context, term, &type_, None)
    }

    /// Erase a value put into a cell or a channel as the slot it fills: against what the roster demands of it, a proof or a type held as its stand-in ([`kept_operand`](Self::kept_operand)).
    fn kept(
        &mut self,
        context: &mut Retyping,
        (term, demand): &Demanded<'_>,
    ) -> Result<Outcome, Error> {
        let type_ = demanded_type(context, demand);
        self.kept_operand(context, term, &type_, None)
    }

    /// Erase the operands (each against its demand, in order) and bind a scalar operation.
    fn operation(
        &mut self,
        context: &mut Retyping,
        operation: curios_ersd::Operation,
        operands: &[Demanded<'_>],
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let mut atoms = Vec::with_capacity(operands.len());
        for operand in operands {
            atoms.push(emitted!(self.demanded(context, operand)?));
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
    /// What belongs inside is the *performance*, not the operands. `IoPure` erases its value eagerly and hands the closure an atom to return; `IoBind` puts the two forces and the continuation's application inside, because those are what must not happen until the description is run (see the `suspension` module).
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
        // Every thunk is a description's performance — the fact the reifier's decline reads.
        self.builder.mark_description(function);
        self.builder.let_functions(vec![function]);

        Ok(Outcome::Emitted(curios_ersd::Atom::Function(function)))
    }

    /// The sequence-family counterpart of [`operation`](Self::operation).
    fn sequence(
        &mut self,
        context: &mut Retyping,
        operation: curios_ersd::SequenceOp,
        operands: &[Demanded<'_>],
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let mut atoms = Vec::with_capacity(operands.len());
        for operand in operands {
            atoms.push(emitted!(self.demanded(context, operand)?));
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

/// Transcribe one intrinsic. Every arm reads its operands, and the type each is erased at, off the roster ([`computed`]), and keeps the operation it binds.
pub(super) fn erase_intrinsic(
    lowering: &mut Lowering<'_>,
    context: &mut Retyping,
    intrinsic: &Intrinsic,
    hint: Option<&str>,
) -> Result<Outcome, Error> {
    /// One scalar-operation arm, over the operands the roster states.
    macro_rules! op {
        ($op:expr) => {{
            let operands = computed(context, intrinsic);
            lowering.operation(context, $op, &operands, hint)
        }};
    }

    /// The sequence-family counterpart of `op!`.
    macro_rules! seq {
        ($op:expr) => {{
            let operands = computed(context, intrinsic);
            lowering.sequence(context, $op, &operands, hint)
        }};
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
        | Intrinsic::ChannelType(_)
        | Intrinsic::IoType(_) => Ok(Outcome::Emitted(lowering.unit())),

        &Intrinsic::Bool(value) => Ok(lowering.constant(curios_ersd::Constant::Bool(value))),
        Intrinsic::BoolAnd(..) => op!(curios_ersd::Operation::BoolAnd),
        Intrinsic::BoolOr(..) => op!(curios_ersd::Operation::BoolOr),
        Intrinsic::BoolXor(..) => op!(curios_ersd::Operation::BoolXor),
        Intrinsic::BoolEql(..) => op!(curios_ersd::Operation::BoolEql),
        Intrinsic::BoolNeq(..) => op!(curios_ersd::Operation::BoolNeq),

        &Intrinsic::Byte(value) => Ok(lowering.constant(curios_ersd::Constant::Byte(value))),
        Intrinsic::ByteToNat(..) => op!(curios_ersd::Operation::ByteToNat),
        Intrinsic::NatToByte { .. } => op!(curios_ersd::Operation::NatToByte),

        Intrinsic::Nat(Nat::Zero) => {
            Ok(lowering.constant(curios_ersd::Constant::Nat(Natural::zero())))
        }
        Intrinsic::Nat(Nat::Succ(spine, inner)) => {
            let spine = spine.clone();
            if matches!(
                inner.as_ref(),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) {
                return Ok(lowering.constant(curios_ersd::Constant::Nat(spine)));
            }
            let [inner] = places(computed(context, intrinsic));
            let inner_atom = emitted!(lowering.demanded(context, &inner)?);
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
        Intrinsic::NatEql(..) => op!(curios_ersd::Operation::NatEql),
        Intrinsic::NatNeq(..) => op!(curios_ersd::Operation::NatNeq),
        Intrinsic::NatAdd(..) => op!(curios_ersd::Operation::NatAdd),
        Intrinsic::NatSub(..) => op!(curios_ersd::Operation::NatSub),
        Intrinsic::NatMul(..) => op!(curios_ersd::Operation::NatMul),
        Intrinsic::NatLt(..) => op!(curios_ersd::Operation::NatLt),
        Intrinsic::NatDiv { .. } => op!(curios_ersd::Operation::NatDiv),
        Intrinsic::NatRem { .. } => op!(curios_ersd::Operation::NatRem),
        Intrinsic::NatLe(..) => op!(curios_ersd::Operation::NatLe),
        Intrinsic::NatAnd(..) => op!(curios_ersd::Operation::NatAnd),
        Intrinsic::NatOr(..) => op!(curios_ersd::Operation::NatOr),
        Intrinsic::NatXor(..) => op!(curios_ersd::Operation::NatXor),
        Intrinsic::NatMin(..) => op!(curios_ersd::Operation::NatMin),
        Intrinsic::NatMax(..) => op!(curios_ersd::Operation::NatMax),
        Intrinsic::NatShl(..) => op!(curios_ersd::Operation::NatShl),
        Intrinsic::NatPow(..) => op!(curios_ersd::Operation::NatPow),
        Intrinsic::NatShr(..) => op!(curios_ersd::Operation::NatShr),

        Intrinsic::Int(value) => Ok(lowering.constant(curios_ersd::Constant::Int(value.clone()))),
        Intrinsic::IntEql(..) => op!(curios_ersd::Operation::IntEql),
        Intrinsic::IntNeq(..) => op!(curios_ersd::Operation::IntNeq),
        Intrinsic::IntAdd(..) => op!(curios_ersd::Operation::IntAdd),
        Intrinsic::IntSub(..) => op!(curios_ersd::Operation::IntSub),
        Intrinsic::IntMul(..) => op!(curios_ersd::Operation::IntMul),
        Intrinsic::IntDiv { .. } => op!(curios_ersd::Operation::IntDiv),
        Intrinsic::IntRem { .. } => op!(curios_ersd::Operation::IntRem),
        Intrinsic::IntLt(..) => op!(curios_ersd::Operation::IntLt),
        Intrinsic::IntLe(..) => op!(curios_ersd::Operation::IntLe),
        Intrinsic::IntAnd(..) => op!(curios_ersd::Operation::IntAnd),
        Intrinsic::IntOr(..) => op!(curios_ersd::Operation::IntOr),
        Intrinsic::IntXor(..) => op!(curios_ersd::Operation::IntXor),
        Intrinsic::IntMin(..) => op!(curios_ersd::Operation::IntMin),
        Intrinsic::IntMax(..) => op!(curios_ersd::Operation::IntMax),
        Intrinsic::IntShl(..) => op!(curios_ersd::Operation::IntShl),
        Intrinsic::IntShr(..) => op!(curios_ersd::Operation::IntShr),

        &Intrinsic::Flt(value) => Ok(lowering.constant(curios_ersd::Constant::Flt(value))),
        Intrinsic::FltAdd(rounding, ..) => op!(curios_ersd::Operation::FltAdd(*rounding)),
        Intrinsic::FltSub(rounding, ..) => op!(curios_ersd::Operation::FltSub(*rounding)),
        Intrinsic::FltMul(rounding, ..) => op!(curios_ersd::Operation::FltMul(*rounding)),
        Intrinsic::FltDiv(rounding, ..) => op!(curios_ersd::Operation::FltDiv(*rounding)),
        Intrinsic::FltFma(rounding, ..) => op!(curios_ersd::Operation::FltFma(*rounding)),
        Intrinsic::FltRem(..) => op!(curios_ersd::Operation::FltRem),
        Intrinsic::FltEql(..) => op!(curios_ersd::Operation::FltEql),
        Intrinsic::FltNeq(..) => op!(curios_ersd::Operation::FltNeq),
        Intrinsic::FltLt(..) => op!(curios_ersd::Operation::FltLt),
        Intrinsic::FltLe(..) => op!(curios_ersd::Operation::FltLe),
        Intrinsic::FltMin(..) => op!(curios_ersd::Operation::FltMin),
        Intrinsic::FltMax(..) => op!(curios_ersd::Operation::FltMax),
        Intrinsic::FltCopysign(..) => op!(curios_ersd::Operation::FltCopysign),
        Intrinsic::FltNeg(..) => op!(curios_ersd::Operation::FltNeg),
        Intrinsic::FltAbs(..) => op!(curios_ersd::Operation::FltAbs),
        Intrinsic::FltSqrt(rounding, ..) => op!(curios_ersd::Operation::FltSqrt(*rounding)),
        Intrinsic::FltRoundIntegral(rounding, ..) => {
            op!(curios_ersd::Operation::FltRoundIntegral(*rounding))
        }

        Intrinsic::NatToInt(..) => op!(curios_ersd::Operation::NatToInt),
        Intrinsic::IntAbs(..) => op!(curios_ersd::Operation::IntAbs),
        Intrinsic::IntSign(..) => op!(curios_ersd::Operation::IntSign),
        Intrinsic::NatToFlt(rounding, ..) => op!(curios_ersd::Operation::NatToFlt(*rounding)),
        Intrinsic::IntToNat { .. } => op!(curios_ersd::Operation::IntToNat),
        Intrinsic::IntToFlt(rounding, ..) => op!(curios_ersd::Operation::IntToFlt(*rounding)),
        Intrinsic::FltToNat { .. } => op!(curios_ersd::Operation::FltToNat),
        Intrinsic::FltToInt { .. } => op!(curios_ersd::Operation::FltToInt),
        Intrinsic::FltMantissa { .. } => op!(curios_ersd::Operation::FltMantissa),
        Intrinsic::FltExponent { .. } => op!(curios_ersd::Operation::FltExponent),
        Intrinsic::FltToLeBytes(..) => op!(curios_ersd::Operation::FltToLeBytes),
        Intrinsic::FltOfLeBytes { .. } => op!(curios_ersd::Operation::FltOfLeBytes),

        Intrinsic::Bin(grain, value) => {
            Ok(lowering.constant(curios_ersd::Constant::Bin(*grain, value.clone())))
        }
        Intrinsic::BinLen(grain, ..) => seq!(curios_ersd::SequenceOp::BinLen(*grain)),
        Intrinsic::BinEql(grain, ..) => seq!(curios_ersd::SequenceOp::BinEql(*grain)),
        Intrinsic::BinGet { grain, .. } => seq!(curios_ersd::SequenceOp::BinGet(*grain)),
        Intrinsic::BinSlice { grain, .. } => seq!(curios_ersd::SequenceOp::BinSlice(*grain)),
        Intrinsic::BinAppend { grain, .. } => seq!(curios_ersd::SequenceOp::BinAppend(*grain)),
        Intrinsic::BinReinterp { grain, .. } => {
            seq!(curios_ersd::SequenceOp::BinReinterp(*grain))
        }
        Intrinsic::BinReplicate { grain, .. } => {
            seq!(curios_ersd::SequenceOp::BinReplicate(*grain))
        }
        Intrinsic::BinAnd { grain, .. } => seq!(curios_ersd::SequenceOp::BinAnd(*grain)),
        Intrinsic::BinOr { grain, .. } => seq!(curios_ersd::SequenceOp::BinOr(*grain)),
        Intrinsic::BinXor { grain, .. } => seq!(curios_ersd::SequenceOp::BinXor(*grain)),
        Intrinsic::BinConcat { grain, .. } => seq!(curios_ersd::SequenceOp::BinConcat(*grain)),

        Intrinsic::List { .. } => seq!(curios_ersd::SequenceOp::ListBuild),
        Intrinsic::ListLen { .. } => seq!(curios_ersd::SequenceOp::ListLen),
        Intrinsic::ListGet { .. } => seq!(curios_ersd::SequenceOp::ListGet),
        Intrinsic::ListSlice { .. } => seq!(curios_ersd::SequenceOp::ListSlice),
        Intrinsic::ListAppend { .. } => seq!(curios_ersd::SequenceOp::ListAppend),
        Intrinsic::ListConcat { .. } => seq!(curios_ersd::SequenceOp::ListConcat),
        Intrinsic::ListMap { .. } => {
            let [list, mapper] = places(computed(context, intrinsic));
            let list_atom = emitted!(lowering.demanded(context, &list)?);
            let mapper_atom = emitted!(lowering.demanded(context, &mapper)?);
            Ok(lowering.bind(
                hint,
                curios_ersd::Rhs::Intrinsic {
                    intrinsic: curios_ersd::Intrinsic::ListMap,
                    operands: vec![list_atom, mapper_atom],
                },
            ))
        }

        Intrinsic::ListFold {
            element, result, ..
        } => {
            let operands = places(computed(context, intrinsic));
            lowering.erase_list_fold(context, element, result, operands, hint)
        }

        &Intrinsic::Handle(token) => Ok(lowering.constant(curios_ersd::Constant::Handle(token))),
        // Every operation the host performs is typed `Io`, so every one erases to a thunk: the operands are computed where the description is *built*, and the operation itself happens only when the description is forced. Nothing below changes what the host call is — only where it sits relative to the closure boundary.
        Intrinsic::Cell { .. } => lowering.thunk(hint.or(Some("io/cell_new")), move |lowering| {
            Ok(lowering.bind(
                None,
                curios_ersd::Rhs::Cell {
                    operation: curios_ersd::CellOperation::New,
                    operands: vec![],
                },
            ))
        }),
        Intrinsic::CellFill { .. } => {
            let [cell, value] = places(computed(context, intrinsic));
            let cell = emitted!(lowering.demanded(context, &cell)?);
            let value = emitted!(lowering.kept(context, &value)?);
            lowering.thunk(hint.or(Some("io/cell_fill")), move |lowering| {
                Ok(lowering.bind(
                    None,
                    curios_ersd::Rhs::Cell {
                        operation: curios_ersd::CellOperation::Fill,
                        operands: vec![cell, value],
                    },
                ))
            })
        }
        Intrinsic::CellPoll { .. } => {
            let [cell] = places(computed(context, intrinsic));
            let cell = emitted!(lowering.demanded(context, &cell)?);
            let syntax = context.syntax().option;
            let some = lowering.outcome_constructor(context, syntax.family, syntax.some)?;
            let none = lowering.outcome_constructor(context, syntax.family, syntax.none)?;
            lowering.thunk(hint.or(Some("io/cell_poll")), move |lowering| {
                Ok(lowering.bind(
                    None,
                    curios_ersd::Rhs::Cell {
                        operation: curios_ersd::CellOperation::Poll { some, none },
                        operands: vec![cell],
                    },
                ))
            })
        }
        Intrinsic::Channel { .. } => {
            let [capacity] = places(computed(context, intrinsic));
            let capacity = emitted!(lowering.demanded(context, &capacity)?);
            lowering.thunk(hint.or(Some("io/channel_new")), move |lowering| {
                Ok(lowering.bind(
                    None,
                    curios_ersd::Rhs::Channel {
                        operation: curios_ersd::ChannelOperation::New,
                        operands: vec![capacity],
                    },
                ))
            })
        }
        Intrinsic::ChannelPush { .. } => {
            let [channel, value] = places(computed(context, intrinsic));
            let channel = emitted!(lowering.demanded(context, &channel)?);
            let value = emitted!(lowering.kept(context, &value)?);
            let syntax = context.syntax().channel;
            let taken = lowering.outcome_constructor(context, syntax.push, syntax.taken)?;
            let full = lowering.outcome_constructor(context, syntax.push, syntax.full)?;
            let closed = lowering.outcome_constructor(context, syntax.push, syntax.closed)?;
            lowering.thunk(hint.or(Some("io/channel_push")), move |lowering| {
                Ok(lowering.bind(
                    None,
                    curios_ersd::Rhs::Channel {
                        operation: curios_ersd::ChannelOperation::Push {
                            taken,
                            full,
                            closed,
                        },
                        operands: vec![channel, value],
                    },
                ))
            })
        }
        Intrinsic::ChannelTake { .. } => {
            let [channel] = places(computed(context, intrinsic));
            let channel = emitted!(lowering.demanded(context, &channel)?);
            let syntax = context.syntax().channel;
            let item = lowering.outcome_constructor(context, syntax.take, syntax.item)?;
            let empty = lowering.outcome_constructor(context, syntax.take, syntax.empty)?;
            let ended = lowering.outcome_constructor(context, syntax.take, syntax.ended)?;
            lowering.thunk(hint.or(Some("io/channel_take")), move |lowering| {
                Ok(lowering.bind(
                    None,
                    curios_ersd::Rhs::Channel {
                        operation: curios_ersd::ChannelOperation::Take { item, empty, ended },
                        operands: vec![channel],
                    },
                ))
            })
        }
        Intrinsic::ChannelClose { .. }
        | Intrinsic::ChannelClosed { .. }
        | Intrinsic::ChannelCount { .. }
        | Intrinsic::ChannelCapacity { .. } => {
            let [channel] = places(computed(context, intrinsic));
            let channel = emitted!(lowering.demanded(context, &channel)?);
            let (operation, name) = match intrinsic {
                Intrinsic::ChannelClose { .. } => {
                    (curios_ersd::ChannelOperation::Close, "io/channel_close")
                }
                Intrinsic::ChannelClosed { .. } => {
                    (curios_ersd::ChannelOperation::Closed, "io/channel_closed")
                }
                Intrinsic::ChannelCount { .. } => {
                    (curios_ersd::ChannelOperation::Count, "io/channel_count")
                }
                Intrinsic::ChannelCapacity { .. } => (
                    curios_ersd::ChannelOperation::Capacity,
                    "io/channel_capacity",
                ),
                _ => unreachable!("channel query arm"),
            };
            lowering.thunk(hint.or(Some(name)), move |lowering| {
                Ok(lowering.bind(
                    None,
                    curios_ersd::Rhs::Channel {
                        operation,
                        operands: vec![channel],
                    },
                ))
            })
        }

        // The description that performs nothing: a closure yielding an already-computed value.
        //
        // The operand is erased at the construction site like every other intrinsic's, not inside the thunk. The language is eager: `/sys/Io/pure` is an ordinary call-by-value wrapper, so a surface `Io/pure(e)` has evaluated `e` before this node exists at all, and erasing the operand inside the closure would delay nothing while making this one arm's evaluation order differ from the rest of the roster. What delays a program's effect is `IoBind`.
        Intrinsic::IoPure { .. } => {
            let [value] = places(computed(context, intrinsic));
            let value = emitted!(lowering.demanded(context, &value)?);
            lowering.thunk(hint.or(Some("io/pure")), move |_| {
                Ok(Outcome::Emitted(value))
            })
        }
        // The description that performs `action`, then the description `continuation` computes from its result. The operands erase at the construction site like the rest of the roster (see `thunk`'s placement rule); the two forces, the test between them and the continuation's application are the performance, and only they stay inside.
        Intrinsic::IoBind { .. } => {
            let [action, continuation] = places(computed(context, intrinsic));
            let action = emitted!(lowering.demanded(context, &action)?);
            let continuation = emitted!(lowering.demanded(context, &continuation)?);

            lowering.thunk(hint.or(Some("io/bind")), move |lowering| {
                Ok(Outcome::Emitted(
                    lowering.perform_bind(action, continuation),
                ))
            })
        }
        // The description that suspends on `on` and `filled`: its performance is the suspension, and whatever forced it decides what happens next.
        Intrinsic::IoSuspend { .. } => {
            let [on, filled, cancel, rest] = places(computed(context, intrinsic));
            let on = emitted!(lowering.demanded(context, &on)?);
            let filled = emitted!(lowering.demanded(context, &filled)?);
            let cancel = emitted!(lowering.demanded(context, &cancel)?);
            let rest = emitted!(lowering.demanded(context, &rest)?);

            lowering.thunk(hint.or(Some("io/suspend")), move |lowering| {
                Ok(Outcome::Emitted(
                    lowering.perform_suspend(on, filled, cancel, rest),
                ))
            })
        }
        // The description that performs `action` and goes on into `done` with its value or into `suspended` with its suspension.
        Intrinsic::IoAdvance { .. } => {
            let [action, done, suspended] = places(computed(context, intrinsic));
            let action = emitted!(lowering.demanded(context, &action)?);
            let done = emitted!(lowering.demanded(context, &done)?);
            let suspended = emitted!(lowering.demanded(context, &suspended)?);

            lowering.thunk(hint.or(Some("io/advance")), move |lowering| {
                Ok(Outcome::Emitted(
                    lowering.perform_advance(action, done, suspended),
                ))
            })
        }
    }
}
