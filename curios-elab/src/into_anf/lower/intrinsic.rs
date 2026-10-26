//! The intrinsic transcription: each Core intrinsic to its arena identity, shape for shape.
//!
//! No carrier is chosen here: `Bool` values and operations stay `Bool`-shaped, `Byte` stays `Byte`, `Handle` stays an opaque handle constant, and a packed binary's element is a `Byte` — every collapse onto a runtime carrier belongs to the lowering out of the representation. Type-level numerals cross whole, the erased carriers being unbounded too; the one narrowing here is a dispatch key's, refused rather than wrapped (`narrow_case_key`).

use {
    super::{Lowering, Outcome},
    crate::{Error, Retyping, into_anf::emitted},
    curios_core::{Global, Intrinsic, Nat, Operand, Subterm, Term},
    curios_num::Natural,
    curios_utilities::SyntaxName,
};

/// A natural-dispatch case key as `curios-anf`'s `NatCase` carries it, refusing one no branch table indexes.
///
/// The one narrowing at the `Nat` half of the Core border. A `Nat` *value* crosses whole — the erased carriers are unbounded too, and only materialization in `curios-emit` refuses one the envelope cannot box — but a dispatch *key* is not a value: it selects an arm, and an arm is a slot in a table the backend builds.
///
/// Shared across `into_anf` because `eliminate`'s switch narrows its case keys through it: Core keys a `Cases::Switch` by `Natural`, so the width is chosen here, at the one boundary that owns it.
pub fn narrow_case_key(value: &Natural) -> Result<u32, Error> {
    u32::try_from(value)
        .ok()
        .ok_or_else(|| Error::nat_case_key_overflow(value.clone()))
}

/// One operand an operation computes with, beside what the roster demands of it.
pub type Demanded<'a> = (&'a Term, Operand);

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
pub fn demanded_type(context: &mut Retyping, demand: &Operand) -> Term {
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
    pub fn outcome_constructor(
        &mut self,
        context: &mut Retyping,
        family: SyntaxName,
        constructor: SyntaxName,
    ) -> Result<curios_anf::ConstructorId, Error> {
        let name = Global::Authored(family.qualifier());
        let index = context
            .induct_decl(&name)
            .expect("erase: registered outcome family")
            .constructor_index(&constructor.last().into())
            .expect("erase: registered outcome constructor");
        Ok(self.induct_constructor(context, &name, index)?.id)
    }

    /// Erase a constant to its interned atom, as the outcome of erasing a term.
    fn constant_outcome(&mut self, constant: curios_anf::Constant) -> Outcome {
        Outcome::Emitted(curios_anf::Atom::Constant(self.builder.constant(constant)))
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
        operation: curios_anf::Operation,
        operands: &[Demanded<'_>],
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let mut atoms = Vec::with_capacity(operands.len());
        for operand in operands {
            atoms.push(emitted!(self.demanded(context, operand)?));
        }
        Ok(self.bind(
            hint,
            curios_anf::Rhs::Operation {
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
    pub fn thunk(
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

        Ok(Outcome::Emitted(curios_anf::Atom::Function(function)))
    }

    /// The sequence-family counterpart of [`operation`](Self::operation).
    fn sequence(
        &mut self,
        context: &mut Retyping,
        operation: curios_anf::SequenceOp,
        operands: &[Demanded<'_>],
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let mut atoms = Vec::with_capacity(operands.len());
        for operand in operands {
            atoms.push(emitted!(self.demanded(context, operand)?));
        }
        Ok(self.bind(
            hint,
            curios_anf::Rhs::Sequence {
                operation,
                operands: atoms,
            },
        ))
    }

    /// Transcribe one intrinsic. Every arm reads its operands, and the type each is erased at, off the roster ([`computed`]), and keeps the operation it binds.
    pub(super) fn erase_intrinsic(
        &mut self,
        context: &mut Retyping,
        intrinsic: &Intrinsic,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        /// One scalar-operation arm, over the operands the roster states.
        macro_rules! op {
            ($op:expr) => {{
                let operands = computed(context, intrinsic);
                self.operation(context, $op, &operands, hint)
            }};
        }

        /// The sequence-family counterpart of `op!`.
        macro_rules! seq {
            ($op:expr) => {{
                let operands = computed(context, intrinsic);
                self.sequence(context, $op, &operands, hint)
            }};
        }

        match intrinsic {
            // Type formers carry nothing to lower.
            Intrinsic::BoolType
            | Intrinsic::NatType
            | Intrinsic::ByteType
            | Intrinsic::IntType
            | Intrinsic::FltType
            | Intrinsic::BytesType
            | Intrinsic::ListType(_)
            | Intrinsic::HandleType
            | Intrinsic::CellType(_)
            | Intrinsic::ChannelType(_)
            | Intrinsic::IoType(_) => Ok(Outcome::Emitted(self.unit())),

            &Intrinsic::Bool(value) => Ok(self.constant_outcome(curios_anf::Constant::Bool(value))),
            Intrinsic::BoolAnd(..) => op!(curios_anf::Operation::BoolAnd),
            Intrinsic::BoolOr(..) => op!(curios_anf::Operation::BoolOr),
            Intrinsic::BoolXor(..) => op!(curios_anf::Operation::BoolXor),
            Intrinsic::BoolEql(..) => op!(curios_anf::Operation::BoolEql),
            Intrinsic::BoolNeq(..) => op!(curios_anf::Operation::BoolNeq),

            &Intrinsic::Byte(value) => Ok(self.constant_outcome(curios_anf::Constant::Byte(value))),
            Intrinsic::ByteToNat(..) => op!(curios_anf::Operation::ByteToNat),
            Intrinsic::NatToByte { .. } => op!(curios_anf::Operation::NatToByte),

            Intrinsic::Nat(Nat::Zero) => {
                Ok(self.constant_outcome(curios_anf::Constant::Nat(Natural::zero())))
            }
            Intrinsic::Nat(Nat::Succ(spine, inner)) => {
                let spine = spine.clone();
                if matches!(
                    inner.as_ref(),
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
                ) {
                    return Ok(self.constant_outcome(curios_anf::Constant::Nat(spine)));
                }
                let [inner] = places(computed(context, intrinsic));
                let inner_atom = emitted!(self.demanded(context, &inner)?);
                let spine_atom = curios_anf::Atom::Constant(
                    self.builder.constant(curios_anf::Constant::Nat(spine)),
                );
                Ok(self.bind(
                    hint,
                    curios_anf::Rhs::Operation {
                        operation: curios_anf::Operation::NatAdd,
                        operands: vec![spine_atom, inner_atom],
                    },
                ))
            }
            Intrinsic::NatEql(..) => op!(curios_anf::Operation::NatEql),
            Intrinsic::NatNeq(..) => op!(curios_anf::Operation::NatNeq),
            Intrinsic::NatAdd(..) => op!(curios_anf::Operation::NatAdd),
            Intrinsic::NatSub(..) => op!(curios_anf::Operation::NatSub),
            Intrinsic::NatMul(..) => op!(curios_anf::Operation::NatMul),
            Intrinsic::NatLt(..) => op!(curios_anf::Operation::NatLt),
            Intrinsic::NatDiv { .. } => op!(curios_anf::Operation::NatDiv),
            Intrinsic::NatRem { .. } => op!(curios_anf::Operation::NatRem),
            Intrinsic::NatLe(..) => op!(curios_anf::Operation::NatLe),
            Intrinsic::NatAnd(..) => op!(curios_anf::Operation::NatAnd),
            Intrinsic::NatOr(..) => op!(curios_anf::Operation::NatOr),
            Intrinsic::NatXor(..) => op!(curios_anf::Operation::NatXor),
            Intrinsic::NatMin(..) => op!(curios_anf::Operation::NatMin),
            Intrinsic::NatMax(..) => op!(curios_anf::Operation::NatMax),
            Intrinsic::NatShl(..) => op!(curios_anf::Operation::NatShl),
            Intrinsic::NatPow(..) => op!(curios_anf::Operation::NatPow),
            Intrinsic::NatShr(..) => op!(curios_anf::Operation::NatShr),

            Intrinsic::Int(value) => {
                Ok(self.constant_outcome(curios_anf::Constant::Int(value.clone())))
            }
            Intrinsic::IntEql(..) => op!(curios_anf::Operation::IntEql),
            Intrinsic::IntNeq(..) => op!(curios_anf::Operation::IntNeq),
            Intrinsic::IntAdd(..) => op!(curios_anf::Operation::IntAdd),
            Intrinsic::IntSub(..) => op!(curios_anf::Operation::IntSub),
            Intrinsic::IntMul(..) => op!(curios_anf::Operation::IntMul),
            Intrinsic::IntDiv { .. } => op!(curios_anf::Operation::IntDiv),
            Intrinsic::IntRem { .. } => op!(curios_anf::Operation::IntRem),
            Intrinsic::IntLt(..) => op!(curios_anf::Operation::IntLt),
            Intrinsic::IntLe(..) => op!(curios_anf::Operation::IntLe),
            Intrinsic::IntAnd(..) => op!(curios_anf::Operation::IntAnd),
            Intrinsic::IntOr(..) => op!(curios_anf::Operation::IntOr),
            Intrinsic::IntXor(..) => op!(curios_anf::Operation::IntXor),
            Intrinsic::IntMin(..) => op!(curios_anf::Operation::IntMin),
            Intrinsic::IntMax(..) => op!(curios_anf::Operation::IntMax),
            Intrinsic::IntShl(..) => op!(curios_anf::Operation::IntShl),
            Intrinsic::IntShr(..) => op!(curios_anf::Operation::IntShr),

            &Intrinsic::Flt(value) => Ok(self.constant_outcome(curios_anf::Constant::Flt(value))),
            Intrinsic::FltAdd(rounding, ..) => op!(curios_anf::Operation::FltAdd(*rounding)),
            Intrinsic::FltSub(rounding, ..) => op!(curios_anf::Operation::FltSub(*rounding)),
            Intrinsic::FltMul(rounding, ..) => op!(curios_anf::Operation::FltMul(*rounding)),
            Intrinsic::FltDiv(rounding, ..) => op!(curios_anf::Operation::FltDiv(*rounding)),
            Intrinsic::FltFma(rounding, ..) => op!(curios_anf::Operation::FltFma(*rounding)),
            Intrinsic::FltRem(..) => op!(curios_anf::Operation::FltRem),
            Intrinsic::FltEql(..) => op!(curios_anf::Operation::FltEql),
            Intrinsic::FltNeq(..) => op!(curios_anf::Operation::FltNeq),
            Intrinsic::FltLt(..) => op!(curios_anf::Operation::FltLt),
            Intrinsic::FltLe(..) => op!(curios_anf::Operation::FltLe),
            Intrinsic::FltMin(..) => op!(curios_anf::Operation::FltMin),
            Intrinsic::FltMax(..) => op!(curios_anf::Operation::FltMax),
            Intrinsic::FltCopysign(..) => op!(curios_anf::Operation::FltCopysign),
            Intrinsic::FltNeg(..) => op!(curios_anf::Operation::FltNeg),
            Intrinsic::FltAbs(..) => op!(curios_anf::Operation::FltAbs),
            Intrinsic::FltSqrt(rounding, ..) => op!(curios_anf::Operation::FltSqrt(*rounding)),
            Intrinsic::FltRoundIntegral(rounding, ..) => {
                op!(curios_anf::Operation::FltRoundIntegral(*rounding))
            }

            Intrinsic::NatToInt(..) => op!(curios_anf::Operation::NatToInt),
            Intrinsic::IntAbs(..) => op!(curios_anf::Operation::IntAbs),
            Intrinsic::IntSign(..) => op!(curios_anf::Operation::IntSign),
            Intrinsic::NatToFlt(rounding, ..) => op!(curios_anf::Operation::NatToFlt(*rounding)),
            Intrinsic::IntToNat { .. } => op!(curios_anf::Operation::IntToNat),
            Intrinsic::IntToFlt(rounding, ..) => op!(curios_anf::Operation::IntToFlt(*rounding)),
            Intrinsic::FltToNat { .. } => op!(curios_anf::Operation::FltToNat),
            Intrinsic::FltToInt { .. } => op!(curios_anf::Operation::FltToInt),
            Intrinsic::FltMantissa { .. } => op!(curios_anf::Operation::FltMantissa),
            Intrinsic::FltExponent { .. } => op!(curios_anf::Operation::FltExponent),
            Intrinsic::FltToLeBytes(..) => op!(curios_anf::Operation::FltToLeBytes),
            Intrinsic::FltOfLeBytes { .. } => op!(curios_anf::Operation::FltOfLeBytes),

            Intrinsic::Bytes(value) => {
                Ok(self.constant_outcome(curios_anf::Constant::Bytes(value.clone())))
            }
            Intrinsic::BytesLen(..) => seq!(curios_anf::SequenceOp::BytesLen),
            Intrinsic::BytesEql(..) => seq!(curios_anf::SequenceOp::BytesEql),
            Intrinsic::BytesGet { .. } => seq!(curios_anf::SequenceOp::BytesGet),
            Intrinsic::BytesSlice { .. } => seq!(curios_anf::SequenceOp::BytesSlice),
            Intrinsic::BytesAppend { .. } => seq!(curios_anf::SequenceOp::BytesAppend),
            Intrinsic::BytesReplicate { .. } => {
                seq!(curios_anf::SequenceOp::BytesReplicate)
            }
            Intrinsic::BytesAnd { .. } => seq!(curios_anf::SequenceOp::BytesAnd),
            Intrinsic::BytesOr { .. } => seq!(curios_anf::SequenceOp::BytesOr),
            Intrinsic::BytesXor { .. } => seq!(curios_anf::SequenceOp::BytesXor),
            Intrinsic::BytesConcat { .. } => seq!(curios_anf::SequenceOp::BytesConcat),

            Intrinsic::List { .. } => seq!(curios_anf::SequenceOp::ListBuild),
            Intrinsic::ListLen { .. } => seq!(curios_anf::SequenceOp::ListLen),
            // A list's element is a read no producer names the carrier of, so it states the one its element type names.
            Intrinsic::ListGet { element, .. } => {
                let operands = computed(context, intrinsic);
                let outcome =
                    self.sequence(context, curios_anf::SequenceOp::ListGet, &operands, hint)?;
                if let Outcome::Emitted(curios_anf::Atom::Value(value)) = outcome {
                    self.state(context, value, element)?;
                }
                Ok(outcome)
            }
            Intrinsic::ListSlice { .. } => seq!(curios_anf::SequenceOp::ListSlice),
            Intrinsic::ListAppend { .. } => seq!(curios_anf::SequenceOp::ListAppend),
            Intrinsic::ListConcat { .. } => seq!(curios_anf::SequenceOp::ListConcat),
            Intrinsic::ListMap { .. } => {
                let [list, mapper] = places(computed(context, intrinsic));
                let list_atom = emitted!(self.demanded(context, &list)?);
                let mapper_atom = emitted!(self.demanded(context, &mapper)?);
                Ok(self.bind(
                    hint,
                    curios_anf::Rhs::Intrinsic {
                        intrinsic: curios_anf::Intrinsic::ListMap,
                        operands: vec![list_atom, mapper_atom],
                    },
                ))
            }

            Intrinsic::ListFold {
                element, result, ..
            } => {
                let operands = places(computed(context, intrinsic));
                self.erase_list_fold(context, element, result, operands, hint)
            }

            &Intrinsic::Handle(token) => {
                Ok(self.constant_outcome(curios_anf::Constant::Handle(token)))
            }
            // Every operation the host performs is typed `Io`, so every one erases to a thunk: the operands are computed where the description is *built*, and the operation itself happens only when the description is forced. Nothing below changes what the host call is — only where it sits relative to the closure boundary.
            Intrinsic::Cell { .. } => self.thunk(hint.or(Some("io/cell_new")), move |lowering| {
                Ok(lowering.bind(
                    None,
                    curios_anf::Rhs::Cell {
                        operation: curios_anf::CellOperation::New,
                        operands: vec![],
                    },
                ))
            }),
            Intrinsic::CellFill { .. } => {
                let [cell, value] = places(computed(context, intrinsic));
                let cell = emitted!(self.demanded(context, &cell)?);
                let value = emitted!(self.kept(context, &value)?);
                self.thunk(hint.or(Some("io/cell_fill")), move |lowering| {
                    Ok(lowering.bind(
                        None,
                        curios_anf::Rhs::Cell {
                            operation: curios_anf::CellOperation::Fill,
                            operands: vec![cell, value],
                        },
                    ))
                })
            }
            Intrinsic::CellPoll { .. } => {
                let [cell] = places(computed(context, intrinsic));
                let cell = emitted!(self.demanded(context, &cell)?);
                let syntax = context.syntax().option;
                let some = self.outcome_constructor(context, syntax.family, syntax.some)?;
                let none = self.outcome_constructor(context, syntax.family, syntax.none)?;
                self.thunk(hint.or(Some("io/cell_poll")), move |lowering| {
                    Ok(lowering.bind(
                        None,
                        curios_anf::Rhs::Cell {
                            operation: curios_anf::CellOperation::Poll { some, none },
                            operands: vec![cell],
                        },
                    ))
                })
            }
            Intrinsic::Channel { .. } => {
                let [capacity] = places(computed(context, intrinsic));
                let capacity = emitted!(self.demanded(context, &capacity)?);
                self.thunk(hint.or(Some("io/channel_new")), move |lowering| {
                    Ok(lowering.bind(
                        None,
                        curios_anf::Rhs::Channel {
                            operation: curios_anf::ChannelOperation::New,
                            operands: vec![capacity],
                        },
                    ))
                })
            }
            Intrinsic::ChannelPush { .. } => {
                let [channel, value] = places(computed(context, intrinsic));
                let channel = emitted!(self.demanded(context, &channel)?);
                let value = emitted!(self.kept(context, &value)?);
                let syntax = context.syntax().channel;
                let taken = self.outcome_constructor(context, syntax.push, syntax.taken)?;
                let full = self.outcome_constructor(context, syntax.push, syntax.full)?;
                let closed = self.outcome_constructor(context, syntax.push, syntax.closed)?;
                self.thunk(hint.or(Some("io/channel_push")), move |lowering| {
                    Ok(lowering.bind(
                        None,
                        curios_anf::Rhs::Channel {
                            operation: curios_anf::ChannelOperation::Push {
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
                let channel = emitted!(self.demanded(context, &channel)?);
                let syntax = context.syntax().channel;
                let item = self.outcome_constructor(context, syntax.take, syntax.item)?;
                let empty = self.outcome_constructor(context, syntax.take, syntax.empty)?;
                let ended = self.outcome_constructor(context, syntax.take, syntax.ended)?;
                self.thunk(hint.or(Some("io/channel_take")), move |lowering| {
                    Ok(lowering.bind(
                        None,
                        curios_anf::Rhs::Channel {
                            operation: curios_anf::ChannelOperation::Take { item, empty, ended },
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
                let channel = emitted!(self.demanded(context, &channel)?);
                let (operation, name) = match intrinsic {
                    Intrinsic::ChannelClose { .. } => {
                        (curios_anf::ChannelOperation::Close, "io/channel_close")
                    }
                    Intrinsic::ChannelClosed { .. } => {
                        (curios_anf::ChannelOperation::Closed, "io/channel_closed")
                    }
                    Intrinsic::ChannelCount { .. } => {
                        (curios_anf::ChannelOperation::Count, "io/channel_count")
                    }
                    Intrinsic::ChannelCapacity { .. } => (
                        curios_anf::ChannelOperation::Capacity,
                        "io/channel_capacity",
                    ),
                    _ => unreachable!("channel query arm"),
                };
                self.thunk(hint.or(Some(name)), move |lowering| {
                    Ok(lowering.bind(
                        None,
                        curios_anf::Rhs::Channel {
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
                let value = emitted!(self.demanded(context, &value)?);
                self.thunk(hint.or(Some("io/pure")), move |_| {
                    Ok(Outcome::Emitted(value))
                })
            }
            // The description that performs `action`, then the description `continuation` computes from its result. The operands erase at the construction site like the rest of the roster (see `thunk`'s placement rule); the two forces, the test between them and the continuation's application are the performance, and only they stay inside.
            Intrinsic::IoBind { .. } => {
                let [action, continuation] = places(computed(context, intrinsic));
                let action = emitted!(self.demanded(context, &action)?);
                let continuation = emitted!(self.demanded(context, &continuation)?);

                self.thunk(hint.or(Some("io/bind")), move |lowering| {
                    Ok(Outcome::Emitted(
                        lowering.perform_bind(action, continuation),
                    ))
                })
            }
            // The description that suspends on `on` and `filled`: its performance is the suspension, and whatever forced it decides what happens next.
            Intrinsic::IoSuspend { .. } => {
                let [on, filled, cancel, rest] = places(computed(context, intrinsic));
                let on = emitted!(self.demanded(context, &on)?);
                let filled = emitted!(self.demanded(context, &filled)?);
                let cancel = emitted!(self.demanded(context, &cancel)?);
                let rest = emitted!(self.demanded(context, &rest)?);

                self.thunk(hint.or(Some("io/suspend")), move |lowering| {
                    Ok(Outcome::Emitted(
                        lowering.perform_suspend(on, filled, cancel, rest),
                    ))
                })
            }
            // The description that performs `action` and goes on into `done` with its value or into `suspended` with its suspension.
            Intrinsic::IoAdvance { .. } => {
                let [action, done, suspended] = places(computed(context, intrinsic));
                let action = emitted!(self.demanded(context, &action)?);
                let done = emitted!(self.demanded(context, &done)?);
                let suspended = emitted!(self.demanded(context, &suspended)?);

                self.thunk(hint.or(Some("io/advance")), move |lowering| {
                    Ok(Outcome::Emitted(
                        lowering.perform_advance(action, done, suspended),
                    ))
                })
            }
        }
    }
}
