//! The intrinsic transcription: each Core intrinsic to its arena identity, shape for shape.
//!
//! No carrier is chosen here: `Bool` values and operations stay `Bool`-shaped, `Byte` stays `Byte`, `Handle` stays an opaque handle constant, and a packed binary's element is a `Byte` — every collapse onto a runtime carrier belongs to the lowering out of the representation. Type-level numerals cross whole, the erased carriers being unbounded too; the one narrowing here is a dispatch key's, refused rather than wrapped (`narrow_case_key`).

use {
    super::{Lowering, Outcome},
    crate::{Error, Sorts, emitted},
    curios_cert::Reader,
    curios_num::Natural,
    curios_utilities::SyntaxName,
};

/// A natural-dispatch case key as `curios-anf`'s `NatCase` carries it, refusing one no branch table indexes.
///
/// The one narrowing at the `Nat` half of the Core border. A `Nat` *value* crosses whole — the erased carriers are unbounded too, and only materialization in `curios-emit` refuses one the envelope cannot box — but a dispatch *key* is not a value: it selects an arm, and an arm is a slot in a table the backend builds.
///
/// Shared across erasure because `eliminate`'s switch narrows its case keys through it: Core keys a `Cases::Switch` by `Natural`, so the width is chosen here, at the one boundary that owns it.
pub fn narrow_case_key(value: &Natural) -> Result<u32, Error> {
    u32::try_from(value)
        .ok()
        .ok_or_else(|| Error::nat_case_key_overflow(value.clone()))
}

/// One operand an operation computes with, beside what the roster demands of it.
pub type Demanded<'a> = (&'a curios_core::Term, curios_core::Operand);

/// The operands `intrinsic` computes with, in order, each beside what [`Intrinsic::signature`](curios_core::Intrinsic::signature) demands of it.
///
/// The roster is the one statement of an operand's type. Both checkers walk it, so an arm that spelled a type of its own could come to disagree with the one its operand was checked at, and erasure runs after the kernel, where a disagreement is a miscompilation rather than a refusal. Two operands are left out, a run holding nothing of either: one demanded as a type, and the proof of the operation's bound ([`Intrinsic::bound`](curios_core::Intrinsic::bound)) — erasure is where a proof stops existing, and nowhere else has to know.
fn computed<'a>(context: &Reader, intrinsic: &'a curios_core::Intrinsic) -> Vec<Demanded<'a>> {
    let bound = intrinsic.bound();

    intrinsic
        .operands()
        .into_iter()
        .zip(intrinsic.signature(&context.syntax()).operands)
        .filter(|(operand, demand)| {
            !matches!(demand, curios_core::Operand::IsType)
                && !bound.is_some_and(|proof| std::ptr::eq(proof, *operand))
        })
        .collect()
}

/// The sorts the family an operation's result names is read at: those of the instance elaboration retained on the operation ([`Intrinsic::result_universes`](curios_core::Intrinsic::result_universes)), and none for an operation whose result family leaves no sort open.
fn result_sorts(intrinsic: &curios_core::Intrinsic) -> Sorts {
    intrinsic
        .result_universes()
        .map_or(Sorts::NONE, |universes| Sorts::of(&universes.qualities))
}

/// The operands of an arm that reads each by its place.
fn places<'a, const N: usize>(operands: Vec<Demanded<'a>>) -> [Demanded<'a>; N] {
    operands
        .try_into()
        .expect("erase: the roster states the operation's arity")
}

/// The type a demand states. A function's is assembled from its halves under binders minted here, the roster having no name source of its own.
pub fn demanded_type(context: &mut Reader, demand: &curios_core::Operand) -> curios_core::Term {
    match demand {
        curios_core::Operand::At(type_) => type_.clone(),
        curios_core::Operand::Function { domains, codomain } => {
            let params = domains
                .iter()
                .map(|domain| (context.fresh(None), domain.clone()))
                .collect::<Vec<_>>();
            curios_core::Term::func_type(params, codomain.clone())
        }
        curios_core::Operand::IsType => {
            unreachable!("erase: an operand demanded as a type is not computed with")
        }
    }
}

impl Lowering<'_> {
    /// Resolve an outcome through the declaration's ordinary registered layout, at the sorts the operation's result states.
    pub fn outcome_constructor(
        &mut self,
        context: &mut Reader,
        (family, sorts): (SyntaxName, Sorts),
        constructor: SyntaxName,
    ) -> Result<curios_anf::ConstructorId, Error> {
        let name = curios_core::Global::Authored(family.qualifier());
        let index = context
            .induct_decl(&name)
            .expect("erase: registered outcome family")
            .constructor_index(&constructor.last().into())
            .expect("erase: registered outcome constructor");
        let name = self.named(name, sorts);
        Ok(self.induct_constructor(context, name, index)?.id)
    }

    /// Erase a constant to its interned atom, as the outcome of erasing a term.
    fn constant_outcome(&mut self, constant: curios_anf::Constant) -> Outcome {
        Outcome::Emitted(curios_anf::Atom::Constant(self.builder.constant(constant)))
    }

    /// Erase one operand against what the roster demands of it.
    fn demanded(
        &mut self,
        context: &mut Reader,
        (term, demand): &Demanded<'_>,
    ) -> Result<Outcome, Error> {
        let type_ = demanded_type(context, demand);
        self.walk(context, term, &type_, None)
    }

    /// Erase a value put into a cell or a channel as the slot it fills: against what the roster demands of it, a proof or a type held as its stand-in ([`kept_operand`](Self::kept_operand)).
    fn kept(
        &mut self,
        context: &mut Reader,
        (term, demand): &Demanded<'_>,
    ) -> Result<Outcome, Error> {
        let type_ = demanded_type(context, demand);
        self.kept_operand(context, term, &type_, None)
    }

    /// Erase the operands (each against its demand, in order) and bind a scalar operation.
    fn operation(
        &mut self,
        context: &mut Reader,
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
        context: &mut Reader,
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
        context: &mut Reader,
        intrinsic: &curios_core::Intrinsic,
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
            curios_core::Intrinsic::BoolType
            | curios_core::Intrinsic::NatType
            | curios_core::Intrinsic::ByteType
            | curios_core::Intrinsic::IntType
            | curios_core::Intrinsic::FltType
            | curios_core::Intrinsic::BytesType
            | curios_core::Intrinsic::ListType(_)
            | curios_core::Intrinsic::HandleType
            | curios_core::Intrinsic::CellType(_)
            | curios_core::Intrinsic::ChannelType(_)
            | curios_core::Intrinsic::IoType(_) => Ok(Outcome::Emitted(self.unit())),

            &curios_core::Intrinsic::Bool(value) => {
                Ok(self.constant_outcome(curios_anf::Constant::Bool(value)))
            }
            curios_core::Intrinsic::BoolAnd(..) => op!(curios_anf::Operation::BoolAnd),
            curios_core::Intrinsic::BoolOr(..) => op!(curios_anf::Operation::BoolOr),
            curios_core::Intrinsic::BoolXor(..) => op!(curios_anf::Operation::BoolXor),
            curios_core::Intrinsic::BoolEql(..) => op!(curios_anf::Operation::BoolEql),
            curios_core::Intrinsic::BoolNeq(..) => op!(curios_anf::Operation::BoolNeq),

            &curios_core::Intrinsic::Byte(value) => {
                Ok(self.constant_outcome(curios_anf::Constant::Byte(value)))
            }
            curios_core::Intrinsic::ByteToNat(..) => op!(curios_anf::Operation::ByteToNat),
            curios_core::Intrinsic::NatToByte { .. } => op!(curios_anf::Operation::NatToByte),

            curios_core::Intrinsic::Nat(curios_core::Nat::Zero) => {
                Ok(self.constant_outcome(curios_anf::Constant::Nat(Natural::zero())))
            }
            curios_core::Intrinsic::Nat(curios_core::Nat::Succ(spine, inner)) => {
                let spine = spine.clone();
                if matches!(
                    inner.as_ref(),
                    curios_core::Subterm::Intrinsic(curios_core::Intrinsic::Nat(
                        curios_core::Nat::Zero
                    ))
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
            curios_core::Intrinsic::NatEql(..) => op!(curios_anf::Operation::NatEql),
            curios_core::Intrinsic::NatNeq(..) => op!(curios_anf::Operation::NatNeq),
            curios_core::Intrinsic::NatAdd(..) => op!(curios_anf::Operation::NatAdd),
            curios_core::Intrinsic::NatSub(..) => op!(curios_anf::Operation::NatSub),
            curios_core::Intrinsic::NatMul(..) => op!(curios_anf::Operation::NatMul),
            curios_core::Intrinsic::NatLt(..) => op!(curios_anf::Operation::NatLt),
            curios_core::Intrinsic::NatDiv { .. } => op!(curios_anf::Operation::NatDiv),
            curios_core::Intrinsic::NatRem { .. } => op!(curios_anf::Operation::NatRem),
            curios_core::Intrinsic::NatLe(..) => op!(curios_anf::Operation::NatLe),
            curios_core::Intrinsic::NatAnd(..) => op!(curios_anf::Operation::NatAnd),
            curios_core::Intrinsic::NatOr(..) => op!(curios_anf::Operation::NatOr),
            curios_core::Intrinsic::NatXor(..) => op!(curios_anf::Operation::NatXor),
            curios_core::Intrinsic::NatMin(..) => op!(curios_anf::Operation::NatMin),
            curios_core::Intrinsic::NatMax(..) => op!(curios_anf::Operation::NatMax),
            curios_core::Intrinsic::NatShl(..) => op!(curios_anf::Operation::NatShl),
            curios_core::Intrinsic::NatPow(..) => op!(curios_anf::Operation::NatPow),
            curios_core::Intrinsic::NatShr(..) => op!(curios_anf::Operation::NatShr),

            curios_core::Intrinsic::Int(value) => {
                Ok(self.constant_outcome(curios_anf::Constant::Int(value.clone())))
            }
            curios_core::Intrinsic::IntEql(..) => op!(curios_anf::Operation::IntEql),
            curios_core::Intrinsic::IntNeq(..) => op!(curios_anf::Operation::IntNeq),
            curios_core::Intrinsic::IntAdd(..) => op!(curios_anf::Operation::IntAdd),
            curios_core::Intrinsic::IntSub(..) => op!(curios_anf::Operation::IntSub),
            curios_core::Intrinsic::IntMul(..) => op!(curios_anf::Operation::IntMul),
            curios_core::Intrinsic::IntDiv { .. } => op!(curios_anf::Operation::IntDiv),
            curios_core::Intrinsic::IntRem { .. } => op!(curios_anf::Operation::IntRem),
            curios_core::Intrinsic::IntLt(..) => op!(curios_anf::Operation::IntLt),
            curios_core::Intrinsic::IntLe(..) => op!(curios_anf::Operation::IntLe),
            curios_core::Intrinsic::IntAnd(..) => op!(curios_anf::Operation::IntAnd),
            curios_core::Intrinsic::IntOr(..) => op!(curios_anf::Operation::IntOr),
            curios_core::Intrinsic::IntXor(..) => op!(curios_anf::Operation::IntXor),
            curios_core::Intrinsic::IntMin(..) => op!(curios_anf::Operation::IntMin),
            curios_core::Intrinsic::IntMax(..) => op!(curios_anf::Operation::IntMax),
            curios_core::Intrinsic::IntShl(..) => op!(curios_anf::Operation::IntShl),
            curios_core::Intrinsic::IntShr(..) => op!(curios_anf::Operation::IntShr),

            &curios_core::Intrinsic::Flt(value) => {
                Ok(self.constant_outcome(curios_anf::Constant::Flt(value)))
            }
            curios_core::Intrinsic::FltAdd(rounding, ..) => {
                op!(curios_anf::Operation::FltAdd(*rounding))
            }
            curios_core::Intrinsic::FltSub(rounding, ..) => {
                op!(curios_anf::Operation::FltSub(*rounding))
            }
            curios_core::Intrinsic::FltMul(rounding, ..) => {
                op!(curios_anf::Operation::FltMul(*rounding))
            }
            curios_core::Intrinsic::FltDiv(rounding, ..) => {
                op!(curios_anf::Operation::FltDiv(*rounding))
            }
            curios_core::Intrinsic::FltFma(rounding, ..) => {
                op!(curios_anf::Operation::FltFma(*rounding))
            }
            curios_core::Intrinsic::FltRem(..) => op!(curios_anf::Operation::FltRem),
            curios_core::Intrinsic::FltEql(..) => op!(curios_anf::Operation::FltEql),
            curios_core::Intrinsic::FltNeq(..) => op!(curios_anf::Operation::FltNeq),
            curios_core::Intrinsic::FltLt(..) => op!(curios_anf::Operation::FltLt),
            curios_core::Intrinsic::FltLe(..) => op!(curios_anf::Operation::FltLe),
            curios_core::Intrinsic::FltMin(..) => op!(curios_anf::Operation::FltMin),
            curios_core::Intrinsic::FltMax(..) => op!(curios_anf::Operation::FltMax),
            curios_core::Intrinsic::FltCopysign(..) => op!(curios_anf::Operation::FltCopysign),
            curios_core::Intrinsic::FltNeg(..) => op!(curios_anf::Operation::FltNeg),
            curios_core::Intrinsic::FltAbs(..) => op!(curios_anf::Operation::FltAbs),
            curios_core::Intrinsic::FltSqrt(rounding, ..) => {
                op!(curios_anf::Operation::FltSqrt(*rounding))
            }
            curios_core::Intrinsic::FltRoundIntegral(rounding, ..) => {
                op!(curios_anf::Operation::FltRoundIntegral(*rounding))
            }

            curios_core::Intrinsic::NatToInt(..) => op!(curios_anf::Operation::NatToInt),
            curios_core::Intrinsic::IntAbs(..) => op!(curios_anf::Operation::IntAbs),
            curios_core::Intrinsic::IntSign(..) => op!(curios_anf::Operation::IntSign),
            curios_core::Intrinsic::NatToFlt(rounding, ..) => {
                op!(curios_anf::Operation::NatToFlt(*rounding))
            }
            curios_core::Intrinsic::IntToNat { .. } => op!(curios_anf::Operation::IntToNat),
            curios_core::Intrinsic::IntToFlt(rounding, ..) => {
                op!(curios_anf::Operation::IntToFlt(*rounding))
            }
            curios_core::Intrinsic::FltToNat { .. } => op!(curios_anf::Operation::FltToNat),
            curios_core::Intrinsic::FltToInt { .. } => op!(curios_anf::Operation::FltToInt),
            curios_core::Intrinsic::FltMantissa { .. } => op!(curios_anf::Operation::FltMantissa),
            curios_core::Intrinsic::FltExponent { .. } => op!(curios_anf::Operation::FltExponent),
            curios_core::Intrinsic::FltToLeBytes(..) => op!(curios_anf::Operation::FltToLeBytes),
            curios_core::Intrinsic::FltOfLeBytes { .. } => op!(curios_anf::Operation::FltOfLeBytes),

            curios_core::Intrinsic::Bytes(value) => {
                Ok(self.constant_outcome(curios_anf::Constant::Bytes(value.clone())))
            }
            curios_core::Intrinsic::BytesLen(..) => seq!(curios_anf::SequenceOp::BytesLen),
            curios_core::Intrinsic::BytesEql(..) => seq!(curios_anf::SequenceOp::BytesEql),
            curios_core::Intrinsic::BytesGet { .. } => seq!(curios_anf::SequenceOp::BytesGet),
            curios_core::Intrinsic::BytesSlice { .. } => seq!(curios_anf::SequenceOp::BytesSlice),
            curios_core::Intrinsic::BytesAppend { .. } => seq!(curios_anf::SequenceOp::BytesAppend),
            curios_core::Intrinsic::BytesReplicate { .. } => {
                seq!(curios_anf::SequenceOp::BytesReplicate)
            }
            curios_core::Intrinsic::BytesAnd { .. } => seq!(curios_anf::SequenceOp::BytesAnd),
            curios_core::Intrinsic::BytesOr { .. } => seq!(curios_anf::SequenceOp::BytesOr),
            curios_core::Intrinsic::BytesXor { .. } => seq!(curios_anf::SequenceOp::BytesXor),
            curios_core::Intrinsic::BytesConcat { .. } => seq!(curios_anf::SequenceOp::BytesConcat),

            curios_core::Intrinsic::List { .. } => seq!(curios_anf::SequenceOp::ListBuild),
            curios_core::Intrinsic::ListLen { .. } => seq!(curios_anf::SequenceOp::ListLen),
            // A list's element is a read no producer names the carrier of, so it states the one its element type names.
            curios_core::Intrinsic::ListGet { element, .. } => {
                let operands = computed(context, intrinsic);
                let outcome =
                    self.sequence(context, curios_anf::SequenceOp::ListGet, &operands, hint)?;
                if let Outcome::Emitted(curios_anf::Atom::Value(value)) = outcome {
                    self.state(context, value, element)?;
                }
                Ok(outcome)
            }
            curios_core::Intrinsic::ListSlice { .. } => seq!(curios_anf::SequenceOp::ListSlice),
            curios_core::Intrinsic::ListAppend { .. } => seq!(curios_anf::SequenceOp::ListAppend),
            curios_core::Intrinsic::ListConcat { .. } => seq!(curios_anf::SequenceOp::ListConcat),
            curios_core::Intrinsic::ListMap { .. } => {
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

            curios_core::Intrinsic::ListFold {
                element, result, ..
            } => {
                let operands = places(computed(context, intrinsic));
                self.erase_list_fold(context, element, result, operands, hint)
            }

            &curios_core::Intrinsic::Handle(token) => {
                Ok(self.constant_outcome(curios_anf::Constant::Handle(token)))
            }
            // Every operation the host performs is typed `Io`, so every one erases to a thunk: the operands are computed where the description is *built*, and the operation itself happens only when the description is forced. Nothing below changes what the host call is — only where it sits relative to the closure boundary.
            curios_core::Intrinsic::Cell { .. } => {
                self.thunk(hint.or(Some("io/cell_new")), move |lowering| {
                    Ok(lowering.bind(
                        None,
                        curios_anf::Rhs::Cell {
                            operation: curios_anf::CellOperation::New,
                            operands: vec![],
                        },
                    ))
                })
            }
            curios_core::Intrinsic::CellFill { .. } => {
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
            curios_core::Intrinsic::CellPoll { .. } => {
                let [cell] = places(computed(context, intrinsic));
                let cell = emitted!(self.demanded(context, &cell)?);
                let syntax = context.syntax().option;
                let family = (syntax.family, result_sorts(intrinsic));
                let some = self.outcome_constructor(context, family, syntax.some)?;
                let none = self.outcome_constructor(context, family, syntax.none)?;
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
            curios_core::Intrinsic::Channel { .. } => {
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
            curios_core::Intrinsic::ChannelPush { .. } => {
                let [channel, value] = places(computed(context, intrinsic));
                let channel = emitted!(self.demanded(context, &channel)?);
                let value = emitted!(self.kept(context, &value)?);
                let syntax = context.syntax().channel;
                let push = (syntax.push, result_sorts(intrinsic));
                let taken = self.outcome_constructor(context, push, syntax.taken)?;
                let full = self.outcome_constructor(context, push, syntax.full)?;
                let closed = self.outcome_constructor(context, push, syntax.closed)?;
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
            curios_core::Intrinsic::ChannelTake { .. } => {
                let [channel] = places(computed(context, intrinsic));
                let channel = emitted!(self.demanded(context, &channel)?);
                let syntax = context.syntax().channel;
                let take = (syntax.take, result_sorts(intrinsic));
                let item = self.outcome_constructor(context, take, syntax.item)?;
                let empty = self.outcome_constructor(context, take, syntax.empty)?;
                let ended = self.outcome_constructor(context, take, syntax.ended)?;
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
            curios_core::Intrinsic::ChannelClose { .. }
            | curios_core::Intrinsic::ChannelClosed { .. }
            | curios_core::Intrinsic::ChannelCount { .. }
            | curios_core::Intrinsic::ChannelCapacity { .. } => {
                let [channel] = places(computed(context, intrinsic));
                let channel = emitted!(self.demanded(context, &channel)?);
                let (operation, name) = match intrinsic {
                    curios_core::Intrinsic::ChannelClose { .. } => {
                        (curios_anf::ChannelOperation::Close, "io/channel_close")
                    }
                    curios_core::Intrinsic::ChannelClosed { .. } => {
                        (curios_anf::ChannelOperation::Closed, "io/channel_closed")
                    }
                    curios_core::Intrinsic::ChannelCount { .. } => {
                        (curios_anf::ChannelOperation::Count, "io/channel_count")
                    }
                    curios_core::Intrinsic::ChannelCapacity { .. } => (
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
            curios_core::Intrinsic::IoPure { .. } => {
                let [value] = places(computed(context, intrinsic));
                let value = emitted!(self.demanded(context, &value)?);
                self.thunk(hint.or(Some("io/pure")), move |_| {
                    Ok(Outcome::Emitted(value))
                })
            }
            // The description that performs `action`, then the description `continuation` computes from its result. The operands erase at the construction site like the rest of the roster (see `thunk`'s placement rule); the two forces, the test between them and the continuation's application are the performance, and only they stay inside.
            curios_core::Intrinsic::IoBind { .. } => {
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
            curios_core::Intrinsic::IoSuspend { .. } => {
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
            curios_core::Intrinsic::IoAdvance { .. } => {
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
