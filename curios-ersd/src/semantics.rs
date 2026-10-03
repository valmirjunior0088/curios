//! The behavior half of the semantic oracle: one total function over the closed operation alphabet.
//!
//! Every query matches the closed alphabets without a fallback arm, so a newly added operation cannot be silently misclassified — the compiler rejects the unhandled variant. Behavior reported here is *node-local*: it excludes the bodies of functions an `Apply` invokes, the callback an intrinsic runs, and the sub-blocks a match, switch, or fold evaluates; composing those to a fixed point is the effect summary's job ([`super::Summary`]), and this module supplies the leaves it joins.
//!
//! Arity is not restated here — it lives on the operation enums where the verifier already reads it. The fold half of the oracle — operation × constant operands → value, would-trap, or unknown — is [`Semantics::fold_operation`] and [`Semantics::fold_sequence`] below.

#[cfg(test)]
mod tests;

use {
    super::{
        CellOperation, ChannelOperation, Constant, Intrinsic, Operation, Rhs, SequenceOp,
        Terminator,
    },
    curios_num::{Binary, Floating, Grain, Integer, Natural, ScalarTrap},
};

/// What allocating a value commits a pass to. Immutable allocation is not language-observable and may be discarded or duplicated; mutable allocation (a cell) may not. Ordered by severity so [`join`](Allocation::join) is `max`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum Allocation {
    #[default]
    None,
    /// A value whose identity is not observable (a tuple, a variant, a slice view, a list).
    Immutable,
    /// A mutable allocation whose identity a program observes (a cell).
    Mutable,
}

impl Allocation {
    /// Whether the allocating computation cannot be discarded or duplicated on the unused-result rule alone.
    pub fn is_observable(self) -> bool {
        matches!(self, Allocation::Mutable)
    }

    pub fn join(self, other: Allocation) -> Allocation {
        self.max(other)
    }
}

/// The language-observable events a computation may cause. Every dimension is independent — a three-valued purity label would lose distinctions a sound pass needs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ObservableBehavior {
    /// May raise a runtime trap (out-of-bounds access, division by zero, an `Unreachable` terminator).
    pub may_trap: bool,
    /// May fail to terminate. No intrinsic diverges; this is acquired through the call graph (a recursive component) by the effect summary.
    pub may_diverge: bool,
    /// May terminate the process (a `Halt` terminator).
    pub may_exit: bool,
    /// May interact with the host (a foreign call).
    pub host_effect: bool,
    /// May read mutable state (a cell read).
    pub state_read: bool,
    /// May write mutable state (a cell write).
    pub state_write: bool,
}

impl ObservableBehavior {
    pub const fn none() -> Self {
        Self {
            may_trap: false,
            may_diverge: false,
            may_exit: false,
            host_effect: false,
            state_read: false,
            state_write: false,
        }
    }

    /// Whether any observable event may occur — the signal that a computation run for effect must be preserved even when its result is unused.
    pub fn is_effectful(self) -> bool {
        self.may_trap
            || self.may_diverge
            || self.may_exit
            || self.host_effect
            || self.state_read
            || self.state_write
    }

    /// The union of two behaviors — the behavior of doing both.
    pub fn join(self, other: ObservableBehavior) -> ObservableBehavior {
        ObservableBehavior {
            may_trap: self.may_trap || other.may_trap,
            may_diverge: self.may_diverge || other.may_diverge,
            may_exit: self.may_exit || other.may_exit,
            host_effect: self.host_effect || other.host_effect,
            state_read: self.state_read || other.state_read,
            state_write: self.state_write || other.state_write,
        }
    }

    /// This behavior with divergence forced on — how the effect summary marks a recursive component whose termination it cannot prove.
    pub fn with_divergence(mut self) -> ObservableBehavior {
        self.may_diverge = true;
        self
    }
}

/// The operational actions a computation performs that are not themselves observable events: what it allocates.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct OperationalBehavior {
    pub allocation: Allocation,
}

impl OperationalBehavior {
    pub fn join(self, other: OperationalBehavior) -> OperationalBehavior {
        OperationalBehavior {
            allocation: self.allocation.join(other.allocation),
        }
    }
}

/// The full behavior of a computation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct LocalBehavior {
    pub observable: ObservableBehavior,
    pub operational: OperationalBehavior,
}

impl LocalBehavior {
    /// A total, allocation-free, effect-free computation.
    pub fn pure() -> Self {
        Self::default()
    }

    /// The conservative top of the lattice: a computation that may do anything. Used for an unknown callee or callback.
    pub fn unknown() -> Self {
        Self {
            observable: ObservableBehavior {
                may_trap: true,
                may_diverge: true,
                may_exit: true,
                host_effect: true,
                state_read: true,
                state_write: true,
            },
            operational: OperationalBehavior {
                allocation: Allocation::Mutable,
            },
        }
    }

    fn observable(observable: ObservableBehavior) -> Self {
        Self {
            observable,
            operational: OperationalBehavior::default(),
        }
    }

    fn trap() -> Self {
        Self::observable(ObservableBehavior {
            may_trap: true,
            ..ObservableBehavior::none()
        })
    }

    fn host() -> Self {
        Self::observable(ObservableBehavior {
            host_effect: true,
            ..ObservableBehavior::none()
        })
    }

    fn state_read() -> Self {
        Self::observable(ObservableBehavior {
            state_read: true,
            ..ObservableBehavior::none()
        })
    }

    fn state_write() -> Self {
        Self::observable(ObservableBehavior {
            state_write: true,
            ..ObservableBehavior::none()
        })
    }

    fn alloc(allocation: Allocation) -> Self {
        Self {
            observable: ObservableBehavior::none(),
            operational: OperationalBehavior { allocation },
        }
    }

    fn with_alloc(mut self, allocation: Allocation) -> Self {
        self.operational.allocation = self.operational.allocation.join(allocation);
        self
    }

    /// Whether evaluating this computation for effect is observable — an unused binding is safe to delete only when this is false.
    pub fn is_observable(self) -> bool {
        self.observable.is_effectful() || self.operational.allocation.is_observable()
    }

    /// The behavior of performing both computations.
    pub fn join(self, other: LocalBehavior) -> LocalBehavior {
        LocalBehavior {
            observable: self.observable.join(other.observable),
            operational: self.operational.join(other.operational),
        }
    }
}

/// The behavior contract: a zero-sized namespace of total functions over the closed alphabets.
pub struct Semantics;

impl Semantics {
    /// The node-local behavior of a right-hand side — its own operation only, excluding callees, callbacks, and sub-blocks.
    pub fn local_behavior(rhs: &Rhs) -> LocalBehavior {
        match rhs {
            // Pure structure: a call's effects are its callee's summary; a match, switch, or Nat fold contributes only its sub-blocks'; aliasing and projection are total and allocation-free.
            Rhs::Alias(_)
            | Rhs::Apply { .. }
            | Rhs::Project { .. }
            | Rhs::MatchVariant { .. }
            | Rhs::SwitchBool { .. }
            | Rhs::SwitchNat { .. }
            | Rhs::FoldNat { .. } => LocalBehavior::pure(),
            Rhs::Operation { operation, .. } => Self::operation(*operation),
            Rhs::Sequence { operation, .. } => Self::sequence(*operation),
            Rhs::Cell { operation, .. } => Self::cell(*operation),
            Rhs::Channel { operation, .. } => Self::channel(*operation),
            Rhs::Foreign { .. } => LocalBehavior::host(),
            Rhs::Intrinsic { intrinsic, .. } => Self::intrinsic(*intrinsic),
            // Building an aggregate allocates an immutable value; a sequence fold materializes suffix views.
            Rhs::Product { .. }
            | Rhs::Construct { .. }
            | Rhs::FoldSequence { .. }
            | Rhs::UnconsSequence { .. } => LocalBehavior::alloc(Allocation::Immutable),
        }
    }

    /// The behavior of a scalar operation. The float-to-integer conversions may trap on non-finite or out-of-range input, and `FltOfLeBytes` may trap on a binary that is not exactly eight bytes — the [`TrapKind::MalformedInput`] its own fold reports, and the reason this arm must cover every operation whose fold reports a *language-partial* trap. No size is refused among them: `Nat` and `Int` grow past the i31 fast path into a boxed magnitude, so add, multiply and left shift are total here, as `curios-cont`'s `Intrinsic::effect` states them from the other side. Every other scalar operation is total and allocation-free.
    ///
    /// The divisions do not trap either: `/sys`'s division takes a proof that its divisor is nonzero, so a term reaching here has already been refused if it could not supply one, and `IntDiv`'s signed overflow — a *range* fact the precondition says nothing about — is a quotient past the i31, which grows into a boxed magnitude. What the classification decides is only whether an unused binding may be deleted, which is safe for a division whatever its divisor; a guard keeps its proof by position, and nothing here moves an operation above one.
    pub fn operation(operation: Operation) -> LocalBehavior {
        match operation {
            Operation::FltToNat
            | Operation::FltToInt
            | Operation::FltMantissa
            | Operation::FltExponent
            | Operation::FltOfLeBytes => LocalBehavior::trap(),
            Operation::NatDiv
            | Operation::NatRem
            | Operation::IntDiv
            | Operation::IntRem
            | Operation::BoolAnd
            | Operation::BoolOr
            | Operation::BoolXor
            | Operation::BoolEql
            | Operation::BoolNeq
            | Operation::NatEql
            | Operation::NatNeq
            | Operation::NatAdd
            | Operation::NatSub
            | Operation::NatMul
            | Operation::NatLt
            | Operation::NatLe
            | Operation::NatAnd
            | Operation::NatOr
            | Operation::NatXor
            | Operation::NatShl
            | Operation::NatShr
            | Operation::ByteToNat
            | Operation::NatToByte
            | Operation::IntEql
            | Operation::IntNeq
            | Operation::IntAdd
            | Operation::IntSub
            | Operation::IntMul
            | Operation::IntLt
            | Operation::IntLe
            | Operation::IntAnd
            | Operation::IntOr
            | Operation::IntXor
            | Operation::IntShl
            | Operation::IntShr
            | Operation::FltAdd(_)
            | Operation::FltSub(_)
            | Operation::FltMul(_)
            | Operation::FltDiv(_)
            | Operation::FltFma(_)
            | Operation::FltRem
            | Operation::FltEql
            | Operation::FltNeq
            | Operation::FltLt
            | Operation::FltLe
            | Operation::FltMin
            | Operation::FltMax
            | Operation::FltCopysign
            | Operation::FltNeg
            | Operation::FltAbs
            | Operation::FltSqrt(_)
            | Operation::FltRoundIntegral(_)
            | Operation::NatToInt
            | Operation::NatToFlt(_)
            | Operation::IntToNat
            | Operation::IntToFlt(_)
            | Operation::FltToLeBytes => LocalBehavior::pure(),
        }
    }

    /// The behavior of a sequence operation. Indexing may trap out of bounds; slicing may trap and allocates a view; append, concat, build, fill, and the pointwise combinations allocate; length and equality are total.
    ///
    /// The pointwise rows do not trap. Their one bound is that the operands share a length, and that is stated in the type and discharged before erasure — nothing survives to this stage that could fail it, so treating them as fallible would keep a dead-result elimination from removing one whose result nothing reads.
    pub fn sequence(operation: SequenceOp) -> LocalBehavior {
        match operation {
            SequenceOp::BinGet(_) | SequenceOp::ListGet => LocalBehavior::trap(),
            SequenceOp::BinSlice(_) | SequenceOp::ListSlice => {
                LocalBehavior::trap().with_alloc(Allocation::Immutable)
            }
            SequenceOp::BinAppend(_)
            | SequenceOp::BinConcat(_)
            | SequenceOp::BinReplicate(_)
            | SequenceOp::BinReinterp(_)
            | SequenceOp::BinAnd(_)
            | SequenceOp::BinOr(_)
            | SequenceOp::BinXor(_)
            | SequenceOp::ListAppend
            | SequenceOp::ListConcat
            | SequenceOp::ListBuild => LocalBehavior::alloc(Allocation::Immutable),
            SequenceOp::BinLen(_) | SequenceOp::ListLen | SequenceOp::BinEql(_) => {
                LocalBehavior::pure()
            }
        }
    }

    /// The behavior of a cell operation: creation allocates a mutable identity, reading observes state, writing mutates it. None is removable on an unused result alone.
    pub fn cell(operation: CellOperation) -> LocalBehavior {
        match operation {
            CellOperation::New => LocalBehavior::alloc(Allocation::Mutable),
            CellOperation::Poll { .. } => LocalBehavior::state_read(),
            CellOperation::Fill => LocalBehavior::state_write(),
        }
    }

    /// The local behavior of a bounded guest channel operation.
    pub fn channel(operation: ChannelOperation) -> LocalBehavior {
        match operation {
            ChannelOperation::New => LocalBehavior::alloc(Allocation::Mutable),
            ChannelOperation::Push { .. }
            | ChannelOperation::Take { .. }
            | ChannelOperation::Close => LocalBehavior::state_write(),
            ChannelOperation::Closed | ChannelOperation::Count | ChannelOperation::Capacity => {
                LocalBehavior::state_read()
            }
        }
    }

    /// The node-local behavior of an intrinsic. `ListMap` allocates its result list; the mapper's own behavior is composed by the effect summary.
    pub fn intrinsic(intrinsic: Intrinsic) -> LocalBehavior {
        match intrinsic {
            Intrinsic::ListMap => LocalBehavior::alloc(Allocation::Immutable),
        }
    }

    /// The observable behavior of a block terminator.
    pub fn terminator(terminator: &Terminator) -> ObservableBehavior {
        match terminator {
            Terminator::Return(_) => ObservableBehavior::none(),
            Terminator::Halt { .. } => ObservableBehavior {
                may_exit: true,
                ..ObservableBehavior::none()
            },
            Terminator::Unreachable => ObservableBehavior {
                may_trap: true,
                ..ObservableBehavior::none()
            },
        }
    }
}

/// The outcome of constant-folding an operation over fully-known operands. The three cases stay distinct because control-flow simplification depends on the difference: a known trap must survive as an explicit computation — never dead code, never a compile-time panic, never [`Unknown`].
///
/// [`Unknown`]: FoldOutcome::Unknown
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FoldOutcome {
    /// The operation evaluates to this constant.
    Value(Constant),
    /// The operation is known to trap at runtime; the optimizer must keep it as an explicit residual computation.
    WouldTrap(TrapKind),
    /// Nothing is known: an operand is not a constant, the operation has no constant carrier (a list operation), or the fold deliberately declines (an `Int` shift by a negative count, which `curios-core` also leaves unfolded).
    Unknown,
}

/// Why a folded operation would trap.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TrapKind {
    /// Integer division or remainder by a zero divisor.
    DivisionByZero,
    /// A float-to-integer conversion of a non-finite or out-of-range value.
    ConversionRange,
    /// A sequence index outside its bounds.
    IndexOutOfBounds,
    /// A sequence slice outside its bounds.
    SliceOutOfBounds,
    /// A packed-binary decode of the wrong length (`FltOfLeBytes`).
    MalformedInput,
}

impl Semantics {
    /// Constant-fold a scalar operation over its operands, under the numeric law: `Nat` and `Int` exact and unbounded — `Nat` subtraction is monus, and a product or left shift whose result would pass `allowance` bits is declined rather than built — and bit-preserving binary64. Comparisons yield a [`Constant::Bool`]; the `0`/`1` carrier is the lowering's decision. i31 appears nowhere here.
    pub fn fold_operation(
        operation: Operation,
        operands: &[Constant],
        allowance: u64,
    ) -> FoldOutcome {
        let nat = |index: usize| match operands.get(index) {
            Some(Constant::Nat(value)) => Some(value),
            _ => None,
        };
        let int = |index: usize| match operands.get(index) {
            Some(Constant::Int(value)) => Some(value),
            _ => None,
        };
        let byte = |index: usize| match operands.get(index) {
            Some(Constant::Byte(value)) => Some(*value),
            _ => None,
        };
        let flt = |index: usize| match operands.get(index) {
            Some(Constant::Flt(value)) => Some(*value),
            _ => None,
        };
        let bool_ = |index: usize| match operands.get(index) {
            Some(Constant::Bool(value)) => Some(*value),
            _ => None,
        };
        let bin_x = |index: usize| match operands.get(index) {
            Some(Constant::Bin(Grain::X, value)) => Some(value),
            _ => None,
        };

        let compute = || -> Option<Result<Constant, TrapKind>> {
            Some(Ok(match operation {
                Operation::BoolAnd => Constant::Bool(bool_(0)? & bool_(1)?),
                Operation::BoolOr => Constant::Bool(bool_(0)? | bool_(1)?),
                Operation::BoolXor => Constant::Bool(bool_(0)? ^ bool_(1)?),
                Operation::BoolEql => Constant::Bool(bool_(0)? == bool_(1)?),
                Operation::BoolNeq => Constant::Bool(bool_(0)? != bool_(1)?),

                Operation::NatAdd => Constant::Nat(nat(0)? + nat(1)?),
                Operation::NatSub => Constant::Nat(nat(0)?.monus(nat(1)?)),
                Operation::NatMul => Constant::Nat(nat(0)?.mul_within(nat(1)?, allowance)?),
                Operation::NatDiv => {
                    return Some(scalar_result(nat(0)?.div(nat(1)?), Constant::Nat));
                }
                Operation::NatRem => {
                    return Some(scalar_result(nat(0)?.rem(nat(1)?), Constant::Nat));
                }
                Operation::NatAnd => Constant::Nat(nat(0)? & nat(1)?),
                Operation::NatOr => Constant::Nat(nat(0)? | nat(1)?),
                Operation::NatXor => Constant::Nat(nat(0)? ^ nat(1)?),
                Operation::NatShl => Constant::Nat(nat(0)?.shl_within(nat(1)?, allowance)?),
                Operation::NatShr => Constant::Nat(nat(0)? >> nat(1)?),
                Operation::NatEql => Constant::Bool(nat(0)? == nat(1)?),
                Operation::NatNeq => Constant::Bool(nat(0)? != nat(1)?),
                Operation::NatLt => Constant::Bool(nat(0)? < nat(1)?),
                Operation::NatLe => Constant::Bool(nat(0)? <= nat(1)?),

                Operation::IntAdd => Constant::Int(int(0)?.clone() + int(1)?.clone()),
                Operation::IntSub => Constant::Int(int(0)?.clone() - int(1)?.clone()),
                Operation::IntMul => Constant::Int(int(0)?.mul_within(int(1)?, allowance)?),
                Operation::IntDiv => {
                    return Some(scalar_result(int(0)?.div(int(1)?), Constant::Int));
                }
                Operation::IntRem => {
                    return Some(scalar_result(int(0)?.rem(int(1)?), Constant::Int));
                }
                Operation::IntAnd => Constant::Int(int(0)?.clone() & int(1)?.clone()),
                Operation::IntOr => Constant::Int(int(0)?.clone() | int(1)?.clone()),
                Operation::IntXor => Constant::Int(int(0)?.clone() ^ int(1)?.clone()),
                Operation::IntShl => Constant::Int(int(0)?.shl_within(nat(1)?, allowance)?),
                Operation::IntShr => Constant::Int(int(0)? >> nat(1)?),
                Operation::IntEql => Constant::Bool(int(0)? == int(1)?),
                Operation::IntNeq => Constant::Bool(int(0)? != int(1)?),
                Operation::IntLt => Constant::Bool(int(0)? < int(1)?),
                Operation::IntLe => Constant::Bool(int(0)? <= int(1)?),

                Operation::FltAdd(rounding) => Constant::Flt(flt(0)?.sum(flt(1)?, rounding)),
                Operation::FltSub(rounding) => Constant::Flt(flt(0)?.difference(flt(1)?, rounding)),
                Operation::FltMul(rounding) => Constant::Flt(flt(0)?.product(flt(1)?, rounding)),
                Operation::FltDiv(rounding) => Constant::Flt(flt(0)?.quotient(flt(1)?, rounding)),
                Operation::FltFma(rounding) => {
                    Constant::Flt(flt(0)?.fma(flt(1)?, flt(2)?, rounding))
                }
                Operation::FltRem => Constant::Flt(flt(0)? % flt(1)?),
                Operation::FltMin => Constant::Flt(flt(0)?.min(flt(1)?)),
                Operation::FltMax => Constant::Flt(flt(0)?.max(flt(1)?)),
                Operation::FltCopysign => Constant::Flt(flt(0)?.copysign(flt(1)?)),
                Operation::FltNeg => Constant::Flt(-flt(0)?),
                Operation::FltAbs => Constant::Flt(flt(0)?.abs()),
                Operation::FltSqrt(rounding) => Constant::Flt(flt(0)?.sqrt(rounding)),
                Operation::FltRoundIntegral(rounding) => {
                    Constant::Flt(flt(0)?.round_integral(rounding))
                }
                Operation::FltEql => Constant::Bool(flt(0)?.eql(flt(1)?)),
                Operation::FltNeq => Constant::Bool(flt(0)?.neq(flt(1)?)),
                Operation::FltLt => Constant::Bool(flt(0)?.lt(flt(1)?)),
                Operation::FltLe => Constant::Bool(flt(0)?.le(flt(1)?)),

                Operation::NatToInt => Constant::Int(Integer::from(nat(0)?.clone())),
                Operation::NatToFlt(rounding) => {
                    Constant::Flt(Floating::of_natural(nat(0)?, rounding))
                }
                Operation::IntToNat => {
                    return Some(scalar_result(Natural::try_from(int(0)?), Constant::Nat));
                }
                Operation::IntToFlt(rounding) => {
                    Constant::Flt(Floating::of_integer(int(0)?, rounding))
                }
                Operation::FltToNat => {
                    return Some(scalar_result(flt(0)?.to_natural(), Constant::Nat));
                }
                Operation::FltToInt => {
                    return Some(scalar_result(flt(0)?.to_integer(), Constant::Int));
                }
                Operation::FltMantissa => {
                    return Some(scalar_result(
                        flt(0)?.to_dyadic().map(|(mantissa, _)| mantissa),
                        Constant::Int,
                    ));
                }
                Operation::FltExponent => {
                    return Some(scalar_result(
                        flt(0)?
                            .to_dyadic()
                            .map(|(_, exponent)| Integer::from(exponent)),
                        Constant::Int,
                    ));
                }
                Operation::ByteToNat => Constant::Nat(Natural::from(byte(0)?)),
                // Declines past the carrier rather than masking, so this folder produces Core's value or none — never a third one. Core refuses the same operand, and the `below` field is what promises neither is reached; the two agree by construction rather than by both truncating.
                Operation::NatToByte => {
                    Constant::Byte(u8::try_from(u32::try_from(nat(0)?).ok()?).ok()?)
                }
                Operation::FltToLeBytes => Constant::Bin(Grain::X, flt(0)?.to_le_bytes()),
                Operation::FltOfLeBytes => {
                    return Some(scalar_result(
                        Floating::of_le_bytes(bin_x(0)?),
                        Constant::Flt,
                    ));
                }
            }))
        };
        fold_outcome(compute())
    }

    /// Constant-fold a sequence operation. Only packed-binary operations can fold — the constant domain has no list carrier, so list operations are always [`FoldOutcome::Unknown`] here (the evaluator interprets them over its own value domain instead). Elements stay grain-shaped: a byte grain yields `Byte`, a bit grain `Bool`.
    pub fn fold_sequence(operation: SequenceOp, operands: &[Constant]) -> FoldOutcome {
        let bin = |index: usize, grain: Grain| match operands.get(index) {
            Some(Constant::Bin(found, value)) if *found == grain => Some(value),
            _ => None,
        };
        let nat = |index: usize| match operands.get(index) {
            Some(Constant::Nat(value)) => Some(value),
            _ => None,
        };
        let byte = |index: usize| match operands.get(index) {
            Some(Constant::Byte(value)) => Some(*value),
            _ => None,
        };
        let bool_ = |index: usize| match operands.get(index) {
            Some(Constant::Bool(value)) => Some(*value),
            _ => None,
        };

        let compute = || -> Option<Result<Constant, TrapKind>> {
            Some(Ok(match operation {
                SequenceOp::BinLen(grain) => {
                    Constant::Nat(Natural::from(bin(0, grain)?.len(grain)))
                }
                SequenceOp::BinEql(grain) => Constant::Bool(bin(0, grain)? == bin(1, grain)?),
                SequenceOp::BinGet(Grain::X) => {
                    return Some(
                        match bin(0, Grain::X)?.byte(usize::try_from(nat(1)?).ok()?) {
                            Some(byte) => Ok(Constant::Byte(byte)),
                            None => Err(TrapKind::IndexOutOfBounds),
                        },
                    );
                }
                SequenceOp::BinGet(Grain::B) => {
                    return Some(
                        match bin(0, Grain::B)?.bit(usize::try_from(nat(1)?).ok()?) {
                            Some(bit) => Ok(Constant::Bool(bit)),
                            None => Err(TrapKind::IndexOutOfBounds),
                        },
                    );
                }
                // A window is `(start, length)`; the packed view takes a half-open range, so the end is computed here and an end past `usize` is the out-of-bounds it would have been anyway.
                SequenceOp::BinSlice(grain) => {
                    let value = bin(0, grain)?;
                    let start = usize::try_from(nat(1)?).ok()?;
                    let count = usize::try_from(nat(2)?).ok()?;
                    return Some(
                        match start
                            .checked_add(count)
                            .and_then(|end| value.slice(grain, start, end))
                        {
                            Some(value) => Ok(Constant::Bin(grain, value)),
                            None => Err(TrapKind::SliceOutOfBounds),
                        },
                    );
                }
                SequenceOp::BinAppend(Grain::X) => {
                    Constant::Bin(Grain::X, bin(0, Grain::X)?.append_byte(byte(1)?)?)
                }
                SequenceOp::BinAppend(Grain::B) => {
                    Constant::Bin(Grain::B, bin(0, Grain::B)?.append_bit(bool_(1)?))
                }
                SequenceOp::BinConcat(grain) => Constant::Bin(
                    grain,
                    Binary::concat(
                        (0..operands.len())
                            .map(|index| bin(index, grain))
                            .collect::<Option<Vec<_>>>()?,
                    ),
                ),
                // Split by grain for [`SequenceOp::BinAppend`]'s reason: the generator is a `Byte` constant at one and a `Bool` at the other, and only the grain says which to read.
                SequenceOp::BinReplicate(Grain::X) => Constant::Bin(
                    Grain::X,
                    Binary::replicate(Grain::X, byte(1)?, usize::try_from(nat(0)?).ok()?),
                ),
                SequenceOp::BinReplicate(Grain::B) => Constant::Bin(
                    Grain::B,
                    Binary::replicate(
                        Grain::B,
                        u8::from(bool_(1)?),
                        usize::try_from(nat(0)?).ok()?,
                    ),
                ),
                // Two literals of different lengths decline to fold rather than answering, exactly as the Core reducer declines them: the length is the type's to hold and the checker's to enforce, and a folder that decided it here would be answering for a run that is neither operand's.
                SequenceOp::BinAnd(grain) => {
                    let (left, right) = (bin(0, grain)?, bin(1, grain)?);
                    (left.bit_length() == right.bit_length())
                        .then(|| Constant::Bin(grain, left.and(right)))?
                }
                SequenceOp::BinOr(grain) => {
                    let (left, right) = (bin(0, grain)?, bin(1, grain)?);
                    (left.bit_length() == right.bit_length())
                        .then(|| Constant::Bin(grain, left.or(right)))?
                }
                SequenceOp::BinXor(grain) => {
                    let (left, right) = (bin(0, grain)?, bin(1, grain)?);
                    (left.bit_length() == right.bit_length())
                        .then(|| Constant::Bin(grain, left.xor(right)))?
                }
                // One condition at both grains: a byte run's bit length is eight times its count and always passes, while a bit run's is exactly what the bound states. A window holding whole bytes at an offset that is not one is repacked rather than shared.
                SequenceOp::BinReinterp(grain) => {
                    let run = bin(0, grain)?;
                    run.bit_length().is_multiple_of(8).then(|| {
                        Constant::Bin(
                            grain.other(),
                            match run.is_x_aligned() {
                                true => run.clone(),
                                false => Binary::from_bytes(run.to_packed_bytes()),
                            },
                        )
                    })?
                }
                SequenceOp::ListLen
                | SequenceOp::ListGet
                | SequenceOp::ListSlice
                | SequenceOp::ListAppend
                | SequenceOp::ListConcat
                | SequenceOp::ListBuild => return None,
            }))
        };
        fold_outcome(compute())
    }
}

fn fold_outcome(result: Option<Result<Constant, TrapKind>>) -> FoldOutcome {
    match result {
        Some(Ok(value)) => FoldOutcome::Value(value),
        Some(Err(trap)) => FoldOutcome::WouldTrap(trap),
        None => FoldOutcome::Unknown,
    }
}

/// Map a shared-semantics outcome ([`ScalarTrap`]) into the fold's constant/trap split.
///
/// `Err` is a *proven* trap and not a decline: the operation has an answer the carrier cannot hold, so the program traps wherever it runs, and recording that lets the trap stand at its execution point instead of surviving as an operation nobody can fold. Declining — leaving the term alone — is the outer `None`, and the two must not be confused.
fn scalar_result<T>(
    result: Result<T, ScalarTrap>,
    wrap: fn(T) -> Constant,
) -> Result<Constant, TrapKind> {
    match result {
        Ok(value) => Ok(wrap(value)),
        Err(ScalarTrap::DivisionByZero) => Err(TrapKind::DivisionByZero),
        Err(ScalarTrap::ConversionRange) => Err(TrapKind::ConversionRange),
        Err(ScalarTrap::Malformed) => Err(TrapKind::MalformedInput),
    }
}
