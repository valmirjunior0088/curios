//! The behavior half of the semantic oracle: one total function over the
//! closed operation alphabet.
//!
//! Every query matches the closed alphabets without a fallback arm, so a
//! newly added operation cannot be silently misclassified — the compiler
//! rejects the unhandled variant. Behavior reported here is *node-local*: it
//! excludes the bodies of functions an `Apply` invokes, the callback an
//! intrinsic runs, and the sub-blocks a match, switch, or fold evaluates;
//! composing those to a fixed point is the effect summary's job
//! ([`super::Summary`]), and this module supplies the leaves it joins.
//!
//! Arity is not restated here — it lives on the operation enums where the
//! verifier already reads it. The fold half of the oracle (operation ×
//! constant operands → value / would-trap / unknown) lands with its consumer,
//! partial evaluation.

use super::{CellOperation, Constant, Intrinsic, Operation, Rhs, SequenceOp, Terminator};

/// What allocating a value commits a pass to. Immutable allocation is not
/// language-observable and may be discarded or duplicated; mutable allocation
/// (a cell) may not. Ordered by severity so [`join`](Allocation::join) is
/// `max`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum Allocation {
    #[default]
    None,
    /// A value whose identity is not observable (a tuple, a variant, a slice
    /// view, a list).
    Immutable,
    /// A mutable allocation whose identity a program observes (a cell).
    Mutable,
}

impl Allocation {
    /// Whether the allocating computation cannot be discarded or duplicated
    /// on the unused-result rule alone.
    pub fn is_observable(self) -> bool {
        matches!(self, Allocation::Mutable)
    }

    pub fn join(self, other: Allocation) -> Allocation {
        self.max(other)
    }
}

/// The language-observable events a computation may cause. Every dimension is
/// independent — a three-valued purity label would lose distinctions a sound
/// pass needs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ObservableBehavior {
    /// May raise a runtime trap (out-of-bounds access, division by zero, an
    /// `Unreachable` terminator).
    pub may_trap: bool,
    /// May fail to terminate. No primitive diverges; this is acquired through
    /// the call graph (a recursive component) by the effect summary.
    pub may_diverge: bool,
    /// May terminate the process (an `Exit` terminator).
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

    /// Whether any observable event may occur — the signal that a computation
    /// run for effect must be preserved even when its result is unused.
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

    /// This behavior with divergence forced on — how the effect summary marks
    /// a recursive component whose termination it cannot prove.
    pub fn with_divergence(mut self) -> ObservableBehavior {
        self.may_diverge = true;
        self
    }
}

/// The operational actions a computation performs that are not themselves
/// observable events: what it allocates.
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

    /// The conservative top of the lattice: a computation that may do
    /// anything. Used for an unknown callee or callback.
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

    /// Whether evaluating this computation for effect is observable — an
    /// unused binding is safe to delete only when this is false.
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

/// The behavior contract: a zero-sized namespace of total functions over the
/// closed alphabets.
pub struct Semantics;

impl Semantics {
    /// The node-local behavior of a right-hand side — its own operation only,
    /// excluding callees, callbacks, and sub-blocks.
    pub fn local_behavior(rhs: &Rhs) -> LocalBehavior {
        match rhs {
            // Pure structure: a call's effects are its callee's summary; a
            // match, switch, or Nat fold contributes only its sub-blocks';
            // aliasing and projection are total and allocation-free.
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
            Rhs::Foreign { .. } => LocalBehavior::host(),
            Rhs::Intrinsic { intrinsic, .. } => Self::intrinsic(*intrinsic),
            // Building an aggregate allocates an immutable value; a sequence
            // fold materializes suffix views.
            Rhs::Product { .. } | Rhs::Construct { .. } | Rhs::FoldSequence { .. } => {
                LocalBehavior::alloc(Allocation::Immutable)
            }
        }
    }

    /// The behavior of a scalar operation. Division and remainder may trap
    /// (zero divisor; signed overflow), and the float-to-integer conversions
    /// may trap on non-finite or out-of-range input; every other scalar
    /// operation is total and allocation-free.
    pub fn operation(operation: Operation) -> LocalBehavior {
        use Operation::*;
        match operation {
            NatDiv | NatRem | IntDiv | IntRem | FltToNat | FltToInt => LocalBehavior::trap(),
            BlnAnd | BlnOr | BlnXor | BlnEql | BlnNeq | NatEql | NatNeq | NatAdd | NatSub
            | NatMul | NatLt | NatGt | NatLte | NatGte | NatAnd | NatOr | NatXor | NatShl
            | NatShr | ByteToNat | NatToByte | ByteEql | ByteLt | ByteLte | ByteGt | ByteGte
            | IntEql | IntNeq | IntAdd | IntSub | IntMul | IntLt | IntGt | IntLte | IntGte
            | IntAnd | IntOr | IntXor | IntShl | IntShr | FltAdd | FltSub | FltMul | FltDiv
            | FltRem | FltEql | FltNeq | FltLt | FltGt | FltLte | FltGte | FltMin | FltMax
            | FltNeg | FltAbs | FltSqrt | FltFloor | FltCeil | FltTrunc | FltNearest | NatToInt
            | NatToFlt | IntToNat | IntToFlt | FltToLeBytes | FltOfLeBytes | IoEql => {
                LocalBehavior::pure()
            }
        }
    }

    /// The behavior of a sequence operation. Indexing may trap out of bounds;
    /// slicing may trap and allocates a view; append, concat, and build
    /// allocate; length and equality are total.
    pub fn sequence(operation: SequenceOp) -> LocalBehavior {
        use SequenceOp::*;
        match operation {
            BinGet(_) | LstGet => LocalBehavior::trap(),
            BinSlice(_) | LstSlice => LocalBehavior::trap().with_alloc(Allocation::Immutable),
            BinAppend(_) | BinConcat(_) | LstAppend | LstConcat | LstBuild => {
                LocalBehavior::alloc(Allocation::Immutable)
            }
            BinLen(_) | LstLen | BinEql(_) => LocalBehavior::pure(),
        }
    }

    /// The behavior of a cell operation: creation allocates a mutable
    /// identity, reading observes state, writing mutates it. None is
    /// removable on an unused result alone.
    pub fn cell(operation: CellOperation) -> LocalBehavior {
        match operation {
            CellOperation::New => LocalBehavior::alloc(Allocation::Mutable),
            CellOperation::Get => LocalBehavior::state_read(),
            CellOperation::Set => LocalBehavior::state_write(),
        }
    }

    /// The node-local behavior of an intrinsic. `LstMap` allocates its result
    /// list; the mapper's own behavior is composed by the effect summary.
    pub fn intrinsic(intrinsic: Intrinsic) -> LocalBehavior {
        match intrinsic {
            Intrinsic::LstMap => LocalBehavior::alloc(Allocation::Immutable),
        }
    }

    /// The observable behavior of a block terminator.
    pub fn terminator(terminator: &Terminator) -> ObservableBehavior {
        match terminator {
            Terminator::Return(_) => ObservableBehavior::none(),
            Terminator::Exit(_) => ObservableBehavior {
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

/// The outcome of constant-folding an operation over fully-known operands.
/// The three cases stay distinct because control-flow simplification depends
/// on the difference: a known trap must survive as an explicit computation —
/// never dead code, never a compile-time panic, never [`Unknown`].
///
/// [`Unknown`]: FoldOutcome::Unknown
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FoldOutcome {
    /// The operation evaluates to this constant.
    Value(Constant),
    /// The operation is known to trap at runtime; the optimizer must keep it
    /// as an explicit residual computation.
    WouldTrap(TrapKind),
    /// Nothing is known: an operand is not a constant, the operation has no
    /// constant carrier (a list operation), or the fold deliberately declines
    /// (a float min/max with a NaN operand, which Rust and wasm disagree on).
    Unknown,
}

/// Why a folded operation would trap.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TrapKind {
    /// Integer division or remainder by a zero divisor.
    DivisionByZero,
    /// Signed integer division overflow (`i32::MIN / -1`).
    IntegerOverflow,
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
    /// Constant-fold a scalar operation over its operands, under the numeric
    /// law: exact `u32`/`i32` (add, subtract — monus for `Nat` — and multiply
    /// wrap the full 32-bit carrier and never trap) and bit-preserving
    /// binary32. Comparisons yield a [`Constant::Bln`]; the `0`/`1` carrier is
    /// the lowering's decision. i31 appears nowhere here.
    pub fn fold_operation(operation: Operation, operands: &[Constant]) -> FoldOutcome {
        use Operation::*;

        let nat = |index: usize| match operands.get(index) {
            Some(Constant::Nat(value)) => Some(*value),
            _ => None,
        };
        let int = |index: usize| match operands.get(index) {
            Some(Constant::Int(value)) => Some(*value),
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
        let bln = |index: usize| match operands.get(index) {
            Some(Constant::Bln(value)) => Some(*value),
            _ => None,
        };
        let io = |index: usize| match operands.get(index) {
            Some(Constant::Io(value)) => Some(*value),
            _ => None,
        };
        let bin_x = |index: usize| match operands.get(index) {
            Some(Constant::Bin(curios_base::Grain::X, value)) => Some(value),
            _ => None,
        };

        let compute = || -> Option<Result<Constant, TrapKind>> {
            Some(Ok(match operation {
                BlnAnd => Constant::Bln(bln(0)? & bln(1)?),
                BlnOr => Constant::Bln(bln(0)? | bln(1)?),
                BlnXor => Constant::Bln(bln(0)? ^ bln(1)?),
                BlnEql => Constant::Bln(bln(0)? == bln(1)?),
                BlnNeq => Constant::Bln(bln(0)? != bln(1)?),

                NatAdd => Constant::Nat(curios_base::nat_add(nat(0)?, nat(1)?)),
                NatSub => Constant::Nat(curios_base::nat_sub(nat(0)?, nat(1)?)),
                NatMul => Constant::Nat(curios_base::nat_mul(nat(0)?, nat(1)?)),
                NatDiv => {
                    return Some(div_result(
                        curios_base::nat_div(nat(0)?, nat(1)?),
                        Constant::Nat,
                    ));
                }
                NatRem => {
                    return Some(div_result(
                        curios_base::nat_rem(nat(0)?, nat(1)?),
                        Constant::Nat,
                    ));
                }
                NatAnd => Constant::Nat(nat(0)? & nat(1)?),
                NatOr => Constant::Nat(nat(0)? | nat(1)?),
                NatXor => Constant::Nat(nat(0)? ^ nat(1)?),
                NatShl => Constant::Nat(curios_base::nat_shl(nat(0)?, nat(1)?)),
                NatShr => Constant::Nat(curios_base::nat_shr(nat(0)?, nat(1)?)),
                NatEql => Constant::Bln(nat(0)? == nat(1)?),
                NatNeq => Constant::Bln(nat(0)? != nat(1)?),
                NatLt => Constant::Bln(nat(0)? < nat(1)?),
                NatGt => Constant::Bln(nat(0)? > nat(1)?),
                NatLte => Constant::Bln(nat(0)? <= nat(1)?),
                NatGte => Constant::Bln(nat(0)? >= nat(1)?),

                ByteEql => Constant::Bln(byte(0)? == byte(1)?),
                ByteLt => Constant::Bln(byte(0)? < byte(1)?),
                ByteGt => Constant::Bln(byte(0)? > byte(1)?),
                ByteLte => Constant::Bln(byte(0)? <= byte(1)?),
                ByteGte => Constant::Bln(byte(0)? >= byte(1)?),

                IntAdd => Constant::Int(curios_base::int_add(int(0)?, int(1)?)),
                IntSub => Constant::Int(curios_base::int_sub(int(0)?, int(1)?)),
                IntMul => Constant::Int(curios_base::int_mul(int(0)?, int(1)?)),
                IntDiv => {
                    return Some(div_result(
                        curios_base::int_div(int(0)?, int(1)?),
                        Constant::Int,
                    ));
                }
                IntRem => {
                    return Some(div_result(
                        curios_base::int_rem(int(0)?, int(1)?),
                        Constant::Int,
                    ));
                }
                IntAnd => Constant::Int(int(0)? & int(1)?),
                IntOr => Constant::Int(int(0)? | int(1)?),
                IntXor => Constant::Int(int(0)? ^ int(1)?),
                IntShl => Constant::Int(curios_base::int_shl(int(0)?, int(1)?)),
                IntShr => Constant::Int(curios_base::int_shr(int(0)?, int(1)?)),
                IntEql => Constant::Bln(int(0)? == int(1)?),
                IntNeq => Constant::Bln(int(0)? != int(1)?),
                IntLt => Constant::Bln(int(0)? < int(1)?),
                IntGt => Constant::Bln(int(0)? > int(1)?),
                IntLte => Constant::Bln(int(0)? <= int(1)?),
                IntGte => Constant::Bln(int(0)? >= int(1)?),

                FltAdd => Constant::Flt(flt(0)? + flt(1)?),
                FltSub => Constant::Flt(flt(0)? - flt(1)?),
                FltMul => Constant::Flt(flt(0)? * flt(1)?),
                FltDiv => Constant::Flt(flt(0)? / flt(1)?),
                FltRem => Constant::Flt(flt(0)? % flt(1)?),
                FltMin => Constant::Flt(fold_min(flt(0)?, flt(1)?)?),
                FltMax => Constant::Flt(fold_max(flt(0)?, flt(1)?)?),
                FltNeg => Constant::Flt(-flt(0)?),
                FltAbs => Constant::Flt(flt(0)?.abs()),
                FltSqrt => Constant::Flt(flt(0)?.sqrt()),
                FltFloor => Constant::Flt(flt(0)?.floor()),
                FltCeil => Constant::Flt(flt(0)?.ceil()),
                FltTrunc => Constant::Flt(flt(0)?.trunc()),
                FltNearest => Constant::Flt(flt(0)?.nearest()),
                FltEql => Constant::Bln(flt(0)?.eql(flt(1)?)),
                FltNeq => Constant::Bln(flt(0)?.neq(flt(1)?)),
                FltLt => Constant::Bln(flt(0)?.lt(flt(1)?)),
                FltGt => Constant::Bln(flt(0)?.gt(flt(1)?)),
                FltLte => Constant::Bln(flt(0)?.lte(flt(1)?)),
                FltGte => Constant::Bln(flt(0)?.gte(flt(1)?)),

                IoEql => Constant::Bln(io(0)? == io(1)?),

                NatToInt => Constant::Int(curios_base::nat_to_int(nat(0)?)),
                NatToFlt => Constant::Flt(curios_base::Flt::from_f32(nat(0)? as f32)),
                IntToNat => Constant::Nat(curios_base::int_to_nat(int(0)?)),
                IntToFlt => Constant::Flt(curios_base::Flt::from_f32(int(0)? as f32)),
                FltToNat => return Some(flt_to_nat(flt(0)?)),
                FltToInt => return Some(flt_to_int(flt(0)?)),
                ByteToNat => Constant::Nat(byte(0)? as u32),
                NatToByte => Constant::Byte(nat(0)? as u8),
                FltToLeBytes => Constant::Bin(
                    curios_base::Grain::X,
                    curios_base::PackedBin::from_bytes(flt(0)?.to_f32().to_le_bytes().to_vec()),
                ),
                FltOfLeBytes => return Some(flt_of_le_bytes(bin_x(0)?)),
            }))
        };
        fold_outcome(compute())
    }

    /// Constant-fold a sequence operation. Only packed-binary operations can
    /// fold — the constant domain has no list carrier, so list operations are
    /// always [`FoldOutcome::Unknown`] here (the evaluator interprets them
    /// over its own value domain instead). Elements stay grain-shaped: a byte
    /// grain yields `Byte`, a bit grain `Bln`.
    pub fn fold_sequence(operation: SequenceOp, operands: &[Constant]) -> FoldOutcome {
        use {SequenceOp::*, curios_base::Grain};

        let bin = |index: usize, grain: Grain| match operands.get(index) {
            Some(Constant::Bin(found, value)) if *found == grain => Some(value),
            _ => None,
        };
        let nat = |index: usize| match operands.get(index) {
            Some(Constant::Nat(value)) => Some(*value),
            _ => None,
        };
        let byte = |index: usize| match operands.get(index) {
            Some(Constant::Byte(value)) => Some(*value),
            _ => None,
        };
        let bln = |index: usize| match operands.get(index) {
            Some(Constant::Bln(value)) => Some(*value),
            _ => None,
        };

        let compute = || -> Option<Result<Constant, TrapKind>> {
            Some(Ok(match operation {
                BinLen(grain) => Constant::Nat(bin(0, grain)?.len(grain) as u32),
                BinEql(grain) => Constant::Bln(bin(0, grain)? == bin(1, grain)?),
                BinGet(Grain::X) => {
                    return Some(match bin(0, Grain::X)?.byte(nat(1)? as usize) {
                        Some(byte) => Ok(Constant::Byte(byte)),
                        None => Err(TrapKind::IndexOutOfBounds),
                    });
                }
                BinGet(Grain::B) => {
                    return Some(match bin(0, Grain::B)?.bit(nat(1)? as usize) {
                        Some(bit) => Ok(Constant::Bln(bit)),
                        None => Err(TrapKind::IndexOutOfBounds),
                    });
                }
                BinSlice(grain) => {
                    let value = bin(0, grain)?;
                    return Some(
                        match value.slice(grain, nat(1)? as usize, nat(2)? as usize) {
                            Some(value) => Ok(Constant::Bin(grain, value)),
                            None => Err(TrapKind::SliceOutOfBounds),
                        },
                    );
                }
                BinAppend(Grain::X) => {
                    Constant::Bin(Grain::X, bin(0, Grain::X)?.append_byte(byte(1)?)?)
                }
                BinAppend(Grain::B) => {
                    Constant::Bin(Grain::B, bin(0, Grain::B)?.append_bit(bln(1)?))
                }
                BinConcat(grain) => Constant::Bin(
                    grain,
                    curios_base::PackedBin::concat(
                        (0..operands.len())
                            .map(|index| bin(index, grain))
                            .collect::<Option<Vec<_>>>()?,
                    ),
                ),
                LstLen | LstGet | LstSlice | LstAppend | LstConcat | LstBuild => return None,
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

/// Map a shared-semantics division outcome ([`curios_base::DivTrap`]) into
/// the fold's constant/trap split.
fn div_result<T>(
    result: Result<T, curios_base::DivTrap>,
    wrap: fn(T) -> Constant,
) -> Result<Constant, TrapKind> {
    match result {
        Ok(value) => Ok(wrap(value)),
        Err(curios_base::DivTrap::DivisionByZero) => Err(TrapKind::DivisionByZero),
        Err(curios_base::DivTrap::Overflow) => Err(TrapKind::IntegerOverflow),
    }
}

/// Float min/max decline on a NaN operand (see [`curios_base::flt_min`]).
fn fold_min(left: curios_base::Flt, right: curios_base::Flt) -> Option<curios_base::Flt> {
    curios_base::flt_min(left.to_f32(), right.to_f32()).map(curios_base::Flt::from_f32)
}

fn fold_max(left: curios_base::Flt, right: curios_base::Flt) -> Option<curios_base::Flt> {
    curios_base::flt_max(left.to_f32(), right.to_f32()).map(curios_base::Flt::from_f32)
}

fn flt_to_nat(value: curios_base::Flt) -> Result<Constant, TrapKind> {
    curios_base::flt_to_nat(value.to_f32())
        .map(Constant::Nat)
        .ok_or(TrapKind::ConversionRange)
}

/// Truncate to `i32`, trapping outside `[-2^31, 2^31)`.
fn flt_to_int(value: curios_base::Flt) -> Result<Constant, TrapKind> {
    curios_base::flt_to_int(value.to_f32())
        .map(Constant::Int)
        .ok_or(TrapKind::ConversionRange)
}

/// Decode a little-endian binary32, trapping unless exactly four bytes.
fn flt_of_le_bytes(value: &curios_base::PackedBin) -> Result<Constant, TrapKind> {
    value
        .to_bytes()
        .as_deref()
        .and_then(|bytes| <[u8; 4]>::try_from(bytes).ok())
        .map(|le_bytes| Constant::Flt(curios_base::Flt::from_f32(f32::from_le_bytes(le_bytes))))
        .ok_or(TrapKind::MalformedInput)
}
