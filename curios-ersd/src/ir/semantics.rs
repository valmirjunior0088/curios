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

use super::{CellOperation, Intrinsic, Operation, Rhs, SequenceOp, Terminator};

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
