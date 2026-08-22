//! Blocks, statements, terminators, right-hand sides, functions, and recursive groups — the direct-style ANF structure of the representation.
//!
//! Every non-atomic computation is bound by a `Let` statement, so evaluation order is exactly statement order — the operand law's home, and the reason effect ordering is a structural property rather than per-pass discipline. Branch and fold bodies are blocks referenced by [`BlockId`]; every block has exactly one structural owner and block references are acyclic. Recursion happens only through function and recursive-group identities, never by making the block graph cyclic.

use super::{
    Atom, BlockId, ConstructorId, FamilyId, ForeignId, FunctionId, Operation, ProductId,
    RecGroupId, SequenceGrain, SequenceOp, StatementId, ValueId,
};

/// The single computation a `Let` statement evaluates and binds.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub enum Rhs {
    /// A computation-free rebinding of an atom.
    Alias(Atom),
    /// Application of a callee to its full, saturated argument list.
    Apply { callee: Atom, arguments: Vec<Atom> },
    /// A scalar intrinsic operation (see [`Operation`]).
    Operation {
        operation: Operation,
        operands: Vec<Atom>,
    },
    /// A packed-binary or list operation (see [`SequenceOp`]); the variadic forms hold their whole operand list here.
    Sequence {
        operation: SequenceOp,
        operands: Vec<Atom>,
    },
    /// Construct a product from its field atoms, in the schema's field order.
    Product {
        schema: ProductId,
        fields: Vec<Atom>,
    },
    /// Construct a variant value from its constructor and payload atoms.
    Construct {
        constructor: ConstructorId,
        fields: Vec<Atom>,
    },
    /// Read one field of a product value by position. The schema identity is carried so shape agreement is checkable and a projection from a known product folds only through a matching schema.
    Project {
        schema: ProductId,
        product: Atom,
        field: u32,
    },
    /// Match a variant scrutinee of the given family. Each arm binds its constructor's payload (positionally, in payload order) in its block; `default` covers every constructor without an arm. The arms plus the default are exhaustive over the family.
    MatchVariant {
        family: FamilyId,
        scrutinee: Atom,
        arms: Vec<VariantArm>,
        default: Option<BlockId>,
    },
    /// Dispatch on a `Bool` scrutinee. A semantic identity distinct from [`Rhs::SwitchNat`]; the carrier is the lowering's decision.
    SwitchBool {
        scrutinee: Atom,
        if_false: BlockId,
        if_true: BlockId,
    },
    /// Dispatch on a `Nat` scrutinee by literal key, falling through to `default`. Binder-free: a zero/successor match's predecessor is an explicit subtraction in the default block.
    SwitchNat {
        scrutinee: Atom,
        cases: Vec<NatCase>,
        default: BlockId,
    },
    /// `Nat` induction as a bounded fold: `zero` computes the base result, and `step` computes each successor result from the predecessor index and the induction hypothesis. A first-class loop form — O(1) native stack by construction.
    FoldNat {
        scrutinee: Atom,
        zero: BlockId,
        step: FoldNatStep,
    },
    /// Sequence elimination as a right fold: `empty` computes the base result, and `step` sees the current element, the suffix view after it, and the accumulated result of folding that suffix. A first-class loop form — O(1) native stack by construction.
    FoldSequence {
        grain: SequenceGrain,
        scrutinee: Atom,
        empty: BlockId,
        step: FoldSequenceStep,
    },
    /// Sequence elimination as a *case split*: `empty` computes the result for the empty sequence, and `cons` sees the head element and the suffix after it.
    ///
    /// The non-looping sibling of [`Rhs::FoldSequence`], and the form a cons arm that ignores its induction hypothesis erases to. It exists so that the erasure states what the Core eliminator states — one peel — instead of open-coding one out of an index dispatch and two bounded reads, which is a window convention two crates would then each have to hold.
    UnconsSequence {
        grain: SequenceGrain,
        scrutinee: Atom,
        empty: BlockId,
        cons: UnconsSequenceStep,
    },
    /// A mutable-cell operation (see [`CellOperation`]). A cell operation's identity is its program point: it is never deleted for an unused result, never duplicated, and never residualized by evaluation.
    Cell {
        operation: CellOperation,
        operands: Vec<Atom>,
    },
    /// A host-observable foreign call through the canonical row registered in the module (see [`super::Module::foreign`]). Binds one language value; the host-level result shape is reconstructed by the lowering.
    Foreign {
        foreign: ForeignId,
        operands: Vec<Atom>,
    },
    /// A call-like intrinsic (see [`Intrinsic`]).
    Intrinsic {
        intrinsic: Intrinsic,
        operands: Vec<Atom>,
    },
}

/// One arm of a [`Rhs::MatchVariant`]: the constructor it matches, the value binders receiving its payload (one per payload field, in order), and the block evaluated when it matches.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct VariantArm {
    pub constructor: ConstructorId,
    pub bindings: Vec<ValueId>,
    pub block: BlockId,
}

/// One case of a [`Rhs::SwitchNat`]: a literal key and the block it selects.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct NatCase {
    pub key: u32,
    pub block: BlockId,
}

/// The successor arm of a [`Rhs::FoldNat`]: binds the predecessor index and the induction hypothesis (the fold's result on the predecessor) in `block`.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct FoldNatStep {
    pub predecessor: ValueId,
    pub hypothesis: ValueId,
    pub block: BlockId,
}

/// The cons arm of a [`Rhs::UnconsSequence`]: binds the head element and the suffix after it in `block`.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct UnconsSequenceStep {
    pub element: ValueId,
    pub suffix: ValueId,
    pub block: BlockId,
}

/// The step arm of a [`Rhs::FoldSequence`]: binds the current element, the suffix view after it, and the accumulated result in `block`.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct FoldSequenceStep {
    pub element: ValueId,
    pub suffix: ValueId,
    pub accumulator: ValueId,
    pub block: BlockId,
}

/// A mutable-cell operation. Operand order: `New` takes the initial value, `Get` takes the cell, `Set` takes the cell then the new value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum CellOperation {
    New,
    Get,
    Set,
}

impl CellOperation {
    /// The exact operand count of this operation.
    pub fn arity(self) -> usize {
        match self {
            Self::New | Self::Get => 1,
            Self::Set => 2,
        }
    }
}

/// A call-like intrinsic. `ListMap` takes the list then the mapper — the carrier-first order of the whole sequence family — and runs the mapper once per element, in order.
///
/// `ListMap` stays a compiler intrinsic because its runtime helper fills a flat output array in place — a construction the language cannot express (there are no mutable-array operations, by design). The library definition (fold + append) was measured two orders of magnitude slower with a shape-quadratic result rope; proofs about map need no intrinsic (list-fold reduction peels rope shapes symbolically).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Intrinsic {
    ListMap,
}

impl Intrinsic {
    /// The exact operand count of this intrinsic.
    pub fn arity(self) -> usize {
        match self {
            Self::ListMap => 2,
        }
    }
}

/// One statement — of a block, or of the module's top-level item list.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub enum Statement {
    /// Bind `result` to the value of `rhs`.
    Let { result: ValueId, rhs: Rhs },
    /// Introduce a group of mutually recursive functions, all in scope in each other's bodies and in the rest of the enclosing scope. A nonrecursive function binding is a one-member group.
    Functions { functions: Vec<FunctionId> },
    /// Introduce a mutually recursive group mixing functions and eagerly computed values (see [`RecGroup`]).
    Rec { group: RecGroupId },
}

/// How a block leaves.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub enum Terminator {
    /// Yield an atom as the block's result.
    Return(Atom),
    /// Nonreturning process exit with the given code.
    Exit(Atom),
    /// An unreachable trap, seated where an arm was proved impossible.
    Unreachable,
}

/// An ordered list of statements evaluated in sequence, then a terminator.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct Block {
    pub statements: Vec<StatementId>,
    pub terminator: Terminator,
}

/// A function: its ordered runtime parameters and its body block. No capture list is stored — free values are derived by analysis, so no rewrite can leave a stale one behind.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct Function {
    pub debug_name: Option<String>,
    pub params: Vec<ValueId>,
    pub body: BlockId,
}

/// A mutually recursive group mixing functions and eagerly computed values. Every member is in scope in every member body and initializer and in the rest of the enclosing scope. Member order is source order; initializers run eagerly in that order, so an initializer may evaluate — directly, or through a function it calls — only an earlier computed member, while a reference from inside a function it constructs but does not call is dormant. The verifier admits exactly the language's recursion classes: cycles must pass through at least one function.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct RecGroup {
    pub functions: Vec<FunctionId>,
    pub values: Vec<RecValue>,
}

/// A computed member of a [`RecGroup`]: the value it binds and the block that eagerly initializes it, which may reference other members of the group.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct RecValue {
    pub value: ValueId,
    pub init: BlockId,
}
