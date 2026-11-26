//! Blocks, statements, terminators, right-hand sides, functions, and recursive groups — the direct-style ANF structure of the representation.
//!
//! Every non-atomic computation is bound by a `Let` statement, so evaluation order is exactly statement order — the operand law's home, and the reason effect ordering is a structural property rather than per-pass discipline. Branch and arm bodies are blocks referenced by [`BlockId`]; every block has exactly one structural owner and block references are acyclic. Recursion happens only through function and recursive-group identities, never by making the block graph cyclic.

use {
    super::{
        Atom, BlockId, ConstructorId, FamilyId, ForeignId, FunctionId, Operation, ProductId,
        RecGroupId, SequenceGrain, SequenceOp, StatementId, ValueId,
    },
    curios_common::Lineage,
    curios_num::{Integer, Natural},
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
    /// Dispatch on a `Bool` scrutinee. A semantic identity distinct from [`Rhs::Switch`]; the carrier is the lowering's decision.
    SwitchBool {
        scrutinee: Atom,
        if_false: BlockId,
        if_true: BlockId,
    },
    /// Dispatch on a scalar scrutinee by literal key, falling through to `default`, keyed by its carrier as Core's switch is ([`Keys`]). Binder-free: a zero/successor match's predecessor is an explicit subtraction in the default block.
    Switch {
        scrutinee: Atom,
        keys: Keys,
        default: BlockId,
    },
    /// Sequence elimination as a *case split*: `empty` computes the result for the empty sequence, and `cons` sees the head element and the suffix after it.
    ///
    /// The form Core's sequence case split erases to, and the one sequence elimination this alphabet names: a loop over a sequence is a tail recursion through it. It exists so that the erasure states what the Core eliminator states — one peel — instead of open-coding one out of an index dispatch and two bounded reads, which is a window convention two crates would then each have to hold.
    UnconsSequence {
        grain: SequenceGrain,
        scrutinee: Atom,
        empty: BlockId,
        cons: UnconsSequenceStep,
    },
    /// A write-once cell operation (see [`CellOperation`]). Its program point is observable: it is never deleted for an unused result, duplicated, or residualized by evaluation.
    Cell {
        operation: CellOperation,
        operands: Vec<Atom>,
    },
    /// A box operation (see [`BoxOperation`]). Its program point is observable, as a cell operation's is.
    Box {
        operation: BoxOperation,
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
    /// A suspension operation (see [`SuspensionOperation`]): what a forced description answers in place of its value when it waits, and the test and reads its forcer tells one apart by. None performs anything — a suspension is data, built by the description that suspends and read by whoever forced it.
    Suspension {
        operation: SuspensionOperation,
        operands: Vec<Atom>,
    },
}

impl Rhs {
    /// Whether this right-hand side names the carrier of what it produces: an operation and a sequence operation by what they compute, a list element excepted, and a construction and a cell, box, host, call-shaped or suspension operation by the shape it hands back. A call's result, a field read, a list element, an alias and a join name none, so only the bound value's type can say it holds a register's population: a binding states a carrier ([`RegisterShape`](crate::RegisterShape)) only where this does not hold, and the verifier refuses one where it does.
    pub fn names_its_carrier(&self) -> bool {
        match self {
            Self::Operation { .. }
            | Self::Product { .. }
            | Self::Construct { .. }
            | Self::Cell { .. }
            | Self::Box { .. }
            | Self::Foreign { .. }
            | Self::Intrinsic { .. }
            | Self::Suspension { .. } => true,
            Self::Sequence { operation, .. } => *operation != SequenceOp::ListGet,
            Self::Alias(_)
            | Self::Apply { .. }
            | Self::Project { .. }
            | Self::MatchVariant { .. }
            | Self::SwitchBool { .. }
            | Self::Switch { .. }
            | Self::UnconsSequence { .. } => false,
        }
    }
}

/// One arm of a [`Rhs::MatchVariant`]: the constructor it matches, the value binders receiving its payload (one per payload field, in order), and the block evaluated when it matches.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct VariantArm {
    pub constructor: ConstructorId,
    pub bindings: Vec<ValueId>,
    pub block: BlockId,
}

/// The keys of a [`Rhs::Switch`], each with the block it selects, by the carrier they are values of: a natural's or an integer's.
///
/// A key is the value Core dispatches on, whole: a key is as unbounded as the value it is compared with, and which keys a branch table indexes is the lowering's to decide, where the table is built.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub enum Keys {
    Nat(Vec<Case<Natural>>),
    Int(Vec<Case<Integer>>),
}

impl Keys {
    /// The block each key selects, in key order.
    pub fn blocks(&self) -> impl Iterator<Item = BlockId> + '_ {
        let (naturals, integers): (&[Case<Natural>], &[Case<Integer>]) = match self {
            Keys::Nat(cases) => (cases, &[]),
            Keys::Int(cases) => (&[], cases),
        };

        naturals
            .iter()
            .map(|case| case.block)
            .chain(integers.iter().map(|case| case.block))
    }

    /// The same keys, each selecting the block `block` maps its own to.
    pub fn with_blocks(&self, mut block: impl FnMut(BlockId) -> BlockId) -> Self {
        match self {
            Keys::Nat(cases) => Keys::Nat(Case::with_blocks(cases, &mut block)),
            Keys::Int(cases) => Keys::Int(Case::with_blocks(cases, &mut block)),
        }
    }
}

/// One case of a [`Rhs::Switch`]: a literal key and the block it selects.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct Case<K> {
    pub key: K,
    pub block: BlockId,
}

impl<K: Clone> Case<K> {
    fn with_blocks(cases: &[Self], block: &mut impl FnMut(BlockId) -> BlockId) -> Vec<Self> {
        cases
            .iter()
            .map(|case| Case {
                key: case.key.clone(),
                block: block(case.block),
            })
            .collect()
    }
}

/// The cons arm of a [`Rhs::UnconsSequence`]: binds the head element and the suffix after it in `block`.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct UnconsSequenceStep {
    pub element: ValueId,
    pub suffix: ValueId,
    pub block: BlockId,
}

/// A write-once cell operation. `New` has no operands, `Fill` takes the cell then its proposed value, and `Poll` takes the cell and answers `some` or `none`: `some` holding the value where its row keeps a field for it, and nothing where the row keeps none, the value being a proof there.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum CellOperation {
    New,
    Fill,
    Poll {
        some: ConstructorId,
        none: ConstructorId,
    },
}

impl CellOperation {
    /// The exact operand count of this operation.
    pub fn arity(self) -> usize {
        match self {
            Self::New => 0,
            Self::Poll { .. } => 1,
            Self::Fill => 2,
        }
    }
}

/// A box operation. `New` takes the value the box is made holding, `Get` takes the box, and `Set` takes the box then the value it holds from then on.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum BoxOperation {
    New,
    Get,
    Set,
}

impl BoxOperation {
    /// The exact operand count of this operation.
    pub fn arity(self) -> usize {
        match self {
            Self::New | Self::Get => 1,
            Self::Set => 2,
        }
    }
}

/// What a forced description answers when it suspends, and how its forcer reads one.
///
/// A suspension holds the handles it waits on, the cells it waits on to be filled — which only the program fills, and only what advances it inside the program reads — the description that cancels it, what runs first when it resumes — the rest of the description that suspended, or a suspension nested in it — and the frames: the continuations of the binds it passed on its way out, innermost first, each applied to what the one before it yields. A bind that forces a suspension appends its continuation rather than wrapping the rest in a new description, so resuming walks the frames in a loop, and a chain of binds pays once for its depth however often its innermost action suspends.
///
/// `New` takes the handles, the cells, the cancel and the rest, and builds a suspension with no frames. `Later` takes the token a host call answered in place of its reply, a list holding one handle, and `again`, which makes the description calling the row once more with a token, dropping it where its second operand is `true` and asking for its answer where it is `false`: it builds the suspension on the token with `again(token, true)` as its cancel and `again(token, false)` as its rest, the one shape every call answered later waits in, as one step the lowering keeps out of line. `Carry` takes a suspension and a bind's continuation and answers the suspension with the continuation appended to its frames: what a bind answers of a suspension its action answered, as one step the lowering keeps out of line, so the path a bind takes when nothing suspends stays small. `Build` takes all five parts, and is how resuming carries a suspension on — a nested one wrapped, its frames still to run. `Is` takes anything a description may answer and answers whether it is a suspension, a `Bool`; `On`, `Filled`, `Cancel`, `Next` and `Frames` take a suspension and read that part.
///
/// `New` and `Later` are kept apart from `Carry` and `Build` because they are the only operations that make a suspension where none existed: a program whose reachable code holds neither holds no suspension at all, which is what lets every `Is` in it fold to false.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum SuspensionOperation {
    New,
    Later,
    Carry,
    Build,
    Is,
    On,
    Filled,
    Cancel,
    Next,
    Frames,
}

impl SuspensionOperation {
    /// The exact operand count of this operation.
    pub fn arity(self) -> usize {
        match self {
            Self::New => 4,
            Self::Later | Self::Carry => 2,
            Self::Build => 5,
            Self::Is | Self::On | Self::Filled | Self::Cancel | Self::Next | Self::Frames => 1,
        }
    }
}

/// A call-like intrinsic. `ListMap` takes the list then the mapper — the carrier-first order of the whole sequence family — and runs the mapper once per element, in order.
///
/// `ListMap` stays a compiler intrinsic because its runtime helper fills a flat output array in place — a construction the language cannot express (there are no mutable-array operations, by design). A library definition (fold + append) builds a shape-quadratic result rope; proofs about map need no intrinsic (list-fold reduction peels rope shapes symbolically).
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
    /// A call to a host row that diverges — `proc_exit` — with its wire operands. It ends the block with no result and nothing after it, so no continuation is ever made for it.
    Halt {
        foreign: ForeignId,
        operands: Vec<Atom>,
    },
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
    /// The function of a declaration's text this one was made of — stamped where erasure mints it, and kept by every copy, since it is a fact of where the function came from that no later shape recovers ([`Lineage`]). `None` for a function of no declaration's text.
    pub lineage: Option<Lineage>,
    pub debug_name: Option<String>,
    pub params: Vec<ValueId>,
    pub body: BlockId,
    /// What the function is part of — stamped where it is born, and kept by every copy, since it is a fact of where the function came from that no later shape recovers.
    pub role: Role,
    /// Whether the definition this function was erased from terminates on every input, together with everything it reaches — as the certifier's own record classifies it (`curios_core::Certification`), carried rather than re-derived.
    ///
    /// **Carried, because below Core there is nothing to derive it from.** Termination is decided by the certifier's walk and filed as its record, and it is already load-bearing above: the obligations that let erasure delete a proof or a type rest on it. A second derivation down here would be a second opinion about a question the trusted base has already answered, and two answers is how the stages come to disagree.
    ///
    /// **Every function of the definition's evaluation, nested ones included.** A record classifies a definition `Total` only where every recursive group its term encloses descends, and closed over everything it mentions, so what the verdict says of the definition it says of each function its evaluation runs: its lambdas and local groups at any depth, the loops erasure builds to compute its intrinsics, and the closures computing it produced. It says nothing of a [`Role::Performance`] or of [`Role::Machinery`]: a description of an effect that runs forever is a total value.
    ///
    /// **Marked on the sealed program, never by erasure.** Erasure leaves every flag false, so an erased form a unit files or the prelude archives carries none; the marks are made once, where the program the back half lowers meets the record of every unit it was erased from and of its entry, by [`Module::mark_total`](crate::Module::mark_total). Nothing reads the flag before then.
    ///
    /// False is the safe reading and the default: a definition the certifier did not find total is `Partial`, and so is anything no record classifies.
    pub total: bool,
}

/// What a function is part of, which decides what the certifier's verdict on the definition it was erased from says of it ([`Function::total`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Role {
    /// Part of computing the value of the definition it was erased from: its lambdas and local groups, the loops erasure builds for its intrinsics, and the closures computing it produced.
    Evaluation,
    /// An erased effect description's performance — stamped where descriptions are born, the erasure's `thunk`, and read by the closed evaluator, which names rather than copies the continuation a replacement holding one names, and declines a copy that leads back to the candidate's function: what a description runs after its performance is the program's own code.
    Performance,
    /// What erasure builds to run performances: the loop that resumes a suspension through its frames, the surfacing of one to the host, a crossing's step and its retry. It belongs to no definition's evaluation.
    Machinery,
}

/// A mutually recursive group mixing functions and computed values. Every member is in scope in every member body and initializer and in the rest of the enclosing scope. A computed member is forced by need: its initializer runs the first time something reads the member, so members may reference one another in any order, and what the verifier refuses is an initializer that evaluates itself — directly, or through the functions it applies — and one that performs an effect, which forcing could not keep in its place. Member order is source order and decides nothing but spelling.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct RecGroup {
    pub functions: Vec<FunctionId>,
    pub values: Vec<RecValue>,
}

/// A computed member of a [`RecGroup`]: the value it binds and the block that initializes it when first forced, which may reference other members of the group.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct RecValue {
    pub value: ValueId,
    pub init: BlockId,
}
