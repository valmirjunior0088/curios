//! What the graph is made of: the identities, the operands, and the nodes that bind a value or transfer control.

use {
    super::Intrinsic,
    curios_abi::ForeignFunction,
    curios_num::{Binary, Floating, Grain, Integer, Natural},
    curios_utilities::id,
    std::{collections::BTreeMap, fmt, sync::Arc},
};

/// How many value bits a small `Nat` or `Int` holds — the width of an `i31ref`'s payload, read signed, and the one width in the whole pipeline that is a fact about the target rather than about the language.
///
/// A `Nat` and an `Int` share one runtime form, and it is a reference: an i31 when the value lies in `[-2³⁰, 2³⁰)`, and a boxed magnitude otherwise, never both. This width is where the two meet, so it bounds the fast path every arithmetic lowering keeps inline and nothing else — no value refuses at it. Above this crate nothing knows the number: `curios-core` computes unbounded and `curios-ersd`'s constants carry whatever the theory produced.
pub const ENVELOPE_BITS: i32 = 31;

/// Whether `value` is a `Nat` the i31 holds: below `2³⁰`, since the i31 is read signed for both carriers.
///
/// Stated here beside the width because two readers ask it — the emitter, which spells a small constant as an i31 and any other as a boxed magnitude, and the representation analysis, which lets only a small literal ride a machine word.
pub fn nat_is_small(value: &Natural) -> bool {
    u32::try_from(value)
        .ok()
        .is_some_and(|value| value >> (ENVELOPE_BITS - 1) == 0)
}

/// Whether `value` is an `Int` the i31 holds: in range exactly when the bit below the sign agrees with it.
pub fn int_is_small(value: &Integer) -> bool {
    i32::try_from(value)
        .ok()
        .is_some_and(|value| value >> (ENVELOPE_BITS - 1) == value >> ENVELOPE_BITS)
}

// Sigils follow the naming scheme shared with `curios-ersd` and `curios-wasm` — see `documentation/design/tools/a-printer-states-each-fact-once-where-it-is-bound.md`.
id!(NodeId, "~n");
id!(ValueId, "~v");
id!(FunctionId, "~f");
id!(ContinuationId, "~k");
id!(RowId, "~r");

impl FunctionId {
    pub fn from_index(index: usize) -> Self {
        Self(index as u32)
    }
}

/// A literal operand. `Flt` holds the bitwise [`Floating`] rather than an `f64` so that the derived equality is identity on the bit pattern: under IEEE equality a NaN literal is unequal to itself, and a pass comparing an edge it rebuilt against the edge it read would report a change on every round, running the fixpoint to its backstop on any module carrying a `NaN` through a jump.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Nat(Natural),
    Int(Integer),
    Flt(Floating),
    Bin(Grain, Binary),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Value(ValueId),
    Fun(FunctionId),
    Literal(Literal),
    /// No value: a reference slot belonging to a wider constructor than the edge or call carrying it fills. It travels as null, which is what the field the constructor never wrote holds, and every reference position admits it; a register slot is padded with its zero literal instead, since a register has no null and the field holds zero there. [`Module::pad`](super::Module::pad) is the one place that chooses.
    ///
    /// The register the *parameter* is held at is decided by `represent` during backend lowering, from the uses of the parameter it feeds — strictly after the passes that create fillers — so a filler reaching a register-held parameter is the emitter's to materialise as that register's zero.
    Filler,
}

#[derive(Debug, Clone)]
pub enum ValueExpr {
    Literal(Literal),
    List(Vec<Atom>),
    Tuple(Vec<Atom>),
    /// A construction of a *nominal* row — a variant family or a product schema — at that row's full width, padded with [`Atom::Filler`] wherever the constructor building it is narrower than the row. A family's slot zero is its tag; a product has none. The Ersd door is the only mint and pads every construction, so a row value's arity is a fact of the row rather than of the site that built it — which is what lets the emitter key one final heap type per row and read it with an exact cast instead of the structural roster cascade.
    Row(RowId, Vec<Atom>),
}

#[derive(Debug, Clone)]
pub enum Callee {
    Known(FunctionId),
    Closure(ValueId),
}

#[derive(Debug, Clone)]
pub struct Edge {
    pub target: ContinuationId,
    pub args: Vec<Atom>,
}

/// Write-once cell storage, shared by guest coordination and compiler-generated knots.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CellOp {
    /// Allocate an empty slot.
    Reserve,
    /// Fill an empty cell, returning whether the value was accepted.
    Fill,
    /// Return presence (zero or one) and the stored payload; the absent payload is a filler.
    Poll,
}

impl CellOp {
    pub fn operand_arity(self) -> usize {
        match self {
            Self::Reserve => 0,
            Self::Poll => 1,
            Self::Fill => 2,
        }
    }

    pub fn result_arity(self) -> usize {
        match self {
            Self::Reserve | Self::Fill => 1,
            Self::Poll => 2,
        }
    }
}

/// Bounded guest queue operations. Outcome codes belong to this lowering protocol, never to the host ABI or a nominal constructor's layout.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChannelOp {
    New,
    /// Return zero for accepted, one for full, or two for closed.
    Push,
    /// Return zero and an item, one and a filler for empty, or two and a filler for ended.
    Take,
    Close,
    Closed,
    Count,
    Capacity,
}

impl ChannelOp {
    pub fn operand_arity(self) -> usize {
        match self {
            Self::Push => 2,
            Self::New | Self::Take | Self::Close | Self::Closed | Self::Count | Self::Capacity => 1,
        }
    }

    pub fn result_arity(self) -> usize {
        match self {
            Self::Take => 2,
            Self::Close => 0,
            Self::New | Self::Push | Self::Closed | Self::Count | Self::Capacity => 1,
        }
    }
}

/// A call-like intrinsic. `ListMap` takes the list then the mapper — the carrier-first order of the whole sequence row, matched by the erased representation so the lowering transcribes without reordering — and runs the mapper once per element, in order.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntrinsicCall {
    ListMap,
}

#[derive(Debug, Clone)]
pub enum Node {
    LetValue {
        result: ValueId,
        value: ValueExpr,
        next: NodeId,
    },
    LetIntrinsic {
        result: ValueId,
        op: Intrinsic,
        args: Vec<Atom>,
        next: NodeId,
    },
    LetFun {
        functions: Vec<FunctionId>,
        body: NodeId,
    },
    LetCont {
        continuations: Vec<ContinuationId>,
        body: NodeId,
    },
    ApplyFun {
        callee: Callee,
        args: Vec<Atom>,
        return_to: ContinuationId,
    },
    ApplyCont(Edge),
    Switch {
        scrutinee: Atom,
        cases: BTreeMap<u32, Edge>,
        default: Option<Edge>,
    },
    Foreign {
        function: Arc<ForeignFunction>,
        args: Vec<Atom>,
        return_to: ContinuationId,
    },
    Cell {
        op: CellOp,
        args: Vec<Atom>,
        return_to: ContinuationId,
    },
    Channel {
        op: ChannelOp,
        args: Vec<Atom>,
        return_to: ContinuationId,
    },
    Intrinsic {
        op: IntrinsicCall,
        args: Vec<Atom>,
        return_to: ContinuationId,
    },
    /// A call to a host row that diverges — `proc/exit` — with its wire operands. Terminal like [`Node::Panic`]: it has no continuation, so no pass can give one to it, and the emitter refuses as [`Panic::HostReply`] should a host return anyway.
    Halt {
        function: Arc<ForeignFunction>,
        args: Vec<Atom>,
    },
    /// A deliberate runtime failure of the given class: the block ends by reporting it and never continues. A lowering seats one where the program can reach a state it has to refuse — the one such state is the knot's forcing state, a member read while its own initializer runs — and the emitter renders every class as its sentence through the `sys.panic` import. Distinct from [`Node::Unreachable`], which marks an arm the theory proved impossible: reaching a `Panic` is the program's doing, reaching an `Unreachable` is the compiler's.
    Panic(Panic),
    /// An arm the theory proved impossible. Never reached by a sound compilation; the emitter renders it as [`Panic::Invariant`]'s sentence so that a compiler bug says so.
    Unreachable,
}

/// The classes of failure a compiled program can stop with, each rendered by the emitter as one sentence naming the rule, the carrier and the remedy. A `Node::Panic` carries one; the emitter's own checks — a narrowing to the host wire, a read past the end, a `Flt` decode, a host's reply — reach for the same classes as instruction sequences, since they are decided while lowering an intrinsic rather than as nodes. The sentences themselves are the emitter's (`curios-emit`'s `into_wasm/refusal.rs`), so what the IR states is the vocabulary and what the emitter states is the text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Panic {
    /// A `Nat` argument to a host function the wire's unsigned `i64` cannot carry. The one place a `Nat` is narrowed by refusing: everywhere inside the program it is unbounded.
    NatWire,
    /// An `Int` argument to a host function the wire's `i64` cannot carry.
    IntWire,
    /// A packed or list read, or a window, past the end of its value.
    OutOfBounds,
    /// A `Flt` decoded from a byte string that is not eight bytes long.
    FltDecode,
    /// A recursive value read while its own initializer is still running — a cycle the eager verifier could not see through a closure, met by forcing.
    Cycle,
    /// A host answered a call with a value outside that call's contract — a `Byte` past 255 among them. Decided by the emitter where it lowers the call, so the host that answered is at fault and never the program.
    HostReply,
    /// An arm the theory proved impossible was taken: a compiler bug, never the program's.
    Invariant,
}

impl Panic {
    /// Every class, in declaration order: the order the emitter writes the refusal helpers a module reaches.
    pub const ALL: [Panic; 7] = [
        Panic::NatWire,
        Panic::IntWire,
        Panic::OutOfBounds,
        Panic::FltDecode,
        Panic::Cycle,
        Panic::HostReply,
        Panic::Invariant,
    ];
}

impl fmt::Display for Panic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Panic::NatWire => "nat_wire",
            Panic::IntWire => "int_wire",
            Panic::OutOfBounds => "bounds",
            Panic::FltDecode => "flt",
            Panic::Cycle => "cycle",
            Panic::HostReply => "host_reply",
            Panic::Invariant => "invariant",
        })
    }
}

#[derive(Debug, Clone)]
pub struct ValueDef {
    pub debug_name: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub debug_name: Option<String>,
    pub params: Vec<ValueId>,
    pub return_cont: ContinuationId,
    pub body: NodeId,
    /// Whether a call of this function whose result nothing reads may simply not happen: it terminates, and running it a second time — or not at all — is not an event a program can observe.
    ///
    /// **A conclusion, not a fact this stage could reach.** Termination is the size-change engine's, decided above Core and carried down; freedom from effects is the erased stage's interprocedural summary. Both are in hand at the lowering that builds this, and only their conjunction crosses, so nothing here needs a lattice or a notion of purity of its own. A trap or a divergence is *not* excluded by it — an occurrence that already ran is what licenses dropping a later one, and a dead call has no dominating occurrence, so this must mean total.
    ///
    /// False is the safe reading and the default: a function this could not be filled from is kept.
    pub droppable: bool,
}

#[derive(Debug, Clone)]
pub struct Continuation {
    pub debug_name: Option<String>,
    pub params: Vec<ValueId>,
    pub body: NodeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UseTarget {
    Value(ValueId),
    Fun(FunctionId),
    Cont(ContinuationId),
}

pub fn atoms(node: &Node) -> Vec<&Atom> {
    let mut output = Vec::new();
    match node {
        Node::LetValue { value, .. } => match value {
            ValueExpr::Literal(_) => {}
            ValueExpr::List(values) | ValueExpr::Tuple(values) | ValueExpr::Row(_, values) => {
                output.extend(values)
            }
        },
        Node::LetIntrinsic { args, .. }
        | Node::ApplyFun { args, .. }
        | Node::Foreign { args, .. }
        | Node::Halt { args, .. }
        | Node::Cell { args, .. }
        | Node::Channel { args, .. }
        | Node::Intrinsic { args, .. } => output.extend(args),
        Node::ApplyCont(edge) => output.extend(&edge.args),
        Node::Switch {
            scrutinee,
            cases,
            default,
        } => {
            output.push(scrutinee);
            for edge in cases.values() {
                output.extend(&edge.args);
            }
            if let Some(edge) = default {
                output.extend(&edge.args);
            }
        }
        Node::LetFun { .. } | Node::LetCont { .. } | Node::Panic(_) | Node::Unreachable => {}
    }
    output
}

pub(crate) fn visit_atoms_mut(node: &mut Node, visitor: &mut impl FnMut(&mut Atom)) {
    match node {
        Node::LetValue { value, .. } => match value {
            ValueExpr::Literal(_) => {}
            ValueExpr::List(values) | ValueExpr::Tuple(values) | ValueExpr::Row(_, values) => {
                values.iter_mut().for_each(visitor)
            }
        },
        Node::LetIntrinsic { args, .. }
        | Node::ApplyFun { args, .. }
        | Node::Foreign { args, .. }
        | Node::Halt { args, .. }
        | Node::Cell { args, .. }
        | Node::Channel { args, .. }
        | Node::Intrinsic { args, .. } => args.iter_mut().for_each(visitor),
        Node::ApplyCont(edge) => edge.args.iter_mut().for_each(visitor),
        Node::Switch {
            scrutinee,
            cases,
            default,
        } => {
            visitor(scrutinee);
            for edge in cases.values_mut() {
                edge.args.iter_mut().for_each(&mut *visitor);
            }
            if let Some(edge) = default {
                edge.args.iter_mut().for_each(visitor);
            }
        }
        Node::LetFun { .. } | Node::LetCont { .. } | Node::Panic(_) | Node::Unreachable => {}
    }
}
