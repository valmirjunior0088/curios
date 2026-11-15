//! What a value may be: the functions it may be a closure of and the constructions it may be, a row's or a tuple's, followed forward through every flow. It is a 0CFA ([Shivers](https://www.ccs.neu.edu/home/shivers/papers/diss.pdf)): a closure call hands its arguments to every function its callee may be and takes their returns, so the call graph a closure call reaches is the analysis's own answer, and a known call, a jump and a return hand on what they carry as everywhere else.
//!
//! **A closure is abstracted by its function, an aggregate by its construction.** A row's or a tuple's slot is keyed by the construction that writes it, and a read takes the slot of every construction the value read may be, so the `Io` driver's surfaced suspension stays apart from the ones inside it, which share every slot of the suspension row. A key per row slot joins every instantiation of a family and every constructor of a carrier. Beside each construction's slot, its row's or its tuple arity's summary takes every store, which is what a read out of a value that may be any reads.
//!
//! **A container is one key.** A list's elements, a cell's contents and a channel's contents are a key each, what is stored into any being what any hands out: a list operation's result holds its operand's elements and a host's list reply is built by no node, so a key per construction would have to follow every list operation for what one key already answers.
//!
//! **Any is what the module does not see.** That is the entry's parameters, and what a heap call answers, which only a module [`bound_frames`](crate::bound_frames) rewrote holds. A host's reply is its wire values, no closure and no construction, and the advance enters only the descriptions the entry surfaced, which take no parameter, so every other parameter takes what the module's own calls hand it. A callee that may be any reaches every function some atom names that takes as many parameters, and a read out of a value that may be any reads the summary, since every aggregate is one the module builds.
//!
//! **The rows a value may be are its constructions' rows**, every row where it may be any. They are what [`rows_reaching`] answers the row tests, the rebuild in place and the verifier, and what [`Module::entry_suspension`] reads off the entry's returns for its ABI ([An entry exports only the turns it can return](../../documentation/design/compilation/an-entry-exports-only-the-turns-it-can-return.md)): each function-return position has its own fact, even where no caller receives it, so only what reaches the entry's returns decides its capability, and a suspension consumed inside the program does not escape merely because it was built.
//!
//! **A flow it misses is a wrong answer** wherever a reader trusts what a value may not be, so every rule matches its node's whole alphabet: no wildcard stands over a node, a value expression or an operation.

#[cfg(test)]
mod closure_tests;

#[cfg(test)]
mod entry_tests;

#[cfg(test)]
mod row_tests;

use {
    super::{
        Atom, Callee, CellOp, ChannelOp, ContinuationId, FunctionId, Intrinsic, IntrinsicCall,
        Lattice, Module, Node, NodeId, RowId, Solver, ValueExpr, ValueId,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// What something may be: these, or any.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MayBe<T> {
    Of(BTreeSet<T>),
    Any,
}

/// The rows a value may be a struct of: these, or any.
pub type Rows = MayBe<RowId>;

impl<T: Ord> MayBe<T> {
    /// Whether `item` is among what it may be.
    pub fn may_be(&self, item: T) -> bool {
        match self {
            MayBe::Of(items) => items.contains(&item),
            MayBe::Any => true,
        }
    }

    fn nothing() -> Self {
        MayBe::Of(BTreeSet::new())
    }

    fn one(item: T) -> Self {
        MayBe::Of(BTreeSet::from([item]))
    }
}

impl<T: Ord + Clone> Lattice for MayBe<T> {
    fn bottom() -> Self {
        MayBe::nothing()
    }

    fn join(&mut self, incoming: Self) {
        *self = match (std::mem::replace(self, MayBe::Any), incoming) {
            (MayBe::Any, _) | (_, MayBe::Any) => MayBe::Any,
            (MayBe::Of(mut left), MayBe::Of(right)) => {
                left.extend(right);
                MayBe::Of(left)
            }
        };
    }
}

/// What one value may be: a closure of these functions, an aggregate built at these constructions.
#[derive(Debug, Clone, PartialEq, Eq)]
struct ValueFact {
    functions: MayBe<FunctionId>,
    constructions: MayBe<NodeId>,
}

impl ValueFact {
    fn nothing() -> Self {
        ValueFact {
            functions: MayBe::nothing(),
            constructions: MayBe::nothing(),
        }
    }

    fn any() -> Self {
        ValueFact {
            functions: MayBe::Any,
            constructions: MayBe::Any,
        }
    }

    fn function(function: FunctionId) -> Self {
        ValueFact {
            functions: MayBe::one(function),
            constructions: MayBe::nothing(),
        }
    }

    fn construction(node: NodeId) -> Self {
        ValueFact {
            functions: MayBe::nothing(),
            constructions: MayBe::one(node),
        }
    }
}

impl Lattice for ValueFact {
    fn bottom() -> Self {
        ValueFact::nothing()
    }

    fn join(&mut self, incoming: Self) {
        self.functions.join(incoming.functions);
        self.constructions.join(incoming.constructions);
    }
}

/// Where a fact lives: a value, a function's result at one position, a construction's slot or field, a row's or a tuple arity's summary, or a container's contents.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum FlowKey {
    Value(ValueId),
    Returned(FunctionId, usize),
    At(NodeId, usize),
    Slot(RowId, usize),
    Field(usize),
    Element,
    Cell,
    Channel,
}

/// What each value of a module may be, solved over the module as it stands.
pub struct ValueFlow {
    facts: BTreeMap<FlowKey, ValueFact>,
    /// The row each construction of a row builds.
    built: BTreeMap<NodeId, RowId>,
}

impl ValueFlow {
    pub fn of(module: &Module) -> Self {
        let constructions = Constructions::of(module);
        let facts = solve(module, &constructions);
        let built = constructions
            .rows
            .into_iter()
            .flat_map(|(row, nodes)| nodes.into_iter().map(move |node| (node, row)))
            .collect();
        Self { facts, built }
    }

    /// The rows of the constructions `fact` may be, every row where it may be any.
    fn rows(&self, fact: &ValueFact) -> Rows {
        match &fact.constructions {
            MayBe::Of(constructions) => MayBe::Of(
                constructions
                    .iter()
                    .filter_map(|construction| self.built.get(construction).copied())
                    .collect(),
            ),
            MayBe::Any => MayBe::Any,
        }
    }

    /// The functions `atom` may be a closure of: a function it names, or what reaches the value it names.
    pub fn functions(&self, atom: &Atom) -> MayBe<FunctionId> {
        match atom {
            Atom::Fun(function) => MayBe::one(*function),
            Atom::Value(value) => self
                .facts
                .get(&FlowKey::Value(*value))
                .map_or(MayBe::Any, |fact| fact.functions.clone()),
            Atom::Literal(_) | Atom::Filler => MayBe::nothing(),
        }
    }
}

/// The rows each value may be a struct of.
pub fn rows_reaching(module: &Module) -> BTreeMap<ValueId, Rows> {
    let flow = ValueFlow::of(module);
    flow.facts
        .iter()
        .filter_map(|(key, fact)| match key {
            FlowKey::Value(value) => Some((*value, flow.rows(fact))),
            _ => None,
        })
        .collect()
}

impl Module {
    /// The registered suspension row when an entry result may carry it; `None` when return flow excludes it, or this module has no entry or suspension row.
    ///
    /// This reads the current graph without caching or changing its row vocabulary. A result the module does not see retains the row; a computation that never returns contributes no returned row and need not be proved terminating.
    pub fn entry_suspension(&self) -> Option<RowId> {
        let entry = self.entry()?;
        let row = self.suspension()?;
        let flow = ValueFlow::of(self);
        let mut returned = flow
            .facts
            .iter()
            .filter_map(|(key, fact)| match key {
                FlowKey::Returned(function, _) if *function == entry => Some(flow.rows(fact)),
                _ => None,
            })
            .peekable();
        match returned.peek().is_none() || returned.any(|rows| rows.may_be(row)) {
            true => Some(row),
            false => None,
        }
    }
}

/// The constructions each read may take a slot of: a row's by its row, and every tuple's.
struct Constructions {
    rows: BTreeMap<RowId, BTreeSet<NodeId>>,
    tuples: BTreeSet<NodeId>,
}

impl Constructions {
    fn of(module: &Module) -> Self {
        let mut constructions = Constructions {
            rows: BTreeMap::new(),
            tuples: BTreeSet::new(),
        };
        for (id, node) in module.live_nodes() {
            if let Node::LetValue { value, .. } = node {
                match value {
                    ValueExpr::Row(row, _) => {
                        constructions.rows.entry(*row).or_default().insert(id);
                    }
                    ValueExpr::Tuple(_) => {
                        constructions.tuples.insert(id);
                    }
                    ValueExpr::Literal(_) | ValueExpr::List(_) | ValueExpr::Rebuilt { .. } => {}
                }
            }
        }
        constructions
    }

    fn of_row(&self, row: RowId) -> &BTreeSet<NodeId> {
        static NONE: BTreeSet<NodeId> = BTreeSet::new();
        self.rows.get(&row).unwrap_or(&NONE)
    }
}

/// The facts of every value, return position, slot and container, solved together, so a closure call reads the same least fixpoint the calls it reaches settle.
fn solve(module: &Module, constructions: &Constructions) -> BTreeMap<FlowKey, ValueFact> {
    let escaping = module.escaping_functions();
    let arities = module.return_arities();
    let receivers: BTreeMap<ContinuationId, Vec<FlowKey>> = module
        .live_continuations()
        .map(|(id, definition)| {
            (
                id,
                definition
                    .params
                    .iter()
                    .copied()
                    .map(FlowKey::Value)
                    .collect(),
            )
        })
        .chain(module.live_functions().map(|(id, definition)| {
            (
                definition.return_cont,
                (0..arities[&id])
                    .map(|index| FlowKey::Returned(id, index))
                    .collect(),
            )
        }))
        .collect();
    let seeds = receivers
        .values()
        .flatten()
        .copied()
        .chain(module.value_ids().map(FlowKey::Value))
        .collect::<Vec<_>>();

    let deliver = |solver: &mut Solver<ValueFact, FlowKey>,
                   target: ContinuationId,
                   facts: &dyn Fn(usize) -> ValueFact| {
        if let Some(keys) = receivers.get(&target) {
            for (index, key) in keys.iter().enumerate() {
                solver.join(*key, facts(index));
            }
        }
    };
    let fact = |solver: &Solver<ValueFact, FlowKey>, atom: &Atom| match atom {
        Atom::Value(value) => solver
            .fact(&FlowKey::Value(*value))
            .cloned()
            .unwrap_or_else(ValueFact::any),
        Atom::Fun(function) => ValueFact::function(*function),
        Atom::Literal(_) | Atom::Filler => ValueFact::nothing(),
    };
    // A key nothing was stored at holds nothing.
    let stored = |solver: &Solver<ValueFact, FlowKey>, key: FlowKey| {
        solver
            .fact(&key)
            .cloned()
            .unwrap_or_else(ValueFact::nothing)
    };
    // Slot `index` of `of`: under each construction among `kind` it may be, or the summary where it may be any.
    let read = |solver: &Solver<ValueFact, FlowKey>,
                of: &Atom,
                kind: &BTreeSet<NodeId>,
                summary: FlowKey,
                index: usize| match fact(solver, of).constructions {
        MayBe::Of(objects) => {
            let mut read = ValueFact::nothing();
            for object in objects.intersection(kind) {
                read.join(stored(solver, FlowKey::At(*object, index)));
            }
            read
        }
        MayBe::Any => stored(solver, summary),
    };
    // The functions a callee may be that take `arity` parameters, every escaping one of them where it may be any.
    let callees = |functions: MayBe<FunctionId>, arity: usize| -> Vec<FunctionId> {
        let takes = |function: &FunctionId| {
            module
                .function(*function)
                .is_some_and(|definition| definition.params.len() == arity)
        };
        match functions {
            MayBe::Of(functions) => functions.into_iter().filter(takes).collect(),
            MayBe::Any => escaping.iter().copied().filter(takes).collect(),
        }
    };
    // A call of `callee` with `args`, resuming `return_to`: the arguments to its parameters, its returns to the resume.
    let call = |solver: &mut Solver<ValueFact, FlowKey>,
                callee: FunctionId,
                args: &[Atom],
                return_to: ContinuationId| {
        let Some(function) = module.function(callee) else {
            return;
        };
        let passed = args
            .iter()
            .map(|atom| fact(solver, atom))
            .collect::<Vec<_>>();
        for (incoming, param) in passed.into_iter().zip(&function.params) {
            solver.join(FlowKey::Value(*param), incoming);
        }
        let returned = (0..arities[&callee])
            .map(|index| stored(solver, FlowKey::Returned(callee, index)))
            .collect::<Vec<_>>();
        deliver(solver, return_to, &|index| {
            returned.get(index).cloned().unwrap_or_else(ValueFact::any)
        });
    };
    let jump = |solver: &mut Solver<ValueFact, FlowKey>, target: ContinuationId, args: &[Atom]| {
        let passed = args
            .iter()
            .map(|atom| fact(solver, atom))
            .collect::<Vec<_>>();
        deliver(solver, target, &|index| {
            passed.get(index).cloned().unwrap_or_else(ValueFact::any)
        });
    };
    let (deliver, fact, stored, read, callees, call, jump) =
        (&deliver, &fact, &stored, &read, &callees, &call, &jump);

    Solver::solve(seeds, |rules| {
        if let Some(entry) = module.entry().and_then(|entry| module.function(entry)) {
            rules.add(move |solver| {
                for param in &entry.params {
                    solver.join(FlowKey::Value(*param), ValueFact::any());
                }
            });
        }
        for (id, node) in module.live_nodes() {
            rules.add(move |solver| match node {
                Node::LetValue { result, value, .. } => match value {
                    ValueExpr::Row(row, atoms) => {
                        for (index, atom) in atoms.iter().enumerate() {
                            let incoming = fact(solver, atom);
                            solver.join(FlowKey::Slot(*row, index), incoming.clone());
                            solver.join(FlowKey::At(id, index), incoming);
                        }
                        solver.join(FlowKey::Value(*result), ValueFact::construction(id));
                    }
                    ValueExpr::Tuple(atoms) => {
                        for (index, atom) in atoms.iter().enumerate() {
                            let incoming = fact(solver, atom);
                            solver.join(FlowKey::Field(index), incoming.clone());
                            solver.join(FlowKey::At(id, index), incoming);
                        }
                        solver.join(FlowKey::Value(*result), ValueFact::construction(id));
                    }
                    // A rebuild writes into the object `of` names and is that object: a construction of its row `of` may be, every one of them where `of` may be any.
                    ValueExpr::Rebuilt { of, row, slots } => {
                        let objects = match fact(solver, of).constructions {
                            MayBe::Of(objects) => objects
                                .intersection(constructions.of_row(*row))
                                .copied()
                                .collect::<BTreeSet<_>>(),
                            MayBe::Any => constructions.of_row(*row).clone(),
                        };
                        for (index, atom) in slots {
                            let incoming = fact(solver, atom);
                            solver.join(FlowKey::Slot(*row, *index), incoming.clone());
                            for &object in &objects {
                                solver.join(FlowKey::At(object, *index), incoming.clone());
                            }
                        }
                        solver.join(
                            FlowKey::Value(*result),
                            ValueFact {
                                functions: MayBe::nothing(),
                                constructions: MayBe::Of(objects),
                            },
                        );
                    }
                    ValueExpr::List(atoms) => {
                        for atom in atoms {
                            let incoming = fact(solver, atom);
                            solver.join(FlowKey::Element, incoming);
                        }
                    }
                    ValueExpr::Literal(_) => {}
                },
                Node::LetIntrinsic {
                    result, op, args, ..
                } => {
                    let incoming = match op {
                        Intrinsic::RowGet(row, index) => read(
                            solver,
                            &args[0],
                            constructions.of_row(*row),
                            FlowKey::Slot(*row, *index),
                            *index,
                        ),
                        Intrinsic::TupleGet(index) => read(
                            solver,
                            &args[0],
                            &constructions.tuples,
                            FlowKey::Field(*index),
                            *index,
                        ),
                        Intrinsic::ListGet => stored(solver, FlowKey::Element),
                        Intrinsic::ImmediateGet => fact(solver, &args[0]),
                        Intrinsic::ListAppend => {
                            let element = fact(solver, &args[1]);
                            solver.join(FlowKey::Element, element);
                            ValueFact::nothing()
                        }
                        // A number, a word, a float, a sequence or a list of what the list key holds already: nothing a closure call or a read follows.
                        Intrinsic::BoolAnd
                        | Intrinsic::BoolOr
                        | Intrinsic::NatEql
                        | Intrinsic::NatNeq
                        | Intrinsic::NatAdd
                        | Intrinsic::NatSub
                        | Intrinsic::NatMul
                        | Intrinsic::NatLt
                        | Intrinsic::NatDiv
                        | Intrinsic::NatRem
                        | Intrinsic::NatLe
                        | Intrinsic::NatAnd
                        | Intrinsic::NatOr
                        | Intrinsic::NatXor
                        | Intrinsic::NatMin
                        | Intrinsic::NatMax
                        | Intrinsic::NatShl
                        | Intrinsic::NatPow
                        | Intrinsic::NatShr
                        | Intrinsic::NatEqz
                        | Intrinsic::NatToInt
                        | Intrinsic::NatToFlt(_)
                        | Intrinsic::IntEql
                        | Intrinsic::IntNeq
                        | Intrinsic::IntAdd
                        | Intrinsic::IntSub
                        | Intrinsic::IntMul
                        | Intrinsic::IntDiv
                        | Intrinsic::IntRem
                        | Intrinsic::IntLt
                        | Intrinsic::IntLe
                        | Intrinsic::IntAnd
                        | Intrinsic::IntOr
                        | Intrinsic::IntXor
                        | Intrinsic::IntMin
                        | Intrinsic::IntMax
                        | Intrinsic::IntAbs
                        | Intrinsic::IntSign
                        | Intrinsic::IntShl
                        | Intrinsic::IntShr
                        | Intrinsic::IntEqz
                        | Intrinsic::IntToNat
                        | Intrinsic::IntToFlt(_)
                        | Intrinsic::FltAdd(_)
                        | Intrinsic::FltSub(_)
                        | Intrinsic::FltMul(_)
                        | Intrinsic::FltDiv(_)
                        | Intrinsic::FltFma(_)
                        | Intrinsic::FltRem
                        | Intrinsic::FltEql
                        | Intrinsic::FltNeq
                        | Intrinsic::FltLt
                        | Intrinsic::FltLe
                        | Intrinsic::FltMin
                        | Intrinsic::FltMax
                        | Intrinsic::FltNeg
                        | Intrinsic::FltAbs
                        | Intrinsic::FltSqrt(_)
                        | Intrinsic::FltRoundIntegral(_)
                        | Intrinsic::FltCopysign
                        | Intrinsic::FltToNat
                        | Intrinsic::FltToLeBytes
                        | Intrinsic::FltOfLeBytes
                        | Intrinsic::FltToInt
                        | Intrinsic::FltMantissa
                        | Intrinsic::FltExponent
                        | Intrinsic::BytesLen
                        | Intrinsic::BytesEql
                        | Intrinsic::BytesGet
                        | Intrinsic::BytesSlice
                        | Intrinsic::BytesRest
                        | Intrinsic::BytesAppend
                        | Intrinsic::BytesConcat(_)
                        | Intrinsic::BytesReplicate
                        | Intrinsic::BytesAnd
                        | Intrinsic::BytesOr
                        | Intrinsic::BytesXor
                        | Intrinsic::BytesChunk(_)
                        | Intrinsic::ListLen
                        | Intrinsic::ListSlice
                        | Intrinsic::ListRest
                        | Intrinsic::ListConcat(_)
                        | Intrinsic::ListSettle
                        | Intrinsic::ListFlat(_)
                        | Intrinsic::WindowExtent
                        | Intrinsic::IsImmediate
                        | Intrinsic::IsRow(_) => ValueFact::nothing(),
                    };
                    solver.join(FlowKey::Value(*result), incoming);
                }
                Node::ApplyFun {
                    callee,
                    args,
                    return_to,
                } => {
                    let reached = match callee {
                        Callee::Known(callee) => vec![*callee],
                        Callee::Closure(closure) => {
                            callees(fact(solver, &Atom::Value(*closure)).functions, args.len())
                        }
                    };
                    for callee in reached {
                        call(solver, callee, args, *return_to);
                    }
                }
                Node::ApplyCont(edge) => jump(solver, edge.target, &edge.args),
                Node::Switch { cases, default, .. } => {
                    for edge in cases.values().chain(default.as_ref()) {
                        jump(solver, edge.target, &edge.args);
                    }
                }
                Node::Cell {
                    op,
                    args,
                    return_to,
                } => match op {
                    CellOp::Fill => {
                        let incoming = fact(solver, &args[1]);
                        solver.join(FlowKey::Cell, incoming);
                    }
                    CellOp::Poll => {
                        let held = stored(solver, FlowKey::Cell);
                        deliver(solver, *return_to, &|index| match index {
                            1 => held.clone(),
                            _ => ValueFact::nothing(),
                        });
                    }
                    CellOp::Reserve => {}
                },
                Node::Channel {
                    op,
                    args,
                    return_to,
                } => match op {
                    ChannelOp::Push => {
                        let incoming = fact(solver, &args[1]);
                        solver.join(FlowKey::Channel, incoming);
                    }
                    ChannelOp::Take => {
                        let held = stored(solver, FlowKey::Channel);
                        deliver(solver, *return_to, &|index| match index {
                            1 => held.clone(),
                            _ => ValueFact::nothing(),
                        });
                    }
                    ChannelOp::New
                    | ChannelOp::Close
                    | ChannelOp::Closed
                    | ChannelOp::Count
                    | ChannelOp::Capacity => {}
                },
                Node::Intrinsic {
                    op,
                    args,
                    return_to,
                } => match op {
                    // Each element to every function the mapper may be, and what each answers to the mapped list's elements.
                    IntrinsicCall::ListMap => {
                        let mappers = callees(fact(solver, &args[1]).functions, 1);
                        for mapper in mappers {
                            let Some(function) = module.function(mapper) else {
                                continue;
                            };
                            let element = stored(solver, FlowKey::Element);
                            solver.join(FlowKey::Value(function.params[0]), element);
                            let image = stored(solver, FlowKey::Returned(mapper, 0));
                            solver.join(FlowKey::Element, image);
                        }
                    }
                    IntrinsicCall::HeapApply => deliver(solver, *return_to, &|_| ValueFact::any()),
                    IntrinsicCall::FrameEnter { .. } | IntrinsicCall::FrameLeave { .. } => {}
                },
                // A host's reply is its wire values.
                Node::Foreign { .. }
                | Node::LetFun { .. }
                | Node::LetCont { .. }
                | Node::Halt { .. }
                | Node::Panic(_)
                | Node::Unreachable => {}
            });
        }
    })
}
