//! What a value may be: the functions it may be a closure of and the constructions it may be, a row's or a tuple's, followed forward through every flow. It is a 0CFA ([Shivers](https://www.ccs.neu.edu/home/shivers/papers/diss.pdf)): a closure call hands its arguments to every function its callee may be and takes their returns, so the call graph a closure call reaches is the analysis's own answer, and a known call, a jump and a return hand on what they carry as everywhere else.
//!
//! **A closure is abstracted by its function, an aggregate by its construction.** A row's or a tuple's slot is keyed by the construction that writes it, and a read takes the slot of every construction the value read may be, so the `Io` driver's surfaced suspension stays apart from the ones inside it, which share every slot of the suspension row. A key per row slot joins every instantiation of a family and every constructor of a carrier. Beside each construction's slot, its row's or its tuple arity's summary takes every store, which is what a read out of a value that may be any reads.
//!
//! **A container is one key.** A list's elements, a cell's contents and a channel's contents are a key each, what is stored into any being what any hands out: a list operation's result holds its operand's elements and a host's list reply is built by no node, so a key per construction would have to follow every list operation for what one key already answers.
//!
//! **Any is what the module does not see.** That is the entry's parameters, and what a heap call answers, which only a module [`bound_frames`](crate::bound_frames) rewrote holds. A host's reply is its wire values, no closure and no construction, and the advance enters only the descriptions the entry surfaced, which take no parameter, so every other parameter takes what the module's own calls hand it. A callee that may be any reaches every function some atom names that takes as many parameters, and a read out of a value that may be any reads the summary, since every aggregate is one the module builds.
//!
//! **The rows a value may be are its constructions' rows**, every row where it may be any. They are what [`ValueFlow::rows`] answers the row tests, the rebuild in place and the verifier, and what [`Module::entry_suspension`] reads off the entry's returns for its ABI ([An entry exports only the turns it can return](../../documentation/design/compilation/an-entry-exports-only-the-turns-it-can-return.md)): each function-return position has its own fact, even where no caller receives it, so only what reaches the entry's returns decides its capability, and a suspension consumed inside the program does not escape merely because it was built.
//!
//! **A solve serves every reader of the module as it stands.** The module files its solve with the generation it was read at ([`Module::value_flow`]), so where several read one module, as the verifier of the rebuilds, the entry's capability and the frame bound do of the module the rebuild in place leaves, they read one solve.
//!
//! **A flow is a copy, and what turns on a value watches it.** A store, a jump, a known call and its return, and a container's contents are copies from one key to another ([`Solver::copy`]); a closure call, a list map, a read and a rebuild are watches of the value they turn on ([`Rules::watch`]), handed each function or construction it gains, each stating the copies that one asks. A solve so carries each function and construction across each flow once, where a rule reading whole facts would copy every set it read each time it ran again.
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
        Lattice, Module, Node, NodeId, RowId, Rules, Solver, ValueExpr, ValueId,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// What something may be: these, or any.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MayBe<T> {
    Of(Ids<T>),
    Any,
}

/// A set of identities, ascending in a vector: what a fact holds of the functions or the constructions a value may be. A solve hands gains on by the hundred thousand, each a few identities merged into a set that keeps its allocation, where a tree pays a node per handful of identities for every one it holds.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Ids<T>(Vec<T>);

impl<T: Ord + Copy> Ids<T> {
    /// No identity.
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// `item` alone.
    pub fn one(item: T) -> Self {
        Self(vec![item])
    }

    /// Whether `item` is among these.
    pub fn contains(&self, item: &T) -> bool {
        self.0.binary_search(item).is_ok()
    }

    /// These, ascending.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.0.iter()
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Gain every identity of `other` this lacks, answering those, ascending.
    fn gain(&mut self, other: &Self) -> Self {
        // A gain handed on is a few identities against a set of many, each sought where it would stand; a whole set handed on is walked beside this one.
        let gained = match other.0.len() * 16 < self.0.len() {
            true => other
                .0
                .iter()
                .copied()
                .filter(|item| !self.contains(item))
                .collect::<Vec<_>>(),
            false => {
                let mut gained = Vec::new();
                let mut held = self.0.iter().peekable();
                for &item in &other.0 {
                    while held.next_if(|&&below| below < item).is_some() {}
                    if held.peek() != Some(&&item) {
                        gained.push(item);
                    }
                }
                gained
            }
        };
        self.merge(&gained);

        Self(gained)
    }

    /// Merge `gained`, ascending and disjoint from these, in place from the back, so only what stands above its least moves.
    fn merge(&mut self, gained: &[T]) {
        let Some(&least) = gained.first() else {
            return;
        };
        let (mut held, mut taken) = (self.0.len(), gained.len());
        self.0.resize(held + taken, least);
        for at in (0..self.0.len()).rev() {
            if taken == 0 {
                break;
            }
            if held > 0 && self.0[held - 1] > gained[taken - 1] {
                held -= 1;
                self.0[at] = self.0[held];
            } else {
                taken -= 1;
                self.0[at] = gained[taken];
            }
        }
    }
}

impl<T: Ord + Copy> FromIterator<T> for Ids<T> {
    fn from_iter<I: IntoIterator<Item = T>>(items: I) -> Self {
        let mut items = items.into_iter().collect::<Vec<_>>();
        items.sort_unstable();
        items.dedup();

        Self(items)
    }
}

impl<T> IntoIterator for Ids<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

/// The rows a value may be a struct of: these, or any.
pub type Rows = MayBe<RowId>;

impl<T: Ord + Copy> MayBe<T> {
    /// Whether `item` is among what it may be.
    pub fn may_be(&self, item: T) -> bool {
        match self {
            MayBe::Of(items) => items.contains(&item),
            MayBe::Any => true,
        }
    }

    /// Whether it may be nothing at all: what no flow reaches, which no run holds.
    pub fn is_nothing(&self) -> bool {
        matches!(self, MayBe::Of(items) if items.is_empty())
    }

    fn nothing() -> Self {
        MayBe::Of(Ids::new())
    }

    fn one(item: T) -> Self {
        MayBe::Of(Ids::one(item))
    }
}

impl<T: Ord + Copy> Lattice for MayBe<T> {
    fn bottom() -> Self {
        MayBe::nothing()
    }

    fn join(&mut self, incoming: Self) {
        self.gain(&incoming);
    }

    // What a set gained is its new members, or any.
    fn gain(&mut self, incoming: &Self) -> Option<Self> {
        match (&mut *self, incoming) {
            (MayBe::Any, _) => None,
            (_, MayBe::Any) => {
                *self = MayBe::Any;

                Some(MayBe::Any)
            }
            (MayBe::Of(held), MayBe::Of(incoming)) => {
                let gained = held.gain(incoming);
                (!gained.is_empty()).then_some(MayBe::Of(gained))
            }
        }
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

    fn gain(&mut self, incoming: &Self) -> Option<Self> {
        let functions = self.functions.gain(&incoming.functions);
        let constructions = self.constructions.gain(&incoming.constructions);
        if functions.is_none() && constructions.is_none() {
            return None;
        }

        Some(ValueFact {
            functions: functions.unwrap_or_else(MayBe::nothing),
            constructions: constructions.unwrap_or_else(MayBe::nothing),
        })
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
#[derive(PartialEq)]
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
    fn rows_of(&self, fact: &ValueFact) -> Rows {
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
    module.value_flow().rows()
}

impl ValueFlow {
    /// The rows each value may be a struct of.
    pub fn rows(&self) -> BTreeMap<ValueId, Rows> {
        self.facts
            .iter()
            .filter_map(|(key, fact)| match key {
                FlowKey::Value(value) => Some((*value, self.rows_of(fact))),
                _ => None,
            })
            .collect()
    }
}

impl Module {
    /// The registered suspension row when an entry result may carry it; `None` when return flow excludes it, or this module has no entry or suspension row.
    ///
    /// This reads the current graph without caching or changing its row vocabulary. A result the module does not see retains the row; a computation that never returns contributes no returned row and need not be proved terminating.
    pub fn entry_suspension(&self) -> Option<RowId> {
        let entry = self.entry()?;
        let row = self.suspension()?;
        let flow = self.value_flow();
        let mut returned = flow
            .facts
            .iter()
            .filter_map(|(key, fact)| match key {
                FlowKey::Returned(function, _) if *function == entry => Some(flow.rows_of(fact)),
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

/// Where a flow starts: what a key holds, or a fact no key holds.
enum Source {
    Key(FlowKey),
    Fact(ValueFact),
}

/// A watch of what `source`'s key holds or, where no key holds it, a rule handed its fact once.
fn watch<'r>(
    rules: &mut Rules<'r, ValueFact, FlowKey>,
    source: Source,
    rule: impl Fn(&mut Solver<ValueFact, FlowKey>, &ValueFact) + 'r,
) {
    match source {
        Source::Key(key) => rules.watch(key, rule),
        Source::Fact(fact) => rules.add(move |solver| rule(solver, &fact)),
    }
}

/// The facts of every value, return position, slot and container, solved together, so a closure call reads the same least fixpoint the calls it reaches settle.
///
/// A flow is a copy, stated by a rule that reads nothing and so runs once. What a closure call, a list map, a read and a rebuild do turns on what a value may be, so each is a watch of that value, handed each function or construction it gains, which states the copies that one asks.
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
    let seen = seeds
        .iter()
        .filter_map(|key| match key {
            FlowKey::Value(value) => Some(*value),
            _ => None,
        })
        .collect::<BTreeSet<_>>();

    let source = |atom: &Atom| match atom {
        Atom::Value(value) if seen.contains(value) => Source::Key(FlowKey::Value(*value)),
        // A value nothing seeded is one the module does not see.
        Atom::Value(_) => Source::Fact(ValueFact::any()),
        Atom::Fun(function) => Source::Fact(ValueFact::function(*function)),
        Atom::Literal(_) | Atom::Filler => Source::Fact(ValueFact::nothing()),
    };
    let flow = |solver: &mut Solver<ValueFact, FlowKey>, source: Source, to: FlowKey| match source {
        Source::Key(from) => solver.copy(from, to),
        Source::Fact(fact) => solver.join(to, fact),
    };
    // What `target` binds, each position from its source.
    let deliver = |solver: &mut Solver<ValueFact, FlowKey>,
                   target: ContinuationId,
                   sources: &dyn Fn(usize) -> Source| {
        if let Some(keys) = receivers.get(&target) {
            for (index, key) in keys.iter().enumerate() {
                flow(solver, sources(index), *key);
            }
        }
    };
    // Slot `index` of an object of `gained`: under each construction among `kind` it may be, or the summary where it may be any.
    let read = |solver: &mut Solver<ValueFact, FlowKey>,
                gained: &ValueFact,
                kind: &BTreeSet<NodeId>,
                summary: FlowKey,
                index: usize,
                result: FlowKey| match &gained.constructions {
        MayBe::Of(objects) => {
            for object in objects.iter().filter(|object| kind.contains(object)) {
                solver.copy(FlowKey::At(*object, index), result);
            }
        }
        MayBe::Any => solver.copy(summary, result),
    };
    // The functions a callee may be that take `arity` parameters, every escaping one of them where it may be any.
    let callees = |functions: &MayBe<FunctionId>, arity: usize| -> Vec<FunctionId> {
        let takes = |function: &FunctionId| {
            module
                .function(*function)
                .is_some_and(|definition| definition.params.len() == arity)
        };
        match functions {
            MayBe::Of(functions) => functions.iter().copied().filter(takes).collect(),
            MayBe::Any => escaping.iter().copied().filter(takes).collect(),
        }
    };
    // A call of `callee` with `args`, resuming `return_to`: the arguments to its parameters, its returns to the resume, any past them.
    let call = |solver: &mut Solver<ValueFact, FlowKey>,
                callee: FunctionId,
                args: &[Atom],
                return_to: ContinuationId| {
        let Some(function) = module.function(callee) else {
            return;
        };
        for (atom, param) in args.iter().zip(&function.params) {
            flow(solver, source(atom), FlowKey::Value(*param));
        }
        let arity = arities[&callee];
        deliver(solver, return_to, &|index| match index < arity {
            true => Source::Key(FlowKey::Returned(callee, index)),
            false => Source::Fact(ValueFact::any()),
        });
    };
    let jump = |solver: &mut Solver<ValueFact, FlowKey>, target: ContinuationId, args: &[Atom]| {
        deliver(solver, target, &|index| {
            args.get(index)
                .map_or(Source::Fact(ValueFact::any()), source)
        });
    };
    let (source, flow, deliver, read, callees, call, jump) =
        (&source, &flow, &deliver, &read, &callees, &call, &jump);

    Solver::solve(seeds, |rules| {
        if let Some(entry) = module.entry().and_then(|entry| module.function(entry)) {
            rules.add(move |solver| {
                for param in &entry.params {
                    solver.join(FlowKey::Value(*param), ValueFact::any());
                }
            });
        }
        for (id, node) in module.live_nodes() {
            match node {
                Node::LetValue { result, value, .. } => match value {
                    ValueExpr::Row(row, atoms) => rules.add(move |solver| {
                        for (index, atom) in atoms.iter().enumerate() {
                            flow(solver, source(atom), FlowKey::Slot(*row, index));
                            flow(solver, source(atom), FlowKey::At(id, index));
                        }
                        solver.join(FlowKey::Value(*result), ValueFact::construction(id));
                    }),
                    ValueExpr::Tuple(atoms) => rules.add(move |solver| {
                        for (index, atom) in atoms.iter().enumerate() {
                            flow(solver, source(atom), FlowKey::Field(index));
                            flow(solver, source(atom), FlowKey::At(id, index));
                        }
                        solver.join(FlowKey::Value(*result), ValueFact::construction(id));
                    }),
                    // A rebuild writes into the object `of` names and is that object: a construction of its row `of` may be, every one of them where `of` may be any.
                    ValueExpr::Rebuilt { of, row, slots } => {
                        rules.add(move |solver| {
                            for (index, atom) in slots {
                                flow(solver, source(atom), FlowKey::Slot(*row, *index));
                            }
                            solver.join(FlowKey::Value(*result), ValueFact::nothing());
                        });
                        watch(rules, source(of), move |solver, gained| {
                            let of_row = constructions.of_row(*row);
                            let objects = match &gained.constructions {
                                MayBe::Of(objects) => objects
                                    .iter()
                                    .copied()
                                    .filter(|object| of_row.contains(object))
                                    .collect::<Ids<_>>(),
                                MayBe::Any => of_row.iter().copied().collect(),
                            };
                            for &object in objects.iter() {
                                for (index, atom) in slots {
                                    flow(solver, source(atom), FlowKey::At(object, *index));
                                }
                            }
                            solver.join(
                                FlowKey::Value(*result),
                                ValueFact {
                                    functions: MayBe::nothing(),
                                    constructions: MayBe::Of(objects),
                                },
                            );
                        });
                    }
                    ValueExpr::List(atoms) => rules.add(move |solver| {
                        for atom in atoms {
                            flow(solver, source(atom), FlowKey::Element);
                        }
                    }),
                    ValueExpr::Literal(_) => {}
                },
                Node::LetIntrinsic {
                    result, op, args, ..
                } => {
                    let result = FlowKey::Value(*result);
                    match op {
                        Intrinsic::RowGet(row, index) => {
                            watch(rules, source(&args[0]), move |solver, gained| {
                                let kind = constructions.of_row(*row);
                                read(
                                    solver,
                                    gained,
                                    kind,
                                    FlowKey::Slot(*row, *index),
                                    *index,
                                    result,
                                );
                            });
                        }
                        Intrinsic::TupleGet(index) => {
                            watch(rules, source(&args[0]), move |solver, gained| {
                                let kind = &constructions.tuples;
                                read(solver, gained, kind, FlowKey::Field(*index), *index, result);
                            });
                        }
                        Intrinsic::ListGet => {
                            rules.add(move |solver| solver.copy(FlowKey::Element, result));
                        }
                        Intrinsic::ImmediateGet => {
                            rules.add(move |solver| flow(solver, source(&args[0]), result));
                        }
                        Intrinsic::ListAppend => {
                            rules.add(move |solver| {
                                flow(solver, source(&args[1]), FlowKey::Element)
                            });
                        }
                        Intrinsic::ListSet | Intrinsic::ListOverwrite => {
                            rules.add(move |solver| {
                                flow(solver, source(&args[2]), FlowKey::Element)
                            });
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
                        | Intrinsic::IsRow(_) => {}
                    }
                }
                Node::ApplyFun {
                    callee,
                    args,
                    return_to,
                } => match callee {
                    Callee::Known(callee) => {
                        rules.add(move |solver| call(solver, *callee, args, *return_to));
                    }
                    Callee::Closure(closure) => {
                        watch(
                            rules,
                            source(&Atom::Value(*closure)),
                            move |solver, gained| {
                                for callee in callees(&gained.functions, args.len()) {
                                    call(solver, callee, args, *return_to);
                                }
                            },
                        );
                    }
                },
                Node::ApplyCont(edge) => {
                    rules.add(move |solver| jump(solver, edge.target, &edge.args))
                }
                Node::Switch { cases, default, .. } => rules.add(move |solver| {
                    for edge in cases.values().chain(default.as_ref()) {
                        jump(solver, edge.target, &edge.args);
                    }
                }),
                Node::Cell {
                    op,
                    args,
                    return_to,
                } => match op {
                    CellOp::Fill => {
                        rules.add(move |solver| flow(solver, source(&args[1]), FlowKey::Cell))
                    }
                    CellOp::Poll => rules.add(move |solver| {
                        deliver(solver, *return_to, &|index| match index {
                            1 => Source::Key(FlowKey::Cell),
                            _ => Source::Fact(ValueFact::nothing()),
                        });
                    }),
                    CellOp::Reserve => {}
                },
                Node::Channel {
                    op,
                    args,
                    return_to,
                } => match op {
                    ChannelOp::Push => {
                        rules.add(move |solver| flow(solver, source(&args[1]), FlowKey::Channel))
                    }
                    ChannelOp::Take => rules.add(move |solver| {
                        deliver(solver, *return_to, &|index| match index {
                            1 => Source::Key(FlowKey::Channel),
                            _ => Source::Fact(ValueFact::nothing()),
                        });
                    }),
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
                        watch(rules, source(&args[1]), move |solver, gained| {
                            for mapper in callees(&gained.functions, 1) {
                                let Some(function) = module.function(mapper) else {
                                    continue;
                                };
                                solver.copy(FlowKey::Element, FlowKey::Value(function.params[0]));
                                solver.copy(FlowKey::Returned(mapper, 0), FlowKey::Element);
                            }
                        });
                    }
                    IntrinsicCall::HeapApply => rules.add(move |solver| {
                        deliver(solver, *return_to, &|_| Source::Fact(ValueFact::any()));
                    }),
                    IntrinsicCall::FrameEnter { .. } | IntrinsicCall::FrameLeave { .. } => {}
                },
                // A host's reply is its wire values.
                Node::Foreign { .. }
                | Node::LetFun { .. }
                | Node::LetCont { .. }
                | Node::Halt { .. }
                | Node::Panic(_)
                | Node::Unreachable => {}
            }
        }
    })
}
