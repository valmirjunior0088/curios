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
//! **A flow is a copy, and what turns on a value watches it.** A store, a jump, a known call and its return, and a container's contents are copies from one key to another ([`Solver::copy`]); a closure call, a list map, a read and a rebuild are watches of the value they turn on ([`Rules::watch`]), handed each function or construction it gains, each stating the copies that one asks. A solve so carries each function and construction across each flow once, where a rule reading whole facts would copy every set it read each time it ran again; and since a place is handed the same members by many flows, a fact holds them by numbers the solve gives them, a bit a number once it is wide ([`Members`]), so a member handed on is a bit tested and set rather than one sought among every member the fact holds.
//!
//! **A flow it misses is a wrong answer** wherever a reader trusts what a value may not be, so every rule matches its node's whole alphabet: no wildcard stands over a node, a value expression or an operation.

#[cfg(test)]
mod closure_tests;

#[cfg(test)]
mod entry_tests;

#[cfg(test)]
mod members_tests;

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

/// A set of identities, ascending in a vector: what the flow answers a value may be, and the members of a fact while they are few (`Members`). A solve hands gains on by the hundred thousand, each a few identities merged into a set that keeps its allocation, where a tree pays a node per handful of identities for every one it holds.
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

/// Past this many members a set holds a bit a number: a member handed on is then a bit tested and set, where a vector seeks it among every member it holds. In bits a set pays a word for every 64 numbers below its greatest, which a narrower set does not pay back.
const WIDE: usize = 64;

/// The members of one kind a fact holds, by the numbers its solve gives them ([`Numbering`]): ascending in a vector while at most [`WIDE`], a bit a number past it, every word to the last holding one. Which of the two a set is follows from its width alone, so two sets holding the same members are equal.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Members {
    Few(Ids<u32>),
    Many(Vec<u64>),
}

impl Members {
    fn nothing() -> Self {
        Members::Few(Ids::new())
    }

    /// The members `numbers` names, ascending.
    fn of_ascending(numbers: Vec<u32>) -> Self {
        match numbers.len() > WIDE {
            true => Members::Many(words_of(&numbers)),
            false => Members::Few(Ids(numbers)),
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            Members::Few(numbers) => numbers.is_empty(),
            Members::Many(_) => false,
        }
    }

    /// The members, ascending.
    fn iter(&self) -> impl Iterator<Item = u32> + '_ {
        let (few, many) = match self {
            Members::Few(numbers) => (Some(numbers.iter().copied()), None),
            Members::Many(words) => (None, Some(bits_of(words))),
        };
        few.into_iter().flatten().chain(many.into_iter().flatten())
    }

    /// Gain every member of `other` these lack, answering those.
    fn gain(&mut self, other: &Self) -> Self {
        if let Members::Few(held) = self {
            if let Members::Few(incoming) = other {
                let gained = held.gain(incoming);
                if held.0.len() > WIDE {
                    *self = Members::Many(words_of(&held.0));
                }
                return Members::Few(gained);
            }
            *self = Members::Many(words_of(&held.0));
        }
        let Members::Many(words) = self else {
            unreachable!("a set past its few holds words")
        };
        match other {
            Members::Few(incoming) => {
                let mut gained = Vec::new();
                for &number in incoming.iter() {
                    let (word, bit) = (number as usize / 64, 1u64 << (number % 64));
                    if word >= words.len() {
                        words.resize(word + 1, 0);
                    }
                    if words[word] & bit == 0 {
                        words[word] |= bit;
                        gained.push(number);
                    }
                }
                Members::Few(Ids(gained))
            }
            Members::Many(incoming) => {
                if incoming.len() > words.len() {
                    words.resize(incoming.len(), 0);
                }
                let mut gained = Vec::with_capacity(incoming.len());
                let mut count = 0;
                for (held, &word) in words.iter_mut().zip(incoming) {
                    let new = word & !*held;
                    *held |= new;
                    count += new.count_ones() as usize;
                    gained.push(new);
                }
                while gained.last() == Some(&0) {
                    gained.pop();
                }
                match count > WIDE {
                    true => Members::Many(gained),
                    false => Members::Few(Ids(bits_of(&gained).collect())),
                }
            }
        }
    }
}

/// The words holding a bit for each of `numbers`, ascending.
fn words_of(numbers: &[u32]) -> Vec<u64> {
    let mut words = vec![0; numbers.last().map_or(0, |last| *last as usize / 64 + 1)];
    for &number in numbers {
        words[number as usize / 64] |= 1 << (number % 64);
    }
    words
}

/// The numbers whose bits `words` holds, ascending.
fn bits_of(words: &[u64]) -> impl Iterator<Item = u32> + '_ {
    words.iter().enumerate().flat_map(|(index, &word)| {
        let mut rest = word;
        std::iter::from_fn(move || {
            (rest != 0).then(|| {
                let bit = rest.trailing_zeros();
                rest &= rest - 1;
                index as u32 * 64 + bit
            })
        })
    })
}

/// What a fact holds of one kind: these members, or any.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Held {
    Of(Members),
    Any,
}

impl Held {
    fn nothing() -> Self {
        Held::Of(Members::nothing())
    }

    fn one(number: u32) -> Self {
        Held::Of(Members::Few(Ids::one(number)))
    }
}

impl Lattice for Held {
    fn bottom() -> Self {
        Held::nothing()
    }

    fn join(&mut self, incoming: Self) {
        self.gain(&incoming);
    }

    // What a set gained is its new members, or any.
    fn gain(&mut self, incoming: &Self) -> Option<Self> {
        match (&mut *self, incoming) {
            (Held::Any, _) => None,
            (_, Held::Any) => {
                *self = Held::Any;

                Some(Held::Any)
            }
            (Held::Of(held), Held::Of(incoming)) => {
                let gained = held.gain(incoming);
                (!gained.is_empty()).then_some(Held::Of(gained))
            }
        }
    }
}

/// What one value may be: a closure of these functions, an aggregate built at these constructions, each by its number.
#[derive(Debug, Clone, PartialEq, Eq)]
struct ValueFact {
    functions: Held,
    constructions: Held,
}

impl ValueFact {
    fn nothing() -> Self {
        ValueFact {
            functions: Held::nothing(),
            constructions: Held::nothing(),
        }
    }

    fn any() -> Self {
        ValueFact {
            functions: Held::Any,
            constructions: Held::Any,
        }
    }

    fn function(function: u32) -> Self {
        ValueFact {
            functions: Held::one(function),
            constructions: Held::nothing(),
        }
    }

    fn construction(node: u32) -> Self {
        ValueFact {
            functions: Held::nothing(),
            constructions: Held::one(node),
        }
    }
}

/// The numbers a solve gives the functions and the constructions a fact may hold, ascending with their identities, so a fact's members read back ascending. A function is one some node names, the only way one comes to be a value; a construction is a row's or a tuple's.
#[derive(PartialEq)]
struct Numbering {
    functions: Vec<FunctionId>,
    function_numbers: BTreeMap<FunctionId, u32>,
    constructions: Vec<NodeId>,
    construction_numbers: BTreeMap<NodeId, u32>,
    /// The row each numbered construction builds, `None` for a tuple's.
    built: Vec<Option<RowId>>,
}

impl Numbering {
    fn of(module: &Module, constructions: &Constructions) -> Self {
        let functions = module.escaping_functions().into_iter().collect::<Vec<_>>();
        let mut built = constructions
            .rows
            .iter()
            .flat_map(|(row, nodes)| nodes.iter().map(move |node| (*node, Some(*row))))
            .chain(constructions.tuples.iter().map(|node| (*node, None)))
            .collect::<Vec<_>>();
        built.sort_unstable_by_key(|(node, _)| *node);
        let number = |index: usize| {
            u32::try_from(index)
                .expect("a module numbers fewer than 2^32 functions and constructions")
        };
        Numbering {
            function_numbers: functions
                .iter()
                .enumerate()
                .map(|(index, function)| (*function, number(index)))
                .collect(),
            functions,
            construction_numbers: built
                .iter()
                .enumerate()
                .map(|(index, (node, _))| (*node, number(index)))
                .collect(),
            constructions: built.iter().map(|(node, _)| *node).collect(),
            built: built.into_iter().map(|(_, row)| row).collect(),
        }
    }

    fn function(&self, function: FunctionId) -> u32 {
        self.function_numbers[&function]
    }

    fn construction(&self, node: NodeId) -> u32 {
        self.construction_numbers[&node]
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
            functions: functions.unwrap_or_else(Held::nothing),
            constructions: constructions.unwrap_or_else(Held::nothing),
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
    numbering: Numbering,
}

impl ValueFlow {
    pub fn of(module: &Module) -> Self {
        let constructions = Constructions::of(module);
        let numbering = Numbering::of(module, &constructions);
        let facts = solve(module, &constructions, &numbering);
        Self { facts, numbering }
    }

    /// The rows of the constructions `fact` may be, every row where it may be any.
    fn rows_of(&self, fact: &ValueFact) -> Rows {
        match &fact.constructions {
            Held::Of(constructions) => MayBe::Of(
                constructions
                    .iter()
                    .filter_map(|construction| self.numbering.built[construction as usize])
                    .collect(),
            ),
            Held::Any => MayBe::Any,
        }
    }

    /// The functions `atom` may be a closure of: a function it names, or what reaches the value it names.
    pub fn functions(&self, atom: &Atom) -> MayBe<FunctionId> {
        match atom {
            Atom::Fun(function) => MayBe::one(*function),
            Atom::Value(value) => {
                self.facts
                    .get(&FlowKey::Value(*value))
                    .map_or(MayBe::Any, |fact| match &fact.functions {
                        Held::Of(functions) => MayBe::Of(
                            functions
                                .iter()
                                .map(|function| self.numbering.functions[function as usize])
                                .collect(),
                        ),
                        Held::Any => MayBe::Any,
                    })
            }
            Atom::Literal(_) | Atom::Filler => MayBe::nothing(),
        }
    }
}

impl ValueFlow {
    /// The rows `value` may be a struct of, `None` where the module holds no such value. A reader asks it of the values it reads: a module's values are tens of thousands and a fact hundreds of constructions wide, where the readers ask a few hundred.
    pub fn rows(&self, value: ValueId) -> Option<Rows> {
        self.facts
            .get(&FlowKey::Value(value))
            .map(|fact| self.rows_of(fact))
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
fn solve(
    module: &Module,
    constructions: &Constructions,
    numbering: &Numbering,
) -> BTreeMap<FlowKey, ValueFact> {
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
        Atom::Fun(function) => Source::Fact(ValueFact::function(numbering.function(*function))),
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
    // Slot `index` of an object of `gained`: under each construction of `kind`, a row's or a tuple's, it may be, or the summary where it may be any.
    let read = |solver: &mut Solver<ValueFact, FlowKey>,
                gained: &ValueFact,
                kind: Option<RowId>,
                summary: FlowKey,
                index: usize,
                result: FlowKey| match &gained.constructions {
        Held::Of(objects) => {
            for object in objects
                .iter()
                .filter(|object| numbering.built[*object as usize] == kind)
            {
                solver.copy(
                    FlowKey::At(numbering.constructions[object as usize], index),
                    result,
                );
            }
        }
        Held::Any => solver.copy(summary, result),
    };
    // The functions a callee may be that take `arity` parameters, every escaping one of them where it may be any.
    let callees = |functions: &Held, arity: usize| -> Vec<FunctionId> {
        let takes = |function: &FunctionId| {
            module
                .function(*function)
                .is_some_and(|definition| definition.params.len() == arity)
        };
        match functions {
            Held::Of(functions) => functions
                .iter()
                .map(|function| numbering.functions[function as usize])
                .filter(takes)
                .collect(),
            Held::Any => numbering.functions.iter().copied().filter(takes).collect(),
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
                        solver.join(
                            FlowKey::Value(*result),
                            ValueFact::construction(numbering.construction(id)),
                        );
                    }),
                    ValueExpr::Tuple(atoms) => rules.add(move |solver| {
                        for (index, atom) in atoms.iter().enumerate() {
                            flow(solver, source(atom), FlowKey::Field(index));
                            flow(solver, source(atom), FlowKey::At(id, index));
                        }
                        solver.join(
                            FlowKey::Value(*result),
                            ValueFact::construction(numbering.construction(id)),
                        );
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
                            let objects = match &gained.constructions {
                                Held::Of(objects) => objects
                                    .iter()
                                    .filter(|object| {
                                        numbering.built[*object as usize] == Some(*row)
                                    })
                                    .collect::<Vec<_>>(),
                                Held::Any => constructions
                                    .of_row(*row)
                                    .iter()
                                    .map(|node| numbering.construction(*node))
                                    .collect(),
                            };
                            for &object in &objects {
                                for (index, atom) in slots {
                                    flow(
                                        solver,
                                        source(atom),
                                        FlowKey::At(
                                            numbering.constructions[object as usize],
                                            *index,
                                        ),
                                    );
                                }
                            }
                            solver.join(
                                FlowKey::Value(*result),
                                ValueFact {
                                    functions: Held::nothing(),
                                    constructions: Held::Of(Members::of_ascending(objects)),
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
                                read(
                                    solver,
                                    gained,
                                    Some(*row),
                                    FlowKey::Slot(*row, *index),
                                    *index,
                                    result,
                                );
                            });
                        }
                        Intrinsic::TupleGet(index) => {
                            watch(rules, source(&args[0]), move |solver, gained| {
                                read(solver, gained, None, FlowKey::Field(*index), *index, result);
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
