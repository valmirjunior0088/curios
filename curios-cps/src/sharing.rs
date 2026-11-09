//! Which constructions of a row may write into a value of that row dying beside them: the proof [`rebuild_in_place`](super::rebuild_in_place) spends ([A value costs when it is kept, not when it is named](../../documentation/design/compilation/a-value-costs-when-it-is-kept-not-when-it-is-named.md)).
//!
//! **Three facts, each on a path.** Whether a value is consumed or only read, and where it is used last: a function's body and each continuation's are blocks, a node has a place along one, and a switch's edge is a path of its own, so a value one arm hands on and another returns is used once. Whether a value is unique: the greatest fixpoint the shared solver reaches as the least one of its dual, *shared*, over values and function returns, with every boundary the module does not see whole shared — an escaping function's or the entry's parameters, what a closure, a host, a cell or a channel hands back, a value a nested function captures, a literal and constant data. A parameter is unique where every known call hands it a unique value dead after the call; a field read of a unique value is unique where no path from the read consumes that value, which is Perceus's rule and what lets a path copy qualify.
//!
//! **Constants by tag.** A constant is one global every use shares, and it reaches every read of a slot it is stored in, so the constructor tags of the constant data that may reach each value flow through slots, calls and returns; a value some constant may be is rebuilt only within the arms of a test of its tag that rule every such tag out.
//!
//! **Borrowing.** A parameter a function only reads is borrowed, so its caller keeps the value it handed, unless the function rebuilds it, captures it, or must own a field of it, as Lean's inference owns what it resets. Which parameters are rebuilt is the candidates' own answer, so the facts are found twice: owning every parameter, then borrowing what the first answer leaves.

use {
    super::{
        Atom, Callee, ContinuationId, FunctionId, Intrinsic, Lattice, Literal, Module, Node,
        NodeId, RowId, Slot, Solver, ValueExpr, ValueId, analyze_calls,
    },
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet},
    },
};

/// A rebuild the facts admit: the construction at `node`, of `row`, may write into `of`, writing only `slots`.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Rebuild {
    pub(crate) node: NodeId,
    pub(crate) of: ValueId,
    pub(crate) row: RowId,
    pub(crate) slots: Vec<(usize, Atom)>,
}

/// Every construction of a row the module may rebuild in place, in node order.
pub(crate) fn rebuilds(module: &Module) -> Vec<Rebuild> {
    let shape = Shape::of(module);
    let tags = constant_tags(module, &shape);
    let owned = BTreeSet::new();
    let first = Facts::solve(module, &shape, &owned).rebuilds(&tags);
    let reused = first
        .iter()
        .map(|rebuild| rebuild.of)
        .collect::<BTreeSet<_>>();
    let borrowed = shape.borrowed(module, &reused);
    match borrowed.is_empty() {
        true => first,
        false => Facts::solve(module, &shape, &borrowed).rebuilds(&tags),
    }
}

/// A function's body or a continuation's: a chain of nodes, entered at its first.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Block {
    Body(FunctionId),
    Cont(ContinuationId),
}

/// How a node uses one of its operands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Site {
    /// A read that keeps nothing: a projection, a row test, a switch's scrutinee, a closure called.
    Read,
    /// Stored in a construction or written by a rebuild.
    Store,
    /// Handed to a known function's parameter.
    Call(FunctionId, usize),
    /// Handed to a continuation, on a jump or on one edge of a switch.
    Jump(ContinuationId),
    /// Handed back to the function's caller.
    Return(usize),
    /// Handed where nothing in the module follows it: a closure's argument, a host's, a cell's, a channel's, an operation's, or the value a rebuild writes into.
    Escape,
}

/// Where a value is defined.
#[derive(Debug, Clone, Copy)]
enum Def {
    Param(FunctionId, usize),
    ContParam(ContinuationId, usize),
    /// A projection of a value, by a row read or a tuple read.
    Field(ValueId),
    /// A construction or a rebuild: fresh where it is no constant.
    Construction,
    /// A literal, an operation's result: no row.
    Other,
}

/// The edge of a switch a use hands its value on, by its target and its case, `None` for the default; `None` for a use at a node.
type Edge = Option<(ContinuationId, Option<u32>)>;

/// One way a continuation is entered: the arguments a jump or a switch's edge hands it, and the switch and key where it is a case.
#[derive(Debug, Clone)]
struct Entry {
    args: Vec<Atom>,
    case: Option<(NodeId, u32)>,
}

/// A point a value is consumed at: a node, or one edge of a switch, which the switch's other edges do not take.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Point {
    node: NodeId,
    into: Edge,
}

/// The module read once: where each node sits, how each value is defined and used, and what is live where.
struct Shape {
    /// The block and the place along it of every node of a live function.
    place: BTreeMap<NodeId, (Block, usize)>,
    /// The function each block belongs to.
    owner: BTreeMap<Block, FunctionId>,
    /// The blocks a block's last node transfers to within its function.
    successors: BTreeMap<Block, Vec<Block>>,
    /// The block each continuation is defined in, and the place of the node defining it.
    defined_at: BTreeMap<ContinuationId, NodeId>,
    defs: BTreeMap<ValueId, Def>,
    /// The node binding each value a node binds.
    binder: BTreeMap<ValueId, NodeId>,
    /// The function each value belongs to.
    home: BTreeMap<ValueId, FunctionId>,
    uses: BTreeMap<ValueId, Vec<(NodeId, Site, Edge)>>,
    /// The one row a value is read at, by every row read and row test of it.
    read_at: BTreeMap<ValueId, Option<RowId>>,
    /// The projections of each value.
    fields: BTreeMap<ValueId, Vec<ValueId>>,
    /// The row reads of each value: the row and what the read binds.
    row_reads: BTreeMap<ValueId, Vec<(RowId, ValueId)>>,
    /// The values each function's nodes name but do not bind, and the functions defined within it name likewise.
    captured: BTreeSet<ValueId>,
    /// What is live on entry to each block.
    live_in: BTreeMap<Block, BTreeSet<ValueId>>,
    /// The last place along its block each value is named at.
    last_named: BTreeMap<(Block, ValueId), usize>,
    /// The known calls of each function.
    calls: BTreeMap<FunctionId, Vec<NodeId>>,
    /// The calls each continuation resumes, and the edges of a switch that enter it with their case.
    resumes: BTreeMap<ContinuationId, Vec<NodeId>>,
    entries: BTreeMap<ContinuationId, Vec<Entry>>,
    returns: BTreeMap<FunctionId, Vec<NodeId>>,
    /// The functions whose parameters callers outside the module's sight hand values to: the escaping ones and the entry.
    open: BTreeSet<FunctionId>,
    /// The blocks reachable from each block's successors, as asked.
    reach: RefCell<BTreeMap<Block, BTreeSet<Block>>>,
}

impl Shape {
    fn of(module: &Module) -> Self {
        let mut shape = Shape {
            place: BTreeMap::new(),
            owner: BTreeMap::new(),
            successors: BTreeMap::new(),
            defined_at: BTreeMap::new(),
            defs: BTreeMap::new(),
            binder: BTreeMap::new(),
            home: BTreeMap::new(),
            uses: BTreeMap::new(),
            read_at: BTreeMap::new(),
            fields: BTreeMap::new(),
            row_reads: BTreeMap::new(),
            captured: BTreeSet::new(),
            live_in: BTreeMap::new(),
            last_named: BTreeMap::new(),
            calls: BTreeMap::new(),
            resumes: BTreeMap::new(),
            entries: BTreeMap::new(),
            returns: BTreeMap::new(),
            open: analyze_calls(module).escaping,
            reach: RefCell::new(BTreeMap::new()),
        };
        shape.open.extend(module.entry());
        let mut free = BTreeMap::<FunctionId, BTreeSet<ValueId>>::new();
        let mut nested = BTreeMap::<NodeId, Vec<FunctionId>>::new();
        let mut blocks = BTreeMap::<Block, Vec<NodeId>>::new();
        for (function, definition) in module.live_functions() {
            for (index, param) in definition.params.iter().enumerate() {
                shape.defs.insert(*param, Def::Param(function, index));
                shape.home.insert(*param, function);
            }
            let mut bound = definition.params.iter().copied().collect::<BTreeSet<_>>();
            let mut named = BTreeSet::new();
            let mut work = vec![(Block::Body(function), definition.body)];
            while let Some((block, start)) = work.pop() {
                shape.owner.insert(block, function);
                let mut chain = Vec::new();
                let mut at = start;
                loop {
                    shape.place.insert(at, (block, chain.len()));
                    chain.push(at);
                    let node = module.node(at).unwrap();
                    for (value, site, into) in node_sites(node, definition.return_cont) {
                        named.insert(value);
                        shape.uses.entry(value).or_default().push((at, site, into));
                    }
                    match node {
                        Node::LetValue {
                            result,
                            value,
                            next,
                        } => {
                            bound.insert(*result);
                            shape.home.insert(*result, function);
                            shape.binder.insert(*result, at);
                            shape.defs.insert(
                                *result,
                                match value {
                                    ValueExpr::Literal(_) => Def::Other,
                                    ValueExpr::List(_)
                                    | ValueExpr::Tuple(_)
                                    | ValueExpr::Row(..)
                                    | ValueExpr::Rebuilt { .. } => Def::Construction,
                                },
                            );
                            at = *next;
                        }
                        Node::LetIntrinsic {
                            result,
                            op,
                            args,
                            next,
                        } => {
                            bound.insert(*result);
                            shape.home.insert(*result, function);
                            shape.binder.insert(*result, at);
                            let def = match (op, args.first()) {
                                (
                                    Intrinsic::RowGet(..) | Intrinsic::TupleGet(_),
                                    Some(Atom::Value(of)),
                                ) => {
                                    shape.fields.entry(*of).or_default().push(*result);
                                    if let Intrinsic::RowGet(row, _) = op {
                                        shape
                                            .row_reads
                                            .entry(*of)
                                            .or_default()
                                            .push((*row, *result));
                                    }
                                    Def::Field(*of)
                                }
                                _ => Def::Other,
                            };
                            shape.defs.insert(*result, def);
                            if let (
                                Intrinsic::RowGet(row, _) | Intrinsic::IsRow(row),
                                Some(Atom::Value(read)),
                            ) = (op, args.first())
                            {
                                let entry = shape.read_at.entry(*read).or_insert(Some(*row));
                                if *entry != Some(*row) {
                                    *entry = None;
                                }
                            }
                            at = *next;
                        }
                        Node::LetFun { functions, body } => {
                            nested.insert(at, functions.clone());
                            at = *body;
                        }
                        Node::LetCont {
                            continuations,
                            body,
                        } => {
                            for continuation in continuations {
                                if let Some(definition) = module.continuation(*continuation) {
                                    shape.defined_at.insert(*continuation, at);
                                    for (index, param) in definition.params.iter().enumerate() {
                                        bound.insert(*param);
                                        shape.home.insert(*param, function);
                                        shape
                                            .defs
                                            .insert(*param, Def::ContParam(*continuation, index));
                                    }
                                    work.push((Block::Cont(*continuation), definition.body));
                                }
                            }
                            at = *body;
                        }
                        terminator => {
                            let mut successors = Vec::new();
                            let mut enter = |target: ContinuationId| {
                                if target != definition.return_cont {
                                    successors.push(Block::Cont(target));
                                }
                            };
                            match terminator {
                                Node::ApplyFun {
                                    callee, return_to, ..
                                } => {
                                    if let Callee::Known(callee) = callee {
                                        shape.calls.entry(*callee).or_default().push(at);
                                    }
                                    match *return_to == definition.return_cont {
                                        true => shape.returns.entry(function).or_default().push(at),
                                        false => {
                                            shape.resumes.entry(*return_to).or_default().push(at)
                                        }
                                    }
                                    enter(*return_to);
                                }
                                Node::Foreign { return_to, .. }
                                | Node::Cell { return_to, .. }
                                | Node::Channel { return_to, .. }
                                | Node::Intrinsic { return_to, .. } => {
                                    match *return_to == definition.return_cont {
                                        true => shape.returns.entry(function).or_default().push(at),
                                        false => {
                                            shape.resumes.entry(*return_to).or_default().push(at)
                                        }
                                    }
                                    enter(*return_to);
                                }
                                Node::ApplyCont(edge) => {
                                    match edge.target == definition.return_cont {
                                        true => shape.returns.entry(function).or_default().push(at),
                                        false => {
                                            shape.entries.entry(edge.target).or_default().push(
                                                Entry {
                                                    args: edge.args.clone(),
                                                    case: None,
                                                },
                                            )
                                        }
                                    }
                                    enter(edge.target);
                                }
                                Node::Switch { cases, default, .. } => {
                                    for (key, edge) in cases {
                                        shape.entries.entry(edge.target).or_default().push(Entry {
                                            args: edge.args.clone(),
                                            case: Some((at, *key)),
                                        });
                                        enter(edge.target);
                                    }
                                    if let Some(edge) = default {
                                        shape.entries.entry(edge.target).or_default().push(Entry {
                                            args: edge.args.clone(),
                                            case: None,
                                        });
                                        enter(edge.target);
                                    }
                                }
                                _ => {}
                            }
                            shape.successors.insert(block, successors);
                            break;
                        }
                    }
                }
                blocks.insert(block, chain);
            }
            free.insert(function, named.difference(&bound).copied().collect());
        }
        // What a nested function names and binds nowhere is captured where it is defined, so a function's free values include its members'.
        loop {
            let mut changed = false;
            for (node, members) in &nested {
                let Some((block, _)) = shape.place.get(node) else {
                    continue;
                };
                let function = shape.owner[block];
                let captured = members
                    .iter()
                    .flat_map(|member| free.get(member).cloned().unwrap_or_default())
                    .filter(|value| shape.home.get(value) != Some(&function))
                    .collect::<Vec<_>>();
                let entry = free.entry(function).or_default();
                for value in captured {
                    changed |= entry.insert(value);
                }
            }
            if !changed {
                break;
            }
        }
        shape.captured = free.values().flatten().copied().collect();
        // A function defined at a node names what it captures there.
        for (node, members) in &nested {
            let Some(&(block, at)) = shape.place.get(node) else {
                continue;
            };
            for value in members
                .iter()
                .flat_map(|member| free.get(member).cloned().unwrap_or_default())
            {
                let last = shape.last_named.entry((block, value)).or_insert(at);
                *last = (*last).max(at);
            }
        }
        for (value, uses) in &shape.uses {
            for (node, _, _) in uses {
                let (block, at) = shape.place[node];
                let last = shape.last_named.entry((block, *value)).or_insert(at);
                *last = (*last).max(at);
            }
        }
        shape.live_in = live_in(module, &shape, &blocks);
        shape
    }

    /// Whether `value` is read after the consumption at `point`, on its edge.
    fn live_after(&self, module: &Module, point: Point, value: ValueId) -> bool {
        match point.into {
            Some((target, _)) => module.continuation(target).is_some_and(|_| {
                self.live_in
                    .get(&Block::Cont(target))
                    .is_some_and(|live| live.contains(&value))
            }),
            None => self.live_past(point.node, value),
        }
    }

    /// Whether `value` is named past `node`: later along its block, or on entry to a block after it.
    fn live_past(&self, node: NodeId, value: ValueId) -> bool {
        let (block, at) = self.place[&node];
        self.last_named
            .get(&(block, value))
            .is_some_and(|last| *last > at)
            || self.successors[&block].iter().any(|successor| {
                self.live_in
                    .get(successor)
                    .is_some_and(|live| live.contains(&value))
            })
    }

    /// Whether `value` is named at `node` or past it.
    fn live_at(&self, node: NodeId, value: ValueId) -> bool {
        let (block, at) = self.place[&node];
        self.last_named
            .get(&(block, value))
            .is_some_and(|last| *last >= at)
            || self.live_past(node, value)
    }

    /// Whether `to` can run after `from` in their function.
    fn reaches(&self, from: NodeId, to: NodeId) -> bool {
        let ((from_block, from_at), (to_block, to_at)) = (self.place[&from], self.place[&to]);
        (from_block == to_block && to_at > from_at) || self.reached(from_block).contains(&to_block)
    }

    /// Whether `to` can run after the consumption at `point`, on its edge.
    fn point_reaches(&self, point: Point, to: NodeId) -> bool {
        match point.into {
            Some((target, _)) => {
                let (to_block, _) = self.place[&to];
                to_block == Block::Cont(target)
                    || self.reached(Block::Cont(target)).contains(&to_block)
            }
            None => point.node == to || self.reaches(point.node, to),
        }
    }

    /// The blocks reachable from `block`'s successors.
    fn reached(&self, block: Block) -> BTreeSet<Block> {
        if let Some(reached) = self.reach.borrow().get(&block) {
            return reached.clone();
        }
        let mut reached = BTreeSet::new();
        let mut work = self.successors.get(&block).cloned().unwrap_or_default();
        while let Some(next) = work.pop() {
            if reached.insert(next) {
                work.extend(self.successors.get(&next).cloned().unwrap_or_default());
            }
        }
        self.reach.borrow_mut().insert(block, reached.clone());
        reached
    }

    /// Where `value` is consumed, a call to a borrowed parameter being a read.
    fn consumed(&self, value: ValueId, borrowed: &BTreeSet<(FunctionId, usize)>) -> Vec<Point> {
        self.uses
            .get(&value)
            .into_iter()
            .flatten()
            .filter(|(_, site, _)| match site {
                Site::Read => false,
                Site::Call(callee, index) => !borrowed.contains(&(*callee, *index)),
                _ => true,
            })
            .map(|(node, _, into)| Point {
                node: *node,
                into: *into,
            })
            .collect()
    }

    /// Whether a read of `value` at `row` runs on every path to `node`, its result in scope there: the read's checked cast is what says `value` is a struct of that row, a value read as a tuple on one arm and as a row on another being either.
    fn read_as_row_before(&self, value: ValueId, row: RowId, node: NodeId) -> bool {
        self.row_reads
            .get(&value)
            .into_iter()
            .flatten()
            .any(|(read, result)| *read == row && self.in_scope(*result, node))
    }

    /// Whether a run can come back to `node` without binding `value` again: a rebuild there would write the memory the last round's rebuild handed on.
    fn returns_without_rebinding(&self, value: ValueId, node: NodeId) -> bool {
        let (block, _) = self.place[&node];
        let home = match self.defs.get(&value) {
            Some(Def::Param(function, _)) => Block::Body(*function),
            Some(Def::ContParam(continuation, _)) => Block::Cont(*continuation),
            _ => match self.binder.get(&value) {
                Some(binder) => self.place[binder].0,
                None => return true,
            },
        };
        let mut seen = BTreeSet::new();
        let mut work = self.successors.get(&block).cloned().unwrap_or_default();
        while let Some(next) = work.pop() {
            if next == home || !seen.insert(next) {
                continue;
            }
            if next == block {
                return true;
            }
            work.extend(self.successors.get(&next).cloned().unwrap_or_default());
        }
        false
    }

    /// Whether `value` is defined where `node` can name it: before it along a block enclosing it.
    fn in_scope(&self, value: ValueId, node: NodeId) -> bool {
        let anchor = match self.defs.get(&value) {
            Some(Def::Param(function, _)) => {
                return self
                    .place
                    .get(&node)
                    .is_some_and(|(block, _)| self.owner[block] == *function);
            }
            Some(Def::ContParam(continuation, _)) => Some((Block::Cont(*continuation), 0)),
            _ => self.binder.get(&value).map(|binder| {
                let (block, at) = self.place[binder];
                (block, at + 1)
            }),
        };
        let Some((home, from)) = anchor else {
            return false;
        };
        let mut current = node;
        loop {
            let (block, at) = self.place[&current];
            if block == home {
                return at >= from;
            }
            match block {
                Block::Cont(continuation) => match self.defined_at.get(&continuation) {
                    Some(definer) => current = *definer,
                    None => return false,
                },
                Block::Body(_) => return false,
            }
        }
    }

    /// The parameters a function only reads, given the values rebuilt when every parameter is owned: optimistic, each demoted where it must own.
    fn borrowed(
        &self,
        module: &Module,
        reused: &BTreeSet<ValueId>,
    ) -> BTreeSet<(FunctionId, usize)> {
        let mut borrowed = module
            .live_functions()
            .filter(|(function, _)| !self.open.contains(function))
            .flat_map(|(function, definition)| {
                (0..definition.params.len()).map(move |index| (function, index))
            })
            .collect::<BTreeSet<_>>();
        loop {
            let snapshot = borrowed.clone();
            borrowed.retain(|(function, index)| {
                let param = module.function(*function).unwrap().params[*index];
                !self.must_own(param, &snapshot, reused)
            });
            if borrowed.len() == snapshot.len() {
                return borrowed;
            }
        }
    }

    /// Whether a function must own `value`: it consumes it, captures it or rebuilds it, or must own a field of it.
    fn must_own(
        &self,
        value: ValueId,
        borrowed: &BTreeSet<(FunctionId, usize)>,
        reused: &BTreeSet<ValueId>,
    ) -> bool {
        reused.contains(&value)
            || self.captured.contains(&value)
            || !self.consumed(value, borrowed).is_empty()
            || self
                .fields
                .get(&value)
                .into_iter()
                .flatten()
                .any(|field| self.must_own(*field, borrowed, reused))
    }
}

/// How `node` uses each operand it names, with the edge of a switch it hands it on.
fn node_sites(node: &Node, own_return: ContinuationId) -> Vec<(ValueId, Site, Edge)> {
    let mut sites = Vec::new();
    let mut push = |atom: &Atom, site: Site, into: Edge| {
        if let Atom::Value(value) = atom {
            sites.push((*value, site, into));
        }
    };
    match node {
        Node::LetValue { value, .. } => match value {
            ValueExpr::Literal(_) => {}
            ValueExpr::List(atoms) | ValueExpr::Tuple(atoms) | ValueExpr::Row(_, atoms) => {
                atoms.iter().for_each(|atom| push(atom, Site::Store, None))
            }
            ValueExpr::Rebuilt { of, slots, .. } => {
                push(of, Site::Escape, None);
                slots
                    .iter()
                    .for_each(|(_, atom)| push(atom, Site::Store, None));
            }
        },
        Node::LetIntrinsic { op, args, .. } => {
            let site = match op {
                Intrinsic::RowGet(..)
                | Intrinsic::TupleGet(_)
                | Intrinsic::IsRow(_)
                | Intrinsic::IsImmediate => Site::Read,
                _ => Site::Escape,
            };
            args.iter().for_each(|atom| push(atom, site, None));
        }
        Node::ApplyFun { callee, args, .. } => {
            if let Callee::Closure(closure) = callee {
                push(&Atom::Value(*closure), Site::Read, None);
            }
            for (index, atom) in args.iter().enumerate() {
                let site = match callee {
                    Callee::Known(function) => Site::Call(*function, index),
                    Callee::Closure(_) => Site::Escape,
                };
                push(atom, site, None);
            }
        }
        Node::ApplyCont(edge) => {
            for (index, atom) in edge.args.iter().enumerate() {
                let site = match edge.target == own_return {
                    true => Site::Return(index),
                    false => Site::Jump(edge.target),
                };
                push(atom, site, None);
            }
        }
        Node::Switch {
            scrutinee,
            cases,
            default,
        } => {
            push(scrutinee, Site::Read, None);
            let keyed = cases.iter().map(|(key, edge)| (Some(*key), edge));
            for (key, edge) in keyed.chain(default.iter().map(|edge| (None, edge))) {
                for atom in &edge.args {
                    let site = match edge.target == own_return {
                        true => Site::Escape,
                        false => Site::Jump(edge.target),
                    };
                    push(atom, site, Some((edge.target, key)));
                }
            }
        }
        Node::Foreign { args, .. }
        | Node::Halt { args, .. }
        | Node::Cell { args, .. }
        | Node::Channel { args, .. }
        | Node::Intrinsic { args, .. } => {
            args.iter().for_each(|atom| push(atom, Site::Escape, None))
        }
        Node::LetFun { .. } | Node::LetCont { .. } | Node::Panic(_) | Node::Unreachable => {}
    }
    sites
}

/// What is live on entry to each block: what it names before binding, and what a block after it needs, short of what its successor binds as parameters.
fn live_in(
    module: &Module,
    shape: &Shape,
    blocks: &BTreeMap<Block, Vec<NodeId>>,
) -> BTreeMap<Block, BTreeSet<ValueId>> {
    let mut named = BTreeMap::<Block, BTreeSet<ValueId>>::new();
    for (block, value) in shape.last_named.keys() {
        named.entry(*block).or_default().insert(*value);
    }
    let bound = |block: Block| -> BTreeSet<ValueId> {
        let mut bound = BTreeSet::new();
        if let Block::Cont(continuation) = block
            && let Some(definition) = module.continuation(continuation)
        {
            bound.extend(&definition.params);
        }
        for node in blocks.get(&block).into_iter().flatten() {
            if let Some(Node::LetValue { result, .. } | Node::LetIntrinsic { result, .. }) =
                module.node(*node)
            {
                bound.insert(*result);
            }
        }
        bound
    };
    let bound = blocks
        .keys()
        .map(|block| (*block, bound(*block)))
        .collect::<BTreeMap<_, _>>();
    let mut live = BTreeMap::<Block, BTreeSet<ValueId>>::new();
    loop {
        let mut changed = false;
        for block in blocks.keys().rev() {
            let mut entry = named.get(block).cloned().unwrap_or_default();
            for successor in &shape.successors[block] {
                entry.extend(live.get(successor).into_iter().flatten());
            }
            entry.retain(|value| !bound[block].contains(value));
            if live.get(block) != Some(&entry) {
                live.insert(*block, entry);
                changed = true;
            }
        }
        if !changed {
            return live;
        }
    }
}

/// Whether a value is shared: the dual the solver's least fixpoint finds, uniqueness being what it leaves.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
struct Shared(bool);

impl Lattice for Shared {
    fn bottom() -> Self {
        Shared(false)
    }

    fn join(&mut self, incoming: Self) {
        self.0 |= incoming.0;
    }
}

/// A value's fact, or a function's result's at one position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Key {
    Value(ValueId),
    Returned(FunctionId, usize),
}

/// The uniqueness the module proves, under one choice of borrowed parameters.
struct Facts<'s> {
    module: &'s Module,
    shape: &'s Shape,
    borrowed: &'s BTreeSet<(FunctionId, usize)>,
    shared: BTreeSet<ValueId>,
}

impl<'s> Facts<'s> {
    fn solve(
        module: &'s Module,
        shape: &'s Shape,
        borrowed: &'s BTreeSet<(FunctionId, usize)>,
    ) -> Self {
        let constant = module.constant_data();
        let arities = module.return_arities();
        let mut sources = BTreeSet::new();
        for (value, def) in &shape.defs {
            let consumed = shape.consumed(*value, borrowed);
            let mut seen = BTreeSet::new();
            let twice = consumed.iter().any(|point| !seen.insert(*point));
            let not_last = consumed
                .iter()
                .any(|point| shape.live_after(module, *point, *value));
            let source = match def {
                Def::Other => true,
                Def::Construction => constant.contains(value),
                Def::Param(function, index) => {
                    shape.open.contains(function)
                        || borrowed.contains(&(*function, *index))
                        || shape.calls.get(function).is_none_or(|calls| {
                            calls
                                .iter()
                                .any(|call| !handed_dead(module, shape, *call, *index))
                        })
                }
                Def::ContParam(continuation, index) => {
                    shape
                        .entries
                        .get(continuation)
                        .into_iter()
                        .flatten()
                        .any(|entry| {
                            !arg_dead(
                                shape,
                                &entry.args[*index],
                                &entry.args,
                                Block::Cont(*continuation),
                            )
                        })
                        || shape
                            .resumes
                            .get(continuation)
                            .into_iter()
                            .flatten()
                            .any(|node| {
                                !matches!(
                                    module.node(*node),
                                    Some(Node::ApplyFun {
                                        callee: Callee::Known(_),
                                        ..
                                    })
                                )
                            })
                }
                Def::Field(_) => false,
            };
            if source || twice || not_last || shape.captured.contains(value) {
                sources.insert(Key::Value(*value));
            }
        }
        for (function, nodes) in &shape.returns {
            for node in nodes {
                let opaque = match module.node(*node) {
                    Some(Node::ApplyCont(edge)) => edge
                        .args
                        .iter()
                        .enumerate()
                        .filter(|(_, atom)| !matches!(atom, Atom::Value(_)))
                        .map(|(index, _)| index)
                        .collect(),
                    Some(Node::ApplyFun {
                        callee: Callee::Known(_),
                        ..
                    }) => Vec::new(),
                    _ => (0..arities.get(function).copied().unwrap_or(0)).collect(),
                };
                sources.extend(
                    opaque
                        .into_iter()
                        .map(|index| Key::Returned(*function, index)),
                );
            }
        }
        let keys =
            shape
                .defs
                .keys()
                .map(|value| Key::Value(*value))
                .chain(arities.iter().flat_map(|(function, arity)| {
                    (0..*arity).map(|index| Key::Returned(*function, index))
                }));
        let (sources, arities) = (&sources, &arities);
        let facts = Solver::solve(keys, |rules| {
            rules.add(move |solver| {
                for key in sources {
                    solver.join(*key, Shared(true));
                }
            });
            let shared = |solver: &Solver<Shared, Key>, atom: &Atom| match atom {
                Atom::Value(value) => solver.fact(&Key::Value(*value)).is_none_or(|fact| fact.0),
                _ => true,
            };
            for (value, def) in &shape.defs {
                let value = *value;
                match *def {
                    Def::Field(of) => {
                        let read = shape.binder[&value];
                        let consumed_after = shape
                            .consumed(of, borrowed)
                            .iter()
                            .any(|point| shape.reaches(read, point.node));
                        rules.add(move |solver| {
                            if consumed_after || shared(solver, &Atom::Value(of)) {
                                solver.join(Key::Value(value), Shared(true));
                            }
                        });
                    }
                    Def::Param(function, index) => {
                        let calls = shape.calls.get(&function).cloned().unwrap_or_default();
                        rules.add(move |solver| {
                            for call in &calls {
                                if let Some(Node::ApplyFun { args, .. }) = module.node(*call)
                                    && args.get(index).is_none_or(|atom| shared(solver, atom))
                                {
                                    solver.join(Key::Value(value), Shared(true));
                                }
                            }
                        });
                    }
                    Def::ContParam(continuation, index) => {
                        let entries = shape
                            .entries
                            .get(&continuation)
                            .cloned()
                            .unwrap_or_default();
                        let resumes = shape
                            .resumes
                            .get(&continuation)
                            .cloned()
                            .unwrap_or_default();
                        rules.add(move |solver| {
                            let entered = entries.iter().any(|entry| {
                                entry
                                    .args
                                    .get(index)
                                    .is_none_or(|atom| shared(solver, atom))
                            });
                            let resumed = resumes.iter().any(|node| match module.node(*node) {
                                Some(Node::ApplyFun {
                                    callee: Callee::Known(callee),
                                    ..
                                }) => solver
                                    .fact(&Key::Returned(*callee, index))
                                    .is_none_or(|fact| fact.0),
                                _ => true,
                            });
                            if entered || resumed {
                                solver.join(Key::Value(value), Shared(true));
                            }
                        });
                    }
                    Def::Construction | Def::Other => {}
                }
            }
            for (function, nodes) in &shape.returns {
                let function = *function;
                for node in nodes {
                    let node = *node;
                    rules.add(move |solver| match module.node(node) {
                        Some(Node::ApplyCont(edge)) => {
                            for (index, atom) in edge.args.iter().enumerate() {
                                if shared(solver, atom) {
                                    solver.join(Key::Returned(function, index), Shared(true));
                                }
                            }
                        }
                        Some(Node::ApplyFun {
                            callee: Callee::Known(callee),
                            ..
                        }) => {
                            for index in 0..arities.get(&function).copied().unwrap_or(0) {
                                if solver
                                    .fact(&Key::Returned(*callee, index))
                                    .is_none_or(|fact| fact.0)
                                {
                                    solver.join(Key::Returned(function, index), Shared(true));
                                }
                            }
                        }
                        _ => {}
                    });
                }
            }
        });
        let shared = facts
            .into_iter()
            .filter_map(|(key, fact)| match (key, fact) {
                (Key::Value(value), Shared(true)) => Some(value),
                _ => None,
            })
            .collect();
        Facts {
            module,
            shape,
            borrowed,
            shared,
        }
    }

    /// The constructions of a row beside a unique value of that row, dead there and consumed on no path to them, rebuilt at most once on a path, and never where a constant may be the value and no tag test rules it out.
    fn rebuilds(&self, tags: &BTreeMap<ValueId, Tags>) -> Vec<Rebuild> {
        let (module, shape) = (self.module, self.shape);
        let constant = module.constant_data();
        let mut by_row = BTreeMap::<(FunctionId, RowId), Vec<ValueId>>::new();
        for (value, row) in &shape.read_at {
            if let (Some(row), Some(function)) = (row, shape.home.get(value))
                && !self.shared.contains(value)
            {
                by_row.entry((*function, *row)).or_default().push(*value);
            }
        }
        let mut chosen = BTreeMap::<ValueId, Vec<NodeId>>::new();
        let mut rebuilds = Vec::new();
        for (node_id, (block, _)) in &shape.place {
            let Some(Node::LetValue {
                result,
                value: ValueExpr::Row(row, atoms),
                ..
            }) = module.node(*node_id)
            else {
                continue;
            };
            if constant.contains(result) {
                continue;
            }
            let function = shape.owner[block];
            // Of the values that qualify, the one the construction keeps most of, so the rebuild writes the fewest slots.
            let found = by_row
                .get(&(function, *row))
                .into_iter()
                .flatten()
                .copied()
                .filter(|value| {
                    shape
                        .consumed(*value, self.borrowed)
                        .iter()
                        .all(|point| !shape.point_reaches(*point, *node_id))
                        && !shape.live_at(*node_id, *value)
                        && shape.in_scope(*value, *node_id)
                        && shape.read_as_row_before(*value, *row, *node_id)
                        && !shape.returns_without_rebinding(*value, *node_id)
                        && clear_of_constants(module, shape, tags, *value, *node_id)
                        && chosen.get(value).into_iter().flatten().all(|other| {
                            !shape.reaches(*other, *node_id) && !shape.reaches(*node_id, *other)
                        })
                })
                .map(|of| {
                    let tag = guard(module, shape, of, *node_id);
                    (of, written(module, shape, of, *row, atoms, tag))
                })
                .min_by_key(|(_, slots)| slots.len());
            if let Some((of, slots)) = found {
                chosen.entry(of).or_default().push(*node_id);
                rebuilds.push(Rebuild {
                    node: *node_id,
                    of,
                    row: *row,
                    slots,
                });
            }
        }
        rebuilds
    }
}

/// Whether no constant may be `value` at `node`: none reaches it, or a test of its tag around `node` rules out every constant's tag.
fn clear_of_constants(
    module: &Module,
    shape: &Shape,
    tags: &BTreeMap<ValueId, Tags>,
    value: ValueId,
    node: NodeId,
) -> bool {
    let Some(possible) = tags.get(&value) else {
        return true;
    };
    !possible.contains(&None)
        && guard(module, shape, value, node).is_some_and(|tag| !possible.contains(&Some(tag)))
}

/// Whether a call hands its parameter `index` a value dead after it, and no other parameter of the call that same value.
fn handed_dead(module: &Module, shape: &Shape, call: NodeId, index: usize) -> bool {
    let Some(Node::ApplyFun { args, .. }) = module.node(call) else {
        return false;
    };
    match args.get(index) {
        Some(atom @ Atom::Value(value)) => {
            args.iter().filter(|other| *other == atom).count() == 1
                && !shape.live_past(call, *value)
        }
        _ => false,
    }
}

/// Whether an argument a jump or an edge hands `block` is a value dead on entry to it and handed once on the edge.
fn arg_dead(shape: &Shape, atom: &Atom, args: &[Atom], block: Block) -> bool {
    match atom {
        Atom::Value(value) => {
            args.iter().filter(|other| *other == atom).count() == 1
                && !shape
                    .live_in
                    .get(&block)
                    .is_some_and(|live| live.contains(value))
        }
        _ => false,
    }
}

/// The slots a rebuild of `of` as the construction `atoms` writes: every slot but those holding what a read of `of` at that slot read, and, where a tag test says `of` was built with the tag the construction builds, the tag and the slots that constructor leaves unwritten.
fn written(
    module: &Module,
    shape: &Shape,
    of: ValueId,
    row: RowId,
    atoms: &[Atom],
    tag: Option<u32>,
) -> Vec<(usize, Atom)> {
    let same_constructor = matches!(
        (tag, atoms.first(), module.defined_row(row).and_then(|row| row.slots.first())),
        (Some(tag), Some(Atom::Literal(Literal::Nat(built))), Some(Slot::Tag)) if u32::try_from(built).ok() == Some(tag)
    );
    atoms
        .iter()
        .enumerate()
        .filter(|(index, atom)| {
            let read_back = match atom {
                Atom::Value(value) => shape.binder.get(value).is_some_and(|binder| {
                    matches!(module.node(*binder), Some(Node::LetIntrinsic { op: Intrinsic::RowGet(read, slot), args, .. })
                        if *read == row && slot == index && args.first() == Some(&Atom::Value(of)))
                }),
                _ => false,
            };
            let kept = same_constructor && (*index == 0 || matches!(atom, Atom::Filler));
            !read_back && !kept
        })
        .map(|(index, atom)| (index, atom.clone()))
        .collect()
}

/// The tag `value` holds at `node`, where a test of it decides it: the innermost continuation around `node` entered only by cases of switches on `value`'s tag, all of one key.
fn guard(module: &Module, shape: &Shape, value: ValueId, node: NodeId) -> Option<u32> {
    let mut current = node;
    loop {
        let (block, _) = shape.place[&current];
        let Block::Cont(continuation) = block else {
            return None;
        };
        let keys = shape.entries.get(&continuation).and_then(|entries| {
            entries
                .iter()
                .map(|entry| {
                    entry.case.and_then(|(switch, key)| {
                        tests_tag(module, shape, switch, value).then_some(key)
                    })
                })
                .collect::<Option<BTreeSet<_>>>()
        });
        if shape
            .resumes
            .get(&continuation)
            .is_none_or(|resumes| resumes.is_empty())
            && let Some(keys) = keys
            && keys.len() == 1
        {
            return keys.first().copied();
        }
        current = *shape.defined_at.get(&continuation)?;
    }
}

/// Whether a switch reads its case off `value`'s tag.
fn tests_tag(module: &Module, shape: &Shape, switch: NodeId, value: ValueId) -> bool {
    let Some(Node::Switch {
        scrutinee: Atom::Value(scrutinee),
        ..
    }) = module.node(switch)
    else {
        return false;
    };
    matches!(shape.defs.get(scrutinee), Some(Def::Field(of)) if *of == value)
        && shape.binder.get(scrutinee).is_some_and(|binder| {
            matches!(
                module.node(*binder),
                Some(Node::LetIntrinsic {
                    op: Intrinsic::RowGet(_, 0),
                    ..
                })
            )
        })
}

/// The constructor tags of the constant data that may reach a value, `None` standing for one with no literal tag.
type Tags = BTreeSet<Option<u32>>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Possible(Tags);

impl Lattice for Possible {
    fn bottom() -> Self {
        Possible(Tags::new())
    }

    fn join(&mut self, incoming: Self) {
        self.0.extend(incoming.0);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum TagKey {
    Value(ValueId),
    Slot(Option<RowId>, usize),
    Returned(FunctionId, usize),
}

/// The tags of the constant data that may reach each value, through the slots it is stored in, the calls and jumps that hand it on and the returns that hand it back.
fn constant_tags(module: &Module, shape: &Shape) -> BTreeMap<ValueId, Tags> {
    let constant = module.constant_data();
    let arities = module.return_arities();
    let keys = shape
        .defs
        .keys()
        .map(|value| TagKey::Value(*value))
        .collect::<Vec<_>>();
    let (constant, arities) = (&constant, &arities);
    let facts = Solver::solve(keys, |rules| {
        let possible = |solver: &Solver<Possible, TagKey>, atom: &Atom| match atom {
            Atom::Value(value) => solver
                .fact(&TagKey::Value(*value))
                .cloned()
                .unwrap_or(Possible::bottom()),
            _ => Possible::bottom(),
        };
        for node_id in shape.place.keys() {
            let node_id = *node_id;
            let Some(node) = module.node(node_id) else {
                continue;
            };
            match node {
                Node::LetValue { result, value, .. } => {
                    let result = *result;
                    let (row, atoms) = match value {
                        ValueExpr::Row(row, atoms) => (Some(*row), atoms.clone()),
                        ValueExpr::Tuple(atoms) => (None, atoms.clone()),
                        _ => continue,
                    };
                    if constant.contains(&result) {
                        let tag = match atoms.first() {
                            Some(Atom::Literal(Literal::Nat(tag))) if row.is_some() => {
                                u32::try_from(tag).ok()
                            }
                            _ => None,
                        };
                        rules.add(move |solver| {
                            solver.join(TagKey::Value(result), Possible(Tags::from([tag])))
                        });
                    }
                    rules.add(move |solver| {
                        for (index, atom) in atoms.iter().enumerate() {
                            let stored = possible(solver, atom);
                            solver.join(TagKey::Slot(row, index), stored);
                        }
                    });
                }
                Node::LetIntrinsic { result, op, .. } => {
                    let result = *result;
                    let slot = match op {
                        Intrinsic::RowGet(row, index) => TagKey::Slot(Some(*row), *index),
                        Intrinsic::TupleGet(index) => TagKey::Slot(None, *index),
                        _ => continue,
                    };
                    rules.add(move |solver| {
                        let read = solver.fact(&slot).cloned().unwrap_or(Possible::bottom());
                        solver.join(TagKey::Value(result), read);
                    });
                }
                Node::ApplyFun {
                    callee: Callee::Known(callee),
                    args,
                    return_to,
                } => {
                    let (callee, args, return_to) = (*callee, args.clone(), *return_to);
                    rules.add(move |solver| {
                        if let Some(function) = module.function(callee) {
                            for (param, atom) in function.params.iter().zip(&args) {
                                let handed = possible(solver, atom);
                                solver.join(TagKey::Value(*param), handed);
                            }
                        }
                        if let Some(continuation) = module.continuation(return_to) {
                            for (index, param) in continuation.params.iter().enumerate() {
                                let returned = solver
                                    .fact(&TagKey::Returned(callee, index))
                                    .cloned()
                                    .unwrap_or(Possible::bottom());
                                solver.join(TagKey::Value(*param), returned);
                            }
                        }
                    });
                }
                Node::ApplyCont(edge) => {
                    let edge = edge.clone();
                    let (block, _) = shape.place[&node_id];
                    let function = shape.owner[&block];
                    let own_return = module.function(function).unwrap().return_cont;
                    rules.add(move |solver| {
                        for (index, atom) in edge.args.iter().enumerate() {
                            let handed = possible(solver, atom);
                            match edge.target == own_return {
                                true => solver.join(TagKey::Returned(function, index), handed),
                                false => {
                                    if let Some(param) = module
                                        .continuation(edge.target)
                                        .and_then(|target| target.params.get(index))
                                    {
                                        solver.join(TagKey::Value(*param), handed);
                                    }
                                }
                            }
                        }
                    });
                }
                Node::Switch { cases, default, .. } => {
                    let edges = cases
                        .values()
                        .chain(default.as_ref())
                        .cloned()
                        .collect::<Vec<_>>();
                    rules.add(move |solver| {
                        for edge in &edges {
                            for (index, atom) in edge.args.iter().enumerate() {
                                let handed = possible(solver, atom);
                                if let Some(param) = module
                                    .continuation(edge.target)
                                    .and_then(|target| target.params.get(index))
                                {
                                    solver.join(TagKey::Value(*param), handed);
                                }
                            }
                        }
                    });
                }
                _ => {}
            }
        }
        // A tail call hands back what its callee does.
        for (function, nodes) in &shape.returns {
            let function = *function;
            for node in nodes {
                if let Some(Node::ApplyFun {
                    callee: Callee::Known(callee),
                    ..
                }) = module.node(*node)
                {
                    let callee = *callee;
                    rules.add(move |solver| {
                        for index in 0..arities.get(&function).copied().unwrap_or(0) {
                            let returned = solver
                                .fact(&TagKey::Returned(callee, index))
                                .cloned()
                                .unwrap_or(Possible::bottom());
                            solver.join(TagKey::Returned(function, index), returned);
                        }
                    });
                }
            }
        }
    });
    facts
        .into_iter()
        .filter_map(|(key, Possible(tags))| match key {
            TagKey::Value(value) if !tags.is_empty() => Some((value, tags)),
            _ => None,
        })
        .collect()
}
