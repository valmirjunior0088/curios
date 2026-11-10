//! Placement: a computation the program may drop where nothing reads it runs only where something does.
//!
//! **The licence is the dead call's.** A construction, an intrinsic, a group of functions that carries one and a call of a [`Function::droppable`](crate::Function::droppable) function are each work a path that never reads it need not do, by the rule [`eliminate_dead_calls`](super::eliminate_dead_calls) already applies to a call no path reads. A trap is not an event that keeps such work in place; an effect, a divergence and a mutable allocation are, and none of these nodes has one.
//!
//! **A binding's scope is its dominance region** (`cse.rs` states why), so the latest point every reader still follows is the innermost continuation holding them all: no dominator tree is built, and the work moves there, at the head of that continuation's body, where some run that does the work leaves the function without entering it. Where every run enters it, the work is read on every path and stays: moving it would save nothing, and would take a continuation that only hands values on away from the forwarding that threads its jumps through it. A reader inside a function defined below the work reads it where that function is bound, since a closure may run any number of times; and a continuation on a cycle that does not pass through the work's own continuation is never a destination, since the work would run once a turn of a loop it ran once before.
//!
//! **Two readers are a branch of their own.** The second operand of an operation whose first can decide it ([`Intrinsic::decided_by_first`]) is read only on the first's other value, and an argument of one switch edge only on that edge, so work whose one reader is either is moved into a continuation made for that case: a branch on the first operand, or a landing on the edge. Neither is made unless work moves into it, so an operation over values already computed keeps its one instruction.
//!
//! **Work that may trap crosses no effect.** It moves only along transfers no call stands on — switch edges and jumps from the chain holding each continuation it enters — so it runs after no effect it ran before; a construction, a total intrinsic or a group of functions cannot fail, and moves past anything.
//!
//! **A moved call returns where its result went on.** Moved to a continuation whose body only hands the call's results on, the call returns to where they are handed, so a call into a branch that returns its answer is a tail call, and a recursion closed by a conjunction a loop that ends at the first operand that decides it.

#[cfg(test)]
mod tests;

use {
    crate::{
        Atom, Callee, Continuation, ContinuationId, Edge, FunctionId, Intrinsic, Literal, Module,
        Node, NodeId, ValueExpr, ValueId, atoms, collect_control_targets,
    },
    curios_num::Natural,
    std::collections::{BTreeMap, BTreeSet, btree_map::Entry},
};

/// Move every droppable computation to where it is read, answering whether anything moved.
pub fn sink_computations(module: &mut Module) -> bool {
    let mut pass = Sink::new(module);
    let functions = pass
        .module
        .live_functions()
        .map(|(id, _)| id)
        .collect::<Vec<_>>();
    for function in functions {
        pass.place_in(function);
    }
    pass.changed
}

/// What holds a node, a continuation or a function in the lexical tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Holder {
    Node(NodeId),
    Continuation(ContinuationId),
    Function(FunctionId),
}

/// A computation that may move, read off the node computing it.
enum Work {
    /// A construction, an intrinsic or a group of functions, bound by `node`.
    Binding { node: NodeId, trapping: bool },
    /// A droppable call at `node`, whose results arrive in `resume`, which nothing else enters.
    Call {
        node: NodeId,
        resume: ContinuationId,
    },
}

/// Where a computation's readers place it.
enum Destination {
    /// The innermost continuation holding every reader, and the continuations between the computation and it, outermost first.
    Into(Vec<ContinuationId>),
    /// The one reader is the second operand of the operation at the node, which the given value of its first decides; the continuations between, outermost first.
    Decided(NodeId, u32, Vec<ContinuationId>),
    /// The one reader is an argument of the switch at the node, on the edge of the given case, or the default.
    Edge(NodeId, Option<u32>, Vec<ContinuationId>),
}

/// The pass: the module, a census it keeps exact through its own moves, and the cycles of the function it is placing in.
struct Sink<'m> {
    module: &'m mut Module,
    /// What each node follows, or the continuation or function whose body it is.
    parent: BTreeMap<NodeId, Holder>,
    /// The `LetCont` binding each continuation.
    held: BTreeMap<ContinuationId, NodeId>,
    /// The `LetFun` binding each function defined inside another.
    bound: BTreeMap<FunctionId, NodeId>,
    /// What binds each value: the node computing it, or the continuation or function whose parameter it is.
    binder: BTreeMap<ValueId, Holder>,
    /// The nodes naming each value, a closure call's callee among them.
    uses: BTreeMap<ValueId, BTreeSet<NodeId>>,
    /// The nodes naming or calling each function.
    references: BTreeMap<FunctionId, BTreeSet<NodeId>>,
    /// The nodes transferring to each continuation.
    transfers: BTreeMap<ContinuationId, BTreeSet<NodeId>>,
    /// The strongly connected component of each continuation of the function being placed in, over the transfers between them.
    component: BTreeMap<ContinuationId, usize>,
    /// The components holding a cycle.
    cyclic: BTreeSet<usize>,
    /// The next component a continuation the pass makes outside every cycle takes.
    fresh: usize,
    /// The `LetFun`s of the function being placed in whose members read a value another computation there binds: the groups whose place is that value's to follow.
    carriers: BTreeSet<NodeId>,
    changed: bool,
}

impl<'m> Sink<'m> {
    fn new(module: &'m mut Module) -> Self {
        let mut pass = Self {
            module,
            parent: BTreeMap::new(),
            held: BTreeMap::new(),
            bound: BTreeMap::new(),
            binder: BTreeMap::new(),
            uses: BTreeMap::new(),
            references: BTreeMap::new(),
            transfers: BTreeMap::new(),
            component: BTreeMap::new(),
            cyclic: BTreeSet::new(),
            fresh: 0,
            carriers: BTreeSet::new(),
            changed: false,
        };
        let nodes = pass
            .module
            .live_nodes()
            .map(|(id, _)| id)
            .collect::<Vec<_>>();
        for id in nodes {
            pass.file(id);
        }
        let continuations = pass
            .module
            .live_continuations()
            .map(|(id, definition)| (id, definition.body, definition.params.clone()))
            .collect::<Vec<_>>();
        for (id, body, params) in continuations {
            pass.parent.insert(body, Holder::Continuation(id));
            for param in params {
                pass.binder.insert(param, Holder::Continuation(id));
            }
        }
        let functions = pass
            .module
            .live_functions()
            .map(|(id, definition)| (id, definition.body, definition.params.clone()))
            .collect::<Vec<_>>();
        for (id, body, params) in functions {
            pass.parent.insert(body, Holder::Function(id));
            for param in params {
                pass.binder.insert(param, Holder::Function(id));
            }
        }
        pass
    }

    /// File what the node at `id` names, transfers to, binds and is followed by.
    fn file(&mut self, id: NodeId) {
        let Some(node) = self.module.node(id) else {
            return;
        };
        let mut targets = BTreeSet::new();
        collect_control_targets(node, &mut targets);
        let mut named_values = Vec::new();
        let mut named_functions = Vec::new();
        for atom in atoms(node) {
            match atom {
                Atom::Value(value) => named_values.push(*value),
                Atom::Fun(function) => named_functions.push(*function),
                Atom::Literal(_) | Atom::Filler => {}
            }
        }
        match node {
            Node::ApplyFun {
                callee: Callee::Closure(value),
                ..
            } => named_values.push(*value),
            Node::ApplyFun {
                callee: Callee::Known(function),
                ..
            } => named_functions.push(*function),
            _ => {}
        }
        let (child, bound, held, defines) = match node {
            Node::LetValue { result, next, .. } | Node::LetIntrinsic { result, next, .. } => {
                (Some(*next), Vec::new(), Vec::new(), Some(*result))
            }
            Node::LetFun { functions, body } => (Some(*body), functions.clone(), Vec::new(), None),
            Node::LetCont {
                continuations,
                body,
            } => (Some(*body), Vec::new(), continuations.clone(), None),
            _ => (None, Vec::new(), Vec::new(), None),
        };
        for value in named_values {
            self.uses.entry(value).or_default().insert(id);
        }
        for function in named_functions {
            self.references.entry(function).or_default().insert(id);
        }
        for target in targets {
            self.transfers.entry(target).or_default().insert(id);
        }
        if let Some(child) = child {
            self.parent.insert(child, Holder::Node(id));
        }
        for function in bound {
            self.bound.insert(function, id);
        }
        for continuation in held {
            self.held.insert(continuation, id);
        }
        if let Some(value) = defines {
            self.binder.insert(value, Holder::Node(id));
        }
    }

    /// Withdraw what the node at `id` names and transfers to, before it is rewritten or dropped.
    fn unfile(&mut self, id: NodeId) {
        let Some(node) = self.module.node(id) else {
            return;
        };
        let mut targets = BTreeSet::new();
        collect_control_targets(node, &mut targets);
        let mut values = Vec::new();
        let mut functions = Vec::new();
        for atom in atoms(node) {
            match atom {
                Atom::Value(value) => values.push(*value),
                Atom::Fun(function) => functions.push(*function),
                Atom::Literal(_) | Atom::Filler => {}
            }
        }
        match node {
            Node::ApplyFun {
                callee: Callee::Closure(value),
                ..
            } => values.push(*value),
            Node::ApplyFun {
                callee: Callee::Known(function),
                ..
            } => functions.push(*function),
            _ => {}
        }
        for value in values {
            if let Some(sites) = self.uses.get_mut(&value) {
                sites.remove(&id);
            }
        }
        for function in functions {
            if let Some(sites) = self.references.get_mut(&function) {
                sites.remove(&id);
            }
        }
        for target in targets {
            if let Some(sources) = self.transfers.get_mut(&target) {
                sources.remove(&id);
            }
        }
    }

    /// Replace the node at `id`, keeping the census exact.
    fn set(&mut self, id: NodeId, node: Node) {
        self.unfile(id);
        *self.module.node_mut(id).expect("a node set is live") = node;
        self.file(id);
    }

    /// Add `node`, filed.
    fn add(&mut self, node: Node) -> NodeId {
        let id = self.module.add_node(node);
        self.file(id);
        id
    }

    /// Point `holder` at `child` as what follows it, or as its body.
    fn link(&mut self, holder: Holder, child: NodeId) {
        match holder {
            Holder::Node(parent) => match self.module.node_mut(parent) {
                Some(Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. }) => {
                    *next = child
                }
                Some(Node::LetFun { body, .. } | Node::LetCont { body, .. }) => *body = child,
                _ => unreachable!("a holding node has one successor"),
            },
            Holder::Continuation(continuation) => {
                self.module
                    .continuation_mut(continuation)
                    .expect("a holding continuation is live")
                    .body = child;
            }
            Holder::Function(function) => {
                self.module
                    .function_mut(function)
                    .expect("a holding function is live")
                    .body = child;
            }
        }
        self.parent.insert(child, holder);
    }

    /// What holds `holder` in turn: a node what it follows, a continuation its `LetCont`, a function its `LetFun`; nothing above a function defined at the top.
    fn above(&self, holder: Holder) -> Option<Holder> {
        match holder {
            Holder::Node(node) => self.parent.get(&node).copied(),
            Holder::Continuation(continuation) => {
                self.held.get(&continuation).map(|&node| Holder::Node(node))
            }
            Holder::Function(function) => self.bound.get(&function).map(|&node| Holder::Node(node)),
        }
    }

    /// Whether `ancestor` holds `holder`, reading up through functions too.
    fn holds(&self, ancestor: Holder, holder: Holder) -> bool {
        let mut at = Some(holder);
        while let Some(current) = at {
            if current == ancestor {
                return true;
            }
            at = self.above(current);
        }
        false
    }

    /// The continuation or function whose body chain holds the node at `node`.
    fn container(&self, node: NodeId) -> Option<Holder> {
        let mut at = Holder::Node(node);
        loop {
            match self.above(at)? {
                Holder::Node(parent) => at = Holder::Node(parent),
                holder => return Some(holder),
            }
        }
    }

    /// Where the node at `node` reads within `function`'s own body: itself, or the `LetFun` binding the outermost function defined in that body that holds it.
    fn site_in(&self, node: NodeId, function: FunctionId) -> Option<NodeId> {
        let mut site = node;
        let mut at = Holder::Node(node);
        loop {
            match at {
                Holder::Function(owner) if owner == function => return Some(site),
                Holder::Function(owner) => {
                    site = *self.bound.get(&owner)?;
                    at = Holder::Node(site);
                }
                holder => at = self.above(holder)?,
            }
        }
    }

    /// The continuations holding `site` below `region`, outermost first; `None` where `region` does not hold it within one function.
    fn between(&self, region: Holder, site: NodeId) -> Option<Vec<ContinuationId>> {
        let mut found = Vec::new();
        let mut at = Holder::Node(site);
        while at != region {
            match at {
                Holder::Continuation(continuation) => found.push(continuation),
                Holder::Function(_) => return None,
                Holder::Node(_) => {}
            }
            at = self.above(at)?;
        }
        found.reverse();
        Some(found)
    }

    /// The node ending the chain that begins at `node`.
    fn terminator(&self, mut node: NodeId) -> NodeId {
        loop {
            match self.module.node(node) {
                Some(
                    Node::LetValue { next, .. }
                    | Node::LetIntrinsic { next, .. }
                    | Node::LetFun { body: next, .. }
                    | Node::LetCont { body: next, .. },
                ) => node = *next,
                _ => return node,
            }
        }
    }

    /// The continuations the chain of `continuation`'s body transfers to.
    fn successors(&self, continuation: ContinuationId) -> BTreeSet<ContinuationId> {
        let mut targets = BTreeSet::new();
        if let Some(definition) = self.module.continuation(continuation)
            && let Some(node) = self.module.node(self.terminator(definition.body))
        {
            collect_control_targets(node, &mut targets);
        }
        targets
    }

    /// Place every computation in `function`'s own body, the last first, so that what a computation feeds has moved before it is placed.
    fn place_in(&mut self, function: FunctionId) {
        let Some(body) = self
            .module
            .function(function)
            .map(|definition| definition.body)
        else {
            return;
        };
        let mut order = Vec::new();
        let mut continuations = Vec::new();
        let mut work = vec![body];
        while let Some(id) = work.pop() {
            order.push(id);
            match self.module.node(id) {
                Some(Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. }) => {
                    work.push(*next)
                }
                Some(Node::LetFun { body, .. }) => work.push(*body),
                Some(Node::LetCont {
                    continuations: members,
                    body,
                }) => {
                    for member in members {
                        if let Some(definition) = self.module.continuation(*member) {
                            continuations.push(*member);
                            work.push(definition.body);
                        }
                    }
                    work.push(*body);
                }
                _ => {}
            }
        }
        self.components(&continuations);
        // A group's definition emits nothing, a closure being made where a member is named, so a group moves only to carry what its members read: a value a computation of this body binds, which cannot move below the group without it.
        self.carriers.clear();
        for id in &order {
            if let Some(Node::LetValue { result, .. } | Node::LetIntrinsic { result, .. }) =
                self.module.node(*id)
            {
                let readers = self.uses.get(result).cloned().unwrap_or_default();
                for reader in readers {
                    if let Some(site) = self.site_in(reader, function)
                        && site != reader
                    {
                        self.carriers.insert(site);
                    }
                }
            }
        }
        for id in order.into_iter().rev() {
            if let Some(work) = self.work(id) {
                self.place(function, work);
            }
        }
    }

    /// The strongly connected components of `continuations` over the transfers between them, and which hold a cycle.
    fn components(&mut self, continuations: &[ContinuationId]) {
        self.component.clear();
        self.cyclic.clear();
        let members = continuations.iter().copied().collect::<BTreeSet<_>>();
        let graph = continuations
            .iter()
            .map(|&continuation| {
                let targets = self
                    .successors(continuation)
                    .into_iter()
                    .filter(|target| members.contains(target))
                    .collect::<Vec<_>>();
                (continuation, targets)
            })
            .collect::<BTreeMap<_, _>>();

        // Tarjan's algorithm, iterative so that a deep nest of continuations does not reach the native stack.
        let mut index = BTreeMap::new();
        let mut low = BTreeMap::new();
        let mut stack = Vec::new();
        let mut on_stack = BTreeSet::new();
        let mut next_index = 0;
        let mut next_component = 0;
        for &start in continuations {
            if index.contains_key(&start) {
                continue;
            }
            let mut frames = vec![(start, 0usize)];
            index.insert(start, next_index);
            low.insert(start, next_index);
            next_index += 1;
            stack.push(start);
            on_stack.insert(start);
            while let Some(&(node, edge)) = frames.last() {
                let targets = &graph[&node];
                if edge < targets.len() {
                    let target = targets[edge];
                    if let Some(frame) = frames.last_mut() {
                        frame.1 += 1;
                    }
                    if let Entry::Vacant(slot) = index.entry(target) {
                        slot.insert(next_index);
                        low.insert(target, next_index);
                        next_index += 1;
                        stack.push(target);
                        on_stack.insert(target);
                        frames.push((target, 0));
                    } else if on_stack.contains(&target) {
                        let lowest = low[&node].min(index[&target]);
                        low.insert(node, lowest);
                    }
                    continue;
                }
                frames.pop();
                if let Some(&(caller, _)) = frames.last() {
                    let lowest = low[&caller].min(low[&node]);
                    low.insert(caller, lowest);
                }
                if low[&node] == index[&node] {
                    let mut size = 0;
                    loop {
                        let member = stack.pop().expect("a component's members are stacked");
                        on_stack.remove(&member);
                        self.component.insert(member, next_component);
                        size += 1;
                        if member == node {
                            break;
                        }
                    }
                    if size > 1 || graph[&node].contains(&node) {
                        self.cyclic.insert(next_component);
                    }
                    next_component += 1;
                }
            }
        }
        self.fresh = next_component;
    }

    /// Give a continuation the pass made the component of the continuation or function whose chain it was carved from: it lies on exactly the cycles that one does.
    fn carved(&mut self, continuation: ContinuationId, from: Option<Holder>) {
        let component = match from {
            Some(Holder::Continuation(origin)) => self.component.get(&origin).copied(),
            _ => None,
        };
        let component = component.unwrap_or_else(|| {
            self.fresh += 1;
            self.fresh - 1
        });
        self.component.insert(continuation, component);
    }

    /// Whether entering `target` may repeat between two runs of `origin`: whether `target` lies on a cycle that does not pass through it.
    fn repeats(&self, target: ContinuationId, origin: Holder) -> bool {
        let Some(&component) = self.component.get(&target) else {
            return true;
        };
        if !self.cyclic.contains(&component) {
            return false;
        }
        let Holder::Continuation(origin) = origin else {
            return true;
        };
        if self.component.get(&origin) != Some(&component) {
            return true;
        }
        let mut seen = BTreeSet::new();
        let mut work = self.successors(target).into_iter().collect::<Vec<_>>();
        while let Some(continuation) = work.pop() {
            if continuation == target {
                return true;
            }
            if continuation == origin
                || self.component.get(&continuation) != Some(&component)
                || !seen.insert(continuation)
            {
                continue;
            }
            work.extend(self.successors(continuation));
        }
        false
    }

    /// Whether some path from the chain holding `from` leaves the function without entering `target`: a return, a tail call or a halt reached around it. Where none does, every run that does the work enters `target`, and moving the work there saves nothing. An arm the theory proved impossible and a refusal are no way a run goes on, so a path ending in one saves no run anything.
    fn avoidable(&self, from: NodeId, target: ContinuationId) -> bool {
        let mut seen = BTreeSet::new();
        let mut work = vec![self.terminator(from)];
        while let Some(terminator) = work.pop() {
            let mut targets = BTreeSet::new();
            match self.module.node(terminator) {
                Some(Node::Halt { .. }) => return true,
                Some(node) => collect_control_targets(node, &mut targets),
                None => {}
            }
            for next in targets {
                if next == target || !seen.insert(next) {
                    continue;
                }
                match self.module.continuation(next) {
                    // A target with no definition is a function's return.
                    None => return true,
                    Some(definition) => work.push(self.terminator(definition.body)),
                }
            }
        }
        false
    }

    /// Whether every continuation of `path` is entered only by switch edges and jumps from the chain holding it, so that work moved along it crosses no call.
    fn pure_path(&self, path: &[ContinuationId]) -> bool {
        path.iter().all(|continuation| {
            let Some(&binder) = self.held.get(continuation) else {
                return false;
            };
            let holding = self.container(binder);
            self.transfers.get(continuation).is_some_and(|sources| {
                sources.iter().all(|source| {
                    matches!(
                        self.module.node(*source),
                        Some(Node::Switch { .. } | Node::ApplyCont(_))
                    ) && self.container(*source) == holding
                })
            })
        })
    }

    /// The work the node at `id` computes, where it may move.
    fn work(&self, id: NodeId) -> Option<Work> {
        match self.module.node(id)? {
            Node::LetValue {
                value: ValueExpr::List(_) | ValueExpr::Tuple(_) | ValueExpr::Row(..),
                ..
            } => Some(Work::Binding {
                node: id,
                trapping: false,
            }),
            Node::LetFun { .. } if self.carriers.contains(&id) => Some(Work::Binding {
                node: id,
                trapping: false,
            }),
            // The window guard's trap is kept at the slice's own evaluation point, which is its whole contract.
            Node::LetIntrinsic { op, .. } if *op != Intrinsic::WindowExtent => {
                Some(Work::Binding {
                    node: id,
                    trapping: op.may_trap(),
                })
            }
            Node::ApplyFun {
                callee: Callee::Known(callee),
                return_to,
                ..
            } => {
                let droppable = self
                    .module
                    .function(*callee)
                    .is_some_and(|function| function.droppable);
                let resume = self.module.continuation(*return_to)?;
                let entered_once = self
                    .transfers
                    .get(return_to)
                    .is_some_and(|sources| sources.len() == 1);
                (droppable
                    && entered_once
                    && !resume.params.is_empty()
                    && !self.module.field_groups().contains_key(return_to))
                .then_some(Work::Call {
                    node: id,
                    resume: *return_to,
                })
            }
            _ => None,
        }
    }

    /// The nodes reading what `work` computes.
    fn readers(&self, work: &Work) -> BTreeSet<NodeId> {
        match work {
            Work::Binding { node, .. } => match self.module.node(*node) {
                Some(Node::LetValue { result, .. } | Node::LetIntrinsic { result, .. }) => {
                    self.uses.get(result).cloned().unwrap_or_default()
                }
                Some(Node::LetFun { functions, .. }) => functions
                    .iter()
                    .flat_map(|function| self.references.get(function).into_iter().flatten())
                    .copied()
                    .collect(),
                _ => BTreeSet::new(),
            },
            Work::Call { resume, .. } => self
                .module
                .continuation(*resume)
                .map(|definition| {
                    definition
                        .params
                        .iter()
                        .flat_map(|param| self.uses.get(param).into_iter().flatten())
                        .copied()
                        .collect()
                })
                .unwrap_or_default(),
        }
    }

    /// The one value `work` computes, where it computes one.
    fn single_value(&self, work: &Work) -> Option<ValueId> {
        match work {
            Work::Binding { node, .. } => match self.module.node(*node)? {
                Node::LetValue { result, .. } | Node::LetIntrinsic { result, .. } => Some(*result),
                _ => None,
            },
            Work::Call { resume, .. } => match self.module.continuation(*resume)?.params.as_slice()
            {
                [param] => Some(*param),
                _ => None,
            },
        }
    }

    /// Where `work`'s readers place it within `function`, or nowhere it may move.
    fn destination(
        &self,
        function: FunctionId,
        work: &Work,
        region: Holder,
    ) -> Option<Destination> {
        let readers = self.readers(work);
        let own = match work {
            Work::Binding { node, .. } => Some(*node),
            Work::Call { .. } => None,
        };
        let mut sites = BTreeSet::new();
        for reader in &readers {
            let site = self.site_in(*reader, function)?;
            // A function of a group naming its own group reads it from inside the work itself.
            if Some(site) != own {
                sites.insert(site);
            }
        }
        let listed = sites.iter().copied().collect::<Vec<_>>();
        if let ([site], Some(value)) = (listed.as_slice(), self.single_value(work))
            && readers.len() == 1
            && readers.contains(site)
        {
            let path = self.between(region, *site)?;
            match self.module.node(*site)? {
                Node::LetIntrinsic { op, args, .. } => {
                    if let (Some(decided), [first, second]) =
                        (op.decided_by_first(), args.as_slice())
                        && *second == Atom::Value(value)
                        && *first != Atom::Value(value)
                    {
                        return Some(Destination::Decided(*site, decided, path));
                    }
                }
                Node::Switch {
                    scrutinee,
                    cases,
                    default,
                } if *scrutinee != Atom::Value(value) => {
                    let names = |edge: &Edge| edge.args.contains(&Atom::Value(value));
                    let mut naming = cases
                        .iter()
                        .filter(|(_, edge)| names(edge))
                        .map(|(case, _)| Some(*case))
                        .chain(default.iter().filter(|edge| names(edge)).map(|_| None));
                    if let (Some(case), None) = (naming.next(), naming.next()) {
                        return Some(Destination::Edge(*site, case, path));
                    }
                }
                _ => {}
            }
        }
        let mut common: Option<Vec<ContinuationId>> = None;
        for site in &sites {
            let path = self.between(region, *site)?;
            common = Some(match common {
                None => path,
                Some(common) => common
                    .into_iter()
                    .zip(path)
                    .take_while(|(a, b)| a == b)
                    .map(|(a, _)| a)
                    .collect(),
            });
        }
        let path = common?;
        (!path.is_empty()).then_some(Destination::Into(path))
    }

    /// Move `work` to where its readers place it, where that is allowed.
    fn place(&mut self, function: FunctionId, work: Work) {
        let (region, origin, trapping) = match &work {
            Work::Binding { node, trapping } => {
                let Some(origin) = self.container(*node) else {
                    return;
                };
                (Holder::Node(*node), origin, *trapping)
            }
            Work::Call { node, resume } => {
                let Some(origin) = self.container(*node) else {
                    return;
                };
                (Holder::Continuation(*resume), origin, true)
            }
        };
        let Some(destination) = self.destination(function, &work, region) else {
            return;
        };
        let (path, innermost) = match &destination {
            Destination::Into(path) => (path, path.last().copied()),
            Destination::Decided(_, _, path) | Destination::Edge(_, _, path) => {
                (path, path.last().copied())
            }
        };
        if innermost.is_some_and(|target| self.repeats(target, origin)) {
            return;
        }
        if trapping && !self.pure_path(path) {
            return;
        }
        // A branch and a landing are entered on one outcome alone; a continuation is worth moving into only where some run that does the work does not reach it.
        if let Destination::Into(path) = &destination {
            let from = match &work {
                Work::Binding { node, .. } => *node,
                Work::Call { resume, .. } => match self.module.continuation(*resume) {
                    Some(definition) => definition.body,
                    None => return,
                },
            };
            if !self.avoidable(from, *path.last().expect("a destination is a continuation")) {
                return;
            }
        }
        // Read before a branch or a landing is made, so that nothing is made for work that stays: one made at a reader holds what holds the reader.
        let lands_at = match &destination {
            Destination::Into(path) => {
                Holder::Continuation(*path.last().expect("a destination is a continuation"))
            }
            Destination::Decided(site, ..) | Destination::Edge(site, ..) => Holder::Node(*site),
        };
        if !self.in_scope_at(&work, lands_at) {
            return;
        }
        let target = match destination {
            Destination::Into(path) => *path.last().expect("a destination is a continuation"),
            Destination::Decided(site, decided, _) => self.branch(site, decided),
            Destination::Edge(site, case, _) => self.land(site, case),
        };
        match work {
            Work::Binding { node, .. } => self.move_binding(node, target),
            Work::Call { node, resume } => self.move_call(node, resume, target),
        }
        self.changed = true;
    }

    /// Whether what `work` names is in scope at `place`: always for a binding, which moves only below itself, and for a call each operand's binder and a function defined in another holding `place`.
    fn in_scope_at(&self, work: &Work, place: Holder) -> bool {
        let Work::Call { node, .. } = work else {
            return true;
        };
        let Some(Node::ApplyFun { callee, args, .. }) = self.module.node(*node) else {
            return false;
        };
        let function_in_scope = |function: &FunctionId| {
            self.bound
                .get(function)
                .is_none_or(|binding| self.holds(Holder::Node(*binding), place))
        };
        let callee_in_scope = match callee {
            Callee::Known(function) => function_in_scope(function),
            Callee::Closure(_) => false,
        };
        callee_in_scope
            && args.iter().all(|arg| match arg {
                Atom::Value(value) => self
                    .binder
                    .get(value)
                    .is_some_and(|binder| self.holds(*binder, place)),
                Atom::Fun(function) => function_in_scope(function),
                Atom::Literal(_) | Atom::Filler => true,
            })
    }

    /// Turn the operation at `site`, whose second operand is about to move, into a branch on its first: the value that decides it goes straight on, and the other enters a continuation, made here, that hands the second operand on. Answer that continuation.
    fn branch(&mut self, site: NodeId, decided: u32) -> ContinuationId {
        let from = self.container(site);
        let Some(Node::LetIntrinsic {
            result, args, next, ..
        }) = self.module.node(site).cloned()
        else {
            unreachable!("a decided operation is an intrinsic binding");
        };
        let [first, second] =
            <[Atom; 2]>::try_from(args).expect("a decided operation takes two operands");
        // Where the operation's result went on: straight to where `next` hands it, or a join made for the rest of the chain.
        let (join, made) = match self.forwarded(next, &[result]) {
            Some(target) => {
                self.unfile(next);
                self.module.remove_node(next);
                self.parent.remove(&next);
                self.module.remove_value(result);
                self.binder.remove(&result);
                (target, false)
            }
            None => {
                let join = self.module.add_continuation(Continuation {
                    debug_name: Some("join".into()),
                    params: vec![result],
                    body: next,
                });
                self.parent.insert(next, Holder::Continuation(join));
                self.binder.insert(result, Holder::Continuation(join));
                self.carved(join, from);
                (join, true)
            }
        };
        let handed = self.add(Node::ApplyCont(Edge {
            target: join,
            args: vec![second],
        }));
        let arm = self.module.add_continuation(Continuation {
            debug_name: Some("arm".into()),
            params: Vec::new(),
            body: handed,
        });
        self.parent.insert(handed, Holder::Continuation(arm));
        self.carved(arm, from);
        let decided_value = Edge {
            target: join,
            args: vec![Atom::Literal(Literal::Nat(Natural::from(decided)))],
        };
        // A switch edge enters a continuation and never returns, so a result the operation handed straight to the function's return reaches it from the decided edge through a continuation of its own.
        let mut members = if made { vec![join, arm] } else { vec![arm] };
        let decided_edge = match self.module.continuation(join) {
            Some(_) => decided_value,
            None => {
                let returned = self.add(Node::ApplyCont(decided_value));
                let settled = self.module.add_continuation(Continuation {
                    debug_name: Some("arm".into()),
                    params: Vec::new(),
                    body: returned,
                });
                self.parent.insert(returned, Holder::Continuation(settled));
                self.carved(settled, from);
                members.push(settled);
                Edge {
                    target: settled,
                    args: Vec::new(),
                }
            }
        };
        let other_edge = Edge {
            target: arm,
            args: Vec::new(),
        };
        let cases = BTreeMap::from([(decided, decided_edge), (1 - decided, other_edge)]);
        let switch = self.add(Node::Switch {
            scrutinee: first,
            cases,
            default: None,
        });
        self.set(
            site,
            Node::LetCont {
                continuations: members,
                body: switch,
            },
        );
        arm
    }

    /// Give the edge of the switch at `site` for `case`, or its default, a landing of its own that takes the edge on, made here, and answer it.
    fn land(&mut self, site: NodeId, case: Option<u32>) -> ContinuationId {
        let from = self.container(site);
        let Some(Node::Switch {
            scrutinee,
            mut cases,
            mut default,
        }) = self.module.node(site).cloned()
        else {
            unreachable!("a landing is made on a switch");
        };
        let slot = match case {
            Some(case) => cases.get_mut(&case),
            None => default.as_mut(),
        }
        .expect("the edge named is the switch's");
        let landing = self.module.reserve_continuation();
        let edge = std::mem::replace(
            slot,
            Edge {
                target: landing,
                args: Vec::new(),
            },
        );
        let handed = self.add(Node::ApplyCont(edge));
        self.module.define_continuation(
            landing,
            Continuation {
                debug_name: Some("arm".into()),
                params: Vec::new(),
                body: handed,
            },
        );
        self.parent.insert(handed, Holder::Continuation(landing));
        self.carved(landing, from);
        let holder = *self.parent.get(&site).expect("a switch is held");
        self.set(
            site,
            Node::Switch {
                scrutinee,
                cases,
                default,
            },
        );
        let hold = self.add(Node::LetCont {
            continuations: vec![landing],
            body: site,
        });
        self.link(holder, hold);
        landing
    }

    /// Where the node at `node` hands `values` on unchanged, where that is all it does: the continuation its jump enters with exactly those arguments, which then has no field groups to keep.
    fn forwarded(&self, node: NodeId, values: &[ValueId]) -> Option<ContinuationId> {
        let Some(Node::ApplyCont(Edge { target, args })) = self.module.node(node) else {
            return None;
        };
        let forwards = args.len() == values.len()
            && args
                .iter()
                .zip(values)
                .all(|(arg, value)| *arg == Atom::Value(*value));
        let named_elsewhere = values.iter().any(|value| {
            self.uses
                .get(value)
                .is_some_and(|sites| sites.iter().any(|site| *site != node))
        });
        (forwards && !named_elsewhere && !self.module.field_groups().contains_key(target))
            .then_some(*target)
    }

    /// Move the binding at `node` to the head of `target`'s body.
    fn move_binding(&mut self, node: NodeId, target: ContinuationId) {
        let holder = *self.parent.get(&node).expect("a binding is held");
        let next = match self.module.node(node) {
            Some(
                Node::LetValue { next, .. }
                | Node::LetIntrinsic { next, .. }
                | Node::LetFun { body: next, .. },
            ) => *next,
            _ => unreachable!("only a binding moves"),
        };
        self.link(holder, next);
        let body = self
            .module
            .continuation(target)
            .expect("a destination is live")
            .body;
        self.link(Holder::Node(node), body);
        self.link(Holder::Continuation(target), node);
    }

    /// Move the call at `node` to the head of `target`'s body, its results arriving where that body hands them on or in a continuation made there: where it stood, a jump enters `resume`, which no longer binds them.
    fn move_call(&mut self, node: NodeId, resume: ContinuationId, target: ContinuationId) {
        let holder = *self.parent.get(&node).expect("a call is held");
        let results = std::mem::take(
            &mut self
                .module
                .continuation_mut(resume)
                .expect("a resume is live")
                .params,
        );
        let enter = self.add(Node::ApplyCont(Edge {
            target: resume,
            args: Vec::new(),
        }));
        self.link(holder, enter);
        let body = self
            .module
            .continuation(target)
            .expect("a destination is live")
            .body;
        let returns_to = match self.forwarded(body, &results) {
            Some(onward) => {
                self.unfile(body);
                self.module.remove_node(body);
                self.parent.remove(&body);
                for result in &results {
                    self.module.remove_value(*result);
                    self.binder.remove(result);
                }
                self.link(Holder::Continuation(target), node);
                onward
            }
            None => {
                let debug_name = self
                    .module
                    .continuation(resume)
                    .and_then(|definition| definition.debug_name.clone());
                let landed = self.module.add_continuation(Continuation {
                    debug_name,
                    params: results.clone(),
                    body,
                });
                self.parent.insert(body, Holder::Continuation(landed));
                for result in &results {
                    self.binder.insert(*result, Holder::Continuation(landed));
                }
                self.carved(landed, Some(Holder::Continuation(target)));
                let hold = self.add(Node::LetCont {
                    continuations: vec![landed],
                    body: node,
                });
                self.link(Holder::Continuation(target), hold);
                landed
            }
        };
        let Some(Node::ApplyFun { callee, args, .. }) = self.module.node(node).cloned() else {
            unreachable!("a call moves as a call");
        };
        self.set(
            node,
            Node::ApplyFun {
                callee,
                args,
                return_to: returns_to,
            },
        );
    }
}
