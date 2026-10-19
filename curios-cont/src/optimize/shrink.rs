//! Shrinking reductions, taken to their normal form in one pass.
//!
//! Each rewrite here leaves the graph smaller and reads only the nodes around it: a binding nothing reads is dropped, an intrinsic over literals is folded, a projection or a row test of a visible construction is answered, an identity law is applied, a switch on a literal takes its edge, a continuation with one jump is put in its place, a function with one call that names it nowhere else is moved into the call, and a continuation or a function nothing reaches is dropped with its body. What one exposes is another's occasion — a folded switch leaves its join one jump, the join put in place hands the next switch its construction — so each is reduced where it is exposed, before the pass ends, rather than by the optimizer's next round: a chain of n such links costs n reductions, where a round apiece would cost n walks of the module.
//!
//! **The census is the pass's own.** What names each value, jumps to each continuation, calls or names each function, binds each entity, and refers to each node is read once off the module where the pass begins, kept exact by every rewrite the pass makes, and dropped where it ends; no use information outlives the pass ([`Module`]'s rule). A rewrite that changes what a node refers to queues every entity whose count moved, and the node itself.
//!
//! A worked-through reference is Appel and Jim's imperative algorithm (*Shrinking lambda expressions in linear time*, JFP 1997), which reduces from a set of redexes instead of rounds of census and reduction; Benton, Kennedy, Lindley and Russo implement it in SML.NET (*Shrinking reductions in SML.NET*, 2004).

#[cfg(test)]
mod tests;

use {
    crate::{
        Atom, Callee, ContinuationId, Edge, FunctionId, Intrinsic, Literal, Module, Node, NodeId,
        ValueExpr, ValueId, analyze_calls, atoms, collect_control_targets, evaluate,
        visit_atoms_mut,
    },
    curios_num::{Integer, Natural},
    std::collections::{BTreeMap, BTreeSet, VecDeque},
};

/// Reduce every shrinking redex in `module`, and every one a reduction exposes, answering whether anything changed.
pub(crate) fn shrink(module: &mut Module) -> bool {
    let mut pass = Shrink::new(module);
    while let Some(redex) = pass.pending.pop() {
        match redex {
            Redex::Node(id) => pass.reduce_node(id),
            Redex::Value(value) => {
                if let Some(&definer) = pass.census.defined.get(&value) {
                    pass.reduce_node(definer);
                }
            }
            Redex::Continuation(continuation) => pass.reduce_continuation(continuation),
            Redex::Function(function) => pass.reduce_function(function),
        }
    }
    pass.changed
}

/// Something whose rewrite a count or a content change may have made possible.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Redex {
    Node(NodeId),
    Value(ValueId),
    Continuation(ContinuationId),
    Function(FunctionId),
}

/// The redexes to look at again, first in first out, each queued once at a time.
#[derive(Default)]
struct Pending {
    order: VecDeque<Redex>,
    queued: BTreeSet<Redex>,
}

impl Pending {
    fn push(&mut self, redex: Redex) {
        if self.queued.insert(redex) {
            self.order.push_back(redex);
        }
    }

    fn pop(&mut self) -> Option<Redex> {
        let redex = self.order.pop_front()?;
        self.queued.remove(&redex);
        Some(redex)
    }
}

/// What refers to a node: the node it follows, or the continuation or function whose body it is.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Referrer {
    Node(NodeId),
    Continuation(ContinuationId),
    Function(FunctionId),
}

/// Who refers to what, by node.
///
/// **A parameter is replaced where it is named.** A continuation's parameters are bound over its body and the functions nested in it, and a substitution walking that region whole walks every continuation nested inside it, so putting a nesting of n of them in place would cost its square. The sites a value is named at are what a substitution rewrites.
#[derive(Default)]
struct Census {
    /// The nodes naming each value: an operand, or a closure call's callee.
    uses: BTreeMap<ValueId, BTreeSet<NodeId>>,
    /// The nodes transferring to each continuation, a function's return sentinel among them.
    transfers: BTreeMap<ContinuationId, BTreeSet<NodeId>>,
    /// The nodes calling each function by name.
    calls: BTreeMap<FunctionId, BTreeSet<NodeId>>,
    /// The nodes naming each function as a value.
    names: BTreeMap<FunctionId, BTreeSet<NodeId>>,
    /// The node binding each value a node computes.
    defined: BTreeMap<ValueId, NodeId>,
    /// The `LetFun` binding each function.
    bound: BTreeMap<FunctionId, NodeId>,
    /// The `LetCont` binding each continuation.
    held: BTreeMap<ContinuationId, NodeId>,
    parent: BTreeMap<NodeId, Referrer>,
}

/// Everything one node refers to, which the census files under the node.
#[derive(Default)]
struct Refers {
    values: Vec<ValueId>,
    targets: BTreeSet<ContinuationId>,
    callee: Option<FunctionId>,
    functions: Vec<FunctionId>,
    defines: Option<ValueId>,
    binds: Vec<FunctionId>,
    holds: Vec<ContinuationId>,
    child: Option<NodeId>,
}

fn refers(node: &Node) -> Refers {
    let mut refers = Refers::default();
    for atom in atoms(node) {
        match atom {
            Atom::Value(value) => refers.values.push(*value),
            Atom::Fun(function) => refers.functions.push(*function),
            Atom::Literal(_) | Atom::Filler => {}
        }
    }
    collect_control_targets(node, &mut refers.targets);
    match node {
        Node::ApplyFun {
            callee: Callee::Known(function),
            ..
        } => refers.callee = Some(*function),
        Node::ApplyFun {
            callee: Callee::Closure(value),
            ..
        } => refers.values.push(*value),
        Node::LetValue { result, next, .. } | Node::LetIntrinsic { result, next, .. } => {
            refers.defines = Some(*result);
            refers.child = Some(*next);
        }
        Node::LetFun { functions, body } => {
            refers.binds = functions.clone();
            refers.child = Some(*body);
        }
        Node::LetCont {
            continuations,
            body,
        } => {
            refers.holds = continuations.clone();
            refers.child = Some(*body);
        }
        _ => {}
    }
    refers
}

impl Census {
    /// File `node` under `id`, queueing it and everything whose count it raises.
    fn note(&mut self, id: NodeId, node: &Node, pending: &mut Pending) {
        let refers = refers(node);
        for value in refers.values {
            self.uses.entry(value).or_default().insert(id);
            pending.push(Redex::Value(value));
        }
        for target in refers.targets {
            self.transfers.entry(target).or_default().insert(id);
            pending.push(Redex::Continuation(target));
        }
        for function in refers.callee.into_iter() {
            self.calls.entry(function).or_default().insert(id);
            pending.push(Redex::Function(function));
        }
        for function in refers.functions {
            self.names.entry(function).or_default().insert(id);
            pending.push(Redex::Function(function));
        }
        if let Some(value) = refers.defines {
            self.defined.insert(value, id);
        }
        for function in refers.binds {
            self.bound.insert(function, id);
        }
        for continuation in refers.holds {
            self.held.insert(continuation, id);
        }
        if let Some(child) = refers.child {
            self.parent.insert(child, Referrer::Node(id));
        }
        pending.push(Redex::Node(id));
    }

    /// Withdraw `node` from under `id`, queueing everything whose count it lowers.
    fn forget(&mut self, id: NodeId, node: &Node, pending: &mut Pending) {
        let refers = refers(node);
        for value in refers.values {
            withdraw(&mut self.uses, value, id);
            pending.push(Redex::Value(value));
        }
        for target in refers.targets {
            withdraw(&mut self.transfers, target, id);
            pending.push(Redex::Continuation(target));
        }
        for function in refers.callee.into_iter() {
            withdraw(&mut self.calls, function, id);
            pending.push(Redex::Function(function));
        }
        for function in refers.functions {
            withdraw(&mut self.names, function, id);
            pending.push(Redex::Function(function));
        }
        if let Some(value) = refers.defines
            && self.defined.get(&value) == Some(&id)
        {
            self.defined.remove(&value);
        }
        for function in refers.binds {
            if self.bound.get(&function) == Some(&id) {
                self.bound.remove(&function);
            }
        }
        for continuation in refers.holds {
            if self.held.get(&continuation) == Some(&id) {
                self.held.remove(&continuation);
            }
        }
        if let Some(child) = refers.child
            && self.parent.get(&child) == Some(&Referrer::Node(id))
        {
            self.parent.remove(&child);
        }
    }

    fn count<K: Ord>(sites: &BTreeMap<K, BTreeSet<NodeId>>, key: &K) -> usize {
        sites.get(key).map_or(0, BTreeSet::len)
    }
}

fn withdraw<K: Ord>(sites: &mut BTreeMap<K, BTreeSet<NodeId>>, key: K, id: NodeId) {
    if let Some(nodes) = sites.get_mut(&key) {
        nodes.remove(&id);
        if nodes.is_empty() {
            sites.remove(&key);
        }
    }
}

/// Where a jump to a continuation stands relative to that continuation's own body.
enum Standing {
    Outside,
    Within,
    Unknown,
}

/// One pass: the module, its census, and what is left to look at.
struct Shrink<'m> {
    module: &'m mut Module,
    census: Census,
    pending: Pending,
    /// The functions on a cycle of the call graph closed under definition where the pass begins ([`analyze_calls`]). A reduction makes no cycle that was not there — a substitution turns into a call only a reference the closed graph already counts as one, and a moved body brings its edges into the function that called it — so a function outside this set never comes to call itself, and moving its body into its one call never moves it into itself.
    recursive: BTreeSet<FunctionId>,
    /// The continuations a jump from within their own body may enter: every one jumped to from inside its body where the pass begins, and every member of a group one of whose members was put in place, since a member put in place within a sibling's body brings its jumps to that sibling inside it. Put in place at a jump within its own body, a continuation would hold itself, so one of these is put in place only once its one jump is found outside it.
    looping: BTreeSet<ContinuationId>,
    changed: bool,
}

impl<'m> Shrink<'m> {
    fn new(module: &'m mut Module) -> Self {
        let mut pending = Pending::default();
        let mut census = Census::default();
        for (id, node) in module.live_nodes() {
            census.note(id, node, &mut pending);
        }
        for (continuation, definition) in module.live_continuations() {
            census
                .parent
                .insert(definition.body, Referrer::Continuation(continuation));
            pending.push(Redex::Continuation(continuation));
        }
        for (function, definition) in module.live_functions() {
            census
                .parent
                .insert(definition.body, Referrer::Function(function));
            pending.push(Redex::Function(function));
        }
        let recursive = analyze_calls(module).recursive;
        let looping = looping_continuations(module);
        Self {
            module,
            census,
            pending,
            recursive,
            looping,
            changed: false,
        }
    }

    /// Replace the node at `id`, keeping the census exact.
    fn set(&mut self, id: NodeId, node: Node) {
        let old = std::mem::replace(self.module.node_mut(id).expect("a node set is live"), node);
        self.census.forget(id, &old, &mut self.pending);
        let new = self.module.node(id).expect("a node set is live");
        self.census.note(id, new, &mut self.pending);
    }

    /// Tombstone the node at `id` and hand back what it held.
    fn remove(&mut self, id: NodeId) -> Node {
        let node = self.module.remove_node(id).expect("a node removed is live");
        self.census.forget(id, &node, &mut self.pending);
        self.census.parent.remove(&id);
        node
    }

    /// Point `referrer` at `child`.
    fn link(&mut self, referrer: Referrer, child: NodeId) {
        match referrer {
            Referrer::Node(parent) => match self.module.node_mut(parent) {
                Some(Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. }) => {
                    *next = child
                }
                Some(Node::LetFun { body, .. } | Node::LetCont { body, .. }) => *body = child,
                _ => unreachable!("a referring node has one successor"),
            },
            Referrer::Continuation(continuation) => {
                self.module
                    .continuation_mut(continuation)
                    .expect("a referring continuation is live")
                    .body = child;
            }
            Referrer::Function(function) => {
                self.module
                    .function_mut(function)
                    .expect("a referring function is live")
                    .body = child;
            }
        }
        self.census.parent.insert(child, referrer);
    }

    /// Take the binding at `id` out of its chain, its successor taking its place.
    fn splice(&mut self, id: NodeId) {
        let referrer = self.census.parent.get(&id).copied();
        let child = match self.remove(id) {
            Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. } => next,
            Node::LetFun { body, .. } | Node::LetCont { body, .. } => body,
            _ => unreachable!("only a binding is spliced"),
        };
        if let Some(referrer) = referrer {
            self.link(referrer, child);
        }
    }

    /// Drop the value binding at `id`, which nothing reads.
    fn unbind(&mut self, id: NodeId, result: ValueId) {
        self.splice(id);
        self.module.remove_value(result);
        self.changed = true;
    }

    /// Whether every site naming `value` can name `replacement` instead: a closure call's callee can be a value or a function, never a literal or a filler.
    fn substitutable(&self, value: ValueId, replacement: &Atom) -> bool {
        if !matches!(replacement, Atom::Literal(_) | Atom::Filler) {
            return true;
        }
        !self.census.uses.get(&value).is_some_and(|sites| {
            sites.iter().any(|site| {
                matches!(
                    self.module.node(*site),
                    Some(Node::ApplyFun { callee: Callee::Closure(callee), .. }) if *callee == value
                )
            })
        })
    }

    /// Name `replacement` wherever `value` is named. A closure call whose callee becomes a function is a call of it, or stands where no run arrives when the function takes another number of arguments ([`Module::applies`]).
    fn substitute(&mut self, value: ValueId, replacement: &Atom) {
        let sites = self.census.uses.get(&value).cloned().unwrap_or_default();
        for site in sites {
            let mut node = self.module.node(site).expect("a use is live").clone();
            if let Node::ApplyFun {
                callee: Callee::Closure(callee),
                args,
                ..
            } = &node
                && *callee == value
                && let Atom::Fun(function) = replacement
                && !self.module.applies(*function, args.len())
            {
                self.set(site, Node::Unreachable);
                continue;
            }
            visit_atoms_mut(&mut node, &mut |atom| {
                if *atom == Atom::Value(value) {
                    *atom = replacement.clone();
                }
            });
            if let Node::ApplyFun { callee, .. } = &mut node
                && let Callee::Closure(callee_value) = *callee
                && callee_value == value
            {
                *callee = match replacement {
                    Atom::Value(replacement) => Callee::Closure(*replacement),
                    Atom::Fun(function) => Callee::Known(*function),
                    Atom::Literal(_) | Atom::Filler => {
                        unreachable!("a callee is replaced only where it is substitutable")
                    }
                };
            }
            self.set(site, node);
        }
    }

    /// Replace each of `params` by its argument, unless one cannot be.
    fn bind_params(&mut self, params: &[ValueId], args: &[Atom]) -> bool {
        if !params
            .iter()
            .zip(args)
            .all(|(param, arg)| self.substitutable(*param, arg))
        {
            return false;
        }
        for (param, arg) in params.iter().zip(args) {
            self.substitute(*param, arg);
        }
        true
    }

    /// Drop the subtree rooted at `root` and everything bound in it.
    fn clean(&mut self, root: NodeId) {
        let mut work = vec![root];
        while let Some(id) = work.pop() {
            if self.module.node(id).is_none() {
                continue;
            }
            match self.remove(id) {
                Node::LetValue { result, next, .. } | Node::LetIntrinsic { result, next, .. } => {
                    self.module.remove_value(result);
                    work.push(next);
                }
                Node::LetFun { functions, body } => {
                    for function in functions {
                        if let Some(definition) = self.module.function(function).cloned() {
                            work.push(definition.body);
                            self.drop_definition_of_function(function, &definition.params);
                        }
                    }
                    work.push(body);
                }
                Node::LetCont {
                    continuations,
                    body,
                } => {
                    for continuation in continuations {
                        if let Some(definition) = self.module.continuation(continuation).cloned() {
                            work.push(definition.body);
                            self.drop_definition_of_continuation(continuation, &definition.params);
                        }
                    }
                    work.push(body);
                }
                _ => {}
            }
        }
    }

    fn drop_definition_of_function(&mut self, function: FunctionId, params: &[ValueId]) {
        self.module.remove_function(function);
        for param in params {
            self.module.remove_value(*param);
        }
    }

    fn drop_definition_of_continuation(
        &mut self,
        continuation: ContinuationId,
        params: &[ValueId],
    ) {
        self.module.remove_continuation(continuation);
        for param in params {
            self.module.remove_value(*param);
        }
    }

    /// Take `continuation` out of the group binding it, and hand back the group's other members.
    fn unhold(&mut self, continuation: ContinuationId) -> Vec<ContinuationId> {
        let Some(&binder) = self.census.held.get(&continuation) else {
            return Vec::new();
        };
        let Some(Node::LetCont {
            continuations,
            body,
        }) = self.module.node(binder).cloned()
        else {
            return Vec::new();
        };
        let rest = continuations
            .into_iter()
            .filter(|member| *member != continuation)
            .collect::<Vec<_>>();
        self.set(
            binder,
            Node::LetCont {
                continuations: rest.clone(),
                body,
            },
        );
        rest
    }

    /// Take `function` out of the group binding it.
    fn unbind_function(&mut self, function: FunctionId) {
        let Some(&binder) = self.census.bound.get(&function) else {
            return;
        };
        let Some(Node::LetFun { functions, body }) = self.module.node(binder).cloned() else {
            return;
        };
        let rest = functions
            .into_iter()
            .filter(|member| *member != function)
            .collect();
        self.set(
            binder,
            Node::LetFun {
                functions: rest,
                body,
            },
        );
    }

    fn reduce_node(&mut self, id: NodeId) {
        let Some(node) = self.module.node(id).cloned() else {
            return;
        };
        match node {
            Node::LetValue {
                result,
                value: ValueExpr::Literal(literal),
                ..
            } => {
                let literal = Atom::Literal(literal);
                if self.substitutable(result, &literal) {
                    self.substitute(result, &literal);
                }
                if Census::count(&self.census.uses, &result) == 0 {
                    self.unbind(id, result);
                }
            }
            Node::LetValue { result, .. } if Census::count(&self.census.uses, &result) == 0 => {
                self.unbind(id, result);
            }
            Node::LetIntrinsic {
                result,
                op,
                args,
                next,
            } => {
                if let Some(literal) = evaluate(op, &args) {
                    self.fold(id, result, literal, next);
                    return;
                }
                match self
                    .forwarded(op, &args)
                    .or_else(|| identity_fold(op, &args))
                {
                    Some(Fold::Operand(replacement))
                        if self.substitutable(result, &replacement) =>
                    {
                        self.substitute(result, &replacement);
                        self.unbind(id, result);
                        return;
                    }
                    Some(Fold::Literal(literal)) => {
                        self.fold(id, result, literal, next);
                        return;
                    }
                    Some(Fold::Operand(_)) | None => {}
                }
                if op.is_total() && Census::count(&self.census.uses, &result) == 0 {
                    self.unbind(id, result);
                }
            }
            Node::LetFun { functions, .. } if functions.is_empty() => {
                self.splice(id);
                self.changed = true;
            }
            Node::LetCont { continuations, .. } if continuations.is_empty() => {
                self.splice(id);
                self.changed = true;
            }
            Node::Switch {
                scrutinee: Atom::Literal(Literal::Nat(tag)),
                cases,
                default,
            } => {
                if let Some(edge) = u32::try_from(&tag)
                    .ok()
                    .and_then(|tag| cases.get(&tag))
                    .or(default.as_ref())
                    .cloned()
                {
                    self.set(id, Node::ApplyCont(edge));
                    self.changed = true;
                }
            }
            _ => {}
        }
    }

    /// Pin the intrinsic at `id` to the literal it computes, which the binding then hands every reader.
    fn fold(&mut self, id: NodeId, result: ValueId, literal: Literal, next: NodeId) {
        self.set(
            id,
            Node::LetValue {
                result,
                value: ValueExpr::Literal(literal),
                next,
            },
        );
        self.changed = true;
    }

    /// The field a projection of a visible construction reads, or the answer a row test of one gives.
    ///
    /// A read forwards only through a construction of the vocabulary it reads in — a `RowGet` never through a structural tuple, nor a `TupleGet` through a row — and a construction is a value of exactly the vocabulary it was built in, so a row test answers whether that is the row it asks for.
    fn forwarded(&self, op: Intrinsic, args: &[Atom]) -> Option<Fold> {
        let [Atom::Value(aggregate)] = args else {
            return None;
        };
        let definer = self.census.defined.get(aggregate)?;
        let Some(Node::LetValue { value, .. }) = self.module.node(*definer) else {
            return None;
        };
        let built = match value {
            ValueExpr::Tuple(fields) => (None, fields),
            ValueExpr::Row(row, fields) => (Some(*row), fields),
            ValueExpr::Literal(_) | ValueExpr::List(_) => return None,
        };
        match op {
            Intrinsic::TupleGet(field) if built.0.is_none() => {
                built.1.get(field).cloned().map(Fold::Operand)
            }
            Intrinsic::RowGet(row, field) if built.0 == Some(row) => {
                built.1.get(field).cloned().map(Fold::Operand)
            }
            Intrinsic::IsRow(asked) => Some(Fold::Literal(Literal::Nat(Natural::from(u32::from(
                built.0 == Some(asked),
            ))))),
            _ => None,
        }
    }

    fn reduce_continuation(&mut self, continuation: ContinuationId) {
        let Some(definition) = self.module.continuation(continuation).cloned() else {
            return;
        };
        let jumps = self
            .census
            .transfers
            .get(&continuation)
            .cloned()
            .unwrap_or_default();
        match jumps.len() {
            0 => self.drop_continuation(continuation),
            1 => {
                let jump = *jumps.first().expect("one jump");
                let Some(Node::ApplyCont(Edge { target, args })) = self.module.node(jump).cloned()
                else {
                    return;
                };
                if target != continuation || args.len() != definition.params.len() {
                    return;
                }
                if self.looping.contains(&continuation) {
                    match self.standing(jump, continuation) {
                        Standing::Outside => {}
                        // Entered from nowhere but itself, it is entered from nowhere.
                        Standing::Within => {
                            self.drop_continuation(continuation);
                            return;
                        }
                        Standing::Unknown => return,
                    }
                }
                self.put_continuation(continuation, jump, &args);
            }
            _ => {}
        }
    }

    /// Where `jump` stands relative to `continuation`'s body, read up the referrers from the jump to the group binding the continuation.
    fn standing(&self, jump: NodeId, continuation: ContinuationId) -> Standing {
        let Some(&binder) = self.census.held.get(&continuation) else {
            return Standing::Unknown;
        };
        let mut at = jump;
        loop {
            if at == binder {
                return Standing::Outside;
            }
            match self.census.parent.get(&at) {
                Some(Referrer::Node(parent)) => at = *parent,
                Some(Referrer::Continuation(enclosing)) => {
                    if *enclosing == continuation {
                        return Standing::Within;
                    }
                    let Some(&enclosing_binder) = self.census.held.get(enclosing) else {
                        return Standing::Unknown;
                    };
                    at = enclosing_binder;
                }
                Some(Referrer::Function(_)) | None => return Standing::Unknown,
            }
        }
    }

    /// Put `continuation`'s body in place of `jump`, its one transfer, each parameter replaced by its argument.
    fn put_continuation(&mut self, continuation: ContinuationId, jump: NodeId, args: &[Atom]) {
        let definition = self
            .module
            .continuation(continuation)
            .cloned()
            .expect("a continuation put in place is live");
        if !self.bind_params(&definition.params, args) {
            return;
        }
        let body = self.remove(definition.body);
        self.set(jump, body);
        let siblings = self.unhold(continuation);
        self.drop_definition_of_continuation(continuation, &definition.params);
        self.looping.extend(siblings);
        self.changed = true;
    }

    fn drop_continuation(&mut self, continuation: ContinuationId) {
        let definition = self
            .module
            .continuation(continuation)
            .cloned()
            .expect("a continuation dropped is live");
        self.unhold(continuation);
        self.clean(definition.body);
        self.drop_definition_of_continuation(continuation, &definition.params);
        self.changed = true;
    }

    fn reduce_function(&mut self, function: FunctionId) {
        if self.module.entry() == Some(function) || self.module.function(function).is_none() {
            return;
        }
        let calls = Census::count(&self.census.calls, &function);
        let names = Census::count(&self.census.names, &function);
        match (calls, names) {
            (0, 0) => self.drop_function(function),
            (1, 0)
                if !self.recursive.contains(&function) && !self.module.is_out_of_line(function) =>
            {
                let call = *self.census.calls[&function].first().expect("one call");
                let Some(Node::ApplyFun {
                    callee: Callee::Known(callee),
                    args,
                    return_to,
                }) = self.module.node(call).cloned()
                else {
                    return;
                };
                if callee == function && self.module.applies(function, args.len()) {
                    self.move_function(function, call, &args, return_to);
                }
            }
            _ => {}
        }
    }

    /// Move `function`'s body into `call`, its one call: each parameter replaced by its argument, and each return made a jump to where the call returns.
    fn move_function(
        &mut self,
        function: FunctionId,
        call: NodeId,
        args: &[Atom],
        return_to: ContinuationId,
    ) {
        let definition = self
            .module
            .function(function)
            .cloned()
            .expect("a function moved is live");
        if !self.bind_params(&definition.params, args) {
            return;
        }
        let returns = self
            .census
            .transfers
            .get(&definition.return_cont)
            .cloned()
            .unwrap_or_default();
        for site in returns {
            let mut node = self.module.node(site).expect("a return is live").clone();
            retarget_transfers(&mut node, definition.return_cont, return_to);
            self.set(site, node);
        }
        let body = self.remove(definition.body);
        self.set(call, body);
        self.unbind_function(function);
        self.drop_definition_of_function(function, &definition.params);
        self.changed = true;
    }

    fn drop_function(&mut self, function: FunctionId) {
        let definition = self
            .module
            .function(function)
            .cloned()
            .expect("a function dropped is live");
        self.unbind_function(function);
        self.clean(definition.body);
        self.drop_definition_of_function(function, &definition.params);
        self.changed = true;
    }
}

/// Make every transfer of `node` to `from` a transfer to `to`.
pub(crate) fn retarget_transfers(node: &mut Node, from: ContinuationId, to: ContinuationId) {
    let mut edge = |edge: &mut Edge| {
        if edge.target == from {
            edge.target = to;
        }
    };
    match node {
        Node::ApplyCont(target) => edge(target),
        Node::Switch { cases, default, .. } => {
            cases.values_mut().for_each(&mut edge);
            default.iter_mut().for_each(edge);
        }
        Node::ApplyFun { return_to, .. }
        | Node::Foreign { return_to, .. }
        | Node::Cell { return_to, .. }
        | Node::Channel { return_to, .. }
        | Node::Intrinsic { return_to, .. } => {
            if *return_to == from {
                *return_to = to;
            }
        }
        Node::LetValue { .. }
        | Node::LetIntrinsic { .. }
        | Node::LetFun { .. }
        | Node::LetCont { .. }
        | Node::Halt { .. }
        | Node::Panic(_)
        | Node::Unreachable => {}
    }
}

/// The continuations entered from within their own body, read in one walk of each function with the continuations whose bodies it stands in.
fn looping_continuations(module: &Module) -> BTreeSet<ContinuationId> {
    enum Step {
        Enter(NodeId),
        Open(ContinuationId),
        Close(ContinuationId),
    }

    let mut looping = BTreeSet::new();
    let mut open = BTreeSet::new();
    let mut targets = BTreeSet::new();
    for (_, function) in module.live_functions() {
        let mut work = vec![Step::Enter(function.body)];
        while let Some(step) = work.pop() {
            let id = match step {
                Step::Enter(id) => id,
                Step::Open(continuation) => {
                    open.insert(continuation);
                    if let Some(definition) = module.continuation(continuation) {
                        work.push(Step::Enter(definition.body));
                    }
                    continue;
                }
                Step::Close(continuation) => {
                    open.remove(&continuation);
                    continue;
                }
            };
            let Some(node) = module.node(id) else {
                continue;
            };
            targets.clear();
            collect_control_targets(node, &mut targets);
            looping.extend(targets.iter().filter(|target| open.contains(*target)));
            match node {
                Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. } => {
                    work.push(Step::Enter(*next));
                }
                Node::LetFun { body, .. } => work.push(Step::Enter(*body)),
                Node::LetCont {
                    continuations,
                    body,
                } => {
                    work.push(Step::Enter(*body));
                    for continuation in continuations {
                        work.push(Step::Close(*continuation));
                        work.push(Step::Open(*continuation));
                    }
                }
                _ => {}
            }
        }
    }
    looping
}

/// The two rewrite shapes an identity law or a read of a construction produces: forward an operand, or pin a literal.
enum Fold {
    Operand(Atom),
    Literal(Literal),
}

/// Match one `Nat`/`Int` identity or absorption law on a binary intrinsic with a literal neutral or absorbing operand: `x + 0`, `x - 0`, `x * 1`, `x * 0`, `x / 1`, `x % 1`, `x & 0`, `x | 0`, `x ^ 0`, and shifts by zero.
///
/// Trap discipline: `Nat` and `Int` arithmetic is unbounded and `nat_sub` is monus, so the only trap among these operations is a zero divisor, which a `/ 1` or `% 1` can never be. Every fold here returns either an operand that is already a live value or a literal, so no trap is added or dropped. `Flt` deliberately has no laws here: `x + 0.0` is not the identity on `-0.0`.
fn identity_fold(op: Intrinsic, args: &[Atom]) -> Option<Fold> {
    let [left, right] = args else { return None };
    // The carriers are unbounded, so a law tests a literal against a value rather than reading a machine scalar out of it.
    let nat = |atom: &Atom, expected: u32| matches!(atom, Atom::Literal(Literal::Nat(value)) if *value == Natural::from(expected));
    let int = |atom: &Atom, expected: i32| matches!(atom, Atom::Literal(Literal::Int(value)) if *value == Integer::from(expected));
    let operand = |atom: &Atom| Some(Fold::Operand(atom.clone()));

    match op {
        Intrinsic::NatAdd | Intrinsic::NatOr | Intrinsic::NatXor => {
            if nat(right, 0) {
                operand(left)
            } else if nat(left, 0) {
                operand(right)
            } else {
                None
            }
        }
        Intrinsic::IntAdd | Intrinsic::IntOr | Intrinsic::IntXor => {
            if int(right, 0) {
                operand(left)
            } else if int(left, 0) {
                operand(right)
            } else {
                None
            }
        }
        // A shift count is a `Nat` on both carriers.
        Intrinsic::NatSub
        | Intrinsic::NatShl
        | Intrinsic::NatShr
        | Intrinsic::IntShl
        | Intrinsic::IntShr => (nat(right, 0)).then(|| operand(left)).flatten(),
        Intrinsic::IntSub => (int(right, 0)).then(|| operand(left)).flatten(),
        Intrinsic::NatMul => {
            if nat(right, 1) {
                operand(left)
            } else if nat(left, 1) {
                operand(right)
            } else if nat(right, 0) || nat(left, 0) {
                Some(Fold::Literal(Literal::Nat(Natural::zero())))
            } else {
                None
            }
        }
        Intrinsic::IntMul => {
            if int(right, 1) {
                operand(left)
            } else if int(left, 1) {
                operand(right)
            } else if int(right, 0) || int(left, 0) {
                Some(Fold::Literal(Literal::Int(Integer::from(0u32))))
            } else {
                None
            }
        }
        Intrinsic::NatDiv => (nat(right, 1)).then(|| operand(left)).flatten(),
        Intrinsic::IntDiv => (int(right, 1)).then(|| operand(left)).flatten(),
        Intrinsic::NatRem => {
            (nat(right, 1)).then_some(Fold::Literal(Literal::Nat(Natural::zero())))
        }
        Intrinsic::IntRem => {
            (int(right, 1)).then_some(Fold::Literal(Literal::Int(Integer::from(0u32))))
        }
        Intrinsic::NatAnd => {
            (nat(right, 0) || nat(left, 0)).then_some(Fold::Literal(Literal::Nat(Natural::zero())))
        }
        Intrinsic::IntAnd => (int(right, 0) || int(left, 0))
            .then_some(Fold::Literal(Literal::Int(Integer::from(0u32)))),
        _ => None,
    }
}
