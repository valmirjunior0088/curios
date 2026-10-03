//! The verifier: the structure every pass may assume of a module, the rows and lexical scope a round boundary promises on top, and the scope walk the call analysis reads back.

use {
    super::{
        Atom, Callee, ContinuationId, Edge, Function, FunctionId, Intrinsic, IntrinsicCall, Module,
        Node, NodeId, RowId, ValueExpr, ValueId, atoms,
    },
    std::{
        collections::{BTreeMap, BTreeSet},
        fmt,
    },
};

/// What the module's functions state about returning: which continuation is whose sentinel, and how many values each hands back.
///
/// The two travel together because every arity question needs both — whether a transfer is a return at all, and how wide a return is — so they are one parameter rather than two threaded in parallel through the verifier.
struct ReturnFacts<'a> {
    owners: &'a BTreeMap<ContinuationId, FunctionId>,
    arities: &'a BTreeMap<FunctionId, usize>,
}

impl ReturnFacts<'_> {
    /// How many values `function` returns, reading absence as the single value a function returns unless a protocol widened it.
    fn arity(&self, function: FunctionId) -> usize {
        self.arities.get(&function).copied().unwrap_or(1)
    }
}

#[derive(Debug, Clone)]
pub struct VerifyError(pub String);

impl fmt::Display for VerifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for VerifyError {}

/// A value a node binds, bound here and nowhere else; `noun` names it in the duplicate-binding message.
struct ScopeBinding {
    value: ValueId,
    noun: &'static str,
}

/// A pending region in a walk over lexical structure, carrying the scope that region sees.
enum ScopeTask {
    Function {
        function: FunctionId,
        values: BTreeSet<ValueId>,
        functions: BTreeSet<FunctionId>,
    },
    Node {
        /// The function the node belongs to. It names the region in a verification message and is needed for nothing else, so a walk that reports nothing has none.
        owner: Option<FunctionId>,
        node: NodeId,
        values: BTreeSet<ValueId>,
        functions: BTreeSet<FunctionId>,
        continuations: BTreeSet<ContinuationId>,
    },
}

/// What one region contributes to a lexical walk: the names it binds, and the regions below it.
#[derive(Default)]
struct ScopeStep {
    values: Vec<ScopeBinding>,
    functions: Vec<FunctionId>,
    tasks: Vec<ScopeTask>,
}

type NodeTask = (
    Option<FunctionId>,
    NodeId,
    BTreeSet<ValueId>,
    BTreeSet<FunctionId>,
    BTreeSet<ContinuationId>,
);

/// The bookkeeping a lexical *verification* walk carries on top of the scope rules: which names have been bound, and which regions are still to visit.
#[derive(Default)]
struct ScopeVerifier {
    bound_functions: BTreeSet<FunctionId>,
    bound_values: BTreeSet<ValueId>,
    function_work: Vec<(FunctionId, BTreeSet<ValueId>, BTreeSet<FunctionId>)>,
    node_work: Vec<NodeTask>,
}

impl ScopeVerifier {
    /// Record what `step` binds, rejecting a name bound twice, then queue the regions below it.
    fn admit(&mut self, step: ScopeStep) -> Result<(), VerifyError> {
        let ScopeStep {
            values,
            functions,
            tasks,
        } = step;
        for function in functions {
            if !self.bound_functions.insert(function) {
                return Err(VerifyError(format!(
                    "function {function} is bound more than once"
                )));
            }
        }
        for ScopeBinding { value, noun } in values {
            if !self.bound_values.insert(value) {
                return Err(VerifyError(format!(
                    "{noun} {value} is bound more than once"
                )));
            }
        }
        self.queue(tasks);
        Ok(())
    }

    /// Queue the regions below a step without deciding anything about what it binds. The recording walk takes this route rather than [`Self::admit`]: it reports nothing, so a name bound twice is not its to refuse, and refusing one would drop the whole region beneath it from a set whose incompleteness costs optimizations.
    fn queue(&mut self, tasks: Vec<ScopeTask>) {
        for task in tasks {
            match task {
                ScopeTask::Function {
                    function,
                    values,
                    functions,
                } => self.function_work.push((function, values, functions)),
                ScopeTask::Node {
                    owner,
                    node,
                    values,
                    functions,
                    continuations,
                } => self
                    .node_work
                    .push((owner, node, values, functions, continuations)),
            }
        }
    }
}

impl Module {
    pub fn verify(&self) -> Result<(), VerifyError> {
        self.verify_with(true)
    }

    /// The round-boundary subset of [`Module::verify`]: every structural clause, without the row-vocabulary one.
    ///
    /// A round's close leaves scoping, ownership and arities canonical, but the vocabulary clause holds only of the *converged* module: constant folding pushes a decided reply's payload into both arms of its dispatch, so until a later round threads the decided switch and prunes behind it, the dead arm legitimately reads that payload in the other vocabulary — the tag the fold decided is what keeps it honest, and no per-round rewrite is obliged to have cleaned it up yet. `/std/Parse`'s reply dispatches reach this state on every `pure`-fed combinator, so the full check at a round boundary would refuse half the cross-stage corpus that the exit gate admits. The entry and exit verifies keep the full set, so a mismatch that survives convergence is still refused where its premise actually holds.
    pub fn verify_structure(&self) -> Result<(), VerifyError> {
        self.verify_with(false)
    }

    fn verify_with(&self, rows: bool) -> Result<(), VerifyError> {
        let entry = self
            .entry()
            .ok_or_else(|| VerifyError("module has no entry function".into()))?;
        self.require_fun(entry, "entry")?;

        let mut returns = BTreeMap::<ContinuationId, FunctionId>::new();
        for (id, function) in self.live_functions() {
            if function.return_cont.index() >= self.continuations().len() {
                return Err(VerifyError(format!(
                    "{id} return continuation {} was not minted by this module",
                    function.return_cont
                )));
            }
            if self.continuation(function.return_cont).is_some() {
                return Err(VerifyError(format!(
                    "{id} return continuation {} also identifies a local continuation",
                    function.return_cont
                )));
            }
            if let Some(previous) = returns.insert(function.return_cont, id) {
                return Err(VerifyError(format!(
                    "{} is the return continuation of both {previous} and {id}",
                    function.return_cont
                )));
            }
            self.require_node(function.body, "function body")?;
            for &param in &function.params {
                self.require_value(param, "function parameter")?;
            }
        }

        for (_, continuation) in self.live_continuations() {
            self.require_node(continuation.body, "continuation body")?;
            for &param in &continuation.params {
                self.require_value(param, "continuation parameter")?;
            }
        }

        let arities = self.return_arities();
        let facts = ReturnFacts {
            owners: &returns,
            arities: &arities,
        };
        let mut node_owners = BTreeMap::<NodeId, FunctionId>::new();
        let mut bound_continuations = BTreeSet::<ContinuationId>::new();
        for (id, function) in self.live_functions() {
            self.verify_function_body(
                id,
                function,
                &facts,
                &mut node_owners,
                &mut bound_continuations,
            )?;
        }
        self.verify_lexical_scopes(entry)?;
        if rows {
            self.verify_rows()?;
        }

        let live_nodes = self.node_ids().collect::<BTreeSet<_>>();
        let owned_nodes = node_owners.keys().copied().collect::<BTreeSet<_>>();
        if live_nodes != owned_nodes {
            return Err(VerifyError(
                "node arena contains an unowned node or an owner references a tombstone".into(),
            ));
        }

        let live_continuations = self.continuation_ids().collect::<BTreeSet<_>>();
        if live_continuations != bound_continuations {
            return Err(VerifyError(
                "local-continuation arena and lexical LetCont bindings disagree".into(),
            ));
        }

        // The recorded fields representations hold: every group names a live continuation and lies inside its parameter list without overlapping a neighbour, so a pass that reshaped a recorded parameter list without maintaining the record fails here rather than silently disagreeing with the split.
        for (continuation, groups) in self.field_groups() {
            let Some(definition) = self.continuation(*continuation) else {
                return Err(VerifyError(format!(
                    "field group records dead continuation {continuation}"
                )));
            };
            let mut end = 0;
            for group in groups {
                if group.width == 0 {
                    return Err(VerifyError(format!(
                        "{continuation} records an empty field group at {}",
                        group.start
                    )));
                }
                if group.start < end {
                    return Err(VerifyError(format!(
                        "{continuation} records overlapping field groups at {}",
                        group.start
                    )));
                }
                end = group.start + group.width;
            }
            if end > definition.params.len() {
                return Err(VerifyError(format!(
                    "{continuation} records a field group past its {} parameters",
                    definition.params.len()
                )));
            }
        }

        Ok(())
    }

    /// The row vocabulary's coherence: every row named by a construction or a read exists, every construction carries exactly its row's width, every read is in range of it — and a read of a value this module visibly constructs is in the vocabulary that construction was minted in.
    ///
    /// This is what the distinct [`ValueExpr::Row`] buys over an annotation on `Tuple`. A row value read at a structural projection, or a construction one slot short of its row, would be a `ref.cast` trap in emitted code far from the pass that caused it; here it is a verifier failure at the boundary that produced it. Padding is the door's job, so a mismatch is always a compiler bug rather than a program's.
    ///
    /// The last clause covers direct operands — a value constructed by a `LetValue` in this module and read by a `TupleGet` or `RowGet` in it — which is every case a pass's own rebuild can produce: `split_returns` rebuilding a resume's `Tuple` for a class returning an `Option` row, read by a `RowGet` that casts `$tuple/2` to the row's final type, would otherwise surface only as a trap far downstream. A value that arrives through a parameter is the emitter's cast to decide.
    fn verify_rows(&self) -> Result<(), VerifyError> {
        // What every visible construction built, so a read can be checked against the vocabulary its operand was actually minted in rather than only against the row's own width.
        let mut built = BTreeMap::<ValueId, Option<RowId>>::new();
        for (_, node) in self.live_nodes() {
            if let Node::LetValue { result, value, .. } = node {
                match value {
                    ValueExpr::Row(row, _) => {
                        built.insert(*result, Some(*row));
                    }
                    ValueExpr::Tuple(_) => {
                        built.insert(*result, None);
                    }
                    ValueExpr::Literal(_) | ValueExpr::List(_) => {}
                }
            }
        }
        for (_, node) in self.live_nodes() {
            if let Node::LetIntrinsic { op, args, .. } = node
                && let [Atom::Value(operand)] = args.as_slice()
                && let Some(&minted) = built.get(operand)
            {
                let read = match op {
                    Intrinsic::RowGet(row, _) => Some(Some(*row)),
                    Intrinsic::TupleGet(_) => Some(None),
                    _ => None,
                };
                if let Some(read) = read
                    && read != minted
                {
                    return Err(VerifyError(format!(
                        "{operand} was built as {} but is read as {}",
                        match minted {
                            Some(row) => format!("{row}"),
                            None => "a structural tuple".into(),
                        },
                        match read {
                            Some(row) => format!("{row}"),
                            None => "a structural tuple".into(),
                        },
                    )));
                }
            }
        }
        for (_, node) in self.live_nodes() {
            match node {
                Node::LetValue {
                    value: ValueExpr::Row(row, atoms),
                    ..
                } => {
                    let Some(definition) = self.defined_row(*row) else {
                        return Err(VerifyError(format!(
                            "row construction names {row}, which was not minted by this module"
                        )));
                    };
                    if atoms.len() != definition.width() {
                        return Err(VerifyError(format!(
                            "row construction of {row} carries {} slots, but the row is {} wide",
                            atoms.len(),
                            definition.width(),
                        )));
                    }
                }
                Node::LetIntrinsic {
                    op: Intrinsic::RowGet(row, index),
                    ..
                } => {
                    let Some(definition) = self.defined_row(*row) else {
                        return Err(VerifyError(format!(
                            "row read names {row}, which was not minted by this module"
                        )));
                    };
                    if *index >= definition.width() {
                        return Err(VerifyError(format!(
                            "row read of {row} at slot {index}, but the row is {} wide",
                            definition.width(),
                        )));
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn verify_lexical_scopes(&self, entry: FunctionId) -> Result<(), VerifyError> {
        let mut walk = ScopeVerifier {
            bound_functions: BTreeSet::from([entry]),
            function_work: vec![(entry, BTreeSet::new(), BTreeSet::from([entry]))],
            ..ScopeVerifier::default()
        };
        let mut visited_nodes = BTreeSet::new();

        while !walk.function_work.is_empty() || !walk.node_work.is_empty() {
            while let Some((function, values, functions)) = walk.function_work.pop() {
                walk.admit(self.function_scope(function, values, functions))?;
            }

            let Some((owner, node_id, values, functions, continuations)) = walk.node_work.pop()
            else {
                continue;
            };
            if !visited_nodes.insert(node_id) {
                continue;
            }
            let owner = owner.expect("a verification task names the function it walks");
            let node = self.node(node_id).unwrap();
            for atom in atoms(node) {
                match atom {
                    Atom::Value(value) if !values.contains(value) => {
                        return Err(VerifyError(format!(
                            "{owner} node {node_id} uses out-of-scope {value}"
                        )));
                    }
                    Atom::Fun(function) if !functions.contains(function) => {
                        return Err(VerifyError(format!(
                            "{owner} node {node_id} uses out-of-scope {function}"
                        )));
                    }
                    Atom::Value(_) | Atom::Fun(_) | Atom::Literal(_) | Atom::Filler => {}
                }
            }
            if let Node::ApplyFun { callee, .. } = node {
                match callee {
                    Callee::Known(function) if !functions.contains(function) => {
                        return Err(VerifyError(format!(
                            "{owner} node {node_id} calls out-of-scope {function}"
                        )));
                    }
                    Callee::Closure(value) if !values.contains(value) => {
                        return Err(VerifyError(format!(
                            "{owner} node {node_id} calls out-of-scope {value}"
                        )));
                    }
                    Callee::Known(_) | Callee::Closure(_) => {}
                }
            }

            walk.admit(self.scope_step(Some(owner), node, values, functions, continuations))?;
        }

        let live_functions = self.function_ids().collect::<BTreeSet<_>>();
        if live_functions != walk.bound_functions {
            return Err(VerifyError(
                "function arena and lexical function bindings disagree".into(),
            ));
        }
        let live_values = self.value_ids().collect::<BTreeSet<_>>();
        if live_values != walk.bound_values {
            return Err(VerifyError(
                "value arena and lexical value bindings disagree".into(),
            ));
        }
        Ok(())
    }

    /// The functions each body may name, by the rule [`Self::scope_step`] states and [`Self::verify_lexical_scopes`] enforces: its own `LetFun` group, every group enclosing it, and every group bound *before* it along the chain from the entry. Recorded rather than checked, so a pass forwarding a function reference into a body can ask whether that body may legally name it.
    ///
    /// A function the walk does not reach is absent rather than empty, and the caller decides what to answer for it. This runs mid-round, where the module is transiently unscoped by design and only a round boundary promises a walk from the entry reaches every live function.
    pub(crate) fn lexical_scopes(&self) -> BTreeMap<FunctionId, BTreeSet<FunctionId>> {
        let Some(entry) = self.entry() else {
            return BTreeMap::new();
        };
        let mut scopes = BTreeMap::new();
        let mut walk = ScopeVerifier {
            function_work: vec![(entry, BTreeSet::new(), BTreeSet::from([entry]))],
            ..ScopeVerifier::default()
        };
        let mut visited_nodes = BTreeSet::new();

        while !walk.function_work.is_empty() || !walk.node_work.is_empty() {
            while let Some((function, values, functions)) = walk.function_work.pop() {
                scopes.insert(function, functions.clone());
                let step = self.function_scope(function, values, functions);
                walk.queue(step.tasks);
            }

            let Some((_, node_id, values, functions, continuations)) = walk.node_work.pop() else {
                continue;
            };
            if !visited_nodes.insert(node_id) {
                continue;
            }
            let Some(node) = self.node(node_id) else {
                continue;
            };
            let step = self.scope_step(None, node, values, functions, continuations);
            walk.queue(step.tasks);
        }
        scopes
    }

    /// The scope a function's own body sees: its parameters join the values it inherits, and no continuation crosses the boundary.
    fn function_scope(
        &self,
        function: FunctionId,
        mut values: BTreeSet<ValueId>,
        functions: BTreeSet<FunctionId>,
    ) -> ScopeStep {
        let definition = self.function(function).unwrap();
        let mut step = ScopeStep::default();
        for value in &definition.params {
            step.values.push(ScopeBinding {
                value: *value,
                noun: "function parameter",
            });
            values.insert(*value);
        }
        step.tasks.push(ScopeTask::Node {
            owner: Some(function),
            node: definition.body,
            values,
            functions,
            continuations: BTreeSet::new(),
        });
        step
    }

    /// What `node` binds, and the regions below it with the scope each one sees.
    ///
    /// This is the single statement of the lexical scoping rules, which [`Self::verify_lexical_scopes`] enforces.
    fn scope_step(
        &self,
        owner: Option<FunctionId>,
        node: &Node,
        values: BTreeSet<ValueId>,
        functions: BTreeSet<FunctionId>,
        continuations: BTreeSet<ContinuationId>,
    ) -> ScopeStep {
        let mut step = ScopeStep::default();
        match node {
            Node::LetValue { result, next, .. } | Node::LetIntrinsic { result, next, .. } => {
                step.values.push(ScopeBinding {
                    value: *result,
                    noun: "node result",
                });
                let mut inner = values;
                inner.insert(*result);
                step.tasks.push(ScopeTask::Node {
                    owner,
                    node: *next,
                    values: inner,
                    functions,
                    continuations,
                });
            }
            Node::LetFun {
                functions: members,
                body,
            } => {
                let mut inner = functions;
                for function in members {
                    step.functions.push(*function);
                    inner.insert(*function);
                }
                for function in members.iter().rev() {
                    step.tasks.push(ScopeTask::Function {
                        function: *function,
                        values: values.clone(),
                        functions: inner.clone(),
                    });
                }
                step.tasks.push(ScopeTask::Node {
                    owner,
                    node: *body,
                    values,
                    functions: inner,
                    continuations,
                });
            }
            Node::LetCont {
                continuations: members,
                body,
            } => {
                let mut inner = continuations;
                inner.extend(members.iter().copied());
                for continuation in members.iter().rev() {
                    // `verify_node` has already rejected a `LetCont` naming a missing member, so the walk may read it.
                    let definition = self.continuation(*continuation).unwrap();
                    let mut continuation_values = values.clone();
                    for value in &definition.params {
                        step.values.push(ScopeBinding {
                            value: *value,
                            noun: "continuation parameter",
                        });
                        continuation_values.insert(*value);
                    }
                    step.tasks.push(ScopeTask::Node {
                        owner,
                        node: definition.body,
                        values: continuation_values,
                        functions: functions.clone(),
                        continuations: inner.clone(),
                    });
                }
                step.tasks.push(ScopeTask::Node {
                    owner,
                    node: *body,
                    values,
                    functions,
                    continuations: inner,
                });
            }
            Node::ApplyFun { .. }
            | Node::ApplyCont(_)
            | Node::Switch { .. }
            | Node::Foreign { .. }
            | Node::Cell { .. }
            | Node::Channel { .. }
            | Node::Intrinsic { .. }
            | Node::Halt { .. }
            | Node::Panic(_)
            | Node::Unreachable => {}
        }
        step
    }

    fn verify_function_body(
        &self,
        owner: FunctionId,
        function: &Function,
        facts: &ReturnFacts<'_>,
        node_owners: &mut BTreeMap<NodeId, FunctionId>,
        bound_continuations: &mut BTreeSet<ContinuationId>,
    ) -> Result<(), VerifyError> {
        let mut work = vec![(function.body, BTreeSet::<ContinuationId>::new())];
        let mut visited = BTreeSet::<NodeId>::new();

        while let Some((id, scope)) = work.pop() {
            // Every node has exactly one structural parent: a `next`, a `body`, or a continuation's. Skipping a node already reached would catch a node shared between two *functions* through `node_owners` and let one shared within one function pass — and a shared node is a region that runs on two paths while binding its values once, which the scope check cannot see either, since it admits the bindings on whichever path reached it first. It is also what a nesting printer would duplicate.
            if !visited.insert(id) {
                return Err(VerifyError(format!(
                    "{id} is reached from more than one place in {owner}"
                )));
            }
            if let Some(previous) = node_owners.insert(id, owner)
                && previous != owner
            {
                return Err(VerifyError(format!(
                    "{id} is owned by both {previous} and {owner}"
                )));
            }
            let node = self
                .node(id)
                .ok_or_else(|| VerifyError(format!("function body references missing {id}")))?;
            self.verify_node(owner, function.return_cont, facts, &scope, id, node)?;

            match node {
                Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. } => {
                    work.push((*next, scope));
                }
                Node::LetFun { body, .. } => {
                    work.push((*body, scope));
                }
                Node::LetCont {
                    continuations,
                    body,
                } => {
                    let mut inner = scope;
                    for &continuation in continuations {
                        if facts.owners.contains_key(&continuation) {
                            return Err(VerifyError(format!(
                                "return ID {continuation} cannot be bound as a local continuation"
                            )));
                        }
                        self.require_cont(continuation, "LetCont member")?;
                        if !bound_continuations.insert(continuation) {
                            return Err(VerifyError(format!(
                                "local continuation {continuation} is bound more than once"
                            )));
                        }
                        inner.insert(continuation);
                    }
                    work.push((*body, inner.clone()));
                    for &continuation in continuations.iter().rev() {
                        work.push((self.continuation(continuation).unwrap().body, inner.clone()));
                    }
                }
                Node::ApplyFun { .. }
                | Node::ApplyCont(_)
                | Node::Switch { .. }
                | Node::Foreign { .. }
                | Node::Cell { .. }
                | Node::Channel { .. }
                | Node::Intrinsic { .. }
                | Node::Halt { .. }
                | Node::Panic(_)
                | Node::Unreachable => {}
            }
        }
        Ok(())
    }

    fn verify_node(
        &self,
        current_function: FunctionId,
        return_cont: ContinuationId,
        facts: &ReturnFacts<'_>,
        scope: &BTreeSet<ContinuationId>,
        id: NodeId,
        node: &Node,
    ) -> Result<(), VerifyError> {
        match node {
            Node::LetValue { result, next, .. } => {
                self.require_value(*result, "let-value result")?;
                self.require_node(*next, "let-value successor")?;
            }
            Node::LetIntrinsic {
                result,
                op,
                args,
                next,
            } => {
                self.require_value(*result, "let-intrinsic result")?;
                self.require_node(*next, "let-intrinsic successor")?;
                if args.len() != op.arity() {
                    return Err(VerifyError(format!(
                        "{id} intrinsic {op:?} expects {} operands, got {}",
                        op.arity(),
                        args.len()
                    )));
                }
            }
            Node::LetFun { functions, body } => {
                for &function in functions {
                    self.require_fun(function, "let-fun member")?;
                }
                self.require_node(*body, "let-fun body")?;
            }
            Node::LetCont {
                continuations,
                body,
            } => {
                for &continuation in continuations {
                    self.require_cont(continuation, "let-cont member")?;
                }
                self.require_node(*body, "let-cont body")?;
            }
            Node::ApplyFun {
                callee,
                args,
                return_to,
            } => {
                match callee {
                    Callee::Known(function) => {
                        self.require_fun(*function, "known callee")?;
                        let arity = self.function(*function).unwrap().params.len();
                        if arity != args.len() {
                            return Err(VerifyError(format!(
                                "{id} calls {function} with {} arguments; expected {arity}",
                                args.len()
                            )));
                        }
                    }
                    Callee::Closure(value) => self.require_value(*value, "closure callee")?,
                }
                // A closure is reached through the shared type of its arity, which carries one result whatever the function behind it returns.
                let results = match callee {
                    Callee::Known(function) => facts.arity(*function),
                    Callee::Closure(_) => 1,
                };
                let params = self.continuation_arity(
                    current_function,
                    return_cont,
                    facts,
                    scope,
                    *return_to,
                )?;
                if params != results {
                    return Err(VerifyError(format!(
                        "{id} user call return continuation {return_to} accepts {params} values, callee returns {results}"
                    )));
                }
            }
            Node::ApplyCont(edge) => {
                self.verify_edge(current_function, return_cont, facts, scope, id, edge)?
            }
            Node::Switch { cases, default, .. } => {
                for edge in cases.values() {
                    self.verify_edge(current_function, return_cont, facts, scope, id, edge)?;
                }
                if let Some(edge) = default {
                    self.verify_edge(current_function, return_cont, facts, scope, id, edge)?;
                }
            }
            Node::Foreign {
                function,
                args,
                return_to,
            } => {
                if args.len() != function.signature().params.len() {
                    return Err(VerifyError(format!(
                        "{id} foreign call expects {} operands, got {}",
                        function.signature().params.len(),
                        args.len()
                    )));
                }
                let results = function.signature().results.len();
                let params = self.continuation_arity(
                    current_function,
                    return_cont,
                    facts,
                    scope,
                    *return_to,
                )?;
                if results != params {
                    return Err(VerifyError(format!(
                        "{id} foreign return continuation expects {params} values, call returns {results}"
                    )));
                }
            }
            Node::Cell {
                op,
                args,
                return_to,
            } => {
                if args.len() != op.operand_arity() {
                    return Err(VerifyError(format!(
                        "{id} cell {op:?} expects {} operands, got {}",
                        op.operand_arity(),
                        args.len()
                    )));
                }
                if self.continuation_arity(
                    current_function,
                    return_cont,
                    facts,
                    scope,
                    *return_to,
                )? != op.result_arity()
                {
                    return Err(VerifyError(format!(
                        "{id} cell {op:?} continuation arity mismatch"
                    )));
                }
            }
            Node::Channel {
                op,
                args,
                return_to,
            } => {
                if args.len() != op.operand_arity() {
                    return Err(VerifyError(format!(
                        "{id} channel {op:?} expects {} operands, got {}",
                        op.operand_arity(),
                        args.len()
                    )));
                }
                if self.continuation_arity(
                    current_function,
                    return_cont,
                    facts,
                    scope,
                    *return_to,
                )? != op.result_arity()
                {
                    return Err(VerifyError(format!(
                        "{id} channel {op:?} continuation arity mismatch"
                    )));
                }
            }
            Node::Intrinsic {
                op: IntrinsicCall::ListMap,
                args,
                return_to,
            } => {
                if args.len() != 2 {
                    return Err(VerifyError(format!(
                        "{id} ListMap expects two operands, got {}",
                        args.len()
                    )));
                }
                if self.continuation_arity(
                    current_function,
                    return_cont,
                    facts,
                    scope,
                    *return_to,
                )? != 1
                {
                    return Err(VerifyError(format!(
                        "{id} ListMap continuation must accept one value"
                    )));
                }
            }
            Node::Halt { function, args } => {
                if !function.diverges() {
                    return Err(VerifyError(format!(
                        "{id} halts through {}, which returns",
                        function.name()
                    )));
                }
                if args.len() != function.signature().params.len() {
                    return Err(VerifyError(format!(
                        "{id} halting call expects {} operands, got {}",
                        function.signature().params.len(),
                        args.len()
                    )));
                }
            }
            Node::Panic(_) | Node::Unreachable => {}
        }

        for atom in atoms(node) {
            match atom {
                Atom::Value(value) => {
                    // Naming the referencing statement turns a dangling-operand refusal from a value id into a site: which node, and — through its spelled form — which rewrite left it behind.
                    self.require_value(*value, &format!("statement {id} ({node:?}) operand"))?
                }
                Atom::Fun(function) => self.require_fun(*function, "function atom")?,
                Atom::Literal(_) | Atom::Filler => {}
            }
        }
        Ok(())
    }

    /// Check one transfer's argument count against its target's. A return edge is covered by the same rule: a transfer to the enclosing function's own return continuation carries its return arity, read off [`Module::return_arities`] — so an edge that disagrees with its siblings is reported here rather than reaching the emitter.
    fn verify_edge(
        &self,
        function: FunctionId,
        return_cont: ContinuationId,
        facts: &ReturnFacts<'_>,
        scope: &BTreeSet<ContinuationId>,
        owner: NodeId,
        edge: &Edge,
    ) -> Result<(), VerifyError> {
        let arity = self.continuation_arity(function, return_cont, facts, scope, edge.target)?;
        if arity != edge.args.len() {
            return Err(VerifyError(format!(
                "{owner} edge to {} carries {} arguments; expected {arity}",
                edge.target,
                edge.args.len()
            )));
        }
        Ok(())
    }

    /// How many values a transfer to `target` carries; a transfer to the enclosing function's own return continuation carries its return.
    fn continuation_arity(
        &self,
        function: FunctionId,
        return_cont: ContinuationId,
        facts: &ReturnFacts<'_>,
        scope: &BTreeSet<ContinuationId>,
        target: ContinuationId,
    ) -> Result<usize, VerifyError> {
        if target == return_cont {
            return Ok(facts.arity(function));
        }
        if let Some(owner) = facts.owners.get(&target) {
            return Err(VerifyError(format!(
                "{function} references {owner}'s return continuation {target}"
            )));
        }
        if !scope.contains(&target) {
            return Err(VerifyError(format!(
                "{function} references undefined or out-of-scope continuation {target}"
            )));
        }
        self.continuation(target)
            .map(|continuation| continuation.params.len())
            .ok_or_else(|| VerifyError(format!("undefined non-return continuation {target}")))
    }

    fn require_node(&self, id: NodeId, what: &str) -> Result<(), VerifyError> {
        self.node(id)
            .map(|_| ())
            .ok_or_else(|| VerifyError(format!("{what} references missing {id}")))
    }

    fn require_value(&self, id: ValueId, what: &str) -> Result<(), VerifyError> {
        self.value(id)
            .map(|_| ())
            .ok_or_else(|| VerifyError(format!("{what} references missing {id}")))
    }

    fn require_fun(&self, id: FunctionId, what: &str) -> Result<(), VerifyError> {
        self.function(id)
            .map(|_| ())
            .ok_or_else(|| VerifyError(format!("{what} references missing {id}")))
    }

    fn require_cont(&self, id: ContinuationId, what: &str) -> Result<(), VerifyError> {
        self.continuation(id)
            .map(|_| ())
            .ok_or_else(|| VerifyError(format!("{what} references missing {id}")))
    }
}
