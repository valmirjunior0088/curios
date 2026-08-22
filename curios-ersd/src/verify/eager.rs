//! What an initializer evaluates, and what it performs — the two rules a knot forced by need rests on.
//!
//! A member is computed the first time something reads it, so the order members are written in decides nothing; what no forcing can satisfy is an initializer that evaluates *itself*, directly or through the functions it applies, which is a cycle in the graph of what each initializer reads. And forcing later, or never, is unobservable only if an initializer performs no effect, so one that reaches a host call, a cell, or an exit is refused — a trap or divergence it only delays, which is what by need means, and those are admitted. Both facts are read the same way: an initializer that applies a function directly evaluates that function's body, so what the body reads and performs is the initializer's, and so is what a function the body applies reads and performs, and so on. Each function reachable that way is summarized once, to a fixed point, as what its eager region *reads*, what it *performs*, and which *values* it applies. An applied value that is one of the function's own parameters names the argument a caller hands there; one the function captures names a value of the scope that binds it, and propagates up to that scope. That is what makes a higher-order combinator legible: `List/fold(xs, zero, step)` applies its `step` — through the `go` it nests, which captures `step` and applies it — so a function atom handed there contributes its reads to the initializer, while `Parse/bind(p, f)` only stores `f` inside the parser it builds, so the same atom there contributes nothing. That is the line between the corpus's parser knots and a fold over a member that reads itself.
//!
//! What the summary cannot see, it treats as dormant — the same line [`check_atom_at`](super::Verifier::check_atom_at) draws for a function the initializer merely constructs. A callee bound in its own region by anything but an alias is a closure whose function is unknown: a projected field, a returned closure, a cell's contents. Before optimization every `!` is one — `Monad/bind(witness)` projects the method and the initializer applies it — so refusing the opaque case would refuse every monadic initializer in `/std`, and admitting it is what keeps a parser knot legal. The gap that leaves is a cycle or an effect reached only through such a closure — `Parse/run(p, input)` with `p` built over the member being initialized — which forcing then meets at runtime, as the trap on a member read while its own initializer runs; closing it needs flow through products and returns, which is a closure analysis this is not. Reads and effects carry the callee they were composed through, so a refusal names the call.

use {
    super::{
        Atom, BlockId, FunctionId, Intrinsic, Module, Rhs, Statement, ValueId, VerifyError,
        spell_function, spell_value,
    },
    crate::{LocalBehavior, RecValue, Semantics},
    std::collections::{BTreeMap, BTreeSet},
};

/// What evaluating a region does to a knot under construction: the values it reads — its own operands, and those of everything it applies, each composed read tagged with the callee it came through — the effect it performs, if any, tagged the same way, and the values it applies that it does not bind itself: its parameters, or values it captures.
#[derive(Clone, PartialEq, Eq, Default)]
struct Evaluation {
    direct: BTreeSet<ValueId>,
    composed: BTreeMap<ValueId, FunctionId>,
    effect: Option<Option<FunctionId>>,
    applies: BTreeSet<ValueId>,
}

/// Whether a behaviour is one forcing cannot move: a host call, a cell read or write, or an exit. A trap and divergence are only delayed by forcing later, and never happen if nothing forces — which is what the language means by a recursive value.
fn performs(behavior: &LocalBehavior) -> bool {
    let observable = behavior.observable;
    observable.host_effect || observable.state_read || observable.state_write || observable.may_exit
}

/// Check one group's computed members: no initializer evaluates itself, and none performs an effect.
pub(super) fn check_group(module: &Module, values: &[RecValue]) -> Result<(), VerifyError> {
    let computed: Vec<ValueId> = values.iter().map(|member| member.value).collect();
    let mut summaries = Summaries::default();
    // Each member's evaluation edges to the members it reads, with the callee a read came through.
    let mut evaluates: Vec<BTreeMap<usize, Option<FunctionId>>> = Vec::with_capacity(values.len());
    for member in values {
        let evaluation = summaries.settled_region(module, member.init);
        if let Some(through) = evaluation.effect {
            return Err(VerifyError(format!(
                "the initializer of computed group member {} performs an effect{}, which \
                 forcing it by need could not keep in its place",
                spell_value(module, member.value),
                match through {
                    Some(callee) =>
                        format!(" through a call to {}", spell_function(module, callee)),
                    None => String::new(),
                }
            )));
        }
        let mut edges = BTreeMap::new();
        for (value, callee) in evaluation.composed {
            if let Some(position) = computed.iter().position(|&member| member == value) {
                edges.entry(position).or_insert(Some(callee));
            }
        }
        // A direct self-reference is the language's `rec loop = loop`: admitted, dropped when unused, and a trap on the cycle when forced. Every other direct read is an edge.
        for value in evaluation.direct {
            if let Some(position) = computed.iter().position(|&member| member == value)
                && computed[position] != member.value
            {
                edges.entry(position).or_insert(None);
            }
        }
        evaluates.push(edges);
    }

    // A cycle in that graph is an initializer evaluating itself: refuse it, naming the path.
    let mut state = vec![Visit::Unseen; values.len()];
    let mut path = Vec::new();
    for start in 0..values.len() {
        if let Some(cycle) = cycle_from(start, &evaluates, &mut state, &mut path) {
            let mut spelled = String::new();
            for window in cycle.windows(2) {
                let (from, to) = (window[0], window[1]);
                spelled.push_str(&spell_value(module, computed[from]));
                spelled.push_str(match evaluates[from][&to] {
                    Some(_) => ", through a call, evaluates ",
                    None => " evaluates ",
                });
            }
            spelled.push_str(&spell_value(module, computed[*cycle.last().unwrap()]));
            return Err(VerifyError(format!(
                "computed group members evaluate each other, which no forcing order can \
                 satisfy: {spelled}"
            )));
        }
    }
    Ok(())
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Visit {
    Unseen,
    Open,
    Done,
}

/// Depth-first from `start`; on a back edge, the cycle as the path from its target back round to it.
fn cycle_from(
    start: usize,
    evaluates: &[BTreeMap<usize, Option<FunctionId>>],
    state: &mut [Visit],
    path: &mut Vec<usize>,
) -> Option<Vec<usize>> {
    if state[start] == Visit::Done {
        return None;
    }
    state[start] = Visit::Open;
    path.push(start);
    for &next in evaluates[start].keys() {
        match state[next] {
            Visit::Open => {
                let from = path.iter().position(|&member| member == next).unwrap();
                let mut cycle = path[from..].to_vec();
                cycle.push(next);
                return Some(cycle);
            }
            Visit::Unseen => {
                if let Some(cycle) = cycle_from(next, evaluates, state, path) {
                    return Some(cycle);
                }
            }
            Visit::Done => {}
        }
    }
    path.pop();
    state[start] = Visit::Done;
    None
}

/// Per-function summaries over the functions some initializer reaches, settled together: a function enters at the bottom the first time a region applies it, and every entered function is re-summarized from the others until none changes. The lattice is finite — reads and applied values over the module's values — and re-summarizing only grows a summary, so the settling terminates. Settling the set as a whole, rather than each function on entry, is what makes mutual recursion sound: a callee summarized against its caller's seed alone would never see the caller's settled reads.
#[derive(Default)]
struct Summaries {
    functions: BTreeMap<FunctionId, Evaluation>,
    /// Bumped whenever a summary enters or grows, so a driver can tell a settled map from one it has to re-read a region against.
    version: usize,
}

/// One region being evaluated: what it binds, the atoms its aliases name, the nested members a read would force, and the functions already composed into it — composing one is idempotent, so a function reached twice, or reaching itself through a capture, is composed once.
struct Scope<'a> {
    params: &'a [ValueId],
    bound: BTreeSet<ValueId>,
    aliases: BTreeMap<ValueId, Atom>,
    /// A nested group's computed members not yet read here, each with the initializer a read would force.
    nested: BTreeMap<ValueId, BlockId>,
    evaluated: BTreeSet<FunctionId>,
}

impl Scope<'_> {
    /// Follow a local alias chain to the atom it names.
    fn resolve(&self, atom: Atom) -> Atom {
        let mut atom = atom;
        let mut seen = BTreeSet::new();
        while let Atom::Value(value) = atom
            && seen.insert(value)
            && let Some(&aliased) = self.aliases.get(&value)
        {
            atom = aliased;
        }
        atom
    }

    /// Whether `value` comes from outside this region — a parameter or a capture — rather than being bound inside it.
    fn is_outer(&self, value: ValueId) -> bool {
        self.params.contains(&value) || !self.bound.contains(&value)
    }
}

impl Summaries {
    /// Evaluate an initializer's region against settled summaries: the region enters the functions it applies, settling summarizes them, and the region is read again until neither changes anything.
    fn settled_region(&mut self, module: &Module, block: BlockId) -> Evaluation {
        loop {
            let version = self.version;
            let evaluation = self.region(module, block, &[]);
            self.settle(module);
            if self.version == version {
                return evaluation;
            }
        }
    }

    /// Re-summarize every entered function from the others until none changes.
    fn settle(&mut self, module: &Module) {
        loop {
            let version = self.version;
            for function in self.functions.keys().copied().collect::<Vec<_>>() {
                let Some(definition) = module.function(function) else {
                    continue;
                };
                let params = definition.params.clone();
                let evaluation = self.region(module, definition.body, &params);
                if self.functions[&function] != evaluation {
                    self.functions.insert(function, evaluation);
                    self.version += 1;
                }
            }
            if self.version == version {
                return;
            }
        }
    }

    /// The summary of `function` as settled so far — the bottom, on its first mention.
    fn function(&mut self, function: FunctionId) -> Evaluation {
        if let Some(summary) = self.functions.get(&function) {
            return summary.clone();
        }
        self.functions.insert(function, Evaluation::default());
        self.version += 1;
        Evaluation::default()
    }

    /// Evaluate one eager region — a function body against its `params`, or an initializer against none — descending through control sub-blocks and nested groups' initializers, never into the body of a function the region merely constructs.
    fn region(&mut self, module: &Module, block: BlockId, params: &[ValueId]) -> Evaluation {
        let mut evaluation = Evaluation::default();
        let mut scope = Scope {
            params,
            bound: BTreeSet::new(),
            aliases: BTreeMap::new(),
            nested: BTreeMap::new(),
            evaluated: BTreeSet::new(),
        };
        let mut blocks = vec![block];
        while let Some(block) = blocks.pop() {
            let Some(block) = module.block(block) else {
                continue;
            };
            for &statement in &block.statements {
                match module.statement(statement) {
                    Some(Statement::Let { result, rhs }) => {
                        scope.bound.insert(*result);
                        scope.bound.extend(rhs.binders());
                        if let Rhs::Alias(atom) = rhs {
                            let atom = scope.resolve(*atom);
                            scope.aliases.insert(*result, atom);
                        }
                        for atom in rhs.operands() {
                            if let Atom::Value(value) = atom {
                                evaluation.direct.insert(value);
                                if let Some(init) = scope.nested.remove(&value) {
                                    blocks.push(init);
                                }
                            }
                        }
                        if performs(&Semantics::local_behavior(rhs)) {
                            evaluation.effect.get_or_insert(None);
                        }
                        self.application(module, rhs, &mut scope, &mut evaluation);
                        blocks.extend(rhs.sub_blocks());
                    }
                    // A nested group's members are forced by need like the outer one's: an initializer is read into this region the first time the region reads its member, not merely because the group is bound here.
                    Some(Statement::Rec { group }) => {
                        if let Some(group) = module.rec_group(*group) {
                            for member in &group.values {
                                scope.bound.insert(member.value);
                                scope.nested.insert(member.value, member.init);
                            }
                        }
                    }
                    Some(Statement::Functions { .. }) | None => {}
                }
            }
            if let Some(Atom::Value(value)) = block.terminator.atom() {
                evaluation.direct.insert(value);
                if let Some(init) = scope.nested.remove(&value) {
                    blocks.push(init);
                }
            }
            if Semantics::terminator(&block.terminator).may_exit {
                evaluation.effect.get_or_insert(None);
            }
        }
        evaluation
    }

    /// What a right-hand side applies: a direct call's callee with its arguments, or the mapper `ListMap` applies to every element.
    fn application(
        &mut self,
        module: &Module,
        rhs: &Rhs,
        scope: &mut Scope<'_>,
        evaluation: &mut Evaluation,
    ) {
        match rhs {
            Rhs::Apply { callee, arguments } => {
                let callee = scope.resolve(*callee);
                let arguments: Vec<Atom> = arguments
                    .iter()
                    .map(|&argument| scope.resolve(argument))
                    .collect();
                self.apply(module, callee, &arguments, scope, evaluation);
            }
            Rhs::Intrinsic {
                intrinsic: Intrinsic::ListMap,
                operands,
            } => {
                if let [_, mapper] = operands[..] {
                    let mapper = scope.resolve(mapper);
                    self.handed(module, mapper, scope, evaluation);
                }
            }
            _ => {}
        }
    }

    /// Apply `callee` to `arguments` inside the region being evaluated.
    fn apply(
        &mut self,
        module: &Module,
        callee: Atom,
        arguments: &[Atom],
        scope: &mut Scope<'_>,
        evaluation: &mut Evaluation,
    ) {
        match callee {
            Atom::Function(function) => {
                self.evaluated(module, function, arguments, scope, evaluation)
            }
            // A parameter's or a capture's body is the scope that binds it: it is applied here, and what the call passes along may be applied by it, so that is handed the same way.
            Atom::Value(value) if scope.is_outer(value) => {
                evaluation.applies.insert(value);
                for &argument in arguments {
                    self.handed(module, argument, scope, evaluation);
                }
            }
            // A closure whose function cannot be seen: dormant, the limit the module documentation states.
            Atom::Value(_) | Atom::Constant(_) => {}
        }
    }

    /// `function` runs inside the region, applied to `arguments` — or to arguments the callee chooses, unseen, when `arguments` is empty because the atom was handed rather than called. Its reads arrive here; each value it applies is the argument at that parameter's position, or a capture resolved in this scope.
    fn evaluated(
        &mut self,
        module: &Module,
        function: FunctionId,
        arguments: &[Atom],
        scope: &mut Scope<'_>,
        evaluation: &mut Evaluation,
    ) {
        if !scope.evaluated.insert(function) {
            return;
        }
        let summary = self.function(function);
        for &value in summary.direct.iter().chain(summary.composed.keys()) {
            evaluation.composed.entry(value).or_insert(function);
        }
        if summary.effect.is_some() {
            evaluation.effect.get_or_insert(Some(function));
        }
        let params = module
            .function(function)
            .map(|definition| definition.params.clone())
            .unwrap_or_default();
        for applied in summary.applies {
            match params.iter().position(|&param| param == applied) {
                Some(position) => {
                    if let Some(&argument) = arguments.get(position) {
                        self.handed(module, argument, scope, evaluation);
                    }
                }
                None => self.applied_capture(module, applied, scope, evaluation),
            }
        }
    }

    /// A function the region evaluates applies `value`, which it captured: it is whatever this scope calls that name.
    fn applied_capture(
        &mut self,
        module: &Module,
        value: ValueId,
        scope: &mut Scope<'_>,
        evaluation: &mut Evaluation,
    ) {
        match scope.resolve(Atom::Value(value)) {
            Atom::Function(function) => self.evaluated(module, function, &[], scope, evaluation),
            Atom::Value(value) if scope.is_outer(value) => {
                evaluation.applies.insert(value);
            }
            Atom::Value(_) | Atom::Constant(_) => {}
        }
    }

    /// `atom` reaches a position that applies it: a function atom is evaluated here, with arguments the callee chooses; a parameter or capture is thereby applied; anything else is a constant or a closure whose function cannot be seen.
    fn handed(
        &mut self,
        module: &Module,
        atom: Atom,
        scope: &mut Scope<'_>,
        evaluation: &mut Evaluation,
    ) {
        match atom {
            Atom::Function(function) => self.evaluated(module, function, &[], scope, evaluation),
            Atom::Value(value) if scope.is_outer(value) => {
                evaluation.applies.insert(value);
            }
            Atom::Value(_) | Atom::Constant(_) => {}
        }
    }
}
