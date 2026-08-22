//! What an initializer evaluates through the functions it applies — the half of recursion admission that looks through a call.
//!
//! An initializer that applies a function directly evaluates that function's body before its own result exists, so a computed member the body reads is evaluated by the initializer, and so is one read by a function the body applies, and so on. Each function reachable that way is summarized once, to a fixed point, as what its eager region *reads* and which *values* it applies. An applied value that is one of the function's own parameters names the argument a caller hands there; one the function captures names a value of the scope that binds it, and propagates up to that scope. That is what makes a higher-order combinator legible: `List/fold(xs, zero, step)` applies its `step` — through the `go` it nests, which captures `step` and applies it — so a function atom handed there contributes its reads to the initializer, while `Parse/bind(p, f)` only stores `f` inside the parser it builds, so the same atom there contributes nothing. That is the line between the corpus's parser knots and the eager fold that once read an unfilled cell.
//!
//! What the summary cannot see, it treats as dormant — the same line [`check_atom_at`](super::Verifier::check_atom_at) draws for a function the initializer merely constructs. A callee bound in its own region by anything but an alias is a closure whose function is unknown: a projected field, a returned closure, a cell's contents. Before optimization every `!` is one — `Monad/bind(witness)` projects the method and the initializer applies it — so refusing the opaque case would refuse every monadic initializer in `/std`, and admitting it is what keeps a parser knot legal. The gap that leaves is an initializer applying such a closure *at once* over a later member, `Parse/run(p, input)` with `p` built over one; closing it needs flow through products and returns, which is a closure analysis this is not. Reads carry the callee they were composed through, so a refusal names the call that evaluates the member.

use {
    super::{
        Atom, BlockId, FunctionId, Intrinsic, Module, Rhs, Statement, ValueId, VerifyError,
        spell_function, spell_value,
    },
    crate::RecValue,
    std::collections::{BTreeMap, BTreeSet},
};

/// What evaluating a region does to a knot under construction: the values it reads — its own operands, and those of everything it applies, each composed read tagged with the callee it came through — and the values it applies that it does not bind itself: its parameters, or values it captures.
#[derive(Clone, PartialEq, Eq, Default)]
struct Evaluation {
    direct: BTreeSet<ValueId>,
    composed: BTreeMap<ValueId, FunctionId>,
    applies: BTreeSet<ValueId>,
}

/// Check every computed member's initializer of one group against what it evaluates through calls.
pub(super) fn check_group(module: &Module, values: &[RecValue]) -> Result<(), VerifyError> {
    let computed: Vec<ValueId> = values.iter().map(|member| member.value).collect();
    let mut summaries = Summaries::default();
    for (index, member) in values.iter().enumerate() {
        let evaluation = summaries.settled_region(module, member.init);
        // The initializer's own reads are `check_atom_at`'s to judge, with its self-knot exemption; a read through a call runs the callee to completion inside the initializer, so the member itself is as unsatisfiable as a later one.
        for (value, callee) in evaluation.composed {
            if let Some(position) = computed.iter().position(|&member| member == value)
                && position >= index
            {
                return Err(VerifyError(format!(
                    "the initializer of computed group member {} evaluates {} before its \
                     initialization, through a call to {}",
                    spell_value(module, member.value),
                    spell_value(module, value),
                    spell_function(module, callee)
                )));
            }
        }
    }
    Ok(())
}

/// Per-function summaries over the functions some initializer reaches, settled together: a function enters at the bottom the first time a region applies it, and every entered function is re-summarized from the others until none changes. The lattice is finite — reads and applied values over the module's values — and re-summarizing only grows a summary, so the settling terminates. Settling the set as a whole, rather than each function on entry, is what makes mutual recursion sound: a callee summarized against its caller's seed alone would never see the caller's settled reads.
#[derive(Default)]
struct Summaries {
    functions: BTreeMap<FunctionId, Evaluation>,
    /// Bumped whenever a summary enters or grows, so a driver can tell a settled map from one it has to re-read a region against.
    version: usize,
}

/// One region being evaluated: what it binds, the atoms its aliases name, and the functions already composed into it — composing one is idempotent, so a function reached twice, or reaching itself through a capture, is composed once.
struct Scope<'a> {
    params: &'a [ValueId],
    bound: BTreeSet<ValueId>,
    aliases: BTreeMap<ValueId, Atom>,
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
                            }
                        }
                        self.application(module, rhs, &mut scope, &mut evaluation);
                        blocks.extend(rhs.sub_blocks());
                    }
                    Some(Statement::Rec { group }) => {
                        if let Some(group) = module.rec_group(*group) {
                            scope
                                .bound
                                .extend(group.values.iter().map(|member| member.value));
                            blocks.extend(group.values.iter().map(|member| member.init));
                        }
                    }
                    Some(Statement::Functions { .. }) | None => {}
                }
            }
            if let Some(Atom::Value(value)) = block.terminator.atom() {
                evaluation.direct.insert(value);
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
