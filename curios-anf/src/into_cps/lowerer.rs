//! The walk that decides what to emit: one pass over the erased arena, statement by statement and block by block, in continuation-passing order.
//!
//! Every method here answers "what does this construct become", and hands the answer to the [`Emitter`] that writes it or the [`Layout`](super::Layout) that shapes it. The split is what keeps this file about control flow alone — nothing below mints a CPS value or lays out a row, and nothing there descends into a block.

use {
    super::{
        Analysis, Atom, Block, BlockId, ConstructorId, Emitter, FamilyEncoding, FamilyId, Function,
        FunctionId, KnotMember, Module, Operation, RecGroup, RecGroupId, Rhs, SequenceFacts,
        SequenceGrain, SequenceOp, Statement, StatementId, Terminator, UnconsSequenceStep, ValueId,
        VariantArm, cell_op, operation_intrinsic, sequence_census, sequence_intrinsic,
        sequence_len_op,
    },
    crate::{
        CellOperation, ChannelOperation, Intrinsic, Keys, SuspensionOperation, droppable_functions,
    },
    curios_abi::ForeignFunction,
    curios_common::recurse,
    curios_num::Natural,
    std::{
        collections::{BTreeMap, BTreeSet},
        sync::Arc,
    },
};

pub struct Lowerer<'a> {
    source: &'a Module,
    /// Whether a call of each source function, its result unread, may simply not happen, and given which of the values it applies — the conjunction of the carried termination verdict and this stage's effect summary, which is the only form of either fact that crosses into CPS.
    droppable: BTreeMap<FunctionId, Option<BTreeSet<ValueId>>>,
    /// Use counts over the finished arena, read only to decline emitting a binding nothing reads. The module does not change during lowering, so one analysis taken at entry stays exact throughout.
    analysis: Analysis,
    /// The sequence-usage census's verdicts, read at every construction site to settle the stores into indexed-only fields.
    facts: SequenceFacts,
    /// The CPS module being built, and what each erased name has become in it.
    emitter: Emitter<'a>,
    /// The step every bind's suspension arm calls, built at the first and bound at the top of the entry.
    carry: Option<curios_cps::FunctionId>,
    /// The step every host call answered later suspends through, built and bound as `carry` is.
    later: Option<curios_cps::FunctionId>,
    /// The top-level statements lowered by need ([`lowered_by_need`]).
    by_need: BTreeSet<StatementId>,
}

/// What runs when a knot's member is first forced.
#[derive(Clone, Copy)]
enum Initializer {
    /// A recursive group's computed member: its block.
    Block(BlockId),
    /// A top-level statement lowered by need: the statement, which binds the member it computes.
    Statement(StatementId),
}

/// The top-level statements a program computes where it first reads them: each `let` that computes ([`Rhs::computes`]), and each that reads a value one before it binds or a top-level group's computed member, which binding it where it stands would force. Every other is bound where it stands: an alias or a construction over what is bound ahead can neither diverge nor trap, so no program tells it from one computed by need.
fn lowered_by_need(source: &Module) -> BTreeSet<StatementId> {
    let mut by_need = BTreeSet::new();
    let mut needed = BTreeSet::new();
    for &item in source.items() {
        match source.statement(item) {
            Some(Statement::Let { result, rhs }) => {
                let reads_needed = rhs
                    .operands()
                    .iter()
                    .any(|atom| matches!(atom, Atom::Value(value) if needed.contains(value)));
                if rhs.computes() || reads_needed {
                    by_need.insert(item);
                    needed.insert(*result);
                }
            }
            Some(Statement::Rec { group }) => {
                if let Some(group) = source.rec_group(*group) {
                    needed.extend(group.values.iter().map(|member| member.value));
                }
            }
            Some(Statement::Functions { .. }) | None => {}
        }
    }
    by_need
}

/// Where a `let` stands in its block: the value it binds, and what follows it — the statements after it, the terminator closing the block and the continuation the block delivers to. What a binding becomes is read off all four together: one nothing follows but its own return delivers straight to the target, and every other opens a continuation for the rest.
#[derive(Clone, Copy)]
struct Site<'a> {
    result: ValueId,
    rest: &'a [StatementId],
    terminator: &'a Terminator,
    target: curios_cps::ContinuationId,
}

impl<'a> Lowerer<'a> {
    /// Open a lowering over a verified arena module: the two analyses it consults are taken once here, because the module does not change while it runs.
    pub fn new(source: &'a Module) -> Self {
        Self {
            source,
            droppable: droppable_functions(source),
            analysis: Analysis::analyze(source),
            facts: sequence_census(source),
            emitter: Emitter::new(source),
            carry: None,
            later: None,
            by_need: lowered_by_need(source),
        }
    }

    /// Run the lowering and hand over the CPS module it built.
    ///
    /// The arena's top level — its item chain followed by its entry block — becomes the parameterless entry `main`, delivering to a bodyless `return_cont`. The result is verified before it leaves: an invalid module here is a lowering bug, not a user error.
    pub fn finish(mut self) -> curios_cps::Module {
        let main = self.emitter.reserve_function();
        let return_cont = self.emitter.reserve_continuation();
        let entry = self
            .source
            .entry()
            .expect("a finalized module has an entry");
        let entry = self.source.block(entry).expect("live entry block").clone();
        let mut statements: Vec<StatementId> = self.source.items().to_vec();
        statements.extend(&entry.statements);
        let mut body = self.lower_statements(&statements, &entry.terminator, return_cont);
        // Each closes over nothing, so binding it ahead of everything puts it in scope of every bind and every call.
        for step in [self.carry, self.later].into_iter().flatten() {
            body = self.emitter.add_node(curios_cps::Node::LetFun {
                functions: vec![step],
                body,
            });
        }
        self.emitter.define_function(
            main,
            curios_cps::Function {
                lineage: None,
                debug_name: Some("main".into()),
                params: Vec::new(),
                return_cont,
                body,
                // The entry is the program; nothing calls it, so whether it could be dropped is not a question.
                droppable: curios_cps::Droppable::Never,
                terminates: false,
            },
        );
        self.emitter.set_entry(main);

        let module = self.emitter.into_module();
        module
            .verify()
            .unwrap_or_else(|error| panic!("arena lowering produced invalid CPS: {error}"));
        module
    }
}

impl Lowerer<'_> {
    /// Lower a block so its result is delivered to `target`.
    fn lower_block(
        &mut self,
        block: BlockId,
        target: curios_cps::ContinuationId,
    ) -> curios_cps::NodeId {
        let block: Block = self.source.block(block).expect("live block").clone();
        self.lower_statements(&block.statements, &block.terminator, target)
    }

    /// Lower a statement suffix closed by `terminator`, delivering to `target`.
    pub fn lower_statements(
        &mut self,
        statements: &[StatementId],
        terminator: &Terminator,
        target: curios_cps::ContinuationId,
    ) -> curios_cps::NodeId {
        recurse(|| self.lower_statements_within(statements, terminator, target))
    }

    fn lower_statements_within(
        &mut self,
        statements: &[StatementId],
        terminator: &Terminator,
        target: curios_cps::ContinuationId,
    ) -> curios_cps::NodeId {
        // A statement forces the members it reads that its path has not, right before it runs: a member is computed where it is read, so a path that does not read it does not compute it.
        let Some((&first, rest)) = statements.split_first() else {
            let members = self.emitter.unforced_members(terminator.atoms());
            return self.with_cell_reads(members, |lowerer| {
                lowerer.lower_terminator(terminator, target)
            });
        };
        if self.by_need.contains(&first)
            && let Some(&Statement::Let { result, .. }) = self.source.statement(first)
        {
            return self.lower_knot(
                &[],
                &[(result, Initializer::Statement(first))],
                rest,
                terminator,
                target,
            );
        }
        self.lower_statement(first, rest, terminator, target)
    }

    /// Lower `statement` where it stands, forcing first each member it reads that its path has not, and then `rest`.
    fn lower_statement(
        &mut self,
        statement: StatementId,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cps::ContinuationId,
    ) -> curios_cps::NodeId {
        let statement = self
            .source
            .statement(statement)
            .expect("live statement")
            .clone();
        let members = match &statement {
            Statement::Let { rhs, .. } => self.emitter.unforced_members(rhs.operands()),
            Statement::Functions { .. } | Statement::Rec { .. } => Vec::new(),
        };
        self.with_cell_reads(members, |lowerer| match statement {
            Statement::Let { result, rhs } => lowerer.lower_let(
                &rhs,
                Site {
                    result,
                    rest,
                    terminator,
                    target,
                },
            ),
            Statement::Functions { functions } => {
                let functions = lowerer.lower_function_group(&functions);
                let body = lowerer.lower_statements(rest, terminator, target);
                lowerer
                    .emitter
                    .add_node(curios_cps::Node::LetFun { functions, body })
            }
            Statement::Rec { group } => lowerer.lower_rec_group(group, rest, terminator, target),
        })
    }

    fn lower_terminator(
        &mut self,
        terminator: &Terminator,
        target: curios_cps::ContinuationId,
    ) -> curios_cps::NodeId {
        match terminator {
            Terminator::Return(atom) => {
                let atom = self.emitter.lower_atom(*atom);
                self.emitter.jump(target, vec![atom])
            }
            Terminator::Halt { foreign, operands } => {
                let function = self
                    .source
                    .foreign(*foreign)
                    .expect("live foreign row")
                    .clone();
                let args = operands
                    .iter()
                    .map(|&atom| self.emitter.lower_atom(atom))
                    .collect();
                self.emitter
                    .add_node(curios_cps::Node::Halt { function, args })
            }
            Terminator::Unreachable => self.emitter.add_node(curios_cps::Node::Unreachable),
        }
    }

    /// What a call of `arena` must hand it to go, its result unread, over the values the lowering bound for what it applies: one lowered to no value of its own is one no call can hand, and the function is then kept.
    fn droppable(&self, arena: FunctionId) -> curios_cps::Droppable {
        let Some(Some(given)) = self.droppable.get(&arena) else {
            return curios_cps::Droppable::Never;
        };
        given
            .iter()
            .map(|&value| match self.emitter.lowered(value) {
                Some(curios_cps::Atom::Value(lowered)) => Some(*lowered),
                _ => None,
            })
            .collect::<Option<_>>()
            .map_or(curios_cps::Droppable::Never, curios_cps::Droppable::Given)
    }

    /// Reserve every function of a group before defining any, so a member body can reference itself and its siblings; return the CPS ids in group order.
    fn lower_function_group(&mut self, functions: &[FunctionId]) -> Vec<curios_cps::FunctionId> {
        let ids: Vec<curios_cps::FunctionId> = functions
            .iter()
            .map(|&arena| self.emitter.bind_function(arena))
            .collect();
        for (&arena, &id) in functions.iter().zip(&ids) {
            self.define_function(arena, id);
        }
        ids
    }

    /// Define a reserved CPS function from its arena function. A body that reads a knot's computed member forces it where it reads it, at call time, unless the path that binds the function forced it already, whose value the body then captures.
    fn define_function(&mut self, arena: FunctionId, id: curios_cps::FunctionId) {
        let function: Function = self.source.function(arena).expect("live function").clone();
        let return_cont = self.emitter.reserve_continuation();
        let params = function
            .params
            .iter()
            .map(|&param| self.emitter.bind_value(param))
            .collect();
        let lowering = self.emitter.lowering(function.lineage);
        let body = self.lower_block(function.body, return_cont);
        self.emitter.lowering(lowering);
        self.emitter.define_function(
            id,
            curios_cps::Function {
                lineage: function.lineage,
                debug_name: function.debug_name.clone(),
                params,
                return_cont,
                body,
                droppable: self.droppable(arena),
                terminates: function.total,
            },
        );
    }

    /// Lower a recursive group by its shape: a function-only group is a plain `LetFun` (erasure emits `Functions` for those, so this is totality); any group with a computed member is a knot tied through cells (see [`Lowerer::lower_knot`]), its function members bound beside the cells.
    fn lower_rec_group(
        &mut self,
        group: RecGroupId,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cps::ContinuationId,
    ) -> curios_cps::NodeId {
        let mut group: RecGroup = self.source.rec_group(group).expect("live group").clone();

        // Drop computed members never referenced outside their own initializer — nothing would ever force them, and a self-knot `rec loop = loop` is legal exactly because its initializer never runs.
        group.values.retain(|member| {
            self.source
                .member_used_outside_init(member.value, member.init)
        });

        if group.values.is_empty() {
            let functions = self.lower_function_group(&group.functions);
            let body = self.lower_statements(rest, terminator, target);
            return self
                .emitter
                .add_node(curios_cps::Node::LetFun { functions, body });
        }

        let values: Vec<(ValueId, Initializer)> = group
            .values
            .iter()
            .map(|member| (member.value, Initializer::Block(member.init)))
            .collect();
        self.lower_knot(&group.functions, &values, rest, terminator, target)
    }

    /// Tie a knot by need: a recursive group's, `functions` beside its computed `values`, or a top-level statement's, a knot of its own. Every computed member gets an empty write-once result cell, a capacity-one initializer channel, and a forcing function. All storage is allocated before closures are bound, and all initializers are queued before any forcing read. The force polls the result, returns it if present, otherwise takes the initializer, runs it and fills the result. Both containers empty means reentrant forcing and reports `Cycle`. A read of a member calls its force right before the statement that reads it, on each path that reads it and no earlier on it (see [`Lowerer::with_cell_reads`]), so an arm that does not read a member does not compute it.
    ///
    /// Forcing on first use is what the language means by a recursive value and by a top-level one, and what the compile-time evaluator does for a closed knot and an item — `force_toplevel` treats either as a CAF with a cycle guard — so the erased program agrees with both on every forward reference, whatever the verifier can or cannot see through. What makes by need *sound* is that an initializer performs no effect, the rule the verifier holds a group's to and what a top-level value is: running it later, or not at all, is unobservable, and the only behaviours forcing can move are a trap and divergence, which it only delays.
    ///
    /// Function members are lowered before any member is forced, so each forces what it reads at its own read, as any function does (see [`Lowerer::define_function`]), and a member that forward-references a computed value is served by the cell however it is reached — called directly, escaped as a closure, or copied by a later pass into a closure the initializers build. That uniformity is the point: patching only *escaping* member closures at a ready point would miss a closure born inside an initializer that merely *calls* a member — `wrap((n) => helper(n))` beside `helper(n) = first(n)` — which would capture the computed value before it exists, where nothing below the CPS verifier's lexical scope rules could see it.
    fn lower_knot(
        &mut self,
        functions: &[FunctionId],
        values: &[(ValueId, Initializer)],
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cps::ContinuationId,
    ) -> curios_cps::NodeId {
        let members: Vec<KnotMember> = values
            .iter()
            .map(|&(value, _)| {
                let knot = KnotMember {
                    result: self.emitter.add_value(None),
                    initializer: self.emitter.add_value(None),
                    force: self.emitter.reserve_function(),
                };
                self.emitter.note_knot_member(value, knot);
                knot
            })
            .collect();
        for (&(value, _), knot) in values.iter().zip(&members) {
            self.emitter
                .define_force(*knot, self.emitter.arena_value_name(value));
        }

        // The members are in the map before any body below is lowered, so a read of a computed sibling — from a function member, a thunk, or the rest — forces it where it stands.
        let functions = self.lower_function_group(functions);
        let thunks: Vec<curios_cps::FunctionId> = values
            .iter()
            .map(|&(value, init)| self.define_thunk(value, init))
            .collect();

        let mut body = self.lower_statements(rest, terminator, target);

        // Publish all initializers before the body can force any member. Each channel is fresh, open, and has capacity one, so every push is taken.
        for (knot, thunk) in members.iter().zip(&thunks).rev() {
            let after_store = self.emitter.reserve_continuation();
            let status = self.emitter.add_value(None);
            self.emitter.define_continuation(
                after_store,
                curios_cps::Continuation {
                    lineage: self.emitter.lineage(),
                    debug_name: Some("knot/stored".into()),
                    params: vec![status],
                    body,
                    terminates: false,
                },
            );
            let store = self.emitter.add_node(curios_cps::Node::Channel {
                op: curios_cps::ChannelOp::Push,
                args: vec![
                    curios_cps::Atom::Value(knot.initializer),
                    curios_cps::Atom::Fun(*thunk),
                ],
                return_to: after_store,
            });
            body = self.emitter.add_node(curios_cps::Node::LetCont {
                continuations: vec![after_store],
                body: store,
            });
        }

        // Binding order, outside in: the cells and channels; the force functions, which capture only that storage; the function members, which call the force functions; the thunks, which call both.
        body = self.emitter.add_node(curios_cps::Node::LetFun {
            functions: thunks,
            body,
        });
        if !functions.is_empty() {
            body = self
                .emitter
                .add_node(curios_cps::Node::LetFun { functions, body });
        }
        body = self.emitter.add_node(curios_cps::Node::LetFun {
            functions: members.iter().map(|knot| knot.force).collect(),
            body,
        });
        for knot in members.iter().rev() {
            let channel_bound = self.emitter.reserve_continuation();
            self.emitter.define_continuation(
                channel_bound,
                curios_cps::Continuation {
                    lineage: self.emitter.lineage(),
                    debug_name: Some("knot/channel".into()),
                    params: vec![knot.initializer],
                    body,
                    terminates: false,
                },
            );
            let channel = self.emitter.add_node(curios_cps::Node::Channel {
                op: curios_cps::ChannelOp::New,
                args: vec![curios_cps::Atom::Literal(curios_cps::Literal::Nat(
                    Natural::from(1u32),
                ))],
                return_to: channel_bound,
            });
            body = self.emitter.add_node(curios_cps::Node::LetCont {
                continuations: vec![channel_bound],
                body: channel,
            });
            let bound = self.emitter.reserve_continuation();
            self.emitter.define_continuation(
                bound,
                curios_cps::Continuation {
                    lineage: self.emitter.lineage(),
                    debug_name: Some("knot/cell".into()),
                    params: vec![knot.result],
                    body,
                    terminates: false,
                },
            );
            let reserve = self.emitter.add_node(curios_cps::Node::Cell {
                op: curios_cps::CellOp::Reserve,
                args: Vec::new(),
                return_to: bound,
            });
            body = self.emitter.add_node(curios_cps::Node::LetCont {
                continuations: vec![bound],
                body: reserve,
            });
        }
        body
    }

    /// `value`'s initializer as a nullary function: what its channel holds until something forces it. It forces each member it reads where it reads it, so a member it depends on is computed when it is needed — the by-need order, found by running rather than computed ahead.
    fn define_thunk(&mut self, value: ValueId, init: Initializer) -> curios_cps::FunctionId {
        let thunk = self.emitter.reserve_function();
        let return_cont = self.emitter.reserve_continuation();
        let body = match init {
            Initializer::Block(block) => self.lower_block(block, return_cont),
            Initializer::Statement(statement) => {
                // The statement binds the member inside its initializer alone, and the binding goes after it, so every read outside forces.
                let body = self.lower_statement(
                    statement,
                    &[],
                    &Terminator::Return(Atom::Value(value)),
                    return_cont,
                );
                self.emitter.restore_value(value, None);
                body
            }
        };
        let hint = self.emitter.arena_value_name(value);
        self.emitter.define_function(
            thunk,
            curios_cps::Function {
                lineage: self.emitter.lineage(),
                debug_name: hint.map(|hint| format!("{hint}/init")),
                params: Vec::new(),
                return_cont,
                body,
                // Forcing owns whether initialization is needed; dropping this call independently would leave the result empty after its initializer was consumed.
                droppable: curios_cps::Droppable::Never,
                terminates: false,
            },
        );
        thunk
    }

    /// Run `build` with each member bound to a fresh local holding its forced value, the forcing ahead of what `build` lowers, which reads the local: the statement that reads a member, what follows it on its path, the arms below them and the functions they bind. A function lowered where a member is not bound forces it at its own read, when it runs, rather than at its construction, which may be inside the initializer of the very member it names.
    fn with_cell_reads(
        &mut self,
        members: Vec<ValueId>,
        build: impl FnOnce(&mut Self) -> curios_cps::NodeId,
    ) -> curios_cps::NodeId {
        if members.is_empty() {
            return build(self);
        }
        let reads: Vec<(
            ValueId,
            curios_cps::ValueId,
            curios_cps::FunctionId,
            Option<curios_cps::Atom>,
        )> = members
            .into_iter()
            .map(|member| {
                let force = self.emitter.knot_force(member);
                let local = self
                    .emitter
                    .add_value(self.emitter.arena_value_name(member));
                let previous = self
                    .emitter
                    .stand_for(member, curios_cps::Atom::Value(local));
                (member, local, force, previous)
            })
            .collect();
        let mut body = build(self);
        for (member, _, _, previous) in &reads {
            self.emitter.restore_value(*member, previous.clone());
        }
        for &(_, local, force, _) in reads.iter().rev() {
            let resume = self.emitter.reserve_continuation();
            self.emitter.define_continuation(
                resume,
                curios_cps::Continuation {
                    lineage: self.emitter.lineage(),
                    debug_name: Some("force/resume".into()),
                    params: vec![local],
                    body,
                    terminates: false,
                },
            );
            let forcing = self.emitter.add_node(curios_cps::Node::ApplyFun {
                callee: curios_cps::Callee::Known(force),
                args: Vec::new(),
                return_to: resume,
            });
            body = self.emitter.add_node(curios_cps::Node::LetCont {
                continuations: vec![resume],
                body: forcing,
            });
        }
        body
    }

    fn lower_let(&mut self, rhs: &Rhs, site: Site<'_>) -> curios_cps::NodeId {
        let Site {
            result,
            rest,
            terminator,
            target,
        } = site;
        match rhs {
            // Aliasing binds an already-computed atom: record the mapping and continue; no CPS node is needed.
            Rhs::Alias(atom) => {
                let atom = self.emitter.lower_atom(*atom);
                self.emitter.stand_for(result, atom);
                self.lower_statements(rest, terminator, target)
            }
            Rhs::Operation {
                operation,
                operands,
            } => self.lower_operation(*operation, operands, site),
            Rhs::Sequence {
                operation,
                operands,
            } => {
                let args: Vec<curios_cps::Atom> = operands
                    .iter()
                    .map(|&atom| self.emitter.lower_atom(atom))
                    .collect();
                if let SequenceOp::ListBuild = operation {
                    return self.straight(site, move |bound, next| curios_cps::Node::LetValue {
                        result: bound,
                        value: curios_cps::ValueExpr::List(args),
                        next,
                    });
                }
                let op = sequence_intrinsic(*operation, args.len());
                self.straight(site, move |bound, next| curios_cps::Node::LetIntrinsic {
                    result: bound,
                    op,
                    args,
                    next,
                })
            }
            Rhs::Apply { callee, arguments } => {
                let callee = self.emitter.lower_callee(*callee);
                let args = arguments
                    .iter()
                    .map(|&atom| self.emitter.lower_atom(atom))
                    .collect();
                self.split(1, site, |return_to| curios_cps::Node::ApplyFun {
                    callee,
                    args,
                    return_to,
                })
            }
            Rhs::Product { schema, fields } => {
                // A shared row stays a structural tuple: it is the row a multi-result split also builds, and that site names no schema to agree with.
                let places = match self.emitter.is_shared(*schema) {
                    true => (0..fields.len()).collect(),
                    false => self.emitter.product_slots(*schema),
                };
                let width = match self.emitter.is_shared(*schema) {
                    true => fields.len(),
                    false => self.emitter.product_width(*schema),
                };
                // One writer, so every slot is filled and no filler is ever placed.
                let mut atoms = vec![curios_cps::Atom::Filler; width];
                let mut marked = vec![false; width];
                for (field, &atom) in fields.iter().enumerate() {
                    atoms[places[field]] = self.emitter.lower_atom(atom);
                    marked[places[field]] = self.facts.indexed_only_product(*schema, field);
                }
                let settles = self.emitter.settle_stores(&marked, &mut atoms);
                let value = match self.emitter.is_shared(*schema) {
                    true => curios_cps::ValueExpr::Tuple(atoms),
                    false => {
                        curios_cps::ValueExpr::Row(self.emitter.product_identity(*schema), atoms)
                    }
                };
                let bound = self.emitter.bind_value(result);
                let next = self.lower_statements(rest, terminator, target);
                let node = self.emitter.add_node(curios_cps::Node::LetValue {
                    result: bound,
                    value,
                    next,
                });
                self.emitter.wrap_settles(settles, node)
            }
            Rhs::Construct {
                constructor,
                fields,
            } => {
                let encoding = self
                    .emitter
                    .family_encoding(self.emitter.constructor_family(*constructor));
                let mut atoms = fields
                    .iter()
                    .map(|&atom| self.emitter.lower_atom(atom))
                    .collect::<Vec<_>>();
                let marked = (0..fields.len())
                    .map(|field| self.facts.indexed_only_constructor(*constructor, field))
                    .collect::<Vec<_>>();
                let settles = self.emitter.settle_stores(&marked, &mut atoms);
                let bare = matches!(encoding, FamilyEncoding::Collapsed) && atoms.len() <= 1
                    || matches!(encoding, FamilyEncoding::Immediate { constructor: bare } if bare == *constructor);
                let node = if bare {
                    let atom = atoms.into_iter().next().unwrap_or_else(|| {
                        curios_cps::Atom::Literal(curios_cps::Literal::Nat(Natural::zero()))
                    });
                    self.emitter.stand_for(result, atom);
                    self.lower_statements(rest, terminator, target)
                } else {
                    let value = self.emitter.constructor_row(*constructor, atoms);
                    self.straight(site, |result, next| curios_cps::Node::LetValue {
                        result,
                        value,
                        next,
                    })
                };
                self.emitter.wrap_settles(settles, node)
            }
            Rhs::Project {
                schema,
                product,
                field,
            } => {
                let op = match self.emitter.is_shared(*schema) {
                    true => curios_cps::Intrinsic::TupleGet(*field as usize),
                    false => curios_cps::Intrinsic::RowGet(
                        self.emitter.product_identity(*schema),
                        self.emitter.product_slots(*schema)[*field as usize],
                    ),
                };
                let product = self.emitter.lower_atom(*product);
                self.straight(site, move |bound, next| curios_cps::Node::LetIntrinsic {
                    result: bound,
                    op,
                    args: vec![product],
                    next,
                })
            }
            Rhs::MatchVariant {
                family,
                scrutinee,
                arms,
                default,
            } => self.lower_match_variant(*family, *scrutinee, arms, *default, site),
            Rhs::SwitchBool {
                scrutinee,
                if_false,
                if_true,
            } => {
                let scrutinee = self.emitter.lower_atom(*scrutinee);
                self.lower_switch(
                    scrutinee,
                    vec![
                        (curios_cps::Literal::Nat(Natural::zero()), *if_false),
                        (curios_cps::Literal::Nat(Natural::one()), *if_true),
                    ],
                    None,
                    site,
                )
            }
            Rhs::Switch {
                scrutinee,
                keys,
                default,
            } => {
                let scrutinee = self.emitter.lower_atom(*scrutinee);
                let arms = match keys {
                    Keys::Nat(cases) => cases
                        .iter()
                        .map(|case| (curios_cps::Literal::Nat(case.key.clone()), case.block))
                        .collect(),
                    Keys::Int(cases) => cases
                        .iter()
                        .map(|case| (curios_cps::Literal::Int(case.key.clone()), case.block))
                        .collect(),
                };
                self.lower_switch(scrutinee, arms, Some(*default), site)
            }
            Rhs::UnconsSequence {
                grain,
                scrutinee,
                empty,
                cons,
            } => self.lower_uncons_sequence(*grain, *scrutinee, *empty, cons, site),
            Rhs::Cell {
                operation,
                operands,
            } => {
                let op = cell_op(*operation);
                let args = operands
                    .iter()
                    .map(|&atom| self.emitter.lower_atom(atom))
                    .collect();
                if let CellOperation::Poll { some, none } = operation {
                    let (join, fresh) = self.open_join(site);
                    let some = (*some, self.holds_value(*some));
                    return self.lower_outcome(
                        join,
                        fresh,
                        2,
                        &[(*none, false), some],
                        |return_to| curios_cps::Node::Cell {
                            op,
                            args,
                            return_to,
                        },
                    );
                }
                self.split(op.result_arity(), site, |return_to| {
                    curios_cps::Node::Cell {
                        op,
                        args,
                        return_to,
                    }
                })
            }
            Rhs::Channel {
                operation,
                operands,
            } => {
                let args = operands
                    .iter()
                    .map(|&atom| self.emitter.lower_atom(atom))
                    .collect();
                let op = match operation {
                    ChannelOperation::New => curios_cps::ChannelOp::New,
                    ChannelOperation::Push { .. } => curios_cps::ChannelOp::Push,
                    ChannelOperation::Take { .. } => curios_cps::ChannelOp::Take,
                    ChannelOperation::Close => curios_cps::ChannelOp::Close,
                    ChannelOperation::Closed => curios_cps::ChannelOp::Closed,
                    ChannelOperation::Count => curios_cps::ChannelOp::Count,
                    ChannelOperation::Capacity => curios_cps::ChannelOp::Capacity,
                };
                let outcomes = match operation {
                    ChannelOperation::Push {
                        taken,
                        full,
                        closed,
                    } => vec![(*taken, false), (*full, false), (*closed, false)],
                    ChannelOperation::Take { item, empty, ended } => {
                        vec![
                            (*item, self.holds_value(*item)),
                            (*empty, false),
                            (*ended, false),
                        ]
                    }
                    _ => Vec::new(),
                };
                if !outcomes.is_empty() {
                    let (join, fresh) = self.open_join(site);
                    self.lower_outcome(join, fresh, op.result_arity(), &outcomes, |return_to| {
                        curios_cps::Node::Channel {
                            op,
                            args,
                            return_to,
                        }
                    })
                } else {
                    self.split(op.result_arity(), site, |return_to| {
                        curios_cps::Node::Channel {
                            op,
                            args,
                            return_to,
                        }
                    })
                }
            }
            Rhs::Foreign { foreign, operands } => {
                let function = self
                    .source
                    .foreign(*foreign)
                    .expect("live foreign row")
                    .clone();
                let args = operands
                    .iter()
                    .map(|&atom| self.emitter.lower_atom(atom))
                    .collect();
                self.lower_foreign(function, args, site)
            }
            Rhs::Intrinsic {
                intrinsic,
                operands,
            } => {
                let op = match intrinsic {
                    Intrinsic::ListMap => curios_cps::IntrinsicCall::ListMap,
                };
                // Both representations bind the list first, then the mapper; the operands transcribe in order.
                let args = operands
                    .iter()
                    .map(|&operand| self.emitter.lower_atom(operand))
                    .collect();
                self.split(1, site, |return_to| curios_cps::Node::Intrinsic {
                    op,
                    args,
                    return_to,
                })
            }
            Rhs::Suspension {
                operation,
                operands,
            } => self.lower_suspension(*operation, operands, site),
        }
    }

    /// The function a bind's suspension arm calls: `carry(suspension, continuation)` answers the suspension with the continuation appended to its frames. Built on first use and kept out of line, so every bind shares the one path it rarely takes, and the path it does take stays small enough to inline — the bind-inlining with a shared slow path of Xie and Leijen's evidence passing, whose `yield_extend` this is.
    fn carry(&mut self, row: curios_cps::RowId) -> curios_cps::FunctionId {
        if let Some(carry) = self.carry {
            return carry;
        }
        // Every bind shares it, so it is made of no declaration's text, whichever function's body first asked for it.
        let lowering = self.emitter.lowering(None);
        let carry = self.emitter.reserve_function();
        let return_cont = self.emitter.reserve_continuation();
        let suspension = self.emitter.add_value(None);
        let continuation = self.emitter.add_value(None);
        let built = self.emitter.add_value(None);
        let appended = self.emitter.add_value(None);
        let parts = [(); 5].map(|()| self.emitter.add_value(None));
        let [kept @ .., frames] = parts;

        let returned = self
            .emitter
            .jump(return_cont, vec![curios_cps::Atom::Value(built)]);
        let rebuilt = self.emitter.add_node(curios_cps::Node::LetValue {
            result: built,
            value: curios_cps::ValueExpr::Row(
                row,
                kept.into_iter()
                    .chain([appended])
                    .map(curios_cps::Atom::Value)
                    .collect(),
            ),
            next: returned,
        });
        let mut body = self.emitter.add_node(curios_cps::Node::LetIntrinsic {
            result: appended,
            op: curios_cps::Intrinsic::ListAppend,
            args: vec![
                curios_cps::Atom::Value(frames),
                curios_cps::Atom::Value(continuation),
            ],
            next: rebuilt,
        });
        for (slot, part) in parts.into_iter().enumerate().rev() {
            body = self.emitter.add_node(curios_cps::Node::LetIntrinsic {
                result: part,
                op: curios_cps::Intrinsic::RowGet(row, slot),
                args: vec![curios_cps::Atom::Value(suspension)],
                next: body,
            });
        }
        self.emitter.define_function(
            carry,
            curios_cps::Function {
                lineage: None,
                debug_name: Some("io/carry".into()),
                params: vec![suspension, continuation],
                return_cont,
                body,
                // It reads and builds, and nothing else: a carry nothing reads may simply not happen.
                droppable: curios_cps::Droppable::ALWAYS,
                terminates: true,
            },
        );
        self.emitter.keep_out_of_line(carry);
        self.emitter.lowering(lowering);
        self.carry = Some(carry);
        carry
    }

    /// The function a host call answered later suspends through: `later(token, again)` answers the suspension on the token's handle, with `again(token, true)`, the description dropping the call, as its cancel, `again(token, false)`, the one asking for its answer, as its rest, and no cells or frames. Built on first use and kept out of line, so every row's call waits in the one shape.
    fn later(&mut self, row: curios_cps::RowId) -> curios_cps::FunctionId {
        if let Some(later) = self.later {
            return later;
        }
        // Every row's call shares it, so it is made of no declaration's text, as `carry` is.
        let lowering = self.emitter.lowering(None);
        let later = self.emitter.reserve_function();
        let return_cont = self.emitter.reserve_continuation();
        let token = self.emitter.add_value(None);
        let again = self.emitter.add_value(None);
        let cancel = self.emitter.add_value(None);
        let rest = self.emitter.add_value(None);
        let cells = self.emitter.add_value(None);
        let frames = self.emitter.add_value(None);
        let built = self.emitter.add_value(None);

        let returned = self
            .emitter
            .jump(return_cont, vec![curios_cps::Atom::Value(built)]);
        let mut body = self.emitter.add_node(curios_cps::Node::LetValue {
            result: built,
            value: curios_cps::ValueExpr::Row(
                row,
                [token, cells, cancel, rest, frames]
                    .into_iter()
                    .map(curios_cps::Atom::Value)
                    .collect(),
            ),
            next: returned,
        });
        for empty in [frames, cells] {
            body = self.emitter.add_node(curios_cps::Node::LetValue {
                result: empty,
                value: curios_cps::ValueExpr::List(Vec::new()),
                next: body,
            });
        }
        // The rest is asked for after the cancel, so each application's continuation holds what the one before made.
        for (made, dropping) in [(rest, 0u32), (cancel, 1u32)] {
            let made_it = self.emitter.reserve_continuation();
            self.emitter.define_continuation(
                made_it,
                curios_cps::Continuation {
                    lineage: self.emitter.lineage(),
                    debug_name: Some("io/later/made".into()),
                    params: vec![made],
                    body,
                    terminates: false,
                },
            );
            let apply = self.emitter.add_node(curios_cps::Node::ApplyFun {
                callee: curios_cps::Callee::Closure(again),
                args: vec![
                    curios_cps::Atom::Value(token),
                    curios_cps::Atom::Literal(curios_cps::Literal::Nat(Natural::from(dropping))),
                ],
                return_to: made_it,
            });
            body = self.emitter.add_node(curios_cps::Node::LetCont {
                continuations: vec![made_it],
                body: apply,
            });
        }
        self.emitter.define_function(
            later,
            curios_cps::Function {
                lineage: None,
                debug_name: Some("io/later".into()),
                params: vec![token, again],
                return_cont,
                body,
                // It applies a closure, whose totality is nothing this lowering states.
                droppable: curios_cps::Droppable::Never,
                terminates: false,
            },
        );
        self.emitter.keep_out_of_line(later);
        self.emitter.lowering(lowering);
        self.later = Some(later);
        later
    }

    /// Lower a suspension operation over the module's suspension row: a construction, a test against the row's type, or a read of one of its slots. A new suspension's frames are the empty list, and a carry and a later are calls of the one out-of-line [`carry`](Self::carry) and [`later`](Self::later).
    fn lower_suspension(
        &mut self,
        operation: SuspensionOperation,
        operands: &[Atom],
        site: Site<'_>,
    ) -> curios_cps::NodeId {
        let row = self.emitter.suspension_row();
        let mut args = operands
            .iter()
            .map(|&atom| self.emitter.lower_atom(atom))
            .collect::<Vec<_>>();
        let op = match operation {
            SuspensionOperation::New => {
                let frames = self.emitter.add_value(None);
                args.push(curios_cps::Atom::Value(frames));
                let built = self.straight(site, move |result, next| curios_cps::Node::LetValue {
                    result,
                    value: curios_cps::ValueExpr::Row(row, args),
                    next,
                });
                return self.emitter.add_node(curios_cps::Node::LetValue {
                    result: frames,
                    value: curios_cps::ValueExpr::List(Vec::new()),
                    next: built,
                });
            }
            SuspensionOperation::Carry => {
                let carry = self.carry(row);
                return self.split(1, site, |return_to| curios_cps::Node::ApplyFun {
                    callee: curios_cps::Callee::Known(carry),
                    args,
                    return_to,
                });
            }
            SuspensionOperation::Later => {
                let later = self.later(row);
                return self.split(1, site, |return_to| curios_cps::Node::ApplyFun {
                    callee: curios_cps::Callee::Known(later),
                    args,
                    return_to,
                });
            }
            SuspensionOperation::Build => {
                return self.straight(site, move |result, next| curios_cps::Node::LetValue {
                    result,
                    value: curios_cps::ValueExpr::Row(row, args),
                    next,
                });
            }
            SuspensionOperation::Is => curios_cps::Intrinsic::IsRow(row),
            SuspensionOperation::On => curios_cps::Intrinsic::RowGet(row, 0),
            SuspensionOperation::Filled => curios_cps::Intrinsic::RowGet(row, 1),
            SuspensionOperation::Cancel => curios_cps::Intrinsic::RowGet(row, 2),
            SuspensionOperation::Next => curios_cps::Intrinsic::RowGet(row, 3),
            SuspensionOperation::Frames => curios_cps::Intrinsic::RowGet(row, 4),
        };
        self.straight(site, move |result, next| curios_cps::Node::LetIntrinsic {
            result,
            op,
            args,
            next,
        })
    }

    /// Lower a scalar operation to a straight-line `LetIntrinsic`. `Byte` and `Nat` share the runtime carrier, so `ByteToNat` is the identity and `NatToByte` masks to a byte.
    fn lower_operation(
        &mut self,
        operation: Operation,
        operands: &[Atom],
        site: Site<'_>,
    ) -> curios_cps::NodeId {
        let Site {
            result,
            rest,
            terminator,
            target,
        } = site;
        match operation {
            Operation::ByteToNat => {
                let atom = self.emitter.lower_atom(operands[0]);
                self.emitter.stand_for(result, atom);
                self.lower_statements(rest, terminator, target)
            }
            Operation::NatToByte => {
                let value = self.emitter.lower_atom(operands[0]);
                self.straight(site, move |bound, next| curios_cps::Node::LetIntrinsic {
                    result: bound,
                    op: curios_cps::Intrinsic::NatAnd,
                    args: vec![
                        value,
                        curios_cps::Atom::Literal(curios_cps::Literal::Nat(Natural::from(0xFFu32))),
                    ],
                    next,
                })
            }
            _ => {
                let op = operation_intrinsic(operation);
                let args = operands
                    .iter()
                    .map(|&atom| self.emitter.lower_atom(atom))
                    .collect();
                self.straight(site, move |bound, next| curios_cps::Node::LetIntrinsic {
                    result: bound,
                    op,
                    args,
                    next,
                })
            }
        }
    }

    // === Control splits ==================================================

    /// Open a join continuation that receives a control split's single result and runs the rest of the block; every arm delivers to it. A split in tail position — the block's last statement, whose result the block returns — delivers straight to the block's own target instead: no administrative join means a self-call in an arm returns to the function's return continuation and is genuinely tail, which is what lets CPS contify the recursion into a loop (a bodyless return continuation is not a forwarding target, so an eta join there would never collapse).
    fn open_join(&mut self, site: Site<'_>) -> (curios_cps::ContinuationId, bool) {
        let Site {
            result,
            rest,
            terminator,
            target,
        } = site;
        if rest.is_empty()
            && matches!(terminator, Terminator::Return(Atom::Value(returned)) if *returned == result)
        {
            return (target, false);
        }
        let join = self.emitter.reserve_continuation();
        let parameter = self.emitter.bind_value(result);
        let body = self.lower_statements(rest, terminator, target);
        self.emitter.define_continuation(
            join,
            curios_cps::Continuation {
                lineage: self.emitter.lineage(),
                debug_name: Some("join".into()),
                params: vec![parameter],
                body,
                terminates: false,
            },
        );
        (join, true)
    }

    /// Build a parameterless continuation lowering `block` into `join`.
    fn plain_arm(
        &mut self,
        block: BlockId,
        join: curios_cps::ContinuationId,
    ) -> curios_cps::ContinuationId {
        let continuation = self.emitter.reserve_continuation();
        let body = self.lower_block(block, join);
        self.emitter.define_continuation(
            continuation,
            curios_cps::Continuation {
                lineage: self.emitter.lineage(),
                debug_name: Some("arm".into()),
                params: Vec::new(),
                body,
                terminates: false,
            },
        );
        continuation
    }

    /// Lower a scalar switch: each `(key, block)` arm and the optional default becomes a parameterless continuation into the join, selected by one `Switch`.
    ///
    /// A `Switch` reads its scrutinee as a machine word, and a `Nat` narrows to one exactly only below the i31, saturating past it — so a key the i31 does not hold is not a table slot at all. Each such key is decided by an equality test on the default path instead, in key order, which leaves the table indexing only words and the saturated value matching no key. An integer's keys are each such a test, since no signed narrowing of the scrutinee indexes a table; where every key is one, the tests are entered with no `Switch` at all.
    fn lower_switch(
        &mut self,
        scrutinee: curios_cps::Atom,
        arms: Vec<(curios_cps::Literal, BlockId)>,
        default: Option<BlockId>,
        site: Site<'_>,
    ) -> curios_cps::NodeId {
        let (join, fresh) = self.open_join(site);
        let mut continuations = if fresh { vec![join] } else { Vec::new() };
        let mut cases = BTreeMap::new();
        let mut tested = Vec::new();
        for (key, block) in arms {
            let continuation = self.plain_arm(block, join);
            continuations.push(continuation);
            match &key {
                curios_cps::Literal::Nat(natural) if curios_cps::nat_is_small(natural) => {
                    cases.insert(
                        u32::try_from(natural).expect("a small natural is a word"),
                        curios_cps::Edge {
                            target: continuation,
                            args: Vec::new(),
                        },
                    );
                }
                _ => tested.push((key, continuation)),
            }
        }
        let mut default = default.map(|block| {
            let continuation = self.plain_arm(block, join);
            continuations.push(continuation);
            curios_cps::Edge {
                target: continuation,
                args: Vec::new(),
            }
        });
        for (key, continuation) in tested.into_iter().rev() {
            let test = self.key_test(&scrutinee, key, continuation, default);
            continuations.push(test);
            default = Some(curios_cps::Edge {
                target: test,
                args: Vec::new(),
            });
        }
        let switch = match (cases.is_empty(), default) {
            (true, Some(first)) => self.emitter.add_node(curios_cps::Node::ApplyCont(first)),
            (_, default) => self.emitter.add_node(curios_cps::Node::Switch {
                scrutinee,
                cases,
                default,
            }),
        };
        self.emitter.add_node(curios_cps::Node::LetCont {
            continuations,
            body: switch,
        })
    }

    /// A parameterless continuation deciding one switch key by its carrier's equality, `NatEql` for a natural and `IntEql` for an integer: `arm` when `scrutinee` is `key`, and `otherwise` — the next test or the switch's own default — when it is not.
    fn key_test(
        &mut self,
        scrutinee: &curios_cps::Atom,
        key: curios_cps::Literal,
        arm: curios_cps::ContinuationId,
        otherwise: Option<curios_cps::Edge>,
    ) -> curios_cps::ContinuationId {
        let op = match &key {
            curios_cps::Literal::Int(_) => curios_cps::Intrinsic::IntEql,
            _ => curios_cps::Intrinsic::NatEql,
        };
        let equal = self.emitter.add_value(None);
        let switch = self.emitter.add_node(curios_cps::Node::Switch {
            scrutinee: curios_cps::Atom::Value(equal),
            cases: BTreeMap::from([(
                1,
                curios_cps::Edge {
                    target: arm,
                    args: Vec::new(),
                },
            )]),
            default: otherwise,
        });
        let body = self.emitter.add_node(curios_cps::Node::LetIntrinsic {
            result: equal,
            op,
            args: vec![scrutinee.clone(), curios_cps::Atom::Literal(key)],
            next: switch,
        });
        let continuation = self.emitter.reserve_continuation();
        self.emitter.define_continuation(
            continuation,
            curios_cps::Continuation {
                lineage: self.emitter.lineage(),
                debug_name: Some("key".into()),
                params: Vec::new(),
                body,
                terminates: false,
            },
        );
        continuation
    }

    /// Lower a variant match: the tag (`RowGet(row, 0)`) selects an arm through a `Switch`; each arm binds its payload from the slots its constructor's fields occupy and delivers to the join. A [`FamilyEncoding::Collapsed`] family has nothing to decide — its single arm (or the default, when the arm is absent) runs unconditionally, inline rather than behind a dispatch.
    fn lower_match_variant(
        &mut self,
        family: FamilyId,
        scrutinee: Atom,
        arms: &[VariantArm],
        default: Option<BlockId>,
        site: Site<'_>,
    ) -> curios_cps::NodeId {
        let scrutinee = self.emitter.lower_atom(scrutinee);

        match self.emitter.family_encoding(family) {
            FamilyEncoding::Collapsed => {
                let (join, fresh) = self.open_join(site);
                let body = match arms.first() {
                    Some(arm) => self.lower_collapsed_arm(arm, scrutinee, join),
                    None => {
                        let block = default.expect("a variant match covers its family");
                        self.lower_block(block, join)
                    }
                };
                return match fresh {
                    true => self.emitter.add_node(curios_cps::Node::LetCont {
                        continuations: vec![join],
                        body,
                    }),
                    false => body,
                };
            }
            FamilyEncoding::Immediate { constructor } => {
                return self.lower_immediate_match(
                    family,
                    constructor,
                    scrutinee,
                    arms,
                    default,
                    site,
                );
            }
            FamilyEncoding::Tagged => {}
        }

        let identity = self.emitter.row_identity(family);
        let (join, fresh) = self.open_join(site);

        let mut continuations = if fresh { vec![join] } else { Vec::new() };
        let mut cases = BTreeMap::new();
        for arm in arms {
            let continuation = self.lower_variant_arm(identity, arm, scrutinee.clone(), join);
            continuations.push(continuation);
            cases.insert(
                self.emitter.constructor_tag(arm.constructor),
                curios_cps::Edge {
                    target: continuation,
                    args: Vec::new(),
                },
            );
        }
        let default = default.map(|block| {
            let continuation = self.plain_arm(block, join);
            continuations.push(continuation);
            curios_cps::Edge {
                target: continuation,
                args: Vec::new(),
            }
        });

        let tag = self.emitter.add_value(None);
        let switch = self.emitter.add_node(curios_cps::Node::Switch {
            scrutinee: curios_cps::Atom::Value(tag),
            cases,
            default,
        });
        let dispatch = self.emitter.add_node(curios_cps::Node::LetIntrinsic {
            result: tag,
            op: curios_cps::Intrinsic::RowGet(identity, 0),
            args: vec![scrutinee],
            next: switch,
        });
        self.emitter.add_node(curios_cps::Node::LetCont {
            continuations,
            body: dispatch,
        })
    }

    /// Lower a match on a [`FamilyEncoding::Immediate`] family: an `IsImmediate` test splits immediate from struct. The immediate side is the bare-payload arm with its binder aliased to the scrutinee; the boxed side keeps the ordinary tagged dispatch minus the immediate case — elided entirely when the family has exactly one boxed constructor, since the test already decided everything.
    fn lower_immediate_match(
        &mut self,
        family: FamilyId,
        immediate: ConstructorId,
        scrutinee: curios_cps::Atom,
        arms: &[VariantArm],
        default: Option<BlockId>,
        site: Site<'_>,
    ) -> curios_cps::NodeId {
        let (join, fresh) = self.open_join(site);
        let mut continuations = if fresh { vec![join] } else { Vec::new() };

        let default_cont = default.map(|block| {
            let continuation = self.plain_arm(block, join);
            continuations.push(continuation);
            continuation
        });

        let immediate_target = match arms.iter().find(|arm| arm.constructor == immediate) {
            Some(arm) => {
                let body = self.lower_immediate_arm(arm, scrutinee.clone(), join);
                let continuation = self.emitter.reserve_continuation();
                self.emitter.define_continuation(
                    continuation,
                    curios_cps::Continuation {
                        lineage: self.emitter.lineage(),
                        debug_name: Some("arm/immediate".into()),
                        params: Vec::new(),
                        body,
                        terminates: false,
                    },
                );
                continuations.push(continuation);
                continuation
            }
            None => default_cont.expect("a variant match covers its family"),
        };

        let identity = self.emitter.row_identity(family);
        let boxed: Vec<ConstructorId> = self
            .source
            .family(family)
            .expect("live family")
            .constructors
            .iter()
            .copied()
            .filter(|&constructor| constructor != immediate)
            .collect();
        let boxed_target = match boxed.as_slice() {
            [only] => match arms.iter().find(|arm| arm.constructor == *only) {
                Some(arm) => {
                    let continuation =
                        self.lower_variant_arm(identity, arm, scrutinee.clone(), join);
                    continuations.push(continuation);
                    continuation
                }
                None => default_cont.expect("a variant match covers its family"),
            },
            _ => {
                let mut cases = BTreeMap::new();
                for arm in arms.iter().filter(|arm| arm.constructor != immediate) {
                    let continuation =
                        self.lower_variant_arm(identity, arm, scrutinee.clone(), join);
                    continuations.push(continuation);
                    cases.insert(
                        self.emitter.constructor_tag(arm.constructor),
                        curios_cps::Edge {
                            target: continuation,
                            args: Vec::new(),
                        },
                    );
                }
                let default = default_cont.map(|continuation| curios_cps::Edge {
                    target: continuation,
                    args: Vec::new(),
                });
                let tag = self.emitter.add_value(None);
                let switch = self.emitter.add_node(curios_cps::Node::Switch {
                    scrutinee: curios_cps::Atom::Value(tag),
                    cases,
                    default,
                });
                let body = self.emitter.add_node(curios_cps::Node::LetIntrinsic {
                    result: tag,
                    op: curios_cps::Intrinsic::RowGet(identity, 0),
                    args: vec![scrutinee.clone()],
                    next: switch,
                });
                let continuation = self.emitter.reserve_continuation();
                self.emitter.define_continuation(
                    continuation,
                    curios_cps::Continuation {
                        lineage: self.emitter.lineage(),
                        debug_name: Some("arm/boxed".into()),
                        params: Vec::new(),
                        body,
                        terminates: false,
                    },
                );
                continuations.push(continuation);
                continuation
            }
        };

        let kind = self.emitter.add_value(None);
        let switch = self.emitter.add_node(curios_cps::Node::Switch {
            scrutinee: curios_cps::Atom::Value(kind),
            cases: BTreeMap::from([(
                1,
                curios_cps::Edge {
                    target: immediate_target,
                    args: Vec::new(),
                },
            )]),
            default: Some(curios_cps::Edge {
                target: boxed_target,
                args: Vec::new(),
            }),
        });
        let dispatch = self.emitter.add_node(curios_cps::Node::LetIntrinsic {
            result: kind,
            op: curios_cps::Intrinsic::IsImmediate,
            args: vec![scrutinee],
            next: switch,
        });
        self.emitter.add_node(curios_cps::Node::LetCont {
            continuations,
            body: dispatch,
        })
    }

    /// One collapsed arm body: a lone payload aliases the scrutinee, which *is* the payload under the collapsed encoding; a wider row projects untagged fields. Returns a body rather than a continuation because the caller inlines it with no dispatch to target it.
    ///
    /// The aliasing is sound *here* and only here. A collapsed family has one constructor, so the scrutinee is the payload on every path there is. The immediate encoding looks like the same shape and is not — its scrutinee is a scalar on one path and a tuple on the other — so it binds through [`lower_immediate_arm`](Self::lower_immediate_arm) instead. Sharing this function with it would miscompile a loop that does arithmetic on the payload; see [`curios_cps::Intrinsic::ImmediateGet`].
    fn lower_collapsed_arm(
        &mut self,
        arm: &VariantArm,
        scrutinee: curios_cps::Atom,
        join: curios_cps::ContinuationId,
    ) -> curios_cps::NodeId {
        if let [binder] = arm.bindings.as_slice() {
            self.emitter.stand_for(*binder, scrutinee);
            return self.lower_block(arm.block, join);
        }
        let bindings: Vec<curios_cps::ValueId> = arm
            .bindings
            .iter()
            .map(|&binder| self.emitter.bind_value(binder))
            .collect();
        let row = self
            .emitter
            .row_identity(self.emitter.constructor_family(arm.constructor));
        let places = self.emitter.constructor_slots(arm.constructor);
        let mut body = self.lower_block(arm.block, join);
        for index in (0..bindings.len()).rev() {
            body = self.emitter.add_node(curios_cps::Node::LetIntrinsic {
                result: bindings[index],
                op: curios_cps::Intrinsic::RowGet(row, places[index]),
                args: vec![scrutinee.clone()],
                next: body,
            });
        }
        body
    }

    /// One immediate-encoded arm body: the payload is the scrutinee's value, bound through [`curios_cps::Intrinsic::ImmediateGet`] rather than aliased to it, so it has a definition of its own for the representation analysis to read a carrier off.
    ///
    /// Exactly one binding, always: the encoding admits only a unary constructor, so there is no wider row to project and a different shape is this lowering's own contract broken rather than a program's fault.
    fn lower_immediate_arm(
        &mut self,
        arm: &VariantArm,
        scrutinee: curios_cps::Atom,
        join: curios_cps::ContinuationId,
    ) -> curios_cps::NodeId {
        let [binder] = arm.bindings.as_slice() else {
            panic!("an immediate constructor binds exactly its one payload")
        };
        let bound = self.emitter.bind_value(*binder);
        let body = self.lower_block(arm.block, join);
        self.emitter.add_node(curios_cps::Node::LetIntrinsic {
            result: bound,
            op: curios_cps::Intrinsic::ImmediateGet,
            args: vec![scrutinee],
            next: body,
        })
    }

    fn lower_variant_arm(
        &mut self,
        family: curios_cps::RowId,
        arm: &VariantArm,
        scrutinee: curios_cps::Atom,
        join: curios_cps::ContinuationId,
    ) -> curios_cps::ContinuationId {
        let bindings: Vec<curios_cps::ValueId> = arm
            .bindings
            .iter()
            .map(|&binder| self.emitter.bind_value(binder))
            .collect();
        let places = self.emitter.constructor_slots(arm.constructor);
        let mut body = self.lower_block(arm.block, join);
        for index in (0..bindings.len()).rev() {
            body = self.emitter.add_node(curios_cps::Node::LetIntrinsic {
                result: bindings[index],
                op: curios_cps::Intrinsic::RowGet(family, places[index]),
                args: vec![scrutinee.clone()],
                next: body,
            });
        }
        let hint = self
            .source
            .constructor(arm.constructor)
            .and_then(|constructor| constructor.debug_name.as_deref())
            .map_or_else(|| "arm".to_string(), |name| format!("arm/{name}"));
        let continuation = self.emitter.reserve_continuation();
        self.emitter.define_continuation(
            continuation,
            curios_cps::Continuation {
                lineage: self.emitter.lineage(),
                debug_name: Some(hint),
                params: Vec::new(),
                body,
                terminates: false,
            },
        );
        continuation
    }

    // === Peels ===========================================================

    /// Lower a sequence case split to a length dispatch over one peel: zero takes the empty block, anything else peels the head and the suffix after it.
    ///
    /// The reads land *inside* the non-empty arm. Beside the dispatch they would run on the empty sequence too, where both are out of range — a trap where the program has an answer.
    fn lower_uncons_sequence(
        &mut self,
        grain: SequenceGrain,
        scrutinee: Atom,
        empty: BlockId,
        cons: &UnconsSequenceStep,
        site: Site<'_>,
    ) -> curios_cps::NodeId {
        let sequence = self.emitter.lower_atom(scrutinee);
        let (join, fresh) = self.open_join(site);
        let mut continuations = if fresh { vec![join] } else { Vec::new() };

        let length = self.emitter.add_value(None);
        let element = self.emitter.bind_value(cons.element);
        // Declined where nothing reads it: a suffix allocates a rope view, and this is the only place that may decline to build one — a later pass cannot drop a read on the grounds its result is dead.
        let suffix = (self.analysis.value_uses(cons.suffix) > 0)
            .then(|| self.emitter.bind_value(cons.suffix));

        let empty_arm = self.plain_arm(empty, join);
        continuations.push(empty_arm);

        let cons_arm = self.emitter.reserve_continuation();
        let cons_body = self.lower_block(cons.block, join);
        let cons_body = self
            .emitter
            .emit_peel(grain, &sequence, element, suffix, cons_body);
        self.emitter.define_continuation(
            cons_arm,
            curios_cps::Continuation {
                lineage: self.emitter.lineage(),
                debug_name: Some("arm/cons".into()),
                params: Vec::new(),
                body: cons_body,
                terminates: false,
            },
        );
        continuations.push(cons_arm);

        let dispatch = self.emitter.add_node(curios_cps::Node::Switch {
            scrutinee: curios_cps::Atom::Value(length),
            cases: BTreeMap::from([(
                0,
                curios_cps::Edge {
                    target: empty_arm,
                    args: Vec::new(),
                },
            )]),
            default: Some(curios_cps::Edge {
                target: cons_arm,
                args: Vec::new(),
            }),
        });
        let dispatch = self.emitter.add_node(curios_cps::Node::LetCont {
            continuations,
            body: dispatch,
        });

        self.emitter.add_node(curios_cps::Node::LetIntrinsic {
            result: length,
            op: sequence_len_op(grain),
            args: vec![sequence],
            next: dispatch,
        })
    }

    // === Effects =========================================================

    /// Lower a control-splitting statement (an application, cell, or intrinsic) returning to a fresh continuation. That continuation receives the statement's results — one for value-producing forms, zero for a cell write, whose bound result is the unit carrier.
    ///
    /// The continuation is named after the operation it resumes from, read off the node `make` builds rather than passed in: where a call returns to and where an eliminator's arms converge are different things, and calling both of them `join` would leave most of the hints in a dump saying the wrong one.
    fn split(
        &mut self,
        result_arity: usize,
        site: Site<'_>,
        make: impl FnOnce(curios_cps::ContinuationId) -> curios_cps::Node,
    ) -> curios_cps::NodeId {
        let Site {
            result,
            rest,
            terminator,
            target,
        } = site;
        // The same tail bypass as `open_join`: a single-result split whose value the block immediately returns delivers to the block's target.
        if result_arity == 1
            && rest.is_empty()
            && matches!(terminator, Terminator::Return(Atom::Value(returned)) if *returned == result)
        {
            return self.emitter.add_node(make(target));
        }
        let join = self.emitter.reserve_continuation();
        let params = if result_arity == 0 {
            self.emitter.stand_for(
                result,
                curios_cps::Atom::Literal(curios_cps::Literal::Nat(Natural::zero())),
            );
            Vec::new()
        } else {
            vec![self.emitter.bind_value(result)]
        };
        let body = self.lower_statements(rest, terminator, target);
        // The node first, so the continuation can be named after it: a caller passing its own role could name a continuation after an operation it did not build.
        let node = self.emitter.add_node(make(join));
        let role = resume_role(self.emitter.node(node).expect("the node just added"));
        self.emitter.define_continuation(
            join,
            curios_cps::Continuation {
                lineage: self.emitter.lineage(),
                debug_name: Some(role.into()),
                params,
                body,
                terminates: false,
            },
        );
        self.emitter.add_node(curios_cps::Node::LetCont {
            continuations: vec![join],
            body: node,
        })
    }

    /// Turn the guest operation's private status into an ordinary constructor before delivering its language result.
    /// Whether the outcome `constructor` holds the value its operation answers: where its row keeps a field for it, and not where the row keeps none, the value being a proof there.
    fn holds_value(&self, constructor: ConstructorId) -> bool {
        self.source
            .constructor(constructor)
            .is_some_and(|constructor| constructor.width() == 1)
    }

    fn lower_outcome(
        &mut self,
        join: curios_cps::ContinuationId,
        fresh_join: bool,
        arity: usize,
        outcomes: &[(ConstructorId, bool)],
        make: impl FnOnce(curios_cps::ContinuationId) -> curios_cps::Node,
    ) -> curios_cps::NodeId {
        let params = (0..arity)
            .map(|_| self.emitter.add_value(None))
            .collect::<Vec<_>>();
        let mut arms = Vec::new();
        let mut cases = BTreeMap::new();
        for (code, &(constructor, has_payload)) in outcomes.iter().enumerate() {
            let fields = if has_payload {
                vec![curios_cps::Atom::Value(params[1])]
            } else {
                Vec::new()
            };
            let family = self.emitter.constructor_family(constructor);
            let encoding = self.emitter.family_encoding(family);
            let bare = matches!(encoding, FamilyEncoding::Collapsed) && fields.len() <= 1
                || matches!(encoding, FamilyEncoding::Immediate { constructor: bare } if bare == constructor);
            let body = if bare {
                let atom = fields.into_iter().next().unwrap_or_else(|| {
                    curios_cps::Atom::Literal(curios_cps::Literal::Nat(Natural::zero()))
                });
                self.emitter.jump(join, vec![atom])
            } else {
                let value = self.emitter.constructor_row(constructor, fields);
                let result = self.emitter.add_value(None);
                let next = self
                    .emitter
                    .jump(join, vec![curios_cps::Atom::Value(result)]);
                self.emitter.add_node(curios_cps::Node::LetValue {
                    result,
                    value,
                    next,
                })
            };
            let arm = self.emitter.continuation_of(body);
            arms.push(arm);
            cases.insert(
                code as u32,
                curios_cps::Edge {
                    target: arm,
                    args: Vec::new(),
                },
            );
        }
        let switch = self.emitter.add_node(curios_cps::Node::Switch {
            scrutinee: curios_cps::Atom::Value(params[0]),
            cases,
            default: None,
        });
        let body = self.emitter.add_node(curios_cps::Node::LetCont {
            continuations: arms,
            body: switch,
        });
        let resume = self.emitter.reserve_continuation();
        self.emitter.define_continuation(
            resume,
            curios_cps::Continuation {
                lineage: self.emitter.lineage(),
                debug_name: Some("outcome/resume".into()),
                params,
                body,
                terminates: false,
            },
        );
        let node = self.emitter.add_node(make(resume));
        let mut continuations = if fresh_join { vec![join] } else { Vec::new() };
        continuations.push(resume);
        self.emitter.add_node(curios_cps::Node::LetCont {
            continuations,
            body: node,
        })
    }

    /// Lower a host call. A single-result foreign returns straight to the block's join; a multi-result foreign returns to a resume continuation that packs the results into the record tuple the consuming code projects through.
    fn lower_foreign(
        &mut self,
        function: Arc<ForeignFunction>,
        args: Vec<curios_cps::Atom>,
        site: Site<'_>,
    ) -> curios_cps::NodeId {
        let Site {
            result,
            rest,
            terminator,
            target,
        } = site;
        let arity = function.signature().imported_results().len();
        if arity == 1 {
            return self.split(1, site, |return_to| curios_cps::Node::Foreign {
                function,
                args,
                return_to,
            });
        }

        let results: Vec<curios_cps::ValueId> =
            (0..arity).map(|_| self.emitter.add_value(None)).collect();
        let record = self.emitter.add_value(None);
        self.emitter
            .stand_for(result, curios_cps::Atom::Value(record));
        let next = self.lower_statements(rest, terminator, target);
        let pack = self.emitter.add_node(curios_cps::Node::LetValue {
            result: record,
            value: curios_cps::ValueExpr::Tuple(
                results
                    .iter()
                    .copied()
                    .map(curios_cps::Atom::Value)
                    .collect(),
            ),
            next,
        });
        let resume = self.emitter.reserve_continuation();
        self.emitter.define_continuation(
            resume,
            curios_cps::Continuation {
                lineage: self.emitter.lineage(),
                debug_name: Some("foreign/resume".into()),
                params: results,
                body: pack,
                terminates: false,
            },
        );
        let call = self.emitter.add_node(curios_cps::Node::Foreign {
            function,
            args,
            return_to: resume,
        });
        self.emitter.add_node(curios_cps::Node::LetCont {
            continuations: vec![resume],
            body: call,
        })
    }

    // === Atoms and identities ============================================

    /// Bind a straight-line result: allocate its CPS value, lower the rest of the block, and emit the node `make` builds in front of it.
    fn straight(
        &mut self,
        site: Site<'_>,
        make: impl FnOnce(curios_cps::ValueId, curios_cps::NodeId) -> curios_cps::Node,
    ) -> curios_cps::NodeId {
        let Site {
            result,
            rest,
            terminator,
            target,
        } = site;
        let bound = self.emitter.bind_value(result);
        let next = self.lower_statements(rest, terminator, target);
        self.emitter.add_node(make(bound, next))
    }
}

/// What a split's continuation resumes from, read off the node the split built.
///
/// A wildcard rather than an exhaustive list: the input is whatever the caller's closure produced, and `split` builds only the four call-like nodes — a neutral name for anything else costs nothing, where a missed arm in a table the header depends on would cost a declaration.
fn resume_role(node: &curios_cps::Node) -> &'static str {
    match node {
        curios_cps::Node::ApplyFun { .. } => "apply/resume",
        curios_cps::Node::Cell { .. } => "cell/resume",
        curios_cps::Node::Channel { .. } => "channel/resume",
        curios_cps::Node::Intrinsic { .. } => "intrinsic/resume",
        curios_cps::Node::Foreign { .. } => "foreign/resume",
        _ => "resume",
    }
}
