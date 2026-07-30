//! The lowering into the landed Cont interface — the one-way door from
//! meaning to mechanism.
//!
//! Every encoding decision the erasure deliberately deferred is made here,
//! exactly once, per the specification's normative desugar table: `Unit`,
//! `Bool`, and `Byte` ride the `Nat` carrier (`Bool` operations become `Nat`
//! bit operations, `Byte` comparisons `Nat` comparisons, `NatToByte` a mask,
//! `ByteToNat` the identity); a `Handle` token is its little-endian bytes as a
//! byte-grain binary and `HandleEql` that grain's binary equality; products and
//! variants are generic tuples (a variant is `(tag, payload…)`, the tag the
//! constructor's position in its family); matches and switches are one
//! `Nat`-keyed `Switch` behind the tag projection; the fold forms are
//! synthesized accumulator loops; recursive groups are `LetFun`/`RecInit`,
//! with value-only knots tied through compiler-internal cells.
//!
//! The lowering is target-continuation shaped: each arena block is lowered
//! against the continuation that receives its result — its terminator
//! delivers there, and its statements build the node chain in front. Because
//! the arena is already ANF, every operand is an atom and maps directly to a
//! [`curios_cont::CpsAtom`]; no administrative continuation is introduced
//! merely to evaluate an operand. Only a genuine control split — an
//! application return, a switch or match, a fold loop, a host call — opens a
//! join continuation whose parameter receives the split's result and whose
//! body is the rest of the block.
//!
//! Arena identities are globally unique and never shadowed, so flat maps to
//! their Cont counterparts suffice; source hints are carried onto the Cont
//! values and functions they lower to.

#[cfg(test)]
mod tests;

use {
    super::{
        Atom, Block, BlockId, CellOperation, Constant, ConstantId, ConstructorId, FoldNatStep,
        FoldSequenceStep, Function, FunctionId, Intrinsic, Module, Operation, RecGroup, RecGroupId,
        Rhs, SequenceGrain, SequenceOp, Statement, StatementId, Terminator, ValueId, VariantArm,
    },
    curios_abi::ForeignFunction,
    curios_base::{Grain, PackedBin},
    num_bigint::BigUint,
    std::{
        collections::{BTreeMap, BTreeSet},
        sync::Arc,
    },
};

/// Lower a verified arena [`Module`] into the landed Cont
/// [`curios_cont::CpsModule`]. The module's top level — its item chain followed by its
/// entry block — becomes the parameterless Cps entry `main`, delivering its
/// result to a bodyless `return_cont`. The produced module is verified; a
/// failure is a lowering bug, not a user error, so it panics.
pub fn lower_to_cont(source: &Module) -> curios_cont::CpsModule {
    curios_profile::profile!("lower_to_cont");
    let mut lowerer = Lowerer {
        source,
        module: curios_cont::CpsModule::new(),
        values: BTreeMap::new(),
        functions: BTreeMap::new(),
        knot_cells: BTreeMap::new(),
    };

    let main = lowerer.module.reserve_function();
    let return_cont = lowerer.module.reserve_continuation();
    let entry = source.entry().expect("a finalized module has an entry");
    let entry = source.block(entry).expect("live entry block").clone();
    let mut statements: Vec<StatementId> = source.items().to_vec();
    statements.extend(&entry.statements);
    let body = lowerer.lower_statements(&statements, &entry.terminator, return_cont);
    lowerer.module.define_function(
        main,
        curios_cont::CpsFunction {
            debug_name: Some("main".into()),
            params: Vec::new(),
            return_cont,
            body,
        },
    );
    lowerer.module.set_entry(main);

    lowerer
        .module
        .verify()
        .unwrap_or_else(|error| panic!("arena lowering produced invalid Cont: {error}"));
    lowerer.module
}

struct Lowerer<'a> {
    source: &'a Module,
    module: curios_cont::CpsModule,
    values: BTreeMap<ValueId, curios_cont::CpsAtom>,
    functions: BTreeMap<FunctionId, curios_cont::CpsFunId>,
    /// Members of value-only recursive knots, mapped to the mutable cell that
    /// ties each knot. A reference to such a member lowers to a read of the
    /// filled cell (see [`Lowerer::with_cell_reads`]), so the tie is forced
    /// once and is invisible to everything but this lowering.
    knot_cells: BTreeMap<ValueId, curios_cont::CpsValueId>,
}

impl Lowerer<'_> {
    /// Lower a block so its result is delivered to `target`.
    fn lower_block(
        &mut self,
        block: BlockId,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        let block: Block = self.source.block(block).expect("live block").clone();
        self.lower_statements(&block.statements, &block.terminator, target)
    }

    /// Lower a statement suffix closed by `terminator`, delivering to `target`.
    fn lower_statements(
        &mut self,
        statements: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        let Some((&first, rest)) = statements.split_first() else {
            return self.lower_terminator(terminator, target);
        };
        let statement = self
            .source
            .statement(first)
            .expect("live statement")
            .clone();
        match statement {
            Statement::Let { result, rhs } => {
                self.lower_let(result, &rhs, rest, terminator, target)
            }
            Statement::Functions { functions } => {
                let functions = self.lower_function_group(&functions);
                let body = self.lower_statements(rest, terminator, target);
                self.module
                    .add_node(curios_cont::CpsNode::LetFun { functions, body })
            }
            Statement::Rec { group } => self.lower_rec_group(group, rest, terminator, target),
        }
    }

    fn lower_terminator(
        &mut self,
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        match terminator {
            Terminator::Return(atom) => {
                let atom = self.lower_atom(*atom);
                self.jump(target, vec![atom])
            }
            Terminator::Exit(atom) => {
                let atom = self.lower_atom(*atom);
                self.module
                    .add_node(curios_cont::CpsNode::Exit { value: Some(atom) })
            }
            Terminator::Unreachable => self.module.add_node(curios_cont::CpsNode::Unreachable),
        }
    }

    fn jump(
        &mut self,
        target: curios_cont::CpsContId,
        args: Vec<curios_cont::CpsAtom>,
    ) -> curios_cont::CpsNodeId {
        self.module
            .add_node(curios_cont::CpsNode::ApplyCont(curios_cont::CpsEdge {
                target,
                args,
            }))
    }

    // === Functions and recursion =========================================

    /// Reserve every function of a group before defining any, so a member
    /// body can reference itself and its siblings; return the Cont ids in
    /// group order.
    fn lower_function_group(&mut self, functions: &[FunctionId]) -> Vec<curios_cont::CpsFunId> {
        let ids: Vec<curios_cont::CpsFunId> = functions
            .iter()
            .map(|&arena| {
                let id = self.module.reserve_function();
                self.functions.insert(arena, id);
                id
            })
            .collect();
        for (&arena, &id) in functions.iter().zip(&ids) {
            self.define_function(arena, id);
        }
        ids
    }

    /// Define a reserved Cont function from its arena function. A body that
    /// references a value-only knot member reads it from the knot's cell at
    /// its own entry — at call time, once the knot is tied — rather than
    /// capturing the value directly.
    fn define_function(&mut self, arena: FunctionId, id: curios_cont::CpsFunId) {
        let function: Function = self.source.function(arena).expect("live function").clone();
        let return_cont = self.module.reserve_continuation();
        let params = function
            .params
            .iter()
            .map(|&param| self.bind_value(param))
            .collect();
        let members = if self.knot_cells.is_empty() {
            Vec::new()
        } else {
            self.block_member_refs(function.body)
        };
        let body = self.with_cell_reads(members, |lowerer| {
            lowerer.lower_block(function.body, return_cont)
        });
        self.module.define_function(
            id,
            curios_cont::CpsFunction {
                debug_name: function.debug_name.clone(),
                params,
                return_cont,
                body,
            },
        );
    }

    /// Lower a recursive group by its shape: a mixed group becomes a
    /// `RecInit` knot; a function-only group a plain `LetFun` (erasure emits
    /// `Functions` for those, so this is totality); a value-only group is
    /// tied through cells (see [`Lowerer::lower_value_knot`]).
    fn lower_rec_group(
        &mut self,
        group: RecGroupId,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        let mut group: RecGroup = self.source.rec_group(group).expect("live group").clone();

        // Drop computed members never referenced outside their own
        // initializer — their init never runs, mirroring the legacy path's
        // member pruning (which is what makes an unused self-knot
        // `rec loop = loop` legal). A member that survives with a direct
        // eager self-reference has no sound initialization order.
        group
            .values
            .retain(|member| self.member_used_outside_init(member.value, member.init));
        for member in &group.values {
            let init = self.source.block(member.init).expect("live init block");
            assert!(
                !self
                    .eager_value_refs(&init.statements, &init.terminator)
                    .contains(&member.value),
                "unsupported eager self-recursive value: its initializer \
                 evaluates the member it defines"
            );
        }

        if group.functions.is_empty() {
            return self.lower_value_knot(&group, rest, terminator, target);
        }

        let functions: Vec<curios_cont::CpsFunId> = group
            .functions
            .iter()
            .map(|&arena| {
                let id = self.module.reserve_function();
                self.functions.insert(arena, id);
                id
            })
            .collect();
        let values: Vec<curios_cont::CpsValueId> = group
            .values
            .iter()
            .map(|member| self.bind_value(member.value))
            .collect();

        for (&arena, &id) in group.functions.iter().zip(&functions) {
            self.define_function(arena, id);
        }

        if group.values.is_empty() {
            let body = self.lower_statements(rest, terminator, target);
            return self
                .module
                .add_node(curios_cont::CpsNode::LetFun { functions, body });
        }

        // Mixed knot: the reserved value slots are visible to the member
        // functions and every initializer through the `RecInit`; the value
        // initializers chain over the group's ready point in eager order.
        let ready = self.lower_statements(rest, terminator, target);
        let order = self.computed_init_order(&group);
        let mut body = ready;
        for &position in order.iter().rev() {
            let init = group.values[position].init;
            let bound = values[position];
            let continuation = self.module.reserve_continuation();
            self.module.define_continuation(
                continuation,
                curios_cont::CpsContinuation {
                    debug_name: None,
                    params: vec![bound],
                    body,
                },
            );
            let entry = self.lower_block(init, continuation);
            body = self.module.add_node(curios_cont::CpsNode::LetCont {
                continuations: vec![continuation],
                body: entry,
            });
        }

        self.module.add_node(curios_cont::CpsNode::RecInit {
            functions,
            values,
            ready,
            body,
        })
    }

    /// Tie a value-only recursive knot with compiler-internal cells: allocate
    /// every cell, run the initializers in eager-dependency order (their
    /// closures capture the cells, not the values), store each result, and
    /// read the filled cells wherever a member is referenced. The verifier
    /// already rejected eager cycles among the members, so no cell is read
    /// before its store; each member's value is built exactly once, and every
    /// reference observes that one value.
    fn lower_value_knot(
        &mut self,
        group: &RecGroup,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        let cells: Vec<curios_cont::CpsValueId> = group
            .values
            .iter()
            .map(|member| {
                let cell = self.module.add_value(None);
                self.knot_cells.insert(member.value, cell);
                cell
            })
            .collect();

        // Downstream reads the now-filled cells for the members it references.
        let ready_members = self.eager_member_refs(rest, terminator);
        let mut body = self.with_cell_reads(ready_members, |lowerer| {
            lowerer.lower_statements(rest, terminator, target)
        });

        // Initialize and store each member in eager-dependency order, inside out.
        let order = self.computed_init_order(group);
        for &position in order.iter().rev() {
            let init = group.values[position].init;
            let cell = cells[position];

            let after_store = self.module.reserve_continuation();
            self.module.define_continuation(
                after_store,
                curios_cont::CpsContinuation {
                    debug_name: None,
                    params: Vec::new(),
                    body,
                },
            );
            let value = self.module.add_value(None);
            let store = self.module.add_node(curios_cont::CpsNode::Cell {
                op: curios_cont::CpsCellOp::Set,
                args: vec![
                    curios_cont::CpsAtom::Value(cell),
                    curios_cont::CpsAtom::Value(value),
                ],
                return_to: after_store,
            });
            let store = self.module.add_node(curios_cont::CpsNode::LetCont {
                continuations: vec![after_store],
                body: store,
            });
            let receive = self.module.reserve_continuation();
            self.module.define_continuation(
                receive,
                curios_cont::CpsContinuation {
                    debug_name: None,
                    params: vec![value],
                    body: store,
                },
            );
            let init_members = self.block_member_refs(init);
            let entry =
                self.with_cell_reads(init_members, |lowerer| lowerer.lower_block(init, receive));
            body = self.module.add_node(curios_cont::CpsNode::LetCont {
                continuations: vec![receive],
                body: entry,
            });
        }

        // Allocate every cell first (a placeholder is never read before its
        // store — the knot is productive), so the initializers' closures can
        // already capture the cells they tie.
        for &cell in cells.iter().rev() {
            let bound = self.module.reserve_continuation();
            self.module.define_continuation(
                bound,
                curios_cont::CpsContinuation {
                    debug_name: None,
                    params: vec![cell],
                    body,
                },
            );
            let new = self.module.add_node(curios_cont::CpsNode::Cell {
                op: curios_cont::CpsCellOp::New,
                args: vec![curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(
                    0,
                ))],
                return_to: bound,
            });
            body = self.module.add_node(curios_cont::CpsNode::LetCont {
                continuations: vec![bound],
                body: new,
            });
        }
        body
    }

    /// Run `build` with each member bound to a fresh local holding a read of
    /// its knot cell, wrapping the result so the reads happen at entry. A
    /// deferred closure re-reads its cell at its own entry rather than
    /// capturing a stale placeholder.
    fn with_cell_reads(
        &mut self,
        members: Vec<ValueId>,
        build: impl FnOnce(&mut Self) -> curios_cont::CpsNodeId,
    ) -> curios_cont::CpsNodeId {
        if members.is_empty() {
            return build(self);
        }
        let reads: Vec<(
            ValueId,
            curios_cont::CpsValueId,
            curios_cont::CpsValueId,
            Option<curios_cont::CpsAtom>,
        )> = members
            .into_iter()
            .map(|member| {
                let cell = self.knot_cells[&member];
                let local = self.module.add_value(self.arena_value_name(member));
                let previous = self
                    .values
                    .insert(member, curios_cont::CpsAtom::Value(local));
                (member, local, cell, previous)
            })
            .collect();
        let mut body = build(self);
        for (member, _, _, previous) in &reads {
            match previous {
                Some(atom) => {
                    self.values.insert(*member, atom.clone());
                }
                None => {
                    self.values.remove(member);
                }
            }
        }
        for &(_, local, cell, _) in reads.iter().rev() {
            let resume = self.module.reserve_continuation();
            self.module.define_continuation(
                resume,
                curios_cont::CpsContinuation {
                    debug_name: None,
                    params: vec![local],
                    body,
                },
            );
            let get = self.module.add_node(curios_cont::CpsNode::Cell {
                op: curios_cont::CpsCellOp::Get,
                args: vec![curios_cont::CpsAtom::Value(cell)],
                return_to: resume,
            });
            body = self.module.add_node(curios_cont::CpsNode::LetCont {
                continuations: vec![resume],
                body: get,
            });
        }
        body
    }

    /// Whether `member` is referenced anywhere in the module outside its own
    /// initializer's subtree (control blocks and nested function regions
    /// included on both sides). An unreferenced member's init never runs.
    fn member_used_outside_init(&self, member: ValueId, init: BlockId) -> bool {
        // The init's own subtree: its blocks, plus the regions of functions
        // bound inside it.
        let mut inside_blocks = BTreeSet::new();
        let mut inside_functions = BTreeSet::new();
        let mut function_work: Vec<FunctionId> = Vec::new();
        let mut block_work = vec![init];
        loop {
            if let Some(block) = block_work.pop() {
                if !inside_blocks.insert(block) {
                    continue;
                }
                let Some(block) = self.source.block(block) else {
                    continue;
                };
                for &statement in &block.statements {
                    match self.source.statement(statement) {
                        Some(Statement::Let { rhs, .. }) => block_work.extend(rhs.sub_blocks()),
                        Some(Statement::Functions { functions }) => {
                            function_work.extend(functions.iter().copied());
                        }
                        Some(Statement::Rec { group }) => {
                            if let Some(group) = self.source.rec_group(*group) {
                                function_work.extend(group.functions.iter().copied());
                                block_work.extend(group.values.iter().map(|m| m.init));
                            }
                        }
                        None => {}
                    }
                }
                continue;
            }
            let Some(function) = function_work.pop() else {
                break;
            };
            if !inside_functions.insert(function) {
                continue;
            }
            if let Some(function) = self.source.function(function) {
                block_work.push(function.body);
            }
        }

        // Any reference from a block outside the subtree is a use.
        for (index, slot) in self.source.blocks().iter().enumerate() {
            let Some(block) = slot else { continue };
            if inside_blocks.contains(&BlockId(index as u32)) {
                continue;
            }
            for &statement in &block.statements {
                if let Some(Statement::Let { rhs, .. }) = self.source.statement(statement)
                    && rhs.operands().contains(&Atom::Value(member))
                {
                    return true;
                }
            }
            if block.terminator.atom() == Some(Atom::Value(member)) {
                return true;
            }
        }
        false
    }

    /// The knot members a block's eager region references directly — its
    /// statements' operands, its terminator, and the control sub-blocks and
    /// nested-group initializers reachable without entering a function body
    /// (a nested function takes its own reads).
    fn block_member_refs(&self, block: BlockId) -> Vec<ValueId> {
        match self.source.block(block) {
            Some(block) => self.eager_member_refs(&block.statements, &block.terminator),
            None => Vec::new(),
        }
    }

    fn eager_member_refs(
        &self,
        statements: &[StatementId],
        terminator: &Terminator,
    ) -> Vec<ValueId> {
        let mut refs = BTreeSet::new();
        for value in self.eager_value_refs(statements, terminator) {
            if self.knot_cells.contains_key(&value) {
                refs.insert(value);
            }
        }
        refs.into_iter().collect()
    }

    /// Every value referenced across an eager region rooted at `statements`
    /// and closed by `terminator`, descending through control sub-blocks and
    /// nested-group initializers but never into a function body.
    fn eager_value_refs(
        &self,
        statements: &[StatementId],
        terminator: &Terminator,
    ) -> BTreeSet<ValueId> {
        let mut refs = BTreeSet::new();
        let mut pending: Vec<StatementId> = statements.to_vec();
        let mut blocks: Vec<BlockId> = Vec::new();
        let mut seen = BTreeSet::new();
        if let Some(Atom::Value(value)) = terminator.atom() {
            refs.insert(value);
        }
        loop {
            if let Some(statement) = pending.pop() {
                match self.source.statement(statement) {
                    Some(Statement::Let { rhs, .. }) => {
                        for atom in rhs.operands() {
                            if let Atom::Value(value) = atom {
                                refs.insert(value);
                            }
                        }
                        blocks.extend(rhs.sub_blocks());
                    }
                    // A nested group's computed initializers are eager; its
                    // functions take their own reads when defined.
                    Some(Statement::Rec { group }) => {
                        if let Some(group) = self.source.rec_group(*group) {
                            blocks.extend(group.values.iter().map(|member| member.init));
                        }
                    }
                    Some(Statement::Functions { .. }) | None => {}
                }
                continue;
            }
            let Some(block) = blocks.pop() else { break };
            if !seen.insert(block) {
                continue;
            }
            if let Some(block) = self.source.block(block) {
                pending.extend(&block.statements);
                if let Some(Atom::Value(value)) = block.terminator.atom() {
                    refs.insert(value);
                }
            }
        }
        refs
    }

    /// Order a group's computed members by eager initialization dependency.
    /// The verifier proves eager acyclicity, so a total order always exists;
    /// cross-references flowing only through closures carry no eager edge.
    fn computed_init_order(&self, group: &RecGroup) -> Vec<usize> {
        let position: BTreeMap<ValueId, usize> = group
            .values
            .iter()
            .enumerate()
            .map(|(index, member)| (member.value, index))
            .collect();
        let count = group.values.len();
        let mut in_degree = vec![0usize; count];
        let mut successors: Vec<Vec<usize>> = vec![Vec::new(); count];
        for (index, member) in group.values.iter().enumerate() {
            let init = self.source.block(member.init).expect("live init block");
            for dependency in self.eager_value_refs(&init.statements, &init.terminator) {
                if let Some(&origin) = position.get(&dependency)
                    && origin != index
                {
                    successors[origin].push(index);
                    in_degree[index] += 1;
                }
            }
        }
        let mut ready: Vec<usize> = (0..count).filter(|&index| in_degree[index] == 0).collect();
        let mut order = Vec::with_capacity(count);
        while let Some(index) = ready.pop() {
            order.push(index);
            for &dependent in &successors[index] {
                in_degree[dependent] -= 1;
                if in_degree[dependent] == 0 {
                    ready.push(dependent);
                }
            }
        }
        assert_eq!(
            order.len(),
            count,
            "a verified group has an acyclic eager initialization order"
        );
        order
    }

    // === Statements ======================================================

    fn lower_let(
        &mut self,
        result: ValueId,
        rhs: &Rhs,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        match rhs {
            // Aliasing binds an already-computed atom: record the mapping and
            // continue; no Cont node is needed.
            Rhs::Alias(atom) => {
                let atom = self.lower_atom(*atom);
                self.values.insert(result, atom);
                self.lower_statements(rest, terminator, target)
            }
            Rhs::Operation {
                operation,
                operands,
            } => self.lower_operation(result, *operation, operands, rest, terminator, target),
            Rhs::Sequence {
                operation,
                operands,
            } => {
                let args: Vec<curios_cont::CpsAtom> =
                    operands.iter().map(|&atom| self.lower_atom(atom)).collect();
                if let SequenceOp::LstBuild = operation {
                    return self.straight(result, rest, terminator, target, move |bound, next| {
                        curios_cont::CpsNode::LetValue {
                            result: bound,
                            value: curios_cont::CpsValueExpr::List(args),
                            next,
                        }
                    });
                }
                let op = sequence_prim(*operation, args.len());
                self.straight(result, rest, terminator, target, move |bound, next| {
                    curios_cont::CpsNode::LetPrim {
                        result: bound,
                        op,
                        args,
                        next,
                    }
                })
            }
            Rhs::Apply { callee, arguments } => {
                let callee = self.lower_callee(*callee);
                let args = arguments
                    .iter()
                    .map(|&atom| self.lower_atom(atom))
                    .collect();
                self.split(result, 1, rest, terminator, target, |return_to| {
                    curios_cont::CpsNode::ApplyFun {
                        callee,
                        args,
                        return_to,
                    }
                })
            }
            Rhs::Product { schema: _, fields } => {
                let fields = fields.iter().map(|&atom| self.lower_atom(atom)).collect();
                self.straight(result, rest, terminator, target, move |bound, next| {
                    curios_cont::CpsNode::LetValue {
                        result: bound,
                        value: curios_cont::CpsValueExpr::Tuple(fields),
                        next,
                    }
                })
            }
            Rhs::Construct {
                constructor,
                fields,
            } => {
                let tag = self.constructor_tag(*constructor);
                let mut atoms = Vec::with_capacity(fields.len() + 1);
                atoms.push(curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(
                    tag,
                )));
                atoms.extend(fields.iter().map(|&atom| self.lower_atom(atom)));
                self.straight(result, rest, terminator, target, move |bound, next| {
                    curios_cont::CpsNode::LetValue {
                        result: bound,
                        value: curios_cont::CpsValueExpr::Tuple(atoms),
                        next,
                    }
                })
            }
            Rhs::Project {
                schema: _,
                product,
                field,
            } => {
                let product = self.lower_atom(*product);
                let index = *field as usize;
                self.straight(result, rest, terminator, target, move |bound, next| {
                    curios_cont::CpsNode::LetPrim {
                        result: bound,
                        op: curios_cont::CpsPrimOp::TplGet(index),
                        args: vec![product],
                        next,
                    }
                })
            }
            Rhs::MatchVariant {
                family: _,
                scrutinee,
                arms,
                default,
            } => self
                .lower_match_variant(result, *scrutinee, arms, *default, rest, terminator, target),
            Rhs::SwitchBool {
                scrutinee,
                if_false,
                if_true,
            } => {
                let scrutinee = self.lower_atom(*scrutinee);
                self.lower_switch(
                    result,
                    scrutinee,
                    vec![(0, *if_false), (1, *if_true)],
                    None,
                    rest,
                    terminator,
                    target,
                )
            }
            Rhs::SwitchNat {
                scrutinee,
                cases,
                default,
            } => {
                let scrutinee = self.lower_atom(*scrutinee);
                let arms = cases.iter().map(|case| (case.key, case.block)).collect();
                self.lower_switch(
                    result,
                    scrutinee,
                    arms,
                    Some(*default),
                    rest,
                    terminator,
                    target,
                )
            }
            Rhs::FoldNat {
                scrutinee,
                zero,
                step,
            } => self.lower_fold_nat(result, *scrutinee, *zero, step, rest, terminator, target),
            Rhs::FoldSequence {
                grain,
                scrutinee,
                empty,
                step,
            } => self.lower_fold_sequence(
                result, *grain, *scrutinee, *empty, step, rest, terminator, target,
            ),
            Rhs::Cell {
                operation,
                operands,
            } => {
                let op = cell_op(*operation);
                let args = operands.iter().map(|&atom| self.lower_atom(atom)).collect();
                self.split(
                    result,
                    op.result_arity(),
                    rest,
                    terminator,
                    target,
                    |return_to| curios_cont::CpsNode::Cell {
                        op,
                        args,
                        return_to,
                    },
                )
            }
            Rhs::Foreign { foreign, operands } => {
                let function = self
                    .source
                    .foreign(*foreign)
                    .expect("live foreign row")
                    .clone();
                let args = operands.iter().map(|&atom| self.lower_atom(atom)).collect();
                self.lower_foreign(result, function, args, rest, terminator, target)
            }
            Rhs::Intrinsic {
                intrinsic,
                operands,
            } => {
                let op = match intrinsic {
                    Intrinsic::LstMap => curios_cont::CpsIntrinsicOp::LstMap,
                };
                // Both representations bind the mapper first; the operands
                // transcribe in order.
                let args = operands
                    .iter()
                    .map(|&operand| self.lower_atom(operand))
                    .collect();
                self.split(result, 1, rest, terminator, target, |return_to| {
                    curios_cont::CpsNode::Intrinsic {
                        op,
                        args,
                        return_to,
                    }
                })
            }
        }
    }

    /// Lower a scalar operation to a straight-line `LetPrim`. `Byte` and
    /// `Nat` share the runtime carrier, so `ByteToNat` is the identity and
    /// `NatToByte` masks to a byte.
    fn lower_operation(
        &mut self,
        result: ValueId,
        operation: Operation,
        operands: &[Atom],
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        match operation {
            Operation::ByteToNat => {
                let atom = self.lower_atom(operands[0]);
                self.values.insert(result, atom);
                self.lower_statements(rest, terminator, target)
            }
            Operation::NatToByte => {
                let value = self.lower_atom(operands[0]);
                self.straight(result, rest, terminator, target, move |bound, next| {
                    curios_cont::CpsNode::LetPrim {
                        result: bound,
                        op: curios_cont::CpsPrimOp::NatAnd,
                        args: vec![
                            value,
                            curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(0xFF)),
                        ],
                        next,
                    }
                })
            }
            _ => {
                let op = operation_prim(operation);
                let args = operands.iter().map(|&atom| self.lower_atom(atom)).collect();
                self.straight(result, rest, terminator, target, move |bound, next| {
                    curios_cont::CpsNode::LetPrim {
                        result: bound,
                        op,
                        args,
                        next,
                    }
                })
            }
        }
    }

    // === Control splits ==================================================

    /// Open a join continuation that receives a control split's single result
    /// and runs the rest of the block; every arm delivers to it. A split in
    /// tail position — the block's last statement, whose result the block
    /// returns — delivers straight to the block's own target instead: no
    /// administrative join means a self-call in an arm returns to the
    /// function's return continuation and is genuinely tail, which is what
    /// lets Cont contify the recursion into a loop (a bodyless return
    /// continuation is not a forwarding target, so an eta join there would
    /// never collapse).
    fn open_join(
        &mut self,
        result: ValueId,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> (curios_cont::CpsContId, bool) {
        if rest.is_empty()
            && matches!(terminator, Terminator::Return(Atom::Value(returned)) if *returned == result)
        {
            return (target, false);
        }
        let join = self.open_join_fresh(result, rest, terminator, target);
        (join, true)
    }

    /// Open a join unconditionally — the fold loops route their exit through
    /// a `Switch` *edge*, and a bodyless return continuation cannot be a
    /// switch target, so they never take the tail bypass.
    fn open_join_fresh(
        &mut self,
        result: ValueId,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsContId {
        let join = self.module.reserve_continuation();
        let parameter = self.bind_value(result);
        let body = self.lower_statements(rest, terminator, target);
        self.module.define_continuation(
            join,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![parameter],
                body,
            },
        );
        join
    }

    /// Build a parameterless continuation lowering `block` into `join`.
    fn plain_arm(
        &mut self,
        block: BlockId,
        join: curios_cont::CpsContId,
    ) -> curios_cont::CpsContId {
        let continuation = self.module.reserve_continuation();
        let body = self.lower_block(block, join);
        self.module.define_continuation(
            continuation,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: Vec::new(),
                body,
            },
        );
        continuation
    }

    /// Lower a scalar switch: each `(key, block)` arm and the optional
    /// default becomes a parameterless continuation into the join, selected
    /// by one `Switch`.
    #[allow(clippy::too_many_arguments)]
    fn lower_switch(
        &mut self,
        result: ValueId,
        scrutinee: curios_cont::CpsAtom,
        arms: Vec<(u32, BlockId)>,
        default: Option<BlockId>,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        let (join, fresh) = self.open_join(result, rest, terminator, target);
        let mut continuations = if fresh { vec![join] } else { Vec::new() };
        let mut cases = BTreeMap::new();
        for (key, block) in arms {
            let continuation = self.plain_arm(block, join);
            continuations.push(continuation);
            cases.insert(
                key,
                curios_cont::CpsEdge {
                    target: continuation,
                    args: Vec::new(),
                },
            );
        }
        let default = default.map(|block| {
            let continuation = self.plain_arm(block, join);
            continuations.push(continuation);
            curios_cont::CpsEdge {
                target: continuation,
                args: Vec::new(),
            }
        });
        let switch = self.module.add_node(curios_cont::CpsNode::Switch {
            scrutinee,
            cases,
            default,
        });
        self.module.add_node(curios_cont::CpsNode::LetCont {
            continuations,
            body: switch,
        })
    }

    /// Lower a variant match: the tag (`TplGet(0)`) selects an arm through a
    /// `Switch`; each arm binds its payload positionally (`TplGet(1 + i)`)
    /// and delivers to the join.
    #[allow(clippy::too_many_arguments)]
    fn lower_match_variant(
        &mut self,
        result: ValueId,
        scrutinee: Atom,
        arms: &[VariantArm],
        default: Option<BlockId>,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        let scrutinee = self.lower_atom(scrutinee);
        let (join, fresh) = self.open_join(result, rest, terminator, target);

        let mut continuations = if fresh { vec![join] } else { Vec::new() };
        let mut cases = BTreeMap::new();
        for arm in arms {
            let continuation = self.lower_variant_arm(arm, scrutinee.clone(), join);
            continuations.push(continuation);
            cases.insert(
                self.constructor_tag(arm.constructor),
                curios_cont::CpsEdge {
                    target: continuation,
                    args: Vec::new(),
                },
            );
        }
        let default = default.map(|block| {
            let continuation = self.plain_arm(block, join);
            continuations.push(continuation);
            curios_cont::CpsEdge {
                target: continuation,
                args: Vec::new(),
            }
        });

        let tag = self.module.add_value(None);
        let switch = self.module.add_node(curios_cont::CpsNode::Switch {
            scrutinee: curios_cont::CpsAtom::Value(tag),
            cases,
            default,
        });
        let dispatch = self.module.add_node(curios_cont::CpsNode::LetPrim {
            result: tag,
            op: curios_cont::CpsPrimOp::TplGet(0),
            args: vec![scrutinee],
            next: switch,
        });
        self.module.add_node(curios_cont::CpsNode::LetCont {
            continuations,
            body: dispatch,
        })
    }

    fn lower_variant_arm(
        &mut self,
        arm: &VariantArm,
        scrutinee: curios_cont::CpsAtom,
        join: curios_cont::CpsContId,
    ) -> curios_cont::CpsContId {
        let bindings: Vec<curios_cont::CpsValueId> = arm
            .bindings
            .iter()
            .map(|&binder| self.bind_value(binder))
            .collect();
        let mut body = self.lower_block(arm.block, join);
        for index in (0..bindings.len()).rev() {
            body = self.module.add_node(curios_cont::CpsNode::LetPrim {
                result: bindings[index],
                op: curios_cont::CpsPrimOp::TplGet(index + 1),
                args: vec![scrutinee.clone()],
                next: body,
            });
        }
        let continuation = self.module.reserve_continuation();
        self.module.define_continuation(
            continuation,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: Vec::new(),
                body,
            },
        );
        continuation
    }

    // === Folds ===========================================================

    /// Lower a `Nat` induction to an up-counting loop `i = 0 … n` threading
    /// the accumulator: the zero block seeds it, and at each step the arena
    /// predecessor/hypothesis binders are the loop index and accumulator.
    #[allow(clippy::too_many_arguments)]
    fn lower_fold_nat(
        &mut self,
        result: ValueId,
        scrutinee: Atom,
        zero: BlockId,
        step: &FoldNatStep,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        let head = self.lower_atom(scrutinee);
        let join = self.open_join_fresh(result, rest, terminator, target);

        let loop_index = self.module.add_value(None);
        let loop_acc = self.module.add_value(None);
        let step_index = self.bind_value(step.predecessor);
        let step_acc = self.bind_value(step.hypothesis);
        let next_index = self.module.add_value(None);
        let next_acc = self.module.add_value(None);
        let comparison = self.module.add_value(None);
        let zero_acc = self.module.add_value(None);

        let loop_cont = self.module.reserve_continuation();
        let step_cont = self.module.reserve_continuation();
        let step_resume = self.module.reserve_continuation();
        let zero_resume = self.module.reserve_continuation();

        // step_resume(next_acc): increment the index and loop.
        let loop_back = self.jump(
            loop_cont,
            vec![
                curios_cont::CpsAtom::Value(next_index),
                curios_cont::CpsAtom::Value(next_acc),
            ],
        );
        let increment = self.module.add_node(curios_cont::CpsNode::LetPrim {
            result: next_index,
            op: curios_cont::CpsPrimOp::NatAdd,
            args: vec![
                curios_cont::CpsAtom::Value(step_index),
                curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(1)),
            ],
            next: loop_back,
        });
        self.module.define_continuation(
            step_resume,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![next_acc],
                body: increment,
            },
        );

        // step_cont(step_index, step_acc): run the step block, then resume.
        let step_body = self.lower_block(step.block, step_resume);
        let step_body = self.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![step_resume],
            body: step_body,
        });
        self.module.define_continuation(
            step_cont,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![step_index, step_acc],
                body: step_body,
            },
        );

        // loop_cont(loop_index, loop_acc): step until the index reaches n.
        let switch = self.module.add_node(curios_cont::CpsNode::Switch {
            scrutinee: curios_cont::CpsAtom::Value(comparison),
            cases: BTreeMap::from([(
                0,
                curios_cont::CpsEdge {
                    target: step_cont,
                    args: vec![
                        curios_cont::CpsAtom::Value(loop_index),
                        curios_cont::CpsAtom::Value(loop_acc),
                    ],
                },
            )]),
            default: Some(curios_cont::CpsEdge {
                target: join,
                args: vec![curios_cont::CpsAtom::Value(loop_acc)],
            }),
        });
        let loop_body = self.module.add_node(curios_cont::CpsNode::LetPrim {
            result: comparison,
            op: curios_cont::CpsPrimOp::NatEql,
            args: vec![curios_cont::CpsAtom::Value(loop_index), head],
            next: switch,
        });
        self.module.define_continuation(
            loop_cont,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![loop_index, loop_acc],
                body: loop_body,
            },
        );

        // zero_resume(zero_acc): enter the loop at index 0 with the base.
        let zero_jump = self.jump(
            loop_cont,
            vec![
                curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(0)),
                curios_cont::CpsAtom::Value(zero_acc),
            ],
        );
        self.module.define_continuation(
            zero_resume,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![zero_acc],
                body: zero_jump,
            },
        );

        let entry = self.lower_block(zero, zero_resume);
        self.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![join, loop_cont, step_cont, zero_resume],
            body: entry,
        })
    }

    /// Lower a sequence right fold to a backward loop `i = len … 0`: the
    /// empty block seeds the accumulator; each step reads `seq[i - 1]` and
    /// the suffix `seq[i ..]` and folds it in.
    #[allow(clippy::too_many_arguments)]
    fn lower_fold_sequence(
        &mut self,
        result: ValueId,
        grain: SequenceGrain,
        scrutinee: Atom,
        empty: BlockId,
        step: &FoldSequenceStep,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        let sequence = self.lower_atom(scrutinee);
        let join = self.open_join_fresh(result, rest, terminator, target);

        let length = self.module.add_value(None);
        let loop_index = self.module.add_value(None);
        let loop_acc = self.module.add_value(None);
        let comparison = self.module.add_value(None);
        let step_index = self.module.add_value(None);
        let step_acc = self.bind_value(step.accumulator);
        let element_index = self.module.add_value(None);
        let element = self.bind_value(step.element);
        let suffix = self.bind_value(step.suffix);
        let base_acc = self.module.add_value(None);
        let next_acc = self.module.add_value(None);

        let loop_cont = self.module.reserve_continuation();
        let step_cont = self.module.reserve_continuation();
        let step_resume = self.module.reserve_continuation();
        let empty_resume = self.module.reserve_continuation();

        // step_resume(next_acc): continue the loop at the element's index.
        let loop_back = self.jump(
            loop_cont,
            vec![
                curios_cont::CpsAtom::Value(element_index),
                curios_cont::CpsAtom::Value(next_acc),
            ],
        );
        self.module.define_continuation(
            step_resume,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![next_acc],
                body: loop_back,
            },
        );

        // step_cont(step_index, step_acc): extract seq[i-1], seq[i..], fold.
        let step_body = self.lower_block(step.block, step_resume);
        let step_body = self.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![step_resume],
            body: step_body,
        });
        let step_body = self.module.add_node(curios_cont::CpsNode::LetPrim {
            result: suffix,
            op: sequence_slice_op(grain),
            args: vec![
                sequence.clone(),
                curios_cont::CpsAtom::Value(step_index),
                curios_cont::CpsAtom::Value(length),
            ],
            next: step_body,
        });
        let step_body = self.module.add_node(curios_cont::CpsNode::LetPrim {
            result: element,
            op: sequence_get_op(grain),
            args: vec![sequence.clone(), curios_cont::CpsAtom::Value(element_index)],
            next: step_body,
        });
        let step_body = self.module.add_node(curios_cont::CpsNode::LetPrim {
            result: element_index,
            op: curios_cont::CpsPrimOp::NatSub,
            args: vec![
                curios_cont::CpsAtom::Value(step_index),
                curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(1)),
            ],
            next: step_body,
        });
        self.module.define_continuation(
            step_cont,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![step_index, step_acc],
                body: step_body,
            },
        );

        // loop_cont(loop_index, loop_acc): fold until the index reaches 0.
        let switch = self.module.add_node(curios_cont::CpsNode::Switch {
            scrutinee: curios_cont::CpsAtom::Value(comparison),
            cases: BTreeMap::from([(
                0,
                curios_cont::CpsEdge {
                    target: step_cont,
                    args: vec![
                        curios_cont::CpsAtom::Value(loop_index),
                        curios_cont::CpsAtom::Value(loop_acc),
                    ],
                },
            )]),
            default: Some(curios_cont::CpsEdge {
                target: join,
                args: vec![curios_cont::CpsAtom::Value(loop_acc)],
            }),
        });
        let loop_body = self.module.add_node(curios_cont::CpsNode::LetPrim {
            result: comparison,
            op: curios_cont::CpsPrimOp::NatEql,
            args: vec![
                curios_cont::CpsAtom::Value(loop_index),
                curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(0)),
            ],
            next: switch,
        });
        self.module.define_continuation(
            loop_cont,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![loop_index, loop_acc],
                body: loop_body,
            },
        );

        // empty_resume(base_acc): enter the loop at the end with the base.
        let empty_jump = self.jump(
            loop_cont,
            vec![
                curios_cont::CpsAtom::Value(length),
                curios_cont::CpsAtom::Value(base_acc),
            ],
        );
        self.module.define_continuation(
            empty_resume,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![base_acc],
                body: empty_jump,
            },
        );

        let entry = self.lower_block(empty, empty_resume);
        let entry = self.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![join, loop_cont, step_cont, empty_resume],
            body: entry,
        });
        // Compute the length up front so every continuation sees it.
        self.module.add_node(curios_cont::CpsNode::LetPrim {
            result: length,
            op: sequence_len_op(grain),
            args: vec![sequence],
            next: entry,
        })
    }

    // === Effects =========================================================

    /// Lower a control-splitting statement (an application, cell, or
    /// intrinsic) returning to a fresh join. The join receives the
    /// statement's results — one for value-producing forms, zero for a cell
    /// write, whose bound result is the unit carrier.
    fn split(
        &mut self,
        result: ValueId,
        result_arity: usize,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
        make: impl FnOnce(curios_cont::CpsContId) -> curios_cont::CpsNode,
    ) -> curios_cont::CpsNodeId {
        // The same tail bypass as `open_join`: a single-result split whose
        // value the block immediately returns delivers to the block's target.
        if result_arity == 1
            && rest.is_empty()
            && matches!(terminator, Terminator::Return(Atom::Value(returned)) if *returned == result)
        {
            return self.module.add_node(make(target));
        }
        let join = self.module.reserve_continuation();
        let params = if result_arity == 0 {
            self.values.insert(
                result,
                curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(0)),
            );
            Vec::new()
        } else {
            vec![self.bind_value(result)]
        };
        let body = self.lower_statements(rest, terminator, target);
        self.module.define_continuation(
            join,
            curios_cont::CpsContinuation {
                debug_name: None,
                params,
                body,
            },
        );
        let node = self.module.add_node(make(join));
        self.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![join],
            body: node,
        })
    }

    /// Lower a host call. A single-result foreign returns straight to the
    /// block's join; a multi-result foreign returns to a resume continuation
    /// that packs the results into the record tuple the consuming code
    /// projects through.
    fn lower_foreign(
        &mut self,
        result: ValueId,
        function: Arc<ForeignFunction>,
        args: Vec<curios_cont::CpsAtom>,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        let arity = function.signature.results.len();
        if arity == 1 {
            return self.split(result, 1, rest, terminator, target, |return_to| {
                curios_cont::CpsNode::Foreign {
                    function,
                    args,
                    return_to,
                }
            });
        }

        let results: Vec<curios_cont::CpsValueId> =
            (0..arity).map(|_| self.module.add_value(None)).collect();
        let record = self.module.add_value(None);
        self.values
            .insert(result, curios_cont::CpsAtom::Value(record));
        let next = self.lower_statements(rest, terminator, target);
        let pack = self.module.add_node(curios_cont::CpsNode::LetValue {
            result: record,
            value: curios_cont::CpsValueExpr::Tuple(
                results
                    .iter()
                    .copied()
                    .map(curios_cont::CpsAtom::Value)
                    .collect(),
            ),
            next,
        });
        let resume = self.module.reserve_continuation();
        self.module.define_continuation(
            resume,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: results,
                body: pack,
            },
        );
        let call = self.module.add_node(curios_cont::CpsNode::Foreign {
            function,
            args,
            return_to: resume,
        });
        self.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![resume],
            body: call,
        })
    }

    // === Atoms and identities ============================================

    /// Bind a straight-line result: allocate its Cont value, lower the rest
    /// of the block, and emit the node `make` builds in front of it.
    fn straight(
        &mut self,
        result: ValueId,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
        make: impl FnOnce(curios_cont::CpsValueId, curios_cont::CpsNodeId) -> curios_cont::CpsNode,
    ) -> curios_cont::CpsNodeId {
        let bound = self.bind_value(result);
        let next = self.lower_statements(rest, terminator, target);
        self.module.add_node(make(bound, next))
    }

    /// Allocate the Cont value representing an arena value, carrying its
    /// source hint, and record the mapping — the single choke point for every
    /// binder that names a source value.
    fn bind_value(&mut self, arena: ValueId) -> curios_cont::CpsValueId {
        let name = self.arena_value_name(arena);
        let cont = self.module.add_value(name);
        self.values.insert(arena, curios_cont::CpsAtom::Value(cont));
        cont
    }

    fn arena_value_name(&self, id: ValueId) -> Option<String> {
        self.source
            .value(id)
            .and_then(|value| value.debug_name.clone())
    }

    /// The runtime tag of a constructor: its position within its family.
    fn constructor_tag(&self, constructor: ConstructorId) -> u32 {
        let family = self
            .source
            .constructor(constructor)
            .expect("live constructor")
            .family;
        self.source
            .family(family)
            .expect("live family")
            .constructors
            .iter()
            .position(|&candidate| candidate == constructor)
            .expect("a constructor belongs to its family") as u32
    }

    fn lower_callee(&self, atom: Atom) -> curios_cont::CpsCallee {
        match self.lower_atom(atom) {
            curios_cont::CpsAtom::Fun(function) => curios_cont::CpsCallee::Known(function),
            curios_cont::CpsAtom::Value(value) => curios_cont::CpsCallee::Closure(value),
            curios_cont::CpsAtom::Literal(_) => {
                panic!("arena application head lowered to a literal")
            }
        }
    }

    fn lower_atom(&self, atom: Atom) -> curios_cont::CpsAtom {
        match atom {
            Atom::Value(value) => self
                .values
                .get(&value)
                .unwrap_or_else(|| panic!("arena lowering lacks value {value}"))
                .clone(),
            Atom::Function(function) => curios_cont::CpsAtom::Fun(
                *self
                    .functions
                    .get(&function)
                    .unwrap_or_else(|| panic!("arena lowering lacks function {function}")),
            ),
            Atom::Constant(constant) => {
                curios_cont::CpsAtom::Literal(self.lower_constant(constant))
            }
        }
    }

    fn lower_constant(&self, constant: ConstantId) -> curios_cont::CpsLiteral {
        match self.source.constant(constant).expect("live constant") {
            // Unit, Bool, and Byte collapse onto the Nat runtime carrier here,
            // at the one-way door — never earlier.
            Constant::Unit => curios_cont::CpsLiteral::Nat(0),
            Constant::Bool(value) => curios_cont::CpsLiteral::Nat(u32::from(*value)),
            Constant::Nat(value) => curios_cont::CpsLiteral::Nat(*value),
            Constant::Byte(value) => curios_cont::CpsLiteral::Nat(u32::from(*value)),
            Constant::Int(value) => curios_cont::CpsLiteral::Int(*value),
            Constant::Flt(value) => curios_cont::CpsLiteral::Flt(value.to_f32()),
            Constant::Bin(grain, value) => curios_cont::CpsLiteral::Bin(*grain, value.clone()),
            // A Handle descriptor token rides the packed-binary carrier: its
            // little-endian bytes at byte grain.
            Constant::Handle(token) => curios_cont::CpsLiteral::Bin(
                Grain::X,
                PackedBin::from_bytes(BigUint::from(*token).to_bytes_le()),
            ),
        }
    }
}

/// The Cont primitive of a scalar [`Operation`]. `Bool` operations run on the
/// `0`/`1` `Nat` carrier (`BoolNeq` is xor on a single bit) and `Byte`
/// comparisons on the `Nat` carrier; `HandleEql` is packed-binary equality at
/// byte grain. The `Byte` conversions are handled before this table.
fn operation_prim(operation: Operation) -> curios_cont::CpsPrimOp {
    use Operation as O;
    match operation {
        O::BoolAnd => curios_cont::CpsPrimOp::NatAnd,
        O::BoolOr => curios_cont::CpsPrimOp::NatOr,
        O::BoolXor => curios_cont::CpsPrimOp::NatXor,
        O::BoolEql => curios_cont::CpsPrimOp::NatEql,
        O::BoolNeq => curios_cont::CpsPrimOp::NatXor,
        O::NatEql => curios_cont::CpsPrimOp::NatEql,
        O::NatNeq => curios_cont::CpsPrimOp::NatNeq,
        O::NatAdd => curios_cont::CpsPrimOp::NatAdd,
        O::NatSub => curios_cont::CpsPrimOp::NatSub,
        O::NatMul => curios_cont::CpsPrimOp::NatMul,
        O::NatLt => curios_cont::CpsPrimOp::NatLt,
        O::NatDiv => curios_cont::CpsPrimOp::NatDiv,
        O::NatRem => curios_cont::CpsPrimOp::NatRem,
        O::NatGt => curios_cont::CpsPrimOp::NatGt,
        O::NatLte => curios_cont::CpsPrimOp::NatLte,
        O::NatGte => curios_cont::CpsPrimOp::NatGte,
        O::NatAnd => curios_cont::CpsPrimOp::NatAnd,
        O::NatOr => curios_cont::CpsPrimOp::NatOr,
        O::NatXor => curios_cont::CpsPrimOp::NatXor,
        O::NatShl => curios_cont::CpsPrimOp::NatShl,
        O::NatShr => curios_cont::CpsPrimOp::NatShr,
        O::NatRotl => curios_cont::CpsPrimOp::NatRotl,
        O::NatRotr => curios_cont::CpsPrimOp::NatRotr,
        O::NatClz => curios_cont::CpsPrimOp::NatClz,
        O::NatCtz => curios_cont::CpsPrimOp::NatCtz,
        O::NatPopcnt => curios_cont::CpsPrimOp::NatPopcnt,
        O::ByteEql => curios_cont::CpsPrimOp::NatEql,
        O::ByteLt => curios_cont::CpsPrimOp::NatLt,
        O::ByteLte => curios_cont::CpsPrimOp::NatLte,
        O::ByteGt => curios_cont::CpsPrimOp::NatGt,
        O::ByteGte => curios_cont::CpsPrimOp::NatGte,
        O::IntEql => curios_cont::CpsPrimOp::IntEql,
        O::IntNeq => curios_cont::CpsPrimOp::IntNeq,
        O::IntAdd => curios_cont::CpsPrimOp::IntAdd,
        O::IntSub => curios_cont::CpsPrimOp::IntSub,
        O::IntMul => curios_cont::CpsPrimOp::IntMul,
        O::IntDiv => curios_cont::CpsPrimOp::IntDiv,
        O::IntRem => curios_cont::CpsPrimOp::IntRem,
        O::IntLt => curios_cont::CpsPrimOp::IntLt,
        O::IntGt => curios_cont::CpsPrimOp::IntGt,
        O::IntLte => curios_cont::CpsPrimOp::IntLte,
        O::IntGte => curios_cont::CpsPrimOp::IntGte,
        O::IntAnd => curios_cont::CpsPrimOp::IntAnd,
        O::IntOr => curios_cont::CpsPrimOp::IntOr,
        O::IntXor => curios_cont::CpsPrimOp::IntXor,
        O::IntShl => curios_cont::CpsPrimOp::IntShl,
        O::IntShr => curios_cont::CpsPrimOp::IntShr,
        O::IntRotl => curios_cont::CpsPrimOp::IntRotl,
        O::IntRotr => curios_cont::CpsPrimOp::IntRotr,
        O::IntClz => curios_cont::CpsPrimOp::IntClz,
        O::IntCtz => curios_cont::CpsPrimOp::IntCtz,
        O::IntPopcnt => curios_cont::CpsPrimOp::IntPopcnt,
        O::FltAdd => curios_cont::CpsPrimOp::FltAdd,
        O::FltSub => curios_cont::CpsPrimOp::FltSub,
        O::FltMul => curios_cont::CpsPrimOp::FltMul,
        O::FltDiv => curios_cont::CpsPrimOp::FltDiv,
        O::FltRem => curios_cont::CpsPrimOp::FltRem,
        O::FltEql => curios_cont::CpsPrimOp::FltEql,
        O::FltNeq => curios_cont::CpsPrimOp::FltNeq,
        O::FltLt => curios_cont::CpsPrimOp::FltLt,
        O::FltGt => curios_cont::CpsPrimOp::FltGt,
        O::FltLte => curios_cont::CpsPrimOp::FltLte,
        O::FltGte => curios_cont::CpsPrimOp::FltGte,
        O::FltMin => curios_cont::CpsPrimOp::FltMin,
        O::FltMax => curios_cont::CpsPrimOp::FltMax,
        O::FltCopysign => curios_cont::CpsPrimOp::FltCopysign,
        O::FltNeg => curios_cont::CpsPrimOp::FltNeg,
        O::FltAbs => curios_cont::CpsPrimOp::FltAbs,
        O::FltSqrt => curios_cont::CpsPrimOp::FltSqrt,
        O::FltFloor => curios_cont::CpsPrimOp::FltFloor,
        O::FltCeil => curios_cont::CpsPrimOp::FltCeil,
        O::FltTrunc => curios_cont::CpsPrimOp::FltTrunc,
        O::FltNearest => curios_cont::CpsPrimOp::FltNearest,
        O::NatToInt => curios_cont::CpsPrimOp::NatToInt,
        O::NatToFlt => curios_cont::CpsPrimOp::NatToFlt,
        O::IntToNat => curios_cont::CpsPrimOp::IntToNat,
        O::IntToFlt => curios_cont::CpsPrimOp::IntToFlt,
        O::FltToNat => curios_cont::CpsPrimOp::FltToNat,
        O::FltToInt => curios_cont::CpsPrimOp::FltToInt,
        O::FltToLeBytes => curios_cont::CpsPrimOp::FltToLeBytes,
        O::FltOfLeBytes => curios_cont::CpsPrimOp::FltOfLeBytes,
        O::HandleEql => curios_cont::CpsPrimOp::BinEql(Grain::X),
        O::ByteToNat | O::NatToByte => {
            unreachable!("Byte conversions are lowered before the primitive table")
        }
    }
}

/// The Cont primitive of a [`SequenceOp`], threading the operand count into
/// the variadic concatenations. `LstBuild` is a list value, never a prim.
fn sequence_prim(operation: SequenceOp, arity: usize) -> curios_cont::CpsPrimOp {
    use SequenceOp as S;
    match operation {
        S::BinLen(grain) => curios_cont::CpsPrimOp::BinLen(grain),
        S::BinEql(grain) => curios_cont::CpsPrimOp::BinEql(grain),
        S::BinGet(grain) => curios_cont::CpsPrimOp::BinGet(grain),
        S::BinSlice(grain) => curios_cont::CpsPrimOp::BinSlice(grain),
        S::BinAppend(grain) => curios_cont::CpsPrimOp::BinAppend(grain),
        S::BinConcat(grain) => curios_cont::CpsPrimOp::BinConcat(grain, arity),
        S::LstLen => curios_cont::CpsPrimOp::LstLen,
        S::LstGet => curios_cont::CpsPrimOp::LstGet,
        S::LstSlice => curios_cont::CpsPrimOp::LstSlice,
        S::LstAppend => curios_cont::CpsPrimOp::LstAppend,
        S::LstConcat => curios_cont::CpsPrimOp::LstConcat(arity),
        S::LstBuild => unreachable!("LstBuild is lowered as a list value"),
    }
}

fn cell_op(operation: CellOperation) -> curios_cont::CpsCellOp {
    match operation {
        CellOperation::New => curios_cont::CpsCellOp::New,
        CellOperation::Get => curios_cont::CpsCellOp::Get,
        CellOperation::Set => curios_cont::CpsCellOp::Set,
    }
}

fn sequence_len_op(grain: SequenceGrain) -> curios_cont::CpsPrimOp {
    match grain {
        SequenceGrain::List => curios_cont::CpsPrimOp::LstLen,
        SequenceGrain::Bin(grain) => curios_cont::CpsPrimOp::BinLen(grain),
    }
}

fn sequence_get_op(grain: SequenceGrain) -> curios_cont::CpsPrimOp {
    match grain {
        SequenceGrain::List => curios_cont::CpsPrimOp::LstGet,
        SequenceGrain::Bin(grain) => curios_cont::CpsPrimOp::BinGet(grain),
    }
}

fn sequence_slice_op(grain: SequenceGrain) -> curios_cont::CpsPrimOp {
    match grain {
        SequenceGrain::List => curios_cont::CpsPrimOp::LstSlice,
        SequenceGrain::Bin(grain) => curios_cont::CpsPrimOp::BinSlice(grain),
    }
}
