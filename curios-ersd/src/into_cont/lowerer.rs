//! The walk that decides what to emit: one pass over the erased arena, statement by statement and block by block, in continuation-passing order.
//!
//! Every method here answers "what does this construct become", and hands the answer to the [`Emitter`] that writes it or the [`Layout`] that shapes it. The split is what keeps this file about control flow alone — nothing below mints a Cont value or lays out a row, and nothing there descends into a block.

use {
    super::{
        Analysis, Atom, Block, BlockId, ConstructorId, Emitter, FamilyEncoding, FamilyId,
        FoldNatStep, FoldSequenceStep, Function, FunctionId, KnotMember, Layout, Module, Operation,
        RecGroup, RecGroupId, Rhs, SequenceFacts, SequenceGrain, SequenceOp, Statement,
        StatementId, Terminator, UNFORCED, UnconsSequenceStep, ValueId, VariantArm, cell_op,
        operation_intrinsic, sequence_census, sequence_intrinsic, sequence_len_op,
    },
    crate::Intrinsic,
    curios_abi::ForeignFunction,
    curios_utilities::recurse,
    std::{collections::BTreeMap, sync::Arc},
};

pub(super) struct Lowerer<'a> {
    source: &'a Module,
    /// Use counts over the finished arena, read only to decline emitting a binding nothing reads. The module does not change during lowering, so one analysis taken at entry stays exact throughout.
    analysis: Analysis,
    /// The sequence-usage census's verdicts, read at every construction site to settle the stores into indexed-only fields.
    facts: SequenceFacts,
    /// The Cont module being built, and what each erased name has become in it.
    emitter: Emitter<'a>,
    /// What every nominal shape's Cont heap type is and what it is called, memoized across the lowering.
    layout: Layout<'a>,
}

impl<'a> Lowerer<'a> {
    /// Open a lowering over a verified arena module: the two analyses it consults are taken once here, because the module does not change while it runs.
    pub(super) fn new(source: &'a Module) -> Self {
        Self {
            source,
            analysis: Analysis::analyze(source),
            facts: sequence_census(source),
            emitter: Emitter::new(source),
            layout: Layout::new(source),
        }
    }

    /// Run the lowering and hand over the Cont module it built.
    ///
    /// The arena's top level — its item chain followed by its entry block — becomes the parameterless entry `main`, delivering to a bodyless `return_cont`. The result is verified before it leaves: an invalid module here is a lowering bug, not a user error.
    pub(super) fn finish(mut self) -> curios_cont::CpsModule {
        let main = self.emitter.module.reserve_function();
        let return_cont = self.emitter.module.reserve_continuation();
        let entry = self
            .source
            .entry()
            .expect("a finalized module has an entry");
        let entry = self.source.block(entry).expect("live entry block").clone();
        let mut statements: Vec<StatementId> = self.source.items().to_vec();
        statements.extend(&entry.statements);
        let body = self.lower_statements(&statements, &entry.terminator, return_cont);
        self.emitter.module.define_function(
            main,
            curios_cont::CpsFunction {
                debug_name: Some("main".into()),
                params: Vec::new(),
                return_cont,
                body,
            },
        );
        self.emitter.module.set_entry(main);

        self.emitter
            .module
            .verify()
            .unwrap_or_else(|error| panic!("arena lowering produced invalid Cont: {error}"));
        self.emitter.module
    }
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
    pub(super) fn lower_statements(
        &mut self,
        statements: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        recurse(|| self.lower_statements_within(statements, terminator, target))
    }

    fn lower_statements_within(
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
                self.emitter
                    .module
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
                let atom = self.emitter.lower_atom(*atom);
                self.emitter.jump(target, vec![atom])
            }
            Terminator::Exit(atom) => {
                let atom = self.emitter.lower_atom(*atom);
                self.emitter
                    .module
                    .add_node(curios_cont::CpsNode::Exit { value: Some(atom) })
            }
            Terminator::Unreachable => self
                .emitter
                .module
                .add_node(curios_cont::CpsNode::Unreachable),
        }
    }

    /// Reserve every function of a group before defining any, so a member body can reference itself and its siblings; return the Cont ids in group order.
    fn lower_function_group(&mut self, functions: &[FunctionId]) -> Vec<curios_cont::CpsFunId> {
        let ids: Vec<curios_cont::CpsFunId> = functions
            .iter()
            .map(|&arena| {
                let id = self.emitter.module.reserve_function();
                self.emitter.functions.insert(arena, id);
                id
            })
            .collect();
        for (&arena, &id) in functions.iter().zip(&ids) {
            self.define_function(arena, id);
        }
        ids
    }

    /// Define a reserved Cont function from its arena function. A body that references a knot's computed member reads it from the knot's cell at its own entry — at call time, once the knot is tied — rather than capturing the value directly.
    fn define_function(&mut self, arena: FunctionId, id: curios_cont::CpsFunId) {
        let function: Function = self.source.function(arena).expect("live function").clone();
        let return_cont = self.emitter.module.reserve_continuation();
        let params = function
            .params
            .iter()
            .map(|&param| self.emitter.bind_value(param))
            .collect();
        let members = if self.emitter.knot_members.is_empty() {
            Vec::new()
        } else {
            self.emitter.block_member_refs(function.body)
        };
        let body = self.with_cell_reads(members, |lowerer| {
            lowerer.lower_block(function.body, return_cont)
        });
        self.emitter.module.define_function(
            id,
            curios_cont::CpsFunction {
                debug_name: function.debug_name.clone(),
                params,
                return_cont,
                body,
            },
        );
    }

    /// Lower a recursive group by its shape: a function-only group is a plain `LetFun` (erasure emits `Functions` for those, so this is totality); any group with a computed member is a knot tied through cells (see [`Lowerer::lower_knot`]), its function members bound beside the cells.
    fn lower_rec_group(
        &mut self,
        group: RecGroupId,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
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
                .module
                .add_node(curios_cont::CpsNode::LetFun { functions, body });
        }

        self.lower_knot(&group, rest, terminator, target)
    }

    /// Tie a recursive knot by need. Every computed member gets a cell, reserved empty before anything else so the closures built below can capture it, and a *force* function: read the cell, and by its state hand the value back, run the initializer and store what it produced, or trap — the third state is the initializer already running, and a read inside it is a cycle no order satisfies. The initializers themselves become nullary functions stored unforced in their cells; the function members bind beside the force functions; and every reference to a member, wherever it stands, is a call of its force function at the referencing region's entry (see [`Lowerer::with_cell_reads`]).
    ///
    /// Forcing on first use is what the language means by a recursive value and what the compile-time evaluator already did for a closed knot — `force_toplevel` treats a member as a CAF with a cycle guard — so the erased program now agrees with both on every forward reference, whatever the verifier can or cannot see through. The lowering once ran the initializers eagerly in an order it computed and later in source order, handing each a cell holding a placeholder and then nothing; a member read out of order computed with the placeholder, then trapped, and now computes what it should. What makes by need *sound* is the rule the verifier holds a knot to: an initializer performs no effect, so running it later, or not at all, is unobservable, and the only behaviours forcing can move are a trap and divergence, which it only delays.
    ///
    /// Function members take the same forcing reads at their own entry as any other function (see [`Lowerer::define_function`]), so a member that forward-references a computed value is served by the cell however it is reached — called directly, escaped as a closure, or copied by a later pass into a closure the initializers build. That uniformity is the point: a knot with function members once lowered to a `RecInit` node whose machine lowering patched *escaping member* closures at the ready point, and a closure born inside an initializer that merely *called* a member — `wrap((n) => helper(n))` beside `helper(n) = first(n)` — captured the computed value before it existed, which nothing below the CPS verifier's lexical scope rules could see.
    fn lower_knot(
        &mut self,
        group: &RecGroup,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        let row = self.emitter.knot_row();
        let members: Vec<KnotMember> = group
            .values
            .iter()
            .map(|member| {
                let knot = KnotMember {
                    cell: self.emitter.module.add_value(None),
                    force: self.emitter.module.reserve_function(),
                };
                self.emitter.knot_members.insert(member.value, knot);
                knot
            })
            .collect();
        for (member, knot) in group.values.iter().zip(&members) {
            self.emitter
                .define_force(row, *knot, self.emitter.arena_value_name(member.value));
        }

        // The members are in the map before any body below is lowered, so a reference to a computed sibling — from a function member, a thunk, or the rest — is a forcing read at its entry.
        let functions = self.lower_function_group(&group.functions);
        let thunks: Vec<curios_cont::CpsFunId> = group
            .values
            .iter()
            .map(|member| {
                self.define_thunk(member.init, self.emitter.arena_value_name(member.value))
            })
            .collect();

        // Downstream forces the members it references.
        let ready_members = self.emitter.eager_member_refs(rest, terminator);
        let mut body = self.with_cell_reads(ready_members, |lowerer| {
            lowerer.lower_statements(rest, terminator, target)
        });

        // Store every thunk unforced, inside out. The cells exist and the thunks are bound, so no read can come between a reservation and its store.
        for (knot, thunk) in members.iter().zip(&thunks).rev() {
            let after_store = self.emitter.module.reserve_continuation();
            self.emitter.module.define_continuation(
                after_store,
                curios_cont::CpsContinuation {
                    debug_name: None,
                    params: Vec::new(),
                    body,
                },
            );
            let unforced = self.emitter.module.add_value(None);
            let store = self.emitter.module.add_node(curios_cont::CpsNode::Cell {
                op: curios_cont::CpsCellOp::Set,
                args: vec![
                    curios_cont::CpsAtom::Value(knot.cell),
                    curios_cont::CpsAtom::Value(unforced),
                ],
                return_to: after_store,
            });
            let store = self.emitter.module.add_node(curios_cont::CpsNode::LetCont {
                continuations: vec![after_store],
                body: store,
            });
            body = self
                .emitter
                .module
                .add_node(curios_cont::CpsNode::LetValue {
                    result: unforced,
                    value: curios_cont::CpsValueExpr::Row(
                        row,
                        vec![
                            curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(UNFORCED)),
                            curios_cont::CpsAtom::Fun(*thunk),
                        ],
                    ),
                    next: store,
                });
        }

        // Binding order, outside in: the cells; the force functions, which capture the cells; the function members, which call the force functions; the thunks, which call both.
        body = self.emitter.module.add_node(curios_cont::CpsNode::LetFun {
            functions: thunks,
            body,
        });
        if !functions.is_empty() {
            body = self
                .emitter
                .module
                .add_node(curios_cont::CpsNode::LetFun { functions, body });
        }
        body = self.emitter.module.add_node(curios_cont::CpsNode::LetFun {
            functions: members.iter().map(|knot| knot.force).collect(),
            body,
        });
        for knot in members.iter().rev() {
            let bound = self.emitter.module.reserve_continuation();
            self.emitter.module.define_continuation(
                bound,
                curios_cont::CpsContinuation {
                    debug_name: None,
                    params: vec![knot.cell],
                    body,
                },
            );
            let reserve = self.emitter.module.add_node(curios_cont::CpsNode::Cell {
                op: curios_cont::CpsCellOp::Reserve,
                args: Vec::new(),
                return_to: bound,
            });
            body = self.emitter.module.add_node(curios_cont::CpsNode::LetCont {
                continuations: vec![bound],
                body: reserve,
            });
        }
        body
    }

    /// A member's initializer as a nullary function: what its cell holds until something forces it. It takes its own forcing reads at entry, so the members it depends on are computed before it runs — which is the by-need order, found by running rather than computed ahead.
    fn define_thunk(&mut self, init: BlockId, hint: Option<String>) -> curios_cont::CpsFunId {
        let thunk = self.emitter.module.reserve_function();
        let return_cont = self.emitter.module.reserve_continuation();
        let init_members = self.emitter.block_member_refs(init);
        let body = self.with_cell_reads(init_members, |lowerer| {
            lowerer.lower_block(init, return_cont)
        });
        self.emitter.module.define_function(
            thunk,
            curios_cont::CpsFunction {
                debug_name: hint.map(|hint| format!("{hint}/init")),
                params: Vec::new(),
                return_cont,
                body,
            },
        );
        thunk
    }

    /// Run `build` with each member bound to a fresh local holding its forced value, wrapping the result so the forcing happens at entry. A closure forces at its own entry — when it runs — rather than at its construction, which may be inside the initializer of the very member it names.
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
            curios_cont::CpsFunId,
            Option<curios_cont::CpsAtom>,
        )> = members
            .into_iter()
            .map(|member| {
                let force = self.emitter.knot_members[&member].force;
                let local = self
                    .emitter
                    .module
                    .add_value(self.emitter.arena_value_name(member));
                let previous = self
                    .emitter
                    .values
                    .insert(member, curios_cont::CpsAtom::Value(local));
                (member, local, force, previous)
            })
            .collect();
        let mut body = build(self);
        for (member, _, _, previous) in &reads {
            match previous {
                Some(atom) => {
                    self.emitter.values.insert(*member, atom.clone());
                }
                None => {
                    self.emitter.values.remove(member);
                }
            }
        }
        for &(_, local, force, _) in reads.iter().rev() {
            let resume = self.emitter.module.reserve_continuation();
            self.emitter.module.define_continuation(
                resume,
                curios_cont::CpsContinuation {
                    debug_name: None,
                    params: vec![local],
                    body,
                },
            );
            let forcing = self
                .emitter
                .module
                .add_node(curios_cont::CpsNode::ApplyFun {
                    callee: curios_cont::CpsCallee::Known(force),
                    args: Vec::new(),
                    return_to: resume,
                });
            body = self.emitter.module.add_node(curios_cont::CpsNode::LetCont {
                continuations: vec![resume],
                body: forcing,
            });
        }
        body
    }

    fn lower_let(
        &mut self,
        result: ValueId,
        rhs: &Rhs,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        match rhs {
            // Aliasing binds an already-computed atom: record the mapping and continue; no Cont node is needed.
            Rhs::Alias(atom) => {
                let atom = self.emitter.lower_atom(*atom);
                self.emitter.values.insert(result, atom);
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
                let args: Vec<curios_cont::CpsAtom> = operands
                    .iter()
                    .map(|&atom| self.emitter.lower_atom(atom))
                    .collect();
                if let SequenceOp::ListBuild = operation {
                    return self.straight(result, rest, terminator, target, move |bound, next| {
                        curios_cont::CpsNode::LetValue {
                            result: bound,
                            value: curios_cont::CpsValueExpr::List(args),
                            next,
                        }
                    });
                }
                let op = sequence_intrinsic(*operation, args.len());
                self.straight(result, rest, terminator, target, move |bound, next| {
                    curios_cont::CpsNode::LetIntrinsic {
                        result: bound,
                        op,
                        args,
                        next,
                    }
                })
            }
            Rhs::Apply { callee, arguments } => {
                let callee = self.emitter.lower_callee(*callee);
                let args = arguments
                    .iter()
                    .map(|&atom| self.emitter.lower_atom(atom))
                    .collect();
                self.split(result, 1, rest, terminator, target, |return_to| {
                    curios_cont::CpsNode::ApplyFun {
                        callee,
                        args,
                        return_to,
                    }
                })
            }
            Rhs::Product { schema, fields } => {
                // A shared row stays a structural tuple: it is the row a multi-result split also builds, and that site names no schema to agree with.
                let places = match self.layout.is_shared(*schema) {
                    true => (0..fields.len()).collect(),
                    false => self.layout.product_slots(&mut self.emitter.module, *schema),
                };
                let width = match self.layout.is_shared(*schema) {
                    true => fields.len(),
                    false => {
                        self.layout
                            .product_layout(&mut self.emitter.module, *schema)
                            .width
                    }
                };
                // One writer, so every slot is filled and no filler is ever placed.
                let mut atoms = vec![curios_cont::CpsAtom::Filler; width];
                let mut marked = vec![false; width];
                for (field, &atom) in fields.iter().enumerate() {
                    atoms[places[field]] = self.emitter.lower_atom(atom);
                    marked[places[field]] = self.facts.indexed_only_product(*schema, field);
                }
                let settles = self.emitter.settle_stores(&marked, &mut atoms);
                let value = match self.layout.is_shared(*schema) {
                    true => curios_cont::CpsValueExpr::Tuple(atoms),
                    false => curios_cont::CpsValueExpr::Row(
                        self.layout
                            .product_identity(&mut self.emitter.module, *schema),
                        atoms,
                    ),
                };
                let bound = self.emitter.bind_value(result);
                let next = self.lower_statements(rest, terminator, target);
                let node = self
                    .emitter
                    .module
                    .add_node(curios_cont::CpsNode::LetValue {
                        result: bound,
                        value,
                        next,
                    });
                self.emitter.wrap_settles(settles, node)
            }
            Rhs::Construct {
                constructor,
                fields,
            } => match self
                .layout
                .family_encoding(self.layout.constructor_family(*constructor))
            {
                // A collapsed construction with at most one payload builds nothing: the result is the payload atom itself (or the interned zero), recorded as an alias in the value map, so downstream code reads the value where the tuple would have been. A marked single field still settles — the value *is* the store — through an ordinary binding instead of the alias.
                FamilyEncoding::Collapsed if fields.len() <= 1 => {
                    if let Some(&payload) = fields.first()
                        && self.facts.indexed_only_constructor(*constructor, 0)
                    {
                        let atom = self.emitter.lower_atom(payload);
                        self.straight(result, rest, terminator, target, |bound, next| {
                            curios_cont::CpsNode::LetIntrinsic {
                                result: bound,
                                op: curios_cont::CpsIntrinsic::ListSettle,
                                args: vec![atom],
                                next,
                            }
                        })
                    } else {
                        let value = match fields.first() {
                            Some(&payload) => self.emitter.lower_atom(payload),
                            None => curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(0)),
                        };
                        self.emitter.values.insert(result, value);
                        self.lower_statements(rest, terminator, target)
                    }
                }
                // Its own nominal row, with no tag: nothing needs discriminating, so it encodes exactly as the struct with the same relevant row does — which is what keeps that equivalence true now that a struct's row is keyed by its schema rather than by its arity.
                FamilyEncoding::Collapsed => {
                    let owner = self.layout.constructor_family(*constructor);
                    let row = self.layout.row_identity(&mut self.emitter.module, owner);
                    let places = self
                        .layout
                        .constructor_slots(&mut self.emitter.module, *constructor);
                    let width = self.layout.row_width(&mut self.emitter.module, owner);
                    let mut atoms = vec![curios_cont::CpsAtom::Filler; width];
                    let mut marked = vec![false; width];
                    for (field, &atom) in fields.iter().enumerate() {
                        atoms[places[field]] = self.emitter.lower_atom(atom);
                        marked[places[field]] =
                            self.facts.indexed_only_constructor(*constructor, field);
                    }
                    let settles = self.emitter.settle_stores(&marked, &mut atoms);
                    let bound = self.emitter.bind_value(result);
                    let next = self.lower_statements(rest, terminator, target);
                    let node = self
                        .emitter
                        .module
                        .add_node(curios_cont::CpsNode::LetValue {
                            result: bound,
                            value: curios_cont::CpsValueExpr::Row(row, atoms),
                            next,
                        });
                    self.emitter.wrap_settles(settles, node)
                }
                // The immediate-unary constructor rides bare: the payload is always an immediate, so the value *is* the payload and the tag is never minted. An immediate is never a list, so no settle applies.
                FamilyEncoding::Immediate { constructor: bare } if bare == *constructor => {
                    let payload = self.emitter.lower_atom(fields[0]);
                    self.emitter.values.insert(result, payload);
                    self.lower_statements(rest, terminator, target)
                }
                FamilyEncoding::Tagged | FamilyEncoding::Immediate { .. } => {
                    let tag = self.layout.constructor_tag(*constructor);
                    let owner = self.layout.constructor_family(*constructor);
                    let family = self.layout.row_identity(&mut self.emitter.module, owner);
                    // Every construction of a family carries every slot, so a narrow constructor is the same heap type as its widest sibling and every read of the family is one exact cast. A slot this constructor does not write takes the filler, which carries no value — the destination's carrier is not known until the backend decides it.
                    let width = self.layout.row_width(&mut self.emitter.module, owner);
                    let places = self
                        .layout
                        .constructor_slots(&mut self.emitter.module, *constructor);
                    let mut atoms = vec![curios_cont::CpsAtom::Filler; width];
                    let mut marked = vec![false; width];
                    atoms[0] = curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(tag));
                    for (field, &atom) in fields.iter().enumerate() {
                        atoms[places[field]] = self.emitter.lower_atom(atom);
                        marked[places[field]] =
                            self.facts.indexed_only_constructor(*constructor, field);
                    }
                    let settles = self.emitter.settle_stores(&marked, &mut atoms);
                    let bound = self.emitter.bind_value(result);
                    let next = self.lower_statements(rest, terminator, target);
                    let node = self
                        .emitter
                        .module
                        .add_node(curios_cont::CpsNode::LetValue {
                            result: bound,
                            value: curios_cont::CpsValueExpr::Row(family, atoms),
                            next,
                        });
                    self.emitter.wrap_settles(settles, node)
                }
            },
            Rhs::Project {
                schema,
                product,
                field,
            } => {
                let op = match self.layout.is_shared(*schema) {
                    true => curios_cont::CpsIntrinsic::TupleGet(*field as usize),
                    false => curios_cont::CpsIntrinsic::RowGet(
                        self.layout
                            .product_identity(&mut self.emitter.module, *schema),
                        self.layout.product_slots(&mut self.emitter.module, *schema)
                            [*field as usize],
                    ),
                };
                let product = self.emitter.lower_atom(*product);
                self.straight(result, rest, terminator, target, move |bound, next| {
                    curios_cont::CpsNode::LetIntrinsic {
                        result: bound,
                        op,
                        args: vec![product],
                        next,
                    }
                })
            }
            Rhs::MatchVariant {
                family,
                scrutinee,
                arms,
                default,
            } => self.lower_match_variant(
                *family, result, *scrutinee, arms, *default, rest, terminator, target,
            ),
            Rhs::SwitchBool {
                scrutinee,
                if_false,
                if_true,
            } => {
                let scrutinee = self.emitter.lower_atom(*scrutinee);
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
                let scrutinee = self.emitter.lower_atom(*scrutinee);
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
            Rhs::UnconsSequence {
                grain,
                scrutinee,
                empty,
                cons,
            } => self.lower_uncons_sequence(
                result, *grain, *scrutinee, *empty, cons, rest, terminator, target,
            ),
            Rhs::Cell {
                operation,
                operands,
            } => {
                let op = cell_op(*operation);
                let args = operands
                    .iter()
                    .map(|&atom| self.emitter.lower_atom(atom))
                    .collect();
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
                let args = operands
                    .iter()
                    .map(|&atom| self.emitter.lower_atom(atom))
                    .collect();
                self.lower_foreign(result, function, args, rest, terminator, target)
            }
            Rhs::Intrinsic {
                intrinsic,
                operands,
            } => {
                let op = match intrinsic {
                    Intrinsic::ListMap => curios_cont::CpsIntrinsicCall::ListMap,
                };
                // Both representations bind the list first, then the mapper; the operands transcribe in order.
                let args = operands
                    .iter()
                    .map(|&operand| self.emitter.lower_atom(operand))
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

    /// Lower a scalar operation to a straight-line `LetIntrinsic`. `Byte` and `Nat` share the runtime carrier, so `ByteToNat` is the identity and `NatToByte` masks to a byte.
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
                let atom = self.emitter.lower_atom(operands[0]);
                self.emitter.values.insert(result, atom);
                self.lower_statements(rest, terminator, target)
            }
            Operation::NatToByte => {
                let value = self.emitter.lower_atom(operands[0]);
                self.straight(result, rest, terminator, target, move |bound, next| {
                    curios_cont::CpsNode::LetIntrinsic {
                        result: bound,
                        op: curios_cont::CpsIntrinsic::NatAnd,
                        args: vec![
                            value,
                            curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(0xFF)),
                        ],
                        next,
                    }
                })
            }
            _ => {
                let op = operation_intrinsic(operation);
                let args = operands
                    .iter()
                    .map(|&atom| self.emitter.lower_atom(atom))
                    .collect();
                self.straight(result, rest, terminator, target, move |bound, next| {
                    curios_cont::CpsNode::LetIntrinsic {
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

    /// Open a join continuation that receives a control split's single result and runs the rest of the block; every arm delivers to it. A split in tail position — the block's last statement, whose result the block returns — delivers straight to the block's own target instead: no administrative join means a self-call in an arm returns to the function's return continuation and is genuinely tail, which is what lets Cont contify the recursion into a loop (a bodyless return continuation is not a forwarding target, so an eta join there would never collapse).
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

    /// Open a join unconditionally — the fold loops route their exit through a `Switch` *edge*, and a bodyless return continuation cannot be a switch target, so they never take the tail bypass.
    fn open_join_fresh(
        &mut self,
        result: ValueId,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsContId {
        let join = self.emitter.module.reserve_continuation();
        let parameter = self.emitter.bind_value(result);
        let body = self.lower_statements(rest, terminator, target);
        self.emitter.module.define_continuation(
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
        let continuation = self.emitter.module.reserve_continuation();
        let body = self.lower_block(block, join);
        self.emitter.module.define_continuation(
            continuation,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: Vec::new(),
                body,
            },
        );
        continuation
    }

    /// Lower a scalar switch: each `(key, block)` arm and the optional default becomes a parameterless continuation into the join, selected by one `Switch`.
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
        let switch = self.emitter.module.add_node(curios_cont::CpsNode::Switch {
            scrutinee,
            cases,
            default,
        });
        self.emitter.module.add_node(curios_cont::CpsNode::LetCont {
            continuations,
            body: switch,
        })
    }

    /// Lower a variant match: the tag (`TupleGet(0)`) selects an arm through a `Switch`; each arm binds its payload positionally (`TupleGet(1 + i)`) and delivers to the join. A [`FamilyEncoding::Collapsed`] family has nothing to decide — its single arm (or the default, when the arm is absent) runs unconditionally, inline rather than behind a dispatch.
    #[allow(clippy::too_many_arguments)]
    fn lower_match_variant(
        &mut self,
        family: FamilyId,
        result: ValueId,
        scrutinee: Atom,
        arms: &[VariantArm],
        default: Option<BlockId>,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        let scrutinee = self.emitter.lower_atom(scrutinee);

        match self.layout.family_encoding(family) {
            FamilyEncoding::Collapsed => {
                let (join, fresh) = self.open_join(result, rest, terminator, target);
                let body = match arms.first() {
                    Some(arm) => self.lower_collapsed_arm(arm, scrutinee, join),
                    None => {
                        let block = default.expect("a variant match covers its family");
                        self.lower_block(block, join)
                    }
                };
                return match fresh {
                    true => self.emitter.module.add_node(curios_cont::CpsNode::LetCont {
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
                    result,
                    scrutinee,
                    arms,
                    default,
                    rest,
                    terminator,
                    target,
                );
            }
            FamilyEncoding::Tagged => {}
        }

        let identity = self.layout.row_identity(&mut self.emitter.module, family);
        let (join, fresh) = self.open_join(result, rest, terminator, target);

        let mut continuations = if fresh { vec![join] } else { Vec::new() };
        let mut cases = BTreeMap::new();
        for arm in arms {
            let continuation = self.lower_variant_arm(identity, arm, scrutinee.clone(), join);
            continuations.push(continuation);
            cases.insert(
                self.layout.constructor_tag(arm.constructor),
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

        let tag = self.emitter.module.add_value(None);
        let switch = self.emitter.module.add_node(curios_cont::CpsNode::Switch {
            scrutinee: curios_cont::CpsAtom::Value(tag),
            cases,
            default,
        });
        let dispatch = self
            .emitter
            .module
            .add_node(curios_cont::CpsNode::LetIntrinsic {
                result: tag,
                op: curios_cont::CpsIntrinsic::RowGet(identity, 0),
                args: vec![scrutinee],
                next: switch,
            });
        self.emitter.module.add_node(curios_cont::CpsNode::LetCont {
            continuations,
            body: dispatch,
        })
    }

    /// Lower a match on a [`FamilyEncoding::Immediate`] family: an `IsImmediate` test splits immediate from struct. The immediate side is the bare-payload arm with its binder aliased to the scrutinee; the boxed side keeps the ordinary tagged dispatch minus the immediate case — elided entirely when the family has exactly one boxed constructor, since the test already decided everything.
    #[allow(clippy::too_many_arguments)]
    fn lower_immediate_match(
        &mut self,
        family: FamilyId,
        immediate: ConstructorId,
        result: ValueId,
        scrutinee: curios_cont::CpsAtom,
        arms: &[VariantArm],
        default: Option<BlockId>,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        let (join, fresh) = self.open_join(result, rest, terminator, target);
        let mut continuations = if fresh { vec![join] } else { Vec::new() };

        let default_cont = default.map(|block| {
            let continuation = self.plain_arm(block, join);
            continuations.push(continuation);
            continuation
        });

        let immediate_target = match arms.iter().find(|arm| arm.constructor == immediate) {
            Some(arm) => {
                let body = self.lower_immediate_arm(arm, scrutinee.clone(), join);
                let continuation = self.emitter.module.reserve_continuation();
                self.emitter.module.define_continuation(
                    continuation,
                    curios_cont::CpsContinuation {
                        debug_name: None,
                        params: Vec::new(),
                        body,
                    },
                );
                continuations.push(continuation);
                continuation
            }
            None => default_cont.expect("a variant match covers its family"),
        };

        let identity = self.layout.row_identity(&mut self.emitter.module, family);
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
                        self.layout.constructor_tag(arm.constructor),
                        curios_cont::CpsEdge {
                            target: continuation,
                            args: Vec::new(),
                        },
                    );
                }
                let default = default_cont.map(|continuation| curios_cont::CpsEdge {
                    target: continuation,
                    args: Vec::new(),
                });
                let tag = self.emitter.module.add_value(None);
                let switch = self.emitter.module.add_node(curios_cont::CpsNode::Switch {
                    scrutinee: curios_cont::CpsAtom::Value(tag),
                    cases,
                    default,
                });
                let body = self
                    .emitter
                    .module
                    .add_node(curios_cont::CpsNode::LetIntrinsic {
                        result: tag,
                        op: curios_cont::CpsIntrinsic::RowGet(identity, 0),
                        args: vec![scrutinee.clone()],
                        next: switch,
                    });
                let continuation = self.emitter.module.reserve_continuation();
                self.emitter.module.define_continuation(
                    continuation,
                    curios_cont::CpsContinuation {
                        debug_name: None,
                        params: Vec::new(),
                        body,
                    },
                );
                continuations.push(continuation);
                continuation
            }
        };

        let kind = self.emitter.module.add_value(None);
        let switch = self.emitter.module.add_node(curios_cont::CpsNode::Switch {
            scrutinee: curios_cont::CpsAtom::Value(kind),
            cases: BTreeMap::from([(
                1,
                curios_cont::CpsEdge {
                    target: immediate_target,
                    args: Vec::new(),
                },
            )]),
            default: Some(curios_cont::CpsEdge {
                target: boxed_target,
                args: Vec::new(),
            }),
        });
        let dispatch = self
            .emitter
            .module
            .add_node(curios_cont::CpsNode::LetIntrinsic {
                result: kind,
                op: curios_cont::CpsIntrinsic::IsImmediate,
                args: vec![scrutinee],
                next: switch,
            });
        self.emitter.module.add_node(curios_cont::CpsNode::LetCont {
            continuations,
            body: dispatch,
        })
    }

    /// One collapsed arm body: a lone payload aliases the scrutinee, which *is* the payload under the collapsed encoding; a wider row projects untagged fields. Returns a body rather than a continuation because the caller inlines it with no dispatch to target it.
    ///
    /// The aliasing is sound *here* and only here. A collapsed family has one constructor, so the scrutinee is the payload on every path there is. The immediate encoding looks like the same shape and is not — its scrutinee is a scalar on one path and a tuple on the other — so it binds through [`lower_immediate_arm`](Self::lower_immediate_arm) instead. Sharing this function with it miscompiled a loop that did arithmetic on the payload; see [`curios_cont::CpsIntrinsic::ImmediateGet`].
    fn lower_collapsed_arm(
        &mut self,
        arm: &VariantArm,
        scrutinee: curios_cont::CpsAtom,
        join: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        if let [binder] = arm.bindings.as_slice() {
            self.emitter.values.insert(*binder, scrutinee);
            return self.lower_block(arm.block, join);
        }
        let bindings: Vec<curios_cont::CpsValueId> = arm
            .bindings
            .iter()
            .map(|&binder| self.emitter.bind_value(binder))
            .collect();
        let row = self.layout.row_identity(
            &mut self.emitter.module,
            self.layout.constructor_family(arm.constructor),
        );
        let places = self
            .layout
            .constructor_slots(&mut self.emitter.module, arm.constructor);
        let mut body = self.lower_block(arm.block, join);
        for index in (0..bindings.len()).rev() {
            body = self
                .emitter
                .module
                .add_node(curios_cont::CpsNode::LetIntrinsic {
                    result: bindings[index],
                    op: curios_cont::CpsIntrinsic::RowGet(row, places[index]),
                    args: vec![scrutinee.clone()],
                    next: body,
                });
        }
        body
    }

    /// One immediate-encoded arm body: the payload is the scrutinee's value, bound through [`curios_cont::CpsIntrinsic::ImmediateGet`] rather than aliased to it, so it has a definition of its own for the representation analysis to read a carrier off.
    ///
    /// Exactly one binding, always: the encoding admits only a unary constructor, so there is no wider row to project and a different shape is this lowering's own contract broken rather than a program's fault.
    fn lower_immediate_arm(
        &mut self,
        arm: &VariantArm,
        scrutinee: curios_cont::CpsAtom,
        join: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        let [binder] = arm.bindings.as_slice() else {
            panic!("an immediate constructor binds exactly its one payload")
        };
        let bound = self.emitter.bind_value(*binder);
        let body = self.lower_block(arm.block, join);
        self.emitter
            .module
            .add_node(curios_cont::CpsNode::LetIntrinsic {
                result: bound,
                op: curios_cont::CpsIntrinsic::ImmediateGet,
                args: vec![scrutinee],
                next: body,
            })
    }

    fn lower_variant_arm(
        &mut self,
        family: curios_cont::CpsRowId,
        arm: &VariantArm,
        scrutinee: curios_cont::CpsAtom,
        join: curios_cont::CpsContId,
    ) -> curios_cont::CpsContId {
        let bindings: Vec<curios_cont::CpsValueId> = arm
            .bindings
            .iter()
            .map(|&binder| self.emitter.bind_value(binder))
            .collect();
        let places = self
            .layout
            .constructor_slots(&mut self.emitter.module, arm.constructor);
        let mut body = self.lower_block(arm.block, join);
        for index in (0..bindings.len()).rev() {
            body = self
                .emitter
                .module
                .add_node(curios_cont::CpsNode::LetIntrinsic {
                    result: bindings[index],
                    op: curios_cont::CpsIntrinsic::RowGet(family, places[index]),
                    args: vec![scrutinee.clone()],
                    next: body,
                });
        }
        let continuation = self.emitter.module.reserve_continuation();
        self.emitter.module.define_continuation(
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

    /// Lower a `Nat` induction to an up-counting loop `i = 0 … n` threading the accumulator: the zero block seeds it, and at each step the arena predecessor/hypothesis binders are the loop index and accumulator.
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
        let head = self.emitter.lower_atom(scrutinee);
        let join = self.open_join_fresh(result, rest, terminator, target);

        let loop_index = self.emitter.module.add_value(None);
        let loop_acc = self.emitter.module.add_value(None);
        let step_index = self.emitter.bind_value(step.predecessor);
        let step_acc = self.emitter.bind_value(step.hypothesis);
        let next_index = self.emitter.module.add_value(None);
        let next_acc = self.emitter.module.add_value(None);
        let comparison = self.emitter.module.add_value(None);
        let zero_acc = self.emitter.module.add_value(None);

        let loop_cont = self.emitter.module.reserve_continuation();
        let step_cont = self.emitter.module.reserve_continuation();
        let step_resume = self.emitter.module.reserve_continuation();
        let zero_resume = self.emitter.module.reserve_continuation();

        // step_resume(next_acc): increment the index and loop.
        let loop_back = self.emitter.jump(
            loop_cont,
            vec![
                curios_cont::CpsAtom::Value(next_index),
                curios_cont::CpsAtom::Value(next_acc),
            ],
        );
        let increment = self
            .emitter
            .module
            .add_node(curios_cont::CpsNode::LetIntrinsic {
                result: next_index,
                op: curios_cont::CpsIntrinsic::NatAdd,
                args: vec![
                    curios_cont::CpsAtom::Value(step_index),
                    curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(1)),
                ],
                next: loop_back,
            });
        self.emitter.module.define_continuation(
            step_resume,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![next_acc],
                body: increment,
            },
        );

        // step_cont(step_index, step_acc): run the step block, then resume.
        let step_body = self.lower_block(step.block, step_resume);
        let step_body = self.emitter.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![step_resume],
            body: step_body,
        });
        self.emitter.module.define_continuation(
            step_cont,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![step_index, step_acc],
                body: step_body,
            },
        );

        // loop_cont(loop_index, loop_acc): step until the index reaches n.
        let switch = self.emitter.module.add_node(curios_cont::CpsNode::Switch {
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
        let loop_body = self
            .emitter
            .module
            .add_node(curios_cont::CpsNode::LetIntrinsic {
                result: comparison,
                op: curios_cont::CpsIntrinsic::NatEql,
                args: vec![curios_cont::CpsAtom::Value(loop_index), head],
                next: switch,
            });
        self.emitter.module.define_continuation(
            loop_cont,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![loop_index, loop_acc],
                body: loop_body,
            },
        );

        // zero_resume(zero_acc): enter the loop at index 0 with the base.
        let zero_jump = self.emitter.jump(
            loop_cont,
            vec![
                curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(0)),
                curios_cont::CpsAtom::Value(zero_acc),
            ],
        );
        self.emitter.module.define_continuation(
            zero_resume,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![zero_acc],
                body: zero_jump,
            },
        );

        let entry = self.lower_block(zero, zero_resume);
        self.emitter.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![join, loop_cont, step_cont, zero_resume],
            body: entry,
        })
    }

    /// Lower a sequence case split to a length dispatch over one peel: zero takes the empty block, anything else peels the head and the suffix after it.
    ///
    /// The reads land *inside* the non-empty arm. Beside the dispatch they would run on the empty sequence too, where both are out of range — a trap where the program has an answer.
    #[allow(clippy::too_many_arguments)]
    fn lower_uncons_sequence(
        &mut self,
        result: ValueId,
        grain: SequenceGrain,
        scrutinee: Atom,
        empty: BlockId,
        cons: &UnconsSequenceStep,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        let sequence = self.emitter.lower_atom(scrutinee);
        let (join, fresh) = self.open_join(result, rest, terminator, target);
        let mut continuations = if fresh { vec![join] } else { Vec::new() };

        let length = self.emitter.module.add_value(None);
        let element = self.emitter.bind_value(cons.element);
        // Declined where nothing reads it, for the reason the fold's is: a suffix allocates a rope view, and this is the only place that may decline to build one — a later pass cannot drop a read on the grounds its result is dead.
        let suffix = (self.analysis.value_uses(cons.suffix) > 0)
            .then(|| self.emitter.bind_value(cons.suffix));

        let empty_arm = self.plain_arm(empty, join);
        continuations.push(empty_arm);

        let cons_arm = self.emitter.module.reserve_continuation();
        let cons_body = self.lower_block(cons.block, join);
        let cons_body = self.emitter.emit_peel(
            grain,
            &sequence,
            element,
            curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(0)),
            suffix.map(|suffix| {
                (
                    suffix,
                    curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(1)),
                )
            }),
            cons_body,
        );
        self.emitter.module.define_continuation(
            cons_arm,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: Vec::new(),
                body: cons_body,
            },
        );
        continuations.push(cons_arm);

        let dispatch = self.emitter.module.add_node(curios_cont::CpsNode::Switch {
            scrutinee: curios_cont::CpsAtom::Value(length),
            cases: BTreeMap::from([(
                0,
                curios_cont::CpsEdge {
                    target: empty_arm,
                    args: Vec::new(),
                },
            )]),
            default: Some(curios_cont::CpsEdge {
                target: cons_arm,
                args: Vec::new(),
            }),
        });
        let dispatch = self.emitter.module.add_node(curios_cont::CpsNode::LetCont {
            continuations,
            body: dispatch,
        });

        self.emitter
            .module
            .add_node(curios_cont::CpsNode::LetIntrinsic {
                result: length,
                op: sequence_len_op(grain),
                args: vec![sequence],
                next: dispatch,
            })
    }

    /// Lower a sequence right fold to a backward loop `i = len … 0`: the empty block seeds the accumulator; each step reads `seq[i - 1]` and the suffix `seq[i ..]` and folds it in.
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
        let sequence = self.emitter.lower_atom(scrutinee);
        let join = self.open_join_fresh(result, rest, terminator, target);

        let length = self.emitter.module.add_value(None);
        let loop_index = self.emitter.module.add_value(None);
        let loop_acc = self.emitter.module.add_value(None);
        let comparison = self.emitter.module.add_value(None);
        let step_index = self.emitter.module.add_value(None);
        let step_acc = self.emitter.bind_value(step.accumulator);
        let element_index = self.emitter.module.add_value(None);
        let element = self.emitter.bind_value(step.element);
        // A step almost never mentions its suffix — `Bytes/fold` and `List/fold` do not, and neither does a step whose only use of it is an argument to a `Prop` constructor, since erasure removes that. Binding it regardless costs one rope-view allocation per element, inside the loop, for a value nothing reads. **This is the only place that can decline to emit it:** a slice may trap, so no later pass may drop one on the grounds that its result is dead, and the fact that this one cannot trap is a property of the loop emitted below rather than anything recoverable downstream.
        let suffix = (self.analysis.value_uses(step.suffix) > 0)
            .then(|| self.emitter.bind_value(step.suffix));
        let base_acc = self.emitter.module.add_value(None);
        let next_acc = self.emitter.module.add_value(None);

        let loop_cont = self.emitter.module.reserve_continuation();
        let step_cont = self.emitter.module.reserve_continuation();
        let step_resume = self.emitter.module.reserve_continuation();
        let empty_resume = self.emitter.module.reserve_continuation();

        // step_resume(next_acc): continue the loop at the element's index.
        let loop_back = self.emitter.jump(
            loop_cont,
            vec![
                curios_cont::CpsAtom::Value(element_index),
                curios_cont::CpsAtom::Value(next_acc),
            ],
        );
        self.emitter.module.define_continuation(
            step_resume,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![next_acc],
                body: loop_back,
            },
        );

        // step_cont(step_index, step_acc): extract seq[i-1], seq[i..], fold.
        let step_body = self.lower_block(step.block, step_resume);
        let step_body = self.emitter.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![step_resume],
            body: step_body,
        });
        let step_body = self.emitter.emit_peel(
            grain,
            &sequence,
            element,
            curios_cont::CpsAtom::Value(element_index),
            suffix.map(|suffix| (suffix, curios_cont::CpsAtom::Value(step_index))),
            step_body,
        );
        let step_body = self
            .emitter
            .module
            .add_node(curios_cont::CpsNode::LetIntrinsic {
                result: element_index,
                op: curios_cont::CpsIntrinsic::NatSub,
                args: vec![
                    curios_cont::CpsAtom::Value(step_index),
                    curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(1)),
                ],
                next: step_body,
            });
        self.emitter.module.define_continuation(
            step_cont,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![step_index, step_acc],
                body: step_body,
            },
        );

        // loop_cont(loop_index, loop_acc): fold until the index reaches 0.
        let switch = self.emitter.module.add_node(curios_cont::CpsNode::Switch {
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
        let loop_body = self
            .emitter
            .module
            .add_node(curios_cont::CpsNode::LetIntrinsic {
                result: comparison,
                op: curios_cont::CpsIntrinsic::NatEql,
                args: vec![
                    curios_cont::CpsAtom::Value(loop_index),
                    curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(0)),
                ],
                next: switch,
            });
        self.emitter.module.define_continuation(
            loop_cont,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![loop_index, loop_acc],
                body: loop_body,
            },
        );

        // empty_resume(base_acc): enter the loop at the end with the base.
        let empty_jump = self.emitter.jump(
            loop_cont,
            vec![
                curios_cont::CpsAtom::Value(length),
                curios_cont::CpsAtom::Value(base_acc),
            ],
        );
        self.emitter.module.define_continuation(
            empty_resume,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![base_acc],
                body: empty_jump,
            },
        );

        let entry = self.lower_block(empty, empty_resume);
        let entry = self.emitter.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![join, loop_cont, step_cont, empty_resume],
            body: entry,
        });
        // Compute the length up front so every continuation sees it.
        self.emitter
            .module
            .add_node(curios_cont::CpsNode::LetIntrinsic {
                result: length,
                op: sequence_len_op(grain),
                args: vec![sequence],
                next: entry,
            })
    }

    // === Effects =========================================================

    /// Lower a control-splitting statement (an application, cell, or intrinsic) returning to a fresh join. The join receives the statement's results — one for value-producing forms, zero for a cell write, whose bound result is the unit carrier.
    fn split(
        &mut self,
        result: ValueId,
        result_arity: usize,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
        make: impl FnOnce(curios_cont::CpsContId) -> curios_cont::CpsNode,
    ) -> curios_cont::CpsNodeId {
        // The same tail bypass as `open_join`: a single-result split whose value the block immediately returns delivers to the block's target.
        if result_arity == 1
            && rest.is_empty()
            && matches!(terminator, Terminator::Return(Atom::Value(returned)) if *returned == result)
        {
            return self.emitter.module.add_node(make(target));
        }
        let join = self.emitter.module.reserve_continuation();
        let params = if result_arity == 0 {
            self.emitter.values.insert(
                result,
                curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(0)),
            );
            Vec::new()
        } else {
            vec![self.emitter.bind_value(result)]
        };
        let body = self.lower_statements(rest, terminator, target);
        self.emitter.module.define_continuation(
            join,
            curios_cont::CpsContinuation {
                debug_name: None,
                params,
                body,
            },
        );
        let node = self.emitter.module.add_node(make(join));
        self.emitter.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![join],
            body: node,
        })
    }

    /// Lower a host call. A single-result foreign returns straight to the block's join; a multi-result foreign returns to a resume continuation that packs the results into the record tuple the consuming code projects through.
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

        let results: Vec<curios_cont::CpsValueId> = (0..arity)
            .map(|_| self.emitter.module.add_value(None))
            .collect();
        let record = self.emitter.module.add_value(None);
        self.emitter
            .values
            .insert(result, curios_cont::CpsAtom::Value(record));
        let next = self.lower_statements(rest, terminator, target);
        let pack = self
            .emitter
            .module
            .add_node(curios_cont::CpsNode::LetValue {
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
        let resume = self.emitter.module.reserve_continuation();
        self.emitter.module.define_continuation(
            resume,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: results,
                body: pack,
            },
        );
        let call = self.emitter.module.add_node(curios_cont::CpsNode::Foreign {
            function,
            args,
            return_to: resume,
        });
        self.emitter.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![resume],
            body: call,
        })
    }

    // === Atoms and identities ============================================

    /// Bind a straight-line result: allocate its Cont value, lower the rest of the block, and emit the node `make` builds in front of it.
    fn straight(
        &mut self,
        result: ValueId,
        rest: &[StatementId],
        terminator: &Terminator,
        target: curios_cont::CpsContId,
        make: impl FnOnce(curios_cont::CpsValueId, curios_cont::CpsNodeId) -> curios_cont::CpsNode,
    ) -> curios_cont::CpsNodeId {
        let bound = self.emitter.bind_value(result);
        let next = self.lower_statements(rest, terminator, target);
        self.emitter.module.add_node(make(bound, next))
    }
}
