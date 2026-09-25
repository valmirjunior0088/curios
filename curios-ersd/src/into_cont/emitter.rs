//! The Cont module under construction, and what each erased name has become in it.
//!
//! The lowering above this is a walk that decides *what* to emit; this is everything the walk emits *through*. It owns the Cont module being built and the three correspondences that make a write possible — an arena value's Cont atom, an arena function's Cont identity, and a knot member's storage and forcing function — so that deciding and emitting are not the same object's business.
//!
//! Nothing here descends into a block or chooses a branch. Every method either mints one Cont node, translates one erased name, answers one question about the knot, or emits a fixed instruction sequence whose shape does not depend on what it is wrapping. [`Emitter::define_force`] is the largest and still obeys that rule: a knot member's forcing function is the same protocol whatever the member computes.

use {
    super::{
        Atom, BlockId, Constant, ConstantId, FunctionId, Module, SequenceGrain, Statement,
        StatementId, Terminator, ValueId, edge, sequence_get_op, sequence_rest_op,
    },
    curios_abi::Handle,
    curios_num::{Binary, Grain, Natural},
    std::collections::{BTreeMap, BTreeSet},
};

/// One computed member: a write-once result cell, a capacity-one initializer channel, and its forcing function.
#[derive(Clone, Copy)]
pub(super) struct KnotMember {
    pub(super) result: curios_cont::ValueId,
    pub(super) initializer: curios_cont::ValueId,
    pub(super) force: curios_cont::FunctionId,
}

/// The Cont module being built, with the erased-to-Cont correspondence that every write consults.
pub(super) struct Emitter<'a> {
    source: &'a Module,
    pub(super) module: curios_cont::Module,
    pub(super) values: BTreeMap<ValueId, curios_cont::Atom>,
    pub(super) functions: BTreeMap<FunctionId, curios_cont::FunctionId>,
    /// Computed members of recursive knots, mapped to its result cell, initializer channel, and forcing function. A reference to such a member lowers to a call of that function at the referencing region's entry, so the member is computed on first use, once, and the tie is invisible to everything but this lowering.
    pub(super) knot_members: BTreeMap<ValueId, KnotMember>,
}

impl<'a> Emitter<'a> {
    pub(super) fn new(source: &'a Module) -> Self {
        Self {
            source,
            module: curios_cont::Module::new(),
            values: BTreeMap::new(),
            functions: BTreeMap::new(),
            knot_members: BTreeMap::new(),
        }
    }

    /// Allocate the Cont value representing an arena value, carrying its source hint, and record the mapping — the single choke point for every binder that names a source value.
    pub(super) fn bind_value(&mut self, arena: ValueId) -> curios_cont::ValueId {
        let name = self.arena_value_name(arena);
        let cont = self.module.add_value(name);
        self.values.insert(arena, curios_cont::Atom::Value(cont));
        cont
    }

    pub(super) fn arena_value_name(&self, id: ValueId) -> Option<String> {
        self.source
            .value(id)
            .and_then(|value| value.debug_name.clone())
    }

    pub(super) fn lower_callee(&self, atom: Atom) -> curios_cont::Callee {
        match self.lower_atom(atom) {
            curios_cont::Atom::Fun(function) => curios_cont::Callee::Known(function),
            curios_cont::Atom::Value(value) => curios_cont::Callee::Closure(value),
            curios_cont::Atom::Literal(_) | curios_cont::Atom::Filler => {
                panic!("arena application head lowered to a literal")
            }
        }
    }

    pub(super) fn lower_atom(&self, atom: Atom) -> curios_cont::Atom {
        match atom {
            Atom::Value(value) => self
                .values
                .get(&value)
                .unwrap_or_else(|| panic!("arena lowering lacks value {value}"))
                .clone(),
            Atom::Function(function) => curios_cont::Atom::Fun(
                *self
                    .functions
                    .get(&function)
                    .unwrap_or_else(|| panic!("arena lowering lacks function {function}")),
            ),
            Atom::Constant(constant) => curios_cont::Atom::Literal(self.lower_constant(constant)),
        }
    }

    pub(super) fn lower_constant(&self, constant: ConstantId) -> curios_cont::Literal {
        match self.source.constant(constant).expect("live constant") {
            // Unit, Bool, and Byte collapse onto the Nat runtime carrier here, at the one-way door — never earlier.
            Constant::Unit => curios_cont::Literal::Nat(Natural::zero()),
            Constant::Bool(value) => curios_cont::Literal::Nat(Natural::from(u32::from(*value))),
            Constant::Nat(value) => curios_cont::Literal::Nat(value.clone()),
            Constant::Byte(value) => curios_cont::Literal::Nat(Natural::from(u32::from(*value))),
            Constant::Int(value) => curios_cont::Literal::Int(value.clone()),
            Constant::Flt(value) => curios_cont::Literal::Flt(*value),
            Constant::Bin(grain, value) => curios_cont::Literal::Bin(*grain, value.clone()),
            // A Handle descriptor token rides the packed-binary carrier at byte grain, spelled by the one encoding the host reads back.
            Constant::Handle(token) => curios_cont::Literal::Bin(
                Grain::X,
                Binary::from_bytes(Handle::encode(&Natural::from(*token))),
            ),
        }
    }

    /// A parameterless continuation over `body`, for a switch arm.
    pub(super) fn continuation_of(
        &mut self,
        body: curios_cont::NodeId,
    ) -> curios_cont::ContinuationId {
        let continuation = self.module.reserve_continuation();
        self.module.define_continuation(
            continuation,
            curios_cont::Continuation {
                debug_name: None,
                params: Vec::new(),
                body,
            },
        );
        continuation
    }

    pub(super) fn jump(
        &mut self,
        target: curios_cont::ContinuationId,
        args: Vec<curios_cont::Atom>,
    ) -> curios_cont::NodeId {
        self.module
            .add_node(curios_cont::Node::ApplyCont(curios_cont::Edge {
                target,
                args,
            }))
    }

    // === Functions and recursion =========================================

    /// Emit a peel: the element at `at`, and — where the arm reads it — the suffix beginning at `after`.
    ///
    /// **The one place the compiler says how a sequence is taken apart.** Both eliminations reach it, `FoldSequence`'s step and `UnconsSequence`'s cons arm, where each used to open-code the pair for itself; the convention the two independently encoded is what a window's operands changing under them found. `at` and `after` name one offset a step apart, and both callers already hold both — the fold as its loop's two indices, the peel as the literals `0` and `1`.
    ///
    /// Neither read names an extent. `sequence_rest_op` takes a start and lets the value decide how much follows, so there is no count for a caller to derive and none for two of them to derive differently.
    pub(super) fn emit_peel(
        &mut self,
        grain: SequenceGrain,
        sequence: &curios_cont::Atom,
        element: curios_cont::ValueId,
        at: curios_cont::Atom,
        suffix: Option<(curios_cont::ValueId, curios_cont::Atom)>,
        next: curios_cont::NodeId,
    ) -> curios_cont::NodeId {
        let next = match suffix {
            Some((suffix, after)) => self.module.add_node(curios_cont::Node::LetIntrinsic {
                result: suffix,
                op: sequence_rest_op(grain),
                args: vec![sequence.clone(), after],
                next,
            }),
            None => next,
        };

        self.module.add_node(curios_cont::Node::LetIntrinsic {
            result: element,
            op: sequence_get_op(grain),
            args: vec![sequence.clone(), at],
            next,
        })
    }

    /// Redirect each marked construction slot through a fresh value the caller settles: the atom is replaced in place, and the returned bindings are what [`Self::wrap_settles`] chains in front of the construction node. A store into a field the census marked indexed-only is where the value's whole future is known — it will only ever be indexed — so it is made (or proven) flat exactly there.
    pub(super) fn settle_stores(
        &mut self,
        marked: &[bool],
        atoms: &mut [curios_cont::Atom],
    ) -> Vec<(curios_cont::ValueId, curios_cont::Atom)> {
        let mut settles = Vec::new();
        for (atom, _) in atoms.iter_mut().zip(marked).filter(|(_, marked)| **marked) {
            let settled = self.module.add_value(None);
            settles.push((settled, atom.clone()));
            *atom = curios_cont::Atom::Value(settled);
        }
        settles
    }

    /// Chain the settle bindings in front of `node`, preserving their field order.
    pub(super) fn wrap_settles(
        &mut self,
        settles: Vec<(curios_cont::ValueId, curios_cont::Atom)>,
        node: curios_cont::NodeId,
    ) -> curios_cont::NodeId {
        settles
            .into_iter()
            .rev()
            .fold(node, |next, (result, atom)| {
                self.module.add_node(curios_cont::Node::LetIntrinsic {
                    result,
                    op: curios_cont::Intrinsic::ListSettle,
                    args: vec![atom],
                    next,
                })
            })
    }

    /// The knot members a block's eager region references directly — its statements' operands, its terminator, and the control sub-blocks reachable without entering a function body or a nested group's initializer, each of which forces its own members at its own entry.
    pub(super) fn block_member_refs(&self, block: BlockId) -> Vec<ValueId> {
        match self.source.block(block) {
            Some(block) => self.eager_member_refs(&block.statements, &block.terminator),
            None => Vec::new(),
        }
    }

    pub(super) fn eager_member_refs(
        &self,
        statements: &[StatementId],
        terminator: &Terminator,
    ) -> Vec<ValueId> {
        let mut refs = BTreeSet::new();
        for value in self.eager_value_refs(statements, terminator) {
            if self.knot_members.contains_key(&value) {
                refs.insert(value);
            }
        }
        refs.into_iter().collect()
    }

    /// Every value referenced across an eager region rooted at `statements` and closed by `terminator`, descending through control sub-blocks but never into a function body or a nested group's initializer — a thunk, like a function, takes its own reads at its entry, and forcing an outer member because a nested initializer names it would force it before the nested member is ever read, which is a cycle by need never meets.
    pub(super) fn eager_value_refs(
        &self,
        statements: &[StatementId],
        terminator: &Terminator,
    ) -> BTreeSet<ValueId> {
        let mut refs = BTreeSet::new();
        let mut pending: Vec<StatementId> = statements.to_vec();
        let mut blocks: Vec<BlockId> = Vec::new();
        let mut seen = BTreeSet::new();
        for atom in terminator.atoms() {
            if let Atom::Value(value) = atom {
                refs.insert(value);
            }
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
                    Some(Statement::Rec { .. } | Statement::Functions { .. }) | None => {}
                }
                continue;
            }
            let Some(block) = blocks.pop() else { break };
            if !seen.insert(block) {
                continue;
            }
            if let Some(block) = self.source.block(block) {
                pending.extend(&block.statements);
                for atom in block.terminator.atoms() {
                    if let Atom::Value(value) = atom {
                        refs.insert(value);
                    }
                }
            }
        }
        refs
    }

    // === Statements ======================================================

    /// Force a member through the ordinary cell and channel operations. Before any force, every initializer has been queued. A filled result is cached; an empty result and a queued initializer is unforced; both empty means reentrant forcing. Taking the initializer releases the channel's reference before it runs, and this function captures only the storage, so completed knots do not retain initializer-only captures.
    pub(super) fn define_force(&mut self, knot: KnotMember, hint: Option<String>) {
        let return_cont = self.module.reserve_continuation();
        let produced = self.module.add_value(None);
        let accepted = self.module.add_value(None);
        let returning = self.jump(return_cont, vec![curios_cont::Atom::Value(produced)]);
        let after_fill = self.module.reserve_continuation();
        self.module.define_continuation(
            after_fill,
            curios_cont::Continuation {
                debug_name: Some("knot/filled".into()),
                params: vec![accepted],
                body: returning,
            },
        );
        let fill = self.module.add_node(curios_cont::Node::Cell {
            op: curios_cont::CellOp::Fill,
            args: vec![
                curios_cont::Atom::Value(knot.result),
                curios_cont::Atom::Value(produced),
            ],
            return_to: after_fill,
        });
        let fill = self.module.add_node(curios_cont::Node::LetCont {
            continuations: vec![after_fill],
            body: fill,
        });
        let receive = self.module.reserve_continuation();
        self.module.define_continuation(
            receive,
            curios_cont::Continuation {
                debug_name: Some("knot/produced".into()),
                params: vec![produced],
                body: fill,
            },
        );
        let thunk = self.module.add_value(None);
        let run = self.module.add_node(curios_cont::Node::ApplyFun {
            callee: curios_cont::Callee::Closure(thunk),
            args: Vec::new(),
            return_to: receive,
        });
        let run = self.module.add_node(curios_cont::Node::LetCont {
            continuations: vec![receive],
            body: run,
        });
        let run = self.continuation_of(run);
        let cycle = self
            .module
            .add_node(curios_cont::Node::Panic(curios_cont::Panic::Cycle));
        let cycle = self.continuation_of(cycle);
        let status = self.module.add_value(None);
        let taken = self.module.add_node(curios_cont::Node::Switch {
            scrutinee: curios_cont::Atom::Value(status),
            cases: BTreeMap::from([(0, edge(run)), (1, edge(cycle))]),
            default: None,
        });
        let taken = self.module.add_node(curios_cont::Node::LetCont {
            continuations: vec![run, cycle],
            body: taken,
        });
        let after_take = self.module.reserve_continuation();
        self.module.define_continuation(
            after_take,
            curios_cont::Continuation {
                debug_name: Some("knot/taken".into()),
                params: vec![status, thunk],
                body: taken,
            },
        );
        let take = self.module.add_node(curios_cont::Node::Channel {
            op: curios_cont::ChannelOp::Take,
            args: vec![curios_cont::Atom::Value(knot.initializer)],
            return_to: after_take,
        });
        let take = self.module.add_node(curios_cont::Node::LetCont {
            continuations: vec![after_take],
            body: take,
        });
        let unforced = self.continuation_of(take);
        let cached = self.module.add_value(None);
        let forced = self.jump(return_cont, vec![curios_cont::Atom::Value(cached)]);
        let forced = self.continuation_of(forced);
        let present = self.module.add_value(None);
        let polled = self.module.add_node(curios_cont::Node::Switch {
            scrutinee: curios_cont::Atom::Value(present),
            cases: BTreeMap::from([(0, edge(unforced)), (1, edge(forced))]),
            default: None,
        });
        let polled = self.module.add_node(curios_cont::Node::LetCont {
            continuations: vec![unforced, forced],
            body: polled,
        });
        let after_poll = self.module.reserve_continuation();
        self.module.define_continuation(
            after_poll,
            curios_cont::Continuation {
                debug_name: Some("knot/polled".into()),
                params: vec![present, cached],
                body: polled,
            },
        );
        let poll = self.module.add_node(curios_cont::Node::Cell {
            op: curios_cont::CellOp::Poll,
            args: vec![curios_cont::Atom::Value(knot.result)],
            return_to: after_poll,
        });
        let body = self.module.add_node(curios_cont::Node::LetCont {
            continuations: vec![after_poll],
            body: poll,
        });
        self.module.define_function(
            knot.force,
            curios_cont::Function {
                debug_name: hint.map(|hint| format!("{hint}/force")),
                params: Vec::new(),
                return_cont,
                body,
                // The private memoization effects must survive even when this particular read's result is unused.
                droppable: false,
            },
        );
    }
}
