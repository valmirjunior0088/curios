//! The Cont module under construction, and what each erased name has become in it.
//!
//! The lowering above this is a walk that decides *what* to emit; this is everything the walk emits *through*. It owns the Cont module being built and the three correspondences that make a write possible — an arena value's Cont atom, a knot member's cell and forcing function, and the row a knot cell holds — so that deciding and emitting are not the same object's business.
//!
//! Nothing here descends into a block or chooses a branch. Every method either mints one Cont node, translates one erased name, answers one question about the knot, or emits a fixed instruction sequence whose shape does not depend on what it is wrapping. [`Emitter::define_force`] is the largest and still obeys that rule: a knot member's forcing function is the same six blocks whatever the member computes.

use {
    super::{
        Atom, BlockId, Constant, ConstantId, FunctionId, Module, SequenceGrain, Statement,
        StatementId, Terminator, ValueId, edge, sequence_get_op, sequence_rest_op,
    },
    curios_abi::Handle,
    curios_num::Natural,
    curios_utilities::{Grain, PackedBin},
    std::collections::{BTreeMap, BTreeSet},
};

/// One computed member of a knot as the lowering ties it: the cell holding its state and the function that forces it.
#[derive(Clone, Copy)]
pub(super) struct KnotMember {
    pub(super) cell: curios_cont::CpsValueId,
    pub(super) force: curios_cont::CpsFunId,
}

/// A knot cell's states, at the knot row's tag slot: the initializer still to run, the value it produced, and the initializer running — a read of which is a cycle.
pub(super) const UNFORCED: u32 = 0;
pub(super) const FORCED: u32 = 1;
pub(super) const FORCING: u32 = 2;

/// The Cont module being built, with the erased-to-Cont correspondence that every write consults.
pub(super) struct Emitter<'a> {
    source: &'a Module,
    pub(super) module: curios_cont::CpsModule,
    pub(super) values: BTreeMap<ValueId, curios_cont::CpsAtom>,
    pub(super) functions: BTreeMap<FunctionId, curios_cont::CpsFunId>,
    /// Computed members of recursive knots, mapped to the cell that ties each and the function that forces it. A reference to such a member lowers to a call of that function at the referencing region's entry, so the member is computed on first use, once, and the tie is invisible to everything but this lowering.
    pub(super) knot_members: BTreeMap<ValueId, KnotMember>,
    /// The row a knot cell holds — its state and, under it, the initializer still to run or the value it produced — minted once per module, the first time a knot is lowered.
    knot_row: Option<curios_cont::CpsRowId>,
}

impl<'a> Emitter<'a> {
    pub(super) fn new(source: &'a Module) -> Self {
        Self {
            source,
            module: curios_cont::CpsModule::new(),
            values: BTreeMap::new(),
            functions: BTreeMap::new(),
            knot_members: BTreeMap::new(),
            knot_row: None,
        }
    }

    /// The row a knot cell holds: a state at slot zero, and under it the unforced initializer, the value it produced, or nothing while it runs.
    pub(super) fn knot_row(&mut self) -> curios_cont::CpsRowId {
        if let Some(row) = self.knot_row {
            return row;
        }
        let row = self.module.add_row(curios_cont::CpsRow {
            debug_name: Some("knot".into()),
            slots: vec![curios_cont::CpsSlot::Tag, curios_cont::CpsSlot::Opaque],
        });
        self.knot_row = Some(row);
        row
    }

    /// Allocate the Cont value representing an arena value, carrying its source hint, and record the mapping — the single choke point for every binder that names a source value.
    pub(super) fn bind_value(&mut self, arena: ValueId) -> curios_cont::CpsValueId {
        let name = self.arena_value_name(arena);
        let cont = self.module.add_value(name);
        self.values.insert(arena, curios_cont::CpsAtom::Value(cont));
        cont
    }

    pub(super) fn arena_value_name(&self, id: ValueId) -> Option<String> {
        self.source
            .value(id)
            .and_then(|value| value.debug_name.clone())
    }

    pub(super) fn lower_callee(&self, atom: Atom) -> curios_cont::CpsCallee {
        match self.lower_atom(atom) {
            curios_cont::CpsAtom::Fun(function) => curios_cont::CpsCallee::Known(function),
            curios_cont::CpsAtom::Value(value) => curios_cont::CpsCallee::Closure(value),
            curios_cont::CpsAtom::Literal(_) | curios_cont::CpsAtom::Filler => {
                panic!("arena application head lowered to a literal")
            }
        }
    }

    pub(super) fn lower_atom(&self, atom: Atom) -> curios_cont::CpsAtom {
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

    pub(super) fn lower_constant(&self, constant: ConstantId) -> curios_cont::CpsLiteral {
        match self.source.constant(constant).expect("live constant") {
            // Unit, Bool, and Byte collapse onto the Nat runtime carrier here, at the one-way door — never earlier.
            Constant::Unit => curios_cont::CpsLiteral::Nat(0),
            Constant::Bool(value) => curios_cont::CpsLiteral::Nat(u32::from(*value)),
            Constant::Nat(value) => curios_cont::CpsLiteral::Nat(*value),
            Constant::Byte(value) => curios_cont::CpsLiteral::Nat(u32::from(*value)),
            Constant::Int(value) => curios_cont::CpsLiteral::Int(*value),
            Constant::Flt(value) => curios_cont::CpsLiteral::Flt(*value),
            Constant::Bin(grain, value) => curios_cont::CpsLiteral::Bin(*grain, value.clone()),
            // A Handle descriptor token rides the packed-binary carrier at byte grain, spelled by the one encoding the host reads back.
            Constant::Handle(token) => curios_cont::CpsLiteral::Bin(
                Grain::X,
                PackedBin::from_bytes(Handle::encode(&Natural::from(*token))),
            ),
        }
    }

    /// A parameterless continuation over `body`, for a switch arm.
    pub(super) fn continuation_of(
        &mut self,
        body: curios_cont::CpsNodeId,
    ) -> curios_cont::CpsContId {
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

    pub(super) fn jump(
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

    /// Emit a peel: the element at `at`, and — where the arm reads it — the suffix beginning at `after`.
    ///
    /// **The one place the compiler says how a sequence is taken apart.** Both eliminations reach it, `FoldSequence`'s step and `UnconsSequence`'s cons arm, where each used to open-code the pair for itself; the convention the two independently encoded is what a window's operands changing under them found. `at` and `after` name one offset a step apart, and both callers already hold both — the fold as its loop's two indices, the peel as the literals `0` and `1`.
    ///
    /// Neither read names an extent. `sequence_rest_op` takes a start and lets the value decide how much follows, so there is no count for a caller to derive and none for two of them to derive differently.
    pub(super) fn emit_peel(
        &mut self,
        grain: SequenceGrain,
        sequence: &curios_cont::CpsAtom,
        element: curios_cont::CpsValueId,
        at: curios_cont::CpsAtom,
        suffix: Option<(curios_cont::CpsValueId, curios_cont::CpsAtom)>,
        next: curios_cont::CpsNodeId,
    ) -> curios_cont::CpsNodeId {
        let next = match suffix {
            Some((suffix, after)) => self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
                result: suffix,
                op: sequence_rest_op(grain),
                args: vec![sequence.clone(), after],
                next,
            }),
            None => next,
        };

        self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
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
        atoms: &mut [curios_cont::CpsAtom],
    ) -> Vec<(curios_cont::CpsValueId, curios_cont::CpsAtom)> {
        let mut settles = Vec::new();
        for (atom, _) in atoms.iter_mut().zip(marked).filter(|(_, marked)| **marked) {
            let settled = self.module.add_value(None);
            settles.push((settled, atom.clone()));
            *atom = curios_cont::CpsAtom::Value(settled);
        }
        settles
    }

    /// Chain the settle bindings in front of `node`, preserving their field order.
    pub(super) fn wrap_settles(
        &mut self,
        settles: Vec<(curios_cont::CpsValueId, curios_cont::CpsAtom)>,
        node: curios_cont::CpsNodeId,
    ) -> curios_cont::CpsNodeId {
        settles
            .into_iter()
            .rev()
            .fold(node, |next, (result, atom)| {
                self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
                    result,
                    op: curios_cont::CpsIntrinsic::ListSettle,
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
                if let Some(Atom::Value(value)) = block.terminator.atom() {
                    refs.insert(value);
                }
            }
        }
        refs
    }

    // === Statements ======================================================

    /// The function that forces one member: read its cell, and by the state found there return the value, run the initializer, or trap on the cycle.
    pub(super) fn define_force(
        &mut self,
        row: curios_cont::CpsRowId,
        knot: KnotMember,
        hint: Option<String>,
    ) {
        let return_cont = self.module.reserve_continuation();
        let held = self.module.add_value(None);
        let state = self.module.add_value(None);

        // Forced: the value is under the state.
        let value = self.module.add_value(None);
        let forced = self.jump(return_cont, vec![curios_cont::CpsAtom::Value(value)]);
        let forced = self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
            result: value,
            op: curios_cont::CpsIntrinsic::RowGet(row, 1),
            args: vec![curios_cont::CpsAtom::Value(held)],
            next: forced,
        });
        let forced = self.continuation_of(forced);

        // Forcing: a read inside the initializer, which no order could satisfy — reachable whenever the eager verifier could not see the cycle through a closure, so it is the program's failure and says so.
        let forcing = self
            .module
            .add_node(curios_cont::CpsNode::Panic(curios_cont::Panic::Cycle));
        let forcing = self.continuation_of(forcing);

        // Unforced: mark the cell, run the initializer, store what it produced, and return it.
        let produced = self.module.add_value(None);
        let after_store = self.module.reserve_continuation();
        let returning = self.jump(return_cont, vec![curios_cont::CpsAtom::Value(produced)]);
        self.module.define_continuation(
            after_store,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: Vec::new(),
                body: returning,
            },
        );
        let stored = self.module.add_value(None);
        let store = self.module.add_node(curios_cont::CpsNode::Cell {
            op: curios_cont::CpsCellOp::Set,
            args: vec![
                curios_cont::CpsAtom::Value(knot.cell),
                curios_cont::CpsAtom::Value(stored),
            ],
            return_to: after_store,
        });
        let store = self.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![after_store],
            body: store,
        });
        let store = self.module.add_node(curios_cont::CpsNode::LetValue {
            result: stored,
            value: curios_cont::CpsValueExpr::Row(
                row,
                vec![
                    curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(FORCED)),
                    curios_cont::CpsAtom::Value(produced),
                ],
            ),
            next: store,
        });
        let receive = self.module.reserve_continuation();
        self.module.define_continuation(
            receive,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![produced],
                body: store,
            },
        );
        let thunk = self.module.add_value(None);
        let run = self.module.add_node(curios_cont::CpsNode::ApplyFun {
            callee: curios_cont::CpsCallee::Closure(thunk),
            args: Vec::new(),
            return_to: receive,
        });
        let run = self.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![receive],
            body: run,
        });
        let after_mark = self.module.reserve_continuation();
        self.module.define_continuation(
            after_mark,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: Vec::new(),
                body: run,
            },
        );
        let mark = self.module.add_value(None);
        let marking = self.module.add_node(curios_cont::CpsNode::Cell {
            op: curios_cont::CpsCellOp::Set,
            args: vec![
                curios_cont::CpsAtom::Value(knot.cell),
                curios_cont::CpsAtom::Value(mark),
            ],
            return_to: after_mark,
        });
        let marking = self.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![after_mark],
            body: marking,
        });
        let marking = self.module.add_node(curios_cont::CpsNode::LetValue {
            result: mark,
            value: curios_cont::CpsValueExpr::Row(
                row,
                vec![
                    curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(FORCING)),
                    curios_cont::CpsAtom::Filler,
                ],
            ),
            next: marking,
        });
        let unforced = self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
            result: thunk,
            op: curios_cont::CpsIntrinsic::RowGet(row, 1),
            args: vec![curios_cont::CpsAtom::Value(held)],
            next: marking,
        });
        let unforced = self.continuation_of(unforced);

        let switch = self.module.add_node(curios_cont::CpsNode::Switch {
            scrutinee: curios_cont::CpsAtom::Value(state),
            cases: BTreeMap::from([
                (UNFORCED, edge(unforced)),
                (FORCED, edge(forced)),
                (FORCING, edge(forcing)),
            ]),
            default: None,
        });
        let switch = self.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![unforced, forced, forcing],
            body: switch,
        });
        let read = self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
            result: state,
            op: curios_cont::CpsIntrinsic::RowGet(row, 0),
            args: vec![curios_cont::CpsAtom::Value(held)],
            next: switch,
        });
        let got = self.module.reserve_continuation();
        self.module.define_continuation(
            got,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: vec![held],
                body: read,
            },
        );
        let get = self.module.add_node(curios_cont::CpsNode::Cell {
            op: curios_cont::CpsCellOp::Get,
            args: vec![curios_cont::CpsAtom::Value(knot.cell)],
            return_to: got,
        });
        let body = self.module.add_node(curios_cont::CpsNode::LetCont {
            continuations: vec![got],
            body: get,
        });
        self.module.define_function(
            knot.force,
            curios_cont::CpsFunction {
                debug_name: hint.map(|hint| format!("{hint}/force")),
                params: Vec::new(),
                return_cont,
                body,
            },
        );
    }
}
