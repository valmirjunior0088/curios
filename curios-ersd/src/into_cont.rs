//! The lowering into the landed Cont interface — the one-way door from meaning to mechanism.
//!
//! Every encoding decision the erasure deliberately deferred is made here, exactly once, per the specification's normative desugar table: `Unit`, `Bool`, and `Byte` ride the `Nat` carrier (`Bool` operations become `Nat` bit operations, `Byte` comparisons `Nat` comparisons, `NatToByte` a mask, `ByteToNat` the identity); a `Handle` token is its little-endian bytes as a byte-grain binary and `HandleEql` that grain's binary equality; products and variants are generic tuples (a variant is `(tag, payload…)`, the tag the constructor's position in its family); a single-constructor family collapses instead — nothing ever needs discriminating, so the tag is never minted and it encodes as the struct with the same relevant row would (one payload the bare value, several an untagged tuple, none the `Nat` zero, matches dispatch-free); a family whose one immediate-unary constructor stands beside boxed siblings rides that constructor bare too, matches discriminating with an `IsImmediate` test instead of a tag read; matches and switches are otherwise one `Nat`-keyed `Switch` behind the tag projection; the fold forms are synthesized accumulator loops; a function-only recursive group is a `LetFun`, and a group with computed members is a knot tied through compiler-internal cells. The per-family choice is [`FamilyEncoding`], a pure function of the registered schema; see `documentation/design/toolchain/a-variant-collapses-when-nothing-needs-to-distinguish-it.md` for the decision.
//!
//! The lowering is target-continuation shaped: each arena block is lowered against the continuation that receives its result — its terminator delivers there, and its statements build the node chain in front. Because the arena is already ANF, every operand is an atom and maps directly to a [`curios_cont::CpsAtom`]; no administrative continuation is introduced merely to evaluate an operand. Only a genuine control split — an application return, a switch or match, a fold loop, a host call — opens a join continuation whose parameter receives the split's result and whose body is the rest of the block.
//!
//! Arena identities are globally unique and never shadowed, so flat maps to their Cont counterparts suffice; source hints are carried onto the Cont values and functions they lower to.

mod census;
pub(crate) use census::{SequenceFacts, sequence_census};

#[cfg(test)]
mod tests;

use {
    super::{
        Analysis, Atom, Block, BlockId, CellOperation, Constant, ConstantId, ConstructorId,
        FamilyId, FieldShape, FoldNatStep, FoldSequenceStep, Function, FunctionId, Intrinsic,
        Module, Operation, ProductId, RecGroup, RecGroupId, Rhs, SequenceGrain, SequenceOp, Sign,
        Statement, StatementId, Terminator, UnconsSequenceStep, ValueId, VariantArm,
    },
    curios_abi::ForeignFunction,
    curios_num::Natural,
    curios_utilities::{Grain, PackedBin, grown, recurse},
    std::{
        collections::{BTreeMap, BTreeSet},
        sync::Arc,
    },
};

/// Lower a verified arena [`Module`] into the landed Cont [`curios_cont::CpsModule`]. The module's top level — its item chain followed by its entry block — becomes the parameterless Cps entry `main`, delivering its result to a bodyless `return_cont`. The produced module is verified; a failure is a lowering bug, not a user error, so it panics.
///
/// The walk recurses once per statement and once per block nesting, inside [`recurse`], and the stage takes its first segment with [`grown`]: a folded parser is a chain of thousands of statements, which overflowed the default test-thread stack at about 1 900 levels.
pub fn lower_to_cont(source: &Module) -> curios_cont::CpsModule {
    curios_profile::profile!("lower_to_cont");
    grown(|| lower_to_cont_within(source))
}

fn lower_to_cont_within(source: &Module) -> curios_cont::CpsModule {
    let mut lowerer = Lowerer {
        source,
        analysis: Analysis::analyze(source),
        facts: sequence_census(source),
        module: curios_cont::CpsModule::new(),
        values: BTreeMap::new(),
        functions: BTreeMap::new(),
        knot_members: BTreeMap::new(),
        knot_row: None,
        families: BTreeMap::new(),
        products: BTreeMap::new(),
        family_ids: BTreeMap::new(),
        product_ids: BTreeMap::new(),
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

/// How a variant family is encoded at runtime, decided per family from its registered schema alone — see [`Lowerer::family_encoding`].
#[derive(Clone, Copy, PartialEq, Eq)]
enum FamilyEncoding {
    /// Every constructor a tagged tuple `(tag, payload…)` — the general encoding.
    Tagged,
    /// A single-constructor family: nothing ever needs discriminating, so it encodes as the struct with the same relevant row would — one payload is the bare value, several are an untagged tuple, none is the `Nat` zero — and a match on it never dispatches.
    Collapsed,
    /// A multi-constructor family whose one immediate-unary constructor rides as its bare payload; every other constructor keeps its tagged tuple. Discrimination is an `IsImmediate` test — the payload is always an immediate and every other constructor a struct, so the two answers are disjoint by construction, and exactly one such constructor is admitted because two would collide on the same immediates.
    Immediate { constructor: ConstructorId },
}

struct Lowerer<'a> {
    source: &'a Module,
    /// Use counts over the finished arena, read only to decline emitting a binding nothing reads. The module does not change during lowering, so one analysis taken at entry stays exact throughout.
    analysis: Analysis,
    /// The sequence-usage census's verdicts, read at every construction site to settle the stores into indexed-only fields.
    facts: SequenceFacts,
    module: curios_cont::CpsModule,
    values: BTreeMap<ValueId, curios_cont::CpsAtom>,
    functions: BTreeMap<FunctionId, curios_cont::CpsFunId>,
    /// Computed members of recursive knots, mapped to the cell that ties each and the function that forces it. A reference to such a member lowers to a call of that function at the referencing region's entry (see [`Lowerer::with_cell_reads`]), so the member is computed on first use, once, and the tie is invisible to everything but this lowering.
    knot_members: BTreeMap<ValueId, KnotMember>,
    /// The row a knot cell holds — its state and, under it, the initializer still to run or the value it produced — minted once per module, the first time a knot is lowered.
    knot_row: Option<curios_cont::CpsRowId>,
    /// The Cont layout of each tagged family, computed on first use. Only the tagged encodings register one: a collapsed family builds a bare value or a structural tuple, and an immediate family's bare constructor is a scalar, so neither has a family heap type to key.
    families: BTreeMap<FamilyId, RowLayout>,
    /// The Cont layout of each product schema, computed on first use.
    products: BTreeMap<ProductId, RowLayout>,
    /// The Cont identity of each row, claimed *before* its layout is computed. A row's slots may name other rows and a self-referential declaration names its own, so identity has to be answerable while the layout that would answer it is still being built.
    family_ids: BTreeMap<FamilyId, curios_cont::CpsRowId>,
    product_ids: BTreeMap<ProductId, curios_cont::CpsRowId>,
}

/// Where one erased row's writers live in its Cont heap type: the arity every construction of it carries, and the slot each writer's relevant fields occupy. The identity is not here — it is claimed before the layout exists, so it lives in the map that hands it out.
///
/// A variant family has one writer per constructor, indexed by the constructor's position — which is its tag. A product schema has exactly one, at index zero.
#[derive(Debug, Clone)]
struct RowLayout {
    width: usize,
    places: Vec<Vec<usize>>,
}

/// One computed member of a knot as the lowering ties it: the cell holding its state and the function that forces it.
#[derive(Clone, Copy)]
struct KnotMember {
    cell: curios_cont::CpsValueId,
    force: curios_cont::CpsFunId,
}

/// A knot cell's states, at the knot row's tag slot: the initializer still to run, the value it produced, and the initializer running — a read of which is a cycle.
const UNFORCED: u32 = 0;
const FORCED: u32 = 1;
const FORCING: u32 = 2;

/// An argumentless edge into a switch arm.
fn edge(target: curios_cont::CpsContId) -> curios_cont::CpsEdge {
    curios_cont::CpsEdge {
        target,
        args: Vec::new(),
    }
}

/// Lay out one nominal row: a tag slot where the row carries one, then a slot range per carrier sized to the widest writer's count of it, and the slot each writer's fields land in.
///
/// Grouping by carrier rather than by field position is what lets every slot name a carrier without the row widening: two writers agreeing on a carrier share its slots, so only a disagreement costs width. A row with a single writer — a product schema, a collapsed family — has no disagreement to pay for, and the grouping degenerates to a permutation of its fields.
fn lay_out(
    tagged: bool,
    writers: &[Vec<curios_cont::CpsSlot>],
) -> (Vec<curios_cont::CpsSlot>, Vec<Vec<usize>>) {
    let mut widths = BTreeMap::<curios_cont::CpsSlot, usize>::new();
    for carriers in writers {
        let mut here = BTreeMap::<curios_cont::CpsSlot, usize>::new();
        for &carrier in carriers {
            *here.entry(carrier).or_default() += 1;
        }
        for (carrier, count) in here {
            let width = widths.entry(carrier).or_default();
            *width = (*width).max(count);
        }
    }

    let mut slots = match tagged {
        true => vec![curios_cont::CpsSlot::Tag],
        false => Vec::new(),
    };
    let mut starts = BTreeMap::<curios_cont::CpsSlot, usize>::new();
    for (&carrier, &count) in &widths {
        starts.insert(carrier, slots.len());
        slots.extend(std::iter::repeat_n(carrier, count));
    }

    let places = writers
        .iter()
        .map(|carriers| {
            let mut taken = BTreeMap::<curios_cont::CpsSlot, usize>::new();
            carriers
                .iter()
                .map(|carrier| {
                    let offset = taken.entry(*carrier).or_default();
                    let place = starts[carrier] + *offset;
                    *offset += 1;
                    place
                })
                .collect()
        })
        .collect();

    (slots, places)
}

/// The slot carrier a recorded field shape names, resolving a nominal shape to the Cont row that holds it.
///
/// Two shapes answer [`curios_cont::CpsSlot::Opaque`] whatever else is true, and for one reason: a carrier that is *sometimes* an immediate has no single heap type to name. A packed value is one; so is a value of an [`FamilyEncoding::Immediate`] family, whose bare constructor rides the i31 while its siblings allocate. Everything unshaped is opaque by definition.
///
/// A *family*-typed field is named here but not necessarily kept: slots are grouped by carrier, so giving a family its own carrier can cost the row width it would otherwise share with the uniform range. [`Lowerer::compute_row_layout`] lays the row out both ways and keeps this one only where it is free.
impl Lowerer<'_> {
    fn slot_of(&mut self, shape: FieldShape, family_typed: bool) -> curios_cont::CpsSlot {
        match shape {
            FieldShape::Immediate(Sign::Unsigned) => curios_cont::CpsSlot::Nat,
            FieldShape::Immediate(Sign::Signed) => curios_cont::CpsSlot::Int,
            FieldShape::Flt => curios_cont::CpsSlot::Flt,
            FieldShape::List => curios_cont::CpsSlot::List,
            FieldShape::Closure(arity) => curios_cont::CpsSlot::Closure(arity),
            FieldShape::Product(schema) => curios_cont::CpsSlot::Row(self.product_identity(schema)),
            // An immediate family's values are *sometimes* the row struct and sometimes the bare payload riding the i31, so no single heap type names its population — the same always-never-sometimes line that keeps a packed carrier out. Every other encoding allocates the row for every constructor.
            FieldShape::Family(family)
                if family_typed
                    && !matches!(
                        self.family_encoding(family),
                        FamilyEncoding::Immediate { .. }
                    ) =>
            {
                curios_cont::CpsSlot::Row(self.row_identity(family))
            }
            FieldShape::Family(_) | FieldShape::Packed(_) | FieldShape::Opaque => {
                curios_cont::CpsSlot::Opaque
            }
        }
    }

    /// The carriers each of `family`'s constructors writes, with its family-typed fields named or left uniform.
    fn row_writers(
        &mut self,
        family: FamilyId,
        bare: Option<ConstructorId>,
        family_typed: bool,
    ) -> Vec<Vec<curios_cont::CpsSlot>> {
        let constructors = self
            .source
            .family(family)
            .expect("live family")
            .constructors
            .clone();
        constructors
            .iter()
            .map(|&constructor| match Some(constructor) == bare {
                true => Vec::new(),
                false => {
                    let shapes: Vec<FieldShape> = self
                        .source
                        .constructor(constructor)
                        .expect("live constructor")
                        .fields
                        .iter()
                        .map(|field| field.shape)
                        .collect();
                    shapes
                        .into_iter()
                        .map(|shape| self.slot_of(shape, family_typed))
                        .collect()
                }
            })
            .collect()
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
    fn lower_statements(
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

    /// Reserve every function of a group before defining any, so a member body can reference itself and its siblings; return the Cont ids in group order.
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

    /// Define a reserved Cont function from its arena function. A body that references a knot's computed member reads it from the knot's cell at its own entry — at call time, once the knot is tied — rather than capturing the value directly.
    fn define_function(&mut self, arena: FunctionId, id: curios_cont::CpsFunId) {
        let function: Function = self.source.function(arena).expect("live function").clone();
        let return_cont = self.module.reserve_continuation();
        let params = function
            .params
            .iter()
            .map(|&param| self.bind_value(param))
            .collect();
        let members = if self.knot_members.is_empty() {
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
        let row = self.knot_row();
        let members: Vec<KnotMember> = group
            .values
            .iter()
            .map(|member| {
                let knot = KnotMember {
                    cell: self.module.add_value(None),
                    force: self.module.reserve_function(),
                };
                self.knot_members.insert(member.value, knot);
                knot
            })
            .collect();
        for (member, knot) in group.values.iter().zip(&members) {
            self.define_force(row, *knot, self.arena_value_name(member.value));
        }

        // The members are in the map before any body below is lowered, so a reference to a computed sibling — from a function member, a thunk, or the rest — is a forcing read at its entry.
        let functions = self.lower_function_group(&group.functions);
        let thunks: Vec<curios_cont::CpsFunId> = group
            .values
            .iter()
            .map(|member| self.define_thunk(member.init, self.arena_value_name(member.value)))
            .collect();

        // Downstream forces the members it references.
        let ready_members = self.eager_member_refs(rest, terminator);
        let mut body = self.with_cell_reads(ready_members, |lowerer| {
            lowerer.lower_statements(rest, terminator, target)
        });

        // Store every thunk unforced, inside out. The cells exist and the thunks are bound, so no read can come between a reservation and its store.
        for (knot, thunk) in members.iter().zip(&thunks).rev() {
            let after_store = self.module.reserve_continuation();
            self.module.define_continuation(
                after_store,
                curios_cont::CpsContinuation {
                    debug_name: None,
                    params: Vec::new(),
                    body,
                },
            );
            let unforced = self.module.add_value(None);
            let store = self.module.add_node(curios_cont::CpsNode::Cell {
                op: curios_cont::CpsCellOp::Set,
                args: vec![
                    curios_cont::CpsAtom::Value(knot.cell),
                    curios_cont::CpsAtom::Value(unforced),
                ],
                return_to: after_store,
            });
            let store = self.module.add_node(curios_cont::CpsNode::LetCont {
                continuations: vec![after_store],
                body: store,
            });
            body = self.module.add_node(curios_cont::CpsNode::LetValue {
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
        body = self.module.add_node(curios_cont::CpsNode::LetFun {
            functions: thunks,
            body,
        });
        if !functions.is_empty() {
            body = self
                .module
                .add_node(curios_cont::CpsNode::LetFun { functions, body });
        }
        body = self.module.add_node(curios_cont::CpsNode::LetFun {
            functions: members.iter().map(|knot| knot.force).collect(),
            body,
        });
        for knot in members.iter().rev() {
            let bound = self.module.reserve_continuation();
            self.module.define_continuation(
                bound,
                curios_cont::CpsContinuation {
                    debug_name: None,
                    params: vec![knot.cell],
                    body,
                },
            );
            let reserve = self.module.add_node(curios_cont::CpsNode::Cell {
                op: curios_cont::CpsCellOp::Reserve,
                args: Vec::new(),
                return_to: bound,
            });
            body = self.module.add_node(curios_cont::CpsNode::LetCont {
                continuations: vec![bound],
                body: reserve,
            });
        }
        body
    }

    /// The row a knot cell holds: a state at slot zero, and under it the unforced initializer, the value it produced, or nothing while it runs.
    fn knot_row(&mut self) -> curios_cont::CpsRowId {
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

    /// A member's initializer as a nullary function: what its cell holds until something forces it. It takes its own forcing reads at entry, so the members it depends on are computed before it runs — which is the by-need order, found by running rather than computed ahead.
    fn define_thunk(&mut self, init: BlockId, hint: Option<String>) -> curios_cont::CpsFunId {
        let thunk = self.module.reserve_function();
        let return_cont = self.module.reserve_continuation();
        let init_members = self.block_member_refs(init);
        let body = self.with_cell_reads(init_members, |lowerer| {
            lowerer.lower_block(init, return_cont)
        });
        self.module.define_function(
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

    /// The function that forces one member: read its cell, and by the state found there return the value, run the initializer, or trap on the cycle.
    fn define_force(&mut self, row: curios_cont::CpsRowId, knot: KnotMember, hint: Option<String>) {
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

        // Forcing: a read inside the initializer, which no order could satisfy.
        let forcing = self.module.add_node(curios_cont::CpsNode::Unreachable);
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

    /// A parameterless continuation over `body`, for a switch arm.
    fn continuation_of(&mut self, body: curios_cont::CpsNodeId) -> curios_cont::CpsContId {
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
                let force = self.knot_members[&member].force;
                let local = self.module.add_value(self.arena_value_name(member));
                let previous = self
                    .values
                    .insert(member, curios_cont::CpsAtom::Value(local));
                (member, local, force, previous)
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
        for &(_, local, force, _) in reads.iter().rev() {
            let resume = self.module.reserve_continuation();
            self.module.define_continuation(
                resume,
                curios_cont::CpsContinuation {
                    debug_name: None,
                    params: vec![local],
                    body,
                },
            );
            let forcing = self.module.add_node(curios_cont::CpsNode::ApplyFun {
                callee: curios_cont::CpsCallee::Known(force),
                args: Vec::new(),
                return_to: resume,
            });
            body = self.module.add_node(curios_cont::CpsNode::LetCont {
                continuations: vec![resume],
                body: forcing,
            });
        }
        body
    }

    /// The knot members a block's eager region references directly — its statements' operands, its terminator, and the control sub-blocks reachable without entering a function body or a nested group's initializer, each of which forces its own members at its own entry.
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
            if self.knot_members.contains_key(&value) {
                refs.insert(value);
            }
        }
        refs.into_iter().collect()
    }

    /// Every value referenced across an eager region rooted at `statements` and closed by `terminator`, descending through control sub-blocks but never into a function body or a nested group's initializer — a thunk, like a function, takes its own reads at its entry, and forcing an outer member because a nested initializer names it would force it before the nested member is ever read, which is a cycle by need never meets.
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
            Rhs::Product { schema, fields } => {
                // A shared row stays a structural tuple: it is the row a multi-result split also builds, and that site names no schema to agree with.
                let places = match self.is_shared(*schema) {
                    true => (0..fields.len()).collect(),
                    false => self.product_slots(*schema),
                };
                let width = match self.is_shared(*schema) {
                    true => fields.len(),
                    false => self.product_layout(*schema).width,
                };
                // One writer, so every slot is filled and no filler is ever placed.
                let mut atoms = vec![curios_cont::CpsAtom::Filler; width];
                let mut marked = vec![false; width];
                for (field, &atom) in fields.iter().enumerate() {
                    atoms[places[field]] = self.lower_atom(atom);
                    marked[places[field]] = self.facts.indexed_only_product(*schema, field);
                }
                let settles = self.settle_stores(&marked, &mut atoms);
                let value = match self.is_shared(*schema) {
                    true => curios_cont::CpsValueExpr::Tuple(atoms),
                    false => curios_cont::CpsValueExpr::Row(self.product_identity(*schema), atoms),
                };
                let bound = self.bind_value(result);
                let next = self.lower_statements(rest, terminator, target);
                let node = self.module.add_node(curios_cont::CpsNode::LetValue {
                    result: bound,
                    value,
                    next,
                });
                self.wrap_settles(settles, node)
            }
            Rhs::Construct {
                constructor,
                fields,
            } => match self.family_encoding(self.constructor_family(*constructor)) {
                // A collapsed construction with at most one payload builds nothing: the result is the payload atom itself (or the interned zero), recorded as an alias in the value map, so downstream code reads the value where the tuple would have been. A marked single field still settles — the value *is* the store — through an ordinary binding instead of the alias.
                FamilyEncoding::Collapsed if fields.len() <= 1 => {
                    if let Some(&payload) = fields.first()
                        && self.facts.indexed_only_constructor(*constructor, 0)
                    {
                        let atom = self.lower_atom(payload);
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
                            Some(&payload) => self.lower_atom(payload),
                            None => curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(0)),
                        };
                        self.values.insert(result, value);
                        self.lower_statements(rest, terminator, target)
                    }
                }
                // Its own nominal row, with no tag: nothing needs discriminating, so it encodes exactly as the struct with the same relevant row does — which is what keeps that equivalence true now that a struct's row is keyed by its schema rather than by its arity.
                FamilyEncoding::Collapsed => {
                    let owner = self.constructor_family(*constructor);
                    let row = self.row_identity(owner);
                    let places = self.constructor_slots(*constructor);
                    let width = self.row_width(owner);
                    let mut atoms = vec![curios_cont::CpsAtom::Filler; width];
                    let mut marked = vec![false; width];
                    for (field, &atom) in fields.iter().enumerate() {
                        atoms[places[field]] = self.lower_atom(atom);
                        marked[places[field]] =
                            self.facts.indexed_only_constructor(*constructor, field);
                    }
                    let settles = self.settle_stores(&marked, &mut atoms);
                    let bound = self.bind_value(result);
                    let next = self.lower_statements(rest, terminator, target);
                    let node = self.module.add_node(curios_cont::CpsNode::LetValue {
                        result: bound,
                        value: curios_cont::CpsValueExpr::Row(row, atoms),
                        next,
                    });
                    self.wrap_settles(settles, node)
                }
                // The immediate-unary constructor rides bare: the payload is always an immediate, so the value *is* the payload and the tag is never minted. An immediate is never a list, so no settle applies.
                FamilyEncoding::Immediate { constructor: bare } if bare == *constructor => {
                    let payload = self.lower_atom(fields[0]);
                    self.values.insert(result, payload);
                    self.lower_statements(rest, terminator, target)
                }
                FamilyEncoding::Tagged | FamilyEncoding::Immediate { .. } => {
                    let tag = self.constructor_tag(*constructor);
                    let owner = self.constructor_family(*constructor);
                    let family = self.row_identity(owner);
                    // Every construction of a family carries every slot, so a narrow constructor is the same heap type as its widest sibling and every read of the family is one exact cast. A slot this constructor does not write takes the filler, which carries no value — the destination's carrier is not known until the backend decides it.
                    let width = self.row_width(owner);
                    let places = self.constructor_slots(*constructor);
                    let mut atoms = vec![curios_cont::CpsAtom::Filler; width];
                    let mut marked = vec![false; width];
                    atoms[0] = curios_cont::CpsAtom::Literal(curios_cont::CpsLiteral::Nat(tag));
                    for (field, &atom) in fields.iter().enumerate() {
                        atoms[places[field]] = self.lower_atom(atom);
                        marked[places[field]] =
                            self.facts.indexed_only_constructor(*constructor, field);
                    }
                    let settles = self.settle_stores(&marked, &mut atoms);
                    let bound = self.bind_value(result);
                    let next = self.lower_statements(rest, terminator, target);
                    let node = self.module.add_node(curios_cont::CpsNode::LetValue {
                        result: bound,
                        value: curios_cont::CpsValueExpr::Row(family, atoms),
                        next,
                    });
                    self.wrap_settles(settles, node)
                }
            },
            Rhs::Project {
                schema,
                product,
                field,
            } => {
                let op = match self.is_shared(*schema) {
                    true => curios_cont::CpsIntrinsic::TupleGet(*field as usize),
                    false => curios_cont::CpsIntrinsic::RowGet(
                        self.product_identity(*schema),
                        self.product_slots(*schema)[*field as usize],
                    ),
                };
                let product = self.lower_atom(*product);
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
                    Intrinsic::ListMap => curios_cont::CpsIntrinsicCall::ListMap,
                };
                // Both representations bind the list first, then the mapper; the operands transcribe in order.
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
                let atom = self.lower_atom(operands[0]);
                self.values.insert(result, atom);
                self.lower_statements(rest, terminator, target)
            }
            Operation::NatToByte => {
                let value = self.lower_atom(operands[0]);
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
                let args = operands.iter().map(|&atom| self.lower_atom(atom)).collect();
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
        let scrutinee = self.lower_atom(scrutinee);

        match self.family_encoding(family) {
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
                    true => self.module.add_node(curios_cont::CpsNode::LetCont {
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

        let identity = self.row_identity(family);
        let (join, fresh) = self.open_join(result, rest, terminator, target);

        let mut continuations = if fresh { vec![join] } else { Vec::new() };
        let mut cases = BTreeMap::new();
        for arm in arms {
            let continuation = self.lower_variant_arm(identity, arm, scrutinee.clone(), join);
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
        let dispatch = self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
            result: tag,
            op: curios_cont::CpsIntrinsic::RowGet(identity, 0),
            args: vec![scrutinee],
            next: switch,
        });
        self.module.add_node(curios_cont::CpsNode::LetCont {
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
                let continuation = self.module.reserve_continuation();
                self.module.define_continuation(
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

        let identity = self.row_identity(family);
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
                        self.constructor_tag(arm.constructor),
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
                let tag = self.module.add_value(None);
                let switch = self.module.add_node(curios_cont::CpsNode::Switch {
                    scrutinee: curios_cont::CpsAtom::Value(tag),
                    cases,
                    default,
                });
                let body = self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
                    result: tag,
                    op: curios_cont::CpsIntrinsic::RowGet(identity, 0),
                    args: vec![scrutinee.clone()],
                    next: switch,
                });
                let continuation = self.module.reserve_continuation();
                self.module.define_continuation(
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

        let kind = self.module.add_value(None);
        let switch = self.module.add_node(curios_cont::CpsNode::Switch {
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
        let dispatch = self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
            result: kind,
            op: curios_cont::CpsIntrinsic::IsImmediate,
            args: vec![scrutinee],
            next: switch,
        });
        self.module.add_node(curios_cont::CpsNode::LetCont {
            continuations,
            body: dispatch,
        })
    }

    /// One collapsed arm body: a lone payload aliases the scrutinee, which *is* the payload under the collapsed encoding; a wider row projects untagged fields. Returns a body rather than a continuation because the caller inlines it with no dispatch to target it.
    ///
    /// The aliasing is sound *here* and only here. A collapsed family has one constructor, so the scrutinee is the payload on every path there is. The immediate encoding looks like the same shape and is not — its scrutinee is a scalar on one path and a tuple on the other — so it binds through [`lower_immediate_arm`] instead. Sharing this function with it miscompiled a loop that did arithmetic on the payload; see [`curios_cont::CpsIntrinsic::ImmediateGet`].
    fn lower_collapsed_arm(
        &mut self,
        arm: &VariantArm,
        scrutinee: curios_cont::CpsAtom,
        join: curios_cont::CpsContId,
    ) -> curios_cont::CpsNodeId {
        if let [binder] = arm.bindings.as_slice() {
            self.values.insert(*binder, scrutinee);
            return self.lower_block(arm.block, join);
        }
        let bindings: Vec<curios_cont::CpsValueId> = arm
            .bindings
            .iter()
            .map(|&binder| self.bind_value(binder))
            .collect();
        let row = self.row_identity(self.constructor_family(arm.constructor));
        let places = self.constructor_slots(arm.constructor);
        let mut body = self.lower_block(arm.block, join);
        for index in (0..bindings.len()).rev() {
            body = self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
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
        let bound = self.bind_value(*binder);
        let body = self.lower_block(arm.block, join);
        self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
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
            .map(|&binder| self.bind_value(binder))
            .collect();
        let places = self.constructor_slots(arm.constructor);
        let mut body = self.lower_block(arm.block, join);
        for index in (0..bindings.len()).rev() {
            body = self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
                result: bindings[index],
                op: curios_cont::CpsIntrinsic::RowGet(family, places[index]),
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
        let increment = self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
            result: next_index,
            op: curios_cont::CpsIntrinsic::NatAdd,
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
        let loop_body = self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
            result: comparison,
            op: curios_cont::CpsIntrinsic::NatEql,
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

    /// Emit a peel: the element at `at`, and — where the arm reads it — the suffix beginning at `after`.
    ///
    /// **The one place the compiler says how a sequence is taken apart.** Both eliminations reach it, `FoldSequence`'s step and `UnconsSequence`'s cons arm, where each used to open-code the pair for itself; the convention the two independently encoded is what a window's operands changing under them found. `at` and `after` name one offset a step apart, and both callers already hold both — the fold as its loop's two indices, the peel as the literals `0` and `1`.
    ///
    /// Neither read names an extent. `sequence_rest_op` takes a start and lets the value decide how much follows, so there is no count for a caller to derive and none for two of them to derive differently.
    fn emit_peel(
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
        let sequence = self.lower_atom(scrutinee);
        let (join, fresh) = self.open_join(result, rest, terminator, target);
        let mut continuations = if fresh { vec![join] } else { Vec::new() };

        let length = self.module.add_value(None);
        let element = self.bind_value(cons.element);
        // Declined where nothing reads it, for the reason the fold's is: a suffix allocates a rope view, and this is the only place that may decline to build one — a later pass cannot drop a read on the grounds its result is dead.
        let suffix =
            (self.analysis.value_uses(cons.suffix) > 0).then(|| self.bind_value(cons.suffix));

        let empty_arm = self.plain_arm(empty, join);
        continuations.push(empty_arm);

        let cons_arm = self.module.reserve_continuation();
        let cons_body = self.lower_block(cons.block, join);
        let cons_body = self.emit_peel(
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
        self.module.define_continuation(
            cons_arm,
            curios_cont::CpsContinuation {
                debug_name: None,
                params: Vec::new(),
                body: cons_body,
            },
        );
        continuations.push(cons_arm);

        let dispatch = self.module.add_node(curios_cont::CpsNode::Switch {
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
        let dispatch = self.module.add_node(curios_cont::CpsNode::LetCont {
            continuations,
            body: dispatch,
        });

        self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
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
        // A step almost never mentions its suffix — `Bytes/fold` and `List/fold` do not, and neither does a step whose only use of it is an argument to a `Prop` constructor, since erasure removes that. Binding it regardless costs one rope-view allocation per element, inside the loop, for a value nothing reads. **This is the only place that can decline to emit it:** a slice may trap, so no later pass may drop one on the grounds that its result is dead, and the fact that this one cannot trap is a property of the loop emitted below rather than anything recoverable downstream.
        let suffix =
            (self.analysis.value_uses(step.suffix) > 0).then(|| self.bind_value(step.suffix));
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
        let step_body = self.emit_peel(
            grain,
            &sequence,
            element,
            curios_cont::CpsAtom::Value(element_index),
            suffix.map(|suffix| (suffix, curios_cont::CpsAtom::Value(step_index))),
            step_body,
        );
        let step_body = self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
            result: element_index,
            op: curios_cont::CpsIntrinsic::NatSub,
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
        let loop_body = self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
            result: comparison,
            op: curios_cont::CpsIntrinsic::NatEql,
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
        self.module.add_node(curios_cont::CpsNode::LetIntrinsic {
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

    /// Bind a straight-line result: allocate its Cont value, lower the rest of the block, and emit the node `make` builds in front of it.
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

    /// Redirect each marked construction slot through a fresh value the caller settles: the atom is replaced in place, and the returned bindings are what [`Self::wrap_settles`] chains in front of the construction node. A store into a field the census marked indexed-only is where the value's whole future is known — it will only ever be indexed — so it is made (or proven) flat exactly there.
    fn settle_stores(
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
    fn wrap_settles(
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

    /// Allocate the Cont value representing an arena value, carrying its source hint, and record the mapping — the single choke point for every binder that names a source value.
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

    /// The encoding of `family`: a pure function of the registered schema, so every construction and match site computes the same answer and no state has to keep them agreeing.
    fn family_encoding(&self, family: FamilyId) -> FamilyEncoding {
        let constructors = &self
            .source
            .family(family)
            .expect("live family")
            .constructors;
        if let [_] = constructors.as_slice() {
            return FamilyEncoding::Collapsed;
        }
        let mut immediate_unary = constructors.iter().filter(|&&constructor| {
            let fields = &self
                .source
                .constructor(constructor)
                .expect("live constructor")
                .fields;
            matches!(fields.as_slice(), [field] if matches!(field.shape, FieldShape::Immediate(_)))
        });
        match (immediate_unary.next(), immediate_unary.next()) {
            (Some(&constructor), None) => FamilyEncoding::Immediate { constructor },
            _ => FamilyEncoding::Tagged,
        }
    }

    /// The family a constructor belongs to.
    /// The Cont layout of `family`, computed on first use and memoized.
    ///
    /// Slot zero is the tag; the payload slots are grouped by carrier, each group as wide as the constructor holding the most fields of that carrier. So two constructors agreeing on a carrier share its slots and only a disagreement costs width, which is what lets every slot name a carrier without the family widening: over the standard library this settles 22 slots against positional assignment's 11, for ten slots more across the whole roster and no growth at all in the families that allocate hot.
    ///
    /// A [`FamilyEncoding::Immediate`] family's bare constructor writes no slot — its value *is* its payload — so it contributes nothing to the widths.
    fn row_layout(&mut self, family: FamilyId) -> &RowLayout {
        self.row_identity(family);
        &self.families[&family]
    }

    fn compute_row_layout(&mut self, family: FamilyId, id: curios_cont::CpsRowId) -> RowLayout {
        let definition = self.source.family(family).expect("live family");
        let debug_name = definition.debug_name.clone();
        let encoding = self.family_encoding(family);
        // A collapsed family discriminates nothing, so it mints no tag and encodes exactly as the struct with the same relevant row does. An immediate family's bare constructor writes no slot at all — its value *is* its payload — so it contributes nothing to the widths.
        let tagged = !matches!(encoding, FamilyEncoding::Collapsed);
        let bare = match encoding {
            FamilyEncoding::Immediate { constructor } => Some(constructor),
            FamilyEncoding::Collapsed | FamilyEncoding::Tagged => None,
        };
        // Both layouts, and the typed one only where it is free.
        //
        // A family-typed slot cannot share the uniform range, so a family whose constructors disagree pays width for it — `/std/Map/Node` goes four slots to six to make its two children concrete, on the corpus's hottest allocated row, and what that buys is the *cheap* kind of cast: an exact compare against a final type, not the `is_subtype` libcall a list or closure slot deletes. Weighed against the `trees` finding that live bytes convert to time under an all-live collector, that trade is declined. The criterion is exact rather than a heuristic — the row widens or it does not — so every free win is still taken; `family_slot_probe` in `curios`'s codegen tests is its figure.
        let typed = self.row_writers(family, bare, true);
        let uniform = self.row_writers(family, bare, false);
        let (typed_slots, typed_places) = lay_out(tagged, &typed);
        let (slots, places) = match lay_out(tagged, &uniform) {
            (uniform_slots, _) if uniform_slots.len() == typed_slots.len() => {
                (typed_slots, typed_places)
            }
            uniform => uniform,
        };
        let width = slots.len();
        self.module
            .define_row(id, curios_cont::CpsRow { debug_name, slots });
        RowLayout { width, places }
    }

    /// The Cont layout of a product schema, computed on first use. One writer, so every slot is written and none is ever a filler.
    fn product_layout(&mut self, schema: ProductId) -> &RowLayout {
        self.product_identity(schema);
        &self.products[&schema]
    }

    /// Whether every row of this width shares `schema` — see [`ProductSchema::shared`](curios_ersd::ProductSchema::shared).
    fn is_shared(&self, schema: ProductId) -> bool {
        self.source.product(schema).expect("live product").shared
    }

    /// The Cont identity of a product schema, laying its row out on first use. See [`Lowerer::row_identity`] for why the reservation comes first.
    fn product_identity(&mut self, schema: ProductId) -> curios_cont::CpsRowId {
        if let Some(&id) = self.product_ids.get(&schema) {
            return id;
        }
        let id = self.module.reserve_row();
        self.product_ids.insert(schema, id);
        {
            let definition = self.source.product(schema).expect("live product");
            let debug_name = definition.debug_name.clone();
            // One writer, so every field takes a slot of its own and no carrier can widen the row: the typed layout is free by construction.
            let shapes: Vec<FieldShape> =
                definition.fields.iter().map(|field| field.shape).collect();
            let writer: Vec<curios_cont::CpsSlot> = shapes
                .into_iter()
                .map(|shape| self.slot_of(shape, true))
                .collect();
            let (slots, places) = lay_out(false, std::slice::from_ref(&writer));
            let width = slots.len();
            self.module
                .define_row(id, curios_cont::CpsRow { debug_name, slots });
            self.products.insert(schema, RowLayout { width, places });
        }
        id
    }

    /// The slot each of a product schema's relevant fields occupies.
    fn product_slots(&mut self, schema: ProductId) -> Vec<usize> {
        self.product_layout(schema).places[0].clone()
    }

    /// The Cont identity of `family`, laying its row out on first use.
    ///
    /// The identity is registered *before* the layout is computed, which is what lets a row whose slots name itself terminate: the recursive call finds the reservation and returns. Reserving and defining are one operation so that no identity can be handed out for a row nothing ever declares.
    fn row_identity(&mut self, family: FamilyId) -> curios_cont::CpsRowId {
        if let Some(&id) = self.family_ids.get(&family) {
            return id;
        }
        let id = self.module.reserve_row();
        self.family_ids.insert(family, id);
        let layout = self.compute_row_layout(family, id);
        self.families.insert(family, layout);
        id
    }

    /// The arity every construction of `family` is padded to.
    fn row_width(&mut self, family: FamilyId) -> usize {
        self.row_layout(family).width
    }

    /// The slot each of `constructor`'s relevant fields occupies in its family's heap type.
    fn constructor_slots(&mut self, constructor: ConstructorId) -> Vec<usize> {
        let family = self.constructor_family(constructor);
        let tag = self.constructor_tag(constructor) as usize;
        self.row_layout(family).places[tag].clone()
    }

    fn constructor_family(&self, constructor: ConstructorId) -> FamilyId {
        self.source
            .constructor(constructor)
            .expect("live constructor")
            .family
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
            curios_cont::CpsAtom::Literal(_) | curios_cont::CpsAtom::Filler => {
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
            // Unit, Bool, and Byte collapse onto the Nat runtime carrier here, at the one-way door — never earlier.
            Constant::Unit => curios_cont::CpsLiteral::Nat(0),
            Constant::Bool(value) => curios_cont::CpsLiteral::Nat(u32::from(*value)),
            Constant::Nat(value) => curios_cont::CpsLiteral::Nat(*value),
            Constant::Byte(value) => curios_cont::CpsLiteral::Nat(u32::from(*value)),
            Constant::Int(value) => curios_cont::CpsLiteral::Int(*value),
            Constant::Flt(value) => curios_cont::CpsLiteral::Flt(*value),
            Constant::Bin(grain, value) => curios_cont::CpsLiteral::Bin(*grain, value.clone()),
            // A Handle descriptor token rides the packed-binary carrier: its little-endian bytes at byte grain.
            Constant::Handle(token) => curios_cont::CpsLiteral::Bin(
                Grain::X,
                PackedBin::from_bytes(Natural::from(*token).to_bytes_le()),
            ),
        }
    }
}

/// The Cont intrinsic of a scalar [`Operation`]. `Bool` operations run on the `0`/`1` `Nat` carrier (`BoolNeq` is xor on a single bit) and `Byte` comparisons on the `Nat` carrier; `HandleEql` is packed-binary equality at byte grain. The `Byte` conversions are handled before this table.
fn operation_intrinsic(operation: Operation) -> curios_cont::CpsIntrinsic {
    use Operation as O;
    match operation {
        O::BoolAnd => curios_cont::CpsIntrinsic::NatAnd,
        O::BoolOr => curios_cont::CpsIntrinsic::NatOr,
        O::BoolXor => curios_cont::CpsIntrinsic::NatXor,
        O::BoolEql => curios_cont::CpsIntrinsic::NatEql,
        O::BoolNeq => curios_cont::CpsIntrinsic::NatXor,
        O::NatEql => curios_cont::CpsIntrinsic::NatEql,
        O::NatNeq => curios_cont::CpsIntrinsic::NatNeq,
        O::NatAdd => curios_cont::CpsIntrinsic::NatAdd,
        O::NatSub => curios_cont::CpsIntrinsic::NatSub,
        O::NatMul => curios_cont::CpsIntrinsic::NatMul,
        O::NatLt => curios_cont::CpsIntrinsic::NatLt,
        O::NatDiv => curios_cont::CpsIntrinsic::NatDiv,
        O::NatRem => curios_cont::CpsIntrinsic::NatRem,
        O::NatLe => curios_cont::CpsIntrinsic::NatLe,
        O::NatAnd => curios_cont::CpsIntrinsic::NatAnd,
        O::NatOr => curios_cont::CpsIntrinsic::NatOr,
        O::NatXor => curios_cont::CpsIntrinsic::NatXor,
        O::NatShl => curios_cont::CpsIntrinsic::NatShl,
        O::NatShr => curios_cont::CpsIntrinsic::NatShr,
        O::ByteEql => curios_cont::CpsIntrinsic::NatEql,
        O::ByteLt => curios_cont::CpsIntrinsic::NatLt,
        O::ByteLe => curios_cont::CpsIntrinsic::NatLe,
        O::IntEql => curios_cont::CpsIntrinsic::IntEql,
        O::IntNeq => curios_cont::CpsIntrinsic::IntNeq,
        O::IntAdd => curios_cont::CpsIntrinsic::IntAdd,
        O::IntSub => curios_cont::CpsIntrinsic::IntSub,
        O::IntMul => curios_cont::CpsIntrinsic::IntMul,
        O::IntDiv => curios_cont::CpsIntrinsic::IntDiv,
        O::IntRem => curios_cont::CpsIntrinsic::IntRem,
        O::IntLt => curios_cont::CpsIntrinsic::IntLt,
        O::IntLe => curios_cont::CpsIntrinsic::IntLe,
        O::IntAnd => curios_cont::CpsIntrinsic::IntAnd,
        O::IntOr => curios_cont::CpsIntrinsic::IntOr,
        O::IntXor => curios_cont::CpsIntrinsic::IntXor,
        O::IntShl => curios_cont::CpsIntrinsic::IntShl,
        O::IntShr => curios_cont::CpsIntrinsic::IntShr,
        O::FltAdd => curios_cont::CpsIntrinsic::FltAdd,
        O::FltSub => curios_cont::CpsIntrinsic::FltSub,
        O::FltMul => curios_cont::CpsIntrinsic::FltMul,
        O::FltDiv => curios_cont::CpsIntrinsic::FltDiv,
        O::FltRem => curios_cont::CpsIntrinsic::FltRem,
        O::FltEql => curios_cont::CpsIntrinsic::FltEql,
        O::FltNeq => curios_cont::CpsIntrinsic::FltNeq,
        O::FltLt => curios_cont::CpsIntrinsic::FltLt,
        O::FltLe => curios_cont::CpsIntrinsic::FltLe,
        O::FltMin => curios_cont::CpsIntrinsic::FltMin,
        O::FltMax => curios_cont::CpsIntrinsic::FltMax,
        O::FltCopysign => curios_cont::CpsIntrinsic::FltCopysign,
        O::FltNeg => curios_cont::CpsIntrinsic::FltNeg,
        O::FltAbs => curios_cont::CpsIntrinsic::FltAbs,
        O::FltSqrt => curios_cont::CpsIntrinsic::FltSqrt,
        O::FltFloor => curios_cont::CpsIntrinsic::FltFloor,
        O::FltCeil => curios_cont::CpsIntrinsic::FltCeil,
        O::FltTrunc => curios_cont::CpsIntrinsic::FltTrunc,
        O::FltNearest => curios_cont::CpsIntrinsic::FltNearest,
        O::NatToInt => curios_cont::CpsIntrinsic::NatToInt,
        O::NatToFlt => curios_cont::CpsIntrinsic::NatToFlt,
        O::IntToNat => curios_cont::CpsIntrinsic::IntToNat,
        O::IntToFlt => curios_cont::CpsIntrinsic::IntToFlt,
        O::FltToNat => curios_cont::CpsIntrinsic::FltToNat,
        O::FltToInt => curios_cont::CpsIntrinsic::FltToInt,
        O::FltToLeBytes => curios_cont::CpsIntrinsic::FltToLeBytes,
        O::FltOfLeBytes => curios_cont::CpsIntrinsic::FltOfLeBytes,
        O::HandleEql => curios_cont::CpsIntrinsic::BinEql(Grain::X),
        O::ByteToNat | O::NatToByte => {
            unreachable!("Byte conversions are lowered before the intrinsic table")
        }
    }
}

/// The Cont intrinsic of a [`SequenceOp`], threading the operand count into the variadic concatenations. `ListBuild` is a list value, never an intrinsic.
fn sequence_intrinsic(operation: SequenceOp, arity: usize) -> curios_cont::CpsIntrinsic {
    use SequenceOp as S;
    match operation {
        S::BinLen(grain) => curios_cont::CpsIntrinsic::BinLen(grain),
        S::BinEql(grain) => curios_cont::CpsIntrinsic::BinEql(grain),
        S::BinGet(grain) => curios_cont::CpsIntrinsic::BinGet(grain),
        S::BinSlice(grain) => curios_cont::CpsIntrinsic::BinSlice(grain),
        S::BinAppend(grain) => curios_cont::CpsIntrinsic::BinAppend(grain),
        S::BinConcat(grain) => curios_cont::CpsIntrinsic::BinConcat(grain, arity),
        S::ListLen => curios_cont::CpsIntrinsic::ListLen,
        S::ListGet => curios_cont::CpsIntrinsic::ListGet,
        S::ListSlice => curios_cont::CpsIntrinsic::ListSlice,
        S::ListAppend => curios_cont::CpsIntrinsic::ListAppend,
        S::ListConcat => curios_cont::CpsIntrinsic::ListConcat(arity),
        S::ListBuild => unreachable!("ListBuild is lowered as a list value"),
    }
}

fn cell_op(operation: CellOperation) -> curios_cont::CpsCellOp {
    match operation {
        CellOperation::New => curios_cont::CpsCellOp::New,
        CellOperation::Get => curios_cont::CpsCellOp::Get,
        CellOperation::Set => curios_cont::CpsCellOp::Set,
    }
}

fn sequence_len_op(grain: SequenceGrain) -> curios_cont::CpsIntrinsic {
    match grain {
        SequenceGrain::List => curios_cont::CpsIntrinsic::ListLen,
        SequenceGrain::Bin(grain) => curios_cont::CpsIntrinsic::BinLen(grain),
    }
}

fn sequence_get_op(grain: SequenceGrain) -> curios_cont::CpsIntrinsic {
    match grain {
        SequenceGrain::List => curios_cont::CpsIntrinsic::ListGet,
        SequenceGrain::Bin(grain) => curios_cont::CpsIntrinsic::BinGet(grain),
    }
}

fn sequence_rest_op(grain: SequenceGrain) -> curios_cont::CpsIntrinsic {
    match grain {
        SequenceGrain::List => curios_cont::CpsIntrinsic::ListRest,
        SequenceGrain::Bin(grain) => curios_cont::CpsIntrinsic::BinRest(grain),
    }
}
