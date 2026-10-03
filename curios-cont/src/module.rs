//! Arena-backed high CPS.
//!
//! The surface of this module is intentionally small: Ersd lowering constructs a [`Module`], the optimizer mutates that graph through its checked mutation API, and `curios-emit`'s lowering to WebAssembly consumes it. Stable integer identities, tombstoned arena entries, and deterministic traversal are representation invariants rather than optimizer conventions. Use information is derived on demand (see [`Module::value_use_counts`]) rather than maintained as a shadow arena.

use {
    super::{
        Atom, Callee, Continuation, ContinuationId, Edge, Function, FunctionId, Intrinsic, Literal,
        Node, NodeId, Repr, RowId, UseTarget, ValueDef, ValueId, atoms, nodes_from,
        visit_atoms_mut,
    },
    curios_num::{Floating, Natural},
    curios_utilities::{Arena, ArenaId},
    std::collections::{BTreeMap, BTreeSet},
};

/// The recorded fields representation: `width` consecutive parameters of a continuation, starting at `start`, that *are* the fields of one former aggregate parameter.
///
/// The record is what makes a split a fact of the program rather than a convention between passes: [`Module::verify`] holds every group to its continuation's parameter list the way it already holds arities, so a pass that reshapes a recorded parameter list without maintaining the record fails loudly instead of silently disagreeing with the split.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FieldGroup {
    pub start: usize,
    pub width: usize,
}

/// The production Cont representation. Arena slots never move or get reused; deletion writes `None` and deterministic compaction is explicit.
#[derive(Debug, Clone, Default)]
pub struct Module {
    nodes: Arena<NodeId, Node>,
    values: Arena<ValueId, ValueDef>,
    functions: Arena<FunctionId, Function>,
    continuations: Arena<ContinuationId, Continuation>,
    field_groups: BTreeMap<ContinuationId, Vec<FieldGroup>>,
    /// The nominal rows this module's [`ValueExpr::Row`](super::ValueExpr::Row)s belong to, appended by the Ersd door and never removed — a row that loses its last construction is simply an unreferenced entry, so the ids stay stable without tombstones.
    rows: Vec<Option<Row>>,
    entry: Option<FunctionId>,
}

/// One nominal row — a variant family or a product schema: its debug name, and the carrier of every slot of its heap type. A family carries its tag at slot zero and a product does not; either way this is the width every [`ValueExpr::Row`](super::ValueExpr::Row) naming it is padded to.
#[derive(Debug, Clone)]
pub struct Row {
    pub debug_name: Option<String>,
    pub slots: Vec<Slot>,
}

impl Row {
    /// The arity every construction of this row carries.
    pub fn width(&self) -> usize {
        self.slots.len()
    }
}

/// What one slot of a row's heap type holds.
///
/// The door decides this from the erased shape recorded on each constructor's fields, and it is the whole point of keying a heap type by row: an arity-keyed type is shared by every constructor of that arity module-wide, so the join over any slot's stores is the top type and nothing can be said about it. A row's slots are written by that row alone, so a slot whose every writer agrees names a carrier — a register for the scalars, a declared heap type for the shapes — and the emitter declares the wasm field at it.
///
/// Slots are assigned by carrier rather than by field position, which is what keeps a family from widening: a constructor's fields are distributed into the slot range their carrier owns, so two constructors sharing a carrier share its slots and only a disagreement costs width. Positional assignment would be free and type almost nothing, since a slot shared by disagreeing constructors joins to the top type, while a disjoint range per constructor would widen every row for little more typing.
///
/// A shape stays [`Slot::Opaque`] when no single heap type names its population: a `Nat` or `Int` is an i31 or a boxed magnitude, a packed carrier an immediate inside the envelope and a rope past it, and a value of a family whose one bare constructor rides the i31 is a row struct only on its other paths. A family-typed field names its row only where the door finds that costs the row no width.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Slot {
    /// A variant family's discriminant, at slot zero. Stored packed and read unsigned, since a family's constructor count is bounded far below the byte the tag occupies; a product row carries none.
    Tag,
    /// A raw unsigned machine-word payload — a `Bool`, a `Byte`, or a payload-less constructor riding the zero. Never a `Nat` or `Int`, which is a reference and takes [`Slot::Opaque`].
    Nat,
    /// A raw binary64 payload — the one slot that deletes an allocation rather than a coercion, since the boxed `Flt` it replaces is a heap object of its own.
    Flt,
    /// A list rope. The base type is not final, so this is the slot that deletes an `is_subtype` libcall rather than an inline check.
    List,
    /// A closure of the given arity. Its environment base is *not* final — it is the supertype of every per-closure environment of that arity — so, like [`Slot::List`], this is a slot that deletes an `is_subtype` libcall rather than an inline check.
    Closure(usize),
    /// A value of the named nominal row. A row's heap type is final, so this is the slot whose read needs no cast at all once Binaryen has the static type.
    Row(RowId),
    /// The uniform reference: a polymorphic payload, or one whose shape names no single heap type.
    Opaque,
}

impl Slot {
    /// The representation a read of this slot produces.
    pub fn repr(self) -> Repr {
        match self {
            Slot::Tag | Slot::Nat => Repr::Nat,
            Slot::Flt => Repr::Flt,
            Slot::List => Repr::List,
            Slot::Closure(_) | Slot::Row(_) | Slot::Opaque => Repr::Ref,
        }
    }
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn entry(&self) -> Option<FunctionId> {
        self.entry
    }

    /// The recorded fields representations, by continuation.
    pub fn field_groups(&self) -> &BTreeMap<ContinuationId, Vec<FieldGroup>> {
        &self.field_groups
    }

    /// Register a nominal row and hand back its identity. The Ersd door is the only caller; see [`ValueExpr::Row`](super::ValueExpr::Row).
    pub fn add_row(&mut self, row: Row) -> RowId {
        let id = self.reserve_row();
        self.define_row(id, row);
        id
    }

    /// Claim an identity before the row it names is known.
    ///
    /// A row's slots may name other rows, and a self-referential declaration names its own — so the identity has to exist before the slots are computed, or computing them would not terminate. An undefined row is a compiler bug, and [`Module::row`] says so rather than carrying an `Option` every caller would unwrap.
    pub fn reserve_row(&mut self) -> RowId {
        let id = RowId::from_index(self.rows.len());
        self.rows.push(None);
        id
    }

    pub fn define_row(&mut self, id: RowId, row: Row) {
        self.rows[id.index()] = Some(row);
    }

    pub fn row(&self, id: RowId) -> &Row {
        self.rows[id.index()]
            .as_ref()
            .unwrap_or_else(|| panic!("{id} was reserved and never defined"))
    }

    /// The row at `id`, where one was defined. [`Module::row`] is the reading of everything that trusts the module; this is the verifier's and the printer's, which meet a module that may name a row nothing defined.
    pub(crate) fn defined_row(&self, id: RowId) -> Option<&Row> {
        self.rows.get(id.index()).and_then(Option::as_ref)
    }

    /// What a transfer hands a slot its construction never wrote: what the constructed row's field holds there — zero for a register slot, since a register has no null, and null, as [`Atom::Filler`], for a reference. `None` is a tuple, whose every field is a reference.
    pub fn pad(&self, row: Option<RowId>, index: usize) -> Atom {
        match row.map(|row| self.row(row).slots[index]) {
            Some(Slot::Tag | Slot::Nat) => Atom::Literal(Literal::Nat(Natural::zero())),
            Some(Slot::Flt) => Atom::Literal(Literal::Flt(Floating::zero(false))),
            Some(Slot::List | Slot::Closure(_) | Slot::Row(_) | Slot::Opaque) | None => {
                Atom::Filler
            }
        }
    }

    /// The representation a read of `row`'s slot at `index` produces. The one result representation that is a fact of the module rather than of the operation, which is why [`Intrinsic::result_repr`] cannot answer it alone.
    pub fn slot_repr(&self, row: RowId, index: usize) -> Repr {
        self.row(row).slots[index].repr()
    }

    /// The representation `op` produces, resolving a row read against this module's slot carriers.
    pub fn result_repr(&self, op: &Intrinsic) -> Repr {
        match op {
            Intrinsic::RowGet(row, index) => self.slot_repr(*row, *index),
            _ => op.result_repr(),
        }
    }

    pub fn rows(&self) -> impl Iterator<Item = (RowId, &Row)> {
        (0..self.rows.len())
            .map(RowId::from_index)
            .map(|id| (id, self.row(id)))
    }

    /// Record that `continuation`'s parameter at `start` was spliced into `width` fields: the new group, *and* every group past it shifted by the parameters the splice added.
    ///
    /// Recording and shifting are one operation because they are one fact: a caller recording without shifting would leave stale starts, reachable as soon as two parameters of one continuation are split in the same pass. Groups are kept sorted by start; [`Module::verify`] holds them to the parameter list.
    pub fn record_split(&mut self, continuation: ContinuationId, start: usize, width: usize) {
        let groups = self.field_groups.entry(continuation).or_default();
        for group in groups.iter_mut() {
            if group.start > start {
                group.start += width - 1;
            }
        }
        groups.push(FieldGroup { start, width });
        groups.sort_by_key(|group| group.start);
    }

    /// Maintain the record across a parameter removal: shift groups past each removed index down, shrink groups losing a member, and drop groups emptied entirely. The caller removes the parameters; this keeps the record telling the truth about what remains.
    pub fn remove_params_from_record(
        &mut self,
        continuation: ContinuationId,
        removed: &BTreeSet<usize>,
    ) {
        let Some(groups) = self.field_groups.get_mut(&continuation) else {
            return;
        };
        for group in groups.iter_mut() {
            let inside = removed
                .iter()
                .filter(|&&index| index >= group.start && index < group.start + group.width)
                .count();
            let before = removed.iter().filter(|&&index| index < group.start).count();
            group.start -= before;
            group.width -= inside;
        }
        groups.retain(|group| group.width > 0);
        if groups.is_empty() {
            self.field_groups.remove(&continuation);
        }
    }

    pub fn set_entry(&mut self, entry: FunctionId) {
        self.entry = Some(entry);
    }

    pub fn nodes(&self) -> &[Option<Node>] {
        self.nodes.slots()
    }

    pub fn values(&self) -> &[Option<ValueDef>] {
        self.values.slots()
    }

    pub fn functions(&self) -> &[Option<Function>] {
        self.functions.slots()
    }

    pub fn continuations(&self) -> &[Option<Continuation>] {
        self.continuations.slots()
    }

    pub fn node(&self, id: NodeId) -> Option<&Node> {
        self.nodes.get(id)
    }

    pub fn function(&self, id: FunctionId) -> Option<&Function> {
        self.functions.get(id)
    }

    pub fn continuation(&self, id: ContinuationId) -> Option<&Continuation> {
        self.continuations.get(id)
    }

    pub fn value(&self, id: ValueId) -> Option<&ValueDef> {
        self.values.get(id)
    }

    /// The live nodes, in identity order.
    pub fn live_nodes(&self) -> impl Iterator<Item = (NodeId, &Node)> {
        self.nodes.iter_live()
    }

    /// The live functions, in identity order.
    pub fn live_functions(&self) -> impl Iterator<Item = (FunctionId, &Function)> {
        self.functions.iter_live()
    }

    /// The live continuations, in identity order.
    pub fn live_continuations(&self) -> impl Iterator<Item = (ContinuationId, &Continuation)> {
        self.continuations.iter_live()
    }

    pub fn node_ids(&self) -> impl Iterator<Item = NodeId> {
        self.nodes.live_ids()
    }

    pub fn value_ids(&self) -> impl Iterator<Item = ValueId> {
        self.values.live_ids()
    }

    pub fn function_ids(&self) -> impl Iterator<Item = FunctionId> {
        self.functions.live_ids()
    }

    pub fn continuation_ids(&self) -> impl Iterator<Item = ContinuationId> {
        self.continuations.live_ids()
    }

    /// Count, per value, how many times it is referenced across the module. A value's use sites are its operand occurrences plus its use as an indirect callee; definitions (`LetValue`/`LetIntrinsic` results, parameters) are not uses, so an unreferenced value is absent from the map. Derived on demand rather than maintained incrementally.
    pub(crate) fn value_use_counts(&self) -> BTreeMap<ValueId, usize> {
        let mut counts = BTreeMap::new();
        for (_, node) in self.nodes.iter_live() {
            for atom in atoms(node) {
                if let Atom::Value(value) = atom {
                    *counts.entry(*value).or_insert(0) += 1;
                }
            }
            if let Node::ApplyFun {
                callee: Callee::Closure(value),
                ..
            } = node
            {
                *counts.entry(*value).or_insert(0) += 1;
            }
        }
        counts
    }

    /// How many values each live function hands back to its caller.
    ///
    /// A function's returns are its edges to its own return sentinel, so the arity those edges carry *is* its result count — nothing declares it, and adding a field to say so would mean restating it at every construction site rather than reading it off the one place that already knows. A function with no such edge returns through some tail position instead: a foreign call, a cell operation, or a `ListMap` hands back what that operation produces, a closure call hands back the one value its shared type carries, and a tail call to a known function hands back whatever *that* function does — which is why the last of those is resolved by propagation rather than locally. A function with none of those neither returns nor is called for a result, and takes the one value every function returns unless a protocol widened it.
    ///
    /// Where a function has both a return edge and a constrained tail position, the edge is taken and the disagreement is left to [`Module::verify`], whose business it is to report rather than to paper over.
    pub fn return_arities(&self) -> BTreeMap<FunctionId, usize> {
        let mut settled = BTreeMap::<FunctionId, usize>::new();
        let mut inherits = BTreeMap::<FunctionId, BTreeSet<FunctionId>>::new();

        for (function, definition) in self.functions.iter_live() {
            let sentinel = definition.return_cont;
            let mut edges = None;
            let mut operation = None;
            let mut tail_calls = BTreeSet::new();

            for node_id in nodes_from(self, definition.body) {
                let mut returning = |edge: &Edge| {
                    if edge.target == sentinel {
                        edges.get_or_insert(edge.args.len());
                    }
                };
                match self.node(node_id).unwrap() {
                    Node::ApplyCont(edge) => returning(edge),
                    Node::Switch { cases, default, .. } => {
                        cases.values().chain(default.as_ref()).for_each(returning);
                    }
                    Node::ApplyFun {
                        callee,
                        return_to: to,
                        ..
                    } if *to == sentinel => match callee {
                        Callee::Known(callee) => {
                            tail_calls.insert(*callee);
                        }
                        Callee::Closure(_) => operation = operation.or(Some(1)),
                    },
                    Node::Foreign {
                        function,
                        return_to,
                        ..
                    } if *return_to == sentinel => {
                        operation = operation.or(Some(function.signature().results.len()));
                    }
                    Node::Cell { op, return_to, .. } if *return_to == sentinel => {
                        operation = operation.or(Some(op.result_arity()));
                    }
                    Node::Channel { op, return_to, .. } if *return_to == sentinel => {
                        operation = operation.or(Some(op.result_arity()));
                    }
                    Node::Intrinsic { return_to, .. } if *return_to == sentinel => {
                        operation = operation.or(Some(1));
                    }
                    _ => {}
                }
            }

            match edges.or(operation) {
                Some(arity) => {
                    settled.insert(function, arity);
                }
                None => {
                    inherits.insert(function, tail_calls);
                }
            }
        }

        // Propagate along tail calls until nothing more resolves. Whatever is left over is mutually tail-recursive with nothing that ever returns, so no edge constrains it.
        while inherits
            .values()
            .flatten()
            .any(|to| settled.contains_key(to))
        {
            for (function, tail_calls) in &inherits {
                if let Some(arity) = tail_calls.iter().find_map(|to| settled.get(to)).copied() {
                    settled.insert(*function, arity);
                }
            }
            inherits.retain(|function, _| !settled.contains_key(function));
        }
        for function in inherits.into_keys() {
            settled.insert(function, 1);
        }
        settled
    }

    pub fn reserve_node(&mut self) -> NodeId {
        self.nodes.reserve()
    }

    pub fn add_node(&mut self, node: Node) -> NodeId {
        let id = self.reserve_node();
        self.define_node(id, node);
        id
    }

    pub fn define_node(&mut self, id: NodeId, node: Node) {
        self.nodes.define(id, node);
    }

    pub fn add_value(&mut self, debug_name: Option<String>) -> ValueId {
        self.values.mint(ValueDef { debug_name })
    }

    pub fn reserve_function(&mut self) -> FunctionId {
        self.functions.reserve()
    }

    pub fn define_function(&mut self, id: FunctionId, function: Function) {
        self.functions.define(id, function);
    }

    pub fn add_function(&mut self, function: Function) -> FunctionId {
        self.functions.mint(function)
    }

    pub fn reserve_continuation(&mut self) -> ContinuationId {
        self.continuations.reserve()
    }

    pub fn define_continuation(&mut self, id: ContinuationId, continuation: Continuation) {
        self.continuations.define(id, continuation);
    }

    pub fn add_continuation(&mut self, continuation: Continuation) -> ContinuationId {
        self.continuations.mint(continuation)
    }

    pub fn remove_node(&mut self, id: NodeId) -> Option<Node> {
        self.nodes.remove(id)
    }

    /// Replace a live node in place, where [`Module::define_node`] fills a reserved one.
    pub(crate) fn set_node(&mut self, id: NodeId, node: Node) {
        self.nodes.set(id, node);
    }

    pub(crate) fn node_mut(&mut self, id: NodeId) -> Option<&mut Node> {
        self.nodes.get_mut(id)
    }

    pub(crate) fn function_mut(&mut self, id: FunctionId) -> Option<&mut Function> {
        self.functions.get_mut(id)
    }

    pub(crate) fn continuation_mut(&mut self, id: ContinuationId) -> Option<&mut Continuation> {
        self.continuations.get_mut(id)
    }

    pub(crate) fn live_nodes_mut(&mut self) -> impl Iterator<Item = (NodeId, &mut Node)> {
        self.nodes.iter_live_mut()
    }

    pub(crate) fn live_functions_mut(
        &mut self,
    ) -> impl Iterator<Item = (FunctionId, &mut Function)> {
        self.functions.iter_live_mut()
    }

    pub(crate) fn live_continuations_mut(
        &mut self,
    ) -> impl Iterator<Item = (ContinuationId, &mut Continuation)> {
        self.continuations.iter_live_mut()
    }

    pub(crate) fn remove_value(&mut self, id: ValueId) {
        self.values.remove(id);
    }

    pub(crate) fn remove_function(&mut self, id: FunctionId) {
        self.functions.remove(id);
    }

    /// Tombstone a continuation, and with it the fields record of its parameters, which describes nothing once they are gone.
    pub(crate) fn remove_continuation(&mut self, id: ContinuationId) {
        self.continuations.remove(id);
        self.field_groups.remove(&id);
    }

    pub(crate) fn retain_nodes(&mut self, keep: &BTreeSet<NodeId>) {
        self.nodes.retain(keep);
    }

    pub(crate) fn retain_values(&mut self, keep: &BTreeSet<ValueId>) {
        self.values.retain(keep);
    }

    pub(crate) fn retain_functions(&mut self, keep: &BTreeSet<FunctionId>) {
        self.functions.retain(keep);
    }

    /// Tombstone every continuation outside `keep`, and drop the fields record of each one that goes.
    pub(crate) fn retain_continuations(&mut self, keep: &BTreeSet<ContinuationId>) {
        self.continuations.retain(keep);
        self.field_groups
            .retain(|continuation, _| keep.contains(continuation));
    }

    pub fn replace_atom(&mut self, from: UseTarget, replacement: Atom) {
        for (_, node) in self.nodes.iter_live_mut() {
            visit_atoms_mut(node, &mut |atom| {
                let matches = match (&from, &*atom) {
                    (UseTarget::Value(a), Atom::Value(b)) => a == b,
                    (UseTarget::Fun(a), Atom::Fun(b)) => a == b,
                    _ => false,
                };
                if matches {
                    *atom = replacement.clone();
                }
            });
        }
    }

    pub fn tombstones(&self) -> (usize, usize, usize, usize) {
        let return_continuations = self
            .functions
            .iter_live()
            .map(|(_, function)| function.return_cont)
            .collect::<BTreeSet<_>>();
        (
            self.nodes.tombstone_count(),
            self.values.tombstone_count(),
            self.functions.tombstone_count(),
            self.continuations
                .slots()
                .iter()
                .enumerate()
                .filter(|(index, slot)| {
                    slot.is_none() && !return_continuations.contains(&ContinuationId(*index as u32))
                })
                .count(),
        )
    }
}
