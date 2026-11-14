//! Which constructions of a row may write into a value of that row dying beside them: the proof [`rebuild_in_place`](super::rebuild_in_place) spends ([A value costs when it is kept, not when it is named](../../documentation/design/compilation/a-value-costs-when-it-is-kept-not-when-it-is-named.md)).
//!
//! **Three facts, each on a path.** Whether a value is consumed or only read, and where it is used last: a function's body and each continuation's are blocks, a node has a place along one, and a switch's edge is a path of its own, so a value one arm hands on and another returns is used once. Whether a value is unique: the greatest fixpoint the shared solver reaches as the least one of its dual, *shared*, over values and function returns, with every boundary the module does not see whole shared — an escaping function's or the entry's parameters, what a closure, a host, a cell or a channel hands back, a value a nested function captures. A filler is null and a literal a number or bytes, neither a row a rebuild writes into, and constant data is kept from one by its tag, below, so none of the three makes what it is handed to shared: a parameter one call hands a constant and another a value nothing else holds is unique wherever it is no constant. A parameter is unique where every known call hands it a unique value dead after the call; a field read of a unique value is unique where no path from the read consumes that value before a run binds it again, which is Perceus's rule and what lets a path copy qualify, and where no run comes back to the read with that value still bound, each round's read naming the one object. Past the block that binds a value its name is another round's value, so what a loop does to its parameter on a later round is done to another value than the one a read took a field of.
//!
//! **Payloads by slot.** A construction holding what something else holds is shared with it, since a read of the construction hands that out; only a row or a tuple counts, a number or a list holding nothing a rebuild writes. A slot whose reads nothing takes apart, wherever what they hand out flows, is a payload's and carries the fact itself: a shared value stored in it shares the slot and leaves the construction one nothing else holds, and a read of a slot so shared is shared. So a leaf built over a payload read back out of another map is a leaf nothing else holds, and every fork over it is rebuilt; a child's slot stays shared with the construction that holds it, so one tree something else holds says nothing of another. Which slots are payloads decides what the facts prove and never whether they hold, a slot's fact covering every store into it.
//!
//! **Constants by tag.** A constant is one global every use shares, and it reaches every parameter it is handed to and every read of a slot it is stored in, so the constructor tags of the constant data that may reach each value flow through slots, calls and returns; a value some constant may be is rebuilt only within the arms of a test of its tag that rule every such tag out. A row's constructors share its slots, a leaf's payload a fork's child, so a slot is followed under the constructor that writes it and read under the one a test around the read decides.
//!
//! **Borrowing.** A parameter a function only reads is borrowed, so its caller keeps the value it handed, unless the function rebuilds it, captures it, or must own a field of it, as Lean's inference owns what it resets. Which parameters are rebuilt is the candidates' own answer, so the facts are found twice: owning every parameter, then borrowing what the first answer leaves.

use {
    super::{
        Atom, Callee, ContinuationId, FunctionId, Intrinsic, Lattice, Literal, Module, Node,
        NodeId, RowId, Rules, Slot, Solver, ValueExpr, ValueId, analyze_calls, return_edges,
    },
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet},
    },
};

/// A rebuild the facts admit: the construction at `node`, of `row`, may write into `of`, writing only `slots`.
#[derive(Debug, Clone, PartialEq)]
pub struct Rebuild {
    pub node: NodeId,
    pub of: ValueId,
    pub row: RowId,
    pub slots: Vec<(usize, Atom)>,
}

/// Every construction of a row the module may rebuild in place, in node order.
pub fn rebuilds(module: &Module) -> Vec<Rebuild> {
    let shape = Shape::of(module);
    let tags = constant_tags(module, &shape);
    let owned = BTreeSet::new();
    let first = Facts::solve(module, &shape, &owned).rebuilds(&tags);
    let reused = first
        .iter()
        .map(|rebuild| rebuild.of)
        .collect::<BTreeSet<_>>();
    let borrowed = shape.borrowed(module, &reused);
    match borrowed.is_empty() {
        true => first,
        false => Facts::solve(module, &shape, &borrowed).rebuilds(&tags),
    }
}

/// The version of a function a judgment is of: its base version, or its reuse version, which assumes unique the parameters it takes apart at a row.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum World {
    Base,
    Reuse,
}

/// Which calls reach a reuse version, each judged in the version of the function it stands in, and the functions some reached call reaches the reuse version of while another reaches the base one: each copied, its copy the reuse version.
pub struct Routes {
    pub copied: Vec<FunctionId>,
    pub reuse: BTreeSet<(NodeId, World)>,
}

/// Both versions of every function solved together, as the greatest fixpoint of uniqueness over both: a call reaches its callee's reuse version where it hands each parameter that version assumes a unique value dead after it, and a call's result takes the facts of the version it reaches, so a loop threading what a reuse version hands back holds it unique where that is consistent. A reuse version counts only where a call from a version that counts reaches it, so a cycle no unique value enters never does.
pub fn routes(module: &Module) -> Routes {
    let shape = Shape::of(module);
    let borrowed = BTreeSet::new();
    let mut assumed = BTreeMap::<FunctionId, BTreeSet<usize>>::new();
    for (function, definition) in module.live_functions() {
        if shape.open.contains(&function) {
            continue;
        }
        for (index, param) in definition.params.iter().enumerate() {
            if matches!(shape.read_at.get(param), Some(Some(_))) {
                assumed.entry(function).or_default().insert(index);
            }
        }
    }
    let arities = module.return_arities();
    let aggregates = aggregates(module, &shape);
    let base = sources(module, &shape, &borrowed, &BTreeMap::new(), &aggregates);
    let reuse = sources(module, &shape, &borrowed, &assumed, &aggregates);
    let (shape, assumed, arities, base, reuse, aggregates) =
        (&shape, &assumed, &arities, &base, &reuse, &aggregates);
    let world_of = move |value: ValueId, world: World| match shape.home.get(&value) {
        Some(function) if assumed.contains_key(function) => world,
        _ => World::Base,
    };
    let keys = shape
        .defs
        .keys()
        .flat_map(|value| {
            [
                (World::Base, Key::Value(*value)),
                (World::Reuse, Key::Value(*value)),
            ]
        })
        .chain(arities.iter().flat_map(|(function, arity)| {
            (0..*arity).flat_map(move |index| {
                [World::Base, World::Reuse].map(|world| (world, Key::Returned(*function, index)))
            })
        }))
        .collect::<Vec<_>>();
    let facts = Solver::solve(keys, |rules| {
        rules.add(move |solver| {
            for key in base {
                solver.join((World::Base, *key), Shared(true));
            }
            for key in reuse {
                solver.join((World::Reuse, *key), Shared(true));
            }
        });
        let shared =
            move |solver: &Solver<Shared, (World, Key)>, world: World, atom: &Atom| match atom {
                Atom::Value(value) if !aggregates.contains(value) => false,
                Atom::Value(value) => solver
                    .fact(&(world_of(*value, world), Key::Value(*value)))
                    .is_none_or(|fact| fact.0),
                Atom::Filler | Atom::Literal(_) => false,
                Atom::Fun(_) => true,
            };
        let route = move |solver: &Solver<Shared, (World, Key)>, call: NodeId, world: World| {
            route(module, shape, assumed, call, &|atom| {
                shared(solver, world, atom)
            })
        };
        for world in [World::Base, World::Reuse] {
            for (value, def) in &shape.defs {
                let value = *value;
                if world_of(value, world) != world {
                    continue;
                }
                match *def {
                    Def::Field(of) => {
                        let read = shape.binder[&value];
                        let consumed_after = shape
                            .consumed(of, &borrowed)
                            .iter()
                            .any(|point| shape.reaches_holding(of, read, point.node));
                        let slot = slot_read(module, shape, of, read);
                        rules.add(move |solver| {
                            // A slot's fact is one for both versions: an object either builds may reach a read in either.
                            let marked = slot.is_some_and(|slot| {
                                solver
                                    .fact(&(World::Base, Key::Slot(slot)))
                                    .is_some_and(|fact| fact.0)
                            });
                            if consumed_after || marked || shared(solver, world, &Atom::Value(of)) {
                                solver.join((world, Key::Value(value)), Shared(true));
                            }
                        });
                    }
                    Def::Param(function, index) => {
                        if world == World::Reuse
                            && assumed
                                .get(&function)
                                .is_some_and(|params| params.contains(&index))
                        {
                            continue;
                        }
                        // A caller is judged in its base version, the one every call can run as.
                        let calls = shape.calls.get(&function).cloned().unwrap_or_default();
                        rules.add(move |solver| {
                            for call in &calls {
                                if let Some(Node::ApplyFun { args, .. }) = module.node(*call)
                                    && args
                                        .get(index)
                                        .is_none_or(|atom| shared(solver, World::Base, atom))
                                {
                                    solver.join((world, Key::Value(value)), Shared(true));
                                }
                            }
                        });
                    }
                    Def::ContParam(continuation, index) => {
                        let entries = shape
                            .entries
                            .get(&continuation)
                            .cloned()
                            .unwrap_or_default();
                        let resumes = shape
                            .resumes
                            .get(&continuation)
                            .cloned()
                            .unwrap_or_default();
                        rules.add(move |solver| {
                            let entered = entries.iter().any(|entry| {
                                entry
                                    .args
                                    .get(index)
                                    .is_none_or(|atom| shared(solver, world, atom))
                            });
                            let resumed = resumes.iter().any(|node| match module.node(*node) {
                                Some(Node::ApplyFun {
                                    callee: Callee::Known(callee),
                                    ..
                                }) => solver
                                    .fact(&(
                                        route(solver, *node, world),
                                        Key::Returned(*callee, index),
                                    ))
                                    .is_none_or(|fact| fact.0),
                                _ => true,
                            });
                            if entered || resumed {
                                solver.join((world, Key::Value(value)), Shared(true));
                            }
                        });
                    }
                    // A construction holding what something else holds is shared, since a read of it hands that out, but where it holds it in a payload slot, which is shared in its place. A constant is never written into, so it spoils nothing.
                    Def::Construction => {
                        let stored = stored_values(module, shape, value, aggregates);
                        rules.add(move |solver| {
                            for (atom, slot) in &stored {
                                if shared(solver, world, atom) {
                                    for key in shared_by(value, *slot) {
                                        let world = match key {
                                            Key::Slot(_) => World::Base,
                                            _ => world,
                                        };
                                        solver.join((world, key), Shared(true));
                                    }
                                }
                            }
                        });
                    }
                    Def::Other => {}
                }
            }
            for (function, nodes) in &shape.returns {
                let function = *function;
                if world == World::Reuse && !assumed.contains_key(&function) {
                    continue;
                }
                for node in nodes {
                    let node = *node;
                    rules.add(move |solver| match module.node(node) {
                        Some(Node::ApplyCont(_) | Node::Switch { .. }) => {
                            for (index, atom) in handed_back(module, function, node) {
                                if shared(solver, world, atom) {
                                    solver.join(
                                        (world, Key::Returned(function, index)),
                                        Shared(true),
                                    );
                                }
                            }
                        }
                        Some(Node::ApplyFun {
                            callee: Callee::Known(callee),
                            ..
                        }) => {
                            let reached = route(solver, node, world);
                            for index in 0..arities.get(&function).copied().unwrap_or(0) {
                                if solver
                                    .fact(&(reached, Key::Returned(*callee, index)))
                                    .is_none_or(|fact| fact.0)
                                {
                                    solver.join(
                                        (world, Key::Returned(function, index)),
                                        Shared(true),
                                    );
                                }
                            }
                        }
                        _ => {}
                    });
                }
            }
        }
    });
    let shared = |world: World, atom: &Atom| match atom {
        Atom::Value(value) => facts
            .get(&(world_of(*value, world), Key::Value(*value)))
            .is_none_or(|fact| fact.0),
        Atom::Filler | Atom::Literal(_) => false,
        Atom::Fun(_) => true,
    };
    // The versions some call reaches: every function's base version, and a reuse version a call from a reached version reaches.
    let mut calls = BTreeMap::<FunctionId, Vec<(NodeId, FunctionId)>>::new();
    for (callee, nodes) in &shape.calls {
        for node in nodes {
            let (block, _) = shape.place[node];
            calls
                .entry(shape.owner[&block])
                .or_default()
                .push((*node, *callee));
        }
    }
    let mut reached = module
        .live_functions()
        .map(|(function, _)| (function, World::Base))
        .collect::<BTreeSet<_>>();
    let mut work = reached.iter().copied().collect::<Vec<_>>();
    let mut reuse_calls = BTreeSet::new();
    let mut base_called = BTreeSet::new();
    while let Some((function, world)) = work.pop() {
        for (call, callee) in calls.get(&function).into_iter().flatten() {
            match route(module, shape, assumed, *call, &|atom| shared(world, atom)) {
                World::Reuse => {
                    reuse_calls.insert((*call, world));
                    if reached.insert((*callee, World::Reuse)) {
                        work.push((*callee, World::Reuse));
                    }
                }
                World::Base => {
                    base_called.insert(*callee);
                }
            }
        }
    }
    Routes {
        copied: reached
            .iter()
            .filter(|(function, world)| *world == World::Reuse && base_called.contains(function))
            .map(|(function, _)| *function)
            .collect(),
        reuse: reuse_calls,
    }
}

/// What a value may be as far as taking it apart goes: a row or a tuple, and which of its fields, where it is a tuple, may hold one. A row's slots are followed by the row; a tuple has no name to key its fields by, so its value carries them.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct Aggregate {
    may_be: bool,
    fields: Fields,
}

/// The fields of a tuple that may hold a row or a tuple: these, or any.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Fields {
    Of(BTreeSet<usize>),
    Any,
}

impl Default for Fields {
    fn default() -> Self {
        Fields::Of(BTreeSet::new())
    }
}

impl Aggregate {
    /// What a value the module cannot see into may be: anything.
    fn any() -> Self {
        Aggregate {
            may_be: true,
            fields: Fields::Any,
        }
    }
}

impl Lattice for Aggregate {
    fn bottom() -> Self {
        Aggregate::default()
    }

    fn join(&mut self, incoming: Self) {
        self.may_be |= incoming.may_be;
        self.fields = match (std::mem::take(&mut self.fields), incoming.fields) {
            (Fields::Any, _) | (_, Fields::Any) => Fields::Any,
            (Fields::Of(mut left), Fields::Of(right)) => {
                left.extend(right);
                Fields::Of(left)
            }
        };
    }
}

/// The values that may be a row or a tuple, which a read can take apart: a construction of one, a read of a row's slot some construction of it stores one in, a read of a tuple's field its constructions store one in, a list's element, what a closure, a cell or a channel hands back, a parameter of a function the module does not see every caller of, and what flows from any of them through calls, jumps and returns. A number, an immediate's payload, a sequence, bytes, a closure and what a host hands back are none, so a slot holding numbers is no aggregate whatever its carrier says.
fn aggregates(module: &Module, shape: &Shape) -> BTreeSet<ValueId> {
    let arities = module.return_arities();
    let keys = shape
        .defs
        .keys()
        .map(|value| TagKey::Value(*value))
        .collect::<Vec<_>>();
    let arities = &arities;
    let anything = |node: Option<&Node>| {
        matches!(
            node,
            Some(
                Node::Cell { .. }
                    | Node::Channel { .. }
                    | Node::ApplyFun {
                        callee: Callee::Closure(_),
                        ..
                    }
            )
        )
    };
    let facts = Solver::solve(keys, |rules| {
        let fact = |solver: &Solver<Aggregate, TagKey>, atom: &Atom| match atom {
            Atom::Value(value) => solver
                .fact(&TagKey::Value(*value))
                .cloned()
                .unwrap_or_default(),
            _ => Aggregate::default(),
        };
        for (value, def) in &shape.defs {
            let value = *value;
            let node = shape
                .binder
                .get(&value)
                .and_then(|binder| module.node(*binder));
            match *def {
                Def::Construction => match node {
                    Some(Node::LetValue {
                        value: ValueExpr::Row(row, atoms),
                        ..
                    }) => {
                        let (row, atoms) = (*row, atoms.clone());
                        rules.add(move |solver| {
                            solver.join(
                                TagKey::Value(value),
                                Aggregate {
                                    may_be: true,
                                    fields: Fields::default(),
                                },
                            );
                            for (index, atom) in atoms.iter().enumerate() {
                                let stored = fact(solver, atom);
                                solver.join(TagKey::Slot(Some(row), index), stored);
                            }
                        });
                    }
                    Some(Node::LetValue {
                        value: ValueExpr::Rebuilt { row, slots, .. },
                        ..
                    }) => {
                        let (row, slots) = (*row, slots.clone());
                        rules.add(move |solver| {
                            solver.join(
                                TagKey::Value(value),
                                Aggregate {
                                    may_be: true,
                                    fields: Fields::default(),
                                },
                            );
                            for (index, atom) in &slots {
                                let stored = fact(solver, atom);
                                solver.join(TagKey::Slot(Some(row), *index), stored);
                            }
                        });
                    }
                    Some(Node::LetValue {
                        value: ValueExpr::Tuple(atoms),
                        ..
                    }) => {
                        let atoms = atoms.clone();
                        rules.add(move |solver| {
                            let fields = atoms
                                .iter()
                                .enumerate()
                                .filter(|(_, atom)| fact(solver, atom).may_be)
                                .map(|(index, _)| index)
                                .collect();
                            solver.join(
                                TagKey::Value(value),
                                Aggregate {
                                    may_be: true,
                                    fields: Fields::Of(fields),
                                },
                            );
                        });
                    }
                    _ => {}
                },
                Def::Field(of) => match node {
                    Some(Node::LetIntrinsic {
                        op: Intrinsic::RowGet(row, index),
                        ..
                    }) => {
                        let slot = TagKey::Slot(Some(*row), *index);
                        rules.add(move |solver| {
                            let read = solver.fact(&slot).cloned().unwrap_or_default();
                            solver.join(TagKey::Value(value), read);
                        });
                    }
                    Some(Node::LetIntrinsic {
                        op: Intrinsic::TupleGet(index),
                        ..
                    }) => {
                        let index = *index;
                        rules.add(move |solver| {
                            let holds = match solver
                                .fact(&TagKey::Value(of))
                                .map(|parent| &parent.fields)
                            {
                                Some(Fields::Of(fields)) => fields.contains(&index),
                                Some(Fields::Any) | None => true,
                            };
                            if holds {
                                solver.join(TagKey::Value(value), Aggregate::any());
                            }
                        });
                    }
                    _ => {}
                },
                // A list's element may be anything; an immediate's payload is a number.
                Def::Other => {
                    if let Some(Node::LetIntrinsic {
                        op: Intrinsic::ListGet,
                        ..
                    }) = node
                    {
                        rules
                            .add(move |solver| solver.join(TagKey::Value(value), Aggregate::any()));
                    }
                }
                Def::Param(function, index) => {
                    let open = shape.open.contains(&function);
                    let calls = shape.calls.get(&function).cloned().unwrap_or_default();
                    rules.add(move |solver| {
                        if open {
                            solver.join(TagKey::Value(value), Aggregate::any());
                        }
                        for call in &calls {
                            if let Some(Node::ApplyFun { args, .. }) = module.node(*call)
                                && let Some(atom) = args.get(index)
                            {
                                let handed = fact(solver, atom);
                                solver.join(TagKey::Value(value), handed);
                            }
                        }
                    });
                }
                Def::ContParam(continuation, index) => {
                    let entries = shape
                        .entries
                        .get(&continuation)
                        .cloned()
                        .unwrap_or_default();
                    let resumes = shape
                        .resumes
                        .get(&continuation)
                        .cloned()
                        .unwrap_or_default();
                    rules.add(move |solver| {
                        for entry in &entries {
                            if let Some(atom) = entry.args.get(index) {
                                let handed = fact(solver, atom);
                                solver.join(TagKey::Value(value), handed);
                            }
                        }
                        for node in &resumes {
                            let returned = match module.node(*node) {
                                Some(Node::ApplyFun {
                                    callee: Callee::Known(callee),
                                    ..
                                }) => solver
                                    .fact(&TagKey::Returned(*callee, index))
                                    .cloned()
                                    .unwrap_or_default(),
                                other if anything(other) => Aggregate::any(),
                                _ => Aggregate::default(),
                            };
                            solver.join(TagKey::Value(value), returned);
                        }
                    });
                }
            }
        }
        for (function, nodes) in &shape.returns {
            let function = *function;
            for node in nodes {
                let node = *node;
                rules.add(move |solver| {
                    let arity = arities.get(&function).copied().unwrap_or(0);
                    let handed: Vec<(usize, Aggregate)> = match module.node(node) {
                        Some(Node::ApplyCont(_) | Node::Switch { .. }) => {
                            handed_back(module, function, node)
                                .into_iter()
                                .map(|(index, atom)| (index, fact(solver, atom)))
                                .collect()
                        }
                        Some(Node::ApplyFun {
                            callee: Callee::Known(callee),
                            ..
                        }) => (0..arity)
                            .map(|index| {
                                let returned = solver
                                    .fact(&TagKey::Returned(*callee, index))
                                    .cloned()
                                    .unwrap_or_default();
                                (index, returned)
                            })
                            .collect(),
                        other if anything(other) => {
                            (0..arity).map(|index| (index, Aggregate::any())).collect()
                        }
                        _ => Vec::new(),
                    };
                    for (index, handed) in handed {
                        solver.join(TagKey::Returned(function, index), handed);
                    }
                });
            }
        }
    });
    facts
        .into_iter()
        .filter_map(|(key, fact)| match key {
            TagKey::Value(value) if fact.may_be => Some(value),
            _ => None,
        })
        .collect()
}

/// A slot of a row as its sharing is followed: the row, its constructor's tag where a construction writes one and a test around a read decides it, and the slot. No tag stands for any constructor, which is a row's one where it holds no tag.
type SlotKey = (RowId, Option<u32>, usize);

/// What the construction binding `value` stores that a read could take apart and a rebuild write into: the values it holds that may be a row or a tuple, a constant among them left out, each with the slot it is stored at where that slot is a payload's.
fn stored_values(
    module: &Module,
    shape: &Shape,
    value: ValueId,
    aggregates: &BTreeSet<ValueId>,
) -> Vec<(Atom, Option<SlotKey>)> {
    let Some(Node::LetValue { value: built, .. }) = shape
        .binder
        .get(&value)
        .and_then(|binder| module.node(*binder))
    else {
        return Vec::new();
    };
    let (atoms, written) = match built {
        ValueExpr::Row(row, atoms) => (
            atoms,
            written_tag(module, *row, atoms).map(|tag| (*row, tag)),
        ),
        ValueExpr::List(atoms) | ValueExpr::Tuple(atoms) => (atoms, None),
        ValueExpr::Literal(_) | ValueExpr::Rebuilt { .. } => return Vec::new(),
    };
    atoms
        .iter()
        .enumerate()
        .filter(|(_, atom)| {
            matches!(atom, Atom::Value(stored) if !shape.constant.contains(stored) && aggregates.contains(stored))
        })
        .map(|(index, atom)| {
            let slot = written
                .map(|(row, tag)| (row, tag, index))
                .filter(|slot| shape.payload.contains(slot));
            (atom.clone(), slot)
        })
        .collect()
}

/// What a shared value stored at `slot` of the construction binding `value` shares: the slot where it is a payload's, under its constructor and under any, and the construction anywhere else.
fn shared_by(value: ValueId, slot: Option<SlotKey>) -> Vec<Key> {
    match slot {
        Some(slot) => vec![Key::Slot(slot), Key::Slot((slot.0, None, slot.2))],
        None => vec![Key::Value(value)],
    }
}

/// The constructor a construction of `row` writes, where one is stated: its literal tag, or none where the row holds no tag. A tag that is no literal states none, and such a construction's slots are judged with the construction.
fn written_tag(module: &Module, row: RowId, atoms: &[Atom]) -> Option<Option<u32>> {
    match module.defined_row(row)?.slots.first() {
        Some(Slot::Tag) => match atoms.first() {
            Some(Atom::Literal(Literal::Nat(tag))) => u32::try_from(tag).ok().map(Some),
            _ => None,
        },
        _ => Some(None),
    }
}

/// The slot the row read binding a field of `of` at `read` reads: under the tag a test around the read decides, or under any. A tuple's field is no slot.
fn slot_read(module: &Module, shape: &Shape, of: ValueId, read: NodeId) -> Option<SlotKey> {
    let Some(Node::LetIntrinsic {
        op: Intrinsic::RowGet(row, index),
        ..
    }) = module.node(read)
    else {
        return None;
    };
    let tag = match module.defined_row(*row)?.slots.first() {
        Some(Slot::Tag) => guard(module, shape, of, read),
        _ => None,
    };
    Some((*row, tag, *index))
}

/// Whether something takes a value apart, wherever the value flows.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
struct Taken(bool);

impl Lattice for Taken {
    fn bottom() -> Self {
        Taken(false)
    }

    fn join(&mut self, incoming: Self) {
        self.0 |= incoming.0;
    }
}

/// What is asked whether something takes it apart: a value, a function's result at one position, a row's slot, a tuple's field, or a list's element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Ask {
    Value(ValueId),
    Returned(FunctionId, usize),
    Slot(SlotKey),
    Field(usize),
    Element,
}

/// The payload slots: each slot a construction writes whose reads nothing takes apart, wherever what they hand out flows — through calls, jumps and returns, and through the slots, the tuples and the lists it is stored in again. A slot some reader takes apart is a child's, shared with the construction that holds it, so one tree something else holds says nothing of another; a payload's carries its own fact, so a leaf over a payload something else holds is still a leaf nothing else holds. Which slots are payloads decides what the facts can prove and never whether they hold: a slot's fact covers every store into it.
fn payload_slots(module: &Module, shape: &Shape) -> BTreeSet<SlotKey> {
    let arities = module.return_arities();
    let arities = &arities;
    let keys = shape
        .defs
        .keys()
        .map(|value| Ask::Value(*value))
        .collect::<Vec<_>>();
    let taken =
        |solver: &Solver<Taken, Ask>, key: Ask| solver.fact(&key).is_some_and(|fact| fact.0);
    let facts = Solver::solve(keys, |rules| {
        rules.add(move |solver| {
            for value in shape.fields.keys().chain(shape.read_at.keys()) {
                solver.join(Ask::Value(*value), Taken(true));
            }
        });
        // What a taken parameter, result, slot, field or element is handed or holds is taken.
        let hand = move |rules: &mut Rules<'_, Taken, Ask>, atom: &Atom, asked: Vec<Ask>| {
            if let Atom::Value(handed) = atom {
                let handed = *handed;
                rules.add(move |solver| {
                    if asked.iter().any(|key| taken(solver, *key)) {
                        solver.join(Ask::Value(handed), Taken(true));
                    }
                });
            }
        };
        for (node_id, (block, _)) in &shape.place {
            let node_id = *node_id;
            let Some(node) = module.node(node_id) else {
                continue;
            };
            let function = shape.owner[block];
            let own_return = module
                .function(function)
                .map(|function| function.return_cont);
            let handed_to = |target: ContinuationId, index: usize| match Some(target) == own_return
            {
                true => Some(Ask::Returned(function, index)),
                false => module
                    .continuation(target)
                    .and_then(|target| target.params.get(index))
                    .map(|param| Ask::Value(*param)),
            };
            match node {
                Node::LetValue { value, .. } => match value {
                    ValueExpr::Row(row, atoms) => {
                        if let Some(tag) = written_tag(module, *row, atoms) {
                            for (index, atom) in atoms.iter().enumerate() {
                                let asked = vec![
                                    Ask::Slot((*row, tag, index)),
                                    Ask::Slot((*row, None, index)),
                                ];
                                hand(rules, atom, asked);
                            }
                        }
                    }
                    ValueExpr::Tuple(atoms) => {
                        for (index, atom) in atoms.iter().enumerate() {
                            hand(rules, atom, vec![Ask::Field(index)]);
                        }
                    }
                    ValueExpr::List(atoms) => {
                        for atom in atoms {
                            hand(rules, atom, vec![Ask::Element]);
                        }
                    }
                    ValueExpr::Literal(_) | ValueExpr::Rebuilt { .. } => {}
                },
                Node::LetIntrinsic {
                    result, op, args, ..
                } => {
                    let result = *result;
                    let read = match (op, args.first()) {
                        (Intrinsic::RowGet(..), Some(Atom::Value(of))) => {
                            slot_read(module, shape, *of, node_id).map(Ask::Slot)
                        }
                        (Intrinsic::TupleGet(index), _) => Some(Ask::Field(*index)),
                        (Intrinsic::ListGet, _) => Some(Ask::Element),
                        _ => None,
                    };
                    if let Some(read) = read {
                        rules.add(move |solver| {
                            if taken(solver, Ask::Value(result)) {
                                solver.join(read, Taken(true));
                            }
                        });
                    }
                }
                Node::ApplyFun {
                    callee: Callee::Known(callee),
                    args,
                    return_to,
                } => {
                    let callee = *callee;
                    if let Some(definition) = module.function(callee) {
                        for (param, atom) in definition.params.iter().zip(args) {
                            hand(rules, atom, vec![Ask::Value(*param)]);
                        }
                    }
                    // What a call's result is handed to asks of what the callee hands back, a tail call's of what its own function's callers ask.
                    let resumed = match Some(*return_to) == own_return {
                        true => (0..arities.get(&function).copied().unwrap_or(0))
                            .map(|index| (index, Ask::Returned(function, index)))
                            .collect::<Vec<_>>(),
                        false => module
                            .continuation(*return_to)
                            .into_iter()
                            .flat_map(|resume| resume.params.iter().enumerate())
                            .map(|(index, param)| (index, Ask::Value(*param)))
                            .collect(),
                    };
                    rules.add(move |solver| {
                        for (index, asked) in &resumed {
                            if taken(solver, *asked) {
                                solver.join(Ask::Returned(callee, *index), Taken(true));
                            }
                        }
                    });
                }
                Node::ApplyCont(edge) => {
                    for (index, atom) in edge.args.iter().enumerate() {
                        hand(
                            rules,
                            atom,
                            handed_to(edge.target, index).into_iter().collect(),
                        );
                    }
                }
                Node::Switch { cases, default, .. } => {
                    for edge in cases.values().chain(default.as_ref()) {
                        for (index, atom) in edge.args.iter().enumerate() {
                            hand(
                                rules,
                                atom,
                                handed_to(edge.target, index).into_iter().collect(),
                            );
                        }
                    }
                }
                _ => {}
            }
        }
    });
    let taken = |key: Ask| facts.get(&key).is_some_and(|fact| fact.0);
    let mut payload = BTreeSet::new();
    for node_id in shape.place.keys() {
        if let Some(Node::LetValue {
            value: ValueExpr::Row(row, atoms),
            ..
        }) = module.node(*node_id)
            && let Some(tag) = written_tag(module, *row, atoms)
        {
            payload.extend(
                (0..atoms.len())
                    .map(|index| (*row, tag, index))
                    .filter(|slot| {
                        !taken(Ask::Slot(*slot)) && !taken(Ask::Slot((slot.0, None, slot.2)))
                    }),
            );
        }
    }
    payload
}

/// The version of its callee `call` reaches, where `shared` judges its arguments: the reuse version where each parameter that version assumes is handed a unique value dead after the call, and no constant itself, of which a rebuild writes nothing.
fn route(
    module: &Module,
    shape: &Shape,
    assumed: &BTreeMap<FunctionId, BTreeSet<usize>>,
    call: NodeId,
    shared: &dyn Fn(&Atom) -> bool,
) -> World {
    let Some(Node::ApplyFun {
        callee: Callee::Known(callee),
        args,
        ..
    }) = module.node(call)
    else {
        return World::Base;
    };
    match assumed.get(callee) {
        Some(params)
            if params.iter().all(|index| {
                args.get(*index).is_some_and(|atom| {
                    matches!(atom, Atom::Value(value) if !shape.constant.contains(value))
                        && !shared(atom)
                }) && handed_dead(module, shape, call, *index)
            }) =>
        {
            World::Reuse
        }
        _ => World::Base,
    }
}

/// A function's body or a continuation's: a chain of nodes, entered at its first.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Block {
    Body(FunctionId),
    Cont(ContinuationId),
}

/// How a node uses one of its operands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Site {
    /// A read that keeps nothing: a projection, a row test, a switch's scrutinee, a closure called.
    Read,
    /// Stored in a construction or written by a rebuild.
    Store,
    /// Handed to a known function's parameter.
    Call(FunctionId, usize),
    /// Handed to a continuation, on a jump or on one edge of a switch.
    Jump(ContinuationId),
    /// Handed back to the function's caller.
    Return(usize),
    /// Handed where nothing in the module follows it: a closure's argument, a host's, a cell's, a channel's, an operation's, or the value a rebuild writes into.
    Escape,
}

/// Where a value is defined.
#[derive(Debug, Clone, Copy)]
enum Def {
    Param(FunctionId, usize),
    ContParam(ContinuationId, usize),
    /// A projection of a value, by a row read or a tuple read.
    Field(ValueId),
    /// A construction or a rebuild: fresh where it is no constant.
    Construction,
    /// A literal, an operation's result: no row.
    Other,
}

/// The edge of a switch a use hands its value on, by its target and its case, `None` for the default; `None` for a use at a node.
type Edge = Option<(ContinuationId, Option<u32>)>;

/// One way a continuation is entered: the arguments a jump or a switch's edge hands it, the block that jump or switch ends, and the switch and key where it is a case.
#[derive(Debug, Clone)]
struct Entry {
    args: Vec<Atom>,
    from: Block,
    case: Option<(NodeId, u32)>,
}

/// A point a value is consumed at: a node, or one edge of a switch, which the switch's other edges do not take.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Point {
    node: NodeId,
    into: Edge,
}

/// The module read once: where each node sits, how each value is defined and used, and what is live where.
struct Shape {
    /// The block and the place along it of every node of a live function.
    place: BTreeMap<NodeId, (Block, usize)>,
    /// The function each block belongs to.
    owner: BTreeMap<Block, FunctionId>,
    /// The blocks a block's last node transfers to within its function.
    successors: BTreeMap<Block, Vec<Block>>,
    /// The block each continuation is defined in, and the place of the node defining it.
    defined_at: BTreeMap<ContinuationId, NodeId>,
    defs: BTreeMap<ValueId, Def>,
    /// The node binding each value a node binds.
    binder: BTreeMap<ValueId, NodeId>,
    /// The function each value belongs to.
    home: BTreeMap<ValueId, FunctionId>,
    uses: BTreeMap<ValueId, Vec<(NodeId, Site, Edge)>>,
    /// The one row a value is read at, by every row read and row test of it.
    read_at: BTreeMap<ValueId, Option<RowId>>,
    /// The projections of each value.
    fields: BTreeMap<ValueId, Vec<ValueId>>,
    /// The slot each projection reads.
    slot_of: BTreeMap<ValueId, usize>,
    /// The row reads of each value: the row and what the read binds.
    row_reads: BTreeMap<ValueId, Vec<(RowId, ValueId)>>,
    /// The values each function's nodes name but do not bind, and the functions defined within it name likewise.
    captured: BTreeSet<ValueId>,
    /// What is live on entry to each block.
    live_in: BTreeMap<Block, BTreeSet<ValueId>>,
    /// The last place along its block each value is named at.
    last_named: BTreeMap<(Block, ValueId), usize>,
    /// The known calls of each function.
    calls: BTreeMap<FunctionId, Vec<NodeId>>,
    /// The calls each continuation resumes, and the edges of a switch that enter it with their case.
    resumes: BTreeMap<ContinuationId, Vec<NodeId>>,
    entries: BTreeMap<ContinuationId, Vec<Entry>>,
    returns: BTreeMap<FunctionId, Vec<NodeId>>,
    /// The functions whose parameters callers outside the module's sight hand values to: the escaping ones and the entry.
    open: BTreeSet<FunctionId>,
    /// The values that may be constant data, each a global every use of it shares.
    constant: BTreeSet<ValueId>,
    /// The slots whose reads nothing takes apart, each carrying its own sharing.
    payload: BTreeSet<SlotKey>,
    /// The blocks reachable from each block's successors, as asked.
    reach: RefCell<BTreeMap<Block, BTreeSet<Block>>>,
    /// The blocks reachable from a block's successors without entering another, the first of each key, as asked.
    reach_outside: RefCell<BTreeMap<(Block, Block), BTreeSet<Block>>>,
}

impl Shape {
    fn of(module: &Module) -> Self {
        let mut shape = Shape {
            place: BTreeMap::new(),
            owner: BTreeMap::new(),
            successors: BTreeMap::new(),
            defined_at: BTreeMap::new(),
            defs: BTreeMap::new(),
            binder: BTreeMap::new(),
            home: BTreeMap::new(),
            uses: BTreeMap::new(),
            read_at: BTreeMap::new(),
            fields: BTreeMap::new(),
            slot_of: BTreeMap::new(),
            row_reads: BTreeMap::new(),
            captured: BTreeSet::new(),
            live_in: BTreeMap::new(),
            last_named: BTreeMap::new(),
            calls: BTreeMap::new(),
            resumes: BTreeMap::new(),
            entries: BTreeMap::new(),
            returns: BTreeMap::new(),
            open: analyze_calls(module).escaping,
            constant: module.constant_data(),
            payload: BTreeSet::new(),
            reach: RefCell::new(BTreeMap::new()),
            reach_outside: RefCell::new(BTreeMap::new()),
        };
        shape.open.extend(module.entry());
        let mut free = BTreeMap::<FunctionId, BTreeSet<ValueId>>::new();
        let mut nested = BTreeMap::<NodeId, Vec<FunctionId>>::new();
        let mut blocks = BTreeMap::<Block, Vec<NodeId>>::new();
        for (function, definition) in module.live_functions() {
            for (index, param) in definition.params.iter().enumerate() {
                shape.defs.insert(*param, Def::Param(function, index));
                shape.home.insert(*param, function);
            }
            let mut bound = definition.params.iter().copied().collect::<BTreeSet<_>>();
            let mut named = BTreeSet::new();
            let mut work = vec![(Block::Body(function), definition.body)];
            while let Some((block, start)) = work.pop() {
                shape.owner.insert(block, function);
                let mut chain = Vec::new();
                let mut at = start;
                loop {
                    shape.place.insert(at, (block, chain.len()));
                    chain.push(at);
                    let node = module.node(at).unwrap();
                    for (value, site, into) in node_sites(node, definition.return_cont) {
                        named.insert(value);
                        shape.uses.entry(value).or_default().push((at, site, into));
                    }
                    match node {
                        Node::LetValue {
                            result,
                            value,
                            next,
                        } => {
                            bound.insert(*result);
                            shape.home.insert(*result, function);
                            shape.binder.insert(*result, at);
                            shape.defs.insert(
                                *result,
                                match value {
                                    ValueExpr::Literal(_) => Def::Other,
                                    ValueExpr::List(_)
                                    | ValueExpr::Tuple(_)
                                    | ValueExpr::Row(..)
                                    | ValueExpr::Rebuilt { .. } => Def::Construction,
                                },
                            );
                            at = *next;
                        }
                        Node::LetIntrinsic {
                            result,
                            op,
                            args,
                            next,
                        } => {
                            bound.insert(*result);
                            shape.home.insert(*result, function);
                            shape.binder.insert(*result, at);
                            let def = match (op, args.first()) {
                                (
                                    Intrinsic::RowGet(..) | Intrinsic::TupleGet(_),
                                    Some(Atom::Value(of)),
                                ) => {
                                    shape.fields.entry(*of).or_default().push(*result);
                                    if let Intrinsic::RowGet(_, slot) | Intrinsic::TupleGet(slot) =
                                        op
                                    {
                                        shape.slot_of.insert(*result, *slot);
                                    }
                                    if let Intrinsic::RowGet(row, _) = op {
                                        shape
                                            .row_reads
                                            .entry(*of)
                                            .or_default()
                                            .push((*row, *result));
                                    }
                                    Def::Field(*of)
                                }
                                _ => Def::Other,
                            };
                            shape.defs.insert(*result, def);
                            if let (
                                Intrinsic::RowGet(row, _) | Intrinsic::IsRow(row),
                                Some(Atom::Value(read)),
                            ) = (op, args.first())
                            {
                                let entry = shape.read_at.entry(*read).or_insert(Some(*row));
                                if *entry != Some(*row) {
                                    *entry = None;
                                }
                            }
                            at = *next;
                        }
                        Node::LetFun { functions, body } => {
                            nested.insert(at, functions.clone());
                            at = *body;
                        }
                        Node::LetCont {
                            continuations,
                            body,
                        } => {
                            for continuation in continuations {
                                if let Some(definition) = module.continuation(*continuation) {
                                    shape.defined_at.insert(*continuation, at);
                                    for (index, param) in definition.params.iter().enumerate() {
                                        bound.insert(*param);
                                        shape.home.insert(*param, function);
                                        shape
                                            .defs
                                            .insert(*param, Def::ContParam(*continuation, index));
                                    }
                                    work.push((Block::Cont(*continuation), definition.body));
                                }
                            }
                            at = *body;
                        }
                        terminator => {
                            let mut successors = Vec::new();
                            let mut enter = |target: ContinuationId| {
                                if target != definition.return_cont {
                                    successors.push(Block::Cont(target));
                                }
                            };
                            match terminator {
                                Node::ApplyFun {
                                    callee, return_to, ..
                                } => {
                                    if let Callee::Known(callee) = callee {
                                        shape.calls.entry(*callee).or_default().push(at);
                                    }
                                    match *return_to == definition.return_cont {
                                        true => shape.returns.entry(function).or_default().push(at),
                                        false => {
                                            shape.resumes.entry(*return_to).or_default().push(at)
                                        }
                                    }
                                    enter(*return_to);
                                }
                                Node::Foreign { return_to, .. }
                                | Node::Cell { return_to, .. }
                                | Node::Channel { return_to, .. }
                                | Node::Intrinsic { return_to, .. } => {
                                    match *return_to == definition.return_cont {
                                        true => shape.returns.entry(function).or_default().push(at),
                                        false => {
                                            shape.resumes.entry(*return_to).or_default().push(at)
                                        }
                                    }
                                    enter(*return_to);
                                }
                                Node::ApplyCont(edge) => {
                                    match edge.target == definition.return_cont {
                                        true => shape.returns.entry(function).or_default().push(at),
                                        false => {
                                            shape.entries.entry(edge.target).or_default().push(
                                                Entry {
                                                    args: edge.args.clone(),
                                                    from: block,
                                                    case: None,
                                                },
                                            )
                                        }
                                    }
                                    enter(edge.target);
                                }
                                Node::Switch { cases, default, .. } => {
                                    // An edge to the function's return hands its arguments back, as a jump there does, and enters no continuation.
                                    if !return_edges(terminator, definition.return_cont).is_empty()
                                    {
                                        shape.returns.entry(function).or_default().push(at);
                                    }
                                    let keyed = cases.iter().map(|(key, edge)| (Some(*key), edge));
                                    for (key, edge) in
                                        keyed.chain(default.iter().map(|edge| (None, edge)))
                                    {
                                        if edge.target != definition.return_cont {
                                            shape.entries.entry(edge.target).or_default().push(
                                                Entry {
                                                    args: edge.args.clone(),
                                                    from: block,
                                                    case: key.map(|key| (at, key)),
                                                },
                                            );
                                        }
                                        enter(edge.target);
                                    }
                                }
                                _ => {}
                            }
                            shape.successors.insert(block, successors);
                            break;
                        }
                    }
                }
                blocks.insert(block, chain);
            }
            free.insert(function, named.difference(&bound).copied().collect());
        }
        // What a nested function names and binds nowhere is captured where it is defined, so a function's free values include its members'.
        loop {
            let mut changed = false;
            for (node, members) in &nested {
                let Some((block, _)) = shape.place.get(node) else {
                    continue;
                };
                let function = shape.owner[block];
                let captured = members
                    .iter()
                    .flat_map(|member| free.get(member).cloned().unwrap_or_default())
                    .filter(|value| shape.home.get(value) != Some(&function))
                    .collect::<Vec<_>>();
                let entry = free.entry(function).or_default();
                for value in captured {
                    changed |= entry.insert(value);
                }
            }
            if !changed {
                break;
            }
        }
        shape.captured = free.values().flatten().copied().collect();
        // A function defined at a node names what it captures there.
        for (node, members) in &nested {
            let Some(&(block, at)) = shape.place.get(node) else {
                continue;
            };
            for value in members
                .iter()
                .flat_map(|member| free.get(member).cloned().unwrap_or_default())
            {
                let last = shape.last_named.entry((block, value)).or_insert(at);
                *last = (*last).max(at);
            }
        }
        for (value, uses) in &shape.uses {
            for (node, _, _) in uses {
                let (block, at) = shape.place[node];
                let last = shape.last_named.entry((block, *value)).or_insert(at);
                *last = (*last).max(at);
            }
        }
        shape.live_in = live_in(module, &shape, &blocks);
        shape.payload = payload_slots(module, &shape);
        shape
    }

    /// Whether `value` is read after the consumption at `point`, on its edge.
    fn live_after(&self, module: &Module, point: Point, value: ValueId) -> bool {
        match point.into {
            Some((target, _)) => module.continuation(target).is_some_and(|_| {
                self.live_in
                    .get(&Block::Cont(target))
                    .is_some_and(|live| live.contains(&value))
            }),
            None => self.live_past(point.node, value),
        }
    }

    /// Whether `value` is named past `node`: later along its block, or on entry to a block after it.
    fn live_past(&self, node: NodeId, value: ValueId) -> bool {
        let (block, at) = self.place[&node];
        self.last_named
            .get(&(block, value))
            .is_some_and(|last| *last > at)
            || self.successors[&block].iter().any(|successor| {
                self.live_in
                    .get(successor)
                    .is_some_and(|live| live.contains(&value))
            })
    }

    /// Whether `value` is named at `node` or past it.
    fn live_at(&self, node: NodeId, value: ValueId) -> bool {
        let (block, at) = self.place[&node];
        self.last_named
            .get(&(block, value))
            .is_some_and(|last| *last >= at)
            || self.live_past(node, value)
    }

    /// Whether `to` can run after `from` in their function.
    fn reaches(&self, from: NodeId, to: NodeId) -> bool {
        let ((from_block, from_at), (to_block, to_at)) = (self.place[&from], self.place[&to]);
        (from_block == to_block && to_at > from_at) || self.reached(from_block).contains(&to_block)
    }

    /// Whether `to` can run after `from` while `value` is still the value `from` names: on a path that does not enter the block binding it, where a run binds it again.
    fn reaches_holding(&self, value: ValueId, from: NodeId, to: NodeId) -> bool {
        let Some(home) = self.home(value) else {
            return self.reaches(from, to);
        };
        let ((from_block, from_at), (to_block, to_at)) = (self.place[&from], self.place[&to]);
        (from_block == to_block && to_at > from_at)
            || self.reached_outside(home, from_block).contains(&to_block)
    }

    /// Whether `to` can run after `from` while `value` names the object it names at `from`: before a run binds it again, or past one where that run may bind it to the same object ([`Self::may_bind_again_to_itself`]).
    fn reaches_as_one(&self, value: ValueId, from: NodeId, to: NodeId) -> bool {
        match self.may_bind_again_to_itself(value) {
            true => self.reaches(from, to),
            false => self.reaches_holding(value, from, to),
        }
    }

    /// Whether a run that binds `value` again may bind it to the object it named: every value may but a loop's parameter each entry its loop's own body makes hands another object, which is a field of the parameter, no object holding itself, a construction, a literal, a filler, or a join's parameter each of whose entries hands one of these.
    fn may_bind_again_to_itself(&self, value: ValueId) -> bool {
        let Some(Def::ContParam(continuation, index)) = self.defs.get(&value) else {
            return true;
        };
        let body = self.reached(Block::Cont(*continuation));
        let mut handed = self
            .entries
            .get(continuation)
            .into_iter()
            .flatten()
            .filter(|entry| body.contains(&entry.from))
            .map(|entry| entry.args[*index].clone())
            .collect::<Vec<_>>();
        let mut joins = BTreeSet::new();
        while let Some(atom) = handed.pop() {
            let Atom::Value(other) = atom else {
                continue;
            };
            if other == value || self.constant.contains(&other) {
                return true;
            }
            match self.defs.get(&other) {
                Some(Def::Construction) => {}
                Some(Def::Field(of)) if *of == value => {}
                Some(Def::ContParam(join, at)) => {
                    if joins.insert((*join, *at)) {
                        handed.extend(
                            self.entries
                                .get(join)
                                .into_iter()
                                .flatten()
                                .map(|entry| entry.args[*at].clone()),
                        );
                    }
                }
                _ => return true,
            }
        }
        false
    }

    /// The block that binds `value`: its function's body, its continuation's, or the one its binder sits along.
    fn home(&self, value: ValueId) -> Option<Block> {
        match self.defs.get(&value) {
            Some(Def::Param(function, _)) => Some(Block::Body(*function)),
            Some(Def::ContParam(continuation, _)) => Some(Block::Cont(*continuation)),
            _ => self.binder.get(&value).map(|binder| self.place[binder].0),
        }
    }

    /// The blocks reachable from `block`'s successors on paths that never enter `closed`.
    fn reached_outside(&self, closed: Block, block: Block) -> BTreeSet<Block> {
        if let Some(reached) = self.reach_outside.borrow().get(&(closed, block)) {
            return reached.clone();
        }
        let mut reached = BTreeSet::new();
        let mut work = self.successors.get(&block).cloned().unwrap_or_default();
        while let Some(next) = work.pop() {
            if next != closed && reached.insert(next) {
                work.extend(self.successors.get(&next).cloned().unwrap_or_default());
            }
        }
        self.reach_outside
            .borrow_mut()
            .insert((closed, block), reached.clone());
        reached
    }

    /// Whether `to` can run after the consumption at `point`, on its edge.
    fn point_reaches(&self, point: Point, to: NodeId) -> bool {
        match point.into {
            Some((target, _)) => {
                let (to_block, _) = self.place[&to];
                to_block == Block::Cont(target)
                    || self.reached(Block::Cont(target)).contains(&to_block)
            }
            None => point.node == to || self.reaches(point.node, to),
        }
    }

    /// The blocks reachable from `block`'s successors.
    fn reached(&self, block: Block) -> BTreeSet<Block> {
        if let Some(reached) = self.reach.borrow().get(&block) {
            return reached.clone();
        }
        let mut reached = BTreeSet::new();
        let mut work = self.successors.get(&block).cloned().unwrap_or_default();
        while let Some(next) = work.pop() {
            if reached.insert(next) {
                work.extend(self.successors.get(&next).cloned().unwrap_or_default());
            }
        }
        self.reach.borrow_mut().insert(block, reached.clone());
        reached
    }

    /// Where `value` is consumed, a call to a borrowed parameter being a read.
    fn consumed(&self, value: ValueId, borrowed: &BTreeSet<(FunctionId, usize)>) -> Vec<Point> {
        self.uses
            .get(&value)
            .into_iter()
            .flatten()
            .filter(|(_, site, _)| match site {
                Site::Read => false,
                Site::Call(callee, index) => !borrowed.contains(&(*callee, *index)),
                _ => true,
            })
            .map(|(node, _, into)| Point {
                node: *node,
                into: *into,
            })
            .collect()
    }

    /// Whether a read of `value` at `row` runs on every path to `node`, its result in scope there: the read's checked cast is what says `value` is a struct of that row, a value read as a tuple on one arm and as a row on another being either.
    fn read_as_row_before(&self, value: ValueId, row: RowId, node: NodeId) -> bool {
        self.row_reads
            .get(&value)
            .into_iter()
            .flatten()
            .any(|(read, result)| *read == row && self.in_scope(*result, node))
    }

    /// Whether a run can come back to `node` without binding `value` again: a rebuild there would write the memory the last round's rebuild handed on, and a read of it there names what the last round's read named.
    fn returns_without_rebinding(&self, value: ValueId, node: NodeId) -> bool {
        let (block, _) = self.place[&node];
        match self.home(value) {
            Some(home) => self.reached_outside(home, block).contains(&block),
            None => true,
        }
    }

    /// Whether `value` is defined where `node` can name it: before it along a block enclosing it.
    fn in_scope(&self, value: ValueId, node: NodeId) -> bool {
        let anchor = match self.defs.get(&value) {
            Some(Def::Param(function, _)) => {
                return self
                    .place
                    .get(&node)
                    .is_some_and(|(block, _)| self.owner[block] == *function);
            }
            Some(Def::ContParam(continuation, _)) => Some((Block::Cont(*continuation), 0)),
            _ => self.binder.get(&value).map(|binder| {
                let (block, at) = self.place[binder];
                (block, at + 1)
            }),
        };
        let Some((home, from)) = anchor else {
            return false;
        };
        let mut current = node;
        loop {
            let (block, at) = self.place[&current];
            if block == home {
                return at >= from;
            }
            match block {
                Block::Cont(continuation) => match self.defined_at.get(&continuation) {
                    Some(definer) => current = *definer,
                    None => return false,
                },
                Block::Body(_) => return false,
            }
        }
    }

    /// The parameters a function only reads, given the values rebuilt when every parameter is owned: optimistic, each demoted where it must own.
    fn borrowed(
        &self,
        module: &Module,
        reused: &BTreeSet<ValueId>,
    ) -> BTreeSet<(FunctionId, usize)> {
        let mut borrowed = module
            .live_functions()
            .filter(|(function, _)| !self.open.contains(function))
            .flat_map(|(function, definition)| {
                (0..definition.params.len()).map(move |index| (function, index))
            })
            .collect::<BTreeSet<_>>();
        loop {
            let snapshot = borrowed.clone();
            borrowed.retain(|(function, index)| {
                let param = module.function(*function).unwrap().params[*index];
                !self.must_own(param, &snapshot, reused)
            });
            if borrowed.len() == snapshot.len() {
                return borrowed;
            }
        }
    }

    /// Whether a function must own `value`: it consumes it, captures it or rebuilds it, or must own a field of it.
    fn must_own(
        &self,
        value: ValueId,
        borrowed: &BTreeSet<(FunctionId, usize)>,
        reused: &BTreeSet<ValueId>,
    ) -> bool {
        reused.contains(&value)
            || self.captured.contains(&value)
            || !self.consumed(value, borrowed).is_empty()
            || self
                .fields
                .get(&value)
                .into_iter()
                .flatten()
                .any(|field| self.must_own(*field, borrowed, reused))
    }
}

/// How `node` uses each operand it names, with the edge of a switch it hands it on.
fn node_sites(node: &Node, own_return: ContinuationId) -> Vec<(ValueId, Site, Edge)> {
    let mut sites = Vec::new();
    let mut push = |atom: &Atom, site: Site, into: Edge| {
        if let Atom::Value(value) = atom {
            sites.push((*value, site, into));
        }
    };
    match node {
        Node::LetValue { value, .. } => match value {
            ValueExpr::Literal(_) => {}
            ValueExpr::List(atoms) | ValueExpr::Tuple(atoms) | ValueExpr::Row(_, atoms) => {
                atoms.iter().for_each(|atom| push(atom, Site::Store, None))
            }
            ValueExpr::Rebuilt { of, slots, .. } => {
                push(of, Site::Escape, None);
                slots
                    .iter()
                    .for_each(|(_, atom)| push(atom, Site::Store, None));
            }
        },
        Node::LetIntrinsic { op, args, .. } => {
            let site = match op {
                Intrinsic::RowGet(..)
                | Intrinsic::TupleGet(_)
                | Intrinsic::IsRow(_)
                | Intrinsic::IsImmediate => Site::Read,
                _ => Site::Escape,
            };
            args.iter().for_each(|atom| push(atom, site, None));
        }
        Node::ApplyFun { callee, args, .. } => {
            if let Callee::Closure(closure) = callee {
                push(&Atom::Value(*closure), Site::Read, None);
            }
            for (index, atom) in args.iter().enumerate() {
                let site = match callee {
                    Callee::Known(function) => Site::Call(*function, index),
                    Callee::Closure(_) => Site::Escape,
                };
                push(atom, site, None);
            }
        }
        Node::ApplyCont(edge) => {
            for (index, atom) in edge.args.iter().enumerate() {
                let site = match edge.target == own_return {
                    true => Site::Return(index),
                    false => Site::Jump(edge.target),
                };
                push(atom, site, None);
            }
        }
        Node::Switch {
            scrutinee,
            cases,
            default,
        } => {
            push(scrutinee, Site::Read, None);
            let keyed = cases.iter().map(|(key, edge)| (Some(*key), edge));
            for (key, edge) in keyed.chain(default.iter().map(|edge| (None, edge))) {
                for (index, atom) in edge.args.iter().enumerate() {
                    let site = match edge.target == own_return {
                        true => Site::Return(index),
                        false => Site::Jump(edge.target),
                    };
                    push(atom, site, Some((edge.target, key)));
                }
            }
        }
        Node::Foreign { args, .. }
        | Node::Halt { args, .. }
        | Node::Cell { args, .. }
        | Node::Channel { args, .. }
        | Node::Intrinsic { args, .. } => {
            args.iter().for_each(|atom| push(atom, Site::Escape, None))
        }
        Node::LetFun { .. } | Node::LetCont { .. } | Node::Panic(_) | Node::Unreachable => {}
    }
    sites
}

/// What is live on entry to each block: what it names before binding, and what a block after it needs, short of what its successor binds as parameters.
fn live_in(
    module: &Module,
    shape: &Shape,
    blocks: &BTreeMap<Block, Vec<NodeId>>,
) -> BTreeMap<Block, BTreeSet<ValueId>> {
    let mut named = BTreeMap::<Block, BTreeSet<ValueId>>::new();
    for (block, value) in shape.last_named.keys() {
        named.entry(*block).or_default().insert(*value);
    }
    let bound = |block: Block| -> BTreeSet<ValueId> {
        let mut bound = BTreeSet::new();
        if let Block::Cont(continuation) = block
            && let Some(definition) = module.continuation(continuation)
        {
            bound.extend(&definition.params);
        }
        for node in blocks.get(&block).into_iter().flatten() {
            if let Some(Node::LetValue { result, .. } | Node::LetIntrinsic { result, .. }) =
                module.node(*node)
            {
                bound.insert(*result);
            }
        }
        bound
    };
    let bound = blocks
        .keys()
        .map(|block| (*block, bound(*block)))
        .collect::<BTreeMap<_, _>>();
    let mut live = BTreeMap::<Block, BTreeSet<ValueId>>::new();
    loop {
        let mut changed = false;
        for block in blocks.keys().rev() {
            let mut entry = named.get(block).cloned().unwrap_or_default();
            for successor in &shape.successors[block] {
                entry.extend(live.get(successor).into_iter().flatten());
            }
            entry.retain(|value| !bound[block].contains(value));
            if live.get(block) != Some(&entry) {
                live.insert(*block, entry);
                changed = true;
            }
        }
        if !changed {
            return live;
        }
    }
}

/// Whether a value is shared: the dual the solver's least fixpoint finds, uniqueness being what it leaves.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
struct Shared(bool);

impl Lattice for Shared {
    fn bottom() -> Self {
        Shared(false)
    }

    fn join(&mut self, incoming: Self) {
        self.0 |= incoming.0;
    }
}

/// A value's fact, a function's result's at one position, or a payload slot's: that some construction stores a shared value in it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Key {
    Value(ValueId),
    Returned(FunctionId, usize),
    Slot(SlotKey),
}

/// The uniqueness the module proves, under one choice of borrowed parameters.
struct Facts<'s> {
    module: &'s Module,
    shape: &'s Shape,
    borrowed: &'s BTreeSet<(FunctionId, usize)>,
    shared: BTreeSet<ValueId>,
}

impl<'s> Facts<'s> {
    fn solve(
        module: &'s Module,
        shape: &'s Shape,
        borrowed: &'s BTreeSet<(FunctionId, usize)>,
    ) -> Self {
        let aggregates = &aggregates(module, shape);
        let sources = sources(module, shape, borrowed, &BTreeMap::new(), aggregates);
        let arities = module.return_arities();
        let keys =
            shape
                .defs
                .keys()
                .map(|value| Key::Value(*value))
                .chain(arities.iter().flat_map(|(function, arity)| {
                    (0..*arity).map(|index| Key::Returned(*function, index))
                }));
        let (sources, arities) = (&sources, &arities);
        let facts = Solver::solve(keys, |rules| {
            rules.add(move |solver| {
                for key in sources {
                    solver.join(*key, Shared(true));
                }
            });
            let shared = |solver: &Solver<Shared, Key>, atom: &Atom| match atom {
                // A filler is null, and a literal and a value that may be neither a row nor a tuple are no row, so nothing shares what a rebuild could write; a function is a closure the module does not see into.
                Atom::Value(value) if !aggregates.contains(value) => false,
                Atom::Value(value) => solver.fact(&Key::Value(*value)).is_none_or(|fact| fact.0),
                Atom::Filler | Atom::Literal(_) => false,
                Atom::Fun(_) => true,
            };
            for (value, def) in &shape.defs {
                let value = *value;
                match *def {
                    Def::Field(of) => {
                        let read = shape.binder[&value];
                        let consumed_after = shape
                            .consumed(of, borrowed)
                            .iter()
                            .any(|point| shape.reaches_holding(of, read, point.node));
                        let slot = slot_read(module, shape, of, read);
                        rules.add(move |solver| {
                            let marked = slot.is_some_and(|slot| {
                                solver.fact(&Key::Slot(slot)).is_some_and(|fact| fact.0)
                            });
                            if consumed_after || marked || shared(solver, &Atom::Value(of)) {
                                solver.join(Key::Value(value), Shared(true));
                            }
                        });
                    }
                    Def::Param(function, index) => {
                        let calls = shape.calls.get(&function).cloned().unwrap_or_default();
                        rules.add(move |solver| {
                            for call in &calls {
                                if let Some(Node::ApplyFun { args, .. }) = module.node(*call)
                                    && args.get(index).is_none_or(|atom| shared(solver, atom))
                                {
                                    solver.join(Key::Value(value), Shared(true));
                                }
                            }
                        });
                    }
                    Def::ContParam(continuation, index) => {
                        let entries = shape
                            .entries
                            .get(&continuation)
                            .cloned()
                            .unwrap_or_default();
                        let resumes = shape
                            .resumes
                            .get(&continuation)
                            .cloned()
                            .unwrap_or_default();
                        rules.add(move |solver| {
                            let entered = entries.iter().any(|entry| {
                                entry
                                    .args
                                    .get(index)
                                    .is_none_or(|atom| shared(solver, atom))
                            });
                            let resumed = resumes.iter().any(|node| match module.node(*node) {
                                Some(Node::ApplyFun {
                                    callee: Callee::Known(callee),
                                    ..
                                }) => solver
                                    .fact(&Key::Returned(*callee, index))
                                    .is_none_or(|fact| fact.0),
                                _ => true,
                            });
                            if entered || resumed {
                                solver.join(Key::Value(value), Shared(true));
                            }
                        });
                    }
                    // A construction holding what something else holds is shared, since a read of it hands that out, but where it holds it in a payload slot, which is shared in its place. A constant is never written into, so it spoils nothing.
                    Def::Construction => {
                        let stored = stored_values(module, shape, value, aggregates);
                        rules.add(move |solver| {
                            for (atom, slot) in &stored {
                                if shared(solver, atom) {
                                    for key in shared_by(value, *slot) {
                                        solver.join(key, Shared(true));
                                    }
                                }
                            }
                        });
                    }
                    Def::Other => {}
                }
            }
            for (function, nodes) in &shape.returns {
                let function = *function;
                for node in nodes {
                    let node = *node;
                    rules.add(move |solver| match module.node(node) {
                        Some(Node::ApplyCont(_) | Node::Switch { .. }) => {
                            for (index, atom) in handed_back(module, function, node) {
                                if shared(solver, atom) {
                                    solver.join(Key::Returned(function, index), Shared(true));
                                }
                            }
                        }
                        Some(Node::ApplyFun {
                            callee: Callee::Known(callee),
                            ..
                        }) => {
                            for index in 0..arities.get(&function).copied().unwrap_or(0) {
                                if solver
                                    .fact(&Key::Returned(*callee, index))
                                    .is_none_or(|fact| fact.0)
                                {
                                    solver.join(Key::Returned(function, index), Shared(true));
                                }
                            }
                        }
                        _ => {}
                    });
                }
            }
        });
        let shared = facts
            .into_iter()
            .filter_map(|(key, fact)| match (key, fact) {
                (Key::Value(value), Shared(true)) => Some(value),
                _ => None,
            })
            .collect();
        Facts {
            module,
            shape,
            borrowed,
            shared,
        }
    }

    /// The constructions of a row beside a unique value of that row, dead there and consumed on no path to them, rebuilt at most once while it names one object, and never where a constant may be the value and no tag test rules it out.
    fn rebuilds(&self, tags: &BTreeMap<ValueId, Tags>) -> Vec<Rebuild> {
        let (module, shape) = (self.module, self.shape);
        let mut by_row = BTreeMap::<(FunctionId, RowId), Vec<ValueId>>::new();
        for (value, row) in &shape.read_at {
            if let (Some(row), Some(function)) = (row, shape.home.get(value))
                && !self.shared.contains(value)
            {
                by_row.entry((*function, *row)).or_default().push(*value);
            }
        }
        let mut chosen = BTreeMap::<ValueId, Vec<NodeId>>::new();
        let mut rebuilds = Vec::new();
        for (node_id, (block, _)) in &shape.place {
            let Some(Node::LetValue {
                result,
                value: ValueExpr::Row(row, atoms),
                ..
            }) = module.node(*node_id)
            else {
                continue;
            };
            if shape.constant.contains(result) {
                continue;
            }
            let function = shape.owner[block];
            // Of the values that qualify, the one the construction keeps most of, so the rebuild writes the fewest slots.
            let found = by_row
                .get(&(function, *row))
                .into_iter()
                .flatten()
                .copied()
                .filter(|value| {
                    shape
                        .consumed(*value, self.borrowed)
                        .iter()
                        .all(|point| !shape.point_reaches(*point, *node_id))
                        && !shape.live_at(*node_id, *value)
                        && shape.in_scope(*value, *node_id)
                        && shape.read_as_row_before(*value, *row, *node_id)
                        && !shape.returns_without_rebinding(*value, *node_id)
                        && clear_of_constants(module, shape, tags, *value, *node_id)
                        && chosen.get(value).into_iter().flatten().all(|other| {
                            !shape.reaches_as_one(*value, *other, *node_id)
                                && !shape.reaches_as_one(*value, *node_id, *other)
                        })
                })
                .map(|of| {
                    let tag = guard(module, shape, of, *node_id);
                    (of, written(module, shape, of, *row, atoms, tag))
                })
                .min_by_key(|(_, slots)| slots.len());
            if let Some((of, slots)) = found {
                chosen.entry(of).or_default().push(*node_id);
                rebuilds.push(Rebuild {
                    node: *node_id,
                    of,
                    row: *row,
                    slots,
                });
            }
        }
        rebuilds
    }
}

/// The facts that hold whatever the others say: a value's own sharing — an operation's result, a parameter some call hands a value read after it or the module does not see the callers of, a value consumed twice or before its last use, a captured one, a field another read on its path reads or its own read reads again on a later round — and a result a function hands back from where the module cannot follow. A parameter `assumed` lists is the reuse version's, unique as it receives it. What a call or a jump hands a parameter shares it only where it may be a row or a tuple, one of `aggregates`: a number handed at two parameters holds nothing a rebuild writes.
fn sources(
    module: &Module,
    shape: &Shape,
    borrowed: &BTreeSet<(FunctionId, usize)>,
    assumed: &BTreeMap<FunctionId, BTreeSet<usize>>,
    aggregates: &BTreeSet<ValueId>,
) -> BTreeSet<Key> {
    let arities = module.return_arities();
    let mut sources = BTreeSet::new();
    let may_hold_a_row = |atom: Option<&Atom>| match atom {
        Some(Atom::Value(handed)) => aggregates.contains(handed),
        Some(Atom::Filler | Atom::Literal(_)) => false,
        Some(Atom::Fun(_)) | None => true,
    };
    for (value, def) in &shape.defs {
        // Constant data is one object wherever it is named, and its tag keeps it from a rebuild.
        if shape.constant.contains(value) {
            continue;
        }
        let consumed = shape.consumed(*value, borrowed);
        let mut seen = BTreeSet::new();
        let twice = consumed.iter().any(|point| !seen.insert(*point));
        let not_last = consumed
            .iter()
            .any(|point| shape.live_after(module, *point, *value));
        let source = match def {
            Def::Other => true,
            Def::Construction => false,
            Def::Param(function, index)
                if assumed
                    .get(function)
                    .is_some_and(|params| params.contains(index)) =>
            {
                false
            }
            Def::Param(function, index) => {
                shape.open.contains(function)
                    || borrowed.contains(&(*function, *index))
                    || shape.calls.get(function).is_none_or(|calls| {
                        calls.iter().any(|call| {
                            let handed = match module.node(*call) {
                                Some(Node::ApplyFun { args, .. }) => args.get(*index),
                                _ => None,
                            };
                            may_hold_a_row(handed) && !handed_dead(module, shape, *call, *index)
                        })
                    })
            }
            Def::ContParam(continuation, index) => {
                shape
                    .entries
                    .get(continuation)
                    .into_iter()
                    .flatten()
                    .any(|entry| {
                        may_hold_a_row(entry.args.get(*index))
                            && !arg_dead(
                                shape,
                                &entry.args[*index],
                                &entry.args,
                                Block::Cont(*continuation),
                            )
                    })
                    || shape
                        .resumes
                        .get(continuation)
                        .into_iter()
                        .flatten()
                        .any(|node| {
                            !matches!(
                                module.node(*node),
                                Some(Node::ApplyFun {
                                    callee: Callee::Known(_),
                                    ..
                                })
                            )
                        })
            }
            // Two reads of one slot name one object where one can run after the other while the value they read names one object, and so does one read on two rounds of a loop that binds what it reads once.
            Def::Field(of) => {
                shape.returns_without_rebinding(*of, shape.binder[value])
                    || shape.fields.get(of).into_iter().flatten().any(|other| {
                        *other != *value
                            && shape.slot_of.get(other) == shape.slot_of.get(value)
                            && (shape.reaches_as_one(*of, shape.binder[value], shape.binder[other])
                                || shape.reaches_as_one(
                                    *of,
                                    shape.binder[other],
                                    shape.binder[value],
                                ))
                    })
            }
        };
        if source || twice || not_last || shape.captured.contains(value) {
            sources.insert(Key::Value(*value));
        }
    }
    for (function, nodes) in &shape.returns {
        for node in nodes {
            let opaque = match module.node(*node) {
                Some(Node::ApplyCont(_) | Node::Switch { .. }) => {
                    handed_back(module, *function, *node)
                        .into_iter()
                        .filter(|(_, atom)| matches!(atom, Atom::Fun(_)))
                        .map(|(index, _)| index)
                        .collect()
                }
                Some(Node::ApplyFun {
                    callee: Callee::Known(_),
                    ..
                }) => Vec::new(),
                _ => (0..arities.get(function).copied().unwrap_or(0)).collect(),
            };
            sources.extend(
                opaque
                    .into_iter()
                    .map(|index| Key::Returned(*function, index)),
            );
        }
    }
    sources
}

/// Whether no constant may be `value` at `node`: none reaches it, or a test of its tag around `node` rules out every constant's tag.
fn clear_of_constants(
    module: &Module,
    shape: &Shape,
    tags: &BTreeMap<ValueId, Tags>,
    value: ValueId,
    node: NodeId,
) -> bool {
    let Some(possible) = tags.get(&value) else {
        return true;
    };
    !possible.contains(&None)
        && guard(module, shape, value, node).is_some_and(|tag| !possible.contains(&Some(tag)))
}

/// What `node` hands `function`'s return, each argument with the place it is handed back at: a jump's arguments, and those of every edge of a switch that goes there, a switch's edge to the return being a return as a jump to it is.
fn handed_back(module: &Module, function: FunctionId, node: NodeId) -> Vec<(usize, &Atom)> {
    let (Some(node), Some(definition)) = (module.node(node), module.function(function)) else {
        return Vec::new();
    };
    return_edges(node, definition.return_cont)
        .into_iter()
        .flat_map(|edge| edge.args.iter().enumerate())
        .collect()
}

/// Whether a call hands its parameter `index` what nothing else holds as a row to write into: a value dead after it that no other parameter of the call is handed, constant data, a literal, or a filler, the null no one holds.
fn handed_dead(module: &Module, shape: &Shape, call: NodeId, index: usize) -> bool {
    let Some(Node::ApplyFun { args, .. }) = module.node(call) else {
        return false;
    };
    match args.get(index) {
        Some(atom @ Atom::Value(value)) => {
            shape.constant.contains(value)
                || (args.iter().filter(|other| *other == atom).count() == 1
                    && !shape.live_past(call, *value))
        }
        Some(Atom::Filler | Atom::Literal(_)) => true,
        _ => false,
    }
}

/// Whether an argument a jump or an edge hands `block` is a value dead on entry to it and handed once on the edge, constant data, a literal, or a filler.
fn arg_dead(shape: &Shape, atom: &Atom, args: &[Atom], block: Block) -> bool {
    match atom {
        Atom::Value(value) => {
            shape.constant.contains(value)
                || (args.iter().filter(|other| *other == atom).count() == 1
                    && !shape
                        .live_in
                        .get(&block)
                        .is_some_and(|live| live.contains(value)))
        }
        Atom::Filler | Atom::Literal(_) => true,
        Atom::Fun(_) => false,
    }
}

/// The slots a rebuild of `of` as the construction `atoms` writes: every slot but those holding what a read of `of` at that slot read, and, where a tag test says `of` was built with the tag the construction builds, the tag and the slots that constructor leaves unwritten.
fn written(
    module: &Module,
    shape: &Shape,
    of: ValueId,
    row: RowId,
    atoms: &[Atom],
    tag: Option<u32>,
) -> Vec<(usize, Atom)> {
    let same_constructor = matches!(
        (tag, atoms.first(), module.defined_row(row).and_then(|row| row.slots.first())),
        (Some(tag), Some(Atom::Literal(Literal::Nat(built))), Some(Slot::Tag)) if u32::try_from(built).ok() == Some(tag)
    );
    atoms
        .iter()
        .enumerate()
        .filter(|(index, atom)| {
            let read_back = match atom {
                Atom::Value(value) => shape.binder.get(value).is_some_and(|binder| {
                    matches!(module.node(*binder), Some(Node::LetIntrinsic { op: Intrinsic::RowGet(read, slot), args, .. })
                        if *read == row && slot == index && args.first() == Some(&Atom::Value(of)))
                }),
                _ => false,
            };
            let kept = same_constructor && (*index == 0 || matches!(atom, Atom::Filler));
            !read_back && !kept
        })
        .map(|(index, atom)| (index, atom.clone()))
        .collect()
}

/// The tag `value` holds at `node`, where a test of it decides it: the innermost continuation around `node` entered only by cases of switches on `value`'s tag, all of one key.
fn guard(module: &Module, shape: &Shape, value: ValueId, node: NodeId) -> Option<u32> {
    let mut current = node;
    loop {
        let (block, _) = shape.place[&current];
        let Block::Cont(continuation) = block else {
            return None;
        };
        let keys = shape.entries.get(&continuation).and_then(|entries| {
            entries
                .iter()
                .map(|entry| {
                    entry.case.and_then(|(switch, key)| {
                        tests_tag(module, shape, switch, value).then_some(key)
                    })
                })
                .collect::<Option<BTreeSet<_>>>()
        });
        if shape
            .resumes
            .get(&continuation)
            .is_none_or(|resumes| resumes.is_empty())
            && let Some(keys) = keys
            && keys.len() == 1
        {
            return keys.first().copied();
        }
        current = *shape.defined_at.get(&continuation)?;
    }
}

/// Whether a switch reads its case off `value`'s tag.
fn tests_tag(module: &Module, shape: &Shape, switch: NodeId, value: ValueId) -> bool {
    let Some(Node::Switch {
        scrutinee: Atom::Value(scrutinee),
        ..
    }) = module.node(switch)
    else {
        return false;
    };
    matches!(shape.defs.get(scrutinee), Some(Def::Field(of)) if *of == value)
        && shape.binder.get(scrutinee).is_some_and(|binder| {
            matches!(
                module.node(*binder),
                Some(Node::LetIntrinsic {
                    op: Intrinsic::RowGet(_, 0),
                    ..
                })
            )
        })
}

/// The constructor tags of the constant data that may reach a value, `None` standing for one with no literal tag.
type Tags = BTreeSet<Option<u32>>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Possible(Tags);

impl Lattice for Possible {
    fn bottom() -> Self {
        Possible(Tags::new())
    }

    fn join(&mut self, incoming: Self) {
        self.0.extend(incoming.0);
    }
}

/// What a flow through slots is followed at: a value, a function's result at one position, a row's slot under any constructor or a tuple's field, a row's slot under one constructor, and a row's slot as a construction whose tag is no literal writes it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum TagKey {
    Value(ValueId),
    Slot(Option<RowId>, usize),
    Under(RowId, u32, usize),
    Unstated(RowId, usize),
    Returned(FunctionId, usize),
}

/// The tags of the constant data that may reach each value, through the slots it is stored in, the calls and jumps that hand it on and the returns that hand it back. A slot is followed under the constructor a construction writes, so a read a test of its value's tag stands around is reached by what that constructor stores and by nothing a sibling stores in the slot: a leaf's payload is no fork's child.
fn constant_tags(module: &Module, shape: &Shape) -> BTreeMap<ValueId, Tags> {
    let arities = module.return_arities();
    let keys = shape
        .defs
        .keys()
        .map(|value| TagKey::Value(*value))
        .collect::<Vec<_>>();
    let (constant, arities) = (&shape.constant, &arities);
    let facts = Solver::solve(keys, |rules| {
        let possible = |solver: &Solver<Possible, TagKey>, atom: &Atom| match atom {
            Atom::Value(value) => solver
                .fact(&TagKey::Value(*value))
                .cloned()
                .unwrap_or(Possible::bottom()),
            _ => Possible::bottom(),
        };
        for node_id in shape.place.keys() {
            let node_id = *node_id;
            let Some(node) = module.node(node_id) else {
                continue;
            };
            match node {
                Node::LetValue { result, value, .. } => {
                    let result = *result;
                    let (row, atoms) = match value {
                        ValueExpr::Row(row, atoms) => (Some(*row), atoms.clone()),
                        ValueExpr::Tuple(atoms) => (None, atoms.clone()),
                        _ => continue,
                    };
                    if constant.contains(&result) {
                        let tag = match atoms.first() {
                            Some(Atom::Literal(Literal::Nat(tag))) if row.is_some() => {
                                u32::try_from(tag).ok()
                            }
                            _ => None,
                        };
                        rules.add(move |solver| {
                            solver.join(TagKey::Value(result), Possible(Tags::from([tag])))
                        });
                    }
                    let written = row.map(|row| (row, written_tag(module, row, &atoms)));
                    rules.add(move |solver| {
                        for (index, atom) in atoms.iter().enumerate() {
                            let stored = possible(solver, atom);
                            match written {
                                Some((row, Some(Some(tag)))) => {
                                    solver.join(TagKey::Under(row, tag, index), stored.clone());
                                }
                                Some((row, None)) => {
                                    solver.join(TagKey::Unstated(row, index), stored.clone());
                                }
                                Some((_, Some(None))) | None => {}
                            }
                            solver.join(TagKey::Slot(row, index), stored);
                        }
                    });
                }
                Node::LetIntrinsic {
                    result, op, args, ..
                } => {
                    let result = *result;
                    let slots = match (op, args.first()) {
                        (Intrinsic::RowGet(row, index), Some(Atom::Value(of))) => {
                            match slot_read(module, shape, *of, node_id) {
                                Some((row, Some(tag), index)) => vec![
                                    TagKey::Under(row, tag, index),
                                    TagKey::Unstated(row, index),
                                ],
                                _ => vec![TagKey::Slot(Some(*row), *index)],
                            }
                        }
                        (Intrinsic::RowGet(row, index), _) => {
                            vec![TagKey::Slot(Some(*row), *index)]
                        }
                        (Intrinsic::TupleGet(index), _) => vec![TagKey::Slot(None, *index)],
                        _ => continue,
                    };
                    rules.add(move |solver| {
                        for slot in &slots {
                            let read = solver.fact(slot).cloned().unwrap_or(Possible::bottom());
                            solver.join(TagKey::Value(result), read);
                        }
                    });
                }
                Node::ApplyFun {
                    callee: Callee::Known(callee),
                    args,
                    return_to,
                } => {
                    let (callee, args, return_to) = (*callee, args.clone(), *return_to);
                    rules.add(move |solver| {
                        if let Some(function) = module.function(callee) {
                            for (param, atom) in function.params.iter().zip(&args) {
                                let handed = possible(solver, atom);
                                solver.join(TagKey::Value(*param), handed);
                            }
                        }
                        if let Some(continuation) = module.continuation(return_to) {
                            for (index, param) in continuation.params.iter().enumerate() {
                                let returned = solver
                                    .fact(&TagKey::Returned(callee, index))
                                    .cloned()
                                    .unwrap_or(Possible::bottom());
                                solver.join(TagKey::Value(*param), returned);
                            }
                        }
                    });
                }
                Node::ApplyCont(edge) => {
                    let edge = edge.clone();
                    let (block, _) = shape.place[&node_id];
                    let function = shape.owner[&block];
                    let own_return = module.function(function).unwrap().return_cont;
                    rules.add(move |solver| {
                        for (index, atom) in edge.args.iter().enumerate() {
                            let handed = possible(solver, atom);
                            match edge.target == own_return {
                                true => solver.join(TagKey::Returned(function, index), handed),
                                false => {
                                    if let Some(param) = module
                                        .continuation(edge.target)
                                        .and_then(|target| target.params.get(index))
                                    {
                                        solver.join(TagKey::Value(*param), handed);
                                    }
                                }
                            }
                        }
                    });
                }
                Node::Switch { cases, default, .. } => {
                    let edges = cases
                        .values()
                        .chain(default.as_ref())
                        .cloned()
                        .collect::<Vec<_>>();
                    let (block, _) = shape.place[&node_id];
                    let function = shape.owner[&block];
                    let own_return = module.function(function).unwrap().return_cont;
                    rules.add(move |solver| {
                        for edge in &edges {
                            for (index, atom) in edge.args.iter().enumerate() {
                                let handed = possible(solver, atom);
                                match edge.target == own_return {
                                    true => solver.join(TagKey::Returned(function, index), handed),
                                    false => {
                                        if let Some(param) = module
                                            .continuation(edge.target)
                                            .and_then(|target| target.params.get(index))
                                        {
                                            solver.join(TagKey::Value(*param), handed);
                                        }
                                    }
                                }
                            }
                        }
                    });
                }
                _ => {}
            }
        }
        // A tail call hands back what its callee does.
        for (function, nodes) in &shape.returns {
            let function = *function;
            for node in nodes {
                if let Some(Node::ApplyFun {
                    callee: Callee::Known(callee),
                    ..
                }) = module.node(*node)
                {
                    let callee = *callee;
                    rules.add(move |solver| {
                        for index in 0..arities.get(&function).copied().unwrap_or(0) {
                            let returned = solver
                                .fact(&TagKey::Returned(callee, index))
                                .cloned()
                                .unwrap_or(Possible::bottom());
                            solver.join(TagKey::Returned(function, index), returned);
                        }
                    });
                }
            }
        }
    });
    facts
        .into_iter()
        .filter_map(|(key, Possible(tags))| match key {
            TagKey::Value(value) if !tags.is_empty() => Some((value, tags)),
            _ => None,
        })
        .collect()
}
