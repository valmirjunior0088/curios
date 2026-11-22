//! One continuation for each class of equal ones: continuations of one function and one lineage whose extents are equal up to the values, continuations and functions they bind, and which read the same of everything else, become one, kept at the innermost `LetCont` that encloses every member, each member a jump to it. Past what the inliner copies into each of a callee's calls (`MULTI_SITE_INLINE_LIMIT`, an extent weighed as the inliner weighs a copy), members that read different values from outside are a class too, each value they read apart a parameter of the one kept, which each member hands what it read.
//!
//! **A class is found by a shape and verified member by member.** Each continuation's extent is read once, bottom up, for its size and a hash of its shape with every identity erased, which a nested definition's shape composes into without being read again. Continuations of one function, one lineage, one size and one shape are candidates, and only candidates are written out in full — what they bind numbered in the order they bind it, what they read named — and compared. Two members of a class are of one size, so neither nests in the other and their extents are disjoint, and a class nested in a member is at most half its size: the extents written out sum to a function's size once for each halving, and the work is the function's size times its logarithm at most.
//!
//! **After the fixpoint, once.** The copies a class holds are the matrix compiler's leaves and what the inliner made of them; merged before the fixpoint, a body the inliner would copy into each of its callers is merged first and copied again. See `curios-cps`'s README.

#[cfg(test)]
mod tests;

use {
    crate::{
        Atom, Callee, Continuation, ContinuationId, Droppable, Edge, FunctionId,
        MULTI_SITE_INLINE_LIMIT, Module, Node, NodeId, ValueDef, ValueExpr, ValueId,
        forward_continuations, prune_unreachable, shrink, visit_atoms_mut,
    },
    curios_common::Lineage,
    std::{
        cmp::Reverse,
        collections::{BTreeMap, BTreeSet, HashMap, hash_map::DefaultHasher},
        hash::{Hash, Hasher},
    },
};

/// Merge every class of equal continuations, answering whether one was.
pub fn merge_equal_continuations(module: &mut Module) -> bool {
    let mut tree = Tree::of(module);
    let shapes = Shapes::of(module);

    let groups = candidates(module, &tree, &shapes);

    let mut merged = false;
    let mut gone = BTreeSet::<ContinuationId>::new();
    for members in groups {
        let mut written = members
            .into_iter()
            .filter(|candidate| !gone.contains(candidate))
            .filter_map(|candidate| Some((candidate, Written::of(module, candidate)?)))
            .collect::<Vec<_>>();
        while written.len() > 1 {
            let (representative, kept) = written.remove(0);
            // Below the line, a class reads alike; past it, what its members read apart is handed in.
            let apart = shapes
                .of_continuation(module, representative)
                .is_some_and(|shape| shape.weight > MULTI_SITE_INLINE_LIMIT);
            let mut members = Vec::new();
            let mut rest = Vec::new();
            for (id, other) in written.drain(..) {
                match correspondence(&kept.tokens, &other.tokens) {
                    Some(reads) if apart || reads.iter().all(|(own, theirs)| own == theirs) => {
                        members.push((id, other, reads));
                    }
                    _ => rest.push((id, other)),
                }
            }
            written = rest;
            if members.is_empty() {
                continue;
            }
            let Some(place) = tree.placement(representative, members.iter().map(|(id, ..)| *id))
            else {
                continue;
            };
            // What a member reads apart from the representative is handed in, in one order for every member.
            let handed = members
                .iter()
                .flat_map(|(_, _, reads)| reads.iter())
                .filter(|(own, theirs)| own != theirs)
                .map(|(own, _)| *own)
                .collect::<BTreeSet<_>>()
                .into_iter()
                .collect::<Vec<_>>();
            let body = merge(
                module,
                place,
                representative,
                &handed,
                members.iter().map(|(id, _, reads)| (*id, reads)),
            );
            // The kept body now stands where it is bound, so a class inside it is placed by where it stands; what the members defined is gone with them.
            tree.parent.insert(body, place);
            for (_, member, _) in &members {
                gone.extend(member.defined.iter().copied());
            }
            merged = true;
        }
    }
    if merged {
        forward_continuations(module);
        shrink(module);
        prune_unreachable(module);
    }
    merged
}

/// What the members of a class share before they are written out: the function whose body binds them, their lineage, and their extents' size and shape.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Candidacy {
    owner: FunctionId,
    lineage: Option<Lineage>,
    size: usize,
    shape: u64,
}

/// What a class is sought among: the continuations of one candidacy, two or more of them, the largest first, so a class inside a member another class drops is never merged into a body that goes.
fn candidates(module: &Module, tree: &Tree, shapes: &Shapes) -> Vec<Vec<ContinuationId>> {
    let mut groups = BTreeMap::<Candidacy, Vec<ContinuationId>>::new();
    for (continuation, definition) in module.live_continuations() {
        let (Some(owner), Some(shape)) = (
            tree.owner.get(&continuation),
            shapes.of_continuation(module, continuation),
        ) else {
            continue;
        };
        let candidacy = Candidacy {
            owner: *owner,
            lineage: definition.lineage,
            size: shape.size,
            shape: shape.hash,
        };
        groups.entry(candidacy).or_default().push(continuation);
    }
    let mut groups = groups
        .into_iter()
        .filter(|(_, members)| members.len() > 1)
        .collect::<Vec<_>>();
    groups.sort_by_key(|(candidacy, members)| (Reverse(candidacy.size), members[0]));
    groups.into_iter().map(|(_, members)| members).collect()
}

/// Bind one continuation holding `representative`'s parameters and body in the `LetCont` at `place`, a parameter beside them for each value of `handed`, which the body then reads in its place, and make the representative and every member a jump to it handing what each reads there, answering the body kept.
fn merge<'r>(
    module: &mut Module,
    place: NodeId,
    representative: ContinuationId,
    handed: &[ValueId],
    members: impl Iterator<Item = (ContinuationId, &'r BTreeMap<ValueId, ValueId>)>,
) -> NodeId {
    let kept = module
        .continuation(representative)
        .cloned()
        .expect("a member is live");
    let members = members.collect::<Vec<_>>();
    // A parameter states the register its values agree on, and none where they do not.
    let parameters = handed
        .iter()
        .map(|own| {
            let register = module
                .value(*own)
                .and_then(|definition| definition.register);
            let agrees = members.iter().all(|(_, reads)| {
                module
                    .value(reads[own])
                    .and_then(|definition| definition.register)
                    == register
            });
            module.add_value_def(ValueDef {
                debug_name: module
                    .value(*own)
                    .and_then(|definition| definition.debug_name.clone()),
                register: if agrees { register } else { None },
            })
        })
        .collect::<Vec<_>>();
    let renamed = handed
        .iter()
        .copied()
        .zip(parameters.iter().copied())
        .collect::<BTreeMap<_, _>>();
    if !renamed.is_empty() {
        for node in extent(module, kept.body) {
            let Some(node) = module.node_mut(node) else {
                continue;
            };
            visit_atoms_mut(node, &mut |atom| {
                if let Atom::Value(value) = atom
                    && let Some(parameter) = renamed.get(value)
                {
                    *value = *parameter;
                }
            });
            if let Node::ApplyFun {
                callee: Callee::Closure(value),
                ..
            } = node
                && let Some(parameter) = renamed.get(value)
            {
                *value = *parameter;
            }
        }
    }
    let target = module.add_continuation(Continuation {
        lineage: kept.lineage,
        debug_name: kept.debug_name.clone(),
        params: kept.params.iter().copied().chain(parameters).collect(),
        body: kept.body,
        terminates: kept.terminates,
    });
    if let Some(Node::LetCont { continuations, .. }) = module.node_mut(place) {
        continuations.push(target);
    }
    // The representative's parameters now belong to the kept body, so the representative takes fresh ones to hand on.
    let fresh = kept
        .params
        .iter()
        .map(|param| {
            let definition = module
                .value(*param)
                .cloned()
                .expect("a parameter is defined");
            module.add_value_def(definition)
        })
        .collect::<Vec<_>>();
    let jump = |module: &mut Module, params: &[ValueId], reads: &dyn Fn(ValueId) -> ValueId| {
        module.add_node(Node::ApplyCont(Edge {
            target,
            args: params
                .iter()
                .copied()
                .chain(handed.iter().map(|own| reads(*own)))
                .map(Atom::Value)
                .collect(),
        }))
    };
    let body = jump(module, &fresh, &|own| own);
    let continuation = module
        .continuation_mut(representative)
        .expect("a member is live");
    continuation.params = fresh;
    continuation.body = body;
    for (member, reads) in members {
        let params = module
            .continuation(member)
            .expect("a member is live")
            .params
            .clone();
        let body = jump(module, &params, &|own| reads[&own]);
        module
            .continuation_mut(member)
            .expect("a member is live")
            .body = body;
    }
    kept.body
}

/// The free values `representative` reads, each beside what `other` reads in its place, where the two are one token stream but for them, one value of the representative standing for one of the other's throughout.
fn correspondence(representative: &[Token], other: &[Token]) -> Option<BTreeMap<ValueId, ValueId>> {
    if representative.len() != other.len() {
        return None;
    }
    let mut reads = BTreeMap::new();
    for (own, theirs) in representative.iter().zip(other) {
        match (own, theirs) {
            (Token::FreeValue(own), Token::FreeValue(theirs)) => {
                if *reads.entry(*own).or_insert(*theirs) != *theirs {
                    return None;
                }
            }
            _ if own == theirs => {}
            _ => return None,
        }
    }
    Some(reads)
}

/// Every node of the extent `body` begins, what it nests included.
fn extent(module: &Module, body: NodeId) -> Vec<NodeId> {
    let mut found = Vec::new();
    let mut work = vec![body];
    while let Some(id) = work.pop() {
        found.push(id);
        match module.node(id) {
            Some(Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. }) => work.push(*next),
            Some(Node::LetFun { functions, body }) => {
                work.push(*body);
                work.extend(
                    functions
                        .iter()
                        .filter_map(|function| module.function(*function))
                        .map(|function| function.body),
                );
            }
            Some(Node::LetCont {
                continuations,
                body,
            }) => {
                work.push(*body);
                work.extend(
                    continuations
                        .iter()
                        .filter_map(|continuation| module.continuation(*continuation))
                        .map(|continuation| continuation.body),
                );
            }
            _ => {}
        }
    }
    found
}

/// A continuation's extent's size and the hash of its shape, every identity erased.
#[derive(Clone, Copy)]
struct Shape {
    size: usize,
    /// What the extent weighs, its nodes' [`Node::weight`]s summed, as the inliner weighs a copy.
    weight: usize,
    hash: u64,
}

/// Each node's shape, read bottom up once, `None` for a node whose extent holds what no merge takes: a rebuild, which names the value it rebuilds by the slots it keeps, a function compiled out of line or with a twin, or a continuation a field group names.
struct Shapes {
    nodes: HashMap<NodeId, Option<Shape>>,
}

impl Shapes {
    fn of(module: &Module) -> Self {
        let mut shapes = Shapes {
            nodes: HashMap::new(),
        };
        for (_, function) in module.live_functions() {
            shapes.node(module, function.body);
        }
        shapes
    }

    fn of_continuation(&self, module: &Module, continuation: ContinuationId) -> Option<Shape> {
        let definition = module.continuation(continuation)?;
        if module.field_groups().contains_key(&continuation) {
            return None;
        }
        let body = (*self.nodes.get(&definition.body)?)?;
        let mut hasher = DefaultHasher::new();
        (definition.params.len(), definition.terminates, body.hash).hash(&mut hasher);
        Some(Shape {
            size: body.size,
            weight: body.weight,
            hash: hasher.finish(),
        })
    }

    fn of_function(&self, module: &Module, function: FunctionId) -> Option<Shape> {
        let definition = module.function(function)?;
        if module.is_out_of_line(function) || module.twin(function).is_some() {
            return None;
        }
        let body = (*self.nodes.get(&definition.body)?)?;
        let mut hasher = DefaultHasher::new();
        let droppable = match &definition.droppable {
            Droppable::Never => None,
            Droppable::Given(values) => Some(values.len()),
        };
        (
            definition.params.len(),
            droppable,
            definition.terminates,
            body.hash,
        )
            .hash(&mut hasher);
        Some(Shape {
            size: body.size,
            weight: body.weight,
            hash: hasher.finish(),
        })
    }

    /// `id`'s shape, reading what it nests first.
    fn node(&mut self, module: &Module, id: NodeId) -> Option<Shape> {
        if let Some(shape) = self.nodes.get(&id) {
            return *shape;
        }
        let shape = curios_common::recurse(|| self.read(module, id));
        self.nodes.insert(id, shape);
        shape
    }

    fn read(&mut self, module: &Module, id: NodeId) -> Option<Shape> {
        let mut hasher = DefaultHasher::new();
        let mut size = 1;
        let mut weight = module.node(id).map_or(1, Node::weight);
        let mut sound = true;
        let mut child = |hasher: &mut DefaultHasher, shape: Option<Shape>| match shape {
            Some(shape) => {
                size += shape.size;
                weight += shape.weight;
                shape.hash.hash(hasher);
            }
            None => sound = false,
        };
        let atoms = |hasher: &mut DefaultHasher, atoms: &[Atom]| {
            atoms.len().hash(hasher);
            for atom in atoms {
                atom_kind(atom).hash(hasher);
            }
        };
        match module.node(id)? {
            Node::LetValue { value, next, .. } => {
                1u8.hash(&mut hasher);
                match value {
                    ValueExpr::Literal(_) => 0u8.hash(&mut hasher),
                    ValueExpr::List(items) => {
                        1u8.hash(&mut hasher);
                        atoms(&mut hasher, items);
                    }
                    ValueExpr::Tuple(items) => {
                        2u8.hash(&mut hasher);
                        atoms(&mut hasher, items);
                    }
                    ValueExpr::Row(row, items) => {
                        3u8.hash(&mut hasher);
                        row.hash(&mut hasher);
                        atoms(&mut hasher, items);
                    }
                    ValueExpr::Rebuilt { .. } => return None,
                }
                let next = self.node(module, *next);
                child(&mut hasher, next);
            }
            Node::LetIntrinsic { op, args, next, .. } => {
                2u8.hash(&mut hasher);
                op.hash(&mut hasher);
                atoms(&mut hasher, args);
                let next = self.node(module, *next);
                child(&mut hasher, next);
            }
            Node::LetFun { functions, body } => {
                3u8.hash(&mut hasher);
                functions.len().hash(&mut hasher);
                for function in functions {
                    if let Some(definition) = module.function(*function) {
                        self.node(module, definition.body);
                    }
                    let shape = self.of_function(module, *function);
                    child(&mut hasher, shape);
                }
                let body = self.node(module, *body);
                child(&mut hasher, body);
            }
            Node::LetCont {
                continuations,
                body,
            } => {
                4u8.hash(&mut hasher);
                continuations.len().hash(&mut hasher);
                for continuation in continuations {
                    if let Some(definition) = module.continuation(*continuation) {
                        self.node(module, definition.body);
                    }
                    let shape = self.of_continuation(module, *continuation);
                    child(&mut hasher, shape);
                }
                let body = self.node(module, *body);
                child(&mut hasher, body);
            }
            Node::ApplyFun { callee, args, .. } => {
                5u8.hash(&mut hasher);
                matches!(callee, Callee::Known(_)).hash(&mut hasher);
                atoms(&mut hasher, args);
            }
            Node::ApplyCont(edge) => {
                6u8.hash(&mut hasher);
                atoms(&mut hasher, &edge.args);
            }
            Node::Switch {
                scrutinee,
                cases,
                default,
            } => {
                7u8.hash(&mut hasher);
                atom_kind(scrutinee).hash(&mut hasher);
                for (key, edge) in cases {
                    key.hash(&mut hasher);
                    atoms(&mut hasher, &edge.args);
                }
                default
                    .as_ref()
                    .map(|edge| edge.args.len())
                    .hash(&mut hasher);
            }
            Node::Foreign { args, .. } => {
                8u8.hash(&mut hasher);
                atoms(&mut hasher, args);
            }
            Node::Cell { args, .. } => {
                9u8.hash(&mut hasher);
                atoms(&mut hasher, args);
            }
            Node::Box { args, .. } => {
                10u8.hash(&mut hasher);
                atoms(&mut hasher, args);
            }
            Node::Intrinsic { op, args, .. } => {
                11u8.hash(&mut hasher);
                std::mem::discriminant(op).hash(&mut hasher);
                atoms(&mut hasher, args);
            }
            Node::Halt { args, .. } => {
                12u8.hash(&mut hasher);
                atoms(&mut hasher, args);
            }
            Node::Panic(_) => 13u8.hash(&mut hasher),
            Node::Unreachable => 14u8.hash(&mut hasher),
        }
        sound.then(|| Shape {
            size,
            weight,
            hash: hasher.finish(),
        })
    }
}

/// Which kind of atom `atom` is, the shape's sole reading of it.
fn atom_kind(atom: &Atom) -> u8 {
    match atom {
        Atom::Value(_) => 0,
        Atom::Fun(_) => 1,
        Atom::Literal(_) => 2,
        Atom::Filler => 3,
    }
}

/// Where each node stands in the tree of nested definitions, and where each continuation is bound and in which function's body.
#[derive(Default)]
struct Tree {
    parent: HashMap<NodeId, NodeId>,
    home: HashMap<ContinuationId, NodeId>,
    owner: HashMap<ContinuationId, FunctionId>,
}

impl Tree {
    fn of(module: &Module) -> Self {
        let mut tree = Tree::default();
        for (function, definition) in module.live_functions() {
            let mut work = vec![definition.body];
            while let Some(id) = work.pop() {
                let mut child = |tree: &mut Tree, next: NodeId| {
                    tree.parent.insert(next, id);
                    work.push(next);
                };
                match module.node(id) {
                    Some(Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. }) => {
                        child(&mut tree, *next)
                    }
                    // A nested function's body is its own function's, walked from it.
                    Some(Node::LetFun { body, .. }) => child(&mut tree, *body),
                    Some(Node::LetCont {
                        continuations,
                        body,
                    }) => {
                        for member in continuations {
                            let Some(continuation) = module.continuation(*member) else {
                                continue;
                            };
                            tree.home.insert(*member, id);
                            tree.owner.insert(*member, function);
                            child(&mut tree, continuation.body);
                        }
                        child(&mut tree, *body);
                    }
                    _ => {}
                }
            }
        }
        tree
    }

    fn depth(&self, mut node: NodeId) -> usize {
        let mut depth = 0;
        while let Some(parent) = self.parent.get(&node) {
            node = *parent;
            depth += 1;
        }
        depth
    }

    fn common(&self, mut left: NodeId, mut right: NodeId) -> NodeId {
        let (mut left_depth, mut right_depth) = (self.depth(left), self.depth(right));
        while left_depth > right_depth {
            left = self.parent[&left];
            left_depth -= 1;
        }
        while right_depth > left_depth {
            right = self.parent[&right];
            right_depth -= 1;
        }
        while left != right {
            left = self.parent[&left];
            right = self.parent[&right];
        }
        left
    }

    /// The innermost `LetCont` binding or enclosing every member's: where the one kept stands. Every member reads the same of what it does not bind, which is in scope wherever each member is bound, so in scope at that `LetCont` too, which encloses them all within one function's body.
    fn placement(
        &self,
        representative: ContinuationId,
        members: impl Iterator<Item = ContinuationId>,
    ) -> Option<NodeId> {
        let mut place = *self.home.get(&representative)?;
        for member in members {
            place = self.common(place, *self.home.get(&member)?);
        }
        Some(place)
    }
}

/// One token of an extent written out: what it binds by the order it binds it, what it reads by its identity.
#[derive(PartialEq)]
enum Token {
    Tag(u8),
    Count(usize),
    Bound(u32),
    Register(Option<crate::Register>),
    FreeValue(ValueId),
    FreeContinuation(ContinuationId),
    FreeFunction(FunctionId),
    Literal(crate::Literal),
    Intrinsic(crate::Intrinsic),
    Call(crate::IntrinsicCall),
    Cell(crate::CellOp),
    Box(crate::BoxOp),
    Row(crate::RowId),
    Foreign(usize),
    Panic(crate::Panic),
    Droppable(Option<Vec<Option<u32>>>),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Bound {
    Value(ValueId),
    Continuation(ContinuationId),
    Function(FunctionId),
}

/// A continuation's extent written out, and the continuations it defines, itself first.
struct Written {
    tokens: Vec<Token>,
    defined: Vec<ContinuationId>,
}

impl Written {
    /// `continuation`'s extent written out, `None` where it holds what no merge takes ([`Shapes`]).
    fn of(module: &Module, continuation: ContinuationId) -> Option<Self> {
        let mut writer = Writer {
            module,
            written: Written {
                tokens: Vec::new(),
                defined: Vec::new(),
            },
            bound: HashMap::new(),
        };
        writer.bind(Bound::Continuation(continuation));
        writer.continuation(continuation)?;
        Some(writer.written)
    }
}

struct Writer<'m> {
    module: &'m Module,
    written: Written,
    bound: HashMap<Bound, u32>,
}

impl Writer<'_> {
    fn push(&mut self, token: Token) {
        self.written.tokens.push(token);
    }

    fn bind(&mut self, id: Bound) {
        let number = self.bound.len() as u32;
        self.bound.insert(id, number);
        self.push(Token::Bound(number));
    }

    fn bind_value(&mut self, value: ValueId) {
        self.bind(Bound::Value(value));
        let register = self
            .module
            .value(value)
            .and_then(|definition| definition.register);
        self.push(Token::Register(register));
    }

    fn read(&mut self, id: Bound) {
        let token = match (self.bound.get(&id), id) {
            (Some(number), _) => Token::Bound(*number),
            (None, Bound::Value(value)) => Token::FreeValue(value),
            (None, Bound::Continuation(continuation)) => Token::FreeContinuation(continuation),
            (None, Bound::Function(function)) => Token::FreeFunction(function),
        };
        self.push(token);
    }

    fn atom(&mut self, atom: &Atom) {
        match atom {
            Atom::Value(value) => {
                self.push(Token::Tag(0));
                self.read(Bound::Value(*value));
            }
            Atom::Fun(function) => {
                self.push(Token::Tag(1));
                self.read(Bound::Function(*function));
            }
            Atom::Literal(literal) => self.push(Token::Literal(literal.clone())),
            Atom::Filler => self.push(Token::Tag(3)),
        }
    }

    fn atoms(&mut self, atoms: &[Atom]) {
        self.push(Token::Count(atoms.len()));
        for atom in atoms {
            self.atom(atom);
        }
    }

    fn edge(&mut self, edge: &Edge) {
        self.read(Bound::Continuation(edge.target));
        self.atoms(&edge.args);
    }

    fn continuation(&mut self, id: ContinuationId) -> Option<()> {
        let module = self.module;
        let definition = module.continuation(id)?;
        if module.field_groups().contains_key(&id) {
            return None;
        }
        self.written.defined.push(id);
        self.push(Token::Count(definition.params.len()));
        for param in &definition.params {
            self.bind_value(*param);
        }
        self.push(Token::Tag(definition.terminates as u8));
        curios_common::recurse(|| self.node(definition.body))
    }

    fn function(&mut self, id: FunctionId) -> Option<()> {
        let module = self.module;
        let definition = module.function(id)?;
        if module.is_out_of_line(id) || module.twin(id).is_some() {
            return None;
        }
        self.push(Token::Count(definition.params.len()));
        for param in &definition.params {
            self.bind_value(*param);
        }
        self.bind(Bound::Continuation(definition.return_cont));
        let droppable = match &definition.droppable {
            Droppable::Never => None,
            Droppable::Given(values) => Some(
                values
                    .iter()
                    .map(|value| self.bound.get(&Bound::Value(*value)).copied())
                    .collect(),
            ),
        };
        self.push(Token::Droppable(droppable));
        self.push(Token::Tag(definition.terminates as u8));
        curios_common::recurse(|| self.node(definition.body))
    }

    /// The chain from `id` to its end, what it nests written where it is bound.
    fn node(&mut self, mut id: NodeId) -> Option<()> {
        let module = self.module;
        loop {
            match module.node(id)? {
                Node::LetValue {
                    result,
                    value,
                    next,
                } => {
                    self.push(Token::Tag(10));
                    match value {
                        ValueExpr::Literal(literal) => self.push(Token::Literal(literal.clone())),
                        ValueExpr::List(items) => {
                            self.push(Token::Tag(11));
                            self.atoms(items);
                        }
                        ValueExpr::Tuple(items) => {
                            self.push(Token::Tag(12));
                            self.atoms(items);
                        }
                        ValueExpr::Row(row, items) => {
                            self.push(Token::Row(*row));
                            self.atoms(items);
                        }
                        ValueExpr::Rebuilt { .. } => return None,
                    }
                    self.bind_value(*result);
                    id = *next;
                }
                Node::LetIntrinsic {
                    result,
                    op,
                    args,
                    next,
                } => {
                    self.push(Token::Intrinsic(*op));
                    self.atoms(args);
                    self.bind_value(*result);
                    id = *next;
                }
                Node::LetFun { functions, body } => {
                    self.push(Token::Tag(13));
                    self.push(Token::Count(functions.len()));
                    for function in functions {
                        self.bind(Bound::Function(*function));
                    }
                    for function in functions {
                        self.function(*function)?;
                    }
                    id = *body;
                }
                Node::LetCont {
                    continuations,
                    body,
                } => {
                    self.push(Token::Tag(14));
                    self.push(Token::Count(continuations.len()));
                    for continuation in continuations {
                        self.bind(Bound::Continuation(*continuation));
                    }
                    for continuation in continuations {
                        self.continuation(*continuation)?;
                    }
                    id = *body;
                }
                Node::ApplyFun {
                    callee,
                    args,
                    return_to,
                } => {
                    match callee {
                        Callee::Known(function) => {
                            self.push(Token::Tag(15));
                            self.read(Bound::Function(*function));
                        }
                        Callee::Closure(value) => {
                            self.push(Token::Tag(16));
                            self.read(Bound::Value(*value));
                        }
                    }
                    self.atoms(args);
                    self.read(Bound::Continuation(*return_to));
                    return Some(());
                }
                Node::ApplyCont(edge) => {
                    self.push(Token::Tag(17));
                    self.edge(edge);
                    return Some(());
                }
                Node::Switch {
                    scrutinee,
                    cases,
                    default,
                } => {
                    self.push(Token::Tag(18));
                    self.atom(scrutinee);
                    self.push(Token::Count(cases.len()));
                    for (key, edge) in cases {
                        self.push(Token::Count(*key as usize));
                        self.edge(edge);
                    }
                    match default {
                        Some(edge) => {
                            self.push(Token::Tag(19));
                            self.edge(edge);
                        }
                        None => self.push(Token::Tag(20)),
                    }
                    return Some(());
                }
                Node::Foreign {
                    function,
                    args,
                    return_to,
                } => {
                    self.push(Token::Foreign(std::sync::Arc::as_ptr(function) as usize));
                    self.atoms(args);
                    self.read(Bound::Continuation(*return_to));
                    return Some(());
                }
                Node::Cell {
                    op,
                    args,
                    return_to,
                } => {
                    self.push(Token::Cell(*op));
                    self.atoms(args);
                    self.read(Bound::Continuation(*return_to));
                    return Some(());
                }
                Node::Box {
                    op,
                    args,
                    return_to,
                } => {
                    self.push(Token::Box(*op));
                    self.atoms(args);
                    self.read(Bound::Continuation(*return_to));
                    return Some(());
                }
                Node::Intrinsic {
                    op,
                    args,
                    return_to,
                } => {
                    self.push(Token::Call(*op));
                    self.atoms(args);
                    self.read(Bound::Continuation(*return_to));
                    return Some(());
                }
                Node::Halt { function, args } => {
                    self.push(Token::Tag(21));
                    self.push(Token::Foreign(std::sync::Arc::as_ptr(function) as usize));
                    self.atoms(args);
                    return Some(());
                }
                Node::Panic(panic) => {
                    self.push(Token::Panic(*panic));
                    return Some(());
                }
                Node::Unreachable => {
                    self.push(Token::Tag(22));
                    return Some(());
                }
            }
        }
    }
}
