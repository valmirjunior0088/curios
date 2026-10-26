//! Continuation scalar replacement: a tuple that travels a join point as one aggregate parameter becomes that many field parameters, and the record in [`FieldGroup`](crate::FieldGroup) is what makes the change a fact of the program.
//!
//! Admission composes two halves (see `documentation/design/compilation/a-value-costs-when-it-is-kept-not-when-it-is-named.md`): the backward half says every use of the parameter is a projection or an eligible transfer (`demands`, `Projected`), and the forward half says every flow reaching it is a construction or an alias of one (`origins`, `Constructed`). Loop backedges are the central case rather than an exclusion — an edge carrying the join's own parameter reads as the constructions that entered it, which is precisely what the forward fixpoint establishes.
//!
//! **A flow of several widths is the same rewrite at the widest.** Where the structural tuples reaching a parameter disagree about arity, the region travels as the widest, and each narrower edge carries its own fields followed by filler; a family's row is one width already, since the door pads every construction to it. The per-edge width is the same forward fact read at that edge's argument, so a projection is never emitted past what a construction carries.
//!
//! What makes the filler safe is that it *inhabits* the slot with what the field would hold, not that nothing reads it. Unreadness would not justify it: the value is passed, and an edge into a raw-carried parameter coerces every argument to that carrier before the discriminant is consulted, so a zero at a guessed carrier traps on the `ref.cast`. The ANF door pads a register slot with its zero literal and a reference slot with [`Atom::Filler`], which every reference position admits as null; the one carrier not known here — a parameter the analysis later raises into a register — is the emitter's, and it materialises the filler as that register's zero.
//!
//! The rewrite is three local edits, and the existing chain finishes the job, exactly as `split_returns` works: the parameter list is spliced and the group recorded; the continuation's head rebuilds the aggregate from the new field parameters and every occurrence of the old parameter is redirected to that rebuild; and every incoming edge projects its argument into fields above the jump. Projection forwarding then collapses the inserted reads through the constructions they see, dead-binding elimination removes the constructions nothing reads any more — and the head rebuild survives exactly where a whole-value use survives, which makes it the materialization boundary the cost contract prescribes rather than a leak. For a flow of several widths that materialization is *wider* than the construction that travelled, which is the one place the rewrite spends rather than saves: a surviving whole-value use of a narrow construction gets its widest sibling's object. It is bounded by the same demand condition that admits the region at all — every use a projection or an eligible transfer — so the rebuild survives only where a boundary the region cannot cross keeps it.
//!
//! Resume continuations are excluded: their parameter list is the call interface the return protocol owns. Splitting inside an already-recorded group is declined — one aggregate is exposed one level per round at fresh joins, and the growth ceiling is what stops recursive structures from flattening without end.

#[cfg(test)]
mod tests;

use {
    crate::{
        Atom, CallAnalysis, Callee, ContinuationId, Demand, Edge, FunctionId, Intrinsic, Module,
        Node, NodeId, Origin, PARAM_SPLIT_GROWTH_LIMIT, RowId, UseTarget, ValueExpr, ValueId,
        analyze_calls, atoms, demand_of, demands, origins, rewire_node, visit_atoms_mut,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// One admitted split: which parameter of which continuation, and the width every flow travels at — the widest construction reaching it, which is the region's own arity where they agree and the widest tuple's where they do not.
struct Split {
    continuation: ContinuationId,
    position: usize,
    param: ValueId,
    width: usize,
    /// The row, where every flow into the parameter is a variant construction of one. Carried from the same origin fact that gave the width, so the head rebuild and the per-edge projections stay in the vocabulary the reads below them use — a variant rebuilt as a structural tuple would trap at the next exact cast.
    row: Option<RowId>,
}

/// Whether a site may take `source` apart into fields.
///
/// A source the fixpoint reports at several widths is a merged tuple flow whose width is undecided *there*, so no fixed number of projections is safe: the widest reads past a narrower construction and traps, and the narrowest drops fields the wider one carries. Such a source is a region of its own, and a round that splits it first turns it into a materialization of one settled width — which is why this is a decline rather than a disqualification. The region's own parameter is the exception it looks like: by the time the edges are rewritten it names the head rebuild, whose width is the region's by construction.
fn takeable(origins: &BTreeMap<ValueId, Origin>, param: ValueId, source: &Atom) -> bool {
    match source {
        Atom::Value(value) => *value == param || origins.get(value).is_none_or(Origin::is_settled),
        Atom::Fun(_) | Atom::Literal(_) | Atom::Filler => false,
    }
}

/// The construction a rebuild emits: the row's own, where one was carried, and a structural tuple otherwise.
fn rebuild_of(row: Option<RowId>, fields: &[ValueId]) -> ValueExpr {
    let atoms = fields.iter().copied().map(Atom::Value).collect();
    match row {
        Some(row) => ValueExpr::Row(row, atoms),
        None => ValueExpr::Tuple(atoms),
    }
}

/// The projection a split emits, in the vocabulary its source was built in.
fn projection_of(row: Option<RowId>, index: usize) -> Intrinsic {
    match row {
        Some(row) => Intrinsic::RowGet(row, index),
        None => Intrinsic::TupleGet(index),
    }
}

/// Every admissible split, in deterministic order: continuations by identity, positions ascending within each.
fn admit(module: &Module, origins: &BTreeMap<ValueId, Origin>) -> Vec<Split> {
    let demands = demands(module);

    // A resume's parameters are the call interface the return protocol owns, whatever their demand says.
    let resumes = resume_targets(module);

    // Every edge into each continuation, so the per-position source check below is one pass rather than one per candidate parameter.
    let mut incoming = BTreeMap::<ContinuationId, Vec<&Edge>>::new();
    for (_, node) in module.live_nodes() {
        for edge in edges_of(node) {
            incoming.entry(edge.target).or_default().push(edge);
        }
    }

    let mut admitted = Vec::new();
    for (continuation, definition) in module.live_continuations() {
        if resumes.contains(&continuation) {
            continue;
        }
        for (position, &param) in definition.params.iter().enumerate() {
            if grouped(module, continuation, position) {
                continue;
            }
            let Some(width) = origins.get(&param).and_then(Origin::width) else {
                continue;
            };
            let Demand::Projected(read) = demand_of(&demands, param) else {
                continue;
            };
            if width == 0 || read.last().is_some_and(|&last| last >= width) {
                continue;
            }
            if definition.params.len() - 1 + width > PARAM_SPLIT_GROWTH_LIMIT {
                continue;
            }
            if !incoming
                .get(&continuation)
                .map_or(&[][..], Vec::as_slice)
                .iter()
                .all(|edge| {
                    edge.args
                        .get(position)
                        .is_some_and(|atom| takeable(origins, param, atom))
                })
            {
                continue;
            }
            admitted.push(Split {
                continuation,
                position,
                param,
                width,
                row: origins.get(&param).and_then(Origin::row),
            });
        }
    }
    admitted
}

/// Whether `split`, admitted against the snapshot `origins`, still holds on the live module after the splits applied before it in the same sweep.
///
/// The sweep reuses one snapshot for every split, which is `inline_known_calls`' shape, and what keeps that honest is that a fresh value *declines* here where the rewrite below could *assume*. An earlier split's head rebuild is absent from the snapshot. Outside a sweep that absence means a value the fixpoint never reached, and the rewrite reads the region's full width for it; in a sweep the rebuild may be standing on an edge into this very candidate — the earlier split's parameter was redirected to it everywhere — at its own region's width rather than this one's, and projecting it at this width would read past a narrower construction. So every incoming argument must be this parameter or a value the snapshot holds a fact for, and whatever an earlier split put there waits for the next round's facts. That wait is the one genuine dependency between two splits, and the only one that costs a round.
///
/// The live parameter list is re-read for the same reason: an earlier split of the same continuation at a higher position has already grown it, and the ceiling is a fact of the list as it stands.
fn still_admissible(
    module: &Module,
    origins: &BTreeMap<ValueId, Origin>,
    carriers: &[NodeId],
    split: &Split,
) -> bool {
    let Some(definition) = module.continuation(split.continuation) else {
        return false;
    };
    if definition.params.get(split.position) != Some(&split.param)
        || grouped(module, split.continuation, split.position)
        || definition.params.len() - 1 + split.width > PARAM_SPLIT_GROWTH_LIMIT
    {
        return false;
    }
    carriers
        .iter()
        .filter_map(|&carrier| module.node(carrier))
        .flat_map(edges_of)
        .filter(|edge| edge.target == split.continuation)
        .all(|edge| match edge.args.get(split.position) {
            Some(Atom::Value(value)) => {
                *value == split.param || origins.get(value).is_some_and(Origin::is_settled)
            }
            _ => false,
        })
}

/// Split every admissible continuation parameter into its fields against one forward snapshot, record each group, and leave the cleanup to the chain.
///
/// One split per call would leave region-wide convergence to the optimizer's own fixpoint at one round of every pass per split — this pass firing on nearly every round of a `Toml/decode` compile, with most of the fixpoint's time going to passes that rewrite nothing on any of them, as `fixpoint_pass_measurements` shows. A sweep applies what one snapshot admits and [`still_admissible`] declines what an earlier split in the sweep has since touched, so the rounds that remain are the ones a split genuinely depends on. Determinism is the order below; termination is unchanged, since every split still consumes an unrecorded aggregate parameter under the growth ceiling.
pub(crate) fn split_parameters(module: &mut Module) -> bool {
    let origins = origins(module);
    let mut admitted = admit(module, &origins);
    // Highest position first within a continuation, so an earlier position's index survives a later one's splice — the order `split_windows` applies its positions in, for the same reason.
    admitted.sort_by_key(|split| (split.continuation, std::cmp::Reverse(split.position)));

    // The nodes carrying an edge into each continuation, indexed once for the sweep. A split repoints a carrier's predecessors at its projection chain and sets the carrier in place, so the index holds across the sweep — and it is what keeps a sweep of a hundred splits from walking the module a hundred times to find the few nodes each one rewrites.
    let mut carriers = BTreeMap::<ContinuationId, Vec<NodeId>>::new();
    for (id, node) in module.live_nodes() {
        for edge in edges_of(node) {
            let entry = carriers.entry(edge.target).or_default();
            if entry.last() != Some(&id) {
                entry.push(id);
            }
        }
    }

    let mut changed = false;
    for split in admitted {
        let carriers = carriers
            .get(&split.continuation)
            .map_or(&[][..], Vec::as_slice);
        if !still_admissible(module, &origins, carriers, &split) {
            continue;
        }
        apply_split(module, &origins, carriers, &split);
        changed = true;
    }
    changed
}

/// The three local edits of one split: splice the parameter list and record the group, rebuild the aggregate at the head and redirect the old parameter to it, and project every incoming edge's argument into fields above the jump — `carriers` being the nodes that hold those edges.
fn apply_split(
    module: &mut Module,
    origins: &BTreeMap<ValueId, Origin>,
    carriers: &[NodeId],
    split: &Split,
) {
    // The field parameters, and the group that records them.
    let fields = (0..split.width)
        .map(|index| {
            module.add_value(Some(format!(
                "field/{}/{index}",
                split.continuation.index()
            )))
        })
        .collect::<Vec<_>>();
    let definition = module
        .continuation_mut(split.continuation)
        .expect("admitted continuation is live");
    definition
        .params
        .splice(split.position..=split.position, fields.iter().copied());
    let body = definition.body;
    module.record_split(split.continuation, split.position, split.width);

    // The head rebuild: the aggregate reconstructed from its fields, standing in for the old parameter everywhere. It survives exactly where a whole-value use survives, and is the materialization the cost contract allows at such a boundary.
    let rebuilt = module.add_value(Some(format!("rebuilt/{}", split.continuation.index())));
    let head = module.add_node(Node::LetValue {
        result: rebuilt,
        value: rebuild_of(split.row, &fields),
        next: body,
    });
    module
        .continuation_mut(split.continuation)
        .expect("admitted continuation is live")
        .body = head;
    module.replace_atom(UseTarget::Value(split.param), Atom::Value(rebuilt));
    module.remove_value(split.param);

    // Every incoming edge projects its argument into fields above the jump; forwarding collapses the reads through visible constructions on the next rounds.
    for &carrier in carriers {
        let mut node = module.node(carrier).expect("carrier is live").clone();
        let mut chain = Vec::new();
        for edge in edges_of_mut(&mut node) {
            if edge.target != split.continuation {
                continue;
            }
            let Atom::Value(source) = &edge.args[split.position] else {
                unreachable!("a construction origin admits only value arguments");
            };
            let source = *source;
            // How wide *this* edge's argument is: the one width its own flows settle on, admission having refused any edge whose source has no such width. The head rebuild minted above is absent from the pre-rewrite facts and is the region's full width by construction; so is a value the fixpoint never reached, which only unreachable code can hold — and nothing else is, since [`still_admissible`] declines a sweep-mate's rebuild rather than letting it fall through to here.
            let carried = origins
                .get(&source)
                .and_then(Origin::settled_width)
                .unwrap_or(split.width);
            let mut replacement = Vec::with_capacity(split.width);
            for index in 0..carried {
                let projection =
                    module.add_value(Some(format!("field/{}/{index}", carrier.index())));
                chain.push((projection, index, source));
                replacement.push(Atom::Value(projection));
            }
            replacement.extend((carried..split.width).map(|index| module.pad(split.row, index)));
            edge.args
                .splice(split.position..=split.position, replacement);
        }
        let ids = chain
            .iter()
            .map(|_| module.reserve_node())
            .collect::<Vec<_>>();
        if let Some(&first) = ids.first() {
            rewire_node(module, carrier, first);
        }
        for (offset, (projection, index, source)) in chain.into_iter().enumerate() {
            let next = ids.get(offset + 1).copied().unwrap_or(carrier);
            module.define_node(
                ids[offset],
                Node::LetIntrinsic {
                    result: projection,
                    op: projection_of(split.row, index),
                    args: vec![Atom::Value(source)],
                    next,
                },
            );
        }
        module.set_node(carrier, node);
    }
}

// -- known-function workers --------------------------------------------------
//
// The same rewrite one boundary over. A join parameter's split stops at a known call, because the callee takes the aggregate whole and the head rebuild has to materialize it — which is exactly where the UTF-8 scan's last per-character construction lives once the loop carries it as fields. Splitting the *callee's* parameter removes that boundary, and the census is what admits it: the known-call flow class is non-empty and names the scan.
//
// **There is no wrapper, because there are no unknown callers.** A worker/wrapper pair exists to keep a retained ABI for a caller the rewrite cannot see; an escaping function is refused outright here instead, so every call site is a `Known` one the rewrite reaches. That is a narrower mechanism than the precedent's.
//
// **The width-two floor is a termination property, not tidiness** — the same one `split_returns` records. A continuation's split is stopped from repeating by the recorded `FieldGroup`; a function carries no such record, so what bounds it is growth: every admitted split adds at least one parameter and `PARAM_SPLIT_GROWTH_LIMIT` caps the total. A width-one split would add none and could be re-admitted forever.

/// One admitted worker split: which parameter of which known function, and the width its flows travel at.
struct Worker {
    function: FunctionId,
    position: usize,
    param: ValueId,
    width: usize,
    /// See [`Split::row`].
    row: Option<RowId>,
}

/// Every admissible worker split in function/position order, against one shared forward and backward snapshot.
fn admit_workers(
    module: &Module,
    origins: &BTreeMap<ValueId, Origin>,
    demands: &BTreeMap<ValueId, Demand>,
    calls: &CallAnalysis,
) -> Vec<Worker> {
    let mut workers = Vec::new();
    for (function, definition) in module.live_functions() {
        if calls.escaping.contains(&function) || module.entry() == Some(function) {
            continue;
        }
        for (position, &param) in definition.params.iter().enumerate() {
            let Some(width) = origins.get(&param).and_then(Origin::width) else {
                continue;
            };
            let Demand::Projected(read) = demand_of(demands, param) else {
                continue;
            };
            if width < 2
                || read.last().is_some_and(|&last| last >= width)
                || definition.params.len() - 1 + width > PARAM_SPLIT_GROWTH_LIMIT
            {
                continue;
            }
            if !calls.call_sites[&function].iter().all(|site| {
                matches!(module.node(*site), Some(Node::ApplyFun { args, .. }) if args.get(position).is_some_and(|atom| takeable(origins, param, atom)))
            }) {
                continue;
            }
            workers.push(Worker {
                function,
                position,
                param,
                width,
                row: origins.get(&param).and_then(Origin::row),
            });
        }
    }
    workers
}

/// Compatible worker edits accumulated before a single substitution and predecessor-redirection walk.
#[derive(Default)]
struct WorkerEdits {
    substitutions: BTreeMap<ValueId, Atom>,
    projections: BTreeMap<NodeId, Vec<Node>>,
}

impl WorkerEdits {
    fn apply(
        &mut self,
        module: &mut Module,
        origins: &BTreeMap<ValueId, Origin>,
        callers: &[NodeId],
        worker: &Worker,
    ) {
        let fields = (0..worker.width)
            .map(|index| {
                module.add_value(Some(format!("worker/{}/{index}", worker.function.index())))
            })
            .collect::<Vec<_>>();
        let definition = module.function_mut(worker.function).unwrap();
        definition
            .params
            .splice(worker.position..=worker.position, fields.iter().copied());
        let body = definition.body;
        let rebuilt = module.add_value(Some(format!("rebuilt/{}", worker.function.index())));
        let head = module.add_node(Node::LetValue {
            result: rebuilt,
            value: rebuild_of(worker.row, &fields),
            next: body,
        });
        module.function_mut(worker.function).unwrap().body = head;
        self.substitutions
            .insert(worker.param, Atom::Value(rebuilt));
        for &caller in callers {
            let Node::ApplyFun { args, .. } = module.node(caller).unwrap() else {
                unreachable!()
            };
            let Atom::Value(source) = args[worker.position] else {
                unreachable!()
            };
            let carried = origins
                .get(&source)
                .and_then(Origin::settled_width)
                .unwrap_or(worker.width);
            let mut replacement = Vec::with_capacity(worker.width);
            let projections = self.projections.entry(caller).or_default();
            for index in 0..carried {
                let projection =
                    module.add_value(Some(format!("worker/{}/{index}", caller.index())));
                projections.push(Node::LetIntrinsic {
                    result: projection,
                    op: projection_of(worker.row, index),
                    args: vec![Atom::Value(source)],
                    next: caller,
                });
                replacement.push(Atom::Value(projection));
            }
            replacement.extend((carried..worker.width).map(|index| module.pad(worker.row, index)));
            let Node::ApplyFun { args, .. } = module.node_mut(caller).unwrap() else {
                unreachable!()
            };
            args.splice(worker.position..=worker.position, replacement);
        }
    }

    fn finish(self, module: &mut Module) {
        // Redirect only the preexisting chains: a fresh projection chain's tail deliberately retains its original carrier.
        let boundary = module.nodes().len();
        let mut redirects = BTreeMap::new();
        for (carrier, nodes) in self.projections {
            let ids = nodes
                .iter()
                .map(|_| module.reserve_node())
                .collect::<Vec<_>>();
            if let Some(&head) = ids.first() {
                redirects.insert(carrier, head);
            }
            for (index, mut node) in nodes.into_iter().enumerate() {
                let Node::LetIntrinsic { next, .. } = &mut node else {
                    unreachable!()
                };
                *next = ids.get(index + 1).copied().unwrap_or(carrier);
                module.define_node(ids[index], node);
            }
        }
        let redirect = |id: &mut NodeId| {
            if let Some(&head) = redirects.get(id) {
                *id = head;
            }
        };
        for (_, function) in module.live_functions_mut() {
            redirect(&mut function.body);
        }
        for (_, continuation) in module.live_continuations_mut() {
            redirect(&mut continuation.body);
        }
        for (id, node) in module.live_nodes_mut() {
            if id.index() < boundary {
                match node {
                    Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. } => redirect(next),
                    Node::LetFun { body, .. } | Node::LetCont { body, .. } => redirect(body),
                    _ => {}
                }
            }
            visit_atoms_mut(node, &mut |atom| {
                if let Atom::Value(value) = atom
                    && let Some(replacement) = self.substitutions.get(value)
                {
                    *atom = replacement.clone();
                }
            });
        }
        for param in self.substitutions.keys() {
            module.remove_value(*param);
        }
    }
}

/// Split every independent known-function parameter admitted by one snapshot. Earlier positions survive descending splices; a newly rebuilt argument waits for fresh origins. Caller projections and parameter substitutions commit together, so a sweep scans the module once to redirect them.
pub(crate) fn split_workers(module: &mut Module) -> bool {
    let origins = origins(module);
    let demands = demands(module);
    let calls = analyze_calls(module);
    let mut workers = admit_workers(module, &origins, &demands, &calls);
    workers.sort_by_key(|worker| (worker.function, std::cmp::Reverse(worker.position)));
    let mut edits = WorkerEdits::default();
    let mut changed = false;
    for worker in workers {
        let definition = module.function(worker.function).unwrap();
        if definition.params.get(worker.position) != Some(&worker.param)
            || definition.params.len() - 1 + worker.width > PARAM_SPLIT_GROWTH_LIMIT
        {
            continue;
        }
        let callers = &calls.call_sites[&worker.function];
        if !callers.iter().all(|site| {
            let Some(Node::ApplyFun {
                callee: Callee::Known(target),
                args,
                ..
            }) = module.node(*site)
            else {
                return false;
            };
            if *target != worker.function {
                return false;
            }
            let Some(Atom::Value(source)) = args.get(worker.position) else {
                return false;
            };
            (*source == worker.param
                || !edits.substitutions.contains_key(source) && origins.contains_key(source))
                && takeable(&origins, worker.param, &Atom::Value(*source))
        }) {
            continue;
        }
        edits.apply(module, &origins, callers, &worker);
        changed = true;
    }
    if changed {
        edits.finish(module);
    }
    changed
}

// -- prepared windows --------------------------------------------------------
//
// The rope analogue of the product split, sharing the record rather than growing a second coordination discipline. A window is `(base, end, length)`, the last `length` elements before `end`: any rope is its own whole window as `(r, len r, len r)`, a virtual slice is monus arithmetic behind a `WindowExtent` guard that keeps the eager bounds trap at the original evaluation point, and a read reaches the base directly at `end ∸ (length ∸ index)`. Counting down to an end rather than up from an offset is what bounds a window: every field is a length or what a monus of one leaves, so the representation analysis puts each below `2³²` and holds it in a word read unsigned, where an offset grown by a sum round a loop is bounded by nothing it can see. A read — the read helper already forces and memoizes an uncached node on first contact, so deferring the physical slice's force changes no observable value. Unlike products there is no cheap head rebuild, so the rewrite is region-atomic: a region is admitted only when every use of every member — the parameters and the slice results flowing between them — is a length, a read, a further slice, or a transfer into the region, and a region with any other use is declined whole, the conservative limit the value-lifetime decision states.

/// Which sequence row a window region reads through, unified across every operation the region contains.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WindowFamily {
    Bytes,
    List,
}

impl WindowFamily {
    fn of(op: Intrinsic) -> Option<(Self, WindowRead)> {
        match op {
            Intrinsic::BytesLen => Some((Self::Bytes, WindowRead::Len)),
            Intrinsic::BytesGet => Some((Self::Bytes, WindowRead::Get)),
            Intrinsic::BytesSlice => Some((Self::Bytes, WindowRead::Slice)),
            Intrinsic::BytesRest => Some((Self::Bytes, WindowRead::Rest)),
            Intrinsic::ListLen => Some((Self::List, WindowRead::Len)),
            Intrinsic::ListGet => Some((Self::List, WindowRead::Get)),
            Intrinsic::ListSlice => Some((Self::List, WindowRead::Slice)),
            Intrinsic::ListRest => Some((Self::List, WindowRead::Rest)),
            _ => None,
        }
    }

    fn len_op(self) -> Intrinsic {
        match self {
            Self::Bytes => Intrinsic::BytesLen,
            Self::List => Intrinsic::ListLen,
        }
    }

    fn get_op(self) -> Intrinsic {
        match self {
            Self::Bytes => Intrinsic::BytesGet,
            Self::List => Intrinsic::ListGet,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum WindowRead {
    Len,
    Get,
    Slice,
    /// A suffix: a slice whose extent the value decides. Distinguished from `Slice` only so the rewrite knows to derive that extent from the member's own `length` field, which is the same fact the emitted rope would have read off itself.
    Rest,
}

/// One occurrence of a value the window walk classifies.
#[derive(Debug, Clone, Copy)]
enum WindowUse {
    Len(NodeId),
    Get(NodeId),
    Slice(NodeId),
    Transfer(ContinuationId, usize),
    Hostile,
}

/// Every value's window-relevant occurrences, in one pass over the module.
fn window_uses(module: &Module) -> BTreeMap<ValueId, Vec<WindowUse>> {
    let mut uses = BTreeMap::<ValueId, Vec<WindowUse>>::new();
    let mut record = |value: ValueId, this: WindowUse| uses.entry(value).or_default().push(this);
    for (id, node) in module.live_nodes() {
        match node {
            Node::LetIntrinsic { op, args, .. } => {
                for (position, atom) in args.iter().enumerate() {
                    let Atom::Value(value) = atom else {
                        continue;
                    };
                    let this = match (WindowFamily::of(*op), position) {
                        (Some((_, WindowRead::Len)), 0) => WindowUse::Len(id),
                        (Some((_, WindowRead::Get)), 0) => WindowUse::Get(id),
                        (Some((_, WindowRead::Slice | WindowRead::Rest)), 0) => {
                            WindowUse::Slice(id)
                        }
                        _ => WindowUse::Hostile,
                    };
                    record(*value, this);
                }
            }
            Node::ApplyCont(_) | Node::Switch { .. } => {
                if let Node::Switch { scrutinee, .. } = node
                    && let Atom::Value(value) = scrutinee
                {
                    record(*value, WindowUse::Hostile);
                }
                for edge in edges_of(node) {
                    let defined = module.continuation(edge.target).is_some();
                    for (position, atom) in edge.args.iter().enumerate() {
                        if let Atom::Value(value) = atom {
                            let this = if defined {
                                WindowUse::Transfer(edge.target, position)
                            } else {
                                WindowUse::Hostile
                            };
                            record(*value, this);
                        }
                    }
                }
            }
            _ => {
                for atom in atoms(node) {
                    if let Atom::Value(value) = atom {
                        record(*value, WindowUse::Hostile);
                    }
                }
                if let Node::ApplyFun {
                    callee: Callee::Closure(value),
                    ..
                } = node
                {
                    record(*value, WindowUse::Hostile);
                }
            }
        }
    }
    uses
}

/// One admitted window region: the continuation parameters it spans, the slice nodes it consumes, and the row every read agrees on.
struct WindowRegion {
    row: WindowFamily,
    params: Vec<(ContinuationId, usize, ValueId)>,
    slices: Vec<NodeId>,
    members: BTreeSet<ValueId>,
}

/// Grow a region from `seed` or refuse it: every member's every use must be a window read or a transfer into a splittable parameter, and the region must consume at least one slice — a rope that is only read is not paying the cost this rewrite removes.
fn grow_window_region(
    module: &Module,
    uses: &BTreeMap<ValueId, Vec<WindowUse>>,
    resumes: &BTreeSet<ContinuationId>,
    seed: (ContinuationId, usize, ValueId),
) -> Option<WindowRegion> {
    let mut row = None;
    let mut params = vec![seed];
    let mut slices = Vec::new();
    let mut members = BTreeSet::from([seed.2]);
    let mut work = vec![seed.2];

    let unify = |row: &mut Option<WindowFamily>, op: Intrinsic| {
        let (this, _) = WindowFamily::of(op)?;
        match row {
            None => {
                *row = Some(this);
                Some(())
            }
            Some(existing) => (*existing == this).then_some(()),
        }
    };

    while let Some(value) = work.pop() {
        for this in uses.get(&value).map_or(&[][..], Vec::as_slice) {
            match this {
                WindowUse::Len(node) | WindowUse::Get(node) => {
                    let Node::LetIntrinsic { op, .. } = module.node(*node)? else {
                        return None;
                    };
                    unify(&mut row, *op)?;
                }
                WindowUse::Slice(node) => {
                    let Node::LetIntrinsic { op, result, .. } = module.node(*node)? else {
                        return None;
                    };
                    unify(&mut row, *op)?;
                    slices.push(*node);
                    if members.insert(*result) {
                        work.push(*result);
                    }
                }
                WindowUse::Transfer(target, position) => {
                    if resumes.contains(target) {
                        return None;
                    }
                    let definition = module.continuation(*target)?;
                    if grouped(module, *target, *position)
                        || definition.params.len() - 1 + 3 > PARAM_SPLIT_GROWTH_LIMIT
                    {
                        return None;
                    }
                    let param = *definition.params.get(*position)?;
                    if members.insert(param) {
                        params.push((*target, *position, param));
                        work.push(param);
                    }
                }
                WindowUse::Hostile => return None,
            }
        }
    }

    if slices.is_empty() {
        return None;
    }
    // The ceiling is a fact of each continuation's whole list after the split, so a region spanning two of one continuation's parameters is measured with both splits applied: the per-parameter check above admitted each one alone, and let a pair land two past the limit.
    let mut widened = BTreeMap::<ContinuationId, usize>::new();
    for &(continuation, ..) in &params {
        *widened.entry(continuation).or_default() += 2;
    }
    if widened.iter().any(|(continuation, growth)| {
        module.continuation(*continuation).unwrap().params.len() + growth > PARAM_SPLIT_GROWTH_LIMIT
    }) {
        return None;
    }
    Some(WindowRegion {
        row: row?,
        params,
        slices,
        members,
    })
}

/// Whether `position` of `continuation` lies inside a recorded field group.
fn grouped(module: &Module, continuation: ContinuationId, position: usize) -> bool {
    module
        .field_groups()
        .get(&continuation)
        .is_some_and(|groups| {
            groups
                .iter()
                .any(|group| position >= group.start && position < group.start + group.width)
        })
}

/// Insert `nodes` above `carrier`, in order, and return nothing: references to the carrier are repointed at the head of the chain before any link is defined, so the chain's own tail reference survives.
fn insert_above(module: &mut Module, carrier: NodeId, nodes: Vec<Node>) {
    if nodes.is_empty() {
        return;
    }
    let ids: Vec<NodeId> = nodes.iter().map(|_| module.reserve_node()).collect();
    rewire_node(module, carrier, ids[0]);
    for (index, node) in nodes.into_iter().enumerate() {
        let mut node = node;
        if let Node::LetIntrinsic { next, .. } | Node::LetValue { next, .. } = &mut node {
            *next = ids.get(index + 1).copied().unwrap_or(carrier);
        }
        module.define_node(ids[index], node);
    }
}

/// Virtualize every window region the module admits, the widest first, each as [`split_widest_window`] virtualizes one.
///
/// One region a call would leave the rest to the optimizer's own fixpoint, at a round of every pass for each region and another for the fold its split exposes, which is what [`split_parameters`] sweeps to avoid. A split records its groups and rewrites its slices, so the uses and the regions are read again from the module as each split leaves it: the widest region is then the widest of what remains, and a region a split has reached into is declined as it is from one round to the next. Termination is each split's own: it records a group over a parameter that had none, under the growth ceiling.
pub(crate) fn split_windows(module: &mut Module) -> bool {
    let mut split = false;
    while split_widest_window(module) {
        split = true;
    }
    split
}

/// Virtualize one whole window region: every parameter becomes `(base, end, length)` under a recorded group, every slice becomes a guarded extent and the end it leaves, every read reaches the base directly, and every entry edge opens its rope as a whole window. The physical views and the helper calls that built them are simply never emitted — nothing here deletes them, they are unread.
fn split_widest_window(module: &mut Module) -> bool {
    let uses = window_uses(module);
    let resumes = resume_targets(module);

    // The widest admissible region, not the first. A region grows forward along transfers from its seed, so a seed downstream of another's grows a *strict sub-region* of it — and splitting that one records a group over positions the larger region also spans, which `grow_window_region` then declines outright, stranding the larger region's slices for the rest of the compilation. `programs/walk_mirror_held_scan.crs` is that shape: its walk's region and the one-continuation sub-region below it are both candidates, and taking the sub-region first would slice a fresh rope per character where `walk_mirror_baseline.crs` virtualizes the same walk. Slices consumed is the key because consuming them is what the rewrite is for; seed order breaks ties, so the choice stays deterministic.
    let mut admitted: Option<WindowRegion> = None;
    for (continuation, definition) in module.live_continuations() {
        if resumes.contains(&continuation) {
            continue;
        }
        for (position, &param) in definition.params.iter().enumerate() {
            if grouped(module, continuation, position)
                || definition.params.len() - 1 + 3 > PARAM_SPLIT_GROWTH_LIMIT
            {
                continue;
            }
            let Some(region) =
                grow_window_region(module, &uses, &resumes, (continuation, position, param))
            else {
                continue;
            };
            let wider = admitted.as_ref().is_none_or(|best| {
                (region.slices.len(), region.members.len())
                    > (best.slices.len(), best.members.len())
            });
            if wider {
                admitted = Some(region);
            }
        }
    }
    let Some(region) = admitted else {
        return false;
    };

    // 1. Split the parameters, highest position first within each continuation so earlier positions stay stable, and record each group.
    let mut fields = BTreeMap::<ValueId, [Atom; 3]>::new();
    let mut splits = region.params.clone();
    splits.sort_by_key(|(continuation, position, _)| (*continuation, std::cmp::Reverse(*position)));
    for &(continuation, position, param) in &splits {
        let base = module.add_value(Some(format!("window/{}/base", continuation.index())));
        let end = module.add_value(Some(format!("window/{}/end", continuation.index())));
        let length = module.add_value(Some(format!("window/{}/length", continuation.index())));
        let definition = module
            .continuation_mut(continuation)
            .expect("admitted continuation is live");
        definition
            .params
            .splice(position..=position, [base, end, length]);
        module.record_split(continuation, position, 3);
        fields.insert(
            param,
            [Atom::Value(base), Atom::Value(end), Atom::Value(length)],
        );
    }

    // 2. Turn each slice into its guard and the end it leaves, in dependency order: a slice of a slice waits until its source's fields exist.
    let mut pending = region.slices.clone();
    while !pending.is_empty() {
        let before = pending.len();
        pending.retain(|&slice| {
            let Node::LetIntrinsic {
                op,
                result,
                args,
                next,
            } = module.node(slice).expect("slice node is live").clone()
            else {
                unreachable!("the region collected only slice intrinsics");
            };
            let Atom::Value(source) = &args[0] else {
                unreachable!("a region slice reads a region member");
            };
            let Some([base, end, length]) = fields.get(source).cloned() else {
                return true;
            };
            let extent = module.add_value(Some(format!("window/{}/extent", slice.index())));

            // A *suffix* names no count, so one is computed here from the member's own `length` — the same fact the emitted rope would have read off itself, derived in this pass because the physical rope is what it is removing — and keeps its end. The guard is the same either way: `WindowExtent` refuses a start past the end, which is exactly what the underflowing difference would ask it for.
            let left = match matches!(WindowFamily::of(op), Some((_, WindowRead::Rest))) {
                true => {
                    let remaining =
                        module.add_value(Some(format!("window/{}/rest", slice.index())));
                    let guard = module.reserve_node();
                    module.set_node(
                        slice,
                        Node::LetIntrinsic {
                            result: remaining,
                            op: Intrinsic::NatSub,
                            args: vec![length.clone(), args[1].clone()],
                            next: guard,
                        },
                    );
                    module.define_node(
                        guard,
                        Node::LetIntrinsic {
                            result: extent,
                            op: Intrinsic::WindowExtent,
                            args: vec![args[1].clone(), Atom::Value(remaining), length.clone()],
                            next,
                        },
                    );
                    end
                }
                // A slice past the guard ends where its window did, less what lies after it: `end ∸ (length ∸ start ∸ count)`.
                false => {
                    let after_start =
                        module.add_value(Some(format!("window/{}/after_start", slice.index())));
                    let after = module.add_value(Some(format!("window/{}/after", slice.index())));
                    let ends = module.add_value(Some(format!("window/{}/end", slice.index())));
                    let [past_start, past, ending] = [(); 3].map(|()| module.reserve_node());
                    module.set_node(
                        slice,
                        Node::LetIntrinsic {
                            result: extent,
                            op: Intrinsic::WindowExtent,
                            args: vec![args[1].clone(), args[2].clone(), length.clone()],
                            next: past_start,
                        },
                    );
                    module.define_node(
                        past_start,
                        Node::LetIntrinsic {
                            result: after_start,
                            op: Intrinsic::NatSub,
                            args: vec![length, args[1].clone()],
                            next: past,
                        },
                    );
                    module.define_node(
                        past,
                        Node::LetIntrinsic {
                            result: after,
                            op: Intrinsic::NatSub,
                            args: vec![Atom::Value(after_start), Atom::Value(extent)],
                            next: ending,
                        },
                    );
                    module.define_node(
                        ending,
                        Node::LetIntrinsic {
                            result: ends,
                            op: Intrinsic::NatSub,
                            args: vec![end, Atom::Value(after)],
                            next,
                        },
                    );
                    Atom::Value(ends)
                }
            };
            fields.insert(result, [base, left, Atom::Value(extent)]);
            false
        });
        assert!(
            pending.len() < before,
            "window slices form a dependency dag rooted at the region's parameters"
        );
    }

    // 3. Rewrite the reads. A length is an alias of the length field; a read reaches the base at `end ∸ (length ∸ index)`, which is `end - length + index` for every index the read's evidence admits.
    for &member in &region.members {
        for this in uses.get(&member).map_or(&[][..], Vec::as_slice) {
            match this {
                WindowUse::Len(node) => {
                    let Node::LetIntrinsic { result, next, .. } =
                        module.node(*node).expect("len node is live").clone()
                    else {
                        unreachable!("the region collected only len intrinsics");
                    };
                    rewire_node(module, *node, next);
                    module.remove_node(*node);
                    module.replace_atom(UseTarget::Value(result), fields[&member][2].clone());
                    module.remove_value(result);
                }
                WindowUse::Get(node) => {
                    let Node::LetIntrinsic {
                        result, args, next, ..
                    } = module.node(*node).expect("get node is live").clone()
                    else {
                        unreachable!("the region collected only get intrinsics");
                    };
                    let ahead = module.add_value(Some(format!("window/{}/ahead", node.index())));
                    let at = module.add_value(Some(format!("window/{}/at", node.index())));
                    insert_above(
                        module,
                        *node,
                        vec![
                            Node::LetIntrinsic {
                                result: ahead,
                                op: Intrinsic::NatSub,
                                args: vec![fields[&member][2].clone(), args[1].clone()],
                                next: *node,
                            },
                            Node::LetIntrinsic {
                                result: at,
                                op: Intrinsic::NatSub,
                                args: vec![fields[&member][1].clone(), Atom::Value(ahead)],
                                next: *node,
                            },
                        ],
                    );
                    module.set_node(
                        *node,
                        Node::LetIntrinsic {
                            result,
                            op: region.row.get_op(),
                            args: vec![fields[&member][0].clone(), Atom::Value(at)],
                            next,
                        },
                    );
                }
                WindowUse::Slice(_) | WindowUse::Transfer(_, _) => {}
                WindowUse::Hostile => unreachable!("an admitted region has no hostile use"),
            }
        }
    }

    // 4. Rewrite every edge into a split continuation, highest position first: a member argument travels as its fields, and any other rope opens as its own whole window with one length read above the jump.
    let carriers: Vec<NodeId> = module
        .live_nodes()
        .filter(|(_, node)| {
            edges_of(node).iter().any(|edge| {
                splits
                    .iter()
                    .any(|(continuation, ..)| edge.target == *continuation)
            })
        })
        .map(|(id, _)| id)
        .collect();
    for carrier in carriers {
        let mut node = module.node(carrier).expect("carrier is live").clone();
        let mut openings = Vec::new();
        for edge in edges_of_mut(&mut node) {
            for &(continuation, position, _) in &splits {
                if edge.target != continuation {
                    continue;
                }
                let atom = edge.args[position].clone();
                let replacement = match &atom {
                    Atom::Value(value) if fields.contains_key(value) => fields[value].to_vec(),
                    _ => {
                        let length =
                            module.add_value(Some(format!("window/{}/open", carrier.index())));
                        openings.push(Node::LetIntrinsic {
                            result: length,
                            op: region.row.len_op(),
                            args: vec![atom.clone()],
                            next: carrier,
                        });
                        vec![atom, Atom::Value(length), Atom::Value(length)]
                    }
                };
                edge.args.splice(position..=position, replacement);
            }
        }
        insert_above(module, carrier, openings);
        module.set_node(carrier, node);
    }

    // 5. The parameters and slice results have no remaining uses; the verifier would name any this pass missed.
    for member in &region.members {
        module.remove_value(*member);
    }

    true
}

/// The continuations that receive call results — the interface the return protocol owns.
fn resume_targets(module: &Module) -> BTreeSet<ContinuationId> {
    module
        .nodes()
        .iter()
        .flatten()
        .filter_map(|node| match node {
            Node::ApplyFun { return_to, .. }
            | Node::Foreign { return_to, .. }
            | Node::Cell { return_to, .. }
            | Node::Channel { return_to, .. }
            | Node::Intrinsic { return_to, .. } => Some(*return_to),
            _ => None,
        })
        .collect()
}

fn edges_of(node: &Node) -> Vec<&Edge> {
    match node {
        Node::ApplyCont(edge) => vec![edge],
        Node::Switch { cases, default, .. } => cases.values().chain(default.as_ref()).collect(),
        _ => vec![],
    }
}

fn edges_of_mut(node: &mut Node) -> Vec<&mut Edge> {
    match node {
        Node::ApplyCont(edge) => vec![edge],
        Node::Switch { cases, default, .. } => cases.values_mut().chain(default.as_mut()).collect(),
        _ => vec![],
    }
}
