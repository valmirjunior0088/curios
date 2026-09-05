//! Continuation scalar replacement: a tuple that travels a join point as one aggregate parameter becomes that many field parameters, and the record in [`FieldGroup`](super::FieldGroup) is what makes the change a fact of the program.
//!
//! Admission composes two halves (see `documentation/design/toolchain/a-value-costs-when-it-is-kept-not-when-it-is-named.md`): the backward half says every use of the parameter is a projection or an eligible transfer (`demands`, `Projected`), and the forward half says every flow reaching it is a construction or an alias of one (`origins`, `Constructed`). Loop backedges are the central case rather than an exclusion — an edge carrying the join's own parameter reads as the constructions that entered it, which is precisely what the forward fixpoint establishes.
//!
//! **A variant is the same rewrite at the widest of several widths.** Where the constructions reaching a parameter disagree about arity — a tagged row's nullary constructor arriving as a one-tuple beside its three-payload sibling's four — the region travels as the widest, and each narrower edge carries its own fields followed by filler. The per-edge width is the same forward fact read at that edge's argument, so a projection is never emitted past what a construction carries.
//!
//! What makes the filler safe is that it *inhabits* the slot with what the field would hold, not that nothing reads it. Unreadness was the stated justification and it is false: the value is passed, and an edge into a raw-carried parameter coerces every argument to that carrier before the discriminant is consulted, so a zero at a guessed carrier trapped on the `ref.cast`. The Ersd door pads a register slot with its zero literal and a reference slot with [`CpsAtom::Filler`], which every reference position admits as null; the one carrier not known here — a parameter the analysis later raises into a register — is the emitter's, and it materialises the filler as that register's zero.
//!
//! The rewrite is three local edits, and the existing chain finishes the job, exactly as `split_returns` works: the parameter list is spliced and the group recorded; the continuation's head rebuilds the aggregate from the new field parameters and every occurrence of the old parameter is redirected to that rebuild; and every incoming edge projects its argument into fields above the jump. Projection forwarding then collapses the inserted reads through the constructions they see, dead-binding elimination removes the constructions nothing reads any more — and the head rebuild survives exactly where a whole-value use survives, which makes it the materialization boundary the cost contract prescribes rather than a leak. For a variant that materialization is *wider* than the constructor that travelled, which is the one place the rewrite spends rather than saves: a surviving whole-value use of a narrow constructor gets its widest sibling's object. It is bounded by the same demand condition that admits the region at all — every use a projection or an eligible transfer — so the rebuild survives only where a boundary the region cannot cross keeps it.
//!
//! Resume continuations are excluded: their parameter list is the call interface the return protocol owns. Splitting inside an already-recorded group is declined — one aggregate is exposed one level per round at fresh joins, and the growth ceiling is what stops recursive structures from flattening without end.

#[cfg(test)]
mod tests;

use {
    super::{
        CpsAtom, CpsCallee, CpsContId, CpsEdge, CpsFunId, CpsIntrinsic, CpsLiteral, CpsModule,
        CpsNode, CpsNodeId, CpsUseTarget, CpsValueExpr, CpsValueId, Demand, Origin,
        analysis::analyze_calls, demand_of, demands, optimize::PARAM_SPLIT_GROWTH_LIMIT, origins,
        simplify::rewire_node,
    },
    curios_utilities::Grain,
    std::collections::{BTreeMap, BTreeSet},
};

/// One admitted split: which parameter of which continuation, and the width every flow travels at — the widest construction reaching it, which is the region's own arity where they agree and the class-merged variant width where they do not.
struct Split {
    continuation: CpsContId,
    position: usize,
    param: CpsValueId,
    width: usize,
    /// The row, where every flow into the parameter is a variant construction of one. Carried from the same origin fact that gave the width, so the head rebuild and the per-edge projections stay in the vocabulary the reads below them use — a variant rebuilt as a structural tuple would trap at the next exact cast.
    row: Option<super::CpsRowId>,
}

/// Whether a site may take `source` apart into fields.
///
/// A source the fixpoint reports at several widths is a variant whose constructor is undecided *there*, so no fixed number of projections is safe: the widest reads past a narrower constructor and traps, and the narrowest drops fields the wider one carries. Such a source is a region of its own, and a round that splits it first turns it into a materialization of one settled width — which is why this is a decline rather than a disqualification. The region's own parameter is the exception it looks like: by the time the edges are rewritten it names the head rebuild, whose width is the region's by construction.
fn takeable(origins: &BTreeMap<CpsValueId, Origin>, param: CpsValueId, source: &CpsAtom) -> bool {
    match source {
        CpsAtom::Value(value) => {
            *value == param || origins.get(value).is_none_or(Origin::is_settled)
        }
        CpsAtom::Fun(_) | CpsAtom::Literal(_) | CpsAtom::Filler => false,
    }
}

/// The construction a rebuild emits: the row's own, where one was carried, and a structural tuple otherwise.
fn rebuild_of(row: Option<super::CpsRowId>, fields: &[CpsValueId]) -> CpsValueExpr {
    let atoms = fields.iter().copied().map(CpsAtom::Value).collect();
    match row {
        Some(row) => CpsValueExpr::Row(row, atoms),
        None => CpsValueExpr::Tuple(atoms),
    }
}

/// The projection a split emits, in the vocabulary its source was built in.
fn projection_of(row: Option<super::CpsRowId>, index: usize) -> CpsIntrinsic {
    match row {
        Some(row) => CpsIntrinsic::RowGet(row, index),
        None => CpsIntrinsic::TupleGet(index),
    }
}

/// Every admissible split, in deterministic order: continuations by identity, positions ascending within each.
fn admit(module: &CpsModule, origins: &BTreeMap<CpsValueId, Origin>) -> Vec<Split> {
    let demands = demands(module);

    // A resume's parameters are the call interface the return protocol owns, whatever their demand says.
    let resumes = resume_targets(module);

    // Every edge into each continuation, so the per-position source check below is one pass rather than one per candidate parameter.
    let mut incoming = BTreeMap::<CpsContId, Vec<&CpsEdge>>::new();
    for (_, node) in module.nodes.iter_live() {
        for edge in edges_of(node) {
            incoming.entry(edge.target).or_default().push(edge);
        }
    }

    let mut admitted = Vec::new();
    for (continuation, definition) in module.continuations.iter_live() {
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
/// The sweep reuses one snapshot for every split, which is `inline_known_calls`' shape, and what keeps that honest is that a fresh value *declines* here where the rewrite below could *assume*. An earlier split's head rebuild is absent from the snapshot. With one split per call that absence could only mean a value the fixpoint never reached, and the rewrite read the region's full width for it; in a sweep the rebuild may be standing on an edge into this very candidate — the earlier split's parameter was redirected to it everywhere — at its own region's width rather than this one's, and projecting it at this width would read past a narrower constructor. So every incoming argument must be this parameter or a value the snapshot holds a fact for, and whatever an earlier split put there waits for the next round's facts. That wait is the one genuine dependency between two splits, and the only one that still costs a round.
///
/// The live parameter list is re-read for the same reason: an earlier split of the same continuation at a higher position has already grown it, and the ceiling is a fact of the list as it stands.
fn still_admissible(
    module: &CpsModule,
    origins: &BTreeMap<CpsValueId, Origin>,
    carriers: &[CpsNodeId],
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
            Some(CpsAtom::Value(value)) => {
                *value == split.param || origins.get(value).is_some_and(Origin::is_settled)
            }
            _ => false,
        })
}

/// Split every admissible continuation parameter into its fields against one forward snapshot, record each group, and leave the cleanup to the chain.
///
/// It was one split per call, on the reasoning that the optimizer's own fixpoint would drive region-wide convergence — and it did, at one round of every pass per split: `fixpoint_pass_measurements` found this pass firing on 54 of a `Toml/decode` compile's 57 rounds, with most of the fixpoint's time going to passes that rewrote nothing on any of them. A sweep applies what one snapshot admits and [`still_admissible`] declines what an earlier split in the sweep has since touched, so the rounds that remain are the ones a split genuinely depends on. Determinism is the order below; termination is unchanged, since every split still consumes an unrecorded aggregate parameter under the growth ceiling.
pub(super) fn split_parameters(module: &mut CpsModule) -> bool {
    let origins = origins(module);
    let mut admitted = admit(module, &origins);
    // Highest position first within a continuation, so an earlier position's index survives a later one's splice — the order `split_windows` applies its positions in, for the same reason.
    admitted.sort_by_key(|split| (split.continuation, std::cmp::Reverse(split.position)));

    // The nodes carrying an edge into each continuation, indexed once for the sweep. A split repoints a carrier's predecessors at its projection chain and sets the carrier in place, so the index holds across the sweep — and it is what keeps a sweep of a hundred splits from walking the module a hundred times to find the few nodes each one rewrites.
    let mut carriers = BTreeMap::<CpsContId, Vec<CpsNodeId>>::new();
    for (id, node) in module.nodes.iter_live() {
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
    module: &mut CpsModule,
    origins: &BTreeMap<CpsValueId, Origin>,
    carriers: &[CpsNodeId],
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
        .continuations
        .get_mut(split.continuation)
        .expect("admitted continuation is live");
    definition
        .params
        .splice(split.position..=split.position, fields.iter().copied());
    let body = definition.body;
    module.record_split(split.continuation, split.position, split.width);

    // The head rebuild: the aggregate reconstructed from its fields, standing in for the old parameter everywhere. It survives exactly where a whole-value use survives, and is the materialization the cost contract allows at such a boundary.
    let rebuilt = module.add_value(Some(format!("rebuilt/{}", split.continuation.index())));
    let head = module.add_node(CpsNode::LetValue {
        result: rebuilt,
        value: rebuild_of(split.row, &fields),
        next: body,
    });
    module
        .continuations
        .get_mut(split.continuation)
        .expect("admitted continuation is live")
        .body = head;
    module.replace_atom(CpsUseTarget::Value(split.param), CpsAtom::Value(rebuilt));
    module.values.remove(split.param);

    // Every incoming edge projects its argument into fields above the jump; forwarding collapses the reads through visible constructions on the next rounds.
    for &carrier in carriers {
        let mut node = module.node(carrier).expect("carrier is live").clone();
        let mut chain = Vec::new();
        for edge in edges_of_mut(&mut node) {
            if edge.target != split.continuation {
                continue;
            }
            let CpsAtom::Value(source) = &edge.args[split.position] else {
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
                replacement.push(CpsAtom::Value(projection));
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
                CpsNode::LetIntrinsic {
                    result: projection,
                    op: projection_of(split.row, index),
                    args: vec![CpsAtom::Value(source)],
                    next,
                },
            );
        }
        module.nodes.set(carrier, node);
    }
}

// -- known-function workers --------------------------------------------------
//
// The same rewrite one boundary over. A join parameter's split stops at a known call, because the callee takes the aggregate whole and the head rebuild has to materialize it — which is exactly where the UTF-8 scan's last per-character construction lives once the loop carries it as fields. Splitting the *callee's* parameter removes that boundary, and the census is what admits it: the known-call flow class is non-empty and names the scan.
//
// **There is no wrapper, because there are no unknown callers.** A worker/wrapper pair exists to keep a retained ABI for a caller the rewrite cannot see; an escaping function is refused outright here instead, so every call site is a `Known` one the rewrite reaches. That is a narrower mechanism than the precedent's and it is the whole of what the measurement asked for.
//
// **The width-two floor is a termination property, not tidiness** — the same one `split_returns` records. A continuation's split is stopped from repeating by the recorded `FieldGroup`; a function carries no such record, so what bounds it is growth: every admitted split adds at least one parameter and `PARAM_SPLIT_GROWTH_LIMIT` caps the total. A width-one split would add none and could be re-admitted forever.

/// One admitted worker split: which parameter of which known function, and the width its flows travel at.
struct Worker {
    function: CpsFunId,
    position: usize,
    param: CpsValueId,
    width: usize,
    /// See [`Split::row`].
    row: Option<super::CpsRowId>,
}

/// The first admissible worker split in deterministic order, if any.
fn admit_worker(module: &CpsModule, origins: &BTreeMap<CpsValueId, Origin>) -> Option<Worker> {
    let demands = demands(module);
    let calls = analyze_calls(module);

    for (function, definition) in module.functions.iter_live() {
        // An escaping function is reached by callers this rewrite cannot see, and the module entry by the host, which is not rewritten with the module.
        if calls.escaping.contains(&function) || module.entry() == Some(function) {
            continue;
        }
        for (position, &param) in definition.params.iter().enumerate() {
            let Some(width) = origins.get(&param).and_then(Origin::width) else {
                continue;
            };
            let Demand::Projected(read) = demand_of(&demands, param) else {
                continue;
            };
            if width < 2 || read.last().is_some_and(|&last| last >= width) {
                continue;
            }
            if definition.params.len() - 1 + width > PARAM_SPLIT_GROWTH_LIMIT {
                continue;
            }
            if !calls
                .call_sites
                .get(&function)
                .map_or(&[][..], Vec::as_slice)
                .iter()
                .all(|site| match module.node(*site) {
                    Some(CpsNode::ApplyFun { args, .. }) => args
                        .get(position)
                        .is_some_and(|atom| takeable(origins, param, atom)),
                    _ => false,
                })
            {
                continue;
            }
            return Some(Worker {
                function,
                position,
                param,
                width,
                row: origins.get(&param).and_then(Origin::row),
            });
        }
    }
    None
}

/// Split one admissible known function's parameter into its fields, and leave the cleanup to the chain — the same three local edits [`split_parameters`] performs, with call sites in place of edges.
pub(super) fn split_workers(module: &mut CpsModule) -> bool {
    let origins = origins(module);
    let Some(worker) = admit_worker(module, &origins) else {
        return false;
    };

    let fields = (0..worker.width)
        .map(|index| module.add_value(Some(format!("worker/{}/{index}", worker.function.index()))))
        .collect::<Vec<_>>();
    let definition = module
        .functions
        .get_mut(worker.function)
        .expect("admitted function is live");
    definition
        .params
        .splice(worker.position..=worker.position, fields.iter().copied());
    let body = definition.body;

    // The head rebuild, standing in for the old parameter everywhere. Projection forwarding erases it wherever the body only reads fields, which the admitted demand guarantees it does.
    let rebuilt = module.add_value(Some(format!("rebuilt/{}", worker.function.index())));
    let head = module.add_node(CpsNode::LetValue {
        result: rebuilt,
        value: rebuild_of(worker.row, &fields),
        next: body,
    });
    module
        .functions
        .get_mut(worker.function)
        .expect("admitted function is live")
        .body = head;
    module.replace_atom(CpsUseTarget::Value(worker.param), CpsAtom::Value(rebuilt));
    module.values.remove(worker.param);

    // Every call site projects its argument into fields above the call, filling what its own construction does not carry.
    let callers = module
        .nodes
        .iter_live()
        .filter(|(_, node)| {
            matches!(
                node,
                CpsNode::ApplyFun { callee: CpsCallee::Known(callee), .. } if *callee == worker.function
            )
        })
        .map(|(id, _)| id)
        .collect::<Vec<_>>();
    for caller in callers {
        let mut node = module.node(caller).expect("caller is live").clone();
        let CpsNode::ApplyFun { args, .. } = &mut node else {
            unreachable!("the callers were selected as known applications");
        };
        let CpsAtom::Value(source) = &args[worker.position] else {
            unreachable!("a construction origin admits only value arguments");
        };
        let source = *source;
        let carried = origins
            .get(&source)
            .and_then(Origin::settled_width)
            .unwrap_or(worker.width);
        let mut inserted = Vec::new();
        let mut replacement = Vec::with_capacity(worker.width);
        for index in 0..carried {
            let projection = module.add_value(Some(format!("worker/{}/{index}", caller.index())));
            inserted.push(CpsNode::LetIntrinsic {
                result: projection,
                op: projection_of(worker.row, index),
                args: vec![CpsAtom::Value(source)],
                next: caller,
            });
            replacement.push(CpsAtom::Value(projection));
        }
        replacement.extend((carried..worker.width).map(|index| module.pad(worker.row, index)));
        let CpsNode::ApplyFun { args, .. } = &mut node else {
            unreachable!("the callers were selected as known applications");
        };
        args.splice(worker.position..=worker.position, replacement);
        insert_above(module, caller, inserted);
        module.nodes.set(caller, node);
    }

    true
}

// -- prepared windows --------------------------------------------------------
//
// The rope analogue of the product split, sharing the record rather than growing a second coordination discipline. A window is `(base, offset, length)`: any rope is its own whole window as `(r, 0, len r)`, a virtual slice is offset arithmetic behind a `WindowExtent` guard that keeps the eager bounds trap at the original evaluation point, and a read reaches the base directly — the read helper already forces and memoizes an uncached node on first contact, so deferring the physical slice's force changes no observable value. Unlike products there is no cheap head rebuild, so the rewrite is region-atomic: a region is admitted only when every use of every member — the parameters and the slice results flowing between them — is a length, a read, a further slice, or a transfer into the region, and a region with any other use is declined whole, the conservative limit the value-lifetime decision allows the first implementation.

/// Which sequence row a window region reads through, unified across every operation the region contains.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WindowFamily {
    Bin(Grain),
    List,
}

impl WindowFamily {
    fn of(op: CpsIntrinsic) -> Option<(Self, WindowRead)> {
        match op {
            CpsIntrinsic::BinLen(grain) => Some((Self::Bin(grain), WindowRead::Len)),
            CpsIntrinsic::BinGet(grain) => Some((Self::Bin(grain), WindowRead::Get)),
            CpsIntrinsic::BinSlice(grain) => Some((Self::Bin(grain), WindowRead::Slice)),
            CpsIntrinsic::BinRest(grain) => Some((Self::Bin(grain), WindowRead::Rest)),
            CpsIntrinsic::ListLen => Some((Self::List, WindowRead::Len)),
            CpsIntrinsic::ListGet => Some((Self::List, WindowRead::Get)),
            CpsIntrinsic::ListSlice => Some((Self::List, WindowRead::Slice)),
            CpsIntrinsic::ListRest => Some((Self::List, WindowRead::Rest)),
            _ => None,
        }
    }

    fn len_op(self) -> CpsIntrinsic {
        match self {
            Self::Bin(grain) => CpsIntrinsic::BinLen(grain),
            Self::List => CpsIntrinsic::ListLen,
        }
    }

    fn get_op(self) -> CpsIntrinsic {
        match self {
            Self::Bin(grain) => CpsIntrinsic::BinGet(grain),
            Self::List => CpsIntrinsic::ListGet,
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
    Len(CpsNodeId),
    Get(CpsNodeId),
    Slice(CpsNodeId),
    Transfer(CpsContId, usize),
    Hostile,
}

/// Every value's window-relevant occurrences, in one pass over the module.
fn window_uses(module: &CpsModule) -> BTreeMap<CpsValueId, Vec<WindowUse>> {
    let mut uses = BTreeMap::<CpsValueId, Vec<WindowUse>>::new();
    let mut record = |value: CpsValueId, this: WindowUse| uses.entry(value).or_default().push(this);
    for (id, node) in module.nodes.iter_live() {
        match node {
            CpsNode::LetIntrinsic { op, args, .. } => {
                for (position, atom) in args.iter().enumerate() {
                    let CpsAtom::Value(value) = atom else {
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
            CpsNode::ApplyCont(_) | CpsNode::Switch { .. } => {
                if let CpsNode::Switch { scrutinee, .. } = node
                    && let CpsAtom::Value(value) = scrutinee
                {
                    record(*value, WindowUse::Hostile);
                }
                for edge in edges_of(node) {
                    let defined = module.continuation(edge.target).is_some();
                    for (position, atom) in edge.args.iter().enumerate() {
                        if let CpsAtom::Value(value) = atom {
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
                for atom in super::atoms(node) {
                    if let CpsAtom::Value(value) = atom {
                        record(*value, WindowUse::Hostile);
                    }
                }
                if let CpsNode::ApplyFun {
                    callee: CpsCallee::Closure(value),
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
    params: Vec<(CpsContId, usize, CpsValueId)>,
    slices: Vec<CpsNodeId>,
    members: BTreeSet<CpsValueId>,
}

/// Grow a region from `seed` or refuse it: every member's every use must be a window read or a transfer into a splittable parameter, and the region must consume at least one slice — a rope that is only read is not paying the cost this rewrite removes.
fn grow_window_region(
    module: &CpsModule,
    uses: &BTreeMap<CpsValueId, Vec<WindowUse>>,
    resumes: &BTreeSet<CpsContId>,
    seed: (CpsContId, usize, CpsValueId),
) -> Option<WindowRegion> {
    let mut row = None;
    let mut params = vec![seed];
    let mut slices = Vec::new();
    let mut members = BTreeSet::from([seed.2]);
    let mut work = vec![seed.2];

    let unify = |row: &mut Option<WindowFamily>, op: CpsIntrinsic| {
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
                    let CpsNode::LetIntrinsic { op, .. } = module.node(*node)? else {
                        return None;
                    };
                    unify(&mut row, *op)?;
                }
                WindowUse::Slice(node) => {
                    let CpsNode::LetIntrinsic { op, result, .. } = module.node(*node)? else {
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
    let mut widened = BTreeMap::<CpsContId, usize>::new();
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
fn grouped(module: &CpsModule, continuation: CpsContId, position: usize) -> bool {
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
fn insert_above(module: &mut CpsModule, carrier: CpsNodeId, nodes: Vec<CpsNode>) {
    if nodes.is_empty() {
        return;
    }
    let ids: Vec<CpsNodeId> = nodes.iter().map(|_| module.reserve_node()).collect();
    rewire_node(module, carrier, ids[0]);
    for (index, node) in nodes.into_iter().enumerate() {
        let mut node = node;
        if let CpsNode::LetIntrinsic { next, .. } | CpsNode::LetValue { next, .. } = &mut node {
            *next = ids.get(index + 1).copied().unwrap_or(carrier);
        }
        module.define_node(ids[index], node);
    }
}

/// Virtualize one whole window region: every parameter becomes `(base, offset, length)` under a recorded group, every slice becomes a guarded extent plus an offset sum, every read reaches the base directly, and every entry edge opens its rope as a whole window. The physical views and the helper calls that built them are simply never emitted — nothing here deletes them, they are unread.
pub(super) fn split_windows(module: &mut CpsModule) -> bool {
    let uses = window_uses(module);
    let resumes = resume_targets(module);

    // The widest admissible region, not the first. A region grows forward along transfers from its seed, so a seed downstream of another's grows a *strict sub-region* of it — and splitting that one records a group over positions the larger region also spans, which `grow_window_region` then declines outright, stranding the larger region's slices for the rest of the compilation. That is the difference between `programs/walk_mirror_held_scan.crs` slicing a fresh rope per character and `walk_mirror_baseline.crs` virtualizing the same walk: the walk's region and the one-continuation sub-region below it were both candidates, and the sub-region was enumerated first. Slices consumed is the key because consuming them is what the rewrite is for; seed order breaks ties, so the choice stays deterministic.
    let mut admitted: Option<WindowRegion> = None;
    for (continuation, definition) in module.continuations.iter_live() {
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
    let mut fields = BTreeMap::<CpsValueId, [CpsAtom; 3]>::new();
    let mut splits = region.params.clone();
    splits.sort_by_key(|(continuation, position, _)| (*continuation, std::cmp::Reverse(*position)));
    for &(continuation, position, param) in &splits {
        let base = module.add_value(Some(format!("window/{}/base", continuation.index())));
        let offset = module.add_value(Some(format!("window/{}/offset", continuation.index())));
        let length = module.add_value(Some(format!("window/{}/length", continuation.index())));
        let definition = module
            .continuations
            .get_mut(continuation)
            .expect("admitted continuation is live");
        definition
            .params
            .splice(position..=position, [base, offset, length]);
        module.record_split(continuation, position, 3);
        fields.insert(
            param,
            [
                CpsAtom::Value(base),
                CpsAtom::Value(offset),
                CpsAtom::Value(length),
            ],
        );
    }

    // 2. Turn each slice into its guard and its offset sum, in dependency order: a slice of a slice waits until its source's fields exist.
    let mut pending = region.slices.clone();
    while !pending.is_empty() {
        let before = pending.len();
        pending.retain(|&slice| {
            let CpsNode::LetIntrinsic {
                op,
                result,
                args,
                next,
            } = module.node(slice).expect("slice node is live").clone()
            else {
                unreachable!("the region collected only slice intrinsics");
            };
            let CpsAtom::Value(source) = &args[0] else {
                unreachable!("a region slice reads a region member");
            };
            let Some([base, offset, length]) = fields.get(source).cloned() else {
                return true;
            };
            let extent = module.add_value(Some(format!("window/{}/extent", slice.index())));
            let sum = module.add_value(Some(format!("window/{}/sum", slice.index())));
            let add = module.reserve_node();

            // A *suffix* names no count, so one is computed here from the member's own `length` — the same fact the emitted rope would have read off itself, derived in this pass because the physical rope is what it is removing. The guard is the same either way: `WindowExtent` refuses a start past the end, which is exactly what the underflowing difference would ask it for.
            match matches!(WindowFamily::of(op), Some((_, WindowRead::Rest))) {
                false => module.nodes.set(
                    slice,
                    CpsNode::LetIntrinsic {
                        result: extent,
                        op: CpsIntrinsic::WindowExtent,
                        args: vec![args[1].clone(), args[2].clone(), length],
                        next: add,
                    },
                ),
                true => {
                    let remaining =
                        module.add_value(Some(format!("window/{}/rest", slice.index())));
                    let guard = module.reserve_node();

                    module.nodes.set(
                        slice,
                        CpsNode::LetIntrinsic {
                            result: remaining,
                            op: CpsIntrinsic::NatSub,
                            args: vec![length.clone(), args[1].clone()],
                            next: guard,
                        },
                    );
                    module.define_node(
                        guard,
                        CpsNode::LetIntrinsic {
                            result: extent,
                            op: CpsIntrinsic::WindowExtent,
                            args: vec![args[1].clone(), CpsAtom::Value(remaining), length.clone()],
                            next: add,
                        },
                    );
                }
            }
            module.define_node(
                add,
                CpsNode::LetIntrinsic {
                    result: sum,
                    op: CpsIntrinsic::NatAdd,
                    args: vec![offset, args[1].clone()],
                    next,
                },
            );
            fields.insert(result, [base, CpsAtom::Value(sum), CpsAtom::Value(extent)]);
            false
        });
        assert!(
            pending.len() < before,
            "window slices form a dependency dag rooted at the region's parameters"
        );
    }

    // 3. Rewrite the reads. A length is an alias of the length field; a read reaches the base at the summed offset.
    for &member in &region.members {
        for this in uses.get(&member).map_or(&[][..], Vec::as_slice) {
            match this {
                WindowUse::Len(node) => {
                    let CpsNode::LetIntrinsic { result, next, .. } =
                        module.node(*node).expect("len node is live").clone()
                    else {
                        unreachable!("the region collected only len intrinsics");
                    };
                    rewire_node(module, *node, next);
                    module.remove_node(*node);
                    module.replace_atom(CpsUseTarget::Value(result), fields[&member][2].clone());
                    module.values.remove(result);
                }
                WindowUse::Get(node) => {
                    let CpsNode::LetIntrinsic {
                        result, args, next, ..
                    } = module.node(*node).expect("get node is live").clone()
                    else {
                        unreachable!("the region collected only get intrinsics");
                    };
                    let sum = module.add_value(Some(format!("window/{}/at", node.index())));
                    insert_above(
                        module,
                        *node,
                        vec![CpsNode::LetIntrinsic {
                            result: sum,
                            op: CpsIntrinsic::NatAdd,
                            args: vec![fields[&member][1].clone(), args[1].clone()],
                            next: *node,
                        }],
                    );
                    module.nodes.set(
                        *node,
                        CpsNode::LetIntrinsic {
                            result,
                            op: region.row.get_op(),
                            args: vec![fields[&member][0].clone(), CpsAtom::Value(sum)],
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
    let carriers: Vec<CpsNodeId> = module
        .nodes
        .iter_live()
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
                    CpsAtom::Value(value) if fields.contains_key(value) => fields[value].to_vec(),
                    _ => {
                        let length =
                            module.add_value(Some(format!("window/{}/open", carrier.index())));
                        openings.push(CpsNode::LetIntrinsic {
                            result: length,
                            op: region.row.len_op(),
                            args: vec![atom.clone()],
                            next: carrier,
                        });
                        vec![
                            atom,
                            CpsAtom::Literal(CpsLiteral::Nat(0)),
                            CpsAtom::Value(length),
                        ]
                    }
                };
                edge.args.splice(position..=position, replacement);
            }
        }
        insert_above(module, carrier, openings);
        module.nodes.set(carrier, node);
    }

    // 5. The parameters and slice results have no remaining uses; the verifier would name any this pass missed.
    for member in &region.members {
        module.values.remove(*member);
    }

    true
}

/// The continuations that receive call results — the interface the return protocol owns.
fn resume_targets(module: &CpsModule) -> BTreeSet<CpsContId> {
    module
        .nodes
        .slots()
        .iter()
        .flatten()
        .filter_map(|node| match node {
            CpsNode::ApplyFun { return_to, .. }
            | CpsNode::Foreign { return_to, .. }
            | CpsNode::Cell { return_to, .. }
            | CpsNode::Intrinsic { return_to, .. } => Some(*return_to),
            _ => None,
        })
        .collect()
}

fn edges_of(node: &CpsNode) -> Vec<&CpsEdge> {
    match node {
        CpsNode::ApplyCont(edge) => vec![edge],
        CpsNode::Switch { cases, default, .. } => cases.values().chain(default.as_ref()).collect(),
        _ => vec![],
    }
}

fn edges_of_mut(node: &mut CpsNode) -> Vec<&mut CpsEdge> {
    match node {
        CpsNode::ApplyCont(edge) => vec![edge],
        CpsNode::Switch { cases, default, .. } => {
            cases.values_mut().chain(default.as_mut()).collect()
        }
        _ => vec![],
    }
}
