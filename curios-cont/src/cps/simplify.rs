use {
    super::evaluate::evaluate,
    super::*,
    std::collections::{BTreeMap, BTreeSet},
};

pub(super) fn rewrite_atoms(module: &mut CpsModule, known: &BTreeMap<CpsValueId, CpsAtom>) -> bool {
    let mut changed = false;
    for (_, node) in module.nodes.iter_live_mut() {
        visit_atoms_mut(node, &mut |atom| {
            if let CpsAtom::Value(value) = atom
                && let Some(replacement) = known.get(value)
                && atom != replacement
            {
                *atom = replacement.clone();
                changed = true;
            }
        });

        // A closure callee holds its callee in a value, which `visit_atoms_mut` does not reach. Remap it here: a known function devirtualizes the call, and a forwarded value (e.g. a projected constructor field) keeps the callee pointing at a live value rather than a deleted one.
        if let CpsNode::ApplyFun { callee, .. } = node
            && let CpsCallee::Closure(value) = *callee
        {
            match known.get(&value) {
                Some(CpsAtom::Fun(function)) => {
                    *callee = CpsCallee::Known(*function);
                    changed = true;
                }
                Some(CpsAtom::Value(replacement)) if *replacement != value => {
                    *callee = CpsCallee::Closure(*replacement);
                    changed = true;
                }
                _ => {}
            }
        }
    }
    changed
}
pub(super) fn forward_continuations(module: &mut CpsModule) -> bool {
    let forwarding = module
        .continuations
        .iter_live()
        .filter_map(|(id, continuation)| {
            let CpsNode::ApplyCont(edge) = module.node(continuation.body)? else {
                return None;
            };
            module.continuation(edge.target)?;
            Some((id, (continuation.params.clone(), edge.clone())))
        })
        .collect::<BTreeMap<_, _>>();
    if forwarding.is_empty() {
        return false;
    }

    let identity = forwarding
        .iter()
        .filter_map(|(&continuation, (params, edge))| {
            (params.len() == edge.args.len()
                && params
                    .iter()
                    .zip(&edge.args)
                    .all(|(param, arg)| arg == &CpsAtom::Value(*param)))
            .then_some((continuation, edge.target))
        })
        .collect::<BTreeMap<_, _>>();
    let resolve_identity = |original: CpsContId| {
        let mut target = original;
        let mut seen = BTreeSet::new();
        loop {
            if !seen.insert(target) {
                return original;
            }
            let Some(next) = identity.get(&target) else {
                break;
            };
            target = *next;
        }
        target
    };

    let mut changed = false;
    for (_, node) in module.nodes.iter_live_mut() {
        match node {
            CpsNode::ApplyCont(edge) => {
                thread_edge(edge, &forwarding, &mut changed);
            }
            CpsNode::Switch { cases, default, .. } => {
                for edge in cases.values_mut().chain(default.iter_mut()) {
                    thread_edge(edge, &forwarding, &mut changed);
                }
            }
            CpsNode::ApplyFun { return_to, .. }
            | CpsNode::Foreign { return_to, .. }
            | CpsNode::Cell { return_to, .. }
            | CpsNode::Intrinsic { return_to, .. } => {
                retarget(return_to, &resolve_identity, &mut changed);
            }
            _ => {}
        }
    }
    changed
}
pub(super) fn thread_edge(
    edge: &mut CpsEdge,
    forwarding: &BTreeMap<CpsContId, (Vec<CpsValueId>, CpsEdge)>,
    changed: &mut bool,
) {
    let original = edge.clone();
    let mut replacement = original.clone();
    let mut seen = BTreeSet::new();
    loop {
        if !seen.insert(replacement.target) {
            return;
        }
        let Some((params, outgoing)) = forwarding.get(&replacement.target) else {
            break;
        };
        if params.len() != replacement.args.len() {
            return;
        }
        let substitutions = params
            .iter()
            .copied()
            .zip(replacement.args.iter().cloned())
            .collect::<BTreeMap<_, _>>();
        replacement = CpsEdge {
            target: outgoing.target,
            args: outgoing
                .args
                .iter()
                .map(|arg| match arg {
                    CpsAtom::Value(value) => substitutions
                        .get(value)
                        .cloned()
                        .unwrap_or_else(|| arg.clone()),
                    _ => arg.clone(),
                })
                .collect(),
        };
    }
    if replacement.target != original.target || replacement.args != original.args {
        *edge = replacement;
        *changed = true;
    }
}
pub(super) fn retarget(
    target: &mut CpsContId,
    resolve: &impl Fn(CpsContId) -> CpsContId,
    changed: &mut bool,
) {
    let replacement = resolve(*target);
    if replacement != *target {
        *target = replacement;
        *changed = true;
    }
}
pub(super) fn simplify_nodes(module: &mut CpsModule) -> bool {
    let mut changed = false;
    for (_, node) in module.nodes.iter_live_mut() {
        match node {
            CpsNode::LetIntrinsic {
                result,
                op,
                args,
                next,
            } => {
                if let Some(literal) = evaluate(*op, args) {
                    *node = CpsNode::LetValue {
                        result: *result,
                        value: CpsValueExpr::Literal(literal),
                        next: *next,
                    };
                    changed = true;
                }
            }
            CpsNode::Switch {
                scrutinee: CpsAtom::Literal(CpsLiteral::Nat(tag)),
                cases,
                default,
            } => {
                if let Some(edge) = cases.get(tag).or(default.as_ref()).cloned() {
                    *node = CpsNode::ApplyCont(edge);
                    changed = true;
                }
            }
            _ => {}
        }
    }
    changed
}
/// The two rewrite shapes an identity law produces: forward the surviving operand, or pin the absorbed result as a literal.
enum IdentityFold {
    Operand(CpsAtom),
    Literal(CpsLiteral),
}

/// Match one `Nat`/`Int` identity or absorption law on a binary intrinsic with a literal neutral or absorbing operand: `x + 0`, `x - 0`, `x * 1`, `x * 0`, `x / 1`, `x % 1`, `x & 0`, `x | 0`, `x ^ 0`, and shifts by zero.
///
/// Trap discipline: `nat_add`/`nat_mul` wrap and `nat_sub` is monus, so the only runtime trap of the `MayTrap` members is the backend's i31 range check on the result. Every fold here returns either an operand that is already a live in-range value or a literal inside the envelope, and a `/ 1` or `% 1` divisor can never be the trapping zero, so no trap is added or dropped. `Flt` deliberately has no laws here: `x + 0.0` is not the identity on `-0.0`.
fn identity_fold(op: CpsIntrinsic, args: &[CpsAtom]) -> Option<IdentityFold> {
    let [left, right] = args else { return None };
    let nat = |atom: &CpsAtom| match atom {
        CpsAtom::Literal(CpsLiteral::Nat(value)) => Some(*value),
        _ => None,
    };
    let int = |atom: &CpsAtom| match atom {
        CpsAtom::Literal(CpsLiteral::Int(value)) => Some(*value),
        _ => None,
    };
    let operand = |atom: &CpsAtom| Some(IdentityFold::Operand(atom.clone()));

    match op {
        CpsIntrinsic::NatAdd | CpsIntrinsic::NatOr | CpsIntrinsic::NatXor => {
            if nat(right) == Some(0) {
                operand(left)
            } else if nat(left) == Some(0) {
                operand(right)
            } else {
                None
            }
        }
        CpsIntrinsic::IntAdd | CpsIntrinsic::IntOr | CpsIntrinsic::IntXor => {
            if int(right) == Some(0) {
                operand(left)
            } else if int(left) == Some(0) {
                operand(right)
            } else {
                None
            }
        }
        // A shift count is a `Nat` on both carriers.
        CpsIntrinsic::NatSub
        | CpsIntrinsic::NatShl
        | CpsIntrinsic::NatShr
        | CpsIntrinsic::IntShl
        | CpsIntrinsic::IntShr => (nat(right) == Some(0)).then(|| operand(left)).flatten(),
        CpsIntrinsic::IntSub => (int(right) == Some(0)).then(|| operand(left)).flatten(),
        CpsIntrinsic::NatMul => {
            if nat(right) == Some(1) {
                operand(left)
            } else if nat(left) == Some(1) {
                operand(right)
            } else if nat(right) == Some(0) || nat(left) == Some(0) {
                Some(IdentityFold::Literal(CpsLiteral::Nat(0)))
            } else {
                None
            }
        }
        CpsIntrinsic::IntMul => {
            if int(right) == Some(1) {
                operand(left)
            } else if int(left) == Some(1) {
                operand(right)
            } else if int(right) == Some(0) || int(left) == Some(0) {
                Some(IdentityFold::Literal(CpsLiteral::Int(0)))
            } else {
                None
            }
        }
        CpsIntrinsic::NatDiv => (nat(right) == Some(1)).then(|| operand(left)).flatten(),
        CpsIntrinsic::IntDiv => (int(right) == Some(1)).then(|| operand(left)).flatten(),
        CpsIntrinsic::NatRem => {
            (nat(right) == Some(1)).then_some(IdentityFold::Literal(CpsLiteral::Nat(0)))
        }
        CpsIntrinsic::IntRem => {
            (int(right) == Some(1)).then_some(IdentityFold::Literal(CpsLiteral::Int(0)))
        }
        CpsIntrinsic::NatAnd => (nat(right) == Some(0) || nat(left) == Some(0))
            .then_some(IdentityFold::Literal(CpsLiteral::Nat(0))),
        CpsIntrinsic::IntAnd => (int(right) == Some(0) || int(left) == Some(0))
            .then_some(IdentityFold::Literal(CpsLiteral::Int(0))),
        _ => None,
    }
}

/// Fold intrinsic identity and absorption laws with one literal operand, which all-literal folding (`evaluate`) cannot reach. An operand fold forwards the surviving value and deletes the binding; an absorption fold pins the result as a literal in place.
pub(super) fn fold_intrinsic_identities(module: &mut CpsModule) -> bool {
    let mut changed = false;
    loop {
        let selected = module.nodes.iter_live().find_map(|(id, node)| {
            let CpsNode::LetIntrinsic {
                result,
                op,
                args,
                next,
            } = node
            else {
                return None;
            };
            let folded = identity_fold(*op, args)?;
            Some((id, *result, *next, folded))
        });
        let Some((node, result, next, folded)) = selected else {
            break;
        };

        match folded {
            IdentityFold::Operand(replacement) => {
                rewrite_atoms(module, &BTreeMap::from([(result, replacement)]));
                rewire_node(module, node, next);
                module.nodes.remove(node);
                module.values.remove(result);
            }
            IdentityFold::Literal(literal) => {
                module.nodes.set(
                    node,
                    CpsNode::LetValue {
                        result,
                        value: CpsValueExpr::Literal(literal),
                        next,
                    },
                );
            }
        }
        changed = true;
    }
    changed
}

/// Fuse a chain of packed appends into one flat chunk build. A literal with non-constant atoms lowers to appends onto whatever precedes it — the free monoid's honest spelling — and each append allocates a one-element leaf and a node the first read then gathers. Where the chain is local and unshared, the elements build one exact flat leaf instead: `BinChunk` alone when the chain is rooted at the empty packed value, or the root concatenated with the chunk otherwise. Only an intermediate append nothing else reads may fuse — a shared intermediate is a value the program observes — and a lone append onto a non-empty root stays as written, since a one-element chunk beside a concat node buys back exactly what it costs.
pub(super) fn fuse_append_chains(module: &mut CpsModule) -> bool {
    let counts = module.value_use_counts();

    // Every packed append by its result — node, grain, base, element, successor — and every packed literal binding, so a chain rooted at an interned empty is recognized.
    let mut appends = BTreeMap::new();
    let mut literals = BTreeMap::new();
    for (id, node) in module.nodes.iter_live() {
        match node {
            CpsNode::LetIntrinsic {
                result,
                op: CpsIntrinsic::BinAppend(grain),
                args,
                next,
            } => {
                appends.insert(
                    *result,
                    (id, *grain, args[0].clone(), args[1].clone(), *next),
                );
            }
            CpsNode::LetValue {
                result,
                value: CpsValueExpr::Literal(CpsLiteral::Bin(grain, value)),
                ..
            } => {
                literals.insert(*result, (*grain, value.clone()));
            }
            _ => {}
        }
    }

    // An interior link is an append result whose one use is the base of a same-grain append; a chain is walked from each tip — an append that is no interior link — down through interior links to its root atom.
    let interior = |value: &CpsValueId, grain: Grain| {
        counts.get(value).copied().unwrap_or(0) == 1
            && appends.get(value).is_some_and(|(_, g, ..)| *g == grain)
    };
    let is_empty_literal = |atom: &CpsAtom, grain: Grain| match atom {
        CpsAtom::Literal(CpsLiteral::Bin(g, value)) => *g == grain && value.len(grain) == 0,
        CpsAtom::Value(value) => literals
            .get(value)
            .is_some_and(|(g, value)| *g == grain && value.len(grain) == 0),
        _ => false,
    };

    let mut changed = false;
    for (&tip, &(tip_node, grain, ref tip_base, ref tip_elem, tip_next)) in &appends {
        let consumed_as_base = appends
            .values()
            .any(|(_, g, base, ..)| *g == grain && *base == CpsAtom::Value(tip));
        if interior(&tip, grain) && consumed_as_base {
            continue;
        }

        let mut chain = Vec::new();
        let mut elems = vec![tip_elem.clone()];
        let mut root = tip_base.clone();
        while let CpsAtom::Value(value) = &root
            && interior(value, grain)
        {
            let (node, _, base, elem, _) = &appends[value];
            chain.push((*node, *value));
            elems.push(elem.clone());
            root = base.clone();
        }
        elems.reverse();

        let rooted_empty = is_empty_literal(&root, grain);
        if !rooted_empty && elems.len() < 2 {
            continue;
        }

        let chunk = CpsIntrinsic::BinChunk(grain, elems.len());
        if rooted_empty {
            module.nodes.set(
                tip_node,
                CpsNode::LetIntrinsic {
                    result: tip,
                    op: chunk,
                    args: elems,
                    next: tip_next,
                },
            );
        } else {
            let chunk_result = module.add_value(None);
            let concat = module.add_node(CpsNode::LetIntrinsic {
                result: tip,
                op: CpsIntrinsic::BinConcat(grain, 2),
                args: vec![root, CpsAtom::Value(chunk_result)],
                next: tip_next,
            });
            module.nodes.set(
                tip_node,
                CpsNode::LetIntrinsic {
                    result: chunk_result,
                    op: chunk,
                    args: elems,
                    next: concat,
                },
            );
        }

        // The interior appends are dead once the tip stops reading them, and dead-binding elimination declines `Allocates` ops, so the chain splices its own nodes out.
        let redirect = chain
            .iter()
            .map(|&(node, value)| {
                let (_, _, _, _, next) = appends[&value];
                (node, next)
            })
            .collect();
        splice_dead_nodes(module, &redirect);
        for (node, value) in chain {
            module.nodes.remove(node);
            module.values.remove(value);
        }
        changed = true;
    }
    changed
}

/// The kinds of piece a flattened construction tree contributes: a whole list operand, or a single element an append wrote.
enum FlatPiece {
    List(CpsAtom),
    Elem(CpsAtom),
}

/// Collect the maximal unshared construction tree under `atom`: a single-use concat contributes its operands' trees in order, a single-use append its base's tree then its element, and anything else — shared, literal, or not a construction — stands as a whole operand. `consumed` receives the tree's own nodes, which the caller splices out.
fn collect_flat_tree(
    atom: &CpsAtom,
    counts: &BTreeMap<CpsValueId, usize>,
    concats: &BTreeMap<CpsValueId, (CpsNodeId, Vec<CpsAtom>, CpsNodeId)>,
    appends: &BTreeMap<CpsValueId, (CpsNodeId, CpsAtom, CpsAtom, CpsNodeId)>,
    consumed: &mut Vec<(CpsNodeId, CpsValueId, CpsNodeId)>,
    out: &mut Vec<FlatPiece>,
) {
    if let CpsAtom::Value(value) = atom
        && counts.get(value).copied().unwrap_or(0) == 1
    {
        if let Some((node, args, next)) = concats.get(value) {
            consumed.push((*node, *value, *next));
            for arg in args {
                collect_flat_tree(arg, counts, concats, appends, consumed, out);
            }
            return;
        }
        if let Some((node, base, elem, next)) = appends.get(value) {
            consumed.push((*node, *value, *next));
            collect_flat_tree(base, counts, concats, appends, consumed, out);
            out.push(FlatPiece::Elem(elem.clone()));
            return;
        }
    }
    out.push(FlatPiece::List(atom.clone()));
}

/// Turn the collected pieces into `ListFlat` operands, interning each run of appended elements as one list literal. Returns the operands and the literal bindings the rewrite chains in front.
fn flat_operands(
    module: &mut CpsModule,
    pieces: Vec<FlatPiece>,
) -> (Vec<CpsAtom>, Vec<(CpsValueId, Vec<CpsAtom>)>) {
    let mut operands = Vec::new();
    let mut literals = Vec::new();
    let mut run: Vec<CpsAtom> = Vec::new();
    let flush = |run: &mut Vec<CpsAtom>,
                 operands: &mut Vec<CpsAtom>,
                 literals: &mut Vec<(CpsValueId, Vec<CpsAtom>)>,
                 module: &mut CpsModule| {
        if !run.is_empty() {
            let value = module.add_value(None);
            literals.push((value, std::mem::take(run)));
            operands.push(CpsAtom::Value(value));
        }
    };
    for piece in pieces {
        match piece {
            FlatPiece::List(atom) => {
                flush(&mut run, &mut operands, &mut literals, module);
                operands.push(atom);
            }
            FlatPiece::Elem(atom) => run.push(atom),
        }
    }
    flush(&mut run, &mut operands, &mut literals, module);
    (operands, literals)
}

/// Rewrite the node at `site` into the literal bindings followed by a `ListFlat` binding `result`, keeping `site`'s identity as the chain's head so every incoming edge stays valid, and splice the consumed tree out.
fn install_flat(
    module: &mut CpsModule,
    site: CpsNodeId,
    result: CpsValueId,
    next: CpsNodeId,
    pieces: Vec<FlatPiece>,
    consumed: Vec<(CpsNodeId, CpsValueId, CpsNodeId)>,
) {
    let (operands, literals) = flat_operands(module, pieces);
    let mut tail = module.add_node(CpsNode::LetIntrinsic {
        result,
        op: CpsIntrinsic::ListFlat(operands.len()),
        args: operands,
        next,
    });
    let mut literals = literals.into_iter();
    let head = literals.next();
    for (value, elems) in literals.rev() {
        tail = module.add_node(CpsNode::LetValue {
            result: value,
            value: CpsValueExpr::List(elems),
            next: tail,
        });
    }
    match head {
        Some((value, elems)) => module.nodes.set(
            site,
            CpsNode::LetValue {
                result: value,
                value: CpsValueExpr::List(elems),
                next: tail,
            },
        ),
        None => {
            // No literal to head the chain: the tail node's content moves into `site` itself.
            let node = module
                .node(tail)
                .cloned()
                .expect("the flat node was just added");
            module.nodes.set(site, node);
            module.nodes.remove(tail);
        }
    }

    let redirect = consumed
        .iter()
        .map(|&(node, _, next)| (node, next))
        .collect();
    splice_dead_nodes(module, &redirect);
    for (node, value, _) in consumed {
        module.nodes.remove(node);
        module.values.remove(value);
    }
}

/// Flatten the list constructions whose reads are already in evidence, so the values a program only ever indexes are flat at birth instead of node-rooted with a gather on first read. Two admissions and no others — the demand route's rules, per the map-wall spec's list-half refinement. A settle (inserted by the door on stores into census-marked fields) over a statically flat value forwards the value, and over an unshared construction tree becomes the tree's one exact flat build. A construction whose own demand is `Indexed` — every use an element, length, window, or settle, interprocedurally — builds flat likewise, since its reads would have paid the gather anyway. Growth-shaped consumption is untouched, which is what keeps the builder and patchwork idioms at their O(1) steps.
pub(super) fn flatten_indexed_lists(module: &mut CpsModule) -> bool {
    let mut changed = false;

    let collect = |module: &CpsModule| {
        let mut concats = BTreeMap::new();
        let mut appends = BTreeMap::new();
        let mut flat = BTreeSet::new();
        for (id, node) in module.nodes.iter_live() {
            match node {
                CpsNode::LetIntrinsic {
                    result,
                    op: CpsIntrinsic::ListConcat(_),
                    args,
                    next,
                } => {
                    concats.insert(*result, (id, args.clone(), *next));
                }
                CpsNode::LetIntrinsic {
                    result,
                    op: CpsIntrinsic::ListAppend,
                    args,
                    next,
                } => {
                    appends.insert(*result, (id, args[0].clone(), args[1].clone(), *next));
                }
                CpsNode::LetIntrinsic {
                    result,
                    op: CpsIntrinsic::ListFlat(_) | CpsIntrinsic::ListSettle,
                    ..
                }
                | CpsNode::LetValue {
                    result,
                    value: CpsValueExpr::List(_),
                    ..
                } => {
                    flat.insert(*result);
                }
                _ => {}
            }
        }
        (concats, appends, flat)
    };

    // Settle sites first. Each is re-read at its turn, so a settle-of-settle chain resolves in any order.
    let settle_sites: Vec<CpsNodeId> = module
        .nodes
        .iter_live()
        .filter_map(|(id, node)| {
            matches!(
                node,
                CpsNode::LetIntrinsic {
                    op: CpsIntrinsic::ListSettle,
                    ..
                }
            )
            .then_some(id)
        })
        .collect();
    for site in settle_sites {
        let Some(CpsNode::LetIntrinsic {
            result,
            op: CpsIntrinsic::ListSettle,
            args,
            next,
        }) = module.node(site).cloned()
        else {
            continue;
        };
        let counts = module.value_use_counts();
        let (concats, appends, flat) = collect(module);
        let operand = args[0].clone();
        match &operand {
            CpsAtom::Value(value) if flat.contains(value) => {
                rewrite_atoms(module, &BTreeMap::from([(result, operand.clone())]));
                rewire_node(module, site, next);
                module.nodes.remove(site);
                module.values.remove(result);
                changed = true;
            }
            CpsAtom::Value(value)
                if counts.get(value).copied().unwrap_or(0) == 1
                    && (concats.contains_key(value) || appends.contains_key(value)) =>
            {
                let mut consumed = Vec::new();
                let mut pieces = Vec::new();
                collect_flat_tree(
                    &operand,
                    &counts,
                    &concats,
                    &appends,
                    &mut consumed,
                    &mut pieces,
                );
                install_flat(module, site, result, next, pieces, consumed);
                changed = true;
            }
            _ => {}
        }
    }

    // Then the demand rule, over what remains.
    let demands = super::demand::demands(module);
    let roots: Vec<CpsNodeId> = module
        .nodes
        .iter_live()
        .filter_map(|(id, node)| match node {
            CpsNode::LetIntrinsic {
                result,
                op: CpsIntrinsic::ListConcat(_) | CpsIntrinsic::ListAppend,
                ..
            } if demand_of(&demands, *result) == Demand::Indexed => Some(id),
            _ => None,
        })
        .collect();
    for site in roots {
        let Some(CpsNode::LetIntrinsic {
            result,
            op,
            args,
            next,
        }) = module.node(site).cloned()
        else {
            continue;
        };
        let counts = module.value_use_counts();
        let (concats, appends, _) = collect(module);
        let mut consumed = Vec::new();
        let mut pieces = Vec::new();
        match op {
            CpsIntrinsic::ListConcat(_) => {
                for arg in &args {
                    collect_flat_tree(arg, &counts, &concats, &appends, &mut consumed, &mut pieces);
                }
            }
            CpsIntrinsic::ListAppend => {
                collect_flat_tree(
                    &args[0],
                    &counts,
                    &concats,
                    &appends,
                    &mut consumed,
                    &mut pieces,
                );
                pieces.push(FlatPiece::Elem(args[1].clone()));
            }
            _ => continue,
        }
        install_flat(module, site, result, next, pieces, consumed);
        changed = true;
    }

    changed
}

/// Forward every projection of a visible construction to the field it reads, in one sweep.
///
/// One snapshot of the module's constructions admits every forwardable projection; the replacements are then collapsed through each other, as `known_values` collapses its substitutions, so a projection of a construction whose field is itself a forwarded projection resolves to what that one forwards rather than to a value this sweep deletes. One `rewrite_atoms` walk then substitutes them all, and the dead projection nodes are spliced out in one pass. It was one projection per call — rescan, rebuild the construction map with every field vector cloned, rewrite the whole module, repeat — which `fixpoint_pass_measurements` found costing 225 ms of the fixpoint's first round on a `Toml/decode` compile, and growing as the split sweeps it cleans up after landed their projections together.
pub(super) fn forward_aggregate_projections(module: &mut CpsModule) -> bool {
    // Keyed by the vocabulary the construction was built in, so a read only ever forwards through a matching construction — a `RowGet` never folds through a structural tuple, nor a `TupleGet` through a row's.
    let mut aggregates = BTreeMap::<(CpsValueId, Option<CpsRowId>), &[CpsAtom]>::new();
    for (_, node) in module.nodes.iter_live() {
        match node {
            CpsNode::LetValue {
                result,
                value: CpsValueExpr::Tuple(fields),
                ..
            } => {
                aggregates.insert((*result, None), fields);
            }
            CpsNode::LetValue {
                result,
                value: CpsValueExpr::Row(row, fields),
                ..
            } => {
                aggregates.insert((*result, Some(*row)), fields);
            }
            _ => {}
        }
    }

    let mut forwarded = BTreeMap::<CpsValueId, CpsAtom>::new();
    let mut redirect = BTreeMap::<CpsNodeId, CpsNodeId>::new();
    for (id, node) in module.nodes.iter_live() {
        let CpsNode::LetIntrinsic {
            result,
            op,
            args,
            next,
        } = node
        else {
            continue;
        };
        let (row, field) = match op {
            CpsIntrinsic::TupleGet(field) => (None, *field),
            CpsIntrinsic::RowGet(row, field) => (Some(*row), *field),
            _ => continue,
        };
        let [CpsAtom::Value(tuple)] = args.as_slice() else {
            continue;
        };
        let Some(replacement) = aggregates
            .get(&(*tuple, row))
            .and_then(|fields| fields.get(field))
        else {
            continue;
        };
        forwarded.insert(*result, replacement.clone());
        redirect.insert(id, *next);
    }
    if forwarded.is_empty() {
        return false;
    }

    // Collapse the chains: a replacement naming a result this sweep forwards resolves to that result's own replacement. A binding cannot precede the construction it projects, so the chains are finite, and the guard mirrors `known_values` rather than trusting that.
    let results = forwarded.keys().copied().collect::<Vec<_>>();
    for result in results {
        let mut value = forwarded[&result].clone();
        let mut seen = BTreeSet::new();
        while let CpsAtom::Value(next) = value {
            if !seen.insert(next) {
                break;
            }
            let Some(replacement) = forwarded.get(&next) else {
                break;
            };
            value = replacement.clone();
        }
        forwarded.insert(result, value);
    }

    rewrite_atoms(module, &forwarded);
    splice_dead_nodes(module, &redirect);
    for &node in redirect.keys() {
        module.nodes.remove(node);
    }
    for &result in forwarded.keys() {
        module.values.remove(result);
    }
    true
}
pub(super) fn eliminate_dead_bindings(module: &mut CpsModule) -> bool {
    let mut changed = false;
    // Remove dead bindings in sweeps: count value uses once, collect every binding the snapshot proves dead, and splice them all out in a single chain-resolving pass rather than recomputing the counts and rewiring the whole module for one removal at a time. Removing a binding only ever lowers another value's use count, so a value dead in the snapshot stays dead; a binding that a removal newly exposes is collected by the next sweep.
    loop {
        let counts = module.value_use_counts();
        let mut redirect = BTreeMap::<CpsNodeId, CpsNodeId>::new();
        let mut dead_values = Vec::<CpsValueId>::new();
        for (id, node) in module.nodes.iter_live() {
            let removal = match node {
                CpsNode::LetValue { result, next, .. }
                    if counts.get(result).copied().unwrap_or(0) == 0 =>
                {
                    Some((*next, Some(*result)))
                }
                CpsNode::LetIntrinsic {
                    result, op, next, ..
                } if op.is_total() && counts.get(result).copied().unwrap_or(0) == 0 => {
                    Some((*next, Some(*result)))
                }
                CpsNode::LetFun { functions, body } if functions.is_empty() => Some((*body, None)),
                CpsNode::LetCont {
                    continuations,
                    body,
                } if continuations.is_empty() => Some((*body, None)),
                _ => None,
            };
            if let Some((successor, value)) = removal {
                redirect.insert(id, successor);
                if let Some(value) = value {
                    dead_values.push(value);
                }
            }
        }
        if redirect.is_empty() {
            break;
        }
        splice_dead_nodes(module, &redirect);
        for &node in redirect.keys() {
            module.nodes.remove(node);
        }
        for value in dead_values {
            module.values.remove(value);
        }
        changed = true;
    }
    changed
}

/// Redirect every control edge that targets a spliced-out node to the first surviving node in its chain. `redirect` maps each removed node to its immediate successor; following the chain skips runs of consecutive removed nodes, so the result is the same as rewiring one node at a time.
fn splice_dead_nodes(module: &mut CpsModule, redirect: &BTreeMap<CpsNodeId, CpsNodeId>) {
    for (_, function) in module.functions.iter_live_mut() {
        function.body = resolve_redirect(redirect, function.body);
    }
    for (_, continuation) in module.continuations.iter_live_mut() {
        continuation.body = resolve_redirect(redirect, continuation.body);
    }
    for (_, node) in module.nodes.iter_live_mut() {
        match node {
            CpsNode::LetValue { next, .. } | CpsNode::LetIntrinsic { next, .. } => {
                *next = resolve_redirect(redirect, *next);
            }
            CpsNode::LetFun { body, .. } | CpsNode::LetCont { body, .. } => {
                *body = resolve_redirect(redirect, *body);
            }
            CpsNode::ApplyFun { .. }
            | CpsNode::ApplyCont(_)
            | CpsNode::Switch { .. }
            | CpsNode::Foreign { .. }
            | CpsNode::Cell { .. }
            | CpsNode::Intrinsic { .. }
            | CpsNode::Exit { .. }
            | CpsNode::Panic(_)
            | CpsNode::Unreachable => {}
        }
    }
}

fn resolve_redirect(redirect: &BTreeMap<CpsNodeId, CpsNodeId>, mut id: CpsNodeId) -> CpsNodeId {
    while let Some(&next) = redirect.get(&id) {
        id = next;
    }
    id
}
pub(super) fn rewire_node(module: &mut CpsModule, from: CpsNodeId, to: CpsNodeId) {
    for (_, function) in module.functions.iter_live_mut() {
        if function.body == from {
            function.body = to;
        }
    }
    for (_, continuation) in module.continuations.iter_live_mut() {
        if continuation.body == from {
            continuation.body = to;
        }
    }
    for (_, node) in module.nodes.iter_live_mut() {
        match node {
            CpsNode::LetValue { next, .. } | CpsNode::LetIntrinsic { next, .. } => {
                if *next == from {
                    *next = to;
                }
            }
            CpsNode::LetFun { body, .. } | CpsNode::LetCont { body, .. } => {
                if *body == from {
                    *body = to;
                }
            }
            CpsNode::ApplyFun { .. }
            | CpsNode::ApplyCont(_)
            | CpsNode::Switch { .. }
            | CpsNode::Foreign { .. }
            | CpsNode::Cell { .. }
            | CpsNode::Intrinsic { .. }
            | CpsNode::Exit { .. }
            | CpsNode::Panic(_)
            | CpsNode::Unreachable => {}
        }
    }
}
/// Drop every entity's unread parameters, and the arguments every caller passes into them.
///
/// Deadness is read from [`super::demand`]'s lattice rather than from a use count, which is the same question asked at the bottom point of a richer order — the one whose `Projected` point a return protocol needs. The lattice defers an argument's demand to the receiving parameter, so `Unused` here reaches further than a zero use count: a value threaded only into parameters nobody reads is dead however many edges carry it, and this pass deleting such a chain is the code motion the strengthening was scheduled to cause. The deletion stays well-formed because a parameter is always removed together with the argument every incoming edge passes into it, so no occurrence survives its binding.
///
/// One snapshot of the lattice serves every entity, because removing a parameter only ever removes uses: a verdict of `Unused` cannot be falsified by an earlier removal in the same sweep, and each entity's edit touches its own parameter list and the edges or calls into it alone. It was one entity per call, which `fixpoint_pass_measurements` found firing on 44 of a `Toml/decode` compile's 45 rounds once `split_parameters` stopped setting the count — each round of every pass bought one continuation's cleanup.
pub(super) fn eliminate_dead_parameters(module: &mut CpsModule) -> bool {
    let demands = demands(module);
    let dead_value = |value: &CpsValueId| demand_of(&demands, *value) == Demand::Unused;
    let dead_indices = |params: &[CpsValueId]| {
        params
            .iter()
            .enumerate()
            .filter_map(|(index, value)| dead_value(value).then_some(index))
            .collect::<BTreeSet<_>>()
    };
    let mut changed = false;

    // Precompute the continuations used as a return target in one pass, rather than rescanning every node for each continuation.
    let return_targets = module
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
        .collect::<BTreeSet<_>>();
    let continuations = module
        .continuations
        .iter_live()
        .filter(|(id, _)| !return_targets.contains(id))
        .filter_map(|(id, definition)| {
            let dead = dead_indices(&definition.params);
            (!dead.is_empty()).then_some((id, dead))
        })
        .collect::<Vec<_>>();
    for (continuation, dead) in continuations {
        let removed = remove_parameter_indices(
            &mut module.continuations.get_mut(continuation).unwrap().params,
            &dead,
        );
        module.remove_params_from_record(continuation, &dead);
        for (_, node) in module.nodes.iter_live_mut() {
            match node {
                CpsNode::ApplyCont(edge) if edge.target == continuation => {
                    remove_parameter_indices(&mut edge.args, &dead);
                }
                CpsNode::Switch { cases, default, .. } => {
                    for edge in cases.values_mut().chain(default.iter_mut()) {
                        if edge.target == continuation {
                            remove_parameter_indices(&mut edge.args, &dead);
                        }
                    }
                }
                _ => {}
            }
        }
        // `Unused` means never *consumed*, not never occurring: the deferral arms of the demand walk leave a dropped parameter standing as a known call's argument, or in an edge into some other continuation whose receiving parameter is itself unused. Those occurrences are proven inert, so they become fillers — arity intact, and the receiving side's own dead-parameter drop erases them on a later round.
        for &value in &removed {
            module.replace_atom(CpsUseTarget::Value(value), CpsAtom::Filler);
        }
        for value in removed {
            module.values.remove(value);
        }
        changed = true;
    }

    let escaping = module
        .nodes
        .slots()
        .iter()
        .flatten()
        .flat_map(atoms)
        .filter_map(|atom| match atom {
            CpsAtom::Fun(function) => Some(*function),
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    let functions = module
        .functions
        .iter_live()
        .filter(|(id, _)| !escaping.contains(id))
        .filter_map(|(id, definition)| {
            let dead = dead_indices(&definition.params);
            (!dead.is_empty()).then_some((id, dead))
        })
        .collect::<Vec<_>>();
    for (function, dead) in functions {
        let removed = remove_parameter_indices(
            &mut module.functions.get_mut(function).unwrap().params,
            &dead,
        );
        for (_, node) in module.nodes.iter_live_mut() {
            if let CpsNode::ApplyFun {
                callee: CpsCallee::Known(callee),
                args,
                ..
            } = node
                && *callee == function
            {
                remove_parameter_indices(args, &dead);
            }
        }
        // The same inert occurrences as the continuation half above — see its comment.
        for &value in &removed {
            module.replace_atom(CpsUseTarget::Value(value), CpsAtom::Filler);
        }
        for value in removed {
            module.values.remove(value);
        }
        changed = true;
    }
    changed
}
pub(super) fn remove_parameter_indices<T>(
    values: &mut Vec<T>,
    removed: &BTreeSet<usize>,
) -> Vec<T> {
    let mut removed_values = Vec::new();
    let mut retained = Vec::with_capacity(values.len() - removed.len());
    for (index, value) in std::mem::take(values).into_iter().enumerate() {
        if removed.contains(&index) {
            removed_values.push(value);
        } else {
            retained.push(value);
        }
    }
    *values = retained;
    removed_values
}
