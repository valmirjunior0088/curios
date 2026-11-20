use {
    crate::{
        Atom, Callee, ContinuationId, Demand, Edge, Intrinsic, Literal, Module, Node, NodeId,
        ValueExpr, ValueId, demand_of, visit_atoms_mut,
    },
    curios_num::Natural,
    std::collections::{BTreeMap, BTreeSet},
};

pub fn rewrite_atoms(module: &mut Module, known: &BTreeMap<ValueId, Atom>) -> bool {
    // The closure calls a known function cannot be a call of, found before the nodes are borrowed — see [`Module::applies`].
    let unreachable = module
        .live_nodes()
        .filter_map(|(id, node)| match node {
            Node::ApplyFun {
                callee: Callee::Closure(value),
                args,
                ..
            } => match known.get(value) {
                Some(Atom::Fun(function)) if !module.applies(*function, args.len()) => Some(id),
                _ => None,
            },
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    let mut changed = false;
    for (id, node) in module.live_nodes_mut() {
        if unreachable.contains(&id) {
            *node = Node::Unreachable;
            changed = true;
            continue;
        }

        visit_atoms_mut(node, &mut |atom| {
            if let Atom::Value(value) = atom
                && let Some(replacement) = known.get(value)
                && atom != replacement
            {
                *atom = replacement.clone();
                changed = true;
            }
        });

        // A closure callee holds its callee in a value, which `visit_atoms_mut` does not reach. Remap it here: a known function devirtualizes the call, and a forwarded value (e.g. a projected constructor field) keeps the callee pointing at a live value rather than a deleted one.
        if let Node::ApplyFun { callee, .. } = node
            && let Callee::Closure(value) = *callee
        {
            match known.get(&value) {
                Some(Atom::Fun(function)) => {
                    *callee = Callee::Known(*function);
                    changed = true;
                }
                Some(Atom::Value(replacement)) if *replacement != value => {
                    *callee = Callee::Closure(*replacement);
                    changed = true;
                }
                _ => {}
            }
        }
    }
    module.substitute_claims(known) || changed
}
pub fn forward_continuations(module: &mut Module) -> bool {
    let forwarding = module
        .live_continuations()
        .filter_map(|(id, continuation)| {
            let Node::ApplyCont(edge) = module.node(continuation.body)? else {
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
                    .all(|(param, arg)| arg == &Atom::Value(*param)))
            .then_some((continuation, edge.target))
        })
        .collect::<BTreeMap<_, _>>();
    let resolve_identity = |original: ContinuationId| {
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
    for (_, node) in module.live_nodes_mut() {
        match node {
            Node::ApplyCont(edge) => {
                thread_edge(edge, &forwarding, &mut changed);
            }
            Node::Switch { cases, default, .. } => {
                for edge in cases.values_mut().chain(default.iter_mut()) {
                    thread_edge(edge, &forwarding, &mut changed);
                }
            }
            Node::ApplyFun { return_to, .. }
            | Node::Foreign { return_to, .. }
            | Node::Cell { return_to, .. }
            | Node::Channel { return_to, .. }
            | Node::Intrinsic { return_to, .. } => {
                retarget(return_to, &resolve_identity, &mut changed);
            }
            _ => {}
        }
    }
    changed
}
pub fn thread_edge(
    edge: &mut Edge,
    forwarding: &BTreeMap<ContinuationId, (Vec<ValueId>, Edge)>,
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
        replacement = Edge {
            target: outgoing.target,
            args: outgoing
                .args
                .iter()
                .map(|arg| match arg {
                    Atom::Value(value) => substitutions
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
/// A jump that hands a known function to a continuation whose whole body calls it, made that call at the jump.
///
/// This is the shape a convoy leaves, a match applied to the evidence its arms take, `match s | … end(evidence)`: each arm is a function of the evidence it carries, and the match's value is applied to the evidence after it, so once erasure has dropped the evidence each arm binds a nullary function and jumps it to a join that does nothing but call it. Left alone, every arm allocates that function's environment and the join calls it indirectly. Called where it is known instead, each arm's function has one direct call and no escaping use, and inlining consumes it, so a convoy costs nothing at run time.
///
/// Sound because the continuation's body is one node and everything it reads is in scope at each jump into it: its parameters are the jump's arguments, and anything else was in scope where it was defined, which every jump into it lies within, since a continuation is local to its function. The call keeps the body's arguments, with the jump's substituted for the parameters, and the body's return continuation, so its arity and protocol are unchanged. Only a jump handing over a known function is forwarded: a callee that is still a value gains nothing from moving and would only copy the node.
pub fn forward_calls(module: &mut Module) -> bool {
    let calling = module
        .live_continuations()
        .filter_map(|(id, continuation)| {
            let Node::ApplyFun {
                callee: Callee::Closure(callee),
                args,
                return_to,
            } = module.node(continuation.body)?
            else {
                return None;
            };
            let position = continuation
                .params
                .iter()
                .position(|param| param == callee)?;

            Some((
                id,
                (
                    continuation.params.clone(),
                    position,
                    args.clone(),
                    *return_to,
                ),
            ))
        })
        .collect::<BTreeMap<_, _>>();
    if calling.is_empty() {
        return false;
    }

    // The jumps whose function cannot be called as the continuation calls it — see [`Module::applies`].
    let unreachable = module
        .live_nodes()
        .filter_map(|(id, node)| {
            let Node::ApplyCont(edge) = node else {
                return None;
            };
            let (params, position, args, _) = calling.get(&edge.target)?;
            if params.len() != edge.args.len() {
                return None;
            }
            let Atom::Fun(function) = edge.args[*position] else {
                return None;
            };

            (!module.applies(function, args.len())).then_some(id)
        })
        .collect::<BTreeSet<_>>();

    let mut changed = false;
    for (id, node) in module.live_nodes_mut() {
        if unreachable.contains(&id) {
            *node = Node::Unreachable;
            changed = true;
            continue;
        }
        let Node::ApplyCont(edge) = node else {
            continue;
        };
        let Some((params, position, args, return_to)) = calling.get(&edge.target) else {
            continue;
        };
        if params.len() != edge.args.len() {
            continue;
        }
        let Atom::Fun(function) = edge.args[*position] else {
            continue;
        };
        let substituted = args
            .iter()
            .map(|arg| match arg {
                Atom::Value(value) => params
                    .iter()
                    .position(|param| param == value)
                    .map_or_else(|| arg.clone(), |index| edge.args[index].clone()),
                _ => arg.clone(),
            })
            .collect();

        *node = Node::ApplyFun {
            callee: Callee::Known(function),
            args: substituted,
            return_to: *return_to,
        };
        changed = true;
    }
    changed
}
pub fn retarget(
    target: &mut ContinuationId,
    resolve: &impl Fn(ContinuationId) -> ContinuationId,
    changed: &mut bool,
) {
    let replacement = resolve(*target);
    if replacement != *target {
        *target = replacement;
        *changed = true;
    }
}
/// Fuse a chain of packed appends into one flat chunk build. A literal with non-constant atoms lowers to appends onto whatever precedes it — the free monoid's honest spelling — and each append allocates a one-element leaf and a node the first read then gathers. Where the chain is local and unshared, the elements build one exact flat leaf instead: `BytesChunk` alone when the chain is rooted at the empty packed value, or the root concatenated with the chunk otherwise. Only an intermediate append nothing else reads may fuse — a shared intermediate is a value the program observes — and a lone append onto a non-empty root stays as written, since a one-element chunk beside a concat node buys back exactly what it costs.
pub fn fuse_append_chains(module: &mut Module) -> bool {
    let counts = module.value_use_counts();

    // Every packed append by its result — node, base, element, successor — and every packed literal binding, so a chain rooted at an interned empty is recognized.
    let mut appends = BTreeMap::new();
    let mut literals = BTreeMap::new();
    for (id, node) in module.live_nodes() {
        match node {
            Node::LetIntrinsic {
                result,
                op: Intrinsic::BytesAppend,
                args,
                next,
            } => {
                appends.insert(*result, (id, args[0].clone(), args[1].clone(), *next));
            }
            Node::LetValue {
                result,
                value: ValueExpr::Literal(Literal::Bytes(value)),
                ..
            } => {
                literals.insert(*result, value.clone());
            }
            _ => {}
        }
    }

    // An interior link is an append result whose one use is the base of an append; a chain is walked from each tip — an append that is no interior link — down through interior links to its root atom.
    let interior = |value: &ValueId| {
        counts.get(value).copied().unwrap_or(0) == 1 && appends.contains_key(value)
    };
    let is_empty_literal = |atom: &Atom| match atom {
        Atom::Literal(Literal::Bytes(value)) => value.is_empty(),
        Atom::Value(value) => literals.get(value).is_some_and(|value| value.is_empty()),
        _ => false,
    };

    let mut changed = false;
    for (&tip, &(tip_node, ref tip_base, ref tip_elem, tip_next)) in &appends {
        let consumed_as_base = appends
            .values()
            .any(|(_, base, ..)| *base == Atom::Value(tip));
        if interior(&tip) && consumed_as_base {
            continue;
        }

        let mut chain = Vec::new();
        let mut elems = vec![tip_elem.clone()];
        let mut root = tip_base.clone();
        while let Atom::Value(value) = &root
            && interior(value)
        {
            let (node, base, elem, _) = &appends[value];
            chain.push((*node, *value));
            elems.push(elem.clone());
            root = base.clone();
        }
        elems.reverse();

        let rooted_empty = is_empty_literal(&root);
        if !rooted_empty && elems.len() < 2 {
            continue;
        }

        let chunk = Intrinsic::BytesChunk(elems.len());
        if rooted_empty {
            module.set_node(
                tip_node,
                Node::LetIntrinsic {
                    result: tip,
                    op: chunk,
                    args: elems,
                    next: tip_next,
                },
            );
        } else {
            let chunk_result = module.add_value(None);
            let concat = module.add_node(Node::LetIntrinsic {
                result: tip,
                op: Intrinsic::BytesConcat(2),
                args: vec![root, Atom::Value(chunk_result)],
                next: tip_next,
            });
            module.set_node(
                tip_node,
                Node::LetIntrinsic {
                    result: chunk_result,
                    op: chunk,
                    args: elems,
                    next: concat,
                },
            );
        }

        // The interior appends are dead once the tip stops reading them, and the chain splices its own nodes out here, in the pass that rewired them, rather than leave a dead binding for dead-binding elimination.
        let redirect = chain
            .iter()
            .map(|&(node, value)| {
                let (_, _, _, next) = appends[&value];
                (node, next)
            })
            .collect();
        splice_dead_nodes(module, &redirect);
        for (node, value) in chain {
            module.remove_node(node);
            module.remove_value(value);
        }
        changed = true;
    }
    changed
}

/// The kinds of piece a flattened construction tree contributes: a whole list operand, or a single element an append wrote.
enum FlatPiece {
    List(Atom),
    Elem(Atom),
}

/// Collect the maximal unshared construction tree under `atom`: a single-use concat contributes its operands' trees in order, a single-use append its base's tree then its element, and anything else — shared, literal, or not a construction — stands as a whole operand. `consumed` receives the tree's own nodes, which the caller splices out.
fn collect_flat_tree(
    atom: &Atom,
    counts: &BTreeMap<ValueId, usize>,
    concats: &BTreeMap<ValueId, (NodeId, Vec<Atom>, NodeId)>,
    appends: &BTreeMap<ValueId, (NodeId, Atom, Atom, NodeId)>,
    consumed: &mut Vec<(NodeId, ValueId, NodeId)>,
    out: &mut Vec<FlatPiece>,
) {
    if let Atom::Value(value) = atom
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
    module: &mut Module,
    pieces: Vec<FlatPiece>,
) -> (Vec<Atom>, Vec<(ValueId, Vec<Atom>)>) {
    let mut operands = Vec::new();
    let mut literals = Vec::new();
    let mut run: Vec<Atom> = Vec::new();
    let flush = |run: &mut Vec<Atom>,
                 operands: &mut Vec<Atom>,
                 literals: &mut Vec<(ValueId, Vec<Atom>)>,
                 module: &mut Module| {
        if !run.is_empty() {
            let value = module.add_value(None);
            literals.push((value, std::mem::take(run)));
            operands.push(Atom::Value(value));
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
    module: &mut Module,
    site: NodeId,
    result: ValueId,
    next: NodeId,
    pieces: Vec<FlatPiece>,
    consumed: Vec<(NodeId, ValueId, NodeId)>,
) {
    let (operands, literals) = flat_operands(module, pieces);
    let mut tail = module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::ListFlat(operands.len()),
        args: operands,
        next,
    });
    let mut literals = literals.into_iter();
    let head = literals.next();
    for (value, elems) in literals.rev() {
        tail = module.add_node(Node::LetValue {
            result: value,
            value: ValueExpr::List(elems),
            next: tail,
        });
    }
    match head {
        Some((value, elems)) => module.set_node(
            site,
            Node::LetValue {
                result: value,
                value: ValueExpr::List(elems),
                next: tail,
            },
        ),
        None => {
            // No literal to head the chain: the tail node's content moves into `site` itself.
            let node = module
                .node(tail)
                .cloned()
                .expect("the flat node was just added");
            module.set_node(site, node);
            module.remove_node(tail);
        }
    }

    let redirect = consumed
        .iter()
        .map(|&(node, _, next)| (node, next))
        .collect();
    splice_dead_nodes(module, &redirect);
    for (node, value, _) in consumed {
        module.remove_node(node);
        module.remove_value(value);
    }
}

/// Flatten the list constructions whose reads are already in evidence, so the values a program only ever indexes are flat at birth instead of node-rooted with a gather on first read. Two admissions and no others — the demand route's rules, per the map-wall spec's list-half refinement. A settle (inserted by the door on stores into census-marked fields) over a statically flat value forwards the value, and over an unshared construction tree becomes the tree's one exact flat build. A construction whose own demand is `Indexed` — every use an element, length, window, or settle, interprocedurally — builds flat likewise, since its reads would have paid the gather anyway. Growth-shaped consumption is untouched, which is what keeps the builder and patchwork idioms at their O(1) steps.
pub fn flatten_indexed_lists(module: &mut Module) -> bool {
    let mut changed = false;

    let collect = |module: &Module| {
        let mut concats = BTreeMap::new();
        let mut appends = BTreeMap::new();
        let mut flat = BTreeSet::new();
        for (id, node) in module.live_nodes() {
            match node {
                Node::LetIntrinsic {
                    result,
                    op: Intrinsic::ListConcat(_),
                    args,
                    next,
                } => {
                    concats.insert(*result, (id, args.clone(), *next));
                }
                Node::LetIntrinsic {
                    result,
                    op: Intrinsic::ListAppend,
                    args,
                    next,
                } => {
                    appends.insert(*result, (id, args[0].clone(), args[1].clone(), *next));
                }
                Node::LetIntrinsic {
                    result,
                    op: Intrinsic::ListFlat(_) | Intrinsic::ListSettle,
                    ..
                }
                | Node::LetValue {
                    result,
                    value: ValueExpr::List(_),
                    ..
                } => {
                    flat.insert(*result);
                }
                _ => {}
            }
        }
        (concats, appends, flat)
    };

    // Settle sites first. Each is read against the module as the sites before it left it, so a settle-of-settle chain resolves in any order; the module is read again only once a rewrite has moved it, since a site that rewrote nothing left it as the last read saw it.
    let settle_sites: Vec<NodeId> = module
        .live_nodes()
        .filter_map(|(id, node)| {
            matches!(
                node,
                Node::LetIntrinsic {
                    op: Intrinsic::ListSettle,
                    ..
                }
            )
            .then_some(id)
        })
        .collect();
    let mut standing = None;
    for site in settle_sites {
        let Some(Node::LetIntrinsic {
            result,
            op: Intrinsic::ListSettle,
            args,
            next,
        }) = module.node(site).cloned()
        else {
            continue;
        };
        let (counts, (concats, appends, flat)) =
            standing.get_or_insert_with(|| (module.value_use_counts(), collect(module)));
        let operand = args[0].clone();
        match &operand {
            Atom::Value(value) if flat.contains(value) => {
                rewrite_atoms(module, &BTreeMap::from([(result, operand.clone())]));
                rewire_node(module, site, next);
                module.remove_node(site);
                module.remove_value(result);
                changed = true;
                standing = None;
            }
            Atom::Value(value)
                if counts.get(value).copied().unwrap_or(0) == 1
                    && (concats.contains_key(value) || appends.contains_key(value)) =>
            {
                let mut consumed = Vec::new();
                let mut pieces = Vec::new();
                collect_flat_tree(
                    &operand,
                    counts,
                    concats,
                    appends,
                    &mut consumed,
                    &mut pieces,
                );
                install_flat(module, site, result, next, pieces, consumed);
                changed = true;
                standing = None;
            }
            _ => {}
        }
    }

    // Then the demand rule, over what remains.
    let demands = module.demands();
    let roots: Vec<NodeId> = module
        .live_nodes()
        .filter_map(|(id, node)| match node {
            Node::LetIntrinsic {
                result,
                op: Intrinsic::ListConcat(_) | Intrinsic::ListAppend,
                ..
            } if demand_of(&demands, *result) == Demand::Indexed => Some(id),
            _ => None,
        })
        .collect();
    let mut standing = None;
    for site in roots {
        let Some(Node::LetIntrinsic {
            result,
            op,
            args,
            next,
        }) = module.node(site).cloned()
        else {
            continue;
        };
        let (counts, (concats, appends, _)) =
            standing.get_or_insert_with(|| (module.value_use_counts(), collect(module)));
        let mut consumed = Vec::new();
        let mut pieces = Vec::new();
        match op {
            Intrinsic::ListConcat(_) => {
                for arg in &args {
                    collect_flat_tree(arg, counts, concats, appends, &mut consumed, &mut pieces);
                }
            }
            Intrinsic::ListAppend => {
                collect_flat_tree(
                    &args[0],
                    counts,
                    concats,
                    appends,
                    &mut consumed,
                    &mut pieces,
                );
                pieces.push(FlatPiece::Elem(args[1].clone()));
            }
            _ => continue,
        }
        install_flat(module, site, result, next, pieces, consumed);
        changed = true;
        standing = None;
    }

    changed
}

/// Answer no for every row test whose operand no flow reaching it builds that row for ([`ValueFlow::rows`](crate::ValueFlow::rows)), in one sweep.
///
/// A module with no row test pays nothing: the analysis is whole-module, so it runs only where a test stands for it to answer.
///
/// It answers in every round rather than only where the round's other passes moved nothing. A test the first rounds' inlining makes answerable is then answered while the rounds that settle what it exposes run anyway; answered once the rest had settled, it would start those rounds again, three or more of every pass for one answer, which costs a program whose test answers more than the solves it spares a program whose tests never do.
pub fn answer_row_tests(module: &mut Module) -> bool {
    let asks = module.live_nodes().any(|(_, node)| {
        matches!(
            node,
            Node::LetIntrinsic {
                op: Intrinsic::IsRow(_),
                ..
            }
        )
    });
    if !asks {
        return false;
    }

    let flow = module.value_flow();
    let mut answered = BTreeMap::<ValueId, Atom>::new();
    let mut redirect = BTreeMap::<NodeId, NodeId>::new();
    for (id, node) in module.live_nodes() {
        let Node::LetIntrinsic {
            result,
            op: Intrinsic::IsRow(row),
            args,
            next,
        } = node
        else {
            continue;
        };
        let [Atom::Value(value)] = args.as_slice() else {
            continue;
        };
        if flow.rows(*value).is_some_and(|rows| !rows.may_be(*row)) {
            answered.insert(*result, Atom::Literal(Literal::Nat(Natural::zero())));
            redirect.insert(id, *next);
        }
    }
    if answered.is_empty() {
        return false;
    }

    rewrite_atoms(module, &answered);
    splice_dead_nodes(module, &redirect);
    for &node in redirect.keys() {
        module.remove_node(node);
    }
    for &result in answered.keys() {
        module.remove_value(result);
    }
    true
}

/// Delete a known call only when its carried proof permits omission and every result is unread. A bodyless return sentinel has no local result demand, so a tail call is kept.
///
/// The jump retains the resume's arity with fillers: the emitter supplies null or the receiving register's zero, and no result reads either. Taking one demand snapshot is sufficient because deleting these calls can only remove demands; newly dead results are found in the next optimizer round.
pub fn eliminate_dead_calls(module: &mut Module) -> bool {
    let demands = module.demands();
    let mut replacements = Vec::new();
    for (id, node) in module.live_nodes() {
        let Node::ApplyFun {
            callee: Callee::Known(callee),
            args,
            return_to,
        } = node
        else {
            continue;
        };
        if !module.may_drop_call(*callee, args) {
            continue;
        }
        let Some(resume) = module.continuation(*return_to) else {
            continue;
        };
        if resume
            .params
            .iter()
            .all(|result| demand_of(&demands, *result) == Demand::Unused)
        {
            replacements.push((
                id,
                Edge {
                    target: *return_to,
                    args: vec![Atom::Filler; resume.params.len()],
                },
            ));
        }
    }
    let changed = !replacements.is_empty();
    for (id, edge) in replacements {
        module.set_node(id, Node::ApplyCont(edge));
    }
    changed
}

/// Drop a loop nothing reads the answer of: a continuation marked terminating — the loop contification made of a recursion the checkers proved total ([`Continuation::terminates`](crate::Continuation::terminates)) — entered from outside only by jumps, where every way out of the loop is one continuation none of whose parameters is read and the loop's body, as the optimizer has left it, does nothing a run observes ([`DeadLoop`]). Each jump in goes to that exit instead, its arity kept with fillers as [`eliminate_dead_calls`] keeps a dropped call's, and what the loop no longer reaches is pruned. Termination is the checkers' verdict, carried with the code, and what the body does is judged here, on the code that would run, as LLVM's loop deletion reads a loop `mustprogress` marks; the body is admitted under the rule a dead call's claim is (`droppable_functions`), a trap among what it may do.
pub fn eliminate_dead_loops(module: &mut Module) -> bool {
    let demands = module.demands();
    let mut replacements = Vec::new();
    for (id, continuation) in module.live_continuations() {
        if !continuation.terminates {
            continue;
        }
        let Some(dead) = DeadLoop::of(module, id) else {
            continue;
        };
        let Some(resume) = module.continuation(dead.exit) else {
            continue;
        };
        if !resume
            .params
            .iter()
            .all(|result| demand_of(&demands, *result) == Demand::Unused)
        {
            continue;
        }
        let mut entries = Vec::new();
        let mut entered_otherwise = false;
        for (node, entering) in module.live_nodes() {
            if dead.region.contains(&node) {
                continue;
            }
            match entering {
                Node::ApplyCont(edge) if edge.target == id => entries.push(node),
                Node::Switch { cases, default, .. } => {
                    entered_otherwise |=
                        cases.values().chain(default).any(|edge| edge.target == id);
                }
                Node::ApplyFun { return_to, .. }
                | Node::Foreign { return_to, .. }
                | Node::Cell { return_to, .. }
                | Node::Channel { return_to, .. }
                | Node::Intrinsic { return_to, .. } => entered_otherwise |= *return_to == id,
                _ => {}
            }
        }
        if entered_otherwise {
            continue;
        }
        let fillers = vec![Atom::Filler; resume.params.len()];
        replacements.extend(entries.into_iter().map(|node| {
            let edge = Edge {
                target: dead.exit,
                args: fillers.clone(),
            };
            (node, edge)
        }));
    }
    let changed = !replacements.is_empty();
    for (id, edge) in replacements {
        module.set_node(id, Node::ApplyCont(edge));
    }
    changed
}

/// A loop whose body observes nothing, as [`eliminate_dead_loops`] reads one: the one continuation every way out of it goes to, and the nodes of its body.
///
/// The body is read node by node: a construction, any operation bound by a `let` — one partial in the language among them, its trap ruled out by the proof a total program holds for it and owed to no program that never runs it — a known call [`Module::may_drop_call`] admits, a jump within the loop or out to its exit, a switch, and nested definitions. A call through a closure, a foreign or host row, a cell, a channel, an effectful intrinsic call and a halt each observe. Every cycle the body's continuations make is closed by a back edge of one marked terminating, a transfer into it from itself or from a continuation its body nests: a loop inside it that is no total recursion's — a partial callee contification put there — may never end, though each of its rounds enter a total loop and resume at its exit, and its absence is what dropping the loop would observe.
struct DeadLoop {
    exit: ContinuationId,
    region: BTreeSet<NodeId>,
}

impl DeadLoop {
    /// The loop entering at `entry`, where its body observes nothing and leaves one way: `None` where it observes something, goes out two ways, or leaves to a function's return.
    fn of(module: &Module, entry: ContinuationId) -> Option<Self> {
        let mut walk = LoopWalk {
            module,
            inside: BTreeSet::from([entry]),
            entered: BTreeSet::from([entry]),
            pending: vec![(module.continuation(entry)?.body, entry)],
            region: BTreeSet::new(),
            exit: None,
            edges: BTreeMap::new(),
            binders: BTreeMap::new(),
            at: entry,
        };
        while let Some((at, owner)) = walk.pending.pop() {
            walk.at = owner;
            if !walk.region.insert(at) {
                continue;
            }
            match module.node(at)? {
                Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. } => {
                    walk.pending.push((*next, owner))
                }
                Node::LetFun { body, .. } => walk.pending.push((*body, owner)),
                Node::LetCont {
                    continuations,
                    body,
                } => {
                    walk.inside.extend(continuations.iter().copied());
                    walk.binders.extend(
                        continuations
                            .iter()
                            .map(|continuation| (*continuation, owner)),
                    );
                    walk.pending.push((*body, owner));
                }
                Node::ApplyFun {
                    callee: Callee::Known(callee),
                    args,
                    return_to,
                } => {
                    if !module.may_drop_call(*callee, args) {
                        return None;
                    }
                    walk.enter(*return_to)?;
                }
                Node::ApplyCont(edge) => walk.enter(edge.target)?,
                Node::Switch { cases, default, .. } => {
                    for edge in cases.values().chain(default) {
                        walk.enter(edge.target)?;
                    }
                }
                Node::Panic(_) | Node::Unreachable => {}
                Node::ApplyFun {
                    callee: Callee::Closure(_),
                    ..
                }
                | Node::Foreign { .. }
                | Node::Cell { .. }
                | Node::Channel { .. }
                | Node::Intrinsic { .. }
                | Node::Halt { .. } => return None,
            }
        }
        let exit = walk.exit?;
        module.continuation(exit)?;
        if walk.cycles_past_terminating_back_edges() {
            return None;
        }
        Some(DeadLoop {
            exit,
            region: walk.region,
        })
    }
}

/// The walk [`DeadLoop::of`] makes: the continuations the loop's body defines, the ones it has entered, the nodes still to read, each with the continuation whose body it stands in, and read, the one continuation outside it a way out has reached, the transfers between the loop's own continuations, from the one whose body is being read, and the continuation whose body binds each the loop's body defines.
struct LoopWalk<'m> {
    module: &'m Module,
    inside: BTreeSet<ContinuationId>,
    entered: BTreeSet<ContinuationId>,
    pending: Vec<(NodeId, ContinuationId)>,
    region: BTreeSet<NodeId>,
    exit: Option<ContinuationId>,
    edges: BTreeMap<ContinuationId, BTreeSet<ContinuationId>>,
    binders: BTreeMap<ContinuationId, ContinuationId>,
    at: ContinuationId,
}

impl LoopWalk<'_> {
    /// A transfer to `target`: into the loop's own continuations, read once each, or out to the one exit; `None` at a second.
    fn enter(&mut self, target: ContinuationId) -> Option<()> {
        if self.inside.contains(&target) {
            self.edges.entry(self.at).or_default().insert(target);
            if self.entered.insert(target) {
                self.pending
                    .push((self.module.continuation(target)?.body, target));
            }
            return Some(());
        }
        match self.exit {
            None => {
                self.exit = Some(target);
                Some(())
            }
            Some(only) if only == target => Some(()),
            Some(_) => None,
        }
    }

    /// Whether the loop's continuations make a cycle no back edge of one marked terminating closes: one standing once each marked continuation's own back edges, the transfers into it from itself or from a continuation its body nests, are taken out, found by a depth-first walk over the rest. A cycle that passes through a marked loop, entering it and resuming at its exit, is the loop's around it, which the walk keeps.
    fn cycles_past_terminating_back_edges(&self) -> bool {
        // Whether `from` is `to` or a continuation `to`'s body nests, by the `LetCont`s the walk read.
        let nested = |from: ContinuationId, to: ContinuationId| {
            let mut at = Some(from);
            while let Some(continuation) = at {
                if continuation == to {
                    return true;
                }
                at = self.binders.get(&continuation).copied();
            }
            false
        };
        let kept = |from: ContinuationId, to: &ContinuationId| {
            !(self
                .module
                .continuation(*to)
                .is_some_and(|definition| definition.terminates)
                && nested(from, *to))
        };
        let mut finished = BTreeSet::new();
        let mut open = BTreeSet::new();
        for start in &self.entered {
            if finished.contains(start) {
                continue;
            }
            let mut stack = vec![(*start, false)];
            while let Some((continuation, left)) = stack.pop() {
                if left {
                    open.remove(&continuation);
                    finished.insert(continuation);
                    continue;
                }
                if finished.contains(&continuation) {
                    continue;
                }
                if !open.insert(continuation) {
                    return true;
                }
                stack.push((continuation, true));
                for next in self.edges.get(&continuation).into_iter().flatten() {
                    if !kept(continuation, next) || finished.contains(next) {
                        continue;
                    }
                    if open.contains(next) {
                        return true;
                    }
                    stack.push((*next, false));
                }
            }
        }
        false
    }
}

/// Redirect every control edge that targets a spliced-out node to the first surviving node in its chain. `redirect` maps each removed node to its immediate successor; following the chain skips runs of consecutive removed nodes, so the result is the same as rewiring one node at a time.
fn splice_dead_nodes(module: &mut Module, redirect: &BTreeMap<NodeId, NodeId>) {
    for (_, function) in module.live_functions_mut() {
        function.body = resolve_redirect(redirect, function.body);
    }
    for (_, continuation) in module.live_continuations_mut() {
        continuation.body = resolve_redirect(redirect, continuation.body);
    }
    for (_, node) in module.live_nodes_mut() {
        match node {
            Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. } => {
                *next = resolve_redirect(redirect, *next);
            }
            Node::LetFun { body, .. } | Node::LetCont { body, .. } => {
                *body = resolve_redirect(redirect, *body);
            }
            Node::ApplyFun { .. }
            | Node::ApplyCont(_)
            | Node::Switch { .. }
            | Node::Foreign { .. }
            | Node::Cell { .. }
            | Node::Channel { .. }
            | Node::Intrinsic { .. }
            | Node::Halt { .. }
            | Node::Panic(_)
            | Node::Unreachable => {}
        }
    }
}

fn resolve_redirect(redirect: &BTreeMap<NodeId, NodeId>, mut id: NodeId) -> NodeId {
    while let Some(&next) = redirect.get(&id) {
        id = next;
    }
    id
}
pub fn rewire_node(module: &mut Module, from: NodeId, to: NodeId) {
    for (_, function) in module.live_functions_mut() {
        if function.body == from {
            function.body = to;
        }
    }
    for (_, continuation) in module.live_continuations_mut() {
        if continuation.body == from {
            continuation.body = to;
        }
    }
    for (_, node) in module.live_nodes_mut() {
        match node {
            Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. } => {
                if *next == from {
                    *next = to;
                }
            }
            Node::LetFun { body, .. } | Node::LetCont { body, .. } => {
                if *body == from {
                    *body = to;
                }
            }
            Node::ApplyFun { .. }
            | Node::ApplyCont(_)
            | Node::Switch { .. }
            | Node::Foreign { .. }
            | Node::Cell { .. }
            | Node::Channel { .. }
            | Node::Intrinsic { .. }
            | Node::Halt { .. }
            | Node::Panic(_)
            | Node::Unreachable => {}
        }
    }
}

#[cfg(test)]
mod tests;
