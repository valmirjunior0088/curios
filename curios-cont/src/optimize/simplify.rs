use {
    crate::{
        Atom, Callee, ContinuationId, Demand, Edge, Intrinsic, Literal, Module, Node, NodeId,
        RowId, UseTarget, ValueExpr, ValueId, atoms, demand_of, demands, evaluate, visit_atoms_mut,
    },
    curios_num::{Grain, Integer, Natural},
    std::collections::{BTreeMap, BTreeSet},
};

pub(crate) fn rewrite_atoms(module: &mut Module, known: &BTreeMap<ValueId, Atom>) -> bool {
    let mut changed = false;
    for (_, node) in module.live_nodes_mut() {
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
    changed
}
pub(crate) fn forward_continuations(module: &mut Module) -> bool {
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
pub(crate) fn thread_edge(
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
/// This is the shape a convoy leaves. `match s: (x) => (e: E(x)) -> R | … end(evidence)` elaborates each arm to a function of the evidence it carries and applies the match's value to the evidence after it, so once erasure has dropped the evidence each arm binds a nullary function and jumps it to a join that does nothing but call it. Left alone, every arm allocates that function's environment and the join calls it indirectly. Called where it is known instead, each arm's function has one direct call and no escaping use, and inlining consumes it, so a convoy costs nothing at run time.
///
/// Sound because the continuation's body is one node and everything it reads is in scope at each jump into it: its parameters are the jump's arguments, and anything else was in scope where it was defined, which every jump into it lies within, since a continuation is local to its function. The call keeps the body's arguments, with the jump's substituted for the parameters, and the body's return continuation, so its arity and protocol are unchanged. Only a jump handing over a known function is forwarded: a callee that is still a value gains nothing from moving and would only copy the node.
pub(crate) fn forward_calls(module: &mut Module) -> bool {
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

    let mut changed = false;
    for (_, node) in module.live_nodes_mut() {
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
pub(crate) fn retarget(
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
pub(crate) fn simplify_nodes(module: &mut Module) -> bool {
    let mut changed = false;
    for (_, node) in module.live_nodes_mut() {
        match node {
            Node::LetIntrinsic {
                result,
                op,
                args,
                next,
            } => {
                if let Some(literal) = evaluate(*op, args) {
                    *node = Node::LetValue {
                        result: *result,
                        value: ValueExpr::Literal(literal),
                        next: *next,
                    };
                    changed = true;
                }
            }
            Node::Switch {
                scrutinee: Atom::Literal(Literal::Nat(tag)),
                cases,
                default,
            } => {
                if let Some(edge) = u32::try_from(&*tag)
                    .ok()
                    .and_then(|tag| cases.get(&tag))
                    .or(default.as_ref())
                    .cloned()
                {
                    *node = Node::ApplyCont(edge);
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
    Operand(Atom),
    Literal(Literal),
}

/// Match one `Nat`/`Int` identity or absorption law on a binary intrinsic with a literal neutral or absorbing operand: `x + 0`, `x - 0`, `x * 1`, `x * 0`, `x / 1`, `x % 1`, `x & 0`, `x | 0`, `x ^ 0`, and shifts by zero.
///
/// Trap discipline: `Nat` and `Int` arithmetic is unbounded and `nat_sub` is monus, so the only trap among these operations is a zero divisor, which a `/ 1` or `% 1` can never be. Every fold here returns either an operand that is already a live value or a literal, so no trap is added or dropped. `Flt` deliberately has no laws here: `x + 0.0` is not the identity on `-0.0`.
fn identity_fold(op: Intrinsic, args: &[Atom]) -> Option<IdentityFold> {
    let [left, right] = args else { return None };
    // The carriers are unbounded, so a law tests a literal against a value rather than reading a machine scalar out of it.
    let nat = |atom: &Atom, expected: u32| matches!(atom, Atom::Literal(Literal::Nat(value)) if *value == Natural::from(expected));
    let int = |atom: &Atom, expected: i32| matches!(atom, Atom::Literal(Literal::Int(value)) if *value == Integer::from(expected));
    let operand = |atom: &Atom| Some(IdentityFold::Operand(atom.clone()));

    match op {
        Intrinsic::NatAdd | Intrinsic::NatOr | Intrinsic::NatXor => {
            if nat(right, 0) {
                operand(left)
            } else if nat(left, 0) {
                operand(right)
            } else {
                None
            }
        }
        Intrinsic::IntAdd | Intrinsic::IntOr | Intrinsic::IntXor => {
            if int(right, 0) {
                operand(left)
            } else if int(left, 0) {
                operand(right)
            } else {
                None
            }
        }
        // A shift count is a `Nat` on both carriers.
        Intrinsic::NatSub
        | Intrinsic::NatShl
        | Intrinsic::NatShr
        | Intrinsic::IntShl
        | Intrinsic::IntShr => (nat(right, 0)).then(|| operand(left)).flatten(),
        Intrinsic::IntSub => (int(right, 0)).then(|| operand(left)).flatten(),
        Intrinsic::NatMul => {
            if nat(right, 1) {
                operand(left)
            } else if nat(left, 1) {
                operand(right)
            } else if nat(right, 0) || nat(left, 0) {
                Some(IdentityFold::Literal(Literal::Nat(Natural::zero())))
            } else {
                None
            }
        }
        Intrinsic::IntMul => {
            if int(right, 1) {
                operand(left)
            } else if int(left, 1) {
                operand(right)
            } else if int(right, 0) || int(left, 0) {
                Some(IdentityFold::Literal(Literal::Int(Integer::from(0u32))))
            } else {
                None
            }
        }
        Intrinsic::NatDiv => (nat(right, 1)).then(|| operand(left)).flatten(),
        Intrinsic::IntDiv => (int(right, 1)).then(|| operand(left)).flatten(),
        Intrinsic::NatRem => {
            (nat(right, 1)).then_some(IdentityFold::Literal(Literal::Nat(Natural::zero())))
        }
        Intrinsic::IntRem => {
            (int(right, 1)).then_some(IdentityFold::Literal(Literal::Int(Integer::from(0u32))))
        }
        Intrinsic::NatAnd => (nat(right, 0) || nat(left, 0))
            .then_some(IdentityFold::Literal(Literal::Nat(Natural::zero()))),
        Intrinsic::IntAnd => (int(right, 0) || int(left, 0))
            .then_some(IdentityFold::Literal(Literal::Int(Integer::from(0u32)))),
        _ => None,
    }
}

/// Fold intrinsic identity and absorption laws with one literal operand, which all-literal folding (`evaluate`) cannot reach. An operand fold forwards the surviving value and deletes the binding; an absorption fold pins the result as a literal in place.
pub(crate) fn fold_intrinsic_identities(module: &mut Module) -> bool {
    let mut changed = false;
    loop {
        let selected = module.live_nodes().find_map(|(id, node)| {
            let Node::LetIntrinsic {
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
                module.remove_node(node);
                module.remove_value(result);
            }
            IdentityFold::Literal(literal) => {
                module.set_node(
                    node,
                    Node::LetValue {
                        result,
                        value: ValueExpr::Literal(literal),
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
pub(crate) fn fuse_append_chains(module: &mut Module) -> bool {
    let counts = module.value_use_counts();

    // Every packed append by its result — node, grain, base, element, successor — and every packed literal binding, so a chain rooted at an interned empty is recognized.
    let mut appends = BTreeMap::new();
    let mut literals = BTreeMap::new();
    for (id, node) in module.live_nodes() {
        match node {
            Node::LetIntrinsic {
                result,
                op: Intrinsic::BinAppend(grain),
                args,
                next,
            } => {
                appends.insert(
                    *result,
                    (id, *grain, args[0].clone(), args[1].clone(), *next),
                );
            }
            Node::LetValue {
                result,
                value: ValueExpr::Literal(Literal::Bin(grain, value)),
                ..
            } => {
                literals.insert(*result, (*grain, value.clone()));
            }
            _ => {}
        }
    }

    // An interior link is an append result whose one use is the base of a same-grain append; a chain is walked from each tip — an append that is no interior link — down through interior links to its root atom.
    let interior = |value: &ValueId, grain: Grain| {
        counts.get(value).copied().unwrap_or(0) == 1
            && appends.get(value).is_some_and(|(_, g, ..)| *g == grain)
    };
    let is_empty_literal = |atom: &Atom, grain: Grain| match atom {
        Atom::Literal(Literal::Bin(g, value)) => *g == grain && value.len(grain) == 0,
        Atom::Value(value) => literals
            .get(value)
            .is_some_and(|(g, value)| *g == grain && value.len(grain) == 0),
        _ => false,
    };

    let mut changed = false;
    for (&tip, &(tip_node, grain, ref tip_base, ref tip_elem, tip_next)) in &appends {
        let consumed_as_base = appends
            .values()
            .any(|(_, g, base, ..)| *g == grain && *base == Atom::Value(tip));
        if interior(&tip, grain) && consumed_as_base {
            continue;
        }

        let mut chain = Vec::new();
        let mut elems = vec![tip_elem.clone()];
        let mut root = tip_base.clone();
        while let Atom::Value(value) = &root
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

        let chunk = Intrinsic::BinChunk(grain, elems.len());
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
                op: Intrinsic::BinConcat(grain, 2),
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
pub(crate) fn flatten_indexed_lists(module: &mut Module) -> bool {
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

    // Settle sites first. Each is re-read at its turn, so a settle-of-settle chain resolves in any order.
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
        let counts = module.value_use_counts();
        let (concats, appends, flat) = collect(module);
        let operand = args[0].clone();
        match &operand {
            Atom::Value(value) if flat.contains(value) => {
                rewrite_atoms(module, &BTreeMap::from([(result, operand.clone())]));
                rewire_node(module, site, next);
                module.remove_node(site);
                module.remove_value(result);
                changed = true;
            }
            Atom::Value(value)
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
    let demands = demands(module);
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
        let counts = module.value_use_counts();
        let (concats, appends, _) = collect(module);
        let mut consumed = Vec::new();
        let mut pieces = Vec::new();
        match op {
            Intrinsic::ListConcat(_) => {
                for arg in &args {
                    collect_flat_tree(arg, &counts, &concats, &appends, &mut consumed, &mut pieces);
                }
            }
            Intrinsic::ListAppend => {
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
/// One snapshot of the module's constructions admits every forwardable projection; the replacements are then collapsed through each other, as `known_values` collapses its substitutions, so a projection of a construction whose field is itself a forwarded projection resolves to what that one forwards rather than to a value this sweep deletes. One `rewrite_atoms` walk then substitutes them all, and the dead projection nodes are spliced out in one pass. One projection per call — rescan, rebuild the construction map with every field vector cloned, rewrite the whole module, repeat — would cost a large share of the fixpoint's first round on a `Toml/decode` compile, as `fixpoint_pass_measurements` shows, and grow with every projection a split sweep lands.
pub(crate) fn forward_aggregate_projections(module: &mut Module) -> bool {
    // Keyed by the vocabulary the construction was built in, so a read only ever forwards through a matching construction — a `RowGet` never folds through a structural tuple, nor a `TupleGet` through a row's.
    let mut aggregates = BTreeMap::<(ValueId, Option<RowId>), &[Atom]>::new();
    for (_, node) in module.live_nodes() {
        match node {
            Node::LetValue {
                result,
                value: ValueExpr::Tuple(fields),
                ..
            } => {
                aggregates.insert((*result, None), fields);
            }
            Node::LetValue {
                result,
                value: ValueExpr::Row(row, fields),
                ..
            } => {
                aggregates.insert((*result, Some(*row)), fields);
            }
            _ => {}
        }
    }

    let mut forwarded = BTreeMap::<ValueId, Atom>::new();
    let mut redirect = BTreeMap::<NodeId, NodeId>::new();
    for (id, node) in module.live_nodes() {
        let Node::LetIntrinsic {
            result,
            op,
            args,
            next,
        } = node
        else {
            continue;
        };
        let (row, field) = match op {
            Intrinsic::TupleGet(field) => (None, *field),
            Intrinsic::RowGet(row, field) => (Some(*row), *field),
            _ => continue,
        };
        let [Atom::Value(tuple)] = args.as_slice() else {
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
        while let Atom::Value(next) = value {
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
        module.remove_node(node);
    }
    for &result in forwarded.keys() {
        module.remove_value(result);
    }
    true
}
pub(crate) fn eliminate_dead_bindings(module: &mut Module) -> bool {
    let mut changed = false;
    // Remove dead bindings in sweeps: count value uses once, collect every binding the snapshot proves dead, and splice them all out in a single chain-resolving pass rather than recomputing the counts and rewiring the whole module for one removal at a time. Removing a binding only ever lowers another value's use count, so a value dead in the snapshot stays dead; a binding that a removal newly exposes is collected by the next sweep.
    loop {
        let counts = module.value_use_counts();
        let mut redirect = BTreeMap::<NodeId, NodeId>::new();
        let mut dead_values = Vec::<ValueId>::new();
        for (id, node) in module.live_nodes() {
            let removal = match node {
                Node::LetValue { result, next, .. }
                    if counts.get(result).copied().unwrap_or(0) == 0 =>
                {
                    Some((*next, Some(*result)))
                }
                Node::LetIntrinsic {
                    result, op, next, ..
                } if op.is_total() && counts.get(result).copied().unwrap_or(0) == 0 => {
                    Some((*next, Some(*result)))
                }
                Node::LetFun { functions, body } if functions.is_empty() => Some((*body, None)),
                Node::LetCont {
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
            module.remove_node(node);
        }
        for value in dead_values {
            module.remove_value(value);
        }
        changed = true;
    }
    changed
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
pub(crate) fn rewire_node(module: &mut Module, from: NodeId, to: NodeId) {
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
/// Drop every entity's unread parameters, and the arguments every caller passes into them.
///
/// Deadness is read from [`crate::demand`]'s lattice rather than from a use count, which is the same question asked at the bottom point of a richer order — the one whose `Projected` point a return protocol needs. The lattice defers an argument's demand to the receiving parameter, so `Unused` here reaches further than a zero use count: a value threaded only into parameters nobody reads is dead however many edges carry it, and this pass deletes such a chain whole. The deletion stays well-formed because a parameter is always removed together with the argument every incoming edge passes into it, so no occurrence survives its binding.
///
/// One snapshot of the lattice serves every entity, because removing a parameter only ever removes uses: a verdict of `Unused` cannot be falsified by an earlier removal in the same sweep, and each entity's edit touches its own parameter list and the edges or calls into it alone. One entity per call would fire on nearly every round of a `Toml/decode` compile, each round of every pass buying one continuation's cleanup.
pub(crate) fn eliminate_dead_parameters(module: &mut Module) -> bool {
    let demands = demands(module);
    let dead_value = |value: &ValueId| demand_of(&demands, *value) == Demand::Unused;
    let dead_indices = |params: &[ValueId]| {
        params
            .iter()
            .enumerate()
            .filter_map(|(index, value)| dead_value(value).then_some(index))
            .collect::<BTreeSet<_>>()
    };
    let mut changed = false;

    // Precompute the continuations used as a return target in one pass, rather than rescanning every node for each continuation.
    let return_targets = module
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
        .collect::<BTreeSet<_>>();
    let continuations = module
        .live_continuations()
        .filter(|(id, _)| !return_targets.contains(id))
        .filter_map(|(id, definition)| {
            let dead = dead_indices(&definition.params);
            (!dead.is_empty()).then_some((id, dead))
        })
        .collect::<Vec<_>>();
    for (continuation, dead) in continuations {
        let removed = remove_parameter_indices(
            &mut module.continuation_mut(continuation).unwrap().params,
            &dead,
        );
        module.remove_params_from_record(continuation, &dead);
        for (_, node) in module.live_nodes_mut() {
            match node {
                Node::ApplyCont(edge) if edge.target == continuation => {
                    remove_parameter_indices(&mut edge.args, &dead);
                }
                Node::Switch { cases, default, .. } => {
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
            module.replace_atom(UseTarget::Value(value), Atom::Filler);
        }
        for value in removed {
            module.remove_value(value);
        }
        changed = true;
    }

    let escaping = module
        .nodes()
        .iter()
        .flatten()
        .flat_map(atoms)
        .filter_map(|atom| match atom {
            Atom::Fun(function) => Some(*function),
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    let functions = module
        .live_functions()
        .filter(|(id, _)| !escaping.contains(id))
        .filter_map(|(id, definition)| {
            let dead = dead_indices(&definition.params);
            (!dead.is_empty()).then_some((id, dead))
        })
        .collect::<Vec<_>>();
    for (function, dead) in functions {
        let removed =
            remove_parameter_indices(&mut module.function_mut(function).unwrap().params, &dead);
        for (_, node) in module.live_nodes_mut() {
            if let Node::ApplyFun {
                callee: Callee::Known(callee),
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
            module.replace_atom(UseTarget::Value(value), Atom::Filler);
        }
        for value in removed {
            module.remove_value(value);
        }
        changed = true;
    }
    changed
}
pub(crate) fn remove_parameter_indices<T>(
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

#[cfg(test)]
mod tests;
