//! Module builders the CPS suites share, so a test states only the flow it is about.
//!
//! Every file here hand-assembles a `CpsModule` node by node — there is no builder in the product to lean on, the way the Ersd suites lean on `ErsdBuilder` — and before this module each of them re-derived the same closing move: reserve a function, reserve its return continuation, define it as `main`, make it the entry. [`module_with`] is that move, and the rest are the shaped fixtures more than one suite reaches for.
//!
//! `pub(super)` rather than private: these are consumed by sibling modules across `cps`, and nothing outside it.

use {
    crate::cps::analysis::function_nodes,
    crate::{
        CpsAtom, CpsCallee, CpsContId, CpsContinuation, CpsEdge, CpsFunId, CpsFunction,
        CpsIntrinsic, CpsLiteral, CpsModule, CpsNode, CpsNodeId, CpsValueExpr, CpsValueId,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// One function whose body is `body_of(&mut module)`, so each test states only the flow it is about.
pub(super) fn module_with(body_of: impl FnOnce(&mut CpsModule) -> CpsNodeId) -> CpsModule {
    let mut module = CpsModule::default();
    let body = body_of(&mut module);
    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();
    module.define_function(
        function,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
        },
    );
    module.set_entry(function);
    module
}

pub(super) fn call_graph(edges: &[(u32, &[u32])]) -> BTreeMap<CpsFunId, BTreeSet<CpsFunId>> {
    edges
        .iter()
        .map(|(function, successors)| {
            (
                CpsFunId(*function),
                successors.iter().map(|s| CpsFunId(*s)).collect(),
            )
        })
        .collect()
}

pub(super) struct PolymorphicLoop {
    pub(super) module: CpsModule,
    pub(super) call1: CpsNodeId,
    pub(super) call2: CpsNodeId,
    pub(super) loop_fn: CpsFunId,
}

/// Build `loop(op, n)` which indirectly calls `op(n)` and recurses forwarding `op`, called from `entry` as `loop(add, 3)` then `loop(second, 4)`. When `second` differs from `add` the two contexts disagree. `padding` prepends dead `LetIntrinsic` nodes to `loop`'s body to inflate its node count.
pub(super) fn polymorphic_loop(second_is_mul: bool, padding: usize) -> PolymorphicLoop {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();

    let trivial = |module: &mut CpsModule, name: &str| {
        let function = module.reserve_function();
        let function_return = module.reserve_continuation();
        let param = module.add_value(Some(format!("{name} x")));
        let function_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: function_return,
            args: vec![CpsAtom::Value(param)],
        }));
        module.define_function(
            function,
            CpsFunction {
                debug_name: Some(name.into()),
                params: vec![param],
                return_cont: function_return,
                body: function_body,
            },
        );
        function
    };
    let add = trivial(&mut module, "add");
    let mul = trivial(&mut module, "mul");

    let loop_fn = module.reserve_function();
    let loop_return = module.reserve_continuation();
    let op = module.add_value(Some("op".into()));
    let n = module.add_value(Some("n".into()));
    let after = module.reserve_continuation();
    let after_r = module.add_value(Some("after r".into()));
    let after_body = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(loop_fn),
        args: vec![CpsAtom::Value(op), CpsAtom::Value(after_r)],
        return_to: loop_return,
    });
    module.define_continuation(
        after,
        CpsContinuation {
            debug_name: Some("after".into()),
            params: vec![after_r],
            body: after_body,
        },
    );
    let recur = module.reserve_continuation();
    let recur_m = module.add_value(Some("recur m".into()));
    let recur_body = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Closure(op),
        args: vec![CpsAtom::Value(recur_m)],
        return_to: after,
    });
    module.define_continuation(
        recur,
        CpsContinuation {
            debug_name: Some("recur".into()),
            params: vec![recur_m],
            body: recur_body,
        },
    );
    let switch = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(n),
        cases: BTreeMap::from([(
            0,
            CpsEdge {
                target: loop_return,
                args: vec![CpsAtom::Value(n)],
            },
        )]),
        default: Some(CpsEdge {
            target: recur,
            args: vec![CpsAtom::Value(n)],
        }),
    });
    let scope = module.add_node(CpsNode::LetCont {
        continuations: vec![recur, after],
        body: switch,
    });
    let mut loop_body = scope;
    for _ in 0..padding {
        let dead = module.add_value(None);
        loop_body = module.add_node(CpsNode::LetIntrinsic {
            result: dead,
            op: CpsIntrinsic::NatAdd,
            args: vec![
                CpsAtom::Literal(CpsLiteral::Nat(0)),
                CpsAtom::Literal(CpsLiteral::Nat(0)),
            ],
            next: loop_body,
        });
    }
    module.define_function(
        loop_fn,
        CpsFunction {
            debug_name: Some("loop".into()),
            params: vec![op, n],
            return_cont: loop_return,
            body: loop_body,
        },
    );

    let second = if second_is_mul { mul } else { add };
    let x1 = module.add_value(Some("x1".into()));
    let call2 = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(loop_fn),
        args: vec![CpsAtom::Fun(second), CpsAtom::Literal(CpsLiteral::Nat(4))],
        return_to: entry_return,
    });
    let k1 = module.reserve_continuation();
    module.define_continuation(
        k1,
        CpsContinuation {
            debug_name: Some("k1".into()),
            params: vec![x1],
            body: call2,
        },
    );
    let call1 = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(loop_fn),
        args: vec![CpsAtom::Fun(add), CpsAtom::Literal(CpsLiteral::Nat(3))],
        return_to: k1,
    });
    let outer = module.add_node(CpsNode::LetCont {
        continuations: vec![k1],
        body: call1,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![loop_fn, add, mul],
        body: outer,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();
    PolymorphicLoop {
        module,
        call1,
        call2,
        loop_fn,
    }
}

pub(super) fn known_callee(module: &CpsModule, node: CpsNodeId) -> CpsFunId {
    match module.node(node).unwrap() {
        CpsNode::ApplyFun {
            callee: CpsCallee::Known(callee),
            ..
        } => *callee,
        _ => panic!("call site changed shape"),
    }
}

// Build `helper(x) = x`, non-escaping, called from `entry` at one or two external sites. Returns the module and the helper function.
pub(super) fn helper_called(two_sites: bool) -> (CpsModule, CpsFunId) {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let helper = module.reserve_function();
    let helper_return = module.reserve_continuation();
    let x = module.add_value(Some("x".into()));
    let helper_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: helper_return,
        args: vec![CpsAtom::Value(x)],
    }));
    module.define_function(
        helper,
        CpsFunction {
            debug_name: Some("helper".into()),
            params: vec![x],
            return_cont: helper_return,
            body: helper_body,
        },
    );

    let inner = if two_sites {
        let call2 = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(helper),
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
            return_to: entry_return,
        });
        let param = module.add_value(None);
        let bridge = module.reserve_continuation();
        module.define_continuation(
            bridge,
            CpsContinuation {
                debug_name: None,
                params: vec![param],
                body: call2,
            },
        );
        let call1 = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(helper),
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
            return_to: bridge,
        });
        module.add_node(CpsNode::LetCont {
            continuations: vec![bridge],
            body: call1,
        })
    } else {
        module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(helper),
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
            return_to: entry_return,
        })
    };
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![helper],
        body: inner,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();
    (module, helper)
}

pub(super) fn tagged_consumer(
    padding: usize,
    sites: &[u32],
) -> (CpsModule, Vec<CpsNodeId>, CpsFunId) {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();

    let consume = module.reserve_function();
    let consume_return = module.reserve_continuation();
    let t = module.add_value(Some("t".into()));
    let tag = module.add_value(Some("tag".into()));
    let val = module.add_value(Some("val".into()));
    let switch = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(tag),
        cases: BTreeMap::from([
            (
                0,
                CpsEdge {
                    target: consume_return,
                    args: vec![CpsAtom::Value(val)],
                },
            ),
            (
                1,
                CpsEdge {
                    target: consume_return,
                    args: vec![CpsAtom::Literal(CpsLiteral::Nat(999))],
                },
            ),
        ]),
        default: Some(CpsEdge {
            target: consume_return,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
        }),
    });
    let project_val = module.add_node(CpsNode::LetIntrinsic {
        result: val,
        op: CpsIntrinsic::TupleGet(1),
        args: vec![CpsAtom::Value(t)],
        next: switch,
    });
    let mut consume_body = module.add_node(CpsNode::LetIntrinsic {
        result: tag,
        op: CpsIntrinsic::TupleGet(0),
        args: vec![CpsAtom::Value(t)],
        next: project_val,
    });
    for _ in 0..padding {
        let dead = module.add_value(None);
        consume_body = module.add_node(CpsNode::LetIntrinsic {
            result: dead,
            op: CpsIntrinsic::NatAdd,
            args: vec![
                CpsAtom::Literal(CpsLiteral::Nat(0)),
                CpsAtom::Literal(CpsLiteral::Nat(0)),
            ],
            next: consume_body,
        });
    }
    module.define_function(
        consume,
        CpsFunction {
            debug_name: Some("consume".into()),
            params: vec![t],
            return_cont: consume_return,
            body: consume_body,
        },
    );

    // Build the call chain forward so the search visits `sites[0]` first. Each site's return continuation is introduced by its own `LetCont`, and returning from site `i` runs site `i + 1`.
    let count = sites.len();
    let results: Vec<CpsValueId> = (0..count)
        .map(|i| module.add_value(Some(format!("r{i}"))))
        .collect();
    let ctors: Vec<CpsNodeId> = (0..count).map(|_| module.reserve_node()).collect();
    let calls: Vec<CpsNodeId> = (0..count).map(|_| module.reserve_node()).collect();
    let scopes: Vec<CpsNodeId> = (0..count).map(|_| module.reserve_node()).collect();
    let conts: Vec<CpsContId> = (0..count).map(|_| module.reserve_continuation()).collect();
    let tail = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: entry_return,
        args: vec![match results.last() {
            Some(&last) => CpsAtom::Value(last),
            None => CpsAtom::Literal(CpsLiteral::Nat(0)),
        }],
    }));
    for i in 0..count {
        let value = module.add_value(Some(format!("v{i}")));
        module.define_node(
            ctors[i],
            CpsNode::LetValue {
                result: value,
                value: CpsValueExpr::Tuple(vec![
                    CpsAtom::Literal(CpsLiteral::Nat(sites[i])),
                    CpsAtom::Literal(CpsLiteral::Nat(i as u32)),
                ]),
                next: calls[i],
            },
        );
        module.define_node(
            calls[i],
            CpsNode::ApplyFun {
                callee: CpsCallee::Known(consume),
                args: vec![CpsAtom::Value(value)],
                return_to: conts[i],
            },
        );
        module.define_node(
            scopes[i],
            CpsNode::LetCont {
                continuations: vec![conts[i]],
                body: ctors[i],
            },
        );
        let next = if i + 1 < count { scopes[i + 1] } else { tail };
        module.define_continuation(
            conts[i],
            CpsContinuation {
                debug_name: Some(format!("k{i}")),
                params: vec![results[i]],
                body: next,
            },
        );
    }
    let first = scopes.first().copied().unwrap_or(tail);
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![consume],
        body: first,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();
    (module, calls, consume)
}

pub(super) fn has_switch(module: &CpsModule, function: CpsFunId) -> bool {
    function_nodes(module, function)
        .iter()
        .any(|&id| matches!(module.node(id), Some(CpsNode::Switch { .. })))
}

/// One entry function `main(x)` binding `result = op(args)` and returning it — the smallest module exercising a single intrinsic fold.
pub(super) fn unary_intrinsic_module(
    op: CpsIntrinsic,
    args: Vec<CpsAtom>,
) -> (CpsModule, CpsNodeId) {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let x = module.add_value(Some("x".into()));
    let result = module.add_value(Some("result".into()));
    let return_node = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(result)],
    }));
    let intrinsic = module.add_node(CpsNode::LetIntrinsic {
        result,
        op,
        args,
        next: return_node,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![x],
            return_cont,
            body: intrinsic,
        },
    );
    module.set_entry(entry);
    (module, intrinsic)
}

/// `main(x, y)` binding `first = op1`, `second = op2`, then `sum = first + second`, returned — the two-occurrence chain every CSE test starts from.
pub(super) fn duplicate_pair_module(
    op1: CpsIntrinsic,
    op2: CpsIntrinsic,
    swap_second: bool,
) -> (CpsModule, CpsNodeId, CpsNodeId, CpsNodeId) {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let x = module.add_value(Some("x".into()));
    let y = module.add_value(Some("y".into()));
    let first = module.add_value(Some("first".into()));
    let second = module.add_value(Some("second".into()));
    let sum = module.add_value(Some("sum".into()));
    let return_node = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(sum)],
    }));
    let add = module.add_node(CpsNode::LetIntrinsic {
        result: sum,
        op: CpsIntrinsic::NatAdd,
        args: vec![CpsAtom::Value(first), CpsAtom::Value(second)],
        next: return_node,
    });
    let second_args = if swap_second {
        vec![CpsAtom::Value(y), CpsAtom::Value(x)]
    } else {
        vec![CpsAtom::Value(x), CpsAtom::Value(y)]
    };
    let second_node = module.add_node(CpsNode::LetIntrinsic {
        result: second,
        op: op2,
        args: second_args,
        next: add,
    });
    let first_node = module.add_node(CpsNode::LetIntrinsic {
        result: first,
        op: op1,
        args: vec![CpsAtom::Value(x), CpsAtom::Value(y)],
        next: second_node,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![x, y],
            return_cont,
            body: first_node,
        },
    );
    module.set_entry(entry);
    (module, first_node, second_node, add)
}

/// The option-join shape: `main(x)` builds `some(x)` or `none()` in two predecessors, both jumping one join continuation whose body switches on the tuple's tag — the allocation-then-rescrutinize shape jump-pattern specialization exists to collapse.
pub(super) fn tagged_join() -> (CpsModule, CpsContId, CpsNodeId, CpsNodeId, CpsValueId) {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let x = module.add_value(Some("x".into()));

    // join(p): switch TupleGet(0)[p] { 0 => return TupleGet(1)[p], 1 => return 7 }.
    let p = module.add_value(Some("p".into()));
    let tag = module.add_value(Some("tag".into()));
    let payload = module.add_value(Some("payload".into()));
    let join = module.reserve_continuation();
    let some_arm = module.reserve_continuation();
    let none_arm = module.reserve_continuation();
    let some_return = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(payload)],
    }));
    let some_body = module.add_node(CpsNode::LetIntrinsic {
        result: payload,
        op: CpsIntrinsic::TupleGet(1),
        args: vec![CpsAtom::Value(p)],
        next: some_return,
    });
    module.define_continuation(
        some_arm,
        CpsContinuation {
            debug_name: Some("some arm".into()),
            params: vec![],
            body: some_body,
        },
    );
    let none_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(7))],
    }));
    module.define_continuation(
        none_arm,
        CpsContinuation {
            debug_name: Some("none arm".into()),
            params: vec![],
            body: none_body,
        },
    );
    let dispatch = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(tag),
        cases: BTreeMap::from([
            (
                0,
                CpsEdge {
                    target: some_arm,
                    args: vec![],
                },
            ),
            (
                1,
                CpsEdge {
                    target: none_arm,
                    args: vec![],
                },
            ),
        ]),
        default: None,
    });
    let read_tag = module.add_node(CpsNode::LetIntrinsic {
        result: tag,
        op: CpsIntrinsic::TupleGet(0),
        args: vec![CpsAtom::Value(p)],
        next: dispatch,
    });
    let join_body = module.add_node(CpsNode::LetCont {
        continuations: vec![some_arm, none_arm],
        body: read_tag,
    });
    module.define_continuation(
        join,
        CpsContinuation {
            debug_name: Some("join".into()),
            params: vec![p],
            body: join_body,
        },
    );

    // Predecessors: build some(x) / none() and jump the join.
    let some_value = module.add_value(Some("some value".into()));
    let none_value = module.add_value(Some("none value".into()));
    let to_some = module.reserve_continuation();
    let to_none = module.reserve_continuation();
    let some_jump = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: join,
        args: vec![CpsAtom::Value(some_value)],
    }));
    let build_some = module.add_node(CpsNode::LetValue {
        result: some_value,
        value: CpsValueExpr::Tuple(vec![
            CpsAtom::Literal(CpsLiteral::Nat(0)),
            CpsAtom::Value(x),
        ]),
        next: some_jump,
    });
    module.define_continuation(
        to_some,
        CpsContinuation {
            debug_name: Some("to some".into()),
            params: vec![],
            body: build_some,
        },
    );
    let none_jump = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: join,
        args: vec![CpsAtom::Value(none_value)],
    }));
    let build_none = module.add_node(CpsNode::LetValue {
        result: none_value,
        value: CpsValueExpr::Tuple(vec![CpsAtom::Literal(CpsLiteral::Nat(1))]),
        next: none_jump,
    });
    module.define_continuation(
        to_none,
        CpsContinuation {
            debug_name: Some("to none".into()),
            params: vec![],
            body: build_none,
        },
    );

    let pick = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(x),
        cases: BTreeMap::from([(
            0,
            CpsEdge {
                target: to_some,
                args: vec![],
            },
        )]),
        default: Some(CpsEdge {
            target: to_none,
            args: vec![],
        }),
    });
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![join, to_some, to_none],
        body: pick,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![x],
            return_cont,
            body,
        },
    );
    module.set_entry(entry);
    (module, join, some_jump, none_jump, x)
}
