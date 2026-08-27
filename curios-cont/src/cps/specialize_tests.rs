//! Specializing an SCC, a call pattern, and a jump pattern, each against its clone and growth budget.

use {
    super::test_support::{
        PolymorphicLoop, has_switch, known_callee, polymorphic_loop, tagged_consumer, tagged_join,
    },
    crate::cps::{
        analysis::{function_nodes, known_values},
        optimize::{
            BRANCH_CLONE_LIMIT, BRANCH_SPECIALIZATION_GROWTH_LIMIT, SCC_CLONE_LIMIT,
            SCC_CLONE_NODE_LIMIT,
        },
        simplify::{forward_aggregate_projections, simplify_nodes},
        specialize::{specialize_call_patterns, specialize_jump_patterns, specialize_scc_calls},
    },
    crate::{
        CpsAtom, CpsCallee, CpsContinuation, CpsEdge, CpsFunction, CpsIntrinsic, CpsLiteral,
        CpsModule, CpsNode, CpsValueExpr,
    },
    std::collections::BTreeMap,
};

#[test]
fn scc_invariant_known_argument_propagates_into_recursive_member() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();

    // A trivial helper used only as an invariant first-class argument.
    let helper = module.reserve_function();
    let helper_return = module.reserve_continuation();
    let helper_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: helper_return,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
    }));
    module.define_function(
        helper,
        CpsFunction {
            debug_name: Some("helper".into()),
            params: vec![],
            return_cont: helper_return,
            body: helper_body,
        },
    );

    // loop(invariant, counter): the recursive call forwards `invariant` unchanged and replaces `counter`, so `invariant` is loop-invariant and `counter` is not.
    let loop_function = module.reserve_function();
    let loop_return = module.reserve_continuation();
    let invariant = module.add_value(Some("invariant".into()));
    let counter = module.add_value(Some("counter".into()));
    let recur = module.reserve_continuation();
    let recur_param = module.add_value(Some("recur".into()));
    let recur_body = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(loop_function),
        args: vec![CpsAtom::Value(invariant), CpsAtom::Value(recur_param)],
        return_to: loop_return,
    });
    module.define_continuation(
        recur,
        CpsContinuation {
            debug_name: Some("recur".into()),
            params: vec![recur_param],
            body: recur_body,
        },
    );
    let switch = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(counter),
        cases: BTreeMap::from([(
            0,
            CpsEdge {
                target: loop_return,
                args: vec![CpsAtom::Value(counter)],
            },
        )]),
        default: Some(CpsEdge {
            target: recur,
            args: vec![CpsAtom::Value(counter)],
        }),
    });
    let loop_body = module.add_node(CpsNode::LetCont {
        continuations: vec![recur],
        body: switch,
    });
    module.define_function(
        loop_function,
        CpsFunction {
            debug_name: Some("loop".into()),
            params: vec![invariant, counter],
            return_cont: loop_return,
            body: loop_body,
        },
    );

    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(loop_function),
        args: vec![CpsAtom::Fun(helper), CpsAtom::Literal(CpsLiteral::Nat(3))],
        return_to: entry_return,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![loop_function, helper],
        body: call,
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

    let known = known_values(&module);
    assert_eq!(
        known.get(&invariant),
        Some(&CpsAtom::Fun(helper)),
        "the invariant recursive parameter is recognized as the known function"
    );
    assert!(
        !known.contains_key(&counter),
        "the varying recursive parameter stays unknown"
    );
}

#[test]
fn specializes_a_polymorphic_recursive_scc_per_call_context() {
    let PolymorphicLoop {
        mut module,
        call1,
        call2,
        loop_fn,
    } = polymorphic_loop(true, 0);

    let mut budget = SCC_CLONE_LIMIT;
    assert!(
        specialize_scc_calls(&mut module, &mut budget),
        "a disagreeing call context is specialized"
    );
    assert_eq!(budget, SCC_CLONE_LIMIT - 1, "one clone consumed the budget");
    module.verify().unwrap();

    let first = known_callee(&module, call1);
    let second = known_callee(&module, call2);
    assert_ne!(
        first, second,
        "the two contexts now call different functions"
    );
    assert!(
        first == loop_fn || second == loop_fn,
        "one context keeps the original polymorphic function"
    );
    assert!(
        first != loop_fn || second != loop_fn,
        "one context is repointed to a fresh clone"
    );
}

#[test]
fn agreeing_call_contexts_are_not_specialized() {
    // Both sites pass `add`, so the module-wide analysis already knows the argument and cloning would add nothing.
    let PolymorphicLoop {
        mut module,
        call1,
        call2,
        loop_fn,
    } = polymorphic_loop(false, 0);

    let mut budget = SCC_CLONE_LIMIT;
    assert!(
        !specialize_scc_calls(&mut module, &mut budget),
        "no clone is made when callers agree"
    );
    assert_eq!(budget, SCC_CLONE_LIMIT);
    assert_eq!(known_callee(&module, call1), loop_fn);
    assert_eq!(known_callee(&module, call2), loop_fn);
}

#[test]
fn specialization_respects_the_clone_budget() {
    let PolymorphicLoop {
        mut module,
        call1,
        call2,
        loop_fn,
    } = polymorphic_loop(true, 0);

    let mut budget = 0;
    assert!(
        !specialize_scc_calls(&mut module, &mut budget),
        "an exhausted budget makes no clone"
    );
    assert_eq!(known_callee(&module, call1), loop_fn);
    assert_eq!(known_callee(&module, call2), loop_fn);
}

#[test]
fn specialization_respects_the_node_budget() {
    // Inflate `loop` past SCC_CLONE_NODE_LIMIT live nodes.
    let PolymorphicLoop {
        mut module,
        call1,
        call2,
        loop_fn,
    } = polymorphic_loop(true, SCC_CLONE_NODE_LIMIT + 1);

    let mut budget = SCC_CLONE_LIMIT;
    assert!(
        !specialize_scc_calls(&mut module, &mut budget),
        "an oversized SCC is not cloned"
    );
    assert_eq!(budget, SCC_CLONE_LIMIT);
    assert_eq!(known_callee(&module, call1), loop_fn);
    assert_eq!(known_callee(&module, call2), loop_fn);
}

#[test]
fn specialization_is_deterministic() {
    let run = || {
        let PolymorphicLoop {
            mut module,
            call1,
            call2,
            ..
        } = polymorphic_loop(true, 0);
        let mut budget = SCC_CLONE_LIMIT;
        specialize_scc_calls(&mut module, &mut budget);
        (
            known_callee(&module, call1).0,
            known_callee(&module, call2).0,
        )
    };
    assert_eq!(run(), run(), "specialization output is a pure function");
}

#[test]
fn specializes_a_constructor_argument_and_collapses_the_switch() {
    let (mut module, calls, consume) = tagged_consumer(0, &[0]);
    let mut budget = BRANCH_CLONE_LIMIT;
    assert!(
        specialize_call_patterns(&mut module, &mut budget),
        "a known tagged-tuple argument is specialized"
    );
    assert_eq!(
        budget,
        BRANCH_CLONE_LIMIT - 1,
        "one clone consumed the budget"
    );
    module.verify().unwrap();

    let clone = known_callee(&module, calls[0]);
    assert_ne!(clone, consume, "the call is repointed to a fresh clone");
    assert!(
        has_switch(&module, consume),
        "the general function keeps its switch"
    );

    // The rebuilt constructor lets the existing folds collapse the switch.
    while forward_aggregate_projections(&mut module) | simplify_nodes(&mut module) {}
    assert!(
        !has_switch(&module, clone),
        "projection and known-switch folding collapse the clone's dispatch"
    );
    module.verify().unwrap();
}

#[test]
fn equivalent_constructor_sites_share_one_clone() {
    // Two tag-0 sites match one pattern; a tag-1 site is a different pattern.
    let (mut module, calls, consume) = tagged_consumer(0, &[0, 0, 1]);
    let before = module.functions().iter().flatten().count();
    let mut budget = BRANCH_CLONE_LIMIT;
    assert!(specialize_call_patterns(&mut module, &mut budget));
    module.verify().unwrap();

    let clone = known_callee(&module, calls[0]);
    assert_ne!(clone, consume);
    assert_eq!(
        known_callee(&module, calls[1]),
        clone,
        "an equivalent site reuses the one clone"
    );
    assert_eq!(
        known_callee(&module, calls[2]),
        consume,
        "a non-matching pattern keeps the original function"
    );
    let after = module.functions().iter().flatten().count();
    assert_eq!(after, before + 1, "exactly one clone is created");
}

#[test]
fn call_pattern_specialization_respects_the_growth_budget() {
    // Inflate `consume` past BRANCH_SPECIALIZATION_GROWTH_LIMIT live nodes.
    let (mut module, calls, consume) =
        tagged_consumer(BRANCH_SPECIALIZATION_GROWTH_LIMIT + 1, &[0]);
    let mut budget = BRANCH_CLONE_LIMIT;
    assert!(
        !specialize_call_patterns(&mut module, &mut budget),
        "an oversized callee is not specialized"
    );
    assert_eq!(budget, BRANCH_CLONE_LIMIT);
    assert_eq!(known_callee(&module, calls[0]), consume);
}

#[test]
fn call_pattern_specialization_respects_the_clone_budget() {
    let (mut module, calls, consume) = tagged_consumer(0, &[0]);
    let mut budget = 0;
    assert!(
        !specialize_call_patterns(&mut module, &mut budget),
        "an exhausted budget makes no clone"
    );
    assert_eq!(known_callee(&module, calls[0]), consume);
}

#[test]
fn call_pattern_specialization_is_deterministic() {
    let run = || {
        let (mut module, calls, _) = tagged_consumer(0, &[0, 0]);
        let mut budget = BRANCH_CLONE_LIMIT;
        specialize_call_patterns(&mut module, &mut budget);
        (
            known_callee(&module, calls[0]).0,
            known_callee(&module, calls[1]).0,
        )
    };
    assert_eq!(run(), run(), "specialization output is a pure function");
}

#[test]
fn specialization_peels_a_recursive_callee_into_the_general_function() {
    // consume(t): leaf returns the field; node recurses on the child.
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();

    let consume = module.reserve_function();
    let consume_return = module.reserve_continuation();
    let t = module.add_value(Some("t".into()));
    let tag = module.add_value(Some("tag".into()));
    let child = module.add_value(Some("child".into()));
    let leaf = module.reserve_continuation();
    let node = module.reserve_continuation();
    let leaf_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: consume_return,
        args: vec![CpsAtom::Value(child)],
    }));
    module.define_continuation(
        leaf,
        CpsContinuation {
            debug_name: Some("leaf".into()),
            params: vec![],
            body: leaf_body,
        },
    );
    let node_body = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(consume),
        args: vec![CpsAtom::Value(child)],
        return_to: consume_return,
    });
    module.define_continuation(
        node,
        CpsContinuation {
            debug_name: Some("node".into()),
            params: vec![],
            body: node_body,
        },
    );
    let switch = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(tag),
        cases: BTreeMap::from([(
            0,
            CpsEdge {
                target: leaf,
                args: vec![],
            },
        )]),
        default: Some(CpsEdge {
            target: node,
            args: vec![],
        }),
    });
    let scope = module.add_node(CpsNode::LetCont {
        continuations: vec![leaf, node],
        body: switch,
    });
    let project_child = module.add_node(CpsNode::LetIntrinsic {
        result: child,
        op: CpsIntrinsic::TupleGet(1),
        args: vec![CpsAtom::Value(t)],
        next: scope,
    });
    let project_tag = module.add_node(CpsNode::LetIntrinsic {
        result: tag,
        op: CpsIntrinsic::TupleGet(0),
        args: vec![CpsAtom::Value(t)],
        next: project_child,
    });
    module.define_function(
        consume,
        CpsFunction {
            debug_name: Some("consume".into()),
            params: vec![t],
            return_cont: consume_return,
            body: project_tag,
        },
    );

    let root = module.add_value(Some("root".into()));
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(consume),
        args: vec![CpsAtom::Value(root)],
        return_to: entry_return,
    });
    let ctor = module.add_node(CpsNode::LetValue {
        result: root,
        value: CpsValueExpr::Tuple(vec![
            CpsAtom::Literal(CpsLiteral::Nat(0)),
            CpsAtom::Literal(CpsLiteral::Nat(5)),
        ]),
        next: call,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![consume],
        body: ctor,
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

    let mut budget = BRANCH_CLONE_LIMIT;
    assert!(specialize_call_patterns(&mut module, &mut budget));
    module.verify().unwrap();

    let clone = known_callee(&module, call);
    assert_ne!(clone, consume);
    let recursive_target =
        function_nodes(&module, clone)
            .into_iter()
            .find_map(|id| match module.node(id) {
                Some(CpsNode::ApplyFun {
                    callee: CpsCallee::Known(target),
                    ..
                }) => Some(*target),
                _ => None,
            });
    assert_eq!(
        recursive_target,
        Some(consume),
        "the clone peels one level and recurses into the general function"
    );
}

#[test]
fn jump_specialization_threads_a_known_tag_edge_through_its_join() {
    let (mut module, join, some_jump, none_jump, x) = tagged_join();
    let mut budget = 4;

    assert!(specialize_jump_patterns(&mut module, &mut budget));
    // The some-edge repoints to a clone carrying the payload directly; the none-edge is a different (tag, arity) pattern and waits its turn.
    let Some(CpsNode::ApplyCont(some_edge)) = module.node(some_jump) else {
        panic!("some jump survives as a jump")
    };
    assert_ne!(some_edge.target, join);
    assert_eq!(some_edge.args, vec![CpsAtom::Value(x)]);
    let Some(CpsNode::ApplyCont(none_edge)) = module.node(none_jump) else {
        panic!("none jump survives as a jump")
    };
    assert_eq!(none_edge.target, join);
    module.verify().unwrap();

    // Repointing the some-edge left the join single-transfer, which is the inliner's territory: a second invocation deliberately finds nothing.
    assert!(!specialize_jump_patterns(&mut module, &mut budget));
    module.verify().unwrap();
    assert_eq!(budget, 3);
}

#[test]
fn jump_specialization_respects_its_budget() {
    let (mut module, _, _, _, _) = tagged_join();
    let mut budget = 0;
    assert!(!specialize_jump_patterns(&mut module, &mut budget));
}
