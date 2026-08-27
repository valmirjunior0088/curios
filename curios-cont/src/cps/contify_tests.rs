//! Turning a function into a continuation, and the call shapes that forbid it.

use {
    super::test_support::helper_called,
    crate::cps::{contify::contify_calls, optimize::optimize},
    crate::{
        CpsAtom, CpsCallee, CpsContinuation, CpsEdge, CpsFunction, CpsLiteral, CpsModule, CpsNode,
    },
    std::collections::BTreeMap,
};

#[test]
fn contifies_a_single_entry_tail_loop_and_bridges_switch_returns() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let loop_function = module.reserve_function();
    let loop_return = module.reserve_continuation();
    let loop_param = module.add_value(Some("loop argument".into()));
    let recur = module.reserve_continuation();
    let recur_param = module.add_value(Some("recur argument".into()));
    let recur_body = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(loop_function),
        args: vec![CpsAtom::Value(recur_param)],
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
        scrutinee: CpsAtom::Value(loop_param),
        cases: BTreeMap::from([(
            0,
            CpsEdge {
                target: loop_return,
                args: vec![CpsAtom::Value(loop_param)],
            },
        )]),
        default: Some(CpsEdge {
            target: recur,
            args: vec![CpsAtom::Value(loop_param)],
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
            params: vec![loop_param],
            return_cont: loop_return,
            body: loop_body,
        },
    );
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(loop_function),
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
        return_to: entry_return,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![loop_function],
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

    optimize(&mut module);

    assert!(module.function(loop_function).is_none());
    let loop_cont = module
        .continuations()
        .iter()
        .flatten()
        .find(|continuation| continuation.debug_name.as_deref() == Some("loop"))
        .unwrap();
    assert_eq!(loop_cont.params, vec![loop_param]);
    let return_bridge = module
        .continuations()
        .iter()
        .flatten()
        .find(|continuation| continuation.debug_name.as_deref() == Some("contified return"))
        .unwrap();
    assert!(matches!(
        module.node(return_bridge.body),
        Some(CpsNode::ApplyCont(CpsEdge { target, .. })) if *target == entry_return
    ));
    let CpsNode::Switch { cases, .. } = module.node(switch).unwrap() else {
        panic!("loop switch changed shape")
    };
    assert_ne!(cases[&0].target, entry_return);
    module.verify().unwrap();
}

#[test]
fn contifies_a_nonrecursive_single_call_function() {
    let (mut module, helper) = helper_called(false);
    assert!(
        contify_calls(&mut module),
        "the single-call helper is contified"
    );
    assert!(
        module.function(helper).is_none(),
        "the contified function is replaced by a local continuation"
    );
    module.verify().unwrap();
}

/// `main` calls `outer` once, `outer` calls `inner` once, and `outer` is minted first so the sweep reaches it first: by `inner`'s turn the function the snapshot names as its owner is gone, contified under `main`. One call contifies both — the owner is resolved through the sweep's own record rather than deferred a round.
#[test]
fn contifies_a_chain_of_single_call_helpers_in_one_sweep() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let outer = module.reserve_function();
    let outer_return = module.reserve_continuation();
    let inner = module.reserve_function();
    let inner_return = module.reserve_continuation();

    // inner(y) = y
    let y = module.add_value(Some("y".into()));
    let inner_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: inner_return,
        args: vec![CpsAtom::Value(y)],
    }));
    module.define_function(
        inner,
        CpsFunction {
            debug_name: Some("inner".into()),
            params: vec![y],
            return_cont: inner_return,
            body: inner_body,
        },
    );

    // outer(x) = inner(x)
    let x = module.add_value(Some("x".into()));
    let call_inner = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(inner),
        args: vec![CpsAtom::Value(x)],
        return_to: outer_return,
    });
    let outer_body = module.add_node(CpsNode::LetFun {
        functions: vec![inner],
        body: call_inner,
    });
    module.define_function(
        outer,
        CpsFunction {
            debug_name: Some("outer".into()),
            params: vec![x],
            return_cont: outer_return,
            body: outer_body,
        },
    );

    // main() = outer(0)
    let call_outer = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(outer),
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
        return_to: entry_return,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![outer],
        body: call_outer,
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

    assert!(contify_calls(&mut module), "the sweep contifies");
    module.verify().unwrap();
    assert!(
        module.function(outer).is_none() && module.function(inner).is_none(),
        "both helpers are contified in one call:\n{module}"
    );
    assert!(
        !contify_calls(&mut module),
        "and nothing is left for a second"
    );
}

#[test]
fn does_not_contify_a_multi_site_function() {
    // Two return contexts: single-site placement cannot cover both, so this is left for common-dominator contification in the machine CFG.
    let (mut module, helper) = helper_called(true);
    assert!(
        !contify_calls(&mut module),
        "a function with two call sites is not contified here"
    );
    assert!(module.function(helper).is_some());
}
