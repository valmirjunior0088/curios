//! Turning a function into a continuation, and the call shapes that forbid it.

use curios_num::Natural;

use {
    super::test_support::{capture_unmentioned_by_owner, helper_called},
    crate::cps::{contify::contify_calls, optimize::optimize},
    crate::{Atom, Callee, Continuation, Edge, Function, Literal, Module, Node},
    std::collections::BTreeMap,
};

#[test]
fn contifies_a_single_entry_tail_loop_and_bridges_switch_returns() {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let loop_function = module.reserve_function();
    let loop_return = module.reserve_continuation();
    let loop_param = module.add_value(Some("loop argument".into()));
    let recur = module.reserve_continuation();
    let recur_param = module.add_value(Some("recur argument".into()));
    let recur_body = module.add_node(Node::ApplyFun {
        callee: Callee::Known(loop_function),
        args: vec![Atom::Value(recur_param)],
        return_to: loop_return,
    });
    module.define_continuation(
        recur,
        Continuation {
            debug_name: Some("recur".into()),
            params: vec![recur_param],
            body: recur_body,
        },
    );
    let switch = module.add_node(Node::Switch {
        scrutinee: Atom::Value(loop_param),
        cases: BTreeMap::from([(
            0,
            Edge {
                target: loop_return,
                args: vec![Atom::Value(loop_param)],
            },
        )]),
        default: Some(Edge {
            target: recur,
            args: vec![Atom::Value(loop_param)],
        }),
    });
    let loop_body = module.add_node(Node::LetCont {
        continuations: vec![recur],
        body: switch,
    });
    module.define_function(
        loop_function,
        Function {
            debug_name: Some("loop".into()),
            params: vec![loop_param],
            return_cont: loop_return,
            body: loop_body,
            droppable: false,
        },
    );
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(loop_function),
        args: vec![Atom::Literal(Literal::Nat(Natural::from(1u32)))],
        return_to: entry_return,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![loop_function],
        body: call,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
            droppable: false,
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
        Some(Node::ApplyCont(Edge { target, .. })) if *target == entry_return
    ));
    let Node::Switch { cases, .. } = module.node(switch).unwrap() else {
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
    let mut module = Module::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let outer = module.reserve_function();
    let outer_return = module.reserve_continuation();
    let inner = module.reserve_function();
    let inner_return = module.reserve_continuation();

    // inner(y) = y
    let y = module.add_value(Some("y".into()));
    let inner_body = module.add_node(Node::ApplyCont(Edge {
        target: inner_return,
        args: vec![Atom::Value(y)],
    }));
    module.define_function(
        inner,
        Function {
            debug_name: Some("inner".into()),
            params: vec![y],
            return_cont: inner_return,
            body: inner_body,
            droppable: false,
        },
    );

    // outer(x) = inner(x)
    let x = module.add_value(Some("x".into()));
    let call_inner = module.add_node(Node::ApplyFun {
        callee: Callee::Known(inner),
        args: vec![Atom::Value(x)],
        return_to: outer_return,
    });
    let outer_body = module.add_node(Node::LetFun {
        functions: vec![inner],
        body: call_inner,
    });
    module.define_function(
        outer,
        Function {
            debug_name: Some("outer".into()),
            params: vec![x],
            return_cont: outer_return,
            body: outer_body,
            droppable: false,
        },
    );

    // main() = outer(0)
    let call_outer = module.add_node(Node::ApplyFun {
        callee: Callee::Known(outer),
        args: vec![Atom::Literal(Literal::Nat(Natural::from(0u32)))],
        return_to: entry_return,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![outer],
        body: call_outer,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
            droppable: false,
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

/// `helper` captures `v`, which `owner` never names: the site is inside `helper`'s `LetFun`, so `v` is in scope there, and the move is admitted on scope rather than on what `owner`'s body mentions.
#[test]
fn contifies_a_callee_whose_capture_the_owner_never_mentions() {
    let (mut module, helper, _) = capture_unmentioned_by_owner();
    assert!(contify_calls(&mut module), "the sweep contifies");
    module.verify().unwrap();
    assert!(
        module.function(helper).is_none(),
        "the capturing helper is contified:\n{module}"
    );
}
