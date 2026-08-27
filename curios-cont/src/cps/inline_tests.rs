//! Inlining a known call and a single-use continuation, and what each must clone rather than share.

use {
    crate::cps::inline::{inline_known_calls, inline_single_use_continuations},
    crate::{
        CpsAtom, CpsCallee, CpsContinuation, CpsEdge, CpsFunction, CpsLiteral, CpsModule, CpsNode,
    },
};

#[test]
fn continuation_beta_rewrites_parameters_captured_by_nested_functions() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let continuation = module.reserve_continuation();
    let captured = module.add_value(Some("captured".into()));

    let nested = module.reserve_function();
    let nested_return = module.reserve_continuation();
    let nested_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: nested_return,
        args: vec![CpsAtom::Value(captured)],
    }));
    module.define_function(
        nested,
        CpsFunction {
            debug_name: Some("nested".into()),
            params: vec![],
            return_cont: nested_return,
            body: nested_body,
        },
    );
    let return_nested = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Fun(nested)],
    }));
    let continuation_body = module.add_node(CpsNode::LetFun {
        functions: vec![nested],
        body: return_nested,
    });
    module.define_continuation(
        continuation,
        CpsContinuation {
            debug_name: Some("capture scope".into()),
            params: vec![captured],
            body: continuation_body,
        },
    );
    let call = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: continuation,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(7))],
    }));
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![continuation],
        body: call,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
        },
    );
    module.set_entry(entry);

    assert!(inline_single_use_continuations(&mut module));
    assert!(matches!(
        module.node(nested_body),
        Some(CpsNode::ApplyCont(CpsEdge { args, .. }))
            if args == &[CpsAtom::Literal(CpsLiteral::Nat(7))]
    ));
    module.verify().unwrap();
}

#[test]
fn known_call_inlining_clones_recursive_local_continuations() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let callee = module.reserve_function();
    let callee_return = module.reserve_continuation();
    let callee_param = module.add_value(None);
    let local_cont = module.reserve_continuation();
    let local_param = module.add_value(None);
    let local_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: local_cont,
        args: vec![CpsAtom::Value(local_param)],
    }));
    module.define_continuation(
        local_cont,
        CpsContinuation {
            debug_name: None,
            params: vec![local_param],
            body: local_body,
        },
    );
    let enter_local = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: local_cont,
        args: vec![CpsAtom::Value(callee_param)],
    }));
    let callee_body = module.add_node(CpsNode::LetCont {
        continuations: vec![local_cont],
        body: enter_local,
    });
    module.define_function(
        callee,
        CpsFunction {
            debug_name: None,
            params: vec![callee_param],
            return_cont: callee_return,
            body: callee_body,
        },
    );
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(callee),
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
        return_to: entry_return,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![callee],
        body: call,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: None,
            params: vec![],
            return_cont: entry_return,
            body,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();

    assert!(inline_known_calls(&mut module));
    assert!(matches!(
        module.node(call),
        Some(CpsNode::LetCont { continuations, .. }) if continuations != &[local_cont]
    ));
    module.verify().unwrap();
}
