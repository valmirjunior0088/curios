use {
    super::{Demand, demand_of, demands},
    crate::{
        CpsAtom, CpsCallee, CpsContinuation, CpsEdge, CpsFunction, CpsIntrinsicOp, CpsModule,
        CpsNode, CpsValueExpr, CpsValueId,
    },
    std::collections::BTreeSet,
};

/// Three parameters, one read only through a projection, one consumed whole, one never mentioned.
fn module() -> (CpsModule, CpsValueId, CpsValueId, CpsValueId) {
    let mut module = CpsModule::default();
    let projected = module.add_value(Some("projected".into()));
    let whole = module.add_value(Some("whole".into()));
    let unused = module.add_value(Some("unused".into()));
    let field = module.add_value(Some("field".into()));
    let built = module.add_value(Some("built".into()));

    let exit = module.add_node(CpsNode::Exit { value: None });
    let construct = module.add_node(CpsNode::LetValue {
        result: built,
        value: CpsValueExpr::Tuple(vec![CpsAtom::Value(whole)]),
        next: exit,
    });
    let project = module.add_node(CpsNode::LetIntrinsic {
        result: field,
        op: CpsIntrinsicOp::TplGet(0),
        args: vec![CpsAtom::Value(projected)],
        next: construct,
    });

    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();
    module.define_function(
        function,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![projected, whole, unused],
            return_cont,
            body: project,
        },
    );
    module.set_entry(function);

    (module, projected, whole, unused)
}

#[test]
fn a_projection_is_not_a_whole_use() {
    let (module, projected, whole, unused) = module();
    let demands = demands(&module);

    // The refinement the return protocol needs: this value is read, but never in one piece.
    assert_eq!(
        demand_of(&demands, projected),
        Demand::Projected(BTreeSet::from([0]))
    );
    assert_eq!(demand_of(&demands, whole), Demand::Opaque);
    assert_eq!(demand_of(&demands, unused), Demand::Unused);
}

#[test]
fn an_unseeded_value_is_opaque_and_never_unused() {
    let (module, ..) = module();
    let demands = demands(&module);
    let absent = CpsValueId(9999);

    // Absence must read as the top rather than the bottom. Reading it as `Unused` is dead-parameter elimination deleting a parameter the walk simply never reached.
    assert!(!demands.contains_key(&absent));
    assert_eq!(demand_of(&demands, absent), Demand::Opaque);
}

/// A caller passing `argument` into a callee that only projects field 1 of its parameter: the deferral hands the argument the callee's refinement instead of consuming it whole.
#[test]
fn an_argument_asks_what_the_receiving_parameter_asks() {
    let mut module = CpsModule::default();
    let param = module.add_value(Some("param".into()));
    let field = module.add_value(Some("field".into()));
    let argument = module.add_value(Some("argument".into()));
    let received = module.add_value(Some("received".into()));

    let callee = module.reserve_function();
    let callee_ret = module.reserve_continuation();
    let callee_exit = module.add_node(CpsNode::Exit { value: None });
    let callee_body = module.add_node(CpsNode::LetIntrinsic {
        result: field,
        op: CpsIntrinsicOp::TplGet(1),
        args: vec![CpsAtom::Value(param)],
        next: callee_exit,
    });
    module.define_function(
        callee,
        CpsFunction {
            debug_name: Some("callee".into()),
            params: vec![param],
            return_cont: callee_ret,
            body: callee_body,
        },
    );

    let caller = module.reserve_function();
    let caller_ret = module.reserve_continuation();
    let resume = module.reserve_continuation();
    let resume_exit = module.add_node(CpsNode::Exit { value: None });
    module.define_continuation(
        resume,
        CpsContinuation {
            debug_name: Some("resume".into()),
            params: vec![received],
            body: resume_exit,
        },
    );
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(callee),
        args: vec![CpsAtom::Value(argument)],
        return_to: resume,
    });
    let caller_body = module.add_node(CpsNode::LetCont {
        continuations: vec![resume],
        body: call,
    });
    module.define_function(
        caller,
        CpsFunction {
            debug_name: Some("caller".into()),
            params: vec![argument],
            return_cont: caller_ret,
            body: caller_body,
        },
    );
    module.set_entry(caller);

    let demands = demands(&module);
    assert_eq!(
        demand_of(&demands, argument),
        Demand::Projected(BTreeSet::from([1]))
    );
    assert_eq!(
        demand_of(&demands, param),
        Demand::Projected(BTreeSet::from([1]))
    );
    assert_eq!(demand_of(&demands, received), Demand::Unused);
}

/// A value threaded along two jumps into a parameter nobody reads is dead however many edges carry it — the reach a use count does not have, and the reason the strengthening moves emitted code.
#[test]
fn deferral_reaches_unused_through_a_chain() {
    let mut module = CpsModule::default();
    let argument = module.add_value(Some("argument".into()));
    let first = module.add_value(Some("first".into()));
    let second = module.add_value(Some("second".into()));

    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let inner = module.reserve_continuation();
    let outer = module.reserve_continuation();

    let inner_exit = module.add_node(CpsNode::Exit { value: None });
    module.define_continuation(
        inner,
        CpsContinuation {
            debug_name: Some("inner".into()),
            params: vec![second],
            body: inner_exit,
        },
    );
    let forward = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: inner,
        args: vec![CpsAtom::Value(first)],
    }));
    module.define_continuation(
        outer,
        CpsContinuation {
            debug_name: Some("outer".into()),
            params: vec![first],
            body: forward,
        },
    );
    let enter = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: outer,
        args: vec![CpsAtom::Value(argument)],
    }));
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![inner, outer],
        body: enter,
    });
    module.define_function(
        function,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![argument],
            return_cont,
            body,
        },
    );
    module.set_entry(function);

    let demands = demands(&module);
    assert_eq!(demand_of(&demands, second), Demand::Unused);
    assert_eq!(demand_of(&demands, first), Demand::Unused);
    assert_eq!(demand_of(&demands, argument), Demand::Unused);
}

/// A value returned on the sentinel is consumed by an interface this lattice does not cross: its consumer is the caller's resume, whose linkage belongs to the return protocol.
#[test]
fn a_returned_value_stays_opaque() {
    let mut module = CpsModule::default();
    let returned = module.add_value(Some("returned".into()));

    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(returned)],
    }));
    module.define_function(
        function,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![returned],
            return_cont,
            body,
        },
    );
    module.set_entry(function);

    let demands = demands(&module);
    assert_eq!(demand_of(&demands, returned), Demand::Opaque);
}

/// A closure call's arguments stay opaque — the callee is unresolved, so no parameter exists to defer to — while the callee value itself reads as applied.
#[test]
fn a_closure_call_consumes_its_arguments_whole() {
    let mut module = CpsModule::default();
    let closure = module.add_value(Some("closure".into()));
    let argument = module.add_value(Some("argument".into()));

    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Closure(closure),
        args: vec![CpsAtom::Value(argument)],
        return_to: return_cont,
    });
    module.define_function(
        function,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![closure, argument],
            return_cont,
            body: call,
        },
    );
    module.set_entry(function);

    let demands = demands(&module);
    assert_eq!(demand_of(&demands, closure), Demand::Applied(1));
    assert_eq!(demand_of(&demands, argument), Demand::Opaque);
}
