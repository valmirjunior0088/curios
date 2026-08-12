use {
    super::{ReturnProtocol, return_protocols},
    crate::{
        CpsAtom, CpsCallee, CpsContId, CpsContinuation, CpsEdge, CpsFunId, CpsFunction,
        CpsIntrinsicOp, CpsModule, CpsNode, CpsValueExpr,
    },
};

/// A function returning a one-field tuple through its own return continuation — the shape a caller takes apart.
fn returning_callee(module: &mut CpsModule, name: &str) -> CpsFunId {
    let field = module.add_value(Some(format!("{name}/field")));
    let built = module.add_value(Some(format!("{name}/built")));
    let function = module.reserve_function();
    let sentinel = module.reserve_continuation();

    let ret = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: sentinel,
        args: vec![CpsAtom::Value(built)],
    }));
    let body = module.add_node(CpsNode::LetValue {
        result: built,
        value: CpsValueExpr::Tuple(vec![CpsAtom::Value(field)]),
        next: ret,
    });
    module.define_function(
        function,
        CpsFunction {
            debug_name: Some(name.into()),
            params: vec![field],
            return_cont: sentinel,
            body,
        },
    );
    function
}

/// A resume continuation that reads its one parameter only through `TplGet(0)` and `TplGet(1)`, then exits.
fn projecting_resume(module: &mut CpsModule, name: &str) -> CpsContId {
    let result = module.add_value(Some(format!("{name}/result")));
    let tag = module.add_value(Some(format!("{name}/tag")));
    let payload = module.add_value(Some(format!("{name}/payload")));
    let resume = module.reserve_continuation();

    let exit = module.add_node(CpsNode::Exit { value: None });
    let second = module.add_node(CpsNode::LetIntrinsic {
        result: payload,
        op: CpsIntrinsicOp::TplGet(1),
        args: vec![CpsAtom::Value(result)],
        next: exit,
    });
    let first = module.add_node(CpsNode::LetIntrinsic {
        result: tag,
        op: CpsIntrinsicOp::TplGet(0),
        args: vec![CpsAtom::Value(result)],
        next: second,
    });
    module.define_continuation(
        resume,
        CpsContinuation {
            debug_name: Some(name.into()),
            params: vec![result],
            body: first,
        },
    );
    resume
}

/// A resume continuation that consumes its one parameter whole, by building a tuple around it.
fn opaque_resume(module: &mut CpsModule, name: &str) -> CpsContId {
    let result = module.add_value(Some(format!("{name}/result")));
    let held = module.add_value(Some(format!("{name}/held")));
    let resume = module.reserve_continuation();

    let exit = module.add_node(CpsNode::Exit { value: None });
    let hold = module.add_node(CpsNode::LetValue {
        result: held,
        value: CpsValueExpr::Tuple(vec![CpsAtom::Value(result)]),
        next: exit,
    });
    module.define_continuation(
        resume,
        CpsContinuation {
            debug_name: Some(name.into()),
            params: vec![result],
            body: hold,
        },
    );
    resume
}

/// A function whose whole body is one call to `callee`. `resume` names where the result lands; `None` returns it straight to this function's caller, which is what makes the call a tail call.
fn calling_function(
    module: &mut CpsModule,
    name: &str,
    callee: CpsFunId,
    resume: Option<CpsContId>,
) -> CpsFunId {
    let argument = module.add_value(Some(format!("{name}/argument")));
    let function = module.reserve_function();
    let sentinel = module.reserve_continuation();

    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(callee),
        args: vec![CpsAtom::Value(argument)],
        return_to: resume.unwrap_or(sentinel),
    });
    let body = match resume {
        Some(resume) => module.add_node(CpsNode::LetCont {
            continuations: vec![resume],
            body: call,
        }),
        None => call,
    };
    module.define_function(
        function,
        CpsFunction {
            debug_name: Some(name.into()),
            params: vec![argument],
            return_cont: sentinel,
            body,
        },
    );
    function
}

#[test]
fn a_result_read_only_through_projections_is_returned_as_its_fields() {
    let mut module = CpsModule::default();
    let callee = returning_callee(&mut module, "callee");
    let resume = projecting_resume(&mut module, "resume");
    let caller = calling_function(&mut module, "caller", callee, Some(resume));
    module.set_entry(caller);

    let protocols = return_protocols(&module);
    assert_eq!(protocols[&callee], ReturnProtocol::Fields(2));
    // The entry is pinned whatever its body does, because the host calls it and is not rewritten with the module.
    assert_eq!(protocols[&caller], ReturnProtocol::Tuple);
}

#[test]
fn a_result_consumed_whole_stays_a_tuple() {
    let mut module = CpsModule::default();
    let callee = returning_callee(&mut module, "callee");
    let resume = opaque_resume(&mut module, "resume");
    let caller = calling_function(&mut module, "caller", callee, Some(resume));
    module.set_entry(caller);

    assert_eq!(return_protocols(&module)[&callee], ReturnProtocol::Tuple);
}

/// `callee` is read only through projections and would be split on its own; `forwarder` is consumed whole and would not. `tail` decides whether the forwarder's call to it is a tail call, and so whether the two are one equivalence class or two.
fn chain(tail: bool) -> (CpsModule, CpsFunId, CpsFunId) {
    let mut module = CpsModule::default();
    let callee = returning_callee(&mut module, "callee");
    let inner = (!tail).then(|| projecting_resume(&mut module, "inner"));
    let forwarder = calling_function(&mut module, "forwarder", callee, inner);
    let outer = opaque_resume(&mut module, "outer");
    let entry = calling_function(&mut module, "entry", forwarder, Some(outer));
    module.set_entry(entry);
    (module, callee, forwarder)
}

#[test]
fn a_tail_call_chain_is_decided_together() {
    let (separate, callee, forwarder) = chain(false);
    let protocols = return_protocols(&separate);
    // Apart, the two disagree: one result is taken apart at its call site and the other is not.
    assert_eq!(protocols[&callee], ReturnProtocol::Fields(2));
    assert_eq!(protocols[&forwarder], ReturnProtocol::Tuple);

    let (joined, callee, forwarder) = chain(true);
    let protocols = return_protocols(&joined);
    // The tail call lowers to `return_call`, whose results must match the caller's, so the disagreement is resolved downward for both.
    assert_eq!(protocols[&callee], ReturnProtocol::Tuple);
    assert_eq!(protocols[&forwarder], ReturnProtocol::Tuple);
}

#[test]
fn a_callee_the_entry_tail_calls_keeps_the_host_protocol() {
    let mut module = CpsModule::default();
    let callee = returning_callee(&mut module, "callee");
    let inner = projecting_resume(&mut module, "inner");
    let reader = calling_function(&mut module, "reader", callee, Some(inner));

    // The entry calls `reader`, then tail-calls `callee` from the continuation that resumes it.
    let argument = module.add_value(Some("entry/argument".into()));
    let result = module.add_value(Some("entry/result".into()));
    let entry = module.reserve_function();
    let sentinel = module.reserve_continuation();
    let resume = module.reserve_continuation();

    let tail = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(callee),
        args: vec![CpsAtom::Value(argument)],
        return_to: sentinel,
    });
    module.define_continuation(
        resume,
        CpsContinuation {
            debug_name: Some("entry/resume".into()),
            params: vec![result],
            body: tail,
        },
    );
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(reader),
        args: vec![CpsAtom::Value(argument)],
        return_to: resume,
    });
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![resume],
        body: call,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("entry".into()),
            params: vec![argument],
            return_cont: sentinel,
            body,
        },
    );
    module.set_entry(entry);

    // `callee`'s own call sites all project, so nothing but the entry's pin keeps it from being split — and splitting it would widen the entry with it, which the host never agreed to.
    let protocols = return_protocols(&module);
    assert_eq!(protocols[&callee], ReturnProtocol::Tuple);
    assert_eq!(protocols[&entry], ReturnProtocol::Tuple);
}

#[test]
fn an_escaping_callee_stays_a_tuple() {
    let mut module = CpsModule::default();
    let callee = returning_callee(&mut module, "callee");
    let resume = projecting_resume(&mut module, "resume");

    // The entry calls `callee` directly, taking the result apart, and also captures it as a value.
    let argument = module.add_value(Some("entry/argument".into()));
    let closure = module.add_value(Some("entry/closure".into()));
    let entry = module.reserve_function();
    let sentinel = module.reserve_continuation();

    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(callee),
        args: vec![CpsAtom::Value(argument)],
        return_to: resume,
    });
    let bind = module.add_node(CpsNode::LetCont {
        continuations: vec![resume],
        body: call,
    });
    let capture = module.add_node(CpsNode::LetValue {
        result: closure,
        value: CpsValueExpr::Tuple(vec![CpsAtom::Fun(callee)]),
        next: bind,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("entry".into()),
            params: vec![argument],
            return_cont: sentinel,
            body: capture,
        },
    );
    module.set_entry(entry);

    // That one capture is enough: it puts the function behind a closure wrapper reaching it at the shared one-result closure type, whatever the direct call site asks for.
    assert_eq!(return_protocols(&module)[&callee], ReturnProtocol::Tuple);
}
