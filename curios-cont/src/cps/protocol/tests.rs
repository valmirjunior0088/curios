use {
    super::{ReturnProtocol, ReturnShape, return_protocols, split_returns},
    crate::{
        CpsAtom, CpsCallee, CpsContId, CpsContinuation, CpsEdge, CpsFunId, CpsFunction,
        CpsIntrinsic, CpsLiteral, CpsModule, CpsNode, CpsRow, CpsRowId, CpsSlot, CpsValueExpr,
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

/// A resume continuation that reads its one parameter only through `TupleGet(0)` and `TupleGet(1)`, then exits.
fn projecting_resume(module: &mut CpsModule, name: &str) -> CpsContId {
    let result = module.add_value(Some(format!("{name}/result")));
    let tag = module.add_value(Some(format!("{name}/tag")));
    let payload = module.add_value(Some(format!("{name}/payload")));
    let resume = module.reserve_continuation();

    let exit = module.add_node(CpsNode::Exit { value: None });
    let second = module.add_node(CpsNode::LetIntrinsic {
        result: payload,
        op: CpsIntrinsic::TupleGet(1),
        args: vec![CpsAtom::Value(result)],
        next: exit,
    });
    let first = module.add_node(CpsNode::LetIntrinsic {
        result: tag,
        op: CpsIntrinsic::TupleGet(0),
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
    assert!(matches!(protocols[&callee], ReturnProtocol::Fields(2, _)));
    // The entry is pinned whatever its body does, because the host calls it and is not rewritten with the module.
    assert_eq!(protocols[&caller], ReturnProtocol::Tuple);
}

/// The interprocedural read: the resume itself projects nothing — it hands its parameter to a join point, and the projections live there. Deferred demand carries the refinement back through the jump, so the callee is split exactly as if the projections were local.
#[test]
fn a_result_projected_only_behind_a_forwarding_jump_is_returned_as_its_fields() {
    let mut module = CpsModule::default();
    let callee = returning_callee(&mut module, "callee");
    let downstream = projecting_resume(&mut module, "downstream");

    let result = module.add_value(Some("resume/result".into()));
    let resume = module.reserve_continuation();
    let forward = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: downstream,
        args: vec![CpsAtom::Value(result)],
    }));
    module.define_continuation(
        resume,
        CpsContinuation {
            debug_name: Some("resume".into()),
            params: vec![result],
            body: forward,
        },
    );

    let argument = module.add_value(Some("caller/argument".into()));
    let caller = module.reserve_function();
    let sentinel = module.reserve_continuation();
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(callee),
        args: vec![CpsAtom::Value(argument)],
        return_to: resume,
    });
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![downstream, resume],
        body: call,
    });
    module.define_function(
        caller,
        CpsFunction {
            debug_name: Some("caller".into()),
            params: vec![argument],
            return_cont: sentinel,
            body,
        },
    );
    module.set_entry(caller);

    assert!(matches!(
        return_protocols(&module)[&callee],
        ReturnProtocol::Fields(2, _)
    ));
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
    assert!(matches!(protocols[&callee], ReturnProtocol::Fields(2, _)));
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

/// A two-slot row — a tag and one payload, the shape of `/std/Option` — minted for the fixtures below.
fn option_row(module: &mut CpsModule) -> CpsRowId {
    module.add_row(CpsRow {
        debug_name: Some("Option".into()),
        slots: vec![CpsSlot::Tag, CpsSlot::Opaque],
    })
}

/// [`returning_callee`] over a row: the construction it returns is `Row(row, [1, field])`.
fn row_returning_callee(module: &mut CpsModule, name: &str, row: CpsRowId) -> CpsFunId {
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
        value: CpsValueExpr::Row(
            row,
            vec![CpsAtom::Literal(CpsLiteral::Nat(1)), CpsAtom::Value(field)],
        ),
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

/// [`projecting_resume`] in the row vocabulary: both reads are `RowGet`.
fn row_projecting_resume(module: &mut CpsModule, name: &str, row: CpsRowId) -> CpsContId {
    let result = module.add_value(Some(format!("{name}/result")));
    let tag = module.add_value(Some(format!("{name}/tag")));
    let payload = module.add_value(Some(format!("{name}/payload")));
    let resume = module.reserve_continuation();

    let exit = module.add_node(CpsNode::Exit { value: None });
    let second = module.add_node(CpsNode::LetIntrinsic {
        result: payload,
        op: CpsIntrinsic::RowGet(row, 1),
        args: vec![CpsAtom::Value(result)],
        next: exit,
    });
    let first = module.add_node(CpsNode::LetIntrinsic {
        result: tag,
        op: CpsIntrinsic::RowGet(row, 0),
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

/// The shape a resume rebuilds in is the class's, not the callee's own. `forwarder` returns only by tail-calling `callee`, so it has no return edge of its own to read a vocabulary off — and its caller's resume reads the result as the row `callee` builds. Deriving the shape per function rebuilt a structural tuple here, which the `RowGet` below then cast to the row's final type; that was `/std/http/header_lookup`, and the only symptom was an HTTP client trapping on its first response header.
///
/// Mutation-checked by reverting the shape to the per-function derivation: the protocol alone still reads `Fields(2, …)`, and what fails is the verifier on the rebuilt `Tuple` — which is the other half of the fix, and why the fixture asserts both.
#[test]
fn a_forwarder_rebuilds_in_its_class_vocabulary() {
    let mut module = CpsModule::default();
    let row = option_row(&mut module);
    let callee = row_returning_callee(&mut module, "callee", row);
    let forwarder = calling_function(&mut module, "forwarder", callee, None);
    let outer = row_projecting_resume(&mut module, "outer", row);
    let entry = calling_function(&mut module, "entry", forwarder, Some(outer));
    // Bound lexically, unlike the other fixtures here, because this one runs the verifier and the verifier reads scope.
    let inner = module.function(entry).unwrap().body;
    let bound = module.add_node(CpsNode::LetFun {
        functions: vec![callee, forwarder],
        body: inner,
    });
    module.functions.get_mut(entry).unwrap().body = bound;
    module.set_entry(entry);
    module.verify().unwrap();

    let protocols = return_protocols(&module);
    assert_eq!(
        protocols[&forwarder],
        ReturnProtocol::Fields(2, ReturnShape::Row(row)),
        "the forwarder has no return edge of its own and takes the class's shape",
    );
    assert_eq!(protocols[&callee], protocols[&forwarder]);

    assert!(split_returns(&mut module));
    module.verify().unwrap_or_else(|error| {
        panic!("the resume must rebuild in the row vocabulary its reads are in: {error}")
    });
    let rebuilt = module
        .continuation(outer)
        .map(|definition| module.node(definition.body).unwrap().clone())
        .unwrap();
    assert!(
        matches!(
            rebuilt,
            CpsNode::LetValue {
                value: CpsValueExpr::Row(rebuilt_row, _),
                ..
            } if rebuilt_row == row
        ),
        "{rebuilt:?}"
    );
}
