//! A known function's parameters and its tail-call class's results, decided as joins, and what withdraws them: an escaping function, a closure tail call, a resume parameter read otherwise.

use {
    super::{Storage, representation, test_support::*},
    crate::{
        Atom, Callee, Continuation, Edge, FunctionId, Intrinsic, Literal, Module, Node, NodeId,
        Repr, ValueExpr, ValueId,
    },
    curios_num::{Floating, Natural, Rounding},
};

/// A call's result reaches its continuation's parameter as a reference where the callee's results are fixed by something other than its own return edges: here a tail call through a closure, which returns at the closure type's one uniform result. The parameter is open to its uses in general, and this is what withdraws it.
#[test]
fn a_call_result_a_closure_tail_call_fixes_stays_boxed() {
    let mut module = Module::new();
    let closure = module.add_value(Some("closure".into()));
    let returned = module.add_value(Some("returned".into()));
    let read = module.add_value(Some("read".into()));
    let callee = module.reserve_function();
    let after = module.reserve_continuation();

    define(
        &mut module,
        callee,
        "callee",
        vec![closure],
        |module, sentinel| {
            module.add_node(Node::ApplyFun {
                callee: Callee::Closure(closure),
                args: vec![],
                return_to: sentinel,
            })
        },
    );
    let done = finish(&mut module, vec![Atom::Value(read)]);
    let after_body = float_read(&mut module, read, returned, done);
    resume(&mut module, after, vec![returned], after_body);
    let outside = module.add_value(Some("outside".into()));
    let body = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![Atom::Value(outside)],
        return_to: after,
    });
    entry(&mut module, vec![outside], body);

    let decided = representation(&module);
    assert_eq!(decided.values[&returned], Storage::Boxed);
    assert_eq!(decided.results[&callee], vec![Storage::Boxed]);
}

/// Read `value` as a float into `result`, then go on to `next`.
fn float_read(module: &mut Module, result: ValueId, value: ValueId, next: NodeId) -> NodeId {
    module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::FltAdd(Rounding::TiesToEven),
        args: vec![
            Atom::Value(value),
            Atom::Literal(Literal::Flt(Floating::from(1.0))),
        ],
        next,
    })
}

/// Bind `result` to a float a definition produces, then go on to `next`.
fn float_made(module: &mut Module, result: ValueId, next: NodeId) -> NodeId {
    module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::FltAdd(Rounding::TiesToEven),
        args: vec![
            Atom::Literal(Literal::Flt(Floating::from(2.0))),
            Atom::Literal(Literal::Flt(Floating::from(3.0))),
        ],
        next,
    })
}

/// Define `callee(x)`, which reads `x` as a float and returns the sum, and answer `x`.
fn float_reader(module: &mut Module, callee: FunctionId) -> ValueId {
    let param = module.add_value(Some("x".into()));
    let sum = module.add_value(Some("sum".into()));
    define(module, callee, "reader", vec![param], |module, sentinel| {
        let done = module.add_node(Node::ApplyCont(Edge {
            target: sentinel,
            args: vec![Atom::Value(sum)],
        }));
        float_read(module, sum, param, done)
    });
    param
}

/// Call `callee(arg)` from a continuation that reads the result as a float, and answer the call and the result's parameter.
fn float_call(module: &mut Module, callee: FunctionId, arg: Atom) -> (NodeId, ValueId) {
    let returned = module.add_value(Some("returned".into()));
    let read = module.add_value(Some("read".into()));
    let after = module.reserve_continuation();
    let done = finish(module, vec![Atom::Value(read)]);
    let after_body = float_read(module, read, returned, done);
    resume(module, after, vec![returned], after_body);
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![arg],
        return_to: after,
    });
    (call, returned)
}

/// A function that neither escapes nor is the entry is entered only by calls naming it, each handing over its arguments, so its parameter is a join whose edges are those calls, decided as a loop header's is. Every call here hands it a float a definition produced, and its body reads it as one.
#[test]
fn a_known_parameter_every_call_hands_a_float_is_held_in_a_register() {
    let mut module = Module::new();
    let callee = module.reserve_function();
    let param = float_reader(&mut module, callee);
    let float = module.add_value(Some("float".into()));
    let (call, _) = float_call(&mut module, callee, Atom::Value(float));
    let body = float_made(&mut module, float, call);
    entry(&mut module, vec![], body);

    let decided = representation(&module).values;
    assert_eq!(decided[&param], Storage::Raw(Repr::Flt));
    assert_eq!(decided[&float], Storage::Raw(Repr::Flt));
}

/// The soundness half: a call handing the parameter a reference states nothing of what the reference holds, so the parameter stays one, as a join's does.
#[test]
fn a_known_parameter_a_reference_reaches_stays_boxed() {
    let mut module = Module::new();
    let callee = module.reserve_function();
    let param = float_reader(&mut module, callee);
    let outside = module.add_value(Some("outside".into()));
    let (call, _) = float_call(&mut module, callee, Atom::Value(outside));
    entry(&mut module, vec![outside], call);

    assert_eq!(representation(&module).values[&param], Storage::Boxed);
}

/// A function named as a value escapes: a closure calls it through its arity's uniform signature, whatever every call naming it hands over.
#[test]
fn an_escaping_functions_parameter_stays_boxed() {
    let mut module = Module::new();
    let callee = module.reserve_function();
    let param = float_reader(&mut module, callee);
    let float = module.add_value(Some("float".into()));
    let named = module.add_value(Some("named".into()));
    let (call, _) = float_call(&mut module, callee, Atom::Value(float));
    let name = module.add_node(Node::LetValue {
        result: named,
        value: ValueExpr::Tuple(vec![Atom::Fun(callee)]),
        next: call,
    });
    let body = float_made(&mut module, float, name);
    entry(&mut module, vec![], body);

    let decided = representation(&module);
    assert_eq!(decided.values[&param], Storage::Boxed);
    assert_eq!(decided.results[&callee], vec![Storage::Boxed]);
}

/// A result slot is a join whose edges are the class's return edges and whose uses are the resume parameters of every call into it. Every return here hands it a float, and the caller reads the result as one, so the slot and the resume parameter are both held in a register.
#[test]
fn a_result_every_return_hands_a_float_is_held_in_a_register() {
    let mut module = Module::new();
    let callee = module.reserve_function();
    float_reader(&mut module, callee);
    let float = module.add_value(Some("float".into()));
    let (call, returned) = float_call(&mut module, callee, Atom::Value(float));
    let body = float_made(&mut module, float, call);
    entry(&mut module, vec![], body);

    let decided = representation(&module);
    assert_eq!(decided.values[&returned], Storage::Raw(Repr::Flt));
    assert_eq!(decided.results[&callee], vec![Storage::Raw(Repr::Flt)]);
}

/// One call reads the result as a float and another only stores it in a tuple. The slot is raw, since a use demands it, and the second resume parameter stays boxed: the emitter boxes the result once as it enters, rather than at each use.
#[test]
fn a_resume_parameter_with_only_boxed_uses_beside_a_raw_result_stays_boxed() {
    let mut module = Module::new();
    let callee = module.reserve_function();
    float_reader(&mut module, callee);
    let float = module.add_value(Some("float".into()));
    let stored = module.add_value(Some("stored".into()));
    let tuple = module.add_value(Some("tuple".into()));
    let after = module.reserve_continuation();
    let done = finish(&mut module, vec![Atom::Value(tuple)]);
    let after_body = module.add_node(Node::LetValue {
        result: tuple,
        value: ValueExpr::Tuple(vec![Atom::Value(stored)]),
        next: done,
    });
    resume(&mut module, after, vec![stored], after_body);
    let storing = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![Atom::Value(float)],
        return_to: after,
    });
    let (reading, returned) = float_call(&mut module, callee, Atom::Value(float));
    let reading = edge_to(&mut module, reading);
    let storing = edge_to(&mut module, storing);
    let both = module.add_node(Node::Switch {
        scrutinee: Atom::Literal(Literal::Nat(Natural::from(0u32))),
        cases: [(0, reading)].into(),
        default: Some(storing),
    });
    let body = float_made(&mut module, float, both);
    entry(&mut module, vec![], body);

    let decided = representation(&module);
    assert_eq!(decided.results[&callee], vec![Storage::Raw(Repr::Flt)]);
    assert_eq!(decided.values[&returned], Storage::Raw(Repr::Flt));
    assert_eq!(decided.values[&stored], Storage::Boxed);
}

/// One call reads the result as a float and another reads it not at all. The unread resume parameter has no use to disagree with the register, so it is held as the slot hands it over, and nothing boxes a result no one reads.
#[test]
fn a_resume_parameter_nothing_reads_is_held_as_its_slot_hands_it_over() {
    let mut module = Module::new();
    let callee = module.reserve_function();
    float_reader(&mut module, callee);
    let float = module.add_value(Some("float".into()));
    let unread = module.add_value(Some("unread".into()));
    let after = module.reserve_continuation();
    let done = finish(&mut module, vec![]);
    resume(&mut module, after, vec![unread], done);
    let discarding = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![Atom::Value(float)],
        return_to: after,
    });
    let (reading, _) = float_call(&mut module, callee, Atom::Value(float));
    let reading = edge_to(&mut module, reading);
    let discarding = edge_to(&mut module, discarding);
    let both = module.add_node(Node::Switch {
        scrutinee: Atom::Literal(Literal::Nat(Natural::from(0u32))),
        cases: [(0, reading)].into(),
        default: Some(discarding),
    });
    let body = float_made(&mut module, float, both);
    entry(&mut module, vec![], body);

    let decided = representation(&module);
    assert_eq!(decided.results[&callee], vec![Storage::Raw(Repr::Flt)]);
    assert_eq!(decided.values[&unread], Storage::Raw(Repr::Flt));
}

/// An argumentless edge into a continuation whose body is `body`.
fn edge_to(module: &mut Module, body: NodeId) -> Edge {
    let target = module.reserve_continuation();
    module.define_continuation(
        target,
        Continuation {
            debug_name: Some("arm".into()),
            params: vec![],
            body,
        },
    );
    Edge {
        target,
        args: vec![],
    }
}
