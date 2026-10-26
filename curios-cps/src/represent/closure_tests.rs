//! Which values hold closures of one arity, so the emitter declares them at that arity's environment type: a join every edge hands one arity, and never one a second arity, a reference or a call's result reaches.

use {
    super::{representation, test_support::*},
    crate::{Atom, Callee, Edge, FunctionId, Module, Node, ValueId},
};

/// Define a function taking `arity` parameters, which returns its first or, taking none, a literal, and answer it.
fn closure_of(module: &mut Module, arity: usize) -> FunctionId {
    let function = module.reserve_function();
    let params = (0..arity)
        .map(|_| module.add_value(Some("x".into())))
        .collect::<Vec<_>>();
    let returned = params.first().map_or(nat(0), |param| Atom::Value(*param));
    define(module, function, "closure", params, |module, sentinel| {
        module.add_node(Node::ApplyCont(Edge {
            target: sentinel,
            args: vec![returned],
        }))
    });
    function
}

/// A join returning its parameter, entered on the switch on `flag`'s case `0` with `first` and on its default with `second`; `params` are the entry's own. Answers the parameter.
fn closure_join(
    module: &mut Module,
    params: Vec<ValueId>,
    flag: ValueId,
    first: Atom,
    second: Atom,
) -> ValueId {
    let joined = module.add_value(Some("joined".into()));
    let join = module.reserve_continuation();
    let done = finish(module, vec![Atom::Value(joined)]);
    resume(module, join, vec![joined], done);
    let body = module.add_node(Node::Switch {
        scrutinee: Atom::Value(flag),
        cases: [(
            0,
            Edge {
                target: join,
                args: vec![first],
            },
        )]
        .into(),
        default: Some(Edge {
            target: join,
            args: vec![second],
        }),
    });
    entry(module, params, body);
    joined
}

/// A join every edge hands a closure of one arity holds closures of it, though each edge hands another closure.
#[test]
fn a_join_every_edge_hands_one_arity_holds_closures_of_it() {
    let mut module = Module::new();
    let flag = module.add_value(Some("flag".into()));
    let first = closure_of(&mut module, 1);
    let second = closure_of(&mut module, 1);
    let joined = closure_join(
        &mut module,
        vec![flag],
        flag,
        Atom::Fun(first),
        Atom::Fun(second),
    );

    assert_eq!(representation(&module).closures.get(&joined), Some(&1));
}

/// Closures of two arities share no environment type, so a join both reach holds closures of neither.
#[test]
fn a_join_two_arities_reach_holds_no_closure_of_one() {
    let mut module = Module::new();
    let flag = module.add_value(Some("flag".into()));
    let first = closure_of(&mut module, 1);
    let second = closure_of(&mut module, 2);
    let joined = closure_join(
        &mut module,
        vec![flag],
        flag,
        Atom::Fun(first),
        Atom::Fun(second),
    );

    assert_eq!(representation(&module).closures.get(&joined), None);
}

/// A reference the host passes states nothing of what it holds, so a closure beside it on another edge does not make the join one.
#[test]
fn a_join_a_reference_reaches_holds_no_closure_of_one() {
    let mut module = Module::new();
    let flag = module.add_value(Some("flag".into()));
    let outside = module.add_value(Some("outside".into()));
    let first = closure_of(&mut module, 1);
    let joined = closure_join(
        &mut module,
        vec![flag, outside],
        flag,
        Atom::Fun(first),
        Atom::Value(outside),
    );

    assert_eq!(representation(&module).closures.get(&joined), None);
}

/// A call hands its result back at its signature's position, which declares no closure, so what a call returns is held as a reference whatever the callee returns.
#[test]
fn a_call_result_holds_no_closure_of_one() {
    let mut module = Module::new();
    let closure = closure_of(&mut module, 1);
    let callee = module.reserve_function();
    define(&mut module, callee, "callee", vec![], |module, sentinel| {
        module.add_node(Node::ApplyCont(Edge {
            target: sentinel,
            args: vec![Atom::Fun(closure)],
        }))
    });
    let after = module.reserve_continuation();
    let returned = module.add_value(Some("returned".into()));
    let done = finish(&mut module, vec![Atom::Value(returned)]);
    resume(&mut module, after, vec![returned], done);
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![],
        return_to: after,
    });
    entry(&mut module, vec![], call);

    assert_eq!(representation(&module).closures.get(&returned), None);
}
