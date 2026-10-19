//! Jump and call forwarding, atom rewriting, and the calls whose results nothing reads.

use {
    super::{eliminate_dead_calls, forward_calls, forward_continuations, rewrite_atoms},
    crate::{
        Atom, Callee, Continuation, ContinuationId, Edge, Function, FunctionId, Literal, Module,
        Node, NodeId,
    },
    curios_num::{Floating, Natural},
    std::collections::BTreeMap,
};

#[test]
fn forwarding_composes_jump_arguments_instead_of_only_retargeting() {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let target = module.reserve_continuation();
    let target_left = module.add_value(Some("target left".into()));
    let target_right = module.add_value(Some("target right".into()));
    let target_body = module.add_node(Node::ApplyCont(Edge {
        target: return_cont,
        args: vec![Atom::Value(target_right)],
    }));
    module.define_continuation(
        target,
        Continuation {
            debug_name: Some("target".into()),
            params: vec![target_left, target_right],
            body: target_body,
        },
    );
    let forwarding = module.reserve_continuation();
    let forwarded = module.add_value(Some("forwarded".into()));
    let forwarding_body = module.add_node(Node::ApplyCont(Edge {
        target,
        args: vec![
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
            Atom::Value(forwarded),
        ],
    }));
    module.define_continuation(
        forwarding,
        Continuation {
            debug_name: Some("forwarding".into()),
            params: vec![forwarded],
            body: forwarding_body,
        },
    );
    let call = module.add_node(Node::ApplyCont(Edge {
        target: forwarding,
        args: vec![Atom::Literal(Literal::Nat(Natural::from(7u32)))],
    }));
    let body = module.add_node(Node::LetCont {
        continuations: vec![forwarding, target],
        body: call,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
            droppable: false,
        },
    );
    module.set_entry(entry);

    assert!(forward_continuations(&mut module));
    assert!(matches!(
        module.node(call),
        Some(Node::ApplyCont(Edge { target: actual, args }))
            if *actual == target
                && args == &[
                    Atom::Literal(Literal::Nat(Natural::from(1u32))),
                    Atom::Literal(Literal::Nat(Natural::from(7u32))),
                ]
    ));
    module.verify().unwrap();
}

/// A NaN literal riding a jump must not keep `forward_continuations` reporting a change on every round: `thread_edge` compares the edge it rebuilt against the edge it read, and under IEEE equality on a float literal a NaN is unequal to itself, so an untouched edge would read as rewritten and the fixpoint would run to its backstop. `Literal::Flt` is bitwise, and this pins the consequence — the second call over a settled module reports nothing.
#[test]
fn forwarding_a_nan_literal_settles_in_one_round() {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let target = module.reserve_continuation();
    let received = module.add_value(Some("received".into()));
    let target_body = module.add_node(Node::ApplyCont(Edge {
        target: return_cont,
        args: vec![Atom::Value(received)],
    }));
    module.define_continuation(
        target,
        Continuation {
            debug_name: Some("target".into()),
            params: vec![received],
            body: target_body,
        },
    );
    let forwarding = module.reserve_continuation();
    let forwarded = module.add_value(Some("forwarded".into()));
    let forwarding_body = module.add_node(Node::ApplyCont(Edge {
        target,
        args: vec![Atom::Value(forwarded)],
    }));
    module.define_continuation(
        forwarding,
        Continuation {
            debug_name: Some("forwarding".into()),
            params: vec![forwarded],
            body: forwarding_body,
        },
    );
    let nan = Atom::Literal(Literal::Flt(Floating::from(f64::NAN)));
    let call = module.add_node(Node::ApplyCont(Edge {
        target: forwarding,
        args: vec![nan.clone()],
    }));
    let body = module.add_node(Node::LetCont {
        continuations: vec![forwarding, target],
        body: call,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
            droppable: false,
        },
    );
    module.set_entry(entry);

    assert!(forward_continuations(&mut module));
    assert!(matches!(
        module.node(call),
        Some(Node::ApplyCont(Edge { target: actual, args }))
            if *actual == target && args == std::slice::from_ref(&nan)
    ));
    assert!(
        !forward_continuations(&mut module),
        "a settled module must report no change"
    );
    module.verify().unwrap();
}

#[test]
fn rewrite_atoms_remaps_and_devirtualizes_a_closure_callee() {
    // The closure callee holds its target in a value that `visit_atoms_mut` never reaches. A forwarded value must follow (else the callee dangles when the original value is deleted), and a known function devirtualizes.
    let mut module = Module::new();
    let ret = module.reserve_continuation();
    let old = module.add_value(Some("old".into()));
    let new = module.add_value(Some("new".into()));
    let target = module.reserve_function();

    let value_call = module.add_node(Node::ApplyFun {
        callee: Callee::Closure(old),
        args: vec![],
        return_to: ret,
    });
    assert!(rewrite_atoms(
        &mut module,
        &BTreeMap::from([(old, Atom::Value(new))]),
    ));
    assert!(
        matches!(module.node(value_call), Some(Node::ApplyFun { callee: Callee::Closure(v), .. }) if *v == new),
        "a forwarded value keeps the closure callee pointing at a live value"
    );

    let fun_call = module.add_node(Node::ApplyFun {
        callee: Callee::Closure(new),
        args: vec![],
        return_to: ret,
    });
    assert!(rewrite_atoms(
        &mut module,
        &BTreeMap::from([(new, Atom::Fun(target))]),
    ));
    assert!(
        matches!(module.node(fun_call), Some(Node::ApplyFun { callee: Callee::Known(f), .. }) if *f == target),
        "a known function devirtualizes the closure call"
    );
}

/// A value that decides a result's type leaves a closure applied both ways, and a copy specialized to one arm keeps the other arm's application: a closure known to be a function of two parameters, forced as a thunk. No run arrives there, and a known call would misstate what the callee takes to every pass that reads it.
#[test]
fn a_closure_call_of_a_function_of_another_arity_is_unreachable() {
    let mut module = Module::new();
    let ret = module.reserve_continuation();
    let closure = module.add_value(Some("closure".into()));
    let first = module.add_value(Some("first".into()));
    let second = module.add_value(Some("second".into()));
    let target = module.reserve_function();
    let target_return = module.reserve_continuation();
    let body = module.add_node(Node::ApplyCont(Edge {
        target: target_return,
        args: vec![Atom::Value(first)],
    }));
    module.define_function(
        target,
        Function {
            debug_name: Some("target".into()),
            params: vec![first, second],
            return_cont: target_return,
            body,
            droppable: false,
        },
    );
    let forced = module.add_node(Node::ApplyFun {
        callee: Callee::Closure(closure),
        args: vec![],
        return_to: ret,
    });

    assert!(rewrite_atoms(
        &mut module,
        &BTreeMap::from([(closure, Atom::Fun(target))]),
    ));
    assert!(
        matches!(module.node(forced), Some(Node::Unreachable)),
        "a call no run arrives at is left as one"
    );
}

/// A join whose body calls the function its jump hands it, with the jump's other argument as the call's, and the one jump into it — handing over the known function `arm`, or, when `known` is false, a closure the entry holds in its parameter. Answers the jump, `arm` and the entry's return continuation.
fn calling_join(known: bool) -> (Module, NodeId, FunctionId, ContinuationId) {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();

    let arm = module.reserve_function();
    let arm_return = module.reserve_continuation();
    let arm_param = module.add_value(Some("arm argument".into()));
    let arm_body = module.add_node(Node::ApplyCont(Edge {
        target: arm_return,
        args: vec![Atom::Value(arm_param)],
    }));
    module.define_function(
        arm,
        Function {
            debug_name: Some("arm".into()),
            params: vec![arm_param],
            return_cont: arm_return,
            body: arm_body,
            droppable: false,
        },
    );

    let join = module.reserve_continuation();
    let join_callee = module.add_value(Some("join callee".into()));
    let join_argument = module.add_value(Some("join argument".into()));
    let join_body = module.add_node(Node::ApplyFun {
        callee: Callee::Closure(join_callee),
        args: vec![Atom::Value(join_argument)],
        return_to: entry_return,
    });
    module.define_continuation(
        join,
        Continuation {
            debug_name: Some("join".into()),
            params: vec![join_callee, join_argument],
            body: join_body,
        },
    );

    let (entry_params, handed) = match known {
        true => (vec![], Atom::Fun(arm)),
        false => {
            let closure = module.add_value(Some("closure".into()));

            (vec![closure], Atom::Value(closure))
        }
    };
    let jump = module.add_node(Node::ApplyCont(Edge {
        target: join,
        args: vec![handed, Atom::Literal(Literal::Nat(Natural::from(7u32)))],
    }));
    let conts = module.add_node(Node::LetCont {
        continuations: vec![join],
        body: jump,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![arm],
        body: conts,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: entry_params,
            return_cont: entry_return,
            body,
            droppable: false,
        },
    );
    module.set_entry(entry);

    (module, jump, arm, entry_return)
}

/// The shape a convoy leaves once its evidence is erased: each arm jumps its function to a join that only calls it. The call is made at the jump instead, with the jump's other argument standing for the join's parameter and the join's return continuation kept, so the function has a known caller that inlining can consume.
#[test]
fn a_known_function_jumped_to_a_join_that_calls_it_is_called_at_the_jump() {
    let (mut module, jump, arm, entry_return) = calling_join(true);

    assert!(forward_calls(&mut module));
    assert!(matches!(
        module.node(jump),
        Some(Node::ApplyFun { callee: Callee::Known(callee), args, return_to })
            if *callee == arm
                && *return_to == entry_return
                && args == &[Atom::Literal(Literal::Nat(Natural::from(7u32)))]
    ));
    module.verify().unwrap();
}

/// A callee still held in a value gains nothing from moving: the call would stay indirect, and the node would only be copied into every jump.
#[test]
fn a_closure_value_jumped_to_a_join_that_calls_it_stays_a_jump() {
    let (mut module, jump, _, _) = calling_join(false);

    assert!(!forward_calls(&mut module));
    assert!(matches!(module.node(jump), Some(Node::ApplyCont(_))));
    module.verify().unwrap();
}

/// A call returning `width` results; only `read` reaches the entry's return sentinel.
fn returning_call(
    width: usize,
    read: Option<usize>,
) -> (Module, NodeId, FunctionId, ContinuationId) {
    let mut module = Module::new();
    let main = module.reserve_function();
    let callee = module.reserve_function();
    let callee_return = module.reserve_continuation();
    let callee_body = module.add_node(Node::ApplyCont(Edge {
        target: callee_return,
        args: (0..width)
            .map(|index| Atom::Literal(Literal::Nat(Natural::from(index as u32))))
            .collect(),
    }));
    module.define_function(
        callee,
        Function {
            debug_name: Some("callee".into()),
            params: vec![],
            return_cont: callee_return,
            body: callee_body,
            droppable: true,
        },
    );
    let main_return = module.reserve_continuation();
    let resume = module.reserve_continuation();
    let params: Vec<_> = (0..width).map(|_| module.add_value(None)).collect();
    let resume_body = module.add_node(Node::ApplyCont(Edge {
        target: main_return,
        args: vec![read.map_or_else(
            || Atom::Literal(Literal::Nat(Natural::from(0u32))),
            |index| Atom::Value(params[index]),
        )],
    }));
    module.define_continuation(
        resume,
        Continuation {
            debug_name: Some("resume".into()),
            params,
            body: resume_body,
        },
    );
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![],
        return_to: resume,
    });
    let resuming = module.add_node(Node::LetCont {
        continuations: vec![resume],
        body: call,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![callee],
        body: resuming,
    });
    module.define_function(
        main,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body,
            droppable: false,
        },
    );
    module.set_entry(main);
    module.verify().unwrap();
    (module, call, callee, resume)
}

#[test]
fn dead_calls_jump_to_the_resume_with_unused_result_padding() {
    let (mut module, call, _, resume) = returning_call(1, None);
    assert!(eliminate_dead_calls(&mut module));
    assert!(matches!(
        module.node(call),
        Some(Node::ApplyCont(Edge { target, args }))
            if *target == resume && args == &vec![Atom::Filler]
    ));
    module.verify().unwrap();
    assert!(!eliminate_dead_calls(&mut module));
}

#[test]
fn dead_calls_pad_every_result_of_a_split_return() {
    let (mut module, call, _, resume) = returning_call(2, None);
    assert!(eliminate_dead_calls(&mut module));
    assert!(matches!(
        module.node(call),
        Some(Node::ApplyCont(Edge { target, args }))
            if *target == resume && args == &vec![Atom::Filler; 2]
    ));
    module.verify().unwrap();
}

#[test]
fn dead_calls_keep_a_call_whose_result_is_read() {
    let (mut module, call, _, _) = returning_call(1, Some(0));
    assert!(!eliminate_dead_calls(&mut module));
    assert!(matches!(module.node(call), Some(Node::ApplyFun { .. })));
    module.verify().unwrap();
}

#[test]
fn dead_calls_keep_a_call_when_only_its_second_result_is_read() {
    let (mut module, call, _, _) = returning_call(2, Some(1));
    assert!(!eliminate_dead_calls(&mut module));
    assert!(matches!(module.node(call), Some(Node::ApplyFun { .. })));
    module.verify().unwrap();
}

#[test]
fn dead_calls_require_the_callees_proof_of_omission() {
    let (mut module, call, callee, _) = returning_call(1, None);
    module.function_mut(callee).unwrap().droppable = false;
    assert!(!eliminate_dead_calls(&mut module));
    assert!(matches!(module.node(call), Some(Node::ApplyFun { .. })));
    module.verify().unwrap();
}

#[test]
fn dead_calls_keep_tail_calls() {
    let (mut module, call, callee, _) = returning_call(1, None);
    let return_to = module
        .function(module.entry().unwrap())
        .unwrap()
        .return_cont;
    module.set_node(
        call,
        Node::ApplyFun {
            callee: Callee::Known(callee),
            args: vec![],
            return_to,
        },
    );
    module.verify().unwrap();
    assert!(!eliminate_dead_calls(&mut module));
    module.verify().unwrap();
}

#[test]
fn dead_calls_keep_unknown_closure_calls() {
    let (mut module, call, _, resume) = returning_call(1, None);
    let closure = module.add_value(None);
    let main = module.entry().unwrap();
    module.function_mut(main).unwrap().params.push(closure);
    module.set_node(
        call,
        Node::ApplyFun {
            callee: Callee::Closure(closure),
            args: vec![],
            return_to: resume,
        },
    );
    module.verify().unwrap();
    assert!(!eliminate_dead_calls(&mut module));
    module.verify().unwrap();
}
