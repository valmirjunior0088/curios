use {
    super::*,
    crate::{Literal, test_support::*},
    curios_num::Natural,
};

/// `main` calls `count(3)`, whose body switches on its parameter: zero answers `0`, anything else calls `count` again, in tail position where `tail` holds and otherwise resuming a continuation that answers what the call answered.
fn counting(tail: bool) -> (Module, FunctionId) {
    let mut module = Module::new();
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let count = module.reserve_function();
    let count_return = module.reserve_continuation();
    let n = module.add_value(Some("n".into()));

    let zero_body = module.add_node(Node::ApplyCont(Edge {
        target: count_return,
        args: vec![Atom::Literal(Literal::Nat(Natural::from(0u32)))],
    }));
    let zero = module.add_continuation(Continuation {
        debug_name: None,
        params: vec![],
        body: zero_body,
    });
    let again = match tail {
        true => module.add_node(Node::ApplyFun {
            callee: Callee::Known(count),
            args: vec![Atom::Value(n)],
            return_to: count_return,
        }),
        false => {
            let answer = module.add_value(None);
            let after_body = module.add_node(Node::ApplyCont(Edge {
                target: count_return,
                args: vec![Atom::Value(answer)],
            }));
            let after = module.add_continuation(Continuation {
                debug_name: None,
                params: vec![answer],
                body: after_body,
            });
            let call = module.add_node(Node::ApplyFun {
                callee: Callee::Known(count),
                args: vec![Atom::Value(n)],
                return_to: after,
            });
            module.add_node(Node::LetCont {
                continuations: vec![after],
                body: call,
            })
        }
    };
    let more = module.add_continuation(Continuation {
        debug_name: None,
        params: vec![],
        body: again,
    });
    let switch = module.add_node(Node::Switch {
        scrutinee: Atom::Value(n),
        cases: BTreeMap::from([(
            0,
            Edge {
                target: zero,
                args: vec![],
            },
        )]),
        default: Some(Edge {
            target: more,
            args: vec![],
        }),
    });
    let count_body = module.add_node(Node::LetCont {
        continuations: vec![zero, more],
        body: switch,
    });
    module.define_function(
        count,
        Function {
            debug_name: Some("count".into()),
            params: vec![n],
            return_cont: count_return,
            body: count_body,
            droppable: Droppable::Never,
        },
    );

    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(count),
        args: vec![Atom::Literal(Literal::Nat(Natural::from(3u32)))],
        return_to: main_return,
    });
    let main_body = module.add_node(Node::LetFun {
        functions: vec![count],
        body: call,
    });
    module.define_function(
        main,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body: main_body,
            droppable: Droppable::Never,
        },
    );
    module.set_entry(main);
    module.verify().expect("the fixture verifies");
    (module, count)
}

/// `apply(c)` calls `c` at no arguments, not in tail position, and answers what it answered; `leaf()` answers `0`; and `again()` tail-calls `apply` with itself where `back` holds and with `leaf` otherwise, naming itself in a list it drops where not, so that a function of no parameters some atom names is what it is either way. `main` calls `apply(leaf)`.
fn applying(back: bool) -> (Module, FunctionId) {
    let zero = || Atom::Literal(Literal::Nat(Natural::from(0u32)));
    let mut module = Module::new();
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let apply = module.reserve_function();
    let apply_return = module.reserve_continuation();
    let leaf = module.reserve_function();
    let leaf_return = module.reserve_continuation();
    let again = module.reserve_function();
    let again_return = module.reserve_continuation();

    let closure = module.add_value(Some("c".into()));
    let answer = module.add_value(None);
    let after_body = module.add_node(Node::ApplyCont(Edge {
        target: apply_return,
        args: vec![Atom::Value(answer)],
    }));
    let after = module.add_continuation(Continuation {
        debug_name: None,
        params: vec![answer],
        body: after_body,
    });
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Closure(closure),
        args: vec![],
        return_to: after,
    });
    let apply_body = module.add_node(Node::LetCont {
        continuations: vec![after],
        body: call,
    });
    module.define_function(
        apply,
        Function {
            debug_name: Some("apply".into()),
            params: vec![closure],
            return_cont: apply_return,
            body: apply_body,
            droppable: Droppable::Never,
        },
    );

    let leaf_body = module.add_node(Node::ApplyCont(Edge {
        target: leaf_return,
        args: vec![zero()],
    }));
    module.define_function(
        leaf,
        Function {
            debug_name: Some("leaf".into()),
            params: vec![],
            return_cont: leaf_return,
            body: leaf_body,
            droppable: Droppable::Never,
        },
    );

    let handed = match back {
        true => again,
        false => leaf,
    };
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(apply),
        args: vec![Atom::Fun(handed)],
        return_to: again_return,
    });
    let again_body = match back {
        true => call,
        false => {
            let named = module.add_value(None);
            module.add_node(Node::LetValue {
                result: named,
                value: ValueExpr::List(vec![Atom::Fun(again)]),
                next: call,
            })
        }
    };
    module.define_function(
        again,
        Function {
            debug_name: Some("again".into()),
            params: vec![],
            return_cont: again_return,
            body: again_body,
            droppable: Droppable::Never,
        },
    );

    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(apply),
        args: vec![Atom::Fun(leaf)],
        return_to: main_return,
    });
    let main_body = module.add_node(Node::LetFun {
        functions: vec![apply, leaf, again],
        body: call,
    });
    module.define_function(
        main,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body: main_body,
            droppable: Droppable::Never,
        },
    );
    module.set_entry(main);
    module.verify().expect("the fixture verifies");
    (module, apply)
}

/// The budget operations the live nodes of `module` hold, each by whether it enters.
fn budget_operations(module: &Module) -> Vec<bool> {
    module
        .live_nodes()
        .filter_map(|(_, node)| match node {
            Node::Intrinsic {
                op: IntrinsicCall::FrameEnter { .. },
                ..
            } => Some(true),
            Node::Intrinsic {
                op: IntrinsicCall::FrameLeave { .. },
                ..
            } => Some(false),
            _ => None,
        })
        .collect()
}

#[test]
fn a_non_tail_self_call_is_guarded_and_its_function_gains_a_twin() {
    let (mut module, count) = counting(false);
    let functions = module.live_functions().count();

    bound_frames(&mut module, &OnceCell::new());

    module.verify().expect("a bounded module verifies");
    let mut operations = budget_operations(&module);
    operations.sort();
    assert_eq!(operations, [false, true]);
    let twin = module
        .twin(count)
        .expect("a function on a cycle has a twin");
    assert_eq!(
        module.function(twin).unwrap().params.len(),
        module.function(count).unwrap().params.len() + 1,
        "a twin takes its function's parameters and the continuation"
    );
    assert!(module.live_functions().count() > functions);
}

#[test]
fn a_tail_self_call_keeps_no_frame_and_is_left_alone() {
    let (mut module, _) = counting(true);
    let functions = module.live_functions().count();

    bound_frames(&mut module, &OnceCell::new());

    module.verify().expect("the module still verifies");
    assert!(budget_operations(&module).is_empty());
    assert_eq!(module.live_functions().count(), functions);
}

#[test]
fn a_call_on_no_cycle_is_left_alone() {
    let (mut module, _) = helper_called(true);
    let functions = module.live_functions().count();

    bound_frames(&mut module, &OnceCell::new());

    assert!(budget_operations(&module).is_empty());
    assert_eq!(module.live_functions().count(), functions);
}

#[test]
fn a_closure_call_reaching_no_function_on_its_cycle_is_left_alone() {
    let (mut module, _) = applying(false);
    let functions = module.live_functions().count();

    bound_frames(&mut module, &OnceCell::new());

    assert!(budget_operations(&module).is_empty());
    assert_eq!(module.live_functions().count(), functions);
}

#[test]
fn a_recursion_through_a_closure_is_guarded_and_its_members_gain_twins() {
    let (mut module, apply) = applying(true);

    bound_frames(&mut module, &OnceCell::new());

    module.verify().expect("a bounded module verifies");
    assert!(budget_operations(&module).contains(&true));
    assert!(module.twin(apply).is_some(), "apply is on the cycle");
}

#[test]
fn a_weight_is_the_overhead_and_one_for_each_value_its_body_binds() {
    let (module, count) = counting(false);

    assert_eq!(frame_weight(&module, count), FRAME_OVERHEAD + 2);
}
