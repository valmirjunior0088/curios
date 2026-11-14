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
            droppable: false,
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
            droppable: false,
        },
    );
    module.set_entry(main);
    module.verify().expect("the fixture verifies");
    (module, count)
}

#[test]
fn a_non_tail_self_call_runs_on_its_twin_and_its_function_enters_it() {
    let (mut module, count) = counting(false);

    bound_frames(&mut module);

    module.verify().expect("a module on the heap verifies");
    let twin = module
        .twin(count)
        .expect("a function on a cycle has a twin");
    assert_eq!(
        module.function(twin).unwrap().params.len(),
        module.function(count).unwrap().params.len() + 1,
        "a twin takes its function's parameters and the continuation"
    );
    let entry = module.function(count).unwrap().body;
    let Some(Node::LetFun { body, .. }) = module.node(entry) else {
        panic!("an entry binds the continuation that hands its answer back");
    };
    assert!(
        matches!(module.node(*body), Some(Node::ApplyFun { callee: Callee::Known(callee), .. }) if *callee == twin),
        "an entry calls its twin"
    );
    // The call stands in a continuation lifted out of the twin, so it is looked for among every node.
    let mut operations: Vec<bool> = module
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
        .collect();
    operations.sort();
    assert_eq!(
        operations,
        [false, true],
        "the twin's call asks the budget and hands its weight back"
    );
}

#[test]
fn a_tail_self_call_keeps_no_frame_and_is_left_alone() {
    let (mut module, count) = counting(true);
    let functions = module.live_functions().count();

    bound_frames(&mut module);

    module.verify().expect("the module still verifies");
    assert_eq!(module.twin(count), None);
    assert_eq!(module.live_functions().count(), functions);
}

#[test]
fn a_call_on_no_cycle_is_left_alone() {
    let (mut module, _) = helper_called(true);
    let functions = module.live_functions().count();

    bound_frames(&mut module);

    assert_eq!(module.live_functions().count(), functions);
}

#[test]
fn a_weight_is_the_overhead_and_one_for_each_value_its_body_binds() {
    let (module, count) = counting(false);

    assert_eq!(frame_weight(&module, count), FRAME_OVERHEAD + 2);
}
