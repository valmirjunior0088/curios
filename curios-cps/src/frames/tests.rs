use {
    super::*,
    crate::{Literal, test_support::*},
    curios_common::{Lineage, Qualifier},
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
        lineage: None,
        debug_name: None,
        params: vec![],
        body: zero_body,
        terminates: false,
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
                lineage: None,
                debug_name: None,
                params: vec![answer],
                body: after_body,
                terminates: false,
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
        lineage: None,
        debug_name: None,
        params: vec![],
        body: again,
        terminates: false,
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
            lineage: None,
            debug_name: Some("count".into()),
            params: vec![n],
            return_cont: count_return,
            body: count_body,
            droppable: Droppable::Never,
            terminates: false,
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
            lineage: None,
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body: main_body,
            droppable: Droppable::Never,
            terminates: false,
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
        lineage: None,
        debug_name: None,
        params: vec![answer],
        body: after_body,
        terminates: false,
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
            lineage: None,
            debug_name: Some("apply".into()),
            params: vec![closure],
            return_cont: apply_return,
            body: apply_body,
            droppable: Droppable::Never,
            terminates: false,
        },
    );

    let leaf_body = module.add_node(Node::ApplyCont(Edge {
        target: leaf_return,
        args: vec![zero()],
    }));
    module.define_function(
        leaf,
        Function {
            lineage: None,
            debug_name: Some("leaf".into()),
            params: vec![],
            return_cont: leaf_return,
            body: leaf_body,
            droppable: Droppable::Never,
            terminates: false,
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
            lineage: None,
            debug_name: Some("again".into()),
            params: vec![],
            return_cont: again_return,
            body: again_body,
            droppable: Droppable::Never,
            terminates: false,
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
            lineage: None,
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body: main_body,
            droppable: Droppable::Never,
            terminates: false,
        },
    );
    module.set_entry(main);
    module.verify().expect("the fixture verifies");
    (module, apply)
}

/// A call of `callee` at `argument`, not in tail position: it resumes a continuation that hands its answer on to `to`.
fn calling(module: &mut Module, callee: FunctionId, argument: Atom, to: ContinuationId) -> NodeId {
    let answer = module.add_value(None);
    let after_body = module.add_node(Node::ApplyCont(Edge {
        target: to,
        args: vec![Atom::Value(answer)],
    }));
    let after = module.add_continuation(Continuation {
        lineage: None,
        debug_name: None,
        params: vec![answer],
        body: after_body,
        terminates: false,
    });
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![argument],
        return_to: after,
    });
    module.add_node(Node::LetCont {
        continuations: vec![after],
        body: call,
    })
}

/// A body answering `at_zero` to `to` where `on` is zero, and running `otherwise` where it is not.
fn switching(
    module: &mut Module,
    on: ValueId,
    at_zero: Atom,
    to: ContinuationId,
    otherwise: NodeId,
) -> NodeId {
    let more = module.add_continuation(Continuation {
        lineage: None,
        debug_name: None,
        params: vec![],
        body: otherwise,
        terminates: false,
    });
    let switch = module.add_node(Node::Switch {
        scrutinee: Atom::Value(on),
        cases: BTreeMap::from([(
            0,
            Edge {
                target: to,
                args: vec![at_zero],
            },
        )]),
        default: Some(Edge {
            target: more,
            args: vec![],
        }),
    });
    module.add_node(Node::LetCont {
        continuations: vec![more],
        body: switch,
    })
}

/// `function` defined as `name`, of `params`, returning through `return_cont` from `body`.
fn define(
    module: &mut Module,
    function: FunctionId,
    name: &str,
    params: Vec<ValueId>,
    return_cont: ContinuationId,
    body: NodeId,
) {
    module.define_function(
        function,
        Function {
            lineage: None,
            debug_name: Some(name.into()),
            params,
            return_cont,
            body,
            droppable: Droppable::Never,
            terminates: false,
        },
    );
}

/// `main` calls `outer(3)`. `outer(n)` defines `inner(m)` and, where `n` is not zero, calls it; `inner` answers `n`, which `outer` binds, where `m` is zero, and calls `outer` back where it is not. Neither call is in tail position: two members of one cycle, one defined within the other.
fn nesting() -> (Module, FunctionId, FunctionId) {
    let mut module = Module::new();
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let outer = module.reserve_function();
    let outer_return = module.reserve_continuation();
    let inner = module.reserve_function();
    let inner_return = module.reserve_continuation();
    let n = module.add_value(Some("n".into()));
    let m = module.add_value(Some("m".into()));

    let back = calling(&mut module, outer, Atom::Value(m), inner_return);
    let inner_body = switching(&mut module, m, Atom::Value(n), inner_return, back);
    define(
        &mut module,
        inner,
        "inner",
        vec![m],
        inner_return,
        inner_body,
    );

    let down = calling(&mut module, inner, Atom::Value(n), outer_return);
    let zero = Atom::Literal(Literal::Nat(Natural::from(0u32)));
    let choose = switching(&mut module, n, zero, outer_return, down);
    let outer_body = module.add_node(Node::LetFun {
        functions: vec![inner],
        body: choose,
    });
    define(
        &mut module,
        outer,
        "outer",
        vec![n],
        outer_return,
        outer_body,
    );

    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(outer),
        args: vec![Atom::Literal(Literal::Nat(Natural::from(3u32)))],
        return_to: main_return,
    });
    let main_body = module.add_node(Node::LetFun {
        functions: vec![outer],
        body: call,
    });
    define(&mut module, main, "main", vec![], main_return, main_body);
    module.set_entry(main);
    module.verify().expect("the fixture verifies");
    (module, outer, inner)
}

/// `main` calls `first(1)`, and `first(x)` and `second(y)` each answer their parameter; `stray` answers its own under the value `second` binds, as `second`'s twin would.
fn straying() -> (Module, FunctionId, FunctionId, FunctionId) {
    let mut module = Module::new();
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let x = module.add_value(Some("x".into()));
    let y = module.add_value(Some("y".into()));
    let answering = |module: &mut Module, name: &str, parameter: ValueId| {
        let function = module.reserve_function();
        let return_cont = module.reserve_continuation();
        let body = module.add_node(Node::ApplyCont(Edge {
            target: return_cont,
            args: vec![Atom::Value(parameter)],
        }));
        define(module, function, name, vec![parameter], return_cont, body);
        function
    };
    let first = answering(&mut module, "first", x);
    let second = answering(&mut module, "second", y);
    let stray = answering(&mut module, "stray", y);

    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(first),
        args: vec![Atom::Literal(Literal::Nat(Natural::from(1u32)))],
        return_to: main_return,
    });
    let main_body = module.add_node(Node::LetFun {
        functions: vec![first, second, stray],
        body: call,
    });
    define(&mut module, main, "main", vec![], main_return, main_body);
    module.set_entry(main);
    (module, first, second, stray)
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

    bound_frames(&mut module);

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

/// A twin runs its function's code on the heap, so it is of its function's lineage: what a reader asks of the declaration finds both.
#[test]
fn a_twin_is_of_its_functions_lineage() {
    let (mut module, count) = counting(false);
    let lineage = Lineage {
        declaration: Qualifier::empty().with("count"),
        function: 0,
    };
    module
        .function_mut(count)
        .expect("count is defined")
        .lineage = Some(lineage);

    bound_frames(&mut module);

    let twin = module
        .twin(count)
        .expect("a function on a cycle has a twin");
    assert_eq!(module.function(twin).unwrap().lineage, Some(lineage));
}

#[test]
fn a_tail_self_call_keeps_no_frame_and_is_left_alone() {
    let (mut module, _) = counting(true);
    let functions = module.live_functions().count();

    bound_frames(&mut module);

    module.verify().expect("the module still verifies");
    assert!(budget_operations(&module).is_empty());
    assert_eq!(module.live_functions().count(), functions);
}

#[test]
fn a_call_on_no_cycle_is_left_alone() {
    let (mut module, _) = helper_called(true);
    let functions = module.live_functions().count();

    bound_frames(&mut module);

    assert!(budget_operations(&module).is_empty());
    assert_eq!(module.live_functions().count(), functions);
}

#[test]
fn a_closure_call_reaching_no_function_on_its_cycle_is_left_alone() {
    let (mut module, _) = applying(false);
    let functions = module.live_functions().count();

    bound_frames(&mut module);

    assert!(budget_operations(&module).is_empty());
    assert_eq!(module.live_functions().count(), functions);
}

#[test]
fn a_recursion_through_a_closure_is_guarded_and_its_members_gain_twins() {
    let (mut module, apply) = applying(true);

    bound_frames(&mut module);

    module.verify().expect("a bounded module verifies");
    assert!(budget_operations(&module).contains(&true));
    assert!(module.twin(apply).is_some(), "apply is on the cycle");
}

#[test]
fn a_weight_is_the_overhead_and_one_for_each_value_its_body_binds() {
    let (module, count) = counting(false);

    assert_eq!(frame_weight(&module, count), FRAME_OVERHEAD + 2);
}

/// A function a member defines is one function however its member is entered: the member's twin copies the member's own body and names what that body defines, so a member defined within another gains its one twin and no copy of either.
#[test]
fn a_member_defined_within_a_member_of_its_cycle_is_twinned_once_and_its_twins_function_shares_it()
{
    let (mut module, outer, inner) = nesting();

    bound_frames(&mut module);

    module.verify().expect("a bounded module verifies");
    let named = |name: &str| {
        module
            .live_functions()
            .filter(|(_, function)| function.debug_name.as_deref() == Some(name))
            .count()
    };
    assert_eq!(
        (named("outer"), named("heap/outer")),
        (1, 1),
        "a member is one function and its twin"
    );
    assert_eq!(
        (named("inner"), named("heap/inner")),
        (1, 1),
        "the member defined within it is one function and its twin"
    );
    let outer_twin = module.twin(outer).expect("outer is on the cycle");
    let inner_twin = module.twin(inner).expect("inner is on the cycle");
    assert!(
        function_nodes(&module, outer_twin)
            .into_iter()
            .any(|node| matches!(
                module.node(node),
                Some(Node::ApplyFun { callee: Callee::Known(callee), .. }) if *callee == inner_twin
            )),
        "outer's twin enters the twin of the function outer defines"
    );
}

/// A twin binds once more what the function it stands for binds, and nothing another function does.
#[test]
fn a_twin_binds_again_what_its_own_function_binds_and_nothing_anothers() {
    let (mut module, _, second, stray) = straying();
    module.record_twin(second, stray);
    module
        .verify()
        .expect("a twin binds what its function binds");

    let (mut module, first, _, stray) = straying();
    module.record_twin(first, stray);
    let refusal = module
        .verify()
        .expect_err("a twin binds nothing another function binds");
    assert!(refusal.0.contains("and again in the twin of"), "{refusal}");
}
