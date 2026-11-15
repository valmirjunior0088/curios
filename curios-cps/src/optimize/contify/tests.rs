//! Turning a function into a continuation, and the call shapes that forbid it.

use {
    super::*,
    crate::{test_support::*, *},
    curios_num::Natural,
    std::collections::BTreeMap,
};

/// A loop's return is the call's once the loop is a continuation: its jumps go there, and so does a switch's edge, which the lowering gives a block of its own where the call returned to the caller's own return.
#[test]
fn contifies_a_single_entry_tail_loop_and_hands_its_returns_to_the_call() {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let loop_function = module.reserve_function();
    let loop_return = module.reserve_continuation();
    let loop_param = module.add_value(Some("loop argument".into()));
    let recur = module.reserve_continuation();
    let recur_param = module.add_value(Some("recur argument".into()));
    let recur_body = module.add_node(Node::ApplyFun {
        callee: Callee::Known(loop_function),
        args: vec![Atom::Value(recur_param)],
        return_to: loop_return,
    });
    module.define_continuation(
        recur,
        Continuation {
            debug_name: Some("recur".into()),
            params: vec![recur_param],
            body: recur_body,
        },
    );
    let switch = module.add_node(Node::Switch {
        scrutinee: Atom::Value(loop_param),
        cases: BTreeMap::from([(
            0,
            Edge {
                target: loop_return,
                args: vec![Atom::Value(loop_param)],
            },
        )]),
        default: Some(Edge {
            target: recur,
            args: vec![Atom::Value(loop_param)],
        }),
    });
    let loop_body = module.add_node(Node::LetCont {
        continuations: vec![recur],
        body: switch,
    });
    module.define_function(
        loop_function,
        Function {
            debug_name: Some("loop".into()),
            params: vec![loop_param],
            return_cont: loop_return,
            body: loop_body,
            droppable: Droppable::Never,
        },
    );
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(loop_function),
        args: vec![Atom::Literal(Literal::Nat(Natural::from(1u32)))],
        return_to: entry_return,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![loop_function],
        body: call,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
            droppable: Droppable::Never,
        },
    );
    module.set_entry(entry);

    optimize(&mut module);

    assert!(module.function(loop_function).is_none());
    let loop_cont = module
        .continuations()
        .iter()
        .flatten()
        .find(|continuation| continuation.debug_name.as_deref() == Some("loop"))
        .unwrap();
    assert_eq!(loop_cont.params, vec![loop_param]);
    let Node::Switch { cases, .. } = module.node(switch).unwrap() else {
        panic!("loop switch changed shape")
    };
    assert_eq!(cases[&0].target, entry_return);
    module.verify().unwrap();
}

#[test]
fn contifies_a_nonrecursive_single_call_function() {
    let (mut module, helper) = helper_called(false);
    assert!(
        contify_calls(&mut module),
        "the single-call helper is contified"
    );
    assert!(
        module.function(helper).is_none(),
        "the contified function is replaced by a local continuation"
    );
    module.verify().unwrap();
}

/// `main` calls `outer` once, `outer` calls `inner` once, and `outer` is minted first so the sweep reaches it first: by `inner`'s turn the function the snapshot names as its owner is gone, contified under `main`. One call contifies both — the owner is resolved through the sweep's own record rather than deferred a round.
#[test]
fn contifies_a_chain_of_single_call_helpers_in_one_sweep() {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let outer = module.reserve_function();
    let outer_return = module.reserve_continuation();
    let inner = module.reserve_function();
    let inner_return = module.reserve_continuation();

    // inner(y) = y
    let y = module.add_value(Some("y".into()));
    let inner_body = module.add_node(Node::ApplyCont(Edge {
        target: inner_return,
        args: vec![Atom::Value(y)],
    }));
    module.define_function(
        inner,
        Function {
            debug_name: Some("inner".into()),
            params: vec![y],
            return_cont: inner_return,
            body: inner_body,
            droppable: Droppable::Never,
        },
    );

    // outer(x) = inner(x)
    let x = module.add_value(Some("x".into()));
    let call_inner = module.add_node(Node::ApplyFun {
        callee: Callee::Known(inner),
        args: vec![Atom::Value(x)],
        return_to: outer_return,
    });
    let outer_body = module.add_node(Node::LetFun {
        functions: vec![inner],
        body: call_inner,
    });
    module.define_function(
        outer,
        Function {
            debug_name: Some("outer".into()),
            params: vec![x],
            return_cont: outer_return,
            body: outer_body,
            droppable: Droppable::Never,
        },
    );

    // main() = outer(0)
    let call_outer = module.add_node(Node::ApplyFun {
        callee: Callee::Known(outer),
        args: vec![Atom::Literal(Literal::Nat(Natural::from(0u32)))],
        return_to: entry_return,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![outer],
        body: call_outer,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
            droppable: Droppable::Never,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();

    assert!(contify_calls(&mut module), "the sweep contifies");
    module.verify().unwrap();
    assert!(
        module.function(outer).is_none() && module.function(inner).is_none(),
        "both helpers are contified in one call:\n{module}"
    );
    assert!(
        !contify_calls(&mut module),
        "and nothing is left for a second"
    );
}

#[test]
fn does_not_contify_a_multi_site_function() {
    // Two return contexts: single-site placement cannot cover both, so the helper stays a function.
    let (mut module, helper) = helper_called(true);
    assert!(
        !contify_calls(&mut module),
        "a function with two call sites is not contified here"
    );
    assert!(module.function(helper).is_some());
}

/// `helper` captures `v`, which `owner` never names: the site is inside `helper`'s `LetFun`, so `v` is in scope there, and the move is admitted on scope rather than on what `owner`'s body mentions.
#[test]
fn contifies_a_callee_whose_capture_the_owner_never_mentions() {
    let (mut module, helper, _) = capture_unmentioned_by_owner();
    assert!(contify_calls(&mut module), "the sweep contifies");
    module.verify().unwrap();
    assert!(
        module.function(helper).is_none(),
        "the capturing helper is contified:\n{module}"
    );
}

#[test]
fn nested_definitions_count_toward_the_copy_limit() {
    let body_limit = SCC_CLONE_NODE_LIMIT - super::CONTIFICATION_NODE_OVERHEAD;
    for size in [body_limit, body_limit + 1] {
        let PolymorphicLoop {
            mut module,
            loop_fn,
            ..
        } = polymorphic_loop(true, 0);
        nest_helpers(&mut module, loop_fn, size);
        assert!(function_nodes(&module, loop_fn).len() < SCC_CLONE_NODE_LIMIT);
        let before = (
            module.nodes().len(),
            module.functions().len(),
            module.continuations().len(),
            module.values().len(),
        );
        let original = module.to_string();
        let mut budget = 1;
        let changed = separate_loop_entries(&mut module, &mut budget);
        assert_eq!(changed, size <= body_limit, "extent of {size} nodes");
        if changed {
            assert_eq!(budget, 0);
            module.verify().unwrap();
        } else {
            assert_eq!(budget, 1);
            assert_eq!(module.to_string(), original);
            assert_eq!(
                before,
                (
                    module.nodes().len(),
                    module.functions().len(),
                    module.continuations().len(),
                    module.values().len()
                )
            );
        }
    }
}

fn arenas(module: &Module) -> [usize; 4] {
    [
        module.nodes().len(),
        module.functions().len(),
        module.continuations().len(),
        module.values().len(),
    ]
}

fn declines_without_minting(mut module: Module, mut budget: usize) {
    module.verify().unwrap();
    let before = arenas(&module);
    let text = module.to_string();
    let remaining = budget;
    assert!(!separate_loop_entries(&mut module, &mut budget));
    assert_eq!(budget, remaining);
    assert_eq!(arenas(&module), before);
    assert_eq!(module.to_string(), text);
    module.verify().unwrap();
}

#[test]
fn a_nested_callback_calling_the_loop_is_not_duplicated() {
    for name in [Some("io/surface"), Some("ordinary loop"), None] {
        let PolymorphicLoop {
            mut module,
            loop_fn,
            ..
        } = polymorphic_loop(true, 0);
        let callback = nest_helpers(&mut module, loop_fn, 0);
        module.function_mut(loop_fn).unwrap().debug_name = name.map(str::to_owned);
        let definition = module.function(callback).unwrap().clone();
        module.set_node(
            definition.body,
            Node::ApplyFun {
                callee: Callee::Known(loop_fn),
                args: module
                    .function(loop_fn)
                    .unwrap()
                    .params
                    .iter()
                    .map(|&p| Atom::Value(p))
                    .collect(),
                return_to: definition.return_cont,
            },
        );
        declines_without_minting(module, 64);
    }
}

#[test]
fn a_loop_copy_is_contified_immediately_with_its_capturing_helpers() {
    let PolymorphicLoop {
        mut module,
        loop_fn,
        call1,
        call2,
    } = polymorphic_loop(true, 0);
    nest_helpers(&mut module, loop_fn, 0);
    let extent = crate::CopyPlan::functions(&module, std::collections::BTreeSet::from([loop_fn]));
    let added = extent.nodes().len() + super::CONTIFICATION_NODE_OVERHEAD;
    let nested = extent.nested().len();
    let before_nodes = module.live_nodes().count();
    let before_functions = module.live_functions().count();
    let mut budget = 2;
    assert!(separate_loop_entries(&mut module, &mut budget));
    assert_eq!(budget, 1);
    assert_eq!(module.live_nodes().count(), before_nodes + added);
    assert_eq!(module.live_functions().count(), before_functions + nested);
    assert_eq!(
        [call1, call2]
            .into_iter()
            .filter(|&site| matches!(module.node(site), Some(Node::LetCont { .. })))
            .count(),
        1
    );
    assert_eq!([call1, call2].into_iter().filter(|&site| matches!(module.node(site), Some(Node::ApplyFun { callee: Callee::Known(target), .. }) if *target == loop_fn)).count(), 1);
    module.verify().unwrap();
    let before = arenas(&module);
    assert!(!separate_loop_entries(&mut module, &mut budget));
    assert_eq!(budget, 1);
    assert_eq!(arenas(&module), before);
    assert!(contify_calls(&mut module));
    assert!(module.function(loop_fn).is_none());
    module.verify().unwrap();
}

/// `main` calling a self-tail loop `calls` times in a row, each call resuming into the next and the last returning: the loop, and its calls.
fn loop_called(calls: usize) -> (Module, FunctionId, Vec<NodeId>) {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let loop_function = module.reserve_function();
    let loop_return = module.reserve_continuation();
    let loop_param = module.add_value(Some("loop argument".into()));
    let recur = module.reserve_continuation();
    let recur_param = module.add_value(Some("recur argument".into()));
    let recur_body = module.add_node(Node::ApplyFun {
        callee: Callee::Known(loop_function),
        args: vec![Atom::Value(recur_param)],
        return_to: loop_return,
    });
    module.define_continuation(
        recur,
        Continuation {
            debug_name: Some("recur".into()),
            params: vec![recur_param],
            body: recur_body,
        },
    );
    let switch = module.add_node(Node::Switch {
        scrutinee: Atom::Value(loop_param),
        cases: BTreeMap::from([(
            0,
            Edge {
                target: loop_return,
                args: vec![Atom::Value(loop_param)],
            },
        )]),
        default: Some(Edge {
            target: recur,
            args: vec![Atom::Value(loop_param)],
        }),
    });
    let loop_body = module.add_node(Node::LetCont {
        continuations: vec![recur],
        body: switch,
    });
    module.define_function(
        loop_function,
        Function {
            debug_name: Some("loop".into()),
            params: vec![loop_param],
            return_cont: loop_return,
            body: loop_body,
            droppable: Droppable::Never,
        },
    );

    // Built last call first: each earlier call resumes into a continuation holding what runs after it.
    let mut rest = module.add_node(Node::ApplyFun {
        callee: Callee::Known(loop_function),
        args: vec![Atom::Literal(Literal::Nat(Natural::from(1u32)))],
        return_to: entry_return,
    });
    let mut sites = vec![rest];
    for _ in 1..calls {
        let answered = module.add_value(None);
        let next = module.add_continuation(Continuation {
            debug_name: Some("next".into()),
            params: vec![answered],
            body: rest,
        });
        let call = module.add_node(Node::ApplyFun {
            callee: Callee::Known(loop_function),
            args: vec![Atom::Literal(Literal::Nat(Natural::from(1u32)))],
            return_to: next,
        });
        sites.push(call);
        rest = module.add_node(Node::LetCont {
            continuations: vec![next],
            body: call,
        });
    }
    let body = module.add_node(Node::LetFun {
        functions: vec![loop_function],
        body: rest,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
            droppable: Droppable::Never,
        },
    );
    module.set_entry(entry);
    (module, loop_function, sites)
}

/// Every extra entry of a loop is given its copy by one call: one call keeps the loop, each other receives a contified copy while the allowance lasts, and a second call finds nothing left.
#[test]
fn every_extra_entry_of_a_loop_is_copied_in_one_call() {
    for (allowance, copies) in [(8, 3), (2, 2)] {
        let (mut module, loop_function, sites) = loop_called(4);
        module.verify().unwrap();
        let mut budget = allowance;
        assert!(separate_loop_entries(&mut module, &mut budget));
        assert_eq!(budget, allowance - copies);
        assert_eq!(
            sites
                .iter()
                .filter(|&&site| matches!(module.node(site), Some(Node::LetCont { .. })))
                .count(),
            copies,
            "an allowance of {allowance}"
        );
        assert_eq!(
            sites
                .iter()
                .filter(|&&site| matches!(module.node(site), Some(Node::ApplyFun { callee: Callee::Known(target), .. }) if *target == loop_function))
                .count(),
            sites.len() - copies
        );
        module.verify().unwrap();
        let before = arenas(&module);
        assert_eq!(
            separate_loop_entries(&mut module, &mut budget),
            copies < sites.len() - 1 && budget > 0
        );
        if budget == 0 {
            assert_eq!(arenas(&module), before);
        }
    }
}

#[test]
fn an_exhausted_entry_budget_does_not_mint_a_copy() {
    let PolymorphicLoop { module, .. } = polymorphic_loop(true, 0);
    declines_without_minting(module, 0);
}

#[test]
fn an_out_of_line_loop_does_not_mint_a_copy() {
    let PolymorphicLoop {
        mut module,
        loop_fn,
        ..
    } = polymorphic_loop(true, 0);
    module.keep_out_of_line(loop_fn);
    declines_without_minting(module, 1);
}

#[test]
fn an_escaping_loop_does_not_mint_a_copy() {
    let PolymorphicLoop {
        mut module,
        loop_fn,
        ..
    } = polymorphic_loop(true, 0);
    let entry = module.function(module.entry().unwrap()).unwrap().body;
    let Node::LetFun { body, .. } = module.node(entry).unwrap() else {
        unreachable!()
    };
    let next = *body;
    let held = module.add_value(None);
    let body = module.add_node(Node::LetValue {
        result: held,
        value: crate::ValueExpr::Tuple(vec![Atom::Fun(loop_fn)]),
        next,
    });
    let Node::LetFun { body: next, .. } = module.node_mut(entry).unwrap() else {
        unreachable!()
    };
    *next = body;
    declines_without_minting(module, 1);
}

#[test]
fn a_nontail_recursive_call_does_not_mint_a_copy() {
    let PolymorphicLoop {
        mut module,
        loop_fn,
        ..
    } = polymorphic_loop(true, 0);
    let definition = module.function(loop_fn).unwrap().clone();
    let resume = module.reserve_continuation();
    let result = module.add_value(None);
    let body = module.add_node(Node::ApplyCont(Edge {
        target: definition.return_cont,
        args: vec![Atom::Value(result)],
    }));
    module.define_continuation(
        resume,
        Continuation {
            debug_name: None,
            params: vec![result],
            body,
        },
    );
    for node in function_nodes(&module, loop_fn) {
        if let Some(Node::ApplyFun {
            callee: Callee::Known(target),
            return_to,
            ..
        }) = module.node_mut(node)
            && *target == loop_fn
        {
            *return_to = resume;
        }
    }
    let body = module.add_node(Node::LetCont {
        continuations: vec![resume],
        body: definition.body,
    });
    module.function_mut(loop_fn).unwrap().body = body;
    declines_without_minting(module, 1);
}

/// `owner(y) = walker(y)` with `walker(x) = walker(owner(x))`: a loop whose step calls back into the function around it, defined by `owner` where `defined_by_owner`, and by `main` beside `owner` otherwise.
fn calling_back(defined_by_owner: bool) -> (Module, FunctionId) {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let owner = module.reserve_function();
    let owner_return = module.reserve_continuation();
    let walker = module.reserve_function();
    let walker_return = module.reserve_continuation();

    let x = module.add_value(Some("x".into()));
    let r = module.add_value(Some("r".into()));
    let again = module.reserve_continuation();
    let recur = module.add_node(Node::ApplyFun {
        callee: Callee::Known(walker),
        args: vec![Atom::Value(r)],
        return_to: walker_return,
    });
    module.define_continuation(
        again,
        Continuation {
            debug_name: Some("again".into()),
            params: vec![r],
            body: recur,
        },
    );
    let call_owner = module.add_node(Node::ApplyFun {
        callee: Callee::Known(owner),
        args: vec![Atom::Value(x)],
        return_to: again,
    });
    let walker_body = module.add_node(Node::LetCont {
        continuations: vec![again],
        body: call_owner,
    });
    module.define_function(
        walker,
        Function {
            debug_name: Some("walker".into()),
            params: vec![x],
            return_cont: walker_return,
            body: walker_body,
            droppable: Droppable::Never,
        },
    );

    let y = module.add_value(Some("y".into()));
    let call_walker = module.add_node(Node::ApplyFun {
        callee: Callee::Known(walker),
        args: vec![Atom::Value(y)],
        return_to: owner_return,
    });
    let owner_body = match defined_by_owner {
        true => module.add_node(Node::LetFun {
            functions: vec![walker],
            body: call_walker,
        }),
        false => call_walker,
    };
    module.define_function(
        owner,
        Function {
            debug_name: Some("owner".into()),
            params: vec![y],
            return_cont: owner_return,
            body: owner_body,
            droppable: Droppable::Never,
        },
    );

    let call_entry = module.add_node(Node::ApplyFun {
        callee: Callee::Known(owner),
        args: vec![Atom::Literal(Literal::Nat(Natural::from(0u32)))],
        return_to: entry_return,
    });
    let functions = match defined_by_owner {
        true => vec![owner],
        false => vec![owner, walker],
    };
    let body = module.add_node(Node::LetFun {
        functions,
        body: call_entry,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
            droppable: Droppable::Never,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();
    (module, walker)
}

/// A loop its caller defines is contified though its step calls back into the caller: it is the caller's loop, and the call back is the caller's own recursion. Mutation-checked: refusing every callee that reaches its caller leaves it a function.
#[test]
fn contifies_a_loop_its_caller_defines_though_its_step_calls_back() {
    let (mut module, walker) = calling_back(true);
    assert!(contify_calls(&mut module), "the loop is contified");
    module.verify().unwrap();
    assert!(
        module.function(walker).is_none(),
        "the loop is a continuation of the function defining it:\n{module}"
    );
}

/// And one another function defines stays a function: calling back into a caller that does not define it is a mutual recursion between two functions, which contification leaves as written.
#[test]
fn does_not_contify_a_function_calling_back_a_caller_that_does_not_define_it() {
    let (mut module, walker) = calling_back(false);
    contify_calls(&mut module);
    assert!(
        module.function(walker).is_some(),
        "the mutual recursion stays two functions:\n{module}"
    );
}
