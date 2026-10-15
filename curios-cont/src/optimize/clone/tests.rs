use {
    super::{CopyPlan, copy_bodies},
    crate::{Atom, Callee, Continuation, Edge, Function, FunctionId, Module, Node, ValueExpr},
    std::collections::BTreeSet,
};

/// `outer` binds a local, then defines `inner` inside its own body — and `inner` reads that local. The shared read is why a nested definition cannot be copied by a separate call: its body names a value the enclosing copy is renaming.
fn nesting_module() -> (Module, FunctionId, FunctionId) {
    let mut module = Module::default();
    let param = module.add_value(Some("param".into()));
    let local = module.add_value(Some("local".into()));

    let inner = module.reserve_function();
    let inner_sentinel = module.reserve_continuation();
    let inner_body = module.add_node(Node::ApplyCont(Edge {
        target: inner_sentinel,
        args: vec![Atom::Value(local)],
    }));
    module.define_function(
        inner,
        Function {
            debug_name: Some("inner".into()),
            params: vec![],
            return_cont: inner_sentinel,
            body: inner_body,
            droppable: false,
        },
    );

    let outer = module.reserve_function();
    let outer_sentinel = module.reserve_continuation();
    let call = module.add_node(Node::ApplyCont(Edge {
        target: outer_sentinel,
        args: vec![Atom::Fun(inner)],
    }));
    let bind = module.add_node(Node::LetFun {
        functions: vec![inner],
        body: call,
    });
    let outer_body = module.add_node(Node::LetValue {
        result: local,
        value: ValueExpr::Tuple(vec![Atom::Value(param)]),
        next: bind,
    });
    module.define_function(
        outer,
        Function {
            debug_name: Some("outer".into()),
            params: vec![param],
            return_cont: outer_sentinel,
            body: outer_body,
            droppable: false,
        },
    );
    module.set_entry(outer);

    (module, outer, inner)
}

#[test]
fn a_copy_carries_its_own_nested_definition() {
    let (mut module, outer, inner) = nesting_module();
    let plan = CopyPlan::functions(&module, BTreeSet::from([outer]));
    let copies = copy_bodies(&mut module, plan);

    // The nested definition is a member of the copy, not a shared reference to the original.
    let copied_inner = copies.functions[&inner];
    assert_ne!(copied_inner, inner);
    assert_ne!(copies.functions[&outer], outer);

    // And the copy's own `LetFun` introduces it, so the lexical binding travels with the body.
    let body = module.function(copies.functions[&outer]).unwrap().body;
    let Node::LetValue { next, .. } = *module.node(body).unwrap() else {
        panic!("the copy opens with the local it binds")
    };
    let Node::LetFun { ref functions, .. } = *module.node(next).unwrap() else {
        panic!("the copy introduces its nested definition")
    };
    assert_eq!(functions, &vec![copied_inner]);
}

#[test]
fn a_nested_definition_reads_the_copy_s_value_not_the_original_s() {
    let (mut module, outer, inner) = nesting_module();
    let original_local = {
        let Node::LetValue { result, .. } =
            *module.node(module.function(outer).unwrap().body).unwrap()
        else {
            panic!("the original opens with the local it binds")
        };
        result
    };
    let plan = CopyPlan::functions(&module, BTreeSet::from([outer]));
    let copies = copy_bodies(&mut module, plan);

    let copied_body = module.function(copies.functions[&inner]).unwrap().body;
    let Node::ApplyCont(ref edge) = *module.node(copied_body).unwrap() else {
        panic!("the nested definition returns the local it read")
    };
    // Copying this definition separately would leave it naming a value the enclosing copy has already renamed, which is why the copiers share one extent.
    assert_ne!(edge.args, vec![Atom::Value(original_local)]);
}

#[test]
fn planning_closes_nested_definitions_transitively_without_minting() {
    let (mut module, outer, inner) = nesting_module();
    let inner_body = module.function(inner).unwrap().body;
    let deeper = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let body = module.add_node(Node::ApplyCont(Edge {
        target: return_cont,
        args: vec![Atom::Filler],
    }));
    module.define_function(
        deeper,
        Function {
            debug_name: None,
            params: vec![],
            return_cont,
            body,
            droppable: false,
        },
    );
    let binding = module.add_node(Node::LetFun {
        functions: vec![deeper],
        body: inner_body,
    });
    module.function_mut(inner).unwrap().body = binding;
    let before = (
        module.nodes().len(),
        module.functions().len(),
        module.continuations().len(),
        module.values().len(),
    );

    let plan = CopyPlan::functions(&module, BTreeSet::from([outer, inner]));

    assert_eq!(plan.nested, BTreeSet::from([inner, deeper]));
    assert_eq!(plan.nodes.len(), module.live_nodes().count());
    assert!(plan.nodes.contains(&body));
    assert_eq!(
        before,
        (
            module.nodes().len(),
            module.functions().len(),
            module.continuations().len(),
            module.values().len(),
        )
    );
}

#[test]
fn function_recursion_follows_the_copy_and_external_callees_stay_shared() {
    let (mut module, outer, inner) = nesting_module();
    let external = module.reserve_function();
    let external_return = module.reserve_continuation();
    let external_body = module.add_node(Node::ApplyCont(Edge {
        target: external_return,
        args: vec![Atom::Filler],
    }));
    module.define_function(
        external,
        Function {
            debug_name: None,
            params: vec![],
            return_cont: external_return,
            body: external_body,
            droppable: false,
        },
    );
    let inner_body = module.function(inner).unwrap().body;
    module.set_node(
        inner_body,
        Node::ApplyFun {
            callee: Callee::Known(external),
            args: vec![],
            return_to: module.function(inner).unwrap().return_cont,
        },
    );
    let root = module.function(outer).unwrap().body;
    let recur = module.add_node(Node::ApplyFun {
        callee: Callee::Known(outer),
        args: vec![Atom::Value(module.function(outer).unwrap().params[0])],
        return_to: module.function(outer).unwrap().return_cont,
    });
    let Node::LetValue { next, .. } = module.node_mut(root).unwrap() else {
        unreachable!()
    };
    let binding = *next;
    let Node::LetFun { body, .. } = module.node_mut(binding).unwrap() else {
        unreachable!()
    };
    *body = recur;

    let plan = CopyPlan::functions(&module, BTreeSet::from([outer]));
    let copies = copy_bodies(&mut module, plan);
    let copied_outer = copies.functions[&outer];
    let Node::LetValue { next, .. } = module
        .node(module.function(copied_outer).unwrap().body)
        .unwrap()
    else {
        unreachable!()
    };
    let Node::LetFun { body, .. } = module.node(*next).unwrap() else {
        unreachable!()
    };
    assert!(
        matches!(module.node(*body), Some(Node::ApplyFun { callee: Callee::Known(target), .. }) if *target == copied_outer)
    );
    assert!(
        matches!(module.node(module.function(copies.functions[&inner]).unwrap().body), Some(Node::ApplyFun { callee: Callee::Known(target), .. }) if *target == external)
    );
    assert!(!copies.functions.contains_key(&external));
}

#[test]
fn a_peeled_continuation_keeps_its_backedge_and_renames_nested_captures() {
    let (mut module, outer, inner) = nesting_module();
    let target = module.reserve_continuation();
    let param = module.add_value(None);
    let recur = module.add_node(Node::ApplyCont(Edge {
        target,
        args: vec![Atom::Value(param)],
    }));
    let body = module.add_node(Node::LetFun {
        functions: vec![inner],
        body: recur,
    });
    module.define_continuation(
        target,
        Continuation {
            debug_name: None,
            params: vec![param],
            body,
        },
    );
    let inner_body = module.function(inner).unwrap().body;
    module.set_node(
        inner_body,
        Node::ApplyCont(Edge {
            target: module.function(inner).unwrap().return_cont,
            args: vec![
                Atom::Value(param),
                Atom::Value(module.function(outer).unwrap().params[0]),
            ],
        }),
    );

    let plan = CopyPlan::continuation(&module, target);
    let copies = copy_bodies(&mut module, plan);
    let copy = module.continuation(copies.continuations[&target]).unwrap();
    let copied_param = copy.params[0];
    assert_ne!(copied_param, param);
    let Node::LetFun { body, .. } = module.node(copy.body).unwrap() else {
        unreachable!()
    };
    assert!(
        matches!(module.node(*body), Some(Node::ApplyCont(edge)) if edge.target == target && edge.args == vec![Atom::Value(copied_param)])
    );
    let Node::ApplyCont(edge) = module
        .node(module.function(copies.functions[&inner]).unwrap().body)
        .unwrap()
    else {
        unreachable!()
    };
    assert_eq!(
        edge.args,
        vec![
            Atom::Value(copied_param),
            Atom::Value(module.function(outer).unwrap().params[0])
        ]
    );
}
