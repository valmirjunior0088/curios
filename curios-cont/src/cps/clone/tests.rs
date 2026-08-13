use {
    super::copy_bodies,
    crate::{CpsAtom, CpsEdge, CpsFunId, CpsFunction, CpsModule, CpsNode, CpsValueExpr},
    std::collections::BTreeSet,
};

/// `outer` binds a local, then defines `inner` inside its own body — and `inner` reads that local. The nesting is what every copier used to refuse, and the shared read is why a nested definition cannot be copied by a separate call: its body names a value the enclosing copy is renaming.
fn nesting_module() -> (CpsModule, CpsFunId, CpsFunId) {
    let mut module = CpsModule::default();
    let param = module.add_value(Some("param".into()));
    let local = module.add_value(Some("local".into()));

    let inner = module.reserve_function();
    let inner_sentinel = module.reserve_continuation();
    let inner_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: inner_sentinel,
        args: vec![CpsAtom::Value(local)],
    }));
    module.define_function(
        inner,
        CpsFunction {
            debug_name: Some("inner".into()),
            params: vec![],
            return_cont: inner_sentinel,
            body: inner_body,
        },
    );

    let outer = module.reserve_function();
    let outer_sentinel = module.reserve_continuation();
    let call = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: outer_sentinel,
        args: vec![CpsAtom::Fun(inner)],
    }));
    let bind = module.add_node(CpsNode::LetFun {
        functions: vec![inner],
        body: call,
    });
    let outer_body = module.add_node(CpsNode::LetValue {
        result: local,
        value: CpsValueExpr::Tuple(vec![CpsAtom::Value(param)]),
        next: bind,
    });
    module.define_function(
        outer,
        CpsFunction {
            debug_name: Some("outer".into()),
            params: vec![param],
            return_cont: outer_sentinel,
            body: outer_body,
        },
    );
    module.set_entry(outer);

    (module, outer, inner)
}

#[test]
fn a_copy_carries_its_own_nested_definition() {
    let (mut module, outer, inner) = nesting_module();
    let copies = copy_bodies(&mut module, &BTreeSet::from([outer]), &BTreeSet::new());

    // The nested definition is a member of the copy, not a shared reference to the original.
    let copied_inner = copies.functions[&inner];
    assert_ne!(copied_inner, inner);
    assert_ne!(copies.functions[&outer], outer);

    // And the copy's own `LetFun` introduces it, so the lexical binding travels with the body.
    let body = module.function(copies.functions[&outer]).unwrap().body;
    let CpsNode::LetValue { next, .. } = *module.node(body).unwrap() else {
        panic!("the copy opens with the local it binds")
    };
    let CpsNode::LetFun { ref functions, .. } = *module.node(next).unwrap() else {
        panic!("the copy introduces its nested definition")
    };
    assert_eq!(functions, &vec![copied_inner]);
}

#[test]
fn a_nested_definition_reads_the_copy_s_value_not_the_original_s() {
    let (mut module, outer, inner) = nesting_module();
    let original_local = {
        let CpsNode::LetValue { result, .. } =
            *module.node(module.function(outer).unwrap().body).unwrap()
        else {
            panic!("the original opens with the local it binds")
        };
        result
    };
    let copies = copy_bodies(&mut module, &BTreeSet::from([outer]), &BTreeSet::new());

    let copied_body = module.function(copies.functions[&inner]).unwrap().body;
    let CpsNode::ApplyCont(ref edge) = *module.node(copied_body).unwrap() else {
        panic!("the nested definition returns the local it read")
    };
    // The whole reason the copiers had to become one: copying this definition separately would have left it naming a value the enclosing copy had already renamed.
    assert_ne!(edge.args, vec![CpsAtom::Value(original_local)]);
}
