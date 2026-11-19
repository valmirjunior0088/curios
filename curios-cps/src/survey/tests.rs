use {super::*, crate::*, curios_common::Qualifier, curios_num::Natural};

/// The lineage of the `function`th function erasure minted for the declaration at `/<declaration>`.
fn of(declaration: &str, function: u32) -> Lineage {
    Lineage {
        declaration: Qualifier::empty().with(declaration),
        function,
    }
}

/// Define a function of `lineage` in `module` answering zero, through a continuation of `standing` bound in its body where one is given, and hand back its identity.
fn define(module: &mut Module, lineage: Option<Lineage>, standing: Option<Lineage>) -> FunctionId {
    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let result = module.add_value(None);
    let answer = module.add_node(Node::ApplyCont(Edge {
        target: return_cont,
        args: vec![Atom::Value(result)],
    }));
    let mut body = module.add_node(Node::LetValue {
        result,
        value: ValueExpr::Literal(Literal::Nat(Natural::from(0u32))),
        next: answer,
    });
    if let Some(standing) = standing {
        let continuation = module.add_continuation(Continuation {
            lineage: Some(standing),
            debug_name: None,
            params: vec![],
            body,
            terminates: false,
        });
        let jump = module.add_node(Node::ApplyCont(Edge {
            target: continuation,
            args: vec![],
        }));
        body = module.add_node(Node::LetCont {
            continuations: vec![continuation],
            body: jump,
        });
    }
    module.define_function(
        function,
        Function {
            lineage,
            debug_name: None,
            params: vec![],
            return_cont,
            body,
            droppable: Droppable::Never,
            terminates: false,
        },
    );

    function
}

#[test]
fn a_lineages_functions_are_held_together_and_one_of_none_by_nothing() {
    let mut module = Module::new();
    let first = define(&mut module, Some(of("loop", 0)), None);
    let copy = define(&mut module, Some(of("loop", 0)), None);
    define(&mut module, None, None);

    let held = held(&module);
    assert_eq!(held.keys().copied().collect::<Vec<_>>(), [of("loop", 0)]);
    assert_eq!(held[&of("loop", 0)].functions, [first, copy]);
}

/// A twin runs its function on the heap: it is held beside the function, and no copy of it a pass made.
#[test]
fn a_heap_twin_is_held_as_its_functions_twin() {
    let mut module = Module::new();
    let walk = define(&mut module, Some(of("walk", 0)), None);
    let twin = define(&mut module, Some(of("walk", 0)), None);
    module.record_twin(walk, twin);

    let held = &held(&module)[&of("walk", 0)];
    assert_eq!(held.functions, [walk]);
    assert_eq!(
        held.twins,
        [Twin {
            function: twin,
            lifted: vec![]
        }]
    );
}

/// The frame bound lifts a twin's continuations into functions its body binds, of its lineage: they are the twin's, run on the heap, and no copy of the function.
#[test]
fn a_function_a_twin_binds_is_the_twins_and_no_copy() {
    let mut module = Module::new();
    let walk = define(&mut module, Some(of("walk", 0)), None);
    let lifted = define(&mut module, Some(of("walk", 0)), None);
    let twin = define(&mut module, Some(of("walk", 0)), None);
    let body = module.function(twin).unwrap().body;
    let binding = module.add_node(Node::LetFun {
        functions: vec![lifted],
        body,
    });
    module.function_mut(twin).unwrap().body = binding;
    module.record_twin(walk, twin);

    let held = &held(&module)[&of("walk", 0)];
    assert_eq!(held.functions, [walk]);
    assert_eq!(
        held.twins,
        [Twin {
            function: twin,
            lifted: vec![lifted]
        }]
    );
}

/// Where a pass put a function's code in another's body, a continuation of its lineage says so: the function holding it is where it stands, and its own function's body is not.
#[test]
fn a_continuation_of_one_lineage_stands_in_the_function_of_another_binding_it() {
    let mut module = Module::new();
    let caller = define(&mut module, Some(of("main", 0)), Some(of("count", 0)));
    define(&mut module, Some(of("main", 1)), Some(of("main", 1)));

    let held = held(&module);
    assert!(held[&of("count", 0)].functions.is_empty());
    assert_eq!(held[&of("count", 0)].standing_in, BTreeSet::from([caller]));
    assert!(held[&of("main", 1)].standing_in.is_empty());
}

#[test]
fn a_rebuild_is_counted_in_the_body_that_builds_it() {
    let mut module = Module::new();
    let row = module.suspension_row();
    let function = define(&mut module, Some(of("step", 0)), None);
    let rebuilt = module.add_value(None);
    let body = module.function(function).unwrap().body;
    let rebuild = module.add_node(Node::LetValue {
        result: rebuilt,
        value: ValueExpr::Rebuilt {
            of: Atom::Filler,
            row,
            slots: vec![],
        },
        next: body,
    });
    module.function_mut(function).unwrap().body = rebuild;

    assert_eq!(rebuilds(&module, function), 1);
}
