use {
    super::uncurry_nests,
    crate::{
        Atom, Callee, Continuation, ContinuationId, Edge, Function, FunctionId, Intrinsic, Module,
        Node, NodeId, ValueExpr, ValueId, shrink,
    },
};

fn jump(module: &mut Module, target: ContinuationId, args: Vec<Atom>) -> NodeId {
    module.add_node(Node::ApplyCont(Edge { target, args }))
}

fn continuation(module: &mut Module, params: Vec<ValueId>, body: NodeId) -> ContinuationId {
    module.add_continuation(Continuation {
        debug_name: None,
        params,
        body,
    })
}

/// `g(x0) = (x1) => … => (x{depth}) => x0`, each lambda bound in the one before and returned.
fn nest(module: &mut Module, depth: usize) -> FunctionId {
    let levels = (0..=depth)
        .map(|_| {
            (
                module.reserve_function(),
                module.reserve_continuation(),
                module.add_value(None),
            )
        })
        .collect::<Vec<_>>();
    let mut body = jump(module, levels[depth].1, vec![Atom::Value(levels[0].2)]);
    for level in (0..=depth).rev() {
        let (function, return_cont, param) = levels[level];
        module.define_function(
            function,
            Function {
                debug_name: None,
                params: vec![param],
                return_cont,
                body,
                droppable: false,
            },
        );
        if level > 0 {
            let returned = jump(module, levels[level - 1].1, vec![Atom::Fun(function)]);
            body = module.add_node(Node::LetFun {
                functions: vec![function],
                body: returned,
            });
        }
    }
    levels[0].0
}

/// `callee(argument)`, then `applications` applications of what it returns, in turn, each to `argument`, and `then` with the last result.
fn applied_in_turn(
    module: &mut Module,
    callee: FunctionId,
    argument: &Atom,
    applications: usize,
    then: impl FnOnce(&mut Module, ValueId) -> NodeId,
) -> NodeId {
    let results = (0..=applications)
        .map(|_| module.add_value(None))
        .collect::<Vec<_>>();
    let tail = then(module, results[applications]);
    let mut resume = continuation(module, vec![results[applications]], tail);
    for index in (0..applications).rev() {
        let apply = module.add_node(Node::ApplyFun {
            callee: Callee::Closure(results[index]),
            args: vec![argument.clone()],
            return_to: resume,
        });
        let body = module.add_node(Node::LetCont {
            continuations: vec![resume],
            body: apply,
        });
        resume = continuation(module, vec![results[index]], body);
    }
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![argument.clone()],
        return_to: resume,
    });
    module.add_node(Node::LetCont {
        continuations: vec![resume],
        body: call,
    })
}

/// `main(a) = g(a)…(a) + g(a)…(a)`: a nest `depth` deep, called twice, each call's result applied `first` and `second` times in turn. `named` also builds a tuple holding `g`, which names it.
fn called_twice(depth: usize, first: usize, second: usize, named: bool) -> (Module, FunctionId) {
    let mut module = Module::new();
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let a = module.add_value(Some("a".into()));
    let g = nest(&mut module, depth);
    let argument = Atom::Value(a);

    let mut body = applied_in_turn(&mut module, g, &argument, first, |module, left| {
        applied_in_turn(module, g, &argument, second, |module, right| {
            let sum = module.add_value(None);
            let deliver = jump(module, main_return, vec![Atom::Value(sum)]);
            module.add_node(Node::LetIntrinsic {
                result: sum,
                op: Intrinsic::NatAdd,
                args: vec![Atom::Value(left), Atom::Value(right)],
                next: deliver,
            })
        })
    });
    if named {
        let holder = module.add_value(None);
        body = module.add_node(Node::LetValue {
            result: holder,
            value: ValueExpr::Tuple(vec![Atom::Fun(g)]),
            next: body,
        });
    }
    let body = module.add_node(Node::LetFun {
        functions: vec![g],
        body,
    });
    module.define_function(
        main,
        Function {
            debug_name: Some("main".into()),
            params: vec![a],
            return_cont: main_return,
            body,
            droppable: false,
        },
    );
    module.set_entry(main);
    module.verify().unwrap();
    (module, g)
}

fn closure_calls(module: &Module) -> usize {
    module
        .live_nodes()
        .filter(|(_, node)| {
            matches!(
                node,
                Node::ApplyFun {
                    callee: Callee::Closure(_),
                    ..
                }
            )
        })
        .count()
}

/// Both calls apply the nest in turn to its depth, so it becomes one function of all its parameters, its body the innermost lambda's, and each call passes what its resumes applied; the resumes then hand the answer on, which the shrinker puts in place.
#[test]
fn a_nest_applied_in_turn_at_two_calls_is_one_function_of_its_parameters() {
    let (mut module, g) = called_twice(3, 3, 3, false);

    assert!(uncurry_nests(&mut module));
    module.verify().unwrap();
    let definition = module.function(g).unwrap();
    assert_eq!(
        definition.params.len(),
        4,
        "every level's parameter:\n{module}"
    );
    assert!(matches!(
        module.node(definition.body),
        Some(Node::ApplyCont(Edge { target, args }))
            if *target == definition.return_cont && args == &[Atom::Value(definition.params[0])]
    ));
    assert_eq!(
        closure_calls(&module),
        0,
        "no closure is applied:\n{module}"
    );

    assert!(shrink(&mut module));
    module.verify().unwrap();
    assert_eq!(
        module.live_continuations().count(),
        2,
        "each call keeps the resume it returns to, and the resumes after it are put in place:\n{module}"
    );
}

/// A call applying fewer levels than the nest holds bounds the collapse for every call: the function takes the levels that call applies, and the deeper applications of the other call are left as they were.
#[test]
fn the_fewest_applications_a_call_makes_bound_the_collapse() {
    let (mut module, g) = called_twice(3, 3, 1, false);

    assert!(uncurry_nests(&mut module));
    module.verify().unwrap();
    assert_eq!(module.function(g).unwrap().params.len(), 2);
    assert_eq!(
        closure_calls(&module),
        2,
        "the first call's two deeper applications stand:\n{module}"
    );
}

/// A function named as a value is called where no call site shows, with what no call passes, so its nest stays as written.
#[test]
fn a_nest_named_as_a_value_stays_as_written() {
    let (mut module, g) = called_twice(3, 3, 3, true);

    assert!(!uncurry_nests(&mut module));
    assert_eq!(module.function(g).unwrap().params.len(), 1);
}
