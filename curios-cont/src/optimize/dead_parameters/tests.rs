//! Positional cleanup, retained interfaces and the escape boundary between the two phases.

use {
    super::eliminate_dead_parameters,
    crate::{
        Atom, Callee, Continuation, ContinuationId, Edge, Function, FunctionId, Literal, Module,
        Node, NodeId, ValueExpr, ValueId,
    },
    curios_num::Natural,
};

#[test]
fn dead_parameter_elimination_rewrites_known_calls() {
    let mut module = Module::new();
    let main = module.reserve_function();
    let callee = module.reserve_function();
    let kept = module.add_value(Some("kept".into()));
    let removed = module.add_value(Some("removed".into()));
    let callee_return = module.reserve_continuation();
    let callee_body = module.add_node(Node::ApplyCont(Edge {
        target: callee_return,
        args: vec![Atom::Value(kept)],
    }));
    module.define_function(
        callee,
        Function {
            debug_name: Some("callee".into()),
            params: vec![kept, removed],
            return_cont: callee_return,
            body: callee_body,
            droppable: false,
        },
    );
    let main_return = module.reserve_continuation();
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
            Atom::Literal(Literal::Nat(Natural::from(2u32))),
        ],
        return_to: main_return,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![callee],
        body: call,
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

    assert!(eliminate_dead_parameters(&mut module));
    assert_eq!(module.function(callee).unwrap().params, vec![kept]);
    assert!(matches!(
        module.node(call),
        Some(Node::ApplyFun { args, .. })
            if args == &[Atom::Literal(Literal::Nat(Natural::from(1u32)))]
    ));
    module.verify().unwrap();
}

fn nat(n: u32) -> Atom {
    Atom::Literal(Literal::Nat(Natural::from(n)))
}

struct Program {
    module: Module,
    entry: FunctionId,
    returned: ContinuationId,
    functions: Vec<FunctionId>,
    continuations: Vec<ContinuationId>,
}

impl Program {
    fn new() -> Self {
        let mut module = Module::new();
        Self {
            entry: module.reserve_function(),
            returned: module.reserve_continuation(),
            module,
            functions: Vec::new(),
            continuations: Vec::new(),
        }
    }

    fn params(&mut self, count: usize) -> Vec<ValueId> {
        (0..count).map(|_| self.module.add_value(None)).collect()
    }

    fn function(&mut self, params: Vec<ValueId>, result: Atom) -> FunctionId {
        let function = self.module.reserve_function();
        self.functions.push(function);
        let returned = self.module.reserve_continuation();
        let body = self.module.add_node(Node::ApplyCont(Edge {
            target: returned,
            args: vec![result],
        }));
        self.module.define_function(
            function,
            Function {
                debug_name: None,
                params,
                return_cont: returned,
                body,
                droppable: false,
            },
        );
        function
    }

    fn continuation(&mut self, params: Vec<ValueId>, body: NodeId) -> ContinuationId {
        let continuation = self.module.reserve_continuation();
        self.continuations.push(continuation);
        self.module.define_continuation(
            continuation,
            Continuation {
                debug_name: None,
                params,
                body,
            },
        );
        continuation
    }

    fn finish(mut self, mut body: NodeId) -> Module {
        if !self.continuations.is_empty() {
            body = self.module.add_node(Node::LetCont {
                continuations: self.continuations,
                body,
            });
        }
        if !self.functions.is_empty() {
            body = self.module.add_node(Node::LetFun {
                functions: self.functions,
                body,
            });
        }
        self.module.define_function(
            self.entry,
            Function {
                debug_name: None,
                params: vec![],
                return_cont: self.returned,
                body,
                droppable: false,
            },
        );
        self.module.set_entry(self.entry);
        self.module.verify().unwrap();
        self.module
    }
}

#[test]
fn continuation_cleanup_releases_a_function_before_the_escape_snapshot() {
    let mut p = Program::new();
    let params = p.params(2);
    let f = p.function(params.clone(), Atom::Value(params[0]));
    let call = p.module.add_node(Node::ApplyFun {
        callee: Callee::Known(f),
        args: vec![nat(7), nat(99)],
        return_to: p.returned,
    });
    let unused = p.params(1);
    let k = p.continuation(unused.clone(), call);
    let jump = p.module.add_node(Node::ApplyCont(Edge {
        target: k,
        args: vec![Atom::Fun(f)],
    }));
    let mut module = p.finish(jump);
    assert!(eliminate_dead_parameters(&mut module));
    assert!(module.continuation(k).unwrap().params.is_empty());
    assert_eq!(module.function(f).unwrap().params, params[..1]);
    assert!(matches!(module.node(call), Some(Node::ApplyFun {args, ..}) if args == &[nat(7)]));
    assert!(matches!(module.node(jump), Some(Node::ApplyCont(edge)) if edge.args.is_empty()));
    assert!(module.value(unused[0]).is_none());
    assert!(module.value(params[1]).is_none());
    module.verify().unwrap();
    assert!(!eliminate_dead_parameters(&mut module));
}

#[test]
fn function_cleanup_releases_a_function_for_the_next_invocation() {
    let mut p = Program::new();
    let params = p.params(2);
    let f = p.function(params.clone(), Atom::Value(params[0]));
    let unused = p.params(1);
    let g = p.function(unused.clone(), nat(0));
    let call_f = p.module.add_node(Node::ApplyFun {
        callee: Callee::Known(f),
        args: vec![nat(7), nat(99)],
        return_to: p.returned,
    });
    let result = p.params(1);
    let resume = p.continuation(result.clone(), call_f);
    let call_g = p.module.add_node(Node::ApplyFun {
        callee: Callee::Known(g),
        args: vec![Atom::Fun(f)],
        return_to: resume,
    });
    let mut module = p.finish(call_g);
    assert!(eliminate_dead_parameters(&mut module));
    assert!(module.function(g).unwrap().params.is_empty());
    assert_eq!(module.function(f).unwrap().params, params);
    assert!(matches!(module.node(call_g), Some(Node::ApplyFun {args, ..}) if args.is_empty()));
    assert_eq!(module.continuation(resume).unwrap().params, result);
    module.verify().unwrap();
    assert!(eliminate_dead_parameters(&mut module));
    assert_eq!(module.function(f).unwrap().params, params[..1]);
    assert!(matches!(module.node(call_f), Some(Node::ApplyFun {args, ..}) if args == &[nat(7)]));
    module.verify().unwrap();
    assert!(!eliminate_dead_parameters(&mut module));
}

#[test]
fn inert_arguments_into_a_retained_signature_become_fillers() {
    let mut p = Program::new();
    let receiver_params = p.params(1);
    let receiver = p.function(receiver_params.clone(), nat(0));
    let worker = p.module.reserve_function();
    p.functions.push(worker);
    let worker_params = p.params(1);
    let returned = p.module.reserve_continuation();
    let inside = p.module.add_node(Node::ApplyFun {
        callee: Callee::Known(receiver),
        args: vec![Atom::Value(worker_params[0])],
        return_to: returned,
    });
    p.module.define_function(
        worker,
        Function {
            debug_name: None,
            params: worker_params.clone(),
            return_cont: returned,
            body: inside,
            droppable: false,
        },
    );
    let call = p.module.add_node(Node::ApplyFun {
        callee: Callee::Known(worker),
        args: vec![nat(42)],
        return_to: p.returned,
    });
    let escaped = p.params(1)[0];
    let body = p.module.add_node(Node::LetValue {
        result: escaped,
        value: ValueExpr::Tuple(vec![Atom::Fun(receiver)]),
        next: call,
    });
    let mut module = p.finish(body);
    assert!(eliminate_dead_parameters(&mut module));
    assert_eq!(module.function(receiver).unwrap().params, receiver_params);
    assert!(module.function(worker).unwrap().params.is_empty());
    assert!(
        matches!(module.node(inside), Some(Node::ApplyFun {args, ..}) if args == &[Atom::Filler])
    );
    assert!(module.value(worker_params[0]).is_none());
    module.verify().unwrap();
}

#[test]
fn an_indirect_callee_keeps_its_binding_and_arguments() {
    let mut p = Program::new();
    let target = p.function(vec![], nat(42));
    let worker = p.module.reserve_function();
    p.functions.push(worker);
    let params = p.params(2);
    let returned = p.module.reserve_continuation();
    let inside = p.module.add_node(Node::ApplyFun {
        callee: Callee::Closure(params[0]),
        args: vec![],
        return_to: returned,
    });
    p.module.define_function(
        worker,
        Function {
            debug_name: None,
            params: params.clone(),
            return_cont: returned,
            body: inside,
            droppable: false,
        },
    );
    let call = p.module.add_node(Node::ApplyFun {
        callee: Callee::Known(worker),
        args: vec![Atom::Fun(target), nat(99)],
        return_to: p.returned,
    });
    let mut module = p.finish(call);
    assert!(eliminate_dead_parameters(&mut module));
    assert_eq!(module.function(worker).unwrap().params, params[..1]);
    assert!(
        matches!(module.node(inside), Some(Node::ApplyFun {callee: Callee::Closure(value), args, ..}) if *value == params[0] && args.is_empty())
    );
    assert!(
        matches!(module.node(call), Some(Node::ApplyFun {args, ..}) if args == &[Atom::Fun(target)])
    );
    module.verify().unwrap();
}

#[test]
fn recursive_jumps_keep_the_order_of_surviving_arguments() {
    let mut p = Program::new();
    let params = p.params(5);
    let header = p.module.reserve_continuation();
    p.continuations.push(header);
    let body = p.module.add_node(Node::Switch {
        scrutinee: Atom::Value(params[0]),
        cases: [(
            0,
            Edge {
                target: header,
                args: params.iter().copied().map(Atom::Value).collect(),
            },
        )]
        .into(),
        default: Some(Edge {
            target: p.returned,
            args: vec![Atom::Value(params[3])],
        }),
    });
    p.module.define_continuation(
        header,
        Continuation {
            debug_name: None,
            params: params.clone(),
            body,
        },
    );
    let enter = p.module.add_node(Node::ApplyCont(Edge {
        target: header,
        args: (1..=5).map(nat).collect(),
    }));
    let mut module = p.finish(enter);
    assert!(eliminate_dead_parameters(&mut module));
    assert_eq!(
        module.continuation(header).unwrap().params,
        [params[0], params[3]]
    );
    assert!(
        matches!(module.node(enter), Some(Node::ApplyCont(edge)) if edge.args == [nat(1), nat(4)])
    );
    let Node::Switch { cases, default, .. } = module.node(body).unwrap() else {
        panic!()
    };
    assert_eq!(
        cases[&0].args,
        [Atom::Value(params[0]), Atom::Value(params[3])]
    );
    assert_eq!(default.as_ref().unwrap().args, [Atom::Value(params[3])]);
    module.verify().unwrap();
    assert!(!eliminate_dead_parameters(&mut module));
}
