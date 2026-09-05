use {
    super::{
        MachineFunction, MachineInstruction, MachineOperand, MachineTerminator, MachineValueId,
        lower,
    },
    crate::{
        CpsAtom, CpsCallee, CpsContinuation, CpsEdge, CpsFunId, CpsFunction, CpsLiteral, CpsModule,
        CpsNode,
        into_wasm::{EmissionHostTarget, EmissionTail},
    },
};

#[test]
fn return_sentinel_becomes_machine_return_without_a_block() {
    let mut source = CpsModule::new();
    let function = source.reserve_function();
    let return_cont = source.reserve_continuation();
    let body = source.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(7))],
    }));
    source.define_function(
        function,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
        },
    );
    source.set_entry(function);

    let machine = lower(&source);
    let function = &machine.functions[&function];
    assert_eq!(function.blocks.len(), 1);
    let MachineTerminator::Return(operands) = &function.blocks[&function.entry].terminator else {
        panic!("the entry block returns")
    };
    assert!(matches!(
        operands.as_slice(),
        [MachineOperand::Literal(CpsLiteral::Nat(7))]
    ));
}

#[test]
fn call_to_return_sentinel_becomes_tail_call_without_resume_state() {
    let mut source = CpsModule::new();
    let main = source.reserve_function();
    let callee = source.reserve_function();

    let callee_return = source.reserve_continuation();
    let callee_body = source.add_node(CpsNode::ApplyCont(CpsEdge {
        target: callee_return,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
    }));
    source.define_function(
        callee,
        CpsFunction {
            debug_name: Some("callee".into()),
            params: vec![],
            return_cont: callee_return,
            body: callee_body,
        },
    );

    let main_return = source.reserve_continuation();
    let main_body = source.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(callee),
        args: vec![],
        return_to: main_return,
    });
    let main_body = source.add_node(CpsNode::LetFun {
        functions: vec![callee],
        body: main_body,
    });
    source.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body: main_body,
        },
    );
    source.set_entry(main);

    let machine = lower(&source);
    let main = &machine.functions[&main];
    assert_eq!(main.blocks.len(), 1);
    assert!(matches!(
        main.blocks[&main.entry].terminator,
        MachineTerminator::TailDirectCall { function, .. } if function == callee
    ));
}

#[test]
fn exit_stays_direct_termination_through_structurization() {
    let mut source = CpsModule::new();
    let main = source.reserve_function();
    let return_cont = source.reserve_continuation();
    let body = source.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Literal(CpsLiteral::Nat(7))),
    });
    source.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
        },
    );
    source.set_entry(main);

    let machine = lower(&source);
    let structured = super::structurize(&machine);
    let (_, function) = structured.funcs().first().unwrap();
    assert!(matches!(
        function.region.tail,
        EmissionTail::Host(EmissionHostTarget::Exit { .. })
    ));
}

fn machine_make_closures(function: &MachineFunction, target: CpsFunId) -> usize {
    function
        .blocks
        .values()
        .flat_map(|block| &block.instructions)
        .filter(|instruction| {
            matches!(
                instruction,
                MachineInstruction::MakeClosure { function, .. } if *function == target
            )
        })
        .count()
}

#[test]
fn repeated_first_class_use_materializes_one_closure() {
    let mut source = CpsModule::new();
    let main = source.reserve_function();
    let target = source.reserve_function();
    let consumer = source.reserve_function();

    let target_return = source.reserve_continuation();
    let target_body = source.add_node(CpsNode::ApplyCont(CpsEdge {
        target: target_return,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
    }));
    source.define_function(
        target,
        CpsFunction {
            debug_name: Some("target".into()),
            params: vec![],
            return_cont: target_return,
            body: target_body,
        },
    );

    let first = source.add_value(Some("first".into()));
    let second = source.add_value(Some("second".into()));
    let consumer_return = source.reserve_continuation();
    let consumer_body = source.add_node(CpsNode::ApplyCont(CpsEdge {
        target: consumer_return,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
    }));
    source.define_function(
        consumer,
        CpsFunction {
            debug_name: Some("consumer".into()),
            params: vec![first, second],
            return_cont: consumer_return,
            body: consumer_body,
        },
    );

    let main_return = source.reserve_continuation();
    let call = source.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(consumer),
        args: vec![CpsAtom::Fun(target), CpsAtom::Fun(target)],
        return_to: main_return,
    });
    let main_body = source.add_node(CpsNode::LetFun {
        functions: vec![target, consumer],
        body: call,
    });
    source.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body: main_body,
        },
    );
    source.set_entry(main);

    let machine = lower(&source);
    assert_eq!(machine_make_closures(&machine.functions[&main], target), 1);
}

#[test]
fn mixed_direct_and_escaping_use_keeps_the_call_direct() {
    let mut source = CpsModule::new();
    let main = source.reserve_function();
    let target = source.reserve_function();

    let target_return = source.reserve_continuation();
    let target_body = source.add_node(CpsNode::ApplyCont(CpsEdge {
        target: target_return,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
    }));
    source.define_function(
        target,
        CpsFunction {
            debug_name: Some("target".into()),
            params: vec![],
            return_cont: target_return,
            body: target_body,
        },
    );

    let main_return = source.reserve_continuation();
    let result = source.add_value(Some("result".into()));
    let escape = source.add_node(CpsNode::ApplyCont(CpsEdge {
        target: main_return,
        args: vec![CpsAtom::Fun(target)],
    }));
    let resume = source.add_continuation(CpsContinuation {
        debug_name: Some("resume".into()),
        params: vec![result],
        body: escape,
    });
    let call = source.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(target),
        args: vec![],
        return_to: resume,
    });
    let with_resume = source.add_node(CpsNode::LetCont {
        continuations: vec![resume],
        body: call,
    });
    let main_body = source.add_node(CpsNode::LetFun {
        functions: vec![target],
        body: with_resume,
    });
    source.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body: main_body,
        },
    );
    source.set_entry(main);

    let machine = lower(&source);
    let main = &machine.functions[&main];
    assert!(main.blocks.values().any(|block| matches!(
        &block.terminator,
        MachineTerminator::DirectCall { function, .. } if *function == target
    )));
    assert_eq!(machine_make_closures(main, target), 1);
}

/// A nullary `main` that immediately exits — the smallest valid machine module, used to seed the verifier-rejection tests.
fn exiting_main() -> (CpsModule, CpsFunId) {
    let mut source = CpsModule::new();
    let main = source.reserve_function();
    let return_cont = source.reserve_continuation();
    let body = source.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Literal(CpsLiteral::Nat(0))),
    });
    source.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
        },
    );
    source.set_entry(main);
    (source, main)
}

#[test]
fn verify_rejects_a_function_without_its_entry_block() {
    let (source, _) = exiting_main();
    let mut machine = lower(&source);

    let entry = machine.entry;
    let function = machine.functions.get_mut(&entry).unwrap();
    let entry_block = function.entry;
    function.blocks.remove(&entry_block);

    let error = machine.verify().unwrap_err();
    assert!(
        error.to_string().contains("has no entry block"),
        "unexpected error: {error}"
    );
}

#[test]
fn verify_rejects_a_nested_block_with_no_lexical_owner() {
    // A `LetCont` continuation becomes a block nested under the entry; drop the scope table and it is left without a lexical owner.
    let mut source = CpsModule::new();
    let main = source.reserve_function();
    let return_cont = source.reserve_continuation();
    let bound = source.add_value(Some("bound".into()));
    let exit = source.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(bound)),
    });
    let resume = source.add_continuation(CpsContinuation {
        debug_name: Some("resume".into()),
        params: vec![bound],
        body: exit,
    });
    let enter = source.add_node(CpsNode::ApplyCont(CpsEdge {
        target: resume,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
    }));
    let body = source.add_node(CpsNode::LetCont {
        continuations: vec![resume],
        body: enter,
    });
    source.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
        },
    );
    source.set_entry(main);

    let mut machine = lower(&source);
    let entry = machine.entry;
    let function = machine.functions.get_mut(&entry).unwrap();
    assert!(
        function.blocks.len() >= 2,
        "fixture must lower to a nested block"
    );
    function.block_scopes.clear();

    let error = machine.verify().unwrap_err();
    assert!(
        error.to_string().contains("has no lexical owner"),
        "unexpected error: {error}"
    );
}

/// A tail position is held to the function's result count. A closure call hands back one value through `return_call_indirect`, so a function widened to two results cannot end in one: the CPS verifier refuses that upstream, and this is the machine-level check that would otherwise be missing beside the one a tail *direct* call already gets.
#[test]
fn verify_rejects_a_tail_closure_call_in_a_function_returning_two_values() {
    let (source, _) = exiting_main();
    let mut machine = lower(&source);
    let entry = machine.entry;
    let function = machine.functions.get_mut(&entry).unwrap();
    function.results = 2;
    let closure = MachineValueId(0);
    let block = function.blocks.get_mut(&function.entry).unwrap();
    block.terminator = MachineTerminator::TailIndirectCall {
        closure,
        args: vec![],
    };

    let error = machine.verify().unwrap_err();
    assert!(
        error
            .to_string()
            .contains("returns 1 values from a tail position"),
        "unexpected error: {error}"
    );
}
