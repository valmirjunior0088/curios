//! What the SCC and known-value analyses derive before any pass rewrites a node.

use {
    super::test_support::call_graph,
    crate::cps::{
        analysis::{analyze_sccs, known_values},
        inline::inline_single_use_continuations,
        optimize::optimize,
    },
    crate::{
        CpsAtom, CpsCallee, CpsContinuation, CpsEdge, CpsFunId, CpsFunction, CpsLiteral, CpsModule,
        CpsNode, atoms,
    },
    std::collections::BTreeMap,
};

#[test]
fn sccs_group_cycles_and_stay_deterministic() {
    // 0 <-> 1 form a cycle; 1 -> 2 and 2 -> 2 leaves 2 a self-looping singleton; 3 is isolated.
    let graph = call_graph(&[(0, &[1]), (1, &[0, 2]), (2, &[2]), (3, &[])]);
    let sccs = analyze_sccs(&graph);

    let component = |function: u32| sccs.component_of[&CpsFunId(function)];
    assert_eq!(component(0), component(1));
    assert_ne!(component(0), component(2));
    assert_ne!(component(2), component(3));
    assert_eq!(sccs.members.len(), 3);
    assert_eq!(
        sccs.members[component(0)],
        vec![CpsFunId(0), CpsFunId(1)],
        "cycle members are reported in CpsFunId order"
    );
    assert_eq!(sccs.members[component(2)], vec![CpsFunId(2)]);

    let again = analyze_sccs(&graph);
    assert_eq!(sccs.component_of, again.component_of);
    assert_eq!(sccs.members, again.members);
}

#[test]
fn known_continuation_values_are_not_substituted_across_scopes() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let seed = module.add_value(Some("seed".into()));
    let forwarding = module.reserve_continuation();
    let forwarded = module.add_value(Some("forwarded".into()));
    let target = module.reserve_continuation();
    let target_param = module.add_value(Some("target".into()));
    let target_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(target_param)],
    }));
    module.define_continuation(
        target,
        CpsContinuation {
            debug_name: Some("target".into()),
            params: vec![target_param],
            body: target_body,
        },
    );
    let forwarding_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target,
        args: vec![CpsAtom::Value(forwarded)],
    }));
    module.define_continuation(
        forwarding,
        CpsContinuation {
            debug_name: Some("forwarding".into()),
            params: vec![forwarded],
            body: forwarding_body,
        },
    );
    let call = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: forwarding,
        args: vec![CpsAtom::Value(seed)],
    }));
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![forwarding, target],
        body: call,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![seed],
            return_cont,
            body,
        },
    );
    module.set_entry(entry);

    optimize(&mut module);

    assert!(
        module
            .nodes()
            .iter()
            .flatten()
            .flat_map(atoms)
            .all(|atom| atom != &CpsAtom::Value(forwarded))
    );
    module.verify().unwrap();
}

/// A continuation parameter that every transfer hands the same literal is known, exactly as a function parameter is. The single-transfer case is still beta-reduced outright — `inline_single_use_continuations` needs no literal to do that — and the analysis recording it first costs nothing; what the analysis is *for* is the multi-transfer join below, which nothing else folds.
#[test]
fn known_value_analysis_records_a_continuation_parameter_every_jump_passes_the_same_literal() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let continuation = module.reserve_continuation();
    let parameter = module.add_value(None);
    let continuation_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(parameter)],
    }));
    module.define_continuation(
        continuation,
        CpsContinuation {
            debug_name: None,
            params: vec![parameter],
            body: continuation_body,
        },
    );
    let call = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: continuation,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(7))],
    }));
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![continuation],
        body: call,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: None,
            params: vec![],
            return_cont,
            body,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();

    assert_eq!(
        known_values(&module).get(&parameter),
        Some(&CpsAtom::Literal(CpsLiteral::Nat(7)))
    );
    assert!(inline_single_use_continuations(&mut module));
    assert!(matches!(
        module.node(call),
        Some(CpsNode::ApplyCont(CpsEdge { args, .. }))
            if args == &[CpsAtom::Literal(CpsLiteral::Nat(7))]
    ));
}

/// A join that is an operation's `return_to` as well as a jump's target learns nothing from the jump, whichever of the two the arena lists first. The call comes first here, which is the order that once left the parameter `Unknown` for the jump to decide.
#[test]
fn a_join_also_reached_by_a_call_result_learns_nothing_from_a_jump() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let callee = module.reserve_function();
    let callee_return = module.reserve_continuation();
    let callee_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: callee_return,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(3))],
    }));
    module.define_function(
        callee,
        CpsFunction {
            debug_name: None,
            params: vec![],
            return_cont: callee_return,
            body: callee_body,
        },
    );
    let join = module.reserve_continuation();
    let received = module.add_value(Some("received".into()));
    let join_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(received)],
    }));
    module.define_continuation(
        join,
        CpsContinuation {
            debug_name: None,
            params: vec![received],
            body: join_body,
        },
    );
    let chooser = module.add_value(Some("chooser".into()));
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(callee),
        args: vec![],
        return_to: join,
    });
    let jump = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: join,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(7))],
    }));
    let calling = module.reserve_continuation();
    let jumping = module.reserve_continuation();
    module.define_continuation(
        calling,
        CpsContinuation {
            debug_name: None,
            params: vec![],
            body: call,
        },
    );
    module.define_continuation(
        jumping,
        CpsContinuation {
            debug_name: None,
            params: vec![],
            body: jump,
        },
    );
    let switch = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(chooser),
        cases: BTreeMap::from([(
            0,
            CpsEdge {
                target: calling,
                args: vec![],
            },
        )]),
        default: Some(CpsEdge {
            target: jumping,
            args: vec![],
        }),
    });
    let arms = module.add_node(CpsNode::LetCont {
        continuations: vec![calling, jumping],
        body: switch,
    });
    let joined = module.add_node(CpsNode::LetCont {
        continuations: vec![join],
        body: arms,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![callee],
        body: joined,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: None,
            params: vec![chooser],
            return_cont,
            body,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();

    assert!(!known_values(&module).contains_key(&received));
}

/// The join `specialize_jump_patterns` or `split_parameters` leaves behind once one tag's jumps are gone: two transfers, both passing tag `1`, into a body that still switches on the tag. The parameter is known, so `rewrite_atoms` turns the switch into one on a literal and the arm the join can never take — which may read the payload in the other tag's vocabulary — folds away before `verify_rows` could see a construction substituted into it.
#[test]
fn a_join_every_transfer_hands_the_same_tag_has_its_tag_known() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let join = module.reserve_continuation();
    let tag = module.add_value(Some("tag".into()));
    let payload = module.add_value(Some("payload".into()));
    let taken = module.reserve_continuation();
    let untaken = module.reserve_continuation();
    let taken_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(payload)],
    }));
    module.define_continuation(
        taken,
        CpsContinuation {
            debug_name: None,
            params: vec![],
            body: taken_body,
        },
    );
    let untaken_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
    }));
    module.define_continuation(
        untaken,
        CpsContinuation {
            debug_name: None,
            params: vec![],
            body: untaken_body,
        },
    );
    let switch = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(tag),
        cases: BTreeMap::from([
            (
                0,
                CpsEdge {
                    target: untaken,
                    args: vec![],
                },
            ),
            (
                1,
                CpsEdge {
                    target: taken,
                    args: vec![],
                },
            ),
        ]),
        default: None,
    });
    let join_body = module.add_node(CpsNode::LetCont {
        continuations: vec![untaken, taken],
        body: switch,
    });
    module.define_continuation(
        join,
        CpsContinuation {
            debug_name: None,
            params: vec![tag, payload],
            body: join_body,
        },
    );
    let chooser = module.add_value(Some("chooser".into()));
    let split = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(chooser),
        cases: BTreeMap::from([(
            0,
            CpsEdge {
                target: join,
                args: vec![
                    CpsAtom::Literal(CpsLiteral::Nat(1)),
                    CpsAtom::Literal(CpsLiteral::Nat(10)),
                ],
            },
        )]),
        default: Some(CpsEdge {
            target: join,
            args: vec![
                CpsAtom::Literal(CpsLiteral::Nat(1)),
                CpsAtom::Literal(CpsLiteral::Nat(20)),
            ],
        }),
    });
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![join],
        body: split,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: None,
            params: vec![chooser],
            return_cont,
            body,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();

    let known = known_values(&module);
    assert_eq!(known.get(&tag), Some(&CpsAtom::Literal(CpsLiteral::Nat(1))));
    assert!(
        !known.contains_key(&payload),
        "the payload differs per transfer"
    );
}
