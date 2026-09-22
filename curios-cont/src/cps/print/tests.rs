//! What the printed continuation IR states: the nesting, the elision rule's two clauses, and the spellings that make a transfer legible.

use {
    crate::{
        Atom, Callee, Continuation, ContinuationId, Edge, Function, Intrinsic, Literal, Module,
        Node, NodeId, Row, Slot, ValueExpr,
    },
    curios_num::Natural,
};

fn nat(value: u32) -> Atom {
    Atom::Literal(Literal::Nat(Natural::from(value)))
}

/// A module whose entry is `~f0$main`, so every fixture reads from the same root. The entry's identities are minted first, which is what keeps the expected output stable as a fixture grows.
fn module_with(body_of: impl FnOnce(&mut Module, ContinuationId) -> NodeId) -> Module {
    let mut module = Module::new();
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let body = body_of(&mut module, main_return);
    module.define_function(
        main,
        Function {
            debug_name: Some("main".into()),
            params: Vec::new(),
            return_cont: main_return,
            body,
            droppable: false,
        },
    );
    module.set_entry(main);
    module
}

/// `~f1$helper(n) = n + 1`, bound in the entry and called into a join that exits with the result.
fn representative() -> Module {
    module_with(|module, _| {
        let helper = module.reserve_function();
        let helper_return = module.reserve_continuation();
        let n = module.add_value(Some("n".into()));
        let sum = module.add_value(None);
        let tail = module.add_node(Node::ApplyCont(Edge {
            target: helper_return,
            args: vec![Atom::Value(sum)],
        }));
        let helper_body = module.add_node(Node::LetIntrinsic {
            result: sum,
            op: Intrinsic::NatAdd,
            args: vec![Atom::Value(n), nat(1)],
            next: tail,
        });
        module.define_function(
            helper,
            Function {
                debug_name: Some("helper".into()),
                params: vec![n],
                return_cont: helper_return,
                body: helper_body,
                droppable: false,
            },
        );

        let join = module.reserve_continuation();
        let total = module.add_value(Some("total".into()));
        let join_body = module.add_node(Node::Exit {
            value: Some(Atom::Value(total)),
        });
        module.define_continuation(
            join,
            Continuation {
                debug_name: Some("join".into()),
                params: vec![total],
                body: join_body,
            },
        );

        let call = module.add_node(Node::ApplyFun {
            callee: Callee::Known(helper),
            args: vec![nat(2)],
            return_to: join,
        });
        let scope = module.add_node(Node::LetCont {
            continuations: vec![join],
            body: call,
        });
        module.add_node(Node::LetFun {
            functions: vec![helper],
            body: scope,
        })
    })
}

#[test]
fn a_representative_module_prints_exactly() {
    assert_eq!(
        representative().to_string(),
        "\
entry ~f0$main() =
    let ~f1$helper(~v0$n) =
        let ~v1 = Nat/add(~v0$n, 1);
        return ~v1;
    cont ~k2$join(~v2$total) =
        exit ~v2$total;
    ~f1$helper(2) -> ~k2$join;
"
    );
}

#[test]
fn printing_is_deterministic_across_constructions() {
    assert_eq!(representative().to_string(), representative().to_string());
}

/// A straight chain of bindings, deeper than any recursive printer would survive. The walk is an explicit job stack, so this is a statement about the stack and not about the module.
#[test]
fn a_deep_module_prints_without_native_stack() {
    const DEPTH: usize = 50_000;

    let module = module_with(|module, ret| {
        let mut body = module.add_node(Node::ApplyCont(Edge {
            target: ret,
            args: Vec::new(),
        }));
        for _ in 0..DEPTH {
            let result = module.add_value(None);
            body = module.add_node(Node::LetValue {
                result,
                value: ValueExpr::Literal(Literal::Nat(Natural::zero())),
                next: body,
            });
        }
        body
    });

    assert_eq!(module.to_string().lines().count(), DEPTH + 2);
}

/// The reason continuations nest rather than listing flat: a flat list would show `~v0$x` free in `~k2`.
#[test]
fn a_value_bound_in_an_enclosing_continuation_reads_in_view_of_its_binder() {
    let module = module_with(|module, _| {
        let outer = module.reserve_continuation();
        let x = module.add_value(Some("x".into()));
        let inner = module.reserve_continuation();
        let inner_body = module.add_node(Node::Exit {
            value: Some(Atom::Value(x)),
        });
        module.define_continuation(
            inner,
            Continuation {
                debug_name: Some("inner".into()),
                params: Vec::new(),
                body: inner_body,
            },
        );
        let enter_inner = module.add_node(Node::ApplyCont(Edge {
            target: inner,
            args: Vec::new(),
        }));
        let outer_body = module.add_node(Node::LetCont {
            continuations: vec![inner],
            body: enter_inner,
        });
        module.define_continuation(
            outer,
            Continuation {
                debug_name: Some("outer".into()),
                params: vec![x],
                body: outer_body,
            },
        );
        let enter_outer = module.add_node(Node::ApplyCont(Edge {
            target: outer,
            args: vec![nat(1)],
        }));
        module.add_node(Node::LetCont {
            continuations: vec![outer],
            body: enter_outer,
        })
    });

    assert_eq!(
        module.to_string(),
        "\
entry ~f0$main() =
    cont ~k1$outer(~v0$x) =
        cont ~k2$inner() =
            exit ~v0$x;
        jump ~k2$inner();
    jump ~k1$outer(1);
"
    );
}

/// A continuation with two predecessors is defined once and named at both — the shape no let-binding could carry, and the reason this rung is not printed as ANF.
#[test]
fn a_join_is_defined_once_and_named_at_every_predecessor() {
    let module = module_with(|module, _| {
        let join = module.reserve_continuation();
        let answer = module.add_value(None);
        let join_body = module.add_node(Node::Exit {
            value: Some(Atom::Value(answer)),
        });
        module.define_continuation(
            join,
            Continuation {
                debug_name: Some("join".into()),
                params: vec![answer],
                body: join_body,
            },
        );
        let switch = module.add_node(Node::Switch {
            scrutinee: nat(0),
            cases: [
                (
                    0,
                    Edge {
                        target: join,
                        args: vec![nat(1)],
                    },
                ),
                (
                    1,
                    Edge {
                        target: join,
                        args: vec![nat(2)],
                    },
                ),
            ]
            .into_iter()
            .collect(),
            default: None,
        });
        module.add_node(Node::LetCont {
            continuations: vec![join],
            body: switch,
        })
    });

    assert_eq!(
        module.to_string(),
        "\
entry ~f0$main() =
    cont ~k1$join(~v0) =
        exit ~v0;
    switch 0
    | 0 => ~k1$join(1)
    | 1 => ~k1$join(2)
    end;
"
    );
}

/// A call returning to its own function's sentinel is a tail call, and the sentinel is spelled by the word rather than by a name whose declaration is elsewhere.
#[test]
fn a_call_returning_to_the_sentinel_prints_as_a_tail_call() {
    let module = module_with(|module, ret| {
        let callee = module.reserve_function();
        let callee_return = module.reserve_continuation();
        let callee_body = module.add_node(Node::ApplyCont(Edge {
            target: callee_return,
            args: vec![nat(7)],
        }));
        module.define_function(
            callee,
            Function {
                debug_name: Some("callee".into()),
                params: Vec::new(),
                return_cont: callee_return,
                body: callee_body,
                droppable: false,
            },
        );
        let call = module.add_node(Node::ApplyFun {
            callee: Callee::Known(callee),
            args: Vec::new(),
            return_to: ret,
        });
        module.add_node(Node::LetFun {
            functions: vec![callee],
            body: call,
        })
    });

    assert_eq!(
        module.to_string(),
        "\
entry ~f0$main() =
    let ~f1$callee() =
        return 7;
    return ~f1$callee();
"
    );
}

/// The lowering stamps every continuation with the role it fills and contification stamps the function's own name, so the hint is the block's answer to what it is.
#[test]
fn a_continuation_prints_the_role_its_hint_names() {
    let module = module_with(|module, _| {
        let arm = module.reserve_continuation();
        let arm_body = module.add_node(Node::Exit { value: None });
        module.define_continuation(
            arm,
            Continuation {
                debug_name: Some("arm/cons".into()),
                params: Vec::new(),
                body: arm_body,
            },
        );
        let jump = module.add_node(Node::ApplyCont(Edge {
            target: arm,
            args: Vec::new(),
        }));
        module.add_node(Node::LetCont {
            continuations: vec![arm],
            body: jump,
        })
    });

    let printed = module.to_string();
    assert!(printed.contains("cont ~k1$arm/cons() ="), "{printed}");
    assert!(printed.contains("jump ~k1$arm/cons();"), "{printed}");
}

/// The row table has no liveness, so only what the program builds from is declared — and a declared row's slots may name rows nothing builds, which have to follow it in.
#[test]
fn an_unreached_row_is_not_declared_and_a_slot_pulls_its_row_in() {
    let module = module_with(|module, _| {
        let inner = module.add_row(Row {
            debug_name: Some("Inner".into()),
            slots: vec![Slot::Nat],
        });
        let outer = module.add_row(Row {
            debug_name: Some("Outer".into()),
            slots: vec![Slot::Row(inner)],
        });
        module.add_row(Row {
            debug_name: Some("Unbuilt".into()),
            slots: vec![Slot::Opaque],
        });
        let built = module.add_value(None);
        let exit = module.add_node(Node::Exit {
            value: Some(Atom::Value(built)),
        });
        module.add_node(Node::LetValue {
            result: built,
            value: ValueExpr::Row(outer, vec![Atom::Filler]),
            next: exit,
        })
    });

    assert_eq!(
        module.to_string(),
        "\
row ~r0$Inner(nat)
row ~r1$Outer(~r0)

entry ~f0$main() =
    let ~v0 = ~r1$Outer(pad);
    exit ~v0;
"
    );
}

/// A hole is what a hintless value nothing reads *is*. A hinted one keeps its hint however dead it is, because the hint is the only thing that says where it came from.
#[test]
fn an_unread_binder_spells_a_hole_only_when_it_is_also_hintless() {
    let module = module_with(|module, _| {
        let anonymous = module.add_value(None);
        let named = module.add_value(Some("kept".into()));
        let exit = module.add_node(Node::Exit { value: None });
        let second = module.add_node(Node::LetValue {
            result: named,
            value: ValueExpr::Literal(Literal::Nat(Natural::from(3u32))),
            next: exit,
        });
        module.add_node(Node::LetValue {
            result: anonymous,
            value: ValueExpr::Literal(Literal::Nat(Natural::from(2u32))),
            next: second,
        })
    });

    assert_eq!(
        module.to_string(),
        "\
entry ~f0$main() =
    let _ = 2;
    let ~v1$kept = 3;
    exit;
"
    );
}

/// The faithfulness boundary: the rule may decline to *repeat* a fact, never to hide one. A binding with exactly one reader still prints as a binding.
#[test]
fn a_single_use_binding_is_not_inlined() {
    let module = module_with(|module, _| {
        let only = module.add_value(None);
        let exit = module.add_node(Node::Exit {
            value: Some(Atom::Value(only)),
        });
        module.add_node(Node::LetValue {
            result: only,
            value: ValueExpr::Literal(Literal::Nat(Natural::from(5u32))),
            next: exit,
        })
    });

    assert_eq!(
        module.to_string(),
        "\
entry ~f0$main() =
    let ~v0 = 5;
    exit ~v0;
"
    );
}

/// The node graph is a tree in everything the compiler builds, but the verifier skips a revisit rather than refusing a second parent — so the printer reports the second rather than duplicating the region under it.
#[test]
fn a_node_reached_twice_is_reported_rather_than_duplicated() {
    let module = module_with(|module, _| {
        let shared = module.add_node(Node::Exit { value: None });
        let first = module.reserve_continuation();
        module.define_continuation(
            first,
            Continuation {
                debug_name: Some("first".into()),
                params: Vec::new(),
                body: shared,
            },
        );
        let second = module.reserve_continuation();
        module.define_continuation(
            second,
            Continuation {
                debug_name: Some("second".into()),
                params: Vec::new(),
                body: shared,
            },
        );
        let jump = module.add_node(Node::ApplyCont(Edge {
            target: first,
            args: Vec::new(),
        }));
        module.add_node(Node::LetCont {
            continuations: vec![first, second],
            body: jump,
        })
    });

    assert_eq!(
        module.to_string(),
        "\
entry ~f0$main() =
    cont ~k1$first() =
        exit
    and ~k2$second() =
        <~n0 again>;
    jump ~k1$first();
"
    );
}

/// The fields record is a fact `Module::verify` holds the parameter list to, so it is stated where the list is rather than in a section of its own.
#[test]
fn a_split_parameter_run_prints_as_one_bracketed_group() {
    let module = module_with(|module, _| {
        let target = module.reserve_continuation();
        let head = module.add_value(Some("head".into()));
        let left = module.add_value(None);
        let right = module.add_value(None);
        let body = module.add_node(Node::Exit {
            value: Some(Atom::Value(left)),
        });
        module.define_continuation(
            target,
            Continuation {
                debug_name: None,
                params: vec![head, left, right],
                body,
            },
        );
        module.record_split(target, 1, 2);
        let jump = module.add_node(Node::ApplyCont(Edge {
            target,
            args: vec![nat(1), nat(2), nat(3)],
        }));
        module.add_node(Node::LetCont {
            continuations: vec![target],
            body: jump,
        })
    });

    let printed = module.to_string();
    assert!(
        printed.contains("cont ~k1(~v0$head, [~v1, _]) ="),
        "{printed}"
    );
}

/// A module the lexical walk does not cover is malformed, and this printer is read while debugging exactly those — so what the walk missed prints anyway rather than vanishing.
#[test]
fn a_function_the_walk_never_reaches_still_prints() {
    let mut module = module_with(|module, _| module.add_node(Node::Exit { value: None }));
    let orphan = module.reserve_function();
    let orphan_return = module.reserve_continuation();
    let body = module.add_node(Node::ApplyCont(Edge {
        target: orphan_return,
        args: Vec::new(),
    }));
    module.define_function(
        orphan,
        Function {
            debug_name: Some("orphan".into()),
            params: Vec::new(),
            return_cont: orphan_return,
            body,
            droppable: false,
        },
    );

    assert_eq!(
        module.to_string(),
        "\
entry ~f0$main() =
    exit;

unreached
    let ~f1$orphan() =
        return;
"
    );
}

/// Every operand position spells its own kind, so nothing in a transfer needs a word to say what it is: a function reference is `~f`, a closure call is `~v`, and an unwritten reference slot is `pad`.
#[test]
fn an_operand_names_its_kind_by_its_sigil() {
    let module = module_with(|module, _| {
        let closure = module.add_value(Some("f".into()));
        let target = module.reserve_continuation();
        let result = module.add_value(None);
        let exit = module.add_node(Node::Exit {
            value: Some(Atom::Value(result)),
        });
        module.define_continuation(
            target,
            Continuation {
                debug_name: Some("resume".into()),
                params: vec![result],
                body: exit,
            },
        );
        let call = module.add_node(Node::ApplyFun {
            callee: Callee::Closure(closure),
            args: vec![Atom::Filler, nat(4)],
            return_to: target,
        });
        let scope = module.add_node(Node::LetCont {
            continuations: vec![target],
            body: call,
        });
        module.add_node(Node::LetValue {
            result: closure,
            value: ValueExpr::Tuple(vec![nat(0)]),
            next: scope,
        })
    });

    assert_eq!(
        module.to_string(),
        "\
entry ~f0$main() =
    let ~v0$f = (0);
    cont ~k1$resume(~v1) =
        exit ~v1;
    ~v0$f(pad, 4) -> ~k1$resume;
"
    );
}

/// Reads are projections because that is what they are; the row qualifies its slot by bare identity, as the erased rung's qualified field does.
#[test]
fn a_row_read_and_a_tuple_read_print_as_projections() {
    let module = module_with(|module, _| {
        let row = module.add_row(Row {
            debug_name: Some("Pair".into()),
            slots: vec![Slot::Nat, Slot::Opaque],
        });
        let built = module.add_value(None);
        let field = module.add_value(None);
        let element = module.add_value(None);
        let exit = module.add_node(Node::Exit {
            value: Some(Atom::Value(element)),
        });
        let tuple_read = module.add_node(Node::LetIntrinsic {
            result: element,
            op: Intrinsic::TupleGet(1),
            args: vec![Atom::Value(field)],
            next: exit,
        });
        let row_read = module.add_node(Node::LetIntrinsic {
            result: field,
            op: Intrinsic::RowGet(row, 0),
            args: vec![Atom::Value(built)],
            next: tuple_read,
        });
        module.add_node(Node::LetValue {
            result: built,
            value: ValueExpr::Row(row, vec![nat(1), Atom::Filler]),
            next: row_read,
        })
    });

    assert_eq!(
        module.to_string(),
        "\
row ~r0$Pair(nat, opaque)

entry ~f0$main() =
    let ~v0 = ~r0$Pair(1, pad);
    let ~v1 = ~v0.~r0/0;
    let ~v2 = ~v1.1;
    exit ~v2;
"
    );
}

/// A group binds its members together, and the chain says so: one keyword, `and` between, one `;` closing the lot.
#[test]
fn a_function_group_prints_as_one_and_chain() {
    let module = module_with(|module, _| {
        let mut members = Vec::new();
        for name in ["first", "second"] {
            let function = module.reserve_function();
            let function_return = module.reserve_continuation();
            let body = module.add_node(Node::ApplyCont(Edge {
                target: function_return,
                args: Vec::new(),
            }));
            module.define_function(
                function,
                Function {
                    debug_name: Some(name.into()),
                    params: Vec::new(),
                    return_cont: function_return,
                    body,
                    droppable: false,
                },
            );
            members.push(function);
        }
        let exit = module.add_node(Node::Exit { value: None });
        module.add_node(Node::LetFun {
            functions: members,
            body: exit,
        })
    });

    assert_eq!(
        module.to_string(),
        "\
entry ~f0$main() =
    let ~f1$first() =
        return
    and ~f2$second() =
        return;
    exit;
"
    );
}

/// The identities a reference can name all print; `~n` does not, because a node is named only by the parent whose nesting already places it.
#[test]
fn no_node_identity_appears_in_a_well_formed_dump() {
    let printed = representative().to_string();
    assert!(!printed.contains("~n"), "{printed}");
}

/// Reached by construction rather than by the lowering, so the table cannot answer a Rust variant name for an operation a program can run.
#[test]
fn an_intrinsic_spells_its_carrier_and_operation() {
    use crate::Intrinsic::*;
    use curios_num::Grain;

    for (op, expected) in [
        (NatAdd, "Nat/add"),
        (IntToFlt, "Int/to_flt"),
        (FltOfLeBytes, "Flt/of_le_bytes"),
        (BinLen(Grain::X), "Bytes/len"),
        (BinChunk(Grain::B, 3), "Bits/chunk"),
        (ListConcat(2), "List/concat"),
        (WindowExtent, "Window/extent"),
        (IsImmediate, "Immediate/is"),
    ] {
        let module = module_with(|module, _| {
            let result = module.add_value(None);
            let exit = module.add_node(Node::Exit {
                value: Some(Atom::Value(result)),
            });
            module.add_node(Node::LetIntrinsic {
                result,
                op,
                args: vec![nat(1)],
                next: exit,
            })
        });
        let printed = module.to_string();
        assert!(printed.contains(&format!("= {expected}(1);")), "{printed}");
    }
}

/// Exercised through the arena rather than a fixture, because a deleted continuation leaves its `LetCont` member list empty and the group must then print nothing at all rather than an empty chain.
#[test]
fn an_empty_binding_group_prints_nothing() {
    let module = module_with(|module, _| {
        let exit = module.add_node(Node::Exit { value: None });
        let conts = module.add_node(Node::LetCont {
            continuations: Vec::new(),
            body: exit,
        });
        module.add_node(Node::LetFun {
            functions: Vec::new(),
            body: conts,
        })
    });

    assert_eq!(
        module.to_string(),
        "\
entry ~f0$main() =
    exit;
"
    );
}

/// An edge to the sentinel is a return wherever it stands, including as the arm of a switch — one rule, so the two spellings cannot drift.
#[test]
fn a_switch_arm_to_the_sentinel_spells_the_return() {
    let module = module_with(|module, ret| {
        let other = module.reserve_continuation();
        let other_body = module.add_node(Node::Exit { value: None });
        module.define_continuation(
            other,
            Continuation {
                debug_name: Some("arm".into()),
                params: Vec::new(),
                body: other_body,
            },
        );
        let switch = module.add_node(Node::Switch {
            scrutinee: nat(0),
            cases: [(
                0,
                Edge {
                    target: other,
                    args: Vec::new(),
                },
            )]
            .into_iter()
            .collect(),
            default: Some(Edge {
                target: ret,
                args: vec![nat(9)],
            }),
        });
        module.add_node(Node::LetCont {
            continuations: vec![other],
            body: switch,
        })
    });

    assert_eq!(
        module.to_string(),
        "\
entry ~f0$main() =
    cont ~k1$arm() =
        exit;
    switch 0
    | 0 => ~k1$arm()
    | _ => return 9
    end;
"
    );
}
