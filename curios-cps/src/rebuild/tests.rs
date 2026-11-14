use {
    super::*,
    crate::{test_support::*, *},
    curios_num::Natural,
};

fn nat(value: u32) -> Atom {
    Atom::Literal(Literal::Nat(Natural::from(value)))
}

/// `bump(t)` reads both slots of a `Pair` and builds a `Pair` of its second; `main` hands it a constant `Pair`, then what that call handed back, which nothing else holds.
fn called_both_ways() -> Module {
    let mut module = Module::new();
    let pair = module.add_row(Row {
        debug_name: Some("Pair".into()),
        slots: vec![Slot::Tag, Slot::Opaque],
    });
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let bump = module.reserve_function();
    let bump_return = module.reserve_continuation();
    let [t, tag, x, n, c, a, b] =
        ["t", "tag", "x", "n", "c", "a", "b"].map(|name| module.add_value(Some(name.into())));
    let handed = module.add_node(Node::ApplyCont(Edge {
        target: bump_return,
        args: vec![Atom::Value(n)],
    }));
    let build = module.add_node(Node::LetValue {
        result: n,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(x)]),
        next: handed,
    });
    let read_x = module.add_node(Node::LetIntrinsic {
        result: x,
        op: Intrinsic::RowGet(pair, 1),
        args: vec![Atom::Value(t)],
        next: build,
    });
    let read_tag = module.add_node(Node::LetIntrinsic {
        result: tag,
        op: Intrinsic::RowGet(pair, 0),
        args: vec![Atom::Value(t)],
        next: read_x,
    });
    module.define_function(
        bump,
        Function {
            debug_name: Some("bump".into()),
            params: vec![t],
            return_cont: bump_return,
            body: read_tag,
            droppable: false,
        },
    );
    let exit = module.add_node(halt_zero());
    let after_second = module.reserve_continuation();
    module.define_continuation(
        after_second,
        Continuation {
            debug_name: None,
            params: vec![b],
            body: exit,
        },
    );
    let second = module.add_node(Node::ApplyFun {
        callee: Callee::Known(bump),
        args: vec![Atom::Value(a)],
        return_to: after_second,
    });
    let second_scope = module.add_node(Node::LetCont {
        continuations: vec![after_second],
        body: second,
    });
    let after_first = module.reserve_continuation();
    module.define_continuation(
        after_first,
        Continuation {
            debug_name: None,
            params: vec![a],
            body: second_scope,
        },
    );
    let first = module.add_node(Node::ApplyFun {
        callee: Callee::Known(bump),
        args: vec![Atom::Value(c)],
        return_to: after_first,
    });
    let first_scope = module.add_node(Node::LetCont {
        continuations: vec![after_first],
        body: first,
    });
    let constant = module.add_node(Node::LetValue {
        result: c,
        value: ValueExpr::Row(pair, vec![nat(0), Atom::Filler]),
        next: first_scope,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![bump],
        body: constant,
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
    module.verify().unwrap();
    module
}

/// A function one call hands a constant and another a value nothing else holds is copied for the second as its reuse version, which rebuilds what it takes apart while the original, still handed the constant, rebuilds nothing; with no allowance nothing is copied and nothing rebuilt.
#[test]
fn a_function_called_both_ways_is_copied_for_its_unique_call_within_the_allowance() {
    for (allowance, functions, rebuilt) in [(REBUILD_COPY_NODE_LIMIT, 3, 1), (0, 2, 0)] {
        let mut module = called_both_ways();
        copy_reuse_versions(&mut module, CopyBudget::new(allowance));
        module.verify().unwrap();
        assert_eq!(module.live_functions().count(), functions, "{module}");
        assert_eq!(rebuilds(&module).len(), rebuilt, "{module}");
    }
}

/// `pick(t)` hands `t` back, by a jump or by both edges of a switch on its tag, and `main` builds a `Pair` beside what a call of it answered: a constant `Pair` where `fresh` is false, and one built of a computed value, which nothing else holds, where it is true.
fn handed_back_by(switch: bool, fresh: bool) -> Module {
    let mut module = Module::new();
    let pair = module.add_row(Row {
        debug_name: Some("Pair".into()),
        slots: vec![Slot::Tag, Slot::Opaque],
    });
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let pick = module.reserve_function();
    let pick_return = module.reserve_continuation();
    let [t, tag, c, v, r, x, y, n] =
        ["t", "tag", "c", "v", "r", "x", "y", "n"].map(|name| module.add_value(Some(name.into())));

    let back = || Edge {
        target: pick_return,
        args: vec![Atom::Value(t)],
    };
    let hands = match switch {
        true => module.add_node(Node::Switch {
            scrutinee: Atom::Value(tag),
            cases: std::collections::BTreeMap::from([(1, back())]),
            default: Some(back()),
        }),
        false => module.add_node(Node::ApplyCont(back())),
    };
    let read_tag = module.add_node(Node::LetIntrinsic {
        result: tag,
        op: Intrinsic::RowGet(pair, 0),
        args: vec![Atom::Value(t)],
        next: hands,
    });
    module.define_function(
        pick,
        Function {
            debug_name: Some("pick".into()),
            params: vec![t],
            return_cont: pick_return,
            body: read_tag,
            droppable: false,
        },
    );

    let exit = module.add_node(halt(vec![Atom::Value(n)]));
    let build = module.add_node(Node::LetValue {
        result: n,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(y)]),
        next: exit,
    });
    let bump = module.add_node(Node::LetIntrinsic {
        result: y,
        op: Intrinsic::NatAdd,
        args: vec![Atom::Value(x), nat(1)],
        next: build,
    });
    let read = module.add_node(Node::LetIntrinsic {
        result: x,
        op: Intrinsic::RowGet(pair, 1),
        args: vec![Atom::Value(r)],
        next: bump,
    });
    let answered = module.reserve_continuation();
    module.define_continuation(
        answered,
        Continuation {
            debug_name: None,
            params: vec![r],
            body: read,
        },
    );
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(pick),
        args: vec![Atom::Value(c)],
        return_to: answered,
    });
    let scope = module.add_node(Node::LetCont {
        continuations: vec![answered],
        body: call,
    });
    let held = module.add_node(Node::LetValue {
        result: c,
        value: ValueExpr::Row(
            pair,
            vec![
                nat(1),
                match fresh {
                    true => Atom::Value(v),
                    false => nat(7),
                },
            ],
        ),
        next: scope,
    });
    let computed = module.add_node(Node::LetIntrinsic {
        result: v,
        op: Intrinsic::NatAdd,
        args: vec![nat(3), nat(4)],
        next: held,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![pick],
        body: computed,
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
    module.verify().unwrap();
    module
}

/// `main` builds a `Pair` holding a `Pair` nothing else holds, and a continuation reads the inner one out of it, reads that one's slot and builds a `Pair` of it beside the inner one dying: once, or, where `looped`, on every round of a loop that binds the outer `Pair` once.
fn read_beside_a_construction(looped: bool) -> Module {
    let mut module = Module::new();
    let pair = module.add_row(Row {
        debug_name: Some("Pair".into()),
        slots: vec![Slot::Tag, Slot::Opaque],
    });
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let [v, inner, outer, k, x, y, n, d] = ["v", "inner", "outer", "k", "x", "y", "n", "d"]
        .map(|name| module.add_value(Some(name.into())));

    let round = module.reserve_continuation();
    let done = module.reserve_continuation();
    let exit = module.add_node(halt(vec![Atom::Value(d)]));
    module.define_continuation(
        done,
        Continuation {
            debug_name: None,
            params: vec![d],
            body: exit,
        },
    );
    let onward = |target| Edge {
        target,
        args: vec![Atom::Value(n)],
    };
    let next = module.add_node(Node::Switch {
        scrutinee: Atom::Value(y),
        cases: std::collections::BTreeMap::from([(0, onward(done))]),
        default: Some(onward(match looped {
            true => round,
            false => done,
        })),
    });
    let build = module.add_node(Node::LetValue {
        result: n,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(y)]),
        next,
    });
    let read_slot = module.add_node(Node::LetIntrinsic {
        result: y,
        op: Intrinsic::RowGet(pair, 1),
        args: vec![Atom::Value(x)],
        next: build,
    });
    let read_inner = module.add_node(Node::LetIntrinsic {
        result: x,
        op: Intrinsic::RowGet(pair, 1),
        args: vec![Atom::Value(outer)],
        next: read_slot,
    });
    module.define_continuation(
        round,
        Continuation {
            debug_name: None,
            params: vec![k],
            body: read_inner,
        },
    );

    let enter = module.add_node(Node::ApplyCont(Edge {
        target: round,
        args: vec![nat(0)],
    }));
    let scope = module.add_node(Node::LetCont {
        continuations: vec![round, done],
        body: enter,
    });
    let holder = module.add_node(Node::LetValue {
        result: outer,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(inner)]),
        next: scope,
    });
    let held = module.add_node(Node::LetValue {
        result: inner,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(v)]),
        next: holder,
    });
    let body = module.add_node(Node::LetIntrinsic {
        result: v,
        op: Intrinsic::NatAdd,
        args: vec![nat(3), nat(4)],
        next: held,
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
    module.verify().unwrap();
    module
}

/// A read a run comes back to with its value still bound names one object on every round, so what a round read is written into by no rebuild: the next round reads it again. Read once, it is rebuilt.
#[test]
fn a_read_a_loop_comes_back_to_names_one_object_on_every_round() {
    assert_eq!(rebuilds(&read_beside_a_construction(false)).len(), 1);
    assert_eq!(rebuilds(&read_beside_a_construction(true)).len(), 0);
}

/// `main` builds a `Pair` nothing else holds over a `Pair` it reads again afterwards, and hands the first to `bump`, which reads its slot and builds a `Pair` of what it read beside its parameter dying. Where `taken_apart`, `bump` also reads a slot of what it read.
fn built_over_a_value_read_again(taken_apart: bool) -> Module {
    let mut module = Module::new();
    let pair = module.add_row(Row {
        debug_name: Some("Pair".into()),
        slots: vec![Slot::Tag, Slot::Opaque],
    });
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let bump = module.reserve_function();
    let bump_return = module.reserve_continuation();
    let [t, tag, x, n, v, s, p, a, z] = ["t", "tag", "x", "n", "v", "s", "p", "a", "z"]
        .map(|name| module.add_value(Some(name.into())));

    let handed = module.add_node(Node::ApplyCont(Edge {
        target: bump_return,
        args: vec![Atom::Value(n)],
    }));
    let build = module.add_node(Node::LetValue {
        result: n,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(x)]),
        next: handed,
    });
    let after_read = match taken_apart {
        true => {
            let result = module.add_value(Some("w".into()));
            module.add_node(Node::LetIntrinsic {
                result,
                op: Intrinsic::RowGet(pair, 1),
                args: vec![Atom::Value(x)],
                next: build,
            })
        }
        false => build,
    };
    let read_x = module.add_node(Node::LetIntrinsic {
        result: x,
        op: Intrinsic::RowGet(pair, 1),
        args: vec![Atom::Value(t)],
        next: after_read,
    });
    let read_tag = module.add_node(Node::LetIntrinsic {
        result: tag,
        op: Intrinsic::RowGet(pair, 0),
        args: vec![Atom::Value(t)],
        next: read_x,
    });
    module.define_function(
        bump,
        Function {
            debug_name: Some("bump".into()),
            params: vec![t],
            return_cont: bump_return,
            body: read_tag,
            droppable: false,
        },
    );

    let exit = module.add_node(halt(vec![Atom::Value(a)]));
    let read_again = module.add_node(Node::LetIntrinsic {
        result: z,
        op: Intrinsic::RowGet(pair, 1),
        args: vec![Atom::Value(s)],
        next: exit,
    });
    let answered = module.reserve_continuation();
    module.define_continuation(
        answered,
        Continuation {
            debug_name: None,
            params: vec![a],
            body: read_again,
        },
    );
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(bump),
        args: vec![Atom::Value(p)],
        return_to: answered,
    });
    let scope = module.add_node(Node::LetCont {
        continuations: vec![answered],
        body: call,
    });
    let holder = module.add_node(Node::LetValue {
        result: p,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(s)]),
        next: scope,
    });
    let held = module.add_node(Node::LetValue {
        result: s,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(v)]),
        next: holder,
    });
    let computed = module.add_node(Node::LetIntrinsic {
        result: v,
        op: Intrinsic::NatAdd,
        args: vec![nat(3), nat(4)],
        next: held,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![bump],
        body: computed,
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
    module.verify().unwrap();
    module
}

/// A value something else holds, stored in a slot whose reads nothing takes apart, shares that slot and leaves the construction over it one nothing else holds, which is rebuilt where it dies. Stored in a slot some reader takes apart, it is a child, and the construction is shared with it.
#[test]
fn a_shared_payload_shares_its_slot_and_a_shared_child_its_construction() {
    assert_eq!(rebuilds(&built_over_a_value_read_again(false)).len(), 1);
    assert_eq!(rebuilds(&built_over_a_value_read_again(true)).len(), 0);
}

/// What a switch's edge hands a function's return is handed back as what a jump hands is: a constant a function returns that way is no value to write into where it is answered, and a value nothing else holds is rebuilt there by either route.
#[test]
fn a_switchs_edge_to_a_return_hands_back_what_a_jump_does() {
    for switch in [false, true] {
        assert_eq!(
            rebuilds(&handed_back_by(switch, false)).len(),
            0,
            "a constant handed back, by a switch's edge: {switch}"
        );
        assert_eq!(
            rebuilds(&handed_back_by(switch, true)).len(),
            1,
            "a value nothing else holds handed back, by a switch's edge: {switch}"
        );
    }
}

/// `main` builds a `Pair` holding a `Pair` nothing else holds and enters a loop over it, whose two arms each read the inner `Pair` out of the loop's parameter, read its slot, and build a `Pair` of what they read beside the inner one dying. Where `back` is false, each arm then builds a `Pair` over what it built and hands it to the next round; where it is true, each hands the loop's parameter back by a join.
fn arms_of_a_round(back: bool) -> Module {
    let mut module = Module::new();
    let pair = module.add_row(Row {
        debug_name: Some("Pair".into()),
        slots: vec![Slot::Tag, Slot::Opaque],
    });
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let [v, inner, outer, p, s, a, y, n, b, z, m, w, d] = [
        "v", "inner", "outer", "p", "s", "a", "y", "n", "b", "z", "m", "w", "d",
    ]
    .map(|name| module.add_value(Some(name.into())));
    let [round, join, done, left, right] = [(); 5].map(|()| module.reserve_continuation());

    // Each arm: the inner `Pair` read out of `p`, its slot read, a `Pair` built of it, and the next round entered.
    let mut arm = |read: ValueId, slot: ValueId, built: ValueId| {
        let next = match back {
            true => module.add_node(Node::ApplyCont(Edge {
                target: join,
                args: vec![Atom::Value(p)],
            })),
            false => {
                let onward = module.add_value(Some("o".into()));
                let enter = module.add_node(Node::ApplyCont(Edge {
                    target: round,
                    args: vec![Atom::Value(onward)],
                }));
                module.add_node(Node::LetValue {
                    result: onward,
                    value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(built)]),
                    next: enter,
                })
            }
        };
        let build = module.add_node(Node::LetValue {
            result: built,
            value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(slot)]),
            next,
        });
        let read_slot = module.add_node(Node::LetIntrinsic {
            result: slot,
            op: Intrinsic::RowGet(pair, 1),
            args: vec![Atom::Value(read)],
            next: build,
        });
        module.add_node(Node::LetIntrinsic {
            result: read,
            op: Intrinsic::RowGet(pair, 1),
            args: vec![Atom::Value(p)],
            next: read_slot,
        })
    };
    let left_body = arm(a, y, n);
    let right_body = arm(b, z, m);
    for (continuation, body) in [(left, left_body), (right, right_body)] {
        module.define_continuation(
            continuation,
            Continuation {
                debug_name: None,
                params: vec![],
                body,
            },
        );
    }
    let to = |target, args| Edge { target, args };
    let pick = module.add_node(Node::Switch {
        scrutinee: Atom::Value(s),
        cases: std::collections::BTreeMap::from([
            (0, to(done, vec![Atom::Value(p)])),
            (1, to(left, vec![])),
        ]),
        default: Some(to(right, vec![])),
    });
    let arms = module.add_node(Node::LetCont {
        continuations: vec![left, right],
        body: pick,
    });
    let read_tag = module.add_node(Node::LetIntrinsic {
        result: s,
        op: Intrinsic::RowGet(pair, 0),
        args: vec![Atom::Value(p)],
        next: arms,
    });
    module.define_continuation(
        round,
        Continuation {
            debug_name: None,
            params: vec![p],
            body: read_tag,
        },
    );
    let again = module.add_node(Node::ApplyCont(to(round, vec![Atom::Value(w)])));
    module.define_continuation(
        join,
        Continuation {
            debug_name: None,
            params: vec![w],
            body: again,
        },
    );
    let exit = module.add_node(halt(vec![Atom::Value(d)]));
    module.define_continuation(
        done,
        Continuation {
            debug_name: None,
            params: vec![d],
            body: exit,
        },
    );

    let enter = module.add_node(Node::ApplyCont(to(round, vec![Atom::Value(outer)])));
    let scope = module.add_node(Node::LetCont {
        continuations: vec![round, join, done],
        body: enter,
    });
    let holder = module.add_node(Node::LetValue {
        result: outer,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(inner)]),
        next: scope,
    });
    let held = module.add_node(Node::LetValue {
        result: inner,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(v)]),
        next: holder,
    });
    let body = module.add_node(Node::LetIntrinsic {
        result: v,
        op: Intrinsic::NatAdd,
        args: vec![nat(3), nat(4)],
        next: held,
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
    module.verify().unwrap();
    module
}

/// A loop's parameter each round binds to another object is read and rebuilt in each arm that takes it apart: the arms of one round exclude each other, and the next round's reads are of another object. Where the loop hands its parameter back by a join, a round reads the object an earlier round read, so nothing it read is written into.
#[test]
fn each_arm_of_a_round_rebuilds_what_it_takes_apart_where_the_round_binds_another_object() {
    assert_eq!(
        rebuilds(&arms_of_a_round(false)).len(),
        4,
        "each arm rebuilds the inner `Pair` it read, and the loop's parameter"
    );
    assert_eq!(
        rebuilds(&arms_of_a_round(true)).len(),
        0,
        "a loop handing its parameter back by a join writes into nothing a round read"
    );
}
