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
            droppable: Droppable::Never,
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
            droppable: Droppable::Never,
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
        assert_eq!(in_place(&module).rows.len(), rebuilt, "{module}");
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
            droppable: Droppable::Never,
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
            droppable: Droppable::Never,
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
            droppable: Droppable::Never,
        },
    );
    module.set_entry(main);
    module.verify().unwrap();
    module
}

/// A read a run comes back to with its value still bound names one object on every round, so what a round read is written into by no rebuild: the next round reads it again. Read once, it is rebuilt.
#[test]
fn a_read_a_loop_comes_back_to_names_one_object_on_every_round() {
    assert_eq!(in_place(&read_beside_a_construction(false)).rows.len(), 1);
    assert_eq!(in_place(&read_beside_a_construction(true)).rows.len(), 0);
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
            droppable: Droppable::Never,
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
            droppable: Droppable::Never,
        },
    );
    module.set_entry(main);
    module.verify().unwrap();
    module
}

/// A value something else holds, stored in a slot whose reads nothing takes apart, shares that slot and leaves the construction over it one nothing else holds, which is rebuilt where it dies. Stored in a slot some reader takes apart, it is a child, and the construction is shared with it.
#[test]
fn a_shared_payload_shares_its_slot_and_a_shared_child_its_construction() {
    assert_eq!(
        in_place(&built_over_a_value_read_again(false)).rows.len(),
        1
    );
    assert_eq!(in_place(&built_over_a_value_read_again(true)).rows.len(), 0);
}

/// What a switch's edge hands a function's return is handed back as what a jump hands is: a constant a function returns that way is no value to write into where it is answered, and a value nothing else holds is rebuilt there by either route.
#[test]
fn a_switchs_edge_to_a_return_hands_back_what_a_jump_does() {
    for switch in [false, true] {
        assert_eq!(
            in_place(&handed_back_by(switch, false)).rows.len(),
            0,
            "a constant handed back, by a switch's edge: {switch}"
        );
        assert_eq!(
            in_place(&handed_back_by(switch, true)).rows.len(),
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
            droppable: Droppable::Never,
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
        in_place(&arms_of_a_round(false)).rows.len(),
        4,
        "each arm rebuilds the inner `Pair` it read, and the loop's parameter"
    );
    assert_eq!(
        in_place(&arms_of_a_round(true)).rows.len(),
        0,
        "a loop handing its parameter back by a join writes into nothing a round read"
    );
}

/// `main` enters a loop with one number as both its counter and the accumulator each round builds a `Pair` over, and the loop hands what it built to a continuation that reads its slot and builds a `Pair` of it beside it dying.
fn built_over_a_number_handed_twice() -> Module {
    let mut module = Module::new();
    let pair = module.add_row(Row {
        debug_name: Some("Pair".into()),
        slots: vec![Slot::Tag, Slot::Opaque],
    });
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let [x, n, acc, k, l, r, y, m] =
        ["x", "n", "acc", "k", "l", "r", "y", "m"].map(|name| module.add_value(Some(name.into())));
    let [build, step, take] = [(); 3].map(|()| module.reserve_continuation());
    let to = |target, args| Edge { target, args };

    let exit = module.add_node(halt_zero());
    let rebuilt = module.add_node(Node::LetValue {
        result: m,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(y)]),
        next: exit,
    });
    let read = module.add_node(Node::LetIntrinsic {
        result: y,
        op: Intrinsic::RowGet(pair, 1),
        args: vec![Atom::Value(r)],
        next: rebuilt,
    });
    module.define_continuation(
        take,
        Continuation {
            debug_name: None,
            params: vec![r],
            body: read,
        },
    );
    let again = module.add_node(Node::ApplyCont(to(
        build,
        vec![Atom::Value(k), Atom::Value(l)],
    )));
    let link = module.add_node(Node::LetValue {
        result: l,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(acc)]),
        next: again,
    });
    let count = module.add_node(Node::LetIntrinsic {
        result: k,
        op: Intrinsic::NatSub,
        args: vec![Atom::Value(n), nat(1)],
        next: link,
    });
    module.define_continuation(
        step,
        Continuation {
            debug_name: None,
            params: vec![],
            body: count,
        },
    );
    let pick = module.add_node(Node::Switch {
        scrutinee: Atom::Value(n),
        cases: std::collections::BTreeMap::from([(0, to(take, vec![Atom::Value(acc)]))]),
        default: Some(to(step, vec![])),
    });
    let round = module.add_node(Node::LetCont {
        continuations: vec![step],
        body: pick,
    });
    module.define_continuation(
        build,
        Continuation {
            debug_name: None,
            params: vec![n, acc],
            body: round,
        },
    );

    let enter = module.add_node(Node::ApplyCont(to(
        build,
        vec![Atom::Value(x), Atom::Value(x)],
    )));
    let scope = module.add_node(Node::LetCont {
        continuations: vec![build, take],
        body: enter,
    });
    let body = module.add_node(Node::LetIntrinsic {
        result: x,
        op: Intrinsic::NatAdd,
        args: vec![nat(3), nat(4)],
        next: scope,
    });
    module.define_function(
        main,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body,
            droppable: Droppable::Never,
        },
    );
    module.set_entry(main);
    module.verify().unwrap();
    module
}

/// A number handed at two parameters of one entry holds nothing a rebuild writes, so it shares neither: the accumulator each round builds over is a value nothing else holds, and the continuation the loop hands it to rebuilds it.
#[test]
fn a_number_handed_at_two_parameters_shares_nothing_built_over_it() {
    assert_eq!(in_place(&built_over_a_number_handed_twice()).rows.len(), 1);
}

/// `main` builds `p`, a `Pair` over a `Pair` `inner`, reads `p`'s second slot into `held`, and enters `k` with `held` at `a` and `p` at `b`, or, where `apart`, with the same read of another such `Pair`, `q`, at `a`. `k` reads `b`'s second slot, reads that one's slot, builds a `Pair` of it beside what it read dying, then reads `a`'s slot and halts on it.
fn child_beside_its_holder(apart: bool) -> Module {
    let mut module = Module::new();
    let pair = module.add_row(Row {
        debug_name: Some("Pair".into()),
        slots: vec![Slot::Tag, Slot::Opaque],
    });
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let [
        x,
        inner_p,
        inner_q,
        p,
        q,
        held,
        a,
        b,
        b_inner,
        v,
        built,
        a_slot,
    ] = [
        "x", "inner_p", "inner_q", "p", "q", "held", "a", "b", "b_inner", "v", "built", "a_slot",
    ]
    .map(|name| module.add_value(Some(name.into())));

    let exit = module.add_node(halt(vec![Atom::Value(a_slot)]));
    let read_a = module.add_node(Node::LetIntrinsic {
        result: a_slot,
        op: Intrinsic::RowGet(pair, 1),
        args: vec![Atom::Value(a)],
        next: exit,
    });
    let build = module.add_node(Node::LetValue {
        result: built,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(v)]),
        next: read_a,
    });
    let read_v = module.add_node(Node::LetIntrinsic {
        result: v,
        op: Intrinsic::RowGet(pair, 1),
        args: vec![Atom::Value(b_inner)],
        next: build,
    });
    let read_b = module.add_node(Node::LetIntrinsic {
        result: b_inner,
        op: Intrinsic::RowGet(pair, 1),
        args: vec![Atom::Value(b)],
        next: read_v,
    });
    let k = module.reserve_continuation();
    module.define_continuation(
        k,
        Continuation {
            debug_name: Some("k".into()),
            params: vec![a, b],
            body: read_b,
        },
    );
    let enter = module.add_node(Node::ApplyCont(Edge {
        target: k,
        args: vec![Atom::Value(held), Atom::Value(p)],
    }));
    let scope = module.add_node(Node::LetCont {
        continuations: vec![k],
        body: enter,
    });
    let read_held = module.add_node(Node::LetIntrinsic {
        result: held,
        op: Intrinsic::RowGet(pair, 1),
        args: vec![Atom::Value(if apart { q } else { p })],
        next: scope,
    });
    let build_q = module.add_node(Node::LetValue {
        result: q,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(inner_q)]),
        next: read_held,
    });
    let build_p = module.add_node(Node::LetValue {
        result: p,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(inner_p)]),
        next: build_q,
    });
    let build_inner_q = module.add_node(Node::LetValue {
        result: inner_q,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(x)]),
        next: build_p,
    });
    let build_inner_p = module.add_node(Node::LetValue {
        result: inner_p,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(x)]),
        next: build_inner_q,
    });
    let body = module.add_node(Node::LetIntrinsic {
        result: x,
        op: Intrinsic::NatAdd,
        args: vec![nat(3), nat(4)],
        next: build_inner_p,
    });
    module.define_function(
        main,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body,
            droppable: Droppable::Never,
        },
    );
    module.set_entry(main);
    module.verify().unwrap();
    module
}

/// A child read out of a value and handed beside it names what the value holds in that slot, so a rebuild of what another name reads out of that slot would write what the first still reads. Mutation-checked: with a value consumed while a field read out of it still names what it read left unique, `b`'s child is rebuilt under `a`.
#[test]
fn a_child_handed_beside_its_holder_is_written_into_by_nothing_the_other_reads() {
    for (apart, rebuilt) in [(false, 0), (true, 1)] {
        let module = child_beside_its_holder(apart);
        let found = in_place(&module).rows;
        assert_eq!(found.len(), rebuilt, "apart: {apart}: {found:?}\n{module}");
    }
}

/// `main` builds `p`, a `Pair` over a `Pair` `inner`, reads `p`'s second slot into `held`, hands `p` to `deep`, and reads `held`'s slot once `deep` returns, or, where `apart`, reads `held` out of another such `Pair`, `q`. `deep(t)` reads its parameter's second slot, reads that one's slot, and builds a `Pair` of it beside what it read dying.
fn child_read_before_its_holder_is_handed_on(apart: bool) -> Module {
    let mut module = Module::new();
    let pair = module.add_row(Row {
        debug_name: Some("Pair".into()),
        slots: vec![Slot::Tag, Slot::Opaque],
    });
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let deep = module.reserve_function();
    let deep_return = module.reserve_continuation();
    let [t, t_inner, t_slot, t_built] =
        ["t", "t_inner", "t_slot", "t_built"].map(|name| module.add_value(Some(name.into())));
    let deep_hands = module.add_node(Node::ApplyCont(Edge {
        target: deep_return,
        args: vec![Atom::Value(t_built)],
    }));
    let deep_build = module.add_node(Node::LetValue {
        result: t_built,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(t_slot)]),
        next: deep_hands,
    });
    let read_slot = module.add_node(Node::LetIntrinsic {
        result: t_slot,
        op: Intrinsic::RowGet(pair, 1),
        args: vec![Atom::Value(t_inner)],
        next: deep_build,
    });
    let deep_body = module.add_node(Node::LetIntrinsic {
        result: t_inner,
        op: Intrinsic::RowGet(pair, 1),
        args: vec![Atom::Value(t)],
        next: read_slot,
    });
    module.define_function(
        deep,
        Function {
            debug_name: Some("deep".into()),
            params: vec![t],
            return_cont: deep_return,
            body: deep_body,
            droppable: Droppable::Never,
        },
    );

    let [x, inner_p, inner_q, p, q, held, deeper, held_slot] = [
        "x",
        "inner_p",
        "inner_q",
        "p",
        "q",
        "held",
        "deeper",
        "held_slot",
    ]
    .map(|name| module.add_value(Some(name.into())));
    let exit = module.add_node(halt(vec![Atom::Value(held_slot)]));
    let read_held = module.add_node(Node::LetIntrinsic {
        result: held_slot,
        op: Intrinsic::RowGet(pair, 1),
        args: vec![Atom::Value(held)],
        next: exit,
    });
    let after = module.reserve_continuation();
    module.define_continuation(
        after,
        Continuation {
            debug_name: None,
            params: vec![deeper],
            body: read_held,
        },
    );
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(deep),
        args: vec![Atom::Value(p)],
        return_to: after,
    });
    let scope = module.add_node(Node::LetCont {
        continuations: vec![after],
        body: call,
    });
    let take_held = module.add_node(Node::LetIntrinsic {
        result: held,
        op: Intrinsic::RowGet(pair, 1),
        args: vec![Atom::Value(if apart { q } else { p })],
        next: scope,
    });
    let build_q = module.add_node(Node::LetValue {
        result: q,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(inner_q)]),
        next: take_held,
    });
    let build_p = module.add_node(Node::LetValue {
        result: p,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(inner_p)]),
        next: build_q,
    });
    let build_inner_q = module.add_node(Node::LetValue {
        result: inner_q,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(x)]),
        next: build_p,
    });
    let build_inner_p = module.add_node(Node::LetValue {
        result: inner_p,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(x)]),
        next: build_inner_q,
    });
    let compute = module.add_node(Node::LetIntrinsic {
        result: x,
        op: Intrinsic::NatAdd,
        args: vec![nat(3), nat(4)],
        next: build_inner_p,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![deep],
        body: compute,
    });
    module.define_function(
        main,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body,
            droppable: Droppable::Never,
        },
    );
    module.set_entry(main);
    module.verify().unwrap();
    module
}

/// A child read out of a value before the value is handed on, and read again after, names what the callee takes apart: the callee may not rebuild that child in place. Mutation-checked: with a value consumed while a field read out of it is live left unique, `deep` rebuilds the child `held` reads.
#[test]
fn a_child_read_before_its_holder_is_handed_on_is_written_into_by_nothing_the_callee_reads() {
    for (apart, rebuilt) in [(false, 0), (true, 1)] {
        let module = child_read_before_its_holder_is_handed_on(apart);
        let found = in_place(&module).rows;
        assert_eq!(found.len(), rebuilt, "apart: {apart}: {found:?}\n{module}");
    }
}

/// How what a continuation's borrowed parameter reads could be written into while the parameter is live ([`written_while_read`]).
#[derive(Clone, Copy, Debug)]
enum Write {
    /// The jump that binds the parameter hands a field read out of what it hands the parameter to an owned parameter beside it, which the continuation rebuilds.
    Field,
    /// The continuation hands the value it entered the parameter with to an owned parameter of a continuation it defines, which rebuilds it.
    Jumped,
    /// The continuation builds a `Pair` beside that value dying.
    Built,
}

/// `main` builds two `Pair`s nothing else holds, `p` and `q`, each over a `Pair` of its own, reads `p`'s tag, and `q`'s where `apart`, and enters a continuation `k` with `p` at `a`, which `k` reads only by handing it to `peek`, a function reading its tag, after a write `write` names: into what `a` names, or, where `apart`, into what `q` holds.
fn written_while_read(write: Write, apart: bool) -> Module {
    let mut module = Module::new();
    let pair = module.add_row(Row {
        debug_name: Some("Pair".into()),
        slots: vec![Slot::Tag, Slot::Opaque],
    });
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let peek = module.reserve_function();
    let peek_return = module.reserve_continuation();
    let [s, s_slot] = ["s", "s_slot"].map(|name| module.add_value(Some(name.into())));

    // `peek(s)`: read the tag and hand it back, a number that holds nothing a rebuild writes, so `peek` only reads what it is handed.
    let peek_hands = module.add_node(Node::ApplyCont(Edge {
        target: peek_return,
        args: vec![Atom::Value(s_slot)],
    }));
    let peek_body = module.add_node(Node::LetIntrinsic {
        result: s_slot,
        op: Intrinsic::RowGet(pair, 0),
        args: vec![Atom::Value(s)],
        next: peek_hands,
    });
    module.define_function(
        peek,
        Function {
            debug_name: Some("peek".into()),
            params: vec![s],
            return_cont: peek_return,
            body: peek_body,
            droppable: Droppable::Never,
        },
    );

    let [x, inner_p, inner_q, p, q, p_tag, a, peeked] =
        ["x", "inner_p", "inner_q", "p", "q", "p_tag", "a", "peeked"]
            .map(|name| module.add_value(Some(name.into())));
    let written = match apart {
        true => q,
        false => p,
    };

    // Within `k`, after the write: `peek(a)`, then a halt on what it read.
    let exit = module.add_node(halt(vec![Atom::Value(peeked)]));
    let after_peek = module.reserve_continuation();
    module.define_continuation(
        after_peek,
        Continuation {
            debug_name: None,
            params: vec![peeked],
            body: exit,
        },
    );
    let call_peek = module.add_node(Node::ApplyFun {
        callee: Callee::Known(peek),
        args: vec![Atom::Value(a)],
        return_to: after_peek,
    });
    let peek_scope = module.add_node(Node::LetCont {
        continuations: vec![after_peek],
        body: call_peek,
    });
    // `target` read at its slot, and a `Pair` built of what it read beside it dying, then `next`.
    let rebuild = |module: &mut Module, target: ValueId, next: NodeId| {
        let [slot, built] = ["slot", "built"].map(|name| module.add_value(Some(name.into())));
        let build = module.add_node(Node::LetValue {
            result: built,
            value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(slot)]),
            next,
        });
        module.add_node(Node::LetIntrinsic {
            result: slot,
            op: Intrinsic::RowGet(pair, 1),
            args: vec![Atom::Value(target)],
            next: build,
        })
    };
    let (params, args, k_body, held) = match write {
        Write::Field => {
            let [b, held] = ["b", "held"].map(|name| module.add_value(Some(name.into())));
            let body = rebuild(&mut module, b, peek_scope);
            (
                vec![a, b],
                vec![Atom::Value(p), Atom::Value(held)],
                body,
                Some(held),
            )
        }
        Write::Jumped => {
            let o = module.add_value(Some("o".into()));
            let j = module.reserve_continuation();
            let j_body = rebuild(&mut module, o, peek_scope);
            module.define_continuation(
                j,
                Continuation {
                    debug_name: Some("j".into()),
                    params: vec![o],
                    body: j_body,
                },
            );
            let jump = module.add_node(Node::ApplyCont(Edge {
                target: j,
                args: vec![Atom::Value(written)],
            }));
            let body = module.add_node(Node::LetCont {
                continuations: vec![j],
                body: jump,
            });
            (vec![a], vec![Atom::Value(p)], body, None)
        }
        Write::Built => {
            let built = module.add_value(Some("built".into()));
            let body = module.add_node(Node::LetValue {
                result: built,
                value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(x)]),
                next: peek_scope,
            });
            (vec![a], vec![Atom::Value(p)], body, None)
        }
    };
    let k = module.reserve_continuation();
    module.define_continuation(
        k,
        Continuation {
            debug_name: Some("k".into()),
            params,
            body: k_body,
        },
    );
    let enter = module.add_node(Node::ApplyCont(Edge { target: k, args }));
    let scope = module.add_node(Node::LetCont {
        continuations: vec![k],
        body: enter,
    });

    // `main`: `p` and `q`, `p` read as a `Pair` and `q` only where it is the one written, and for `Field` the field handed beside `p` read out of what is written.
    let after_reads = match held {
        Some(held) => module.add_node(Node::LetIntrinsic {
            result: held,
            op: Intrinsic::RowGet(pair, 1),
            args: vec![Atom::Value(written)],
            next: scope,
        }),
        None => scope,
    };
    let read_q = match (apart, write) {
        (true, Write::Jumped | Write::Built) => {
            let q_tag = module.add_value(Some("q_tag".into()));
            module.add_node(Node::LetIntrinsic {
                result: q_tag,
                op: Intrinsic::RowGet(pair, 0),
                args: vec![Atom::Value(q)],
                next: after_reads,
            })
        }
        _ => after_reads,
    };
    let read_p = module.add_node(Node::LetIntrinsic {
        result: p_tag,
        op: Intrinsic::RowGet(pair, 0),
        args: vec![Atom::Value(p)],
        next: read_q,
    });
    let build_q = module.add_node(Node::LetValue {
        result: q,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(inner_q)]),
        next: read_p,
    });
    let build_p = module.add_node(Node::LetValue {
        result: p,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(inner_p)]),
        next: build_q,
    });
    let build_inner_q = module.add_node(Node::LetValue {
        result: inner_q,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(x)]),
        next: build_p,
    });
    let build_inner_p = module.add_node(Node::LetValue {
        result: inner_p,
        value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(x)]),
        next: build_inner_q,
    });
    let compute_x = module.add_node(Node::LetIntrinsic {
        result: x,
        op: Intrinsic::NatAdd,
        args: vec![nat(3), nat(4)],
        next: build_inner_p,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![peek],
        body: compute_x,
    });
    module.define_function(
        main,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body,
            droppable: Droppable::Never,
        },
    );
    module.set_entry(main);
    module.verify().unwrap();
    module
}

/// A continuation's parameter that only reads what it was handed is borrowed, and nothing writes into what it names while it is live: not an owned parameter the same jump hands a field read out of it, which the continuation rebuilds; not an owned parameter of a continuation it hands that value to, which rebuilds it; and not a construction beside that value dying. Each is written into where it is `q`'s, which nothing the parameter reads is. Mutation-checked, each write against the rule only it answers to: with a value handed to a borrowed parameter beside a field of it handed to an owned one left borrowed, `Field` rebuilds `p`'s inner `Pair`; with a value consumed while the parameter is live left borrowed, `Jumped` rebuilds `p`; and with the rebuilds the facts admit not read against the borrows, `Built` does.
#[test]
fn what_a_borrowed_continuation_parameter_reads_is_written_into_by_nothing_while_it_is_live() {
    for write in [Write::Field, Write::Jumped, Write::Built] {
        for (apart, rebuilt) in [(false, 0), (true, 1)] {
            let module = written_while_read(write, apart);
            assert_eq!(
                in_place(&module).rows.len(),
                rebuilt,
                "{write:?}, apart: {apart}\n{module}"
            );
        }
    }
}

/// What `main` makes of the list it builds before an update of it at index 0, and reads after the update: nothing, or a second holder of the list's storage.
#[derive(Debug, Clone, Copy)]
enum Beside {
    Nothing,
    Window,
    Append,
    Concatenation,
    Settle,
}

/// `main` builds a list of a value nothing else holds, or of literals, makes `beside` of it, updates it at index 0 and reads the update's length, and the length of what it made.
fn updated_beside(beside: Beside, constant: bool) -> Module {
    module_with(|module| {
        let [v, l, m, h, u, a, b] =
            ["v", "l", "m", "h", "u", "a", "b"].map(|name| module.add_value(Some(name.into())));
        let exit = module.add_node(halt(vec![Atom::Value(a)]));
        let after = match beside {
            Beside::Nothing => exit,
            _ => module.add_node(Node::LetIntrinsic {
                result: b,
                op: Intrinsic::ListLen,
                args: vec![Atom::Value(h)],
                next: exit,
            }),
        };
        let measure = module.add_node(Node::LetIntrinsic {
            result: a,
            op: Intrinsic::ListLen,
            args: vec![Atom::Value(u)],
            next: after,
        });
        let update = module.add_node(Node::LetIntrinsic {
            result: u,
            op: Intrinsic::ListSet,
            args: vec![Atom::Value(l), nat(0), Atom::Value(v)],
            next: measure,
        });
        let (op, args) = match beside {
            Beside::Nothing => (None, Vec::new()),
            Beside::Window => (
                Some(Intrinsic::ListSlice),
                vec![Atom::Value(l), nat(0), nat(1)],
            ),
            Beside::Append => (
                Some(Intrinsic::ListAppend),
                vec![Atom::Value(l), Atom::Value(v)],
            ),
            Beside::Concatenation => (
                Some(Intrinsic::ListConcat(2)),
                vec![Atom::Value(m), Atom::Value(l)],
            ),
            Beside::Settle => (Some(Intrinsic::ListSettle), vec![Atom::Value(l)]),
        };
        let held = match op {
            Some(op) => module.add_node(Node::LetIntrinsic {
                result: h,
                op,
                args,
                next: update,
            }),
            None => update,
        };
        let other = module.add_node(Node::LetValue {
            result: m,
            value: ValueExpr::List(vec![Atom::Value(v)]),
            next: held,
        });
        let elements = match constant {
            true => vec![nat(1), nat(2)],
            false => vec![Atom::Value(v), Atom::Value(v)],
        };
        let built = module.add_node(Node::LetValue {
            result: l,
            value: ValueExpr::List(elements),
            next: other,
        });
        module.add_node(Node::LetIntrinsic {
            result: v,
            op: Intrinsic::NatAdd,
            args: vec![nat(3), nat(4)],
            next: built,
        })
    })
}

/// An update writes into a list nothing else holds, and copies one whose storage something else may still read: a window of it, an append to it, a concatenation over it and a settled leaf of it each read it after the update, and a constant is every use's.
#[test]
fn an_update_writes_into_a_list_only_where_nothing_else_holds_it() {
    assert_eq!(
        in_place(&updated_beside(Beside::Nothing, false))
            .lists
            .len(),
        1
    );
    assert_eq!(
        in_place(&updated_beside(Beside::Nothing, true)).lists.len(),
        0,
        "a constant list is every use's"
    );
    for beside in [
        Beside::Window,
        Beside::Append,
        Beside::Concatenation,
        Beside::Settle,
    ] {
        let module = updated_beside(beside, false);
        assert!(in_place(&module).lists.is_empty(), "{beside:?}\n{module}");
    }
}

/// `main` builds a list of two `Pair`s nothing else holds, reads the first, builds a `Pair` of its slot beside it dying, and updates the list at `at` with what it built.
fn element_written_back(at: u32) -> Module {
    module_with(|module| {
        let pair = module.add_row(Row {
            debug_name: Some("Pair".into()),
            slots: vec![Slot::Tag, Slot::Opaque],
        });
        let [v, p, q, l, e, f, n, u, a] = ["v", "p", "q", "l", "e", "f", "n", "u", "a"]
            .map(|name| module.add_value(Some(name.into())));
        let exit = module.add_node(halt(vec![Atom::Value(a)]));
        let measure = module.add_node(Node::LetIntrinsic {
            result: a,
            op: Intrinsic::ListLen,
            args: vec![Atom::Value(u)],
            next: exit,
        });
        let update = module.add_node(Node::LetIntrinsic {
            result: u,
            op: Intrinsic::ListSet,
            args: vec![Atom::Value(l), nat(at), Atom::Value(n)],
            next: measure,
        });
        let build = module.add_node(Node::LetValue {
            result: n,
            value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(f)]),
            next: update,
        });
        let read_slot = module.add_node(Node::LetIntrinsic {
            result: f,
            op: Intrinsic::RowGet(pair, 1),
            args: vec![Atom::Value(e)],
            next: build,
        });
        let read_element = module.add_node(Node::LetIntrinsic {
            result: e,
            op: Intrinsic::ListGet,
            args: vec![Atom::Value(l), nat(0)],
            next: read_slot,
        });
        let list = module.add_node(Node::LetValue {
            result: l,
            value: ValueExpr::List(vec![Atom::Value(p), Atom::Value(q)]),
            next: read_element,
        });
        let second = module.add_node(Node::LetValue {
            result: q,
            value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(v)]),
            next: list,
        });
        let first = module.add_node(Node::LetValue {
            result: p,
            value: ValueExpr::Row(pair, vec![nat(1), Atom::Value(v)]),
            next: second,
        });
        module.add_node(Node::LetIntrinsic {
            result: v,
            op: Intrinsic::NatAdd,
            args: vec![nat(3), nat(4)],
            next: first,
        })
    })
}

/// An element read at the index an update then writes over is held by nothing else once its list is unique, so what is built of it is rebuilt into it; read at another index than the one written, the list still holds it after the update, and it is copied.
#[test]
fn an_element_an_update_writes_over_is_held_by_nothing_else() {
    let over = in_place(&element_written_back(0));
    assert_eq!((over.rows.len(), over.lists.len()), (1, 1));

    let beside = in_place(&element_written_back(1));
    assert_eq!((beside.rows.len(), beside.lists.len()), (0, 1));
}
