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
