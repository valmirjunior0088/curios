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
