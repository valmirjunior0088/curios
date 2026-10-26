use {
    super::{Rows, rows_reaching},
    crate::{
        Atom, Callee, Continuation, Edge, Function, FunctionId, Intrinsic, Literal, Module, Node,
        NodeId, Row, RowId, Slot, ValueExpr, ValueId, test_support::halt_zero,
    },
    curios_num::Natural,
    std::collections::BTreeSet,
};

/// A program whose entry calls a function returning `returned`, which `produce` binds ahead of the return node it is handed, and resumes with what it returns in the value handed back. Where `tail`, a function between them calls the producer in tail position, so its return is the one the entry's call resumes with.
fn resumed_with(
    tail: bool,
    produce: impl FnOnce(&mut Module, ValueId, NodeId) -> NodeId,
) -> (Module, ValueId) {
    let mut module = Module::default();

    let returned = module.add_value(Some("returned".into()));
    let producer = module.reserve_function();
    let producer_ret = module.reserve_continuation();
    let returning = module.add_node(Node::ApplyCont(Edge {
        target: producer_ret,
        args: vec![Atom::Value(returned)],
    }));
    let body = produce(&mut module, returned, returning);
    module.define_function(
        producer,
        Function {
            debug_name: Some("producer".into()),
            params: Vec::new(),
            return_cont: producer_ret,
            body,
            droppable: false,
        },
    );

    let called = match tail {
        false => producer,
        true => {
            let middle = module.reserve_function();
            let middle_ret = module.reserve_continuation();
            let call = module.add_node(Node::ApplyFun {
                callee: Callee::Known(producer),
                args: Vec::new(),
                return_to: middle_ret,
            });
            module.define_function(
                middle,
                Function {
                    debug_name: Some("middle".into()),
                    params: Vec::new(),
                    return_cont: middle_ret,
                    body: call,
                    droppable: false,
                },
            );
            middle
        }
    };

    let received = module.add_value(Some("received".into()));
    let entry: FunctionId = module.reserve_function();
    let entry_ret = module.reserve_continuation();
    let resume = module.reserve_continuation();
    let exit = module.add_node(halt_zero());
    module.define_continuation(
        resume,
        Continuation {
            debug_name: Some("resume".into()),
            params: vec![received],
            body: exit,
        },
    );
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(called),
        args: Vec::new(),
        return_to: resume,
    });
    let body = module.add_node(Node::LetCont {
        continuations: vec![resume],
        body: call,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("entry".into()),
            params: Vec::new(),
            return_cont: entry_ret,
            body,
            droppable: false,
        },
    );
    module.set_entry(entry);

    (module, received)
}

fn one_slot_row(module: &mut Module) -> RowId {
    module.add_row(Row {
        debug_name: Some("row".into()),
        slots: vec![Slot::Opaque],
    })
}

#[test]
fn a_call_resumes_with_the_rows_its_callee_returns() {
    for tail in [false, true] {
        let mut row = None;
        let (module, received) = resumed_with(tail, |module, returned, next| {
            let built = one_slot_row(module);
            row = Some(built);
            module.add_node(Node::LetValue {
                result: returned,
                value: ValueExpr::Row(built, vec![Atom::Filler]),
                next,
            })
        });
        let row = row.expect("the producer built its row");

        assert_eq!(
            rows_reaching(&module)[&received],
            Rows::Of(BTreeSet::from([row])),
            "tail: {tail}"
        );
    }
}

#[test]
fn a_number_a_callee_computes_is_no_row() {
    let (mut module, received) = resumed_with(true, |module, returned, next| {
        module.add_node(Node::LetIntrinsic {
            result: returned,
            op: Intrinsic::NatAdd,
            args: vec![
                Atom::Literal(Literal::Nat(Natural::from(1u32))),
                Atom::Literal(Literal::Nat(Natural::from(2u32))),
            ],
            next,
        })
    });
    let row = one_slot_row(&mut module);

    assert!(!rows_reaching(&module)[&received].may_be(row));
}

#[test]
fn a_reference_read_out_of_a_value_may_be_any_row() {
    let (module, received) = resumed_with(false, |module, returned, next| {
        let tuple = module.add_value(Some("tuple".into()));
        let read = module.add_node(Node::LetIntrinsic {
            result: returned,
            op: Intrinsic::TupleGet(0),
            args: vec![Atom::Value(tuple)],
            next,
        });
        module.add_node(Node::LetValue {
            result: tuple,
            value: ValueExpr::Tuple(vec![Atom::Filler]),
            next: read,
        })
    });

    assert_eq!(rows_reaching(&module)[&received], Rows::Any);
}
