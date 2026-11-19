//! The rows a value may be a struct of, which the row tests, the rebuild in place and the verifier read.

use {
    super::*,
    crate::{test_support::*, *},
    curios_num::Natural,
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
            lineage: None,
            debug_name: Some("producer".into()),
            params: Vec::new(),
            return_cont: producer_ret,
            body,
            droppable: Droppable::Never,
            terminates: false,
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
                    lineage: None,
                    debug_name: Some("middle".into()),
                    params: Vec::new(),
                    return_cont: middle_ret,
                    body: call,
                    droppable: Droppable::Never,
                    terminates: false,
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
            lineage: None,
            debug_name: Some("resume".into()),
            params: vec![received],
            body: exit,
            terminates: false,
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
            lineage: None,
            debug_name: Some("entry".into()),
            params: Vec::new(),
            return_cont: entry_ret,
            body,
            droppable: Droppable::Never,
            terminates: false,
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
            Rows::Of(Ids::one(row)),
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
fn a_reference_read_out_of_a_value_is_the_rows_its_construction_stores() {
    for stores_a_row in [false, true] {
        let mut row = None;
        let (module, received) = resumed_with(false, |module, returned, next| {
            let tuple = module.add_value(Some("tuple".into()));
            let read = module.add_node(Node::LetIntrinsic {
                result: returned,
                op: Intrinsic::TupleGet(0),
                args: vec![Atom::Value(tuple)],
                next,
            });
            let stored = module.add_value(Some("stored".into()));
            let tupled = module.add_node(Node::LetValue {
                result: tuple,
                value: ValueExpr::Tuple(vec![match stores_a_row {
                    true => Atom::Value(stored),
                    false => Atom::Filler,
                }]),
                next: read,
            });
            let built = one_slot_row(module);
            row = Some(built);
            module.add_node(Node::LetValue {
                result: stored,
                value: ValueExpr::Row(built, vec![Atom::Filler]),
                next: tupled,
            })
        });
        let rows = match stores_a_row {
            true => Ids::one(row.expect("the producer built its row")),
            false => Ids::new(),
        };

        assert_eq!(
            rows_reaching(&module)[&received],
            Rows::Of(rows),
            "stores a row: {stores_a_row}"
        );
    }
}

#[test]
fn a_heap_call_answers_any_row_its_continuation_may() {
    let mut module = Module::default();
    let entry = module.reserve_function();
    let entry_ret = module.reserve_continuation();
    let closure = module.add_value(Some("closure".into()));
    let received = module.add_value(Some("received".into()));
    let exit = module.add_node(halt_zero());
    let resume = module.add_continuation(Continuation {
        lineage: None,
        debug_name: Some("resume".into()),
        params: vec![received],
        body: exit,
        terminates: false,
    });
    let call = module.add_node(Node::Intrinsic {
        op: IntrinsicCall::HeapApply,
        args: vec![Atom::Value(closure), Atom::Value(closure)],
        return_to: resume,
    });
    let body = module.add_node(Node::LetCont {
        continuations: vec![resume],
        body: call,
    });
    module.define_function(
        entry,
        Function {
            lineage: None,
            debug_name: Some("entry".into()),
            params: vec![closure],
            return_cont: entry_ret,
            body,
            droppable: Droppable::Never,
            terminates: false,
        },
    );
    module.set_entry(entry);

    assert_eq!(rows_reaching(&module)[&received], Rows::Any);
}
