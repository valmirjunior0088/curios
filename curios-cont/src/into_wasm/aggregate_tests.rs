//! Tuples, list and packed literals, cells and variants — built and read at their own carriers.

//! Backend lowering coverage: build a [`CpsModule`](crate::CpsModule) directly, lower it with [`into_wasm`](crate::into_wasm), and assert the *shape* of the emitted wasm (its WAT text). These are the shape half of a split: the fixtures that once built the old region API and *executed* the module became shape inspection here, and end-to-end semantics in `curios/src/tests/codegen` and the native `.crs` corpus. `into_wasm` performs no optimization, so a `LetIntrinsic` over literal operands lowers one-for-one without constant folding, and the emitted instruction is exactly what codegen chose.

use {
    crate::{
        CpsAtom, CpsCellOp, CpsContinuation, CpsEdge, CpsFunction, CpsIntrinsic, CpsLiteral,
        CpsModule, CpsNode, CpsSlot, CpsValueExpr,
    },
    curios_utilities::{Grain, PackedBin},
};

use super::test_support::*;

#[test]
fn tuple_construction_and_projection() {
    let wat = wat(&tuple_project());
    assert_contains(&wat, "struct.new $tuple");
    assert_contains(&wat, "struct.get $tuple");
}

#[test]
fn list_literal_builds_a_rope_leaf() {
    let wat = wat(&list_len());
    assert_contains(&wat, "struct.new $rope/list/leaf");
    assert_contains(&wat, "array.new_fixed $elems");
}

#[test]
fn small_packed_literal_rides_the_immediate() {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let bin = module.add_value(Some("bin".into()));
    let exit = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(bin)),
    });
    let build = module.add_node(CpsNode::LetValue {
        result: bin,
        value: CpsValueExpr::Literal(CpsLiteral::Bin(
            Grain::X,
            PackedBin::from_bytes(vec![1, 2, 3]),
        )),
        next: exit,
    });
    module.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: Vec::new(),
            return_cont,
            body: build,
        },
    );
    module.set_entry(main);

    // Inside the envelope nothing is allocated and no data segment exists: the value is one i31 constant.
    let wat = wat(&module);
    assert_contains(&wat, "ref.i31");
    assert_eq!(count(&wat, "array.new_data"), 0);
    assert_eq!(count(&wat, "(data "), 0);
}

#[test]
fn packed_bin_literal_builds_a_rope_leaf() {
    let wat = wat(&bin_len());
    assert_contains(&wat, "struct.new $rope/bin/leaf");
    // The static bytes come from a passive data segment.
    assert_contains(&wat, "array.new_data $bytes");
    assert_contains(&wat, "(data ");
}

#[test]
fn cell_new_and_get_use_the_cell_struct() {
    let wat = wat(&cell_roundtrip());
    assert_contains(&wat, "struct.new $cell");
    assert_contains(&wat, "struct.get $cell");
}

/// A `Tuple` reaching a raw-carried continuation parameter is refused at compile time.
///
/// The positive control for `Context::refuse_raw_aggregate`, and the reason that guard is an assertion in the emitter rather than a remark in the door. A continuation parameter offers `Offer::Open`, so a raw demand from *any* of its uses settles it raw whatever flows in — the aggregate's own `Offer::Never` never enters the question, because the coercion happens on the edge rather than at the aggregate's definition. The edge then loads its argument at that carrier, and an aggregate argument becomes `ref.cast (ref i31)` over a `struct.new`: a module that verifies, emits, and traps.
///
/// This is that shape stated directly — a continuation whose parameter feeds `NatAdd`, entered with a tuple. `curios-ersd`'s door no longer produces it (an immediate arm's binder was aliased to its scrutinee and now gets a definition of its own, `CpsIntrinsic::ImmediateGet`), but the IR still permits it, so the guard is what keeps the class from returning silently. Observed against the pre-fix door on 2026-08-20: `` `m869` is a `Tuple`/`List` construction loaded at the raw carrier Nat`` — the same value the emitted wasm had been casting.
///
/// The tuple here is closed, so `hoist` lifts it to a module const: this covers the const half of the population. [`a_region_aggregate_reaching_a_raw_parameter_is_refused`] covers the other.
#[test]
#[should_panic = "loaded at the raw carrier"]
fn an_aggregate_reaching_a_raw_parameter_is_refused() {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();

    let param = module.add_value(Some("param".into()));
    let sum = module.add_value(Some("sum".into()));
    let aggregate = module.add_value(Some("aggregate".into()));

    // The parameter's one use demands a raw `Nat`, which is what raises it out of a reference.
    let exit = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(sum)),
    });
    let add = module.add_node(CpsNode::LetIntrinsic {
        result: sum,
        op: CpsIntrinsic::NatAdd,
        args: vec![CpsAtom::Value(param), nat(1)],
        next: exit,
    });
    let raised = module.add_continuation(CpsContinuation {
        debug_name: Some("raised".into()),
        params: vec![param],
        body: add,
    });

    let enter = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: raised,
        args: vec![CpsAtom::Value(aggregate)],
    }));
    let scope = module.add_node(CpsNode::LetCont {
        continuations: vec![raised],
        body: enter,
    });
    let body = module.add_node(CpsNode::LetValue {
        result: aggregate,
        value: CpsValueExpr::Tuple(vec![nat(1), nat(2)]),
        next: scope,
    });

    module.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
        },
    );
    module.set_entry(main);

    wat(&module);
}

/// The region half of [`an_aggregate_reaching_a_raw_parameter_is_refused`].
///
/// Same shape, but one element of the tuple is a cell handle — a call-shaped result, so the tuple is not closed and stays a binding in the region instead of being hoisted to a global. This is the half the immediate-arm bug actually took: the aggregate was a cons cell built inside a loop.
#[test]
#[should_panic = "loaded at the raw carrier"]
fn a_region_aggregate_reaching_a_raw_parameter_is_refused() {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();

    let cell = module.add_value(Some("cell".into()));
    let param = module.add_value(Some("param".into()));
    let sum = module.add_value(Some("sum".into()));
    let aggregate = module.add_value(Some("aggregate".into()));

    let exit = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(sum)),
    });
    let add = module.add_node(CpsNode::LetIntrinsic {
        result: sum,
        op: CpsIntrinsic::NatAdd,
        args: vec![CpsAtom::Value(param), nat(1)],
        next: exit,
    });
    let raised = module.add_continuation(CpsContinuation {
        debug_name: Some("raised".into()),
        params: vec![param],
        body: add,
    });

    let enter = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: raised,
        args: vec![CpsAtom::Value(aggregate)],
    }));
    let scope = module.add_node(CpsNode::LetCont {
        continuations: vec![raised],
        body: enter,
    });
    let built = module.add_node(CpsNode::LetValue {
        result: aggregate,
        value: CpsValueExpr::Tuple(vec![CpsAtom::Value(cell), nat(2)]),
        next: scope,
    });
    let made = module.add_continuation(CpsContinuation {
        debug_name: Some("made".into()),
        params: vec![cell],
        body: built,
    });
    let new = module.add_node(CpsNode::Cell {
        op: CpsCellOp::New,
        args: vec![nat(0)],
        return_to: made,
    });
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![made],
        body: new,
    });

    module.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
        },
    );
    module.set_entry(main);

    wat(&module);
}

/// A variant is constructed at its row's own final type and read back with one exact cast — no roster cascade, because a row value's type is a fact of the row rather than of the constructor that built it.
#[test]
fn a_variant_is_built_and_read_at_its_family_type() {
    let mut module = CpsModule::new();
    let row = module.add_row(crate::CpsRow {
        debug_name: Some("Shape".into()),
        slots: vec![CpsSlot::Tag, CpsSlot::Opaque, CpsSlot::Opaque],
    });
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let built = module.add_value(Some("built".into()));
    let field = module.add_value(Some("field".into()));
    let exit = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(field)),
    });
    let read = module.add_node(CpsNode::LetIntrinsic {
        result: field,
        op: CpsIntrinsic::RowGet(row, 1),
        args: vec![CpsAtom::Value(built)],
        next: exit,
    });
    let build = module.add_node(CpsNode::LetValue {
        result: built,
        value: CpsValueExpr::Row(row, vec![nat(0), nat(7), CpsAtom::Filler]),
        next: read,
    });
    module.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body: build,
        },
    );
    module.set_entry(main);

    let wat = wat(&module);
    assert_contains(&wat, "(type $row/0$Shape");
    assert_contains(&wat, "struct.new $row/0$Shape");
    assert_contains(&wat, "ref.cast (ref $row/0$Shape)");
    assert_contains(&wat, "struct.get $row/0$Shape $1");
    assert_absent(&wat, "ref.test");
    // The row type is final and unrelated: the printer renders no `sub` wrapper for one.
    for line in wat.lines().filter(|line| line.contains("(type $row/")) {
        assert!(!line.contains("sub"), "row types must be final: {line}");
    }
}

/// A construction that does not carry its row's width is a compiler bug, and the verifier is where it stops — the check the distinct variant vocabulary exists to make possible.
#[test]
#[should_panic = "the row is 3 wide"]
fn a_short_variant_construction_is_refused() {
    let mut module = CpsModule::new();
    let row = module.add_row(crate::CpsRow {
        debug_name: Some("Shape".into()),
        slots: vec![CpsSlot::Tag, CpsSlot::Opaque, CpsSlot::Opaque],
    });
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let built = module.add_value(Some("built".into()));
    let exit = module.add_node(CpsNode::Exit { value: None });
    let build = module.add_node(CpsNode::LetValue {
        result: built,
        value: CpsValueExpr::Row(row, vec![nat(0)]),
        next: exit,
    });
    module.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body: build,
        },
    );
    module.set_entry(main);

    let _ = wat(&module);
}

// A head rebuild loads a reference slot tolerantly and an ordinary construction does not. The rebuild's field may hold the boxed `i31` a padded edge handed it, which the slot's exact cast refuses; tested first, it lands as the slot's null instead. The test is on the emitted shape because the trap it prevents needs a whole split pipeline to arrange.
#[test]
fn a_head_rebuild_loads_a_reference_slot_tolerantly() {
    let mut module = CpsModule::new();
    let row = module.add_row(crate::CpsRow {
        debug_name: Some("Shape".into()),
        slots: vec![CpsSlot::Tag, CpsSlot::Nat, CpsSlot::List],
    });
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let payload = module.add_value(Some("payload".into()));
    let built = module.add_value(Some("built".into()));
    let field = module.add_value(Some("field".into()));
    let exit = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(field)),
    });
    let read = module.add_node(CpsNode::LetIntrinsic {
        result: field,
        op: CpsIntrinsic::RowGet(row, 1),
        args: vec![CpsAtom::Value(built)],
        next: exit,
    });
    let build = module.add_node(CpsNode::LetValue {
        result: built,
        value: CpsValueExpr::Row(row, vec![nat(0), nat(7), CpsAtom::Value(payload)]),
        next: read,
    });
    let hold = module.add_node(CpsNode::LetValue {
        result: payload,
        value: CpsValueExpr::List(vec![]),
        next: build,
    });
    module.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body: hold,
        },
    );
    module.set_entry(main);

    let exact = wat(&module);
    assert_absent(&exact, "ref.test");

    module.mark_rebuilt(built);
    let tolerant = wat(&module);
    assert_contains(&tolerant, "ref.test");
    assert_contains(&tolerant, "ref.null none");
}
