//! Tuples, list and packed literals, cells and variants — built and read at their own carriers.

use {
    super::test_support::*,
    curios_num::{Binary, Grain, Rounding},
};

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
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let bin = module.add_value(Some("bin".into()));
    let exit = module.add_node(halt(vec![curios_cont::Atom::Value(bin)]));
    let build = module.add_node(curios_cont::Node::LetValue {
        result: bin,
        value: curios_cont::ValueExpr::Literal(curios_cont::Literal::Bin(
            Grain::X,
            Binary::from_bytes(vec![1, 2, 3]),
        )),
        next: exit,
    });
    module.define_function(
        main,
        curios_cont::Function {
            debug_name: Some("main".into()),
            params: Vec::new(),
            return_cont,
            body: build,
            droppable: false,
        },
    );
    module.set_entry(main);

    // Inside the envelope nothing is allocated and no data segment exists for the program's own constant: the value is one i31 constant. A refusal the code reaches brings a data segment of its own, named `refusal/…`, which is not what this counts.
    let wat = wat(&module);
    assert_contains(&wat, "ref.i31");
    assert_eq!(count(&wat, "array.new_data $bytes $const/"), 0);
    assert_eq!(count(&wat, "(data $const/"), 0);
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
fn cell_fill_and_poll_use_the_cell_struct() {
    let wat = wat(&cell_roundtrip());
    assert_contains(&wat, "struct.new_default $cell");
    assert_contains(&wat, "struct.set $cell");
    assert_contains(&wat, "struct.get $cell");
}

#[test]
fn taking_a_channel_item_clears_the_slot_without_a_host_call() {
    let wat = wat(&channel_roundtrip());
    assert_contains(&wat, "array.new_default $elems");
    assert_contains(&wat, "array.get $elems");
    assert_contains(&wat, "ref.null any");
    // One store enqueues; the other clears the consumed slot. Creation guards the requested capacity before allocating; the state transitions need no host operation.
    assert_eq!(count(&wat, "array.set $elems"), 2, "{wat}");
    assert_contains(&wat, "call $refuse/capacity");
    assert_eq!(count(&wat, "(call "), 0, "{wat}");
}

/// A `Tuple` read where a float is, is refused at compile time.
///
/// The positive control for `Context::refuse_raw_aggregate`, and the reason that guard is an assertion in the emitter rather than a remark in the door. The representation analysis keeps an aggregate out of every register it decides: the aggregate's own definition offers nothing, and a parameter it reaches is offered a word or a float only where every argument reaching it is one. What it does not decide is a read: an operand position loads its atom at the operation's carrier whatever the atom is held at, so an aggregate standing where a scalar's name should be is cast there, and traps in a function far from whatever put it there.
///
/// This is that shape stated directly — a tuple handed to `FltAdd`. The tuple is closed, so `hoist` lifts it to a module const: this covers the const half of the population. [`a_region_aggregate_read_at_a_raw_carrier_is_refused`] covers the other.
#[test]
#[should_panic = "loaded at the raw carrier"]
fn an_aggregate_read_at_a_raw_carrier_is_refused() {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();

    let sum = module.add_value(Some("sum".into()));
    let aggregate = module.add_value(Some("aggregate".into()));

    let exit = module.add_node(halt(vec![curios_cont::Atom::Value(sum)]));
    let add = module.add_node(curios_cont::Node::LetIntrinsic {
        result: sum,
        op: curios_cont::Intrinsic::FltAdd(Rounding::TiesToEven),
        args: vec![curios_cont::Atom::Value(aggregate), flt(1.0)],
        next: exit,
    });
    let body = module.add_node(curios_cont::Node::LetValue {
        result: aggregate,
        value: curios_cont::ValueExpr::Tuple(vec![nat(1), nat(2)]),
        next: add,
    });

    module.define_function(
        main,
        curios_cont::Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
            droppable: false,
        },
    );
    module.set_entry(main);

    wat(&module);
}

/// The region half of [`an_aggregate_read_at_a_raw_carrier_is_refused`].
///
/// Same shape, but one element of the tuple is a cell handle — a call-shaped result, so the tuple is not closed and stays a binding in the region instead of being hoisted to a global. This is the half an aliased immediate arm reaches: a cons cell built inside a loop.
#[test]
#[should_panic = "loaded at the raw carrier"]
fn a_region_aggregate_read_at_a_raw_carrier_is_refused() {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();

    let cell = module.add_value(Some("cell".into()));
    let sum = module.add_value(Some("sum".into()));
    let aggregate = module.add_value(Some("aggregate".into()));

    let exit = module.add_node(halt(vec![curios_cont::Atom::Value(sum)]));
    let add = module.add_node(curios_cont::Node::LetIntrinsic {
        result: sum,
        op: curios_cont::Intrinsic::FltAdd(Rounding::TiesToEven),
        args: vec![curios_cont::Atom::Value(aggregate), flt(1.0)],
        next: exit,
    });
    let built = module.add_node(curios_cont::Node::LetValue {
        result: aggregate,
        value: curios_cont::ValueExpr::Tuple(vec![curios_cont::Atom::Value(cell), nat(2)]),
        next: add,
    });
    let made = module.add_continuation(curios_cont::Continuation {
        debug_name: Some("made".into()),
        params: vec![cell],
        body: built,
    });
    let new = module.add_node(curios_cont::Node::Cell {
        op: curios_cont::CellOp::Reserve,
        args: vec![],
        return_to: made,
    });
    let body = module.add_node(curios_cont::Node::LetCont {
        continuations: vec![made],
        body: new,
    });

    module.define_function(
        main,
        curios_cont::Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
            droppable: false,
        },
    );
    module.set_entry(main);

    wat(&module);
}

/// A `Tuple` reaching a continuation parameter something reads as a float leaves the parameter a reference, and the module is emitted.
///
/// The shape a variant's shared payload slot and a type a match computes both produce: a join entered with a float on one edge and with something else on another, read as a float under the one. The analysis offers the parameter no register, since not every argument reaching it is a float, so the edge hands the tuple over as the reference it is and only the read unboxes.
#[test]
fn an_aggregate_reaching_a_parameter_read_as_a_float_leaves_it_a_reference() {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();

    let param = module.add_value(Some("param".into()));
    let sum = module.add_value(Some("sum".into()));
    let aggregate = module.add_value(Some("aggregate".into()));

    let exit = module.add_node(halt(vec![curios_cont::Atom::Value(sum)]));
    let add = module.add_node(curios_cont::Node::LetIntrinsic {
        result: sum,
        op: curios_cont::Intrinsic::FltAdd(Rounding::TiesToEven),
        args: vec![curios_cont::Atom::Value(param), flt(1.0)],
        next: exit,
    });
    let joined = module.add_continuation(curios_cont::Continuation {
        debug_name: Some("joined".into()),
        params: vec![param],
        body: add,
    });

    let enter = module.add_node(curios_cont::Node::ApplyCont(curios_cont::Edge {
        target: joined,
        args: vec![curios_cont::Atom::Value(aggregate)],
    }));
    let scope = module.add_node(curios_cont::Node::LetCont {
        continuations: vec![joined],
        body: enter,
    });
    let body = module.add_node(curios_cont::Node::LetValue {
        result: aggregate,
        value: curios_cont::ValueExpr::Tuple(vec![nat(1), nat(2)]),
        next: scope,
    });

    module.define_function(
        main,
        curios_cont::Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
            droppable: false,
        },
    );
    module.set_entry(main);

    let wat = wat(&module);
    let locals = wat
        .lines()
        .find(|line| line.trim_start().starts_with("(local "))
        .expect("the entry declares its locals");
    assert_eq!(
        count(locals, " f64)"),
        1,
        "the sum alone is held in a float register, and the parameter in none: {locals}"
    );
    assert_contains(&wat, "ref.cast (ref $flt)");
}

/// A variant is constructed at its row's own final type and read back with one exact cast — no roster cascade, because a row value's type is a fact of the row rather than of the constructor that built it.
#[test]
fn a_variant_is_built_and_read_at_its_family_type() {
    let mut module = curios_cont::Module::new();
    let row = module.add_row(curios_cont::Row {
        debug_name: Some("Shape".into()),
        slots: vec![
            curios_cont::Slot::Tag,
            curios_cont::Slot::Opaque,
            curios_cont::Slot::Opaque,
        ],
    });
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let built = module.add_value(Some("built".into()));
    let field = module.add_value(Some("field".into()));
    let exit = module.add_node(halt(vec![curios_cont::Atom::Value(field)]));
    let read = module.add_node(curios_cont::Node::LetIntrinsic {
        result: field,
        op: curios_cont::Intrinsic::RowGet(row, 1),
        args: vec![curios_cont::Atom::Value(built)],
        next: exit,
    });
    let build = module.add_node(curios_cont::Node::LetValue {
        result: built,
        value: curios_cont::ValueExpr::Row(row, vec![nat(0), nat(7), curios_cont::Atom::Filler]),
        next: read,
    });
    module.define_function(
        main,
        curios_cont::Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body: build,
            droppable: false,
        },
    );
    module.set_entry(main);

    let wat = wat(&module);
    assert_contains(&wat, "(type $row/0$Shape");
    assert_contains(&wat, "struct.new $row/0$Shape");
    assert_contains(&wat, "ref.cast (ref $row/0$Shape)");
    assert_contains(&wat, "struct.get $row/0$Shape $1");
    // No cascade of tests against candidate types; the exit's narrowing tests for an i31, which is not one.
    assert_absent(&wat, "ref.test (ref $");
    // The row type is final and unrelated: the printer renders no `sub` wrapper for one.
    for line in wat.lines().filter(|line| line.contains("(type $row/")) {
        assert!(!line.contains("sub"), "row types must be final: {line}");
    }
}

/// A construction that does not carry its row's width is a compiler bug, and the verifier is where it stops — the check the distinct variant vocabulary exists to make possible.
#[test]
#[should_panic = "the row is 3 wide"]
fn a_short_variant_construction_is_refused() {
    let mut module = curios_cont::Module::new();
    let row = module.add_row(curios_cont::Row {
        debug_name: Some("Shape".into()),
        slots: vec![
            curios_cont::Slot::Tag,
            curios_cont::Slot::Opaque,
            curios_cont::Slot::Opaque,
        ],
    });
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let built = module.add_value(Some("built".into()));
    let exit = module.add_node(halt_zero());
    let build = module.add_node(curios_cont::Node::LetValue {
        result: built,
        value: curios_cont::ValueExpr::Row(row, vec![nat(0)]),
        next: exit,
    });
    module.define_function(
        main,
        curios_cont::Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body: build,
            droppable: false,
        },
    );
    module.set_entry(main);

    let _ = wat(&module);
}
