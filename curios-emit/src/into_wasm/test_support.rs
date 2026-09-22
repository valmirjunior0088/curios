//! Emitting a fixture and reading the text back: the harness the Wasm emission suites assert through, and the programs they emit.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

//! Backend lowering coverage: build a [`curios_cont::Module`](curios_cont::Module) directly, lower it with [`into_wasm`](crate::into_wasm), and assert the *shape* of the emitted wasm (its WAT text). These are the shape half of a split: the fixtures that once built the old region API and *executed* the module became shape inspection here, and end-to-end semantics in `curios/src/tests/codegen` and the native `.crs` corpus. `into_wasm` performs no optimization, so a `LetIntrinsic` over literal operands lowers one-for-one without constant folding, and the emitted instruction is exactly what codegen chose.

use curios_num::{Integer, Natural};

use {
    crate::into_wasm,
    curios_abi::host_ops,
    curios_num::Floating,
    curios_utilities::{Grain, PackedBin},
    std::collections::BTreeMap,
};

/// The emitted module rendered as WAT text — the public inspection surface (`Module`'s items are private; `Display` is how consumers read it back).
pub(super) fn wat(module: &curios_cont::Module) -> String {
    into_wasm(module).to_string()
}

#[track_caller]
pub(super) fn assert_contains(wat: &str, needle: &str) {
    assert!(wat.contains(needle), "expected wat to contain `{needle}`");
}

#[track_caller]
pub(super) fn assert_absent(wat: &str, needle: &str) {
    assert!(!wat.contains(needle), "expected wat to lack `{needle}`");
}

pub(super) fn count(wat: &str, needle: &str) -> usize {
    wat.matches(needle).count()
}

/// The refusal calls a module makes besides its exit's: a fixture exits with the value it computed, and an exit code crosses the wire as any `Nat` argument does, so its narrowing can refuse whatever the operation under test did.
pub(super) fn refusals_besides_the_exit(wat: &str) -> usize {
    count(wat, "call $refuse/") - count(wat, "call $refuse/nat_wire")
}

pub(super) fn nat(value: u32) -> curios_cont::Atom {
    curios_cont::Atom::Literal(curios_cont::Literal::Nat(Natural::from(value)))
}

pub(super) fn int(value: i32) -> curios_cont::Atom {
    curios_cont::Atom::Literal(curios_cont::Literal::Int(Integer::from(value)))
}

pub(super) fn flt(value: f64) -> curios_cont::Atom {
    curios_cont::Atom::Literal(curios_cont::Literal::Flt(Floating::from_f64(value)))
}

/// A nullary `main` that binds one intrinsic over `args` and exits with the result — the CPS analogue of the deleted fixtures' "compute one thing, exit with it". `into_wasm` does not fold, so the op lowers verbatim.
pub(super) fn intrinsic_main(
    op: curios_cont::Intrinsic,
    args: Vec<curios_cont::Atom>,
) -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let result = module.add_value(Some("result".into()));
    let exit = module.add_node(curios_cont::Node::Exit {
        value: Some(curios_cont::Atom::Value(result)),
    });
    let body = module.add_node(curios_cont::Node::LetIntrinsic {
        result,
        op,
        args,
        next: exit,
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
    module
}

// --- Aggregates / packed / cells -----------------------------------------

/// Construct a tuple then project field 0 from it.
pub(super) fn tuple_project() -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let tuple = module.add_value(Some("tuple".into()));
    let field = module.add_value(Some("field".into()));
    let exit = module.add_node(curios_cont::Node::Exit {
        value: Some(curios_cont::Atom::Value(field)),
    });
    let project = module.add_node(curios_cont::Node::LetIntrinsic {
        result: field,
        op: curios_cont::Intrinsic::TupleGet(0),
        args: vec![curios_cont::Atom::Value(tuple)],
        next: exit,
    });
    let build = module.add_node(curios_cont::Node::LetValue {
        result: tuple,
        value: curios_cont::ValueExpr::Tuple(vec![nat(1), nat(2)]),
        next: project,
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
    module
}

/// A list literal then its length.
pub(super) fn list_len() -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let list = module.add_value(Some("list".into()));
    let len = module.add_value(Some("len".into()));
    let exit = module.add_node(curios_cont::Node::Exit {
        value: Some(curios_cont::Atom::Value(len)),
    });
    let measure = module.add_node(curios_cont::Node::LetIntrinsic {
        result: len,
        op: curios_cont::Intrinsic::ListLen,
        args: vec![curios_cont::Atom::Value(list)],
        next: exit,
    });
    let build = module.add_node(curios_cont::Node::LetValue {
        result: list,
        value: curios_cont::ValueExpr::List(vec![nat(1), nat(2), nat(3)]),
        next: measure,
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
    module
}

/// A packed-bytes literal then its length.
pub(super) fn bin_len() -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let bin = module.add_value(Some("bin".into()));
    let len = module.add_value(Some("len".into()));
    let exit = module.add_node(curios_cont::Node::Exit {
        value: Some(curios_cont::Atom::Value(len)),
    });
    let measure = module.add_node(curios_cont::Node::LetIntrinsic {
        result: len,
        op: curios_cont::Intrinsic::BinLen(Grain::X),
        args: vec![curios_cont::Atom::Value(bin)],
        next: exit,
    });
    let build = module.add_node(curios_cont::Node::LetValue {
        result: bin,
        // Four bytes: one past the small-canonical envelope, so the literal exercises the rope path these fixtures pin rather than the immediate a smaller value now rides.
        value: curios_cont::ValueExpr::Literal(curios_cont::Literal::Bin(
            Grain::X,
            PackedBin::from_bytes(vec![1, 2, 3, 4]),
        )),
        next: measure,
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
    module
}

/// Allocate a cell, read it back, and exit with the value.
pub(super) fn cell_roundtrip() -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let cell = module.add_value(Some("cell".into()));
    let value = module.add_value(Some("value".into()));
    let exit = module.add_node(curios_cont::Node::Exit {
        value: Some(curios_cont::Atom::Value(value)),
    });
    let get_k = module.add_continuation(curios_cont::Continuation {
        debug_name: Some("got".into()),
        params: vec![value],
        body: exit,
    });
    let get = module.add_node(curios_cont::Node::Cell {
        op: curios_cont::CellOp::Get,
        args: vec![curios_cont::Atom::Value(cell)],
        return_to: get_k,
    });
    let new_k = module.add_continuation(curios_cont::Continuation {
        debug_name: Some("made".into()),
        params: vec![cell],
        body: get,
    });
    let new = module.add_node(curios_cont::Node::Cell {
        op: curios_cont::CellOp::New,
        args: vec![nat(0)],
        return_to: new_k,
    });
    let body = module.add_node(curios_cont::Node::LetCont {
        continuations: vec![new_k, get_k],
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
    module
}

// --- Foreign ABI ----------------------------------------------------------

/// A host call whose signature has `results` results, resuming into a continuation that binds them all and exits with the first.
pub(super) fn foreign_call(name: &str) -> curios_cont::Module {
    let function = host_ops()
        .get(name)
        .unwrap_or_else(|| panic!("host_ops defines {name}"))
        .clone();
    let arity = function.signature.params.len();
    let results = function.signature.results.len();

    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let bound = (0..results)
        .map(|i| module.add_value(Some(format!("result{i}"))))
        .collect::<Vec<_>>();
    let exit = module.add_node(curios_cont::Node::Exit {
        value: bound.first().copied().map(curios_cont::Atom::Value),
    });
    let resume = module.add_continuation(curios_cont::Continuation {
        debug_name: Some("resume".into()),
        params: bound,
        body: exit,
    });
    let call = module.add_node(curios_cont::Node::Foreign {
        function,
        args: (0..arity).map(|_| nat(0)).collect(),
        return_to: resume,
    });
    let body = module.add_node(curios_cont::Node::LetCont {
        continuations: vec![resume],
        body: call,
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
    module
}

// --- Higher-order / closure ABI + module wiring ---------------------------

/// `main` builds a closure of `target` and passes it to `apply`, which invokes it indirectly — an unknown callee that must go through the closure ABI.
pub(super) fn indirect_apply() -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let target = module.reserve_function();
    let apply = module.reserve_function();

    let target_return = module.reserve_continuation();
    let target_body = module.add_node(curios_cont::Node::ApplyCont(curios_cont::Edge {
        target: target_return,
        args: vec![nat(0)],
    }));
    module.define_function(
        target,
        curios_cont::Function {
            debug_name: Some("target".into()),
            params: vec![],
            return_cont: target_return,
            body: target_body,
            droppable: false,
        },
    );

    let closure = module.add_value(Some("closure".into()));
    let apply_return = module.reserve_continuation();
    let apply_body = module.add_node(curios_cont::Node::ApplyFun {
        callee: curios_cont::Callee::Closure(closure),
        args: vec![],
        return_to: apply_return,
    });
    module.define_function(
        apply,
        curios_cont::Function {
            debug_name: Some("apply".into()),
            params: vec![closure],
            return_cont: apply_return,
            body: apply_body,
            droppable: false,
        },
    );

    let main_return = module.reserve_continuation();
    let call = module.add_node(curios_cont::Node::ApplyFun {
        callee: curios_cont::Callee::Known(apply),
        args: vec![curios_cont::Atom::Fun(target)],
        return_to: main_return,
    });
    let body = module.add_node(curios_cont::Node::LetFun {
        functions: vec![target, apply],
        body: call,
    });
    module.define_function(
        main,
        curios_cont::Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body,
            droppable: false,
        },
    );
    module.set_entry(main);
    module
}

// --- Rope operations ------------------------------------------------------

pub(super) fn bin_lit(bytes: Vec<u8>) -> curios_cont::Atom {
    curios_cont::Atom::Literal(curios_cont::Literal::Bin(
        Grain::X,
        PackedBin::from_bytes(bytes),
    ))
}

/// A list literal read at an index — the list-rope read helper.
pub(super) fn list_read() -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let list = module.add_value(Some("list".into()));
    let elem = module.add_value(Some("elem".into()));
    let exit = module.add_node(curios_cont::Node::Exit {
        value: Some(curios_cont::Atom::Value(elem)),
    });
    let read = module.add_node(curios_cont::Node::LetIntrinsic {
        result: elem,
        op: curios_cont::Intrinsic::ListGet,
        args: vec![curios_cont::Atom::Value(list), nat(0)],
        next: exit,
    });
    let build = module.add_node(curios_cont::Node::LetValue {
        result: list,
        value: curios_cont::ValueExpr::List(vec![nat(7), nat(8)]),
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
    module
}

/// Map a closure over a list literal — the `list/map` intrinsic, which threads the mapping function through as a closure and services the fill via the shared helper.
pub(super) fn list_map() -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let mapper = module.reserve_function();

    let mapper_return = module.reserve_continuation();
    let element = module.add_value(Some("element".into()));
    let mapper_body = module.add_node(curios_cont::Node::ApplyCont(curios_cont::Edge {
        target: mapper_return,
        args: vec![curios_cont::Atom::Value(element)],
    }));
    module.define_function(
        mapper,
        curios_cont::Function {
            debug_name: Some("mapper".into()),
            params: vec![element],
            return_cont: mapper_return,
            body: mapper_body,
            droppable: false,
        },
    );

    let list = module.add_value(Some("list".into()));
    let mapped = module.add_value(Some("mapped".into()));
    let exit = module.add_node(curios_cont::Node::Exit {
        value: Some(curios_cont::Atom::Value(mapped)),
    });
    let resume = module.add_continuation(curios_cont::Continuation {
        debug_name: Some("mapped".into()),
        params: vec![mapped],
        body: exit,
    });
    let map = module.add_node(curios_cont::Node::Intrinsic {
        op: curios_cont::IntrinsicCall::ListMap,
        args: vec![
            curios_cont::Atom::Value(list),
            curios_cont::Atom::Fun(mapper),
        ],
        return_to: resume,
    });
    let with_cont = module.add_node(curios_cont::Node::LetCont {
        continuations: vec![resume],
        body: map,
    });
    let build = module.add_node(curios_cont::Node::LetValue {
        result: list,
        value: curios_cont::ValueExpr::List(vec![nat(1), nat(2)]),
        next: with_cont,
    });
    let body = module.add_node(curios_cont::Node::LetFun {
        functions: vec![mapper],
        body: build,
    });
    let return_cont = module.reserve_continuation();
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
    module
}

/// A long left-leaning chain of appends, each over the previous result — the compile-time analogue of the deleted deep-rope fixtures. Lowering must stay on the default test-thread stack (iterative, never widened), so the only assertion that matters is that `into_wasm` returns at all.
pub(super) fn deep_bin_chain(depth: usize) -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let values = (0..depth)
        .map(|i| module.add_value(Some(format!("v{i}"))))
        .collect::<Vec<_>>();
    let mut next = module.add_node(curios_cont::Node::Exit {
        value: Some(curios_cont::Atom::Value(values[depth - 1])),
    });
    for i in (0..depth).rev() {
        let carrier = if i == 0 {
            bin_lit(vec![0])
        } else {
            curios_cont::Atom::Value(values[i - 1])
        };
        next = module.add_node(curios_cont::Node::LetIntrinsic {
            result: values[i],
            op: curios_cont::Intrinsic::BinAppend(Grain::X),
            args: vec![carrier, nat(1)],
            next,
        });
    }
    module.define_function(
        main,
        curios_cont::Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body: next,
            droppable: false,
        },
    );
    module.set_entry(main);
    module
}

// --- Control-flow structuring (loops vs. localized dispatch) ---------------

/// A single self-recursive continuation: one entry, so a reducible natural loop.
pub(super) fn reducible_loop() -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let loop_cont = module.reserve_continuation();
    let counter = module.add_value(Some("counter".into()));
    let again = module.add_node(curios_cont::Node::ApplyCont(curios_cont::Edge {
        target: loop_cont,
        args: vec![curios_cont::Atom::Value(counter)],
    }));
    module.define_continuation(
        loop_cont,
        curios_cont::Continuation {
            debug_name: Some("loop".into()),
            params: vec![counter],
            body: again,
        },
    );
    let enter = module.add_node(curios_cont::Node::ApplyCont(curios_cont::Edge {
        target: loop_cont,
        args: vec![nat(0)],
    }));
    let body = module.add_node(curios_cont::Node::LetCont {
        continuations: vec![loop_cont],
        body: enter,
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
    module
}

/// Two continuations that jump to each other, entered from *both* arms of a switch — a two-entry (irreducible) component that only a localized dispatcher can structure.
pub(super) fn irreducible_pair() -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let k1 = module.reserve_continuation();
    let k2 = module.reserve_continuation();

    let to_k2 = module.add_node(curios_cont::Node::ApplyCont(curios_cont::Edge {
        target: k2,
        args: vec![],
    }));
    module.define_continuation(
        k1,
        curios_cont::Continuation {
            debug_name: Some("k1".into()),
            params: vec![],
            body: to_k2,
        },
    );
    let to_k1 = module.add_node(curios_cont::Node::ApplyCont(curios_cont::Edge {
        target: k1,
        args: vec![],
    }));
    module.define_continuation(
        k2,
        curios_cont::Continuation {
            debug_name: Some("k2".into()),
            params: vec![],
            body: to_k1,
        },
    );

    let switch = module.add_node(curios_cont::Node::Switch {
        scrutinee: nat(0),
        cases: BTreeMap::from([
            (
                0,
                curios_cont::Edge {
                    target: k1,
                    args: vec![],
                },
            ),
            (
                1,
                curios_cont::Edge {
                    target: k2,
                    args: vec![],
                },
            ),
        ]),
        default: None,
    });
    let body = module.add_node(curios_cont::Node::LetCont {
        continuations: vec![k1, k2],
        body: switch,
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
    module
}

/// Two bindings of the same constant tuple, the second projected — the hoister must intern both to one global.
pub(super) fn constant_tuple_pair() -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let first = module.add_value(Some("first".into()));
    let second = module.add_value(Some("second".into()));
    let got = module.add_value(Some("got".into()));
    let exit = module.add_node(curios_cont::Node::Exit {
        value: Some(curios_cont::Atom::Value(got)),
    });
    let project = module.add_node(curios_cont::Node::LetIntrinsic {
        result: got,
        op: curios_cont::Intrinsic::TupleGet(0),
        args: vec![curios_cont::Atom::Value(second)],
        next: exit,
    });
    let build_second = module.add_node(curios_cont::Node::LetValue {
        result: second,
        value: curios_cont::ValueExpr::Tuple(vec![nat(2)]),
        next: project,
    });
    let build_first = module.add_node(curios_cont::Node::LetValue {
        result: first,
        value: curios_cont::ValueExpr::Tuple(vec![nat(2)]),
        next: build_second,
    });
    module.define_function(
        main,
        curios_cont::Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body: build_first,
            droppable: false,
        },
    );
    module.set_entry(main);
    module
}

/// A tuple over a computed element — not constant, so it must stay an inline allocation.
pub(super) fn runtime_tuple() -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let sum = module.add_value(Some("sum".into()));
    let tuple = module.add_value(Some("tuple".into()));
    let got = module.add_value(Some("got".into()));
    let exit = module.add_node(curios_cont::Node::Exit {
        value: Some(curios_cont::Atom::Value(got)),
    });
    let project = module.add_node(curios_cont::Node::LetIntrinsic {
        result: got,
        op: curios_cont::Intrinsic::TupleGet(0),
        args: vec![curios_cont::Atom::Value(tuple)],
        next: exit,
    });
    let build = module.add_node(curios_cont::Node::LetValue {
        result: tuple,
        value: curios_cont::ValueExpr::Tuple(vec![curios_cont::Atom::Value(sum)]),
        next: project,
    });
    let compute = module.add_node(curios_cont::Node::LetIntrinsic {
        result: sum,
        op: curios_cont::Intrinsic::NatAdd,
        args: vec![nat(1), nat(2)],
        next: build,
    });
    module.define_function(
        main,
        curios_cont::Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body: compute,
            droppable: false,
        },
    );
    module.set_entry(main);
    module
}

/// A tuple over a scalar past the i31 — a boxed magnitude, whose limbs no constant expression can read.
pub(super) fn big_tuple() -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let tuple = module.add_value(Some("tuple".into()));
    let got = module.add_value(Some("got".into()));
    let exit = module.add_node(curios_cont::Node::Exit {
        value: Some(curios_cont::Atom::Value(got)),
    });
    let project = module.add_node(curios_cont::Node::LetIntrinsic {
        result: got,
        op: curios_cont::Intrinsic::TupleGet(0),
        args: vec![curios_cont::Atom::Value(tuple)],
        next: exit,
    });
    let build = module.add_node(curios_cont::Node::LetValue {
        result: tuple,
        value: curios_cont::ValueExpr::Tuple(vec![nat(0x8000_0000)]),
        next: project,
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
    module
}
