//! Emitting a fixture and reading the text back: the harness the Wasm emission suites assert through, and the programs they emit.
//!
//! `pub(super)` rather than private: consumed by the suites across the crate, and by nothing outside it.
//!
//! Backend lowering coverage: build a [`curios_cps::Module`](curios_cps::Module) directly, lower it with [`into_wasm`](crate::into_wasm()), and assert the *shape* of the emitted wasm (its WAT text). These are the shape half of a split whose semantic half runs end to end, in `curios/src/tests/codegen` and the native `.crs` corpus. `into_wasm` performs no optimization, so a `LetIntrinsic` over literal operands lowers one-for-one without constant folding, and the emitted instruction is exactly what codegen chose.

use {
    crate::into_wasm,
    curios_abi::{ForeignFunction, WireResults, WireReturn, WireSignature, WireType},
    curios_num::{Binary, Floating, Integer, Natural},
    std::{collections::BTreeMap, sync::Arc},
};

/// The row a program declares as `/name`, taking `params` and coming back with `returns`.
fn declared(name: &str, params: Vec<WireType>, returns: WireReturn) -> Arc<ForeignFunction> {
    Arc::new(ForeignFunction {
        name: format!("/{name}"),
        signature: WireSignature { params, returns },
    })
}

/// The row a program declares as `/name`, taking `params` and answering `results`.
pub(super) fn row(name: &str, params: Vec<WireType>, results: WireResults) -> Arc<ForeignFunction> {
    declared(name, params, WireReturn::Answers(results))
}

/// A halt reading `args`, through a row that does not return, `/exit: (Byte) -> Never` — the terminal a fixture ends with when what it passes must stay live.
pub(super) fn halt(args: Vec<curios_cps::Atom>) -> curios_cps::Node {
    curios_cps::Node::Halt {
        function: declared("exit", vec![WireType::Byte], WireReturn::Never),
        args,
    }
}

/// A halt reading nothing of the program's own: its code is the literal zero.
pub(super) fn halt_zero() -> curios_cps::Node {
    halt(vec![nat(0)])
}

/// The emitted module rendered as WAT text — the public inspection surface (`Module`'s items are private; `Display` is how consumers read it back).
pub(super) fn wat(module: &curios_cps::Module) -> String {
    into_wasm(module).0.to_string()
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

/// The refusal calls a module makes besides its closing halt's: a fixture halts with the value it computed, whose narrowing to the wire can refuse whatever the operation under test did.
pub(super) fn refusals_besides_the_exit(wat: &str) -> usize {
    count(wat, "call $refuse/") - count(wat, "call $refuse/nat_wire")
}

pub(super) fn nat(value: u32) -> curios_cps::Atom {
    curios_cps::Atom::Literal(curios_cps::Literal::Nat(Natural::from(value)))
}

pub(super) fn int(value: i32) -> curios_cps::Atom {
    curios_cps::Atom::Literal(curios_cps::Literal::Int(Integer::from(value)))
}

pub(super) fn flt(value: f64) -> curios_cps::Atom {
    curios_cps::Atom::Literal(curios_cps::Literal::Flt(Floating::from(value)))
}

/// A nullary `main` that binds one intrinsic over `args` and exits with the result — compute one thing, exit with it. `into_wasm` does not fold, so the op lowers verbatim.
pub(super) fn intrinsic_main(
    op: curios_cps::Intrinsic,
    args: Vec<curios_cps::Atom>,
) -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let result = module.add_value(Some("result".into()));
    let exit = module.add_node(halt(vec![curios_cps::Atom::Value(result)]));
    let body = module.add_node(curios_cps::Node::LetIntrinsic {
        result,
        op,
        args,
        next: exit,
    });
    module.define_function(
        main,
        curios_cps::Function {
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
pub(super) fn tuple_project() -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let tuple = module.add_value(Some("tuple".into()));
    let field = module.add_value(Some("field".into()));
    let exit = module.add_node(halt(vec![curios_cps::Atom::Value(field)]));
    let project = module.add_node(curios_cps::Node::LetIntrinsic {
        result: field,
        op: curios_cps::Intrinsic::TupleGet(0),
        args: vec![curios_cps::Atom::Value(tuple)],
        next: exit,
    });
    let build = module.add_node(curios_cps::Node::LetValue {
        result: tuple,
        value: curios_cps::ValueExpr::Tuple(vec![nat(1), nat(2)]),
        next: project,
    });
    module.define_function(
        main,
        curios_cps::Function {
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
pub(super) fn list_len() -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let list = module.add_value(Some("list".into()));
    let len = module.add_value(Some("len".into()));
    let exit = module.add_node(halt(vec![curios_cps::Atom::Value(len)]));
    let measure = module.add_node(curios_cps::Node::LetIntrinsic {
        result: len,
        op: curios_cps::Intrinsic::ListLen,
        args: vec![curios_cps::Atom::Value(list)],
        next: exit,
    });
    let build = module.add_node(curios_cps::Node::LetValue {
        result: list,
        value: curios_cps::ValueExpr::List(vec![nat(1), nat(2), nat(3)]),
        next: measure,
    });
    module.define_function(
        main,
        curios_cps::Function {
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
pub(super) fn bytes_len() -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let bytes = module.add_value(Some("bytes".into()));
    let len = module.add_value(Some("len".into()));
    let exit = module.add_node(halt(vec![curios_cps::Atom::Value(len)]));
    let measure = module.add_node(curios_cps::Node::LetIntrinsic {
        result: len,
        op: curios_cps::Intrinsic::BytesLen,
        args: vec![curios_cps::Atom::Value(bytes)],
        next: exit,
    });
    let build = module.add_node(curios_cps::Node::LetValue {
        result: bytes,
        // Four bytes: one past the small-canonical envelope, so the literal exercises the rope path these fixtures pin rather than the immediate a smaller value rides.
        value: curios_cps::ValueExpr::Literal(curios_cps::Literal::Bytes(Binary::from_bytes(
            vec![1, 2, 3, 4],
        ))),
        next: measure,
    });
    module.define_function(
        main,
        curios_cps::Function {
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

/// Allocate a cell, fill and poll it, and exit with the value.
pub(super) fn cell_roundtrip() -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let cell = module.add_value(Some("cell".into()));
    let present = module.add_value(None);
    let filled = module.add_value(None);
    let value = module.add_value(Some("value".into()));
    let exit = module.add_node(halt(vec![curios_cps::Atom::Value(value)]));
    let get_k = module.add_continuation(curios_cps::Continuation {
        debug_name: Some("got".into()),
        params: vec![present, value],
        body: exit,
    });
    let get = module.add_node(curios_cps::Node::Cell {
        op: curios_cps::CellOp::Poll,
        args: vec![curios_cps::Atom::Value(cell)],
        return_to: get_k,
    });
    let filled_k = module.add_continuation(curios_cps::Continuation {
        debug_name: Some("filled".into()),
        params: vec![filled],
        body: get,
    });
    let fill = module.add_node(curios_cps::Node::Cell {
        op: curios_cps::CellOp::Fill,
        args: vec![curios_cps::Atom::Value(cell), nat(0)],
        return_to: filled_k,
    });
    let operations = module.add_node(curios_cps::Node::LetCont {
        continuations: vec![filled_k, get_k],
        body: fill,
    });
    let new_k = module.add_continuation(curios_cps::Continuation {
        debug_name: Some("made".into()),
        params: vec![cell],
        body: operations,
    });
    let new = module.add_node(curios_cps::Node::Cell {
        op: curios_cps::CellOp::Reserve,
        args: vec![],
        return_to: new_k,
    });
    let body = module.add_node(curios_cps::Node::LetCont {
        continuations: vec![new_k],
        body: new,
    });
    module.define_function(
        main,
        curios_cps::Function {
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

pub(super) fn channel_roundtrip() -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let channel = module.add_value(None);
    let pushed = module.add_value(None);
    let taken = module.add_value(None);
    let payload = module.add_value(None);
    let exit = module.add_node(halt(vec![curios_cps::Atom::Value(payload)]));
    let after_take = module.add_continuation(curios_cps::Continuation {
        debug_name: None,
        params: vec![taken, payload],
        body: exit,
    });
    let take = module.add_node(curios_cps::Node::Channel {
        op: curios_cps::ChannelOp::Take,
        args: vec![curios_cps::Atom::Value(channel)],
        return_to: after_take,
    });
    let after_push = module.add_continuation(curios_cps::Continuation {
        debug_name: None,
        params: vec![pushed],
        body: take,
    });
    let push = module.add_node(curios_cps::Node::Channel {
        op: curios_cps::ChannelOp::Push,
        args: vec![curios_cps::Atom::Value(channel), nat(0)],
        return_to: after_push,
    });
    let body = module.add_node(curios_cps::Node::LetCont {
        continuations: vec![after_push, after_take],
        body: push,
    });
    let after_new = module.add_continuation(curios_cps::Continuation {
        debug_name: None,
        params: vec![channel],
        body,
    });
    let new = module.add_node(curios_cps::Node::Channel {
        op: curios_cps::ChannelOp::New,
        args: vec![nat(1)],
        return_to: after_new,
    });
    let body = module.add_node(curios_cps::Node::LetCont {
        continuations: vec![after_new],
        body: new,
    });
    module.define_function(
        main,
        curios_cps::Function {
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

/// A host call whose signature has `results` results, resuming into a continuation that binds them all and the lane's token and exits with the first. Its operands are zeros, the lane's token an empty list.
pub(super) fn foreign_call_to(function: Arc<ForeignFunction>) -> curios_cps::Module {
    let params = function.signature().imported_params();
    let results = function.signature().imported_results().len();

    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let bound = (0..results)
        .map(|i| module.add_value(Some(format!("result{i}"))))
        .collect::<Vec<_>>();
    let exit = module.add_node(match bound.first() {
        Some(&first) => halt(vec![curios_cps::Atom::Value(first)]),
        None => halt_zero(),
    });
    let resume = module.add_continuation(curios_cps::Continuation {
        debug_name: Some("resume".into()),
        params: bound,
        body: exit,
    });
    let none = module.add_value(Some("none".into()));
    let args = params
        .iter()
        .map(|wire| match wire {
            WireType::List(_) | WireType::Option(_) => curios_cps::Atom::Value(none),
            _ => nat(0),
        })
        .collect();
    let call = module.add_node(curios_cps::Node::Foreign {
        function,
        args,
        return_to: resume,
    });
    let call = module.add_node(curios_cps::Node::LetValue {
        result: none,
        value: curios_cps::ValueExpr::List(Vec::new()),
        next: call,
    });
    let body = module.add_node(curios_cps::Node::LetCont {
        continuations: vec![resume],
        body: call,
    });
    module.define_function(
        main,
        curios_cps::Function {
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
pub(super) fn indirect_apply() -> curios_cps::Module {
    indirect_application(false)
}

pub(super) fn indirect_apply_discarding_result() -> curios_cps::Module {
    indirect_application(true)
}

fn indirect_application(discard_result: bool) -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let target = module.reserve_function();
    let apply = module.reserve_function();

    let target_return = module.reserve_continuation();
    let target_body = module.add_node(curios_cps::Node::ApplyCont(curios_cps::Edge {
        target: target_return,
        args: vec![nat(0)],
    }));
    module.define_function(
        target,
        curios_cps::Function {
            debug_name: Some("target".into()),
            params: vec![],
            return_cont: target_return,
            body: target_body,
            droppable: false,
        },
    );

    let closure = module.add_value(Some("closure".into()));
    let apply_return = module.reserve_continuation();
    let apply_body = module.add_node(curios_cps::Node::ApplyFun {
        callee: curios_cps::Callee::Closure(closure),
        args: vec![],
        return_to: apply_return,
    });
    module.define_function(
        apply,
        curios_cps::Function {
            debug_name: Some("apply".into()),
            params: vec![closure],
            return_cont: apply_return,
            body: apply_body,
            droppable: false,
        },
    );

    let main_return = module.reserve_continuation();
    let resume = discard_result.then(|| {
        let continuation = module.reserve_continuation();
        let ignored = module.add_value(None);
        let body = module.add_node(curios_cps::Node::ApplyCont(curios_cps::Edge {
            target: main_return,
            args: vec![nat(7)],
        }));
        module.define_continuation(
            continuation,
            curios_cps::Continuation {
                debug_name: None,
                params: vec![ignored],
                body,
            },
        );
        continuation
    });
    let call = module.add_node(curios_cps::Node::ApplyFun {
        callee: curios_cps::Callee::Known(apply),
        args: vec![curios_cps::Atom::Fun(target)],
        return_to: resume.unwrap_or(main_return),
    });
    let body = module.add_node(curios_cps::Node::LetFun {
        functions: vec![target, apply],
        body: call,
    });
    let body = match resume {
        Some(resume) => module.add_node(curios_cps::Node::LetCont {
            continuations: vec![resume],
            body,
        }),
        None => body,
    };
    module.define_function(
        main,
        curios_cps::Function {
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

pub(super) fn bytes_lit(bytes: Vec<u8>) -> curios_cps::Atom {
    curios_cps::Atom::Literal(curios_cps::Literal::Bytes(Binary::from_bytes(bytes)))
}

/// A list literal read at an index — the list-rope read helper.
pub(super) fn list_read() -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let list = module.add_value(Some("list".into()));
    let elem = module.add_value(Some("elem".into()));
    let exit = module.add_node(halt(vec![curios_cps::Atom::Value(elem)]));
    let read = module.add_node(curios_cps::Node::LetIntrinsic {
        result: elem,
        op: curios_cps::Intrinsic::ListGet,
        args: vec![curios_cps::Atom::Value(list), nat(0)],
        next: exit,
    });
    let build = module.add_node(curios_cps::Node::LetValue {
        result: list,
        value: curios_cps::ValueExpr::List(vec![nat(7), nat(8)]),
        next: read,
    });
    module.define_function(
        main,
        curios_cps::Function {
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

/// A list literal settled and its length read — the settle's build arm, which seals a leaf over the run's exact payload.
pub(super) fn list_settle() -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let list = module.add_value(Some("list".into()));
    let settled = module.add_value(Some("settled".into()));
    let length = module.add_value(Some("length".into()));
    let exit = module.add_node(halt(vec![curios_cps::Atom::Value(length)]));
    let measure = module.add_node(curios_cps::Node::LetIntrinsic {
        result: length,
        op: curios_cps::Intrinsic::ListLen,
        args: vec![curios_cps::Atom::Value(settled)],
        next: exit,
    });
    let settle = module.add_node(curios_cps::Node::LetIntrinsic {
        result: settled,
        op: curios_cps::Intrinsic::ListSettle,
        args: vec![curios_cps::Atom::Value(list)],
        next: measure,
    });
    let build = module.add_node(curios_cps::Node::LetValue {
        result: list,
        value: curios_cps::ValueExpr::List(vec![nat(7), nat(8)]),
        next: settle,
    });
    module.define_function(
        main,
        curios_cps::Function {
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
pub(super) fn list_map() -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let mapper = module.reserve_function();

    let mapper_return = module.reserve_continuation();
    let element = module.add_value(Some("element".into()));
    let mapper_body = module.add_node(curios_cps::Node::ApplyCont(curios_cps::Edge {
        target: mapper_return,
        args: vec![curios_cps::Atom::Value(element)],
    }));
    module.define_function(
        mapper,
        curios_cps::Function {
            debug_name: Some("mapper".into()),
            params: vec![element],
            return_cont: mapper_return,
            body: mapper_body,
            droppable: false,
        },
    );

    let list = module.add_value(Some("list".into()));
    let mapped = module.add_value(Some("mapped".into()));
    let exit = module.add_node(halt(vec![curios_cps::Atom::Value(mapped)]));
    let resume = module.add_continuation(curios_cps::Continuation {
        debug_name: Some("mapped".into()),
        params: vec![mapped],
        body: exit,
    });
    let map = module.add_node(curios_cps::Node::Intrinsic {
        op: curios_cps::IntrinsicCall::ListMap,
        args: vec![curios_cps::Atom::Value(list), curios_cps::Atom::Fun(mapper)],
        return_to: resume,
    });
    let with_cont = module.add_node(curios_cps::Node::LetCont {
        continuations: vec![resume],
        body: map,
    });
    let build = module.add_node(curios_cps::Node::LetValue {
        result: list,
        value: curios_cps::ValueExpr::List(vec![nat(1), nat(2)]),
        next: with_cont,
    });
    let body = module.add_node(curios_cps::Node::LetFun {
        functions: vec![mapper],
        body: build,
    });
    let return_cont = module.reserve_continuation();
    module.define_function(
        main,
        curios_cps::Function {
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

/// A long left-leaning chain of appends, each over the previous result. Lowering must return on the default test-thread stack, so the only assertion that matters is that `into_wasm` returns at all.
pub(super) fn deep_bytes_chain(depth: usize) -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let values = (0..depth)
        .map(|i| module.add_value(Some(format!("v{i}"))))
        .collect::<Vec<_>>();
    let mut next = module.add_node(halt(vec![curios_cps::Atom::Value(values[depth - 1])]));
    for i in (0..depth).rev() {
        let carrier = if i == 0 {
            bytes_lit(vec![0])
        } else {
            curios_cps::Atom::Value(values[i - 1])
        };
        next = module.add_node(curios_cps::Node::LetIntrinsic {
            result: values[i],
            op: curios_cps::Intrinsic::BytesAppend,
            args: vec![carrier, nat(1)],
            next,
        });
    }
    module.define_function(
        main,
        curios_cps::Function {
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
pub(super) fn reducible_loop() -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let loop_cont = module.reserve_continuation();
    let counter = module.add_value(Some("counter".into()));
    let again = module.add_node(curios_cps::Node::ApplyCont(curios_cps::Edge {
        target: loop_cont,
        args: vec![curios_cps::Atom::Value(counter)],
    }));
    module.define_continuation(
        loop_cont,
        curios_cps::Continuation {
            debug_name: Some("loop".into()),
            params: vec![counter],
            body: again,
        },
    );
    let enter = module.add_node(curios_cps::Node::ApplyCont(curios_cps::Edge {
        target: loop_cont,
        args: vec![nat(0)],
    }));
    let body = module.add_node(curios_cps::Node::LetCont {
        continuations: vec![loop_cont],
        body: enter,
    });
    module.define_function(
        main,
        curios_cps::Function {
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
pub(super) fn irreducible_pair() -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let k1 = module.reserve_continuation();
    let k2 = module.reserve_continuation();

    let to_k2 = module.add_node(curios_cps::Node::ApplyCont(curios_cps::Edge {
        target: k2,
        args: vec![],
    }));
    module.define_continuation(
        k1,
        curios_cps::Continuation {
            debug_name: Some("k1".into()),
            params: vec![],
            body: to_k2,
        },
    );
    let to_k1 = module.add_node(curios_cps::Node::ApplyCont(curios_cps::Edge {
        target: k1,
        args: vec![],
    }));
    module.define_continuation(
        k2,
        curios_cps::Continuation {
            debug_name: Some("k2".into()),
            params: vec![],
            body: to_k1,
        },
    );

    let switch = module.add_node(curios_cps::Node::Switch {
        scrutinee: nat(0),
        cases: BTreeMap::from([
            (
                0,
                curios_cps::Edge {
                    target: k1,
                    args: vec![],
                },
            ),
            (
                1,
                curios_cps::Edge {
                    target: k2,
                    args: vec![],
                },
            ),
        ]),
        default: None,
    });
    let body = module.add_node(curios_cps::Node::LetCont {
        continuations: vec![k1, k2],
        body: switch,
    });
    module.define_function(
        main,
        curios_cps::Function {
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
pub(super) fn constant_tuple_pair() -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let first = module.add_value(Some("first".into()));
    let second = module.add_value(Some("second".into()));
    let got = module.add_value(Some("got".into()));
    let exit = module.add_node(halt(vec![curios_cps::Atom::Value(got)]));
    let project = module.add_node(curios_cps::Node::LetIntrinsic {
        result: got,
        op: curios_cps::Intrinsic::TupleGet(0),
        args: vec![curios_cps::Atom::Value(second)],
        next: exit,
    });
    let build_second = module.add_node(curios_cps::Node::LetValue {
        result: second,
        value: curios_cps::ValueExpr::Tuple(vec![nat(2)]),
        next: project,
    });
    let build_first = module.add_node(curios_cps::Node::LetValue {
        result: first,
        value: curios_cps::ValueExpr::Tuple(vec![nat(2)]),
        next: build_second,
    });
    module.define_function(
        main,
        curios_cps::Function {
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
pub(super) fn runtime_tuple() -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let sum = module.add_value(Some("sum".into()));
    let tuple = module.add_value(Some("tuple".into()));
    let got = module.add_value(Some("got".into()));
    let exit = module.add_node(halt(vec![curios_cps::Atom::Value(got)]));
    let project = module.add_node(curios_cps::Node::LetIntrinsic {
        result: got,
        op: curios_cps::Intrinsic::TupleGet(0),
        args: vec![curios_cps::Atom::Value(tuple)],
        next: exit,
    });
    let build = module.add_node(curios_cps::Node::LetValue {
        result: tuple,
        value: curios_cps::ValueExpr::Tuple(vec![curios_cps::Atom::Value(sum)]),
        next: project,
    });
    let compute = module.add_node(curios_cps::Node::LetIntrinsic {
        result: sum,
        op: curios_cps::Intrinsic::NatAdd,
        args: vec![nat(1), nat(2)],
        next: build,
    });
    module.define_function(
        main,
        curios_cps::Function {
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
pub(super) fn big_tuple() -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let tuple = module.add_value(Some("tuple".into()));
    let got = module.add_value(Some("got".into()));
    let exit = module.add_node(halt(vec![curios_cps::Atom::Value(got)]));
    let project = module.add_node(curios_cps::Node::LetIntrinsic {
        result: got,
        op: curios_cps::Intrinsic::TupleGet(0),
        args: vec![curios_cps::Atom::Value(tuple)],
        next: exit,
    });
    let build = module.add_node(curios_cps::Node::LetValue {
        result: tuple,
        value: curios_cps::ValueExpr::Tuple(vec![nat(0x8000_0000)]),
        next: project,
    });
    module.define_function(
        main,
        curios_cps::Function {
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
