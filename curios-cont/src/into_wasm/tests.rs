//! Backend lowering coverage: build a [`CpsModule`](crate::CpsModule) directly, lower it with [`into_wasm`](crate::into_wasm), and assert the *shape* of the emitted wasm (its WAT text). These are the shape half of a split: the fixtures that once built the old region API and *executed* the module became shape inspection here, and end-to-end semantics in `curios/src/tests/codegen` and the native `.crs` corpus. `into_wasm` performs no optimization, so a `LetIntrinsic` over literal operands lowers one-for-one without constant folding, and the emitted instruction is exactly what codegen chose.

use {
    crate::{
        CpsAtom, CpsCallee, CpsCellOp, CpsContinuation, CpsEdge, CpsFunction, CpsIntrinsicCall,
        CpsIntrinsicOp, CpsLiteral, CpsModule, CpsNode, CpsValueExpr, into_wasm,
    },
    curios_abi::host_ops,
    curios_utilities::{Grain, PackedBin},
    std::collections::BTreeMap,
};

/// The emitted module rendered as WAT text — the public inspection surface (`Module`'s items are private; `Display` is how consumers read it back).
fn wat(module: &CpsModule) -> String {
    into_wasm(module).to_string()
}

#[track_caller]
fn assert_contains(wat: &str, needle: &str) {
    assert!(wat.contains(needle), "expected wat to contain `{needle}`");
}

#[track_caller]
fn assert_absent(wat: &str, needle: &str) {
    assert!(!wat.contains(needle), "expected wat to lack `{needle}`");
}

fn count(wat: &str, needle: &str) -> usize {
    wat.matches(needle).count()
}

/// Every `main` ends by diverging into `exit`, which the emitter follows with one `unreachable`. A trapping intrinsic adds another `unreachable` in its overflow/range guard, so the count distinguishes guarded ops from total ones without matching exact bytes.
#[track_caller]
fn assert_traps(wat: &str) {
    assert!(
        count(wat, "unreachable") >= 2,
        "expected a trap guard beyond the exit divergence",
    );
}

#[track_caller]
fn assert_total(wat: &str) {
    assert_eq!(
        count(wat, "unreachable"),
        1,
        "expected no trap guard beyond the exit divergence",
    );
}

const fn nat(value: u32) -> CpsAtom {
    CpsAtom::Literal(CpsLiteral::Nat(value))
}

const fn int(value: i32) -> CpsAtom {
    CpsAtom::Literal(CpsLiteral::Int(value))
}

const fn flt(value: f32) -> CpsAtom {
    CpsAtom::Literal(CpsLiteral::Flt(value))
}

/// A nullary `main` that binds one intrinsic over `args` and exits with the result — the CPS analogue of the deleted fixtures' "compute one thing, exit with it". `into_wasm` does not fold, so the op lowers verbatim.
fn intrinsic_main(op: CpsIntrinsicOp, args: Vec<CpsAtom>) -> CpsModule {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let result = module.add_value(Some("result".into()));
    let exit = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(result)),
    });
    let body = module.add_node(CpsNode::LetIntrinsic {
        result,
        op,
        args,
        next: exit,
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
    module
}

// --- Nat ------------------------------------------------------------------

#[test]
fn nat_add_guards_the_i31_carrier() {
    let wat = wat(&intrinsic_main(
        CpsIntrinsicOp::NatAdd,
        vec![nat(3), nat(4)],
    ));
    assert_contains(&wat, "i32.add");
    // Overflow past bit 31 traps through the special label.
    assert_traps(&wat);
}

#[test]
fn nat_sub_is_saturating_monus_without_a_guard() {
    let wat = wat(&intrinsic_main(
        CpsIntrinsicOp::NatSub,
        vec![nat(3), nat(4)],
    ));
    assert_contains(&wat, "i32.sub");
    assert_contains(&wat, "select");
    assert_total(&wat);
}

#[test]
fn nat_mul_widens_to_i64_and_guards() {
    let wat = wat(&intrinsic_main(
        CpsIntrinsicOp::NatMul,
        vec![nat(3), nat(4)],
    ));
    assert_contains(&wat, "i64.mul");
    assert_traps(&wat);
}

#[test]
fn nat_div_is_unsigned_and_rem_unsigned() {
    assert_contains(
        &wat(&intrinsic_main(
            CpsIntrinsicOp::NatDiv,
            vec![nat(9), nat(2)],
        )),
        "i32.div_u",
    );
    assert_contains(
        &wat(&intrinsic_main(
            CpsIntrinsicOp::NatRem,
            vec![nat(9), nat(2)],
        )),
        "i32.rem_u",
    );
}

#[test]
fn nat_lt_compares_unsigned() {
    let wat = wat(&intrinsic_main(CpsIntrinsicOp::NatLt, vec![nat(3), nat(4)]));
    assert_contains(&wat, "i32.lt_u");
    assert_total(&wat);
}

#[test]
fn nat_and_is_bitwise_and_total() {
    let wat = wat(&intrinsic_main(
        CpsIntrinsicOp::NatAnd,
        vec![nat(6), nat(3)],
    ));
    assert_contains(&wat, "i32.and");
    assert_total(&wat);
}

#[test]
fn nat_to_flt_converts_unsigned() {
    let wat = wat(&intrinsic_main(CpsIntrinsicOp::NatToFlt, vec![nat(7)]));
    assert_contains(&wat, "f32.convert_i32_u");
}

// --- Int ------------------------------------------------------------------

#[test]
fn int_add_guards_the_signed_carrier() {
    let wat = wat(&intrinsic_main(
        CpsIntrinsicOp::IntAdd,
        vec![int(3), int(-4)],
    ));
    assert_contains(&wat, "i32.add");
    assert_traps(&wat);
}

#[test]
fn int_mul_widens_to_i64_and_guards() {
    let wat = wat(&intrinsic_main(
        CpsIntrinsicOp::IntMul,
        vec![int(3), int(-4)],
    ));
    assert_contains(&wat, "i64.mul");
    assert_traps(&wat);
}

#[test]
fn int_div_is_signed_and_guarded() {
    let wat = wat(&intrinsic_main(
        CpsIntrinsicOp::IntDiv,
        vec![int(-9), int(2)],
    ));
    assert_contains(&wat, "i32.div_s");
    assert_traps(&wat);
}

#[test]
fn int_lt_compares_signed() {
    let wat = wat(&intrinsic_main(
        CpsIntrinsicOp::IntLt,
        vec![int(-3), int(4)],
    ));
    assert_contains(&wat, "i32.lt_s");
    assert_total(&wat);
}

// --- Flt ------------------------------------------------------------------

#[test]
fn flt_add_boxes_into_the_flt_struct() {
    let wat = wat(&intrinsic_main(
        CpsIntrinsicOp::FltAdd,
        vec![flt(1.5), flt(2.5)],
    ));
    assert_contains(&wat, "f32.add");
    assert_contains(&wat, "struct.new $flt");
}

#[test]
fn flt_div_divides() {
    assert_contains(
        &wat(&intrinsic_main(
            CpsIntrinsicOp::FltDiv,
            vec![flt(3.0), flt(2.0)],
        )),
        "f32.div",
    );
}

#[test]
fn flt_to_le_bytes_packs_a_four_byte_leaf() {
    let wat = wat(&intrinsic_main(
        CpsIntrinsicOp::FltToLeBytes,
        vec![flt(1.0)],
    ));
    assert_contains(&wat, "i32.reinterpret_f32");
    assert_contains(&wat, "array.new_fixed");
    assert_contains(&wat, "struct.new $rope/bin/leaf");
}

#[test]
fn flt_to_int_truncates_and_guards_the_range() {
    let wat = wat(&intrinsic_main(CpsIntrinsicOp::FltToInt, vec![flt(1.0)]));
    assert_contains(&wat, "i32.trunc_f32_s");
    assert_traps(&wat);
}

// --- Aggregates / packed / cells -----------------------------------------

/// Construct a tuple then project field 0 from it.
fn tuple_project() -> CpsModule {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let tuple = module.add_value(Some("tuple".into()));
    let field = module.add_value(Some("field".into()));
    let exit = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(field)),
    });
    let project = module.add_node(CpsNode::LetIntrinsic {
        result: field,
        op: CpsIntrinsicOp::TplGet(0),
        args: vec![CpsAtom::Value(tuple)],
        next: exit,
    });
    let build = module.add_node(CpsNode::LetValue {
        result: tuple,
        value: CpsValueExpr::Tuple(vec![nat(1), nat(2)]),
        next: project,
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
    module
}

#[test]
fn tuple_construction_and_projection() {
    let wat = wat(&tuple_project());
    assert_contains(&wat, "struct.new $tpl");
    assert_contains(&wat, "struct.get $tpl");
}

/// A list literal then its length.
fn list_len() -> CpsModule {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let list = module.add_value(Some("list".into()));
    let len = module.add_value(Some("len".into()));
    let exit = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(len)),
    });
    let measure = module.add_node(CpsNode::LetIntrinsic {
        result: len,
        op: CpsIntrinsicOp::ListLen,
        args: vec![CpsAtom::Value(list)],
        next: exit,
    });
    let build = module.add_node(CpsNode::LetValue {
        result: list,
        value: CpsValueExpr::List(vec![nat(1), nat(2), nat(3)]),
        next: measure,
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
    module
}

#[test]
fn list_literal_builds_a_rope_leaf() {
    let wat = wat(&list_len());
    assert_contains(&wat, "struct.new $rope/list/leaf");
    assert_contains(&wat, "array.new_fixed $elems");
}

/// A packed-bytes literal then its length.
fn bin_len() -> CpsModule {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let bin = module.add_value(Some("bin".into()));
    let len = module.add_value(Some("len".into()));
    let exit = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(len)),
    });
    let measure = module.add_node(CpsNode::LetIntrinsic {
        result: len,
        op: CpsIntrinsicOp::BinLen(Grain::X),
        args: vec![CpsAtom::Value(bin)],
        next: exit,
    });
    let build = module.add_node(CpsNode::LetValue {
        result: bin,
        // Four bytes: one past the small-canonical envelope, so the literal exercises the rope path these fixtures pin rather than the immediate a smaller value now rides.
        value: CpsValueExpr::Literal(CpsLiteral::Bin(
            Grain::X,
            PackedBin::from_bytes(vec![1, 2, 3, 4]),
        )),
        next: measure,
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
    module
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

/// Allocate a cell, read it back, and exit with the value.
fn cell_roundtrip() -> CpsModule {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let cell = module.add_value(Some("cell".into()));
    let value = module.add_value(Some("value".into()));
    let exit = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(value)),
    });
    let get_k = module.add_continuation(CpsContinuation {
        debug_name: Some("got".into()),
        params: vec![value],
        body: exit,
    });
    let get = module.add_node(CpsNode::Cell {
        op: CpsCellOp::Get,
        args: vec![CpsAtom::Value(cell)],
        return_to: get_k,
    });
    let new_k = module.add_continuation(CpsContinuation {
        debug_name: Some("made".into()),
        params: vec![cell],
        body: get,
    });
    let new = module.add_node(CpsNode::Cell {
        op: CpsCellOp::New,
        args: vec![nat(0)],
        return_to: new_k,
    });
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![new_k, get_k],
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
    module
}

#[test]
fn cell_new_and_get_use_the_cell_struct() {
    let wat = wat(&cell_roundtrip());
    assert_contains(&wat, "struct.new $cell");
    assert_contains(&wat, "struct.get $cell");
}

// --- Foreign ABI ----------------------------------------------------------

/// A host call whose signature has `results` results, resuming into a continuation that binds them all and exits with the first.
fn foreign_call(name: &str) -> CpsModule {
    let function = host_ops()
        .get(name)
        .unwrap_or_else(|| panic!("host_ops defines {name}"))
        .clone();
    let arity = function.signature.params.len();
    let results = function.signature.results.len();

    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let bound = (0..results)
        .map(|i| module.add_value(Some(format!("result{i}"))))
        .collect::<Vec<_>>();
    let exit = module.add_node(CpsNode::Exit {
        value: bound.first().copied().map(CpsAtom::Value),
    });
    let resume = module.add_continuation(CpsContinuation {
        debug_name: Some("resume".into()),
        params: bound,
        body: exit,
    });
    let call = module.add_node(CpsNode::Foreign {
        function,
        args: (0..arity).map(|_| nat(0)).collect(),
        return_to: resume,
    });
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![resume],
        body: call,
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
    module
}

#[test]
fn foreign_call_imports_and_invokes_the_host() {
    let wat = wat(&foreign_call("read"));
    assert_contains(&wat, "(import \"sys\" \"read\"");
    assert_contains(&wat, "call $host/sys/read");
}

#[test]
fn foreign_result_arity_shapes_the_resume() {
    // A single scalar result forwards straight through; a multi-result row with a reference field embeds that field back into a rope before binding it.
    let one = wat(&foreign_call("bind"));
    assert_contains(&one, "call $host/sys/bind");
    assert_absent(&one, "$bytes/embed");

    let many = wat(&foreign_call("read"));
    assert_contains(&many, "call $host/sys/read");
    assert_contains(&many, "call $bytes/embed");
}

// --- Higher-order / closure ABI + module wiring ---------------------------

/// `main` builds a closure of `target` and passes it to `apply`, which invokes it indirectly — an unknown callee that must go through the closure ABI.
fn indirect_apply() -> CpsModule {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let target = module.reserve_function();
    let apply = module.reserve_function();

    let target_return = module.reserve_continuation();
    let target_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: target_return,
        args: vec![nat(0)],
    }));
    module.define_function(
        target,
        CpsFunction {
            debug_name: Some("target".into()),
            params: vec![],
            return_cont: target_return,
            body: target_body,
        },
    );

    let closure = module.add_value(Some("closure".into()));
    let apply_return = module.reserve_continuation();
    let apply_body = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Closure(closure),
        args: vec![],
        return_to: apply_return,
    });
    module.define_function(
        apply,
        CpsFunction {
            debug_name: Some("apply".into()),
            params: vec![closure],
            return_cont: apply_return,
            body: apply_body,
        },
    );

    let main_return = module.reserve_continuation();
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(apply),
        args: vec![CpsAtom::Fun(target)],
        return_to: main_return,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![target, apply],
        body: call,
    });
    module.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body,
        },
    );
    module.set_entry(main);
    module
}

#[test]
fn unknown_callee_dispatches_through_the_closure_table() {
    let wat = wat(&indirect_apply());

    // The environment's code field is an `i32` table index, so construction writes a constant and dispatch reads it back into `call_indirect` — no funcref is ever materialized in function code.
    assert_contains(&wat, "call_indirect $clsr ");
    assert_absent(&wat, "call_ref");
    assert_absent(&wat, "ref.func");

    // One shared funcref table sized for every closure body plus the null slot 0, filled by one active segment at offset 1 in definition order.
    assert_contains(&wat, "(table $clsr i32 2 2 (ref null func))");
    assert_contains(&wat, "(elem $clsr (table $clsr) (offset i32.const 1) func");
}

#[test]
fn module_exports_the_entry_and_defines_every_function() {
    let wat = wat(&indirect_apply());
    assert_contains(&wat, "(export \"func/main\"");
    assert_contains(&wat, "(func $func/main");
    // `main`, `target`, and `apply` each lower to their own function.
    assert!(
        count(&wat, "(func $func/") >= 3,
        "expected three user functions",
    );
}

// --- Rope operations ------------------------------------------------------

fn bin_lit(bytes: Vec<u8>) -> CpsAtom {
    CpsAtom::Literal(CpsLiteral::Bin(Grain::X, PackedBin::from_bytes(bytes)))
}

#[test]
fn bin_slice_calls_the_shared_slice_helper() {
    let wat = wat(&intrinsic_main(
        CpsIntrinsicOp::BinSlice(Grain::X),
        vec![bin_lit(vec![1, 2, 3]), nat(0), nat(2)],
    ));
    assert_contains(&wat, "call $bytes/slice");
    // The helper is emitted with its own declared type — the ABI both ends share.
    assert_contains(&wat, "(func $bytes/slice (type $bytes/slice)");
}

#[test]
fn bin_read_calls_the_read_helper_and_forces_its_input() {
    let wat = wat(&intrinsic_main(
        CpsIntrinsicOp::BinGet(Grain::X),
        vec![bin_lit(vec![1, 2, 3]), nat(1)],
    ));
    assert_contains(&wat, "call $bytes/read");
    // Consuming a rope forces its outstanding view/node chain first.
    assert_contains(&wat, "call $bytes/force");
}

#[test]
fn bin_eql_calls_the_equality_helper() {
    let wat = wat(&intrinsic_main(
        CpsIntrinsicOp::BinEql(Grain::X),
        vec![bin_lit(vec![1, 2]), bin_lit(vec![1, 2])],
    ));
    assert_contains(&wat, "call $bytes/eql");
}

#[test]
fn bin_concat_builds_o1_nodes_inline_without_a_helper() {
    let wat = wat(&intrinsic_main(
        CpsIntrinsicOp::BinConcat(Grain::X, 2),
        vec![bin_lit(vec![1]), bin_lit(vec![2])],
    ));
    assert_contains(&wat, "struct.new $rope/bin/node");
    assert_absent(&wat, "call $bytes/slice");
}

/// A list literal read at an index — the list-rope read helper.
fn list_read() -> CpsModule {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let list = module.add_value(Some("list".into()));
    let elem = module.add_value(Some("elem".into()));
    let exit = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(elem)),
    });
    let read = module.add_node(CpsNode::LetIntrinsic {
        result: elem,
        op: CpsIntrinsicOp::ListGet,
        args: vec![CpsAtom::Value(list), nat(0)],
        next: exit,
    });
    let build = module.add_node(CpsNode::LetValue {
        result: list,
        value: CpsValueExpr::List(vec![nat(7), nat(8)]),
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
    module
}

#[test]
fn list_read_calls_the_list_read_helper() {
    assert_contains(&wat(&list_read()), "call $list/read");
}

/// Map a closure over a list literal — the `list/map` intrinsic, which threads the mapping function through as a closure and services the fill via the shared helper.
fn list_map() -> CpsModule {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let mapper = module.reserve_function();

    let mapper_return = module.reserve_continuation();
    let element = module.add_value(Some("element".into()));
    let mapper_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: mapper_return,
        args: vec![CpsAtom::Value(element)],
    }));
    module.define_function(
        mapper,
        CpsFunction {
            debug_name: Some("mapper".into()),
            params: vec![element],
            return_cont: mapper_return,
            body: mapper_body,
        },
    );

    let list = module.add_value(Some("list".into()));
    let mapped = module.add_value(Some("mapped".into()));
    let exit = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(mapped)),
    });
    let resume = module.add_continuation(CpsContinuation {
        debug_name: Some("mapped".into()),
        params: vec![mapped],
        body: exit,
    });
    let map = module.add_node(CpsNode::Intrinsic {
        op: CpsIntrinsicCall::ListMap,
        args: vec![CpsAtom::Value(list), CpsAtom::Fun(mapper)],
        return_to: resume,
    });
    let with_cont = module.add_node(CpsNode::LetCont {
        continuations: vec![resume],
        body: map,
    });
    let build = module.add_node(CpsNode::LetValue {
        result: list,
        value: CpsValueExpr::List(vec![nat(1), nat(2)]),
        next: with_cont,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![mapper],
        body: build,
    });
    let return_cont = module.reserve_continuation();
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
    module
}

#[test]
fn list_map_calls_the_map_helper() {
    assert_contains(&wat(&list_map()), "call $list/map");
}

/// A long left-leaning chain of appends, each over the previous result — the compile-time analogue of the deleted deep-rope fixtures. Lowering must stay on the default test-thread stack (iterative, never widened), so the only assertion that matters is that `into_wasm` returns at all.
fn deep_bin_chain(depth: usize) -> CpsModule {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let values = (0..depth)
        .map(|i| module.add_value(Some(format!("v{i}"))))
        .collect::<Vec<_>>();
    let mut next = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(values[depth - 1])),
    });
    for i in (0..depth).rev() {
        let carrier = if i == 0 {
            bin_lit(vec![0])
        } else {
            CpsAtom::Value(values[i - 1])
        };
        next = module.add_node(CpsNode::LetIntrinsic {
            result: values[i],
            op: CpsIntrinsicOp::BinAppend(Grain::X),
            args: vec![carrier, nat(1)],
            next,
        });
    }
    module.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body: next,
        },
    );
    module.set_entry(main);
    module
}

#[test]
fn deep_rope_chain_lowers_on_the_default_stack() {
    let wat = wat(&deep_bin_chain(5000));
    assert_contains(&wat, "struct.new $rope/bin/node");
}

// --- Control-flow structuring (loops vs. localized dispatch) ---------------

/// A single self-recursive continuation: one entry, so a reducible natural loop.
fn reducible_loop() -> CpsModule {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let loop_cont = module.reserve_continuation();
    let counter = module.add_value(Some("counter".into()));
    let again = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: loop_cont,
        args: vec![CpsAtom::Value(counter)],
    }));
    module.define_continuation(
        loop_cont,
        CpsContinuation {
            debug_name: Some("loop".into()),
            params: vec![counter],
            body: again,
        },
    );
    let enter = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: loop_cont,
        args: vec![nat(0)],
    }));
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![loop_cont],
        body: enter,
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
    module
}

/// Two continuations that jump to each other, entered from *both* arms of a switch — a two-entry (irreducible) component that only a localized dispatcher can structure.
fn irreducible_pair() -> CpsModule {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let k1 = module.reserve_continuation();
    let k2 = module.reserve_continuation();

    let to_k2 = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: k2,
        args: vec![],
    }));
    module.define_continuation(
        k1,
        CpsContinuation {
            debug_name: Some("k1".into()),
            params: vec![],
            body: to_k2,
        },
    );
    let to_k1 = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: k1,
        args: vec![],
    }));
    module.define_continuation(
        k2,
        CpsContinuation {
            debug_name: Some("k2".into()),
            params: vec![],
            body: to_k1,
        },
    );

    let switch = module.add_node(CpsNode::Switch {
        scrutinee: nat(0),
        cases: BTreeMap::from([
            (
                0,
                CpsEdge {
                    target: k1,
                    args: vec![],
                },
            ),
            (
                1,
                CpsEdge {
                    target: k2,
                    args: vec![],
                },
            ),
        ]),
        default: None,
    });
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![k1, k2],
        body: switch,
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
    module
}

#[test]
fn a_single_entry_loop_needs_no_dispatcher() {
    let wat = wat(&reducible_loop());
    assert_contains(&wat, "loop $$loop/");
    assert_absent(&wat, "dispatch");
}

#[test]
fn an_irreducible_component_uses_exactly_one_localized_dispatcher() {
    let wat = wat(&irreducible_pair());
    assert_contains(&wat, "loop $$dispatch/");
    assert_eq!(
        count(&wat, "loop $$dispatch/"),
        1,
        "expected exactly one localized dispatcher",
    );
}

/// Two bindings of the same constant tuple, the second projected — the hoister must intern both to one global.
fn constant_tuple_pair() -> CpsModule {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let first = module.add_value(Some("first".into()));
    let second = module.add_value(Some("second".into()));
    let got = module.add_value(Some("got".into()));
    let exit = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(got)),
    });
    let project = module.add_node(CpsNode::LetIntrinsic {
        result: got,
        op: CpsIntrinsicOp::TplGet(0),
        args: vec![CpsAtom::Value(second)],
        next: exit,
    });
    let build_second = module.add_node(CpsNode::LetValue {
        result: second,
        value: CpsValueExpr::Tuple(vec![nat(2)]),
        next: project,
    });
    let build_first = module.add_node(CpsNode::LetValue {
        result: first,
        value: CpsValueExpr::Tuple(vec![nat(2)]),
        next: build_second,
    });
    module.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body: build_first,
        },
    );
    module.set_entry(main);
    module
}

#[test]
fn constant_tuples_hoist_to_one_interned_global() {
    let wat = wat(&constant_tuple_pair());
    // One construction in the start function serves both bindings; the projection reads it back through the const global.
    assert_eq!(count(&wat, "struct.new $tpl/1"), 1);
    assert_contains(&wat, "global.set $const/");
    assert_contains(&wat, "global.get $const/");
}

/// A tuple over a computed element — not constant, so it must stay an inline allocation.
fn runtime_tuple() -> CpsModule {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let sum = module.add_value(Some("sum".into()));
    let tuple = module.add_value(Some("tuple".into()));
    let got = module.add_value(Some("got".into()));
    let exit = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(got)),
    });
    let project = module.add_node(CpsNode::LetIntrinsic {
        result: got,
        op: CpsIntrinsicOp::TplGet(0),
        args: vec![CpsAtom::Value(tuple)],
        next: exit,
    });
    let build = module.add_node(CpsNode::LetValue {
        result: tuple,
        value: CpsValueExpr::Tuple(vec![CpsAtom::Value(sum)]),
        next: project,
    });
    let compute = module.add_node(CpsNode::LetIntrinsic {
        result: sum,
        op: CpsIntrinsicOp::NatAdd,
        args: vec![nat(1), nat(2)],
        next: build,
    });
    module.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body: compute,
        },
    );
    module.set_entry(main);
    module
}

#[test]
fn runtime_tuples_stay_inline() {
    let wat = wat(&runtime_tuple());
    assert_contains(&wat, "struct.new $tpl/1");
    assert_absent(&wat, "const/");
}

#[test]
fn constant_bin_literals_hoist_into_a_start_initialized_global() {
    let wat = wat(&bin_len());
    assert_contains(&wat, "global.set $const/");
    assert_contains(&wat, "global.get $const/");
    assert_eq!(count(&wat, "array.new_data"), 1);
}

/// A tuple over an i31-overflowing scalar — its materialization is a trap, which must stay at its execution point instead of failing validation inside a global initializer.
fn overflowing_tuple() -> CpsModule {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let tuple = module.add_value(Some("tuple".into()));
    let got = module.add_value(Some("got".into()));
    let exit = module.add_node(CpsNode::Exit {
        value: Some(CpsAtom::Value(got)),
    });
    let project = module.add_node(CpsNode::LetIntrinsic {
        result: got,
        op: CpsIntrinsicOp::TplGet(0),
        args: vec![CpsAtom::Value(tuple)],
        next: exit,
    });
    let build = module.add_node(CpsNode::LetValue {
        result: tuple,
        value: CpsValueExpr::Tuple(vec![nat(0x8000_0000)]),
        next: project,
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
    module
}

#[test]
fn overflowing_scalars_and_their_aggregates_stay_inline() {
    let wat = wat(&overflowing_tuple());
    assert_absent(&wat, "const/");
    assert_traps(&wat);
}
