//! The emitted module's exports and dispatch table, and the loop shapes that need a dispatcher.

//! Backend lowering coverage: build a [`CpsModule`](crate::CpsModule) directly, lower it with [`into_wasm`](crate::into_wasm), and assert the *shape* of the emitted wasm (its WAT text). These are the shape half of a split: the fixtures that once built the old region API and *executed* the module became shape inspection here, and end-to-end semantics in `curios/src/tests/codegen` and the native `.crs` corpus. `into_wasm` performs no optimization, so a `LetIntrinsic` over literal operands lowers one-for-one without constant folding, and the emitted instruction is exactly what codegen chose.

use {
    super::test_support::*,
    crate::{CpsFunction, CpsIntrinsic, CpsModule, CpsNode, Panic},
};

#[test]
fn unknown_callee_dispatches_through_the_closure_table() {
    let wat = wat(&indirect_apply());

    // The environment's code field is an `i32` table index, so construction writes a constant and dispatch reads it back into `call_indirect` — no funcref is ever materialized in function code. The one `ref.func` in the module is the element segment's item.
    assert_contains(&wat, "call_indirect $clsr/0 ");
    assert_absent(&wat, "call_ref");
    assert_eq!(count(&wat, "ref.func"), 1, "ref.func outside the segment");

    // One table per dispatch arity, typed at the arity's own final func type so the `call_indirect` signature check is satisfied statically, sized for that arity's bodies plus the null slot 0, and filled by one active typed-expression segment at offset 1 in definition order.
    assert_contains(&wat, "(table $clsr/0 i32 2 2 (ref null $clsr/0))");
    assert_contains(
        &wat,
        "(elem $clsr/0 (table $clsr/0) (offset i32.const 1) (ref $clsr/0)",
    );
}

#[test]
fn exports_the_entry_and_defines_every_function() {
    let wat = wat(&indirect_apply());
    assert_contains(&wat, "(export \"func/main\"");
    assert_contains(&wat, "(func $func/main");
    // `main`, `target`, and `apply` each lower to their own function.
    assert!(
        count(&wat, "(func $func/") >= 3,
        "expected three user functions",
    );
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

/// A refusal reaches the user as a sentence: every module declares the `sys.panic` import and carries one message per refusal class, and a checked operation's overflow branch hands its class's message to the import before the `unreachable` — never a bare trap.
#[test]
fn a_refusal_calls_the_panic_import_with_its_class_message() {
    let wat = wat(&intrinsic_main(CpsIntrinsic::NatAdd, vec![nat(1), nat(2)]));
    assert_contains(&wat, "(import \"sys\" \"panic\"");
    assert_eq!(
        count(&wat, "(global $refusal/"),
        6,
        "one message per panic class"
    );
    assert_contains(&wat, "global.get $refusal/nat");
    assert_contains(&wat, "call $panic");
    let call = wat.find("call $panic").unwrap();
    assert!(
        wat[call..]
            .trim_start_matches("call $panic")
            .trim_start()
            .starts_with("unreachable"),
        "the import call is followed by the unreachable that keeps the block's type"
    );
}

/// A `Panic` node a lowering seated — the knot's forcing state — ends its block by handing its class's message to the import, the same sequence a refusal decided in the emitter emits.
#[test]
fn a_panic_node_reports_its_class() {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let body = module.add_node(CpsNode::Panic(Panic::Cycle));
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

    let wat = wat(&module);
    assert_contains(&wat, "global.get $refusal/cycle");
    assert_contains(&wat, "call $panic");
}
