//! The emitted module's exports and dispatch table, and the loop shapes that need a dispatcher.

//! Backend lowering coverage: build a [`curios_cont::Module`](curios_cont::Module) directly, lower it with [`into_wasm`](crate::into_wasm), and assert the *shape* of the emitted wasm (its WAT text). These are the shape half of a split: the fixtures that once built the old region API and *executed* the module became shape inspection here, and end-to-end semantics in `curios/src/tests/codegen` and the native `.crs` corpus. `into_wasm` performs no optimization, so a `LetIntrinsic` over literal operands lowers one-for-one without constant folding, and the emitted instruction is exactly what codegen chose.

use super::test_support::*;

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

/// A refusal reaches the user as a sentence: every module declares the `sys.panic` import, and a checked operation's guard calls its class's helper before the `unreachable` — never a bare trap. The helper builds the sentence from its data segment where the refusal fires, so the module carries exactly the classes its code can reach and allocates no message at start-up.
#[test]
fn a_refusal_calls_its_class_helper_which_builds_the_message_where_it_fires() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::FltToNat,
        vec![flt(1.0)],
    ));
    assert_contains(&wat, "(import \"sys\" \"panic\"");
    // One class: the narrowing's own guard. The exit's code is a `Byte`, a word already, so it crosses with nothing to refuse.
    assert_eq!(
        count(&wat, "(func $refuse/"),
        1,
        "one helper per class the code reaches"
    );
    assert_eq!(count(&wat, "(func $refuse/invariant"), 1);
    assert_eq!(count(&wat, "(data $refusal/"), 1);
    assert_absent(&wat, "(global $refusal/");
    assert_contains(&wat, "call $refuse/invariant");

    let helper = &wat[wat.find("(func $refuse/invariant").unwrap()..];
    let build = helper
        .find("array.new_data $bytes $refusal/invariant")
        .unwrap();
    let call = helper.find("call $panic").unwrap();
    assert!(build < call, "the message is built inside the helper");
    assert!(
        helper[call..]
            .trim_start_matches("call $panic")
            .trim_start()
            .starts_with("unreachable"),
        "the import call is followed by the unreachable that keeps the block's type"
    );
}

/// A `Panic` node a lowering seated — the knot's forcing state — ends its block by calling its class's helper, the same sequence a refusal decided in the emitter emits.
#[test]
fn a_panic_node_reports_its_class() {
    let mut module = curios_cont::Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let body = module.add_node(curios_cont::Node::Panic(curios_cont::Panic::Cycle));
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
    assert_contains(&wat, "call $refuse/cycle");
    assert_contains(&wat, "array.new_data $bytes $refusal/cycle");
}
