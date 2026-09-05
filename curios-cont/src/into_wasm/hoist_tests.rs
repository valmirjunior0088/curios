//! What hoists to an interned global and what stays inline.

//! Backend lowering coverage: build a [`CpsModule`](crate::CpsModule) directly, lower it with [`into_wasm`](crate::into_wasm), and assert the *shape* of the emitted wasm (its WAT text). These are the shape half of a split: the fixtures that once built the old region API and *executed* the module became shape inspection here, and end-to-end semantics in `curios/src/tests/codegen` and the native `.crs` corpus. `into_wasm` performs no optimization, so a `LetIntrinsic` over literal operands lowers one-for-one without constant folding, and the emitted instruction is exactly what codegen chose.

use super::test_support::*;

#[test]
fn constant_tuples_hoist_to_one_interned_global() {
    let wat = wat(&constant_tuple_pair());
    // One construction in the start function serves both bindings; the projection reads it back through the const global.
    assert_eq!(count(&wat, "struct.new $tuple/1"), 1);
    assert_contains(&wat, "global.set $const/");
    assert_contains(&wat, "global.get $const/");
}

#[test]
fn runtime_tuples_stay_inline() {
    let wat = wat(&runtime_tuple());
    assert_contains(&wat, "struct.new $tuple/1");
    assert_absent(&wat, "const/");
}

#[test]
fn constant_bin_literals_hoist_into_a_start_initialized_global() {
    let wat = wat(&bin_len());
    assert_contains(&wat, "global.set $const/");
    assert_contains(&wat, "global.get $const/");
    // The program's one constant; the refusal messages are hoisted the same way under `refusal/…` and are not counted here.
    assert_eq!(count(&wat, "array.new_data $bytes $const/"), 1);
}

#[test]
fn overflowing_scalars_and_their_aggregates_stay_inline() {
    let wat = wat(&overflowing_tuple());
    assert_absent(&wat, "const/");
    assert_traps(&wat);
}
