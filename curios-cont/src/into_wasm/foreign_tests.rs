//! A foreign call's import and the resume its result arity shapes.

//! Backend lowering coverage: build a [`CpsModule`](crate::CpsModule) directly, lower it with [`into_wasm`](crate::into_wasm), and assert the *shape* of the emitted wasm (its WAT text). These are the shape half of a split: the fixtures that once built the old region API and *executed* the module became shape inspection here, and end-to-end semantics in `curios/src/tests/codegen` and the native `.crs` corpus. `into_wasm` performs no optimization, so a `LetIntrinsic` over literal operands lowers one-for-one without constant folding, and the emitted instruction is exactly what codegen chose.

use super::test_support::*;

#[test]
fn call_imports_and_invokes_the_host() {
    let wat = wat(&foreign_call("read"));
    assert_contains(&wat, "(import \"sys\" \"read\"");
    assert_contains(&wat, "call $host/sys/read");
}

#[test]
fn result_arity_shapes_the_resume() {
    // A single scalar result forwards straight through; a multi-result row with a reference field embeds that field back into a rope before binding it.
    let one = wat(&foreign_call("bind"));
    assert_contains(&one, "call $host/sys/bind");
    assert_absent(&one, "$bytes/embed");

    let many = wat(&foreign_call("read"));
    assert_contains(&many, "call $host/sys/read");
    assert_contains(&many, "call $bytes/embed");
}
