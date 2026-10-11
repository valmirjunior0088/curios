//! A foreign call's import and the resume its result arity shapes.

use {
    super::test_support::*,
    curios_abi::{ForeignFunction, WireResults, WireType},
    std::sync::Arc,
};

/// A row answering a status alone, `/bind: (Handle, Bytes) -> Nat`.
fn bind() -> Arc<ForeignFunction> {
    row(
        "bind",
        vec![WireType::Handle, WireType::Bytes],
        WireResults::single(WireType::Nat),
    )
}

/// A row answering a record with a reference in it, `/read: (Handle, Nat) -> {status: Nat, bytes: Bytes}`.
fn read() -> Arc<ForeignFunction> {
    row(
        "read",
        vec![WireType::Handle, WireType::Nat],
        WireResults::record(
            ("status".to_string(), WireType::Nat),
            ("bytes".to_string(), WireType::Bytes),
            Vec::new(),
        ),
    )
}

#[test]
fn call_imports_and_invokes_the_host() {
    let wat = wat(&foreign_call_to(read()));
    assert_contains(&wat, "(import \"ffi\" \"/read\"");
    assert_contains(&wat, "call $host/ffi//read");
}

#[test]
fn result_arity_shapes_the_resume() {
    // A single scalar result forwards straight through; a multi-result row with a reference field embeds that field back into a rope before binding it.
    let one = wat(&foreign_call_to(bind()));
    assert_contains(&one, "call $host/ffi//bind");
    assert_absent(&one, "$bytes/embed");

    let many = wat(&foreign_call_to(read()));
    assert_contains(&many, "call $host/ffi//read");
    assert_contains(&many, "call $bytes/embed");
}

/// A `Byte` crosses as the word it is and is boxed as it stands: whatever answered the row lowered a byte, so the guest checks nothing before it reads one.
#[test]
fn a_byte_result_is_boxed_as_it_crossed() {
    let function = row(
        "flip",
        vec![WireType::Byte],
        WireResults::single(WireType::Byte),
    );

    let wat = wat(&foreign_call_to(function));
    assert_contains(&wat, "(param i32) (result i32)");
    assert_absent(&wat, "i32.gt_u");
    assert_contains(&wat, "ref.i31");
}

/// A diverging row is imported and called like any other, and nothing resumes after it: the call is followed by the instruction that says no return arrives, never by a continuation.
#[test]
fn a_halt_calls_its_row_and_holds_nothing_for_a_return() {
    let wat = wat(&foreign_call_to(bind()));
    assert_contains(&wat, "(import \"ffi\" \"/exit\"");
    assert_contains(&wat, "call $host/ffi//exit");
    assert_contains(&wat, "unreachable");
    assert_absent(&wat, "call $refuse/");
    assert_absent(&wat, "(import \"sys\" \"exit\"");
}
