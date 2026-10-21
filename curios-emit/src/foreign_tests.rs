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
    // The byte, then the lane: a pending token and whether it is dropped, and a pending token back.
    assert_contains(&wat, "(param i32 (ref null $elems) i32)");
    assert_contains(&wat, "(result i32 (ref null $elems))");
    // Read in the function that calls the row, since the helpers the lane's token reaches compare words of their own.
    let caller = wat
        .split("(func ")
        .find(|function| function.contains("call $host/ffi//flip"))
        .expect("a function calls the row");
    assert_absent(caller, "i32.gt_u");
    assert_contains(caller, "ref.i31");
}

/// A diverging row is imported and called like any other, and nothing resumes after it: the call is followed by the instruction that says no return arrives, never by a continuation.
#[test]
fn a_halt_calls_its_row_and_holds_nothing_for_a_return() {
    let wat = wat(&foreign_call_to(bind()));
    assert_contains(&wat, "(import \"ffi\" \"/exit\"");
    assert_absent(&wat, "(import \"sys\" \"exit\"");
    // What follows the call is the claim: a refusal elsewhere in the module, on a shape no operand can have, is not one.
    let after = wat
        .lines()
        .map(str::trim)
        .skip_while(|line| !line.starts_with("call $host/ffi//exit"))
        .nth(1)
        .map(|line| line.trim_end_matches(')'));
    assert_eq!(after, Some("unreachable"));
}
