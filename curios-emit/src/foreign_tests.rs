//! A foreign call's import and the resume its result arity shapes.

use {
    super::test_support::*,
    curios_abi::{DeclaredForeign, ForeignFunction, WireResults, WireSignature, WireType},
    std::sync::Arc,
};

#[test]
fn call_imports_and_invokes_the_host() {
    let wat = wat(&foreign_call("handle_read"));
    assert_contains(&wat, "(import \"sys\" \"handle_read\"");
    assert_contains(&wat, "call $host/sys/handle_read");
}

#[test]
fn result_arity_shapes_the_resume() {
    // A single scalar result forwards straight through; a multi-result row with a reference field embeds that field back into a rope before binding it.
    let one = wat(&foreign_call("socket_bind"));
    assert_contains(&one, "call $host/sys/socket_bind");
    assert_absent(&one, "$bytes/embed");

    let many = wat(&foreign_call("handle_read"));
    assert_contains(&many, "call $host/sys/handle_read");
    assert_contains(&many, "call $bytes/embed");
}

/// A `Byte` crosses as a word the host chose, so the guest refuses one past 255 before boxing it — as the host's fault, not the program's.
#[test]
fn a_byte_result_past_255_is_refused_as_a_host_reply() {
    let function = Arc::new(ForeignFunction::Declared(DeclaredForeign {
        name: "/flip".to_string(),
        label: "flip".to_string(),
        signature: WireSignature {
            params: vec![WireType::Byte],
            results: WireResults::single(WireType::Byte),
        },
    }));

    let wat = wat(&foreign_call_to(function));
    assert_contains(&wat, "(param i32) (result i32)");
    assert_contains(&wat, "i32.gt_u");
    assert_contains(&wat, "call $refuse/host_reply");
    assert_contains(&wat, "ref.i31");
}

/// A diverging row is imported and called like any other, and nothing resumes after it: the call is followed by the refusal of a host that returns anyway, never by a continuation.
#[test]
fn a_halt_calls_its_row_and_refuses_a_host_that_returns() {
    let wat = wat(&foreign_call("socket_bind"));
    assert_contains(&wat, "(import \"sys\" \"proc_exit\"");
    assert_contains(&wat, "call $host/sys/proc_exit");
    assert_contains(&wat, "call $refuse/host_reply");
    assert_absent(&wat, "(import \"sys\" \"exit\"");
}
