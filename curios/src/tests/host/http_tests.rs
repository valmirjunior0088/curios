//! HTTP: the messages a program is refused where it writes them.

use crate::tests::typecheck;

/// A program that makes one message and performs nothing.
fn making(message: &str) -> String {
    format!(
        "use /std/{{Str, http}};\nuse /std/http/{{Url, Request, Response}};\nlet _ = {message};\n/std/print(\"\")"
    )
}

/// The report `message` is refused with.
fn refusal(message: &str) -> String {
    typecheck(&making(message)).expect_err("the message is refused")
}

// A message no wire could carry is refused where it is written, by the proposition its literal fails: a field name that is no token, a value that holds a line's end, a status code that is not three digits, and content under a status that carries none. The same makers accept what a wire carries.
#[test]
fn a_message_no_wire_carries_is_refused_where_it_is_written() {
    let get = r#"Request/get(Url/lit("http://h/"))"#;

    for accepted in [
        format!(r#"Request/with_header({get}, "X-A", "one")"#),
        r#"Response/of_text(200, "hello")"#.to_string(),
        r#"Response/of(204, "text/plain", x[])"#.to_string(),
        "Response/empty(304)".to_string(),
    ] {
        assert_eq!(typecheck(&making(&accepted)), Ok(()), "{accepted}");
    }

    let name = refusal(&format!(r#"Request/with_header({get}, "X (A)", "one")"#));
    assert!(name.contains("nothing discharged"), "{name}");
    assert!(name.contains(r#"is_token("X (A)")"#), "{name}");

    for refused in [
        format!(r#"Request/with_header({get}, "X-A", "one\r\nX-Injected: two")"#),
        r#"Response/of(204, "text/plain", Str/to_bytes("oops"))"#.to_string(),
        r#"Response/of_text(204, "oops")"#.to_string(),
        r#"Response/of_text(1000, "oops")"#.to_string(),
        "Response/empty(99)".to_string(),
    ] {
        let report = refusal(&refused);
        assert!(report.contains("nothing discharged"), "{refused}: {report}");
    }
}
