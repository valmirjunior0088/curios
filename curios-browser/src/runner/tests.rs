use super::*;

#[test]
fn a_write_and_an_outcome_are_each_read_as_the_runner_sends_them() {
    let Sent::Wrote(wrote) =
        Sent::parse(br#"{"wrote":{"stream":"stderr","bytes":"aGk="}}"#).unwrap()
    else {
        panic!("a write");
    };
    assert_eq!(wrote.stream, Stream::Stderr);
    assert_eq!(wrote.decoded().unwrap(), b"hi");

    assert_eq!(
        Sent::parse(br#"{"outcome":{"exitCode":0,"trap":null,"held":true}}"#).unwrap(),
        Sent::Outcome(Posted {
            exit_code: Some(0),
            trap: None,
            held: Some(true),
        })
    );
}

#[test]
fn a_message_that_is_none_is_refused_saying_so() {
    assert!(
        Sent::parse(b"{")
            .unwrap_err()
            .starts_with("the message is not one:")
    );
    let Sent::Wrote(wrote) = Sent::parse(br#"{"wrote":{"stream":"stdout","bytes":"!"}}"#).unwrap()
    else {
        panic!("a write");
    };

    assert!(
        wrote
            .decoded()
            .unwrap_err()
            .starts_with("what the page wrote is not base64:")
    );
}

#[test]
fn a_reply_carries_what_was_written_in_base64_and_the_next_step() {
    let reply = Reply::new(
        "├─ /app/t; passed in 0.0s\n".as_bytes(),
        Some(Step {
            subject: 0,
            test: 1,
        }),
    );

    assert_eq!(Reply::parse(&reply.render()).unwrap(), reply);
    assert_eq!(
        reply.decoded().unwrap(),
        "├─ /app/t; passed in 0.0s\n".as_bytes()
    );
    assert_eq!(
        String::from_utf8(reply.render()).unwrap(),
        r#"{"written":"4pSc4pSAIC9hcHAvdDsgcGFzc2VkIGluIDAuMHMK","next":{"subject":0,"test":1}}"#
    );
    assert_eq!(
        String::from_utf8(Reply::new(b"", None).render()).unwrap(),
        r#"{"written":"","next":null}"#
    );
}

/// The document carries the run as one JSON block the script reads, and the script whole, which spells no `</script` of its own.
#[test]
fn the_runner_document_carries_the_run_and_the_script() {
    let document = String::from_utf8(runner()).unwrap();

    assert!(
        document
            .contains(r#"<script type="application/json" id="run">{"argument":"arg"}</script>"#),
        "{document}"
    );
    assert!(document.contains("<script type=\"module\">"), "{document}");
    assert!(document.contains(RUNNER), "{document}");
    assert!(!RUNNER.contains("</script"));
}
