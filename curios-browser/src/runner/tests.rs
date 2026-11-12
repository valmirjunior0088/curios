use super::*;

#[test]
fn an_outcome_is_read_as_the_runner_sends_it_and_its_streams_decoded() {
    let outcome =
        Posted::parse(br#"{"stdout":"aGk=","stderr":"","exitCode":0,"trap":null,"held":true}"#)
            .unwrap();

    assert_eq!(outcome.exit_code, Some(0));
    assert_eq!(outcome.held, Some(true));
    assert_eq!(outcome.streams().unwrap(), (b"hi".to_vec(), Vec::new()));
}

#[test]
fn an_outcome_that_is_none_is_refused_saying_so() {
    assert!(
        Posted::parse(b"{")
            .unwrap_err()
            .starts_with("the outcome is not one:")
    );
    let outcome =
        Posted::parse(br#"{"stdout":"!","stderr":"","exitCode":null,"trap":"t","held":null}"#)
            .unwrap();

    assert!(
        outcome
            .streams()
            .unwrap_err()
            .starts_with("the outcome's stdout is not base64:")
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
