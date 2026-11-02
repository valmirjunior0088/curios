use super::*;

#[test]
fn a_report_is_read_as_the_runner_posts_it_and_its_streams_decoded() {
    let report = Report::parse(
        br#"{"run":"abc","subject":1,"test":2,"outcome":{"stdout":"aGk=","stderr":"","exitCode":0,"trap":null,"held":true}}"#,
    )
    .unwrap();

    assert_eq!(report.run, "abc");
    assert_eq!(
        report.step(),
        Step {
            subject: 1,
            test: 2
        }
    );
    assert_eq!(report.outcome.exit_code, Some(0));
    assert_eq!(report.outcome.held, Some(true));
    assert_eq!(
        report.outcome.streams().unwrap(),
        (b"hi".to_vec(), Vec::new())
    );
}

#[test]
fn a_report_that_is_none_is_refused_saying_so() {
    assert!(
        Report::parse(b"{")
            .unwrap_err()
            .starts_with("the report is not one:")
    );
    let report = Report::parse(
        br#"{"run":"abc","subject":0,"test":0,"outcome":{"stdout":"!","stderr":"","exitCode":null,"trap":"t","held":null}}"#,
    )
    .unwrap();

    assert!(
        report
            .outcome
            .streams()
            .unwrap_err()
            .starts_with("the report's stdout is not base64:")
    );
}

#[test]
fn a_reply_carries_what_was_written_in_base64_and_the_next_step() {
    let taken = Reply::taken(
        b"/app/t: passed\n",
        b"",
        Some(Step {
            subject: 0,
            test: 1,
        }),
    );

    assert_eq!(Reply::parse(&taken.render()).unwrap(), taken);
    assert_eq!(
        String::from_utf8(taken.render()).unwrap(),
        r#"{"taken":true,"stdout":"L2FwcC90OiBwYXNzZWQK","stderr":"","next":{"subject":0,"test":1}}"#
    );
    assert_eq!(
        String::from_utf8(Reply::ignored().render()).unwrap(),
        r#"{"taken":false,"stdout":"","stderr":"","next":null}"#
    );
}

/// The document carries the run as one JSON block the script reads, and the script whole, which spells no `</script` of its own.
#[test]
fn the_runner_document_carries_the_run_and_the_script() {
    let document = String::from_utf8(runner(
        "0123",
        Some(Step {
            subject: 0,
            test: 0,
        }),
    ))
    .unwrap();

    assert!(
        document.contains(
            r#"<script type="application/json" id="run">{"token":"0123","next":{"subject":0,"test":0},"argument":"arg"}</script>"#
        ),
        "{document}"
    );
    assert!(document.contains("<script type=\"module\">"), "{document}");
    assert!(document.contains(RUNNER), "{document}");
    assert!(!RUNNER.contains("</script"));
    assert!(
        String::from_utf8(runner("0123", None))
            .unwrap()
            .contains(r#""next":null"#)
    );
}
