//! How the server paces its checks: a burst checked as one batch, a wait as a share of the last check under its cap, a warming batch, the progress it tells, and a refresh asked only of a client that takes one.

use {
    super::test_support::*,
    crate::{server::*, *},
    std::{collections::BTreeMap, path::PathBuf, sync::Arc, time::Duration},
};

/// A keystroke leaving `scratch` at `text`.
fn edit(text: &str) -> Job {
    Job {
        paths: vec![scratch()],
        raised: Raised::Document,
        documents: overlay(text),
    }
}

/// A burst settles before it is compiled: handed the first keystroke, an idle analyst asks to wait, and what arrives inside the wait is one batch from the text the burst ended on. Without the wait it would start on the first keystroke's text and publish an answer about a buffer two keystrokes out of date.
#[test]
fn a_burst_is_one_batch_from_the_text_it_ended_on() {
    let spent = Duration::from_secs(1);
    let mut rest = [edit("mm"), edit("mmm")].into_iter();
    let mut waits = Vec::new();

    let (documents, dirty) = batch(Some(spent), edit("m"), |wait| {
        waits.push(wait);
        rest.next()
    });

    assert_eq!(documents, overlay("mmm"));
    assert!(dirty == BTreeMap::from([(scratch(), Raised::Document)]));
    assert_eq!(waits, [spent / SETTLE_SHARE; 3]);
}

/// The wait is its share of what the last check cost: nothing for a session's first check, which has no cost to weigh, and the cap for a check whose share would pass it.
#[test]
fn a_wait_is_a_share_of_the_last_check_and_never_past_the_cap() {
    let waits = |spent| {
        let mut waits = Vec::new();
        batch(spent, edit("m"), |wait| {
            waits.push(wait);
            None
        });

        waits
    };

    assert_eq!(waits(None), [Duration::ZERO]);
    assert_eq!(waits(Some(SETTLE)), [SETTLE / SETTLE_SHARE]);
    assert_eq!(waits(Some(SETTLE * SETTLE_SHARE * 2)), [SETTLE]);
}

/// The warming check is worth the whole cap by itself, a document arriving being the reason to drop it, and the wait is the document's once one joins.
#[test]
fn a_warming_batch_waits_the_cap_until_a_document_joins_it() {
    let warming = Job {
        paths: vec![PathBuf::from("/")],
        raised: Raised::Warming,
        documents: BTreeMap::new(),
    };
    let mut rest = [edit("m")].into_iter();
    let mut waits = Vec::new();

    let (documents, _) = batch(None, warming, |wait| {
        waits.push(wait);
        rest.next()
    });

    assert_eq!(documents, overlay("m"));
    assert_eq!(waits, [SETTLE, Duration::ZERO]);
}

/// A check is told the editor as it goes where the client takes progress: a progress begun under a token the client is asked to create, a report for each unit the fold compiles and none for one it reuses or for the program, and an end; a client that advertises none is told none. Mutation-checked: with a report at a reused unit too, the reused prefix is reported.
#[test]
fn a_check_is_told_as_a_progress_reporting_each_unit_compiled() {
    let sent = Arc::new(std::sync::Mutex::new(Vec::new()));
    let sender: Box<dyn Fn(Message) -> Result<(), String> + Send> = {
        let sent = Arc::clone(&sent);
        Box::new(move |message| {
            sent.lock()
                .expect("the test holds the lock alone")
                .push(message);
            Ok(())
        })
    };
    let mut told = 0;
    let compiled = curios_common::Qualifier::from(["app"]);
    let reused = curios_common::Qualifier::from(["std"]);
    let mut reporting = Reporting {
        sender: &*sender,
        told: &mut told,
    };
    reporting.asked(&curios_package::Named::Stdin, None);
    reporting.step(curios_pipeline::Progress::Reused(&reused), None);
    reporting.step(
        curios_pipeline::Progress::Compiling {
            unit: &compiled,
            compiled: curios_module::Compiled::Built,
        },
        None,
    );
    reporting.step(curios_pipeline::Progress::Entry { held: false }, None);
    reporting.ended(Ended::Done);

    let heard = sent
        .lock()
        .expect("the test holds the lock alone")
        .iter()
        .map(|message| match message {
            Message::Request(request) => request.method.clone(),
            Message::Notification(notification) => {
                match serde_json::from_value::<ProgressParams>(notification.params.clone())
                    .expect("a progress")
                    .value
                {
                    ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(begun)) => {
                        format!("begin {}", begun.message.unwrap_or_default())
                    }
                    ProgressParamsValue::WorkDone(WorkDoneProgress::Report(report)) => {
                        format!("report {}", report.message.unwrap_or_default())
                    }
                    ProgressParamsValue::WorkDone(WorkDoneProgress::End(_)) => "end".to_string(),
                }
            }
            Message::Response(_) => "response".to_string(),
        })
        .collect::<Vec<_>>();
    assert_eq!(
        heard,
        [
            "window/workDoneProgress/create".to_string(),
            "begin <stdin>".to_string(),
            format!("report compiling {}", compiled.join()),
            "end".to_string(),
        ]
    );

    let mut params = lsp_types::InitializeParams::default();
    assert!(!reports_progress(&params));
    params.capabilities.window = Some(lsp_types::WindowClientCapabilities {
        work_done_progress: Some(true),
        ..Default::default()
    });
    assert!(reports_progress(&params));
}

/// A refresh is asked only of a client that declares it takes one.
#[test]
fn a_refresh_is_asked_only_of_a_client_that_takes_one() {
    let client = |capabilities| {
        serde_json::from_value::<InitializeParams>(
            serde_json::json!({ "capabilities": capabilities }),
        )
        .expect("the parameters read")
    };

    assert!(refreshes_tokens(&client(serde_json::json!({
        "workspace": { "semanticTokens": { "refreshSupport": true } }
    }))));
    assert!(!refreshes_tokens(&client(serde_json::json!({
        "workspace": { "semanticTokens": { "refreshSupport": false } }
    }))));
    assert!(!refreshes_tokens(&client(serde_json::json!({}))));
}
