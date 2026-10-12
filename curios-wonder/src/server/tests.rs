use {
    super::{Job, Raised, SETTLE, SETTLE_SHARE, batch},
    std::{collections::BTreeMap, path::PathBuf, time::Duration},
};

/// The document every job here is raised for.
fn scratch() -> PathBuf {
    PathBuf::from("/scratch.crs")
}

/// What the editor holds once `scratch` reads `text`.
fn overlay(text: &str) -> BTreeMap<PathBuf, String> {
    BTreeMap::from([(scratch(), text.to_string())])
}

/// A keystroke leaving `scratch` at `text`.
fn edit(text: &str) -> Job {
    Job {
        document: scratch(),
        raised: Raised::Edited,
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
    assert!(dirty == BTreeMap::from([(scratch(), Raised::Edited)]));
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
        document: PathBuf::from("/"),
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
