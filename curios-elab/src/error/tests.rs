//! What a batch of refusals is: one error carrying every member, flat, classified and reported member by member.

use {
    super::*,
    curios_core::{Anchor, Anchored},
    curios_utilities::{Source, Span},
};

fn refusal() -> Error {
    Error::CannotInfer
}

fn goals() -> Error {
    Error::goals(Vec::new())
}

/// The name a declaration of the entry module is declared under.
fn declared(label: &str) -> Global {
    Global::Authored(Qualifier::from([label]))
}

/// Where the entry's text `a b` lies, and the extent of each of its two letters and of the whole.
fn written() -> (Anchors, [Extent; 3]) {
    let source = Source::inline("a b");
    let mut anchors = Anchors::default();
    let within = anchors.site(
        Anchor::of(Anchored::Entry),
        &Span::new(source.clone(), 0, 3),
    );
    let extent = |start, end| {
        within
            .extent(&Span::new(source.clone(), start, end))
            .expect("the span lies in the text")
    };

    (anchors, [extent(0, 1), extent(2, 3), extent(0, 3)])
}

#[test]
fn a_batch_of_one_refusal_is_that_refusal() {
    assert!(matches!(Error::batch(vec![refusal()]), Error::CannotInfer));
}

#[test]
fn a_batch_of_batches_is_flat() {
    let batch = Error::batch(vec![Error::batch(vec![refusal(), goals()]), refusal()]);

    assert_eq!(batch.each().count(), 3);
    assert!(
        batch
            .each()
            .all(|member| !matches!(member, Error::Batch(_)))
    );
}

#[test]
fn a_batch_is_incomplete_only_when_every_member_is() {
    assert!(Error::batch(vec![goals(), goals()]).is_incomplete());
    assert!(!Error::batch(vec![goals(), refusal()]).is_incomplete());
}

/// The members carry their own attribution, so the wrappers a caller adds around the whole leave it as it is.
#[test]
fn a_batch_is_neither_located_nor_attributed() {
    let (_, [first, second, whole]) = written();
    let batch = Error::batch(vec![refusal().at(first), refusal().at(second)]);

    let wrapped = batch.at(whole).in_declaration([declared("outer")]);

    assert!(matches!(wrapped, Error::Batch(_)));
}

#[test]
fn a_batch_renders_one_report_per_member() {
    let (anchors, [first, second, _]) = written();
    let batch = Error::batch(vec![
        refusal().at(first).in_declaration([declared("first")]),
        refusal().at(second),
    ]);

    let reports = batch.reports(&Rc::new(Spelling::default()), &BTreeMap::new(), &[&anchors]);

    assert_eq!(reports.len(), 2);
    assert!(reports[0].message.starts_with("while elaborating /first:"));
    assert_eq!(
        reports
            .iter()
            .map(|report| report.span.as_ref().map(|span| (span.start, span.end)))
            .collect::<Vec<_>>(),
        vec![Some((0, 1)), Some((2, 3))]
    );
}
