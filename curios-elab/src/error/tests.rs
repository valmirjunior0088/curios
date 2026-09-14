//! What a batch of refusals is: one error carrying every member, flat, classified and reported member by member.

use {super::*, curios_utilities::Source};

fn refusal() -> Error {
    Error::CannotInfer
}

fn goals() -> Error {
    Error::goals(Vec::new())
}

fn span(source: &Rc<Source>, start: usize, end: usize) -> Span {
    Span::new(Rc::clone(source), start, end)
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
    let source = Source::inline("a b");
    let batch = Error::batch(vec![
        refusal().at(span(&source, 0, 1)),
        refusal().at(span(&source, 2, 3)),
    ]);

    let wrapped = batch.at(span(&source, 0, 3)).in_declaration("outer");

    assert!(matches!(wrapped, Error::Batch(_)));
}

#[test]
fn a_batch_renders_one_report_per_member() {
    let source = Source::inline("a b");
    let batch = Error::batch(vec![
        refusal().at(span(&source, 0, 1)).in_declaration("first"),
        refusal().at(span(&source, 2, 3)),
    ]);

    let reports = batch.reports(&Rc::new(Spelling::default()), &BTreeMap::new());

    assert_eq!(reports.len(), 2);
    assert!(reports[0].message.starts_with("while elaborating first:"));
    assert_eq!(
        reports
            .iter()
            .map(|report| report.span.as_ref().map(|span| (span.start, span.end)))
            .collect::<Vec<_>>(),
        vec![Some((0, 1)), Some((2, 3))]
    );
}
