//! Who asked what, read backwards off a unit's traces.

use {super::*, curios_utilities::Qualifier};

fn named(segments: &[&str]) -> Global {
    Global::Authored(Qualifier::from(segments.iter().copied()))
}

fn asking(questions: &[(Question, Answered)]) -> Trace<Question, Answered> {
    let mut trace = Trace::default();
    for (question, answered) in questions {
        trace.record(*question, *answered);
    }

    trace
}

#[test]
fn a_name_is_read_by_every_declaration_that_asked_it_with_the_parts_each_asked() {
    let (a, b, x) = (named(&["m", "a"]), named(&["m", "b"]), named(&["m", "x"]));
    let mut traces = Traces::default();
    traces.file(
        a,
        asking(&[
            (Question::Signature(x), Answered::Part),
            (Question::Body(x), Answered::Part),
        ]),
    );
    traces.file(b, asking(&[(Question::Signature(x), Answered::Part)]));

    let readers = Readers::of(&traces);

    assert_eq!(
        readers.of_name(&x),
        [
            (a, Parts::SIGNATURE.with(Parts::BODY)),
            (b, Parts::SIGNATURE)
        ]
    );
    assert!(readers.of_name(&a).is_empty(), "nothing asked about a");
}

#[test]
fn a_declaration_made_again_replaces_the_edges_its_earlier_trace_added() {
    let (a, x, y) = (named(&["m", "a"]), named(&["m", "x"]), named(&["m", "y"]));
    let earlier = asking(&[(Question::Body(x), Answered::Part)]);
    let mut traces = Traces::default();
    traces.file(a, earlier.clone());
    let mut readers = Readers::of(&traces);

    readers.replace(
        a,
        Some(&earlier),
        Some(&asking(&[(Question::Kind(y), Answered::Part)])),
    );

    assert!(readers.of_name(&x).is_empty(), "a no longer reads x");
    assert_eq!(readers.of_name(&y), [(a, Parts::KIND)]);
}

#[test]
fn a_dropped_declaration_reads_nothing_and_was_answered_nothing_about_lemmas() {
    let (a, x) = (named(&["m", "a"]), named(&["m", "x"]));
    let earlier = asking(&[
        (Question::Lemmas, Answered::Whether(true)),
        (Question::Signature(x), Answered::Part),
    ]);
    let mut traces = Traces::default();
    traces.file(a, earlier.clone());
    let mut readers = Readers::of(&traces);
    assert_eq!(readers.lemmas(&a), Some(true));

    readers.replace(a, Some(&earlier), None);

    assert!(readers.of_name(&x).is_empty());
    assert_eq!(readers.lemmas(&a), None);
}

#[test]
fn parts_meet_where_they_share_one() {
    let read = Parts::SIGNATURE.with(Parts::BODY);

    assert!(read.meets(Parts::BODY));
    assert!(!read.meets(Parts::KIND));
    assert!(read.meets(Parts::ALL));
    assert_eq!(Parts::of(&Question::Lemmas), None);
}
