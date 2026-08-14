use super::Qualifier;

#[test]
fn without_first_drops_the_leading_segment() {
    let qualifier = Qualifier::from(["a", "b", "c"]);

    assert_eq!(qualifier.without_first(), Qualifier::from(["b", "c"]));
}

#[test]
fn without_first_of_a_single_segment_is_empty() {
    let qualifier = Qualifier::from(["a"]);

    assert_eq!(qualifier.without_first(), Qualifier::empty());
}

#[test]
fn without_first_of_empty_is_empty() {
    assert_eq!(Qualifier::empty().without_first(), Qualifier::empty());
}

#[test]
fn is_within_accepts_itself_and_its_descendants() {
    let module = Qualifier::from(["a", "b"]);

    assert!(module.is_within(&module));
    assert!(Qualifier::from(["a", "b", "c"]).is_within(&module));
}

#[test]
fn is_within_rejects_ancestors_and_siblings() {
    let module = Qualifier::from(["a", "b"]);

    assert!(!Qualifier::from(["a"]).is_within(&module));
    assert!(!Qualifier::from(["a", "c"]).is_within(&module));
}

// Segment-wise, not textual: a longer segment that merely starts with the ancestor's is a different module.
#[test]
fn is_within_compares_whole_segments() {
    assert!(!Qualifier::from(["Foobar"]).is_within(&Qualifier::from(["Foo"])));
}

// The empty qualifier is the root, so everything lies within it.
#[test]
fn every_qualifier_is_within_the_root() {
    assert!(Qualifier::empty().is_within(&Qualifier::empty()));
    assert!(Qualifier::from(["a", "b"]).is_within(&Qualifier::empty()));
}
