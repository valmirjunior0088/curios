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
